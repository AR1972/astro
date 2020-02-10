/***    DELTREE.C - Delete directory tree
 *
 *      Microsoft Confidential
 *      Copyright (C) Microsoft Corporation 1992
 *      All Rights Reserved.
 *
 *      History:
 *          07-Nov-1992 bens   Copied from ZTOOLS project
 *	    02-Dec-1992 rhogue Remove sargv.obj which doesn't recognize
 *			       hidden/system files.
 */

#include <tools.h>
#include <ctype.h>
#include <stdlib.h>
#include <conio.h>
#include <string.h>

#include "deltree.msg" // Localizable strings

#define BS    0x08
#define CTRLC 0x03
#define CTRLZ 0x1A
#define ENTER 0x0d

flagType fAsk = TRUE;  // global variable for asking confirmation

/***    Usage - show command line /? help
 *
 */
void Usage(void)
{
    int     i;

    for (i=0; i<(sizeof(apszHelp)/sizeof(char *)); i++) {
        printf ("%s\n",apszHelp[i]);
    }
    exit(1);
}

static void fConfirm (
    char		*name,
    struct findType	*pBuf,
    va_list		ap
    )
{
    int ch, chLast;
    char sz[MAXPATHLEN];

    fileext (name, sz);
    if (!strcmp ("..", sz) || !strcmp (".", sz))
	return;

    if (fAsk == FALSE) {
	printf (pszMsgDeleting, name);
	delnode (name);
	return;
    }

    /*	flush pending inut
     */
    while (!kbhit ())
	zgetch ();  //int 21 ah 7 call

    /* if name is a directory then display the del dir msg, else display
     * the del file msg.
     */
    if ( ((char) getattr (name)) == A_D)
	printf (pszMsgDelDirPrompt, name);
    else
	printf (pszMsgDelFilePrompt, name);

    /*	Wait until we see a Y<CR> or N<CR>
     */

    chLast = 0;
    while (TRUE) {
	ch = getch ();
	ch = tolower (ch);
        if (ch == ENTER && (chLast == chYES || chLast == chNO)) {
	    printf ("\n");
	    if (ch == ENTER && (chLast == chYES)) {
		//"Deleting %s...\n"
		printf (pszMsgDeleting, name);
		delnode (name);
		return;
		} else
		    return;
	    }
	if (ch == CTRLC || ch == CTRLZ)
	    return;
	if (ch == chYES || ch == chNO) {
	    putch (ch);
	    putch (BS);
            chLast = ch;    // Remember most recent selection
	    }
	}
}

void
main(int c, char **v)
{
    char sz[MAXPATHLEN];
    int i;

    SHIFT (c, v);
    while (c && fSwitChr (**v)) {
        /*
         *  /Y is NOT localized, as is the standard for 1-char switch names
         */
	if (!strcmp (*v+1, "y") || !strcmp (*v+1, "Y"))
	    fAsk = FALSE;
	else {
            if (!strcmp (*v+1, "?")) {
		Usage ();
                exit (0);
                }
            else {
		printf (pszMsgInvalidSwitch,*v+1);
                exit (1);
                }
	    }
	SHIFT (c, v);
	}
    if (c == 0) {
	printf (pszMsgNoParameters);
	exit (1);
    }
    for (i=0; i < c; i++) {

	/* create a full path so forfile doesn't get confused */
	if (!fileext (v[i], sz))
	    /*	We have a directory.  upd() produces a "canonical" path
	     *	name (c:\foo\bar), eliminating any .\.. meta character
	     *	stuff, so that forfile will be happy.
	     */
	    upd ("*.*", v[i], sz);
	else {
	    /*	We have a file name.  If it is "." or "..", we need to
	     *	append \*.*, so that forfile will delete the specified
	     *	directory.  If we do not append the \*.*, forfile gets
	     *	confused and refuses to delete anything.
	     */
	    if (!strcmp ("..", sz) || !strcmp (".", sz)) {
		strcpy (sz, v[i]);
		pathcat (sz, "*.*");
	    } else
		strcpy (sz, v[i]);
	}

	forfile (sz, A_H | A_S | A_D, fConfirm);
    }
    exit(0);
}
