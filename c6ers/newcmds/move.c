/***    MOVE.C - Move and Rename files and directories
 *
 *      Microsoft Confidential
 *      Copyright (C) Microsoft Corporation 1992
 *      All Rights Reserved.
 *
 *      History:
 *          07-Nov-1992 bens   Copied from ZTOOLS project
 *          14-Nov-1992 bens   Update to \\TOOLSVR\SOURCE\...\SYSTOOLS project,
 *                                 and link with SETARGV.OBJ from C RTL!
 *	    02-Dec-1992 rhogue Remove sargv.obj which doesn't recognize
 *			       hidden/system files. Other bugs too.
 */

#include <dos.h>
#include <tools.h>
#include <malloc.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <io.h>
#include <direct.h>

#include "move.msg" // localizable strings

#define BS    0x08
#define CTRLC 0x03
#define CTRLZ 0x1A
#define ENTER 0x0d
#define MAX_DIR_PATH 66

char attr;    // keeps track of the destination files attribute (file, directory, etc...)
char * dst;   // pointer to destination path
char * src;   // pointer to source path
char * name;  // temp variable for parts of source path from forfile
int erc;      // variable for error condition

/***    Usage - show command line /? help
 *
 */
void Usage(void)
{
    int     i;

    for (i=0; i<(sizeof(apszHelp)/sizeof(char *)); i++) {
        printf ("%s\n",apszHelp[i]);
    }
}

/* find_longest - find the longest path in a subtree.  Recursive
*  function.
*
*	Input : full path name from which to start search
*	Output : length of longest path found
*	Stops if path length greater than MAX_DIR_PATH is found
*/
int
find_longest(register char *start_path)
{
	struct	find_t	findbuf;		/* buffer for DOS info */
	char	save_path[128]; 		/* save area for path */
	register int	 len1, len2;		/* length holders */

	/* save original path, and set up search path */
	strcpy(save_path,start_path);
	strcat(start_path,"\\*.*");
	len1 = strlen(save_path);		/* save starting length */

	/* skip over . and .. if present.  Return if not found */
	if (_dos_findfirst(start_path,_A_SUBDIR|_A_HIDDEN|_A_SYSTEM,&findbuf) ||
	    _dos_findnext(&findbuf))
		return(len1);

	/* find each subdirectory and check the path size */
	while (_dos_findnext(&findbuf) == 0)
	{
		if (!(findbuf.attrib & _A_SUBDIR))
			continue;		/* bounce files */
		strcpy(start_path,save_path);
		strcat(start_path,"\\");
		strcat(start_path,findbuf.name);
		len2 = find_longest(start_path);
		if (len2 > len1)
			len1 = len2;
	}

	return(len1);	/* return longest path found */
}

/* PromptUser - prompts the user for chYes or chNo
 *
 * return TRUE if chYes, and FALSE if chNo.
 */
int PromptUser(void)
{
    int ch, chLast;

    /*	flush pending inut */
    while (!kbhit ())
	zgetch ();  //int 21 ah 7 call

    /*	Wait until we see a Y<CR> or N<CR>
     */
    chLast = 0;
    while (TRUE) {
	ch = getch ();
	ch = tolower (ch);
        if (ch == ENTER && (chLast == chYES || chLast == chNO)) {
	    printf ("\n");
	    if (ch == ENTER && (chLast == chYES)) {
		return TRUE;
		} else
		    return FALSE;
	    }
	if (ch == CTRLC || ch == CTRLZ)
	    return FALSE;
	if (ch == chYES || ch == chNO) {
	    putch (ch);
	    putch (BS);
            chLast = ch;    // Remember most recent selection
	    }
	}
    // return FALSE;
}

/* fConfirm is called from forfile() to deltree a specific file or directory
   sz	   : file name or directory to deltree
   pBuf    : file attribute type, hidden,system etc...
   va_list : forfile() expected parameter
*/
static void fConfirm (
    char		*sz,
    struct findType	*pBuf,
    va_list		ap
    )
{
    int cb;
    char *s, szDstName[_MAX_PATH],szSrcPath[_MAX_PATH];

	fileext (sz, name);
	if (!strcmp ("..", name) || !strcmp (".", name))
	    return;

	if ( (rootpath (sz, src) == -1) )
	    goto cantmove;

	/* Check to see of the src file exists. fmove() doesn't check,
	 * and will delete the dst file even if the src doesn't exists
	 */
	if (access (sz,0) == -1)
	    goto cantmove;

	/* if src file is a system or hidden file tell the user we can't/won't move it */
	if ( ((char) getattr (src)) & (A_S|A_H) ) {
	    errno = EACCES;
	    goto cantmove;
	}

	/* If src is a directory then we are renaming a directory. We
	 * need to make sure the the rename doesn't create a path length
	 * greater than MAX_DIR_PATH.
	 */
	if ( ((char) getattr (src)) == A_D) {

	    /* Get the Destination directory name */
	    fileext (dst, szDstName);

	    /* if the destination dir name is longer than the src name,
	     * then we need to check to see of the creation of a longer
	     * directory will cause an path larger than MAX_DIR_PATH.
	     */
	    cb = (strlen(szDstName)) - (strlen(name));
	    strcpy (szSrcPath, src); // find_longest() modifies the string - make copy
	    if ( cb > 0 ) {

	       /* if the longest path plus the extra characters from the
		* new directory is greater than MAX_DIR_PATH then error.
		*/
	       if ( ((find_longest(szSrcPath))+cb) > MAX_DIR_PATH) {
		   errno = EACCES;
		   // "Cannot move %s - %s"
		   printf (pszErrCannotMove, sz, error ());
		   erc++;
		   return;
		   }
		}
	     }

	cb = strlen(src);
	if ( !strncmp(src, dst, cb) && *strbscan(dst+cb, "\\/") ) {
	    // takes 'loop'ers out with permissions error;
	    //	if match for all of source and there's a subdir, it's loop
	    errno = EACCES;
            // "Cannot move %s - %s"
cantmove:   printf (pszErrCannotMove, sz, error ());
            erc++;
	    return;
            }
	strcpy (name, dst);
        if (TESTFLAG(attr, A_D)) {
            if (!fPathChr (name[strlen(name)-1]))
                strcat (name, "\\");
            upd (src, name, name);
            }

	/* Check to see if the destination and source file are not identical */
	if (strcmpi (src,name) == 0) {
	    printf (pszMsgSrcEqualsDst, src, error ());
            erc++;
	    return;
	    }

	printf ("%s => %s ", src, name);
	fflush (stdout);
	s = fmove( src, name );
	if (s) {
	    erc++;
	    printf ("[%s]\n", s);
	    }
	else
	    // "[ok]\n"
	    printf (pszMsgOK);
}

void 
main (int c, char **v)
{
    int i, iLen, isMultipleFile=FALSE;
    char * pszBeginTokenString, * pszEndTokenString; // pointers strchr call
    char * pszSourceArgs;
    char * pszCwd;		    /* save current directory */
    unsigned num_drives;	    /* for setdrive call */
    unsigned cur_drive; 	    /* records current drive */
    struct findType *fbuf;	    /* for find first - find next */
    flagType fAsk = TRUE;	    /* for asking confirmation */

    pszSourceArgs = malloc (MAXPATHLEN);
    memset(pszSourceArgs, 0, MAXPATHLEN);
    src = malloc (MAXPATHLEN);
    memset(src, 0, MAXPATHLEN);
    dst = malloc (MAXPATHLEN);
    memset(dst, 0, MAXPATHLEN);
    name = malloc (MAXPATHLEN);
    memset(name, 0, MAXPATHLEN);

    /* check parameter validity */
    SHIFT (c,v);
    while (c && fSwitChr (**v)) {
        /*
         *  /Y is NOT localized, as is the standard for 1-char switch names
         */
	if (!strcmp (*v+1, "y") || !strcmp (*v+1, "Y"))
	    fAsk = FALSE;
	else {
	    /* check for /? switch to display usage */
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
    if (c < 2) {
        /* not enough parameters */
	printf (pszMsgNoParameters);
        exit (1);
	}

    if ( rootpath (v[c-1], dst) ) {
        // "Cannot move %s - %s\n"
        printf (pszErrCannotMove, v[c-1], error ());
        exit(1);
	}

    /* see if last char is path char ; if so remove it unless x:\ */
    if (fPathChr (dst[strlen(dst)-1])) {
	/* if not at root */
	if (strlen(dst) > 3) {
            /* get rid of trailing path char */
	    dst[strlen(dst)-1] = 0;
	}
    }

    /* White space is allowed in the command line with the comma used as a
     * delimeter.
     */
    strcpy (pszSourceArgs,v[0]);
    for (i=1; i < c-1; i++) {
	if ((v[i][0] == ',') || (v[i-1][(strlen(v[i-1]))-1] == ',') )
	    strcat (pszSourceArgs, v[i]);
	else {
	    printf (pszMsgTooManyParameters, v[i]);
	    exit (1);
	}
    }

    /* If the command line has the last arg as a source file, then
     * there isn't a destination file (assumed to be the last arg).
     *
     * Note: i is at least one because the for loop sets it to 1.
     */
    if ((v[c-1][0] == ',') || (v[i-1][(strlen(v[i-1]))-1] == ',') ) {
        /* not enough parameters */
	printf (pszMsgNoParameters);
        exit (1);
	}

    /* All the args are now in a single array in "file1,file2,..." format.
     * We need to find out if the dest is a directory or a file.
     *	   if move of multiple files to file	     - error.
     *	   if move of multiple files to non existant - confirm creation of dir.
     *	   if move of single file to non existant    - dest is a file.
     *	   if move of multiple files to dir	     - continue.
     *	   if move of single file to file | dir      - continue.
     */

    /* Check to see if there are muliple source files */

    pszBeginTokenString = pszSourceArgs;
    pszEndTokenString = strchr (pszBeginTokenString, ',');

    if (pszEndTokenString != NULL)
	isMultipleFile = TRUE;
    else {
    /* Even though we only have one src file, it could be a wildcard file.
     * if we can't access the source file then we will assume it is a
     * multiple file
     */
	/* Since there is only one src file, we can do a findfirst findnext
	 * to see if multiple files exist. If ffist fails, the error will
	 * get picked up during file forfile() process
	 */
	if (ffirst (pszBeginTokenString,A_ALL,fbuf) == FALSE) {
	    if (fnext (fbuf) == FALSE)
		isMultipleFile = TRUE;

	    findclose (fbuf);
	}
    }

    /* get the destinations file attribute FILE or DIRECTORY */
    attr = (char) getattr (dst);

    /* getattr will return -1 if the file doesn't exist.
     * if destintation doesn't exist (attr=-1), then if src is multiple file, ask
     * if the user wants to create the destination as a direcory. If the
     * src is a single file, then destination is a file
     */
    if (attr == -1) {
	if (isMultipleFile == TRUE) {
	    /* ask if the user wants to make the direcory for the mulitple file
	     * copy. If they don't, then we error because you can't copy multiple
	     * files to a file.
	     */

	    /*	if the /Y switch was given to suppress prompting, skip it */
	    if (fAsk == TRUE) {
		printf (pszMsgMakeDirPrompt, dst);
		i = PromptUser();
	    } else
		i=TRUE;

	    if (i == TRUE) {
		i = MkPath(dst);
		/* see if MkPath gave us an error */
		if (i != 0) {
		    // "Cannot move > 1 file to another file\n"
		    printf (pszErrCannotMoveMultiple);
		    exit (1);
		}
	    } else {
		// "Cannot move > 1 file to another file\n"
		printf (pszErrCannotMoveMultiple);
		exit (1);
	    }

	    /* set dest attribute to be of a directory type */
	    SETFLAG (attr, A_D);
	} else
	  /* set dest attribute to be a file type */
	  RSETFLAG (attr, A_D);
    } else {
	/* If multiple files and destination is a file then error */
	if ( !TESTFLAG(attr, A_D) && (isMultipleFile==TRUE) ) {
	    // "Cannot move > 1 file to another file\n"
	    printf (pszErrCannotMoveMultiple);
	    exit (1);
	}
    }

    erc = 0;
    while (pszBeginTokenString != NULL) {
	pszEndTokenString = strchr (pszBeginTokenString, ',');
	if (pszEndTokenString != NULL) {
	    *pszEndTokenString = '\0';
	    pszEndTokenString += 1;
	    }

	/* if the file is null, don't process it, otherwise we get an error message */
	if (*pszBeginTokenString != '\0') {

	    /* We may change drives in case of a directory move changing the CWD
	     * So keep track of the current drive.
	     */
	    _dos_getdrive(&cur_drive);	    /* remember current drive */

	    /* set CWD - to source file/dir drive. If the user changes
	     * the CWD for this drive, we will set the CWD to the root.
	     */
	    if (*(pszBeginTokenString+1) == ':')
		_dos_setdrive(toupper(*pszBeginTokenString) - 'A' + 1, &num_drives);

	    /* save current directory and drive */
	    pszCwd = getcwd (NULL,_MAX_PATH);


	    if (forfile (pszBeginTokenString, A_H | A_S | A_D, fConfirm) == FALSE)
		/* If forfile fails, it does not call fConfirm.  To avoid
		   duplicating error message code, we call fConfirm with
		   special parameters to force it to generate an error
		   message. */
		fConfirm (pszBeginTokenString,NULL,NULL);
	    }

	    /* if CWD has been changed we will remain an the root */
	    chdir("\\");			    /* change to root */
	    chdir(pszCwd);			    /* attempt to change dir to old CWD */
	    free((void *) pszCwd);

	    /* go back to the drive we originally came from */
	    _dos_setdrive(cur_drive, &num_drives);

	    pszBeginTokenString = pszEndTokenString;
	}

    exit (erc != 0);
}

