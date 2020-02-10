/*  enfile.c - perform generalized file enumeration
 *
 *  Modifications:
 *
 *	31-Jul-1986 mz	Use tools.h definitions
 *	29-Oct-1986 mz	Use c-runtime instead of Z-alike
 */

#include "..\h\tools.h"
#include <malloc.h>
#include <signal.h>
#include <string.h>
#include <direct.h>

static flagType iforfile (char *, char *, int, 
		void (*)(char *, struct findType *, va_list),
		va_list);

/*  EnFile - apply a procedure to all files that match the input pattern
 *
 *  pat 	pattern of file to enumerate
 *  attr	attribute to see
 *  rtn 	routine to call
 *  args	arguments to pass to routine
 */
int EnFile (pat, attr, rtn, ...)
char *pat;
int attr;
void (*rtn)(char *, struct findType *, va_list	);
{

    va_list	ap;	//unsigned args;
    flagType f = FALSE;

#if defined(DOS)
    int drive = -1;
    char *conn = (*tools_alloc) (MAXPATHLEN);
    char *rest = (*tools_alloc) (MAXPATHLEN);
    int (*sig)();


    if (conn != NULL && rest != NULL) {
	conn[0] = '\0';
	if (pat[0] && pat[1] == ':' && fPathChr (pat[2]) && fPathChr (pat[3]))
	    pat += 2;

	sig = signal (SIGINT, SIG_IGN);
	if ((drive = Connect (pat, conn, rest+2)) != -1) {
	    rest[0] = (char) (TOKTODRV(drive) +  'A');
	    rest[1] = ':';
	    pat = rest;
	    }
	va_start(ap, rtn);
	f = iforfile (conn, pat, attr, rtn, ap);
	va_end(ap);
	fDisconnect (drive);
	signal (SIGINT, sig);
	}

    if (conn != NULL)
	free (conn);
    if (rest != NULL)
	free (rest);

    return f;
#else
    va_start(ap, rtn);
    f = iforfile ("", pat, attr, rtn, ap);
    va_end(ap);
    return f;
#endif
}

static flagType iforfile (conn, pat, attr, rtn, ap)
char *conn;
char *pat;
int attr;
void (*rtn)(char *, struct findType *, va_list);
va_list ap;
{
    struct findType *fbuf = (struct findType *) (*tools_alloc) (sizeof (*fbuf));
    char *buf = (*tools_alloc) (MAXPATHLEN);
    flagType f = FALSE;
    va_list pass_ap;

    if (fbuf != NULL && buf != NULL && !ffirst (pat, attr, fbuf)) {
	if (conn[0] != 0)
	    strcpy (buf, conn);
	else
	    drive (pat, buf);
	path (pat, strend (buf));
	pat = strend (buf);

	do {
	    strcpy (pat, PFT_FOUNDNAME(fbuf));
	    /*	Assume the case correct form has been returned by ffirst/fnext
	     */
	    memcpy(pass_ap, ap, sizeof(va_list));
	    (*rtn) (buf, fbuf, pass_ap);
	    va_end(pass_ap);
	} while (!fnext (fbuf));
	f = TRUE;
	}

    if (buf != NULL)
	free (buf);
    if (fbuf != NULL) {
	findclose(fbuf);
	free ((char *) fbuf);
	}

    return f;
}
