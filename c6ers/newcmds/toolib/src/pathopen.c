/* reasonable imitation of logical names
 *
 *	4/14/86     dl	findpath: test for trailing && leading \ before
 *                          appending a \
 *	29-Oct-1986 mz	Use c-runtime instead of Z-alike
 *      03-Sep-1987 dl  fPFind: rtn nonzero iff exists AND is ordinary file
 *                      i.e., return false for directories
 *	11-Sep-1987 mz	Remove static declaration from findpath
 *	01-Sep-1988 bw	Allow $filenam.ext as a filename in findpath
 *	23-Nov-1988 mz	Use pathcat, allow $(VAR)
 */
#include "..\h\tools.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <string.h>

/* iterative routine takes args as pbuf, pfile */
static flagType fPFind (char *p, va_list ap)
{
    char *	pa[2];

     // Note that the call gives a pointer to the original args passed
     //     to pathopen rother than passing the parms themselves
    pa[1] = (char *)va_arg(ap, int *);
    pa[0] = (char *)va_arg(ap, int *);

    va_end(ap);

    /*  p == dir from env variable expansion or null
     *	pa[1] == file name
     *	pa[0] == buffer for getting p\pa[1] or pa[1] if p null
     */

    strcpy ((char *)pa[0], p);
    pathcat ((char *) pa[0], (char *) pa[1]);
#if defined(DOS) || defined(OS2)
    {
    struct stat sbuf;

    if (stat ((char *)pa[0], &sbuf) == -1)
	return FALSE;

    if ((sbuf.st_mode & S_IFREG) == 0)
	return FALSE;
    }
#elif defined(NT)
    {
        HANDLE TmpHandle;
        WIN32_FIND_DATA buffer;

        TmpHandle = FindFirstFile((LPSTR)pa[0],&buffer);

	if (TmpHandle == INVALID_HANDLE_VALUE)
            return FALSE;

        FindClose(TmpHandle);

	if (!(buffer.dwFileAttributes & FILE_ATTRIBUTE_NORMAL))
            return FALSE;
    }
#endif
    pname ((char *) pa[0]);
    return TRUE;
}

/*  $ENV:foo uses pathcat
 *  foo uses strcat
 */
flagType findpath (filestr, pbuf, fNew)
char *filestr, *pbuf;
flagType fNew;
{
    char *p;
    char *pathstr;
    size_t n;

    /*	Set pathstr to be text to walk or empty.
     *	Set filestr to be file name to look for.
     */
    pathstr = NULL;

    /*	Are we starting $ENV: or $(ENV)?
     */
    if (*filestr == '$')

	/*  Are we starting $(ENV)?
	 */
	if (filestr[1] == '(')

	    /*	Do we have $(ENV)?
	     */
	    if ((p = strchr (filestr, ')')) != NULL) {
		*p = 0;
		pathstr = getenv (filestr + 2);
		*p++ = ')';
		filestr = p;
		}
	    else
		;
	else
	/*  Do we have $ENV: ?
	 */
	if ((p = strchr (filestr, ':')) != NULL) {
	    *p = 0;
	    pathstr = getenv (filestr + 1);
	    *p++ = ':';
	    filestr = p;
	    }
	else
	    ;
    else
	;

    /*	Convert pathstr into true string
     */
    if (pathstr == NULL)
	pathstr = "";

    /*	If we find an existing file in the path
     */
    if (forsemi (pathstr, fPFind, filestr, pbuf))
	return TRUE;

    /*	If this is not a new file
     */
    if (!fNew)
        return FALSE;

    /*	File does not exist.  Take first dir from pathstr and use it
     *	as prefix for result
     */
    p = strchr (pathstr, ';');
    n = ((p == NULL) ? strlen (pathstr) : p - pathstr);
    strncpy (pbuf, pathstr, n);
    pbuf[n] = '\0';

    if (*pathstr == 0)
	strcat (pbuf, filestr);
    else
	pathcat (pbuf, filestr);

    return TRUE;
}

FILE *pathopen (name, buf, mode)
char *name, *mode, *buf;
{
    return findpath (name, buf, TRUE) ? fopen (buf, mode) : NULL;
}
