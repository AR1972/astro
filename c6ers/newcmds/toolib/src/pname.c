/***	pname.c - form a "pretty" version of a user file name
 *
 *	OS/2 v1.2 and later will allow filenames to retain the case
 *	when created while still being case insensitive for all operations.
 *	This allows the user to create more visually appealing file names.
 *
 *	All runtime routines should, therefore, preserve the case that was
 *	input.	Since the user may not have input in the case that the entries
 *	were created, we provide a service whereby a pathname is adjusted
 *	to be more visually appealing.	The rules are:
 *
 *	if (real mode)
 *	    lowercase the sucker
 *	else
 *	if (version is <= 1.1)
 *	    lowercase the sucker
 *	else
 *	if (filesystem is FAT)
 *	    lowercase the sucker
 *	else
 *	    for each component starting at the root, use DosFindFirst
 *		to retrieve the original case of the name.
 *
 *	Modifications:
 *	    10-Oct-1989 mz  First implementation
 *
 */

#if defined (OS2)
#define INCL_ERRORS
#define INCL_DOSFILEMGR
#define INCL_DOSMODULEMGR
#include <os2.h>
#endif

#include <string.h>
#include <stdlib.h>
#include <malloc.h>

#include "..\h\tools.h"

char *pname (char *pszName)
{
#if defined (DOS)
    return strlwr (pszName);
#else
    if (!IsMixedCaseSupported (pszName))
	return strlwr (pszName);

    /*	The underlying file system supports mixed case.  Iterate through
     *	the path doing find-firsts to elicit the correct case from the
     *	file system
     */
    {
#if defined(OS2)
	HDIR		hdir;
	FILEFINDBUF	findbuf;
#elif defined(NT)
	HANDLE		hdir;
	WIN32_FIND_DATA findbuf;
#endif
	unsigned int	cFound;
	char *pszSrc, *pszDst, *pszEnd, chEnd;

	/*  skip drive and leading / if present */
	pszDst = pszName;
	if (pszDst[1] == ':')
	    pszDst += 2;

	/*  We skip to first filename component.  If the path is UNC, then
	 *  we skip over machine name and sharename and LC them.
	 */

	if (fPathChr (*pszDst)) {
	    pszDst++;
	    if (fPathChr (*pszDst)) {
		pszDst = strbscan (pszDst + 1, "/\\");
		if (*pszDst != '\0')
		    pszDst = strbscan (pszDst + 1, "/\\");

		/*  pszDst points to the slash following the share name or
		 *  points to '\0' for mere computer or computer\share
		 */
		chEnd = *pszDst;
		*pszDst = '\0';
		strlwr (pszName);
		if ((*pszDst++ = chEnd) == '\0')
		    return pszName;
		}
	    }

	/*  If we are pointing at the root, just give up
	 */
	if (*pszDst == '\0')
	    return pszName;

	/*  pszDst points to first char of first component
	 */

	pszSrc = pszDst;
	while (TRUE) {
	    /*	Find and terminate next component after pszSrc
	     */
	    pszEnd = strbscan (pszSrc, "/\\");
	    chEnd = *pszEnd;
	    *pszEnd = 0;

	    /*	Pack next component up against pszDst
	     */
	    strcpy (pszDst, pszSrc);

	    /*	Use DosFindFirst to return "canonical" case-correct
	     *	version of last component EXCEPT if meta chars present
	     */
#if defined(OS2)
	    hdir = HDIR_CREATE;
#endif
	    cFound = 1;
	    if (*strbscan (pszDst, "*?") == 0 &&
		strcmp (pszDst, ".") && strcmp (pszDst, "..") &&
#if defined(OS2)
		DosFindFirst2 (pszName,
			      &hdir,
			      FILE_HIDDEN + FILE_SYSTEM + FILE_DIRECTORY,
			      &findbuf,
			      sizeof (findbuf),
			      &cFound,
			      FIL_STANDARD,
			      0L) == 0
#elif defined(NT)
		( ( hdir = FindFirstFile( pszName, &findbuf ) ) != (HANDLE)-1 )
#endif
		) {
		/*  The entry was found and is valid.  Go and
		 *  copy it from the returned buffer
		 */
#if defined(OS2)
#define FOUNDNAME   findbuf.achName
#elif defined(NT)
#define FOUNDNAME   findbuf.cFileName
#endif
		strcpy (pszDst, FOUNDNAME);

		/*  Release search handle
		 */
#if defined(OS2)
#define CLOSEFIND   DosFindClose
#elif defined(NT)
#define CLOSEFIND   FindClose
#endif
		CLOSEFIND (hdir);
		}
	    else
		/*  Meta char present or search did not find file
		 *  Just leave it alone
		 */
		;

	    /*	pszName points to name being constructed
	     *	pszDst	points to correctly-formed last component
	     *	chEnd	contains character that terminated last component
	     *	pszEnd	is where chEnd was retrieved from
	     */

	    /*	Terminate newly found component
	     */
	    pszDst += strlen (pszDst);
	    *pszDst++ = '\\';

	    /*	If we were at end of string,
	     *	then we're all done
	     */
	    if (chEnd == '\0') {
		pszDst[-1] = '\0';
		break;
		}

	    /*	Set pszSrc to point to beginning of next component
	     *	We can do this because we haven't seen a terminating NUL
	     */
	    pszSrc = pszEnd + 1;
	    }
	return pszName;
    }
#endif
}

#if defined (OS2) || defined(NT)
/*	IsMixedCaseSupported - determine if a file system supports mixed case
 *
 *	We presume that all OS's prior to OS/2 1.2 or FAT filesystems
 *	do not support mixed case.  It is up to the client to figure
 *	out what to do.
 *
 *	We presume that non FAT filesystems on 1.2 and later DO support mixed
 *	case
 *
 *	We do some caching to prevent redundant calls to the file systems.
 *
 *	returns     TRUE    (MCA_SUPPORT) if it is supported
 *		    FALSE   (MCA_NOTSUPP) if unsupported
 *
 */
#define MCA_UNINIT	123
#define MCA_SUPPORT	TRUE
#define MCA_NOTSUPP	FALSE

static	USHORT mca[27] = { MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT,
			   MCA_UNINIT, MCA_UNINIT, MCA_UNINIT };

USHORT (pascal far *pDosQPathInfo) (PSZ, USHORT, PBYTE, USHORT, ULONG);

USHORT static QueryMixedCaseSupport (char *psz)
{
#if defined(NT)
    psz;	//unreferenced

    return MCA_NOTSUPP;
#elif defined(OS2)

    UCHAR *pUpdPath;
    FILESTATUS fsts;
    USHORT erc;

    /*	If OS/2 before 1.2, presume no mixed case support
     */
    if (_osmajor < 10 || (_osmajor == 10 && _osminor < 2))
	return MCA_NOTSUPP;

    /*	Attach to v1.2 function
     */
    if (pDosQPathInfo == NULL) {
	HMODULE hmod;

	if (DosGetModHandle ("DOSCALLS", &hmod) != 0)
	    return MCA_NOTSUPP;

	if (DosGetProcAddr (hmod, "#98", (PPFN) &pDosQPathInfo) != 0) {
	    pDosQPathInfo = NULL;
	    return MCA_NOTSUPP;
	    }
	}

    pUpdPath = (*tools_alloc) (MAXPATHLEN);
    if (pUpdPath == NULL)
	return MCA_NOTSUPP;

    /*	Replace lowest path component with "." to force a directory
     */
    upd (psz, ".", pUpdPath);

    /*	Retrieve timestamp info
     */
    erc = (*pDosQPathInfo) (pUpdPath,
			    FIL_STANDARD,
			    (PBYTE) &fsts,
			    sizeof (fsts),
			    0L);
    free (pUpdPath);

    /*	If no error and if there was a creation time on the dir
     *	then we presume the mixed case is supported
     */
    if (erc == NO_ERROR && MAKETYPE (fsts.fdateCreation, USHORT) != 0)
	return MCA_SUPPORT;
    else
	return MCA_NOTSUPP;
#endif
}

USHORT IsMixedCaseSupported (char *psz)
{
    USHORT mcaSupp;
    unsigned int uDrvOrd;
    BOOL fUNC;
#if defined (OS2)
    ULONG  ulDrvMap;
#endif

    fUNC = (fPathChr (psz[0]) && fPathChr (psz[1])) ||
	    (psz[0] != 0 && psz[1] == ':' &&
	     fPathChr (psz[2]) && fPathChr (psz[3]));

    /*	Obtain drive ordinal and return cached value if valid
     */
    if (!fUNC) {
	if (psz[0] != 0 && psz[1] == ':')
	    uDrvOrd = (psz[0] | 0x20) - 'a' + 1;
	else {
#if defined (OS2)
	    DosQCurDisk (&uDrvOrd, &ulDrvMap);
#elif defined(NT)
            char buf[5];

            GetCurrentDirectory( 5, buf );
	    uDrvOrd = ( buf[0] | 0x20 ) - 'a' + 1;
#endif
	}

	if (mca[uDrvOrd] != MCA_UNINIT)
	    return mca[uDrvOrd];
	}

    /*	Get support value
     */
    mcaSupp = QueryMixedCaseSupport (psz);

    if (!fUNC)
	mca[uDrvOrd] = mcaSupp;

    return mcaSupp;
}
#endif
