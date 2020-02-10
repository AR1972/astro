/* find.c - MSDOS find first and next matching files
 *
 *	09-Dec-1986 bw	Added DOS5 support
 *	24-Feb-1987 bw	Define findclose() function.
 *	30-Oct-1987 bw	Change 'DOS5' to 'OS2'
 *	08-Dec-1988 mz	Add net enum
 */

#if defined (DOS)
#include <dos.h>

#include <string.h>
#include "..\h\tools.h"

/*  ffirst - begin find enumeration given a pattern
 *
 *  file	char pointer to name string with pattern in last component.
 *  attr	inclusive attributes for search
 *  fbuf	pointer to buffer for find stuff
 *
 *  returns	(DOS) TRUE if error, FALSE if success
 *		(OS2) error code or NO_ERROR
 */

/*  fnext - continue find enumeration
 *
 *  fbuf	pointer to find buffer
 *
 *  returns	(DOS) TRUE if error, FALSE if success
 *		(OS2) error code or NO_ERROR
 */

/*  findclose - release system resources upon find completion
 *
 *  Allows z runtime and filesystem to release resources
 *
 *  fbuf	pointer to find buffer
 */

ffirst (file, attr, fbuf)
char *file;
int attr;
struct findType *fbuf;
{
    union REGS regs;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    struct SREGS sregs;
#endif

    /* point DTA to buffer */
    regs.h.ah = 0x1A;
#if (defined(M_I86CM) || defined(M_I86LM) || defined(M_I86HM))
    segread(&sregs);
    sregs.ds = FP_SEG(fbuf);
    regs.x.dx = FP_OFF(fbuf);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.dx = (unsigned) fbuf;
    intdos (&regs, &regs);
#endif
    /* issue find first call */
    regs.h.ah = 0x4e;
    regs.x.cx = attr;
#if (defined(M_I86CM) || defined(M_I86LM) || defined(M_I86HM))
    segread(&sregs);
    sregs.ds = FP_SEG(file);
    regs.x.dx = FP_OFF(file);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.dx = (unsigned) file;
    intdos (&regs, &regs);
#endif
    if (!regs.x.cflag)
	strlwr (fbuf->name);
    return regs.x.cflag;
}

fnext (fbuf)
struct findType *fbuf;
{
    union REGS regs;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    struct SREGS sregs;
#endif

    /* point DTA to buffer */
    regs.h.ah = 0x1A;
#if (defined(M_I86CM) || defined(M_I86LM) || defined(M_I86HM))
    segread(&sregs);
    sregs.ds = FP_SEG(fbuf);
    regs.x.dx = FP_OFF(fbuf);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.dx = (unsigned) fbuf;
    intdos (&regs, &regs);
#endif
    /* issue find next call */
    regs.h.ah = 0x4F;
#if (defined(M_I86CM) || defined(M_I86LM) || defined(M_I86HM))
    sregs.ds = FP_SEG(fbuf);
    intdosx(&regs, &regs, &sregs);
#else
    intdos (&regs, &regs);
#endif
    if (!regs.x.cflag)
	strlwr (fbuf->name);
    return regs.x.cflag;
}


void findclose (fbuf)
struct findType *fbuf;
{
}

#elif defined(OS2) || defined(NT)

#define INCL_DOSERRORS
#define INCL_DOSMODULEMGR
#if defined(OS2)
#include <os2.h>
#elif defined(NT)
#include <nt.h>
#include <ntrtl.h>
#include <nturtl.h>
#include <windows.h>
#endif
#include "..\h\tools.h"

#include <malloc.h>
#include <string.h>
#include <stdio.h>

#if defined (OS2)
#include <netcons.h>
#include <neterr.h>
#include <server.h>
#include <shares.h>
#include <access.h>
#include <wksta.h>
#elif defined (NT)
#define _LM_
#include <lmcons.h>
#include <lmerr.h>
#include <lmserver.h>
#include <lmshare.h>
#include <lmaccess.h>
#include <lmwksta.h>
#endif

#define CCHPAT	128
#if defined(OS2)
#define STATUS_OK		NO_ERROR
#define NO_MORE_FILES		ERROR_NO_MORE_FILES
typedef struct share_info_1	SHARE_INFO_1;
typedef struct server_info_0	SERVER_INFO_100;
#define SV_NAME 		sv0_name
#define CLOSEFIND		DosFindClose
#elif defined(NT)
#define SRCHATTR		(FILE_ATTRIBUTE_HIDDEN | \
				 FILE_ATTRIBUTE_SYSTEM | \
				 FILE_ATTRIBUTE_DIRECTORY)
static BOOL AttributesMatch( NPFIND fbuf );
#define CLOSEFIND		FindClose
#define SV_NAME 		sv100_name
#endif

#if defined(OS2)
HANDLE hmodOEM;
#endif
HANDLE hmodAPI;

#if defined(OS2)
USHORT (pascal FAR *pNetServerGetInfo) (const char FAR *,
					short,
					char FAR *,
					unsigned short,
					unsigned short FAR *);

USHORT (pascal FAR *pNetServerEnum) (const char FAR *,
				     short,
				     char FAR *,
				     unsigned short,
				     unsigned short FAR *,
				     unsigned short FAR *);

USHORT (pascal FAR *pNetShareEnum) (const char FAR *,
				    short,
				    char FAR *,
				    unsigned short,
				    unsigned short FAR *,
				    unsigned short FAR *);
#elif defined (NT)
NET_API_STATUS (NET_API_FUNCTION
*pNetServerEnum) (
    IN  LPTSTR      servername OPTIONAL,
    IN  DWORD       level,
    OUT LPBYTE      *bufptr,
    IN  DWORD       prefmaxlen,
    OUT LPDWORD     entriesread,
    OUT LPDWORD     totalentries,
    IN  DWORD       servertype,
    IN  LPTSTR      domain OPTIONAL,
    IN OUT LPDWORD  resume_handle OPTIONAL
    );

NET_API_STATUS (NET_API_FUNCTION
*pNetServerGetInfo) (
    IN  LPTSTR  servername OPTIONAL,
    IN  DWORD   level,
    OUT LPBYTE  *bufptr
    );

NET_API_STATUS (NET_API_FUNCTION
*pNetShareEnum) (
    IN  LPTSTR      servername,
    IN  DWORD       level,
    OUT LPBYTE      *bufptr,
    IN  DWORD       prefmaxlen,
    OUT LPDWORD     entriesread,
    OUT LPDWORD     totalentries,
    IN OUT LPDWORD  resume_handle
    );
#endif


typedef SERVER_INFO_100 SI;
typedef struct serverbuf {
    USHORT csi; 			/* count of si's in array */
    USHORT isiLast;			/* index of last returned server */
    int    attr;			/* attribute of search */
    BYTE   szPattern[CCHPAT];		/* pattern for matching */
    SI	   asi[1];			/* array of server blocks */
    } SB;
typedef SB FAR * PSB;

typedef SHARE_INFO_1 SHI;
typedef struct sharebuf {
    USHORT cshi;			/* count of shi's in array */
    USHORT ishiLast;			/* index of last returned share */
    int    attr;			/* attribute of search */
    BYTE   szServer[CCHPAT];		/* server name */
    BYTE   szPattern[CCHPAT];		/* pattern for matching */
    SHI    ashi[1];			/* array of share blocks */
    } SHB;
typedef SHB FAR * PSHB;

static int	 usFileFindNext (struct findType *fbuf);

static int	 usSharFindFirst (char *npsz, int attr, struct findType *fbuf);
static int	 usSharFindNext (struct findType *fbuf);
static int	 usServFindFirst (char *npsz, int attr, struct findType *fbuf);
static int	 usServFindNext (struct findType *fbuf);
static int	 usLoadNet (void);

#if defined(OS2)
#define InNetServerGetInfo	    (*pNetServerGetInfo)
#define InNetServerEnum 	    (*pNetServerEnum)
#define InNetShareEnum		    (*pNetShareEnum)
#elif defined(NT)
static InNetServerGetInfo (const char *pszServer,
				short sLevel,
				char *pbBuffer,
				unsigned short cbBuffer,
				unsigned short *pcbTotalAvail);

static InNetServerEnum (const char *pszServer,
			     short sLevel,
			     char * pbBuffer,
			     unsigned short cbBuffer,
			     unsigned short *pcEntriesRead,
			     unsigned short *pcTotalAvail);

static InNetShareEnum (const char *pszServer,
			     short sLevel,
			     char * pbBuffer,
			     unsigned short cbBuffer,
			     unsigned short *pcEntriesRead,
			     unsigned short *pcTotalAvail);
#endif

/*  returns error code or NO_ERROR
 */
int ffirst (char *file, int attr, struct findType *fbuf)
{
    int erc;

    fbuf->type = FT_DONE;

    {	char *p = file;
#if !defined(NT) || defined(_LM_)
	char *p1;
#else
	UNREFERENCED_PARAMETER( attr );
#endif // !NT || _LM_

	/*  We need to handle the following cases:
	 *
	 *  [D:]\\pattern
	 *  [D:]\\machine\pattern
	 *  [D:]\\machine\share\pattern
	 *  [D:]path\pattern
	 */

	/*  skip drive
	 */
	if (p[0] != 0 && p[1] == ':')
	    p += 2;

#if !defined(NT) || defined(_LM_)
	/*  If UNC present
	 */

	if (fPathChr (p[0]) && fPathChr (p[1]))
	    /*	If not fully specified then set up server enumerate
	     */
	    if (*(p1 = strbscan (p + 2, "/\\")) == 0) {
		erc = usLoadNet ();
		if (erc == 0)
		    erc = usServFindFirst (p, attr, fbuf);
		goto casefix;
		}
	    else {
		p1 = strbskip (p1, "/\\");
		if (*strbscan (p1, "/\\") == 0) {
		    erc = usLoadNet ();
		    if (erc == 0)
			erc = usSharFindFirst (p, attr, fbuf);
		    goto casefix;
		    }
		}
#endif // !NT || _LM_
    }

    fbuf->type = FT_FILE;
#if defined(OS2)
    {	unsigned SearchCount = 1;

	fbuf->type = FT_FILE;
	fbuf->dir_handle = 0xFFFF;		 /*  Give me a directory handle */

	erc =  DosFindFirst
		(   file ,			    /*		File path name */
		    (PHDIR)&fbuf->dir_handle,	    /* Directory search handle */
		    attr & A_ALL & ~A_V ,	    /*	      Search attribute */
		    (PFILEFINDBUF)&fbuf->create_date,
						    /*		 Result buffer */
		    sizeof(*fbuf) - sizeof(fbuf->dir_handle) ,
						    /*	  Result buffer length */
		    (PUSHORT)&SearchCount,	    /*	  # of entries to find */
		    0L				    /* Reserved (must be zero) */
		);
    }
#elif defined(NT)
    {
        fbuf->attr = attr;
        erc = ( ( fbuf->dir_handle = FindFirstFile( file, &( fbuf->fbuf ) ) ) == (HANDLE)-1 ) ? 1 : 0;
	if ( (!erc) && !AttributesMatch( fbuf ) ) {
            erc = fnext( fbuf );
        }
    }
#endif

casefix:
    if (
#if defined(OS2)
	erc == NO_ERROR
#elif defined(NT)
	fbuf->dir_handle != (HANDLE)-1
#endif
    )
	if (!IsMixedCaseSupported (file))
	    strlwr (PFT_FOUNDNAME(fbuf));
	else
	    SETFLAG (fbuf->type, FT_MIX);

    return erc;
}

fnext (struct findType *fbuf)
{
    int erc;

    switch (fbuf->type & FT_MASK ) {
    case FT_FILE:
	erc = usFileFindNext (fbuf);
	break;
#if !defined(NT) || defined(_LM_)
    case FT_SERV:
	erc = usServFindNext (fbuf);
	break;
    case FT_SHAR:
	erc = usSharFindNext (fbuf);
	break;
#endif // !NT || _LM_
    default:
	erc = ERROR_NO_MORE_FILES;
	}

    if (erc == NO_ERROR && !TESTFLAG (fbuf->type, FT_MIX))
	strlwr (PFT_FOUNDNAME(fbuf));
    return erc;
}

void findclose (struct findType *fbuf)
{
    switch (fbuf->type & FT_MASK ) {
    case FT_FILE:
	CLOSEFIND (fbuf->dir_handle);
	break;
#if !defined(NT) || defined(_LM_)
    case FT_SERV:
    case FT_SHAR:
#if defined(OS2)
	DosFreeSeg ((SEL) fbuf->dir_handle);
#else
	free( (void *)fbuf->dir_handle );
#endif
	break;
#endif // !NT || _LM_
	}
    fbuf->type = FT_DONE;
}

#if defined(NT)

static BOOL AttributesMatch( NPFIND fbuf )
{
    //
    //	We emulate the OS/2 behaviour of attribute matching. The semantics
    //	are evil, so I provide no explanation.
    //
    fbuf->fbuf.dwFileAttributes &= (0x000000FF & ~(FILE_ATTRIBUTE_NORMAL));

    if (! ((fbuf->fbuf.dwFileAttributes & SRCHATTR) & ~(fbuf->attr))) {
        return TRUE;
    } else {
        return FALSE;
    }
}

#endif	//NT


/*  Find next routines
 */



static int
usFileFindNext (struct findType *fbuf)
{
#if defined(OS2)

    unsigned SearchCount = 1;

    return DosFindNext (fbuf->dir_handle,
			(PFILEFINDBUF)&fbuf->create_date,
			sizeof(*fbuf) - sizeof(fbuf->dir_handle),
			(PUSHORT)&SearchCount);

#elif defined(NT)

    while ( TRUE ) {
	if ( !FindNextFile( fbuf->dir_handle, &( fbuf->fbuf ) ) )
	    return GetLastError();
	else if ( AttributesMatch( fbuf ) )
	    return NO_ERROR;
	}
    // return( FindNextFile( fbuf->dir_handle, &( fbuf->fbuf ) ) );

#endif
}

#if !defined(NT) || defined(_LM_)

static int
usServFindNext (struct findType *fbuf)
{
    BYTE szPattern[CCHPAT];
    PSB psb;
    SI si;
    USHORT isiNext;

#if defined(OS2)
    SELECTOROF (psb) = fbuf->dir_handle;
    OFFSETOF (psb) = 0;
#elif defined(NT)
    // Retrive the psb stored in fbuf->dir_handle
    psb = (PSB)fbuf->dir_handle;
#endif

    Move (psb->szPattern, szPattern, CCHPAT);
    for (isiNext = (USHORT)(psb->isiLast + 1); isiNext < psb->csi; isiNext++) {
	si = psb->asi[isiNext];
	if (TESTFLAG (psb->attr, A_D) && fMatch (szPattern, si.SV_NAME)) {
	    psb->isiLast = isiNext;
#if defined(OS2)
	    fbuf->create_date = fbuf->create_time = 0;
	    fbuf->access_date = fbuf->access_time = 0;
	    fbuf->date = fbuf->time = 0;
	    fbuf->length = fbuf->alloc = 0;
	    fbuf->attr = A_D;
	    strcpy (fbuf->name, si.SV_NAME);
	    fbuf->nam_len = (unsigned char) strlen (fbuf->name);
#elif defined(NT)
	    fbuf->fbuf.ftCreationTime.dwLowDateTime = 0;
	    fbuf->fbuf.ftCreationTime.dwHighDateTime = 0;
	    fbuf->fbuf.ftLastAccessTime.dwLowDateTime = 0;
	    fbuf->fbuf.ftLastAccessTime.dwHighDateTime = 0;
	    fbuf->fbuf.ftLastWriteTime.dwHighDateTime = 0;
	    fbuf->fbuf.ftLastWriteTime.dwLowDateTime = 0;
	    fbuf->fbuf.nFileSizeLow = 0;
	    fbuf->fbuf.nFileSizeHigh = 0;
	    fbuf->attr = A_D;
	    fbuf->fbuf.dwFileAttributes = A_D;
	    strcpy (fbuf->fbuf.cFileName, si.SV_NAME);
#endif
	    return NO_ERROR;
	    }
	}
    return ERROR_NO_MORE_FILES;
}

/***	usServFindFirst - set up findbuf to do enumeration on server
 */
static int
usServFindFirst (char *npsz, int attr, struct findType *fbuf)
{
    PSB     psb;
    int     erc;
    USHORT  read, total;
    char    *npszServ = NULL;

    /*	If there is no meta matching, just see if server exists
     */
    if (*strbscan (npsz, "?*") == 0) {
	npszServ = npsz;

#if defined(OS2)
	erc = DosAllocSeg (sizeof(SB),
			   (PSEL) & fbuf->dir_handle,
			   0x0000);
	if (erc != 0)
	    return erc;

	SELECTOROF (psb) = fbuf->dir_handle;
	OFFSETOF (psb) = 0;
#elif defined(NT)
    if( ( psb = (PSB)malloc( sizeof(SB) ) ) == NULL )
	return ERROR_NOT_ENOUGH_MEMORY;
#endif
	/* Get array of server info
	 */
	erc = InNetServerGetInfo   (npszServ,
				    0,
				    (PBYTE) psb->asi,
				    sizeof(SI),
				    &total);

	psb->csi = 1;
	psb->isiLast = -1;
	}

    /*	Get list of known servers
    */
    else {
	/*  Adjust pattern
	 */
	if (!strcmp (npsz + 2, "*.*") || !strcmp (npsz + 2, "*."))
	    strcpy (npsz + 2, "*");

	/* Find number of servers
	 */
	erc = InNetServerEnum (npszServ,
				 0,
				 NULL,
				 0,
				 &read,
				 &total);

	if (erc != ERROR_MORE_DATA || total == 0)
	    if (erc != 0)
		return erc;
	    else
		return ERROR_NO_MORE_FILES;

	/* The LANMAN API's are useless in that it is difficult to get an accurate
	 * count of bytes necessary to store a particular enumeration.	As a
	 * result, we allocate a max-sized segment and waste space
	 */
#if defined(OS2)
	erc = DosAllocSeg (0xFFFF,
			   (PSEL) & fbuf->dir_handle,
			   0x0000);
	if (erc != 0)
	    return erc;

	SELECTOROF (psb) = fbuf->dir_handle;
	OFFSETOF (psb) = 0;
#elif defined(NT)
    if( ( psb = (PSB)malloc( 0xFFFF ) ) == NULL )
	return ERROR_NOT_ENOUGH_MEMORY;
#endif

	/*  Get array of server info
	 */
	erc = InNetServerEnum (npszServ,
				 0,
				 (PBYTE) psb->asi,
				 0xFFFF - sizeof (SB),
				 &read,
				 &total);

	psb->csi = read;
	psb->isiLast = -1;
	}

    /*	Fill in search attributes
     */
    psb->attr = attr;
    Fill (psb->szPattern, 0, CCHPAT);
    Move (npsz + 2, psb->szPattern, min (CCHPAT-1, strlen (npsz + 2)));
    fbuf->type = FT_SERV;

    /*	Find/return first matching
     */
    if (fnext (fbuf)) {
	Fill ((char far *) fbuf, 0, sizeof (*fbuf));
#if defined(OS2)
	DosFreeSeg (SELECTOROF (psb));
#elif defined(NT)
	free (psb);
#endif
	return ERROR_NO_MORE_FILES;
	}
    return NO_ERROR;
}

/***	usSharFindFirst - begin enumeration of share set
 *
 *	npsz	pointer to pattern of the form \\machine\pattern
 *	attr	attribute allowing search
 *	fbuf	buffer for find poop
 */
static int
usSharFindFirst (char *npsz, int attr, struct findType *fbuf)
{
    char    *npszPat = strbscan (npsz + 2, "\\/");
    char    *npsz1 = npszPat;
    BYTE    c = *npszPat;
    PSHB    pshb;
    int     erc;
    USHORT  read, total;

    *npszPat++ = 0;

    /*	npszPat now points to pattern portion
     *	Adjust pattern
     */
    if (!strcmp (npszPat, "*.*") || !strcmp (npszPat, "*."))
	npszPat = "*";

    /*	Find number of shares
     */
    erc = InNetShareEnum (npsz,
			    0,
			    NULL,
			    0,
			    &read,
			    &total);

    if (erc != ERROR_MORE_DATA || total == 0) {
	*npsz1 = c;
	if (erc != 0)
	    return erc;
	else
	    return ERROR_NO_MORE_FILES;
	}

    /*	Allocate segment for share data
     */
#if defined (OS2)
    erc = DosAllocSeg (0xFFFF,
		       (PSEL) & fbuf->dir_handle,
		       0x0000);
    if (erc != 0) {
	*npsz1 = c;
	return erc;
	}

    SELECTOROF (pshb) = fbuf->dir_handle;
    OFFSETOF (pshb) = 0;
#elif defined(NT)
    if( ( pshb = malloc( 0xFFFF ) ) == NULL ) {
	*npsz1 = c;
	return ERROR_NOT_ENOUGH_MEMORY;
    }

    // Store the pointer to the memory block so it may be used again for the
    // next file - cast it to a unsigned and store it in fbuf->dir_handle.
    // When retrieved, cast it back to a pointer.

    fbuf->dir_handle = ( void * )pshb;
#endif

    /*	Get array of server info
     */
    erc = InNetShareEnum (npsz,
			    1,
			    (PBYTE) pshb->ashi,
			    0xFFFF - sizeof (SHB),
			    &read,
			    &total);

    pshb->cshi = read;
    pshb->ishiLast = -1;
    pshb->attr = attr;
    Fill (pshb->szPattern, 0, CCHPAT);
    Fill (pshb->szServer,  0, CCHPAT);
    Move (npszPat, pshb->szPattern, min (CCHPAT-1, strlen (npszPat)));
    Move (npsz,    pshb->szServer,  min (CCHPAT-1, strlen (npsz)));
    *npsz1 = c;
    fbuf->type = FT_SHAR;

    /*	Find/return first matching
     */
    if (fnext (fbuf)) {
	Fill ((char far *) fbuf, 0, sizeof (*fbuf));
#if defined(OS2)
	DosFreeSeg (SELECTOROF (pshb));
#elif defined(NT)
	free( pshb );
#endif
	return ERROR_NO_MORE_FILES;
	}
    return NO_ERROR;
}

static int
usSharFindNext (struct findType *fbuf)
{
    BYTE szPattern[CCHPAT];
    PSHB pshb;
    SHI shi;
    USHORT ishiNext;

    // Retrive the psb stored in fbuf->dir_handle
#if defined (OS2)
    SELECTOROF (pshb) = fbuf->dir_handle;
    OFFSETOF (pshb) = 0;
#elif defined (NT)
    pshb = (PSHB)fbuf->dir_handle;
#endif

    Move (pshb->szPattern, szPattern, CCHPAT);
    for (ishiNext = (USHORT)(pshb->ishiLast + 1); ishiNext < pshb->cshi; ishiNext++) {
	shi = pshb->ashi[ishiNext];
	if (shi.shi1_netname[strlen (shi.shi1_netname) - 1] != '$' &&
	    fMatch (szPattern, shi.shi1_netname)) {

	    if (shi.shi1_type != STYPE_DISKTREE || TESTFLAG (pshb->attr, A_D)) {
		pshb->ishiLast = ishiNext;
#if defined(OS2)
		fbuf->create_date = fbuf->create_time = 0;
		fbuf->access_date = fbuf->access_time = 0;
		fbuf->date = fbuf->time = 0;
		fbuf->length = fbuf->alloc = 0;
		fbuf->attr = shi.shi1_type == STYPE_DISKTREE ? A_D : 0;
		strcpy (fbuf->name, shi.shi1_netname);
		fbuf->nam_len = (UCHAR) strlen (fbuf->name);
#elif defined(NT)
		fbuf->fbuf.ftCreationTime.dwLowDateTime = 0;
		fbuf->fbuf.ftCreationTime.dwHighDateTime = 0;
		fbuf->fbuf.ftLastAccessTime.dwLowDateTime = 0;
		fbuf->fbuf.ftLastAccessTime.dwHighDateTime = 0;
		fbuf->fbuf.ftLastWriteTime.dwHighDateTime = 0;
		fbuf->fbuf.ftLastWriteTime.dwLowDateTime = 0;
		fbuf->fbuf.nFileSizeLow = 0;
		fbuf->fbuf.nFileSizeHigh = 0;
		fbuf->fbuf.dwFileAttributes =
		  fbuf->attr = shi.shi1_type == STYPE_DISKTREE ? A_D : 0;
		strcpy (fbuf->fbuf.cFileName, shi.shi1_netname);
#endif
		return NO_ERROR;
		}
	    }
	}
    return ERROR_NO_MORE_FILES;
}


static int
usLoadNet ()
{
#if defined(OS2)
    int erc;
#endif

    if (hmodAPI != 0)
	return NO_ERROR;

#if defined(OS2)
    if ((erc = DosLoadModule (NULL, 0, "netoem", &hmodOEM)) != 0)
	return erc;
    if ((erc = DosGetProcAddr (hmodOEM, "NETSERVERENUM", (PFN far *)&pNetServerEnum)) != 0)
	return erc;
    if ((erc = DosLoadModule (NULL, 0, "netapi", &hmodAPI)) != 0)
	return erc;
    if ((erc = DosGetProcAddr (hmodAPI, "NETSERVERGETINFO", (PFN far *)&pNetServerGetInfo)) != 0)
	return erc;
    return DosGetProcAddr (hmodAPI, "NETSHAREENUM", (PFN far *)&pNetShareEnum);
#elif defined(NT)
    if( !( hmodAPI = LoadLibrary( "netapi32" ) )			  ||
#pragma message ("Known different function parameter lists")
	!( pNetServerEnum = GetProcAddress( hmodAPI, "NETSERVERENUM" ) ) ||
#pragma message ("Known different function parameter lists")
	!( pNetShareEnum = GetProcAddress( hmodAPI, "NETSHAREENUM" ) )	||
#pragma message ("Known different function parameter lists")
	!( pNetServerGetInfo = GetProcAddress( hmodAPI, "NETSERVERGETINFO" ) ) )
        return( GetLastError() );
    else
	return( 0 );
#endif
}

#endif // !NT || _LM_

#if defined(NT)
static InNetServerGetInfo (const char *pszServer,
					short sLevel,
					char *pbBuffer,
					unsigned short cbBuffer,
					unsigned short *pcbTotalAvail)
{
int		status;
LPBYTE		bufGiven;

status = (*pNetServerGetInfo) (
IN	(LPTSTR)	pszServer,
IN	(DWORD)		sLevel,
OUT	(LPBYTE *)	&bufGiven
    );
memmove(pbBuffer, bufGiven, cbBuffer);
free(bufGiven);
*pcbTotalAvail = 0;	//Return isn't used here
return status;
}

static InNetServerEnum (const char *pszServer,
				     short sLevel,
				     char * pbBuffer,
				     unsigned short cbBuffer,
				     unsigned short *pcEntriesRead,
				     unsigned short *pcTotalAvail)
{
DWORD		entriesread, totalentries;
LPBYTE		bufGiven;
int		status;

status = (*pNetServerEnum) (
IN	(LPTSTR)	pszServer,
IN	(DWORD)		sLevel,
OUT	(LPBYTE *)	&bufGiven,
IN	(DWORD)		cbBuffer,
OUT	(LPDWORD)	&entriesread,
OUT	(LPDWORD)	&totalentries,
IN	(DWORD)		SV_TYPE_ALL,
IN	(LPTSTR)	NULL,
IN OUT	(LPDWORD)	NULL
    );
memmove(pbBuffer, bufGiven, cbBuffer);
free(bufGiven);
*pcEntriesRead = (USHORT) entriesread;
*pcTotalAvail = (USHORT) totalentries;
return status;
}

static InNetShareEnum (const char *pszServer,
				     short sLevel,
				     char * pbBuffer,
				     unsigned short cbBuffer,
				     unsigned short *pcEntriesRead,
				     unsigned short *pcTotalAvail)
{
DWORD		entriesread, totalentries;
static DWORD	resume_handle = 0;
LPBYTE		bufGiven;
int		status;

status = (*pNetShareEnum) (
IN	(LPTSTR)	pszServer,
IN	(DWORD)		sLevel,
OUT	(LPBYTE *)	&bufGiven,
IN	(DWORD)		cbBuffer,
OUT	(LPDWORD)	&entriesread,
OUT	(LPDWORD)	&totalentries,
IN OUT	(LPDWORD)	resume_handle
    );
memmove(pbBuffer, bufGiven, cbBuffer);
free(bufGiven);
*pcEntriesRead = (USHORT) entriesread;
*pcTotalAvail = (USHORT) totalentries;
return status;
}
#endif

#endif
