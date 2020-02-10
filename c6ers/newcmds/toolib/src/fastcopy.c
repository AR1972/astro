/*  fastcopy - use multiple threads to whack data from one file to another
 */

#define INCL_DOSPROCESS
#define INCL_DOSSEMAPHORES

#if defined (OS2)
#include <os2.h>
#endif
#include <malloc.h>
#include <process.h>
#include <stdlib.h>
#include "..\h\tools.h"

#define BUFSIZE     0xFE00		/*  full segment minus sector	      */
#define STACKSIZE   256 		/*  stack size for child thread       */

typedef struct BUF BUF;

#if defined (OS2)

struct BUF {
    BOOL flag;
    USHORT cch;
    BUF far * fpbufNext;
    BYTE ach[BUFSIZE];
    };

#define LAST	TRUE
#define NOTLAST FALSE

#define TO_INFINITE 0xFFFFFFFF

static long	   avail;
static long	   queue;
static BUF far *   fpbufHead = NULL;
static BUF far *   fpbufTail = NULL;
static HANDLE	   hfSrc, hfDst;

/*  forward type definitions
 */

static NPSZ writer (void);
static void far reader (void);
static BUF far * dequeue (void);
static void enqueue (BUF far * fpbuf);

static NPSZ writer ()
{
    BUF far *fpbuf;
    USHORT cch;
    BOOL f = !LAST;
    NPSZ npsz = NULL;

    while (f != LAST && npsz == NULL) {
	fpbuf = dequeue ();
	if ((f = fpbuf->flag) != LAST)
	    if (DosWrite (hfDst, fpbuf->ach, fpbuf->cch, &cch) != 0)
		npsz = "DosWrite error";
	    else
	    if (cch != fpbuf->cch)
		npsz = "DosWrite out-of-space";
	    else
		;
	else
	    npsz = *(NPSZ far *)fpbuf->ach;
	if (DosFreeSeg (SELECTOROF (fpbuf)) != 0)
	    npsz = "DosFreeSeg error";
	}
    return npsz;
}

static void far reader ()
{
    BUF far * fpbuf;
    USHORT erc;
    BOOL f = !LAST;

    while (f != LAST) {
	if (DosAllocSeg (sizeof (BUF), (PSEL) & SELECTOROF (fpbuf), (USHORT) 0) != 0) {
	    printf ("DosAllocSeg error\n");
	    exit (1);
	    }
	OFFSETOF (fpbuf) = 0;
	f = fpbuf->flag = NOTLAST;
	erc = DosRead (hfSrc, (CHAR far *) fpbuf->ach, BUFSIZE, & fpbuf->cch);
	if (fpbuf->cch == 0 || erc != 0) {
	    f = fpbuf->flag = LAST;
	    *(PSZ far *)fpbuf->ach = erc != 0 ? "DosReadError" : NULL;
	    }
	enqueue (fpbuf);
	}
}

static BUF far * dequeue (void)
{
    USHORT erc;
    BUF far * fpbuf;

    while (TRUE) {
	erc = DosSemSet (&avail);
	if (fpbufHead != NULL) {
	    erc = DosSemRequest (&queue, TO_INFINITE);
	    fpbufHead = (fpbuf = fpbufHead)->fpbufNext;
	    if (fpbufTail == fpbuf)
		fpbufTail = NULL;
	    erc = DosSemClear (&queue);
	    break;
	    }
	erc = DosSemWait (&avail, TO_INFINITE);
	}
    return fpbuf;
}

static void enqueue (fpbuf)
BUF far * fpbuf;
{
    USHORT erc;

    fpbuf->fpbufNext = NULL;
    erc = DosSemRequest (&queue, TO_INFINITE);
    if (fpbufTail == NULL)
	fpbufHead = fpbuf;
    else
	fpbufTail->fpbufNext = fpbuf;
    fpbufTail = fpbuf;
    erc = DosSemClear (&queue);
    erc = DosSemClear (&avail);
}

/*  fastcopy - copy data quickly from one handle to another
 *
 *  hfSrcParm	    file handle to read from
 *  hfDstParm	    file handle to write to
 *
 *  returns	    NULL if successful
 *		    pointer to error string otherwise
 */
char * fastcopy (HANDLE hfSrcParm, HANDLE hfDstParm)
//HFILE hfSrcParm, hfDstParm;
{
    BYTE *pchStack;
    USHORT erc;
    USHORT ignore;
    NPSZ npsz;

    hfSrc = hfSrcParm;
    hfDst = hfDstParm;

    pchStack = (*tools_alloc)  (STACKSIZE);
    if (pchStack == NULL)
	return "not enough memory";

    erc = DosCreateThread (reader, &ignore, pchStack + STACKSIZE);
    if (erc) {
	free (pchStack);
	return "can't create thread";
	}
    npsz = writer ();
    free (pchStack);
    return npsz;
}

#elif defined(NT)
#include <io.h>

struct BUF {
    BOOL  flag;
    ULONG cbBuf;
    BUF  *fpbufNext;
    BYTE  ach[BUFSIZE];
    };

typedef unsigned char *NPSZ;

#define LAST    TRUE
#define NOTLAST FALSE

static HANDLE            hevQNotEmpty;
static CRITICAL_SECTION  hcrtQLock;
//static HMTX            hmtxQLock;
//static HEV             hevQNotEmpty;
static BUF              *fpbufHead = NULL;
static BUF              *fpbufTail = NULL;
static int		 hfSrc, hfDst;
static HANDLE		 hThread;
static BOOL		 fAbort;

/*  forward type definitions
 */

NPSZ	writer( void ); 	  //static
DWORD	reader( void ); 	  //static
BUF	*dequeue( void );	  //static
void	enqueue( BUF *fpbuf );	  //static

//static
 NPSZ writer ()
{
    BUF *fpbuf;
    DWORD cbBytesOut;
    BOOL f = !LAST;
    NPSZ npsz = NULL;
    CHAR szErr[50] = "WriteFile: error ";

    while (f != LAST && npsz == NULL) {
        fpbuf = dequeue ();
        if ((f = fpbuf->flag) != LAST) {
	    if( (cbBytesOut = _write( hfDst, fpbuf->ach, fpbuf->cbBuf)) == -1 ) {
		npsz = szErr;
		//ultoa((unsigned long) errno, szErr+17, 10);
		strcpy(szErr+17, strerror(errno));
            } else if( cbBytesOut != ( DWORD )fpbuf->cbBuf ) {
                npsz = "WriteFile: out-of-space";
            }
        } else {
            npsz = *(NPSZ *)fpbuf->ach;
        }
        LocalFree(fpbuf);
    }
    if ( f != LAST )
	fAbort = TRUE;
    WaitForSingleObject( hThread, -1 );
    CloseHandle(hThread);
    CloseHandle(hevQNotEmpty);
    DeleteCriticalSection(&hcrtQLock);
    return npsz;
}


//static
 DWORD reader()
{
    BUF *fpbuf;
    BOOL f = !LAST;

    while ( !fAbort && f != LAST) {
        if ( (fpbuf = LocalAlloc(LMEM_FIXED,sizeof(BUF)) ) == 0) {
            printf ("LocalAlloc error %ld\n",GetLastError());
            exit (1);
        }
        f = fpbuf->flag = NOTLAST;
	if ( ((fpbuf->cbBuf = _read( hfSrc, fpbuf->ach, BUFSIZE)) == -1) || (fpbuf->cbBuf == 0) ) {
	    f = fpbuf->flag = LAST;
	    if (fpbuf->cbBuf == -1) { //error
		fpbuf->cbBuf = 0;
		*(NPSZ *)fpbuf->ach = "read error";
		}
	    else
		*(NPSZ *)fpbuf->ach = NULL;
        }
        enqueue (fpbuf);
    }
    return( 0 );
}

//static
 BUF *dequeue( void )
{
    BUF *fpbuf;

    while (TRUE) {

        if (fpbufHead != NULL) {
            EnterCriticalSection( &hcrtQLock );
            fpbufHead = (fpbuf = fpbufHead)->fpbufNext;
            if( fpbufTail == fpbuf ) {
                fpbufTail = NULL;
            }
            LeaveCriticalSection( &hcrtQLock );
            break;
        }

        /*
           the head pointer is null so the list is empty.
           block on eventsem until enqueue posts (ie. adds to queue)
        */

        WaitForSingleObject( hevQNotEmpty, -1 );
    }
    return fpbuf;
}

//static
 void enqueue( BUF *fpbuf )
{
    fpbuf->fpbufNext = NULL;

    EnterCriticalSection( &hcrtQLock );

    if( fpbufTail == NULL ) {
        fpbufHead = fpbuf;
    } else {
        fpbufTail->fpbufNext = fpbuf;
    }
    fpbufTail = fpbuf;
    LeaveCriticalSection( &hcrtQLock );

    SetEvent( hevQNotEmpty );
}

/*  fastcopy - copy data quickly from one handle to another
 *
 *  hfSrcParm       file handle to read from
 *  hfDstParm       file handle to write to
 *
 *  returns         NULL if successful
 *                  pointer to error string otherwise
 */
char *fastcopy( HANDLE hfSrcParm, HANDLE hfDstParm)
{
    DWORD dwReader;

    hfSrc = (int)hfSrcParm;
    hfDst = (int)hfDstParm;

    hevQNotEmpty = CreateEvent( NULL, (BOOL)FALSE, (BOOL)FALSE,NULL );
    if ( hevQNotEmpty == INVALID_HANDLE_VALUE )
	return "can't create Event object";
    fAbort = FALSE;
    InitializeCriticalSection( &hcrtQLock );

    hThread = CreateThread( 0, STACKSIZE, (LPTHREAD_START_ROUTINE)reader, 0, 0, &dwReader );
    if( hThread == INVALID_HANDLE_VALUE ) {
        return "can't create thread";
    }
    return( writer() );
}

#endif
