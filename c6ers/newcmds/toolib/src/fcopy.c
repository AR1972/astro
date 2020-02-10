/* fcopy.c - fast copy between two file specs
 *
 *	09-Dec-1986 bw	Added DOS 5 support
 *	30-Oct-1987 bw	Change 'DOS5' to 'OS2'
 *      13-May-1989 wc  use sopen for sourcefile. For System Languages Testing.
 *                      also include share.h, and io.h.
 *
*/

#define INCL_DOSFILEMGR
#if defined (OS2)
#include <os2.h>
#endif

#include <stdlib.h>
#include <fcntl.h>
#include <sys\types.h>
#include <sys\stat.h>
#include <malloc.h>
#include <share.h>
#include <io.h>
#if !defined(OS2)
#include <dos.h>
#endif
#include "..\h\tools.h"
#include "fcopytxt.h"

#if defined (DOS)
#define IBUF    10240
#define IBUFSM  1024

/* fcopy (source file, destination file) copies the source to the destination
 * preserving attributes and filetimes.  Returns NULL if OK or a char pointer
 * to the corresponding text of the error
 */
char *fcopy (char *src,char *dst)
{
    int srcfh, dstfh, cnt;
    char *copybuf, *result;
    union REGS regs;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
	 struct SREGS sregs;
#endif
    int cBuf = IBUF;

    result = NOT_ENOUGH_MEM;
    srcfh = dstfh = -1;
    if ((copybuf = (*tools_alloc) (IBUF)) == NULL) {
        cBuf = IBUFSM;
	if ((copybuf = (*tools_alloc) (IBUFSM)) == NULL)
            goto done;
        }

    result = CANT_OPEN_SOURCE;

/*    if ((srcfh = open (src, O_RDONLY | O_BINARY)) == -1) */

    if ((srcfh = sopen (src, O_RDONLY | O_BINARY, SH_DENYNO)) == -1) 
        goto done;

    result = CANT_CREATE_DEST;
    if ((dstfh = open (dst, O_CREAT | O_TRUNC | O_WRONLY | O_BINARY,
                       S_IREAD | S_IWRITE)) == -1)
        goto done;

    result = CANT_WRITE_DEST;
    while ((cnt = read (srcfh, copybuf, cBuf)))
        if (write (dstfh, copybuf, cnt) != cnt)
            goto done;

    result = CANT_READ_SOURCE;
    if (cnt)
        goto done;


    /* get the last-modify time for source */
    result = CANT_GET_SRC_TIME;
    regs.x.ax = 0x5700;
    regs.x.bx = srcfh;
    intdos (&regs, &regs);
    if (regs.x.cflag)
        goto done;
    close (srcfh);
    srcfh = -1;
    /* set the last modification time */
    result = CANT_SET_DEST_TIME;
    regs.x.ax = 0x5701;
    regs.x.bx = dstfh;
    intdos (&regs, &regs);
    if (regs.x.cflag)
        goto done;
    close (dstfh);
    dstfh = -1;
    /* get source attributes */
    result = CANT_GET_SRC_ATTR;
    regs.x.ax = 0x4300;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    segread( &sregs);
    sregs.ds = FP_SEG(src);
    regs.x.dx = FP_OFF(src);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.dx = (unsigned) src;
    intdos (&regs, &regs);
#endif
    if (regs.x.cflag)
        goto done;
    /* set destination attributes (leave off readonly) */
    result = CANT_SET_DEST_ATTR;
//  RSETFLAG (regs.x.cx, A_RO);
    regs.x.ax = 0x4301;
#if ( defined(M_I86CM) || defined (M_I86LM) || defined (M_I86HM) )
    segread( &sregs);
    sregs.ds = FP_SEG(dst);
    regs.x.dx = FP_OFF(dst);
    intdosx(&regs, &regs, &sregs);
#else
    regs.x.dx = (unsigned) dst;
    intdos (&regs, &regs);
#endif
    if (regs.x.cflag)
        goto done;
    result = NULL;


done:
    if (copybuf)
        free( copybuf );
    if (srcfh != -1)
        close( srcfh );
    if (dstfh != -1)
        close( dstfh );
    return result;
}


#elif defined(OS2) || defined(NT)

/* fcopy (source file, destination file) copies the source to the destination
 * preserving attributes and filetimes.  Returns NULL if OK or a char pointer
 * to the corresponding text of the error
 */
char *fcopy (char *src, char *dst)
{
    int srcfh, dstfh;
    char *result;
#if defined(OS2)
    FILESTATUS SrcStatus;
    FEALIST far *fpfeal = NULL;
#elif defined(NT)
    FILETIME CreationTime, LastAccessTime, LastWriteTime;
    DWORD    attrib;
#endif

    srcfh = dstfh = -1;

// if ((srcfh = open (src, O_RDONLY | O_BINARY)) == -1) {
    if ((srcfh = sopen (src, O_RDONLY | O_BINARY, SH_DENYNO)) == -1) {
	result = CANT_OPEN_SOURCE;
        goto done;
	}

    if ((dstfh = open (dst, O_CREAT | O_TRUNC | O_WRONLY | O_BINARY,
		       S_IREAD | S_IWRITE)) == -1) {
	result = CANT_CREATE_DEST;
        goto done;
	}

    result = fastcopy ((HANDLE) srcfh, (HANDLE) dstfh);

    if (result != NULL)
	goto done;

#if defined(OS2)

    /*	If we're on v1.2 or later, do an EA copy and skip any errors
     *	about unsupported EAs
     */

    if (_osmajor > 10 || _osminor >= 20) {
	EAOP eaop;
	USHORT erc;

	erc = DosAllocSeg (0, &SELECTOROF (fpfeal), SEG_NONSHARED);
	if (erc != 0) {
	    result = "Out of memory for EA copy";
	    }
	else {
	    eaop.fpFEAList = fpfeal;
	    fpfeal->cbList = 65534L; //1.3 doesn't like it if you give all 64K
	    erc = DosQFileInfo (srcfh, 4, &eaop, sizeof (eaop));

	    if (!erc) {
		DosSetFileInfo (dstfh, 2, (PBYTE) &eaop, sizeof (eaop));
		}

	    }
	}

    if (NULL == result) {
	if (DosQFileInfo (srcfh, 1, (PBYTE) &SrcStatus, sizeof SrcStatus) != 0) {
	    result = "Unable to get source file information";
	    }
	else {
	    RSETFLAG ( SrcStatus.attrFile , A_RO );

	    if (DosSetFileInfo (dstfh, 1, (PBYTE) &SrcStatus, sizeof SrcStatus) != 0) {
		result = "Unable to set destination file attributes";
		}
	    }
	}

done:
    if (fpfeal != NULL)
	DosFreeSeg (SELECTOROF (fpfeal));
#elif defined(NT)
    if (!GetFileTime(z_handle(srcfh), &CreationTime, &LastAccessTime, &LastWriteTime)) {
        result = "Unable to get time of source";
        goto done;
    }

    if (!SetFileTime(z_handle(dstfh), &CreationTime, &LastAccessTime, &LastWriteTime)) {
        result = "Unable to set time of destination";
        goto done;
    }

    if( ( attrib = GetFileAttributes( src ) ) == -1 ) {
	result = "Unable to get source file information";
	goto done;
    }

    RSETFLAG( attrib, FILE_ATTRIBUTE_READONLY );

    if( SetFileAttributes( dst, attrib ) == FALSE ) {
	result = "Unable to set destination file attributes";
	goto done;
    }

done:
#endif

    if (srcfh != -1)
	close( srcfh );
    if (dstfh != -1)
	close( dstfh );
    return result;
}

#endif
