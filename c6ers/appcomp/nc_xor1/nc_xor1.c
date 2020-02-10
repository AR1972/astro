/* TS = none */
/*
**  NC_XOR1.C  --  No Compression (Straight Copy) and Xor Compression Algorithm
**                 - Compression and Decompression routines.
*/

#include <stdio.h>
#include <io.h>
#include <stdlib.h>
#include <malloc.h>
#include <dos.h>
#include "..\sutkcomp.h"


extern  USHORT  CbReadFar(int fh, BYTE far * fp, USHORT cb);


/*
**  static  BOOL  XorBuf(BYTE far * fpbBuf, LONG lcbBuf)
**
**  Xor lcbBuf bytes in fpbBuf with ~0.
*/
static  BOOL  XorBuf(BYTE far * fpbBuf, LONG lcbBuf)
{
    while (lcbBuf-- > 0)
        {
        *fpbBuf = *fpbBuf ^ (~0);
        fpbBuf++;
        }

    return(TRUE);
}


/*
**  static  LONG  LcbFarRead(int fh, BYTE far * fpbBuf, LONG lcb)
**
**  Read lcb bytes into fpbBuf from fh.  Return number of bytes read
**  or an error return code.
*/
static  LONG  LcbFarRead(int fh, BYTE far * fpbBuf, LONG lcb)
{
    USHORT  cbBuf = (32 * 1024);
    LONG    lcbSav = lcb;

    while (lcb > (LONG)cbBuf)
        {
        if (CbReadFar(fh, fpbBuf, cbBuf) != cbBuf)
            return((LONG)rcReadError);
        lcb -= (LONG)cbBuf;
        fpbBuf += cbBuf;
        }
    if (CbReadFar(fh, fpbBuf, (USHORT)lcb) != (USHORT)lcb)
        return((LONG)rcReadError);

    return(lcbSav);
}


/*
**  LONG  Lcb_NC_XOR1_StraightCopy(int fhSrc, int fhDest, LONG lcbDestMax,
**                              BYTE far * fpbBufDest, LONG libStart, BOOL fXor)
**
**  Copy from fhSrc to either fhDest or fpbBufDest.   XORing them with ~0 as we
**  go if fXor is TRUE.  Skip the first libStart bytes.  Copy at most
**  lcbDestMax bytes.  Return number of bytes copied if successful, or an
**  error return code if not.
*/
LONG  Lcb_NC_XOR1_StraightCopy(int fhSrc, int fhDest, LONG lcbDestMax,
                                BYTE far * fpbBufDest, LONG libStart, BOOL fXor)
{
    LONG   lcbToWrite;
    LONG   libSrcSav;

    if (fhDest != -1 && fpbBufDest != NULL)
        return((LONG)rcWriteError);

    if (fhSrc == -1 || (libSrcSav = tell(fhSrc)) == NIL)
        return((LONG)rcReadError);

    if ((lcbToWrite = lseek(fhSrc, 0L, SEEK_END)) == NIL)
        return((LONG)rcReadSeekError);

    lcbToWrite -= libSrcSav;               /* equals length that CAN be read  */

    if (libStart == NIL)
        libStart = 0L;

    if (lcbToWrite <= libStart)
        return(0L);

    lcbToWrite -= libStart;                /* equals length that WILL be read */

    if (lcbDestMax == NIL || lcbDestMax > lcbToWrite)
        lcbDestMax = lcbToWrite;

    if (lcbDestMax == 0L)
        return(0L);

    if (fhDest == -1 && fpbBufDest == NULL)
        lseek(fhSrc, libSrcSav + libStart + lcbDestMax, SEEK_SET);
    else
        {
        lcbToWrite = lcbDestMax;
        lseek(fhSrc, libSrcSav + libStart, SEEK_SET);
        if (fpbBufDest != NULL)                            /* write to buffer */
            {
            LONG   lReturn;

            if ((lReturn = LcbFarRead(fhSrc, fpbBufDest, lcbToWrite)) !=
                                                                     lcbToWrite)
                return((lReturn < 0L) ? lReturn : (LONG)rcReadError);
            if (fXor && !XorBuf(fpbBufDest, lcbToWrite))
                return((LONG)rcGenericCompError);
            }
        else                                                 /* write to file */
            {
            BYTE *  pbBuf;
            USHORT  cbBuf = (32 * 1024);

            while (cbBuf >= 256 && (pbBuf = (BYTE *)malloc(cbBuf)) == NULL)
                cbBuf >>= 1;

            if (pbBuf == NULL)
                return((LONG)rcOutOfMemory);

            while (lcbToWrite > (LONG)cbBuf)
                {
                if (read(fhSrc, pbBuf, cbBuf) != cbBuf)
                    {
                    free(pbBuf);
                    return((LONG)rcReadError);
                    }
                if (fXor && !XorBuf((BYTE far *)pbBuf, (LONG)cbBuf))
                    {
                    free(pbBuf);
                    return((LONG)rcGenericCompError);
                    }
                if (write(fhDest, pbBuf, cbBuf) != cbBuf)
                    {
                    free(pbBuf);
                    return((LONG)rcWriteError);
                    }
                lcbToWrite -= (LONG)cbBuf;
                }
            if (read(fhSrc, pbBuf, (USHORT)lcbToWrite) != (USHORT)lcbToWrite)
                {
                free(pbBuf);
                return((LONG)rcReadError);
                }
            if (fXor && !XorBuf((BYTE far *)pbBuf, lcbToWrite))
                {
                free(pbBuf);
                return((LONG)rcGenericCompError);
                }
            if (write(fhDest, pbBuf, (USHORT)lcbToWrite) != (USHORT)lcbToWrite)
                {
                free(pbBuf);
                return((LONG)rcWriteError);
                }

            free(pbBuf);
            }
        }

    return(lcbDestMax);
}


/*
**  USHORT  CbReadFar(int fh, BYTE far * fp, USHORT cb)
**
**  Reads cb bytes from file handle fh into far pointer fp.
**  Returns number of bytes read, 0 on error.
*/
USHORT  CbReadFar(int fh, BYTE far * fp, USHORT cb)
{
#ifndef OS2_VER
    union  REGS  inregs, outregs;
    struct SREGS segregs;

    inregs.x.bx = fh;            /* file handle */
    inregs.x.cx = cb;            /* bytes to read */
    inregs.x.dx = FP_OFF(fp);    /* offset of buffer */
    segregs.ds  = FP_SEG(fp);    /* segment of buffer */
    inregs.h.ah = 0x3F;          /* Read Handle */
    intdosx(&inregs, &outregs, &segregs);

    return((outregs.x.cflag) ? 0 : outregs.x.ax);
#else /* OS2_VER */
/* REVIEW - total kludge!! */
    BYTE   rgb[128];
    USHORT cbReadTotal = 0;
    USHORT cbRead;
    USHORT ib;

    while (cb > 0)
        {
        if ((cbRead = read(fh, rgb, (cb > 128 ? 128 : cb))) == -1)
            {
            return(0);
            }
        if (cbRead == 0)
            {
            return(cbReadTotal);
            }
        cbReadTotal += cbRead;
        cb -= cbRead;
        for (ib = 0; ib < cbRead; ib++)
            {
            *(fp++) = rgb[ib];
            }
        }
    return(cbReadTotal);
#endif /* !OS2_VER */
}
