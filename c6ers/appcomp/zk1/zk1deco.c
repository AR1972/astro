/* TS = none */
/*
**  ZK1DECO.C  --  Zeck Decompression module - first implementation
**
**  This comes to us from Languages via Excel and is used many places.
**  Still, I altered some of the code to fit my API.  Original sources
**  can be found in \\odin\slm!src\setup\compress.
*/

/*                    Zeck Data Compression Program
**                      (C) Copyright 1989 by Microsoft
**                           Written By Steven Zeck
**
**  This program will compress files using the Zeck compress algorithm.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <io.h>
#include <fcntl.h>

#include "..\sutkcomp.h"

#ifdef OS2SU
#include <doscalls.h>
#endif

#include "zk1.h"

#include "setjmp.h"

extern BOOL vfUserCancel;
extern PFNWFROMW vpfnYield;

extern BOOL    fJmpEnvSet;
extern jmp_buf jmpEnv;

  /* forward declarations */
BOOL  UnpackZK1(void);


/*
**  LONG  Lcb_ZK1_DecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
**                                                                LONG libStart)
*/
LONG Lcb_ZK1_DecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
                                                                  LONG libStart)
{
    int iJmpReturn;

    if (lcbDestMax != NIL)
        return((LONG)rcGenericDecompError);

    if (!FAllocateLZGlobals((lcbDestMax == NIL) ? NIL : (libStart + lcbDestMax),
                                                                         FALSE))
        return((LONG)rcOutOfMemory);

    if ((iJmpReturn = setjmp(jmpEnv)) != 0)
        {
        FreeLZGlobals();
        fJmpEnvSet = FALSE;
        return((LONG)iJmpReturn);
        }
    fJmpEnvSet = TRUE;

      /* lcbDestStop == libStart + lcbDestMax */
    if (lcbDestMax != NIL && (LONG)(fpbOutBufEnd - fpbOutBuf) > lcbDestStop)
        fpbOutBufEnd = fpbOutBuf + (SHORT)lcbDestStop;

    fhDestGlobal = fhDest;
    fhSrcGlobal = fhSrc;
    lcbSkipOut = libStart;
    fpbBufDest = NULL;

    if (!UnpackZK1())
        {
        fJmpEnvSet = FALSE;
        FreeLZGlobals();
        return((LONG)rcGenericDecompError);
        }

    fJmpEnvSet = FALSE;
    FreeLZGlobals();

    if (fWriteError)
        return((LONG)rcWriteError);
    else
        return(lcbDest - libStart);
}


/*
**  LONG Lcb_ZK1_DecompressToBuffer(int fhSrc, BYTE far * fpbBuf, LONG lcbBuf,
**                                                               LONG libStart)
*/
LONG Lcb_ZK1_DecompressToBuffer(int fhSrc, BYTE far * fpbBuf, LONG lcbBuf,
                                                                 LONG libStart)
{
    int iJmpReturn;

    if (lcbBuf <= 0L)
        return((LONG)rcGenericDecompError);

    if (!FAllocateLZGlobals(libStart + lcbBuf, FALSE))
        return((LONG)rcOutOfMemory);

    if ((iJmpReturn = setjmp(jmpEnv)) != 0)
        {
        fpbBufDest = NULL;
        FreeLZGlobals();
        fJmpEnvSet = FALSE;
        return((LONG)iJmpReturn);
        }
    fJmpEnvSet = TRUE;

      /* lcbDestStop == libStart + lcbBuf */
    if ((LONG)(fpbOutBufEnd - fpbOutBuf) > lcbDestStop)
        fpbOutBufEnd = fpbOutBuf + (SHORT)lcbDestStop;

    lcbSkipOut = libStart;
    fpbBufDest = fpbBuf;
    fhDestGlobal = -1;
    fhSrcGlobal = fhSrc;

    if (!UnpackZK1())
        {
        fJmpEnvSet = FALSE;
        fpbBufDest = NULL;
        FreeLZGlobals();
        return((LONG)rcGenericDecompError);
        }

    fJmpEnvSet = FALSE;
    fpbBufDest = NULL;
    FreeLZGlobals();

    if (fWriteError)
        return((LONG)rcWriteError);
    else
        return(lcbDest - libStart);
}


/*
**  BOOL UnpackZK1(void)
**
*/
BOOL UnpackZK1(void)
{
    int     i, cb, oStart, ibBufCur;
    USHORT  b;
    USHORT  usFlags;
    LONG    lcbOut;
    USHORT  cCyclesUntilBreak = 1;

    _fmemset(ringBuf, ' ', cbBufMax - cbStrMax);
    ibBufCur = cbBufMax - cbStrMax;
    usFlags = 0;
    lcbOut = 0;

    while (TRUE)
        {
        if (--cCyclesUntilBreak == 0)
            {
            if (vpfnYield != NULL)
                {
                (*vpfnYield)(0);
                cCyclesUntilBreak = 300;
                }
            else
                cCyclesUntilBreak = 30000;

            if (vfUserCancel)
                {
                if (fJmpEnvSet)
                    longjmp(jmpEnv, rcGenericDecompError);
                else
                    return((BOOL)FALSE);
                }
            }

        b = ReadByte(fhSrcGlobal);
        if (b == EOF)
            break;

          /* high order byte counts the # bits used in the low order byte */
        if (((usFlags >>= 1) & 0x100) == 0)
            {
            usFlags = b | 0xff00; /* set bit mask describing the next 8 bytes */
            b = ReadByte(fhSrcGlobal);
            if (b == EOF)
                break;
            }

        if (usFlags & 1)
            {
              /* just store the literal character into the buffer */
            WriteByte((BYTE)b);
            if (fDestFull)
                return((BOOL)TRUE);
            lcbOut++;
            ringBuf[ibBufCur++] = (BYTE)b;
            ibBufCur &= cbBufMax - 1;
            }
        else
            {
              /* extract the buffer offset and count to unpack */
            cb = ReadByte(fhSrcGlobal);
            if (cb == EOF)
                break;

            oStart = (cb & 0xf0) << 4 | b;
            cb     = (cb & 0x0f) + cbIndex;

            for (i = 0; i <= cb; i++)
                {
                b = ringBuf[(oStart + i) & (cbBufMax - 1)];
                WriteByte((BYTE)b);
                if (fDestFull)
                    return((BOOL)TRUE);
                lcbOut++;
                ringBuf[ibBufCur++] = (BYTE)b;
                ibBufCur &= cbBufMax - 1;
                }
            }
        }
    
    if (!eof(fhSrcGlobal))
        return((BOOL)TRUE);  /* okay if splitting */

    WriteOutBuff((BYTE)'\0');
    return((BOOL)TRUE);
}
