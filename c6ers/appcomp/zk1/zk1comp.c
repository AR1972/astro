/* TS = none */
/*
**  ZK1COMP.C  --  Zeck Compression module - first implementation
**
**  This comes to us from Languages via Excel and is used many places.
**  Still, I altered some of the code to fit my API.  Original sources
**  can be found in \\odin\slm!src\setup\compress.
*/

/*                    Zeck Data Compression Program
**                      (C) Copyright 1989 by Microsoft
**                           Written By Steven Zeck
*/

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <io.h>
#include <fcntl.h>
#include <stdio.h>

#include "..\sutkcomp.h"

#ifdef OS2SU
#include <doscalls.h>
#endif

#include "zk1.h"

#include "setjmp.h"

extern BOOL    fJmpEnvSet;
extern jmp_buf jmpEnv;



/*
**  LONG Lcb_ZK1_CompressToFile(int fhSrc, int fhDest, LONG lcbDestMax)
**
**  Note - this is basically SteveZ's encode() routine.
**
**  Assumes that the header has already been written.
*/
LONG Lcb_ZK1_CompressToFile(int fhSrc, int fhDest, LONG lcbDestMax)
{
    USHORT   us;
    SHORT    ch, cbLen, ibCharCur, ibStringCur, cbMatchLast, ibCodeBuf;
    BYTE     rgbCodeBuf[cbStrMax], mask;
    LONG     libSrcStart = tell(fhSrc);
    LONG     libDestStart = tell(fhDest);
    LONG     lReturn;
    int      iJmpReturn;

    if (!FAllocateLZGlobals(lcbDestMax, (BOOL)TRUE))
        return((LONG)rcOutOfMemory);

    if ((iJmpReturn = setjmp(jmpEnv)) != 0)
        {
        FreeLZGlobals();
        fJmpEnvSet = FALSE;
        return((LONG)iJmpReturn);
        }
    fJmpEnvSet = TRUE;

    if (lcbDestMax != NIL && (LONG)(fpbOutBufEnd - fpbOutBuf) > lcbDestMax)
        fpbOutBufEnd = fpbOutBuf + (SHORT)lcbDestMax;

    fhDestGlobal = fhDest;
    lcbSkipOut = 0L;
    fpbBufDest = NULL;

    LZInitTree();

      /* rgbCodeBuf[1..16] saves eight units of code, and
      ** rgbCodeBuf[0] works as eight flags, "1" representing that the unit
      ** is an unencoded letter (1 byte), "0" a position-and-length pair
      ** (2 bytes).  Thus, eight units require at most 16 bytes of code.
      */
    rgbCodeBuf[0] = 0;
    ibCodeBuf = mask = 1;

      /* Clear the buffer with any character that will appear often. */
    ibStringCur = 0;
    _fmemset(ringBuf, ' ', (ibCharCur = cbBufMax - cbStrMax));

      /* Read cbStrMax bytes into the last cbStrMax bytes of the buffer */
    for (cbLen = 0; cbLen < cbStrMax && (ch = ReadByte(fhSrc)) != EOF; cbLen++)
        ringBuf[ibCharCur + cbLen] = (BYTE)ch;

      /* Insert the cbStrMax strings,
      ** each of which begins with one or more 'space' characters.  Note
      ** the order in which these strings are inserted.  This way,
      ** degenerate trees will be less likely to occur.
      */
    for (us = 1; us <= cbStrMax; us++)
        LZInsertNode(ibCharCur - us);

      /* Finally, insert the whole string just read.  The
      ** global variables cbMatchCur and iMatchCur are set.
      */
    LZInsertNode(ibCharCur);

    do  {
            /* cbMatchCur may be spuriously long near the end of text. */
        if (cbMatchCur > (USHORT)cbLen)
            cbMatchCur = (USHORT)cbLen;

        if (cbMatchCur <= cbIndex)
            {   /* Not long enough match. Send one byte. */
                /* 'send one byte' flag.  Send uncoded. */
            cbMatchCur = 1;
            rgbCodeBuf[0] |= mask;
            rgbCodeBuf[ibCodeBuf++] = ringBuf[ibCharCur];
            }
        else
            {   /* Send position and length pair. Note cbMatchCur > cbIndex. */
            rgbCodeBuf[ibCodeBuf++] = (BYTE)iMatchCur;
            rgbCodeBuf[ibCodeBuf++] = (BYTE)((iMatchCur >> 4 & 0xf0) |
                                                  (cbMatchCur - (cbIndex + 1)));
            }

        if ((mask <<= 1) == 0)   /* Shift mask left one bit. */
            {
                /* Send at most 8 units of code together */
            for (us = 0; us < (USHORT)ibCodeBuf; us++)
                {
                WriteByte(rgbCodeBuf[us]);
                if (fWriteError || fDestFull)
                    goto LExit;
                }

            rgbCodeBuf[0] = 0;
            ibCodeBuf = mask = 1;
            }
        cbMatchLast = cbMatchCur;

        for (us = 0; us < (USHORT)cbMatchLast && (ch = ReadByte(fhSrc)) != EOF;
                                                                           us++)
            {
            LZDeleteNode(ibStringCur);      /* Delete old strings and */
            ringBuf[ibStringCur] = (BYTE)ch;        /* read new bytes */

              /* If the position is near the end of buffer, extend the
              ** buffer to make string comparison easier.
              */
            if (ibStringCur < cbStrMax - 1)
                ringBuf[ibStringCur + cbBufMax] = (BYTE)ch;

              /* ring buffer, increment the position modulo N. */
            ibStringCur = ibStringCur+1 & (cbBufMax - 1);
            ibCharCur = ibCharCur+1 & (cbBufMax - 1);

              /* Register the string in ringBuf[r..r+cbStrMax-1] */
            LZInsertNode(ibCharCur);
            }

        while (us++ < (USHORT)cbMatchLast)         /* After the end of text, */
            {
            LZDeleteNode(ibStringCur);        /* no need to read, but */

            ibStringCur = ibStringCur+1 & (cbBufMax - 1);
            ibCharCur = ibCharCur+1 & (cbBufMax - 1);
            if (--cbLen)
                LZInsertNode(ibCharCur);          /* buffer may not be empty. */
            }
         } while (cbLen > 0); /* length of string to be processed is zero */

    if (ibCodeBuf > 1)                 /* Send remaining code. */
        {
        for (us = 0; us < (USHORT)ibCodeBuf; us++)
            {
            WriteByte(rgbCodeBuf[us]);
            if (fWriteError || fDestFull)
                goto LExit;
            }
        }

    WriteOutBuff(0);

LExit:
    fJmpEnvSet = FALSE;
    FreeLZGlobals();

    if (fWriteError)
        return((LONG)rcWriteError);

    if (lcbDestMax == NIL || fhDest == -1)
        return(lcbDest);

      /* fix fhSrc because we split */
    lReturn = lcbDest;             /* lcbDest gets altered by the Decomp call */

      /* reset fhDest (after header) */
    if (lseek(fhDest, libDestStart, SEEK_SET) == -1L)
        return((LONG)rcWriteSeekError);

      /* lcbDest = uncompressed length of this current piece */
    lcbDest = Lcb_ZK1_DecompressToFile(fhDest, -1, NIL, 0L);
    if (lcbDest < rcNoError)
        return(lcbDest);

      /* reset fhSrc */
    if (lseek(fhSrc, libSrcStart + lcbDest, SEEK_SET) == -1L)
        return((LONG)rcReadSeekError);

    return(lReturn);
}
