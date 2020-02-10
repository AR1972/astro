/*
**  JJJ1DECO.C  --  Jeff Johnson Decompression module - first implementation
*/

/*  This code was written by modifying the Zeck compression code
**  and adding a triple huffman compressor.  The results are much
**  smaller.  This was done by Jeff Johnson, 10/15/90 to accomodate
**  Microsoft Excel 3.0.
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

#include "jjj1.h"

#include "setjmp.h"


extern BOOL vfUserCancel;
extern PFNWFROMW vpfnYield;

extern BOOL    fJmpEnvSet;
extern jmp_buf jmpEnv;

  /* forward declarations */
BOOL  UnpackJJJ1(void);


/*
**  LONG  Lcb_JJJ1_DecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
**              LONG libStart, BYTE far * fpbBuf, LONG lcbBuf)
*/
LONG Lcb_JJJ1_DecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
           LONG libStart, BYTE far * fpbBuf, LONG lcbBuf)
{
    int iJmpReturn;

    if (lcbDestMax != NIL)
        return((LONG)rcGenericDecompError);

    if (!FAllocateJJJGlobals((lcbDestMax == NIL) ? NIL : (libStart +lcbDestMax),
                                                                         FALSE))
        return((LONG)rcOutOfMemory);

    if ((iJmpReturn = setjmp(jmpEnv)) != 0)
        {
        fJmpEnvSet = FALSE;
        FreeJJJGlobals();
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

    if (!UnpackJJJ1())
        {
        fJmpEnvSet = FALSE;
        FreeJJJGlobals();
        return((LONG)rcGenericDecompError);
        }

    fJmpEnvSet = FALSE;

    FreeJJJGlobals();

    if (fWriteError)
        return((LONG)rcWriteError);
    else
        return(lcbDest - libStart);
}


#ifdef COMPLEX
/*
**  LONG Lcb_JJJ1_DecompressToBuffer(int fhSrc, BYTE far * fpbBuf, LONG lcbBuf,
**                                                               LONG libStart)
*/
LONG Lcb_JJJ1_DecompressToBuffer(int fhSrc, BYTE far * fpbBuf, LONG lcbBuf,
                                                                 LONG libStart)
{
    int iJmpReturn;

    if (lcbBuf <= 0L)
        return((LONG)rcGenericDecompError);

    if (!FAllocateJJJGlobals(libStart + lcbBuf, FALSE))
        return((LONG)rcOutOfMemory);

    if ((iJmpReturn = setjmp(jmpEnv)) != 0)
        {
        fJmpEnvSet = FALSE;
        fpbBufDest = NULL;
        FreeJJJGlobals();
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

    if (!UnpackJJJ1())
        {
        fJmpEnvSet = FALSE;
        fpbBufDest = NULL;
        FreeJJJGlobals();
        return((LONG)rcGenericDecompError);
        }

    fJmpEnvSet = FALSE;
    fpbBufDest = NULL;
    FreeJJJGlobals();

    if (fWriteError)
        return((LONG)rcWriteError);
    else
        return(lcbDest - libStart);
}
#endif /* COMPLEX */


USHORT  usIHold = 0, cbIHold = 0;
BOOL    fHitEOF = FALSE;

int  ReadHuffman(CODETABLE far *fpct, BYTE far *fpbLookup)
{
    USHORT  b, ndx, ent;

    if (cbIHold < 8)
        {
        if (!fHitEOF)
            {
            if ((b = ReadByte(fhSrcGlobal)) == EOF)
                {
                fHitEOF = (BOOL)TRUE;
                ent = fpbLookup[((usIHold << (8 - cbIHold)) & 255)];
                if (cbIHold >= fpct[ent].cbCode)
                    {
                    cbIHold -= fpct[ent].cbCode;
                    return(ent);
                    }
                return(EOF);
                }
            usIHold = (usIHold << 8) | b;
            cbIHold += 8;
            }
        else
            {
            ent = fpbLookup[((usIHold << (8 - cbIHold)) & 255)];
            if (cbIHold >= fpct[ent].cbCode)
                {
                cbIHold -= fpct[ent].cbCode;
                return(ent);
                }
            return(EOF);
            }
        }

    ndx = usIHold >> (cbIHold - 8);
    ent = fpbLookup[ndx & 255];
    if (fpct[ent].cbCode <= 8)
        {
        cbIHold -= fpct[ent].cbCode;
        return(ent);
        }

    // We have to walk the linked list
    cbIHold -= 8;
    if ((b = ReadByte(fhSrcGlobal)) == EOF)
        fHitEOF = (BOOL)TRUE;
    else
        {
        usIHold = (usIHold << 8) | b;
        cbIHold += 8;
        }

    while(1)
        {
        if (cbIHold + 8 < fpct[ent].cbCode)
            return(EOF);
        ndx = fpct[ent].cbCode - 8;
        if (((usIHold >> (cbIHold - ndx)) & iPowers[ndx]) ==
                (fpct[ent].usCode&iPowers[ndx]))
            {
            cbIHold -= ndx;
            return(ent);
            }
        ent = fpct[ent].nextCode;
        }
}


int  ReadBits(int bitcnt)
{
    int  b;

    if ((int) cbIHold < bitcnt)
        if (!fHitEOF)
            {
            if ((b = ReadByte(fhSrcGlobal)) == EOF)
                {
                fHitEOF = (BOOL)TRUE;
                return(EOF);
                }
            usIHold = (usIHold<<8) | b;
            cbIHold += 8;
            }
        else
            return(EOF);

    cbIHold -= bitcnt;
    return((usIHold >> cbIHold) & iPowers[bitcnt]);
}


void  ReadHTable(CODETABLE far *fpct, int usct, int Method)
{
    int   cnt, b;
    BYTE  byte;

    switch (Method)
        {
    case NOTABLE: // Build an evensized huffman table
        b = 0;
        cnt = usct;
        while (cnt >>= 1)
            b++;
        for (cnt = 0; cnt < usct; cnt++)
            fpct[cnt].cbCode = (BYTE) b;
        break;

    case COMPRESSNONE:
        for (cnt = 0; cnt < usct; cnt += 2)
            {
            b = ReadBits(8);
            fpct[cnt  ].cbCode = (BYTE) ((int) b>>4);
            fpct[cnt+1].cbCode = (BYTE) ((int) b&15);
            }
        break;

    case COMPRESS1BIT:
        fpct[0].cbCode = byte = (BYTE) ReadBits(4);
        for (cnt = 1; cnt < usct; cnt++)
            {
            if (!ReadBits(1))
                fpct[cnt].cbCode = byte;
            else if (!ReadBits(1))
                fpct[cnt].cbCode = ++byte;
            else
                fpct[cnt].cbCode = byte = (BYTE) ReadBits(4);
            }
        break;

    case COMPRESS2BIT:
        fpct[0].cbCode = byte = (BYTE) ReadBits(4);
        for (cnt = 1; cnt < usct; cnt++)
            {
            if ((b=ReadBits(2)) != 3)
                fpct[cnt].cbCode = byte += (b-1);
            else
                fpct[cnt].cbCode = byte = (BYTE) ReadBits(4);
            }
        break;
        }
}


void  BuildHLookup(CODETABLE far *fpct, BYTE far *fpbLookup, int usct)
{
    int  usLookup = 0, cnt = 0, bCurNdx;
    int  bCurCode = 0, bCurLen = 100;
    int  iTemp; // HACK!!! bug in C600x requires expr to be broken down!
                // Otherwise it uses a WORD PTR instead of a BYTE PTR and
                // this GP faults as the 2nd byte often hangs off the edge
                // of a segment!!!

    for (cnt = 0; cnt < usct; cnt++)
        {
        if ((fpct[cnt].cbCode < (BYTE)bCurLen) && fpct[cnt].cbCode)
            {
            bCurLen = fpct[cnt].cbCode;
            bCurNdx = cnt;
            }
        }

    bCurCode = fpct[bCurNdx].usCode;
    if (bCurLen > 8)
        {
        bCurCode >>= (bCurLen - 8);
        bCurLen    = 8;
        }

    cnt = 0;
    while (1)
        {
        while (((cnt>>(8-bCurLen)) == bCurCode) && cnt < 256)
            fpbLookup[cnt++] = (BYTE) bCurNdx;

        iTemp = fpct[bCurNdx].nextCode;

        if (cnt == 256)
            break;

        if (fpct[iTemp].cbCode <= 8)
            bCurNdx = fpct[bCurNdx].nextCode;
        else
            {
            while (1)
                {
                bCurNdx = fpct[bCurNdx].nextCode;
                if ((fpct[bCurNdx].usCode>>(fpct[bCurNdx].cbCode-8)) != 
                        (BYTE) bCurCode)
                    break;
                }
            }
        bCurCode= fpct[bCurNdx].usCode;
        if ((bCurLen = fpct[bCurNdx].cbCode) > 8)
            {
            bCurCode >>= (bCurLen-8);
            bCurLen  = 8;
            }
        }
}


/*
**  BOOL UnpackJJJ1(void)
*/
BOOL UnpackJJJ1(void)
{
    int     i, cb, oStart, ibBufCur;
    USHORT  b, cnt, in;
    USHORT  usFlags;
    LONG    lcbOut;
    int     cType;
    int     b8Bit, b6Bit, b4aBit, b4bBit, b5Bit;
    int     isPtr = FALSE;
    USHORT  cCyclesUntilBreak = 1;

    vfUserCancel = FALSE;

    ibBufCur = cbBufMax - cbStrMax;
    usFlags = 0;
    lcbOut = 0;

    // Get the code table compression info
    b4aBit  = b4bBit = ReadByte(fhSrcGlobal);
    b5Bit   = b6Bit  = ReadByte(fhSrcGlobal);
    b8Bit            = ReadByte(fhSrcGlobal);

    b4aBit >>= 4;
    b5Bit  >>= 4;
    b8Bit  >>= 4;

    b4bBit &= 15;
    b6Bit  &= 15;

    _fmemset(ringBuf, ' ', cbBufMax - cbStrMax);

    cbIHold = 0;      //Reset globals
    fHitEOF = FALSE;

    ReadHTable(fpct4a, 16, b4aBit);
    BuildCodeTable(fpct4a, 16);
    BuildHLookup(fpct4a, fpbLookup4a, 16);

    ReadHTable(fpct4b, 16, b4bBit);
    BuildCodeTable(fpct4b, 16);
    BuildHLookup(fpct4b, fpbLookup4b, 16);

    ReadHTable(fpct5, 32, b5Bit);
    BuildCodeTable(fpct5, 32);
    BuildHLookup(fpct5, fpbLookup5, 32);

    ReadHTable(fpct6, 64, b6Bit);
    BuildCodeTable(fpct6, 64); 
    BuildHLookup(fpct6, fpbLookup6, 64);

    ReadHTable(fpct8, 256, b8Bit);
    BuildCodeTable(fpct8, 256);
    BuildHLookup(fpct8, fpbLookup8, 256);

    while (TRUE)
        {
        if (--cCyclesUntilBreak == 0)
            {
            if (vpfnYield != NULL)
                {
                cCyclesUntilBreak = 300;
                (*vpfnYield)(0);
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

        if (isPtr)
            {
            if ((cType = ReadHuffman(fpct4b, fpbLookup4b)) == EOF)
                break;
            }
        else
            {
            if ((cType = ReadHuffman(fpct4a, fpbLookup4a)) == EOF)
               break;
            }

        if (cType) // It's a pointer
            {
            isPtr = FALSE;

            /* extract the buffer offset and count to unpack */
            if ((oStart = ReadHuffman(fpct6, fpbLookup6)) == EOF)
                break;
            if ((i = ReadBits(6)) == EOF)
                break;
            oStart  = (oStart << 6) | i;
            oStart  = (ibBufCur - oStart + cbBufMax) & (cbBufMax - 1);

            cb = cType + cbIndex - 1;

            for (i = 0; i <= cb; i++)
                {
                WriteByte((BYTE)(b = ringBuf[(oStart + i) & (cbBufMax - 1)]));
                if (fDestFull)
                    return((BOOL)TRUE);
                lcbOut++;
                ringBuf[ibBufCur++] = (BYTE)b;
                ibBufCur &= cbBufMax - 1;
                }
            }
        else  // It's a literal count
            {
            if ((b = ReadHuffman(fpct5, fpbLookup5) + 1) == cbLitMax)
                isPtr = FALSE;
            else
                isPtr = TRUE;

            for (cnt = 0; cnt < b; cnt++)
                {
                if ((in = ReadHuffman(fpct8, fpbLookup8)) == EOF)
                    break;

                WriteByte((BYTE)in);
                if (fDestFull)
                    return((BOOL)TRUE);
                lcbOut++;
                ringBuf[ibBufCur++] = (BYTE)in;
                ibBufCur &= cbBufMax - 1;
                }
            if (cnt != b)
                break;
            }
     } 

     if (!eof(fhSrcGlobal))
         return((BOOL)TRUE);  // not an error condition if splitting file

     WriteOutBuff((BYTE)'\0');
     return((BOOL)TRUE);
}
