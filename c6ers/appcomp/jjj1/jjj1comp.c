/*
**  JJJ1COMP.C  --  JJJ1 Compression module - first implementation
*/

/*  This code was written by modifying the Zeck compression code
**  and adding a triple huffman compressor.  The results are much
**  smaller.  This was done by Jeff Johnson, 10/15/90 to accomodate
**  Microsoft Excel 3.0.
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

#include "jjj1.h"

#include "setjmp.h"

extern BOOL    fJmpEnvSet;
extern jmp_buf jmpEnv;



// Globals used by compression
BYTE  bPassCnt = 0;
long  lcbSrcStart;

BYTE    rgbLitBuf[cbLitMax];
USHORT  ibLitBuf=0;
BYTE    bBitHold = 0, cbBitHold=0;

void  WriteBits(unsigned int val, unsigned int cnt)
{
    if (cnt + cbBitHold <= 8)
        {
        bBitHold <<= (cnt);
        bBitHold  |= (val) & iPowers[cnt];
        if ((cbBitHold += (cnt)) == 8)
            {
            WriteByte(bBitHold);
            cbBitHold = 0;
            }
        }
    else
        {
        bBitHold <<= 8 - cbBitHold;
        bBitHold  |= (val >> (cnt -= (8 - cbBitHold))) & iPowers[8 - cbBitHold];
        WriteByte(bBitHold);
        if (cnt >= 8)
            WriteByte((BYTE) (val >> (cnt -= 8)));
        cbBitHold = (BYTE) cnt;
        bBitHold = (BYTE) val;
        }
}

#define WriteHuffman4a(val) WriteBits(fpct4a[val].usCode,fpct4a[val].cbCode)
#define WriteHuffman4b(val) WriteBits(fpct4b[val].usCode,fpct4b[val].cbCode)
#define WriteHuffman5(val)  WriteBits(fpct5 [val].usCode,fpct5 [val].cbCode)
#define WriteHuffman6(val)  WriteBits(fpct6 [val].usCode,fpct6 [val].cbCode)
#define WriteHuffman8(val)  WriteBits(fpct8 [val].usCode,fpct8 [val].cbCode)


void  InitAnalyze()
{
    int  cnt;

    for (cnt = 0; cnt < 256; cnt++)
        fpbAnalysis8[cnt] = 0;
    for (cnt = 0; cnt < 64; cnt++)
        fpbAnalysis6[cnt] = 0;
    for (cnt = 0; cnt < 32; cnt++)
        fpbAnalysis5[cnt] = 0;
    for (cnt = 0; cnt < 16; cnt++)
        fpbAnalysis4a[cnt] = fpbAnalysis4b[cnt] = 0;
}


int  TraverseHTree(int curpos, CODETABLE far *fpct, int depth)
{
    int  dep1, dep2;

    if (fphtArr[curpos].left == -1) //leaf node
        {
        fpct[curpos].cbCode = (BYTE) depth;
        return(depth);
        }
    dep1 = TraverseHTree(fphtArr[curpos].left , fpct, depth+1);
    dep2 = TraverseHTree(fphtArr[curpos].right, fpct, depth+1);
    if (dep1 > dep2)
        return(dep1);
    else
        return(dep2);
}


void  BuildHTree(long far *fpbInWgt, int cbInWgt, CODETABLE far *fpct)
{
    int   cnt, passcnt, ndxsmall, ndxsmallest, unused;
    long  small, smallest;

LStartBuildHTree:
    unused = 0;
    for (cnt = 0; cnt < cbInWgt; cnt++)
        fpct[cnt].cbCode = 0;

    for (cnt = 0; cnt < cbInWgt; cnt++)
        {
        fphtArr[cnt].left = fphtArr[cnt].right = fphtArr[cnt].parent = -1;
        if ((fphtArr[cnt].weight = fpbInWgt[cnt]) == 0) // Don't give it a code
            {
            fphtArr[cnt].parent = -2;
            unused++;
            }
        }

    if (unused >= cbInWgt - 1) // Can't build a proper tree
        {
        if (unused == cbInWgt) // No entries, just assign 1-bit to last 2 codes
            fpct[cbInWgt-1].cbCode = fpct[cbInWgt-2].cbCode = 1;
        else
            {
            cnt = -1;
            while (!fpbInWgt[++cnt]);
                if (cnt)
                    fpct[cnt].cbCode = fpct[cnt-1].cbCode = 1;
                else
                    fpct[cnt].cbCode = fpct[cnt+1].cbCode = 1;
            }
        return;
        }
       
    for (passcnt = 0; passcnt < cbInWgt - unused - 1; passcnt++)
        {
        small = smallest = 1L << 30;
        for (cnt = 0; cnt < cbInWgt + passcnt; cnt++)
            {
            if ((fphtArr[cnt].weight < small) && (fphtArr[cnt].parent == -1))
                {
                if (fphtArr[cnt].weight < smallest)
                    {
                    small = smallest;
                    ndxsmall = ndxsmallest;
                    smallest = fphtArr[cnt].weight;
                    ndxsmallest = cnt;
                    }
                else
                    {
                    small = fphtArr[cnt].weight;
                    ndxsmall = cnt;
                    }
                }
            }
        fphtArr[ndxsmall].parent = fphtArr[ndxsmallest].parent = cnt;
        fphtArr[cnt].weight = small+smallest;
        fphtArr[cnt].left   = ndxsmallest;
        fphtArr[cnt].right  = ndxsmall;
        fphtArr[cnt].parent = -1;
        }

    for (cnt = 0; cnt < cbInWgt; cnt++)
        fpct[cnt].cbCode = 0;

    if (TraverseHTree(2 * cbInWgt - unused - 2, fpct, 0) < 16)
        return;

    small = 1;
    smallest = 1L << 30;
    for (cnt = 0; cnt < cbInWgt; cnt++)
        {
        long  wgt = fpbInWgt[cnt];

        if (wgt > 0L)
            {
            if (wgt < smallest)
                {
                small = smallest;
                smallest = wgt;
                }
            else if (wgt > smallest && wgt < small)
                small = wgt;
            }
        }

    if (smallest >= small || smallest == 0L)
        return;    /* ERROR */

    for (cnt = 0; cnt < cbInWgt; cnt++)
        {
        if (fpbInWgt[cnt] == smallest)
            fpbInWgt[cnt] = small;
        }
    goto LStartBuildHTree;
}


// Returns TRUE if the given codetable is better than no code at all
BOOL  CodeTradeoff(long far * fpAnal, CODETABLE far * fpct, int cbAnal, int cbHdr,
        int *bBit)
{
    int   cnt = cbAnal, codesize = 0;
    long  size = 0, repcnt = 0;

    while (cnt >>= 1)
        codesize++;

    for (cnt = 0; cnt < cbAnal; cnt++)
        {
        size   += fpAnal[cnt] * fpct[cnt].cbCode;
        repcnt += fpAnal[cnt];
        }
    size >>= 3; // Convert to bytes
    repcnt = repcnt * codesize / 8;

   size += cbHdr;  // Account for the cost of the header

// Calculate interesting info
#ifdef STATS
{

   long totfreq = 0;
   double result = 0.0, work, log(double);
   for(cnt=0;cnt<cbAnal;cnt++)
      totfreq += fpAnal[cnt];
   for(cnt=0;cnt<cbAnal;cnt++)
   {
      if(fpAnal[cnt]) //Don't do if 0 freq
      {
         work = (double) fpAnal[cnt] / (double) totfreq;
         work = log(work) / log(2.0);
         work*= (double) (-fpAnal[cnt]);
         result += work;
      }
   }
   printf("Entries: %3d, Uncomp: %6ld, Comp: %6ld, Optimal: %6.0f, Savings: %6.0f\n",
          cbAnal, repcnt, size, result/8,(double) size - (result/8));
}
#endif

    if (size >= repcnt) // Not worth it
        {
        *bBit = NOTABLE;  // Have to rebuild generic huffman tables
        for (cnt = 0; cnt < cbAnal; cnt++)
            fpct[cnt].cbCode = (BYTE) codesize;
        BuildCodeTable(fpct, cbAnal);
        }
    return ((BOOL)(size < repcnt));
}


int  TableComp(CODETABLE far *fpct, int cbct, int *bBit)
{
//  int  totsize[2] = { 4,4 };			// jem
    int  totsize[2];
    int  lastval    = fpct[0].cbCode;
    int  cnt, diff;

    totsize[0] = totsize[1] = 4;

    for (cnt = 1; cnt < cbct; cnt++)
        {
        diff = fpct[cnt].cbCode - lastval;

        if (!diff)
            totsize[0]++;
        else if (diff == 1)
            totsize[0]+=2;
        else
            totsize[0]+=6;


        if (diff > -2 && diff < 2)
            totsize[1] += 2;
        else
            totsize[1] += 6;

        lastval = fpct[cnt].cbCode;
        }

    totsize[0] >>= 3; //Convert to bytes
    totsize[1] >>= 3; //Convert to bytes

    if ((totsize[0] < totsize[1]) && (totsize[0] < (cbct>>1)))
        {
        *bBit = COMPRESS1BIT;
        cnt   = totsize[0];
        }
    else if (totsize[1] < (cbct >> 1))
        {
        *bBit = COMPRESS2BIT;
        cnt   = totsize[1];
        }
    else
        {
        *bBit = COMPRESSNONE;
        cnt   = cbct >> 1;
        }
    return(cnt);
}


void  WriteTable(CODETABLE far *fpct, int cbct, int Method)
{
    int  cnt, lastval = fpct[0].cbCode, diff, curval;

    if (Method == NOTABLE)
        return;

    WriteBits(lastval, 4);

    for (cnt = 1; cnt < cbct; cnt++)
        {
        curval = fpct[cnt].cbCode;
        diff   = curval - lastval;

        switch (Method)
            {
        case COMPRESSNONE:
            WriteBits(curval, 4);
            break;
        case COMPRESS1BIT:
            if (diff == 0)
                WriteBits(0, 1);
            else if (diff == 1)
                WriteBits(2, 2);
            else
                WriteBits(0x30|curval, 6);
            break;
        case COMPRESS2BIT:
            if ((diff > -2) && (diff < 2))
                WriteBits(diff+1, 2);
            else
                WriteBits(0x30|curval, 6);
            break;
            }
        lastval = curval;
        }
}


/*
**  LONG Lcb_JJJ1_CompressToFile(int fhSrc, int fhDest, LONG lcbDestMax)
**
**  Assumes that the header has already been written.
*/
LONG Lcb_JJJ1_CompressToFile(int fhSrc, int fhDest, LONG lcbDestMax)
{
    USHORT   us,cnt;
    SHORT    ch, cbLen, ibCharCur, ibStringCur, cbMatchLast,iMatchRel;
    LONG     libSrcStart = tell(fhSrc);
    LONG     libDestStart = tell(fhDest);
    LONG     lReturn;
    static   SHORT    b8Bit, b6Bit, b5Bit, b4aBit, b4bBit;
    SHORT    cb8Hdr, cb6Hdr, cb5Hdr, cb4aHdr, cb4bHdr;
    int      isPtr = FALSE;
    int      iJmpReturn;

    bBitHold = cbBitHold = 0;
    ibLitBuf = 0;

    if (bPassCnt == 0)
        {
        if (!FAllocateJJJGlobals(lcbDestMax, (BOOL)TRUE))
            return((LONG)rcOutOfMemory);

        if ((lcbSrcStart = lseek(fhSrc, 0L, SEEK_CUR)) == -1L)
            {
            FreeJJJGlobals();
            return((LONG)rcReadSeekError);
            }

        if ((iJmpReturn = setjmp(jmpEnv)) != 0)
            {
            fJmpEnvSet = FALSE;
            FreeJJJGlobals();
            return((LONG)iJmpReturn);
            }
        fJmpEnvSet = TRUE;

        if (lcbDestMax != NIL && (LONG)(fpbOutBufEnd - fpbOutBuf) > lcbDestMax)
              fpbOutBufEnd = fpbOutBuf + (SHORT)lcbDestMax;

        fhDestGlobal = fhDest;
        lcbSkipOut = 0L;
        fpbBufDest = NULL;

        InitAnalyze();
        }
    else
        {
        // Output the header info and tables
        WriteByte((BYTE) ((b4aBit<<4) | b4bBit));
        WriteByte((BYTE) ((b5Bit<<4)  | b6Bit));  
        WriteByte((BYTE) ((b8Bit<<4)  | 0));  

        WriteTable(fpct4a, 16, b4aBit);
        WriteTable(fpct4b, 16, b4bBit);
        WriteTable(fpct5,  32, b5Bit);
        WriteTable(fpct6,  64, b6Bit);
        WriteTable(fpct8, 256, b8Bit);
        }

    LZInitTree();

    /* Clear the buffer with any character that will appear often. */
    ibStringCur = 0;
    _fmemset(ringBuf, ' ', (ibCharCur = cbBufMax - cbStrMax));

    /* Read cbStrMax bytes into the last cbStrMax bytes of the buffer */
    for (cbLen = 0; (USHORT)cbLen < cbStrMax && (ch = ReadByte(fhSrc)) != EOF;
            cbLen++)
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
        if ((SHORT) cbMatchCur > cbLen)
            cbMatchCur = cbLen;

        if (cbMatchCur <= cbIndex)
            {  /* Not long enough match. Send one byte. */
               /* 'send one byte' flag.  Send uncoded. */
            cbMatchCur = 1;
            rgbLitBuf[ibLitBuf++] = ringBuf[ibCharCur];
            if (ibLitBuf >= cbLitMax)
                {
                if (ibLitBuf)  // Flush Literals
                    {
                    if (ibLitBuf == cbLitMax)
                        isPtr = FALSE;
                    else
                        isPtr = TRUE;

                    if (bPassCnt == 0)  // Analyze phase
                        {
                        fpbAnalysis4a[0]++;
                        fpbAnalysis5[ibLitBuf-1]++;
                        }
                    else
                        {
                        WriteHuffman4a(0);
                        WriteHuffman5((BYTE) (ibLitBuf-1));
                        }
                    if (fWriteError || fDestFull)
                        goto LExit;

                    for (cnt = 0; cnt < ibLitBuf; cnt++)
                        {
                        if (bPassCnt == 0) // Analyze phase
                            fpbAnalysis8[rgbLitBuf[cnt]]++;
                        else
                            WriteHuffman8(rgbLitBuf[cnt]);

                        if (fWriteError || fDestFull)
                            goto LExit;
                        }
                    ibLitBuf = 0; 
                    }
                }
            }
        else
            {  /* Send position and length pair. Note cbMatchCur > cbIndex. */
            if (ibLitBuf)  // Flush Literals
                {
                if (ibLitBuf == cbLitMax)
                    isPtr = FALSE;
                else
                    isPtr = TRUE;

                if (bPassCnt == 0)  // Analyze phase
                    {
                    fpbAnalysis4a[0]++;
                    fpbAnalysis5[ibLitBuf-1]++;
                    }
                else
                    {
                    WriteHuffman4a(0);
                    WriteHuffman5((BYTE) (ibLitBuf-1));
                    }
                if (fWriteError || fDestFull)
                    goto LExit;

                for (cnt = 0; cnt < ibLitBuf; cnt++)
                    {
                    if (bPassCnt == 0) // Analyze phase
                        fpbAnalysis8[rgbLitBuf[cnt]]++;
                    else
                        WriteHuffman8(rgbLitBuf[cnt]);

                    if (fWriteError || fDestFull)
                        goto LExit;
                    }
                ibLitBuf = 0; 
                }

            iMatchRel = (ibCharCur + cbBufMax - iMatchCur) & (cbBufMax-1);
            if (bPassCnt == 0) // Analyze Phase
                {
                if (isPtr)
                    fpbAnalysis4b[cbMatchCur-cbIndex]++;
                else
                    fpbAnalysis4a[cbMatchCur-cbIndex]++;

                fpbAnalysis6 [iMatchRel>>6]++;
                }
            else
                {
                if (isPtr)
                    WriteHuffman4b((BYTE) (cbMatchCur-cbIndex));
                else
                    WriteHuffman4a((BYTE) (cbMatchCur-cbIndex));

                WriteHuffman6((BYTE) (iMatchRel>>6));
                WriteBits(iMatchRel, 6);
                }
            isPtr = FALSE;
            }

        cbMatchLast = cbMatchCur;

        for (us = 0; us < (USHORT)cbMatchLast && (ch=ReadByte(fhSrc))!=EOF;us++)
            {
            LZDeleteNode(ibStringCur);      /* Delete old strings and */
            ringBuf[ibStringCur] = (BYTE)ch;        /* read new bytes */

              /* If the position is near the end of buffer, extend the
              ** buffer to make string comparison easier.
              */
            if ((USHORT)ibStringCur < cbStrMax - 1)
                ringBuf[ibStringCur + cbBufMax] = (BYTE)ch;

              /* ring buffer, increment the position modulo N. */
            ibStringCur = ibStringCur+1 & (cbBufMax - 1);
            ibCharCur = ibCharCur+1 & (cbBufMax - 1);

              /* Register the string in ringBuf[r..r+cbStrMax-1] */
            LZInsertNode(ibCharCur);
            }

        while (us++ < (USHORT) cbMatchLast)      /* After the end of text, */
            {
            LZDeleteNode(ibStringCur);        /* no need to read, but */

            ibStringCur = ibStringCur+1 & (cbBufMax - 1);
            ibCharCur = ibCharCur+1 & (cbBufMax - 1);
            if (--cbLen)
                LZInsertNode(ibCharCur);          /* buffer may not be empty. */
            }
        } while (cbLen > 0); /* length of string to be processed is zero */

    if (ibLitBuf)  // Flush Literals
        {
        if (ibLitBuf == cbLitMax)
            isPtr = FALSE;
        else
            isPtr = TRUE;

        if (bPassCnt == 0)  // Analyze phase
            {
            fpbAnalysis4a[0]++;
            fpbAnalysis5[ibLitBuf-1]++;
            }
        else
            {
            WriteHuffman4a(0);
            WriteHuffman5((BYTE) (ibLitBuf-1));
            }
        if (fWriteError || fDestFull)
            goto LExit;
        for (cnt = 0; cnt < ibLitBuf; cnt++)
            {
            if (bPassCnt == 0) // Analyze phase
                fpbAnalysis8[rgbLitBuf[cnt]]++;
            else
                WriteHuffman8(rgbLitBuf[cnt]);

            if (fWriteError || fDestFull)
                goto LExit;
            }
        ibLitBuf = 0; 
        }

    if (bPassCnt > 0)  // Not the analyze phase
        {
        if (cbBitHold)  // There are a few bits left in the stream
            if (isPtr)
                // Make sure extraneous bits aren't interpeted as a 0
                if (fpct4b[0].usCode&1)
                    WriteBits(0,   8-cbBitHold);
                else
                    WriteBits(255, 8-cbBitHold);
            else
                if (fpct4a[0].usCode&1)
                    WriteBits(0,   8-cbBitHold);
                else
                    WriteBits(255, 8-cbBitHold);

        WriteOutBuff(0);
        }
    else
        {
        BuildHTree(fpbAnalysis4a, 16, fpct4a);
        if (!BuildCodeTable(fpct4a, 16))
            {
            fJmpEnvSet = FALSE;
            FreeJJJGlobals();
            return(rcGenericCompError);
            }

        BuildHTree(fpbAnalysis4b, 16, fpct4b);  
        if (!BuildCodeTable(fpct4b, 16)) 
            {
            fJmpEnvSet = FALSE;
            FreeJJJGlobals();
            return(rcGenericCompError);
            }

        BuildHTree(fpbAnalysis5, 32, fpct5);
        if (!BuildCodeTable(fpct5, 32))
            {
            fJmpEnvSet = FALSE;
            FreeJJJGlobals();
            return(rcGenericCompError);
            }

        BuildHTree(fpbAnalysis6, 64, fpct6);
        if (!BuildCodeTable(fpct6, 64))
            {
            fJmpEnvSet = FALSE;
            FreeJJJGlobals();
            return(rcGenericCompError);
            }

        BuildHTree(fpbAnalysis8, 256, fpct8);
        if (!BuildCodeTable(fpct8, 256))
            {
            fJmpEnvSet = FALSE;
            FreeJJJGlobals();
            return(rcGenericCompError);
            }
      
        cb4aHdr = TableComp(fpct4a, 16, &b4aBit);
        cb4bHdr = TableComp(fpct4b, 16, &b4bBit);
        cb5Hdr  = TableComp(fpct5,  32, &b5Bit);
        cb6Hdr  = TableComp(fpct6,  64, &b6Bit);
        cb8Hdr  = TableComp(fpct8, 256, &b8Bit);
      
        CodeTradeoff(fpbAnalysis4a, fpct4a, 16, cb4aHdr, &b4aBit);
        CodeTradeoff(fpbAnalysis4b, fpct4b, 16, cb4bHdr, &b4bBit);
        CodeTradeoff(fpbAnalysis5 , fpct5,  32, cb5Hdr,  &b5Bit);
        CodeTradeoff(fpbAnalysis6 , fpct6,  64, cb6Hdr,  &b6Bit);
        CodeTradeoff(fpbAnalysis8 , fpct8, 256, cb8Hdr,  &b8Bit);

        if (lseek(fhSrc, lcbSrcStart, SEEK_SET) == -1L)
            {
            fJmpEnvSet = FALSE;
            FreeJJJGlobals();
            return((LONG)rcReadSeekError);
            }

        bPassCnt = 1;
        lReturn  = Lcb_JJJ1_CompressToFile(fhSrc, fhDest, lcbDestMax);
        bPassCnt = 0;
        return(lReturn);
        }
LExit:
    fJmpEnvSet = FALSE;
    FreeJJJGlobals();
 
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
    lcbDest = Lcb_JJJ1_DecompressToFile(fhDest, -1, NIL, 0L, NULL, 0L);
    if (lcbDest < rcNoError)
        return(lcbDest);

    /* reset fhSrc */
    if (lseek(fhSrc, libSrcStart + lcbDest, SEEK_SET) == -1L)
        return((LONG)rcReadSeekError);

    return(lReturn);
}
