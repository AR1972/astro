/*
**  JJJ1SHAR.C --  Shared code, variables, and defines for
**                 JJJ1 compression/decompression module.
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
#include <malloc.h>
#include <dos.h>
#include <stdio.h>

#include "..\sutkcomp.h"
#include "jjj1.h"

#ifdef WIN_VER
HANDLE vhAnalysis = NULL;
HANDLE vhCt4a = NULL;
HANDLE vhHtArr = NULL;
HANDLE vhLookup4a = NULL;
#endif /* WIN_VER */


USHORT iPowers[16] = { 0,1,3,7,15,31,63,127,255,511,1023,2047,4095,8191,16383,
                       32767 };

//Analysis globals
long far *   fpbAnalysis     = NULL;
long far *   fpbAnalysis4a   = NULL;
long far *   fpbAnalysis4b   = NULL;
long far *   fpbAnalysis5    = NULL;
long far *   fpbAnalysis6    = NULL;
long far *   fpbAnalysis8    = NULL;

HTREE far *  fphtArr         = NULL;

CODETABLE far *  fpct4a      = NULL;
CODETABLE far *  fpct4b      = NULL;
CODETABLE far *  fpct5       = NULL;
CODETABLE far *  fpct6       = NULL;
CODETABLE far *  fpct8       = NULL;

//Decomp globals
BYTE far *       fpbLookup4a = NULL;
BYTE far *       fpbLookup4b = NULL;
BYTE far *       fpbLookup5  = NULL;
BYTE far *       fpbLookup6  = NULL;
BYTE far *       fpbLookup8  = NULL;


/*
**  BOOL  FAllocateJJJGlobals(LONG lcbDestMax, fCompressing)
**
**  Global arrays take about 45K so I wanted to allocate and free them rather
**  than leave them around when not needed.
**
**  rgRoot and rgND are only needed when compressing, so they are only allocated
**  if fCompressing is TRUE.
*/
BOOL  FAllocateJJJGlobals(LONG lcbDestMax, BOOL fCompressing)
{
    if (fCompressing)
        {
        // This chunk holds all the long arrays for analysis
#ifndef WIN_VER
        fpbAnalysis = (long far *)_fmalloc(((256+64+32+16+16)*sizeof(long)));
#else  /* WIN_VER */
        GlobalCompact((DWORD)((256+64+32+16+16)*sizeof(long)));
        if ((vhAnalysis = GlobalAlloc(GHND,
                (DWORD)((256+64+32+16+16)*sizeof(long)))) != NULL)
            fpbAnalysis = (long far *)GlobalLock(vhAnalysis);
        else
            fpbAnalysis = NULL;
#endif /* WIN_VER */

        if (fpbAnalysis == NULL)
            {
            FreeJJJGlobals();
            return(FALSE);
            }
        fpbAnalysis8 = fpbAnalysis;
        fpbAnalysis6 = &(fpbAnalysis8[256]);
        fpbAnalysis5 = &(fpbAnalysis6[64]);
        fpbAnalysis4a= &(fpbAnalysis5[32]);
        fpbAnalysis4b= &(fpbAnalysis4a[16]);
        }
    else
        {
        fpbAnalysis = NULL;
#ifdef WIN_VER
        vhAnalysis = NULL;
#endif /* WIN_VER */
        }

    // This chunk holds all the code tables
#ifndef WIN_VER
    fpct4a = (CODETABLE far *)_fmalloc(((256+64+32+16+16)*sizeof(CODETABLE)));
#else  /* WIN_VER */
    GlobalCompact((DWORD)((256+64+32+16+16)*sizeof(CODETABLE)));
    if ((vhCt4a = GlobalAlloc(GHND,
            (DWORD)((256+64+32+16+16)*sizeof(CODETABLE)))) != NULL)
        fpct4a = (CODETABLE far *)GlobalLock(vhCt4a);
    else
        fpct4a = NULL;
#endif /* WIN_VER */

    if (fpct4a == NULL)
        {
        FreeJJJGlobals();
        return(FALSE);
        }
    fpct4b = &(fpct4a[16]);
    fpct5  = &(fpct4b[16]);
    fpct6  = &(fpct5[32]);
    fpct8  = &(fpct6[64]);


    // Allocate a structure used to build huffman trees
#ifndef WIN_VER
    fphtArr = (HTREE far *)_fmalloc((512*sizeof(HTREE)));
#else  /* WIN_VER */
    GlobalCompact((DWORD)(512*sizeof(HTREE)));
    if ((vhHtArr = GlobalAlloc(GHND, (DWORD)(512*sizeof(HTREE)))) != NULL)
        fphtArr = (HTREE far *)GlobalLock(vhHtArr);
    else
        fphtArr = NULL;
#endif /* WIN_VER */

    if (fphtArr == NULL)
        {
        FreeJJJGlobals();
        return(FALSE);
        }

    // This chunk holds all the decomp lookup tables
    if (!fCompressing)
        {
#ifndef WIN_VER
        fpbLookup4a = (BYTE far *)_fmalloc(((5*256)*sizeof(BYTE)));
#else  /* WIN_VER */
        GlobalCompact((DWORD)((5*256)*sizeof(BYTE)));
        if ((vhLookup4a = GlobalAlloc(GHND,
                (DWORD)((5*256)*sizeof(BYTE)))) != NULL)
            fpbLookup4a = (BYTE far *)GlobalLock(vhLookup4a);
        else
            fpbLookup4a = NULL;
#endif /* WIN_VER */

        if (fpbLookup4a == NULL)
            {
            FreeJJJGlobals();
            return(FALSE);
            }
        fpbLookup4b = &(fpbLookup4a[256]);
        fpbLookup5  = &(fpbLookup4b[256]);
        fpbLookup6  = &(fpbLookup5[256]);
        fpbLookup8  = &(fpbLookup6[256]);
        }
    else
        {
        fpbLookup4a = NULL;
#ifdef WIN_VER
        vhLookup4a = NULL;
#endif /* WIN_VER */
        }

    cbStrMax = (15 + cbIndex);
    if (!FAllocateLZGlobals(lcbDestMax, fCompressing))
        {
        FreeJJJGlobals();
        return(FALSE);
        }

    return((BOOL)TRUE);
}


/*
**  void  FreeJJJGlobals(void)
*/
void FreeJJJGlobals(void)
{
    FreeLZGlobals();

#ifndef WIN_VER
    if (fpbAnalysis != NULL)
        _ffree(fpbAnalysis);

    if (fpct4a != NULL)
        _ffree(fpct4a);

    if (fpbLookup4a != NULL)
        _ffree(fpbLookup4a);

    if (fphtArr != NULL)
        _ffree(fphtArr);
#else  /* WIN_VER */
    if (vhAnalysis != NULL)
        {
        GlobalUnlock(vhAnalysis);
        GlobalFree(vhAnalysis);
        vhAnalysis = NULL;
        }

    if (vhCt4a != NULL)
        {
        GlobalUnlock(vhCt4a);
        GlobalFree(vhCt4a);
        vhCt4a = NULL;
        }

    if (vhLookup4a != NULL)
        {
        GlobalUnlock(vhLookup4a);
        GlobalFree(vhLookup4a);
        vhLookup4a = NULL;
        }

    if (vhHtArr != NULL)
        {
        GlobalUnlock(vhHtArr);
        GlobalFree(vhHtArr);
        vhHtArr = NULL;
        }
#endif /* WIN_VER */
}


// Routine builds huffman codes given their lengths and the table size
BOOL BuildCodeTable(CODETABLE far * fpct, int cbct)
{
    int           cnt, prev = -1;
    BYTE          curlen = 0;
    unsigned int  curcode = 0;

    for (cnt = 0; cnt < cbct; cnt++)
        {
        fpct[cnt].usCode = 0;
        }

    while (++curlen < 16)
        {
        for (cnt = 0; cnt < cbct; cnt++)
            {
            if (fpct[cnt].cbCode == curlen)  // Assign it a code
                {
                fpct[cnt].usCode = curcode++;
                if (prev != -1)
                    fpct[prev].nextCode = (BYTE) cnt;
                prev = cnt;
                }
            }
        curcode <<= 1;
        }

    for (cnt = 0; cnt < cbct; cnt++)
        {
        if (fpct[cnt].cbCode >= 16)
            return(FALSE);
        }

    return((BOOL)TRUE);
}
