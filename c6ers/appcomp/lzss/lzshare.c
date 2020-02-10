/* TS = none */
/*
**  LZSHARE.C --  Shared code, variables, and defines for
**                 LZ compression/decompression module.
*/

#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <io.h>
#include <fcntl.h>
#include <malloc.h>
#include <dos.h>

#include "..\sutkcomp.h"
#include "lz.h"

#ifdef WIN_VER
HANDLE hRingBuf = NULL;
#endif /* WIN_VER */


USHORT  iMatchCur = 0;                  /* of longest match. These are */
USHORT  cbMatchCur = 0;                 /* set by the InsertNode() procedure. */

USHORT  cbStrMax = (16 + cbIndex);

ND *  rgRoot = NULL;                       // array of root pointers
ND *  rgND   = NULL;                       // node pointers themselves
ND    nilND  = { NULL };

  /* ring buffer of size cbBufMax, with extra cbStrMax-1 bytes */
  /* to facilitate string comparison */
BYTE  far *  ringBuf = NULL;


/*
**  BOOL  FAllocateLZGlobals(LONG lcbDestMax, fCompressing)
**
**  LZGlobal arrays take about 45K so I wanted to allocate and free them rather
**  than leave them around when not needed.
**
**  rgRoot and rgND are only needed when compressing, so they are only allocated
**  if fCompressing is TRUE.
*/
BOOL  FAllocateLZGlobals(LONG lcbDestMax, BOOL fCompressing)
{
    if (fCompressing)
        {
        rgND = (ND *)(malloc((cbBufMax + 1) * sizeof(ND)));
        rgRoot = (ND *)(malloc(256 * sizeof(ND)));
        if (rgND == NULL || rgRoot == NULL)
            {
            FreeLZGlobals();
            return(FALSE);
            }
        }

#define cbBuf ((cbBufMax + cbStrMax - 1) * sizeof(BYTE))
#ifndef WIN_VER
    ringBuf = (BYTE far *)_fmalloc(cbBuf);
#else  /* WIN_VER */
    GlobalCompact((DWORD)cbBuf);
    if ((hRingBuf = GlobalAlloc(GHND, (DWORD)cbBuf)) != NULL)
        ringBuf = (BYTE far *)GlobalLock(hRingBuf);
    else
        ringBuf = NULL;
#endif /* WIN_VER */
#undef cbBuf

    if (ringBuf == NULL)
        {
        FreeLZGlobals();
        return(FALSE);
        }

    if (!FAllocateReadWriteGlobals(lcbDestMax))
        {
        FreeLZGlobals();
        return(FALSE);
        }

    return((BOOL)TRUE);
}


/*
**  void  FreeLZGlobals(void)
*/
void FreeLZGlobals(void)
{
    FreeReadWriteGlobals();
    if (rgND != NULL)
        {
        free(rgND);
        rgND = NULL;
        }
    if (rgRoot != NULL)
        {
        free(rgRoot);
        rgRoot = NULL;
        }
#ifndef WIN_VER
    if (ringBuf != NULL)
        {
        _ffree(ringBuf);
        ringBuf = NULL;
        }
#else  /* WIN_VER */
    if (hRingBuf != NULL)
        {
        GlobalUnlock(hRingBuf);
        GlobalFree(hRingBuf);
        hRingBuf = NULL;
        ringBuf = NULL;
        }
#endif /* WIN_VER */
}
