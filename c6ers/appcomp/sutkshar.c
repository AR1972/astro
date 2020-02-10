/* TS = none */
/*
**  SUTKSHAR.C  --  general shared comp/decompression routines for
**                  Setup Toolkits.  Handles all header manipulation.
*/

#include <io.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <dos.h>
#include <ctype.h>
#include <fcntl.h>

#include "sutkcomp.h"

#ifdef WIN_VER
HANDLE hOutBuf = NULL;
#endif /* WIN_VER */

BYTE _bWrite = 0;

BOOL vfUserCancel = FALSE;

BOOL FTerminateDecomp(void)
{
    vfUserCancel = TRUE;
    return(TRUE);
}

#include "setjmp.h"

BOOL    fJmpEnvSet = FALSE;
jmp_buf jmpEnv = { NULL };


  /* global header info variables and default values */
SHORT   vwAlgType =        wAlgTypeNil;
LONG    vlcbSrcLength =    NIL;
BOOL    vfChecksum =       FALSE;
USHORT  vusChecksum =      0;
USHORT  vcbArgs =          0;
BYTE *  vrgbArgs =         NULL;
CHAR    vszBaseName[9] =   "\0";
CHAR    vszExtension[4] =  "\0";
SZ      vszText =          NULL;
USHORT  vcbText =          0;
#ifdef EAS
CHAR *  vrgbEAs =          NULL;
CHAR far *  vfrgbEAs =     NULL;
USHORT  vuscbEAs =         0;
#endif /* EAS */

  /* global variables needed within toolkit layer */
LONG    vlibChecksum =     NIL;
LONG    vlibSrcLength =    NIL;

  /* global variables needed for Progress Gizmo Callback */
PFNWFROMW vpfn       = NULL;
PFNWFROMW vpfnYield  = NULL;
int       vcTicks    = 0;
LONG      vcbPerTick = 0L;
LONG      vcbCur     = 0L;


  /* forward declarations of local routines */
extern  BOOL    far FFreeHeaderInfo(void);



/*
**  BOOL  far  FFreeHeaderInfo(void)
**
**  Frees any allocated global header structures, and resets all header
**  variables to their default values.  Always returns TRUE.
**
**  This procedure is called at the beginning of WReadHeaderInfo, so the
**  user only needs to directly call this procedure after finishing reading
**  the last file to be decompressed.
*/
BOOL  far  FFreeHeaderInfo(void)
{
      /* free any space used */
    if (vrgbArgs != NULL)
        free(vrgbArgs);
    if (vszText != NULL)
        free(vszText);
#ifdef EAS
    if (vrgbEAs != NULL)
        free(vrgbEAs);
    if (vfrgbEAs != NULL)
        _ffree(vrgbEAs);
#endif /* EAS */

      /* reset to defaults */
    vwAlgType =        wAlgTypeNil;
    vlcbSrcLength =    NIL; 
    vfChecksum =       FALSE;
    vusChecksum =      0;
    vcbArgs =          0;
    vrgbArgs =         NULL;
    vszBaseName[0] =   '\0';
    vszExtension[0] =  '\0';
    vszText =          NULL;
    vcbText =          0;
#ifdef EAS
    vrgbEAs =          NULL;
    vfrgbEAs=          NULL;
    vuscbEAs=          0;
#endif /* EAS */

    return((BOOL)TRUE);
}


  /* Buffered Read/Write stuff */
static	void	  fmemmove(BYTE far * fpb1, BYTE far * fpb2, USHORT cb);
static	BYTE far *NormalizePtr(BYTE far *fp);
static  unsigned  readf(int fh, CHAR far * fp, unsigned uLen);
static  unsigned  writef(int fh, CHAR far * fp, unsigned uLen);

LONG        lcbDest      = 0L;     /* bytes already emptied from output buf */
LONG        lcbDestStop  = 0L;     /* when to quit, NIL means run to EOF */
BOOL        fWriteError  = FALSE;
BOOL        fDestFull    = FALSE;  /* set when we reach lcbDestStop */
LONG        lcbSkipOut   = 0L;     /* bytes to skip (seek) before writing */
int         fhDestGlobal = 0;      /* output file */
int         fhSrcGlobal  = 0;      /* used by UnpackZK1() for input */
BYTE far *  fpbBufDest   = NULL;   /* buffer to use instead of a file */

BYTE far *  fpbOutBuf    = NULL;
BYTE far *  fpbOutBufCur = NULL;
BYTE far *  fpbOutBufEnd = NULL;

BYTE far *  fpbInBuf     = NULL;
BYTE far *  fpbInBufCur  = NULL;
BYTE far *  fpbInBufEnd  = NULL;


/*
**  void  WriteOutBuff(BYTE bToAdd)
**
**  Empty the output buffer.  Set fWriteError if a problem is encountered.
**  Set fDestFull if we have surpassed lcbDestStop.  Increment lcbDest by
**  how many bytes are emptied.  There are three cases of how to empty the
**  bytes:
**    1) written to a buffer                   [fpbBufDest != NULL]
**    2) written to a file                     [fhDestGlobal != -1]
**    3) not written at all (just counting!)   [else]
*/
void WriteOutBuff(BYTE bToAdd)
{
    USHORT   cbWrite;
    USHORT   cbSkip = 0;      /* how many bytes of the current buffer to skip */

    cbWrite = fpbOutBufCur - fpbOutBuf;                    /* bytes in buffer */

    if (lcbDestStop != NIL && (LONG)cbWrite >= (lcbDestStop - lcbDest))
        {
        cbWrite = (USHORT)(lcbDestStop - lcbDest);              /* stop early */
        fDestFull = (BOOL)TRUE;
        }

    if (lcbDestStop != NIL && lcbDest > lcbDestStop)      /* already too many */
        {
        lcbDest = 0L;
        fWriteError = fDestFull = (BOOL)TRUE;
        }
    
    if (lcbDest + cbWrite > lcbSkipOut)    /* do we actually need to transfer */
        {
        if (lcbSkipOut > lcbDest)                  /* skip a few at beginning */
            cbWrite -= (cbSkip = (USHORT)(lcbSkipOut - lcbDest));

        if (fhDestGlobal != -1)                              /* write to file */
            {
            if (cbWrite != writef(fhDestGlobal, fpbOutBuf + cbSkip, cbWrite))
                {
                lcbDest = 0L;
                fWriteError = fDestFull = (BOOL)TRUE;
                }
            }
        else if (fpbBufDest != NULL)                       /* write to buffer */
	    {
	    fpbBufDest = NormalizePtr(fpbBufDest);
            fmemmove(fpbBufDest, fpbOutBuf + cbSkip, cbWrite);
            fpbBufDest += cbWrite;      /* fix fpbBufDest so next write works */
            }
        }
    /* case 3 - just counting - falls through */

    if (!fWriteError)
        lcbDest += cbWrite + cbSkip;

    fpbOutBufCur = fpbOutBuf;

    *fpbOutBufCur++ = bToAdd;
}


/*
**  static  void  fmemmove(BYTE far * fpb1, BYTE far * fpb2, USHORT cb)
**
**  a far version of memmove
*/
static  void  fmemmove(BYTE far * fpb1, BYTE far * fpb2, USHORT cb)
{
    if (fpb1 == fpb2 || cb == 0)
        return;

    if (fpb1 < fpb2)
        for ( ; cb-- > 0; )
            *(fpb1++) = *(fpb2++);
    else
        {
        fpb1 += cb - 1;
        fpb2 += cb - 1;
        for ( ; cb-- > 0; )
            *(fpb1--) = *(fpb2--);
        }
}

static BYTE far *NormalizePtr(BYTE far *fp)
{
    FP_SEG(fp) += FP_OFF(fp) >> 4;
    FP_OFF(fp) &= 0x000F;

    return(fp);
}


/*
**  BOOL  FAllocateReadWriteGlobals(LONG lcbDestMax)
*/
BOOL  FAllocateReadWriteGlobals(LONG lcbDestMax)
{
    USHORT  cbBuf;

    lcbDest = 0L;
    lcbDestStop = lcbDestMax;
    fpbBufDest = NULL;
    fWriteError = FALSE;
    fDestFull = FALSE;

    /* allocate the biggest possible buffer, then split between read & write */
    cbBuf = ((USHORT)126 * 512);
#ifndef WIN_VER
    while (cbBuf >= (3 * 512)
           && (fpbOutBuf = (BYTE far *)_fmalloc(cbBuf)) == NULL)
        cbBuf -= (3 * 512);
#else  /* WIN_VER */
    GlobalCompact((DWORD)cbBuf);
    while (cbBuf >= (3 * 512)
           && (hOutBuf = GlobalAlloc(GHND, (DWORD)cbBuf)) == NULL)
        cbBuf -= (3 * 512);
    if (hOutBuf != NULL)
        fpbOutBuf = (BYTE far *)GlobalLock(hOutBuf);
    else
        fpbOutBuf = NULL;
#endif /* WIN_VER */

    if (fpbOutBuf == NULL)
        {
        FreeReadWriteGlobals();
        return(FALSE);
        }

    fpbInBufEnd = fpbOutBuf + cbBuf;
    fpbInBufCur = fpbInBufEnd;   /* to force a buffer refill the first time */
    fpbOutBufCur = fpbOutBuf;

    cbBuf /= 512;
    cbBuf /= 3;
    cbBuf *= 512;

    /* REVIEW if cbBuf > libStart + lcbBuf in DecompToBuffer case then
    **   should be able to quit earlier by limiting OutBuf */

    fpbOutBufEnd = fpbOutBuf + cbBuf;
    fpbInBuf = fpbOutBufEnd;

    return((BOOL)TRUE);
}


/*
**  void  FreeReadWriteGlobals(void)
*/
void FreeReadWriteGlobals(void)
{
#ifndef WIN_VER
    if (fpbOutBuf != NULL)
        {
        _ffree(fpbOutBuf);
        fpbOutBuf = NULL;
        }
#else  /* WIN_VER */
    if (hOutBuf != NULL)
        {
        GlobalUnlock(hOutBuf);
        GlobalFree(hOutBuf);
        hOutBuf = NULL;
        fpbOutBuf = NULL;
        }
#endif /* WIN_VER */
}



USHORT  ReadByte(fh)
int fh;
{
    if (fpbInBufCur >= fpbInBufEnd)
        {
        USHORT   cb;

          /* Progress Gizmo */
        if (vcTicks <= 0)
            vcbCur = 0L;
        else if (vcbCur > vcbPerTick && vpfn != NULL)
            {
            long lcTicks = vcbCur / vcbPerTick;
            int  cTicks  = (int)(lcTicks & 0x00007FFF);

            vcbCur -= (cTicks * vcbPerTick);
            if (cTicks > vcTicks)
                cTicks = vcTicks;
            if (!((*vpfn)(cTicks)))
                vpfn = NULL;
            vcTicks -= cTicks;
            }

        if ((cb = readf(fh, fpbInBuf, (USHORT)(fpbInBufEnd - fpbInBuf))) == 0)
            return((USHORT)EOF);
        fpbInBufCur = fpbInBuf;
        fpbInBufEnd = fpbInBuf + cb;

        vcbCur += cb;
        }

    return(*fpbInBufCur++);
}


/* read with far pointer; returns count read if successful, 0 if error */
static unsigned readf(int fh, CHAR far * fp, unsigned uLen)
{
    unsigned  uResult;

    if (vpfnYield != NULL)
        (*vpfnYield)(0);

#ifdef OS2_VER
    if (DosRead((HFILE)fh, (PVOID)fp, (USHORT)uLen, (PUSHORT)(&uResult)))
#else  /* DOS_VER */
    if (_dos_read(fh, fp, uLen, &uResult))
#endif /* OS2_VER */
        {
        if (fJmpEnvSet)
            longjmp(jmpEnv, rcReadError);
        return(0);
        }

    if (vpfnYield != NULL)
        (*vpfnYield)(0);

    return(uResult);
}


/* write with far pointer; returns count written if successful, 0 if error */
static unsigned writef(int fh, CHAR far * fp, unsigned uLen)
{
    unsigned  uResult;

#ifdef OS2_VER
    if (DosWrite(fh, (PVOID)fp, uLen, (PUSHORT)(&uResult)))
#else /* DOS_VER */
    if (_dos_write(fh, fp, uLen, &uResult))
#endif /* OS2_VER */
        {
        if (fJmpEnvSet)
            longjmp(jmpEnv, rcWriteError);
        return(0);
        }
    return(uResult);
}
