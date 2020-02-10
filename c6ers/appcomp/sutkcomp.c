/* TS = none */
/* REVIEW - we really should have a ReadChar() in case we support kanji */
/*
**  SUTKCOMP.C  --  general compression routines for
**                  Setup Toolkits.  Handles all header manipulation.
*/

#include <io.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <dos.h>

#include "sutkcomp.h"


  /* global header info variables */
extern SHORT   vwAlgType;
extern LONG    vlcbSrcLength;
extern BOOL    vfChecksum;
extern USHORT  vusChecksum;
extern USHORT  vcbArgs;
extern BYTE *  vrgbArgs;
extern CHAR    vszBaseName[9];
extern CHAR    vszExtension[4];
extern SZ      vszText;
extern USHORT  vcbText;

#ifdef WIN2_VER
extern HANDLE  vhRgbArgs;
extern HANDLE  vhSzText;
#endif /* WIN2_VER */

  /* global variables needed within toolkit layer */
extern  LONG    vlibChecksum;
extern  LONG    vlibSrcLength;


  /* forward declarations of local routines */
extern  SHORT   far CbWriteFileHeader(int fhDest);
extern  BOOL        FPatchUs(int fh, LONG lib, USHORT us);
extern  BOOL        FPatchUl(int fh, LONG lib, ULONG ul);

#ifdef NOT_USED
extern  USHORT      CbWriteFar(int fh, BYTE far * fp, USHORT cb);
#endif /* NOT_USED */


/*
**  SHORT  far  CbWriteFileHeader(int fhDest)
**
**  Write the info contained in the header globals out to fhDest.  Assumes
**  that fhDest is positioned to where the header should be written (the
**  beginning of the file.)  If the source file length or checksum need to
**  be included in the header, the global variables vlibSrcLength and
**  vlibChecksum are set to the positions to patch those numbers later when 
**  the length and checksum are known.
**
**  Returns the length of the header written.  fhDest is left positioned at
**  the byte after the header.  If there is an error during writing, the file
**  position is reset to its original value, and the file truncated.
*/
SHORT  far  CbWriteFileHeader(int fhDest)
{
    USHORT    us;
    USHORT    cbWritten = 0;
    USHORT    cbBase;
    USHORT    cbExt;
    ULONG     ulSizeSeek;
    BOOL      fOkayWrite;
    LONG      libDestStart;

    if (fhDest != -1 && (libDestStart = tell(fhDest)) == -1)
        return(rcWriteSeekError);

    cbBase = strlen(vszBaseName);
    cbExt = strlen(vszExtension);

    /* REVIEW: this sleazy thing checks to make sure we have a valid alg type
       for the flags that are currently turned on.  REWRITE THIS. */
    if (TRUE
#ifdef ZK1
            && (vwAlgType != wAlgTypeZK1)
#endif /* ZK1 */
#ifdef JJJ1
            && (vwAlgType != wAlgTypeJJJ1)
#endif /* JJJ1 */
#ifdef NC_XOR1
            && (vwAlgType != wAlgTypeNoCompress)
            && (vwAlgType != wAlgTypeXOR1)
#endif /* NC_XOR1 */
            )
        {
        return(rcUnknownAlgType);
        }

    if (cbBase > 8 || cbExt > 3)
        return(rcFilenamesTooLong);

    fOkayWrite = FWriteNBytes(fhDest, rgbMagicValue, cbMagic);
    cbWritten += cbMagic;

    fOkayWrite &= FWriteUs(fhDest, vwAlgType);
    cbWritten += sizeof(USHORT);

    if (fhDest != -1)
        ulSizeSeek = tell(fhDest);         /* where to write cbHeader & flags */
    fOkayWrite &= FWriteUs(fhDest, 0);                            /* cbHeader */
    fOkayWrite &= FWriteUs(fhDest, 0);                               /* flags */
    cbWritten += sizeof(USHORT) * 2;

    us = 0;                            /* build flags and write optional data */
    if (vlcbSrcLength != NIL)
        {
        us |= bmSrcLength;
        if (fhDest != -1)
            vlibSrcLength = tell(fhDest);     /* length will be patched later */
        fOkayWrite &= FWriteUl(fhDest, NIL);
        cbWritten += sizeof(ULONG);
        }
    if (vfChecksum)
        {
        us |= bmChecksum;
        if (fhDest != -1)
            vlibChecksum = tell(fhDest);           /* where to write checksum */
        fOkayWrite &= FWriteUs(fhDest, vusChecksum);
        cbWritten += sizeof(USHORT);
        }
    if (vcbArgs != 0)
        {
        us |= bmArgs;
        fOkayWrite &= FWriteUs(fhDest, vcbArgs);
        fOkayWrite &= FWriteNBytes(fhDest, vrgbArgs, vcbArgs);
        cbWritten += sizeof(USHORT) + vcbArgs;
        }
    if (vszBaseName[0] != '\0')
        {
        us |= bmBaseName;
        fOkayWrite &= FWriteNBytes(fhDest, vszBaseName, cbBase);
        fOkayWrite &= FWriteByte(fhDest, '\0');
        cbWritten += 1 + cbBase;
        }
    if (vszExtension[0] != '\0')
        {
        us |= bmExtension;
        /* REVIEW for " " should we write " \0" or "\0"? */
        fOkayWrite &= FWriteNBytes(fhDest, vszExtension, cbExt);
        fOkayWrite &= FWriteByte(fhDest, '\0');
        cbWritten += 1 + cbExt;
        }
    if (vszText != NULL)
        {
        us |= bmText;
        fOkayWrite &= FWriteUs(fhDest, vcbText);
        fOkayWrite &= FWriteNBytes(fhDest, vszText, vcbText);
        cbWritten += sizeof(USHORT) + vcbText;
        }

#ifdef EAS
    if (vuscbEAs > 0)
        {
        us |= bmEAs;
        fOkayWrite &= FWriteUs(fhDest, vuscbEAs);
        fOkayWrite &= FWriteNBytes(fhDest, vrgbEAs, vuscbEAs);
        cbWritten += sizeof(USHORT) + vuscbEAs;
        free(vrgbEAs);
        vrgbEAs = NULL;
        vuscbEAs = 0;
        }
#endif /* EAS */

      /* Patch unfilled fields in header (cbHeader and flags) */
    if (fOkayWrite && fhDest != -1 && ulSizeSeek != -1)
        {
        BOOL   fOkaySeek = (BOOL)TRUE;

        if (lseek(fhDest, ulSizeSeek, SEEK_SET) == -1)
            fOkaySeek = FALSE;

        fOkayWrite &= FWriteUs(fhDest, cbWritten);
        fOkayWrite &= FWriteUs(fhDest, us);

        if (lseek(fhDest, 0L, SEEK_END) == -1)
            fOkaySeek = FALSE;

        if (!fOkaySeek)
            {
            lseek(fhDest, libDestStart, SEEK_SET);
            chsize(fhDest, libDestStart);
            return(rcWriteSeekError);
            }
        }

      /* if there's an error, blows away file */
    if (!fOkayWrite && fhDest != -1)
        {
        lseek(fhDest, libDestStart, SEEK_SET);
        chsize(fhDest, libDestStart);
        return(rcWriteError);
        }

    return(cbWritten);
}


/*
**  BOOL  FPatchUs(int fh, LONG lib, USHORT us)
**
**  Patch the USHORT at lib offset of fh with the us value, but leave
**  the fh unchanged.  Return TRUE if okay, FALSE in failure.
*/
BOOL  FPatchUs(int fh, LONG lib, USHORT us)
{
    LONG   libSav;

    if (fh == -1 || (libSav = tell(fh)) == -1 || lseek(fh, lib, SEEK_SET) == -1)
        return(FALSE);

    if (!FWriteUs(fh, us))
        {
        lseek(fh, libSav, SEEK_SET);
        return(FALSE);
        }

    if (lseek(fh, libSav, SEEK_SET) == -1)
        return(FALSE);

    return((BOOL)TRUE);
}


/*
**  BOOL  FPatchUl(int fh, LONG lib, ULONG ul)
**
**  Patch the ULONG at lib offset of fh with the ul value, but leave
**  the fh unchanged.  Return TRUE if okay, FALSE in failure.
*/
BOOL  FPatchUl(int fh, LONG lib, ULONG ul)
{
    LONG   libSav;

    if (fh == -1 || (libSav = tell(fh)) == -1 || lseek(fh, lib, SEEK_SET) == -1)
        return(FALSE);

    if (!FWriteUl(fh, ul))
        {
        lseek(fh, libSav, SEEK_SET);
        return(FALSE);
        }

    if (lseek(fh, libSav, SEEK_SET) == -1)
        return(FALSE);

    return((BOOL)TRUE);
}


#ifdef NOT_USED
/*
**  USHORT  CbWriteFar(int fh, BYTE far * fp, USHORT cb)
**
**  write with far pointer
*/
USHORT  CbWriteFar(int fh, BYTE far * fp, USHORT cb)
{
    union  REGS  inregs, outregs;
    struct SREGS segregs;

    inregs.x.bx = fh;            /* file handle */
    inregs.x.cx = cb;            /* bytes to write */
    inregs.x.dx = FP_OFF(fp);    /* offset of buffer */
    segregs.ds  = FP_SEG(fp);    /* segment of buffer */
    inregs.h.ah = 0x40;          /* Write Handle */

    intdosx(&inregs, &outregs, &segregs);

    return((outregs.x.cflag) ? 0 : outregs.x.ax);
}
#endif /* NOT_USED */
