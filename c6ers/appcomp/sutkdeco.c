/* TS = none */
/*
**  SUTKDECO.C  --  general decompression routines for
**                  Setup Toolkits.  Handles all header manipulation.
*/

#include <io.h>
#include <stdlib.h>
#include <stdio.h>
#include <malloc.h>
#include <string.h>
#include <dos.h>

#include "sutkcomp.h"


  /* global header info variables and default values */
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
#ifdef EAS
extern CHAR    *vrgbEAs;
extern CHAR    far *vfrgbEAs;
extern USHORT  vuscbEAs;
#endif /* EAS */


#ifdef WIN2_VER
extern HANDLE  vhRgbArgs;
extern HANDLE  vhSzText;
#endif /* WIN2_VER */


  /* forward declarations of local routines */
extern  SHORT   far WReadHeaderInfo(int fhSrc);
extern  BOOL        FReadNBytes(int fh, BYTE *pb, int n);
extern  BOOL        FReadUs(int fh, USHORT *pw);
extern  BOOL        FReadUl(int fh, ULONG *pul);

#ifdef ZECKFORMAT
/* This is the magic value for non-split Zeck files.  Since we can't handle
   Zeck split files, we don't try to understand that form of header. */
#define rgbZeckMagicValue  "\x53\x5a\x20\x88\xf0\x27\x33\xd1"
BOOL   vfZeckFormat = FALSE;
#endif /* ZECKFORMAT */


/*
**  SHORT  far  WReadHeaderInfo(int fhSrc)
**  
**  Determine whether the given file has a header in our format, and if
**  so, extract the information from it into globals.  If the 8 magic bytes
**  at the beginning of the file are correct, we assume it's a compressed
**  file and we give errors if something is wrong with the rest of the
**  header (eg the agorithm is not a known type, or something is wrong with
**  the format of the rest of the header.)  If the 8 magic bytes don't match,
**  we assume it's uncompressed.
**  (Notice that the file could also be uncompressed but still have a
**  header.  The wAlgType in the header would be wAlgTypeNoCompress.)
**  Hitting EOF in reading the magic bytes is not an error, but premature
**  EOF while reading any of the rest of the header is an error.
**
**  Returns the algorithm type used to compress the file, or a return code
**  indicating an error.  If the header is read sucessfully, this procedure
**  may allocate memory to store some of the header information; the caller
**  should always call FFreeHeaderInfo() after the decompression has finished.
**  WReadHeaderInfo calls FFreeHeaderInfo before reading and on errors, so
**  the headers of any number of files can be read and FFreeHeaderInfo need
**  only be called after the last file is read.
**
**  Seeks fhSrc to the byte after the header, if there was a header.
**  If there was no header, resets fhSrc to its original value.
*/
SHORT  far  WReadHeaderInfo(int fhSrc)
{
    USHORT  us;
    USHORT  cbHeader;
    USHORT  usFlags;
    SHORT   rcError;
    LONG    libSav;
    BYTE    b;
    BYTE    rgb[8];

    FFreeHeaderInfo();

    if (fhSrc == -1)
        return(rcReadError);

    if ((libSav = tell(fhSrc)) == NIL)
        return(rcReadSeekError);

      /* read first 8 required bytes */
    if (!FReadNBytes(fhSrc, rgb, 8))
        {
        rcError = rcNoHeader;
        goto ErrorReturn;
        }

      /* check magic number */
    for (us = 0;  us < cbMagic;  us++)
        {
        if (rgb[us] != (BYTE)(*(rgbMagicValue + us)))
            break;
        }
    if (us != cbMagic)   /* magic bytes didn't match */
        {
#ifdef ZECKFORMAT
        for (us = 0;  us < cbMagic;  us++)   /* check Zeck magic bytes */
            {
            if (rgb[us] != (BYTE)(*(rgbZeckMagicValue + us)))
                break;
            }
        if (us == cbMagic - 1)  /* only last byte is different -- split file */
            {
            /* Zeck split file */
            rcError = rcZeckSplitFile;
            goto ErrorReturn;
            }
        if (us == cbMagic && FReadUl(fhSrc, &vlcbSrcLength))
            {
            vfZeckFormat = (BOOL)TRUE;
            vwAlgType = wAlgTypeZK1;
            return(vwAlgType);
            }
#endif /* ZECKFORMAT */
          /* must be an uncompressed file with no header */
        rcError = rcNoHeader;
        goto ErrorReturn;
        }

      /* read next 6 required bytes */
    if (!FReadNBytes(fhSrc, rgb, 6))
        goto ReadErrorReturn;

      /* get algorithm type */
    us = rgb[0] + (rgb[1] << 8);

    /* REVIEW: this sleazy thing checks to make sure we have a valid alg type
       for the flags that are currently turned on.  REWRITE THIS. */
    if (FALSE
#ifdef ZK1
            || (us == wAlgTypeZK1)
#endif /* ZK1 */
#ifdef JJJ1
            || (us == wAlgTypeJJJ1)
#endif /* JJJ1 */
#ifdef NC_XOR1
            || (us == wAlgTypeNoCompress)
            || (us == wAlgTypeXOR1)
#endif /* NC_XOR1 */
            )
        vwAlgType = us;
    else
        {
        rcError = rcUnknownAlgType;
        goto ErrorReturn;
        }

      /* get size of header; save for later Seek */
    cbHeader = rgb[2] + (rgb[3] << 8);

      /* get first flag word */
    usFlags = rgb[4] + (rgb[5] << 8);

      /* then read the optional components that had their flags set */
    if (usFlags & bmSrcLength)
        {
        if (!FReadUl(fhSrc, &vlcbSrcLength))
            goto ReadErrorReturn;
        }
    if (usFlags & bmChecksum)
        {
        vfChecksum = (BOOL)TRUE;
        if (!FReadUs(fhSrc, &vusChecksum))
            goto ReadErrorReturn;
        }
    if (usFlags & bmArgs)
        {
        if (!FReadUs(fhSrc, &vcbArgs))
            goto ReadErrorReturn;
        if (vcbArgs == 0)
            {
            rcError = rcBadHeader;
            goto ErrorReturn;
            }
#ifndef WIN2_VER
        vrgbArgs = (BYTE *)malloc(vcbArgs * sizeof(BYTE));
#else /* WIN2_VER */
        if ((vhRgbArgs = LocalAlloc(LMEM_FIXED, vcbArgs * sizeof(BYTE)))
                                                                        != NULL)
            if ((vszText = (BYTE *)LocalLock(vhRgbArgs)) == NULL)
                {
                LocalFree(vhRgbArgs);
                vhRgbArgs = NULL;
                }
#endif /* WIN2_VER */
        if (vrgbArgs == NULL)
            {
            rcError = rcOutOfMemory;
            goto ErrorReturn;
            }
        if (!FReadNBytes(fhSrc, (BYTE *)vrgbArgs, vcbArgs))
            goto ReadErrorReturn;
        }

      /* handle filename strings */
    if (usFlags & bmBaseName)
        {
        us = 0;
        b = 'Z';
        while (FReadByte(fhSrc, &b) && ((vszBaseName[us] = b) != '\0'))
            {
            if (us < 8)
                us++;
            else
                {
                rcError = rcFilenamesTooLong;
                goto ErrorReturn;
                }
            }
        if (b != '\0')
            goto ReadErrorReturn;
        }
    if (usFlags & bmExtension)
        {
        us = 0;
        b = 'Z';
        while (FReadByte(fhSrc, &b) && ((vszExtension[us] = b) != '\0'))
            {
            if (us < 3)
                us++;
            else
                {
                rcError = rcFilenamesTooLong;
                goto ErrorReturn;
                }
            }
        if (b != '\0')
            goto ReadErrorReturn;
        }
    if (usFlags & bmText)
        {
        if (!FReadUs(fhSrc, &vcbText))
            goto ReadErrorReturn;
        if (vcbText == 0)
            {
            rcError = rcBadHeader;
            goto ErrorReturn;
            }
#ifndef WIN2_VER
        vszText = (SZ)malloc(vcbText * sizeof(CHAR));
#else /* WIN2_VER */
        if ((vhSzText = LocalAlloc(LMEM_FIXED, vcbText * sizeof(CHAR))) != NULL)
            if ((vszText = (SZ)LocalLock(vhSzText)) == NULL)
                {
                LocalFree(vhSzText);
                vhSzText = NULL;
                }
#endif /* WIN2_VER */
        if (vszText == NULL)
            {
            rcError = rcOutOfMemory;
            goto ErrorReturn;
            }
        if (!FReadNBytes(fhSrc, (BYTE *)vszText, vcbText))
            goto ReadErrorReturn;
        }

#ifdef EAS
    if (usFlags & bmEAs)
        {
        if (!FReadNBytes(fhSrc, (BYTE *) &vuscbEAs, sizeof(USHORT)))
            goto ReadErrorReturn;
#ifdef OS2_VER
        vfrgbEAs = _fmalloc(vuscbEAs);
        if (!vfrgbEAs)
            {
            if (lseek(fhSrc, (LONG)vuscbEAs, SEEK_CUR) == -1)
                {
                rcError = rcReadSeekError;
                goto ErrorReturn;
                }
            }
        else
            {
            USHORT cbRead;

            if (DosRead(fhSrc, vfrgbEAs, vuscbEAs, &cbRead))
                {
                _ffree(vfrgbEAs);
                vfrgbEAs = NULL;
                vuscbEAs = 0;
                goto ReadErrorReturn;
                }    
            }
#else
        if (lseek(fhSrc, (LONG)vuscbEAs, SEEK_CUR) == -1)
            {
            rcError = rcReadSeekError;
            goto ErrorReturn;
            }
#endif /* OS2_VER */
        }
#endif /* EAS */

      /* skip any remaining header components that we don't grok */
    if (lseek(fhSrc, (LONG)cbHeader, SEEK_SET) == -1)
        {
        rcError = rcReadSeekError;
        goto ErrorReturn;
        }

    return(vwAlgType);

    /* errors that cause jumps to here have a trashed header (ie magic
       bytes were correct but something else was bad) or a read failure,
       but it is not an error if header is missing entirely */
ReadErrorReturn:
    if (eof(fhSrc))
        rcError = rcBadHeader;
    else
        rcError = rcReadError;

ErrorReturn:
    FFreeHeaderInfo();
    lseek(fhSrc, libSav, SEEK_SET);
    return(rcError);
}


/*
**  BOOL  FReadNBytes(int fh, BYTE * pb, int n)
**
**  Reads n bytes from file handle fh into buffer pointed to by pb.
**  Returns TRUE if read was sucessful, FALSE if not (or if EOF.)
*/
BOOL  FReadNBytes(int fh, BYTE * pb, int n)
{
    if (read(fh, pb, n) == n)
        return((BOOL)TRUE);
    else
        return(FALSE);
}


/*
**  BOOL  FReadUs(int fh, USHORT * pw)
**
**  Reads a short word from file handle fh into word pointed to by pw.
**  Returns TRUE if read was sucessful, FALSE if not (or if EOF.)
*/
BOOL  FReadUs(int fh, USHORT * pw)
{
    BYTE  rgb[2];

    if (FReadNBytes(fh, rgb, 2))
        {
        *pw = rgb[0] + (rgb[1] << 8);
        return((BOOL)TRUE);
        }
    return(FALSE);
}


/*
**  BOOL  FReadUl(int fh, ULONG * pul)
**
**  Reads a long from file handle fh into the long pointed to by pul.
**  Returns TRUE if read was sucessful, FALSE if not (or if EOF.)
*/
BOOL  FReadUl(int fh, ULONG * pul)
{
    BYTE   rgb[4];
    ULONG  ul;
    ULONG  ulShift;

    if (!FReadNBytes(fh, rgb, 4))
        return(FALSE);
    ul = rgb[0];

    ulShift = rgb[1];
    ul = (ulShift << 8) + ul;

    ulShift = rgb[2];
    ul = (ulShift << 16) + ul;

    ulShift = rgb[3];
    ul = (ulShift << 24) + ul;

    *pul = ul;
    return((BOOL)TRUE);
}
