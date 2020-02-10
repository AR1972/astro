/* TS = none */
/*
**  DECOMP.C  --  general decompression routines for Setup Toolkits.
*/

#include <io.h>
#include <stdio.h>
#ifdef EAS
#include <malloc.h>
#endif /* EAS */
#include "sutkcomp.h"

extern PFNWFROMW vpfn;
extern PFNWFROMW vpfnYield;
extern int       vcTicks;
extern LONG      vcbPerTick;
extern LONG      vcbCur;


/*
**  LONG  far  LcbDecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
**                                       LONG libStart, BOOL fHeaderAlreadyRead)
**
**  If fHeaderAlreadyRead is TRUE, assumes the header has already been read
**  (and global variables have been set), otherwise it reads the header.
**  Then calls the appropriate decompress routine.  The routine starts
**  decompression at fhSrc's current position, and writes starting at fhDest's
**  current position (either of which may not be the beginning of the file.)
**  If vlcbSrcLength has been set to a size other than NIL, this procedure
**  checks the decompressed file's length and returns an error if mismatched.
**  Returns number of bytes written to fhDest, or an error return code.
*/
LONG  far  LcbDecompressToFile(int fhSrc, int fhDest, LONG lcbDestMax,
                                         LONG libStart, BOOL fHeaderAlreadyRead)
{
    return(LcbDecompFile(fhSrc, fhDest, lcbDestMax, libStart,
            fHeaderAlreadyRead, NULL, 0L, NULL, 0, NULL));
}



/*
**  LONG  far  LcbDecompFile(int fhSrc, int fhDest, LONG lcbDestMax,
**                 LONG libStart, BOOL fHeaderAlreadyRead, BYTE far * fpbBuf,
**                 LONG lcbBuf, PFNWFROMW pfn, int cProgTicks,
**                 PFNWFROMW pfnYield)
**
**  If fHeaderAlreadyRead is TRUE, assumes the header has already been read
**  (and global variables have been set), otherwise it reads the header.
**  Then calls the appropriate decompress routine.  The routine starts
**  decompression at fhSrc's current position, and writes starting at fhDest's
**  current position (either of which may not be the beginning of the file.)
**  If vlcbSrcLength has been set to a size other than NIL, this procedure
**  checks the decompressed file's length and returns an error if mismatched.
**  Returns number of bytes written to fhDest, or an error return code.
*/
LONG  far  LcbDecompFile(int fhSrc, int fhDest, LONG lcbDestMax,
               LONG libStart, BOOL fHeaderAlreadyRead, BYTE far * fpbBuf,
               LONG lcbBuf, PFNWFROMW pfn, int cProgTicks, PFNWFROMW pfnYield)
{
    SHORT   wAlgType;
    LONG    lcbReturn;
    LONG    libDestStart;
    long    lfaCur, lfaEnd;

    if (fhSrc == -1)
        return((LONG)rcReadError);

    if (fhDest == -1 || fhSrc == fhDest)
        return((LONG)rcWriteError);

    if ((libDestStart = tell(fhDest)) == NIL)
        return((LONG)rcWriteSeekError);

    if (!fHeaderAlreadyRead)
        if ((wAlgType = WReadHeaderInfo(fhSrc)) < rcNoError)
            return((LONG)wAlgType);

    if (pfn == NULL || cProgTicks < 0)
        cProgTicks = 0;
    if (cProgTicks == 0)
        pfn = NULL;

      /* zero-length files */
    if (eof(fhSrc))
        {
        if (pfn != NULL)
            (*pfn)(cProgTicks);
        return(0L);
        }

#ifdef EAS
#ifdef OS2_VER
    if (vfrgbEAs && vuscbEAs)  // Need to dump out EA stuff...
        {
        EAOP eaop;

        eaop.fpFEAList = (PFEALIST) vfrgbEAs;
        if (DosSetFileInfo(fhDest, 2, (PBYTE)&eaop, sizeof(EAOP)))
            {
            }
        }
    if (vfrgbEAs)
        {
        _ffree(vfrgbEAs);
        vfrgbEAs = NULL;
        }
    vuscbEAs = 0;
#endif /* OS2_VER */
#endif /* EAS */

    if ((lfaCur = lseek(fhSrc, 0L, SEEK_CUR)) == -1L ||
            (lfaEnd = lseek(fhSrc, 0L, SEEK_END)) == -1L ||
            lseek(fhSrc, lfaCur, SEEK_SET) != lfaCur ||
            lfaCur > lfaEnd)
        return((LONG)rcReadSeekError);

	vpfnYield = pfnYield;
    if ((vpfn = pfn) != NULL)
        {
        vcTicks    = cProgTicks;
        vcbPerTick = ((lfaEnd - lfaCur) / cProgTicks) + 1;
        }
    vcbCur     = 0L;

    switch (vwAlgType)
        {
    default:
        lcbReturn = (LONG)rcUnknownAlgType;
        break;

#ifdef NC_XOR1
    case wAlgTypeNoCompress:
        lcbReturn = Lcb_NC_XOR1_StraightCopy(fhSrc, fhDest, lcbDestMax, NULL,
                                                               libStart, FALSE);
        break;

    case wAlgTypeXOR1:
        lcbReturn = Lcb_NC_XOR1_StraightCopy(fhSrc, fhDest, lcbDestMax, NULL,
                                                                libStart, TRUE);
        break;
#endif

#ifdef ZK1
    case wAlgTypeZK1:
#if 1  /* REVIEW eventually remove */
        if (lcbDestMax != NIL)
            lcbReturn = rcGenericDecompError;
        else
#endif
        lcbReturn = Lcb_ZK1_DecompressToFile(fhSrc, fhDest, lcbDestMax,
                                                                      libStart);
        break;
#endif

#ifdef JJJ1
    case wAlgTypeJJJ1:
#if 1  /* REVIEW eventually remove */
        if (lcbDestMax != NIL)
            lcbReturn = rcGenericDecompError;
        else
#endif
        lcbReturn = Lcb_JJJ1_DecompressToFile(fhSrc, fhDest, lcbDestMax,
                                                      libStart, fpbBuf, lcbBuf);
        break;
#endif
        }
    
    /* check that source length matches */
    if (lcbReturn > rcNoError && vlcbSrcLength != NIL &&
                                  (lcbDestMax == NIL || lcbReturn < lcbDestMax))
        {
        if (lcbReturn != vlcbSrcLength)
            lcbReturn = (LONG)rcCompLengthBad;
        }

    if (lcbReturn < rcNoError)
        {
        lseek(fhDest, libDestStart, SEEK_SET);
        chsize(fhDest, libDestStart);
        }

    if (vcTicks > 0 && vpfn != NULL)
        (*vpfn)(vcTicks);
    vpfn       = NULL;
	vpfnYield  = NULL;
    vcTicks    = 0;
    vcbPerTick = 0L;
    vcbCur     = 0L;

    return(lcbReturn);
}



#ifdef COMPLEX
/*
**  LONG  far  LcbDecompressToBuffer(int fhSrc, BYTE far * fpbBuf, LONG lcbBuf,
**                                       LONG libStart, BOOL fHeaderAlreadyRead)
**
**  If fHeaderAlreadyRead = FALSE, reads the header contents; otherwise assumes
**  the header has been read into the global variables.  Calls the appropriate
**  decompression routine, which decompresses into buffer fpbBuf at most
**  lcbBuf bytes, starting the buffer filling at the offset libStart bytes
**  from the current position in fhSrc.  (It is an error for fpbBuf == NULL.
**  If lcbBuf == NIL, nothing is read into the buffer.)
**  Returns number of bytes actually uncompressed or an error return code.
*/
LONG  far  LcbDecompressToBuffer(int fhSrc, BYTE far * fpbBuf, LONG lcbBuf,
                                         LONG libStart, BOOL fHeaderAlreadyRead)
{
    SHORT  wAlgType;
    
    if (fhSrc == -1)
        return((LONG)rcReadError);

    if (lcbBuf == NIL)
        return(0L);

    if (fpbBuf == NULL)
        return((LONG)rcWriteError);

    if (!fHeaderAlreadyRead)
        if ((wAlgType = WReadHeaderInfo(fhSrc)) < rcNoError)
            return((LONG)wAlgType);


    switch (vwAlgType)
        {
    default:                            /* unrecognized compression algorithm */
        return((LONG)rcUnknownAlgType);

#ifdef NC_XOR1
    case wAlgTypeNoCompress:
        return(Lcb_NC_XOR1_StraightCopy(fhSrc, -1, lcbBuf, fpbBuf, libStart,
                                                                        FALSE));

    case wAlgTypeXOR1:
        return(Lcb_NC_XOR1_StraightCopy(fhSrc, -1, lcbBuf, fpbBuf, libStart,
                                                                         TRUE));
#endif

#ifdef ZK1
    case wAlgTypeZK1:
        return(Lcb_ZK1_DecompressToBuffer(fhSrc, fpbBuf, lcbBuf, libStart));
#endif

#ifdef JJJ1
    case wAlgTypeJJJ1:
        return(Lcb_JJJ1_DecompressToBuffer(fhSrc, fpbBuf, lcbBuf, libStart));
#endif
        }
}
#endif /* COMPLEX */



#ifdef COMPLEX
/*
**  LONG  far  LcbCalculateDecompressedLength(int fhSrc,
**                                                      BOOL fHeaderAlreadyRead)
**
**  If fHeaderAlreadyRead == TRUE, assumes the header contents are already
**  available in the global variables; otherwise it reads the header.
**  Calculates the decompressed length of the file but doesn't actually
**  create the output file.  Assumes that fhSrc is set to the beginning of 
**  the file, or right past the header if fHeaderAlreadyRead is TRUE.  If 
**  successful, it resets fhSrc to its original value.
**  Returns length of decompressed file or an error return code.
*/
LONG  far  LcbCalculateDecompressedLength(int fhSrc, BOOL fHeaderAlreadyRead)
{
    SHORT  wAlgType;
    LONG   lcbReturn;
    LONG   libSrcStart;

    if (fhSrc == -1)
        return((LONG)rcReadError);

    if ((libSrcStart = tell(fhSrc)) == NIL)
        return((LONG)rcReadSeekError);

    if (!fHeaderAlreadyRead)
        if ((wAlgType = WReadHeaderInfo(fhSrc)) < rcNoError)
            return((LONG)wAlgType);

    if (vlcbSrcLength != NIL)
        {
        lseek(fhSrc, libSrcStart, SEEK_SET);
        return(vlcbSrcLength);
        }

    switch (vwAlgType)
        {
    default:
        return((LONG)rcUnknownAlgType);

#ifdef NC_XOR1
    case wAlgTypeNoCompress:
    case wAlgTypeXOR1:
        lcbReturn = Lcb_NC_XOR1_StraightCopy(fhSrc, -1, -1L, NULL, 0L, FALSE);
#endif

#ifdef ZK1
    case wAlgTypeZK1:
        lcbReturn = Lcb_ZK1_DecompressToFile(fhSrc, -1, NIL, 0L);
#endif

#ifdef JJJ1
    case wAlgTypeJJJ1:
        lcbReturn = Lcb_JJJ1_DecompressToFile(fhSrc, -1, NIL, 0L, NULL, 0L);
#endif
        }

    if (lcbReturn >= rcNoError)
        lseek(fhSrc, libSrcStart, SEEK_SET);

    return(lcbReturn);
}
#endif /* COMPLEX */
