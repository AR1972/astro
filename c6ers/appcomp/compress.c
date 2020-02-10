/* TS = none */
/*
**  COMPRESS.C  --  general compression routines for Setup Toolkits.
*/

#include <string.h>
#include <io.h>
#include <stdio.h>

#include "sutkcomp.h"



/*
**  LONG  far  LcbCompressToFile(SHORT wAlgType, int fhSrc, int fhDest,
**                                                              LONG lcbDestMax)
**
**  Calls the appropriate compression routine.  Assumes that fhSrc points to
**  location to start reading (may not be the beginning if we are splitting).
**  If it returns successfully, fhSrc points at either EOF or the next source
**  byte to start compression at if a split occurred because lcbDestMax was
**  reached.  Returns number of bytes written to fhDest if successful, or an
**  error return code if not.  Resets fhDest to its original value when an
**  error occurs.
**
**  if lcbDestMax == NIL then no limit on output size.
*/
LONG  far  LcbCompressToFile(SHORT wAlgType, int fhSrc, int fhDest,
                                                                LONG lcbDestMax)
{
    SHORT    cbHeader;
    LONG     lcbHeader;
    LONG     lcbReturned;                     /* also used in error-exit case */
    LONG     libSrcStart;
    LONG     libDestStart;

    if (fhSrc == -1)
        return((LONG)rcReadError);

    if (vlcbSrcLength != NIL && (libSrcStart = tell(fhSrc)) == NIL)
        return((LONG)rcReadSeekError);

    if (fhDest == -1 || fhSrc == fhDest)
        return((LONG)rcWriteError);

    if ((libDestStart = tell(fhDest)) == NIL)
        return((LONG)rcWriteSeekError);

      /* set global vars for header info */
    vwAlgType = wAlgType;                        /* REVIEW maybe already done */

    cbHeader = CbWriteFileHeader(fhDest);
    if (cbHeader < rcNoError && cbHeader > -100)
        {
        lcbReturned = (LONG)cbHeader;     /* cbHeader contains the error code */
        goto LCompToFileError;
        }
	 lcbHeader = (LONG) ((USHORT) cbHeader);

      /* zero-length files */
    if (eof(fhSrc))
        return(lcbHeader);

    if (lcbDestMax != NIL && lcbHeader >= lcbDestMax)
        {
        lcbReturned = (LONG)rcSplitSizeTooSmall;
        goto LCompToFileError;
        }

    if (lcbDestMax != NIL)
        lcbDestMax -= lcbHeader;

    switch (wAlgType)
        {
    default:
        lcbReturned = (LONG)rcUnknownAlgType;
        goto LCompToFileError;

#ifdef NC_XOR1
    case wAlgTypeNoCompress:
        lcbReturned = Lcb_NC_XOR1_StraightCopy(fhSrc, fhDest, lcbDestMax, NULL,
                                                                     0L, FALSE);
        break;

    case wAlgTypeXOR1:
        lcbReturned = Lcb_NC_XOR1_StraightCopy(fhSrc, fhDest, lcbDestMax, NULL,
                                                                      0L, TRUE);
        break;
#endif /* NC_XOR1 */

#ifdef ZK1
    case wAlgTypeZK1:
        lcbReturned = Lcb_ZK1_CompressToFile(fhSrc, fhDest, lcbDestMax);
        break;
#endif /* ZK1 */

#ifdef JJJ1
    case wAlgTypeJJJ1:
        lcbReturned = Lcb_JJJ1_CompressToFile(fhSrc, fhDest, lcbDestMax);
        break;
#endif /* JJJ1 */
        }

    if (lcbReturned < rcNoError)             /* compression returned an error */
        goto LCompToFileError;

      /* patch usChecksum */
    if (vfChecksum && vlibChecksum != NIL &&
                                   !FPatchUs(fhDest, vlibChecksum, vusChecksum))
        {
        lcbReturned = (LONG)rcDestPatchError;
        goto LCompToFileError;
        }

      /* patch lcbSrcLength at location vlibSrcLength */
    if (vlcbSrcLength != NIL && vlibSrcLength != NIL && libSrcStart != NIL)
        {
        if ((vlcbSrcLength = tell(fhSrc)) == NIL)
            {
            lcbReturned = (LONG)rcReadSeekError;
            goto LCompToFileError;
            }
        vlcbSrcLength -= libSrcStart;
        if (!FPatchUl(fhDest, vlibSrcLength, (ULONG)vlcbSrcLength))
            {
            lcbReturned = (LONG)rcDestPatchError;
            goto LCompToFileError;
            }
        }

    return(lcbReturned + lcbHeader);

LCompToFileError:                       /* reset fhDest to its original value */
    lseek(fhDest, libDestStart, SEEK_SET);
    chsize(fhDest, libDestStart);
    return(lcbReturned);
}


/*
**  LONG  far  LcbCalculateCompressedLength(SHORT wAlgType, int fhSrc,
**                                                              LONG lcbDestMax)
**
**  Call the appropriate compression routine.  Assumes that fhSrc points to
**  location to start reading.  If it returns successfully, fhSrc points to
**  its original value.  Returns number of bytes that would have been written
**  if this had been a call to LcbCompressToFile() and it was successful, or
**  an error return code if not.
**
**  if lcbDestMax == NIL then no limit on output size.
*/
LONG  far  LcbCalculateCompressedLength(SHORT wAlgType, int fhSrc,
                                                                LONG lcbDestMax)
{
    SHORT   cbHeader;
    LONG    lcbHeader;
    LONG    lcbReturned;
    LONG    libSrcStart;

    if (fhSrc == -1)
        return((LONG)rcReadError);

    if ((libSrcStart = tell(fhSrc)) == NIL)
        return((LONG)rcReadSeekError);

      /* set global vars for header info */
    vwAlgType = wAlgType;                        /* REVIEW maybe already done */

    cbHeader = CbWriteFileHeader(-1);
    if (cbHeader < rcNoError && cbHeader > -100)
        return((LONG)cbHeader);
    lcbHeader = (LONG) ((USHORT) cbHeader);

    if (lcbDestMax != NIL && lcbHeader >= lcbDestMax)
        return((LONG)rcSplitSizeTooSmall);

    if (lcbDestMax != NIL)
        lcbDestMax -= lcbHeader;

    switch (wAlgType)
        {
    default:
        return((LONG)rcUnknownAlgType);

#ifdef NC_XOR1
    case wAlgTypeNoCompress:
        lcbReturned = Lcb_NC_XOR1_StraightCopy(fhSrc, -1, lcbDestMax, NULL, 0L,
                                                                         FALSE);
        break;

    case wAlgTypeXOR1:
        lcbReturned = Lcb_NC_XOR1_StraightCopy(fhSrc, -1, lcbDestMax, NULL, 0L,
                                                                          TRUE);
        break;
#endif /* NC_XOR1 */

#ifdef ZK1
    case wAlgTypeZK1:
        lcbReturned = Lcb_ZK1_CompressToFile(fhSrc, -1, lcbDestMax);
        break;
#endif /* ZK1 */

#ifdef JJJ1
    case wAlgTypeJJJ1:
        lcbReturned = Lcb_JJJ1_CompressToFile(fhSrc, -1, lcbDestMax);
        break;
#endif /* JJJ1 */
        }

    if (lcbReturned < rcNoError)             /* compression returned an error */
        return(lcbReturned);

    if (lseek(fhSrc, libSrcStart, SEEK_SET) == NIL)
        return((LONG)rcReadSeekError);

    return(lcbReturned + lcbHeader);
}
