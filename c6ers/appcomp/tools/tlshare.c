/* TS = none */
/*
**  TLSHARE.C  --  Shared code for Compression/Decompression Tools.
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "..\sutkcomp.h"
#include "strings.h"

  /* forward declarations */
void  ErrorMsgRc(SHORT rc);
void  ExitErrorMsgRc(SHORT rc);
void  MSCopyright(BOOL fCompress);
#ifdef COMPLEX
BOOL  FUserConfirmsYN(void);
void  DividePathIntoBaseExt(SZ szPath, SZ *pszBase, SZ *pszExt);
void  GetDollarSzFromSzSrc(SZ szSrcFile, SZ szDstFile);

BOOL  vfForceOverwrite;
#endif /* COMPLEX */

/*
**  void  ExitErrorMsgRc(SHORT rc)
**
**  Prints an error message that corresponds to the given error code,
**  then exits.
**  See ..\retcodes.h for a list of the error return codes used.
*/
void ExitErrorMsgRc(SHORT rc)
{
    ErrorMsgRc(rc);
    exit((SHORT)(-rc));
}


/*
**  void  ErrorMsgRc(SHORT rc)
**
**  Prints an error message that corresponds to the given error code.
**  See ..\retcodes.h for a list of the error return codes used.
*/
void ErrorMsgRc(SHORT rc)
{
    printf(szError);
    switch (rc)
        {
    default:
        printf(szInternalError);
        break;
    case rcNoHeader:
        printf(szNoHeader);
        break;
    case rcUnknownAlgType:
        printf(szUnknownAlgType);
        break;
    case rcBadHeader:
        printf(szBadHeader);
        break;
    case rcFilenamesTooLong:
        printf(szFilenameTooLong);
        break;
    case rcReadError:
        printf(szReadError);
        break;
    case rcReadSeekError:
        printf(szReadSeekError);
        break;
    case rcWriteError:
        printf(szWriteError);
        break;
    case rcWriteSeekError:
        printf(szWriteSeekError);
        break;
    case rcDestPatchError:
        printf(szDestPatchError);
        break;
    case rcCompChecksumBad:
        printf(szCompChecksumBad);
        break;
    case rcDecompChecksumBad:
        printf(szDecompChecksumBad);
        break;
    case rcCompLengthBad:
        printf(szCompLengthBad);
        break;
    case rcDecompLengthBad:
        printf(szDecompLengthBad);
        break;
    case rcGenericCompError:
        printf(szGenericCompError);
        break;
    case rcGenericDecompError:
        printf(szGenericDecompError);
        break;
    case rcSplitSizeTooSmall:
        printf(szSplitSizeTooSmall);
        break;
    case rcOutOfMemory:
        printf(szOutOfMemory);
        break;
    case rcZeckSplitFile:
        printf(szZeckSplitFile);
        break;
        }

    printf("\n");
}


#ifdef COMPLEX
/*
**  BOOL  FUserConfirmsYN(void)
**
**  Assumes the user has already been asked a question.  This proc waits
**  for the answer, makes sure it is valid, flushes excess input characters,
**  and returns TRUE if they answered in the affirmative and FALSE if they
**  answered in the negative.
*/
BOOL  FUserConfirmsYN()
{
    CHAR  chRead;
    CHAR  chFlush;

    while (TRUE)
        {
        chRead = chFlush = (CHAR)getchar();
        while (chFlush != '\n')      /* flush input so next getchar will work */
            chFlush = (CHAR)getchar();
        printf("\n");

        switch (chRead)
            {
        default:
            printf(szYorN);
            break;
        case 'Y':
        case 'y':
        case '\n':
            return((BOOL)TRUE);
        case 'N':
        case 'n':
            return(FALSE);
            }
        }
}
#endif /* COMPLEX */


#ifdef COMPLEX
void DividePathIntoBaseExt(SZ szPath, SZ * pszBase, SZ * pszExt)
{
    CHAR  *pchFind;

    *pszBase = szPath;
    *pszExt = NULL;

    pchFind = strrchr(szPath, ':');
    if (pchFind != NULL)
        *pszBase = ++pchFind;

    pchFind = strrchr(szPath, '\\');
    if (pchFind != NULL)
        *pszBase = ++pchFind;

    pchFind = strchr(*pszBase, '.');
    if (pchFind != NULL)
        *pszExt = ++pchFind;
}
#endif /* COMPLEX */


/*
**  void  MSCopyright(BOOL fCompress)
**
**  Print copyright message whenever utility is run.
*/
void MSCopyright(BOOL fCompress)
{
    if (fCompress)
        printf(szCompCopyright1);
    else
        printf(szDecompCopyright1);
    printf(szCopyright2);
    printf(szCopyright3);
}


#ifdef COMPLEX
/*
**  void  GetDollarSzFromSzSrc(SZ szSrcFile, SZ szBaseFile)
**
**  Get a suitable name for the output files from the source name,
**  using the Languages Group's way of tagging compressed files.
**         If there's no extension, make '._' the extension.
**         If there's room to append an underscore to the extension, append it.
**         Else make the last character of the extension an underscore.
*/
void GetDollarSzFromSzSrc(SZ szSrcFile, SZ szDstFile)
{
    int  cch;
    CHAR *pchExt;
    CHAR *pchBack;

    strcpy(szDstFile, szSrcFile);
    cch = strlen(szDstFile);
    pchExt  = strchr(szDstFile, '.');
    pchBack = strchr(szDstFile, '\\');

    if (pchBack != NULL && pchExt != NULL && pchBack > pchExt)
        pchExt = NULL;

    if (pchExt == NULL)
        strcat(szDstFile, "._");
    else if (strlen(pchExt) >= 4)           /* pch includes the decimal point */
        {
        szDstFile[cch - 1] = '_';
        pchExt[3] = '_';
        }
    else
        strcat(szDstFile, "_");
}
#endif /* COMPLEX */
