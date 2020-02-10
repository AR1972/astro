/* TS = none */
/*
**  DECOMPTL.C  --  Decompression Tool for Setup Toolkits.
*/

#include <stdio.h>
#include <stdlib.h>
#include <io.h>
#include <string.h>
#include <fcntl.h>
#include <direct.h>
#include <ctype.h>
#include <sys\types.h>
#include <sys\stat.h>
#ifndef OS2_VER
#include <dos.h>
#endif /* !OS2_VER */
#include "..\sutkcomp.h"
#include "strings.h"


  /* forward declarations */
int   main(int argc, char ** argv);
void  Usage(SHORT wExitCode);
void  UsageMessageOnly(void);
int   FhOpenSrc(SZ szSrcPath, SZ szSrcFileName);

#ifdef COMPLEX
void  GetSzSrcFileFromUser(SZ * pszSrcFile);
void  GetSzDestFileFromUser(SZ szDestFile, SZ szSrcFile);

extern  void  DividePathIntoBaseExt(SZ szPath, SZ *pszBase, SZ *pszExt);
extern  BOOL  FUserConfirmsYN(void);
extern  void  GetDollarSzFromSzSrc(SZ szSrcFile, SZ szDstFile);

extern  BOOL  vfForceOverwrite;
#endif /* COMPLEX */

extern  void  ExitErrorMsgRc(SHORT rc);
extern  void  ErrorMsgRc(SHORT rc);
extern  void  MSCopyright(BOOL fCompress);

#ifdef ZECKFORMAT
extern  BOOL  vfZeckFormat;
#endif  /* ZECKFORMAT */

#ifdef OS2_VER
#define  ZZNAMEZZ  achName
#else  /* !OS2_VER */
#define  ZZNAMEZZ  name
#endif /* !OS2_VER */



#ifdef XL_EXT_TRANSLATE
/*
**  static  BOOL  FTranslateExcelExtension(SZ szSrcExt)
**
**  If extension is of the form "CP?", use the Excel translation table
**  to fill vszExtension (as if it had been found in the header).
*/
static  BOOL  FTranslateExcelExtension(SZ szSrcExt)
{
    if (*szSrcExt != 'C' && *szSrcExt != 'c')
        return(FALSE);
    if (*(szSrcExt + 1) != 'P' && *(szSrcExt + 1) != 'p')
        return(FALSE);

    switch (*(szSrcExt + 2))
        {
    default:
        return(FALSE);

    case '1':
        strcpy(vszExtension, "1");
        break;

    case '2':
        strcpy(vszExtension, "2");
        break;

    case '3':
        strcpy(vszExtension, "3");
        break;

    case 'c':
    case 'C':
        strcpy(vszExtension, "XLC");
        break;

    case 'e':
    case 'E':
        strcpy(vszExtension, "EXE");
        break;

    case 'f':
    case 'F':
        strcpy(vszExtension, "FON");
        break;

    case 'h':
    case 'H':
        strcpy(vszExtension, "HLP");
        break;

    case 'm':
    case 'M':
        strcpy(vszExtension, "XLM");
        break;

    case 's':
    case 'S':
        strcpy(vszExtension, "XLS");
        break;

    case 't':
    case 'T':
        strcpy(vszExtension, "CBT");
        break;

    case 'v':
    case 'V':
        strcpy(vszExtension, "DRV");
        break;

    case 'w':
    case 'W':
        strcpy(vszExtension, "XLW");
        break;

    case 'y':
    case 'Y':
        strcpy(vszExtension, "SYS");
        break;
        }

    return(TRUE);
}
#endif


int  FhOpenSrc(SZ szSrcPath, SZ szSrcFileName)
{
    CHAR   rgchSrcFile[_MAX_PATH + _MAX_FNAME + _MAX_EXT + 1];
    CHAR * pch;

    strcpy(rgchSrcFile, szSrcPath);

    if ((pch = strrchr(rgchSrcFile, '\\')) == NULL)
        {
        if (rgchSrcFile[1] == ':')
            rgchSrcFile[2] = '\0';
        else
            rgchSrcFile[0] = '\0';
        }
    else
        *(pch + 1) = '\0';

    strcat(rgchSrcFile, szSrcFileName);
    strcpy(szSrcPath, rgchSrcFile);

    return(open(rgchSrcFile, O_RDONLY | O_BINARY));
}


#ifdef COMPLEX
void  GetSzSrcFileFromUser(SZ * pszSrcFile)
{
    if ((*pszSrcFile = (SZ)malloc(80 * sizeof(CHAR))) == NULL)
        ExitErrorMsgRc(rcOutOfMemory);

LAskForSrcName:
    printf(szEnterSrcName);
    if (gets(*pszSrcFile) == NULL)
        {
        printf(szGetsFailure);
        exit(26);
        }

    *(*pszSrcFile + 79) = '\0';
    while (strlen(*pszSrcFile) && isspace(**pszSrcFile))
        memmove(*pszSrcFile, *pszSrcFile + 1, 79);

    if (!strlen(*pszSrcFile))
        goto LAskForSrcName;
}
#endif /* COMPLEX */


#ifdef COMPLEX
void  GetSzDestFileFromUser(SZ szDestFile, SZ szSrcFile)
{
    while (1)
        {
        printf(szEnterDestName);
        if (gets(szDestFile) == NULL)
            {
            printf(szGetsFailure);
            exit(26);
            }

        *(szDestFile + 79) = '\0';
        while (strlen(szDestFile) && isspace(*szDestFile))
            memmove(szDestFile, szDestFile + 1, 79);

        if (stricmp(szSrcFile, szDestFile))
            return;

        printf(szSrcDestDifferent);
        }
}
#endif /* COMPLEX */


/*
**  int  main(int argc, char * argv[])
**
**  Main decompress routine.
*/
int main(int argc, char * argv[])
{
    int     iArg;
    SZ      szSrcFile = NULL;
    BOOL    fForceAppend = FALSE;
    BOOL    fComputeOnly = FALSE;
    BOOL    fFileFound;
#ifdef COMPLEX
    SZ      szSrcBase;
    SZ      szSrcExt;
    SHORT   cch;
    struct  stat  statBuf;
#endif /* COMPLEX */
#ifdef DOLLAR_SUFFIX
    CHAR    chLast;
#endif /* DOLLAR_SUFFIX */
    int     fhDest;
    int     fhSrc;
    CHAR    rgchSrcFile[_MAX_PATH + _MAX_FNAME + _MAX_EXT + 1];
    CHAR    rgchDestFile[_MAX_PATH + _MAX_FNAME + _MAX_EXT + 1];
    CHAR    rgchDestFileSav[_MAX_PATH + _MAX_FNAME + _MAX_EXT + 1];
    LONG    lcbDecompressed;
    SHORT   wReturn;
    SHORT   wExitCode = 0;

#ifdef OS2_VER
    HDIR    hdir;
    USHORT  usSearchCount;
    FILEFINDBUF     ffindBuf;
    FILESTATUS      fsSrc;
#else  /* !OS2_VER */
    struct  find_t  ffindBuf;
    USHORT  usDate;
    USHORT  usTime;
#endif /* !OS2_VER */

#ifdef DOLLAR_SUFFIX
    BOOL    fUsingDollarSignedSrc = FALSE;
#endif /* DOLLAR_SUFFIX */

#ifdef COMPLEX
    vfForceOverwrite = FALSE;
#endif /* COMPLEX */

    MSCopyright(FALSE);

    for (iArg = 1; iArg < argc
            && (argv[iArg][0] == '-' || argv[iArg][0] == '/'); iArg++)
        {
        switch (argv[iArg][1])
            {
        default:
            Usage(20);

        case 'a':
        case 'A':
            fForceAppend = (BOOL)TRUE;
            break;

#ifdef COMPLEX
        case 'f':
        case 'F':
            vfForceOverwrite = (BOOL)TRUE;
            break;
#endif /* COMPLEX */

        case '?':
        case 'h':
        case 'H':
            Usage(0);

#ifdef COMPLEX
        case 'q':
        case 'Q':
            fComputeOnly = (BOOL)TRUE;
            break;
#endif /* COMPLEX */
            }
        }

#ifdef COMPLEX
      /* there can be at most two filenames after flags */
    if (iArg < argc - 2)
#else  /* !COMPLEX */
      /* there must be two filenames after flags */
    if (iArg != argc - 2)
#endif /* !COMPLEX */
        Usage(19);

    if (iArg < argc)
        {
        szSrcFile = argv[iArg++];
        if (!strcmp(szSrcFile, "?"))
            Usage(0);
        }

    if (iArg < argc)
        strcpy(rgchDestFile, argv[iArg]);
#ifdef COMPLEX
    else
        strcpy(rgchDestFile, "");

    if (szSrcFile == NULL)
        {
        UsageMessageOnly();
        GetSzSrcFileFromUser(&szSrcFile);
        if (!fComputeOnly)
            GetSzDestFileFromUser(rgchDestFile, szSrcFile);
        }
#endif /* COMPLEX */

    strcpy(rgchSrcFile, szSrcFile);

    strcpy(rgchDestFileSav, rgchDestFile);
#ifdef COMPLEX
      /* it may LOOK like a filename, but add a slash if it's really a dir */
    if ((cch = strlen(rgchDestFile)) != 0 && rgchDestFile[cch - 1] != '\\'
            && (cch != 2 || rgchDestFile[1] != ':') && !access(rgchDestFile, 00)
            && !stat(rgchDestFile, &statBuf) && (statBuf.st_mode & S_IFDIR))
        strcat(rgchDestFileSav, "\\");
#endif /* COMPLEX */

#ifdef DOLLAR_SUFFIX
    chLast = *(rgchSrcFile + strlen(rgchSrcFile) - 1);
    if (chLast == '_' || chLast == '$' || chLast == '*' || chLast == '?')
        fUsingDollarSignedSrc = (BOOL)TRUE;
#endif /* DOLLAR_SUFFIX */

#ifdef OS2_VER
    usSearchCount = 1;
    hdir = HDIR_CREATE;
    fFileFound = (BOOL)(!DosFindFirst(rgchSrcFile, &hdir,
            FILE_NORMAL | FILE_READONLY, &ffindBuf, sizeof(FILEFINDBUF),
            &usSearchCount, 0L));
#else  /* !OS2_VER */
    fFileFound = (BOOL)(!_dos_findfirst(rgchSrcFile, _A_NORMAL | _A_RDONLY,
            &ffindBuf));
#endif /* !OS2_VER */

    if (!fFileFound)
        {
#ifdef DOLLAR_SUFFIX
        fUsingDollarSignedSrc = (BOOL)TRUE;
        GetDollarSzFromSzSrc(szSrcFile, rgchSrcFile);

LTryFindFirstFile:

#ifdef OS2_VER
        DosFindClose(hdir);
        usSearchCount = 1;
        hdir = HDIR_CREATE;
        fFileFound = (BOOL)(!DosFindFirst(rgchSrcFile, &hdir,
                FILE_NORMAL | FILE_READONLY, &ffindBuf, sizeof(FILEFINDBUF),
                &usSearchCount, 0L));
#else  /* !OS2_VER */
        fFileFound = (BOOL)(!_dos_findfirst(rgchSrcFile, _A_NORMAL | _A_RDONLY,
                &ffindBuf));
#endif /* !OS2_VER */

        if (!fFileFound)
            if (*(rgchSrcFile + strlen(rgchSrcFile) - 1) == '_')
                {
                *(rgchSrcFile + strlen(rgchSrcFile) - 1) = '$';
                goto LTryFindFirstFile;
                }
            else
#endif /* DOLLAR_SUFFIX */
                {
                printf(szNoSrcFile);
                exit(21);
                }
        }

#ifdef COMPLEX
    while (fFileFound)
#endif /* !COMPLEX */
        {
        BOOL  fAppend = fForceAppend;

        if ((fhSrc = FhOpenSrc(rgchSrcFile, ffindBuf.ZZNAMEZZ)) == -1)
            {
            printf(szCantOpenInput, rgchSrcFile);
            goto LFindNextFile;
            }

        printf("%s\n", rgchSrcFile);

        if ((wReturn = WReadHeaderInfo(fhSrc)) < rcNoError)
            {
            ErrorMsgRc(wReturn);
            wExitCode = -wReturn;
            goto LFindNextFile;
            }

#ifdef COMPLEX
        if (fComputeOnly)
            {
            lcbDecompressed = LcbCalculateDecompressedLength(fhSrc, (BOOL)TRUE);
            close(fhSrc);
            if (lcbDecompressed < (LONG)rcNoError)
                {
                ErrorMsgRc((SHORT)lcbDecompressed);
                wExitCode = (SHORT)(-lcbDecompressed);
                }
            else
                printf(szDecompSize, lcbDecompressed);
            goto LFindNextFile;
            }
#endif /* COMPLEX */

        strcpy(rgchDestFile, rgchDestFileSav);
#ifdef COMPLEX
        cch = strlen(rgchDestFile);
        if (!cch || rgchDestFile[cch-1] == '\\' || rgchDestFile[cch-1] == ':')
            {
            DividePathIntoBaseExt(ffindBuf.ZZNAMEZZ, &szSrcBase, &szSrcExt);

#ifdef XL_EXT_TRANSLATE
            if (vszExtension[0] == '\0')
                FTranslateExcelExtension(szSrcExt);
#endif

            if (szSrcExt != NULL)
                *(szSrcExt - 1) = '\0';

            if (vszBaseName[0] != '\0')
                strcat(rgchDestFile, vszBaseName);
            else
                strcat(rgchDestFile, szSrcBase);

            if (vszExtension[0] != '\0')
                {
                strcat(rgchDestFile, ".");
                if (*vszExtension != ' ')
                    strcat(rgchDestFile, vszExtension);
                }
            else if (szSrcExt != NULL)
                {
                strcat(rgchDestFile, ".");
                strcat(rgchDestFile, szSrcExt);
                }

            if (szSrcExt != NULL)
                *(szSrcExt - 1) = '.';
            }
#endif /* COMPLEX */

        if (!fForceAppend && !access(rgchDestFile, 00))
            {
#ifdef COMPLEX
            if (!vfForceOverwrite)
                {
                printf(szFileExists, rgchDestFile);

                printf(szAppendQ);
                if ((fAppend = FUserConfirmsYN()))
                    goto LAppendToExistingFile;

                printf(szOverwriteYN);
                }
            if (vfForceOverwrite || FUserConfirmsYN())
                {
#endif /* COMPLEX */
                chmod(rgchDestFile, S_IREAD | S_IWRITE);
                remove(rgchDestFile);
                if (!access(rgchDestFile, 00))
                    {
                    printf(szCantOverwrite, rgchDestFile);
                    close(fhSrc);
                    goto LFindNextFile;
                    }
#ifdef COMPLEX
                }
            else
                {
                close(fhSrc);
                goto LFindNextFile;
                }
#endif /* COMPLEX */
            }

#ifdef COMPLEX
LAppendToExistingFile:
#endif /* COMPLEX */
        if (fAppend)
            {
            fhDest = open(rgchDestFile, O_APPEND | O_BINARY | O_CREAT |O_WRONLY,
                    S_IREAD | S_IWRITE);
            if (lseek(fhDest, 0L, 2) == -1L)
                {
                close(fhSrc);
                close(fhDest);
                ErrorMsgRc(rcWriteSeekError);
                wExitCode = -rcWriteSeekError;
                goto LFindNextFile;
                }
            }
        else
            fhDest = open(rgchDestFile, O_TRUNC | O_BINARY | O_CREAT | O_WRONLY,
                    S_IREAD | S_IWRITE);

        if (fhDest == -1)
            {
            close(fhSrc);
            printf(szCantOpenOutput, rgchDestFile);
            goto LFindNextFile;
            }

#ifdef OS2_VER
        if (DosQFileInfo((HFILE)fhSrc, (USHORT)0x0001, &fsSrc,
                (USHORT)(sizeof(fsSrc))))
#else /* not OS2_VER */
        if (_dos_getftime(fhSrc, &usDate, &usTime))
#endif /* OS2_VER */
            {
            ErrorMsgRc(rcReadError);
            wExitCode = -rcReadError;
            goto LFindNextFile;
            }

        lcbDecompressed = LcbDecompressToFile(fhSrc, fhDest, NIL,0L,(BOOL)TRUE);

#ifdef OS2_VER
        if (DosSetFileInfo((HFILE)fhDest, (USHORT)0x0001, &fsSrc,
                (USHORT)(sizeof(fsSrc))))
#else /* not OS2_VER */
        if (_dos_setftime(fhDest, usDate, usTime))
#endif /* OS2_VER */
            {
            ErrorMsgRc(rcWriteError);
            wExitCode = -rcWriteError;
            goto LFindNextFile;
            }

        close(fhSrc);
        close(fhDest);

        if (lcbDecompressed < rcNoError)
            {
#ifdef ZECKFORMAT
            if (vfZeckFormat && lcbDecompressed == rcDecompLengthBad)
                ExitErrorMsgRc(rcZeckSplitFile);
#endif /* ZECKFORMAT */
            ErrorMsgRc((SHORT)lcbDecompressed);
            wExitCode = (SHORT)(-lcbDecompressed);
            goto LFindNextFile;
            }
        else
            printf(szWroteBytes, lcbDecompressed, rgchDestFile);

LFindNextFile:

#ifdef COMPLEX
#ifdef OS2_VER
        usSearchCount = 1;
        fFileFound = (BOOL)(!DosFindNext(hdir, &ffindBuf, sizeof(FILEFINDBUF),
                &usSearchCount));
#else  /* !OS2_VER */
        fFileFound = (BOOL)(!_dos_findnext(&ffindBuf));
#endif /* !OS2_VER */

#ifdef DOLLAR_SUFFIX
        if (!fFileFound && !fUsingDollarSignedSrc)
            {
            fUsingDollarSignedSrc = (BOOL)TRUE;
            GetDollarSzFromSzSrc(szSrcFile, rgchSrcFile);

LTryFindNextFile:
#ifdef OS2_VER
            DosFindClose(hdir);
            usSearchCount = 1;
            hdir = HDIR_CREATE;
            fFileFound = (BOOL)(!DosFindFirst(rgchSrcFile, &hdir,
                    FILE_NORMAL | FILE_READONLY, &ffindBuf, sizeof(FILEFINDBUF),
                    &usSearchCount, 0L));
#else  /* !OS2_VER */
            fFileFound = (BOOL)(!_dos_findfirst(rgchSrcFile,
                    _A_NORMAL | _A_RDONLY, &ffindBuf));
#endif /* !OS2_VER */
            if (!fFileFound && *(rgchSrcFile + strlen(rgchSrcFile) - 1) == '_')
                {
                *(rgchSrcFile + strlen(rgchSrcFile) - 1) = '$';
                goto LTryFindNextFile;
                }
            }
#endif /* DOLLAR_SUFFIX */
#else  /* !COMPLEX */
            ;
#endif /* !COMPLEX */
        } /* while (!fFileFound) */

#ifdef OS2_VER
    DosFindClose(hdir);
#endif /* OS2_VER */

    exit(wExitCode);
    return(wExitCode);
}


/*
**  void  UsageMessageOnly(void)
**
**  Print usage message when command line args are bad.
*/
void UsageMessageOnly(void)
{
    printf(szDecompUsage1);
    printf(szDecompUsage2);
#ifdef COMPLEX
    printf(szDecompUsage3);
    printf(szDecompUsage4);
    printf(szDecompUsage5);

    printf(szDecompUsage6);
    printf(szDecompUsage7);
    printf(szDecompUsage8);
    printf(szDecompUsage9);
    printf(szDecompUsage10);
#endif /* COMPLEX */
}


/*
**  void  Usage(SHORT wExitCode)
**
**  Print usage message when command line args are bad then exit.
*/
void Usage(SHORT wExitCode)
{
    UsageMessageOnly();
    exit(wExitCode);
}
