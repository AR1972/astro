/* TS = none */
/*
**  COMPTOOL.C  --  Compression Tool for Setup Toolkits.
*/


#include <stdio.h>
#include <stdlib.h>
#ifdef EAS
#include <malloc.h>
#endif /* EAS */
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
int           main(int argc, char ** argv);
void          MakeDirs(SZ rgchDestArg);
void          CopyAndFixSzDir(SZ rgchArg, SZ szArg);
BOOL          FWildcardPath(SZ rgchSrcArg);
BOOL          FDirectory(SZ rgchSrcArg);
void          RecurseOnDir(SZ rgchSrcDir, SZ rgchDestDir);
void          HandleDirFiles(SZ rgchSrcArg, SZ rgchDestDir);
void          HandleOneFile(SZ rgchSrcFile, SZ rgchDestArg);
void          Usage(SHORT wExitCode);
BOOL          FEnsureSzBaseDoesNotExist(SZ szBaseFile);
BOOL          FIncrementSzBase(SZ szBaseFile);
#ifdef EAS
#ifdef OS2_VER
BOOL QueryEAs(CHAR * pszPath, CHAR ** pEABuf);
#endif /* !OS2_VER */
#endif /* EAS */

extern  void  ExitErrorMsgRc(SHORT rc);
extern  BOOL  FUserConfirmsYN(void);
extern  void  DividePathIntoBaseExt(SZ szPath, SZ *pszBase, SZ *pszExt);
extern  void  MSCopyright(BOOL fCompress);
extern  void  GetDollarSzFromSzSrc(SZ szSrcFile, SZ szDstFile);

extern  BOOL  vfForceOverwrite;

BOOL          fDollarSuffix = (BOOL)TRUE;

#ifdef JJJ1
#define  wAlgTypeDefault  3
#else

#ifdef ZK1
#define  wAlgTypeDefault  2
#else

#ifdef NC_XOR1
#define  wAlgTypeDefault  1
#else

Force_a_compile_error__need_to_define_at_least_one_alg_type

#endif /* NC_XOR1 */
#endif /* ZK1 */
#endif /* JJJ1 */

struct  _SDL
    {
    SZ             szSubdir;
    struct _SDL *  psdlNext;
    };
typedef  struct _SDL  SDL;


  /* have our own globals because WReadHeaderInfo resets its globals */
USHORT  wAlgType = wAlgTypeDefault;
BOOL	fIncludeSrcLength = TRUE;
BOOL    fIncludeChecksum = FALSE;
BOOL    fIncludeBase = FALSE;
BOOL    fIncludeExt = FALSE;
#ifdef EAS
BOOL    fIncludeEAs = TRUE;
#endif /* EAS */

USHORT  cbText = 0;
SZ      szText = NULL;

BOOL    fComputeOnly = FALSE;
BOOL    fOnePieceOnly = FALSE;
LONG    lcbSize = NIL;

int     cFiles = 0;
LONG    lcbSrcTotal = 0L;
LONG    lcbDestTotal = 0L;


/*
**  int  main(int argc, char * argv[])
**
**  Main compress routine.
*/
int main(int argc, char * argv[])
{
    int     iArg;
    CHAR    rgchSrcArg[_MAX_PATH + _MAX_FNAME + _MAX_EXT + 1];
    CHAR    rgchDestArg[_MAX_PATH + _MAX_FNAME + _MAX_EXT + 1];

    MSCopyright((BOOL)TRUE);

    vfForceOverwrite = FALSE;

    if (argc < 2)
        Usage(19);

      /* Parse command line arguments */
    for (iArg = 1; iArg < argc
            && (argv[iArg][0] == '-' || argv[iArg][0] == '/'); iArg++)
        {
        switch (argv[iArg][1])
            {
        default:
            Usage(20);

        case 'a':
        case 'A':
            wAlgType = atoi(&(argv[iArg][2]));
            break;

        case 'b':
        case 'B':
            fIncludeBase = (BOOL)TRUE;
            break;

        case 'c':
        case 'C':
            /* REVIEW */
#if 1
            printf(szNoChecksum);
#else
            fIncludeChecksum = (BOOL)TRUE;
#endif
            break;

        case 'e':
        case 'E':
            fIncludeExt = (BOOL)TRUE;
            break;

        case 'f':
        case 'F':
            vfForceOverwrite = (BOOL)TRUE;
            break;

        case '?':
        case 'h':
        case 'H':
            Usage(0);

        case 'l':
        case 'L':
	    fIncludeSrcLength = (BOOL)FALSE;
            break;

        case 'q':
        case 'Q':
            fComputeOnly = (BOOL)TRUE;
            break;

        case 's':
        case 'S':
            lcbSize = atol(argv[iArg] + 2);
            lcbSize *= (ULONG)512;       /* user enters 512 bytes */
            break;

        case 't':
        case 'T':
            cbText = strlen(argv[iArg]) - 2;                     /* skip flag */
            szText = argv[iArg] + 2;
            break;

#ifdef EAS
        case 'x':
        case 'X':
            fIncludeEAs = FALSE;
            break;
#endif /* EAS */

        case 'z':
        case 'Z':
            fOnePieceOnly = (BOOL)TRUE;
            lcbSize = atol(argv[iArg] + 2);
            lcbSize *= (ULONG)512;       /* user enters 512 bytes */
            break;

        case '$':
            fDollarSuffix = FALSE;
            break;
            }
        }

    if (lcbSize != NIL && fComputeOnly)
        printf(szNoQWithS);

      /* there needs to be at least one and at most two filenames after flags */
    if (iArg != argc - 1 && iArg != argc - 2)
        Usage(19);

    CopyAndFixSzDir(rgchSrcArg, argv[iArg++]);

    if (iArg < argc)
        CopyAndFixSzDir(rgchDestArg, argv[iArg]);
    else
        rgchDestArg[0] = '\0';

      /* info is passed as global vars */
    if (FWildcardPath(rgchSrcArg))
        {
        if (!fComputeOnly)
            MakeDirs(rgchDestArg);
        if (fComputeOnly || FDirectory(rgchDestArg))
            HandleDirFiles(rgchSrcArg, rgchDestArg);
        else
            printf(szDestDirWild);
        }
    else if (FDirectory(rgchSrcArg))
        {
        if (!fComputeOnly)
            MakeDirs(rgchDestArg);
        if (fComputeOnly || FDirectory(rgchDestArg))
            RecurseOnDir(rgchSrcArg, rgchDestArg);
        else
            printf(szDestDirTree);
        }
    else
        HandleOneFile(rgchSrcArg, rgchDestArg);

    if (cFiles > 1)
        {
        printf(szTotals);
        printf(szBytesCompressed, lcbSrcTotal, lcbDestTotal);
        printf(szPercentSavings, 
                ((200 * (lcbSrcTotal - lcbDestTotal) / lcbSrcTotal) + 1 ) / 2);
        }

    exit(0);
    return(0);
}


void  MakeDirs(SZ rgchDestArg)
{
    int  ichMac = strlen(rgchDestArg) - 1;
    SZ   pchSlash;

    if (ichMac >= 0 && rgchDestArg[ichMac] != '\\'
            && rgchDestArg[ichMac] != ':')
        strcat(rgchDestArg, "\\");

    pchSlash = strchr(rgchDestArg + 1, '\\');
    while (pchSlash != NULL)
        {
        *pchSlash = '\0';

        if (!fComputeOnly)
            mkdir(rgchDestArg);

        *pchSlash = '\\';
        pchSlash = strchr(pchSlash + 1, '\\');
        }
}


void  CopyAndFixSzDir(SZ rgchArg, SZ szArg)
{
    if (szArg == NULL || *szArg == '\0')
        rgchArg[0] = '\0';
    else
        {
        int   ichMac = strlen(szArg) - 1;

        strcpy(rgchArg, szArg);
        if (FDirectory(rgchArg) && rgchArg[ichMac] != '\\'
                && rgchArg[ichMac] != ':')
            strcat(rgchArg, "\\");
        }
}


BOOL  FWildcardPath(SZ rgchSrcArg)
{
    if (strcspn(rgchSrcArg, "?*") != strlen(rgchSrcArg))
        return((BOOL)TRUE);
    return(FALSE);
}


BOOL  FDirectory(SZ rgchSrcArg)
{
    int     ichMac;
    struct  stat  statBuf;

    if (rgchSrcArg == NULL || *rgchSrcArg == '\0')
        return((BOOL)TRUE);

    ichMac = strlen(rgchSrcArg) - 1;
    if (*(rgchSrcArg + ichMac) == '\\' || *(rgchSrcArg + ichMac) == ':')
        return((BOOL)TRUE);
    if (!stat(rgchSrcArg, &statBuf) && (statBuf.st_mode & S_IFDIR))
        return((BOOL)TRUE);
    return(FALSE);
}


#ifdef OS2_VER
#define  ZZNAMEZZ  achName
#else  /* !OS2_VER */
#define  ZZNAMEZZ  name
#endif /* !OS2_VER */


void  RecurseOnDir(SZ rgchSrcDir, SZ rgchDestDir)
{
    int     cchSrcSav = strlen(rgchSrcDir);
    int     cchDestSav = strlen(rgchDestDir);
    BOOL    fDirFound;
    SDL *   psdlHead = NULL;
#ifdef OS2_VER
    HDIR    hdir;
    USHORT  usSearchCount;
    FILEFINDBUF     ffindBuf;
#else  /* !OS2_VER */
    struct  find_t  ffindBuf;
#endif /* !OS2_VER */

    strcat(rgchSrcDir, "*.*");
#ifdef OS2_VER
    hdir = HDIR_CREATE;
    usSearchCount = 1;
    fDirFound = (BOOL)(!DosFindFirst(rgchSrcDir, &hdir,
            FILE_NORMAL | FILE_DIRECTORY, &ffindBuf, sizeof(FILEFINDBUF),
            &usSearchCount, 0L));
#else  /* !OS2_VER */
    fDirFound = (BOOL)(!_dos_findfirst(rgchSrcDir, _A_SUBDIR, &ffindBuf));
#endif /* !OS2_VER */

    while (fDirFound)
        {
        if (strcmp(ffindBuf.ZZNAMEZZ, ".") && strcmp(ffindBuf.ZZNAMEZZ, ".."))
            {
#ifdef OS2_VER
            if (ffindBuf.attrFile & FILE_DIRECTORY)
#else  /* !OS2_VER */
            struct  stat  statBuf;

            strcpy(rgchSrcDir + cchSrcSav, ffindBuf.ZZNAMEZZ);
            if (!stat(rgchSrcDir, &statBuf) && (statBuf.st_mode & S_IFDIR))
#endif /* !OS2_VER */
                {
                SDL *  psdl = (SDL *)malloc(sizeof(SDL));

                psdl->szSubdir = strdup(ffindBuf.ZZNAMEZZ);
                psdl->psdlNext = psdlHead;
                psdlHead = psdl;
                }
            }

#ifdef OS2_VER
        usSearchCount = 1;
        fDirFound = (BOOL)(!DosFindNext(hdir, &ffindBuf, sizeof(FILEFINDBUF),
                &usSearchCount));
#else  /* !OS2_VER */
        fDirFound = (BOOL)(!_dos_findnext(&ffindBuf));
#endif /* !OS2_VER */
        }
#ifdef OS2_VER
    DosFindClose(hdir);
#endif /* OS2_VER */

    strcpy(rgchSrcDir + cchSrcSav, "*.*");
    rgchDestDir[cchDestSav] = '\0';
    HandleDirFiles(rgchSrcDir, rgchDestDir);

    while (psdlHead != NULL)
        {
        SDL *  psdlSav = psdlHead;

        strcat(strcpy(rgchSrcDir + cchSrcSav, psdlHead->szSubdir), "\\");

        strcpy(rgchDestDir + cchDestSav, psdlHead->szSubdir);
        if (!fComputeOnly)
            mkdir(rgchDestDir);
        strcat(rgchDestDir, "\\");

        RecurseOnDir(rgchSrcDir, rgchDestDir);

        psdlHead = psdlHead->psdlNext;
        free(psdlSav->szSubdir);
        free(psdlSav);
        }
}


void  HandleDirFiles(SZ rgchSrcArg, SZ rgchDestDir)
{
    int     cchSrcSav = strlen(rgchSrcArg);
    int     cchDestSav = strlen(rgchDestDir);
    BOOL    fFileFound;
#ifdef OS2_VER
    HDIR    hdir;
    USHORT  usSearchCount;
    FILEFINDBUF     ffindBuf;
#else  /* !OS2_VER */
    struct  find_t  ffindBuf;
#endif /* !OS2_VER */

      /* only count chars in SrcPath (includes backslash) */
    while (cchSrcSav && *(rgchSrcArg + cchSrcSav - 1) != '\\'
            && *(rgchSrcArg + cchSrcSav - 1) != ':')
        cchSrcSav--;

#ifdef OS2_VER
    hdir = HDIR_CREATE;
    usSearchCount = 1;
    fFileFound = (BOOL)(!DosFindFirst(rgchSrcArg, &hdir,
            FILE_NORMAL | FILE_READONLY, &ffindBuf, sizeof(FILEFINDBUF),
            &usSearchCount, 0L));
#else  /* !OS2_VER */
    fFileFound = (BOOL)(!_dos_findfirst(rgchSrcArg, _A_NORMAL | _A_RDONLY,
            &ffindBuf));
#endif /* !OS2_VER */

    while (fFileFound)
        {
        strcpy(rgchSrcArg + cchSrcSav, ffindBuf.ZZNAMEZZ);
        rgchDestDir[cchDestSav] = '\0';

        HandleOneFile(rgchSrcArg, rgchDestDir);

#ifdef OS2_VER
        usSearchCount = 1;
        fFileFound = (BOOL)(!DosFindNext(hdir, &ffindBuf, sizeof(FILEFINDBUF),
                &usSearchCount));
#else  /* !OS2_VER */
        fFileFound = (BOOL)(!_dos_findnext(&ffindBuf));
#endif /* !OS2_VER */
        }
#ifdef OS2_VER
    DosFindClose(hdir);
#endif /* OS2_VER */
}


void  HandleOneFile(SZ rgchSrcFile, SZ rgchDestArg)
{
    SZ      szSrcBase;
    SZ      szSrcExt;
    int     fhSrc;
    int     fhDest;
    LONG    lcbSrc;
    LONG    lcbDest;
    LONG    lcbCompressed = 0L;
    BOOL    fDotRemoved = FALSE;
#ifdef OS2_VER
    FILESTATUS  fsSrc;
#else  /* !OS2_VER */
    USHORT  usDate;
    USHORT  usTime;
#endif /* !OS2_VER */

    printf("%s\n", rgchSrcFile);

    if (FDirectory(rgchDestArg))
        {
        CHAR *  szSrcFileName = strrchr(rgchSrcFile, '\\');

        if (szSrcFileName == NULL)
            szSrcFileName = strrchr(rgchSrcFile, ':');

        if (szSrcFileName == NULL)
            szSrcFileName = rgchSrcFile;
        else
            szSrcFileName++;

        if (fDollarSuffix)
            GetDollarSzFromSzSrc(szSrcFileName,
                    rgchDestArg + strlen(rgchDestArg));
        else
            strcat(rgchDestArg, szSrcFileName);

        if (!stricmp(rgchDestArg, rgchSrcFile))
            FIncrementSzBase(rgchDestArg);
        }

    if ((fhSrc = open(rgchSrcFile, O_RDONLY | O_BINARY)) == -1)
        {
        printf(szCantOpenInput, rgchSrcFile);
        exit(21);
        }

    if (WReadHeaderInfo(fhSrc) >= wAlgTypeNoCompress)
        {
        FFreeHeaderInfo();

        if (!vfForceOverwrite)
            printf(szAlreadyCompressed, rgchSrcFile);

        if (vfForceOverwrite || FUserConfirmsYN())
            lseek(fhSrc, 0L, SEEK_SET);
        else
            {
            close(fhSrc);
            exit(22);
            }
        }

    DividePathIntoBaseExt(rgchSrcFile, &szSrcBase, &szSrcExt);
    if (szSrcExt != NULL)
        {
        *(szSrcExt - 1) = '\0';        /* remove the '.' between base and ext */
        fDotRemoved = TRUE;
        }
    else
        szSrcExt = "";

    lcbSrc = filelength(fhSrc);
    if (lcbSrc == -1L)
        ExitErrorMsgRc(rcReadSeekError);

      /* set global variables */
    if (strlen(szSrcBase) > _MAX_FNAME - 1 || strlen(szSrcExt) > _MAX_EXT - 1)
        {
        printf(szFilenameTooLongSz, szSrcBase, szSrcExt);
        exit(4);
        }

    if (fIncludeSrcLength)
        vlcbSrcLength = 0L;

    if (fIncludeChecksum)
        vfChecksum = (BOOL)TRUE;

    if (fIncludeBase)
        strcpy(vszBaseName, szSrcBase);

    if (fIncludeExt)
        {
        if (*szSrcExt == '\0')    /* use a space to indicate a null extension */
            strcpy(vszExtension, " ");
        else
            strcpy(vszExtension, szSrcExt);
        }

#ifdef EAS
#ifdef OS2_VER
    if (fIncludeEAs)
        {
        vuscbEAs = 0;

        if (fDotRemoved)
            *(szSrcExt - 1) = '.';

        if (!QueryEAs(rgchSrcFile, &vrgbEAs))
            printf("Unable to read in file's EAs.\n");

        if (fDotRemoved)
            *(szSrcExt - 1) = '\0';

        if (vrgbEAs)
            vuscbEAs = (USHORT)(*((LONG far *)vrgbEAs));
        }
#endif /* !OS2_VER */
#endif /* EAS */

    if (fDotRemoved)
        *(--szSrcExt) = '.';              /* replace what we took out earlier */

    if (cbText > 0)
        {
        vcbText = cbText;
        vszText = szText;
        }

    if (fComputeOnly)
        {
        lcbCompressed = LcbCalculateCompressedLength(wAlgType, fhSrc, NIL);
        close(fhSrc);
        if (lcbCompressed < rcNoError)
            ExitErrorMsgRc((SHORT)lcbCompressed);
        else
            {
            printf(szBytesCompressed, lcbSrc, lcbCompressed);
            ++cFiles;
            lcbSrcTotal += lcbSrc;
            lcbDestTotal += lcbCompressed;
            if (lcbCompressed < lcbSrc)
                printf(szPercentSavings, 
                          ((200 * (lcbSrc - lcbCompressed) / lcbSrc) + 1 ) / 2);
            else
                printf(szNoSavings);
            return;
            }
        }

#ifdef OS2_VER
    if (DosQFileInfo((HFILE)fhSrc, (USHORT)0x0001, &fsSrc,
                                                       (USHORT)(sizeof(fsSrc))))
#else /* not OS2_VER */
    if (_dos_getftime(fhSrc, &usDate, &usTime))
#endif /* OS2_VER */
        ExitErrorMsgRc(rcReadError);

    /* LOOP TO DO COMPRESSION */
    do     /* while (!eof(fhSrc)) */
        {
        if (!FEnsureSzBaseDoesNotExist(rgchDestArg))
            {
            close(fhSrc);
            exit(24);
            }

        if ((fhDest = open(rgchDestArg, O_RDWR | O_CREAT | O_BINARY | O_TRUNC,
                                                     S_IREAD | S_IWRITE)) == -1)
            {
            close(fhSrc);
            printf(szCantOpenOutput, rgchDestArg);
            exit(23);
            }

        lcbDest = LcbCompressToFile(wAlgType, fhSrc, fhDest, lcbSize);

#ifdef OS2_VER
        if (DosSetFileInfo((HFILE)fhDest, (USHORT)0x0001, &fsSrc,
                                                       (USHORT)(sizeof(fsSrc))))
#else /* not OS2_VER */
        if (_dos_setftime(fhDest, usDate, usTime))
#endif /* OS2_VER */
            ExitErrorMsgRc(rcWriteError);

        close(fhDest);

        if (lcbDest < rcNoError)
            {
            close(fhSrc);
            remove(rgchDestArg);
            ExitErrorMsgRc((SHORT)lcbDest);
            }

        printf(szWroteBytes, lcbDest, rgchDestArg);
        lcbCompressed += lcbDest;

        if (!eof(fhSrc))
            {
            if (!FIncrementSzBase(rgchDestArg))
                {
                close(fhSrc);
                exit(25);
                }
            }

        }  while (!eof(fhSrc) && !fOnePieceOnly);


    if (!eof(fhSrc))
        {
        LONG    lcbTail = lcbSrc - tell(fhSrc);
        BYTE    rgbBuffer[128];
        SHORT   cbSize;

        lcbSrc -= lcbTail;               /* uncompressed bytes of first piece */

        printf(szBytesCompressed, lcbSrc, lcbCompressed);
        ++cFiles;
        lcbSrcTotal += lcbSrc;
        lcbDestTotal += lcbCompressed;
        if (lcbCompressed >= lcbSrc)
            printf(szNoSavings);
        else
            printf(szPercentSavings,
                          ((200 * (lcbSrc - lcbCompressed) / lcbSrc) + 1 ) / 2);

        if (!FEnsureSzBaseDoesNotExist(rgchDestArg))
            {
            close(fhSrc);
            exit(24);
            }

        printf(szTailBytes, lcbTail, rgchDestArg);

        if ((fhDest = open(rgchDestArg, O_RDWR | O_CREAT | O_BINARY | O_TRUNC,
                                                     S_IREAD | S_IWRITE)) == -1)
            {
            close(fhSrc);
            printf(szCantOpenOutput, rgchDestArg);
            exit(23);
            }

        if ((cbSize = read(fhSrc, rgbBuffer, 128)) <= 0)
            {
            close(fhSrc);
            ExitErrorMsgRc(rcReadError);
            }

        while (cbSize > 0)
            {
            if (write(fhDest, rgbBuffer, cbSize) != cbSize)
                {
                close(fhSrc);
                close(fhDest);
                ExitErrorMsgRc(rcWriteError);
                }
            cbSize = read(fhSrc, rgbBuffer, 128);
            }

#ifdef OS2_VER
        if (DosSetFileInfo((HFILE)fhDest, (USHORT)0x0001, &fsSrc,
                                                       (USHORT)(sizeof(fsSrc))))
#else /* not OS2_VER */
        if (_dos_setftime(fhDest, usDate, usTime))
#endif /* OS2_VER */
            ExitErrorMsgRc(rcWriteError);

        close(fhDest);
        }
    else
        {
        printf(szBytesCompressed, lcbSrc, lcbCompressed);
        ++cFiles;
        lcbSrcTotal += lcbSrc;
        lcbDestTotal += lcbCompressed;
        if (lcbCompressed >= lcbSrc)
            printf(szNoSavings);
        else
            printf(szPercentSavings,
                          ((200 * (lcbSrc - lcbCompressed) / lcbSrc) + 1 ) / 2);
        }

    close(fhSrc);
}


/*
**  void  Usage(void)
**
**  Print usage message when command line args are bad.
*/
void Usage(SHORT wExitCode)
{
    printf(szCompUsage1);
    printf(szCompUsage2, wAlgTypeDefault);
#ifdef NC_XOR1
    printf(szCompUsage3, wAlgTypeNoCompress);
    printf(szCompUsage4, wAlgTypeXOR1);
#endif
#ifdef ZK1
    printf(szCompUsage5, wAlgTypeZK1);
#endif
#ifdef JJJ1
    printf(szCompUsageJJJ1, wAlgTypeJJJ1);
#endif
    printf(szCompUsage6);
    printf(szCompUsage7);
    printf(szCompUsage8);
    printf(szCompUsage9);
    printf(szCompUsage10);
    printf(szCompUsage11);
    printf(szCompUsage12);
    printf(szCompUsage13);
    printf(szCompUsage14);
    printf(szCompUsage15);
    printf(szCompUsage16);
    printf(szCompUsage17);
    printf(szCompUsage18);
    printf(szCompUsage19);
#ifdef EAS
    printf(szCompUsage20);
#endif /* EAS */

    exit(wExitCode);
}


/*
**  BOOL  FEnsureSzBaseDoesNotExist(SZ szBaseFile)
*/
BOOL FEnsureSzBaseDoesNotExist(SZ szBaseFile)
{
    if (access(szBaseFile, 00))
        return((BOOL)TRUE);

    if (!vfForceOverwrite)
        {
        printf(szFileExists, szBaseFile);
        printf(szOverwriteYN);
        }

    if (vfForceOverwrite || FUserConfirmsYN())
        {
        chmod(szBaseFile, S_IWRITE | S_IREAD);
        remove(szBaseFile);
        if (access(szBaseFile, 00))
            return((BOOL)TRUE);
        printf(szCantOverwrite, szBaseFile);
        if (!FIncrementSzBase(szBaseFile))
            return(FALSE);
        }
    else
        if (!FIncrementSzBase(szBaseFile))
            return(FALSE);

    return(FEnsureSzBaseDoesNotExist(szBaseFile));
}


/*
**  BOOL  FIncrementSzBase(SZ szBaseFile)
**
**  If the last character of szBaseFile (not counting the extension) is
**  a digit, then we can increment the filename.  Otherwise, change it to
**  a two.
*/
BOOL FIncrementSzBase(SZ szBaseFile)
{
    char *pchBase;
    char *pchExt;
    char *pchDigit;
    char  szNewName[13];

    DividePathIntoBaseExt(szBaseFile, &pchBase, &pchExt);
    if (pchExt != NULL)
        *(--pchExt) = '\0';             /* erase the '.' between base and ext */

    if (strlen(pchBase) >= 8)
        pchDigit = pchBase + 7;
    else
        pchDigit = pchBase + strlen(pchBase) - 1;

    if (!isdigit(*pchDigit))
        {
        if (strlen(pchBase) >= 8)
            {
            *pchDigit = '2';
            if (pchExt != NULL)
                *pchExt = '.';
            }
        else
            {
            strcpy(szNewName, pchBase);
            if (pchExt != NULL)
                {
                strcat(szNewName, "2.");
                strcat(szNewName, pchExt + 1);
                }
            else
                strcat(szNewName, "2");
            strcpy(pchBase, szNewName);
            }
        return((BOOL)TRUE);
        }

    if (*pchDigit == '9')
        {
        printf(szNoOutputNames);
        return(FALSE);
        }

    (*pchDigit)++;
    if (pchExt != NULL)
        *pchExt = '.';

    return((BOOL)TRUE);
}


#ifdef EAS
#ifdef OS2_VER

#define MAX_GEA         300 /* Max size for a GEA List              */
#define Ref_ASCIIZ      1   /* Reference type for DosEnumAttribute  */
#define GetInfoLevel1   1   /* Get info from SFT */
#define GetInfoLevel3   3   /* Get FEAlist given the GEAlist */

BOOL QueryEAs(CHAR * pszPath, CHAR ** pEABuf)
{
    struct FSPlus
        {
        FILESTATUS fs;
        long       easize;
        } fsp;
    USHORT usRet;
    ULONG  ulCurFEA, dAlloc;

    CHAR * pAlloc, * pAlloc2;
    CHAR * pRetBuf;

    ULONG  ulEntryNum = 1; /* count of current EA to read (1-relative)        */
    ULONG  ulEnumCnt;      /* Number of EAs for Enum to return, always 1      */

    FEA *  pFEA;           /* Used to read from Enum's return buffer          */
    GEALIST * pGEAList;    /* Ptr used to set up buffer for DosQPathInfo call */
    EAOP   eaopGet;        /* Used to call DosQPathInfo                       */

    *pEABuf = NULL;

    usRet = DosQPathInfo(pszPath, FIL_QUERYEASIZE, (char *)&fsp, sizeof(fsp),
                        0L);
    if (usRet)
        {
        printf("Can't even query the EAs size. Error: %u\n",usRet);
        printf("File name: '%s'\n",pszPath);
        return FALSE;
        }

    if (fsp.easize == 4L)
        return TRUE;

    pRetBuf = malloc((USHORT)fsp.easize);
    if (!pRetBuf)
        {
        printf("Couldn't allocated %ld bytes on the far heap...\n",fsp.easize);
        return FALSE;
        }
    *pEABuf = pRetBuf;

    ulCurFEA = fsp.easize - 4;

    pAlloc  = malloc(MAX_GEA);
    pAlloc2 = malloc(MAX_GEA);
    if (!pAlloc || !pAlloc2)
        {
        free(pRetBuf);
        pRetBuf = NULL;
        printf("Couldn't allocated the 500 bytes of local memory needed...\n");
        return FALSE;
        }

    pFEA = (FEA *)pAlloc;
    while (TRUE)
        {
        ulEnumCnt = 1;
        if (DosEnumAttribute(Ref_ASCIIZ, pszPath, ulEntryNum, pAlloc, MAX_GEA,
                          &ulEnumCnt, (LONG)GetInfoLevel1, 0L))
            break;

        if (ulEnumCnt != 1)
            break;

        ulEntryNum++;

        dAlloc = (ULONG)(pFEA->cbName + 1 + pFEA->cbValue + sizeof(FEA));
        ulCurFEA -= dAlloc;

        pGEAList = (GEALIST *)pAlloc2;

        pGEAList->cbList = sizeof(GEALIST) + pFEA->cbName;
        pGEAList->list[0].cbName = pFEA->cbName;
        strcpy(pGEAList->list[0].szName, pAlloc + sizeof(FEA));

        eaopGet.fpGEAList = (GEALIST far *)pAlloc2;
        eaopGet.fpFEAList = (FEALIST far *)&pRetBuf[ulCurFEA];

        eaopGet.fpFEAList->cbList = dAlloc + sizeof(ULONG);

        usRet = DosQPathInfo(pszPath, GetInfoLevel3, (PVOID)&eaopGet,
                    sizeof(EAOP), 0L);
        if (usRet)
            {
            printf("Error on reading EA from disk\n");
            free(pAlloc);
            free(pAlloc2);
            return FALSE;
            }
        }
    free(pAlloc);
    free(pAlloc2);

    if (ulCurFEA != 0)
        {
        printf("Checksum problems...\n");
        return FALSE;
        }
    eaopGet.fpFEAList->cbList = fsp.easize;

    return(TRUE);
}
#endif /* !OS2_VER */
#endif /* EAS */
