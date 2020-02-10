/* wingroup.c - Adds a new Program Manager group to PROGMAN.INI.
 * Roy Harper
 * Copyright (C) Microsoft, 1992
 * June 8, 1992
 *
 * Portions came from DSUSRCH.C by Mike Colee
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <io.h>
#include <dos.h>
#include <errno.h>
#include <disk_io.h>
#include <file_io.h>
#include <fcntl.h>
#include <share.h>

#include <window.h>
#include <alias.h>
#include <install.h>
#include <message.h>

#ifdef	OEM_PROGRAM
	#include <oem.h>
#else
	#include <global.h>
#endif

static struct BPB	Bpb;							/* Bpb from current disk		 */

char *gszGroup                         = "Group";
#define GSZ_GROUP_LEN                    5
char *gszGroupsSection                 = "[Groups]";
char *gszGroupTemplate                 = "Group%d=%s";
char *gszProgmanIni                    = "PROGMAN.INI";
char *gszProgmanTmp                    = "__PGI__.$$$";
char *gszProgmanBak                    = "PROGMAN.BAK";
char *gszWNToolsGrp                    = "WNTOOLS.GRP";
char *gszWinCom                        = "WIN.COM";
char *gszSystemIni                     = "SYSTEM.INI";
char *gszSystemTmp                     = "__SYS__.$$$";
char *gszSystemBak                     = "SYSTEM.BAK";
char *gszVFINTD386                     = "VFINTD.386";
char *gszWinfileIni                    = "WINFILE.INI";
char *gszWinfileTmp                    = "__WFI__.$$$";
char *gszWinfileBak                    = "WINFILE.BAK";
char *gszMstoolsDll                    = "MSTOOLS.DLL";
char *gsz386EnhSection                 = "[386Enh]";
char *gszCommentBegin                  =
    ";======== MS-DOS 6 Setup Modification - Begin ========";
char *gszCommentEnd                    =
    ";======== MS-DOS 6 Setup Modification - End ========";

char *gszRemovalComment                = ";Removed by MS-DOS 6.0 Setup: ";


/* DBCS */
#ifndef DBCS
   #define AnsiPrev(h,p)     ((p)-1)      // AnsiPrev for SBCS
   #define AnsiNext(p)       ((p)+1)      // AnsiNext for SBCS
   #define IsDBCSLeadByte(p) FALSE        // IsDBCSLeadByte for SBCS
   #define OFFSET(x)  (x)
#else
   #define OFFSET(fp) ((PSTR)(LOWORD((DWORD)fp)))
   #ifdef DOSONLY
      #include <dbcs.h>
   #else
      #ifndef WM_USER  // Is Windows.H included ?
         extern int   far pascal IsDBCSLeadByte(unsigned char);
         extern FPUCH far pascal AnsiNext(FPUCH);
         extern FPUCH far pascal AnsiPrev(FPUCH, FPUCH);
      #endif
   #endif
#endif  //DBCS

#ifdef JANUS
int fnIsOs2Win(char *szPath);
#endif

void InstallWNToolsGrp (void);
void InstallVFINTD386 (void);
void InstallMstoolsDll (void);
unsigned AddSectionAndLine (int      fReplace,
                            char     *pszSection,
                            char     *pszLine,
                            char     *szBuffer,
                            unsigned uCharCount);
unsigned MyDeleteLine (char *szBuffer, unsigned uCharCount, char *sz);
unsigned MyInsertLine (char     *szNewString,
                       char     *szBuffer,
                       unsigned uCharCount,
                       char     *sz);
int MyGetFixedDisks (int *piDriveLetters);
int GetFileAttributes (char *pszPathname);
int SetFileAttributes (char *pszPathname, int iAttribute);
int ValidDrive (char bDrive);
int myfnSearchDisk (char *szPath, char *szFile1, char *szFile2,
                    char *szFile3);
char DosGetCurrentDrive (void);
char DosSetCurrentDrive (char bDrive);

int  UniqueRename (char *szFile, char *szNew);


/*********************************************************************
 * InstallWNToolsGrp - Installs WNTOOLS.GRP file in PROGMAN.INI.
 *********************************************************************/

void InstallWNToolsGrp (void)
{
  char     *szIniPath;                /* Path to PROGMAN.INI           */
  char     *szTmpPath;                /* Path to __PGI__.$$$           */
  char     *szGrpPath;                /* Path to WNTOOLS.GRP           */
  char     *szBuffer;                 /* Input/output buffer           */
  char     *sz;                       /* Character pointer             */
  int      fileInput;                 /* Old PROGMAN.INI file handle   */
  int      fileOutput;                /* .TMP output file handle       */
  int      ShareAccess;               /* Share access                  */
  int      fProgmanBakAttr;           /* MS-DOS File attributes        */
  int      fProgmanIniAttr;           /* MS-DOS File attributes        */
  int      iReadValue    = OK;        /* Return value from DosReadLine */
  int      iWriteValue   = OK;        /* Return value from DosWriteLine*/
  int      iMaxGroupNmbr = 0;         /* Highest numbered group        */
  int      iGroupNmbr    = 0;         /* Highest numbered group        */
  int      iGroupCount   = 0;         /* Count of the number of groups */
  int      iBlankLines   = 0;         /* Counts the number of blank    */
                                      /*   lines at the end of the     */
                                      /*   [Groups] section.           */


  /* Create the path to PROGMAN.INI */
  szIniPath = GetMemory (MAX_PATH_LEN);
  strcpy (szIniPath, vInfo.szWinPath);
  mycatpath (szIniPath, gszProgmanIni);


  /* Create the path to __PGI__.$$$ */
  szTmpPath = GetMemory (MAX_PATH_LEN);

  /* Determine if the Windows directory is on the destination drive */
  if (vInfo.szWinPath[0] == vInfo.szTmpDir[0])
    strcpy (szTmpPath, vInfo.szTmpDir);
  else
    strcpy (szTmpPath, vInfo.szWinPath);

  mycatpath (szTmpPath, gszProgmanTmp);


  /* Create the path to WNTOOLS.GRP */
  szGrpPath = GetMemory (MAX_PATH_LEN);
  strcpy (szGrpPath, vInfo.szPath);
  mycatpath (szGrpPath, gszWNToolsGrp);


  /* Add the group to the PROGMAN.INI file */
  if (_osmajor < 3  || (_osmajor == 3 && _osminor < 10))
    ShareAccess = O_RDONLY;
  else
    ShareAccess = SH_DENYWR;


  {
    int iRetVal;    /* Return codes from _dos_open */

    /* Open PROGMAN.INI for input and __PGI__.$$$ for output */
    if ((iRetVal = _dos_open (szIniPath, ShareAccess, &fileInput)) != OK ||
        (_dos_creat (szTmpPath, 0, &fileOutput)) != OK)
      {
        char  *apszError[PROGMAN_INI_ERROR_LINES]; /* error message string array */

        GetMessage (apszError, PROGMAN_INI_ERROR_TEXT);
        Error (apszError);

        /* Close the files that were opened */
        if (iRetVal == OK)
          _dos_close (fileInput);

        FreeMemory (szIniPath);
        FreeMemory (szTmpPath);
        FreeMemory (szGrpPath);

        return;
      }
  }

  /* Allocate space for the buffer */
  szBuffer = GetMemory (MAX_PROGMAN_INI_LINE_LEN + 1);

  /* Find the section */
  if (MyFindSection (gszGroupsSection, fileInput, fileOutput) != TRUE)
    {
      while (iWriteValue == OK &&
             (iReadValue = DosReadLine (szBuffer, MAX_PROGMAN_INI_LINE_LEN,
                               fileInput)) == OK && szBuffer[0] != '[')
        {
          /* Check to see if the WNTOOLS.GRP file is already mentioned */
          if ((sz = strchr (szBuffer, '=')) != NULL &&
               stricmp (++sz, szGrpPath) == 0)
            {
              /* If the existing PROGMAN.INI mentions our .GRP file */
              /*   we don't have to change PROGMAN.INI.             */

              /* Close the files */
              _dos_close (fileInput);
              _dos_close (fileOutput);

              /* Delete __PGI__.$$$ */
              AbsUnlink (szTmpPath);

              /* Free up szBuffer */
              FreeMemory (szBuffer);
              FreeMemory (szIniPath);
              FreeMemory (szTmpPath);
              FreeMemory (szGrpPath);

              return;
            }

          /* Count up the blank lines */
          if (szBuffer[0])
            {
              /* Write out the pending blank lines */
              while (iBlankLines > 0 && iWriteValue == OK)
                {
                  --iBlankLines;
                  iWriteValue = DosWriteLine (NULL, fileOutput);
                }

              /* Write out the line right away */
              if (iWriteValue == OK)
                iWriteValue = DosWriteLine (szBuffer, fileOutput);

              iBlankLines = 0;

              /* Check for error */
              if (iWriteValue)
                break;
            }
          else
            ++iBlankLines;

          /* Ignore blank lines or lines beginning with a semi-colon */
          if (szBuffer[0] == '\0' || szBuffer[0] == ';')
            continue;

          /* Is this a "Group%d" line? */
          if (strnicmp (gszGroup, szBuffer, GSZ_GROUP_LEN) == 0)
            {
              ++iGroupCount;

              /* Find the highest numbered group */
              iGroupNmbr = max (atoi (&szBuffer[GSZ_GROUP_LEN]), iGroupNmbr);
            }
        }

      /* If we just read a new section, unread the line */
      if (iReadValue == OK && szBuffer[0] == '[')
        DosUnreadLine (szBuffer);

      /* We have reached the end of the section -- Write our group line, */
      /*   but only if Program Manager isn't at it's limit already       */
      if (iGroupCount < MAX_PROGRAM_MANAGER_GROUPS)
        {
          if (iWriteValue == OK)
            iWriteValue = DosWriteLine (gszCommentBegin, fileOutput);

          /* Write the group line */
          sprintf (szBuffer, gszGroupTemplate, iGroupNmbr + 1, szGrpPath);
          if (iWriteValue == OK)
            iWriteValue = DosWriteLine (szBuffer, fileOutput);

          if (iWriteValue == OK)
            iWriteValue = DosWriteLine (gszCommentEnd, fileOutput);
        }

      /* Write out the pending blank lines */
      while (iBlankLines > 0 && iWriteValue == OK)
        {
          --iBlankLines;
          iWriteValue = DosWriteLine (NULL, fileOutput);
        }

      /* Copy the rest of the PROGMAN.INI file through */
      while ((DosReadLine (szBuffer, MAX_PROGMAN_INI_LINE_LEN, fileInput)) == OK &&
             iWriteValue == OK)
        iWriteValue = DosWriteLine (szBuffer, fileOutput);
    }
  else
    {
      /* A [Groups] section was not found.  Add one. */
      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (NULL, fileOutput);

      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (gszCommentBegin, fileOutput);

      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (gszGroupsSection, fileOutput);

      /* Add the group */
      sprintf (szBuffer, gszGroupTemplate, 1, szGrpPath);
      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (szBuffer, fileOutput);

      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (gszCommentEnd, fileOutput);
    }

  /* Close the files */
  _dos_close (fileInput);
  _dos_close (fileOutput);


  /* If a write error occured, give an error message */
  if (iWriteValue)
    {
      char  *apszError[PROGMAN_INI_ERROR_LINES]; /* error message string array */

      /* Delete __PGI__.$$$ */
      AbsUnlink (szTmpPath);

      GetMessage (apszError, PROGMAN_INI_ERROR_TEXT);
      Error (apszError);
    }
  else
    {
      if (vInfo.szWinPath[0] == vInfo.szTmpDir[0])
        {
          /* .INI file IS on the destination drive */
          if ((UniqueRename (szIniPath, NULL)    != OK) ||
              ReplaceFile (szTmpPath, szIniPath) != OK)
            ProcessCopyError (szIniPath, ERR_RENAMING);
        }
      else /* .INI file IS NOT on the destination drive */
        {
          /* Create the path to PROGMAN.BAK */
          strcpy (szBuffer, szIniPath);
          myfnTruncFN (szBuffer);
          mycatpath (szBuffer, gszProgmanBak);

          /* Get System, Hidden, and Read-only bits */
          fProgmanBakAttr = GetFileAttributes (szBuffer);

          /* Delete PROGMAN.BAK */
          AbsUnlink (szBuffer);

          /* Rename PROGMAN.INI to PROGMAN.BAK */
          fProgmanIniAttr = GetFileAttributes (szIniPath);
          if (fProgmanIniAttr != -1)
            SetFileAttributes (szIniPath, fProgmanIniAttr & 0xF8);
          rename (szIniPath, szBuffer);
          if (fProgmanBakAttr != -1)
            SetFileAttributes (szIniPath, fProgmanBakAttr);

          /* Rename __PGI__.$$$ to PROGMAN.INI */
          rename (szTmpPath, szIniPath);

          /* Make the new PROGMAN.INI's file attributes match the original's */
          if (fProgmanIniAttr != -1)
            SetFileAttributes (szBuffer, fProgmanIniAttr);
        }
    }

  /* Free up szBuffer */
  FreeMemory (szBuffer);
  FreeMemory (szIniPath);
  FreeMemory (szTmpPath);
  FreeMemory (szGrpPath);
}


/*********************************************************************
 * InstallVFINTD386 - Installs VFINTD.386 in SYSTEM.INI.
 *********************************************************************/

void InstallVFINTD386 (void)
{
  char     *apszVxd[VXD_REMOVAL_LIST_LINES]; /* VxDs to remove         */
  char     *szIniPath;                /* Path to SYSTEM.INI            */
  char     *szTmpPath;                /* Path to __SYS__.$$$           */
  char     *szDevPath;                /* Path to VFINTD.386            */
  char     *szBuffer;                 /* Input/output buffer           */
  char     *sz;                       /* Character pointer             */
  int      u;                         /* Index variable                */
  int      fileInput;                 /* Old SYSTEM.INI file handle    */
  int      fileOutput;                /* .TMP output file handle       */
  int      ShareAccess;               /* Share access                  */
  int      fSystemBakAttr;            /* MS-DOS File attributes        */
  int      fSystemIniAttr;            /* MS-DOS File attributes        */
  int      iCommentBeginLen;          /* Length of gszCommentBegin     */
  int      iCommentEndLen;            /* Length of gszCommentEnd       */
  int      iReadValue  = OK;          /* Return value from DosReadLine */
  int      iWriteValue = OK;          /* Return value from DosWriteLine*/
  int      iBlankLines = 0;           /* Counts the number of blank    */
                                      /*   lines at the end of the     */
                                      /*   [Groups] section.           */


  /* Set the lengths */
  iCommentBeginLen = strlen (gszCommentBegin);
  iCommentEndLen   = strlen (gszCommentEnd);


  /* Create the path to SYSTEM.INI */
  szIniPath = GetMemory (MAX_PATH_LEN);
  strcpy (szIniPath, vInfo.szWinPath);
  mycatpath (szIniPath, gszSystemIni);


  /* Create the path to __SYS__.$$$ */
  szTmpPath = GetMemory (MAX_PATH_LEN);

  /* Determine if the Windows directory is on the destination drive */
  if (vInfo.szWinPath[0] == vInfo.szTmpDir[0])
    strcpy (szTmpPath, vInfo.szTmpDir);
  else
    strcpy (szTmpPath, vInfo.szWinPath);

  mycatpath (szTmpPath, gszSystemTmp);


  /* Create the path to VFINTD.386 */
  szDevPath = GetMemory (MAX_PATH_LEN);
  strcpy (szDevPath, vInfo.szPath);
  mycatpath (szDevPath, gszVFINTD386);


  /* Add the device driver to the SYSTEM.INI file */
  if (_osmajor < 3  || (_osmajor == 3 && _osminor < 10))
    ShareAccess = O_RDONLY;
  else
    ShareAccess = SH_DENYWR;


  {
    int iRetVal;    /* Return codes from _dos_open */

    /* Open SYSTEM.INI for input and __SYS__.$$$ for output */
    if ((iRetVal = _dos_open (szIniPath, ShareAccess, &fileInput)) != OK ||
        (_dos_creat (szTmpPath, 0, &fileOutput)) != OK)
      {
        char  *apszError[SYSTEM_INI_ERROR_LINES];

        GetMessage (apszError, SYSTEM_INI_ERROR_TEXT);
        Error (apszError);

        /* Close the files that were opened */
        if (iRetVal == OK)
          _dos_close (fileInput);

        FreeMemory (szIniPath);
        FreeMemory (szTmpPath);
        FreeMemory (szDevPath);

        return;
      }
  }

  /* Allocate space for the buffer */
  szBuffer = GetMemory (MAX_SYSTEM_INI_LINE_LEN + 1);

  /* Find the section */
  if (MyFindSection (gsz386EnhSection, fileInput, fileOutput) != TRUE)
    {
      /* Add the lines for the Windows Backup .VxD */
      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (gszCommentBegin, fileOutput);
      strcpy (szBuffer, "device=");
      strcat (szBuffer, szDevPath);
      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (szBuffer, fileOutput);
      if (iWriteValue == OK)
        iWriteValue = DosWriteLine (gszCommentEnd, fileOutput);

      /* Get the list of .VxDs to remove */
      GetMessage (apszVxd, VXD_REMOVAL_LIST_TEXT);


      /* Now, go through the rest of the [386Enh] section */
      /*   removing lines that must be removed.           */
      while (iWriteValue == OK &&
             (iReadValue =
                  DosReadLine (szBuffer, MAX_SYSTEM_INI_LINE_LEN,
                               fileInput)) == FALSE && szBuffer[0] != '[')
        {
          /* Check for MS-DOS 6.0's "do not modify" comment lines */
          if (strnicmp (szBuffer, gszCommentBegin, iCommentBeginLen) == 0)
            {
              /* Remove all lines between these comment lines */
              while (iReadValue == OK &&
                     (iReadValue =
                          DosReadLine (szBuffer, MAX_SYSTEM_INI_LINE_LEN,
                                       fileInput)) == FALSE &&
                      strnicmp (szBuffer, gszCommentEnd, iCommentEndLen) != 0)
                ;

              /* Display an error message if we didn't find */
              /*   a matching line before the end of file   */
              if (iReadValue != OK)
                {
                  char  *apszError[SYSTEM_INI_MODIFIED_ERROR_LINES];

                  /* Close the files */
                  _dos_close (fileInput);
                  _dos_close (fileOutput);

                  /* Delete __SYS__.$$$ */
                  AbsUnlink (szTmpPath);

                  /* Free up szBuffer */
                  FreeMemory (szBuffer);
                  FreeMemory (szIniPath);
                  FreeMemory (szTmpPath);
                  FreeMemory (szDevPath);

                  GetMessage (apszError, SYSTEM_INI_MODIFIED_ERROR_TEXT);
                  Error (apszError);

                  return;
                }
              else
                /* Continue reading lines */
                continue;
            }


          /* Check for .VxDs that have to be removed */

          /* Skip whitespace */
          u = strspn (szBuffer, " \t");
          sz = &szBuffer[u];

          if (strnicmp (sz, "device", 6) == 0)
            {
              /* Skip whitespace after "device" */
              sz += 6;
              sz += strspn (sz, " \t");

              /* Check for an '=' sign */
              if ((sz = strchr (sz, '=')) != NULL)
                {
                  int  i;                   /* Looping variable */
                  int  fRemoveLine = FALSE; /* TRUE when line must be removed */
                  char szBuffer1[MAX_SYSTEM_INI_LINE_LEN];
                  char szVxdPath[MAX_SYSTEM_INI_LINE_LEN]; /* Identifies .VxD */
                  char *pszVxdPath;         /* Used as character pointer */


                  /* Skip whitespace after the '=' sign */
                  ++sz;
                  sz += strspn (sz, " \t");

                  /* Copy the .VxD pathname */
                  strcpy (szVxdPath, sz);
                  pszVxdPath = &szVxdPath[0];

                  /* Remove extraneous characters */
                  u = strcspn (pszVxdPath, "/,; \t");
                  if (u)
                    pszVxdPath[u] = '\0';

                  /* Look for the last colon or backslash */
                  if (strrchr (pszVxdPath, ':'))
                    pszVxdPath = strrchr (pszVxdPath, ':') + 1;
                  if (strrchr (pszVxdPath, '\\'))
                    pszVxdPath = strrchr (pszVxdPath, '\\') + 1;

                  /* pszVxdPath now points to the filename. */
                  /*   Check the removal list               */
                  for (i = 0; apszVxd[i] != NULL && fRemoveLine == FALSE; ++i)
                    {
                      if (stricmp (pszVxdPath, apszVxd[i]) == 0)
                        fRemoveLine = TRUE;
                    }

                  /* If this line is to be removed, comment the line out */
                  if (fRemoveLine == TRUE)
                    {
                      strcpy (szBuffer1, gszRemovalComment);
                      strcat (szBuffer1, szBuffer);
                      strcpy (szBuffer, szBuffer1);
                      fRemoveLine = FALSE;
                    }
                }
            }

          /* Write out the line */
          if (iWriteValue == OK)
            iWriteValue = DosWriteLine (szBuffer, fileOutput);
        }

      /* If we just read a new section, unread the line */
      if (iReadValue == OK && szBuffer[0] == '[')
        DosUnreadLine (szBuffer);

      /* Write out the pending blank lines */
      while (iBlankLines > 0 && iWriteValue == OK)
        {
          --iBlankLines;
          iWriteValue = DosWriteLine (NULL, fileOutput);
        }

      /* Copy the rest of the SYSTEM.INI file through */
      while ((DosReadLine (szBuffer, MAX_SYSTEM_INI_LINE_LEN, fileInput)) == OK &&
             iWriteValue == OK)
        iWriteValue = DosWriteLine (szBuffer, fileOutput);
    }

  /* Close the files */
  _dos_close (fileInput);
  _dos_close (fileOutput);

  /* If an error occured, give an error message */
  if (iWriteValue)
    {
      char  *apszError[SYSTEM_INI_ERROR_LINES];


      /* Delete __SYS__.$$$ */
      AbsUnlink (szTmpPath);

      GetMessage (apszError, SYSTEM_INI_ERROR_TEXT);
      Error (apszError);
    }
  else
    {
      if (vInfo.szWinPath[0] == vInfo.szTmpDir[0])
        {
          /* .INI file IS on the destination drive */
          if ((UniqueRename (szIniPath, NULL)    != OK) ||
              ReplaceFile (szTmpPath, szIniPath) != OK)
            ProcessCopyError (szIniPath, ERR_RENAMING);
        }
      else /* .INI file IS NOT on the destination drive */
        {
          /* Create the path to SYSTEM.BAK */
          strcpy (szBuffer, szIniPath);
          myfnTruncFN (szBuffer);
          mycatpath (szBuffer, gszSystemBak);

          /* Get System, Hidden, and Read-only bits */
          fSystemBakAttr = GetFileAttributes (szBuffer);

          /* Delete SYSTEM.BAK */
          AbsUnlink (szBuffer);

          /* Rename SYSTEM.INI to SYSTEM.BAK */
          fSystemIniAttr = GetFileAttributes (szIniPath);
          if (fSystemIniAttr != -1)
            SetFileAttributes (szIniPath, fSystemIniAttr & 0xF8);
          rename (szIniPath, szBuffer);
          if (fSystemBakAttr != -1)
            SetFileAttributes (szIniPath, fSystemBakAttr);

          /* Rename __SYS__.$$$ to SYSTEM.INI */
          rename (szTmpPath, szIniPath);

          /* Make the new SYSTEM.INI's file attributes match the original's */
          if (fSystemIniAttr != -1)
            SetFileAttributes (szBuffer, fSystemIniAttr);
        }
    }

  /* Free up szBuffer */
  FreeMemory (szBuffer);
  FreeMemory (szIniPath);
  FreeMemory (szTmpPath);
  FreeMemory (szDevPath);
}


#define MAX_INI_FILE_LEN  (65536 - 48)
#define ADD_TO            1
#define REPLACE_LINE_TYPE 2

/*********************************************************************
 * InstallMstoolsDll - Installs MSTOOLS.DLL into WINFILE.INI file.
 *********************************************************************/

void InstallMstoolsDll (void)
{
  char     *szIniPath;                /* Path to WINFILE.INI            */
  char     *szTmpPath;                /* Path to __WFI__.$$$            */
  char     *szDllPath;                /* Path to MSTOOLS.DLL            */
  char     *szBuffer;                 /* Buffer for holding WINFILE.INI */
  char     *sz;                       /* Character pointer              */
  int      fileInput;                 /* Old WINFILE.INI file handle    */
  int      fileOutput;                /* .TMP output file handle        */
  int      ShareAccess;               /* Share access                   */
  int      fWinfileBakAttr;           /* MS-DOS File attributes         */
  int      fWinfileIniAttr;           /* MS-DOS File attributes         */
  unsigned uLen;                      /* String length (+ 1)            */
  int      iReturnValue = OK;         /* Return value from DosReadLine  */
  unsigned uCharCount   = 0;          /* Count of characters in file    */
  int      iRetVal;    /* Return codes from _dos_open */


  /* Create the path to WINFILE.INI */
  szIniPath = GetMemory (MAX_PATH_LEN);
  strcpy (szIniPath, vInfo.szWinPath);
  mycatpath (szIniPath, gszWinfileIni);


  /* Create the path to __WFI__.$$$ */
  szTmpPath = GetMemory (MAX_PATH_LEN);

  /* Determine if the Windows directory is on the destination drive */
  if (vInfo.szWinPath[0] == vInfo.szTmpDir[0])
    strcpy (szTmpPath, vInfo.szTmpDir);
  else
    strcpy (szTmpPath, vInfo.szWinPath);

  mycatpath (szTmpPath, gszWinfileTmp);


  /* Create the path to MSTOOLS.DLL */
  szDllPath = GetMemory (MAX_PATH_LEN);
  strcpy (szDllPath, vInfo.szPath);
  mycatpath (szDllPath, gszMstoolsDll);


  /* Add the lines to the WINFILE.INI file */
  if (_osmajor < 3  || (_osmajor == 3 && _osminor < 10))
    ShareAccess = O_RDONLY;
  else
    ShareAccess = SH_DENYWR;


  /* Read the file */
  {

    /* Open WINFILE.INI for input.  File may not exist. */
    iRetVal = _dos_open (szIniPath, ShareAccess, &fileInput);
    if ( (iRetVal != OK) && (iRetVal != ENOENT) )
      {
        char  *apszError[WINFILE_INI_ERROR_LINES];

        GetMessage (apszError, WINFILE_INI_ERROR_TEXT);
        Error (apszError);

        /* Close the files that were opened */
        if (iRetVal == OK)
          _dos_close (fileInput);

        FreeMemory (szIniPath);
        FreeMemory (szTmpPath);
        FreeMemory (szDllPath);

        return;
      }
  }


  /* Read the file */

  /* Allocate space for WINFILE.INI's text */
  szBuffer = GetMemory (MAX_INI_FILE_LEN);
  sz = szBuffer;

  if (iRetVal != ENOENT)
  {
      while ((iReturnValue = DosReadLine (sz, MAX_SYSTEM_INI_LINE_LEN,
                                          fileInput)) == FALSE)
        {
          uLen = strlen (sz) + 1;
          sz += uLen;
          uCharCount += uLen;

          /* Break out if there isn't enough memory */
          if ((long) uCharCount + (long) MAX_SYSTEM_INI_LINE_LEN >
              (long) MAX_INI_FILE_LEN)
            {
              char  *apszError[WINFILE_INI_ERROR_LINES];

              GetMessage (apszError, WINFILE_INI_ERROR_TEXT);
              Error (apszError);

              /* Close the file */
              _dos_close (fileInput);

              FreeMemory (szIniPath);
              FreeMemory (szTmpPath);
              FreeMemory (szDllPath);
              FreeMemory (szBuffer);

              return;
            }
        }


      /* Close the input file */
      _dos_close (fileInput);
  }

  /* Make the edits */

  {
    char szNewLine[MAX_SYSTEM_INI_LINE_LEN];    /* sprintf buffer     */
    char szNewPath[MAX_PATH_LEN];               /* DOS path workspace */
    char *apszAddOns[WINFILE_ADDONS_LINES];     /* [AddOns] strings   */
    char *apszSettings[WINFILE_SETTINGS_LINES]; /* [Settings] strings */


    /* Get the strings */
    GetMessage (apszAddOns, WINFILE_ADDONS_TEXT);
    GetMessage (apszSettings, WINFILE_SETTINGS_TEXT);


    /* [AddOns] section */

    /* Create the path to the .DLL */
    strcpy (szNewPath, vInfo.szPath);
    mycatpath (szNewPath, apszAddOns[2]);

    /* Create the line to be added */
    sprintf (szNewLine, apszAddOns[1], szNewPath);

    /* Add the line */
    uCharCount = AddSectionAndLine (REPLACE_LINE_TYPE, apszAddOns[0],
                                    szNewLine, szBuffer, uCharCount);


    /* [Settings] section */

    /* Create the path to the .DLL */
    strcpy (szNewPath, vInfo.szPath);
    mycatpath (szNewPath, apszSettings[2]);

    /* Create the line to be added */
    sprintf (szNewLine, apszSettings[1], szNewPath);

    /* Add the line */
    uCharCount = AddSectionAndLine (REPLACE_LINE_TYPE, apszSettings[0],
                                    szNewLine, szBuffer, uCharCount);
  }


  /* Write the file */

  {
    /* Open WINFILE.INI for input */
    if ((_dos_creat (szTmpPath, 0, &fileOutput)) != OK)
      {
        char  *apszError[WINFILE_INI_ERROR_LINES];

        GetMessage (apszError, WINFILE_INI_ERROR_TEXT);
        Error (apszError);

        /* Close the file */
        _dos_close (fileOutput);

        FreeMemory (szIniPath);
        FreeMemory (szTmpPath);
        FreeMemory (szDllPath);
        FreeMemory (szBuffer);

        return;
      }

    /* Write the lines */
    sz = szBuffer;
    iReturnValue = OK;
    while (sz < &szBuffer[uCharCount] && iReturnValue == OK)
      {
        iReturnValue = DosWriteLine (sz, fileOutput);
        sz += strlen (sz) + 1;
      }
  }

  /* Close the file */
  _dos_close (fileOutput);

  /* If an error occured, give an error message */
  if (iReturnValue)
    {
      char  *apszError[WINFILE_INI_ERROR_LINES];


      /* Delete __WFI__.$$$ */
      AbsUnlink (szTmpPath);

      GetMessage (apszError, WINFILE_INI_ERROR_TEXT);
      Error (apszError);
    }
  else
    {
      if (vInfo.szWinPath[0] == vInfo.szTmpDir[0])
        {
          /* .INI file IS on the destination drive */
          if ((UniqueRename (szIniPath, NULL)    != OK) ||
              ReplaceFile (szTmpPath, szIniPath) != OK)
            ProcessCopyError (szIniPath, ERR_RENAMING);
        }
      else /* .INI file IS NOT on the destination drive */
        {
          /* Create the path to WINFILE.BAK */
          strcpy (szBuffer, szIniPath);
          myfnTruncFN (szBuffer);
          mycatpath (szBuffer, gszWinfileBak);

          /* Get System, Hidden, and Read-only bits */
          fWinfileBakAttr = GetFileAttributes (szBuffer);

          /* Delete WINFILE.BAK */
          AbsUnlink (szBuffer);

          /* Rename WINFILE.INI to WINFILE.BAK */
          fWinfileIniAttr = GetFileAttributes (szIniPath);
          if (fWinfileIniAttr != -1)
            SetFileAttributes (szIniPath, fWinfileIniAttr & 0xF8);
          rename (szIniPath, szBuffer);
          if (fWinfileBakAttr != -1)
            SetFileAttributes (szIniPath, fWinfileBakAttr);

          /* Rename __WFI__.$$$ to WINFILE.INI */
          rename (szTmpPath, szIniPath);

          /* Make the new WINFILE.INI's file attributes match the original's */
          if (fWinfileIniAttr != -1)
            SetFileAttributes (szBuffer, fWinfileIniAttr);
        }
    }

  FreeMemory (szIniPath);
  FreeMemory (szTmpPath);
  FreeMemory (szDllPath);
  FreeMemory (szBuffer);
}


/***********************************************************************
 * AddSectionAndLine - Adds a section line, if the section does not
 *                     already exist, and a line to that section.
 *
 * Arguments: fReplace    - ADD_TO adds the line to the section,
 *                          REPLACE_LINE replaces the first matching item.
 *            pszSection  - Section name.  Ie, "[Settings]"
 *            pszLine     - Line to add.  Ie, "UNDELETE.DLL=MSTOOLS.DLL"
 *            szBuffer    - Buffer holding ASCIIZ strings.
 *            uCharCount  - Number of characters in the buffer.
 * Returns:   Void.
 **********************************************************************/

unsigned AddSectionAndLine (int      fReplace,
                            char     *pszSection,
                            char     *pszLine,
                            char     *szBuffer,
                            unsigned uCharCount)
{
  int      fSectionFound;       /* TRUE when section found                */
  int      fEndOfSectionFound;  /* TRUE when end of section found         */
  int      fLineAlreadyThere;   /* TRUE if our line was already added     */
  char     *sz;                 /* Points to current location             */
  char     *szSectionStart;     /* Locates the start of the section       */
  char     szLineType[MAX_SYSTEM_INI_LINE_LEN]; /* Stores "device=", etc. */


  /* Get the line type */
  if (fReplace == REPLACE_LINE_TYPE)
    {
      int u;   /* Index to szLineType */

      /* Skip whitespace */
      sz = pszLine;
      sz += strspn (sz, " \t");
      strcpy (szLineType, sz);

      /* Check for ' ', '\t', '=', and ';' */
      if ((u = strcspn (szLineType, " \t=;")) != 0)
        szLineType[u] = '\0';
    }


  fSectionFound = FALSE;
  sz = szBuffer;

  while (fSectionFound == FALSE && sz < &szBuffer[uCharCount])
    {
      /* Skip whitespace */
      sz += strspn (sz, " \t");

      /* Check for our section */
      if (strnicmp (sz, pszSection, strlen (pszSection)) == 0)
        fSectionFound = TRUE;

      sz += strlen (sz) + 1;
    }


  /* If the section was found, see if our line already exists */
  if (fSectionFound == TRUE)
    {
      szSectionStart = sz;

      fLineAlreadyThere  = FALSE;
      fEndOfSectionFound = FALSE;

      while (fEndOfSectionFound == FALSE && sz < &szBuffer[uCharCount])
        {
          /* Skip whitespace */
          sz += strspn (sz, " \t");

          /* Check for a new section */
          if (*sz == '[')
            fEndOfSectionFound == TRUE;
          else
            {
              /* Check for an exact duplicate */
              if (stricmp (sz, pszLine) == 0)
                {
                  fLineAlreadyThere = TRUE;
                  break;
                }
              else if (fReplace == REPLACE_LINE_TYPE &&
                       strnicmp (sz, szLineType, strlen (szLineType)) == 0)
                {
                  char szString[MAX_SYSTEM_INI_LINE_LEN];


                  /* Create the commented out the line */
                  strcpy (szString, gszRemovalComment);
                  strcat (szString, sz);

                  /* Remove the original line */
                  uCharCount = MyDeleteLine (szBuffer, uCharCount, sz);

                  /* Insert the commented out line */
                  uCharCount = MyInsertLine (szString, szBuffer,
                                             uCharCount, sz);

                  /* Break out */
                  break;
                }
            }

          /* Point to the next line */
          sz += strlen (sz) + 1;
        }

      /* Add the line if it wasn't there already */
      if (fLineAlreadyThere == FALSE)
        {
          sz = szSectionStart;

          uCharCount = MyInsertLine (gszCommentBegin, szBuffer, uCharCount, sz);
          sz += strlen (sz) + 1;

          uCharCount = MyInsertLine (pszLine, szBuffer, uCharCount, sz);
          sz += strlen (sz) + 1;

          uCharCount = MyInsertLine (gszCommentEnd, szBuffer, uCharCount, sz);
          sz += strlen (sz) + 1;
        }
    }
  else  /* The section was not found */
    {
      /* Add the section and our line */

      sz = &szBuffer[uCharCount];

      /* Blank line */
      uCharCount = MyInsertLine ("", szBuffer, uCharCount, sz);
      sz += strlen (sz) + 1;

      uCharCount = MyInsertLine (gszCommentBegin, szBuffer, uCharCount, sz);
      sz += strlen (sz) + 1;

      /* Section string */
      uCharCount = MyInsertLine (pszSection, szBuffer, uCharCount, sz);
      sz += strlen (sz) + 1;

      /* Actual line */
      uCharCount = MyInsertLine (pszLine, szBuffer, uCharCount, sz);
      sz += strlen (sz) + 1;

      uCharCount = MyInsertLine (gszCommentEnd, szBuffer, uCharCount, sz);
      sz += strlen (sz) + 1;
    }

  /* Return the new character count */
  return (uCharCount);
}


/***********************************************************************
 * MyDeleteLine - Removes a line from the buffer, and moves the rest of
 *                the buffer down to fill in the empty space.
 *
 * Arguments: szBuffer   - Points to the start of the buffer.
 *            uCharCount - Total number of characters in the buffer.
 *            sz         - Points to line to be deleted.
 * Returns:   Void.
 **********************************************************************/

unsigned MyDeleteLine (char *szBuffer, unsigned uCharCount, char *sz)
{
  unsigned uLen;        /* sz string length plus 1 */
  unsigned uMoveCount;  /* Number of bytes to move */
  char *szAfter;        /* Point to the line after this line */


  uLen = strlen (sz) + 1;

  /* Point to the character beyond the line to be deleted */
  szAfter = sz + uLen;

  /* Calculate the number of bytes to move */
  uMoveCount = uCharCount - (szAfter - szBuffer);

  /* Pull everything over the line to be deleted */
  if (uMoveCount)
    memmove (sz, szAfter, uMoveCount);

  /* Return the new character count */
  return (uCharCount - uLen);
}


/***********************************************************************
 * MyInsertLine - Removes a line from the buffer, and moves the rest of
 *                the buffer down to fill in the empty space.
 *
 * Arguments: szNewLine  - Line to be added to the buffer.
 *            szBuffer   - Points to the start of the buffer.
 *            uCharCount - Total number of characters in the buffer.
 *            sz         - Points to line to be deleted.
 * Returns:   Void.
 **********************************************************************/

unsigned MyInsertLine (char     *szNewLine,
                       char     *szBuffer,
                       unsigned uCharCount,
                       char     *sz)
{
  unsigned uLen;        /* szNewLine string length plus 1 */
  unsigned uMoveCount;  /* Number of bytes to move */
  char *szNew;          /* Point to the new location of the lines */
                        /*   that will be after the new line */


  uLen = strlen (szNewLine) + 1;

  /* Point to the character beyond where the new line will be inserted */
  szNew = sz + uLen;

  /* Calculate the number of bytes to move */
  uMoveCount = uCharCount - (sz - szBuffer);

  /* Move everything out of the way */
  if (uMoveCount)
    memmove (szNew, sz, uMoveCount);

  /* Put the line into place */
  strcpy (sz, szNewLine);

  /* Return the new character count */
  return (uCharCount + uLen);
}


/* int myfnSearchForOldWin3 (char *szWinPath, char *szFile1, char *szFile2,
 *                         char *szFile3);
 *
 * Function will search the given path for the file(s) defined by szFile1,
 *     szFile2, and szFile3.
 *
 * ENTRY: szWinPath   - Pointer to buffer in which path will be returned.
 *        szFile1/2/3 - Files to search for -- files that are in a Windows
 *                      3.0+ directory.
 *
 * EXIT: int --> TRUE == File(s) found. FALSE == File(s) not found.
 */
int myfnSearchForOldWin3 (char *szWinPath, char *szFile1, char *szFile2,
                        char *szFile3)
{
   unsigned          Disks[26];
   unsigned          uNumDisks;
   register unsigned i;
   int               nRetVal = FALSE;  // Init to oldwin not found.
   int               nFoundType;

   /* Search rules:
    *
    *    1) Search path first.
    *    2) Search each local fixed disk entirely (depth-wise search).
    */

   /* Tell int 24h handler were searching in case our customer puts a
    * floppy on the path or joins a floppy.
    */
   gbPathSearch = TRUE;

   /* Search path. */
   _searchenv(szFile1, "PATH", szWinPath);

   if (szWinPath[0])
   {
      myfnTruncFN(szWinPath);

#ifdef JANUS
      if ( fnIsOs2Win(szWinPath) == TRUE )
          ;
      else
#endif

      /* Check to see if szFile2 exists */
      if (szFile2[0])
      {
          mycatpath (szWinPath, szFile2);

          if (access (szWinPath, 00))
              nRetVal = FALSE;
          else
              nRetVal = TRUE;

          myfnTruncFN(szWinPath);
      }

      /* Check to see if szFile3 exists */
      if (nRetVal && szFile3[0])
      {
          mycatpath (szWinPath, szFile3);

          if (access (szWinPath, 00))
              nRetVal = FALSE;
          else
              nRetVal = TRUE;

          myfnTruncFN(szWinPath);
      }
   }

   if (nRetVal)
   {
      /* Tell Int24h handler we're done searching. */
      gbPathSearch = FALSE;
      return nRetVal;
   }

   /* Search each local fixed disk entirely (depth-wise search). */

   uNumDisks = MyGetFixedDisks(Disks);

   for (i = 0; i < uNumDisks; i++)
   {
      szWinPath[0] = (char)(Disks[i] + 'A');
      szWinPath[1] = ':';
      szWinPath[2] = '\\';
      szWinPath[3] = '\0';
      if (nFoundType = myfnSearchDisk (szWinPath, szFile1, szFile2, szFile3))
      {
         myfnTruncFN (szWinPath);
         nRetVal = nFoundType;
         break;
      }
   }

   /* Tell Int24h handler we're done searching. */
   gbPathSearch = FALSE;
   return nRetVal;
}

/* int myfnSearchDisk (char *szPath, char *szFile1, char *szFile2,
 *                   char *szFile3);
 *
 * Function will search the given path for the file defined by szFile1 and
 * szFile2. The function will return the first location where a valid
 * szFile1 and szFile2 is found to exist.
 *
 * ENTRY: szPath      - path to be searched.
 *        szFile1/2/3 - Files to search for -- files that are in a Windows
 *                      3.0+ directory.
 *
 * EXIT: int --> TRUE  == Valid szFile1/2/3 found.
 *               FALSE == Valid szFile1/2/3 not found.
 */
int myfnSearchDisk (char *szPath, char *szFile1, char *szFile2,
                  char *szFile3)
{
   struct find_t MyFCB;     /* Need an FCB for findfirst / findnext. */

   /* Try to locate the file. */

   if (strlen(szPath) >= MAX_PATH_LEN)
   {
      myfnTruncFN(szPath);
      return FALSE;
   }
   mycatpath(szPath, szFile1);

   if (!access (szPath, 00))
   {
#ifdef JANUS
		if ( fnIsOs2Win(szPath) == TRUE )
			;
		else
#endif

      /* Check to see if szFile2 exists */
      if (szFile2[0])
      {
          myfnTruncFN(szPath);
          mycatpath (szPath, szFile2);

          if (!access (szPath, 00))
              return (TRUE);
      }

      /* Check to see if szFile3 exists */
      if (szFile3[0])
      {
          myfnTruncFN(szPath);
          mycatpath (szPath, szFile3);

          if (!access (szPath, 00))
              return (TRUE);
      }
   }

   myfnTruncFN(szPath);
   mycatpath(szPath, "*.*");
   if (_dos_findfirst (szPath, _A_SUBDIR, &MyFCB))
      return FALSE;
   else
   {
      myfnTruncFN(szPath);
      while (TRUE)
      {
         while ((MyFCB.name[0] == '.') || (! (MyFCB.attrib & _A_SUBDIR)))
         {
            if (_dos_findnext (&MyFCB))
            {
               myfnTruncFN(szPath);
               return FALSE;
            }
         }
         mycatpath(szPath, MyFCB.name);

         /* Recurse */
         if (myfnSearchDisk (szPath, szFile1, szFile2, szFile3))
            return TRUE;
         MyFCB.name[0] = '.';
      }
   }
}

/* void myfnTruncFN(PSTR);
 *
 * ENTRY: Pointer to string containing fully qualified file name.
 * EXIT:  Truncates the file name portion of the fully qualified filename.
 *
 * WARNING: Danger ! assumes NULL terminated string. Also assumes it can
 *          Write a NULL char into the buffer pointed to by the func arg.
 *
 */
void myfnTruncFN (char *szPathStr)
{
   register char   *szFile    = szPathStr;
   register char   *BeginChar = szPathStr;

   while ( *szFile != '\0' )   /* Seek end of string. */
      szFile = OFFSET (AnsiNext(szFile));

   /* Ok, seek back till we hit a path char or the begining of the string */

   while ((*szFile != '\\') && (*szFile != '/') && (szFile != BeginChar) && (*szFile != ':'))
      szFile = OFFSET (AnsiPrev(BeginChar,szFile));

   /*  DBCS Note, This is a path were parsing and according to the DBCS ferry, the only
    *  place the character ':' may appear in a path is as a drive seperator. Therefore,
    *  this code will work correctly when DBCS is turned on.
    */
   if (*szFile == ':' || (*(szFile-1) == ':') )
      *(szFile+1) = '\0';
   else
      *szFile = '\0';

}

/* BUGBUG: fnIsOs2Win does not appear necessary for DOSONLY */
#if 0
/* int fnIsOs2Win(char *szWinPath);
 *
 * Function will determine whether the path given by the function argument
 * points to an OS/2 version of windows. We make this determination
 * by looking for unique OS/2 windows files whose names are listed in
 * SETUP.INF.  We only require 1 of the files in the list to be present.
 *
 * ENTRY: szWinPath - Pointer to possible windows path.
 *
 * EXIT: Boolean - TRUE == OS/2 windows, FALSE == Not OS/2 windows.
 *
 *
 */
int fnIsOs2Win(char *szWinPath)
 {
    int   bRet = FALSE;
    int  	iPathLen;
	int	i;
	int	iLines;
	PINF  pinfSect;
	PINF  pinfLine;
	char	szPath[MAX_INF_LINE_LEN];

	if ( (pinfSect = infFindSection( NULL, "winos2" )) == NULL ||
		  (iLines = infLineCount( pinfSect )) == 0 )
		return( bRet );

	pinfLine = pinfSect;

	_fstrcpy( szPath, szWinPath );
	strcat( szPath, "\\" );
   iPathLen = strlen(szPath);

	for ( i=0; i < iLines; i++ )
	{
		infParseField( pinfLine, 1, (LPSTR) &szPath[iPathLen],
							(MAX_INF_LINE_LEN - iPathLen) );

        if (access (szPath, 00))
		{
			bRet = TRUE;
			break;			/* return when first match is found */
		}

		pinfLine = infNextLine(pinfLine);
		szPath[iPathLen] = '\0';
	}

	return( bRet );
}
#else
int fnIsOs2Win(char *szWinPath)
{
    return (FALSE);
}
#endif


/*********************************************************************
 * MyGetFixedDisks - Stores the list of local fixed disks in the array
 *                 pointed to by piDriveLetters.
 *********************************************************************/

int MyGetFixedDisks (int *piDriveLetters)
{
  int i;                /* Looping variable */
  int iIndex = 0;       /* Index to aiDriveLetters array */
  char bLastDrive;      /* Last valid drive */
  char bCurrentDrive;   /* Last valid drive */

  /* Store the current drive number */
  bCurrentDrive = DosGetCurrentDrive();

  /* Determine the last valid drive to check */
  bLastDrive = DosSetCurrentDrive (bCurrentDrive);

  /* Begin counting up the valid drives */
  for (i = 0; i < bLastDrive; ++i)
    {
      if (ValidDrive    ((char) i)     &&
          IsLocalDrive  ((char) i + 1) &&
          !IsRemoveable ((char) i + 1))
        {
          piDriveLetters[iIndex] = i;
          ++iIndex;
        }
    }

  return (iIndex);
}


/*********************************************************************
 * ValidDrive - Determines if this drive number is a valid drive.
 *
 * bDrive - Drive letter to test.
 *
 * Returns:  TRUE if this is a valid drive number.  FALSE if not.
 *********************************************************************/

int ValidDrive (char bDrive)
{
  /* Attempt to make this drive number the current drive */
  DosSetCurrentDrive (bDrive);


  /* Return success or failure */
  if (DosGetCurrentDrive() == bDrive)
    return (TRUE);
  else
    return (FALSE);
}


/*********************************************************************
 * DosGetCurrentDrive - Obtain drive number of the current drive.
 *
 * Returns:  Current drive number.
 *********************************************************************/

char DosGetCurrentDrive (void)
{
  union REGS inregs, outregs;   /* Register structures for int86() */


  /* DOS call to return the current drive number */
  inregs.h.ah = 0x19;
  int86 (0x21, &inregs, &outregs);


  /* Return the current drive letter */
  return (outregs.h.al);
}


/*********************************************************************
 * DosSetCurrentDrive - Sets the current drive to the given drive
 *                      number.
 *
 * bDrive      - Drive number to set as the current drive.
 * wDosVersion - DOS Version (ie 3.20 == 320).
 *
 * Returns:  The highest potentially valid drive letter.  The highest
 *           of 5, LASTDRIVE in CONFIG.SYS, or the highest drive
 *           number in the system.
 *********************************************************************/

char DosSetCurrentDrive (char bDrive)
{
  union REGS inregs, outregs;   /* Register structures for int86() */


  /* DOS call to return the current drive number */
  inregs.h.ah = 0x0e;
  inregs.h.dl = bDrive;
  int86 (0x21, &inregs, &outregs);

  return (outregs.h.al);
}


void mycatpath (char *path, char *sz)
{
    //
    // Remove any drive letters from the directory to append
    //
    if ( sz[1] == ':' )
       sz+=2;

    //
    // Remove any current directories ".\" from directory to append
    //
    while (sz[0] == '.' && SLASH(sz[1]))
		  sz+=2;

	//
	// Remove leading slashes.
	//
	while (SLASH(*sz))
		sz++;

    //
    // Dont append a NULL string or a single "."
    //
    if (*sz && !(sz[0] == '.' && sz[1] == 0))
    {
       if ( (!SLASH(path[strlen(path)-1])) && ((path[strlen(path)-1]) != ':') )
          strcat(path,CHSEPSTR);
       strcat(path,sz);
    }
}


/**********************************************************************
 * GetFileAttributes - Gets a file's MS-DOS attributes
 *
 * pszFilename - Filename
 *
 * Returns: -1 on error.
 **********************************************************************/

int GetFileAttributes (char *pszPathname)
{
  union REGS inregs, outregs;   /* Register values for int86x */
  struct SREGS sregs;           /* Segment register values */
  char far * fszPathname = (char far *) pszPathname;

  inregs.x.ax = 0x4300;
  sregs.ds    = (unsigned) FP_SEG (fszPathname);
  inregs.x.dx = (unsigned) FP_OFF (fszPathname);

  int86x (0x21, &inregs, &outregs, &sregs);

  if (outregs.x.cflag)
    return (-1);
  else
    return (outregs.x.cx);
}


/**********************************************************************
 * SetFileAttributes - Gets a file's MS-DOS attributes
 *
 * pszFilename - Filename
 * iAttribute - New attribute for the file
 *
 * Returns: DOS Error number:
 **********************************************************************/

int SetFileAttributes (char *pszPathname, int iAttribute)
{
  union REGS inregs, outregs;   /* Register values for int86x */
  struct SREGS sregs;           /* Segment register values */
  char far * fszPathname = (char far *) pszPathname;

  inregs.x.ax = 0x4301;
  inregs.x.cx = iAttribute;
  sregs.ds    = (unsigned) FP_SEG (fszPathname);
  inregs.x.dx = (unsigned) FP_OFF (fszPathname);

  int86x (0x21, &inregs, &outregs, &sregs);

  if (outregs.x.cflag)
    return (outregs.x.ax);
  else
    return (0);
}

