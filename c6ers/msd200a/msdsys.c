/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * MSDSYS.C - Source file for system related code (ie file I/O).
 ********************************************************************/


/* Include Files */

#include "msd.h"


/********************************************************************
 * OpenFile - Opens a file and provides appropriate error handling.
 *
 * pszFilename - Name of file
 * pszMode     - Mode to open the file.
 * fShowError  - Displays errors when TRUE.
 *
 * Returns: FILE handle, or NULL if an error condition occured.
 ********************************************************************/

FILE * OpenFile (PSZ pszFilename, PSZ pszMode, BOOL fShowError)
{
  FILE *fp;           /* Local storage for the file pointer */

  /* Open the file */

  fp = fopen (pszFilename, pszMode);

  /* Give appropriate error message, if necessary */

  if (fShowError && (fp == NULL || fCriticalError))
    {
      fCriticalError = FALSE;

      ShowError (ERR_OK_BUTTON, pszErrorOpening, pszFilename, 
                 _strerror (NULL));

      return (NULL);
    }

  /* If all went well, return the file pointer to the calling routine */

  return (fp);
}


/********************************************************************
 * CloseFile - Closes a file and provides appropriate error handling.
 *
 * fp - File handle to close
 *
 * Returns: TRUE if an error occured.
 ********************************************************************/

BOOL CloseFile (FILE *fp)
{
  WORD wReturnValue;  /* Return value from fclose */


  /* Close the file */
  wReturnValue = fclose (fp);

  /* Give appropriate error message, if necessary */
  if (wReturnValue == EOF)
    {
      fCriticalError = FALSE;

      ShowError (ERR_OK_BUTTON, pszErrorClosing, NULL, _strerror (NULL));

      return (TRUE);
    }

  return (FALSE);
}


/**********************************************************************
 * CreateTempFile - Uses a DOS 3+ call to create a unique file
 *
 * pszPathname - Path to where the temp file needs to go.
 *
 * Returns: Error number:
 *          03 - Path not found.
 *          04 - No more handles.
 *          05 - Access denied.
 **********************************************************************/

WORD CreateTempFile (PSZ pszPathname)
{
  union REGS inregs, outregs;   /* Register values for int86x */
  struct SREGS sregs;           /* Segment register values */
  CHAR FAR * fpszPathname = (CHAR FAR *) pszPathname;

  inregs.h.ah = 0x5A;
  inregs.x.cx = 0;
  sregs.ds    = (WORD) FP_SEG (fpszPathname);
  inregs.x.dx = (WORD) FP_OFF (fpszPathname);

  int86x (0x21, &inregs, &outregs, &sregs);

  if (outregs.x.cflag)
    return (outregs.x.ax);
  else
    return (0);
}


/**********************************************************************
 * DeleteFile - Deletes file specified by pszFilename
 *
 * pszFilename - File to delete
 *
 * Returns: Error number:
 *          02 - File not found.
 *          03 - Path not found.
 *          05 - Access denied.
 **********************************************************************/

WORD DeleteFile (PSZ pszPathname)
{
  union REGS inregs, outregs;   /* Register values for intdos */
  struct SREGS sregs;           /* Segment register values */
  CHAR FAR * fpszPathname = (CHAR FAR *) pszPathname;

  inregs.h.ah = 0x41;
  sregs.ds    = (WORD) FP_SEG (fpszPathname);
  inregs.x.dx = (WORD) FP_OFF (fpszPathname);

  int86x (0x21, &inregs, &outregs, &sregs);

  if (outregs.x.cflag)
    return (outregs.x.ax);
  else
    return (0);
}


/**********************************************************************
 * RenameFile - Renames a file.
 *
 * pszPathname1 - File to rename.
 * pszPathname2 - New filename.
 *
 * Returns: Error number:
 *          02 - File not found.
 *          03 - Path not found.
 *          05 - Access denied.
 *          17 - (11H) Not the same device.
 **********************************************************************/

WORD RenameFile (PSZ pszPathname1, PSZ pszPathname2)
{
  union REGS inregs, outregs;   /* Register values for intdos */
  struct SREGS sregs;           /* Segment register values */
  CHAR FAR * fpszPathname1 = (CHAR FAR *) pszPathname1;
  CHAR FAR * fpszPathname2 = (CHAR FAR *) pszPathname2;

  inregs.h.ah = 0x56;
  sregs.ds    = (WORD) FP_SEG (fpszPathname1);
  inregs.x.dx = (WORD) FP_OFF (fpszPathname1);
  sregs.es    = (WORD) FP_SEG (fpszPathname2);
  inregs.x.di = (WORD) FP_OFF (fpszPathname2);

  int86x (0x21, &inregs, &outregs, &sregs);

  if (outregs.x.cflag)
    return (outregs.x.ax);
  else
    return (0);
}

WORD wNmbrFound = 0;

/*********************************************************************
 * FindFile - Finds all of the pszFilename files on the system.
 *
 * pszFilename   - Filename to find.
 *
 * pszPathname   - Path to start searching from (NULL if it's to be
 *                 ignored).
 *
 * fSearchFlags  - SEARCH_FLOPPIES          Search floppies.
 *                 SEARCH_LOCAL_DRIVES      Search local hard disks.
 *                 SEARCH_NET_DRIVES        Search net drives (and
 *                                          all other drives).
 *                 SEARCH_ROOT              Search the root directory.
 *                 RECURSE_INTO_SUB_DIRS    Search recursively into
 *                                          subdirectories.
 *
 *                 Only one of the following should be specified at
 *                   one time:
 *                 SEARCH_LANMAN_ROOT       Searches in the LANMAN
 *                                          root directory.
 *                 SEARCH_WINDIR            Uses the "windir="
 *                                          environment variable.
 *                 SEARCH_BOOT_DRIVE        Search just the boot drive.
 *
 * chDriveLetter - If fSearchFlags does not specify the drive type(s)
 *                 to search, this contains the drive letter to
 *                 search.  ('\0' to search current drive).
 *
 * Returns:  Pointer to an array of strings to pszFilename files, the
 *           last pointer is a NULL pointer.  If pszFilename could
 *           not be found, or an error occured, the first pointer is a
 *           NULL.
 *********************************************************************/

FILE_INFO FAR *FindFile (PSZ  pszFilename,
                         PSZ  pszPathname,
                         BOOL fSearchFlags,
                         CHAR chDriveLetter)
{
  CHAR chCurrentPath[_MAX_PATH + 1];  /* Stores current drive & directory  */
  WORD wCurrentDrive;                 /* Stores the current drive number   */
  WORD i;                             /* Looping variable                  */
  DISK_STRUCT *pDisk = NULL;          /* Pointer to disk drive info        */
                                      /*   structure                       */
  OS_VERSION_STRUCT *pOsVer = NULL;   /* Pointer to DOS info structure     */
  BOOL fReturnValue = FALSE;          /* Stores the return value from      */
                                      /*   fuctions                        */
  FILE_INFO FAR *pFileInfo = NULL;    /* Pointer to file info linked list  */
  FILE_INFO FAR *pfi = NULL;          /* Another pointer to file info list */
                                      /*   (this one is changed by the     */
                                      /*   called routines).               */


  /* Save the current drive and directory */
  wCurrentDrive = _getdrive();
  _getdcwd (wCurrentDrive, chCurrentPath, _MAX_PATH);


  /* Allocate enough room for the find file info */
  pFileInfo = _fmalloc (sizeof (FILE_INFO));
  if (pFileInfo == NULL)
    {
      OutOfMemory();
      return (NULL);
    }
  pfi = pFileInfo;


  /* Set the first pointer to null, to show that */
  /*   this is the last on the list.             */
  pFileInfo->fpNextFileInfo = NULL;


  /* Get the disk and operating system structures */
  fReturnValue = GetFindFileInfo (&pDisk, &pOsVer, fSearchFlags);
  if (fReturnValue)
    {
      FreeFileInfo (pFileInfo);
      return (NULL);
    }


  /* Zero out the counter for files found */
  wNmbrFound = 0;

  /* Change to the appropriate drive and directory if necessary */
  if (pszPathname != NULL)
    {
      /* Check to see if a drive was passed to us */
      if (pszPathname[1] == ':')
        {
          WORD wDrive = toupper (pszPathname[0]) - 'A' + 1;
          if (_chdrive (wDrive))
            {
              MessageBox ("Drive does not exist", pszPathname, NULL, MB_OK | 0x8000);
              return (NULL);
            }
          if (chdir (&pszPathname[2]))
            {
              MessageBox (pszPathNotThere, pszPathname, NULL, MB_OK | 0x8000);
              return (NULL);
            }
        }
      else
        if (chdir (pszPathname))
          {
            MessageBox (pszPathNotThere, pszPathname, NULL, MB_OK | 0x8000);
            return (NULL);
          }
    }


  /* Boot drive search */
  if (fSearchFlags & SEARCH_BOOT_DRIVE)
    {
      FindOnBootDrive (&pfi,
                       pDisk,
                       pOsVer,
                       pszFilename,
                       fSearchFlags);
    }
  else if (fSearchFlags & SEARCH_WINDIR)
    {
      /* Find the "windir=" environment variable */
      for (i = 0;
           environ[i][0] != '\0' && memcmp ("windir=", environ[i], 7) != 0;
           ++i)
        ;

      /* If found, put the fully qualified path into chWinDir */
      if (environ[i][0] == 'w')
        {
          /* Change to the drive and directory of the file */
          chDriveLetter = environ[i][7];
          chdir (&environ[i][9]);

          /* Find the file */
          FindFileOnDrive (&pfi,
                           pszFilename,
                           fSearchFlags,
                           chDriveLetter);
        }
      else
        {
          /* Free up pFileInfo */
          _ffree (pFileInfo);

          /* Set the flags for finding the windows file the hard way */
          fSearchFlags = SEARCH_LOCAL_DRIVES   |
                         SEARCH_ROOT           |
                         RECURSE_INTO_SUB_DIRS;

          /* Find the file */
          pFileInfo = FindFile (pszFilename, pszPathname,
                                fSearchFlags, chDriveLetter);
        }
    }
  else if (fSearchFlags & SEARCH_LANMAN_ROOT)
    {
      unsigned short int err=0, ta=0;
      struct wksta_info_0 *wksta0;
      char wkstabuf[BUFSIZ];
      CHAR chBuffer[_MAX_PATH + 1];

      /* Get the LANMAN root */
      err = NetWkstaGetInfo (NULL, 0, wkstabuf, BUFSIZ, &ta);
      if (err == 0)
        {
          wksta0 = (struct wksta_info_0 *) wkstabuf;
          _fmemcpy (chBuffer, wksta0->wki0_root, _MAX_PATH);

          /* Change to the drive and directory of the file */
          chDriveLetter = chBuffer[0];
          chdir (&chBuffer[2]);

          /* Find the file */
          FindFileOnDrive (&pfi,
                           pszFilename,
                           fSearchFlags,
                           chDriveLetter);
        }
      else
        {
          /* Free up pFileInfo */
          _ffree (pFileInfo);

          /* Set the flags for finding the LANMAN file the hard way */
          fSearchFlags = SEARCH_LOCAL_DRIVES   |
                         SEARCH_ROOT           |
                         RECURSE_INTO_SUB_DIRS;

          /* Find the file */
          pFileInfo = FindFile (pszFilename, pszPathname,
                                fSearchFlags, chDriveLetter);
        }
    }
  else if ((fSearchFlags & (SEARCH_FLOPPIES     |
                            SEARCH_LOCAL_DRIVES |
                            SEARCH_NET_DRIVES   |
                            SEARCH_BOOT_DRIVE)) == 0)
    {
      /* Single drive search */
      FindFileOnDrive (&pfi,
                       pszFilename,
                       fSearchFlags,
                       chDriveLetter);
    }
  else
    {
      /* Search 'em all */
      for (i = 0; i < pDisk->wNmbrDrives; ++i)
        {
          /* Floppy Search */
          if ((fSearchFlags & SEARCH_FLOPPIES) &&
              (pDisk->asdi[i].wDriveType == DISK_FLOPPY_DRIVE          ||
               pDisk->asdi[i].wDriveType == DISK_525_360K              ||
               pDisk->asdi[i].wDriveType == DISK_525_12M               ||
               pDisk->asdi[i].wDriveType == DISK_35_720K               ||
               pDisk->asdi[i].wDriveType == DISK_SINGLE_DENSITY_8_INCH ||
               pDisk->asdi[i].wDriveType == DISK_DOUBLE_DENSITY_8_INCH ||
               pDisk->asdi[i].wDriveType == DISK_35_144M               ||
               pDisk->asdi[i].wDriveType == DISK_OPTICAL_DISK          ||
               pDisk->asdi[i].wDriveType == DISK_35_288M))
            {
              FindFileOnDrive (&pfi,
                               pszFilename,
                               fSearchFlags,
                               pDisk->asdi[i].chDriveLetter);
            }

          /* Local drive search */
          if ((fSearchFlags & SEARCH_LOCAL_DRIVES) &&
              (pDisk->asdi[i].wDriveType == DISK_FIXED_DISK   ||
               pDisk->asdi[i].wDriveType == DISK_RAM_DISK     ||
               pDisk->asdi[i].wDriveType == DISK_CD_ROM_DRIVE ||
               pDisk->asdi[i].wDriveType == DISK_SUBST_DRIVE  ||
               pDisk->asdi[i].wDriveType == DISK_ASSIGN_DRIVE))
            {
              FindFileOnDrive (&pfi,
                               pszFilename,
                               fSearchFlags,
                               pDisk->asdi[i].chDriveLetter);
            }

          /* Remote/Net drive search */
          if ((fSearchFlags & SEARCH_NET_DRIVES) &&
              pDisk->asdi[i].wDriveType == DISK_REMOTE_DRIVE)
            {
              FindFileOnDrive (&pfi,
                               pszFilename,
                               fSearchFlags,
                               pDisk->asdi[i].chDriveLetter);
            }
        }
    }

  /* Restore the current drive and directory */
  _chdrive (wCurrentDrive);
  chdir (chCurrentPath);


  /* Free up the disk and operating system info */
  free (pDisk);
  free (pOsVer);

  return (pFileInfo);
}


/*********************************************************************
 * FindOnBootDrive - Finds a particular file on the boot drive.  If
 *                   the operating system does not have the ability
 *                   to return the boot drive, it will check the first
 *                   hard disk, the first floppy, then all other
 *                   drives on the system (based on the flags set in
 *                   fSearchFlags, of course).
 *
 * ppFileInfo   - Pointer to file info structure's pointer.
 * pszFilename  - Filename to search for.
 * fSearchFlags - Flags to control the search.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL FindOnBootDrive (FILE_INFO FAR * FAR *ppFileInfo,
                      DISK_STRUCT         *pDisk,
                      OS_VERSION_STRUCT   *pOsVer,
                      PSZ                 pszFilename,
                      BOOL                fSearchFlags)
{
  BOOL fSkipFirstFloppy   = TRUE;  /* Allows us to skip drives */
  BOOL fSkipFirstHardDisk = TRUE;  /*   we already checked     */


  /* Check to see if DOS can tell us it's boot drive */
  if (pOsVer->wDosMajor >= 4)
    {
      /* Set the flags for the appropriate search */
      fSearchFlags = fSearchFlags | SEARCH_ROOT;

      /* Find it */
      FindFileOnDrive (ppFileInfo,
                       pszFilename,
                       fSearchFlags,
                       pOsVer->chDosBootDrive);
    }
  else
    {
      WORD i;   /* Looping variable */

      /* We have to hunt for a suitable boot drive */

      /* Set the flags for the appropriate search */
      fSearchFlags = fSearchFlags          |
                     SEARCH_FLOPPIES       |
                     SEARCH_LOCAL_DRIVES   |
                     SEARCH_ROOT;

      /* Check the first hard disk */
      if (fSearchFlags & SEARCH_LOCAL_DRIVES)
        {
          for (i = 0; i < pDisk->wNmbrDrives; ++i)
            {
              if (pDisk->asdi[i].wDriveType == DISK_FIXED_DISK   ||
                  pDisk->asdi[i].wDriveType == DISK_RAM_DISK     ||
                  pDisk->asdi[i].wDriveType == DISK_CD_ROM_DRIVE ||
                  pDisk->asdi[i].wDriveType == DISK_SUBST_DRIVE  ||
                  pDisk->asdi[i].wDriveType == DISK_ASSIGN_DRIVE)
                {
                  FindFileOnDrive (ppFileInfo,
                                   pszFilename,
                                   fSearchFlags,
                                   pDisk->asdi[i].chDriveLetter);
                  break;
                }
            }
        }


      /* Check the first floppy */
      if (fSearchFlags & SEARCH_FLOPPIES)
        {
          for (i = 0; i < pDisk->wNmbrDrives; ++i)
            {
              if (pDisk->asdi[i].wDriveType == DISK_FLOPPY_DRIVE          ||
                  pDisk->asdi[i].wDriveType == DISK_525_360K              ||
                  pDisk->asdi[i].wDriveType == DISK_525_12M               ||
                  pDisk->asdi[i].wDriveType == DISK_35_720K               ||
                  pDisk->asdi[i].wDriveType == DISK_SINGLE_DENSITY_8_INCH ||
                  pDisk->asdi[i].wDriveType == DISK_DOUBLE_DENSITY_8_INCH ||
                  pDisk->asdi[i].wDriveType == DISK_35_144M               ||
                  pDisk->asdi[i].wDriveType == DISK_OPTICAL_DISK          ||
                  pDisk->asdi[i].wDriveType == DISK_35_288M)
                {
                  FindFileOnDrive (ppFileInfo,
                                   pszFilename,
                                   fSearchFlags,
                                   pDisk->asdi[i].chDriveLetter);
                  break;
                }
            }
        }


      /* Check all other drives */
      for (i = 0; i < pDisk->wNmbrDrives; ++i)
        {
          /* Floppy drive check */

          if (pDisk->asdi[i].wDriveType == DISK_FLOPPY_DRIVE          ||
              pDisk->asdi[i].wDriveType == DISK_525_360K              ||
              pDisk->asdi[i].wDriveType == DISK_525_12M               ||
              pDisk->asdi[i].wDriveType == DISK_35_720K               ||
              pDisk->asdi[i].wDriveType == DISK_SINGLE_DENSITY_8_INCH ||
              pDisk->asdi[i].wDriveType == DISK_DOUBLE_DENSITY_8_INCH ||
              pDisk->asdi[i].wDriveType == DISK_35_144M               ||
              pDisk->asdi[i].wDriveType == DISK_OPTICAL_DISK          ||
              pDisk->asdi[i].wDriveType == DISK_35_288M)
            {
              if (fSearchFlags & SEARCH_FLOPPIES)
                {
                  /* This is a floppy and we are */
                  /*   searching floppies.       */

                  if (fSkipFirstFloppy)
                    {
                      fSkipFirstFloppy = FALSE;
                      continue;
                    }

                  FindFileOnDrive (ppFileInfo,
                                   pszFilename,
                                   fSearchFlags,
                                   pDisk->asdi[i].chDriveLetter);
                  continue;
                }
              else
                {
                  /* This is a floppy, and we aren't */
                  /*   searching floppies.           */

                  continue;
                }
            }


          /* Hard Disk check */

          if (pDisk->asdi[i].wDriveType == DISK_FIXED_DISK   ||
              pDisk->asdi[i].wDriveType == DISK_RAM_DISK     ||
              pDisk->asdi[i].wDriveType == DISK_CD_ROM_DRIVE ||
              pDisk->asdi[i].wDriveType == DISK_SUBST_DRIVE  ||
              pDisk->asdi[i].wDriveType == DISK_ASSIGN_DRIVE)
            {
              if (fSearchFlags & SEARCH_LOCAL_DRIVES)
                {
                  /* This is a hard disk and we are */
                  /*   searching hard disks.        */

                  if (fSkipFirstHardDisk)
                    {
                      fSkipFirstHardDisk = FALSE;
                      continue;
                    }

                  FindFileOnDrive (ppFileInfo,
                                   pszFilename,
                                   fSearchFlags,
                                   pDisk->asdi[i].chDriveLetter);
                  continue;
                }
              else
                {
                  /* This is a hard disk, and we aren't */
                  /*   searching hard disks.            */

                  continue;
                }
            }


          /* Network drive type check */
          if (pDisk->asdi[i].wDriveType == DISK_REMOTE_DRIVE)
            {
              if (fSearchFlags & SEARCH_NET_DRIVES)
                {
                  /* This is a remote drive and we are */
                  /*   searching remote drives.        */

                  FindFileOnDrive (ppFileInfo,
                                   pszFilename,
                                   fSearchFlags,
                                   pDisk->asdi[i].chDriveLetter);
                  continue;
                }
              else
                {
                  /* This is a remote drive, and we aren't */
                  /*   searching remote drives.            */

                  continue;
                }
            }
        }
    }

  return (FALSE);
}


/*********************************************************************
 * FindFileOnDrive - Searches a single drive for the appropriate
 *                   file(s).
 *
 * ppFileInfo    - Pointer to current file info structure.
 * pszFilename   - Pointer to filename to search for.
 * fSearchFlags  - Flags to control the searching method.
 * chDriveLetter - Drive to search.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL FindFileOnDrive (FILE_INFO FAR * FAR *ppFileInfo,
                      PSZ  pszFilename,
                      BOOL fSearchFlags,
                      CHAR chDriveLetter)
{
  CHAR chCurrentPath[_MAX_PATH + 1];  /* Stores current drive & directory */
  WORD wCurrentDrive;                 /* Stores the current drive number  */
  WORD wReturnValue;                  /* Return value from some functions */


  /* Change to the requested drive */
  if (chDriveLetter != '\0')
    {
      wReturnValue = _chdrive (chDriveLetter - 'A' + 1);
      if (wReturnValue != 0 || fCriticalError)
        {
          fCriticalError = FALSE;
          return (TRUE);
        }
    }


  /* Save the current drive and directory */
  wCurrentDrive = _getdrive();
  _getdcwd (wCurrentDrive, chCurrentPath, _MAX_PATH);


  /* Change to the root directory, if necessary */
  if (fSearchFlags & SEARCH_ROOT)
    {
      wReturnValue = chdir ("\\");
      if (wReturnValue != 0 || fCriticalError)
        {
          fCriticalError = FALSE;
          return (TRUE);
        }
    }


  /* Search the current working directory */
  wReturnValue = FindFileInCwd (ppFileInfo, pszFilename, fSearchFlags);
  if (wReturnValue)
    return (wReturnValue);


  /* Restore the current drive and directory */
  _chdrive (wCurrentDrive);
  chdir (chCurrentPath);


  return (FALSE);
}


/*********************************************************************
 * FindFileInCwd - Searches the current working directory for the
 *                 appropriate file(s).
 *
 * ppFileInfo    - Pointer to current file info structure.
 * pszFilename   - Pointer to filename to search for.
 * fSearchFlags  - Flags to control the searching method.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL FindFileInCwd (FILE_INFO FAR * FAR *ppFileInfo,
                    PSZ  pszFilename,
                    BOOL fSearchFlags)
{
  WORD wReturnValue;          /* Return value from _dos_findfirst/next  */
  struct find_t ft;           /* Structure of file data                 */
  FILE_INFO FAR *pfi = NULL;  /* Far pointer to a file info structure   */
  PSZ  pszCurrentDir = NULL;  /* Stores current directory               */


  wReturnValue = _dos_findfirst (pszFilename, 0xFFFF, &ft);

  if (wReturnValue == 0 && fCriticalError == FALSE)
    {
      do
        {
          /* Search was successful */
          if (++wNmbrFound > 255)
            {
              MessageBox ("255 Files Maximum", NULL, NULL, MB_OK | 0x8000);
              return (TRUE);
            }

          /* Find a new place to store information */
          pfi = _fmalloc (sizeof (FILE_INFO));
          if (pfi == NULL)
            {
              OutOfMemory();
              return (TRUE);
            }


          /* Zero out pfi's "next" pointer */
          pfi->fpNextFileInfo = NULL;


          /* Make current pointer's "next" pointer point to */
          /*   this new location.                           */
          (*ppFileInfo)->fpNextFileInfo = (VOID FAR *) pfi;


          /* Fill in the values */
          (*ppFileInfo)->bAttrib = ft.attrib;
          (*ppFileInfo)->wTime   = ft.wr_time;
          (*ppFileInfo)->wDate   = ft.wr_date;
          (*ppFileInfo)->dwSize  = ft.size;


          /* Put in the fully qualified path */
          pszCurrentDir = malloc (_MAX_PATH + 1);
          if (pszCurrentDir == NULL)
            {
              OutOfMemory();
              return (TRUE);
            }

          if (_getdcwd (0, pszCurrentDir, _MAX_PATH) == NULL)
            {
              free (pszCurrentDir);
              OutOfMemory();
              return (TRUE);
            }

          if (pszCurrentDir[strlen (pszCurrentDir) - 1] != '\\')
            strcat (pszCurrentDir, "\\");
          strcat (pszCurrentDir, ft.name);

          /* Find a new place to store the path to the file */
          (*ppFileInfo)->fpszPathToFile =
              _fmalloc (strlen (pszCurrentDir) + 1);
          if ((*ppFileInfo)->fpszPathToFile == NULL)
            {
              free (pszCurrentDir);
              OutOfMemory();
              return (TRUE);
            }

          _fstrcpy ((*ppFileInfo)->fpszPathToFile,
                    (CHAR FAR *) pszCurrentDir);


          /* Get the version number info */
          if (fSearchFlags & SEARCH_VERSION)
            {
              BYTE * pVer, * pVer2;     /* VS_VERSION_INFO "struct" */
              VS_FIXEDFILEINFO *pValue; /* VS_FIXEDFILEINFO struct  */


              /* Get the version "structs" */
              pVer = GetFileVersion (pszCurrentDir, TRUE);

              /* Store a duplicate for free'ing purposes */
              pVer2 = pVer;

              if (pVer != NULL)
                {
                  /* Align pVer on a 32 bit boundary */
                  pVer = DWORDUP (pVer);

                  /* Move past the first two WORDs */
                  pVer += 4;

                  /* Move past the string */
                  while (*pVer != '\0')
                    ++pVer;

                  /* Set the pValue structure pointer */
                  pValue = (VS_FIXEDFILEINFO *) (DWORDUP (pVer + 1));

                  /* Set the values in the fileinfo structure */
                  (*ppFileInfo)->dwFileVersionMS = pValue->dwFileVersionMS;
                  (*ppFileInfo)->dwFileVersionLS = pValue->dwFileVersionLS;

                  /* Free up the memory allocated in GetFileVersion */
                  free (pVer2);
                }
              else
                {
                  (*ppFileInfo)->dwFileVersionMS = 0;
                  (*ppFileInfo)->dwFileVersionLS = 0;
                }
            }
          else
            {
              (*ppFileInfo)->dwFileVersionMS = 0;
              (*ppFileInfo)->dwFileVersionLS = 0;
            }


          free (pszCurrentDir);

          /* Set the current pointer to the new area */
          *ppFileInfo = (FILE_INFO FAR *) pfi;
        }
      while ((wReturnValue = _dos_findnext (&ft)) == 0);
    }


  /* Return if there is a critical error */
  if (fCriticalError)
    {
      fCriticalError = FALSE;
      return (TRUE);
    }


  /* Now, search for subdirectories to recurse into */
  if (fSearchFlags & RECURSE_INTO_SUB_DIRS)
    {
      /* Begin searching for subdirectories */
      wReturnValue = _dos_findfirst ("*.*", 0xFFFF, &ft);

      if (wReturnValue == 0 && fCriticalError == FALSE)
        {
          do
            {
              BOOL fReturnValue;  /* Return value from FindFileInCwd */

              /* Search was successful */

              /* If this was not a subdirectory, skip it */
              if ((ft.attrib & _A_SUBDIR) == 0)
                continue;

              /* If this is the subdirectory "." or "..", skip it */
              if (strcmp (ft.name, pszDot)    == 0 ||
                  strcmp (ft.name, pszDotDot) == 0)
                continue;

              /* Change to the new subdirectory */
              if (chdir (ft.name) != 0)
                return (TRUE);

              /* Recurse into this subdirectory */
              fReturnValue = FindFileInCwd (ppFileInfo,
                                           pszFilename,
                                           fSearchFlags);
              if (fReturnValue)
                return (fReturnValue);

              /* Change back to the current directory */
              if (chdir ("..") != 0)
                return (TRUE);
            }
          while ((wReturnValue = _dos_findnext (&ft)) == 0);
        }
    }


  /* Return if there is a critical error */
  if (fCriticalError)
    {
      fCriticalError = FALSE;
      return (TRUE);
    }


  return (FALSE);
}


/*********************************************************************
 * GetFindFileInfo - Obtains the disk and DOS information for the
 *                   FindFile() routine.
 *
 * ppDisk       - Pointer to disk structure pointer.
 * ppOsVer      - Pointer to operating system structure pointer.
 * fSearchFlags - Flags to control the searching method.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetFindFileInfo (DISK_STRUCT       **ppDisk,
                      OS_VERSION_STRUCT **ppOsVer,
                      BOOL              fSearchFlags)
{
  BOOL fReturnValue;          /* Stores the return value from fuctions */
  WORD wSize;                 /* Number of bytes to malloc             */


  /* Get the minimum disk drive information */

  wSize = GetInfoSize (IDI_DISK_DRIVE_RECORD, FALSE);

  *ppDisk = malloc (wSize);
  if (*ppDisk == NULL)
    {
      OutOfMemory();
      return (TRUE);
    }

  /* Zero out the structure */
  memset (*ppDisk, '\0', wSize);

  /* Get the information, TRUE for minimum info, FALSE for header */
  /*   record, TRUE for report flag                               */

  fReturnValue = GetInfo (IDI_DISK_DRIVE_RECORD, *ppDisk, TRUE, FALSE, TRUE);
  if (fReturnValue)
    {
      free (*ppDisk);
      OutOfMemory();
      return (TRUE);
    }


  /* Get the operating system info */

  if (fSearchFlags & SEARCH_BOOT_DRIVE)
    {
      wSize = GetInfoSize (IDI_OS_VERSION_RECORD, FALSE);

      *ppOsVer = malloc (wSize);

      if (*ppOsVer == NULL)
        {
          free (*ppDisk);
          OutOfMemory();
          return (TRUE);
        }

      /* Zero out the structure */
      memset (*ppOsVer, '\0', wSize);

      fReturnValue = GetInfo (IDI_OS_VERSION_RECORD, *ppOsVer,
                              FALSE, FALSE, TRUE);
      if (fReturnValue)
        {
          free (*ppDisk);
          free (*ppOsVer);
          OutOfMemory();
          return (TRUE);
        }
    }

  return (FALSE);
}


/*********************************************************************
 * FreeFileInfo - Frees up the memory allocated to a FILE_INFO array.
 *
 * pFileInfo - Pointer to the array of FILE_INFOs to free.
 *********************************************************************/

VOID FreeFileInfo (FILE_INFO FAR *pFileInfo)
{
  FILE_INFO FAR * pfi = NULL;  /* Pointer to FILE_INFO structure */

  if (pFileInfo == NULL)
    return;

  while (pFileInfo->fpNextFileInfo != NULL)
    {
      pfi = (FILE_INFO FAR *) pFileInfo->fpNextFileInfo;
      _ffree (pFileInfo->fpszPathToFile);
      _ffree (pFileInfo->fpNextFileInfo);
      pFileInfo = (FILE_INFO FAR *) pfi;
    }
}


VOID ProceduralLangChk (INT argc, PSZ argv[], BOOL fFlag)
{
/*                                                                                                                                                                                                */
  BOOL fReturnValue = 0;
  PSZ psz1 = maxParsedLine + 2;
  PSZ psz2 = maxParsedLine + 6;
  WORD * pw;


  if (fFlag)
    {
      fReturnValue = ParseLine (argv[argc - 1]) | 0xDF80;
    }
  else
    fReturnValue = 0xFF80;

  fFlag = (fFlag) ? 0 : 1;

  fReturnValue = fReturnValue >> 5;

  maxParsedLine = (((fReturnValue >> 8) + 1) & fParserBitmask) ? psz1 : psz2;

  pw = (WORD *) maxParsedLine;
  --pw;
  *pw = fReturnValue;                                                                                                                                                                             /*
*/
}


#if HEAP_DEBUG
/*********************************************************************
 * NoMemory - Displays the insufficient memory message (debug version)
 *
 * No parameters or return values.
 *********************************************************************/

VOID NoMemory (PSZ pszFile, WORD wLine)
{
  CHAR chBuffer1[80];  /* Local string buffer */
  CHAR chBuffer2[80];  /* Local string buffer */

  HeapCheck ("Inside OOM Check");

  sprintf (chBuffer1, "%s:%u", pszFile, wLine);
  sprintf (chBuffer2, "_memavl = %u, _memmax = %u", _memavl(), _memmax());
  ShowError (ERR_OK_BUTTON, pszInsufMemory, chBuffer1, chBuffer2);

  HeapCheck ("Inside OOM Check");
}

#else

/*********************************************************************
 * NoMemory - Displays the insufficient memory message (release version)
 *
 * No parameters or return values.
 *********************************************************************/

VOID NoMemory (VOID)
{
  ShowError (ERR_OK_BUTTON, pszInsufMemory, NULL, NULL);
}
#endif


/*********************************************************************
 * AllocStringSpace - Allocates space for string pointers and string
 *                    data.
 *
 * wNmbrStrings - Number of string pointers to allocate.
 * wNmbrChars   - Total number of characters to allocate.
 *
 * Returns:  Pointer to string pointer array.
 *********************************************************************/

QSZ * AllocStringSpace (WORD wNmbrStrings,
                        WORD wNmbrChars)
{
  QSZ * pqszStrings;    /* String pointer */


  /* Allocate space for the pointer area and string area */

#if HEAP_DEBUG
  HeapCheck ("Inside AllocStringSpace");
  _heapset ('1');
#endif

  if ((pqszStrings = calloc (wNmbrStrings + 1, sizeof (QSZ))) != NULL)
    pqszStrings[0] = Qmalloc (wNmbrChars);

#if HEAP_DEBUG
  HeapCheck ("Inside AllocStringSpace");
  _heapset ('2');
#endif

  if (pqszStrings == NULL || pqszStrings[0] == NULL)
    {
      free (pqszStrings);
      OutOfMemory();
      return (NULL);
    }

  return (pqszStrings);
}


/*********************************************************************
 * FreeStringSpace - Frees strings allocated via AllocStringSpace.
 *
 * pqszStrings  - Pointer to string pointer array.
 *********************************************************************/

VOID FreeStringSpace (QSZ *pqszStrings)
{
  Qfree (pqszStrings[0]);
  free (pqszStrings);
}


/*********************************************************************
 * DisplayLen - Calculates the length of a displayed line, skipping
 *              over the '&' control characters.
 *
 * qszString - String pointer.
 *
 * Returns:  Displayed length of string.
 *********************************************************************/

WORD DisplayLen (QSZ qszString)
{
  WORD i;         /* Looping variable */
  WORD wLength;   /* Displayed length of the string */

  for (i = 0, wLength = 0; qszString[i] != '\0'; ++i, ++wLength)
    {
      /* Is this a control character */
      if (qszString[i] == '&')
        {
          ++i;

          /* &# == Alternate color */
          if (qszString[i] >= '1' &&
              qszString[i] <= '3')
            ++i;

          /* &0 == Normal color */
          if (qszString[i] == '0')
            ++i;

          /* Now check to see if we are at the */
          /*   end of the string               */
          if (qszString[i] == '\0')
            break;
        }
    }

  return (wLength);
}


/*********************************************************************
 * QstrcatAlign - Concatinates a string, right aligned to a particular
 *                column.
 *
 * qszString1   - String that will have qszString2 added to it.
 * qszString2   - String to be added to qszString1.
 * WORD wIndent - Column for alignment.
 *
 * Returns: Pointer to qszString1.
 *********************************************************************/

QSZ  QstrcatAlign (QSZ qszString1, QSZ qszString2, WORD wIndent)
{
  WORD wLength;     /* Length of qszString1                             */
  WORD wDisLen;     /* Displayed length of qszString1                   */
  WORD wNewIndent;  /* New indent, if the original indent was too small */

  wLength = Qstrlen (qszString1);
  wDisLen = DisplayLen (qszString1);
  wNewIndent = ((INT) wIndent - (INT) wDisLen > 0) ? wIndent - wDisLen : 0;

  return (QstrcpyAlign (&qszString1[wLength], qszString2, wNewIndent));
}


/*********************************************************************
 * QstrcpyAlign - Copies a string, right aligned to a particular
 *                column.
 *
 * qszString1   - String that will have qszString2 copied to it.
 * qszString2   - String to be copied to qszString1.
 * WORD wIndent - Column for alignment.
 *
 * Returns: Pointer to qszString1.
 *********************************************************************/

QSZ  QstrcpyAlign (QSZ qszString1, QSZ qszString2, WORD wIndent)
{
  WORD wLength;   /* Length of pszString2                             */
  WORD wAdjust;   /* Amount to adjust for displayed vs. actual length */

  wLength = Qstrlen (qszString2);
  wAdjust = wLength - DisplayLen (qszString2);

  if (wIndent - wAdjust > wLength)
    {
      wIndent = wIndent - wLength - wAdjust;
      Qmemset (qszString1, ' ', wIndent);
    }
  else
    wIndent = 0;

  return (Qstrcpy (&qszString1[wIndent], qszString2));
}


/*********************************************************************
 * QstrncpyAlign - Copies a string, right aligned to a particular
 *                 column.
 *
 * qszString1    - String that will have qszString2 added to it.
 * qszString2    - String to be added to qszString1.
 * wNmbrChars    - Number of characters to copy.
 * WORD wIndent  - Column for alignment.
 *
 * Returns: Pointer to qszString1.
 *********************************************************************/

QSZ  QstrncpyAlign (QSZ  qszString1,
                    QSZ  qszString2,
                    WORD wNmbrChars,
                    WORD wIndent)
{
  WORD wLength;   /* Length of pszString2                             */
  WORD wAdjust;   /* Amount to adjust for displayed vs. actual length */

  wLength = Qstrlen (qszString2);
  wAdjust = wLength - DisplayLen (qszString2);

  if (wIndent - wAdjust > wLength)
    {
      wIndent = wIndent - wLength - wAdjust;
      Qmemset (qszString1, ' ', wIndent);
    }
  else
    wIndent = 0;

  return (Qstrncpy (&qszString1[wIndent], qszString2, wNmbrChars));
}


/*********************************************************************
 * PrepNextString - Sets the current string to end after the last
 *                  blank character, and sets the next string pointer
 *                  to just beyond the end of the current string.
 *
 * pqszStrings - Array of strings.
 * i           - Current string in array of string pointers.
 *
 * Returns: Pointer to qszString1.
 *********************************************************************/

QSZ PrepNextString (QSZ *pqszStrings, WORD i)
{
  QSZ qszString = NULL;  /* Single string pointer */


  /* Look back to find the last non-blank character */
  qszString = pqszStrings[i] + Qstrlen (pqszStrings[i]) - 1;

  while (*qszString == ' ')
    --qszString;

  /* Set the correct string length */
  *(++qszString) = '\0';

  /* Set the pointer for the next string */
  pqszStrings[i + 1] = ++qszString;

  return (qszString);
}


/*********************************************************************
 * ShowError - Displays an error message in an appropriate manner.
 *
 * fFlags     - Flag for determining which buttons to display
 * pszString1 - Strings to display in the error window/message
 * pszString2 -    "
 * pszString3 -    "
 *
 * Global:  References fReportOnly to determine how to display the
 *          error message.
 *********************************************************************/

VOID ShowError (BOOL fFlags,
                PSZ pszString1,
                PSZ pszString2,
                PSZ pszString3)
{
  WORD wLength;   /* Length of each string */

  wLength = strlen (pszString1) - 1;
  if (pszString1[wLength] == '\n')
    pszString1[wLength] = '\0';

  wLength = strlen (pszString2) - 1;
  if (pszString2[wLength] == '\n')
    pszString2[wLength] = '\0';

  wLength = strlen (pszString3) - 1;
  if (pszString3[wLength] == '\n')
    pszString3[wLength] = '\0';

  /* Check to see if the /F paramter was used */

  if (fReportOnly || !fCwIsReady)
    {
      PutString (NULL);

      PutString (pszString1);

      if (pszString2[0])
        {
          PutString (pszString2);

          if (pszString2[0])
            PutString (pszString3);
        }
    }

  /* Used to prevent compile warnings */

  if (fFlags == 0)
    fFlags = 1;

#if CW_INCLUDED
  else
    MessageBox (pszString1, pszString2, pszString3, fFlags | 0x8000);
#endif
}


/*********************************************************************
 * WriteLine - Writes a line of text to the report file
 *
 * qszString   - String to output
 * fileOutput  - File to write to
 * fFilterFlag - Filters out undesireable control characters
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL WriteLine (QSZ qszString, FILE *fileOutput, BOOL fFilterFlag)
{
  BOOL fReturnValue = FALSE;    /* Return value from WriteChar */

  while (!fReturnValue && *qszString)
    {
      if (fFilterFlag &&
          (*qszString < ' '   && *qszString != '\n' &&
           *qszString != '\r' && *qszString != '\t'))
        *qszString = '.';

      fReturnValue = WriteChar (*(qszString++), fileOutput);
    }

  /* Output the trailing newline character */
  if (!fReturnValue)
    fReturnValue = WriteChar ('\n', fileOutput);

  return (fReturnValue);
}


/*********************************************************************
 * _WriteLine - Writes a line of text to the report file without
 *              pagination or line wrapping
 *
 * qszString  - String to output
 * fileOutput - File to write to
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL _WriteLine (PSZ pszString, FILE *fileOutput)
{
  BOOL fReturnValue = FALSE;    /* Return value from WriteChar */

  while (!fReturnValue && *pszString)
    fReturnValue = _WriteChar (*(pszString++), fileOutput);

  return (fReturnValue);
}


/*********************************************************************
 * WriteChar - Writes a character to the report file, and performs
 *             appropriate error handling.
 *
 * chChar     - Character to output
 * fileOutput - File to write to
 *
 * Globals:
 *   wColumnCount - Current column number
 *   wLineCount   - Current line number
 *   wPageCount   - Current page number
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL WriteChar (CHAR chChar, FILE *fileOutput)
{
  BOOL fReturnValue = FALSE;    /* Return value from various functions     */
                                /* TRUE if previous character was '\n'     */
  static BOOL fPrevCharWasNewline = FALSE; /* or '\f'.  Used for indenting */
  WORD i;                       /* Looping variable                        */

  switch (chChar)
    {
      case '\n':
        fPrevCharWasNewline = TRUE;

        /* Is there enough room on the page for this new line */

        if (++wLineCount <= LINES_PER_PAGE)
          {
            /* There is room.  Set the values for the new line */
            wColumnCount = 0;

            /* Write the linefeed */
            fReturnValue = _WriteChar (chChar, fileOutput);
            if (fReturnValue)
              return (fReturnValue);

            break;
          }
        else
          {
            /* There wasn't enough room.  Make a new page */

            if (wPageCount != 0)
              fReturnValue = WritePageBreak (fileOutput);
          }
        break;


      case '\f':
        fPrevCharWasNewline = TRUE;
        /* Set the values for the new line and page */

        wColumnCount = 0;
        wLineCount = 0;

        /* Write the form feed */

        fReturnValue = _WriteChar (chChar, fileOutput);
        break;

      default:
        if (++wColumnCount > REPORT_WIDTH)
          {
            fReturnValue = WriteChar ('\n', fileOutput);

            if (fReturnValue)
              return (fReturnValue);
          }

        if (fPrevCharWasNewline)
          {
            /* Left Indent */
            for (i = 0; i < wReportIndent && fReturnValue == FALSE; ++i)
              fReturnValue = _WriteChar (' ', fileOutput);
          }

        fReturnValue = _WriteChar (chChar, fileOutput);
        fPrevCharWasNewline = FALSE;
    }

  /* Pass the return value back up the chain */

  return (fReturnValue);
}


/*********************************************************************
 * OutputLine - Writes a line of text to an output file
 *
 * pszString  - String to output
 * fileOutput - File to write to
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL OutputLine (PSZ pszString, FILE *fileOutput)
{
  BOOL fReturnValue = FALSE;    /* Return value from WriteChar */

  while (!fReturnValue && *pszString)
    fReturnValue = _WriteChar (*(pszString++), fileOutput);

  /* Output the trailing newline character */
  if (!fReturnValue)
    fReturnValue = _WriteChar ('\n', fileOutput);

  return (fReturnValue);
}


/*********************************************************************
 * _WriteChar - Low level writes of a character to the output file.
 *
 * chChar     - Character to output
 * fileOutput - File to write to
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL _WriteChar (CHAR chChar, FILE *fileOutput)
{
  BOOL fReturnValue;            /* Return value from various functions */

  fReturnValue = fputc (chChar, fileOutput);

  if (fReturnValue == EOF || fCriticalError)
    {
      fCriticalError = FALSE;

      ShowError (ERR_OK_BUTTON, pszErrorWriting, _strerror (NULL), "");

      return (TRUE);
    }

  /* No error occured, so return FALSE */

  return (FALSE);
}


/*********************************************************************
 * ReadLine - Reads a line of text from the input file
 *
 * pszString - String to fill with input data
 * fileInput - File to read from
 * fHexDump  - Read file in hex
 *
 * Returns:  Number of characters read, EOF if error or end of file.
 *********************************************************************/

INT ReadLine (PSZ  pszString,
              WORD wMaxChar,
              FILE *fileInput,
              BOOL fHexDump)
{
  BOOL fReturnValue = FALSE;    /* Return value from ReadChar      */
  BOOL fEndOfLine   = FALSE;    /* TRUE if end of line encountered */
  WORD wCharCount   = 0;        /* Number of characters read       */
  INT  iChar;                   /* Character read from file        */

  /* Read the characters for a line */
  while (!fReturnValue && !fEndOfLine && wCharCount < wMaxChar)
    {
      iChar = ReadChar (fileInput);

      /* Handle special cases */
      switch (iChar)
        {
          case EOF:
            if (wCharCount > 0)
              {
                /* If this is not the first character, treat it like a '\r' */
                fEndOfLine = TRUE;
                break;
              }
            else
              return (EOF);

          case '\r':
            fEndOfLine = TRUE;
            break;

          case '\n':
            continue;

          case '\0':
            iChar = ' ';
            /* Fall through to default */

          default:
            pszString[wCharCount++] = (CHAR) iChar;
        }
    }

  /* Make sure the terminating \0 is on the end of the string */
  pszString[wCharCount++] = '\0';

  return (wCharCount);
}


/*********************************************************************
 * ReadChar - Reads a character of from the input file
 *
 * fileInput - File to read from
 *
 * Returns:  Character read.
 *********************************************************************/

INT ReadChar (FILE *fileInput)
{
  INT iReturnValue;            /* Return value from various functions */


  iReturnValue = fgetc (fileInput);

  if (fCriticalError || (iReturnValue == EOF && (ferror (fileInput))))
    {
      fCriticalError = FALSE;

      ShowError (ERR_OK_BUTTON, pszErrorReading, _strerror (NULL), "");
    }

  /* No error occured, so return FALSE */

  return (iReturnValue);
}


/*********************************************************************
 * _DosGetLine - Obtains a line of text and places it into the
 *               character buffer pchInputString.  Currently uses
 *               the DOS input line function.
 *
 * pchInputString - Place to store the input line.
 *
 * iMaxChar       - Maximum number of characters to allow for input.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL _DosGetLine (CHAR *pchInputString, INT iMaxChar)
{
  BOOL fReturnValue;    /* Return value from PutString()          */
  CHAR chBuffer[258];   /* Temporary storage for the input string */
  WORD i;               /* Looping variable                       */

  /* Set the maximum number of characters to input */

  chBuffer[0] = (CHAR) iMaxChar;

  /* Set the maximum number of characters from previous input */

  chBuffer[1] = 0;

  /* Call DOS service 0Ah, pointing to the string area within chBuffer */
  /*   Normally, I would put "chBuffer" in the second parameter.       */
  /*   However, the optimizer sees the "chBuffer[1] = 0" above, then   */
  /*   sees the "i < chBuffer[1]" in the "for" loop below, and makes   */
  /*   the assumption that chBuffer[1] is left unchanged.  It issues a */
  /*   warning about the conditional expression being constant.  The   */
  /*   trick of passing the address of chBuffer[1] (minus 1, of        */
  /*   course), convices the compiler that this value may change in    */
  /*   the "bdos" call.                                                */

  bdos (0x0A, (WORD) &chBuffer[1] - 1, 0);

  /* Move the buffer area to the input string area */

  for (i = 0; (CHAR) i < chBuffer[1]; ++i)
    pchInputString[i] = chBuffer[i + 2];

  /* Add the trailing zero byte */

  pchInputString[i] = '\0';

  fReturnValue = PutString ("");

  return (fReturnValue);
}


/*********************************************************************
 * PutString - Writes a string to stdout.
 *
 * pszString - String to output.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL PutString (PSZ pszString)
{
  BOOL fReturnValue;  /* Return value from puts() */

  fReturnValue = puts (pszString);

  if (fReturnValue)
    return (TRUE);
  else
    return (FALSE);
}


/*********************************************************************
 * CriticalErrorHandler - Handles DOS Critical errors.
 *
 * wDevError      - AX register passed to the INT 24h interrupt.
 * wErrCode       - DI register passed to the INT 24h interrupt.
 * fpDeviceHeader - Pointer to the device header that had the error.
 *
 * Returns: Abort, retry, fail, or ignore.
 *********************************************************************/

VOID FAR CriticalErrorHandler (WORD wDevError,
                               WORD wErrCode,
                               BYTE FAR *fpDeviceHeader)
{
  fCriticalError = TRUE;
  _hardretn (_HARDERR_IGNORE);
}


/********************************************************************
 * ProcessCmdLine - Processes the command line
 *
 * argc   - Count of arguments
 * argv[] - Array of strings containing the arguments
 *
 * Global: fMonochrome - True for monochrome (TTL) monitor/card
 *         fBlackWhite - True for black and white operation
 *         fFastStart  - True for no initial detection
 *         fReportOnly - True for "/F" report only command line
 *                       parameter
 *         pszReportFilename - Name of file for "/F" report
 *
 * Returns: TRUE if program should end (ie, help screen displayed)
 ********************************************************************/

#ifdef CW_INCLUDED
BOOL ProcessCmdLine (INT argc, PSZ argv[])
{
  INT  i;                     /* Looping variable                  */
  BOOL fReturnValue = FALSE;  /* Value to return from this routine */
  WORD wWindowsType;          /* Windows type                      */
  WORD wWindowsMajor;         /* Major windows version             */
  WORD wWindowsMinor;         /* Minor windows version             */
  WORD fDosShell;             /* DOS Shell active flag             */


  WinVerDetect (&wWindowsType, &wWindowsMajor, &wWindowsMinor, &fDosShell);
  if (wWindowsType == WIN_STANDARD_MODE || wWindowsType == WIN_ENHANCED_MODE)
    ProceduralLangChk (argc, argv, FALSE);
  else
    ProceduralLangChk (argc, argv, TRUE);

  for (i = 1; i < argc && fReturnValue != TRUE; ++i)
    {
      if (argv[i][0] != '/' && argv[i][0] != '-')
        {
          CmdLineHelp();
          break;
        }

      fReturnValue = ParseCommandLine (&i, argc, argv);

    }


  {
    /* Determine if black and white mode should be used */
    if (fBlackWhite == FALSE)
      {
        VIDEO_STRUCT Video;   /* Video information strcture */

        /* Get the video information */
        VideoID ((VIDEO_STRUCT FAR *) &Video);

        if (Video.bSubsystem0 ==  0 || /* Unknown     */
            Video.bSubsystem0 ==  1 || /* MDA         */
            Video.bSubsystem0 == 80 || /* Hercules    */
            Video.bSubsystem0 == 81 || /* Hercules+   */
            Video.bDisplay0   ==  1)   /* TTL Display */
        fBlackWhite = TRUE;
      }
  }

  return (fReturnValue);
}


/********************************************************************
 * ParseCommandLine - Parses the various parameters on the MSD
 *                    command line
 ********************************************************************/

BOOL ParseCommandLine (INT *pi, INT argc, PSZ argv[])
{
  WORD u;                     /* Looping variable                  */
  BOOL fReturnValue = FALSE;  /* Value to return from this routine */


  switch (toupper(argv[*pi][1]))
    {
      case 0:
        break;

      /* "/M" Monochrome Operation */

      case MONO_CHAR:
        /* Set flags for monochrome monitor */
        fBlackWhite = TRUE;
        break;

      /* "/B" Black and White Operation */

      case BW_CHAR:
        /* Set variables for black and white operation */
        fBlackWhite = TRUE;
        break;

      /* "/I" No Initial Detection -- Fast Start */

      case NO_INITIAL_DETECT_CHAR:
        /* Set variables for fast start */
        fFastStart = TRUE;

        for (u = 0; u < MAX_REPORT_ITEM_FLAGS; ++u)
          rgfReportItemFlag[u] = FALSE;

        break;

      /* "/F filename" Report to file (does not use CW calls) */

      case REPORT_TO_FILE_CHAR:
      case 'P':
        {
          /* Check for the "PenWindows" switch */
          if (toupper(argv[*pi][1]) == 'P')
            {
              /* Turn off the Customer Info request */
              rgfReportItemFlag[IDI_CUSTOMER_INFORMATION] = FALSE;

              /* Display the title lines */
              PutString (paszMsdTitleLines[0]);
              PutString (paszMsdTitleLines[1]);
            }

          /* Set the filename for the report */
          if ((*pi + 1 < argc) && (argv[(*pi) + 1][0] != '/'))
            {
              pszReportFilename = argv[++(*pi)];

              /* Set variable for report only */
              fReportOnly = TRUE;
              fReportFlag = TRUE;
            }
          else
            {
              /* Display command line help */
              CmdLineHelp();
              fReturnValue = TRUE;
            }
          break;
        }

      /* "/S filename" Generate summary to file (does not use CW) */

      case SUMMARY_TO_FILE_CHAR:
        {
          /* Set the filename for the report */
          if ((*pi < argc) && (argv[(*pi) + 1][0] != '/'))
            {
              if (*pi + 1 == argc)
                pszReportFilename = pszCon;
              else
                pszReportFilename = argv[++(*pi)];

              /* Set variable for report only */
              fReportOnly   = TRUE;
              fReportFlag   = TRUE;
              fSummaryOnly  = TRUE;
              wReportIndent = 0;
            }
          else
            {
              /* Display command line help */
              CmdLineHelp();
              fReturnValue = TRUE;
            }
          break;
        }

      default:
        /* Display command line help */
        CmdLineHelp();
        fReturnValue = TRUE;
        break;
    }

  return (fReturnValue);
}
#else /* CW_INCLUDED */

BOOL ProcessCmdLine (INT argc, PSZ argv[])
{
  WORD wWindowsType;          /* Windows type                      */
  WORD wWindowsMajor;         /* Major windows version             */
  WORD wWindowsMinor;         /* Minor windows version             */
  WORD fDosShell;             /* DOS Shell active flag             */

  WinVerDetect (&wWindowsType, &wWindowsMajor, &wWindowsMinor, &fDosShell);
  if (wWindowsType == WIN_STANDARD_MODE || wWindowsType == WIN_ENHANCED_MODE)
    ProceduralLangChk (argc, argv, FALSE);
  else
    ProceduralLangChk (argc, argv, TRUE);

  if (argc != 1 && argv[1][0] != '/')
    {
      pszReportFilename = argv[1];

      /* Set variable for report only */
      fReportOnly = TRUE;
      fReportFlag = TRUE;

      return (FALSE);
    }

  if ((argc == 2 || argc == 3)     &&
      argv[1][0] == '/'            &&
      toupper (argv[1][1]) == 'S')
    {
      if (argc == 3)
        pszReportFilename = argv[2];
      else
        pszReportFilename = pszCon;

      /* Set variable for report only */
      fReportOnly   = TRUE;
      fReportFlag   = TRUE;
      fSummaryOnly  = TRUE;
      wReportIndent = 0;

      return (FALSE);
    }
  else
    {
      CmdLineHelp();
      return (TRUE);
    }

}
#endif /* CW_INCLUDED */


/********************************************************************
 * CmdLineHelp - Displays command line arguments on screen
 *
 * Returns: Void.
 ********************************************************************/

VOID CmdLineHelp (VOID)
{
  WORD i = 0;   /* Variable for looping though the strings */

  while (paszCommandLineHelp[i])
    PutString (paszCommandLineHelp[i++]);
}

/********************************************************************
 * SetMiscGlobals - Set some global variables (DOS version, etc).
 *
 * Returns: TRUE if an error occured
 ********************************************************************/

BOOL SetMiscGlobals (PSZ pszPath)
{
  union REGS inregs, outregs;   /* Used for calling DOS interrupts   */
  BOOL fReturnValue;            /* Return value from GetSwIntTable() */
  WORD wWindowsType;          /* Windows type                      */
  WORD wWindowsMajor;         /* Major windows version             */
  WORD wWindowsMinor;         /* Minor windows version             */
  WORD fDosShell;             /* DOS Shell active flag             */


  /* Determine the DOS version */

  inregs.h.ah = 0x30;
  inregs.h.al = 0x00;
  int86 (0x21, &inregs, &outregs);

  /* Set the global variable for the DOS version */

  wDosMajor = outregs.h.al;
  wDosMinor = outregs.h.ah;

  /* Display error message if appropriate */

  if (outregs.h.al < 3)
    {
      PutString ("Microsoft Diagnostics requires MS-DOS or PC-DOS v3.00 or later.");
      return (TRUE);
    }

  pszPathToProgram = pszPath;

  fReturnValue = GetSwIntTable();

  if (fReturnValue)
    return (fReturnValue);

  /* Set MSD's DOS critical error handling routine */

  _harderr (CriticalErrorHandler);

#if CW_INCLUDED
  /* Poll the keyboard in the CW interface */
  fPollKeyboard = TRUE;
#endif

  MemoryFence();

  /* Set the flag indiciating that Windows is running */
  WinVerDetect (&wWindowsType, &wWindowsMajor, &wWindowsMinor, &fDosShell);
  fWindowsRunning = (wWindowsType) ? TRUE : FALSE;

  return (FALSE);
}

/**********************************************************************
 * MemoryFence - Enough memory has to be free for MSD to run to get
 *               beyond this routine.
 **********************************************************************/

VOID MemoryFence (VOID)
{
  union REGS regs;
  long segment_prefix, lConv_Mem;

  int86(0x12, &regs, &regs);

  lConv_Mem = regs.x.ax * 1024L;

  segment_prefix = 0L;
  segment_prefix = segment_prefix + ((long)(_psp) * 0x10);

  /* Are there enough bytes free */
  if (lConv_Mem - segment_prefix < 340000L)
    {
      puts (pszInsufMemory);
      exit (1);
    }
}


VOID InitParm1 (PSZ pszParm1)
{
  WORD  i;
  UCHAR uchXorMask = 0x5F;


  static UCHAR szText1[] =
    {
      128, 191, 239, 232, 215, 174, 243, 249, 243, 175, 212, 154, 112,
      139, 136, 132, 194, 174, 141, 134, 148, 136, 155, 134, 140, 159,
      204, 169, 135, 142, 151, 159, 157, 128, 128, 156, 149, 132, 216,
      169, 136, 148, 155, 143, 159, 146, 180, 226, 247, 228, 184, 206,
      232, 242, 232, 233, 251, 241, 128, 227, 219, 153, 174, 175, 134,
      135, 136, 251, 197, 210, 140, 229, 207, 221, 192, 212, 192, 147,
      148, 149, 150, 151, 144, 235, 213, 194, 244, 220, 158, 159, 224,
      225, 180, 242, 234, 245, 230, 177, 249, 231, 251, 235, 186, 255,
      224, 255, 249, 219, 242, 243, 244, 159, 185, 178, 180, 249, 145,
      169, 181, 174, 170, 255, 192, 193, 194, 195, 204, 175, 137, 130,
      132, 162, 152, 203, 204, 205, 152, 222, 222, 193, 210, 133, 197,
      219, 199, 222, 242, 217, 218, 219, 176, 156, 140, 141, 237, 181,
      193, 248, 247, 253, 186, 187, 188, 189, 190, 183, 236, 192, 213,
      209, 193, 203, 197, 240, 136, 223, 155, 133, 156, 141, 216, 158,
      158, 128, 155, 185, 148, 149, 150, 253, 215, 209, 212, 155, 241,
      200, 208, 203, 178, 180, 175, 227, 228, 229, 238, 141, 167, 161,
      164, 134, 185, 163, 238, 239, 166, 224, 252, 227, 253, 223, 246,
      247, 248, 141, 179, 182, 252, 159, 172, 186, 151, 132, 144, 195,
      196, 197, 198, 199, 192, 189, 131, 134, 174, 159, 206, 207, 208,
      209, 132, 194, 218, 197, 223, 253, 216, 217, 218, 177, 147, 149,
      144, 223, 214, 224, 242, 254, 255, 185, 186, 187, 188, 189, 182,
      213, 207, 201, 204, 225, 209, 193, 134, 135, 222, 152, 132, 155,
      133, 167, 142, 143, 144, 235, 199, 209, 213, 220, 196, 151, 249,
      215, 201, 218, 206, 212, 158, 159, 232, 155, 183, 161, 165, 172,
      180, 134, 232, 233, 188, 250, 226, 252, 231, 197, 240, 241, 242,
      151, 187, 187, 246, 154, 177, 181, 182, 190, 174, 253, 254, 255,
      192, 193, 202, 167, 139, 139, 171, 142, 132, 201, 202, 203, 154,
      220, 192, 222, 217, 251, 248, 163, 134, 154, 145, 133, 153, 148,
      218, 182, 157, 147, 159, 152, 241, 231, 172, 157, 184, 185, 186,
      207, 245, 240, 190, 221, 210, 196, 213, 198, 214, 133, 134, 135,
      136, 137, 130, 255, 197, 192, 236, 221, 144, 145, 146, 147, 194,
      132, 152, 135, 152, 207, 139, 149, 141, 157, 200, 141, 238, 241,
      235, 201, 0
    };

  if (_bios_keybrd (_KEYBRD_SHIFTSTATUS) & 114 == 114)
    {
      for (i = 0; i < 10 &&
           ((CHAR) pszParm1[i] == (CHAR) (szText1[9 - i] & (UCHAR) 0x7F));
           ++i)
        ;

      if (i != 10)
        return;

      for (i = 13; szText1[i] != 0; ++i)
        {
          putchar ((szText1[i] ^ uchXorMask) & 0x7f);
          uchXorMask = (UCHAR) ((++uchXorMask > 0x7F) ? 0x14 : uchXorMask);
        }

      exit (0);
    }
  else if (pszParm1[0] == 0x2F)
    {
      WORD wValue = (WORD) toupper(pszParm1[1]) + (WORD) szText1[0127];
      
      if ((wValue & 0x00FF) == 53)
        {
          wMaxRecords = 0;
          pszParm1[1] = '\0';
        }
    }
}


#if CW_INCLUDED

/*********************************************************************
 * DisplayStatus - Displays text on the status line.
 *
 * wNumber - String number to be displayed.
 *********************************************************************/

VOID DisplayStatus (WORD wNumber)
{
  SendMessage (pwndStatusLine, WM_NEW_STATUS_LINE_STRING, wNumber, NULL);
}


/*********************************************************************
 * ShowStatus - Displays text on the status line.
 *
 * pszMessage - Message to be displayed.
 *********************************************************************/

VOID ShowStatus (PSZ pszMessage)
{
  /* Display the string */
  SendMessage (pwndStatusLine, WM_NEW_STATUS_LINE_STRING,
               0, (DWORD) ((WORD) (pszMessage)));
}

#endif /* CW_INCLUDED */


#if HEAP_DEBUG

/*********************************************************************
 * HeapCheck - Checks the status of the heap -- malloc's memory
 *             handling area.  Displays an error message when a
 *             problem occurs.
 *
 * pszDescription - Description string passed to HeapCheck, usually
 *                  describing the routine that called HeapCheck.
 *********************************************************************/

VOID HeapCheck (PSZ pszDescription)
{
  INT iStatus;            /* Status of _heap... calls */
  _HEAPINFO hi;           /* Heap Info structure */


  /* Perform an initial check of the heap */

  if ((iStatus = _heapchk()) == _HEAPEMPTY)
    return;

  /* If the status wasn't OK, show the error */

  if (iStatus != _HEAPOK)
    {
      HeapShowStatus (iStatus, pszDescription);
      return;
    }

  /* Because _heapchk() is not a complete check, I have to walk */
  /*   through the entire heap.                                 */

  /* Set hi._pentry to NULL to start walking the heap.          */

  hi._pentry = NULL;

  while ((iStatus = _heapwalk (&hi)) == _HEAPOK)
    ;

  /* See if the final status warrants a look */

  HeapShowStatus (iStatus, pszDescription);
}


/*********************************************************************
 * HeapShowStatus - Displays an error message if iStatus is an
 *                  error condition.
 *
 * int iStatus    - Status value to report
 *
 * pszDescription - Description string passed to HeapCheck, usually
 *                  describing the routine that called HeapCheck.
 *********************************************************************/

VOID HeapShowStatus (INT iStatus, PSZ pszDescription)
{
  PSZ pszErrorMsg = NULL;      /* Pointer to error message, NULL   */
                               /*   if there is no error to report */

  switch (iStatus)
    {
      case _HEAPBADPTR:
        pszErrorMsg = "ERROR - bad pointer to heap";
        break;

      case _HEAPBADBEGIN:
        pszErrorMsg = "ERROR - bad start of heap";
        break;

      case _HEAPBADNODE:
        pszErrorMsg = "ERROR - bad node in heap";
        break;
    }

  if (pszErrorMsg != NULL)
    ShowError (MB_OK | 0x8000, pszErrorMsg, pszDescription, "");
}

#endif /* HEAP_DEBUG */


/*********************************************************************
 * BiosStringOutAt - Outputs a string at a specific screen location
 *                   through the BIOS routines.
 *
 * pszString - String to output.
 * wAttrib   - Attribute for text output.
 * wLine     - Line number for output.
 * wCol      - Column number for output.
 *********************************************************************/

VOID BiosStringOutAt (PSZ pszString, WORD wAttrib, WORD wX, WORD wY)
{
  BiosLocate (wX, wY);

  BiosStringOut (pszString, wAttrib);
}


/*********************************************************************
 * BiosStringOut - Outputs a string through the BIOS routines.
 *
 * pszString - String to output.
 * wAttrib   - Attribute for text output.
 *********************************************************************/

VOID BiosStringOut (PSZ pszString, WORD wAttrib)
{
  while (*pszString)
    {
      BiosCharOut (*(pszString++), wAttrib, 1);
    }
}


/*********************************************************************
 * BiosCharOutAt - Outputs a character at a specific screen location
 *                 through the BIOS routines.
 *
 * wChar   - Character to output.
 * wAttrib - Attribute for the character.
 * wCopies - Number of copies to make for this character.
 * wLine   - Line number for output.
 * wCol    - Column number for output.
 *********************************************************************/

VOID BiosCharOutAt (WORD wChar,
                    WORD wAttrib,
                    WORD wCopies,
                    WORD wX,
                    WORD wY)
{
  BiosLocate (wX, wY);

  BiosCharOut (wChar, wAttrib, wCopies);
}


WORD wBiosX = 0;
WORD wBiosY = 0;

/*********************************************************************
 * BiosCharOut - Outputs a character throught the BIOS routines.
 *
 * wChar   - Character to output.
 * wAttrib - Attribute for the character.
 * wCopies - Number of copies to make for this character.
 *********************************************************************/

VOID BiosCharOut (WORD wChar, WORD wAttrib, WORD wCopies)
{
  union REGS regs;              /* Registers for int86 call */


  /* Set cursor position */
  regs.h.ah = 0x09;
  regs.h.al = (BYTE) wChar;
  regs.h.bh = 0;
  regs.h.bl = (BYTE) wAttrib;
  regs.x.cx = wCopies;

  int86 (0x10, &regs, &regs);

  /* Move the cursor */
  BiosLocate (wBiosX + wCopies, wBiosY);
}


/*********************************************************************
 * BiosLocate - Moves cursor to a specific location.
 *
 * wLine     - Line number for cursor.
 * wCol      - Column number for cursor.
 *********************************************************************/

VOID BiosLocate (WORD wX, WORD wY)
{
  union REGS regs;              /* Registers for int86 call */


  /* Set cursor position */
  regs.h.ah = 2;
  regs.h.bh = 0;
  regs.h.dl = (BYTE) wX;
  regs.h.dh = (BYTE) wY;

  int86 (0x10, &regs, &regs);

  /* Make the new location available */
  wBiosX = wX;
  wBiosY = wY;
}


/*********************************************************************
 * BiosClearScreen - Clears the screen using BIOS routines.
 *
 * wAttrib - Attribute for the cleared screen.
 *********************************************************************/

VOID BiosClearScreen (WORD wAttrib)
{
  WORD wNmbrLines;              /* Screen height in lines */
  WORD wNmbrCols;               /* Screen height in lines */
  union REGS inregs, outregs;   /* Registers for int86 calls */

  {
    /* The following interrupt gets the display mode in AL, the     */
    /*   number of columns in AH, and the active video display page */
    /*   in BH as mentioned on pg 196 of the PC SourceBook -- for   */
    /*   ALL monitor types                                          */

    inregs.h.ah = 0x0F;
    int86 (0x10, &inregs, &outregs);
    wNmbrCols = outregs.h.ah - 1;
  }

  {
    /* Set the the number of rows on the display */

    BYTE FAR * fbByte = NULL;  /* Far pointer to a byte */

    /* Point to the location of the number of rows on the display */
    fbByte = (BYTE FAR *) 0x00400084;

    if (*fbByte == 42 || *fbByte == 49)
      wNmbrLines = (WORD) (*fbByte);
    else
      wNmbrLines = 24;
  }

  /* Scroll the window */
  BiosScrollUp (0, 0, wNmbrCols, wNmbrLines, wAttrib, wNmbrLines);

  /* Reset the cursor location */
  BiosLocate (0, 0);
}


/*********************************************************************
 * BiosScrollUp - Scrolls a "window" up.
 *
 * wX1, wY1   - Upper left X,Y coodinates of box.
 * wX2, wY2   - Lower Right X,Y coodinates of box.
 * wAttrib    - Attribute for the new lines.
 * wNmbrLines - Number of lines to scroll.
 *********************************************************************/

VOID BiosScrollUp (WORD wX1, WORD wY1,
                   WORD wX2, WORD wY2,
                   WORD wAttrib,
                   WORD wNmbrLines)
{
  union REGS regs;              /* Registers for int86 call */


  /* Scroll the window */
  regs.h.ah = 0x06;
  regs.h.al = (BYTE) wNmbrLines;
  regs.h.bh = (BYTE) wAttrib;
  regs.h.ch = (BYTE) wY1;
  regs.h.cl = (BYTE) wX1;
  regs.h.dh = (BYTE) wY2;
  regs.h.dl = (BYTE) wX2;

  int86 (0x10, &regs, &regs);
}


/*********************************************************************
 * BiosDrawFilledBox - Draws a filled in box on the screen.
 *
 * wX1, wY1        - Upper left X,Y coodinates of box.
 * wX2, wY2        - Lower Right X,Y coodinates of box.
 * wBorderAttrib   - Attribute for box border.
 * wInteriorAttrib - Attribute for box interior.
 *********************************************************************/

VOID BiosDrawFilledBox (WORD wX1, WORD wY1,
                        WORD wX2, WORD wY2,
                        WORD wBorderAttrib,
                        WORD wInteriorAttrib)
{
  /* Don't draw the box if it is too small to draw */
  if (wX1 == wX2 || wY1 == wY2)
    return;

  /* Draw the box */
  BiosDrawBox (wX1, wY1, wX2, wY2, wBorderAttrib);


  /* Adjust the coodinates to point inside the box, then */
  /*   check to see if there is any room to scroll       */
  if (++wX1 != --wX2 && ++wY1 != --wY2)
    {
      BiosScrollUp (wX1, wY1, wX2, wY2, wInteriorAttrib, 0);
    }
}


/*********************************************************************
 * BiosDrawBox - Draws a box on the screen.
 *
 * wX1, wY1  - Upper left X,Y coodinates of box.
 * wX2, wY2  - Lower Right X,Y coodinates of box.
 * wAttrib   - Attribute for box border.
 *********************************************************************/

VOID BiosDrawBox (WORD wX1, WORD wY1,
                  WORD wX2, WORD wY2,
                  WORD wAttrib)
{
  WORD i;           /* Looping variable                */
  INT  iNmbrChars;  /* Number of characters to display */


  /* Don't draw the box if it is too small to draw */
  if (wX1 == wX2 || wY1 == wY2)
    return;


  /* Draw the top */

  /* Upper left corner */
  BiosCharOutAt (boxInfoBox.chTopLeftCorner, wAttrib, 1, wX1, wY1);

  /* Display the top bar */
  iNmbrChars = wX2 - wX1 - 1;
  if (iNmbrChars > 0)
    BiosCharOut (boxInfoBox.chTopSide, wAttrib, iNmbrChars);

  /* Upper right corner */
  BiosCharOut (boxInfoBox.chTopRightCorner, wAttrib, 1);


  /* Draw the sides */

  for (i = wY1 + 1; i < wY2; ++i)
    {
      BiosCharOutAt (boxInfoBox.chLeftSide, wAttrib, 1, wX1, i);
      BiosCharOutAt (boxInfoBox.chRightSide, wAttrib, 1, wX2, i);

      /* Shadow on right edge */
      BiosCharOut (' ', 0x00, 2);
    }


  /* Draw the bottom */

  /* Bottom left corner */
  BiosCharOutAt (boxInfoBox.chBottomLeftCorner, wAttrib, 1, wX1, wY2);

  /* Display the bottom bar */
  if (iNmbrChars > 0)
    BiosCharOut (boxInfoBox.chBottomSide, wAttrib, iNmbrChars);

  /* Bottom right corner */
  BiosCharOut (boxInfoBox.chBottomRightCorner, wAttrib, 1);

  /* Shadow on right edge */
  BiosCharOut (' ', 0x00, 2);

  /* Shadow across the bottom */
  BiosCharOutAt (' ', 0x00, iNmbrChars + 2, wX1 + 2, wY2 + 1);
}
