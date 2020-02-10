/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * DISKINFO.C - Source file for obtaining disk drive information.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetDiskInfo - Gets the disk information.
 *
 * pDisk        - Pointer to disk information structure.
 * fMinimumInfo - TRUE if minimum info is requested.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetDiskInfo (DISK_STRUCT *pDisk, BOOL fMinimumInfo)
{
  BYTE  i;                      /* Looping variable                       */
  BYTE  bCurrentDrive;          /* Stores the current drive number        */
  BYTE  bLastDrive;             /* Last drive number in the system        */
  WORD  wDriveIndex;            /* Index to the table of drive info       */
  union REGS inregs, outregs;   /* Register structures for int86() calls  */
  struct SREGS sregs;           /* Segment registers for int86x() calls   */
  BOOL  fSingleFloppy;          /* TRUE if there is only one floppy drive */
  BOOL  fSkipSizeTest;          /* TRUE if size test should be skipped    */
  BOOL  fUseBiosCall = TRUE;    /* IOCTL 0Dh failed, use BIOS call        */
  BYTE FAR *fbSingleFloppyIsB = (BYTE FAR *) 0x00400104;
                                /* if on a single floppy system, this     */
                                /*   byte is == 1 if the floppy is acting */
                                /*   as drive B:                          */
  WORD  wDosVersion;            /* DOS Version (ie 3.30 == 330)           */
  WORD  wDriveType;             /* Local variable for drive type          */
  WORD  wNmbrFloppies = 0;      /* Number of floppies (for INT 13h)       */
  WORD  wNmbrFixedDisks = 0;    /* Number of hard disks (for INT 13h)     */


  /* Set the local DOS version variable */
  wDosVersion = wDosMajor * 100 + wDosMinor;


  /* Zero out the disk drive information structure */
  memset (pDisk, '\0', sizeof (DISK_STRUCT));


  /* Get the list of installed programs and data */
  if (fMinimumInfo == FALSE)
    GetDiskProgList (pDisk, wDosVersion);


  /* Determine the number of physical floppies in the A:-B: range */

  int86 (0x11, &inregs, &outregs);

  /* Bit 0 is a flag indicating if floppies are present.  If set, */
  /*   bits 6-7 show the count of floppies in the system.         */

  if ((outregs.x.ax & 0x0001) && (outregs.x.ax & 0x00C0) == 0x0000)
    fSingleFloppy = TRUE;
  else
    fSingleFloppy = FALSE;


  /* Store the current drive number */
  bCurrentDrive = DosGetCurrentDrive();
  pDisk->chCurrentDrive = (CHAR) (bCurrentDrive + 'A');

  /* Determine the last valid drive to check */
  bLastDrive = DosSetCurrentDrive (bCurrentDrive);
  pDisk->chLastDrive = (CHAR) (bLastDrive + 'A' - 1);


  /* Begin counting up the valid drives */

  for (i = 0; i < bLastDrive; ++i)
    if (ValidDrive (i))
      {
        /* Default to performing the size test */
        fSkipSizeTest = FALSE;


        /* If this is a single floppy, and it's acting as B: */
        /*   skip the A: check.                              */

        if (fSingleFloppy && *fbSingleFloppyIsB == 1 && i == 0)
          continue;


        /* If this is a single floppy, and it's acting as A: */
        /*   skip the B: check.                              */

        if (fSingleFloppy && *fbSingleFloppyIsB != 1 && i == 1)
          continue;


        /* Bump the count of valid drives */
        wDriveIndex = (WORD) (pDisk->wNmbrDrives)++;

        /* Store the drive letter of the valid drive */
        pDisk->asdi[wDriveIndex].chDriveLetter = (CHAR) (i + 'A');


        /* Check to see if drives are removable or not */

        inregs.x.ax = 0x4408;
        inregs.h.bl = (BYTE) (i + 1);
        int86 (0x21, &inregs, &outregs);

        /* CF is clear if IOCTL call was successful */
        if (outregs.x.cflag == 0)
          {
            /* AX == 0000h means it's a removable drive */
            if (outregs.x.ax == 0x0000)
              {
                pDisk->asdi[wDriveIndex].wDriveType = DISK_FLOPPY_DRIVE;
                strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                        paszDriveTypes[DISK_FLOPPY_DRIVE]);
              }
            else
              {
                pDisk->asdi[wDriveIndex].wDriveType = DISK_FIXED_DISK;
                strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                        paszDriveTypes[DISK_FIXED_DISK]);
              }
          }
        else
          {
            /* Something went wrong.  If this is DOS 3.0 assume */
            /*   it as a "Remote Drive"                         */

            if (wDosVersion == 300)
              {
                pDisk->asdi[wDriveIndex].wDriveType = DISK_REMOTE_DRIVE;
                strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                        paszDriveTypes[DISK_REMOTE_DRIVE]);
              }
          }


        /* If this is DOS 3.1 or better, we can tell for sure if it's */
        /*   a remote drive.                                          */

        if (wDosVersion >= 310)
          {
            inregs.x.ax = 0x4409;
            inregs.h.bl = (BYTE) (i + 1);
            int86 (0x21, &inregs, &outregs);

            /* CF is clear if IOCTL call was successful */
            if (outregs.x.cflag == 0)
              {
                if (outregs.x.dx & 0x8000)
                  {
                    /* Drive is SUBSTutited */
                    pDisk->asdi[wDriveIndex].wDriveType = DISK_SUBST_DRIVE;
                    strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                            paszDriveTypes[DISK_SUBST_DRIVE]);
                    pDisk->fSubstInstalled = TRUE;
                  }
                else if (outregs.x.dx & 0x1000)
                  {
                    /* Check to see if this is a MSCDEX CD-ROM drive */
                    inregs.x.ax = 0x150B;
                    inregs.x.cx = i;
                    int86 (0x2F, &inregs, &outregs);

                    if (outregs.x.bx == 0xADAD && outregs.x.ax != 0x0000)
                      {
                        /* Drive is a CD-ROM drive */
                        pDisk->asdi[wDriveIndex].wDriveType =
                            DISK_CD_ROM_DRIVE;
                        strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                                paszDriveTypes[DISK_CD_ROM_DRIVE]);
                      }
                    else
                      {
                        /* Drive is remote */
                        pDisk->asdi[wDriveIndex].wDriveType =
                            DISK_REMOTE_DRIVE;
                        strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                                paszDriveTypes[DISK_REMOTE_DRIVE]);
                      }
                  }

                if (outregs.x.dx & 0x0200)
                  {
                    /* Direct access is not allowed.  Skip the size test */
                    fSkipSizeTest = TRUE;
                  }
              }
          }


        /* That's all for minimum info */
        if (fMinimumInfo)
          continue;


        /* If this is DOS 3.2 or better, we can use IOCTL call 0Dh,  */
        /*   Generic Block Device Request.  This is where a great    */
        /*   deal of useful information can be found.  Unfortunately */
        /*   this call may require that the disk be "hit."           */

        if (wDosVersion >= 320)
          {
            BYTE abParamBlock[256];   /* Parameter block buffer */
            BYTE FAR *fbByte = NULL;  /* Far pointer to a byte  */
            WORD *pwWord = NULL;      /* WORD pointer           */


            /* Prepare for the Generic Block Device Request */

            fbByte = abParamBlock;

            /* Clear out the register structure */
            memset (&inregs, '\0', sizeof (union REGS));

            inregs.x.ax = 0x440D;
            inregs.h.bl = (BYTE) (i + 1);
            inregs.x.cx = 0x0860;
            sregs.ds    = FP_SEG (fbByte);
            inregs.x.si = FP_SEG (fbByte);
            inregs.x.dx = FP_OFF (fbByte);
            inregs.x.di = FP_OFF (fbByte);
            int86x (0x21, &inregs, &outregs, &sregs);


            /* CF is clear if IOCTL call was successful */
            if (outregs.x.cflag == 0)
              {
                /* Set the disk type */
                if (pDisk->asdi[wDriveIndex].wDriveType != DISK_SUBST_DRIVE &&
                    abParamBlock[1] < 0x0A)
                  {
                    wDriveType = abParamBlock[1] + 2;
                    pDisk->asdi[wDriveIndex].wDriveType = wDriveType;
                    strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                            paszDriveTypes[wDriveType]);
                  }

                /* Set the number of cylinders */
                pwWord = (WORD *) &abParamBlock[4];
                pDisk->asdi[wDriveIndex].wCylinders = *pwWord;


                /* Set the bytes per sector */
                pwWord = (WORD *) &abParamBlock[7];
                pDisk->asdi[wDriveIndex].wBytesPerSector = *pwWord;


                /* Set the sectors per track */
                pwWord = (WORD *) &abParamBlock[20];
                pDisk->asdi[wDriveIndex].wSectorsPerTrack = *pwWord;


                /* Set the number of heads */
                pwWord = (WORD *) &abParamBlock[22];
                pDisk->asdi[wDriveIndex].wHeads = *pwWord;


                /* BIOS Call is not necessary, we have the data */
                fUseBiosCall = FALSE;
              }
            else if (pDisk->asdi[wDriveIndex].wDriveType ==
                        DISK_FLOPPY_DRIVE)
              {
                /* The IOCTL call failed on a floppy drive. */
                /*   Use the BIOS call instead              */
                fUseBiosCall = TRUE;
              }
            else if (pDisk->asdi[wDriveIndex].wDriveType ==
                        DISK_FIXED_DISK)
              {
                /* This is a special case drive.  For now, I'll   */
                /*   assume all special case drives are RAM Disks */

                pDisk->asdi[wDriveIndex].wDriveType = DISK_RAM_DISK;
                strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                        paszDriveTypes[DISK_RAM_DISK]);


                /* BIOS call will not be used on this drive type */
                fUseBiosCall = FALSE;
              }
          }


        if (wDosVersion < 320)
          {
            /* This is DOS 3.1 or lower.  Use the BIOS call */
            fUseBiosCall = TRUE;
          }


        /* BIOS Call to determine disk characteristics */

        if (fUseBiosCall)
          {
            if (pDisk->asdi[wDriveIndex].wDriveType == DISK_FLOPPY_DRIVE)
              {
                /* Get the info for the floppy drive */

                inregs.h.ah = 0x08;
                inregs.h.dl = (BYTE) wNmbrFloppies;
                int86x (0x13, &inregs, &outregs, &sregs);

                if (outregs.x.cflag == 0 &&
                    outregs.h.dl > (BYTE) wNmbrFloppies);
                  {
                    /* Award and AMI BIOS may have bits 10-11 */
                    /*   of cylinder number in DH bits 6-7    */

                    pDisk->asdi[wDriveIndex].wHeads     =
                        outregs.h.dh & 0x3F + 1;

                    pDisk->asdi[wDriveIndex].wCylinders =
                        (WORD) outregs.h.ch +
                        ((((WORD) outregs.h.cl) & 0xC0) << 2) + 1;


                    /* Set the floppy type */

                    switch (outregs.h.bl)
                      {
                        case 1:
                          pDisk->asdi[wDriveIndex].wDriveType =
                              DISK_525_360K;
                          strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                                  paszDriveTypes[DISK_525_360K]);
                          break;

                        case 2:
                          pDisk->asdi[wDriveIndex].wDriveType =
                              DISK_525_12M;
                          strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                                  paszDriveTypes[DISK_525_12M]);
                          break;

                        case 3:
                          pDisk->asdi[wDriveIndex].wDriveType =
                              DISK_35_720K;
                          strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                                  paszDriveTypes[DISK_35_720K]);
                          break;

                        case 4:
                          pDisk->asdi[wDriveIndex].wDriveType =
                              DISK_35_144M;
                          strcpy (pDisk->asdi[wDriveIndex].szDriveType,
                                  paszDriveTypes[DISK_35_144M]);
                          break;
                      }
                  }
              }
            else
              {
                /* This is a fixed disk.  Check to see if */
                /*   SWBIOS is installed for this drive   */

                inregs.h.ah = 0xF9;
                inregs.h.dl = (BYTE) (wNmbrFixedDisks & 0x80);
                int86 (0x13, &inregs, &outregs);

                if (outregs.x.cflag == 0);
                  {
                    /* Find out the true number of cylinders */
                    /*   for this fixed disk                 */

                    inregs.h.ah = 0xFE;
                    inregs.h.dl = (BYTE) (wNmbrFixedDisks + 0x80);
                    int86 (0x13, &inregs, &outregs);

                    /* True cylinder count is in DX */
                    pDisk->asdi[wDriveIndex].wCylinders =
                        outregs.x.dx + 1;
                  }


                /* Call INT 13h/AH=08h to determine disk information */

                inregs.h.ah = 0x08;
                inregs.h.dl = (BYTE) (wNmbrFixedDisks + 0x80);
                int86 (0x13, &inregs, &outregs);

                if (outregs.x.cflag == 0 &&
                    outregs.h.dl > (BYTE) wNmbrFixedDisks);
                  {
                    /* Award and AMI BIOS may have bits 10-11 */
                    /*   of cylinder number in DH bits 6-7    */

                    pDisk->asdi[wDriveIndex].wHeads =
                        outregs.h.dh & 0x3F + 1;


                    /* If the SWBIOS call didn't tell us the number */
                    /*   of cylinders, get it from this call        */

                    if (pDisk->asdi[wDriveIndex].wCylinders == 0)
                      {
                        pDisk->asdi[wDriveIndex].wCylinders =
                            ((WORD) outregs.h.ch) +
                            ((((WORD) outregs.h.cl) & 0xC0) << 2) +
                            ((((WORD) outregs.h.dh) & 0xC0) << 4) + 1;
                      }
                  }
              }
          }


        /* Free space isn't checked on floppy drives */

        wDriveType = pDisk->asdi[wDriveIndex].wDriveType;

        if (wDriveType == DISK_FLOPPY_DRIVE          ||
            wDriveType == DISK_525_360K              ||
            wDriveType == DISK_525_12M               ||
            wDriveType == DISK_35_720K               ||
            wDriveType == DISK_SINGLE_DENSITY_8_INCH ||
            wDriveType == DISK_DOUBLE_DENSITY_8_INCH ||
            wDriveType == DISK_35_144M               ||
            wDriveType == DISK_35_288M)
          {
            fSkipSizeTest = TRUE;

            /* Bump the number of floppies */
            ++wNmbrFloppies;
          }


        /* Free space isn't checked on CD-ROM drives, either */

        if (wDriveType == DISK_CD_ROM_DRIVE)
          fSkipSizeTest = TRUE;


        if (wDriveType == DISK_FIXED_DISK)
          {
            /* Determine CMOS characteristics */
            GetCmosDiskInfo (wNmbrFixedDisks, pDisk, wDriveIndex);

            /* Bump the number of fixed disks */
            ++wNmbrFixedDisks;
          }


        if (fSkipSizeTest == FALSE)
          {
            /* Determine the size of the drive, */
            /*   and the amount of free space   */

            inregs.h.ah = 0x36;
            inregs.h.dl = (BYTE) (i + 1);
            int86 (0x21, &inregs, &outregs);

            if (outregs.x.ax != 0xFFFF)
              {
                /* Free space is AX * BX * CX */
                pDisk->asdi[wDriveIndex].dwFreeBytes = (DWORD) outregs.x.ax *
                                                       (DWORD) outregs.x.bx *
                                                       (DWORD) outregs.x.cx;

                /* Total space (size of drive) is AX * CX * DX */
                pDisk->asdi[wDriveIndex].dwTotalBytes = (DWORD) outregs.x.ax *
                                                        (DWORD) outregs.x.cx *
                                                        (DWORD) outregs.x.dx;


                /* If it has not already been determined, set the number */
                /*   of bytes per sector.                                */

                if (pDisk->asdi[wDriveIndex].wBytesPerSector == 0)
                  pDisk->asdi[wDriveIndex].wBytesPerSector = outregs.x.cx;
              }
          }
      }


  /* Restore the current drive */
  DosSetCurrentDrive (bCurrentDrive);

  return (FALSE);
}


/*********************************************************************
 * GetCmosDiskInfo - Obtains the CMOS drive parameters.
 *
 * wHardDriveNmbr - Hard disk number (0 or 1).
 * pDisk          - Structure for storing disk information.
 * wDriveIndex    - Index to pDisk.
 *
 * Returns:  TRUE if this is a valid drive number.  FALSE if not.
 *********************************************************************/

VOID _cdecl GetCmosDiskInfo (WORD wHardDriveNmbr,
                             DISK_STRUCT *pDisk,
                             WORD wDriveIndex)
{
  WORD i;               /* Looping variable */
  WORD wCmosIndex = 0;  /* Value to obtain  */
  WORD wCmosValue;      /* Value from CMOS  */


  /* Obtain the drive type appropriate for this drive */
  outp (0x70, 0x12);

  /* Wait */
  for (i = 0; i < 1; ++i)
    ;

  /* Input the drive type */
  wCmosValue = inp (0x71);


  /* First HD type is in bits 4-7, second is in bits 0-3 */
  if (wHardDriveNmbr == 0)
    wCmosValue = (wCmosValue & 0xF0) >> 4;
  else
    wCmosValue = (wCmosValue & 0x0F);


  /* If the HD type is 15 (0Fh), look elsewhere */
  if (wCmosValue == 0x0F)
    {
      /* Choose the appropriate value for the hard drive number */
      if (wHardDriveNmbr == 0)
        wCmosIndex = 0x19;
      else
        wCmosIndex = 0x1A;

      outp (0x70, wCmosIndex);

      /* Wait */
      for (i = 0; i < 1; ++i)
        ;

      /* Get the hard drive type */
      wCmosValue = inp (0x71);
    }


  /* Set the CMOS Drive Type */
  pDisk->asdi[wDriveIndex].wCmosDriveType = wCmosValue;


  /* Get the drive parameters */


  /* Is this a user defined drive type (48 or 49) */
  if (pDisk->asdi[wDriveIndex].wCmosDriveType == 48 ||
      pDisk->asdi[wDriveIndex].wCmosDriveType == 49)
    {
      /* Set the base index for the drive paramter table */
      if (wHardDriveNmbr == 0)
        wCmosIndex = 0x20;
      else
        wCmosIndex = 0x35;


      /* Cylinders */
      outp (0x70, wCmosIndex + 1);

      for (i = 0; i < 1; ++i)
        ;

      wCmosValue = inp (0x71);
      wCmosValue = wCmosValue << 8;

      outp (0x70, wCmosIndex + 1);

      for (i = 0; i < 1; ++i)
        ;

      wCmosValue = wCmosValue + inp (0x70);
      pDisk->asdi[wDriveIndex].wCmosCylinders = wCmosValue;


      /* Heads */
      outp (0x70, wCmosIndex + 2);

      for (i = 0; i < 1; ++i)
        ;

      pDisk->asdi[wDriveIndex].wCmosHeads = inp (0x71);


      /* Sectors per track */
      outp (0x70, wCmosIndex + 7);

      for (i = 0; i < 1; ++i)
        ;

      pDisk->asdi[wDriveIndex].wCmosSectorsPerTrack = inp (0x71);
    }
  else
    {
      CMOS_DRIVE_TYPE FAR *pCmosInfo = NULL;  /* CMOS Info in BIOS  */
      WORD wTableNmbr;                        /* Drive Table Number */
      union REGS regs;                        /* int86x registers   */
      struct SREGS sregs;                     /* int86x segments    */


      /* This is a predefined drive type -- located in the BIOS */

      /* Use the appropriate lookup table */
      if (wHardDriveNmbr == 0)
        wTableNmbr = 0x41;
      else
        wTableNmbr = 0x46;


      /* Get the table's address */
      regs.h.ah = 0x35;
      regs.h.al = (BYTE) wTableNmbr;
      int86x (0x21, &regs, &regs, &sregs);

      pCmosInfo = (CMOS_DRIVE_TYPE FAR *)
                      (((DWORD) sregs.es << 16) + regs.x.bx);


      /* Get the information */
      pDisk->asdi[wDriveIndex].wCmosCylinders =
                                      pCmosInfo->wNmbrCylinders;
      pDisk->asdi[wDriveIndex].wCmosHeads     =
                                      pCmosInfo->bNmbrHeads;
      pDisk->asdi[wDriveIndex].wCmosSectorsPerTrack =
                                      pCmosInfo->bSectorsPerTrack;
    }


  /* Set the CMOS type on the "Fixed Disk" line */

  sprintf (pDisk->asdi[wDriveIndex].szDriveType,
           "Fixed Disk, CMOS Type %d",
           pDisk->asdi[wDriveIndex].wCmosDriveType);
}


/*********************************************************************
 * ValidDrive - Determines if this drive number is a valid drive.
 *
 * bDrive - Drive letter to test.
 *
 * Returns:  TRUE if this is a valid drive number.  FALSE if not.
 *********************************************************************/

BOOL ValidDrive (BYTE bDrive)
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

BYTE DosGetCurrentDrive (VOID)
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

BYTE DosSetCurrentDrive (BYTE bDrive)
{
  union REGS inregs, outregs;   /* Register structures for int86() */


  /* DOS call to return the current drive number */
  inregs.h.ah = 0x0e;
  inregs.h.dl = bDrive;
  int86 (0x21, &inregs, &outregs);

  return (outregs.h.al);
}


/*********************************************************************
 * GetDiskProgList - Gets the list of disk related programs installed
 *                   in memory (JOIN, ASSIGN, MSCDEX, etc).
 *
 * pDisk - Pointer to disk information structure.
 *********************************************************************/

VOID GetDiskProgList (DISK_STRUCT *pDisk, WORD wDosVersion)
{
  union  REGS  inregs, outregs;   /* Register structures for int86() calls */
  struct SREGS sregs;             /* Segment registers for int86x() calls  */
  BYTE FAR *fbByte = NULL;        /* Far pointer to a BYTE                 */
  WORD   i;                       /* Looping variable                      */

  /* Determine if JOIN.EXE is being used */
  if (wDosVersion >= 310)
    {
      inregs.h.ah = 0x52;
      int86x (0x21, &inregs, &outregs, &sregs);
      fbByte = (BYTE FAR *) ((DWORD) sregs.es << 16) + outregs.x.bx;
      if (fbByte[0x34] != 0)
        pDisk->fJoinInstalled = TRUE;
    }


  /* Determine if SHARE.EXE is installed */
  inregs.x.ax = 0x1000;
  int86 (0x2F, &inregs, &outregs);
  if (outregs.h.al == 0xFF)
    pDisk->fShareInstalled = TRUE;


  /* Determine if ASSIGN.EXE installed */
  inregs.x.ax = 0x0600;
  int86 (0x2F, &inregs, &outregs);
  if (outregs.h.al == 0xFF)
    {
      pDisk->fAssignInstalled = TRUE;

      /* Load up the ASSIGN drive table */
      inregs.x.ax = 0x0601;
      int86x (0x2F, &inregs, &outregs, &sregs);
      if (sregs.es > 0x0000)
        {
          WORD wAssignIndex;    /* Index to the AssignTable */
          /* Point to the drive assignment table */
          fbByte = (BYTE FAR *) ((DWORD) sregs.es << 16) + 0x0102;


          /* Loop through, looking for assignments */
          /*   (ASSIGN reserves room for A:-Z:, no */
          /*   matter what LASTDRIVE is set to).   */

          for (i = 1, wAssignIndex = 0; i < MAX_ASSIGN_TABLE; ++i)
            if (fbByte[i] != (BYTE) i)
              {
                pDisk->atAssignTable[wAssignIndex].chAssignTo   = (CHAR) i;
                pDisk->atAssignTable[wAssignIndex].chAssignFrom = fbByte[i];

                ++wAssignIndex;
              }
        }
    }


  /* Determine if APPEND.EXE installed */
  inregs.x.ax = 0xB700;
  int86 (0x2F, &inregs, &outregs);
  if (outregs.h.al == 0xFF)
    {
      CHAR FAR * fpszAppendPath = NULL; /* Far pointer to the APPEND path */

      pDisk->fAppendInstalled = TRUE;

      /* Locate the APPEND path */
      inregs.x.ax = 0xB704;
      int86x (0x2f, &inregs, &outregs, &sregs);

      /* Place it into the Disk Info structure */
      fpszAppendPath = (CHAR FAR *) ((DWORD) sregs.es << 16) + outregs.x.di;
      _fstrncpy ((CHAR FAR *) pDisk->szAppendPath, fpszAppendPath,
                 MAX_APPEND_PATH - 1);
      pDisk->szAppendPath[MAX_APPEND_PATH - 1] = '\0';
    }


  /* Determine if MSCDEX.EXE is installed */
  inregs.x.ax = 0x1500;
  inregs.x.bx = 0x0000;
  int86 (0x2F, &inregs, &outregs);
  if (outregs.x.bx != 0x0000)
    {
      /* MSCDEX is installed -- Determine version number */

      inregs.x.ax = 0x150C;
      inregs.x.bx = 0x0000;
      int86 (0x2F, &inregs, &outregs);
      if (outregs.x.bx == 0)
        {
          /* Set the version to 1.x */
          pDisk->wMscdexMajor = 1;
          pDisk->wMscdexMinor = 0xFF;
        }
      else
        {
          pDisk->wMscdexMajor = outregs.h.bh;
          pDisk->wMscdexMinor = outregs.h.bl;
        }
    }
}


/*********************************************************************
 * SprintDiskInfo - Put disk information into a set of strings to be
 *                  printed or displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintDiskInfo (DISK_STRUCT *pDisk,
                      CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;        /* Number of strings                    */
  WORD wNmbrChars;          /* Number of characters in the strings  */
  WORD wUnderlineLength;    /* Length of the underline string       */
  WORD wDriveIndex;         /* Index to the structure of disk data  */
  WORD i;                   /* Looping variables                    */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers */
  CHAR chBuffer[80];        /* Buffer for string data               */


  /* Summary Strings */
  if (szSumStrings != NULL)
    {
      WORD wDrivesThisLine = 0;   /* Current number of drive letters */
                                  /*   on this line.                 */
      i = 0;
      szSumStrings[i][0] = '\0';

      for (wDriveIndex = 0; wDriveIndex < pDisk->wNmbrDrives; ++wDriveIndex)
        {
          /* Drive Letter */
          sprintf (chBuffer, "%c: ", pDisk->asdi[wDriveIndex].chDriveLetter);

          /* Is there room for it on this line */
          if (++wDrivesThisLine > MAX_DRIVES_PER_LINE)
            {
              wDrivesThisLine = 1;

              /* Is there another line that we can use */
              if (++i > 1)
                {
                  /* There are more drives than will fit on   */
                  /*   summary screen.  Put "..." at the end. */
                  /*
                  wNmbrChars = strlen (szSumStrings[1]);
                  szSumStrings[1][wNmbrChars - 4] = '\0';
                  strcat (szSumStrings[1], "...");
                  */
                  return (NULL);
                }
              else
                szSumStrings[i][0] = '\0';
            }

          strncat (szSumStrings[i], chBuffer,
                   MAX_SUMM_INFO - strlen (szSumStrings[0]));
        }

      return (NULL);
    }


  /* Calculate the amount of space required for the strings */

  wNmbrStrings = pDisk->wNmbrDrives + 3;

  for (i = 0; i < pDisk->wNmbrDrives; ++i)
    {
      if (pDisk->asdi[i].wCylinders > 0 ||
          pDisk->asdi[i].wHeads > 0)
        ++wNmbrStrings;

      if (pDisk->asdi[i].wBytesPerSector > 0 ||
          pDisk->asdi[i].wSectorsPerTrack > 0)
        ++wNmbrStrings;

      if (pDisk->asdi[i].wDriveType == DISK_FIXED_DISK)
        {
          /* If the CMOS settings do not match, display the lines. */
          /*   The CMOS cylinders can be off by 2.                 */

          if (!((pDisk->asdi[i].wCylinders      ==
                 pDisk->asdi[i].wCmosCylinders)     ||
                (pDisk->asdi[i].wCylinders + 1  ==
                 pDisk->asdi[i].wCmosCylinders)     ||
                (pDisk->asdi[i].wCylinders + 2  ==
                 pDisk->asdi[i].wCmosCylinders)     &&
                pDisk->asdi[i].wHeads           ==
                pDisk->asdi[i].wCmosHeads           &&
                pDisk->asdi[i].wSectorsPerTrack ==
                pDisk->asdi[i].wCmosSectorsPerTrack))
            {
              wNmbrStrings += 3;
            }
        }
    }
  if (pDisk->fJoinInstalled)
    ++wNmbrStrings;

  if (pDisk->fSubstInstalled)
    ++wNmbrStrings;

  if (pDisk->fShareInstalled)
    ++wNmbrStrings;

  if (pDisk->fAssignInstalled)
    ++wNmbrStrings;

  if (pDisk->fAppendInstalled)
    ++wNmbrStrings;

  if (pDisk->wMscdexMajor != 0)
    ++wNmbrStrings;


  wUnderlineLength = strlen (pszDiskUnderline);

  wNmbrChars   = wNmbrStrings * (wUnderlineLength + 1);


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the first two strings in place */

  Qstrcpy (pqszStrings[0], pszDiskHeader);
  pqszStrings[1] = pqszStrings[0] + Qstrlen (pqszStrings[0]) + 1;

  Qstrcpy (pqszStrings[1], pszDiskUnderline);
  pqszStrings[2] = pqszStrings[1] + wUnderlineLength + 1;


  /* Put the Disk information in place */

  for (i = 2, wDriveIndex = 0; wDriveIndex < pDisk->wNmbrDrives;
       ++i, ++wDriveIndex)
    {
      WORD wLength;       /* Current length of string        */


      /* Clear out the string */
      Qmemset (pqszStrings[i], ' ', wUnderlineLength);
      pqszStrings[i][wUnderlineLength] = '\0';


      /* Drive Letter */
      wLength = sprintf (chBuffer, "%c:",
                         pDisk->asdi[wDriveIndex].chDriveLetter);
      Qstrncpy (&pqszStrings[i][DISK_DRIVE_COL], chBuffer, wLength);


      /* Type */
      Qstrncpy (&pqszStrings[i][DISK_TYPE_COL],
               pDisk->asdi[wDriveIndex].szDriveType,
               strlen (pDisk->asdi[wDriveIndex].szDriveType));


      /* Free Space */
      if (pDisk->asdi[wDriveIndex].dwFreeBytes > (DWORD) 0 ||
          pDisk->asdi[wDriveIndex].dwTotalBytes > (DWORD) 0)
        {
          if (pDisk->asdi[wDriveIndex].dwFreeBytes > 9000000L)
            wLength = sprintf (chBuffer, "%9luM",
                          pDisk->asdi[wDriveIndex].dwFreeBytes / (1024L * 1024L));
          else
            wLength = sprintf (chBuffer, "%9luK",
                          pDisk->asdi[wDriveIndex].dwFreeBytes / 1024);
          Qstrncpy (&pqszStrings[i][DISK_FREE_SPACE_COL], chBuffer, wLength);
        }


      /* Total Free */
      if (pDisk->asdi[wDriveIndex].dwTotalBytes > (DWORD) 0)
        {
          if (pDisk->asdi[wDriveIndex].dwTotalBytes > 9000000L)
            wLength = sprintf (chBuffer, "%9luM",
                          pDisk->asdi[wDriveIndex].dwTotalBytes / (1024L * 1024L));
          else
            wLength = sprintf (chBuffer, "%9luK",
                          pDisk->asdi[wDriveIndex].dwTotalBytes / 1024);
          Qstrncpy (&pqszStrings[i][DISK_TOTAL_FREE_COL], chBuffer, wLength);
        }


      /* Prep the current and next strings */
      PrepNextString (pqszStrings, i);


      /* Cylinders / Heads line */

      if (pDisk->asdi[wDriveIndex].wCylinders > 0 ||
          pDisk->asdi[wDriveIndex].wHeads > 0)
        {
          /* Next string */
          ++i;


          /* Clear out the string */
          Qmemset (pqszStrings[i], ' ', DISK_EXTRA_INFO_COL);
          pqszStrings[i][DISK_EXTRA_INFO_COL] = '\0';


          /* Cylinders */
          if (pDisk->asdi[wDriveIndex].wCylinders > 0)
            {
              sprintf (chBuffer, pszCylinders,
                       pDisk->asdi[wDriveIndex].wCylinders);
              Qstrcat (pqszStrings[i], chBuffer);
            }


          /* Comma space */
          if (pDisk->asdi[wDriveIndex].wCylinders > 0 &&
              pDisk->asdi[wDriveIndex].wHeads > 0)
            Qstrcat (pqszStrings[i], pszCommaSpace);


          /* Heads */
          if (pDisk->asdi[wDriveIndex].wHeads > 0)
            {
              sprintf (chBuffer, pszHeads,
                       pDisk->asdi[wDriveIndex].wHeads);
              Qstrcat (pqszStrings[i], chBuffer);
            }


          /* Prep the current and next strings */
          PrepNextString (pqszStrings, i);
        }


      /* Bytes per sector / Sectors per track line */

      if (pDisk->asdi[wDriveIndex].wBytesPerSector > 0 ||
          pDisk->asdi[wDriveIndex].wSectorsPerTrack > 0)
        {
          /* Next string */
          ++i;


          /* Clear out the string */
          Qmemset (pqszStrings[i], ' ', DISK_EXTRA_INFO_COL);
          pqszStrings[i][DISK_EXTRA_INFO_COL] = '\0';


          /* Bytes per sector */
          if (pDisk->asdi[wDriveIndex].wBytesPerSector > 0)
            {
              sprintf (chBuffer, pszBytesPerSector,
                       pDisk->asdi[wDriveIndex].wBytesPerSector);
              Qstrcat (pqszStrings[i], chBuffer);
            }


          /* Comma space */
          if (pDisk->asdi[wDriveIndex].wBytesPerSector > 0 &&
              pDisk->asdi[wDriveIndex].wSectorsPerTrack > 0)
            Qstrcat (pqszStrings[i], pszCommaSpace);


          /* Sectors per track */
          if (pDisk->asdi[wDriveIndex].wSectorsPerTrack > 0)
            {
              sprintf (chBuffer, pszSectorsPerTrack,
                       pDisk->asdi[wDriveIndex].wSectorsPerTrack);
              Qstrcat (pqszStrings[i], chBuffer);
            }


          /* Prep the current and next strings */
          PrepNextString (pqszStrings, i);
        }

      /* If this is a fixed disk, check the CMOS settings */
      if (pDisk->asdi[wDriveIndex].wDriveType == DISK_FIXED_DISK)
        {
          /* If the CMOS settings do not match, display the lines. */
          /*   The CMOS cylinders can be off by 2.                 */

          if (!((pDisk->asdi[wDriveIndex].wCylinders      ==
                 pDisk->asdi[wDriveIndex].wCmosCylinders)     ||
                (pDisk->asdi[wDriveIndex].wCylinders + 1  ==
                 pDisk->asdi[wDriveIndex].wCmosCylinders)     ||
                (pDisk->asdi[wDriveIndex].wCylinders + 2  ==
                 pDisk->asdi[wDriveIndex].wCmosCylinders)     &&
                pDisk->asdi[wDriveIndex].wHeads           ==
                pDisk->asdi[wDriveIndex].wCmosHeads           &&
                pDisk->asdi[wDriveIndex].wSectorsPerTrack ==
                pDisk->asdi[wDriveIndex].wCmosSectorsPerTrack))
            {
              /* Clear out the string */
              Qmemset (pqszStrings[++i], ' ', wUnderlineLength);
              pqszStrings[i][wUnderlineLength] = '\0';


              /* CMOS title */
              Qstrcpy (&pqszStrings[i][DISK_TYPE_COL],
                      "CMOS Fixed Disk Parameters");
              PrepNextString (pqszStrings, i++);

              /* Clear out the string */
              Qmemset (pqszStrings[i], ' ', DISK_EXTRA_INFO_COL);
              pqszStrings[i][DISK_EXTRA_INFO_COL] = '\0';


              /* Cylinders */
              sprintf (chBuffer, pszCylinders,
                       pDisk->asdi[wDriveIndex].wCmosCylinders);
              Qstrcat (pqszStrings[i], chBuffer);
              Qstrcat (pqszStrings[i], pszCommaSpace);

              /* Heads */
              sprintf (chBuffer, pszHeads,
                       pDisk->asdi[wDriveIndex].wCmosHeads);
              Qstrcat (pqszStrings[i], chBuffer);


              /* Next line */
              PrepNextString (pqszStrings, i++);

              /* Clear out the string */
              Qmemset (pqszStrings[i], ' ', DISK_EXTRA_INFO_COL);
              pqszStrings[i][DISK_EXTRA_INFO_COL] = '\0';


              /* Sectors per track */
              sprintf (chBuffer, pszSectorsPerTrack,
                       pDisk->asdi[wDriveIndex].wCmosSectorsPerTrack);
              Qstrcat (pqszStrings[i], chBuffer);

              PrepNextString (pqszStrings, i);
            }
        }
    }


  /* JOIN */
  if (pDisk->fJoinInstalled)
    {
      /* Clear out the string */
      pqszStrings[i][0] = '\0';

      Qstrcat (pqszStrings[i], pszJoinInstalled);

      /* Prep the current and next strings */
      PrepNextString (pqszStrings, i++);
    }


  /* SUBST */
  if (pDisk->fSubstInstalled)
    {
      /* Clear out the string */
      pqszStrings[i][0] = '\0';

      Qstrcat (pqszStrings[i], pszSubstInstalled);

      /* Prep the current and next strings */
      PrepNextString (pqszStrings, i++);
    }


  /* SHARE */
  if (pDisk->fShareInstalled)
    {
      /* Clear out the string */
      pqszStrings[i][0] = '\0';

      Qstrcat (pqszStrings[i], pszShareInstalled);

      /* Prep the current and next strings */
      PrepNextString (pqszStrings, i++);
    }


  /* ASSIGN */
  if (pDisk->fAssignInstalled)
    {
      /* Clear out the string */
      pqszStrings[i][0] = '\0';

      if (pDisk->atAssignTable[0].chAssignTo == '\0')
        Qstrcat (pqszStrings[i], pszAssignInstalled);
      else
        {
          WORD u;   /* Looping variable */

          Qstrcat (pqszStrings[i], pszAssign);

          /* List the ASSIGNed drive letters */
          for (u = 0; pDisk->atAssignTable[u].chAssignTo != '\0'; ++u)
            {
              sprintf (chBuffer, " %c:=%c:",
                       pDisk->atAssignTable[u].chAssignTo + 'A' - 1,
                       pDisk->atAssignTable[u].chAssignFrom + 'A' - 1);

              Qstrcat (pqszStrings[i], chBuffer);
            }
        }

      /* Prep the current and next strings */
      PrepNextString (pqszStrings, i++);
    }


  /* APPEND */
  if (pDisk->fAppendInstalled)
    {
      /* Clear out the string */
      pqszStrings[i][0] = '\0';

      if (pDisk->szAppendPath[0] == '\0')
        Qstrcat (pqszStrings[i], pszAppendInstalled);
      else
        {
          Qstrcat (pqszStrings[i], pszAppendPath);
          Qstrcat (pqszStrings[i], pDisk->szAppendPath);
        }

      /* Prep the current and next strings */
      PrepNextString (pqszStrings, i++);
    }


  /* MSCDEX */
  if (pDisk->wMscdexMajor != 0)
    {
      /* Clear out the string */
      pqszStrings[i][0] = '\0';

      /* Special case for MSCDEX 1.x */
      if (pDisk->wMscdexMinor == 0xFF)
        sprintf (chBuffer, pszMscdex1x, pDisk->wMscdexMajor);
      else
        sprintf (chBuffer, pszMscdexInstalled, pDisk->wMscdexMajor,
                 pDisk->wMscdexMinor);

      Qstrcat (pqszStrings[i], chBuffer);

      /* Prep the current and next strings */
      PrepNextString (pqszStrings, i++);
    }


  /* LASTDRIVE */

  /* Clear out the string */
  pqszStrings[i][0] = '\0';

  sprintf (chBuffer, pszLastdrive, pDisk->chLastDrive);
  Qstrcat (pqszStrings[i], chBuffer);


  /* Set the last pointer to a NULL */
  pqszStrings[++i] = NULL;

  /* Return the pointer to pqszStrings */
  return (pqszStrings);
}
