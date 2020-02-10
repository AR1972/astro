/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * IRQINFO.C - Source file for serial port detection code.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/* Stores the interrupt vector table at program startup */

DWORD *padwIntTable = NULL;


/*********************************************************************
 * GetIrqInfo - Gets the hardware interrupt (IRQ) information.
 *
 * pIrq         - IRQ structure for storing data
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetIrqInfo (IRQ_STRUCT *pIrq, BOOL fMinimumInfo)
{
  /* Structure pointers for holding data related to IRQs */

  COMPUTER_STRUCT       *pComputer = NULL;
  MOUSE_STRUCT          *pMouse    = NULL;
  DISK_STRUCT           *pDisk     = NULL;
  LPT_STRUCT            *pLpt      = NULL;
  COM_STRUCT            *pCom      = NULL;
  TSR_PROGRAMS_STRUCT   *pTsr      = NULL;
  DEVICE_DRIVER_STRUCT  *pDevice   = NULL;

  BOOL fReturnValue = FALSE;    /* Return value from various GetInfo calls */
  WORD wSize;                   /* Size, in bytes, to store the data       */
  WORD i;                       /* Looping variable                        */
  VOID *pStructForInfo = NULL;  /* Pointer to a generic struct             */
  BYTE FAR *fpBunchOfBytes = NULL;


  /* There is no minimum info from this routine */
  if (fMinimumInfo)
    return (FALSE);

#if HEAP_DEBUG
  HeapCheck ("Beginning of GetIrqInfo");
#endif

  /* See if any low memory locations change */
  fpBunchOfBytes = _fmalloc (256 * 4);
  _fmemcpy (fpBunchOfBytes, (BYTE FAR *) 0x00000000, 256*4);

#if HEAP_DEBUG
  HeapCheck ("After copy of BunchOfBytes");
#endif

  /* Fill the structures with the information related to IRQs */

  for (i = 0; fReturnValue == FALSE && rwIrqRecordTypes[i] != 0; ++i)
    {
      wSize = GetInfoSize (rwIrqRecordTypes[i], FALSE);

      if (wSize == 0)
        {
          fReturnValue = TRUE;
          break;
        }

      pStructForInfo = malloc (wSize);

      if (pStructForInfo == NULL)
        {
          OutOfMemory();
          fReturnValue = TRUE;
          break;
        }


      /* Zero out the structure before passing it on */
      memset (pStructForInfo, '\0', wSize);

      {
#if HEAP_DEBUG
        CHAR chBuffer[80];
        sprintf (chBuffer, "After malloc for IDI_ %d", rwIrqRecordTypes[i]);
        HeapCheck (chBuffer);
#endif
      }

      switch (rwIrqRecordTypes[i])
        {
          case IDI_COMPUTER_RECORD:
            fReturnValue = GetComputerIrqInfo (pStructForInfo);
            if (!fReturnValue)
              pComputer = (COMPUTER_STRUCT *) pStructForInfo;
            break;

          case IDI_MOUSE_RECORD:
            fReturnValue = GetInfo (rwIrqRecordTypes[i],
                                    pStructForInfo,
                                    FALSE,
                                    FALSE,
                                    FALSE);
            if (!fReturnValue)
              pMouse = (MOUSE_STRUCT *) pStructForInfo;
            break;

          case IDI_DISK_DRIVE_RECORD:
            fReturnValue = GetInfo (rwIrqRecordTypes[i],
                                    pStructForInfo,
                                    TRUE,   /* Minimum Info */
                                    FALSE,
                                    FALSE);
            if (!fReturnValue)
              pDisk = (DISK_STRUCT *) pStructForInfo;
            break;

          case IDI_LPT_RECORD:
            fReturnValue = GetInfo (rwIrqRecordTypes[i],
                                    pStructForInfo,
                                    FALSE,
                                    FALSE,
                                    FALSE);
            if (!fReturnValue)
              pLpt = (LPT_STRUCT *) pStructForInfo;
            break;

          case IDI_COM_RECORD:
            fReturnValue = GetInfo (rwIrqRecordTypes[i],
                                    pStructForInfo,
                                    FALSE,
                                    FALSE,
                                    FALSE);
            if (!fReturnValue)
              pCom = (COM_STRUCT *) pStructForInfo;
            break;
        }
    }

  if (fReturnValue)
    {
      free (pCom);
      free (pLpt);
      free (pDisk);
      free (pMouse);
      free (pComputer);

      return (fReturnValue);
    }


  /* Determine how many IRQs the computer has:  A Cascaded IRQ2 */
  /*   means that there are 16 IRQs, without means 8 (IRQs are  */
  /*   numbered starting at zero).                              */

  if (pComputer->fCascadeIntLvl2)
    pIrq->wNmbrIrqs = 15;
  else
    pIrq->wNmbrIrqs = 7;


  /* Set the serial strings for PS/2 or non-PS/2 computers */
  if (pComputer->wComputerClass == COMPUTER_CLASS_PS2)
    {
      paszAtIrqDescriptions[3][5]  = '\0';
      paszAtIrqDescriptions[4][11] = ' ';
    }


  /* Set the "detected" strings */
  SetIrqDetectedStrings (pIrq,
                         pComputer,
                         pMouse,
                         pDisk,
                         pLpt,
                         pCom);
  _memmax();
  free (pComputer);
  free (pMouse);
  free (pDisk);
  free (pLpt);
  free (pCom);
  _memmax();


  /* Get TSR info */
  wSize = GetInfoSize (IDI_TSR_PROGRAMS_RECORD, FALSE);
  if (wSize == 0)
    return (TRUE);

  pTsr = malloc (wSize);
  if (pTsr == NULL)
    {
      OutOfMemory();
      return (TRUE);
    }

  /* Zero out the structure before passing it on */
  memset (pTsr, '\0', wSize);
  fReturnValue = GetInfo (IDI_TSR_PROGRAMS_RECORD, pTsr,
                          FALSE, FALSE, FALSE);


  /* Get Device Driver info */
  wSize = GetInfoSize (IDI_DEVICE_DRIVERS_RECORD, FALSE);
  if (wSize == 0)
    return (TRUE);

  pDevice = malloc (wSize);
  if (pDevice == NULL)
    {
      OutOfMemory();
      return (TRUE);
    }

  /* Zero out the structure before passing it on */
  memset (pDevice, '\0', wSize);
  fReturnValue = GetInfo (IDI_DEVICE_DRIVERS_RECORD, pDevice,
                          FALSE, FALSE, FALSE);

  _memmax();
  /* Set the Interrupt vector address and the "Handled By" string */
  SetIrqHandledByStrings (pIrq,
                          pTsr,
                          pDevice);


  /* Free up the structure data */

  free (pDevice);
  free (pTsr);

#if HEAP_DEBUG
  HeapCheck ("After GetIrqInfo's set of free()s");
#endif

  return (FALSE);
}


/*********************************************************************
 * SetIrqDetectedStrings - Sets the szDetected[][] strings in the IRQ
 *                         data structure.
 *
 * pIrq      - IRQ information structure
 * pComptuer - Computer information structure
 * pMouse    - Mouse information structure
 * pLpt      - LPT Port information structure
 * pCom      - COM Port information structure
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL SetIrqDetectedStrings (IRQ_STRUCT       *pIrq,
                            COMPUTER_STRUCT  *pComputer,
                            MOUSE_STRUCT     *pMouse,
                            DISK_STRUCT      *pDisk,
                            LPT_STRUCT       *pLpt,
                            COM_STRUCT       *pCom)
{
  union REGS inregs, outregs;   /* Register structures for int86 call */
  WORD i, u, x;                 /* Looping variables                  */

  for (i = 0; i <= pIrq->wNmbrIrqs; ++i)
    {
      for (u = 0; u < MAX_IRQ_DETECTED_STRINGS; ++u)
        pIrq->IrqxInfo[i].szDetected[u][0] = '\0';

      /* Zero out the index to pIrq->IrqxInfo[i].szDetected[] */
      u = 0;

      /* PC IRQ 5 == AT IRQ 14 */
      if (i == 5 && pComputer->wComputerClass == COMPUTER_CLASS_XT)
        i = 14;

      switch (i)
        {
          /* Timer Click, always assumed to be there */
          case  0:
            {
              pIrq->IrqxInfo[i].fDetected = TRUE;
              strcpy (pIrq->IrqxInfo[i].szDetected[0], pszYes);
              break;
            }

          /* Keyboard, always assumed to be there */
          case  1:
            {
              pIrq->IrqxInfo[i].fDetected = TRUE;
              strcpy (pIrq->IrqxInfo[i].szDetected[0], pszYes);
              break;
            }

          /* Cascade IRQ2 (AT) or I/O Channel (PC, always */
          /*   assumed to be there)                       */
          case  2:
            {
              if (pComputer->wComputerClass == COMPUTER_CLASS_XT)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszYes);
                }
              else if (pComputer->fCascadeIntLvl2)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszYes);
                }
              else
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszNo);
                }

              /* Check for a mouse */
              if (pMouse->wIrq == i)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                          pMouse->szMouseHardwareType);
                }
              break;
            }

          /* COM2: (all), COM4: (non PS/2) */
          case  3:
            {
              /* COM2: index == 1 */
              if (pCom->ComxInfo[1].fComPortDetected)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszCom[2]);
                }

              /* COM4: index == 3 */
              if (pCom->ComxInfo[3].fComPortDetected &&
                  pComputer->wComputerClass != COMPUTER_CLASS_PS2)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszCom[4]);
                }

              /* Check for a mouse */
              if (pMouse->wIrq == i)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                          pMouse->szMouseHardwareType);
                }

              /* If nothing was detected for IRQ 3, set it to "No" */
              if (u == 0)
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }

              break;
            }

          /* COM1:/COM3: (all), COM4: (PS/2) */
          case  4:
            {
              /* COM1: index == 0 */
              if (pCom->ComxInfo[0].fComPortDetected)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszCom[1]);
                }

              /* COM3: index == 2 */
              if (pCom->ComxInfo[2].fComPortDetected)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszCom[3]);
                }

              /* COM4: index == 3 */
              if (pCom->ComxInfo[3].fComPortDetected &&
                  pComputer->wComputerClass == COMPUTER_CLASS_PS2)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszCom[4]);
                }

              /* Check for a mouse */
              if (pMouse->wIrq == i)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                          pMouse->szMouseHardwareType);
                }

              /* If nothing was detected for IRQ 4, set it to "No" */
              if (u == 0)
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }

              break;
            }

          /* LPT2: */
          case  5:
            {
              /* LPT2: index = 1 */
              if (pLpt->LptxInfo[1].fLptPortDetected)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszYes);
                }

              /* Check for a mouse */
              if (pMouse->wIrq == i)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                          pMouse->szMouseHardwareType);
                }

              /* If nothing was detected for IRQ 5, set it to "No" */
              if (u == 0)
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }

              break;
            }

          /* Floppy Controller */
          case  6:
            {
              int86 (0x11, &inregs, &outregs);

              /* Bit zero is set if floppies are present */
              if (outregs.x.ax & 1)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszYes);
                }

              /* Check for a mouse */
              if (pMouse->wIrq == i)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                          pMouse->szMouseHardwareType);
                }

              /* If nothing was detected for IRQ 6, set it to "No" */
              if (u == 0)
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }

              break;
            }

          /* LPT1: */
          case  7:
            {
              /* LPT1: index = 0 */
              if (pLpt->LptxInfo[0].fLptPortDetected)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszYes);
                }

              /* Check for a mouse */
              if (pMouse->wIrq == i)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                          pMouse->szMouseHardwareType);
                }

              /* If nothing was detected for IRQ 7, set it to "No" */
              if (u == 0)
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }

              break;
            }

          /* Real Time Clock */
          case  8:
            {
              if (pComputer->fRealTimeClock)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszYes);
                }
              else
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }

              break;
            }

          /* Redirected IRQ2 */
          case  9:
            {
              if (pComputer->fCascadeIntLvl2)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszYes);
                }
              else
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }
              break;
            }

          /* Reserved:  I don't detect these */
          case 10:
          case 11:
          case 15:
            break;


          case 12:
            /* Check for a mouse */
            if (pMouse->wIrq == i)
              {
                pIrq->IrqxInfo[i].fDetected = TRUE;
                strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                        pMouse->szMouseHardwareType);
              }
            break;

          /* Math Co-processor */
          case 13:
            {
              if (pComputer->wCoProcessor != _NOCOPROCESSOR ||
                  pComputer->wProcessor   == _80486)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszYes);
                }
              else
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }
              break;
            }

          /* Fixed Disk (PC = IRQ 5, non-PC = IRQ 14) */
          case 14:
            {
              /* Restore "i" to the correct value */
              if (i == 14 && pComputer->wComputerClass == COMPUTER_CLASS_XT)
                i = 5;

              for (x = 0; x < pDisk->wNmbrDrives &&
                          pDisk->asdi[x].wDriveType != DISK_FIXED_DISK; ++x)
                ;

              if (x == pDisk->wNmbrDrives)
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszNo);
                }
              else
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++], pszYes);
                }

              /* Check for a mouse */
              if (pMouse->wIrq == i)
                {
                  pIrq->IrqxInfo[i].fDetected = TRUE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[u++],
                          pMouse->szMouseHardwareType);
                }

              /* If nothing was detected for IRQ 4, set it to "No" */
              if (u == 0)
                {
                  pIrq->IrqxInfo[i].fDetected = FALSE;
                  strcpy (pIrq->IrqxInfo[i].szDetected[0], pszNo);
                }

              break;
            }
        }
    }

  return (FALSE);
}


/*********************************************************************
 * SetIrqHandledByStrings - Sets the szHandledBy[] strings and the
 *                          IRQ Address in the IRQ data structure.
 *
 * pIrq    - IRQ information structure
 * pTsr    - TSR Program information structure
 * pDevice - Device Driver information structure
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL SetIrqHandledByStrings (IRQ_STRUCT           *pIrq,
                             TSR_PROGRAMS_STRUCT  *pTsr,
                             DEVICE_DRIVER_STRUCT *pDevice)
{
  BOOL  fFoundHandler;        /* TRUE if IRQ handler found             */
  WORD  i, u;                 /* Looping variables                     */
  WORD  wSwInt;               /* Software interrupt number             */
  WORD  wDeviceHeaderSeg;     /* Segment of the device driver's header */
  DWORD dwLowestMcbSeg;       /* Lowest MCB segment (anything lower is */
                              /*   the DOS System Area)                */
  DWORD dwNormalIntVector;    /* Normalized interrupt vector (ie,      */
                              /*   F000:1234 becomes F1234)            */
  DWORD dwNormalTsrAddress;   /* Normalized TSR Address                */


  /* Calculate the lowest MCB */

  dwLowestMcbSeg = 0xFFFF;

  for (u = 0; !(pTsr[u].wAddress == 0 && pTsr[u].dwBlockSize == 0); ++u)
    {
      if (pTsr[u].wAddress != 0x0008 &&
          pTsr[u].wAddress < (WORD) dwLowestMcbSeg)
        dwLowestMcbSeg = (DWORD) pTsr[u].wAddress;
    }

  /* Normalize the lowest MCB */
  dwLowestMcbSeg = dwLowestMcbSeg << 4;


  /* Set the szHandledBy for all IRQs */

  for (i = 0, wSwInt = 8; i <= pIrq->wNmbrIrqs; ++i, ++wSwInt)
    {
      /* IRQ0-IRQ7 use INT 08-10H, IRQ8-IRQ15 use INT 70-77H */
      if (i == 8)
        wSwInt = 0x70;

      /* Clear out the szHandledBy string */
      pIrq->IrqxInfo[i].szHandledBy[0] = '\0';

      /* Set the "Found" flag to FALSE */
      fFoundHandler = FALSE;

      /* Set the IRQ Address in the IRQ structure */
      pIrq->IrqxInfo[i].dwIrqAddress = padwIntTable[wSwInt];

      /* Set the interrupt's Normalized vector */
      dwNormalIntVector  = ((DWORD) FP_SEG (padwIntTable[wSwInt]) << 4) +
                           FP_OFF (padwIntTable[wSwInt]);


      /* Is the BIOS handling the interrupt */

      if (fFoundHandler == FALSE && dwNormalIntVector >= 0xF0000)
        {
          strcpy (pIrq->IrqxInfo[i].szHandledBy, pszBIOS);
          fFoundHandler = TRUE;
        }


      /* Check for device drivers handling this interrupt */

      for (u = 0; fFoundHandler == FALSE && pDevice[u].dwHeader != 0; ++u)
        {
          wDeviceHeaderSeg = (WORD) (pDevice[u].dwHeader >> 16);

          /* A match is determined if the segment of the interrupt */
          /*   matches the segment of the device driver's header.  */
          /*   Address 0070: is excluded as that is the DOS system */
          /*   area.                                               */

          if (wDeviceHeaderSeg != 0x0070 &&
              wDeviceHeaderSeg == (WORD) (padwIntTable[wSwInt] >> 16))
            {
              /* A match was found */
              strcpy (pIrq->IrqxInfo[i].szHandledBy,
                      pDevice[u].szDeviceName);
              fFoundHandler = TRUE;
            }
        }


      /* Check for TSR programs handling this interrupt */

      for (u = 0; fFoundHandler       == FALSE  &&
                  !(pTsr[u].wAddress  == 0      &&
                  pTsr[u].dwBlockSize == 0);       ++u)
        {
          /* A match is determined if the interrupt vector points to */
          /*   a location within a TSR's MCB                         */

          dwNormalTsrAddress = (DWORD) pTsr[u].wAddress << 4;

          if (dwNormalIntVector >= dwNormalTsrAddress &&
              dwNormalIntVector <= dwNormalTsrAddress + pTsr[u].dwBlockSize)
            {
              /* A match was found.  Skip if it is the "DOS System Data" */
              /*   in DOS versions 4.0 and up.                           */
              if (wDosMajor >= 4 && wDosMajor < 10 &&
                  strcmp (pTsr[u].szTsrName, pszDosSystemData) == 0)
                continue;

              if (pTsr[u].szTsrName[0] == ' ' && pTsr[u].szTsrName[1] == ' ')
                strcpy (pIrq->IrqxInfo[i].szHandledBy,
                        &(pTsr[u].szTsrName[2]));
              else
                strcpy (pIrq->IrqxInfo[i].szHandledBy,
                        pTsr[u].szTsrName);

              fFoundHandler = TRUE;
            }
        }


      /* Is the DOS System Area handling the interrupt */

      if (fFoundHandler == FALSE && dwNormalIntVector < dwLowestMcbSeg)
        {
          strcpy (pIrq->IrqxInfo[i].szHandledBy, pszDosSystemArea);
          fFoundHandler = TRUE;
        }


      /* Otherwise, I don't know who is handling this interrupt */
      if (fFoundHandler == FALSE)
        strcpy (pIrq->IrqxInfo[i].szHandledBy, pszUnknown);
    }

  return (FALSE);
}


/*********************************************************************
 * SprintIrqInfo - Put IRQ information into a set of strings to be
 *                 printed or displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintIrqInfo (IRQ_STRUCT *pIrq)
{
  WORD wNmbrStrings;            /* Number of strings                      */
  WORD wNmbrChars;              /* Number of characters in the strings    */
  WORD wUnderlineLength;        /* Length of the underline string         */
  WORD wIrqIndex;               /* Index to the structure of IRQ data     */
  WORD i;                       /* Looping variables                      */
  QSZ  *pqszStrings = NULL;     /* Location for storing string pointers   */
  PSZ  *paszIrqTitles = NULL;   /* The appropriate IRQ titles for PC or   */
                                /*   AT/PS/2                              */


  /* Choose the appropriate set of titles */

  if (pIrq->wNmbrIrqs == 7)
    paszIrqTitles = paszPcIrqDescriptions;
  else
    paszIrqTitles = paszAtIrqDescriptions;


  /* Calculate the amount of space required for the strings */

  wUnderlineLength = strlen (pszIrqUnderline);

  wNmbrChars   = strlen (pszIrqHeader) + 1 +
                 wUnderlineLength      + 1 +
                 ((pIrq->wNmbrIrqs + 1) * (wUnderlineLength + 1));

  /* The underline string is expected to be as long as a line of */
  /*   IRQ info.                                                 */

  wNmbrStrings = pIrq->wNmbrIrqs + 4;


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the first two strings in place */

  Qstrcpy (pqszStrings[0], pszIrqHeader);
  pqszStrings[1] = pqszStrings[0] + Qstrlen (pqszStrings[0]) + 1;

  Qstrcpy (pqszStrings[1], pszIrqUnderline);
  pqszStrings[2] = pqszStrings[1] + wUnderlineLength + 1;


  /* Put the IRQ information in place */

  for (i = 2, wIrqIndex = 0; wIrqIndex <= pIrq->wNmbrIrqs;
       ++i, ++wIrqIndex)
    {
      WORD u;             /* Looping variable                */
      WORD wLength;       /* Current length of string        */
      CHAR chBuffer[80];  /* Buffer for string data          */

      /* Clear out the string */
      Qmemset (pqszStrings[i], ' ', wUnderlineLength);
      pqszStrings[i][wUnderlineLength] = '\0';

      /* IRQ number */
      sprintf (chBuffer, "%3d", wIrqIndex);
      Qstrncpy (pqszStrings[i], chBuffer, strlen (chBuffer));

      /* Address */
      sprintf (chBuffer, "%04X:%04X",
               FP_SEG (pIrq->IrqxInfo[wIrqIndex].dwIrqAddress),
               FP_OFF (pIrq->IrqxInfo[wIrqIndex].dwIrqAddress));
      Qstrncpy (&pqszStrings[i][IRQ_ADDRESS_COL], chBuffer,
               strlen (chBuffer));

      /* Description */
      Qstrncpy (&pqszStrings[i][IRQ_DESCRIPTION_COL],
               paszIrqTitles[wIrqIndex],
               strlen (paszIrqTitles[wIrqIndex]));

      /* Detected strings */
      for (u = 0, wLength = 0;
           u < MAX_IRQ_DETECTED_STRINGS &&
           pIrq->IrqxInfo[wIrqIndex].szDetected[u][0] != '\0';
           ++u)
        {
          /* Add this "detected" string */
          Qstrncpy (&pqszStrings[i][IRQ_DETECTED_COL + wLength],
                   pIrq->IrqxInfo[wIrqIndex].szDetected[u],
                   strlen (pIrq->IrqxInfo[wIrqIndex].szDetected[u]));

          /* Move to one space past this "detected" string */
          wLength += strlen (pIrq->IrqxInfo[wIrqIndex].szDetected[u]) + 1;
        }

      /* Handled By */
      Qstrcpy (&pqszStrings[i][IRQ_HANDLED_BY_COL],
              pIrq->IrqxInfo[wIrqIndex].szHandledBy);

      /* Set the pointer for the next string */
      PrepNextString (pqszStrings, i);
    }

  /* Set the last pointer to a NULL */
  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */
  return (pqszStrings);
}


/*********************************************************************
 * GetSwIntTable - Makes a copy of the software interrupt vector table
 *                 at program start up time.
 *
 * Global: padwIntTable - Pointer to an array of DWORDs to store the
 *                        interrupt vectors.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetSwIntTable (VOID)
{
  union  REGS  regs;    /* Registers for int86x         */
  struct SREGS sregs;   /* Segment registers for int86x */
  WORD  i;              /* Looping variable             */


  /* Make room to store the table */

  padwIntTable = malloc (sizeof (DWORD) * 256);

  if (padwIntTable == NULL)
    {
      OutOfMemory();
      return (TRUE);
    }


  /* This is the quick and easy way */
  /*
  _femcpy ((DWORD FAR *) padwIntTable,
           (DWORD FAR *) 0x00000000,
           sizeof (DWORD) * 256);
  */


  /* This is the official way */

  for (i = 0; i < 256; ++i)
    {
      /* Get interrupt vector */
      regs.h.ah = 0x35;
      regs.h.al = (BYTE) i;
      int86x (0x21, &regs, &regs, &sregs);

      /* Store the interrupt vector */
      padwIntTable[i] = ((DWORD) sregs.es << 16) + regs.x.bx;
    }

  if (maxParsedLine[6] != '\0')
    OsErr();

  return (FALSE);
}
