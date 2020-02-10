/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * MOUSINFO.C - Source file for mouse detection code.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetMouseInfo - Gets the mouse information.
 *
 * pMouse       - Mouse information structure
 * fMinimumInfo - TRUE if minimum information is requested.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetMouseInfo (MOUSE_STRUCT *pMouse)
{
  union  REGS  inregs, outregs;   /* Register structure for int86()        */
  struct SREGS sregs;             /* Segment regs for int86x()             */
  WORD  wType;                    /* Mouse driver type (InPort, Bus, etc). */
  WORD  wMouseHardware;           /* Mouse hardware type                   */
  WORD  wSize;                    /* Size required to store mouse state    */
  WORD  wMouseVersion;            /* Mouse version (ie 801 == 8.01)        */
  BYTE FAR *fbMouseState = NULL;  /* Pointer to mouse state                */
  static BOOL fMouseInfoLoaded = FALSE;   /* Set to TRUE after mouse       */
                                          /*   info obtained               */
  static MOUSE_STRUCT FAR *fpMouse = NULL;/* Stored mouse info             */


  /* We obtain the mouse information only once, then store the mouse */
  /*   information for later requests.  If a second request is made  */
  /*   (ie., for IRQ info, etc.), this routine will copy the stored  */
  /*   mouse information to pMouse and return immediately.  This is  */
  /*   done because the mouse information is very time consuming to  */
  /*   obtain on some computers and/or mice.                         */

  if (fMouseInfoLoaded)
    {
      _fmemcpy ((MOUSE_STRUCT FAR *) pMouse, fpMouse, sizeof (MOUSE_STRUCT));
      return (FALSE);
    }


  /* Check to see if the mouse driver is safe to call */

  /* Get interrupt vector */
  inregs.h.ah = 0x35;
  inregs.h.al = 33;
  int86x (0x21, &inregs, &outregs, &sregs);

  if ((sregs.es | outregs.x.bx) == 0)
    {
      NoMouseDriver (pMouse);
      return (FALSE);
    }


  /* Save Microsoft Mouse state (important for the CW routines) */

  inregs.x.ax = 0x0015;
  inregs.x.bx = 0x0000;
  int86 (0x33, &inregs, &outregs);
  wSize = outregs.x.bx;

  if (wSize == 0)
    fbMouseState = NULL;
  else
    {
      fbMouseState = _fmalloc (wSize);
      if (fbMouseState != NULL)
        {
          inregs.x.ax = 0x0016;
          sregs.es    = FP_SEG (fbMouseState);
          inregs.x.dx = FP_OFF (fbMouseState);
          inregs.x.bx = wSize;
          int86x (0x33, &inregs, &outregs, &sregs);
        }
    }


  /* Zero out the OEM version number, driver mfgr, and COM port */
  pMouse->wOemMajorVersion = 0;
  pMouse->wOemMinorVersion = 0;
  pMouse->wDriverMfgr      = 0;
  memset (pMouse->szDriverMfgr, '\0', MAX_MOUSE_DRIVER_MFGR);
  pMouse->wComPort         = 0;
  pMouse->szComPort[0]     = '\0';


  /* Determine mouse hardware */

  wMouseHardware = fnHardMouseDetect();
  pMouse->wMouseHardwareType = wMouseHardware;

  if (wMouseHardware == 0xFFFF)
    {
      strcpy (pMouse->szMouseHardwareType, pszDeviceNotDetected);
      pMouse->fHardwareInstalled = FALSE;
    }
  else
    {
      if (wMouseHardware == MSPS2_MOUSE)
        wMouseHardware = PS2_STYLE_MOUSE;

      strcpy (pMouse->szMouseHardwareType, paszMouseTypes[wMouseHardware]);
      pMouse->fHardwareInstalled = TRUE;
    }


  if (AltPS2MouseChk() == TRUE)
    {
      /* A PS/2 Style mouse has been detected.  I can tell the     */
      /*   difference between a Logitech PS/2 mouse and a IBM/     */
      /*   Microsoft PS/2 mouse only at DOS.  If we're in Windows, */
      /*   the mice behave identically.  Therefore, if we're in    */
      /*   Windows, I'll declare it to be a PS/2 Style Mouse, but  */
      /*   if we're at DOS, I'll say it's a Logitech if that was   */
      /*   detected.                                               */

      if (wMouseHardware == LOGITECH_PS2_MOUSE)
        {
          WORD wWindowsType;        /* Local copies of data */
          WORD wWindowsMajor;
          WORD wWindowsMinor;
          BOOL fDosShell;


          /* Determine if we are running in Windows 3.0 or above */
          WinVerDetect (&wWindowsType,
                        &wWindowsMajor,
                        &wWindowsMinor,
                        &fDosShell);

          if (wWindowsType != NO_WINDOWS && wWindowsMajor >= 3)
            {
              strcpy (pMouse->szMouseHardwareType,
                      paszMouseTypes[PS2_STYLE_MOUSE]);
              pMouse->wMouseHardwareType = PS2_STYLE_MOUSE;
              pMouse->fHardwareInstalled = TRUE;
            }
        }
      else if (wMouseHardware == NO_MOUSE_INSTALLED)
        {
          strcpy (pMouse->szMouseHardwareType,
                  paszMouseTypes[PS2_STYLE_MOUSE]);
          pMouse->wMouseHardwareType = PS2_STYLE_MOUSE;
          pMouse->fHardwareInstalled = TRUE;
        }
    }

  /* Check to see if non-Microsoft mouse drivers are resident */

  /* PC Mouse Driver */
  inregs.x.ax = 0x0042;
  int86 (0x33, &inregs, &outregs);
  if (outregs.x.ax == 0xFFFF)
    {
      pMouse->wOemMajorVersion = 0;
      pMouse->wOemMinorVersion = 0;
      pMouse->wDriverMfgr      = MOUSE_MFGR_PC_MOUSE;
      strcpy (pMouse->szDriverMfgr, paszMouseMfgrs[MOUSE_MFGR_PC_MOUSE]);
    }

  /* Logitech Mouse Driver */
  inregs.x.ax = 0x266C;
  int86 (0x33, &inregs, &outregs);
  if (outregs.x.bx == 0x5353)  /* 'SS' */
    {
      WORD rgwLMouseInfo[16];   /* An array to hold mouse info */

      pMouse->wOemMajorVersion = outregs.h.ch - '0';
      pMouse->wOemMinorVersion = outregs.h.cl - '0';
      pMouse->wDriverMfgr      = MOUSE_MFGR_LOGITECH;
      strcpy (pMouse->szDriverMfgr, paszMouseMfgrs[MOUSE_MFGR_LOGITECH]);

      /* Load Logitech serial mouse port number */
      segread (&sregs);
      inregs.x.ax = 0x246C;
      inregs.x.dx = (WORD) &rgwLMouseInfo[0];
      int86x (0x33, &inregs, &outregs, &sregs);
      if (outregs.x.ax == 0xFFFF) /* AX = FFFFh means it's a serial mouse */
        {
          if (rgwLMouseInfo[5] <= 4)
            pMouse->wComPort = rgwLMouseInfo[5];
        }
    }


  /* Reset Microsoft driver and read status */

  inregs.x.ax = 0x0000;
  int86 (0x33, &inregs, &outregs);

  if (outregs.x.ax == 0xFFFF)
    {
      /* Microsoft mouse driver (or compatible) installed */
      if (pMouse->wDriverMfgr == 0)
        {
          pMouse->wDriverMfgr = MOUSE_MICROSOFT_DRIVER;
          strcpy (pMouse->szDriverMfgr, paszMouseMfgrs[MOUSE_MFGR_MICROSOFT]);
        }


      /* Set the number of mouse buttons */
      if (outregs.x.bx == 0xFFFF)
        pMouse->wNmbrButtons = 2;
      else
        pMouse->wNmbrButtons = outregs.x.bx;


      /* Most of the information is not available under OS/2 */
      if (wDosMajor >= 10)
        {
          /* A mouse IRQ of zero will not be checked for IRQ conflicts */
          pMouse->wIrq = 0;

          /* Restore Microsoft Mouse state */
          if (fbMouseState)
            {
              inregs.x.ax = 0x0017;
              sregs.es    = FP_SEG (fbMouseState);
              inregs.x.dx = FP_OFF (fbMouseState);
              inregs.x.bx = wSize;
              int86x (0x33, &inregs, &outregs, &sregs);
              _ffree (fbMouseState);
            }
        }


      /* Get Mouse driver version and mouse type */
      inregs.x.ax = 0x0024;
      int86 (0x33, &inregs, &outregs);


      /* Driver version number */
      pMouse->wMsMajorVersion = outregs.h.bh;
      pMouse->wMsMinorVersion = outregs.h.bl;
      wMouseVersion = ((WORD) outregs.h.bh) * 100 + (WORD) outregs.h.bl;


      /* Mouse IRQ */
      if (outregs.h.cl == 0 || outregs.h.cl == 0xFF)
        pMouse->wIrq = 12;    /* PS/2 mice use IRQ 12 */
      else
        pMouse->wIrq = outregs.h.cl;


      /* Mouse Type from int 33H, AX=0024H */
      wType = outregs.h.ch;


      /* Check for Ballpoint mouse */
      inregs.x.ax = 0x0030;
      inregs.h.ch = 0x00;
      int86 (0x33, &inregs, &outregs);


      /* Set the mouse type */
      if (outregs.x.ax != 0x30 && outregs.x.ax != 0xFFFF)
        {
          pMouse->wMouseDriverType = 0x80;
          strcpy (pMouse->szMouseDriverType, pszBallPoint);
        }
      else
        {
          pMouse->wMouseDriverType  = wType;
          strcpy (pMouse->szMouseDriverType, paszMouseTypes[wType]);
        }


      /* Set mouse's COM port */
      if (pMouse->wComPort == 0 &&
          (pMouse->wMouseDriverType == SERIAL_MOUSE          ||
           pMouse->wMouseDriverType == LOGITECH_SERIAL_MOUSE ||
           pMouse->wMouseDriverType == BALLPOINT_MOUSE))
        {
          /* Serial mouse.  Determine the serial port */
          if (pMouse->wIrq == 3)
            pMouse->wComPort = 2;
          else if (pMouse->wIrq == 4)
            pMouse->wComPort = 1;
          else
            {
              pMouse->wComPort        = 0;
              pMouse->wComPortAddress = 0;
              strcpy (pMouse->szComPort, pszUnknown);
            }
        }


      /* Set mouse's COM port address */
      if (pMouse->wComPort != 0)
        {
          /* 40:0 is the DOS Device Table */
          WORD FAR *fwDosDeviceTable = (WORD FAR *) 0x00400000;
          WORD wPort;     /* COM port number (ie, 0 = COM1:) */

          wPort = pMouse->wComPort;
          pMouse->wComPortAddress = fwDosDeviceTable[wPort - 1];
          strcpy (pMouse->szComPort, pszCom[wPort]);
        }


      /* Mouse Sensitivity */
      inregs.x.ax = 0x001B;
      int86 (0x33, &inregs, &outregs);


      /* Set sensitivities */
      pMouse->wHMickeys       = outregs.x.bx;
      pMouse->wVMickeys       = outregs.x.cx;
      pMouse->wThresholdSpeed = outregs.x.dx;


      /* Mouse Language */
      inregs.x.ax = 0x0023;
      inregs.x.bx = 0x0000;
      int86 (0x33, &inregs, &outregs);


      /* Set the mouse language */
      pMouse->wLanguage = outregs.x.bx;
      strcpy (pMouse->szLanguage, paszMouseLanguages[outregs.x.bx]);


      /* Determine the Mouse driver's file type (.SYS, .COM) */
      if (wMouseVersion >= 626)
        {
          inregs.x.ax = 0x0025;
          int86 (0x33, &inregs, &outregs);

          /* Bit 15 is set for .SYS files, clear for .COM files */
          if (outregs.x.ax & 0x8000)
            {
              pMouse->wDriverFileType = MOUSE_SYS_FILE;
              strcpy (pMouse->szDriverFileType,
                      paszDriverFileTypes[MOUSE_SYS_FILE]);
            }
          else
            {
              pMouse->wDriverFileType = MOUSE_COM_FILE;
              strcpy (pMouse->szDriverFileType,
                      paszDriverFileTypes[MOUSE_COM_FILE]);
            }
        }
      else
        {
          pMouse->wDriverFileType = MOUSE_UNKNOWN_FILE;
          pMouse->szDriverFileType[0] = '\0';
        }


      /* Determine the location of the MOUSE.INI file */
      if (wMouseVersion >= 800)
        {
          inregs.x.ax = 0x0034;
          int86x (0x33, &inregs, &outregs, &sregs);

          /* ES:DX point to the fully qualified path to MOUSE.INI */
          if (outregs.x.ax != 0 && (sregs.es | outregs.x.dx))
            {
              CHAR FAR * fpszString = NULL;  /* Far string pointer */

              fpszString = (CHAR FAR *)
                             ((DWORD) sregs.es << 16) + outregs.x.dx;

              _fstrncpy ((CHAR FAR *) pMouse->szMouseIniPath, fpszString,
                         MAX_MOUSE_INI_PATH - 1);
              pMouse->szMouseIniPath[MAX_MOUSE_INI_PATH - 1] = '\0';
            }
          else
            pMouse->szMouseIniPath[0] = '\0';
        }
      else
        pMouse->szMouseIniPath[0] = '\0';
    }
  else
    NoMouseDriver (pMouse);


  /* Restore Microsoft Mouse state, unless it's a 7.04 mouse driver.  */
  /*   A bug in the 7.04 mouse driver will divide overflow and crash. */

  if (fbMouseState)
    if (pMouse->wMsMajorVersion != 7 && pMouse->wMsMinorVersion != 4)
      {
        inregs.x.ax = 0x0017;
        sregs.es    = FP_SEG (fbMouseState);
        inregs.x.dx = FP_OFF (fbMouseState);
        inregs.x.bx = wSize;
        int86x (0x33, &inregs, &outregs, &sregs);
        _ffree (fbMouseState);
      }
    else
      _ffree (fbMouseState);


  /* Store the mouse structure for future use */
  fpMouse = _fmalloc (sizeof (MOUSE_STRUCT));
  if (fpMouse != NULL)
    {
      _fmemcpy (fpMouse, (MOUSE_STRUCT FAR *) pMouse, sizeof (MOUSE_STRUCT));
      fMouseInfoLoaded = TRUE;
    }

  /* Clear the serial ports of stray characters */
  inp (0x03F8);   /* COM1: */
  inp (0x02F8);   /* COM2: */
  inp (0x03E8);   /* COM3: */
  inp (0x02E8);   /* COM4: */

  return (FALSE);
}


/*********************************************************************
 * NoMouseDriver - Sets the values in the mouse structure to reflect
 *                 the fact that there is no mouse driver present
 *                 in the system.
 *
 * pMouse - Mouse structure.
 *********************************************************************/

VOID NoMouseDriver (MOUSE_STRUCT *pMouse)
{
  pMouse->wOemMajorVersion = 0;
  pMouse->wOemMinorVersion = 0;
  pMouse->wMsMajorVersion  = 0;
  pMouse->wMsMinorVersion  = 0;
  pMouse->wDriverMfgr      = MOUSE_NO_MOUSE_DRIVER;
  strcpy (pMouse->szDriverMfgr, paszMouseMfgrs[MOUSE_NO_MOUSE_DRIVER]);
}


/*********************************************************************
 * SprintMouseInfo - Put mouse information into a set of strings to be
 *                   printed or displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintMouseInfo (MOUSE_STRUCT *pMouse,
                       CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings;        /* Number of strings                     */
  WORD wNmbrChars;          /* Number of characters in the strings   */
  WORD wIndent;             /* Indent amount for aligning colons     */
  WORD wIndex;              /* Index to mouse title strings          */
  WORD wRatioIndex;         /* Index for looking up the mouse ratio  */
  WORD i;                   /* Looping variable                      */
  CHAR chBuffer[80];        /* Local string                          */
  QSZ  *pqszStrings = NULL; /* Location for storing string pointers  */


  /* Summary Strings */
  if (szSumStrings != NULL)
    {
      /* Clear out the summary strings */
      szSumStrings[0][0] = '\0';
      szSumStrings[1][0] = '\0';


      /* Check to see if the driver is detected, */
      /*   but the hardware is not detected.     */
      if (pMouse->fHardwareInstalled  == FALSE &&
          (pMouse->wDriverMfgr        != MOUSE_NO_MOUSE_DRIVER ||
           pMouse->wMouseHardwareType == NO_MOUSE_INSTALLED))
        {
          strcpy (szSumStrings[0], "Driver Detected");
        }
      else
        {
          /* Mouse hardware type */
          strcpy (szSumStrings[0], pMouse->szMouseHardwareType);

          /* Is mouse driver version available */
          if (wDosMajor >= 10 || pMouse->wDriverMfgr == MOUSE_NO_MOUSE_DRIVER)
            return (NULL);
        }

      /* Mouse driver version */
      if (pMouse->wOemMajorVersion != 0)
        sprintf (chBuffer, " %x.%02x", pMouse->wOemMajorVersion,
                 pMouse->wOemMinorVersion);
      else
        sprintf (chBuffer, " %x.%02x", pMouse->wMsMajorVersion,
                 pMouse->wMsMinorVersion);

      if (strlen (szSumStrings[0]) + strlen (chBuffer) > MAX_SUMM_INFO)
        {
          i = 1;
          szSumStrings[i][0] = '\0';
        }
      else
        i = 0;

      strcat (szSumStrings[i], chBuffer);

      return (NULL);
    }



  /* Overestimate the amount of space required for the strings */

  for (wNmbrStrings = 0; paszMouseTitles[wNmbrStrings] != NULL;
       ++wNmbrStrings)
    ;

  ++wNmbrStrings;

  wNmbrChars = wNmbrStrings * MAX_MOUSE_LINE_LEN;


  /* Calculate the indent.  If there is an OEM version number, */
  /*   then the Microsoft version number line will have to be  */
  /*   displayed, and it is the longest line.  If not, then    */
  /*   the next longest line will be needed.                   */

  if (pMouse->wOemMajorVersion != 0)
    wIndent = MAX_MOUSE_TITLE;
  else
    wIndent = MAX_MOUSE_TITLE - 1;


  /* Allocate space for the pointer area and string area */
  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  for (wIndex = 0, i = 0; paszMouseTitles[wIndex] != NULL; ++wIndex, ++i)
    {
      /* Determine if the "Microsoft Driver Version" line is required */
      if (wIndex == MOU_MS_DRIVER_VERSION &&
          pMouse->wOemMajorVersion == 0)
        {
          --i;
          continue;
        }


      /* Determine if the "DOS Driver Type" line is required */
      if (wIndex == MOU_DRIVER_FILE_TYPE &&
          pMouse->szDriverFileType[0] == '\0')
        {
          --i;
          continue;
        }


      /* Determine if the "Path to MOUSE.INI" line is required */
      if (wIndex == MOU_MOUSE_INI_PATH &&
          pMouse->szMouseIniPath[0] == '\0')
        {
          --i;
          continue;
        }


      /* Determine if Serial Mouse information is necessary */
      if (wIndex == MOU_COM_PORT || wIndex == MOU_COM_PORT_ADDRESS)
        if (pMouse->wMouseDriverType != SERIAL_MOUSE          &&
            pMouse->wMouseDriverType != LOGITECH_SERIAL_MOUSE &&
            pMouse->wMouseDriverType != BALLPOINT_MOUSE)
          {
            /* It's not a serial mouse, so skip it */
            --i;
            continue;
          }


      /* Most information is not available under OS/2 */
      if ((wDosMajor >= 10 ||
          pMouse->wDriverMfgr == MOUSE_NO_MOUSE_DRIVER) &&
          wIndex > MOU_DRIVER_MFGR)
        break;


      /* Put the title on the line */
      QstrcpyAlign (pqszStrings[i], paszMouseTitles[wIndex], wIndent);

      switch (wIndex)
        {
          case MOU_HARDWARE:
            {
              Qstrcat (pqszStrings[i], pMouse->szMouseHardwareType);
              break;
            }

          case MOU_DRIVER_MFGR:
            {
              Qstrcat (pqszStrings[i], pMouse->szDriverMfgr);
              break;
            }

          case MOU_DOS_DRIVER_TYPE:
            {
              Qstrcat (pqszStrings[i], pMouse->szMouseDriverType);
              break;
            }

          case MOU_DRIVER_FILE_TYPE:
            {
              Qstrcat (pqszStrings[i], pMouse->szDriverFileType);
              break;
            }

          case MOU_DOS_DRIVER_VERSION:
            {
              /* If there is an OEM version number, display it,    */
              /*   otherwise, display the Microsoft version number */

              if (pMouse->wOemMajorVersion != 0)
                sprintf (chBuffer, "%x.%02x", pMouse->wOemMajorVersion,
                         pMouse->wOemMinorVersion);
              else
                sprintf (chBuffer, "%x.%02x", pMouse->wMsMajorVersion,
                         pMouse->wMsMinorVersion);

              Qstrcat (pqszStrings[i], chBuffer);
              break;
            }

          case MOU_MS_DRIVER_VERSION:
            {
              sprintf (chBuffer, "%x.%02x", pMouse->wMsMajorVersion,
                       pMouse->wMsMinorVersion);
              Qstrcat (pqszStrings[i], chBuffer);
              break;
            }

          case MOU_MOUSE_IRQ:
            {
              sprintf (chBuffer, "%d", pMouse->wIrq);
              Qstrcat (pqszStrings[i], chBuffer);
              break;
            }

          case MOU_COM_PORT:
            {
              Qstrcat (pqszStrings[i], pMouse->szComPort);
              break;
            }

          case MOU_COM_PORT_ADDRESS:
            {
              if (pMouse->wComPort)
                {
                  sprintf (chBuffer, "%04XH", pMouse->wComPortAddress);
                  Qstrcat (pqszStrings[i], chBuffer);
                }
              break;
            }

          case MOU_NMBR_BUTTONS:
            {
              sprintf (chBuffer, "%d", pMouse->wNmbrButtons);
              Qstrcat (pqszStrings[i], chBuffer);
              break;
            }

          case MOU_H_SENSITIVITY:
            {
              sprintf (chBuffer, "%d", pMouse->wHMickeys);
              Qstrcat (pqszStrings[i], chBuffer);
              break;
            }

          case MOU_H_CURSOR_RATIO:
            {
              wRatioIndex = pMouse->wHMickeys / 5 - 1;
              if (wRatioIndex < MAX_MOUSE_RATIO_STRINGS)
                {
                  Qstrcat (pqszStrings[i], paszMouseRatios[wRatioIndex]);
                  Qstrcat (pqszStrings[i], pszColonOne);
                }
              else
                Qstrcat (pqszStrings[i], pszUndefined);
              break;
            }

          case MOU_V_SENSITIVITY:
            {
              sprintf (chBuffer, "%d", pMouse->wVMickeys);
              Qstrcat (pqszStrings[i], chBuffer);
              break;
            }

          case MOU_V_CURSOR_RATIO:
            {
              wRatioIndex = pMouse->wVMickeys / 5 - 1;
              if (wRatioIndex < MAX_MOUSE_RATIO_STRINGS)
                {
                  Qstrcat (pqszStrings[i], paszMouseRatios[wRatioIndex]);
                  Qstrcat (pqszStrings[i], pszColonOne);
                }
              else
                Qstrcat (pqszStrings[i], pszUndefined);
              break;
            }

          case MOU_THRESHOLD_SPEED:
            {
              sprintf (chBuffer, "%d", pMouse->wThresholdSpeed);
              Qstrcat (pqszStrings[i], chBuffer);
              break;
            }

          case MOU_LANGUAGE:
            {
              Qstrcat (pqszStrings[i], pMouse->szLanguage);
              break;
            }

          case MOU_MOUSE_INI_PATH:
            {
              Qstrcat (pqszStrings[i], pMouse->szMouseIniPath);
              break;
            }
        }

      /* Set the next pointer */
      PrepNextString (pqszStrings, i);
    }


  /* Set the last pointer to NULL */

  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */

  return (pqszStrings);
}


/*********************************************************************
 * AltPS2MouseChk - Another check for a PS/2 mouse.
 *********************************************************************/

BOOL AltPS2MouseChk (VOID)
{
  union REGS regs;

  /* PS/2 Pointing Device BIOS Interface -- Reset */

  regs.x.ax = 0xC201;
  regs.x.bx = 0xFFFF;
  int86 (0x15, &regs, &regs);

  /* Carry Flag set indicates error */

  if (regs.x.cflag || regs.x.bx == 0xFFFF)
    return (FALSE);
  else
    return (TRUE);
}
