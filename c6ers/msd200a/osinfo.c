/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * OSINFO.C - Source file for determing Operating System information.
 ********************************************************************/


/* Include Files */

#include "msd.h"


/*********************************************************************
 * GetOsVersionInfo - Gets the Operating System version number and the
 *                    version numbers of various operating
 *                    operating environments.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetOsVersionInfo (OS_VERSION_STRUCT *pOsVer, BOOL fMinimumInfo)
{
  union  REGS inregs, outregs;  /* Register structures */


  {
    /* Get DOS Version */

    /* Get Real DOS Version (intended for DOS >= 5.00) */

    inregs.x.ax = 0x3306;
    inregs.x.bx = 0x0000;
    int86 (0x21, &inregs, &outregs);


    /* Was the call supported? */

    if (outregs.x.bx != 0x0000)
      {
        /* It was.  Get the real DOS version */
        pOsVer->wDosMajor    = outregs.h.bl;
        pOsVer->wDosMinor    = outregs.h.bh;
        pOsVer->wDosRevision = outregs.h.dl;

        /* Determine where DOS was loaded */
        pOsVer->wDosLoadedFlags = outregs.h.dh;

        /* Call the standard DOS Version call */
        inregs.x.ax = 0x3000;
        int86 (0x21, &inregs, &outregs);
      }
    else
      {
        /* Get the DOS version from the standard DOS call */
        inregs.x.ax = 0x3000;
        int86 (0x21, &inregs, &outregs);

        /* Set the DOS version flags */
        pOsVer->wDosMajor    = outregs.h.al;
        pOsVer->wDosMinor    = outregs.h.ah;

        /* Set the DOS >= 5.00 specific flags */
        pOsVer->wDosRevision    = 0x00;
        pOsVer->wDosLoadedFlags = 0x00;
      }

    /* Set user serial number and OEM number */
    pOsVer->dwUserSerialNumber = ((DWORD) outregs.h.bl << 16) + outregs.x.cx;
    pOsVer->wOemSerialNumber   = outregs.h.bh;

    /* Set the flag and values for OS/2 */
    if (pOsVer->wDosMajor >= 10)
      {
        pOsVer->fOs2Installed = TRUE;
        pOsVer->wDosMajor = pOsVer->wDosMajor / 10;
      }
    else
      pOsVer->fOs2Installed = FALSE;
  }

  {
    /* Check for Windows and DOSSHELL */

    WinVerDetect (&(pOsVer->wWindowsType),  &(pOsVer->wWindowsMajor),
                  &(pOsVer->wWindowsMinor), &(pOsVer->fDosShellTaskSwitcher));
  }

  {
    /* Get OEM Name and Version */

    GetDosOemStrings (pOsVer);
  }

  {
    if (fMinimumInfo)
      return (FALSE);
  }

  {
    /* Get DOS Boot Drive */

    if (pOsVer->fOs2Installed == FALSE && pOsVer->wDosMajor >= 4)
      {
        inregs.x.ax = 0x3305;
        int86 (0x21, &inregs, &outregs);

        pOsVer->chDosBootDrive = (CHAR) (outregs.h.dl + 'A' - 1);
      }
    else
      pOsVer->chDosBootDrive = '\0';
  }

  {
    /* DESQview Information */

    inregs.x.ax = 0x2B01;
    inregs.x.cx = 0x4445; /* 'DE' */
    inregs.x.dx = 0x5251; /* 'SQ' */
    int86 (0x21, &inregs, &outregs);

    if (outregs.h.al == 0xFF)
      {
        /* DESQview was not installed */
        pOsVer->wDesqViewMajor = 0x00;
        pOsVer->wDesqViewMinor = 0x00;
      }
    else
      {
        /* DESQview was installed */
        if (outregs.x.bx == 0x0002)
          {
            pOsVer->wDesqViewMajor = 0x02;
            pOsVer->wDesqViewMinor = 0x00;
          }
        else
          {
            pOsVer->wDesqViewMajor = outregs.h.bh;
            pOsVer->wDesqViewMinor = outregs.h.bl;
          }
      }
  }

  {
    /* 3270 Control Program Information */

    inregs.x.ax = 0x3000;
    inregs.x.cx = 0x0000;
    inregs.x.dx = 0x0000;
    int86 (0x10, &inregs, &outregs);

    if (outregs.x.cx == 0x0000 && outregs.x.dx == 0x0000)
      {
        /* No 3270 program is installed */

        pOsVer->w3270Installed = NO_3270_PROGRAM;
        pOsVer->w3270Major     = 0;
        pOsVer->w3270Minor     = 0;
      }
    else
      {
        /* CX:DX points to a structure of 3270 info */

        WORD FAR *pfwWord = NULL;  /* Far pointer to a WORD */

        /* Segment of version information is at offset 8 */
        pfwWord = (WORD FAR *)
                    ((((DWORD) outregs.x.cx << 16) + outregs.x.dx) + 8);

        /* The version information is at offset 0 */
        pfwWord = (WORD FAR *) ((DWORD) (*pfwWord) << 16);

        if (*pfwWord <= 0x03FF)
          {
            /* This is the IBM 3270 PC Control Program */

            pOsVer->w3270Installed = _3270_PC_CONTROL_PROGRAM;
            pOsVer->w3270Major     = *pfwWord >> 8;
            pOsVer->w3270Minor     = *pfwWord & 0x00FF;
          }
        else
          {
            /* This is the IBM 3270 Workstation Program */

            pOsVer->w3270Installed = _3270_WORKSTATION_PROGRAM;
            pOsVer->w3270Major     = (*pfwWord >> 8) - 2;
            pOsVer->w3270Minor     = *pfwWord & 0x00FF;
          }
      }
  }

  {
    /* Is DoubleDOS Installed */

    inregs.x.ax = 0xF400;
    int86 (0x21, &inregs, &outregs);

    if (outregs.h.al == 00)
      pOsVer->fDoubleDosInstalled = FALSE;
    else
      pOsVer->fDoubleDosInstalled = TRUE;
  }

  {
    /* TaskView and TopView information */

    if (pOsVer->wDesqViewMajor > 0)
      {
        /* DesqView includes all of TopView's API's */

        pOsVer->fTaskViewInstalled = FALSE;
        pOsVer->wTopViewMajor      = 0;
        pOsVer->wTopViewMinor      = 0;
      }
    else
      {
        /* TopView Get Version Call */

        inregs.x.ax = 0x1022;
        inregs.x.bx = 0x0000;
        int86 (0x15, &inregs, &outregs);

        if (outregs.x.bx == 0x0000)
          {
            pOsVer->fTaskViewInstalled = FALSE;
            pOsVer->wTopViewMajor      = 0;
            pOsVer->wTopViewMinor      = 0;
          }
        if (outregs.x.bx == 0x0001)
          {
            pOsVer->fTaskViewInstalled = TRUE;
            pOsVer->wTopViewMajor      = 0;
            pOsVer->wTopViewMinor      = 0;
          }
        else
          {
            pOsVer->fTaskViewInstalled = FALSE;
            pOsVer->wTopViewMajor      = outregs.h.bl;
            pOsVer->wTopViewMinor      = outregs.h.bh;
          }
      }
  }

  {
    /* Store the fully qualified path to the program */

    strncpy (pOsVer->szPathToProgram, pszPathToProgram,
             MAX_PATH_TO_PROGRAM - 1);

    pOsVer->szPathToProgram[MAX_PATH_TO_PROGRAM - 1] = '\0';
  }

  return (FALSE);
}


/*********************************************************************
 * SprintOsVersionInfo - Put Operating System/Environment information
 *                       into a set of strings to be printed or
 *                       displayed.
 *
 * Returns:  NULL if an error occured.
 *********************************************************************/

QSZ * SprintOsVersionInfo (OS_VERSION_STRUCT *pOsVer,
                           CHAR szSumStrings[][MAX_SUMM_INFO + 5])
{
  WORD wNmbrStrings = 0;    /* Number of strings                     */
  WORD wNmbrChars = 0;      /* Number of characters in the strings   */
  WORD i, u;                /* Looping variables                     */
  CHAR chBuffer[80];        /* Local string                          */
  QSZ  *pqszStrings;        /* Location for storing string pointers  */
  WORD wAlignColumn;        /* Column to align titles                */
  WORD wLength;             /* Length of environment string          */
  WORD wMaxLength = 0;      /* Maximum length of environment strings */


  /* Summary Strings */
  if (szSumStrings != NULL)
    {
      if (pOsVer->szOemVer[0][0] != '\0' &&
          strlen (pOsVer->szOemVer[0]) < MAX_SUMM_INFO + 3)
        strncpy (szSumStrings[0], pOsVer->szOemVer[0], MAX_SUMM_INFO + 3);
      else
        {
          strncpy (szSumStrings[0], pszMsDos, MAX_SUMM_INFO);
          sprintf (chBuffer, " %d.%02d", pOsVer->wDosMajor,
                   pOsVer->wDosMinor);
          strncat (szSumStrings[0], chBuffer,
                   MAX_SUMM_INFO - strlen (szSumStrings[0]));
        }


      if (pOsVer->wWindowsType != NO_WINDOWS)
        {
          switch (pOsVer->wWindowsType)
            {
              case WIN_286:
                strncpy (szSumStrings[1], pszWin286, MAX_SUMM_INFO);
                strncat (szSumStrings[1], psz_2dotX,
                         MAX_SUMM_INFO - strlen (szSumStrings[1]));
                break;

              case WIN_386:
                strncpy (szSumStrings[1], pszWin386, MAX_SUMM_INFO);
                strncat (szSumStrings[1], psz_2dotX,
                         MAX_SUMM_INFO - strlen (szSumStrings[1]));
                break;

              default:
                strncpy (szSumStrings[1], pszWindows, MAX_SUMM_INFO);

                sprintf (chBuffer, " %d", pOsVer->wWindowsMajor);
                strncat (szSumStrings[1], chBuffer,
                         MAX_SUMM_INFO - strlen (szSumStrings[1]));

                if (pOsVer->wWindowsMinor == 0xFF)
                  strncat (szSumStrings[1], ".x ",
                           MAX_SUMM_INFO - strlen (szSumStrings[1]));
                else
                  {
                    sprintf (chBuffer, ".%02d ", pOsVer->wWindowsMinor);
                    strncat (szSumStrings[1], chBuffer,
                             MAX_SUMM_INFO - strlen (szSumStrings[1]));
                  }
            }
        }
      else if (pOsVer->fDosShellTaskSwitcher)
        strncpy (szSumStrings[1], pszDosShell, MAX_SUMM_INFO);

      return (NULL);
    }

  /* Overestimate the amount of space required for the strings */

  /* Count up the environment space */
  for (i = 0; environ[i][0] != '\0'; ++i)
    {
      wLength = strlen (environ[i]);
      wNmbrStrings += (wLength / 72) + 1;
      wNmbrChars += wLength + (wLength / 72) + 1;

      if (wLength > 72)
        wLength = 72;

      if (wMaxLength < wLength)
        wMaxLength = wLength;
    }

  wNmbrStrings += 3 + MAX_OS_TITLES;


  /* Calculate the correct alignment column */

  wAlignColumn = 20;

  if (pOsVer->fDosShellTaskSwitcher)
    wAlignColumn = 24;

  if (pOsVer->w3270Installed)
    wAlignColumn = 26;

  /* Center the information */
  wLength = strlen (pOsVer->szPathToProgram) + wAlignColumn;
  if (wLength < wMaxLength)
    wAlignColumn += ((wMaxLength / 2) - (wLength / 2));

  wNmbrChars += MAX_OS_TITLES * (MAX_OS_VERSION_LINE_LEN +
                                 MAX_PATH_TO_PROGRAM     +
                                 wAlignColumn);


  /* Allocate space for the pointer area and string area */

  pqszStrings = AllocStringSpace (wNmbrStrings, wNmbrChars);
  if (pqszStrings == NULL)
    return (NULL);


  /* Put the information in place */

  i = 0;

  {
    /* Operating System Version */

    QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_VERSION],
                 wAlignColumn);

    if (pOsVer->fOs2Installed)
      Qstrcat (pqszStrings[i], pszOs2);
    else
      Qstrcat (pqszStrings[i], pszMsDos);

    sprintf (chBuffer, " %d.%02d", pOsVer->wDosMajor, pOsVer->wDosMinor);
    Qstrcat (pqszStrings[i], chBuffer);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);
  }

  {
    /* Internal Revision */

    if (pOsVer->fOs2Installed == FALSE && pOsVer->wDosMajor >= 5)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_INTERNAL_REV],
                     wAlignColumn);
        sprintf (chBuffer, "%02d", pOsVer->wDosRevision);
        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* OEM Serial Number */

    QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_OEM_SERIAL],
                 wAlignColumn);
    sprintf (chBuffer, "%02XH", pOsVer->wOemSerialNumber);
    Qstrcat (pqszStrings[i], chBuffer);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);
  }

  {
    /* User Serial Number */

    QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_USER_SERIAL],
                 wAlignColumn);
    sprintf (chBuffer, "%06XH", pOsVer->dwUserSerialNumber);
    Qstrcat (pqszStrings[i], chBuffer);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);
  }

  {
    /* OEM Version String */

    QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_OEM_VERSION],
                 wAlignColumn);
    Qstrcat (pqszStrings[i], pOsVer->szOemVer[0]);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);

    /* Add the extra two strings, if they exist */
    for (u = 1; u < MAX_OEM_VER_STRINGS && pOsVer->szOemVer[u][0] != '\0'; ++u)
      {
        /* Add the string */
        QstrcpyAlign (pqszStrings[i], "", wAlignColumn);
        Qstrcat (pqszStrings[i], pOsVer->szOemVer[u]);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* DOS Location */

    if (pOsVer->fOs2Installed == FALSE && pOsVer->wDosMajor >= 5)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_DOS_LOCATION],
                     wAlignColumn);

        if (pOsVer->wDosLoadedFlags & 0x08)
          Qstrcat (pqszStrings[i], pszRom);

        if (pOsVer->wDosLoadedFlags & 0x10)
          Qstrcat (pqszStrings[i], pszHma);

        if (pOsVer->wDosLoadedFlags == 0)
          Qstrcat (pqszStrings[i], pszConventional);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* DOS Boot Drive */

    if (pOsVer->fOs2Installed == FALSE && pOsVer->wDosMajor >= 4)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_BOOT_DRIVE],
                     wAlignColumn);

        chBuffer[0] = pOsVer->chDosBootDrive;
        chBuffer[1] = ':';
        chBuffer[2] = '\0';

        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* DOSSHELL Task Switcher */

    if (pOsVer->fDosShellTaskSwitcher)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_DOSSHELL],
                     wAlignColumn);

        Qstrcat (pqszStrings[i], pszActive);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* Windows */

    if (pOsVer->wWindowsType != NO_WINDOWS)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_WINDOWS],
                     wAlignColumn);

        switch (pOsVer->wWindowsType)
          {
            case WIN_286:
              Qstrcat (pqszStrings[i], pszWin286);
              Qstrcat (pqszStrings[i], psz_2dotX);
              break;

            case WIN_386:
              Qstrcat (pqszStrings[i], pszWin386);
              Qstrcat (pqszStrings[i], psz_2dotX);
              break;

            default:
              Qstrcat (pqszStrings[i], pszWindows);

              sprintf (chBuffer, " %d", pOsVer->wWindowsMajor);
              Qstrcat (pqszStrings[i], chBuffer);

              if (pOsVer->wWindowsMinor == 0xFF)
                Qstrcat (pqszStrings[i], ".x ");
              else
                {
                  sprintf (chBuffer, ".%02d ", pOsVer->wWindowsMinor);
                  Qstrcat (pqszStrings[i], chBuffer);
                }

              switch (pOsVer->wWindowsType)
                {
                  case WIN_REAL_MODE:
                    Qstrcat (pqszStrings[i], pszRealMode);
                    break;

                  case WIN_STANDARD_MODE:
                    Qstrcat (pqszStrings[i], pszStandardMode);
                    break;

                  case WIN_ENHANCED_MODE:
                    Qstrcat (pqszStrings[i], pszEnhancedMode);
                    break;
                }
          }

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* DESQview Version */

    if (pOsVer->wDesqViewMajor != 0 || pOsVer->wDesqViewMinor != 0)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_DESQVIEW],
                     wAlignColumn);

        sprintf (chBuffer, "%d.%02d", pOsVer->wDesqViewMajor,
                 pOsVer->wDesqViewMinor);

        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* 3270 Program Information */

    if (pOsVer->w3270Installed)
      {
        strcpy (chBuffer, paszOsVersionTitles[OS_3270]);

        if (pOsVer->w3270Installed == _3270_PC_CONTROL_PROGRAM)
          Qstrcat (chBuffer, pszPCControlProgram);
        else
          Qstrcat (chBuffer, pszWorkstationProgram);

        QstrcpyAlign (pqszStrings[i], chBuffer, wAlignColumn);

        sprintf (chBuffer, "%d.%02d", pOsVer->w3270Major,
                 pOsVer->w3270Minor);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* DoubleDOS Installed */

    if (pOsVer->fDoubleDosInstalled)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_DOUBLE_DOS],
                     wAlignColumn);

        Qstrcat (pqszStrings[i], pszYes);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* TaskView Installed */

    if (pOsVer->fTaskViewInstalled)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_TASKVIEW],
                     wAlignColumn);

        Qstrcat (pqszStrings[i], pszYes);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* TopView Version */

    if (pOsVer->wTopViewMajor != 0 || pOsVer->wTopViewMinor != 0)
      {
        QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_TOPVIEW],
                     wAlignColumn);

        sprintf (chBuffer, "%d.%02d", pOsVer->wTopViewMajor,
                 pOsVer->wTopViewMinor);

        Qstrcat (pqszStrings[i], chBuffer);

        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }

  {
    /* Fully Qualified Path to Program */

    QstrcpyAlign (pqszStrings[i], paszOsVersionTitles[OS_PATH_TO_PROGRAM],
                 wAlignColumn);

    Qstrcat (pqszStrings[i], pOsVer->szPathToProgram);

    /* Set the next pointer */
    PrepNextString (pqszStrings, i++);
  }

  {
    /* Add the environment strings */

    PSZ  pszString;   /* Single String pointer */
    WORD u;           /* Looping variable      */


    /* Add the "Environment Strings" string */
    pqszStrings[i][0] = '\0';
    PrepNextString (pqszStrings, i++);

    wLength = (wMaxLength / 2) - (strlen (pszEnvironmentStr) / 2);
    Qmemset (pqszStrings[i], ' ', wLength);
    pqszStrings[i][wLength] = '\0';
    Qstrcat (pqszStrings[i], pszEnvironmentStr);
    PrepNextString (pqszStrings, i++);

    /* And underline */
    Qmemset (pqszStrings[i], '-', wMaxLength);
    pqszStrings[i][wMaxLength] = '\0';
    PrepNextString (pqszStrings, i++);


    for (u = 0; environ[u][0] != '\0'; ++u)
      {
        pszString = environ[u];

        Qstrcpy (pqszStrings[i], pszString);


        /* Wrap the string into pqszStrings */
        while (strlen (pszString) > 72)
          {
            pszString = &pszString[72];

            pqszStrings[i][72] = '\0';

            /* Set the next pointer */
            PrepNextString (pqszStrings, i++);
          }


        /* Set the next pointer */
        PrepNextString (pqszStrings, i++);
      }
  }


  /* Set the last pointer to NULL */

  pqszStrings[i] = NULL;

  /* Return the pointer to pqszStrings */

  return (pqszStrings);
}


VOID OsErr (VOID)
{
#if 0
  CHAR chBuffer[256];
  WORD * pw = (WORD *) minParsedLine;
  INT c = 0;


  sprintf (chBuffer, pszUnexplainedError, pw[-1] % 10000);
  puts (chBuffer);

  if (rgfReportItemFlag[IDI_CUSTOMER_INFORMATION] == FALSE)
    {
      WORD FAR *fpWord = (WORD FAR *) 0x0040006C;
      WORD wPause = (*fpWord) + 37;
      while (*fpWord < wPause)
        ;
    }
  else
    {
      puts ("Press ENTER to exit or C to continue ...");

      while ((c = getch()) != '\r' && c != 'C' && c != 'c')
        if (c == 0)
          getch();
    }


  if (c == '\r')
    exit (1);
#endif
}


/*********************************************************************
 * WinVerDetect - Detects type/mode of windows, and windows version.
 *                Also detects the DOSSHELL Task Switcher.
 *
 * pwWindowsType  - Type or mode of windows (/286, /386, enhanced,
 *                  standard, real, or none)
 * pwWindowsMajor - Major part of Windows version number.
 * pwWindowsMinor - Minor part of Windows version number.
 * pfDosShell     - TRUE if DOSSHELL task switcher is running.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL WinVerDetect (WORD *pwWindowsType,
                   WORD *pwWindowsMajor,
                   WORD *pwWindowsMinor,
                   WORD *pfDosShell)
{
  WORD wWindowsType;        /* Local copies of data */
  WORD wWindowsMajor;
  WORD wWindowsMinor;
  BOOL fDosShell = FALSE;   /* Defaults to FALSE    */

  _asm
    {
      ; Check for Windows 3.1

        mov     ax,160Ah                ; WIN31CHECK
        int     2Fh                     ; check if running under win 3.1
        or      ax,ax
        jnz     Win30EnhModeCheck

      ; Windows 3.1 detected

        mov     wWindowsMajor,3         ; Set the version number
        mov     wWindowsMinor,10

      ;   CX = 3 - Enhanced, CX = 2 - Standard, CX = 1 - Real.

        cmp     cx,1
        jne     Win31StdChk
        mov     wWindowsType, WIN_REAL_MODE
        jmp     WinDetectComplete

      Win31StdChk:

        cmp     cx,2
        jne     Win31EnhChk
        mov     wWindowsType, WIN_STANDARD_MODE
        jmp     WinDetectComplete

      Win31EnhChk:

        cmp     cx,3
        jne     Win31UnknownMode
        mov     wWindowsType, WIN_ENHANCED_MODE
        jmp     WinDetectComplete

      Win31UnknownMode:

        mov     wWindowsType, WIN_UNKNOWN_MODE
        jmp     WinDetectComplete


      ; Check for 3.0 Enhanced mode

      Win30EnhModeCheck:
        mov     ax,1600h                ; WIN386CHECK
        int     2Fh
        test    al,7Fh
        jz      Win286Check

      ; Windows 3.0 Enhanced Mode detected

        mov     wWindowsMajor,3         ; Set the version number
        mov     wWindowsMinor,0
                                        ; Set the mode
        mov     wWindowsType, WIN_ENHANCED_MODE
        jmp     WinDetectComplete


      ; Check for Windows/286

      Win286Check:
        mov     ax,1700h                ; WIN286CHECK
        int     2Fh
        cmp     al,2h                   ; If /286 installed, ver = AL.AH
        jnz     WinOldApCheck           ; /286 is always 2.x

      ; Windows/286 detected

        xor     bh,bh
        mov     bl,al
        mov     wWindowsMajor,bx
        mov     bl,ah
        mov     wWindowsMinor,bx
        mov     wWindowsType, WIN_286
        jmp     WinDetectComplete


      ; Check for Windows 3.0 WINOLDAP

      WinOldApCheck:
        mov     ax,4680h                ; IS_WINOLDAP_ACTIVE
        int     2Fh
        or      ax,ax                   ; running under 3.0 derivative ?
        jz      DosShellCheck

      ; Windows is not running on this computer

        jmp     NotRunningUnderWin


      ; Check for DOS 5.0 DOSSHELL Task Switcher

      DosShellCheck:
        mov     ax,4b02h                ; detect switcher
        push    bx
        push    es
        push    di
        xor     bx,bx
        mov     di,bx
        mov     es,bx
        int     2Fh
        pop     di
        pop     es
        pop     bx
        or      ax,ax
        jnz     RunningUnderWinStdReal30

      ; Running under DOS 5.0 task switcher

        mov     wWindowsMajor,0         ; Windows is not running
        mov     wWindowsMinor,0
        mov     wWindowsType, NO_WINDOWS
        mov     fDosShell, TRUE         ; Set the flag for the DOSSHELL
        jmp     WinDetectComplete


      RunningUnderWinStdReal30:

        mov     ax,1605h                ; PMODE_START
        int     2Fh
        cmp     cx,-1
        jnz     Running30RealOr386

      ; Windows 3.0 Standard Mode detected

        mov     ax,1606h                ; PMODE_STOP
        int     2Fh                     ; in case someone is accounting.

        mov     wWindowsMajor,3         ; Set the version number
        mov     wWindowsMinor,0
                                        ; Set the Windows mode
        mov     wWindowsType, WIN_STANDARD_MODE
        jmp     WinDetectComplete

      Running30RealOr386:

        mov     ax,1606h                ; PMODE_STOP
        int     2Fh                     ; in case someone is accounting.

        cmp     al,1                    ; WIN386CHECK again
        jnz     RunningUnderRealMode
        cmp     al,0FFh
        jz      RunningUnderWin386

      RunningUnderRealMode:

        mov     wWindowsMajor,3         ; Set the version number
        mov     wWindowsMinor,0
                                        ; Set the Windows mode
        mov     wWindowsType, WIN_REAL_MODE
        jmp     WinDetectComplete


      RunningUnderWin386:

        mov     wWindowsMajor,2
        mov     wWindowsMinor,0FFh
        mov     wWindowsType, WIN_386
        jmp     WinDetectComplete

      NotRunningUnderWin:

        mov     wWindowsMajor,0         ; Windows is not running
        mov     wWindowsMinor,0
        mov     wWindowsType, NO_WINDOWS

      WinDetectComplete:
    }

  /* Set the new-found values */

  *pwWindowsType  = wWindowsType;
  *pwWindowsMajor = wWindowsMajor;
  *pwWindowsMinor = wWindowsMinor;
  *pfDosShell     = fDosShell;

  return (FALSE);
}


/*********************************************************************
 * GetDosOemStrings - Gets the DOS OEM version string (the same as
 *                    typing VER at the DOS prompt.  This is
 *                    accomplished by performing a COMMAND/C VER and
 *                    re-routing the output to a file.  The file is
 *                    read, and the strings are stored in the pOsVer
 *                    structure.
 *
 * pOsVer - Operating system version structure.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetDosOemStrings (OS_VERSION_STRUCT *pOsVer)
{
  CHAR szTempPath[_MAX_PATH];   /* Stores path to temp directory       */
  BOOL fReturnValue;            /* Return value from various functions */
                                /* Command to get the version          */
  CHAR szCommandVer[_MAX_PATH + 20];
  CHAR chBuffer[80];            /* Local string buffer                 */
  FILE *fileVer;                /* file handle to read version file    */
  WORD i, u;                    /* Index variable, looping variable    */
  WORD wLastChar;               /* Index to last character in path     */
  static CHAR FAR *fpszOemStrings = NULL;  
                                /* Store the strings, so I only have   */
                                /*   to get them once                  */
  struct find_t ft;             /* Used to validate temp directory     */

  /* If I've already got the OEM strings, return them */
  if (fpszOemStrings)
    {
      _fmemcpy (pOsVer->szOemVer, fpszOemStrings,
                MAX_OEM_VER_STRINGS * MAX_OEM_VER);
    }
  else
    {
      /* Zero out the string */
      memset (szTempPath, '\0', _MAX_PATH);

      /* Get the TEMP or TMP path or current directory for the temp file */
      strcpy (szTempPath, getenv (pszTemp));
      if (szTempPath[0] != '\0')
        {
          /* Add "*.*" to the temp path */
          strcpy (szCommandVer, szTempPath);
          wLastChar = strlen (szCommandVer) - 1;
          if (szCommandVer[wLastChar] != '\\')
            strcat (szCommandVer, "\\");
          strcat (szCommandVer, "*.*");

          /* Check to see if the directory is valid */
          fReturnValue = _dos_findfirst (szCommandVer, 0xFFFF, &ft);
          if (fReturnValue != 0)
            {
              fTempPathInvalid = TRUE;

              if (fCwIsReady)
                {
                  strcpy (szTempPath, pszTemp);
                  strcat (szTempPath, "=");
                  strcat (szTempPath, getenv (pszTemp));
                  MessageBox (pszEnvironInvalid, szTempPath, NULL,
                              MB_OK | 0x8000);
                }

              szTempPath[0] = '\0';
            }
        }

      if (szTempPath[0] == '\0')
        {
          strcpy (szTempPath, getenv (pszTmp));
          if (szTempPath[0] != '\0')
            {
              /* Add "*.*" to the temp path */
              strcpy (szCommandVer, szTempPath);
              wLastChar = strlen (szCommandVer) - 1;
              if (szCommandVer[wLastChar] != '\\')
                strcat (szCommandVer, "\\");
              strcat (szCommandVer, "*.*");

              /* Check to see if the directory is valid */
              fReturnValue = _dos_findfirst (szCommandVer, 0xFFFF, &ft);
              if (fReturnValue != 0)
                {
                  fTmpPathInvalid = TRUE;

                  if (fCwIsReady)
                    {
                      strcpy (szTempPath, pszTmp);
                      strcat (szTempPath, "=");
                      strcat (szTempPath, getenv (pszTmp));
                      MessageBox (pszEnvironInvalid, szTempPath, NULL,
                                  MB_OK | 0x8000);
                    }

                  szTempPath[0] = '\0';
                }
            }
        }

      /* Current directory */
      if (szTempPath[0] == '\0')
        {
          getcwd (szTempPath, _MAX_PATH - 1);

          /* A: and B: are not allowed */
          if (szTempPath[0] == '\0'               ||
              strnicmp (szTempPath, "A:", 2) == 0 ||
              strnicmp (szTempPath, "B:", 2) == 0)
            return (TRUE);
        }


      /* Add the filename */
      wLastChar = strlen (szTempPath) - 1;
      if (szTempPath[wLastChar] != '\\')
        strcat (szTempPath, "\\");
      strcat (szTempPath, "__MSD__.$$$");

      /* Create the complete command string */
      strcpy (szCommandVer, "VER > ");
      strcat (szCommandVer, szTempPath);

      fReturnValue = system (szCommandVer);
      if (fReturnValue)
        {
          DeleteFile (szTempPath);
          return (TRUE);
        }

      /* Open the file with the version */
      fileVer = OpenFile (szTempPath, "rb", FALSE);
      if (fileVer == NULL)
        {
          DeleteFile (szTempPath);
          return (TRUE);
        }

      /* Read the DOS OEM version strings */
      i = 0;
      while (ReadLine (chBuffer, MAX_OEM_VER - 1, fileVer, FALSE) != EOF &&
             i < MAX_OEM_VER_STRINGS)
        {
          if (chBuffer[0] != '\0')
            {
              strcpy (pOsVer->szOemVer[i++], chBuffer);

              /* Word wrap the line if it's > 50 chars */
              if (strlen (chBuffer) > 51)
                {
                  /* Look back for a space */
                  for (u = 50; u > 0 && chBuffer[u] != ' '; --u)
                    ;

                  /* Clear out the existing string */
                  pOsVer->szOemVer[i - 1][u] = '\0';

                  if (i < MAX_OEM_VER_STRINGS)
                    {
                      /* Copy the excess to the next string */
                      strcpy (pOsVer->szOemVer[i++], &chBuffer[++u]);
                    }
                }
            }
        }


      /* Close the temp file */
      CloseFile (fileVer);

      /* Delete the temp file */
      DeleteFile (szTempPath);

      /* Store the OEM Version strings for later use */
      fpszOemStrings = _fmalloc (MAX_OEM_VER_STRINGS * MAX_OEM_VER);
      if (fpszOemStrings)
        _fmemcpy (fpszOemStrings, pOsVer->szOemVer,
                  MAX_OEM_VER_STRINGS * MAX_OEM_VER);
    }

  /* Check for DOS 4.01 */
  if (pOsVer->wDosMajor == 4 && pOsVer->wDosMinor == 0)
    {
      for (i = 0; i < MAX_OEM_VER_STRINGS; ++i)
        {
          if (strstr (pOsVer->szOemVer[i], "4.01") != NULL)
            pOsVer->wDosMinor = 1;
        }
    }

  return (FALSE);
}
