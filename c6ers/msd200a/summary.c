/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * SUMMARY.C - Operates the summary screen and supporting CW routines.
 *********************************************************************/


/* Include Files */

#include "msd.h"

#ifdef CW_INCLUDED

#include "cgraphic.h"
#include <process.h>

AY FAR PASCAL GetUILine (void);


/********************************************************************
 * Include the auxiliary CW code
 ********************************************************************/

#include "scr.c"
#include "auxcow.c"


/********************************************************************
 * BeginCwInterface - Starts the CW interface code
 *
 * Returns: Void.
 ********************************************************************/

VOID BeginCwInterface (VOID)
{
  ExamineDisplay();

  if (GetCwSumInfo())
    exit (1);

#ifdef BASED

  /* Initialize the CW window segment */

  if (!WndSegInit())
    {
      PutString ("Error creating window segment");
      exit(2);
    }
#endif

  /* Initialize CW */

  if (!FInitCow())
    {
      PutString ("Error initializing COW");
      exit (2);
    }

  SetCursorBlink (TRUE);

  /* SaveOriginalScreen(); */

  SetIsaColors (fBlackWhite);

  InitModeMapping();

  if (!FInitScreenMode (-1))
    {
      PutString ("Error initializing screen");
      exit (2);
    }

  EnableKeyboard (TRUE);

  EnableOverlap (TRUE, (PWND) NULL, 0);

  SetCWHook (WH_FRAME, FrameFilter);

  fCwIsReady = TRUE;

  InitMainWindows();

  InitMsgBoxPlateBtns();

  InitMyEditItem();

  WarnWindowsUser();


  /* Main Message Pump */

  while (1)
    {
      MSG msg;

      /* Dispatch messages until queue empty */

      while (PeekMessage (&msg))
        {
          DispatchMessage (&msg);
        }

      while (!FMessage())    /* DOS wait */
        {
          /* If using FMessage(), application must poll keyboard */

          if (fPollKeyboard)
            PollKeyboard();
        }
    }

  /* NOT REACHED */
}


/*********************************************************************
 * GetCwSumInfo - Get the summary information while using the CW
 *                interface.
 *
 * Returns:  TRUE if an error occured.
 *********************************************************************/

BOOL GetCwSumInfo (VOID)
{
  WORD wSize;             /* Size of SUMMARY_STRUCT */


  /* Obtain the info structure's size */
  wSize = GetInfoSize (IDI_SUMMARY_SCREEN, FALSE);

  if (wSize == 0)
    return (TRUE);


  /* Allocate enough room for the info structure */
  pSum = malloc (wSize);

  if (pSum == NULL)
    {
      OutOfMemory();
      return (TRUE);
    }

  /* Zero out the structure */
  memset (pSum, '\0', wSize);

  /* Fill the structure with the information */
  if (GetInfo (IDI_SUMMARY_SCREEN, pSum, FALSE, FALSE, TRUE))
    {
      free (pSum);
      return (TRUE);
    }

  return (FALSE);
}


AY FAR PASCAL GetUILine (VOID)
{
  return ((AY) 0);
}


VOID ScreenRedraw (VOID)
{
  DrawWindow ( NULL );
}


/*********************************************************************
 * SetIsaColors - Sets the correct colors for the CW MSD
 *
 * fBlackWhiteFlag - Uses the black and white color set if TRUE.
 *********************************************************************/

VOID SetIsaColors (BOOL fBlackWhiteFlag)
{
  ISA_PAIR  *pisap = NULL;  /* Pointer to the correct ISA */
  ISA  i;                   /* Looping variable           */


  /* Choose between color and black and white */
  if (fBlackWhiteFlag)
    pisap = aisapBlackAndWhite;
  else
    pisap = aisapColor;

  /* Set the screen colors */

  for (i = 0; i < isaMax; ++i)
    SetIsaColor (i, pisap[i].wFore, pisap[i].wBack);

  CheckMenuItem (midBlackWhite, fBlackWhiteFlag);

  if (fCwIsReady)
    DrawWindow (pwndDesktop);
}


/*******************************/
/* Initialize the main windows */
/*******************************/

VOID FAR InitMainWindows (VOID)
{
  /* Register the main window */

  if (!RegisterClass (0,
                      0,
                      0,
                      MainProcExtraBytes,
                      MainProcCallID,
                      MainWndProc))

    {
      return;
    }

  /* Register the Info Provider window */

  if (!RegisterClass (0,
                      0,
                      0,
                      MainProcExtraBytes,
                      InfoWndCallID,
                      InfoWndProc))

    {
      return;
    }

  /* Register the Info Provider text window */

  if (!RegisterClass (0,
                      0,
                      0,
                      MainProcExtraBytes,
                      InfoTxtWndCallID,
                      InfoTxtWndProc))

    {
      return;
    }

  /* Register the status line window */

  if (!RegisterClass (0,
                      0,
                      0,
                      MainProcExtraBytes,
                      StatusLineCallID,
                      StatusLineProc))

    {
      return;
    }


  /* Create the status line window */

  pwndStatusLine = (PWND) CreateWindow (StatusLineCallID,
                                        NULL,
                                        NULL,
                                        WS_VISIBLE,
                                        0, ayMac - (AY) 1, axMac, 1,
                                        (HWND) pwndDesktop,
                                        (HMENU) NULL,
                                        NULL,
                                        1);
  if (pwndStatusLine == NULL)
    return;

  ShowWindow (pwndStatusLine, SW_VISIBLE, TRUE);


  /* Create the main window */

  pwndMainFrame = (PWND) CreateWindow (MainProcCallID,
                                       NULL,
                                       WS_CLIPOUT,
                                       WS_FRAME,
                                       0, 0, axMac, ayMac - (AY) 1,
                                       (HWND) pwndDesktop,
                                       (HMENU) hmnuMenuBar,
                                       NULL,
                                       1);
  if (pwndMainFrame == NULL)
    return;

  EnableWindow (pwndMainFrame, TRUE);


  ShowWindow (NULL, SW_VISIBLE, TRUE);

  /* Enable mouse */

  FEnableMouse(TRUE);

  /* AddAccelTable (&prgmpvkeyid); */

  SetFocus (pwndMainFrame);
}


/*********************************************************************
 * MainWndProc - Window to display the pull down menus, the summary
 *               information, and the buttons
 *********************************************************************/

STATIC LONG FAR PASCAL MainWndProc (PWND  pwnd,
                                    WORD  message,
                                    WORD  wParam,
                                    DWORD lParam)
{
  WORD i;             /* Looping variable                      */
  WORD wKeyState;     /* Shift key state when a key is pressed */
  static struct BUTTON_INFO
    {
      PWND pwndInfo;  /* PWND of the button's Info Window, or  */
                      /*   NULL if there is no Info window up  */
                      /*   for that button.                    */
      CHAR chAccel;   /* Accelerator character for the button  */
      PSZ  pszHelp;   /* Help text for info window             */
    } ButtonInfo[] =
        {
          { NULL, 'P', "Computer: Displays computer, BIOS, and processor information." },
          { NULL, 'M', "Memory: Displays visual memory map and various types of memory." },
          { NULL, 'V', "Video: Displays video adapter make, model, type, and BIOS." },
          { NULL, 'N', "Network: Displays network information." },
          { NULL, 'O', "OS Version: Displays operating system information." },
          { NULL, 'U', "Mouse: Displays mouse information." },
          { NULL, 'A', "Other Adapters: Displays game adapter information." },
          { NULL, 'D', "Disk Drives: Displays disk drive types and sizes." },
          { NULL, 'L', "LPT Ports: Displays status of parallel ports." },
          { NULL, 'C', "COM Ports: Displays status of serial ports." },
          { NULL, 'Q', "IRQ Status: Displays current usage of hardware interrupts." },
          { NULL, 'T', "TSR Programs: Displays allocated memory control blocks." },
          { NULL, 'R', "Device Drivers: Displays installable device driver information." }
        };

  switch (message)
    {
      case WM_CREATE:
        CreateChildWindows (pwnd);
        break;

      case WM_PAINT:
        {
          NPRRC  prrc;
          RRC    rrc;

          /* clear the window */

          if ((prrc = (NPRRC) LOWORD (lParam)) == NULL)
            {
              GetClientRrc(pwnd, &rrc);
              prrc = &rrc;
            }

          FillRrc (pwnd, prrc, ' ', isaSummaryText);

          WriteSummaryText (pwnd);

          break;
        }

      case WM_COMMAND:
        {
          switch (wParam)
            {
              case BaseBtnID +  0:
              case BaseBtnID +  1:
              case BaseBtnID +  2:
              case BaseBtnID +  3:
              case BaseBtnID +  4:
              case BaseBtnID +  5:
              case BaseBtnID +  6:
              case BaseBtnID +  7:
              case BaseBtnID +  8:
              case BaseBtnID +  9:
              case BaseBtnID + 10:
              case BaseBtnID + 11:
              case BaseBtnID + 12:
              case BaseBtnID + 13:
                if (HIWORD (lParam) == BN_CLICKED)
                  {
                    if (ButtonInfo[wParam - BaseBtnID].pwndInfo)
                      {
                        /* Bring up the entire info window */
                        SetFocus (ButtonInfo[wParam - BaseBtnID].pwndInfo);
                      }
                    else
                      {
#if HEAP_DEBUG
                        CHAR chBuffer[80];

                        sprintf (chBuffer, "_memavl = %u, _memmax = %u",
                                 _memavl(), _memmax());

                        ShowStatus (chBuffer);
#endif
                        DisplayStatus (ST_WORKING);
                        ButtonInfo[wParam - BaseBtnID].pwndInfo =
                            ShowInfo (wParam - BaseBtnID + 4);
                        ShowStatus (ButtonInfo[wParam - BaseBtnID].pszHelp);
                      }
                  }
                break;

              default:
                MenuCommand (pwnd, wParam);
                break;
            }

          break;
        }

      case WM_INFO_WND_CLOSED:
        {
          WORD wNmbrOpen = 0;       /* Number of open info windows      */

          /* Zero out the window pointer in the Button Info structure */
          for (i = 0; i < NumBtns; ++i)
            {
              if (ButtonInfo[i].pwndInfo == (PWND) wParam)
                ButtonInfo[i].pwndInfo = NULL;

              /* Count up the number of open info windows */
              if (ButtonInfo[i].pwndInfo != NULL)
                ++wNmbrOpen;
            }

          /* If we just closed the last info window, set the */
          /*   focus back here                               */

          if (wNmbrOpen == 0)
            SetFocus (pwnd);

          DisplayStatus (0);

          break;
        }

      case WM_DRAWITEM:
        {
          PWND_BTN pwndbtn;             /* Button info structure     */
          RRC rrcButton;                /* Button's relative co-ords */
          RRC rrcShadow;                /* Shadow's relative co-ords */
          DWORD dwButtonState;          /* Button's state            */
          DRAWITEMSTRUCT FAR * fpdis;   /* Pointer to struct for     */
                                        /*   drawing the button      */
          WORD wCenterLoc;              /* Location to center the    */
                                        /*   button's text           */
          WORD wAccelPos;               /* This button's accel pos   */
          static WORD awAccelPos[] =  /* Button Accelerator's position */
            {
              4, /* Com&puter       */
              1, /* &Memory         */
              1, /* &Video          */
              1, /* &Network        */
              1, /* &OS Version     */
              3, /* Mo&use          */
              7, /* Other &Adapters */
              1, /* &Disk Drives    */
              1, /* &LPT Ports      */
              1, /* &COM Ports      */
              3, /* IR&Q Status     */
              1, /* &TSR Programs   */
              9  /* Device D&rivers */
            };

          /* Set the struct pointer */
          fpdis = (DRAWITEMSTRUCT FAR *) lParam;
          pwndbtn = (PWND_BTN) fpdis->hwndItem;

          /* Get the button and shadow sizes */
          _fmemcpy (&rrcButton, &(fpdis->rcItem), sizeof (RRC));
          _fmemcpy (&rrcShadow, &(fpdis->rcItem), sizeof (RRC));

          ++(rrcShadow.ryTop);
          ++(rrcShadow.ryTop);

          /* Find the centering location */
          wCenterLoc = ((rrcButton.rxRight - rrcButton.rxLeft) / 2) -
                       (strlen (pwndbtn->szDialog) / 2);

          /* Determine if the button is pushed in or not */
          dwButtonState = SendMessage (fpdis->hwndItem, BM_GETSTATE, 0, 0L);
          if (dwButtonState & 0x0004)
            {
              /* The button is pressed in */

              /* Repaint the shadow area */
              FillRrc (fpdis->hwndItem, &rrcShadow, 'ß', isaSummaryText);

              CharOut (fpdis->hwndItem, 0, 0, ' ', isaSummaryText);
              CharOut (fpdis->hwndItem, 0, 1, ' ', isaSummaryText);
              CharOut (fpdis->hwndItem, 0, 2, ' ', isaSummaryText);


              /* Repaint the button face */
              ++(rrcButton.rxLeft);

              rrcButton.ryBottom -= 2;
              FillRrc (fpdis->hwndItem, &rrcButton, 'Ü', isaSummaryBtnShadow);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, ' ', isaPushButton);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, 'ß', isaSummaryBtnShadow);

              TextOut (fpdis->hwndItem, (RY) (wCenterLoc + 1), 1,
                       pwndbtn->szDialog, -1, isaPushButton);
            }
          else
            {
              /* The button is out */

              /* Repaint the button face */
              --(rrcButton.rxRight);
              wAccelPos = awAccelPos[fpdis->CtlID - BaseBtnID] - 1;

              rrcButton.ryBottom -= 2;
              FillRrc (fpdis->hwndItem, &rrcButton, 'Ü', isaSummaryBtnShadow);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, ' ', isaPushButton);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, 'Ü', isa3DPushBtnOut);

              /* Lower left corner */
              CharOut (fpdis->hwndItem,
                       rrcButton.rxLeft, 2,
                       'ß',
                       isaSummaryBtnShadow);

              /* Upper right corner */
              CharOut (fpdis->hwndItem,
                       rrcButton.rxRight, 0,
                       ' ',
                       isaSummaryText);


              /* Repaint the shadow area to the right */
              CharOut (fpdis->hwndItem, rrcButton.rxRight, 1,
                       'Û', isa3DPushBtnOut);

              CharOut (fpdis->hwndItem, rrcButton.rxRight, 2,
                       'Û', isa3DPushBtnOut);


              /* Display the button's text */
              TextOut (fpdis->hwndItem, (RY) (wCenterLoc), 1,
                       pwndbtn->szDialog, -1, isaPushButton);


              /* Highlight the accellerator for the button */
              CharOut (fpdis->hwndItem, (RY) (wCenterLoc + wAccelPos),
                       1, pwndbtn->szDialog[wAccelPos], isa3DBtnHilite);
            }

          break;
        }

      case WM_MENUSELECT:
        {
          DisplayStatus (wParam);
        }

      case WM_CHAR:
        {
          float fFloat = (float) 0.0;


          wKeyState = HIWORD (lParam);

          /* Black & White keystroke */
          /* F5 */
          if ((wKeyState & (KK_MENU | KK_CONTROL | KK_SHIFT)) == 0 &&
              wParam == VK_F5)
            {
              SetIsaColors (fBlackWhite = !fBlackWhite);
              break;
            }


          /* Exit keystrokes */
          /* F3 */
          if ((wKeyState & (KK_MENU | KK_CONTROL | KK_SHIFT)) == 0 &&
              wParam == VK_F3)
            Exit (0);

          /* Alt-F4 */
          if ((wKeyState & (KK_MENU | KK_CONTROL | KK_SHIFT)) == KK_MENU &&
              wParam == VK_F4)
            Exit (0);

          if ((wKeyState & (KK_MENU | KK_CONTROL | KK_SHIFT)) == 0)
            {
              /* Loop through the list of accelerator keys, and */
              /*   bring up the correct window                  */
              for (i = 0; i < NumBtns; ++i)
                if (ButtonInfo[i].chAccel == (CHAR) toupper(wParam))
                  {
                    SendMessage (GetDlgItem (pwnd, i + BaseBtnID),
                                 WM_LBUTTONDOWN,
                                 MK_LBUTTON,
                                 NULL);
                    PostMessage (GetDlgItem (pwnd, i + BaseBtnID),
                                 WM_LBUTTONUP,
                                 NULL,
                                 NULL);
                    break;
                  }
              }

          break;
        }

      case WM_CLOSE:
        DestroyWindow (pwnd);
        Exit (0);

      default:
        return (DefWindowProc (pwnd, message, wParam, lParam));
    }

  return (0L);
}


/*********************************************************************
 * CreateChildWindows - Creates the child windows of the main
 *                      "Summary" window.
 *********************************************************************/

STATIC BOOL PASCAL CreateChildWindows (PWND pwndParent)
{
  WORD i;             /* Looping variable                 */
  AX X;               /* X Coordinate                     */
  AY Y;               /* Y Coordinate                     */
  PWND pwnd = NULL;   /* Current window pointer           */
  AY ayButtonHeight;  /* Height, in lines, of the buttons */
  AY ayBtnSeparator;  /* Distance between buttons         */
  AY ayStart;         /* Starting location for buttons    */

  /* Set the button height */
  ayButtonHeight = 3;

  /* Calculate the separation between buttons */
  ayBtnSeparator = (ayMac > (AY) 25) ? (AY) 1 : (AY) 0;

  ayStart = (AY) ((ayMac / (AY) 2) -
            ((NumBtns + 1) / 2 * (ayButtonHeight + ayBtnSeparator)) / 2);

  X = SUM_COLUMN_ONE;
  Y = ayStart;

  for (i = 0; i < NumBtns; i++)
    {
      if (X < 40 && i > NumBtns / 2)
        {
          X = SUM_COLUMN_TWO;
          Y = ayStart;
        }

      pwnd = (PWND)CreateWindow (WC_BUTTON,
                                 paszMainBtnArray[i],
                                 BS_PUSHBUTTON | BS_OWNERDRAW,
                                 WES_3D | WES_TABSTOP,
                                 X, Y,
                                 SUM_BUTTON_WIDTH,
                                 ayButtonHeight,
                                 (HWND) pwndParent,
                                 NULL,
                                 NULL,
                                 BaseBtnID + i);

      Y += ayButtonHeight + ayBtnSeparator;

      if (pwnd == NULL)
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * WriteSummaryText - Display the summary text next to the buttons
 *********************************************************************/

STATIC BOOL PASCAL WriteSummaryText (PWND pwndParent)
{
  WORD i;             /* Looping variable                         */
  WORD wIndex;        /* Index to summary strings                 */
  RX X;               /* X Coordinate                             */
  RY Y;               /* Y Coordinate                             */
  AY ayButtonHeight;  /* Height, in lines, of the buttons         */
  AY ayBtnSeparator;  /* Distance between buttons         */
  short sLength;      /* String length                            */
  RY ryStart;         /* Starting point for locating buttons/text */

  /* Set the button height */
  ayButtonHeight = (ayMac > (AY) 25) ? (AY) 3 : (AY) 3;

  /* Calculate the separation between buttons */
  ayBtnSeparator = (ayMac > (AY) 25) ? (AY) 1 : (AY) 0;

  ryStart = (RY) ((ayMac / (RY) 2) -
            ((NumBtns + 1) / 2 * (ayButtonHeight + ayBtnSeparator)) / 2);

  X = 23;
  Y = ryStart;

  for (i = 0, wIndex = IDI_FIRST_RECORD_TO_SUMMARIZE * 2;
       i < NumBtns; ++i, ++wIndex)
    {
      if (X < 40 && i > NumBtns / 2)
        {
          X = 40 + SUM_BUTTON_WIDTH + 7;
          Y = ryStart;
        }

      /* Set the maximum length for summary text line 1 */

      sLength = strlen (pSum->szSumStrings[wIndex]);

      if (X < 40)
        {
          if (sLength > SUM_COLUMN_TWO - X - 2)
            sLength = SUM_COLUMN_TWO - X - 2;
        }
      else
        {
          if (sLength > axMac - X - 1)
            sLength = axMac - X - 1;
        }

      TextOut (pwndParent, X, Y, pSum->szSumStrings[wIndex], sLength,
               isaSummaryText);

      /* Set the maximum length for summary text line 2 */
      ++wIndex;
      sLength = strlen (pSum->szSumStrings[wIndex]);

      if (X < 40)
        {
          if (sLength > SUM_COLUMN_TWO - X - 2)
            sLength = SUM_COLUMN_TWO - X - 2;
        }
      else
        {
          if (sLength > axMac - X - 1)
            sLength = axMac - X - 1;
        }

      TextOut (pwndParent, X, ++Y, pSum->szSumStrings[wIndex],
               sLength, isaSummaryText);

      --Y;

      Y += ayButtonHeight + ayBtnSeparator;
    }

  return (TRUE);
}


/*
AY FAR PASCAL GetUILine (VOID)
{
  return ((AY) 0);
}


VOID ScreenRedraw (VOID)
{
  DrawWindow (NULL);
}
*/


char FAR * FARPUBLIC LpvDerefAppText (PWND pwnd, WORD hText, WORD *cch)
{
  /* Minimum code required for far data. */

  CHAR * chPointer = (CHAR *) hText;

  *cch = strlen ((char *) hText);

  return ((char FAR *) chPointer);
}


/*********************************************************************
 * FrameFilter - Filters frame messages
 *********************************************************************/

STATIC BOOL FARPUBLIC FrameFilter (PMSG pmsg, WORD *pwRegion)
{
  if (*pwRegion == FZ_CLOSE ||
      *pwRegion == FZ_TMOVE)
    return (TRUE);

  return (FALSE);
}


/*********************************************************************
 * InitMsgBoxPlateBtns - Initializes "Plate" buttons for MessageBox'es
 *********************************************************************/

VOID InitMsgBoxPlateBtns (VOID)
{
  PWNDCLASS pcls;

  pcls = FindClass (WC_MSGBOX);

  if (pcls == NULL)
    return;

  OldMsgBoxProc = pcls->pfnWndProc;

  pcls->pfnWndProc = MyMsgBoxProc;
}


/*********************************************************************
 * CreateMsgBtn - Allows me to put plate buttons on MessageBox'es
 *********************************************************************/

PWND FAR CreateMsgBtn ( szWindowName,
                        wStyle, wExStyle,
                        X, Y,
                        nWidth, nHeight,
                        hwndDlg,
                        id )
char * szWindowName;
WORD   wStyle;
WORD   wExStyle;
WORD   X;
WORD   Y;
WORD   nWidth;
WORD   nHeight;
HWND   hwndDlg;
WORD   id;
{
  HWND hwnd;

  hwnd = (HWND) ((PWND_BTN) CreateWindow (WC_BUTTON,
                                          szWindowName,
                                          wStyle | BS_OWNERDRAW,
                                          wExStyle,
                                          X,
                                          Y,
                                          nWidth + 1,
                                          nHeight,
                                          (HWND)hwndDlg,
                                          NULL,
                                          NULL,
                                          id));

  return (hwnd);
}


/*********************************************************************
 * MyMsgBoxProc - Message box button drawing facility.
 *********************************************************************/

LONG FAR
MyMsgBoxProc (REGISTER PWND_DLG pwnd,
              WORD  message,
              WORD  wParam,
              DWORD lParam)
{
  switch (message)
    {
      case WM_DRAWITEM:
        {
          PWND_BTN pwndbtn;             /* Button info structure     */
          RRC rrcButton;                /* Button's relative co-ords */
          RRC rrcShadow;                /* Shadow's relative co-ords */
          DWORD dwButtonState;          /* Button's state            */
          DRAWITEMSTRUCT FAR * fpdis;   /* Pointer to struct for     */
                                        /*   drawing the button      */
          WORD wCenterLoc;              /* Location to center the    */
                                        /*   button's text           */

          /* Set the struct pointer */
          fpdis = (DRAWITEMSTRUCT FAR *) lParam;
          pwndbtn = (PWND_BTN) fpdis->hwndItem;

          /* Get the button and shadow sizes */
          _fmemcpy (&rrcButton, &(fpdis->rcItem), sizeof (RRC));
          _fmemcpy (&rrcShadow, &(fpdis->rcItem), sizeof (RRC));

          ++(rrcShadow.ryTop);
          ++(rrcShadow.ryTop);

          /* Find the centering location */
          wCenterLoc = ((rrcButton.rxRight - 1 - rrcButton.rxLeft) / 2) -
                       (strlen (pwndbtn->szDialog) / 2);

          /* Determine if the button is pushed in or not */
          dwButtonState = SendMessage (fpdis->hwndItem, BM_GETSTATE, 0, 0L);
          if (dwButtonState & 0x0004)
            {
              /* The button is pressed in */

              /* Repaint the shadow area */
              FillRrc (fpdis->hwndItem, &rrcShadow, 'ß', isaDialogBox);

              CharOut (fpdis->hwndItem, 0, 0, ' ', isaDialogBox);
              CharOut (fpdis->hwndItem, 0, 1, ' ', isaDialogBox);
              CharOut (fpdis->hwndItem, 0, 2, ' ', isaDialogBox);


              /* Repaint the button face */
              ++(rrcButton.rxLeft);

              rrcButton.ryBottom -= 2;
              FillRrc (fpdis->hwndItem, &rrcButton, 'Ü', isaMessageBtn1);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, ' ', isaPushButton);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, 'ß', isaMessageBtn1);

              TextOut (fpdis->hwndItem, (RY) (wCenterLoc + 1), 1,
                       pwndbtn->szDialog, -1, isaPushButton);
            }
          else
            {
              /* The button is out */

              /* Repaint the button face */
              --(rrcButton.rxRight);

              rrcButton.ryBottom -= 2;
              FillRrc (fpdis->hwndItem, &rrcButton, 'Ü', isaMessageBtn1);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, ' ', isaPushButton);

              ++(rrcButton.ryBottom);
              ++(rrcButton.ryTop);
              FillRrc (fpdis->hwndItem, &rrcButton, 'Ü', isa3DPushBtnOut);

              /* Lower left corner */
              CharOut (fpdis->hwndItem,
                       rrcButton.rxLeft, 2,
                       'ß',
                       isaMessageBtn1);

              /* Upper right corner */
              CharOut (fpdis->hwndItem,
                       rrcButton.rxRight, 0,
                       ' ',
                       isaDialogBox);


              /* Repaint the shadow area to the right */
              CharOut (fpdis->hwndItem, rrcButton.rxRight, 1,
                       'Û', isa3DPushBtnOut);

              CharOut (fpdis->hwndItem, rrcButton.rxRight, 2,
                       'Û', isa3DPushBtnOut);


              /* Display the button's text */
              TextOut (fpdis->hwndItem, (RY) (wCenterLoc), 1,
                       pwndbtn->szDialog, -1, isaPushButton);
            }

          break;
        }

      default:
        return ( (*(PLFN_WNDPROC)(OldMsgBoxProc))((PWND)pwnd, message, wParam, lParam));
    }
}



/*********************************************************************
 * InitMyEditItem - Inits the subclassing of edit items.
 *********************************************************************/

VOID InitMyEditItem (VOID)
{
  PWNDCLASS pcls;

  pcls = FindClass (WC_EDIT);

  if (pcls == NULL)
    return;

  OldEditItemProc = pcls->pfnWndProc;

  pcls->pfnWndProc = MyEditItemProc;
}


/*********************************************************************
 * MyEditItemProc - Edit Item handler (sends the message MY_EN_SETFOCUS).
 *********************************************************************/

LONG FAR
MyEditItemProc (REGISTER PWND_EDIT pwnd,
                WORD  message,
                WORD  wParam,
                DWORD lParam)
{
  if (message == WM_SETFOCUS)
    SendMessage (pwnd->pwndParent, MY_EN_SETFOCUS, 0, 0L);

  return ((*(PLFN_WNDPROC)(OldEditItemProc))((PWND)pwnd, message, wParam, lParam));
}



/*********************************************************************
 * StatusLineProc - Displays the status line at the bottom of the
 *                  screen.
 *********************************************************************/

STATIC LONG FAR PASCAL StatusLineProc (PWND  pwnd,
                                       WORD  message,
                                       WORD  wParam,
                                       DWORD lParam)
{
  static STATUS_LINE_STRINGS sls[] =
    {
      { enClear,         "Press ALT for menu, or press highlighted letter, or F3 to quit MSD." },
      { enCommand,       NULL },

      { midFile,         "Finds files, prints reports, exits." },
      { midFind,         "Locates and views files." },
      { midReport,       "Prints a report to a printer or a file." },
      { midAutoexecBat,  "Views AUTOEXEC.BAT." },
      { midConfigSys,    "Views CONFIG.SYS." },
      { midSystemIni,    "Locates and views SYSTEM.INI." },
      { midWinIni,       "Locates and views WIN.INI." },
      { midQuit,         "Exits MSD." },

      { midUtil,         "Memory views, insert commands, test printer, color toggle." },
      { midBlockDisplay, "Displays allocated memory on visual memory map." },
      { midBrowser,      "Searches for key words in ROM areas." },
      { midInsert,       "Inserts common commands in system files." },
      { midPrtTst,       "Tests printer connection." },
      { midBlackWhite,   "Toggles between color and black & white." },

      { midHelp,         "Information about MSD." },
      { midAbout,        "Displays MSD version." },

      { ST_WARN_WINDOWS_USER, "Press ENTER to continue, or ESC to quit MSD." },
      { ST_WORKING,      "Working ..." },
      { ST_SEARCHING,    "Searching ..." },
      { ST_REPORT_COMPLETED, "Report Completed." },
      { ST_INSERT_DLG1,  "Choose command to insert into specified file", },
      { ST_INSERT_DLG2,  "Edit or replace Command, Section and Filename." },
      { ST_INSERT_DLG3,  "Choose file to modify and press ENTER." },
      { ST_INSERT_DLG4,  "Choose action and press ENTER." },
      { ST_CUST_INFO,    "Press TAB to move to the next field, ENTER to continue." },
      { ST_VIEW_WHICH_FILE, "Choose file to view and press ENTER." },
      { ST_FINDFILE2,    "Choose the file to view and press ENTER." },

      { 0xFFFF,          NULL }
    };

  static PSZ pszCurrentString;


  switch (message)
    {
      case WM_CREATE:
        pszCurrentString = sls[0].pszString;
        break;

      case WM_ACTIVATE:
        return (rspActiveDecline);

      case WM_NEW_STATUS_LINE_STRING:
        {
          if (lParam == NULL)
            {
              WORD i;

              for (i = 0; sls[i].wStringNmbr != wParam &&
                          sls[i].wStringNmbr != 0xFFFF;   ++i)
                ;
              if (sls[i].wStringNmbr != 0xFFFF)
                pszCurrentString = sls[i].pszString;
              else
                pszCurrentString = sls[0].pszString;
            }
          else
            {
              static CHAR chBuffer[80];

              strcpy (chBuffer, (PSZ) lParam);
              pszCurrentString = chBuffer;
            }
        }

        /* Fall through to WM_PAINT */

      case WM_PAINT:
        {
          WORD wLength;       /* Length of the displayed string                */
          RX   rxCurrentPos;  /* Current position for clearing the status line */

          /* Check to see if we have a message to display */
          if (pszCurrentString == NULL)
            break;

          /* Display the message */
          wLength = strlen (pszCurrentString);
          wLength = (wLength > 80) ? 80 : wLength;
          TextOut (pwndStatusLine, 0, 0, pszCurrentString,
                   wLength, isaStatusLine);

          /* Clear out the rest of the status line */
          rxCurrentPos = (RY) wLength;
          wLength = axMac - wLength;
          while (wLength--)
            CharOut (pwndStatusLine, rxCurrentPos++, 0, ' ', isaStatusLine);
          break;
        }

      case WM_CLOSE:
        DestroyWindow (pwnd);
        Exit (0);

      default:
        return (DefWindowProc (pwnd, message, wParam, lParam));
    }

  return (0L);
}


/*********************************************************************
 * WarnWindowsUser - Displays a simple warning/informational message
 *                   when MSD is brought up under Windows.
 *********************************************************************/

VOID WarnWindowsUser (VOID)
{
  WORD wWindowsType;
  WORD wWindowsMinor;
  WORD wWindowsMajor;
  BOOL fDosShellTaskSwitcher;
  BOOL fReturnValue;
  CHAR chBuffer[80];


  /* Since this is the location for warnings, I put */
  /*   the TEMP/TMP invalid warning here.           */

  if (fTempPathInvalid)
    {
      strcpy (chBuffer, pszTemp);
      strcat (chBuffer, "=");
      strcat (chBuffer, getenv (pszTemp));
      MessageBox (pszEnvironInvalid, chBuffer, NULL, MB_OK | 0x8000);
    }

  if (fTmpPathInvalid)
    {
      strcpy (chBuffer, pszTmp);
      strcat (chBuffer, "=");
      strcat (chBuffer, getenv (pszTmp));
      MessageBox (pszEnvironInvalid, chBuffer, NULL, MB_OK | 0x8000);
    }


  /* Determine if Windows is running */
  WinVerDetect (&wWindowsType,  &wWindowsMajor,
                &wWindowsMinor, &fDosShellTaskSwitcher);

  /* Display an appropriate message if Windows is running */
  if (wWindowsType != NO_WINDOWS)
    {
      /*
      CHAR chBuffer[80];
      static PSZ pszLine1 =
                      "You are running %s.  The information MSD";

      switch (wWindowsType)
        {
          case WIN_286:
          case WIN_386:
            sprintf (chBuffer, pszLine1, "Windows version 2.x");
            break;

          case WIN_REAL_MODE:
            sprintf (chBuffer, pszLine1, "Real mode Windows");
            break;

          case WIN_STANDARD_MODE:
            sprintf (chBuffer, pszLine1, "Standard mode Windows");
            break;

          case WIN_ENHANCED_MODE:
            sprintf (chBuffer, pszLine1, "386 Enhanced mode Windows");
            break;

          default:
            sprintf (chBuffer, pszLine1, "Microsoft Windows");
        }

      DisplayStatus (ST_WARN_WINDOWS_USER);

      fReturnValue = DialogBox (&dlgWarnWindowsUser, WarnWindowsUserDlg);

      fReturnValue = MessageBox (chBuffer,
                  "provides may be more complete or accurate if you run MSD outside",
                  "of Windows.  Choose OK to continue or Cancel to quit MSD.",
                  MB_OKCANCEL | 0x8000);

      DisplayStatus (0);

      if (fReturnValue != IDOK)
        Exit (0);

      */

      DisplayStatus (ST_WARN_WINDOWS_USER);

      fReturnValue = DialogBox (&dlgWarnWindowsUser, WarnWindowsUserDlg);

      DisplayStatus (0);

      if (fReturnValue != IDOK)
        Exit (0);
    }
}


/*********************************************************************
 * FHelpMsg - Replaces the CW supplied help callback with this
 *            routine.  By returning FALSE from this routine, the CW
 *            help engine is never executed.
 *********************************************************************/

BOOL FHelpMsg (PMSG pmsg, WORD cnx)
{
  return (FALSE);
}


/*********************************************************************
 * Help - Replaces the CW supplied help message with this routine
 *********************************************************************/

VOID FARPUBLIC Help (WORD hem, WORD hid, VOID *pv, WORD kk)
{
}


/*********************************************************************
 * ExamineDisplay - Displays "MSD is examining your system"
 *********************************************************************/

#define EXAM_BACKGROUND            0x17
#define EXAM_WINDOW_BORDER         0x47
#define EXAM_WINDOW_AREA           0x4F
#define EXAM_WINDOW_HIGHLIGHT      0xCE
#define BW_EXAM_BACKGROUND         0x07
#define BW_EXAM_WINDOW_BORDER      0x70
#define BW_EXAM_WINDOW_AREA        0x70
#define BW_EXAM_WINDOW_HIGHLIGHT   0xF0

VOID ExamineDisplay (VOID)
{
  BYTE FAR * fpLines = (BYTE FAR *) 0x00400084;
                              /* Number of lines on the display        */
  WORD wNmbrLines = (*fpLines) + 1;
  WORD wLineAdj;              /* Number of lines to add (for 43 and 50 */
                              /*   line displays)                      */
  WORD wExamBackground;       /* Screen background color               */
  WORD wExamWindowBorder;     /* Color of window border                */
  WORD wExamWindowArea;       /* Color of window text area             */
  WORD wExamWindowHighlight;  /* Window highlight color (blinking)     */
  WORD wMenuBar;              /* Menu bar attribute                    */
  WORD wStatusBar;            /* Status bar attribute                  */


  /* Set the colors */
  if (fBlackWhite)
    {
      wExamBackground      = BW_EXAM_BACKGROUND;
      wExamWindowBorder    = BW_EXAM_WINDOW_BORDER;
      wExamWindowArea      = BW_EXAM_WINDOW_AREA;
      wExamWindowHighlight = BW_EXAM_WINDOW_HIGHLIGHT;
      wMenuBar             = 0x70;
      wStatusBar           = 0x70;
    }
  else
    {
      wExamBackground      = EXAM_BACKGROUND;
      wExamWindowBorder    = EXAM_WINDOW_BORDER;
      wExamWindowArea      = EXAM_WINDOW_AREA;
      wExamWindowHighlight = EXAM_WINDOW_HIGHLIGHT;
      wMenuBar             = 0x70;
      wStatusBar           = 0x70;
    }

  /* Set the line adjust amount */
  if (wNmbrLines == 43)
    wLineAdj = 9;
  else if (wNmbrLines == 50)
    wLineAdj = 13;
  else
    wLineAdj = 0;

  BiosClearScreen (wExamBackground);

  /* Fake menu and status bar */
  BiosCharOutAt (' ', wMenuBar, 80, 0, 0);
  BiosStringOutAt ("  File  Utilities  Help", wMenuBar, 0, 0);
  BiosCharOutAt (' ', wStatusBar, 80, 0, wNmbrLines - 1);
  BiosStringOutAt ("MSD is examining your system", wStatusBar, 0, wNmbrLines - 1);


  /* Draw the information box */
  BiosDrawFilledBox (12, 4 + wLineAdj, 68, 19 + wLineAdj,
                     wExamWindowBorder, wExamWindowArea);

  BiosStringOutAt ("Microsoft (R) Diagnostics",
                   wExamWindowArea, 27, 6 + wLineAdj);
  BiosStringOutAt ("Version ",
                   wExamWindowArea, 33, 7 + wLineAdj);
  BiosStringOut (pszVersionNumber, wExamWindowArea);

  BiosStringOutAt ("Copyright (C) Microsoft Corporation, 1990-92",
                   wExamWindowArea, 17, 8 + wLineAdj);
  BiosStringOutAt ("All Rights Reserved",
                   wExamWindowArea, 30, 9 + wLineAdj);

  BiosStringOutAt ("The Microsoft Diagnostics are  designed  to  assist",
                   wExamWindowArea, 15, 11 + wLineAdj);
  BiosStringOutAt ("Microsoft Product Support  personnel  in  obtaining",
                   wExamWindowArea, 15, 12 + wLineAdj);
  BiosStringOutAt ("detailed technical information about your computer.",
                   wExamWindowArea, 15, 13 + wLineAdj);

  BiosStringOutAt ("Thank you for using Microsoft Products.",
                   wExamWindowArea, 20, 15 + wLineAdj);

#if 0
  BiosStringOutAt (paszCommandLineHelp[0],
                   wExamWindowArea, 5, 18 + wLineAdj);
#endif

  BiosStringOutAt ("MSD is examining your system ...",
                   wExamWindowHighlight, 23, 17 + wLineAdj);

  /* Put the cursor in the shadow */
  BiosLocate (14, 20 + wLineAdj);
}

#endif /* CW_INCLUDED */
