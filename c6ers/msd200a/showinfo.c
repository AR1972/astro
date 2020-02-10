/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * SHOWINFO.C - Source file for displaying information
 ********************************************************************/


/* Include Files */

#include "msd.h"


#ifdef CW_INCLUDED

/********************************************************************
 * ShowInfo - Displays the appropriate record in the info window.
 *
 * wRecordType - Record to display
 *
 * Returns: PWND of the info window, NULL if an error condition
 *          occured.
 ********************************************************************/

PWND ShowInfo (WORD wRecordType)
{
  VOID *pStructForInfo = NULL;  /* Pointer to structure with data   */
  WORD wStructSize;             /* Stores memory requirements for   */
                                /*   GetInfo record                 */
  QSZ  *pqszStrings;            /* Array of strings to print        */
  BOOL fReturnValue;            /* Stores return value from GetInfo */
  PWND pwndInfo = NULL;         /* Info Window's PWND               */


  /* Determine the memory required to store the record */
  wStructSize = GetInfoSize (wRecordType, 0);

  if (wStructSize == 0)
    {
#if HEAP_DEBUG
      CHAR chBuff1[80];   /* String for error message */

      sprintf (chBuff1, "%s, line %d\n", __FILE__, __LINE__);

      ShowError (MB_BEEP | MB_OK | 0x8000, "Invalid Record Type", "", "");
#endif
      return (NULL);
    }

  /* Allocate memory for the structure */
  pStructForInfo = malloc (wStructSize);
#if HEAP_DEBUG
  HeapCheck ("In ShowInfo, After malloc (wStructSize)");
#endif

  if (pStructForInfo == NULL)
    {
      OutOfMemory();
      return (NULL);
    }

  /* Zero out the structure */
  memset (pStructForInfo, '\0', wStructSize);

  /* If /I was used, set the flag stating that this information */
  /*   is now available for reporting.                          */
  rgfReportItemFlag[wRecordType] = TRUE;
  rgfReportItemFlag[IDI_SUMMARY_SCREEN] = TRUE;

  /* Fill it with the GetInfo data */
  fReturnValue = GetInfo (wRecordType,
                          pStructForInfo,
                          FALSE,    /* Do not obtain minimum info */
                          FALSE,    /* No include header record   */
                          TRUE);    /* We are doing a report      */

#if HEAP_DEBUG
  HeapCheck ("In ShowInfo, After GetInfo()");
#endif

  if (fReturnValue)
    {
      free (pStructForInfo);
      return (NULL);
    }

  /* Get the information into displayable strings */
  pqszStrings = SprintInfo (wRecordType,
                            pStructForInfo,
                            NULL,
                            FALSE);  /* We are not doing a report */
#if HEAP_DEBUG
  HeapCheck ("In ShowInfo, After SprintInfo()");
#endif

  /* Update the summary strings */
  SprintInfo (wRecordType, pStructForInfo,
              &(pSum->szSumStrings[wRecordType * 2]), TRUE);

#if HEAP_DEBUG
  HeapCheck ("In ShowInfo, After update of summary strings");
#endif

  /* PostMessage (pwndMainFrame, WM_PAINT, NULL, NULL); */

  /* Display the strings in the window */
  if (pqszStrings != NULL)
    pwndInfo = CreateInfoWnd (paszButtonNames[wRecordType],
                              pqszStrings,
                              FALSE);

#if HEAP_DEBUG
  HeapCheck ("In ShowInfo, After CreateInfoWnd()");
#endif

  free (pStructForInfo);

#if HEAP_DEBUG
  HeapCheck ("In ShowInfo, After free (pStructForInfo)");
#endif

  return (pwndInfo);
}


BOOL fVisibleFlag;            /* Set if scroll bar should be visible    */

/*********************************************************************
 * CreateInfoWnd - Procedure to create the info provider window.
 *
 * pwndParent     - Parent window for this window.
 * pszWindowTitle - Title for the info window.
 * pqszStrings    - Strings to be displayed in the info window.
 * fKeepFocus     - TRUE if this window should keep the focus.
 *
 * Returns:  PWND if creating Info Window was successful, NULL if an
 *           error occured.
 *********************************************************************/

STATIC PWND CreateInfoWnd (PSZ  pszWindowTitle,
                           QSZ  *pqszStrings,
                           BOOL fKeepFocus)
{
  PWND pwndInfoFrame = NULL;  /* Handle to the info window's frame      */
  PWND pwndInfoTxt = NULL;    /* Text window handle                     */
  PWND pwndScrollBar = NULL;  /* Handle to the scroll bar               */
  WORD wNmbrLines;            /* Number of lines in this string array   */
  WORD wLongestLine = 0;      /* The length of the longest line         */
  WORD wLength;               /* The length of the current line         */
  AX   axRight;               /* Coordinates for the upper left corner  */
  AY   ayTop;                 /*   of the info window.                  */
  AX   axWidth;               /* Size of the info window                */
  AY   ayHeight;
  PSZ  pszNewWindowTitle;     /* New location (malloc'ed) for the title */

  /* Determine the number of lines and the length of the longest line */

  for (wNmbrLines = 0; pqszStrings[wNmbrLines] != NULL; ++wNmbrLines)
    {
      if ((wLength = DisplayLen (pqszStrings[wNmbrLines])) > wLongestLine)
        wLongestLine = wLength;
    }

  /* Calculate the size of the window */

  if (wNmbrLines > (WORD) ayMac - 7)
    {
      ayHeight = ayMac - (AY) 2;
      axWidth  = (wLongestLine + 5> (WORD) axMac - 6) ?
                 axMac - (AX) 2 : (AX) wLongestLine + (AX) 5;
      fVisibleFlag = TRUE;
    }
  else
    {
      ayHeight = (AY) wNmbrLines + (AY) 5;
      axWidth  = (wLongestLine + 4> (WORD) axMac - 6) ?
                 axMac - (AX) 2 : (AX) wLongestLine + (AX) 4;
      fVisibleFlag = FALSE;
    }


  /* Calculate the coordinates of the info window */

  axRight = (AX) ((axMac / 2) - (axWidth / 2));
  ayTop   = (AY) ((ayMac / 2) - (ayHeight / 2));

  /* Copy the window title */

  pszNewWindowTitle = malloc (strlen (pszWindowTitle) + 1);
  if (pszNewWindowTitle == NULL)
    return (NULL);
  strcpy (pszNewWindowTitle, pszWindowTitle);


  /* Create the window */

  pwndInfoFrame = (PWND) CreateWindow (InfoWndCallID,
                                       NULL,
                                       WS_CLIPOUT | WS_BORDER |
                                         WS_OVERLAP /* | WS_VSCROLL */ ,
                                       WS_FRAME | WES_SHADOW |
                                         WES_OWNERREDRAW,
                                       axRight,
                                       ayTop,
                                       axWidth,
                                       ayHeight,
                                       (HWND) pwndMainFrame,
                                       (HMENU) NULL,
                                       NULL,
                                       1);

  if (pwndInfoFrame == NULL)
    return (NULL);

  /* Get the Info Text window's window handle */
  pwndInfoTxt = GetDlgItem (pwndInfoFrame, InfoTxtID);

  /* Set the pointer to the button's name */
  SetWindowWord (pwndInfoFrame, WEB_WINDOW_TITLE, (WORD) pszNewWindowTitle);

  /* Set the scroll bar's minimum value in the extra bytes */
  SetWindowWord (pwndInfoTxt, WEB_MIN_SCROLL, 0);

  /* Set the scroll bar's maximum value in the extra bytes */
  SetWindowWord (pwndInfoTxt, WEB_MAX_SCROLL, wNmbrLines - (ayHeight - 5));

  /* Set the total number of lines in this string array */
  SetWindowWord (pwndInfoTxt, WEB_NMBR_LINES, wNmbrLines);

  /* Set the number of scrollable lines in the window */
  SetWindowWord (pwndInfoTxt, WEB_SCROLLABLE_LINES, (WORD) ayHeight - 5);

  /* Set the pointer to the text to display */
  SetWindowWord (pwndInfoTxt, WEB_PQSZ_TEXT, (WORD) pqszStrings);

  /* Set the flag on wether this window should keep the focus or not */
  SetWindowWord (pwndInfoTxt, WEB_KEEP_FOCUS, (WORD) fKeepFocus);

  /* Set the ranges for the scroll bar */
  pwndScrollBar = GetDlgItem (pwndInfoTxt, InfoScrollID);

  SetScrollRange (pwndScrollBar,                /* Handle to scroll bar */
                  0,                            /* Minimum scroll value */
                  wNmbrLines - (ayHeight - 5),  /* Maximum scroll value */
                  FALSE);                       /* Redraws when TRUE */

  SetScrollPos (pwndScrollBar,     /* Handle to scroll bar */
                0,                 /* New scroll bar position */
                FALSE);            /* Redraws when TRUE */

  EnableWindow (pwndInfoFrame, TRUE);

  ShowWindow (pwndInfoFrame, SW_VISIBLE, TRUE);

  /* Hide the scroll bar if it is not necessary */

  if (fVisibleFlag == FALSE)
    ShowWindow (pwndScrollBar, SW_INVISIBLE, TRUE);

  /* Set the focus to the OK button */
  SetFocus (pwndInfoFrame);

  return (pwndInfoFrame);
}

/*********************************************************************
 * InfoWndProc - Procedure for handling the "Information Provider"
 *               window.
 *********************************************************************/

STATIC LONG FAR PASCAL InfoWndProc (PWND  pwnd,
                                    WORD  message,
                                    WORD  wParam,
                                    DWORD lParam)
{
  switch (message)
    {
      case WM_CREATE:
        CreateInfoChildWindows (pwnd);
        break;

#if 0
      case WM_ACTIVATE:
        {
          PWND pwndInfoTxt = NULL;   /* Handle to the text window */

          /* Get the window handles */
          pwndInfoTxt   = GetDlgItem (pwnd, InfoTxtWndCallID);

          if (GetWindowWord (pwndInfoTxt, WEB_KEEP_FOCUS))
            return (rspActiveDecline);
          else
            return (rspActiveLive);
          break;
        }
#endif

      case WM_COMMAND:

        /* Close window when OK button clicked */

        if (HIWORD (lParam) == BN_CLICKED && wParam == OkButtonID)
          PostMessage (pwnd, WM_CLOSE, NULL, NULL);
        break;

      case WM_CLOSE:
        {
#if HEAP_DEBUG
          _heapset ('W');
#endif

          /* Free up the window title */
          free ((PSZ) GetWindowWord (pwnd, WEB_WINDOW_TITLE));

#if HEAP_DEBUG
          _heapset ('X');
#endif

          /* Inform the summary window that we are shutting down */
          SendMessage (pwnd->pwndParent, WM_INFO_WND_CLOSED, (WORD) pwnd,
                       NULL);

          /* Destroy this info window */
          DestroyWindow (pwnd);
          break;
        }

      case WM_FRAMEDRAW:
        {
          RRC rrc;  /* Relative rectangle of window */

          GetClientRrc (pwnd, &rrc);

          DrawBorderAlign (pwnd,
                           pboxInfoBox,
                           isaInfoActiveBorder,
                           isaInfoActiveBorder,
                           (PSZ) GetWindowWord (pwnd, WEB_WINDOW_TITLE),
                           TRUE);

          if (FIsTopWindow(pwnd))
            DrawOverlapShadow (pwnd);

          CharOutBorder (pwnd,
                         0,
                         (RY) (rrc.ryBottom - 2),
                         'Ã',
                         isaInfoActiveBorder);

          CharOutBorder (pwnd,
                         (RY) (rrc.rxRight + 1),
                         (RY) (rrc.ryBottom - 2),
                         '´',
                         isaInfoActiveBorder);

          break;
        }

      case WM_CHAR:
        {
          PWND pwndInfoTxt = NULL;   /* Handle to the text window          */
          PWND pwndScrollBar = NULL; /* Handle to text window's scroll bar */
          WORD wKeyState;            /* Shift key state when key pressed   */


          /* Get the window handles */

          pwndInfoTxt   = GetDlgItem (pwnd, InfoTxtWndCallID);
          pwndScrollBar = GetDlgItem (pwndInfoTxt, InfoScrollID);


          /* Handle the main window's keys */
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


          /* Handle the info window specific keys */

          switch (wParam)
            {
              case VK_ESCAPE:
              case 27:
                PostMessage (pwnd, WM_CLOSE, NULL, NULL);
                break;

              /* VK_RETURN will have to be modified to do the button */

              case VK_RETURN:
              case 13:
                PostMessage (pwnd, WM_CLOSE, NULL, NULL);
                break;

              case VK_HOME:
                SendMessage (pwndInfoTxt, WM_VSCROLL, SB_TOP,
                             ((DWORD) pwndScrollBar) << 16);
                break;

              case VK_END:
                SendMessage (pwndInfoTxt, WM_VSCROLL, SB_BOTTOM,
                             ((DWORD) pwndScrollBar) << 16);
                break;

              case VK_PRIOR:
                SendMessage (pwndInfoTxt, WM_VSCROLL, SB_PAGEUP,
                             ((DWORD) pwndScrollBar) << 16);
                break;

              case VK_NEXT:
                SendMessage (pwndInfoTxt, WM_VSCROLL, SB_PAGEDOWN,
                             ((DWORD) pwndScrollBar) << 16);
                break;

              case VK_UP:
                SendMessage (pwndInfoTxt, WM_VSCROLL, SB_LINEUP,
                             ((DWORD) pwndScrollBar) << 16);
                break;

              case VK_DOWN:
                SendMessage (pwndInfoTxt, WM_VSCROLL, SB_LINEDOWN,
                             ((DWORD) pwndScrollBar) << 16);
                break;

            } /* End switch (wParam) */

          break;
        } /* End case WM_CHAR */

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_PAINT:
        {
          NPRRC prrc;   /* Pointer to rectangle    */
          RRC rrc;      /* Entire client area      */
          RX rxX;
          RY ryY;

          /* Get the entire client area */

          GetClientRrc (pwnd, &rrc);

          /* Clear the window */

          if ((prrc = (NPRRC) LOWORD (lParam)) == NULL)
            {
              prrc = &rrc;
            }

          FillRrc (pwnd, prrc, ' ', isaInfoActive);

          /* Draw the separator line */

          for (rxX = 0, ryY = rrc.ryBottom - (RY) 3; rxX < rrc.rxRight; ++rxX)
            CharOut (pwnd, rxX, ryY, 'Ä', isaInfoActiveBorder);

          break;
        }

      default:
       return (DefWindowProc (pwnd, message, wParam, lParam));
    }

  return (0L);
}

/*********************************************************************
 * CreateInfoChildWindows - Adds the child windows to the main
 *                          "Information Provider" window
 *********************************************************************/

STATIC BOOL PASCAL CreateInfoChildWindows (PWND pwndParent)
{
  PWND pwnd = NULL;     /* Window pointer for examining window status */
  AX axLeft;            /* Left edge of window */
  AY ayTop;             /* Top edge of window  */
  WORD wWidth, wHeight; /* Width and height of window */


  /* Set the text window's coordinates */

  axLeft  = pwndParent->arcWindow.axLeft + (AX) 1;
  ayTop   = pwndParent->arcWindow.ayTop + (AX) 1;
  wHeight = pwndParent->arcWindow.ayBottom -
            pwndParent->arcWindow.ayTop - 5;
  if (fVisibleFlag == TRUE)
    wWidth  = pwndParent->arcWindow.axRight -
              pwndParent->arcWindow.axLeft - 2;
  else
    wWidth  = pwndParent->arcWindow.axRight -
              pwndParent->arcWindow.axLeft - 1;

  /**************************/
  /* Create the text window */
  /**************************/

  pwnd = (PWND) CreateWindow (InfoTxtWndCallID,
                              NULL,
                              WS_CLIPOUT,
                              WS_VISIBLE,
                              axLeft,
                              ayTop,
                              wWidth,
                              wHeight,
                              (HWND) pwndParent,
                              (HMENU) NULL,
                              NULL,
                              InfoTxtID);

  if (pwnd == NULL)
    {
      return (TRUE);
    }


  /************************/
  /* Create the OK button */
  /************************/

  axLeft  = ((pwndParent->arcWindow.axRight -
            pwndParent->arcWindow.axLeft) / (AX) 2) - (AX) (8 / 2) +
            pwndParent->arcWindow.axLeft;
  ayTop   = pwndParent->arcWindow.ayBottom - (AY) 3;
  wWidth  = 9;
  wHeight = 2;

  pwnd = (PWND) CreateWindow (WC_BUTTON,
                              "OK",
                              BS_DEFPUSHBUTTON | BS_OWNERDRAW | WS_CLIPOUT,
                              WES_3D | WES_TABSTOP,
                              axLeft,
                              ayTop,
                              wWidth,
                              wHeight,
                              (HWND) pwndParent,
                              (HMENU) NULL,
                              NULL,
                              OkButtonID);

  if (pwnd == NULL)
    {
      return (TRUE);
    }

  return (FALSE);
}

/*********************************************************************
 * InfoTxtWndProc - Procedure for handling the "Information Provider"
 *                  window.
 *********************************************************************/

STATIC LONG FAR PASCAL InfoTxtWndProc (PWND  pwnd,
                                       WORD  message,
                                       WORD  wParam,
                                       DWORD lParam)
{
  switch (message)
    {
      case WM_CREATE:
        CreateInfoTxtChildren (pwnd);
        break;

      case WM_DESTROY:
        FreeStringSpace ((QSZ *) GetWindowWord (pwnd, WEB_PQSZ_TEXT));
        break;

      case WM_VSCROLL:
        InfoTxtWndScroll (pwnd, wParam, lParam);
        break;

      case WM_PAINT:
        {
          PWND pwndScrollBar = NULL; /* Handle to the text window's scroll bar */
          RRC  rrc;           /* Entire client area                     */
          QSZ  *pqszStrings;  /* Strings to display                     */
          WORD i;             /* Looping variable                       */
          WORD u;             /* Index to pqszStrings[i]                */
          WORD wTopLine;      /* Top line of text to display            */
          WORD wLimitLine;    /* Bottom line of text to display         */
          WORD wLength;       /* String length                          */
          RY   ryCurrentLine; /* Current display line on window         */
          RX   rxCurrentPos;  /* Current position on window             */
          ISA  isaColor = isaInfoActive;  /* Current color              */


          /* Get the scroll bar's PWND */
          pwndScrollBar = GetDlgItem (pwnd, InfoScrollID);

          /* Get the entire client area */
          GetClientRrc (pwnd, &rrc);

          /* Get the pointer to the strings */
          pqszStrings = (QSZ *) GetWindowWord (pwnd, WEB_PQSZ_TEXT);

          /* Set the top line and limit line for the text */
          wTopLine   = GetScrollPos (pwndScrollBar);
          wLimitLine = wTopLine + rrc.ryBottom;

          if (wLimitLine > GetWindowWord (pwnd, WEB_NMBR_LINES) + 1)
            wLimitLine = GetWindowWord (pwnd, WEB_NMBR_LINES) + 1;

          /* Display the text */
          for (i = wTopLine, ryCurrentLine = 0; i < wLimitLine;
               ++i, ++ryCurrentLine)
            {
              wLength = Qstrlen (pqszStrings[i]);

              CharOut (pwnd, 0, ryCurrentLine, ' ', isaInfoActive);

              for (u = 0, rxCurrentPos = 1; pqszStrings[i][u] != '\0';
                   ++u, ++rxCurrentPos)
                {
                  /* Is this a control character */
                  if (pqszStrings[i][u] == '&')
                    {
                      /* && == Display '&' */
                      if (pqszStrings[i][u + 1] == '&')
                        ++u;

                      /* &# == Alternate color */
                      if (pqszStrings[i][u + 1] >= '1' &&
                          pqszStrings[i][u + 1] <= '3')
                        {
                          isaColor = isaAlternate;
                          u += 2;
                        }

                      /* &0 == Normal color */
                      if (pqszStrings[i][u + 1] == '0')
                        {
                          isaColor = isaInfoActive;
                          u += 2;
                        }

                      /* Now check to see if we are at the */
                      /*   end of the string               */
                      if (pqszStrings[i][u] == '\0')
                        break;
                    }

                  /* Display the character */
                  CharOut (pwnd, rxCurrentPos, ryCurrentLine,
                           pqszStrings[i][u], isaColor);
                }

              /* rxCurrentPos = rrc.rxLeft + (RX) wLength + (RX) 1; */

              while (rxCurrentPos < rrc.rxRight - (RX) 1)
                CharOut (pwnd, rxCurrentPos++, ryCurrentLine, ' ',
                         isaInfoActive);
            }

          /* Erase the lines below the last line, if necessary */
          if (ryCurrentLine < rrc.ryBottom)
            {
              rrc.ryTop = ryCurrentLine;
              --rrc.rxRight;
              FillRrc (pwnd, &rrc, ' ', isaInfoActive);
            }

          break;
        }

      default:
       return (DefWindowProc (pwnd, message, wParam, lParam));
    }

  return (0L);
}

/*********************************************************************
 * InfoTxtWndScroll - Handles scroll bar messages.
 *********************************************************************/

STATIC VOID InfoTxtWndScroll (PWND pwnd, WORD wParam, DWORD lParam)
{
  STATIC INT iScrollPos;    /* Position of the scroll bar */
  BOOL fRedraw = FALSE;     /* Set to TRUE if a redraw required */


  /* Get the current position */
  iScrollPos = GetScrollPos ((PWND) HIWORD (lParam));


  /* Handle requested scroll action */
  switch (wParam)
    {
      case SB_LINEUP:
        {
          /* Adjust the scroll bar's position */

          if (--iScrollPos >= (INT) GetWindowWord (pwnd, WEB_MIN_SCROLL))
            fRedraw = TRUE;
          else
            iScrollPos = GetWindowWord (pwnd, WEB_MIN_SCROLL);

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_LINEDOWN:
        {
          /* Adjust the scroll bar's position */

          if (++iScrollPos <= (INT) GetWindowWord (pwnd, WEB_MAX_SCROLL))
            fRedraw = TRUE;
          else
            iScrollPos = GetWindowWord (pwnd, WEB_MAX_SCROLL);

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_PAGEUP:
        {
          /* Set the flag for redraw */

          fRedraw = TRUE;

          /* Adjust the scroll bar's position */

          iScrollPos = iScrollPos -
                       GetWindowWord (pwnd, WEB_SCROLLABLE_LINES) + 1;

          if (iScrollPos < (INT) GetWindowWord (pwnd, WEB_MIN_SCROLL))
            iScrollPos = GetWindowWord (pwnd, WEB_MIN_SCROLL);

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_PAGEDOWN:
        {
          /* Set the flag for redraw */

          fRedraw = TRUE;

          /* Adjust the scroll bar's position */

          iScrollPos = iScrollPos +
                       GetWindowWord (pwnd, WEB_SCROLLABLE_LINES) - 1;

          if (iScrollPos > (INT) GetWindowWord (pwnd, WEB_MAX_SCROLL))
            iScrollPos = GetWindowWord (pwnd, WEB_MAX_SCROLL);

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_THUMBPOSITION:
        {
          /* Get the current position */

          iScrollPos = LOWORD (lParam);

          /* Turn on the fRedraw flag */

          fRedraw = TRUE;

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_THUMBTRACK:
        {
          /* Get the current position */

          iScrollPos = LOWORD (lParam);

          /* Turn on the fRedraw flag */

          fRedraw = TRUE;

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_ENDSCROLL:
        {
          /* Get the current position */

          iScrollPos = LOWORD (lParam);

          /* Turn on the fRedraw flag */

          fRedraw = TRUE;

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_TOP:
        {
          /* Get the current position */

          iScrollPos = GetWindowWord (pwnd, WEB_MIN_SCROLL);

          /* Turn on the fRedraw flag */

          fRedraw = TRUE;

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }

      case SB_BOTTOM:
        {
          /* Get the current position */

          iScrollPos = GetWindowWord (pwnd, WEB_MAX_SCROLL);

          /* Turn on the fRedraw flag */

          fRedraw = TRUE;

          /* Set the new position for the scroll bar */

          SetScrollPos ((PWND) HIWORD (lParam), iScrollPos, fRedraw);

          break;
        }
    } /* End "switch (wParam)" for WM_VSCROLL */

  /* Display the text in it's new location */

  PostMessage (pwnd, WM_PAINT, NULL, NULL);
}

/*********************************************************************
 * CreateInfoTxtChildren - Creates the scrolling text window
 *********************************************************************/

STATIC BOOL PASCAL CreateInfoTxtChildren (PWND pwndParent)
{
  PWND pwnd = NULL;     /* Window pointer for examining window status */
  AX axLeft;            /* Left edge of window */
  AY ayTop;             /* Top edge of window  */
  WORD wWidth, wHeight; /* Width and height of window */


  /********************************/
  /* Create the scroll bar window */
  /********************************/

  axLeft  = pwndParent->arcWindow.axRight - (AX) 1;
  ayTop   = pwndParent->arcWindow.ayTop;
  wWidth  = 1;
  wHeight = pwndParent->arcWindow.ayBottom -
            pwndParent->arcWindow.ayTop;

  if (wHeight < 2)
    wHeight = 2;

  pwnd = (PWND) CreateWindow (WC_SCROLLBAR,
                              NULL,
                              WS_CHILD | SBS_VERT | WS_CLIPOUT,
                              WS_VISIBLE,
                              axLeft,
                              ayTop,
                              wWidth,
                              wHeight,
                              (HWND) pwndParent,
                              (HMENU) NULL,
                              NULL,
                              InfoScrollID);

  if (pwnd == NULL)
    {
      return (TRUE);
    }
}


/*********************************************************************
 * DrawPlateButton1 - Draws a "plate" button that is 1 character high
 *                    with a half character shadow.
 *********************************************************************/

VOID DrawPlateButton1 (PWND  pwnd,
                       WORD  message,
                       WORD  wParam,
                       DWORD lParam)
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
  --(rrcButton.ryBottom);

  /* Find the centering location */
  wCenterLoc = ((rrcButton.rxRight - 1 - rrcButton.rxLeft) / 2) -
               (strlen (pwndbtn->szDialog) / 2);

  /* Determine if the button is pushed in or not */
  dwButtonState = SendMessage (fpdis->hwndItem, BM_GETSTATE, 0, 0L);
  if (dwButtonState & 0x0004)
    {
      /* The button is pressed in */

      /* Repaint the shadow area */
      FillRrc (fpdis->hwndItem, &rrcShadow, ' ', isaInfoActive);

      CharOut (fpdis->hwndItem, 0, 0, ' ', isaInfoActive);


      /* Repaint the button face */
      ++(rrcButton.rxLeft);

      FillRrc (fpdis->hwndItem, &rrcButton, ' ', isaPushButton);

      TextOut (fpdis->hwndItem, (RY) (wCenterLoc + 1), 0,
               pwndbtn->szDialog, -1, isaPushButton);
    }
  else
    {
      /* The button is out */

      /* Repaint the button face */
      --(rrcButton.rxRight);

      FillRrc (fpdis->hwndItem, &rrcButton, ' ', isaPushButton);

      TextOut (fpdis->hwndItem, (RY) (wCenterLoc), 0,
               pwndbtn->szDialog, -1, isaPushButton);


      /* Repaint the shadow area */
      CharOut (fpdis->hwndItem, rrcShadow.rxLeft, rrcShadow.ryTop,
               ' ', isaInfoActive);

      ++(rrcShadow.rxLeft);

      FillRrc (fpdis->hwndItem, &rrcShadow, 'ß', isaInfoActive);

      CharOut (fpdis->hwndItem, rrcButton.rxRight, 0, 'Ü',
               isaInfoActive);
    }
}

#endif /* CW_INCLUDED */
