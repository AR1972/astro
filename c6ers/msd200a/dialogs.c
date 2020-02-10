/*********************************************************************
 * Microsoft Diagnostics Version 2.0
 *
 * A diagnostic utility to detect as much useful information about a
 *   customer's computer system as is possible.
 *
 * Microsoft Diagnostics:  We detect the World.
 *
 * DIALOGS.C - Source file for handling the various dialog boxes
 ********************************************************************/


/* Include Files */

#include "msd.h"


#ifdef CW_INCLUDED


// An example of how to call ADM to process your dialog.

//VOID CallReportsDialog()
//{
//  WORD wReturn;

  // Call with the address of the dialog template ( in dialog.adm ) and
  // the address of the app's dialog proc.
  // wReturn will contain the value passed to EndDialog () within the apps
  // dialog proc.

//  wReturn = DialogBox ( &dlgReport, ReportDlg );
//}


/* Global to this file */
char EditBuf[256];
char EditBuf2[256];
char EditBuf3[256];


/*********************************************************************
 * ReportDlg - Procedure to handle the Print Report dialog box.
 *********************************************************************/

LONG FAR PASCAL ReportDlg (PWND pwnd,
                           WORD message,
                           WORD wParam,
                           DWORD lParam)
{
  WORD i;                      /* Looping variable              */


  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          // It is the repsonsibility of the application to supply a buffer
          //  for the edit control. In the example below, EditBuf is the
          //  buffer declared in the source, somewhere. ( Don't make it a local
          //  variable in this proc!!! ). lParam has two values set, both equal
          //  to 255. These values are the size of the buffer. If your buffer
          //  is only 80 characters wide, then use MAKELONG(80,80).
          //
          SendMessage (GetDlgItem (pwnd, IDD_USER + 33), EM_SETBUF,
                       (WORD) EditBuf, MAKELONG (255, 255));


          /* Set the default edit text */
          SetEditText (GetDlgItem (pwnd, IDD_USER + 33), "REPORT.MSD", TRUE);

          // We should set a default value for the group of radio buttons
          // which indicate where we 'Print To'. Like under Windows,
          // The first parameter is the Dialog PWND, 2nd and third are the id's
          // of the first and last radio button in the group. The fourth paramter
          // is the ID of the radio button that is initally 'on'.

          /* Set the default to what it was when we left (wReportToIndex) */
          CheckDlgRadioButton (pwnd, IDD_USER + 25, IDD_USER + 32,
                               wReportToIndex + IDD_USER + 25);

          /* Set the default items to report */
          for (i = 2; i < MAX_REPORT_ITEM_FLAGS; ++i)
            if (rgfReportItemFlag[i] == TRUE)
              {
                // CheckDlgButton (GetDlgItem (pwnd, IDD_USER + i),
                //                 bstOn, TRUE);

                SendMessage (GetDlgItem (pwnd, IDD_USER + i),
                                    BM_SETCHECK, TRUE, 0L);
                DrawWindow ((PWND_BTN) GetDlgItem (pwnd, IDD_USER + i));
              }

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        // For the 'Print To' radio group, we have to assume responsability for
        // checking/unchecking the buttons.
        if ( wParam >= IDD_USER + 25 &&
             wParam <= IDD_USER + 32 )
          {
            CheckDlgRadioButton (pwnd, IDD_USER + 25, IDD_USER + 32, wParam);

            /* Set the report filename to the correct item */
            wReportToIndex = wParam - (IDD_USER + 25);

            /* If this is the "File" radio button, set the focus */
            /*   to the edit box.                                */

            if (wParam == IDD_USER + 32)
              SetFocus (GetDlgItem (pwnd, IDD_USER + 33));

            return (TRUE);
          }

        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              {
                /* Set the filename for the report */
                if (wReportToIndex == 7)  /* File: */
                  {
                    CHAR chBuffer[256];  /* Holds the edit item's text */

                    GetEditText (GetDlgItem (pwnd, IDD_USER + 33),
                                 chBuffer, 255);

                    pszReportFilename = malloc (strlen (chBuffer) + 1);
                    if (pszReportFilename == NULL)
                      {
                        /* If there's insufficient memory, cancel */
                        //  EndDialog (pwnd, wParam);
                        ((PWND_DLG)pwnd)->wParamEnd = IDCANCEL;
                        break;
                      }

                    strcpy (pszReportFilename, chBuffer);
                  }

                /* Set the default items to report */
                for (i = 1; i < MAX_REPORT_ITEM_FLAGS; ++i)
                  {
                    if (SendMessage (GetDlgItem (pwnd, IDD_USER + i),
                                     BM_GETCHECK, 0, 0L))
                      rgfReportItemFlag[i] = TRUE;
                    else
                      rgfReportItemFlag[i] = FALSE;
                  }

                //  EndDialog (pwnd, wParam);
                ((PWND_DLG)pwnd)->wParamEnd = wParam;
                break;
              }

            case IDCANCEL:
              //  EndDialog (pwnd, wParam);
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;

            // The 'Report All' checkbox was clicked on. If it is now 'on', we
            // want to turn on all the other checkbox's. That is what this little
            // piece of code is doing.

            case IDD_USER + 1:
              if (SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                               BM_GETCHECK, 0, 0L))
                {
                  i = IDD_USER + 2;

                  for (i = IDD_USER + 2; i <= IDD_USER + 23; ++i)
                    {
                      // CheckDlgButton (GetDlgItem (pwnd, i), bstOn, TRUE);

                      SendMessage (GetDlgItem (pwnd, i),
                                   BM_SETCHECK, TRUE, 0L);
                      DrawWindow ((PWND_BTN) GetDlgItem (pwnd, i));
                    }
                }
              break;

            case IDD_USER +  2:
            case IDD_USER +  3:
            case IDD_USER +  4:
            case IDD_USER +  5:
            case IDD_USER +  6:
            case IDD_USER +  7:
            case IDD_USER +  8:
            case IDD_USER +  9:
            case IDD_USER + 10:
            case IDD_USER + 11:
            case IDD_USER + 12:
            case IDD_USER + 13:
            case IDD_USER + 14:
            case IDD_USER + 15:
            case IDD_USER + 16:
            case IDD_USER + 17:
            case IDD_USER + 18:
            case IDD_USER + 19:
            case IDD_USER + 20:
            case IDD_USER + 21:
            case IDD_USER + 22:
            case IDD_USER + 23:
              // CheckDlgButton (GetDlgItem (pwnd, IDD_USER + 1),
              //                 bstOff, TRUE);

              SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                           BM_SETCHECK, FALSE, 0L);
              DrawWindow ((PWND_BTN) GetDlgItem (pwnd, IDD_USER + 1));

              break;
          }

        break;

      case MY_EN_SETFOCUS:
        SendMessage (pwnd, WM_COMMAND, IDD_USER + 32, 0L);
        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


BOOL fDlgSearchFlags;

/*********************************************************************
 * FindFileDlg1 - Procedure to handle the Print Report dialog box.
 *********************************************************************/

LONG FAR PASCAL FindFileDlg1 (PWND pwnd,
                              WORD message,
                              WORD wParam,
                              DWORD lParam)
{
  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          CHAR chPath[_MAX_PATH];   /* Current path */


          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          /* Clear out EditBuf and EditBuf2 */
          EditBuf[0]  = '\0';
          EditBuf2[0] = '\0';

          SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                       EM_SETBUF, (WORD) EditBuf, MAKELONG(255, 255));

          SendMessage (GetDlgItem (pwnd, IDD_USER + 3),
                       EM_SETBUF, (WORD) EditBuf2, MAKELONG(255, 255));

          /* Put the current path into the "Search from" edit item */
          getcwd (chPath, _MAX_PATH - 1);
          SetEditText (GetDlgItem (pwnd, IDD_USER + 3), chPath, TRUE);

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            WORD wResult;
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              {
                if (strlen (EditBuf) == 0)
                  {
                    MessageBox ("A filespec must be entered",
                                "in the \"Search for\" field",
                                NULL, MB_OK | 0x8000);
                    break;
                  }

                /* Check to see if a path was entered in "Search for" */
                if (strchr (EditBuf, '\\') || strchr (EditBuf, ':'))
                  {
                    PSZ pszLast;  /* Last path character */
                    CHAR chBuffer1[256];  /* Holds the edit item's text */
                    CHAR chBuffer2[256];  /* Holds the edit item's text */

                    GetEditText (GetDlgItem (pwnd, IDD_USER + 1),
                                 chBuffer1, 255);

                    /* A path was found, make it the "search from" string */
                    chBuffer2[0] = '\0';
                    strcpy (chBuffer2, chBuffer1);
                    pszLast = max (strrchr (chBuffer2, '\\'),
                                   strrchr (chBuffer2, ':'));
                    if (pszLast[0] == ':')
                      ++pszLast;

                    pszLast[0] = '\0';


                    /* Now, adjust the "search for" string */
                    pszLast = max (strrchr (chBuffer1, '\\'),
                                   strrchr (chBuffer1, ':')) + 1;

                    memmove (chBuffer1, pszLast, strlen (pszLast) + 1);
                    SetEditText (GetDlgItem (pwnd, IDD_USER + 1),
                                 chBuffer1, TRUE);
                    SetEditText (GetDlgItem (pwnd, IDD_USER + 3),
                                 chBuffer2, TRUE);
                  }

                /* Set the flags for the search */
		fDlgSearchFlags = SEARCH_VERSION;

                if (SendMessage (GetDlgItem (pwnd, IDD_USER + 4),
                                 BM_GETCHECK, 0, 0L))
                  fDlgSearchFlags |= RECURSE_INTO_SUB_DIRS;
		/*
                if (SendMessage (GetDlgItem (pwnd, IDD_USER + 5),
                                 BM_GETCHECK, 0, 0L))
                  fDlgSearchFlags |= SEARCH_BOOT_DRIVE;
		*/
		if (SendMessage (GetDlgItem (pwnd, IDD_USER + 5),
                                 BM_GETCHECK, 0, 0L))
                  fDlgSearchFlags |= SEARCH_FLOPPIES | SEARCH_LOCAL_DRIVES |
				  SEARCH_NET_DRIVES | SEARCH_ROOT;


                if ((wResult = DialogBox (&dlgFindFile2, FindFileDlg2))
                     != IDCANCEL)
                  {
                    //  EndDialog(pwnd, wParam );
                    ((PWND_DLG)pwnd)->wParamEnd = wResult;
                  }

                break;
              }

            case IDCANCEL:
              {
                //  EndDialog(pwnd, wParam );
                ((PWND_DLG)pwnd)->wParamEnd = wParam;
                break;
              }
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * FindFileDlg2 - Procedure to handle the Print Report dialog box.
 *********************************************************************/

LONG FAR PASCAL FindFileDlg2 (PWND pwnd,
                              WORD message,
                              WORD wParam,
                              DWORD lParam)
{
  static FILE_INFO FAR * ffi  = NULL;   /* File Information structure */
  static FILE_INFO FAR * ffi2 = NULL;   /* File Information structure (copy) */


  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          BOOL fSearchFlags = 0;  /* Search flags for FindFile  */
          CHAR chBuffer[80];      /* String to hand to list box */
          CHAR sprintfBuffer[80]; /* Buffer for sprintf-ing     */


          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          /* Clear out the previous FileInfo structure */
          FreeFileInfo (ffi2);

          /* Inform the user that we are searching */
          DisplayStatus (ST_SEARCHING);


          /* Clear out the listbox */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 8),
                       LB_RESETCONTENT,
                       NULL,
                       NULL);


          /* Perform the search */
          if (EditBuf2[0] == '\0')
            ffi = FindFile (EditBuf, NULL, fDlgSearchFlags, '\0');
          else
            ffi = FindFile (EditBuf, EditBuf2, fDlgSearchFlags, '\0');

          /* Preserve the FindFile info */
          ffi2 = ffi;

          /* Redisplay the status line */
          PostMessage (pwndStatusLine, WM_PAINT, NULL, NULL);


          /* Add the filenames to the listbox */
          if (ffi == NULL || ffi->fpNextFileInfo == NULL)
            {
              MessageBox ("No matching files", NULL, NULL, MB_OK | 0x8000);
	      //  EndDialog(pwnd, wParam );

	      /* Display the previous status line text */
	      DisplayStatus (midFind);

              ((PWND_DLG)pwnd)->wParamEnd = IDCANCEL;
              break;
            }

          while (ffi != NULL && ffi->fpNextFileInfo != NULL)
            {
              DATE_INFO FAR *fpDate;  /* For printing the date   */
              WORD      wLength;      /* Length of sprintfBuffer */
              WORD      wYear;        /* Adjusted year           */


              /* Clear out chBuffer */
              memset (chBuffer, ' ', FIND_FILE_LINE_LENGTH);
              chBuffer[FIND_FILE_LINE_LENGTH] = '\0';


              /* Put the text date into chBuffer */
              fpDate = (DATE_INFO FAR *) &(ffi->wDate);

              /* Adjust date if >= year 2000 */
              wYear = 80 + fpDate->Year;
              wYear = (wYear >= 100) ? wYear - 100 : wYear;

              wLength = sprintf (sprintfBuffer, "%2d/%02d/%02d",
                                 fpDate->Month,
                                 fpDate->Day,
                                 wYear);

              strncpy (&chBuffer[DATE_COLUMN],
                       sprintfBuffer,
                       wLength);


              /* Put the file size into chBuffer */
              wLength = sprintf (sprintfBuffer, "%15lu",
                                 ffi->dwSize);

              strncpy (&chBuffer[SIZE_COLUMN],
                       sprintfBuffer,
                       wLength);


              if ((ffi->dwFileVersionMS | ffi->dwFileVersionLS) != 0)
                {
                  wLength = sprintf (sprintfBuffer,
                                     "%u.%u.%u.%u ",
                                     FP_SEG (ffi->dwFileVersionMS),
                                     FP_OFF (ffi->dwFileVersionMS),
                                     FP_SEG (ffi->dwFileVersionLS),
                                     FP_OFF (ffi->dwFileVersionLS));

                  strncpy (&chBuffer[SIZE_COLUMN + 5 - wLength],
                           sprintfBuffer,
                           wLength);
                }


              /* Put the filename into chBuffer */
              wLength = _fstrlen (ffi->fpszPathToFile);

              if (wLength > FIND_FILE_LINE_LENGTH - 2)
                wLength = FIND_FILE_LINE_LENGTH - 2;

              _fstrncpy (chBuffer, ffi->fpszPathToFile, wLength);

              chBuffer[wLength] = ' ';


              /* Add the item to the listbox */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 8),
                           LB_ADDSTRING,
                           ((WORD) (isaNil) << 8) + TRUE,
                           (DWORD) ((CHAR FAR *) (&chBuffer[0])));


              /* Point to the next File info structure */
              ffi = (FILE_INFO FAR *) (ffi->fpNextFileInfo);
            }

          if (SendMessage (GetDlgItem (pwnd, IDD_USER + 8),
                           LB_GETCOUNT, 0, 0L) > 0)
            {
              /* Highlight the first item in the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 8), LB_SETCURSEL, 0, 0L);
            }

          /* Inform the user that we are searching */
          DisplayStatus (ST_FINDFILE2);

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:           /* ENTER key    */
            case IDD_USER + 10:  /* Display File */
            case IDD_USER + 11:  /* File Info    */
              {
                INT   i;                    /* Looping variable     */


                /* Get the path */

                /* This gets the index of the item */
                i = (INT) SendMessage (GetDlgItem (pwnd, IDD_USER + 8),
                                       LB_GETCURSEL, 0, 0L);

                /* Find the i'th element of the list */
                ffi = ffi2;

                if (i != isaNil)
                  {
                    while (i-- && ffi != NULL && ffi->fpNextFileInfo != NULL)
                      {
                        /* Point to the next File info structure */
                        ffi = (FILE_INFO FAR *) (ffi->fpNextFileInfo);
                      }

                    if (++i != 0)
                      {
                        pqszBrowseStrings = NULL;
                        pszBrowseTitle = NULL;
                        wParam = IDCANCEL;
                      }
                    else
                      {
                        /* Put the path in a more permanent "home" */
                        _fstrcpy (EditBuf2, ffi->fpszPathToFile);

                        pszBrowseTitle = EditBuf2;
                      }
                  }
                else
                  {
                    pqszBrowseStrings = NULL;
                    pszBrowseTitle = NULL;
                    wParam = IDCANCEL;
                  }

                /* Clear out the previous FileInfo structure */
                FreeFileInfo (ffi2);

                /* Set them back to NULL */
                ffi  = NULL;
                ffi2 = NULL;

                //  EndDialog(pwnd, wParam );
                ((PWND_DLG)pwnd)->wParamEnd = wParam;
                break;
              }

            case IDCANCEL:
              {
                /* Clear out the previous FileInfo structure */
                FreeFileInfo (ffi2);

                /* Set them back to NULL */
                ffi  = NULL;
                ffi2 = NULL;

                /* Display the previous status line text */
                DisplayStatus (midFind);

                //  EndDialog(pwnd, wParam );
                ((PWND_DLG)pwnd)->wParamEnd = wParam;
                break;
              }
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * InsertCmdDlg1 - Procedure to handle the Insert Command dialog box.
 *********************************************************************/

LONG FAR PASCAL InsertCmdDlg1 (PWND pwnd,
                               WORD message,
                               WORD wParam,
                               DWORD lParam)
{
  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle (pwnd, pwnd->style | WS_BORDER);

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          /* Display the help string */
          DisplayStatus (ST_INSERT_DLG1);

          ReadCommands (GetDlgItem (pwnd, IDD_USER + 3));

          if (SendMessage (GetDlgItem (pwnd, IDD_USER + 3),
                           LB_GETCOUNT, 0, 0L) > 0)
            {
              /* Highlight the first item in the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 3), LB_SETCURSEL, 0, 0L);
            }

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              {
                static CHAR szCommand[256];    /* Stores the command string     */
                static CHAR szSection[256];    /* Stores the section to change  */
                static CHAR szFilename[256];   /* Stores the filename to change */
                CHAR szLbString[256];          /* Selected string stored here   */
                INT  i;                        /* Looping variable              */


                /* Get the string from the listbox */
                SendMessage (GetDlgItem (pwnd, IDD_USER + 3), LB_GETTEXT,
                             255, (DWORD) ((CHAR FAR *) szLbString));


                /* Extract the szCommand string */
                if (strlen (szLbString) < INI_SECTION_COL - 1)
                  i = strlen (szLbString);
                else
                  i = INI_SECTION_COL - 1;

                for (; i >= 0 && szLbString[i] == ' '; --i)
                  ;

                if (i > 0)
                  {
                    memcpy (szCommand, szLbString, ++i);
                    szCommand[i] = '\0';
                  }
                else
                  szCommand[0] = '\0';


                /* Extract the szSection string */
                if (strlen (szLbString) < INI_FILENAME_COL - 1)
                  i = strlen (szLbString);
                else
                  i = INI_FILENAME_COL - 1;

                for (; i >= INI_SECTION_COL && szLbString[i] == ' '; --i)
                  ;

                if (i > INI_SECTION_COL)
                  {
                    memcpy (szSection, &szLbString[INI_SECTION_COL],
                            ++i - INI_SECTION_COL);
                    szSection[i - INI_SECTION_COL] = '\0';
                  }
                else
                  szSection[0] = '\0';


                /* Extract the szFilename string */
                for (i = strlen (szLbString);
                     i >= INI_FILENAME_COL && szLbString[i] == ' '; --i)
                  ;

                if (i > INI_FILENAME_COL)
                  {
                    memcpy (szFilename, &szLbString[INI_FILENAME_COL],
                            ++i - INI_FILENAME_COL);
                    szFilename[i - INI_FILENAME_COL] = '\0';
                  }
                else
                  szFilename[0] = '\0';


                /* Bring up edit dialog box */
                pszInsertCommand  = szCommand;
                pszInsertSection  = szSection;
                pszInsertFilename = szFilename;
                if (DialogBox (&dlgInsertCmd2, InsertCmdDlg2) == IDOK)
                  ((PWND_DLG)pwnd)->wParamEnd = IDOK;
                else
                  DisplayStatus (ST_INSERT_DLG1);

                break;
              }

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * InsertCmdDlg2 - Procedure to handle the Insert Command editing
 *                 dialog box.
 *********************************************************************/

LONG FAR PASCAL InsertCmdDlg2 (PWND pwnd,
                               WORD message,
                               WORD wParam,
                               DWORD lParam)
{
  WORD wReturnValue;  /* Return value */


  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle (pwnd, pwnd->style | WS_BORDER);

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          /* Display the help string */
          DisplayStatus (ST_INSERT_DLG2);

          // It is the repsonsibility of the application to supply a buffer
          //  for the edit control. In the example below, EditBuf is the
          //  buffer declared in the source, somewhere. ( Don't make it a local
          //  variable in this proc!!! ). lParam has two values set, both equal
          //  to 255. These values are the size of the buffer. If your buffer
          //  is only 80 characters wide, then use MAKELONG(80,80).
          //
          SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                       EM_SETBUF, (WORD) EditBuf, MAKELONG(255, 255));
          SetEditText (GetDlgItem (pwnd, IDD_USER + 1), pszInsertCommand, TRUE);
          SendMessage (GetDlgItem (pwnd, IDD_USER + 3),
                       EM_SETBUF, (WORD) EditBuf2, MAKELONG(255, 255));
          SetEditText (GetDlgItem (pwnd, IDD_USER + 3), pszInsertSection, TRUE);
          SendMessage (GetDlgItem (pwnd, IDD_USER + 5),
                       EM_SETBUF, (WORD) EditBuf3, MAKELONG(255, 255));
          SetEditText (GetDlgItem (pwnd, IDD_USER + 5), pszInsertFilename, TRUE);
          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              /* Get the updated strings */
              GetEditText (GetDlgItem (pwnd, IDD_USER + 1),
                           pszInsertCommand, 255);
              GetEditText (GetDlgItem (pwnd, IDD_USER + 3),
                           pszInsertSection, 255);
              GetEditText (GetDlgItem (pwnd, IDD_USER + 5),
                           pszInsertFilename, 255);

              /* Check for blank fields */
              if (strlen (pszInsertCommand)  == 0)
                {
                  wReturnValue = MessageBox ("A command must be entered in the Command field",
                                              NULL, NULL, MB_OKCANCEL | 0x8000);

                  if (wReturnValue == IDCANCEL)
                    ((PWND_DLG)pwnd)->wParamEnd = wParam;
                }
              else if (strlen (pszInsertFilename) == 0)
                {
                  wReturnValue = MessageBox ("A filename must be entered in the File field",
                                              NULL, NULL, MB_OKCANCEL | 0x8000);

                  if (wReturnValue == IDCANCEL)
                    ((PWND_DLG)pwnd)->wParamEnd = IDCANCEL;
                }
              else
                ((PWND_DLG)pwnd)->wParamEnd = IDOK;

              break;

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = IDCANCEL;
              break;
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * InsertCmdDlg3 - Procedure to handle the Insert Command file choice
 *                 dialog box.
 *********************************************************************/

LONG FAR PASCAL InsertCmdDlg3 (PWND pwnd,
                               WORD message,
                               WORD wParam,
                               DWORD lParam)
{
  FILE_INFO FAR *pfi;   /* Local file info structure */


  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          DisplayStatus (ST_INSERT_DLG3);

          /* Clear out the listbox */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                       LB_RESETCONTENT, NULL, NULL);

          /* Load the list box with the list of matching files */
          pfi = pfiDlg;

          while (pfi != NULL && pfi->fpNextFileInfo != NULL)
            {
              SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                           LB_ADDSTRING,
                           ((WORD) (isaNil) << 8) + TRUE,
                           (DWORD) (pfi->fpszPathToFile));

              pfi = (FILE_INFO FAR *) pfi->fpNextFileInfo;
            }

          if (SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                           LB_GETCOUNT, 0, 0L) > 0)
            {
              /* Highlight the first item in the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 1), LB_SETCURSEL, 0, 0L);
            }

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              {
                /* Get the string from the listbox */
                SendMessage (GetDlgItem (pwnd, IDD_USER + 1), LB_GETTEXT,
                             255, (DWORD) ((CHAR FAR *) EditBuf));

                pszInsertFilename = EditBuf;

                // Fall through to IDCANCEL
              }

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * InsertCmdDlg4 - Procedure to handle the Insert Command line choice
 *                 dialog box.
 *********************************************************************/

LONG FAR PASCAL InsertCmdDlg4 (PWND pwnd,
                               WORD message,
                               WORD wParam,
                               DWORD lParam)
{
  static CHAR szSearchString[80]; /* String to search for in command file */
  INT  i1, i2;                    /* Looping variables                    */
  BOOL fReturnValue;              /* Return value from various functions  */
  WORD wIndex;                    /* Index to the list box                */
  static WORD wReplaceAllIndex;   /* Index to the "REPLACE ALL" line      */
  FILE *fpIn;                     /* File handles for reading input file  */
  static PWND pwndList;           /* List box's PWND                      */


  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          DisplayStatus (ST_INSERT_DLG4);

          /* Clear out the listbox */
          pwndList = GetDlgItem (pwnd, IDD_USER + 1);
          SendMessage (pwndList, LB_RESETCONTENT, 0, 0L);

          /* Add the "ADD LINE" string */
          SendMessage (pwndList, LB_ADDSTRING,
                       ((WORD) (isaNil) << 8) + TRUE,
                       (DWORD) ((CHAR FAR *) "ADD LINE"));


          /* Prepare search string by finding the primary part of the command. */
          /*   This is accomplished by searching for the first "=" character.  */
          /*   If found, extra whitespace behind the "=" sign is removed from  */
          /*   the search string.  If no "=" sign is found, search for the     */
          /*   first whitespace character, and call that the delimiter.        */
          /*                                                                   */
          /*   Examples:                                                       */
          /*             szCommand:                   szSearchString:          */
          /*             "EmmExclude = C000 - EFFF"   "EmmExclude"             */
          /*             "SET TEMP=?"                 "SET TEMP"               */
          /*             "PATH C:\;C:\DOS"            "PATH"                   */

          for (i1 = 0; pszInsertCommand[i1] != '\0' && 
                       pszInsertCommand[i1] != '=';  ++i1)
            ;

          /* Check for "=" sign */

          if (pszInsertCommand[i1] == '=')
            {
              /* Remove white space between command and the "=" sign */

              for (i2 = i1 - 1; i2 >= 0 &&
                                (pszInsertCommand[i2] == ' ' ||
                                 pszInsertCommand[i2] == '\t'); --i2)
                ;

              /* Put the information into the search string */

              for (i1 = 0; i1 <= i2; ++i1)
                szSearchString[i1] = pszInsertCommand[i1];

              szSearchString[i1] = '\0';
            }
          else
            {
              for (i1 = 0; pszInsertCommand[i1] != '\0' &&
                           pszInsertCommand[i1] != ' '  &&
                           pszInsertCommand[i1] != '\t';   ++i1)
                ;

              /* Put the information into the search string, up to, but not */
              /*   including, the white space character.                    */

              for (i2 = 0; i2 < i1; ++i2)
                szSearchString[i2] = pszInsertCommand[i2];

              szSearchString[i2] = '\0';
            }


          /* Open the file */
          fpIn = OpenFile (pszInsertFilename, "rb", TRUE);
          if (fpIn == NULL)
            {
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = IDOK;

              return (TRUE);
            }


          /* Locate the section in the file */
          if (pszInsertSection[0] != '\0')
            {
              fReturnValue = FindSection (pszInsertSection, fpIn, NULL);

              if (fReturnValue == TRUE)
                {
                  if (MessageBox (pszInsertSection,
                                  "Was not found.",
                                  "Add this to the file?",
                                  MB_YESNO | 0x8000) == IDNO)
                    {
                      CloseFile (fpIn);

                      //  EndDialog(pwnd, wParam );
                      ((PWND_DLG)pwnd)->wParamEnd = IDOK;

                      return (TRUE);
                    }
                }
            }
          else
            fReturnValue = FALSE;


          /* Load the list box with the list of matching lines */
          if (fReturnValue == FALSE)
            HandleDuplicates (HD_SEARCH,
                              szSearchString,
                              NULL,
                              fpIn,
                              NULL,
                              pwndList);


          wIndex = (WORD) SendMessage (pwndList, LB_GETCOUNT, 0, 0L);

          /* If there is more than 1 add and 1 replace line, */
          /*   add "REPLACE ALL".                            */
          if (wIndex > 2)
            {
              SendMessage (pwndList, LB_ADDSTRING,
                           ((WORD) (isaNil) << 8) + TRUE,
                           (DWORD) ((CHAR FAR *) "REPLACE ALL"));

              wReplaceAllIndex = wIndex;
            }
          else
            wReplaceAllIndex = 2;

          CloseFile (fpIn);

          /* Highlight the first item in the list box */
          SendMessage (pwndList, LB_SETCURSEL, 0, 0L);

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              {
                wIndex = (WORD) SendMessage (pwndList, LB_GETCURSEL, 0, 0L);

                if (wIndex == wReplaceAllIndex)
                  ChangeFile (pszInsertFilename,
                              pszInsertSection,
                              pszInsertCommand,
                              szSearchString,
                              (WORD) HD_REPLACE_ALL);
                else
                  ChangeFile (pszInsertFilename,
                              pszInsertSection,
                              pszInsertCommand,
                              szSearchString,
                              wIndex);

                // Fall through to IDCANCEL
              }

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * TestPrinterDlg - Procedure to handle the "Test Printer" dialog box.
 *********************************************************************/

LONG FAR PASCAL TestPrinterDlg (PWND pwnd,
                                WORD message,
                                WORD wParam,
                                DWORD lParam)
{
  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          // We should set a default value for the group of radio buttons
          // which indicate where we 'Print To'. Like under Windows,
          // The first parameter is the Dialog PWND, 2nd and third are the id's
          // of the first and last radio button in the group. The fourth paramter
          // is the ID of the radio button that is initally 'on'.

          CheckDlgRadioButton ( pwnd, IDD_USER + 1,
                         IDD_USER + 2,
                         IDD_USER + 1 );

          CheckDlgRadioButton ( pwnd, IDD_USER + 4,
                         IDD_USER + 5,
                         IDD_USER + 4 );

          CheckDlgRadioButton ( pwnd, IDD_USER + 7,
                         IDD_USER + 13,
                         IDD_USER + 7 );

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        // We have to assume responsability for checking/unchecking
        // radio buttons.

        if ( wParam >= IDD_USER + 1 &&
             wParam <= IDD_USER + 2 )
          {
            CheckDlgRadioButton (pwnd, IDD_USER + 1, IDD_USER + 2, wParam);
            return (TRUE);
          }

        if ( wParam >= IDD_USER + 4 &&
             wParam <= IDD_USER + 5 )
          {
            CheckDlgRadioButton (pwnd, IDD_USER + 4, IDD_USER + 5, wParam);
            return (TRUE);
          }

        if ( wParam >= IDD_USER + 7 &&
             wParam <= IDD_USER + 13 )
          {
            CheckDlgRadioButton (pwnd, IDD_USER + 7, IDD_USER + 13, wParam);
            return (TRUE);
          }

        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              {
                WORD i;   /* Looping variable */


                /* Postscript Test */
                if (SendMessage (GetDlgItem (pwnd, IDD_USER + 2),
                                 BM_GETCHECK, 0, 0L))
                  tpValue.fPostscript = TRUE;
                else
                  tpValue.fPostscript = FALSE;


                /* Test Type (7 or 8 bit ASCII) */
                if (SendMessage (GetDlgItem (pwnd, IDD_USER + 5),
                                 BM_GETCHECK, 0, 0L))
                  tpValue.f8BitTest = TRUE;
                else
                  tpValue.f8BitTest = FALSE;


                /* Serial/Parallel and Port number */
                for (i =  IDD_USER +  7;
                     i <= IDD_USER + 13 &&
                     SendMessage (GetDlgItem (pwnd, i),
                                  BM_GETCHECK, 0, 0L)  == 0;
                     ++i)
                  ;

                i = i - (IDD_USER + 7);

                if (i < 3)
                  {
                    /* Parallel Port */
                    tpValue.fSerialTest = FALSE;
                    tpValue.wPort = i;
                  }
                else
                  {
                    /* Serial Port */
                    tpValue.fSerialTest = TRUE;
                    tpValue.wPort = i - 3;
                  }

                // Fall through to IDCANCEL.
              }

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * CustInfoCmdDlg - Procedure to handle the Customer Information
 *                  dialog box.
 *********************************************************************/

LONG FAR PASCAL CustInfoCmdDlg (PWND pwnd,
                                WORD message,
                                WORD wParam,
                                DWORD lParam)
{
  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          DisplayStatus (ST_CUST_INFO);

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          /* Set up the edit items */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 2),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[0],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          SendMessage (GetDlgItem (pwnd, IDD_USER + 4),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[1],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          SendMessage (GetDlgItem (pwnd, IDD_USER + 6),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[2],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          SendMessage (GetDlgItem (pwnd, IDD_USER + 8),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[3],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          SendMessage (GetDlgItem (pwnd, IDD_USER + 10),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[4],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          SendMessage (GetDlgItem (pwnd, IDD_USER + 12),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[5],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          SendMessage (GetDlgItem (pwnd, IDD_USER + 14),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[6],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          SendMessage (GetDlgItem (pwnd, IDD_USER + 16),
                       EM_SETBUF, (WORD) pCustInfoDlg->chCustInfo[7],
                       MAKELONG (MAX_CUST_INFO, MAX_CUST_INFO));
          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              // Fall through to IDCANCEL.

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * AboutDlg - Procedure to handle the about box.
 *********************************************************************/

LONG FAR PASCAL AboutDlg (PWND pwnd,
                          WORD message,
                          WORD wParam,
                          DWORD lParam)
{
  switch (message)
    {
      case WM_INITDIALOG:
        {
#if HEAP_DEBUG
          {
            CHAR chBuffer[80];
            PSZ  pszPointer;
            pszPointer = malloc (_memmax() - 16);
            free (pszPointer);
            _heapset (0x50);
            sprintf (chBuffer, "_memavl = %u - _memmax = %u = %u, at = %04X",
                     _memavl(), _memmax(), _memavl() - _memmax(), pszPointer);
            ShowStatus (chBuffer);
          }
#endif
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );
          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            case IDOK:
            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }
        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * MemoryBlockDisplayDlg - Procedure to handle the about box.
 *********************************************************************/

LONG FAR PASCAL MemoryBlockDisplayDlg (PWND pwnd,
                                       WORD message,
                                       WORD wParam,
                                       DWORD lParam)
{
  static MEMORY_STRUCT *pMem;         /* Stores memory structure       */
  static TSR_PROGRAMS_STRUCT *pTsr;   /* Stores TSR Programs structure */
  static MEM_MAP *pmmMap = NULL;      /* Local copy of visual mem map  */
  INT  i, u;                          /* Looping variables             */
  WORD wSize;                         /* Size of various structs       */
  CHAR chBuffer[80];                  /* Local string buffer           */
  QSZ  *pqszStrings = NULL;           /* String buffer                 */


  switch (message)
    {
      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );


          /* Set the colors for the listbox */
          {
            PWND_LIST pwndList;

            pwndList = (PWND_LIST) GetDlgItem (pwnd, IDD_USER + 3);
            pwndList->isaHiliteColor = isaMemoryMap;
         /* pwndList->isaColor       = isaMemoryMap; */
          }


          /* Get the memory map */

          wSize = GetInfoSize (IDI_MEMORY_RECORD, FALSE);

          if (wSize == 0)
            break;


          /* Allocate enough room for the info structure */
          pMem = malloc (wSize);

          if (pMem == NULL)
            {
              OutOfMemory();
              break;
            }

          /* Zero out the structure */
          memset (pMem, '\0', wSize);

          /* Fill the structure with the information */
          if (GetInfo (IDI_MEMORY_RECORD, pMem, FALSE, FALSE, FALSE))
            {
              free (pMem);
              break;
            }


          /* Add the memory map to the memory listbox */

          /* Allocate a local copy of the visual mem map */
          pmmMap = malloc (sizeof (MEM_MAP));
          if (pmmMap == NULL)
            {
              free (pMem);
              break;
            }

          /* Clear the list box */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 3),
                       LB_RESETCONTENT, NULL, NULL);

          /* Add all the strings */
          for (i = NUM_OF_ROWS - 1; i >= 0; --i)
            {
              /* Put the overlay over the memory map */
              strcat (chBuffer, pMem->abMemoryMap[i]);
              for (u = 0; u < NUM_OF_COLS - 1; ++u)
                if (pMem->abMemMapOverlay[i][u])
                  pMem->abMemoryMap[i][u] = pMem->abMemMapOverlay[i][u];

              /* Display hexadecimal equivilent of memory location */
              sprintf (chBuffer, "%04X %s", (WORD) i * 0x0400, 
                       pMem->abMemoryMap[i]);

              /* Put the string into the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 3),
                           LB_ADDSTRING,
                           ((WORD) (isaMemoryMap) << 8) + TRUE,
                           (DWORD) ((CHAR FAR *) (chBuffer)));

              /* Store this line of the visual mem map */
              strcpy (pmmMap->abMap[i], chBuffer);
            }


          /* Free up the memory info strcture */
          free (pMem);


          /* Get the TSR list */

          wSize = GetInfoSize (IDI_TSR_PROGRAMS_RECORD, FALSE);

          if (wSize == 0)
            break;


          /* Allocate enough room for the info structure */
          pTsr = malloc (wSize);

          if (pTsr == NULL)
            {
              OutOfMemory();
              break;
            }

          /* Zero out the structure */
          memset (pTsr, '\0', wSize);

          /* Fill the structure with the information */
          if (GetInfo (IDI_TSR_PROGRAMS_RECORD, pTsr, FALSE, FALSE, FALSE))
            {
              free (pTsr);
              break;
            }

          /* Convert the structure to strings */
          pqszStrings = SprintTsrInfo (pTsr, FALSE);


          /* Add the TSR list to the memory listbox */

          /* Clear the list box */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                       LB_RESETCONTENT, NULL, NULL);;

          for (u = 2; pqszStrings[u] != NULL; ++u)
            ;

          /* Add all the strings */
          for (i = u - 1; i >= 2; --i)
            {
              /* Drop the command line parameters */
              pqszStrings[i][TSR_CMD_LINE_COL] = '\0';


              /* Put the string into the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                           LB_ADDSTRING,
                           ((WORD) (isaNil) << 8) + TRUE,
                           (DWORD) ((CHAR FAR *) (pqszStrings[i])));
            }

          /* Highlight the first item in the list box */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 1), LB_SETCURSEL, 0, 0L);

          /* Show where the initial item is stored */
          UpdateMemMap (pwnd, pmmMap);

          free (pTsr);
          FreeStringSpace (pqszStrings);

          break;
        }

      case WM_LISTBOX_COMMAND:
        {
          switch (HIWORD (lParam))
            {
              case LBN_SELCHANGE:
                {
                  /* Do nothing if the item was re-selected, or if it */
                  /*   came from the wrong listbox.                   */
                  if ((LOWORD (lParam) & flbrReselect) || wParam != IDD_USER + 1)
                    break;

                  UpdateMemMap (pwnd, pmmMap);
                }
            }

          break;
        }


      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            case IDCANCEL:
            case IDOK:
              free (pmmMap);
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }
        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * UpdateMemMap - Updates the memory map with the indication of where
 *                the currently selected item in the TSR list is
 *                located.
 *
 * pwnd   - Dialog box PWND
 * pmmMap - Pointer to the MEM_MAP structure.
 *********************************************************************/

VOID UpdateMemMap (PWND pwnd, MEM_MAP *pmmMap)
{
  CHAR  chBuffer[80];   /* Local string                       */
  WORD  wAddress;       /* Address of TSR/MCB                 */
  DWORD dwBlockSize;    /* Length of TSR/MCB                  */
  INT   iRow;           /* Row in mem map                     */
  INT   iCol;           /* Column in mem map                  */
  INT   iNumOfChars;    /* Number of marker characters to add */
  PWND  pwndMap;        /* PWND of mem map                    */
  INT   iIndex;         /* Character count of marker          */
  INT   i;              /* Looping variable                   */


  /* Get the visual memory map's PWND */
  pwndMap = GetDlgItem (pwnd, IDD_USER + 3);

  /* Determine which item has been selected */
  SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
               LB_GETTEXT,
               79, (DWORD) ((CHAR FAR *) chBuffer));

  sscanf (&chBuffer[TSR_ADDRESS_COL], "%04X", &wAddress);
  dwBlockSize = atol (&chBuffer[TSR_SIZE_COL]);


  /* Find the place to begin marking the visual mem map */
  iRow        = (NUM_OF_ROWS - 1) - (wAddress / 0x400);
  iCol        = (wAddress % 0x400) / 0x40;
  iNumOfChars = (WORD) ((dwBlockSize + 1023) / 1024);


  /* Re-add the original lines to the listbox */
  SendMessage (pwndMap, LB_RESETCONTENT, NULL, NULL);

  for (i = NUM_OF_ROWS - 1; i >= 0; --i)
    {
      SendMessage (pwndMap, LB_ADDSTRING,
                   ((WORD) (isaMemoryMap) << 8) + FALSE,
                   (DWORD) ((CHAR FAR *) (pmmMap->abMap[i])));
    }

  /* Now change the lines in the list box */

  /* Set the selection to our item */
  SendMessage (pwndMap, LB_SETCURSEL, iRow, NULL);

  /* Get the line from the list box */
  SendMessage (pwndMap, LB_GETTEXT, 79,
               (DWORD) (CHAR FAR *) (chBuffer));

  iIndex = 0;

  while (iIndex < iNumOfChars)
    {
      /* Choose the appropriate character */
      if (iNumOfChars == 1)
        chBuffer[iCol + 5] = 'º';
      else if (iIndex == 0)
        chBuffer[iCol + 5] = 'Ã';
      else if (iIndex + 1 == iNumOfChars)
        chBuffer[iCol + 5] = '´';
      else
        chBuffer[iCol + 5] = 'Ä';

      /* Calculate the next string */
      if (++iCol > NUM_OF_COLS - 2)
        {
          /* Send the string back into the list box */
          SendMessage (pwndMap, LB_REPLACESTRING, (iRow << 8) + isaMemoryMap,
                       (DWORD) (CHAR FAR *) (chBuffer));

          /* Calculate where the next character should go */
          if (iIndex + 1 < iNumOfChars)
            {
              iCol = 0;
              iRow--;

              /* Get the new string */
              SendMessage (pwndMap, LB_SETCURSEL, iRow, NULL);

              SendMessage (pwndMap, LB_GETTEXT, 79,
                           (DWORD) (CHAR FAR *) (chBuffer));
            }
        }

      ++iIndex;
    }

  /* Send the last string back into the list box */
  SendMessage (pwndMap, LB_REPLACESTRING, (iRow << 8) + isaMemoryMap,
               (DWORD) (CHAR FAR *) (chBuffer));

  /* Cause the listbox to repaint */
  DrawWindow (pwndMap);
}


/*********************************************************************
 * MemoryBrowserDlg - Procedure to handle the about box.
 *********************************************************************/

LONG FAR PASCAL MemoryBrowserDlg (PWND  pwnd,
                                  WORD  message,
                                  WORD  wParam,
                                  DWORD lParam)
{
  static MEMORY_STRUCT *pMem;         /* Stores memory structure       */
  static MEM_MAP *pmmMap = NULL;      /* Local copy of visual mem map  */
  ROM_MAP *pRomMap = NULL;            /* ROM area locations and sizes  */
  COMPUTER_STRUCT *pCpu = NULL;       /* For getting BUS type (PS/2)   */
  INT  i, u;                          /* Looping variables             */
  WORD wSize;                         /* Size of various structs       */
  CHAR chBuffer[80];                  /* Local string buffer           */
  PSZ  *ppszStrings = NULL;           /* String buffer                 */


  switch (message)
    {
      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          /* Set the colors for the listbox */
          {
            PWND_LIST pwndList;

            pwndList = (PWND_LIST) GetDlgItem (pwnd, IDD_USER + 3);
            pwndList->isaHiliteColor = isaMemoryMap;
         /* pwndList->isaColor       = isaMemoryMap; */
          }


          /* Set up the edit buffer */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 5), EM_SETBUF,
                       (WORD) EditBuf, MAKELONG (64, 64));

          EditBuf[0] = '\0';

          /* Get the memory map */

          wSize = GetInfoSize (IDI_MEMORY_RECORD, FALSE);

          if (wSize == 0)
            break;


          /* Allocate enough room for the info structure */
          pMem = malloc (wSize);

          if (pMem == NULL)
            {
              OutOfMemory();
              break;
            }

          /* Zero out the structure */
          memset (pMem, '\0', wSize);

          /* Fill the structure with the information */
          if (GetInfo (IDI_MEMORY_RECORD, pMem, FALSE, FALSE, FALSE))
            {
              free (pMem);
              break;
            }


          /* Add the memory map to the memory listbox */

          /* Allocate a local copy of the visual mem map */
          pmmMap = malloc (sizeof (MEM_MAP));
          if (pmmMap == NULL)
            {
              free (pMem);
              break;
            }

          /* Clear the list box */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 3),
                       LB_RESETCONTENT, NULL, NULL);

          /* Add all the strings */
          for (i = NUM_OF_ROWS - 1; i >= 0; --i)
            {
              /* Put the overlay over the memory map */
              strcat (chBuffer, pMem->abMemoryMap[i]);
              for (u = 0; u < NUM_OF_COLS - 1; ++u)
                if (pMem->abMemMapOverlay[i][u])
                  pMem->abMemoryMap[i][u] = pMem->abMemMapOverlay[i][u];

              /* Display hexadecimal equivilent of memory location */
              sprintf (chBuffer, "%04X %s", (WORD) i * 0x0400, 
                       pMem->abMemoryMap[i]);

              /* Put the string into the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 3),
                           LB_ADDSTRING,
                           ((WORD) (isaMemoryMap) << 8) + TRUE,
                           (DWORD) ((CHAR FAR *) (chBuffer)));

              /* Store this line of the visual mem map */
              strcpy (pmmMap->abMap[i], chBuffer);
            }


          /* Free up the memory info strcture */
          free (pMem);


          /* Get the list of ROMs and Option ROMs */

          ppszStrings = ListOptionRoms();


          /* Clear the list box */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                       LB_RESETCONTENT, NULL, NULL);;


          /* Load the Option ROM information into the listbox */
          for (i = 0; ppszStrings[i] != NULL; ++i)
            {
              /* Put the string into the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                           LB_ADDSTRING,
                           ((WORD) (isaNil) << 8) + TRUE,
                           (DWORD) ((CHAR FAR *) (ppszStrings[i])));
            }

          free (ppszStrings[0]);
          free (ppszStrings);


          /* Highlight the first item in the list box */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 1), LB_SETCURSEL, 0, 0L);


          /* Show where the initial item is stored */
          UpdateMemMap (pwnd, pmmMap);

          break;
        }

      case WM_LISTBOX_COMMAND:
        {
          switch (HIWORD (lParam))
            {
              case LBN_SELCHANGE:
                {
                  /* Do nothing if the item was re-selected, or if it */
                  /*   came from the wrong listbox.                   */
                  if ((LOWORD (lParam) & flbrReselect) ||
                       wParam != IDD_USER + 1)
                    break;

                  UpdateMemMap (pwnd, pmmMap);
                }
            }

          break;
        }


      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            case IDOK:
              {
                CHAR  chTitle[80];          /* Title of ROM area    */
                WORD  wSearchSegment;       /* Segment to search    */
                DWORD dwSearchLength;       /* Search Length        */
                INT   i;                    /* Looping variable     */


                /* Get the string from the edit box */
                SendMessage (GetDlgItem (pwnd, IDD_USER + 1), LB_GETTEXT,
                             64, (DWORD) ((CHAR FAR *) chTitle));

                /* Obtain the search parameters */
                sscanf (&chTitle[TSR_ADDRESS_COL], "%04X", &wSearchSegment);
                dwSearchLength = atol (&chTitle[TSR_SIZE_COL]);

                /* Put the title in a more permanent "home" */
                for (i = TSR_ADDRESS_COL - 1; i > 0 && chTitle[i] == ' '; --i)
                  ;
                chTitle[i + 1] = '\0';
                strcpy (EditBuf2, chTitle);

                DisplayStatus (ST_SEARCHING);

                /* Free up the memory map before browsing */
                free (pmmMap);

                pqszBrowseStrings =
                    GetBrowseInfo (EditBuf2,
                                   EditBuf,
                                   (CHAR FAR *) ((DWORD) wSearchSegment << 16),
                                   dwSearchLength);

                pszBrowseTitle = EditBuf2;

                PostMessage (pwndStatusLine, WM_PAINT, NULL, NULL);
                PostMessage (pwndMainFrame, WM_PAINT, NULL, NULL);

                //  EndDialog(pwnd, wParam );
                ((PWND_DLG)pwnd)->wParamEnd = wParam;
                break;
              }

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              free (pmmMap);
              break;
          }
        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * ViewWhichFile - Procedure to handle the Insert Command file choice
 *                 dialog box.
 *********************************************************************/

LONG FAR PASCAL ViewWhichFileDlg (PWND  pwnd,
                                  WORD  message,
                                  WORD  wParam,
                                  DWORD lParam)
{
  FILE_INFO FAR *pfi;   /* Local file info structure */


  switch (message)
    {
      // WM_INITDIALOG is your chance to perform Dialog initialization before
      // before the dialog box is displayed.

      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );

          // Gives the dialog box a shadow.
          //  pwnd->wExtStyle |= WS_SHADOW;

          DisplayStatus (ST_VIEW_WHICH_FILE);


          /* Clear out the listbox */
          SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                       LB_RESETCONTENT, NULL, NULL);

          /* Load the list box with the list of matching files */
          pfi = pfiDlg;

          while (pfi != NULL && pfi->fpNextFileInfo != NULL)
            {
              SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                           LB_ADDSTRING,
                           ((WORD) (isaNil) << 8) + TRUE,
                           (DWORD) (pfi->fpszPathToFile));

              pfi = (FILE_INFO FAR *) pfi->fpNextFileInfo;
            }

          if (SendMessage (GetDlgItem (pwnd, IDD_USER + 1),
                           LB_GETCOUNT, 0, 0L) > 0)
            {
              /* Highlight the first item in the list box */
              SendMessage (GetDlgItem (pwnd, IDD_USER + 1), LB_SETCURSEL, 0, 0L);
            }

          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            // User pressed the Ok or the Cancel Button.
            //  At this point, you should retrieve any state info you need.
            //  The Edit buffer will contain any text you may need.
            //  EndDialog MUST be called if you intend to end the dialog.

            case IDOK:
              {
                /* Get the string from the listbox */
                SendMessage (GetDlgItem (pwnd, IDD_USER + 1), LB_GETTEXT,
                             255, (DWORD) ((CHAR FAR *) EditBuf));

                pszInsertFilename = EditBuf;

                // Fall through to IDCANCEL
              }

            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }

        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


/*********************************************************************
 * WarnWindowsUserDlg - Procedure to handle the about box.
 *********************************************************************/

LONG FAR PASCAL WarnWindowsUserDlg (PWND pwnd,
                                    WORD message,
                                    WORD wParam,
                                    DWORD lParam)
{
  switch (message)
    {
      case WM_INITDIALOG:
        {
          // Gives the dialog box a border.
          SetWindowStyle ( pwnd, pwnd->style | WS_BORDER );
          break;
        }

      case WM_DRAWITEM:
        {
          DrawPlateButton1 (pwnd, message, wParam, lParam);
          break;
        }

      case WM_COMMAND:
        switch (wParam)
          {
            case IDOK:
            case IDCANCEL:
              //  EndDialog(pwnd, wParam );
              ((PWND_DLG)pwnd)->wParamEnd = wParam;
              break;
          }
        break;

      default:
        return (FALSE);
    }

  return (TRUE);
}


#endif /* CW_INCLUDED */
