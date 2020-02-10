#include <winenv.h>
#include "ws.h"
#include "wsrc.h"
#include "pro.h"
#include "lib\\common\\sulib.h"

/*-------------------------------------------------------------------------*/
/* The following code was added to support the DOS/Windows merge				*/
/*-------------------------------------------------------------------------*/

#define	FMT_UPDATE			500

WORD		wFatalErrorType;		/* Used by wsFatalError()	*/
WORD  	wWillReboot;			/* Used by wsFatalError()	*/

HANDLE	hFmtWnd;					/* Handle to format status dialog		*/

FARPROC	fpxFmtProDlg;			/* Ptr to fmt status handler instance	*/
PSTR		szFmtStatus[ MAXSTR / 2 ];
PSTR		szPercent[ MAXSTR / 2 ];

WORD		wCpyErrorType;
WORD		wWillExit;
PSTR		szErrorStr[ MAXSTR / 2 ];
PSTR		szFileName[ MAXSTR / 2 ];

WORD     wPrepErrType;
HCURSOR  hSavedCur = NULL;

/*-------------------------------------------------------------------------*/
/*	Function which prompts the user to insert the disk they labeled			*/
/* Uninstall into drive A:.																*/
/* 																								*/
/* WORD PUBLIC wsInsertUninstalDisk( void )											*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:																						*/
/*     OK (0) if user pressed OK key else												*/
/*-------------------------------------------------------------------------*/

WORD PUBLIC wsInsertUninstall(char *szUserLabel )
{
	return ( (WORD)fDialogWithlParam( DLG_UNINSTALL,GetActiveWindow(),
									          wsUninstallDlg, (LONG)((LPSTR)szUserLabel) ) );
}

/*-------------------------------------------------------------------------*/
/*	wsUninstallDlg( hDlg, uiMessage, wParam, lParam )                       */
/*	                                                                        */
/*	Arguments:                                                              */
/*	    hDlg            window handle of about dialog window                */
/*	    uiMessage       message number                                      */
/*	    wParam          message-dependent                                   */
/*	    lParam          message-dependent                                   */
/*	                                                                        */
/*	Returns:                                                                */
/*	    TRUE if message has been processed, else FALSE                      */
/*	                                                                        */
/*-------------------------------------------------------------------------*/

BOOL FAR PASCAL wsUninstallDlg(HWND hDlg,unsigned uiMessage,WORD wParam,long lParam)
{
   char    szTmp1[200];
   char    szTmp2[200];

	switch (uiMessage)
	{
		case WM_COMMAND:
			switch (wParam )
			{
				case ID_OK:
					EndDialog( hDlg, 0 );
					break;

				default:
					break;
			}
			return TRUE;

		case WM_INITDIALOG:
			wsDlgInit( hDlg, DLGINIT_REMOVE_CLOSE_MENU );
         GetDlgItemText(hDlg,ID_STATUS0,szTmp1,sizeof(szTmp1));
         wsprintf(szTmp2,szTmp1,(LPSTR)lParam);
         SetDlgItemText(hDlg,ID_STATUS0,szTmp2);
			MessageBeep( 0 );
			return TRUE;

		default:
			return( FALSE );
	}
}

/*-------------------------------------------------------------------------*/
/*	Function to display a dialog box to let the user select the proper		*/
/*	format for a 3.5" disk.                                                 */
/* 																								*/
/* WORD PUBLIC wsSelectDskFmt( void )                                      */
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:                                                                */
/*    0 if 720K, 1 if 1.44 MB, 2 if CANCEL                                 */
/*-------------------------------------------------------------------------*/

WORD PUBLIC wsSelectDskFmt( int iDriveType )
{

   return( fDialogWithlParam(DLG_FMTTYPE,GetActiveWindow(),wsDskTypeDlg,
                             (DWORD)iDriveType) );

}

/*-------------------------------------------------------------------------*/
/* Process messages sent to the dialog box DLG_DISKFMT_TYPE which allows	*/
/*	the user to select the proper format for a 3.5" disk.							*/
/*	                                                                        */
/*	wsUninstallDlg( hDlg, uiMessage, wParam, lParam )                       */
/*																									*/
/*	Arguments:                                                              */
/*	    hDlg            window handle of about dialog window                */
/*	    uiMessage       message number                                      */
/*	    wParam          message-dependent                                   */
/*	    lParam          message-dependent                                   */
/*                     at WM_INIT time, lParam will be iDriveType where    */
/*                                                                         */
/*                     iDriveType = 2 = 1.2mb floppy drive.                */
/*                     iDriveType = 4 = 1.44mb floppy drive.               */
/*	                                                                        */
/*	Returns: Return value will indicate choice of disk capacity chosen:     */
/*                                                                         */
/*          Return of 0 indicates lower density choice was made.           */
/*          Return of 1 indicates high density choice was made.            */
/*	                                                                        */
/*-------------------------------------------------------------------------*/

BOOL FAR PASCAL wsDskTypeDlg( HWND hDlg, unsigned uiMessage, WORD wParam,
										  long lParam )
{
	WORD		  wSelection;
   static int iSavDrvType;
	
	switch (uiMessage)
	{
		case WM_COMMAND:
			switch (wParam )
			{
				case ID_OK:
					if ( ! SendDlgItemMessage( hDlg, ID_LOW_DENSITY, BM_GETCHECK, 0, 0L ) )
						wSelection = 1;
               else
						wSelection = 0;
					EndDialog( hDlg, wSelection );
					break;

			}
			return TRUE;

		case WM_INITDIALOG:

         iSavDrvType = (int)LOWORD(lParam);

         if ( iSavDrvType == 2 ) {
            SetDlgItemText(hDlg, ID_HIGH_DENSITY, wsLoadSz(IDS_KB1200,NULL,0));
            SetDlgItemText(hDlg, ID_LOW_DENSITY, wsLoadSz(IDS_KB360,NULL,0));
         }
			wsDlgInit( hDlg, DLGINIT_REMOVE_CLOSE_MENU );

			/* Send messge to hi-lite top radio button (high capacity choice)	*/

			SendDlgItemMessage( hDlg, ID_HIGH_DENSITY, BM_SETCHECK, 1, 0L );
			MessageBeep( 0 );
			return TRUE;

		default:
			return( FALSE );
	}
}

/*-------------------------------------------------------------------------*/
/*	Opens the disk formatting status box on the screen and then calls the	*/
/*	format status update function to set the initial state to 0%.				*/
/*																									*/
/*	void PUBLIC wsFmtDlgOpen( void )														*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/*-------------------------------------------------------------------------*/

void PUBLIC wsFmtDlgOpen( void )
{
	
	fpxFmtProDlg = MakeProcInstance( (FARPROC)wsFmtProDlg, hInstWS );
	hFmtWnd = CreateDialog( hInstWS, MAKEINTRESOURCE( DLG_FMTSTATUS ),
									GetActiveWindow(), fpxFmtProDlg );

	ShowWindow( hFmtWnd, SHOW_OPENWINDOW );
	UpdateWindow( hFmtWnd );
	wsFmtDlgUpdate( 0 );
}   

/*-------------------------------------------------------------------------*/
/* Removes the format status box from the screen.									*/
/*																									*/
/*	void PUBLIC wsFmtDlgClose( void )													*/
/*																									*/
/*	ARGUMENTS:	NONE																			*/
/*	RETURNS:		void																			*/
/*																									*/
/*-------------------------------------------------------------------------*/

void PUBLIC wsFmtDlgClose( void )
{
	DestroyWindow( hFmtWnd );
	FreeProcInstance( fpxFmtProDlg );
}

/*-------------------------------------------------------------------------*/
/* Updates the percentage complete line in the format status dialog by		*/
/* sending a message to the window with the new percentage value.				*/
/*																									*/
/*	void PUBLIC wsFmtDlgUpdate( WORD wPercent )										*/
/*																									*/
/*	ARGUMENTS:	wPercent	-	Percentage of format that is complete.				*/
/*	RETURNS:		void																			*/
/*																									*/
/*-------------------------------------------------------------------------*/

void PUBLIC wsFmtDlgUpdate( WORD wPercent )
{
	SendMessage( hFmtWnd, FMT_UPDATE, wPercent, 0L );
}

/*-------------------------------------------------------------------------*/
/* Updates the percentage complete line in the format status dialog by		*/
/* sending a message to the window with the new percentage value.				*/
/*																									*/
/*	void PUBLIC wsFmtDlgUpdate( WORD wPercent )										*/
/*																									*/
/*	Arguments:                                                              */
/*		hDlg			Handle for the format status window								*/
/*		uiMessage	message number FMT_UPDATE or WN_INITDIALOG					*/
/*		wParam		If uiMessage == FMT_UPDATE the percentage complete value	*/
/*		lParam		Not used.																*/
/*	                                                                        */
/*	Returns:			TRUE if message was handled else FALSE							*/
/*																									*/
/*-------------------------------------------------------------------------*/

BOOL FAR PASCAL wsFmtProDlg( HWND hDlg, unsigned uiMessage, WORD wParam,
									  long lParam )
{
	switch ( uiMessage )
	{
		case WM_INITDIALOG:
			wsDlgInit( hDlg, 0 );
			wsLoadSz( IDS_PERCENT, (PSTR)szPercent , MAXSTR*2);
			/* 	MessageBeep( 0 ); */
			return( TRUE );

		case FMT_UPDATE:
			wsprintf( (LPSTR)szFmtStatus, "%d%s", wParam, (LPSTR)szPercent );
			SetDlgItemText( hFmtWnd, ID_STATUS0, (LPSTR)szFmtStatus );
			return( TRUE );

		default:
			return( FALSE );
	}
}

/*-------------------------------------------------------------------------*/
/* Displays a file copy error message in a dialog box and tells the user	*/
/*	the program will either continue to exit to DOS depending of the value	*/
/* of the argument WillReboot.															*/
/* 																								*/
/*	WORD PUBLIC wsDosCopyError( char *szFile, WORD wType, WORD WillReboot ) */
/*																									*/
/*	ARGUMENTS:																					*/
/*		szFile			Ptr to a file name												*/
/*		wErrorType		Enumerated error type											*/
/*		WillExit			Signals which exit message should be displayed			*/
/*							!0 will show reboot ELSE exit to DOS message				*/
/*	RETURNS:																						*/
/*																									*/
/*-------------------------------------------------------------------------*/

void PUBLIC wsDosCopyError( char *szFile, WORD wErrorType, WORD WillExit )
{
   char   szText1[512];
   char   szText2[512];
   char   szCaption[50];

	if ( wErrorType > MAX_DOSCPYERR )
		wErrorType = MAX_DOSCPYERR;

   wsLoadSz(IDS_CPYERRPREFIX, szText1 , sizeof(szText1));
   wsLoadSz(IDS_DOSCPYERROR + wErrorType, szText2 , sizeof(szText2));

   lstrcat(szText1,szText2);
   wsprintf(szText2, szText1, (LPSTR)szFile);

   if ( wWillExit ) {
      wsLoadSz(IDS_DOSERROPT + 1, szText1 , sizeof(szText1));
      wsLoadSz(IDS_FATAL_CAPTION, szCaption , sizeof(szCaption));
   }
   else {
      wsLoadSz(IDS_DOSERROPT, szText1 , sizeof(szText1));
      wsLoadSz(IDS_NONFATAL_CAPTION, szCaption , sizeof(szCaption));
   }
   lstrcat(szText2,szText1);
   
   MessageBox(GetActiveWindow(),szText2,szCaption,MB_OK|MB_SYSTEMMODAL|
              MB_ICONEXCLAMATION);
}

/*-------------------------------------------------------------------------*/
/*	Displays a generic error dialog on the screen. The caller passes the		*/
/* resourse ID number of the dialog to be displayed.								*/
/*                 																			*/
/*	void PUBLIC wsNonFatalError( WORD wMsgID )         							*/
/*																									*/
/*	ARGUMENTS:																					*/
/*		wMsgID 	   Resource ID of the dialog box to be displayed.				*/
/*	RETURNS:																						*/
/*     void																						*/
/*-------------------------------------------------------------------------*/

void PUBLIC wsNonFatalError( WORD wMsgID )
{
   char   szText1[512];
   char   szCaption[50];

   wsLoadSz(wMsgID,szText1,sizeof(szText1));
   wsLoadSz(IDS_NONFATAL_CAPTION,szCaption,sizeof(szCaption));

   MessageBox(GetActiveWindow(),szText1,szCaption,MB_OK|MB_SYSTEMMODAL|
              MB_ICONEXCLAMATION);

}

/*-------------------------------------------------------------------------*/
/* Displays a fatal error message in a dialog box and tells the user the	*/
/* program will exit either by a return to DOS or a reboot.						*/
/* 																								*/
/*	WORD PUBLIC wsFatalError( WORD wType, WORD WillReboot )						*/
/*																									*/
/*	ARGUMENTS:	wType			- Enumerated error type.								*/
/*					WillReboot	- Signals which exit message should be displayed*/
/*									  !0 will show reboot ELSE exit to DOS message	*/
/*	RETURNS:		VOID                                                        */
/*-------------------------------------------------------------------------*/

void PUBLIC wsFatalError( WORD wType, WORD WillReboot )
{
   char   szText1[512];
   char   szText2[256];
   char   szCaption[50];
   WORD   wMBflags;

   wsLoadSz(IDS_FERROR+wType,szText1,sizeof(szText1));
   wsLoadSz(IDS_FATAL_CAPTION,szCaption,sizeof(szCaption));
   wMBflags = MB_OK | MB_SYSTEMMODAL;

   if ( WillReboot ) {
	   wsLoadSz(IDS_FERRORABORT,szText2,sizeof(szText2));
      wMBflags |= MB_ICONSTOP;
   }
   else {
	   wsLoadSz(IDS_FERROREXIT,szText2,sizeof(szText2));
      wMBflags |= MB_ICONEXCLAMATION;
   }

   lstrcat(szText1,"\n\n");
   lstrcat(szText1,szText2);

   MessageBox(GetActiveWindow(),szText1,szCaption,wMBflags);
}

/*----------------------------------------------------------------------------*\
|   wsQueryStartDlg( hDlg, uiMessage, wParam, lParam )                         |
|                                                                              |
|   Queries the user if they want Windows started at boot time.
|
|   Returns:                                                                   |
|       TRUE if message has been processed, else FALSE                         |
|                                                                              |
\*----------------------------------------------------------------------------*/

BOOL FAR PASCAL  wsQueryStartDlg( HWND hDlg, unsigned uiMessage, WORD wParam,
											 long lParam )
{
    WORD    fOptions;

	switch (uiMessage)
	{
		case WM_COMMAND:
			switch (wParam)
			{
				case ID_AUTOSTART:
					CheckDlgButton( hDlg, wParam, !IsDlgButtonChecked( hDlg, wParam ) );
					break;

				case ID_OK:
					fOptions = 0;
					if ( IsDlgButtonChecked( hDlg, ID_AUTOSTART ) )
						fOptions = F_AUTOSTART;
					EndDialog( hDlg, fOptions );
					return( TRUE );

				case ID_CANCEL:
					    // make this the same as pressing F3
					if ( QueryExit(NULL) )
					{
						EndDialog(hDlg, 0);
						AppQuit();
					}
					break;
			}
			return TRUE;

		case WM_INITDIALOG:
			wsDlgInit(hDlg, DLGINIT_REMOVE_CLOSE_MENU);
			CheckDlgButton( hDlg, ID_AUTOSTART, TRUE );
#ifdef NEED_FIX
			wsHelp( DLG_WINSETUP );
#endif
		return( TRUE );
	}
	return( FALSE );
}


/*-------------------------------------------------------------------------*/
/*	wsBadDosExitDlg( hDlg, uiMessage, wParam, lParam )                       */
/*	                                                                        */
/*	Arguments:                                                              */
/*	    hDlg            window handle of about dialog window                */
/*	    uiMessage       message number                                      */
/*	    wParam          message-dependent                                   */
/*	    lParam          message-dependent                                   */
/*	                                                                        */
/*	Returns:                                                                */
/*	    
/*	                                                                        */
/*-------------------------------------------------------------------------*/

BOOL FAR PASCAL wsBadDosExitDlg( HWND hDlg, unsigned uiMessage, WORD wParam, long lParam )
{

	switch (uiMessage)
	{
		case WM_COMMAND:
			switch (wParam )
			{
				case ID_EXITSETUP:
					EndDialog( hDlg, TRUE );
					AppQuit();
					break;

				case ID_OK:
					EndDialog( hDlg, FALSE );
					break;

				default:
					break;
			}
			return FALSE;

		case WM_INITDIALOG:
			wsDlgInit( hDlg, 0 );
			MessageBeep( 0 );
			return TRUE;

		default:
			return( FALSE );
	}
}

#if 0

/* BOOL PUBLIC dsWelcomeDlg(HWND hDlg, unsigned uiMessage, WORD wParam, long lParam);
 *
 * Dialog proc for welcome UI. Explane about the process, need for an
 * uninstall disk. Allow user access to help or quit. Pretty generic DlgProc
 * since help and exit are handled for this class.
 *
 * Used with template DLG_UNINSTALLINFO. Dialog is of CLS_MYDLGS.
 *
 * ENTRY:  hDlg            window handle for this dialog window.
 *         uiMessage       message for us.
 *         wParam          message-dependent                   
 *         lParam          message-dependent                   
 *
 * EXIT: BOOL as per message processing. TRUE to user if we processed.
 *                                       FALSE to user if we did not process.
 *
 *       EndDialog() return is not important.
 */
BOOL PUBLIC dsWelcomeDlg(HWND hDlg, unsigned uiMessage, WORD wParam, long lParam)
{
	switch (uiMessage) {
		case WM_COMMAND:
			switch (wParam){
				case ID_OK:
					EndDialog(hDlg,0);
					break;

				default:
					break;
			}
			return TRUE;

		case WM_INITDIALOG:
			wsDlgInit( hDlg, DLGINIT_REMOVE_CLOSE_MENU );
			return TRUE;

		default:
			return( FALSE );
	}
}

#endif

/* void PUBLIC dsStartWait(void);
 *
 * Turn the DosSetup cursor to a hour glass
 *
 * ENTRY: None.
 * EXIT: None.
 *
 */
void PUBLIC dsStartWait()
{
   HCURSOR  hCur;
        
   if ( hCur = LoadCursor(NULL,IDC_WAIT) )
      hSavedCur = SetCursor(hCur);
}

/* void PUBLIC dsEndWait(void);
 *
 * Turn the DosSetup cursor to an arrow.
 *
 * ENTRY: None.
 * EXIT: None.
 *
 */
void PUBLIC dsEndWait()
{
   if ( hSavedCur )
      SetCursor(hSavedCur);
}








