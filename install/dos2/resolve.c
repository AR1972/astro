#include <string.h>

#include "winenv.h"
#include "pro.h"
#include "ws.h"
#include "wsrc.h"

typedef		unsigned char		UCHAR;
#include	<install.h>
#include <data.h>
#include <copy.h>
#include <global.h>
#include	<file_io.h>

#include "lib\\common\\sulib.h"

extern char 		*GetDistribPrompt	( int iDiskNum );
void RebootSystem(void);

char     gpszLoadBuf[MAX_RES];    // Used only in wsLoadsz() function for NULL case.
WORD	fExit;

typedef struct {
   LPSTR lpszPath;
   BYTE  cDisk;
} DISK, *PDISK;

extern 	struct MULT_FILES *gFiles;
extern 	void 	 InsertDisk( int DskNum );
extern   int    iWillReboot;

/*
 *  Declared in towin.c, always contains the currect uninstall disk
 *  number (zero based)
 */
extern int iUninstallDiskNum;
extern void	InitUninstallDisk	( int iUninstallDiskNum );

/*----------------------------------------------------------------------------*\
|   fDialog(id,hwnd,fpfn)                                                       |
|                                                                               |
|   Description:                                                               |
|   This function displays a dialog box and returns the exit code.                |
|   the function passed will have a proc instance made for it.                   |
|                                                                               |
|   this also handles special keyboard input by calling CheckSpecialKeys()      |
|                                                                              |
|                                                                              |
|   Arguments:                                                                 |
|   id      resource id of dialog to display                                        |
|   hwnd      parent window of dialog                                            |
|   fpfn      dialog message function                                            |
|                                                                              |
|   Returns:                                                                   |
|   exit code of dialog (what was passed to EndDialog)                            |
|                                                                              |
\*----------------------------------------------------------------------------*/
int FAR fDialog(int id, HWND hwnd, FARPROC fpfn)
{
    int      result = 0;
    MSG     msg;

    while (PeekMessage((LPMSG)&msg, NULL, WM_KEYFIRST, WM_MOUSELAST,
                        PM_NOYIELD|PM_REMOVE));

    fpfn  = MakeProcInstance(fpfn,hInstWS);

    if (!fpfn)
       goto ERR_EXIT;

    result = DialogBox(hInstWS, MAKEINTRESOURCE(id), hwnd, fpfn);

    FreeProcInstance(fpfn);

ERR_EXIT:

//    wsHelp(HLP_NULL);
    return result;
}

/*----------------------------------------------------------------------------*\
|   fDialogWithlParam(int id, HWND hwnd, PARPROC fpfn, DWORD lParam)          |
|                                  |
|   Description:                                                               |
|   This function displays a dialog box and returns the exit code.          |
|   the function passed will have a proc instance made for it.          |
|       This function is equivelent to fDialog except it calls                 |
|       DialogBoxParam(); to create the dialog so that init info               |
|       can be passed to the dislog proc at WM_INITDIALOG time.                |
|                                  |
|   this also handles special keyboard input by calling CheckSpecialKeys() |
|                                  |
|   Arguments:                                                                 |
|   id      resource id of dialog to display             |
|   hwnd      parent window of dialog                 |
|   fpfn      dialog message function                 |
|       lParam          DWORD info that will become the lParam on the          |
|                       WM_INITDIALOG message to the dialog proc.              |
|                                                                              |
|   Returns:                                                                   |
|   exit code of dialog (what was passed to EndDialog)             |
|                                                                              |
\*----------------------------------------------------------------------------*/
int FAR fDialogWithlParam(int id, HWND hwnd, FARPROC fpfn, DWORD lParam)
{
    int      result = 0;
    MSG     msg;

//    wsHelp(id);

    while (PeekMessage((LPMSG)&msg, NULL, WM_KEYFIRST, WM_MOUSELAST,
                        PM_NOYIELD|PM_REMOVE));

    fpfn  = MakeProcInstance(fpfn,hInstWS);

    if (!fpfn)
       goto ERR_EXIT;

    result = DialogBoxParam(hInstWS,MAKEINTRESOURCE(id),hwnd,fpfn,lParam);

    FreeProcInstance(fpfn);

ERR_EXIT:

//    wsHelp(HLP_NULL);
    return result;
}

/***************************************************************************
 *
 *
 * note:
 *   this uses a TASKMODAL dialog so no parent is needed.
 *
 * returns:
 *   TRUE   user wants to exit
 *   FALSE   doesn't want to exit
 *
 ***************************************************************************/

BOOL PUBLIC QueryExit(HWND hwnd)
{
   char buf[MAXSTR];
   BOOL res;
   HWND lHwnd;

   // user press exit (implies aborted setup)
   //
   // first time setup, user is told that windows is not
   // setup and given choice to continue

   EnableExit(FALSE);

   if ( hwnd )
      lHwnd = hwnd;
   else
      lHwnd = GetActiveWindow();

	if ( fExit & EF_NOEXIT )
		return ( fDialog( DLG_BADDOSEXIT, lHwnd, wsBadDosExitDlg ) );
	else
	{
	   res = MessageBox(lHwnd, 
            wsLoadSz(IDS_EXITNOTSETUP, NULL, 0),
            wsLoadSz(IDS_EXITCAP, buf, sizeof(buf)), 
            (MB_YESNO | MB_ICONEXCLAMATION | MB_DEFBUTTON2 | MB_SYSTEMMODAL))
            == IDYES;
	}

   EnableExit(TRUE);

   return res;
}


/*----------------------------------------------------------------------------*\
|   wsLoadSz()                                                                 |
|                                                                              |
|   Description:                                                               |
|       Load a string from our resource file                                   |
|                                  |
|       ids     Id of string to load                                           |
|       pch     buffer to place string, if NULL string will be placed in a     |
|               global buf                                                     |
|                                  |
|   Returns:                                                                   |
|       near pointer to loaded string                                          |
|                                                                              |
\*----------------------------------------------------------------------------*/
PSTR PUBLIC wsLoadSz(int ids, PSTR pch, int iBufLen)
{
   if (pch == NULL)
      {
      pch = gpszLoadBuf;
      LoadString(hInstWS,ids,pch,MAX_RES);
      }
   else
      LoadString(hInstWS,ids,pch,iBufLen);

   return pch;
}

void PUBLIC EnableExit(BOOL bEnable)
{
   static int iCount = 1;      // start out enabled

   if (bEnable)
      {
      if (iCount < 1)
         iCount++;
      }
   else
      iCount--;
}


/* void PUBLIC wsDlgInit(HWND hDlg, WORD wWorkFlags);
 *
 * Function preforms various common dialog init duties, these include:
 *
 * Centering the dialog.
 * Removing the Close menu.
 * Flushing the keyboard queue.
 *
 * EMTRY: HWND hDlg - Handle to dialog window were initing.
 *
 *        WORD wWorkFlags - 16 bits used to identify optional work needed
 *                          for a particular dialog init.
 *
 * EXIT: None.
 *
 */
void PUBLIC wsDlgInit(HWND hDlg, WORD wWorkFlags)
{
    RECT   rc;
    MSG    msg;
    HANDLE hMenu;
    /*
     *   Center the dialog.
     */
    GetWindowRect(hDlg,&rc);
    SetWindowPos(hDlg,NULL,
        (GetSystemMetrics(SM_CXSCREEN) - (rc.right - rc.left)) / 2,
        (GetSystemMetrics(SM_CYSCREEN) - (rc.bottom - rc.top)) / 3,
        0, 0, SWP_NOSIZE | SWP_NOACTIVATE);
    /*
     *   Remove Close menu if were asked to.
     */
    if ( wWorkFlags & DLGINIT_REMOVE_CLOSE_MENU ) {
       hMenu = GetSystemMenu(hDlg,FALSE);
       if ( hMenu ) {
          DeleteMenu(hMenu,SC_CLOSE,MF_BYCOMMAND);
          DrawMenuBar(hDlg);
      }
   }
   // Perform "flush" keyboard operation
   // We retrieve and delete all keyboard messages placed in
   // our application queue after calling CreateDialog. Doing so
   // removes "strange" actions for impatient user, who pressed keys
   // during creation of dialog.

   while (PeekMessage((LPMSG)&msg, NULL, WM_KEYFIRST, WM_MOUSELAST,
                      PM_NOYIELD|PM_REMOVE));

}

/*----------------------------------------------------------------------------*\
|   wsYield()                                                                  |
|                                                                              |
|   Description:                                                               |
|       Handle any messages in our queue, return when the queue is empty.      |
|                                                                              |
|   Returns:                                                                   |
|       FALSE if a WM_QUIT message was encountered, TRUE otherwise             |
|
|
|  PAK               6/20/91     Added hwnd parameter to allow call to
|                                IsDialogMessage in order to process
|                                key messages such as 'Enter', 'Esc', et al.,
|                                when such processing is required.  E.g.,
|                                when the Progress Dialog has a Cancel
|                                Button.
|
\*----------------------------------------------------------------------------*/
BOOL PUBLIC wsYield(HWND hwnd)
{
   MSG     msg;

	while (PeekMessage(&msg, NULL, 0, 0, PM_REMOVE)) 
	{
//		if ( msg.message == WM_KEYDOWN && msg.wParam == VK_F3 )
//			PostMessage(hwndWS,WM_COMMAND,ID_EXITSETUP,(LONG)hwnd);

      if ( !hwnd || !IsDialogMessage(hwnd, &msg) ) 
		{
         TranslateMessage(&msg);
         DispatchMessage(&msg);
      }
   }
   return TRUE;
}

void PUBLIC AppQuit()
{
    /*
     * end the windows session, if are the boot shell
     */
#ifdef DEBUG
   char         szTmp[100];

   wsprintf(szTmp,"DOSIIGUI Goodbye, iWillReboot = %i",iWillReboot);
   FatalAppExit(0,szTmp);
#endif

   if ( iWillReboot)
      PostMessage(lpInstall->wHwndWinSetup,UM_CHILD_TERMINATED,EW_REBOOTSYSTEM,0L);
	else
      PostMessage(lpInstall->wHwndWinSetup,UM_CHILD_TERMINATED,0,0L);

   DosExit(0);   // dossetup goes bye bye via Int 21h/4Ch
}

char	cDisk;
LPSTR	szEdit;

/*----------------------------------------------------------------------------*\
|                                                                              |
| wsCopyError()                                                                |
|                                                                              |
|   Handles errors, as the result of copying files.                            |
|                                                                              |
|   this may include net contention errors, in witch case the user must	       |
|   retry the operation.						       |
|                                                                              |
\*----------------------------------------------------------------------------*/
WORD PUBLIC wsCopyError(int n, LPSTR sz)
{
	char buf[200];
   char file[MAXSTR];
   int  res;
	LPSTR lpMessageText;


   lstrcpy(file, sz);		// in case our DS moves durring wsLoadSz()

	if (!wsLoadSz(IDS_ERROR + n, buf, 200)) 
	{
   	// error string doesn't exist.  if it is a net error use
		// net string, if not that use generic error string

    	if (n > ERROR_SHARE)
			wsLoadSz(IDS_ERROR + ERROR_SHARE, buf, 200);
		else
	   	wsprintf(buf, wsLoadSz(IDS_ERROR,NULL, NULL), n);
   }

   lstrcat(buf,"\n");

   // check for the out of disk space case

   lstrcat(buf, file);		// add the file name

   lpMessageText = (LPSTR) buf;
   res = fDialogWithlParam(DLG_COPYERROR, GetActiveWindow(),
                           wsErrorDlg, (DWORD)lpMessageText);
   return res;
}
/*----------------------------------------------------------------------------*\
|                                                                              |
| wsInsertDisk()                                                               |
|                                                                              |
|   Handles errors, as the result of copying files.                            |
|                                                                              |
\*----------------------------------------------------------------------------*/
WORD PUBLIC wsInsertDisk(LPSTR szSrcPath)
{

   DISK disk;

   disk.lpszPath = szSrcPath;
   disk.cDisk = (BYTE)gFiles->DiskNumber;

   szEdit = szSrcPath;

   return (WORD)fDialogWithlParam(DLG_EXPRESSINSERTDISK, GetActiveWindow(),
                                  wsDiskDlg, MAKELONG(&disk,0));
}

/*----------------------------------------------------------------------------*\
|   wsDiskDlg( hDlg, uiMessage, wParam, lParam )                               |
|                                                                              |
|   Arguments:                                                                 |
|       hDlg            window handle of about dialog window                   |
|       uiMessage       message number                                         |
|       wParam          message-dependent                                      |
|       lParam          message-dependent                                      |
|                                                                              |
|   Returns:                                                                   |
|       TRUE if message has been processed, else FALSE                         |
|                                                                              |
\*----------------------------------------------------------------------------*/

BOOL FAR PASCAL wsDiskDlg(HWND hDlg, unsigned uiMessage, WORD wParam, long lParam)
{

    switch (uiMessage)
    {
		case WM_COMMAND:
	   	switch (wParam)
	    	{
				case ID_OK:
            	EndDialog(hDlg, FC_RETRY);
		    		break;

				case ID_CANCEL:

					//BUGBUG EndDialog(hDlg, fFirstTime ? FC_IGNORE : FC_ABORT);
		    		EndDialog(hDlg, FC_IGNORE);
		    		break;
	    	}
	    	return TRUE;

		case WM_INITDIALOG:
	   {
			PDISK pdisk;
         char szTmp[MAX_INF_LINE_LEN];
			char szTmp2[MAX_INF_LINE_LEN];
			char ach[2];

         EnableMenuItem(GetSystemMenu(hDlg, FALSE), SC_CLOSE,
                        MF_BYCOMMAND | MF_GRAYED | MF_DISABLED);

         pdisk = (PDISK)LOWORD(lParam);

			if ( pdisk->cDisk == NOT_REMOVEABLE )
				strcpy (szTmp2, GetDistribPrompt(GetRealSrcDisk( gFiles->Name.Source, NOT_REMOVEABLE )));
			else
				strcpy(szTmp2,GetDistribPrompt(pdisk->cDisk));

         SetDlgItemText(hDlg, ID_STATUS0, szTmp2);

//            ach[0] = (char)UPCASE(*szDiskPath);
	  		ach[0] = vInfo.chSource;
			ach[1] = 0;
         wsLoadSz(IDS_INTO_DRIVE, szTmp, sizeof(szTmp));
         wsprintf(szTmp2, szTmp, (LPSTR)ach);
         SetDlgItemText(hDlg, ID_STATUS1, szTmp2);

         wsDlgInit(hDlg, DLGINIT_REMOVE_CLOSE_MENU);
         MessageBeep(0);
         return TRUE;

	    }
    }
    return FALSE;
}

/*----------------------------------------------------------------------------*\
|   wsErrorDlg( hDlg, uiMessage, wParam, lParam )                |
|                                                                              |
|   Arguments:                                                                 |
|       hDlg            window handle of about dialog window                   |
|       uiMessage       message number                                         |
|       wParam          message-dependent                                      |
|       lParam          message-dependent                                      |
|                                                                              |
|   Returns:                                                                   |
|       TRUE if message has been processed, else FALSE                         |
|                                                                              |
\*----------------------------------------------------------------------------*/
BOOL FAR PASCAL wsErrorDlg(HWND hDlg, unsigned uiMessage, WORD wParam, long lParam)
{
	extern int CreatingRecovery;

   switch (uiMessage)
   {
      case WM_SYSCOMMAND:    // suppress taskman
         if(wParam == SC_TASKLIST)
            return TRUE;
         break;

      case WM_COMMAND:
         switch (wParam)
         {
            case ID_RETRY:
               EndDialog(hDlg, FC_RETRY);
               break;

            case ID_IGNORE:
               EndDialog(hDlg, FC_IGNORE);
               break;

            default:
               return(FALSE);
         }
         return TRUE;
         break;

      case WM_INITDIALOG:
         SetDlgItemText(hDlg, ID_STATUS1, (LPSTR) lParam);

         EnableMenuItem(GetSystemMenu(hDlg, FALSE), SC_CLOSE,
                        MF_BYCOMMAND | MF_GRAYED | MF_DISABLED);
			if ( CreatingRecovery )
				EnableWindow( GetDlgItem(hDlg, ID_IGNORE), FALSE );
         wsDlgInit(hDlg, DLGINIT_REMOVE_CLOSE_MENU);
         MessageBeep(0);
         return TRUE;
         break;

      }

   return(FALSE);
}

/*----------------------------------------------------------------------------*\
|   define the call back function used by the FileCopy() function.	       |
|                                                                              |
\*----------------------------------------------------------------------------*/

WORD FAR PASCAL wsCopyStatus(int msg, int n, LPSTR szFile)
{
	extern int CreatingRecovery;
	WORD res;
//	char buf[80];

   switch (msg)
   {
	case COPY_INSERTDISK:
		if ( CreatingRecovery )
		{
			InitUninstallDisk(iUninstallDiskNum);
			return FC_RETRY;
		}
		else
		{
			if ( gFiles->DiskNumber == NOT_REMOVEABLE )
				return wsInsertDisk(szFile);

			InsertDisk(gFiles->DiskNumber);
			return FC_RETRY;
		}
	case COPY_ERROR:

      if ( n == ERROR_WRITE && CreatingRecovery && iUninstallDiskNum == 0 ) {
         /*
          *  For this case, we must prepare another uninstall disk. This
          *  may happen when folks are using 360kb or 720kb uninstall disks.
          */

          /* Need to add disk space check here to assure the disk is indeed full */

			InitUninstallDisk(++iUninstallDiskNum);
         return FC_RETRY;
      }
      res = wsCopyError(n,szFile);
		if ( res == FC_RETRY )
			if ( CreatingRecovery )
				InitUninstallDisk(iUninstallDiskNum);
			else
				InsertDisk(gFiles->DiskNumber);
		
		return res;

	case COPY_QUERYCOPY:

	    // special case hack for .386 files built into win386.exe

//	    infParseField(szFile, 1, buf);

//	    if (*FileName(buf) == '*')
//	    	return FALSE;		// don't copy

	    return TRUE;

	case COPY_END:
	case COPY_START:
	    SetErrorMode(msg == COPY_START);	// don't crit error on us
	    break;

   case COPY_STATUS:
//		if (n == 0)
//		{
		  	// if their is a title update it.  this allows shared titles
//			if ( infParseField( szFile, 2, buf ) )
//				ProPrintf( ID_STATUS2, wsLoadSz( IDS_COPYING,NULL,NULL ), (LPSTR)buf );
//		}
		if (n == 100)
			ProDeltaPos(1);
		wsYield(hwndParent);
		break;

	}	/* End of switch */
	return FC_IGNORE;
}

/*----------------------------------------------------------------------------*\
|   wsExitDlg( hDlg, uiMessage, wParam, lParam )                               |
|                                                                              |
|   Arguments:                                                                 |
|       hDlg            window handle of about dialog window                   |
|       uiMessage       message number                                         |
|       wParam          message-dependent                                      |
|       lParam          message-dependent                                      |
|                                                                              |
|   Returns:                                                                   |
|       TRUE if message has been processed, else FALSE                         |
|                                                                              |
\*----------------------------------------------------------------------------*/
BOOL EXPORT wsExitDlg(HWND hDlg, unsigned uiMessage, WORD wParam, LONG lParam )
{
   #define WEC_RESTART 0x42

	switch (uiMessage)
	{
		case WM_COMMAND:
			switch (wParam)
			{
				case ID_REBOOT:
					ProClose();
               iWillReboot = TRUE;  // Assure reboot.
               AppQuit();

					break;

				case ID_CANCEL:
					#ifdef DEBUG
													// allow abort in debug release
//						if ( !fFirstTime )
							EndDialog(hDlg,FALSE);
					#endif
					break;
		
				default:
				    return FALSE;
			}
			return TRUE;

		case WM_INITDIALOG:
			wsDlgInit( hDlg, DLGINIT_REMOVE_CLOSE_MENU );
			return TRUE;
	}
	return FALSE;
}




#define ISDIGIT(c)  ((c) >= '0' && (c) <= '9')

int PUBLIC atoi(PSTR sz)
{
	int n = 0;

	while (ISDIGIT(*sz))
   {
		n *= 10;
		n += *sz - '0';
		sz++;
   }
   return n;
}

