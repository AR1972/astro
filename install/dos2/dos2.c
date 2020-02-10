//=========================================================================
// 																								
// DOS2.C																					   
// 																								
//		Copyright (c) 1992 - Microsoft Corp.											
//		All rights reserved.																	
//		Microsoft Confidential																
// 								  																
// This is the main module for the GUI portion of the combined DOS/WINDOWS
//	setup program. It intializes the main window for the program, registers
// the progress dialog window class and the sbuttin window class, initializes
// the Dos upgrade and calls off to wsUpgradeHard.
// 
// Created 01-25-92
//
//=========================================================================

#include <winenv.h>
#include	<install.h>
#include "pro.h"
#include "wsrc.h"
#include "ws.h"
#include "doscode.h"
#include "sbutton.h"
#include "lib\common\sulib.h"

extern HWND  ghWnd;               // declared in pro.c
extern DWORD DptPtr;

/* Local function prorotypes. */

long FAR PASCAL WndProc     (HWND, WORD, WORD, LONG) ;
void PRIVATE wsInstallDos(void);

HWND hwndParent;
HANDLE hinstWS;

BOOL bIsUpgrade = TRUE;
char szHimemSwitch[80] = "";  //BUGBUG


/* int PASCAL WinMain (HANDLE hInstance, HANDLE hPrevInstance, LPSTR lpszCmdLine, int nCmdShow);
 * 
 * Entry point for DOS2GUI. Handles init, calls secondary dispatcher
 * wsInstallDos.
 *
 * ENTRY: hInstance     - This instance handle.
 *        hPrevInstance - Handle to previous instance, NULL if were the first.
 *        lpszCmdLine   - Command line.
 *        nCmdShow      - How do we init ? (full screen, iconized, ?).
 *
 * EXIT: None.
 *        
 */
int PASCAL WinMain (HANDLE hInstance, HANDLE hPrevInstance, LPSTR lpszCmdLine, int nCmdShow)
{
	WNDCLASS    wndclass ;

	if (hPrevInstance)
	     return FALSE ;

   dsStartWait();   // Put up hourglass while we do all this init stuff.
   wsYield(NULL);   // Yield to allow repaint before we get started.

	hinstWS = hInstance;

   /*  Register setup's own personal dialog class. We do this so that we
    *  can have generic help and exit buttons on all the setup dialogs
    *  that need them.
    */

   wndclass.hCursor       = LoadCursor(NULL,IDC_ARROW);
   wndclass.hIcon         = NULL;
   wndclass.lpszMenuName  = NULL;
   wndclass.lpszClassName = CLS_MYDLGS;
   wndclass.hbrBackground = NULL;
   wndclass.hInstance     = hInstance;
   wndclass.style         = CS_DBLCLKS | CS_SAVEBITS | CS_BYTEALIGNWINDOW;
   wndclass.lpfnWndProc   = wsDefSetupDlgProc;
   wndclass.cbClsExtra    = 0;
   wndclass.cbWndExtra    = DLGWINDOWEXTRA;

   if (!RegisterClass(&wndclass))
      return FALSE;

	/* Register the "Status Text" and "Special button" classes. */

	if (!ControlInit(hPrevInstance,hInstance))
		return FALSE;

	if (!ProInit(hPrevInstance, hInstance)) // progress dialogs!
		return FALSE;

	wsExitInit();

	wsInstallDos();
}

/* void PRIVATE wsInstallDos(void);
 *
 * Secondary level dispatcher. Handle some init work, call off to main
 * dispatcher wsUpgradeHard. Could this be modularity for modularitys sake ?
 *
 * ENTRY: None.
 *
 * EXIT: None.
 *
 */
void PRIVATE wsInstallDos(void)
{
	int nFiles;

   if ( InitDosUpgrade() ) {
      /*
       *  Some welcoming UI telling user to prepare an uninstall disk(s),
       *  Let'em quit if they get scared, provide help, ect. Allow repaint
       *  after we take the dialog down, turn on arrow cursor.
       */

      dsEndWait();

#if 0
		if ( lpInstall->Flags.fUninstall )
		   fDialog(DLG_UNINSTALLINFO,GetActiveWindow(),dsWelcomeDlg);
#endif

      wsYield(NULL);

      /*
       *  Ok, now get on with it, first display the gas gauge.
       */
		ProOpen(NULL,DLG_PROGRESS,NULL);  // Prepare progress dialog.
		ProClear(NULL);
		ProSetBarPos(0);

		nFiles = CopyDosFiles();	// get count of DOS files	

      ProSetBarRange(nFiles+21);

		DptPtr = GlobalDosAlloc(16);

		WinAssert ( DptPtr != NULL );

		GetDPTVec();
      wsUpgradeHard();

		ProClose();

		GlobalDosFree( (UINT) DptPtr );
   }
   else
      AppQuit();

	wsExitTerm(NULL);
   CleanUpDroppings();

	fDialog( DLG_DOSREBOOT, GetActiveWindow(), (FARPROC) wsExitDlg );
}
