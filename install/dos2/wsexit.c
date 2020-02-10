/***************************************************************************

MODULE: wshelp.c  - Pretty much new for 3.1

   Copyright (C) Microsoft, 1991

HISTORY:

   Modified by:      Date:       Comment:
     Mikecole       7/23/91      Do away with RTF help window. Work for new
                                 context sensitive help design.

***************************************************************************/
#include "winenv.h"
#include	<file_io.h>
#include "lib\\common\\sulib.h"
#include "ws.h"
#include "wsrc.h"
#include "helpcon.h"

// Local function prototypes.

DWORD EXPORT wsSetupExit(int nCode, WORD wParam, DWORD lParam);

// Globals to this module.

char            szHelpFile[MAXSTR];
static  BOOL	 bHelpCalled = FALSE;

FARPROC lpfnOldHook = NULL;
FARPROC lpfnMyHook  = NULL;

/* BOOL PUBLIC wsExitInit(void)
 *
 * Function will grab the name of the help file from setup.inf and put
 * in the help hook.
 *
 * ENTRY: None.
 *
 * EXIT: BOOL - Success = TRUE. Failure == FALSE.
 *
 */
BOOL PUBLIC wsExitInit(void)
{
   wsLoadSz(IDS_HELPFILE,szHelpFile,MAXSTR);

   if (!(lpfnMyHook = MakeProcInstance((FARPROC)wsSetupExit, hInstWS)))
      return FALSE;

   lpfnOldHook = SetWindowsHook(WH_MSGFILTER,lpfnMyHook);

   return TRUE;
}

/* DWORD EXPORT wsSetupHelp(int nCode, WORD wParam, DWORD lParam);
 *
 * This is the WH_MSGFILTER hook function for Windows setup. This functions
 * purpose is to intercept help and exit requests and  convert them to
 * the appropriate WM_COMMAND messages.
 *
 * ENTRY: nCode - Code value that tells this function where messages are
 *                coming from; ie. dialog or message-box, keyb or mouse.
 *
 *        wParam - Always NULL.
 *
 *        lParam - Pointer to a MSG data structure.
 *
 * EXIT: Non-zero (TRUE) if this function processes the message. Otherwise FALSE.
 *
 */
DWORD EXPORT wsSetupExit(nCode, wParam, lParam)
int   nCode;
WORD  wParam;
DWORD lParam;
{
   LPMSG   lpMsg = (LPMSG)lParam;
   WORD    wHelpContext = 0;
   HWND    hDlgWnd = NULL, hWnd = NULL;
   char    szClassName[25];

   bHelpCalled = TRUE;

   if ( nCode < 0 )
      return DefHookProc(nCode, wParam, lParam, &lpfnOldHook);

   /* First check that we received a message for dialog box   */

   if ( nCode == MSGF_DIALOGBOX )
   {
       /* We want to process only keyboard messsages          */
       if ( lpMsg->message == WM_KEYDOWN )
       {

         if ( lpMsg->wParam != VK_F3 )
            return FALSE;

         /* Now we have to detetrmine handle of current dialog window.
          * We know that class name of all our dialogs is MYDLG so
          * I am going through the chain of parent windows for current
          * focus windows until I will find parent dialog or NULL.
          */

         hWnd = lpMsg->hwnd;
         while ( hWnd )
         {
             *szClassName = '\0';
             GetClassName(hWnd,szClassName,sizeof(szClassName));
             AnsiUpper(szClassName);
             if ( lstrcmpi((LPSTR)szClassName,(LPSTR)CLS_MYDLGS) == 0 )
               break;

             hWnd = GetParent(hWnd);
         }

         /* Did we find anything ???                          */
         if ( ! hWnd )
            return FALSE;

         /* Convert keyboard messages came into WM_COMMANDs to
          * the found dialog. Return TRUE because we procecessed
          */
         switch (lpMsg->wParam)
         {
            case VK_F3:                        
               PostMessage(hWnd,WM_COMMAND,ID_EXIT,(LONG)lpMsg->lParam);
               return TRUE;
               break;
         }
       }
   }
   return FALSE;
}


/* LONG EXPORT wsDefSetupDlgProc(HWND hwnd, unsigned msg, WORD wParam,
 *                               LONG lParam);
 *
 * Function acts as setup's DefDialogProc(). We use this to process Help and
 * Exit button usage so that we don't have to put code into each of our dialog
 * procs to do this. The way it works is we filter all the dialog message for
 * WM_COMMAND - ID_HELP/ID_EXIT messages, these we process right here. The rest
 * of the messages are passed on to DefDialogProc().
 *
 * ENTRY: hwnd   - Handle to dialog box who received the focus.
 *        msg    - Message.
 *        wParam - Message dependent.
 *        lParam - Message dependent.
 *
 * EXIT:  BOOL   - TRUE if message processed, FALSE if not.
 *
 */
LONG EXPORT wsDefSetupDlgProc(HWND hwnd, WORD msg, WORD wParam, LONG lParam)
{
   switch(msg)
   {
      case WM_KEYDOWN:
         switch(wParam)
         {
            case VK_F1:
               SendMessage(hwnd,WM_COMMAND,ID_HELP,lParam);
               break;
            case VK_F3:
               SendMessage(hwnd,WM_COMMAND,ID_EXIT,lParam);
               break;
         }
         break;

      case WM_SYSCOMMAND:           // suppress taskman
         if(wParam == SC_TASKLIST)
            return TRUE;
         break;

      case WM_COMMAND:
         switch(wParam)
         {
            case ID_EXIT:
               if ( QueryExit(NULL) ) {
                  if ( hwndParent )
                     DestroyWindow(hwndParent);
                  CleanUpDroppings();
                  AppQuit();
               }
               break;
           
            case ID_HELP:
            {
               HWND hContextWnd;
               WORD wHelpContext = 0;

               hContextWnd = GetWindow(hwnd, GW_CHILD);
               wHelpContext = GetWindowWord(hContextWnd, GWW_ID);

               if ( (wHelpContext >= HELP_CONTEXT_MIN) && (wHelpContext <= HELP_CONTEXT_MAX) )
               {
                  bHelpCalled = WinHelp(hwnd,szHelpFile,HELP_CONTEXT,(LONG)wHelpContext);

                  WinHelp(hwnd,szHelpFile,HELP_SETINDEX,DOS2GUI_INDEX);

                  if (! bHelpCalled)
                  {
                     MessageBox(hwnd, wsLoadSz(IDS_HELP_ERROR, NULL, 0), NULL,
                                MB_APPLMODAL | MB_OK);
                     WinHelp(hwnd, szHelpFile, HELP_QUIT, 0L);
                  }
               }
               return(TRUE);
            }
         }
   }
   return(DefDlgProc(hwnd, msg, wParam, lParam));
}

/* VOID PUBLIC wsExitTerm(HWND hwnd);
 *
 * Function properly cleans up after setup's help usage by telling
 * the engine we no longer need it and un-hooking the message filter
 * we put in when we started.
 *
 * ENTRY: hWnd - Handle to parent window.
 *
 * EXIT: none.
 *
 */
VOID PUBLIC wsExitTerm(hwnd)
HWND hwnd;
{
	if ( bHelpCalled )
		WinHelp(hwnd, szHelpFile, HELP_QUIT, 0L);

   if ( lpfnMyHook )
      UnhookWindowsHook(WH_MSGFILTER, lpfnMyHook);
}

/* void PUBLIC CleanUpDroppings(void);
 *
 * Function cleans up any temporarialy copied files along with any other
 * house keeping that needs to be done when the user either completes or
 * terminates DOS install.
 *
 * ENTRY: None.
 *
 * EXIT: None.
 *
 */
void PUBLIC CleanUpDroppings(void)
{
   char  szTmp[MAXFILESPECLEN];

   GetWindowsDirectory(szTmp,sizeof(szTmp));  // Where are we ?
   catpath(szTmp,szHelpFile);                 // Build FQP to the victim.
   AbsUnlink(szTmp);                          // The kill.
}



