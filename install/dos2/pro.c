
#include "winenv.h"
#include "lib\\common\\sulib.h"
#include "wsrc.h"
#include "pro.h"
#include "ws.h"


//======================================================================
//
//		e x t e r n s 
//
//======================================================================

extern HWND hwndParent;
extern HANDLE hInstWS;

//======================================================================
//                                                                      
//   g l o b a l   v a r i a b l e s                                    
//                                                                      
//======================================================================

HWND    ghWnd = NULL;
static int     iCnt;
static FARPROC fpxProDlg;
static DWORD   rgbFG;
static DWORD   rgbBG;
static FARPROC fpfnWait;
static HWND    hwndStatus;

#define BAR_RANGE 0
#define BAR_POS   2

#define BAR_SETRANGE  WM_USER+BAR_RANGE
#define BAR_SETPOS    WM_USER+BAR_POS
#define BAR_DELTAPOS  WM_USER+4

#ifndef COLOR_HIGHLIGHT
    #define COLOR_HIGHLIGHT     (COLOR_APPWORKSPACE + 1)
    #define COLOR_HIGHLIGHTTEXT   (COLOR_APPWORKSPACE + 2)
#endif

#define COLORBG  rgbBG
#define COLORFG  rgbFG

extern BOOL fMono;
char szNull[] = "";

//========================================================================
//
//		f u n c t i o n   d e f i n i t i o n s
//
//========================================================================


BOOL FAR PASCAL ProDlgProc(HWND, WORD, WORD, DWORD);
LONG FAR PASCAL ProBarProc(HWND, unsigned, WORD, LONG);

/*----------------------------------------------------------------------------*\
|   ProInit( hPrev,hInst )                         |
|                                                                              |
|   Description:                                                               |
|       This is called when the application is first loaded into               |
|   memory.  It performs all initialization.                |
|                                                                              |
|   Arguments:                                                                 |
|   hPrev      instance handle of previous instance              |
|   hInst      instance handle of current instance                |
|                                                                              |
|   Returns:                                                                   |
|       TRUE if successful, FALSE if not                                       |
|                                                                              |
\*----------------------------------------------------------------------------*/

BOOL PUBLIC ProInit (HANDLE hPrev, HANDLE hInst)
{
   WNDCLASS   rClass;

   if (!hPrev)
   {
      rClass.hCursor       = LoadCursor(NULL,IDC_ARROW);
      rClass.hIcon         = NULL;
      rClass.lpszMenuName  = NULL;
      rClass.lpszClassName = PRO_CLASS;
      rClass.hbrBackground = (HBRUSH)COLOR_WINDOW+1;
      rClass.hInstance     = hInst;
      rClass.style         = CS_HREDRAW | CS_VREDRAW;
      rClass.lpfnWndProc   = ProBarProc;
      rClass.cbClsExtra    = 0;
      rClass.cbWndExtra    = 2*sizeof(WORD);

      if (!RegisterClass(&rClass))
         return FALSE;
   }

#if 0
   rgbFG = GetSysColor(COLOR_HIGHLIGHTTEXT);
   rgbBG = GetSysColor(COLOR_HIGHLIGHT);
#endif

   if (fMono)
   {
      rgbBG = RGB(0,0,0);
      rgbFG = RGB(255,255,255);
   }
   else
   {
      rgbBG = RGB(0,0,255);
      rgbFG = RGB(255,255,255);
   }
   return TRUE;
}

void PUBLIC ProClear(HWND hDlg)
{
   if (!hDlg)
      hDlg = ghWnd;

   SetDlgItemText (hDlg, ID_STATUS1, szNull);
   SetDlgItemText (hDlg, ID_STATUS2, szNull);
//   SetDlgItemText (hDlg, ID_STATUS3, szNull);
//   SetDlgItemText (hDlg, ID_STATUS4, szNull);
}


/***************************************************************************

ProDlgProc
----------

DESCRIPTION:

   This is the dialog procedure for the Progress Dialog 



HISTORY:

   Modified by:      Date:       Comment:

   PAK               8/21/91     Created

***************************************************************************/
BOOL EXPORT ProDlgProc(HWND hDlg, WORD wMsg, WORD wParam, DWORD dwParam)
{
   switch (wMsg)
   {
      case WM_COMMAND:
         switch (wParam)
         {
//            case ID_CANCEL:
//               gbProgressCancel = TRUE;
//               break;

//            case ID_EXIT:
//               PostMessage(hwndWS, WM_COMMAND, ID_EXITSETUP, (LONG)hDlg);
//               break;

            default:
               return FALSE;
         }
         break;

      case WM_INITDIALOG:
         ProClear(hDlg);
         if (dwParam)
            SetDlgItemText(hDlg, ID_CANCEL, (PSTR)dwParam);
         wsDlgInit(hDlg, DLGINIT_REMOVE_CLOSE_MENU);
         break;

      default:
         return FALSE;
   }
   return TRUE;
}

/*----------------------------------------------------------------------------*\
|   ProBarProc( hWnd, uiMessage, wParam, lParam )
|
|   Description:
|   The window proc for the Progress Bar chart
|
|   Arguments:
|   hWnd      window handle for the dialog
|       uiMessage       message number
|       wParam          message-dependent
|       lParam          message-dependent
|
|   Returns:
|       0 if processed, nonzero if ignored
|
\*----------------------------------------------------------------------------*/

LONG EXPORT ProBarProc( HWND hWnd, unsigned uiMessage,
                        WORD wParam, long lParam )
{
   PAINTSTRUCT rPS;
   RECT        rc1,rc2;
   WORD        dx,dy,x;
   WORD        iRange,iPos;
   char        ach[30];
   DWORD       dwExtent;

   switch (uiMessage)
   {
      case WM_CREATE:
         SetWindowWord (hWnd,BAR_RANGE,100);
         SetWindowWord (hWnd,BAR_POS,0);
         return 0L;

      case BAR_SETRANGE:
      case BAR_SETPOS:
         SetWindowWord (hWnd,uiMessage-WM_USER,wParam);
         InvalidateRect (hWnd,NULL,FALSE);
         UpdateWindow(hWnd);
         return 0L;

      case BAR_DELTAPOS:
         iPos = GetWindowWord (hWnd,BAR_POS);
         SetWindowWord (hWnd,BAR_POS,iPos+wParam);
         InvalidateRect (hWnd,NULL,FALSE);
         UpdateWindow(hWnd);
         return 0L;

      case WM_PAINT:
         BeginPaint(hWnd,&rPS);
         GetClientRect (hWnd,&rc1);
         FrameRect(rPS.hdc,&rc1,GetStockObject(BLACK_BRUSH));
         InflateRect(&rc1,-1,-1);

         rc2 = rc1;
         iRange = GetWindowWord (hWnd,BAR_RANGE);
         iPos   = GetWindowWord (hWnd,BAR_POS);

         if (iRange <= 0)
            iRange = 1;

         if (iPos > iRange)   // make sure we don't go past 100%
            iPos = iRange;

#ifdef fixbar
THIS_CODE_NOT_DEBUGGED
         /* following code should fix overwrite of left edge of
         status bar.  If we have time to verify this, enable it */
         dx = rc1.right - rc1.left;
         dy = rc1.bottom - rc1.top;
         x  = (WORD)((DWORD)iPos * dx / iRange);
#else
         dx = rc1.right;
         dy = rc1.bottom;
         x  = (WORD)((DWORD)iPos * dx / iRange) + 1;
#endif

         wsprintf (ach,"%3d%%",(WORD)((DWORD)iPos * 100 / iRange));
         dwExtent = GetTextExtent (rPS.hdc,ach,4);

#ifdef fixbar
THIS_CODE_NOT_DEBUGGED
         /* following code should fix overwrite of left edge of
         status bar.  If we have time to verify this, enable it */
         rc1.right = rc2.left  = (rc1.left + x);
#else
         rc1.right = x;
         rc2.left  = x;
#endif

         SetBkColor(rPS.hdc,COLORBG);
         SetTextColor(rPS.hdc,COLORFG);
         ExtTextOut (rPS.hdc,
                     (dx-LOWORD(dwExtent))/2,(dy-HIWORD(dwExtent))/2,
                     ETO_OPAQUE | ETO_CLIPPED,
                     &rc1,
                     ach,4,NULL);

         SetBkColor(rPS.hdc,COLORFG);
         SetTextColor(rPS.hdc,COLORBG);
         ExtTextOut (rPS.hdc,
                     (dx-LOWORD(dwExtent))/2,(dy-HIWORD(dwExtent))/2,
                     ETO_OPAQUE | ETO_CLIPPED,
                     &rc2,
                     ach,4,NULL);

         EndPaint(hWnd,(LPPAINTSTRUCT)&rPS);
         return 0L;

    }
    return DefWindowProc(hWnd,uiMessage,wParam,lParam);
}


/*----------------------------------------------------------------------------*\
|   ProOpen ()                               |
|                                                                              |
|   Description:                                                               |
|                                                                              |
|   Arguments:                                                                 |
|                                                                              |
|   Returns:                                                                   |
|       0 if processed, nonzero if ignored                                     |
|                                                                              |
\*----------------------------------------------------------------------------*/
HWND PUBLIC ProOpen(HWND hWnd, int id, char *pszCancelButtonText)
{
   DWORD dwParam = 0L;

//   if (id == DLG_PROGRESS_GENERIC)
//      dwParam = MAKELONG(pszCancelButtonText,0);
      
   iCnt++;
   if (!ghWnd)
   {
      fpxProDlg = MakeProcInstance((FARPROC)ProDlgProc,NULL);
      ghWnd = hwndParent = CreateDialogParam(hInstWS, MAKEINTRESOURCE(id),
                                             hWnd, fpxProDlg, dwParam);
      WinAssert(ghWnd);
      ShowWindow (ghWnd,SHOW_OPENWINDOW);
      UpdateWindow(ghWnd);
   }
   ProSetBarRange(100);
   ProSetBarPos(0);
   return ghWnd;
}

/*----------------------------------------------------------------------------*\
|   ProClose ()                             |
|                                                                              |
|   Description:                                                               |
|                                                                              |
|   Arguments:                                                                 |
|                                                                              |
|   Returns:                                                                   |
|       0 if processed, nonzero if ignored                                     |
|                                                                              |
\*----------------------------------------------------------------------------*/
BOOL PUBLIC ProClose()
{
   iCnt--;
   if (ghWnd && iCnt == 0) {
      DestroyWindow (ghWnd);
      FreeProcInstance (fpxProDlg);
      ghWnd = NULL;
   }
   return TRUE;
}


BOOL PUBLIC ProSetText (int i,LPSTR lpch)
{
   if (ghWnd)
   {
      SetDlgItemText (ghWnd,i,lpch);
      return TRUE;
   }
   return FALSE;
}

BOOL FAR cdecl ProPrintf (int i, LPSTR lpch, ...)
{
   char ach[200];

   if (ghWnd)
   {
      wvsprintf(ach, lpch, (LPSTR)(&lpch+1));
      SetDlgItemText(ghWnd, i, ach);
      return TRUE;
   }
   return FALSE;
}

BOOL PUBLIC ProSetBarRange (int i)
{
   if (ghWnd)
   {
      SendDlgItemMessage(ghWnd,ID_BAR,BAR_SETRANGE,i,0L);
      return TRUE;
   }
   return FALSE;
}

BOOL PUBLIC ProSetBarPos (int i)
{
   if (ghWnd)
   {
      SendDlgItemMessage(ghWnd,ID_BAR,BAR_SETPOS,i,0L);
      return TRUE;
   }
   return FALSE;
}

BOOL PUBLIC ProDeltaPos (int i)
{
   if (ghWnd)
   {
      SendDlgItemMessage(ghWnd,ID_BAR,BAR_DELTAPOS,i,0L);
      return TRUE;
   }
   return FALSE;
}

void PUBLIC ProToTop(void)
{
   if (ghWnd)
      BringWindowToTop(ghWnd);
}



