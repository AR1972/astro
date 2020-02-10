/*
	COW : Character Oriented Windows

	dlgcore: dialog core routines
*/

#define COW
#include <cow.h>

#define DIALOG

#include <udialog.h>
#include <uevent.h>
#include <vkey.h>
#include <uwindow.h>
#include <umenu.h>
#include <uutil.h>
#include <uscreen.h>
#include <uisa.h>
#include <kkeyboar.h>
#include <kinput.h>

#include "dialog.h"
#include "button.h"
#include "event.h"
#include "window.h"
#include "screen.h"
#include "util.h"
#include "case.h"
#include "overlap.h"

#include "shadow.h"


#ifdef EXTRAS
#include <cowextra.h>
#endif

#ifdef DUAL
#include <os2.h>
extern	WORD	fProtectMode;
#else
#ifdef DOS5
#include <os2.h>
#endif
#endif

#ifdef SCREEN_FFONT
#ifdef KANJI
	-- Kanji + FFONT is not allowed
#endif
#endif


PRIVATE	BYTE fDrawItem = TRUE;			/* FALSE if don't paint */
PRIVATE	BYTE fRedrawItem = TRUE;		/* FALSE if initializing */

PRIVATE BYTE FAR * lpbWorkTemp = NULL;/* !NULL => use temporary work buffer */


STATIC PLFN_WNDPROC pfnDlg;	/* current Dialog function */

STATIC PWND pwndDlg = NULL;	/* current Dialog Window */
#ifdef DIALOG_NOSAVE
STATIC PWND pwndDlg2 = NULL;	/* previous Dialog Window */
#endif
PRIVATE BYTE fShowDlgAccel;	/* show dialog accelerators ? */
PRIVATE BYTE fButtonAction;	/* TRUE => mouse button down on button */

#ifdef DIALOG_LINE
STATIC BYTE fTwoLineDlg;	/* TRUE => dialog has 2 bottom lines of buttons */
#endif /*DIALOG_LINE*/

/* forward */
STATIC PWND	PwndPreviousControl(PWND, PWND);
BOOL FARPRIVATE	DialogFilterProc(PMSG);
STATIC BOOL	FProcessAccel(WORD);
STATIC VOID	DrawDialogBorder(PWND);

#ifdef	DIALOG_NOSAVE
extern VOID	FAR PASCAL	ScreenRedraw(VOID);
#endif	// DIALOG_NOSAVE


//
// a-emoryh - We need to alter the VK_TAB behavior in certain cases in QHELP
//    mode (new behavior is enabled if pwndPrintEdit != NULL).
//
extern PWND pwndPrintEdit;




WORD FARPRIVATE
DialogBox(pwndDialog, pfn)
/*
  -- process the dialog for a dialog box
  -- NOTE : we never actual send a KILL/SET focus to the current focus
*/
REGISTER PWND pwndDialog;
PLFN pfn;
	{
	MSG msg;

	/* save old state of world */
	PWND pwndDlgOld = pwndDlg;
	PLFN pfnDlgOld = pfnDlg;
	PWND pwndRootOld = pwndRoot;
	PWND pwndFocusOld = pwndFocus;
	PWND pwndCaptureOld = pwndCapture;
	BYTE fShowDlgAccelOld = fShowDlgAccel;
#ifdef DIALOG_LINE
	BYTE fTwoLineOld = fTwoLineDlg;
#endif /*DIALOG_LINE*/

	BOOL fMenuEnabled;
	PFFN_FILTER pfnFilterOld;
#ifdef WINDOW_OVERLAP
	PFFN_FILTER pfnOverlapFilterOld;
#endif /*WINDOW_OVERLAP*/

#ifndef DIALOG_NOSAVE
	BYTE FAR *lpbSave;		/* save under dialog */

	DrawThisWnd(NULL);      /* No window defined yet */

/* use lpbWorkTemp if non-null */
	if (lpbWorkTemp != NULL)
		{
		lpbSave = lpbWorkTemp;
		}
	else
		{
		// allocate a buffer

		lpbSave = LpbAllocWorkFar(
		     CbSizeDialog(&pwndDialog->arcWindow) + daxDbcs * 2 *
		     (pwndDialog->arcWindow.ayBottom - pwndDialog->arcWindow.ayTop +
		     dayShadow) * 2);

		if (lpbSave == NULL)
			return IDCANCEL;		// no memory
		}

#endif	// DIALOG_NOSAVE

	fMenuEnabled = FEnableMenuBar(FALSE);

#ifndef DIALOG_NOSAVE

	/* Save window (ajust for shadow and DBCS padding) */
	SaveArc(pwndDialog->arcWindow.axLeft - daxDbcs,
	    pwndDialog->arcWindow.ayTop,
	    pwndDialog->arcWindow.axRight + daxShadow + daxDbcs,
	    pwndDialog->arcWindow.ayBottom + dayShadow, lpbSave);

#endif	// DIALOG_NOSAVE

#ifdef	DIALOG_NOSAVE
	if (pwndDlg2 == NULL)
		pwndDlg2 = pwndDialog;
#endif	// DIALOG_NOSAVE
	pwndDlg = pwndDialog;
	pwndRoot = pwndDialog;
	pfnDlg = pfn;
	pwndDialog->wParamEnd = 0;
	fShowDlgAccel = FALSE;
	fButtonAction = FALSE;
	pwndCapture = NULL;		/* clear the capture */

#ifdef WINDOW_OVERLAP
	RethinkDisplay();       	/* With new root */
#endif /* WINDOW_OVERLAP */

	pwndFocus = NULL;
	pfnFilterOld = pfnFilter;
#ifdef WINDOW_OVERLAP
	pfnOverlapFilterOld = pfnOverlapFilter;
#endif /*WINDOW_OVERLAP*/
	KillAlarm();			/* just in case */

	/* 3 stages to init :
	*	fDrawItem	fRedrawItem		state
	*	FALSE		FALSE			init (no drawing)
	*	TRUE		FALSE			first time painting
	*	TRUE		TRUE			normal processing
	*/
	fDrawItem = fRedrawItem = FALSE;
	SendMessageShort(pwndDialog, WM_INITDIALOG);

#ifdef DIALOG_LINE
	/* check for pushbutton on bottom line */
	{
	REGISTER WND *	pwnd;
	AY ay = pwndDialog->arcWindow.ayBottom - 3;

	fTwoLineDlg = FALSE;
	for (pwnd = pwndDialog->pwndChild; pwnd != NULL;
	    pwnd = pwnd->pwndSibling)
		{
		if (pwnd->arcWindow.ayTop == ay)
			{
			fTwoLineDlg = TRUE;
			break;
			}
		}
	}	

#endif /*DIALOG_LINE*/

	BeginDraw();

	fDrawItem = TRUE;
	DrawWindow(pwndDialog);
	fRedrawItem = TRUE;

	if (pwndFocus == NULL)
		{
		/* INITDIALOG message did not set focus => set to first item */
		SetFocus(pwndDialog);
		}

	pfnFilter = DialogFilterProc;
#ifdef WINDOW_OVERLAP
	pfnOverlapFilter = DummyFilter;
#endif /*WINDOW_OVERLAP*/
	if (FAltDown())
		HiliteDialogAccel();
		
	EndDraw();

	while (1)
		{
		PollKeyboard();

		while (PeekMessage(&msg))
			{
			/* check to see if the filter ended the dialog */
			if (pwndDialog->wParamEnd != 0)
				goto break_loop;
			if (msg.message == WM_CHAR && msg.wParam == VK_MENU)
				{
				/* ALT DOWN */
				if (!fShowDlgAccel)
					HiliteDialogAccel();
				}
#ifdef	MULTIPLE_ACTION
			else if (msg.message == WM_CHAR)
				{
				if (!(HIWORD(msg.lParam) & KK_MENU) ||
				    !FProcessAccel(msg.wParam))
				    {
				    if (DispatchMessage(&msg) == ((DWORD) FALSE))
				    	{
					    if (!FProcessAccel(msg.wParam))
							{
							/* Dialog Control did not eat key */
							if (HIWORD(msg.lParam) & KK_MENU)
								{
								/* Invalid ALT selection */
								Beep();
								}
							else
						        	SendMessage(pwndDialog, WM_CHAR,
									msg.wParam, msg.lParam);
							}
						}
				    }
			    }
			else
			    {
				DispatchMessage(&msg);
				}	
#else	/* MULTIPLE_ACTION */
			else if (DispatchMessage(&msg) == ((DWORD) FALSE) &&
			    msg.message == WM_CHAR)
				{
				if (!FProcessAccel(msg.wParam))
					{
					/* Dialog Control did not eat key */
					if (HIWORD(msg.lParam) & KK_MENU)
						{
						/* Invalid ALT selection */
						Beep();
						}
					else
						SendMessage(pwndDialog, WM_CHAR,
						    msg.wParam, msg.lParam);
					}
				}
#endif	/* MULTIPLE_ACTION */

			/* now check to see if it ended the dialog */
			if (pwndDialog->wParamEnd != 0)
				goto break_loop;
			}

		/* check for idle case */
		if (pwndDialog->wParamEnd != 0)
			goto break_loop;

		/* else idle */
#ifdef DUAL
		if (fProtectMode)
			{
			/* send idle message (will return TRUE if we should sleep */
			if (SendMessageShort(pwndDialog, WM_IDLE))
				{
				/* see if the idle message told us to die */
				if (pwndDialog->wParamEnd != 0)
					goto break_loop;
				/* sleep, wait for input */
				DosSemWait(hsemaMessage, -1L);
				}
			}
		else
			{
			SendMessageShort(pwndDialog, WM_IDLE);
			}
#else /*!DUAL*/
#ifndef DOS5
		SendMessageShort(pwndDialog, WM_IDLE);
#else
		/* send idle message (will return TRUE if we should sleep */
		if (SendMessageShort(pwndDialog, WM_IDLE))
			{
			/* see if the idle message told us to die */
			if (pwndDialog->wParamEnd != 0)
				goto break_loop;
			/* sleep, wait for input */
			DosSemWait(hsemaMessage, -1L);
			}
#endif
#endif /*!DUAL*/

		}
break_loop:
	Assert(pfnFilter == DialogFilterProc);
	pfnFilter = pfnFilterOld;
#ifdef WINDOW_OVERLAP
	Assert(pfnOverlapFilter == DummyFilter);
	pfnOverlapFilter = pfnOverlapFilterOld;
#endif /*WINDOW_OVERLAP*/

	/* restore old state of world */
	pwndRoot = pwndRootOld;
	pfnDlg = pfnDlgOld;
	pwndDlg = pwndDlgOld;

#ifdef DIALOG_LINE
	fTwoLineDlg = fTwoLineOld;
#endif /*DIALOG_LINE*/

#ifdef WINDOW_OVERLAP
	RethinkDisplay();
#endif /* WINDOW_OVERLAP */

	fShowDlgAccel = fShowDlgAccelOld;
	/* KLUDGE ALERT: restore old capture, if there was window with the
		capture then put in a few fake messages */
	if ((pwndCapture = pwndCaptureOld) != NULL)
		PostMouseUpMessages();

#ifndef DIALOG_NOSAVE

	/* Restore contents under dialog */
	DrawThisWnd(NULL);  /* Set cur window to NULL */
#ifdef KANJI
	fRestoreDbcs = TRUE;		/* for DBCS repaint */
#endif	/*KANJI*/
	RestoreArc(pwndDialog->arcWindow.axLeft - daxDbcs,
	    pwndDialog->arcWindow.ayTop,
	    pwndDialog->arcWindow.axRight + daxShadow + daxDbcs,
	    pwndDialog->arcWindow.ayBottom + dayShadow,
	    lpbSave);
	if (lpbWorkTemp == NULL)
		FreeWorkFar(lpbSave);
#ifdef KANJI
	fRestoreDbcs = FALSE;
#endif	/*KANJI*/

#else	// DIALOG_NOSAVE
	ScreenRedraw();
	pwndFocus = NULL;
	if (pwndDlg != NULL)
		{
		if (pwndDlg2 != pwndDlg && pwndDlg2 != NULL)
			DrawWindow(pwndDlg2);
		if (pwndDlg != pwndDialog)
			DrawWindow(pwndDlg);
		}
	else
		pwndDlg2 = NULL;
#endif	// DIALOG_NOSAVE

	FEnableMenuBar(fMenuEnabled);
	pwndFocus = pwndFocusOld;
	fButtonAction = FALSE;		/* clear for parent dialogs */
	UpdateCursor();
	KillAlarm();			/* just in case */

	return(pwndDialog->wParamEnd);
	}




STATIC PWND
PwndPreviousControl(pwndRoot, pwndControl)
/*
  -- Searches the list of children windows of pwndRoot for the window before
	pwndControl.
  -- If pwndControl is the first window in the list, find the last window
	in the list.
  -- Note: Assumes that there is at least 1 control in the dialog box.
*/
PWND pwndRoot;
REGISTER PWND pwndControl;
	{
	REGISTER PWND pwndCur = pwndRoot->pwndChild;

	if (pwndControl == pwndCur)
		pwndControl = NULL;
	while (pwndCur->pwndSibling != pwndControl)
		pwndCur = pwndCur->pwndSibling;
	return(pwndCur);
	}




BOOL FARPRIVATE
DialogFilterProc(pmsg)
/*
  -- filter procedure for Dialogs (filter characters)
	(ignore repeat counts most of the time)
*/
REGISTER PMSG pmsg;
	{
	REGISTER PWND pwnd;

	switch (pmsg->message)
		{
	default:
		return FALSE;
		/*break*/

	case WM_CHAR:

		pwnd = pwndDlg;
		/* Use LOBYTE of wParam since we
		   don't care about key translation */
		switch (LOBYTE(pmsg->wParam))
		{
		default:
			/* help if HELP key
			   (but not recursive or if using special buffer */
			if (pmsg->wParam == VK_HELP_KEY && lpbWorkTemp == NULL)
				{
				(*pfnDlg)(pwnd, WM_HELP,
					HIWORD(pmsg->lParam), 0L);
				return TRUE;
				}
			return FALSE;
	
		case LOBYTE(VK_RETURN):
			{
			PWND	pwndAction;
			PWND	pwndFocusOld;
	
			if ((pwndFocus->style & WS_TYPE) == WS_BUTTON &&
			    ((pwndFocus->style & WS_SUBSTYLE)
							== BS_PUSHBUTTON ||
			     (pwndFocus->style & WS_SUBSTYLE)
							== BS_DEFPUSHBUTTON))
				pwndAction = pwndFocus;
			else if ((pwndAction = PwndDefaultPushButton(pwnd))
							== NULL)
				return FALSE;
				/*break; let return through */
	

			if (!pwndAction->fEnabled)
				return FALSE;
				/*break: ignore if disabled */

			Assert(pwndFocus != NULL);
			SendMessageShort(pwndFocusOld=pwndFocus,WM_KILLFOCUS);
			pwndFocus = NULL;

			(*pfnDlg)(pwnd, WM_DIALOG, pwndAction->id,
			    MAKELONG(pwndFocus->id, 1));
	
			if (pwnd->wParamEnd == 0 && pwndFocus == NULL)
				{
				/* don't end the dialog yet => restore focus */
				SetFocus(pwndFocusOld);
				}
			}
			break;

		case LOBYTE(VK_ESCAPE):
			/* lParam == 1L => escape key */
			(*pfnDlg)(pwnd, WM_DIALOG, IDCANCEL, MAKELONG(0, 1));
			break;
	
		case LOBYTE(VK_TAB):
			{
			REGISTER PWND pwndCur = pwndFocus;
//
// a-emoryh - Default Shift+Tab handling was not working correctly in the new
//    FilePrint dialog, forcing me to process VK_TAB myself in this case.
//
         if ((pwndPrintEdit != NULL) &&
             (pwndFocus == pwndPrintEdit))
         {
            // Let the edit-control handle the tab.
            return FALSE;
         }

			do
				{
				if (HIWORD(pmsg->lParam) & KK_SHIFT)
					pwndCur =
					  PwndPreviousControl(pwnd, pwndCur);
				else
					{
					pwndCur = pwndCur->pwndSibling;
					if (pwndCur == NULL)
						pwndCur = pwnd->pwndChild;
					}
				}
			while (!SendMessageShort(pwndCur, WM_WANTFOCUS));
			SetFocus(pwndCur);
			UpdateCursor();
			}
			break;

			}
		break;

		}

	if (pwnd->wParamEnd != 0)
		{
		/* turn message into a harmless one */
		pmsg->pwnd = pwnd;
		pmsg->message = WM_ENDDIALOG;
		return FALSE;
		}

	UndoRepeat(pmsg->wParam, pmsg->lParam);	/* undo repeat for key */
	return TRUE;
	}



DWORD FARPUBLIC			/* WndProcs are PUBLIC */
DialogWndProc(pwnd, message, wParam, lParam)
/*
  -- Processes messages in a dialog.
   The Dialog Manager (this routine) receives, and interprets messages sent
   to a dialog. These messages may come from the system (key strokes, mouse
   movement, etc.), or from controls which are in the dialog box.
   The Dialog Manager keeps track of which control has the input focus
   (notifying the controls when they gain or loose the input focus).
   Some messages are passed on to the Dialog Specific Manager for further
   interpretation.
*/
REGISTER PWND pwnd;
WORD message, wParam;
DWORD lParam;
	{
	RRC rrc;
	switch (message)
		{

	case WM_PAINT:
		GetClientRrc(pwnd, &rrc);
		BeginDraw();
		FillRrc(pwnd, &rrc, ' ', isaDialogBox);
		DrawDialogBorder(pwnd);
		DrawThisWnd(NULL);      /* No window defined yet */
		ShadowArc(&pwnd->arcWindow);
		EndDraw();
		/* fall through to propagate message */

	default:
		return ((*pfnDlg)(pwnd, message, wParam, lParam));

	case WM_SETFOCUS:
		{
		REGISTER PWND pwndT = pwnd->pwndChild;
		while (!SendMessageShort(pwndT, WM_WANTFOCUS))
			{
			pwndT = pwndT->pwndSibling;
			AssertSz(pwndT, "No control can receive focus");
			}
		Assert(SendMessageShort(pwndT, WM_WANTFOCUS));
		SendMessageShort(pwndFocus = pwndT, WM_SETFOCUS);
		UpdateCursor();
		}
		break;

		}
	return(0L);
	}




VOID FARPRIVATE
EndDialog(pwnd, wParam)
REGISTER PWND pwnd;
WORD wParam;
/*	This routine is called to terminate a dialog. Quit messages should
   not be posted, it is done through this routine. The reason is to insure
   that only 1 quit message appears in the queue at a time, thus preventing
   the case were the quit message handling is slower then the keyboard and
   we are quited right out of qb. */
	{
	REGISTER PWND pwndCtl = pwnd->pwndChild;

	Assert(wParam != 0);
	Assert(pwnd == pwndDlg);
	if (pwnd->wParamEnd == 0)
		{
		/* free up stuff used by list boxes */
		while (pwndCtl)
			{
			if (pwndCtl->style & WS_LISTBOX)
				SendMessageShort(pwndCtl, LB_RESETCONTENT);
			pwndCtl = pwndCtl->pwndSibling;
			}

		pwnd->wParamEnd = wParam;
		}
	}



STATIC BOOL
FProcessAccel(wParam)
/*
  -- process a key event that may be an accelerator
  -- return TRUE if processed (i.e. key eaten)
  -- NOTE : we only find first match (accelerators MUST be unique) !!!
  -- NOTE : accelerators must be alphabetic
*/
WORD wParam;	/* wParam of WM_CHAR (ch or vk) */
	{
	/* walk the tree looking for a match */
	REGISTER PWND pwnd;
	REGISTER BYTE chTest;
#ifdef ACCEL_MULTIPLE
	WORD	cwndMatch;
#endif /*ACCEL_MULTIPLE*/
#ifdef KANJI
	char	chOther;		/* alternate Roman/Kana */
#endif /*KANJI*/

	Assert(PwndChild(pwnd) != NULL);

	if (wParam > VK_MIN)
		{
		if (wParam == VK_RIGHT)
			wParam = chRightArrow;
		else if (wParam == VK_LEFT)
			wParam = chLeftArrow;
		else if (wParam < VK_0 || wParam > VK_Z)
			{
			/* probably not a valid accelerator */
			return FALSE;
			}
		}

	chTest = LOBYTE(wParam);
	if (chTest >= 'a' && chTest <= 'z')
		chTest -= 'a' - 'A';
	else if ((unsigned) chTest > 0x7f)
		chTest = ChUpperFromChExt(chTest);

#ifdef KANJI
	chOther = ChAlternateKeytop(chTest);
#endif /*KANJI*/

#ifdef ACCEL_MULTIPLE
	/* test for multiple accelerators */
	cwndMatch = 0;

	for (pwnd = PwndChild(pwndDlg); pwnd != NULL; pwnd = PwndSibling(pwnd))
		{
		if (pwnd->aclDialog != aclNil)
			{
			char	ch = ChAccel(pwnd);

			if (ch >= 'a' && ch <= 'z')
				ch -= 'a' - 'A';
			else if ((unsigned) ch > 0x7f)
				ch = ChUpperFromChExt(ch);

			if (ch == chTest)
				cwndMatch++;
			}
		}
	if (cwndMatch == 0)
		return FALSE;
#endif /*ACCEL_MULTIPLE*/

	/* go next from focus */
	Assert(pwndFocus != NULL && PwndParent(pwndFocus) == pwndDlg);

	pwnd = pwndFocus;
	do
		{
		if ((pwnd = PwndSibling(pwnd)) == NULL)
			pwnd = PwndChild(pwndDlg);	/* wrap */

		if (pwnd->aclDialog != aclNil)
			{
			char	ch = ChAccel(pwnd);

			if (ch >= 'a' && ch <= 'z')
				ch -= 'a' - 'A';

#ifndef KANJI
			if (ch == chTest)
#else
			if (ch == chTest || ch == chOther)
#endif
				{
				/* we got one */
				if ((pwnd->style & WS_TYPE) == WS_STATIC)
					{
					/* pointing to preceding text,
					    skip 1 */
					pwnd = PwndSibling(pwnd);
					}

				/* can't have 2 text in a row */
				Assert((pwnd->style & WS_TYPE) != WS_STATIC);

				/* set focus if it will let us demand it */
				if (SendMessage(pwnd, WM_WANTFOCUS, 1, 0L))
					{
					SetFocus(pwnd);
					/* activate enabled items */
#ifdef ACCEL_MULTIPLE
					if (cwndMatch > 1)
						return TRUE;	/* just move */
#endif /*ACCEL_MULTIPLE*/

					/* Activate it if enabled */
					if (pwnd->fEnabled)
						SendMessageShort(pwnd, WM_MAKEACTIVE);
					return TRUE;
					}

				}
			}
		}
	while (pwnd != pwndFocus);

	/* no luck */
	return FALSE;
	}



VOID FARPUBLIC
SetDialogCaption(hdlg, sz)
/*
  -- modify the caption of a dialog
  -- hdlg is not currently used (for modeless dialogs)
*/
HANDLE hdlg;
char *sz;
	{
	AssertSz(hdlg == NULL, "SetDialogCaption : hdlg != NULL");
	AssertSz(pwndDlg != NULL, "SetDialogCaption : no modal dialog");
	
	pwndDlg->szDialog = (WORD) sz;
	DrawDialogBorder(pwndDlg);
	}



STATIC VOID
DrawDialogBorder(pwnd)
/*
  -- draw the border around a dialog
*/
REGISTER PWND pwnd;
	{
	DrawBorder(pwnd, &boxSingle, isaDialogBox, (char *) pwnd->szDialog);
#ifdef DIALOG_LINE
	{
	/* draw the line near the bottom of the Dialog Box */
	AY	ay;
	RRC	rrc;

	GetClientRrc(pwnd, &rrc);
	if (fTwoLineDlg)
		rrc.ryBottom--;
	rrc.ryTop = (rrc.ryBottom -= dayBorder) - dayBorder;
	ay = AyOfRy(pwnd, rrc.ryTop);
	FillRrc(pwnd, &rrc, chTopSide1, isaDialogBox);
	CharOutAbs(pwnd->arcWindow.axLeft, ay, chMiddleLeft1, isaDialogBox);
	CharOutAbs(pwnd->arcWindow.axRight - daxBorder, ay, chMiddleRight1, isaDialogBox);
	}
#endif /*DIALOG_LINE*/
	}



VOID FARPUBLIC
HiliteDialogAccel()
/*
  -- hilite dialog accelerators
*/
	{
	REGISTER PWND pwndT;

	fShowDlgAccel = TRUE;
	/* paint all dialog accelerator windows */
	for (pwndT = PwndChild(pwndDlg);
	    pwndT != NULL;
	    pwndT = PwndSibling(pwndT))
		{
		if (pwndT->aclDialog != aclNil)
			{
			BeginDraw();
			DrawWindow(pwndT);
			EndDraw();
			}
		}
	}



WORD FARPRIVATE
CwSizeDialog(parc)
/*
  -- return size of rectangle needed for dialog
  -- adjust for fFontAvailable and shadow
*/
REG ARC * parc;
	{
	WORD	cw;

	cw = (parc->axRight-parc->axLeft+daxShadow) *
	    (parc->ayBottom-parc->ayTop+dayShadow);

#ifdef SCREEN_FFONT
	if (fFontAvailable)
		cw *= 2;
#endif /*SCREEN_FFONT*/

	return cw;
	}
