/*
	COW : Character Oriented Windows
	(COW USER DIALOG)

	sedit.c : simple (single-line) edit wnd proc
*/

#define COW
#include <cow.h>

#include <uedit.h>
#include <uevent.h>
#include <udialog.h>
#include <vkey.h>
#include <uwindow.h>
#include <uscreen.h>
#include <uisa.h>
#include <uutil.h>
#include <kinput.h>
#include <kkeyboar.h>

#include "dialog.h"
#include "event.h"
#include "screen.h"
#include "util.h"
#include "overlap.h"

#include "edit.h"		/* includes "sedit.h" */
#include "_sedit.h"

#ifndef EDIT_FULLMGR

/* forward */
STATIC VOID DisplayEb(PWND);
STATIC BOOL FAddCh(PWND, char);
STATIC VOID DelCh(PWND, WORD);
STATIC VOID MoveCursorLeft(PWND);
STATIC VOID MoveCursorRight(PWND);
STATIC VOID DelChLeft(PWND);
STATIC VOID DelChRight(PWND);
STATIC VOID MoveCursorBegin(PWND);
STATIC VOID MoveCursorEnd(PWND);
STATIC VOID HiliteEditSel(PWND, BOOL);
STATIC VOID DeleteSelection(PWND, char *);
STATIC VOID PasteSz(PWND, char *);
STATIC VOID CopySelection(PWND, char *);



STATIC VOID
DisplayEb(pwnd)
/*
  -- display the text in the edit box
*/
REGISTER PWND pwnd;
	{
	WORD ichMac = pwnd->ichMacEb;
	WORD cch;
	RRC rrc;

	/* For fixed length edit items:
	   In order to prevent scrolling to the right due to cursor
	   or mouse moves, we reset the left edge here */
	if (pwnd->cchDialog != pwnd->cchMaxEb) pwnd->ichLeftEb = 0;

	GetClientRrc(pwnd,&rrc);
	cch = min( (WORD) rrc.rxRight, ichMac - pwnd->ichLeftEb);
#ifdef EDIT_SECRET
	/* Secret edit items for password protection */
	if (pwnd->style & ES_SECRET)
		{
		/* Fill edit item with chSecret */
		RRC rrcFill;
		rrcFill.rxLeft = rrcFill.ryTop = 0;
		rrcFill.ryBottom = 1;
		rrcFill.rxRight = (RX) cch;
		FillRrc(pwnd, &rrcFill, chSecret, pwnd->isaEb);
		}
	else
#endif /*EDIT_SECRET*/
	/*maybe*/TextOut(pwnd, 0, 0, (char *) pwnd->szDialog + pwnd->ichLeftEb,
	    cch, pwnd->isaEb);

	/* fill trailing width with fill character */
	rrc.rxLeft = (RX) cch;
	FillRrc(pwnd, &rrc, (char) pwnd->chFillDialog, pwnd->isaEb);
	}



STATIC BOOL
FAddCh(pwnd, ch)
/*
  -- add a character at the cursor position
*/
REGISTER PWND pwnd;
char ch;
	{
	WORD ichCursor = pwnd->ichCursorEb;
	short ichMac = pwnd->ichMacEb;
	char *sz = SzEdit(pwnd);
	RRC	rrc;
	RX rx;

	if (ichMac >= pwnd->cchDialog)
		return(FALSE);
	rx = (RX) (ichCursor - pwnd->ichLeftEb);
	if (ichMac >= pwnd->cchMaxEb)
		{
		if (ichCursor < pwnd->cchMaxEb)
			{
			sz[ichCursor] = ch;
			pwnd->ichCursorEb++;
			CharOut(pwnd, rx, 0, ch, pwnd->isaEb);
			}
		else
			{
			Beep();
			}
		return(TRUE);
		}
	GetClientRrc(pwnd,&rrc);
	if ((rx >= rrc.rxRight-1)&&(pwnd->cchDialog == pwnd->cchMaxEb))
		{
		pwnd->ichLeftEb++;
		BltRrc(pwnd, 0, 0, rrc.rxRight-1, 1, 1, 0);
		rx--;
		}
	if (ichCursor < ichMac)
		{
		bltbyte(sz+ichCursor, sz+ichCursor+1, ichMac-ichCursor);
		if (rx < rrc.rxRight-1)
			BltRrc(pwnd, rx+1, 0, rrc.rxRight-rx-1, 1, rx, 0);
		}
	sz[ichCursor] = ch;
#ifdef EDIT_SECRET
	if (pwnd->style & ES_SECRET)
		ch = chSecret;
#else
	AssertSz(!(pwnd->style & ES_SECRET), "Secret Edit Items not supported");
#endif /*!EDIT_SECRET*/
	CharOut(pwnd, rx, 0, ch, pwnd->isaEb);
	pwnd->ichMacEb++;
	pwnd->ichCursorEb++;
	return(TRUE);
	}



STATIC VOID
DelCh(pwnd, ichCur)
/*
  -- delete a character at the cursor position
  -- called by DelChLeft/DelChRight
*/
REGISTER PWND pwnd;
WORD ichCur;
	{
	char *sz = SzEdit(pwnd);
	WORD ichMac = pwnd->ichMacEb;

	pwnd->ichMacEb--;
	bltbyte(sz+ichCur+1, sz+ichCur, ichMac-ichCur-1);

	/* if room, make it look like we are 1 back from where we are
	*  (so that we will see an empty item only when it is really empty)
        */

	if (ichCur > 0)
		ichCur--;	/* try to fake it back one */
	if (ichCur < pwnd->ichLeftEb)
		pwnd->ichLeftEb = ichCur;

	DisplayEb(pwnd);
	}



STATIC VOID
MoveCursorLeft(pwnd)
/*
  -- move the cursor left; scroll if necessary
*/
REGISTER PWND pwnd;
	{
	short ichNew;

	if ((ichNew = pwnd->ichCursorEb-1) < 0)
		return;
	pwnd->ichCursorEb = ichNew;
	if (ichNew < pwnd->ichLeftEb)
		{
		pwnd->ichLeftEb = ichNew;
		DisplayEb(pwnd);
		}
	}



STATIC VOID
MoveCursorRight(pwnd)
/*
  -- move the cursor right; scroll if necessary
*/
REGISTER PWND pwnd;
	{
	RRC	rrc;
	WORD ichNew;
	WORD ichRight;

	GetClientRrc(pwnd,&rrc);
	ichRight = pwnd->ichLeftEb + rrc.rxRight;
	if ((ichNew = pwnd->ichCursorEb+1) > pwnd->ichMacEb)
		return;
	pwnd->ichCursorEb = ichNew;
	if (ichNew >= ichRight)
		{
		pwnd->ichLeftEb = ichNew - rrc.rxRight + 1;
		DisplayEb(pwnd);
		}
	}



STATIC VOID
DelChLeft(pwnd)
/*
  -- delete the character to the left of the cursor
*/
REGISTER PWND pwnd;
	{
	if (pwnd->ichCursorEb > 0)
		DelCh(pwnd, --pwnd->ichCursorEb);
	}



STATIC VOID
DelChRight(pwnd)
/*
  -- delete the character at the cursor
*/
REGISTER PWND pwnd;
	{
	if (pwnd->ichCursorEb < pwnd->ichMacEb)
		DelCh(pwnd, pwnd->ichCursorEb);
	}



STATIC VOID
MoveCursorBegin(pwnd)
/*
  -- move the cursor to the beginning of the edit buffer
  -- scroll as needed
*/
REGISTER PWND pwnd;
	{
	pwnd->ichCursorEb = 0;
	if (pwnd->ichLeftEb > 0)
		{
		pwnd->ichLeftEb = 0;
		DisplayEb(pwnd);
		}
	}



STATIC VOID
MoveCursorEnd(pwnd)
/*
  -- move the cursor to the end of the edit buffer
  -- scroll as needed
*/
REGISTER PWND pwnd;
	{
	REGISTER WORD ichCursor = pwnd->ichMacEb;
	WORD ichNewLeft;
	RRC	rrc;

	GetClientRrc(pwnd,&rrc);
	pwnd->ichCursorEb = ichCursor = pwnd->ichMacEb;
	if (ichCursor >= rrc.rxRight &&
	    (ichNewLeft = ichCursor - rrc.rxRight) >= pwnd->ichLeftEb)
		{
		pwnd->ichLeftEb = ichNewLeft+1;
		DisplayEb(pwnd);
		}
	}



STATIC VOID
HiliteEditSel(pwnd, fHilite)
/*
  -- hilite the current selection
*/
REGISTER PWND pwnd;
BOOL fHilite;
	{
	int ichCursor = (int) pwnd->ichCursorEb - pwnd->ichLeftEb;
	int ichSel = (int) pwnd->ichSelEb - pwnd->ichLeftEb;
	RRC rrc;

	GetClientRrc(pwnd, &rrc);
	rrc.rxLeft = (RX) ((ichSel < 0) ? 0 : ichSel);
	rrc.rxLeft = min( rrc.rxLeft, rrc.rxRight);
	rrc.rxRight = (RX) ((ichCursor < 0) ? 0 : ichCursor);

	if (ichSel >= ichCursor)
		{
		/* reverse rxLeft & right */
		RX rxT = rrc.rxLeft;
		rrc.rxLeft = rrc.rxRight;
		rrc.rxRight = rxT;
		}
	FillRrc(pwnd, &rrc, ' ', dmAttrOnly |
	    (fHilite ? pwnd->isaSelEb : pwnd->isaEb));
	}



STATIC VOID
DeleteSelection(pwnd, sz)
/*
  -- cut the current selection
  -- copying it to sz if it is not NULL
*/
REGISTER PWND pwnd;
char *sz;
	{
	WORD ichLeft, ichSel = pwnd->ichSelEb;
	WORD ichStart, ichEnd, ichMac = pwnd->ichMacEb;
	int ichCursor = pwnd->ichCursorEb;
	int cch;
	char *szStart, *szEnd;

	if (ichSel == ichCursor)
		return;
	else if (ichSel < ichCursor)
		{
		ichStart = ichSel;
		ichEnd = ichCursor-1;
		}
	else
		{
		ichStart = ichCursor;
		ichEnd = ichSel-1;
		}
	if (ichEnd >= ichMac)
		ichEnd = ichMac-1;
	cch = ichEnd - ichStart + 1;
	pwnd->ichMacEb -= cch;

	szStart = (char *) (pwnd->szDialog + ichStart);
	szEnd = (char *) (pwnd->szDialog + ichEnd);

	if (sz != NULL)
		{
		bltbyte(sz, szStart, cch);
		sz[cch] = '\0';
		}

	bltbyte(szEnd+1, szStart, ichMac-ichEnd-1);
	if (pwnd->ichLeftEb > ichEnd)
		pwnd->ichLeftEb = ichLeft - cch;
	else if (pwnd->ichLeftEb > ichStart)
		pwnd->ichLeftEb = ichStart;
	else if (pwnd->ichLeftEb == ichStart && ichStart != 0)
		pwnd->ichLeftEb = ichStart - 1;
	pwnd->ichCursorEb = ichStart;

	DisplayEb(pwnd);
	}



STATIC VOID
PasteSz(pwnd, szPaste)
/*
  -- paste the contents of szPaste at the cursor
*/
REGISTER PWND pwnd;
char *szPaste;
	{
	WORD cch = strlen(szPaste);
	WORD ichCursor = pwnd->ichCursorEb;
	char *sz = SzEdit(pwnd) + ichCursor;
	short cchShift = min(pwnd->cchDialog - ichCursor - cch,
			     min(pwnd->cchMaxEb - ichCursor - cch,
			     pwnd->ichMacEb - ichCursor));
	RRC rrc;
	WORD ichRight;

	GetClientRrc(pwnd,&rrc);
	ichRight = pwnd->ichLeftEb + rrc.rxRight;
	bltbyte(sz, sz+cch, cchShift);
	cch = min(cch, min(pwnd->cchDialog - ichCursor,
			pwnd->cchMaxEb - ichCursor));
	pwnd->ichMacEb += cch;
	pwnd->ichCursorEb +=cch;
	bltbyte(szPaste, sz, cch);
	if (pwnd->ichCursorEb > ichRight)
		pwnd->ichLeftEb += cch;
	DisplayEb(pwnd);
	}



STATIC VOID
CopySelection(pwnd, sz)
/*
  -- copy the current selection into the paste buffer (sz)
  -- assume buffer to be large enough
*/
REGISTER PWND pwnd;
char *sz;
	{
	WORD ichSel = pwnd->ichSelEb;
	WORD ichStart, ichEnd, ichMac = pwnd->ichMacEb;
	WORD cch, ichCursor = pwnd->ichCursorEb;

	if (ichSel != ichCursor)
		{
		if (ichSel < ichCursor)
			{
			ichStart = ichSel;
			ichEnd = ichCursor-1;
			}
		else
			{
			ichStart = ichCursor+1;
			ichEnd = ichSel;
			}
		ichEnd = min(ichMac-1, ichEnd);
		cch = ichEnd - ichStart + 1;
		bltbyte((char *) (pwnd->szDialog) + ichStart, sz, cch);
		sz[cch] = '\0';
		}
	else
		*sz = '\0';
	}



PRIVATE DWORD FARPUBLIC			/* WndProcs are PUBLIC */
InternalEditWndProc(pwnd, message, wParam, lParam)
/*
  -- the Edit Window Procedure
*/
REGISTER PWND pwnd;
WORD message, wParam;
DWORD lParam;
	{
	MSP	msp;
	RRC	rrc;
	BOOL	fResetSelection;
	WORD	ichRight;
	BOOL	fOk;	/* for return */

	Assert(!FFocus(pwnd) || pwnd == pwndFocus);

	GetClientRrc(pwnd,&rrc);

	fResetSelection = FALSE;
	ichRight = min(pwnd->ichLeftEb + rrc.rxRight - 1, pwnd->ichMacEb);

	switch(message)
		{
	default:
	/*case WM_ACTIVATE:*/
		return FALSE;
		/*break;*/

	case WM_PAINT:
		if (!pwnd->fNoBracketEb)
			{
			/* put brackets at start and end of edit item */
			/*  NOTE : brackets drawn outside the window !! */
			DrawThisWnd(NULL);
			CharOutAbs(pwnd->arcWindow.axLeft-1,
			    pwnd->arcWindow.ayTop, '[', pwnd->isaEb);
			CharOutAbs(pwnd->arcWindow.axRight,
			    pwnd->arcWindow.ayTop, ']', pwnd->isaEb);
			}
		DisplayEb(pwnd);
		break;

	case WM_LBUTTONDOWN:
		SetFocus(pwnd);
		HiliteEditSel(pwnd, FALSE);
		msp.lParam = lParam;
		if (msp.s.ax < pwnd->arcClipping.axLeft)
			pwnd->ichCursorEb = pwnd->ichLeftEb;
		else if (msp.s.ax > pwnd->arcClipping.axRight)
			pwnd->ichCursorEb = ichRight-1;
		else
			pwnd->ichCursorEb =
			    min(pwnd->ichMacEb, msp.s.rx+pwnd->ichLeftEb);
		pwnd->ichSelEb = pwnd->ichCursorEb;
		SetCapture(pwnd);
		fResetSelection = TRUE;
		break;

	case WM_MOUSEMOVE:
		if (FCaptured(pwnd))
			{
			HiliteEditSel(pwnd, FALSE);
			msp.lParam = lParam;
			if (msp.s.ax < pwnd->arcClipping.axLeft)
				{
				MoveCursorLeft(pwnd);
				SetAlarm(pwnd, ctickRepEdit);
				}
			else if (msp.s.ax >= pwnd->arcClipping.axRight)
				{
				MoveCursorRight(pwnd);
				SetAlarm(pwnd, ctickRepEdit);
				}
			else
				{
				pwnd->ichCursorEb =
				      min((WORD) msp.s.rx + pwnd->ichLeftEb,
				          pwnd->ichMacEb);
				}
			}
		break;

	case WM_ALARM:
		if (FCaptured(pwnd))
			{
			if (axMouse < pwnd->arcClipping.axLeft)
				{
				MoveCursorLeft(pwnd);
				SetAlarm(pwnd, ctickRepEdit);
				}
			else if (axMouse > pwnd->arcClipping.axRight)
				{
				MoveCursorRight(pwnd);
				SetAlarm(pwnd, ctickRepEdit);
				}
			}
		break;

	case WM_LBUTTONUP:
		if (FCaptured(pwnd))
			{
			ReleaseCapture();
			KillAlarm();
			}

		break;

	case WM_KILLFOCUS:
		HiliteEditSel(pwnd, FALSE);
		EnableCursor(pwnd, FALSE);
		SetFFocus(pwnd, FALSE);
		SendMessage(pwnd->pwndParent,WM_DIALOG_KILLFOCUS,pwnd->id,0L);
		if (FChanged(pwnd))
			SendMessage(pwnd->pwndParent, WM_DIALOG, pwnd->id,
			    MAKELONG((PWND) pwnd, EN_CHANGE));
		return TRUE;
		/*break;*/

	case WM_WANTFOCUS:
		return (pwnd->fEnabled); /* don't go to disabled edit item */
		/*break;*/

	case WM_SETFOCUS:
		SetFFocus(pwnd, TRUE);
		SetFChanged(pwnd, FALSE);
		SendMessage(pwnd->pwndParent,WM_DIALOG_SETFOCUS,pwnd->id,0L);
		if (pwnd->fRetainSelEb)
			break;	/* retain old selection */
		/* set selection to all of edit window */
		lParam = MAKELONG(0, ichSelectEnd);
		/* fall through to set selection */

	case EM_SETSEL:
		{
		REGISTER WORD ichLim;

		if ((ichLim = HIWORD(lParam)) == ichSelectEnd)
			{
			/* special case for select till end */
			ichLim = pwnd->ichMacEb;
			}

		AssertSz(LOWORD(lParam) <= ichLim && ichLim <= pwnd->ichMacEb,
		    "invalid Edit selection");

		HiliteEditSel(pwnd, FALSE);
		pwnd->ichCursorEb = ichLim;
		pwnd->ichSelEb = LOWORD(lParam);
		if (ichLim < pwnd->ichLeftEb || ichLim > ichRight)
			{
			/* selection outside visible region */
			WORD cchMac = rrc.rxRight;

			pwnd->ichLeftEb = (ichLim < cchMac) ? 0 :
			    ichLim - cchMac + cchMac / 8;
				/* show end of selection */
			DisplayEb(pwnd);
			}
		} /*EM_SETSEL*/
		break;

	case EM_GETSEL:
		{
		if (pwnd->ichCursorEb > pwnd->ichSelEb)
			return(MAKELONG(pwnd->ichSelEb, pwnd->ichCursorEb));
		else
			return(MAKELONG(pwnd->ichCursorEb, pwnd->ichSelEb));
		break;
		}

	/*
	*  NOTE : for WM_CUT, WM_PASTE and WM_COPY
	*	API specifies "lParam" contains far pointer,
	*	make sure it is in the DDS, then use near pointer
	*/

	case WM_CUT:
		HiliteEditSel(pwnd, FALSE);
		AssertSz(HIWORD(lParam) == HIWORD((char far *) pwnd),
		    "CUT buffer must be in default data segment");
		DeleteSelection(pwnd, (char *) LOWORD(lParam));
		SetFChanged(pwnd, TRUE);
		fResetSelection = TRUE;
		break;

	case WM_PASTE:
		HiliteEditSel(pwnd, FALSE);
		DeleteSelection(pwnd, NULL);
		fResetSelection = TRUE;
		AssertSz(HIWORD(lParam) == HIWORD((char far *) pwnd),
		    "PASTE buffer must be in default data segment");
		PasteSz(pwnd, (char *) LOWORD(lParam));
		SetFChanged(pwnd, TRUE);
		break;

	case WM_COPY:
		HiliteEditSel(pwnd, FALSE);
		AssertSz(HIWORD(lParam) == HIWORD((char far *) pwnd),
		    "COPY buffer must be in default data segment");
		CopySelection(pwnd, (char *) LOWORD(lParam));
		break;

	case WM_INSERT:
		goto insert_char;
		
	case WM_CHAR:
		{
		UndoRepeat(wParam, lParam);

		if (HIWORD(lParam) & KK_MENU)
			return FALSE;	/* ignore ALT keys */

		HiliteEditSel(pwnd, FALSE);
		if (FCaptured(pwnd))
			{
			ReleaseCapture();
			KillAlarm();
			}
		fResetSelection = TRUE;

		if (wParam >= wEditFirst && wParam <= wEditLast)
			{
			if (wParam == chDelete)
				{
				if (pwnd->ichCursorEb != pwnd->ichSelEb)
					DeleteSelection(pwnd, NULL);
				else
					DelChRight(pwnd);
				}
			else
				{
insert_char:
				/* fast case for simple typing */
				SetFChanged(pwnd, TRUE);
				DeleteSelection(pwnd, NULL);
				FAddCh(pwnd, (char) wParam);
				/* reset the selection */
				pwnd->ichSelEb = pwnd->ichCursorEb;
				SetFChanged(pwnd, TRUE);
				}
			}
		else
			{
			/* check for special keys */
			switch(wParam)
				{
			default:
				/* pass character back to Dialog manager */
				fOk = FALSE;
				goto restore_selection;

			case VK_LEFT:
				MoveCursorLeft(pwnd);
				if (HIWORD(lParam) & KK_SHIFT)
					fResetSelection = FALSE;
				break;

			case VK_RIGHT:
				MoveCursorRight(pwnd);
				if (HIWORD(lParam) & KK_SHIFT)
					fResetSelection = FALSE;
				break;

			case LOBYTE(VK_BACK):	/* translated */
				if (pwnd->ichCursorEb != pwnd->ichSelEb)
					DeleteSelection(pwnd, NULL);
				else if ((HIWORD(lParam) & KK_SHIFT) != 0)
					DelChRight(pwnd);
				else
					DelChLeft(pwnd);

				SetFChanged(pwnd, TRUE);
				break;

			case VK_HOME:
				MoveCursorBegin(pwnd);
				if (HIWORD(lParam) & KK_SHIFT)
					fResetSelection = FALSE;
				break;

			case VK_END:
				MoveCursorEnd(pwnd);
				if (HIWORD(lParam) & KK_SHIFT)
					fResetSelection = FALSE;
				}
			}
		} /*WM_CHAR*/
		break;

		}/*switch*/

	if (fResetSelection)
		pwnd->ichSelEb = pwnd->ichCursorEb;

	fOk = TRUE;

restore_selection:
#ifdef LATER
	// cursor should be visible during selections
	if (FFocus(pwnd))
		{
		WORD rx = max(pwnd->ichCursorEb - pwnd->ichLeftEb, 0);
		if (pwnd->ichCursorEb != pwnd->ichSelEb)
			HiliteEditSel(pwnd, TRUE);
		EnableCursor(pwnd, TRUE);
		MoveCursor(pwnd, (RX) rx, 0);
		}
#else
	if (FFocus(pwnd))
		{
		if (pwnd->ichCursorEb != pwnd->ichSelEb)
			{
			EnableCursor(pwnd, FALSE);
			HiliteEditSel(pwnd, TRUE);
			}
		else
			{
			WORD rx = max(pwnd->ichCursorEb - pwnd->ichLeftEb, 0);
			EnableCursor(pwnd, TRUE);
			MoveCursor(pwnd, (RX) rx, 0);
			}
		}
#endif /*!LATER*/

	return((DWORD) fOk);
	}



PUBLIC VOID FARPUBLIC
SetEditText(pwnd, sz, fDrawWindow)
REGISTER PWND pwnd;
char *sz;
BOOL fDrawWindow;
	{
	StartPublic();
	WORD cch;

	stringcpy((char *) pwnd->szDialog, sz, pwnd->cchMaxEb + 1);

	pwnd->ichMacEb = cch = strlen((char *) pwnd->szDialog);
	pwnd->ichLeftEb = 0;
	pwnd->ichCursorEb = 0;
	pwnd->ichSelEb = cch;
	if (fDrawWindow)
		DrawWindow(pwnd);
	StopPublic();
	}

PUBLIC VOID FARPUBLIC
SetEditWidth(pwnd, cch)
REGISTER PWND pwnd;
WORD cch;
	{
	StartPublic();

	pwnd->cchMaxEb = (cch >= 0) ? cch : pwnd->cchDialog;

	StopPublic();
	}

PUBLIC WORD FARPUBLIC
GetEditText(pwnd, sz, cchMac)
REGISTER PWND pwnd;
char *sz;
REGISTER WORD cchMac;
	{
	StartPublic();
	char *szDlg = (char *) pwnd->szDialog;

	if (cchMac > pwnd->ichMacEb + 1)	/* +1 for terminator */
		cchMac = pwnd->ichMacEb + 1;
	stringcpy(sz, szDlg, cchMac);

	StopPublic();
	return(cchMac);
	}



PUBLIC DWORD FARPUBLIC
EditWndProc(pwnd, message, wParam, lParam)
/*
  -- for edit windows
*/
PWND pwnd;
WORD wParam, message;
DWORD lParam;
	{
	StartPublic();

	DWORD lRet = InternalEditWndProc(pwnd, message, wParam, lParam);

	ReturnPublic(lRet, DWORD);
	}

#endif /* EDIT_FULLMGR */
