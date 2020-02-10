/*
	COW : Character Oriented Windows
	(COW USER DIALOG)

	button.c : button functions
*/

#define COW
#include <cow.h>

#include <uwindow.h>
#include <uevent.h>
#include <vkey.h>
#include <uisa.h>
#include <kinput.h>

#include "dialog.h"
#include "event.h"
#include "window.h"
#include "util.h"
#include "screen.h"

#include "button.h"
#include "_button.h"


/* data for rendering */
BTR btrRadioButton =
	{
	chRadioButtonOff,
	'?',		/* must load from run-time character */
	chRadioButtonPrefix,
	chRadioButtonSuffix,
	'\0',		/* no greyed radio buttons */
	};

BTR btrCheckBox =
	{
	chCheckBoxOff,
	chCheckBoxOn,
	chCheckBoxPrefix,
	chCheckBoxSuffix,
	chCheckBoxGrey,
	};

/* forward */

STATIC VOID	DrawButton(PWND);
STATIC VOID	DrawCheckBox(PWND, BTR *);
STATIC VOID	DrawPushButton(PWND);
STATIC VOID	DrawBracketBox(PWND, WORD);
STATIC VOID	DrawAccel(PWND, RX, WORD, BOOL);
STATIC VOID	DoButton(PWND);



PRIVATE DWORD FARPUBLIC			/* WndProcs are PUBLIC */
ButtonWndProc(pwnd, message, wParam, lParam)
/*
  -- WndProc for handling button windows
*/
PWND pwnd;
WORD message, wParam;
DWORD lParam;
	{
	MSP msp;
	BYTE fUp = FALSE;

	switch (message)
		{
	case WM_PAINT:
		DrawButton(pwnd);
		break;

	case WM_SETFOCUS:
		Assert((pwnd->style & WS_SUBSTYLE) != BS_GROUP);
		Assert(pwnd == pwndFocus);
		SendMessage(pwnd->pwndParent, WM_DIALOG_SETFOCUS, pwnd->id, 0L);
		DrawButton(pwnd);
		break;

	case WM_WANTFOCUS:
		switch (pwnd->style & WS_SUBSTYLE)
			{
		default:
			break;	/* return TRUE => accept focus */
		case BS_GROUP:
ReturnFalse:
			return((DWORD) FALSE);	/* never accept focus */
			/*break*/
		case BS_RADIOBUTTON:
			/* Accept focus if button is on, or demanding it */
			if (BstOfWnd(pwnd) == bstOn || wParam & 1)
				goto ReturnTrue;
			else if (FFirstButton(pwnd))
				{
				REGISTER PWND pwndT = pwnd;
				/* first button -> are we in ninch ?? */

				do
					{
					pwndT = PwndButtonNext(pwndT);
					if (BstOfWnd(pwndT) == bstOn)
						goto ReturnFalse;
					}
				while (pwndT != pwnd);
				/* ninch state -- accept for first button */
				goto ReturnTrue;
				}
			else
				goto ReturnFalse;

			return (wParam);
			/*NOTREACHED*/
			}
		break;

	case WM_KILLFOCUS:
		Assert((pwnd->style & WS_SUBSTYLE) != BS_GROUP);
		SendMessage(pwnd->pwndParent,WM_DIALOG_KILLFOCUS,pwnd->id, 0L);
		SetFButtonDown(pwnd, FALSE);
		Assert(pwnd == pwndFocus);
		pwndFocus = NULL;	/* special signal for losing focus */
		DrawButton(pwnd);
		pwndFocus = pwnd;	/* restore it properly */
		break;

	case WM_MAKEACTIVE:
		DoButton(pwnd);
		break;		/* return TRUE */

	case WM_CHAR:
		/* return FALSE if key ignored */
		UndoRepeat(wParam, lParam);
		switch (wParam)
			{
		default:
			return((DWORD) FALSE);
			break;

		case ' ':
			if (!FButtonDown(pwnd) && pwnd->fEnabled)
				{
				SetFButtonDown(pwnd, TRUE);
				DrawButton(pwnd);
				}
			break;

		case VK_UP:
		case VK_LEFT:
			fUp = TRUE;
			/* fall through */

		case VK_DOWN:
		case VK_RIGHT:
			switch (pwnd->style & WS_SUBSTYLE)
				{
			default:
				return((DWORD) FALSE);
				break;

			case BS_RADIOBUTTON:
				{
				/* move within a radio group */
				REGISTER PWND pwndT = pwnd;

				/* move if current button is on or disabled,
				* (i.e. stay if off and enabled)
				*/
				if (BstOfWnd(pwnd) == bstOn || !pwnd->fEnabled)
					{
					/* move to next or prev */
					do
						{
						pwndT = PwndButtonNext(pwndT);
						}
					while (fUp &&
					   PwndButtonNext(pwndT) != pwnd);
					}
				/* move focus - change selection */
				SetFocus(pwndT);
				Assert(pwnd->pwndParent == pwndT->pwndParent);
				if (pwndT->fEnabled)
					DoButton(pwndT);
				}
				break;

			case BS_CHECKBOX:
			case BS_AUTOCHECKBOX:
				/* checkbox change */
				if (fUp != (BYTE) BstOfWnd(pwnd) &&
				    pwnd->fEnabled)
					DoButton(pwnd);
				break;
				}
			break;
			}
		break;

	case WM_LBUTTONUP:
		if (FCaptured(pwnd))
			ReleaseCapture();
		if (fButtonAction)
			{
			fButtonAction = FALSE;
			DoButton(pwnd);
			}
		break;

	case WM_KEYUP:
		if (FButtonDown(pwnd) && wParam == VK_SPACE)
			DoButton(pwnd);
		break;

	case WM_MOUSEMOVE:
		if (!fButtonAction)
			break;		/* ignore first movement */
	case WM_LBUTTONDOWN:
	case WM_LBUTTONDBLCLK:
		fButtonAction = TRUE;
		if (wParam & MK_LBUTTON &&
		    (pwnd->style & WS_SUBSTYLE) != BS_GROUP)
			{
			/* if button down and mouse move */
			BOOL	fWasDown = FButtonDown(pwnd);

			msp.lParam = lParam;
			SetFButtonDown(pwnd, PtInRect((PRRC) &pwnd->arcWindow,
			    msp.s.ax, msp.s.ay));

			SetFocus(pwnd);	/* grab */

			if (FButtonDown(pwnd))
				{
				SetCapture(pwnd); /* trap all mouse events */
				if (fWasDown)
					goto ReturnTrue;	/* skip draw */
				}
			else
				{
				ReleaseCapture();
				    /* release mouse events to other windows */
				/* re-post the message to proper window */
				RePostMouseMessage(message, wParam, lParam);
				}

			DrawButton(pwnd);
			}
		break;
		}

ReturnTrue:
	return((DWORD) TRUE);
	}



STATIC VOID
DrawButton(pwnd)
/*
  -- draws a button (radio/push/checkbox) or groupbox
*/
REGISTER PWND pwnd;
	{
	switch (pwnd->style & WS_SUBSTYLE)
		{
	default:
		AssertSz(FALSE, "Invalid Button");
		break;

	case BS_RADIOBUTTON:
		btrRadioButton.chOn = chRadioButtonOn;	/* use run-time char */
		DrawCheckBox(pwnd, &btrRadioButton);
		break;

	case BS_CHECKBOX:
	case BS_AUTOCHECKBOX:
		DrawCheckBox(pwnd, &btrCheckBox);
		break;

	case BS_PUSHBUTTON:
	case BS_DEFPUSHBUTTON:
		DrawPushButton(pwnd);
		break;

	case BS_GROUP:
		DrawBorder(pwnd, &boxSingle, isaButton,
		    (char *) pwnd->szDialog);
		break;
		}
	}



STATIC VOID
DrawPushButton(pwnd)
/*
  -- Draw a pushbutton
*/
REGISTER PWND pwnd;
	{
	REGISTER WORD di;
	WORD	cch = CchRealLenSz((char *) pwnd->szDialog);
	BYTE	drx;
	RX	rxText;
	RRC	rrc;

	GetClientRrc(pwnd, &rrc);
	if (cch >= (drx = rrc.rxRight - rrc.rxLeft - daxPushButton))
		{
		/* button text too big (or just right) for window */
		rxText = daxPushButton / 2;
		cch = drx;
		}
	else
		{
		rxText = (RX) (((drx - cch + 1) / 2) + daxPushButton/2);
		}

	/* set cursor position */
	pwnd->axCursor = pwnd->arcClipping.axLeft + rxText;
#ifdef	KANJI
	pwnd->ayCursor = pwnd->arcClipping.ayTop;
#endif

	if (!fDrawItem)
		return;		/* don't actually draw yet */

	di = FButtonDown(pwnd) ? isaButtonDown :
	    ((pwnd->fEnabled) ? isaPushButton : isaButtonDisabled);

	FillRrc(pwnd, &rrc, ' ', di);

	DrawAccel(pwnd, rxText, di, !FButtonDown(pwnd));

	if (FButtonDown(pwnd))
		{
		/* button is down -> there better be no change in status */
		Assert(pwndFocus != NULL);
		/* Assert(pwndFocus == pwnd); -> removed because if you 
		   redraw button on a dlmKillFocus the focus is elsewhere */
		}
	else if (pwndFocus == NULL)
		{
		/* moving off a button (or init) -> restore to default */
		PWND pwndT;

		pwndT = PwndDefaultPushButton(PwndParent(pwnd));
		if (pwndT == pwnd)
			{
			/* losing focus, but we are the default */
			di = isaDialogAccel;	/* hilite this button */
			}
		else if (pwndT != NULL)
			{
			/* restore default button */
			Assert(pwndT->fEnabled);
			DrawBracketBox(pwndT, isaDialogAccel);
			}
		}
	else if (((pwndFocus->style & WS_TYPE) != WS_BUTTON ||
	     (((pwndFocus->style & WS_SUBSTYLE) != BS_PUSHBUTTON) &&
	      ((pwndFocus->style & WS_SUBSTYLE) != BS_DEFPUSHBUTTON))) &&
		(pwnd->style & WS_SUBSTYLE) == BS_DEFPUSHBUTTON)
		{
		/* focus is not a push button, but we are the default */
		di = isaDialogAccel;	/* hilite this button */
		}
	else if (pwnd == pwndFocus)
		{
		/* we are the focus, clear old default */
		PWND pwndT;

		pwndT = PwndDefaultPushButton(PwndParent(pwnd));
		if ((pwndT != pwnd)&&(pwndT != NULL))
			{
			Assert(pwndT->fEnabled);
			DrawBracketBox(pwndT, isaPushButton);
			}
		di = isaDialogAccel;	/* hilite this button */
		}
	DrawBracketBox(pwnd, di);

	/* cursor must be on for dialog tabbing on top line */
	Assert(pwnd->fCursorOn);
	Assert(pwnd->ayCursor = pwnd->arcClipping.ayTop);
	}



STATIC VOID
DrawBracketBox(pwnd, di)
/*
  -- draw brackets or box around a pushbutton
*/
REGISTER PWND pwnd;
WORD di;
	{
#ifdef BUTTON_LARGE
	BOX *	pbox;
	/* for large buttons, draw border instead */

	Assert(pwnd != NULL);
	pbox = &boxSingle;
	if (di == isaDialogAccel)
		{
		/* it has the focus => show as double border */
		pbox = &boxDouble;
		di = isaPushButton;
		}
	DrawBorder(pwnd, pbox, di, NULL);
#else
		{
		RRC rrc;

		Assert(pwnd != NULL);
		GetClientRrc(pwnd,&rrc);
		CharOut(pwnd, 0, 0, '<', di);
		CharOut(pwnd, rrc.rxRight-1, 0, '>', di);
		}
#endif /*BUTTON_LARGE*/
	}



STATIC VOID
DrawCheckBox(pwnd, pbtr)
/*
  -- draw a checkbox or radiobutton (depends on pbtr)
*/
REGISTER PWND pwnd;
REGISTER BTR *pbtr;
	{
	char ch;
	WORD di;

	di = (pwnd->fEnabled) ? isaButton : isaButtonDisabled;

	if ((BstOfWnd(pwnd)) == bstOff)
		ch = pbtr->chOff;
	else if (BstOfWnd(pwnd) == bstGreyed)
		ch = pbtr->chGrey;
	else
		ch = pbtr->chOn;

	BeginDraw();

	CharOut(pwnd, rxPrefix, 0, pbtr->chPrefix, di);
	CharOut(pwnd, rxButton, 0, ch, di);
	CharOut(pwnd, rxSuffix, 0, pbtr->chSuffix, di);

	DrawAccel(pwnd, rxButtonText, di, TRUE);

	EndDraw();

	pwnd->axCursor = pwnd->arcClipping.axLeft + rxButton;

	/* debug check that the cursor is in the correct position */
	Assert(pwnd->fCursorOn);
	Assert(pwnd->ayCursor == pwnd->arcClipping.ayTop);
	}



STATIC VOID
DrawAccel(pwnd, rx, di, fHilite)
/*
  -- draw text with optional accelerator in a window
  -- start at rx
*/
REGISTER PWND pwnd;
RX rx;
WORD di;
BOOL fHilite;		/* should we use isaDialogAccel ?? */
	{
	TextOut(pwnd, rx, 0, (char *) pwnd->szDialog, -1, di);

	if (pwnd->aclDialog == aclNil)
		return;

	if (fShowDlgAccel && pwnd->fEnabled)
		CharOut(pwnd, rx + IchAccel(pwnd), 0, ChAccel(pwnd),
		    fHilite ? isaDialogAccel : di);
	}




WORD FARPRIVATE
WButtonChecked(pwnd)
/*
  -- return state of button
  -- note : pwnd == window of the button !!
  -- for Radio/Push buttons, return TRUE if a button is "on"
  -- for checkboxes, return state (0,1 or 2 for ninch)
*/
PWND pwnd;
	{
	Assert(pwnd != NULL);
	return(BstOfWnd(pwnd));
	}



VOID FARPRIVATE
CheckRadioButton(pwndFirst, bnSet, fDisplay)
/*
  -- check a radiobutton in a group and uncheck all others in the group
  -- pwndFirst is the first button in the group (usually)
  -- bnSet is the button number to set (i.e. 0 based)
  -- if bnSet is invalid then no button will be set
  NOTE : we use the fact the uNinchRadio is -1 to get an invalid bn.
  NOTE : while we are at it, we assert that the button "id"s are sequential
  NOTE : if bnSet == 0, the pwndFirst does not have to be the first button
		in the group (and the order assertion is disabled).
*/
REGISTER PWND pwndFirst;
WORD bnSet;		/* index of button to set */
BOOL fDisplay;		/* TRUE => redraw it */
	 {
	REGISTER PWND pwnd = pwndFirst;
	Debug(WORD idCheck = pwndFirst->id);
	Debug(BOOL fDontCheckOrder = bnSet == 0);

	Assert(pwndFirst != NULL);
	Assert(bstOff == 0 && bstOn == 1);

	do
		{
		BYTE bst;
		AssertSz((pwnd->style & WS_SUBSTYLE) == BS_RADIOBUTTON,
		    "Invalid Radio Group");
		Assert(fDontCheckOrder || pwnd->id == idCheck++);

		bst = (BYTE) (bnSet-- == 0);
			/* 1 iff matching button */

		if (BstOfWnd(pwnd) != bst)
			{
			SetWndBst(pwnd, bst);
			if (fDisplay)
				DrawButton(pwnd);
			}
		pwnd = PwndButtonNext(pwnd);
		}
	while (pwnd != pwndFirst);
	}



VOID FARPRIVATE
CheckDlgButton(pwnd, bst, fDisplay)
/*
  -- set the state of a checkbox
  -- pwnd is a window pointer for a button
*/
REGISTER PWND pwnd;
WORD bst;
BOOL fDisplay;
	{
	Assert(pwnd != NULL);
	AssertSz(bst < bstMax, "Invalid button state");
	SetWndBst(pwnd, bst);
	if (fDisplay)
		DrawWindow(pwnd);
	}



STATIC VOID
DoButton(pwnd)
/*
  -- a button has been pushed
*/
REGISTER PWND pwnd;
	{
	SetFButtonDown(pwnd, FALSE);

	/* If auto then toggle the state */
	if (pwnd->style & BS_AUTO)
		{
		/* toggle button */
		Assert((pwnd->style & WS_SUBSTYLE) == BS_AUTOCHECKBOX);
		pwnd->wButton ^= bstOn;
		Assert((bstGreyed & bstOn) == 0);
		pwnd->wButton &= ~bstGreyed;
		}
	SendMessage(pwnd->pwndParent, WM_DIALOG, pwnd->id, 0L);
	DrawButton(pwnd);
	}



PWND FARPRIVATE
PwndDefaultPushButton(pwnd)
/*
  -- return the first default pushbutton in the dialog
*/
REGISTER PWND pwnd;
	{
	pwnd = pwnd->pwndChild;

	while (pwnd)
		{
		if ((pwnd->style & WS_TYPE) == WS_BUTTON &&
		   (pwnd->style & WS_SUBSTYLE) == BS_DEFPUSHBUTTON)
		        return(pwnd);
		pwnd = PwndSibling(pwnd);
		}
	return NULL;
	}
