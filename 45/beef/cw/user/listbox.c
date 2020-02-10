/*
	COW : Character Oriented Windows
	(COW USER DIALOG)

	listbox.c : list boxes

    Note : much of the math in the module is excessive (i.e. WORDs for BYTES)
    Note : all far pointer control is performed with limited-lifetime pointers
*/


#define COW
#include <cow.h>

#include <udialog.h>
#include <uscroll.h>
#include <uwindow.h>
#include <uscreen.h>
#include <uisa.h>
#include <uevent.h>
#include <vkey.h>
#include <kinput.h>
#include <kkeyboar.h>
#include <kmem.h>		/* kernel exports for memory management */

/* really integrated into SDM */
#include <sdmver.h>
#include <usdm.h>
#include <usdmtmpl.h>
#include "sdm.h"

#include "window.h"
#include "dialog.h"
#include "event.h"
#include "scroll.h"
#include "screen.h"
#include "util.h"
#include "case.h"

#include "listbox.h"
#include "_listbox.h"

#ifdef LISTBOX_LIMIT_SIZE
#define	cchListTextMax	64
#else
#define	cchListTextMax	256
#endif


#ifdef EXTRAS
#ifndef LISTBOX_HORIZ
#define LISTBOX_COLOR			/* listbox content may contain color */
#endif
#endif

/* REVIEW: move this to WINDOW.H !!! */
#ifdef EXTRAS
#define	chColorPrefix	((char) 0xfe)		/* => special color prefix */
#endif

/* forward */

STATIC VOID DisplayListBox(PWND);
STATIC VOID FillListBox(PWND, RY, RY, WORD);
STATIC VOID ScrollListBox(PWND, short, BOOL);
STATIC VOID ScrollHorizListBox(PWND, short);	/* LISTBOX_HORIZ only */
STATIC VOID HiliteListSel(PWND, BOOL);
STATIC VOID MoveSelection(PWND, WORD);
STATIC VOID MoveSelectionDown(PWND);
STATIC VOID MoveSelectionUp(PWND);
STATIC VOID MoveSelectionLeft(PWND, WORD);	/* LISTBOX_HORIZ only */
STATIC VOID MoveSelectionRight(PWND, WORD);	/* LISTBOX_HORIZ only */
STATIC VOID SetScrollWindow(PWND);
STATIC VOID ResetContent(PWND);
STATIC VOID AddListSz(PWND, char *, WORD);
STATIC VOID InsertSz(PWND, WORD, char *, BOOL, WORD);
STATIC VOID ReplaceSz(PWND, WORD, char *);
STATIC VOID DeleteSz(PWND, WORD, WORD);
STATIC VOID RevertToOomLb(PWND, WORD);
STATIC BOOL FLocateMatch(PWND, WORD);
STATIC VOID GetOnDemand(PWND, WORD, WORD FAR **, char FAR **, char *);
STATIC VOID FAR * LpvDeref(WORD);
#ifdef KANJI
STATIC int fdircmp(char FAR *, char FAR *);
STATIC int fjstrcmp(unsigned char FAR *, unsigned char FAR *);
#endif



PUBLIC DWORD FARPUBLIC			/* WndProcs are PUBLIC */
ListBoxWndProc(pwnd, message, wParam, lParam)
/*
  -- WndProc for List Boxes
  -- handles all messages from SDM and Dialog manager
*/
REG PWND pwnd;
WORD	message;
WORD	wParam;
DWORD	lParam;
	{
	WORD	wSelect;	/* indicates cause of selection */
	WORD	iszNew;
	REG WORD iszCur;
	MSP	msp;
	RRC	rrc;
	short	dryLb;			/* height of listbox */

#ifdef LISTBOX_HORIZ
	BOOL	fHoriz = pwnd->style & WS_HSCROLL;
#endif /*LISTBOX_HORIZ*/

	Assert(pwnd->iszCurLb != iszNil);

#ifndef LISTBOX_HORIZ
	Assert(pwnd->axCursor == AxOfRx(pwnd, rxListBoxMin));
#endif
	GetClientRrc(pwnd,&rrc);
	iszCur = FSelected(pwnd) ? pwnd->iszCurLb : iszNil;
	dryLb = rrc.ryBottom;
	wSelect = 0;

	switch(message)
		{
	default:
	/*case WM_ACTIVATE:*/
ReturnFalse:
		return((DWORD) FALSE);
		break;

	case WM_PAINT:
#ifdef DEBUG
#ifndef LISTBOX_ONELINE
		AssertSz(rrc.ryBottom - rrc.ryTop >= 2,
			    "Listbox too small");
#endif /*LISTBOX_ONELINE*/
#endif /*DEBUG*/
		DrawBorder(pwnd, &boxSingle, pwnd->isaColor, NULL);
		DisplayListBox(pwnd);
		goto ReturnTrue;

	case WM_WANTFOCUS:
		return((DWORD) pwnd->cszLb);	/* returns FALSE if empty */
		/*break;*/

	case WM_SETFOCUS:
		Assert(pwnd->cszLb != 0);
		if (pwnd->pwndParent != NULL)
			SendMessage(pwnd->pwndParent, WM_DIALOG_SETFOCUS,
					pwnd->id, 0L);
		goto ReturnTrue;

	case WM_KILLFOCUS:
		if (pwnd->pwndParent != NULL)
			SendMessage(pwnd->pwndParent, WM_DIALOG_KILLFOCUS,
					pwnd->id, 0L);
		goto ReturnTrue;

	case LB_SETCURSEL:
		if (wParam == iszNil)
			{
			/* unselect it */
			HiliteListSel(pwnd, FALSE);
			SetFSelected(pwnd, FALSE);
			}
		else if (wParam < pwnd->cszLb && wParam != iszCur)
			{
			MoveSelection(pwnd, wParam);	/* move+draw */
			}
		else
			{
			/* invalid selection: if debugging give a warning */
			AssertSz(wParam < pwnd->cszLb, "Invalid listbox selection");
			goto ReturnFalse;	/* selection not set */
			}
	
		goto ReturnTrue;
		/*break;*/

	case WM_MOUSEMOVE:
		/* ignore non-capture & client moves */
		if (!FCaptured(pwnd) || (wParam & MK_NONCLIENT))
			goto ReturnTrue;
		/* else : fall through */

	case WM_LBUTTONDOWN:
	case WM_LBUTTONDBLCLK:
		msp.lParam = lParam;
		/* listbox must be non-empty & left button down */
		if (pwnd->cszLb == 0 || !(wParam & MK_LBUTTON))
			goto ReturnFalse;

		wSelect |= lbrMouse;

		/* we should have the mouse captured */
		if (!FCaptured(pwnd))
			{
			SetCapture(pwnd);
			SetAlarm(pwnd, ctickRepScrollStart);
			}

		if (!(wParam & MK_NONCLIENT))
			{
			/* in middle of listbox */
			SetFocus(pwnd);
			iszNew = pwnd->iszTopLb + msp.s.ry;
#ifdef LISTBOX_HORIZ
			if (fHoriz)
				{
				// bump extra width
				/* which column are we in ? */
				iszNew += ((msp.s.rx - rxListBoxMin) /
				    (pwnd->drxItemLb + 1)) * dryLb;
				}
#endif /*LISTBOX_HORIZ*/
			if (iszNew >= pwnd->cszLb)
				{
				wSelect = lbrOther;
				if (FSelected(pwnd))
					wSelect |= flbrReselect;
				iszCur = iszNil;    /* force message */
				}

			else if (message == WM_LBUTTONDBLCLK &&
			    iszNew == iszCur)
				{
				/* double click selection */
				if (FCaptured(pwnd))
					{
					ReleaseCapture();
					KillAlarm();
					}
				SendMessage(pwnd->pwndParent, WM_DIALOG,
				    pwnd->id, MAKELONG(0, LBN_DBLCLK));
				goto ReturnTrue;
				}
			else
				{
				if (iszNew == iszCur)
					wSelect |= flbrReselect;
				iszCur = iszNil;    /* force message */
				MoveSelection(pwnd, iszNew);
				}
			}
		else if (msp.s.ay < pwnd->arcClipping.ayTop)
			MoveSelectionUp(pwnd);
		else if (msp.s.ay >= pwnd->arcClipping.ayBottom)
			MoveSelectionDown(pwnd);
#ifdef LISTBOX_HORIZ
		/* special horizontal scroll */
		else if (msp.s.ax < pwnd->arcClipping.axLeft)
			MoveSelectionLeft(pwnd, 1);
		else if (msp.s.ax >= pwnd->arcClipping.axRight)
			MoveSelectionRight(pwnd, 1);
#endif /*LISTBOX_HORIZ*/
		break;

	case WM_ALARM:
		if (FCaptured(pwnd))
			{
			wSelect |= lbrMouse;
			SetAlarm(pwnd, pwnd->ctickRepLb);
			if (ayMouse < pwnd->arcClipping.ayTop)
				{
				MoveSelectionUp(pwnd);
				/* fast repeat ? */
				if (ayMouse < pwnd->arcClipping.ayTop - 2)
					SetAlarm(pwnd, pwnd->ctickRepLb / 2);
				}
			else if (ayMouse >= pwnd->arcClipping.ayBottom)
				{
				MoveSelectionDown(pwnd);
				/* fast repeat ? */
				if (ayMouse >= pwnd->arcClipping.ayBottom + 2)
					SetAlarm(pwnd, pwnd->ctickRepLb / 2);
				}
#ifdef LISTBOX_HORIZ
			/* special horizontal scroll (1 speed) */
			else if (axMouse < pwnd->arcClipping.axLeft)
				MoveSelectionLeft(pwnd, 1);
			else if (axMouse >= pwnd->arcClipping.axRight)
				MoveSelectionRight(pwnd, 1);
#endif /*LISTBOX_HORIZ*/
			}
		break;

	case WM_LBUTTONUP:
		if (!FCaptured(pwnd))
			goto ReturnFalse;	/* ignore */
		ReleaseCapture();
		KillAlarm();
		SendMessage(pwnd->pwndParent, WM_DIALOG, pwnd->id,
			MAKELONG(0,LBN_SELECT_DONE));
		goto ReturnTrue;
		/* break; */

	case WM_VSCROLL:
		{
		short	dry = 0;

		if (pwnd->cszLb == 0)
			goto ReturnFalse; /* nothing for empty listboxes */

		wSelect |= lbrScroll;

		switch (wParam)
			{
		default:
			break;

		case SB_LINEDOWN:
			dry++;			/* 1 line down */
			Assert(dry == 1);
			break;

		case SB_LINEUP:
			dry--;			/* 1 line up */
			Assert(dry == -1);
			break;

		case SB_PAGEDOWN:
			dry = dryLb;
			break;

		case SB_PAGEUP:
			dry = -dryLb;
			break;

		case SB_THUMBPOSITION:
			dry = LOWORD(lParam) - pwnd->iszTopLb;
			break;

		case SB_UPCLICK:
			SendMessage(pwnd->pwndParent, WM_DIALOG, pwnd->id,
				MAKELONG(0,LBN_SELECT_DONE));
			break;

			}

		if (dry != 0)
			ScrollListBox(pwnd, dry, TRUE);
		}
		break;

#ifdef LISTBOX_HORIZ

	case WM_HSCROLL:
		{
		if (pwnd->cszLb == 0)
			goto ReturnFalse; /* nothing for empty listboxes */

		wSelect |= lbrScroll;

		/* move selection (since scrolling makes no sense */
		switch (wParam)
			{
		default:
			break;

		case SB_LINEDOWN:
			MoveSelectionRight(pwnd, 1);
			break;

		case SB_LINEUP:
			MoveSelectionLeft(pwnd, 1);
			break;

		case SB_PAGEDOWN:
			MoveSelectionRight(pwnd, pwnd->citemWidthLb);
			break;

		case SB_PAGEUP:
			MoveSelectionLeft(pwnd, pwnd->citemWidthLb);
			break;

		case SB_THUMBPOSITION:
			{
			WORD	iszNew;
			iszNew = LOWORD(lParam) * dryLb + iszCur % dryLb;

			if (iszNew >= pwnd->cszLb)
				iszNew = pwnd->cszLb - 1;
			MoveSelection(pwnd, iszNew);
			}
			break;

			}/*switch*/

		}
		break;
#endif /*LISTBOX_HORIZ*/

	case LB_RESETCONTENT:
		ResetContent(pwnd);
		goto ReturnTrue;
		/*break;*/

	case LB_ADDSTRING:
		AddListSz(pwnd, (char *) wParam, LOWORD(lParam));
		goto ReturnTrue;
		/*break;*/

	case LB_INSERTSTRING:
		InsertSz(pwnd, HIWORD(lParam), (char *) wParam, FALSE, LOWORD(lParam));
		goto ReturnTrue;
		/*break;*/

	case LB_REPLACESTRING:
		ReplaceSz(pwnd, HIWORD(lParam), (char *) wParam);
		goto ReturnTrue;
		/*break;*/

	case LB_DELETESTRING:
		DeleteSz(pwnd, HIWORD(lParam), LOWORD(lParam));
		goto ReturnTrue;
		/*break;*/

	case LB_GETCURSEL:
		return ((DWORD) iszCur);
		/*break;*/

	case LB_GETCOUNT:
		return ((DWORD) pwnd->cszLb);
		/*break;*/

	case LB_GETTEXT:
		return ((DWORD) GetListText(
			pwnd, (char *) wParam, HIWORD(lParam)));
		/*break;*/

#ifdef LISTBOX_HORIZ
	case LB_SETWIDTH:
		/* set # item width for horizontal listboxes
		   (wParam == citem) -- usually sent before repainting */
		Assert(wParam != 0);

		pwnd->citemWidthLb = wParam;
		/* 1 extra at start and after each row */
		Assert(rxListBoxMin == 1);
		pwnd->drxItemLb = ((rrc.rxRight - 1) / wParam) - 1;

		/* KLUDGE: convert vertical listbox into horizontal */
		if (pwnd->style & WS_VSCROLL)
			{
			REG PWND pwndScrl;

			pwndScrl = PwndChild(pwnd);
			Assert(pwndScrl != NULL);
			Assert(pwndScrl->style & SBS_VERT);

			// switch scroll bar position (to bottom)
			pwndScrl->arcWindow.axLeft = pwnd->arcWindow.axLeft +
			    daxBorder;
			pwndScrl->arcWindow.axRight = pwnd->arcWindow.axRight -
			    daxBorder;
			pwndScrl->arcWindow.ayTop =
			   (pwndScrl->arcWindow.ayBottom =
			    pwnd->arcWindow.ayBottom) - dayBorder;

			// switch style & validate
			pwndScrl->style = (pwndScrl->style & ~SBS_VERT) | SBS_HORZ;
			ValidateWindow(pwndScrl);

			// switch parent style
			pwnd->style = (pwnd->style & ~WS_VSCROLL) | WS_HSCROLL;
			ValidateWindow(pwnd);
			}
		goto ReturnTrue;
#endif /*LISTBOX_HORIZ*/

	case WM_CHAR:
		/* return FALSE if key ignored */
		if ((HIWORD(lParam) & KK_MENU) || (!pwnd->fEnabled))
			goto ReturnFalse;	/* pass ALT keys through */

		UndoRepeat(wParam, lParam);

		wSelect |= lbrKeys;

		switch(wParam)
			{
		default:
			/* we should eat all non-control ascii keys
			* (they may or may not do anything)
			* control keys and non-ascii will not be eaten
			*/
			if (!FLocateMatch(pwnd, wParam) &&
			   (wParam < 0x20 || wParam >= VK_MIN))
				{
				/* control or non-ascii did not match */
				goto ReturnFalse;
				}
			break;

		case VK_LEFT:
#ifdef LISTBOX_HORIZ
			MoveSelectionLeft(pwnd, 1);
			break;
#endif /*LISTBOX_HORIZ*/

		case VK_UP:
			MoveSelectionUp(pwnd);
			break;

		case VK_RIGHT:
#ifdef LISTBOX_HORIZ
			MoveSelectionRight(pwnd, 1);
			break;
#endif /*LISTBOX_HORIZ*/

		case VK_DOWN:
			MoveSelectionDown(pwnd);
			break;

		case VK_NEXT:
#ifdef LISTBOX_HORIZ
			if (fHoriz)
				MoveSelectionRight(pwnd, pwnd->citemWidthLb);
			else
#endif /*LISTBOX_HORIZ*/
				ScrollListBox(pwnd, dryLb, TRUE);
			break;

		case VK_PRIOR:
#ifdef LISTBOX_HORIZ
			if (fHoriz)
				MoveSelectionLeft(pwnd, pwnd->citemWidthLb);
			else
#endif /*LISTBOX_HORIZ*/
				ScrollListBox(pwnd, -dryLb, TRUE);
			break;

		case ' ':
			wSelect = flbrReselect | lbrSpace;
			/* force message for change of selection */
			iszCur = iszNil;	/* force a change */
			HiliteListSel(pwnd, TRUE);
			break;

		case VK_HOME:
			if (pwnd->cszLb != 0)
				MoveSelection(pwnd, iszMin);
			break;

		case VK_END:
			if (pwnd->cszLb != 0)
				MoveSelection(pwnd, pwnd->cszLb - 1);
			break;

			}
		break;
		}

	if (FSelected(pwnd) && iszCur != pwnd->iszCurLb)
		{
		/* tell parent that selection has changed */
		Assert((wSelect & lbrCause) != lbrNone)
		SendMessage(pwnd->pwndParent, WM_DIALOG, pwnd->id,
		    MAKELONG(wSelect, LBN_SELCHANGE));
		}

ReturnTrue:
	return((DWORD) TRUE);	/* default case */
	}



STATIC VOID
DisplayListBox(pwnd)
/*
  -- display the contents of the list box
*/
REG PWND pwnd;
	{
	RRC	rrc;

	Assert(fDrawItem);

	GetClientRrc(pwnd, &rrc);
	FillRrc(pwnd, &rrc, ' ', pwnd->isaColor);

	if (pwnd->cszLb > 0)
		FillListBox(pwnd, 0, rrc.ryBottom, pwnd->iszTopLb);

	SetScrollWindow(pwnd);
	}



STATIC VOID
SetScrollWindow(pwnd)
/*
  -- sets the scroll range and position
  -- vertical is simple, horizontal is a pain
*/
REG PWND pwnd;
	{
	RRC	rrc;
	/* check for no scrollbar */
#ifdef LISTBOX_ONELINE
	if (!(pwnd->style & (WS_VSCROLL | WS_HSCROLL)))
		return;
#else
	Assert(pwnd->style & (WS_VSCROLL | WS_HSCROLL));
#endif /*LISTBOX_ONELINE*/

	GetClientRrc(pwnd,&rrc);
#ifdef LISTBOX_HORIZ
	if (pwnd->style & WS_HSCROLL)
		{
		REG short colMac;
		BYTE	dry;

		dry = rrc.ryBottom;
		colMac = (pwnd->cszLb - 1) / dry + 1;

		SetScrollRange(pwnd->pwndChild, 0, colMac, FALSE);
		SetScrollPos(pwnd->pwndChild, pwnd->iszCurLb / dry, TRUE);
		}
	else
#endif	/*LISTBOX_HORIZ*/
		{
		// vertical listbox case
		REG short iszBot;

		iszBot = pwnd->cszLb - rrc.ryBottom;
		if (iszBot < 1)
			iszBot = 1;	/* set range to 1 */

		SetScrollRange(pwnd->pwndChild, 0, iszBot, FALSE);
		SetScrollPos(pwnd->pwndChild, pwnd->iszTopLb, TRUE);
		}
	}



STATIC VOID
FillListBox(pwnd, ryTop, ryBottom, isz)
/*
  -- fill in the specified lines in the listbox
  -- ryTop is the top line, ryBottom is the bottom line
  -- isz is the isz corresponding to the top line
*/
REG PWND pwnd;
RY	ryTop;
RY	ryBottom;
WORD	isz;
	{
	char	sz[cchListTextMax];
	char FAR * lgpsz;
	WORD FAR * lmpiszoff;
	REG RY	ry;
#ifdef LISTBOX_HORIZ
	WORD	ccol;			/* # of columns */
	RX	rxLeft;			/* current left position */
#endif

	/* lock down string buffer and indexes (handles may be NULL) */
	lgpsz = LpvDeref(pwnd->hmemGpszLb);
	lmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);

	BeginDraw();

#ifdef LISTBOX_HORIZ
	/* Horizontal : fill in vertical list, then go to the right */
	ccol = pwnd->citemWidthLb;
	Assert(ccol != 0);
	rxLeft = RxLeftItem(pwnd, 0);
	while (ccol--)
		{
		WORD	cch;
#endif /*LISTBOX_HORIZ*/
		for (ry = ryTop; ry < ryBottom; ry++)
			{
			WORD	off;

			if (isz >= pwnd->cszLb)
				{
#ifndef LISTBOX_HORIZ
				break;		/* gone too far */
#else
				cch = 0;
				goto BlankFillItem;
#endif /*LISTBOX_HORIZ*/
				}

			if (pwnd->hmemGpszLb != NULL &&
			    (off = lmpiszoff[isz]) != offEmpty)
				{
				/* valid cached string */
				fstrcpy((char FAR *) sz, lgpsz+off);
				}
			else
				{
				/* not in cache */
				GetOnDemand(pwnd, isz, &lmpiszoff, &lgpsz, sz);
				}

#ifndef LISTBOX_HORIZ
			TextOut(pwnd, rxListBoxMin, ry, sz, -1, pwnd->isaColor);
#else
			/* display the string in proper column */
			if ((cch = CchRealLenSz(sz)) > pwnd->drxItemLb)
				cch = pwnd->drxItemLb;
			TextOut(pwnd, rxLeft, ry, sz, cch, pwnd->isaColor);
BlankFillItem:
			if (cch != pwnd->drxItemLb)
				{
				/* fill remaining part of line */
				RRC	rrc;

				rrc.rxLeft = rxLeft + cch;
				rrc.rxRight = rxLeft + pwnd->drxItemLb;
				rrc.ryBottom = (rrc.ryTop = ry) + 1;
				FillRrc(pwnd, &rrc, ' ', pwnd->isaColor);
				}
#endif /*LISTBOX_HORIZ*/

			/* invert the listbox selection */
			if (isz == pwnd->iszCurLb && FSelected(pwnd))
				{
				/* selected */
				RRC	rrc;

#ifndef LISTBOX_HORIZ
				/* entire width of listbox */
				GetClientRrc(pwnd, &rrc);
#else
				/* just this item with 1 before & after */
				rrc.rxRight = (rrc.rxLeft = rxLeft - 1) +
				   pwnd->drxItemLb + 2;
#endif
				rrc.ryBottom = (rrc.ryTop = ry) + 1;
				FillRrc(pwnd, &rrc, '\0', DiMake(dmAttrOnly,
				    pwnd->isaHiliteColor));
				}

			isz++;
			}
#ifdef LISTBOX_HORIZ
		rxLeft += (BYTE) pwnd->drxItemLb + 1;
		}/*end while(ccol--); */
#endif

	EndDraw();

	/* unlock string buffers and indexes */
	/*GlobalUnlock(pwnd->hmemGpszLb);*/
	/*GlobalUnlock(pwnd->hmemMpiszoffLb);*/
	}



STATIC VOID
ScrollListBox(pwnd, dry, fKillOld)
/*
  -- scroll vertical listbox by dry lines (may be negative)
  -- if hit top show top, if hit bottom show bottom
*/
REG PWND pwnd;
short	dry;
BOOL	fKillOld;	/* TRUE => kill old selection */
	{
	WORD	iszTop;
	RRC	rrc;
	WORD	iszBottom;
	short	iszNewTop;
	short	iszNewBottom;	/* note : use for signed compares */
	WORD	ryBottom;

#ifdef LISTBOX_HORIZ
	if (pwnd->style & WS_HSCROLL)
		{
		// horizontal listbox
		Assert(!fKillOld);
		ScrollHorizListBox(pwnd, dry);
		return;
		}
#endif /*LISTBOX_HORIZ*/
	iszTop = pwnd->iszTopLb;

	/* optionally turn off old selection */
	if (fKillOld)
		HiliteListSel(pwnd, FALSE);	/* turn off old */

	GetClientRrc(pwnd, &rrc);
	ryBottom = rrc.ryBottom;
	iszBottom = iszTop + ryBottom - 1;
	iszNewTop = iszTop + dry;
	iszNewBottom = iszBottom + dry;

	/* see if we should stop at top or bottom */
	if (iszNewTop < 0)
		{
		iszNewTop = iszMin;
		iszNewBottom = ryBottom - 1;
		}
	else if (iszNewBottom >= pwnd->cszLb)
		{
		iszNewBottom = pwnd->cszLb - 1;
		if ((iszNewTop = iszNewBottom - (ryBottom - 1)) < 0)
			iszNewTop = iszMin;
		}

	dry = iszNewTop - iszTop;
	/* move selection if necessary */
	if (pwnd->iszCurLb > iszNewBottom)
		pwnd->iszCurLb = iszNewBottom;
	else if (pwnd->iszCurLb < iszNewTop)
		pwnd->iszCurLb = iszNewTop;

	pwnd->iszTopLb = iszNewTop;

	/* smooth scroll */
	if (iszNewTop > iszTop && iszNewTop < iszBottom)
		{
		BltRrc(pwnd, 0, 0, rrc.rxRight, (BYTE) (ryBottom - dry),
		    0, (BYTE) dry);
		rrc.ryTop = (RY) (ryBottom-dry);
		FillRrc(pwnd, &rrc, ' ', pwnd->isaColor);
		FillListBox(pwnd, rrc.ryTop, (RY) ryBottom, iszNewTop+rrc.ryTop);
		}
	else if (iszNewBottom > iszTop && iszNewBottom < iszBottom)
		{
		BltRrc(pwnd, 0, (RY) (0-dry), rrc.rxRight, (BYTE) (ryBottom + dry),
		    0, 0);
		rrc.ryBottom = (RY) (0-dry);
		FillRrc(pwnd, &rrc, ' ', pwnd->isaColor);
		FillListBox(pwnd, 0, (RY) (0-dry), iszNewTop);
		}
	else if (iszNewTop != iszTop)
		{
		/* outside range to scroll => redraw it */
		DisplayListBox(pwnd);
		}

	SetScrollWindow(pwnd);
	HiliteListSel(pwnd, TRUE);
	}


#ifdef LISTBOX_HORIZ

STATIC VOID
ScrollHorizListBox(pwnd, dcol)
/*
  -- scroll a horizontal listbox by dcol columns (may be negative)
  -- if hit top show first column, if hit bottom show last column
*/
REG PWND pwnd;
short	dcol;
	{
	WORD	iszTop;
	BYTE	dry;
	RRC	rrc;
	short	iszNewTop;	/* note : use for signed compares */

	Assert(pwnd->style & WS_HSCROLL);


	GetClientRrc(pwnd,&rrc);
	iszTop = pwnd->iszTopLb;
	dry = rrc.ryBottom;
	Assert(iszTop % dry == 0);
	
	iszNewTop = iszTop + dcol * dry;
	if (iszNewTop < 0)
		{
		pwnd->iszCurLb = iszNewTop = iszMin;
		}
	else if (iszNewTop >= pwnd->cszLb)
		{
		/* adjust to right hand side */
		iszNewTop = ((pwnd->cszLb - 1) / dry) * dry;
		pwnd->iszCurLb = pwnd->cszLb - 1;	/* select last */
		}

	pwnd->iszTopLb = iszNewTop;

	if (iszNewTop != iszTop)
		FillListBox(pwnd, 0, dry, pwnd->iszTopLb = iszNewTop);
	SetScrollWindow(pwnd);

	HiliteListSel(pwnd, TRUE);
	}
#endif /*LISTBOX_HORIZ*/



STATIC VOID
HiliteListSel(pwnd, fHilite)
/*
  -- highlight/unhighlight the iszCur in the listbox.
  -- if iszCur is not displayed force it to be displayed
*/
REG PWND pwnd;
BOOL	fHilite;	/* TRUE => hilite, FALSE => unhilite */
	{
	REG WORD iszCur;
	RRC	rrc;
#ifdef LISTBOX_HORIZ
	BYTE	dry;		/* height */
#endif

	iszCur = pwnd->iszCurLb;

	/* Cursor should be always on */
	Assert(pwnd->fCursorOn);
	/* position must be valid (assume < 32K) */
	Assert((short) iszCur >= 0);

	GetClientRrc(pwnd, &rrc);

#ifdef LISTBOX_HORIZ
	if (!(pwnd->style & WS_HSCROLL))
#endif
		{
		/* if out of visible range, scroll vertically into range */
		if (fHilite &&
		    (iszCur < pwnd->iszTopLb ||
		     iszCur >= (pwnd->iszTopLb +
		     CitemVisible(pwnd, rrc.ryBottom))))
			{
			/* shift visible range */
			if (iszCur > pwnd->cszLb -
			    CitemVisible(pwnd, rrc.ryBottom))
				pwnd->iszTopLb = pwnd->cszLb -
				    CitemVisible(pwnd, rrc.ryBottom);
			else
				pwnd->iszTopLb = iszCur;

			if (fDrawItem)
				DisplayListBox(pwnd);
			}
		/* Vertical : compute 1 whole line to be [un]highlighted */
		rrc.ryBottom = (rrc.ryTop = (RY) (iszCur - pwnd->iszTopLb)) + 1;

		Assert(rrc.rxLeft == 0);
		MoveCursor(pwnd, rxListBoxMin, rrc.ryTop);
		}

#ifdef LISTBOX_HORIZ
	else
		{
		Assert(pwnd->style & WS_HSCROLL);

		dry = rrc.ryBottom - rrc.ryTop;
		/* if out of visible range, scroll horizontally into range */
		if (fHilite && iszCur < pwnd->iszTopLb)
			{
			/* adjust to be at the left column */
			pwnd->iszTopLb = (iszCur / dry) * dry;
			if (fDrawItem)
				DisplayListBox(pwnd);
			}
		else if (fHilite &&
		    iszCur >= (pwnd->iszTopLb +
		     CitemVisible(pwnd, rrc.ryBottom)))
			{
			/* adjust to be at the right column */
			pwnd->iszTopLb = (iszCur / dry) * dry;
			if (pwnd->iszTopLb >= dry * (pwnd->citemWidthLb - 1))
				pwnd->iszTopLb -= dry * (pwnd->citemWidthLb - 1);
			if (fDrawItem)
				DisplayListBox(pwnd);
			}

		/* Horizontal : compute partial line to hilight */
		iszCur -= pwnd->iszTopLb;	/* from top of listbox */
		Assert((short)iszCur >= 0 &&
		    iszCur < CitemVisible(pwnd, rrc.ryBottom));

		rrc.ryBottom = (rrc.ryTop = (RY) (iszCur % dry)) + 1;

		/* width includes 1 space before and after */
		Assert(rxListBoxMin == 1);
		rrc.rxRight =
		    (rrc.rxLeft = RxLeftItem(pwnd, iszCur / dry) - 1) +
		      pwnd->drxItemLb + 2;

		MoveCursor(pwnd, rrc.rxLeft + rxListBoxMin, rrc.ryTop);
		}
#endif /*LISTBOX_HORIZ*/

	if (fDrawItem)
		{
		FillRrc(pwnd, &rrc, '\0', DiMake(dmAttrOnly,
		    fHilite ? pwnd->isaHiliteColor : pwnd->isaColor));
#ifdef LISTBOX_COLOR
		if (!fHilite)
			{
			/* refill the listbox to get correct colors */
			SetFSelected(pwnd, FALSE);	/* fake not selected */
			FillListBox(pwnd, rrc.ryTop, rrc.ryTop+1, iszCur);
			}
#endif
		}
	SetFSelected(pwnd, TRUE);
	}



STATIC VOID
MoveSelection(pwnd, iszNew)
/*
  -- move the selection to iszNew
*/
REG PWND pwnd;
WORD iszNew;
	{
	Assert(iszNew < pwnd->cszLb);

	if (pwnd->iszCurLb == iszNew && FSelected(pwnd))
		return;		/* already there */

	HiliteListSel(pwnd, FALSE);	/* turn old off */
	pwnd->iszCurLb = iszNew;
	HiliteListSel(pwnd, TRUE);
	}



STATIC VOID
MoveSelectionDown(pwnd)
/*
  -- move iszCur down a line
*/
REG PWND pwnd;
	{
	WORD	iszBottom;
	RRC	rrc;

	GetClientRrc(pwnd,&rrc);
	iszBottom = pwnd->iszTopLb + CitemVisible(pwnd, rrc.ryBottom) - 1;

	if (FSelected(pwnd) && pwnd->iszCurLb + 1 < pwnd->cszLb)
		{
		HiliteListSel(pwnd, FALSE);
		if (pwnd->iszCurLb++ == iszBottom)
			{
			ScrollListBox(pwnd, 1, FALSE);
			return;
			}
		}
	HiliteListSel(pwnd, TRUE);
	}



STATIC VOID
MoveSelectionUp(pwnd)
/*
  -- move iszCur up a line
*/
REG PWND pwnd;
	{
	if (FSelected(pwnd) && pwnd->iszCurLb != iszMin)
		{
		HiliteListSel(pwnd, FALSE);
		if (pwnd->iszCurLb-- == pwnd->iszTopLb)
			{
			ScrollListBox(pwnd, -1, FALSE);
			return;
			}
		}
	HiliteListSel(pwnd, TRUE);
	}



#ifdef LISTBOX_HORIZ
STATIC VOID
MoveSelectionLeft(pwnd, dcol)
/*
  -- move selection left "dcol" columns
  -- vertical listboxes => move up
*/
REG PWND pwnd;
WORD	dcol;
	{
	if (pwnd->style & WS_VSCROLL)
		{
		Assert(dcol == 1);
		MoveSelectionUp(pwnd);
		}
	else
		{
		// horizontal

		WORD	cszMove;	/* # strings to move selection by */
		RRC	rrc;

		GetClientRrc(pwnd,&rrc);
		cszMove = rrc.ryBottom * dcol;

		if (pwnd->iszCurLb - iszMin < cszMove)
			MoveSelection(pwnd, iszMin);
		else
			MoveSelection(pwnd, pwnd->iszCurLb - cszMove);
		}
	}



STATIC VOID
MoveSelectionRight(pwnd, dcol)
/*
  -- move selection right "dcol" columns (horizontal only)
  -- vertical listboxes => move down
*/
REG PWND pwnd;
WORD	dcol;
	{
	if (pwnd->style & WS_VSCROLL)
		{
		Assert(dcol == 1);
		MoveSelectionDown(pwnd);
		}
	else
		{
		WORD	iszNew;
		RRC	rrc;

		GetClientRrc(pwnd,&rrc);
		iszNew = pwnd->iszCurLb + rrc.ryBottom * dcol;
		if (iszNew >= pwnd->cszLb)
			MoveSelection(pwnd, pwnd->cszLb - 1);
		else
			MoveSelection(pwnd, iszNew);
		}
	}
#endif /*LISTBOX_HORIZ*/



STATIC BOOL
FLocateMatch(pwnd, wParam)
/*
  -- find the entry in the list box beginning with ch (case insensitive)
  -- move current location if found
  -- return TRUE if selection made
*/
REG PWND pwnd;
WORD	wParam;			/* wParam of WM_CHAR message */
	{
	char FAR * lgpsz;
	WORD FAR * lmpiszoff;
	REG WORD iszCur;
	BOOL	fRet;

	fRet = FALSE;

 	if (wParam > 0xff)
 		return FALSE;	/* non-ascii */
 
	iszCur = pwnd->iszCurLb;
	if (FSelected(pwnd))
		if (++iszCur >= pwnd->cszLb)
			iszCur = iszMin;	/* try from this point on */

	lgpsz = LpvDeref(pwnd->hmemGpszLb);
	lmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);

	/* convert to upper case */
	if ((char) wParam >= 'a' && (char) wParam <= 'z')
		wParam -= 'a' - 'A';
	else if (wParam > 0x7f)
		wParam = ChUpperFromChExt((char)wParam);

	do
		{
		char FAR * lgpsz2;
		char	ch2;
		WORD	off;

		if (pwnd->hmemGpszLb != NULL &&
		    (off = lmpiszoff[iszCur]) != offEmpty)
			{
			/* valid cached string */
			lgpsz2 = lgpsz+off;		/* &lgpsz[off] */
			}
		else
			{
			/* not in cache */
			char	szBuff[cchListTextMax];

			GetOnDemand(pwnd, iszCur, &lmpiszoff, &lgpsz, szBuff);
			lgpsz2 = (char FAR *) szBuff;
			}
#ifdef EXTRAS
		if (*lgpsz2 == chColorPrefix)
				lgpsz2 += 2;
#endif
		while (((unsigned char)(*lgpsz2) < '0') && (*lgpsz2 != '\0'))
			lgpsz2++;

		ch2 = *lgpsz2;

		if (ch2 >= 'a' && ch2 <= 'z')
			ch2 -= 'a' - 'A';
		else if ((unsigned char) ch2 > 0x7f)
			ch2 = ChUpperFromChExt(ch2);

		if (ch2 == (char) wParam)
			{
			/* got it */
			MoveSelection(pwnd, iszCur);
			fRet = TRUE;
			break;
			}

		/* move iszCur to next possibility */
		if (++iszCur >= pwnd->cszLb)
			iszCur = iszMin;	/* wrap around */

		}
	while (iszCur != pwnd->iszCurLb);

	/*GlobalUnlock(pwnd->hmemGpszLb);*/
	/*GlobalUnlock(pwnd->hmemMpiszoffLb);*/
	return (fRet);
	}




STATIC VOID
ResetContent(pwnd)
/*
  -- resets the contents of the list box
*/
REG PWND pwnd;
	{

#ifdef LISTBOX_HORIZ
	if (pwnd->citemWidthLb == 0)
		{
		RRC rrc;
		GetClientRrc(pwnd,&rrc);
		/* initialize horizontal listbox info */
		pwnd->citemWidthLb = 1;
		pwnd->drxItemLb = rrc.rxRight - 2;
			/* 2 extra for start and end space */
		}
#endif /*LISTBOX_HORIZ*/

	/* free any listbox data */
	if (pwnd->hmemGpszLb != NULL)
		{
		GlobalFree(pwnd->hmemGpszLb);
		Assert(pwnd->hmemMpiszoffLb != NULL);
		GlobalFree(pwnd->hmemMpiszoffLb);
		pwnd->hmemGpszLb = NULL;
		pwnd->hmemMpiszoffLb = NULL;
		}
	else
		{
		Assert(pwnd->hmemMpiszoffLb == NULL);
		}
	pwnd->cszLb = pwnd->iszTopLb = 0;
	pwnd->iszCurLb = iszMin;
	SetFSelected(pwnd, FALSE);
	/* set cursor to no selection */
	MoveCursor(pwnd, rxListBoxMin, 0);
	}


PRIVATE VOID FARPRIVATE
AddListString(pwnd, sz)
/*
  -- add an item to the list box
  -- if sz == NULL, mark as not yet known (for fill on-demand)
*/
REG PWND pwnd;
char *sz;
	{
	AddListSz(pwnd, sz, FALSE);
	}

PRIVATE VOID FARPRIVATE
AddListSz(pwnd, sz, fRedraw)
/*
  -- add an item to the list box
  -- if sz == NULL, mark as not yet known (for fill on-demand)
*/
REG PWND pwnd;
char *sz;
WORD fRedraw;
	{
#ifdef LISTBOX_DIR
	if (!(pwnd->style & LBS_SORT))
		{
		/* not sorted : we must fit it in !!! */
		InsertSz(pwnd, pwnd->cszLb, sz, TRUE, fRedraw);
		}
	else if (pwnd->cszLb == 0)
		{
		/* first element in sorted listbox */
		InsertSz(pwnd, 0, sz, FALSE, fRedraw);
		}
	else
		{
		/* insert in sorted (no OnDemand concerns) */
		char FAR * lgpsz;
		WORD FAR * lmpiszoff;
		short	iszLo, iszHi;

		Assert(sz != NULL);		/* on-demand not allowed */

		lgpsz = LpvDeref(pwnd->hmemGpszLb);
		lmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);
		iszLo = iszMin;
		iszHi = pwnd->cszLb;
		do
			{
			REG short iszProbe = (iszLo + iszHi) / 2;

#ifdef KANJI
			if (fdircmp((char *) sz, lgpsz+lmpiszoff[iszProbe]) > 0)
#else
			if (fstrcmp((char *) sz, lgpsz+lmpiszoff[iszProbe]) > 0)
#endif
				iszLo = iszProbe+1;
			else
				iszHi = iszProbe;
			}
		while (iszLo < iszHi);

		/*GlobalUnlock(pwnd->hmemGpszLb);*/
		/*GlobalUnlock(pwnd->hmemMpiszoffLb);*/

		InsertSz(pwnd, iszLo, sz, FALSE, fRedraw);
		}
#else
	/* no DIR listboxes -- don't bother with sort option */
	Assert(!(pwnd->style & LBS_SORT));
	InsertSz(pwnd, pwnd->cszLb, sz, FALSE, fRedraw);	/* add at end */
#endif
	}

#ifdef KANJI
STATIC int
/*
 * it is not for KANJI.
 * sorting dirname as same as WINDOWS.
 *  order is as follows
 *  1: x.xxx	files
 *  2: [..]	parent directory
 *  3: [xxx]	directories
 *  4: [-x-]	drives
 */
fdircmp(s1, s2)
char FAR *s1, FAR *s2;
{
	if (*s1 != '[') {
		if (*s2 == '[') return(-1);
	} else if (*s2 != '[') {
		return(1);
	} else {
		++s1, ++s2;
		if (*s1 == '.') return(-1);
		if (*s2 == '.') return(1);
		if (*s1 != '-') {
			if (*s2 == '-') return(-1);
		} else  if (*s2 != '-') return(1);
	}
#ifdef KANJI
	return(fjstrcmp(s1, s2)); /* really needs for KANJI */
#else
	return(fstrcmp(s1, s2));
#endif
}

#define	iskanji(c)	( ((c)>=0x81 && (c)<=0x9F)||((c)>=0xE0 && (c)<= 0xFC) )

STATIC int
fjstrcmp(s1, s2)
unsigned char FAR *s1, FAR *s2;
{
	register unsigned short c1, c2;

	for (;;) {
		c1 = *s1++;
		if (iskanji(c1))
			c1 = (*s1 == 0)? 0: (c1 << 8)|*s1++;
		c2 = *s2++;
		if (iskanji(c2))
			c2 = (*s2 == 0)? 0: (c2 << 8)|*s2++;

		if (c1 < c2) return(-1);
		if (c1 > c2) return(1);
		if (c1 == 0) return(0);
	}
}

#endif /* KANJI */


STATIC VOID
InsertSz(pwnd, isz, sz, fForce, fRedraw)
/*
  -- insert a string at the specified position in the listbox
  -- if no memory left, do not add (no error condition)
  -- if sz == NULL, mark as special (on-demand)
  -- if previous position empty, replace string
  -- if changing size and fForce and no memory then go to low memory case
	(free any blocks and do everything on demand)
*/
REG PWND pwnd;
WORD isz;
char *sz;
BOOL fForce;
WORD fRedraw;
	{
	WORD FAR * lmpiszoff;		/* string pointers */
	WORD	cch;
	WORD	off;
	RRC	rrc;

	cch = (sz != NULL) ? (strlen(sz) + 1) : 0;
	if (pwnd->cszLb == 0)
		{
		/* first time allocation */
		Assert(pwnd->hmemGpszLb == NULL);
		Assert(pwnd->hmemMpiszoffLb == NULL);

		if ((pwnd->hmemGpszLb = GlobalAlloc(GMEM_MOVEABLE,
		    (DWORD) cbInitLb)) == NULL)
			{
			/* out of memory for string data */
			if (!fForce)
				return;
			/* force => no handle on demand all the time */
			}
		else if ((pwnd->hmemMpiszoffLb = GlobalAlloc(GMEM_MOVEABLE,
		    (DWORD) coffInitLb * sizeof(WORD))) == NULL)
			{
			/* out of memory for string indices */
			GlobalFree(pwnd->hmemGpszLb);
			pwnd->hmemGpszLb = NULL;
			if (!fForce)
				return;
			}
		/* initialize it */
		pwnd->offLb = 0;
		pwnd->iszTopLb = iszMin;
		pwnd->offMaxLb = cbInitLb;
		pwnd->iszMacLb = coffInitLb;
		pwnd->iszCurLb = iszMin;	/* start at top */
		}

	if (pwnd->hmemGpszLb == NULL)
		{
		/* we are doing things on demand */
		if (fForce)
			RevertToOomLb(pwnd, isz);
		return;
		}

	/* try to insert into the cache */
	Assert(pwnd->hmemGpszLb != NULL);
	Assert(pwnd->hmemMpiszoffLb != NULL);

	if (pwnd->cszLb >= pwnd->iszMacLb)
		{
		/* try to grow for indices */
		/* if >64K then request to GlobalReAlloc should fail! */
		WORD	hmem;

		if ((hmem = GlobalReAlloc(pwnd->hmemMpiszoffLb,
		    (DWORD) (pwnd->iszMacLb + coffGrowLb) * sizeof(WORD),
		    GMEM_MOVEABLE)) == NULL)
			{
			if (fForce)
				RevertToOomLb(pwnd, isz);
			return;
			}

		pwnd->iszMacLb += coffGrowLb;
		pwnd->hmemMpiszoffLb = hmem;
		}

	if (((off = pwnd->offLb) + cch) >= pwnd->offMaxLb)
		{
		WORD	hmem;

		if ((hmem = GlobalReAlloc(pwnd->hmemGpszLb,
		    (DWORD) pwnd->offMaxLb + cbGrowLb,
		    GMEM_MOVEABLE)) == NULL)
			{
			if (fForce)
				RevertToOomLb(pwnd, isz);
			return;
			}
		pwnd->offMaxLb += cbGrowLb;
		pwnd->hmemGpszLb = hmem;
		}

	/* now place in cache */
	if (sz == NULL)
		{
		off = offEmpty;
		}
	else
		{
		char FAR * lgpsz;		/* string data */

		lgpsz = LpvDeref(pwnd->hmemGpszLb);
		bltbytex((char FAR *) sz, lgpsz+off, cch);
		/*GlobalUnlock(pwnd->hmemGpszLb);*/
		}

	/* Insert offset into list */
	lmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);
	if (isz >= pwnd->cszLb)
		{
		/* easy insertion at end of list */
		lmpiszoff[pwnd->cszLb++] = off;
		}
	else
		{
		/* may be empty ? */
		WORD FAR * lpoff;

		lpoff = &lmpiszoff[isz];		/* where it should go */

		if (*lpoff != offEmpty)
			{
			/* move space in index block */
			bltbytex(lpoff, lpoff+1,
			    (pwnd->cszLb - isz) * sizeof(WORD));
			pwnd->cszLb++;

			/* if string was added before visible portion, update
			*   listbox indices (for top and current) */
			if (isz < pwnd->iszTopLb)
				{
				pwnd->iszTopLb++;
				pwnd->iszCurLb++;
				}
			}
		Debug(else Assert(off != offEmpty));
			/* else just replacing current on-demand string */

		*lpoff = off;
		}

	GetClientRrc(pwnd,&rrc);

	if ((isz >= pwnd->iszTopLb)
		 && (isz <= pwnd->iszTopLb + (WORD) rrc.ryBottom) && fRedraw)
		DrawWindow(pwnd);

	/*GlobalUnlock(pwnd->hmemMpiszoffLb);*/
	pwnd->offLb += cch;
	}

STATIC VOID
DeleteSz(pwnd, isz, fRedraw)
/*
  -- delete a string at the specified position in the listbox
*/
REG PWND pwnd;
WORD isz;
WORD fRedraw;
	{
	char FAR * lgpsz;		/* string data */
	WORD FAR * lmpiszoff;		/* string pointers */
	WORD	cch;
	WORD	off;
	WORD	offOld;
	WORD	iszLoop;
	WORD FAR * lpoff;
	RRC	rrc;


	Assert((isz >= 0) && (isz < pwnd->cszLb));

	/* try to insert into the cache */
	Assert(pwnd->hmemGpszLb != NULL);
	Assert(pwnd->hmemMpiszoffLb != NULL);

	lgpsz = LpvDeref(pwnd->hmemGpszLb);
	lmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);
	off = lmpiszoff[isz];

	Assert(off != offEmpty);	/* can't be on-demand */

	cch = fstrlen(lgpsz+off);
	bltbytex(lgpsz+off+cch,lgpsz+off,(pwnd->offLb-off-cch));
	pwnd->offLb -= cch;

	lpoff = &lmpiszoff[isz];		/* where it is */

	if (isz != pwnd->cszLb) bltbytex(lpoff+1, lpoff,
			    (pwnd->cszLb - isz) * sizeof(WORD));
	pwnd->cszLb--;

	for (iszLoop = 0; iszLoop < pwnd->cszLb; iszLoop++)
		{
		if ((offOld = lmpiszoff[iszLoop]) >= off)
			lmpiszoff[iszLoop] = offOld-cch;
		}

	GetClientRrc(pwnd,&rrc);

	if (pwnd->cszLb == 0)
		{
		SendMessage(pwnd,LB_RESETCONTENT,0,0L);
		}
	else
		{
		if (isz < pwnd->iszTopLb)
			{
			pwnd->iszTopLb--;
			if (pwnd->iszCurLb > 0) pwnd->iszCurLb--;
			}

		if (pwnd->iszCurLb >= pwnd->cszLb)
			pwnd->iszCurLb--;
		if ((pwnd->iszTopLb > 0)
			&&(pwnd->iszTopLb + (WORD) rrc.ryBottom > pwnd->cszLb))
			pwnd->iszTopLb--;
		}

	if ((isz >= pwnd->iszTopLb)
		 && (isz <= pwnd->iszTopLb + (WORD) rrc.ryBottom) && fRedraw)
		DrawWindow(pwnd);

	}



STATIC VOID
ReplaceSz(pwnd, isz, sz)
/*
  -- replace a string in the listbox
  -- update the screen if necessary
*/
REG PWND pwnd;
WORD isz;
char *sz;
	{
	char FAR * lgpsz;
	WORD FAR * lmpiszoff;
	WORD	off;
	RY	ry;
	RRC	rrc;

	/* lock down string buffer and indexes (handles may be NULL) */
	lgpsz = LpvDeref(pwnd->hmemGpszLb);
	lmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);


	Assert(isz < pwnd->cszLb);

	if (pwnd->hmemGpszLb != NULL &&
	    (off = lmpiszoff[isz]) != offEmpty)
		{
		/* valid cached string */
		Assert(strlen(sz) == fstrlen((char FAR *)(lgpsz+off)));
		fstrcpy(lgpsz+off, (char FAR *) sz);
		}

	GetClientRrc(pwnd,&rrc);

	if ((isz >= pwnd->iszTopLb)
		 && (isz < pwnd->iszTopLb + (WORD) rrc.ryBottom))
		{
		/* item is displayed */
		ry = (RY) (isz - pwnd->iszTopLb);
		TextOut(pwnd, rxListBoxMin, ry, sz, -1, isaListBox);
		if (isz == pwnd->iszCurLb && FSelected(pwnd))
			{
			rrc.ryBottom = (rrc.ryTop = ry) + 1;
			FillRrc(pwnd, &rrc, '\0', DiMake(dmAttrOnly,
				pwnd->isaHiliteColor));
			}
		}
	}




STATIC VOID
RevertToOomLb(pwnd, isz)
/*
  -- revert to low memory useage of listbox data
    (both handles NULL, all data by on-demand)
  -- must not be sorted
*/
REG PWND pwnd;
WORD	isz;		/* string that is inserted */
	{
	Assert(!(pwnd->style & LBS_SORT));

	if (pwnd->hmemGpszLb != NULL)
		{
		GlobalFree(pwnd->hmemGpszLb);
		Assert(pwnd->hmemMpiszoffLb != NULL);
		GlobalFree(pwnd->hmemMpiszoffLb);
		pwnd->hmemGpszLb = NULL;
		pwnd->hmemMpiszoffLb = NULL;
		}
	/* if the new string is after the end of the listbox, adjust size */
	if (isz >= pwnd->cszLb)
		pwnd->cszLb = isz + 1;
	}




PRIVATE WORD FARPRIVATE
GetListText(pwnd, sz, cchMax)
/*
  -- fill sz with the contents of the currently selected string
  -- return number of characters copied
*/
REG PWND pwnd;
char *sz;
WORD cchMax;
	{
	char FAR * lgpsz;
	WORD FAR * lmpiszoff;
	char FAR * lsz;
	WORD	cch;
	char	szBuff[cchListTextMax];

	/* don't copy anything if the listbox is empty or cursor is not
	   on anything. */

	if (pwnd->cszLb == 0 || !FSelected(pwnd))
		{
		sz[0] = '\0';
		return (0);
		}

	if (pwnd->hmemGpszLb != NULL)
		{
		/* get from cache */
		lgpsz = LpvDeref(pwnd->hmemGpszLb);
		lmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);
		Assert(lmpiszoff[pwnd->iszCurLb] != offEmpty);
		lsz = (char FAR *) lgpsz + lmpiszoff[pwnd->iszCurLb];
		/*GlobalUnlock(pwnd->hmemMpiszoffLb);*/
		}
	else
		{
		GetOnDemand(pwnd, pwnd->iszCurLb, NULL, NULL, szBuff);
		lsz = (char FAR *) szBuff;
		}

	if ((cch = fstrlen(lsz) + 1) > cchMax)
		cch = cchMax;
	/* copy on the number of characters needed */
	bltbytex(lsz, (char FAR *) sz, cch);

	/*GlobalUnlock(pwnd->hmemGpszLb);*/
	return(cchMax);
	}



STATIC VOID
GetOnDemand(pwnd, isz, plmpiszoff, plgpsz, szDest)
/*
  -- get a string for an on-demand listbox
  -- unlock the blocks, call listbox proc, relock the block
  -- fill szDest with string
  -- save string (so we don't have to go through this again) if room
*/
REG PWND pwnd;
WORD	isz;
WORD FAR ** plmpiszoff;
char FAR ** plgpsz;
char *	szDest;
	{
	/*GlobalUnlock(pwnd->hmemGpszLb);*/
	/*GlobalUnlock(pwnd->hmemMpiszoffLb);*/

	(*PwfnCtlLb(pwnd))(tmmText, szDest, isz, pwnd->id, 0, 0);

	if (pwnd->cszLb == 0 || pwnd->hmemGpszLb)
		{
		/* starting a new listbox or not oom listbox */
		InsertSz(pwnd, isz, szDest, FALSE, FALSE); /* may fail */
		}

	*plgpsz = LpvDeref(pwnd->hmemGpszLb);
	*plmpiszoff = LpvDeref(pwnd->hmemMpiszoffLb);
	}



STATIC VOID FAR *
LpvDeref(hmem)
/*
  -- defererence a handle
  -- (or return NULL if handle is NULL)
*/
WORD	hmem;
	{
	return (hmem == NULL) ? NULL : LpvGlobalDeref(hmem);
	}

/**************************************************************************/
/*   Public interface for listboxes                                       */
/**************************************************************************/

#ifndef LISTBOX_HORIZ	// only supported for vertical

PUBLIC VOID	FARPUBLIC
InitListBox(pwnd, pwfn_ctl)
PWND	 pwnd;
PWFN_CTL pwfn_ctl;
/*
  -- fix color
  -- put on demand function supplied into the wnd struct
  -- reset listbox
  -- fill listbox
*/
	{
	StartPublic();

		/* Non-directory ListBox */
	WORD	isz;
	WORD	csz;
	char	szBuffer[cchListTextMax];

	if (pwfn_ctl != NULL) PwfnCtlLb(pwnd) = pwfn_ctl;
	pwfn_ctl = PwfnCtlLb(pwnd);

	SendMessageShort(pwnd, LB_RESETCONTENT);
	csz = (*pwfn_ctl)(tmmCount, NULL, NULL, pwnd->id, 0, 0);
	isz = 0;

	if ((csz == cszUnknown)||(pwnd->style && LBS_SORT))
		{
		/* we must fill in the listbox */
		while (csz == cszUnknown || isz < csz)
			{
			if ((*pwfn_ctl)(tmmText, szBuffer, isz++, pwnd->id,
				    0, 0))
				{
				/* we have a string */
				SendMessage(pwnd, LB_ADDSTRING,
				    (WORD) szBuffer, 0L);
				}
			else if (csz == cszUnknown)
				break;	/* stop filling */
			}
		}
	else
		{
		/* fill on demand (fill with empties) */
		Assert(csz != cszUnknown);
		while (csz--)
			SendMessage(pwnd, LB_ADDSTRING, 0, 0L);
		}

	StopPublic();

	}



PUBLIC VOID FARPUBLIC
GetListBoxOrientation(pwnd, pisz, pdisz)
PWND pwnd;
WORD * pisz;
WORD * pdisz;
	{
	*pisz = (WORD) SendMessage(pwnd, LB_GETCURSEL, 0, 0L);
	*pdisz = (*pisz == iszNil) ? 0 : (*pisz - pwnd->iszTopLb);
	}



PUBLIC VOID	FARPUBLIC
InitListBoxOriented(pwnd, pwfn_ctl, pisz, pdisz)
PWND	 pwnd;
PWFN_CTL pwfn_ctl;
WORD	* pisz;			/* current selection */
WORD	* pdisz;		/* distance from top of box */
/*
  -- Init then orient listbox
*/
	{
	StartPublic();

	InitListBox(pwnd, pwfn_ctl);

	if (*pisz != iszNil) 
		{
		if (pwnd->cszLb == 0)
			{
			*pisz = iszNil;
			*pdisz = 0;
			}
		else
			{
			RRC	rrc;
			WORD	dry;

			GetClientRrc(pwnd,&rrc);
			dry = (WORD) rrc.ryBottom;

			/* 1 - make selection legal
			   2 - make sure no blank space at the bottom
			   3 - make distance from top legal */

			*pisz  = min(*pisz, pwnd->cszLb - 1);
			if (pwnd->cszLb + *pdisz < *pisz + dry)
				*pdisz = *pisz + dry - pwnd->cszLb;
			*pdisz = min(*pisz, min(*pdisz, dry-1));

			pwnd->iszTopLb = *pisz - *pdisz;
			pwnd->iszCurLb = *pisz;
			}
		}

	DrawWindow(pwnd);
	SendMessage(pwnd,LB_SETCURSEL,*pisz,0L);

	StopPublic();
	}

#endif /*!LISTBOX_HORIZ*/
