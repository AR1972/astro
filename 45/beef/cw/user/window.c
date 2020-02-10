/*
	COW : Character Oriented Windows

	window.c  : Window Support

	This module contains the window relative output routines
        as well as routines for accessing and changing the window
        data structure.
*/

#define COW
#include <cow.h>

#define WINDOW
#include <uwindow.h>
#include <uevent.h>
#include <umenu.h>
#include <uscreen.h>
#include <uisa.h>

#include "event.h"
#include "screen.h"
#include "util.h"
#include "overlap.h"

#include "window.h"


#ifdef EXTRAS
#define	chColorPrefix	((char) 0xfe)		/* => special color prefix */
#endif

STATIC WORD	fBlockCursor = 1;		/* see SetCursorBlock() */

#ifdef WINDOW_OVERLAP
extern PWND pwndClip;
#endif /*WINDOW_OVERLAP*/

/* forward declarations for STATIC procedures */
STATIC VOID EnableWindowAndSiblings(PWND, BOOL);
STATIC VOID DrawWindowAndSiblings(PWND);
STATIC VOID TextOutColor(AX, AY, char *, WORD, WORD);
#ifdef WINDOW_OVERLAP
STATIC VOID MoveWindowAndSiblings(PWND, BYTE, BYTE);
STATIC VOID ValidateAWindow(PWND);
STATIC VOID ValidateWindowAndSiblings(PWND);
STATIC VOID ValidateWindowAndParents(PWND);
STATIC VOID MoveCursorClipped(PWND);
#endif /*WINDOW_OVERLAP*/



PUBLIC VOID FARPUBLIC
AddChild(pwndParent, pwndChild)
PWND pwndParent, pwndChild;
/* AddChild adds a child to the specified window.  If pwndParent is
   null then a child is added to the root directory.  Child is added
   to the end of the sibling chain. */
	{
	StartPublic();
	PWND pwndCur, pwndPrev;
	if (pwndParent)
		{
		if ((pwndPrev = pwndParent->pwndChild) == NULL)
			pwndParent->pwndChild = pwndChild;
		else
			{
			while ((pwndCur = pwndPrev->pwndSibling) != NULL)
				pwndPrev = pwndCur;
			pwndPrev->pwndSibling = pwndChild;
			}
		pwndChild->pwndSibling = NULL;
		EnableWindow(pwndChild, pwndParent->fEnabled);
#ifdef WINDOW_OVERLAP
		if (pwndParent->style & WS_CLIPOUT)
			{
			pwndChild->style |= WS_CLIPOUT;
			ClipoutWindowAndSiblings(pwndChild->pwndChild);
			}
#endif /*WINDOW_OVERLAP*/
		}
	else
		{
		pwndChild->pwndSibling = pwndRoot;
		pwndRoot = pwndChild;
		EnableWindow(pwndChild, TRUE);
		}
	pwndChild->pwndParent = pwndParent;
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
EnableWindow(pwnd, fEnabled)
PWND pwnd;
BOOL fEnabled;
/* Enable or disable a window.  All children are enabled or disabled as well */
	{
	StartPublic();
	pwnd->fEnabled = (fEnabled != FALSE);
	EnableWindowAndSiblings(pwnd->pwndChild, fEnabled);
	StopPublic();
	}



STATIC VOID
EnableWindowAndSiblings(pwnd, fEnabled)
REGISTER PWND pwnd;
BOOL fEnabled;
	{
	while (pwnd)
		{
		pwnd->fEnabled = (fEnabled != FALSE);
		EnableWindowAndSiblings(pwnd->pwndChild, fEnabled);
		pwnd = pwnd->pwndSibling;
		}
	}



PUBLIC VOID FARPUBLIC
RemoveChild(pwndChild)
/*
  -- removes a child from its parent window
  -- special case allows removal of root windows
*/
REGISTER PWND pwndChild;
	{
	StartPublic();
	PWND pwndParent = pwndChild->pwndParent;
	REGISTER PWND pwnd;

	AssertSz(pwndChild, "RemoveChild(NULL)");

	if (pwndParent == NULL)
		{
		/* should be a root window */
		if (pwndChild == pwndRoot)
			{
			/* remove first root */
			pwndRoot = pwndChild->pwndSibling;
			}
		else
			{
			pwnd = pwndRoot;
			goto scan_sibling;
			}
		}
	else if ((pwnd = pwndParent->pwndChild) == pwndChild)
		{
		/* remove first child */
		pwndParent->pwndChild = pwndChild->pwndSibling;
		}
	else
		{
scan_sibling:
		while (pwnd->pwndSibling != pwndChild)
			{
			pwnd = pwnd->pwndSibling;
			AssertSz(pwnd != NULL, "child window not found");
			}
		pwnd->pwndSibling = pwndChild->pwndSibling;
		}
	pwndChild->pwndParent = NULL;
	pwndChild->pwndSibling = NULL;
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
GetClientRrc(pwnd, prrc)
/*
  -- create a rectangle that composes the entire clent area.
  -- Note that the top left hand corner of the client rrc is
	always 0,0 since all output is relative to this point.
*/
REGISTER PWND pwnd;
REGISTER PRRC prrc;
	{
	StartPublic();
	prrc->ryTop = 0;
	prrc->rxLeft = 0;
	prrc->ryBottom = (pwnd->arcWindow.ayBottom)-(pwnd->arcWindow.ayTop);
	prrc->rxRight = (pwnd->arcWindow.axRight)-(pwnd->arcWindow.axLeft);
	if (pwnd->style & WS_BORDER)
		{
		/* border around window */
		prrc->ryBottom -= 2 * dayBorder;
		prrc->rxRight -= 2 * daxBorder;
#ifdef WINDOW_OVERLAP
		if (pwnd->style & WS_OVERLAP)
			{
			if (pwnd->style & WS_HSCROLL)
				prrc->ryBottom -= dayBorder;
			if (pwnd->style & WS_VSCROLL)
				prrc->rxRight -= daxBorder;
			}
#endif /*WINDOW_OVERLAP*/
		}
	else
		{
		if (pwnd->style & WS_HSCROLL)
			prrc->ryBottom -= dayBorder;
		if (pwnd->style & WS_VSCROLL)
			prrc->rxRight -= daxBorder;
		}
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
SetWindowSize(pwnd, drx, dry)
/*
  -- changes the size of a window to be (drx, dry).
  -- Actual window and client window are adjusted appropriately
*/
REGISTER PWND pwnd;
BYTE drx, dry;
	{
	StartPublic();
	pwnd->arcWindow.axRight = pwnd->arcWindow.axLeft+drx;
	pwnd->arcWindow.ayBottom = pwnd->arcWindow.ayTop+dry;
	ValidateWindow(pwnd);
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
SetWindowStyle(pwnd, style)
/*
  -- change the style of the window.
*/
PWND pwnd;
WORD style;
	{
	StartPublic();
	pwnd->style = style;
	ValidateWindow(pwnd);
	StopPublic();
	}


#ifdef WINDOW_OVERLAP

PRIVATE VOID FARPRIVATE
ValidateWindow(pwnd)
/*
  -- computes the clipping rectangle for a window and all of its children
*/
REGISTER PWND pwnd;
	{
	StartPublic();

	ValidateWindowAndParents(pwnd->pwndParent);
	ValidateAWindow(pwnd);
	ValidateWindowAndSiblings(pwnd->pwndChild);
	StopPublic();
	}

STATIC VOID
ValidateAWindow(pwnd)
/*
  -- computes the clipping rectangle for a window
*/
REGISTER PWND pwnd;
	{
	RRC	rrc;


	GetClientRrc(pwnd,&rrc);
	pwnd->arcClipping = pwnd->arcWindow;
	if (pwnd->style & WS_BORDER)
		{
		/* border around window */
		pwnd->arcClipping.axLeft += daxBorder;
		pwnd->arcClipping.ayTop += dayBorder;
		}
	pwnd->arcClipping.axRight = pwnd->arcClipping.axLeft + rrc.rxRight;
	pwnd->arcClipping.ayBottom = pwnd->arcClipping.ayTop + rrc.ryBottom;

	if ((pwnd->pwndParent != NULL) && (pwnd->style & WS_CLIPOUT))
		{
		if ((pwnd->style & WS_TYPE) == WS_SCROLL) 
			{
			if (pwnd->pwndParent->pwndParent != NULL)
				{
				pwnd->arcClipping.axRight =
				 min(pwnd->arcClipping.axRight,
			 pwnd->pwndParent->pwndParent->arcClipping.axRight);
				pwnd->arcClipping.ayBottom =
				 min(pwnd->arcClipping.ayBottom,
			 pwnd->pwndParent->pwndParent->arcClipping.ayBottom);
				}
			}
		else
			{
			pwnd->arcClipping.axRight =
				 min(pwnd->arcClipping.axRight,
				 pwnd->pwndParent->arcClipping.axRight);
			pwnd->arcClipping.ayBottom =
				 min(pwnd->arcClipping.ayBottom,
				 pwnd->pwndParent->arcClipping.ayBottom);
			}
		}

	StopPublic();
	}


STATIC VOID
ValidateWindowAndSiblings(pwnd)
/*
  -- computes the clipping rectangle for a window and all of its children
*/
REGISTER PWND pwnd;
	{
	while (pwnd != NULL)
		{
		ValidateAWindow(pwnd);
		ValidateWindowAndSiblings(pwnd->pwndChild);
		pwnd = pwnd->pwndSibling;
		}
	}


STATIC VOID
ValidateWindowAndParents(pwnd)
/*
  -- computes the clipping rectangle for a window and all of its children
*/
REGISTER PWND pwnd;
	{
	if (pwnd != NULL)
		{
		ValidateWindowAndParents(pwnd->pwndParent);
		ValidateAWindow(pwnd);
		}
	}

#else

/* No Overlapping windows to worry about */

PRIVATE VOID FARPRIVATE
ValidateWindow(pwnd)
/*
  -- computes the clipping rectangle for a window and all of its children
*/
REGISTER PWND pwnd;
	{
	StartPublic();
	RRC	rrc;


	GetClientRrc(pwnd,&rrc);
	pwnd->arcClipping = pwnd->arcWindow;
	if (pwnd->style & WS_BORDER)
		{
		/* border around window */
		pwnd->arcClipping.axLeft += daxBorder;
		pwnd->arcClipping.ayTop += dayBorder;
		}
	pwnd->arcClipping.axRight = pwnd->arcClipping.axLeft + rrc.rxRight;
	pwnd->arcClipping.ayBottom = pwnd->arcClipping.ayTop + rrc.ryBottom;

	StopPublic();
	}


#endif /*WINDOW_OVERLAP*/

PUBLIC VOID FARPUBLIC
DrawWindow(pwnd)
/*
  -- draw window and children
*/
PWND pwnd;
	{
	StartPublic();
	if (pwnd)
		{
		SendMessageShort(pwnd, WM_PAINT);
		DrawWindowAndSiblings(pwnd->pwndChild);
		}
	else if (pwndRoot)
		{
		DrawMenubar();
		DrawWindowAndSiblings(pwndRoot);
		}
	StopPublic();
	}



STATIC VOID
DrawWindowAndSiblings(pwnd)
PWND pwnd;
	{
	while (pwnd != NULL)
		{
		SendMessageShort(pwnd, WM_PAINT);
		DrawWindowAndSiblings(pwnd->pwndChild);
		pwnd = pwnd->pwndSibling;
		}
	}



PUBLIC VOID FARPUBLIC
DrawBorder(pwnd, pbox, di, szTitle)
/*
  -- draw a box around the window only if WS_STYLE includes WS_BORDER
  -- MERGE ABS/REL
*/
REGISTER PWND pwnd;
BOX *	pbox;
WORD	di;
char *	szTitle;
	{
	StartPublic();
	if (pwnd->style & WS_BORDER)
		{
#ifdef WINDOW_OVERLAP
		if (pwnd->style & WS_CLIPOUT)
			pwndClip = pwnd->pwndParent;
#endif /*WINDOW_OVERLAP*/
		DrawThisWnd(pwnd);
		DrawBoxArc(pbox, &pwnd->arcWindow, di,
			   !(pwnd->style & WS_VSCROLL),
			   !(pwnd->style & WS_HSCROLL),
			   szTitle);
#ifdef WINDOW_OVERLAP
		if (pwnd->style & WS_CLIPOUT)
			pwndClip = NULL;
#endif /*WINDOW_OVERLAP*/
		}
	StopPublic();
	}



#ifdef EXTRAS
PRIVATE int FARPRIVATE
CchRealLenSz(sz)
/*
  -- return string length (excluding special color characters)
*/
REG char * sz;
	{
	REG WORD cch;

	cch = 0;
	while (*sz != '\0')
		{
		if (*sz == chColorPrefix)
			{
			/* skip prefix and following character */
			Assert(sz[1] != '\0');
			sz += 2;
			}
		else
			{
			cch++;
			sz++;
			}
		}
	return cch;
	}



VOID FARPRIVATE
TextOutColor(ax, ay, pch, cch, diOrig)
AX	ax;
AY	ay;
char *	pch;
WORD	cch;
WORD	diOrig;
/*
  -- text out with special color check
*/
	{
	REG char * pchT;
	REG WORD cchT;
	WORD	di = diOrig;

	cchT = 0;

	while (1)
		{
		/* skip to end of string or to color prefix character */
		pchT = pch;
		while (cchT < cch && *pchT != chColorPrefix)
			{
			cchT++;
			pchT++;
			}

		TextOutAbs(ax, ay, pch, cchT, di);
		if (cchT == cch)
			break;		/* done */
		ax += (BYTE) cchT;
		Assert(*pchT == chColorPrefix);
		if (pchT[1] < '0' || pchT[1] > '9')
			{
			/* revert to original color */
			di = diOrig;
			}
		else
			{
			/* set to specified user color */
			di = (diOrig & 0xff00) | (isaUserMin + pchT[1] - '0');
			}

		pch = pchT+2;		/* after color magic */
		}
	}

#endif /*EXTRAS*/



PUBLIC VOID FARPUBLIC
TextOut(pwnd, rx, ry, pch, cch, di)
/*
  -- output a line of text to the specified window
  -- (rx, ry) specify the relative position to start output
  -- cch is the number of characters to display, -1 indicates sz string
  -- di is the display info
  -- MERGE ABS/REL
*/
REGISTER PWND pwnd;
RX	rx;
RY	ry;
char *	pch;
short	cch;
WORD	di;
	{
	StartPublic();
	AX ax;
	AY ay;

	if (cch == -1)
		cch = CchRealLenSz(pch);

	DrawThisWnd(pwnd);

	if (pwnd == NULL)
		{
#ifdef EXTRAS
		TextOutColor((AX) rx, (AY) ry, pch, cch, di);
#else
		TextOutAbs((AX) rx, (AY) ry, pch, cch, di);
#endif
		StopPublic();
		return;
		}

	ax = pwnd->arcClipping.axLeft+rx;
	ay = pwnd->arcClipping.ayTop+ry;
	if ((ax < pwnd->arcClipping.axRight) && (ay < pwnd->arcClipping.ayBottom) && (ry >= 0))
		{
		if ((ax+cch) > pwnd->arcClipping.axRight)
			cch = (pwnd->arcClipping.axRight-ax);
#ifdef EXTRAS
		TextOutColor(ax, ay, pch, cch, di);
#else
		TextOutAbs(ax, ay, pch, cch, di);
#endif
		}
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
FillRrc(pwnd, prrc, ch, di)
/*
  -- fill a rectangle with a given character
  -- MERGE ABS/REL
*/
PWND	pwnd;
PRRC	prrc;
ACHAR	ch;
WORD	di;
	{
	StartPublic();
	ARC arc;

	if (FArcFromRrc(pwnd, prrc, &arc))
		{
		DrawThisWnd(pwnd);
		FillArc(arc.axLeft, arc.ayTop, arc.axRight, arc.ayBottom,
			ch, di);
		}
	StopPublic();
	}



PRIVATE BOOL FARPRIVATE
FArcFromRrc(pwnd, prrc, parc)
/*
  -- convert a relative rectangle into an absolute rectangle
  -- return TRUE if rectangle within client area
*/
PWND pwnd;
PRRC prrc;
REGISTER PARC parc;
	{
	if (pwnd == NULL)
		{
		*parc = *(PARC)prrc;
		return TRUE;
		}
	parc->axLeft = prrc->rxLeft+pwnd->arcClipping.axLeft;
	parc->axRight = prrc->rxRight+pwnd->arcClipping.axLeft;
	parc->ayTop = prrc->ryTop+pwnd->arcClipping.ayTop;
	parc->ayBottom = prrc->ryBottom+pwnd->arcClipping.ayTop;
	return(IntersectRect((PRRC) parc, (PRRC) &(pwnd->arcClipping), (PRRC) parc));
	}




PUBLIC VOID FARPUBLIC
BltRrc(pwnd, rxDest, ryDest, drx, dry, rxSrc, rySrc)
/*
  -- moves source rectangle to destination
*/
REGISTER PWND pwnd;
RX rxDest, rxSrc;
RY ryDest, rySrc;
BYTE drx, dry;
	{
	StartPublic();

	if (pwnd != NULL)
		{
		/* convert Relative to Absolute */
		rxSrc += pwnd->arcClipping.axLeft;
		rySrc += pwnd->arcClipping.ayTop;
		rxDest += pwnd->arcClipping.axLeft;
		ryDest += pwnd->arcClipping.ayTop;

		/* from this point on rx & ry are really ax & ay */
		if ((rxSrc >= pwnd->arcClipping.axRight) ||
		    (rxDest >= pwnd->arcClipping.axRight) ||
		    (rySrc >= pwnd->arcClipping.ayBottom) ||
		    (ryDest >= pwnd->arcClipping.ayBottom))
			{
			/* outside range */
			StopPublic();
			return;
			}
		if ((rxSrc+drx) >= pwnd->arcClipping.axRight)
			drx = pwnd->arcClipping.axRight - rxSrc;
		if ((rxDest+drx) >= pwnd->arcClipping.axRight)
			drx = pwnd->arcClipping.axRight - rxDest;
		if ((rySrc+dry) >= pwnd->arcClipping.ayBottom)
			dry = pwnd->arcClipping.ayBottom - rySrc;
		if ((ryDest+dry) >= pwnd->arcClipping.ayBottom)
			dry = pwnd->arcClipping.ayBottom - ryDest;
		}

	DrawThisWnd(pwnd);	/* this one is tricky */
	BltArc((AX) rxDest, (AY) ryDest, drx, dry, (AX) rxSrc, (AY) rySrc);
	}



PUBLIC VOID FARPUBLIC
DrawBox(pwnd, prrc, pbox, di)
/*
  -- draws a box specified by the rectangle prrc using the characters
	defined by pbox.
  -- note : the entire rectangle must be contained in the window
  -- MERGE ABS/REL
*/
PWND pwnd;
REGISTER PRRC prrc;
BOX *pbox;
WORD di;
	{
	StartPublic();
	ARC arc;
	AssertCorrectRrc(prrc)
	if (FArcFromRrc(pwnd, prrc, &arc))
		{
		DrawThisWnd(pwnd);
		DrawBoxArc(pbox, &arc, di, TRUE, TRUE, NULL);
		}
	StopPublic();
	}

#ifdef WINDOW_OVERLAP
STATIC VOID
MoveCursorClipped(pwnd)
REGISTER PWND pwnd;
	{
	if (pwnd->style & WS_CLIPOUT)
		{
		if (((pwnd->axCursor >= pwnd->arcClipping.axRight) || 
		     (pwnd->ayCursor >= pwnd->arcClipping.ayBottom)) || 
		    ((psOverlap) &&
		     (pwnd != *((PWND FAR *) 
		        MAKELONG( sizeof(PWND) *
				        (pwnd->ayCursor*axMac + pwnd->axCursor),
				  psOverlap
		   )))         ))
			MoveHardwareCursor(0, 0, FALSE);
		else
			{
			MoveHardwareCursor(pwnd->axCursor, pwnd->ayCursor,
			    pwnd->fCursorOn ? fBlockCursor : FALSE);
			}
		}
	else
		{
		MoveHardwareCursor(pwnd->axCursor, pwnd->ayCursor,
		    pwnd->fCursorOn ? fBlockCursor : FALSE);
		}
	}
#endif /*WINDOW_OVERLAP*/

PUBLIC VOID FARPUBLIC
MoveCursor(pwnd, rx, ry)
/*
  -- move the character cursor to the appropriate position within the window
  -- only changes the hardware cursor for the window in focus
*/
REGISTER PWND pwnd;
RX rx;
RY ry;
	{
	StartPublic();
	RRC	rrc;

	GetClientRrc(pwnd,&rrc);
	AssertSz((rx <= rrc.rxRight) && (ry <= rrc.ryBottom),
		 "Attempt to Move Cursor Outside of Rectangle");

	pwnd->axCursor = pwnd->arcClipping.axLeft + rx;
	pwnd->ayCursor = pwnd->arcClipping.ayTop + ry;
	if (pwnd == pwndFocus)
#ifdef WINDOW_OVERLAP
		MoveCursorClipped(pwnd);
#else
		{
		MoveHardwareCursor(pwnd->axCursor, pwnd->ayCursor,
		    pwnd->fCursorOn ? fBlockCursor : FALSE);
		}
#endif /*WINDOW_OVERLAP*/
	StopPublic();
	}



PRIVATE VOID FARPRIVATE
UpdateCursor()
/*
  -- call this to update the state of the hardware cursor
*/
	{
	REGISTER PWND pwnd;

	if ((pwnd = pwndFocus) != NULL)
		{
		DrawThisWnd(pwnd);
#ifdef WINDOW_OVERLAP
		MoveCursorClipped(pwnd);
#else
		MoveHardwareCursor(pwnd->axCursor, pwnd->ayCursor,
		    pwnd->fCursorOn ? fBlockCursor : FALSE);
#endif /*WINDOW_OVERLAP*/
		}
	else
		{
		/* no window with focus */
		MoveHardwareCursor(0, 0, FALSE);
		}
	}



PUBLIC VOID FARPUBLIC
EnableCursor(pwnd, fOn)
/*
  -- enable/disable cursor in given window
*/
REGISTER PWND pwnd;
BOOL fOn;	/* TRUE => turn on */
	{
	StartPublic();
	pwnd->fCursorOn = (fOn != FALSE);
	if (pwnd == pwndFocus)
		{
		DrawThisWnd(pwnd);
#ifdef WINDOW_OVERLAP
		MoveCursorClipped(pwnd);
#else
		MoveHardwareCursor(pwnd->axCursor, pwnd->ayCursor,
		    pwnd->fCursorOn ? fBlockCursor : FALSE);
#endif /*WINDOW_OVERLAP*/
		}
	StopPublic();
	}


PUBLIC VOID FARPUBLIC
SetCursorBlock(fBlock)
/*
  -- set cursor size	
*/
BOOL fBlock;	/* 0-underline, 1-block */
	{
	fBlockCursor = fBlock ? 2 : 1;
	UpdateCursor();
	}
		

PUBLIC VOID FARPUBLIC
MoveWindow(pwnd, ax, ay)
/*
  -- move the window to absolute coordinates (ax, ay).
  -- update all absolute position items in the WND structure

*/
REGISTER PWND pwnd;
AX ax;
AY ay;
	{
	StartPublic();
	BYTE dax = ax - pwnd->arcWindow.axLeft;
	BYTE day = ay - pwnd->arcWindow.ayTop;

	/* Note : following explicit & 0xff is needed due to Cmerge 4.0
		bug with LOBYTE() */
#ifndef WINDOW_OVERLAP
	AssertSz(((pwnd->arcWindow.ayBottom + day) & 0xff) <= ayMac &&
	    ((pwnd->arcWindow.axRight + dax) & 0xff) <= axMac,
	    "Moving rectangle off the screen");
#endif

	pwnd->arcWindow.axLeft = ax;
	pwnd->arcWindow.ayTop = ay;
	pwnd->arcWindow.axRight += dax;
	pwnd->arcWindow.ayBottom += day;

	pwnd->axCursor += dax;
	pwnd->ayCursor += day;
	if (pwnd == pwndFocus)
		{
#ifdef WINDOW_OVERLAP
		MoveCursorClipped(pwnd);
#else
		MoveHardwareCursor(pwnd->axCursor, pwnd->ayCursor,
		    pwnd->fCursorOn ? fBlockCursor : FALSE);
#endif /*WINDOW_OVERLAP*/
		}

#ifdef WINDOW_OVERLAP
	MoveWindowAndSiblings(pwnd->pwndChild, dax, day);
#endif /*WINDOW_OVERLAP*/

	ValidateWindow(pwnd);

	StopPublic();
	}


#ifdef WINDOW_OVERLAP
STATIC VOID
MoveWindowAndSiblings(pwnd, dax, day)
/*
  -- move the window to absolute coordinates (ax, ay).
  -- update all absolute position items in the WND structure
*/
REGISTER PWND pwnd;
BYTE dax;
BYTE day;
	{
	while (pwnd != NULL)
		{
		pwnd->arcWindow.axLeft += dax;
		pwnd->arcWindow.ayTop += day;
		pwnd->arcWindow.axRight += dax;
		pwnd->arcWindow.ayBottom += day;

		pwnd->axCursor += dax;
		pwnd->ayCursor += day;
		if (pwnd == pwndFocus)
			{
			MoveCursorClipped(pwnd);
			}

		MoveWindowAndSiblings(pwnd->pwndChild, dax, day);
		pwnd = pwnd->pwndSibling;
		}
	}
#endif /*WINDOW_OVERLAP*/




PUBLIC VOID FARPUBLIC
SaveRrc(pwnd, prrc, lpbBuf)
/*
  -- restores the relative rectangle prrc in the near buffer pbBuf
  -- MERGE ABS/REL
*/
PWND	pwnd;
PRRC	prrc;
BYTE FAR * lpbBuf;
	{
	StartPublic();
	ARC arc;

	AssertCorrectRrc(prrc);
	if (FArcFromRrc(pwnd, prrc, &arc))
		{
		DrawThisWnd(pwnd);	/* this is sort of stupid */
		SaveArc(arc.axLeft, arc.ayTop, arc.axRight, arc.ayBottom,
		    lpbBuf);
		}
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
RestoreRrc(pwnd, prrc, lpbBuf)
/*
  -- restores the relative rectangle prrc in the near buffer pbBuf
  -- MERGE ABS/REL
*/
PWND	pwnd;
PRRC	prrc;
BYTE FAR * lpbBuf;
	{
	StartPublic();
	ARC arc;

	AssertCorrectRrc(prrc);
	if (FArcFromRrc(pwnd, prrc, &arc))
		{
		DrawThisWnd(pwnd);
		RestoreArc(arc.axLeft, arc.ayTop, arc.axRight, arc.ayBottom,
		    lpbBuf);
		}
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
CharOut(pwnd, rx, ry, ch, di)
/*
  -- put single character in window
  -- MERGE ABS/REL
*/
REGISTER PWND pwnd;
RX	rx;
RY	ry;
ACHAR	ch;
WORD	di;
	{
	if (pwnd != NULL)
		{
		rx += pwnd->arcClipping.axLeft;
		ry += pwnd->arcClipping.ayTop;
		if (rx >= pwnd->arcClipping.axRight ||
		    ry >= pwnd->arcClipping.ayBottom)
			return;		/* clip that character */
		}

	DrawThisWnd(pwnd);
	CharOutAbs((AX) rx, (AY) ry, ch, di);
	}



