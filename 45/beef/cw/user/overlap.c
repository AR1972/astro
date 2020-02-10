
/*
	COW : Character Oriented Windows

	overlap.c  : Overlapping Window Support

*/


#define COW
#include <cow.h>

#define WINDOW
#include <uwindow.h>
#include <uevent.h>
#include <umenu.h>
#include <uisa.h>
#include <uscreen.h>
#include <vkey.h>
#include <kkeyboar.h>
#include <kinput.h>

#include "event.h"
#include "screen.h"
#include "util.h"
#include "dialog.h"
#include "window.h"
#include "shadow.h"
#include "overlap.h"


#ifdef WINDOW_OVERLAP	/* entire file */

extern void MemSetW(WORD, WORD, WORD, WORD);
extern PWND pwndClip;

/* forward */
STATIC VOID ThinkWindowAndSiblings(PWND);
STATIC BOOL FAllocDragSave(void);
STATIC VOID InitDragRrc(PWND);
STATIC VOID SaveDragRrc(void);
STATIC VOID RestoreDragRrc(void);
STATIC VOID DisplayDrag(void);
STATIC VOID EndDrag(void);
STATIC BOOL FAdjustDragRrc(int *, int *);


PRIVATE BOOL fDragging = FALSE;	/* am moving a window */
PRIVATE OWDS owds;		/* current window drag status */
				/* is valid only if fDragging == TRUE */
/*
  -- OWDF's contain the overlapping window drag flags
  -- rgowdf contains the initial settings for when a mouse clicks
          on the window.
  -- owdfKeyboardMove/Size contain the initial settings for keyboard
          moves and sizes
  -- format: { fMouse, fButton, fOutline, fXDrag, fYDrag, fEscapeMove, message}
*/
PRIVATE OWDF rgowdf[] =
	{
		{ 1, 1, 0, 0, 0, 0, WM_CLOSE },	/* top left corner */
		{ 1, 0, 1, 0, 0, 0, WM_MOVE },  /* top side */
		{ 1, 1, 0, 0, 0, 0, WM_ZOOM },	/* top right corner */
		{ 1, 0, 1, 0, 0, 0, WM_MOVE },	/* left side */
		{ 0, 0, 0, 0, 0, 0, 0 },	/* middle */
		{ 1, 0, 1, 1, 0, 0, WM_SIZE },	/* right side */
		{ 1, 0, 1, 0, 0, 0, WM_MOVE },	/* bottom left corner */
		{ 1, 0, 1, 0, 1, 0, WM_SIZE },	/* bottom side */
		{ 1, 0, 1, 1, 1, 0, WM_SIZE }	/* bottom right corner */
	};

PRIVATE OWDF owdfKeyboardMove = { 0, 0, 1, 0, 0, 0, WM_MOVE };
PRIVATE OWDF owdfKeyboardSize = { 0, 0, 1, 1, 1, 0, WM_SIZE };

PRIVATE PWND pwndCur;		/* current drawing window */
PRIVATE WORD psOverlap;		/* overlapping window table */

VOID FARPUBLIC
AddChildHead(pwndParent, pwndChild)
/*
  -- add child to head of child list
*/
PWND pwndParent, pwndChild;
	{
	StartPublic();

	Assert(pwndParent != NULL && pwndChild != NULL);

	pwndChild->pwndSibling = pwndParent->pwndChild;
	pwndChild->pwndParent = pwndParent;
	pwndParent->pwndChild = pwndChild;
	if (pwndParent->style & WS_CLIPOUT)
		{
		pwndChild->style |= WS_CLIPOUT;
		ClipoutWindowAndSiblings(pwndChild->pwndChild);
		}
	StopPublic();
	}

VOID FARPRIVATE
ClipoutWindowAndSiblings(pwnd)
/*
  -- pwnd inherits the clipout type along with its children
*/
PWND	pwnd;
	{

	while (pwnd != NULL)
		{
		pwnd->style |= WS_CLIPOUT;
		ClipoutWindowAndSiblings(pwnd->pwndChild);
		pwnd = pwnd->pwndSibling;
		}
	}



BOOL FARPUBLIC
FIsTopWindow(pwnd)
PWND	pwnd;
	{
	StartPublic();
	ReturnPublic(pwnd->pwndSibling == NULL, BOOL);
	}

PWND FARPUBLIC
PwndGetTopWindow(pwnd)
PWND	pwnd;
	{
	StartPublic();
	while (pwnd->pwndSibling != NULL)
		pwnd = pwnd->pwndSibling;
	ReturnPublic(pwnd, PWND);
	}

VOID FARPUBLIC
AddChildTail(pwndParent, pwndChild)
PWND pwndParent, pwndChild;
/*
  -- add child to tail of child list
*/
	{
	StartPublic();
	PWND pwndCur, pwndPrev;

	Assert(pwndParent != NULL && pwndChild != NULL);

	if ((pwndPrev = pwndParent->pwndChild) == NULL)
		pwndParent->pwndChild = pwndChild;
	else
		{
		while ((pwndCur = pwndPrev->pwndSibling) != NULL)
			pwndPrev = pwndCur;
		pwndPrev->pwndSibling = pwndChild;
		}
	pwndChild->pwndParent = pwndParent;
	if (pwndParent->style & WS_CLIPOUT)
		{
		pwndChild->style |= WS_CLIPOUT;
		ClipoutWindowAndSiblings(pwndChild->pwndChild);
		}
	StopPublic();
	}


VOID FARPRIVATE
DrawThisWndProc(pwnd)
REGISTER PWND pwnd;
/*
  -- sets the current window for clipping purposes
  -- NULL means no overlap clipping
*/
	{
	if (!(pwnd->style & WS_CLIPOUT) || !(psOverlap))
		{
		/* window isn't clipout type or there is no overlap table */
		pwndCur = NULL;
		return;
		}
	pwndCur = pwnd;
	}
	


STATIC VOID
ThinkWindowAndSiblings(pwnd)
REGISTER PWND pwnd;
/*
  -- fills the overlap table for pwnd and its children
  -- the overlap table is an array that maps screen positions onto the
	window that is in the foreground at a given position
  -- this is accomplished by walking through the tree and filling the
	table for each window.  hence the window that is lower in the
	tree has higher priority and overwrites window that are higher.
*/
	{
	while (pwnd != NULL)
		{
		WORD	off;			/* pointer to array position */
		AY	ay;
		ARC	arc;
		WORD	cwRow;			/* Count of words in each row */

		if ((pwnd->pwndParent != NULL) && (pwnd->style & WS_CLIPOUT))
			{
			if ((pwnd->style & WS_TYPE) == WS_SCROLL)
				{
				Assert(pwnd->pwndParent->pwndParent != NULL);
				IntersectRect((PRRC) &arc,
					 (PRRC) &pwnd->arcWindow, (PRRC)
				    &pwnd->pwndParent->pwndParent->arcClipping);
				}
			else
				{
				IntersectRect((PRRC) &arc,
					(PRRC) &pwnd->arcWindow,
					(PRRC) &pwnd->pwndParent->arcClipping);
				}
			}
		else
			arc = pwnd->arcWindow;

		off = (arc.ayTop * axMac + arc.axLeft) * sizeof(PWND);
		cwRow = arc.axRight - arc.axLeft;

		if (cwRow != 0)
			{
			/* Now simply work through each row */
			for (ay = arc.ayTop; ay < arc.ayBottom; ay++)
				{
				Assert(sizeof(PWND) == sizeof(WORD));
				/* otherwise this won't work */
				MemSetW(psOverlap, off,(WORD)pwnd,cwRow);
				off += axMac * sizeof(PWND);
				}
			}
		ThinkWindowAndSiblings(pwnd->pwndChild);
		pwnd = pwnd->pwndSibling;
		}
	}




VOID FARPUBLIC
RethinkDisplay()
/*
  -- recalculate overlap table
*/
	{
	StartPublic();

	/* first fill in base windows */
	if (psOverlap != 0)
		MemSetW(psOverlap, 0, NULL, axMac * ayMac);

	ThinkWindowAndSiblings(pwndRoot);

	StopPublic();
	}


VOID FARPUBLIC
RedrawDamagedRegions()
/*
  -- This function will redraw only the damaged regions of the screen
  -- Especially after a window has been moved/Deleted/ etc.
*/
	{
	StartPublic();

#ifdef LATER
    -- Put optimizer in here
#endif /* LATER */

	RethinkDisplay();   /* For now just rethink display on draw */
	DrawWindow(NULL);   /* All windows */

	StopPublic();
	}


VOID FARPUBLIC
DrawOverlapShadow(pwnd)
PWND	pwnd;
/*
  -- does a shadow for an overlapping window
*/
	{
	StartPublic();
	AX	daxShadowSave = daxShadow;

	Assert(pwnd->pwndParent != NULL);

	pwndClip = pwnd->pwndParent;
	DrawThisWnd(pwnd->pwndParent);
	ShadowArc(&pwnd->arcWindow);
	daxShadow /= 2;
	DrawThisWnd(NULL);
	ShadowArc(&pwnd->arcWindow);
	daxShadow = daxShadowSave;
	pwndClip = NULL;

	StopPublic();
	}


BOOL FARPUBLIC
FWindowToTop(pwnd)
PWND pwnd;
/*
  -- bring window to tail of child list
*/
	{
	StartPublic();
	PWND pwndParent = pwnd->pwndParent;
	PWND pwndOldTop = PwndGetTopWindow(pwnd);

	Assert(pwndParent != NULL && pwnd != NULL);

	if(SendMessage( pwndOldTop, WM_ACTIVATE, FALSE, 0L))
		{
		if(SendMessage(pwnd, WM_ACTIVATE, TRUE, 0L))
			{
			RemoveChild(pwnd);
			AddChildTail(pwndParent, pwnd);
			ThinkWindowAndSiblings(pwndOldTop);
			DrawWindow(pwndOldTop);
			DrawWindow(pwnd);
			UpdateCursor();
			ReturnPublic(TRUE,BOOL);
			}
		}

	ReturnPublic(FALSE,BOOL);
	}





BOOL FARPUBLIC
FMoveSizeWithKeyboard(pwnd,fMove)
PWND	pwnd;
BOOL	fMove;		/* true move false size */
/*
  -- Moves or sizes the window with the cursor
*/
	{
	StartPublic();
	MSG	msg;
	PWND	pwndRootOld = pwndRoot;
	PWND	pwndFocusOld = pwndFocus;
	PWND	pwndCaptureOld = pwndCapture;
	PFFN_FILTER	pfnFilterOld = pfnFilter;

	AssertSz(((pwnd->style & WS_CLIPOUT) && (pwnd->style & WS_OVERLAP)),
			"can only move overlap windows");

	Assert(!fDragging);

	if (!FIsTopWindow(pwnd))
		{
		if (!FWindowToTop(pwnd))
			ReturnPublic(FALSE,BOOL);
		}

	if (fMove)
		owds.owdf = owdfKeyboardMove;
	else
		owds.owdf = owdfKeyboardSize;

	owds.pwndMoving = pwnd;
	owds.pwndBackground = pwnd->pwndParent;

	if (!FAllocDragSave()) ReturnPublic(FALSE,BOOL);

	InitDragRrc(pwnd);
	fDragging = TRUE;	/* we are dragging and owds is valid */
	SaveDragRrc();
	DisplayDrag();

	pfnFilter = DummyFilter;
	pwndRoot = NULL;	/* set stuff to null so mouse input 
				   doesn't muck us up */
	pwndFocus = NULL;
	pwndCapture = NULL;
	UpdateCursor();

	/* we don't return control to the application until we're
	   done dragging */

	while(fDragging)
		{
		PollKeyboard();

		while (PeekMessage(&msg))
			DispatchMessage(&msg);
		}

	pfnFilter = pfnFilterOld;
	pwndRoot = pwndRootOld;
	pwndFocus = pwndFocusOld;
	pwndCapture = pwndCaptureOld;

	EndDrag();

	if (owds.owdf.fEscapeMove)
		{
		owds.owdf.fEscapeMove = 0;
		ReturnPublic(FALSE,BOOL);
		}

	UpdateCursor();
	ReturnPublic(TRUE,BOOL);
	}



BOOL PASCAL
OverlapFilter(pmsg)
PMSG	pmsg;
/*
  -- Does handling for overlapping windows
*/
	{
	AX		ax;		/* The Abs position of event */
	AY		ay;		/* The Abs position of event */
	short		iZoneX,iZoneY;	/* Location inside the window */
	PWND		pwnd = pmsg->pwnd;

	ax = LOBYTE(HIWORD(pmsg->lParam));        /* Get x, y position */
	ay = HIBYTE (HIWORD(pmsg->lParam));

	switch (pmsg->message)
		{
	case WM_LBUTTONDOWN:
		if (fDragging)
			return(FALSE);
		if (!(pwnd->style & WS_CLIPOUT))
			return(FALSE);

			/*
			 * find the overlapping window that we have
			 * clicked upon and then bring it to the top
			 * if it is not already there
			*/
			{
			PWND	pwndNewTop;

			pwndNewTop = pwnd;
			while (!(pwndNewTop->style & WS_OVERLAP)
					&&(pwndNewTop != NULL))
				pwndNewTop = pwndNewTop->pwndParent;
			if ((pwndNewTop == NULL) || (!pwndNewTop->fEnabled))
				 return(FALSE);
			if (!FIsTopWindow(pwndNewTop))
				{
				if (!FWindowToTop(pwndNewTop)) return(FALSE);
				}
			}

		if (!(pwnd->style & WS_OVERLAP))
			return(FALSE);

		/* (iZoneY * 3 + iZoneX) yields:
		 *  0 - Top left corner		*  1 - Top side
		 *  2 - Top right corner	*  3 - Left side
		 *  4 - middle of window	*  5 - Right side
		 *  6 - Bottom left corner	*  7 - Bottom side
		 *  8 - Bottom right corner
		 */

		iZoneY = (ay == pwnd->arcWindow.ayTop) ? 0 :
			 ((ay == pwnd->arcWindow.ayBottom-1) ? 2 : 1);
		iZoneX = (ax == pwnd->arcWindow.axLeft) ? 0 :
			 ((ax == pwnd->arcWindow.axRight-1) ? 2 : 1);

		if (iZoneY * 3 + iZoneX == 4)  /* not on the border */
			return(FALSE);

		owds.owdf = rgowdf[iZoneY * 3 + iZoneX];

		owds.pwndMoving = pwnd;
		owds.pwndBackground = pwnd->pwndParent;

		if (!FAllocDragSave()) return(FALSE);

		SetCapture (pwnd);  /* we want the mouse captured */
		owds.ax = ax;
		owds.ay = ay;
		fDragging = TRUE;	/* we are dragging -> owds is valid */
		InitDragRrc(pwnd);
		SaveDragRrc();
		DisplayDrag();
		return(TRUE);


	case WM_MOUSEMOVE:
		/* if we're not dragging or not dragging due to mouse */
		if ((!fDragging)||(!owds.owdf.fMouse))
			return (FALSE);

		{
		int	dax, day;
		PWND	pwndParent = pwnd->pwndParent;

		ax = (AX) max((int)ax, (int)pwndParent->arcClipping.axLeft);
		ay = (AY) max((int)ay, (int)pwndParent->arcClipping.ayTop);
		ax = (AX) min((int)ax, (int)pwndParent->arcClipping.axRight -1);
		ay = (AY) min((int)ay, (int)pwndParent->arcClipping.ayBottom-1);
		dax = ax - owds.ax;
		day = ay - owds.ay;

		if (FAdjustDragRrc(&dax, &day))
			{
			if (owds.owdf.fOutline)
				{
				owds.ax = (AX) ((int) owds.ax + dax);
				owds.ay = (AY) ((int) owds.ay + day);
				}
			SaveDragRrc();
			DisplayDrag();
			}
		}
		return (TRUE);


	case WM_LBUTTONUP:
		if ((!fDragging) || (!owds.owdf.fMouse))
			return (FALSE);
		ReleaseCapture ();
		EndDrag();
		return (TRUE);


	case WM_CHAR:
		if (HIWORD(pmsg->lParam) & KK_MENU)
			return(FALSE);
		if ((fDragging) && (!owds.owdf.fMouse))
			{
			int	dax = 0, day = 0;

			switch(LOBYTE(pmsg->wParam))
				{
			default:
				return(FALSE);

			case LOBYTE(VK_LEFT):
				dax = -1;
				break;
			case LOBYTE(VK_RIGHT):
				dax = 1;
				break;
			case LOBYTE(VK_DOWN):
				day = 1;
				break;
			case LOBYTE(VK_UP):
				day = -1;
				break;
			case LOBYTE(VK_ESCAPE):
				owds.owdf.fEscapeMove = 1;
			case LOBYTE(VK_RETURN):
				fDragging = FALSE;
				return (TRUE);
				}

			if (HIWORD(pmsg->lParam) & KK_CONTROL)
				{
				dax *= 8;
				day *= 4;
				}
			if (owds.owdf.message == WM_MOVE)
				{
				/* make sure that we don't move off
				 * the right side or bottom
				*/
				dax = min( dax,
				 (int) owds.pwndBackground->arcClipping.axRight
				  - 1 - (int) AxOfRx(owds.pwndBackground,
					 owds.rrc.rxLeft));
				day = min( day,
				 (int) owds.pwndBackground->arcClipping.ayBottom
				  - 1 - (int) AyOfRy(owds.pwndBackground,
					owds.rrc.ryTop));
				}
			else
				{
				/* stop window from extending beyond
				 * right or bottom
				*/
				Assert(owds.owdf.message == WM_SIZE);
				dax = min( dax,
				 (int) owds.pwndBackground->arcClipping.axRight
				  - (int) AxOfRx(owds.pwndBackground,
					 owds.rrc.rxRight));
				day = min( day,
				 (int) owds.pwndBackground->arcClipping.ayBottom
				  - (int) AyOfRy(owds.pwndBackground,
					owds.rrc.ryBottom));
				}
			if (FAdjustDragRrc(&dax, &day))
				{
				SaveDragRrc();
				DisplayDrag();
				}
			return(TRUE);
			}
		}

	return(FALSE);
	}




STATIC BOOL
FAllocDragSave()
/*
  -- Allocate the save buffers, return FALSE if not possible
*/
	{
	RRC	rrc;

	if (owds.owdf.fOutline)
		{
		RX	rxSave;

		GetClientRrc(owds.pwndBackground, &rrc);

		/*
		 * We allocate enough space to save the top side
		 * right, left and bottom sides of the moveable outline.
		*/
		rxSave		= rrc.rxRight;
		rrc.rxRight	= 1;
		owds.cbSaveSide	= CbSizeRrc(&rrc);
		rrc.rxRight	= rxSave;
		rrc.ryBottom	= 1;
		owds.cbSaveTop	= CbSizeRrc(&rrc);

		owds.lpbSave	= LpbAllocWorkFar(owds.cbSaveTop * 2
						 + owds.cbSaveSide * 2);

		if (owds.lpbSave == NULL)
			return(FALSE);
		}
	else
		{
		/* we just need enough space to save the
		 * corner button
		*/
		rrc.rxLeft = rrc.ryTop = 0;
		rrc.ryBottom = 1;
		rrc.rxRight = 3;

		owds.cbSaveTop = CbSizeRrc(&rrc);

		owds.lpbSave = LpbAllocWorkFar(owds.cbSaveTop);

		if (owds.lpbSave == NULL)
			return(FALSE);
		}

	return(TRUE);
	}




STATIC VOID
InitDragRrc(pwnd)
PWND	pwnd;
/*
  -- initialize the rrcDrag
*/
	{
	if (owds.owdf.fOutline)
		{
		/* moveable outline is initialized to be the border of
		 * the window being moved
		*/
		owds.rrc.rxLeft
			 = RxOfAx(owds.pwndBackground,pwnd->arcWindow.axLeft);
		owds.rrc.rxRight
			 = RxOfAx(owds.pwndBackground,pwnd->arcWindow.axRight);
		owds.rrc.ryTop
			 = RyOfAy(owds.pwndBackground,pwnd->arcWindow.ayTop);
		owds.rrc.ryBottom
			 = RyOfAy(owds.pwndBackground,pwnd->arcWindow.ayBottom);
		}
	else
		{
		/* for a corner button, we save the corner and a space
		 * to the left and right
		*/
		owds.rrc.rxLeft = RxOfAx(owds.pwndBackground,owds.ax);
		owds.rrc.ryTop = RyOfAy(owds.pwndBackground,owds.ay);
		owds.rrc.rxRight = owds.rrc.rxLeft + 2;
		owds.rrc.ryBottom = owds.rrc.ryTop + 1;
		if (owds.rrc.rxLeft > 0) owds.rrc.rxLeft--;
		}
	}



STATIC VOID
DisplayDrag()
/*
  -- display the border or button
*/
	{
	DrawThisWnd(NULL);
	if (owds.owdf.fOutline)
		{
		ARC	arc;

		arc.axLeft	= AxOfRx(owds.pwndBackground,owds.rrc.rxLeft);
		arc.ayTop	= AyOfRy(owds.pwndBackground,owds.rrc.ryTop);
		arc.axRight	= AxOfRx(owds.pwndBackground,owds.rrc.rxRight);
		arc.ayBottom	= AyOfRy(owds.pwndBackground,owds.rrc.ryBottom);

		pwndClip = owds.pwndBackground;
		DrawBoxArc(&boxActiveWindowOut, &arc, isaButtonDown,
				 TRUE, TRUE, NULL);
		pwndClip = NULL;
		}
	else
		{
		if (owds.owdf.fButton)
			{
			CharOutAbs(owds.ax,owds.ay,chBullet, isaButtonDown);
			if (owds.ax > owds.pwndBackground->arcClipping.axLeft)
				CharOutAbs(owds.ax-1,owds.ay,'<',isaButtonDown);
			if (owds.ax <owds.pwndBackground->arcClipping.axRight-1)
				CharOutAbs(owds.ax+1,owds.ay,'>',isaButtonDown);
			}
		}
	}


STATIC VOID
SaveDragRrc()
/*
  -- save the  rrcDrag
*/
	{
	if (owds.owdf.fOutline)
		{
		/* just save the outline around the rrcDrag */
		RRC	rrc;

		rrc = owds.rrc;
		rrc.ryBottom = rrc.ryTop + 1;	/* save the top side */
		SaveRrc(owds.pwndBackground, &rrc, owds.lpbSave);
		rrc.ryBottom = owds.rrc.ryBottom;

		rrc.rxRight = rrc.rxLeft + 1;	/* save the left side */
		SaveRrc(owds.pwndBackground, &rrc, owds.lpbSave
						 + owds.cbSaveTop);
		rrc.rxRight = owds.rrc.rxRight;

		rrc.ryTop = rrc.ryBottom - 1;	/* save the bottom side */
		SaveRrc(owds.pwndBackground, &rrc, owds.lpbSave
				 + owds.cbSaveTop + owds.cbSaveSide);
		rrc.ryTop = owds.rrc.ryTop;

		rrc.rxLeft = rrc.rxRight - 1;	/* save the right side */
		SaveRrc(owds.pwndBackground, &rrc, owds.lpbSave
				 + 2*owds.cbSaveTop + owds.cbSaveSide);
		rrc.rxLeft = owds.rrc.rxLeft;
		}
	else
		{
		/* for a corner button */
		SaveRrc(owds.pwndBackground, &owds.rrc, owds.lpbSave);
		}
	}



STATIC VOID
RestoreDragRrc()
/*
  -- save the rrcDrag
*/
	{
	if (owds.owdf.fOutline)
		{
		RRC	rrc;

		rrc = owds.rrc;
		rrc.ryBottom = rrc.ryTop + 1;	/* restore top side */
		RestoreRrc(owds.pwndBackground, &rrc, owds.lpbSave);
		rrc.ryBottom = owds.rrc.ryBottom;

		rrc.rxRight = rrc.rxLeft + 1;	/* restore left side */
		RestoreRrc(owds.pwndBackground, &rrc, owds.lpbSave
						 + owds.cbSaveTop);
		rrc.rxRight = owds.rrc.rxRight;

		rrc.ryTop = rrc.ryBottom - 1;   /* restore the bottom */
		RestoreRrc(owds.pwndBackground, &rrc, owds.lpbSave 
					+ owds.cbSaveTop + owds.cbSaveSide);
		rrc.ryTop = owds.rrc.ryTop;

		rrc.rxLeft = rrc.rxRight - 1;	/* restore the right side */
		RestoreRrc(owds.pwndBackground, &rrc, owds.lpbSave 
					+ 2*owds.cbSaveTop + owds.cbSaveSide);
		rrc.rxLeft = owds.rrc.rxLeft;
		}
	else
		{
		RestoreRrc(owds.pwndBackground, &owds.rrc, owds.lpbSave);
		}
	}



STATIC BOOL
FAdjustDragRrc(pdax, pday)
int	*pdax;
int	*pday;
/*
  -- adjust the dragged rectangle, restore the old one if necessary
  -- return true if adjustment was made
  -- dax and day must already be adjusted so that the window doesn't
     go off the right side or the bottom (since the cursor and keyboard
     cases are different)
*/
	{
	int	dax = *pdax;
	int	day = *pday;

	if (owds.owdf.fOutline)
		{
		if (owds.owdf.message == WM_MOVE)
			{
			/* don't go off the left or top */
			dax = max(dax, - (int) owds.rrc.rxLeft);
			day = max(day, - (int) owds.rrc.ryTop);
			}
		else
			{
			/* don't size too small */
			Assert(owds.owdf.message == WM_SIZE);
			if (owds.owdf.fXDrag)
				dax = max(dax, (int) axMinSize + (int)
					owds.rrc.rxLeft -(int)owds.rrc.rxRight);
			else
				dax = 0;
			if (owds.owdf.fYDrag)
				day = max(day, (int) ayMinSize + (int)
					owds.rrc.ryTop -(int)owds.rrc.ryBottom);
			else
				day = 0;
			}

		if ((dax == 0) && (day == 0))
			return(FALSE);

		RestoreDragRrc();

		owds.rrc.rxRight  = (RX) ((int) owds.rrc.rxRight + dax);
		owds.rrc.ryBottom = (RY) ((int) owds.rrc.ryBottom + day);
		if (owds.owdf.message == WM_MOVE)
			{
			owds.rrc.rxLeft = (RX) ((int) owds.rrc.rxLeft + dax);
			owds.rrc.ryTop  = (RY) ((int) owds.rrc.ryTop + day);
			}
		}
	else
		{ /* dealing with a corner button */
		if (((dax > -2) && (dax < 2)) && (day == 0))
			{
			if (owds.owdf.fButton)
				return(FALSE);
			else
				owds.owdf.fButton = 1;
			}
		else
			{
			if (!(owds.owdf.fButton))
				return(FALSE);
			else
				owds.owdf.fButton = 0;
			}

		RestoreDragRrc();
		}

	*pdax = dax;
	*pday = day;
	return(TRUE);
	}

VOID PRIVATE
EndDrag()
/*
  -- deallocate the buffers and send the message and restore the screen
*/
	{
	WORD	wParam = 0;
	DWORD	lParam = 0L;

	fDragging = FALSE;
	RestoreDragRrc();
	FreeWorkFar(owds.lpbSave);

	if ((!(owds.owdf.fOutline) && !(owds.owdf.fButton))
				   || owds.owdf.fEscapeMove)
		return;

	if (owds.owdf.fOutline)
		{
		/*
		 * include the location and size of the moved window
		 * ax,ay in the high word, dax,day in the low word
		*/
		lParam = MAKELONG(
			MAKEWORD((owds.rrc.ryBottom - owds.rrc.ryTop),
				 (owds.rrc.rxRight - owds.rrc.rxLeft)), 
			MAKEWORD(AyOfRy(owds.pwndBackground,owds.rrc.ryTop),
				 AxOfRx(owds.pwndBackground,owds.rrc.rxLeft))
				 );
		}

	SendMessage(owds.pwndMoving, owds.owdf.message, wParam, lParam);
	RedrawDamagedRegions();
	UpdateCursor();
	}

#endif /* WINDOW_OVERLAP, entire file */
