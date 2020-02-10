/*
	COW : Character Oriented Windows

	scroll.c : scroll bar control
*/

#define COW
#include <cow.h>

#define SCROLL

#include <uscroll.h>
#include <uwindow.h>
#include <uscreen.h>
#include <uisa.h>
#include <uevent.h>
#include <kinput.h>

#include "dialog.h"
#include "event.h"
#include "util.h"
#include "screen.h"

#include "scroll.h"
#include "_scroll.h"



/*
	scroll bar slots

ptCurSb		current logical position in range 
ptMinSb		minimum logical position in scroll range 
ptMaxSb		maximum logical position in scroll range 
wSb		extra word for info defined below
*/



/* ScrollBarWndProc(pwnd, message, wParam, lParam) - 
*  Process scroll bar messages.
*  Purpose:
*	Handles user interaction with scroll bars.
*
*	Scroll Bar:
*	^	<--- Up Arrow
*	+	<-------------------
*	+     ^                     ^
*	+     |                     |
*	+     |                     |
*	+     +--- Page Up Area     |
*	+     |                     |
*	+     |                     |
*	+     v                     |
*	+  <---                     |
*	#	<------ Elevator         +--- Scroll range
*	+  <---                     |
*	+     ^                     |
*	+     |                     |
*	+     |                     |
*	+     +--- Page Down Area   |
*	+     |                     |
*	+     |                     |
*	+     v                     v
*	+	<-------------------
*	v	<--- Down Arrow
*
*	The scroll bar manager sends WM_VSCROLL or WM_HSCROLL messages to
*	its Parent when a scroll bar event is detected.
*	The value of the wParam and lParam parameters of this
*       message determine what event has occured.
*
*	wParam			Event
*	------			-----
*	SB_LINEUP		The mouse was clicked on the Up Arrow
*	SB_LINEDOWN		The mouse was clicked on the Down Arrow
*	SB_PAGEUP		The mouse was clicked in the Page Up Area
*	SB_PAGEDOWN		The mouse was clicked in the Page Down Area
*	SB_THUMBPOSITION	The elevator was dragged to a new position.
*	SB_THUMBTRACK		The elevator has moved but the mousebutton
*				is still down.
*
*	The Application sees the elevator as being in the logical range
*	ptMin..ptMax. These logical values are used for communication between
*	the Scroll Bar Manager, and the Application. 
*
*
*****************************************************************************/


PUBLIC DWORD FARPUBLIC 
ScrollBarWndProc(pwnd, message, wParam, lParam)
/*
  -- the real scroll bar Wnd Proc
*/
REGISTER PWND pwnd;
DWORD lParam;
WORD message, wParam;
	{
	StartPublic();
	RRC rrc;
	BYTE ptDownLine, ptElevator;
	short ptNew;
	MSP msp;
	WORD messageScroll;
	WORD wNonClient;		/* value to mask for non-client */
	BOOL fSmallScroll;

	/* statics for the scroll bar with capture */
	static WORD wParamScroll;
	static BOOL fInWindow = TRUE;
	static BYTE ptStartElevator;
	static BYTE ptMouse;
	BYTE pt;		/* the position if a mouse message */
	
	GetClientRrc(pwnd, &rrc);
	/* set up parameter based on whether horizontal or vertical scroll */
	ptElevator = PtElevatorSb(pwnd);
	msp.lParam = lParam;

	if (pwnd->style & SBS_VERT)
		{
		messageScroll = WM_VSCROLL;
		ptDownLine = rrc.ryBottom-rrc.ryTop-1;
		pt = msp.s.ry;
		wNonClient = MK_NONCLIENT_Y;
		fSmallScroll = rrc.ryBottom - rrc.ryTop == dyScrollMin;
		}
	else 
		{
		messageScroll = WM_HSCROLL;
		ptDownLine = rrc.rxRight-rrc.rxLeft-1;
		pt = msp.s.rx;
		wNonClient = MK_NONCLIENT_X;
		fSmallScroll = rrc.rxRight - rrc.rxLeft == dxScrollMin;
		}

	switch (message) 
		{
	default:
		break;
		
	case WM_PAINT:
		/* draw the scroll bar */
#ifdef DEBUG
		if (pwnd->style & SBS_VERT)
			{
			AssertSz(rrc.ryBottom - rrc.ryTop >= dyScrollMin,
			    "Scroll bars too small");
			}
		else
			{
			AssertSz(rrc.rxRight - rrc.rxLeft >= dxScrollMin,
			    "Scroll bars too small");
			}
#endif /*DEBUG*/
#ifndef REMOVE_LATER
		if (pwnd->style & SBS_VERT)
			{
			rrc.ryTop++;
			rrc.ryBottom--;
			}
		else
			{
			rrc.rxLeft++;
			rrc.rxRight--;
			}
#endif

		BeginDraw();

		FillRrc(pwnd, &rrc, (ACHAR) chScrollbar, DiNormal(isaScrollbar));

		if (pwnd->style & SBS_VERT)
			{
			CharOut(pwnd, 0, 0, chUpArrow,
			    DiNormal(isaScrollbar));
			CharOut(pwnd, 0, ptDownLine, chDownArrow,
			    DiNormal(isaScrollbar));
			if (!fSmallScroll)
				CharOut(pwnd, 0, ptElevator, chElevator,
				    DiNormal(isaElevator));
			}
		else 
			{
			CharOut(pwnd, 0, 0, chLeftArrow,
			    DiNormal(isaScrollbar));
			CharOut(pwnd, ptDownLine, 0, chRightArrow,
			    DiNormal(isaScrollbar));
			if (!fSmallScroll)
				CharOut(pwnd, ptElevator, 0, chElevator,
				    DiNormal(isaElevator));
			}

		EndDraw();

		break;
		
	case WM_LBUTTONDOWN:
	case WM_LBUTTONDBLCLK:
		fInWindow = TRUE;
		/* capture mouse for repeated scrolling or thumbtrack */
		SetCapture(pwnd);

		if ((ptMouse = pt) == 0)
			wParamScroll = SB_LINEUP;
		else if (ptMouse == ptDownLine)
			wParamScroll = SB_LINEDOWN;
		else if (ptMouse > ptElevator)
			wParamScroll = SB_PAGEDOWN;
		else if (ptMouse < ptElevator)
			wParamScroll = SB_PAGEUP;
		else 
			{
			/* capturing the elevator */
			wParamScroll = SB_THUMBPOSITION;
			ptStartElevator = ptElevator;
			break;	/* do not set alarm or send message */
			}

		SetAlarm(pwnd, ctickRepScrollStart);
		SendMessage(pwnd->pwndParent, messageScroll,
		    wParamScroll, MAKELONG(0, (WORD) pwnd));

		break;
		
	case WM_MOUSEMOVE:
		/* mouse movements are only used for Thumb-tracking */

		/* see if mouse is still in the window */
		fInWindow = FCaptured(pwnd) && !(wParam & wNonClient);
		if (fSmallScroll)
			break;		/* not thumbtrack possible */

		ptMouse = pt;

		if (wParamScroll == SB_THUMBPOSITION) 
			{
			if (!fInWindow)
				/* reset thumb */
				ptMouse = ptStartElevator;
			if (ptMouse != ptElevator &&
			    ptMouse > 0 && 
			    ptMouse < ptDownLine)
				{
				/* change thumbposition */
				SetPtElevatorSb(pwnd, ptMouse);
				/* ??? efficiency ??? */
				if (pwnd->style & SBS_VERT)
					{
					CharOut(pwnd, 0, ptElevator, 
					    (ACHAR) chScrollbar,
					    DiNormal(isaScrollbar));
					/*maybe*/CharOut(pwnd, 0, ptMouse,
					    chElevator, DiNormal(isaElevator));
					}
				else 
					{
					CharOut(pwnd, ptElevator, 0, 
					    (ACHAR) chScrollbar,
					    DiNormal(isaScrollbar));
					/*maybe*/CharOut(pwnd, ptMouse, 0,
					    chElevator, DiNormal(isaElevator));
					}
				/* translate physical to logical; round up */
				ptNew = TranslatePosition(ptMouse, 
				    1, ptDownLine-1,
				    pwnd->ptMinSb, pwnd->ptMaxSb, TRUE);

				pwnd->ptCurSb = ptNew;
				SendMessage(pwnd->pwndParent, messageScroll,
				    SB_THUMBTRACK,
				    MAKELONG(ptNew, (WORD) pwnd));
				}
			}
		break;
		
	case WM_ALARM:
		if (FCaptured(pwnd))
			{
			SetAlarm(pwnd, pwnd->ctickRepSb);
			if (fInWindow) 
				{
				switch (wParamScroll) 
					{
				default:
					goto ReturnFalse;

				case SB_LINEUP:
					if (ptMouse != 0)
						goto ReturnFalse;
					break;
					
				case SB_LINEDOWN:
					if (ptMouse != ptDownLine)
						goto ReturnFalse;
					break;
					
				case SB_PAGEUP:
					if (ptMouse == 0 || 
					    ptMouse >= ptElevator)
						goto ReturnFalse;
					break;
					
				case SB_PAGEDOWN:
					if (ptMouse == ptDownLine ||
					    ptMouse <= ptElevator)
						goto ReturnFalse;
					break;
					
					}
				SendMessage(pwnd->pwndParent, messageScroll,
				    wParamScroll, MAKELONG(0, (WORD) pwnd));
				}
			}
		break;
		
	case WM_LBUTTONUP:
		if (wParamScroll == SB_THUMBPOSITION && !fSmallScroll)
			{
			/* end tracking elevator */
			ptNew = TranslatePosition(ptElevator, 1, ptDownLine-1, 
			    pwnd->ptMinSb, pwnd->ptMaxSb, TRUE);

			if (ptNew != -1)	/* -1 if calculation error */
				{
				SendMessage(pwnd->pwndParent, messageScroll,
				    SB_THUMBPOSITION,
				    MAKELONG(ptNew, (WORD) pwnd));

				SendMessage(pwnd->pwndParent, messageScroll,
				    SB_ENDSCROLL,
				    MAKELONG(ptNew, (WORD) pwnd));
				}
			}
		SendMessage(pwnd->pwndParent, messageScroll, SB_UPCLICK, 0L);
		wParamScroll = 0;
		if (FCaptured(pwnd))
			{
			ReleaseCapture();
			KillAlarm();
			}
		break;
		
#ifdef DEBUG
	case WM_KEYDOWN:
	case WM_KEYUP:
	case WM_CHAR:		/* just an extra check */
		Assert(FALSE);
#endif

		}
ReturnFalse:
	ReturnPublic(0L, DWORD);
	}



PUBLIC VOID FARPUBLIC
SetScrollRange(pwnd, ptMin, ptMax, fRedraw)
/*
  -- set the scroll range for the scrollbar window "pwnd"
  -- 
*/
PWND pwnd;
short ptMin, ptMax;
BOOL fRedraw;
	{
	StartPublic();

	AssertSz(ptMax > ptMin, "SetScrollRange : invalid range");
	pwnd->ptCurSb = pwnd->ptMinSb = ptMin;
	pwnd->ptMaxSb = ptMax;
	if (PtElevatorSb(pwnd) == 0)
		SetPtElevatorSb(pwnd, 1);
	if (fRedraw)
		DrawWindow(pwnd);
	StopPublic();
	}



PUBLIC short FARPUBLIC
SetScrollPos(pwnd, ptNew, fRedraw)
/*
  -- set the scroll position for the scrollbar window "pwnd"
  -- useless operation if scroll-bar is 2-wide
*/
PWND pwnd;
short ptNew;
BOOL fRedraw;
	{
	StartPublic();
	short ptOld = pwnd->ptCurSb;
	RRC rrc;
	WORD ptMax;

	AssertSz((short) pwnd->ptMaxSb >= (short) pwnd->ptMinSb, "SetScrollPos : invalid range");
	AssertSz(ptNew >= pwnd->ptMinSb && ptNew <= pwnd->ptMaxSb,
	    "SetScrollPos : out of range");

	GetClientRrc(pwnd, &rrc);
	pwnd->ptCurSb = ptNew;
	if (pwnd->style & SBS_VERT)
		{
		ptMax = rrc.ryBottom - 2;	/* one position for downline */
		if (rrc.ryBottom - rrc.ryTop == dyScrollMin)
			ReturnPublic(ptOld, short);
		}
	else
		{
		ptMax = rrc.rxRight - 2;	/* one position for downline */
		if (rrc.rxRight - rrc.rxLeft == dxScrollMin)
			ReturnPublic(ptOld, short);
		}

	/* translate from logical to physical range, round down */
	ptNew = TranslatePosition(ptNew, pwnd->ptMinSb, pwnd->ptMaxSb,
				  1, ptMax, FALSE);
	Assert(!(ptNew & 0xff00));
  	SetPtElevatorSb(pwnd, ptNew);
	/* redraw elevator */
	if (fRedraw)
		DrawWindow(pwnd);

	ReturnPublic(ptOld, short);
	}	



PUBLIC short FARPUBLIC
GetScrollPos(pwnd)
PWND pwnd;
	{
	StartPublic();
	ReturnPublic(pwnd->ptCurSb, short);
	}


