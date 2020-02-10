/*
	COW : Character Oriented Windows

	event.c - Main event handler
*/

#define COW
#include <cow.h>

#define EVENT
#include <uevent.h>
#include <vkey.h>
#include <umenu.h>
#include <uwindow.h>
#include <uscreen.h>
#include <kinput.h>
#include <kkeyboar.h>
#include <uutil.h>
#ifdef EXTRAS
#include <cowextra.h>
#endif

#ifdef WINDOW_OVERLAP
#include "overlap.h"
#endif

#include "window.h"
#include "menu.h"
#include "util.h"

#ifdef DUAL
#include <os2.h>
extern	WORD	fProtectMode;
#else
#ifdef DOS5
#include <os2.h>
#endif
#endif

#include "event.h"
#include "_event.h"

#ifdef	KANJI
extern	BYTE	fMenu;
extern	VOID	FAR PASCAL	NotifyKKFocus(BOOL);
#endif	// KANJI

/* ------------------------------------------------------------ */

PUBLIC VOID FARPUBLIC
UngetMessage(pmsg)
/*
  -- push a message onto the front of the queue
*/
PMSG pmsg;
	{
	StartPublic();
	AssertSz(!fUseCache, "too many successive UngetMessage() calls");

	msgCache = *pmsg;
	fUseCache = TRUE;

	StopPublic();
	}



VOID FARPRIVATE
UndoRepeat(wParam, lParam)
/*
  -- call for WM_CHAR messages only !!
  -- unget a message if repeat count > 1
*/
WORD	wParam;
DWORD	lParam;
	{
#ifdef KANJI
	if ((lParam & KJ_COUNT) == 1)
#else
	if (LOWORD(lParam) == 1)
#endif
		return;		/* last key */
	Assert(!fUseCache);
	/* msgCache will be filled in later */
	msgCache.message = WM_CHAR;
	msgCache.wParam = wParam;
	/* DECREMENT LOW WORD */
	(*((WORD *) &lParam))--;
	msgCache.lParam = lParam;
	msgCache.time = 0L;			/* bogus time */
	fUseCache = TRUE;
	}


PRIVATE VOID FARPRIVATE
PostMouseUpMessages()
/*
  -- KLUDGE
  -- when restoring the capture to an old Window, we want to make sure
	that they are not waiting for a MOUSEUP message by posting a
	MOUSEUP message for both the buttons.
  -- This may cause extra MOUSEUP messages to windows which have the focus
  -- NOTE: this uses the last known mouse position and the current keyboard
	 shift states
*/
	{
	WORD	wParam = 0;

	/* get shift states from keyboard handler */
	wParam = MkGetShiftStates();

	FQueueMsg(&msgqMouse, NULL, WM_RBUTTONUP, wParam,
	    mspPrev.lParam, ClockTicks());
	FQueueMsg(&msgqMouse, NULL, WM_LBUTTONUP, wParam,
	    mspPrev.lParam, ClockTicks());
	}



PRIVATE VOID FARPRIVATE
RePostMouseMessage(message, wParam, lParam)
/*
  -- re-post a mouse message if mouse queue empty
  -- used if SetCapture / ReleaseCapture moves outside window.
*/
WORD message;
WORD wParam;
DWORD lParam;
	{
	if (msgqMouse.cmsg == 0)
		FQueueMsg(&msgqMouse, NULL, message, wParam, lParam, 0L);
		/* Note : time == 0L => remove from queue first */
	}


#ifdef EXTRAS

DWORD FARPUBLIC
SendMessageToSibbling(pwnd, message, wParam, lParam)
/*
  -- for fake overlap of windows
  -- send message to sibbling in chain if applicable
  -- capture should not be set
*/
PWND pwnd;
WORD message;
WORD wParam;
DWORD lParam;
	{
	PWND pwndSave;
	MSG msg;

	Assert(pwndCapture == NULL);
	Assert(message >= WM_MOUSEFIRST && message <= WM_MOUSELAST);
	if (PwndSibling(pwnd) == NULL)
		return FALSE;

	/* save old tree */
	pwndSave = pwndRoot;
	/* move tree temporarily */
	pwndRoot = PwndSibling(pwnd);

	msg.pwnd = NULL;
	msg.message = message;
	msg.wParam = wParam;
	msg.lParam = lParam;
	FindMouseWnd(&msg);
	Assert(msg.pwnd != NULL);

	/* move tree back */
	pwndRoot = pwndSave;

	/* send the message to sibbling (or child of sibbling) */
	return (SendMessage(msg.pwnd, msg.message, msg.wParam, msg.lParam));
	}
#endif /*EXTRAS*/


PUBLIC BOOL FARPUBLIC
PostMessage(pwnd, message, wParam, lParam)
/*
  -- queues a message
  -- returns FALSE if it cannot queue the message (queue full)
*/
PWND pwnd;
WORD message, wParam;
DWORD lParam;
	{
	StartPublic();
	SetMessage();
	ReturnPublic(FQueueMsg(&msgqAppl, pwnd, message, wParam, lParam, ClockTicks()), BOOL);
	}



PUBLIC PWND FARPUBLIC
SetCapture(pwnd)
/*
  -- Specify the window to capture mouse messages.
  -- return the old window with the mouse capture
  -- when pwndCapture is set the menu is disabled
*/
PWND pwnd;
	{
	StartPublic();
	PWND pwndRet = pwndCapture;	/* return value */

	AssertSz(pwndCapture == NULL || pwndCapture == pwnd || pwnd == NULL,
	    "Capturing a mouse twice");

	pwndCapture = pwnd;

	ReturnPublic(pwndRet, PWND);
	}



PUBLIC VOID FARPUBLIC
ReleaseCapture()
/*
  -- release the mouse capture
*/
	{
	StartPublic();
	SetCapture(NULL);
	StopPublic();
	}



PUBLIC DWORD FARPUBLIC
DispatchMessage(pmsg)
/*
  -- send message to its proper WndProc
*/
REGISTER PMSG pmsg;
	{
	StartPublic();
	REGISTER PWND pwnd;
	pwnd = pmsg->pwnd;
	if (pwnd)
		{
		/* may leave COW to process WndProc */
		ReturnLeave((*pwnd->pfnWndProc)(pwnd, pmsg->message, pmsg->wParam, pmsg->lParam), DWORD);
		}
	else
		{
		ReturnPublic(0L, DWORD);
		}
	}



DWORD FARPUBLIC
SendMessage(pwnd, message, wParam, lParam)
PWND pwnd;
WORD message;
WORD wParam;
DWORD lParam;
	{
	StartPublic();
	Assert(pwnd != NULL);
	Assert(HIWORD(pwnd->pfnWndProc) != 0);
	ReturnLeave((*pwnd->pfnWndProc)(pwnd, message, wParam, lParam), DWORD);
	}



WORD FARPRIVATE
SendMessageShort(pwnd, message)
/*
  -- shorter send message
  -- returns only WORD
*/
PWND pwnd;
WORD message;
	{
	Assert(pwnd != NULL);
	return (WORD) ((*pwnd->pfnWndProc)(pwnd, message, 0, 0L));
	}

PUBLIC PWND FARPUBLIC
GetFocus()
	{
	StartPublic();
	ReturnPublic(pwndFocus, PWND);
	}

PUBLIC PWND FARPUBLIC
SetFocus(pwnd)
/*
  -- move the input focus to window "pwnd"
  -- send the appropriate messages
  -- control the hardware cursor accordingly
  -- NOTE : weird recursion call
	if the KILLFOCUS message causes a SetFocus(), then this has
	priority (to allow SetFocus() calls for dlmChange).
*/
REGISTER PWND pwnd;
	{
	StartPublic();
	REGISTER PWND pwndOld;

	if (pwnd != (pwndOld = pwndFocus))
		{
		if (pwndOld != NULL)
			{
			SendMessageShort(pwndOld, WM_KILLFOCUS);
			}

		if (pwndFocus == pwndOld)
			{
			/* did not change due to KILLFOCUS */
			SendMessageShort(pwndFocus = pwnd, WM_SETFOCUS);
			}
		}
#ifdef	KK_UNIT
	fMenu = FALSE;
	NotifyKKFocus (TRUE);
#endif	// KK_UNIT


	UpdateCursor();

	ReturnPublic(pwndOld, PWND);
	}


#ifdef MOUSE_EXTRAS

PUBLIC WORD FARPUBLIC
SetDoubleClickTime(time)
/*
  -- 1/18 of a second for DOS 3, ms for DOS 5
*/
WORD time;
	{
	StartPublic();
	WORD timeRet = timeDoubleClick;
	timeDoubleClick = time;			/* save as word */
	ReturnPublic(timeRet, WORD);
	}

#endif /*MOUSE_EXTRAS*/
