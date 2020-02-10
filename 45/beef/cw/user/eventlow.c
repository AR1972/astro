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

#ifdef KK_UNIT
BOOL	FARPUBLIC KKFilter(PMSG);
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

/* ----------------------------------------------------------------------- */

#ifdef RECORD_PLAYBACK
/* Forward declarations */
BOOL FAR PASCAL PlayBackMsg(MSG *);
VOID FAR PASCAL RecordMsg(MSG *);
#endif


#ifdef PROFILE
int PASCAL crefCow = 0;		/* reference count of Cow Public entries */
#endif

#ifdef DOS5
/* NOTE : semaphore is CLEARED when a message is ready !! */
PUBLIC DWORD PASCAL semaMessage = 0;	/* RAM semaphore */
#else
#ifdef	DUAL
PUBLIC DWORD PASCAL semaMessage = 0;	/* RAM semaphore */
#endif
PUBLIC BOOL PASCAL fMessage = FALSE;
#endif

PRIVATE PWND PASCAL pwndRoot = NULL;		/* root window */

MSG msgNull = {0, WM_NULL, 0, 0l, timeMsgMax};	/* a NULL message */

MSGQ msgqAppl = {0, &msgNull, &msgqAppl.rgmsg[0]};
MSGQ msgqKeyboard = {0, &msgNull, &msgqKeyboard.rgmsg[0]};
MSGQ msgqMouse = {0, &msgNull, &msgqMouse.rgmsg[0]};

PMSG pmsgLast = &msgNull;
	/* last message queued used by keyboard and mouse handlers */
PMSG pmsgLastKeyboard = &msgNull;	/* for TSR kludge */

/* window with the focus */
PWND pwndFocus = NULL;
/* window with the mouse capture */
PWND pwndCapture = NULL;

/* Filter of messages */
PRIVATE BOOL FARPRIVATE DummyFilter(PMSG);	/* forward declaration */
PRIVATE PFFN_FILTER pfnFilter = DummyFilter;
#ifdef WINDOW_OVERLAP
PRIVATE PFFN_FILTER pfnOverlapFilter = OverlapFilter;
#endif /*WINDOW_OVERLAP*/

/* Mouse control */
WORD timeDoubleClick = timeDoubleClickDefault;
MSP mspPrev = { 0xff, 0xff, 0xff, 0xff }; /* position of last move */

/* for 1 level of UngetMessage */
BYTE fUseCache = FALSE;
MSG msgCache;

/* Menu Info : see note in MENU.H */
PRIVATE MNI mniCur = { imenuNil, iitemNil, NULL, FALSE, FALSE, FALSE };

/* ----------------------------------------------------------------------- */

/* forward */
STATIC BOOL FNextMsg(MSG *);
STATIC VOID DequeueTopMsg(MSGQ *);
STATIC PWND PwndLocate(PWND, AX, AY);
STATIC VOID CheckDoubleClick(MSG *);

/* ----------------------------------------------------------------------- */


PUBLIC BOOL FARPUBLIC
PeekMessage(pmsg)
/*
  -- check to see if a message is ready
  -- return TRUE if *pmsg filled with a message
  -- return FALSE if no messages pending
  -- always poll the keyboard
*/
PMSG pmsg;
	{
	StartPublic();

	do
		{
		/* don't always poll the keyboard */
		if (fPollKeyboard)
			PollKeyboard();

		ClearMessage();
		fAbort = FALSE;

		/* try to get a message from Cache or Queue */
		if (fUseCache)
			{
			*pmsg = msgCache;
			fUseCache = FALSE;
			/* if cached message is keyboard, use the current
				focus */
			if (msgCache.message >= WM_KEYFIRST &&
			    msgCache.message <= WM_KEYLAST)
				pmsg->pwnd = pwndFocus;
			}
		else
			{
			if (!FNextMsg(pmsg))
				{
				/* event's background process */
				if (!FCheckAlarm(pmsg))
					ReturnPublic(FALSE, BOOL);
				}
#ifdef RECORD_PLAYBACK
			else
				RecordMsg(pmsg);
#endif
			}
		}
#ifndef WINDOW_OVERLAP
#ifdef	KK_UNIT
	while (KKFilter(pmsg) || (*pfnFilter)(pmsg));
#else	// KK_UNIT
	while ((*pfnFilter)(pmsg));
#endif	// KK_UNIT
#else
	while ((*pfnFilter)(pmsg) || (*pfnOverlapFilter)(pmsg));
#endif /*WINDOW_OVERLAP*/

	ReturnPublic(TRUE, BOOL);
	}



STATIC BOOL
FNextMsg(pmsgDest)
/*
  -- Obtain the next message from the queue by looking at the three event queues
  -- return FALSE if there are no messages
  -- fill *pmsgDest with the message
*/
PMSG pmsgDest;
	{
	/* Get a message from each of the queues */
	PMSG pmsgAppl, pmsgKeyboard, pmsgMouse;

retry:
	/* If the menu is active, block application messages */
	pmsgAppl = FMenuActive() ? &msgNull : msgqAppl.pmsgHead;
	pmsgKeyboard = msgqKeyboard.pmsgHead;
	pmsgMouse = msgqMouse.pmsgHead;

	if (pmsgKeyboard->time < pmsgAppl->time)
		{
		if (pmsgMouse->time < pmsgKeyboard->time)
			goto mouse_event;
		/* keyboard earliest */

		pmsgKeyboard->pwnd = pwndFocus;

		*pmsgDest = *pmsgKeyboard;

		DequeueTopMsg(&msgqKeyboard);
		pmsgLastKeyboard = pmsgLast;
#ifdef DOS3
		/* if TSR present, poll to get next message */
		fPollKeyboard = TRUE;
#endif /*DOS3*/

		/* if message is a shift state change, call UpdateShiftKk */
		if (pmsgDest->message == WM_KEYSTATE)
			{
			static WORD	ssOld = 0;
#ifdef KANJI
			static WORD	ssOldKj = 0;
#endif

			UpdateShiftKk(pmsgDest->wParam, ssOld);
			ssOld = pmsgDest->wParam;
#ifdef KANJI
			UpdateShiftKj(LOWORD(pmsgDest->lParam), ssOldKj);
			ssOldKj = LOWORD(pmsgDest->lParam);
#endif
			goto retry;		/* get another */
			}
		}
	else if (pmsgMouse->time < pmsgAppl->time)
		{
		/* mouse earliest */
	mouse_event:
		*pmsgDest = *pmsgMouse;
		DequeueTopMsg(&msgqMouse);
		/* clean up mouse message */
		FindMouseWnd(pmsgDest);
		CheckDoubleClick(pmsgDest);
#ifdef DEBUG
		/* RIGHT BUTTON USED AS DEVELOPMENT HOOK */
		if (pmsgDest->message == WM_RBUTTONDOWN &&
		    FTrapMouseRbutton())
			goto retry;
#endif /*DEBUG*/
		}
	else if (pmsgAppl->time != timeMsgMax)
		{
		/* application earliest */
		*pmsgDest = *pmsgAppl;
		DequeueTopMsg(&msgqAppl);
		}
	else
		{
		/* all 3 queues had null messages when we entered */
#ifdef RECORD_PLAYBACK
		{
		static BOOL fPlayback = FALSE;

		if ((fPlayback = !fPlayback) && PlayBackMsg (pmsgDest))
			{
			if (pmsgDest->message >= WM_MOUSEFIRST &&
			    pmsgDest->message <= WM_MOUSELAST)
				FindMouseWnd(pmsgDest);
			else
				pmsgDest->pwnd = pwndFocus;
			return TRUE;
			}
		}
#endif
		if (!FMenuActive())
			return FALSE;	/* no message available */
		/* never return if inside menu */
		*pmsgDest = msgNull;
		}
	return TRUE;
	}



STATIC VOID
DequeueTopMsg(pmsgq)
/*
  -- dequeue the top message of the specified queue
  -- if pmsgLast == the message being dequeued, then set pmsgLast to &msgNull
  -- the queue must be originally non-empty
  -- if queue gets empty, stick &msgNull in head slot
*/
MSGQ *pmsgq;
	{
	DisableInterrupts();
	AssertSz(pmsgq->cmsg > 0, "Dequeueing an empty queue");
	if (pmsgLast == pmsgq->pmsgHead)
		pmsgLast = &msgNull;
	if (--pmsgq->cmsg == 0)
		pmsgq->pmsgHead = &msgNull;
	else if (++(pmsgq->pmsgHead) == &pmsgq->rgmsg[imsgMax])
		pmsgq->pmsgHead = &pmsgq->rgmsg[0];
	EnableInterrupts();
	}



PUBLIC VOID FARPUBLIC
FlushAbort()
/*
  -- Flush all keyboard messages up to the first ESCAPE
  -- for TSR keyboard input there should be 1 escape key only
  -- can be called from TSR with SS != DS
*/
	{
	REGISTER MSG *pmsg;

	/* if a key in the cache, then flush it */
	if (fUseCache && msgCache.message >= WM_KEYFIRST &&
	    msgCache.message <= WM_KEYLAST)
		{
		/* it was a keyboard message */
		fUseCache = FALSE;
		if (msgCache.message == WM_CHAR && msgCache.wParam == VK_ESCAPE)
			return;	/* already have an escape */
		}

	while ((pmsg = msgqKeyboard.pmsgHead) != &msgNull)
		{
		BOOL	fDone;

		fDone = pmsg->wParam == LOBYTE(VK_ESCAPE);
		DequeueTopMsg(&msgqKeyboard);
		if (fDone)
			break;		/* quit after first ESCAPE */
		}
	}



#ifdef	KANJI
VOID FAR PASCAL /* called by kanji keyboard driver */
KeyboardMessage(sc, vw, wParamKey, kk, fKeyUp)
#else	// !KANJI
VOID FAR PASCAL	/* called by keyboard driver */
KeyboardMessage(vw, wParamKey, kk, fKeyUp)
#endif	// KANJI
/*
  -- called by PollKeyboard for polled input
  -- normally generate only WM_CHAR Messages
  -- two special cases for WM_KEYUP :
		VK_SPACE		(needed for buttons)
		VK_MENU			(needed for menus)
  -- all events at this level set Message
*/
#ifdef	KANJI
BYTE sc;		/* Scan code for KANJI KK-unit */
#endif	// KANJI
BYTE vw;		/* Windows virtual key */
WORD wParamKey;		/* wParam for WM_KEY */
REGISTER WORD kk;	/* KK shift states */
BOOL fKeyUp;		/* TRUE => WM_KEYUP, FALSE => WM_CHAR */
	{
	WORD message;
	WORD vk;				/* the virtual key only */
#ifdef KANJI
	WORD kj;	/* KJ shift states */

	kj = kk & KJ_KK;
	Assert((kj & KJ_COUNT) == 0);
	kk &= ~KK_VK;
#endif

	Assert(vw <= 0xff);
	vk = VkOfVw(vw);			/* change virtual key */

#ifndef KANJI
	Assert((kk & KK_VK) == 0);
#endif

	if (!fKeyUp)
		{
		/* key depressed */
		message = WM_CHAR;
		if (vk == VK_ESCAPE)
			{
			/* ABORT case */
			fAbort = TRUE;
			}
		else if (pmsgLast->message == WM_CHAR &&
#ifdef KANJI
		    (pmsgLast->lParam & KJ_COUNT) < KJ_COUNT &&
#endif
		    pmsgLast->wParam == wParamKey &&
		    kk == HIWORD(pmsgLast->lParam) &&
		    /* BUG FIX FOR WORKS : don't repeat ENTER or ALT KEYS */
		    !(kk & KK_MENU) && wParamKey != LOBYTE(VK_RETURN))
			{
			/* coallese keydown repeat counts */
			pmsgLast->lParam++;
			return;		/* message already posted */
			}
		kk |= vk;
		}
	else if (vw != 0)
		{
		/* key released */
		message = WM_KEYUP;
		kk |= vk;
		}
	else
		{
		/* just change in shift state */
		/* wParamKey = kkOld, kk = kkNew */
		if (pmsgLastKeyboard->message == WM_KEYSTATE)
			{
			/* coalesce shift key transitions */
			pmsgLastKeyboard->wParam = kk;
			SetMessage();
			return;
			}

		message = WM_KEYSTATE;
		wParamKey = kk;		/* new shift states */
		}

#ifdef KANJI
	if (!FQueueMsg(&msgqKeyboard, NULL, message, wParamKey,
		       MAKELONG(MAKEWORD(1 | kj, sc), kk),
#else
	if (!FQueueMsg(&msgqKeyboard, NULL, message, wParamKey, MAKELONG(1, kk),
#endif /*KANJI*/
	    ClockTicks()))
		{
		/* buffer full !! */
		Beep();
		}

	pmsgLastKeyboard = pmsgLast;
	SetMessage();	/* after every key event seen here */
	}



VOID FAR PASCAL
MouseMessage(message)
/*
  -- post a mouse message
  -- This routine is called as part of an interrupt service routine.
	It does not ensure that SS == DS, so this routine, and any routine
	it calls MUST NOT take the address of stack variables (procedure
	parameters and locals).

	Message format :
		message = WM_...
		wParam  = MK_ code for keys down
		lParam  = mouse coordinates
			LOBYTE(LOWORD(lParam)) = rx
			HIBYTE(LOWORD(lParam)) = ry
			LOBYTE(HIWORD(lParam)) = ax
			HIBYTE(HIWORD(lParam)) = ay
*/
WORD message;
	{
	REGISTER WORD wParam = sstMouse;

	if (message == WM_MOUSEMOVE)
		{
		if (mspPrev.s.ay == ayMouse && mspPrev.s.ax == axMouse)
			return;	/* same place */
		mspPrev.s.ax = axMouse;
		mspPrev.s.ay = ayMouse;
		if (pmsgLast->message == WM_MOUSEMOVE)
			{
			/* change coordinates of last message */
			pmsgLast->lParam = mspPrev.lParam;
			return;
			}
		/* only set Message if the mouse moved with a button down */
		if (wParam != 0)
			SetMessage();
		}
	else
		{
		SetMessage();
		}

	/* get shift states from keyboard handler */
	wParam |= MkGetShiftStates();

	FQueueMsg(&msgqMouse, NULL, message, wParam,
	    mspPrev.lParam, ClockTicks());
	}


PRIVATE BOOL
FQueueMsg(pmsgq, pwnd, message, wParam, lParam, time)
/*
  -- Queue a message - return TRUE if Queued
  -- does not set Message
  -- can be called from TSR with SS != DS
*/
MSGQ *pmsgq;
PWND pwnd;
WORD message, wParam;
DWORD lParam;
DWORD time;
	{
	PMSG pmsg;
	if (pmsgq->cmsg == imsgMax)
		return(FALSE);
	pmsg = pmsgq->pmsgNext;
	/* NOTE : we can only change the head if it was pointing at msgNull */
	if (pmsgq->cmsg++ == 0)
		{
		/* count was 0 => we better be pointing to msgNull */
		Assert(pmsgq->pmsgHead == &msgNull);
		pmsgq->pmsgHead = pmsg;		/* set head */
		}
	else
		{
		Assert(pmsgq->pmsgHead != &msgNull);
		}
	if (++(pmsgq->pmsgNext) == &pmsgq->rgmsg[imsgMax])
		pmsgq->pmsgNext = &pmsgq->rgmsg[0];	/* wrap around */

	pmsgLast = pmsg;
	pmsg->pwnd = pwnd;
	pmsg->message = message;
	pmsg->wParam = wParam;
	pmsg->lParam = lParam;
	pmsg->time = time;
	return(TRUE);
	}



STATIC PWND
PwndLocate(pwnd, ax, ay)
REGISTER PWND pwnd;
AX ax;
AY ay;
/*
  -- Check if the mouse is pointing within or to a sibbling of the current
     window.
  -- returns pointer to window in containing mouse or NULL if none
*/
	{
	AssertSz(pwnd != NULL, "invalid call to PwndLocate");

	do
		{
		if ((ax < pwnd->arcWindow.axRight) &&
		    (ax >= pwnd->arcWindow.axLeft) &&
		    (ay < pwnd->arcWindow.ayBottom) &&
		    (ay >= pwnd->arcWindow.ayTop) &&
		    pwnd->fEnabled)
			return(pwnd);
		pwnd = pwnd->pwndSibling;
		}
	while (pwnd);
	return(NULL);


	}



PRIVATE VOID
FindMouseWnd(pmsg)
/*
  -- convert mouse message to indicate the receiver of the message
  -- If WINDOW_OVERLAP is defined we can simply use the Overlap array to
     convert the X,Y to a window pointer, else we must search.
*/
REGISTER PMSG pmsg;
	{
	REGISTER PWND pwnd;
	MSP msp;
	AX ax;
	AY ay;

	msp.lParam = pmsg->lParam;
	ax = msp.s.ax;
	ay = msp.s.ay;

	Assert(pmsg->message >= WM_MOUSEFIRST && pmsg->message <= WM_MOUSELAST);

	if (pwndRoot == NULL)
		{
		/* return absolute coordinates only */
		pmsg->pwnd = NULL;
		pmsg->lParam = msp.lParam;
		return;
		}

	if (pwndCapture != NULL)
		{
		pwnd = pwndCapture;
		}
	else
		{
#ifdef WINDOW_OVERLAP
		if (psOverlap)
			{
			/* psOverlap is the segment of the array containing
			 * the overlapping window array.  */
			pwnd = *((PWND FAR *) MAKELONG(
				sizeof(PWND)*(ay * axMac + ax), psOverlap));
			}
		else
#endif /* WINDOW_OVERLAP */
			{
			if (pwnd = PwndLocate(pwndRoot, ax, ay))
				{
				PWND pwndChild;
				/* found the root window containing mouse */
				/* scan till no more children or mouse is
				   outside all pwnd's children */
				while ((pwndChild = pwnd->pwndChild) != NULL &&
				    (pwndChild = PwndLocate(pwndChild, ax, ay))
				      != NULL)
					pwnd = pwndChild;
				}
			}
		}

	if (pwnd == NULL)
		{
		/* no enabled window to receive mouse input */
		Assert(pmsg->pwnd == NULL);
		return;
		}

	/* update msg with window specific info */
	pmsg->pwnd = pwnd;
	msp.s.rx = ax - pwnd->arcClipping.axLeft;
	msp.s.ry = ay - pwnd->arcClipping.ayTop;

	/* set bits for non-client */
	if (ax < pwnd->arcClipping.axLeft ||
	    ax >= pwnd->arcClipping.axRight)
		pmsg->wParam |= MK_NONCLIENT_X;
	if (ay < pwnd->arcClipping.ayTop ||
	    ay >= pwnd->arcClipping.ayBottom)
		pmsg->wParam |= MK_NONCLIENT_Y;

	pmsg->lParam = msp.lParam;
	}



STATIC VOID
CheckDoubleClick(pmsg)
/*
  -- test to see if message should be converted to a double click
*/
REGISTER PMSG pmsg;
	{
	static DWORD timeLeft = 0L;
	static DWORD timeRight = 0L;
	static DWORD lParamOld;
	if (pmsg->lParam != lParamOld)	/* mouse has moved */
		{
		lParamOld = pmsg->lParam;
		timeLeft = timeRight = 0L;
		}
	else if (pmsg->message == WM_LBUTTONDOWN)
		{
		if (timeLeft && pmsg->time - timeLeft < timeDoubleClick)
			{
			pmsg->message = WM_LBUTTONDBLCLK;
			timeLeft = 0L;
			}
		else
			{
			timeLeft = pmsg->time;
			}
		}
	else if (pmsg->message == WM_RBUTTONDOWN)
		{
		if (timeRight && pmsg->time - timeRight < timeDoubleClick)
			{
			pmsg->message = WM_RBUTTONDBLCLK;
			timeRight = 0L;
			}
		else
			{
			timeRight = pmsg->time;
			}
		}
	}



PRIVATE BOOL FARPRIVATE
DummyFilter(pmsg)
PMSG pmsg;
	{
	Unreferenced(pmsg);
	return(FALSE);
	}


VOID FAR PASCAL	/* called by keyboard driver */
SpecialTsrAbort()
/*
  -- special TSR abort occured
  -- set the fAbort flag
  -- flush the input buffer (should contain at most 1 character)
  -- insert 1 ESCAPE message
  -- can be called from TSR with SS != DS
*/
	{
#ifdef DUAL
	if (fProtectMode)
		return;
#endif /*DUAL */
#ifndef DOS5
	Assert(!fNormalKeyboard);	/* only call if TSR driven keyboard */

	fAbort = TRUE;
	FlushAbort();
	FQueueMsg(&msgqKeyboard, NULL, WM_CHAR, LOBYTE(VK_ESCAPE), MAKELONG(1, VK_ESCAPE),
	    ClockTicks());
#endif /*!DOS5*/
	}



BOOL FAR PASCAL	/* called by keyboard driver */
FTestKeyboardEmpty()
/*
  -- return TRUE if there is enough room to place 1 key event in buffer
  -- we need worst case 4 messages for each key
		(ALT DOWN, SPACE UP, key, ALT UP).
  -- do special work for TSR
*/
	{
	if (fNormalKeyboard)
		return (msgqKeyboard.cmsg < imsgMax - 4);
	else
		{
		/* if TSR (i.e. MSKEY), then only look 1 ahead (for repeat)
			but not if previous was an ALT DOWN */
		if (msgqKeyboard.cmsg > 1)
			return FALSE;		/* no chance */
		else if (msgqKeyboard.cmsg == 0)
			return TRUE;		/* we have room */
		else
			{
			/* if last event was MENU (either UP or DOWN)
				then don't allow any more */
			return (pmsgLastKeyboard->wParam != VK_MENU);
			}
		}
	}


VOID FAR PASCAL
FlushKeyEvents()
/*
  -- flush key events from CW queue
  -- put them back into keyboard buffer
*/
	{
	char	rgch[imsgMax + 10];	/* max queue size + extra */
	char *	pch = rgch;
	REG MSG * pmsg;

	while ((pmsg = msgqKeyboard.pmsgHead) != &msgNull)
		{
		DequeueTopMsg(&msgqKeyboard);
		if (pmsg->message == WM_CHAR && pmsg->wParam < VK_MIN)
			{
			/* a real character */
			*pch++ = (char) pmsg->wParam;
			}
		}
	*pch = '\0';		/* terminate array */

	inkj.pfnFlushKeyRgchKbd(rgch);
	}
