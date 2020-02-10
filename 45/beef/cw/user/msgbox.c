/*
	COW : Character Oriented Windows

	msgbox.c : message boxes
*/

#define COW
#define cwExtraWnd	5	/* for static structure size */
#include <cow.h>

#include <udialog.h>
#include <uevent.h>
#include <uwindow.h>
#include <uisa.h>
#include <uutil.h>
#ifdef	HELP_BUTTON
#include <itl.h>
#endif	// HELP_BUTTON

#include "dialog.h"
#include "event.h"
#include "window.h"
#include "button.h"
#include "util.h"

#include "shadow.h"

#include "strings.h"

#include "_msgbox.h"

#ifdef DUAL
extern	WORD	fProtectMode;
#endif

/* forward */
STATIC VOID PrepareMbs(MBS *);

#ifdef ADJUST_MSGBOX
AY FAR PASCAL GetUILine (void);
#endif
#ifdef	DIALOG_NOSAVE
extern VOID	FAR PASCAL	ScreenRedraw(VOID);
#endif	// DIALOG_NOSAVE

PRIVATE DWORD FARPUBLIC MboxWndProc(PWND, WORD, WORD, DWORD);

/* NOTE : using chAccel's as acl's assumes 1st letter uniqueness (ich == 0) */

STATIC WND PASCAL wndDialog =
	wndGeneric(0, WS_CHILD | WS_BORDER | WS_DIALOG, 1, 0, 0, 1, 1,
		  DialogWndProc, NULL, NULL, NULL)
	{aclNil, NULL /* no caption */, 0}
	endWndGeneric;

STATIC WND wndCancel =
	wndGenericCursor(IDCANCEL, wsPushButton, 1, 0, 0, 0xff, 1,
		   ButtonWndProc, &wndDialog, NULL, NULL, 0, 0)
	{aclNil /* no accelerator */, (WORD) szCancelString, 0xff }
	endWndGeneric;

STATIC WND wndNo =
	wndGenericCursor(IDNO, wsPushButton, 1, 0, 0, 0xff, 1,
		   ButtonWndProc, &wndDialog, &wndCancel, NULL, 0, 0)
	{/* chAccelNo */ 0, (WORD) szNoString, 0xff }
	endWndGeneric;

STATIC WND wndOk =
	wndGenericCursor(IDOK, wsDefPushButton, 1, 0, 0, 0xff, 1,
		   ButtonWndProc, &wndDialog, NULL, NULL, 0, 0)
	{aclNil /* no accelerator */, (WORD) szOkString, 0xff }
	endWndGeneric;

STATIC WND wndYes =
	wndGenericCursor(IDYES, wsDefPushButton, 1, 0, 0, 0xff, 1,
		   ButtonWndProc, &wndDialog, &wndNo, NULL, 0, 0)
	{/* chAccelYes */ 0, (WORD) szYesString, 0xff }
	endWndGeneric;

STATIC WND wndRetry =
	wndGenericCursor(IDRETRY, wsDefPushButton, 1, 0, 0, 0xff, 1,
		   ButtonWndProc, &wndDialog, &wndCancel, NULL, 0, 0)
	{/* chAccelRetry */ 0, (WORD) szRetryString, 0xff }
	endWndGeneric;

STATIC WND wndAbort =
	wndGenericCursor(IDABORT, wsDefPushButton, 1, 0, 0, 0xff, 1,
		   ButtonWndProc, &wndDialog, NULL, NULL, 0, 0)
	{/* chAccelAbort */ 0, (WORD) szAbortString, 0xff }
	endWndGeneric;

#ifdef	HELP_BUTTON
static WND wndHelp =
	wndGenericCursor(IDHELP, wsDefPushButton, 1, 0, 0, 0xff, 1,
		   ButtonWndProc, &wndDialog, NULL, NULL, 0, 0)
	{/* chAccelHelp */ 0, (WORD) szHelpString, 0xff }
	endWndGeneric;
#endif	// HELP_BUTTON

/***
 * Define a bit mask for which buttons are used in the different message
 * box types each nibble from reverse to front is a index into the
 * array of button windows.
 */
STATIC WORD mpmbrgn[]=	/* rgn = array of 4 nibbles */
#ifdef	HELP_BUTTON
	{0x0071, 0x7234, 0x0725, 0x0721, 0x0076, 0x0734, 0x075};
#else	// HELP_BUTTON
 	{0x0001, 0x0234, 0x0025, 0x0021, 0x0006, 0x0034, 0x005};
#endif	// HELP_BUTTON

STATIC PWND rgpwndButton[] =
#ifdef	HELP_BUTTON
	{NULL, &wndOk, &wndCancel, &wndNo, &wndYes, &wndRetry, &wndAbort, &wndHelp};
#else	// HELP_BUTTON
	{NULL, &wndOk, &wndCancel, &wndNo, &wndYes, &wndRetry, &wndAbort};
#endif	// HELP_BUTTON


STATIC MBS *	pmbsCur;		/* current state */



PUBLIC WORD FARPUBLIC
MessageBox(sz0, sz1, sz2, mb)
/*
  -- display a message box.
  -- see udialog.h for allowed MB_xxxxxx values
  -- Up to three lines of text can be displayed in the message box.
*/
char *sz0, *sz1, *sz2;
WORD mb;
	{
	StartPublic();

	MBS	mbs;			/* the message-box state */
	REGISTER MBI *pmbi;

	if (mb & MB_CAPTION)
		{
		AssertSz(sz0 != NULL, "MessageBox(MB_CAPTION) but no caption");
		mbs.szTitle = sz0;
		sz0 = NULL;
		}
	else
		{
		mbs.szTitle = NULL;		/* no caption */
		}

#ifdef	HELP_BUTTON
	mbs.fNoHelp = (mb & MB_NOHELP);
#endif	// HELP_BUTTON

	mbs.rgmbi[0].sz = sz0;
	mbs.rgmbi[1].sz = sz1;
	mbs.rgmbi[2].sz = sz2;

	/* Figure out how many lines we have, what their lengths are, and
		what the maximum length is. */
#ifdef	HELP_BUTTON
#ifdef	BUTTON_CENTER
	mbs.cchMac = mpmbcchButton[(mbs.mbSimple = mb & MB_TYPE) - mbMin] -
				   (mbs.fNoHelp ? 3+(cchHelp+2) : 0);
#else	// BUTTON_CENTER
	mbs.cchMac = mpmbcchButton[(mbs.mbSimple = mb & MB_TYPE) - mbMin] -
				   (mbs.fNoHelp ? 2+(cchHelp+2) : 0);
#endif	// BUTTON_CENTER
#else	// HELP_BUTTON
	mbs.cchMac = mpmbcchButton[(mbs.mbSimple = mb & MB_TYPE) - mbMin];
#endif	// HELP_BUTTON
	mbs.imbiMac = 0;

	for (pmbi = mbs.rgmbi; pmbi < &mbs.rgmbi[imbiMax]; pmbi++)
		{
		if (pmbi->sz == NULL)
			continue;		/* first NULL ends */
		if ((pmbi->cch = strlen(pmbi->sz)) > mbs.cchMac)
			{
			if (pmbi->cch > axMac - 6)
				pmbi->cch = axMac - 6;
			mbs.cchMac = pmbi->cch;
			}
		mbs.imbiMac++;
		}
	AssertSz(mbs.imbiMac != 0, "MessageBox(NULL,NULL,NULL,...)");

	/* Calculate start location of each of the strings */
	for (pmbi = mbs.rgmbi; pmbi < &mbs.rgmbi[imbiMax]; pmbi++)
		pmbi->rx = (RX) ((mbs.cchMac + 4 - pmbi->cch) >> 1);

	PrepareMbs(&mbs);

	if (mb & MB_BEEP)
		Beep();

	ReturnPublic(DialogBox(&wndDialog, MboxWndProc), WORD);
	}




STATIC VOID
PrepareMbs(pmbs)
/*
  -- given a message box state, prepare the global windows to reflect this
	new state, and set pmbsCur to pmbs
*/
REGISTER MBS *	pmbs;
	{
	AX		ax;
	AY		ayButton;
	WORD		rgn;		/* array of 4 nibbles */
	PWND *		ppwndPrev;

	wndDialog.szDialog = (WORD) pmbs->szTitle;

#ifdef ADJUST_MSGBOX
	{
	extern BYTE fAdjustMsgBox;
	AY day = (AY) pmbs->imbiMac + dayMsgBox + dayPushButton;
	AY ay = (ayMac - pmbs->imbiMac - dayMsgBox - dayPushButton) / 2;
	AY ayUI = GetUILine ();
	AX dax = (AX) pmbs->cchMac + 6;

	SetWindowSize(&wndDialog, dax, day);
	if (fAdjustMsgBox && ayUI >= ay && ayUI < (ay + day))
		MoveWindow(&wndDialog, (axMac - dax) / 2, ayMac - (day+1));
	else
		MoveWindow(&wndDialog, (axMac - dax) / 2, ay);
	}
#else
	/* Build the main dialog box */
	SetWindowSize(&wndDialog, (BYTE) pmbs->cchMac+6,
	    (BYTE) pmbs->imbiMac + dayMsgBox + dayPushButton);
	MoveWindow(&wndDialog, (axMac - pmbs->cchMac - 6) / 2,
	    (ayMac - pmbs->imbiMac - dayMsgBox - dayPushButton) / 2);
#endif /*ADJUST_MSGBOX*/

#ifdef	HELP_BUTTON
#ifdef	BUTTON_CENTER
	/* more complicated button centering to match TWIN */
	ax = (((wndDialog.arcClipping.axRight - wndDialog.arcClipping.axLeft) -
	       mpmbcchButton[pmbs->mbSimple - mbMin] +
	       (pmbs->fNoHelp ? 3+(cchHelp+2) : 0)) / 2) +
	     wndDialog.arcClipping.axLeft;
#else
	ax = wndDialog.arcClipping.axRight -
	     mpmbcchButton[pmbs->mbSimple - mbMin] +
	     (pmbs->fNoHelp ? 2+(cchHelp+2) : 0);
#endif	/*!BUTTON_CENTER*/
#else	// HELP_BUTTON
#ifdef	BUTTON_CENTER
	/* more complicated button centering to match TWIN */
	ax = (((wndDialog.arcClipping.axRight - wndDialog.arcClipping.axLeft) -
	       mpmbcchButton[pmbs->mbSimple - mbMin]) / 2) +
	     wndDialog.arcClipping.axLeft;
#else
	ax = wndDialog.arcClipping.axRight -
	     mpmbcchButton[pmbs->mbSimple - mbMin];
#endif	/*!BUTTON_CENTER*/
#endif	// HELP_BUTTON
	ayButton = wndDialog.arcClipping.ayBottom - dayPushButton;

	rgn = mpmbrgn[pmbs->mbSimple - mbMin];
	Assert(rgn != 0);	/* must have at least 1 */

	ppwndPrev = &wndDialog.pwndChild;

	do
		{
		REGISTER PWND 	pwnd;
		REGISTER MBB	*pmbb;

	 	/* Point to the correct button for the item */
		pwnd = rgpwndButton[rgn & 0xf];
		Assert(pwnd != NULL);
		pmbb = &rgmbb[rgn & 0xf];

		*ppwndPrev = pwnd;		/* link to prev */
		ppwndPrev = &pwnd->pwndSibling;

		/* Set the button value to zero */
		pwnd->wButton = 0;

		/* Make sure we have the accelerator set right */
		Assert(aclNil == 0);
		pwnd->aclDialog = pmbb->chAccel;

		/* Setup the window size and position */
		pwnd->cchDialog = pmbb->cch;
		MoveWindow(pwnd, ax, ayButton);
		SetWindowSize(pwnd, pmbb->drx, dayPushButton);
		/* Setup position of next button */
#ifdef BUTTON_CENTER
		ax += (AX) (pmbb->drx + 3);
#else
		ax += (AX) (pmbb->drx + 2);
#endif /*!BUTTON_CENTER*/
		}
#ifdef HELP_BUTTON
	while ((rgn >>= 4) != 0 &&	/* next nibble */
	       (!pmbs->fNoHelp || ((rgn >> 4) != 0)));/* Skip last nibble if fNoHelp */
#else	// HELP_BUTTON
	while ((rgn >>= 4) != 0);	/* next nibble */
#endif	// HELP_BUTTON
	*ppwndPrev = NULL;

	/* set global for information */
	pmbsCur = pmbs;
	}



PRIVATE DWORD FARPUBLIC		/* WndProcs PUBLIC SIZE */
MboxWndProc(pwnd, message, wParam, lParam)
/*
  -- dialog specific procedure for message boxes
*/
REGISTER PWND pwnd;
WORD message, wParam;
DWORD lParam;
	{
	Unreferenced(lParam);

	switch (message)
		{
	default:
		break;

	case WM_PAINT:
		{
		REGISTER MBS *pmbs = pmbsCur;
		REGISTER MBI *pmbi;
		RY ry = 1;

		for (pmbi = pmbs->rgmbi; pmbi < &pmbs->rgmbi[imbiMax]; pmbi++)
			if (pmbi->sz != NULL)
				TextOut(pwnd, pmbi->rx, ry++, (char *) pmbi->sz,
				    pmbi->cch, isaMessageBox);
		}
		break;

	case WM_DIALOG:
#ifdef HELP_BUTTON
		if (wParam == IDHELP)
			{
			wParam = 0;	/* No shift states for Help */
			goto do_help;
			}
#endif
		/* pass button hit to who invoked dialog box */
		EndDialog(pwnd, wParam);
		break;

	case WM_HELP:
#ifdef HELP_BUTTON
do_help:
#endif
		/* HELP : this may in turn call MessageBox recursively,
		*  so we must re-initialize the message box
		*/
		{
		MBS *	pmbsSave = pmbsCur;

		Help(hemMbox, pmbsCur->mbSimple, NULL, wParam);
			/* wParam contains the shift states */

		/* restore the world */
		PrepareMbs(pmbsSave);
#ifdef	DIALOG_NOSAVE
		DrawWindow(&wndDialog);
#endif	// DIALOG_NOSAVE
		UpdateCursor();
		wndDialog.wParamEnd = 0;    /* not finished this dialog yet */
		}
		break;

#ifdef DUAL
	case WM_IDLE:
		/* for DOS 5, in idle return TRUE (=> dialog processor
		*  will put us to sleep
		*/
		if (fProtectMode)
			return TRUE;		/* default idle */
#else /*!DUAL*/
#ifdef DOS5
	case WM_IDLE:
		/* for DOS 5, in idle return TRUE (=> dialog processor
		*  will put us to sleep
		*/
		return TRUE;		/* default idle */
#endif
#endif /*!DUAL*/
		}
	return (0L);
	}
