/*
	COW : Character Oriented Windows

	static.c : WndProc for static text items
*/

#define COW
#include <cow.h>

#include <uwindow.h>
#include <uevent.h>
#include <uisa.h>

#include "dialog.h"
#include "util.h"

#include "static.h"


PRIVATE DWORD FARPUBLIC			/* WndProcs are PUBLIC */
StaticWndProc(pwnd, message, wParam, lParam)
/*
  -- WndProc for static text items
*/
REGISTER PWND pwnd;
WORD message, wParam;
DWORD lParam;
	{
	RX rx;
	char *sz = (char *) pwnd->szDialog;
	BYTE cch;
	RRC rrc;
	REGISTER WORD di;

	Unreferenced(wParam);
	Unreferenced(lParam);

	Assert((pwnd->style & WS_TYPE) == WS_STATIC ||
	    (pwnd->style & WS_TYPE) == WS_STATIC_NOACCEL);

	switch(message)
		{
	default:
		break;

	case WM_WANTFOCUS:
		return((DWORD) FALSE);  /* refuse to accept the focus */
		break;

#ifdef DEBUG
	case WM_SETFOCUS:
	case WM_KILLFOCUS:
		Assert(FALSE);
		/*break;*/
#endif
		
	case WM_PAINT:
		di = pwnd->fEnabled ? isaStatic : isaDisabled;

		DrawBorder(pwnd, NULL, di, NULL);
		GetClientRrc(pwnd, &rrc);
		FillRrc(pwnd, &rrc, ' ', di);
		cch = (BYTE) CchRealLenSz(sz);
		switch(pwnd->style & WS_SUBSTYLE)
			{
		default:
			Assert(FALSE);
			break;

		case SS_LEFT:
			rx = 0;
			break;

		case SS_RIGHT:
			rx = rrc.rxRight - min(cch, rrc.rxRight);
			break;

		case SS_CENTER:
			rx = (rrc.rxRight - min(cch, rrc.rxRight)) / 2;
			break;
			}
		TextOut(pwnd, rx, 0, sz, cch, di);
		if (fShowDlgAccel &&
		    pwnd->aclDialog != aclNil &&
		    pwnd->fEnabled)
			{
			CharOut(pwnd, rx + IchAccel(pwnd), 0,
			    ChAccel(pwnd), isaDialogAccel);
			}
		break;
		}
	return TRUE;
	}
