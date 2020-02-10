/*
	COW : Character Oriented Windows

	general.c : WndProc for general pictures
*/

#define COW
#include <cow.h>

#include <uwindow.h>
#include <uevent.h>

/* really integrated into SDM */
#include <sdmver.h>
#include <usdm.h>
#include <usdmtmpl.h>
#include "sdm.h"

#include "dialog.h"
#include "general.h"


PRIVATE DWORD FARPUBLIC			/* WndProcs are PUBLIC */
GeneralWndProc(pwnd, message, wParam, lParam)
/*
  -- WndProc for general pictures
*/
REGISTER PWND pwnd;
WORD message, wParam;
DWORD lParam;
	{
	Unreferenced(lParam);

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
#ifdef	KANJI
		FlushDraw();
#endif	// KANJI
		(*PwfnCtlGeneral(pwnd))(tmmPaint, (VOID *) pwnd, NULL, wParam,
		    0, 0);
		break;
		}
	return TRUE;
	}



VOID FARPUBLIC
RedisplayTmc(tmc)
TMC tmc;
	{
	SendMessage(PtifFromTmc(tmc)->pwnd, WM_PAINT, tmc, 0L);
	}
 
