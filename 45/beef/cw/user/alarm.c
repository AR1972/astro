/*
	COW : Character Oriented Windows
	(COW USER EVENT)

	alarm.c : alarm function
*/

#define COW
#include <cow.h>

#include <uevent.h>
#include <uutil.h>
#include <kinput.h>

#include "event.h"


STATIC DWORD ticksAlarm = 0L;
PUBLIC PWND pwndAlarm = NULL;


PUBLIC VOID FARPUBLIC
SetAlarm(pwnd, ctick)
/*
  -- set alarm to send a WM_ALARM message to window "pwnd" in "ctick" ticks
*/
PWND pwnd;
WORD ctick;
	{
	StartPublic();
	pwndAlarm = pwnd;
	KickTimer((DWORD) ctick);	/* set kick time before finding time */
	ticksAlarm = ClockTicks() + ctick;
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
KillAlarm()
	{
	StartPublic();
	pwndAlarm = NULL;
	KickTimer(ctickIdle);			/* idle timer thread */
	StopPublic();
	}



PRIVATE BOOL FARPRIVATE
FCheckAlarm(pmsg)
MSG *pmsg;
	{
	if (pwndAlarm != NULL && ClockTicks() >= ticksAlarm) 
		{
		pmsg->pwnd = pwndAlarm;
		pmsg->message = WM_ALARM;
		KillAlarm();
		return TRUE;
		}
	return FALSE;
	}
