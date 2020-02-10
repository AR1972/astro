/*
	COW : Character Oriented Windows

	menucore.c : menu handler

	Contains functions that must be in core segment.
*/

#define COW
#include <cow.h>

#include <uevent.h>
#include <umenu.h>
#include <kkeyboar.h>

#include "event.h"
#include "menu.h"
#include "_menu.h"


PUBLIC BOOL FARPUBLIC
FEnableMenuBar(fEnable)
/*
  -- Enable/Disable menu bar
*/
BOOL fEnable;
	{
	StartPublic();
	BOOL fEnabled = FMenuAllowed();
	
	if (fEnable && !fEnabled)
		{
		Assert(pfnFilter == DummyFilter);
		pfnFilter = MenuFilterProc;
		}
	else if (!fEnable && fEnabled)
		{
		Assert(pfnFilter == MenuFilterProc);
		pfnFilter = DummyFilter;
		}

	ReturnPublic(fEnabled, BOOL);
	}

