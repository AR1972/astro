/*
	COW : Character Oriented Windows

	trapstub.c : stub for mouse trap

	(included for development hook)
	(this module should not be included in any final versions)
*/

#define COW
#include <cow.h>

#include <udialog.h>

#ifdef DEBUG

BOOL FARPUBLIC
FTrapMouseRbutton()
	{
	return FALSE;		/* ignore */
	}

#endif /*DEBUG*/

