/*
	COW : Character Oriented Windows

	helpstub.c : stub for help

	(included for applications that don't supply help)
	(for those that do, this module will not be included)
*/

#define COW
#include <cow.h>

#include <uevent.h>
#include <udialog.h>
#include <kkeyboar.h>

STATIC BOOL fInsideHelp = FALSE;


VOID FARPUBLIC
Help(hem, hid, pv, kk) 
WORD	hem;		/* help message */
WORD	hid;		/* help id */
VOID *	pv;		/* optional parameter */
WORD	kk;		/* key shift states */
	{
	Unreferenced(hem);
	Unreferenced(hid);
	Unreferenced(pv);

	if (fInsideHelp)
		return;		/* no recursive please */

	fInsideHelp = TRUE;
	MessageBox(
	   (kk & KK_SHIFT) ? "No Tutorial Available" : "No Help Available",
	   "(so leave me alone)", NULL, MB_OK);
	fInsideHelp = FALSE;
	}

