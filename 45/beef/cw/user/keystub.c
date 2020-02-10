/*
	CW : Character Windows

	keystub.c : stub for keyboard : UpdateShiftKk
					UpdateShiftKj

	(included for applications that don't supply a routine)
	(for those that do, this module will not be included)
*/

#define COW
#include <cow.h>

#include <udialog.h>


VOID FARPUBLIC
UpdateShiftKk(kkNew, kkOld)
WORD kkNew, kkOld;
	{
	Unreferenced(kkNew);
	Unreferenced(kkOld);
	}

#ifdef KANJI
VOID FARPUBLIC
UpdateShiftKj(kjNew, kjOld)
WORD kjNew, kjOld;
	{
	Unreferenced(kjNew);
	Unreferenced(kjOld);
	}
#endif

