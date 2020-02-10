/*
	COW : Character Oriented Windows

	uutil.h : Definitions for User Windows
		Cmerge definitions from util.asm + sdmasm.asm
*/

#include <insyd.h>				/* installable driver */

/***BEGIN_PUBLIC***/

extern BYTE PASCAL	fSingleFloppy;	/* TRUE => 1 floppy system */
extern BYTE PASCAL	fInt24Error;	/* Set if INT 24 error detected */

/* Sound Facilities */
#define	Beep()		DoSound(0)
#define	Click()		DoSound(1)

BOOL		FARPUBLIC FValidDrive(char);
/***END_PUBLIC***/

