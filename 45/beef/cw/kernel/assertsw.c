/*
	COW : Character Oriented Windows

	assertsw.c : AssertFailed() procedures for swapped environment
*/

#define COW
#include <cow.h>

#include <kmem.h>
#include <kinput.h>
#include <uwindow.h>
#include <umenu.h>			/* Menu structure */
#include <uscreen.h>
#include <cwdebug.h>			/* debugging support */

#ifdef DEBUG	/* entire file for debug only */


VOID FAR cdecl	AssertPrintf(char *, ...);


PRIVATE VOID FAR
CowAssertFailedLine(szFile, ln)
/*
  -- print assert failed message
*/
char *szFile;
int ln;
	{
	AssertPrintf("Assertion failed: file %s, line %d\n", szFile, ln);
	AssertBreak();
	}



PRIVATE VOID FAR
CowAssertFailedSz(sz)
/*
  -- print assert failed message
*/
char *sz;
	{
	AssertPrintf("Assertion failed: %s\n", sz);
	AssertBreak();
	}

#endif /* DEBUG (entire file) */

