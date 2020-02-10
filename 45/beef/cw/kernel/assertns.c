/*
	COW : Character Oriented Windows

	assertns.c : AssertFailed() procedures for DOS 3/5 non-swapped
*/

#define COW
#include <cow.h>

#include <kinput.h>	/* for EndCow() */

#define LINT_ARGS
#include <stdio.h>
#include <process.h>	/* for exit() */


#ifdef DEBUG	/* entire file for debug only */

#define	exAssert	99


PUBLIC VOID FAR PASCAL CowAssertFailed(void);	/* called from assembler */


PRIVATE VOID FAR PASCAL
CowAssertFailedLine(szFile, ln)
/*
  -- print assert failed message
*/
char *szFile;
int ln;
	{
	printf("COW Assertion failed: %s(%d)\n", szFile, ln);
	EndCow(FALSE);
	exit(exAssert);
	}



PRIVATE VOID FAR PASCAL
CowAssertFailedSz(sz)
/*
  -- print assert failed message
*/
char *sz;
	{
	printf("COW Assertion failed: %s\n", sz);
	EndCow(FALSE);
	exit(exAssert);
	}


PRIVATE VOID FAR PASCAL		/* always far */
CowAssertFailed()
/*
  -- low level assert failed
*/
	{
	printf("COW Assertion failed (assembler)\n");
	EndCow(FALSE);
	exit(exAssert);
	}


#endif /* DEBUG (entire file) */
