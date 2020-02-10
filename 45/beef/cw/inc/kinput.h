/*
	COW : Character Oriented Windows

	kinput.h : Kernel Input module header
*/

#include <inmou.h>			/* mouse driver */

/***BEGIN_PUBLIC***/

extern BYTE	PASCAL	fMousePresent;		/* valid after init */

VOID		FARPUBLIC LeaveCow(BOOL);			/*OPTIONAL*/
				/* temporary leave COW */
/* Swapped Cow init/exit */
#ifdef CC
VOID		FAR CDECL exit(int);				/*OPTIONAL*/
#endif

VOID		FARPUBLIC GetProgDir(char *);			/*OPTIONAL*/

/* Non-swapped Cow init/end */
BOOL		FARPUBLIC FInitCow(void);			/*OPTIONAL*/
VOID		FARPUBLIC EndCow(WORD);				/*OPTIONAL*/

/***END_PUBLIC***/


#ifdef COW

#ifdef DOS5
VOID		FARPRIVATE KickTimer(DWORD);
#define	ctickIdle	((DWORD) -1L)			/* idle timer */
#else
#define	KickTimer(ctick)				/* nothing here */
#endif

#endif /*COW*/

