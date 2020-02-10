/*
	COW : Character Oriented Windows

	kmem.h : Kernel Memory module header + misc Kernel stuff
*/

/***BEGIN_PUBLIC***/

extern BYTE PASCAL	fShellPresent;		/* DOS 3 : SHELL.EXE hook */

HANDLE		FARPUBLIC GlobalAlloc(WORD, DWORD);		/*OPTIONAL*/
HANDLE		FARPUBLIC GlobalFree(HANDLE);			/*OPTIONAL*/
LPSTR		FARPUBLIC GlobalLock(HANDLE);			/*OPTIONAL*/
HANDLE		FARPUBLIC GlobalReAlloc(HANDLE, DWORD, WORD);	/*OPTIONAL*/
BOOL		FARPUBLIC GlobalUnlock(HANDLE);			/*OPTIONAL*/
DWORD		FARPUBLIC GlobalCompact(DWORD);			/*OPTIONAL*/

DWORD		FARPUBLIC RerrExec(char *, char *, char *, BOOL, BOOL);/*OPTIONAL*/
VOID		FARPUBLIC BindSegment(PFN, BOOL);		/*OPTIONAL*/
VOID		FARPUBLIC AccessSwapFile(BOOL);			/*OPTIONAL*/

/* RerrExec return values */
#define	rerrOk		0
#define	rerrBadFile	2
#define	rerrBadPath	3
#define	rerrNoMemory	8
#define	rerrBadFormat	11
#define	rerrBadRead	30
#define	rerrBadVersion	90
#define	rerrBadMemReq	91

/***END_PUBLIC***/



#ifdef COW

DWORD		FARPUBLIC GlobalHandle(HANDLE);

/* dereference global data object handle -- limited lifetime */
#define	LpvGlobalDeref(h)	((VOID FAR *) MAKELONG(0, HIWORD(GlobalHandle(h))))

#define GMEM_MOVEABLE	    0x0002
#define GMEM_ZEROINIT	    0x0040
#define LHND		(LMEM_MOVEABLE | LMEM_ZEROINIT)
#define LPTR		(LMEM_FIXED    | LMEM_ZEROINIT)
#define NONZEROLHND	(LMEM_MOVEABLE)
#define NONZEROLPTR	(LMEM_FIXED)

#endif /*COW*/

