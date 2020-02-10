/*
	CW : Character Windows

	cwdebug.h : debugging support
		NOTE : only valid for swap based system
*/

typedef	struct
	{
	WORD	cblkTotal;
	WORD	cblkCode;
	WORD	cblkData;
	WORD	cblkFree;
	DWORD	cbTotal;
	DWORD	cbCode;
	DWORD	cbData;
	DWORD	cbFree;
	} CKH;	/* Heap checking structure */

/* Debugging routines */
VOID FAR CDECL dprintf(char *, ...);		/* debug printf */
VOID FARPUBLIC PrDebugRgch(char far *, int);
BOOL FARPUBLIC FCheckGlobalHeap(CKH FAR *);
