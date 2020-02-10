/*
	os2.h : OS/2 calls (from DOSCALLS.H)
	NOTE : these procedures are mixed case and use machine independent
		parameters (i.e. WORD, DWORD).
*/

#define	OS2CALL	extern WORD FAR PASCAL

/* Semaphores */
OS2CALL		DosSemClear(DWORD);
OS2CALL		DosSemRequest(DWORD, long);
OS2CALL		DosSemSet(DWORD);
OS2CALL		DosSemSetWait(DWORD, long);
OS2CALL		DosSemWait(DWORD, long);
