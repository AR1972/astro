/*
	COW : Character Oriented Windows

	general.h : general render cow private interface
*/

DWORD	FARPUBLIC	GeneralWndProc(PWND, WORD, WORD, DWORD);

/* Function at end of WND structure !! */
#define	PwfnCtlGeneral(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))

#if cwExtraGeneral != cwExtraMin+2
.....
#endif

