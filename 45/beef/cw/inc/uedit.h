/*    	COW : Character Oriented Windows

	uedit.h: Definitions for user Edit
*/

/***BEGIN_PUBLIC***/

#ifndef NOWNDMACROS

#define wndEdit(id, fBorder, fEnabled, ax, ay, dax, day, pwndParent, pwndSibbling, szBuf, cchMax, chFill, isa, isaSel) {\
	id, WS_CHILD | WS_EDIT | (fBorder ? WS_BORDER : 0), \
	0, fEnabled, {ax, ay, ax+dax, ay+day}, \
	{((fBorder) ? ax+1 : ax), (fBorder ? ay+1 : ay), \
	 ((fBorder) ? ax+dax-1 : ax+dax), (fBorder ? ay+day-1 : ay+day)}, \
	(PLFN) InternalEditWndProc, pwndParent, pwndSibbling, NULL, 0, 0, \
	{0, (WORD) szBuf, cchMax, isa, isaSel, chFill, 0, 0, 0, 0, TRUE, 0, \
	 (WORD) szBuf}}
/* InternalEditWndProc must be forward defined for CS compiler */
DWORD 		FARPUBLIC InternalEditWndProc(PWND, WORD, WORD, DWORD);

#endif /* !NOWNDMACROS */

/* for selection (SetTmcSel) */
#define	ichSelectEnd	(0x7fff)		/* select to end */


VOID		FARPUBLIC SetEditText(PWND, char *, BOOL);
VOID		FARPUBLIC SetEditWidth(PWND, WORD);	/* OPTIONAL */
WORD		FARPUBLIC GetEditText(PWND, char *, WORD);
DWORD		FARPUBLIC EditWndProc(PWND, WORD, WORD, DWORD);
VOID		FARPUBLIC DefaultEditMgrInit(VOID);	/* OPTIONAL */

#define EN_CHANGE			1
#define EN_CURSORMOVED			2

/***END_PUBLIC***/
