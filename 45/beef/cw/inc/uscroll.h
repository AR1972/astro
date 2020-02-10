/*
	COW : Character Oriented Windows

	uscroll.h: Definitions for User Scroll
*/

/***BEGIN_PUBLIC***/

#ifndef NOSUBSTYLES
/* Scroll bar constants */
#define	SBS_HORZ			0
#define	SBS_VERT			1
#endif /*!NOSTYLES*/


#ifndef NOSCROLL

/* Scroll Commands */
#define	SB_LINEUP		0
#define	SB_LINEDOWN		1
#define	SB_PAGEUP		2
#define	SB_PAGEDOWN		3
#define	SB_THUMBPOSITION	4
#define	SB_THUMBTRACK		5
#define	SB_TOP			6
#define	SB_BOTTOM		7
#define SB_ENDSCROLL		8
/* define SB_UPCLICK 15 (private message) */
#endif /*!NOSCROLL*/


#ifndef NOWNDMACROS

DWORD		FARPUBLIC ScrollBarWndProc(PWND, WORD, WORD, DWORD);

#ifdef CC
#ifndef cwExtraWnd
#define cwExtraWnd 5
#endif /*no extra size*/
#endif /*CC*/
#define wndScrollBar(id, fVert, fEnabled, ax, ay, dax, day, pwndParent, pwndSibbling, ctickRep) { \
	id, WS_CHILD | WS_SCROLL | (fVert ? SBS_VERT : SBS_HORZ), \
	0, fEnabled, {ax, ay, ax+dax, ay+day}, {ax, ay, ax+dax, ay+day}, \
	(PLFN) ScrollBarWndProc, pwndParent, pwndSibbling, NULL, 0, 0, \
	{ctickRep, 0, 0, 1, 1}}

#endif /* !NOWNDMACROS */

short		FARPUBLIC SetScrollPos(PWND, short, BOOL);
short		FARPUBLIC GetScrollPos(PWND);
VOID		FARPUBLIC SetScrollRange(PWND, short, short, BOOL);

/***END_PUBLIC***/
