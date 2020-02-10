/*
	COW: Character Oriented Windows
	
	uwindow.h: Publics for USER Window
*/

/***BEGIN_PUBLIC***/

VOID		FARPUBLIC AddChild(PWND, PWND);
VOID		FARPUBLIC RemoveChild(PWND);
VOID		FARPUBLIC EnableWindow(PWND, BOOL);
VOID		FARPUBLIC GetClientRrc(PWND, RRC *);
VOID		FARPUBLIC SetWindowStyle(PWND, WORD);
VOID		FARPUBLIC SetWindowSize(PWND, BYTE, BYTE);
VOID		FARPUBLIC DrawWindow(PWND);
VOID		FARPUBLIC DrawBorder(PWND, BOX *, WORD, char *);
VOID		FARPUBLIC TextOut(PWND, RX, RY, char *, short, WORD);
VOID		FARPUBLIC CharOut(PWND, RX, RY, ACHAR, WORD);
VOID		FARPUBLIC FillRrc(PWND, PRRC, ACHAR, WORD);
VOID		FARPUBLIC BltRrc(PWND, RX, RY, BYTE, BYTE, RX, RY);
VOID		FARPUBLIC DrawBox(PWND, PRRC, BOX *, WORD);
VOID		FARPUBLIC SaveRrc(PWND, PRRC, BYTE FAR *);
VOID		FARPUBLIC RestoreRrc(PWND, PRRC, BYTE FAR *);
VOID		FARPUBLIC EnableCursor(PWND, BOOL);
VOID		FARPUBLIC MoveCursor(PWND, RX, RY);
VOID		FARPUBLIC MoveWindow(PWND, AX, AY);
BOOL		FARPUBLIC IntersectRect(PRRC, PRRC, PRRC);
VOID		FARPUBLIC UnionRect(PRRC, PRRC, PRRC);
BOOL		FARPUBLIC IsRectEmpty(PRRC);
VOID		FARPUBLIC SetRect(PRRC, RX, RY, RX, RY);
BOOL		FARPUBLIC PtInRect(PRRC, RX, RY);
WORD		FARPUBLIC CwSizeRrc(PRRC);
VOID		FARPUBLIC ShadowArc(PARC);
/*	* for overlap windows */
VOID		FARPUBLIC AddChildHead(PWND, PWND);		/*OPTIONAL*/
VOID		FARPUBLIC AddChildTail(PWND, PWND);		/*OPTIONAL*/
VOID		FARPUBLIC RethinkDisplay(void);			/*OPTIONAL*/
BOOL		FARPUBLIC FWindowToTop(PWND);            	/*OPTIONAL*/
VOID		FARPUBLIC RedrawDamagedRegions(void);		/*OPTIONAL*/
BOOL		FARPUBLIC FMoveSizeWithKeyboard(PWND, BOOL);	/*OPTIONAL*/
BOOL		FARPUBLIC FIsTopWindow(PWND);			/*OPTIONAL*/
PWND		FARPUBLIC PwndGetTopWindow(PWND);		/*OPTIONAL*/
VOID		FARPUBLIC DrawOverlapShadow(PWND);		/*OPTIONAL*/
/*	Listbox routines */
DWORD		FARPUBLIC ListBoxWndProc(PWND, WORD, WORD, DWORD);
VOID		FARPUBLIC InitListBox(PWND, PWFN);		/*OPTIONAL*/
VOID		FARPUBLIC InitListBoxOriented(PWND, PWFN, WORD *, WORD *);/*OPTIONAL*/
VOID		FARPUBLIC GetListBoxOrientation(PWND, WORD *, WORD *);/*OPTIONAL*/
#define FMoveOverlapWithKeyboard(pwnd) FMoveSizeWithKeyboard(pwnd,TRUE);
#define FSizeOverlapWithKeyboard(pwnd) FMoveSizeWithKeyboard(pwnd,FALSE);

#ifndef NORECT
#define CopyRect(prrcDest, prrcSrc) {*(prrcDest) = *(prrcSrc) }
#define SetRectEmpty(prrc) {*((long *) prrc) = 0; }
#endif /*!NORECT*/

#ifndef NOWNDMACROS

#define wndGeneric(id, style, fEnabled, ax, ay, dax, day, pfnWndProc, pwndParent, pwndSibling, pwndChild) { \
	id, style, 0, fEnabled, \
	{ax, ay, ax+dax, ay+day}, \
	{((style) & WS_BORDER) ? ax+1 : ax, \
	 ((style) & WS_BORDER) ? ay+1 : ay, \
	 ((style) & (WS_BORDER | WS_VSCROLL)) ? ax+dax-1 : ax+dax, \
	 ((style) & (WS_BORDER | WS_HSCROLL)) ? ay+day-1 : ay+day}, \
	(PLFN) pfnWndProc, pwndParent, pwndSibling, pwndChild, 0, 0,

#define wndGenericCursor(id, style, fEnabled, ax, ay, dax, day, pfnWndProc, pwndParent, pwndSibling, pwndChild, axCurs, ayCurs) { \
	id, style, TRUE, fEnabled, \
	{ax, ay, ax+dax, ay+day}, \
	{((style) & WS_BORDER) ? ax+1 : ax, \
	 ((style) & WS_BORDER) ? ay+1 : ay, \
	 ((style) & (WS_BORDER | WS_VSCROLL)) ? ax+dax-1 : ax+dax, \
	 ((style) & (WS_BORDER | WS_HSCROLL)) ? ay+day-1 : ay+day}, \
	(PLFN) pfnWndProc, pwndParent, pwndSibling, pwndChild, axCurs, ayCurs,
		
#define endWndGeneric }

#define wndListBox(id,fBorder,fSorted,fEnabled,ax,ay,dax,day,pfnWndProc,pwndParent,pwndSibling,pwndChild,color,isaHilite,ctickRep) { \
	id, (WS_CHILD | WS_LISTBOX | WS_VSCROLL | \
             (fSorted ? LBS_SORT : 0) | (fBorder ? WS_BORDER : 0)), \
	TRUE, fEnabled, {ax, ay, ax+dax, ay+day},                   \
	{ (fBorder ? ax+1 : ax),                                    \
	  (fBorder ? ay+1 : ay),                                    \
	  ((TRUE)  ? ax+dax-1 : ax+dax),                      \
	  (fBorder ? ay+day-1 : ay+day)     }, \
	(PLFN) pfnWndProc, pwndParent, pwndSibling,      \
	pwndChild, \
        (fBorder ? ax+2 : ax+1), 1 \
 	,{0,0,0,0,0,0,0,0,0,1,0,0,color,isaHilite,ctickRep,0} }

#define	SetWindowProc(pwnd, pfn) {(pwnd)->pfnWndProc = pfn;}
#define	GetWindowWord(pwnd, iw) ((pwnd)->rgwExtra[(iw)])
#define	SetWindowWord(pwnd, iw, w) {(pwnd)->rgwExtra[(iw)] = (w);}
#define	PwndParent(pwnd) ((pwnd)->pwndParent)
#define	PwndChild(pwnd) ((pwnd)->pwndChild)
#define	PwndSibling(pwnd) ((pwnd)->pwndSibling)

#endif /*NOWNDMACROS*/

#ifndef NODRAW
/* Window Drawing support */
#define	AxOfRx(pwnd,rx) ((pwnd)->arcClipping.axLeft+(rx))
#define	AyOfRy(pwnd,ry) ((pwnd)->arcClipping.ayTop+(ry))
#define	RxOfAx(pwnd,ax) ((ax)-(pwnd)->arcClipping.axLeft)
#define	RyOfAy(pwnd,ay) ((ay)-(pwnd)->arcClipping.ayTop)
#define CbSizeRrc(prrc) (CwSizeRrc(prrc) << 1)
#endif /*!NODRAW*/

#ifndef NOWINSTYLES
/* Window styles */
#define	WS_TILED		0x0000
#define	WS_CHILD		0x0000	/* note : no distinction */

#define	WS_BORDER		0x0100
#define	WS_VSCROLL		0x0200
#define	WS_HSCROLL		0x0400
#define WS_SUBSTYLE		0x003f

#define WS_OVERLAP		0x0040
#define WS_CLIPOUT		0x0080

#define WS_TYPE 		0x3800	/* type mask */
#define WS_EDIT			0x0800
#define WS_LISTBOX		0x1000
#define WS_BUTTON		0x1800
#define WS_STATIC		0x2000
#define WS_DIALOG		0x2800
#define WS_SCROLL		0x3000
#define WS_STATIC_NOACCEL	0x3800

#define LBS_SORT		0x0001
#endif /*!NOWINSTYLES*/

/* codes for listbox item indices */
#define	iszMin	((WORD)	0)
#define	iszNil	((WORD)	-1)		/* invalid isz */

/***END_PUBLIC***/
