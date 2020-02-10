/*
    	COW : Character Oriented Windows

	window.h : Window cow private interface
*/

extern PWND PASCAL pwndRoot;

VOID	FARPRIVATE	ValidateWindow(PWND);
VOID	FARPRIVATE	UpdateCursor(void);
VOID	FARPRIVATE	DrawBoxArc(BOX *, PARC, WORD, BOOL, BOOL, char *);
BOOL	FARPRIVATE	FArcFromRrc(PWND, PRRC, PARC);

#define AssertCorrectRrc(prrc) \
	AssertSz((prrc->rxLeft < prrc->rxRight) && \
	         (prrc->ryTop < prrc->ryBottom), \
		 "Invalid Rectangle")
