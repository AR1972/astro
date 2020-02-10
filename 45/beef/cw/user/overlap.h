/*
    	COW : Character Oriented Windows

	overlap.h : overlapping window control
*/

#ifdef WINDOW_OVERLAP

BOOL	FARPRIVATE OverlapFilter(PMSG);
VOID	FARPRIVATE DrawThisWndProc(PWND);
VOID	FARPRIVATE ClipoutWindowAndSiblings(PWND);

extern PWND pwndCur;
extern WORD psOverlap;

#define	DrawThisWnd(pwnd)	DrawThisWndProc(pwnd);

typedef struct _owdf
	{
	BITS	fMouse : 1;		/* Cause by mouse? (or keyboard) */
	BITS	fButton : 1;		/* Corner button down? */
	BITS	fOutline : 1;		/* Moving outline of window? */
	BITS	fXDrag : 1;		/* Drag in X direction? */
	BITS	fYDrag : 1;		/* Drag in Y direction? */
	BITS	fEscapeMove : 1;	/* escaped from window move */
	WORD	message;		/* message to be sent */
	} OWDF; /* Overlapping window drag flags */

typedef struct _owds
	{
	AX      ax;			/* Last mouse position */
	AY      ay;
	RRC     rrc;			/* current hilited rectangle */
	PWND	pwndMoving;		/* window that is being moved */
	PWND	pwndBackground;		/* parent of current overlap window */
	OWDF	owdf;			/* flags */
	BYTE FAR *lpbSave;		/* space to save screen under outline*/
	WORD	cbSaveTop;		/* number of bytes to save top/bottom*/
	WORD	cbSaveSide;		/* number of bytes to save side */
	} OWDS;	/* Overlapping window drag status */

#define     ayMinSize      2
#define     axMinSize      3

#else /* no overlapping */

#define	DrawThisWnd(pwnd)

#endif /*!WINDOW_OVERLAP*/
