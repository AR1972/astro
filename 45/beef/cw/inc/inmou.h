/*
	CW: Character Windows
	
	inmou.h: Mouse specifics (not currently installable)
*/

/***BEGIN_PUBLIC***/

/*** mcb - mouse cursor block */
typedef struct mcb
	{
	WORD	colHot;					/* Hot Spot for								*/
	WORD	rowHot;					/*   graphics mouse cursor.				*/
	WORD	rgwAndMaskGfx[16];	/* Bit map masks for							*/
	WORD	rgwXorMaskGfx[16];	/*   graphics mouse cursor.				*/
	WORD	wAndMaskText;			/* Character and Attribute masks for	*/
	WORD	wXorMaskText;			/*   text mouse cursor.						*/
	} MCB; 

/*** mcb - mouse conditional off block */
typedef struct mcob
	{
	WORD	xLeft;
	WORD	yTop;
	WORD	xRight;
	WORD	yBottom;
	} MCOB; 

/* Key state masks for mouse messages (in wParam) */
#define	MK_LBUTTON		0x0001
#define	MK_RBUTTON		0x0002
#define	MK_SHIFT		0x0004
#define	MK_CONTROL		0x0008
#define	MK_MBUTTON		0x0010
#define	MK_NONCLIENT		0x0060	/* either X or Y outside */
#define	MK_NONCLIENT_X		0x0020
#define	MK_NONCLIENT_Y		0x0040
#define	MK_MENU			0x8000

BOOL		FARPUBLIC FEnableMouse(BOOL);
VOID		FARPUBLIC SetMouseCursor(BYTE FAR *);		/*OPTIONAL*/
VOID		FARPUBLIC MouseConditionalOff(PARC);	/*OPTIONAL*/
VOID		FARPUBLIC SetMousePos(WORD, WORD);		/*OPTIONAL*/
VOID		FARPUBLIC SetMouseDoubleSpeed(WORD);		/*OPTIONAL*/
BOOL		FARPUBLIC SwapMouseButton(BOOL);		/*OPTIONAL*/
WORD		FARPUBLIC CbSizeMouseState(void);		/*OPTIONAL*/
VOID		FARPUBLIC SaveMouseState(BYTE FAR *);		/*OPTIONAL*/
VOID		FARPUBLIC RestoreMouseState(BYTE FAR *);	/*OPTIONAL*/

/***END_PUBLIC***/

/*****************************************************************************/
/* Private info */

#ifdef COW

extern AX	PASCAL axMouse;
extern AY	PASCAL ayMouse;
extern WORD	PASCAL sstMouse;

#endif /*COW*/
