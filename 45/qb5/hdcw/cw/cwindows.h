/*
	CW : Character Windows
	cwindows.h

	CC should be defined for compilation with Cmerge.

    If defined, the following flags inhibit definition
      of the indicated constants.

    NOALL		Defines all the following:
    NOMINMAX		Macros min(a,b) and max(a,b)
    NOWND		WND / PWND typedef's or macros
    NOWINSTYLES		WS_*
    NOCOLOR		colors or ISA's
    NORECT		typedefs ARC, RRC
    NOMSG		typedef MSG
    NOWINMESSAGES	WM_*
    NOMENUS		menus
    NOMEMMGR		LMEM_*
    NODRAW		drawing macros / constants
    NOSCROLL		SB_*
    NOSUBSTYLES		BS_*, LBS_*, ES_*
    NOVIRTUALKEYCODES	VK_*
    NOMB		MB_* & ID's
    NOPROCS		procedure definitions (always for CS)
    NOWNDMACROS		window creation macros

-- Created Wed Sep 14 16:42:26 1988 */ 

#ifdef NOALL
#define NOMINMAX
#define NOWND
#define NOWINSTYLES
#define NOCOLOR
#define NORECT
#define NOMSG
#define NOWINMESSAGES
#define NOKEYSTATE
#define NOMENUS
#define NOMEMMGR
#define NODRAW
#define NOSCROLL
#define NOSUBSTYLES
#define NOVIRTUALKEYCODES
#define NOMB
#define NOWNDMACROS
#endif /*NOALL*/


#ifndef CC
/* -- Pcode Specific Definitions -- */
#ifndef EXPORT
#define	EXPORT	export
#endif
#ifndef NATIVE
#define	NATIVE	native
#endif
#else
/* -- Cmerge Compiler -- */
#ifndef CDECL
#define CDECL cdecl
#endif
#endif /*CC*/


#ifndef PASCAL
#define	PASCAL pascal
#endif

#define	FALSE	0
#define	TRUE	1
#define	NULL	0

#define	FAR	far
#define	LONG	long
#define	VOID	void
#ifdef CC
#define	NEAR	near
#else
#define NEAR
#endif

#define FARPUBLIC	FAR PASCAL		/* all interfaces FAR */

typedef unsigned char	BYTE;
typedef unsigned short	WORD;
typedef unsigned long	DWORD;
typedef int		BOOL;
typedef int	(FARPUBLIC *PFN)();		/* General Procedure */
typedef BOOL	(FARPUBLIC *PFFN)();		/* BOOL Procedure */
typedef VOID	(FARPUBLIC *PVFN)();		/* Void Procedure */
typedef WORD	(FARPUBLIC *PWFN)();		/* Word Procedure */
typedef DWORD	(FARPUBLIC *PLFN)();		/* DWORD Procedure */
typedef VOID	(FARPUBLIC *LPFN)();		/* explicit FAR procedure */
typedef WORD		HANDLE;
typedef unsigned	BIT;
typedef unsigned	BITS;

/* special type for WndProc pointers */
typedef DWORD	(FARPUBLIC *PLFN_WNDPROC)(struct _wnd *, WORD, WORD, DWORD);

/* BYTE/WORD types */
#ifndef CC	/* Pcode => WORD */
typedef WORD		AX;
typedef WORD		AY;
typedef WORD		RX;
typedef WORD		RY;
typedef WORD		ISA;
#else
typedef BYTE		AX;
typedef BYTE		AY;
typedef BYTE		RX;
typedef BYTE		RY;
typedef BYTE		ISA;
#endif /*CC*/

typedef char FAR	*LPSTR;
typedef int  FAR	*LPINT;

#ifndef NOMINMAX
#define	max(a,b)	((a) > (b) ? (a) : (b))
#define	min(a,b)	((a) < (b) ? (a) : (b))
#endif

#define	MAKELONG(l, h)	((long)(((unsigned)(l)) | ((unsigned long)((unsigned)(h))) << 16))
#define MAKEWORD(l, h)	((WORD)(((BYTE)(l)) | ((WORD)((BYTE)(h))) << 8))
#define	LOWORD(l)	((WORD)(l))
#define	HIWORD(l)	((WORD)(((DWORD)(l) >> 16) & 0xffff))
#define	LOBYTE(w)	((BYTE) ((w) & 0xff))
#define	HIBYTE(w)	((BYTE)(((WORD)(w) >> 8) & 0xff))

/* DCHAR = Double Byte Character */
typedef	WORD		DCHAR;
/* CHAR = unsigned char */
typedef unsigned char	CHAR;

/* ACHAR = either a DCHAR or a CHAR */
typedef	CHAR		ACHAR;

/*****************************************************************************/

#ifndef NORECT
typedef struct _rrc
	{
	BYTE	rxLeft;
	BYTE	ryTop;
	BYTE	rxRight;
	BYTE	ryBottom;
	} RRC;
typedef RRC *PRRC;
typedef struct _arc
	{
	BYTE	axLeft;
	BYTE	ayTop;
	BYTE	axRight;
	BYTE	ayBottom;
	} ARC;
typedef ARC *PARC;
typedef struct _box
	{
	char	chTopLeftCorner;
	char	chTopRightCorner;
	char	chBottomLeftCorner;
	char	chBottomRightCorner;
	char	chTopSide;
	char	chBottomSide;
	char	chLeftSide;
	char	chRightSide;
	} BOX;
extern BOX PASCAL boxSingle, PASCAL boxDouble;
extern BOX PASCAL boxActiveWindowOut;
extern BOX PASCAL boxInactiveWindowOut;
extern BOX PASCAL boxActiveWindowIn;
extern BOX PASCAL boxInactiveWindowIn;
extern BYTE PASCAL axMac;
extern BYTE PASCAL ayMac;
/* all CW applications should work with screens up to 254x254 */
#define axMax 254
#define ayMax 254

#endif /*!NORECT*/

#ifndef NOWND

#ifndef cwExtraWnd
#ifdef CC
/* -- for CC compiler : fixed length */
#define cwExtraWnd 1 
#else
/* -- for CS compiler : variable length */
#define cwExtraWnd
#endif
#endif

typedef struct _wnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	ARC	arcWindow;
	ARC	arcClipping;
	PLFN_WNDPROC pfnWndProc;			/* Medium Model */
	struct _wnd *pwndParent;
	struct _wnd *pwndSibling;
	struct _wnd *pwndChild;
	BYTE	axCursor;
	BYTE	ayCursor;
	WORD	rgwExtra[cwExtraWnd];
	} WND;

typedef WND *PWND;
#endif /*!NOWND*/


extern BYTE	PASCAL	fMousePresent;		/* valid after init */

VOID		FARPUBLIC LeaveCow(BOOL);			/*OPTIONAL*/
				/* temporary leave COW */
/* Swapped Cow init/exit */
#ifdef CC
VOID		FAR CDECL exit(int);				/*OPTIONAL*/
#endif

VOID		FARPUBLIC GetProgDir(char *);			/*OPTIONAL*/

/* Non-swapped Cow init/end */
BOOL		FARPUBLIC FInitCow(void);			/*OPTIONAL*/
VOID		FARPUBLIC EndCow(WORD);				/*OPTIONAL*/


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

/* Key state masks for keyboard messages (in HIWORD(lParam)) */
#define	KK_EXTENDED		0x8000	/* from extended keypad usually */
/* shifts */
#define	KK_CAPLOCK		0x4000
#define	KK_NUMLOCK		0x2000
#define	KK_SCRLOCK		0x1000
#define	KK_ALT			0x0800
#define	KK_CONTROL		0x0400
#define	KK_SHIFT		0x0200

/* for WM_CHAR */
#define	KK_VK			0x01ff	/* mask to get untranslated VK_ */

/* for WM_KEYUP / WM_KEYDOWN */
#define	KK_SC			0x00ff	/* mask to get scan code */

extern BOOL PASCAL fAbort;			/* normal Abort */
extern BOOL PASCAL fPollKeyboard;		/* Poll the keyboard ? */
extern BYTE PASCAL fKeyIsUp, fKeyWasUp;		/* Key transitions */
extern WORD PASCAL wRateKeyRepeat;		/* repeat rate */
VOID		FARPUBLIC EnableKeyboard(BOOL);
VOID		FARPUBLIC PollKeyboard(void);
VOID		FARPUBLIC SetShiftKk(WORD);
VOID		FARPUBLIC DisableExtendedKeyboard(void);

/*****************************************************************************/
/* Extra Font info */

typedef	struct _inft
	{
	/* character/font size */
	BYTE	dxChar;			/* width of character in pixels */
	BYTE	dyChar;			/* height of character in pixels */

	BYTE	dyBaseLine;		/* base line height */
	BYTE	ifont;			/* font index */
	} INFT;

/*****************************************************************************/
/* Installable state */

typedef WORD	FINST;
#define	finstText	1		/* text mode */
#define	finstGraphics	2		/* graphics mode */
#define	finstMonochrome	4		/* monochrome */
#define	finstAlternate	8		/* alternate adapter (2nd screen) */
#define	finstFont	0x10		/* supports fonts */
#define	finstCgaSnow	0x20	/* indicate snow CGA (see user\screen.asm) */
#define	finstDisableMouse	0x1000		/* for color grpahics mode not supported by mouse (hercules)*/
#define	finstFastScroll 0x2000	/* fast scroll (BltArc) for graphics text */
#define	finstQuestionable 0x4000	/* questionable mode selection */
#define	finstAvailable	0x8000		/* available with current hardware */

typedef struct _inst
	{
	FINST	finst;
	BYTE	axMac;
	BYTE	ayMac;

	/* color palette info */
	BYTE	coMac;			/* maximum color index */
	BYTE	covMac;			/* maximum color value (palette) */
	WORD	coiMac;			/* maximum color intensity */

	/* Extra information */
	WORD	imode;			/* video mode */

	INFT	inft;			/* font info (pointers may be NULL) */
	WORD	ffontSupported;	/* valid ffont values */

	/* buffers (if non-zero then do not need to be allocated */
	WORD	psPrim;			/* primary screen buffer */
	WORD	psSec;			/* secondary screen buffer */

	WORD	cwExtra;		/* requested extra size */
	WORD	psExtra;		/* extra screen buffer (driver's use) */

	/* CW internal info */
	BIT	fAllocPrim:1;
	BITS	filler:15;

	/* Driver specific info */
	WORD	wDriver1;
	WORD	reserved2[7];
	} INST;

/* Special IMODEs */
#define	imodeUnknown	0xffff	/* unknown mode */

/*****************************************************************************/
/* Installable characters */

typedef struct _inch
	{
	/* Single Line Box */
	char	_chTopLeftCorner1;
	char	_chTopRightCorner1;
	char	_chBottomLeftCorner1;
	char	_chBottomRightCorner1;
	char	_chTopSide1;
	char	_chBottomSide1;
	char	_chLeftSide1;
	char	_chRightSide1;

	/* other single line */
	char	_chMiddleLeft1;
	char	_chMiddleRight1;

	/* Double Line Box */
	char	_chTopLeftCorner2;
	char	_chTopRightCorner2;
	char	_chBottomLeftCorner2;
	char	_chBottomRightCorner2;
	char	_chTopSide2;
	char	_chBottomSide2;
	char	_chLeftSide2;
	char	_chRightSide2;
	/*note: no middles */

	/* Arrows */
	char	_chUpArrow;
	char	_chDownArrow;
	char	_chLeftArrow;
	char	_chRightArrow;

	/* Misc */
	char	_chBullet;			/* for menu */
	char	_chMiddleDot;			/* for edit */
	char	_chScrollbar;			/* for scroll bar */
	char	_chElevator;			/* for scroll bar */
	char	_chShadowInit;			/* b&w shadow character */

	/* For Overlapping windows */
	char	_chClose;			/* Close box */
	char	_chZoomIn;			/* Zoom in */
	char	_chZoomOut;			/* Zoom out */
	char	_chUpDownArrow;			/* double arrow */
	char	_chLeftRightArrow;		/* double arrow */

	WORD	reserved[16];
	} INCH;

/*****************************************************************************/
/* font variations */

#define	ffontNormal		0x0000	/* normal attributes */

#define	ffontUnderline		0x0001
#define	ffontDoubleUnderline	0x0002
#define	ffontOrUnderline	0x0003	/* ... underline */
#define	ffontStrikeThrough	0x0004	/* horizontal strike through */
#define	ffontBold		0x0008
#define	ffontSubscript		0x0010
#define	ffontSuperscript	0x0020
#define	ffontMiniCap		0x0030	/* mini capital */
#define	ffontItalic		0x0040
#define	ffontOrCharacter	0x0080	/* extra field => font extension */
#define	ffontReservedBits	0x0f00
#define	ffontExtraMask		0xf000	/* one of 16 extra items */
#define	ffontOrUnderlineSupport	0x1000

/*****************************************************************************/
/* screen save info */

typedef WORD	FVIDS;			/* flags */
#define	fvidsChAttr	1		/* video buffer is in form ch:attr */

typedef	struct _vids
	{
	BYTE	mode;			/* screen mode */
	BYTE	page;			/* page # */
	FVIDS	fvids;			/* flags for saved state */
	WORD	cwVidData;		/* # of bytes of screen data */
	WORD	cwExtra;		/* # of extra bytes for mode info */
	WORD	rgwExtra[1];		/* actually rgwExtra[cwExtra] */
	} VIDS;	/* Video state */

/*****************************************************************************/

#define	fmemFixed	1		/* always set !! */
#define	fmemNear	2		/* allocate near */

/* limit of near space / driver service */
#define	cbNearMemServiceMax	128

/* INDT: Driver Service Types */
#define	indtNil			0
#define	indtKeyboard		1			/* .KBD */
#define	indtCharacterScreen	2			/* .CSD */
#define	indtGraphicScreen	3			/* .GSD */
#define	indtCharacterPrinter	4			/* .PRD */
#define	indtGraphicPrinter	5			/* .GPD */
#define	indtSystem		6			/* .SYD */
#define	indtSerialComm		7			/* .SCD */
/* ... others to be defined ... */

typedef struct _indv
	{
	/* input values */
	BYTE	indt;				/* service wanted */
	BYTE	filler;				/* reserved */
	LPFN *	rglpfn;				/* where to put vectors */
	WORD	cpfnNeedMin;			/* # entries needed */
	WORD	cpfnNeedMac;			/* # entries wanted */
	/* return values */
	WORD	psLoaded;			/* != 0 => loaded */
	WORD	cpfnLoaded;			/* # entries loaded */
	} INDV;

WORD	FARPUBLIC RerrLoadDrv(char *, INDV *, WORD);	/* OPTIONAL */
WORD	FARPUBLIC RerrLoadCwDrv(char *);		/* OPTIONAL */


extern INST PASCAL instCur;
extern INCH PASCAL inch;		/* near buffer for characters */
extern BOOL PASCAL fFontAvailable;	/* extra "ffont" drawing available ? */

/* name aliases for the actual characters */
#define	chTopSide1		(inch._chTopSide1)
#define	chBottomSide1		(inch._chBottomSide1)
#define	chLeftSide1		(inch._chLeftSide1)
#define	chRightSide1		(inch._chRightSide1)
#define	chTopLeftCorner1	(inch._chTopLeftCorner1)
#define	chTopRightCorner1	(inch._chTopRightCorner1)
#define	chBottomLeftCorner1	(inch._chBottomLeftCorner1)
#define	chBottomRightCorner1	(inch._chBottomRightCorner1)
#define chMiddleLeft1		(inch._chMiddleLeft1)
#define chMiddleRight1		(inch._chMiddleRight1)
#define	chTopSide2		(inch._chTopSide2)
#define	chBottomSide2		(inch._chBottomSide2)
#define	chTopLeftCorner2	(inch._chTopLeftCorner2)
#define	chTopRightCorner2	(inch._chTopRightCorner2)
#define	chBottomLeftCorner2	(inch._chBottomLeftCorner2)
#define	chBottomRightCorner2	(inch._chBottomRightCorner2)
#define	chUpArrow		(inch._chUpArrow)
#define	chDownArrow		(inch._chDownArrow)
#define	chRightArrow		(inch._chRightArrow)
#define	chLeftArrow		(inch._chLeftArrow)
#define	chBullet		(inch._chBullet)
#define	chMiddleDot		(inch._chMiddleDot)
#define	chScrollbar		(inch._chScrollbar)
#define	chElevator		(inch._chElevator)
#define	chShadowInit		(inch._chShadowInit)

/* for overlapping windows */
#define	chClose			(inch._chClose)
#define	chZoomIn		(inch._chZoomIn)
#define	chZoomOut		(inch._chZoomOut)
#define	chUpDownArrow		(inch._chUpDownArrow)
#define	chLeftRightArrow	(inch._chLeftRightArrow)

/*****************************************************************************/
/* Screen Procedures */

typedef WORD FAR *	(FAR PASCAL *LPFN_DRV_ALLOC)(WORD, WORD);
typedef VOID		(FAR PASCAL *LPFN_DRV_FREE)(WORD FAR *);

WORD	FARPUBLIC ImodeGuessCurrent(void);			/*OPTIONAL*/
BOOL	FARPUBLIC FQueryInst(INST *, WORD);			/*OPTIONAL*/
BOOL	FARPUBLIC FAllocInstBuffers(INST *, LPFN_DRV_ALLOC, BOOL); /*OPTIONAL*/
VOID	FARPUBLIC FreeInstBuffers(INST *, LPFN_DRV_FREE);	/*OPTIONAL*/

BOOL	FARPUBLIC FAllocOverlapTable(INST *, LPFN_DRV_ALLOC);	/*OPTIONAL*/
VOID	FARPUBLIC FreeOverlapTable(LPFN_DRV_FREE);		/*OPTIONAL*/

BOOL	FARPUBLIC FInitScreen(INST *);				/*OPTIONAL*/
VOID	FARPUBLIC EndScreen(BOOL);				/*OPTIONAL*/

BOOL	FARPUBLIC FGetColorPalette(WORD, WORD *, WORD *);	/*OPTIONAL*/
VOID	FARPUBLIC SetColorPalette(WORD, WORD, WORD *);		/*OPTIONAL*/

VOID	FARPUBLIC MoveHwCursCsd(AX, AY, WORD);			/*OPTIONAL*/

BOOL	FARPUBLIC FQueryInft(INFT *, WORD);			/*OPTIONAL*/

WORD	FARPUBLIC CbSizeVids(void);				/*OPTIONAL*/
BOOL	FARPUBLIC FSaveVids(VIDS *, INST *);			/*OPTIONAL*/
BOOL	FARPUBLIC FRestoreVids(VIDS *);				/*OPTIONAL*/
VOID	FARPUBLIC SaveVidData(VIDS *, WORD FAR *);		/*OPTIONAL*/
VOID	FARPUBLIC RestoreVidData(VIDS *, WORD FAR *);		/*OPTIONAL*/
VOID	FARPUBLIC EnableVidsMonitor(BOOL);			/*OPTIONAL*/

VOID	FARPUBLIC GetCharMap(INFT *, BYTE, BYTE *);


WORD		FARPUBLIC MessageBox(char *, char *, char *, WORD);
VOID		FARPUBLIC SetDialogCaption(HANDLE, char *);
VOID		FARPUBLIC HiliteDialogAccel(void);

/* Message Box Definitions */

#ifndef NOMB
#define	IDDEFAULT		1
#define	IDCANCEL		2
#define	IDNO			3
#define	IDOK			IDDEFAULT
#define	IDYES			IDDEFAULT
#define	IDRETRY			IDDEFAULT
#define	IDABORT			IDDEFAULT
#ifdef	HELP_BUTTON
#define IDHELP			4
#endif	// HELP_BUTTON
#define	MB_OK			1
#define	MB_YESNOCANCEL		2
#define	MB_RETRYCANCEL		3
#define	MB_OKCANCEL		4
#define	MB_ABORT		5
#define MB_YESNO		6
#define MB_RETRY		7
#define	MB_TYPE			0x0f		/* message type */
#define MB_BEEP			0x10
#define MB_CAPTION		0x20		/* 1st param is caption */
#ifdef	HELP_BUTTON
#define MB_NOHELP		0x8000
#endif	// HELP_BUTTON
#endif /*!NOMB*/

/* for Special MessageBox */
extern BYTE FAR * PASCAL lpbWorkTemp;	/* App should never use directly */

#define	InitSpecialMessageBox(lpbBuff)	\
	{				\
	Assert(lpbWorkTemp == NULL);	\
	lpbWorkTemp = (lpbBuff);	\
	}

#define	EndSpecialMessageBox()		\
	{				\
	Assert(lpbWorkTemp != NULL);	\
	lpbWorkTemp = NULL;		\
	}



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


#ifndef NOMSG
/* Message structure */
typedef struct _msg
	{
	PWND	pwnd;
	WORD	message;
	WORD	wParam;
	DWORD	lParam;
	DWORD	time;
	} MSG;
typedef MSG *PMSG;
#ifndef DOS5
extern BOOL PASCAL fMessage;
#ifdef DUAL
extern DWORD PASCAL semaMessage;	/* message semaphore */
#define	hsemaMessage	((DWORD) (DWORD FAR *) &semaMessage)
#endif /*DUAL*/
#else
extern DWORD PASCAL semaMessage;	/* message semaphore */
#define	hsemaMessage	((DWORD) (DWORD FAR *) &semaMessage)
#endif
#endif /*!NOMSG*/


#ifndef NOWINMESSAGES
/* Window Messages */
#define	WM_NULL			0x0000
#define	WM_CREATE		0x0001
#define	WM_WANTFOCUS		0x0005
#define	WM_MAKEACTIVE		0x0006
#define	WM_SETFOCUS		0x0007
#define	WM_KILLFOCUS		0x0008
#define	WM_PAINT		0x000f
#define	WM_QUIT			0x0012

#define	WM_KEYFIRST		0x0100
#define WM_KEYLAST		0x0102

#define	WM_KEYDOWN		0x0100
#define	WM_KEYUP		0x0101
#define	WM_CHAR			0x0102

#define	WM_CUT			0x0103
#define	WM_PASTE		0x0104
#define	WM_COPY			0x0105
#define	WM_INSERT		0x0106

#define	WM_MENUIDLE		0x0110		/* Menu Idle */
#define	WM_COMMAND		0x0111
#define	WM_MENUSELECT		0x0112		/* selecting a menu item */
#define	WM_MENUSTART		0x0113		/* starting a menu action */
#define	WM_HSCROLL		0x0114
#define	WM_VSCROLL		0x0115
#define	WM_INITMENUPOPUP	0x0116
#define	WM_ALARM		0x0117

#define	WM_MOUSEFIRST		0x0200
#define	WM_LMOUSELAST		0x0203	/* last of Left mouse actions */
#define	WM_MOUSELAST		0x0206

#define	WM_MOUSEMOVE		0x0200	/* mouse related constants */
#define	WM_LBUTTONDOWN		0x0201
#define	WM_LBUTTONUP		0x0202
#define	WM_LBUTTONDBLCLK	0x0203
#define	WM_RBUTTONDOWN		0x0204
#define	WM_RBUTTONUP		0x0205
#define	WM_RBUTTONDBLCLK	0x0206

/* Edit Wnd Proc Messages */
#define EM_SETSEL		0x0300
#define EM_GETSEL		0x0301

/* Overlap Wnd Proc Messages */
#define WM_ACTIVATE	0x0320
#define WM_ZOOM		0x0321
#define WM_CLOSE	0x0322
#define WM_MOVE		0x0323
#define WM_SIZE		0x0324


/*WM-DIALOG is actually private (and LB_SETWIDTH)*/
#define WM_DIALOG		0x0380
/* Listbox Proc Messages */
#define WM_LISTBOX_COMMAND	WM_DIALOG
#define LB_RESETCONTENT		0x0340
#define LB_ADDSTRING		0x0341
#define LB_DELETESTRING		0x0342
#define LB_SETCURSEL		0x0343
#define LB_GETCURSEL		0x0344
#ifdef LISTBOX_HORIZ
#define LB_SETWIDTH		0x0345
#endif /*LISTBOX_HORIZ*/
#define LB_GETTEXT		0x0346
#define LB_GETCOUNT		0x0347
#define LB_REPLACESTRING	0x0348
#define LB_INSERTSTRING		0x0349

/* private window messages start here */
#define	WM_USER			0x0400
#endif

/* Window's aliases for KK_ states */
#define	KK_MENU			KK_ALT
#define	KK_CAPITAL		KK_CAPLOCK

/* HELP */
#define	VK_HELP_KEY		VK_F1

/* listbox notification codes */
#define LBN_SELCHANGE		0
#define LBN_DBLCLK		1
#define LBN_SELECT_DONE		2


/* List Box Selection Codes */
#define lbrCause		0xf
#define lbrNone			0
#define lbrMouse		1
#define lbrScroll		2
#define lbrKeys			3
#define lbrSpace		4
#define lbrOther		5
#define flbrReselect		0x10


/* help types/contexts */
#define	hemMenu			1
#define	hemMenuItem		2
#define	hemMbox			3
#define	hemDialog		4
#define	hemUserMin		0x10		/* For Application contexts */

/* help id's for message boxes */
#ifndef NOMB
#define	hidMboxOk		MB_OK
#define	hidMboxYesNoCancel	MB_YESNOCANCEL
#define	hidMboxRetryCancel	MB_RETRYCANCEL
#define	hidMboxOkCancel		MB_OKCANCEL
#define	hidMboxAbort		MB_ABORT
#define hidMboxYesNo		MB_YESNO
#define hidMboxRetry		MB_RETRY
#endif /*!NOMB*/

VOID		FARPUBLIC SetAlarm(PWND, WORD);
VOID		FARPUBLIC KillAlarm(void);
VOID		FARPUBLIC UngetMessage(PMSG);
BOOL		FARPUBLIC PeekMessage(PMSG);
PWND		FARPUBLIC GetFocus(void);
PWND		FARPUBLIC SetFocus(PWND);
VOID		FARPUBLIC FlushAbort(void);
PWND		FARPUBLIC SetCapture(PWND);
VOID 		FARPUBLIC ReleaseCapture(void);
DWORD		FARPUBLIC DispatchMessage(PMSG);
BOOL		FARPUBLIC PostMessage(PWND, WORD, WORD, DWORD);
DWORD		FARPUBLIC SendMessage(PWND, WORD, WORD, DWORD);
WORD		FARPUBLIC SetDoubleClickTime(WORD);		/*OPTIONAL*/
VOID		FARPUBLIC SynthesizeShiftKeys(WORD, WORD);	/*OPTIONAL*/

#ifndef NOCOLOR

/*	* General colors */
#define	isaBackground		0

#define	isaHilite		1		/* hilite / inversion */
#define	isaGreyed		2		/* not currently used */
#define	isaEnabled		3
#define	isaDisabled		4
#define	isaAlert		5

/*	* Dialog elements :	*/
#define	isaDialogBox		6	/* the actual dialog box */
#define isaStatic		isaDialogBox	/* static text */
#define isaButton		isaDialogBox	/* radio/check buttons */
#define isaPushButton		7		/* push buttons */
#define isaButtonDown		8		/* pushed button */
#define isaListBox		9		/* listbox background */
#define isaEdit			isaDialogBox

/*	* Scroll Bars :		*/
#define isaScrollbar		10
#define isaElevator		11

/*	* Menus :		*/
#define isaMenuBox		12		/* box around pull downs */
#define	isaMenu			13		/* non-selected MENU */
#define isaMenuSelected		14		/* selected menu item */
#define	isaMenuHilite		15		/* hilited character */
/* hilited character under selection */
#define	isaMenuHiliteSel	16		/* for menu titles */
#define	isaItemHiliteSel	17		/* for menu items */

#define	isaDialogAccel		18		/* dialog accelerators */

/*	* Shadows :		*/
#define	isaShadow		19

/* User Colors :		*/
#define	isaUserMin		20
#define	isaUserMax		(isaUserMin+16)
#define isaMax			isaUserMax

#endif /*!NOCOLOR*/

#ifndef NOMENUS
/* hack for nameless unions in CC */
#ifdef CC
#define CC_USZ u
#define CC_URG u
#else
#define CC_USZ
#define CC_URG
#endif


typedef struct _mpvkeyid
	{
	WORD	vkey;
	WORD	idItem;
	} MPVKEYID;
#define VkeyOfVkKk(vk, kk)	((vk) | (kk))
typedef struct _menuitem
	{
	WORD	idItem;			/* id for menuitem */
	BITS	fEnabled:1;		/* TRUE => enabled, FALSE => greyed */
	BITS	fChecked:1;		/* TRUE => checked */
	BITS	fSeparator:1;		/* TRUE => separator */
	BITS	fHandle:1;		/* TRUE => use pszItem, else szItem */
	BITS	ichHilite:4;		/* index of prefix character */
	BITS	bParamUser:8;		/* available for application use */
	union
		{
		char *szItem;
		char **pszItem;
		} CC_USZ;
	WORD	wParamUser;		/* available for application use */
	} MENUITEM;
typedef struct _menu
	{
	WORD	idMenu;
	BITS	rxTitle:8;
	BITS	ichHilite:4;		/* index of prefix character */
	BITS	fHandle:1;
	BITS	fEnabled:1;		/* is menu 'enabled' (not greyed) */
	BITS	filler:2;
	WORD	cchTitle;
	char	*pchTitle;
	WORD	citem;
	WORD	cchitemMax;
	union
		{
		MENUITEM *rgmenuitem;
		MENUITEM **prgmenuitem;
		} CC_URG;
	WORD	wParamUser;		/* available for application use */
	} MENU;
typedef struct _menubar
	{
	WORD	cmenu;
	MENU	*rgmenu;
	MPVKEYID *rgmpvkeyid;
	} MENUBAR;

typedef MENU *PMENU;
typedef MENUITEM *PMENUITEM;
typedef MENUBAR *PMENUBAR;

/* menu item define macros :
	D => disabled
	H => string handle
	X => with index (otherwise default to 0)
*/

/* enabled simple item */
#define menuitem(mid, sz, w) {(mid), TRUE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w)},
#define menuitemD(mid, sz, w) {(mid), FALSE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w)},
#define menuitemDH(mid, psz, w) {(mid), FALSE, FALSE, FALSE, TRUE,\
	0, 0, {(char *)(psz)}, (WORD)(w)},
#define menuitemX(mid, sz, ich, w) {(mid), TRUE, FALSE, FALSE, FALSE,\
	(ich), 0, {(sz)}, (WORD) (w)},
#define menuitemDX(mid, sz, ich, w) {(mid), FALSE, FALSE, FALSE, FALSE,\
	(ich), 0, {(sz)}, (WORD) (w)},
/* separator */
#define	menuitemSep	{0, FALSE, FALSE, TRUE, FALSE, 0, 0, {NULL}, 0},

#endif /*!NOMENUS*/

VOID		FARPUBLIC InitMenu(PWND, PMENUBAR);
BOOL		FARPUBLIC FEnableMenuBar(BOOL);
VOID		FARPUBLIC EnableMenu(WORD, BOOL);
VOID		FARPUBLIC EnableMenuItem(WORD, BOOL);
VOID		FARPUBLIC CheckMenuItem(WORD, BOOL);
BOOL		FARPUBLIC FMenuItemChecked(WORD);
PMENUITEM	FARPUBLIC FindMenuItem(WORD);
VOID		FARPUBLIC DrawMenubar(void);
VOID		FARPUBLIC OpenMenu(WORD);		/*OPTIONAL*/


#ifndef NOCOLOR
#define	DiMake(dm, isa)		((WORD) ((dm) | (isa)))

/*	-- non-special modes --	*/
#define	dmNormal		0
#define	DiNormal(isa)		((WORD) dmNormal | (isa))
#define	dmText			0x100
#define	dmTextOnly		dmText
#define	dmForeBack		0x200
#define	dmAttrOnly		dmForeBack
#define	dmFore			0x300
#define	dmBack			0x400
#define	dmTextFore		0x500
#define	dmTextBack		0x600

/*	-- special modes --	*/
#define	dmSpecialMin		0x700
#define	dmTextMapB		0x700
#define	dmTextMapF		0x800
#define	dmMapB			0x900
#define	dmMapF			0xA00

/* special FFONT control */
#define	fdmKeepFfont		0x8000

#endif /*!NOCOLOR*/

extern BYTE PASCAL fMonochrome;		/* TRUE => monochrome screen */
extern char PASCAL chShadow;		/* shadow character */
extern WORD PASCAL diShadow;		/* shadow draw mode, 0 => no shadow */
VOID		FARPUBLIC SetCursorBlock(BOOL);


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


/* System Specifics */
VOID		FARPUBLIC DoSound(WORD);
DWORD		FARPUBLIC ClockTicks(void);


extern BYTE PASCAL	fSingleFloppy;	/* TRUE => 1 floppy system */
extern BYTE PASCAL	fInt24Error;	/* Set if INT 24 error detected */

/* Sound Facilities */
#define	Beep()		DoSound(0)
#define	Click()		DoSound(1)

BOOL		FARPUBLIC FValidDrive(char);
#ifndef NOVIRTUALKEYCODES	
#define	VK_MIN		0x100

#define	VK_LBUTTON	0x101
#define	VK_RBUTTON	0x102
#define	VK_CANCEL	0x103
#define	VK_MBUTTON	0x104	/* NOT contiguous with L & RBUTTON */
#define	VK_BACK		0x108
#define	VK_TAB		0x109
#define	VK_CLEAR	0x10C
#define	VK_RETURN	0x10D
#define	VK_SHIFT	0x110
#define	VK_CONTROL	0x111
#define	VK_ALT		0x112
#define	VK_PAUSE	0x113
#define	VK_CAPLOCK	0x114

/* special VK_s for Kanji and Kana-Kanji conversion */
#define	VK_KANA		0x115
#define	VK_ROMAJI	0x116
#define	VK_ZENKAKU	0x117
#define	VK_HIRAGANA	0x118
#define	VK_KANJI	0x119
/* note: hole for 1A and 1B */
#define	VK_CONVERT	0x11C
#define	VK_NONCONVERT	0x11D
#define	VK_ACCEPT	0x11E
#define	VK_MODECHANGE	0x11F

#define	VK_ESCAPE	0x11B
#define	VK_SPACE	0x120

#define	VK_PRIOR	0x121
#define	VK_NEXT		0x122
#define	VK_END		0x123
#define	VK_HOME		0x124
#define	VK_LEFT		0x125
#define	VK_UP		0x126
#define	VK_RIGHT	0x127
#define	VK_DOWN		0x128

#define	VK_0		0x130
/* 1..8 */
#define	VK_9		0x139
#define	VK_A		0x141
/* B..Y */
#define	VK_Z		0x15A

#define	VK_SELECT	0x129
#define	VK_PRINT	0x12A
#define	VK_EXECUTE	0x12B
#define	VK_INSERT	0x12D
#define	VK_DELETE	0x12E
#define	VK_HELP		0x12F

#define	VK_NUMPAD0	0x160
#define	VK_NUMPAD1	0x161
#define	VK_NUMPAD2	0x162
#define	VK_NUMPAD3	0x163
#define	VK_NUMPAD4	0x164
#define	VK_NUMPAD5	0x165
#define	VK_NUMPAD6	0x166
#define	VK_NUMPAD7	0x167
#define	VK_NUMPAD8	0x168
#define	VK_NUMPAD9	0x169
#define	VK_MULTIPLY	0x16A
#define	VK_ADD		0x16B
#define	VK_SEPARATOR	0x16C
#define	VK_SUBTRACT	0x16D
#define	VK_DECIMAL	0x16E
#define	VK_DIVIDE	0x16F

#define	VK_F1		0x170
#define	VK_F2		0x171
#define	VK_F3		0x172
#define	VK_F4		0x173
#define	VK_F5		0x174
#define	VK_F6		0x175
#define	VK_F7		0x176
#define	VK_F8		0x177
#define	VK_F9		0x178
#define	VK_F10		0x179
#define	VK_F11		0x17A
#define	VK_F12		0x17B
#define	VK_F13		0x17C
#define	VK_F14		0x17D
#define	VK_F15		0x17E
#define	VK_F16		0x17F

#define VK_OAX		0x180

#define	VK_NUMLOCK	0x190
#define	VK_SCRLOCK	0x191

/* alternative names */
#define	VK_MENU		VK_ALT
#define	VK_CAPITAL	VK_CAPLOCK
#define	VK_OEM_NUMBER	VK_NUMLOCK
#define	VK_OEM_SCROLL	VK_SCRLOCK
#define	VK_SEPARATER	VK_SEPARATOR

#endif /*!NOVIRTUALKEYCODES*/	

BOOL		FARPUBLIC FIsDbcsChar(ACHAR);		/* OPTIONAL */
WORD		FARPUBLIC CchLenDbcs(char *);		/* OPTIONAL */
char *		FARPUBLIC PchNextDbcs(char *);		/* OPTIONAL */
char *		FARPUBLIC PchPrevDbcs(char *, char *);	/* OPTIONAL */


#define	SetSysColor(isa, coBack, coFore, fBlink, fHilite) \
	SetIsaColor(isa,				\
 	    (coFore) + ((fHilite) ? 8 : 0),	\
 	    (coBack) + ((fBlink) ? 8 : 0))

VOID		FARPUBLIC SetIsaColor(ISA, WORD, WORD);
VOID		FARPUBLIC GetIsaColor(ISA, WORD *, WORD *);
VOID		FARPUBLIC SetIsaRgca(ISA, BYTE *);
VOID		FARPUBLIC SetIsaFfont(ISA, WORD);		/*OPTIONAL*/
