;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
	CW : Character Windows
	cwindows.h

	CC should be defined for compilation with Cmerge.

    If defined, the following flags inhibit definition
      of the indicated constants.

    NOALL		Defines all the following:
    NOCOLOR		colors or ISA's
    NODRAW		drawing macros / constants
    NOMB		MB_* & ID's
    NOMEMMGR		LMEM_*
    NOMENUS		menus
    NOMINMAX		Macros min(a,b) and max(a,b)
    NOMSG		typedef MSG
    NOPROCS		procedure definitions (always for CS)
    NORECT		typedefs ARC, RRC
    NOSCROLL		SB_*
    NOSUBSTYLES		BS_*, LBS_*, ES_*
    NOSWAP		non-swapped version (currently only for small model)
    NOVIRTUALKEYCODES	VK_*
    NOWINMESSAGES	WM_*
    NOWINSTYLES		WS_*
    NOWND		WND / PWND typedef's or macros
    NOWNDMACROS		window creation macros

-- Created Fri Mar 23 11:33:10 1990 */ 

#ifdef NOALL
#define NOCOLOR
#define NODRAW
#define NOKEYSTATE
#define NOMB
#define NOMEMMGR
#define NOMENUS
#define NOMINMAX
#define NOMSG
#define NOPROCS
#define NORECT
#define NOSCROLL
#define NOSUBSTYLES
#define NOSWAP
#define NOVIRTUALKEYCODES
#define NOWINMESSAGES
#define NOWINSTYLES
#define NOWND
#define NOWNDMACROS
#endif /*NOALL*/


#ifdef OS2_INCLUDED
/* defined in OS2.H */
#undef	CHAR
#endif	/* OS2_INCLUDED */


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

#ifndef VAP_API
#define	FAR	far
#else
#define	FAR	near
#endif

#define	LONG	long
#define	VOID	void
#ifdef CC
#define	NEAR	near
#else
#define NEAR
#endif

#define FARPUBLIC	FAR PASCAL		/* all interfaces FAR */

#ifndef OS2_INCLUDED
/* defined in OS2.H */
typedef unsigned char	BYTE;
typedef int		BOOL;
typedef int	(FARPUBLIC *PFN)();		/* General Procedure */
#endif	/* !OS2_INCLUDED */

typedef unsigned short	WORD;
typedef unsigned long	DWORD;
typedef BOOL	(FARPUBLIC *PFFN)();		/* BOOL Procedure */
typedef VOID	(FARPUBLIC *PVFN)();		/* Void Procedure */
typedef WORD	(FARPUBLIC *PWFN)();		/* Word Procedure */
typedef DWORD	(FARPUBLIC *PLFN)();		/* DWORD Procedure */
typedef VOID	(FARPUBLIC *LPFN)();		/* explicit FAR procedure */
typedef BYTE FAR *	(FARPUBLIC *LPFN_LPB)();
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

typedef	struct szi_
	{
	char *	sz;
	ISA	isa;
	} SZI;

typedef char FAR	*LPSTR;
typedef int  FAR	*LPINT;

#ifndef NOMINMAX
#define	max(a,b)	((a) > (b) ? (a) : (b))
#define	min(a,b)	((a) < (b) ? (a) : (b))
#endif

#ifndef OS2_INCLUDED
/* defined in OS2.H */
#define	MAKELONG(l, h)	((long)(((unsigned)(l)) | ((unsigned long)((unsigned)(h))) << 16))
#define	LOBYTE(w)	((BYTE) ((w) & 0xff))
#define	HIBYTE(w)	((BYTE)(((WORD)(w) >> 8) & 0xff))
#endif	/* !OS2_INCLUDED */

#define MAKEWORD(l, h)	((WORD)(((BYTE)(l)) | ((WORD)((BYTE)(h))) << 8))
#define	LOWORD(l)	((WORD)(l))
#define	HIWORD(l)	((WORD)(((DWORD)(l) >> 16) & 0xffff))

/* DCHAR = Double Byte Character */
typedef	WORD		DCHAR;
/* CHAR = unsigned char */
typedef unsigned char	CHAR;

/* ACHAR = either a DCHAR or a CHAR */
#ifdef KANJI
typedef	DCHAR		ACHAR;
extern BOOL PASCAL fKanaAccel;	/* set by application to choose accelerators */
#else
typedef	CHAR		ACHAR;
#endif

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
extern BOX PASCAL boxActiveWnd;
extern BOX PASCAL boxInactiveWnd;
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
#ifndef REVIEW	/* size issue */
	RRC	rrcInvalid;			/* overlap update info */
#endif
	PLFN_WNDPROC pfnWndProc;		/* Medium Model */
	struct _wnd *pwndParent;
	struct _wnd *pwndSibling;
	struct _wnd *pwndChild;
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef NO_WNDEXTRA
	WORD	rgwExtra[cwExtraWnd];
#endif
	} WND;

#ifndef	REVIEW	/* size issue */ /* for wnd static window construction macros */
#define	rrcInvalidStd	{0, 0, 0, 0},
#else
#define	rrcInvalidStd
#endif

typedef WND *PWND;
#endif /*!NOWND*/


extern BYTE	PASCAL	fMousePresent;		/* valid after init */

#ifndef	NOPROCS

VOID		FARPUBLIC GetProgDir(char *);			/*OPTIONAL*/

BOOL		FARPUBLIC FInitCow(void);			/*OPTIONAL*/
VOID		FARPUBLIC LeaveCow(BOOL);			/*OPTIONAL*/
VOID		FARPUBLIC BackToCow(BOOL);			/*OPTIONAL*/
VOID		FARPUBLIC EndCow(BOOL);				/*OPTIONAL*/

/* Swapped Cow exit */
#ifndef	NOSWAP
#ifdef	CC
VOID		FAR CDECL exit(int);				/*OPTIONAL*/
#endif
#endif

#endif	/* NOPROCS */


/*** mcb - mouse cursor block */
typedef struct mcb
	{
	WORD	colHot;			/* Hot Spot for			     */
	WORD	rowHot;			/*   graphics mouse cursor.	     */
	WORD	rgwAndMaskGfx[16];	/* Bit map masks for		     */
	WORD	rgwXorMaskGfx[16];	/*   graphics mouse cursor.	     */
	WORD	wAndMaskText;		/* Character and Attribute masks for */
	WORD	wXorMaskText;		/*   text mouse cursor.		     */
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

#ifndef	NOPROCS
BOOL		FARPUBLIC FEnableMouse(BOOL);
VOID		FARPUBLIC SetMouseCursor(MCB FAR *);		/*OPTIONAL*/
VOID		FARPUBLIC MouseConditionalOff(PARC);		/*OPTIONAL*/
VOID		FARPUBLIC SetMousePos(WORD, WORD);		/*OPTIONAL*/
VOID		FARPUBLIC SetMouseDoubleSpeed(WORD);		/*OPTIONAL*/
BOOL		FARPUBLIC SwapMouseButton(BOOL);		/*OPTIONAL*/
WORD		FARPUBLIC CbSizeMouseState(void);		/*OPTIONAL*/
VOID		FARPUBLIC SaveMouseState(BYTE FAR *);		/*OPTIONAL*/
VOID		FARPUBLIC RestoreMouseState(BYTE FAR *);	/*OPTIONAL*/
#endif	/* !NOPROCS */



typedef	struct _gheap {
	/* FGlobalHeapInfo */
	WORD	cpDataTotal;	/* Sum of all Data blocks */
	WORD	cpCodeTotal;	/* Sum of all Code blocks below fence */
	WORD	cpFreeTotal;	/* Size of all Free blocks below fence */
	WORD	cpFreeMac;	/* Size of largest Free below fence */
	WORD	cpReserve;	/* Size of codefence reserve area */
	} GHEAP;

extern BYTE PASCAL	fShellPresent;		/* DOS 3 : SHELL.EXE hook */

#ifndef	NOPROCS
HANDLE		FARPUBLIC GlobalAlloc(WORD, DWORD);		/*OPTIONAL*/
HANDLE		FARPUBLIC GlobalFree(HANDLE);			/*OPTIONAL*/
LPSTR		FARPUBLIC GlobalLock(HANDLE);			/*OPTIONAL*/
DWORD		FARPUBLIC GlobalHandle(HANDLE);			/*OPTIONAL*/
HANDLE		FARPUBLIC GlobalReAlloc(HANDLE, DWORD, WORD);	/*OPTIONAL*/
BOOL		FARPUBLIC GlobalUnlock(HANDLE);			/*OPTIONAL*/
DWORD		FARPUBLIC GlobalCompact(DWORD);			/*OPTIONAL*/

BOOL		FARPUBLIC FSetCodeReserve(WORD *); 		/*OPTIONAL*/
BOOL		FARPUBLIC FGlobalHeapInfo(GHEAP FAR *); 		/*OPTIONAL*/

DWORD		FARPUBLIC RerrExec(char *, char *, char *, BOOL, BOOL); /*OPTIONAL*/
								/*OPTIONAL*/
BYTE FAR *	FARPUBLIC LpbAppDataUnpack(void);		/*OPTIONAL*/
VOID		FARPUBLIC DosIdle(void);			/*OPTIONAL*/
VOID		FARPUBLIC BindSegment(PFN, BOOL);		/*OPTIONAL*/
VOID		FARPUBLIC AccessSwapFile(BOOL);			/*OPTIONAL*/
#endif	/* !NOPROCS */

/* GlobalAlloc flags */
#define GMEM_MOVEABLE	    0x0002
#define GMEM_ZEROINIT	    0x0040


/* rerr return values */

#define	rerrOk			0			/* must be zero! */

#define	rerrBadFile		2
#define	rerrBadPath		3
#define	rerrAccessDenied	5
#define	rerrNoMemory		8

#define	rerrBadEnv		10
#define	rerrBadFormat		11

#define	rerrBadRead		30

#define	rerrBadVersion		90
#define	rerrBadMemReq		91
#define	rerrMemUnstable		92
#define	rerrNoService		93
#define	rerrTSR			94

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

#define KJ_SC			0xff00
#define KJ_KANA 		0x0080
#define KJ_OTHER		0x0040
#define KJ_KK			0x00b0
#define KJ_COUNT		0x003f

#define	abAbortNone	0
#define	abAbortESC	1
#define	abAbortBRK	2

extern BOOL PASCAL abAbort;			/* normal Abort */
extern BOOL PASCAL fPollKeyboard;		/* Poll the keyboard ? */
extern BYTE PASCAL fKeyIsUp, PASCAL fKeyWasUp;	/* Key transitions */
extern WORD PASCAL wRateKeyRepeat;		/* repeat rate */

#ifndef	NOPROCS
DWORD	FARPUBLIC EnableKeyboard(BOOL);
VOID	FARPUBLIC PollKeyboard(void);
VOID	FARPUBLIC PollPhysicalKeyboard(void);
VOID	FARPUBLIC SetShiftKk(WORD);
VOID	FARPUBLIC DisableExtendedKeyboard(void);
VOID	FARPUBLIC SetTsrProtocol(WORD, BYTE, BYTE);
#endif	/* !NOPROCS */

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

typedef	struct _cib
	{
	/* CSD info block */
	WORD	flags;
	WORD	coImbue1;
	} CIB;

typedef	struct _cdi
	{
	/* CSD Default Information */
	WORD	cwcib;			/* length of rgw */
	CIB	cib;			/* info words */
	} CDI;


/*****************************************************************************/
/* font variations */

#ifdef KANJI
typedef	DWORD	FFONT;			/* 32 bit FFONT for Kanji */
#else
typedef	WORD	FFONT;			/* 16 bits normal FFONT */
#endif


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
#define	ffontReservedBits	0x0F00
#define	ffontExtraMask		0xf000	/* one of 16 extra items */
#define	ffontOrUnderlineSupport	0x1000

#ifdef KANJI
/* 32-bit ffont extension for Kanji support */
#define	ffontVgrid		0x000F0000
#define	ffontHgrid		0x00F00000
#define	ffontShadeType		0x03000000
#define	ffontDoubleWidth	0x04000000
#define	ffontDoubleHeight	0x08000000
#define	ffontDoubleRight	0x10000000
#define	ffontDoubleLower	0x20000000
#define	ffontDwLeft		(ffontDoubleWidth)
#define	ffontDwRight		(ffontDoubleWidth | ffontDoubleRight)
#define	ffontDhUpper		(ffontDoubleHeight)
#define	ffontDhLower		(ffontDoubleHeight | ffontDoubleLower)
#define	ffontReserved		0xC0000000
#endif /*KANJI*/


/* CSD flags */
#define	CSD_FORCERESET	0x0001	/* force the CSD to do a BIOS reset */
#define	CSD_SNOW		0x0002	/* use snow protection */

/*****************************************************************************/
/* Installable state */

typedef WORD	FINST;
#define	finstText		0x0001	/* text mode */
#define	finstGraphics		0x0002	/* graphics mode */
#define	finstMonochrome		0x0004	/* monochrome mode */
#define	finstAlternate		0x0008	/* alternate adapter (2nd screen) */
#define	finstFont		0x0010	/* supports fonts */
#define	finstAttrFont		0x0020	/* map Font calls into attribute info */
#define	finstExtendedMono	0x0040	/* EGA and VGA mono */
#define	finstGraphicArc		0x0080	/* Save/Restore Graphics Arc support */
#define	finstBltArcCsd		0x0100	/* Specific BltArcCsd available */
#define finstSizeable		0x0200	/* app can specify size when mode is
					   initialized */
#define	finstDisableMouse	0x1000	/* gfx w/o mouse support (hercules,os/2) */
#define	finstFastScroll		0x2000	/* fast scroll (BltArc) for gfx text */
#define	finstQuestionable	0x4000	/* questionable mode selection */
#define	finstAvailable		0x8000	/* available with current hardware */


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
	WORD	ffontSupported;		/* valid ffont values */
					/* non-grid line styles */

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

/* INDT: Driver Service Types */			/* Extension and Owner/Initiator */
#define	indtNil			0
#define	indtKeyboard		1			/* .KBD	CW */
#define	indtCharacterScreen	2			/* .CSD	CW */
#define	indtGraphicScreen	3			/* .GSD	CW */
#define	indtCharacterPrinter	4			/* .PRD	? */
#define	indtGraphicPrinter	5			/* .GPD	CW */
#define	indtSystem		6			/* .SYD	CW */
#define	indtSerialComm		7			/* .SCD	EBU */
#define	indtCellPrinter		8			/* .CPD	MSKK */
#define	indtMachineData		9			/* .MDD	MSKK */
#define	indtKkcInterface	10			/* .KID	MSKK */
#define	indtPseudoPrinter	11			/* .PPD MSKK */
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

#ifndef	NOPROCS
WORD	FARPUBLIC RerrLoadDrv(char *, INDV *, WORD);	/* OPTIONAL */
WORD	FARPUBLIC RerrLoadCwDrv(char *);		/* OPTIONAL */
WORD	FARPUBLIC RerrLoadDrvFd(WORD, INDV *, WORD);	/* OPTIONAL */
WORD	FARPUBLIC RerrLoadCwDrvFd(WORD);		/* OPTIONAL */
VOID	FARPUBLIC FreeDrv(INDV *, WORD);		/* OPTIONAL */
VOID	FARPUBLIC FreeCwDrv(void);			/* OPTIONAL */
#endif	/* !NOPROCS */


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
#define	chLeftSide2			(inch._chLeftSide2)
#define	chRightSide2		(inch._chRightSide2)
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

#ifndef	NOPROCS
WORD	FARPUBLIC ImodeGuessCurrent(void);			/*OPTIONAL*/
BOOL	FARPUBLIC FQueryInst(INST *, WORD);			/*OPTIONAL*/
BOOL	FARPUBLIC FAllocInstBuffers(INST *, LPFN_DRV_ALLOC, BOOL); /*OPTIONAL*/
VOID	FARPUBLIC FreeInstBuffers(INST *, LPFN_DRV_FREE);	/*OPTIONAL*/

BOOL	FARPUBLIC FAllocOverlapTable(INST *, LPFN_DRV_ALLOC);	/*OPTIONAL*/
VOID	FARPUBLIC FreeOverlapTable(LPFN_DRV_FREE);		/*OPTIONAL*/

BOOL	FARPUBLIC FInitScreen(INST *, WORD);				/*OPTIONAL*/
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

VOID	FARPUBLIC GetCSDDefaultInfo(CDI *);			/*OPTIONAL*/
VOID	FARPUBLIC SetCSDDefaultInfo(CDI *);			/*OPTIONAL*/

#ifdef KANJI
VOID	FARPUBLIC GetCharMap(INFT *, WORD, BYTE *);
#else /*!KANJI*/
VOID	FARPUBLIC GetCharMap(INFT *, BYTE, BYTE *);
#endif /*KANJI*/
#endif	/* !NOPROCS */


WORD		FARPUBLIC MessageBox(char *, char *, char *, WORD);
WORD		FARPUBLIC MessageBoxAxAy(char *, char *, char *, WORD, AX, AY);
VOID		FARPUBLIC SetDialogCaption(HANDLE, char *);
VOID		FARPUBLIC HiliteDialogAccel(void);
BOOL		FARPUBLIC EndDlgTmc(WORD);

/* Message Box Definitions */

#ifndef NOMB
#define	IDDEFAULT		10
#define IDOK	  		1
#define IDCANCEL  		2
#define IDABORT   		3
#define IDRETRY   		4
#define IDIGNORE  		5
#define IDYES	  		6
#define IDNO	  		7
#ifdef	HELP_BUTTON
#define IDHELP			8
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
#define	MB_BUTTON		0x0300	/* button mask */
#define	MB_DEFBUTTON1	0X0000	/* initial button to get focus */
#define	MB_DEFBUTTON2	0X0100
#define	MB_DEFBUTTON3	0X0200
#ifdef	HELP_BUTTON
#define	MB_DEFBUTTON4	0X0300
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

/* so that rspAppIdle can determine where the current msg box dialog is */
extern PWND	pwndDlg;		/* Again, app should never use */

#define PwndDlgCur()		pwndDlg


#ifndef NOWNDMACROS

#ifndef	REVIEW	/* fBorder=True not support */
#define wndEdit(id, fBorder, fEnabled, ax, ay, dax, day, pwndParent, pwndSibbling, szBuf, cchMax, chFill, isa, isaSel) {\
	id, WS_CHILD | WS_EDIT, \
	0, fEnabled, {ax, ay, ax+dax, ay+day}, \
	{ax, ay, ax+dax, ay+day}, \
	rrcInvalidStd	\
	(PLFN) InternalEditWndProc, pwndParent, pwndSibbling, NULL, 0, 0, \
	{0, (WORD) szBuf, cchMax, isa, isaSel, chFill, 0, 0, 0, 0, TRUE, 0, \
	 cchMax}}
#else
#define wndEdit(id, fBorder, fEnabled, ax, ay, dax, day, pwndParent, pwndSibbling, szBuf, cchMax, chFill, isa, isaSel) {\
	id, WS_CHILD | WS_EDIT | (fBorder ? WS_BORDER : 0), \
	0, fEnabled, {ax, ay, ax+dax, ay+day}, \
	{((fBorder) ? ax+1 : ax), (fBorder ? ay+1 : ay), \
	 ((fBorder) ? ax+dax-1 : ax+dax), (fBorder ? ay+day-1 : ay+day)}, \
	rrcInvalidStd	\
	(PLFN) InternalEditWndProc, pwndParent, pwndSibbling, NULL, 0, 0, \
	{0, (WORD) szBuf, cchMax, isa, isaSel, chFill, 0, 0, 0, 0, TRUE, 0, \
	 cchMax}}
#endif

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

#define EN_CHANGE			0x0300
#define EN_CURSORMOVED			0x0301


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

#ifndef	OS2_INCLUDED
#define	HSEM	DWORD
#endif	/* !OS_INCLUDED */

#ifndef OS2
extern BOOL PASCAL fMessage;
#ifdef DUAL
extern DWORD PASCAL semaMessage;	/* message semaphore */
#define	hsemaMessage	((HSEM) (DWORD FAR *) &semaMessage)
#endif /*DUAL*/
#else
extern DWORD PASCAL semaMessage;	/* message semaphore */
#define	hsemaMessage	((HSEM) (DWORD FAR *) &semaMessage)
#endif
#endif /*!NOMSG*/

typedef BOOL (FARPUBLIC *PBFN_KBDMSG)(WORD, WORD, DWORD);

#ifndef NOWINMESSAGES
/* Window Messages */
#define	WM_NULL			0x0000
#define	WM_CREATE		0x0001
#define	WM_WANTFOCUS		0x1005
#define	WM_MAKEACTIVE		0x1006
#define	WM_SETFOCUS		0x0007
#define	WM_KILLFOCUS		0x0008
#define	WM_REPAINT		0x100e
#define	WM_PAINT		0x000f
#define	WM_QUIT			0x0012

#define	repModeReset	0		/* handled by RepaintScreen */
#define	repRedraw		1		/* handled by RepaintScreen */    
#define	repGraphic	2		/* app should redraw GSD graphic */

/* Non-client (for Overlap only) */
#define	WM_NCLBUTTONDOWN	0x00a1

#define	WM_KEYFIRST		0x0100
#define WM_KEYLAST		0x0102

#define	WM_KEYDOWN		0x0100
#define	WM_KEYUP		0x0101
#define	WM_CHAR			0x0102

#define	WM_CUT			0x0300

#define	WM_COPY			0x0301
#define	WM_PASTE		0x0302
#define	WM_INSERT		0x1303

#define	WM_MENUIDLE		0x1110		/* Menu Idle */
#define	WM_MENUINACTIVE		0x1111		/* Menu inactive */
#define	WM_COMMAND		0x0111
#define	WM_MENUSELECT		0x0112		/* selecting a menu item */
#define	WM_MENUSTART		0x0113		/* starting a menu action */
#define	WM_HSCROLL		0x0114
#define	WM_VSCROLL		0x0115
#define	WM_INITMENUPOPUP	0x0117
#define	WM_ALARM		0x1118
#define WM_DISABLEDCOMMAND	0x0119

#define WM_MESSAGEBOX_START	0x0120
#define WM_MESSAGEBOX_END	0x0121

#define	WM_MOUSEFIRST		0x0200
#define	WM_LMOUSELAST		0x0203	/* last of Left mouse actions */
#define	WM_MOUSELAST		0x0209

#define	WM_MOUSEMOVE		0x0200	/* mouse related constants */
#define	WM_LBUTTONDOWN		0x0201
#define	WM_LBUTTONUP		0x0202
#define	WM_LBUTTONDBLCLK	0x0203
#define	WM_RBUTTONDOWN		0x0204
#define	WM_RBUTTONUP		0x0205
#define	WM_RBUTTONDBLCLK	0x0206

/* Edit Wnd Proc Messages */
#define EM_SETSEL		0x0400
#define EM_GETSEL		0x0401

/* Debugging Message */
#define WM_TRACEOUT		0x0500
#define WM_TRACETAG_REGISTER	0x0501

/* Overlap Wnd Proc Messages */
#define WM_ACTIVATE		0x0006
#define WM_ZOOM			0x0321
#define WM_CLOSE		0x0010
#define WM_MOVE			0x0003
#define WM_SIZE			0x0005


/* WM_ACTIVATE response codes */
#define	rspActiveLive		((DWORD) 2)	/* pass activating event on */
#define	rspActive			((DWORD) 1)	/* eat activating event */
#define	rspActiveDecline	((DWORD) 0)	/* [de]activation declined */


/* Dialog related messages */
#define WM_DIALOGINACTIVE	0x0360

/*WM-DIALOG is actually private (and LB_SETWIDTH)*/
#define WM_DIALOG		0x0380

#define WM_DIALOGIDLE 0x0382

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
#define LB_HILITECURSEL		0x0350

/* VAP Request for application to handle */
#define WM_VAP_REQUEST		0x0390

/* Kkc converter message for application to handle */
#define	WM_KKCONVERT	0x03f0

#define	WM_KKENABLE	0x03f7
#define	WM_KKDISABLE	0x03f8

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
#ifndef REVIEW /* should UndoRepeat be exported? */
VOID		FARPUBLIC UndoRepeat(WORD, DWORD);
#endif
VOID		FARPUBLIC UngetMessage(PMSG);
BOOL		FARPUBLIC PeekMessage(PMSG);
BOOL		FARPUBLIC FNextMsg(PMSG);
PWND		FARPUBLIC GetFocus(void);
PWND		FARPUBLIC SetFocus(PWND);
VOID		FARPUBLIC FlushAbort(void);
PWND		FARPUBLIC SetCapture(PWND);
VOID			FARPUBLIC ReleaseCapture(void);
DWORD		FARPUBLIC DispatchMessage(PMSG);
BOOL		FARPUBLIC PostMessage(PWND, WORD, WORD, DWORD);
DWORD		FARPUBLIC SendMessage(PWND, WORD, WORD, DWORD);
WORD		FARPUBLIC SetDoubleClickTime(WORD);		/*OPTIONAL*/
VOID		FARPUBLIC SynthesizeShiftKeys(WORD, WORD);	/*OPTIONAL*/
VOID		FARPUBLIC HookKeyboardMessage(BOOL, PBFN_KBDMSG);
VOID		FARPUBLIC HookKeyboardPoll(BOOL, LPFN);
BOOL		FARPUBLIC InsertKeyboardMessage(WORD, WORD, DWORD);
WORD		FARPUBLIC CmsgKeyboardQueueEntry(WORD, PMSG);

#ifndef NOCOLOR

/*	* General colors */
#define	isaNil			((ISA) -1)

#define	isaBackground		0

#define	isaHilite		1		/* hilite / inversion */
#define	isaGreyed		2		/* not currently used */
#define	isaEnabled		3
#define	isaDisabled		4
#define	isaAlert		5

/*	* Dialog elements :	*/
#define	isaDialogBox		6	/* the actual dialog box */
#define	isaStatic		isaDialogBox	/* static text */
#define	isaButton		isaDialogBox	/* radio/check buttons */
#define	isaPushButton		7		/* push buttons */
#define	isaButtonDown		8		/* pushed button */
#define	isaListBox		9		/* listbox background */
#define	isaEdit			isaDialogBox

/*	* Scroll Bars :		*/
#define	isaScrollbar		10
#define	isaElevator		11

/*	* Menus :		*/
#define	isaMenuBox		12		/* box around pull downs */
#define	isaMenu			13		/* non-selected MENU */
#define	isaMenuSelected		14		/* selected menu item */
#define	isaMenuHilite		15		/* hilited character */
/* hilited character under selection */
#define	isaMenuHiliteSel	16		/* for menu titles */
#define	isaItemHiliteSel	17		/* for menu items */

#define	isaDialogAccel		18		/* dialog accelerators */
#define	isaDialogAccelBor	19		/* dialog accelerator border */

/*	* Shadows :		*/
#define	isaShadow		20

/* User Colors :		*/
#define	isaUserMin		21
#define	isaUserMax		(isaUserMin+16)
#define	isaMax			isaUserMax

#ifdef KANJI
#define	ffontNil	((FFONT)0L)
#else
#define	ffontNil	((FFONT)0)
#endif	/*KANJI*/

#endif /*!NOCOLOR*/

#ifndef NOMENUS
/* hack for nameless unions in CC */
#ifdef CC
#define CC_USZ u
#define CC_URG u
#define cwExtraMenu 1
#else
#define CC_USZ
#define CC_URG
#define cwExtraMenu 0
#endif

/* Menu Modes : HIWORD(lParam) for WM_COMMAND / WM_MENUSELECT messages */
#define	mmdItem		0		/* dropdown item */
#define	mmdAccel	1		/* keyboard accelerator */
#define	mmdMenu		2		/* dropdown not item */
#define mmdString	3		/* WM_MENUSELECT contains string */

/* Help Line codes for wParam WM_MENUSELECT (lParam == 0) */
#define	enClear		0		/* clear must be zero */
#define	enCommand	1		/* prepare help line for command */


typedef struct _mpvkeyid
	{
	WORD	vkey;
	WORD	idItem;
	} MPVKEYID;
#define VkeyOfVkKk(vk, kk)	((vk) | (kk))

#ifndef SMM			/* Old menu structures */

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
#ifdef KANJI
	WORD	chKanaAccel;		/* Kana Accelerators */
#endif /*KANJI*/
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
#ifdef KANJI
	WORD	chKanaAccel;		/* Kana Accelerators */
#endif /*KANJI*/
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

#ifndef KANJI
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

#else	/* KANJI - 1 extra value needed for Kana Accelerator */
	/* Accelerator should always be at start */

#define menuitem(mid, sz, chKana, w) {(mid), TRUE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w), (WORD) (chKana)},
#define menuitemD(mid, sz, chKana, w) {(mid), FALSE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w), (WORD) (chKana)},
#define	menuitemSep	{0, FALSE, FALSE, TRUE, FALSE, 0, 0, {NULL}, 0, 0},

#endif /*KANJI*/

#else // SMM			/* New menu structures */

typedef struct _mtm
	{
	WORD	id;
	BYTE	bFlags;
/*	BITS	fDisabled:1;
	BITS	fChecked:1;
	BITS	fSeparator:1;
	BITS	fString:1;
	BITS	fHelp:1;
	BITS	fMenuBreak:1;
	BITS	fSubMenu:1;
	BITS	filler:1;	*/
	BYTE	cwTotal;
	union
		{
		WORD	sid;
		WORD	iwString;
		} CC_USZ;
	WORD	rgwExtra[cwExtraMenu];
	} MTM, *PMTM;

typedef struct _mnu
	{
	struct _mnu **hmnuNext;
	WORD	cmtm;
	struct _mtm grmtm;
	} MNU, **HMNU;

#define MM_DISABLED	0x01
#define MM_CHECKED	0x02
#define MM_SEPARATOR	0x04
#define MM_STRING	0x08
#define MM_HELP		0x10
#define MM_MENUBREAK	0x20
#define MM_SUBMENU	0x40

/* defaults: */

#define MM_ENABLED	0
#define MM_UNCHECKED	0
#define MM_SID		0

/* special ID#'s */

#define idNil		((WORD) 0)
#define idSysMenu	((WORD) -1)

#define cbMTM (sizeof(MTM)-cwExtraMenu*sizeof(WORD))
#define cwMTM (cbMTM/sizeof(WORD))
#define cbMNU (sizeof(MNU)-sizeof(MTM))

#define HmnuFromPmtm(pmtm) ((HMNU) pmtm->rgwExtra[pmtm->cwTotal-1])
#define SzFromPmtm(pmtm) ((CHAR *) &(pmtm->rgwExtra[pmtm->u.iwString]))

#endif // SMM

#endif /*!NOMENUS*/

#ifndef	NOPROCS
BOOL		FARPUBLIC FEnableMenuBar(BOOL);
VOID		FARPUBLIC EnableMenuItem(WORD, BOOL);
VOID		FARPUBLIC CheckMenuItem(WORD, BOOL);
BOOL		FARPUBLIC FMenuItemChecked(WORD);
VOID		FARPUBLIC DrawMenubar(VOID);
VOID		FARPUBLIC SetMenuKeys(WORD, WORD);	/*OPTIONAL*/
VOID		FARPUBLIC OpenMenu(WORD);		/*OPTIONAL*/
VOID		FARPUBLIC DeAltMenu(VOID);		/*MACROS ONLY*/

#ifndef SMM			/* Old functions */

VOID		FARPUBLIC InitMenu(PWND, PMENUBAR);
PMENUITEM	FARPUBLIC FindMenuItem(WORD);
VOID		FARPUBLIC EnableMenu(WORD, BOOL);

#else // SMM

				/* Replacements for above */
VOID		FARPUBLIC InitMenubar(PWND, HMNU, PARC, WORD);	/*OPTIONAL*/
PMTM		FARPUBLIC PmtmFindId(HMNU, WORD, BOOL); 	/*OPTIONAL*/

				/* New functions */

VOID		FARPUBLIC OpenMenuPopup(PWND, HMNU, PARC, BOOL);	/*OPTIONAL*/
HMNU            FARPUBLIC HmnuNewMenu(void);            /*OPTIONAL*/
PMTM		FARPUBLIC PmtmAddItem(HMNU, WORD, WORD, CHAR *, HMNU, WORD, BYTE);	/*OPTIONAL*/
VOID		FARPUBLIC DeleteItem(HMNU, WORD);	/*OPTIONAL*/
VOID            FARPUBLIC DeleteMenu(HMNU, BOOL);       /*OPTIONAL*/
WORD            FARPUBLIC CchGetMenuString(HMNU, WORD, CHAR *, WORD); /*OPTIONAL*/
VOID		FARPUBLIC AddAccelTable(MPVKEYID **);	/*OPTIONAL*/
VOID		FARPUBLIC DeleteAccelTable(MPVKEYID **);	/*OPTIONAL*/

#endif // SMM

#endif	/* !NOPROCS */


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

#define	dmFfontOnly		0xB00

/* special FFONT control */
#define	fdmKeepFfont	0x8000

#ifdef 	KANJI

/* kanji gridline support */
#define	dmGridOnly		0xC00
#define	fdmKeepGrid		0x4000

#endif	/*KANJI*/

#endif /*!NOCOLOR*/

extern BYTE PASCAL fMonochrome;		/* TRUE => monochrome screen */
extern char PASCAL chShadow;		/* shadow character */
extern WORD PASCAL diShadow;		/* shadow draw mode, 0 => no shadow */

#ifndef	NOPROCS
VOID		FARPUBLIC SetCursorBlock(BOOL);
VOID		FARPUBLIC SetCursorBlink(BOOL);
VOID		FARPUBLIC RepaintScreen(BOOL);
#endif	/* !NOPROCS */

VOID		FARPUBLIC SetGraphicArc(PARC, BOOL);


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
	rrcInvalidStd	\
	(PLFN) ScrollBarWndProc, pwndParent, pwndSibbling, NULL, 0, 0, \
	{ctickRep, 0, 0, 1, 1}}

#endif /* !NOWNDMACROS */

#ifndef	NOPROCS
short		FARPUBLIC SetScrollPos(PWND, short, BOOL);
short		FARPUBLIC GetScrollPos(PWND);
VOID		FARPUBLIC SetScrollRange(PWND, short, short, BOOL);
#endif	/* !NOPROCS */


#ifndef	NOPROCS
VOID		FARPUBLIC AddChild(PWND, PWND);
VOID		FARPUBLIC RemoveChild(PWND);
VOID		FARPUBLIC EnableWindow(PWND, BOOL);
VOID		FARPUBLIC GetClientRrc(PWND, RRC *);
VOID		FARPUBLIC SetWindowStyle(PWND, WORD);
VOID		FARPUBLIC SetWindowSize(PWND, BYTE, BYTE);
VOID		FARPUBLIC DrawWindow(PWND);
VOID		FARPUBLIC TextOut(PWND, RX, RY, char *, short, WORD);
VOID		FARPUBLIC DrawBorder2(PWND, BOX *, WORD, WORD, char *);
VOID		FARPUBLIC DrawBorderAlign(PWND, BOX *, WORD, WORD, char *, BOOL);
VOID		FARPUBLIC CharOut(PWND, RX, RY, ACHAR, WORD);
VOID		FARPUBLIC CharOutBorder(PWND, RX, RY, ACHAR, WORD);
VOID		FARPUBLIC FillRrc(PWND, PRRC, ACHAR, WORD);
VOID		FARPUBLIC BltRrc(PWND, RX, RY, BYTE, BYTE, RX, RY);
VOID		FARPUBLIC BltRrcTop(PWND, RX, RY, BYTE, BYTE, RX, RY);	/*OPTIONAL*/
VOID		FARPUBLIC BeginPaint(PWND, VOID FAR *);
VOID		FARPUBLIC EndPaint(PWND, VOID FAR *);
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
VOID		FARPUBLIC EnableOverlap(BOOL, PWND, WORD);			/*OPTIONAL*/
VOID		FARPUBLIC AddChildHead(PWND, PWND);		/*OPTIONAL*/
VOID		FARPUBLIC AddChildTail(PWND, PWND);		/*OPTIONAL*/
VOID		FARPUBLIC RethinkDisplay(void);			/*OPTIONAL*/
VOID		FARPUBLIC WindowToTop(PWND);            	/*OPTIONAL*/
VOID		FARPUBLIC RedrawDamagedRegions(void);		/*OPTIONAL*/
VOID		FARPUBLIC InvalidateRrc(PWND,PRRC);		/*OPTIONAL*/
BOOL		FARPUBLIC FMoveSizeWithKeyboard(PWND, BOOL);	/*OPTIONAL*/
BOOL		FARPUBLIC FIsTopWindow(PWND);			/*OPTIONAL*/
PWND		FARPUBLIC PwndGetTopWindow(PWND);		/*OPTIONAL*/
VOID		FARPUBLIC DrawOverlapShadow(PWND);		/*OPTIONAL*/
VOID		FARPUBLIC MoveSizeOverlap(PWND, AX, AY, BYTE, BYTE); /*OPTIONAL*/
/*	Listbox routines */
DWORD		FARPUBLIC ListBoxWndProc(PWND, WORD, WORD, DWORD);
VOID		FARPUBLIC InitListBox(PWND, PWFN);		/*OPTIONAL*/
VOID		FARPUBLIC InitListBoxOriented(PWND, PWFN, WORD *, WORD *);/*OPTIONAL*/
VOID		FARPUBLIC GetListBoxOrientation(PWND, WORD *, WORD *);/*OPTIONAL*/
/*	Dropdown Listbox routines */
DWORD		FARPUBLIC DropDownWndProc(PWND, WORD, WORD, DWORD);	/*OPTIONAL*/
DWORD		FARPUBLIC DropHolderWndProc(PWND, WORD, WORD, DWORD);	/*OPTIONAL*/
DWORD		FARPUBLIC DropListBoxWndProc(PWND, WORD, WORD, DWORD);	/*OPTIONAL*/
DWORD		FARPUBLIC DropEditWndProc(PWND, WORD, WORD, DWORD);	/*OPTIONAL*/
DWORD		FARPUBLIC DropButtonWndProc(PWND, WORD, WORD, DWORD);	/*OPTIONAL*/
VOID		FARPUBLIC AddDropWindow(PWND, PWND, PWND, PWND, PWND, PWND, BOOL); /*OPTIONAL*/
VOID		FARPUBLIC RemoveDropWindow(PWND);			/*OPTIONAL*/
#endif	/* !NOPROCS */

#define	OV_NOSHADOW	0x0001

#define	axNil	255
#define	ayNil	255
#define	rxNil	255
#define	ryNil	255

#define FMoveOverlapWithKeyboard(pwnd) FMoveSizeWithKeyboard(pwnd,TRUE);
#define FSizeOverlapWithKeyboard(pwnd) FMoveSizeWithKeyboard(pwnd,FALSE);
#define MoveWindowOverlap(pwnd, ax, ay) MoveSizeOverlap(pwnd, ax, ay, rxNil, ryNil)
#define SizeWindowOverlap(pwnd, drx, dry) MoveSizeOverlap(pwnd, axNil, ayNil, drx, dry)
#define CloseWindowOverlap(pwnd) MoveSizeOverlap(pwnd, axNil, ayNil, rxNil, ryNil)
#define DrawBorder(pwnd, pbox, di, sz)	DrawBorder2(pwnd, pbox, di, di, sz)

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
	rrcInvalidStd	\
	(PLFN) pfnWndProc, pwndParent, pwndSibling, pwndChild, 0, 0,

#define wndGenericCursor(id, style, fEnabled, ax, ay, dax, day, pfnWndProc, pwndParent, pwndSibling, pwndChild, axCurs, ayCurs) { \
	id, style, TRUE, fEnabled, \
	{ax, ay, ax+dax, ay+day}, \
	{((style) & WS_BORDER) ? ax+1 : ax, \
	 ((style) & WS_BORDER) ? ay+1 : ay, \
	 ((style) & (WS_BORDER | WS_VSCROLL)) ? ax+dax-1 : ax+dax, \
	 ((style) & (WS_BORDER | WS_HSCROLL)) ? ay+day-1 : ay+day}, \
	rrcInvalidStd	\
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
	rrcInvalidStd	\
	(PLFN) pfnWndProc, pwndParent, pwndSibling,      \
	pwndChild, \
        (fBorder ? ax+2 : ax+1), 1 \
 	,{0,0,0,0,0,0,0,0,0,0,1,0,0,color,isaHilite,ctickRep,0} }

#define wndDropDown(id,fEnabled,fCombo) { \
	id, (WS_CHILD | WS_CLIPOUT), FALSE, TRUE, \
	{ 0, 0, 0, 0 }, { 0, 0, 0, 0 }, \
	rrcInvalidStd \
	(PLFN) DropDownWndProc, NULL, NULL, NULL, 0, 0, \
	{ 0, (fCombo ? 1 : 0), NULL, NULL, NULL, NULL, \
		NULL, NULL, NULL, NULL } }

#define wndDropDownButton(id, fEnabled, ax, ay) { \
	id, WS_CHILD | WS_CLIPOUT, 0, fEnabled, \
	{ ax, ay, ax + 1, ay + 1 }, \
	{ ax, ay, ax + 1, ay + 1 }, \
	rrcInvalidStd \
	(PLFN) DropButtonWndProc, \
	NULL, NULL, NULL, 0, 0 }

#define wndDropDownHolder() { \
	0, 0, 0, 0, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, \
	rrcInvalidStd \
	(PLFN) NULL, NULL, NULL, NULL, 0, 0, { NULL } }

#define	GetWindowProc(pwnd) ((pwnd)->pfnWndProc)
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
#define WS_SUBSTYLE		0x001f

#define WS_BEENDIRTIED		0x0020
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

/* special edit window specific isa's */
#define IsaEdit(pwnd)	((pwnd)->rgwExtra[3])
#define IsaSelEdit(pwnd)	((pwnd)->rgwExtra[4])
#define IsaDisabledEdit(pwnd)	((pwnd)->rgwExtra[13])

/* special listbox window specific isa's */
#define IsaListbox(pwnd)	((pwnd)->rgwExtra[13])
#define IsaHiliteListbox(pwnd)	((pwnd)->rgwExtra[14])


/*****************************************************************************/
/* System Specifics */
#ifndef	NOPROCS
VOID		FARPUBLIC DoSound(WORD,WORD);
DWORD		FARPUBLIC ClockTicks(void);
BYTE		FARPUBLIC GetPrinterTimeOut(BYTE);
VOID		FARPUBLIC SetPrinterTimeOut(BYTE,BYTE);
WORD		FARPUBLIC PrinterCmd(BYTE,BYTE,BYTE);
WORD		FARPUBLIC ComCmd(BYTE,BYTE,BYTE);
#endif	/* !NOPROCS */


extern BYTE PASCAL	fSingleFloppy;	/* TRUE => 1 floppy system */
extern BYTE PASCAL	fInt24Error;	/* Set if INT 24 error detected */

/* Sound Facilities */
#define	Beep()		DoSound(3,1400)
#define	Click()		DoSound(2,700)

BOOL		FARPUBLIC FValidDrive(char);

/* File find info; it's the DOS 3 structure, emulated in OS/2. */
typedef struct _fde
	{
	char	reserved[21];	/* MSDOS requires this	*/
	char	atr;		/* File attribute	*/
	WORD	wTime;		/* File time of last write */
	WORD	wDate;		/* File date of last write */
	DWORD	cbFile;		/* File size in bytes	*/
#ifndef OS2
	char	szName[13];	/* File name packed	*/
#else	
	char	szName[256];	/* File name packed	*/
#endif
	} FDE;	/* Find directory entry */

typedef FDE *PFDE;

BOOL		FARPUBLIC FFindFirst(PFDE, char *, WORD);
BOOL		FARPUBLIC FFindNext(PFDE);
BOOL		FARPUBLIC FindClose(void);
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

#define VK_RESIZE	0x1A0

/* alternative names */
#define	VK_MENU		VK_ALT
#define	VK_CAPITAL	VK_CAPLOCK
#define	VK_OEM_NUMBER	VK_NUMLOCK
#define	VK_OEM_SCROLL	VK_SCRLOCK
#define	VK_SEPARATER	VK_SEPARATOR

#endif /*!NOVIRTUALKEYCODES*/	

#ifndef	NOPROCS
BOOL		FARPUBLIC FIsDbcsChar(ACHAR);		/* OPTIONAL */
WORD		FARPUBLIC CchLenDbcs(unsigned char *);		/* OPTIONAL */
char *		FARPUBLIC PchNextDbcs(unsigned char *);		/* OPTIONAL */
char *		FARPUBLIC PchPrevDbcs(unsigned char *, unsigned char *);	/* OPTIONAL */
WORD		FARPUBLIC CtOfDbcsChar(unsigned char *, unsigned char *);	/* OPTIONAL */
WORD		FARPUBLIC CtOfDbcsCharX(unsigned char far *, unsigned char far *);	/* OPTIONAL */
VOID		FARPUBLIC TextOutAttrX(PWND, RX, RY, PRRC, unsigned char far *, unsigned char far *, short, short);	/* OPTIONAL */
#endif	/* !NOPROCS */


/* Character Types */
#define	ctSNG	0	/* single byte character */
#define	ctDB1	1	/* 1st byte of double byte character */
#define	ctDB2	2	/* 2nd byte of double byte character */


#define	SetSysColor(isa, coBack, coFore, fBlink, fHilite) \
	SetIsaColor(isa,				\
 	    (coFore) + ((fHilite) ? 8 : 0),	\
 	    (coBack) + ((fBlink) ? 8 : 0))

#ifndef	NOPROCS
VOID		FARPUBLIC SetIsaColor(ISA, WORD, WORD);
VOID		FARPUBLIC GetIsaColor(ISA, WORD *, WORD *);
VOID		FARPUBLIC SetIsaRgca(ISA, BYTE *);
VOID		FARPUBLIC SetIsaFfont(ISA, FFONT);		/*OPTIONAL*/
# ifdef KANJI
FFONT		FARPUBLIC GetIsaFfont(ISA);			/*OPTIONAL*/
# endif /* KANJI */
#endif	/* !NOPROCS */



/* LszPromptSwapDisk */

#define	rpsdNULL	-1
#define	rpsdNotFound	1
#define	rpsdLocked	2
#define	rpsdLoadError	3


/* RspAppIdle */

#define	rspSleep	1		/* sleep some more */
#define	rspContinue	2		/* check for new events */
#define	rspAbort	3		/* abort from the current context */

#define	cnxNull		0		/* context is not given */

#define	cnxMenu		1		/* context is Menu */
#define	cnxDialog	2		/* context is Dialog */
#define	cnxMBox		3		/* context is Message Box */

#define	cnxMacro		4		/* context is Key Macro library */
#define	cnxVAP		5		/* context is VAP interface */

#define	cnxMoveSize	6		/* context is overlap window move/size */


#ifndef NOPROCS
CHAR FAR *	FARPUBLIC LszPromptSwapDisk(char far *, WORD, WORD);	/* OPTIONAL */
WORD		FARPUBLIC RspAppIdle(WORD, DWORD);		/* OPTIONAL */
BOOL		FARPUBLIC FHelpMsg(PMSG, WORD);		/* OPTIONAL */
#endif	/* !NOPROCS */



#define fKkcOn		0x0001		/* kana-kanji converter on */
#define fKkcScreen	0x0002		/* screen input mode */
#define fKkcEnScreen	0x0003		/* enable screen input mode */

BOOL FARPUBLIC FKKCInit(void);					/*OPTIONAL*/
VOID FARPUBLIC KKCEnd(void);					/*OPTIONAL*/
BOOL FARPUBLIC FKKCActive(void);				/*OPTIONAL*/
VOID FARPUBLIC KKCEnable(BOOL);					/*OPTIONAL*/
WORD FARPUBLIC FKKCGetMode(void);				/*OPTIONAL*/
VOID FARPUBLIC KKCSetMode(WORD);				/*OPTIONAL*/
VOID FARPUBLIC KKCWindowPos(BYTE, BYTE, BYTE, BYTE, BYTE);	/*OPTIONAL*/
BOOL FARPUBLIC FKKCMsg(PMSG);					/*OPTIONAL*/


/* CPD_STRUCTURE: as yet undocumented CPD structure */
typedef	void	CPD_STRUCTURE;		//REVIEW: define and document this !


/*****************************************************************************/
/* default machine data group/element ID code */
#define mdEidDir	0			/* directory element (all) */

#define mdGidGeneral	0			/* general group */
#define mdEidGenName	1			/* machine name element */

#define mdGidScreen	1			/* screen group */

#define mdGidKey	2			/* key group */

/*****************************************************************************/
/* System Specifics */
#ifndef	NOPROCS
BOOL		FARPUBLIC FInitMachineDataMdd(VOID);	/*OPTIONAL*/
WORD		FARPUBLIC CbMachineDataMdd(BYTE, BYTE); /*OPTIONAL*/
WORD		FARPUBLIC CbGetMachineDataMdd(BYTE, BYTE, WORD, BYTE FAR *);/*OPTIONAL*/
#endif	/* !NOPROCS */


#define	sopsFreeze	0
#define	sopsThaw		1

#define	serrOk		0
#define	serrNoMem	1
#define	serrBadSops	2

typedef	struct _csdc
	{
	WORD	cFreeze;
	WORD	psPrimOld;			/* old primary screen buffer */
	BYTE	axCursCur;
	BYTE	ayCursCur;
	VOID FAR *	lpfnFInit;
	VOID FAR *	lpfnMoveHwCurs;
	VOID FAR *	lpfnPrepUpdate;
	VOID FAR *	lpfnDoUpdate;
	VOID FAR *	lpfnDoneUpdate;
	VOID FAR *	lpfnBltArc;
	} CSDC;

WORD	FARPUBLIC	SerrScreenCtrl(WORD, CSDC FAR *, VOID FAR *);


/*****************************************************************************/
/* Pseudo Printer Specifics */
#ifndef	NOPROCS
WORD		FARPUBLIC RerrLoadPpd(char *);			/*OPTIONAL*/
VOID		FARPUBLIC FreePpd(void);			/*OPTIONAL*/
BOOL		FARPUBLIC FInitPpd(DWORD);			/*OPTIONAL*/
VOID		FARPUBLIC TermPpd(VOID);			/*OPTIONAL*/
BOOL		FARPUBLIC FPrintChPpd(WORD);			/*OPTIONAL*/
#endif	/* !NOPROCS */


/*****************************************************************************/
/* KKC Interface specifics (OPTIONAL) */

/* kkret = KK converter return value */

#define	kkretSuccess		0
#define	kkretFailure		(-1)
#define	kkretThrough		1

/* kkpos = Conversion position */

#define	kkposFree		0
#define	kkposBottom		1
#define	kkposCursor		2

/* Structure to communicate with KID */

#define	cbReservedKkcv		34

typedef struct _kkcv
	{
	WORD		wType;		/* Data type of wAscii */
	WORD		wScan;		/* Key scan code */
	WORD		wAscii;		/* Ascii code */
	WORD		wShift;		/* Shift key status */
	WORD		wExShift;	/* Extended Shift key status */

	WORD		cchResult;	/* Length of Result string */
	char FAR *	lpchResult;	/* Pointer to Result string buffer */

	WORD		cchMode;	/* Length of Mode string */
	char FAR *	lpchMode;	/* Pointer to Mode string buffer */
	char FAR *	lpattrMode;	/* Pointer to System attribute buffer */

	WORD		cchSystem;	/* Length of System string */
	char FAR *	lpchSystem;	/* Pointer to System string buffer */
	char FAR *	lpattrSystem;	/* Pointer to System attribute buffer */

	WORD		cchBuf;		/* Length of Display string */
	char FAR *	lpchBuf;	/* Pointer to Display string buffer */
	char FAR *	lpattrBuf;	/* Pointer to Display attribute buffer*/
	WORD		cchBufCursor;	/* Cursor position in Display buffer */                                                                                 
	char		rgbReserved[cbReservedKkcv];
	} KKCV;	/* KK Converter interface structure */


#ifndef NOPROCS
int		FARPUBLIC KKOpen(KKCV FAR *);			/*OPTIONAL*/
int		FARPUBLIC KKClose(VOID);			/*OPTIONAL*/
int		FARPUBLIC KKJoin(KKCV FAR *);			/*OPTIONAL*/
int		FARPUBLIC KKFree(VOID);				/*OPTIONAL*/
int		FARPUBLIC KKInOut(KKCV FAR *);			/*OPTIONAL*/
int		FARPUBLIC KKPosGet(VOID);			/*OPTIONAL*/
int		FARPUBLIC KKPosSet(KKCV FAR *, WORD);		/*OPTIONAL*/
BOOL		FARPUBLIC FKKMode(void);			/*OPTIONAL*/
int		FARPUBLIC KKSetMode(KKCV FAR *, BOOL);		/*OPTIONAL*/
#endif /* !NOPROCS */


#ifdef TRACES_ENABLED

/* leave space for null at end */
#define cchWord		7

/* tag structure */
#define cchTagId	12
#define cchOwner	12
#define cchDesc		36

#define FTAG		WORD

typedef struct _tag
	{
	WORD	id;
	CHAR	szTagId[cchTagId];
	CHAR	szOwner[cchOwner];
	CHAR	szDesc[cchDesc];
	FTAG	ftag;
	} TAG;

#define LPTAG		TAG FAR *
#define LSZ		CHAR FAR *

/* CW-defined ftags (the rest are for application use) */
#define ftagMaskOn	0x00ff
#define ftagRegistered	0x8000

/* data passed to application for trace */
typedef struct _tod
	{
	LPTAG		lptag;
	LSZ		lsz;
	LONG		lParam;
	} TOD;

/* function prototypes */
#ifndef NO_PROCS
VOID FARPUBLIC InitCWTraceTags();
PWND FARPUBLIC PwndSetPwndTrace(PWND);
VOID FARPUBLIC TraceTagRegister(LPTAG, WORD);
VOID FARPUBLIC CWTrace(LPTAG, LSZ, LONG);
VOID FARPUBLIC AppendSzToSz(CHAR *, CHAR **);
#endif	/* !NO_PROCS */

/* tracing on/off */
extern PWND PASCAL	pwndTrace;

#define FTracingOn()	(pwndTrace != NULL)

#define FTraceTagOn(tag) \
			(FTracingOn() && ((tag).ftag & ftagMaskOn))
#define FTraceTagRegistered(tag) \
			((tag).ftag & ftagRegistered)

/* full tracing macros */
#define Trace(tag)	if (FTraceTagOn(tag)) CWTrace((LPTAG) &(tag), NULL, 0L)

#define TraceSz(tag, sz) \
			if (FTraceTagOn(tag)) CWTrace((LPTAG) &(tag),	\
							(LSZ) sz, 0L)

#define TraceAssert(fCond, tag, sz) \
	 		if (FTraceTagOn(tag) && fCond)			\
				CWTrace((LPTAG) &(tag), (LSZ) sz, 0L)

#define TraceWord(tag, wParam)  \
			if (FTraceTagOn(tag))				\
				{					\
				CHAR	szWord[cchWord];		\
				SzFromInt(szWord, wParam);		\
				CWTrace((LPTAG) &(tag), (LSZ) szWord,	\
							(LONG) wParam);	\
				}

#define BeginTraceSz(tag) \
			if (FTraceTagOn(tag))				\
				{					\
				CHAR	_szTrace[cchSzTrace];		\
				CHAR *	_pchEnd = _szTrace;

/* MUST exist only within BeginTraceSz and EndTraceSz */
#define AppendSzToSzTrace(sz) \
				AppendSzToSz(sz, &_pchEnd)

#define AppendWordToSzTrace(wParam, fform) \
				SzFromInt(_pchEnd, wParam);		\
				_pchEnd += cchWord;

/* MUST follow previous BeginTraceSz() "call" */
#define EndTraceSz(tag) \
				Assert(_pchEnd - _szTrace < cchSzTrace);\
				CWTrace((LPTAG) &(tag), (LSZ) _szTrace, 0L);\
				}

#define DoBenchMark(tag, stmt) \
			{						\
			LONG	lTimeStart;				\
			WORD	wTimeElapsed;				\
			lTimeStart = ClockTicks();			\
			stmt;						\
			wTimeElapsed = (WORD) (ClockTicks() - lTimeStart); \
			TraceWord(tag, wTimeElapsed);			\
			}

#define Repeat(stmt, crep) \
			{ 						\
			REGISTER WORD	irep;				\
			for (irep = 0; irep < crep; irep++)		\
				stmt;					\
			}

#else	/* TRACES_ENABLED */

#define InitCWTraceTags()
#define PwndSetPwndTrace(pwnd)
#define TraceTagRegister(rgtag, ctag)
#define Trace(tag)
#define TraceSz(tag, sz)
#define TraceAssert(fCond, tag, sz)
#define TraceWord(tag, wParam, fform)
#define BeginTraceSz(tag)
#define AddSzToTraceSz(sz)
#define AddWordToTraceSz(sz)
#define EndTraceSz(tag)
#define DoBenchMark(tag, stmt)
#define Repeat(stmt, crep)

#endif	/* !TRACES_ENABLED */

