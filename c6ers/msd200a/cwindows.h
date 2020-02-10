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

-- Created Mon Dec 16 14:28:48 1991 */ 


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

// typedef DWORD	(FARPUBLIC *PLFN_WNDPROC)(struct _wnd *, WORD, WORD, DWORD);

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
	};


typedef struct _arc
	{
	BYTE	axLeft;
	BYTE	ayTop;
	BYTE	axRight;
	BYTE	ayBottom;
	};

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

typedef BOX *PBOX;

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



#ifdef BASED

typedef struct _wnd _based(pWndSeg) WND;
typedef WND *PWND;

typedef DWORD	(FARPUBLIC *PLFN_WNDPROC)(PWND, WORD, WORD, DWORD);

typedef struct _rrc RRC;
typedef struct _rrc *NPRRC;
typedef struct _rrc _based(pWndSeg) *PRRC;

typedef struct _arc ARC;
typedef struct _arc *NPARC;
typedef struct _arc _based(pWndSeg) *PARC;

typedef VOID _based(pWndSeg) *PVOID;

#else

typedef VOID *PVOID;

typedef struct _wnd WND;
typedef WND *PWND;

typedef DWORD	(FARPUBLIC *PLFN_WNDPROC)(PWND, WORD, WORD, DWORD);

typedef struct _rrc RRC;
typedef RRC *PRRC;
typedef RRC *NPRRC;

typedef struct _arc ARC;
typedef ARC *PARC;
typedef ARC *NPARC;

#endif



#ifdef BLADE

#ifdef BROADSWORD

typedef struct _wnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
	PLFN_WNDPROC pfnWndProc;
	PWND pwndParent;
	PWND pwndSibling;
	PWND pwndChild;
	WORD	pcls;
	BYTE	axCursor;
	BYTE	ayCursor;
	};

#else

typedef struct _wnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifndef REVIEW	/* size issue */
	RRC	rrcInvalid;			/* overlap update info */
#endif
	PLFN_WNDPROC pfnWndProc;		/* Medium Model */
	PWND pwndParent;
	PWND pwndSibling;
	PWND pwndChild;
	BYTE	axCursor;
	BYTE	ayCursor;
	BYTE	wndBytes;
#ifdef MLE
	WORD	rgwExtra[cwExtraWnd];
	BYTE	Pad;
#endif
	};

#endif //BROADSWORD


#else	//BLADE

typedef struct _wnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
#ifdef BLADE
	WORD	wExtStyle;
#endif
#ifdef PROJECT_PWB
	WORD	wExtStyle;
#endif
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
	};

#endif //BLADE



#ifndef BROADSWORD

typedef struct wndClass
{
  struct wndClass *next;
  WORD wStyle;
  WORD wExtStyle;
  PLFN_WNDPROC pfnWndProc;
  WORD  cbClsExtra;
  WORD  cbWndExtra;
  WORD  wClass;
} WNDCLASS;

typedef WNDCLASS *PWNDCLASS;

#else

typedef struct tagWNDCLASS
  {
    WORD				style;
	 PLFN_WNDPROC 	lpfnWndProc;
    int				cbClsExtra;
    int				cbWndExtra;
    WORD				hInstance;
    WORD				hIcon;
    WORD				hCursor;
    WORD				hbrBackground;
    WORD				lpszMenuName;
    WORD				lpszClassName;
  } WNDCLASS;

typedef WNDCLASS	    *PWNDCLASS;
typedef WNDCLASS NEAR *NPWNDCLASS;
typedef WNDCLASS FAR	 *LPWNDCLASS;


// Window internal class structure 

typedef struct tagCLS
  {
    // NOTE: The order of the following fields is assumed. 
    struct tagCLS   *pclsNext;
    int					cWndReferenceCount;   // The number of windows registered  with this class 
    WORD					style;
	 PLFN_WNDPROC		lpfnWndProc;
    int					cbClsExtra;
    int					cbWndExtra;
    WORD					hInstance;
    WORD					hIcon;
    WORD					hCursor;
    WORD					hbrBackground;
    WORD					lpszMenuName;
    WORD					lpszClassName;
  } CLS;

typedef CLS *PCLS;
typedef CLS far *LPCLS;
typedef PCLS  *PPCLS;

#endif

typedef BOOL	(FARPUBLIC *PWFN_ENUM)(WORD, WORD );


#ifndef	REVIEW	/* size issue */ /* for wnd static window construction macros */
#define	rrcInvalidStd	{0, 0, 0, 0},
#else
#define	rrcInvalidStd
#endif

// typedef WND *PWND;
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
	WORD	colHot;					// Hot Spot for			     
	WORD	rowHot;					//   graphics mouse cursor.	     
	WORD	rgwAndMaskGfx[16];	// Bit map masks for		     
	WORD	rgwXorMaskGfx[16];	//   graphics mouse cursor.	     
	WORD	wAndMaskText;			// Character and Attribute masks for 
	WORD	wXorMaskText;			//   text mouse cursor.		     
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

#define	MK_LBUTTON			0x0001
#define	MK_RBUTTON			0x0002
#define	MK_SHIFT				0x0004
#define	MK_CONTROL			0x0008
#define	MK_MBUTTON			0x0010
#define	MK_NONCLIENT		0x0060	/* either X or Y outside */
#define	MK_NONCLIENT_X		0x0020
#define	MK_NONCLIENT_Y		0x0040
#define	MK_ALT				0x8000
#define	MK_MENU				0x8000

#ifndef	NOPROCS

BOOL		FARPUBLIC FEnableMouse(BOOL);
VOID		FARPUBLIC SetMouseCursor(MCB FAR *);		/*OPTIONAL*/
VOID		FARPUBLIC MouseConditionalOff( MCOB *);	/*OPTIONAL*/
VOID		FARPUBLIC SetMousePos(WORD, WORD);			/*OPTIONAL*/
VOID		FARPUBLIC SetMouseDoubleSpeed(WORD);		/*OPTIONAL*/
BOOL		FARPUBLIC SwapMouseButton(BOOL);			 	/*OPTIONAL*/
WORD		FARPUBLIC CbSizeMouseState(void);			/*OPTIONAL*/
VOID		FARPUBLIC SaveMouseState(BYTE FAR *);		/*OPTIONAL*/
VOID		FARPUBLIC RestoreMouseState(BYTE FAR *);	/*OPTIONAL*/

#endif	/* !NOPROCS */


#ifdef COW
#ifdef BLADE
extern AX	PASCAL axMouse;
extern AY	PASCAL ayMouse;
#endif
#endif




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
#define	abAbortRepaint	3

extern BOOL PASCAL abAbort;							/* normal Abort */
extern BOOL PASCAL fPollKeyboard;					/* Poll the keyboard ? */
extern BYTE PASCAL fKeyIsUp, PASCAL fKeyWasUp;	/* Key transitions */
extern WORD PASCAL wRateKeyRepeat;					/* repeat rate */

#ifndef	NOPROCS

#ifdef BLADE
BOOL		FARPUBLIC IsAltKeyDown(VOID);					/*OPTIONAL*/
#endif

DWORD	FARPUBLIC EnableKeyboard(BOOL);
VOID	FARPUBLIC PollKeyboard(void);
VOID	FARPUBLIC PollPhysicalKeyboard(void);
VOID	FARPUBLIC SetShiftKk(WORD);
VOID	FARPUBLIC DisableExtendedKeyboard(void);
VOID	FARPUBLIC SetTsrProtocol(WORD, BYTE, BYTE);

#endif	// !NOPROCS 


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
	WORD	coImbue1;		/* default extra color */
	WORD	sig;			/* signature (cwdrv.doc has sigs) */
	WORD	coGridAndShade;		/* KANJI: color for grid & shade */
	WORD	mppHoriz;		/* KANJI: horizontal mickey ratio */
	WORD	mppVert;		/* KANJI: vertical mickey ratio */
	} CIB;

typedef	struct _cdi
	{
	/* CSD Default Information */
	WORD	cwcib;			/* length of rgw */
	CIB	cib;			/* info words */
	} CDI;


/*****************************************************************************/
/* CSD signatures */

#define	sigStandard	0x0000
#define	sigNEC98		0x9800

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

/* extra ffont (high 16-bit) for inscr.inc */
#define	fxfontVgrid		0x000F
#define	fxfontHgrid		0x00F0
#define	fxfontShadeType		0x0300
#define	fxfontDoubleWidth	0x0400
#define	fxfontDoubleHeight	0x0800
#define	fxfontDoubleRight	0x1000
#define	fxfontDoubleLower	0x2000
#define	fxfontDwLeft		0x0400
#define	fxfontDwRight		0x1400
#define	fxfontDhUpper		0x0800
#define	fxfontDhLower		0x2800
#define	fxfontReserved		0xC000

/* CSD flags */
#define	CSD_FORCERESET	0x0001	/* force the CSD to do a BIOS reset */
#define	CSD_SNOW	0x0002	/* use snow protection */
#define	CSD_NOSETMOUSE	0x0004	/* this hardware has an obscure mouse */

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
#define  indtKeytop		12
#define  indtFileTransfer		13			/* .FTD EBU Works */
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
VOID	FARPUBLIC SetCoBackPriority(BYTE FAR *, WORD);	/*OPTIONAL*/
#else /*!KANJI*/
VOID	FARPUBLIC GetCharMap(INFT *, BYTE, BYTE *);
#endif /*KANJI*/
#endif	/* !NOPROCS */


#if !defined(ADM)
	WORD	FARPUBLIC MessageBox(char *, char *, char *, WORD);
	WORD	FARPUBLIC MessageBoxAxAy(char *, char *, char *, WORD, AX, AY);
	VOID	FARPUBLIC SetDialogCaption(HANDLE, char *);	/*OPTIONAL*/
	BOOL	FARPUBLIC EndDlgTmc(WORD);				/*OPTIONAL*/
	BOOL	FARPUBLIC PassMsgToDialogProc ( WORD, WORD, WORD, WORD, WORD );	/*OPTIONAL*/
#else
	VOID	FARPUBLIC SetDialogCaption(HANDLE, char *);	/*OPTIONAL*/
	WORD	FARPUBLIC GetDlgItemText	(PVOID, char *, WORD);	
	VOID	FARPUBLIC SetDlgItemText	(PVOID, char *, BOOL);	
#endif

VOID	FARPUBLIC HiliteDialogAccel(void);	/*OPTIONAL*/

#define	dlmReturn	0x7fff	// for PWB guys to process return key events.

/* Message Box Definitions */

#ifndef NOMB

#define	IDDEFAULT	10
#define	IDOK	  		1
#define	IDCANCEL 	2
#define	IDABORT  	3
#define	IDRETRY  	4
#define	IDIGNORE 	5
#define	IDYES	  		6
#define	IDNO	  		7

#ifdef	HELP_BUTTON
#define	IDHELP		8
#endif

#define	MB_OK					1
#define	MB_YESNOCANCEL		2
#define	MB_RETRYCANCEL		3
#define	MB_OKCANCEL			4
#define	MB_ABORT				5
#define	MB_YESNO				6
#define	MB_RETRY				7
#define	MB_TYPE			0x0f		// message type 
#define	MB_BEEP			0x10
#define	MB_CAPTION		0x20		// 1st param is caption 
#define	MB_BUTTON		0x0300	// button mask 
#define	MB_DEFBUTTON1	0x0000	// initial button to get focus 
#define	MB_DEFBUTTON2	0x0100
#define	MB_DEFBUTTON3	0x0200

#ifdef	HELP_BUTTON
#define	MB_DEFBUTTON4	0x0300
#define	MB_NOHELP		0x8000
#endif

#endif //!NOMB

// for Special MessageBox 
extern BYTE FAR * PASCAL lpbWorkTemp;	// App should never use directly 

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
// extern PWND_DLG	pwndDlg;		/* Again, app should never use */

#define PwndDlgCur()		pwndDlg

#if defined(ADM)

#ifdef DEBUG
	VOID FARPUBLIC EndDialog		( PVOID, WORD);	/*OPTIONAL*/
#else
	#define EndDialog(pwnd, wParam )  ((PWND_DLG)pwnd)->wParamEnd = wParam;
#endif

WORD FARPUBLIC DialogBox      ( VOID *, PLFN_WNDPROC );		/*OPTIONAL*/
PWND FARPUBLIC NextChild	( PVOID, PVOID);
PWND FARPUBLIC PrevChild	( PVOID, PVOID );

PWND FARPUBLIC GetNextDlgGroupItem ( PVOID, PVOID, BOOL );

// #define WM_DLGUSER  20

// ADM defines
BOOL FARPUBLIC CreateChildren ( PVOID, VOID *, WORD );		/*OPTIONAL*/
PWND FARPUBLIC WinLoadDlg     ( VOID *, PLFN_WNDPROC );		/*OPTIONAL*/
WORD FARPUBLIC WinProcessDlg  ( PVOID, PLFN );					/*OPTIONAL*/
PWND FARPUBLIC GetDlgItem     ( PVOID, WORD );					/*OPTIONAL*/



#define LB_FILE TRUE
#define LB_DIR  FALSE


PWND FARPUBLIC CreateDialog	( VOID *, PWND, PLFN_WNDPROC );		/*OPTIONAL*/

VOID FARPUBLIC WinDlgDirList	( PVOID, char *, WORD, WORD, WORD, BOOL );  /*OPTIONAL*/

VOID FARPUBLIC DrawDlg			( PVOID, DWORD);											  /*OPTIONAL*/

DWORD FARPUBLIC ADMDialogWndProc ( PVOID,WORD, WORD, DWORD );				  /*OPTIONAL*/
DWORD FARPUBLIC ADMMboxWndProc	( PVOID,WORD, WORD, DWORD );				  /*OPTIONAL*/

WORD FARPUBLIC MessageBox     ( VOID *, VOID *, VOID *, WORD );			  /*OPTIONAL*/
WORD FARPUBLIC MessageBoxAxAy ( VOID *, VOID *, VOID *, WORD, WORD, WORD ); /*OPTIONAL*/

PWND FARPUBLIC CreateMsgBtn ( char *,WORD,WORD,WORD,WORD,WORD,WORD,PVOID,WORD );


#ifdef WIN_BTN

VOID FARPUBLIC CheckRadioButton ( PVOID, WORD, WORD, WORD );				 /*OPTIONAL*/

#define CheckDlgRadioButton(hwnd,idFirstButton,idLastButton,idCheckButton) CheckRadioButton(hwnd,idFirstButton,idLastButton,idCheckButton)

#else

VOID FARPUBLIC CheckDlgRadioButton ( PVOID, WORD, WORD, WORD );				 /*OPTIONAL*/

#endif

#endif //ADM


// Control Procedure 
typedef WORD	(FARPUBLIC *PWFN_CTL)(WORD, char *, WORD, WORD, WORD, WORD);


// BLADE enhanced version.

#ifdef BLADE

#ifdef BROADSWORD

#ifdef FULL_EDIT

#ifdef BASED
	typedef struct _winwnded _based(pWndSeg)  WND_WINEDIT;
#else
	typedef struct _winwnded WND_WINEDIT;
#endif

typedef WND_WINEDIT *PWND_WINEDIT;


typedef struct _winwnded
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PED ped;
	};

#define GetEditPed(pwnd) (((PWND_WINEDIT)pwnd)->ped)
#define SetEditPed(pwnd, ped) ( ((PWND_WINEDIT)pwnd)->ped = ped)

#endif

#endif


#ifdef BASED
	typedef struct _wndmle _based(pWndSeg) WND_MLE;
#else
	typedef struct _wndmle WND_MLE;
#endif

typedef WND_MLE *PWND_MLE;

typedef struct _wndmle
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;
#endif
	WORD	padding;
	WORD  pEF_Structure;
	};

#ifdef BASED
	typedef struct _wnddesktop _based(pWndSeg) WND_DESKTOP;
#else
	typedef struct _wnddesktop WND_DESKTOP;
#endif

typedef WND_DESKTOP *PWND_DESKTOP;

typedef struct _wnddesktop
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	BYTE  bFill;
	WORD  isaDesktop;
	};



#ifdef BASED
	typedef struct _wndgeneric _based(pWndSeg) WND_GEN;
#else
	typedef struct _wndgeneric WND_GEN;
#endif

typedef WND_GEN *PWND_GEN;

typedef struct _wndgeneric
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;			//	 6 bytes
	};



#ifdef BASED
	typedef struct _wndgenpic _based(pWndSeg) WND_GENPIC;
#else
	typedef struct _wndgeneric WND_GENPIC;
#endif

typedef WND_GENPIC *PWND_GENPIC;

typedef struct _wndgenpic
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;			//	 6 bytes
	PWFN_CTL pfnGenProc;
 	WORD wParam;				//	 6 bytes
	};

// General window stuff

// #define	PwfnCtlGeneral(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))
#define	PwfnCtlGeneral(pwnd)	((PWND_GENPIC)pwnd)->pfnGenProc
#define	WParamGeneral(pwnd)	((PWND_GENPIC)pwnd)->wParam


#define SetAccelerator(pwnd, Position, szText) \
	 ((PWND_GEN)(pwnd))->aclDialog = (((BYTE)((Position) - 1)) << 8) + \
					    *((unsigned char *)(szText) + ((Position) - 1))

#ifdef BASED
	typedef struct _wndgroup _based(pWndSeg) WND_GROUP;
#else
	typedef struct _wndgroup WND_GROUP;
#endif

typedef WND_GROUP *PWND_GROUP;

typedef struct _wndgroup
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;
#endif
	WORD aclDialog;
 	char *szDialog;
	PWND pwndAccel;	
	};


#ifdef BASED
	typedef struct _wndstatic _based(pWndSeg) WND_STATIC;
#else
	typedef struct _wndstatic WND_STATIC;
#endif

typedef WND_STATIC *PWND_STATIC;

typedef struct _wndstatic
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;						// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;
	PWND pwndAccel;	
	};

#ifdef BASED
	typedef struct _dlgwnd _based(pWndSeg)  WND_DLG;
#else
	typedef struct _dlgwnd WND_DLG;
#endif

typedef WND_DLG *PWND_DLG;

typedef struct _dlgwnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD wParamEnd;
	VOID *pmbsDialog;
	PLFN pfnUserProc;			// 12 bytes
 	WORD hidHelp;
	};

// so that rspAppIdle can determine where the current msg box dialog is 
extern PWND_DLG	pwndDlg;		// Again, app should never use 


typedef struct _drop DROP;
typedef DROP *PDROP;

typedef struct _Drop {
		WORD Dummy;
		WORD wDropFlags;
		PWND pwndDropListBox;
		PWND pwndDropButton;
		PWND pwndDropEdit;
		PLFN_WNDPROC pfnDropListBoxProc;
		PLFN_WNDPROC pfnDropEditProc;
	};


#ifdef BASED
	typedef struct _wnddrop _based(pWndSeg)  WND_DROP;
#else
	typedef struct _wnddrop WND_DROP;
#endif

typedef WND_DROP *PWND_DROP;

typedef struct _wnddrop	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD wDropState;
	WORD wDropFlags;			// TRUE ( 1 ) = Combo, FALSE ( 0 ) = Plain
	PWND pwndDropListBox;
	PWND pwndDropButton;
	PWND pwndDropEdit;
	PLFN_WNDPROC pfnDropListBoxProc;
	PLFN_WNDPROC pfnDropEditProc;		// 20 bytes
	};

#define PwndDropListBox(pwnd) ( (PWND_LIST)( (PWND_DROP)pwnd )->pwndDropListBox ) 
#define PwndDropButton(pwnd) 	( (PWND_BTN)( (PWND_DROP)pwnd )->pwndDropButton )
#define PwndDropEdit(pwnd)		( (PWND_EDIT)( (PWND_DROP)pwnd )->pwndDropEdit)
#define PfnDropListBoxProc(pwnd) ( ( (PWND_DROP)pwnd )->pfnDropListBoxProc)
#define PfnDropEditProc(pwnd)		( ( (PWND_DROP)pwnd )->pfnDropEditProc)


#ifdef BASED
	typedef struct _wnddroplist _based(pWndSeg)  WND_DROP_LIST;
#else
	typedef struct _wnddroplist WND_DROP_LIST;
#endif

typedef WND_DROP_LIST *PWND_DROP_LIST;

typedef struct _wnddroplist	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PWFN_CTL pfnLBFtn;
	WORD wParam;
	WORD wUnknown;
	WORD iszTopLb; 			// first item in Display 
	WORD cszLb;					// # of strings in list in list 
	WORD iszCurLb;				// currently selected item 
	WORD hmemMpiszoffLb;		// array of offsets in string buffer
	WORD hmemGpszLb;			// string buffer pool 
	WORD offLb; 				// next string buffer pointer 
	WORD offMaxLb; 			// size of string buffer 
	WORD iszMacLb; 			// max isz before index buf is grown
	WORD fSelectedLb;			// do we have a selection ? 
	WORD isaColor;				// colour of the listbox 
	WORD isaHiliteColor;		// colour of the listbox hilite
	WORD ctickRepLb;			// scrolling rate 
	WORD drxItemLb;			// width of a single item (add 1 for space) 
	WORD citemWidthLb;		// max # of items wide 	  // 36 bytes
	PWND_DROP pwndDrop;
	};



#ifdef BASED
	typedef struct _wnddlgdir _based(pWndSeg)  WND_DLGDIR;
#else
	typedef struct _wnddlgdir WND_DLGDIR;
#endif

typedef WND_DLGDIR *PWND_DLGDIR;

typedef struct _wnddlgdir	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PWND	pwndDirDrivesBox;
	PWND	pwndFilesBox;
	PWND	pwndDirEdit;
	PWND  pwndDirStatic;
	};


#define PwndDlgDirEdit(pwnd)	( ( (PWND_DLGDIR)pwnd )->pwndDirEdit)
#define PwndDirStatic(pwnd)	( ( (PWND_DLGDIR)pwnd )->pwndDirStatic)


typedef struct _Scroll SCROLL;
typedef SCROLL *PSCROLL;

typedef struct _Scroll {
	WORD ctickRepSb;	 	// # of ticks to repeat
	short ptCurSb; 			// current position on scroll line
	short ptMinSb; 			// minimum position on scroll line
	short ptMaxSb; 			// end position on scroll line 
	BYTE ptElevatorSb;		// elevator position
	BYTE dummy; 				// word alignment
	};


#ifdef BASED
	typedef struct _wndscroll _based(pWndSeg)  WND_SCROLL;
#else
	typedef struct _wndscroll WND_SCROLL;
#endif

typedef WND_SCROLL *PWND_SCROLL;

typedef struct _wndscroll
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;			
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD ctickRepSb;	 		// # of ticks to repeat
	short ptCurSb; 				// current position on scroll line
	short ptMinSb; 				// minimum position on scroll line
	short ptMaxSb; 				// end position on scroll line 
	short ptElevatorSb;		// elevator position (lower byte)	// 10 bytes
	};

typedef struct _List LISTBOX;
typedef LISTBOX *PLISTBOX;


typedef struct _List {
	PWFN_CTL pfnLBFtn;
	WORD wParam;
	WORD wUnknown;
	WORD iszTopLb; 			// first item in Display 
	WORD cszLb;					// # of strings in list in list 
	WORD iszCurLb;				// currently selected item 
	WORD hmemMpiszoffLb;		// array of offsets in string buffer
	WORD hmemGpszLb;			// string buffer pool 
	WORD offLb; 				// next string buffer pointer 
	WORD offMaxLb; 			// size of string buffer 
	WORD iszMacLb;				// max isz before index buf is grown
	WORD fSelectedLb;			// do we have a selection ? 
	WORD isaColor;				//	colour of the listbox 
	WORD isaHiliteColor;		// colour of the listbox hilite
	WORD ctickRepLb;			// scrolling rate 
	WORD drxItemLb;			// width of a single item (add 1 for space) 
	WORD citemWidthLb;		// max # of items wide 
	};


#ifdef BASED
	typedef struct _wndlist _based(pWndSeg)  WND_LIST;
#else
	typedef struct _wndlist WND_LIST;
#endif

typedef WND_LIST *PWND_LIST;

typedef struct _wndlist	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	PWFN_CTL pfnLBFtn;
	WORD wParam;
	WORD wUnknown;
	WORD iszTopLb; 			// first item in Display 
	WORD cszLb;					// # of strings in list in list 
	WORD iszCurLb;				// currently selected item 
	WORD hmemMpiszoffLb;		// array of offsets in string buffer
	WORD hmemGpszLb;			// string buffer pool 
	WORD offLb; 				// next string buffer pointer 
	WORD offMaxLb; 			// size of string buffer 
	WORD iszMacLb; 			// max isz before index buf is grown
	WORD fSelectedLb;			// do we have a selection ? 
	WORD isaColor;				// colour of the listbox 
	WORD isaHiliteColor;		// colour of the listbox hilite
	WORD ctickRepLb;			// scrolling rate 
	WORD drxItemLb;			// width of a single item (add 1 for space) 
	WORD citemWidthLb;		// max # of items wide 	  // 36 bytes
	};


#define	PwfnCtlLb(pwnd)	(((PWND_LIST)pwnd)->pfnLBFtn)
#define	WParamLb(pwnd)		(((PWND_LIST)pwnd)->wParam)

// special listbox window specific isa's 

#define IsaListbox(pwnd)			(((PWND_LIST)pwnd)->isaColor)
#define IsaHiliteListbox(pwnd)	(((PWND_LIST)pwnd)->isaHiliteColor)

typedef struct _Edit EDIT;
typedef EDIT *PEDIT;

typedef struct _Edit {
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;
	WORD isaEb;					// color 
	WORD isaSelEb;				// selected color 
	BYTE chFillDialog;			// fill char for trailing spaces 
	WORD ichMacEb;				// last character in edit buffer 
	WORD ichLeftEb;				// leftmost character displayed 
	WORD ichCursorEb;			// current cursor position, to the left of  insertion point 
	WORD ichSelEb;				// start of selection 
	WORD fNoBracketEb;			// don't show brackets ?? 
	WORD wEb;						// random flags 
	WORD cchMaxEb;				// for fixed length edit items 
	WORD isaDisabledEb;			// disabled color 
	char *szWildCardEb;			// auxilary edit field 
	BOOL fCWAllocated;
	BOOL Dummy;
};

#ifdef BASED
	typedef struct _wnded _based(pWndSeg)  WND_EDIT;
#else
	typedef struct _wnded WND_EDIT;
#endif

typedef WND_EDIT *PWND_EDIT;

typedef struct _wnded
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	WORD cchDialog;
	WORD isaEb;					// color 
	WORD isaSelEb;				// selected color 
	BYTE chFillDialog;		// fill char for trailing spaces 
	WORD ichMacEb;				// last character in edit buffer 
	WORD ichLeftEb;			// leftmost character displayed 
	WORD ichCursorEb;			// current cursor position, to the left of  insertion point 
	WORD ichSelEb;				// start of selection 
	WORD fNoBracketEb;		// don't show brackets ?? 
	WORD wEb;					// random flags 
	WORD cchMaxEb;				// for fixed length edit items 
	WORD isaDisabledEb;		// disabled color 
	char *szWildCardEb;		// auxilary edit field 
	BOOL fCWAllocated;
	BOOL Dummy;					// 34 bytes
	};


#define ShowEditBracket(pwnd, fNoBracket ) (((PWND_EDIT)pwnd)->fNoBracketEb = !(fNoBracket)	)
#define SetEditFillCharacter(pwnd, ch)		 (((PWND_EDIT)pwnd)->chFillDialog = (BYTE)(ch) )
#define SetEditDisabledIsa(pwnd, isa)		 (((PWND_EDIT)pwnd)->isaDisabledEb = (isa) )
#define SetEditSelectedIsa(pwnd, isa)		 (((PWND_EDIT)pwnd)->isaSelEb = (isa) )
#define SetEditIsa(pwnd, isa)					 (((PWND_EDIT)pwnd)->isaEb = (isa) )

// special edit window specific isa's 
#define IsaEdit(pwnd)			(((PWND_EDIT)pwnd)->isaEb)
#define IsaSelEdit(pwnd)		(((PWND_EDIT)pwnd)->isaSelEb )
#define IsaDisabledEdit(pwnd)	(((PWND_EDIT)pwnd)->isaDisabledEb)

#ifdef BASED
	typedef struct _btnwnd  _based(pWndSeg) WND_BTN;
#else
	typedef struct _btnwnd WND_BTN;
#endif

typedef WND_BTN *PWND_BTN;

typedef struct _btnwnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;			// 31 bytes
#endif
	WORD aclDialog;
 	char *szDialog;	
 	BYTE cchDialog;
	BYTE wButton;
#ifndef WIN_BTN
	PWND_BTN pwndButtonNext;		//	 8 bytes
#endif
	};


#ifndef WIN_BTN
// walking the list of radio buttons 
#define	PwndButtonNext(pwnd) ((PWND_BTN) (pwnd)->pwndButtonNext)

// is it a (radio) button? 
#define	FPwndIsButton(pwnd) (((pwnd)->style & WS_TYPE) == WS_BUTTON)
#define	FButtonIsRadio(pwnd) (((pwnd)->style & WS_SUBSTYLE) == BS_RADIOBUTTON)

// bits :
//  0..1 '=> bst (button state)
//  2 => fButtonDown
//  3 => first in group
//
// button states 


#ifdef BLADE

#define bstOff		(BYTE)0
#define bstOn		(BYTE)1
#define bstGreyed	(BYTE)2 
#define bstMax		(BYTE)3


// Macros for bit accesses 


#define BstOfWnd(pwnd) ((pwnd)->wButton & 3)

#define SetWndBst(pwnd, bst) (pwnd)->wButton = ((pwnd)->wButton & ~((BYTE)3)) | (bst) 

#define FButtonDown(pwnd) ((pwnd)->wButton & 4)

#define SetFButtonDown(pwnd, fDown) \
	{if (fDown) (pwnd)->wButton |= (BYTE)4; \
	  else (pwnd)->wButton &= ~(BYTE)4;}

#define FFirstButton(pwnd) ((pwnd)->wButton & 8)

#define SetFFirstButton(pwnd) (pwnd)->wButton = ((pwnd)->wButton |= (BYTE)8)

#define SetNextGroupButton(pwnd, pwndNext )  (pwnd)->pwndButtonNext =(pwndNext)


#else

#define bstOff		0
#define bstOn		1
#define bstGreyed	2 
#define bstMax		3


// Macros for bit accesses 


#define BstOfWnd(pwnd) ((pwnd)->wButton & 3)

#define SetWndBst(pwnd, bst) (pwnd)->wButton = ((pwnd)->wButton & ~(3)) | (bst) 

#define FButtonDown(pwnd) ((pwnd)->wButton & 4)

#define SetFButtonDown(pwnd, fDown) \
	{if (fDown) (pwnd)->wButton |= 4; \
	  else (pwnd)->wButton &= ~4;}

#define FFirstButton(pwnd) ((pwnd)->wButton & 8)

#define SetFFirstButton(pwnd) (pwnd)->wButton = ((pwnd)->wButton |= 8)

#define SetNextGroupButton(pwnd, pwndNext )  (pwnd)->pwndButtonNext =(pwndNext)

#endif //BLADE


#endif //WIN_BTN


#ifdef BASED
	typedef struct _framewnd _based(pWndSeg)  WND_FRAME;
#else
	typedef struct _framewnd WND_FRAME;
#endif

typedef WND_FRAME *PWND_FRAME;

typedef struct _framewnd
	{
	WORD	id;
	BITS	style:14;
	BITS	fCursorOn:1;
	BITS	fEnabled:1;
	WORD	wExtStyle;
	ARC	arcWindow;
	ARC	arcClipping;
#ifdef BROADSWORD
	ARC	rrcInvalid;
	BYTE  hbrDraw;			  	// Color for drawing.
	BYTE  hbrBackGround;		//	Color for background.
#else
	RRC	rrcInvalid;
#endif
	PLFN_WNDPROC pfnWndProc;
	PWND  pwndParent;
	PWND  pwndSibling;
	PWND  pwndChild;
#ifdef BROADSWORD
	WORD	pcls;
#endif
	BYTE	axCursor;
	BYTE	ayCursor;
#ifndef BROADSWORD
	BYTE	wndBytes;				// 31 bytes
#endif
	BOOL  fWinActive;
	char *szFrameText;
	VOID **hFrameMenu;
	WORD 	 axyLastPos;
	WORD 	 rxyLastPos;
	BOOL	 fZoomFrame;
	ARC	 arcMenuArc;				//	16 bytes
	WORD	 wFrameHeight;
	PLFN_WNDPROC pfnClientWndProc;
	};

#else	 // Non-Blade stuff follows

typedef PWND PWND_MLE;
typedef PWND PWND_SCROLL;
typedef PWND PWND_EDIT;
typedef PWND PWND_BTN;
typedef PWND PWND_DLG;
typedef PWND PWND_GEN;
typedef PWND PWND_STATIC;
typedef PWND PWND_LIST;
typedef PWND PWND_DROP_LIST;
typedef PWND PWND_DROP;
typedef PWND PWND_FRAME;
typedef PWND PWND_DESKTOP;
typedef PWND PWND_GROUP;

#define	GetWindowWord(pwnd, iw) 	((pwnd)->rgwExtra[(iw)])
#define	SetWindowWord(pwnd, iw, w) {(pwnd)->rgwExtra[(iw)] = (w);}


// special edit window specific isa's 

#define IsaEdit(pwnd)			((pwnd)->rgwExtra[3])
#define IsaSelEdit(pwnd)		((pwnd)->rgwExtra[4])
#define IsaDisabledEdit(pwnd)	((pwnd)->rgwExtra[13])


// Common portion of all dialog windows 

#define	cwExtraMin	1		// at least 1 field for all items 
									// not used for dialog box 

#define	aclDialog	rgwExtra[0]	// dialog accelerator 

// Text info for : edit, static and button controls 

#define cwExtraText	(cwExtraMin+2)	// 2 more for text controls 
#define szDialog	rgwExtra[1]			// also Dialog Box caption 
#define cchDialog	rgwExtra[2]			// edit, static & buttons 

// flag indicating whether EndDialog has been
// called.  Used to prevent multiple calls
// to PostQuitMessage 

#define wParamEnd		rgwExtra[2]


// sizes of rgwExtra for the various controls 

#define cwExtraDialog	(cwExtraMin+2)		// szDialog + wParamEnd 

#define cwExtraSdmDlg	(cwExtraDialog+((sizeof(SDS)+1)/2))	// ... + sds 

#define cwExtraMsgBox	(cwExtraDialog+1) // ... + pmbs 
#define cwExtraStatic	(cwExtraText)		// simple text 
#define cwExtraButton	(cwExtraText+2)	// text + button state + next radio
#define cwExtraGroup    (cwExtraText+2)	// text 


#define cwExtraEdit		(cwExtraText+11)	// see sedit.h for details 
#define cwExtraEditAux	(cwExtraEdit+1)	
#define cwExtraGeneral	(cwExtraMin+3)		// see general.h for details 

#ifndef LISTBOX_HORIZ
	#define cwExtraListBox	(cwExtraMin+15)	// see _listbox.h for details 
#else
	#define cwExtraListBox	(cwExtraMin+17)	// see _listbox.h for details 
#endif


#define cwExtraDropDown		(cwExtraMin+9)		// 10
#define cwExtraDropHolder 	(cwExtraMin+1)		// 2
#define cwExtraDropButton 	(cwExtraMin)		// 1


#define wButton 	rgwExtra[cwExtraText]

// flag word for button control 

#define	pwndButtonNext	rgwExtra[cwExtraText+1]	 // next radio button 

//#if cwExtraButton != cwExtraText+2
//.....
//#endif

// walking the list of radio buttons 
#define	PwndButtonNext(pwnd) ((PWND_BTN) (pwnd)->pwndButtonNext)

// is it a (radio) button? 
#define	FPwndIsButton(pwnd) (((pwnd)->style & WS_TYPE) == WS_BUTTON)
#define	FButtonIsRadio(pwnd) (((pwnd)->style & WS_SUBSTYLE) == BS_RADIOBUTTON)

// bits :
//  0..1 '=> bst (button state)
//  2 => fButtonDown
//  3 => first in group
//
// button states 



#define bstOff		0
#define bstOn		1
#define bstGreyed	2 
#define bstMax		3


// Macros for bit accesses 


#define BstOfWnd(pwnd) ((pwnd)->wButton & 3)

#define SetWndBst(pwnd, bst) (pwnd)->wButton = ((pwnd)->wButton & ~(3)) | (bst) 

#define FButtonDown(pwnd) ((pwnd)->wButton & 4)

#define SetFButtonDown(pwnd, fDown) \
	{if (fDown) (pwnd)->wButton |= 4; \
	  else (pwnd)->wButton &= ~4;}

#define FFirstButton(pwnd) ((pwnd)->wButton & 8)

#define SetFFirstButton(pwnd) (pwnd)->wButton = ((pwnd)->wButton |= 8)

#define SetNextGroupButton(pwnd, pwndNext )  (pwnd)->pwndButtonNext =(pwndNext)



#define cwExtraFrame (cwExtraText + 4)	

#define fWinActive   rgwExtra[1]
#define szFrameText  rgwExtra[2]
#define hFrameMenu   rgwExtra[3]

#define axyLastPos	rgwExtra[4]
#define rxyLastPos	rgwExtra[5]
#define fZoomFrame	rgwExtra[6]
#define arcMenuArc   rgwExtra[7]

// General window stuff

#define	PwfnCtlGeneral(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))
#define	WParamGeneral(pwnd)	(pwnd->rgwExtra[cwExtraMin+2])

//#if cwExtraGeneral != cwExtraMin+3
//.....
//#endif

// General window stuff


//listbox stuff

// customization 

// extra word usage 

// 0,1,2 are listbox function and wParam (for on-demand) 

#define iszTopLb 	rgwExtra[cwExtraMin+3]			// first item in Display 
#define cszLb		rgwExtra[cwExtraMin+4]			// # of strings in list in list 
#define iszCurLb	rgwExtra[cwExtraMin+5]			// currently selected item 
#define hmemMpiszoffLb	rgwExtra[cwExtraMin+6]	// array of offsets in string buffer
#define hmemGpszLb	rgwExtra[cwExtraMin+7]		// string buffer pool 
#define offLb 		rgwExtra[cwExtraMin+8]			// next string buffer pointer 
#define offMaxLb 	rgwExtra[cwExtraMin+9]			// size of string buffer 
#define iszMacLb 	rgwExtra[cwExtraMin+10]			// max isz before index buf is grown
#define fSelectedLb	rgwExtra[cwExtraMin+11]		// do we have a selection ? 
#define isaColor	rgwExtra[cwExtraMin+12]			// colour of the listbox 
#define isaHiliteColor	rgwExtra[cwExtraMin+13]	// colour of the listbox hilite
#define ctickRepLb	rgwExtra[cwExtraMin+14]		// scrolling rate 

#ifndef LISTBOX_HORIZ
	#define	cwExtraNeeded	(cwExtraMin+15)
#else
	// Horizontal scrolling extra info 

	#define	drxItemLb	rgwExtra[cwExtraMin+15]		// width of a single item (add 1 for space) 
	#define	citemWidthLb	rgwExtra[cwExtraMin+16]	// max # of items wide 
	#define	cwExtraNeeded	(cwExtraMin+17)
#endif

//#if cwExtraListBox != cwExtraNeeded
//.....
//#endif

#define	PwfnCtlLb(pwnd)	(*((PWFN_CTL *) &pwnd->rgwExtra[cwExtraMin]))
#define	WParamLb(pwnd)		(pwnd->rgwExtra[cwExtraMin+2])


// special listbox window specific isa's 

#define IsaListbox(pwnd)			((pwnd)->rgwExtra[13])
#define IsaHiliteListbox(pwnd)	((pwnd)->rgwExtra[14])

//listbox stuff

//Scrollbar defs

#define cwExtraScroll	5	// size of rgwExtra for scroll windows 

// scroll bar definitions 

#define ctickRepSb	rgwExtra[0]	// # of ticks to repeat 
#define ptCurSb		rgwExtra[1]	// current position on scroll line 
#define ptMinSb		rgwExtra[2]	// minimum position on scroll line 
#define ptMaxSb		rgwExtra[3]	// end position on scroll line 
#define ptElevatorSb	rgwExtra[4]	// elevator position (lower byte) 

//Scrollbar defs



//Editbox defs

// extra word usage (filled by SDM) 

#define	isaEb		rgwExtra[cwExtraText+0]			// color 
#define	isaSelEb	rgwExtra[cwExtraText+1]			// selected color 
#define chFillDialog	rgwExtra[cwExtraText+2]		// fill char for trailing spaces 
#define ichMacEb	rgwExtra[cwExtraText+3]			// last character in edit buffer 
#define ichLeftEb	rgwExtra[cwExtraText+4]			// leftmost character displayed 
#define ichCursorEb	rgwExtra[cwExtraText+5]		// current cursor position, to the left of
															   // insertion point 
#define ichSelEb	rgwExtra[cwExtraText+6]			// start of selection 
#define	fNoBracketEb	rgwExtra[cwExtraText+7]	// don't show brackets ?? 
#define wEb		rgwExtra[cwExtraText+8]				// random flags 
#define cchMaxEb	rgwExtra[cwExtraText+9]			// for fixed length edit items 
#define isaDisabledEb	rgwExtra[cwExtraText+10]// disabled color 
#define szWildCardEb	rgwExtra[cwExtraText+11]	// auxilary edit field 

//#if cwExtraEdit != cwExtraText+11
//.....
//#endif

//#if cwExtraEditAux != cwExtraText+12
//.....
//#endif

//Editbox defs


// Message box defs

// this structure is defined only so that C will allocate enough extra
// words in the msg box window structures put on the stack 

typedef struct _wndm
	{
	WND	wnd;
	WORD 	rgwExtraPlus[cwExtraMsgBox-1];
	} WNDM;

// message box state 
#define pmbsDialog		rgwExtra[cwExtraDialog]

// Message box defs


// Dropdown defs

// dropdown rgwExtra aliases 
#define wDropFlags      rgwExtra[cwExtraMin]
#define pwndDropListBox rgwExtra[cwExtraMin+1]
#define pwndDropButton  rgwExtra[cwExtraMin+2]
#define pwndDropEdit    rgwExtra[cwExtraMin+3]
#define pwndDropHolder  rgwExtra[cwExtraMin+4]
#define pfnDropListBoxProc	rgwExtra[cwExtraMin+5]	// actually two words 
#define pfnDropEditProc	rgwExtra[cwExtraMin+7]		// actually two words 


#define PwndDropListBox(pwnd)		(*(PWND*)&((pwnd)->pwndDropListBox))
#define PwndDropButton(pwnd)		(*(PWND*)&((pwnd)->pwndDropButton))
#define PwndDropEdit(pwnd)			(*(PWND*)&((pwnd)->pwndDropEdit))
#define PwndDropHolder(pwnd)		(*(PWND*)&((pwnd)->pwndDropHolder))
#define PfnDropListBoxProc(pwnd) (*(PLFN_WNDPROC*)&((pwnd)->pfnDropListBoxProc))
#define PfnDropEditProc(pwnd)		(*(PLFN_WNDPROC*)&((pwnd)->pfnDropEditProc))

// dropdown holding window rgwExtra aliases and macros 

#define pwndHolderDrop rgwExtra[cwExtraMin]

#define PwndHolderDrop(pwnd) (*(PWND*)&((pwnd)->pwndHolderDrop))

// Dropdown defs

extern PWND_DLG	pwndDlg;		// Again, app should never use 

#endif //	BLADE

extern PWND_DESKTOP PASCAL pwndDesktop;


typedef PWND HWND; 


#ifdef BLADE

typedef PWND HDC; 

#define HMENU void **

typedef struct _rect
	{
	BYTE	left;
	BYTE	top;
	BYTE	right;
	BYTE  bottom;
	}RECT;

typedef RECT *LPRECT;

typedef RRC PAINTSTRUCT;

typedef WORD HBRUSH;

#endif

#ifdef BROADSWORD

#include <winapi.h>

#endif //BROADSWORD

// #include "wndstruc.h"

#define HEAD	0
#define TAIL	1


#ifndef	NOPROCS

BOOL	FARPUBLIC InitDesktop	( VOID );
DWORD FARPUBLIC DesktopWndProc(PVOID, WORD, WORD, DWORD);

VOID FARPUBLIC RemoveChild		(PVOID);
VOID FARPUBLIC EnableWindow	(PVOID, BOOL);
VOID FARPUBLIC SetWindowStyle	(PVOID, WORD);
VOID FARPUBLIC SetWindowSize	(PVOID, BYTE, BYTE);
VOID FARPUBLIC DrawWindow		(PVOID);
VOID FARPUBLIC TextOut			(PVOID, RX, RY, char *, short, WORD);
VOID FARPUBLIC lpTextOut(PVOID, RX, RY, char far *, short, WORD); /*OPTIONAL*/
VOID FARPUBLIC DrawBorder2			(PVOID, BOX *, WORD, WORD, char *);
VOID FARPUBLIC DrawBorderAlign	(PVOID, BOX *, WORD, WORD, char *, BOOL);
VOID FARPUBLIC CharOut				(PVOID, RX, RY, ACHAR, WORD);
VOID FARPUBLIC CharOutBorder		(PVOID, RX, RY, ACHAR, WORD);
VOID FARPUBLIC BltRrc				(PVOID, RX, RY, BYTE, BYTE, RX, RY);
VOID FARPUBLIC BltRrcTop			(PVOID, RX, RY, BYTE, BYTE, RX, RY);	/*OPTIONAL*/


VOID FARPUBLIC EndPaint				(PVOID, VOID FAR *);
VOID FARPUBLIC DrawBox				(PVOID, NPRRC, BOX *, WORD);
VOID FARPUBLIC SaveRrc				(PVOID, NPRRC, BYTE FAR *);
VOID FARPUBLIC RestoreRrc			(PVOID, NPRRC, BYTE FAR *);
VOID FARPUBLIC EnableCursor		(PVOID, BOOL);
VOID FARPUBLIC MoveCursor			(PVOID, RX, RY);
VOID FARPUBLIC MoveWindow			(PVOID, AX, AY);
BOOL FARPUBLIC IntersectRect		(NPRRC, NPRRC, NPRRC);
VOID FARPUBLIC UnionRect			(NPRRC, NPRRC, NPRRC);
BOOL FARPUBLIC IsRectEmpty			(NPRRC);
VOID FARPUBLIC SetRect				(NPRRC, RX, RY, RX, RY);
BOOL FARPUBLIC PtInRect				(NPRRC, RX, RY);
WORD FARPUBLIC CwSizeRrc			(NPRRC);
VOID FARPUBLIC ShadowArc			(NPARC);
VOID FARPUBLIC EnableOverlap		(BOOL, VOID *, WORD); 
VOID FARPUBLIC ReplaceChild		(PVOID, PVOID);					/*OPTIONAL*/
VOID FARPUBLIC DrawOverlapShadow(PVOID);								/*OPTIONAL*/
VOID FARPUBLIC RedrawDamagedRegions	(void);						/*OPTIONAL*/
BOOL FARPUBLIC FMoveSizeWithKeyboard	(PVOID, BOOL);			 	/*OPTIONAL*/
/*	Listbox routines */
DWORD		FARPUBLIC ListBoxWndProc		(PVOID, WORD, WORD, DWORD);   /*OPTIONAL*/
VOID		FARPUBLIC InitListBox			(PVOID, PWFN);							/*OPTIONAL*/
VOID		FARPUBLIC InitListBoxOriented	(PVOID, PWFN, WORD *, WORD *);	/*OPTIONAL*/
VOID		FARPUBLIC GetListBoxOrientation(PVOID, WORD *, WORD *);			/*OPTIONAL*/
VOID		FARPUBLIC SetListBoxOrientation(PVOID, WORD *, WORD *);

/*	Dropdown Listbox routines */
DWORD		FARPUBLIC DropDownWndProc		(PVOID, WORD, WORD, DWORD);		/*OPTIONAL*/
DWORD		FARPUBLIC DropHolderWndProc	(PVOID, WORD, WORD, DWORD);		/*OPTIONAL*/
DWORD		FARPUBLIC DropListBoxWndProc	(PVOID, WORD, WORD, DWORD);		/*OPTIONAL*/
DWORD		FARPUBLIC DropEditWndProc		(PVOID, WORD, WORD, DWORD);		/*OPTIONAL*/
DWORD		FARPUBLIC DropButtonWndProc	(PVOID, WORD, WORD, DWORD);		/*OPTIONAL*/
VOID		FARPUBLIC AddDropWindow			(PVOID, PVOID, PVOID, PVOID, PVOID, PVOID, BOOL); /*OPTIONAL*/
VOID		FARPUBLIC RemoveDropWindow		(PVOID);									/*OPTIONAL*/

DWORD		FARPUBLIC ButtonWndProc			(PVOID, WORD, WORD, DWORD);   /*OPTIONAL*/
DWORD	   FARPUBLIC StaticWndProc			(PVOID, WORD, WORD, DWORD);   /*OPTIONAL*/

DWORD		FARPUBLIC ScrollBarWndProc(PVOID, WORD, WORD, DWORD);	    /*OPTIONAL*/
DWORD  	FARPUBLIC InternalEditWndProc(PVOID, WORD, WORD, DWORD);  /*OPTIONAL*/

BOOL FARPUBLIC RectEqual( NPRRC, NPRRC );  /*OPTIONAL*/
VOID FARPUBLIC EnumListBoxEntries ( PVOID, PWFN_ENUM );   /*OPTIONAL*/
VOID FARPUBLIC DrawTextJustified ( PVOID, char *, WORD, WORD, NPRRC );   /*OPTIONAL*/
VOID FARPUBLIC SetSeg ( VOID );	/*OPTIONAL*/
VOID FARPUBLIC DrawBoxText ( VOID *, AX, AX, AX );   /*OPTIONAL*/
VOID FARPUBLIC DrawBorderAlign3D ( PVOID, WORD, WORD, char *, BOOL, BOOL, WORD, WORD );	/*OPTIONAL*/
PWND FARPUBLIC TreeToTop ( PVOID );	  /*OPTIONAL*/

#ifdef BLADE

PWND 	FARPUBLIC CreateWindow ( WORD, char *,  WORD, WORD, WORD,WORD,WORD,WORD,	PVOID, HMENU, LPSTR, WORD );  /*OPTIONAL*/
BOOL 	FARPUBLIC DestroyWindow ( PVOID );  /*OPTIONAL*/
BOOL 	FARPUBLIC RegisterWindows( VOID );  /*OPTIONAL*/
DWORD	FARPUBLIC FrameWndProc(PVOID, WORD, WORD, DWORD);   /*OPTIONAL*/
PWND 	FARPUBLIC WindowFromID ( PVOID,WORD );  /*OPTIONAL*/
DWORD	FARPUBLIC DefDlgProc (PVOID, WORD, WORD, DWORD);  /*OPTIONAL*/
VOID	FARPUBLIC SiblingToTop		(PVOID, BOOL);          /*OPTIONAL*/	
VOID	FARPUBLIC DirtyShadow ( PVOID, ARC);
DWORD FARPUBLIC DlgDirWndProc ( PVOID, WORD, WORD, DWORD ); /*OPTIONAL*/
BOOL  FARPUBLIC ShowWindow ( PVOID, WORD, BOOL ); /*OPTIONAL*/
VOID 	FARPUBLIC SetWindowText ( PVOID, char * );  /*OPTIONAL*/
WORD 	FARPUBLIC GetWindowText ( PVOID, char *, WORD );  /*OPTIONAL*/
WORD	FARPUBLIC GetWindowTextLength ( PVOID );  /*OPTIONAL*/
DWORD	FARPUBLIC GroupWndProc(PVOID, WORD, WORD, DWORD);  /*OPTIONAL*/
PWND	FARPUBLIC InternalWindowFromID ( PVOID, WORD );  /*OPTIONAL*/
VOID	FARPUBLIC AddAChild(PVOID, PVOID, WORD);		/*OPTIONAL*/

LONG 	FAR PASCAL DefWindowProc ( PVOID,WORD,WORD,DWORD );  /*OPTIONAL*/


#ifndef BROADSWORD //BLADE and NOT BROADSWORD def's.

/**** BLADE and NOT BROADSWORD def's.	****************/

WORD FARPUBLIC GetWindowWord ( PVOID, WORD );  /*OPTIONAL*/
WORD FARPUBLIC SetWindowWord ( PVOID, WORD, WORD );  /*OPTIONAL*/
BOOL FARPUBLIC RegisterClass ( WORD, WORD, WORD, WORD, WORD, PLFN_WNDPROC );   /*OPTIONAL*/
BOOL FARPUBLIC UnregisterClass ( WORD, HANDLE );   /*OPTIONAL*/
PWNDCLASS FARPUBLIC FindClass	( WORD );   /*OPTIONAL*/

#ifndef DATA_HANDLE
	PWND FARPUBLIC CreateDlgDirCombo ( PVOID, PVOID, PVOID, PVOID, PVOID, char * );/*OPTIONAL*/
#else
	PWND FARPUBLIC CreateDlgDirCombo ( PVOID, PVOID, PVOID, PVOID, PVOID, WORD );  /*OPTIONAL*/
#endif

#endif

#else  // Non-Blade def's.

VOID	FARPUBLIC AddChildHead		(PVOID, PVOID);		/*OPTIONAL*/
VOID	FARPUBLIC AddChildTail		(PVOID, PVOID);		/*OPTIONAL*/

#endif //Non-Blade def's



#ifndef BROADSWORD

VOID  FARPUBLIC FRrcFromArc		( PVOID, NPARC, NPRRC );
VOID	FARPUBLIC WindowToTop		(PVOID);            	/*OPTIONAL*/	
PWND  FARPUBLIC PwndGetTopWindow	(PVOID);					/*OPTIONAL*/
BOOL	FARPUBLIC FIsTopWindow		(PVOID);					/*OPTIONAL*/
VOID	FARPUBLIC GetClientRrc		(PVOID, RRC *);
VOID	FARPUBLIC FillRrc				(PVOID, NPRRC, ACHAR, WORD);
VOID	FARPUBLIC InvalidateRrc		(PVOID,NPRRC);						/*OPTIONAL*/
VOID	FARPUBLIC MoveSizeOverlap	(PVOID, AX, AY, BYTE, BYTE);	/*OPTIONAL*/
VOID	FARPUBLIC BeginPaint			(PVOID, VOID FAR *);

#else	// BROADSWORD exclusive def's.

PWND FARPUBLIC BeginPaint			(PVOID, VOID FAR *);
HWND FARPUBLIC CreateDlgDirCombo ( PVOID, PVOID, PVOID, PVOID, PVOID, char * );  /*OPTIONAL*/
BOOL FARPUBLIC RegisterClass ( LPWNDCLASS ); /*OPTIONAL*/
BOOL FARPUBLIC UnregisterClass ( WORD, HANDLE ); /*OPTIONAL*/

PCLS FARPUBLIC FindClass	( WORD ); /*OPTIONAL*/

WORD FARPUBLIC GetWindowWord ( PVOID, int ); /*OPTIONAL*/
WORD FARPUBLIC SetWindowWord ( PVOID, int, WORD );			 /*OPTIONAL*/

LONG FARPUBLIC GetWindowLong ( PVOID, int ); /*OPTIONAL*/
LONG FARPUBLIC SetWindowLong ( PVOID, int, LONG ); /*OPTIONAL*/

WORD FARPUBLIC GetClassWord ( PVOID, int ); /*OPTIONAL*/
WORD FARPUBLIC SetClassWord ( PVOID, int, WORD ); /*OPTIONAL*/

LONG FARPUBLIC GetClassLong ( PVOID, int );		  /*OPTIONAL*/
LONG FARPUBLIC SetClassLong ( PVOID, int, LONG ); /*OPTIONAL*/

BOOL FARPUBLIC GetClassInfo ( WORD, LPWNDCLASS ); /*OPTIONAL*/

WORD FARPUBLIC GetClassName ( PVOID );				  /*OPTIONAL*/

#endif //BROADSWORD



/************* Button def's. ***********************/
#ifndef WIN_BTN

VOID	FARPUBLIC	CheckDlgButton(PVOID, WORD, BOOL);	  /*OPTIONAL*/
WORD	FARPUBLIC	WButtonChecked(PVOID);					  /*OPTIONAL*/
WORD	FARPUBLIC	BnRadioButtonChecked(PVOID);			  /*OPTIONAL*/
VOID	FARPUBLIC	CheckRadioButton(PVOID, WORD, BOOL);  /*OPTIONAL*/

#ifdef ADM
WORD	FARPUBLIC	BnGroupRadioButtonChecked(PVOID);  /*OPTIONAL*/
#endif

PWND FARPUBLIC	PwndDefaultPushButton(PVOID);  /*OPTIONAL*/

#else

PWND FARPUBLIC	PwndDefaultPushButton(PVOID);	  /*OPTIONAL*/
VOID	FARPUBLIC		CheckDlgButton(PVOID, WORD, WORD);  /*OPTIONAL*/
VOID	FARPUBLIC		CheckRadioButton(PVOID, WORD, WORD, WORD);  /*OPTIONAL*/

#endif

/************* Button def's. ***********************/


/************* Some odds and sods. ***********************/

#ifdef XOR_OUTLINE
VOID		FARPUBLIC DrawXorBoxArc(NPARC);	/*OPTIONAL*/
#endif


#ifdef DATA_HANDLE
	char FAR * FARPUBLIC LpvDerefAppText ( PVOID, WORD, WORD * );		  /*OPTIONAL*/
	char FAR * FARPUBLIC LpvDerefText    ( PVOID, WORD, WORD, WORD * );  /*OPTIONAL*/
	BOOL FARPUBLIC fEnableFarData ( BOOL );							  /*OPTIONAL*/
#endif

#ifndef BIGOVERLAP
	PWND FARPUBLIC PwndAxAy(PVOID, AX, AY);   /*OPTIONAL*/
#endif /* BIGOVERLAP */

#endif	/* !NOPROCS */



#define	OV_NOSHADOW	0x0001

#define	axNil	255
#define	ayNil	255
#define	rxNil	255
#define	ryNil	255

#ifndef NORECT

#define FMoveOverlapWithKeyboard(pwnd) FMoveSizeWithKeyboard(pwnd,TRUE);
#define FSizeOverlapWithKeyboard(pwnd) FMoveSizeWithKeyboard(pwnd,FALSE);

#ifndef BROADSWORD

#define MoveWindowOverlap(pwnd, ax, ay) MoveSizeOverlap(pwnd, ax, ay, rxNil, ryNil)
#define SizeWindowOverlap(pwnd, drx, dry) MoveSizeOverlap(pwnd, axNil, ayNil, drx, dry)
#define CloseWindowOverlap(pwnd) MoveSizeOverlap(pwnd, axNil, ayNil, rxNil, ryNil)

#endif

#define DrawBorder(pwnd, pbox, di, sz)	DrawBorder2(pwnd, pbox, di, di, sz)

#define CopyRect(prrcDest, prrcSrc) {*(prrcDest) = *(prrcSrc) }
#define SetRectEmpty(prrc) {*((long *) prrc) = 0; }

#ifdef BLADE

#define AddChildHead(pwndParent,pwnd)  AddAChild(pwndParent,pwnd,HEAD)
#define AddChildTail(pwndParent,pwnd)  AddAChild(pwndParent,pwnd,TAIL)
#define InvalidateShadow(pwnd,arc)  DirtyShadow((pwnd),(arc))

#endif

#endif /*!NORECT*/

#ifndef NOWNDMACROS

#ifndef BLADE

#ifdef PROJECT_PWB
// Apparently you can't insert an #ifdef in the middle of a macro definition,
// so I have to duplicate all the wnd* definitions just to add the extra
// 0 for wExtStyle
//

#define wndGeneric(id, style, fEnabled, ax, ay, dax, day, pfnWndProc, pwndParent, pwndSibling, pwndChild) { \
	id, style, 0, fEnabled, 0, \
	{ax, ay, ax+dax, ay+day}, \
	{((style) & WS_BORDER) ? ax+1 : ax, \
	 ((style) & WS_BORDER) ? ay+1 : ay, \
	 ((style) & (WS_BORDER | WS_VSCROLL)) ? ax+dax-1 : ax+dax, \
	 ((style) & (WS_BORDER | WS_HSCROLL)) ? ay+day-1 : ay+day}, \
	rrcInvalidStd	\
	(PLFN) pfnWndProc, pwndParent, pwndSibling, pwndChild, 0, 0,

#define wndGenericCursor(id, style, fEnabled, ax, ay, dax, day, pfnWndProc, pwndParent, pwndSibling, pwndChild, axCurs, ayCurs) { \
	id, style, TRUE, fEnabled, 0, \
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
	TRUE, fEnabled, 0, {ax, ay, ax+dax, ay+day},		       \
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
	id, (WS_CHILD | WS_CLIPOUT), FALSE, TRUE, 0, \
	{ 0, 0, 0, 0 }, { 0, 0, 0, 0 }, \
	rrcInvalidStd \
	(PLFN) DropDownWndProc, NULL, NULL, NULL, 0, 0, \
	{ 0, (fCombo ? 1 : 0), NULL, NULL, NULL, NULL, \
		NULL, NULL, NULL, NULL } }

#define wndDropDownButton(id, fEnabled, ax, ay) { \
	id, WS_CHILD | WS_CLIPOUT, 0, fEnabled, 0, \
	{ ax, ay, ax + 1, ay + 1 }, \
	{ ax, ay, ax + 1, ay + 1 }, \
	rrcInvalidStd \
	(PLFN) DropButtonWndProc, \
	NULL, NULL, NULL, 0, 0 }

#define wndDropDownHolder() { \
	0, 0, 0, 0, 0, { 0, 0, 0, 0 }, { 0, 0, 0, 0 }, \
	rrcInvalidStd \
	(PLFN) NULL, NULL, NULL, NULL, 0, 0, { NULL } }


#define wndButton(id, style, fEnabled, ax,ay,dax,day, szTitle) { \
	id,style,TRUE,fEnabled, 0, {ax,ay,ax+dax,ay+day}, {ax,ay, ax+dax, ay+day},\
	rrcInvalidStd \
	(PLFN) ButtonWndProc, NULL, NULL, NULL, ax,ay, {0,szTitle,0,wButtonInit,NULL}}


#define wndGroup(id, ax,ay,dax,day, szTitle) { \
	id,WS_CHILD | WS_BUTTON | BS_GROUP | WS_BORDER, TRUE,FALSE, 0, \
   {ax,ay,ax+dax,ay+day},  \
	{ ax+1, ay+1, ax+dax-1,ay+day-1}, \
	rrcInvalidStd \
	(PLFN) ButtonWndProc, NULL, NULL, NULL, ax,ay, {0,szTitle,0,wButtonInit,NULL}}


#define wndStatic(id, style, fEnabled, ax,ay,dax,day, szTitle) { \
	id,style,FALSE,fEnabled, 0, {ax,ay,ax+dax,ay+day}, {ax,ay, ax+dax, ay+day},\
	rrcInvalidStd \
	(PLFN) StaticWndProc, NULL, NULL, NULL, ax,ay, {0,szTitle,0,0, NULL }}

#else // !PROJECT_PWB

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
	TRUE, fEnabled, {ax, ay, ax+dax, ay+day},		    \
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


#define wndButton(id, style, fEnabled, ax,ay,dax,day, szTitle) { \
	id,style,TRUE,fEnabled, {ax,ay,ax+dax,ay+day}, {ax,ay, ax+dax, ay+day},\
	rrcInvalidStd \
	(PLFN) ButtonWndProc, NULL, NULL, NULL, ax,ay, {0,szTitle,0,wButtonInit,NULL}}


#define wndGroup(id, ax,ay,dax,day, szTitle) { \
	id,WS_CHILD | WS_BUTTON | BS_GROUP | WS_BORDER, TRUE,FALSE, \
   {ax,ay,ax+dax,ay+day},  \
	{ ax+1, ay+1, ax+dax-1,ay+day-1}, \
	rrcInvalidStd \
	(PLFN) ButtonWndProc, NULL, NULL, NULL, ax,ay, {0,szTitle,0,wButtonInit,NULL}}


#define wndStatic(id, style, fEnabled, ax,ay,dax,day, szTitle) { \
	id,style,FALSE,fEnabled, {ax,ay,ax+dax,ay+day}, {ax,ay, ax+dax, ay+day},\
	rrcInvalidStd \
	(PLFN) StaticWndProc, NULL, NULL, NULL, ax,ay, {0,szTitle,0,0, NULL }}

#endif // PROJECT_PWB
#endif // BLADE

// Routines to access elements of WND struc.

#define	IsWindowEnabled(pwnd) ((pwnd)->fEnabled)

#define	GetWindowProc(pwnd) ((pwnd)->pfnWndProc)
#define	SetWindowProc(pwnd, pfn) {(pwnd)->pfnWndProc = pfn;}

#define	PwndParent(pwnd) ((pwnd)->pwndParent)
#define	PwndChild(pwnd) ((pwnd)->pwndChild)
#define	PwndSibling(pwnd) ((pwnd)->pwndSibling)

#define	GetWindowId(pwnd)   ((pwnd)->id)
#define	SetWindowId(pwnd, wIdNew) {(pwnd)->id = wIdNew;}

#define  GetWinArcClipping(pwnd)       ((pwnd)->arcClipping)
#define  SetWinArcClipping(pwnd, arc)  {(pwnd)->arcClipping = arc;}

#define  GetWinArcWindow(pwnd)       ((pwnd)->arcWindow)
#define  SetWinArcWindow(pwnd, arc)  {(pwnd)->arcWindow = arc;}

#define  GetWindowaxCur(pwnd)           ((pwnd)->axCursor)
#define  SetWindowaxCur(pwnd, axCursor) {(pwnd)->axCursor = axCursor;}

#define  GetWindowayCur(pwnd)           ((pwnd)->ayCursor)
#define  SetWindowayCur(pwnd, ayCursor) {(pwnd)->ayCursor = ayCursor;}

#endif /*NOWNDMACROS*/

#ifndef NODRAW
	/* Window Drawing support */
	#define	AxOfRx(pwnd,rx) (AX)((pwnd)->arcClipping.axLeft+(rx))
	#define	AyOfRy(pwnd,ry) (AY)((pwnd)->arcClipping.ayTop+(ry))
	#define	RxOfAx(pwnd,ax) (RX)((ax)-(pwnd)->arcClipping.axLeft)
	#define	RyOfAy(pwnd,ay) (RY)((ay)-(pwnd)->arcClipping.ayTop)
	#define  CbSizeRrc(prrc) (CwSizeRrc(prrc) << 1)
#endif

#ifndef NOWINSTYLES
/* Window styles */
#define	WS_TILED			0x0000
#define	WS_CHILD			0x0000	/* note : no distinction */

#define WS_BORDER			0x0100
#define WS_VSCROLL		0x0200
#define WS_HSCROLL		0x0400
#define WS_SUBSTYLE		0x001f

#define WS_BEENDIRTIED	0x0020

#ifndef BROADSWORD
	#define WS_OVERLAP		0x0040
#else
	#define WS_OVERLAP		0x0040
//	#define WS_OVERLAP		0x0000
#endif

#define WS_CLIPOUT		0x0080

#define WS_TYPE 		0x3800	// type mask 
#define WS_EDIT		0x0800
#define WS_LISTBOX	0x1000
#define WS_BUTTON		0x1800
#define WS_STATIC		0x2000
#define WS_DIALOG		0x2800
#define WS_SCROLL		0x3000
#define WS_STATIC_NOACCEL	0x3800
#define WS_COMBO		0x3800

#ifdef BROADSWORD
	#define WS_MSGBOX		0x0001
	#define WS_DLGBOX		0x0002
#endif


#define LBS_SORT			0x0001
#define LBS_OWNERHILITE	0x0002

#ifdef BLADE

// The old style identifiers.
#define WS_LBCOMBO	0x0001
#define WS_LBSIMPLE	0x0002
#define WS_LBDROPPED	0x0004
#define WS_LBNOAUTO	0x0008
#define WS_LBNOSAVE	0x0010

// Windows compatible identifiers.
#define CBS_DROPDOWN			0x0001
#define CBS_DROPDOWNLIST	0x0002
#define CBS_SIMPLE			0x0004

// These two are NOT Win Compatible.
#define CBS_LBNOAUTO			0x0008
#define CBS_LBNOSAVE			0x0010

#else

#define WS_LBCOMBO	0x0001
#define WS_LBDROPPED	0x0002
#define WS_LBSIMPLE	0x0002
#define WS_LBNOAUTO	0x0004

#endif



#ifdef PROJECT_PWB
// These are style bits for wExtStyle
//
#define WES_NOVBORDERS		0x8000
#define WES_NOBOTTOMBORDER	0x4000
#endif
#endif /*!NOWINSTYLES*/


// Extended window styles.


#define WS_DLGPARENT		0x0010
#define WS_DLGWINDOW		0x0020
#define WS_VISIBLE		0x0040
#define XS_GROUP			0x0080
#define WF_GROUP			0x0080
#define WS_TABSTOP			0x0100
#define WC_TABSTOP			0x0100
#define WS_FRAME				0x2000
#define WS_OWNERREDRAW		0x4000 
#define WS_SHADOW				0x8000

#define WES_DLGPARENT	0x0010
#define WES_DLGWINDOW	0x0020
#define WES_VISIBLE		0x0040
#define WES_GROUP			0x0080
#define WES_TABSTOP			0x0100
#define WES_HSCROLL			WS_VSCROLL
#define WES_VSCROLL			WS_HSCROLL
#define WES_RADIOGROUP		0x0200
#define WES_3D					0x1000
#define WES_FRAME				0x2000
#define WES_OWNERREDRAW		0x4000 
#define WES_SHADOW			0x8000

#define BS_OWNERDRAWFIXED 	0x4000 


#ifdef BROADSWORD

#define WS_OVERLAPPEDWINDOW  ( WS_OVERLAP | WS_CLIPOUT | WS_BORDER )
#define WES_OVERLAPPEDWINDOW ( WES_FRAME )

#endif


#define IsDlgControl(pwnd)		((pwnd)->wExtStyle & WES_DLGWINDOW)
#define IsWindowVisible(hwnd)	((hwnd)->wExtStyle & WES_VISIBLE)
#define IsAGroupWindow(hwnd)	((hwnd)->wExtStyle & WES_GROUP)
#define IsFrameWindow(hwnd)	((hwnd)->wExtStyle & WES_FRAME)



#define SetExtStyleFlags(pwnd, flags) ((pwnd)->wExtStyle |= (flags))
#define GetExtStyleFlags(pwnd, flags) ((pwnd)->wExtStyle & (flags))
#define ClearExtStyleFlags(pwnd, flags) ((pwnd)->wExtStyle &= ~(flags))


/* codes for listbox item indices */
#define	iszMin	((WORD)	0)
#define	iszNil	((WORD)	-1)		/* invalid isz */

// Frame control styles 
// ---------------------
#define FS_CLOSE           0x01
#define FS_MINMAX          0x02
#define FS_VHSIZE          0x04
#define FS_HSIZE           0x08
#define FS_VSIZE           0x10
#define FS_DEFAULT         0x1f



/*from static.h */
/* static control styles */
#define SS_LEFT 		0	/* left justified text */
#define SS_CENTER 	1	/* centered text */
#define SS_RIGHT 		2	/* right justified text */


//  Frame zone interpretation flags.

#define    FZ_TL      0 	     // top left corner 
#define    FZ_TOP     1         // top side 
#define    FZ_TR      2	        // top right corner 
#define    FZ_LEFT    3         // left side 
#define    FZ_CLIENT  4         // middle 
#define    FZ_RIGHT   5         // right side
#define    FZ_BL      6         // bottom left corner
#define    FZ_BOTTOM  7         // bottom side 
#define    FZ_BR      8         // bottom right corner

#define    FZ_CLOSE   0 	     // top left corner 
#define    FZ_TMOVE   1         // top side 
#define    FZ_ZOOM    2	        // top right corner 
#define    FZ_LMOVE   3         // left side 
#define    FZ_NOP	    4         // middle 
#define    FZ_RSIZE   5         // right side
#define    FZ_LCMOVE  6         // bottom left corner
#define    FZ_BSIZE   7         // bottom side 
#define    FZ_BRCSIZE 8         // bottom right corner

#define    FZ_BLCSIZE 9         // bottom left corner
#define    FZ_TLCSIZE 10         // top left corner
#define    FZ_TRCSIZE 11         // top right corner
#define    FZ_LSIZE   12         // left side size
#define    FZ_TSIZE   13         // top side size

// The largest frame zone code CW defines. App's may pass back other values
//	from the frame filter, in which case the CW overlap filter will pass
//	them to the app directly as a message.
#define    FZ_MAXINTERNAL	FZ_TSIZE

#define WM_DLGRETURN			0x8004

#ifdef BLADE

// Frame control IDs    
// Lower byte is for client ID. Upper byte is reserved for US!
 
#define FID_SYSMENU         0x8002
#define FID_TITLEBAR        0x8003
#define FID_MINMAX          0x8004
#define FID_MENU            0x8005
#define FID_VERTSCROLL      0x8006
#define FID_HORZSCROLL      0x8007

#define FID_FRAME           0x0100
#define FID_CLIENT          0x0200

#define WM_DESTROY         0x0009
#define WM_FRAMEDRAW       0x8000
#define WM_OWNERDRAW       0x8000

#define WM_ADDMENU	      0x8001
#define WM_DELETEMENU      0x8002
#define WM_REPLACEMENU		0x8003

#define WM_VISIBLE			0x8005
#define WM_CALCRECT			0x000b
#define WM_NCCALCSIZE		0x8006
#define WM_VALIDATEDIR		0x8007
#define WM_WINTOTOP			0x8008
#define WM_CALCCLIENT		0x8009
#define WM_SETDLGPOS			0x800a
#define WM_DRAWITEM			0x800b
#define WM_GETDLGCODE		0x800c
#define WM_ENABLE				0x800e
#define WM_CTLCOLOR			0x800f
#define WM_GETDLGCTL			0x8010
#define WM_SYSTIMER			0x8011
#define WM_ERASEBKGND		0x8012
#define WM_NEXTDLGCTL		0x8013
#define WM_SIZESCROLL		0x8014


#define WC_STATIC			0x8001
#define WC_GROUP			0x8003
#define WC_BUTTON			0x8004
#define WC_CHECKBOX		0x8005
#define WC_RADIOBUTTON  0x8006
#define WC_EDIT			0x8007
#define WC_LIST			0x8008

#define WC_DROPDOWN		0x800b
#define WC_DROPEDIT		0x800c
#define WC_DROPBUTTON	0x800d
#define WC_DROPLIST		0x800e
#define WC_DROPHOLDER	0x800f

#define WC_FRAME			0x8010
#define WC_SCROLLBAR		0x8011
#define WC_DIALOG			0x8012
#define WC_MSGBOX			0x8013
#define WC_MLE				0x8014
#define WC_DLGDIR			0x8015
#define WC_MENU			0x8016
#define WC_SCROLL_CLIPPED	0x8017
#define WC_GENPIC			0x8018

#endif // BLADE

#ifdef BROADSWORD

DWORD FARPUBLIC WinEditWndProc(PVOID, WORD, register WORD, DWORD);  /*OPTIONAL*/

#define WC_WINEDIT	0x8018

#endif

#ifndef EDIT_FULLMGR
	#define WM_SETTEXT       0x0002
	#define WM_GETTEXT       0x0004
#else
	#define WM_SETTEXT	(WM_USER+2+6)
	#define WM_GETTEXT	(WM_USER+2+7)
#endif

#define WM_GETTEXTLENGTH 0x000a
#define	WM_INITDIALOG	 0x0381


// Limit size for QB 
#ifdef EDIT_LIMIT_SIZE
	#define cwEditBuffer 64 			// 128 char limit 
#else
	#define cwEditBuffer 128			// 256 char limit 
#endif

#define	cbEditDefault	(cwEditBuffer * sizeof(WORD) - 1)


/********************** CW Button Code. ***********************************/

// There are two sets of button source code. One is the pre-existing button
// code of old CW origin ( Contained in button.c ). The other code is a
// re-write which brings the code into Windows conformity, and corrects 
// many defects of the old code ( Contained in btnctl.c and btndraw.c )
// 
// Apps should strive to use the new button code whenever possible. This
// code was originally written for inclusion in BROADSWORD, but has been
// backported to CW 3.1 for current clients.
// 

#ifndef WIN_BTN
// Button notification codes.

#define BN_DOUBLECLICKED	0
#define BN_CLICKED			1
#define BN_PAINT				2
#define BN_HILITE				3
#define BN_UNHILITE			4
#define BN_DISABLE			5


#define BS_AUTO				0x10
#define BS_PUSHBUTTON		0
#define BS_DEFPUSHBUTTON	1
#define BS_CHECKBOX			2
#define BS_AUTOCHECKBOX		(BS_CHECKBOX | BS_AUTO)
#define BS_RADIOBUTTON		3
#define BS_GROUP 				4


#define	wButtonFirstRadio	8	// Initial value for 1st button 
#define	wButtonInit		0		// Initial value for other buttons 


#define  wsRadioButton	(WS_CHILD | WS_BUTTON | BS_RADIOBUTTON)
#define  wsCheckbox		(WS_CHILD | WS_BUTTON | BS_CHECKBOX)
#define	wsPushButton	(WS_CHILD | WS_BUTTON | BS_PUSHBUTTON)
#define	wsDefPushButton	(WS_CHILD | WS_BUTTON | BS_DEFPUSHBUTTON)
#define	dayPushButton	1		/* single line button */
#define	daxPushButton	2		/* <> overhead */

#else

// The Windows compatable button source.

/* button control styles */
#define BS_PUSHBUTTON    0
#define BS_DEFPUSHBUTTON 1
#define BS_CHECKBOX      2
#define BS_AUTOCHECKBOX  3
#define BS_RADIOBUTTON   4
#define BS_3STATE        5
#define BS_AUTO3STATE    6
#define BS_GROUPBOX      7
#define BS_USERBUTTON    8
#define BS_AUTORADIOBUTTON 9
#define BS_PUSHBOX       10
#define BS_OWNERDRAW	   0x0B

#define BS_GROUP 			  7

/* User Button Notification Codes */
#define BN_CLICKED	0
#define BN_PAINT	   1
#define BN_HILITE	   2
#define BN_UNHILITE	3
#define BN_DISABLE	4
#define BN_DOUBLECLICKED   5

// Button Control Messages 
#define BM_GETCHECK	   (WM_USER+0)
#define BM_SETCHECK	   (WM_USER+1)
#define BM_GETSTATE	   (WM_USER+2)
#define BM_SETSTATE	   (WM_USER+3)
#define BM_SETSTYLE	   (WM_USER+4)
#define BM_CLICK		   (WM_USER+5)

// The following def's are for compatability with the older button
// code. Don't use them if you can help it! They WILL be gone soon.

#define  wsRadioButton	(WS_CHILD | WS_BUTTON | BS_RADIOBUTTON)
#define  wsCheckbox		(WS_CHILD | WS_BUTTON | BS_CHECKBOX)
#define	wsPushButton	(WS_CHILD | WS_BUTTON | BS_PUSHBUTTON)
#define	wsDefPushButton	(WS_CHILD | WS_BUTTON | BS_DEFPUSHBUTTON)
#define	dayPushButton	1		/* single line button */
#define	daxPushButton	2		/* <> overhead */

#endif

/********************** End Of Button Code *********************************/


#ifndef BROADSWORD

#define InvalidateRect(hwnd, prrc) InvalidateRrc((hwnd),prrc)
#define GetClientRect(hwnd,prrc) GetClientRrc((hwnd),prrc)

#define UpdateWindow(pwnd)  XSendMessage((pwnd), WM_PAINT, 0, 0L )

#define GetDC(hWnd)	(hWnd)
#define ReleaseDC(hWnd, hDC)

#endif //!BROADSWORD


#ifdef BLADE

// Owner draw control types 
#define ODT_MENU		1
#define ODT_LISTBOX	2
#define ODT_COMBOBOX	3
#define ODT_BUTTON	4

// Owner draw actions 
#define ODA_DRAWENTIRE	0x0001
#define ODA_SELECT		0x0002
#define ODA_FOCUS			0x0004

// Owner draw state 
#define ODS_SELECTED	0x0001
#define ODS_GRAYED	0x0002
#define ODS_DISABLED	0x0004
#define ODS_CHECKED	0x0008
#define ODS_FOCUS		0x0010


// DRAWITEMSTRUCT for ownerdraw 

typedef struct tagDRAWITEMSTRUCT
	{
	WORD	CtlType;
	WORD	CtlID;
	WORD	itemID;
	WORD	itemAction;
	WORD	itemState;
	HWND	hwndItem;
	HDC	hDC;
#ifdef NOGD_RECT
	RECT	rcItem;
#else
	ARC	rcItem;
#endif
	DWORD	itemData;
	} DRAWITEMSTRUCT;

typedef DRAWITEMSTRUCT NEAR *PDRAWITEMSTRUCT;
typedef DRAWITEMSTRUCT FAR  *LPDRAWITEMSTRUCT;


// Dialog Codes 
#define DLGC_WANTARROWS			0x0001	// Control wants arrow keys	    
#define DLGC_WANTTAB				0x0002	// Control wants tab keys	    
#define DLGC_WANTALLKEYS		0x0004	// Control wants all keys	    
#define DLGC_WANTMESSAGE		0x0004	// Pass message to control	    
#define DLGC_HASSETSEL			0x0008	// Understands EM_SETSEL message    
#define DLGC_DEFPUSHBUTTON		0x0010	// Default pushbutton		    
#define DLGC_UNDEFPUSHBUTTON	0x0020	// Non-default pushbutton	    
#define DLGC_RADIOBUTTON		0x0040	// Radio button 		    
#define DLGC_WANTCHARS			0x0080	// Want WM_CHAR messages	    
#define DLGC_STATIC				0x0100	// Static item: don't include	    
#define DLGC_BUTTON				0x2000	// Button item: can be checked	    


#endif //BLADE



#define ENUM_START	0
#define ENUM_DATA		1
#define ENUM_END		2


#define WC_STATICDIR	0x0800
#define WC_LISTDIR	0x1000
#define WC_LISTDRIVE	0x1800



typedef	struct fszi_
	{
	LPSTR	lsz;
	ISA	isa;
	} FSZI;


#define RethinkDisplay()		/* Rethinkdisplay is no longer used */


#define SW_VISIBLE	 1
#define SW_INVISIBLE	 2


typedef struct _boxtextparms
	{
#if defined(BLADE) || defined(PROJECT_PWB)
	WORD 	_aclDialog;
#endif
	BOOL	fCenter;
	char *szTitle;
	BOOL	fBottom;
	BOOL	fRight;
	WORD	diRest;
	WORD	diTop;
	ARC * parc;
	BOX * pbox;
	} BOXTEXT_PARMS, *PBOXTEXT_PARMS;



#ifdef ADM

// For TED. Temporary.

#define wndGeneric(id, style, fEnabled, ax, ay, dax, day, pfnWndProc, pwndParent, pwndSibling, pwndChild) { \
	id, style, 0, fEnabled, WS_VISIBLE,\
	{ax, ay, ax+dax, ay+day}, \
	{((style) & WS_BORDER) ? ax+1 : ax, \
	 ((style) & WS_BORDER) ? ay+1 : ay, \
	 ((style) & (WS_BORDER | WS_VSCROLL)) ? ax+dax-1 : ax+dax, \
	 ((style) & (WS_BORDER | WS_HSCROLL)) ? ay+day-1 : ay+day}, \
	rrcInvalidStd	\
	(PLFN) pfnWndProc, pwndParent, pwndSibling, pwndChild, 0, 0, 0,

#define endWndGeneric }

#endif


#ifdef BLADE

#define _optNotButton	0
#define _optIsButton		1
#define _optIsDefButton	2

#endif

#define ID_DESKTOP	0xabcd
#define ID_INTERNAL	0xffff
#define ID_DLGDIR		0xfffd
#define ID_DIALOG		0xfffc
#define ID_MSGBOX		0xfffb

#define ID_VSCROLL	0xfffa
#define ID_HSCROLL	0xfff9


#ifdef BLADE
#define Is3D( pwnd )  (pwnd)->wExtStyle & WES_3D;

extern PBOX pboxUL;
extern PBOX pboxBR;
	
extern PBOX pboxDoubleUL;
extern PBOX pboxDoubleBR;

#endif

#ifdef BROADSWORD

// Window field offsets for GetWindowLong() and GetWindowWord() 

#define GWL_WNDPROC	    (-14)
#define GWW_HWNDPARENT	 (-10)
#define GWW_ID		    	 (-34)
#define GWW_STYLE	    	 (-32)
#define GWW_EXSTYLE	    (-30)

// Class field offsets for GetClassLong() and GetClassWord()

#define GCL_MENUNAME	    	(-4)
#define GCW_HBRBACKGROUND  (-8)
#define GCW_HCURSOR	    	(-10)
#define GCW_HICON	    		(-12)
#define GCW_CBWNDEXTRA	   (-16)
#define GCW_CBCLSEXTRA	   (-18)
#define GCL_WNDPROC	    	(-22)
#define GCW_STYLE	    		(-24)


#endif


#ifndef NOWNDMACROS

#ifndef BLADE

#ifndef	REVIEW	/* fBorder=True not support */
#define wndEdit(id, fBorder, fEnabled, ax, ay, dax, day, pwndParent, pwndSibling, szBuf, cchMax, chFill, isa, isaSel) {\
	id, WS_CHILD | WS_EDIT, \
	0, fEnabled, {ax, ay, ax+dax, ay+day}, \
	{ax, ay, ax+dax, ay+day}, \
	rrcInvalidStd	\
	(PLFN) InternalEditWndProc, pwndParent, pwndSibling, NULL, 0, 0, \
	{0, (WORD) szBuf, cchMax, isa, isaSel, chFill, 0, 0, 0, 0, TRUE, 0, \
	 cchMax}}
#else
#define wndEdit(id, fBorder, fEnabled, ax, ay, dax, day, pwndParent, pwndSibling, szBuf, cchMax, chFill, isa, isaSel) {\
	id, WS_CHILD | WS_EDIT | (fBorder ? WS_BORDER : 0), \
	0, fEnabled, {ax, ay, ax+dax, ay+day}, \
	{((fBorder) ? ax+1 : ax), (fBorder ? ay+1 : ay), \
	 ((fBorder) ? ax+dax-1 : ax+dax), (fBorder ? ay+day-1 : ay+day)}, \
	rrcInvalidStd	\
	(PLFN) InternalEditWndProc, pwndParent, pwndSibling, NULL, 0, 0, \
	{0, (WORD) szBuf, cchMax, isa, isaSel, chFill, 0, 0, 0, 0, TRUE, 0, \
	 cchMax}}
#endif
#endif // BLADE


// Edit Control Text Styles 
#ifndef BROADSWORD

#define ES_LEFT		0	// left justified text 
#define ES_CENTER		1	// centered text 
#define ES_RIGHT		2	// right justified text 

#endif

#define	ES_SECRET	4	// secret edit field 
#define	ES_DLGDIR	8	// edit field belongs to dir/files combo.



// InternalEditWndProc must be forward defined for CS compiler 
DWORD 		FARPUBLIC InternalEditWndProc ( PVOID, WORD, WORD, DWORD);

#endif // !NOWNDMACROS 

/* for selection (SetTmcSel) */
#define	ichSelectEnd	(0x7fff)		/* select to end */

#ifdef BLADE

#ifndef EDIT_FULLMGR	
VOID		FARPUBLIC SetEditText	( PVOID, char far *, BOOL);
#else
VOID		FARPUBLIC SetEditText	(PVOID, char *, BOOL);
#endif

#else

VOID		FARPUBLIC SetEditText	(PVOID, char *, BOOL);

#endif


#ifndef EDIT_FULLMGR	
VOID		FARPUBLIC SetEditWidth	(PVOID, int);				/*OPTIONAL*/
#else
VOID		FARPUBLIC SetEditWidth	(PVOID, WORD);				/*OPTIONAL*/
#endif

WORD		FARPUBLIC GetEditText	(PVOID, char *, WORD);
DWORD		FARPUBLIC EditWndProc	(PVOID, WORD, WORD, DWORD);

VOID		FARPUBLIC DefaultEditMgrInit(VOID);						/*OPTIONAL*/

#define EN_CHANGE			0x0300
#define EN_CURSORMOVED			0x0301


#ifndef NOMSG

// Message structure 

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
#endif

#ifndef OS2

extern BOOL PASCAL fMessage;
extern BOOL PASCAL fUseCache;

#ifndef	KANJI
#define	FMessage()		(fMessage || fUseCache)
#else
extern BOOL PASCAL fKkcMessage;
#define	FMessage()		(fKkcMessage || fMessage || fUseCache)
#endif

#ifdef DUAL
extern DWORD PASCAL semaMessage;	// message semaphore 
#define	hsemaMessage	((HSEM) (DWORD FAR *) &semaMessage)
#endif

#else

extern DWORD PASCAL semaMessage;	// message semaphore 
#define	hsemaMessage	((HSEM) (DWORD FAR *) &semaMessage)

#endif


#endif // !NOMSG

typedef BOOL (FARPUBLIC *PBFN_KBDMSG)(WORD, WORD, DWORD);

#ifndef NOWINMESSAGES

// Window Messages 

#define	WM_NULL			0x0000
#define	WM_CREATE		0x0001
#define	WM_WANTFOCUS	0x1005
#define	WM_MAKEACTIVE	0x1006
#define	WM_SETFOCUS		0x0007
#define	WM_KILLFOCUS	0x0008
#define	WM_REPAINT		0x100e
#define	WM_PAINT			0x000f
#define	WM_QUIT			0x0012

#define	repModeReset	0		// handled by RepaintScreen
#define	repRedraw		1		// handled by RepaintScreen
#define	repGraphic		2		// app should redraw GSD graphic

// Non-client (for Overlap only) 
#define	WM_NCLBUTTONDOWN	0x00a1

#define	WM_KEYFIRST		0x0100
#define	WM_KEYLAST		0x0102

#define	WM_KEYDOWN		0x0100
#define	WM_KEYUP			0x0101
#define	WM_CHAR			0x0102

#define	WM_CUT			0x0300

#define	WM_COPY			0x0301
#define	WM_PASTE			0x0302
#define	WM_INSERT		0x1303

#define	WM_MENUIDLE			0x1110		// Menu Idle
#define	WM_MENUINACTIVE	0x1111		// Menu inactive 
#define	WM_COMMAND			0x0111
#define	WM_MENUSELECT		0x0112		// selecting a menu item 
#define	WM_MENUSTART		0x0113		// starting a menu action 
#define	WM_HSCROLL			0x0114
#define	WM_VSCROLL			0x0115
#define	WM_INITMENUPOPUP	0x0117
#ifdef ESCHER
#define	WM_MENU_COMMAND	0x0118
#else
#define	WM_MENU_COMMAND	WM_COMMAND
#endif
#define	WM_DISABLEDCOMMAND	0x0119

#define	WM_ALARM				0x1118

#define WM_DLGMSG_START		0x0120
#define WM_DLGMSG_END		0x0121

#define	WM_MOUSEFIRST		0x0200
#define	WM_LMOUSELAST		0x0203	// last of Left mouse actions 
#define	WM_MOUSELAST		0x0209

#define	WM_MOUSEMOVE		0x0200	// mouse related constants 
#define	WM_LBUTTONDOWN		0x0201
#define	WM_LBUTTONUP		0x0202
#define	WM_LBUTTONDBLCLK	0x0203
#define	WM_RBUTTONDOWN		0x0204
#define	WM_RBUTTONUP		0x0205
#define	WM_RBUTTONDBLCLK	0x0206

// Edit Wnd Proc Messages 

#ifndef FULL_EDIT

#define EM_SETSEL	0x0400
#define EM_GETSEL	0x0401

#endif

#ifdef BLADE
	#define EM_SETDEF	0x0402
	#define EM_SETBUF	0x0403

	#define EM_FIRST	EM_SETSEL
	#define EM_LAST	EM_SETBUF
#endif

// Debugging Message 
#define WM_TRACEOUT		0x0500
#define WM_TRACETAG_REGISTER	0x0501

// Overlap Wnd Proc Messages 
#define WM_ACTIVATE		0x0006
#define WM_ZOOM			0x0321
#define WM_CLOSE			0x0010
#define WM_MOVE			0x0003
#define WM_SIZE			0x0005


// WM_ACTIVATE response codes 
#define	rspActiveLive		((DWORD) 2)	// pass activating event on 
#define	rspActive			((DWORD) 1)	// eat activating event 
#define	rspActiveDecline	((DWORD) 0)	// [de]activation declined 


// Dialog related messages 
#define WM_DIALOGINACTIVE	0x0360

//WM-DIALOG is actually private (and LB_SETWIDTH)

#define WM_DIALOG		0x0380

// Listbox Proc Messages 

#define WM_LISTBOX_COMMAND	WM_DIALOG
#define LB_FIRST				LB_RESETCONTENT
#define LB_RESETCONTENT		0x0340
#define LB_ADDSTRING			0x0341
#define LB_DELETESTRING		0x0342
#define LB_SETCURSEL			0x0343
#define LB_GETCURSEL			0x0344
#define LB_SETWIDTH			0x0345
#define LB_GETTEXT			0x0346
#define LB_GETCOUNT			0x0347
#define LB_REPLACESTRING	0x0348
#define LB_INSERTSTRING		0x0349
#define LB_HILITECURSEL		0x0350

#define LB_SETDEF			0x0351
#define LB_INIT			0x0353

#define LB_LAST	 LB_INIT

// VAP Request for application to handle 
#define WM_VAP_REQUEST		0x0390

// Kkc converter message for application to handle 
#define	WM_KKCONVERT		0x03f0
#define	WM_KKPAINT			0x03f1
#define	WM_KKCSTR_START	0x03f2
#define	WM_KKCSTR_END		0x03f3

#define	WM_KKENABLE			0x03f7
#define	WM_KKDISABLE		0x03f8

// drop down listbox notification messages 
#define DD_MINIMIZED		0x0370
#define DD_MAXIMIZED		0x0371
#define DD_MINIMIZE_NOW 0x0372
#define DD_MAXIMIZE_NOW 0x0373

	// Added DD_MAXIMIZE_NOW to allow users to maximize drop down 
	//  listboxes.						    
	// LGT: 10/08/90					     


// parameter for DD_MINIMIZED in low byte of lParam 
#define ddMinEsc			1
#define ddMinEnter		2
#define ddMinControl		4
#define ddMinKillFocus	8
#define ddMinMsg			16

// private window messages start here 
#define	WM_USER			0x0400
#endif

// Window's aliases for KK_ states 

#define	KK_MENU			KK_ALT
#define	KK_CAPITAL		KK_CAPLOCK

// HELP 
#define	VK_HELP_KEY		VK_F1

// listbox notification codes 
#define LBN_SELCHANGE		0
#define LBN_DBLCLK			1
#define LBN_SELECT_DONE		2
#define LBN_OOM				3
#define LBN_OWNERHILITE		4


// List Box Selection Codes 
#define lbrCause		0xf
#define lbrNone		0
#define lbrMouse		1
#define lbrScroll		2
#define lbrKeys		3
#define lbrSpace		4
#define lbrOther		5
#define lbrEnter		6
#define flbrReselect	0x10


// help types/contexts 
#define	hemMenu			1
#define	hemMenuItem		2
#define	hemMbox			3
#define	hemDialog		4
#define	hemUserMin		0x10		// For Application contexts 

// help id's for message boxes 

#ifndef NOMB

#define	hidMboxOk				MB_OK
#define	hidMboxYesNoCancel	MB_YESNOCANCEL
#define	hidMboxRetryCancel	MB_RETRYCANCEL
#define	hidMboxOkCancel		MB_OKCANCEL
#define	hidMboxAbort			MB_ABORT
#define hidMboxYesNo				MB_YESNO
#define hidMboxRetry				MB_RETRY

#endif //!NOMB

VOID		FARPUBLIC SetAlarm( PVOID, WORD);
VOID		FARPUBLIC KillAlarm(void);

#ifndef REVIEW // should UndoRepeat be exported? 
	VOID		FARPUBLIC UndoRepeat(WORD, DWORD);
#endif


DWORD		FARPUBLIC SendMessage	(PVOID, WORD, WORD, DWORD);

// #if defined(BLADE) && !defined(DEBUG)
#ifdef BLADE

#define XSendMessage(pwnd, message, wParam, lParam ) (*(pwnd)->pfnWndProc)( (pwnd), message, wParam, lParam)

#else

#define XSendMessage(pwnd, message, wParam, lParam ) SendMessage((pwnd), (message), (wParam), (lParam) )

#endif


#ifdef BROADSWORD

extern PWND PASCAL pwndCapture;
extern PWND PASCAL pwndFocus;

#define GetFocus()	pwndFocus

#define DispatchMessage(pmsg)	\
	(pmsg)->pwnd != NULL ?  (*((pmsg)->pwnd)->pfnWndProc)((pmsg)->pwnd, (pmsg)->message, (pmsg)->wParam, (pmsg)->lParam) : \
	0L

#define ReleaseCapture() 	SetCapture(NULL)

#ifdef PAINTQ
BOOL		FARPUBLIC PostPaintMessage	( PVOID ); /*OPTIONAL*/
#endif

#else	//BROADSWORD

PWND		FARPUBLIC GetFocus		(void);
DWORD		FARPUBLIC DispatchMessage	(PMSG);
VOID 		FARPUBLIC ReleaseCapture	(void);

#endif	//BROADSWORD


VOID		FARPUBLIC UngetMessage	(PMSG);
BOOL		FARPUBLIC PeekMessage	(PMSG);
BOOL		FARPUBLIC FNextMsg		(PMSG);
PWND		FARPUBLIC SetFocus		( PVOID );
VOID		FARPUBLIC FlushAbort		(void);
PWND		FARPUBLIC SetCapture		( PVOID );
BOOL		FARPUBLIC PostMessage	(PVOID, WORD, WORD, DWORD);
VOID		FARPUBLIC HookKeyboardPoll	(BOOL, LPFN);
WORD		FARPUBLIC SetDoubleClickTime	(WORD);		//OPTIONAL
VOID		FARPUBLIC SynthesizeShiftKeys	(WORD, WORD);	//OPTIONAL
VOID		FARPUBLIC HookKeyboardMessage	(BOOL, PBFN_KBDMSG);
BOOL		FARPUBLIC InsertKeyboardMessage	(WORD, WORD, DWORD);
WORD		FARPUBLIC CmsgKeyboardQueueEntry	(WORD, PMSG);


// CW Hook constants.

#define WH_MENU     0
#define WH_OVERLAP  1
#define WH_DIALOG   2
#define WH_FRAME    3


typedef BOOL	(FARPUBLIC *PFFN_FILTER)(PMSG);	// filter 
typedef BOOL	(FARPUBLIC *PFFN_FRAME_FILTER)(PMSG, WORD *);	// filter 

PFFN_FILTER FARPUBLIC SetCWHook ( WORD, PFFN_FILTER );


/**END_PUBLIC**/

#ifdef COW

// application-supplied callback functions 
BOOL FARPUBLIC FAppNextMsg(PMSG);

// window if alarm set (used by timer) 
extern PWND pwndAlarm;

// is keyboard polling hooked 
extern BOOL fHookPollKeyboard;

#ifdef KANJI
	VOID		FAR PASCAL KeyboardMessage(BYTE, BYTE, WORD, WORD, BOOL);
#else
	VOID		FAR PASCAL KeyboardMessage(BYTE, WORD, WORD, BOOL);
#endif

VOID		FAR PASCAL SpecialTsrAbort(void);
BOOL		FAR PASCAL FTestKeyboardEmpty(void);
VOID		FAR PASCAL FlushKeyEvents(VOID);

#endif


#ifndef NOCOLOR

//	* General colors 
#define	isaNil			((ISA) -1)

#define	isaBackground		0

#define	isaHilite		1		// hilite / inversion 
#define	isaGreyed		2		// not currently used 
#define	isaEnabled		3
#define	isaDisabled		4
#define	isaAlert		5

//	* Dialog elements :	
#define	isaDialogBox		6					// the actual dialog box 
#define	isaStatic			isaDialogBox	// static text 
#define	isaButton			isaDialogBox	// radio/check buttons 
#define	isaPushButton		7					// push buttons 
#define	isaButtonDown		8					// pushed button 
#define	isaListBox			9					// listbox background 
#define	isaEdit				isaDialogBox

//	 Scroll Bars :		
#define	isaScrollbar		10
#define	isaElevator			11

//	Menus :		

#ifdef BLADE

//  NOTE! Due to isa re-mapping for Escher, these two isa's are not used
// 		 internally by CW anymore. There slots are freed up.
//  isaItemHiliteSel
//  isaMenuBox

	#define	isaMenu				13			// non-selected MENU 
	#define	isaMenuBox			isaMenu	// box around pull downs 
	#define	isaMenuSelected	14			// selected menu item 
	#define	isaMenuHilite		15			// hilited character 
#else
	#define	isaMenuBox			12		// box around pull downs 
	#define	isaMenu				13		// non-selected MENU 
	#define	isaMenuSelected	14		// selected menu item 
	#define	isaMenuHilite		15		// hilited character 
#endif

// hilited character under selection 

#ifdef BLADE
	#define	isaMenuHiliteSel	16		// for menu titles 
	#define	isaItemHiliteSel	isaMenuHiliteSel		// for menu items 
#else
	#define	isaMenuHiliteSel	16		// for menu titles 
	#define	isaItemHiliteSel	17		// for menu items 
#endif


#define	isaDialogAccel		18		// dialog accelerators 
#define	isaDialogAccelBor	19		// dialog accelerator border 

//	 Shadows :		
#define	isaShadow		20

#ifdef BLADE
// 3D Effect 


#define	isaDialogBar		21
#define	isaDialogTitle		22

#define	isa3DGroupBoxIn	23		// 3D group box effects 
#define	isa3DGroupBoxOut	24
#define	isa3DListboxIn		25
#define	isa3DListboxOut	26

#define	isa3DPushBtnIn		27
#define	isa3DPushBtnOut	28


#define	isa3DGroupBoxUL	isa3DGroupBoxIn	
#define	isa3DGroupBoxBR	isa3DGroupBoxOut	

#define	isa3DListboxUL		isa3DListboxIn	
#define	isa3DListboxBR		isa3DListboxOut

#define	isa3DPushBtnUL		isa3DPushBtnIn	
#define	isa3DPushBtnBR		isa3DPushBtnOut

#define	isaButtonAccel		29		// dialog accelerators 

#define	isaSystemMax		29

#else	//BLADE

#define	isaSystemMax	   isaShadow

#endif //BLADE

// User Colors :		
#define	isaUserMin		isaSystemMax+1
#define	isaUserMax		(isaUserMin+16)
#define	isaMax			isaUserMax

#ifdef KANJI
#define	ffontNil	((FFONT)0L)
#else
#define	ffontNil	((FFONT)0)
#endif

#endif //!NOCOLOR


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

// Menu Modes : HIWORD(lParam) for WM_COMMAND / WM_MENUSELECT messages 

#define	mmdItem		0		// dropdown item 
#define	mmdAccel		1		// keyboard accelerator 
#define	mmdMenu		2		// dropdown not item 
#define	mmdString	3		// WM_MENUSELECT contains string 

// Help Line codes for wParam WM_MENUSELECT (lParam == 0) 

#define	enClear		0		// clear must be zero 
#define	enCommand	1		// prepare help line for command 


typedef struct _mpvkeyid
	{
	WORD	vkey;
	WORD	idItem;
	} MPVKEYID;
#define VkeyOfVkKk(vk, kk)	((vk) | (kk))

#ifndef SMM			// Old menu structures 

typedef struct _menuitem
	{
	WORD	idItem;			// id for menuitem 
	BITS	fEnabled:1;		// TRUE => enabled, FALSE => greyed 
	BITS	fChecked:1;		// TRUE => checked 
	BITS	fSeparator:1;	// TRUE => separator 
	BITS	fHandle:1;		// TRUE => use pszItem, else szItem 
	BITS	ichHilite:4;	// index of prefix character 
	BITS	bParamUser:8;	// available for application use 

	union
		{
		char *szItem;
		char **pszItem;
		} CC_USZ;
	WORD	wParamUser;		// available for application use 

#ifdef KANJI
	WORD	chKanaAccel;	// Kana Accelerators 
#endif

	} MENUITEM;
typedef struct _menu
	{
	WORD	idMenu;
	BITS	rxTitle:8;
	BITS	ichHilite:4;		// index of prefix character
	BITS	fHandle:1;
	BITS	fEnabled:1;			// is menu 'enabled' (not greyed) 
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
	WORD	wParamUser;		// available for application use 
#ifdef KANJI
	WORD	chKanaAccel;		// Kana Accelerators 
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

// menu item define macros :
//	D => disabled
//	H => string handle
//	X => with index (otherwise default to 0)

#ifndef KANJI

// enabled simple item 

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

// separator
#define	menuitemSep	{0, FALSE, FALSE, TRUE, FALSE, 0, 0, {NULL}, 0},

#else	// KANJI - 1 extra value needed for Kana Accelerator 
		// Accelerator should always be at start 

#define menuitem(mid, sz, chKana, w) {(mid), TRUE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w), (WORD) (chKana)},

#define menuitemD(mid, sz, chKana, w) {(mid), FALSE, FALSE, FALSE, FALSE,\
	0, 0, {(sz)}, (WORD) (w), (WORD) (chKana)},

#define menuitemDH(mid, psz, chKana, w) {(mid), FALSE, FALSE, FALSE, TRUE,\
	0, 0, {(char *)(psz)}, (WORD)(w), (WORD) (chKana)},

// separator 
#define	menuitemSep	{0, FALSE, FALSE, TRUE, FALSE, 0, 0, {NULL}, 0, 0},

#endif /*KANJI*/

#else // SMM		  

// New menu structures 

typedef struct _mtm
	{
	WORD	id;
	BYTE	bFlags;
//	BITS	fDisabled:1;
//	BITS	fChecked:1;
//	BITS	fSeparator:1;
//	BITS	fString:1;
//	BITS	fHelp:1;
//	BITS	fMenuBreak:1;
//	BITS	fSubMenu:1;
//	BITS	filler:1;	
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
#define MM_STRING		0x08
#define MM_HELP		0x10
#define MM_MENUBREAK	0x20
#define MM_SUBMENU	0x40
#ifdef ESCHER
#define MM_INVISIBLE	0x80
#endif

// defaults: 

#define MM_ENABLED	0
#define MM_UNCHECKED	0
#define MM_SID		0

// special ID#'s 

#define idNil		((WORD) 0)
#define idSysMenu	((WORD) -1)

#define cbMTM (sizeof(MTM)-cwExtraMenu*sizeof(WORD))
#define cwMTM (cbMTM/sizeof(WORD))
#define cbMNU (sizeof(MNU)-sizeof(MTM))

#define HmnuFromPmtm(pmtm) ((HMNU) pmtm->rgwExtra[pmtm->cwTotal-1])
#define SzFromPmtm(pmtm) ((CHAR *) &(pmtm->rgwExtra[pmtm->u.iwString]))

#endif // SMM

#endif // !NOMENUS

#ifndef	NOPROCS
BOOL		FARPUBLIC FEnableMenuBar	(BOOL);			/*OPTIONAL*/
VOID		FARPUBLIC EnableMenuItem	(WORD, BOOL);	/*OPTIONAL*/
VOID		FARPUBLIC CheckMenuItem		(WORD, BOOL);	/*OPTIONAL*/
BOOL		FARPUBLIC FMenuItemChecked	(WORD);		 	/*OPTIONAL*/
VOID		FARPUBLIC DrawMenubar		(VOID);			/*OPTIONAL*/
VOID		FARPUBLIC SetMenuKeys		(WORD, WORD);	/*OPTIONAL*/
VOID		FARPUBLIC OpenMenu			(WORD);			/*OPTIONAL*/
VOID		FARPUBLIC DeAltMenu			(VOID);			/*MACROS ONLY*/

#ifndef SMM			// Old functions 

VOID		FARPUBLIC InitMenu			(PWND, PMENUBAR);	/*OPTIONAL*/
PMENUITEM	FARPUBLIC FindMenuItem	(WORD);			 	/*OPTIONAL*/
VOID		FARPUBLIC EnableMenu			(WORD, BOOL);		/*OPTIONAL*/

#else // SMM

// Replacements for above 
VOID		FARPUBLIC InitMenubar	(PWND, HMNU, NPARC, WORD);	/*OPTIONAL*/
PMTM		FARPUBLIC PmtmFindId		(HMNU, WORD, BOOL);		 	/*OPTIONAL*/

// New functions 

VOID	FARPUBLIC OpenMenuPopup		(PWND, HMNU, NPARC, BOOL);	/*OPTIONAL*/
HMNU	FARPUBLIC HmnuNewMenu		(void);            			/*OPTIONAL*/
PMTM	FARPUBLIC PmtmAddItem		(HMNU, WORD, WORD, CHAR *, HMNU, WORD, BYTE);	/*OPTIONAL*/
VOID	FARPUBLIC DeleteItem			(HMNU, WORD);					/*OPTIONAL*/
VOID	FARPUBLIC DeleteMenu			(HMNU, BOOL);       			/*OPTIONAL*/
WORD	FARPUBLIC CchGetMenuString	(HMNU, WORD, CHAR *, WORD);/*OPTIONAL*/
VOID	FARPUBLIC AddAccelTable		(MPVKEYID **);					/*OPTIONAL*/
VOID	FARPUBLIC DeleteAccelTable	(MPVKEYID **);					/*OPTIONAL*/

#ifdef BLADE

BOOL FARPUBLIC ProcessMenuAccel ( VOID * ); // VOID * is of type PMSG
VOID FARPUBLIC SetMenuBarWindow ( PVOID );
PWND FARPUBLIC GetMenuBarWindow ( VOID );

#endif

#define   MS_ENABLE  0x0001
#define   MS_CALCARC 0x0002

WORD FARPUBLIC SizeMenubar			(PWND, HMNU, NPARC, WORD);					/*OPTIONAL*/
VOID FARPUBLIC DrawWinMenubar 	( PWND, VOID **, NPARC, WORD, WORD );	/*OPTIONAL*/
VOID FARPUBLIC EnableWinMenubar 	( PWND, VOID **, NPARC, WORD, WORD );	/*OPTIONAL*/

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

// XOR drawing mode
#define dmXor			0xD00

#endif /*!NOCOLOR*/

extern BYTE PASCAL fMonochrome;		/* TRUE => monochrome screen */
extern char PASCAL chShadow;		/* shadow character */
extern WORD PASCAL diShadow;		/* shadow draw mode, 0 => no shadow */

#ifndef	NOPROCS
VOID		FARPUBLIC SetCursorBlock(BOOL);
VOID		FARPUBLIC SetCursorBlink(BOOL);
VOID		FARPUBLIC RepaintScreen(BOOL);
#endif	/* !NOPROCS */

VOID		FARPUBLIC SetGraphicArc( NPARC, BOOL);



#ifndef NOSUBSTYLES				// Scroll bar constants.
#define	SBS_HORZ			0
#define SBS_VERT			1
#ifdef PROJECT_PWB
#define SBS_TRANSPARENT 		0x10
#endif
#endif

#ifndef NOSCROLL					// Scroll commands.
#define	SB_LINEUP		0
#define	SB_LINEDOWN		1
#define	SB_PAGEUP		2
#define	SB_PAGEDOWN		3
#define	SB_THUMBPOSITION	4
#define	SB_THUMBTRACK	5
#define	SB_TOP			6
#define	SB_BOTTOM		7
#define	SB_ENDSCROLL	8
/* define SB_UPCLICK 15 (private message) */
#endif

#ifndef NOWNDMACROS

DWORD		FARPUBLIC ScrollBarWndProc(PVOID, WORD, WORD, DWORD);

#ifdef CC
#ifndef cwExtraWnd
#define cwExtraWnd 5
#endif /*no extra size*/
#endif /*CC*/

#ifndef BLADE

#ifdef PROJECT_PWB
#define wndScrollBar(id, fVert, fEnabled, ax, ay, dax, day, pwndParent, pwndSibling, ctickRep) { \
	id, WS_CHILD | WS_SCROLL | (fVert ? SBS_VERT : SBS_HORZ), \
	0, fEnabled, 0, {ax, ay, ax+dax, ay+day}, {ax, ay, ax+dax, ay+day}, \
	rrcInvalidStd	\
	(PLFN) ScrollBarWndProc, pwndParent, pwndSibling, NULL, 0, 0, \
	{ctickRep, 0, 0, 1, 1}}

#else

#define wndScrollBar(id, fVert, fEnabled, ax, ay, dax, day, pwndParent, pwndSibling, ctickRep) { \
	id, WS_CHILD | WS_SCROLL | (fVert ? SBS_VERT : SBS_HORZ), \
	0, fEnabled, {ax, ay, ax+dax, ay+day}, {ax, ay, ax+dax, ay+day}, \
	rrcInvalidStd	\
	(PLFN) ScrollBarWndProc, pwndParent, pwndSibling, NULL, 0, 0, \
	{ctickRep, 0, 0, 1, 1}}
#endif

#endif

#endif /* !NOWNDMACROS */

#ifndef	NOPROCS

#ifndef BROADSWORD

short		FARPUBLIC SetScrollPos	(PVOID, short, BOOL);
short		FARPUBLIC GetScrollPos	(PVOID);
VOID		FARPUBLIC SetScrollRange(PVOID, short, short, BOOL);

#else

short		FARPUBLIC SetScrollPos	(PVOID, WORD, short, BOOL);
short		FARPUBLIC GetScrollPos	(PVOID, WORD);
VOID		FARPUBLIC SetScrollRange(PVOID, WORD, short, short, BOOL);
VOID		FARPUBLIC GetScrollRange(PVOID, WORD, short *, short *);

#endif

#endif



/*****************************************************************************/
/* System Specifics */
#ifndef	NOPROCS
VOID		FARPUBLIC DoSound(WORD,WORD);
DWORD		FARPUBLIC ClockTicks(void);
BYTE		FARPUBLIC GetPrinterTimeOut(BYTE);
VOID		FARPUBLIC SetPrinterTimeOut(BYTE,BYTE);
WORD		FARPUBLIC PrinterCmd(BYTE,BYTE,BYTE);
WORD		FARPUBLIC ComCmd(BYTE,BYTE,BYTE);
WORD		FARPUBLIC DtOfChDrive(BYTE);
BOOL		FARPUBLIC FInitSyd(void);
VOID		FARPUBLIC TermSyd(void);
#endif	/* !NOPROCS */

/* DtOfChDrive return values (Note that all valid drive return values must
	be bounded by dtValidMin and dtValidMac and all invalid ones must
	be bounded by dtInvalidMin and dtInvalidMac. */
#define dtValidMin	0x0000
#define dtValidMac	0x0fff
#define dtExist		dtValidMin
#define dtRemoveable	(dtValidMin + 1)
#define dtFixed		(dtValidMin + 2)
#define dtInvalidMin	0x1000
#define dtInvalidMac	0xffff
#define dtNone		dtInvalidMin
#define dtPhantom	(dtInvalidMin + 1)


extern BYTE PASCAL	fSingleFloppy;	/* TRUE => 1 floppy system */
extern BYTE PASCAL	fInt24Error;	/* Set if INT 24 error detected */

/* Sound Facilities */
#ifdef PROJECT_PWB
VOID	FARPUBLIC bell (VOID);	/*OPTIONAL*/
#define Beep()		bell ()
#define Click() 	bell ()
#else
#define	Beep()		DoSound(3,1400)
#define	Click()		DoSound(2,700)
#endif

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

BOOL		FARPUBLIC FSetCurDir(char *);
VOID		FARPUBLIC GetCurDir(char, char *);
char		FARPUBLIC GetCurDrive(void);
VOID		FARPUBLIC SetCurDrive(char);
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



// LszPromptSwapDisk 

#define	rpsdNULL	-1
#define	rpsdNotFound	1
#define	rpsdLocked	2
#define	rpsdLoadError	3


// RspAppIdle 

#define	rspSleep	1			// sleep some more 
#define	rspContinue	2		// check for new events 
#define	rspAbort	3			// abort from the current context 

#define	cnxNull		0		// context is not given 

#define	cnxMenu		1		// context is Menu 
#define	cnxDialog	2		// context is Dialog 
#define	cnxMBox		3		// context is Message Box 

#define	cnxMacro		4		// context is Key Macro library 
#define	cnxVAP		5		// context is VAP interface 

#define	cnxMoveSize	6		// context is overlap window move/size 


#ifndef NOPROCS

CHAR FAR *	FARPUBLIC LszPromptSwapDisk(char far *, WORD, WORD);	/* OPTIONAL */
WORD		FARPUBLIC RspAppIdle(WORD, DWORD);		/* OPTIONAL */
BOOL		FARPUBLIC FHelpMsg(PMSG, WORD);		/* OPTIONAL */
VOID		FARPUBLIC MoveSizeOOM(PWND);

#endif



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
#define	kkposStatus		1
#define	kkposCursor		2

/* Structure to communicate with KID */

#define	cbReservedKkcv		34

typedef struct _kkcv
	{
	WORD		wType;			/* Data type of wAscii */
	WORD		wScan;			/* Key scan code */
	WORD		wAscii;			/* Ascii code */
	WORD		wShift;			/* Shift key status */
	WORD		wExShift;		/* Extended Shift key status */

	WORD		cchResult;		/* Length of Result string */
	CHAR FAR *	lpchResult;		/* Pointer to Result string buffer */

	WORD		cchMode;		/* Length of Mode string */
	CHAR FAR *	lpchMode;		/* Pointer to Mode string buffer */
	CHAR FAR *	lpattrMode;		/* Pointer to Mode attribute buffer */

	WORD		cchSystem;		/* Length of System string */
	CHAR FAR *	lpchSystem;		/* Pointer to System string buffer */
	CHAR FAR *	lpattrSystem;	/* Pointer to System attribute buffer */

	WORD		cchBuf;			/* Length of Display string */
	CHAR FAR *	lpchBuf;		/* Pointer to Display string buffer */
	CHAR FAR *	lpattrBuf;		/* Pointer to Display attribute buffer*/
	WORD		cchBufCursor;	/* Cursor position in Display buffer */                                                                                 
	CHAR		rgbReserved[cbReservedKkcv];
	} KKCV;	/* KK Converter interface structure */


#ifndef NOPROCS
int		FARPUBLIC KKOpen(KKCV FAR *);			/*OPTIONAL*/
int		FARPUBLIC KKClose(VOID);				/*OPTIONAL*/
int		FARPUBLIC KKJoin(KKCV FAR *);			/*OPTIONAL*/
int		FARPUBLIC KKFree(VOID);					/*OPTIONAL*/
int		FARPUBLIC KKInOut(KKCV FAR *);			/*OPTIONAL*/
int		FARPUBLIC KKPosGet(VOID);				/*OPTIONAL*/
int		FARPUBLIC KKPosSet(KKCV FAR *, WORD);	/*OPTIONAL*/
BOOL	FARPUBLIC FKKMode(void);				/*OPTIONAL*/
int		FARPUBLIC KKSetMode(KKCV FAR *, BOOL);	/*OPTIONAL*/
#endif /* !NOPROCS */


#ifdef TRACES_ENABLED

// leave space for null at end 
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

#define LPTAG		TAG  FAR *
#define LSZ			CHAR FAR *

// CW-defined ftags (the rest are for application use) 

#define ftagMaskOn	0x00ff
#define ftagRegistered	0x8000

// data passed to application for trace 
typedef struct _tod
	{
	LPTAG		lptag;
	LSZ		lsz;
	LONG		lParam;
	} TOD;

#ifndef NO_PROCS

VOID FARPUBLIC InitCWTraceTags(VOID);			 /*OPTIONAL*/
PWND FARPUBLIC PwndSetPwndTrace(PWND);			 /*OPTIONAL*/
VOID FARPUBLIC TraceTagRegister(LPTAG, WORD); /*OPTIONAL*/
VOID FARPUBLIC CWTrace(LPTAG, LSZ, LONG);		 /*OPTIONAL*/
VOID FARPUBLIC AppendSzToSz(CHAR *, CHAR **); /*OPTIONAL*/

#endif

// tracing on/off 
extern PWND PASCAL	pwndTrace;

#define FTracingOn()	(pwndTrace != NULL)

#define FTraceTagOn(tag) \
			(FTracingOn() && ((tag).ftag & ftagMaskOn))
#define FTraceTagRegistered(tag) \
			((tag).ftag & ftagRegistered)

// full tracing macros 

// LGT: Trace renamed to CWTraceMacro
#define CWTraceMacro(tag) \
			if (FTraceTagOn(tag)) CWTrace((LPTAG) &(tag), NULL, 0L)

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

// MUST exist only within BeginTraceSz and EndTraceSz 

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

#else	// TRACES_ENABLED 

#define InitCWTraceTags()
#define PwndSetPwndTrace(pwnd)
#define TraceTagRegister(rgtag, ctag)

// LGT: Trace renamed to CWTraceMacro
#define CWTraceMacro(tag)

#define TraceSz(tag, sz)
#define TraceAssert(fCond, tag, sz)
#define TraceWord(tag, wParam, fform)
#define BeginTraceSz(tag)
#define AddSzToTraceSz(sz)
#define AddWordToTraceSz(sz)
#define EndTraceSz(tag)
#define DoBenchMark(tag, stmt)
#define Repeat(stmt, crep)

#endif	// !TRACES_ENABLED 


/* Public variables for kkc filter */

extern BOOL PASCAL fKkcAvailable;	/* TRUE -> kkif drv. initialized */
extern BOOL PASCAL fKkcEnabled;		/* TRUE -> kkc enabled */
extern KKCV PASCAL kkcv;			/* Common area for kid and kkc filter */
extern BOOL PASCAL fKkcMessage;		/* TRUE -> converted string pending */


/* kkc filter mode */

#define	kkmodeNil		0
#define	kkmodeLYR		1		/* layer displays undetermined string */
#define	kkmodeAPP		2		/* ap displays undetermined string */


#ifndef NOPROCS
BOOL FARPUBLIC FInitKkc(PWND);			/*OPTIONAL*/
VOID FARPUBLIC TermKkc(VOID);			  	/*OPTIONAL*/
BOOL FARPUBLIC SetModeKkc(WORD);		  	/*OPTIONAL*/
BOOL FARPUBLIC FSuspendKkc(BOOL);	  	/*OPTIONAL*/
BOOL FARPUBLIC FEnableKkc(BOOL);		  	/*OPTIONAL*/
BOOL FARPUBLIC FActivateKkc(BOOL);	  	/*OPTIONAL*/
BOOL FARPUBLIC FActiveKkc(VOID);		  	/*OPTIONAL*/
BOOL FARPUBLIC FSetPosKkc(WORD);		  	/*OPTIONAL*/
BOOL FARPUBLIC GetPosKkc(VOID);		  	/*OPTIONAL*/
BOOL FARPUBLIC FFlushKkc(VOID);		  	/*OPTIONAL*/
VOID FARPUBLIC SetWindowKkc(PRRC);	  	/*OPTIONAL*/
VOID FARPUBLIC SetCursorKkc(RX, RY);  	/*OPTIONAL*/
VOID FARPUBLIC TextOutAttrX(PWND, RX, RY, PRRC, CHAR FAR *, CHAR FAR *, short, short); 	/*OPTIONAL*/
#endif /* !NOPROCS */


char * FARPUBLIC SzDirSpec			(char *, char *, BOOL *);
VOID 	 FARPUBLIC SetCurrentPath	(char *);
VOID	 FARPUBLIC MakeDirName		(char *, char *);

BOOL	FARPUBLIC  FCorrectDriveDir	(char *);
char * FARPUBLIC SzChopText			(PVOID, char * );
char * FARPUBLIC SzWildCard			( char * );

VOID	FARPUBLIC  DlgDirList		( PVOID, char *, PVOID, BOOL, PVOID );
BOOL	FARPUBLIC  DlgDirSelect		( PVOID, char *, PVOID );
BOOL	FARPUBLIC  FMaybeDir			( char *);

