/*
	CW: Character Windows

	inscr.h: CSD Driver specifics
*/

/***BEGIN_PUBLIC***/

/*****************************************************************************/
/* Extra Font info */

typedef struct _inft
	{
	/* character/font size */
	BYTE	dxChar; 		/* width of character in pixels */
	BYTE	dyChar; 		/* height of character in pixels */

	BYTE	dyBaseLine;		/* base line height */
	BYTE	ifont;			/* font index */
	} INFT;

/*****************************************************************************/
/* Installable state */

typedef WORD	FINST;
#define finstText			0x0001	/* text mode */
#define finstGraphics		0x0002	/* graphics mode */
#define finstMonochrome 0x0004	/* monochrome mode */
#define finstAlternate		0x0008	/* alternate adapter (2nd screen) */
#define finstFont			0x0010	/* supports fonts */
#define finstAttrFont		0x0020	/* map Font calls into attribute info */
#define finstExtendedMono	0x0040	/* EGA and VGA mono */
#define finstDisableMouse	0x1000	/* gfx w/o mouse support (hercules,os/2) */
#define finstFastScroll 0x2000	/* fast scroll (BltArc) for gfx text */
#define finstQuestionable	0x4000	/* questionable mode selection */
#define finstAvailable		0x8000	/* available with current hardware */

typedef struct _inst
	{
	FINST	finst;
	BYTE	axMac;
	BYTE	ayMac;

	/* color palette info */
	BYTE	coMac;			/* maximum color index */
	BYTE	covMac; 		/* maximum color value (palette) */
	WORD	coiMac; 		/* maximum color intensity */

	/* Extra information */
	WORD	imode;			/* video mode */

	INFT	inft;			/* font info (pointers may be NULL) */
	WORD	ffontSupported; /* valid ffont values */

	/* buffers (if non-zero then do not need to be allocated */
	WORD	psPrim; 		/* primary screen buffer */
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
#define imodeUnknown	0xffff	/* unknown mode */

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
	char	_chUpDownArrow; 		/* double arrow */
	char	_chLeftRightArrow;		/* double arrow */

	WORD	reserved[16];
	} INCH;

/*****************************************************************************/
/* font variations */

#define ffontNormal		0x0000	/* normal attributes */

#define ffontUnderline		0x0001
#define ffontDoubleUnderline	0x0002
#define ffontOrUnderline	0x0003	/* ... underline */
#define ffontStrikeThrough	0x0004	/* horizontal strike through */
#define ffontBold		0x0008
#define ffontSubscript		0x0010
#define ffontSuperscript	0x0020
#define ffontMiniCap		0x0030	/* mini capital */
#define ffontItalic		0x0040
#define ffontOrCharacter	0x0080	/* extra field => font extension */
#define ffontReservedBits	0x0f00
#define ffontExtraMask		0xf000	/* one of 16 extra items */
#define ffontOrUnderlineSupport 0x1000

/*****************************************************************************/
/* screen save info */

typedef WORD	FVIDS;			/* flags */
#define fvidsChAttr	1		/* video buffer is in form ch:attr */

typedef struct _vids
	{
	BYTE	mode;			/* screen mode */
	BYTE	page;			/* page # */
	FVIDS	fvids;			/* flags for saved state */
	WORD	cwVidData;		/* # of bytes of screen data */
	WORD	cwExtra;		/* # of extra bytes for mode info */
	WORD	rgwExtra[1];		/* actually rgwExtra[cwExtra] */
	} VIDS; /* Video state */

/*****************************************************************************/

/***END_PUBLIC***/

/*****************************************************************************/
/* indtCharacterScreen service : loaded representation */

/*
  -- the INSJ structure contains far pointers to routines in an installable
	driver
*/

/* Prototypes for functions */
typedef WORD	(FAR PASCAL *LPFN_SC_CUR)(VOID);
typedef BOOL	(FAR PASCAL *LPFN_SC_QRY)(struct _inst *, WORD);
typedef BOOL	(FAR PASCAL *LPFN_SC_INIT)(struct _inst *, struct _inch *);
typedef VOID	(FAR PASCAL *LPFN_SC_TERM)(VOID);
typedef VOID	(FAR PASCAL *LPFN_SC_CURS)(AX, AY, BOOL);
typedef BOOL	(FAR PASCAL *LPFN_SC_GETPAL)(WORD, WORD *, DWORD *);
typedef VOID	(FAR PASCAL *LPFN_SC_SETPAL)(WORD, WORD, DWORD);

typedef VOID	(FAR PASCAL *LPFN_SC_UPDATE1)(AY, AX, BYTE, WORD, BOOL);
typedef VOID	(FAR PASCAL *LPFN_SC_UPDATE2)(VOID);
typedef VOID	(FAR PASCAL *LPFN_SC_UPDATE3)(VOID);	/* to be defined */

typedef WORD	(FAR PASCAL *LPFN_SC_CBVIDS)(VIDS *, INST *);
typedef WORD	(FAR PASCAL *LPFN_SC_SAVE1)(VIDS *);
typedef WORD	(FAR PASCAL *LPFN_SC_REST1)(VIDS *);
typedef WORD	(FAR PASCAL *LPFN_SC_SAVE2)(VIDS *, WORD FAR *);
typedef WORD	(FAR PASCAL *LPFN_SC_REST2)(VIDS *, WORD FAR *);
typedef VOID	(FAR PASCAL *LPFN_SC_MONITOR)(BOOL);

typedef BOOL	(FAR PASCAL *LPFN_SC_FQINFT)(struct _inft *, WORD);

typedef VOID	(FAR PASCAL *LPFN_SC_BLT)(AX, AY, BYTE, BYTE, AX, AY);

typedef VOID	(FAR PASCAL *LPFN_SC_GCM)(struct _inft *, BYTE, BYTE *);

#ifdef FL_TAIWAN
typedef VOID	(FAR PASCAL *LPFN_SC_END)(VOID);/* add on Chinese CMEX */
#endif

typedef struct _insj
	{
	/* Init */
	LPFN_SC_CUR	lpfnImodeGuessCurrentCsd;
	LPFN_SC_QRY	lpfnFQueryInstCsd;
	LPFN_SC_INIT	lpfnFInitCsd;
	LPFN_SC_TERM	lpfnTermCsd;

	LPFN_SC_CURS	lpfnMoveHwCursCsd;
	LPFN_SC_FQINFT	lpfnFQueryInftCsd;

	/* Color palette */
	LPFN_SC_GETPAL	lpfnFGetColorPaletteCsd;
	LPFN_SC_SETPAL	lpfnSetColorPaletteCsd;

	/* Screen update */
	LPFN_SC_UPDATE1 lpfnPrepUpdateCsd;
	LPFN_SC_UPDATE1 lpfnDoUpdateCsd;
	LPFN_SC_UPDATE2 lpfnDoneUpdateCsd;
	LPFN_SC_UPDATE3 lpfnSpecialUpdateCsd;

	/* Screen save/restore */
	LPFN_SC_CBVIDS	lpfnCbSizeVidsCsd;
	LPFN_SC_SAVE1	lpfnFSaveVidsCsd;
	LPFN_SC_REST1	lpfnFRestoreVidsCsd;
	LPFN_SC_SAVE2	lpfnSaveVidDataCsd;
	LPFN_SC_REST2	lpfnRestoreVidDataCsd;
	LPFN_SC_MONITOR lpfnEnableVidsMonitorCsd;

	/* block move for graphics text modes */
	LPFN_SC_BLT	lpfnBltArcCsd;

	/* return character bit map */
	LPFN_SC_GCM	lpfnGetCharMapCsd;

#ifdef FL_TAIWAN
	/* end of cow for Chinese CMEX function */
	LPFN_SC_END	lpfnEndCowCsd;
#endif
	} INSJ; /* Installable screen jump table */

#ifdef FL_TAIWAN
#define cpfnCsdMin	21
#else
#define cpfnCsdMin      20
#endif

/*****************************************************************************/
