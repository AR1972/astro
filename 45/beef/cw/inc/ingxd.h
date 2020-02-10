/*
	CW: Character Windows
	
	ingxd.h: Graphic Drawing (GSD and GPD)
	NOTE: probably missing some GPD related stuff

	for Pcode use (unnamed unions/structs) define NOGD_????
*/

/***BEGIN_PUBLIC***/

/* Primative Graphic Types */

#ifndef NOGD_GPT
typedef struct _gpoint
	{
	int	x;
	int	y;
	} GPT; /* GPOINT / Graphic Point */
#endif /*!NOGD_GPT*/

#ifndef NOGD_RECT
typedef struct _rect
	{
	int	xLeft;
	int	yTop;
	int	xRight;
	int	yBottom;
	} RECT;
#endif /*!NOGD_RECT*/

#ifndef NOGD_POLYGON
typedef struct _polygon
	{
	WORD	cbPolygon;
	RECT	rectBound;
	GPT	rggpt[1];
	} POLYGON;
#endif /*!NOGD_POLYGON*/

#define	cpenMax		41	/* maximum number of pens */
#define	icoMax		41	/* maximum ico number */

/* Device descriptor */
typedef struct _ingd
	{
	WORD	fingd;		/* flags -- see below			*/
	WORD	reserved2;	/* for future use			*/
	WORD	fingpSupported;	/* procedures supported -- see below	*/
	WORD	dimH;		/* horizontal size in twips		*/
	WORD	dimV;		/* vertical size in twips		*/
	WORD	dxScreen;	/* number of horizontal pixels		*/
	WORD	dyScreen;	/* number of vertical pixels		*/
	WORD	dimPenH;	/* horizontal pen size in twips		*/
	WORD	dimPenV;	/* vertical pen size in twips		*/
	BYTE	cpen;		/* number of pens available		*/
	BYTE	icoAvailMac;	/* number of colors available		*/
	BYTE	icoPrefMac;	/* number of preferred colors		*/
	BYTE	ipaLineMac;	/* number of line styles		*/
	BYTE	ipaAreaMac;	/* number of area fill patterns		*/
	BYTE	ccopln;		/* number of color planes or ribbons	*/

	WORD	rgcoAvail[cpenMax];
	WORD	rgcoPref[cpenMax];
	BYTE	rgpaLine[5];
	BYTE	rgpaArea[16];
	char	szName[40];
	} INGD;


/* FINGD : Flags for the options supported in INGD */
#define	fingdRstrVctr	0x0004	/* Raster : MUST BE SET */
#define	fingdRasterFonts 0x0040	/* can use raster fonts			*/
#define	fingdMultiColor	0x0100	/* device has infinite colors		*/
#define	fingdVirtualPen	0x0200	/* do not prompt for pen changes	*/
#define	fingdFilm	0x0400	/* film device				*/
#define	fingdVarPenSize	0x1000	/* device can vary the pen size		*/
#define	fingdNotAvailable 0x8000/* device does not support this DEV	*/

/* FINGP: GSD procedures supported */
#define	fingpSetAreaPat	0x0001	/* set the current area pattern		*/
#define	fingpSetLinePat	0x0002	/* set the current line pattern		*/
#define	fingpSetLineWeight 0x0004 /* set the current line weight	*/
#define	fingpSetColor	0x0008	/* set the current color		*/
#define	fingpText	0x0010	/* draw a text string			*/
#define	fingpRectangle	0x0020	/* draw or fill a rectangle		*/
#define	fingpArc	0x0040	/* draw or fill an arc			*/
#define	fingpPolygon	0x0080	/* draw or fill a polygon		*/
#define	fingpBitBlt	0x0100	/* copy a bitmap			*/

/***END_PUBLIC***/

/*****************************************************************************/
/* CW private info */

/*****************************************************************************/
/* GPD jump vectors */

typedef BOOL		(FAR PASCAL *LPFN_GD_INIT)(VOID *, VOID FAR *);
typedef VOID		(FAR PASCAL *LPFN_GD_TERM)(void);
typedef VOID		(FAR PASCAL *LPFN_GD_XY)(WORD, WORD);
typedef VOID		(FAR PASCAL *LPFN_GD_INDEX)(WORD);
typedef VOID		(FAR PASCAL *LPFN_GD_COLOR)(WORD, WORD);
typedef VOID		(FAR PASCAL *LPFN_GD_TEXT)(char far *, WORD, WORD, WORD, int);
typedef VOID		(FAR PASCAL *LPFN_GD_RECT)(struct _rect far *);
typedef VOID		(FAR PASCAL *LPFN_GD_ARC)(struct _rect far *, int, int);
typedef VOID		(FAR PASCAL *LPFN_GD_POLY)(struct _polygon far *);
typedef VOID		(FAR PASCAL *LPFN_GD_BITBLT)(struct _rect far *, BYTE far *, WORD, BOOL);

typedef struct _indj /* drawing jump vectors */
	{
	LPFN_GD_INIT	pfnFInitGraphics;
	LPFN_GD_TERM	pfnTermGraphics;
	LPFN_GD_XY	pfnMove;
	LPFN_GD_XY	pfnDraw;
	LPFN_GD_INDEX	pfnSetAreaPat;
	LPFN_GD_INDEX	pfnSetLinePat;
	LPFN_GD_INDEX	pfnSetLineWeight;
	LPFN_GD_COLOR	pfnSetColor;
	LPFN_GD_TEXT	pfnText;
	LPFN_GD_RECT	pfnRectangle;
	LPFN_GD_ARC	pfnArc;
	LPFN_GD_POLY	pfnPolygon;
	LPFN_GD_BITBLT	pfnBitBlt;
	} INDJ;

/* indtGraphicScreen/indtGraphicPrinter service */
#define	cpfnGxdMin	13
#define	cpfnGsdMin	cpfnGxdMin
#define	cpfnGpdMin	cpfnGxdMin

/*****************************************************************************/
