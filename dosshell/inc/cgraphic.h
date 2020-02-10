;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*
	CW : Character Windows
	cgraphic.h : Graphic Drawing

	Yes, COW not only does Windows, but it does graphics too

	CC should be defined for compilation with Cmerge.

-- Created Fri Mar 23 11:35:04 1990 */ 

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
#define	fingpInitGraphics 0x0200 /* Init/Term/Move			*/
#define	fingpDraw	0x0400	/* draw					*/
#define	fingpSnapShot	0x0800	/* Capture TSR support		*/
#define	fingpBltBlock	0x1000	/* move a block	*/
#define	fingpSetDrawMode	0x2000	/* set the current draw mode	*/


extern BOOL PASCAL fPrinting;

/*****************************************************************************/
/* Graphic Drawing Procedures */

#define	FLoadGsd(sz)	(RerrLoadGsd(sz) == rerrOk)
#define	FLoadGpd(sz)	(RerrLoadGpd(sz) == rerrOk)

#ifndef	NOPROCS
WORD	FARPUBLIC RerrLoadGsd(char *);
WORD	FARPUBLIC RerrLoadGpd(char *);
VOID	FARPUBLIC FreeGsd(void);
VOID	FARPUBLIC FreeGpd(void);
VOID	FARPUBLIC SetPrinting(BOOL);

BOOL	FARPUBLIC FInitGraphics(VOID *, VOID FAR *);
VOID	FARPUBLIC TermGraphics(void);
VOID	FARPUBLIC Move(WORD, WORD);
VOID	FARPUBLIC Draw(WORD, WORD);
VOID	FARPUBLIC SetAreaPat(WORD);
VOID	FARPUBLIC SetLinePat(WORD);
VOID	FARPUBLIC SetLineWeight(WORD);
VOID	FARPUBLIC SetColor(WORD, WORD);
VOID	FARPUBLIC Text(char far *, WORD, WORD, WORD, int);
VOID	FARPUBLIC Rectangle(struct _rect far *);
VOID	FARPUBLIC Arc(struct _rect far *, int, int);
VOID	FARPUBLIC Polygon(struct _polygon far *);
VOID	FARPUBLIC BitBlt(struct _rect far *, BYTE far *, WORD, BOOL);
VOID	FARPUBLIC DrawXOR(WORD, WORD);		/* Capture TSR support */
VOID	FARPUBLIC BitRead(struct _rect far *, BYTE far *, WORD);
VOID	FARPUBLIC SetDrawMode(WORD);
VOID	FARPUBLIC BltBlock(WORD,WORD, WORD,WORD, WORD,WORD);

#ifdef KANJI
/* CPD Functions */
// REVIEW: to be documented and properly supported !!!
WORD	FARPUBLIC RerrLoadCpd(char *);
VOID	FARPUBLIC FreeCpd(void);
WORD	FARPUBLIC InitCpd(CPD_STRUCTURE far *);
WORD	FARPUBLIC TermCpd(void);
WORD	FARPUBLIC AdvVertCpd(WORD);
WORD	FARPUBLIC AdvHorizCpd(WORD);
WORD	FARPUBLIC AdvPageCpd(void);
WORD	FARPUBLIC PrintRgchCpd(CPD_STRUCTURE far *);
WORD	FARPUBLIC PrintHdrCpd(CPD_STRUCTURE far *);
WORD	FARPUBLIC CrLfCpd(void);
#endif	/*KANJI*/
#endif	/* !NOPROCS */

