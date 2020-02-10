/*
	CW : Character Windows

	csdmtmpl.h : SDM template header (include for .sdm files only)				CS Compiled only !!

-- Created Mon Dec 16 14:30:51 1991 */ 

#define	SDM_COW		1


/* Compact Rectangle */
typedef struct _crc
	{
	BYTE	x, y, dx, dy;		/* order critical */
	} CRC;		/* Compact Rectangle */

/* -- defined Dialog Item */
typedef struct _tm
	{
	BITS	tmtBase:6;	/* item type */
	BIT	filler:1;	/* fThinBdr */
	BIT	fAction:1;
#ifdef SDM_ENGINE		/*CW's internal representation */
	BIT	f1:1;
	BIT	f2:1;
	BIT	f3:1;
	BIT	f4:1;
	BIT	f5:1;
	BIT	f6:1;
	BIT	f7:1;
	BIT	f8:1;
#else
	BITS	bitSlot:8;	/* misc flags */
#endif /*SDM_ENGINE*/

#ifdef CC
	WORD	bpString;	/* based pointer or wParam */
#else
	char	sz[];		/* string or wParam */
#endif /*!CC*/

	LONG	l;		/* compact rectangle or PFN */
	} TM;


/* Dialog Proc Type */
typedef BOOL	(FARPUBLIC *PFFN_DLG)(WORD, TMC, WORD, WORD, WORD);


/* dialog header */
typedef struct _dlg
	{
	CRC	crcDlg;			/* Compact Rectangle */
	WORD	hid;			/* dialog help id */
	TMC	tmcSelInit;		/* initial selection */
#ifdef VAP_API
	DWORD	pfnDlg; 		/* dialog function */
#else
	PFFN_DLG pfnDlg; 		/* dialog function */
#endif
	WORD	ctmBase;		/* # of base SDMTMs */
	WORD	filler;			/* bdr */

/* title string : based pointer */
#ifdef CC
	WORD bpTitle;			/* based pointer to title */
#else
	char szTitle[];		/* dialog title */
#endif /*!CC*/


#ifndef CC
	TM	rgtm[];			/* variable length for CS */
#else
#ifdef SDM_ENGINE
	TM	rgtm[1];		/* array starts here */
#endif
#endif /*CC*/
	} DLG;	/* Dialog Template */




// dialog header 
typedef struct _dlgitemtemplate
	{
	WORD  wClass;
	WORD  style;
	WORD  ExStyle;
	WORD  wID;
	BYTE  X;
	BYTE  Y;
	BYTE  Width;
	BYTE  Height;
	WORD  wAccelPos;		// Position of accelerator character within string.
	char *szText;
	} DLGITEMTEMPLATE, *PDLGITEMTEMPLATE;



// dialog header 
typedef struct _dlgheadtemplate
	{
	BYTE  X;
	BYTE  Y;
	BYTE  Width;
	BYTE  Height;
	WORD	hid;				// dialog help id 
	TMC	tmcSelInit;		// initial selection 

	BYTE	ctmBase;			// # of dlg items.
	char *szTitle;		// dialog title 

#ifndef CC
	DLGITEMTEMPLATE	rgtm[];			// variable length for CS 
#else
#ifdef SDM_ENGINE
	DLGITEMTEMPLATE	rgtm[1];		// array starts here 
#endif
#endif
	} DLGHEADTEMPLATE, *PDLGHEADTEMPLATE;



/*****************************************************************************/

/* instances of tmtBase  (basic tm types) */
#define	tmtNull			0
#define	tmtEnd			0
#define	tmtStaticText		1
#define	tmtFormattedText	2
#define	tmtGroupBox		3

#define	tmtPushButton		4
#define	tmtCheckBox		5
#define	tmtRadioButton		6


#define	tmtEdit			7
#define	tmtListBox		8

#define	tmtDummyText		9		/* dummy static text */
#define	tmtGeneralPicture	10		/* with Render Proc */

#define	tmtDropList		11			/* drop down list box */

#define	tmtNormalMax		12

#define	tmtUserMin		16
/* App / future  extensions (32 types) */
#define	tmtUserMax		48

#define	tmtSpecialMin		48
/* Special Records */
#define	tmtConditional		48

/* Extension Records (still Special) */
#define	tmtExtensionMin		61

#define	tmtExt1			61
#define	tmtExt2			62

#define	tmtSpecialMax		64

#define	tmtMax			64		/* 6 bits */


