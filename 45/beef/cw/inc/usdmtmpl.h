/*
	COW : Character Oriented Windows

	usdmtmpl.h : Template include for SDM

	Normally only compiled with the CS compiler

	* * * * WARNING * * * *
	!! application code should not depend on the contents of this file !!
*/

/***BEGIN_PUBLIC***/

/* -- defined Dialog Item */
typedef struct _sdmtm
	{
	BITS	tmtBase:6;	/* item type */
	BITS	tmsBase:4;	/* sub-type */
	BIT	f1:1;		/* general purpose # 1 */
	BIT	f2:1;		/* general purpose # 2 */
	BIT	f3:1;		/* general purpose # 3 */
	BIT	fDisable:1;	/* if originally disabled */
	BIT	fProc:1;	/* if call dialog proc */
	BIT	fDismiss:1; 	/* if dismiss dialog */

#ifdef CC
	WORD	bpString;	/* based pointer or wParam */
#else
#ifdef SDM_ST
	char	st[];		/* based pointer or wParam */
#else
	char	sz[];		/* based pointer or wParam */
#endif
#endif /*!CC*/

	LONG	l;		/* compact rectangle or PFN */
#ifdef SDM_LONG_COORD
	LONG	l2;
#endif
	} SDMTM;


#ifdef SDM_COW
/* Compact Rectangle */
typedef struct _crc
	{
	BYTE	x, y, dx, dy;		/* order critical */
	} CRC;		/* Compact Rectangle */
#endif

/* Dialog Proc Type */
typedef BOOL	(FARPUBLIC *PFFN_DLG)(WORD, TMC, WORD, WORD, WORD);


/* dialog header */
typedef struct _dlg
	{
#ifdef SDM_COW
	CRC	crcDlg;			/* Compact Rectangle */
#else
	WRC wrcDlg;
	long lStyle;			/* dialog style */
#endif /*!SDM_COW*/

/* title string : based pointer */
#ifdef CC
	WORD bpTitle;			/* based pointer to title */
#else
#ifdef SDM_ST
	char stTitle[];
#else
	char szTitle[];		/* dialog title */
#endif
#endif /*!CC*/

	WORD	hid;			/* dialog help id */

	PFFN_DLG pfnDlg; 		/* dialog function */
	TMC	tmcSelInit;		/* initial selection */
#ifndef CC
	SDMTM	rgtm[];			/* variable length for CS */
#else
#ifdef SDM_ENGINE
	SDMTM	rgtm[1];		/* array starts here */
#endif
#endif /*CC*/
	} DLG;	/* Dialog Template */

/*****************************************************************************/

/* instances of tmtBase  (basic tm types) */
#define	tmtNull			0
#define	tmtEnd			0
#define	tmtStaticText		1
#define	tmtFormattedText	2
#define	tmtTrackingText		3
#define	tmtGroupBox		4

#define	tmtPushButton		5
#define	tmtCheckBox		6
#define	tmtRadioButton		7
#define	tmtToggleBox		8
#define	tmtToggleButton		9
#define	tmtEdit			10
#define	tmtListBox		11
#define	tmtStandardButton	12

#define	tmtDummyText		13		/* dummy static text */
#define	tmtGeneralPicture	14		/* with Render Proc */

#define	tmtNormalMax		15

#define	tmtUserMin		16
/* App / future  extensions (32 types) */
#define	tmtUserMax		48

#define	tmtSpecialMin		48
/* Special Records */
#define	tmtConditional		48

/* Extension Records (still Special) */
#define	tmtExtensionMin		61

#define	tmtExtension1		61
#define	tmtExtension2		62
#define	tmtExtension3		63

#define	tmtSpecialMax		64

#define	tmtMax			64		/* 6 bits */


/***END_PUBLIC***/

/******************************************************************/
/* Information that is tightly bound to the contents of this file */
/******************************************************************/

#ifdef COW

/* The standard TM has many parallel structures which are tightly
	bound to the SDMTM structure !!! */


/* macro to decide if tmt is real or an extension */
#define	FNormalTmt(tmt)		((tmt) < tmtNormalMax)
#define	FExtensionTmt(tmt)	((tmt) >= tmtExtensionMin)
#define	FSpecialTmt(tmt)	((tmt) >= tmtSpecialMin)

/***** ***** ***** NORMAL TMs ***** ***** *****/

#define	bpStringFromCab		((WORD) 1)

/***** special use of general flags *****/
/* when fDismiss is set */
#define	fCabDismiss		f1		/* if Dismiss with CAB */

/* for RadioButtons / ToggleButtons */
#define	fFirstButton		f1		/* if first radio button */

/* for Listboxes */
#define	fComboListBox		f1		/* if Combo */
#define	fDirListBox		f2		/* if Directory */
#define	fSortedListBox		f3		/* if sorted */

/* for PushButtons */
#define fDefaultPushButton	f2		/* if default */

/* for StandardButtons */
#define	fStandardCancel		f3		/* if Cancel else Ok */

/* for Edit Items (Secret Edit Text right now) */
#define	fCharValidated		f1

/* for Static Text, Edit Item (not ipl) */
#define	fNotLeftJustify		f2		/* CENTER or RIGHT */
#define	fNotRightJustify	f3		/* CENTER */

/***** ***** ***** CONDITIONAL TMs ***** ***** *****/

#define	PtmoOfTm(ptm)		((TMO *) (ptm))
#define	FUseSubDialog(ptm, sab)	((sab) & (1 << PtmoOfTm(ptm)->jSab))

typedef struct _tmo
	{
	BITS	tmtBase:6;	/* tmtConditional */
	BITS	jSab:4;		/* bit index to SAB */
	BITS	filler:6;

	WORD	ctmNormal;	/* # of normal TMs to skip */
	WORD	ddxDialog;	/* extra width */
	WORD	ddyDialog;	/* extra height */
	} TMO; /* cOnditional TM */


/***** ***** ***** EXTENSION 1 ***** ***** *****/
/* if extension 1 exists it MUST come right after a normal TM */

#define Ptm1OfTm(ptm)		((TM1 *) (ptm))
#define Ptm1OfTif(ptif)		Ptm1OfTm((ptif)->ptm+1)
/* is there an extension 1 record ?? */
#define	FExtension1(ptif)	(Ptm1OfTif(ptif)->tmtBase == tmtExtension1)
/* assert that item has proc (listbox proc, parse proc, ...) */
#define	AssertExtension1(ptif)	Assert(FExtension1(ptif))

/* Control Procedure */
typedef WORD	(FARPUBLIC *PWFN_CTL)(WORD, char *, WORD, TMC, WORD, WORD);

typedef struct _tm1
	{
	BITS	tmtBase:6;	/* tmtExtension1 */
	BITS	reserved:4;	/* will be item size */
	BITS	filler:6;

	WORD	tmcImport;	/* imported TMC or tmcNull */
	PWFN_CTL pfnCtl;		/* Control Proc */
	} TM1;	/* TM extension #1 */


#endif /*COW*/
