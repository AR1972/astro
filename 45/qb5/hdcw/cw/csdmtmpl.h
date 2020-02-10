/*
	CW : Character Windows

	csdmtmpl.h : SDM template header (include for .sdm files only)				CS Compiled only !!

-- Created Mon Aug 29 22:44:57 1988 */ 

#define	SDM_COW		1
/* #define SDM_ST */
/* #define SDM_LOCK */


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


