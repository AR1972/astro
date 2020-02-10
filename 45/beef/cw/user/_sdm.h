/*
	_sdm.h : Really local SDM info
*/


/* State Of SDM Engine */

typedef struct _sds
	{
	WORD	ctif;			/* count of TIFs */
	PTIF	rgtif;			/* pointer to array of TIF's */
	PWND	pwndDialog;		/* window of dialog */
	HCAB	hcab;			/* original CAB */
	PDLG	pdlg;			/* start of template */
	BYTE *	pbWorkBuffer;
	} SDS;	/* SDM state */

/* macros to mask access to global state */
#define FExistsDialogProc()	(sdsCur.pdlg->pfnDlg != NULL)
#define	FCallDialogProc(dlm, tmc, wNew, wOld, wParam)	\
	(*sdsCur.pdlg->pfnDlg)(dlm, tmc, wNew, wOld, wParam)

extern SDS PASCAL sdsCur;

/* SDM template initialization */

typedef struct _tci
	{
	WORD	cbWnd;		/* # of bytes for the window */
	WORD	cbAdditional;	/* # additional bytes or cbRect */
	WORD	style;		/* normal style */
	} TCI;	/* iTem Creation Info */


/* make a TCI item with extra info */
#define TciMake(cwExtra, cwAdditional, style)	\
	{ (cwWnd + (cwExtra)) * sizeof(WORD), (cwAdditional) * sizeof(WORD),\
	  (style) }

/* the special "rect" value means calculate additional size from rectangle */
#define	cwRect	((WORD) -1)
#define	cbRect	((WORD) -2)


#define	tmtDialog	tmtNull
#define	tmtScroll	(tmtNormalMax+0)

/* Special masking to get stuff from normal TM */

#define	PcrcOfTm(ptm)		((CRC *) &(ptm)->l)
