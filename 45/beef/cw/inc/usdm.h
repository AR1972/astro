/*
	COW : Character Oriented Windows

	usdm.h : Definitions for SDM
*/


/***BEGIN_PUBLIC***/

typedef WORD TMC;		/* iTeM Codes */
typedef WORD DLM;		/* Dialog Messages */
typedef WORD TMM;		/* Control proc Messages */

/* standard item codes (tmc) */
#define	tmcNull		((TMC) 0)
#define	tmcOK		((TMC) 1)
#define	tmcOk		tmcOK
#define	tmcCancel	((TMC) 2)
#define	tmcSysMin	((TMC) 0x10)
#ifdef HELP_BUTTON
#define tmcHelp 	((TMC) tmcSysMin-1)
#endif
#define	tmcSysMax	((TMC) 0x100)
#define	tmcGrouped	0x8000		/* OR'd to specify whole group
					  as opposed to first button */
#define	tmcUserMin	tmcSysMax
#define	tmcUserMax	tmcGrouped



/* special ninch (No Input, No CHange) values */
#define	wNinch		(-32767)	/* ints */
#define	uNinch		(0xffff)	/* unsigned */
#define	iszNinchList	uNinch		/* listboxes */
#define	uNinchList	uNinch		/* other name */
#define	uNinchRadio	uNinch		/* radio groups */
#define	uNinchCheck	2		/* check boxes */
#define	wNinchCheck	uNinchCheck	/* old name */

/* special parse error values */
#define	wError		(-32766)	/* ints */
#define	uError		(0xfffe)	/* unsigneds */


/* command argument block header */
typedef struct _cabh
	{
	WORD	cwData;			/* total size of CAB less CABH */
	WORD	cagHandle;		/* # of handles */
	} CABH;

typedef void **HCAB;		/* CAB is just a concept, no real type */
typedef	void *PCAB;		/* pointer to CAB */

typedef	struct _dlg *PDLG;	/* Dialog pointer */

/* minimum CAB size : header + SAB */
#define	cwCabMin	((sizeof(CABH) + sizeof(WORD)) / sizeof(WORD))

/* Dialog Messages */
#define	dlmInit 	0	/* do custom initialization */
#define	dlmClick	1	/* item clicked */
#define	dlmChange	2	/* edit control may have changed */
#define	dlmKey		3	/* any untrapped key */
#define	dlmDblClk	4	/* double click in listbox/radio */

#define	dlmSetFocus	5	/* set focus */
#define	dlmKillFocus	6	/* lose focus */

#define	dlmTerm 	7	/* termination for one of many reasons */
#define	dlmIdle		8	/* idle for dialogs */

#define	dlmUnclick	9	/* item unclicked */

#define	dlmUserMin	16	/* for user extensions */


/* ListboxProc (pfnTm) messages */
#define	tmmCount	0	/* return number of items in listbox */
#define	tmmText 	1	/* return text associated with n'th item in listbox */
#define	tmmEditText	2	/* like msgText for Combo listboxes */

#define	cszUnknown	((WORD) -1)	/* return to tmmCount if unknown */

/* ParseProc (pfnTm) messages */
#define	tmmFormat	0	/* format data */
#define	tmmParse	1	/* parse data */
#define	tmmCwVal	2	/* return size of data in words */

/* RenderProc (pfnTm) messages */
#define	tmmPaint	0	/* paint yourself */


#define	tmmUserMin	16	/* for user extensions */


/* default no help */
#define	hidDlgNull	0	/* for no help */


/* macro to get void pointer to general CAB arg */
#define	PvParseArg(hObj, bArg) ((VOID *) (*((WORD *)(hObj)) + (bArg)))


/* Iag macro
   returns iag corresponding to field fld in application structure str
*/
#define Iag(str, fld)	((WORD)((int)&(((str *)0)->fld) / sizeof(int) - cwCabMin))


/* Globally Exported Function Definitions */
#ifdef CC

TMC		FARPUBLIC TmcDoDlg(VOID *, HCAB);
HCAB		FARPUBLIC HcabAlloc(WORD);
VOID		FARPUBLIC FreeCab(HCAB);
VOID		FARPUBLIC FreeCabData(HCAB);
VOID		FARPUBLIC SzToCab(HCAB, char *, WORD);
char *		FARPUBLIC SzFromCab(HCAB, char *, WORD, WORD);
VOID		FARPUBLIC PszToCab(HCAB, char **, WORD);	/*OPTIONAL*/

VOID		FARPUBLIC EnableTmc(TMC, BOOL);
BOOL		FARPUBLIC FEnabledTmc(TMC);
VOID		FARPUBLIC SetTmcVal(TMC, WORD);
WORD		FARPUBLIC GetTmcVal(TMC);
VOID		FARPUBLIC SetTmcSel(TMC, WORD, WORD);
VOID		FARPUBLIC GetTmcText(TMC, char *, WORD);
VOID		FARPUBLIC SetTmcText(TMC, char *);
VOID		FARPUBLIC SetFocusTmc(TMC);
VOID		FARPUBLIC RedisplayListBox(TMC);
VOID		FARPUBLIC RedisplayListBoxOriented(TMC, WORD *, WORD *); /*OPTIONAL*/
VOID		FARPUBLIC GetTmcListBoxOrientation(TMC, WORD *, WORD *); /*OPTIONAL*/
VOID		FARPUBLIC RedisplayTmc(TMC);
VOID		FARPUBLIC SetDefaultTmc(TMC);
WORD		FARPUBLIC ParseInt(WORD, char *, HANDLE, TMC, WORD, WORD); /*OPTIONAL*/
VOID		FARPUBLIC SetTmcListWidth(TMC, WORD);		/*OPTIONAL*/
VOID		FARPUBLIC SetTmcEditWidth(TMC, WORD);		/*OPTIONAL*/

VOID		FARPUBLIC InitCab(HCAB, WORD);
VOID		FARPUBLIC ReinitCab(HCAB, WORD);

PWND		FARPUBLIC PwndOfListbox(TMC);
#define	TmcListBoxResetcontent(tmc)	\
	SendMessage(PwndOfListbox(tmc), LB_RESETCONTENT, 0, 0L)
#define	TmcListBoxAddstring(tmc,sz,fRedraw)	\
	SendMessage(PwndOfListbox(tmc), LB_ADDSTRING, (WORD) sz, MAKELONG(fRedraw,0))
#define	TmcListBoxInsertstring(tmc,sz,isz,fRedraw)	\
	SendMessage(PwndOfListbox(tmc), LB_INSERTSTRING,(WORD)sz, MAKELONG(fRedraw,isz))
#define	TmcListBoxDeletestring(tmc,isz,fRedraw)	\
	SendMessage(PwndOfListbox(tmc), LB_DELETESTRING, 0, MAKELONG(fRedraw,isz))
#define	TmcListBoxReplacestring(tmc,sz,isz)	\
	SendMessage(PwndOfListbox(tmc), LB_REPLACESTRING, (WORD) sz, MAKELONG(0,isz))
#define TmcListBoxRepaint(tmc)	\
	SendMessage(PwndOfListbox(tmc), WM_PAINT, 0, 0L)
#define TmcGetClientRrc(tmc,prrc)	\
	GetClientRrc(PwndOfListbox(tmc), prrc)

#endif /*CC*/

/***END_PUBLIC***/

/* locking not allowed */
PCAB		FARPUBLIC PcabLockCab(HCAB);
VOID		FARPUBLIC UnlockCab(HCAB);
/* should not be needed : take out later ??? */
VOID		FARPUBLIC StToCab(HCAB, char *, WORD);
VOID		FARPUBLIC RgbToCab(HCAB, BYTE *, WORD, WORD);
char *		FARPUBLIC StFromCab(HCAB, char *, WORD, WORD);
BYTE *		FARPUBLIC RgbFromCab(HCAB, BYTE *, WORD, WORD);

#ifdef COW

/******************************************************************/
/* Information that is tightly bound to the contents of this file */
/******************************************************************/


/* CABX : primative CAB type */

typedef struct _cabx
	{
	CABH	cabh;			/* header */
	WORD	sab;			/* sub dialog word - always present */
	HANDLE	rgh[1];			/* where handles start */
	} CABX; /* NOTE : never use sizeof(CABX) */

#define	cbCabOverhead	(sizeof(CABX) - sizeof(HANDLE))

typedef CABX	*PCABX;
typedef CABX	**HCABX;

/* macro to get temporary pointer to CABX from a HCAB */
#define PcabxOfCab(hcab)	((CABX *)(*(hcab)))
/* macro to get handle (usually a psz) */
#define PpvFromCab(hcab, iag) ((VOID **)(PcabxOfCab(hcab)->rgh[iag]))

#endif /*COW*/
