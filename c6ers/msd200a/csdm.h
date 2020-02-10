/*
	CW : Character Windows

	csdm.h : SDM global exports
-- Created Mon Dec 16 14:30:09 1991 */ 

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
#define	ftmcGrouped	0x8000		/* OR'd to specify whole group
					  as opposed to first button */
#define	tmcUserMin	tmcSysMax
#define	tmcUserMax	((TMC) ftmcGrouped)



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

#define	dlmTerm 	7	/* termination request for one of many reasons */
#define	dlmIdle		8	/* idle for dialogs */

#define	dlmUnclick	9	/* item unclicked */

#define	dlmClientMouse	10	/* mouse click within dialog box */
#define	dlmNonClientMouse	11	/* mouse click outside dialog box */

#define	dlmSuspend	12	/* dialog suspended pending subdialog */
#define	dlmResume	13	/* dialog resumed */
#define	dlmRepaint	14
#define	dlmExit	15	/* the dialog is exiting (last dlm before termination) */

#define	dlmUserMin	16	/* for user extensions */


/* ListboxProc (pfnTm) messages */
#define	tmmCount	0	/* return number of items in listbox */
#define	tmmText 	1	/* return text associated with n'th item in listbox */
#define	tmmEditText	2	/* like msgText for Combo listboxes */

#define	cszUnknown	((WORD) -1)	/* return to tmmCount if unknown */

#define bNoArg		((WORD) -1)	/* put in bArg field if not used */

/* ParseProc (pfnTm) messages */
#define	tmmFormat	0	/* format data */
#define	tmmParse	1	/* parse data */
#define	tmmCwVal	2	/* return size of data in words */

/* RenderProc (pfnTm) messages */
#define	tmmPaint	0	/* paint yourself */


#define	tmmUserMin	16	/* for user extensions */

/* Control Procedure */
typedef WORD	(FARPUBLIC *PWFN_CTL)(WORD, char *, WORD, TMC, WORD, WORD);


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

#ifndef	NOPROCS
#ifdef SDM_3X
VOID	FARPUBLIC GetDlgArc(PDLG, NPARC);		/*OPTIONAL*/
#else
VOID	FARPUBLIC GetDlgArc(PDLG, PARC);		/*OPTIONAL*/
#endif

TMC	FARPUBLIC TmcDoDlg(VOID *, HCAB);/*OPTIONAL*/
TMC	FARPUBLIC TmcDoDlgAxAy(VOID *, HCAB, AX, AY);	/*OPTIONAL*/
HCAB	FARPUBLIC HcabAlloc(WORD);				/*OPTIONAL*/
VOID	FARPUBLIC FreeCab(HCAB);				/*OPTIONAL*/
VOID	FARPUBLIC FreeCabData(HCAB);			/*OPTIONAL*/
VOID	FARPUBLIC SzToCab(HCAB, char *, WORD);/*OPTIONAL*/
char *FARPUBLIC SzFromCab(HCAB, char *, WORD, WORD);/*OPTIONAL*/
VOID	FARPUBLIC PszToCab(HCAB, char **, WORD);	/*OPTIONAL*/

VOID	FARPUBLIC EnableTmc(TMC, BOOL);/*OPTIONAL*/
BOOL	FARPUBLIC FEnabledTmc(TMC);	 /*OPTIONAL*/
VOID	FARPUBLIC SetTmcVal(TMC, WORD);/*OPTIONAL*/
WORD	FARPUBLIC GetTmcVal(TMC);		 /*OPTIONAL*/
VOID	FARPUBLIC SetTmcSel(TMC, WORD, WORD);/*OPTIONAL*/
VOID	FARPUBLIC GetTmcText(TMC, char *, WORD);/*OPTIONAL*/
VOID	FARPUBLIC SetTmcText(TMC, char *);		 /*OPTIONAL*/
VOID	FARPUBLIC SetFocusTmc(TMC);				 /*OPTIONAL*/
VOID	FARPUBLIC RedisplayListBox(TMC);			 /*OPTIONAL*/
VOID	FARPUBLIC RedisplayListBoxOriented(TMC, WORD *, WORD *); /*OPTIONAL*/
VOID	FARPUBLIC GetTmcListBoxOrientation(TMC, WORD *, WORD *); /*OPTIONAL*/
VOID	FARPUBLIC GetListBoxEntry(TMC, WORD, char *, WORD);	/*OPTIONAL*/
VOID	FARPUBLIC RedisplayTmc(TMC);/*OPTIONAL*/
VOID	FARPUBLIC SetDefaultTmc(TMC);/*OPTIONAL*/
VOID	FARPUBLIC SzFromInt(char *, int);/*OPTIONAL*/
WORD	FARPUBLIC ParseInt(WORD, char *, HANDLE, TMC, WORD, WORD); /*OPTIONAL*/
VOID	FARPUBLIC SetTmcListWidth(TMC, WORD);		/*OPTIONAL*/
VOID	FARPUBLIC SetTmcEditWidth(TMC, WORD);		/*OPTIONAL*/

VOID	FARPUBLIC InitCab(HCAB, WORD);	/*OPTIONAL*/
VOID	FARPUBLIC ReinitCab(HCAB, WORD);	/*OPTIONAL*/

PWND	FARPUBLIC PwndOfTmc(TMC);			/*OPTIONAL*/

TMC	FARPUBLIC TmcDoDlgExt(VOID *, HCAB, WORD, DWORD);	/* OPTIONAL */

#endif	/* !NOPROCS */

#endif /*CC*/

#define	PwndOfListbox(tmc)	PwndOfTmc(tmc)
#define	TmcListBoxResetcontent(tmc)	\
	SendMessage(PwndOfListbox(tmc), LB_RESETCONTENT, 0, 0L)
#define	TmcListBoxAddstring(tmc,isa,sz,fRedraw)	\
	SendMessage(PwndOfListbox(tmc), LB_ADDSTRING, (WORD) sz, MAKELONG(fRedraw,isa))
#define	TmcListBoxInsertstring(tmc,isaT,szT,isz,fRedraw)	\
	{						\
	SZI	szi;					\
							\
	szi.sz = szT;					\
	szi.isa = isaT;					\
	SendMessage(PwndOfListbox(tmc), LB_INSERTSTRING,(WORD)&szi, MAKELONG(fRedraw,isz)); \
	}
#define	TmcListBoxDeletestring(tmc,isz,fRedraw)	\
	SendMessage(PwndOfListbox(tmc), LB_DELETESTRING, 0, MAKELONG(fRedraw,isz))
#define	TmcListBoxReplacestring(tmc,isa,sz,isz)	\
	SendMessage(PwndOfListbox(tmc), LB_REPLACESTRING, (WORD) sz, MAKELONG(isa,isz))
#define TmcListBoxRepaint(tmc)	\
	SendMessage(PwndOfListbox(tmc), WM_PAINT, 0, 0L)
#define TmcGetClientRrc(tmc,prrc)	\
	GetClientRrc(PwndOfListbox(tmc), prrc)
#define CentryListBoxTmc(tmc)	\
	((WORD)SendMessage(PwndOfListbox(tmc), LB_GETCOUNT, 0, 0L))

// Out Of Memory Support. 
typedef WORD		SEV;
#define sevMinor	1		// Minor (painting) error.
					// don't cast, since used in MASM
#define sevMajor	((SEV)2)	// Major error.
#define sevLmem		((SEV)3)	// Out of LMEM memory.
#define sevHcabFromDlg	((SEV)4)	// HcabFromDlg() failure.
#define sevDirFill	((SEV)5)	// Directory fill failed.
#define sevList		((SEV)6)	// Non-directory ListBox fill failure.

