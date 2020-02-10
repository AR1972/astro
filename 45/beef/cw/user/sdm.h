/*
	CW : Character Windows

	sdm.h : sdm specific stuff
*/

typedef WORD	SB;
#include <lmem.h>		/* requires LMEM for local allocations */

/* ODD pwnds don't really exist */
#define	FFakeWnd(pwnd)	((int) (pwnd) & 1)
#define	FRealWnd(pwnd)	(((int) (pwnd) & 1) == 0)


#define ichTextMax 256    /* size for string buffers */

typedef struct _sdmtm	*PTM;		/* generic TM */

/* CAB Arg info */
#define	iagNil		((WORD) -1)	/* if no CAB arg */


/*
  TIF's : are stored in a global array (sdsCur.rgtif) with size sdsCur.ctif
  mapping from tmc->tif is performed in the following manner :
	1) if tmc <= tmcSysMin && tmc < tmcSysMax then rgtif[tmc-tmcSysMin]
		must contain the tmc (if it is defined).
	2) if tmc < tmcSysMin or tmc >= tmcSysMax (i.e. a special or imported
		TMC), then the array must be scanned.
  NOTE : there is always a guard TIF at the end of rgtif (ptm == NULL,
							tmc == tmcNull).
*/
  
/* if tmc is in tmcSys range */
#define ItifOfTmc(tmc)		((tmc) - tmcSysMin)

typedef struct _tif
	{
	TMC	tmc;
	PTM	ptm;			/* start of extension records */
	WORD	iagText;		/* CAB entry for text from CAB */
	WORD	iagMain;		/* index to main CAB arg */
	PWND	pwnd;			/* pointer to window */
	BIT	fReal:1;		/* a real window ?? */
	} TIF;	/* iTem InFo */

typedef TIF	*PTIF;			/* pointer to a TIF */


PRIVATE TIF * 	PtifFromTmc(TMC);
PRIVATE WORD	GetRadVal(PTIF);
PRIVATE VOID	FillListBoxTif(PTIF, WORD);
PRIVATE VOID	KillDefaultButton(VOID);

/* generic heap procedure equivalents */
#define	sbHeap		1
#define HeapAlloc(cb)		PpvAllocCb(sbHeap, cb)
#define HeapFree(h)		FreePpv(sbHeap, (VOID **) (h))
#define CwFromCch(cch)	(((cch) + 1) / 2)

#define cwWnd (sizeof(WND)/sizeof(WORD) - cwExtraWnd)


#ifdef EDIT_LIMIT_SIZE
/* Limit size for QB */
#define cwEditBuffer 64 			/* 128 char limit */
#else
#define cwEditBuffer 128			/* 256 char limit */
#endif /*!EDIT_LIMIT_SIZE*/

#define Alert(sz) \
	MessageBox(sz, NULL, NULL, MB_OK)

/* append sz2 to end of sz1 */
#define SzAppendSDM(sz1, sz2) \
	fstrcpy((char far *) sz1+strlen(sz1), (char far *) sz2)
