/*
	CW: Character Oriented Windows
	
	indrv.h: Installable screen driver - driver info
*/

/*****************************************************************************/

/***BEGIN_PUBLIC***/
#define	fmemFixed	1		/* always set !! */
#define	fmemNear	2		/* allocate near */

/* limit of near space / driver service */
#define	cbNearMemServiceMax	128

/* INDT: Driver Service Types */
#define	indtNil			0
#define	indtKeyboard		1			/* .KBD */
#define	indtCharacterScreen	2			/* .CSD */
#define	indtGraphicScreen	3			/* .GSD */
#define	indtCharacterPrinter	4			/* .PRD */
#define	indtGraphicPrinter	5			/* .GPD */
#define	indtSystem		6			/* .SYD */
#define	indtSerialComm		7			/* .SCD */
/* ... others to be defined ... */

typedef struct _indv
	{
	/* input values */
	BYTE	indt;				/* service wanted */
	BYTE	filler;				/* reserved */
	LPFN *	rglpfn;				/* where to put vectors */
	WORD	cpfnNeedMin;			/* # entries needed */
	WORD	cpfnNeedMac;			/* # entries wanted */
	/* return values */
	WORD	psLoaded;			/* != 0 => loaded */
	WORD	cpfnLoaded;			/* # entries loaded */
	} INDV;

WORD	FARPUBLIC RerrLoadDrv(char *, INDV *, WORD);	/* OPTIONAL */
WORD	FARPUBLIC RerrLoadCwDrv(char *);		/* OPTIONAL */

/***END_PUBLIC***/

/*****************************************************************************/
/* installable screen drivers */


#define	szMagicDRV	"DRV0"		/* CW Drivers version #0 */

typedef struct _indh
	{
	char	rgchMagic[4];
	DWORD	dlfaTable;		/* offset from end of file */
	} INDH;		/* driver header (at end of file) */

typedef struct _inds
	{
	BYTE	indt;			/* service type */
	BYTE	fload;			/* load flags */
	WORD	cbCode;			/* size of code (in file) */
	DWORD	dlfaCode;		/* offset from INDH */
	} INDS;		/* driver service */

typedef struct _indt
	{
	char	rgchMagic[4];
	WORD	cinds;				/* # of device services */
	INDS	rginds[1];			/* variable length */
	} INDT;		/* driver table (directory) */


/* FLOAD: load flags */
#define	floadFixed	1			/* must allocated fixed */
#define	floadRealMode	0x10			/* loads under Real Dos */
#define	floadProtMode	0x20			/* loads under OS/2 */
#define	floadAnyMode	(floadRealMode|floadProtMode) /* loads under any OS */
#define	floadStandard	0x80			/* adheres to standard format */

/*****************************************************************************/
/* OS Specific information */


/*
* For OS/2, we provide far pointers several DLL entry points
*/
typedef VOID		(FAR PASCAL *LPFN_DOS)();

typedef	struct _inos
	{
	/* Driver-OS/2 linkage */
	WORD		sdGlis;			/* global info */
	WORD		sdLois;			/* local info */

	WORD		cpfn;			/* # of entries in INOS */

	/* Configuration info */
	LPFN_DOS	lpfnDosGetVersion;	/* DosGetVersion */
	LPFN_DOS	lpfnDosGetEnv;		/* DosGetEnv */
	LPFN_DOS	lpfnDosDevConfig;	/* DosDevConfig */
	LPFN_DOS	lpfnDosGetCtryInfo;	/* DosGetCtryInfo */
	LPFN_DOS	lpfnDosGetDBCSEv;	/* DosGetDBCSEv */
	LPFN_DOS	lpfnDosGetInfoSeg;	/* DosGetInfoSeg */

	/* Low level hardware access */
	LPFN_DOS	lpfnDosDevIOCtl;	/* DosDevIOCtl */
	LPFN_DOS	lpfnDosPortAccess;	/* DosPortAccess */

	/* Linkage to DLL entries (for ones not listed here) */
	LPFN_DOS	lpfnDosGetModHandle;	/* DosGetModHandle */
	LPFN_DOS	lpfnDosLoadModule;	/* DosLoadModule */
	LPFN_DOS	lpfnDosGetProcAddr;	/* DosGetProcAddr */

	/* Threads */
	LPFN_DOS	lpfnDosCreateThread;	/* DosCreateThread */
	LPFN_DOS	lpfnDosSetPrty;		/* DosSetPrty */
	LPFN_DOS	lpfnDosExit;		/* DosExit */

	/* File Routines */
	LPFN_DOS	lpfnDosOpen;		/* DosOpen */
	LPFN_DOS	lpfnDosClose;		/* DosClose */
	LPFN_DOS	lpfnDosRead;		/* DosRead */
	LPFN_DOS	lpfnDosWrite;		/* DosWrite */

	/* Monitor Routines */
	LPFN_DOS	lpfnDosMonOpen;		/* DosMonOpen */
	LPFN_DOS	lpfnDosMonClose;	/* DosMonClose */
	LPFN_DOS	lpfnDosMonReg;		/* DosMonReg */
	LPFN_DOS	lpfnDosMonRead;		/* DosMonRead */
	LPFN_DOS	lpfnDosMonWrite;	/* DosMonWrite */

	/* Memory Allocation */
	LPFN_DOS	lpfnDosAllocSeg;	/* DosAllogSeg */
	LPFN_DOS	lpfnDosReAllocSeg;	/* DosReAllocSeg */
	LPFN_DOS	lpfnDosFreeSeg;		/* DosFreeSeg */

	/* Common VIO Routines */
	LPFN_DOS	lpfnVioGetBuf;		/* VioGetBuf */
	LPFN_DOS	lpfnVioShowBuf;		/* VioShowBuf */
	LPFN_DOS	lpfnVioGetConfig;	/* VioGetConfig */
	LPFN_DOS	lpfnVioGetMode;		/* VioGetMode */
	LPFN_DOS	lpfnVioSetMode;		/* VioSetMode */
	LPFN_DOS	lpfnVioGetState;	/* VioGetState */
	LPFN_DOS	lpfnVioSetState;	/* VioSetState */

	LPFN_DOS	lpfnVioGetCurType;	/* VioGetCurType */
	LPFN_DOS	lpfnVioSetCurType;	/* VioSetCurType */
	LPFN_DOS	lpfnVioGetCurPos;	/* VioGetCurPos */
	LPFN_DOS	lpfnVioSetCurPos;	/* VioSetCurPos */

	LPFN_DOS	lpfnVioGetFont;		/* VioGetFont */
	LPFN_DOS	lpfnVioSetFont;		/* VioSetFont */
	LPFN_DOS	lpfnVioGetCP;		/* VioGetCP */
	LPFN_DOS	lpfnVioSetCP;		/* VioSetCP */

	LPFN_DOS	lpfnVioScrollUp;	/* VioScrollUp */

	/* Common KBD Routines */
	LPFN_DOS	lpfnKbdOpen;		/* KbdOpen */
	LPFN_DOS	lpfnKbdClose;		/* KbdClose */
	LPFN_DOS	lpfnKbdCharIn;		/* KbdCharIn */
	LPFN_DOS	lpfnKbdGetStatus;	/* KbdGetStatus */
	LPFN_DOS	lpfnKbdSetStatus;	/* KbdSetStatus */

	/* Misc */
	LPFN_DOS	lpfnDosBeep;		/* DosBeep */

	LPFN_DOS	lpfnVioGetPhysBuf;	/* VioGetPhysBuf */

	/* special IOPL routines */
	LPFN_DOS	lpfnCwBeginIO;		/* CwBeginIO */
	LPFN_DOS	lpfnCwEndIO;		/* CwEndIO */

	/* more may be added (at the end) in the future */
	} INOS;


/*****************************************************************************/
/* Call Back Services */

typedef	struct _incs
	{
	WORD		cpfn;			/* # of entries in INCS */

#ifdef LATER
	/* to be defined */
#endif /*LATER*/

	} INCS;

/*****************************************************************************/
/* Standard header */

/* When "floadStandard" is specified, this structure should be at the start
	or each service's code segment */

typedef struct _insh
	{
	union
		{
		struct	/* in file representation */
			{
			WORD	cbData;		/* size of data segment */
			WORD	fmem;		/* allocation flags */
			} in_file;
		struct
			{
			WORD FAR * lpwData;	/* far pointer to data */
			} in_mem;
		} lpwData;

	INOS *	pinos;			/* pointer to OS specific info,
						will be 0 for DOS 3 */
	INCS *	pincs;			/* pointer to INCS */

	WORD	cpfn;
	/* followed by an array of NPFN_DRV pointers ("cpfn" of them) */
	} INSH;

typedef WORD	NPFN_DRV; /* offset to near procedure (never use directly) */

/*****************************************************************************/
