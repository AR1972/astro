/***************************************************************************\
*
* Module Name: BSEDOS.H
*
* OS/2 Base Include File
*
* This file is compatible with OS/2 version 1.0.
*
* Copyright (c) 1988  Microsoft Corporation
*
*****************************************************************************
*
* Subcomponents marked with "+" are partially included by default
*
*   #define:                To include:
*
* + INCL_DOSPROCESS         Process and thread support
*   INCL_DOSINFOSEG         InfoSeg support
* + INCL_DOSFILEMGR         File Management
* + INCL_DOSMEMMGR          Memory Management
* + INCL_DOSSEMAPHORES      Semaphore support
* + INCL_DOSDATETIME        Date/Time and Timer support
*   INCL_DOSMODULEMGR       Module manager
* + INCL_DOSRESOURCES       Resource support
*   INCL_DOSNLS             National Language Support
*   INCL_DOSMISC            Miscellaneous
*   INCL_DOSSIGNALS         Signals
*   INCL_DOSMONITORS        Monitors
*   INCL_DOSQUEUES          Queues
*   INCL_DOSSESMGR          Session Manager Support
*
\***************************************************************************/

#define INCL_DOSINCLUDED

#ifdef INCL_DOS

#define INCL_DOSPROCESS
#define INCL_DOSINFOSEG
#define INCL_DOSFILEMGR
#define INCL_DOSMEMMGR
#define INCL_DOSMISC
#define INCL_DOSSEMAPHORES
#define INCL_DOSDATETIME
#define INCL_DOSMODULEMGR
#define INCL_DOSRESOURCES
#define INCL_DOSNLS
#define INCL_DOSSIGNALS
#define INCL_DOSMONITORS
#define INCL_DOSQUEUES
#define INCL_DOSSESMGR

#endif /* INCL_DOS */

#ifdef INCL_ERRORS
#define INCL_DOSERRORS
#endif /* INCL_ERRORS */

#if (defined(INCL_DOSPROCESS) | !defined(INCL_NOCOMMON))

/*** General services */

USHORT APIENTRY DosBeep(USHORT, USHORT);

/*** Process and Thread support */

VOID APIENTRY DosExit(USHORT, USHORT);

/* DosExit codes */

#define EXIT_THREAD         0
#define EXIT_PROCESS        1

#endif /* common INCL_DOSPROCESS stuff */

#ifdef INCL_DOSPROCESS

typedef struct _PIDINFO {         /* pidi */
    PID pid;
    TID tid;
    PID pidParent;
} PIDINFO;
typedef PIDINFO FAR *PPIDINFO;

USHORT APIENTRY DosCreateThread(VOID (FAR *)(VOID), PTID, PBYTE);
USHORT APIENTRY DosResumeThread(TID);
USHORT APIENTRY DosSuspendThread(TID);

/* Action code values */

#define DCWA_PROCESS        0
#define DCWA_PROCESSTREE    1

/* Wait option values */

#define DCWW_WAIT   0
#define DCWW_NOWAIT 1

typedef struct _RESULTCODES {     /* resc */
    USHORT codeTerminate;
    USHORT codeResult;
} RESULTCODES;
typedef RESULTCODES FAR *PRESULTCODES;

USHORT APIENTRY DosCWait(USHORT, USHORT, PRESULTCODES, PPID, PID);
USHORT APIENTRY DosSleep(ULONG);

/* codeTerminate values (also passed to ExitList routines) */

#define TC_EXIT          0
#define TC_HARDERROR     1
#define TC_TRAP          2
#define TC_KILLPROCESS   3

VOID   APIENTRY DosEnterCritSec(VOID);
VOID   APIENTRY DosExitCritSec(VOID);
USHORT APIENTRY DosExitList(USHORT, VOID (FAR *)(USHORT));

/* DosExitList functions */

#define EXLST_ADD       1
#define EXLST_REMOVE    2
#define EXLST_EXIT      3

USHORT APIENTRY DosExecPgm(PCHAR, USHORT, USHORT, PSZ, PSZ, PRESULTCODES, PSZ);

/* DosExecPgm functions */

#define EXEC_SYNC           0
#define EXEC_ASYNC          1
#define EXEC_ASYNCRESULT    2
#define EXEC_TRACE          3
#define EXEC_BACKGROUND     4
#define EXEC_LOAD           5

USHORT APIENTRY DosGetPid(PPIDINFO);

USHORT APIENTRY DosGetPrty(USHORT, PUSHORT, USHORT);
USHORT APIENTRY DosSetPrty(USHORT, USHORT, SHORT, USHORT);

/* Priority scopes */

#define PRTYS_PROCESS       0
#define PRTYS_PROCESSTREE   1
#define PRTYS_THREAD        2

/* Priority classes */

#define PRTYC_NOCHANGE      0
#define PRTYC_IDLETIME      1
#define PRTYC_REGULAR       2
#define PRTYC_TIMECRITICAL  3

USHORT APIENTRY DosKillProcess(USHORT, PID);

#define DKP_PROCESS         0
#define DKP_PROCESSTREE     1

#endif /* INCL_DOSPROCESS */


/*** InfoSeg support */

#ifdef INCL_DOSINFOSEG

/* Global Info Seg */

typedef struct _GINFOSEG {      /* gis */
    ULONG   time;
    ULONG   msecs;
    UCHAR   hour;
    UCHAR   minutes;
    UCHAR   seconds;
    UCHAR   hundredths;
    USHORT  timezone;
    USHORT  cusecTimerInterval;
    UCHAR   day;
    UCHAR   month;
    USHORT  year;
    UCHAR   weekday;
    UCHAR   uchMajorVersion;
    UCHAR   uchMinorVersion;
    UCHAR   chRevisionLetter;
    UCHAR   sgCurrent;
    UCHAR   sgMax;
    UCHAR   cHugeShift;
    UCHAR   fProtectModeOnly;
    USHORT  pidForeground;
    UCHAR   fDynamicSched;
    UCHAR   csecMaxWait;
    USHORT  cmsecMinSlice;
    USHORT  cmsecMaxSlice;
    USHORT  bootdrive;
    UCHAR   amecRAS[32];
} GINFOSEG;
typedef GINFOSEG FAR *PGINFOSEG;

/* Local Info Seg */

typedef struct _LINFOSEG {      /* lis */
    PID     pidCurrent;
    PID     pidParent;
    USHORT  prtyCurrent;
    TID     tidCurrent;
    USHORT  sgCurrent;
    USHORT  sgSub;
    BOOL    fForeground;
} LINFOSEG;
typedef LINFOSEG FAR *PLINFOSEG;

/* Process Type codes (local info seg typeProcess field) */

#define PT_FULLSCREEN       0
#define PT_REALMODE         1
#define PT_WINDOWABLEVIO    2
#define PT_PM               3
#define PT_DETACHED         4

USHORT APIENTRY DosGetInfoSeg(PSEL, PSEL);

/* Helper macros used to convert selector to PINFOSEG or LINFOSEG */

#define MAKEPGINFOSEG(sel)  ((PGINFOSEG)MAKEP(sel, 0))
#define MAKEPLINFOSEG(sel)  ((PLINFOSEG)MAKEP(sel, 0))

#endif /* INCL_DOSINFOSEG */

#if (defined(INCL_DOSFILEMGR) | !defined(INCL_NOCOMMON))

/*** File manager */

USHORT APIENTRY DosOpen(PSZ, PHFILE, PUSHORT, ULONG, USHORT, USHORT, USHORT, ULONG);
USHORT APIENTRY DosClose(HFILE);
USHORT APIENTRY DosRead(HFILE, PVOID, USHORT, PUSHORT);
USHORT APIENTRY DosWrite(HFILE, PVOID, USHORT, PUSHORT);

/* File time and date types */

typedef struct _FILESTATUS {    /* fsts */
    FDATE  fdateCreation;
    FTIME  ftimeCreation;
    FDATE  fdateLastAccess;
    FTIME  ftimeLastAccess;
    FDATE  fdateLastWrite;
    FTIME  ftimeLastWrite;
    ULONG  cbFile;
    ULONG  cbFileAlloc;
    USHORT attrFile;
} FILESTATUS;
typedef FILESTATUS FAR *PFILESTATUS;

typedef struct _FSALLOCATE {    /* fsalloc */
    ULONG  idFileSystem;
    ULONG  cSectorUnit;
    ULONG  cUnit;
    ULONG  cUnitAvail;
    USHORT cbSector;
} FSALLOCATE;
typedef FSALLOCATE FAR *PFSALLOCATE;

typedef SHANDLE HDIR;        /* hdir */
typedef HDIR FAR *PHDIR;

USHORT APIENTRY DosDelete(PSZ, ULONG);
USHORT APIENTRY DosDupHandle(HFILE, PHFILE);

USHORT APIENTRY DosQFHandState(HFILE, PUSHORT);
USHORT APIENTRY DosSetFHandState(HFILE, USHORT);
USHORT APIENTRY DosQHandType(HFILE, PUSHORT, PUSHORT);

USHORT APIENTRY DosReadAsync (HFILE, PULONG, PUSHORT, PVOID, USHORT, PUSHORT);
USHORT APIENTRY DosWriteAsync(HFILE, PULONG, PUSHORT, PVOID, USHORT, PUSHORT);

USHORT APIENTRY DosFindFirst(PSZ, PHDIR, USHORT, PFILEFINDBUF, USHORT, PUSHORT, ULONG);
USHORT APIENTRY DosFindNext(HDIR, PFILEFINDBUF, USHORT, PUSHORT);
USHORT APIENTRY DosFindClose(HDIR);

USHORT APIENTRY DosNewSize(HFILE, ULONG);
USHORT APIENTRY DosBufReset(HFILE);

USHORT APIENTRY DosChgFilePtr(HFILE, LONG, USHORT, PULONG);

USHORT APIENTRY DosFileLocks(HFILE, PLONG, PLONG);

USHORT APIENTRY DosMove(PSZ, PSZ, ULONG);
USHORT APIENTRY DosMkdir(PSZ, ULONG);
USHORT APIENTRY DosRmdir(PSZ, ULONG);
USHORT APIENTRY DosSelectDisk(USHORT);
USHORT APIENTRY DosQCurDisk(PUSHORT, PULONG);

USHORT APIENTRY DosChdir(PSZ, ULONG);
USHORT APIENTRY DosQCurDir(USHORT, PBYTE, PUSHORT);

USHORT APIENTRY DosQFSInfo(USHORT, USHORT, PBYTE, USHORT);
USHORT APIENTRY DosSetFSInfo(USHORT, USHORT, PBYTE, USHORT);
USHORT APIENTRY DosQVerify(PUSHORT);
USHORT APIENTRY DosSetVerify(USHORT);
USHORT APIENTRY DosSetMaxFH(USHORT);

USHORT APIENTRY DosQFileInfo(HFILE, USHORT, PFILESTATUS, USHORT);
USHORT APIENTRY DosSetFileInfo(HFILE, USHORT, PFILESTATUS, USHORT);

USHORT APIENTRY DosQFileMode(PSZ, PUSHORT, ULONG);
USHORT APIENTRY DosSetFileMode(PSZ, USHORT, ULONG);

#endif /* common INCL_DOSFILEMGR */

#if (defined(INCL_DOSMEMMGR) | !defined(INCL_NOCOMMON))
/*** Memory management */

USHORT APIENTRY DosAllocSeg(USHORT, PSEL, USHORT);
USHORT APIENTRY DosReallocSeg(USHORT, SEL);
USHORT APIENTRY DosFreeSeg(SEL);
USHORT APIENTRY DosGiveSeg(SEL, PID, PSEL);
USHORT APIENTRY DosGetSeg(SEL);

/* Segment attribute flags (used with DosAllocSeg) */

#define SEG_GIVEABLE        0x0001
#define SEG_GETTABLE        0x0002
#define SEG_DISCARDABLE     0x0004

#endif /* common INCL_DOSMEMMGR */

#ifdef INCL_DOSMEMMGR

USHORT APIENTRY DosAllocHuge(USHORT, USHORT, PSEL, USHORT, USHORT);
USHORT APIENTRY DosReallocHuge(USHORT, USHORT, SEL);
USHORT APIENTRY DosGetHugeShift(PUSHORT);

USHORT APIENTRY DosAllocShrSeg(USHORT, PSZ, PSEL);

USHORT APIENTRY DosLockSeg(SEL);
USHORT APIENTRY DosUnlockSeg(SEL);

USHORT APIENTRY DosGetShrSeg(PSZ, PSEL);

USHORT APIENTRY DosMemAvail(PULONG);
USHORT APIENTRY DosCreateCSAlias(SEL, PSEL);

USHORT APIENTRY DosSubAlloc(SEL, PUSHORT, USHORT);
USHORT APIENTRY DosSubFree(SEL, USHORT, USHORT);
USHORT APIENTRY DosSubSet(SEL, USHORT, USHORT);

#endif /* INCL_DOSMEMMGR */

#if (defined(INCL_DOSSEMAPHORES) | !defined(INCL_NOCOMMON))

/*** Semaphore support */

USHORT APIENTRY DosSemClear(HSEM);
USHORT APIENTRY DosSemSet(HSEM);
USHORT APIENTRY DosSemWait(HSEM, LONG);
USHORT APIENTRY DosSemSetWait(HSEM, LONG);
USHORT APIENTRY DosSemRequest(HSEM, LONG);

#endif /* common INCL_DOSSEMAPHORES */

#ifdef INCL_DOSSEMAPHORES

typedef LHANDLE HSYSSEM;          /* hssm */
typedef HSYSSEM FAR *PHSYSSEM;

USHORT APIENTRY DosCreateSem(USHORT, PHSYSSEM, PSZ);

#define CSEM_PRIVATE    0
#define CSEM_PUBLIC     1

USHORT APIENTRY DosOpenSem(PHSEM, PSZ);
USHORT APIENTRY DosCloseSem(HSEM);

typedef struct _MUXSEM {        /* mxs */
    USHORT zero;
    HSEM   hsem;
} MUXSEM;
typedef MUXSEM FAR *PMUXSEM;

typedef struct _MUXSEMLIST {    /* mxsl */
    USHORT  cmxs;
    MUXSEM  amxs[16];
} MUXSEMLIST;
typedef MUXSEMLIST FAR *PMUXSEMLIST;

/*
 * Since a MUXSEMLIST structure is actually a variable length
 * structure, the following macro may be used to define a MUXSEMLIST
 * structure having size elements, named "name".
 */
#define DEFINEMUXSEMLIST(name, size) \
    struct {                         \
        USHORT cmxs;                 \
        MUXSEM amxs[size];           \
    } name;

/*
 * This function actually takes a far pointer to a MUXSEMLIST structure
 * as its second parameter, but in order to allow its use with the
 * DEFINEMUXSEMLIST macro, it is declared here as PVOID.
 */
USHORT APIENTRY DosMuxSemWait(PUSHORT, PVOID, LONG);

#endif /* INCL_DOSSEMAPHORES */

#if (defined(INCL_DOSDATETIME) | !defined(INCL_NOCOMMON))

/*** Time support */

typedef struct _DATETIME {    /* date */
    UCHAR   hours;
    UCHAR   minutes;
    UCHAR   seconds;
    UCHAR   hundredths;
    UCHAR   day;
    UCHAR   month;
    USHORT  year;
    SHORT   timezone;
    UCHAR   weekday;
} DATETIME;
typedef DATETIME FAR *PDATETIME;

USHORT APIENTRY DosGetDateTime(PDATETIME);
USHORT APIENTRY DosSetDateTime(PDATETIME);

#endif /* common INCL_DOSDATETIME */

#ifdef INCL_DOSDATETIME

typedef SHANDLE HTIMER;
typedef HTIMER FAR *PHTIMER;

USHORT APIENTRY DosTimerAsync(ULONG, HSEM, PHTIMER);
USHORT APIENTRY DosTimerStart(ULONG, HSEM, PHTIMER);
USHORT APIENTRY DosTimerStop(HTIMER);

#endif /* INCL_DOSDATETIME */


/*** Module manager */

#ifdef INCL_DOSMODULEMGR

USHORT APIENTRY DosLoadModule(PSZ, USHORT, PSZ, PHMODULE);
USHORT APIENTRY DosFreeModule(HMODULE);
USHORT APIENTRY DosGetProcAddr(HMODULE, PSZ, PPFN);
USHORT APIENTRY DosGetModHandle(PSZ, PHMODULE);
USHORT APIENTRY DosGetModName(HMODULE, USHORT, PCHAR);

#endif /* INCL_DOSMODULEMGR */


/*** NLS Support */

#ifdef INCL_DOSNLS

typedef struct _COUNTRYCODE { /* ctryc */
    USHORT country;
    USHORT codepage;
} COUNTRYCODE;
typedef COUNTRYCODE FAR *PCOUNTRYCODE;

typedef struct _COUNTRYINFO { /* ctryi */
    USHORT country;
    USHORT codepage;
    USHORT fsDateFmt;
    CHAR   szCurrency[5];
    CHAR   szThousandsSeparator[2];
    CHAR   szDecimal[2];
    CHAR   szDateSeparator[2];
    CHAR   szTimeSeparator[2];
    UCHAR  fsCurrencyFmt;
    UCHAR  cDecimalPlace;
    UCHAR  fsTimeFmt;
    USHORT abReserved1[2];
    CHAR   szDataSeparator[2];
    USHORT abReserved2[5];
} COUNTRYINFO;
typedef COUNTRYINFO FAR *PCOUNTRYINFO;

USHORT APIENTRY DosGetCtryInfo(USHORT, PCOUNTRYCODE, PCOUNTRYINFO, PUSHORT);
USHORT APIENTRY DosGetDBCSEv(USHORT, PCOUNTRYCODE, PCHAR);
USHORT APIENTRY DosCaseMap(USHORT, PCOUNTRYCODE, PCHAR);
USHORT APIENTRY DosGetCollate(USHORT, PCOUNTRYCODE, PCHAR, PUSHORT);
USHORT APIENTRY DosGetCp(USHORT, PUSHORT, PUSHORT);
USHORT APIENTRY DosSetCp(USHORT, USHORT);

#endif /* INCL_DOSNLS */


/*** Signal support */

#ifdef INCL_DOSSIGNALS

/* Signal Numbers for DosSetSigHandler  */

#define SIG_CTRLC           1       /* Control C            */
#define SIG_BROKENPIPE      2       /* Broken Pipe          */
#define SIG_KILLPROCESS     3       /* Program Termination  */
#define SIG_CTRLBREAK       4       /* Control Break        */
#define SIG_PFLG_A          5       /* Process Flag A       */
#define SIG_PFLG_B          6       /* Process Flag B       */
#define SIG_PFLG_C          7       /* Process Flag C       */
#define SIG_CSIGNALS        8       /* number of signals plus one */

/* Flag Numbers for DosFlagProcess      */

#define PFLG_A              0       /* Process Flag A       */
#define PFLG_B              1       /* Process Flag B       */
#define PFLG_C              2       /* Process Flag C       */

/* Signal actions */

#define SIGA_KILL           0
#define SIGA_IGNORE         1
#define SIGA_ACCEPT         2
#define SIGA_ERROR          3
#define SIGA_ACKNOWLEDGE    4

/* DosHoldSignal constants */

#define HLDSIG_ENABLE       0
#define HLDSIG_DISABLE      1

/* DosFlagProcess codes */

#define FLGP_SUBTREE        0
#define FLGP_PID            1

typedef VOID (FAR PASCAL *PFNSIGHANDLER)(USHORT, USHORT);

USHORT APIENTRY DosSetSigHandler(PFNSIGHANDLER, PFNSIGHANDLER FAR *, PUSHORT, USHORT, USHORT);
USHORT APIENTRY DosFlagProcess(PID, USHORT, USHORT, USHORT);
USHORT APIENTRY DosHoldSignal(USHORT);
USHORT APIENTRY DosSendSignal(USHORT, USHORT);

#endif /* INCL_DOSSIGNALS */


/*** Monitor support */

#ifdef INCL_DOSMONITORS

typedef SHANDLE HMONITOR;    /* hmon */
typedef HMONITOR FAR *PHMONITOR;

USHORT APIENTRY DosMonOpen(PSZ, PHMONITOR);
USHORT APIENTRY DosMonClose(HMONITOR);
USHORT APIENTRY DosMonReg(HMONITOR, PBYTE, PBYTE, USHORT, USHORT);
USHORT APIENTRY DosMonRead(PBYTE, USHORT, PBYTE, PUSHORT);
USHORT APIENTRY DosMonWrite(PBYTE, PBYTE, USHORT);

#endif /* INCL_DOSMONITORS */


/*** Pipe and queue support */

#ifdef INCL_DOSQUEUES

typedef SHANDLE HQUEUE;  /* hq */
typedef HQUEUE FAR *PHQUEUE;

USHORT APIENTRY DosMakePipe(PHFILE, PHFILE, USHORT);
USHORT APIENTRY DosCloseQueue(HQUEUE);
USHORT APIENTRY DosCreateQueue(PHQUEUE, USHORT, PSZ);
USHORT APIENTRY DosOpenQueue(PUSHORT, PHQUEUE, PSZ);
USHORT APIENTRY DosPeekQueue(HQUEUE, PULONG, PUSHORT, PULONG, PUSHORT, USHORT, PBYTE, HSEM);
USHORT APIENTRY DosPurgeQueue(HQUEUE);
USHORT APIENTRY DosQueryQueue(HQUEUE, PUSHORT);
USHORT APIENTRY DosReadQueue(HQUEUE, PULONG, PUSHORT, PULONG, USHORT, USHORT, PBYTE, HSEM);
USHORT APIENTRY DosWriteQueue(HQUEUE, USHORT, USHORT, PBYTE, USHORT);

#endif /* INCL_DOSQUEUES */


/*** Miscellaneous functions */

#ifdef INCL_DOSMISC

USHORT APIENTRY DosError(USHORT);
USHORT APIENTRY DosSetVec(USHORT, PFN, PPFN);
USHORT APIENTRY DosGetMessage(PCHAR FAR *, USHORT, PCHAR, USHORT, USHORT, PSZ, PUSHORT);
USHORT APIENTRY DosErrClass(USHORT, PUSHORT, PUSHORT, PUSHORT);
USHORT APIENTRY DosInsMessage(PCHAR FAR *, USHORT, PSZ, USHORT, PCHAR, USHORT, PUSHORT);
USHORT APIENTRY DosPutMessage(HFILE, USHORT, PCHAR);
USHORT APIENTRY DosGetEnv(PUSHORT, PUSHORT);
USHORT APIENTRY DosScanEnv(PSZ, PSZ  FAR *);
USHORT APIENTRY DosSearchPath(USHORT, PSZ, PSZ, PBYTE, USHORT);
USHORT APIENTRY DosGetVersion(PUSHORT);
USHORT APIENTRY DosGetMachineMode(PBYTE);

#endif /* INCL_DOSMISC */


/*** Session manager support */

#ifdef INCL_DOSSESMGR

typedef struct _STARTDATA {   /* stdata */
    USHORT cb;
    USHORT Related;
    USHORT FgBg;
    USHORT TraceOpt;
    PSZ    PgmTitle;
    PSZ    PgmName;
    PBYTE  PgmInputs;
    PBYTE  TermQ;
} STARTDATA;
typedef STARTDATA FAR *PSTARTDATA;

typedef struct _STATUSDATA { /* stsdata */
    USHORT cb;
    USHORT SelectInd;
    USHORT BindInd;
} STATUSDATA;
typedef STATUSDATA FAR *PSTATUSDATA;

USHORT APIENTRY DosStartSession(PSTARTDATA, PUSHORT, PUSHORT);
USHORT APIENTRY DosSetSession(USHORT, PSTATUSDATA);
USHORT APIENTRY DosSelectSession(USHORT, ULONG);
USHORT APIENTRY DosStopSession(USHORT, USHORT, ULONG);

#endif /* INCL_DOSSESMGR */


/*** Device support */

#ifdef INCL_DOSDEVICES

USHORT APIENTRY DosDevConfig(PVOID, USHORT, USHORT);
USHORT APIENTRY DosDevIOCtl(PVOID, PVOID, USHORT, USHORT, USHORT);
USHORT APIENTRY DosSystemService(USHORT, PVOID, PVOID);

USHORT APIENTRY DosCLIAccess(VOID);
USHORT APIENTRY DosPortAccess(USHORT, USHORT, USHORT, USHORT);
USHORT APIENTRY DosPhysicalDisk(USHORT, PBYTE, USHORT, PBYTE, USHORT);

#endif /* INCL_DOSDEVICES */
