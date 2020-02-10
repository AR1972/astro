/***************************************************************************\
*
* Module Name: OS2DEF.H
*
* OS/2 Common Definitions file
*
* This file is compatible with OS/2 version 1.0.
*
* Copyright (c) 1988  IBM Corporation
* Copyright (c) 1988  Microsoft Corporation
*
\***************************************************************************/

#define PASCAL  pascal
#define FAR     far
#define NEAR    near
#define VOID    void

typedef unsigned short SHANDLE;
typedef void far      *LHANDLE;

#define EXPENTRY pascal far
#define APIENTRY pascal far

#define CHAR    char            /* ch  */
#define SHORT   int             /* s   */
#define LONG    long            /* l   */
#define INT     int             /* i   */

typedef unsigned char UCHAR;    /* uch */
typedef unsigned int  USHORT;   /* us  */
typedef unsigned long ULONG;    /* ul  */
typedef unsigned int  UINT;     /* ui  */

typedef unsigned char BYTE;     /* b   */

typedef char far *PSZ;
typedef char     *NPSZ;

typedef char far *PCH;
typedef char     *NPCH;

typedef int   (pascal far  *PFN)();
typedef int   (pascal near *NPFN)();
typedef int   (pascal far * far *PPFN)();

typedef BYTE   FAR *PBYTE;
typedef BYTE       *NPBYTE;

typedef CHAR   FAR *PCHAR;
typedef SHORT  FAR *PSHORT;
typedef LONG   FAR *PLONG;
typedef INT    FAR *PINT;

typedef UCHAR  FAR *PUCHAR;
typedef USHORT FAR *PUSHORT;
typedef ULONG  FAR *PULONG;
typedef UINT   FAR *PUINT;

typedef VOID   FAR *PVOID;

typedef unsigned short BOOL;    /* f */
typedef BOOL FAR *PBOOL;

#define FALSE   0
#define TRUE    1

typedef unsigned short SEL;     /* sel */
typedef SEL FAR *PSEL;

typedef SHANDLE HFILE;          /* hf */
typedef HFILE FAR *PHFILE;

/*
 * The HSEM type is used in structures and functions that take both
 * a system semaphore handle, or a far pointer to a RAM semaphore.
 * It is declared as VOID FAR * so that the use of both types of
 * semaphores will not result in a compiler warning.
 */
typedef VOID FAR *HSEM;         /* hsem */
typedef HSEM FAR *PHSEM;

/*** Useful Helper Macros */

/* Create untyped far pointer from selector and offset */
#define MAKEP(sel, off)     ((PVOID)MAKEULONG(off, sel))

/* Extract selector or offset from far pointer */
#define SELECTOROF(p)       (((PUSHORT)&(p))[1])
#define OFFSETOF(p)         (((PUSHORT)&(p))[0])

/* Cast any variable to an instance of the specified type. */
#define MAKETYPE(v, type)   (*((type far *)&v))

/* Calculate the byte offset of a field in a structure of type type. */
#define FIELDOFFSET(type, field)    ((SHORT)&(((type *)0)->field))

/* Combine l & h to form a 32 bit quantity. */
#define MAKEULONG(l, h)  ((ULONG)(((USHORT)(l)) | ((ULONG)((USHORT)(h))) << 16))
#define MAKELONG(l, h)   ((LONG)MAKEULONG(l, h))

/* Combine l & h to form a 16 bit quantity. */
#define MAKEUSHORT(l, h) (((USHORT)(l)) | ((USHORT)(h)) << 8)
#define MAKESHORT(l, h)  ((SHORT)MAKEUSHORT(l, h))

/* Extract high and low order parts of 16 and 32 bit quantity */
#define LOBYTE(w)       LOUCHAR(w)
#define HIBYTE(w)       HIUCHAR(w)
#define LOUCHAR(w)      ((UCHAR)(w))
#define HIUCHAR(w)      (((USHORT)(w) >> 8) & 0xff)
#define LOUSHORT(l)     ((USHORT)(l))
#define HIUSHORT(l)     ((USHORT)(((ULONG)(l) >> 16) & 0xffff))

/*** Common Error definitions ****/

typedef ULONG ERRORID;  /* errid */
typedef ERRORID FAR *PERRORID;

/* Combine severity and error code to produce ERRORID */
#define MAKEERRORID(sev, error) (ERRORID)(MAKEULONG((error), (sev)))

/* Severity codes */
#define SEVERITY_NOERROR                    0x0000
#define SEVERITY_WARNING                    0x0004
#define SEVERITY_ERROR                      0x0008
#define SEVERITY_SEVERE                     0x000C
#define SEVERITY_UNRECOVERABLE              0x0010

/* Base component error values */

#define WINERR_BASE     0x1000  /* Window Manager                  */
#define GPIERR_BASE     0x2000  /* Graphics Presentation Interface */
#define DEVERR_BASE     0x3000  /* Device Manager                  */
#define SPLERR_BASE     0x4000  /* Spooler                         */

/*** Common types used across components */

/*** Common DOS types */

typedef USHORT    HMODULE;  /* hmod */
typedef HMODULE FAR *PHMODULE;

typedef USHORT    PID;      /* pid  */
typedef PID FAR *PPID;

typedef USHORT    TID;      /* tid  */
typedef TID FAR *PTID;

/*** Common SUP types */

typedef LHANDLE   HAB;      /* hab  */
typedef HAB FAR *PHAB;

/*** Common GPI/DEV types */

typedef LHANDLE   HPS;      /* hps  */
typedef HPS FAR *PHPS;

typedef LHANDLE   HDC;      /* hdc  */
typedef HDC FAR *PHDC;

typedef LHANDLE   HRGN;     /* hrgn */
typedef HRGN FAR *PHRGN;

typedef LHANDLE   HBITMAP;  /* hbm  */
typedef HBITMAP FAR *PHBITMAP;

typedef LHANDLE   HMF;      /* hmf  */
typedef HMF FAR *PHMF;

typedef ULONG     COLOR;    /* clr  */
typedef COLOR FAR *PCOLOR;

/* common DOS/SHL types */

/* File time and date types */

typedef struct _FTIME {         /* ftime */
    unsigned twosecs : 5;
    unsigned minutes : 6;
    unsigned hours   : 5;
} FTIME;
typedef FTIME FAR *PFTIME;

typedef struct _FDATE {         /* fdate */
    unsigned day     : 5;
    unsigned month   : 4;
    unsigned year    : 7;
} FDATE;
typedef FDATE FAR *PFDATE;

typedef struct _FILEFINDBUF {   /* findbuf */
    FDATE  fdateCreation;
    FTIME  ftimeCreation;
    FDATE  fdateLastAccess;
    FTIME  ftimeLastAccess;
    FDATE  fdateLastWrite;
    FTIME  ftimeLastWrite;
    ULONG  cbFile;
    ULONG  cbFileAlloc;
    USHORT attrFile;
    UCHAR  cchName;
    CHAR   achName[13];
} FILEFINDBUF;
typedef FILEFINDBUF FAR *PFILEFINDBUF;

/*** Common WIN types */

typedef LHANDLE HWND;      /* hwnd */
typedef HWND FAR *PHWND;
