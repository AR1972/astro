/***************************************************************************\
*
* Module Name: BSESUB.H
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
*   #define:                To include:
*
*   INCL_SUB                KBD, VIO, MOU
*
\***************************************************************************/

#ifdef INCL_SUB

typedef SHANDLE         HKBD;
typedef HKBD    far *   PHKBD;

USHORT APIENTRY KbdRegister (PSZ, PSZ, ULONG);

#define KR_KBDCHARIN        0x00000001
#define KR_KBDPEEK          0x00000002
#define KR_KBDFLUSHBUFFER   0x00000004
#define KR_KBDGETSTATUS     0x00000008
#define KR_KBDSETSTATUS     0x00000010
#define KR_KBDSTRINGIN      0x00000020
#define KR_KBDOPEN          0x00000040
#define KR_KBDCLOSE         0x00000080
#define KR_KBDGETFOCUS      0x00000100
#define KR_KBDFREEFOCUS     0x00000200
#define KR_KBDGETCP         0x00000400
#define KR_KBDSETCP         0x00000800
#define KR_KBDXLATE         0x00001000
#define KR_KBDSETCUSTXT     0x00002000

#define IO_WAIT     0
#define IO_NOWAIT   1

USHORT APIENTRY KbdDeRegister ( void );

/* KBDKEYINFO structure, for KbdCharIn and KbdPeek */

typedef struct _KBDKEYINFO {    /* kbci */
        UCHAR    chChar;
        UCHAR    chScan;
        UCHAR    fbStatus;
        UCHAR    bNlsShift;
        USHORT   fsState;
        ULONG    time;
        }KBDKEYINFO;
typedef KBDKEYINFO far *PKBDKEYINFO;

USHORT APIENTRY KbdCharIn ( PKBDKEYINFO, USHORT, HKBD );
USHORT APIENTRY KbdPeek ( PKBDKEYINFO, HKBD );

/* structure for KbdStringIn() */

typedef struct _STRINGINBUF {   /* kbsi */
        USHORT cb;
        USHORT cchIn;
        } STRINGINBUF;
typedef STRINGINBUF far *PSTRINGINBUF;

USHORT APIENTRY KbdStringIn ( PCH, PSTRINGINBUF, USHORT, HKBD );

USHORT APIENTRY KbdFlushBuffer ( HKBD );

/* KBDINFO structure, for KbdSet/GetStatus */
typedef struct _KBDINFO {       /* kbst */
        USHORT cb;
        USHORT fsMask;
        USHORT chTurnAround;
        USHORT fsInterim;
        USHORT fsState;
        }KBDINFO;
typedef KBDINFO far *PKBDINFO;

USHORT APIENTRY KbdSetStatus ( PKBDINFO, HKBD );
USHORT APIENTRY KbdGetStatus ( PKBDINFO, HKBD );

USHORT APIENTRY KbdSetCp ( USHORT, USHORT, HKBD);
USHORT APIENTRY KbdGetCp ( ULONG, PUSHORT, HKBD );

USHORT APIENTRY KbdOpen ( PHKBD );
USHORT APIENTRY KbdClose ( HKBD );

USHORT APIENTRY KbdGetFocus ( USHORT, HKBD );
USHORT APIENTRY KbdFreeFocus ( HKBD );

USHORT APIENTRY KbdSynch ( USHORT );

/* structure for KbdXlate() */
typedef struct _KBDTRANSLATE {      /* kbxl */
        UCHAR      chChar;
        UCHAR      chScan;
        UCHAR      fbStatus;
        UCHAR      bNlsShift;
        USHORT     fsState;
        ULONG      time;
        USHORT     fsDD;
        USHORT     fsXlate;
        USHORT     fsShift;
        USHORT     sZero;
        } KBDTRANSLATE;
typedef KBDTRANSLATE far *PKBDTRANSLATE;

USHORT APIENTRY KbdXlate ( PKBDTRANSLATE, HKBD );
USHORT APIENTRY KbdSetCustXt ( PUSHORT, HKBD );

typedef SHANDLE         HVIO;
typedef HVIO    far *   PHVIO;

USHORT APIENTRY VioRegister ( PSZ, PSZ, ULONG, ULONG );

USHORT APIENTRY VioDeRegister ( void );

USHORT APIENTRY VioGetBuf ( PULONG, PUSHORT, HVIO );

USHORT APIENTRY VioGetCurPos ( PUSHORT, PUSHORT, HVIO );
USHORT APIENTRY VioSetCurPos ( USHORT, USHORT, HVIO );

/* structure for VioSet/GetCurType() */
typedef struct _VIOCURSORINFO { /* vioci */
        USHORT   yStart;
        USHORT   cEnd;
        USHORT   cx;
        USHORT   attr;
        } VIOCURSORINFO;
typedef VIOCURSORINFO FAR *PVIOCURSORINFO;

USHORT APIENTRY VioGetCurType ( PVIOCURSORINFO, HVIO );
USHORT APIENTRY VioSetCurType ( PVIOCURSORINFO, HVIO );

/* structure for VioSet/GetMode() */
typedef struct _VIOMODEINFO {   /* viomi */
        USHORT cb;
        UCHAR  fbType;
        UCHAR  color;
        USHORT col;
        USHORT row;
        USHORT hres;
        USHORT vres;
        UCHAR  fmt_ID;
        UCHAR  attrib;
        } VIOMODEINFO;
typedef VIOMODEINFO FAR *PVIOMODEINFO;

#define VGMT_OTHER          0x01
#define VGMT_GRAPHICS       0x02
#define VGMT_DISABLEBURST   0x04

USHORT APIENTRY VioGetMode ( PVIOMODEINFO, HVIO );
USHORT APIENTRY VioSetMode ( PVIOMODEINFO, HVIO );

/* structure for VioGetPhysBuf() */

typedef struct _VIOPHYSBUF {    /* viopb */
        PBYTE    pBuf;
        ULONG    cb;
        SEL      asel[1];
        } VIOPHYSBUF;
typedef VIOPHYSBUF far *PVIOPHYSBUF;

USHORT APIENTRY VioGetPhysBuf ( PVIOPHYSBUF, USHORT );

USHORT APIENTRY VioReadCellStr ( PCH, PUSHORT, USHORT, USHORT, HVIO );
USHORT APIENTRY VioReadCharStr ( PCH, PUSHORT, USHORT, USHORT, HVIO );
USHORT APIENTRY VioWrtCellStr ( PCH, USHORT, USHORT, USHORT, HVIO );
USHORT APIENTRY VioWrtCharStr ( PCH, USHORT, USHORT, USHORT, HVIO );

USHORT APIENTRY VioScrollDn ( USHORT, USHORT, USHORT, USHORT,
                              USHORT, PBYTE,  HVIO );
USHORT APIENTRY VioScrollUp ( USHORT, USHORT, USHORT, USHORT,
                              USHORT, PBYTE,  HVIO );
USHORT APIENTRY VioScrollLf ( USHORT, USHORT, USHORT, USHORT,
                              USHORT, PBYTE,  HVIO );
USHORT APIENTRY VioScrollRt ( USHORT, USHORT, USHORT, USHORT,
                              USHORT, PBYTE,  HVIO );

USHORT APIENTRY VioWrtNAttr ( PBYTE, USHORT, USHORT, USHORT, HVIO );
USHORT APIENTRY VioWrtNCell ( PBYTE, USHORT, USHORT, USHORT, HVIO );
USHORT APIENTRY VioWrtNChar ( PCH, USHORT, USHORT, USHORT, HVIO );
USHORT APIENTRY VioWrtTTy ( PCH, USHORT, HVIO );
USHORT APIENTRY VioWrtCharStrAtt ( PCH, USHORT, USHORT, USHORT, PBYTE, HVIO );

USHORT APIENTRY VioShowBuf ( USHORT, USHORT, HVIO );


#define ANSI_ON     1
#define ANSI_OFF    0

USHORT APIENTRY VioSetAnsi ( USHORT, HVIO );
USHORT APIENTRY VioGetAnsi ( PUSHORT, HVIO );

USHORT APIENTRY VioPrtSc ( HVIO );
USHORT APIENTRY VioPrtScToggle ( HVIO );

#define VSRWI_SAVEANDREDRAW  0
#define VSRWI_REDRAW         1

#define VSRWN_SAVE           0
#define VSRWN_REDRAW         1

#define UNDOI_GETOWNER          0
#define UNDOI_RELEASEOWNER      1

#define UNDOK_ERRORCODE         0
#define UNDOK_TERMINATE         1

USHORT APIENTRY VioSavRedrawWait ( USHORT, PUSHORT, USHORT);
USHORT APIENTRY VioSavRedrawUndo ( USHORT, USHORT, USHORT );

#define VMWR_POPUP  0
#define VMWN_POPUP  0

USHORT APIENTRY VioModeWait ( USHORT, PUSHORT, USHORT );
USHORT APIENTRY VioModeUndo ( USHORT, USHORT, USHORT );

#define LOCKIO_NOWAIT   0
#define LOCKIO_WAIT     1

#define LOCK_SUCCESS    0
#define LOCK_FAIL       1

USHORT APIENTRY VioScrLock ( USHORT, PBYTE, HVIO );
USHORT APIENTRY VioScrUnLock ( HVIO );

#define VP_NOWAIT       0x0000
#define VP_WAIT         0x0001
#define VP_OPAQUE       0x0000
#define VP_TRANSPARENT  0x0002

USHORT APIENTRY VioPopUp ( PUSHORT, HVIO );
USHORT APIENTRY VioEndPopUp ( HVIO );

/* structure for VioGetConfig() */

typedef struct _VIOCONFIGINFO { /* vioin */
        USHORT  cb     ;
        USHORT  adapter;
        USHORT  display;
        ULONG   cbMemory;
        } VIOCONFIGINFO;
typedef VIOCONFIGINFO far *PVIOCONFIGINFO;

USHORT APIENTRY VioGetConfig ( USHORT, PVIOCONFIGINFO, HVIO );

/* structure for VioGet/SetFont() */
typedef struct _VIOFONTINFO {   /* viofi */
        USHORT  cb;
        USHORT  type;
        USHORT  cxCell;
        USHORT  cyCell;
        PVOID   pbData;
        USHORT  cbData;
        } VIOFONTINFO;
typedef VIOFONTINFO far *PVIOFONTINFO;

#define VGFI_GETCURFONT     0
#define VGFI_GETROMFONT     1

USHORT APIENTRY VioGetFont ( PVIOFONTINFO, HVIO );
USHORT APIENTRY VioSetFont ( PVIOFONTINFO, HVIO );

USHORT APIENTRY VioGetCp ( USHORT, PUSHORT, HVIO );
USHORT APIENTRY VioSetCp ( USHORT, USHORT, HVIO );

typedef struct _VIOPALSTATE {   /* viopal */
        USHORT  cb;
        USHORT  type;
        USHORT  iFirst;
        USHORT  acolor[1];
        }VIOPALSTATE;
typedef VIOPALSTATE far *PVIOPALSTATE;

typedef struct _VIOOVERSCAN {   /* vioos */
        USHORT  cb;
        USHORT  type;
        USHORT  color;
        }VIOOVERSCAN;
typedef VIOOVERSCAN far *PVIOOVERSCAN;

typedef struct _VIOINTENSITY {  /* vioint */
        USHORT  cb;
        USHORT  type;
        USHORT  fs;
        }VIOINTENSITY;
typedef VIOINTENSITY far *PVIOINTENSITY;

USHORT APIENTRY VioGetState ( PVOID, HVIO );
USHORT APIENTRY VioSetState ( PVOID, HVIO );

typedef SHANDLE         HMOU;
typedef HMOU    far *   PHMOU;

USHORT APIENTRY MouRegister ( PSZ, PSZ, ULONG );

USHORT APIENTRY MouDeRegister ( void );

USHORT APIENTRY MouFlushQue ( HMOU );

#define MHK_BUTTON1         0x0002
#define MHK_BUTTON2         0x0004
#define MHK_BUTTON3         0x0008

USHORT APIENTRY MouGetHotKey ( PUSHORT, HMOU );
USHORT APIENTRY MouSetHotKey ( PUSHORT, HMOU );

/* structure for MouGet/SetPtrPos() */
typedef struct _PTRLOC {    /* moupl */
        USHORT row;
        USHORT col;
        } PTRLOC;
typedef PTRLOC far *PPTRLOC;

USHORT APIENTRY MouGetPtrPos ( PPTRLOC, HMOU );
USHORT APIENTRY MouSetPtrPos ( PPTRLOC, HMOU );

/* structure for MouGet/SetPtrShape() */
typedef struct _PTRSHAPE {  /* moups */
        USHORT cb;
        USHORT col;
        USHORT row;
        USHORT colHot;
        USHORT rowHot;
        } PTRSHAPE;
typedef PTRSHAPE far *PPTRSHAPE;

USHORT APIENTRY MouSetPtrShape ( PBYTE, PPTRSHAPE, HMOU );
USHORT APIENTRY MouGetPtrShape ( PBYTE, PPTRSHAPE, HMOU );

USHORT APIENTRY MouGetDevStatus ( PUSHORT, HMOU );

USHORT APIENTRY MouGetNumButtons ( PUSHORT, HMOU );
USHORT APIENTRY MouGetNumMickeys ( PUSHORT, HMOU );

/* structure for MouReadEventQue() */
typedef struct _MOUEVENTINFO {  /* mouev */
        USHORT fs;
        ULONG  Time;
        USHORT row;
        USHORT col;
        }MOUEVENTINFO;
typedef MOUEVENTINFO far *PMOUEVENTINFO;

USHORT APIENTRY MouReadEventQue ( PMOUEVENTINFO, PUSHORT, HMOU );

/* structure for MouGetNumQueEl() */
typedef struct _MOUQUEINFO {    /* mouqi */
        USHORT cEvents;
        USHORT cmaxEvents;
        } MOUQUEINFO;
typedef MOUQUEINFO far *PMOUQUEINFO;

USHORT APIENTRY MouGetNumQueEl ( PMOUQUEINFO, HMOU );

USHORT APIENTRY MouGetEventMask ( PUSHORT, HMOU );
USHORT APIENTRY MouSetEventMask ( PUSHORT, HMOU );

/* structure for MouGet/SetScaleFact() */
typedef struct _SCALEFACT { /* mousc */
        USHORT rowScale;
        USHORT colScale;
        } SCALEFACT;
typedef SCALEFACT far *PSCALEFACT;

USHORT APIENTRY MouGetScaleFact ( PSCALEFACT, HMOU );
USHORT APIENTRY MouSetScaleFact ( PSCALEFACT, HMOU );

USHORT APIENTRY MouOpen ( PSZ, PHMOU );
USHORT APIENTRY MouClose ( HMOU );

/* structure for MouRemovePtr() */
typedef struct _NOPTRRECT { /* mourt */
        USHORT row;
        USHORT col;
        USHORT cRow;
        USHORT cCol;
        } NOPTRRECT;
typedef NOPTRRECT far *PNOPTRRECT;

USHORT APIENTRY MouRemovePtr ( PNOPTRRECT, HMOU );

USHORT APIENTRY MouDrawPtr ( HMOU );

USHORT APIENTRY MouSetDevStatus ( PUSHORT, HMOU );
USHORT APIENTRY MouInitReal ( PSZ );

USHORT APIENTRY MouSynch ( USHORT );

#endif /* INCL_SUB */
