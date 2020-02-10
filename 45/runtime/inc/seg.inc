;***
; SEG.INC (& cMacros.inc) - 18-Jun-1986 - Common Runtime Segment Definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; Contains the cMacros segment definitions used throughout the common runtime.
; Also includes cMacros.inc
;
;******************************************************************************
?WIN	=	0		;[18]No, not windows
?PLM	=	1		;[18]Default to PLM conventions
?DF	=	1		;[18]We'll define our own seg's
?TF	=	1		;[18]Don't need MOV SP,BP at cEnd
?NODATA =	1		;[31]no DS-setup prologue for Windows
				;[31]	harmless when ?WIN == 0, but required
				;[31]	so cMacros won't object to ATOMIC attr
	include cMacros.inc
;******************************************************************************
;
; Component segment usages.
; If a component segment name has been turned on, we now turn on the
; individual segments required/used by that component.
;
;******************************************************************************
IFDEF	_?rt_seg_RT		;Runtime Core Component
	useSeg	<RT_TEXT,_DATA>
ENDIF
IFDEF	_?rt_seg_NH		;Near Heap manager
	useSeg	SS_TEXT
ENDIF
IFDEF	_?rt_seg_FH		;Far heap manager
	useSeg	FH_TEXT
ENDIF
IFDEF	_?rt_seg_ST		;String functions
	useSeg	ST_TEXT
ENDIF
IFDEF	_?rt_seg_GR		;Graphics
	useSeg	GR_TEXT
ENDIF
IFDEF	_?rt_seg_MT		;Floating Point math
	useSeg	MT_TEXT
ENDIF
IFDEF	_?rt_seg_ER		;Error handling / processing
	useSeg	ER_TEXT
ENDIF
IFDEF	_?rt_seg_EV		;Event Handling
	useSeg	EV_TEXT
ENDIF
IFDEF	_?rt_seg_SN		;Sound and Music
	useSeg	SN_TEXT
ENDIF
IFDEF	_?rt_seg_DV		;Device I/O support
	useSeg	DV_TEXT
ENDIF
IFDEF	_?rt_seg_CN		;Console I/O support
	useSeg	CN_TEXT
ENDIF
IFDEF	_?rt_seg_DK		;Disk I/O support
	useSeg	DK_TEXT
ENDIF
IFDEF	_?rt_seg_OI		;COM/Printer I/O support
	useSeg	OI_TEXT
ENDIF
IFDEF	_?rt_seg_OS		;Operating system functions/features
	useSeg	OS_TEXT
ENDIF
IFDEF	_?rt_seg_DB		;Debug code (user level)
	useSeg	DB_TEXT
ENDIF
IFDEF	_?rt_seg_ID		;Internal debugging support
	useSeg	ID_TEXT
ENDIF
;******************************************************************************
;
; _?rt_cSeg	n,ln,a,co,cl,grp
;
; where:
;	n	= physical (external) name of segment
;	ln	= logical (internal) name of segment
;	a	= alignment (optional)
;	co	= combine type (optional)
;	cl	= class (optional)
;	grp	= group (optional)
;
; Mimics cMacros createSeg macro, except that it conditionally defines the
; desired segment only if "_?rt_seg_" symbol is defined to turn on that
; segment.
;
;******************************************************************************
_?rt_cSeg	macro	n,ln,a,co,cl,grp
IFDEF	_?rt_seg_&ln
	createSeg	n,ln,a,co,cl,grp
ENDIF
	endm
	.XCREF	_?rt_cSeg	;Don't list or cross reference it
;******************************************************************************
;
; _?rt_cdSeg
; Same as _?rt_cSeg, except that it also takes care of conditionally setting
; a flag that is later used to conditionally define DGROUP
;
;******************************************************************************
_?rt_cdSeg	macro	n,ln,a,co,cl,grp
IFDEF	_?rt_seg_&ln
	createSeg	n,ln,a,co,cl,grp
_?rt_dataseg	EQU	TRUE
ENDIF
	endm
	.XCREF	_?rt_cdSeg	;Don't list or cross reference it
;******************************************************************************
;
; Code segments
;
;******************************************************************************
CODESEGS MACRO		;[7]
	_?rt_cSeg	LOADRTM,LOADRTM,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,RT_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,NH_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,FH_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,ST_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,GR_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,MT_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,ER_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,EV_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,SN_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,DV_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,CN_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,DK_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,OI_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,OS_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,DB_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	CODE,ID_TEXT,WORD,PUBLIC,CODE
	_?rt_cSeg	_TEXT,_TEXT,PARA,PUBLIC,CODE			;[10]

	_?rt_cSeg	CODE,INIT_CODE,WORD,PUBLIC,CODE
	_?rt_cSeg	FAR_DATA,FAR_DATA,PARA,PUBLIC,FAR_DATA		;[14]
	_?rt_cSeg	FAR_BSS,FAR_BSS,PARA,PUBLIC,FAR_BSS		;[14]

	ENDM		;[7]
;******************************************************************************
;
; Data segments
;
;******************************************************************************
DATASEGS MACRO		;[7]
	_?rt_cdSeg	NULL,	NULL,	PARA,	PUBLIC, BEGDATA,DGROUP ;[13]
	_?rt_cdSeg	BR_DATA,BR_DATA,WORD,	PUBLIC, DATA,	DGROUP ;[6]
	_?rt_cdSeg	CONST,	CONST,	WORD,	PUBLIC, DATA,	DGROUP ;[6]
	_?rt_cdSeg	_BSS,	_BSS,	WORD,	PUBLIC, DATA,	DGROUP ;[6]
	_?rt_cdSeg	_DATA,	_DATA,	WORD,	PUBLIC, DATA,	DGROUP ;[6]
	_?rt_cdSeg	CDATA,	CDATA,	WORD,	PUBLIC, DATA,	DGROUP
	_?rt_cdSeg	XIFB,	XIFB,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XIF,	XIF,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XIFE,	XIFE,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XIB,	XIB,	WORD,	PUBLIC, DATA,	DGROUP
	_?rt_cdSeg	XI,	XI,	WORD,	PUBLIC, DATA,	DGROUP
	_?rt_cdSeg	XIE,	XIE,	WORD,	PUBLIC, DATA,	DGROUP
	_?rt_cdSeg	XPB,	XPB,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XP,	XP,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XPE,	XPE,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XCB,	XCB,	WORD,	PUBLIC, DATA,	DGROUP ;[9]
	_?rt_cdSeg	XC,	XC,	WORD,	PUBLIC, DATA,	DGROUP ;[9]
	_?rt_cdSeg	XCE,	XCE,	WORD,	PUBLIC, DATA,	DGROUP ;[9]
	_?rt_cdSeg	XCFB,	XCFB,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XCF,	XCF,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	XCFE,	XCFE,	WORD,	PUBLIC, DATA,	DGROUP ;[22]
	_?rt_cdSeg	DBDATA,	DBDATA,	WORD,	COMMON, DATA,	DGROUP	;[39]
	_?rt_cdSeg	_CALLTAB,_CALLTAB,WORD,	PUBLIC, CALLTAB,DGROUP ;[30]
	_?rt_cdSeg	HDR,	HDR,	BYTE,	PUBLIC, MSG,	DGROUP ;[30]
	_?rt_cdSeg	MSG,	MSG,	BYTE,	PUBLIC, MSG,	DGROUP ;[30]
	_?rt_cdSeg	PAD,	PAD,	BYTE,	COMMON, MSG,	DGROUP ;[30]
	_?rt_cdSeg	EPAD,	EPAD,	BYTE,	COMMON, MSG,	DGROUP ;[30]
	_?rt_cdSeg	BC_DATA,BC_DATA,WORD,	PUBLIC, BC_DATA,DGROUP ;[25]
	_?rt_cdSeg	BC_FT,	BC_FT,	WORD,	PUBLIC, BC_SEGS,DGROUP
	_?rt_cdSeg	BC_CN,	BC_CN,	WORD,	PUBLIC, BC_SEGS,DGROUP
	_?rt_cdSeg	BC_DS,	BC_DS,	WORD,	PUBLIC, BC_SEGS,DGROUP
	_?rt_cdSeg	BC_SAB, BC_SAB, WORD,	PUBLIC, BC_SEGS,DGROUP
	_?rt_cdSeg	BC_SA,	BC_SA,	WORD,	PUBLIC, BC_SEGS,DGROUP
	_?rt_cdSeg	BC_SAE, BC_SAE, WORD,	PUBLIC, BC_SEGS,DGROUP ;[26]
	_?rt_cdSeg	XOB,	XOB,	WORD,	PUBLIC, BSS,	DGROUP ;[8]
	_?rt_cdSeg	XO,	XO,	WORD,	PUBLIC, BSS,	DGROUP ;[8]
	_?rt_cdSeg	XOE,	XOE,	WORD,	PUBLIC, BSS,	DGROUP ;[8]
	_?rt_cdSeg	BR_SKYS,BR_SKYS,PARA,	PUBLIC, BLANK,	DGROUP ;[20]
	_?rt_cdSeg	COMMON, COMMON, WORD,	COMMON, BLANK,	DGROUP
	_?rt_cdSeg	NMALLOC,NMALLOC,WORD,	COMMON, BC_VARS,DGROUP ;[15]
	_?rt_cdSeg	ENMALLOC,ENMALLOC,WORD, COMMON, BC_VARS,DGROUP ;[15]
	_?rt_cdSeg	STACK,	STACK,	PARA,	STACK,	STACK,	DGROUP ;[8]
IFDEF	_?rt_dataseg
	defGrp	DGROUP
ENDIF
	ENDM		;[7]
;******************************************************************************
;
; Order code vs. data based on whether or not we are dos 3 rtm
;
;******************************************************************************


IFNDEF SEGS_DEFINED		;[7]
	CODESEGS		;[7] code comes first everywhere else
	DATASEGS		;[7]
ENDIF				;[7] IFNDEF SEGS_DEFINED

;******************************************************************************
;
; Segment assumptions. We always assume DS=SS, and they both point at
; DGROUP. CS must be assumed by individual modules.
;
;******************************************************************************
IFDEF	_?rt_dataseg
assumes DS,DGROUP
assumes ES,NOTHING		;[37]
assumes SS,DGROUP
ENDIF

	PURGE	_?rt_cSeg	;Macros no longer needed
	PURGE	_?rt_cdSeg	;[7]
	PURGE	useSeg		;[7]
	PURGE	defGrp		;[7]
	PURGE	DATASEGS	;[7]
	PURGE	CODESEGS	;[7]

;******************************************************************************
;
;	The INITIALIZER macro puts the address of the passed routine
;	into the XI segment.  Each module that gets linked that has
;	an INITIALIZER macro invokation will also put its address
;	in the XI segment.  This has the effect of building up a
;	table of initialization routines that need called.  Crt0
;	will call all of these routines before it calls _main.
;
;******************************************************************************
INITIALIZER	MACRO	pINI
sBegin	XIB			;;[23] XIB and XIE must bracket XI, or
sEnd	XIB			;;[23] initializers will NOT get called!
				;;[23] this code makes sure that USESEG's
				;;[23] are present for XIB and XIE.
sBegin	XI
	DD	&pINI
sEnd	XI
sBegin	XIE			;;[23]
sEnd	XIE			;;[23]
	ENDM

	.LIST
