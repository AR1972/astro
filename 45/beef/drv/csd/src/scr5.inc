;*
;*	CW : Character Windows
;*
;*	scr5.inc : screen driver data info (OS/2)


;*****************************************************************************
;*	* CursorData structure (VioXxxCurType)
CT	STRUC
	lineStartCt	DW	?
	lineEndCt	DW	?
	widthCt		DW	?
	atrCt		DW	?
CT	ENDS

cbCtMin	EQU	size CT
;*****************************************************************************
;*	* ModeData structure (VioXxxMode)
MD	STRUC
	cbMd		DW	?
	typeMd		DB	?
	colorMd		DB	?
	axMacMd		DW	?
	ayMacMd		DW	?
	hResMd		DW	?
	vResMd		DW	?
;	fmtMd		DB	?
;	attribMd	DB	?
MD	ENDS

cbMdMin	EQU	size MD
;*****************************************************************************
;*	* ConfigData structure (VioGetConfig)
CD	STRUC
	cbCd		DW	?
	adapCd		DW	?
	dispCd		DW	?
	lcbmemCd	DD	?
CD	ENDS

cbCdMin	EQU	size CD
;*****************************************************************************
;*	* FontInfo structure (VioxxxFont)
FI	STRUC
	cbFi		DW	?
	typeFi		DW	?
	axCellFi	DW	?
	ayCellFi	DW	?
	pbDataFi	DD	?
	cbDataFi	DW	?
FI	ENDS

cbFiMin	EQU	size FI

PB	STRUC
	pBufPb		DD	?
	cbPb  		DD	?
	sel0Pb		DW	?
	sel1Pb		DW	?
	sel2Pb		DW	?
	sel3Pb		DW	?
PB	ENDS

cbPbMin EQU	size PB
;*****************************************************************************
;*	* Palette request block (VioXxxState)

;*	* PR is a variable structure.  The number of cov's set/received
;*	* depends on cbPr.  To get info for n palettes starting with coFirstPr
;*	* use:
;*	*	cbPr equ cbPrMin + n * 2
;*	*	localV pr,cbPr
;*	*	mov pr.cbPr,cbPr

PR	STRUC
	cbPr		DW	?
	rtPr		DW	?
	coFirstPr	DW	?
PR	ENDS

rgcovPr	EQU	coFirstPr + 2

cbPrMin	EQU	size PR
cbPr1	EQU	cbPrMin + 2
cbPrMax	EQU	38
;*****************************************************************************

adapMDA		EQU	0
adapCGA		EQU	1
adapEGA		EQU	2
adapVGA		EQU	3

dispMDA		EQU	0
dispCGD		EQU	1
dispEGD		EQU	2

hvioCur		EQU	0			;* Vio Handle (0 for OS/2 1.x)
atrCurOff	EQU	-1

;*****************************************************************************

PushArg	MACRO	arg_list		;; Push multiple registers
	irp	x,<arg_list>
		push	x
	endm
ENDM

Os2Call	MACRO	ofn			;* Call through link table
	call	[bx+lpfn&ofn&Inos]
ENDM


;*****************************************************************************
