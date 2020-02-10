;***
; BASLIBMA.INC - 26-Aug-86 - General Utility Macro Definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; General purpose runtime macros.
;
;******************************************************************************
;
;	Undefined switch initialization
;
NDEF	MACRO	switch
.xcref
ifndef	switch
switch= 0
endif
.cref
	endm

NDEFV	MACRO	switch,vl
.xcref
ifndef	switch
switch= vl
endif
.cref
	endm
;******************************************************************************
;
;	Conditional text output
;
OUTIF	MACRO	vl,txt
ndef	vl
if	vl
	%out	txt
endif
	endm

OUTELSE MACRO	vl,txt
ndef	vl
ife	vl
	%out	txt
endif
	endm

;******************************************************************************
;
;	Special Table Generation Macros 	(formerly TABMAC.INC)
;
ENTORG	MACRO	First_entry
	.XCREF
___ZZZ= First_entry
	.CREF
	ENDM

ENT	MACRO	Lab,Incr
Lab=	___ZZZ
	.XCREF
___ZZZ= ___ZZZ+Incr
	.CREF
	ENDM

ENTI	MACRO	Incr
	.XCREF
___ZZZ= ___ZZZ+Incr
	.CREF
	ENDM

;******************************************************************************
;
; Skip execution of the next "n" bytes. Eats them up in a one byte register
; compare instruction. Needless to say, flags are lost.
;

SKIP	MACRO	LEN

IF	LEN EQ 1		;;Is length 1?
	DB	03CH		;;CMP AL,...
ENDIF				;;IF LENGTH EQ 1

IF	LEN EQ 2		;;Is length 2?
	DB	03DH		;;CMP AX,...
ENDIF				;;IF LENGTH EQ 2

IF	LEN GT 2
	.ERR	SKIP MACRO: ILLEGAL SKIP LENGTH: LEN ;;Cause an error
	%OUT	SKIP MACRO: ILLEGAL SKIP LENGTH: LEN ;;Cause an error
ENDIF				;;IF LENGTH GT 2
	ENDM

;***
;PUSHI macro - push immediate data word
;
;Entry:
;	reg - register to use as intermediate in the case we're not
;		building for a 286 environment
;	wData - a word of data to be pushed
;Exit:
;	none.
;Modifies:
;	Will alter the given register if not HP_286.
;**************************************************************************
PUSHI 	MACRO	reg,wData
;if	286 instructions are enabled, use a real immediate push
;	PUSH	wData
;else
	MOV	reg,wData
	PUSH	reg
;endif
	endm
	.LIST
