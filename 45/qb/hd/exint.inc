;exint.inc - executor internal declarations

;===================================================================

EXINT_INC = -1

;Constants

LOWUND	EQU LOW UNDEFINED

;The following are masks that identify the exponent bits in the exponent word.
MATH_R4SignMask EQU 8000h
MATH_R8SignMask EQU 8000h
MATH_R4ExpMask	EQU 3f80h
MATH_R8ExpMask	EQU 3ff0h

;The following are the high words of IEEE reals 0 through 10.
LitR81HighWord	EQU 3ff0h
LitR82HighWord	EQU 4000h
LitR83HighWord	EQU 4008h
LitR84HighWord	EQU 4010h
LitR85HighWord	EQU 4014h
LitR86HighWord	EQU 4018h
LitR87HighWord	EQU 401ch
LitR88HighWord	EQU 4020h
LitR89HighWord	EQU 4022h
LitR810HighWord EQU 4024h

LitR40HighWord	EQU    0h
LitR41HighWord	EQU 3f80h
LitR42HighWord	EQU 4000h
LitR43HighWord	EQU 4040h
LitR44HighWord	EQU 4080h
LitR45HighWord	EQU 40A0h
LitR46HighWord	EQU 40C0h
LitR47HighWord	EQU 40E0h
LitR48HighWord	EQU 4100h
LitR49HighWord	EQU 4110h
LitR410HighWord EQU 4120h

;===================================================================
;Math library entrypoints


extrn	B$RMI4:FAR
extrn	B$MUI4:FAR
extrn	B$DVI4:FAR
extrn	B$SCAT:FAR
extrn	B$SCMP:FAR
extrn	B$FLEN:FAR
extrn	B$ISCMPNoRel:FAR


;======================================================================
;Math related utilities
;
ife	EXMATHOP_ASM
extrn	exMathErrOVF:near
extrn	exMathErrDV0:near
endif			;EXMATHOP_ASM

;===================================================================
;Common runtime entrypoints

ife	EXFOR_ASM
sBegin	DATA
extrn	Stat:word
sEnd	DATA
endif

ife	EXARYUTL_ASM
extrn	ResolveArray:near
endif

ife	EXREC_ASM
extrn	MoveRec:near
endif

ife	EXTORT_ASM
externNP	DispMov
externNP	DispMovAx
externNP	DispMovDxAx
externNP	Disp
externNP	DispAx
externNP	DispDxAx
externNP	GetEsDi
endif	;ife EXTORT_ASM


;***********************************************************************
;Equates/Macros to handle different versions
;
ExToRtDispMovSd	equ	<ExToRtDispMovAx>


pVarBx		equ	bx+di
pFrame		equ	bx
FrameReg	equ	bx

GetpFrame	MACRO
	mov	bx,[bx+di]		;;Get oBP
	add	bx,bp
	ENDM

assumes DS, DATA



RestorePcode MACRO
	call	GetEsDi
	ENDM

RestorePcodeVar MACRO
	call	GetEsDi
	ENDM

RestoreVar MACRO
	GETSEG	ds,[grs.GRS_SbVar],,<SIZE,LOAD>
	ENDM


;===================================================================
;Executor Macros
;

	;LODSWTX loads a word from the text table

LODSWTX equ	<lods word ptr es:[si]>

PTRTX	equ	<word ptr es:>


;===================================================================
;MakeExe makes an executor entry point.


MakeExe MACRO	exeName,opName,Type
ifnb	<Type>				;; ET_ type is present
	dw	&opName+&Type*(OPCODE_MASK+1)
else
	dw	&opName 		;;Opcode for descan/list
endif

	public	&exeName
&exeName:				;;Executor entry definition

	ENDM


	;SameExe
	;Equates a new executor name to an existing executor

SameExe	MACRO	New,Old
New	=	Old
	public	New
	ENDM

;SkipExHeader
;
;Purpose:
;   skip over an executor header.
;
;NOTE:
;   this skip must be the same size as the ex header.

SkipExHeader	equ	<db 3dh>    ;Eat the next word as part of cmp ax,data


;DispMac - dispatch to the next pcode
;
;Purpose:
;   Executor dispatch definition
;   Nonrelease versions will dispatch through a central dispatcher,
;   making debugging slightly easier.
;   Release versions will dispatch directly from the end of each executor.
;	LQB version uses central dispatcher to slow it down a bit.


DispMac equ	<jmp Dispatch>



OTX_OTXEXIT equ 0	;offset in procedure pcode table to where an oTx
			;is stored (by the execute scanner). This oTx is to
			;the start of the pcode to exit the procedure


sBegin	CODE
ife	EXMISC_ASM
externNP	Dispatch
endif			;ife EXMISC_ASM
ife	EXPROC_ASM
externNP	SetSP
endif			;ife EXPROC_ASM
sEnd	CODE

;
;======================================================================
;
; Macro fpoptwo simply pops two elements of the numeric stack.  It turns out
; that the best expansion of this is different in Altmath then in emulator
; math.
; (Added by GJ)
fpoptwo MACRO
	fcompp			;; pop two things of the stack
	ENDM	; fpoptwo
;
;======================================================================
