;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center 				*
;									*
;	STARTUP.ASM							*
;									*
;************************************************************************

TITLE	Startup

include	keyboard.inc

		EXTRN	runFrontEnd:near

	PUBLIC cmdLineLen
	PUBLIC intNumber
	PUBLIC functionNumber
	PUBLIC tsrLoaded
	
	PUBLIC parameters
	PUBLIC paramsOffset
	PUBLIC paramsSegment

	PUBLIC originalInt
	PUBLIC orgIntOffset
	PUBLIC orgIntSegment


	PUBLIC accessIntHandler


_TEXT	SEGMENT  WORD PUBLIC 'CODE'
_TEXT	ENDS
CONST	SEGMENT  WORD PUBLIC 'CONST'
CONST	ENDS
_DATA	SEGMENT  WORD PUBLIC 'DATA'
_DATA	ENDS
c_common segment word public 'BSS'
c_common ends
_BSS	SEGMENT  WORD PUBLIC 'BSS'
_BSS	ENDS
DGROUP	GROUP _TEXT, CONST, _DATA, c_common, _BSS

_TEXT	segment
	assume cs:_TEXT
	assume ds:nothing
        assume es:nothing
	assume ss:nothing

display 	MACRO	string
        	mov	dx,OFFSET string
	        mov	ah,9
        	int	21h
	        ENDM



;------------------------------------------------------------------------
	        org 80h
cmdLineLen	label   byte

;------------------------------------------------------------------------
	        org     100h
start:
                jmp     runFrontEnd		


intNumber	db	DEFAULT_INTERRUPT
functionNumber	db	ACCESS_FUNCTION
tsrLoaded       dw      TSR_HERE

parameters	label	dword
paramsOffset	dw	0
paramsSegment	dw	0

originalInt	label	dword
orgIntOffset	dw	0
orgIntSegment	dw	0


;----------------------------------------------------------------------
	PUBLIC accessIntHandler
accessIntHandler	proc	far

		jmp	accessIntHandler10

	PUBLIC programStamp
	PUBLIC pgm_stamp_len
	PUBLIC pgm_stamp_ofs

programStamp	db	'ACCESSv1.00', 0
pgm_stamp_ofs	dw	programStamp-accessIntHandler
pgm_stamp_len	dw	$-programStamp

accessIntHandler10:
		push    ds
                push    cs
                pop     ds
		assume	ds:_TEXT

		cmp	ah, functionNumber	        ; is this a call for an AccessDOS function?
		jne	notAccess		        ; no -- go to original int handler
		cmp	al, TSR_DETECT  	        ; does user want to detect if tsr loaded?
		je	retDetect
		cmp	al, TSR_PARAMS  	        ; does user want address of parameters?
		je	retParams

notAccess:
                pop     ds
                assume  ds:nothing
		jmp	cs:originalInt
                iret

retDetect:
		mov	ax, tsrLoaded		        ; yes, the tsr is loaded
                pop     ds
                assume  ds:nothing
		iret

retParams:
		mov	ax, paramsOffset	        ; return parameter block address
		mov	bx, paramsSegment
                pop     ds
                assume  ds:nothing
		iret

accessIntHandler	endp


_TEXT		ends
	        end	start

