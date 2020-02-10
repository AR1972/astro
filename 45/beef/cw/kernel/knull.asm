;*
;*	COW : Character Oriented Windows
;*
;*	knull.asm : Kernel NULL segment / Segment ordering
;*
;*	* Kludge Segments to force proper segments
;*
;*	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;*	* WARNING : only change this file if you know exactly what you are doing
;*	* WARNING : THIS FILE MUST BE POST-PROCESSED BY SEGORDER !!
;*	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

;*	* NOTE :
;*	* the kernel buffer must be after all non-zero initialized data,
;*	*  and before the _BSS segment, this allows loading in the DGROUP +
;*	*  rlbFix in 1 read and zero filling the remaining (thus starting with
;*	*  rlbFix already in the work buffer
;*	* In order for the loader to know where this buffer starts, the address
;*	*  is placed at address 4 in the DGROUP (part of NULL).
;*	*  (the loader asserts that the buffer is right after BSS data), also
;*	*  the work buffer is NOT zero initialized.

include	version.inc

IFNDEF	FOR_QC
public	__acrtused
	__acrtused = 1
ENDIF	; !FOR_QC

;*	* Funny segment order *
_TEXT	SEGMENT PARA PUBLIC 'CODE'
_TEXT	ENDS

NULL	SEGMENT	PARA PUBLIC 'BEGDATA'
NULL	ENDS
_DATA	SEGMENT WORD PUBLIC 'DATA'
_DATA	ENDS
CONST	SEGMENT WORD PUBLIC 'CONST'
CONST	ENDS
KERNEL_BUFF SEGMENT WORD PUBLIC 'BEFORE_BSS'
	EXTRN	rgbKernelBuff:BYTE
KERNEL_BUFF ENDS
_BSS	SEGMENT PARA PUBLIC 'BSS'
_BSS	ENDS

DGROUP	GROUP	NULL,_DATA,CONST,KERNEL_BUFF,_BSS

;*****************************************************************************

	PUBLIC	pbKernelBuff
	PUBLIC	bpOldStack, ssOldStack
	PUBLIC	pStackTop, pStackMin, pStackBot

NULL	SEGMENT
		DW	0		;* first word of DDS is zero
bpOldStack	DW	0		;* old stack's BP
ssOldStack	DW	0		;* old stack's SS
		DW	0		;* reserved
pbKernelBuff	DW	OFFSET DGROUP: rgbKernelBuff	;* offset $A
pStackTop	DW	0	;* top of stack
pStackMin	DW	0	;* minimum value of SP (old stack's SP)
pStackBot	DW	0	;* bottom of stack
NULL	ENDS


	END

