;*
;*	CW : Character Windows Drivers
;*
;*	Amstrad.asm : for Amstrad PC1512 and PC1640
;*****************************************************************************

	include	csd_head.inc

	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:
;* #0 - standard monochrome text mode (9 x 14 characters)
	DB	fvmAmstradMD
	DB	0ffh				;* any
	DB	7				;* mode
	DW	finstText OR finstMonochrome
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	0, 0, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0b0cH				;* cursor
	DW	0				;* extra (RamFont info)
;* #1 - standard color mode (CGA)
	DB	fvmAmstradCD or fvmAmstradECD or fvmAmstradCM
	DB	0ffh				;* any
	DB	3 
	DW	finstText
	DB	80, 25
	DB	16				;* coMac
	DB	0, 0, 0, 0			;* INFT
	DW	0B800H
	DW	0607H
	DW	0				;* reserved

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

;*****************************************************************************

NonStandard	FvmGetCur

; array to map switch 2-4 settings to fvm
mpSwitchFvm	db	0
		db	0		
		db	fvmAmstradMD
		db	fvmAmstradECD
		db	fvmAmstradCD
		db	fvmAmstradMD
		db	fvmAmstradECD
		db	fvmAmstradCD

;*****************************************************************************
;********** FvmGetCur **********
;*	entry:	DS:DI => driver data
;*	* if first time called, identify the current screen and set fvmCur
;*	*  subsequent calls will return fvmCur
;*	*  After this call: fvmCur is initialized
;*	exit:	AL == fvm for current screen (0 => no supported screen found)
;*		AH == monitor

cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>, <DS>
cBegin	FvmGetCur
	AssertEQ di,OFF_lpwDataCsd
	mov	al,[di].fvmCurAdap
	mov	ah,[di].fvmCurDisp
	or	ax,ax
	jnz	end_fvm_get

; Check for an Amstrad computer
;	    Code is from MS WORD
	xor	bx,bx			; Preset result to invalid function
	mov	ah, 6			; Sub-Function 6:  Get ROS Version
	stc				; Just in case no one answers
	int	015h			; Query the BIOS as to the machine nature
	mov	ax, 0			; Don't change the flags
	jc	end_fvm_get		; Carry still set -> Not Amstrad
	or	bx,bx			; Did the Amstrad BIOS change the register?
	jz	end_fvm_get		; If not, no Amstrad

; This test for a PC1512 is described in section 1.10.2 (Printer
; Control Latch) in the Amstrad PC1640 Technical Reference Manual.

	mov	dx, 03DAh
	cli
	in	al, dx
	mov	dx, 037Ah
	in	al, dx
	sti
	test	al, 020h		; if bit 5 == 1,
	mov	ax, fvmAmstradCM
	jnz	FvmQueryRet		;    we have a PC1512

	xor	ax,ax
	push	ds
	mov	ds,ax
	mov	bl,ds:[0488h]		; read switch settings
	pop	ds
	and	bx,000fh
	shr	bx, 1			; ignore switch 1 (bit 0)
	mov	al, cs:mpSwitchFvm[bx]
FvmQueryRet:
	mov	[di].fvmCurAdap,al
	mov	[di].fvmCurDisp,0FFh	;any monitor
end_fvm_get: ;* ax = fvm
cEnd	FvmGetCur


;*****************************************************************************

	include	csd_std.asm		;* standard init/term

	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************

	include	csd_vram.asm		;* default procs for direct video I/O

	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************

	END

