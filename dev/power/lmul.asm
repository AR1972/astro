	page	,132
	title	lmul - long multiply routine
;***
;lmul.asm - long multiply routine
;
;	Copyright (c) 1985-1991, Microsoft Corporation. All rights reserved.
;
;Purpose:
;	Defines long multiply routines
;	Both signed and unsigned routines are the same, since multiply's
;	work out the same in 2's complement
;
;Revision History:
;	11-29-83  DFW	initial version
;	06-01-84  RN	modified to use cmacros
;	04-17-85  TC	ignore signs since they take care of themselves
;			do a fast multiply if both hiwords of arguments are 0
;	10-10-86  MH	slightly faster implementation, for 0 in upper words
;	03-20-89  SKS	Remove redundant "MOV SP,BP" from epilogs
;	05-18-89  SKS	Preserve BX
;	11-28-89  GJF	Fixed copyright
;
;       06-05-91  MD    Stolen from C runtime for use with POWER.EXE
;
;*******************************************************************************


.xlist
include mm.inc
.list

Trans_Code      segment word public 'CODE'
Trans_Code      ends

Trans_Data      segment word public 'DATA'
Trans_Data      ends

;***
;lmul - long multiply routine
;
;Purpose:
;	Does a long multiply (same for signed/unsigned)
;	Parameters are not changed.
;
;Entry:
;	Parameters are passed on the stack:
;		1st pushed: multiplier (DWORD)
;		2nd pushed: multiplicand (DWORD)
;
;Exit:
;	DX:AX - product of multiplier and multiplicand
;	NOTE: parameters are removed from the stack
;
;Uses:
;	CX
;
;Exceptions:
;
;*******************************************************************************

Trans_Code      segment
        assume cs:Trans_Code, ds:Trans_Data

lmul    proc    near
        public  lmul

A	equ	BPARGBAS[bp]	; stack address of a
B	equ	BPARGBAS+4[bp]	; stack address of b

;
;	AHI, BHI : upper 16 bits of A and B
;	ALO, BLO : lower 16 bits of A and B
;
;	      ALO * BLO
;	ALO * BHI
; +	BLO * AHI
; ---------------------

        push    bp
        mov     bp,sp

	mov	ax,HIWORD(A)
	mov	cx,HIWORD(B)
	or	cx,ax		;test for both hiwords zero.
	mov	cx,LOWORD(B)
	jnz	hard		;both are zero, just mult ALO and BLO

	mov	ax,LOWORD(A)
	mul	cx

	pop	bp
	ret

hard:
	push	bx

	mul	cx		;ax has AHI, cx has BLO, so AHI * BLO
	mov	bx,ax		;save result

	mov	ax,LOWORD(A)
	mul	word ptr HIWORD(B) ;ALO * BHI
	add	bx,ax		;bx = ((ALO * BHI) + (AHI * BLO))

	mov	ax,LOWORD(A)	;cx = BLO
	mul	cx		;so dx:ax = ALO*BLO
	add	dx,bx		;now dx has all the LO*HI stuff

	pop	bx
        mov     bp,sp
        pop     bp
        ret
lmul    endp

Trans_Code      ends

	end
