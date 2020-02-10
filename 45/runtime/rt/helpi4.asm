	TITLE	HELPI4 - I4 compiler helpers
;***
; HELPI4 - I4 compiler helpers
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	This file contains several compiler helpers for 4 byte integer
;	support not contained in the standard C math packages.
;
;	The routines are for mul, div, mod, and cmp.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	RT_TEXT

	INCLUDE seg.inc


;	for now use C library routines which do modular arithmetic

externFP	__aFlmul
externFP	__aFldiv
externFP	__aFlrem


sBegin	RT_TEXT
assumes cs,RT_TEXT

;***
; B$MUI4(I4 op1,I4 op2)
;
;Purpose:
;	Long integer multiply
;
;Entry:
;	op1 and op2 on stack
;
;Exit:
;	dx:ax = op1*op2
;
;Uses:
;	ax,cx,dx,bx
;
;Exceptions:
;	none
;
;******************************************************************************


cProc	B$MUI4,<FAR,PUBLIC>,<> 	

cBegin	nogen

	jmp	__aFlmul

cEnd	nogen



;***
; B$DVI4(I4 op1,I4 op2)
;
;Purpose:
;	Long integer divide
;
;Entry:
;	op1 and op2 on stack
;
;Exit:
;	dx:ax = op1/op2
;
;Uses:
;	ax,cx,dx,bx
;
;Exceptions:
;	hardware divide overflow
;
;******************************************************************************


cProc	B$DVI4,<FAR,PUBLIC>,<> 	

cBegin	nogen

	jmp	__aFldiv

cEnd	nogen



;***
; B$RMI4(I4 op1,I4 op2)
;
;Purpose:
;	Long integer remainder
;
;Entry:
;	op1 and op2 on stack
;
;Exit:
;	dx:ax = op1 mod op2
;
;Uses:
;	ax,cx,dx,bx
;
;Exceptions:
;	hardware divide overflow
;
;******************************************************************************


cProc	B$RMI4,<FAR,PUBLIC>,<> 	

cBegin	nogen

	jmp	__aFlrem

cEnd	nogen


;***
; B$CPI4(I4 op1,I4 op2) - long integer compare
;
;Purpose:
;	Long integer compare
;	This routine returns a signed compare result.
;
;Entry:
;	op1 and op2 on stack
;
;Exit:
;	flags = signed compare of op1 ? op2 (in AH for Windows)
;
;Uses:
;	ax,cx,dx,bx
;
;Exceptions:
;	hardware divide overflow
;
;******************************************************************************

cProc	B$CPI4,<FAR,PUBLIC>,<AX>

	parmD	op1
	parmD	op2

cBegin
	mov	ax,word ptr (op1+2)	; signed compare high words
	cmp	ax,word ptr (op2+2)
	jne	LCMPretx		;not equal - return flags

	mov	ax,word ptr (op1)	; unsigned compare low words
	cmp	ax,word ptr (op2)

	lahf				; (ah) = "unsigned" flags
	and	ax,4100h		; (ah,al) = interesting flags,0
	shr	ax,1			; move C0(cf) to high bit of al(sf)
	shl	ah,1			; restore C3(zf) to original position
	or	ah,al			; (ah) = signed flags
	sahf				; set flags

LCMPretx:				

cEnd


;***
; B$CMI4(I4 op1,I4 op2) - long integer compare
;
;Purpose:
;	Added with [4].
;	Long integer compare
;	This routine performs a signed comparison, but returns the flags
;	for the use of unsigned jumps.
;	This allows LAHF to save all required flags for Windows and other
;	QB uses.
;
;Entry:
;	op1 and op2 on stack
;
;Exit:
;	flags = signed compare of op1 ? op2 (in AH for Windows)
;
;Uses:
;	cx,dx,bx
;
;******************************************************************************

cProc	B$CMI4,<FAR,PUBLIC>

	parmD	op1
	parmD	op2

cBegin
	push	ax			;preserve AX for Compiler

	mov	ax,word ptr (op1+2)	; signed compare high words
	cmp	ax,word ptr (op2+2)
	stc				;BELOW == CF set
	jl	LCMPret 		;exit if LESS (BELOW)
	clc				;ABOVE == CF clr
	jg	LCMPret 		;exit if GREATER (ABOVE)
	mov	ax,word ptr (op1)	;unsigned compare low words
	cmp	ax,word ptr (op2)
LCMPret:
	pop	ax			; restore AX for compiler

cEnd


sEnd	RT_TEXT

end
