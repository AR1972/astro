	TITLE	HELPFP87 - IEEE floating point compiler helpers (8087 version)
;***
; HELPFP87 - IEEE floating point compiler helpers (8087 version)
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	This file contains several compiler helpers for floating point
;	support not contained in the standard C math packages.
;
;	The routines load and store 4 byte integers with the 8087 stack.
;	A fast compare is also provided.
;
;******************************************************************************
	.8087
	INCLUDE switch.inc
	INCLUDE rmacros.inc


	useSeg	MT_TEXT

	INCLUDE seg.inc

sBegin	MT_TEXT
assumes cs,MT_TEXT

;***
; B$fcompz - Compare 8087 top stack entry with zero
;
;Purpose:
; This routine is used to compare the top 8087 stack entry with zero. Note!
; Carry is reversed from an equivalent CMP x,0!
;
;Entry:
; ST(0)
;
;Exit:
;	Z   C	Indicates
;	0   0	ST(1) < 0
;	0   1	ST(1) > 0
;	1   0	ST(1) = 0
;	1   1	Error
;
;Preserves: (optional)
; All
;
;Exceptions:
;
;******************************************************************************
cProc	B$fcompz,<FAR,PUBLIC>
cBegin
	FLDZ			;Throw on a zero, and fall in
cEnd	nogen

;***
; B$fcomp - Compare and pop 1 8087 stack entry
;
;Purpose:
; This routine is used to compare the top two 8087 stack entries and pop 1.
;
;Entry:
; ST(0) and ST(1)
;
;Exit:
;	Z C Indicates
;	0 0 ST(0) > ST(1)
;	0 1 ST(0) < ST(1)
;	1 0 ST(0) = ST(1)
;	1 1 Error
;
; ST(0) <= ST(1)
;
;Preserves: (optional)
; All
;
;*****************************************************************************
cProc	B$fcomp,<PUBLIC,FAR>,AX
localW	result
cBegin
	FCOMP			; compare and ST(0)
	FSTSW	result		; get compare status
	FWAIT			; synchronize
	MOV	AH,BYTE PTR (result + 1) ; get compare status
	SAHF			; set flags like unsigned compare
cEnd

;***
; B$fcompp - Compare and pop 2 8087 stack entries
;
;Purpose:
; This routine is used to compare the top two 8087 stack entries and pop both.
;
;Entry:
; ST(0) and ST(1)
;
;Exit:
;	Z C Indicates
;	0 0 ST(0) > ST(1)
;	0 1 ST(0) < ST(1)
;	1 0 ST(0) = ST(1)
;	1 1 Error
;
; ST(0) <= ST(1)
;
;Preserves: (optional)
; All
;
;*****************************************************************************
cProc	B$fcompp,<PUBLIC,FAR>,AX
localW	result
cBegin
	FCOMPP			; compare and pop ST(0) and ST(1)
	FSTSW	result		; get compare status
	FWAIT			; synchronize
	MOV	AH,BYTE PTR (result + 1) ; get compare status
	SAHF			; set flags like unsigned compare
cEnd

sEnd	MT_TEXT


end
