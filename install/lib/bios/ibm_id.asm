; ========================================================

COMMENT #

	IBM_ID.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 03/03/89

END COMMENT #

; ========================================================

include BIOS_IO.INC
include	MODEL.INC

; ========================================================

.DATA

; ========================================================

COPYR_LEN	EQU	15

CopyrightIBM	db	'The IBM Personal Computer Basic'

; ========================================================

.CODE

; ========================================================
; Check for a ROM containing IBM basic and returns false
; 0 if the ROM is found else NOT 0.
;
; int IsNotIBM( void );
;
; ========================================================

IsIBM PROC	USES ES DI SI

	mov	AX,0fac0h		; Load segment ES
	mov	ES,AX
	mov	DI,09bh			; Put offset in DI
	mov	SI,OFFSET CopyrightIBM	; Set up for cmpstring
	mov	CX,COPYR_LEN		; Put length of string in CX
	repz	cmpsw
	jz	IsTrueIBM

	mov	AH,22h
	int	15h
	jc	NotIBM

	or	AH,AH
	jnz	NotIBM

IsTrueIBM:
	mov	AX,1
	ret	

NotIBM:
	xor	AX,AX
	ret	

IsIBM ENDP

; ========================================================

	END

; ========================================================
