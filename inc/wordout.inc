;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1989-1991
;
;   Module:   WORDOUT.INC - Basically code for debugging; displays data.
;
;   Version:  0.01
;
;   Date:     May 14, 1992
;
;******************************************************************************
;
;   Modification log:
;
;     DATE    WHO  REVISION  DESCRIPTION
;   --------  ---  --------  --------------------------------------------------
;   05/14/92  RPJ    0.01    Original
;
;******************************************************************************

; -----------------------------------------------------------------------------
; charout is a macro that accepts one argument to print; charout al/charout 'X'
; printcr prints CR/LF
; printtab prints a TAB character
; getchar waits for a keypress, any keypress
; -----------------------------------------------------------------------------
; nibout prints AL's lowest four bits as one hex digit
; byteout prints AL as two hex digits
; wordout prints AX as four hex digits, in proper word-order
; -----------------------------------------------------------------------------
; Examples:
;	dec	ax	; Prepare to load
;	charout	'L'	; DEBUG
;	charout	':'	; DEBUG
;	call	wordout	; DEBUG
;	printcr		; DEBUG -- displays "L:01F8", depending on AX.
;	
; -----------------------------------------------------------------------------

charout	macro	chr
	push	ax
	push	dx
	mov	dl, chr
	mov	ah, 2
	int	21h
	pop	dx
	pop	ax
	endm

printcr	macro
	push	ax
	push	dx
	mov	dl, 13
	mov	ah, 2
	int	21h
	mov	dl, 10
	mov	ah, 2
	int	21h
	pop	dx
	pop	ax
	endm

printtab macro
	charout	9
	endm

getchar	macro
	push	ax
	mov	ah, 8
	int	21h
	pop	ax
	endm

public	nibout
public	byteout
public	wordout

nibout	proc	near
	push	ax
	push	dx

	cmp	al, 0Ah
	jge	no_hex
	add	al, '0'
	mov	dl, al
	mov	ah, 2
	int	21h
	jmp short no_done

no_hex:
	add	al, 'A'-0Ah
	mov	dl, al
	mov	ah, 2
	int	21h

no_done:
	pop	dx
	pop	ax
	ret
nibout	endp

byteout	proc	near
	push	ax
	push	ax

	push	cx
	mov	cl, 4	; four-bit shift
	shr	al, cl	; right here.
	pop	cx
	call	nibout
	pop	ax

	and	al, 0Fh
	call	nibout
	pop	ax

	ret
byteout	endp

wordout	proc	near
	push	ax
	push	ax
	mov	al, ah
	call	byteout
	pop	ax
	call	byteout
	pop	ax
	ret
wordout endp

