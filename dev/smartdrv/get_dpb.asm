;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include dpb.inc

;
;OVERVIEW
;	The module contains an asm callable function for
;getting a pointer to a drive's DPB.
;	This function is similar to the DOS call with the same name,
;but does not access the drive.
;	The primary entry point is get_dpb


zseg    segment public 'CODE'

	assume  cs:zseg,ds:nothing,es:nothing

PUBLIC	get_dpb
extrn	dos_3x:word

;
;INPUT
;	dl = drive number
;OUTPUT
;	no carry
;	  ds:bx -> DPB for this drive
;	carry
;	  drive not found
;
;USED
;	ax
get_DPB proc near
	push	ax				;preserve registers
	push	cx
	push	es
	mov	ah,52h				;get dpb chain
	int	21h
	;;;	es:bx->DPB chain
	push	es
	pop	ds
	assume	ds:nothing
	lds	bx,ds:[bx]
	xor	ch,ch
	mov	cl,dl
	dec	cl
	jz	found_DPB	
next_DPB:
	add	bx,dos_3x		; adjust for DOS 3.x DPB
	cmp	WORD PTR ds:[bx].dpb_next_dpb,-1	
	je	error_drive_not_found
	lds	bx,ds:[bx].dpb_next_dpb
	loop	next_DPB
found_DPB:
	pop	es
	pop	cx
	pop	ax
	clc
	ret
error_drive_not_found:
	pop	es
	pop	cx
	pop	ax
	stc
	ret	
get_DPB endp

zseg	ends
end

