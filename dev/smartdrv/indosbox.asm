;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;OVERVIEW
;
;	This module contains an asm callable routine for detecting
;if the program is running under a windows VM or dosshell task
;The primary entry point is in_dos_box


zseg    segment word public 'CODE'

	assume  cs:zseg
	assume  ds:zseg

public	in_dos_box

;
;FUNCTION
;	zero flag set if program was run from within a dosshell or
;	window's dos box or desqview
;USES
;	none
in_dos_box proc near
	push	ax
	push	bx
	push	cx
	push	dx
	push	bp
	push	si
	push	di
	push	es
	push	ds

	mov     ax,4B02h        ; Detect_Switcher
	xor     bx,bx           ; bx is defined to be zero in spec DTS API
	mov     es,bx           ; es:di = 0
	mov     di,bx
	int     2fh
	or      ax,ax
	jnz     no_DTS_Responder
	jmp	short	exit_zero_flag_setup
no_DTS_Responder:
	;;;detection code for older MS taskers (<=W3.0)
	mov     ax,4680h ;switcher call out
	int     2fh
	or      ax,ax
	jz      exit_zero_flag_setup
	mov     ax,1600h ;enhanced mode callout
	int     2fh
	test    al,7fh   ;if not low bits set 
			 ;there is a random himem which might set hi bit
	jnz 	multi_tasking

;	int 1
	;;; now check for desqview
	mov	cx,'DE'
	mov	dx,'SQ'
	mov	ax,2B01h
	int	21h
	cmp	al,0ffh
	jne	multi_tasking
	
	or 	al,1	 ; zero flag was set, so unset it
	jmp	short	exit_zero_flag_setup
	ret
multi_tasking:
	xor	ax,ax	 ; zero flag not set, so set it
	;;;	fall through

exit_zero_flag_setup:
	pop	ds
	pop	es
	pop	di
	pop	si
	pop	bp
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
in_dos_box endp


zseg ends

end

