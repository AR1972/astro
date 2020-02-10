;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */


;
;OVERVIEW
;       This module contains an asm callable function for
;displaying a dword value in decimal, right justifed with commas
;The primary entry point is display_dec_dword
;


zseg    segment public 'CODE'

	assume  cs:zseg,ds:zseg,es:zseg

PUBLIC  display_dec_dword
PUBLIC  set_thousands_separator


separator_char  db      ','

;Routines to output 32-bit decimal numbers, space justified, w/commas

radix   dw      10              ; number base for output

obuf    db      10 dup (?)      ; buffer


;FUNCION
;       display 32-bit value to stdout in pretty-display decimal
;       (see display_dec_dword below)
;INPUT
;       DX:BX = value to be displayed
;OUTPUT
;       none
;USES
;       ALL
dec_output proc near

	push    cs              ; point ds/es -> our code segment
	pop     es
	push    cs
	pop     ds
	mov     di,offset obuf
	mov     si,di           ; si/di -> buffer
	mov     cx,length obuf
	mov     al,' '          ; space fill
	cld
	rep     stosb

;       Leave di -> end of output buffer + 1

	mov     ax,dx           ; get number into ax:bx

div_loop:
	xor     dx,dx
	div     radix           ; divide high 16 bits by radix
	xchg    ax,bx           ; high 16 -> bx, low 16 -> ax
	div     radix           ; divide low 16 bits by radix
	xchg    ax,bx           ; low 16 -> bx, high 16 -> ax
	add     dl,'0'          ; convert remainder to ASCII
	dec     di
	mov     byte ptr [di],dl        ; save remainder
	or      ax,ax           ; loop until result of divide is zero
	jnz     div_loop
	or      bx,bx
	jnz     div_loop

;       Now our number is in the buffer.  Output it with comma insertion.

	call    o1dig
	call    comma3
	call    comma3
comma3:
	cmp     al,' '          ; was last character a space?
	jz      comma3_no       ;  no comma if not
	mov     al,separator_char       ; insert comma once we hit real digits
comma3_no:
	call    cofa
	call    o1dig
	call    o1dig
o1dig:
	lodsb
	cmp     al,' '
	je      dont_pad

cofa:
	push    ax
	mov     dl,al
	mov     ah,2
	int     21h             ; output char through DOS
	pop     ax
dont_pad:
	ret
dec_output endp

;
;FUNCION
;       display 32-bit value to stdout in pretty-display decimal
;       (see display_dec_dword below)
;INPUT
;       DX:BX = value to be displayed
;OUTPUT
;       none
;USES
;       none
display_dec_dword proc near
	push    ax
	push    bx
	push    cx
	push    dx
	push    si
	push    di
	push    es
	push    ds
	call    dec_output
	pop     ds
	pop     es
	pop     di
	pop     si
	pop     dx
	pop     cx
	pop     bx
	pop     ax
	ret
display_dec_dword endp



CountryBuffer struc
			Date_Format     db  2 dup(?)
			Currency_Symbol db  5 dup(?)
			Thousands_Sep   db  2 dup(?)
			Decimal_Sep     db  2 dup(?)
			Date_Sep        db  2 dup(?)
			Time_Sep        db  2 dup(?)
			Currency_Pos    db  ?
			Num_Decimals    db  ?
			Time_Format     db  ?
			Case_Mapping    dd  ?
			Data_Sep        db  2 dup(2)
			Reserved        db 10 dup(?)
			;;;;size of structure must be at least 34 bytes!!
CountryBuffer ends

nationdata      CountryBuffer <>

set_thousands_separator proc near
	lea     dx, nationdata          ; DS:DX points to NationBuffer
	mov     ax,3800h                        ; Get Current Country info
	int     21h                             ; If success, Carry clear and BX = country code
	jc      use_default_separator
	mov     al,nationdata.thousands_sep
	mov     separator_char,al

use_default_separator:
	ret

set_thousands_separator endp

zseg    ends
end
