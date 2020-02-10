	page	,132
	title	xtoa - convert integers/longs to ASCII string
;***
;xtoa.asm - convert integers/longs to ASCII string
;
;	Copyright (c) 1985-1991, Microsoft Corporation. All rights reserved.
;
;Purpose:
;	The module has code to convert integers/longs to ASCII strings.
;
;Revision History:
;	01-06-85  GFW	initial version
;	02-01-85  RN	fixed itoa so not sign-extended to 32 bits if radix
;			is not 10 (-1 in base 16 should be "ffff", not
;			"ffffffff")
;	02-05-85  RN	split itoa, ltoa, and ultoa out into separate modules
;			so user can redefine any or all of them without
;			running into 'bundling' problems and naming conflicts
;			when s/he links
;	11-22-89  GJF	Fixed copyright
;
;       06-05-91  MD    Stolen from C runtime for use with POWER.EXE
;
;*******************************************************************************


Trans_Code      segment word public 'CODE'
Trans_Code      ends

Trans_Data      segment word public 'DATA'
Trans_Data      ends

Trans_Code      segment
        assume  cs:Trans_Code, ds:Trans_Data

get_dec       proc    near
        public  get_dec

	mov	cx,10
        xor     bl,bl

;	ds:di = buffer
;	dx:ax = value
;	cx = radix
;	bl = 1 if signed, 0 if unsigned

	push	di			; save start of buffer
	push	ds
	pop	es			; ds = es = segment of buffer
	cld
	xchg	bx,ax			; dx:bx = number , ax = signed
	or	al,al
	jz	uxtoa			; unsigned conversion

	cmp	cx,10			; check for radix = 10
	jne	uxtoa			;   no - treat as unsigned

	or	dx,dx			; test for negative number
	jns	uxtoa

	mov	al,'-'
	stosb				; stuff out -
	neg	bx			; negate dx:bx
	adc	dx,0
	neg	dx

uxtoa:
	mov	si,di			; save start of number

divdown:
	xchg	ax,dx			; divide hi
	xor	dx,dx
	or	ax,ax
	jz	nohigh			; save a divide
	div	cx			; dx = rem, ax = hi div

nohigh:
	xchg	ax,bx			; ax = lo, bx = hi div
	div	cx			; dx = rem, bx:ax = div
	xchg	ax,dx			; ax = rem, bx:dx = div
	xchg	dx,bx			; ax = rem, dx:bx = div (tight!!!!)
	add	al,'0'
	cmp	al,'9'
	jbe	isadig			; is a digit already
	add	al,'a'-'0'-10		; convert to letter

isadig:
	stosb
	mov	ax,dx
	or	ax,bx
	jnz	divdown 		; crack out next digit

;	reverse digits

	mov	[di],al 		; stuff string terminator

revloop:
	dec	di			; point back to last char
	lodsb				; exchange bytes
	xchg	[di],al
	mov	[si-1],al
; The following is equivalent to "cmp si,(di-1)"
; but avoids segment wrap in case DI == 0
	lea	ax,[si+1]
	cmp	ax,di			; are we halfway?
	jb	revloop 		;   no

	pop	ax			; buffer offset
        ret

get_dec endp

Trans_Code      ends


	end
