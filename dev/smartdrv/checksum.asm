;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */



PUBLIC  data_check_sum
PUBLIC  buffer_sum

extrn	QueueLength:WORD
extrn	cache_block_words:WORD
extrn	cache_to_buffer_no_check_sum:near


zseg    segment public 'CODE'

	assume  cs:zseg

CHECKSUMBUFFERSIZE 	equ	1000h
check_sum_buffer	db	CHECKSUMBUFFERSIZE dup(?)
buffer_sum		dw	CHECKSUMBUFFERSIZE dup(?)

;
; INPUT
;	BP = buffer to checksum
; OUTPUT
;	AX = checksum for this buffer
;
data_check_sum proc near
	int	3

	push	es
	push	bp
	push	di
	push	si
	push	bx
	push 	cx
	push	dx

	push	cs
	pop	es

	mov	di,offset cs:check_sum_buffer

	push	cx
	mov	cx,cs:cache_block_words
	xor	si,si
	call	Cache_To_Buffer_no_check_sum
	pop	cx

	xor	ax,ax
	mov	cx,cs:cache_block_words
buffer_sum_loop:
	mov	bx,cx
	dec	bx
	add	bx,bx
	add	ax,WORD PTR cs:check_sum_buffer[bx]
	loop	buffer_sum_loop
	pop	dx
	pop	cx
	pop	bx
	pop	si
	pop	di
	pop	bp
	pop	es
	ret		
data_check_sum endp

zseg ends

end
