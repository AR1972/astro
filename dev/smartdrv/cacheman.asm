;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include	bambi.inc

public	buffer_to_cache
public	cache_to_buffer
public	reinitialize_cache

;
;	data from rdata.asm
;
extrn	cache_block_bytes	:word
extrn	cache_block_words	:word
extrn	in_bambi		:byte
extrn	XMShandlevalid		:byte
extrn	XMShandle		:word

;
;	routines from xms.asm
;
extrn	block_write		:near
extrn	block_read		:near
extrn	reallocate_xms_memory	:near
;
;	routines from queueman.asm
;
extrn	initqueue		:near



zseg    segment public 'CODE'

	assume  cs:zseg
	assume  ds:zseg



; BUG BUG assumes ptr+size does not wrap segment
; INPUT
;	ES:DI = Near Buffer 
;       CX = number of words to copy
;	BP = Cache entry index	
;	SI = offset into cache buffer

Buffer_To_Cache proc near

	push	bp	
	mov	ax,cs:cache_block_words
	mul	bp			; use word index, multiply by words
	add	ax,si			; note that on 386 mul will be 9 clocks
	adc	dx,0	

	mov	bx,cs:XMShandle
	call	Block_Write
	pop	bp

;	push bp
;	push ax
;	call data_check_sum
;	shl bp,1
;	mov WORD PTR cs:buffer_sum[bp],ax
;	pop ax
;	pop bp
	ret

Buffer_To_Cache endp

; INPUT
;	ES:DI = Near Buffer 
;       CX = number of words to copy
;	BP = Cache entry index	
;	SI = offset into cache buffer
Cache_To_Buffer proc near


;	push ax
;	push bp
;	call data_check_sum
;	shl bp,1
;	cmp WORD PTR cs:buffer_sum[bp],ax
;	pop bp
;	pop ax

cache_to_buffer_no_check_sum:

	mov	ax,cs:cache_block_words
	mul	bp

	add	ax,si
	adc	dx,0

	mov	bx,cs:XMShandle
	jmp	Block_Read	

Cache_To_Buffer endp


;
;	INPUT
;		CX number of cache elements
;	OUTPUT
;		none
reinitialize_cache proc near

	inc	cs:in_bambi

	push	cx

	mov	ax,cx
	mul	cs:cache_block_bytes
	
	mov	si,1024
	div	si
	mov	bx,ax
	;bx = K to allocate
	;dx = handle
	mov	dx,cs:XMShandle
	call	ReAllocate_XMS_Memory
	or	ax,ax
	pop	cx
	jz	realloc_error1

	call	InitQueue
	
	dec	cs:in_bambi
	clc
	ret		

realloc_error1:
	mov	ax,ERROR_ALLOCATION_FAILED	
	dec	cs:in_bambi
	stc
	ret	
reinitialize_cache endp

zseg ends

end

