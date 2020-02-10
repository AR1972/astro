;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include	bambi.inc

public	initialize_cache
public	halt_cache

;
;	data from rdata.asm
;
extrn	XMShandlevalid		:byte
extrn	XMShandle		:word
extrn	cache_block_bytes	:word
;
;	routines from xms.asm
;
extrn	initialize_xms		:near
extrn	allocate_xms_memory	:near
extrn	free_xms_memory		:near
;
;	routines fro queueman.asm
;
extrn	initqueue		:near


zseg    segment public 'CODE'

	assume  cs:zseg
	assume  ds:zseg


;
;	INPUT
;		CX number of cache elements

Initialize_Cache proc near

	or	cx,cx
	jz	no_xms

	mov	cs:XMShandlevalid,0
	call	Initialize_XMS
	jc	MemoryError1
	
	mov	ax,cx
	mul	cs:cache_block_bytes
	
	mov	si,1024
	div	si
	mov	dx,ax
	;dx = K to allocate
	call	Allocate_XMS_Memory
	or	ax,ax
	jz	MemoryError2 
	mov	cs:XMShandle,dx
	mov	cs:XMShandlevalid,1
no_xms:
	clc
	ret		

MemoryError1:
	mov	ax,ERROR_NO_XMS_DRIVER
errorout:
	stc
	ret	
MemoryError2:
	mov	ax,ERROR_ALLOCATION_FAILED
	jmp	short errorout
Initialize_Cache endp

;
; INPUT none
; OUTPUT none
Halt_Cache proc near

	cmp	cs:XMShandlevalid,1
	jne	donehalt
	mov 	dx,cs:XMShandle
	call 	Free_XMS_Memory
donehalt:
 	ret
Halt_Cache endp

zseg ends

end

