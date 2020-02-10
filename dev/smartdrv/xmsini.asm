;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;OVERVIEW
;	This module contains the initialization portion for xms.asm.
;Please see xms.asm for more info
;

include xms.inc

public	allocate_xms_memory
public	free_xms_memory
public  initialize_xms
public	query_free_xms_memory

extrn	XMSControl		:dword

zseg    segment public 'CODE'

	assume  cs:zseg
	assume  ds:zseg

;
;	local data
;

;
; INPUT 
;	none
; OUTPUT
;	carry if no XMS driver found
;
Initialize_XMS	proc	near
	push	ax
	mov	ax,XMS_MULTIPLEX SHL 8 + XMS_INSTALL_CHECK
	int	2Fh
	cmp	al,80h			; Q: installed
	jne	cXMS_no_driver		; N: set error, quit
;
; get the XMS control functions entry point, save it, we
; need to call it later.
;
	push	es
	push	bx
	mov	ax,XMS_MULTIPLEX SHL 8 + XMS_FUNCTION_ADDR
	int	2Fh
	mov	word ptr cs:[XMScontrol], bx
	mov	word ptr cs:[XMScontrol+2],es
	pop	bx
	pop	es
	pop	ax
	clc
	ret				; done
;
; flag error : XMS driver not present
;
cXMS_no_driver:
	stc
	pop	ax
	ret

Initialize_XMS	endp


;
; INPUT
;	DX == Kbytes to allocate
; OUTPUT
;	AX == 0 on error
;	DX == handle to XMS Memory
;
Allocate_XMS_Memory proc near
	mov	ah, XMS_ALLOC_EMB	; allocate dx K of extended mem.
	call	DWORD PTR cs:[XMSControl]
	ret
Allocate_XMS_Memory endp

;
; INPUT
;	none
; OUTPUT
;	AX = largest free block	
;	DX = total free extended memory
;
query_free_XMS_Memory proc near
	mov	ah, XMS_QUERY_FREE_EXTMEM  ;returns total kb free in dx
	call	DWORD PTR cs:[XMSControl]
	ret
query_free_XMS_Memory endp

;
; INPUT
;	DX == handle to Memory
; OUTPUT
;	none
;
Free_XMS_Memory proc near
	mov	ah, XMS_FREE_EMB	; allocate dx K of extended mem.
	call DWORD PTR cs:[XMSControl]
	ret
Free_XMS_Memory endp

zseg ends

end
