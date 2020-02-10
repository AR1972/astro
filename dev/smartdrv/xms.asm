;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
;OVERVIEW
;	This moduel contains asm callable routines for manipulating
;XMS memory. This module depends on being initialzed by the
;initialize_xms call from xmsini.asm.
;	The XMS routines are divide into xmsini.asm and xms.asm
;so initialization code can be separated from resident code.
;	The expected use of these function is		
;		initialze XMS via initialize_xms
;		allocate some XMS memory via allocate_xms_memory
;		read/write from XMS memory via block_read and block_write
;		resize a memory block via reallocate_xms_memory
;		free XMS memory via free_xms_memory (warning: in xmsini.asm)

include xms.inc

public	block_write
public	block_read
public	reallocate_xms_memory
public	XMSControl

;	align the XMS packet to a word boundary

zseg    segment word public 'CODE'

	assume  cs:zseg
	assume  ds:zseg

;
;	local data
;

XMSMovePacket	extmemmovestruct <0>
XMSControl 	dd	0

;
; INPUT
;	BX == new Kbytes to re-allocate
;	DX == handle to XMS Memory
; OUTPUT
;	AX == 0 on error
;	error code in BX
;
ReAllocate_XMS_Memory proc near
	mov	ah, XMS_REALLOC_EMB	; allocate dx K of extended mem.
	call	DWORD PTR cs:[XMSControl]
	ret
ReAllocate_XMS_Memory endp


;
; INPUT
;
;	BX = XMS Handle
;	DX:AX = offset into XMS block referred to by AX
;	ES:DI = Near Memory Source
;	CX = Number of words to transfer
; OUTPUT
Block_Read proc near
	push	dx
	push	ax
	push	bx
	push	cx
	push	si
	
	mov	si, offset cs:XMSMovePacket	; cs:si points to buffer


; initialize length field

	shl	cx, 1			; cx = # of bytes to transfer
	mov	word ptr cs:[si.mov_length], cx
;redundant	mov	word ptr cs:[si.mov_length + 2], 0

; initialize source handle 

	mov	cs:[si.src_handle], bx

; initialize source offset 

	mov	word ptr cs:[si.src_offset], ax
	mov	word ptr cs:[si.src_offset + 2], dx


; initialize destination handle	and offset 

	mov	cs:[si.dst_handle], 0		; offset is Segment:Offset
	mov	word ptr cs:[si.dst_offset], di
	mov	word ptr cs:[si.dst_offset + 2], es

do_move:
	mov	ah, XMS_MOVE_EMB	; move ext. mem. block function
	call	DWORD PTR cs:[XMScontrol]

	shr	ax,1			; rotate 'success' bit into carry
	cmc				; make it a failure flag

	pop	si
	pop	cx
	pop	bx
	pop	ax
	pop	dx
	ret
Block_Read endp


;
; INPUT
;
;	BX = XMS Handle
;	DX:AX = offset into XMS block referred to by AX
;	ES:DI = Near Memory Source
;	CX = Number of words to transfer
;	
; OUTPUT
Block_Write proc near
	push	dx
	push	ax
	push	bx
	push	cx
	push	si
	
	mov	si, offset cs:XMSMovePacket	; cs:si points to buffer


; initialize length field
	shl	cx, 1			; cx = # of bytes to transfer
	mov	word ptr cs:[si.mov_length], cx
;redundant	mov	word ptr cs:[si.mov_length + 2], 0

; initialize source handle and offset

	mov	cs:[si.src_handle], 0		; offset is Segment:Offset
	mov	word ptr cs:[si.src_offset], di
	mov	word ptr cs:[si.src_offset + 2], es

; initialize destination handle
	mov	cs:[si.dst_handle], bx

; initialize destination offset

	mov	word ptr cs:[si.dst_offset], ax
	mov	word ptr cs:[si.dst_offset + 2], dx

; call XMS to move memory
	jmp	do_move	;in block read above

Block_Write endp


zseg ends

end
