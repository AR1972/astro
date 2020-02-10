;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */


zseg    segment public 'CODE'

	assume  cs:zseg
	assume  ds:nothing
public	initqueue

;
;	data from rdata.asm
;

	ifdef	USE_VALID
extrn	validindexoffset		:word
	endif
extrn	dirtyindexoffset		:word
extrn	word_masks			:byte
extrn	elementidentifierlowordoffset	:word
extrn	elementidentifierhiwordoffset	:word
extrn	number_of_dirty_elements	:word
extrn	queueheadptr			:dword
extrn	queuelength			:word


extrn	initialize_lru_queue		:near

;
; INPUT
;       CX    number of elements in the cache   
;
InitQueue proc near
	push	ds
	push    cs
	pop     ds


	mov     WORD PTR cs:QueueHeadPtr[0],0
	mov	WORD PTR cs:Number_Of_Dirty_Elements,0
	mov     WORD PTR cs:QueueLength,cx

	or	cx,cx
	jz	quick_out
	
	xor	bp,bp			; initialize loop pointer

init_cache_loop:
	mov     si,cs:ElementIdentifierLowordOffset
	mov     WORD PTR cs:[bp+si],-1
	mov     si,cs:ElementIdentifierHiwordOffset
	mov     WORD PTR cs:[bp+si],-1
	mov     si,cs:DirtyIndexOffset

;	Note:  There are two different options for the remainder of
;		this loop, depending on whether we're using byte or
;		words for our mask arrays.  The final loop termination
;		is duplicated in both parts for speed.

	cmp	word_masks,0		; are mask array(s) words or bytes?
	jz	init_cache_bytes

	mov     WORD PTR cs:[bp+si],0	; init dirty_bits
	ifdef	USE_VALID
	mov	si,cs:validindexoffset
	mov	word ptr cs:[bp+si],0	; init valid_bits
	endif
init_cache_common:
	add	bp,2			; next element
	loop    init_cache_loop

	call	initialize_lru_queue
quick_out:
	pop	ds
	ret

init_cache_bytes:
	shr	bp,1			; convert to byte mask
	mov	byte ptr cs:[bp+si],0	; set dirty_bits
	ifdef	USE_VALID
	mov	si,cs:validindexoffset
	mov	byte ptr cs:[bp+si],0	; init valid_bits
	endif
	shl	bp,1			; back to word mask domain
	jmp	init_cache_common

InitQueue endp

zseg ends

end
