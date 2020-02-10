;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc

PUBLIC	get_cache_hits
PUBLIC	get_cache_misses
PUBLIC	get_cache_dirty_elements
PUBLIC	get_cache_info
PUBLIC	commit_cache
PUBLIC	reset_cache
PUBLIC	cache_a_drive

_TEXT	segment	public	para 'CODE'
	assume cs:_TEXT

;
;INPUT
;	none
;OUTPUT
;	DX:AX = cache hits
;
get_cache_hits proc near
	push	bp
	push	si
	push	di
	mov	ax,MULT_BAMBI
	mov	bx,BAMBI_GET_STATS
	int	2fh
	cmp	ax,BAMBI_SIGNATURE
	jne	no_bambi
	mov	ax,bx
retout:
	pop	di
	pop	si
	pop	bp
	ret
no_bambi:
	mov	dx,-1
	mov	ax,dx
	jmp	short retout
get_cache_hits endp

;
;INPUT
;	none
;OUTPUT
;	DX:AX = cache hits
;
get_cache_misses proc near
	push	bp
	push	si
	push	di
	mov	ax,MULT_BAMBI
	mov	bx,BAMBI_GET_STATS
	int	2fh
	cmp	ax,BAMBI_SIGNATURE
	jne	missno_bambi
	mov	dx,di
	mov	ax,si
missretout:
	pop	di
	pop	si
	pop	bp
	ret
missno_bambi:
	mov	dx,-1
	mov	ax,dx
	jmp	short missretout
get_cache_misses endp

;
;INPUT
;	none
;OUTPUT
;	AX = # dirty elements
;
get_cache_dirty_elements proc near
	push	si
	push	di
	mov	ax,MULT_BAMBI
	mov	bx,BAMBI_GET_STATS
	int	2fh
	cmp	ax,BAMBI_SIGNATURE
	jne	dirtyno_bambi
	mov	ax,cx
dirtyretout:
	pop	di
	pop	si
	ret
dirtyno_bambi:
	mov	dx,-1
	mov	ax,dx
	jmp	short dirtyretout
get_cache_dirty_elements endp


;
;INPUT
;	none
;OUTPUT
;	none
commit_cache proc near
	mov	ah,0dh	    ; DOS disk reset function
	int	21h
	xor	ax,ax
	ret
commit_cache endp


;
;INPUT
;	none
;OUTPUT
;	none
reset_cache proc near
	push	bx
	mov	ax,MULT_BAMBI
	mov	bx,BAMBI_REINITIALIZE
	int	2fh
	pop	bx
	xor	ax,ax
	ret
reset_cache endp


;
;INPUT
;	pointer to cache block count under DOS
;	pointer to cache block count under Windows
;
;OUTPUT
;	ax = cache block size in bytes
;
get_cache_info proc near
	push	bp
	mov	bp,sp
	push	bx
	push	cx
	push	si
	mov	si,[bp+6]
	mov	bp,[bp+4]

	mov	ax,MULT_BAMBI
	mov	bx,BAMBI_GET_INFO
	int	2fh
	mov	word ptr [si],ax	; block # under DOS
	mov	word ptr [bp],bx	; block # under Win
	mov	ax,cx

	pop	si
	pop	cx
	pop	bx
	pop	bp
	ret	4
get_cache_info	endp


;
;INPUT
;	unit # (0 based) of drive
;	function # 0 = get,
;		   1 = enable read caching
;		   2 = disable read caching
;		   3 = enable write caching
;		   4 = disable write caching
;OUTPUT
;	al = drive cache info
;
cache_a_drive proc near
	push	bp
	mov	bp,sp
	mov	dx,[bp+6]	;function#
	mov	bp,[bp+4]	;drive unit
	push	si
	push	di

	mov	ax,MULT_BAMBI
	mov	bx,BAMBI_CACHE_DRIVE
	int	2fh
	cmp	ax,BAMBI_SIGNATURE
	jne	cacheno_bambi
	mov	al,dl
	xor	ah,ah
cacheout:
	pop	di
	pop	si
	pop	bp
	ret	4
cacheno_bambi:
	mov	ax,-1
	jmp	short cacheout
cache_a_drive endp

_TEXT ends

end
