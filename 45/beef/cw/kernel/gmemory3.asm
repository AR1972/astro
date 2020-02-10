;*
;*	COW : Character Oriented Windows
;*
;*	gmemory3.asm : Global memory for DOS 3
;*	* NOTE : the old size of the block is stored in the last 2 bytes
;*	*  of the MSDOS arena !!

	TITLE	GMEMORY - Windows interface to DOS memory management DOS 3

	.xlist
	include	kernel.inc
	.list

sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,DATA


;********** CparaOfCb **********
;*	entry : DX:BX = long count of bytes
;*	* round up to number of paragraphs
;*	exit : BX = # of paragraphs

cProc	CparaOfCb,<NEAR, ATOMIC>
cBegin	CparaOfCb
	add	bx,0fh			;* round up
	adc	dx,0
	mov	cx,4
shift_loop:
	shr	dx,1
	rcr	bx,1
	loop	shift_loop
cEnd	CparaOfCb



cProc	GlobalAlloc,<PUBLIC,FAR,ATOMIC>
    parmW   flags
    parmD   lcb
cBegin	GlobalAlloc
	mov	bx,word ptr (lcb)
	mov	dx,word ptr (lcb+2)	;* DX:AX = lcb
	cCall	CparaOfCb
	mov	ah,48h			;* alloc block
	push	bx
	int	21h
	pop	bx
	jc	alloc_error
	dec	ax
	mov	es,ax			;* point to arena
	inc	ax
	mov	es:[14],bx		;* save size
alloc_end:	;* ax = hgmem

cEnd	GlobalAlloc

alloc_error:
	xor	ax,ax			;* return NULL handle
	jmp	alloc_end


cProc	GlobalReAlloc,<PUBLIC,FAR,ATOMIC>
    parmW   hgmem
    parmD   lcb
    parmW   flags
cBegin	GlobalReAlloc

	mov	bx,word ptr (lcb)
	mov	dx,word ptr (lcb+2)	;* DX:AX = lcb
	cCall	CparaOfCb
	mov	es,hgmem		;* block to grow/shrink
	mov	ah,4ah			;* change size of block
	push	bx
	int	21h
	pop	bx
	mov	ax,hgmem
	jnc	realloc_ok

;*	* failed : attempt to allocate new block
	mov	ah,48h
	push	bx
	int	21h
	pop	bx
	jc	realloc_error		;* sorry - no room

;*	* copy from old to new
	push	si
	push	di
	push	ds
	mov	es,ax
	xor	di,di			;* es:di => destination

	mov	dx,hgmem		;* old source
	dec	dx
	mov	ds,dx			;* point to arena
	mov	cx,ds:[14]		;* old size in paragraphs
	inc	dx
	mov	ds,dx			;* point to old data

	xor	si,si			;* ds:si => old data

	shl	cx,1
	shl	cx,1
	shl	cx,1			 ;* cx = cw

	rep movsw			;* move to new location

	pop	ds
	pop	di
	pop	si

	push	ax			;* new hgmem
	mov	es,hgmem
	mov	ah,49h
	int	21h			;* free old block
	pop	ax			;* new hgmem

realloc_ok:	;* ax = hgmem
	dec	ax
	mov	es,ax			;* point to arena
	mov	es:[14],bx		;* save new size
	inc	ax
realloc_end:

cEnd	GlobalReAlloc

realloc_error:
	xor	ax,ax			;* return NULL handle
	jmp	realloc_end



cProc	GlobalFree,<PUBLIC,FAR,ATOMIC>
    parmW   hgmem
cBegin	GlobalFree
	mov	es,hgmem
	mov	ah,49h
	int	21h		;* free block
cEnd	GlobalFree



cProc	GlobalLock,<PUBLIC,FAR,ATOMIC>
   parmW   hgmem
cBegin	GlobalLock
	mov	dx,hgmem
	xor	ax,ax
cEnd	GlobalLock



cProc	GlobalUnlock,<PUBLIC,FAR,ATOMIC>
   parmW   hgmem
cBegin	GlobalUnlock
;*	* Do nothing
cEnd	GlobalUnlock


cProc	GlobalHandle,<PUBLIC,FAR,ATOMIC>
   parmW   hgmem
cBegin	GlobalHandle

	mov	ax,hgmem			;* return handle/address
	mov	dx,ax				;* in both parts

cEnd	GlobalHandle



sEnd	KERNEL

	END
