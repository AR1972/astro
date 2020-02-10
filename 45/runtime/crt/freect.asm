	page	,132
	title	freect - C runtime heap allocation _freect and _memavl
;***
;freect.asm - C runtime heap allocation _freect and _memavl
;
;	Copyright (c) 1987-1988, Microsoft Corporation, All Rights Reserved
;
;Purpose:
;	_freect and _memavl routines are useful for determining how
;	much memory within the default data segment is still free for
;	dynamic allocation (by malloc in small/medium memory models,
;	and by _nmalloc in all memory models).
;
;*******************************************************************************

include	version.inc
.xlist
include	cmacros.inc
include	brkctl.inc
.list

sBegin	data
assumes	ds,data

externW	_asegds
extrn b$nmalloc_end:word	;[1] end of BASIC near malloc buffer
externW	_abrktb

sEnd	data

externP	_nmalloc


sBegin	code
assumes	ds,data
assumes	cs,code

;
; freect() assumes that DGroup_Left() does not trash CX
;	In real mode (DOS 3), uses actual size of DGROUP
;	If executing in Protected Mode, assume DGROUP can be grown to 65535
;


cProc	DGroup_Left,<NEAR>,<>

cBegin
 	mov	ax,[b$nmalloc_end] ;[1] size of Near malloc buffer...
 	dec	ax		;[1] ...-1
not_prot_mode:
	sub	ax,3		; overhead - can only use 65534 bytes
				; allocating an entry takes 2 bytes
				; plus 1 to make AX=DGROUP-overhead
	mov	dx,[_abrktb].sz	; end of heap memory
	mov	bx,[_asegds]
	cmp	word ptr [bx+2],0fffeH ; check for heap not really set-up
	jnz	have_heap
	sub	ax,3		; more overhead: dummy heap entries
have_heap:
	cmp	ax,dx
	ja	have_room
	mov	dx,ax		; no room left
have_room:
	sub	ax,dx		; number of bytes than can be allocated
cEnd

page
;***
;unsigned _memmax() - find size of largest free block in near heap
;
;Purpose:
;	returns the size in bytes of the maximal free block left in
;	the default data segment (in the near heap)
;
;Entry:
;	None.
;
;Exit:
;	returns AX = WORD size in bytes of largest free block in DGROUP
;
;Uses:
;	BX, CX, DX.
;
;Exceptions:
;
;*******************************************************************************

cProc	_memmax,<PUBLIC>,<si>

cBegin
	call	_initheap	; make sure heap is init'd
	or	ax,ax		; did _initheap succeed ??
	jz	x_exit		; nope - return 0

	call	DGroup_Left	; Number of free bytes left in DGROUP
	mov	cx,ax		; after the end of the current heap
	mov	bx,ax		; Save in BX for later

	mov	si,[_asegds].bottom ; si = start of near heap
	jmp	short x_first	; into the middle of things

x_notfree:
	cmp	ax,-2		; check for end of heap
	je	x_endheap
	add	si,ax		; advance to next heap entry

x_first:
	lodsw			; ax = length
	test	al,1
	jz	x_notfree	;   in use - go to next one
;
;	Add up a series of 1 or more contiguous free blocks as if
;	they were coalesced.  Coalescing is only done when needed.
;
	mov	dx,-2		; bias by -2 for first header

x_isfree:
	dec	ax		; ax = length of free block
	inc	dx
	inc	dx		; add in size of freeable header
	add	dx,ax		; add in free block
	add	si,ax		; si = next entry
	lodsw
	test	al,1		; is next free?
	jnz	x_isfree	;   yes - total it up

	cmp	ax,-2
	jne	x_notend
	add	dx,bx		; add in space left in DS after heap
	add	dx,2		; don't need a new header
x_notend:
	cmp	cx,dx		; add in "would-be coalesced" block
	ja	x_notfree	; made up of adjacent free entries
	mov	cx,dx
	jmp	x_notfree	; CX keeps track of largest

x_endheap:
	mov	ax,cx		; return largest number of free
				; bytes in one contiguous piece
x_exit:

cEnd

page
;***
;unsigned _memavl() - gives byte count of remaining space in near heap
;
;Purpose:
;	The _memval function returns the approximate size, in bytes, of
;	the memory available for dynamic memory allocation within the
;	default data segment.  This function can be used with calloc,
;	malloc, or realloc in small/medium memory models, and with
;	_nmalloc in all memory models.
;
;	This is the DOS version of _memavl, and it calls DGroup_Left.
;
;Entry:
;	None.
;
;Exit:
;	returns AX = WORD size in bytes of available 'near heap' memory
;
;Uses:
;	CX, DX.
;
;Exceptions:
;
;*******************************************************************************

cProc	_memavl,<PUBLIC>,<si>

cBegin
	call	_initheap	; make sure heap is init'd
	or	ax,ax		; did _initheap succeed ??
	jz	_exit		; nope - return 0

	call	DGroup_Left	; Number of free bytes left in DGROUP
	mov	cx,ax		; after the end of the current heap

	mov	si,[_asegds].bottom ; si = start of near heap
	jmp	short _first	; into the middle of things

_notfree:
	cmp	ax,-2		; check for end of heap
	je	_endheap
	add	si,ax		; advance to next heap entry

_first:
	lodsw			; ax = length
	test	al,1
	jz	_notfree	;   in use - go to next one
;
;	Add up a series of 1 or more contiguous free blocks as if
;	they were coalesced.  Coalescing is only done when needed.
;
	mov	dx,-2		; bias by -2 for first header

_isfree:
	dec	ax		; ax = length of free block
	inc	dx
	inc	dx		; add in size of freeable header
	add	dx,ax		; add in free block
	add	si,ax		; si = next entry
	lodsw
	test	al,1		; is next free?
	jnz	_isfree		;   yes - total it up

	add	cx,dx		; add in "would-be coalesced" block
	jmp	_notfree	; made up of adjacent free entries

_endheap:
	mov	ax,cx		; return number of free bytes

_exit:

cEnd

page
;***
;unsigned _freect(size) - count of blocks of given size that can be _nmalloc()'d
;
;Purpose:
;	To give an indication of how much free space is available
;	within the default data segment for dynamic memory allocation
;	(malloc/_nmalloc), in terms of the number of blocks of the
;	given size that can be _nmalloc()'d before running out of space.
;
;Entry:
;	blksiz = WORD size of block to be used as a measuring rod
;		 of available memory
;
;Exit:
;	returns AX = WORD number of blocks of size 'blksiz' that could
;	             be _nmalloc()'d
;
;Uses:
;	BX, CX, DX.
;
;Exceptions:
;
;*******************************************************************************

cProc	_freect,<PUBLIC>,<si>

	parmW	blksiz		; block size

cBegin
	call	_initheap	; make sure heap is init'd
	or	ax,ax		; did _initheap succeed ??
	jz	freectxit	; nope - return 0

	mov	si,[_asegds].bottom ; si = start of near heap
	mov	bx,blksiz
	add	bx,3
	and	bl,not 1	; round up even (blksiz+2)
	xor	cx,cx		; # free entries
	jmp	short first	; into the middle of things

notfree:
	cmp	ax,-2		; check for end of heap
	je	endheap
	add	si,ax

first:
	lodsw			; ax = length
	test	al,1
	jz	notfree		;   in use - go to next one

	mov	dx,-2		; bias by -2

isfree:
	dec	ax		; ax = length of free block
	inc	dx
	inc	dx		; add in size of freeable header
	add	dx,ax		; add in free block
	add	si,ax		; si = next entry
	lodsw
	test	al,1		; is next free?
	jnz	isfree		;   yes - total it up

	push	ax
	xchg	ax,dx
	xor	dx,dx
	div	bx
	add	cx,ax		; cx += freeblock / (blksiz+2)
	pop	ax
	jmp	notfree

endheap:
	push	bx		; DGroup_Left may trash BX
	call	DGroup_Left	; must preserve CX
	pop	bx
	xor	dx,dx
	div	bx
	add	ax,cx		; ax = cx + DGROUPfree / (blksiz+2)

freectxit:
cEnd

page
;***
;_initheap - Initializes the heap, if necessary.
;
;Purpose:
;	_initheap tests to see if the heap is initialized.  If not,
;	it issues a malloc which causes the heap to be initialized.
;	_initheap then frees the memory it aquired (since it is not
;	really needed).
;
;	We must check to see if _nmalloc returns NULL.
;
;Entry:
;	None.
;
;Exit:
;	AX =  0 = failure, heap no room to initialize heap
;	     !0 = success, heap initialized
;
;Uses:
;	AX, BX
;
;Exceptions:
;	None.
;
;*******************************************************************************

cProc	_initheap,<NEAR>,<>

cBegin
	mov	ax,1		;ax = 1 = success flag
	mov	bx,dataoffset[_asegds] ;heap data base
	cmp	word ptr[bx],0	;is it init'd ??
	jne	_initdone	;leave if heap init'd

	;mov	ax,1		;allocate a 1-byte area
	push	ax		;onto the stack
	call	_nmalloc	;initialize the heap
	pop	bx		;[1] clean stack after call for people who's
				;[1] CMACROS doesn't put a MOV SP,BP at the end
	or	ax,ax		;did _nmalloc() succeed ??
	jz	_initdone	;if failure, exit with AX=0
	mov	bx,ax		;get mem addr in index reg
	or	byte ptr[bx-2],1 ;free the memory

_initdone:			;return with ax

cEnd

sEnd	code

	end
