;*
;*	COW : Character Oriented Windows
;*
;*	galloc.asm : global memory allocation

	TITLE	GALLOC - Global memory allocator

	.xlist
	include kernel.inc
	include galloc.inc
	include pbi.inc
	.list


;*****************************************************************************


sBegin	BSS
    assumes DS,DGROUP

staticW plblFailSearch,0		;* label to jump to if compact fails

sEnd	BSS

sBegin	DATA

externW <psLom>

sEnd	DATA


;*****************************************************************************


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING		;* DS points to MOB usually
    assumes SS,DGROUP

externNP	<gcompact,gmovebusy>		;* gcompact.asm
externNP	<galign>			;* ginterf.asm
externNP	<UnbindAll>			;* ldtunk.asm

	PUBLIC	gsplice, gjoin, gzero, gsearch, gmarkfree, gfindfree, gcheckfree
	PUBLIC	DoSearchAddMemory

SUBRS	PROC	NEAR

; Subroutine to splice in a new block into the arena after an existing block
;
;   Inputs:	ES:DI = address of existing block
;		SI = address of new block to add to arena after existing block
;		DS:DI = address of global arena information structure
;
;   Outputs:	Updated size field for old and new block
;		Updated hi_count field in global arena information structure
;
;   Destroys:	CX
;

gsplice:
	inc	[di].hi_count		; Adding new arena entry
	push	si			; save new
	push	es			; save old
	mov	cx,si			; save old.next
	xchg	ES:[di].ga_next,cx	; and old.next = new
	mov	es,cx
	mov	ES:[di].ga_prev,si	; [old old.next].prev = new
	mov	es,si
	mov	ES:[di].ga_next,cx	; new.next = old old.next
	sub	si,cx			; new.size = new.next - new - 1
	neg	si
	dec	si
	mov	ES:[di].ga_size,si
	pop	cx			; new.prev = old
	mov	ES:[di].ga_prev,cx
	mov	ES:[di].ga_owner,di	; Zero owner & handle fields
	mov	ES:[di].ga_handle,di
	mov	ES:[di].ga_sig,GA_SIGNATURE
	mov	es,cx			; ES = old
	sub	cx,ES:[di].ga_next	; old.size = old.next - old - 1
	neg	cx
	dec	cx
	mov	ES:[di].ga_size,cx
	pop	si			; Restore new
	ret


; Subroutine to join two blocks together, by removing one
;
;   Inputs:	ES:DI = address of block to remove
;		DS:DI = address of global arena information structure
;
;   Outputs:	ES:DI = address of block that pointed to the one removed, with
;			update size field and next pointer.
;		Updated hi_count field in global arena information structure
;
;   Destroys:	SI
;

gjoin:
	dec	[di].hi_count
	mov	si,ES:[di].ga_prev	; who points to this block
	mov	es,ES:[di].ga_next	; Get address of block after
	mov	ES:[di].ga_prev,si	; one we are removing.
	push	es			; Change it's back link
	mov	es,si
	pop	ES:[di].ga_next 	; and change the forward link
	sub	si,ES:[di].ga_next	; Recompute size of block
	neg	si
	dec	si
	mov	ES:[di].ga_size,si
	ret


; Subroutine to fill a block with zeros
;
;   Inputs:	BX = address of first paragraph to zero
;		CX = address of last paragraph to zero
;
;   Outputs:	BX = 0
;
;   Destroys:	CX
;

gzero:
	push	ax
	push	di
	push	es

	mov	es,bx		    ; ES = destination
	sub	bx,cx		    ; BX = #para
	neg	bx
	inc	bx
zero1:
	mov	cx,1000h
	cmp	bx,cx
	jae	zero2
	mov	cx,bx
	jcxz	zero3
zero2:
	sub	bx,cx
	shl	cx,1
	shl	cx,1
	shl	cx,1
	xor	ax,ax
	xor	di,di
	cld
	rep	stosw		; Zero it out

	mov	ax,es		; Move to next 64k block
	add	ah,10h
	mov	es,ax

	jmp	zero1
zero3:
	pop	es
	pop	di
	pop	ax
	ret


;*	* special trap routines for search failure
;*	* We will first try to compact. Then we will try to allocate some
;*	* Additonal memory.  If This fails we will then try to trash som
;*	* bound segments(for code).  Finally we fail.
;*	*
;* if we failed we will then try to allocate memory to add onto our
;*	* We will get the largest free memory.	If > predefined minimum
;*	* to add, then we will try to add this new segment into our
;*	* memory list. Note: we will continue trying to allocate memory
;*	* Until we do not get a big enough chunk.

DoSearchFail:		;* if search fails second time, give up
	pop	ax
	pop	cx
	pop	bx
	xor	ax,ax			;* failure
	ret

DoSearchUnBind:
;*	* if after compacting we still don't have enough
;*	*   if compacting for a data request => give up (DoSearchFail)
;*	*   if compacting for code => unbind any segments and try again
	pop	ax
	push	ax			;* ax = flags
	test	al,GA_ALLOCHIGH 	; Allocating from high memory?
	jz	DoSearchFail		;* give up for data
;*	* still trying code (un-bind any bound segments)
	cCall	UnbindAll
	mov	plblFailSearch,kernelOffset DoSearchFail
	jmp	do_compact




DoSearchAddMemory:
IFDEF	WINDOWS_OLD_APP
	jmp	DoSearchUnbind		;* We can't handle this case 
ELSE	; !WINDOWS_OLD_APP

	mov	bx,0ffffh		; Try to allocate 1 meg?
	mov	ah,48h
	int	21h
	cmp	bx,100h 		; Add minimum of 4k
	jc	DoSearchUnbind		; Not enough try 3rd pass

	mov	ah,48h			; Now lets allocate it
	int	21h			;
	jc	DoSearchUnbind		; Allocate now failed?

;;;;; for now we will only handle ones added to end of list.
	; see if we insert new chunk at begining, ending, or in middle
	cmp	ax,[di].hi_first	; see if before current first block
	jc	DoSearchAddMemory	 ; New lowest block
	cmp	ax,[di].hi_last 	; see if after highest
	jc	DoSearchAddMemory	; Tsr went away?
	; New highest point - most probable case.  We ran something that
	; was a TSR.  Now lets merge this memory in.  We need to create a
	; new End sentinal.  As the last ax=start, bx=lenth in paragraphs

	push	ax			;* Save original paragraph number
	test	ax,GA_ALIGN		;* see if aligned right
	jz	ok_align		;* yes
	mov	cx,ax			;* save orignal size
	add	ax,GA_ALIGN
	and	ax,GA_MASK		;* new starting position
	push	ax
	sub	ax,cx			;* see the difference
	sub	bx,ax			;* now update count of bytes
	pop	ax			;* restore new starting position
ok_align:
	mov	cx,ss:psLom		;* Need to make sure in range
	mov	es,cx			;* Use segment register.
	mov	cx,ax			; Save start postion.
	add	ax,bx			;* ax = new end
;*	* check to make sure we are within the useable limits
	cmp	ax,es:[psUseMax]
	jb	still_in_range		;* ok request
	mov	ax,es:[psUseMax]
Still_in_range:
	sub	ax,GA_ALIGN		;* Lets get right position
	and	al,LOW(GA_MASK) 	;* make even
	mov	bx,ax			;* compute new free area added
	sub	bx,cx			;*
	dec	bx			;*

;*	now	lets create a header for new free memory segment
	mov	es,cx			;* setup segment.
	mov	es:[di].ga_sig,GA_SIGNATURE ;* data segment
	mov	es:[di].ga_owner,0	;* Free memory
	mov	es:[di].ga_size,bx	;* How much free area added
	mov	es:[di].ga_flags,0
	mov	es:[di].ga_handle,di	;* no handle
	mov	es:[di].ga_next,ax	;* link to new sentinal
	mov	bx,[di].hi_last 	;* Get old high mark.
	mov	es:[di].ga_prev,bx	;* Link to old sentinal

;*	* create new sentinal
	mov	es,ax			;* setup new sentinal segment
	mov	es:[di].ga_sig,GA_ENDSIG
	mov	es:[di].ga_owner,-1	;* sentinal
	mov	es:[di].ga_size,GA_ALIGN
	mov	es:[di].ga_flags,0
	mov	es:[di].ga_handle,di	;* no handle
	mov	es:[di].ga_next,es	;* link to self
	mov	es:[di].ga_prev,cx

;*	* update Old sentinal to be	;* fixed block that cant move
	xchg	ax,[di].hi_last 	;* Setup pointer to new last object
	mov	es,ax			;* now we need to modify header
	mov	es:[di].ga_sig,GA_HOLE	;* setup as a hole.
	mov	es:[di].ga_next,cx	;* Setup next pointer to new memory object
	sub	cx,ax			;* Setup size of hole.
	dec	cx			;*
	mov	es:[di].ga_size,cx	;* size of hole
	pop	es:[di].ga_newpara	   ;* put allocation seg paragraph number


;*	* now add 2 to the number of items in the list
	add	[di].hi_count,2 	;* add new free list, and endsig

	jmp	short do_compact	; now compact using new memory
ENDIF	; WINDOWS_OLD_APP


;**********************************************************
DoSearchCompact:	;* if search fails first time, compact and try again
	mov	plblFailSearch,kernelOffset DoSearchAddMemory
do_compact:
	push	bx
	call	gcompact		; End of arena.  Try compacting.
	pop	bx
;*	* now retry
	pop	ax
	pop	cx
	pop	bx
	jmp	short gsearch_again


; Subroutine to search for a free global object.  Called from within
; the global memory manager's critical section.
;
;   Inputs:	AX = allocations flags
;		BX = #paras
;		CX = owner field value
;		DS:DI = address of global arena information structure
;
;   Outputs:	AX = data address of block allocated or NULL
;		DX = allocation flags or size of largest free block if
;		     AX = 0
;		Z flag set if AX = zero
;
;   Destroys:	BX,CX,SI,ES
;

gsearch:
	mov	plblFailSearch,kernelOffset DoSearchCompact
gsearch_again:
	push	bx			;* save cpara
	push	cx			; Save owner
	push	ax			; Save flags
	add	bx,1			; Room for header (set flags for galign)
	call	galign			; Get requested size in DX
	mov	cx,[di].hi_count	; ES:DI is our arena pointer
	mov	es,[di].hi_first	; Assume fixed, start with first entry
	mov	bx,ga_next		; and search forwards along next links
	test	al,GA_ALLOCHIGH 	; Allocating from high memory?
	jz	gs1			; No, continue
	mov	es,[di].hi_last 	; Yes, start with last entry
	mov	bl,ga_prev		; and search backwards along prev links
gs1:	mov	es,es:[bx]		; Skip first or last block
aloop:
	cmp	ES:[di].ga_owner,di	; Is block free?
	je	afree			; Yes, see if it is big enough
	mov	si,ES:[di].ga_handle	; See if moveable
	cmp	bl,ga_next		; No, moving forward?
	jne	aloop1			; No, see if we should stop looking
	or	si,si
	jz	anext
	cmp	[si].he_count,bh
	jne	anext			; No, keep looking
	pop	ax
	push	ax
	test	al,GA_MOVEABLE		; Yes, is this a fixed request?
	jnz	anext			; No, keep looking
	mov	ax,es:[di].ga_size	; Yes, is this moveable block
	inc	ax			; big enough for the fixed space
	cmp	ax,dx			; needed?
	jb	anext			; No, keep looking
	push	dx
	mov	dx,ax			; Yes, try to find a place for the
	call	gfindfree		; moveable block
	pop	dx
	or	ax,ax			; Did we find a place?
	jz	anext			; No, keep looking
	push	es:[di].ga_prev 	; Where to pick up enumeration from
	mov	si,es			; SI = moveable block
	mov	es,ax			; ES = free block big enough for it
	call	gmovebusy		; Move moveable block out of the way
	pop	es
	jmp	short aloop		; Restart enumeration to find new free block
aloop1:
	cmp	ds:[di].gi_reserve,di	; Is there a reserved swap area?
	je	anext			; No, next block
	or	si,si			; Stop backwards scan if not moveable
	jz	anotfound
	test	es:[di].ga_flags,GA_DISCCODE	; ... or not discardable code
	jz	anotfound
	jmp	short anext		; Otherwise keep scanning backwards
afree:
	call	gcheckfree		; Yes, room for requested size?
	jae	afound			; Yes, exit search loop
	cmp	ds:[di].gi_reserve,di	; No, is the reserved swap area?
	je	anext			; No, next block
	cmp	bl,ga_prev		; Yes, are we allocating for code?
	je	anotfound		; Yes, then can only go in the first
					; free block.
anext:
	mov	es,ES:[bx]		; ES = address of next block
	loop	aloop			; Loop to next arena block if there

anotfound:
	jmp	[plblFailSearch]	;* jump to retry or fail

afound:
	mov	ax,es:[di].ga_size	; Use actual size of free block
	inc	ax
; Here when we have a block big enough.
;   ES:DI = address of block
;   AX = size of block, including header
;   DX = requested size, including header
;   BX = ga_prev if backwards search and ga_next if forwards search
;
	mov	cx,ax			; See how much extra space there is
	sub	cx,dx			; (found size - requested size)
	xor	si,si			; Assume nothing extra to free
	jcxz	aexit			; No, continue
	cmp	bl,ga_prev		; Yes, scanning forwards or backwards?
	je	abwd			; Backwards.
	mov	si,es			; Forwards.   Put extra space at end of
	add	si,dx			; free block
	call	gsplice 		; ES:DI = block we are allocating
	jmp	short aexit		; SI = block to mark as free
abwd:
	mov	si,ES:[di].ga_next	; Scanning backwards.  Put extra space
	sub	si,dx			; at beginning of free block.
	call	gsplice
	mov	es,si			; ES:DI = block we are allocating
	mov	si,ES:[di].ga_prev	; SI = block to mark as free

; Here with allocated block
;   AX = data address or zero if nothing allocated
;   ES:DI = address of block to mark as busy and zero init if requested
;   SI = address of block to mark as free
;
aexit:
	pop	dx			; Restore flags
	pop	ES:[di].ga_owner	; Mark this block as busy
	pop	ax			;* discard original size request
	mov	al,GA_SEGTYPE
	and	al,dl
	mov	ES:[di].ga_flags,al	; Store segment type bits

	mov	ax,es			; AX = address of client data
	inc	ax

	test	dl,GA_ZEROINIT		; Want it zeroed?
	jz	aexit1			; No, all done
	mov	cx,ES:[di].ga_next	; Yes, zero paragraphs
	dec	cx			; to end of this block
	mov	bx,ax			; from beginning of client data
	call	gzero			; zero them
aexit1:
	mov	es,si			; Free any extra space
	call	gmarkfree
	or	ax,ax
	ret				; Return AX points to client portion
					; of block allocated.

; Subroutine to mark a block as free, coalescing it with any free blocks
; before or after it.
;
;   Inputs:	ES:DI = block to mark as free.
;		DS:DI = address of global arena information structure
;
;   Outputs:	SI = zero if freed a fixed block.  For moveable blocks,
;		SI = handle table entry
;		ES:DI = block freed (may have been coelesced)
;		Updated hi_count field in global arena information structure
;
;   Destroys:	SI
;
gmarkfree:
	mov	si,es
	or	si,si
	jz	free4

	; Mark this block as free by clearing the owner field.
	mov	ES:[di].ga_owner,di	; Mark as free

	; Remember the handle value in DX, before setting to zero.
	push	dx
	xor	dx,dx
	xchg	ES:[di].ga_handle,dx

	; Try to coelesce with next block, if it is free
	push	ES:[di].ga_prev 	; save previous block
	mov	es,ES:[di].ga_next	; ES = next block
	cmp	ES:[di].ga_owner,di	; Is it free?
	jne	free2			; No, continue
	call	gjoin			; Yes, coelesce with block we are freeing
free2:
	pop	es			; ES = previous block
	cmp	ES:[di].ga_owner,di	; Is it free?
	jne	free3			; No, continue
	mov	es,ES:[di].ga_next	; Yes, coelesce with block we are freeing;
	call	gjoin
free3:
	mov	si,dx			; Return 0 or handle in SI
	pop	dx			; restore DX
	cmp	ES:[di].ga_owner,di	; Point to free block?
	je	free4			; Yes, done
	mov	es,ES:[di].ga_next	; No, leave ES pointing at free block
free4:
	or	si,si
	ret

; Subroutine to search for a free block that is big enough but does not
; encroach on the area reserved for code swapping.
;
;   Inputs:	ES:DI = address of existing block to start looking at
;		CX = #arena entries left to look at
;		BX = direction of search, ga_next or ga_prev
;		DX = #paragraphs needed
;		DS:DI = address of global arena information structure
;
;   Outputs:	AX = zero or address of free block that is big enough
;

gfindfree:
	push	es
	push	cx
gffloop:
	cmp	es:[di].ga_owner,di	; Free block?
	jne	gffnext 		; No, continue
	call	gcheckfree		; Yes, is it big enough?
	mov	ax,es
	jae	gffexit 		; Yes, return
	cmp	bl,ga_prev		; No, scanning backwards?
	je	gfffail 		; Yes, fail
gffnext:
	mov	es,es:[bx]		; next or previous block
	loop	gffloop
gfffail:
	xor	ax,ax			; No, return zero in AX
gffexit:
	pop	cx
	pop	es
	ret

; Subroutine to check the size of the passed free block against the passed
; desired size, making sure that the limitations of the code reserve area
; are not violated.
;
;   Inputs:	ES:DI = address of free block
;		DX = #paragraphs needed
;		DS:DI = address of global arena information structure
;
;   Outputs:	AX = apparent size of free block
;
;   Destroys:	SI
;

gcheckfree:
	mov	ax,es:[di].ga_size	; Compute size of free block
	inc	ax
	cmp	[di].gi_reserve,di	; Is there a code reserve area?
	je	gcftest 		; No, size okay
	test	byte ptr [di].gi_cmpflags,GA_DISCCODE
	jnz	gcftest 		; Discardable code not restricted
	push	es
	mov	es,es:[di].ga_next	; Go to block after free block
	cmp	es:[di].ga_sig,GA_ENDSIG    ; Is it the end of the arena?
	je	gcfrsrv 		; Yes, go enforce reserve area
	test	es:[di].ga_flags,GA_DISCCODE	; Is it a discardable code seg?
	jz	gcftest1		; No, test size
gcfrsrv:
	pop	es			; Yes, compute real available size
	mov	si,[di].hi_last 	; See if beginning of reserve area
	sub	si,[di].gi_reserve	; is above the end of the free block
	cmp	si,es:[di].ga_next
	jae	gcftest 		; Yes, return actual size of free block
	sub	si,es:[di].ga_next	; No, compute amount of overlap
	neg	si
	cmp	ax,si			; Is it more than what is free?
	jbe	gcfrsrv1		; Yes, then apparent size is zero
	sub	ax,si			; No, reduce apparent size of free block
	jmp	short gcftest		; Test size.
gcfrsrv1:
	xor	ax,ax			; Nothing left, set apparent size to 0
	jmp	short gcftest		; Test size.
gcftest1:
	pop	es
gcftest:
	cmp	ax,dx			; Return results of the comparison
	ret

SUBRS	ENDP

sEnd	KERNEL

	END
