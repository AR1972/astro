;*
;*	COW : Character Oriented Windows
;*
;*	gcompact.asm : global compaction
;*	* NOTE :assumes stack never moves !

	TITLE	GCOMPACT - Global memory compactor

	.xlist
	include kernel.inc
	include galloc.inc
	.list


;*****************************************************************************

sBegin	DATA

externW     <psLom>

;*************************** compact kludge ***********************
globalB     fCompactLowerHeap,0


sEnd	DATA

;*****************************************************************************


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING			;* DS == pGlobalHeap
    assumes SS,DATA

; These are all the external subroutines needed by this source file.
;
externNP    <genter>			; GINTERF.ASM
externNP    <gjoin,gmarkfree,gcheckfree>; GALLOC.ASM
externNP    <gnotify>			; GINTERF.ASM

; These are all of the internal subroutines defined in this source file.
;
	PUBLIC	gcompact		;* compact global heap
	PUBLIC	gmovebusy

IFDEF DEBPUB
	PUBLIC	gmove, gmoveable, gslidecommon
	PUBLIC	gcmpheap, gslide, gbestfit, gdiscard
ENDIF

SUBRS	PROC	NEAR


; Subroutine to move a moveable block into the top part of a free block
; The low order bit of the source and destination may be either set or
; reset.  If set, then this routine does NOT move the arena header paragraph.
; If the bit is reset, then the arena header is moved.	Only the low order
; bit of SI is examined, and the low order bit of ES is assumed to the same.
;
;   Inputs:	ES:0  = address of destination block
;		SI:0  = address of source block
;

gmove:
	push	es
	push	si
	push	di
	push	ax
	push	bx
	push	cx
	push	dx

	mov	dx,si			; DX = 0 if moving arena header too
	and	dx,1			; and 1 if not.

	push	es			; Save destination
	mov	cx,es			; CX = client data address of dest.
	or	cl,1

	mov	ax,si			; ES:DI = arena header of source
	xor	ax,dx			;
	mov	es,ax

	xor	dl,1			; DX = #paragraphs to move
	add	dx,ES:[di].ga_size
	push	dx			; Save #paragraphs

	mov	ax,GN_MOVE
	mov	bx,ES:[di].ga_handle	; BX = handle of source
	or	bx,bx
	jnz	gm1
	mov	bx,es
	inc	bx
gm1:
	push	es
	mov	dx,es:[di].ga_owner	;* set up owner for gnotify
	call	gnotify 		; Call global notify procedure
	pop	cx
	inc	cx
	pop	dx			; DX = #paragraphs to move
	pop	di			; DI = destination

	; Save DS value AFTER call to gnotify, as it might be the one
	; we are moving and thus changed by the global heap notify proc.
	push	ds

	mov	ax,dx			; AX:0 = end of source
	add	ax,si
	mov	bx,dx			; BX:0 = end of destination
	add	bx,di

	cmp	si,di			; Moving up towards high memory?
	jb	move0a			; Yes, all set
	mov	ax,si			; No, start at beginning of source
	mov	bx,di			; ... and destination
move0a:

; AX:0 = end of source
; BX:0 = end of destination
; DX   = #paragraphs left to move
;
move1:
	mov	cx,1000h	    ; Can only move 64k at a time
	cmp	dx,cx		    ; More than that left?
	jae	move2		    ; Yes, just move 64k bytes
	mov	cx,dx		    ; No, move what is left then
	jcxz	move3		    ; All done if nothing left to move
move2:
	sub	dx,cx		    ; DX = #paragraphs left after this xfer
	mov	si,cx
	shl	cx,1		    ; CX = #words to move
	shl	cx,1
	shl	cx,1

	cmp	ax,bx		    ; Moving up towards high memory?
	jb	move2a		    ; Yes, handle separately

	cld
	mov	ds,ax		    ; DS:SI = first word in source block
	mov	es,bx		    ; ES:DI = first word in dest. block
	add	ax,si		    ; AX:0 = end of source block this xfer
	add	bx,si		    ; BX:0 = end of dest. block this xfer
	xor	si,si
	jmp	short move2b
move2a:
	std
	sub	ax,si		    ; AX:0 = beginning of source block this xfer
	sub	bx,si		    ; BX:0 = beginning of dest. block this xfer
	mov	si,cx
	dec	si
	shl	si,1
	mov	ds,ax		    ; DS:SI = last word in source block
	mov	es,bx		    ; ES:DI = last word in dest. block
move2b:
	mov	di,si
	rep	movsw		    ; move this section
	jmp	move1		    ; Back for more
move3:
	pop	ds		    ; Restore DS (it might be different)
	pop	dx		    ; Restore registers
	pop	cx
	pop	bx
	pop	ax
	pop	di
	pop	si
	pop	es
	cld		    ; Protect people like MarkCl from themselves
	ret

; Subroutine to compact the global heap
;
;   Inputs:	DX = minimum #contiguous bytes needed
;		DS:DI = address of global heap information
;
;   Outputs:	AX = size of largest contiguous free block
;		ES:DI = arena header of largest contiguous free block
;		DX = minimum #contiguous bytes needed
;
;   Destroys:	CX
;

gcompact:
	push	si

gcompactl:
	push	dx		    ; Save requested size
	cmp	fCompactLowerHeap,0 ; See if only compacting lower heap
	jz	gcompactn	    ; no normal compact
	mov	es,[di].hi_first    ; Yes, compact lower heap
	mov	bx,ga_next
	jmp	short gcompact1a    ; Now see any special flags

gcompactn:
	cmp	[di].gi_reserve,di  ; Is there a reserve swap area?
	je	gcompact1	    ; No, then dont compact lower heap
	mov	es,[di].hi_first    ; Yes, compact lower heap
	mov	bx,ga_next
	push	dx
	call	gcmpheap
	pop	dx
gcompact1:
	mov	es,[di].hi_last     ; Compact upper heap
	mov	bx,ga_prev
gcompact1a:
	call	gcmpheap
	pop	dx		    ; Get requested size
	mov	es,ax		    ; ES points to largest free block
	or	ax,ax		    ; Did we find a free block?
	jz	gcompact2	    ; No, try discarding
	call	gcheckfree	    ; Yes, see if block big enough
	jae	gcompactx	    ; Yes, all done
gcompact2:			    ; Discarding allowed?
	test	byte ptr [di].gi_cmpflags,GA_NODISCARD+GA_NOCOMPACT
	jnz	gcompactx	    ; No, return
	call	gdiscard	    ; No, try discarding
	jnz	gcompactl	    ; Compact again if something discarded
gcompactx:
	pop	si		    ; Restore SI
	ret

gcmpheap:
	mov	cx,[di].hi_count
	xor	ax,ax		    ; Nothing found yet
	push	ax		    ; Save largest free block so far
gchloop:
	cmp	es:[di].ga_owner,di
	je	gchfreefnd
gchnext:
	mov	es,es:[bx]
	loop	gchloop
	pop	ax		    ; Return largest free block in AX
	ret

gchfreefnd:					; Compaction allowed?
	test	byte ptr [di].gi_cmpflags,GA_NOCOMPACT
	jnz	gchmaxfree			; No, just compute max free.
;*	* if compacting upper heap, the free block must be just before another
;*	*  code block (or the end of the heap)
	push	es
	cmp	bl,ga_prev		;* upper heap ??
	jne	no_hack 		;* lower heap normal
	test	byte ptr ds:[di].gi_cmpflags,GA_DISCCODE
	jz	no_hack 		;* hack is only for discardable code
	cmp	dx,es:[di].ga_size
	ja	no_hack 		;* to small to fit anyway
	mov	es,es:[di].ga_next	;* next block (CODE or END)
	test	es:[di].ga_flags,GA_DISCCODE
	jnz	hack			;* it is code, we can use it
	mov	ax,es
	cmp	ax,ds:[di].hi_last
	je	hack
;*	* we are looking for code in the upper heap but the only free block
;*	*  that is available is not connected to the upper heap, hence we
;*	*  can't use it.
	pop	es
	pop	ax
	xor	ax,ax			;* fail, try to discard to get
					;*  legitimate space out of the code
					;*  swap area.
	ret

hack:
	pop	es
	pop	ax
	mov	ax,es			;* use this block only
	ret

no_hack:
	pop	es			;* restore es from hack check
	call	gslide
	jnz	gchfreefnd
	call	gbestfit
	jnz	gchfreefnd
gchmaxfree:
	cmp	bl,ga_prev	    ; Compacting upper heap?
	jne	gchnext 	    ; No, then dont compute largest free block
	pop	si		    ; Recover largest free block so far
	mov	ax,es		    ; AX = current free block
	cmp	si,ax		    ; Same as current?
	je	gchmf2		    ; Yes, no change then
	cmp	ES:[di].ga_owner,di ; No, is current free?
	jne	gchmf2		    ; No, ignore it then
	or	si,si		    ; Yes, First time?
	jz	gchmf1		    ; Yes, special case
	cmp	ds:[di].gi_reserve,di	; Is there a code reserve area?
	je	gchmf0			; No, continue
	test	byte ptr ds:[di].gi_cmpflags,GA_DISCCODE ; Yes, use only first free
	jnz	gchmf2		    ; block if allocating discardable code
gchmf0:
	push	es
	mov	es,si
	mov	ax,ES:[di].ga_size  ; No, get size of largest free block
	pop	es		    ; Compare with size of this free block
	cmp	ES:[di].ga_size,ax  ; Is it bigger?
	jb	gchmf2		    ; No, do nothing
gchmf1: mov	si,es		    ; Yes, remember biggest free block
gchmf2: push	si		    ; Save largest free block so far
	jmp	gchnext 	    ; Go process next block


; Subroutine to test if an object is moveable
;
;   Inputs:	ES:DI = arena header of object
;		DS:DI = address of global heap information
;		BX = ga_next or ga_prev
;
;   Outputs:	Z flag clear if object moveable
;		Z flag set if object not moveable
;
;   Destroys:	SI
;

gmoveable:
	mov	si,es:[di].ga_handle
	or	si,si				; If no handle then fixed
	jz	gmfixed
	cmp	[si].he_count,bh		; If locked then fixed
	jne	gmfixed
;**** compact kludge ****
	cmp	fCompactLowerHeap,bh		; see if in special mode
	jnz	gmokay				; Yes allow anything to move
	test	es:[di].ga_flags,GA_DISCCODE	; If discardable code
	jz	gmnotcode
	cmp	bl,ga_next			; Discardable code can only
	ret					; move up in memory
gmnotcode:
	cmp	[di].gi_reserve,di		; If no reserved code area?
	je	gmokay				; Then anything can move up
	cmp	bl,ga_prev			; Otherwise can only move down
	ret					; in memory
gmfixed:
	xor	si,si				; Return zero if fixed
gmokay:
	or	si,si				; Return with Z flag set if
	ret					; not moveable


; Subroutine to see if next/previous block can slide into the
; passed free block.
;
;   Inputs:	ES:DI = free block
;		DS:DI = address of global heap information
;		CX = #arena entries left to examine
;		BX = ga_next or ga_prev
;
;   Outputs:	Z flag clear if block found and moved into passed free
;		block and ES:DI point to new free block
;
;		Z flag set if no block found and ES:DI points to
;		original free block
;
;   Destroys:	AX,DX,SI
;

gslide:
	push	es
	mov	es,es:[bx]
	mov	ax,es
	mov	dx,es:[di].ga_size
	call	gmoveable
	pop	es
	jnz	gslidecommon
	ret
gslidecommon:			    ; Enter here from gbestfit
				    ; moving non-contiguous blocks
	mov	si,ax		    ; Source is busy block
	inc	dx		    ; DX = busy.ga_size + 1
	cmp	bl,ga_next
	je	gslidedown

; Here to slide moveable block up to high end of free block.
;
; Free and busy block adjacent
;     0000:0	|	    |		    |		|
;		|-----------|	     a ->   |-----------|
;		|   busy    |		    |	free  ? |
;		|-----------|		    |		|
;		|   free    |	     b ->   |-----------|
;		|	    |		    | ? busy  ? |
;		|-----------|	     c ->   |-----------|
;     FFFF:0	|	    |		    | ? 	|
;
;
;	a = busy
;	b = free.ga_next - busy.ga_size - 1
;	c = free.ga_next
;	destination = b
;
; Free and busy block NOT adjacent
;     0000:0	|	    |		    |		|
;		|-----------|		    |-----------|
;		|   busy    |		    |	free	|
;		|-----------|		    |-----------|
;		|	    |		    |		|
;		|-----------|	     a ->   |-----------|
;		|   free    |		    |	free  ? |
;		|	    |	     b ->   |-----------|
;		|	    |		    | ? busy  ? |
;		|-----------|	     c ->   |-----------|
;     FFFF:0	|	    |		    | ? 	|
;
;
;	a = free
;	b = free.ga_next - busy.ga_size - 1
;	c = free.ga_next
;	destination = b
;
gslideup:
	mov	ax,es:[di].ga_next
	push	ax			; Save c
	sub	ax,dx
	push	ax			; Save b
	cmp	es:[bx],si		; Are blocks adjacent?
	je	gslideup1
	push	es			; No, a = free
	jmp	short gslideup2
gslideup1:
	push	si			; Yes, a = busy
gslideup2:
	mov	es,ax			; Destination is b
	xor	ax,ax			; a.ga_prev will remain valid
	jmp	short gslidemove

; Here to slide moveable block down to low end of free block.
;
; Free and busy block adjacent
;     0000:0	|	    |		    |		|
;		|-----------|	     a ->   |-----------|
;		|   free    |		    | ? busy  ? |
;		|	    |	     b ->   |-----------|
;		|-----------|		    | ? free  ? |
;		|   busy    |		    |		|
;		|-----------|	     c ->   |-----------|
;     FFFF:0	|	    |		    | ? 	|
;
;	a = free
;	b = free + busy.ga_size + 1
;	c = busy.ga_next
;	destination = free
;
; Free and busy block NOT adjacent
;     0000:0	|	    |		    |		|
;		|-----------|	     a ->   |-----------|
;		|   free    |		    | ? busy  ? |
;		|	    |	     b ->   |-----------|
;		|	    |		    | ? free  ? |
;		|-----------|	     c ->   |-----------|
;		|	    |		    | ? 	|
;		|-----------|		    |-----------|
;		|   busy    |		    |	free	|
;		|-----------|		    |-----------|
;     FFFF:0	|	    |		    |		|
;
;
;	a = free
;	b = free + busy.ga_size + 1
;	c = free.ga_next
;	destination = free
;

gslidedown:
	cmp	es:[bx],si		; Are blocks adjacent?
	je	gslidedn1
	push	es:[di].ga_next 	; No, c = free.ga_next
	jmp	short gslidedn2
gslidedn1:
	add	ax,dx			; Yes, c = busy.ga_next
	push	ax
gslidedn2:
	mov	ax,es
	add	ax,dx
	push	ax			; Save b
	push	es			; Save a
	mov	ax,es:[di].ga_prev	; a.ga_prev must be restored after move
gslidemove:
	call	gmove
	mov	si,es			; Save new busy block location
	pop	es			; ES = a
	or	ax,ax			; Does a.prev need to be restored?
	jz	gslide1 		; No, continue
	mov	es:[di].ga_prev,ax	; Yes, do it
gslide1:
	pop	ax
	mov	es:[di].ga_next,ax	; a.ga_next = b
	mov	dx,es
	mov	es,ax
	mov	es:[di].ga_prev,dx	; b.ga_prev = a
	pop	ax
	mov	es:[di].ga_next,ax	; b.ga_next = c
	mov	dx,es
	mov	es,ax
	mov	es:[di].ga_prev,dx	; c.ga_prev = b
	mov	es,si			; ES = new busy block
	mov	si,es:[di].ga_handle	; SI = handle
	or	si,si
	jz	gslide2
	mov	ax,es
	inc	ax
	mov	ds:[si].he_address,ax	; Update client address
gslide2:
	mov	es,es:[bx]		; Move to new free block
	mov	ax,es:[di].ga_next	; Set size and signature
	mov	si,es			; byte fields of new free block
	sub	ax,si
	dec	ax
	mov	es:[di].ga_size,ax
	mov	es:[di].ga_sig,GA_SIGNATURE
	mov	es:[di].ga_flags,0
	mov	es:[di].ga_handle,di
	call	gmarkfree		; Coelesce new free block
	or	ax,ax
	ret


; Subroutine to search for the largest moveable block that
; will fit in the passed free block.
;
;   Inputs:	ES:DI = free block
;		DS:DI = address of global heap information
;		CX = #arena entries left to examine
;		BX = ga_next or ga_prev
;
;   Outputs:	Z flag set if block found and moved into passed free
;		block with no extra room.
;		ES:DI = busy block before/after new busy block.
;
;		Z flag clear if ES:DI points to a free block, either the
;		original one or what is left over after moving a block
;		into it.
;
;   Destroys:	DX,SI
;

gbestfit:
	push	es
	push	cx
	xor	si,si		    ; Have not found anything yet
	mov	dx,ES:[di].ga_size  ; Compute max size to look for
gbfloop:
	cmp	ES:[di].ga_owner,di ; Is this block busy?
	je	gbfnext 	    ; No, continue
	push	si
	call	gmoveable	    ; Yes, is it moveable
	pop	si
	jz	gbfnext 	    ; No, continue
	cmp	ES:[di].ga_size,dx  ; Yes, is block bigger than max size?
	ja	gbfnext 	    ; Yes, continue
	or	si,si		    ; First block we have found?
	jz	gbf1st		    ; Yes, special case
	push	es
	mov	es,si
	mov	ax,ES:[di].ga_size  ; No, get size of largest block so far
	pop	es		    ; Compare with this block
	cmp	ES:[di].ga_size,ax  ; Is it bigger than the largest so far?
	jbe	gbfnext 	    ; No, continue
gbf1st:
	mov	si,es		    ; Yes, remember biggest block
	mov	ax,ES:[di].ga_size  ; ...and size
gbfnext:
	mov	es,ES:[bx]	    ; Skip past this block
	loop	gbfloop
	pop	cx		    ; All done looking
	pop	es
	or	si,si		    ; Did we find a block?
	jz	gbestfit1	    ; No, return with Z flag
	call	gmovebusy	    ; Yes, move it into free block
gbestfit1:
	ret

; Subroutine to move a busy block to a free block of the same size,
; preserving the appropriate arena header fields, freeing the old
; busy block and updating the handle table entry to point to the
; new location of the block
;
;   Inputs:	SI = old busy block location
;		ES:DI = new busy block location
;		DS:DI = address of global heap information
;
;   Outputs:	ES:DI = points to new busy block arena header
;
;   Destroys:	AX,SI
;

gmovebusy:
	push	cx
	push	dx
	mov	ax,es
	mov	cx,es:[di].ga_size  ; CX = size of destination
	cmp	es:[di].ga_owner,di ; Is destination busy?
	mov	es,si
	mov	dx,es:[di].ga_size  ; DX = size of source
	jne	gmbexactfit	    ; Yes, then dont create extra block
	cmp	cx,dx		    ; No, are source and destination same size?
	je	gmbexactfit	    ; Yes, then dont create extra block

	mov	es,ax		    ; ES = destination
	mov	ax,si		    ; AX = source
	push	si		    ; Save busy block address
	call	gslidecommon	    ; Call common code to do the move
	inc	[di].hi_count	    ; Just created a new arena entry
	mov	ax,es		    ; Save new free block address
	pop	es		    ; Get old busy block address
	call	gmarkfree	    ; Mark as free and coalesce
	mov	es,ax		    ; Restore new free block address
	or	ax,ax		    ; Return with Z flag clear.
	jmp	gmbexit

gmbexactfit:
	inc	si			; SI = old client data address
	mov	cl,ES:[di].ga_flags
	push	ES:[di].ga_owner
	mov	es,ax
	pop	ES:[di].ga_owner	; Copy client words to new header
	mov	ES:[di].ga_flags,cl
	inc	ax
	mov	es,ax			; ES = new client data address
	call	gmove			; Move the client data
	dec	si
	mov	es,si			; ES:DI = old arena header
	call	gmarkfree		; Free old block
	dec	ax
	mov	es,ax			; ES:DI = new arena header
	inc	ax
	or	si,si
	jz	gmb1
	mov	[si].he_address,ax	; Set new client data address
	mov	ES:[di].ga_handle,si	; Set back link to handle in new block
	xor	si,si			; Set Z flag
gmb1:
gmbexit:
	pop	dx
	pop	cx
	ret

; Subroutine to walk segment list, discarding objects until the #paras
; discarded, plus the biggest free block is greater than the #paras
; we are looking for.
;
;   Inputs:	ES:DI = largest free block
;		AX = size of largest free block
;		DX = minimum #paras needed
;		DS:DI = address of global heap information
;
;   Outputs:	Z flag clear if one or more objects discarded.
;
;		Z flag set if no objects discarded.
;
;   Destroys:	BX,CX,SI
;

gdiscard:
	push	es
	push	ax
	push	dx

	mov	[di].hi_ncompact,0	; Clear compaction flag
	sub	dx,ax			; How much to discard before
	mov	[di].hi_distotal,dx	; compacting again.
	mov	es,ds:[di].hi_last
	mov	cx,es			; Assume no discard fence
	cmp	ds:[di].gi_reserve,di	; True if no reserve area
	je	gdstart
	test	byte ptr ds:[di].gi_cmpflags,GA_DISCCODE    ; or code request
	jnz	gdstart
gdloop0:
	mov	es,es:[di].ga_prev
	cmp	es:[di].ga_owner,di
	je	gdfence
	test	es:[di].ga_flags,GA_DISCCODE
	jnz	gdloop0
gdfence:
	sub	cx,ds:[di].gi_reserve	; Compute beginning of reserve area
	cmp	cx,es:[di].ga_next	; Does all disc. code lie within it?
	jbe	gdstart 		; Yes, set discard fence
	mov	cx,ds:[di].hi_last	; No, then no discard fence
gdstart:
	mov	ds:[di].gi_disfence,cx	; Set the discard fence
gdloop:
	call	ggetlru 		;* get least recently used code handle
	jz	gdexit			; No, more see if we discarded anything
					;* ignore items below swap fence

IFDEF DEBUG
	cmp	[si].he_count,0 	; Is this handle locked?
	je	gd_not_locked
	cCall	CowAssertFailed 	;* we do not support code locking
gd_not_locked:
ENDIF ;DEBUG
	mov	bx,[si].he_address
	dec	bx
	mov	es,bx			;* es:0 => arena
	mov	bx,si			; BX = handle
	mov	al,GN_DISCARD		; AX = GN_DISCARD
	mov	dx,es:[di].ga_owner	;* set up owner for gnotify
	push	dx			;* save owner
	push	[si].he_address 	;* save original address
	call	gnotify

	pop	ax			;* original address
	dec	ax
	mov	es,ax			; ES:DI = address of block to free
	mov	dx,es:[di].ga_size	; Save size !!!
	call	gmarkfree		; Free the block associated with this handle
					;* DX not trashed !!
	pop	[si].he_address 	; Remember owner in handle table entry
	or	[si].he_flags,HE_DISCARDED  ; ...and mark discarded
	mov	[di].hi_ncompact,1	; Remember we discarded something
	sub	[di].hi_distotal,dx	; Have we discarded enough yet?
	ja	gdloop			; No, look at next handle
gdexit:
	cmp	[di].hi_ncompact,0	; Return with Z flag set or clear
	pop	dx
	pop	ax
	pop	es
	ret


;********** ggetlru **********
;*	entry : DI = 0
;*	* scan LRU table, return handle to code segment to discard
;*	* Note : this is a simple scan to find the maximum segref
;*	*  (later on make more efficient depending on code requirements needed).
;*	* NOTE : ignores items that are above swap fence
;*		(swap fence (gi_disfence) should not be set for GA_DISCCODE
;*		   allocations).
;*	exit : Z => no more left
;*		else SI = handle

ggetlru:
	push	bp
	mov	bp,ds:[di].gi_disfence		;* swap fence limit

	mov	es,psLom
	mov	cx,es:[neLom.ne_cseg]
	dec	cx				;* count of code segments
	mov	si,es:[neLom.ne_psegrefbytes]
	mov	di,es:[neLom.ne_segtab] 	;* point to segtab
;*	* note : DI != 0
	xor	dx,dx				;* null return (handle)
	xor	ah,ah				;* see if you can beat or match 0

gget_loop:
	lods	byte ptr es:[si]		;* al = segref
	cmp	al,ah
	jl	gget_next			;* too small
;*	* test to see if below swap fence
	mov	bx,es:[di].ns_handle		;* get handle
	cmp	[bx].he_address,bp		;* above swap fence ?
	jae	gget_next			;* above => don't discard.
;*	* the new maximum (so far)
	mov	dx,bx				;* new handle
	mov	ah,al				;* new maximum segref
gget_next:
	add	di,SIZE NEW_SEG1
	loop	gget_loop
;*	* dx = handle of discardable segment with largest seg-ref (lru)
	mov	si,dx
	xor	di,di				;* restore DI
	or	si,si				;* z=> none found
	pop	bp
	ret

SUBRS	ENDP


;*****************************************************************************


sEnd	KERNEL

	END
