;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1988-1992
; *                      All Rights Reserved.
; */

;
;*****************************************************************************
;
; HIMEM4A.ASM : 286 SPECIFIC routines
;
;*****************************************************************************
;
		public	aQueryExtMemory
		public	aAllocExtMemory
		public	aFreeExtMemory
		public	aGetExtMemoryInfo
		public	aReallocExtMemory
		public	MoveBlock286
		public	aAddMem

Begin286	label	byte

;*----------------------------------------------------------------------*
;*									*
;*  QueryExtMemory -					FUNCTION 08h    *
;*									*
;*	Returns the size of the largest free extended memory block in K	*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = Size of largest free block in K.  BL = Error code	*
;*	DX = Total amount of free extended memory in K			*
;*  REGS:   AX, BX, DX, DI, SI and Flags clobbered			*
;*									*
;*  INTERNALLY REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

aQueryExtMemory proc far

	test	ah, 80h
	jnz	BadFuncJ

	push	cx
	push	si
	push	di

	mov	dl,ERR_OUTOMEMORY	; init error code in dl

;	scan for largest FREE block

	xor	di,di			; di = Max. size found so far
	xor	si,si			; si = Total amount of free memory
	mov	bx,[KiddValley]
	mov     cx,[cHandles]		; Loop through the handle table
aQEMLoop:
	cmp	[bx].Flags,FREEFLAG
	jne     aQEMBottom

	add     si,[bx].Len.lo 		; add free block to total

	mov	ax,[bx].Len.lo		; is this the largest so far?
	cmp	di,ax
	jae     aQEMBottom

	mov	di,ax			; Yes, save it away
	xor     dl,dl			; We ain't Out o' Memory

aQEMBottom:
	add	bx,SIZE Handle
	loop    aQEMLoop

	mov	ax,di
	mov	bl,dl		; Retrieve the error code
	mov     dx,si
	mov	winbug_fix,dx	; save for windows bug workaround

	pop	di
	pop	si
	pop	cx
	ret

BadFuncJ:
	jmp	BadFunc

aQueryExtMemory endp

;*----------------------------------------------------------------------*
;*									*
;*  AllocExtMemory -					FUNCTION 09h    *
;*									*
;*	Reserve a block of extended memory				*
;*									*
;*  ARGS:   DX = Amount of K being requested				*
;*  RETS:   AX = 1 of successful, 0 otherwise.	BL = Error Code		*
;*	DX = 16-bit handle to the allocated block			*
;*  REGS:   AX, BX, DX and Flags clobbered				*
;*									*
;*	Notice:  called internally from ReallocExtMemory		*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

; Algorithm -
;
;   Scan the Handle Table looking for BOTH an unused handle and
;	a free block which is large enough:
;
;   1.	If both are found -
;	Mark the free block as used and adjust its size.
;	Make the unused handle a free block containing the remaining
;		unallocated portion of the original free block.
;
;   2.	If only an unused handle is found -
;	We're out of memory.  (Unless zero length block requested)
;
;   3.	If only a properly sized free block is found -
;	We only have one handle left.
;	Mark the free block as used.  The requester gets all of the
;		block's memory.
;
;   4.	If neither are found -
;	We're out of memory.

;hFreeBlock	dw  ?
;hUnusedBlock	dw  ?

;aemStack	struc
; hFreeBlock	dw	?
; hUnusedBlock	dw	?
;aemStack	ends

aAllocExtMemoryNear proc near
aAllocExtMemoryNear endp
aAllocExtMemory proc far

	test	ah, 80h
	jnz	BadFuncJ

	cli 			; This is a non-reentrant function

	push	cx
	push	si
	push	di


; Scan the handle table looking for BOTH an unused handle and
;	a free block which is large enough.

	xor	si, si			; Have not found Free Handle
	mov	di, si			; Have not found Unused Handle

	mov	bx, [KiddValley]
	mov     cx, [cHandles]		; Loop through the handle table

;	Have we already found a free block which is large enough?

aAEMhLoop:
	or	si, si			; did we already find a Free Handle ?
	jne     aAEMUnused		; Yes, see if this one is unused

	cmp     [bx].Flags, FREEFLAG	; Is this block free?
	jne     aAEMUnused		; No, see if it is unused

	cmp	dx, [bx].Len.lo		; Is it large enough?
	ja	aAEMNexth		; No, get the next handle

	mov	si, bx			; save this one away
	jmp	short aAEMBottom

aAEMUnused:
	or	di, di			; did we already find an unused handle?
	jne     aAEMNexth		; Yes, get the next handle

	cmp     [bx].Flags, UNUSEDFLAG	; Is this block unused?
	jne     aAEMNexth		; No, get the next handle

	mov	di, bx			; save this guy away

	cmp	si, 0			; have we found all we need?
	je	aAEMNexth
aAEMBottom:
	cmp	di, 0
	jne     aAEMGotBoth		; Yes, continue

aAEMNexth:
	add	bx, SIZE Handle		; go check the next handle
	loop    aAEMhLoop

;	We are at the end of the handle table and we didn't find both
;	things we were looking for.  Did we find a free block?

	or	si, si
	jnz     aAEMRetSuc		; Yes, Case 3 - Alloc the entire block

	or	di, di			; did we find an unused handle?
	jz	aAEMOOHandles		; No, Case 4 - We're out of handles

	or	dx, dx			; Case 2 - req for zero-length handle?
	jnz     aAEMOOMemory		; No, we're out of memory

	mov	si, di			; reserve the zero-length handle
	mov	[si].Len.lo, 0		; force length field to zero
	jmp	short aAEMRetSuc

aAEMGotBoth:

;	 We're at Case 1 above.
;	Mark the free block as used (done below) and adjust its size.
;	Make the unused handle a free block containing the remaining
;	   unallocated portion of the original free block.

	mov	ax, [si].Base.lo	; Unused.Base = Old.Base + request
	add	ax, dx
	mov	[di].Base.lo, ax

	mov	ax, dx			; New.Len = request
	xchg    [si].Len.lo, ax

	sub	ax, dx			; Unused.Len = Old.Len - request
	mov	[di].Len.lo, ax
	mov	[di].Flags, FREEFLAG	; Unused.Flags = FREE
	jnz	aAEM_nonzero_residual
	mov	[di].Flags, UNUSEDFLAG	; Unused.Flags = UNUSED
aAEM_nonzero_residual:

aAEMRetSuc:
	mov	[si].Flags, USEDFLAG	; New.Flags = USED

	if	keep_cs
	mov	ax, callers_cs
	mov	[si].Acs, ax		; keep track of allocator's cs:
	endif

	mov	dx, si
	mov	ax, 1
	xor	bl, bl
aAEM9:
	pop	di
	pop	si
	pop	cx
	ret

aAEMOOMemory:
	mov	bl, ERR_OUTOMEMORY
	jmp	short aAEMErrRet

aAEMOOHandles:
	mov	bl, ERR_OUTOHANDLES
aAEMErrRet:
	xor	ax, ax			; Return failure
	mov	dx, ax
	jmp	short aAEM9		;
aAllocExtMemory endp

;*----------------------------------------------------------------------*
;*									*
;*  FreeExtMemory -					FUNCTION 0Ah    *
;*									*
;*	Frees a block of extended memory				*
;*									*
;*  ARGS:   DX = 16-bit handle to the extended memory block		*
;*  RETS:   AX = 1 if successful, 0 otherwise.	BL = Error code		*
;*  REGS:   AX, BX, CX, DX, SI, DI and Flags clobbered			*
;*									*
;*	called internally from ReallocExtMemory				*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

aFreeExtMemoryNear proc near
aFreeExtMemoryNear endp

aFreeExtMemory proc far

	cli 				; This is a non-reentrant function

	push	cx
	push	dx
	push	si
	push	di

	call    ValidateHandle		; Make sure handle is valid
	jnc     aFEMBadh
	mov	si,dx			; Move the handle into SI

	cmp	[si].cLock,0		; make sure it points to unlocked block
	jne     aFEMLockedh

	mov	[si].Flags,UNUSEDFLAG	;  mark it as UNUSED
	cmp	[si].Len.lo,0		; if zero length block
	jz	aFEMExit			; done if it was zero length
	mov	[si].Flags,FREEFLAG	; mark it as FREE

;	now see if there's a free adjacent block

aFEMScanIt:
	mov	bx,[si].Base.lo		; BX = base of block
	mov	ax,bx			; calculate ax = top of block
	add	ax,[si].Len.lo

;	Search for an adjacent FREE block.

	mov	di,[KiddValley]		; di = Handle being scanned
	mov     cx,[cHandles]		; Loop through the handle table
aFEMLoopTop:
	cmp	[di].Flags,FREEFLAG	; Is this block free?
	jne     aFEMNexth		; No, continue

	mov	dx,[di].Base.lo		; is this block just above freed one?
	cmp     dx,ax
	je	aFEMBlockAbove     	; Yes, coalesce upwards

	add	dx,[di].Len.lo		; is it just below?
	cmp     dx,bx
	je	aFEMBlockBelow     	; Yes, coalesce downwards

aFEMNexth:
	add	di,SIZE Handle
	loop    aFEMLoopTop

;	No adjacent blocks to coalesce.

aFEMExit:
	mov     ax,1			; Return success
	xor	bl,bl
aFEM9:
	pop	di
	pop	si
	pop	dx
	pop	cx

	ret

;	Exchange the pointer to the "upper" and "lower" blocks.

aFEMBlockBelow:
	xchg    si,di

;	Move the free block above into the current handle.

aFEMBlockAbove:
	mov	dx,[si].Len.lo
	add     dx,[di].Len.lo 		; Combine the lengths
	mov	[si].Len.lo,dx
	mov     [di].Flags,UNUSEDFLAG   ; Mark old block's handle as UNUSED
	jmp	short aFEMScanIt		; Rescan the list

aFEMBadh:
	mov	bl,ERR_INVALIDHANDLE
	jmp	short aFEMErrExit

aFEMLockedh:
	mov	bl,ERR_EMBLOCKED
aFEMErrExit:
	xor     ax,ax			; Return failure
	jmp	short aFEM9
aFreeExtMemory endp



;*----------------------------------------------------------------------*
;*									*
;*  aGetExtMemoryInfo -					FUNCTION 0Eh    *
;*									*
;*	Gets other information about an extended memory block		*
;*									*
;*  ARGS:   DX = 16-bit handle to the extended memory block		*
;*  RETS:   AX = 1 if successful, 0 otherwise.	BL = Error code		*
;*	    BH = EMB's lock count					*
;*	    BL = Total number of unused handles in the system		*
;*	    DX = EMB's length						*
;*  REGS:   AX, BX, CX, DX and Flags clobbered				*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

aGetExtMemoryInfo proc	far

	test	ah, 80h
	jnz	BadFunc

	cli 			; This is a non-reentrant function

	push	cx
	push	si

	call    ValidateHandle	; is the handle valid?
	jnc     aGEMBadh
	mov     si,dx		; Move the handle into SI

	cmp	[si].Len.hi, 0	; Size > 64M ?
	jne	aGEMBadh		; yes, we cannot handle this function

	xor     ax,ax		; count number of UNUSED handles
	mov     bx,[KiddValley]
	mov     cx,[cHandles]	; Loop through the handle table
aGEMLoop:
	cmp     [bx].Flags,USEDFLAG ; Is this handle in use?
	je	aGEMNexth	; Yes, continue
	inc     ax		; No, increment the count
aGEMNexth:
	add     bx,SIZE Handle
	loop    aGEMLoop

	mov     dx,[si].Len.lo 	; Length in DX
	mov     bh,[si].cLock	; Lock count in BH
	or	ah, ah		; Free handles > 255 ?
	jz	@f
	mov	al, 0ffh	; make it 255 if > 255
@@:
	pop	si
	pop	cx
	mov     bl,al
	mov	ax,1
	ret

aGEMBadh:
	pop	si
	pop	cx
	mov	bl,ERR_INVALIDHANDLE
	xor	ax,ax
	ret

aGetExtMemoryInfo endp

;*----------------------------------------------------------------------*
;*									*
;*  BadFunc -								*
;*									*
;*  ARGS:   None							*
;*  RETS:   Invalid function error in Bl and 0 in AX			*
;*  REGS:   Trashes AX & BL						*
;*									*
;*----------------------------------------------------------------------*

BadFunc	proc	near
	mov	bl, ERR_NOTIMPLEMENTED
	xor	ax, ax
	ret
BadFunc	endp

;*----------------------------------------------------------------------*
;*									*
;*  ReallocExtMemory -					FUNCTION 0Fh    *
;*									*
;*	Reallocates a block of extended memory				*
;*									*
;*  ARGS:   DX = 16-bit handle to the extended memory block		*
;*	    BX = new size for block					*
;*  RETS:   AX = 1 if successful, 0 otherwise.	BL = Error code		*
;*  REGS:   trashes si,di,bx,cx,dx					*
;*									*
;*  INTERNALLY NON-REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

;	Define our memory move structure for calling the move function

aReallocExtMemory	proc	far

	test	ah, 80h
	jnz	BadFunc

	cli 			; This is a non-reentrant function

	push	cx
	push	dx
	push	si
	push	di

	push    bp		; preserve caller's bp

	call    ValidateHandle	; is the handle valid?
	mov	si,dx		; Move the handle into SI
	mov	dx,bx		; Move the new length into dx
	mov	bl,ERR_INVALIDHANDLE
	jnc     aREMError

	cmp	[si].cLock,0	; We can only work on unlocked EMBs
	mov	bl,ERR_EMBLOCKED
	jnz	aREMError

;	There are basically five successful cases for this function:
;
;	1. The new size is the same.  No operation.
;
;	2. We're making the block smaller.  Truncate & Coalesce.
;
;	3. We're making the block bigger and sufficient memory is
;	   available from the immediately following block to append.
;
;	4. We're making the block bigger and sufficient memory is
;	   available from the immediately preceding block (and possibly
;	   the following block as well).  Add the memory and slide data down.
;
;	5. We must find a whole new block which is bigger.
;
	cmp	dx,[si].Len.lo		; check for cases 1 and 2
	ja	aREMGrow			; cases 3, 4, 5 - growing
	jb	aREMShrink		; case 2 - shrinking

;	Case 1:  no change in size.  We're done.

aREMExit:
	mov	ax,1			; succesful return
	xor	bl,bl			; non-documented no-error return
aREM9:
	pop	bp			; restore caller's bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	ret

;	Case 2: Truncate the block and coalesce
;	   Scan the list of handles to see if there is an
;	   existing EMB immediately following us that we can
;	   append the memory to.

aREMShrink:

	push    dx			; save new size
	call    aFindAdjacent		; see if a block follows us
	pop	dx			; restore our new size
	or	bp,bp			; does a block follow us?
	jz	aREMShrink5		;  brif not

;	This is the easy case of shrinking.  All we have to do is move
;	   the deallocated memory into the following block

	mov	ax,[si].Len.lo		; old length
	sub	ax,dx			; we're truncating this much
	mov	[si].Len.lo,dx		; adjust our block downwards

	add	ds:[bp].Len.lo,ax		; increase next block by same
	sub	ds:[bp].Base.lo,ax		;  and lower its base

	jmp	short aREMExit		; exit okay!

;	Locate an error return here for easy access

aREMError:
	xor	ax,ax
	jmp	short aREM9

;	We'll have to create a new block to give the freed space to.

aREMShrink5:
	mov	di,[KiddValley]		; look for an available handle
	mov	cx,[cHandles]
aREMS5a:
	cmp	[di].Flags,UNUSEDFLAG
	jz	aREMS5b			; brif found free handle
	add	di,SIZE Handle
	loop    aREMS5a
	mov	bl,ERR_OUTOHANDLES
	jmp	short aREMError		; Abort without shrinking if none

aREMS5b:
	mov	ax,dx			; get new length
	add	ax,[si].Base.lo		;  calculate begin of new block
	mov	[di].Base.lo,ax

	mov	ax,[si].Len.lo		; new length
	sub	ax,dx			;  calculate length of new block
	mov	[di].Len.lo,ax

	mov	[di].Flags,FREEFLAG     ; setup lock count and flags
	mov	[di].cLock,0

	mov	[si].Len.lo,dx		; adjust length of original block

	jmp	short aREMExit		; exit okay!


aREMGrow:
	push    dx			; save new length
	call    aFindAdjacent		; locate adjacent blocks
	pop	dx

;	hopefully the following block is big enough.  that's the easiest
;	   case.  If so, we'll just snatch the space and then kill it if
;	   its size goes to zero.

;	Don't forget:  di = preceding block's handle (or zero)
;		   si = original block's handle
;		   bp = following block's handle (or zero)

	or	bp,bp			; is there a following block?
	jz	aREMGrow4		; skip if not
	mov	ax,dx			; new size
	sub	ax,[si].Len.lo		;  calculate growth amount
	cmp	ax,ds:[bp].Len.lo	; will next block accomodate?
	ja	aREMGrow4		;  nope.  not big enough.

;	Case 3: The following block has enough space for our needs.

	jz	aREMGrow2		; skip if we're using whole block

	sub	ds:[bp].Len.lo,ax	; make it smaller
	add	ds:[bp].Base.lo,ax		;  and start later
	jmp	short aREMGrow3

aREMGrow2:
	mov	ds:[bp].Flags,UNUSEDFLAG; mark it as free
aREMGrow3:
	mov	[si].Len.lo,dx		; update length of caller's block
	jmp	aREMExit

;	Come here if we can't find enough room from a following block.
;	  Now decide whether to use case 4 (use prev or both adjacent blocks)
;	  or case 5 (find another block entirely).  Notice that either
;	  of these solutions will require a memory move operation.

aREMGrow4:
	or	di,di			; is there a preceding block?
	jz	aREMGrow7x		;  nope.  resort to case 5

	mov	ax,[di].Len.lo
	add	ax,[si].Len.lo		; get size of prev + orig block
	or	bp,bp			; Is there a following block?
	jz	aREMGrow4a		;  skip if not
	add	ax,ds:[bp].Len.lo		; calculate max possible resource
aREMGrow4a:
	cmp	dx,ax			; is all of that sufficient
	jna	aREMGrow5		;  to handle whole thing? brif so
aREMGrow7x:
	jmp	aREMGrow7

;	There will be only one or two blocks left of the possible three
;	 when we're finished.  We'll start by merging the following block
;	 onto the middle block and freeing its handle, but we'll save the
;	 original length of the middle block so we don't end up moving more
;	 memory than we need.

aREMGrow5:

;	Now we need a move structure.  Allocate one on the stack and point
;	   es:bx to it

;	Always make sure stack stays on even boundary

	sub	sp,0fffeh and (1+SIZE MoveExtendedStruc)
	push    ss			; point es:bx to stack frame
	pop	es
	mov	bx,sp

	push    [si].Len.lo		; save length of move
	or	bp,bp			;  is there a following block?
	jz	aREMGrow5a		;  brif not

	mov	ax,ds:[bp].Len.lo		; get it's length
	add	[si].Len.lo,ax		; suck it in
	mov	ds:[bp].Flags,UNUSEDFLAG; that handle is now unused

;	Now change the main block to include all of the space.  Notice
;	  that the 'previous' block will still exist as an overlapping
;	  subset of the total block during the move.  We'll sort it out
;	  again after the move when we're able to disable interrupts
;	  again.

aREMGrow5a:
	mov	ax,[di].Len.lo		; get length of bottom block
	add	[si].Len.lo,ax		; add it into our target block
	push    [si].Base.lo		; save base of data block to move
	mov	ax,[di].Base.lo		; base of bottom block becomes
	mov	[si].Base.lo,ax		;  of super block
	inc	[di].cLock		; lock both blocks so's we don't
	inc	[si].cLock		;  get moved around

;	Now make the move in the new merged block.  Offsets are relative
;	  to the base of the block.  Zero is the destination.  The old
;	  base ([sp]) is the source (in 1k), the old length ([sp+2]) is
;	  the length (in 1k).

	mov	es:[bx].SourceHandle,si	; both handles = our main block
	mov	es:[bx].DestHandle,si
	mov	es:word ptr [bx].DestOffset,0 ; move data to base of block
	mov	es:word ptr [bx].DestOffset+2,0

	pop	ax			; get source begin address
	sub	ax,[si].Base.lo		; find offset within block

;	now use trick to make cx:ax = ax * 1024

	ror	ax,6			; like rcl ax,10 but faster
	mov	cx,ax
	and	cx,3ffh
	and	ax,0fc00h
	mov	es:word ptr [bx].SourceOffset,ax
	mov	es:word ptr [bx].SourceOffset+2,cx

	pop	ax			; get length in Kilobytes
	ror	ax,6			; trick multiply by 1024 again
	mov	cx,ax
	and	cx,3ffh
	and	ax,0fc00h
	mov	es:word ptr [bx].bCount,ax
	mov	es:word ptr [bx].bCount+2,cx

	push    si			; save main handle
	push    di			; save residual handle
	push    dx			; save new block length
	mov	si,bx
;
	push	cs
	call	BlkMovX
	pop	dx
	pop	di
	pop	si

;	Release the move structure stack frame

	add	sp,0fffeh and (1+SIZE MoveExtendedStruc)

	cli				; can't mess with us
	dec	[di].cLock		; remove our locks
	dec	[si].cLock
	mov	ax,[si].Len.lo		; get total block length
	sub	ax,dx			;  less amount requested by user
	mov	[si].Len.lo,dx
	jz	aREMGrow5b		; branch if no residual block

	add	dx,[si].Base.lo		; get base of residual block
	mov	[di].Base.lo,dx
	mov	[di].Len.lo,ax		; save length of residual block
	jmp	aREMExit			; We're done!

aREMGrow5b:
	mov	[di].Flags,UNUSEDFLAG   ; free the handle if block empty
	jmp	aREMExit			; DONE!

;	Another error return centrally for easy conditional branch access

aREMErrora:
	xor	ax,ax			; indicate error condition
	jmp	aREM9

aREMGrow7:
	push    si			; save our handle
	push    dx			; save our desired length
	push	cs			; fake a far call
	call	aAllocExtMemoryNear	; try to allocate a new block
	cli
	mov	di,dx			;  save its results
	pop	dx
	pop	si
	or	ax,ax			; did we fail?
	mov	bl,ERR_OUTOMEMORY	;  not enough memory
	jz	aREMErrora
	inc	[di].cLock		; lock both blocks so's we don't
	inc	[si].cLock		;  get moved around

;	Now move the data from the old block to the new one before
;	  swapping the handles and deallocating the original block

;	Allocate a stack frame for a move structure

	sub	sp,0fffeh and (1+SIZE MoveExtendedStruc)
	push    ss			; point es:bx to a move structure
	pop	es
	mov	bx,sp

	mov	es:[bx].SourceHandle,si ; source handle = our main block
	mov	es:[bx].DestHandle,di   ; dest handle = new block
	xor	cx,cx
	mov	es:word ptr [bx].DestOffset,cx ; move to base of new block
	mov	es:word ptr [bx].DestOffset+2,cx
	mov	es:word ptr [bx].SourceOffset,cx ; from base of old block
	mov	es:word ptr [bx].SourceOffset+2,cx

	mov	ax,[si].Len.lo		; get length of old block
	ror	ax,6			; trick multiply by 1024
	mov	cx,ax
	and	cx,3ffh
	and	ax,0fc00h
	mov	es:word ptr [bx].bCount,ax
	mov	es:word ptr [bx].bCount+2,cx

	push    si			; save main handle
	push    di			; save new handle
	mov	si,bx
	push	cs
	call	BlkMovX
	pop	di
	pop	si

;	Deallocate stack frame

	add	sp,0fffeh and (1+SIZE MoveExtendedStruc)

	cli				; can't mess with us

;	Now we have to reverse the two handles so that we can return
;	  the new block to the user without changing his handle

	mov	ax,[si].Base.lo
	xchg    ax,[di].Base.lo
	mov	[si].Base.lo,ax

	mov	ax,[si].Len.lo
	xchg    ax,[di].Len.lo
	mov	[si].Len.lo,ax

	dec	[si].cLock		; remove our locks
	dec	[di].cLock

	mov	dx,di			; handle to free
	pop	bp			; restore caller's bp

	pop	di
	pop	si
	pop	cx			; suck out dx on stack
	pop	cx

	jmp	aFreeExtMemoryNear	; do it!, return any errors

aReallocExtMemory	endp


;*----------------------------------------------------------------------*
;*									*
;*  FindAdjacent unused blocks						*
;*									*
;*	Scan through handle list looking for blocks adjacent		*
;*	  to a given handle.						*
;*									*
;*  ARGS:   SI handle of original block					*
;*  RETS:   DI = handle of adjacent block below or zero if none		*
;*	BP = handle of adjacent block above or zero if none		*
;*									*
;*  TRASHES: AX,BX,CX,DX						*
;*									*
;*  messes with handle table - not reentrant - assumes ints disabled	*
;*									*
;*----------------------------------------------------------------------*

aFindAdjacent	proc	near

	mov	ax,[si].Base.lo		; look for blocks ending here
	mov	dx,[si].Len.lo
	add	dx,ax			; and ending here

	xor	di,di			; initialize to fail condition
	mov	bp,di

	mov	bx,[KiddValley]		; prepare to loop thru handle tab
	mov	cx,[cHandles]

	push    si			; preserve original handle

aFindAdj1:
	cmp	[bx].Flags,FREEFLAG
	jnz	aFindAdj3		; ignore blocks that aren't UNUSED
	mov	si,[bx].Base.lo
	cmp	dx,si			; found beg block?
	jnz	aFindAdj2		;  skip if not
	mov	bp,bx			;  remember the handle
	or	di,di			; did we already find a follower?
	jnz	aFindAdj9		;  we're done if so

aFindAdj2:
	add	si,[bx].Len.lo		; find length
	cmp	si,ax			; does this block end at spec addr?
	jnz	aFindAdj3		;  skip if not
	mov	di,bx			;  remember the handle
	or	bp,bp			; did we already find a leader?
	jnz	aFindAdj9		;  we're done if so

aFindAdj3:
	add	bx,SIZE handle
	loop    aFindAdj1

aFindAdj9:
	pop	si			; restore original handle
	ret
;

aFindAdjacent	endp

;*----------------------------------------------------------------------*
;*									*
;*  MoveExtMemory -					FUNCTION 0Bh    *
;*									*
;*	Copys a block of memory from anywhere to anywhere		*
;*									*
;*  ARGS:   ES:SI = Pointer to an Extended Memory Block Move Structure	*
;*		(NOTE: Originally passed in as DS:SI)			*
;*  RETS:   AX = 1 of successful, 0 otherwise.	BL = Error code.	*
;*  REGS:   Everybody clobbered						*
;*									*
;*	Note:  A20 must be enabled before we're invoked!!!!		*
;*									*
;*	Notice:  called internally from ReallocExtMemory		*
;*									*
;*  INTERNALLY REENTRANT (believe it or not)				*
;*									*
;*----------------------------------------------------------------------*

	EVEN				; Must be word aligned.

; NOTE: From here on, the code/data is variable, and will be setup
; by the init code.

;*----------------------------------------------------------------------*

	public	CS0, CS1, LCSS, CSDES

MoveBlock286:

;*******************************************************************************
;
; MoveExtended286
;	XMM Move Extended Memory Block for the 80286
;
;	Copyright (c) 1988, Microsoft Corporation
;
; Entry:
;	ES:BX	Points to a MoveExtendedStruc
;
; Return:
;	AX = 1	Success
;	AX = 0	Failure
;		Error Code in BL
;
; Registers Destroyed:
;	Flags, CX, SI, DI, ES
;
;			WARNING
;			=======
;
; This routine enables interrupts and can be re-entered
;
; Notes:
;	The case of copying from conventional to conventional memory
;	is not treated specially in this example.
;
; History:
;	Wed Jul 13 - AWG - Original version
;-------------------------------------------------------------------------------
MoveExtended286 proc	far

	assume	ds:_text

	sti					; Be nice
	push	bp				; Set up stack frame so we
	mov	bp, sp				; can have local variables
	sub	sp, 18				; Space for local variables
Count	  = -4					; Local DWORD for byte count
MEReturn  = -6					; Local WORD for return code
SrcHandle = -8
DstHandle = -10
SrcLinear = -14
DstLinear = -18
	push	bx
	push	dx

	xor	ax, ax
	mov	[bp.MEReturn], ax		; Assume success
	mov	[bp.SrcHandle], ax
	mov	[bp.DstHandle], ax
	mov	ax, word ptr es:[si].bCount	; Pick up length specified
	mov	word ptr [bp.Count], ax
	mov	cx, word ptr es:[si].bCount+2
	mov	word ptr [bp.Count+2], cx
	or	cx, ax
	jcxz	MEM2_Exit			; Exit immediately if zero

	lea	bx, [si].SourceHandle		; Normalize Source
	call	GetLinear286			; Linear address in DX:AX
	jc	MEM2_SrcError			; Have Dest Error Code

	mov	word ptr [bp.SrcLinear], ax	; Save Linear address
	mov	word ptr [bp.SrcLinear+2], dx
	mov	[bp.SrcHandle], bx		; Save Handle for Unlock

	lea	bx, [si].DestHandle		; Normalize Destination
	call	GetLinear286
	jc	MEM2_Error

	mov	word ptr [bp.DstLinear], ax	; Save Linear address
	mov	word ptr [bp.DstLinear+2], dx
	mov	[bp.DstHandle], bx		; Save Handle for Unlock

	shr	word ptr [bp.Count+2], 1	; Make word count
	rcr	word ptr [bp.Count], 1
	jc	MEM2_InvCount			; Odd count not allowed

	call	LEnblA20
	cmp	ax, 1
	jne	MEM2_Error

	call	DoLoadAll

	call	LDSblA20
	cmp	ax, 1
	jne	MEM2_Error

MEM2_Exit:
	mov	bx, [bp.SrcHandle]		; Unlock Handles if necessary
	or	bx, bx
	jz	NoSrcHandle
	dec	[bx].cLock			; Unlock Source
NoSrcHandle:
	mov	bx, [bp.DstHandle]
	or	bx, bx
	jz	NoDstHandle
	dec	[bx].cLock			; Unlock Destination
NoDstHandle:
	pop	dx
	pop	bx
	mov	ax, 1
	cmp	word ptr [bp.MEReturn], 0
	jz	MEM2_Success
	dec	ax
	mov	bl, byte ptr [bp.MEReturn]
MEM2_Success:
	mov	sp, bp				; Unwind stack
	pop	bp
	ret

MEM2_SrcError:
	cmp	bl, ERR_LENINVALID		; Invalid count
	je	MEM2_Error			;  yes, no fiddle
	sub	bl, 2				; Convert to Source error code
	jmp	short MEM2_Error
MEM2_InvCount:
	mov	bl, ERR_LENINVALID
MEM2_Error:
	mov	byte ptr [bp.MEReturn], bl	; Pass error code through
	jmp	short MEM2_Exit

;*******************************************************************************
;
; GetLinear286
;	Convert Handle and Offset (or 0 and SEG:OFFSET) into Linear address
;	Locks Handle if necessary
;	Nested with MoveExtended286 to access local variables
;
; Entry:
;	ES:BX	Points to structure containing:
;			Handle	dw
;			Offset	dd
;	[BP.Count]	Count of bytes to move
;
; Return:
;	BX	Handle of block (0 if conventional)
;	AX:DX	Linear address
;	CARRY	=> Error
;		Error code in BL
;
; Registers Destroyed:
;	Flags, CX, DI
;
;-------------------------------------------------------------------------------

GetLinear286	proc	near
	push	si
	cli					; NO INTERRUPTS
	mov	si, word ptr es:[bx+2]		; Offset from start of handle
	mov	di, word ptr es:[bx+4]		; in DI:SI
	mov	bx, word ptr es:[bx]		; Handle in bx
	or	bx, bx
	jz	GL2_Conventional

	test	[bx].Flags, USEDFLAG		; Valid Handle?
	jz	GL2_InvHandle

	mov	ax, [bx].Len.lo			; Length of Block
	mov	cx, 1024
	mul	cx				; mul is faster on the 286
	sub	ax, si
	sbb	dx, di				; DX:AX = max possible count
	jc	GL2_InvOffset			; Base past end of block
	sub	ax, word ptr [bp.Count]
	sbb	dx, word ptr [bp.Count+2]
	jc	GL2_InvCount			; Count too big

	inc	[bx].cLock			; Lock the Handle
	mov	ax, [bx].Base.lo
	mul	cx
	add	ax, si				; Linear address
	adc	dx, di				; in DX:AX

GL2_OKExit:
	clc
GL2_Exit:
	sti
	pop	si
	ret

GL2_Conventional:
	mov	ax, di				; Convert SEG:OFFSET into
	mov	dx, 16				; 24 bit address
	mul	dx
	add	ax, si
	adc	dx, 0				; DX:AX has base address
	mov	di, dx
	mov	si, ax
	add	si, word ptr [bp.Count]		; Get End of Block + 1 in DI:SI
	adc	di, word ptr [bp.Count+2]

	cmp	di, 010h			; Make sure it doesn't wrap
	ja	GL2_InvCount			;  past the end of the HMA
	jb	GL2_OKExit
	cmp	si, 0FFF0h
	jbe	GL2_OKExit			; Must be < 10FFF0h
GL2_InvCount:
	mov	bl, ERR_LENINVALID
	jmp	short GL2_Error

GL2_InvHandle:
	mov	bl, ERR_DHINVALID		; Dest handle invalid
	jmp	short GL2_Error

GL2_InvOffset:
	mov	bl, ERR_DOINVALID		; Dest Offset invalid
GL2_Error:
	stc
	jmp	short GL2_Exit

GetLinear286	endp

;******************************************************************************
;
; DoLoadAll
;	Use 286 LoadAll for copy - see warnings below
;	Nested within MoveExtended286
;
; Entry:
;	[BP.Count]	Word count for move
;	[BP.SrcLinear]	Linear address of the source
;	[BP.DstLinear]	Linear address of the destination
;
;	Interrupts are ON
;
; Return:
;	CARRY	=> Error
;		Error code in BL
;
; Registers Destroyed:
;	Flags, AX, BX, CX, DX, SI, DI
;
;------------------------------------------------------------------------------

	EVEN		;* WORD alignment for data

; Swap buffer for contents of 80:0
cwBuffer  EQU	51
rgwSwap80 DW	cwBuffer DUP (?)

; LOADALL data buffer placed at 80:0
;
LOADALL_TBL	LABEL	BYTE
	DB	6 DUP(0)
LDSW	DW	?
	DB	14 DUP (0)
TR	DW	0
LFLAGS	DW	0		; High 4 bits 0, Int off, Direction clear
				;   Trace clear. Rest don't care.
LIP	DW	OFFSET	AFTER_LOADALL
LDT	DW	0
LDSS	DW	8000H
LSSS	DW	80H
LCSS	DW	?
LESS	DW	8000H
LDI	DW	0
LSI	DW	Offset rgwSwap80
LBP	DW	0
LSP	DW	?
LBX	DW	?
LDX	DW	?
LCX	DW	cwBuffer
LAX	DW	80H
ESDES	SEGREG_DESCRIPTOR <>
CSDES	SEGREG_DESCRIPTOR <>
SSDES	SEGREG_DESCRIPTOR <800H,0>
DSDES	SEGREG_DESCRIPTOR <>
GDTDES	DTR_DESCRIPTOR <>
LDTDES	DTR_DESCRIPTOR <0D000H,0,0FFH,0088H>
IDTDES	DTR_DESCRIPTOR <>
TSSDES	DTR_DESCRIPTOR <0C000H,0,0FFH,0800H>

DescSaved	dw	-1		; Flag for reentrancy

SaveDesc	macro	reg
	push	word ptr reg		; Save 3 word descriptor
	push	word ptr reg+2	; pointed to by reg
	push	word ptr reg+4
	endm

RestoreDesc	macro	reg
	pop	word ptr reg+4	; Restore 3 word descriptor
	pop	word ptr reg+2	; pointed to by reg
	pop	word ptr reg
	endm

NOP4	macro
	sti				; Faster than nop
	sti
	sti
	sti
	endm

DoLoadAll	proc	near


	cld					;* just to be sure

	mov	ax, word ptr [bp.SrcLinear]	; Create descriptors for source
	mov	dl, byte ptr [bp.SrcLinear+2]	; and destination of transfer
	mov	cx, word ptr [bp.DstLinear]
	mov	dh, byte ptr [bp.DstLinear+2]
	xchg	[DSDES].SEG_BASE_LO,ax		; Fill in table and pick up
	xchg	[DSDES].SEG_BASE_HI,dl		; old values
	xchg	[ESDES].SEG_BASE_LO,cx
	xchg	[ESDES].SEG_BASE_HI,dh

	mov	bx,Offset IDTDES
	mov	si,Offset GDTDES

	inc	[DescSaved]
	jz	DLA_NoSave			; Don't save old vals 1st time
	push	ax				; Save so we can be re-entrant
	push	cx
	push	dx
	push	[LDSW]
	SaveDesc	cs:[bx]			; Save IDTDES on stack
	SaveDesc	cs:[si]			; Save GDTDES on stack
DLA_NoSave:

	smsw	[LDSW]				; put MSW, GDT & IDT in buffer

;* NOW The damn SXXX instructions store the desriptors in a
;*	  different order than LOADALL wants

	sidt	cs:fword ptr [bx]
	call	FixDescriptor
	mov	bx, si
	sgdt	cs:fword ptr [bx]
	call	FixDescriptor

DLA_MoveLoop:
						; byte count fits in one word
	mov	cx, 7fffh			; Must be < 8000h
	cmp	word ptr [bp.Count+2], 0	; Lots to do?
	ja	DLA_DoMove
	cmp	word ptr [bp.Count], cx
	jae	DLA_DoMove
	mov	cx, word ptr [bp.Count]		; Just what is left

DLA_DoMove:
	push	ds
	push	bp			;* gets trashed later
	mov	ax, 80H			; Set up for first copy - can do before
	mov	bx, cs			; disabling interrupts
	mov	dx, cwBuffer
	xor	si, si
	mov	es, bx
	mov	di, Offset rgwSwap80	;* ES:DI = CS:rgwSwap80 = save address

	CLI				; Un interruptable
	MOV	[LSP], sp		;* the real stack limit
	mov	[LBX], ss
	mov	[LDX], cx		;* the actual count of words to move
					;* will be in DX
	mov	ds, ax			;* DS:SI = 80:0 = source address

	assume	ds:nothing

	mov	cx, dx			; cwBuffer

	rep movsw			; Save contents of 80:0

;*	* now move info from LOADALL_TBL buffer into 80:0

	xor	di, di
	mov	es, ax			;* ES:DI = 80:0 (dest)
	mov	si, Offset LOADALL_TBL
	mov	ds, bx			;* DS:SI = CS:LOADALL_TBL (src)
	mov	cx, dx			; cwBuffer
	rep movsw			; Transfer in LOADALL info
	LODAL286			;* LOADALL INSTRUCTION (set AX = 80H)

; now set up stack for moving 80:0 info back again
; LOADALL will set the following registers:
;	AX = 80H
;	BX = old SS
;	CX = cwBufer
;	DX = word count to move
;	SI = OFFSET rgwSwap80
;	DI = 0
;	BP = 0, SS = 80H  (SS:BP => loadall area to restore)
;	DS:0 = source address (DS may be outside real mode), do not rely
;		on the DS register.
;	ES:0 = destination address (ES may be outside real mode), do not
;		rely on the ES register unless reading.

AFTER_LOADALL:
move_loop:
	lods	word ptr cs:[si]
	mov	ss:[bp],ax		;* can't override ES
	inc	bp
	inc	bp
	loop	move_loop

;*	* now actually move the data

	mov	ss, bx	 		;* restore SS
	mov	cx, dx			;* actual word count
	mov	si, di			;* source and destination 0
	STI

;* * * ** End Interrupt Off Code
;* * * ** Begin Interrupt Protected Code

	rep movsw			;* Move data

	db	0EAh				; jmp far move_stopped
	dw	Offset move_stopped		;	fixes up CS
cs0	dw	0

;* * * ** End Interrupt Protected Code
;*	* if this ever gets interrupted, we will jump 16 bytes
;*	*  CS will be bogus CS+1 value
;		CX WILL INDICATE IF THE MOVE WAS COMPLETED
	NOP4
	NOP4
	NOP4
	NOP4

	db	0EAh				; jmp far move_stopped
	dw	Offset move_stopped		;	fixes up CS
cs1	dw	0

move_stopped:					;* resume after move stops
	pop	bp
	pop	ds				; Get our DS back
	assume	ds:_text

	mov	cx, si
	shr	cx, 1				; # words moved
	sub	word ptr [bp.Count], cx		; Subtract what we just did
	jz	DLA_TestDone
	sbb	word ptr [bp.Count+2], 0	; Not zero, more to do
DLA_KeepGoing:
	add	[DSDES].SEG_BASE_LO, si		; Update base of segments to end
	adc	[DSDES].SEG_BASE_HI, 0		; of transfer done so far.  Our
	add	[ESDES].SEG_BASE_LO, di		; LoadAll buffer always puts 0
	adc	[ESDES].SEG_BASE_HI, 0		; in SI and DI
	jmp	DLA_MoveLoop

DLA_TestDone:
	sbb	word ptr [bp.Count+2], 0
	jnz	DLA_KeepGoing			; High word nonzero, more to do

DLA_done:
	cmp	[DescSaved], 0			; Did we save registers?
	jz	DLA_NoRestore

	mov	bx, offset GDTDES
	RestoreDesc	cs:[bx]
	mov	bx, offset IDTDES
	RestoreDesc	cs:[bx]
	pop	[LDSW]
	pop	dx				; Restore original Segment bases
	pop	bx				; in our LoadAll buffer
	pop	ax
	mov	[DSDES].SEG_BASE_LO, ax
	mov	[DSDES].SEG_BASE_HI, dl
	mov	[ESDES].SEG_BASE_LO, bx
	mov	[ESDES].SEG_BASE_HI, dh
DLA_NoRestore:
	dec	[DescSaved]
	ret

;**	FixDescriptor - Shuffle GTD IDT descriptors
;*
;*	The segment descriptors for the IDT and GDT are stored
;*	by the SIDT instruction in a slightly different format
;*	than the LOADALL instruction wants them. This routine
;*	performs the transformation.
;*
;*	ENTRY:
;*	    CS:BX points to IDT or GDT descriptor in SIDT form
;*	EXIT:
;*	    CS:BX points to IDT or GDT descriptor in LOADALL form
;*	USES: AX,CX,DX
;*
FixDescriptor	proc	near
	mov	ax,cs:[bx+4]
	mov	cx,cs:[bx+2]
	mov	dx,cs:[bx]
	mov	cs:[bx+4],dx
	mov	cs:[bx],cx
	mov	cs:[bx+2],ax
	ret
FixDescriptor	endp

DoLoadAll	endp

MoveExtended286	endp

;*----------------------------------------------------------------------*
;*									*
;*  aAddMem - add memory to free pool					*
;*									*
;*	The trick here is that we're going to check for overlapping	*
;*	  or adjacent blocks and crunch them together.  The thinking	*
;*	  here is that we may be informed of a memory resource from	*
;*	  more than one source.  In any case, we NEVER want the same	*
;*	  memory to appear in our resource table more than once.	*
;*									*
;*	Note:  there's presently no way of reporting errors if the	*
;*	  handle table is full.  If it happens, we'll just lose the	*
;*	  memory block.  This should not be a problem as long as	*
;*	  we're only being called during program initialization.	*
;*									*
;*	It would be nice if we could throw this code away after		*
;*	  initialization, unfortunately this is actually invoked	*
;*	  at HookInt15 time, so it's too late to do away with		*
;*	  obsolete code.						*
;*									*
;*  ARGS:    CX - base of block in 1K increments			*
;*	     AX - length of block in 1K increments			*
;*  TRASHES: AX,BX,CX,DX,SI,DI						*
;*									*
;*  messes with handle table - not reentrant - assumes ints disabled	*
;*									*
;*----------------------------------------------------------------------*

aAddMem	proc	far

;	We might as well be scanning for a free handle while we're
;	 at it since we're normally going to need one at the end

	mov	dx,ax		; save new block length in dx
	mov	si,cx		; save new block base in si
	xor	di,di		; haven't found free handle yet

	mov	bx,[KiddValley]	; prepare to loop thru handle tab
	mov	cx,[cHandles]

aAM01:
	cmp	[bx].Flags,UNUSEDFLAG ; is this handle available?
	jnz	aAM02		; skip if not

	or	di,di		; use the first free handle we
	jnz	aAM05		;  find.  skip if we've got one

	mov	di,bx		; save the unused handle in di
	jmp	short aAM05

aAM02:


;	Note:  Normally all handles will be either UNUSED or FREE at
;	  this point.  However, in the case of checking for Zenith memory,
;	  it may have a temporarily allocated dummy block.  Therefore
;	  we'll only be merging blocks marked as FREE.

	cmp	[bx].Flags,FREEFLAG
	jnz	aAM05		; ignore USED blocks

;	   First check for new block being entirely after block at [bx]

	mov	ax,[bx].Base.lo
	add	ax,[bx].Len.lo
	cmp	ax,si		; is [bx].end < new.Base?
	jb	aAM05		;  done checking this entry if so

;	   Now check for new block being entirely before block at [bx]

	mov	ax,si		; new.base
	add	ax,dx		; + new.len = new.end
	cmp	ax,[bx].Base.lo
	jb	aAM05		; brif no overlap at all
;
;	   Now suck the block at [bx] up into our block in registers so
;	   that we can continue the scan.  There may be other adjacent
;	   blocks, even in the case of no overlap, fr'instance when a
;	   block is added which entirely fills the gap between two others.

	mov	ax, [bx].Len.lo
	add	ax, [bx].Base.lo; AX = end of this block
	add	dx, si		; DX = end of new block

	cmp	si, [bx].Base.lo; does the current block start before the new
	jbe	@f		;  no, keep the new start
	mov	si, [bx].Base.lo;  yes, update new block start
@@:
	cmp	dx, ax		; does the cur block end after the new one
	jae	@f		;  no, keep the new end
	mov	dx, ax		;  yes, update the new end
@@:
	sub	dx, si		; DX = Length of combined block
				; SI = Base of Combined block

	mov	[bx].Flags,UNUSEDFLAG ; mark the block unused
	or	di,di		; did we find an unused handle yet?
	jnz	aAM05		;  brif so
	mov	di,bx		; save this one if not

aAM05:
	add	bx,SIZE handle
	loop    aAM01

	or	di,di		; did we find a free handle?
	jz	aAM06		;  error!  no handles free!

	mov	[di].cLock,0
	mov	[di].Flags,FREEFLAG ; create the free memory block
	mov	[di].Base.lo,si
	mov	[di].Len.lo,dx

aAM06:
	ret

aAddMem	endp

End286	label	byte



