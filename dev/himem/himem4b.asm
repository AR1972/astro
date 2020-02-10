;/*
; *                      Microsoft Confidential
; *			 Copyright (C) Microsoft Corporation 1988-1992
; *                      All Rights Reserved.
; */
;
;*****************************************************************************
;
; HIMEM4B.ASM : 386 SPECIFIC routines
;
;*****************************************************************************
;
Begin386	label	byte	; Start of 386 code

;*----------------------------------------------------------------------*
;*									*
;*  QueryExtMemory -					FUNCTION 08h    *
;*									*
;*	Returns the size of the largest free extended memory block in K	*
;*									*
;*  ARGS:   None							*
;*  RETS:   AX = Size of largest free block in K.  BL = Error code	*
;*	DX = Total amount of free extended memory in K			*
;*  REGS:   AX, BX, DX and Flags clobbered				*
;*									*
;*  INTERNALLY REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

QueryExtMemory proc far

	test	ah, 80h				; Super call ?
	jnz	SQueryExtMemoryNear
	push	si
	push	di

	push	eax
	push	ecx
	push	edx

	push	cs
	call	SQueryExtMemoryNear		; call the super ONE!
;
;------ if (eax < 64M) si = ax ; else di = 64M ;
;
	mov	si, ax
	cmp	eax, 0ffffh
	jbe	short @f
	mov	si, 0ffffh
@@:
;
;------ if (edx < 64M) di = ax ; else di = 64M ;
;
	mov	di, dx
	cmp	edx, 0ffffh
	jbe	short @f
	mov	di, 0ffffh
@@:
	pop	edx
	pop	ecx
	pop	eax

	mov	ax, si
	mov	dx, di

	pop	di
	pop	si
	ret

QueryExtMemory endp


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

AllocExtMemoryNear proc near
AllocExtMemoryNear endp
AllocExtMemory proc far

	test	ah, 80h
	jnz	SAllocExtMemoryNear
	push	si
	push	edx
	movzx	edx, dx
	push	cs
	call	SAllocExtMemoryNear	; call the super ONE!
	mov	si, dx
	pop	edx
	mov	dx, si
	pop	si
	ret

AllocExtMemory endp

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

FreeExtMemoryNear proc near
FreeExtMemoryNear endp

FreeExtMemory proc far

	cli 				; This is a non-reentrant function

	push	si
	push	di
	push	cx
	push	edx

	push	eax
	push	ebx

	mov	si, offset ValidateHandle
	call	si

;	call    ValidateHandle		; Make sure handle is valid

	jnc     short FEMBadh
	mov	si,dx			; Move the handle into SI

	cmp	[si].cLock,0		; make sure it points to unlocked block
	jne     short FEMLockedh

	mov	[si].Flags,UNUSEDFLAG	;  mark it as UNUSED
	cmp	[si].Len,0		; if zero length block
	jz	short FEMExit			; done if it was zero length
	mov	[si].Flags,FREEFLAG	; mark it as FREE

;	now see if there's a free adjacent block

FEMScanIt:
	mov	ebx,[si].Base		; BX = base of block
	mov	eax,ebx			; calculate ax = top of block
	add	eax,[si].Len

;	Search for an adjacent FREE block.

	mov	di,[KiddValley]		; di = Handle being scanned
	mov     cx,[cHandles]		; Loop through the handle table
FEMLoopTop:
	cmp	[di].Flags,FREEFLAG	; Is this block free?
	jne     short FEMNexth		; No, continue

	mov	edx,[di].Base		; is this block just above freed one?
	cmp     edx,eax
	je	short FEMBlockAbove     	; Yes, coalesce upwards

	add	edx,[di].Len		; is it just below?
	cmp     edx,ebx
	je	short FEMBlockBelow     	; Yes, coalesce downwards

FEMNexth:
	add	di,SIZE Handle
	loop    FEMLoopTop

;	No adjacent blocks to coalesce.

FEMExit:
	pop	ebx
	pop	eax

	mov     ax,1			; Return success
	xor	bl,bl
FEM9:
	pop	edx
	pop	cx
	pop	di
	pop	si

	ret

;	Exchange the pointer to the "upper" and "lower" blocks.

FEMBlockBelow:
	xchg    si,di

;	Move the free block above into the current handle.

FEMBlockAbove:
	mov	edx,[si].Len
	add     edx,[di].Len 		; Combine the lengths
	mov	[si].Len,edx
	mov     [di].Flags,UNUSEDFLAG   ; Mark old block's handle as UNUSED
	jmp	short FEMScanIt		; Rescan the list

FEMBadh:
	pop	ebx
	mov	bl,ERR_INVALIDHANDLE
	jmp	short FEMErrExit

FEMLockedh:
	pop	ebx
	mov	bl,ERR_EMBLOCKED
FEMErrExit:
	pop	eax
	xor     ax,ax			; Return failure
	jmp	short FEM9

FreeExtMemory endp

;*----------------------------------------------------------------------*
;*									*
;*  GetExtMemoryInfo -					FUNCTION 0Eh    *
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

GetExtMemoryInfo proc	far

	test	ah, 80h
	jnz	SGetExtMemoryInfoNear

	cli 			; This is a non-reentrant function

	push	cx
	push	si

	mov	si, offset ValidateHandle
	call	si

;	call    ValidateHandle	; is the handle valid?

	jnc     short GEMBadh
	mov     si,dx		; Move the handle into SI

	cmp	[si].Len.hi, 0	; Size > 64M ?
	jne	short GEMBadh		; yes, we cannot handle this function

	xor     ax,ax		; count number of UNUSED handles
	mov     bx,[KiddValley]
	mov     cx,[cHandles]	; Loop through the handle table
GEMLoop:
	cmp     [bx].Flags,USEDFLAG ; Is this handle in use?
	je	short GEMNexth	; Yes, continue
	inc     ax		; No, increment the count
GEMNexth:
	add     bx,SIZE Handle
	loop    GEMLoop

	mov     dx,[si].Len.lo 	; Length in DX
	mov     bh,[si].cLock	; Lock count in BH
	or	ah, ah		; Free handles > 255 ?
	jz	short @f
	mov	al, 0ffh	; make it 255 if > 255
@@:
	pop	si
	pop	cx
	mov     bl,al
	mov	ax,1
	ret

GEMBadh:
	pop	si
	pop	cx
	mov	bl,ERR_INVALIDHANDLE
	xor	ax,ax
	ret

GetExtMemoryInfo endp


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

ReallocExtMemory	proc	far
	test	ah, 80h
	jnz	SReallocExtMemoryNear
	push	si
	push	di
	push	eax
	push	ebx
	movzx	ebx, bx
	push	cs
	call	SReallocExtMemoryNear
	mov	si, ax
	mov	di, bx
	pop	ebx
	mov	bx, di
	pop	eax
	mov	ax, si
	pop	di
	pop	si
	ret
ReallocExtMemory	endp

;*----------------------------------------------------------------------*
;*									*
;*  SQueryExtMemory -					FUNCTION 88h    *
;*									*
;*	Returns the size of the largest free extended memory block in K	*
;*									*
;*  ARGS:   None							*
;*  RETS:   EAX = Size of largest free block in K.  BL = Error code	*
;*	    EDX = Total amount of free extended memory in K		*
;*	    ECX = Highest Ending Address of any block			*
;*  REGS:   AX, BX, DX, DI, SI and Flags clobbered			*
;*									*
;*  INTERNALLY REENTRANT						*
;*									*
;*----------------------------------------------------------------------*

SQueryExtMemoryNear proc near
SQueryExtMemoryNear endp

SQueryExtMemory proc far

	push	esi
	push	edi

;	scan for largest FREE block

	xor	eax, eax	       	; eax = Max. size found so far
	xor	edx, edx		; edx = Total amount of free memory
	xor	esi, esi		; esi = Highest Address so far

	mov	bx,[KiddValley]
	mov     cx,[cHandles]		; Loop through the handle table
SQEMLoop:
	cmp	[bx].Flags, UNUSEDFLAG
	je	short SQEMBottom

	mov	edi, [bx].Base
	add	edi, [bx].Len
	dec	edi			; edi=Addr of last byte of the block
	cmp	esi, edi		; Have we found a bigger end address
	jae	short @f
	mov	esi, edi
@@:
	cmp	[bx].Flags,FREEFLAG
	jne     short SQEMBottom

	mov	edi, [bx].Len
	add     edx, edi 		; add free block to total

	cmp	eax, edi		; is this the largest so far?
	jae     short SQEMBottom

	mov	eax, edi		; Yes, save it away
SQEMBottom:
	add	bx,SIZE Handle
	loop    SQEMLoop

	mov	ecx, esi		; get the address of last byte into ecx

	mov	bl, ERR_OUTOMEMORY	; assume error
	or	edx, edx		; do we have any free memory
	jz	short @f			; if not keep error code in BL
	xor	bl, bl			; else clear the error
@@:
	pop	edi
	pop	esi
	ret

SQueryExtMemory endp


;*----------------------------------------------------------------------*
;*									*
;*  SAllocExtMemory -					FUNCTION 09h    *
;*									*
;*	Reserve a block of extended memory				*
;*									*
;*  ARGS:   EDX = Amount of K being requested				*
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

SAllocExtMemoryNear proc near
SAllocExtMemoryNear endp

SAllocExtMemory proc far
 
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

SAEMhLoop:
	or	si, si			; did we already find a Free Handle ?
	jne     short SAEMUnused		; Yes, see if this one is unused

	cmp     [bx].Flags, FREEFLAG	; Is this block free?
	jne     short SAEMUnused		; No, see if it is unused

	cmp	edx, [bx].Len		; Is it large enough?
	ja	short SAEMNexth		; No, get the next handle

	mov	si, bx			; save this one away
	jmp	short SAEMBottom

SAEMUnused:
	or	di, di			; did we already find an unused handle?
	jne     short SAEMNexth		; Yes, get the next handle

	cmp     [bx].Flags, UNUSEDFLAG	; Is this block unused?
	jne     short SAEMNexth		; No, get the next handle

	mov	di, bx			; save this guy away

	cmp	si, 0			; have we found all we need?
	je	short SAEMNexth
SAEMBottom:
	cmp	di, 0
	jne     short SAEMGotBoth		; Yes, continue

SAEMNexth:
	add	bx, SIZE Handle		; go check the next handle
	loop    SAEMhLoop

;	We are at the end of the handle table and we didn't find both
;	things we were looking for.  Did we find a free block?

	or	si, si
	jnz     short SAEMRetSuc		; Yes, Case 3 - Alloc the entire block

	or	di, di			; did we find an unused handle?
	jz	short SAEMOOHandles		; No, Case 4 - We're out of handles

	or	edx, edx		; Case 2 - req for zero-length handle?
	jnz     short SAEMOOMemory		; No, we're out of memory

	mov	si, di			; reserve the zero-length handle
	mov	[si].Len, 0		; force length field to zero
	jmp	short SAEMRetSuc

SAEMGotBoth:

;	 We're at Case 1 above.
;	Mark the free block as used (done below) and adjust its size.
;	Make the unused handle a free block containing the remaining
;	   unallocated portion of the original free block.

	push	eax

	mov	eax, [si].Base		; Unused.Base = Old.Base + request
	add	eax, edx
	mov	[di].Base, eax

	mov	eax, edx		; New.Len = request
	xchg    [si].Len, eax

	sub	eax, edx		; Unused.Len = Old.Len - request
	mov	[di].Len, eax
	mov	[di].Flags, FREEFLAG	; Unused.Flags = FREE
	jnz	short SAEM_nonzero_residual
	mov	[di].Flags, UNUSEDFLAG	; Unused.Flags = UNUSED

SAEM_nonzero_residual:

	pop	eax
SAEMRetSuc:
	mov	[si].Flags, USEDFLAG	; New.Flags = USED

	if	keep_cs
	mov	ax, callers_cs
	mov	[si].Acs, ax		; keep track of allocator's cs:
	endif

	mov	dx, si
	mov	ax, 1
	xor	bl, bl
SAEM9:
	pop	di
	pop	si
	pop	cx
	ret

SAEMOOMemory:
	mov	bl, ERR_OUTOMEMORY
	jmp	short SAEMErrRet

SAEMOOHandles:
	mov	bl, ERR_OUTOHANDLES
SAEMErrRet:
	xor	ax, ax			; Return failure
	mov	dx, ax
	jmp	short SAEM9		;
SAllocExtMemory endp

;*----------------------------------------------------------------------*
;*									*
;*  GetExtMemoryInfo -					FUNCTION 0Eh    *
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

SGetExtMemoryInfoNear	proc	near
SGetExtMemoryInfoNear	endp

SGetExtMemoryInfo proc	far

	cli 			; This is a non-reentrant function

	push	cx
	push	si

	mov	si, offset ValidateHandle
	call	si

;	call    ValidateHandle	; is the handle valid?

	jnc     short SGEMBadh
	mov     si,dx		; Move the handle into SI

	xor     ax,ax		; count number of not USED handles
	mov     bx,[KiddValley]
	mov     cx,[cHandles]	; Loop through the handle table
SGEMLoop:
	cmp     [bx].Flags,USEDFLAG ; Is this handle in use?
	je	short SGEMNexth	; Yes, continue
	inc     ax		; No, increment the count
SGEMNexth:
	add     bx,SIZE Handle
	loop    SGEMLoop

	mov     edx,[si].Len 	; Length in EDX
	mov     bh,[si].cLock	; Lock count in BH
	xchg	cx, ax		; number of free handles in CX
	pop	si
	pop	cx
	mov	ax,1
	xor	bl, bl
	ret

SGEMBadh:
	pop	si
	pop	cx
	mov	bl,ERR_INVALIDHANDLE
	xor	ax,ax
	ret

SGetExtMemoryInfo endp


;*----------------------------------------------------------------------*
;*									*
;*  SReallocExtMemory -					FUNCTION 8Fh    *
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

SReallocExtMemoryNear	proc	near
SReallocExtMemoryNear	endp

SReallocExtMemory	proc	far

	cli 			; This is a non-reentrant function

	push    bp		; preserve caller's bp

	push	edx
	push	si
	push	di


	mov	si, offset ValidateHandle
	call	si

;	call    ValidateHandle	; is the handle valid?
	mov	si,dx		; Move the handle into SI
	mov	edx,ebx		; Move the new length into dx
	mov	bl,ERR_INVALIDHANDLE
	jnc     short REMError

	cmp	[si].cLock,0	; We can only work on unlocked EMBs
	mov	bl,ERR_EMBLOCKED
	jnz	short REMError

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
	cmp	edx,[si].Len		; check for cases 1 and 2
	ja	REMGrow			; cases 3, 4, 5 - growing
	jb	short REMShrink		; case 2 - shrinking

;	Case 1:  no change in size.  We're done.

REMExit:
	mov	ax,1			; succesful return
	xor	bl,bl			; non-documented no-error return
	pop	di
	pop	si
	pop	edx
	pop	bp
	ret

;	Case 2: Truncate the block and coalesce
;	   Scan the list of handles to see if there is an
;	   existing EMB immediately following us that we can
;	   append the memory to.

REMShrink:

	push    edx			; save new size
	call    FindAdjacent		; see if a block follows us
	pop	edx			; restore our new size
	or	bp,bp			; does a block follow us?
	jz	short REMShrink5		;  brif not

;	This is the easy case of shrinking.  All we have to do is move
;	   the deallocated memory into the following block

	mov	eax,[si].Len		; old length
	sub	eax,edx			; we're truncating this much
	mov	[si].Len,edx		; adjust our block downwards

	add	ds:[bp].Len,eax		; increase next block by same
	sub	ds:[bp].Base,eax	;  and lower its base

	jmp	short REMExit		; exit okay!

;	Locate an error return here for easy access

REMError:
	xor	ax,ax
	pop	di
	pop	si
	pop	edx
	pop	bp			; preserve caller's bp
	ret

;	We'll have to create a new block to give the freed space to.

REMShrink5:
	push	cx
	mov	di,[KiddValley]		; look for an available handle
	mov	cx,[cHandles]
REMS5a:
	cmp	[di].Flags,UNUSEDFLAG
	jz	short REMS5b			; brif found free handle
	add	di,SIZE Handle
	loop    REMS5a
	pop	cx

	mov	bl,ERR_OUTOHANDLES
	jmp	short REMError		; Abort without shrinking if none

REMS5b:
	pop	cx
	mov	eax,edx			; get new length
	add	eax,[si].Base		;  calculate begin of new block
	mov	[di].Base,eax

	mov	eax,[si].Len		; new length
	sub	eax,edx			;  calculate length of new block
	mov	[di].Len,eax

	mov	[di].Flags,FREEFLAG     ; setup lock count and flags
	mov	[di].cLock,0

	mov	[si].Len,edx		; adjust length of original block

	jmp	short REMExit		; exit okay!


REMGrow:
	push    edx			; save new length
	call    FindAdjacent		; locate adjacent blocks
	pop	edx

;	hopefully the following block is big enough.  that's the easiest
;	   case.  If so, we'll just snatch the space and then kill it if
;	   its size goes to zero.

;	Don't forget:  di = preceding block's handle (or zero)
;		   si = original block's handle
;		   bp = following block's handle (or zero)

	or	bp,bp			; is there a following block?
	jz	short REMGrow4		; skip if not
	mov	eax,edx			; new size
	sub	eax,[si].Len		;  calculate growth amount
	cmp	eax,ds:[bp].Len		; will next block accomodate?
	ja	short REMGrow4		;  nope.  not big enough.

;	Case 3: The following block has enough space for our needs.

	jz	short REMGrow2		; skip if we're using whole block

	sub	ds:[bp].Len,eax		; make it smaller
	add	ds:[bp].Base,eax		;  and start later
	jmp	short REMGrow3

REMGrow2:
	mov	ds:[bp].Flags,UNUSEDFLAG; mark it as free
REMGrow3:
	mov	[si].Len,edx		; update length of caller's block
	jmp	REMExit

;	Come here if we can't find enough room from a following block.
;	  Now decide whether to use case 4 (use prev or both adjacent blocks)
;	  or case 5 (find another block entirely).  Notice that either
;	  of these solutions will require a memory move operation.

REMGrow4:
	or	di,di			; is there a preceding block?
	jz	short REMGrow7x		;  nope.  resort to case 5

	mov	eax,[di].Len
	add	eax,[si].Len		; get size of prev + orig block
	or	bp,bp			; Is there a following block?
	jz	short REMGrow4a		;  skip if not
	add	eax,ds:[bp].Len		; calculate max possible resource
REMGrow4a:
	cmp	edx,eax			; is all of that sufficient
	jna	short REMGrow5		;  to handle whole thing? brif so
REMGrow7x:
	jmp	REMGrow7

;	There will be only one or two blocks left of the possible three
;	 when we're finished.  We'll start by merging the following block
;	 onto the middle block and freeing its handle, but we'll save the
;	 original length of the middle block so we don't end up moving more
;	 memory than we need.

REMGrow5:

;	Now we need a move structure.  Allocate one on the stack and point
;	   es:bx to it

;	Always make sure stack stays on even boundary

	sub	sp,0fffeh and (1+SIZE MoveExtendedStruc)
	push    ss			; point es:bx to stack frame
	pop	es
	mov	bx,sp

	push    [si].Len		; save length of move
	or	bp,bp			;  is there a following block?
	jz	short REMGrow5a		;  brif not

	mov	eax,ds:[bp].Len		; get it's length
	add	[si].Len,eax		; suck it in
	mov	ds:[bp].Flags,UNUSEDFLAG; that handle is now unused

;	Now change the main block to include all of the space.  Notice
;	  that the 'previous' block will still exist as an overlapping
;	  subset of the total block during the move.  We'll sort it out
;	  again after the move when we're able to disable interrupts
;	  again.

REMGrow5a:
	mov	eax,[di].Len		; get length of bottom block
	add	[si].Len,eax		; add it into our target block
	push    [si].Base		; save base of data block to move
	mov	eax,[di].Base		; base of bottom block becomes
	mov	[si].Base,eax		;  of super block
	inc	[di].cLock		; lock both blocks so's we don't
	inc	[si].cLock		;  get moved around

;	Now make the move in the new merged block.  Offsets are relative
;	  to the base of the block.  Zero is the destination.  The old
;	  base ([sp]) is the source (in 1k), the old length ([sp+2]) is
;	  the length (in 1k).

	mov	es:[bx].SourceHandle,si	; both handles = our main block
	mov	es:[bx].DestHandle,si
	mov	es:[bx].DestOffset,0	; move data to base of block

	pop	eax			; get source begin address
	sub	eax,[si].Base		; find offset within block

;	now use trick to make cx:ax = ax * 1024

;	rcr	ax,6			; like rcl ax,10 but faster
;	mov	cx,ax
;	and	cx,3ffh
;	and	ax,0fc00h

	shl	eax, 10

	mov	es:[bx].SourceOffset,eax

	pop	eax			; get length in Kilobytes

;	rcr	ax,6			; trick multiply by 1024 again
;	mov	cx,ax
;	and	cx,3ffh
;	and	ax,0fc00h

	shl	eax, 10

	mov	es:[bx].bCount,eax

	push    si			; save main handle
	push    di			; save residual handle
	push    edx			; save new block length
	mov	si,bx
;
	push	cs

	mov	bx, offset BlkMovX
	call	bx

;	call	BlkMovX

	pop	edx
	pop	di
	pop	si

;	Release the move structure stack frame

	add	sp,0fffeh and (1+SIZE MoveExtendedStruc)

	cli				; can't mess with us
	dec	[di].cLock		; remove our locks
	dec	[si].cLock
	mov	eax,[si].Len		; get total block length
	sub	eax,edx			;  less amount requested by user
	mov	[si].Len,edx
	jz	short REMGrow5b		; branch if no residual block

	add	edx,[si].Base		; get base of residual block
	mov	[di].Base,edx
	mov	[di].Len,eax		; save length of residual block
	jmp	REMExit			; We're done!

REMGrow5b:
	mov	[di].Flags,UNUSEDFLAG   ; free the handle if block empty
	jmp	REMExit			; DONE!

;	Another error return centrally for easy conditional branch access

REMErrora:
	xor	ax,ax			; indicate error condition
	pop	di
	pop	si
	pop	edx
	pop	bp			; restore caller's bp
	ret

REMGrow7:
	push    si			; save our handle
	push    dx			; save our desired length
	push	cs			; fake a far call
	call	SAllocExtMemoryNear	; try to allocate a new block
	cli
	mov	di,dx			;  save its results
	pop	dx
	pop	si
	or	ax,ax			; did we fail?
	mov	bl,ERR_OUTOMEMORY	;  not enough memory
	jz	REMErrora
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
	xor	eax,eax
	mov	es:[bx].DestOffset,eax	; move to base of new block
	mov	es:[bx].SourceOffset,eax; from base of old block

	mov	eax,[si].Len		; get length of old block

;	rcr	ax,6			; trick multiply by 1024
;	mov	cx,ax
;	and	cx,3ffh
;	and	ax,0fc00h

	shl	eax, 10

	mov	es:[bx].bCount,eax

	push    si			; save main handle
	push    di			; save new handle
	mov	si,bx
	push	cs

	mov	bx, offset BlkMovX
	call	bx

;	call	BlkMovX

	pop	di
	pop	si

;	Deallocate stack frame

	add	sp,0fffeh and (1+SIZE MoveExtendedStruc)

	cli				; can't mess with us

;	Now we have to reverse the two handles so that we can return
;	  the new block to the user without changing his handle

	mov	eax,[si].Base
	xchg    eax,[di].Base
	mov	[si].Base,eax

	mov	eax,[si].Len
	xchg    eax,[di].Len
	mov	[si].Len,eax

	dec	[si].cLock		; remove our locks
	dec	[di].cLock

	mov	ax,di			; handle to free
	pop	di
	pop	si
	pop	edx
	mov	dx, ax			; handle to free
	pop	bp
	jmp	FreeExtMemoryNear	; do it!, return any errors

SReallocExtMemory	endp

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

FindAdjacent	proc	near

	mov	eax,[si].Base		; look for blocks ending here
	mov	edx,[si].Len
	add	edx,eax			; and ending here

	xor	di,di			; initialize to fail condition
	mov	bp,di

	mov	bx,[KiddValley]		; prepare to loop thru handle tab
	mov	cx,[cHandles]

	push    esi			; preserve original handle

FindAdj1:
	cmp	[bx].Flags,FREEFLAG
	jnz	short FindAdj3		; ignore blocks that aren't UNUSED
	mov	esi,[bx].Base
	cmp	edx,esi			; found beg block?
	jnz	short FindAdj2		;  skip if not
	mov	bp,bx			;  remember the handle
	or	di,di			; did we already find a follower?
	jnz	short FindAdj9		;  we're done if so

FindAdj2:
	add	esi,[bx].Len		; find length
	cmp	esi,eax			; does this block end at spec addr?
	jnz	short FindAdj3		;  skip if not
	mov	di,bx			;  remember the handle
	or	bp,bp			; did we already find a leader?
	jnz	short FindAdj9		;  we're done if so

FindAdj3:
	add	bx,SIZE handle
	loop    FindAdj1

FindAdj9:
	pop	esi			; restore original handle
	ret
;

FindAdjacent	endp

;*----------------------------------------------------------------------*

Zero segment at 0
	org	13*4
	public	Int13Vector
Int13Vector	label	dword
Zero	ends


	public	Patch3, descCS, OurGDT, GDTPtr
	public	MoveExtended386

MoveBlock386:


Here3	equ	($+(offset Begin286-offset Begin386))

OurGDT	=	byte ptr Here3		; Simple GDT
		DESC386	<>
descCS	=	byte ptr Here3
		DESC386	<0FFFFh,0,0,09Fh,0,0>		; Conforming CS
descRealBig	=	byte ptr Here3
		DESC386	<0FFFFh,0,0,093h,0cfh,0>	; Page Granularity
							; 4Gb Limit
GDTLen	EQU Here3-OurGDT

GDTPtr	=	qword ptr Here3
		GDT386 <GDTLen,0,0>

OldInt13	=	dword ptr Here3
		dd	0			; Old contents of int 13 vector

GDTMoveBlock	= byte ptr Here3
						; 386 Template OK for Move Block
		DESC386	<>			; Nul Descriptor
		DESC386	<>			; GDT Descriptor
descSource	= byte ptr Here3
		DESC386 <0FFFFh,0,0,93h,0,0>	; Source Segment Descriptor
descDest	= byte ptr Here3
		DESC386 <0FFFFh,0,0,93h,0,0>	; Destination Segment Descriptor
		DESC386	<>			; BIOS use
		DESC386	<>			; BIOS use

;******************************************************************************
;
; Movext386
;	XMM Move Extended Memory Block for the 80386
;
; Entry:
;	ES:BX	Points to structure containing:
;		bCount		dd	?	; Length of block to move
;		SourceHandle	dw	?	; Handle for souce
;		SourceOffset	dd	?	; Offset into source
;		DestHandle	dw	?	; Handle for destination
;		DestOffset	dd	?	; Offset into destination
;
; Return:
;	AX = 1	Success
;	AX = 0	Failure
;		Error code in BL
;
; Registers Destroyed:
;	Flags
;
;------------------------------------------------------------------------------

	public	MoveExtended386
MoveExtended386 = Here3

Movext386	proc	far
	assume	ds:_text


	sti					; Be nice
	push	bp				; Set up stack frame so we
	mov	bp, sp				; can have local variables
	sub	sp, 18
Count	= -4					; Local DWORD for byte count
Return	= -6					; Local WORD for return code
SrcHandle = -8
DstHandle = -10
SrcLinear = -14
DstLinear = -18
	push	eax				; Save upper word of registers
	push	ecx
	push	esi
	push	edi
	push	bx

	xor	ax, ax
	mov	[bp.Return], ax			; Assume success
	mov	[bp.SrcHandle], ax
	mov	[bp.DstHandle], ax
	mov	ecx, es:[si].bCount
	mov	[bp.Count], ecx

	shr	dword ptr [bp.Count], 1		; No odd byte counts
	jc	MEM3_InvCount
	jz	MEM3_Exit			; Exit immediately if zero

	lea	bx, [si].SourceHandle		; Normalize Source
	call	GetLinear386			; Linear address in edi
	jc	MEM3_SrcError			; Have Dest Error Code
	xchg	esi, edi			; Save source address in ESI
	mov	[bp.SrcHandle], bx		; Save Handle for Unlock

	lea	bx, [di].DestHandle
	call	GetLinear386			; Normalize Destination
	jc	MEM3_Error
	mov	[bp.DstHandle], bx		; Save Handle for Unlock

	smsw	ax
	shr	ax, 1				; Protected mode?
	jc	MEM3_MoveBlock			;   if so, use int 15h
						; Must preserve DS
	push	si
	push	di
	call	LEnblA20
	pop	di
	pop	si

	cmp	ax, 1
	jne	MEM3_Error

	xor	cx, cx
	mov	es, cx
	assume	es:Zero
	mov	ax, cs
	shl	eax, 16
	mov	ax, offset Int13Handx
	cli
	push	[OldInt13]			; For reentrancy
	xchg	eax, [Int13Vector]		; Install our int 13 handler
	mov	[OldInt13], eax
	sti

	push	ds				; save _text segment
	mov	ds, cx
	assume	ds:Zero
	mov	ecx, [bp.Count]
	shr	ecx, 1				; Now DWORD count
						; Odd word count in carry
	cld

; Now we have:
;	ESI = 32 bit Source Linear Address
;	EDI = 32 bit Destination Linear Address
;	DS = ES = 0
;
;	 If the limit of DS or ES is still the Real Mode
;	 default of 64k and ESI or EDI is greater than 64k,
;	 these instructions will fault with an int 13.
;	 In this case, our int 13 handler will set up
;	 the descriptors to have 4Gb limits (real big mode)
;	 and will iret to the faulting instruction.

;	 The following persuades masm to output
;	 both a 66h and 67h prefix

Fault0	=	Here3

	rep movs dword ptr [esi], dword ptr [edi]	; DWORDS first
			; THE NEXT INSTRUCTION MUST HAVE ADDRESS SIZE OVERRIDE
	db	67h		; BUG AVOIDANCE - DO NOT REMOVE
	nop			; BUG AVOIDANCE - DO NOT REMOVE

	rcl	ecx, 1
Fault1	=	Here3

	rep movs word ptr [esi], word ptr [edi]		; Now the odd word
			; THE NEXT INSTRUCTION MUST HAVE ADDRESS SIZE OVERRIDE
	db	67h		; BUG AVOIDANCE - DO NOT REMOVE
	nop			; BUG AVOIDANCE - DO NOT REMOVE

	pop	ds
	assume	ds:_text
	pop	eax				; saved [OldInt13]
	cli					; NECESSARY
	xchg	eax, [OldInt13]			; OldInt13 potentially INVALID
	mov	[Int13Vector], eax		; Deinstall our handler
	sti

	call	LDsblA20
	cmp	ax, 1
	jne	short MEM3_Error

MEM3_Exit:
	mov	bx, [bp.SrcHandle]		; Unlock Handles if necessary
	or	bx, bx
	jz	short MEM3_NoSrcHandle
	dec	[bx].cLock			; Unlock Source
MEM3_NoSrcHandle:
	mov	bx, [bp.DstHandle]
	or	bx, bx
	jz	short MEM3_NoDstHandle
	dec	[bx].cLock			; Unlock Destination
MEM3_NoDstHandle:
	pop	bx				; Restore original registers
	pop	edi
	pop	esi
	pop	ecx
	pop	eax
	mov	ax, 1
	cmp	word ptr [bp.Return], 0
	je	short MEM3_Success
	dec	ax				; AX = 0 for error
	mov	bl, byte ptr [bp.Return]
MEM3_Success:
	mov	sp, bp				; Unwind stack
	pop	bp
	ret

MEM3_InvCount:
	mov	bl, ERR_LENINVALID
	jmp	short MEM3_Error
MEM3_SrcError:
	cmp	bl, ERR_LENINVALID		; Invalid count
	je	short MEM3_Error		;   yes, no fiddle
	sub	bl, 2				; Convert to Source error code
MEM3_Error:
	mov	[bp.Return], bl
	jmp	short MEM3_Exit

;******************************************************************************
;
; GetLinear386
;	Convert Handle and Offset (or 0 and SEG:OFFSET) into Linear address
;	Locks Handle if necessary
;	Nested with Movext386 to access local variables
;
; Entry:
;	ES:BX	Points to structure containing:
;		Handle	dw
;		Offset	dd
;	ECX	Count of bytes to move
;
; Return:
;	BX	Handle of block (0 if conventional)
;	EDI	Linear address
;	CARRY	=> Error
;
; Registers Destroyed:
;	EAX
;
;------------------------------------------------------------------------------

GetLinear386	proc	near
	cli					; NO INTERRUPTS
	mov	edi, dword ptr es:[bx+2]	; Offset from start of handle
	mov	bx, word ptr es:[bx]		; Handle in bx
	or	bx, bx
	jz	short GL3_Conventional

	cmp	[bx].Flags, USEDFLAG		; Valid Handle?
	jne	short GL3_InvHandle

	mov	eax, [bx].Len			; Length of Block
	shl	eax, 10				; now in bytes
	sub	eax, edi			; EAX = max possible count
	jb	short GL3_InvOffset		; Base past end of block
	cmp	eax, ecx
	jb	short GL3_InvCount		; Count too big

	inc	[bx].cLock			; Lock Handle
	mov	eax, [bx].Base
	shl	eax, 10				; Base byte address
	add	edi, eax			; Linear address

GL3_OKExit:
	clc
	sti
	ret

GL3_Conventional:
	movzx	eax, di				; Offset in EAX
	shr	edi, 16
	shl	edi, 4				; Segment*16 in EDI
	add	edi, eax			; Linear address in EDI
	mov	eax, edi
	add	eax, ecx
	cmp	eax, 10FFF0h			; Max addressable inc. HMA
	jbe	GL3_OKExit
GL3_InvCount:
	mov	bl, ERR_LENINVALID
	jmp	short GL3_Error
GL3_InvHandle:
	mov	bl, ERR_DHINVALID		; Dest handle invalid
	jmp	short GL3_Error
GL3_InvOffset:
	mov	bl, ERR_DOINVALID		; Dest Offset invalid
GL3_Error:
	stc
	sti
	ret

GetLinear386	endp

;******************************************************************************
;
; Int13Handler
;	Handler for int 13 during our rep moves
;	If it is a real interrupt, jump to the old handler
;	If it is a fault, set Real Big Mode and return
;
; Entry:
;
; Return:
;
; Registers Destroyed:
;	BX, DS, ES if fault from one of our instructions, otherwise
;	NONE
;
;------------------------------------------------------------------------------

Int13Handx	=	Here3
Int13Handler	proc	far
	assume	ds:nothing, es:nothing
	push	bp
	mov	bp, sp			; Base to look at faulting address
	push	ax

	mov	al, 0Bh			; Party on PIC to see if interrupt
	out	20h, al
	jmp	$+2			; M001
	in	al, 20h			; ISR
	test	al, 20h			; IRQ5, int 13
	jnz	short NotOurInt13

	mov	ax, cs
	cmp	[bp+4], ax		; Fault from our cs?
	jne	short NotOurInt13	;   no, SOMETHING IS FUNNY!
	cmp	word ptr [bp+2], offset Fault0
	je	short LoadDescriptorCache
	cmp	word ptr [bp+2], offset Fault1
	jne	short NotOurInt13	; Not one of our instructions ????

LoadDescriptorCache:
	mov	bx, descRealBig - OurGDT ; Special 4Gb selector
	lgdt	fword ptr cs:[GDTPtr]

	mov	eax, cr0
	or	al,1
	mov	cr0, eax		; Go into Protected Mode
					; NOTE: NMIs will kill us!!!

	db	0eah			; jmp far flush_prot
	dw	offset flush_prot	; Clears the prefetch
	dw	descCS - OurGDT

flush_prot	=	Here3
	mov	es, bx			; Set up the segments we want
	mov	ds, bx

	and	al, 0FEh
	mov	cr0, eax		; Return to Real Mode

	db	0EAH			; jmp far flush_real
	dw	offset flush_real
patch3	= word ptr Here3
	dw	0

flush_real	=	Here3

	xor	ax, ax
	mov	ds, ax
	mov	es, ax

	pop	ax
	pop	bp
	iret				; Back to faulting instruction

NotOurInt13:
	pop	ax
	pop	bp
	jmp	cs:[OldInt13]

Int13Handler	endp

;******************************************************************************
;
; MEM3_MoveBlock
;	Set up GDT and call int 15h Move Block
;	Nested within Movext386
;	See 80286 programmer's reference manual for GDT entry format
;	See Int 15h documentation for Move Block function
;
; Entry:
;	[BP.Count]	Word count for move
;	ESI		Linear address of the source
;	EDI		Linear address of the destination
;
;	Interrupts are ON
;
; Return:
;	CARRY	=> Error
;		Error code in BL
;
; Registers Destroyed:
;	Flags, EAX, ECX, ESI, EDI, ES
;
;------------------------------------------------------------------------------
MEM3_MoveBlock:
	mov	[bp.SrcLinear], esi
	mov	[bp.DstLinear], edi

	push	cs			; set es -> funky segment
	pop	es


DMB_loop:
	mov	ecx, 512			; Do max of # words left or
	cmp	ecx, [bp.Count]			; or max Move Block allows
	jbe	short DMB0
	mov	ecx, [bp.Count]
DMB0:
	push	ecx
	lea	si, [GDTMoveBlock]		; Pointer to GDT for Block Move

	lea	di, [descSource.LO_apDesc386]	; Source Descriptor
	mov	eax, dword ptr [bp.SrcLinear]

	CLI					; No interrupts until int 15h
						; Allows reentrancy
	cld

	stosw
	shr	eax, 16
	stosb
	mov	byte ptr es:[di+2],ah		; Source Descriptor done

	lea	di, [descDest.LO_apDesc386]	; Destination Descriptor
	mov	eax, dword ptr [bp.DstLinear]
	stosw
	shr	eax, 16
	stosb
	mov	byte ptr es:[di+2],ah		; Destination Descriptor done

	clc					; MUST DO THIS, int 15h doesn't
	mov	ah, 87h				; Block Move - Assumes protect
	int	15h				; mode code allows interrupts

	sti
	pop	ecx
	jc	short DMB_Error

	sub	[bp.Count], ecx
	jz	MEM3_Exit			; All done
	shl	ecx, 1				; Back to byte count
	add	[bp.SrcLinear], ecx		; Update source for next chunk
	add	[bp.DstLinear], ecx		; Update destination
	jmp	short DMB_loop

DMB_Error:
	xor	bh, bh
	mov	bl, ah			; ah contains errorcode ;M003
	mov	bl, cs:[Int15Err][bx]		; Pick up correct error code
	jmp	MEM3_Error

Int15Err	=	byte ptr Here3
		db	0, ERR_PARITY, ERR_LENINVALID, ERR_A20

Movext386	endp
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
;*  ARGS:    BX:CX - base of block in 1K increments			*
;*	     DX:AX - length of block in 1K increments			*
;*  TRASHES: AX,BX,CX,DX,SI,DI						*
;*									*
;*  messes with handle table - not reentrant - assumes ints disabled	*
;*									*
;*----------------------------------------------------------------------*

AddMem	proc	far

	push	eax
	push	edx
	push	esi

;	We might as well be scanning for a free handle while we're
;	 at it since we're normally going to need one at the end

	mov	si, bx
	shl	esi, 16
	mov	si, cx		; ESI = Base of block to be added

	shl	edx, 16
	mov	dx, ax		; EDX = Len of block to be added

	xor	di,di		; haven't found free handle yet

	mov	bx,[KiddValley]	; prepare to loop thru handle tab
	mov	cx,[cHandles]

AM01:
	cmp	[bx].Flags,UNUSEDFLAG ; is this handle available?
	jnz	short AM02		; skip if not

	or	di,di		; use the first free handle we
	jnz	short AM05		;  find.  skip if we've got one

	mov	di,bx		; save the unused handle in di
	jmp	short AM05

AM02:


;	Note:  Normally all handles will be either UNUSED or FREE at
;	  this point.  However, in the case of checking for Zenith memory,
;	  it may have a temporarily allocated dummy block.  Therefore
;	  we'll only be merging blocks marked as FREE.

	cmp	[bx].Flags,FREEFLAG
	jnz	short AM05		; ignore USED blocks

;	   First check for new block being entirely after block at [bx]

	mov	eax,[bx].Base
	add	eax,[bx].Len
	cmp	eax,esi		; is [bx].end < new.Base?
	jb	short AM05	;  done checking this entry if so

;	   Now check for new block being entirely before block at [bx]

	mov	eax,esi		; new.base
	add	eax,edx		; + new.len = new.end
	cmp	eax,[bx].Base
	jb	short AM05		; brif no overlap at all
;
;	   Now suck the block at [bx] up into our block in registers so
;	   that we can continue the scan.  There may be other adjacent
;	   blocks, even in the case of no overlap, fr'instance when a
;	   block is added which entirely fills the gap between two others.

	mov	eax, [bx].Len
	add	eax, [bx].Base	; EAX = end of this block
	add	edx, esi	; EDX = end of new block

	cmp	esi, [bx].Base	; does the current block start before the new
	jbe	short @f	;  no, keep the new start
	mov	esi, [bx].Base	;  yes, update new block start
@@:
	cmp	edx, eax	; does the cur block end after the new one
	jae	short @f	;  no, keep the new end
	mov	edx, eax	;  yes, update the new end
@@:
	sub	edx, esi	; DX = Length of combined block
				; SI = Base of Combined block

	mov	[bx].Flags,UNUSEDFLAG ; mark the block unused
	or	di,di		; did we find an unused handle yet?
	jnz	short AM05	;  brif so
	mov	di,bx		; save this one if not

AM05:
	add	bx,SIZE handle
	loop    AM01

	or	di,di		; did we find a free handle?
	jz	short AM06	;  error!  no handles free!

	mov	[di].cLock,0
	mov	[di].Flags,FREEFLAG ; create the free memory block
	mov	[di].Base,esi
	mov	[di].Len,edx
AM06:
	pop	esi
	pop	edx
	pop	eax
	ret

AddMem	endp

End386	label	byte


