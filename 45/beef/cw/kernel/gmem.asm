;*
;*	CW : Character Windows
;*
;*	
	TITLE	GMEM - Register interface to global memory allocator

	.xlist
	include	kernel.inc
	include	galloc.inc
	.list


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING			;* usually points to MOB
    assumes SS,DATA

;*	* low level procs
externNP    <halloc,hfree,hdref>	    ; HANDLE.ASM
externNP    <gsplice,gjoin,gzero>	    ; GALLOC.ASM
externNP    <gsearch,gmarkfree,gcheckfree>  ; GALLOC.ASM
externNP    <gmovebusy,gcompact>	    ; GCOMPACT.ASM
externNP    <gnotify>			    ; GINTERF.ASM
IFDEF	REALLOC_FIXED
externNP    <gfixedCompact>		    ; GFIXCOMP.ASM
ENDIF	; REALLOC_FIXED

	PUBLIC	galign, gdref, ghandle, galloc, grealloc
	PUBLIC	gfree, glock, gunlock

SUBRS	PROC	NEAR

; GALIGN - procedure to align the size request for a global item to
; the next high valid paragraph boundary.
;
;   Input:	BX = #paras
;		Carry flag set if #paras overflowed.
;
;   Output:	DX = #paras aligned to next higher multiple of 4
;
galign: jc	align1		    ; Overflow occur?
	lea	dx,[bx+GA_ALIGN]    ; No, add alignment amount
	and	dl,GA_MASK	    ; ...modulo alignment boundary
	cmp	dx,bx		    ; Test for overflow
	jb	align1		    ; Yes, continue
	ret
align1: mov	dx,GA_MASK	    ; Return largest possible size
	ret


; GDREF - procedure to dereference a global handle.
;
;   Inputs:	DX = handle
;
;   Outputs:	ES:DI = address of arena header
;		AX = address of client data
;		CH = lock count or zero for fixed objects
;		SI = handle table entry address or zero for fixed objects
;		Z flag set if NULL handle or AX = 0
;

gdref:
	xor	cx,cx			; Zero lock count
	mov	ax,dx			; Return handle if fixed object
	test	al,GA_FIXED		; Is it moveable?
	jz	dref3			; Yes, go dereference handle
	xor	si,si			; Set SI to zero for fixed objects
dref1:
	or	ax,ax			; No, were we passed NULL?
	jz	dref2			; Yes, then all done
	mov	bx,ax			; No, return ES:DI pointing
	dec	bx
	mov	es,bx			; ...to arena header
	cmp	ES:[di].ga_sig,GA_SIGNATURE ; Return error if, invalid sig.
	jne	dref4
	cmp	ES:[di].ga_handle,si	    ; Handle mismatch
	jne	dref4
	cmp	ES:[di].ga_owner,di	    ; or free block
	je	dref4
dref2:					; Otherwise return with Z flag clear
	ret
dref3:
	mov	si,dx
	call	hdref	    ; Get true address in AX and lock count in CH
	jnz	dref1	    ; Compute arena header if valid true address
	mov	bx,[si].he_address	; Return owner of discarded object
dref4:					; in BX
	xor	ax,ax
	ret

;
; GHANDLE - procedure return the handle for a global segment.
;
;   Inputs:	AX = handle or segment address parameter
;		DS:DI = master object segment address
;
;   Outputs:	AX = handle or zero if invalid segment address
;		Z flag set if DX is zero.  O.W. Z flag reset and
;		BX = pointer to handle table entry
;		CX = lock count and flags for moveable object
;		DX = segment address or zero if discarded or invalid handle
;		ES:DI = arena header of block
;
;   Destroys:	ES,BX
;

ghandle:
	mov	bx,ax			; Compute address of arena header
	test	bl,GA_FIXED		; Fixed?
	jnz	gh0			; Yes, have segment address then
	or	bx,bx			; Null handle?
	jz	gh1			; Yes, return null
	test	bl,2			; Valid moveable handle?
	jz	gh1			; No, return null
	mov	bx,ds:[di].hi_htable	; Get base of handle table
	cmp	ax,bx			; Handle pointer before it?
	jbe	gh1			; Yes, return null
	mov	cx,ds:[bx].ht_count	; Get size of handle table
	shl	cx,1
	shl	cx,1
	add	bx,cx
	cmp	ax,bx			; Handle pointer after it?
	jae	gh1			; Yes, return null
	mov	bx,ax
	mov	cx,word ptr [bx].he_flags
	inc	cx			; Free handle?
	jz	gh1			; Yes, return null
	dec	cx
	test	cl,HE_DISCARDED 	; No, discarded?
	jnz	gh3			; Yes, then no segment address
	mov	ax,ds:[bx].he_address	; No, convert handle to
	mov	bx,ax			; segment address
gh0:
	dec	bx
	cmp	ds:[di].hi_first,bx	; Error if points outside arena
	jae	gh1
	cmp	ds:[di].hi_last,bx
	jbe	gh1
	mov	es,bx
	cmp	ES:[di].ga_sig,GA_SIGNATURE
	jne	gh1			; Error if invalid signature byte
	cmp	ES:[di].ga_owner,di
	je	gh1			; Error if free segment
	mov	bx,ES:[di].ga_handle	; Get back pointer to handle
	or	bx,bx
	jz	ghx			; Return seg parameter if not moveable
	cmp	DS:[bx].he_address,ax	; Does handle point to segment?
	jne	gh1			; No, return error
gh0a:
	mov	cx,word ptr [bx].he_flags
	errnz	<2-he_flags>
	errnz	<3-he_count>
	mov	dx,ax			; Return segment address in DX
	or	dx,dx			; Clear Z flag
	mov	ax,bx			; Yes, return handle
	ret				; Return
gh1:	xor	ax,ax
ghx:	xor	cx,cx
	mov	dx,ax
	ret
gh3:
	xor	ax,ax
	test	cl,HE_SWAPPED		; Discarded, is it swapped?
	jz	gh0a			; No, return NULL

	jmp	short gh1		;* all done


; GALLOC - procedure to allocate global memory.
;
;   Inputs:	AX = allocation flags
;		BX = #paragraphs
;		CX = owner field
;		DS:DI = address of global heap info
;
;   Outputs:	AX = handle to object or zero
;		BX = size of largest free block if AX = 0
;		CX = AX
;

galloc:
	or	bx,bx			; Allocating zero size?
	jnz	ga1			; No, continue
	xchg	ax,cx			; AX = owner, CX = flags
	test	cl,GA_MOVEABLE		; Yes, moveable?
	jz	gaerr			; No, return error (AX = 0)
	push	cx			; save flags
	call	halloc			; and allocate handle for owner
	pop	dx			; restore flags
	jcxz	gaerr			; Error return
	or	dh,HE_DISCARDED 	; and mark as discarded
	mov	[bx].he_flags,dh	; Set handle flags
	jmp	short gax
gaerr1:
	dec	dx			; Free block if no available
	mov	es,dx			; handles.
	call	gmarkfree
gaerr:
	xor	dx,dx			; DX = 0 means NOT out of memory
	xor	ax,ax			; Return AX = 0 to indicate error
	jmp	short gax
ga1:
	call	gsearch 		; Search for block big enough
	jz	gax			; Done, if couldn't get enough
	test	dl,GA_MOVEABLE		; Is this a moveable object?
	jz	gax			; No, all done
	call	halloc			; Yes, allocate handle for object in AX
	jcxz	gaerr1			; No handles, free memory
	mov	si,[bx].he_address	; Store back link to handle in header
	dec	si
	mov	es,si
	mov	ES:[di].ga_handle,bx	; Mark as moveable block
	mov	[bx].he_flags,dh	; Save discard level in handle
gax:
	mov	cx,ax
	ret


; GREALLOC - procedure to reallocate global memory object
;
;   Inputs:	AX = allocation flags
;		BX = #paragraphs for new size
;		CX = new owner field value
;		DX = existing handle
;		DS:DI = address of global heap info
;
;   Outputs:	AX = handle to object or zero
;		DX = size of largest free block if AX = 0
;		CX = AX
;

grealloc:
	push	bp
	mov	bp,sp
	push	ax
rflags	EQU	word ptr [bp-2]
	push	dx
h	EQU	word ptr [bp-4]
	push	cx
owner	EQU	word ptr [bp-6]
	push	bx
rsize	EQU	word ptr [bp-8]
	call	gdref
	mov	bx,rsize
	jz	racreate	    ; Do nothing with zero, free or discarded  handles
	or	bx,bx		    ; Are we reallocing to zero length?
	jnz	raokay1  	    ; No, continue
	jcxz	radiscard	    ; No, is handle locked?
rafail:
	xor	ax,ax		    ; Yes, return failure
	xor	dx,dx
	jmp	raexit

radiscard:			    ; No, then try to discard the object
	; Here to discard object, when reallocating to zero size.  This
	; feature is only enabled if the caller passes the moveable flag
	test	byte ptr rflags,GA_MOVEABLE ; Did they want to discard?
	jz	rafail		    ; No, then return failure.
	mov	al,GN_DISCARD	    ; AL = discard message code
	xor	cx,cx		    ; CX = discard level of 0 (means realloc)
	mov	bx,h		    ; BX = handle
	push	es
	mov	dx,es:[di].ga_owner	;* set owner for notify
	call	gnotify			;* notify that we are discarding
					;* note : no return value
	pop	es
	push	ES:[di].ga_owner    ; Save owner field
	push	ax
	call	gmarkfree	    ; Free client data
	pop	cx
	jz	rafixed 	    ; Return NULL if freed a fixed block
	mov	[si].he_address,cx  ; No, remember owner in handle table entry
	or	[si].he_flags,HE_DISCARDED  ; ...and mark discarded
	jmp	short rasame1	    ; Return original handle, except
				    ; GlobalLock will now return null.
rafixed:
	pop	ax
	xor	ax,ax
	jmp	raexit

rasame1:
        jmp     short rasame
raokay1:
        jmp     short raokay
rafail0:
	jmp	rafail

racreate:
	test	cl,HE_DISCARDED     ; Is this a discarded handle?
	jz	rafail0 	    ; No, return error
	or	bx,bx		    ; Are we reallocing to zero length?
	jz	rasame		    ; Yes, return handle as is.
	mov	ax,GA_MOVEABLE	    ; Reallocating a moveable object
	or	ax,rflags	    ; ...plus any flags from the caller
	mov	cl,[si].he_flags    ; ...plus saved segment type bits
	and	cl,GA_SEGTYPE
	or	al,cl
	or	ah,cl
	test	al,GA_DISCCODE	    ; Discardable code segment?
	jz	ranotcode
	or	al,GA_ALLOCHIGH     ; Yes, allocate high
ranotcode:
	mov	[di].gi_cmpflags,al ; Save flags for gcompact
	mov	cx,[si].he_owner    ; Use previous owner
	push	si		    ; save handle
	call	gsearch 	    ; Find block big enough
	pop	si		    ; restore existing handle
	jz	racreate1
	xor	[si].he_flags,HE_DISCARDED  ; and mark as not discarded
	mov	[si].he_address,ax  ; Set new client data address
	mov	ch,[si].he_flags    ; Get handle flags
	xchg	ax,si		    ; Return original handle
				    ; Set back link to handle in new block
	dec	si
	mov	es,si
	mov	ES:[di].ga_handle,ax
	and	ch,GA_SEGTYPE
	mov	ES:[di].ga_flags,ch ; Copy segment type flags to ga_flags
racreate0:
	jmp	raexit
racreate1:
	jmp	rafailmem1
raokay:
	mov	si,bx
	add	bx,ax		    ; Compute address of new next header
	call	galign		    ; assuming there is room.
; Here if not trying to realloc this block to zero
; ES:0 = arena header of current block
; AX:0 = client address of current block
; CH = lock count of current block
; DX:0 = new next block, based on new requested size
; SI = new requested size of block
	mov	bx,ES:[di].ga_next  ; Get address of current next header
	cmp	dx,bx		    ; Are we growing or shrinking?
	ja	ragrow		    ; We are growing
rashrink:			    ; We are shrinking
; Here to shrink a block
; ES:0 = arena header of current block
; BX:0 = arena header of next block
; DX:0 = new next block, based on new requested size
	mov	si,dx		    ; SI = new next block
	inc	dx		    ; Test for small shrinkage
	inc	dx
	errnz	<GA_ALIGN-1>

	cmp	dx,bx		    ; Is there enough room from for free block?
	jae	raoverflow	    ; No, then no change to make
				    ; Yes, ES:DI = current block, SI=new block
	call	gsplice 	    ; splice new block into the arena
	mov	es,si		    ; ES:DI = new free block
	call	gmarkfree	    ; Mark it as free
rasame:
	mov	ax,h		    ; Return the same handle
rax:	jmp	raexit		    ; All done

raoverflow:
	test	byte ptr rflags,GA_ZEROINIT ; Zero file extension?
	jz	rasame	            ; No, continue
	mov	cx,bx		    ; Next heap header
	dec	cx		    ; Last paragraph for us
	mov	bx,si		    ; Where the new block would have started
	call	gzero		    ; zero fill extension
	jmp	short rasame

; Here to try to grow the current block
; AX:0 = client address of current block
; CH = lock count of current block
; ES:0 = arena header of current block
; BX:0 = arena header of next block
; DX:0 = new next block, based on new requested size
; SI = new requested size of block
ragrow:
	mov	al,1		    ;* first time through
ragrowagain:	;* al == 0 => don't retry
	push	es		    ; Save current block address
	mov	es,bx		    ; ES = next block address
	cmp	ES:[di].ga_owner,di ; Is next block free?
	jne	ragrow1		    ; No, can't fit it in
	sub	dx,bx		    ; Yes, compute how much of it we need
	push	si		    ; See if free block is big enough
	push	ax		    ;* save flag
	call	gcheckfree
	pop	ax
	pop	si
	jb	ragrow2		    ; No, can't fit it in
	add	dx,bx		    ; Yes, restore new next block address
	pop	cx		    ; Discard saved value (gjoin returns it)
	mov	cx,es		    ; Yes, save free block address in CX
	call	gjoin		    ; and attach to end of current block
	test	byte ptr rflags,GA_ZEROINIT ; Zero fill extension?
	jz	ranz		    ; No, continue
	mov	bx,cx		    ; Yes, BX = first paragraph to fill
	mov	cx,dx		    ; compute last paragraph to fill
	dec	cx		    ; into CX
	call	gzero		    ; zero fill extension
ranz:
	mov	bx,ES:[di].ga_next  ; Pick up new next block address
	jmp	rashrink	    ; Now shrink block to correct size

ragrow2:
	add	dx,bx		    ;* restore dx
; Here to try to move the current block (or grow fixed block in place)
; BX:0 = arena header of next block
; CH = lock count of current block
; DX:0 = new next block, based on new requested size
; (on stack) ES:0 = arena header of current block
; SI = new requested size of block
ragrow1:
	pop	es		    ; Recover current block address
	push	bx
	push	dx		    ;* save flags in case we must retry
	mov	dx,rflags	    ; get the passed in flags
	AssertReset dl,GA_DISCCODE  ;* you can not grow fixed code !
	mov	bx,GA_MOVEABLE	    ; Determine if okay to move this guy
	jcxz	ramove1 	    ; Continue if this handle not locked
	test	dx,bx		    ; Locked.  Did they say move anyway?
	jnz	ramove1 	    ; Yes, go do it
;*	* Can't move (i.e. fixed or locked)
	jmp	short ragrowfixed   ;* force grow in place
ramove1:
	or	dx,bx		    ; make sure moveable bit set
	test	h,GA_FIXED	    ; Is this a moveable handle?
	jz	ramove2 	    ; Yes, okay to move
	xor	dx,bx		    ; No, clear moveable flag in arg to gsearch
	test	rflags,bx	    ; Did they say it's okay to move?
	jnz	ramove2 	    ; Yes, proceed
ragrowfixed:
	or	al,al
	jz	rafailmem	    ;* try maximum of twice
	push	ax
	push	si
	push	es
IFDEF	REALLOC_FIXED
	mov	dx,si		    ;**** over size request M00BUG
	cCall	gfixedCompact
ELSE	; REALLOC_FIXED
	xor	dx,dx		    ; No, get size of largest free block
	call	gcompact	    ; AX = size of largest free block
ENDIF	; REALLOC_FIXED
	pop	es
	pop	si
	pop	ax
	xor	al,al		    ;* last chance
;*	* block can not be moved, but gcompact may have freed sufficient
;*	* space.
	pop	dx
	pop	bx		    ;* restore everything to look like ragrow
	jmp	ragrowagain

rafailmem:
	mov	bx,h
	test	bl,GA_FIXED
	jnz	rafail2
	mov	bx,[bx].he_address
rafail2:			    ; BX = client data address
	dec	bx
	mov	es,bx		    ; ES = arena header address
	mov	ax,es:[di].ga_size  ; AX = size of current block
	mov	es,es:[di].ga_next  ; Check following block
	cmp	es:[di].ga_owner,di ; Is it free?
	jne	rafailmem0	    ; No, continue
	add	ax,es:[di].ga_size  ; Yes, then include it as well
	inc	ax
rafailmem0:
	cmp	ax,dx		    ; Chose the larger of the two
	jbe	rafailmem1
	mov	dx,ax
rafailmem1:
	xor	ax,ax
	jmp	rax

ramove2:
	pop	ax
	pop	ax		    ;* ignore 2 saved values from ragrow1
	mov	ax,dx		    ; AX = allocation flags
	mov	bx,si		    ; BX = size of new block
	mov	cx,si		    ; CX = owner (use size for now)
	call	gsearch 	    ; Find block big enough
	jz	racantmove	    ; Cant find one, grow in place now?
	mov	si,h
	mov	cx,ax
	test	si,GA_FIXED
	jnz	ramove3
	mov	cx,si
	mov	si,[si].he_address  ; SI = old client data address
ramove3:
	dec	ax
	mov	es,ax		    ; ES = destination arena
	dec	si		    ; SI = source arena
	call	gmovebusy	    ; Call common code to move busy block
	mov	ax,cx		    ; Return new handle
raexit:
	mov	cx,ax
	mov	sp,bp
	pop	bp
	ret


racantmove:
	mov	bx,h
	test	bl,GA_FIXED
	jnz	racmove1
	mov	bx,[bx].he_address
racmove1:			    ; BX = client data address
	mov	ax,bx
	dec	bx
	mov	es,bx		    ; ES = arena header address

	mov	bx,rsize
	add	bx,ax		    ; Compute address of new next header
	call	galign		    ; assuming there is room.

	mov	bx,ES:[di].ga_next  ; Get address of current next header
	mov	es,bx		    ; ES = next block address
	cmp	ES:[di].ga_owner,di ; Is next block free?
	jne	racmove3	    ; No, try to move the current block
	sub	dx,bx		    ; Yes, compute how much of it we need
	call	gcheckfree	    ; See if free block is big enough
	jb	racmove3	    ; No, try to move the current block
	add	dx,bx		    ; Yes, restore new next block address
	mov	cx,es		    ; Yes, save free block address in CX
	call	gjoin		    ; and attach to end of current block
	test	byte ptr rflags,GA_ZEROINIT ; Zero fill extension?
	jz	racmove2	    ; No, continue
	mov	bx,cx		    ; Yes, BX = first paragraph to fill
	mov	cx,dx		    ; compute last paragraph to fill
	dec	cx		    ; into CX
	call	gzero		    ; zero fill extension
racmove2:
	mov	bx,ES:[di].ga_next  ; Pick up new next block address
	jmp	rashrink	    ; Now shrink block to correct size

racmove3:
	xor	dx,dx		    ; No, get size of largest free block
	call	gcompact
	mov	dx,ax		    ; DX = size of largest free block
	jmp	rafailmem


; GFREE - procedure to free a global object
;
;   Inputs:	DX = global memory object handle
;		CX = owner field value to match or zero if dont care
;		DS:DI = address of global heap info
;
;   Outputs:	AX = zero it successful or handle if not
;		CX = AX
;

gfree:
	push	cx
	call	gdref
	pop	dx
	jz	gf2		    ; Free handle if object discarded
	or	dx,dx
	jnz	gf3
gf1:
	call	gmarkfree	    ; No, free the object
gf2:
	call	hfree		    ; Free handle
gfx:
	mov	cx,ax
	ret
gf3:
	cmp	ES:[di].ga_owner,dx
	je	gf1
	mov	ax,-1
	jmp	short gfx


; GLOCK - procedure to increment the lock count of an object
;
;   Inputs:	BX = handle to global object
;		CX = handle table flags and lock count for moveable objects
;		DX = segment address of object
;		DS:DI = address of master object
;
;   Outputs:	Updated lock count in handle table entry and CX
;		Z flag set if count overflowed.
;

glock:
	inc	ch		    ; Increment lock count
	jz	gl2		    ; All done if overflow
	mov	[bx].he_count,ch    ; Update lock count
gl2:
	ret


; GUNLOCK - procedure to deccrement the lock count of an object
;
;   Inputs:	BX = handle to global object
;		CX = handle table flags and lock count for moveable objects
;		DX = segment address of object
;		DS:DI = address of master object
;
;   Outputs:	CX = 0 if handle already unlocked. non-zero o.w.
;		Updated lock count in handle table entry and CX
;		Z flag set if count underflowed.
;

gunlock:
	dec	ch		    ; Decrement usage count
	cmp	ch,0FFh-1	    ; ff -> fe, 0 -> ff
	jae	gul1		    ; Return if pinned, or was already zero
	dec	[bx].he_count	    ; Non-zero update lock count
	jnz	gul2		    ; All done if still non-zero
	test	cl,GA_DISCARDABLE   ; Is this a discardable handle?
	jz	gul1		    ; No, all done
gul1:
	xor	cx,cx
gul2:
	ret			    ; Non-zero lock count. Return true


SUBRS	ENDP

sEnd	KERNEL

	END

