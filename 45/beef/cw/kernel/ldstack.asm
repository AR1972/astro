;*
;*	COW : Character Oriented Windows
;*
;*	ldstack.asm : stack walking

	TITLE	LDSTACK - stack walking procedures

	.xlist
	include kernel.inc
	include galloc.inc
	.list

;*****************************************************************************

;*	* Stack format (off of BP)
bpNext	EQU	0			;* next BP
pfnRet	EQU	2			;* return far address
offRet	EQU	pfnRet			;* return offset
psRet	EQU	pfnRet+2		;* return segment address
					;* may be < 256 for return thunks

;*****************************************************************************

sBegin	DATA

externW     <psLom>

IFDEF DEBUG
externW     <pGlobalHeap>
ENDIF ;DEBUG

externW <bpOldStack, ssOldStack>

sEnd	DATA

;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING				;* DS == pGlobalHeap
    assumes SS,DGROUP

externW	<bpSaveThunk, spSaveThunk, ssSaveThunk>	;* saved stack

IFDEF KERNEL_SWAP_STACK
;*	*  These variables are in the kernel for QC's weird stack swapping
externW	sstVap
externW	spVap
externW	ssVap
externW	bpVap
ENDIF	;KERNEL_SWAP_STACK


;********** PrepWalkStack **********
;*	entry:	n/a (SS:BP => this stack)
;*	* prepare for stack walking
;*	exit:	DS:CX => first stack location to walk

cProc	PrepWalkStack, <NEAR, ATOMIC>
cBegin	PrepWalkStack
	mov	ax,ss
	mov	ds,ax
	mov	cx,[bp].bpNext			;* ignore this frame
	mov	ax,bpSaveThunk
	or	ax,ax
	jz	walk_from_stack
;*	* walk from thunk
	mov	cx,ax
	mov	ds,ssSaveThunk
walk_from_stack:
cEnd	PrepWalkStack



;********** PatchStackMoved **********
;*	entry : segno = segment that moved
;*		psOld = old segment address
;*		psNew = new segment address
;*	* Walk the stack, update any address to return to the moved segment
;*	* Walks from the current BP
;*	exit : n/a

cProc	PatchStackMoved,<NEAR,PUBLIC,ATOMIC>,<DS>
    parmB segno
    parmW psOld
    parmW psNew
cBegin	PatchStackMoved

	cCall	PrepWalkStack
	mov	ax,psOld			;* value to scan for
	mov	dx,psNew			;* value to update to
psm_loop:	;* DS:CX => stack
	mov	bx,cx
	mov	cx,ds:[bx].bpNext		;* next BP (odd => far frame)
	jcxz	psm_done_this_stack		;* next BP == 0 => end of frame
	test	cl,1
	jz	psm_loop			;* ignore near frames
;*	* far frame
	dec	cx
	cmp	ax,ds:[bx].psRet		;* see if return to this segment
	jne	psm_loop
	mov	ds:[bx].psRet,dx
	jmp	psm_loop

psm_done_this_stack:

IFNDEF KERNEL_SWAP_STACK
;*	* if the bpOldStack in this stack is set, link to that stack
	mov	cx,ds:[bpOldStack]		;* may or may not be default DS
	jcxz	psm_exit			;* that's all
	mov	ds,ds:[ssOldStack]
	jmp	short psm_loop

ELSE
;* REVIEW: This is Jim's stack walking code, this should be replaced
;*	 by the above multiple stack code
	cmp	sstVap,SST_NO_VAP		;* See if a vap is active
	jz	psm_exit			;* No - then exit

	mov	cx,bpVap
	mov	ds,ssVap

psm_loop1:
	mov	bx,cx
	mov	cx,ds:[bx].bpNext		;* next BP (odd => far frame)
	jcxz	psm_exit			;* next BP == 0 => end of frame
	test	cl,1
	jz	psm_loop1			;* ignmore near frames
;*	* far frame
	dec	cx
	cmp	ax,ds:[bx].psRet		;* see if return to this segment
	jne	psm_loop1
	mov	ds:[bx].psRet,dx
	jmp	psm_loop1

ENDIF			; KERNEL_SWAP_STACK

psm_exit:

cEnd	PatchStackMoved



;********** PatchStackDiscarded **********
;*	entry : segno = segment that moved
;*		psOld = old segment address
;*	* Walk the stack, update any return addresses to return thunk.
;*	exit : n/a

cProc	PatchStackDiscarded,<NEAR,PUBLIC,ATOMIC>,<SI,DS>
    parmB segno
    parmW psOld
cBegin	PatchStackDiscarded

	mov	al,segno
	cCall	PrepareEntret			;* ES:SI => entret
	cCall	PrepWalkStack
	xor	dx,dx				;* special value => stuff thunk

	mov	ax,psOld			;* value to scan for
psd_loop:
	mov	bx,cx
	mov	cx,ds:[bx].bpNext		;* next BP (odd => far frame)
	jcxz	psd_done_this_stack		;* next BP == 0 => end of frame
	test	cl,1
	jz	psd_loop			;* ignore near frames
;*	* far frame
	dec	cx
	cmp	ax,ds:[bx].psRet		;* see if return to this segment
	jne	psd_loop
;*	* if (dx != 0) then set to that value
	mov	ds:[bx].psRet,dx
	or	dx,dx
	jnz	psd_loop
;*	* otherwise we have the first return address => point to return thunk
	mov	dx,ds:[bx].offRet
	mov	es:[si].offEntret,dx		;* save offset in thunk
	mov	ds:[bx].offRet,si
	mov	ds:[bx].psRet,es		;* es:si => thunk
;*	* all remaining returns will be to segno:offset
	xor	dh,dh
	mov	dl,segno
	jmp	psd_loop

psd_done_this_stack:

IFNDEF KERNEL_SWAP_STACK
;*	* if the bpOldStack in this stack is set, link to that stack
	mov	cx,ds:[bpOldStack]		;* may or may not be default DS
	jcxz	psd_exit			;* that's all
	mov	ds,ds:[ssOldStack]
	jmp	short psd_loop

ELSE
;* REVIEW: This is Jim's stack walking code, this should be replaced
;*	 by the above multiple stack code
	cmp	sstVap,SST_NO_VAP		;* Is there a vap in the system?
	je	psd_end1
	
	mov	cx,bpVap			;* Get pointers to vap stack
	mov	ds,ssVap

psd_loop1:
	mov	bx,cx
	mov	cx,ds:[bx].bpNext		;* next BP (odd => far frame)
	jcxz	psd_exit			;* next BP == 0 => end of frame
	test	cl,1
	jz	psd_loop1			;* ignore near frames
;*	* far frame
	dec	cx
	cmp	ax,ds:[bx].psRet		;* see if return to this segment
	jne	psd_loop1
;*	* if (dx != 0) then set to that value
	mov	ds:[bx].psRet,dx
	or	dx,dx
	jnz	psd_loop1
;*	* otherwise we have the first return address => point to return thunk
	mov	dx,ds:[bx].offRet
	mov	es:[si].offEntret,dx		;* save offset in thunk
	mov	ds:[bx].offRet,si
	mov	ds:[bx].psRet,es		;* es:si => thunk
;*	* all remaining returns will be to segno:offset
	xor	dh,dh
	mov	dl,segno
	jmp	psd_loop1

psd_end1:
ENDIF						; KERNEL_SWAP_STACK
psd_exit:

cEnd	PatchStackDiscarded



;********** PatchStackLoaded **********
;*	entry : segno = segment that got loaded
;*		psNew = new segment address
;*	* Walk the stack, update return thunks to real address
;*	exit : n/a

cProc	PatchStackLoaded,<NEAR,PUBLIC,ATOMIC>,<SI,DS>
    parmB segno
    parmW psNew
cBegin	PatchStackLoaded

	mov	al,segno
	cCall	PrepareEntret			;* ES:SI => entret
	cCall	PrepWalkStack
	mov	dx,psNew			;* proper ps
	mov	ax,es				;* first ps to find
	cmp	es:[si].offEntret,-1		;* -1 => top already found
	jne	psl_loop			;* find top entry (a thunk)
;*	* top entry already found, find next
psl_find_next:
	xor	ah,ah
	mov	al,segno

psl_loop:
	mov	bx,cx
	mov	cx,ds:[bx].bpNext		;* next BP (odd => far frame)
	jcxz	psl_done_this_stack		;* next BP == 0 => end of frame
	test	cl,1
	jz	psl_loop			;* ignore near frames
;*	* far frame
	dec	cx
	cmp	ax,ds:[bx].psRet		;* ps's match ?
	jne	psl_loop
;*	* if (ah != 0) then return thunk (may not be ours)
	or	ah,ah
	jnz	psl_retthunk
	mov	ds:[bx].psRet,dx
	jmp	psl_loop

psl_retthunk:
	cmp	ds:[bx].offRet,si		;* is it our return thunk ?
	jne	psl_loop			;* someone else's thunk
;*	* otherwise we have the proper return thunk, fix it up
	mov	ax,es:[si].offEntret
	mov	ds:[bx].offRet,ax
;*	* all remaining scans will be for 00:segno
	mov	ds:[bx].psRet,dx
	jmp	psl_find_next

psl_done_this_stack:

IFNDEF KERNEL_SWAP_STACK
;*	* if the bpOldStack in this stack is set, link to that stack
	mov	cx,ds:[bpOldStack]		;* may or may not be default DS
	jcxz	psl_exit			;* that's all
	mov	ds,ds:[ssOldStack]
	jmp	short psl_loop

ELSE
;* REVIEW: This is Jim's stack walking code, this should be replaced
;*	 by the above multiple stack code

	cmp	sstVap,SST_NO_VAP	;* Is there a vap process?
	je	psl_exit		;* No skip this
	
	mov	cx,bpVap		;* Point to the vap's stack
	mov	ds,ssVap

	mov	ax,es				;* first ps to find
	cmp	es:[si].offEntret,-1		;* -1 => top already found
	jne	psl_loop1			;* find top entry (a thunk)
;*	* top entry already found, find next
psl_find_next1:
	xor	ah,ah
	mov	al,segno

psl_loop1:
	mov	bx,cx
	mov	cx,ds:[bx].bpNext		;* next BP (odd => far frame)
	jcxz	psl_exit			;* next BP == 0 => end of frame
	test	cl,1
	jz	psl_loop1			;* ignore near frames
;*	* far frame
	dec	cx
	cmp	ax,ds:[bx].psRet		;* ps's match ?
	jne	psl_loop1
;*	* if (ah != 0) then return thunk (may not be ours)
	or	ah,ah
	jnz	psl_retthunk1
	mov	ds:[bx].psRet,dx
	jmp	psl_loop1

psl_retthunk1:
	cmp	ds:[bx].offRet,si		;* is it our return thunk ?
	jne	psl_loop1			;* someone else's thunk
;*	* otherwise we have the proper return thunk, fix it up
	mov	ax,es:[si].offEntret
	mov	ds:[bx].offRet,ax
;*	* all remaining scans will be for 00:segno
	mov	ds:[bx].psRet,dx
	jmp	psl_find_next1

ENDIF						; KERNEL_SWAP_STACK
psl_exit:

cEnd	PatchStackLoaded



;********** PrepareEntret **********
;*	entry : al = segno
;*	* prepare ES:SI to point to the specified return thunk (ENTRET)
;*	* also prepare CX for scanning
;*	* This routine is specifically designed for use by PatchStack routines.
;*	exit : ES:SI => entret of proper segment
;*		DS:CX = BP to start scan

cProc	PrepareEntret,<NEAR,ATOMIC>
cBegin	PrepareEntret

;*	* point ES:SI to ENTRET for this return thunk
	mov	es,psLom
	dec	al				;* make zero based
	xor	ah,ah				;* ax = segno-1
	Assert	<SIZE ENTRET EQ 6>
	shl	ax,1
	mov	si,ax
	shl	ax,1				;* times 4
	add	si,ax				;* segno * 6
	add	si,es:[neLom.ne_pretthunks]	;* &entret

;*	* special stack preparations
	cCall	PrepWalkStack

cEnd	PrepareEntret

;*****************************************************************************

;********** ThrowStack **********
;*	entry : bpNew = new BP value
;*	* Patch up any return thunks to reflect the new position
;*	* NOTE : this is not terribly fast, but should be only used for
;*	*  error recovery anyway.
;*	* NOTE : bpNew must be a previous valid bp.
;*	exit : n/a

cProc	ThrowStack,<FAR, PUBLIC, ATOMIC>,<SI>
    parmW  bpNew
cBegin	ThrowStack

;*	* scan from current bp value
;*	* if far return to psLom, assume a return thunk

	mov	dx,bpNew
	mov	cx,[bp].bpNext			;* ignore this frame
ts_resume:
	mov	ax,psLom			;* return thunks segment address
ts_loop:
	mov	bx,cx
	cmp	bx,dx				;* hit the end of throw frame ?
	jae	ts_end				;* done or error
	mov	cx,ss:[bx].bpNext	;* next BP (odd => far frame)
	test	cl,1
	jz	ts_loop			;* ignore near frames
;*	* far frame
	dec	cx
	cmp	ax,ss:[bx].psRet	;* see if return address to this segment
	jne	ts_loop
;*	* get address of thunk
	les	si,ss:[bx].pfnRet	;* get return address (i.e. thunk)
	AssertEq es:[si].opcEntret,opcCalln	;* should be call near
	xor	ah,ah
	mov	al,es:[si].segnoEntret	;* get segment to look for
IFDEF DEBUG
	cCall	AssertSegmentNotResident
ENDIF ;DEBUG
;*	* Scan from the new bp - find first return to "segno"
	push	cx
	mov	cx,bpNew
ts_loop2:
	mov	bx,cx
	jcxz	ts_end_scan
	mov	cx,ss:[bx].bpNext		;* next BP (odd => far frame)
	test	cl,1
	jz	ts_loop2			;* ignore near frames
;*	* far frame
	dec	cx
	cmp	ax,ss:[bx].psRet		;* see if return seg == segno
	jne	ts_loop2
;*	* this is a return to this segment
	mov	ax,ss:[bx].offRet
	mov	es:[si].offEntret,ax		;* save offset in thunk
	mov	ss:[bx].offRet,si
	mov	ss:[bx].psRet,es		;* es:si => thunk
ts_end_scan:					;* no more thunks found
	pop	cx
	jmp	ts_resume

ts_end:
IFDEF DEBUG	;* if above then gone too far => bad bpNew
	je	ts_ok
	cCall	CowAssertFailed
	DB	"ThrowStack - bogus BP$"
ts_ok:
ENDIF ;DEBUG

cEnd	ThrowStack


;*****************************************************************************

IFDEF DEBUG

;********** AssertSegmentNotResident **********
;*	entry : ax = segno
;*	* Debug assert that segment is not resident
;*	exit : n/a : No registers trashed !!!

cProc	AssertSegmentNotResident,<NEAR, ATOMIC>,<ES,DI>
cBegin	AssertSegmentNotResident

	push	ax				;* save segno
	dec	al				;* make zero based
	mov	ah,SIZE NEW_SEG1
	mul	ah
	add	ax,es:[neLom.ne_segtab]
	mov	di,ax
	mov	di,es:[di].ns_handle		;* get handle
	AssertReset di,1			;* must be even
	mov	es,pGlobalHeap
	AssertSet es:[di].he_flags,HE_DISCARDED
	pop	ax				;* restore ax = segno
	AssertEq ax,es:[di].he_owner

cEnd	AssertSegmentNotResident


ENDIF ;DEBUG

;*****************************************************************************

sEnd	KERNEL

	END
