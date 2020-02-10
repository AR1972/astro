;*
;*	COW : Character Oriented Windows
;*
;*	ldthunk.asm : code THUNK reloader

	TITLE	LDTHUNK - THUNK handler

	.xlist
	include kernel.inc
	include galloc.inc
	include sstack.inc
	.list

IFDEF DEBPUB	;* Debugging publics
	PUBLIC	ReloadSegment, RelruSegment, ReturnThunk
ENDIF ;DEBPUB


sBegin	DATA
    assumes DS,DGROUP

externW     <psLom>
externW     <pGlobalHeap>
externW	    <bpOldStack, ssOldStack, pStackMin>

IFNDEF NOPCODE
externW     <$q_mpsnq>			;* HandleTable - 2
ENDIF ;!NOPCODE

sEnd	DATA

sBegin	BSS
    assumes DS,DGROUP

staticB	    levelLru,0			;* 0 => no Relru's since last sweep
					;* 1 => recent Relru's since last sweep
					;* >1 => major Relru's since last sweep
staticB     fLockoutSweep,0		;* TRUE => in the middle of changing thunks

sEnd	BSS


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING		;* may be called from anywhere
    assumes SS,DGROUP

externNP    <LoadSegment>			;* from ldseg.asm

globalW	bpSaveThunk,0
globalW	spSaveThunk,0
globalW	ssSaveThunk,0
staticW	axSaveThunk,0
staticW	bxSaveThunk,0

IFDEF KERNEL_SWAP_STACK
globalW	sstVap,SST_NO_VAP		; Current Vap Stack State

;*
;*	*  Variables associated with Current Vap Stack

globalW	spVap,?
globalW	ssVap,?
globalW bpVap,?

;*	* Variables associated with Kernel Stack

globalW	spKernel,0
globalW	ssKernel,0
globalW bpKernel,0

ENDIF						;* KERNEL_SWAP_STACK

;*	* Variables in the code segment !!! *

staticW csSave,?
staticW ipSave,?
staticW flagsSave,?

;* * since LRU may occur for resident segments, separate variables are needed
staticW ipSaveLru,?
staticW flagsSaveLru,?
staticW	csSaveLru,?


;********** ReloadSegment **********
;*	entry : MUST be called in the following format
;*		and must be part of ENTMOVE1 structure - in psLom !!
;*			CALLN to near, then JMPF ReloadSegment
;*			DW offset
;*			DB segno
;*	* Load the requested segment, fix up entry table
;*	* then jump to the entry point
;*	exit : never return to caller, jump to destination segment/offset
;*	* DOES NOT ALTER ANY REGISTERS (ignores changing DS)

cProc	ReloadSegment,<FAR>
cBegin	nogen	;ReloadSegment

    assumes DS,NOTHING
	pop	ipSave
	pushf
	pop	flagsSave
;*	* point return address to start of ENTMOVE
	Assert	<offDestEntmove1-opcEntmove1 EQ 3>	;* Near call size
	sub	ipSave,offDestEntmove1-opcEntmove1

IFNDEF KERNEL_SWAP_STACK
	SetStackToDds	1
ELSE
;*	* REVIEW: replace this with CRMGR stack interface
	;*	See if called with the kernel's stack or with a vap stack.

	cmp	sstVap,SST_ACT_VAP	; Is the vap currently active?
	jne	rs10
	
	;*
	;*	Create a stack frame for this function -- may need to back
	;*		batch the call to here if we discard the wrong segment
	;*
	
	inc	bp
	push	bp
	mov	bp,sp
	
	;*	The Vap stack is currently active!!
	;*	Change to the KERNEL stack since ss==DGROUP must be true.
	
	mov	bpVap,bp		; Save the vap's stack
	mov	ssVap,ss
	mov	spVap,sp
	
	mov	bp,bpKernel		; Now load in the kernel's stack
	cli
	mov	ss,ssKernel
	mov	sp,spKernel
	sti
	
	;	Now that stack are changed continue with the real code

rs10:
ENDIF							;* KERNEL_SWAP_STACK
	inc	bp
	push	bp
	mov	bp,sp
	push	ds
	push	es		    ; These registers used
	push	dx
	push	cx
IFDEF KERNEL_SWAP_STACK
	push	bx
	push	ax
ENDIF

	    ; Get address of interrupt instruction within entry table
	mov	es,psLom			;* call from psLom
	mov	csSave,es			;* save this for later
	mov	bx,ipSave
	    ; Get segment number to load
	xor	cx,cx
	mov	cl,es:[bx+segnoEntmove1]
	push	cx			;* segno

	cCall	LoadSegment	;<cx> - returns ax = ps (ignore)

IFDEF KERNEL_SWAP_STACK
	pop	ax
	pop	bx
ENDIF
	pop	cx
	pop	dx
	pop	es
	pop	ds
	pop	bp
	dec	bp
	
IFNDEF KERNEL_SWAP_STACK
	RestoreStackFromDds 1
ELSE
;*	* REVIEW: replace this with CRMGR stack interface
	;	Now - Do we need to restore the vap's stack?

	cmp	sstVap,SST_ACT_VAP	; Is the vap currently active?
	jne	rs99
	
	mov	bp,bpVap
	cli
	mov	ss,ssVap
	mov	sp,spVap
	sti
	
	pop	bp			;* Remove the stack frame
	dec	bp
	
	; Now that the correct stack is pointed to, set-up the return
	;	address as an interupt return.   We indirect back through
	;	the thunk entry which is now a near call to the routine
	;	RelruSegment rather than a near call to this routine.

rs99:
ENDIF						;KERNEL_SWAP_STACK
	push	flagsSave
	push	csSave
	push	ipSave
	iret

cEnd	nogen	;ReloadSegment


;********** RelruSegment **********
;*	entry : MUST be called in the following format
;*		and must be part of ENTMOVE1 structure - in psLom !!
;*			CALLN to near, then JMPF RelruSegment
;*			DW offset
;*			DB segno
;*	* Segment MUST! be loaded, just update LRU
;*	exit : never return to caller, jump to destination segment/offset

cProc	RelruSegment,<FAR>
cBegin	nogen	;ReluSegment

    assumes DS,NOTHING
	pop	ipSaveLru		;* near call return address
	pushf
	pop	flagsSaveLru

IFNDEF KERNEL_SWAP_STACK
	SetStackToDds	0		;* no swapping will occur
ELSE
;*	* REVIEW: replace this with CRMGR stack interface
	;*	See if called with the kernel's stack or with a vap stack.

	cmp	sstVap,SST_ACT_VAP	; Is the vap currently active?
	jne	rls10
	
	;*	The Vap stack is currently active!!
	;*	Change to the KERNEL stack since ss==DGROUP must be true.
	
	mov	bpVap,bp		; Save the vap's stack
	mov	ssVap,ss
	mov	spVap,sp
	
	mov	bp,bpKernel		; Now load in the kernel's stack
	cli
	mov	ss,ssKernel
	mov	sp,spKernel
	sti
	
	;	Now that stack are changed continue with the real code

rls10:
ENDIF						;KERNEL_SWAP_STACK

	push	ds
	push	es		    ; These registers used
	push	dx
	push	cx
IFDEF KERNEL_SWAP_STACK
	push	bx
	push	ax
ENDIF

	mov	fLockoutSweep,1

;*	* point return address to start of ENTMOVE1
	Assert	<offDestEntmove1-opcEntmove1 EQ 3>	;* Near call size
	mov	ax,ipSaveLru
	sub	ax,offDestEntmove1-opcEntmove1
	mov	ipSaveLru,ax
	mov	dx,ax

;*	* Get segment number to re-lru
	mov	ds,psLom
    assumes DS,NOTHING
	mov	csSaveLru,ds
	mov	bx,dx				;* call was in psLom
	mov	al,ds:[bx+segnoEntmove1]
	dec	al
	xor	ah,ah
	mov	bx,ax				;* bx = zero based segment #
	add	bx,ds:[neLom.ne_psegrefbytes]
	xor	cl,cl
	xchg	cl,ds:[bx]			;* re-lru'd
						;* stuff 0, cl = old count
	or	cl,cl
	js	negative_segref
	or	levelLru,cl			;* if segref > 1 then
						;* levelLru will be > 1
	jmp	short resume_segref

;*	* negative_segref
;*	-- a segref was negative, the only allowed negative segrefs are
;*	-- for bound segments (assert that) and restore segref
;*	-- (i.e. bound stay bound).

negative_segref:
	AssertEq cl,segrefBound
	mov	ds:[bx],cl 		;* restore it

resume_segref:

;*	* get segment address
	Assert	<SIZE NEW_SEG1 EQ 10>
	shl	ax,1
	mov	bx,ax				;* times 2
	shl	ax,1
	shl	ax,1
	add	bx,ax				;* times 10
	add	bx,ds:[neLom.ne_segtab]		;* ds:bx => NEW_SEG1
	mov	bx,ds:[bx].ns_handle		;* MUST BE A HANDLE
	AssertReset bx,1			;* (i.e. odd)
;*	* deference handle - cheap
	mov	es,pGlobalHeap
	AssertNe es:[bx].he_flags,HE_DISCARDED	;* MUST be resident
	mov	ax,es:[bx].he_address

;*	* change ENTMOVE1 (relru) to ENTMOVE
	mov	bx,dx				;* ES:BX => ENTMOVE / ENTMOVE1
	AssertEq ds:[bx].opcEntmove,opcCalln	;* should have been a call
	mov	ds:[bx].opcEntmove,opcJmpf
	mov	dx,ds:[bx].offDestEntmove1
	mov	ds:[bx].offEntmove,dx
	mov	ds:[bx].segEntmove,ax		;* ps of loaded code segment

;*	* done
	mov	fLockoutSweep,0
IFDEF KERNEL_SWAP_STACK
	pop	ax
	pop	bx
ENDIF
	pop	cx
	pop	dx
	pop	es
	pop	ds

IFNDEF KERNEL_SWAP_STACK
	RestoreStackFromDds 0		;* no swapping will occur
ELSE
;*	* REVIEW: replace this with CRMGR stack interface
	;	Now - Do we need to restore the vap's stack?

	cmp	sstVap,SST_ACT_VAP	; Is the vap currently active?
	jne	rls99
	
	mov	bp,bpVap
	cli
	mov	ss,ssVap
	mov	sp,spVap
	sti
	
	; Now that the correct stack is pointed to, set-up the return
	;	address as an interupt return.   We indirect back through
	;	the thunk entry which is now a jump to the real address
	;	rather than a near call to this routine.

rls99:
ENDIF						;KERNEL_SWAP_STACK
	push	flagsSaveLru
	push	csSaveLru
	push	ipSaveLru
	iret

cEnd	nogen	;RelruSegment



;********** ReturnThunk **********
;*	entry : MUST be called in the following format
;*		and must be part of entry table:
;*			CALLN then JMPF to ReturnThunk
;*			DB segno
;*			DW offset
;*	* Return via a return thunk
;*	* DS is actually the destination IP
;*	* the return CS:IP has the handle of the proper DS encoded in it.
;*	* load in the returned segment (if needed), return to the proper
;*	*  place with the proper DS
;*	exit : never return to caller, jump to return address
;*	* DOES NOT ALTER ANY REGISTERS (other than DS)

cProc	ReturnThunk,<FAR>
cBegin	nogen ;ReturnThunk

    assumes DS,NOTHING
	pop	ipSave					;* => segno
	pushf
	pop	flagsSave

IFNDEF KERNEL_SWAP_STACK
	SetStackToDds	1
ELSE
;*	* REVIEW: replace this with CRMGR stack interface
	;*	See if called with the kernel's stack or with a vap stack.

	cmp	sstVap,SST_ACT_VAP	; Is the vap currently active?
	jne	rt10
	
	;*
	;*	Create a stack segment for insurance
	;*
	
	inc	bp
	push	bp
	mov	bp,sp
	
	;*	The Vap stack is currently active!!
	;*	Change to the KERNEL stack since ss==DGROUP must be true.
	
	mov	bpVap,bp		; Save the vap's stack
	mov	ssVap,ss
	mov	spVap,sp
	
	mov	bp,bpKernel		; Now load in the kernel's stack
	cli
	mov	ss,ssKernel
	mov	sp,spKernel
	sti
	
	;	Now that stack are changed continue with the real code

rt10:
ENDIF						; KERNEL_SWAP_STACK
	inc	bp
	push	bp
	mov	bp,sp
	push	ds
	push	es		    ; These registers used
	push	dx
	push	cx
IFDEF KERNEL_SWAP_STACK
	push	bx
	push	ax
ENDIF

;*	* Get address after return thunk's call instruction
	mov	es,psLom			;* call from psLom
	mov	bx,ipSave
;*	* Get return offset
	mov	cx,es:[bx+offEntret-3]		;* after near call
	mov	ipSave,cx			;* return offset
	mov	es:[bx+offEntret-3],-1		;* top entry already found
;*	* Get segment number to load
	Assert	<segnoEntret EQ 3>		;* after near call
	xor	cx,cx
	mov	cl,es:[bx+segnoEntret-3]
	push	cx			;* segno

	cCall	LoadSegment	;<es,cx,bx,bx> - returns AX = psLoaded
	mov	csSave,ax

IFDEF KERNEL_SWAP_STACK
	pop	ax
	pop	bx
ENDIF
	pop	cx
	pop	dx
	pop	es
	pop	ds
	pop	bp
	dec	bp

IFNDEF KERNEL_SWAP_STACK	
	RestoreStackFromDds 1
ELSE
;*	* REVIEW: replace this with CRMGR stack interface
	;	Now - Do we need to restore the vap's stack?

	cmp	sstVap,SST_ACT_VAP	; Is the vap currently active?
	jne	rt99
	
	mov	bp,bpVap
	cli
	mov	ss,ssVap
	mov	sp,spVap
	sti

	pop	bp			;* Remove stack frame
	dec	bp
	
rt99:
ENDIF						; KERNEL_SWAP_STACK
	push	flagsSave
	push	csSave				;* return to proper segment
	push	ipSave
	iret

cEnd	nogen ;ReturnThunk

;*****************************************************************************

;*	* Thunk Patch Routines *

    assumes ds,nothing


;********** PatchThunkMoved **********
;*	entry : segno = segment number that moved (1 based)
;*		psNew = new physical segment it is located at
;*	* Fix up thunks & other info for moved segment
;*		1) fix up moveable entry table
;*	exit : n/a

cProc	PatchThunkMoved,<NEAR,PUBLIC>,<DS>
    parmB segno
    parmW psNew
cBegin	PatchThunkMoved

	mov	fLockoutSweep,1

;*	* Scan the entry table
	mov	ds,psLom			;* assumed throughout
	mov	bx,ds:[neLom.ne_rgentmove]
	mov	cx,ds:[neLom.ne_cmovent]
	mov	al,segno
	mov	es,psNew
pmv_lp:
	cmp	al,ds:[bx].segnoEntmove		;* segment in question ?
	jne	pmv_nxt
	cmp	ds:[bx].opcEntmove,opcJmpf
	jnz	pmv_nxt				;* ignore if Relru
	mov	ds:[bx].segEntmove,es		;* fix direct jump address
pmv_nxt:
	add	bx,SIZE ENTMOVE
	loop	pmv_lp

	mov	fLockoutSweep,0

cEnd	PatchThunkMoved



;********** PatchThunkLoaded **********
;*	entry : segno = segment number that got loaded (1 based)
;*		psNew = new physical segment it is located at
;*	* Fix up thunks & other info for moved segment
;*		1) fix up moveable entry table
;*		2) clear segment reference byte
;*	exit : n/a

cProc	PatchThunkLoaded,<NEAR,PUBLIC>,<DS>
    parmB segno
    parmW psNew
cBegin	PatchThunkLoaded

	mov	fLockoutSweep,1

;*	* Scan the entry table
	mov	ds,psLom			;* assumed throughout
	mov	bx,ds:[neLom.ne_rgentmove]
	mov	cx,ds:[neLom.ne_cmovent]
	mov	al,segno
	mov	es,psNew
pld_lp:
	cmp	al,ds:[bx].segnoEntmove		;* segment in question ?
	jne	pld_nxt
	AssertEq ds:[bx].opcEntmove,opcCalln	;* should have been a call
	mov	ds:[bx].opcEntmove,opcJmpf
	mov	dx,ds:[bx].offDestEntmove1
	mov	ds:[bx].offEntmove,dx
	mov	ds:[bx].segEntmove,es
pld_nxt:
	add	bx,SIZE ENTMOVE
	loop	pld_lp

;*	* clear segment reference byte
	dec	al
	xor	ah,ah
	add	ax,ds:[neLom.ne_psegrefbytes]
	mov	bx,ax
	Assert	<segrefLoaded EQ 0>
	mov	byte ptr ds:[bx],segrefLoaded

	mov	levelLru,0ffh			;* force sweep
	mov	fLockoutSweep,0

cEnd	PatchThunkLoaded



;********** PatchThunkDiscarded **********
;*	entry : segno = segment number that got discarded (1 based)
;*		DS => DDS
;*	* Fix up thunks & other info for moved segment
;*		1) fix up moveable entry table
;*		2) set segment reference byte to 0ffh
;*	exit : n/a

cProc	PatchThunkDiscarded,<NEAR,PUBLIC>,<DS>
    parmB segno
cBegin	PatchThunkDiscarded

	mov	fLockoutSweep,1

;*	* Scan the entry table
	mov	ds,psLom			;* assumed throughout
	mov	bx,ds:[neLom.ne_rgentmove]
	mov	cx,ds:[neLom.ne_cmovent]
	mov	al,segno
pds_lp:
	cmp	al,ds:[bx].segnoEntmove		;* segment in question ?
	jne	pds_nxt
	cmp	ds:[bx].opcEntmove1,opcCalln	;* already a calln
	jz	pds_already_entmove1
;*	* convert ENTMOVE -> ENTMOVE1
	AssertEq ds:[bx].opcEntmove1,opcJmpf	;* must have been far call
	mov	ds:[bx].opcEntmove1,opcCalln
	mov	dx,ds:[bx].offEntmove		;* resident offset
	mov	ds:[bx].offDestEntmove1,dx	;* non-resident offset
pds_already_entmove1:
	mov	dx,opcReloadLom-3		;* reload - 3 for call
	sub	dx,bx
	mov	ds:[bx].relEntmove1,dx		;* relative jump
pds_nxt:
	add	bx,SIZE ENTMOVE
	loop	pds_lp

;*	* set segment reference byte
	dec	al
	xor	ah,ah
	add	ax,ds:[neLom.ne_psegrefbytes]
	mov	bx,ax
	mov	byte ptr ds:[bx],segrefDiscarded

	mov	fLockoutSweep,0

cEnd	PatchThunkDiscarded

;*****************************************************************************

;***** LRU SWEEP *****
    assumes ds,DATA
    assumes ss,nothing		;* called by interrupt

;********** SweepLru **********
;*	entry : n/a (DS = DDS)
;*	* if necessary, sweep the LRU
;*	exit : n/a
;*	* NOTE : sweeping the LRU is defined as follows:
;*		1) for all thunks that are direct jumps (ENTMOVE, i.e. JMPF),
;*		    change them to ENTMOVE1 calls to RelruSegment
;*		2) bump all segref bytes by 1, stick if new value negative
;*			NOTE : fixed, discarded or max LRU will not change

cProc	SweepLru,<NEAR,PUBLIC,ATOMIC>
cBegin	SweepLru

    assumes DS,DGROUP

	cmp	levelLru,1			;* <=1 => ignore
ifdef	FOR_QC
	ja	@F
	jmp	sw_end
@@:
else	; !FOR_QC
	jbe	sw_end
endif	; FOR_QC
;*	* if we interrupted internal processing, then ignore interrupt
	test	fLockoutSweep,0ffh
ifdef	FOR_QC
	jz	@F
	jmp	sw_end
@@:
else	; !FOR_QC
	jnz	sw_end
endif	; FOR_QC
;*	* Perform Sweep
	push	ds
	mov	ds,psLom			;* all work in LOM
    assumes ds,NOTHING

;*	* STEP 1 - sweep thunks

	mov	bx,ds:[neLom.ne_rgentmove]
	mov	cx,ds:[neLom.ne_cmovent]
	jcxz	sw_end
	AssertNe cx,0
IFDEF	FOR_QC
	mov	dx,-1
ENDIF	; FOR_QC

swthk_loop:
	Assert	<opcEntmove EQ opcEntmove1>
	cmp	ds:[bx].opcEntmove,opcJmpf	;* JMPF => resident
	jne	swthk_next			;* else not resident
IFDEF	FOR_QC
;*	* See if the segment is bound
	mov	al,ds:[bx].segnoEntmove		;* Get the segment number
	cmp	al,dl				;* Is it the same as the last?
	je	swthk_qc1
	push	bx				;* Save value
	mov	bx,ds:[neLom.ne_psegrefbytes]	;* Point to array of segrefs
	xor	ah,ah				;* Clear byte
	add	bx,ax				;* Offset by segment number
	dec	bx				;* Make zero based
	mov	dh,ds:[bx]			;* Get current value
	mov	dl,al				;* Save segment number
	pop	bx				;* Restore value
swthk_qc1:
	test	dh,080h				;* Is it negative?
	jnz	swthk_next			;* Yes -- bound -- skip
ENDIF	; FOR_QC

;*	* convert from resident to ReLru.
	mov	ds:[bx].opcEntmove1,opcCalln
	mov	ax,ds:[bx].offEntmove		;* resident offset
	mov	ds:[bx].offDestEntmove1,ax	;* non-resident offset
	mov	ax,opcRelruLom-3		;* relru - 3 for call
	sub	ax,bx
	mov	ds:[bx].relEntmove1,ax		;* relative jump

swthk_next:
	add	bx,SIZE ENTMOVE
	loop	swthk_loop

;*	* STEP 2 - sweep seg ref bytes
	Assert	<segrefLoaded EQ 0>
	Assert	<(segrefDiscarded+1) AND 80H>	;* must be negative to stick
	Assert	<(segrefFixed+1) AND 80H>	;*	"	"	"

	mov	cx,ds:[neLom.ne_cseg]
	dec	cx				;* none for DGROUP
	mov	bx,ds:[neLom.ne_psegrefbytes]
	AssertNe cx,0

swref_loop:
	inc	byte ptr ds:[bx]
	jns	swref_next
	dec	byte ptr ds:[bx]		;* overflow => keep at 255
swref_next:
	inc	bx
	loop	swref_loop

	pop	ds
    assumes ds,DATA

IFNDEF NOPCODE
;*	* sweep clear handle table
	push	di
	mov	di,dataOffset $q_mpsnq
	xor	cx,cx
	xchg	cx,ds:[di-2]			;* clear cwClear / get count
	push	ds
	pop	es				;* es:di => entries
	xor	ax,ax
	cld					;* called from interrupt
						;* D flag unknown !
	rep stosw				;* clear table
	pop	di
ENDIF ;!NOPCODE

	mov	levelLru,0			;* not active until Reload or
						;*  Relru is called
sw_end:

cEnd	SweepLru




;********** BindSegment **********
;*	entry : lpfn = pointer to a thunk
;*		fBind => whether to bind or unbind
;*	* Bind or unbind a segment
;*	exit : n/a

cPublic	BindSegment, <>
    parmD lpfn
    parmW fBind
cBegin	BindSegment
    assumes DS,DATA

    assumes DS,DGROUP
	les	bx,lpfn			;* es == psLom

IFDEF DEBUG
;*	* make sure that it is a thunk
	mov	ax,psLom
	mov	cx,es
	cmp	ax,cx
	je	ok_bind
	cCall	CowAssertFailed
	DB	"bad BindSegment call$"
ok_bind:
ENDIF ;DEBUG
;*	* Get segment number to bind/unbind
	xor	dx,dx
	mov	dl,es:[bx+segnoEntmove1]
	mov	bx,dx
	dec	bx
BS10:
	add	bx,es:[neLom.ne_psegrefbytes]

	mov	al,es:[bx]		;* al = segref
	mov	cx,fBind
	jcxz	bind_unbind		;* cl == 0 => just LRU'd

;*	* bind : we may have to reload the segment
	cmp	al,segrefBound
	je	end_bind			;* already bound

	cmp	al,segrefDiscarded
	jne	bind_loaded

	Save	<bx,dx>
	cCall	LoadSegment, <dx>		;* load it
bind_loaded:
	mov	cl,segrefBound
bind_update:	;* cl = segref
	mov	es:[bx],cl

ifdef	FOR_QC
;*	* Walk all thunks for this segment and make jmpfs
	mov	fLockoutSweep,1
	push	ds
	;*	Scan the entry table
	mov	ax,dx			;* Get segment number
	dec	ax			;* Zero base it
	mov	cx,SIZE NEW_SEG1	;* Convert into offset in
	imul	cl			;*    the segment info table
	mov	bx,ax			;*
	add	bx,es:[neLom.ne_segtab]	;* 
	mov	bx,es:[bx].ns_handle	;* Get the segments handle
	mov	es,[pGlobalHeap]	;* Convert handle into segment no
	AssertNe es:[bx].he_flags, HE_DISCARDED
	mov	es,es:[bx].he_address	;*
	mov	ds,[psLom]		;*
	assumes	ds,nothing		;*
	mov	bx,ds:[neLom.ne_rgentmove]
	mov	cx,ds:[neLom.ne_cmovent]
	mov	ax,dx
bs_pld_lp:
	cmp	al,ds:[bx].segnoEntmove		;* Segment in question ?
	jne	bs_pld_nxt
	cmp	ds:[bx].opcEntMove,opcJmpf
	je	bs_pld_nxt
	AssertEq ds:[bx].opcEntmove,opcCalln	;* should have been a call
	mov	ds:[bx].opcEntmove,opcJmpf
	mov	dx,ds:[bx].offDestEntmove1
	mov	ds:[bx].offEntmove,dx
	mov	ds:[bx].segEntmove,es
bs_pld_nxt:
	add	bx,SIZE ENTMOVE
	loop	bs_pld_lp
	pop	ds
	assumes	ds,DGROUP
	mov	fLockoutSweep,0
endif	; FOR_QC

end_bind:
cEnd	BindSegment

bind_unbind:	;* unbind segment
;*	* Note: due to overbinding, the segment could be:
;*	*  normal resident (moveable/discardable), bound or discarded
	cmp	al,segrefDiscarded
	je	end_bind			;* keep discarded
	AssertEQ cx,0
	jmp	short bind_update		;* just re-lru



IFDEF KERNEL_SWAP_STACK
;* the following is QC specific

;********** BindPS **********
;*	entry : ps = physical segment to bind
;*		fBind => whether to bind or unbind
;*	* Bind or unbind a segment
;*	exit : n/a

cPublic	BindPS,<>
    parmW ps
    parmW dummy
    parmW fBind
cBegin	BindPS
    assumes DS,DGROUP
	push	si

	mov	es,psLom
	mov	si,es:[neLom.ne_segtab]
	mov	cx,es:[neLom.ne_cseg]
	xor	dx,dx
BPS10:
	mov	bx,es:[si].ns_handle	;* handle or ps
	mov	ax,es:[si].ns_flags	;* 
	test	ax,NSMOVE		;* can it move?
	jz	BPS50			;* no-
	push	es
	mov	es,pGlobalHeap		;*
	test	es:[bx].he_flags,HE_DISCARDED	;* Can't bind a discarded 
					;* segment this way
	mov	bx,es:[bx].he_address	;* dereference
	pop	es
	jnz	BPS50			;* --- TRUE if segment is discarded
	cmp	bx,ps			;* Is this the segment I want?
	je	BPS60			;* Yes -- bind it down
	
BPS50:
	add	si,SIZE NEW_SEG1
	inc	dx
	loop	BPS10
ifdef	DEBUG
	cCall	CowAssertFailed
	DB	"bad BindPS call$"
endif	; DEBUG

BPS60:
	pop	si
	mov	bx,dx
	inc	dx
	jmp	bs10
	
cEnd	BindPS

ENDIF ;KERNEL_SWAP_STACK



;********** UnbindAll **********
;*	entry : n/a (DS != DDS)
;*	* unbind all bound segments (for low memory conditions)
;*	exit : n/a

cProc	UnbindAll, <NEAR, PUBLIC, ATOMIC>, <DS>
cBegin	UnbindAll
    assumes SS,DGROUP
    assumes DS,NOTHING
	mov	ds,psLom
	mov	cx,ds:[neLom.ne_cseg]
	dec	cx				;* cx = # of code segments
	mov	bx,ds:[neLom.ne_psegrefbytes]	;* es:bx => segref array
unbind_loop:
	cmp	byte ptr ds:[bx],segrefBound
	jne	unbind_next
	mov	byte ptr ds:[bx],segrefLoaded	;* unbound
unbind_next:
	inc	bx
	loop	unbind_loop

cEnd	UnbindAll


;********** GetCodeHandle **********
;*	entry : ps = psThunk, ib = ibThunk
;*	* Return a valid code handle for given thunk
;*	* Code segment will be resident on exit
;*	exit : ax == code handle
;*

cProc	GetCodeHandle,<FAR,PUBLIC,ATOMIC>,<SI>
    parmW	ps
    parmW	ib
cBegin	GetCodeHandle
    assumes DS,DGROUP

	mov	es,ps			; es:bx => thunk
	mov	bx,ib

IFDEF DEBUG
	mov	ax,es
	AssertEq ax,psLom		; thunks are in psLom
ENDIF ;DEBUG

	cmp	es:[bx].opcEntmove,opcJmpf
	jnz	not_ENTMOVE

;*	* ENTMOVE	Segment is resident, return ga_handle from arena

	mov	ax,es:[bx].segEntmove
	dec	ax
	mov	es,ax			; es:0 => arena
	mov	ax,es:[ga_handle]
	jmp	short gch_end

not_ENTMOVE:

;*	* ENTMOVE1 or ENTMOVE2
;			Segment may or may not be resident, load segment
;			if necessary, return ns_handle from NEWSEG1

;*	* get ns_handle

	xor	ax,ax
	mov	al,es:[bx].segnoEntmove1
	AssertSet ax,0ffh		; must be non-zero
	mov	cx,ax			; segno in cx for LoadSegment

	dec	ax
	mov	bl,size NEW_SEG1	; bigger speedier method in LoadSegment
	mul	bl
	mov	si,ax

IFDEF DEBUG
	mov	ax,es
	AssertEq ax,psLom		; es should not have changed
ENDIF ;DEBUG

	add	si,es:[neLom.ne_segtab]	; es:si => NEW_EXE1
	mov	si,es:[si].ns_handle	; si == ns_handle
	mov	es,pGlobalHeap		; es:si => handle entry

;*	* if segment is discarded, load it

	test	es:[si].he_flags,HE_DISCARDED
	jz	no_load
	cCall	LoadSegment,<cx>

no_load:
	mov	ax,si

gch_end:

cEnd	GetCodeHandle


;*****************************************************************************


;IFDEF DEBUG	;* Debug entry for thunk info

;*********** GetCodeInfo **********
;*	entry : lpProc = far pointer to a thunk or code segment
;*		lpSegInfo => buffer of size SIZE(NEW_SEG1) + 2 bytes
;*	exit : ax = 0 if not a valid thunk / entry
;*	    else ax != 0, *lpSegInfo filled with seg info (SEGI: see winreq.doc)


cProc	GetCodeInfo,<PUBLIC,FAR>,<ds,si,di>
	parmD	lpProc
	parmD	lpSegInfo
cBegin

    assumes DS,NOTHING
    assumes SS,DGROUP
	lds	si,lpProc
	mov	ax,ds
	cmp	ax,psLom
	jnz	not_a_thunk
;*	* ds:si => thunk
	mov	al,ds:[si].segnoEntmove
save_segi:	;* al = segno (1 based)
	mov	ds,psLom
	dec	ax			;* (dec al) -- make 0 based
	mov	bl,SIZE NEW_SEG1
	mul	bl
	add	ax,ds:[neLom.ne_segtab]
	mov	si,ax			;* source
	les	di,lpSegInfo		;* destination
	mov	cx,SIZE NEW_SEG1 / 2
;;	cld
	rep	movsw
	mov	bx,lomOffset neLom	;* DS:BX => New Exe header
	mov	ax,ds:[bx].ne_align	; Return segment aligment
	stosw
	sub	si,SIZE NEW_SEG1
	sub	di,SIZE NEW_SEG1+2
	cmp	si,ds:[bx].ne_autodata
	jne	gciExit
	mov	ax,ds:[bx].ne_stack
	add	ax,ds:[bx].ne_heap
	add	es:[di].ns_minalloc,ax
gciExit:
	mov	cx,ax
cEnd

not_a_thunk:	;* ds:si => a procedure in a fixed segment
	mov	dx,ds				;* psFixed
	mov	ds,psLom
	mov	cx,ds:[neLom.ne_cseg]
	mov	bx,ds:[neLom.ne_segtab]
	mov	ax,1				;* 1 based segment #
find_fixed_loop:
	cmp	dx,ds:[bx].ns_handle
	je	save_segi
	add	bx,SIZE NEW_SEG1
	inc	al
	loop	find_fixed_loop
	xor	ax,ax				;* failure
	jmp	short gciExit

;ENDIF ;DEBUG


;*****************************************************************************

sEnd	KERNEL

;*****************************************************************************

sBegin	INIT
    assumes CS,INIT
    assumes DS,DATA
    assumes SS,DGROUP


;********** InitThunk **********
;*	entry : n/a
;*	* Initialize the thunk manager
;*	exit : n/a
;*	* NONCONFORMING (trashes SI/DI)

cProc	InitThunk,<PUBLIC,FAR,ATOMIC>
cBegin	InitThunk

    assumes ds,dgroup
	push	ds
	mov	ds,psLom			;* assumed throughout
    assumes ds,nothing
	xor	di,di

;*	* Patch near calls to far calls
;*	* opcodes
	mov	al,opcJmpf
	mov	ds:[di].opcReloadLom,al
	mov	ds:[di].opcRelruLom,al
	mov	ds:[di].opcRetThunkLom,al
;*	* segments
	mov	ax,SEG kernelBase		;* KERNEL FIXED segment
	mov	ds:[di].psReloadLom,ax
	mov	ds:[di].psRelruLom,ax
	mov	ds:[di].psRetThunkLom,ax
;*	* offsets
	mov	ds:[di].offReloadLom,kernelOffset ReloadSegment
	mov	ds:[di].offRelruLom,kernelOffset RelruSegment
	mov	ds:[di].offRetThunkLom,kernelOffset ReturnThunk
;*	* First re-format the entry table to contain reload pointers
;*	* The loader has put CALLN opcodes in the entries that are not resident
	mov	si,ds:[neLom.ne_rgentmove]
	mov	cx,ds:[neLom.ne_cmovent]
	jcxz	thi_done
thi_lp:
	mov	al,ds:[si].opcEntmove		;* opcJmpf or opcCallf
	cmp	al,opcJmpf
	je	thi_nxt
	AssertEq al,opcCalln
;*	* insert CALLN to ReloadSegment
	mov	ax,opcReloadLom-3		;* where to jump to - 3 for jump
	sub	ax,si				;* relative offset
	mov	ds:[si].relEntmove1,ax
thi_nxt:
	add	si,SIZE ENTMOVE
	loop	thi_lp

;*	* Next, fill in the return thunk table
;*	* The loader has done nothing but allocate this.
	mov	cx,ds:[neLom.ne_cseg]
	dec	cx				;* ignore DGROUP
	mov	si,ds:[neLom.ne_pretthunks]
	mov	dl,1				;* 1 based seg #
thi2_lp:
	mov	ds:[si].opcEntret,opcCalln
	mov	ax,opcRetThunkLom-3		;* where to jump to - 3 for jump
	sub	ax,si				;* relative offset
	mov	ds:[si].relEntret,ax
	mov	ds:[si].segnoEntret,dl
	inc	dl
	add	si,SIZE ENTRET
	loop	thi2_lp

    assumes ds,data
thi_done:
	pop	ds

cEnd	InitThunk


sEnd	INIT

;*****************************************************************************


	END

