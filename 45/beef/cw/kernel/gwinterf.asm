;*
;*	COW : Character Oriented Windows
;*
;*	gwinterf.asm : Global memory allocator - Windows interface procs

	.xlist
	include kernel.inc
	include galloc.inc
	.list


IFDEF DEBUG

CheckHeap MACRO n
local	ok
	cmp	fCheckCwHeap,0
	je	ok
	cCall	CheckGlobalHeap
	or	ax,ax
	jz	ok
	push	ax
	push	dx
	cCall	<far ptr PrintGlobalHeap> 	;*!! look at heap on exit
	pop	dx
	pop	ax			;* restore ax to see why we died
	cCall	CowAssertFailed
	DB	"&n"
	DB	":invalid global heap$"
ok:
ENDM
ELSE
CheckHeap MACRO n
ENDM
ENDIF


sBegin	DATA

externW     <hGlobalHeap,pGlobalHeap>

IFNDEF NOPCODE
externW     <$q_mpsnq>			;* HandleTable - 2
ENDIF ;!NOPCODE

IFDEF DEBUG
staticB  szThunkFormat, <"Thunk %x: sf=%x, ow=%x, sz=%x, nx=%x",0dh,0ah,0>
globalB	fCheckCwHeap,1
ENDIF ;DEBUG

IFDEF	WINDOWS_OLD_APP
externW	<fWoaPresent>
ENDIF	; WINDOWS_OLD_APP

IFDEF	DUAL
IFDEF	DEBUG
externW	fProtectMode
ENDIF	;DEBUG
ENDIF	;DUAL

sEnd	DATA


IFDEF	WINDOWS_OLD_APP
externFP	<WoaRelease>			;* User supplied
ENDIF	; WINDOWS_OLD_APP

IFDEF DEBUG
externFPublic <_dprintf>			;* in assertsw.c
ENDIF ;DEBUG


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING			;* DS usually points to MOB
    assumes SS,DATA			;* get variables from here


externNP    <PatchStackMoved, PatchStackDiscarded>
externNP    <PatchThunkMoved, PatchThunkDiscarded>	;* ldthunk.asm

IFDEF DEBUG
externNP    <CheckGlobalHeap>			;* lddebug.asm
ENDIF ;DEBUG

IFDEF	KEEP_SYMDEB_CODE
externNP    <DebugMovedSegment, DebugFreeSegment>	;* lddebug.asm
ENDIF	; KEEP_SYMDEB_CODE

;*	* low level subroutines
externNP    <galloc,grealloc>		; GMEM.ASM
externNP    <gfree>			; GMEM.ASM
externNP    <gdref,ghandle,galign>	; GMEM.ASM
externNP    <gfindfree> 		; GALLOC.ASM
externNP    <gcompact>			; GCOMPACT.ASM
externNP    <halloc,hthread>		; HANDLE.ASM


	PUBLIC	genter
	PUBLIC	gnotify
	PUBLIC	ghexpand
	PUBLIC	xhandle

IFDEF DEBPUB
	PUBLIC	gavail, gbtop
	PUBLIC	gmemfail, gmemcheck
ENDIF ;DEBPUB

SUBRS	PROC	NEAR


; Subroutine to enter a critical region for the global heap.
;
;   Output:	DS:DI = address of GlobalInfo for global heap
;*	* NOTE : gi_lrulock is not needed due to simplified LRU
;*	* and accordingly gleave is not needed !
;
genter:
	mov	ds,pGlobalHeap
	xor	di,di
	ret

; Subroutine to convert a 32-bit byte count to a 16-bit paragraph count.
;
;   Inputs:	AX = allocation flags or -1 if called from GlobalCompact
;		BX = stack address of 32-bit unsigned integer
;		DX = handle being reallocated or zero
;		DS:DI = address of GlobalInfo for global heap
;
;   Outputs:	AX = updated allocation flags
;		BX = #paragraphs needed to contain that many bytes
;		CX = owner value to use
;		DX = handle being reallocated or zero
;
gbtop:
	push	dx
	mov	dx,ss:[bx+2]
	mov	bx,ss:[bx]
	mov	cx,4
	add	bx,15
	adc	dx,0
	jnc	gbtop1
	dec	dx
	dec	bx
gbtop1:
	shr	dx,1
	rcr	bx,1
	loop	gbtop1
	pop	dx
	inc	ax
	jz	gbtopx		; All done if called from GlobalCompact
	dec	ax

	mov	[di].gi_cmpflags,al	; Save flags for gcompact
	test	dl,GA_FIXED		; Are we reallocating a fixed handle?
	jnz	gbtop4			; Yes, then go allocate low
	or	dx,dx			; Reallocating a moveable handle?
	jnz	gbtop3a 		; Yes, allocate high
	test	al,GA_MOVEABLE		; Is this a moveable request?
	jz	gbtop4			; No, then go allocate low
gbtop3a:
	or	al,GA_ALLOCHIGH 	; Yes, allocate high
	cmp	[di].gi_reserve,di	; Reserve area for code?
	je	gbtop4			; No, all moveable allocated high
	test	al,GA_DISCCODE		; Yes, is this discardable code?
	jnz	gbtop4			; Yes, then allocate high
	xor	al,GA_ALLOCHIGH 	; No, then allocate low
gbtop4:
	test	ah,HE_DISCARDABLE	; Discard level?
	jz	gbtop4a
	and	ah,not HE_DISCARDABLE	; Yes, convert to boolean value
	or	ah,GA_DISCARDABLE
gbtop4a:
	mov	cl,GA_SEGTYPE		; Copy segment type bits to handle
	and	cl,al
	or	ah,cl			; table flags
;*	* Figure out the owner field
	or	dx,dx			;* reallocating ?
	mov	cx,ownerData		;* owner for all global data
	jz	gbtopx			;* new block => global data
;*	* use the old owner field
	xchg	dx,si
	mov	cx,[si].he_address	;* owner or ps
	test	[si].he_flags,HE_DISCARDED
	jnz	owner_discarded 	;* owner in cx
;*	* we must get ga_owner field from resident block
	mov	es,cx
	mov	cx,es:[di].ga_owner	;* old owner field
owner_discarded:
	xchg	dx,si			;* restore si/dx
gbtopx:
	ret

; Subroutine to get the available memory.
;
gavail:
	mov	byte ptr [di].gi_cmpflags,0
	call	gcompact
	or	dx,dx
	jz	gavail1
	jmp	gcsize
gavail1:
	push	dx			    ; No max yet
	mov	es,[di].hi_first
gcstat:
	cmp	es:[di].ga_sig,GA_ENDSIG    ; Last block?
	je	gcstat3 		    ; Yes all done
	mov	es,es:[di].ga_next	    ; Next block
	cmp	es:[di].ga_owner,di	    ; Free?
	je	gcstat0 		    ; Yes
	mov	si,es:[di].ga_handle
	or	si,si			    ; Fixed?
	jz	gcstat			    ; Yes, next block
	cmp	[si].he_count,0 	    ; Locked?
	jne	gcstat			    ; Yes, next block
	test	[si].he_flags,GA_DISCARDABLE	; Discardable?
	jz	gcstat			    ; No, next block
	test	es:[di].ga_flags,GA_DISCCODE; No, discardable code?
	jnz	gcstat			    ; Yes, next block
gcstat0:
	push	es			    ; Save starting point
	mov	ax,es:[di].ga_size	    ; Use this size
	mov	cx,[di].hi_count
gcstat0a:
	mov	es,es:[di].ga_next	    ; Next block
	cmp	es:[di].ga_owner,di	    ; Free?
	je	gcstat1 		    ; Yes, include size
	cmp	es:[di].ga_sig,GA_ENDSIG    ; End of arena?
	je	gcstat0b		    ; Yes, handle reserve area
	mov	si,es:[di].ga_handle
	or	si,si			    ; Fixed?
	jz	gcstat2 		    ; Yes, stop looking
	cmp	[si].he_count,0 	    ; Locked?
	jne	gcstat2 		    ; Yes, stop looking
	test	[si].he_flags,GA_DISCARDABLE	; Discardable?
	jz	gcstat1a		    ; No, dont include in count then
	test	es:[di].ga_flags,GA_DISCCODE; Yes, discardable code?
	jz	gcstat1 		    ; No, include in count then
gcstat0b:
	mov	si,es			    ; Yes, see if we are trying
	sub	si,[di].hi_last 	    ; to steal some of the reserve area
	neg	si
	sub	si,[di].gi_reserve
	inc	ax			    ; add in one arena header
	add	ax,si			    ; Adjust amount by reserve area
	jz	gcstat2
	dec	ax			    ; remove one arena header
	jmp	short gcstat2
gcstat1:				    ; Free or Discardable
	add	ax,es:[di].ga_size	    ; Increase availabe space
	inc	ax			    ; by size of this block
gcstat1a:				    ; Moveable
	loop	gcstat0a
gcstat2:
	pop	es			    ; Back to starting point
	cmp	ax,dx			    ; Did we find a bigger block?
	jbe	gcstat			    ; No, then look again
	mov	dx,ax			    ; Yes, remember size
	pop	ax
	push	es			    ; ...and block
	jmp	gcstat
gcstat3:
	pop	es
	mov	ax,dx
gcsize:
	or	ax,ax		    ; zero mem available?
	jz	gcfinal 	    ; yes, return 0
	dec	ax		    ; Dont be too exact
	jz	gcfinal
	dec	ax
gcfinal:
	and	al,GA_MASK	    ; round down to nearest alignment
	xor	dx,dx
	ret

;********** gnotify **********
; Subroutine to update all info if a global object moved / discarded
;
;   Inputs:	AL = message code (GN_MOVE, GN_DISCARD)
;		BX = handle
;		CX = optional argument (new ps, discard flags)
;*		DX = original owner field
;*		DS = pGlobalHeap
;
;   Outputs:	n/a
;
;   Destroys:	AX,BX,CX,DX,ES
;
gnotify:
IFDEF DEBUG
;*	* Test for valid owner
	or	dh,dh
	jz	ok_notify_owner 		;* code segment
	cmp	dx,ownerData
	jz	ok_notify_owner 		;* data segment
	cmp	dx,-1
	jz	ok_notify_owner 		;* master object
bad_notify_owner:
	cCall	CowAssertFailed
	DB	"gnotify : bad owner$"
ok_notify_owner:
ENDIF ;DEBUG

	xor	ah,ah
	push	si
	push	di

IFNDEF NOPCODE
	Save	<ax,bx,cx,dx>
	cCall	FixHandleTable
ENDIF ;NOPCODE

	mov	di,cx				;* di = new ps / discard flags
	mov	cx,dx				;* cx = owner
	AssertReset bl,GA_FIXED 		;* MUST BE MOVEABLE
					;* never discard FIXED objects
	mov	si,[bx].he_address
	AssertNe si,0				;* the old block must exist
	Assert	<GN_MOVE EQ 1>
	dec	ax
	jnz	notify_discard

;*	* MOVING :
	AssertNe di,0				;* di = new destination ps
	mov	[bx].he_address,di		;* update handle address
IFDEF KEEP_SYMDEB_CODE
	Save	<cx>
	cCall	DebugMovedSegment,<si,di>
ENDIF ; KEEP_SYMDEB_CODE
;*	* Test to see if we moved a code segment
	or	ch,ch				;* upper byte zero for CODE
	jnz	notify_move_data
;*	* moving code
	Save	<cx>
	cCall	PatchThunkMoved,<cx,di>
	cCall	PatchStackMoved,<cx,si,di>
	jmp	short notify_exit

notify_move_data:
;*	* Test to see if MOB moved
	mov	ax,ds
	cmp	ax,si			    ; Did we move DS?
	jne	notify_exit		    ; No, continue
;*	* We have moved the Master Object
	push	di				; update DS pointer.
	pop	ds
	mov	pGlobalHeap,ds			;* and the master object pointer
	jmp	short notify_exit

notify_discard:
	Assert	<GN_DISCARD EQ 2>
	AssertEq ax,1

;*	* DISCARDING :
	mov	[bx].he_address,0		;* discarded
IFDEF KEEP_SYMDEB_CODE
	Save	<cx>
	cCall	DebugFreeSegment,<si>
ENDIF ; KEEP_SYMDEB_CODE
	or	ch,ch				;* upper byte zero for CODE
	jnz	notify_exit			;* discarding data

;*	* Discarding code
	Save	<cx>
	cCall	PatchThunkDiscarded,<cx>
	cCall	PatchStackDiscarded,<cx,si>

notify_exit:
	pop	di
	pop	si
	ret


; Procedure to expand a global handle table
;
;   Inputs:	CX = #handle table entries to expand the table by
;		DS:DI global info structure
;
;   Outputs:	AX = address of handle table block of requested size
;
;   Destroys:	ES,BX,CX,DX
;
ghexpand:
	mov	ax,ds
	push	cx
	call	ghandle
	pop	cx
	jz	ghfail
	mov	dx,ax
	xor	ax,ax
	mov	bx,DS:[di].hi_htable
	mov	bx,DS:[bx].ht_count
	inc	bx
	add	bx,cx
	shl	bx,1
	shl	bx,1
	add	bx,DS:[di].hi_htable
	jc	ghfail
	add	bx,15
	jc	ghfail
	mov	cl,4
	shr	bx,cl
	; Dont allow master object into reserve swap area
	and	byte ptr [di].gi_cmpflags,not GA_DISCCODE
	call	grealloc
	jcxz	ghfail
	mov	dx,ax
	call	gdref
	jz	ghfail
	mov	bx,ES:[di].ga_size
	mov	cl,4
	shl	bx,cl
	sub	bx,HE_ALIGN
	and	bl,HE_MASK
	sub	bx,DS:[di].hi_htable
	mov	cl,2
	shr	bx,cl
	mov	ax,bx
	mov	bx,DS:[di].hi_htable
	mov	cx,ax
	xchg	DS:[bx].ht_count,ax
	sub	cx,ax
	inc	bx
	inc	bx
	shl	ax,1
	shl	ax,1
	add	bx,ax
	xchg	bx,di
	call	hthread
	mov	DS:[di],cx
	mov	di,bx
	mov	cx,ax
	ret
ghfail:
	xor	cx,cx
	ret

gmemcheck:
	or	ax,ax
	jz	gmemfail
	ret
gmemfail:
	ret


SUBRS	ENDP


IFNDEF NOPCODE
;********** FixHandleTable **********
;    Inputs:	AL = message code (GN_MOVE, GN_DISCARD)
;		BX = handle
;		CX = optional argument (new ps, discard flags)
;*		DS = pGlobalHeap
;*	* Fix Handle Table
;*	exit : n/a - Trashes AX,BX,CX,DX,DI,ES

cProc	FixHandleTable,<NEAR,ATOMIC>
cBegin	FixHandleTable

;*	* find new address
	cmp	al,GN_MOVE
	je	move_in_handle_table
	xor	cx,cx				;* new ps is 0
move_in_handle_table:

	mov	ax,ss
	mov	es,ax				;* set ES to DDS

;*	* find old address
	mov	ax,ds:[bx].he_address

;*	* set up for test : ax = old ps, cx = new ps
	mov	dx,cx				;* dx = psNew
	mov	di,dataOffset $q_mpsnq
	mov	cx,DGROUP:[DI-4]		;* count of entries

;*	* scan till old value matched
	cld
fix_ht_loop:
	jcxz	fix_ht_end
	repne	scasw
	jne	fix_ht_end
	mov	es:[di-2],dx
	jmp	fix_ht_loop
fix_ht_end:

cEnd	FixHandleTable

ENDIF ;!NOPCODE

;*****************************************************************************

; The remainder of this file implements the exported interface to the
; global memory manager.
;
;   DWORD	far PASCAL GlobalSize( HANDLE );
;   HANDLE	far PASCAL GlobalAlloc( WORD, DWORD );
;   HANDLE	far PASCAL GlobalReAlloc( HANDLE, DWORD, WORD );
;   HANDLE	far PASCAL GlobalFree( HANDLE );
;   DWORD	far PASCAL GlobalCompact( DWORD );
;   #define GlobalDiscard( h ) GlobalReAlloc( h, 0L, GMEM_MOVEABLE )
;   HANDLE	far PASCAL GlobalHandle( WORD );
;


;
; Procedure return the handle for a global segment.
;
;   Inputs:	Stack = sp   -> near return return address of ghandle
;			sp+2 -> far return return address of caller
;			sp+6 -> segment address parameter
;
;   Outputs:	AX = handle or zero if invalid segment address
;		Old DS,DI have been pushed on the stack
;		Z flag set if invalid or fixed segment.  O.W. Z flag
;		reset and BX = pointer to handle table entry
;			  CX = flags and count word from handle table
;			  DX = segment address
;			  ES:DI = arena header of object
;			  DS:DI = master object segment address
;
;
;
xhandle PROC	NEAR
	pop	dx		    ; Get near return address
	mov	bx,sp		    ; Get seg parameter from stack
	mov	ax,SS:[bx+4]
	inc	ax		    ; Is it -1?
	jz	xh2		    ; Yes, handle special
	dec	ax		    ; No, restore AX
xh1:
	push	ds		    ; Save DS:DI
	push	di
	mov	ds,pGlobalHeap		; Point to master object
	xor	di,di
	inc	ds:[di].gi_lrulock
	push	dx		    ; Call ghandle and return
	jmp	ghandle 	    ; to our caller directly
xh2:
	mov	ax,ds		    ; If passed -1 use callers DS
	jmp	xh1
xhandle ENDP


cPublic GlobalHandle
;   parmW   seg
cBegin	nogen
gh0:
	call	xhandle
xhandlex:
	dec	ds:[di].gi_lrulock
	pop	di
	pop	ds
	ret	2
cEnd	nogen ;GlobalHandle


cPublic GlobalSize
;   parmW   h
cBegin	nogen
	call	xhandle 		; Call ghandle with handle in DX
	or	dx,dx			; Did we get a segment address?
	jz	xhandlex		; No, all done then
	mov	ax,ES:[di].ga_size	; Yes, get size in paragraphs
	push	ax
	xor	dx,dx			; Returning a long result
	mov	cx,4
gs2:
	shl	ax,1
	rcl	dx,1
	loop	gs2
	pop	cx			; Return number paragraphs in CX
	jmp	short xhandlex
cEnd	nogen ;GlobalSize


cPublic GlobalAlloc,<>,<ds,si,di>
    parmW   flags
    parmD   nbytes
cBegin
	CheckHeap   GlobalAlloc
IFDEF	WINDOWS_OLD_APP
	cmp	fWoaPresent,0
	jz	GA10
	xor	ax,ax
	cCall	WoaRelease,<nbytes,ax>
GA10:
ENDIF	; WINDOWS_OLD_APP
; EnterCrit
	call	genter			; About to modify memory arena
	xor	dx,dx			; No handle
	mov	ax,flags		; Allocate space for object
	lea	bx,nbytes		; Convert requested bytes to paragraphs
	call	gbtop			; ... into BX
	call	galloc
	call	gmemcheck
cEnd	GlobalAlloc


cPublic GlobalReAlloc,<>,<ds,si,di>
    parmW   h
    parmD   nbytes
    parmW   rflags
cBegin
	CheckHeap   GlobalReAlloc

IFDEF	WINDOWS_OLD_APP
	cmp	fWoaPresent,0
	jz	GR10
	cCall	GlobalSize,<h>
	sub	ax, WORD PTR (nbytes)
	sbb	dx, WORD PTR (nbytes+2)
	jnc	GR10
	xor	ax,ax
	cCall	WoaRelease,<nbytes,ax>
GR10:
ENDIF	; WINDOWS_OLD_APP

; EnterCrit
	call	genter		    ; About to modify memory arena
	mov	dx,h
	mov	ax,rflags
	lea	bx,nbytes	    ; Convert requested bytes to paragraphs
	call	gbtop		    ; ... into BX
	call	grealloc	    ; Reallocate global object
	call	gmemcheck
cEnd	GlobalReAlloc


cPublic GlobalFree,<>,<ds,si,di>
    parmW   h
cBegin
	CheckHeap   GlobalFree
; EnterCrit
	call	genter		    ; About to modify memory arena
IFDEF DEBUG
	mov	dx,h
	call	gdref
	or	si,si
	jz	free_ok
	cmp	ch,00h		    ; Debugging check for count underflow
	je	free_ok
	cCall	CowAssertFailed
	DB	"GlobalFree: freeing locked object$"
free_ok:
ENDIF ;DEBUG
	mov	dx,h		    ; Free handle
	xor	cx,cx		    ; Dont check owner field
	call	gfree
cEnd	GlobalFree


cPublic GlobalCompact,<>,<ds,si,di>
    parmD   minBytes
cBegin
IFDEF	DUAL
IFDEF	DEBUG
	mov	cx,fProtectMode		; real mode only!
	jcxz	@F
	BREAKPOINT			; m3 would call cowassertfaileddos3
@@:
ENDIF
ENDIF
	CheckHeap   GlobalCompact
; EnterCrit
	call	genter		    ; About to modify memory arena
	mov	ax,-1
	lea	bx,minBytes
	call	gbtop
	clc			    ; galign should be called with carry clear
	call	galign
	call	gavail		    ; Returns paragraphs in DX:AX
	mov	cx,4		    ; Convert paragraphs to bytes
	push	ax
gcsize1:
	shl	ax,1
	rcl	dx,1
	loop	gcsize1
	pop	cx		    ; let caller to jcxz to test failure
cEnd	GlobalCompact


;*****************************************************************************
;* Debug support

IFDEF	DEBUG

;********** PrintGlobalHeap **********
;*	entry : n/a
;*	* print the contents of the global heap
;*	exit : n/a
;*	* NOTE : this code must be fixed since it throws everything out.

cProc	PrintGlobalHeap, <FAR, PUBLIC, ATOMIC>, <DS,ES,SI,DI,AX,BX,CX,DX>
cBegin	PrintGlobalHeap

	mov	ax,DGROUP
	mov	ds,ax
    assumes DS,DGROUP

;*	* compact the heap (removing any free gaps) -- throw out code as well

	mov	es,pGlobalHeap
	xor	di,di

	mov	es,es:[di].hi_first	 ;* es => first block.
;*	* scan from end till finding free block

Print_next_thunk:
	xor	ah,ah			;get all pieces to print.
	mov	al,es:[di].ga_sig	; Put sig with flags
	mov	ah,es:[di].ga_flags
	mov	bx,es:[di].ga_owner
	mov	cx,es:[di].ga_size
	mov	dx,es:[di].ga_next	;
	mov	si,offset dgroup:szThunkFormat

	push	es
?PLM = 0
	cCall	dprintf, <si,es,ax,bx,cx,dx>
?PLM = 1
	pop	es

	mov	ax,es
	cmp	ax,dx			;see if at end of list
	jz	end_print		; yes lets get out
	mov	es,dx			; no move to segment
	jmp	Print_next_thunk

end_print:

cEnd	PrintGlobalHeap
ENDIF ;DEBUG


;*****************************************************************************

sEnd	KERNEL

	END
