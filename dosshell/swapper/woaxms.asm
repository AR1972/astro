;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file contains all the modules necessary for managing the old app XMS  ;
; requiements.	This code is an optional part of the real mode stub segment, ;
; and will be discarded if not needed.					     ;
;									     ;
; History:								     ;
;									     ;
;	 10-Oct-90 AC		PrevXmm handler allocates all handle numbers ;
;      	 20-Sep-90 AC		Functions that don't succeed are chained on. ;
;	 20-Sep-90 AC		Hooks XMS properly.			     ;
;	 7-Mar-90  jimmat	Now part of real mode stub instead of being  ;
;				a seperate segment.			     ;
;        Mon June-26-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created. (Added the History legend)				     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	njmp.mac
	include macros.mac
	.list

	.286p

;----------------------------------------------------------------------------;
; define two macros for doing near and far returns.			     ;
;----------------------------------------------------------------------------;

near_ret  macro
	local	dummy
dummy	proc	near
	ret	
dummy	endp
	endm

far_ret	macro
	local	dummy
dummy	proc	far
	ret
dummy	endp
	endm
;----------------------------------------------------------------------------;
createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

	assumes cs,StubSeg

; XmsStartLine MUST be at the very start of the XMS code/data.	This defines
; the address which will be used to discard the XMS code/data if the
; current DOS app does not require XMS support.

	public	XmsStartLine

XmsStartLine	equ	$	;MAKE ME FIRST!  Everything after this will
				;  be discarded if XMS isn't needed.

;----------------------------------------------------------------------------;
; define the external function calls.		          		     ;
;----------------------------------------------------------------------------;

	;-------------------------------------------------------;
	; external OLDAPP procedures.			        ;
	;-------------------------------------------------------;

;----------------------------------------------------------------------------;
; declare the variables which are defined elsewhere.			     ;
;----------------------------------------------------------------------------;

externD	ActualXMS			;XMS entry point.
externB	WoafXmsInstalled		;XMS installed or not

;----------------------------------------------------------------------------;
; define the variables that are passed into it at load time.		     ;
;----------------------------------------------------------------------------;

GlobalW	XmsBaseHandle,?			;handle of extended memory block
GlobalD AppXmsBase,?			;base of the apps XMS area
GlobalD AppXmsSize,?			;size of the apps XMS area
GlobalB	WoaAppUsesXMS,0			;whether app allocates XMS or not

;----------------------------------------------------------------------------;
; define variables that are needed for hooking the XMS chain and chaining on ;
;----------------------------------------------------------------------------;

XmmControlBase		dd     -1	;node that is being patched up
XmmControlJmpVal	db	?	;offset of near jump
PrevXmm			dd	?	;for chaining down

;----------------------------------------------------------------------------;
; declare public labels.						     ;
;----------------------------------------------------------------------------;

	public	GetLargestFreeBlock			
	public	AllocateXmsBlock
	public	FreeXmsBlock
	public	MoveXmsBlock
	public	LockXmsBlock
	public	UnlockXmsBlock
	public	GetXmsHandleInfo
	public	RellocateXmsBlock
	public	XmsDispatcher
	public  AllocationTable

;----------------------------------------------------------------------------;
; now define the data structures to be used for manipulating the XMS areas.  ;
;----------------------------------------------------------------------------;

; the main table for managing the handle entries has the following format:


 ;----------|------------------------|------------|---------------------;
 ; Handle   |   32 bit base address  | lock count | size of block in K  ;
 ; (byte)   |       (dword)          |  (byte)    |       (word)        ;
 ;----------|------------------------|------------|---------------------;

; declare a structure for the above table

XmsTableEntry	STRUC

THandle		dw	?		;handle in the table
TBaseLo		dw	?		;loword of base
TBaseHi		dw	?		;high word of base
TLckCount	db	?		;lock count
TSize		dw	?		;size of block

XmsTableEntry	ENDS

; if the handle field is zero, the block is a free block. The Allocation 
; table can have a maximum of 32 + 32 entries and will always be kept sorted
; on the linear address.Allocate one extra entry for the end of table tag,
; which will have a 0ffh in the handle field

AllocationTable	db	(65 * (SIZE XmsTableEntry)) dup (0) ;reserve max size for table

;----------------------------------------------------------------------------;
; define other variables that are needed.				     ;
;----------------------------------------------------------------------------;

ActualInt2f		dd	0  	;original int 2f vector
ExtraMoveOffsetLo	dw	?	;low word of extra offset
ExtraMoveOffsetHi	dw	?	;high word of extra offset
MoveStructure		db 16 dup (?)   ;the move parameter block

MoveTemplate STRUC

Mlength		dd	?		;length of transfer
MSrcHandle	dw	?		;source handle
MSrcOffsetLo	dw	?		;low source offset
MSrcOffsetHi	dw	?		;high source offset
MDstHandle	dw	?		;destination handle
MDstOffsetLo	dw	?		;lo dst offset
MDstOffsetHi	dw	?		;hi dst offset

MoveTemplate ENDS



;----------------------------------------------------------------------------;
; define the dispatch table for the XMS calls that we trap.		     ;
;----------------------------------------------------------------------------;

XmsTraps	label byte

		db	01h		;request high memory area
		dw	XmsFunc01	;routine which handles it
		db	02h		;release high memory area
		dw	XmsFunc02	;routine which handles it
		db	08h		;query free extended memory
		dw	XmsFunc08	;routine which handles it
		db	09h		;allocate extended memory block
		dw	XmsFunc09	;routine which handles it
		db	0ah		;free extended memory block
		dw	XmsFunc0a	;routine which handles it
		db	0bh		;move extended meory block
		dw	XmsFunc0b	;routine which handles it
		db	0ch		;lock extended memory block
		dw	XmsFunc0c	;routine which handles it
		db	0dh		;unlock extended memory block
		dw	XmsFunc0d	;routine which handles it
	     	db	0eh		;get EMB handle information
		dw	XmsFunc0e	;routine which handles it
		db	0fh		;rellocate extended memory block
		dw	XmsFunc0f	;routine which handles it
		db	10h		;request upper memory block
		dw	XmsFunc10	;routine which handles it
		db	11h		;release upper memory block
		dw	XmsFunc11	;routine which handles it
XmsDefaultCode	db	?		;put AH here on entry to trap default
		dw	XmsDefaultFunc	;pass on to original handler

;----------------------------------------------------------------------------;
; XmsInit:								     ;
;									     ;
; This routine is called from the real mode stub, and it initializes the     ;
; allocation table to account for one big free block and hooks in the our    ;
; XMS handler. 
;----------------------------------------------------------------------------;


cProc	XmsInit,<NEAR,PUBLIC,PASCAL>

cBegin	

	smov	ds,cs			;make ds to point to XMS segment
	assumes ds,StubSeg


; the address of current XMS handler is already in 'ActualXms'
; first get the offset from the start of the actual XMS block to the start of
; the apps XMS block

	mov	dx,XmsBaseHandle	;the handle of the actual block
	mov	ah,0ch			;lock handle call
	call	[ActualXms]		;get base in  DX:BX
	pushem	di,cx			;save
	mov	cx,wptr [AppXmsBase]	;get loword of apps base
	mov	di,wptr [AppXmsBase+2]	;get the high word
	sub	cx,bx			;subtract the low word
	sbb	di,dx			;then the high word
	mov	ExtraMoveOffsetLo,cx	;save loword of exttra offset
	mov	ExtraMoveOffsetHi,di	;save hiword of move offset
	popem	di,cx			;restore
	mov	dx,XmsBaseHandle	;get the base handle
	mov	ah,0dh			;unlock call
	call	[ActualXms]		;unlock the block

; we access the table without the structure tags, make sure what we do is
; ok

	.errnz	THandle    - 0
	.errnz	TBaseLo    - 2
	.errnz	TBaseHi    - 4
	.errnz	TLckCount  - 6
	.errnz	TSize	   - 7

; initialize the allocation table to have one big free block.

	push	es			;save
	smov	es,ds			;have own segment
	mov	di,StubSegOFFSET AllocationTable
	xor	ax,ax			;first entry is free.
	stosw				;mark as free
	mov	ax,wptr [AppXmsBase]	;get low word of base
	stosw				;save it
	mov	ax,wptr [AppXmsBase+2]	;get hiword of base
	stosw				;save it
	xor	al,al			;set lock count to 0
	stosb				;save lock count
	mov	dx,wptr [AppXmsSize+2]	;get high word of size
	mov	ax,wptr [AppXmsSize]	;get low word of size
	mov	bx,1024			;need to get size in K
	div	bx			;ax has the quotient
	stosw				;save the size
	mov	ax,0ffffh 		;mark for end of table tag
	stosw	 			;store it in next entries handle field
	pop	es			;restore

; now hook ourselves to the XMS chain.

	call	HookOurXmsCode		;hooks the vectors

; initialization done

XmsInitRet:

cEnd
;----------------------------------------------------------------------------;
; XmsHandler:							             ;
; 									     ;
; This is our own XMS handler dispatcher. It filters out the functions we    ;
; want to trap and passes the rest of them to the actual XMS handler.	     ;
;----------------------------------------------------------------------------;

XmsHandler  proc far

	jmp	short XmsDispatcher	;jump to actual code
	nop				;dummy nops for hookability
	nop				;in case some one hools after us
	nop				;could be bad for them

XmsDispatcher:

	pushf
	push	bp			;save before trashing
	push	ds			;save ds
	smov	ds,cs			;have own segment
	mov	cs:[XmsDefaultCode],ah	;jam current code in
	mov	bp,StubSegOFFSET XmsTraps
	sub	bp,3			;each entry of 3 bytes

; search for the address of the function which handles the call

@@:

	add	bp,3			;look at next entry
	cmp	ah,cs:[bp]		;matches code ?
	jnz	@b			;continue searching

;  jump off to the function, BP is pushed on stack

	
	push	wptr cs:[bp+1]		;save jump address
	mov	bp,sp			;will access stack with it
	add	bp,4			;ss:[bp] has original bp
	near_ret			;jump to the handler

XmsHandler	endp

	;----------------------------------------------------------;
	; XMS function handler top level routines are defined next ;
	;----------------------------------------------------------;


	;----------------------------------------------------------;
	; This one handles all calls that are not trapped by WOA   ;
	;----------------------------------------------------------;

XmsDefaultFunc:

	pop	ds			;restore ds
	pop	bp			;restore bp
	popf
	jmp	cs:[PrevXmm]		;invoke actual handler

	;----------------------------------------------------------;
	; This one is used to return back to caller		   ;
	;----------------------------------------------------------;

XmsRet:

	pop	ds			;restore ds
	pop	bp			;retore bp
	popf
   	far_ret				;go back to caller

	;----------------------------------------------------------;
	; Request High Memory area:			           ;
	;							   ;
	; we return saying HMA is already in use.		   ;
	;----------------------------------------------------------;

XmsFunc01:

	xor	ax,ax			;cannot allocate HMA
	mov	bl,91h			;HMA already in use
	jmp	XmsRet			;go back to caller

	;----------------------------------------------------------;
	; Release High Memory area:			           ;
	;							   ;
	; we return saying HMA was not allocated		   ;
	;----------------------------------------------------------;

XmsFunc02:

	xor	ax,ax			;unsuccessful realease
	mov	bl,93h			;as it was not allocated
	jmp	XmsRet			;go back to caller

	;----------------------------------------------------------;
	; Query Free Extended Block:				   ;
	;							   ;
	; we return size of largest block in Kbytes		   ;
	;----------------------------------------------------------;

XmsFunc08:

	pushem	ax,bx,dx		;save entry parameters
	call	GetLargestFreeBlock	;gets largest free block

; if there is no memory available in our area we should chain on.

	or	dx,dx			;is there ant memory ?
	jnz	Trap08Worked		;yes.

; chain on.

	popem	ax,bx,dx		;restore
	jmp	XmsDefaultFunc		;chain on.

Trap08Worked:

	add	sp,6			;discard saved values.
	jmp	XmsRet

	;----------------------------------------------------------;
	; Allocate Extended Memory Block			   ;
	;							   ;
	; tries to do a local allocation.			   ;
	;----------------------------------------------------------;

XmsFunc09:

	or	dx,dx			;zero length allocations ?
	jz	ChainXmsFunc09		;yes, let prev handler handle it
	pushem	ax,bx,dx		;save entry parameters
	call	AllocateXmsBlock	;do the allocation

; if we fail to allocate memory, we should chain it on.

	or	ax,ax			;did we succeed ?
	jnz	Trap09Worked		;yes.

; chain on.

	popem	ax,bx,dx		;restore entry parameters

ChainXmsFunc09:

	jmp	XmsDefaultFunc		;chain on.

Trap09Worked:

	add	sp,6			;discard saved values
	jmp	XmsRet			;go back to caller

	;----------------------------------------------------------;
	; Free Extended Memory Block				   ;
	;							   ;
	; frees up an allocation				   ;
	;----------------------------------------------------------;


XmsFunc0a:

	pushem	ax,bx			;save
	call	FreeXmsBlock		;free up an allocation
	or	ax,ax			;did it succeed ?
	jnz	Trap0AWorked		;yes.

; if the call failed bacause of an invalid handle, we should chain it on.

	cmp	bl,0a2h			;bad handle ?
	jnz	Trap0AWorked		;no.

; chain the call on as we do not know about the handle.
	
	popem	ax,bx			;restore entry parameters
	jmp	XmsDefaultFunc		;chain on.

Trap0AWorked:

	add	sp,4			;discard saved entry values
	jmp	XmsRet			;go back to caller

	;----------------------------------------------------------;
	; Move Extended Memory Block				   ;
	;							   ;
	; we return saying HMS is already in use.		   ;
	;----------------------------------------------------------;

XmsFunc0b:

	pop	ds			;get back ds
	call	MoveXmsBlock		;move the block
	pop	bp			;restore
	popf
	far_ret

	;----------------------------------------------------------;
	; LockExtendedMemoryBlock:                                 ;
	;							   ;
	; we return the linear address  of the block		   ;
	;----------------------------------------------------------;

XmsFunc0c:

	pushem	ax,bx			;save entry values.
	call	LockXmsBlock		;get the address of the block
	or	ax,ax			;did it succeed ?
	jnz	Trap0CWorked		;yes.

; if the call failed bacause of an invalid handle, we should chain it on.

	cmp	bl,0a2h			;bad handle ?
	jnz	Trap0CWorked		;no.

; chain the call on as we do not know about the handle.
	
	popem	ax,bx			;restore entry parameters
	jmp	XmsDefaultFunc		;chain on.

Trap0CWorked:

	add	sp,4			;discard saved entry values
	jmp	XmsRet			;go back to caller

	;----------------------------------------------------------;
	; Unlock Extended Memory Block:				   ;
	;							   ;
	; If the handle is valid, return success		   ;
	;----------------------------------------------------------;

XmsFunc0d:

	pushem	ax,bx			;save
	call	UnlockXmsBlock		;unlock the block
	or	ax,ax			;did it succeed ?
	jnz	Trap0DWorked		;yes.

; if the call failed bacause of an invalid handle, we should chain it on.

	cmp	bl,0a2h			;bad handle ?
	jnz	Trap0DWorked		;no.

; chain the call on as we do not know about the handle.
	
	popem	ax,bx			;restore entry parameters
	jmp	XmsDefaultFunc		;chain on.

Trap0DWorked:

	add	sp,4			;discard saved entry values
	jmp	XmsRet			;go back to caller

	;----------------------------------------------------------;
	; Get EMB handle information				   ;
	;							   ;
	; get information about the handle			   ;
	;----------------------------------------------------------;

XmsFunc0e:

	push	ax			;save
	call	GetXmsHandleInfo	;get information about handle
	or	ax,ax			;did it succeed ?
	jnz	Trap0EWorked		;yes.

; chain the call on as we do not know about the handle.
	
	pop	ax			;restore entry parameter
	jmp	XmsDefaultFunc		;chain on.

Trap0EWorked:

	add	sp,2			;discard saved entry values
	jmp	XmsRet			;go back to caller

	;----------------------------------------------------------;
	; Rellocate Extended Memory Block			   ;
	;							   ;
	; try to grow or shrink block without moving it		   ;
	;----------------------------------------------------------;

XmsFunc0f:

	pushem	ax,bx			;save entry values
	call	RellocateXmsBlock	;try to do the rellocation
	or	ax,ax			;did it succeed ?
	jnz	Trap0FWorked		;yes.

; if the call failed bacause of an invalid handle, we should chain it on.

	cmp	bl,0a2h			;bad handle ?
	jnz	Trap0FWorked		;no.

; chain the call on as we do not know about the handle.
	
	popem	ax,bx			;restore entry parameters
	jmp	XmsDefaultFunc		;chain on.

Trap0FWorked:

	add	sp,4			;discard saved entry values
	jmp	XmsRet			;get back to caller

	;----------------------------------------------------------;
	; Request Upper Memory Block			           ;
	;							   ;
	; we return saying no UMBs area aveilable		   ;
	;----------------------------------------------------------;

XmsFunc10:

	xor	ax,ax			;fail the call
	mov	bl,0b1h			;no UMBs area available
	jmp	XmsRet


	;----------------------------------------------------------;
	; Release Upper Memory Block			           ;
	;							   ;
	; we return saying segment no is invalid		   ;
	;----------------------------------------------------------;

XmsFunc11:

	xor	ax,ax			;fail the call
	mov	bl,0b1h			;segmet number invalid
	jmp	XmsRet

;----------------------------------------------------------------------------;
; GetLargestFreeBlock:							     ;
;									     ;
; Traverses the allocation table, looking for the largest free block and also;
; accumulation total amount of free space.				     ;
;----------------------------------------------------------------------------;

GetLargestFreeBlock  proc near

	assumes ds,StubSeg

	push	si			;save it
	xor	ax,ax			;initialize largest block count
	xor	dx,dx			;initialize total free count
	mov	si,StubSegOFFSET AllocationTable
	sub	si,SIZE XmsTableEntry	;bias it back for addition below

LookForFreeBlocks:

	add	si,SIZE XmsTableEntry	;look at the next entry
	cmp	[si].THandle,0ffffh	;is this the end of the table
	jz	GetLargestFreeBlockRet	;yes, return back
	cmp	[si].THandle,0		;is it a free block
	jnz	LookForFreeBlocks	;keep looking
	add	dx,[si].TSize		;accumulate net free block
	cmp	ax,[si].TSize		;is this block bigger
	jae	LookForFreeBlocks	;no
	mov	ax,[si].TSize		;largest block obtained till now
	jmp	LookForFreeBlocks	;keep looking

GetLargestFreeBlockRet:

	pop	si			;restore
	ret

GetLargestFreeBlock  endp
;----------------------------------------------------------------------------;
; AllocateXmsBlock:							     ;
;									     ;
; Allocates the first fit XMS block.  It also incs the 'WoaAppUsesXms' field ;
; if the allocation succeedes.						     ;
;----------------------------------------------------------------------------;

AllocateXmsBlock proc near

	pushem	si,di,cx		;save work registers
	call	GetFreeHandle		;get a free handle
	or	bx,bx			;free handle obtained ?
	jz	AllocXmsNoHandle	;no more free handles

; look for the first block that matches size.

	mov	si,StubSegOFFSET AllocationTable
	sub	si,SIZE XmsTableEntry	;bias it for subtraction below

AllocXms1:

	add	si,SIZE XmsTableEntry	;look at the next entry
	cmp	[si].THandle,0ffffh	;have we reached the end ?
	jz	AllocXmsNoFree		;yes, there are no appropriate blocks
	cmp	[si].THandle,0		;is it free ?
	jnz	AllocXms1		;no, keep looking
	cmp	[si].TSize,dx		;appropriate for allocation ?
	jb	AllocXms1		;no.
	call	AllocateBlock		;allocates entry
	mov	dx,bx			;get the handle in DX
	mov	ax,1			;block allocated
	xor	bl,bl			;no errors
	inc	WoaAppUsesXMS		;one more allocation done.
	jmp	short AllocXmsRet	;return back

AllocXmsNoFree:

	call	ReleaseHandle		;mark handle in bx as free
	xor	ax,ax			;could not allocate
	mov	bl,0a0h			;no appropriate frr blocks
	jmp	short AllocXmsRet  	;go back

AllocXmsNoHandle:

	xor	ax,ax			;could not allocate
	mov	bl,0a1h			;as alll handles are in use
	
AllocXmsRet:

	popem	si,di,cx		;restore
	ret				;get back

AllocateXmsBlock endp
;----------------------------------------------------------------------------;
; GetFreeHandle:	  						     ;
;									     ;
; This function calls the previous XMS handler to allocate a zero length     ;
; XMS handle and returns that in BX. If a handle cannot be allocated, we     ;
; return with 0 in BX. If some handler uses 0 as a valid handle which is     ;
; still available for use, we will thereby ignore it.			     ;
;----------------------------------------------------------------------------;

GetFreeHandle	proc near

	assumes ds,StubSeg

	pushem	ax,dx			;save
	mov	ah,09h			;allocate block
	xor	dx,dx			;need a zero length block
	call	[PrevXmm]		;call the previous handler
	mov	bx,dx			;get the handle in BX
	or	ax,ax			;was it successful ?
	jnz	GetFreeHandleRet	;yes.
	xor	bx,bx			;no handle

GetFreeHandleRet:

	popem	ax,dx			;restore
	ret				;go back

GetFreeHandle  endp
;----------------------------------------------------------------------------;
; ReleaseHandle:							     ;
;									     ;
; On entry BX has a handle number and it is freed by calling the previous    ;
; XMS handler.								     ;
;----------------------------------------------------------------------------;

ReleaseHandle	proc near

	assumes ds,StubSeg

	pushem	ax,dx			;save
	mov	ah,0ah			;free xms block
	mov	dx,bx			;get the handle
	call	[PrevXmm]		;free the handle
	popem	ax,dx			;restore
	ret

ReleaseHandle	endp
;----------------------------------------------------------------------------;
; AllocateBlock:							     ;
;									     ;
; On entry si points to a free entry in the allocation table and dx has the  ;
; size of the allocation that we need. This routine does the allocation, if  ;
; the free size is exactly equal to the requested size, the same entry can   ;
; be updated, else the entries below the slot have to be pushed down to make ;
; room for a free entry. Also BX has the handle to allocate.		     ;
;----------------------------------------------------------------------------;

AllocateBlock	proc  near

	assumes ds,StubSeg

	cmp	[si].TSize,dx		;exact fit ?
	jnz	NotExactFit		;no

; we have an exact fit situation, update the entry to mark it with the handle
; and we are done

	mov	[si].THandle,bx		;mark as allocated
	jmp	short AllocateBlockRet	;go back

NotExactFit:

	call	MoveEntriesDown		;make room for a free entry 
	mov	[si].THandle,bx		;mark this one as allocated
	push	di			;save
	mov	di,[si].TBaseHi		;high word of base
	mov	cx,[si].TBaseLo		;get loword of base
	call	UpdateBase		;add DX*1024 to DI:CI
	mov	ax,[si].TSize		;get current size
	mov	[si].TSize,dx		;allocated size
	sub	ax,dx			;size of free block at end
	add	si,SIZE XmsTableEntry	;point to next entry
	mov	wptr [si].THandle,0	;mark as free
	mov	wptr [si].TBaseHi,di	;set hiword of base
	mov	wptr [si].TBaseLo,cx	;lo word of base
	pop	di			;restore
	mov	bptr [si].TLckCount,0	;not locked
	mov	[si].TSize,ax		;size of free area

AllocateBlockRet:

	ret				;end of allocation

AllocateBlock  endp
;----------------------------------------------------------------------------;
; UpdateBase:								     ;
;									     ;
; This adds DX*1024 to DI:CX without destroying other register.		     ;
;----------------------------------------------------------------------------;

UpdateBase  proc  near

	pushem	ax,bx,dx		;save
	mov	ax,dx			;get multiplicand in ax
	xor	dx,dx			;will hold result
	mov	bx,1024			;multiplier
	mul	bx			;dx:ax has result
	add	cx,ax			;add in low wors
	adc	di,dx			;add in hi words with carry
	popem	ax,bx,dx		;restored thrashed registers
	ret

UpdateBase  endp
;----------------------------------------------------------------------------;
; MoveEntriesDown:							     ;
;									     ;
; On entry SI points to an entry in the allocation table. This routine moves ;
; all the allocated entries below the one pointed by si, by one slot.	     ;
;----------------------------------------------------------------------------;

MoveEntriesDown	 proc  near

	assumes ds,StubSeg

	pushem	es,si,di,cx		;save registers

	smov	es,ds			;same segment

; first find out no of entries to move

	mov	di,si			;get the current entry
	mov	cx,1			;initialize count

MoveEntriesDown1:

	add	di,SIZE XmsTableEntry	;point to next entry
	cmp	wptr [di].THandle,0ffffh;is the end ?
	jz	MoveEntriesDown2	;no of entries to move is in cx
	inc	cx			;one more to move
	jmp	short MoveEntriesDown1	;keep counting

MoveEntriesDown2:

	mov	si,di			;last entry in the table
	add	di,SIZE XmsTableEntry	;new place for it
	cld				;set proper direction

MoveEntriesDown3:

	pushem	di,si,cx	       	;save current pointers and count
	mov	cx,SIZE XmsTableEntry	;size of one entry
	cld
	rep	movsb			;move it down
	popem	di,si,cx		;restore counts and pointers
	sub	si,SIZE XmsTableEntry	;source goes back by 1
	sub	di,SIZE XmsTableEntry	;destination goes back by 1
	loop	MoveEntriesDown3	;move required number of entries down

	popem	es,si,di,cx		;restore initial values
	ret

MoveEntriesDown endp
;----------------------------------------------------------------------------;
; FreeXmsBlock:							             ;
; 									     ;
; Tries to free an allocated XMS block, and then compacts the table to merge ;
; any consecutive free blocks together.	It also decs the 'WoaAppUsesXMS'     ;
; field after the deallocation.						     ;
;----------------------------------------------------------------------------;

FreeXmsBlock  proc near

	assumes ds,StubSeg

	pushem	si,di,es		;save work registers

; locate the entry for the block to be freed first

	call	LocateTableEntry	;locates the table entry
	jc	FreeInvalidHandle	;could not locate the entry

; we cannot free the block if it is locked

	cmp	bptr [si].TLckCount,0	;is it locked ?
	jnz	FreeLockedBlock		;yes,cannot free the block

; mark the block as free and compact the table, merging successive free
; blocks.

	push	bx			;save
	xor	bx,bx			;id for free handle.
	xchg	bx,[si].THandle		;get handle, mark entry as free
	call	ReleaseHandle		;reslease the handle
	pop	bx			;restore
	call	CompactTable		;compact the table
	mov	ax,1			;block successfully freed
	xor	bl,bl			;no error code
	dec	WoaAppUsesXMS		;one more block freed.
	jmp	short FreeXmsBlockRet 	;go back

FreeInvalidHandle:

	xor	ax,ax			;could not free the block
	mov	bl,0a2h			;as handle is invalid
	jmp	short FreeXmsBlockRet  	;return back

FreeLockedBlock:
	
	xor	ax,ax			;could not free the block
	mov	bl,0abh			;as block is locked

FreeXmsBlockRet:
	
	popem	si,di,es		;restore work registers
	ret

FreeXmsBlock	endp
;----------------------------------------------------------------------------;
; LocateTableEntry:							     ;
;									     ;
; Given a handle value in DX, this routine returns si pointing to the entry  ;
; in the allocation table, carry will be set if the handle is invalid .	     ;
;----------------------------------------------------------------------------;


LocateTableEntry  proc  near

	mov	si,StubSegOFFSET AllocationTable
	or	dx,dx			;handle valid ?
	jz	LocateInvalidHandle	;no.

LocateEntry1:

	cmp	[si].THandle,0ffffh	;end of table ?
	jz	LocateInvalidHandle	;bad handle provided
	cmp	[si].THandle,dx 	;is this the entry ?
	jz	@f			;yes, all done
	add	si,size XmsTableEntry	;no, keep looking.
	jmp	short LocateEntry1
@@:
	clc				;handle found
	jmp	short LocateTableEntryRet;fo back

LocateInvalidHandle:

	stc				;not a valid handle

LocateTableEntryRet:

	ret

LocateTableEntry  endp
;----------------------------------------------------------------------------;
;CompactTable:							             ;
;									     ;
; Compacts the allocation table, merging consecutive free blocks together.   ;
;----------------------------------------------------------------------------;

CompactTable proc near

	smov	es,ds			;both points to our segment
	mov	si,StubSegOFFSET AllocationTable
	mov	di,si			;get start of table in si
	add	di,SIZE XmsTableEntry	;di points to next one

CompactionLoop:

	cmp	wptr [di].THandle,0ffffh;end of table ?
	jz	CompactTableRet		;yes, compaction done.
	cmp	wptr [si].THandle,0	;is it a free entry ?
	jz	LookForAnotherFree	;is next block free too ?

BumpCompactionPointers:

	mov	si,di			;point tp next one
	add	di,SIZE XmsTableEntry	;and the one after that
	jmp	short CompactionLoop	;keep compacting

LookForAnotherFree:

	cmp	wptr [di].THandle,0	;is the next one free too ?
	jnz	BumpCompactionPointers	;no scope of compaction.

; we have a couple of entries to compact into 1.

	mov	ax,[di].TSize		;get size of second entry
	add	[si].TSize,ax		;compact the two.
	call	MoveEntriesUp		;move subsequent entries up
	jmp	short CompactionLoop	;look for more compactions

CompactTableRet:

	ret

CompactTable  endp
;----------------------------------------------------------------------------;
; MoveEntriesUp:							     ;
;									     ;
; On entry DI points to a table entry, this routine moves the entries after  ;
; that by one slot, obliterating the entry pointed by DI. On exit SI,DI      ;
; should maintain their entry time values.				     ;
;----------------------------------------------------------------------------;

MoveEntriesUp	proc  near

	assumes ds,StubSeg

	pushem	si,di,cx		;must preserve values
	smov	es,ds			;both must have own segment
	mov	si,di			;current entry
	add	si,SIZE XmsTableEntry	;point to next one
	cld				;have proper move direction

MoveEntryLoop:

	mov	cx,SIZE XmsTableEntry	;size of an entry
	cld
	rep	movsb			;move one entry up.
	cmp	wptr [di].THandle,0ffffh;was the last entry moved ?
	jnz	MoveEntryLoop		;cont. ptrs are correct
	
	popem	si,di,cx		;restore values
	ret				;go back, entries moved up

MoveEntriesUp endp
;----------------------------------------------------------------------------;
; MoveXmsBlock:								     ;
;									     ;
; If the src and dst handles in the move structure have valid handle values  ;
; this routine will substitute the handles by the actual XMS block handle    ;
; and add in the offset of the apps XMS base to the offsets specified and    ;
; then will invoke the actual XMS handler to move the block.		     ;
;----------------------------------------------------------------------------;

MoveXmsBlock proc near


	pushem	es,ds,di,si,cx,dx	;save registers
	smov	es,cs			;get own segment

; first copy over the move structure into our segment

	mov	di,StubSegOFFSET MoveStructure
	mov	cx,16			;16 bytes to move
	cld
	rep	movsb			;transfer the block
	smov	ds,es			;have own segment
	assumes ds,StubSeg

	mov	si,StubSegOFFSET MoveStructure

; patch up the structure

	push	ax			;save
	mov	ax,[si].MSrcHandle	;get the source handle
	or	ax,ax			;is it null ?
	jz	PatchDstMove		;no need to modify src
	call	GetBaseOffset		;get offset from start of XMS block
	jc	PatchDstMove		;invalid source handle
	add	ax,ExtraMoveOffsetLo	;add in the low word of pad area
	adc	dx,ExtraMoveOffsetHi	;add in hiword of pad area
	add	[si].MSrcOffsetLo,ax	;add it in
	adc	[si].MSrcOffsetHi,dx	;add it in
	mov	ax,XmsBaseHandle	;get the actual handle
	mov	[si].MSrcHandle,ax	;put in the actual handle

PatchDstMove:

	mov	ax,[si].MDstHandle	;get the destination handle
	or	ax,ax			;is it null ?
	jz	PatchDoMove		;no need to modify dst
	call	GetBaseOffset		;get offset from start of XMS block
	jc	PatchDoMove		;invalid dest handle
	add	ax,ExtraMoveOffsetLo	;add in the low word of pad area
	adc	dx,ExtraMoveOffsetHi	;add in hiword of pad area
	add	[si].MDstOffsetLo,ax	;add it in
	adc	[si].MDstOffsetHi,dx	;add it in
	mov	ax,XmsBaseHandle	;get the actual handle
	mov	[si].MDstHandle,ax	;put in the actual handle

PatchDoMove:
	
	pop	ax			;restore
	call	[PrevXmm]		;get original XMS to do the move
	popem	es,ds,di,si,cx,dx	;restore registers
	ret

MoveXmsBlock	endp
;----------------------------------------------------------------------------;
; LockXmsBlock:								     ;
;									     ;
; Increases the lock count of a specified block.			     ;
;----------------------------------------------------------------------------;

LockXmsBlock  proc near

	assumes ds,StubSeg

	push	si			;save

; locate the entry for the handle first
	
	call	LocateTableEntry	;if found, ds:si points to the entry
	jc	LockInvalidHandle	;not a valid handle

; increment the lock count

	cmp	bptr [si].TLckCount,0ffh;max count already ?
	jz	LockCountOverflow	;has been locked too many times
	inc	[si].TLckCount		;locking the block
	mov	bx,[si].TBaseLo		;get low wprd of base
	mov	dx,[si].TBaseHi		;get hiword of base
	mov	ax,1			;block is locked
	jmp	short LockXmsRet	;get back

LockInvalidHandle:

	xor	ax,ax			;cannot lock
	mov	bl,0a2h			;invalid handle
	jmp	short LockXmsRet	;go back

LockCountOverflow:

	xor	ax,ax			;could not lock the block
	mov	bl,0ach			;count overflow

LockXmsRet:

	pop	si			;restore
	ret

LockXmsBlock	endp
;----------------------------------------------------------------------------;
; UnlockXmsBlock:							     ;
;									     ;
; decreases the lock count of a specified block.			     ;
;----------------------------------------------------------------------------;

UnlockXmsBlock  proc near

	assumes ds,StubSeg

	push	si			;save

; locate the entry for the handle first
	
	call	LocateTableEntry	;if found, ds:si points to the entry
	jc	UnlockInvalidHandle	;not a valid handle

; decrement the Unlock count

	cmp	bptr [si].TLckCount,0	;is it locked ?
	jz	UnlockNotLocked		;not a locked block
	dec	[si].TLckCount		;locking the block
	mov	ax,1			;block is locked
	jmp	short UnlockXmsRet	;get back

UnlockInvalidHandle:

	xor	ax,ax			;cannot lock
	mov	bl,0a2h			;invalid handle
	jmp	short UnlockXmsRet	;go back

UnlockNotLocked:

	xor	ax,ax			;could not lock the block
	mov	bl,0aah			;block was not locked

UnlockXmsRet:

	pop	si			;restore
	ret

UnlockXmsBlock	endp
;----------------------------------------------------------------------------;
; GetXmsHandleInfo:							     ;
;									     ;
; Returns information about the a handle and no of free handles in the system;
;----------------------------------------------------------------------------;

GetXmsHandleInfo  proc near

	assumes ds,StubSeg

	push	si			;save
	call	LocateTableEntry	;try to loacte the entry
	jc	InvalidHandleInfo	;no a valid handle
	call	GetFreeHandleCount	;get no of free handles in system
	mov	bh,[si].TLckCount	;get lock count
	mov	dx,[si].TSize		;get the size
	mov	ax,1			;call was successful
	jmp	short GetXmsHandleInfoRet;go back

InvalidHandleInfo:

	xor	ax,ax			;call didnot succeed

GetXmsHandleInfoRet:
	
	pop	si			;restore
	ret

GetXmsHandleInfo  endp
;----------------------------------------------------------------------------;
; GetFreeHandleCount:							     ;
;								             ;
; Get number of free handles in the system. Since we allocate handle number  ;
; by allocating 0 length blocks from the previous handler, we will let the   ;
; the previous handler handle this call too.				     ;
;									     ;
; We just make the call on our base handle.				     ;
;----------------------------------------------------------------------------;

GetFreeHandleCount  proc  near

	assumes ds,StubSeg

	pushem	si,cx,dx,ax		;save
	mov	dx,XmsBaseHandle	;the handle of the 'big' block
	mov	ah,0eh	  		;get EMB handle info call
	call	[PrevXmm]		;get information, num handles in BL
	popem	si,cx,dx,ax		;restore 
	ret

GetFreeHandleCount  endp
;----------------------------------------------------------------------------;
; RelocateXmsBlock:							     ;
;									     ;
; Tries to grow or shrink the block. 				             ;
;----------------------------------------------------------------------------;

cProc	RellocateXmsBlock,<NEAR,PUBLIC,PASCAL>

	localV	MoveParams,16		;move parameter block, if needed
	localW	Handle1			;the original handle
	localW	Handle2			;the new handle
	localW	CurrentSize		;keeps current block size
	localW	Size1			;new size

cBegin

	assumes ds,StubSeg

	push	si			;save
	call	LocateTableEntry	;try to locate the entry
	njc	RellocateInvalidHandle	;bad handle

; test to see if the block is locked or not

	cmp	[si].TLckCount,0	;is it locked
	njnz	RellocateLockedBlock	;can't rellocate

; get the current size and decide whether to grow or shrink block

	cmp	bx,[si].TSize		;compare sizes
	nje	RellocationDone		;same size
	ja	RellocateGrow		;want to grow the block

; we are trying to shrink the block

	pushem	si,di,ax,bx,cx,es	;save
	call	MoveEntriesDown		;allocate a hole
	mov	ax,[si].TSize		;get current size
	sub	ax,bx			;size of free block created
	mov	[si].TSize,bx		;update size
	mov	di,[si].TBaseHi		;get hiword of current base
	mov	cx,[si].TBaseLo		;get lo word of current base
	mov	dx,bx			;size of the block in K
	call	UpdateBase		;add dx*1024 to di:cx
	add	si,SIZE XmsTableEntry	;point to the free slot entry
	mov	wptr [si].THandle,0	;mark as free
	mov	[si].TBaseLo,cx		;lo word of base
	mov	[si].TBaseHi,di		;hi word of base
	mov	[si].TSize,ax		;size of free block
	mov	bptr [si].TLckCount,0	;not locked
	call	CompactTable		;merge contiguous free blocks
	popem	si,di,ax,bx,cx,es	;restore
	jmp	RellocationDone		;successful

RellocateGrow:

	pushem	si,di,ax,bx,cx,dx,es	;save
	mov	di,si			;get the current entry
	add	di,SIZE XmsTableEntry	;point to next one
	cmp	wptr [di].THandle,0ffffh;end of table ?
	je	HarderRellocation	;need to rellocate the hard way
	cmp	wptr [di].THandle,0	;is it a free block ?
	jne	HarderRellocation	;need to relocate hard way
	mov	ax,[si].TSize		;get current size
	add	ax,[di].TSize		;the size of the free area
	cmp	bx,ax			;compare with size requested
	ja	HarderRellocation	;need to rellocate the hard way
	je	ExactFitRellocation	;free block will totaly be consumed

; we reach here when there is a sufficiently large free block below the the
; requested block and we can grow into this area yet have some left

	mov	[si].TSize,bx		;new size
	sub	ax,bx			;the size of the free area left
	mov	bx,[di].TSize		;original size
	sub	bx,ax			;size taken away
	mov	[di].TSize,ax		;update the size
	mov	si,di			;need pointer to the free block
	mov	di,[si].TBaseHi		;get the high word of base
	mov	cx,[si].TBaseLo		;get the low word of base
	mov	dx,bx			;size in K by which area was shrunk
	call	UpdateBase		;update DI:CX accordingly
	mov	[si].TBaseHi,di		;update hi word of base
	mov	[si].TBaseLo,cx		;update lo word of base
	popem	si,di,ax,bx,cx,dx,es	;restore
	jmp	RellocationDone		;successful

ExactFitRellocation:

; we reach here if there is a free block after the desired block and it will
; totally be taken up

	mov	[si].TSize,bx		;update size
	call	MoveEntriesUp		;obliterate the free block
	popem	si,di,ax,bx,cx,dx,es	;restore
	jmp	short RellocationDone	;successful

HarderRellocation:

; try to allocate a block of the new size, by calling own XMS

	pushem	bx,dx			;save
	mov	Handle1,dx		;save the original handle
	mov	Size1,bx		;save requested size
	mov	ax,[si].TSize		;get the current size
	mov	CurrentSize,ax		;save it
	mov	ah,9			;allocate call
	mov	dx,bx			;amount of memory required
	call	XmsHandler		;try to allocate the block
	or	ax,ax			;was it successful ?
	jz	BiggerRellocFails	;no.
	mov	Handle2,dx		;save the new handle

; the bigger block could be allocated, now we have to move the original block

	push	ds			;save
	smov	ds,ss			;need to access MoveParams
	lea	si,MoveParams		;point to the parameter block
	mov	[si].MDstHandle,dx	;save destination handle
	mov	[si].MDstOffsetLo,0	
	mov	[si].MDstOffsetHi,0	;dst offset is zero
	mov	[si].MSrcOffsetLo,0	
	mov	[si].MSrcOffsetHi,0	;src offset is zero
	mov	ax,CurrentSize		;get the current size
	mov	bx,1024			;it is in K
	mul	bx			;ax:dx has length of move
	mov	[si],ax			;save lo length
	mov	[si][2],dx		;save high length
	mov	dx,Handle1		;get the original handle
	mov	es:[si].MSrcHandle,dx	;save source handle
	mov	ah,0bh			;want to do a move
	call	XmsHandler		;move the block
	pop	ds			;get back own handle
	mov	dx,Handle1		;get the original handle
	mov	ah,0ah			;free block code
	call	XmsHandler		;free the block
	mov	dx,Handle2		;get the new handle
	mov	bx,Handle1		;the original handle
	call	LocateTableEntry	;ds:si points to entry
	mov	[si].THandle,bx		;set the original handle back
	mov	bx,dx			;the work handle
	call	ReleaseHandle		;release the handle
	popem	bx,dx			;restore
	jmp	short RellocationDone	;success

BiggerRellocFails:

	popem	bx,dx			;restore
	xor	ax,ax			;rellocation fails
	mov	bl,0a0h			;no more available memory
	jmp	short RellocateXmsBlockRet;done

RellocationDone:

	mov	ax,1			;operation successful
	xor	bl,bl			;no error
	jmp	short RellocateXmsBlockRet;go back

RellocateInvalidHandle:

	xor	ax,ax			;error
	mov	bl,0a2h			;because of invalid handle
	jmp	short RellocateXmsBlockRet;go back

RellocateLockedBlock:

	xor	ax,ax			;cannot rellocate
	mov	bl,0abh			;as the block is locked

RellocateXmsBlockRet:

	pop	si			;restore

cEnd
;----------------------------------------------------------------------------;
; GetBaseOffset:							     ;
;									     ;
; Given a handle in AX, this routine gets the offset of the start of the     ;
; block from line from which XMS allocation starts. The offset is returned   ;
; in DX:AX and carry is cleared if the handle is valid.			     ;
;----------------------------------------------------------------------------;

GetBaseOffset proc near

	push	si			;save
	mov	dx,ax			;get the handle
	call	LocateTableEntry	;si-> node for the handle
	jc	GetBaseOffsetRet	;handle not valid
	mov	ax,[si].TBaseLo	   	;get low word of base
	mov	dx,[si].TbaseHi		;DX:AX has actual base
	sub	ax,wptr [AppXmsBase]	;get the offset from the start line
	sbb	dx,wptr [AppXmsBase+2]	;DX:AX has relative offset
	clc				;success

GetBaseOffsetRet:		

	pop	si			;restore
	ret

GetBaseOffset endp
;----------------------------------------------------------------------------;
; HookOurXmsCode:							     ;
;									     ;
; This tries to hook our XMS code. If it fails it does nothing.		     ;
;----------------------------------------------------------------------------;
cProc	HookOurXMSCode,<NEAR,PUBLIC,PASCAL>

cBegin

	pushem	es,bx,ax,cx,si,di		;save

	mov	bx,word ptr [ActualXMS]
	mov	es,word ptr [ActualXMS+2]   	;ES:BX = ptr to 1st XMM header

NextXmmHeader:

	mov	word ptr [PrevXmm+2],es		;save seg of prev control adr
	mov	word ptr [XmmControlBase+2],es	;save address for patch
	mov	word ptr [XmmControlBase],bx
	mov	cx,word ptr es:[bx]
	cmp	cl,0EBh				;compare short jmp opcode
	je	ShortJmp
	cmp	cl,0EAh				;compare far jmp opcode
	jne	XmmChainHosed			;bad XMM control chain

FarJmp:

	mov	si,word ptr es:[bx+1]		;SI = offset of jmp
	mov	es,word ptr es:[bx+1+2]		;ES = segment of jmp
	mov	bx,si				;ES:BX points to next node
	jmp	NextXmmHeader			;continue down control chain

ShortJmp:

	cmp	word ptr es:[bx+2],9090h	; check NOPs
	jne	XmmChainHosed			; bad XMM control chain
	cmp	byte ptr es:[bx+4],90h
	jne	XmmChainHosed			; bad XMM control chain
	mov	di,bx				; DI = ptr to XMM header
	xor	ax,ax
	mov	al,ch				; AX = offset of short jmp
	mov	[XmmControlJmpVal],al		; save offset of short jmp
	add	ax,2				; add length of jmp instr
	add	bx,ax				; BX = target of jmp
	mov	word ptr [PrevXmm],bx  		; save previous control addr

; Install ourselves in XMM control chain.

	mov	byte ptr es:[di],0EAh		; far immediate jmp opcode
	mov	word ptr es:[di+1],StubSegOFFSET XmsHandler
	mov	word ptr es:[di+3],cs

	jmp	short Xh_done

XmmChainHosed:

Xh_done:

	popem	es,bx,ax,cx,si,di		;save

HookXMSRet:

cEnd
;----------------------------------------------------------------------------;
; UnHookOurXmsCode:							     ;
;									     ;
; We take out our XMS hook if we had put it in.				     ;
;----------------------------------------------------------------------------;
cProc	UnHookOurXmsCode,<NEAR,PUBLIC,PASCAL>

cBegin

	cmp	word ptr [XmmControlBase],-1
	jne	UnhookXmm
	cmp	word ptr [XmmControlBase+2],-1
	je	UnHookXMSRet		;we didn't hook- don't unhook
UnhookXmm:
	pushem	es,bx			;save
	mov	al,0EBh			;AL = opcode for short jump
	mov	ah,[XmmControlJmpVal]	;AH = displacement for short jump
	les	bx,[XmmControlBase]	;ES:BX = ptr to previous XMM header
	mov	word ptr es:[bx],ax	;restore previous XMM's short jump
	mov	word ptr es:[bx+2],9090h;followed by nop's
	mov	byte ptr es:[bx+4],90h	;third nop
	mov	word ptr [XmmControlBase],-1;we don't have our hook
	mov	word ptr [XmmControlBase+2],-1

	popem	es,bx			;restore

UnHookXMSRet:

cEnd
;----------------------------------------------------------------------------;
sEnd	StubSeg

end
	
