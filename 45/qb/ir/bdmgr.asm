	TITLE BdMgr.asm - Buffer Descriptor Management Routines

COMMENT	\

--------- --- ---- -- ---------- ----
COPYRIGHT (C) 1985 BY MICROSOFT, INC.
--------- --- ---- -- ---------- ----

\

;============================================================================
; Module: BdMgr.asm - Buffer Descriptor Management Routines
;	This is a layer of routines which depends on the BASCOM Runtime Heap
;	Management routines in source file (strutl.asm).
;	It manages entries in the Interpreter and Far heap.
; System: Quick BASIC Interpreter
;============================================================================

	.xlist

	include version.inc
	BDMGR_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	parser
	includeOnce	txtmgr
	includeOnce	util

	.list

;	.sall


assumes	DS,DATA
assumes	ES,DATA
assumes	SS,DATA

sBegin	DATA

; HMEM_ constants used by SBMGR allocation routines

HMEM_FIXED	EQU	0000H		
HMEM_MOVEABLE	EQU	0002H		
HMEM_NOCOMPACT	EQU	0010H		
HMEM_ZEROINIT	EQU	0040H		
HMEM_DISCARDABLE EQU	0F00H		

externW b$fVarHeapActive		;non-0 when variable heap is active

DbOMCnt	MACRO	label
	ENDM

sEnd	DATA

	EXTRN	AdjustCommon:FAR
	EXTRN	AdjustVarTable:FAR


sBegin RT
assumes	CS,RT

	EXTRN	B$ILHALC:NEAR
	EXTRN	B$LHREALC:NEAR
	EXTRN	B$LHDALC:NEAR
	EXTRN	B$LHChgBakPtr:NEAR
	EXTRN	B$ILHADJ:NEAR
	EXTRN	B$LHForEachEntry:NEAR
	EXTRN	B$NHCPCT:NEAR
	EXTRN	B$TglHeapSpt:FAR

	EXTRN	B$IFHAlloc:NEAR 	
	EXTRN	B$FHDealloc:NEAR	
	EXTRN	B$FHRealloc:NEAR	
	EXTRN	B$FHAdjDesc:NEAR	
	EXTRN	B$FHAdjOneDesc:NEAR	

CBBUFBLOCK equ 512	;Never grow a near heap by less than this
			; number of bytes to reduce heap thrashing.

VAR_EXTRA equ 30	;we want to keep up to this much free space beyond
			; cbLogical in IT_VAR (variable) tables when compressing
			; other bd's all the way back to cbLogical. This is to
			; help the user's chances of edit and CONTinuing.




;------------------------------------------------------------
;---  Interpreter Buffer Descriptor Management Routines   ---
;------------------------------------------------------------
;***
;B$IHeapEntryMoved - handle movement of Interpreter-specific Heap entry
;Purpose:
;	This routine is called by the Runtime Heap management
;	code just before it moves an Interpreter-specific Heap entry.
;	This routine performs any updating necessary due to
;	the movement of an Interpreter-specific Heap entry
;	it does not need to update entry pointer in the bd[p] for
;	the entry being moved; it just dispatches based on the
;	heap entry type to a routine which finds all string/heap
;	owners within the heap entry being moved, and calls the heap
;	management code to update the backpointers. Note that each 
;	such routine will be located in it's own component; these 
;	routines will call the runtime heap manager directly, but
;	will do so via a macro to keep runtime heap interface knowledge
;	confined to this module and associated header files.
;Entry:
;	AX = number of bytes the heap entry has moved.
;	       A positive number indicates the entry has moved
;	       to a higher address.
;	BX = pointer to the bd[p].pb field of the buffer descriptor for
;		the heap entry being moved.
;	CL = type constant for the heap type being moved (IT_MRS, ... etc.)
;
;Exit:
;	none.
;Modifies:
;	May modify BX, CX, or PSW - no others
;***************************************************************************
cProc	B$IHeapEntryMoved,<PUBLIC,NEAR,NODATA>,<AX,DX,ES,SI,DI>
cBegin

	mov	si,bx
	cmp	cl,IT_NO_OWNERS_BDP	;a bdp entry being moved?
	jnz	Not_Bdp			;brif not

	add	[bx.BDP_pbCur-2],ax	;adjust the pbCur field
	jmp	short Entry_Moved_Exit	;that's all, folks

Not_Bdp:
	mov	di,ax			;communicate adjustment factor in DI
	mov	ax,[bx-2]		;put cbLogical (table end offset) in ax
	mov	bx,[bx]
	push	ax			;save until Entry_Moved_Cont
	push	bx			;save until Entry_Moved_Cont
	push	cx			;save until Entry_Moved_Cont

	cmp	cl,IT_COMMON_VALUE	
	jae	Update_Stack_Ptrs	;brif we have to update stack ptrs, or
					;var or common table
	cmp	cl,IT_PRS		; no bd's in prs tables
	jz	Entry_Moved_Cont	; brif prs table

	cCall	AdjustITable,<bx,ax,cx> ;adjust the table, whatever it is.

Entry_Moved_Cont:
	pop	cx			;restore type constant
	pop	bx			;restore start of range
	pop	dx

FHD_TBL EQU NOT IT_M_INTERP AND (IT_MRS OR IT_PRS OR IT_COMMON_VALUE OR IT_VAR)
	test	cl,FHD_TBL
	jz	Entry_Moved_Exit	;brif entry can't contain far heap desc.

	;prs and mrs tables contain bdl's - - - get far heap to update these
	add	dx,bx			;dx is now end of range
	mov	ax,di			;adjustment factor
	call	B$FHAdjDesc

Entry_Moved_Exit:
cEnd

Update_Stack_Ptrs:
	jnz	Upd_Stk_Ptrs_Cont	;brif cl != IT_COMMON_VALUE
	call	AdjustCommon		;update backpointers to any string 
					;descriptors or string array descriptors
					;in given IT_COMMON_VALUE heap entry
	jmp	short Entry_Moved_Cont
Upd_Stk_Ptrs_Cont:
	DbAssertRelB  cl,z,IT_VAR,RT,<B$IHeapEntryMoved: cl == IT_VAR expected>
	call	AdjustVarTable		;update backpointers to any string 
					;descriptors or string array descriptors
					;in given IT_VAR heap entry
	jmp	short Entry_Moved_Cont

;***
;BdCompress - Compress a Runtime Heap entry
;Purpose:
;	Call B$LhRealloc to reduce cbPhysical to cbLogical for a Bd whose
;	pb field is at a given location.
;	Called via B$LHForEachEntry by BdCompressAll.
;
;	Note that variable tables are handled specially - - they're trimmed
;	back so that they keep up to VAR_EXTRA free space at the end of the
;	table to maximize the change of CONTinuing after variables are added.
;Entry:
;	BX = pointer to owner of a local heap entry - - - for interpreter
;		buffers, that amounts to a pointer to the 'pb' field of the
;		owning bd.
;	DL = heap entry type.
;Exit:
;	none.
;Preserves:
;	CX,SI
;Exceptions:
;	none.
;
;***************************************************************************
cProc	BdCompress,<NEAR,NODATA>
cBegin	BdCompress
	DbChk	Heaps			;ife RELEASE & checking enabled, check
					;	Local & Far Heaps for problems
	test	dl,IT_M_INTERP		;is this an interpreter buffer?
	jz	BdCompress_Exit

	dec	bx			;turn bx into a real pBd (for cmp below)
	dec	bx

	mov	ax,[bx.BD_cbLogical]
	cmp	dl,IT_VAR		;is this a variable table?
	jnz	BdCompress_Cont		;  brif not
	
	add	ax,VAR_EXTRA		;want to have up to VAR_EXTRA bytes
					;  left in each var table to improve
					;  chances of user adding a few 
					;  variables and still CONTinuing
	cmp	ax,[bx.BD_cbPhysical]
	jae	BdCompress_Exit		;brif cbPhysical <= size we want buffer
					;  to be - - - leave buffer alone
BdCompress_Cont:
	lea	dx,[ps.PS_bdpDst.BDP_cbLogical]
	cmp	bx,dx			;brif not special parser buffer that 
	jnz	BdCompress_Cont1	;  must always have a minimal amount

	cmp	ax,CB_PCODE_MIN		;never trim below this minimum
	ja	BdCompress_Cont1	;brif cbLogical > Minimum required

DbAssertRel [bx.BD_cbPhysical],ae,CB_PCODE_MIN,RT,<BdCompress:Parser buffer is too small>
	mov	ax,CB_PCODE_MIN
BdCompress_Cont1:
	push	cx			;preserve for caller
	push	si			;preserve for caller
	mov	[bx.BD_cbPhysical],ax	;set new desired cbPhysical
	mov	si,[bx.BD_pb]		;pointer to start of data in buffer
	call	B$LHREALC		;reduce entry to cbLogical size
					;  MUST succeed and CANNOT cause
					;  heap movement, because it is either
					;  reducing entry size or doing nothing

	pop	si
	pop	cx
BdCompress_Exit:
cEnd	BdCompress

;***
;BdCompressHeap - Compress all Runtime Heap entries in currently active heap
;Purpose:
;	Same as BdCompressAll (below), but only crunches bd's in the currently
;	active heap (either the local heap or the variable heap).
;Input:
;	none.
;Output:
;	none.
;Modifies:
;	no permanent registers.
;Exceptions:
;	Chance of string space corrupt.
;***************************************************************************
cProc	BdCompressHeap,<NEAR,NODATA>
cBegin
	mov	cx,RTOFFSET BdCompress
	call	B$LHForEachEntry	;compress all bd's down to cbLogical
					;  (effect is to create free blocks
					;   out of extraneous space in bd's)
	cmp	[b$fVarHeapActive],FALSE
	jnz	BdCompressHeap_Exit	;don't compact the variable heap
					;  only runtime init. does that - - 
					;  B$NHCPCT assumes local heap active.
	call	B$NHCPCT		;compact Local Heap and String Space
BdCompressHeap_Exit:
	DbChk	Heaps
cEnd
;***
;BdCompressAll - Compress all Runtime Heap entries
;Purpose:
;	To increase the speed of BdGrow, we keep a little free
;	space at the end of each heap entry.  When the program
;	begins execution, this routine is called to
;	release all this space and compact interpreter-specific entries
;	to the top of the Runtime Heap. 
;	Note that this routine is ONLY called by interpreter code, 
;	and never by the shared-runtime code.
;
;	NOTE: after this operation is complete, the 'pbCurrent' field in
;		bdp's will still be correct and useable, assuming that
;		such pointers weren't pointing beyond cbLogical ...
;Input:
;	none.
;Output:
;	none.
;Modifies:
;	no permanent registers.
;Exceptions:
;	Chance of string space corrupt.
;
;***************************************************************************
cProc	BdCompressAll,<PUBLIC,FAR,NODATA>
cBegin
	call	BdCompressHeap		;compress the active heap
	call	B$TglHeapSpt		;activate the other heap
	call	BdCompressHeap		;compress the active heap
	call	B$TglHeapSpt		;reactivate the originally active heap
cEnd

;***
;BdAdjust(pBd)
;	This routine takes a pointer to a bd as a parameter and assumes
;	that an adjustment factor (the bd is being moved) is in DI.
;	It calls a heap manager routine which updates the entry backpointer,
;	if the bd is an owner (i.e., if the pb field is not NULL).
;Entry:
;	pBd - pointer to a bd that's being moved
;	DI contains adjustment factor it's being moved by
;Exit:
;	none.
;Modifies:
;	none. (no permanent registers)
;Exceptions:
;	if anything wrong with heap entry for this bd, can end up calling
;	the non-trapable "String Space Corrupt" error.
;***************************************************************************
cProc	BdAdjust,<PUBLIC,FAR,NODATA>
	parmW	pBd
cBegin	BdAdjust
	mov	bx,[pBd]
	mov	ax,[bx.BD_pb]
	cmp	ax,NULL
	jz	BdAdjust_Done

	call	B$ILHADJ		;get heap manager to do adjustment
BdAdjust_Done:
cEnd	BdAdjust
	page


;***
;BdAllocVar - Allocate a Runtime Heap entry in the variable heap
;Purpose:
;	Allocate an Interpreter-specific Heap entry from the variable heap.
;	Uses the same interface and BdAlloc (see below).
;Entry, Exit, Modifies:
;	Same as BdAlloc (see below).
;Note: Shares and exits via BdAlloc, below
;***************************************************************************
cProc	BdAllocVar,<PUBLIC,FAR,NODATA>
cBegin	<nogen>
	DbAssertRel grs.GRS_otxCONT,z,UNDEFINED,RT,<BdAllocVar: CAN continue>
	call	B$TglHeapSpt		;make variable heap the active one
cEnd	<nogen>				;fall into BdAlloc, below

;***
;BdAlloc - Allocate a Runtime Heap entry
;Purpose:
;	Allocate an Interpreter-specific Heap entry from the Runtime
;	Heap.
;	Note that this routine should ask for only the amount of space asked
;	for; growing a buffer will increase requests to some minimal block size,
;	but many buffers need to be initially allocated to some minimal 
;	(possibly zero) size.
;	NOTE: current heap manager interface demands that the owner-to-be
;	should not be subject to heap movement (i.e., not in heap, or
;	heap locked). 
;Entry:
;	parm: bd *pbdOwner - points to owner-to-be of new heap entry
;	parm: ushort cbSize - number of bytes needed
;if	NOT FV_LMEM
;	parm: char interpType - type of interp. table (IT_VALUE etc)
;endif
;Exit:
;	if entry was successfully allocated:
;	   pbdOwner->cbLogical = cbSize
;if	FV_LMEM
;	   pbdOwner->ppb = ptr to ptr to new heap entry (and is now owner)
;else
;	   pbdOwner->pb = pointer to new heap entry (and is now a heap owner)
;	   pbdOwner->cbPhysical = cbSize
;endif
;	   [AX] = TRUE (non-zero)
;	else
;	   [AX] = FALSE (0) (Out of memory)
;Modifies:
;	none  (NOTE: DOES modify ES)
;
;***************************************************************************
cProc	BdAlloc,<PUBLIC,FAR,NODATA>,<SI>
	parmW	pbdOwner
	parmW	cbSize
	parmB	interpType
cBegin
	DbShiftLH			;ife RELEASE cause some heap movement
	DbChk	Heaps
	mov	dl,[interpType]
	mov	cx,[cbSize]
	mov	bx,[pbdOwner]
	DbChk	BdNotOwner,bx		;ensure that given bd isn't an owner now
	mov	[bx.BD_pb],NULL		;in case allocation fails and caller
					;  blindly calls BdFree with this bd
	DbOMCnt	BD_END
	xchg	bx,cx			;input order required by B$ILHALC
	inc	cx			;'owner' to heap manager is the actual
	inc	cx			;  pointer to the heap entry, not a pbd
	call	B$ILHALC		;call heap manager to allocate memory
	jc	BD_Crunch_BDs		;brif OM return; trim bd's, try again

BdAlloc_Success:
	mov	bx,[pbdOwner]		;assumes bdOwner not moved by allocation
	mov	[bx.BD_pb],si		;SI is data ptr returned from B$ILHALC
	mov	ax,[cbSize]		;save requested size as both logical
	mov	[bx.BD_cbLogical],ax	;  and physical size, and return it as
	mov	[bx.BD_cbPhysical],ax	;  our non-zero (i.e., 'TRUE') result
	mov	al,TRUE			;in case input size was zero
BD_END:
	cmp	[b$fVarHeapActive],FALSE
	jz	BdAlloc_Exit		;brif variable heap not active

	call	B$TglHeapSpt		;reactivate the local heap
BdAlloc_Exit:
cEnd

BD_Crunch_BDs:
	push	dx
	push	cx
	call	far ptr BdCompressAll	;trim bd's, compress heap space
	pop	cx
	pop	dx
	call	B$ILHALC		;try allocation again
	jnc	BdAlloc_Success		;  brif it worked this time
	xor	ax,ax			;OM error return
	jmp	short BD_END

;***
;BdFree(pbdOwner) - Release a Heap entry
;Purpose:
;	Release a Runtime Heap entry. If pbdOwner.BD_pb is NULL,
;	just return (as input wasn't really an owner).
;Entry:
;	parm: bd *pbdOwner - points to owner of new heap entry
;
;***************************************************************************
cProc	BdFree,<PUBLIC,FAR,NODATA>,<SI>
	parmW	pbdOwner
cBegin
	mov	bx,[pbdOwner]
	mov	si,[bx.BD_pb]
	cmp	si,NULL
	jz	BdFree_Exit		;brif bd isn't an owner

	mov	[bx.BD_pb],NULL
	call	B$LHDALC
BdFree_Exit:
cEnd

;***
;BdChgContents(pbd, psdNew) - Change contents of a buffer
;Purpose:
;	Change the contents of a given buffer. Note that the buffer may or
;	may not be an owner already; if it is an owner, it will be Free'd.
;	The buffer will then be allocated, and the input sd contents copied in.
;
;	NOTE: psdNew must not point into a heap entry!
;Entry:
;	parm: bd *pbd - points to current owner of heap entry
;	parm: bd *psdNew -   points to sd, contents of which are to be put
;				in the input bd.
;Exit:
;	if operation successful
;		[AX] = TRUE (non-zero)
;	else
;		[AX] = FALSE (0) (Out of memory), and the original contents
;							of the bd are lost.
;***************************************************************************
cProc	BdChgContents,<PUBLIC,FAR,NODATA>,<SI,DI>
	parmW	pbd
	parmW	psdNew
cBegin
	mov	si,[pbd]
	cCall	BdFree,<si>		;free original contents if any

	mov	di,[psdNew]
	DbChk	PtrNotInHeap,di

	mov	cx,[di.SD_cb]
	push	cx			;save across call

	push	si
	push	cx
	PUSHI	dx,IT_NO_OWNERS
	call	BdAlloc			;alloc to size of desired contents

	pop	cx
	or	ax,ax
	jz	BdChgContents_Exit	;brif OM error on allocation

	mov	ax,[si.BD_pb]
	mov	bx,[di.SD_pb]
	cCall	CopyBlk,<bx,ax,cx>	;copy sd contents into bd
BdChgContents_Exit:
cEnd

;***
;BdChgOwner(pbdOwner, pbdNew) - Change the owner of a Heap entry
;BdChgOwner_NoCopy(pbdOwner, pbdNew) - Change the owner of a Heap entry
;Purpose:
;	Change the owner of an Interpreter-specific Heap entry. If 
;	pbdOwner.BD_pb is NULL, just return (as it wasn't really an owner to 
;	begin with).
;	BdChgOwner copies the bd contents to the new bd.
;	BdChgOwner_NoCopy is provided as a speed improvement, and should be
;		called in cases where the bd has already been copied BEFORE
;		this routine is called.
;
;	NOTE: This routine is guaranteed not to cause heap movement to occur.
;
;	NOTE: This routine must be called AFTER a block containing the bd is
;		moved if such movement is to take place, because this routine
;		changes the contents of bdOwner to indicate that it's no longer
;		an owner.
;Entry:
;	parm: bd *pbdOwner - points to current owner of heap entry
;	parm: bd *pbdNew -   points to new owner of heap entry
;
;***************************************************************************
	PUBLIC BdChgOwner
BdChgOwner:
	mov	cx,SIZE BD		;non-zero - - - do the copy
	SKIP2_PSW			;skip to start of common code
	PUBLIC BdChgOwner_NoCopy
BdChgOwner_NoCopy:
	xor	cx,cx
cProc	Chg_The_Owner,<FAR,NODATA>,<SI>
	parmW	pbdOwner
	parmW	pbdNew
cBegin
	mov	si,[pbdOwner]
	cmp	[si.BD_pb],NULL
	jz	BdChg_Exit

	DbChk	BdOwner,si		;ensure that given bd is an owner

	jcxz	BdChg_CopyDone		;brif caller already did this copy

	push	si
	push	pbdNew
	push	cx			;set to SIZE BD for BdChgOwner
	call	CopyBlk
BdChg_CopyDone:
	mov	cx,[pbdNew]
	inc	cx			;to heap manager, 'owner' is the actual
	inc	cx			;  pointer to heap data, not a pbd
	push	si			;si is an input to B$LHChgBakPtr
	mov	si,[si.BD_pb]
	call	B$LHChgBakPtr
	pop	si			;so we can set bd.pb to NULL

	mov	[si.BD_pb],NULL		;mark that this is no longer an owner
BdChg_Exit:
cEnd

;***
;EnsPhysicalSize - ensure physical size of near heap >= ax
;Purpose:
;	Change physical size of an Interpreter-specific Heap entry if necessary.
;	This is used by BdGrow and BdCheckFree. 
;	Note that this is not an external entry point, only for use
;	within this module, and can thus use register calling conventions.
;
;	NOTE: current heap manager interface demands that the owner
;	should not be subject to heap movement. 
;Entry:
;	[di] - points to owner of heap entry
;	[ax] = new total size requested for the buffer (i.e., new minimum
;		cbPhysical desired).
;Exit:
;	if enough memory is available:
;          [ax] = TRUE (non-zero)
;	   [bx] = new value for cbLogical (i.e., [ax] exit = entry)
;	otherwise,
;	   [ax] = 0
;	
;***************************************************************************
EnsPhysicalSize PROC	NEAR
	DbChk	BdOwner,di		;ensure that given bd is an owner
	DbChk	Heaps
	push	ax			;save input requested size
	DbOMCnt Ens_End2
	cmp	ax,[di.BD_cbPhysical]
	jbe	NoChange		;branch if already big enough

	push	si			;save caller's si
	push	ax			;in case initial try fails
	sub	ax,[di.BD_cbPhysical]	;ax=amount to grow
	cmp	ax,CBBUFBLOCK
	jae	Big_Enough		;branch if growing by significant amount

	mov	ax,CBBUFBLOCK		;never grow by less than this amount
Big_Enough:
	add	ax,[di.BD_cbPhysical]	;ax=(hopefully) new cbPhysical
	push	ax			;save (hopefully) new cbPhysical
	mov	si,[di.BD_pb]
	call	B$LHREALC		;call heap manager to realloc
	pop	bx			;size we realloced to
	or	ax,ax			;test result
	jz	Ens_Crunch_BDs		;brif realloc failed

	pop	cx			;clean stack
Ens_Phy_Success:
	mov	[di.BD_cbPhysical],bx
	mov	[di.BD_pb],si		;in case realloc moved the entry
Ens_End1:
	pop	si			;restore caller's si
Ens_End2:
	pop	bx			;restore input size for retval
	ret

NoChange:
	mov	al,TRUE			;ensure TRUE return, even if passed ax=0
	jmp	short Ens_End2

EnsPhysicalSize ENDP

Ens_Crunch_BDs:
	call	far ptr BdCompressAll	;trim all bd's, compress heaps
	pop	ax			;input to B$LHREALC
	push	ax			;save for return
	mov	si,[di.BD_pb]		;may be trashed on error return
	call	B$LHREALC
	pop	bx			;cb we tried to realloc to
	or	ax,ax			;did we succeed this time?
	jz	Ens_End1		;  brif not
	jmp	short Ens_Phy_Success	;succeeded this time - - go wrap up

;***
;BdGrowVar - Grow a Runtime Heap entry in the variable heap
;Purpose:
;	Same as BdGrow (below), but for an entry in the variable heap.
;	Uses the same interface and BdGrow (see below).
;Entry, Exit, Modifies:
;	Same as BdGrow (see below).
;Note: Shares and exits via BdGrow, below
;***************************************************************************
cProc	BdGrowVar,<PUBLIC,FAR,NODATA>
cBegin	<nogen>
	DbAssertRel grs.GRS_otxCONT,z,UNDEFINED,RT,<BdlGrowVar: CAN continue>
	call	B$TglHeapSpt		;make variable heap the active one
cEnd	<nogen>				;fall into BdGrow, below

;***
;BdGrow - Increase the logical size of a Heap entry
;Purpose:
;	Change logical size of an Interpreter-specific Heap entry.  This can
;	result in the movement of this and other heap entries as well
;	as strings.  
;	When this routine actually needs to grow the physical size
;	of the heap, it grows more than needed for this request, to
;	reduce heap thrashing.
;
;	NOTE: current heap manager interface demands that the owner
;	should not be subject to heap movement. 
;Entry:
;	parm: bd *pbdOwner - points to owner of heap entry
;	parm: ushort cbGrow - number of bytes needed
;Exit:
;	if enough memory is available:
;	   pbdOwner->cbLogical += cbGrow,
;	   pbdOwner->cbPhysical >= pbdOwner->cbLogical
;	   [AX] = TRUE (non-zero)
;	else
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************

cProc	BdGrow,<PUBLIC,FAR,NODATA>,<di>
	parmW	pbdOwner
	parmW	cbGrow
cBegin
	mov	di,[pbdOwner]		;di points to bd descriptor
	mov	ax,[cbGrow]		;[AX] == increase desired
	add	ax,[di.BD_cbLogical]	;[AX] == new logical size
	jc	GrowOmErr		;branch if overflow (can't grow > 64k)

	;*****************************
	;NOTE: BdRealloc jumps in here
	;*****************************
BdRealloc1:
	call	EnsPhysicalSize		;change physical size (inputs ax & di)
	or	ax,ax			;test boolean result
	jz	BdGrow_End		;brif out-of-memory case
	
	mov	[di.BD_cbLogical],bx	;new cbLogical - successful return
BdGrow_End:
	cmp	[b$fVarHeapActive],FALSE
	jz	BdGrow_Exit		;brif variable heap not active

	call	B$TglHeapSpt		;reactivate the local heap
BdGrow_Exit:
cEnd

GrowOmErr:
	xor	ax,ax
	jmp	short BdGrow_End

;***
;BdRealloc - Change the logical size of a Heap entry
;Purpose:
;	Change logical size of an Interpreter-specific Heap entry.  This can
;	result in the movement of this and other heap entries as well
;	as strings.  
;	When this routine actually needs to grow the physical size
;	of the heap, it grows more than needed for this request, to
;	reduce heap thrashing.
;
;	NOTE: current heap manager interface demands that the owner
;	should not be subject to heap movement. 
;Entry:
;	parm: bd *pbdOwner - points to owner of heap entry
;	parm: ushort cbLogicalNew - new size of heap entry
;Exit:
;	if enough memory is available:
;	   pbdOwner->cbLogical = cbLogicalNew,
;	   pbdOwner->cbPhysical >= pbdOwner->cbLogical
;	   [AX] = TRUE (non-zero)
;	else
;	   pbdOwner->cbLogical is unchanged
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************

cProc	BdRealloc,<PUBLIC,FAR,NODATA>,<di>
	parmW	pbdOwner
	parmW	cbNew
cBegin
	mov	di,[pbdOwner]		;di points to bd descriptor
	mov	ax,[cbNew]		;[AX] == increase desired
	jmp	SHORT BdRealloc1
cEnd	<nogen>

;***
;BdCheckFree - Make sure buffer has some free space
;Purpose:
;	This is identical to BdGrow, but it does not alter the
;	descriptor's cbLogical field.  Some typical cases when it is
;	called include:
;	1-  Before calling BdAppend to copy from one bd to another.
;	    By calling this first, we know BdAppend won't have to
;	    grow the heap entry, causing movement, which could invalidate
;	    BdAppend's pb argument.
;	2-  When the caller is about to do an operation which will
;	    append information to a bd, but the caller doesn't know
;	    exactly how many bytes will be added, but an upper limit is known.
;
;	NOTE: current heap manager interface demands that the owner
;	should not be subject to heap movement. 
;Entry:
;	parm: bd *pbdOwner - points to owner of heap entry
;	parm: ushort cbFree - number of free bytes needed
;Exit:
;	pbdOwner->cbLogical is ALWAYS UNCHANGED
;	If enough memory is available:
;	   pbdOwner->cbPhysical >= pbdOwner->cbLogical + cbFree
;	   [AX] = TRUE (non-zero)
;	else
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************
cProc	BdCheckFree,<PUBLIC,FAR,NODATA>,<DI>
	parmW	pbdOwner
	parmW	cbFree
cBegin
	mov	di,[pbdOwner]		;di points to bd descriptor
	mov	ax,[cbFree]		;[AX] == increase desired
	add	ax,[di.BD_cbLogical]	;[AX] == resulting size
	jc	CheckOmErr		;branch if overflow (can't grow > 64k)

	call	EnsPhysicalSize		;change physical size (inputs ax & di)
BdCheck_End:
cEnd

CheckOmErr:
	xor	ax,ax
	jmp	short BdCheck_End

;***
; boolean BdShiftRight(pbd, obStart, cb)
;
; Purpose:
;	Grow the buffer descriptor, and shift its contents right
;	(copying content to higher addresses) starting at offset
;	obStart until the end of the buffer.
;
;	NOTE: current heap manager interface demands that the owner
;	should not be subject to heap movement. 
;
; Entry:
;	parmW pbd points to the buffer descriptor
;	parmW obStart = byte offset for 1st byte to be shifted right
;	parmW cb = number of bytes each byte is to be shifted
;
; Exit:
;	If not enough memory can be obtained,
;		[AX] = FALSE
;	else
;		pbdDst->cbLogical is updated
;		[AX] = TRUE
;
;   Before BdShiftRight(pbd, 2, 2):
;	high memory 	
;	  pbd->cbLogical------->+-----+
;				|  E  |
;				|  D  |
;				|  C  |
;				|  B  |
;				|  A  |
;	low memory		+-----+
;
;   After:
;	high memory 	
;	  pbd->cbLogical------->+-----+
;				|  E  |
;				|  D  |
;				|  C  |
;				|  D  |
;				|  C  |
;				|  B  |
;				|  A  |
;	low memory		+-----+
;
;***************************************************************************
cProc	BdShiftRight,<PUBLIC,FAR,NODATA>,<SI,DI>
	parmW pbd
	parmW obStart
	parmW cb
cBegin
	push	pbd
	push	cb
	call	BdCheckFree		;1st grow the buffer
	or	ax,ax
	je	BdShiftExit		;branch if out-of-memory, return 0
	mov	bx,pbd			;bx -> descriptor
	mov	cx,[bx.BD_cbLogical]	;[CX] = current size of buffer
	mov	si,[bx.BD_pb]		;si points to start of buffer
	add	si,cx			;si points beyond end of current content
	dec	si			;si points to 1st byte to copy
	mov	di,si
	mov	ax,cb
	add	di,ax			;di points to dst for 1st byte to copy
	add	[bx.BD_cbLogical],ax	;update size of buffer
	sub	cx,obStart		;[CX] = number of bytes to copy
	jcxz	Copy0Bytes
	push	ds
	pop	es			;es=ds
	std				;copy from high to low address
	rep movsb			;do the block copy
	cld
Copy0Bytes:
	mov	ax,TRUE
BdShiftExit:
cEnd

;***
; boolean BdShiftLeft(pbd, obStart, cb)
;
; Purpose:
;	Shrink the buffer descriptor, and shift its contents left
;	(copying content to lower addresses) starting at offset
;	obStart until the end of the buffer.
;
; Entry:
;	parmW pbd points to the buffer descriptor
;	parmW obStart = byte offset for 1st byte to be deleted
;	parmW cb = number of bytes to be deleted
;
; Exit:
;	pbdDst->cbLogical is updated
;	no return value
;
;   Before BdShiftLeft(pbd, 2, 2):
;	high memory 	
;	  pbd->cbLogical------->+-----+
;				|  E  |
;				|  D  |
;				|  C  |
;				|  B  |
;				|  A  |
;	low memory		+-----+
;
;   After:
;	high memory 	
;	  pbd->cbLogical------->+-----+
;				|  E  |
;				|  B  |
;				|  A  |
;	low memory		+-----+
;
;***************************************************************************
cProc	BdShiftLeft,<PUBLIC,FAR,NODATA>,<SI,DI>
	parmW pbd
	parmW obStart
	parmW cb
cBegin
	mov	bx,pbd			;bx -> descriptor
	mov	di,[bx.BD_pb]		;di points to start of buffer
	add	di,obStart		;di points to 1st byte to delete
	mov	si,di
	add	si,cb			;si points beyond last byte to delete
	mov	cx,[bx.BD_cbLogical]	;[CX] = current size of buffer
	sub	cx,cb			;cx = new size of buffer
	mov	[bx.BD_cbLogical],cx	;update descriptor
	sub	cx,obStart		;cx = # bytes to copy
	jcxz	LeftExit		;brif 0 bytes to copy
	push	ds
	pop	es			;es=ds
	rep movsb			;do the block copy
LeftExit:
cEnd

;***
; boolean BdAppend(pbdDst, pbSrc, cb)
;
; Purpose:
;	Append a string of bytes to a Buffer Descriptor.
;	If this is preceeded by a call to BdCheckFree(pbdDst, cb)
;	then pbSrc can point within another heap entry with no
;	fear of movement before the copy is complete.  Otherwise,
;	pbSrc had better not point within a heap entry.
;
;	NOTE: current heap manager interface demands that the owner
;	should not be subject to heap movement. 
;
; Entry:
;	parmW pbdDst points to the destination buffer descriptor
;	parmW pbSrc points to 1st byte to be copied into buffer
;	parmW cb = number of bytes to be copied
;
; Exit:
;	If not enough memory can be obtained,
;		[AX] = FALSE
;	else
;		pbdDst->cbLogical is updated
;		[AX] = TRUE
;
;***************************************************************************
cProc	BdAppend,<PUBLIC,FAR,NODATA>,<SI,DI>
	parmW pbdDst
	parmW pbSrc
	parmW cb
	localW pbDst
	localW cbTemp
cBegin
	push	pbdDst
	push	cb
	call	BdCheckFree
	or	ax,ax
	je	BdAppendExit		;branch if out-of-memory, return 0
	mov	cx,cb			;[CX] = # bytes to copy
	mov	di,pbdDst		;di -> destination descriptor
	mov	ax,[di.BD_cbLogical]	;ax = current size of buffer
	add	[di.BD_cbLogical],cx	;update size of buffer
	mov	di,[di.BD_pb]		;di points to start of dest buffer
	add	di,ax			;add new bytes at end of buffer
	mov	si,pbSrc		;si = source byte ptr
	push	ds
	pop	es			;es=ds
	rep movsb			;do the block copy
	mov	ax,TRUE
BdAppendExit:
cEnd


;-----------------------------------------------------------------
;---   Large Far Heap Buffer Descriptor Management Routines    ---
;-----------------------------------------------------------------


FAR_EXTRA = 512		;never grow a far heap entry by less than 512 bytes

;***
;AllocBdl - Allocate a Far Heap entry (workhorse for BdlAlloc)
;AllocBdl_Sb - same, but allocates a given sb for this
;Purpose:
;	Allocate a Heap entry from the Far Heap.  This can cause
;	movement of Runtime and String heap entries.
;	Note that this routine should ask for only the amount of space asked
;	for; growing a buffer will increase requests to some minimal block size,
;	but many buffers need to be initially allocated to some minimal 
;	(possibly zero) size.
;Entry:
;	di = pbdlOwner - points to owner of new heap entry
;	si = cbSize - number of bytes needed
;	For EB versions, bx = type constant for type of bdl buffer
;	For AllocBdl_Sb, cx = sb to use
;Exit:
;	if entry was successfully allocated:
;	   pbdlOwner->cbLogical = cbSize
;	   pbdlOwner->cbPhysical = cbSize
;	   [AX] = TRUE (non-zero)
;	   pbdlOwner->status != NOT_OWNER
;	else
;	   [AX] = FALSE (0) (Out of memory)
;	PSW.Z is set on exit based on an 'OR AX,AX' instruction
;
;***************************************************************************
cProc	AllocBdl,<NEAR,NODATA>
cBegin	<nogen>
	mov	cx,0			; use any sb that's free
cEnd	<nogen>
cProc	AllocBdl_Sb,<NEAR,NODATA>
cBegin
	mov	ax,si

	DbAssertRel ax,be,0FFF0H,RT,<BdlAlloc: caller asked for more than FFF0H>
	;The above assertion is based on the problem where a request to
	;   B$IFHAlloc for greater than 0FFF0H bytes will be rounded UP to past
	;   64k, with no error reported.
	xor	dx,dx			;DX:AX is input size to B$IFHAlloc
	mov	bx,di
	DbChk	BdlNotOwner,di
	mov	[bx.BDL_cbLogical],ax
	call	B$IFHAlloc		;allocate a far heap entry (0 if can't)
	or	ax,ax			;set zero flag for caller
cEnd	AllocBdl


;***
;BdlAlloc - Allocate a Far Heap entry
;Purpose:
;	Allocate a Heap entry from the Far Heap.  This can cause
;	movement of Runtime and String heap entries.
;	Note that this routine should ask for only the amount of space asked
;	for; growing a buffer will increase requests to some minimal block size,
;	but many buffers need to be initially allocated to some minimal 
;	(possibly zero) size.
;
;	[5] Note that at least some callers depend on the new block being zero-
;	[5] filled (EB varmgr code, for one).
;Entry:
;	parm: bdl *pbdlOwner - points to owner of new heap entry
;	parm: ushort cbSize - number of bytes needed
;Exit:
;	if entry was successfully allocated:
;	   pbdlOwner->cbLogical = cbSize
;	   pbdlOwner->cbPhysical = cbSize
;	   [AX] = TRUE (non-zero)
;	   pbdlOwner->status != NOT_OWNER
;	else
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************
cProc	BdlAlloc,<PUBLIC,FAR,NODATA>,<si,di>
	parmW	pbdlOwner
	parmW	cbSize
cBegin
	DbOMCnt	BdlAlloc_Exit
	mov	di,[pbdlOwner]
	mov	si,[cbSize]
	cCall	AllocBdl
	jnz	BdlAlloc_Exit		;brif success

	call	far ptr BdCompressAll	;trim bd's, compress heap space
	cCall	AllocBdl
BdlAlloc_Exit:
cEnd

;***
;BdlAllocSb - Allocate a Far Heap entry, given a desired sb
;Purpose:
;	Same as BdlAlloc, but accepts as a third parm the sb value that
;	is to be used.
;	Added as revision [13].
;Entry:
;	parm: bdl *pbdlOwner - points to owner of new heap entry
;	parm: ushort cbSize - number of bytes needed
;	parm: ushort sbInput - sb we must use for this allocation
;			(caller guarantees this is unallocated).
;Exit:
;	if entry was successfully allocated:
;	   pbdlOwner->cbLogical = cbSize
;	   pbdlOwner->cbPhysical = cbSize
;	   [AX] = TRUE (non-zero)
;	   pbdlOwner->status != NOT_OWNER
;	else
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************
cProc	BdlAllocSb,<PUBLIC,FAR,NODATA>,<si,di>
	parmW	pbdlOwner
	parmW	cbSize
	parmW	sbInput
cBegin
	DbOMCnt BdlAllocSb_Exit
	mov	di,[pbdlOwner]
	mov	si,[cbSize]
	mov	cx,[sbInput]		
	cCall	AllocBdl_Sb
BdlAllocSb_Exit:
cEnd

;***
;BdlFree - Release a far Heap entry
;Purpose:
;	Release a far Heap entry. If bdl is not an owner, this routine just
;	returns, with no error.
;Entry:
;	parm: bdl *pbdlOwner - points to owner of new heap entry
;Exit:
;	bdl is released; pbdlOwner->status = NOT_OWNER
;
;***************************************************************************
cProc	BdlFree,<PUBLIC,FAR,NODATA>
	parmW	pbdlOwner
cBegin
	mov	bx,[pbdlOwner]
	cmp	[bx.BDL_status],NOT_OWNER
	jz	BdlFree_Exit		;brif bdl already free

	DbChk	BdlOwner,bx		;ensure that given bdl is an owner
	push	bx
	call	B$FHDealloc		;free an allocated far heap entry
	pop	bx
	mov	[bx.BDL_status],NOT_OWNER ;indicate that bdl is not an owner
BdlFree_Exit:
cEnd

;***
;BdlChgOwner(pbdlOwner, pbdlNew) - Change the owner of a Far Heap entry
;Purpose:
;	Change the owner of a Far Heap entry. If pbdlOwner.BDL_status is 
;	NOT_OWNER, just return (as it wasn't really an owner to begin with).
;
;	NOTE: This routine is guaranteed not to cause heap movement to occur.
;
;	NOTE: This routine must be called AFTER a block containing the bdl is
;		copied, as the far heap manager modifies the FHD according to
;		its original location. This copy MUST be done by the caller
;		prior to this routine being called.
;		Note also that it is NOT safe to block copy a range containing
;		multiple bdl's and then call this routine once per bdl - - -
;		Since the far heap code chains all bdl's together, a call to
;		BdlChgOwner can cause another bdl to be modified (in the 
;		'status' a.k.a. 'pNext' field).
;Entry:
;	parm: bdl *pbdlOwner - points to current owner of far heap entry
;	parm: bdl *pbdlNew -   points to new owner of far heap entry
;Exit:
;	none.
;
;***************************************************************************
cProc	BdlChgOwner,<PUBLIC,FAR,NODATA>,<SI>
	parmW	pbdlOwner
	parmW	pbdlNew
cBegin
	mov	si,[pbdlOwner]
	cmp	[si.BDL_status],NOT_OWNER
	jz	BdlChg_Exit		;brif bdl wasn't an owner

	DbChk	BdlOwner,si		;ensure that bdlOwner is an owner

	mov	dx,si			;pFHD for FHD that's being moved
	mov	cx,[pbdlNew]
	sub	cx,si			;cx = pNew - pOld (adjustment factor)
	call	B$FHAdjOneDesc

	mov	[si.BDL_status],NOT_OWNER
BdlChg_Exit:
cEnd

;***
;BdlRealloc - reallocate a Far Heap entry
;Purpose:
;	reallocate a Heap entry from the Far Heap.  This can cause
;	movement of String and Runtime heap entries.
;
;	[5] Note that at least some callers depend on additional space being
;	[5] zero-filled (EB varmgr code, for one).
;Entry:
;	parm: bdl *pbdlOwner - points to owner of heap entry
;	parm: ushort cbNew - new buffer size desired
;Exit:
;	if entry was successfully reallocated:
;	   pbdlOwner->cbLogical = cbNew
;	   pbdlOwner->cbPhysical >= cbNew
;	   [AX] = TRUE (non-zero)
;	else
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************
cProc	BdlRealloc,<PUBLIC,FAR,NODATA>,<di>
	parmW	pbdlOwner
	parmW	cbNew
	localB	fTryAgain
cBegin
	mov	[fTryAgain],TRUE
	DbOMCnt	BdlRealloc_Exit
	mov	di,[pbdlOwner]
	mov	bx,[di.BDL_cPhysical]
	SHIFT	H,L,bx,4		;shift left to convert cPara to cbytes
	mov	ax,[cbNew]
	cmp	bx,ax
	jae	Change_cbLogical	;brif physical size is big enough

	cmp	ax,0FFE0H		;if ask far heap for > FFE0H, it will
					;  round request up to para boundary ...
	jbe	BdlRealloc_Cont		;brif request not too large

	xor	ax,ax			;Out of Memory return
	jmp	short BdlRealloc_Exit

BdlRealloc_Crunch:
	cmp	[fTryAgain],FALSE
	jz	BdlRealloc_Exit		;brif we've already tried this - give up

	mov	[fTryAgain],FALSE	;remember this is the 2nd attempt
	call	far ptr BdCompressAll	;trim all bd's, compress heaps,
	mov	ax,[cbNew]		;and try again w/o blocking factor
	jmp	short BdlRealloc_Cont1

BdlRealloc_Cont:
	add	ax,FAR_EXTRA		;ax = ax + FAR_EXTRA to reduce thrashing
	;Under DOS 3, we know the heap manager actually allocates in 16-byte
	;  (paragraph) quantities, so to ensure we don't waste an average of
	;  8 bytes per bdl, pay a few bytes of code here to round up
	jc	RealcForMax		;brif this puts us over 64k

BdlRealloc_Cont1:
	add	ax,000FH		;constant for rounding up to paragraph
	jnc	TryToRealloc		;brif still under 64k

RealcForMax:
	mov	ax,0FFE0H		; try for maximum - - - note that
					;'maximum' can't be FFFFH, because
					;the far heap code will round this
					;up to the nearest paragraph boundary
TryToRealloc:
	and	al,0F0H 		;[9] finish rounding size up to para
	mov	dx,0FFE0H		
	cmp	ax,dx			; is result > legal max?
	jbe	ReallocAttempt		; brif not

	xchg	ax,dx			
ReallocAttempt:
	DbChk	BdlOwner,di		;ensure that bdlOwner is an owner
	xor	dx,dx
	mov	bx,di
	call	B$FHRealloc
	or	ax,ax
	jz	BdlRealloc_Crunch	;brif insufficient memory
	mov	ax,[cbNew]		;requested size
Change_cbLogical:
	mov	[di.BDL_cbLogical],ax	;save new logical size
	mov	ax,sp			;signal success (cbNew could be zero ..)
BdlRealloc_Exit:
cEnd

;***
;BdlCheckFree - Make sure far heap entry has some free space
;Purpose:
;	Change size of a far Heap entry if necessary to
;	ensure that there is a certain number of free bytes at the
;	end of the entry.  This can result in the movement of this
;	and other heap entries.
;	This routine does not work with HUGE heap entries (i.e. > 64k)
;Entry:
;	parm: bdl *pbdlOwner - points to owner of heap entry
;	parm: ushort cbFree - number of free bytes needed
;Exit:
;	If enough memory is available:
;	   pbdlOwner->cbLogical is unchanged
;	   pbdlOwner->cbPhysical >= pbdlOwner->cbLogical + cbFree
;	   [AX] = TRUE (non-zero) (successful return)
;	else
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************
cProc	BdlCheckFree,<PUBLIC,FAR,NODATA>,<di>
	parmW	pbdlOwner
	parmW	cbFree
cBegin
	DbOMCnt	BdlCheckFreeExit
	mov	di,[pbdlOwner]
	mov	ax,[di.BDL_cPhysical]	;ax = current physical size
	SHIFT	H,L,ax,4		;shift left to convert cPara to cbytes
	push	ax
	sub	ax,[di.BDL_cbLogical]	;ax = current free size
	sub	ax,cbFree		;ax = new free size
	jnc	SizeIsOk		;brif we're already big enough

	neg	ax
	pop	bx
	add	ax,bx			;ax = minimum new free size
	jc	BdlCheckDenied		;error if attempting to grow > 64k

	push	[di.BDL_cbLogical]	;save this across call to BdlRealloc
	cCall	BdlRealloc,<di,ax>
	pop	[di.BDL_cbLogical]
BdlCheckFreeExit:
cEnd

BdlCheckDenied:
	xor	ax,ax			;return ERROR result (zero)
	SKIP1_PSW			;this 'eats' the next instruction
SizeIsOk:
	pop	ax			;cPhysical known to be non-zero
	jmp	short BdlCheckFreeExit

;***
;BdlGrow - Increase the logical size of a Heap entry
;Purpose:
;	Change logical size of a bdl.  This can result in the movement of this
;	and other heap entries.
;	When this routine actually needs to grow the physical size
;	of the bdl, it grows more than needed for this request, to
;	reduce heap thrashing.
;
;	Added as part of revision [9].
;Entry:
;	parm: bd *pbdlOwner
;	parm: ushort cbGrow - number of additional bytes needed
;Exit:
;	if enough memory is available:
;	   pbdlOwner->cbLogical += cbGrow,
;	   pbdlOwner->cPhysical increased to account for >= cbLogical bytes
;	   [AX] = TRUE (non-zero)
;	else
;	   [AX] = FALSE (0) (Out of memory)
;
;***************************************************************************
cProc	BdlGrow,<PUBLIC,FAR>
	parmW	pbdlOwner
	parmW	cbGrow
cBegin
	mov	bx,[pbdlOwner]
	mov	ax,[cbGrow]
	add	ax,[bx.BDL_cbLogical]
	cCall	BdlRealloc,<bx,ax>
cEnd

;***
;BdlCopyFrom - Copy data from a far Heap entry to DS
;Purpose:
;	Copy data from a far Heap entry to DS
;	Does not work with HUGE heap entries (i.e. > 64k)
;Entry:
;	parm: bdl *pbdlOwner - points to owner of new heap entry
;	parm: ushort oSrc - 16 bit offset into bdl to source
;	parm: char *pbDst - points to 1st byte of destination
;	parm: ushort cb - number of bytes to copy
;
;***************************************************************************
cProc	BdlCopyFrom,<PUBLIC,FAR,NODATA>,<si,di>
	parmW	pbdlOwner
	parmW	oSrc
	parmW	pbDst
	parmW	cb
cBegin
	mov	si,[oSrc]		;si = source offset
	mov	di,[pbDst]		;di = destination offset
	mov	bx,[pbdlOwner]
	DbChk	BdlOwner,bx		;ensure that bdlOwner is an owner
	GETSEG	ax,[bx.BDL_seg],,<SIZE,LOAD> ;[4] seg of far heap entry
	mov	bx,ds			;bx -> DGROUP
	mov	ds,ax			;set up source seg (heap entry)
	mov	es,bx			;set up destination seg (DGROUP)
CopyCommon:
	mov	cx,cb			;cx = byte count
	shr	cx,1			;convert to word count
	rep	movsw			;transfer from ds:si to es:di
	jnc	CopyFrom_Even		;no carry if count was even
	movsb				;move the last (odd) byte
CopyFrom_Even:
	mov	ds,bx			;restore ds->DGROUP
cEnd

;***
;BdlCopyTo - Copy data from DS into a far Heap entry
;Purpose:
;	Copy data from DS into a far Heap entry
;	Does not work with HUGE heap entries (i.e. > 64k)
;Entry:
;	parm: bdl *pbdlOwner - points to owner of new heap entry
;	parm: ushort oDst - 16 bit offset into bdl to destination
;	parm: char *pbSrc - points to 1st byte of source
;	parm: ushort cb - number of bytes to copy
;
;***************************************************************************
cProc	BdlCopyTo,<PUBLIC,FAR,NODATA>,<si,di>
	parmW	pbdlOwner
	parmW	oDst
	parmW	pbSrc
	parmW	cb
cBegin
	mov	si,[pbSrc]		;si = source offset
	mov	di,[oDst]		;di = destination offset
	mov	bx,[pbdlOwner]
	DbChk	BdlOwner,bx		;ensure that bdlOwner is an owner
	GETSEG	ax,[bx.BDL_seg],,<SIZE,LOAD> ;[4] seg of far heap entry
	mov	es,ax			;set up destination seg
	mov	bx,ds
	jmp	short CopyCommon
cEnd	nogen


;***
;BdlCopyFromTo - Copy data from one bdl to another
;Purpose:
;	Copy data from one far heap entry into another.
;	Does not work with HUGE heap entries (i.e. > 64k)
;
;	Added as part of revison [7]
;Entry:
;	parm: bdl *pbdlSrc - points to source bdl
;	parm: ushort oSrc - 16 bit offset into bdl to source
;	parm: bdl *pbdlDst - points to destination bdl
;	parm: ushort oDst - 16 bit offset into bdl to destination
;	parm: ushort cb - number of bytes to copy
;Exit:
;	none.
;***************************************************************************
cProc	BdlCopyFromTo,<PUBLIC,FAR,NODATA>,<si,di,ds>
	parmW	pbdlSrc
	parmW	oSrc
	parmW	pbdlDst
	parmW	oDst
	parmW	cb
cBegin
	mov	si,[pbdlDst]		
	DbChk	BdlOwner,si		;ensure that bdlDst is an owner
	GETSEG	dx,[si.BDL_seg],,<SIZE,LOAD>  
					; dx = seg of far heap entry (dst)
	mov	bx,[pbdlSrc]
	DbChk	BdlOwner,bx		;ensure that bdlSrc is an owner
	GETSEG	ds,[bx.BDL_seg],,<SIZE,LOAD,NOFLUSH>	
					; ds = seg of far heap entry (src)
assumes DS,NOTHING
	mov	es,dx			
	mov	si,[oSrc]		;si = source offset
	mov	di,[oDst]		;di = destination offset
	mov	cx,[cb]
	shr	cx,1			;convert to word count
	rep	movsw			;transfer from ds:si to es:di
	jnc	CopyFrom_Even2		;no carry if count was even
	movsb				;move the last (odd) byte
CopyFrom_Even2:
cEnd
assumes DS,DATA


;***
;BdlTrim - trim given bdl down to cbLogical
;Purpose:
;	Releases excess space in a given bdl
;Entry:
;	parm: bdl *pbdl
;
;***************************************************************************
cProc	BdlTrim,<PUBLIC,FAR,NODATA>,<si,di>
	parmW	pbdl
cBegin
	mov	bx,[pbdl]
	DbChk	BdlOwner,bx		;ensure that bdlOwner is an owner
	mov	ax,[bx.BDL_cbLogical]	;size to realloc to
	xor	dx,dx
	call	B$FHRealloc		;must succeed; we're either reducing
					;  or asking for existing entry size
cEnd


;seg_rt = segment address for the RT segment
;It can be referenced from any module as follows:
	;	EXTRN	seg_rt:abs
	;	mov	ax,SEG seg_rt
	
	PUBLIC	seg_rt
	seg_rt	EQU	SEG BdlTrim

sEnd	RT


;------------------------------------------------------------
;---  Interpreter Buffer Descriptor Management Routines   ---
;------------------------------------------------------------
sBegin	DATA
	staticB	bdGrabSpace,NULL,<SIZE BD>
sEnd	DATA

sBegin 	CODE
assumes	CS,CODE

CBNEAR_GRAB equ 2 * CBBUFBLOCK

;***
;GrabSpace - grab some heap space
;Purpose:
;	Allocates CBNEAR_GRAB bytes via BdAlloc. 
;	Called to lock up a chunk of heap space so we ensure that
;	enough space exists to do simple things like CLEAR for more memory!
;
;	NOTE: It's important that grabspace just grab space from the near
;		heap, not the far heap; if we grabbed far space instead,
;		this could allow the user to tie up all of DGROUP with
;		variable tables with plenty of DGROUP space free.
;Entry:
;	none.
;Exit:	
;	ax = 0 if insufficient memory, else ax != 0
;***************************************************************************
cProc	GrabSpace,<PUBLIC,FAR,NODATA>
cBegin
	mov	ax,[bdGrabSpace.BD_pb]
	or	ax,ax
	jnz	GotSpace		;return ax<>0 if already have space
	PUSHI	ax,<dataOFFSET bdGrabSpace>
	PUSHI	ax,CBNEAR_GRAB
	PUSHI	ax,IT_NO_OWNERS
	call	BdAlloc
	or	ax,ax
GotSpace:
cEnd

;***
;ReleaseSpace - Release the space grabbed by GrabSpace
;Purpose:
;	Deallocates the bd allocated by GrabSpace if it is currently allocated.
;	Note that it's perfectly o.k. to call this even when no space has
;	been grabbed.
;Entry:
;	none.
;Exit:
;	ax = 0.
;***************************************************************************
cProc	ReleaseSpace,<PUBLIC,FAR,NODATA>
cBegin
	PUSHI	ax,<dataOFFSET bdGrabSpace>
	call	BdFree			;deallocate bd if couldn't allocate bdl
cEnd

sEnd	CODE

	end
