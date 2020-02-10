	TITLE	FHINIT - Far Heap Initialization and Support
	PAGE	56,132
;***
;FHINIT.ASM - Far Heap Initialization for the BASIC 3 Common Runtime
;
;	Copyright (C) Microsoft Corp. 1986.
;
;*****************************************************************************

	SUBTTL	INCLUDES AND DEFINITIONS FOLLOWS

INCLUDE switch.inc
INCLUDE rmacros.inc

	USESEG	<INIT_CODE>	;Initialization
	USESEG	<BR_DATA>	
	USESEG	<_DATA>
	USESEG	<_BSS>
	USESEG	<NH_TEXT>
	USESEG	<RT_TEXT>	
	USESEG	<FH_TEXT>
	USESEG	<DV_TEXT>	
	USESEG	<XIB>		
	USESEG	<XI>		
	USESEG	<XIE>		
	USESEG	<BC_SAB>	
	USESEG	<BC_SA> 	

INCLUDE seg.inc
INCLUDE idmac.inc
INCLUDE messages.inc		
INCLUDE compvect.inc		
INCLUDE array.inc
INCLUDE smchain.inc		
INCLUDE oscalls.inc		
INCLUDE	addr.inc		
INCLUDE baslibma.inc		
INCLUDE string.inc		
INCLUDE stack2.inc		

	externFP	B$ERR_OM_FH	; out of memory error
	externFP	B$ERR_MEM	;arena destroyed error
	externNP	B$ERR_FHC	;bad memory block address


	externFP	B$ULAllocDataImage ;alloc UL data images for restart
	externFP	B$ULDataRelease ;dealloc UL data images
	externFP	CompressHelp	; compress and close help system
	externFP	ShrinkHelp	; compress help system (stay open)
	externFP	fEditorActive	
	externFP	CompressBufs	

	INITIALIZER	B$xFHINI	;put B$xFHINI in initializer list.

	SUBTTL	DATA DEFINITIONS

sBegin	BR_DATA

	externW __acmdseg	; C startup __psp

sEnd	BR_DATA

sBegin	_DATA

	externW b$ini_disp	;One time initialization dispatch table
	externW b$clrt_disp	;CLEAR dispatch table
	externW b$run_disp	;RUN dispatch table
	externW b$pFHRaiseBottom ;Vector to B$FHRaiseBottom
	externW b$pFHLowerTop	;Vector to B$FHLowerTop
	externW b$shli_disp	;Shell initialization dispatch table
	externW b$shlt_disp	;Shell termination dispatch table

	externB b$Chaining	;non-zero if we are CHAINING

sEnd	_DATA

sBegin	_BSS


	globalB b$FHDStart,,<SIZE FHD>	 ; starting FHD for list




	globalB b$FHDEnd,,<SIZE FHD> ;ending FHD for list

	externW b$NH_first	;NHINIT - starting offset of near heap
	externW b$NH_last	;NHINIT - ending offset of near heap

	externW b$UcodeOff	;offset of usercode header
	externW b$UcodeSeg	;segment of usercode header


sEnd	_BSS

sBegin	DV_TEXT 		
	externNP B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 		


sBegin	NH_TEXT

	externNP	B$NHMOV 	;move near heap

sEnd	NH_TEXT

sBegin	RT_TEXT 			
sEnd	RT_TEXT 			

	PAGE
	SUBTTL	Far Heap Initialization

assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xFHINI - Far Heap initializer
;PLM B$xFHINI()
;
;Purpose:
;	Initializer for Far Heap component.  This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the far heap routines.  This
;	insures that the only time that the far heap is accessed is when
;	this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	Appropriate dispatch vectors filled.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****************************************************************************

cProc	B$xFHINI,<FAR>
cBegin

;	update "ONE" time initialization dispatch address to B$FHIni

	MOV	WORD PTR [b$ini_disp].FH_IVEC,FH_TEXTOFFSET B$FHIni 

;	update CLEAR time initialization dispatch address to B$FHClear

	MOV	WORD PTR [b$clrt_disp].FH_CLTVEC,FH_TEXTOFFSET B$FHClear


;	update RUN time initialization dispatch address to B$FHClear

	MOV	WORD PTR [b$run_disp].FH_RVEC,FH_TEXTOFFSET B$FHClear  



cEnd

sEnd	INIT_CODE


ASSUMES CS, FH_TEXT
sBegin	FH_TEXT


;***
; B$FHIni - One time initialization for far heap (DOS 3 & 5)
;
;Purpose:
;	One time initialization for the far heap.  For DOS 3, this routine
;	allocates all available memory and sets up the initial free space.
;	Set vector b$pFHRaiseBottom to B$FHRaiseBottom (DOS 3).
;	Set vector b$pFHLowerTop to B$FHLowerTop (DOS 3).
;
;Entry:
;	None.
;
;Exit:
;	Allocates memory and inits descriptor chain (DOS 3)
;
;Uses:
;	None.
;
;Exceptions:
;	Out of memory condition if allocation cannot be done.
;******************************************************************************

FHIniError:
	JMP	B$ERR_MEM	;report out of memory error

DbPub	B$FHIni 		
cProc	B$FHIni,<NEAR>,<ES>	
cBegin

	MOV	b$pFHRaiseBottom, FH_TEXTOFFSET B$FHRaiseBottom 	 
	MOV	b$pFHLowerTop, FH_TEXTOFFSET B$FHLowerTop		
	MOV	ES,__acmdseg	;get address for CODE/DGROUP block
	MOV	BX,0FFFFH	;ask for maximum memory possible...
	MOV	AH,4AH		;so DOS will get maximum size...
	CALL	B$FHMemDosCall	;call to perform INT and handle fatal errors
	JNC	FHIniError	;call should always fail
	MOV	AH,4AH		;allocate the maximum block, leaving none
	CALL	B$FHMemDosCall	;remaining for any other process
	JC	FHIniError	;error if allocation failed

;	Build the FHD list static descriptors b$FHDStart and b$FHDEnd.

	MOV	AX,b$NH_last	;get last offset of near heap
	INC	AX		;nxt word (+2) + nxt para (+0F) - one para (10)
	MOV	CL,4		;...and shift by four...
	SHR	AX,CL		;...to get paragraph size
	INC	AX		;put paragraph back that was subtracted
	MOV	CX,DS		;get starting segment of DGROUP
	ADD	CX,AX		;add to get lower boundary paragraph
	MOV	b$FHDEnd.FHD_hData,CX ;put lower boundary in static descriptor
	MOV	CX,ES		;get start of CODE/DGROUP block
	ADD	CX,BX		;add maximum size to get upper boundary
	MOV	b$FHDStart.FHD_hData,CX ; put upper boundary in static desc
	XOR	AX,AX		;clear for repeated use below
	MOV	b$FHDStart.FHD_cPara,AX ; size of starting entry in list is zero
	MOV	b$FHDStart.FHD_pNext,OFFSET DGROUP:b$FHDEnd ; start points to end
	MOV	b$FHDEnd.FHD_cPara,AX ;size of ending entry in list is zero
	MOV	b$FHDEnd.FHD_pNext,AX ;last entry points to null - end of list

;	Allocate Preinited Data images for quick restartability
	CALL	B$ULAllocDataImage


cEnd

;***
; B$FHClear - CLEAR support for far heap
;
;Purpose:
;	Added routine as part of revision [22].
;	Far heap support for the CLEAR statement.  Deallocate all
;	entries either in COMMON or in the user variables.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$FHClear,<NEAR,PUBLIC>,<BX,ES> 
cBegin

	CMP	b$UcodeSeg,0	;test if any compiled code
	JZ	FHClearRet	;if not, just return

	LES	BX,DWORD PTR b$UcodeOff ;ES:BX = seg:off of usercode header
	MOV	AX,OFFSET FHClearEntry ;get offset to routine for each entry
	CALL	B$FHSelect	;select the far heap entries to deallocate



FHClearRet:

cEnd

;***
; FHClearEntry - FHSelect routine for CLEAR support
;
;Purpose:
;	Added as part of revision [22].
;	Returns flag to deallocate any FH entry in COMMON or
;	the user variables.
;Entry:
;	ES:BX = far pointer to usercode header
;	SI = pointer to FH descriptor to check.
;Exit:
;	AX = 0 - do not deallocate entry described at SI.
;	AX <> 0 - deallocate entry described at SI.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHClearEntry,<NEAR>,<SI>	
cBegin
	MOV	AX,-1		;default to deallocate entry

;	Test if descriptor is in the user variables.  If so, flag deallocation.

	CMP	SI,ES:[BX].OF_DAT ;test against lower bound of user variables
	JB	FHClrEntCommon	;if too low, then test for COMMON area
	CMP	SI,ES:[BX].OF_FT ;test against upper bound of user variables
	JB	FHClrEntDealloc	;if within bounds, then jump to flag deallocate

;	Test if descriptor is in COMMON.  If so, flag deallocation.

FHClrEntCommon:

	CMP	b$Chaining,0	;are we CHAINing in interpreter?
	JNZ	FHClrEntNoDealloc ;don't deallocate COMMON entries if so

	SUB	SI,ES:[BX].OF_COM ; compute offset of descriptor in COMMON
	JB	FHClrEntNoDealloc ;if below COMMON, then flag no dealloc
	CMP	SI,ES:[BX].SZ_COM ; test if offset if with COMMON
				; this use of SZ_COM is OK, since we are
				; looking at ALL the modules
	JB	FHClrEntDealloc	;if in COMMON, then deallocate

;	Increment AX to 0 for no deallocation, else leave at -1.

FHClrEntNoDealloc:
	INC	AX		;set to zero for no deallocation
FHClrEntDealloc:

cEnd
	SUBTTL	B$FHClearRange - Delete entries in a range
	PAGE
;*** 
; B$FHClearRange - Delete entries in a range (QBI Only)
; Added Rev [39]
; needed for BC6 also now
;
;Purpose:
; Delete all far heap entries whose descriptors fall into a passed DGROUP
; range.
;
;Entry:
; [AX]	= start of the range to deallocate in
; [BX]	= end of range
;
;Exit:
; none
;
;Uses:
; Per convention
;
;******************************************************************************
cProc	B$FHClearRange,<NEAR,PUBLIC>
cBegin
	XCHG	AX,CX		;[CX] = start of range
	MOV	AX,OFFSET FHClearRangeEntry
	Call	B$FHSelect
cEnd

;*** 
; B$FHClearRangeEntry - Delete entry if in a range (QBI Only)
; needed for BC6 also now
; Added Rev [39]
;
;Purpose:
; Delete far heap entry if descriptor falls into a passed DGROUP range.
;
;Entry:
; [CX]	= start of the range to deallocate in
; [BX]	= end of range
; [SI]	= address of heap descriptor
;
;Exit:
; [AX]	= Non-zero if entry to be deallocated
;
;Uses:
; Per convention
;
;******************************************************************************
cProc	FHClearRangeEntry,NEAR
cBegin
	XOR	AX,AX		;Assume not deallocating
	CMP	SI,CX
	JB	ClearRangeSkip	;Jump if below range
	CMP	SI,BX
	JA	ClearRangeSkip	;Jump if above range
	DEC	AX		;[AX] = -1
ClearRangeSkip:
cEnd

;***
;B$FHAllocFAR - far entrypoint for B$FHAlloc.
;
;Purpose:
;	Added with revision [75].
;	Far entrypoint for B$FHAlloc (called from LOADRTM segment).
;Entry:
;	Same as B$FHAlloc
;Exit:
;	Same as B$FHAlloc
;Uses:
;	Same as B$FHAlloc
;Exceptions:
;	Same as B$FHAlloc
;******************************************************************************
cProc	B$FHAllocFAR,<FAR,PUBLIC>
cBegin
	CALL	B$FHAlloc
cEnd

;***
; B$FHAlloc - allocate a far heap entry (DOS 3 & 5)
;
;Purpose:
;	Calls B$IFHAlloc to allocate an entry of the requested size.
;	If the allocation attempt fails, an Out of Memory runtime error
;	is issued.
;Entry:
;	DX:AX = 32-bit size in bytes.
;	BX = unallocated far heap descriptor for allocated entry.
;Exit:
;	[BX].FHD_hData <> segment if allocated (0 if unallocated)
;Uses:
;	None.
;Exceptions:
;	Out of memory condition if allocation cannot be done.
;Note:
;	For DOS 5 the cPara field is never altered by the
;	Far Heap manager.
;******************************************************************************

cProc	B$FHAlloc,<PUBLIC,NEAR>,<AX> 
cBegin				
	cCall	B$IFHAlloc	;call common allocation code
	OR	AX,AX		;did allocation succeed?
	JNZ	FHAllocSuccess	;brif no error
	JMP	B$ERR_OM_FH	;issue out of memory error if failed
FHAllocSuccess: 		
cEnd


;***
; B$IFHAlloc - allocate a far heap entry (return error code on fail)(DOS 3 & 5)
;
;Purpose:
;	Allocates a far heap entry of the given size.  The far heap is
;	first scanned from its upper to lower boundary using a first-fit
;	algorithm.  If no space is found of sufficient size, a compaction
;	is done and the resulting unallocated space is tested.	If still
;	no room, the near heap is shrunk and the space reclaimed to the
;	far heap to provide the allocation.
;Entry:
;	DX:AX = 32-bit size in bytes.
;	BX = unallocated far heap descriptor for allocated entry.
;Exit:
;	AX = zero if not enough memory is present for requested allocation.
;	[BX].FHD_hData <> segment if allocated (0 if unallocated)
;Uses:
;	None.
;Exceptions:
;	None.
;Note:
;	For DOS 5 the cPara field is never altered by the
;	Far Heap manager.
;******************************************************************************

IFHAllocDone:			

cProc	B$IFHAlloc,<NEAR,PUBLIC>,<BX,CX,DX,SI,DI,ES> 
cBegin

;This routine can cause movement of the near heap, thus potentially
;invalidating the entry pointer to this routine if it is in the near heap.

DbAssertRel BX,B,b$NH_first,<FH_TEXT>,<Attempted to Far alloc with owner in near heap>

;	Convert size DX:AX from bytes to paragraphs.

	CALL	FHByteToPara	;change DX:AX bytes to paragraphs in DX:AX
	OR	DX,DX		;test if more than 1M bytes requested
	JNZ	FHAllocError	;if so, give error immediately

DbAssertRel	AX,NZ,0,FH_TEXT,<Cannot B$FHAlloc a buffer of size 0>

;	To allocate a far heap entry, a first-fit allocation is attempted.
;	SI traverses down the far heap descriptor list.  FHTryAlloc is used
;	to determine if any unallocated space exists adjacent to the lower
;	boundary of the far heap entry described by SI.

	MOV	SI,OFFSET DGROUP:b$FHDStart ;set pointer to first desc in list
FHAllocLoop:
	CALL	FHTryAlloc	;try to allocate AX para adjacent to desc at SI
	JNC	FHAllocDone	;if success, then jump to finish
	MOV	SI,[SI].FHD_pNext ;get pointer to next descriptor in the list
	CMP	SI,OFFSET DGROUP:b$FHDEnd ;test if final descriptor
	JNZ	FHAllocLoop	;if not last descriptor, then jump to try again

;	If first-fit fails, next compact the far heap and test the resulting
;	unallocated space.

	CALL	FHTryAllocCompact ; compact to combine the free areas,
				; and try to allocate again
	JNC	FHAllocDone	;if success, then jump to finish

;	If compaction fails, shrink the near heap enough to get the necessary
;	room for the allocation.

	MOV	DX,AX		;save request amount in DX
	CALL	FHParaSize	;get size of unallocated entry in AX
	SUB	AX,DX		;compute negative of size needed yet...
	NEG	AX		;...now value is positive
	CALL	B$FHLowerBottom   ;attempt to lower the FH bottom for more room
	XCHG	AX,DX		; [AX] = original allocation request
	CALL	FHTryAllocCompact ; compact to combine the free areas,
				; and try to allocate again

	JNC	FHAllocDone	; if success, then jump to finish

;	If there was not enough near heap space, then release user lib
;	data images, recover released space and retry allocation.

	PUSH	AX		;save original allocation request
	PUSH	BX		; Save FHD
	CALL	B$ULDataRelease ;free UL data images (if present)
	POP	BX		
	POP	AX		;[AX] = original allocation request
	CALL	FHTryAllocCompact ; compact to combine the free areas,
				; and try to allocate again
	JNC	FHAllocDone	; it worked, don't keep trying!

;	If there was STILL not enough space, then have the help engine close
;	down to release the remaining memory that it has, and re-try.

	PUSH	AX		; save registers
	PUSH	BX		
	CALL	CompressHelp	; have help engine shrink down to minimum
	POP	BX		; restore registers
	POP	AX		
	CALL	FHTryAllocCompact ; compact to combine the free areas,
				; and try to allocate again
NoHelpShrink:			

	JNC	FHAllocDone	;if success, then return

FHAllocError:			
	XOR	AX,AX		;return error code - no memory available

;	Allocation succeeded - done.

FHAllocDone:

cEnd


;***
; FHByteToPara - convert size in bytes to paragraphs (DOS 3)
;
;Purpose:
;	Shift the 32-bit value given right four times to compute
;	paragraphs.
;Entry:
;	DX:AX = 32-bit number of bytes
;Exit:
;	DX:AX = 32-bit number of paragraphs
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHByteToPara,<NEAR>
cBegin

	ADD	AX,0FH		;round up DX:AX to the...
	ADC	DX,0		;...nearest paragraph
	SHR	DX,1		;shift DX:AX...
	RCR	AX,1		;...once
	SHR	DX,1		;and...
	RCR	AX,1		;...twice
	SHR	DX,1		;and...
	RCR	AX,1		;...thrice
	SHR	DX,1		;and...
	RCR	AX,1		;...four times to get the paragraphs

cEnd

;***
; FHTryAlloc/FHTryAllocCompact - try to allocate after a specific entry (DOS 3)
;
;Purpose:
;	Given a DGROUP offset to a far heap descriptor and its corresponding
;	far heap entry, compute the size of the unallocated space adjacent
;	to the lower boundary of the entry.  Allocate the upper part of the
;	space if large enough to satisfy the allocation request.
;
;	FHTryAllocCompact added with revision [74] to compact far heap before
;	trying to do the allocation.
;
;Entry:
;	SI = DGROUP offset of allocated far heap descriptor to test
;	     for the adjacent unallocated entry.
;	BX = DGROUP offset of unallocated far heap descriptor to be
;	     allocated.
;	AX = Size of allocation request in paragraphs.
;Exit:
;	CF clear = allocation successful - descriptor at BX filled
;	     and linked into the far heap descriptor list.
;	CF set = allocation failed - descriptor at BX unchanged.
;Uses:
;	BX, CX, DX, DI.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHTryAllocCompact,<NEAR>
cBegin
	cCall	B$FHCompact		; compact the heap to combine new free
					; areas
cEnd	<nogen>				; fall into FHTryAlloc

cProc	FHTryAlloc,<NEAR>,<AX>
cBegin

;DbAssertRel	 [BX].FHD_hData,z,0,FH_TEXT,<FHTryAlloc: entry already allocated> 

;	Determine the location and size of the unallocated region in the
;	far heap by determining the lower boundary of the given allocated
;	far heap entry and upper boundary of the next allocated far heap
;	entry.	The unallocated region is exclusively between these two
;	values.

	MOV	DI,[SI].FHD_pNext ;get descriptor pointer of next heap entry
	MOV	DX,[SI].FHD_hData ;get start segment of given heap entry
	MOV	CX,[DI].FHD_hData ;get start segment of next heap entry
	ADD	CX,[DI].FHD_cPara ;add size to get seg past upper bound of next
	SUB	CX,DX		;compute negative size of unallocated region
	NEG	CX		;negate to get correct size
	SUB	DX,AX		;get allocated seg to be adjacent given entry

;	Test if the unallocation region of size CX paragraphs is large
;	enough to satisfy the allocation request of AX paragraphs.  If
;	not, then jump with carry set to exit.

	CMP	CX,AX		;test if region is large enough for allocation
	JB	FHTryFail	;jump with CARRY set for failure

;	Allocation succeeded - new entry is AX paragraphs in length at
;	DX:0.  Note that the allocation is taken from the upper part of
;	the unallocated region to try to minimize compaction.  The new
;	far heap descriptor segment and length is filled and is linked
;	between the given and next descriptor in the list.

	MOV	[BX].FHD_hData,DX ;fill in segment value of new descriptor
	MOV	[BX].FHD_cPara,AX ;fill in paragraph size of new descriptor
	MOV	[BX].FHD_pNext,DI ;new descriptor is linked to next descriptor
	MOV	[SI].FHD_pNext,BX ;given descriptor is linked to new descriptor

;	Clear the entry using B$FHMove with BX=DX.  Return CARRY clear.

	MOV	BX,DX		;set equal so B$FHMove clears the entry
	CALL	B$FHMove	;clear the entry at DX:0 for AX paragraphs
	CLC			;clear carry flag for success

FHTryFail:

cEnd

;***
;B$FHRealloc - Reallocate a far heap entry (DOS 3 & 5)
;
;Purpose:
;	Dos 3 support added with revision [32].
;	Added as part of revision [11].
;	Reallocate a block of memory from DOS.
;Entry:
;	DX:AX = 32-bit total size in bytes to reallocate.
;	BX = DGROUP offset of FH descriptor.
;Exit:
;	AX = 0 means reallocation failed.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************


cProc	B$FHRealloc,<PUBLIC,NEAR>,<SI,DI,ES>
localV	FHDTmp,<SIZE FHD>
cBegin

	MOV	CX,AX		;save byte size of request in DI:CX
	mov	DI,dx		
	cCall	FHByteToPara	;[DX:AX] = para size of request
DbAssertRel	DX,Z,0,<FH_TEXT>,<Reallocation request too large after rounding in B$FHRealloc>
	CMP	AX,[BX].FHD_cPara ;are we growing this entry?
	JBE	RealcSaveSize	;brif not - save new size and exit

;This routine can cause movement of the near heap, thus potentially
;invalidating the entry pointer to this routine if it is in the near heap.

DbAssertRel BX,B,b$NH_first,<FH_TEXT>,<Attempted a Far realloc with owner in near heap>

;	We need to grow the entry.  Our reallocation strategy is:
;
;	  1. Check the adjacent entry.	If there is enough free space
;	     to satisfy the allocation, use it.
;	  2. Try to allocate a new entry of the requested size.  If
;	     successful, copy data to newly allocated heap entry, and
;	     change the owner of the entry to be the descriptor passed
;	     to us on entry to B$FHRealloc.
;	  3. As a last resort, see if enough room exists for the additional
;	     amount requested.	If so, then move all far heap entries
;	     from the requested entry to the last allocated entry, down
;	     by the additional amount.	The requested entry can then
;	     reclaim the freed space to satisfy the allocation request.

;	Check the adjacent entry for enough free space.

	cCall	FHPrevDesc	;[SI] = descriptor before [BX]
	MOV	DX,[SI].FHD_hData
	SUB	DX,[BX].FHD_hData ;[DX] = total paras available for allocation
	CMP	DX,AX		;is it enough to satisfy the request?
	JB	RealcNew	;brif not

RealcSaveSize:

;	Enough free space exists between this entry and adjacent entry.

	MOV	[BX].FHD_cPara,AX ;update size
	JMP	SHORT RealcDone ;exit

RealcNew:

;	Try to allocate a new entry of requested size.

	XCHG	AX,CX		;[AX]=low word of byte count, [CX]=para count
	mov	dx,di		; [DX:AX] = byte count of original request
	PUSH	BX		;save entry FHD
	LEA	BX,FHDTmp	;point to temp FHD
	cCall	B$IFHAlloc	;try to allocate a new entry
	MOV	SI,BX		;save new FHD
	POP	BX		;recover entry FHD
	OR	AX,AX		;was alloc successful?
	JZ	RealcPartial	;brif not, try to allocate incremental diff

;	The allocation succeded. Copy the entry and change the owner.

	MOV	AX,[BX].FHD_cPara ;get size of original entry
	PUSH	BX		;save original FHD
	MOV	BX,[BX].FHD_hData ;get start of area to move
	MOV	DX,[SI].FHD_hData ;get destination
	PUSH	CX		;save para size of allocation
	cCall	B$FHMove	;move the data
	POP	CX		;recover para size of new allocation
	POP	BX		;recover entry FHD
	cCall	B$FHDealloc	;deallocate entry FHD

;	Now make the entry FHD the owner of the newly allocated heap entry.

	MOV	AX,[SI].FHD_hData
	MOV	[BX].FHD_hData,AX ;update segment of allocation
	MOV	AX,[SI].FHD_pNext
	MOV	[BX].FHD_pNext,AX ;update ptr to next descriptor in chain
	MOV	[BX].FHD_cPara,CX ;update size of entry
	PUSH	BX		;save entry FHD
	MOV	BX,SI		;get FHD of newly allocated entry
	cCall	FHPrevDesc	;find the FHD which points to new FHD
	POP	BX		;recover entry FHD
	MOV	[SI].FHD_pNext,BX ;replace new FHD with entry FHD in chain
	JMP	SHORT RealcDone ;exit

RealcPartial:

;	Check to see if enough room exists for current entry to expand
;	Note: we are assuming that the attempted allocation above caused
;	compaction of the far heap to high memory, leaving one free
;	entry at the end of the far heap (lower memory).

	SUB	CX,[BX].FHD_cPara ;compute amount needed to grow entry
	PUSH	BX		;save entry FHD
	MOV	BX,OFFSET DGROUP:b$FHDEnd ;get ending heap descriptor
	cCall	FHPrevDesc	;find last allocated descriptor
	MOV	DI,SI		;[DI] = last allocated FHD
	cCall	FHParaSize	;[AX] = size of Free entry
	POP	BX		;recover entry FHD
	CMP	AX,CX		;does enough memory exist to satisfy allocation?
	JNB	RealcPartialOk	;brif so

	XOR	AX,AX		;return AX=0 meaning not enough memory
	JMP	SHORT RealcDone ;exit

RealcPartialOk:

;	Enough memory exists.  Move free space adjacent to current allocation
;	and allocate it to the Entry FHD.

	PUSH	BX		;save entry FHD
	PUSH	CX		;save increment of allocation
	MOV	AX,[BX].FHD_hData ;get current start of allocation
	ADD	AX,[BX].FHD_cPara ;compute end of current allocation
	MOV	BX,[DI].FHD_hData ;get start of area to move
	SUB	AX,BX		;compute amount of data to move
	MOV	DX,BX		;start of data - size needed = ...
	SUB	DX,CX		;... start of data destination
	cCall	B$FHMove	;move the data
	POP	AX		;recover increment needed
	POP	SI		;recover entry FHD

;	Data is moved, now update all .FHD_hData fields for owners of
;	the moved data.

	XOR	BX,BX		;adjust all descriptors
	MOV	DX,BX		;starting with the Entry FHD to the
	DEC	DX		;end of the descriptor chain
	NEG	AX		;make negative to move heap down
	cCall	FHAdjDescSegSI	;adjust the descriptors
	MOV	BX,SI		;[BX] = entry FHD
	SUB	[BX].FHD_cPara,AX ;update the size to reflect the allocation

RealcDone:
cEnd

;***
;B$FHDeallocFAR - far entrypoint for B$FHDealloc.
;
;Purpose:
;	Added with revision [75].
;	Far entrypoint for B$FHDealloc (called from LOADRTM segment).
;Entry:
;	Same as B$FHDealloc
;Exit:
;	Same as B$FHDealloc
;Uses:
;	Same as B$FHDealloc
;Exceptions:
;	Same as B$FHDealloc
;******************************************************************************
cProc	B$FHDeallocFAR,<FAR,PUBLIC>
cBegin
	CALL	B$FHDealloc
cEnd

;***
; B$FHDealloc - deallocate a far heap entry (DOS 3 & 5)
;
;Purpose:
;	Deallocate the far heap entry referenced by the given
;	far heap descriptor.
;Entry:
;	BX = far heap descriptor of entry to be deallocated
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	Fatal internal error if descriptor not in descriptor list.
;******************************************************************************

cProc	B$FHDealloc,<NEAR,PUBLIC>,<AX,SI>
cBegin

;	Determine the far heap descriptor preceding the one to
;	be deallocated using FHPrevDesc.  Remove the descriptor
;	to be deallocated from the descriptor list and then clear
;	the descriptor segment (FHD_hData).

	CALL	FHPrevDesc	;pointer to previous FHD is in SI
	MOV	AX,[BX].FHD_pNext ;get FHD after the one to be deallocated
	MOV	[SI].FHD_pNext,AX ;link this with the previous FHD


	MOV	[BX].FHD_hData,0 ;clear the deallocated FHD segment value

cEnd

;***
; FHPrevDesc, B$FHPrevDescSI - get previous descriptor in list (DOS 3 & 5)
;
;Purpose:
;	Given a far heap descriptor in the descriptor list,
;	determine its predecessor in the list.
;Entry:
;	BX = FHD offset of the far heap descriptor
;	SI = FHD offset of FHD to start search with (B$FHPrevDescSI only)
;Exit:
;	SI = FHD offset of the predecessor FHD to that pointed
;	     pointer by BX
;Uses:
;	None.
;Exceptions:
;	Fatal internal error if given FHD not in descriptor list.
;******************************************************************************

cProc	FHPrevDesc,<NEAR>
cBegin

	MOV	SI,OFFSET DGROUP:b$FHDStart ;get first FHD of the list
cEnd	<nogen> 		;fall into B$FHPrevDescSI

cProc	B$FHPrevDescSI,<NEAR,PUBLIC> 
cBegin				

FHPrevLoop:
	CMP	[SI].FHD_pNext,BX ;test if predecessor to entry FHD
	JE	FHPrevFound	;if found, then exit with SI set
	MOV	SI,[SI].FHD_pNext ;get the next FHD in the descriptor list
	CMP	SI,OFFSET DGROUP:b$FHDEnd ;test if final descriptor
	JNZ	FHPrevLoop	;if not, then loop to process next FHD
	JMP	SHORT FHPrevError ;fatal error if FHD not found
FHPrevFound:

cEnd

FHPrevError:
	JMP	B$ERR_FHC	;give far heap inconsistency error

;***
; B$FHCompact - compact the far heap (DOS 3 & 5)
;
;Purpose:
;	Compact the far heap entries to upper memory within the
;	far heap.
;	This routine is a nop for DOS 5.
;
;Entry:
;	None.
;Exit:
;	SI = DGROUP offset of the next to last far heap descriptor
;	     in the FHD list.
;Uses:
;	None.
;Exceptions:
;	Fatal internal error if far heap inconsistency.
;******************************************************************************

cProc	B$FHCompact,<NEAR,PUBLIC>,<AX,BX,CX,DX,DI,ES>
cBegin

	CALL	ShrinkHelp	; Kick the help system to release all

	call	CompressBufs	; compress the document buffers

;	SI is a pointer that traverses down the far heap descriptor
;	list starting at b$FHDStart.  DX points to the segment in the
;	compacted far heap space.

	MOV	SI,OFFSET DGROUP:b$FHDStart ;initialize the FHD list pointer
	MOV	DX,[SI].FHD_hDATA ;get segment pointer to compacted space

;	This loop is executed for each entry in the far heap.  SI points
;	to the descriptor in the list while BX points to the start of the
;	far heap entry referenced by the descriptor.

FHCompactLoop:
	MOV	BX,[SI].FHD_hData ;get segment referenced by descriptor at SI
	MOV	AX,[SI].FHD_cPara ;get the length of the far heap entry
	SUB	DX,AX		;compute entry start segment after compaction
	CMP	BX,DX		;test source and destination if movement needed
	JE	FHCompactSkip	;if no movement needed, just skip to next entry
	CALL	B$FHMove	;move the entry from BX:0 to DX:0
	MOV	[SI].FHD_hData,DX ;update descriptor with new entry segment
FHCompactSkip:

;	Save the descriptor pointer in AX in case of exit.  Check far heap
;	consistency by ensuring segments of adjacent descriptors in the list
;	are not increasing.  Advance SI to the next descriptor in the list
;	and, if not the last, loop to process it.

	MOV	AX,SI		;save the descriptor pointer
	MOV	SI,[SI].FHD_pNext ;point to next descriptor in the list
	CMP	[SI].FHD_hData,BX ;test if entry segments are not increasing
	JA	FHCompactError	;if not, then internal error
	CMP	SI,OFFSET DGROUP:b$FHDEnd ;test if last is reserved descriptor
	JNE	FHCompactLoop	;if not, process it

;	Finished with the compaction.  Check whether the last descriptor is
;	the special one reserved.  Set exit variable with next to last
;	descriptor in the list.  This is used to directly determine the size
;	and location of the unallocated space in the far heap.

	CMP	[SI].FHD_cPara,0 ;test if entry size is zero
	JNZ	FHCompactError	;if not, then internal error
	CMP	[SI].FHD_pNext,0 ;test if link field is zero
	JNZ	FHCompactError	;if not, then internal error
	MOV	SI,AX		;set exit variable to next to last descriptor

cEnd

FHCompactError:
	JMP	B$ERR_FHC	;give far heap inconsistency error


;***
; B$FHMove - move or clear a far heap region (DOS 3)
;
;Purpose:
;	Moves a far heap region either up or down within the far heap
;	or clears a far heap region to zeroes.	The region may consist
;	of one or more far heap entries, even the entire heap.
;
;Entry:
;	BX = starting segment of the far heap region to move
;	DX = destination segment of the far heap region
;	AX = size of region to move in paragraphs
;
;Exit:
;	if BX > DX, move AX paragraphs down from BX:0 to DX:0
;	if BX < DX, move AX paragraphs up from BX:0 to DX:0
;	if BX = DX, clear AX paragraphs from BX:0
;
;Uses:
;	AX, BX, CX, DX, DI, ES.
;Exceptions:
;	None.
;******************************************************************************

;	Since the repeated string move instruction (REP MOVSW) is used,
;	a memory region must be split into a number of 64K-byte
;	partitions followed by final partition of less than 64K bytes.
;
;	For moving a region down in memory, the lowest word in the
;	region is moved down first, followed by the next up to the
;	highest word in the region.  To implement this using the minimum
;	number of string move instructions, the first partition is defined
;	at the lower boundary of the region with any succeeding partitions
;	located 64K bytes (or 1000H paragraphs) higher in memory.  The
;	string moves are executed with the direction flag cleared (CLD).
;
;	For moving a region up in memory, the highest word in the
;	region is moved up first, followed by the next down to the lowest
;	word in the region.  The first partition is defined from the
;	upper boundary of the region with any succeeding partitions
;	located 64K bytes (or 1000H paragraphs) lower in memory.  The
;	the string moves are executed with the direction flag set (STD).
;
;	For clearing a region in memory, each partition is cleared
;	separately.  The partitions are defined from lowest word in the
;	region to the highest as in moving the region down.  The first
;	word of the partition is cleared and then a repeated string move
;	propagated the cleared word to the end of the partition.

cProc	B$FHMove,<PUBLIC,NEAR>,<SI,DS> 
cBegin

; Add assertion to check if FH is expected to be stable.
DbAssertRel	[B$FHStable],Z,0,FH_TEXT,<Attempted to relocate far heap when not expected to.>

;	On procedure entry, the direction flag is assumed cleared and BX
;	and DX point to the lower boundary of the region before and after
;	the move, respectively.  For moving down or clearing, BX and DX
;	also point to the start of the first partition to be moved or
;	cleared.

	CMP	BX,DX		;test if moving down or clearing region
	JAE	FHMoveLoop	;if so, then jump to start of partition loop

;	For moving up the direction flag must be set and BX and DX point
;	to the end of the region which is also the end of the first
;	partition.  They will be adjusted to the partition start before
;	the string move is done.

	STD			;set direction flag - moves go from high to low
	ADD	BX,AX		;source pointer at partition end
	ADD	DX,AX		;destination pointer at partition end

;	This loop is executed for each partition.  The size of the partition
;	in paragraphs is computed.  AX contains the number of paragraphs
;	left to process.

FHMoveLoop:
	MOV	CX,1000H	;get default size of partition
	CMP	CX,AX		;test if a full partition can be processed
	JB	FHNotLast	;if so, then jump to use the default size
	MOV	CX,AX		;othewise use the remaining paragraphs
FHNotLast:
	SUB	AX,CX		;update paragraphs left to process

;	If moving up, update the partition pointers in BX and DX to the
;	start of the next partition lower in memory.

	CMP	BX,DX		;test if moving entry up
	JAE	FHNotUpNext	;if not, jump to set segments
	SUB	BX,CX		;point to start of source partition
	SUB	DX,CX		;point to start of destination partition
FHNotUpNext:

;	Set segment registers in preparation to string move.

	MOV	DS,BX		;set the source segment register
	MOV	ES,DX		;set the destination segment register

;	If moving down or clearing, update the partition pointers in
;	BX and DX to start of the next partition up in memory.

	CMP	BX,DX		;test if moving down or clearing
	JB	FHNotDownNext	;if not, then jump to set the count
	ADD	BX,CX		;point to start of next source partition
	ADD	DX,CX		;point to start of next destination partition
FHNotDownNext:

;	Compute the word count for the string move.

	SHL	CX,1		;Paragraphs to words - shift once...
	SHL	CX,1		;...and twice...
	SHL	CX,1		;...and thrice for the word count

;	Compute the source offset.  If moving down or clearing, the offset
;	is zero.  If moving up, the offset is the byte offset of the last
;	word in the partition.

	XOR	SI,SI		;assume source offset is zero
	CMP	BX,DX		;test if moving entry up
	JAE	FHNotUpOffset	;if not, then use jump to use the zero offset
	MOV	SI,CX		;get the word count for partition
	DEC	SI		;make zero-relative word index
	SHL	SI,1		;shift to make byte offset of last word
FHNotUpOffset:

;	Compute the destination offset.  If moving up or down, it is the
;	same as the source offset.  If clearing, it is two more than the
;	source offset.	Clearing uses a string move to overlap an initial
;	zero word entry through the entire partition.

	MOV	DI,SI		;assume moving - destination same as source
	CMP	BX,DX		;test if clearing partition
	JNE	FHNotClear	;if not, then jump to perfrom string move
	MOV	[SI],SI 	;clear the first word of the partition
	INC	DI		;set destination pointer to...
	INC	DI		;the next word in the partition
	DEC	CX		;first word is already zero
FHNotClear:

;	Perform the string move.  If any more partitions, then loop to
;	process them.

	REP	MOVSW		;move or clear the partition
	OR	AX,AX		;test if more partitions to process
	JNZ	FHMoveLoop	;if so, then jump
	CLD			;clear the direction flag

cEnd

;***
; B$FHTestRaiseBottom - raise bottom only if below DGROUP end
; B$FHRaiseBottom - raise the bottom of the far heap (DOS 3)
;
;Purpose:
; Raise the bottom boundary of the far heap to allow the released space to be
; used for near heap allocations. The far heap is raised to maximize the near
; heap size.
;
;Entry:
; None.
;
;Exit:
; None.
;
;Uses:
; AX	  (B$FHTestRaiseBottom)
; None.
;
;Exceptions:
; None.
;
;******************************************************************************
cProc	B$FHTestRaiseBottom,<NEAR,PUBLIC> ; Important: no frame
cBegin				
	MOV	AX,DS		; [AX] = paragraph base of dgroup
	ADD	AH,10H		; [AX] = paragraph base of dgroup + 64k
	CMP	AX,b$FHDEnd.FHD_hData ; See if FH below end of DGROUP
	JA	B$FHRaiseBottom ; Jump if it is, see if can reclaim DGROUP
cEnd				

;  Make B$FHRaiseBottom be called by vector

cProc	B$FHRaiseBottom,<NEAR,PUBLIC>,<AX,BX,CX,DX,SI>
cBegin

;	Compute the paragraphs to raise the far heap to maximize the
;	near heap size.

	MOV	DX,0FFFEH	;get maximum DGROUP for near heap
	SUB	DX,b$NH_last	;subtract to get maximum amount to raise
	MOV	CL,4		;byte-to-paragraph shift count
	SHR	DX,CL		;compute maximum paragraphs to raise

;	Compute the paragraphs of free space in the far heap.

	CALL	B$FHCompact	;compact the far heap (SI = desc of top entry)
	CALL	FHParaSize	;get paragraphs of free space

;	Select the minimum of the two values to use to raise the far heap.

	CMP	AX,DX		;compare available room with maximum request
	JB	FHRaiseBotAvail ;if available room less than max, jump
	MOV	AX,DX		;else use the maximum
FHRaiseBotAvail:
	ADD	b$FHDEnd.FHD_hData,AX ;adjust the bottom FH boundary

;	Raise the top of the near heap by AX paragraphs.

	SHL	AX,CL		;movement amount now in bytes (CL still 4)
	MOV	CX,AX		;new near heap ending offset is...
	ADD	CX,b$NH_last	;...adjusted by the amount
	MOV	AX,b$NH_first	;use present near heap starting offset
	CALL	B$NHMOV 	;raise the near heap top
	MOV	b$NH_last,CX	;update the near heap ending offset

cEnd

;***
; B$FHLowerBottom - lower bottom boundary of far heap. (DOS 3)
;
;Purpose:
;	Lower the bottom boundary of the far heap to allow the released
;	space to be used for far heap allocations. If the far heap
;	cannot be lowered by the requested amount, it is lowered the
;	maximum amount possible.  The top of the near heap is lowered
;	as necessary.
;Entry:
;	AX = number of paragraphs requested to lower.
;Exit:
;	None
;Uses:
;	AX
;Exceptions:
;	Fatal error due to near heap inconsistency.
;******************************************************************************

cProc	B$FHLowerBottom,<NEAR,PUBLIC>,<CX>
cBegin

;	First screen the request for unreasonable requests.  This includes
;	a request at or over 1000H paragraphs (64K bytes).

	TEST	AH,0F0H 	;test if request is 64K bytes or over
	JNZ	FHLowerBotMax	;if so, just try for maximum request

;	Convert the request to bytes and compute the new ending offset.
;	If the byte request is larger than the near heap ending offset,
;	then just try the maximum request.

	MOV	CL,4		;paragraph-to-byte shift is four...
	SHL	AX,CL		;shift to get bytes to lower near heap
	MOV	CX,b$NH_last	;get present ending offset of near heap
	SUB	CX,AX		;compute new ending offset in request
	JC	FHLowerBotMax	;if ending offset would be negative, then max

;	If this ending offset is before the current starting offset,
;	then just try maximum request.

	MOV	AX,b$NH_first	;get start of near heap for move operation
; Fix comparison order in CMP.
	CMP	CX,AX		;test if starting offset before computed ending
	JBE	FHLowerBotMax	;if so, just try for maximum room available

;	Try to lower the far heap by the request amount.

	CALL	B$NHMOV	;lower near heap to maximum size
	JNC	FHLowerBotDone	;if successful, jump to clean up and exit

;	If the near heap cannot be lowered by the requested amount,
;	lower it by the maximum amount possible.

FHLowerBotMax:
	MOV	AX,b$NH_first	;get the near heap starting offset
	XOR	CX,CX		;set offset for maximum release of near heap
	CALL	B$NHMOV 	;lower near heap to maximum size
;
; After the lowering, update the near heap ending offset variable, and compute
; the new start of the far heap, which is the immediately following paragraph.
;
FHLowerBotDone:
	MOV	b$NH_last,CX	; update new near heap ending offset
	XCHG	AX,CX		; [AX] = end of near heap
	MOV	CL,4		; [CL] = byte to para chift count
	ADD	AX,10H		; [AX] = end of near heap + 1 para
	RCR	AX,CL		
	AND	AH,01FH 	; [AX] = para dgroup offset of far heap
	MOV	CX,DS		
	ADD	AX,CX		; [AX] = absolute para far heap start
	MOV	b$FHDEnd.FHD_hData,AX ; Set the new far heap base

cEnd

;***
; B$FHRaiseTop - raise top boundary of far heap (DOS 3)
;
;Purpose:
;	Raise the top boundary of the far heap to allow the reclaimed
;	space to be used for far heap allocations.  If the far heap
;	cannot be raised by the requested amount, it is raised the
;	maximum amount possible.
;Entry:
;	AX = number of paragraphs requested to raise.
;Exit:
;	AX = number of paragraphs actually reclaimed.
;Uses:
;	None.
;Exceptions:
;	Fatal error due to far heap inconsistency.
;******************************************************************************

cProc	B$FHRaiseTop,<NEAR,PUBLIC>,<BX,ES>
cBegin

;	Calculate the paragraph increment from the current far heap
;	top to the theoretical maximum paragraph.  Attempt to reallocate
;	with the minimum of this value and the requested value.

	MOV	BX,b$FHDStart.FHD_hData ;get the top of the far heap
	NOT	BX		;invert for calculation 0FFFFH-FHTop
	CMP	AX,BX		;test for minimum of the two values
	JB	FHRaiseTopReq	;if request if less, then use it
	MOV	AX,BX		;use the calculated maximum value
FHRaiseTopReq:
	CALL	FHSetTop	;attempt to reset the far heap top

cEnd

;***
; B$FHLowerTop - lower top boundary of far heap (DOS 3)
;
;Purpose:
;	Lower the top boundary of the far heap to allow the released
;	space to be used for external allocations. If the far heap
;	cannot be lowered by the requested amount, it is lowered the
;	maximum amount possible.
;Entry:
;	AX = number of paragraphs requested to lower.
;Exit:
;	AX = number of paragraphs actually released.
;Uses:
;	None.
;Exceptions:
;	Fatal error due to far heap inconsistency.
;******************************************************************************

cProc	B$FHLowerTop,<NEAR,PUBLIC>,<BX,CX,DX,SI,DI,ES>
cBegin

;	First, compact the far heap and determine the size of the
;	unallocated space after compaction.

	MOV	CX,AX		;save requested allocation
	CALL	B$FHCompact	;compact - SI is next to last desc in list
	CALL	FHParaSize	;get unallocated size in AX

;	We will need to use the FHD_hData field from the descriptor pointed
;	to by SI, but the descriptor may move during B$FHLowerBottom.  So get
;	the seg value now.  (The descriptor may move but the data won't.)
	MOV	SI,[SI].FHD_hData ; get data seg of entry

;	Test if request can fit within the unallocated space left.
	;if enough, then branch to move heap

	CMP	CX,AX		;test request against unallocated space
	JBE	FHLowTopFit	;if enough, then branch to move heap

;	Compute the room still needed from the near heap and try to
;	obtain it.

	SUB	AX,CX		;subtract request size from unalloc space...
	NEG	AX		;...and negate to compute room needed
	CALL	B$FHLowerBottom ;try to lower far heap bottom AX paragraphs

;	Set up for far heap move.  Get present far heap starting
;	paragraph in BX.

FHLowTopFit:
	MOV	BX,SI		;segment of next to last entry is start

;	Compute new starting paragraph in DX.

	MOV	DX,BX		;start with present ending segment
	SUB	DX,CX		;subtract request size (size if full request)
	JB	FHLowMaxDown	;if wraparound, just use lower FH bound
	CMP	DX,b$FHDEnd.FHD_hData ;test if all memory requested released
	JA	FHLowTopFull	;if so, then jump to process full request
FHLowMaxDown:			
	MOV	DX,b$FHDEnd.FHD_hData ;set to partial request (end of heap)
FHLowTopFull:
;
; Check for the source and destination apara's to be the same. If they are, we
; didn't release anything, and needn't do more.
;
	XOR	AX,AX		; return value if no movement
	CMP	BX,DX		
	JZ	FHLowTopDone	; nothing to move, just quit.

;	Compute size of entire allocated far heap for move and perform
;	the move down in memory.

	MOV	AX,b$FHDStart.FHD_hData ;get starting segment of far heap
	SUB	AX,BX		;subtract ending segment for allocated size
	CALL	B$FHMove	;move far heap data to new location

;	Compute the actual distance moved.  Since the move was down,
;	this value is negative.  Set the far heap top to update the
;	descriptor and reallocate the DOS block to reflect the new
;	starting segment.

	MOV	AX,DX		;get new ending segment of far heap
	SUB	AX,BX		;subtract old ending segment to get distance
	CALL	FHSetTop	;reallocate the block to release the space

;	Adjust all allocated descriptors in the descriptor list to
;	reflect the far heap movement.

	XOR	BX,BX		;adjust all descriptors - lowest starting...
	MOV	DX,0FFFFH	;...and highest ending offset to adjust
	CALL	B$FHAdjDescSeg	;adjust all descriptor segments by AX

;	Negate the sense of the move to return a positive value.

	NEG	AX		;return positive paragraphs returned
FHLowTopDone:			

cEnd

;***
; B$FHByteSize - get size of unallocated entry after compaction (DOS 3)
;
;Purpose:
;	Return the size in bytes of the unallocated entry,
;	if present, after a far heap compaction is performed.
;Entry:
;	SI = DGROUP offset to next to last far heap descriptor in
;	     the descriptor list.  Returned by the far heap compaction
;	     routine B$FHCompact.
;Exit:
;	DX:AX = size in bytes of the unallocated entry.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$FHByteSize,<NEAR,PUBLIC>
cBegin

;	Get the entry size in paragraphs in AX and then convert to
;	bytes in DX:AX.

	CALL	FHParaSize	;get entry size in paragraphs
	MOV	DX,AX		;move into both registers
	MOV	CL,4		;shift count for lower word
	SHL	AX,CL		;compute low-word byte size
	MOV	CL,12		;shift count for upper word
	SHR	DX,CL		;compute high-word byte size

cEnd

;***
; FHParaSize - get size of unallocated entry after compaction (DOS 3)
;
;Purpose:
;	Return the size in paragraphs of the unallocated entry,
;	if present, after a far heap compaction is performed.
;Entry:
;	SI = DGROUP offset to next to last far heap descriptor in
;	     the descriptor list.  Returned by the far heap compaction
;	     routine B$FHCompact.
;Exit:
;	AX = size in paragraphs of the unallocated entry.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHParaSize,<NEAR>
cBegin

;	Size of entry is the difference between the segment values
;	of the next-to-last entry in the descriptor list and the
;	last entry.

	MOV	AX,[SI].FHD_hData ;get value of next-to-last entry
	SUB	AX,b$FHDEnd.FHD_hData ;subtract value of last entry

cEnd

;***
;B$FHSelect, B$FHSelectSI - Call a proc for each FH descriptor (DOS 3 & 5)
;
;Purpose:
;	Added as part of revision [11].
;	This routine is used to indirectly dispatch to a routine
;	for each FH descriptor.  The called routine takes specific
;	action for the descriptor and returns a flag stating whether
;	or not to deallocate the entry.
;Entry:
;	SI    = Address of descriptor to start with (B$FHSelectSI only)
;	AX    = Address of near routine to dispatch to
;	BX    = parameter passed/returned from dispatch routine
;	CX    = parameter passed/returned to dispatched routine
;Exit:
;	None.
;Uses:
;	Per Convention.
;Exceptions:
;	None.
;******************************************************************************
cProc	B$FHSelectSI,<NEAR,PUBLIC>,<SI>
cBegin
	JMP	SHORT FHSelect	;jmp to common code
cEnd	<nogen>

cProc	B$FHSelect,<PUBLIC,NEAR>,<SI>
cBegin
	MOV	SI,OFFSET DGROUP:b$FHDStart ;point to start of descriptor chain

FHSelect:
	PUSH	DI
	XCHG	AX,DI		;DI points to address of routine to call


SelectLoop:
	MOV	SI,[SI].FHD_pNext ;get ptr to next descriptor in chain
	CMP	SI,OFFSET DGROUP:b$FHDEnd ;test if final descriptor
	JZ	SelectDone	;brif so - finished

DbAssertRel	SI,NZ,-1,FH_TEXT,<FHSelect found pNext field containing 0FFFFH>



	CALL	DI		;call routine
	OR	AX,AX		;deallocate entry?
	JZ	SelectLoop	;brif not - get next descriptor
	PUSH	BX
	MOV	BX,SI
	cCall	B$FHDealloc	;deallocate entry
	POP	BX
	JMP	SHORT SelectLoop ;go get next descriptor
SelectDone:
	POP	DI
cEnd


;***
; B$FHAdjDesc,B$FHAdjDescSI - adjust a range of far heap descriptor (DOS 3 & 5)
;
;Purpose:
;	Added as part of revision [11].
;	To adjust selected far heap descriptors by adding a given
;	value to the pNext field in each descriptor.  The
;	selection is determined by the DGROUP offset of the
;	descriptor being in the specified range.  The static
;	starting and ending descriptors are never adjusted here.
;	These routines are used when Far Heap DESCRIPTORS are moved.
;Entry:
;	AX = adjustment value to the pNext field of each affected
;	     far heap descriptor.
;	BX = lower DGROUP offset of descriptor range, inclusive.
;	DX = upper DGROUP offset of descriptor range, exclusive.
;	SI = address of descriptor to start adjustment with.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$FHAdjDescSI,<NEAR,PUBLIC>,<SI>
cBegin
	JMP	SHORT FHAdjDescCommon
cEnd	<nogen>

cProc	B$FHAdjDesc,<NEAR,PUBLIC>,<SI>
cBegin
	MOV	SI,OFFSET DGROUP:b$FHDStart ;Start with beginning of list

FHAdjDescCommon:
	XCHG	AX,CX		;CX = desc adjustment
	XOR	AX,AX		;no segment adjustment
	cCall	FHAdjDesc	;call common routine to adjust seg/desc
cEnd


;***
; B$FHAdjDescSeg,FHAdjDescSegSI - adjust a range of far heap descriptor segments (DOS 3)
;
;Purpose:
;	FHAdjDescSegSI added as part of revision [32].
;	Renamed and rewritten as part of revision [11].
;	To adjust selected far heap descriptors by adding a given
;	value to the segment value in each descriptor.	The
;	selection is determined by the DGROUP offset of the
;	descriptor being in the specified range.  The static
;	starting and ending descriptors are never adjusted here.
;	This routine is used when Far Heap DATA is moved.
;Entry:
;	AX = adjustment value to the segment of each affected
;	     far heap descriptor.
;	BX = lower DGROUP offset of descriptor range, inclusive.
;	DX = upper DGROUP offset of descriptor range, exclusive.
;	SI = descriptor to start adjustments with (FHAdjDescSegSI only)
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHAdjDescSegSI,<NEAR>,<SI>
cBegin
	JMP	SHORT AdjDescSegCommon
cEnd	<nogen>

cProc	B$FHAdjDescSeg,<NEAR,PUBLIC>,<SI>
cBegin
	MOV	SI,b$FHDStart.FHD_pNext ;[16]start with second FH desc in list

AdjDescSegCommon:		
	XOR	CX,CX		;No adjustment for pNext field
	cCall	FHAdjDesc	;common routine to adjust seg/descriptor
cEnd


;***
; FHAdjDesc - adjust a range of far heap descriptors (DOS 3 & 5)
;
;Purpose:
;	Added as part of revision [11].
;	To adjust selected far heap descriptors by adding a given
;	value to the segment value and/or to the pNext field in each
;	descriptor.  The selection is determined by the DGROUP offset
;	of the descriptor being in the specified range.  The static
;	starting and ending descriptors are never adjusted here.
;	This routine is used when Far Heap DATA and or Far Heap
;	descriptors are moved.
;Entry:
;	AX = adjustment value to the segment of each affected
;	     far heap descriptor.
;	CX = adjustment value to the pNext field of each affected
;	     far heap descriptor.
;	BX = lower DGROUP offset of descriptor range, inclusive.
;	DX = upper DGROUP offset of descriptor range, exclusive.
;	SI = ptr to FHD to start search with.
;Exit:
;	None.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHAdjDesc,<NEAR,PUBLIC>,<SI,DI>
cBegin

;	Using SI, traverse the descriptor list until the ending of the
;	descriptor list is reached.

	MOV	DI,SI		;fall into loop

FHAdjDescLoop:
	MOV	SI,DI		;[SI] = current desc location
	MOV	DI,[SI].FHD_pNext ;[DI] = next descriptor in the list
	OR	DI,DI		;test if end of descriptor list
	JZ	FHAdjDescDone	;if so, then done

DbAssertRel	DI,NZ,-1,FH_TEXT,<FHAdjDesc found a pNext field containing 0FFFFH>

;	For each descriptor, adjust if in the specified range.

	CMP	DI,BX		;test against lower range, inclusive
	JB	FHAdjDescLoop	;if less, then loop for next
	CMP	DI,DX		;test against upper range, exclusive
	JAE	FHAdjDescLoop	;if more or equal, then loop for next
	ADD	[SI].FHD_hData,AX ;in range - adjust FH segment
	ADD	[SI].FHD_pNext,CX ;in range - adjust FH desc list
	JMP	SHORT FHAdjDescLoop ;loop for next descriptor

;	Done - restore and return.

FHAdjDescDone:

cEnd
	PAGE
;***
; B$FHAdjOneDesc - adjust a far heap descriptor (DOS 3 & 5)
;
;Purpose:
; Added as part of revision [44].
; This routine is called when a single far heap descriptor is about to be
; moved. We traverse the FHD chain in search of the FHD containing a pNext that
; points to the FHD about to move, and then we adjust that pNext to reflect the
; movement. This routine adjusts only one FHD, and stops as soon as a match is
; found, or the end of the FHD chain is found.
;
; NOTE: THIS ROUTINE IS SPEED CRITICAL. The code below is written for speed.
; The main loop deals with TWO successive FHDs each time through the loop to
; eliminate a bit of pointer movement.
;
;Entry:
; [CX]	= adjustment value to the pNext field of the affected far heap
;	  descriptor.
; [DX]	= pointer to FHD which is about to move.
;
;Exit:
; None.
;
;Uses:
; Per Convention
;
;Exceptions:
; None.
;******************************************************************************

cProc	B$FHAdjOneDesc,<NEAR,PUBLIC>,<SI>
cBegin

	MOV	SI,OFFSET DGROUP:b$FHDStart ;Start with beginning of list
ADJONELOOP:

DbAssertRel	SI,NZ,0,FH_TEXT,<FHAdjOneDesc hit end of FarHeap Chain>
	MOV	BX,[SI].FHD_pNext ;[BX] = next descriptor in the list
	CMP	BX,DX		;do we point to the FHD of interest?
	JZ	ADJONESI	;Then adjust the next pointer via BX

DbAssertRel	BX,NZ,0,FH_TEXT,<FHAdjOneDesc hit end of FarHeap Chain>
	MOV	SI,[BX].FHD_pNext ;[SI] = next descriptor in the list
	CMP	SI,DX		;do we point to the FHD of interest?
	JNZ	ADJONELOOP	;Loop until we do or are done

	MOV	SI,BX		;[SI] = pointer to preceding FDH for adjust
ADJONESI:
	ADD	[SI].FHD_pNext,CX ;adjust FH desc list
ADJONEDONE:

cEnd



;***
; B$SETM - SETMEM function runtime entry point (DOS 3 & 5)
;
;Purpose:
;	To attempt to adjust the top limit of the far heap by the
;	(signed) number of bytes in the input parameter.  The function
;	returns the space allocated for both the near and far heaps in bytes.
;Entry:
;	[SP+4] = low-order word of adjustment value.
;	[SP+6] = high-order word of adjustment value.
;Exit:
;	DX:AX = 32-bit size of near and far heaps in bytes.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	B$SETM,<FAR,PUBLIC>
	parmD	incr
cBegin

;	Get adjustment value in DX:AX.

	MOV	AX,off_incr	;get low-order word
	MOV	DX,seg_incr	;get high-order word


;	Test sign of adjustment value and jump if negative.

	OR	DX,DX		;test if adjustment is negative
	JS	FHSetMemNeg	;if so, then jump

;	Positive adjustment.  Get request in AX and raise far heap top.

	CALL	FHSetMemReq	;get request value in AX
	CALL	B$FHRaiseTop	;attempt to raise far heap top
	JMP	SHORT FHSetMemCommon  ;jump to common code to finish

;	Negative adjustment.  Negate value, get request in AX, and lower
;	the far heap

FHSetMemNeg:
	NEG	AX		;negate lower word of value in DX:AX
	ADC	DX,0		;propagate carry to upper word
	NEG	DX		;and negate upper word to finish
FHSetMemLower:			
	CALL	FHSetMemReq	;get request value in AX
	CALL	B$FHLowerTop	;attempt to lower far heap top

;	Common code.  Calculate size of far and near heaps and
;	return value to caller.

FHSetMemCommon:
	CALL	B$FHTestRaiseBottom ; and attempt to raise bottom also
	CALL	FHSetMemValue	;compute size in DX:AX

				; 32K near heap + 128K far heap
	call	fEditorActive	; did we start with /Editor
	jnz	SetMem_Exit	; brif so, allow anything
	cmp	dx,2		; less than 128K total free?
	jb	SetMem_Exit	; brif so -- exit
	ja	TooBig		; brif more than 172K free -- too much free
	or	ax,ax		; less than 128K + 32K free?
	jns	SetMem_Exit	; brif so -- exit

TooBig:				; Reserve <ABOUT> 128K + 32K heap space
	sub	ax,7ff0h	; Can't do 8000h or it will infinite loop!)
	sbb	dx,2		
	jmp	FHSetMemLower	; lower top of far heap by DX:AX
	
SetMem_Exit:			

cEnd

;***
; FHSetMemReq - compute SETMEM request value (DOS 3)
;
;Purpose:
;	Convert the given request value in bytes to paragraphs.
;	If more than 1M bytes, return 0FFFFH paragraphs.
;Entry:
;	DX:AX = 32-bit request value in bytes.
;Exit:
;	AX = 16-bit request value in paragraphs.
;Uses:
;	DX.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHSetMemReq,<NEAR>
cBegin

	CALL	FHByteToPara	;convert DX:AX in bytes to DX:AX in paragraphs
	OR	DX,DX		;test if over 0FFFFH paragraphs
	JZ	FHSetMemReqOK	;if not, then request is okay as is
	MOV	AX,0FFFFH	;set maximum request
FHSetMemReqOK:

cEnd

;***
; FHSetMemValue - compute the SETMEM function value (DOS 3)
;
;Purpose:
;	Compute the size of the near and far heaps in bytes.
;	This is the value returned by the SETMEM function.
;Entry:
;	None.
;Exit:
;	DX:AX = size of heaps in bytes.
;Uses:
;	None.
;Exceptions:
;	None.
;******************************************************************************

cProc	FHSetMemValue,<NEAR>,<CX>
cBegin

;	Compute the paragraph start of the near heap.

	MOV	AX,b$NH_first	;get DGROUP offset of the near heap start
	MOV	CL,4		;byte-to-paragraph factor is 4
	SHR	AX,CL		;get paragraph offser to DGROUP of NH start
	MOV	CX,DS		;get DGROUP paragraph value
	ADD	AX,CX		;add to get paragraph start of near heap

;	Difference from far heap start to near heap start is value
;	to compute.  Convert size from paragraphs to bytes.

	MOV	DX,b$FHDStart.FHD_hData ;get starting (top) paragraph of FH
	SUB	DX,AX		;paragraph size of near and far heaps
	MOV	AX,DX		;move into both registers
	MOV	CL,4		;shift count for lower word
	SHL	AX,CL		;compute low-word byte size
	MOV	CL,12		;shift count for upper word
	SHR	DX,CL		;compute high-word byte size

cEnd

;***
; FHSetTop - set new top of the far heap. (DOS 3)
;
;Purpose:
;	Set the new top of the far heap by reallocating the DOS
;	memory block containing the program and updating the starting
;	descriptor which defines the top of the far heap.
;Entry:
;	AX = number of paragraphs to adjust the far heap top.
;	     (positive grows, negative shrinks)
;Exit:
;	AX = number of paragraphs actually adjusted.  This number
;	     may be different than the entry value if the heap grows
;	     and insufficient memory is available.
;Uses:
;	ES.
;Exceptions:
;	Fatal error if DOS reallocation fails with legal pool size.
;
;******************************************************************************

cProc	FHSetTop,<NEAR>,<BX>
cBegin

;	Attempt to reallocate the DOS block with the given increment
;	of size.

	MOV	ES,__acmdseg	;get DOS memory block address
	MOV	BX,b$FHDStart.FHD_hData ;get top segment of the far heap
	SUB	BX,__acmdseg	;subtract to get present block size
	ADD	BX,AX		;add increment to get new block size
	MOV	AH,4AH		;get DOS function to reallocate
	CALL	B$FHMemDosCall	;call to perform INT and handle fatal errors
	JNC	FHSetTopDone	;if no error, then jump

;	Reallocation failed with attempted increment.  Try to reallocate
;	with the given maximum size returned in BX.

	MOV	AH,4AH		;try the reallocation again
	CALL	B$FHMemDosCall	;call to perform INT and handle fatal errors
	JC	FHSetTopError	;if reallocation failed, then fatal error

;	Reallocation with new block size in BX succeeded.  Update the
;	far heap descriptor for the top of far heap.

FHSetTopDone:
	ADD	BX,__acmdseg	;add to get new top far heap segment
	MOV	AX,BX		;move top segment...
	SUB	AX,b$FHDStart.FHD_hData ;and subtract to get change in top seg
	MOV	b$FHDStart.FHD_hData,BX ;update new top segment in descriptor

cEnd

FHSetTopError:
	JMP	B$ERR_OM_FH	;give out of memory error


;***
; B$FHMemDosCall - perform memory DOS call (DOS 3)
;
;Purpose:
;	Performs the INT 21H for the DOS allocate, deallocate, and
;	reallocate calls.  Error checking is done here to save space.
;Entry:
;	Setup for the appropriate DOS call, this call replaces the
;	INT 21H instruction.  The DOS calls used are allocate memory
;	(48H), deallocate memory (49H), and reallocate memory (4AH).
;Exit:
;	Post-INT 21H register changes.
;Uses:
;	Post-INT 21H register changes.
;Exceptions:
;	B$ERR_MEM if arena is trashed.
;	B$ERR_FHC if bad memory block address.
;******************************************************************************

cProc	B$FHMemDosCall,<NEAR,PUBLIC>
cBegin

	INT	21H		;perform the DOS call
	JNC	FHMemDosReturn	;if successful, then exit now
	CMP	AX,8		;test if out of memory
	JNE	FHMemDosError	;if not, then jump to trap error
	STC			;set the carry again
FHMemDosReturn:

cEnd

FHMemDosError:
	CMP	AX,7		;test if error was arena destroyed
	JNE	FHMemDosBlock	;if not, then invalid block was used
	JMP	B$ERR_MEM	;jump to fatal error for arena destroyed
FHMemDosBlock:
	JMP	B$ERR_FHC	;jump to fatal error for inconsistenct FH

sEnd	FH_TEXT


	END
