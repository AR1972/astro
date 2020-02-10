	TITLE	NHLHUTIL - Local Heap utilities
;***
; NHLHUTIL - Local Heap utilities
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE rmacros.inc

		USESEG	_DATA
		USESEG	_BSS
		USESEG	NH_TEXT


	INCLUDE seg.inc
	INCLUDE nhutil.inc	;for heap definitions
	INCLUDE idmac.inc
	INCLUDE array.inc	;for array definitions

sBegin	_BSS


	externW b$STRING_FIRST		;defined in NHSTUTIL.ASM
	externW b$NH_first		;defined in NHINIT.ASM
	externW b$NH_last		;defined in NHINIT.ASM

	externW b$HEAP_FIRST		; heap start pointer
	externW b$HEAP_FREE		
	externW b$HEAP_END		
	externW b$P_HEAP_GROW		

CW_SWAP_VARS	EQU	4		; must swap 4 words to chg context
	externW b$HEAP_FIRST_SWAP	
	externW b$HEAP_END_SWAP 	

	externW b$fVarHeapActive	; non-0 when variable heap
					; is active

	externB b$Chaining	; in chain flag
	externW b$commonfirst	
	externW b$commonlast	

sEnd	_BSS

sBegin	_DATA

	externW b$pFHRaiseBottom	;vector for B$FHRaiseBottom

sEnd	_DATA



sBegin	NH_TEXT

	ASSUMES CS,NH_TEXT

	PUBLIC	B$NHCPCT	;compact all dynamic space
	PUBLIC	B$NHMOV	;mov dynamic space boundaries


	PUBLIC	B$LHFDBLOC	; find file entry for file number given
	PUBLIC	B$LHLOCFDB	; find file number for file entry given

	PUBLIC	B$ILHALC	;allocate heap entry - error code return

	PUBLIC	B$LHFLDDESCADD	; add descriptor to heap backpointer string

	PUBLIC	B$LHChgBakPtr	;called to change the location of an LH owner
	PUBLIC	B$LHREALC	;reallocate heap entry
	PUBLIC	B$ILHADJ	;update backpointer in a given LH entry
	PUBLIC	B$LHForEachEntry ;call given function for each LH entry
	PUBLIC	B$LH_SPLIT

	externNP B$LHADJ	; adjust heap entry
	externNP B$LHDALC	; deallocate heap entry
	externNP B$LHSetFree	; set free heap entry pointer
	externNP B$LH_ALC_FREE	; try to allocate the free heap entry
	externNP B$LH_CPCT	
	externNP B$LH_FROM_SS	

	externNP B$LH_PTR_CHECK ; check entry at [SI] for consistency
	externNP B$LH_PTR_FROM_DATA 
	externNP B$LH_SCAN	

	externNP B$STDALC
	externNP B$STALCTMPSUB
	externNP B$STCPCT
	externNP B$STFromLH
	externNP B$STMOV
	externNP B$STDALCTMPDSC
	externNP B$ERR_OM_NH		 

	externNP B$SSClean	

	externNP B$IHeapEntryMoved


SET_ES_TO_DS	MACRO
	PUSH	DS
	POP	ES
	ENDM

ASSERT_NOT_VARHEAP	MACRO	SEG	;
	ENDM				;


;Given a user requested size, convert to total resulting size of entry
GET_ENTRY_SIZE	MACRO	CBGIVEN,LHTYPE
	LOCAL	CONTINUE
	.xlist
	ADD	CBGIVEN,LH_STD_HDR_LEN+2+1 ;add for hdr, backlength, & roundup
	AND	CBGIVEN,0FFFEh	;finish roundup
	CMP	LHTYPE,LOW LH_FILE
	JNZ	CONTINUE	;brif not an fdb entry
	ADD	CBGIVEN,(LH_FDB_HDR_LEN - LH_STD_HDR_LEN)
				;fdb header is bigger than standard header
CONTINUE:
	.list
	ENDM


;***
; B$LHALC_CPCT - Compact local heap and allocate heap entry.  Added with [44]
;
; Purpose:
;	Combined with B$LHALC as part of [44]
;	Same as B$ILHALC, below, but jumps to B$ERR_OM on out of memory error.
;	Also, compacts heap before allocation.
;
; Inputs:
;	BX = Length of local heap space required.
;	DL = type of heap entry to allocate.
;	CL = if DL=LH_FILE, file number
;	CX = if DL anything else, ptr to owner (where backptr should point to)
; Outputs:
;	SI = Address of start of data of the allocated entry.
; Modifies:
;	None.
; Preserves:
;	ES
; Exceptions:
;	Will jump to B$ERR_OM if insufficient memory for allocation.
;****

cProc	B$LHALC_CPCT,<NEAR,PUBLIC>
cBegin
	CALL	B$LH_CPCT		; compact heap before allocation
	CALL	B$ILHALC		; allocate the entry
	JC	BLHALC_OM_ERR		; psw.c set on error
cEnd

BLHALC_OM_ERR:
	JMP	B$ERR_OM_NH		; out of memory error

;***
; B$ILHALC - allocate local heap entry
; Purpose:
;	Find and allocate an appropriate entry of the local heap.
;	Ten bytes will be added to the requested length with 2 bytes
;	for the backlength and 8 bytes for the heap header.  The
;	This value is rounded up to the next 8-byte value since all
;	allocations are multiples of this value.
;
;	Each successive step is performed until the allocation is done:
;
;	1. Test the free heap entry pointed by [b$HEAP_FREE] for
;	   being unallocated and having the adequate room.
;
;	2. The local heap space is searched from start to end for the
;	   first unallocated entry large enough to satisfy the allocation.
;	   Adjacent unallocated entries are concatenated as they are
;	   found before the allocation is attempted.  If no allocation
;	   can be done, the free heap entry is moved to just past the
;	   last allocated entry in the heap.
;
;	3. If the free string entry is unallocated and the last in string
;	   space, the string-heap boundary is moved so that its space
;	   is added to the free heap entry.  The free entry is tested.
;
;	4. A compaction of string space is performed, leaving a free
;	   unallocated string in high memory.  This storage is placed
;	   in the local heap as in step 3.  The free entry is then tested.
;
;	5. If this is an interpreter-version of the runtime, compact the
;	   Local Heap, and try the free heap entry again.
;
;	If no allocation cannot be done, the program is aborted by
;	   an "Out of Memory" error.
;
;	If the allocation is successful and string space was converted
;	   to heap space, the remaining free heap space is given back
;	   to the string space.
;
;	NOTE: it is assumed that the input owner-to-be is NOT in
;	the local heap (and thus, that heap movement will not cause the
;	owner to move).
;
; Inputs:
;	BX = Length of local heap space required.
;	DL = type of heap entry to allocate.
;	CL = if DL=LH_FILE, file number
;	CX = if DL anything else, ptr to owner (where backptr should point to)
;
; Outputs:
;	if PSW.C is clear
;		SI = Address of start of data of the allocated entry.
;	else (PSW.C set) out of memory error return.
; Modifies:
;	SI
; Preserves:
;	ES
; Exceptions:
;	None.
;****

B$ILHALC	PROC	NEAR
	PUSH	AX		;save the registers used...
	PUSH	BX
	PUSH	ES		


	SET_ES_TO_DS		;set ES = DS if interpreter version

	GET_ENTRY_SIZE	BX,DL	;convert input size to total entry size needed

;	Step 1 - Test if free heap entry can perform allocation.

	CALL	B$LH_ALC_FREE	; try to allocate the free heap entry
	JNC	ALCLHP_FIRST	;if successful, then jump to return

	PUSH	CX		;push more registers...
	PUSH	DX
	PUSH	DI

;	Step 2 - Scan local heap from beginning to end.

	CALL	B$LH_SCAN	;scan the local heap
	JNC	ALCLHP_DONE	;jump if allocation successful

	CALL	[b$P_HEAP_GROW] ; call appropriate routine to complete
				;   allocation (carry clear on return if 
				;   successful)
ALCLHP_DONE:
	POP	DI		;restore registers used...
	POP	DX
	POP	CX

ALCLHP_FIRST:
	POP	ES		
	POP	BX
	POP	AX
	RET			;return with SI pointing to heap data area
B$ILHALC	ENDP

;***
; B$VAR_ALC_GROW - Grow var heap to support allocation of a block of given size
; Purpose:
;	Added with revision [23].
;	Called when B$ILHALC called to [re]alloc a var heap entry and has
;	insufficient space in the var heap.
;	Grows the var heap by (just) the required amount, recurses to B$ILHALC
;	to actually do the allocation.
; Inputs:
;	ES = DS
;	BX = total size of local heap space to be allocated
;	DL = type of heap entry to allocate.
;	CL = if DL=LH_FILE, file number
;	CX = if DL anything else, ptr to owner (where backptr should point to)
; Outputs:
;	Carry Clear if allocation accomplished successfully
; Modifies:
;	SI
; Exceptions:
;
;****
cProc	B$VAR_ALC_GROW,<NEAR,PUBLIC> 
	LocalW	junk
cBegin
;	Step 3 - Compress the variable heap and try to alloc again
	CALL	B$LH_CPCT	;combine all free entries into one
	CALL	B$LH_SCAN	;scan the local heap
	JNC	VAR_ALC_EXIT	;jump if allocation successful

;	Step 4 - Allocate a local heap entry of required size. If this fails,
;		 quit. If it succeeds, free that entry, move string space up
;		 (putting the required freespace in variable heap), and
;		 recurse to B$ILHALC to do the allocation.
	PUSH	BX			; save original request size
	PUSH	DX
	PUSH	CX
	ADD	BX,LH_STD_HDR_LEN+2	; grab enough space from LH for
					; required entry PLUS overhead
	DbAssertRelB  dl,nz,<LOW LH_FILE>,NH_TEXT,<VAR_ALC_GROW: FDB entry>
	; the above assertion is due to the fact that we're just adding
	; in overhead for a standard heap entry, not an FDB

	PUSH	BX
	CALL	B$TglHeapSptNEAR	; switch context to local heap
	LEA	CX,[junk]		;for back ptr
	CALL	B$ILHALC		;If this succeeds, we now have
					;  sufficient space in DGROUP for alloc
	JC	VAR_ALC_FAIL		; brif insufficient space in system

	CALL	B$LHDALC		;deallocate that space now
	POP	SI			
	PUSH	SI			
	ADD	SI,[b$STRING_FIRST]	; we want to move SS up to here
	CALL	B$STMOV
	JNC	BSTMOV_SUCCESS  	; brif success

	CALL	B$LH_CPCT		; compact local heap
	CALL	B$STMOV		; try again

BSTMOV_SUCCESS:				
	CALL	B$TglHeapSptNEAR	; switch context back to var heap
	POP	BX
	POP	CX
	POP	DX
	;Move top of Var Heap up to string space
	ADD	[b$HEAP_FIRST],BX	
	MOV	SI,[b$HEAP_FIRST]	
	;Mark this as a free entry
	MOV	[SI].LHTYPE,LOW LH_FREE
	MOV	[SI].LHLEN,BX
	MOV	[b$HEAP_FREE],SI
	SUB	SI,BX			
	MOV	[SI+1],BX		; backlength of free entry
	POP	BX			; restore original size request
	CALL	B$ILHALC		;MUST succeed
	JNC	VAR_ALC_EXIT

	DbHalt NH_TEXT,<VAR_ALC_GROW: B$ILHALC call failed!>
VAR_ALC_FAIL:
	CALL	B$TglHeapSptNEAR	; switch context back to var heap
	POP	BX			; restore stack for exit
	POP	CX			
	POP	DX			
	POP	BX			
	STC				; signal failure
VAR_ALC_EXIT:
cEnd

;***
; B$VarHeap_CPCT - Compact the Variable heap down, put free space in SS.
; Purpose:
;	Added with revision [23].
;	Compact the variable heap down, give all resulting free space to
;	string space.
; Inputs:
;	None
; Outputs:
;	None
; Modifies:
; Exceptions:
;	None
;****
cProc	B$VarHeap_CPCT,<PUBLIC,NEAR>,<SI>
cBegin
	ASSERT_NOT_VARHEAP NH_TEXT	
	CALL	B$TglHeapSptNEAR	; switch context to variable heap
	CALL	LH_MOV_DOWN		;crunches heap down, leaving hole
					;  above; returns new b$HEAP_FIRST in SI
	INC	SI			;SI points to where SS is to start at
	CALL	B$STMOV		;mov SS down
	CALL	B$TglHeapSptNEAR	; switch context back to local heap
cEnd






;***
; B$LHREALC - reallocate a Local Heap entry
; Purpose:
;	Given a pointer to the start of data in an existing local heap
;	entry (which is guaranteed to have a back pointer in the header)
;	and a byte count, reallocate the entry to be of the given byte
;	count size.
;	Note that, if the reallocation results in a reduction or no
;	change in the size, this routine is guaranteed not to cause
;	heap movement.
;	Note also that this routine will succeed if sufficient space
;	exists for the reallocation; it does NOT require the heap to
;	have the full input size free, as it just grabs the additionally
;	required space and combines it with the given entry.
;
;	NOTE: it is assumed that the owner of the given entry is NOT in
;	the local heap if the entry is growing (and thus, that heap movement
;	will not cause the owner to move).
;
; Inputs:
;	AX = number of bytes to realloc to.
;	SI = pointer to start of data in an LH entry; note that the
;		entry is assumed to have a back pointer.
; Outputs:
;	AX = FALSE if insufficient memory for reallocation,
;	     TRUE (non-zero) if operation successful.
;	SI = pointer to the start of data for the reallocated entry if
;		AX = TRUE (note, however, that SI is trashed if AX = FALSE).
;		This may or may not be different from the entry. Note that,
;		although we do have a backpointer, this routine does not
;		update the pointer - - - the return value of SI is provided
;		for the caller to do that.
; Modifies:
;	AX & SI are outputs, plus BX,CX,DX are be modified, and ES = DS,
;		regardless of input value.
; Exceptions:
;
;****
B$LHREALC	PROC	NEAR
	SET_ES_TO_DS		;set ES = DS if interpreter version
	PUSH	DI
	MOV	DI,AX		;DI = copy of input size request
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer

 	; The below assertion is based on assumptions below on the size
 	; of the entry header - - - which is just to save bytes
 	DbAssertRelB [SI].LHTYPE,NZ,LH_FILE,NH_TEXT,<can't realloc fdb's>
 	ADD	AX,LH_STD_HDR_LEN+2+1 ; add for hdr, backlength, & roundup
 	AND	AX,0FFFEh	; finish roundup

	MOV	CX,AX		;save size of desired entry in CX
	MOV	BX,[SI].LHLEN	;get length of existing entry
	SUB	AX,BX		;subtract existing size from desired size
	JA	BLHREALC_Grow	;brif we must grow the existing entry

	NEG	AX		;make difference a positive number
	CALL	B$LH_SPLIT	;split this entry; free entry out of spare space
	MOV	SI,DI		;SI = pointer to realloc'd entry
	SUB	SI,[SI].LHLEN	;move SI back to previous entry, and then
	ADD	SI,3		;  make it data pointer to realloc'd entry
	JMP	REALC_TRUE_Exit ;done - return TRUE to signal success

BLHREALC_Grow:
;	Algorithm:
;		get and save backpointer
;	    STEP1:
;		call B$ILHALC with the input byte count
;		if this succeeds, block copy the contents of the original
;			entry and free it; ensure the back pointer in the
;			new entry is correct - exit and return TRUE
;	    STEP2:
;		call B$ILHALC for the additional space required
;		if this fails, return FALSE
;		free the newly obtained entry, but keep a pointer to it.
;		if this entry is above entry to realloc in memory, collapse
;			the Local Heap, split resulting free entry so that
;			higher of two is required size, set free ptr to that
;		else set free ptr to newly free'd entry.
;		call LH_MOV_DN_RG to move all from free ptr and entry to
;			realloc down
;		change header and trailer to combine free entry with given
;			entry


	;----------------------------------------------------------------------
	;Start of code to Grow an existing entry - at this point:
	;	AX = additional space needed (on top of what current entry has)
	;	BX = size of existing entry
	;	CX = total size of entry needed to satisfy users request
	;	DI = input size request
	;	SI = pointer to header of entry-to-realloc
	;----------------------------------------------------------------------

	;growing an entry - STEP 1: try to allocate a block of size to
	;				accomodate entire entry, then copy
	;				original entry contents, free original

	PUSH	AX		;save additional space required
	MOV	CX,[SI].LHBAKP	;get backpointer
	MOV	DL,[SI].LHTYPE	;input to B$ILHALC
	MOV	BX,DI		;input realloc size request
	CALL	B$ILHALC	;try to alloc a block of size caller requested

	MOV	DI,SI		;DI = ptr to start of data in new entry
	MOV	DX,SI		;DX = ptr to start of data in new entry
	MOV	SI,CX		;SI = backpointer
	MOV	SI,[SI] 	;SI = ptr to start of data in old entry
	JC	REALC_STEP2	;brif insufficient memory for whole block

	;now just block copy data from old entry to new, & free old entry
	PUSH	AX		;save additional space required across call
	PUSH	SI		;save pointer to data in entry across call
	PUSH	BX		;modified by call to b$LH_I_ADJ
	MOV	AX,DI
	SUB	AX,SI		;AX = adjustment factor for backpointers to
				;	any owners contained in this entry
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer
	CALL	B$LH_I_ADJ	; adjust backpointers to any owners in entry
	POP	BX
	POP	SI
	POP	AX
	MOV	CX,[SI-2]	;CX = number of bytes in original entry
 	SUB	CX,LH_STD_HDR_LEN+2 ; only copy data, not old header ...
	SHR	CX,1		;CX = number of words in original entry
	PUSH	SI
	REP	MOVSW		;copy contents of old entry to new
	POP	SI		;need this ptr for deallocation
	CALL	B$LHDALC	;deallocate original entry
	POP	AX		;clean up stack
	MOV	SI,DX		;return value SI = ptr to start of entry data
DJMP	JMP	SHORT REALC_TRUE_Exit 

	;growing an entry - STEP 2: try to allocate a block of the size of
	;				the additional space required, then
	;				free this block; if this entry is
	;				above entry to realloc in memory,
	;				collapse the Local Heap, split
	;				resulting free entry, set ptr to higher
	;				of the two free entries; call
	;				LH_MOV_DN_RG to move all from free
	;				ptr to entry to realloc down in memory.
	;				Change hdr to combine free entry with
	;				given entry.
REALC_STEP2:
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer
	POP	BX		;additional space required for realloc
	PUSH	BX		;still want this saved
	PUSH	CX		;save backpointer to input entry
	PUSH	AX		;put a word on stack to act as backptr for alloc
	MOV	CX,SP		;backpointer
	MOV	DL,[SI].LHTYPE
	CALL	B$ILHALC	;try to alloc block of additional size required
	POP	CX		;clean stack
	POP	BX		;backpointer to input entry
	POP	DX		;additional space required for realloc
	JC	JB_REALC_FALSE_Exit ;brif insuff. memory - can't realloc [52]

	MOV	DI,SI		;DI = ptr to start of data in new entry
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer
	XCHG	SI,DI
	CALL	B$LHDALC	;now free this entry - keep its hdr ptr in DI
	MOV	SI,[BX] 	;SI = ptr to start of data in entry to realloc
	CMP	SI,DI		;is entry-to-realloc above free entry?
	JA	REALC_MOV_DOWN	;  brif so - don't need to collapse

	;free entry is above entry-to-realloc; must collapse the local heap to
	;  get it below, then split resulting free entry so we can then 'bubble
	;  up' a free entry of the size we wish to add to the input entry
	CALL	B$LH_CPCT	   ;compact current entries to top of heap
	MOV	SI,[BX] 	;SI = ptr to start of data in input entry
	MOV	DI,[b$HEAP_FREE] ;DI = pointer to resulting free entry at bottom
	MOV	AX,[b$HEAP_END]
	MOV	[b$HEAP_FREE],AX ;no longer a b$HEAP_FREE - - - we'll move it
				;  all up in memory to realloc a piece of it
REALC_MOV_DOWN:
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer
	PUSH	DX
	MOV	DX,SI
	MOV	SI,DI		;hdr pointer for free entry
	CALL	LH_MOV_DN_RG	;mov all in this range down
	POP	DX
	;Now, DI points to hdr of entry to realloc. If next entry up is
	;  a free entry of sufficient size, split it (if necessary), and
	;  tack on the extra amount needed to fulfill reallocation request.
	MOV	SI,DI
	ADD	SI,[SI+1]	;SI = pointer to next entry up
	CMP	[SI].LHTYPE,LOW LH_FREE
	JNZ	REALC_FALSE_Exit ;brif next entry up is not a free one - fail

	MOV	AX,[SI].LHLEN
	SUB	AX,DX		;subtract amount needed from amount free
JB_REALC_FALSE_Exit:		; rel jmp out of range made this necessary
	JB	REALC_FALSE_Exit ;brif insufficient amount for realloc request

	PUSH	DI		;save hdr ptr to entry to realloc
	CALL	B$LH_SPLIT	   ;split entry
	JNC	REALC_CONT1	;brif split succeeded
	MOV	DI,SI		;will grab all of existing entry

REALC_CONT1:
	;now combine entry-to-realloc and free entry pointed to by DI
	POP	SI		;ptr to hdr for entry to realloc
	MOV	AX,[DI].LHLEN	;length of free header being added

	DbAssertRelB  <[SI].LHTYPE>,ne,<LOW LH_FILE>,NH_TEXT,<can't realloc fdb's>
				;assuming here that realloc header size is STD
	SUB	SI,(LH_STD_HDR_LEN - 1)
	SUB	DI,(LH_STD_HDR_LEN - 1)
	MOV	CX,LH_STD_HDR_LEN
	REP	MOVSB		;copy existing header to new (top) location
	DEC	DI
	ADD	AX,[DI].LHLEN	;add in size of original entry
	MOV	[DI].LHLEN,AX	;save new entry size
	SUB	DI,AX
	MOV	[DI+1],AX	;save it as the back-length too
	MOV	SI,DI
	ADD	SI,3		;make SI = data ptr for realloc'd entry (retval)

REALC_TRUE_Exit:
	MOV	AL,1		;return non-zero in AX for successful realloc
REALC_Exit:
	POP	DI
	RET

REALC_FALSE_Exit:
	XOR	AX,AX
	JMP	SHORT REALC_Exit

B$LHREALC	ENDP


;***
; B$LHChgBakPtr
; Purpose:
;	When the ownership of an LH entry is changed or an owner moves, this
;	routine is called to change the back pointer.
; Inputs:
;	SI = ptr to data of entry whose owner is being changed.
;	CX = new value for the backpointer (i.e., a pointer to the new owner)
; Outputs:
;	none. ON exit, the back pointer is modified to point to the new owner.
;	Note, however, that the new owner contents are not changed to point
;	to this entry; the caller must do that.
; Modifies:
;	SI only.
; Exceptions:
;	B$ERR_ssc if heap entry is inconsistent.
;****
B$LHChgBakPtr	PROC	NEAR
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer
	MOV	[SI].LHBAKP,CX	;change back pointer
	RET
B$LHChgBakPtr	ENDP

;***
; B$ILHADJ - adjust the backpointer for a given heap entry
; Purpose:
;	This routine provides a mechanism by which the interpreter call-back
;	routine B$IHeapEntryMoved can update the back pointer for a single
;	heap entry.
;
; Inputs:
;	AX = pointer to start of data for a Local Heap entry.
;	DI = adjustment factor (same as B$LHADJ passes to B$IHeapEntryMoved)
; Outputs:
;	none.
; Modifies:
;	none.
; Exceptions:
;	none.
;****
B$ILHADJ	PROC	NEAR
	PUSH	SI
	MOV	SI,AX
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer
	ADD	[SI].LHBAKP,DI
	POP	SI
	RET
B$ILHADJ	ENDP


;***
;B$IAdUpd - Adjust string array entry backptr when ad moves
;
;Purpose:
;	Added with revision [18].
;	QB calls this routine to adjust a string array entry backptr
;	when the variable table owning the array moves.  The
;	variable table is allocated in the local heap.
;Entry:
;	pAdStr	- pointer to string array descriptor that is moving
;	Delta	- distance that descriptor is moving
;Exit:
;	Array entry backpointer is adjusted
;Uses:
;	AX
;Exceptions:
;	None.
;****
labelFP <PUBLIC,B_IAdUpd>		;Interpeter Reachable Label
cProc	B$IAdUpd,<PUBLIC,FAR>,<SI>
parmW	pAdStr
parmW	Delta
cBegin
	MOV	SI,pAdStr	;get AD ptr

	CMP	[SI].AD_fhd.FHD_hData,0 ; is the array allocated?
	JZ	AdUpd_Exit		; brif not

	MOV	SI,[SI].AD_fhd.FHD_oData ;get ptr to array data
	CALL	B$LH_PTR_FROM_DATA ;[SI] = ptr to heap header
DbAssertRel	<WORD PTR[SI].LHBAKP>,Z,pAdStr,NH_TEXT,<Invalid pAD passed to B$ISdUpd>
	MOV	AX,Delta	;get adjustment value
	ADD	[SI].LHBAKP,AX	;adjust backptr to reflect new location
AdUpd_Exit:			
cEnd


;***
; B$LH_SPLIT - split an entry into two pieces
; Purpose:
;	Given a header pointer to an entry and a size (must be a size rounded
;	to the current header size) for a new free entry, split the entry
;	into a free entry (of exactly the requested size) in high mem., with
;	the existing entry in low mem. - - - no heap movement takes place.
;
; Inputs:
;	SI = hdr ptr to an entry
;	AX = size of new entry to be split off
;	ES = DS
; Outputs:
;	SI is unchanged but is now the hdr ptr for the new free entry
;	DI is the new hdr ptr for the existing entry (lower in mem. than SI)
;	PSW.C is clear if successful; if input split-off size was zero, PSW.C
;		will be set and the block will be unmodified.
; Modifies:
;	DI only
; Exceptions:
;	none.
;****
B$LH_SPLIT	   PROC    NEAR
	PUSH	BX
	PUSH	CX
	MOV	DI,SI		;in case AX = 0 on entry
	CMP	AX,LH_STD_HDR_LEN
	JC	B$LH_SPLIT_EXIT   ;brif wish to split off less than a hdr's worth

	MOV	CX,LH_STD_HDR_LEN
	CMP	[SI].LHTYPE,LOW LH_FILE
	JNZ	B$LH_SPLIT_CONT1

	ADD	CX,(LH_FDB_HDR_LEN - LH_STD_HDR_LEN)
B$LH_SPLIT_CONT1:
	MOV	BX,[SI].LHLEN
	SUB	BX,AX
	CMP	BX,CX
	JC	B$LH_SPLIT_EXIT   ;brif can't take AX worth from entry and leave
				;  enough for existing header
	SUB	SI,CX
	INC	SI		;SI now points to start of header
	MOV	DI,SI
	SUB	DI,AX		;set DI to point to start of new header block
				;  i.e., new header for existing entry. SI
				;  points to start of old header block
	REP	MOVSB		;copy header
	DEC	SI		;SI = hdr pointer for new (free) entry

	MOV	[DI],AX 	;set back length for new free entry
	DEC	DI
	MOV	[SI].LHLEN,AX	;set length of new free entry
	MOV	[SI].LHTYPE,LOW LH_FREE
	MOV	CX,[DI].LHLEN
	SUB	CX,AX		;new length of existing entry
	MOV	[DI].LHLEN,CX	;update length of existing entry
	SUB	DI,CX
	MOV	[DI+1],CX	;set new back length for existing entry
	ADD	DI,CX		;set DI back as entry hdr ptr for return
	CLC			;signal successful return
B$LH_SPLIT_EXIT:
	POP	CX
	POP	BX
	RET
B$LH_SPLIT	   ENDP


;***
; B$TglHeapSptNEAR - Toggle near heap support code between near heap & var heap
;
; Purpose:
;	Added with revision [23].
; Entry:
;	For non-RELEASE use, b$fVarHeapActive is non-zero if the variable
;		heap is the currently active heap.
; Exit:
;	b$fVarHeapActive is updated.
; Uses:
;	ES set to DS on exit, otherwise None.
; Exceptions:
;	None
;****
cProc	B$TglHeapSptNEAR,<PUBLIC,NEAR>,<AX,CX,SI,DI>	
cBegin
	SET_ES_TO_DS			;movement code requires ES == DS
	MOV	CX,CW_SWAP_VARS
	MOV	SI,OFFSET DGROUP:b$HEAP_FIRST
	MOV	DI,OFFSET DGROUP:b$HEAP_FIRST_SWAP
TglHeap_Loop:
	;exchange [si] with [di], advancing si & di
	LODSW
	XCHG	AX,[DI]
	MOV	[SI-2],AX
	INC	DI
	INC	DI
	LOOP	TglHeap_Loop

	CMP	[b$fVarHeapActive],CX
	JNZ	Set_NR_Flag		;brif flag was true; set it false
	INC	CX			;set flag true - - var heap now active
	DbAssertRel b$HEAP_FIRST,b,b$HEAP_END_SWAP,NH_TEXT,<TglHeapSpt error>
Set_NR_Flag:
	MOV	[b$fVarHeapActive],CX	;set flag for assertion checking
cEnd

;***
; B$TglHeapSpt - Toggle near heap support code between near heap & var heap
;
; Purpose:
;	Added with revision [23].
;	This is just a PUBLIC FAR interface to a NEAR routine.
; Entry, Exit, Uses, Exceptions:
;	Same as for B$TglHeapSptNEAR, above.
;****
cProc	B$TglHeapSpt,<PUBLIC,FAR>
cBegin
	CALL	B$TglHeapSptNEAR	
cEnd



;***
; B$NHCPCT - compact all dynamic space
; Purpose:
;	Compacts all allocated strings to the bottom of string space.
;	Compacts all allocated heap entries to the top of the local
;	heap. All free heap space is given to the string space.
;
; Inputs:
;	None.
; Outputs:
;	None.
; Modifies:
;	None
; Exceptions:
;	B$ERR_SSC - nontrappable error if compaction finds corruption
;		  in string space structure.
;****
B$NHCPCT:
	ASSERT_NOT_VARHEAP NH_TEXT 
	CALL	B$STCPCT	;compact the string space
	CALL	B$LH_CPCT	   ;compact the local heap space
	CALL	B$STFromLH	;return free heap entry to string space
	RET			;return to caller

	SUBTTL	B$NHMOVALL - Move ALL of dgroup heaps around
	PAGE
;*** 
;B$NHMOVALL - Move ALL of dgroup heaps around
; Addedm revision [26]
;
;Purpose:
; Move everything in the dgroup above __atopsp up or down. This includes
; EVERYTHING above the stack. (Generally precipitated by the movement of the
; top of stack).
;
;Entry:
; [AX]		= Proposed delta to __atopsp. Move everything in the heap to
;		  fit just above this.
; Carry 	= Set if moving stack DOWN, else reset.
;
;Exit:
; Carry set on error (out of memory).
; [b$NH_First] Updated.
;
;Uses:
; Per convention.
;
;Preserves:
; AX
;
;Exceptions:
; Branches to B$ERR_OM for out of memory.
;
;******************************************************************************
cProc	B$NHMOVALL,<NEAR,PUBLIC>,AX
cBegin

	JC	NHMOVALL_DOWN	;Jump if we are moving __atopsp DOWN
	ASSERT_NOT_VARHEAP NH_TEXT ;Should be local heap at this time
;
; Moving up.
; 1) Move the near heap up by the change delta.
; 2) Move the var heap up to the new _atopsp.
; 3) Chop the var heap trailing free entry off by the move amount.
;
	PUSH	AX		;Save delta
	ADD	AX,[b$NH_First];[AX] = proposed phyiscal base of near heap
	JC	NHMOVALL_EXIT_POP ;Jump if bad error.
	MOV	CX,[b$NH_Last] ;[CX] = unchanged phyiscal end of near heap
	PUSH	AX		;Save prposed base
	cCall	B$NHMOV	;[AX] = resulting phyiscal base of near heap
	POP	AX		;[AX] = proposed phyiscal base of near heap
	JNC	NHMOVALL_VARUP	;Jump if not out of memory
	CALL	[b$pFHRaiseBottom];Ask Far Heap to move out of the way
	CALL   B$NHMOV 	;[AX] = resulting phyiscal base of near heap
NHMOVALL_VARUP:
	MOV	[b$NH_First],AX
	JC	NHMOVALL_EXIT_POP  ;If didn't work, go return right away
	CALL	B$TglHeapSptNEAR ; switch context to variable heap
	XCHG	AX,SI		;[SI] = phyiscal base of near heap
	DEC	SI		;[SI] = physical top of var heap
	CALL	LH_MOV		;Move the var heap up.  
				;SI = offset of new local heap start

	MOV	SI,[b$HEAP_END] ;[SI] = physical base of var heap
	POP	BX		;[BX] = distance changed
	ADD	SI,BX		;[SI] = proposed new physical base of var heap
	CALL	LHSetEnd	;Set the new physical base
	CALL	B$TglHeapSptNEAR ; switch context to back to local heap
	JMP	SHORT NHMOVALL_DONE

NHMOVALL_EXIT_POP:		;Error exit, with register pop
	POP	AX		;Discard TOS
	JMP	SHORT NHMOVALL_EXIT
;
; Moving Down
; 1) create a trailing free space entry in the var heap
; 2) Move the var heap down
; 3) Move the near heap down
;
NHMOVALL_DOWN:
	ASSERT_NOT_VARHEAP NH_TEXT ;Should be local heap at this time
	CALL	B$TglHeapSptNEAR  ; use var heap pointers
	ADD	AX,[b$HEAP_END] ;[AX] = proposed new physical heap end
	XCHG	AX,SI		;[SI] = proposed new physical heap end
	CALL	LHSetEnd	;Set the new end of var heap

	CALL	LH_MOV_DOWN	;Move the var heap down

	XCHG	AX,SI		;[AX] = var heap's new b$HEAP_FIRST
	CALL	B$TglHeapSptNEAR ; use local heap pointers

	INC	AX		;[AX] = new b$NH_First
	MOV	CX,[b$NH_Last]	;[CX] = unchanged last
	CALL	B$NHMOV		;Move near heap around
	MOV	[b$NH_First],AX

NHMOVALL_DONE:
	OR	AX,AX		;Successfull return
NHMOVALL_EXIT:

cEnd

	SUBTTL

;***
; B$NHMOV - move dynamic space (strings and local heap)
; Purpose:
;	Moves the contents of the string and local heaps to the beginning
;	and ending word offsets given.
;
; Inputs:
;	AX = offset to define start of dynamic space
;	CX = (if nonzero) offset to define end of dynamic space
;	     (if zero) offset is to be minimum possible
; Outputs:
;	AX = starting offset of dynamic space
;	CX = ending offset of dynamic space
;	CF = 0 - move successful - offsets reflect those requested
;	     1 - move unsuccesssful - request space too small
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$NHMOV  PROC	  NEAR		
	ASSERT_NOT_VARHEAP NH_TEXT 
	PUSH	SI		;save register...

;	If CX=0, the local heap is to be adjacent to the string heap
;	with no intervening free space.  Assume here that the local
;	heap will move down.

	JCXZ	MOVDYN_LH_DOWN	;heap will move down

;	Test that dynamic space start is before the end.  If not,
;	then report the error by returning with carry set.

	INC	CX		;heap header index is on odd byte of last word
	CMP	CX,AX		;test if start is before end
	JB	MOVDYN_DONE	;if not, then jump with carry set for error

;	Jump if local heap will move down.

	CMP	CX,[b$HEAP_FIRST] ;test if heap moves down
	JBE	MOVDYN_LH_DOWN	;brif so

;	Local heap moves up to offset in CX.  Then move the string heap
;	to offset in AX.

	MOV	SI,CX		;get offset to move local heap
	CALL	LH_MOV	;move the local heap up
	MOV	CX,SI		;update end offset of dynamic space (+1)
	MOV	SI,AX		;get offset to move string heap
	CALL	B$STMOV	;move the string heap (up or down)
	XCHG	AX,SI		; [AX] = offset of dynamic space
	JMP	SHORT MOVDYN_DONE_CLC ;jump to nonerror return

;	Local heap is to be moved down in memory.  Test if string heap
;	is moved down or up.

MOVDYN_LH_DOWN:
	CMP	AX,[b$STRING_FIRST] ;test if strings moving down
	JBE	MOVDYN_SS_DOWN	;if so, then jump

;	The string heap is moving up and the local heap is moving down.
;	First, compact the heap and give any free space to string heap.

	CALL	B$LH_CPCT	   ;compact local heap
	CALL	B$STFromLH	;give any local heap free space to string heap

;	Move string heap to requested location.

MOVDYN_SS_DOWN:
	MOV	SI,AX		;get request offset for string heap
	CALL	B$STMOV	;move the string heap
	XCHG	AX,SI		; [AX] = starting offset of dynamic heap
	JC	MOVDYN_DONE	;if error moving string heap, return with carry

;	If local heap need not be moved, then finished.

;MOVDYN_SS_SAME:
	CMP	CX,[b$HEAP_FIRST] ;test if local heap need be moved
	JE	MOVDYN_DONE	;if not, done, return with carry clear

;	Move local heap down be first making it adjacent to the string
;	heap.  If this was originally requested, then finished.

	XOR	SI,SI		;clear for minimum local heap offset
	CALL	LH_MOV	;move local heap next to string heap
	XCHG	CX,SI		;swap requested offset and return offset
	OR	SI,SI		;test if requested offset was zero
	JZ	MOVDYN_DONE	;if so, then return with carry clear

;	Move heap to final requested location.

	CMP	SI,CX		;compare requested location with returned one
	JBE	MOVDYN_DONE	;if less, ran out of room, return with carry
				;if equal, request finished, return w/o carry
	CALL	LH_MOV	;move the local heap to requested offset in SI
	MOV	CX,SI		;update end offset of dynamic space

;	Finished with no error - return carry cleared.

MOVDYN_DONE_CLC:
	CLC			;clear carry for no error

;	Finished - carry set or cleared appropriately.

MOVDYN_DONE:
	DEC	CX		;point to even byte of ending offset
	POP	SI		;restore register
	RET			;near return to caller
B$NHMOV  ENDP			

;***
; LH_MOV - move local heap
; Purpose:
;
; Moves local heap to the offset specified in SI if SI<>0.  IF SI=0, then move
; heap just under allocated string space. Local heap start [b$HEAP_FIRST] is set
; appropriately. The space is returned compacted.
;
; Inputs:
;	SI = offset for new local heap start, or 0 for reverse compaction.
; Outputs:
;	SI = offset of new local heap start.
; Modifies:
;	None.
; Exceptions:
;	B$ERR_SSC - nontrappable error if compaction finds corruption
;		  in local heap structure.
;****
LH_MOV	PROC	NEAR
	PUSH	AX		;save register
	OR	SI,SI		;test if moving heap to under string space
	JNZ	LH_MOV_UP	;if not, then jump to move heap up

;	SI=0, so perform the reverse compaction of the local heap just
;	under string space and return the new heap start offset in SI.

	CALL	B$LH_FROM_SS	;get any leading string entries to heap
	CALL	LH_MOV_DOWN	;perform reverse compaction to under strings
	JMP	SHORT LH_MOV_RETURN ;jump to set new heap start and return

;	SI nonzero, so prefix the heap with an unallocated entry from the
;	new start offset to the present start offset, then perform a
;	heap compaction.

LH_MOV_UP:
	MOV	AX,SI		;compute length of new entry to prefix...
	SUB	AX,[b$HEAP_FIRST] ;by difference between the two offsets
	CMP	AX,LH_STD_HDR_LEN
	JC	LH_MOV_NOMOVE	; If not moving by enough, don't add free
	MOV	[SI].LHTYPE,LOW LH_FREE ;set type of unallocated heap block
	MOV	[SI].LHLEN,AX	;set length of new heap block
	MOV	[b$HEAP_FIRST],SI ;set new entry as heap start
	SUB	SI,AX		;point to old heap start offset
	MOV	[SI+1],AX	;set new heap backlength
LH_MOV_NOMOVE:			
	CALL	B$LH_CPCT	   ;compact heap including new entry

LH_MOV_RETURN:
	MOV	SI,[b$HEAP_FIRST] ;report back start of heap
	POP	AX		;restore register
	RET			;return to caller
LH_MOV	ENDP

;***
; LH_MOV_DOWN - move local heap down in memory
; Purpose:
;	Compacts the local heap to low memory starting at the last
;	entry to the first.  Sets b$HEAP_FIRST to the new location of
;	the first allocated entry as no unallocated entry exists.
;	The heap will be just after the string space.
;
; Inputs:
;	None.
; Outputs:
;	[b$HEAP_FIRST] and [b$HEAP_FREE] of compacted heap.
;	[SI] - new value of b$HEAP_FIRST
; Modifies:
;	AX,SI
; Exceptions:
;	None.
;****

LH_MOV_DOWN:
	PUSH	CX		;save registers...
	PUSH	DI
	PUSH	DX

;	Scan local heap from [b$HEAP_END] to [b$HEAP_FIRST] by using the
;	entry backlengths to determine the next entry.

	MOV	SI,[b$HEAP_END] ;initialize pointer for scan
	MOV	[b$HEAP_FREE],SI ;free pointer will point to last heap entry

;	Skip over allocated entries as these are not moved.  If the last
;	entry to be scanned in [b$HEAP_FIRST], exit with no compaction.

LH_MOV_SKIP_LOOP:
	CMP	SI,[b$HEAP_FIRST]  ;test if end of scanning
	JE	LH_MOV_NO_COMPACT ;if so, then no compaction needed
	ADD	SI,[SI+1]	; [SI] = pointer to next entry header
	CMP	[SI].LHTYPE,LOW LH_FREE ;test if entry is allocated
	JNE	LH_MOV_SKIP_LOOP ;try for next allocated entry

	MOV	DX,[b$HEAP_FIRST] ;end of range to move
	CALL	LH_MOV_DN_RG	;given 1st unallocated entry, move all down

;	Finish reverse compaction by setting [b$HEAP_FIRST] to the last
;	entry in the compacted heap.

	MOV	[b$HEAP_FIRST],DI;put into pointer of heap start
LH_MOV_NO_COMPACT:
	MOV	SI,[b$HEAP_FIRST];return new pointer to new first entry
	POP	DX
	POP	DI		;restore registers...
	POP	CX
	RET			;return to caller

;***
; LH_MOV_DN_RG
; Purpose:
;	Compacts the local heap to low memory starting at a given unallocated
;	entry (bottom of range to be compacted) and ending with a given
;	allocated entry (the last entry (entry at highest memory of the
;	range) to be compacted).
;	Note that nothing is done to the free space which bubbles up to
;	the top of the range; no header is given to it.
;	NOTE: if the interpreter refuses permission for LH entries to move,
;		this routine will result in no movement (exit values will
;		be correctly set up for this case).
;
; Inputs:
;	SI = pointer to heap entry hdr for unallocated entry at bottom of
;		range to be compacted.
;	DX = pointer to heap entry hdr for allocated entry at top of range
;		to be compacted.
; Outputs:
;	DI = pointer to heap entry hdr for allocated entry at top of range
;		after compaction
;	DX = same as input value of DX, i.e., pointer to top of range allocated
;		heap entry hdr prior to compaction
; Modifies:
;	AX,CX,SI,DI
;
; Exceptions:
;	None.
;****
LH_MOV_DN_RG:
;	Set up pointer DI as the destination offset as the first byte of the
;	given unallocated entry.
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	MOV	DI,SI		;get pointer to the unallocated entry
	SUB	DI,[SI].LHLEN	;point to one less than entry backlength
	INC	DI		;entry backlength - compaction starts here

;	Loop for entries after the given unallocated entry.
;	Check for entry consistency and heap end.

LH_MOV_ENTRY_LOOP:
	CMP	SI,DX		;test if end of scanning
	JE	LH_MOV_FINISH	;if so, then jump to finish
	ADD	SI,[SI+1]	; [SI] = pointer to next entry header
	CALL	B$LH_PTR_CHECK	 ;check entry at [SI] for consistency
	CMP	[SI].LHTYPE,LOW LH_FREE ;test if entry is allocated
	JE	LH_MOV_ENTRY_LOOP ;if so, loop to check it, etc.
;
;	Allocated entry needs to be block transferred to ES:DI.
;	First adjust the entry, then move the entry itself.
;
	LEA	CX,[SI+1]	; get pointer one past entry to be moved
	SUB	CX,[SI].LHLEN	;point to backlength, ready for transfer

	MOV	AX,DI		;compute adj by destination backlength ptr...
	SUB	AX,CX		;less the source backlength pointer
	CALL	B$LHADJ	;adjust the entry backpointers

	MOV	SI,CX		;put source backlength pointer in reg
	MOV	CX,[SI] 	;backlength number of bytes
	SHR	CX,1		;convert to number of words to transfer
	REP	MOVSW		;perform the transfer - DI ready for next

; Now there is a void between two entries and the heap is not walkable.
; Make the void a free entry.
	NEG	AX			;convert back to positive byte count
	DEC	SI			; si = pHdr for new free entry
	MOV	[SI].LHLEN,AX		; stuff in length
	MOV	[SI].LHTYPE,LOW LH_FREE	; mark it as free
	MOV	[DI],AX 		; fill in backlength for new free entry
	NEG	AX			; back to negative adjustment factor

	JMP	SHORT LH_MOV_ENTRY_LOOP ;jump to process next entry

LH_MOV_FINISH:
	DEC	DI		;last byte is header offset

	;now set the free block up with a hdr & backlength in case this is req'd
	MOV	CX,DX
	SUB	CX,DI		;CX = size of new free block
	MOV	SI,DX
	MOV	[SI].LHLEN,CX
	MOV	[SI].LHTYPE,LOW LH_FREE
	MOV	[DI+1],CX	;backlength for free block
LH_MOV_RG_EXIT:
	POP	ES		
	RET



;***
; B$LH_I_ADJ
; Purpose:
;	Update the backpointers to any owners contained in an entry that's
;	about to be moved.
; Inputs:
;	SI = hdr ptr to entry about to be moved
;	AX = adjustment factor (to be added to backpointers to any owners
;		in this entry).
; Outputs:
;	none.
; Modifies:
;	BX, CX
;****
cProc	B$LH_I_ADJ,<PUBLIC,NEAR>
cBegin				
	MOV	CL,[SI].LHTYPE
	TEST	CL,LOW LH_IM_CALL_BACK
	JZ	LH_I_ADJ_DONE	;brif no owners in entry being moved

	TEST	CL,LH_IM_ENTRY
	JZ	LH_I_ADJ_DONE	;brif not an interpreter entry

	MOV	BX,[SI].LHBAKP	;put backpointer in BX
	CALL	B$IHeapEntryMoved ;call interpreter to do the work of
				;  updating backptrs to owners in this entry
				;  AX,BX,CL are parms
LH_I_ADJ_DONE:
cEnd				



;***
; B$LHForEachEntry
; Purpose:
;	Given a pointer to a (NEAR) function, walk the local heap, calling
;	This function for each entry, with the entry backpointer (pointer
;	to the entry owner) and the entry type byte.
;
;	On entry to routines called for each entry,
;		BX = pointer to entry owner,
;		DL = entry type byte
;	Users within this module can also use the fact the SI will be a pointer
;		to the header for the current entry.
;	Called functions must preserve all registers except BX and DX.
;	Callers of this function may pass other parameters to their called
;		function via AX and/or DI, which are not used by this routine.
;
;	NOTE: for entries which do not have backpointers, the called function
;		will receive (in BX) whatever is in the field corresponding
;		to the backpointer for that entry.
;
; Inputs:
;	CX = pointer to function to call for each entry.
;	ES = segment that the state vars are in
;	other parameters may be in AX and/or DI, to be passed to the function
;		whose pointer is given in CX. (BC only)
; Outputs:
;	none.
; Modifies:
;	none, unless called function modifies some register(s).
; Exceptions:
;	none (does not check entries for integrity).
;****
assumes	ES,DGROUP			
B$LHForEachEntry	PROC	NEAR
	PUSH	DX
	PUSH	BX
	PUSH	SI
	MOV	SI,[b$HEAP_FIRST]	
ForEach_Loop:
	CMP	SI,[b$HEAP_END] 	; done searching heap?
	JBE	ForEach_Exit		;  brif so

	MOV	DL,[SI].LHTYPE		;get type byte
	MOV	BX,[SI].LHBAKP		;get pointer to owner
	CALL	CX			;call function

	SUB	SI,[SI].LHLEN		;move down to next entry
	JMP	SHORT ForEach_Loop

ForEach_Exit:
	POP	SI
	POP	BX
	POP	DX
	RET
B$LHForEachEntry	ENDP
assumes	ES,NOTHING			

	SUBTTL	LHSetEnd - Set new HEAP_END
	PAGE
;*** 
;LHSetEnd - Set new HEAP_END
; Added, revision [26]
;
;Purpose:
; Truncate or expand the trailing free entry in the heap in order to set a new
; b$HEAP_END pointer.
;
;Entry:
; [SI]	= Proposed new b$HEAP_END
;
;Exit:
; Carry set on failure
;
;Uses:
; Per convention
;
;******************************************************************************
cProc	LHSetEnd,NEAR,DI
cBegin
	CALL	B$LHSetFree	;[DI] = trailing free heap entry
	CMP	SI,[b$HEAP_END] ;Determine which way the pointer is moving
	JC	LHSetEnd_DOWN	;Jump if we're expanding down

	CMP	DI,SI		;See if trying to move above the free entry
	JC	LHSetEnd_EXIT	;Jump if we are (carry set). Can't do that
	JZ	LHSetEnd_Empty	;Jump if we are not moving (carry not set).

LHSetEnd_DOWN:
	MOV	AX,DI		;[AX] = trailing free heap entry
	SUB	AX,SI		;[AX] = new size of free entry in AX
	MOV	[DI].LHTYPE,LOW LH_FREE ;set heap entry type
	MOV	[DI].LHLEN,AX	;set free heap entry length
	MOV	[SI+1],AX	;set free heap entry backlength

LHSetEnd_Empty:
	MOV	[SI].LHTYPE,LOW LH_END ;define the entry type
	MOV	[b$HEAP_END],SI ;define the heap end pointer
	OR	AX,AX		;Done, clear carry

LHSetEnd_EXIT:

cEnd

;***
; B$LHFLDDESCADD - add descriptor to field backpointer string.
; Purpose:
;	Add the descriptor value specified to the backpointer string
;	in the heap entry in SI.
;
; Inputs:
;	BX = address of descriptor to be added.
;	SI = FDB address of heap entry.
; Outputs:
;	None.
; Modifies:
;	SI.
; Exceptions:
;	None.
;****


B$LHFLDDESCADD:		
	ASSERT_NOT_VARHEAP NH_TEXT 
	PUSH	AX		;save registers...
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DI

;	Compute heap entry descriptor from the FDB address specified.

	CALL	B$LH_PTR_FROM_DATA ;heap entry pointer is now SI
	ADD	SI,LHPLEN	;move pointer to heap backpointer descriptor

;	Copy backpointer string to temp string two bytes longer.

	MOV	AX,BX		;save added descriptor value
	MOV	BX,SI		;get copy of heap descriptor pointer
	XOR	CX,CX		;copy from the string start
	MOV	DX,[BX] 	;get backpointer string length
	ADD	DX,2		;new string is to be two bytes longer
	CALL	B$STALCTMPSUB	;BX is ptr to desc of longer copy of string

;	Add fielded descriptor to string at the end.

	MOV	DI,DX		;get length of new string
	ADD	DI,[BX+2]	;point past end of the new string
	MOV	[DI-2],AX	;add descriptor to string at last two bytes

;	Deallocate old string data pointed by SI.

	XCHG	BX,SI		;BX=old string desc - SI=new string desc
	CALL	B$STDALC	;deallocate old string data (BX desc)

;	Copy new descriptor at SI to old descriptor at BX and then adjust
;	the new string backpointer to point to BX. (BX is in heap entry)

	ADD	WORD PTR [BX],2 ;new string is always two bytes longer
	MOV	DI,[SI+2]	;get new string data offset
	MOV	[BX+2],DI	;put into heap entry descriptor
	MOV	[DI-2],BX	;set new string backpointer

;	Free the temp descriptor at SI.

	MOV	BX,SI		;get temp descriptor pointer
	CALL	B$STDALCTMPDSC ;and free the descriptor

	POP	DI		;restore registers...
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET			;return to caller

;***
; B$LHLOCFDB - return file number from FDB pointer
; Purpose:
;	Return the file number of the heap entry containing the
;	FDB pointed by the specified value.
;
; Inputs:
;	SI = pointer to FDB
; Outputs:
;	BL = file number
; Modifies:
;	None.
; Exceptions:
;	B$ERR_ssc if heap entry is inconsistent.
;****

B$LHLOCFDB:			
	PUSH	SI		;save FDB pointer
	CALL	B$LH_PTR_FROM_DATA ;get heap entry pointer (check entry lengths)
	MOV	BL,[SI].LHFNUM	;get file number from heap entry
	POP	SI		;restore FDB pointer
	RET			;return

;***
;B$LHFDBLOC - locate FDB with a specified file number
;DBCS-callback
;
;Purpose:
;	Return a pointer to the FDB in the local heap FILE
;	entry that contains the specified file number.
;
;Inputs:
;	BL = file number
;
;Outputs:
;	SI = pointer to FDB (if found) or 0 (if not found).
;	ZF = clear (if found) or set (if not found)
;
;Modifies:
;	None.
;
;Exceptions:
;	None.
;****

B$LHFDBLOC:			
	MOV	SI,[b$HEAP_FIRST];start pointer at start of local heap
FDBLOC_LOOP:
	CMP	[SI].LHTYPE,LOW LH_END ;test if at heap end
	JE	FDBLOC_END	;if so, jump to note failure
	CMP	[SI].LHTYPE,LOW LH_FILE ;test if file entry
	JNE	FDBLOC_NEXT	;if not, then jump to try for next entry
	CMP	[SI].LHFNUM,BL	;test if correct file number
	JE	FDBLOC_FOUND	;jump to show success
FDBLOC_NEXT:
	SUB	SI,[SI].LHLEN	;point to next entry
	JMP	SHORT FDBLOC_LOOP ;and jump to try the next one
FDBLOC_FOUND:
	SUB	SI,[SI].LHLEN	;point to entry after one found
	ADD	SI,3		;point past backlength to FDB start
	RET			;return (with ZF clear)
FDBLOC_END:
	XOR	SI,SI		;clear SI to show failure
	RET			;return (with ZF set)

	SUBTTL	B$NHCLEAN - Clean string space and local heap
	PAGE
;***
;B$NHCLEAN - Clean string space and local heap
;
;Purpose:
; To clean the string space and local heap of entries whose descriptors are not
; in blank COMMON or other special areas. This routine is used during the
; CHAINing process.
;
;Entry:
; [DS]	= segment that heap is in
; [ES]	= segment that state vars are in
;	  Normally, this means ES=DS=DGROUP, execpt for DOS 5 chain, during
;	  which this routine is called with ES = DGROUP, DS = the shared memory
;	  selector.
;
;Exit:
; None.
;
;Modifies:
; NONE
;
;Preserves:
; ALL (for previous compatibility)
;
;******************************************************************************
assumes	ES,DGROUP		
assumes	DS,NOTHING		
cProc	B$NHCLEAN,<PUBLIC,NEAR>,<AX,BX,CX,DX>
cBegin
	CALL	B$LHClean	;the local heap must be cleaned first
	CALL	B$SSClean	;then the string space can be cleaned
cEnd

	SUBTTL	B$LHClean - Clean the local heap
	PAGE
;***
;B$LHClean - Clean the local heap
;
;Purpose:
; Scan the heap and process each entry accordingly:
;
; If CHAINing (b$Chaining <> 0):
;
;     FILE:	deallocate the fielded strings not in COMMON and update the
;		fielded string backpointer string.
;     ARRAY:	determine if the array descriptor is wholly within COMMON and
;		if not, delete the array using LH_ADJ.
;
; ELSE:
;
;     FILE:	deallocate all fielded strings
;     ARRAY:	delete the array
;
;Entry:
; [DS]	= segment that heap is in
; [ES]	= segment that state vars are in
;	  Normally, this means ES=DS=DGROUP, execpt for DOS 5 chain, during
;	  which this routine is called with ES = DGROUP, DS = the shared memory
;	  selector.
;
;Exit:
; None.
;
;Modifies:
; Per Convention.
;
;****
cProc	B$LHClean,<NEAR,PUBLIC>
cBegin
;
; If chaining, clear out all items from 0 to b$commonfirst
;
	XOR	AX,AX
	CMP	ES:b$Chaining,AL ; If we are not chaining, clear 0-ffff
	JZ	LHCleanAll

	MOV	BX,ES:[b$commonfirst]
	DEC	BX		;Range is inclusive
	XOR	CX,CX		; only release string arrays and fdb's
	CALL	B$LHClearRange
;
; Clear out all items from b$commonlast to 0FFFFH
;
	MOV	AX,ES:[b$commonlast]
LHCleanAll:
	MOV	BX,0FFFFH
	XOR	CX,CX		; only release string arrays and fdb's
cEnd	nogen			;Fall into B$LHClearRange

	SUBTTL	B$LHClearRange - Clean the local heap of entries
	PAGE
;***
;B$LHClearRange - Clean the local heap of entries
;
;Purpose:
;
; Scan the heap and process each entry accordingly:
;
;   FILE:	fielded strings whose descriptors fall into the passed range
;		deallocated, and the fielded string back pointer string is
;		updated.
;
;   ARRAY:	If the array descriptor falls into the range, delete the array
;		using LH_ADJ.
;
;Entry:
;	AX	= Start address of range in which to delete
;	BX	= End address of range in which to delete (Range is inclusive)
;	CX	= non-zero if we're to release ALL owners in given range
;		  zero if we should release only string arrays and fdb's
;	DS	= Segment containing the near heap
;	ES	= Segment containing heap state vars (& stack)
;
;		    Normally, this means ES=DS=DGROUP, execpt for DOS 5 chain,
;		    during which this routine is called with ES = DGROUP, DS =
;		    the shared memory selector.
;
;Exit:
;
;Modifies:
; Per Convention.
;
;Exceptions:
; None.
;
;******************************************************************************
cProc	B$LHClearRange,<NEAR,FORCEFRAME,PUBLIC>,DI
cBegin
	PUSH	CX		; [BP-2] = flag - release ALL owners if TRUE
	PUSH	BX		; [BP-4] = end offset
	PUSH	AX		; [BP-6] = start offset

	MOV	DI,SP		;[SS:DI] = pointer to start and end data
	MOV	CX,OFFSET LHCleanRangeEntry	;[CX] = routine to call
	CALL	B$LHForEachEntry		;Scan Local Heap

	POP	AX		;Clean stack
	POP	BX
	POP	CX		
cEnd
;*** 
; LHCleanRangeEntry - Clear local heap entry, if conditions are right
;
;Purpose:
;
; Process heap entry accordingly:
;
;   FILE:	fielded strings whose descriptors fall into the passed range
;		deallocated, and the fielded string back pointer string is
;		updated.
;
;   ARRAY:	If the array descriptor falls into the range, delete the array
;		using LH_ADJ.
;
;Entry:
;	DL	= Heap entry type
;	BX	= Offset of heap entry owner
;	SS:DI	= Pointer to range words:
;			[SS:DI]   = start
;			[SS:DI+2] = end
;			[SS:DI+4] = flag - only in EI_QB versions. 
;					zero means release only LH_FILE and
;					LH_ARRAY entries. non-zero means
;					release ALL entries. 
;	DS:SI	= Pointer to heap entry
;	DS	= Segment containing the near heap
;	ES	= Segment containing heap state vars (& stack)
;
;Exit:
; None
;
;Uses:
; Per Convention
;
;******************************************************************************
cProc	LHCleanRangeEntry,NEAR
cBegin
;
;	If file entry, test if any fielded strings and call FIELD
;	to remove any fielded strings from the backpointer string.
;
	CMP	DL,LH_FILE	;test if file entry
	JNE	LH_NOT_FILE	;if not, then jump

	CMP	WORD PTR [SI].LHPLEN,0 ;test if any fielded strings
	JE	LH_DONE 	;if not, then try for the next entry
	CALL	LHCleanRangeField ;clean out entries
	JMP	SHORT LH_DONE	;done - try for the next entry
;
;	If ARRAY entry, compute the offset of the LAST word of the array
;	descriptor, and base decisions on that.
;
LHCleanQBIEntry:		
	CMP	WORD PTR SS:[DI+4],0	
	JNE	LHClean_Dealc	
	JMP	SHORT LH_DONE	; brif we only want to free arrays & fdb's

LH_NOT_FILE:
	MOV	BX,[SI].LHBAKP	;get backpointer to owner
	CMP	DL,LH_ARRAY	;test if array entry
	JNE	LHCleanQBIEntry ; brif not array entry

;  QB allocates static arrays at scan time.  We can't blindly release
;  the array data, or we will get SSC.  So, we treat this like other
;  QBI heap entries. They get released at new, rude edit, etc.

	TEST	[BX].AD_fFeatures,FADF_STATIC ;QBI static string array?
	JNE	LHCleanQBIEntry ;brif so, treat like QB entry
	MOV	AL,[BX].AD_cDims;get the number of dimensions of array
	CBW			;make it a word value in AX
	SHL	AX,1		;now make in a word index in AX
	ADD	BX,AD_tDM-2	;point to word before first dimension entry
	ADD	BX,AX		;one word per dimension - so last in desc.
LHClean_Dealc:			
	CMP	BX,SS:[DI]	;before start?
	JB	LH_DONE 	;jump if so, don't touch
	CMP	BX,SS:[DI+2]	;after end?
	JA	LH_DONE 	;jump if so, don't touch

	XOR	AX,AX		;set flag for deletion
	CALL	B$LHADJ	;unallocate the heap entry
	MOV	[SI].LHTYPE,LOW LH_FREE ; set entry type to FREE, in case
					; this is not a string array entry
LH_DONE:
cEnd

;***
;LHCleanRangeField - clean out fielded strings
;
;Purpose:
; fielded strings whose descriptors fall into the passed range deallocated, and
; the fielded string back pointer string is updated. Ignores strings whose
; descriptors are in the local heap, as they are part of dynamic strign arrays,
; dealt with elsewhere.
;
;Entry:
;	SS:DI	= Pointer to range words:
;			[SS:DI]   = start
;			[SS:DI+2] = end
;	DS:SI	= Pointer to file heap entry
;	DS	= Segment containing the near heap
;	ES	= Segment containing heap state vars (& stack)
;
;Exit:
; None.
;
;Modifies:
; Per Convention
;
;******************************************************************************
cProc	LHCleanRangeField,NEAR,
cBegin
;
;	Compute pointers to starting and ending word offsets of the
;	fielded string backpointer string.
;
	PUSH	SI		;Save heap entry pointer
	MOV	BX,[SI].LHPLEN	;[BX] = length of backpointer string
	MOV	SI,[SI].LHPOFF	;[SI] = pointer to start of backpointer string
	LEA	BX,[BX+SI-2]	;[BX] = pointer to end of backpointer string
;
;	Loop here for each backpointer string word element.
;
FIELD_LOOP:

	LODSW			;[AX] = next string element to process

	CMP	AX,ES:[b$HEAP_END] ;String in near heap?
	JA	FIELD_NOTIN	;jump if so, don't touch
	CMP	AX,SS:[DI]	;before start?
	JB	FIELD_NOTIN	;jump if so, don't touch
	CMP	AX,SS:[DI+2]	;after end?
	JA	FIELD_NOTIN	;jump if so, don't touch
;
; Delete the element but keep the backpointer string at the same offset (as
; well as the same backpointer). This is done by moving the last element of the
; string (at [BX]) over the deleted element (at [SI]), setting the entry at
; [BX] to 1 (a null string entry), and finally moving BX forward to point to
; the new last element. Note that the setting of [BX] occurs before setting
; [SI] so the case of a one-element string (BX=SI) works.
;
	DEC	SI
	DEC	SI		;[SI] = ptr back to processed element
	MOV	AX,[BX] 	;[AX] = last element value; last elem = null
	MOV	[SI],AX 	;overwrite deleted element with last one
	MOV	WORD PTR [BX],1
	DEC	BX
	DEC	BX		;backup over null string header
;
;	Test if last element to process.  If not, then loop for next.
;
FIELD_NOTIN:
	CMP	SI,BX		;test if last element to process
	JBE	FIELD_LOOP	;if not, then loop
;
;	Compute new string length for descriptor.
;
	XCHG	AX,SI		;[AX] = address of last element
	POP	SI		;[SI] = pointer to heap entry
	SUB	AX,[SI].LHPOFF	;get new length of backpointer string
	MOV	[SI].LHPLEN,AX	;put new length in descriptor
	JNZ	FIELD_NOT_EMPTY ;if new string not empty, then jump
;
;	Backpointer string empty, deallocate string header and clear offset.
;
	MOV	WORD PTR [BX],1 ;clear string header
	MOV	[SI].LHPOFF,AX	;clear the heap descriptor offset

FIELD_NOT_EMPTY:
cEnd

sEnd	NH_TEXT

	END
