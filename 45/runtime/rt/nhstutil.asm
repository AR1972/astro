	TITLE	NHSTUTIL - String space utilities
;***
; NHSTUTIL - String space utilities
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

		USESEG	_DATA
		USESEG	_BSS
		USESEG	NH_TEXT
		USESEG	COMMON	


	INCLUDE seg.inc
	INCLUDE idmac.inc
	INCLUDE nhutil.inc	


ASSERT_NOT_VARHEAP	MACRO	SEG	;
	ENDM				;

sBegin	_DATA

	PUBLIC	b$nuldes	

NULSTR		DW	DGROUP:b$nuldes  ;Backpointer to descriptor
b$nuldes	DW	0,DGROUP:NULSTR+2 ;Descriptor for zero-length string

	NUMTEMP = 20D		;number of string temps
	LENTEMP = 6		;descriptor length is six bytes

TMPL		DW	NUMTEMP*(LENTEMP/2) DUP(-1) ;temp string descriptors
TMPH		LABEL	WORD

	externW	b$NH_first	; defined in nhinit.asm
sEnd	_DATA

sBegin	_BSS

	PUBLIC	b$STRING_FIRST
b$STRING_FIRST	DW	1 DUP(?)	;String space FWA (even)
	PUBLIC	b$STRING_FREE
b$STRING_FREE	DW	1 DUP(?)	;Scan for free string space from here
	PUBLIC	b$STRING_END
b$STRING_END	DW	1 DUP(?)	;String space LWA (odd)

	EXTRN	b$HEAP_FREE:WORD ;defined in LHUTIL.ASM
	EXTRN	b$HEAP_END:WORD	;defined in LHUTIL.ASM

	EXTRN	b$STRTAB:WORD		
	EXTRN	b$STRTAB_END:WORD	

	EXTRN	b$HEAP_FIRST_SWAP:WORD	;defined in LHUTIL.ASM
	EXTRN	b$HEAP_END_SWAP:WORD	;defined in LHUTIL.ASM

TMPHDR		DW	1 DUP(?)

	externW b$curlevel		;current program level

	externW b$fVarHeapActive	; non-0 when variable heap is

	externB b$Chaining	; in chain flag
	externW b$commonfirst	
	externW b$commonlast	


sEnd	_BSS

PAGE
sBegin	NH_TEXT

	ASSUMES CS,NH_TEXT

	PUBLIC	B$STINIT	;initialize string space
	PUBLIC	B$STCHKTMP	;check if temp string
	PUBLIC	B$STALCTMP	;allocate temp string
	PUBLIC	B$STALCTMPCPY	;allocate temp string copy
	PUBLIC	B$STALCTMPSUB	;allocate temp substring copy
	PUBLIC	B$STALCMAXTMP	;allocate maximum length temp string
	PUBLIC	B$STALCMAXTMPNC ; allocate max len temp str w/o compacting
	PUBLIC	B$STDALCTMP	;deallocate if temp string
	PUBLIC	B$STDALCTMPDSC ;deallocate temporary string desc
	PUBLIC	B$STDALCALLTMP ;deallocate all temp strings
	PUBLIC	B$STADJ	;deallocate/adjust string

	PUBLIC	B$STALC	;allocate string data
	PUBLIC	B$STDALC	;deallocate string
	PUBLIC	B$STMOV	;move string space start

	PUBLIC	B$STCPCT	;compact string space

	PUBLIC	B$STGETFRESIZ	;get size of free string in string space
	PUBLIC	B$STSWAPDESC	;swap the given string descriptors
	PUBLIC	B$STGETSTRLEN	;get string length

	PUBLIC	B$STFromLH
	PUBLIC	B$STSetFree

	PUBLIC	B$STPUTZ

PAGE
externNP B$ERR_SSC		

	externNP B$LHFLDDESCADJ 
	EXTRN	B$LHSetFree:NEAR
	EXTRN	B$ERR_OS:NEAR	
	EXTRN	B$ERR_ST:NEAR	

externNP B$TglHeapSptNEAR	; switch context to local heap

;*** 
;B$ISTALCTMPSUB - Allocate temporary substring copy
;
;Purpose:
;	FAR entry point for Interpreter added with revison [42].
;Entry:
;	Same as B$STALCTMPSUB
;Exit:
;	Same as B$STALCTMPSUB
;Uses:
;	Same as B$STALCTMPSUB
;Preserves:
;	Same as B$STALCTMPSUB
;Exceptions:
;	Same as B$STALCTMPSUB
;******************************************************************************
cProc	B$ISTALCTMPSUB,<FAR,PUBLIC>
cBegin
	call	B$STALCTMPSUB		; do a near call to do the work
cEnd

;*** 
;B$ISTDALCTMP - Deallocate if temporary string
;
;Purpose:
;	FAR entry point for Interpreter added with revison [42].
;Entry:
;	Same as B$STDALCTMP
;Exit:
;	Same as B$STDALCTMP
;Uses:
;	Same as B$STDALCTMP
;Preserves:
;	Same as B$STDALCTMP
;Exceptions:
;	Same as B$STDALCTMP
;******************************************************************************
cProc	B$ISTDALCTMP,<FAR,PUBLIC>
cBegin
	call	B$STDALCTMP		; do a near call to do the work
cEnd


PAGE
;***
; B$STALC - allocate string
; Purpose:
;	Find and allocate an appropriate chunk of string space.
;	The requested length will be rounded up if odd because
;	allocation is by 16-bit words only.  An extra word is
;	added to the front as required by the string format for
;	the backpointer.  This backpointer is NOT set up but it
;	must be before any scan of string space is made (by
;	B$STALC, B$STCPCT, etc.).
;
;	Each successive step is tried until the allocation is done:
;
;	1. Test the free string entry.	This entry, pointed by [b$STRING_FREE],
;	   can usually allocate the desired storage unless the space is
;	   nearly filled or extremely fragamented.  It usually follows
;	   last allocated entry or is the unallocated storage after a
;	   string space compaction.
;
;	2. String space is searched for the first unallocated space large
;	   enough to satisfy the requested length.  Adjacent unallocated
;	   entries are concatenated as they are found before any
;	   allocation is attempted.  The string space segment from the
;	   free pointer [b$STRING_FREE] to the end pointer [b$STRING_END] is
;	   scanned first. The segment from the start pointer [b$STRING_FIRST]
;	   to the free pointer is then scanned if allocation did not occur.
;
;	3. If the local heap free entry is unallocated and last in the
;	   heap space, the heap-string boundary is moved to reclaim the
;	   entry into string space.  The space now becomes part of the
;	   free string which is then tested for allocation.
;
;	4. The string space is compacted, with all allocated strings
;	   adjacent in lower memory and the remaining space made into
;	   the free string.  The free string is then tested.
;
;	5. The local heap is compacted with all allocated entries adjacent
;	   in upper dgroup, and the remaining free space given to string
;	   space.  The free string is then tested.
;
;	6. The Far Heap is asked to move its bottom....in case it had
;	   previously robbed space from DS. We then grab any additional space
;	   from near heap, and try the resulting free string.
;
;	7. If QBI version, the user library data images are released, the
;	   far heap is moved up in memory, the local heap is moved up, and
;	   and any resulting free space is returned to string space.
;
;	If no allocation can be done, the program is aborted by
;	   the nontrappable  "Out of Memory" error.
;
; Inputs:
;	BX = Length of string space required (may be odd).
; Outputs:
;	BX = Address of string data.  BX-2 is the header location.
; Modifies:
;	None.
; Exceptions:
;	None.
;****
B$STALC:

DbAssertRel	[b$STRING_FREE],BE,[b$STRING_END],NH_TEXT,<B$STALC: b$STRING_FREE past b$STRING_END> 
DbAssertRel	[b$STRING_FREE],AE,[b$STRING_FIRST],NH_TEXT,<B$STALC: b$STRING_FREE prior to b$STRING_FIRST> 

	PUSH	AX		;save the registers used...
	PUSH	SI

;	Round up BX to an even value and then add one.	This value
;	would be the unallocated string header value that could just
;	accomodate the allocation.

	INC	BX		;add one and...
	JZ	OutOfSS		; brif overflow -- out of string space
	OR	BL,1		;set bit for header value

;	Step 1 - Test if free string can perform allocation.

	CALL	SS_ALC_FREE	;try to allocate from free string
	JNC	ALCSTR_FREE	;if successful, then jump to return

	PUSH	CX		;push more registers...
	PUSH	DX
	PUSH	DI

;	Step 2 - Scan [b$STRING_FREE] to [b$STRING_END],
;		 then [b$STRING_FIRST] to [b$STRING_FREE].

	CALL	SS_SCAN 	;scan around the free pointer
	JNC	ALCSTR_DONE	;jump if allocation successful

;	Step 3 - Combine adjoining heap entries, test free string.

	CALL	B$STFromLH	;combine trailing entries into free string
	CALL	SS_ALC_FREE	;test if free string can handle allocation
	JNC	ALCSTR_DONE	;jump if allocation successful

;	Step 4 - Perform string space compaction, test free string.

	CALL	B$STCPCT	;perform the compaction
	CALL	SS_ALC_FREE	;test free string for room
	JNC	ALCSTR_DONE	;jump if allocation successful

;	No allocation possible, jump to error routine.

OutOfSS:			
	JMP	B$ERR_OS	; give "Out of string space" error

;	Allocation successful - clean stack and return.

ALCSTR_DONE:
	POP	DI		;restore registers used...
	POP	DX
	POP	CX

ALCSTR_FREE:
	POP	SI
	POP	AX

DbAssertRel	BX,BE,[b$STRING_END],NH_TEXT,<B$STALC: String alloc past b$STRING_END> 
DbAssertRel	BX,AE,[b$STRING_FIRST],NH_TEXT,<B$STALC: String alloc prior to b$STRING_FIRST> 

	RET			;return with BX pointing to new data
PAGE
;***
; SS_ALC_FREE - test if free string can allocate the request length
;
; Inputs:
;	BX - length of string data to allocate.
;	[b$STRING_FREE] - pointer to string to test
;
; Outputs:
;	CF=0 - allocation was successful.
;	     BX - pointer to data in new string.
;	     [b$STRING_FREE] - updated to new free string.
;	CF=1 - allocation failed.
;****

SS_ALC_FREE:
	MOV	SI,[b$STRING_FREE] ;point to the free string
	MOV	AX,[SI] 	;get free string header
	TEST	AL,1		;test if free string allocated
	JZ	SS_ALC_NO_ALLOC ;if so, cannot allocate from it
	CMP	AX,0FFFFH	;test if free string at string space end
	JZ	SS_ALC_NO_ALLOC ;if so, cannot allocate from it
	CMP	BX,AX		;test requested header against free header
	JA	SS_ALC_NO_ALLOC ;if requested too large, cannot allocate

;	SS_ALC - allocate string from string space entry pointed by SI.
;		 BX - header of requested string allocation
;		 AX - header of current entry pointed by SI
;		 ZF - set if BX=AX else cleared

SS_ALC:
	JE	SS_ALC_EXACT	;jump if exact allocation to be done

;	Split the current string into the requested allocation in lower
;	addressed portion and the new free string in the higher portion.

	MOV	[SI],BX 	;move in new allocated string header
	INC	BX		;length of allocated string entry (hdr+data)
	SUB	AX,BX		;subtract to get new free header value
	XCHG	BX,SI		;BX=alloc string entry ptr - SI=alloc length
	ADD	SI,BX		;pointer to new free string entry
	MOV	[SI],AX 	;move in new free string header value
	MOV	[b$STRING_FREE],SI ;update free string pointer
	ADD	BX,2		;pointer to allocated string data (carry clear)
	RET			;return to caller in B$STALC

;	Exact allocation just updates the free string pointer [b$STRING_FREE]
;	and points to the data location of the current string entry.

SS_ALC_EXACT:
	STC			;set carry for add to follow
	ADC	[b$STRING_FREE],AX ;add current entry length (hdr+data)
	MOV	BX,SI		;get current entry pointer
	ADD	BX,2		;and point to the data part (carry clear)
	RET			;return to caller in B$STALC

;	If failure, then set carry for return

SS_ALC_NO_ALLOC:
	STC			;carry set for failure
	RET			;return to caller in B$STALC
PAGE
;***
; SS_SCAN - scan string space for allocation
; Purpose:
;	Scan string space for a first-fit allocation of the specified
;	amount of space.  The scan starts from the free string to
;	string space end.  If unsuccessful, the scan continues from
;	string space start to the free string.
;
; Inputs:
;	BX = amount of space to allocate
; Outputs:
;	CF=0 - allocation successful
;	       BX = pointer to string data
;	       [b$STRING_FREE] = points to entry past one allocated
;	CF=1 - allocation failed
;	       [b$STRING_FREE] = points past last allocated entry
;****

SS_SCAN:

;	Scan from free string to end of string space.

	MOV	SI,[b$STRING_FREE] ;start of search
	MOV	DX,[b$STRING_END]	;end of search
	CALL	SS_SCAN_START	;start the scan...
	JNC	SS_SCAN_RETURN	;jump if allocation successful

;	Scan from start of string space to free string.
;	DX contains the pointer past the last allocated entry

	MOV	SI,[b$STRING_FIRST] ;start of search
	XCHG	DX,[b$STRING_FREE] ;swap last entry and end of search
	CALL	SS_SCAN_START	;start the scan...
	JNC	SS_SCAN_RETURN	;jump if allocation successful

;	Determine the last allocated entry in string space and
;	set the free string pointer just past it.  This will be needed
;	for the next allocation step of combining the free heap entry.

	CMP	DX,[b$STRING_FREE] ;test if the pointer should be changed
	JA	SS_SCAN_FREE	;if not, then jump
	MOV	[b$STRING_FREE],DX ;update the pointer
SS_SCAN_FREE:
	STC			;note allocation failure
SS_SCAN_RETURN:
	RET			;and return to B$STALC
PAGE
;***
; SS_SCAN_START - scan string for allocation of length requested.
;
; Inputs:
;	BX - length of string data to allocate.
;	SI - pointer to start of search.
;	DX - pointer to end of search.
; Outputs:
;	CF=0 - search was successful.
;	       BX - pointer to data in new string.
;	       [b$STRING_FREE] - updated to new free string.
;	CF=1 - search failed.
;	       DX - pointer past last allocated entry
;		    (either string space end or the last entry
;		     when it is unallocated)
;****

;	Test if string at scan pointer (SI) is allocated.
;	If so, get length of string header and data in AX.
;	Jump if string space end encountered (0FFFFH entry).

SS_SCAN_START:
	MOV	AX,[SI] 	;get scan ptr string header
	TEST	AL,1		;test if string is allocated
	JZ	SS_SCAN_SKIP	;if allocated, then jump
	INC	AX		;get length of string header and data
	JZ	SS_SCAN_FAILED	;jump if string space end was read

;	Compute pointer to next entry in DI.

	MOV	DI,SI		;get copy of current entry pointer
	ADD	DI,AX		;add entry length (header and data) for next
	DEC	AX		;return header value (data+1)

;	Test next block (pointed by DI) for possible combination with
;	the current block (pointed by SI).
;	If both are unallocated, then combine and try again.

SS_SCAN_NEXT:
	MOV	CX,[DI] 	;get header of next string entry
	TEST	CL,1		;test if next entry is allocated
	JZ	SS_SCAN_TRY_ALLOC ;if so, then try allocation with current
	INC	CX		;get length of string header and data
	JZ	SS_SCAN_TRY_FINAL ;if string end, then try final allocation
	ADD	AX,CX		;add entry length to current header value
	ADD	DI,CX		;move the next block pointer along
	JMP	SHORT SS_SCAN_NEXT ;and continue the scanning

;	Check entry at SI (with header AX) for sufficient room.
;	If not enough, compute the pointer to the entry after the
;	allocated one that stopped the combining.

SS_SCAN_TRY_ALLOC:
	MOV	[SI],AX 	;update the header in the current entry
	CMP	AX,BX		;compare current header with needed one
	JAE	SS_SCAN_ALC	;if enough room, then finish allocation
	MOV	AX,CX		;move header (descriptor) of allocated entry
	MOV	SI,DI		;skip past allocated entry pointed by DI

;	Compute pointer after the allocated entry at SI whose descriptor
;	is pointed by AX.

SS_SCAN_SKIP:
	MOV	DI,AX		;move descriptor pointer for referencing
	ADD	SI,[DI] 	;add length of string to pointer
	ADD	SI,3		;add 2 for header and 1 for even roundup
	AND	SI,NOT 1	;finish roundup process
	CMP	SI,DX		;test new pointer to scan finish
	JBE	SS_SCAN_START	;if not finished, then try again

;	The scan has failed, set carry and return.
;	Set DX to string space end since last entry was allocated.

SS_SCAN_FAILED:
	MOV	DX,SI		;points to string space end
	STC			;set carry to show failure
	RET			;and return to caller

;	Try a final allocation.  If the allocation fails, then scan fails.

SS_SCAN_TRY_FINAL:
	MOV	[SI],AX 	;update current string entry
	CMP	AX,BX		;compare current header with needed one
	JNAE	SS_SCAN_BYPASS	;if not enough room, bypass long jump
SS_SCAN_ALC:
	MOV	[b$STRING_FREE],SI ;set scanned entry as free string
	JMP	SS_ALC		;if room, jump to finish allocation
SS_SCAN_BYPASS:
	MOV	DX,SI		;point to last unallocated entry
	RET			;return to caller with carry set
PAGE
;***
; B$STCPCT - compact string space
; Purpose:
;	Compacts all allocated strings to the bottom of string space.
;	The string descriptors referenced by the string header are
;	adjusted to reflect their movement.
;	The remaining unallocated space is made into the free string.
;
; Inputs:
;	None.
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	B$ERR_SSC - nontrappable error if compaction finds corruption
;		  in string space structure.
;****

B$STCPCT:
	PUSH	AX		;save registers used...
	PUSH	BX
	PUSH	CX
	PUSH	SI
	PUSH	DI
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS

	MOV	SI,[b$STRING_FIRST] ;pointer to string scan

;	Skip over leading allocated entries which can be ignored since
;	they will not be moved.

SS_CPCT_SKIP:
	MOV	BX,[SI] 	;get header of scanned string entry
	TEST	BL,1		;test if entry is allocated
	JNZ	SS_CPCT_FIRST_FREE ;if not allocated, skipping is over

;	Entry is allocated.  First test for backpointer consistency.

	MOV	AX,[BX+2]	;get descriptor pointer to string data
	SUB	AX,2		;adjust to point to string header
	CMP	AX,SI		;test for backpointer consistency
	JNE	SSC_ERR 	; if not, signal string space corruption

;	Compute from descriptor string length the entry length and
;	therefore, the next string entry.  Jump to process this new entry

	ADD	SI,[BX] 	;add string length from descriptor
	ADD	SI,3		;add 2 for header length and 1 for roundup
	AND	SI,NOT 1	;complete roundup to next word
	JMP	SHORT SS_CPCT_SKIP ;and try again for next entry

;	First unallocated string found.  Initialize compacted pointer DI.

SS_CPCT_FIRST_FREE:
	MOV	DI,SI		;SI=scan pointer - DI=compacted pointer

;	Unallocated entry encountered in scan.	First test if string space
;	end (0FFFFH) and if so, jump to finish up.  Otherwise advance the
;	scan pointer SI by the entry size.

SS_CPCT_NEXT_FREE:
	INC	BX		;entry size from header value (hdr+size)
	JZ	SS_CPCT_END	;if it was 0FFFFH, then jump for end
	ADD	SI,BX		;advance scan pointer to next entry

;	Process next entry at scan pointer SI.	If entry unallocated, jump
;	to process it above.

SS_CPCT_NEXT_STRING:
	MOV	BX,[SI] 	;get header of scanned entry
	TEST	BL,1		;test if entry is allocated
	JNZ	SS_CPCT_NEXT_FREE ;if not, then jump to advance scan pointer

;	Entry is allocated.  First test for backpointer consistency.

	MOV	AX,[BX+2]	;get string data offset from descriptor
	SUB	AX,2		;adjust to point to string entry
	CMP	AX,SI		;test for consistency
	JNE	SSC_ERR 	; jump to process corruption error

;	Allocated entry at SI must be moved down to compacted space at DI.
;	First adjust descriptor pointer from SI+2 to DI+2.

	MOV	AX,DI		;get compacted space pointer
	ADD	AX,2		;point to new data portion of moved string
	MOV	[BX+2],AX	;and move it to the descriptor pointer

;	Move the string entry at SI to DI.

	MOV	CX,[BX] 	;get length of string to move
	ADD	CX,3		;add 2 for header and 1 for roundup
	SHR	CX,1		;length of entry in words
	REP	MOVSW		;move the string - SI and DI updated
	JMP	SHORT SS_CPCT_NEXT_STRING ;jump to process next entry

;	String space end encountered.  Put space between pointers SI and
;	DI into one unallocation string entry and make it the free string.

SS_CPCT_END:
	SUB	SI,DI		;get length of new free string entry at DI
	JE	SS_CPCT_NO_FREE ;if no space, then just update [b$STRING_FREE]
	DEC	SI		;new value of header (string data + 1)
	MOV	[DI],SI 	;move in new free string header
SS_CPCT_NO_FREE:
	MOV	[b$STRING_FREE],DI ;update new free string pointer

;	Finished with compaction - restore registers and return.

	POP	ES		
	POP	DI		;restore registers...
	POP	SI
	POP	CX
	POP	BX
	POP	AX
	RET			;return to caller

SSC_ERR:			
	JMP	B$ERR_SSC	;[23]
PAGE
;***
; B$STMOV - move string space
; Purpose:
;	Moves string space to the offset specified in AX.  String space
;	start [b$STRING_FIRST] is now set to AX and the free string entry
;	is adjusted since the string space end at [b$STRING_END] remains
;	as the same value.  The space is returned compacted.
;
; Inputs:
;	SI = offset for new string space start.
; Outputs:
;	CF = 0 - no error
;	     1 - error, not enough memory for move
; Modifies:
;	None.
; Exceptions:
;	B$ERR_SSC - nontrappable error if compaction finds corruption
;		  in string space structure.
;****

B$STMOV:
	PUSH	AX		;save registers...
	PUSH	SI


;	Determine if string space is to be expanded, contracted, or
;	neither.

	MOV	AX,[b$STRING_FIRST] ;get present start of string space
	CMP	SI,AX		;test new start with present start offset
	JE	MOVSTR_COMPACT	;if no difference, then just return
	JB	MOVSTR_DOWN	;if less, then expand space

;	Contract string space by compacting, moving allocated
;	strings up in memory, and adjusting the free string size.

	CALL	B$STCPCT	;compact the string space
	CALL	B$STFromLH	;get any leading heap entries
	CALL	SS_MOV_UP	;move allocated strings up to [SI]
				;this call sets carry if not enough memory
	JMP	SHORT MOVSTR_RETURN ;finished - jump to return

;	Expand string space by prefixing an unallocated string entry
;	located from the new offset to the old and then compacting.

MOVSTR_DOWN:
	SUB	AX,SI		;get size of string entry to create
	DEC	AX		;header is data length plus one
	MOV	[SI],AX 	;place header value in string entry
	MOV	[b$STRING_FIRST],SI ;new value of string space start
	MOV	[b$NH_first],SI ; start of string space is start of dynamic
				 ; space for QB4
MOVSTR_COMPACT:
	CALL	B$STCPCT	;compact strings with new entry
	CALL	B$STFromLH	;get any leading heap entries
	CLC			;clear carry for no error

MOVSTR_RETURN:
	POP	SI		;restore registers...
	POP	AX
	RET			;return...
PAGE
;***
; SS_MOV_UP - move allocated string entries up
; Purpose:
;	Assuming a compacted string space, move all allocated entries
;	starting at offset [SI].  This is done by updating the string
;	descriptor offsets to point to the new entry locations and
;	then moving the allocated strings up as a block.  The new free
;	string is then created as well as the new [b$STRING_FIRST] and
;	[b$STRING_FREE] pointers.
;
; Inputs:
;	AX = offset of present string space start.
;	SI = offset for new string space start.
; Outputs:
;	CF = 0 - no error
;	     1 - error, not enough memory for move
; Modifies:
;	AX, SI.
; Exceptions:
;	None.
;****

SS_MOV_UP:
	PUSH	BX		;save registers...
	PUSH	CX
	PUSH	DI
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	XCHG	AX,SI		;AX=new offset - SI=present offset
	SUB	AX,SI		;compute adjustment factor for move up

;	Test if enough room exists to move the strings down in memory.
;	The present free string will be reduced by the adjustment factor,
;	so a negative string length sets carry to denote the error.

	CALL	B$STGETFRESIZ	;get size of free string in BX
	CMP	BX,AX		;compare present free length with adj factor
	JB	SS_MOV_RETURN	;return immediately with carry set

;	Scan through the compacted string space using pointer [SI] and
;	adjust the string descriptor referenced by the backpointer by
;	adding AX to the offset word.

SS_MOV_SCAN_LOOP:
	MOV	BX,[SI] 	;get header of current scanned entry
	TEST	BL,1		;test if entry is allocated
	JNZ	SS_MOV_BLOCK	;if not allocated, then end of scan, jump
	ADD	[BX+2],AX	;adjust the descriptor offset up
	ADD	SI,[BX] 	;add string length to scan pointer
	ADD	SI,3		;add for header and round up to next word...
	AND	SI,NOT 1	;if string length was odd
	JMP	SHORT SS_MOV_SCAN_LOOP ;loop to try for next entry

;	Move all allocated strings together up AX bytes in memory.

SS_MOV_BLOCK:
	MOV	SI,[b$STRING_FREE] ;entry past last allocated one
	MOV	CX,SI		;get offset to compute block length...
	SUB	CX,[b$STRING_FIRST] ;by subtracting start of string space
	SHR	CX,1		;get number of words to move
	SUB	SI,2		;point to highest word to move
	MOV	DI,SI		;compute destination...
	ADD	DI,AX		;by adding amount to move up
	STD			;moving up requires direction flag set
	REP	MOVSW		;perform the move of CX words
	CLD			;restore direction flag

;	Adjust string space pointer and create new free entry.

	ADD	[b$STRING_FIRST],AX ;adjust new start of string space
	ADD	[b$NH_first],AX ; start of string space is start of dynamic
				 ; space for QB4
	MOV	BX,[b$STRING_FREE] ;get original free entry offset
	ADD	BX,AX		;compute new free entry offset
	MOV	[b$STRING_FREE],BX ;put in new free entry offset
	MOV	CX,[b$STRING_END]	;get unchanging end offset of string space
	SUB	CX,BX		;get size of new free entry
	DEC	CX		;header is data length + 1 (entry length - 1)
	MOV	[BX],CX 	;define new free entry by inserting header

SS_MOV_RETURN:
	POP	ES		
	POP	DI		;restore registers...
	POP	CX
	POP	BX
	RET			;return to caller
PAGE
;***
; B$STALCMAXTMP	- allocate maximum-sized temporary string
; B$STALCMAXTMPNC      - Allocate maximum-sized temp string w/o compact
; Purpose:
;	Allocate the maximum-sized string availiable and attach it to a temp
;	descriptor. Backpointer in string space set but data is not moved in.
;
; Inputs:
;	None.
; Outputs:
;	DX = Address of string data area.
;	BX = Address of temp string descriptor
; Modifies:
;	None.
; Exceptions:
;	None.
;****

cProc	B$STALCMAXTMP,<PUBLIC,NEAR>	
cBegin					
	CALL	B$STCPCT	;compact the string space

labelNP	<B$STALCMAXTMPNC>	; Alloc after compaction

	CALL	B$STFromLH	;append any leading local heap entries
	CALL	B$STGETFRESIZ	;get the resulting free string size in BX
	PUSH	[b$STRING_FREE]	; Save ptr to free size
	CALL	B$STALCTMP	;use the size in BX to allocate temp string
	POP	[b$STRING_FREE]	; Get back saved value
				; return to caller (PAINT)
cEnd				; End of B$STALCMAXTMP,B$STALCMAXTMPNC

PAGE
;***
;B$STALCTMP - allocate temporary string
;DBCS-callback
;
;Purpose:
;	Allocate the requested amount of free space and attach it to a temp
;	descriptor. Backpointer in string space set but data is not moved in.
;
;Inputs:
;	BX = Length of string
;
;Outputs:
;	DX = Address of string data area.
;	BX = Address of temp string descriptor
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, CX
;
;Exceptions:
;	None
;****

B$STALCTMP:
	OR	BX,BX
	JZ	ATSNUL		;Nul strings have special descriptor
	CMP	WORD PTR[TMPHDR],0 ;See if we have any temps left
	JZ	TEMPOV
	PUSH	SI
	PUSH	BX
	CALL	B$STALC	;[C1]Allocate the space

ALCTMP: 			;jumped to from MakeTemp
	MOV	SI,[TMPHDR]	;Get the next temp
	MOV	DX,[SI] 	;Make the one after that
	MOV	[TMPHDR],DX	;   the new "next temp"
	POP	[SI]		;Set length in descriptor
	MOV	[BX-2],SI	;Set backpointer
	MOV	[SI+2],BX	;Set pointer in descriptor
	MOV	DX,b$curlevel	;set program level of allocation
	MOV	[SI+4],DX	;  in descriptor
	MOV	DX,BX		;Put data pointer in DX
	MOV	BX,SI		;Need temp descriptor in BX
	POP	SI
	RET

;	Return pointer to null string.

ATSNUL:
	MOV	DX,OFFSET DGROUP:NULSTR
	MOV	BX,OFFSET DGROUP:b$nuldes 
	RET

TEMPOV:
	JMP	B$ERR_ST	; Clean stack and report string temporary ovfl.
PAGE
;***
;B$STMakeTemp - transfer ownership of a string to a temp. (QBI only).
;
;Purpose:
;	Added with revision [22].
;	Transfers ownership of a string to a temp.  This is used
;	for QBI string functions where the return sd lives on the
;	stack and will be lost at exit.  The string will be transferred
;	to a temp just before the sd is trashed.  It is guaranteed that
;	b$curlevel will be adjusted appropriately before this routine
;	is called. If the SD is 0 length, a ptr to b$nuldes will be returned.
;Entry:
;	psdOwner - pointer to sd currently owning string.
;Exit:
;	AX - pointer to temp sd that is new owner of string
;	     (b$nuldes if 0 length).
;Uses:
;	Per convention.
;Exceptions:
;	B$ERR_ST if no temps are left.
;****

cProc	B$STMakeTemp,<PUBLIC,FAR>
parmW	psdOwner
cBegin
	XOR	AX,AX		
	CMP	WORD PTR[TMPHDR],AX ;are there any temps left
	JZ	TEMPOV		;brif not - error out

	MOV	BX,[psdOwner]
	CMP	[BX],AX 	;is this a null (0-length) string?
	JNZ	NotNullstr	;brif not

	MOV	AX,OFFSET DGROUP:b$nuldes ;get ptr to b$NulDes
	JMP	SHORT BSTMakeTempX ;return it in ax

NotNullStr:			
	cCall	MakeTemp	;call near routine to do the work
	XCHG	AX,BX		;return new sd in AX
BSTMakeTempX:			
cEnd

cProc	MakeTemp,<NEAR>
cBegin	<nogen>
	PUSH	SI		;preserve SI
	PUSH	[BX]		;push size of string
	MOV	BX,[BX+2]	;[BX] = ptr to string data
	JMP	SHORT ALCTMP	;jump to common code to make the temp
				;the new owner
cEnd	<nogen>

PAGE
;***
; B$STCHKTMP - Check for temporary string descriptor
; Function:
;	Check to see if the string descriptor is a temporary.
; Inputs:
;	BX = Address of string descriptor
; Outputs:
;	Carry flag set if a temp
;	Carry clear in not
; Modifies:
;	None
; Exceptions:
;	None
;****

B$STCHKTMP:			;[C1]
	CMP	BX,OFFSET DGROUP:TMPH
	JNC	RETL		;If above high end, not a temp
	CMP	BX,OFFSET DGROUP:TMPL
	CMC
	RET
PAGE
;***
; B$STGETSTRLEN - get string length
; Purpose:
;	Computes the byte length of a zero-terminated ASCII string.
;
; Inputs:
;	BX = offset of string
; Outputs:
;	AX = Length of the line (not including 00)
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$STGETSTRLEN:
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	CX
	PUSH	DI
	MOV	CX,-1		;Scan up to 65535 characters
	MOV	AL,0		;Search for a zero
	MOV	DI,BX
	REPNE	SCASB		;Scan for a 00
	MOV	AX,CX		;Amount of the 65535 left
	NOT	AX
	DEC	AX
	POP	DI
	POP	CX
	POP	ES		
	RET
PAGE
;***
;B$STALCTMPCPY - Allocate a temp string copy.
;DBCS-callback
;
;Purpose:
;	Allocate a temporary string of the length specified.  Copy the
;	data from the string whose descriptor was passed to the new string.
;	Does not deallocate the original string.
;
;Inputs:
;	BX = Length of string to copy.
;	DX = Address of string data to copy
;
;Outputs:
;	BX = Address of temp string descriptor
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, CX
;
;Exceptions:
;	None
;****

B$STALCTMPCPY: 		;[C1]
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	CX
	MOV	CX,BX		;Get length in CX
	PUSH	SI
	MOV	SI,DX		;Point to source of string data
	CALL	B$STALCTMP	;[C1]Get a temporary string of the right length
	PUSH	DI
	MOV	DI,DX		;String data destination
	MOV	DX,SI		;Restore DX
	INC	CX		;Round up if odd
	SHR	CX,1		;Move by words
	REP	MOVSW
	POP	DI
	POP	SI
	POP	CX
	POP	ES		
	RET
PAGE
;***
; B$STDALCALLTMP - Deallocate all temporary strings
; Purpose:
;	Free all temporary strings and descriptors which were
;	allocated at or above the specified level.
;
; Inputs:
;	AX = program level at or above which temporary strings
;	     will be deallocated.  Use 0 (the minimum level) to
;	     deallocate ALL temporary strings.
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$STDALCALLTMP:			;[C1]
	PUSH	BX
	MOV	BX,OFFSET DGROUP:TMPL ;First string descriptor
DTSLOOP:
	CMP	WORD PTR[BX+4],AX ;string allocation level >= specified
	JL	NEXTMP		;skip if not
	CALL	DELTMP		;otherwise delete it
NEXTMP:
	ADD	BX,LENTEMP	;Next temp descriptor
	CMP	BX,OFFSET DGROUP:TMPH ;Done yet?
	JB	DTSLOOP
	POP	BX
RETL:
	RET
PAGE
;***
;B$STDALCTMP - Deallocate if temporary string
;DBCS-callback
;
;Purpose:
;	Check to see if the descriptor is a temporary one.
;	If so, deallocate its string data and release the descriptor
;	for future use.
;
;Inputs:
;	BX = Address of string descriptor
;
;Outputs:
;	None.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;****

B$STDALCTMP:			;[C1]
	CALL	B$STCHKTMP	;[C1]See if temp string
	JNC	RETL		 ;If not, leave it
DELTMP:
	CALL	B$STDALC	;[C1]Free the space

;***
; B$STDALCTMPDSC - deallocate temporary descriptor
; Purpose:
;	Deallocate the temporary string descriptor pointed by BX and
;	relink it in the temporary descriptor chain for subsequent
;	allocation.
;
; Inputs:
;	BX = address of temporary string descriptor
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$STDALCTMPDSC:

;	Make sure that we do not attempt to deallocate a temp twice

DbAssertRel	[BX+4],NZ,-1,NH_TEXT,<Attempted to dealloc an already dealloced string temp>

	PUSH	AX
	MOV	AX,BX
	XCHG	AX,[TMPHDR]	;Make this one the first free
	MOV	[BX],AX 	;Set link to former first one
;	MOV	WORD PTR[BX+2],-1 ;Make it easy to see its free
	MOV	WORD PTR[BX+4],-1 ; < min level (0) means free temp desc
	POP	AX
	RET
PAGE
;***
;B$STALCTMPSUB - Allocate temporary substring copy
;DBCS-callback
;
;Purpose:
;	Create a temporary string and copy the specified portion of the given
;	string into it. The given string is deleted if it is a temp.
;
;	The starting position in the string is specified as an offset from
;	the beginning of the string.  Thus 0 <= CX <= Len(BX)-DX
;
;Inputs:
;	BX = Address of string descriptor
;	CX = Starting position within string
;	DX = Length
;
;Outputs:
;	BX = Address of temp string descriptor
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, CX, DX
;
;Exceptions:
;	None.
;****

B$STALCTMPSUB: 			;[C1]
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	AX		;Save registers
	PUSH	CX
	PUSH	SI
	PUSH	DI

	MOV	SI,CX		;SI = offset into string
	MOV	AX,BX		;AX = source string descriptor
	MOV	CX,DX		;CX = length
	MOV	BX,DX		;BX = length
	CALL	B$STALCTMP	;[C1]Allocate temp string
	MOV	DI,DX		;DI = temp string contents address
	XCHG	AX,BX		;AX = temp string descriptor
				;BX = source string descriptor
	ADD	SI,[BX+2]	;add in old string contents address
	CALL	B$STDALCTMP	;[C1]delete old temp string
	XCHG	AX,BX		;BX = new string descriptor
	MOV	DX,CX		;get length back in DX to leave it unchanged
	INC	CX		;if odd, round up to even
	SHR	CX,1		;bytes to words
	REP	MOVSW		;move into temp string

	POP	DI
	POP	SI
	POP	CX
	POP	AX
	POP	ES		
	RET
PAGE
;***
; B$STDALC - Deallocate string
; Purpose:
;	Deallocate the string data pointed by the string descriptor.
;
; Inputs:
;	BX = Address of string descriptor.
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$STDALC:			;[C1]
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	AX		;save register
	XOR	AX,AX		;clear to set flag for deletion
	CALL	B$STADJ	;call routine for string data deallocation
	POP	AX		;restore register
	POP	ES		
	RET			;return to caller
PAGE
;***
; B$STADJ - deallocate/adjust string data
; Function:
;	Deallocates or adjusts the string data pointed by the
;	string descriptor in BX.  If AX=0, the data is deallocated.
;	If AX<>0, the string backpointer is adjusted by the value
;	of AX.
;
; Inputs:
;	AX = adjustment value
;	     AX=0  - deallocate string.
;	     AX<>0 - adjust string backpointer.
;	BX = Address of string descriptor.
;	DS = segment that heap is in
;	ES = segment that state vars are in
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;****
assumes	ES,DGROUP		
assumes	DS,NOTHING		
B$STADJ:
	CMP	WORD PTR [BX],0 ; is string allocated?
	JZ	ADJSTR_EXIT	; brif not

	PUSH	CX		;save register
	PUSH	SI
	MOV	SI,[BX+2]	;Get pointer to string data

;	Test if string is in string space - from [b$STRING_FIRST] to
;	[b$STRING_END].

	CMP	SI,ES:[b$STRING_FIRST] ;test if before string space
	JB	ADJSTR_RETURN	;if less, then constant, do not deallocate
	CMP	SI,ES:[b$STRING_END]	;test if after string space
	JA	ADJSTR_FIELDED	;if more, then fielded string, jump

;	String is in string space.  First adjust the backpointer by AX.
;	Then test if deallocation is requested and, if so, replace
;	backpointer with free string header with <data length>+1.

	ADD	[SI-2],AX	;adjust backpointer of string
	OR	AX,AX		;test if deallocation was requested
	JNZ	ADJSTR_RETURN	;if so, then finished, jump
	MOV	CX,[BX] 	;Get length of string to free
	INC	CX		;Round up if odd
	OR	CL,1		;And set free bit
	XCHG	CX,[SI-2]	;Get backpointer and set free length
	CMP	CX,BX		;Check backpointer
	JE	ADJSTR_RETURN	;if equal, then just return
	PUSH	ES		;force DS = ES incase of CHAIN
	POP	DS		;sets up DS = BASIC DS
	JMP	B$ERR_SSC	;jump to error routine

;	String is in the local heap, probably a fielded string.
;	Call routine to delete descriptor from the appropriate heap
;	entry backpointer string.

ADJSTR_FIELDED:
	CALL	B$LHFLDDESCADJ	;delete descriptor in BX for heap entry

ADJSTR_RETURN:
	POP	SI		;restore registers
	POP	CX
ADJSTR_EXIT:			
	RET			;return to caller
assumes	DS,DGROUP		
assumes	ES,NOTHING		
PAGE
;***
; B$STPUTZ - Put zero at end of string
;
; Inputs:
;	BX = Address of string descriptor
; Function:
;	Make into a string with a zero byte on the end. If length is presently
;	odd, just jam a zero into the extra byte. If even, create a temp
;	one byte longer and put the zero there.
; Outputs:
;	BX = Address of string descriptor
; Registers:
;	Only BX and F affected.
;****

B$STPUTZ:
	PUSH	DX
	MOV	DX,[BX]
	PUSH	CX
	XOR	CX,CX		;Offset = 0
	INC	DX		;Length = old length + 1
	CALL	B$STALCTMPSUB	;[C1]Always copy string into string space
	DEC	DX
	POP	CX
	PUSH	BX
	MOV	BX,[BX+2]	;Get pointer to data
	ADD	BX,DX
	MOV	BYTE PTR [BX],0
	POP	BX
	POP	DX
	RET
PAGE
;***
; B$STSetFree - set free string entry pointer
; Purpose:
;	Determines if the current free string is both the last string
;	entry and unallocated.	If so, its current value is returned
;	in SI.	Otherwise, the free string pointer b$STRING_FREE is set to
;	the string space end and its value returned in SI.
;
; Inputs:
;	None.
; Outputs:
;	SI = pointer to free string (b$STRING_FREE).
; Modifies:
;	AX.
; Exceptions:
;	None.
;****

B$STSetFree:
	MOV	SI,[b$STRING_FREE] ;get pointer to current free string
	MOV	AX,[SI] 	;get header of free string
	TEST	AL,1		;test if free string allocated
	JZ	SS_SET_END	;if so, then jump
	INC	AX		;get length of free entry
	JZ	SS_SET_RETURN	;jump if free entry was at end of string space
	ADD	AX,SI		;get pointer to entry after free string
	CMP	AX,[b$STRING_END] ;test if free was last string in space
	JE	SS_SET_RETURN	;if so, jump to leave pointer unchanged
SS_SET_END:
	MOV	SI,[b$STRING_END] ;get end pointer of string space
	MOV	[b$STRING_FREE],SI ;and set the free string to it
SS_SET_RETURN:
	RET			;return with SI set to new free string
PAGE
;***
; B$STFromLH - get string space from local heap space
; Purpose:
;	Determine if a free entry exists at the end of the
;	local heap.  If so, change the string and heap space
;	pointers so that it is now part of string space.
;
; Inputs:
;	None.
; Outputs:
;	[b$STRING_FREE] = pointer to new free string containing any space
;		   retrieved from the local heap.
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$STFromLH:
	PUSH	AX		;save registers...
	PUSH	SI
	PUSH	DI

	CALL	B$LHSetFree	;DI points to trailing heap free entry
	CMP	DI,[b$HEAP_END]	;test if any free space at all
	JE	SS_LH_RETURN	;if not, nothing to reclaim

;	The new END heap entry is now at [DI].	Setup the entry and
;	set the heap end pointer [b$HEAP_END].  If the heap free pointer
;	[b$HEAP_FREE] pointed within the reclaimed area, set it to
;	[b$HEAP_END] to give it a valid value.

	MOV	[DI].LHTYPE,LOW LH_END ;define the new END heap entry
	MOV	[b$HEAP_END],DI	;define heap end pointer
	CMP	DI,[b$HEAP_FREE] ;test if free pointer in reclaimed area
	JB	SS_LH_FREE	;if not, then do not set it
	MOV	[b$HEAP_FREE],DI ;otherwise, set it to the heap end
SS_LH_FREE:

;	The new end of string space is the word before the heap END
;	entry.

	SUB	DI,LH_STD_HDR_LEN+1 ;point to new string space end
	MOV	[DI],0FFFFH	;set string space end entry
	CALL	B$STSetFree	;get free string pointer in SI
	MOV	[b$STRING_END],DI ;define new end of string space

;	Form new free string entry from [SI] up to [DI].

	SUB	DI,SI		;get length of new entry
	DEC	DI		;make header for entry
	MOV	[SI],DI 	;put header into entry

SS_LH_RETURN:
	POP	DI		;restore registers...
	POP	SI
	POP	AX
	RET			;return with new free string
PAGE
;***
; B$STGETFRESIZ - get size of free string
; Purpose:
;	Return the current size of the free string in string space.
;
; Inputs:
;	None.
; Outputs:
;	BX = size of free string in bytes.
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$STGETFRESIZ:
	MOV	BX,[b$STRING_FREE] ;get location of free entry in string space
	MOV	BX,[BX] 	;get free string header
	TEST	BL,1		;test if string entry is allocated
	JZ	GETFRESIZ_ZERO	;if so, then return zero as size
	CMP	BX,0FFFFH	;test if at end of string space
	JNE	GETFRESIZ_NZERO ;if not, header is one more than size
GETFRESIZ_ZERO:
	MOV	BX,1		;size will be zero after decrementing
GETFRESIZ_NZERO:
	DEC	BX		;get the free string size
	RET			;return to caller
PAGE
;***
; B$STSWAPDESC - swap string descriptor contents
; Purpose:
;	Swap the contents of the two string descriptors and adjust the
;	string backpointers, if necessary.
;
; Inputs:
;	SI = pointer to first string descriptor
;	DI = pointer to second string descriptor
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$STSWAPDESC:
	PUSH	BX		;save register

	MOV	BX,[SI] 	;get length in first descriptor
	XCHG	BX,[DI] 	;exchange with length in second
	MOV	[SI],BX 	;put exchanged length back in first

	MOV	BX,[SI+2]	;get offset in first descriptor
	XCHG	BX,[DI+2]	;exchange with offset in second
	MOV	[SI+2],BX	;put exchanged offset back in first

	MOV	BX,[SI+2]	;get offset to first descriptor
	CMP	BX,[b$STRING_FIRST] ;test if before string space
	JB	SWAP_NO_FIX	;if so, then do not adjust
	CMP	BX,[b$STRING_END] ;test if after string space
	JAE	SWAP_NO_FIX	;if so, then do not adjust
	MOV	[BX-2],SI	;adjust the first backpointer
SWAP_NO_FIX:
	MOV	BX,[DI+2]	;get offset to second descriptor
	CMP	BX,[b$STRING_FIRST] ;test if before string space
	JB	SWAP_RETURN	;if so, then do not adjust
	CMP	BX,[b$STRING_END] ;test if after string space
	JAE	SWAP_RETURN	;if so, then do not adjust
	MOV	[BX-2],DI	;adjust the first backpointer
SWAP_RETURN:
	POP	BX		;restore register
	RET			;return to caller in SWAP.ASM
PAGE
;***
; B$STINIT - Initialize string space
; Purpose:
;	Initialize the necessary pointers and structures for string space.
; Inputs:
;	SI = last word of string space
;	AX = first word of string space
; Outputs:
;	None.
; Modifies:
;	SI, DI
; Exceptions:
;	None.
;****

B$STINIT:
	MOV	[b$STRING_END],SI	;string space ends here
	MOV	[SI],0FFFFH		;mark end of string space
	MOV	[b$STRING_FIRST],AX	;dynamic space start is string start
	MOV	DI,AX			;get offset of string space start
	MOV	[b$STRING_FREE],DI	;and set it as the free string
	SUB	SI,DI			;get the total size of string space
	DEC	SI			;allow for header size less one
	MOV	[DI],SI 		;and set the string header
$$XSTS:
	MOV	b$curlevel,0		;main program level is zero
	MOV	CX,NUMTEMP		;Number of string temporaries
	MOV	DI,OFFSET DGROUP:TMPL	;First temp
	MOV	[TMPHDR],DI
	MOV	BX,-1			;Flag for each temp indicates free
	MOV	AX,OFFSET DGROUP:TMPL+LENTEMP ;second temp
	PUSH	ES			
	PUSH	DS			
	POP	ES			;Set ES = DS
LINKTMP:
	STOSW				;Link to next temp
	XCHG	AX,BX
	STOSW				;Store free flag
	STOSW				;in level field of descriptor too
	XCHG	AX,BX
	ADD	AX,LENTEMP		;bytes per temp
	LOOP	LINKTMP
	POP	ES			

	MOV	WORD PTR [DI-LENTEMP],0 ;Last temp must point to nul
	RET
PAGE
	SUBTTL	B$SSClean - Clean String Space
	PAGE
;***
;B$SSClean - Clean String Space
;
;Purpose:
; Scan string space and delete all strings not in the function key table or the
; local heap, or if chaining not in blank common.
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
assumes	ES,DGROUP		
assumes	DS,NOTHING		
cProc	B$SSClean,<NEAR,PUBLIC>
cBegin
	XOR	AX,AX
	CMP	ES:b$Chaining,AL ; If we are not chaining, clear 0-ffff
	JZ	SSCleanAll
;
; clear out all items from 0 to b$commonfirst
;
	MOV	BX,ES:[b$commonfirst]
	CMP	BX,AX		; nothing to clear?
	JE	ClearNext	
	DEC	BX		;Range is inclusive
	CALL	B$SSClearRange

ClearNext:			
;
; Clear out all items from b$commonlast (or start of COMMON if chaining and
; not EI_QB) to 0FFFFH.
;
	MOV	AX,ES:[b$commonlast]



SSCleanAll:
	MOV	BX,0FFFFH
cEnd	nogen			;Fall into B$SSClearRange

	SUBTTL	B$SSClearRange - Clear SS of entries in a range
	PAGE
;***
;B$SSClearRange - Clear SS of entries in a range
;
;Purpose:
; Scan the string space and deallocate all strings whose descriptors are in
; range, and not in the local heap area. (When this routine is used, the local
; heap must be cleaned before, such that the only strings left are fielded
; string backpointer strings or string dynamic array elements.
;
;Entry:
;	AX	= Start address of range in which to delete
;	BX	= End address of range in which to delete (range is inclusive)
;	DS	= Segment containing the near heap
;	ES	= Segment containing heap state vars (& stack)
;
;		    Normally, this means ES=DS=DGROUP, execpt for DOS 5 chain,
;		    during which this routine is called with ES = DGROUP, DS =
;		    the shared memory selector.
;
;Exit:
; None.
;
;Modifies:
; Per Convention
;
;****
cProc	B$SSClearRange,<NEAR,PUBLIC>,SI
cBegin
	MOV	SI,ES:[b$STRING_FIRST] ;initialize scanning pointer
	MOV	CX,BX		;[CX] = end address
;
; Test if string entry is allocated.  If not, then compute the next entry and
; try again. If string space end, then jump to exit.
;
SS_CLEAN_LOOP:
	MOV	BX,[SI] 	;[BX] = string entry header
	TEST	BL,1		;test if entry is allocated
	JZ	SS_CLEAN_ALLOC	;if not, then jump to test if kept
	INC	BX		;[BX] = total length of entry
	JZ	SS_CLEAN_DONE	;if string space end, then jump to finish
	ADD	SI,BX		;[SI] = pointer to next entry in string space
	JMP	SHORT SS_CLEAN_LOOP ;jump to process next entry
;
; String entry is allocated.  Keep string if in blank COMMON.
;
SS_CLEAN_ALLOC:
	CMP	BX,AX		;test if before range
	JB	SS_CLEAN_SKIP	;if so, skip deletion
	CMP	BX,CX		;test if after range
	JA	SS_CLEAN_SKIP	;if so, skip deletion
;
; Keep string if in softkey string table.
;
	CMP	BX,OFFSET DGROUP:B$STRTAB	;test if before softkey table
	JB	SS_CLEAN_VAR_HEAP_TEST		;if so, then jump to next test
	CMP	BX,OFFSET DGROUP:B$STRTAB_END	;test if in the softkey table
	JB	SS_CLEAN_SKIP			;if so, then keep the string

SS_CLEAN_VAR_HEAP_TEST:
	CMP	BX,ES:[b$HEAP_END_SWAP]	;test if before VarHeap
	JB	SS_CLEAN_HEAP_TEST	;if so, then jump to next test
	CMP	BX,ES:[b$HEAP_FIRST_SWAP];test if in VarHeap
	JB	SS_CLEAN_SKIP		;if so, then keep the string

;
; Keep string if descriptor is in the local heap.  Since the local heap was
; cleaned first, the string is either fielded to a file block heap entry or an
; element of a dynamic array.
;
SS_CLEAN_HEAP_TEST:
	CMP	BX,ES:[b$HEAP_END]	;test if in the local heap
	JAE	SS_CLEAN_SKIP		;if so, then keep the string
;
; String is to be deallocated. Compute next entry offset and jump.
;
	MOV	BX,[BX] 	;get length of string data
	INC	BX		;roundup to next word...
	OR	BL,1		;and add one to get unallocated header value
	MOV	[SI],BX 	;put header into string entry
	STC			;set carry
	ADC	SI,BX		;add string header plus one for next entry
	JMP	SHORT SS_CLEAN_LOOP ;branch to process next string entry
;
;	String is to be kept - just compute next offset.
;
SS_CLEAN_SKIP:
	ADD	SI,[BX] 	;add length of string data
	ADD	SI,3		;add two for header and one for roundup...
	AND	SI,NOT 1	;finish roundup to next word
	JMP	SHORT SS_CLEAN_LOOP ;branch to process next string entry

;	Done - restore registers and return.

SS_CLEAN_DONE:
cEnd
assumes	DS,DGROUP		
assumes	ES,NOTHING		

	PAGE
;***
;B$ISdUpd - Adjust string entry backptr when sd moves
;
;Purpose:
;	Added with revision [16].
; QB calls this routine to adjust a string entry backptr when the variable
; table owning the string moves. The variable table is allocated in the local
; heap. Assumes that the variable heap is active.
;
;Entry:
;	pSd	- pointer to string descriptor that is moving
;	Delta	- distance that descriptor is moving
;Exit:
;	string entry backpointer is adjusted
;Uses:
;	AX, BX
;Exceptions:
;	None.
;****

labelFP <PUBLIC,B_ISdUpd>		;Intepreter reachable Label
cProc	B$ISdUpd,<PUBLIC,FAR>,ES	
parmW	pSd
parmW	Delta
cBegin

	CALL	B$TglHeapSptNEAR	; switch context to local heap
	ASSERT_NOT_VARHEAP NH_TEXT	

	MOV	AX,Delta	
DbAssertRel	AX,NE,0,NH_TEXT,<Invalid Delta passed to B$ISdUpd>

	MOV	BX,pSd		;get ptr to string descriptor

	PUSH	DS		; set ES=DS
	POP	ES		
	cCall	B$STADJ	; Adjust backpointer

	CALL	B$TglHeapSptNEAR	; switch context to var heap

cEnd
PAGE


sEnd	NH_TEXT


	END
