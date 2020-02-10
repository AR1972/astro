	TITLE	NHLHCORE - Core Local Heap utilities
;***
;NHLHCORE - Core Local Heap utilities
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
; This module contains the core local heap utilities that are required to
; support /O programs which use string space and simple OPEN statements. Field
; manipulations and additional CHAIN support is elsewhere.
;
;******************************************************************************
;
; Near Heap Support
;
; The near heap support code deals with three pieces:
;
;	Local Heap	- Contains FDBs & Dynamic (small) arrays & QBI tables
;	String Space	- Strings and FIELDed string entries
;	Variable Heap	- Interpeter Only, value tables
;
; In memory, the heaps, associated variables and stack are layed out as
; follows:
;
;			   High Memory (DGROUP)
;
;			+--+--+--+--+--+--+--+--+
; b$HEAP_FIRST	-->	|			| odd:	last useable physical
;			+--+--+--+--+--+--+--+--+	byte of Heap
; b$NH_last	-->	|			| even: last useable physical
;			\ 			\ 	word of Heap
;				Local Heap
;			\ 			\
;			|			|
;			+--+--+--+--+--+--+--+--+
; b$HEAP_END	-->	|    LH_END Constant	| odd: ?
;			+--+--+--+--+--+--+--+--+
;			|			|
;			+			+
; b$STRING_END	-->	|			| even: word containing 0xFFFF
;			\ 			\
;				String Space
; b$NH_First	-\ 	\ 			\
; b$STRING_FIRST --\-->	|			| first useable byte/word
;			+--+--+--+--+--+--+--+--+ \
; b$HEAP_FIRST_SWAP --> |			|  \
;			\ 			\   \
;			      Variable Heap	     Interpeter Only
;			\ 			\   /
; b$HEAP_END_SWAP   --> |			|  /
;			+--+--+--+--+--+--+--+--+ /
;			|			|
;			+			+
; __atopsp	-->	|			| last useable word of stack
;			\ 			\
;				  Stack
;			\ 			\
; b$pend	-->	|			| first useable word of stack
;			+--+--+--+--+--+--+--+--+
;
;==============================================================================
;
;
; Heap Entry:
;
;			+--+--+--+--+--+--+--+--+	High memory
; Pointer to entry -->	|    Entry Type Byte	|
;			+--+--+--+--+--+--+--+--+
;			| File #, if applicable |
;			+--+--+--+--+--+--+--+--+
;			|			|
;			+   Total Entry Size	+
;			|			|
;			+--+--+--+--+--+--+--+--+
;			|     Back Pointer	|
;			+ (Pointer to		+
;			|	 owner pointer) |
;			+--+--+--+--+--+--+--+--+
;			| (offset part of sd	|
;			+  for fielded string	+
;			|  if applicable)	|
;			+--+--+--+--+--+--+--+--+
;			| (length part of sd	|
;			+  for fielded string	+
;			|  if applicable)	|
;			+--+--+--+--+--+--+--+--+
;			|			|
;			\ 			\
;				 Data
;			\ 			\
; Start of Data -->	|			|
;			+--+--+--+--+--+--+--+--+
;			|			|
;			+   Total Entry Size	+
;			|  (Same as in header)	|
;			+--+--+--+--+--+--+--+--+	Low memory
;
; The pointer to an entry normally points to the type byte in the header; since
; length of the entry (hdr + data + trailer length word) is kept at the start
; and the end of the block, it is easy to access; subtract that length from the
; current entry pointer, and it will be pointing to the same place in the next
; hdr - - - yes, since the heap grows down, subtracting from the pointer moves
; you "forward" in the heap. Heap entries can be of any even-byte size; headers
; are 6 bytes in length, except for fdb's, which have 10-byte headers.
;
;==============================================================================
;
; Internal structure of (simple) fielded strings.
;
; FDB:
;
;    /--+-----------------------+--/ /--+---------------+--/ /--+-------+
;	|   Field Buffer	|	|  String Desc	|	|LH_FILE|
;    /--+-----------------------+--/ /--+------------+--+--/ /--+-------+
;	       ^   ^	   ^		      ^      |
;	       |   |	   |		      |      |
; Individual   |   |	   +-----------+      |      |
; Static       |   |		       |      |      |
; String       |   +-------+	       |      |      |
; Descriptors: |	   |	       |      |      |
;	       |	   |	       |      |      |
;	  +-------+   +-------+   +-------+   |      |
;	  |  SD   |   |   SD  |   |  SD   |   |      |
;	  +-------+   +-------+   +-------+   |      |
;		^	    ^		^     |      |
; Fielded	|	    |		|     |      |
; String	+----+	    ++	     +--+     |      |
; BackPointer	     |	     |	     |	      |      |
; String:	     |	     |	     |	      |      |
;		     |	     |	     |	      |      |
;	    +---------------------------------+      |
;	    |	     |	     |	     |		     |
;	    |	  +----------------------------------+
;	    |	 \/  |	     |	     |
;	+---+---+----+--+----+--+----+--+--/
;	| BkPtr | SD Ptr| SD Ptr| SD Ptr|
;	+-------+-------+-------+-------+--/
;
; In summary, the FDB contains a string descriptor which references a string
; (the fielded string backpointer string) containing pointers to several string
; descriptors. These string descriptors (static, in this example) reference
; locations in the FIELD buffer in the FDB, and define the position and length
; of the individual FIELDs.
;
; The backpointer string can grow as additional FIELD statements are executed,
; and can shrink at CHAIN time. Since it is a string, it is also subject to
; movement as string-space compaction occurs. The compaction code must be
; sensitive to the pointers involved in such movement.
;
; When the fielded strings are defined in  a static string array, operation is
; essentially the same as individual static strings. The static SD's do not
; move, and dereferencing performed by any calling code looks exactly like
; static individual SD's.
;
;
; Internal structure of fielded strings in dynamic arrays.
;
; FDB:
;
;    /--+-----------------------+--/ /--+---------------+--/ /--+-------+
;	|   Field Buffer	|	|  String Desc	|	|LH_FILE|
;    /--+-----------------------+--/ /--+------------+--+--/ /--+-------+
;	       ^   ^	   ^		      ^      |
;	       |   |	   |		      |      +------------------+
; Array of     |   |	   +---+	      | 			|
; String       |   |	       |	      +---------------------+	|
; Descriptors: |   +---+       |				    |	|
;	       |       |       |				    |	|
;	+------++------++------++--/ /--+-------+--/ /--+-------+   |	|
;	|  SD	|   SD	|  SD	|	| BkPtr |	|LH_ARRA|   |	|
;	+-------+-------+-------+--/ /--+-----+-+--/ /--+-------+   |	|
;	       ^       ^       ^	      | 		    |	|
; Fielded      |       |       |	      | 		    |	|
; String       +-----+ +-----+ +-----+	      +----+		    |	|
; BackPointer  ^     |	     |	     |		   |		    |	|
; String:      +-----|-------|-------|----------+  |		    |	|
;		     |	     |	     |		|  |		    |	|
;	    +-------------------------------------------------------+	|
;	    |	     |	     |	     |		|  |			|
;	    |	  +-----------------------------------------------------+
;	    |	 \/  |	     |	     |		|  |
;	+---+---+----+--+----+--+----+--+--/	|  |
;	| BkPtr | SD Ptr| SD Ptr| SD Ptr|	|  |
;	+-------+-------+-------+-------+--/	|  |
;						|  |
; String	 +------------------------------+  |
; Array 	 |				   |
; Descriptor:	 |     +---------------------------+
;		 |    \/
;		++------+--/
;		|DatPtr |
;		+-------+--/
;
;
; The added complication of dynamic fielded string arrays is that the string
; descriptors live in the local heap, and hence can move, in addition to the
; backpoint string, which lives in string space and can also move. Both LH and
; SS code must update pointers appropriately.
;
;==============================================================================
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

	externW b$STRING_END		;defined in NHSTUTIL.ASM
	externW b$STRING_FREE		; defined in NHSTUTIL.ASM
	externW b$NH_first		;defined in NHINIT.ASM
	externW b$PTRFIL		;defined in GLOBAL.INC

;NOTE: Variable Heap support code requires next 4 data items be contiguous [23]

	globalW b$HEAP_FIRST,,1 	;heap start pointer
	globalW b$HEAP_FREE,,1
	globalW b$HEAP_END,,1
	globalW b$P_HEAP_GROW,,1	

;NOTE: Variable Heap support code requires next 4 data items be contiguous [23]

	globalW b$HEAP_FIRST_SWAP,,1	 
	globalW b$HEAP_FREE_SWAP,,1	 
	globalW b$HEAP_END_SWAP,,1	 
	staticW P_HEAP_GROW_SWAP,,1	 

	globalW b$fVarHeapActive,,1	 ; non-0 when variable heap
					 ; is active

sEnd	_BSS

sBegin	_DATA

	globalW b$pFHRaiseBottom,Near_Ret,1  ;vector for B$FHRaiseBottom

	globalB b$Clearing,0,1		; CLEAR statement in process flag
sEnd	_DATA

	externFP B$ULDataRelease ;releases ul Data images
	externFP CompressHelp	

sBegin	NH_TEXT

	ASSUMES CS,NH_TEXT

	PUBLIC	B$LHNXTFIL	; find next file entry in heap

	PUBLIC	B$LHADJ	;adjust heap entry
	PUBLIC	B$LHDALC	;deallocate heap entry

	PUBLIC	B$LHFLDDESCADJ	; delete/adjust descriptor in backpointer string
	PUBLIC	B$LHDALCALLFLD	; deallocate all fielded strings from heap entry

	PUBLIC	B$LHSetFree	;set free heap entry pointer
	PUBLIC	B$LH_ALC_FREE	;[ln]
	PUBLIC	B$LH_PTR_FROM_DATA 
	PUBLIC	B$LH_CPCT	
	PUBLIC	B$LH_FROM_SS	;[ln]
	PUBLIC	B$LH_PTR_CHECK	;[ln] check entry at [SI] for consistency
	PUBLIC	B$LH_SCAN	;[ln]

	PUBLIC	B$NHINIT	;initialize dynamic space


	externNP B$LH_I_ADJ	;[ln] adjust backptrs to any owners in this entry
	externNP B$VAR_ALC_GROW ;[ln]

	externNP B$ERR_SSC		 
	externNP B$STCPCT
	externNP B$STADJ
	externNP B$STINIT
	externNP B$STSetFree
	externNP B$SSClean	; clean string space

ASSERT_NOT_VARHEAP	MACRO	SEG	;
	ENDM				;


	PAGE
;***
; LH_ALC_GROW - Grow local heap to support allocation of a block of given size
; Purpose:
;	Added with revision [23].
;
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
DbPub	LH_ALC_GROW
LH_ALC_GROW:
;	Step 3 - Get free string entry to heap space, test free entry.

	CALL	B$LH_FROM_SS	;[ln] get free string from string space
	CALL	B$LH_ALC_FREE	;test free entry for room
	JNC	LH_ALC_GROW_DONE; allocated - jump to return to string

;	Step 4 - Perform string compaction, get free string, test free entry.

	CALL	B$STCPCT	;perform the string compaction
	CALL	B$LH_FROM_SS	;[ln] get free string from string space
	CALL	B$LH_ALC_FREE	;test free entry for room
	JNC	LH_ALC_GROW_DONE; allocated - jump to return to string
;
;	Step 5 - Kick FH out of DS if possible
;
	CALL	[b$pFHRaiseBottom] ;[46]
	CALL	B$LH_FROM_SS	;[ln] get free string from string space
	CALL	B$LH_ALC_FREE	; test free entry for room
	JNC	LH_ALC_GROW_DONE;allocated jump to return string

;
;	Step 6 - Kick UL data images out of FH and raise the bottom

	PUSH	AX		;preserve AX-DX across call to
	PUSH	BX		;B$ULDataRelease, as the far
	PUSH	CX		;call dispatcher to the quicklibrary
	PUSH	DX		;can honk on these registers.
	CALL	B$ULDataRelease ;free UL data images if allocated
	POP	DX		
	POP	CX		
	POP	BX		
	POP	AX		;recover regs

	CALL	[b$pFHRaiseBottom] ;give FH's free space to NH
	CALL	B$LH_FROM_SS	;[ln]get free string from string space
	CALL	B$LH_ALC_FREE	;test free entry for room
	JNC	LH_ALC_GROW_DONE;allocated jump to return string

;
;	Step 7 - Kick Help out of FH and raise the bottom

	PUSH	AX		;preserve AX-DX across call to
	PUSH	BX		;CompressHelp
	PUSH	CX		
	PUSH	DX		
	CALL	CompressHelp	;Free the help system
	POP	DX		
	POP	CX		
	POP	BX		
	POP	AX		;recover regs

	CALL	[b$pFHRaiseBottom] ;give FH's free space to NH
	CALL	B$LH_FROM_SS	;get free string from string space
	CALL	B$LH_ALC_FREE	;test free entry for room

Near_Ret:			;Near Return for vector
LH_ALC_GROW_DONE:
	RET

;***
; B$LH_FROM_SS - get local heap space from string space
; Purpose:
;	Determine if a free entry exists at the end of
;	string space.  If so, change the string and heap
;	space pointers so that it is now part of the local
;	heap space.
;
;	Mostly rewritten with revision [53].
;
; Strategy (order of attempts swapped with revision [64]):
; (1)	If the last entry in the LH is a free entry, tack the free
;	string bytes onto that entry and move b$HEAP_END past it.
;
; (2)	If the last entry in the LH is NOT a free entry and the free
;	string is large enough to create a heap entry (LH_STD_HDR_LEN
;	bytes + 2 bytes for back length), then create a new LH entry
;	with the free space.
;
;	If neither (1) or (2) is possible, then we can't give any of
;	the free string to the LH, just exit.
;
; Inputs:
;	None.
; Outputs:
;	[b$HEAP_FREE] = pointer to new (or enlarged) free heap entry.
; Modifies:
;	None.
; Exceptions:
;	None.
;****
B$LH_FROM_SS:
	ASSERT_NOT_VARHEAP NH_TEXT	
	PUSH	AX			;save registers...
	PUSH	SI
	PUSH	DI
	CALL	B$STSetFree		;SI points to trailing free string
	MOV	AX,[b$STRING_END]	;get end pointer in AX
	SUB	AX,SI			;compute size of free string
	JZ	LH_SS_RETURN		;if no free bytes, exit quickly

;	Size of free string (including header) is in AX.
;	Pointer to free string header is in SI.

DbAssertTst AX,Z,1,NH_TEXT,<Free string size odd in B$LH_FROM_SS>

;	(1) If the last entry in the LH is free, tack the new empty space
;	    on to it.

	MOV	DI,[b$HEAP_END] 	;[DI] = heap END entry
	CMP	DI,[B$HEAP_FIRST]	; is END the only entry?
	JE	LH_SS_TRY_2		; yes, go create a free entry
	ADD	DI,[DI+1]		;[DI] = last entry before END
	CMP	[DI].LHTYPE,LOW LH_FREE ;is entry unallocated?
	JNE	LH_SS_TRY_2		;no, go try next option
	ADD	AX,[DI].LHLEN		;compute new length
	JMP	SHORT LH_SS_FINISH_UP	; finish updating pointers and exit

;	(2) If there are enough free bytes to create a new free entry at the
;	    end of the LH, do that.

LH_SS_TRY_2:
	CMP	AX,LH_STD_HDR_LEN+2	;enough free space?
	JB	LH_SS_RETURN		;if not, we can't do anything

;	Create the new heap entry from the empty space.

	MOV	DI,SI			;[DI] = what will be string END entry
	ADD	DI,LH_STD_HDR_LEN+1	;[DI] = what will be heap END entry
	ADD	DI,AX			;[DI] = new entry being created

LH_SS_FINISH_UP:
;	When we get here:
;		SI points to new string space end location
;		DI points to new (or enlarged) free heap entry
;		AX = size of new (or enlarged) free heap entry

;	Define the new string space end entry and its pointer.

	MOV	[SI],0FFFFH		;set string end entry
	MOV	[b$STRING_END],SI	;set string end pointer
	MOV	[b$STRING_FREE],SI	;set free string pointer

;	Define the new heap END entry and its pointer.

	ADD	SI,LH_STD_HDR_LEN+1	;point to new END entry
	MOV	[SI].LHTYPE,LOW LH_END	;define the entry type
	MOV	[b$HEAP_END],SI 	;define the heap end pointer

;	Update header for new or enlarged free heap entry and set
;	b$HEAP_FREE to point to it.

	MOV	[DI].LHTYPE,LOW LH_FREE ;set heap entry type
	MOV	[DI].LHLEN,AX		;set free heap entry length
	MOV	[SI+1],AX		;set free heap entry backlength
	MOV	[b$HEAP_FREE],DI	;set heap free entry pointer

LH_SS_RETURN:
	POP	DI			;restore registers...
	POP	SI
	POP	AX
	RET				;return with new heap free entry

;***
;	B$LH_SCAN -Scan the entire local heap area for an entry of the
;		   requested amount of storage.  Adjacent unallocated
;		   entries are combined before tested for allocation.
;
;	Inputs: 	BX - amount of heap storage to allocate.
;			DL - type of heap entry to allocate.
;			CL - if DL=LH_FILE, file number
;			CX - if DL=LH_ARRAY, array descriptor offset
;			[b$HEAP_FIRST] - pointer to first entry to test
;
;	Outputs:	CF=0 - allocation was successful.
;			     SI - pointer to data in allocated entry.
;			CF=1 - allocation failed.
;
;	Start the scan with the entry pointed by [b$HEAP_FIRST].
;****
B$LH_SCAN:
	MOV	SI,[b$HEAP_FIRST] ;starting point of scan

;	No pending unallocated entries.  If END entry, then failed.
;	If FREE entry, then jump to try to combine subsequent ones.

LH_SCAN_NEXT:
	CMP	[SI].LHTYPE,LOW LH_END ;test if last local heap entry
	JE	LH_SCAN_FAIL	;if so, then allocation failed
	CMP	[SI].LHTYPE,LOW LH_FREE ;test if entry is unallocated
	JE	LH_SCAN_FREE	;if so, jump to scan for next free entries
	SUB	SI,[SI].LHLEN	;move pointer to the next entry
	JMP	SHORT LH_SCAN_NEXT ;and try again

;	Pending unallocated entry.  Test the next entry.  If END, try
;	a final allocation attempt.  If allocated, attempt allocation
;	and continue scan if it fails.	If FREE, combine the two entries
;	and loop back.	Keep size of free entry in AX until allocation.

LH_SCAN_FREE:
	MOV	AX,[SI].LHLEN	;get size of first free block
LH_SCAN_NEXT_FREE:
	MOV	DI,SI		;get copy of present scan pointer
	SUB	DI,AX		;now points to following entry
	CMP	[DI].LHTYPE,LOW LH_END ;test if next entry is END
	JE	LH_SCAN_TRY	;if so, try a final allocation
	CMP	[DI].LHTYPE,LOW LH_FREE ;test if next entry is FREE
	JNE	LH_SCAN_TRY	;if not, try allocation, but continue
	ADD	AX,[DI].LHLEN	;add for length of both FREE entries
	JMP	SHORT LH_SCAN_NEXT_FREE ;and loop to try again

;	Try to allocate from the entry at [SI].  If failure, point
;	past the allocated entry at [DI] and continue scan if not at
;	heap end.  If at heap end, return with failure.

LH_SCAN_TRY:
	MOV	[SI].LHLEN,AX	;set the new length of the combined block
	SUB	SI,AX		;point to next block (one after backlength)
	MOV	[SI+1],AX	;set the backlength
	ADD	SI,AX		;return pointer to start of combined block
	CMP	AX,BX		;test entry against allocation needed
	JAE	LH_ALC		;if enough room, finish allocation
	CMP	[DI].LHTYPE,LOW LH_END ;test if at end of heap
	JE	LH_SCAN_FAIL	;if so, then just fail
	SUB	DI,[DI].LHLEN	;point to entry after allocated one
	MOV	SI,DI		;move pointer to continue scan
	JMP	SHORT LH_SCAN_NEXT ;and jump to continue it

;	Allocate not possible, return with carry set for failure.

LH_SCAN_FAIL:
	MOV	[b$HEAP_FREE],SI ;set free entry to last unallocated one
	STC			;carry set for failure
	RET			;and return to caller
;	B$LH_ALC_FREE - test if free heap entry can allocate the
;			requested amount of storage

;	Inputs: 	BX - amount of heap storage to allocate.
;			DL - type of heap entry to allocate.
;			CL - if DL=LH_FILE, file number
;			CX - if DL=LH_ARRAY, array descriptor offset
;			[b$HEAP_FREE] - pointer to entry to test

;	Outputs:	CF=0 - allocation was successful.
;			     SI - pointer to data in allocated entry.
;			CF=1 - allocation failed.

;	Test if free local heap entry is unallocated and large enough.
;	If so, then allocate it, otherwise fail.

B$LH_ALC_FREE:
	MOV	SI,[b$HEAP_FREE] ;point to free local heap entry
	CMP	[SI].LHTYPE,LOW LH_FREE ;test if free entry is unallocated
	JNE	LH_ALC_FAIL	;if not free, then jump to fail
	MOV	AX,[SI].LHLEN	;get length of the entry
	CMP	AX,BX		;test if entry is large enough
	JAE	LH_ALC		;if so, then jump to allocate
LH_ALC_FAIL:
	STC			;set carry to note failure
	RET			;return to caller

;	There wasn't enough room to split entry and allocate a new FREE
;	entry.	Allocate the whole entry instead of splitting it.

LH_ALC_ALL:			
	ADD	BX,AX		;change requested block size to whole entry
	JMP	SHORT LH_ALC_EXACT ;allocate whole block

;	The entry at [SI] can be allocated.  If the entry is larger than
;	the size needed, split it into two entries with the higher one
;	used in the allocation.

LH_ALC:
	JE	LH_ALC_EXACT	;if exact allocation, then no split needed
	SUB	AX,BX		;size of remainder block
	CMP	AX,LH_STD_HDR_LEN ;is there enough room for new header?
	JB	LH_ALC_ALL	;allocate whole entry if not enough room
	SUB	SI,BX		;point to header of new remainder entry
	MOV	[SI].LHTYPE,LOW LH_FREE ;set type for FREE entry
	MOV	[SI].LHLEN,AX	;put size into new entry header
	SUB	SI,AX		;point to next entry (byte before backlength)
	MOV	[SI+1],AX	;put in backlength for the present entry
	ADD	SI,AX		;point back to remainder block
	ADD	SI,BX		;point back to newly allocated block
LH_ALC_EXACT:

;	Clear the new heap entry to zeroes.

	PUSH	CX		;save registers for clear...
	PUSH	DI
	PUSH	ES		
	PUSH	DS		
	POP	ES		;set ES = DS
	MOV	DI,SI		;copy pointer to header of allocated block
	SUB	DI,BX		;point to header of previous block
	MOV	[b$HEAP_FREE],DI ;free entry is now previous block
	INC	DI		;now point to data of block to be allocated
	XOR	AX,AX		;value to set entry locations
	MOV	CX,BX		;get size of entry in bytes
	SHR	CX,1		;convert size to words
	REP	STOSW		;clear the entry
	POP	ES		
	POP	DI		;restore registers...
	POP	CX

;	Finish by setting the header values

	MOV	[SI].LHTYPE,DL	;set the heap entry type
	MOV	[SI].LHLEN,BX	;set the heap entry length
	SUB	SI,BX		;point to next entry (byte before backlength)
	MOV	[SI+1],BX	;set the backlength
	ADD	SI,BX		;set point back to entry header

	CMP	[SI].LHTYPE,LOW LH_FILE ;test if FILE entry
	JNE	LH_ALC_NOT_FILE ;if not, then branch
	MOV	[SI].LHFNUM,CL	;set file number
	JMP	SHORT LH_ALC_EXIT

LH_ALC_NOT_FILE:
	MOV	[SI].LHBAKP,CX	;all entries have backpointers except fdb's

LH_ALC_EXIT:
	SUB	SI,BX		;point to previous entry
	ADD	SI,3		;set to start of entry data (past backlength)
	RET			;return with carry clear



	page
;*** 
; B$LHDALC_CPCT -- deallocate heap entry and compact heap.  Added with [44].
;
;Purpose:
;	Deallocates heap entry, and then compacts local heap, so that no 
;	no "holes" develop in the heap.
;
;Entry/Exit/Uses/Exceptions:
;	Same as B$LHDALC/B$LH_CPCT.
;
;******************************************************************************
cProc	B$LHDALC_CPCT,<PUBLIC,NEAR>
cBegin
	CALL	B$LHDALC		; deallocate heap entry
cEnd	<nogen>				; fall into B$LH_CPCT

;***
; B$LH_CPCT - compact local heap space
; Purpose:
;	Compacts all allocated local heap entries to the top of
;	the local heap space.  The backpointers in the ARRAY and FILE
;	entries are adjusted appropriately to reflect their movement.
;	The remaining unallocated space is made into the free entry.
;	NOTE: The scan (and compaction) moves from high memory to low.
;
; Inputs:
;	None
; Outputs:
;	[b$HEAP_FREE] points to the new free heap entry.
; Exceptions:
;	B$ERR_SSC - nontrappable error if compaction finds corruption
;		  in the local heap space structure.
;****

B$LH_CPCT PROC    NEAR
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	AX		;save registers used...
	PUSH	BX
	PUSH	CX
	PUSH	SI
	PUSH	DI

	MOV	SI,[b$HEAP_FIRST] ;pointer to heap scan

;	Skip over leading allocated entries which can be ignored since
;	they will not be moved.  While skipping, an END entry implies
;	no compaction need to be done.	Check all allocated entries.

B$LH_CPCT_SKIP:
	CMP	[SI].LHTYPE,LOW LH_END ;test for end of local heap
	JE	B$LH_CPCT_DONE    ;if so, no compact, just return
	CALL	B$LH_PTR_CHECK	  ;check entry at [SI] for consistency
	CMP	[SI].LHTYPE,LOW LH_FREE ;test if leading allocated entry
	JE	B$LH_CPCT_FIRST_FREE ;if not, then skipping is over
	SUB	SI,[SI].LHLEN	;point to next entry
	JMP	SHORT B$LH_CPCT_SKIP ;and try again

;	First unallocated heap entry found.  Initialize compacted pointer DI.

B$LH_CPCT_FIRST_FREE:
	MOV	DI,SI		;SI=scan pointer - DI=compacted pointer

;	Unallocated heap entry just advances the scan pointer SI.

B$LH_CPCT_NEXT_FREE:
	SUB	SI,[SI].LHLEN	;point to entry after unallocated one

;	Process the entry at [SI].  First check consistency of entry.

B$LH_CPCT_NEXT:

;	If END entry, compaction is done - jump to finish up.

	CMP	[SI].LHTYPE,LOW LH_END ;test for END entry
	JE	B$LH_CPCT_SETUP_FREE ;jump to set up free entry

;	If FREE entry, jump to advance scan pointer SI.

	CALL	B$LH_PTR_CHECK	  ;check entry at [SI]
	CMP	[SI].LHTYPE,LOW LH_FREE ;test for FREE entry
	JE	B$LH_CPCT_NEXT_FREE ;jump to advance scan pointer

;	Allocated entry - adjust the entry backpointers to reflect
;	its new location at [DI].

	MOV	AX,DI		;set to new entry location
	SUB	AX,SI		;subtract to get adjustment factor
	CALL	B$LHADJ	;adjust the entry using the factor in AX

;	Move the entry from [SI] to [DI] with direction flag SET.

	MOV	CX,[SI].LHLEN	;get length of entry in bytes
	SHR	CX,1		;make the length in words
	DEC	SI		;point to word address in source
	DEC	DI		;point to word address in destination
	STD			;all strings move down in memory
	REP	MOVSW		;move the entry to its new location
	CLD			;restore direction flag
	INC	SI		;point back to byte in source
	INC	DI		;also in destination

;	Now make the space between the moved entry and the next entry
;	a free heap entry.  This will ensure heap consistency and
;	allow routines called during the middle of compaction to
;	walk the heap.

	MOV	[DI].LHTYPE,LOW LH_FREE ;set type of FREE entry
	MOV	[DI].LHLEN,AX	;set size of new entry
	MOV	[SI+1],AX	;also set backlength of new entry
	JMP	SHORT B$LH_CPCT_NEXT ;try again (SI and DI are already adjusted)

;	Compaction is done - set up the unused space as a FREE entry and
;	point to it for the next allocation.

B$LH_CPCT_SETUP_FREE:
	MOV	AX,DI		;get length of remaining entry...
	SUB	AX,SI		;by the difference of the pointers
	JZ	B$LH_CPCT_NO_FREE ;if no entry left, then jump
	MOV	[DI].LHTYPE,LOW LH_FREE ;set type of FREE entry
	MOV	[DI].LHLEN,AX	;set size of new entry
	MOV	[SI+1],AX	;also set backlength of new entry
B$LH_CPCT_NO_FREE:
	MOV	[b$HEAP_FREE],DI ;new entry for next allocation

;	Restore registers and return.

B$LH_CPCT_DONE:
	POP	DI		;restore registers...
	POP	SI
	POP	CX
	POP	BX
	POP	AX
B$LH_CPCT_EXIT:
	POP	ES		
	RET			;return to caller
B$LH_CPCT ENDP


;***
; B$LHDALC - deallocate heap entry
; Purpose:
;	To deallocate the heap entry whose data area is pointed by SI.
;
; Inputs:
;	SI = pointer to heap entry data area
; Outputs:
;	None.
; Modifies:
;	ES, if interpreter version
; Exceptions:
;	None.
;****
B$LHDALC	PROC	NEAR
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	PUSH	AX		;save registers...
	PUSH	SI
	CALL	B$LH_PTR_FROM_DATA ;get entry pointer from SI data pointer
	XOR	AX,AX		;set flag for entry deallocation
	CALL	B$LHADJ	;call to deallocate all string data in entry
	MOV	[SI].LHTYPE,LOW LH_FREE ;set array type to FREE
	POP	SI		;restore registers...
	POP	AX
	POP	ES		
	RET			;return to caller
B$LHDALC	ENDP


;***
; B$LHADJ - delete/adjust a heap entry
; Function:
;	Deallocates or adjusts the heap entry pointed by SI.
;	If AX=0, all string data referenced by descriptors within
;	the entry is deallocated.
;	If AX<>0, all backpointers referenced within the entry are
;	adjusted by the value of AX.
;
;	NOTE: In QB versions, B$LHDALC can call this routine to deallocate [23]
;	NOTE: an entry in the variable heap while the local heap is active. [23]
;	NOTE: This causes no problems in this routine as it is now, because [23]
;	NOTE: we'll never have an LH_FILE or LH_ARRAY entry in the variable [23]
;	NOTE: heap.                                                         [23]
;
; Inputs:
;	AX = adjustment value
;	     AX=0  - deallocate all entry string data.
;	     AX<>0 - adjust all entry backpointers.
;	SI = address of heap entry.
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

B$LHADJ  PROC	  NEAR		
	PUSH	BX		;save registers...
	PUSH	CX
	CMP	[SI].LHTYPE,LOW LH_FILE ;test if entry is FILE
	JNE	ADJLHP_NOT_FILE ;if not, then jump [23]

	ASSERT_NOT_VARHEAP NH_TEXT 

	LEA	BX,[SI+3]	;point to 1 word past heap header
	SUB	BX,[SI].LHLEN	;point at heap entry data
	CMP	BX,ES:[b$PTRFIL] ;are we moving PTRFIL?
	JNZ	NotPtrFil	;brif not

	ADD	ES:[b$PTRFIL],AX ;adjust if moving ptrfil

NotPtrFil:

;	Entry is FILE - deallocate/adjust the fielded string backpointer
;	if it exists.

	CMP	WORD PTR [SI].LHPLEN,0 ;test if fielded string is null
	JZ	ADJLHP_EXIT	;if so, then just return
	MOV	BX,[SI].LHPOFF	;get offset of fielded string data
	ADD	[BX-2],AX	;add adjustment to the string backpointer
	OR	AX,AX		;test if deallocation was requested
;	JNZ	ADJLHP_QUIT	;if just adjustment, then jump
	JNZ	ADJLHP_ALL_FLDS ;if adjustment, then adust fielded strings
	MOV	[SI].LHTYPE,LOW LH_FREE ;mark the deallocated block free
	CALL	B$LHDALCALLFLD	; otherwise deallocate all fielded strings
	JMP	SHORT ADJLHP_QUIT ;jump to return to caller

;	Entry contains fielded strings - adjust pointers into field buffer

ADJLHP_ALL_FLDS:
	MOV	CX,[SI].LHPLEN	;get size of string entry for fielded strs
	SHR	CX,1		;compute number of fielded strings
	PUSH	SI		;preserve heap pointer
ADJ_FLD_LOOP:
	MOV	SI,[BX] 	;get pointer to field variable
	ADD	[SI+2],AX	;adjust pointer into field buffer
	INC	BX		;move to next field variable
	INC	BX
	LOOP	ADJ_FLD_LOOP	;loop until all ptrs to field buffer have
				;been adjusted
	POP	SI		;recover heap pointer
ADJLHP_EXIT:
	JMP	SHORT ADJLHP_QUIT ;jump to return to caller

;	If entry is not ARRAY, then return.

ADJLHP_NOT_FILE:
	CMP	[SI].LHTYPE,LOW LH_ARRAY ;test if entry is ARRAY
	JNE	ADJLHP_INTERP	;if not, consider interp. entries (if any)

;	Entry is ARRAY, first clear/adjust the array descriptor pointer.

	MOV	BX,[SI].LHBAKP	;get offset of array descriptor
	OR	AX,AX		;test if array is being deallocated
	JNZ	ADJLHP_ADJUST	;if just being adjusted, then jump
	MOV	[BX].AD_fhd.FHD_hData,AX ;deallocated - clear the descriptor
	MOV	[SI].LHBAKP,AX	;also clear the heap backpointer
	MOV	[SI].LHTYPE,LOW LH_FREE ;mark the deallocated block free
ADJLHP_ADJUST:

;	Next, get pointer to start of array data and deallocate/adjust
;	the backpointers of all nonnull strings in the array.

	MOV	CX,[SI].LHLEN	;get length of heap entry (hdr+data+backlen)
	MOV	BX,SI		;get copy of heap entry pointer
	SUB	BX,CX		;point to next heap entry
	ADD	BX,3		;move past backlength to array start
	SUB	CX,LH_STD_HDR_LEN+2 ;data length less header and backlength
	SHR	CX,1		;data length in words...
	SHR	CX,1		;length in doublewords (number of descs)

;	For each nonnull descriptor, deallocate/adjust the string data.
; Process null strings through B$STADJ, fielded ones must be adjusted.

ADJLHP_LOOP:
;	CMP	WORD PTR [BX],0 ;test if string is nonnull
;	JZ	ADJLHP_NEXT	;if so, then skip this entry
	CALL	B$STADJ	;deallocate/adjust the string at desc [BX]
ADJLHP_NEXT:
	ADD	BX,4		;point to next descriptor
	LOOP	ADJLHP_LOOP	;loop to process the next one

ADJLHP_INTERP:

	OR	AX,AX
	JZ	ADJLHP_QUIT	;brif deallocation - the below is for adjustment
	CALL	B$LH_I_ADJ	;[ln] adjust backptrs to any owners in this entry

ADJLHP_DONE:
	;NOTE: important to update the owner LAST, after interpreter call
	;	back work is done.
	MOV	BX,[SI].LHBAKP	;get pointer to owner
	ADD	[BX],AX 	;add adjustment to owner pointer
	CMP	[SI].LHTYPE,LOW LH_ARRAY; test if entry is ARRAY
	JNE	ADJLHP_QUIT		; jump if not
	ADD	[BX].AD_oAdjusted,AX	; else update adjusted pointer

ADJLHP_QUIT:
	POP	CX		;restore registers...
	POP	BX
	RET			;return to caller
B$LHADJ  ENDP			
assumes	DS,DGROUP		
assumes	ES,NOTHING		


;***
; B$LHDALCALLFLD - deallocate all fielded strings for heap entry
; Purpose:
;	To deallocate all fielded strings associated with the heap
;	entry pointed by SI.  The heap backpointer string is also
;	deallocated.
;
; Inputs:
;	SI = pointer to heap entry
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;****

B$LHDALCALLFLD:		
	ASSERT_NOT_VARHEAP NH_TEXT 
	PUSH	AX		;save registers...
	PUSH	BX
	PUSH	CX
	PUSH	DI
	XOR	AX,AX		;clear AX to zero
	MOV	BX,AX		;same for BX...
	MOV	CX,AX		;and CX
	XCHG	CX,[SI].LHPLEN	;swap bkptr str len and 0
	XCHG	BX,[SI].LHPOFF	;swap bkptr str off and 0
	INC	CX		;add one for backpointer value
	MOV	[BX-2],CX	;free string by setting backpointer value
	SHR	CX,1		;number of words to process
DALCALLFLD_LOOP:
	MOV	DI,[BX] 	;get next word from string
	MOV	[DI],AX 	;clear descr length
	MOV	[DI+2],AX	;clear descr offset
	ADD	BX,2		;point to next descriptor in string
	LOOP	DALCALLFLD_LOOP ;if not done, then loop
	POP	DI		;restore registers...
	POP	CX
	POP	BX
	POP	AX
	RET			;return to caller


;***
; B$LHFLDDESCADJ - delete/adjust descriptor in field backpointer string.
; Purpose:
;	Search through the local heap FILE entries for the descriptor
;	address specified in BX.  If AX=0, then remove it from the
;	field backpointer string.  If AX<>0, then just adjust the
;	backpointer string element by adding AX to it.
;
; Inputs:
;	AX = adjustment factor
;	     AX=0 - delete descriptor BX from backpointer string.
;	     AX<>0 - adjust descriptor BX in backpointer string by AX.
;	BX = address of descriptor to be deleted/adjusted.
;	DS = segment that heap is in
;	ES = segment that state vars are in
; Outputs:
;	None.
; Modifies:
;	SI.
; Exceptions:
;	B$ERR_SSC if descriptor offset not found.
;****

B$LHFLDDESCADJ:
assumes	ES,DGROUP		
assumes	DS,NOTHING		

	ASSERT_NOT_VARHEAP NH_TEXT 

	PUSH	AX		;save registers...
	PUSH	BX
	PUSH	CX
	PUSH	DX
	PUSH	DI

	XCHG	DX,AX		; [DX] = adjustment factor
	XCHG	AX,BX		; [AX] = object of searches
	XOR	BX,BX		;start at beginning of heap
FLDDESCADJ_NEXT:
	MOV	SI,BX		;put current FDB point in for test
	CALL	NXTFIL_ES	;get next FILE data pointer in SI
	JZ	FLDDESCADJ_ERROR ;if no more entries, then done

	MOV	BX,SI		;save current FDB point for next time
	CALL	B$LH_PTR_FROM_DATA ;get heap entry pointer in SI

;	Heap entry is for a file.  Determine if a backpointer string
;	is defined.  If so, then scan string for descriptor value in AX.

	MOV	CX,[SI].LHPLEN	;get length of string
	JCXZ	FLDDESCADJ_NEXT ;if null string, then try next entry
	SHR	CX,1		;make it in words
	MOV	DI,[SI].LHPOFF	;get offset of the string
	PUSH	ES		;make sure that we use the proper
	PUSH	DS		;segment in case that
	POP	ES		;we are in the middle of unwinding a CHAIN
	REPNE	SCASW		;scan for word in AX
	POP	ES		;recover correct ES = BASIC DS
	JNZ	FLDDESCADJ_NEXT ;if not there, try next

;	Word found in string.  DI is pointing just after the word
;	while CX contains the number of words in the string AFTER
;	the one searched.  If modifying (DX<>0), just add the adjustment
;	factor to the word.

	ADD	[DI-2],DX	;assume adjustment is made (deleted otherwise)
	OR	DX,DX		;test if adjustment really wanted
	JNZ	FLDDESCADJ_DONE ;if so, then jump to return now

;	If deleting (DX=0), start at DI and move CX words left by two
;	and put a null free string header where the last word of the
;	string was.

	SUB	WORD PTR [SI].LHPLEN,2 ;string size is less by 2
	JZ	FLDDESCADJ_CLR	;jump if string is now empty
	MOV	SI,DI		;SI points to byte after
	DEC	DI		;point DI to...
	DEC	DI		;the word before (AX value)
	PUSH	ES		;make sure that we use the proper
	PUSH	DS		;segment in case that
	POP	ES		;we are in the middle of unwinding a CHAIN
	REP	MOVSW		;move the string left 2
	POP	ES		;recover correct ES = BASIC DS
	MOV	WORD PTR [DI],1 ;null string free header
	JMP	SHORT FLDDESCADJ_DONE ;done, jump to finish
FLDDESCADJ_CLR:
	XOR	DI,DI		;clear register
	XCHG	DI,[SI].LHPOFF	;clear descr offset, get it
	MOV	WORD PTR [DI-2],3 ;set ptr to empty string
FLDDESCADJ_DONE:
	POP	DI		;restore registers...
	POP	DX
	POP	CX
	POP	BX
	POP	AX
	RET			;return to caller

FLDDESCADJ_ERROR:
	JMP	B$ERR_SSC	; report string space corruption
assumes	DS,DGROUP		
assumes	ES,NOTHING		

;***
; B$LHNXTFIL, NXTFIL_ES - return FDB pointer of next file in heap after [SI]
; Purpose:
;	To return a pointer to the next FDB in the heap.  The
;	local heap is searched from the last FDB specified in SI
;	(or the first if SI=0).
;
; Inputs:
;	SI = pointer to FDB to start search from (0 if first FDB
;	     pointer is to be returned).
; Outputs:
;	SI = pointer to next FDB after the one specified on routine
;	     entry (0 if no more FDB's in the heap).
;	ZF = clear if FDB returned
;	     set if none returned
; Modifies:
;	None.
; Exceptions:
;	None.
;****
NXTFIL_ES:			;Assumes ES already set up
	PUSH	ES		
	JMP	SHORT NXTFIL_COMMON 

B$LHNXTFIL:			
	PUSH	ES		
	PUSH	DS		
	POP	ES		;set ES = DS

NXTFIL_COMMON:			;common entry
	ASSERT_NOT_VARHEAP NH_TEXT 
assumes	ES,DGROUP		
assumes	DS,NOTHING		
	OR	SI,SI		;test if first file is to be searched
	JNZ	NXTFIL_NEXT	;if so, then branch
	MOV	SI,ES:[b$HEAP_FIRST] ;initialize to start of local heap
	ADD	SI,3		;adjust to avoid extra jump
NXTFIL_NEXT:
	SUB	SI,3		;move from FDB over backlength to next entry
NXTFIL_LOOP:
	CMP	[SI].LHTYPE,LOW LH_END ;test if last entry in heap
	JE	NXTFIL_END	;if so, then jump
	CMP	[SI].LHTYPE,LOW LH_FILE ;test if entry is file
	JE	NXTFIL_FILE	;if so, then jump
	SUB	SI,[SI].LHLEN	;point to next heap entry
	JMP	SHORT NXTFIL_LOOP ;and continue the search
NXTFIL_FILE:
	SUB	SI,[SI].LHLEN	;point to previous entry
	ADD	SI,3		;move over backlength to entry FDB
assumes	ES,NOTHING		
	POP	ES		
	RET			;return to caller (ZF clear)
NXTFIL_END:
	XOR	SI,SI		;clear to show no more files
	POP	ES		
	RET			;return to caller (ZF set)

;***
;B$LH_PTR_FROM_DATA - get heap entry pointer from data pointer
;LH_PTR_FROM_BLEN - get heap entry pointer from backlength pointer
;B$LH_PTR_CHECK     - check heap entry referenced by its pointer
;
;Purpose:
; From the pointer given return the heap entry data pointer. Also check the
; heap entry by matching its length and backlength values and confirming that
; the length is nonzero.
;
;Inputs:
; SI = specified pointer
;
;Outputs:
;
; SI = heap entry header pointer
;
;Modifies:
; None.
;
;Exceptions:
; If heap entry is inconsistent, jump to B$ERR_SSC.
;****

B$LH_PTR_FROM_DATA:
	SUB	SI,2		;now point to entry backlength
LH_PTR_FROM_BLEN:
	ADD	SI,[SI] 	;point to one past header entry
	DEC	SI		;point to header entry
B$LH_PTR_CHECK:
	PUSH	AX		;save registers...
	PUSH	DI
	MOV	AX,[SI].LHLEN	;get length of heap entry
	MOV	DI,SI		;compute offset of entry backlength...
	SUB	DI,AX		;by moving the pointer to before backlength
	CMP	[DI+1],AX	;compare entry length and backlength
	JNE	LH_PTR_ERROR	;if not equal, then report error
	OR	AX,AX		;test for illegal zero value
	JZ	LH_PTR_ERROR	;if so, then jump
	POP	DI		;restore registers...
	POP	AX
	RET			;return to caller

LH_PTR_ERROR:
	PUSH	ES		;force DS = basic dgroup in case
	POP	DS		;we are unwinding chain
assumes	DS,DGROUP		
	DbHalt NH_TEXT,<B$LH_PTR_CHECK found inconsistent heap entry>
	JMP	B$ERR_SSC	;note heap is inconsistent if ID_RELEASE version

;***
; B$LHSetFree - set free heap entry pointer
; Purpose:
;	Determines if the current free heap entry is both the last
;	heap entry and unallocated.  If so, its current value is
;	returned in DI.  Otherwise, the free heap entry pointer
;	[b$HEAP_FREE] is set to the heap space end and its value returned
;	in DI.
;
; Inputs:
;	None.
; Outputs:
;	DI = pointer to free heap entry [b$HEAP_FREE].
; Modifies:
;	AX.
; Exceptions:
;	None.
;****
B$LHSetFree	PROC	NEAR
	MOV	DI,[b$HEAP_FREE] ;point to free heap entry
	CMP	[DI].LHTYPE,LOW LH_FREE ;test if entry unallocated
	JNE	LH_SET_END	;if allocated, then jump
	MOV	AX,DI		;get pointer to free entry
	SUB	AX,[DI].LHLEN	;point to entry after free one
	CMP	AX,[b$HEAP_END] ;test if free entry was last in space
	JE	LH_SET_RETURN	;if so, jump leaving pointer unchanged
LH_SET_END:
	MOV	DI,[b$HEAP_END] ;get pointer to heap end
	CMP	DI,[b$HEAP_FIRST] ; Empty heap?
	JZ	LH_SET_FREE	; then don't try anything fancy
	MOV	AX,DI		; [AX] = pointer to end entry
	ADD	AX,[DI+1]	; [AX] = pointer to entry before end
	XCHG	AX,DI		; [AX] = ptr to end, [DI] = entry before
	CMP	[DI].LHTYPE,LOW LH_FREE ; test if entry after unallocated
	JZ	LH_SET_FREE	; if so, that's the free entry
	XCHG	AX,DI		; else use end entry
LH_SET_FREE:
	MOV	[b$HEAP_FREE],DI ;and set the free pointer to it
LH_SET_RETURN:
	RET			;return with DI pointing to free entry
B$LHSetFree	ENDP

	SUBTTL	B$NHINIT - Initialize dynamic space
	PAGE
;***
; B$NHINIT - Initialize dynamic space
; Purpose:
;	Initialize the necessary pointers and structures for string and
;	local heap space.
; Inputs:
;	AX = first word of dynamic space.
;	CX = last word of dynamic space.
; Outputs:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;****
cProc	B$NHINIT,<PUBLIC,NEAR>,<AX,BX,CX,SI,DI>	
cBegin					

	MOV	SI,CX			;get last word of dynamic space
	INC	SI			;point to last byte of space
	MOV	[b$HEAP_FIRST],SI	;the local heap starts here...
	MOV	[b$HEAP_FREE],SI	;has its free entry here...
	MOV	[b$HEAP_END],SI 	;and ends here
	MOV	[SI].LHTYPE,LOW LH_END	;set the heap entry type

	ADD	AX,LH_STD_HDR_LEN	; dyn space starts after var heap
	MOV	[b$NH_first],AX	
	MOV	BX,AX			
	DEC	BX			; point to last byte of var heap
	MOV	[b$HEAP_FIRST_SWAP],BX	; the variable heap starts here...
	MOV	[b$HEAP_FREE_SWAP],BX	; has its free entry here...
	MOV	[b$HEAP_END_SWAP],BX	; and ends here
	MOV	[BX].LHTYPE,LOW LH_END	; set heap entry type
	;Set up pointers to routine to be called when B$ILHALC needs to
	;  grow the appropriate heap
	MOV	[P_HEAP_GROW_SWAP],NH_TEXTOFFSET B$VAR_ALC_GROW ;[ln]
	MOV	[b$P_HEAP_GROW],NH_TEXTOFFSET LH_ALC_GROW	;[ln]
	SUB	SI,LH_STD_HDR_LEN+1	;point to even address before entry
	CMP	b$Clearing,0		;are we doing a CLEAR statement?
	JZ	NotClearing		;no, go initialize string space
	CALL	B$SSClean		; clean out entries, so we don't
	JMP	SHORT InitExit		; bash function keys
NotClearing:				
	CALL	B$STINIT		;initialize string space
InitExit:				

cEnd					

sEnd	NH_TEXT
	END
