	TITLE	LLQUE - Queue Management
;***
; LLQUE - Queue Management
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

	INCLUDE switch.inc	;switch file [new]
	INCLUDE rmacros.inc	;useseg macro

	USESEG	RT_TEXT 	;core runtime segment
	USESEG	_DATA		

	INCLUDE seg.inc 	;segment definitions
	INCLUDE oscalls.inc	;Dos 5 structures
	INCLUDE ibmunv.inc
	INCLUDE intmac.inc
	INCLUDE idmac.inc	


	externFP	B$ULDataRelease ;dealloc UL data images
	externFP	CompressHelp	


	externFP	B$ERR_MEM	;arena destroyed error
	externNP	B$ERR_FHC	;bad memory block address

;***
;b$pFHLowerTop - vector for Memory Management
;OEM-interface routine
;
;Purpose:
;	Move the top of the Far Heap (Basic data not in DGROUP) down
;	if at all possible so that there is more memory available
;	for DOS allocation.  This is used to try to free enough space
;	for B$FHHighAlloc.  If the far heap cannot be move to free up
;	the amount of memory requested, it releases the maximum amount
;	of memory that it can.
;
;	NOTE: b$pFHLowerTop is a variable through which the routine
;	      is indirectly called.
;
;Entry:
;	AX = number of paragraphs of memory to release.
;
;Exit:
;	AX = number of paragraphs of memory actually released.
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	Fatal error due to far heap inconsistency.
;
;Indirection Information:
;	The indirection variable b$pFHLowerTop is allocated in the _DATA
;	segment by the OEM.  It should be statically initialized to a
;	RET statement.	Once set, the OEM should not modify this variable.
;	If a Far Heap exist, the OEM-Independent code will change the
;	variable so that it points to a routine which performs the
;	indicated function.
;
;****
sBegin	_DATA				
	globalW b$pFHLowerTop,Near_Ret,1 ;vector for FHLowerTop
sEnd	_DATA				






sBegin	RT_TEXT 		
	assumes CS,RT_TEXT	


;***
;B$INIQUE
;
;PURPOSE:
;	This routine initializes the top and bottom pointers
;	of the queue. It also initializes the number
;	of notes and the number of bytes in the queue to 0.
;
;ENTRY:
;	[BX] = addr of music queue ctrl block.
;	[AX] = addr of music queue
;
;EXIT:
;	None
;
;MODIFIED:
;	None
;
;****
cProc	B$INIQUE,<PUBLIC,NEAR>	
cBegin				
	MOV	[BX].QUEGET,AX	;reset pointers to
	MOV	[BX].QUEPUT,AX	;beginning of queue buffer
	MOV	[BX].QUENUM,0	;clear queue counter
	MOV	[BX].QUNOTE,0	;clear # of notes in queue
Near_Ret:			;Near return for vector
cEnd				; End of B$INIQUE

;***
;B$QUE
;
;PURPOSE:
;	This routine queues in a byte contained in [AL] into
;	the appropriate music control queue. Address of the music
;	control queue is contained in [BX].
;
;ENTRY:
;	[AL] = Byte to be queued
;	[BX] = Address of appropriate music queue
;	[ES] = either [DS] or $COMBUF
;
;EXIT:
;
;MODIFIED:
;	QUENUM, QUEPUT, QUEGET
;
;****
cProc	B$QUE,<PUBLIC,NEAR>,<SI> 
cBegin				
	MOV	SI,[BX].QUEPUT	;get put PTR
	MOV	ES:[SI],AL	;queue in the byte
	INC	SI		;advance put PTR
	CMP	SI,[BX].QUETOP	;wrap around ?
	JNZ	NWRQUE		;No
	MOV	SI,[BX].QUEBOT	;get queue bottom
NWRQUE:
	MOV	[BX].QUEPUT,SI	;store new put PTR
	INC	[BX].QUENUM
cEnd				; End of B$QUE

;***
;B$DQUE
;
;PURPOSE:
;	This routine supports GETSND by dequeueing one byte from
;	the music queue and updating the associated variables.
;
;ENTRY:
;	[BX] = Adrress of the music queue
;	[ES] = either [DS] or $COMBUF
;
;EXIT:
;	[AL] = dequeued byte
;
;MODIFIED:
;	QUENUM, QUEGET, QUEPUT
;
;****
cProc	B$DQUE,<PUBLIC,NEAR>,<SI>	
cBegin				
	MOV	SI,[BX].QUEGET	;fetch get PTR
	MOV	AL,ES:[SI]	;get the byte
	INC	SI
	CMP	SI,[BX].QUETOP	;wraparound?
	JNZ	NWRDQU		;Brif not
	MOV	SI,[BX].QUEBOT	;get queue bottom PTR
NWRDQU:
	MOV	[BX].QUEGET,SI	;store new get PTR
	DEC	[BX].QUENUM	;dec # of bytes in queue
cEnd				; End of B$DQUE



;
;	(Moved here from fhinit with revision [11].)
;
;***
;B$FHHighAlloc - Allocate a block of memory outside of BASIC's normal access
;OEM-Interface Routine
;
;Purpose:
;	Allocate a block of memory.  This is done by calling the
;	Operating System, thus the memory returned will not be
;	in DGROUP and will not be under the control of the rest
;	of BASICS memory management routines. The far and near
;	heaps may be moved down if necessary to make adequate room.
;
;	The size of the block of memory will be rounded up to the nearest
;	word for OS/2 and to the nearest paragraph for DOS 3.
;
;	Note that the memory will only be deallocated by an explicit call
;	to B$FHHighDealloc.
;
;Entry:
;	DX:AX = Size of block to allocate.
;
;Exit:
;	CX = segment of the memory block allocated or 0 if memory
;	     could not be allocated.
;	     The segment will be an SB (not a physical segment) if FV_SBPTR.
;Uses:
;	Per convention.
;
;Preserves:
;	AX, DX
;
;Exceptions:
;	None.
;******************************************************************************


cProc	B$FHHighAlloc,<NEAR,PUBLIC>,<AX,DX,SI>	
cBegin

;	Convert allocation size to paragraphs in DX:AX.

	ADD	AX,0FH		; Round up to nearest paragraph
	ADC	DL,0		
	MOV	CX,4		; Divide by 32 (shift right 4 times)
CvrtLoop:
	SHR	DX,1		
	RCR	AX,1		
	LOOP	CvrtLoop	
	OR	DX,DX		;test if allocation over 1 megabyte
	JNZ	FHHighError	;if so, then immediate error

;	Try to allocate block from any available memory first.

	MOV	SI,AX		;save paragraph size
	CALL	FHMemAllocCall	;call to perform INT and handle fatal errors
	JNC	FHHighDone	;if successful, use this block, jump

;	Lower the top of the far heap and try to allocate the block
;	using the released memory.  The call to FHLowerTop may not
;	release all the memory requested, but try the allocation
;	anyway in case enough is there.

	CALL	FHMemAllocCall_Lower  ; CY clear if memory allocated

	JNC	FHHighDone	;if successful, use allocated block

	CALL	B$ULDataRelease ;release UL data images (if present)
	CALL	FHMemAllocCall_Lower  ; CY clear if memory allocated
	JNC	FHHighDone	;if successful, use allocated block

	CALL	CompressHelp	;release UL data images (if present)
	CALL	FHMemAllocCall_Lower  ; CY clear if memory allocated

	JNC	FHHighDone	;if successful, return

FHHighError:
	XOR	AX,AX		;return 0 for error

FHHighDone:
	XCHG	CX,AX		;block starts at far pointer CX:0
cEnd

;
;	(Moved here from fhinit with revision [11].)
;
;***
; B$FHHighDealloc - deallocate high memory allocated with B$FHHighAlloc
;OEM-Interface Routine
;
;Purpose:
;	To deallocate a block of memory outside the runtime heap
;	management at the top of memory.  The block must have been
;	allocated by B$FHHighAlloc.  There is no check that the
;	segment was ever allocated or that multiple deallocations
;	on the same segment are being attempted.
;
;Entry:
;	AX = segment of block to be deallocated.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;******************************************************************************


cProc	B$FHHighDealloc,<NEAR,PUBLIC>,<AX,ES>
cBegin

;	Deallocate the block through a DOS call.

	MOV	ES,AX		;put memory segment in for call
	MOV	AH,49H		;DOS function to deallocate a memory block
	CALL	FHMemDosCall	;call to perform INT and handle fatal errors

cEnd

;***
; FHZeroEntry - Zero fill a DOS 5 allocation (DOS 5)
;
;Purpose:
;	Fill a DOS 5 memory allocation with zeros.
;	(Moved here from fhinit with revision [11].)
;Entry:
;	DX:AX = 32 Bit size of block in bytes.
;	CX = segment of the memory block allocated.
;Exit:
;	None.
;Uses:
;	AX,CX,SI
;Exceptions:
;	None.
;******************************************************************************


; Added with [22] for size
;***
; FHMemAllocCall_Lower - perform memory allocation DOS call (DOS 3)
;
;Purpose:
;	Performs the INT21H for the DOS allocate call, after requesting the
;	far heap lower the top of memory.  Error checking is done here to
;	save space.
;
;Entry:
;	SI = # paragraphs of memory to allocate
;
;Exit:
;	Post-INT 21H register changes.
;
;Exceptions:
;	B$ERR_MEM if arena is trashed.
;	B$ERR_FHC if bad memory block address.
;****
cProc	FHMemAllocCall_Lower,<NEAR>
cBegin
	MOV	AX,SI		;AX = size of memory requested
	INC	AX		;add one for DOS memory header
	CALL	[b$pFHLowerTop] ;try to lower the top of the far heap
cEnd	<nogen> 		;Fall into FHMemAllocCall

; Added with [22] for size
;***
; FHMemAllocCall - perform memory allocation DOS call (DOS 3)
;
;Purpose:
;	Performs the INT21H for the DOS allocate call.	Error checking is
;	done here to save space.
;
;Entry:
;	SI = # paragraphs of memory to allocate
;
;Exit:
;	Post-INT 21H register changes.
;
;Exceptions:
;	B$ERR_MEM if arena is trashed.
;	B$ERR_FHC if bad memory block address.
;****
cProc	FHMemAllocCall,<NEAR>
cBegin
	MOV	BX,SI		;BX = size of memory request
	MOV	AH,48H		;DOS function to allocate memory block
cEnd	<nogen> 		;Fall into FHMemDosCall

;***
; FHMemDosCall - perform memory DOS call (DOS 3)
;
;Purpose:
;	Performs the INT 21H for the DOS allocate, deallocate, and
;	reallocate calls.  Error checking is done here to save space.
;	(Copied here from fhinit with revision [11].)
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

cProc	FHMemDosCall,<NEAR>
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

sEnd	RT_TEXT 		
	END
