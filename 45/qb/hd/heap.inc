;*** 
;heap.inc
;
;	Copyright <C> 1986, 1987, 1988 Microsoft Corporation
;
;*******************************************************************************
; NOTE: When making changes to this file, be sure to make equivalent
;	changes to file HEAP.H

HEAP_INC = ON
	IncludeOnce	pointers	

;======================= bd ==========================================
;
;buffer descriptor structure.
;
;   Notice that this is a superset of a string-descriptor
;   structure, which means that some functions which expect
;   string descriptors can take buffer descriptors as input.
;
;Examples of  (bd) structures  include the  prs table, mrs
;table, value  tables and  name tables.
;
;=======================================================================*

bd	STRUC
BD_cbLogical		DW 0	;logical size of content part of
				;heap entry.  This is the size as requested by
				;the last call to BdAlloc or BdRealloc.  There
				;may be additional padding space at the end.
BD_pb			DW 0	;points to 1st byte of content part
				;of heap entry.  This pointer is updated when
				;heap entry is moved.
BD_cbPhysical		DW 0	;actual size of heap entry, including empty
				;space at the end.  Always >= cbLogical
bd	ENDS


;======================= bdp ==========================================
;
;buffer descriptor with pointer structure
;Notice that this is a superset of a (bd) structure,
;which means that some functions which expect (bd)'s as
;input can take (bdp)'s as input.
;Examples of (bdp) structures include  the buffer  used to pass the parser a
;source line and the  buffer the parser returns pcode in.
;
;=======================================================================*

bdp	STRUC
BDP_cbLogical		DW 0	;logical size of content part of
				;heap entry.  This is the size as requested by
				;the last call to BdAlloc or BdRealloc.  There
				;may be additional padding space at the end.
BDP_pb			DW 0	;points to 1st byte of content part
				;of heap entry.  This pointer is updated when
				;heap entry is moved.
BDP_cbPhysical		DW 0	;actual size of heap entry, including empty
				;space at the end.  Always >= cbLogical

; Note: IF FV_LMEM, BDP_pbCur is used as an offset from the start of the
; Note: block rather than a pointer into the block.	The pointer needed
; Note: can always be calculated by adding this offset to the starting
; Note: address of the block.  This removes the need to update this value
; Note: when the heap entry is moved.

BDP_pbCur		DW 0	;points to current byte within entry
				;This pointer is updated when heap entry is
				;moved.
bdp	ENDS


;======================= bdl ==========================================
;
;Large far buffer descriptor structure.
;Examples of bdl structures include text and name tables.
;
;=======================================================================*

IncludeOnce	array		;get FHD structure definition

NOT_OWNER	EQU	UNDEFINED	;used to test the BDL_status field

bdl	STRUC
BDL_cbLogical	DW	?	;maintained by bdmgr, ignored by FH manager.
BDL_seg 	DW	NOT_OWNER ; interpreter name for segment address
				;   or sb, depending on version.
BDL_dummy	DW	?	; required due to overlaying FHD struct
				; over the bdl struct
BDL_cPhysical	DW	?	;cPara is only maintained by FH manager under
				;DOS 3 or less; under DOS 5, bdmgr.asm maintains
				;this field as a byte count

bdl	ENDS

BDL_status	EQU	BDL_seg ; NOT_OWNER is a value that is known to
				; be an illegal value in the seg field for
				; any version. We access this same field via
				; the name 'status' to more clearly document
				; our intent.  It is assumed that this field
				; contains NOT_OWNER in a bdl that does not
				; own a far block.

; ensure that the BDL structure properly corresponds to the FHD structure
.errnz BDL_cbLogical - FHD_oData
.errnz BDL_seg 	- FHD_hData
.errnz BDL_dummy - FHD_pNext	
.errnz BDL_cPhysical - FHD_cPara
.errnz SIZE BDL - SIZE FHD

;***
;PUSHBDL_TYPE
;
;Purpose:
;	Push a word constant indicating type of bdl for EB versions
;	('type' is something like 'pcode table', 'name table', etc.)
;Entry:
;	bdlType is a constant to be passed to BdlAlloc[Sb] for EB versions.
;	reg is the helper register used to push the value (not needed in
;		versions which support the push immediate instruction)
;Modifies:
;	Given reg only
;**************************************************************************
PUSHBDL_TYPE	MACRO	bdlType,reg ;
	ENDM			;


;**===================================================================
;
;Constants and structures related to entries in the Interpreter Heap
;
;=======================================================================**

;Types of Interpreter Heap entries
;NOTE: These constants are (to some degree) known and shared by the heap
;	manager code maintained by the runtime group (currently in devdef.inc).
;	They CANNOT be modified without understanding the ramifications to
;	the heap management code (currently in nhlhutil.asm).
;	The current definition is that if bits 5 & 6 are set in the type byte,
;	the heap manager must itself do the work to update backpointers to
;	any owners in an entry being moved, else, if any of bits 4, 5, or 6
;	are set, B$IHeapEntryMoved will be called (in our bdmgr.asm) to
;	do the updating. If bit 7 is set, the heap manager knows that the
;	entry is interpreter-specific. Basically, we've got the high 4-bits,
;	and the low 4-bits are for general purpose types.
IT_M_INTERP		EQU	080h	;mask that says "interpreter entry"
IT_NO_OWNERS		EQU	080h	;Heap entry which contains no owners.
					;Entry is owned by a bd structure.
					;Can be moved w/o examining content
IT_NO_OWNERS_BDP	EQU	090h	;Same as IT_NO_OWNERS, but owned by a
					;bdp structure instead of a bd, so when
					;moved, bdp.pbCur field must be updated.
IT_MRS			EQU	0A0h	;module register set table
IT_PRS			EQU	0B0h	;procedure register set table
IT_COMMON_BLOCK 	EQU	0C0h	;table of owners of common value tables
IT_COMMON_VALUE 	EQU	0E0h	;packed table of common values
IT_VAR			EQU	0F0h	;module variable table
IT_STRARYDESC		EQU	002h	;same constant used in common runtime ..

;============================================================================
;the following EXTRN's are included in ALL files EXCEPT bdmgr.asm
;============================================================================


ife	BDMGR_ASM

	EXTRN	BdAlloc:far
	EXTRN	BdAllocVar:far
	EXTRN	BdFree:far
	EXTRN	BdChgContents:far
	EXTRN	BdChgOwner:far
	EXTRN	BdChgOwner_NoCopy:far
	EXTRN	BdGrow:far
	EXTRN	BdGrowVar:far
	EXTRN	BdRealloc:far
	EXTRN	BdCheckFree:far
	EXTRN	BdShiftRight:far
	EXTRN	BdShiftLeft:far
	EXTRN	BdAppend:far
	EXTRN	BdAdjust:far
	EXTRN	BdCompressAll:far
	EXTRN	GrabSpace:far
	EXTRN	ReleaseSpace:far
	EXTRN	BdlAlloc:far
	EXTRN	BdlAllocSb:far		
	EXTRN	BdlRealloc:far
	EXTRN	BdlGrow:far		
	EXTRN	BdlFree:far
	EXTRN	BdlChgOwner:far
	EXTRN	BdlCheckFree:far
	EXTRN	BdlCopyFrom:far
	EXTRN	BdlCopyTo:far
	EXTRN	BdlCopyFromTo:far
	EXTRN	BdlTrim:far
	EXTRN	GrabSpace:far
	EXTRN	ReleaseSpace:far
endif	;BDMGR_ASM



;======================================================================
;bd manager non-RELEASE macros
;======================================================================

DbDmpLHHeap	MACRO
		ENDM
DbShiftLH	MACRO
		ENDM
DbHeapMoveOff	MACRO
		ENDM
DbHeapMoveOn	MACRO
		ENDM
DbSegMoveOff	MACRO			;
		ENDM			;
DbSegMoveOn	MACRO			;
		ENDM			;


;======================================================================
;memory manager interface constants
;======================================================================

GMEM_ZEROINIT = 040h
