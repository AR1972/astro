;*** 
; varmgr.inc - structure offsets and other constants for the VarMgr
;
;	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
;
;
;*******************************************************************************
	IncludeOnce	heap

VARMGR_INC = ON

;VARPTR_D Structure- VARPTR$ descriptor definition
VARPTR_D	STRUC
VARPTR_D_oTyp	DW 0
VARPTR_D_pValue	DD 0
VARPTR_D	ENDS

;====================  Variable Table =====================================
;The variable table contains binding information for each variable or 
;function procedure. In many cases, the variable table also contains the value.
;
;There are two logical kinds of variable table. There is one tMV (Module 
;variable table) per module, and it contains module static variables, all 
;COMMON variables, and procedure reference entries.  There is one tPV 
;(Procedure variable table) per procedure, and it contains the formals, 
;dynamics, and an entry for the function return value if it is a function.
;Both logical table types reside in the same physical table; there is one
;of these per module.
;
;These tables are constructed at parse time, or by the rude scanner. They are
;considered to be built at SS_PARSE and not built as SS_RUDE.
;
;Since these tables can be built at parse time, there may be tPV entries for
;variables which are later declared to be functions. This case is handled via
;special code in the varmgr itself (old entries are redirected).
;
;The hash functions for both the tMV and the tPV is a hash on the Name
;Table offset (oNam). The hash tables for the tMV and tPV will be of
;different size.
;
;It is necessary to detect from the variable table which procedure owns each
;variable in the tMV.  This is done using oPrs.  This could be a byte entry
;if each procedure were given a unique byte size identifier.  oPrs for the
;main level is UNDEFINED.  At table build time, variables must be unique by 
;oNam, oPrs, and oTyp.
;============================================================================

; sizes of variable hash tables
CBINITMVHASH	EQU 32d		;size of module hash table for var table
CBINITPVHASH	EQU 16d		;size of a procedure hash table for var table

HASH_MV_NAMMASK	EQU 001EH	;a 32-byte table contains 16 2-byte offsets,
				;each to the start of a unique hash chain.
				;001E in binary is 0000000000011110, i.e.,
				;it is used to mask off all but bits 1,2,3,
				;& 4. When this mask is AND'd with an oNam, 
				;it produces an even hash value (hash table
				;offset) from 0 to 30
HASH_PV_NAMMASK	EQU 000EH	;a 16-byte table contains 8 2-byte offsets,
				;each to the start of a unique hash chain.
				;000E in binary is 0000000000001110, i.e.,
				;it is used to mask off all but bits 1,2,
				;& 3. When this mask is AND'd with an oNam, 
				;it produces an even hash value (hash table
				;offset) from 0 to 14

OMVHASH EQU	0		; module hash table still starts at offset
				; 0 in var table for QB5, at least for now

;AFRAME Structure -	value part of an array frame variable
AFRAME		STRUC
AFRAME_oFrame	DW 0
AFRAME_cDims	DB 0
AFRAME_filler1	DB 0
AFRAME		ENDS

;AFORMAL Structure -	value part of an array formal variable
AFORMAL		STRUC
AFORMAL_oFrame	DW 0
AFORMAL_cDims	DB 0
AFORMAL_filler1	DB 0
AFORMAL		ENDS

IncludeOnce	array	;to get AD struct

;ASTAT Structure -	value part of a static (not frame) array variable
ASTAT		STRUC
ASTAT_filler0	dw 0
ASTAT_cDims	DB 0
ASTAT_filler1	DB 0
ASTAT_ad	DB SIZE AD DUP(0)
ASTAT		ENDS

.errnz	AFRAME_cDims - AFORMAL_cDims
.errnz	AFRAME_cDims - ASTAT_cDims

CDIMS_DEFAULT	EQU	8	;default (assumed) number of dimensions in
				;  an array.


;VAR Structure - variable table entry
;Note that the oTyp field is only present when the low 3 or 4 bits in the flags
;	field are not all zero. A pointer to this structure points to the
;	value field.
VAR_oMrsTyp	EQU -10 	; only for PUBLIC's of user-defined type
VAR_oNamForm	EQU -8		; used for form/menu variables only
VAR_cbFixed	EQU -8
VAR_oTyp	EQU -8
VAR_oNam	EQU -6
VAR_oHashLink	EQU -4
VAR_flags	EQU -2
VAR_value	EQU 0

VAR_fStat EQU	VAR_oHashLink	;flag FV_STATIC stored in low bit of oHashLink

VAR_STRUCT_SIZE EQU 6		;size of the size-invarient part of var entry

;Macro for getting oTyp
;Arguments:
;	1.  16-bit register for oTyp
;	2.  Address expression which evaluates to pVar
;
;For example, a typical invocation would be
;	GetOtyp	ax,[bx]		;bx points to variable table entry
;
GetOtyp	Macro	Reg,pVar
	local	HavOtyp
	mov	Reg,pVar+VAR_flags
	and	Reg,FV_TYP_MASK
	jnz	HavOtyp
	mov	Reg,pVar+VAR_oTyp
HavOtyp:
	endm

;Text macro for accessing a var entry
;Assumes that in FV_LONGPTR case, var table is a far item, and
;	ES is set to the var table segment
PTRVAR	EQU	word ptr 
BPTRVAR	EQU	byte ptr 


;================ Variable Manager Interface Definitions =================
;
;The following constants, types, and procedures define the interface
;to the variable manager.
;=========================================================================

;Flag constants used with MKVAR_flags2, below
MV_fConstFound	EQU	01H
MV_fONamInOTyp	EQU	02H
MV_fDontCreate	EQU	04H
MV_fRecRef	EQU	08H

;MKVAR - global structure used as input to MakeVariable():
MKVAR_TYPE		STRUC
MKVAR_flags		DW 0
MKVAR_exitFlags		DW 0
MKVAR_oNam		DW 0
MKVAR_oTyp		DW 0
MKVAR_flags2		DB 0
MKVAR_cDimensions	DB 0
MKVAR_fsLength		DW 0
MKVAR_oMrsTyp		DW 0
MKVAR_TYPE		ENDS

MKVAR_oNamForm	EQU	MKVAR_fsLength	; used for form/menu variables only

;error codes returned by MakeVariable for the parser -
;two sets are defined, one for the parser (bits toward the high end
;of the word) and one for the scanner (toward the lower end of the word).
;Note that callers can count on the scanner error bits being in the
;low byte and the parser bits + 08000H in the high byte if the depending
;code does a .errnz SCN_ER_MASK - 0FFH
PRS_ER_RE	EQU 8100H	;Rude Edit -     bits 8  & 15 set
PRS_ER_RP	EQU 8200H	;Reparse -       bits 9  & 15 set

SCN_ER_MASK	EQU 0FFH	;other scanner error constants are ER_ and
				;MSG_ errors from qbimsgs.inc/.h

;====================  Type Table ========================================
;The type table is used to manage user defined variable types.
;
;There is one type table per module.  The scanner verifies type compatibility
;between modules for COMMON variables and procedure parameters.
;
;Type and element entries are maintained in the same table.
;
;The word at offset oOFIRST_TYP in the type/var table (mrsCur.bdVar) is an 
;offset to the first type in a chain of types that comprise the table. Elements
;are chained from their parent types. Following this word in table, at offset
;oCTYPES, is a count of types currently defined in the table.
;============================================================================
CTYPESMAX 	EQU 240d	;Maximum number of user-defined types 
				;allowed in a module

oOFIRST_TYP	EQU CBINITMVHASH + OMVHASH
				;the offset to the first TYP structure in the
				;var/type table is stored in the table
				;immediately following the module hash table
oCTYPES		EQU oOFIRST_TYP + 2
				;the count of TYP's in the var/type table is
				;stored immediately following the offset to the
				;first TYP.
CBINIT_TTYP	EQU 4		;number of words of overhead storage required
				;by typmgr to be allocated in MRS_bdVar right
				;after the module heash table.

;TYP Structure - type table type entry
TYP	STRUC
TYP_oNam		DW 0
TYP_oTypNext		DW 0
TYP_cbData		DW 0
TYP_oElementFirst	DW 0			;Low 15 bits only
TYP	ENDS

TYP_fReferenced		EQU TYP_oElementFirst+1	;high-bit of oElementFirst
						;used to indicate if element
						;has been referenced. This
						;constant allows easy access
						;to the high byte of this field
TYP_flags		EQU TYP_oTypNext+1	; high-bit of oTypNext used
						; as a flag
F_NOBLKCPYTYP		EQU 080H		; flag to use with TPY_flags
						; If set, means that code
						; that assigns one user-
						; defined type to another
						; cannot do so via a
						; simple block copy

;ELEM Structure - type table element entry
ELEM	STRUC
ELEM_oNam		DW 0
ELEM_oElementNext	DW 0
ELEM_oTyp		DW 0
ELEM_oVar		DW 0
ELEM	ENDS

ELEM_cbFixed	equ	ELEM_oVar + 2		;this field is only present
						;for fixed-length string/text
						;elements


;======================================================================
; External declarations for varmgr.c & vardebug.c routines
;======================================================================
sBegin	DATA
	EXTRN	mkVar:byte
ife VARUTIL_ASM
	EXTRN	oTypComMax:word 	;max oTyp of Blank Common for all MRSs
	EXTRN	oValComMax:word 	;max oVal of Blank Common for all MRSs
endif	;VARUTIL_ASM
sEnd	DATA


	IncludeOnce types	;this part split out so some modules don't
				;  need to include all of this file
