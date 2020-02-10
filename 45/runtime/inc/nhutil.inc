;***
; NHUTIL.INC - 15-May-86 - Near Heap structures and definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Near heap structures and definitions.
;
;******************************************************************************

;	Heap Entry Header format:

LHTYPE	EQU	 0		;local heap block type
LHFNUM	EQU	-1		;local heap file number (for FILE block)
LHLEN	EQU	-3		;local heap block length (including header)
LHBAKP	EQU	-5		;local heap backpointer (pointer to owner)
LHPOFF	EQU	-7		;local heap sd offset (for FILE blocks only)
LHPLEN	EQU	-9		;local heap sd length (for FILE blocks only)

LH_STD_HDR_LEN EQU	 6	;local heap standard header size
LH_FDB_HDR_LEN EQU	10	;local heap fdb header size (includes 4 bytes
				;	of sd for fielded strings)


;	Heap entry type-byte constants (LHTYPE, above):

	LH_FREE 	EQU	01h	;free entry
	LH_ARRAY	EQU	02h	;dynamic string array in entry
	LH_END		EQU	04h	;last block in heap
	LH_FILE 	EQU	08h	;FDB (field buffer) in entry
	;the type byte is split into two nibbles; the low nibble contains only
	;  the above general-purpose type constants. The high nibble is
	;  reserved for the interpreter. If the high bit (bit 7) is set,
	;  the entry is an interpreter entry. If any of bits 4, 5, or 6 are set
	;  then we must call back to the interpreter for it to adjust 
	;  backpointers to any owners in the entry.
	LH_IM_ENTRY	EQU	080h	;mask to see if interp. entry
	LH_I_NO_OWNERS	EQU	080h	;entry contains no owners
	LH_I_IN_USE_1	EQU	090h	;used by interpreter
	LH_I_IN_USE_2	EQU	0A0h	;used by interpreter
	LH_I_IN_USE_3	EQU	0B0h	;used by interpreter
	LH_I_IN_USE_4	EQU	0C0h	;used by interpreter
	LH_I_IN_USE_5	EQU	0E0h	;used by interpreter
	LH_I_IN_USE_6	EQU	0F0h	;used by interpreter
	LH_IM_CALL_BACK EQU	070h	;mask of three bits; if any of
					;  these three bits set, must call back
					;  to interpreter to update

	.LIST
