;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1988-1991
;
;   Title:	instance.inc -
;
;   Version:	1.00
;
;   Date:	19-Jul-1988
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   19-Jul-1988 RAL Original
;
;==============================================================================

;
; Defines the size of the instance owner array. The MAX allowed instance page
;    number = MAX_INST_PAGES - 1
;
MAX_INST_PAGES		equ	100h
MIN_LOADHI_INST_PAGE    equ	0a0h

;
;   This structure defines a single instance field in a particular instance
;   page.  The Inst_Buf_Offset+VM.CB_Inst_Buf_Offset points to the instance
;   buffer area for a given VM.  VM_Address is the linear address in the
;   VM address space of the item.  The Lead_Byte_Count specifies a count at
;   the start of the item which is byte aligned and must be moved with a
;   movsb instruction.	After this move, both the VM address and instance
;   buffer address will be dword aligned (this field may be 0).  The Field_
;   Length specifies the remaining number of BYTES to move starting at the
;   dword aligned addresses.  Note that this may not be a multiple of four,
;   the Length MOD 4 specifies an odd byte area at the end that must be moved
;   a byte at a time (this field may be 0).  The Flags are currently unused.
;
LoadHi_Instance_Map_Struc  STRUC
IMap_Inst_Buf_Offset	dd	?		; Offset to VM's instance buffer
IMap_VM_Address 	dd	?		; Linear VM address of inst data
IMap_Field_Length	dw	?		; DWord aligned field length
IMap_Lead_Byte_Count	db	?		; Initial length to dword align
IMap_Flags		db	?		; special flags
LoadHi_Instance_Map_Struc  ENDS

;
; This bit being set means that the IMap_VM_Address field is an address in
;   the PhysLinear region instead of in the VM since wrap pages are not
;   necessarily mapped in the VM when instance swaps occur on them.
;
IMap_F_Wrap_Pg		equ	00000001B	; Is INSTANCE page in wrap area
IMap_F_Wrap_PgBit	equ	0
