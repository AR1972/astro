ifndef	INC_LIST
.xlist
endif
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1986-1991
; (C) Copyright COMPAQ Computer Corp. 1986-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;		EMMLIB.LIB - Expanded Memory Manager Library
;
; Module:	EMMFUNCT.INC - defines for EMM code.
;
; Version:	0.10
;
; Date:		June 21, 1986
;
; Author:	Steve Preston
;
;******************************************************************************
;
; Change log:
;
; DATE		REV	DESCRIPTION
; --------	------	-----------------------------------------------------
; 06/21/86	0.0
; 06/25/86	0.02	Changed TOTAL_HANDLES to 255 to match LIM spec (SBP).
; 06/28/86	0.02	Name change from CEMM386 to CEMM (SBP).
; 07/06/86	0.04	Changed save area struct (SBP).
; 01/01/89	0.10	Rewritten for LIM 4.0 (DJM).
;******************************************************************************
;******************************************************************************
; GENERAL DEFINES
;******************************************************************************
MAX_HANDLES		EQU	255	; total number of handles supported
MAX_REGISTER_SETS	EQU	255	; number of register sets
TOTAL_EMS_WINDOWS	EQU	48	; Maximum number of 4.0 windows
TOTAL_PF_WINDOWS	EQU	4	; total number of 3.2 windows
TOTAL_PAGE_TABLES	=	10	; total number of page tables
NUMBER_BASE_WINDOWS	=	16	; number of windows from 256k to 512k
FIRST_HANDLE_PTE	=	0C00h	; starting location for handles
TOTAL_HANDLE_PTES	=	1000h	; number of handle PTEs
PAGE_SIZE		=	1000h	; 4k page size
EMS_PAGE_SIZE		=	4000H	; 16k EMS pages
START_OF_ROM_MEMORY	=	0E000h
MAX_SIZE		=   (32 * 1024) ; max size of EMS memory pool
MS_DOS			=	21h	; DOS interrupts
PRINT_STRING		=	09h	; DOS print string
GET_VERSION		=	30h	; DOS get version
X_HI_MEM_SEG		=	0F000h	; segment for the following words
X_MT_AT 		=	0FFFEh	; Machine type
DOS3X_ADJ		=	1	; DOS 3.x base memory adjustment
MAX_ARG_LEN		=	5	; max command line argument length

OS_ENABLED_BIT		=	0	; bit index into OS_function variable
OS_KEY_OUT_BIT		=	1	; bit index into OS_function variable
SAVE_MAP_IN_USE_BIT	=	15	; bit index into the save_map array
PAGE_ALLOCATED_BIT	=	9	; bit index into a page table entry
PAGE_ALLOCATED_BIT_MASK =	0200h	; bit mask for above bit
VCPI_USAGE_BIT		=	10	; bit index into a page table entry
HANDLE_ALLOCATED_BIT	=	11	; bit index into a page table entry
HANDLE_ALLOCATED_BIT_SET=	800h	; bit index into a page table entry

TRUE			=	1
FALSE			=	0
FREE			=	-1	; for save_flag
EMS_STATUS_FUNCTION	=	40h
VCPI_FUNCTION_OPCODE	=	0DEh
NULL_PAGE		=	7FFFh	; defines a handle not in use
PTE_ADDRESS_BIT_MASK	=   0FFFFF000h	; bit mask for a page table entry
PTE_CONTROL_BIT_MASK	=   000000FFFh	; bit mask for a page table entry
EMM_VERSION		=	040h	; the current version
UNMAP_WINDOW_OPCODE	=	0FFFFh	; for unmapping a window
DOS_MASTER_VECTOR	=	08h	; normal DOS master 8259 vector
DOS_SLAVE_VECTOR	=	70h	; normal DOS slave 8259 vector
TSS_RESET_BUSY		equ	0FDh	; for turning off the TSS's busy bit

;******************************************************************************
; STATUS DEFINES
;******************************************************************************
OK			=	0
EMM_SW_MALFUNCTION	=	080h
EMM_HW_MALFUNCTION	=	081h
INVALID_HANDLE		=	083h
INVALID_FUNCTION	=	084h
NO_MORE_HANDLES 	=	085h
SAVED_PAGE_DEALLOC	=	086h
NOT_ENOUGH_EXT_MEM	=	087h
NOT_ENOUGH_FREE_MEM	=	088h
ZERO_PAGES		=	089h
LOG_PAGE_RANGE		=	08Ah
PHYS_PAGE_RANGE 	=	08Bh
SAVE_AREA_FULL		=	08Ch
MAP_PREV_SAVED		=	08Dh
NO_MAP_SAVED		=	08Eh
INVALID_SUBFUNCTION	=	08Fh
NOT_SUPPORTED		=	091h
SUCCESSFUL_OVERLAP	=	092h
INSUFFICIENT_MEMORY	=	093h
INVALID_OFFSET		=	095h
INVALID_REGION		=	096h
INVALID_OVERLAP 	=	097h
INVALID_SOURCE_TYPE	=	098h
UNSUPPORTED_REGISTER_SET=	09Ah
NO_FREE_REGISTER_SETS	=	09Bh
NO_ALT_DMA_REG_SETS	=	09Ch
NO_ALT_REG_SETS 	=	09Ch
INVALID_REGISTER_SET	=	09Dh
NO_DEDICATED_DMA	=	09Eh
NAME_NOT_FOUND		=	0A0h
INVALID_NAME		=	0A1h
WRAP_AT_1MB		=	0A2h
WINDOW_INDEX_RANGE	=	0A3h
ACCESS_DENIED		=	0A4h

;==============================================================================
;==  Flags used allocation/deallocation of memory
;==============================================================================
fEMSPageAllocatedBit	equ	9	; bit to indicate an EMS allocated page
fVCPIPageAllocatedBit	equ	10	; bit to indicate a VCPI allocated page
fXMSPageAllocatedBit	equ	11	; bit to indicate a page from XMS pool

fEMSPageAllocated	equ	1 shl fEMSPageAllocatedBit
fVCPIPageAllocated	equ	1 shl fVCPIPageAllocatedBit
fXMSPageAllocated	equ	1 shl fXMSPageAllocatedBit

fWINPageBit		equ	9	; bit to indicate page for WINdows
fWINPage		equ	1 shl fWINPageBit
;==============================================================================
;==  Flags used for Page4K[] array.
;==============================================================================
INUSEbit		equ 	0  	; for WinSrch: ROM and default eXclude
EXCLUDEbit		equ	1	; for user specified X= parameter
RAMbit			equ	2	; for RAM=mmmm-nnnn
EMSbit			equ	3	; for EMS=mmmm-nnnn
ROMbit			equ	4	; for ROM=mmmm-nnnn
WINbit			equ	5	; for WIN=mmmm-nnnn

INUSE			equ	1 shl INUSEbit
EXCLUDE			equ 	1 shl EXCLUDEbit
RAM			equ	1 shl RAMbit
EMS			equ	1 shl EMSbit
ROM			equ	1 shl ROMbit
WIN			equ	1 shl WINbit
;******************************************************************************
; VCPI STATUS DEFINES
;******************************************************************************
SUCCESS 		equ	000h	; return code for AH using 32-bit regs
FAILURE 		equ	0ffh	; return code for AH using 32-bit regs

D_PRES			equ	080h	; present in memory
D_DPL0			equ	0	; Ring 0
D_CTRL			equ	0	; Control descriptor
D_386INT_GATE		equ	0eh	; 386 interrupt gate
D_386INT0	equ	(D_PRES+D_DPL0+D_CTRL+D_386INT_GATE) ; Ring 0 int gate

;******************************************************************************
;	 S T R U C T U R E S
;******************************************************************************
;***********************************************************************
; EMSmap[]
;	This structure is used for holding info about the EMS windows.
;	The windows mapping consist of PTE entries.
;***********************************************************************
EMSmap_struc	struc
		dd	0
EMSmap_struc		ends

;***********************************************************************
; RegisterStack_struc[]
;	This structure is used for accessing the user's original registers.
;	There are 2 EBP locations which have different meanings depending
;	on how CEMM was called.  If CEMM was on (in protected mode) then
;	'stack_frame_EBP' points to the fault stack frame on the ring zero
;	stack and 'reg_EBP' is the user's EBP.  If CEMM was off (in real mode)
;	then 'stack_frame_EBP' is the user's original EBP and there is no
;	fault stack frame.
;***********************************************************************
RegisterStack_struc	struc
	reg_EDI 	dd	?
	reg_ESI 	dd	?
	stack_frame_EBP	dd	?
	reg_ESP 	dd	?
	reg_EBX 	dd	?
	reg_EDX 	dd	?
	reg_ECX 	dd	?
	reg_EAX 	dd	?
	ret_IP		dw	?
	ret_CS		dw	?
	reg_DS		dw	?
	reg_ES		dw	?
	reg_EBP 	dd	?
RegisterStack_struc	ends

;***********************************************************************
; register_set[]
;	This structure is used with the register_set array.  The
;	'active' fields specifies if this register set has been allocated
;	or not.  The 'saved_mapping' is the window mapping for this set.
;***********************************************************************
RegisterSet_struc	struc
	active		db	0
	RSrsvd		db	0
	a20_state	dw	0
	page_table_ptr	dd	0
RegisterSet_struc	ends

ifdef QEMS
RegisterSet_struc	struc
	active		db	0
	a20_state	dw	0
	page_table_ptr	dd	0
	saved_mapping	dw	0
			dd	TOTAL_EMS_WINDOWS dup (NULL_PAGE)
RegisterSet_struc	ends
endif
;***********************************************************************
; handle_table[]
;	This structure is used with the handle_table[] array for
;	holding handle information.  The 'base_PTE_index' is the PTE index
;	where the handle starts at.  The 'number_PTEs' is how many PTE's
;	are allocated to this handle.  The PTE's are always contiguous and
;	for each PTE, the address saved in it is allocated to that handle.
;	A value of NULL_PAGE in 'base_PTE_index' means the handle is free.
;***********************************************************************
HandleTable_struc	struc
	base_PTE_index	dw	?
	number_PTEs	dw	?
HandleTable_struc	ends

HandleName_struc	STRUC
			DB	8 DUP (0)
HandleName_struc	ENDS

HandleSaveMap_struc	STRUC
			dd	4 dup (0)
HandleSaveMap_struc	ENDS

ifdef QEMS
;***********************************************************************
; EMS_window[]
;	This structure is used for holding info about the EMS windows.
;	The windows mapping consist of its handle and logical page.
;***********************************************************************
EMS_window_struc	struc
	handle		DB	?
	logical_4k_page	dw	?
EMS_window_struc		ends
endif

;***********************************************************************
; MACROS
;	These macro's are needed due to a 80386 bug in versions prior to
;	the D step.  The problem is that the EDI register will not be
;	incremented correctly but only the 16 bit DI register is if an
;	address size override instruction does not follow.
;***********************************************************************
REP_MOVS_DWORD_USING_DS_ESI	macro
	rep movs	dword ptr es:[edi],dword ptr ds:[esi]
	db	67h
	nop
endm
REP_MOVS_WORD_USING_DS_ESI	macro
	rep	movs	word ptr es:[edi], word ptr ds:[esi]
	db	67h
	nop
endm
REP_MOVS_BYTE_USING_DS_ESI	macro
	rep	movs	BYTE ptr es:[edi], BYTE ptr ds:[esi]
	db	67h
	nop
endm
MOVS_DWORD_USING_DS_ESI	macro
	movs	dword ptr es:[edi],dword ptr ds:[esi]
	db	67h
	nop
endm
MOVS_WORD_USING_DS_ESI	macro
	movs	word ptr es:[edi],word ptr ds:[esi]
	db	67h
	nop
endm
REP_MOVS_DWORD_USING_ES_ESI	macro
	rep movs	dword ptr es:[edi],dword ptr es:[esi]
	db	67h
	nop
endm
MOVS_DWORD_USING_ES_ESI	macro
	movs	dword ptr es:[edi],dword ptr es:[esi]
	db	67h
	nop
endm
REP_MOVS_BYTE_USING_ES_ESI	macro
	rep movs	byte ptr es:[edi],byte ptr es:[esi]
	db	67h
	nop
endm
MOVS_BYTE_USING_ES_ESI	macro
	movs	byte ptr es:[edi],byte ptr es:[esi]
	db	67h
	nop
endm
STOS_BYTE_PTR_ES_EDI macro
	stos	byte ptr es:[edi]
	db	67h		; Address mode prefix
	nop
	endm
STOS_WORD_PTR_ES_EDI macro
	stos	word ptr es:[edi]
	db	67h		; Address mode prefix
	nop
	endm
STOS_DWORD_PTR_ES_EDI macro
	stos	dword ptr es:[edi]
	db	67h		; Address mode prefix
	nop
	endm
REP_STOS_DWORD_PTR_ES_EDI macro
	rep stos    dword ptr es:[edi]
	db	67h		; Address mode prefix
	nop
	endm
LODS_BYTE_PTR_ES_ESI macro
	lods	byte ptr es:[esi]
	db	67h		; Address mode prefix
	nop
	endm
LODS_WORD_PTR_ES_ESI macro
	lods	word ptr es:[esi]
	db	67h		; Address mode prefix
	nop
	endm
LODS_DWORD_PTR_ES_ESI macro
	lods	dword ptr es:[esi]
	db	67h		; Address mode prefix
	nop
	endm
.list
