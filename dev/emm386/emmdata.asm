.386p
page	58,132
;******************************************************************************
	title	EMMDATA - EMM data structures definitions
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1986-1991
; (C) Copyright COMPAQ Computer Corp. 1986-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;		EMMLIB.LIB - Expanded Memory Manager Functions Library
;
; Module:	EMMDATA
;
; Version:	0.04
;
; Date:		June 14,1986
;
; Author:	Phil Barrett.
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION                  DESCRIPTION
;   --------  --------  -------------------------------------------------------
;
;******************************************************************************
;
;   Functional Description:
;	data definitions for emm/lim
;
;******************************************************************************

page
;******************************************************************************
; PUBLICS
;******************************************************************************
PUBLIC	xma2ems				; An XMA2EMS compatible
public	total_handles
public	ttl_hndls
public	number_EMS_windows
public	EMS_window_location
public	EMSsegLoc
public	UMBptr
public	HMAptr
public	UMBHMA
public	hndl_tbl_ptr
public	hndl_nam_ptr
public	save_map_ptr
public	save_flag
public	OS_functions
public	OS_key
public	current_register_set
public	handle_count
public	cntWinPages
public	ScratchHandleSpace
public	TopOfHandleSpace
public	TopOfFreeEMSspace
public	TopOfUsedEMSspace
public	BotOfVCPIspace
public	UsedEMSPages
public	UsedVCPIPages
public	MaxEMSpool
public	MinEMSpool
public	NumberHandlePages
public	LastHandleMap
public	CurrentHandleMap
public	subfunction_number
public	function_number
public	saved_gdtr
public	saved_idtr
public	saved_ldtr
public	saved_tr
public	saved_cr2
public	saved_cr3
public	VCPI_version
public	number_code_PTEs
public	code_address
public	code_offset
public	data_address
public	data_offset
public	real_idt
public	end_of_base_memory_PTE
public	context_save_area_size
public	page_directory
public	page_tables
public	Stack0
public	Stack0_top
public	driver_end
public	segR1_CODE
public	IRQ1Event
public	p60data
public	p64data
public	TopOfPhysicalMemory
public	MaxPTEIndex
public	msg_flag
public	PF_Base
public	VCPIset
public	NoEMSset
public	NoPFset
public	UserDS
public	UserES
public	UserFS
public	UserGS
public	UserSS
public	UserSP
public	RealSS
public	RealSP
public	StackTop
public	RealStack
public	LastStack
public	RealStackTop
public	LastStackTop
public	RealStack2_top
public	Current_State
public	Current_Mode
public	Auto_State
public	Weitek_State
public	PageD_Addr
public	TrapFlags
public	ErrType
public	ErrNum
;;public	PrevInt10
;;public	PrevInt11
public	PrevInt13
public	PrevInt15
public	PrevInt19
public	PrevInt2F
;;public	PrevInt4B
public	CROM_Length
public	arg_str
public	Initial_Mode
public	Initial_Weitek
public	DMABufferSize
public	DMABufferAddress
public	DMAFlags
public	DMAPortList
public	DMAEISAPortList
public	DMAMCAPortList
public	TOTAL_DMA_PORTS
public	TOTAL_DMA_EISA_PORTS
public	TOTAL_DMA_MCA_PORTS
public	DMA_page_port
public	DMA_address_port
public	DMA_count_port
public	DMA_EISA_HighPagePort
public	DMA_EISA_HighCountPort
public	DMA_single_mask_port
public	DMA_clr_FF_port
public	DMAActive
public	Int10_Save
public	RR_Last
public	RR85save
public	RR84save
public	buffer
public	ext_rem
PUBLIC 	total_register_sets
public	saved_pointer
public	GDT_Ptr
public	IDT_Ptr
public	DiagByte
public	Exit_Flags
public	strtng_bs_wndw_PTE
public	ROMID
public	GenFlags
public	TSS
public	IOBitMap
public	TSSLEN
public	p_TEXT
public	p_DATA
public	pSTACK
public	pTSS
public	pGDT
public	pIDT
ifdef MSFLAG
public	Stack2
public	Stack2_top
endif

ifdef QEMS
public	installed
public	first_free_handle_space
public	ext_vdisk
public	avail_mem
public	high_memory_address
public	hi_size
public	hi_alloc
public	hisys_alloc
public	ext_memory_address
public	ext_size
public	total_mem
public	AllocMapPtr
public	VCPIAllocMapPtr
public	pool_size
public	number_page_tables
public	starting_handle_PTE
public	number_handle_PTEs
public	starting_ext_mem_PTE
public	number_ext_mem_PTEs
public	starting_high_mem_PTE
public	number_high_mem_PTEs
public	starting_conv_mem_PTE
public	number_conv_mem_PTEs
PUBLIC	free_pages			; Original free 4K pages
public	free_4k_pages
public	total_4k_pages
PUBLIC	PH_entries
PUBLIC	PH_boundary
PUBLIC	my_AX
public	EMS_cntxt
public	EMS_window
public  EMS_window_ptr
public	save_area_pointer
public	target_offset
public	target_segment
endif

;******************************************************************************
; INCLUDES
;******************************************************************************
include	vdmseg.inc
include	vdmsel.inc
include	desc.inc
include	dma.inc
include	ascii_sm.equ
include	emmfunct.inc
include oemdep.inc
include	emm386.inc
include	page.inc
page
;******************************************************************************
; SEGMENTS
;******************************************************************************
R_CODE	segment
ALIGN	4
msg_flag		dd	0	;
PageD_Addr		dd	0	; The address of page directory
HMAptr			dd	100000h	; physical address of HMA
;;PrevInt10		dd	0	; The old int10 vector
;;PrevInt11		dd	0	; The old int11 vector
PrevInt13		dd	0	; The old int13 vector
PrevInt15		dd	0	; The old int15 vector
PrevInt19		dd	0	; The old int19 vector
;;PrevInt4B		dd	0	; The old int4B vector
Int10_Save		dd	?	; saved int10 vector from CEMM load
GDT_Ptr 		dd 2 dup (0)	; GDT ptr for LGDT
IDT_Ptr 		dd 2 dup (0)	; IDT ptr for LIDT
GenFlags		dd  fNoEMSInt	; General System Flags (no MES interrupts)

ttl_hndls		dw	64
TrapFlags		dw	0	; Flags to trap into protected mode
TopOfHandleSpace	dw	0
TopOfFreeEMSspace	dw	0
TopOfUsedEMSspace	dw	0
BotOfVCPIspace		dw	0
UsedEMSPages		dw	0
UsedVCPIPages		dw	0
MaxEMSpool		dw	0	; max size of EMS pool in Kb
MinEMSpool		dw	0	; min size (preallocated) of EMS pool
UserDS			dw	0	; save area for entry stack DS
UserES			dw	0	; save area for entry stack ES
UserFS			dw	0	; save area for entry stack FS
UserGS			dw	0	; save area for entry stack GS
UserSS			dw	0	; save area for entry stack SS
UserSP			dw	0	; save area for entry stack SP
RealSS			dw	0	; save area for CEMM real stack SS
RealSP			dw	0	; save area for CEMM real stack SP
StackTop		dw	Stack0_top  ; size of protected mode stack
segR1_CODE		dw	seg R1_CODE ; final segment of R1_CODE
driver_end		dw	0	; segment address of the driver's end
Current_State		dw	0	; The current ON/OFF state of CEMM
UMBptr			dw	0	; pointer to first UMB arena
ErrType			dw	0	; Error type (Exception, Privilige, DMA)
ErrNum			dw	0	; Error number
CROM_Length		dw	0	; length of COMPAQ option ROM
ext_rem 		dw	0	; remaining extended memory in kbytes
Exit_Flags		dw	0	; flags for int15 exit
PF_Base			dw	0FFFFh	;
handle_count		dw	0	; number of handles allocated
cntWinPages		dw	0	; count of WIN= pages
Current_Mode		db	0	; The current auto ON/OFF state of CEMM
Auto_State		db	0	; The current auto ON/OFF state of CEMM
Weitek_State		db	0	; The current Weitek ON/OFF state
Initial_Mode		db	0FFh	; initial CEMM mode, ON/OFF/AUTO
Initial_Weitek		db	0FFh	; initial weitek mode, ON/OFF
VCPIset			db	-1
NoEMSset		db	FALSE
NoPFset			db	FALSE
UMBHMA			db	FALSE	; CEMM is providing UMBs or and HMA

ifdef QEMS
free_pages		DW	0	; Original free 4K pages
free_4k_pages		dw	0	; number of unallocated 4k pages
total_4k_pages		dw	0	; total available 4k pages
starting_ext_mem_PTE	dw	0	; starting extended memory location
number_ext_mem_PTEs	dw	0	; number of extended memory PTEs
starting_high_mem_PTE	dw	0	; starting Built In Memory location
number_high_mem_PTEs	dw	0	; number of Built In Memory PTEs
starting_conv_mem_PTE	dw	0	; starting conventional 640k location
number_conv_mem_PTEs	dw	0	; number of conventional memory PTEs
number_page_tables	dw	0	; the number of used page tables
endif
;***********************************************************************
; VCPI Variables
;***********************************************************************
ALIGN	4
saved_cr2		dd	?		; used for storing CEMM's regs.
saved_cr3		dd	?		; used for storing CEMM's regs.
VCPI_version		dd	0100h		; the version number
data_offset		dd	?		; for ModeInterface
data_address		dd	?		; for ModeInterface
code_offset		dd	?		; for ModeInterface
code_address		dd	?		; for ModeInterface
saved_gdtr		dq	?		; used for storing CEMM's regs.
saved_idtr		dq	?		; used for storing CEMM's regs.
saved_ldtr		dw	?		; used for storing CEMM's regs.
saved_tr		dw	?		; used for storing CEMM's regs.
real_idt		dw	0400h		; real mode DOS IDT limit
			dd	0		; and base ptr
			dw	0		; just in case qword used
number_code_PTEs	dw	?		; for ModeInterface

;===============================================================================
;==  32 bit pointers to major data blocks
;===============================================================================
ALIGN	4
p_TEXT		dd	0		; 32 bit pointer to _TEXT
p_DATA		dd	0		; 32 bit pointer to _DATA
pSTACK		dd	0		; 32 bit pointer to STACK
pTSS		dd	0		; 32 bit pointer to TSS
pGDT		dd	0		; 32 bit pointer to GDT
pIDT		dd	0		; 32 bit pointer to IDT
R_CODE	ends

R1_CODE segment
	dw	128	dup(0FFFFh)
RealStack2_top	label	byte
PrevInt2F		dd	0	; The old int2F vector
R1_CODE ends

page
;******************************************************************************
; SEGMENTS
;******************************************************************************
_DATA	segment
	assume	cs:_DATA,ds:_DATA
ALIGN	4
page_directory		dd	0	; 32 bit address of the page directory
page_tables		dd	0	; 32 bit address of the page tables
ScratchHandleSpace	dd	0	; 32 bit address of scratch handle space
TopOfPhysicalMemory	dd	0	; top of physical memory
MaxPTEIndex		dd	0	; maximum PTE index in page tables
saved_pointer		dd	0
xma2ems			DW	?	; An XMA2EMS compatible
total_handles		dw	64	; Default handles supported
strtng_bs_wndw_PTE	dw	40h	; Default base window starts at 256K
end_of_base_memory_PTE	dw	0	; last valid real memory location
context_save_area_size	dw	0	; size to save the window mapping in
NumberHandlePages       dw	?	; pages used for handle space
LastHandleMap		dw	0FFCh	; used for handle space mapping
CurrentHandleMap	dw	0	; used for handle space mapping
buffer	   		dw	0	; buffer for 1 word move blocks
subfunction_number	db	0	; the AL subfunction number
function_number		db	0	; the AH subfunction number
IRQ1Event		db	TRUE	; flag indicating IRQ1 has ocurred
p60data			db	0	; last keyboard scan code
p64data			db	0	; last CMD written to port 64h
arg_str 		db	MAX_ARG_LEN+1	dup(0)
RR_Last 		db	0	; last RR port trapped
RR85save		db	0FFh
RR84save		db	0FFh
total_register_sets 	DB 	8 	; 7 alternate register sets + register set 0
				;      at int19h.
DiagByte	db	LOW LOCK_ROM	; most recent diag byte written by user
ROMID 		db  	0		; machine ID

ifdef QEMS
hi_size 	dw	0	; size of hi memory in kbytes
hi_alloc	dw	0	; actual hi memory allocated (due to waste)
hisys_alloc	dw	0	; amount of hi system mem allocated in 4k bytes
ext_size	dw	0	; size of extended memory in kbytes
total_mem	dw	0	; Total extended memory
ext_vdisk	dw	0	; VDISK extended memory used kbytes
avail_mem	dw	0	; Available memory
first_free_handle_space	dw	0
starting_handle_PTE	dw	?	; starting handle location
my_AX			DW	?	; Scratch area
high_memory_address	dd	0	; 32 bit address of high memory pool
ext_memory_address	dd	100000h	; 24 bit address of extended memory pool
pool_size		dw	0	; Default is 256K
number_handle_PTEs	dw	?	; number of PTEs reserved for handles
PH_entries		DW	?	; Number of entries for PHs
PH_boundary		DW	?	; Beyond the boundary of PHs
target_offset		dw	0
target_segment		dw	0
installed	db	0	; 1 => VDISK style header already installed
endif

;***********************************************************************
; OS_functions
;	These are various variables used for the OS/E functions.
;***********************************************************************
ALIGN	4
OS_key		dd	0
OS_functions	dw	001h

;***********************************************************************
; DMA
;	These are the various DMA port register variables.
;***********************************************************************
ALIGN	4
DMABufferSize		dd	8000h		; default of 32K (changed by parser)
DMABufferAddress	dd	0		; entered during initialization
DMAFlags		db	0		; DMA genearal flags
DMAActive		db	0		; bit set if a DMA channel is active

DMA_single_mask_port label byte
	db	DMA_SINMASK1		; DMA single mask registers
	db	DMA_SINMASK1
	db	DMA_SINMASK1
	db	DMA_SINMASK1
	db	0
	db	DMA_SINMASK2
	db	DMA_SINMASK2
	db	DMA_SINMASK2
DMA_clr_FF_port	label byte
	db	DMA_CLR_FF1		; reset flip-flop commands
	db	DMA_CLR_FF1
	db	DMA_CLR_FF1
	db	DMA_CLR_FF1
	db	0
	db	DMA_CLR_FF2
	db	DMA_CLR_FF2
	db	DMA_CLR_FF2

DMAPortList	label	byte
DMA_page_port label byte
	db	DMA_P0			; DMA page registers
	db	DMA_P1
	db	DMA_P2
	db	DMA_P3
	db	0
	db	DMA_P5
	db	DMA_P6
	db	DMA_P7
DMA_address_port label byte
	db	DMA_B0			; DMA base registers
	db	DMA_B1
	db	DMA_B2
	db	DMA_B3
	db	0
	db	DMA_B5
	db	DMA_B6
	db	DMA_B7
DMA_count_port label byte
	db	DMA_C0			; DMA count registers
	db	DMA_C1
	db	DMA_C2
	db	DMA_C3
	db	0
	db	DMA_C5
	db	DMA_C6
	db	DMA_C7

	db	DMA_STAT1
	db	DMA_STAT2
	db	DMA_REQUEST1
	db	DMA_REQUEST2
	db	DMA_SINMASK1		; DMA single mask registers
	db	DMA_SINMASK2
	db	DMA_MODE1
	db	DMA_MODE2
	db	DMA_RESET1
	db	DMA_RESET2
	db	DMA_RESMASK1
	db	DMA_RESMASK2
	db	DMA_MASK1
	db	DMA_MASK2
	db	DMA_CLR_FF1
	db	DMA_CLR_FF2
TOTAL_DMA_PORTS	equ	$-DMAPortList

DMAEISAPortList	label	word
DMA_EISA_HighPagePort label word
	dw	DMA_E_P0		; EISA DMA high page port
	dw	DMA_E_P1
	dw	DMA_E_P2
	dw	DMA_E_P3
	dw	DMA_E_P5
	dw	DMA_E_P6
	dw	DMA_E_P7
DMA_EISA_HighCountPort label word
	dw	DMA_E_C0		; EISA DMA high count port
	dw	DMA_E_C1
	dw	DMA_E_C2
	dw	DMA_E_C3
	dw	DMA_E_C5
	dw	DMA_E_C6
	dw	DMA_E_C7

	dw	DMA_E_IS	; EISA Channel interrupt status register (read only)
	dw	DMA_E_CS	; EISA Chaining mode status register (read only)

	dw	DMA_E_CH1	; EISA ChainMode for channels 0-3 (write only)
	dw	DMA_E_CH2	; EISA ChainMode for channels 5-7 (write only)

	dw	DMA_E_EM1	; EISA Extended Mode for channels 0-3
	dw	DMA_E_EM2	; EISA Extended Mode for channels 5-7
TOTAL_DMA_EISA_PORTS equ ($-DMAEISAPortList)/2

DMAMCAPortList	label	byte
	db	DMA_XFN		; MCA exteneded function register
	db	DMA_EXE		; MCA execute register
TOTAL_DMA_MCA_PORTS equ $-DMAMCAPortList


;=============================================================================
;==
;==  The TSS is in the _DATA segment for easy access during interrupt handling
;==
;=============================================================================
even
TSS:	TSS386STRUC	<>
;
;   I/O Bit Map for Virtual Mode I/O trapping
;
	public	IOBitMap
IOBitMap	equ	$-TSS
	db	2000h dup (0)		; initialize all ports to NO trapping
	db	0FFh			; last byte is all 1's
TSSLEN		equ	$-TSS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following items have dynamic sizes computed during istallation.  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;***********************************************************************
; EMS_window[]
;	This is an array of structures where each structure defines the
;	PTE index of where the window is located and it current mapping.
;	This is initialized at init time and then the contents are copied
;	to the array in Register Set 0. The value of EMS_window_ptr is then
;	initialized to the Register Set 0 array. This EMS_window array is
;	then used only during SetAlternateRegSet for copying the contents
;	of the user buffer before we do the switch to another reg set.
;
;	The label EMS_cntct is used for this purpose. The first word is
;	used to hold the number of ems windows and the secong is used to
;	hold the ID.
;
;***********************************************************************
number_EMS_windows	dw	?	; actual number of windows available
EMS_window_location	dw	TOTAL_EMS_WINDOWS dup (FREE)
EMSsegLoc		db	256 dup (-1)

ifdef QEMS
EMS_cntxt	label	word
			dw	?	;
			dw	?	;
EMS_window	label	word
rept	TOTAL_EMS_WINDOWS
	EMS_window_struc	<FREE,NULL_PAGE>
endm

EMS_window_ptr	dd	?
endif

;***********************************************************************
; register_set[]
;	These are various variables used for the alternate page registers
;	One additional data space is reserved for the MapAndCall routine.
;***********************************************************************
current_register_set	db	0

;QEMS save_area_pointer	dd	0

;register_set	label	word
;rept	TOTAL_REGISTER_SETS + 1
;	RegisterSet_struc	<>
;endm

;***********************************************************************
; handle_table
;	These are various variables used for the handles
;***********************************************************************
;handle_table	label	word
;rept	MAX_HANDLES
;	HandleTable_struc	<NULL_PAGE,0>
;endm
hndl_tbl_ptr	DD	?

;handle_name	label	qword		; used for holding the handle's name
;rept	MAX_HANDLES
;	db	8 dup (0)
;endm
hndl_nam_ptr	DD	?

;***********************************************************************
; save_map[]
;	This is an array of structures that save the current mapping state.
;	The save_map array is only used with the 'Save Page Map' EMS function
;	which only saves the 3.2 windows (page frame base).
;***********************************************************************
;save_map	label	word
;rept	MAX_HANDLES
;	EMS_window_struc	TOTAL_PF_WINDOWS dup (<FREE, NULL_PAGE>)
;endm
save_map_ptr	DD	?

save_flag	LABEL	BYTE
rept	MAX_HANDLES
	DB	FREE
endm

ifdef QEMS
;==============================================================================
;==  EMS and VCPI memory allocation bit map
;==============================================================================
align 16
AllocMapPtr	 dd 0 ; pointer to page alloc bitmap
VCPIAllocMapPtr	 dd 0 ; pointer to VCPI page alloc bitmap
endif

_DATA	ends

page
;******************************************************************************
; SEGMENTS
;******************************************************************************
STACK	segment

;******************************************************************************
; Stack0_top  - Ring 0 stack for VDM exception/int handling
;******************************************************************************
Stack0		label	byte
	dd	Stack0Size dup(0)
Stack0_top	label	byte

ifdef MSFLAG
Stack2		label	byte
	dw	64	dup(0)
Stack2_top	label	byte
endif
STACK ends

R_STACK	segment
RealStack	label	byte
	dw	128	dup(0FFFFh)
RealStackTop	label	byte
R_STACK ends

L_STACK	segment
LastStack	label	byte
	dw	1024	dup('TS')
LastStackTop	label	byte
L_STACK ends
END

