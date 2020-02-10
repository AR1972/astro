ifndef	INC_LIST
.xlist
endif
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1989-1991
; (C) Copyright COMPAQ Computer Corp. 1989-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;		EMMLIB.LIB - Expanded Memory Manager Library
;
; Module:	EMMDATA.INC - external references
;
; Version:	0.10
;
; Date:		January 6, 1989
;
; Author:	Daniel J. Mazina
;******************************************************************************
;   Change log:
;
;     DATE    REVISION                  DESCRIPTION
;   --------  --------  -------------------------------------------------------
;   01/06/89  Original
;******************************************************************************

R_CODE	segment
	extrn	ext_rem:word
	extrn	Int10_Save:dword
	extrn	Initial_Mode:byte
	extrn	Initial_Weitek:byte
	extrn	CROM_Length:word
	extrn	Current_State:word
	extrn	Current_Mode:byte
	extrn	driver_end:word
	extrn	segR1_CODE:word
	extrn	msg_flag:dword
	extrn	UMBptr:word
	extrn	HMAptr:dword
	extrn	UMBHMA:byte
	extrn	saved_gdtr:qword
	extrn	saved_idtr:qword
	extrn	saved_ldtr:word
	extrn	saved_tr:word
	extrn	saved_cr2:dword
	extrn	saved_cr3:dword
	extrn	data_offset:dword
	extrn	data_address:dword
	extrn	code_offset:dword
	extrn	code_address:dword
	extrn	VCPI_version:dword
	extrn	real_idt:word
	extrn	UserDS:word
	extrn	UserES:word
	extrn	UserFS:word
	extrn	UserGS:word
	extrn	UserSS:word
	extrn	UserSP:word
	extrn	RealSS:word
	extrn	RealSP:word
	extrn	StackTop:word
	extrn	RealStackTop:byte
	extrn	Auto_State:byte
	extrn	Weitek_State:byte
	extrn	PageD_Addr:dword
	extrn	ErrType:word
	extrn	ErrNum:word
	extrn	TrapFlags:word
	extrn	TopOfHandleSpace:word
	extrn	TopOfFreeEMSspace:word
	extrn	TopOfUsedEMSspace:word
	extrn	BotOfVCPIspace:word
	extrn	UsedEMSPages:word
	extrn	UsedVCPIPages:word
	extrn	MaxEMSpool:word
	extrn	MinEMSpool:word
;;	extrn	PrevInt10:dword
;;	extrn	PrevInt11:dword
	extrn	PrevInt13:dword
	extrn	PrevInt15:dword
	extrn	PrevInt19:dword
	extrn	end_of_R_CODE:byte
	extrn	GDT_Ptr:dword
	extrn	IDT_Ptr:dword
	extrn	Exit_Flags:word
	extrn	PF_Base:word
	extrn	VCPIset:byte
	extrn	NoEMSset:byte
	extrn	NoPFset:byte
	extrn	handle_count:word
	extrn	cntWinPages:word
	extrn	GenFlags:dword
	extrn	p_TEXT:dword
	extrn	p_DATA:dword
	extrn	pSTACK:dword
	extrn	pTSS:dword
	extrn	pGDT:dword
	extrn	pIDT:dword
ifdef QEMS
	extrn	number_page_tables:word
	extrn	number_code_PTEs:word
	EXTRN	free_pages: WORD	; Original free 4K pages	;@PIW
	extrn	free_4k_pages:word
	extrn	total_4k_pages:word
	extrn	starting_ext_mem_PTE:word
	extrn	number_ext_mem_PTEs:word
	extrn	starting_high_mem_PTE:word
	extrn	number_high_mem_PTEs:word
	extrn	starting_conv_mem_PTE:word
	extrn	number_conv_mem_PTEs:word
endif

R_CODE	ends


R1_CODE segment
	extrn	PrevInt2F:dword
R1_CODE ends


_DATA	segment
	EXTRN	xma2ems: WORD							; An XMA2EMS compatible
	extrn	subfunction_number:byte
	extrn	function_number:byte
	extrn	LastHandleMap:word
	extrn	CurrentHandleMap:word
	extrn	number_EMS_windows:word
	extrn	ScratchHandleSpace:dword
	extrn	total_handles:word
	extrn	total_register_sets:byte
	extrn	EMS_window_location:word
	extrn	EMSsegLoc:byte
	extrn	hndl_tbl_ptr: DWORD	; word
	extrn	save_map_ptr: DWORD	; word
	extrn	hndl_nam_ptr: DWORD	; qword
	extrn	save_flag:BYTE
	extrn	OS_functions:word
	extrn	OS_key:dword
	extrn	current_register_set:byte
	extrn	register_set:word
	extrn	end_of_base_memory_PTE:word
	extrn	context_save_area_size:word
	extrn	page_directory:dword
	extrn	page_tables:dword
	extrn	IRQ1Event:byte
	extrn	p60data:byte
	extrn	p64data:byte
	extrn	TopOfPhysicalMemory:dword
	extrn	MaxPTEIndex:dword
	extrn	end_of_DATA:byte
	extrn	arg_str:byte
	extrn	DMABufferSize:dword
	extrn	DMABufferAddress:dword
	extrn	DMAFlags:word
	extrn	DMAPortList:byte
	extrn	DMAEISAPortList:word
	extrn	DMAMCAPortList:byte
	extrn	TOTAL_DMA_PORTS:abs
	extrn	TOTAL_DMA_EISA_PORTS:abs
	extrn	TOTAL_DMA_MCA_PORTS:abs
	extrn	DMA_page_port:byte
	extrn	DMA_address_port:byte
	extrn	DMA_count_port:byte
	extrn	DMA_EISA_HighPagePort:word
	extrn	DMA_EISA_HighCountPort:word
	extrn	DMA_single_mask_port:byte
	extrn	DMA_clr_FF_port:byte
	extrn	DMAActive:byte
	extrn	buffer:word
	extrn	RR_Last:byte
	extrn	RR85save:byte
	extrn	RR84save:byte
	extrn	saved_pointer:dword
	extrn	DiagByte:byte
	extrn 	ROMID:byte
	extrn	TSS:byte
	extrn	IOBitMap:abs
	extrn	TSSLEN:abs
	extrn	strtng_bs_wndw_PTE:word
	extrn	NumberHandlePages:word

ifdef QEMS
	extrn	pool_size:word
	extrn	first_free_handle_space:word
	extrn	high_memory_address:dword
	extrn	hi_size :word
	extrn	hi_alloc:word
	extrn	hisys_alloc:word
	extrn	ext_memory_address:dword
	extrn	ext_size:word
	extrn	total_mem:word
	extrn	ext_vdisk:word
	extrn	avail_mem:word
	extrn	installed:byte
	extrn	VCPIAllocMapPtr:dword
	extrn	AllocMapPtr:dword
	extrn	starting_handle_PTE:word
	extrn	number_handle_PTEs:word
	EXTRN	PH_entries: WORD
	EXTRN	PH_boundary: WORD
	EXTRN	my_AX: WORD								; Used as scratch register
	extrn	EMS_window:word
	extrn	EMS_cntxt:word
	extrn	EMS_window_ptr:DWORD
	extrn	save_area_pointer:dword
	extrn	target_offset:word
	extrn	target_segment:word
endif
_DATA	ends

STACK	segment
	extrn	Stack0:byte
	extrn	Stack0_top:byte
ifdef MSFLAG
	extrn	Stack2:byte
	extrn	Stack2_top:byte
endif
	extrn	end_of_STACK:byte
STACK	ends

R_STACK	segment
	extrn	RealStack:byte
	extrn	RealStackTop:byte
	extrn	end_of_R_STACK:byte
R_STACK	ends

L_STACK	segment
	extrn	LastStack:byte
	extrn	LastStackTop:byte
	extrn	end_of_L_STACK:byte
L_STACK	ends


_TEXT	segment
	extrn	end_of_TEXT:byte
_TEXT	ends

GDT	segment
	extrn	GDTLEN:abs
GDT	ends

IDT	segment
	extrn	IDTLEN:abs
IDT	ends

ifdef TSSQLEO
TSS	segment
	extrn	TSSLEN:abs
TSS	ends
endif

.list ; end of EMMData.inc

