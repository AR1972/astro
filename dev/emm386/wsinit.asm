.386p
	page 58,132
;=============================================================================
	title	W S I N I T - allocates/moves workspace to extended memory
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: WSInit - Allocates and moves workspace up to extended memory
;==
;==	Version: 1.00
;==
;==	Date:	September 16,1989
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     09/16/89 0.00	        Original
;==
;=============================================================================
;=============================================================================
;==	P U B L I C   D E C L A R A T I O N S
;=============================================================================
	public	WSInit
	public	WSMove
	public	GDT_Seg
	public	IDT_Seg
;=============================================================================
;==	E X T E R N A L  D E C L A R A T I O N S
;=============================================================================
LAST	segment
	extrn	SegTo24:near
	extrn	SetSegDesc:near
	extrn	SetPageEntry:near
	extrn	DoMoveBlock:near
	extrn	tempBuffer: BYTE
;QEMS	extrn	tempAllocMap:word
;QEMS	extrn	tempAllocMapLen:abs
	extrn	VxDInit:near

data_addr	DD	?
LAST	ends

;=============================================================================
;==	L O C A L   C O N S T A N T S
;=============================================================================
	include vdmseg.inc
	include vdmsel.inc
	include desc.inc
	include page.inc
	include oemdep.inc
	include romxbios.equ
	include emm386.inc
	include emmfunct.inc
	include	emmdata.inc
	include	allocmem.inc
	include	winemm.inc
;=============================================================================
;==	D A T A   S T R U C T U R E S
;=============================================================================
sMoveSegment	struc
 MSSource	dd	0       ; Source address
 MSDest		dd	0       ; Destination address
 MSSize		dd	0       ; Size of segment
sMoveSegment	ends
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
R_CODE	segment
 GDT_Seg 	dw	seg GDT 	; current segment for GDT
 IDT_Seg 	dw	seg IDT 	; current segment for IDT
R_CODE	ends
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
LAST	segment
	assume	cs:LAST,ds:_DATA,es:_DATA,gs:R_CODE
;=============================================================================
;==	L O C A	L   D A T A
;=============================================================================
;QLEO TSSLEO NumSegExt	equ	6	; number of segments reallocated to extended memory
NumSegExt	equ	5	; number of segments reallocated to extended memory
MoveSegmentList	sMoveSegment	NumSegExt dup (<>)

ARPT		dd 	255 dup(-1)

;==============================================================================
;==
;==  WSInit: Allocate work space and initialize GDT selectors with extended
;==	     memory addresses.  Does not move the segments to extended memory
;==	     due to _DATA not completely initialized until the EMS pool is
;==	     allocated.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================

WSInit	proc	near
	push	eax
	push	ebx
	push	edx
	push	esi

;
;  _TEXT segment
;
	mov	eax,offset end_of_TEXT
	mov	ebx,PARA_BOUNDARY
	mov	dx,VDMCA_GSEL
	shl	edx,16
	mov	dx,VDMC_GSEL
	mov	esi,offset LAST:MoveSegmentList
	call	AllocWS
	jc	WSIexit
	mov	ebx,cs:[esi].MSDest
	mov	gs:[p_TEXT],ebx

;
;  _DATA segment
;
;	mov	eax,offset end_of_DATA
;
; +++ CHNG  Add the space of the alternate register sets
	MOV	BX, [total_handles]

	mov	eax,size HandleTable_struc+size HandleName_struc+size HandleSaveMap_struc+size save_flag+size EMM_Handle

ifdef QEMS
	MOV	EAX, TYPE HandleTable_struc+TYPE HandleName_struc+TYPE EMS_window_struc * TOTAL_PF_WINDOWS+TYPE save_flag+TYPE EMM_Handle
endif
	MUL	BL			; Size of handle related data
	MOV	EDX, EAX
	MOV	BL, SIZE RegisterSet_struc
	MOV	AL, [total_register_sets]
	MUL	BL			; Size of total_register_sets

ifdef 910707
	add	edx,eax

	mov	eax,[MaxPTEindex]	; number of bits required for alloc_map
	shr	eax,2			; also for vcpi_alloc_map.
endif
	ADD	EAX, EDX
	ADD	eax,offset end_of_DATA
; +++
	mov	ebx,PARA_BOUNDARY
	mov	edx,VDMD_GSEL
	add	esi,size sMoveSegment
	call	AllocWS
	jc	WSIexit
	mov	ebx,cs:[esi].MSDest
	mov	gs:[p_DATA],ebx

	call	VxDInit

;
;  Fixup TSS selectors in GDT
;
	mov	eax,TSSLEN
	mov	ebx,cs:[esi].MSDest
	add	ebx,offset _DATA:[TSS]
	mov	gs:[pTSS],ebx
	mov	dx,TSSD_GSEL
	shl	edx,16
	mov	dx,TSS_GSEL
	call	UpdateGDT

;
;  STACK segment
;
	mov	eax,offset end_of_STACK
	mov	ebx,PARA_BOUNDARY
	mov	edx,VDMS_GSEL
	add	esi,size sMoveSegment
	call	AllocWS
	jc	WSIexit
	mov	ebx,cs:[esi].MSDest
	mov	gs:[pSTACK],ebx

ifdef TSSLEO	;QLEO
;
;  TSS segment
;
	mov	eax,TSSLEN
	mov	ebx,PARA_BOUNDARY
	mov	dx,TSSD_GSEL
	shl	edx,16
	mov	dx,TSS_GSEL
	add	esi,size sMoveSegment
	call	AllocWS
	jc	short WSIexit
	mov	ebx,cs:[esi].MSDest
	mov	gs:[pTSS],ebx
endif
;
;  IDT segment
;
	mov	eax,IDTLEN
	mov	ebx,PARA_BOUNDARY
	mov	edx,IDTD_GSEL
	add	esi,size sMoveSegment
	call	AllocWS
	jc	short WSIexit
	mov	ebx,cs:[esi].MSDest
	mov	gs:[pIDT],ebx

;
;  GDT segment
;
	mov	eax,GDTLEN
	mov	ebx,PARA_BOUNDARY
	mov	edx,GDTD_GSEL
	add	esi,size sMoveSegment
	call	AllocWS
	mov	ebx,cs:[esi].MSDest
	mov	gs:[pGDT],ebx

	cmp	gs:[NoEMSset],TRUE	; Q: Is NoEMS mode active?
	je      short WSIexit		; Y: done
	call	AllocARPTs		; N: allocate Page Tables for the
					;    Alternate Register sets.

WSIexit:
	pop	esi
	pop	edx
	pop	ebx
	pop	eax
	ret
WSInit	endp

;==============================================================================
;==
;==  WSMove: Move work space up to extended memory.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==
;==============================================================================
	assume	cs:LAST,ds:_DATA,es:_DATA,gs:R_CODE
WSMove	proc	near
	pushad
	push	ds
	push	es

; +++ Initialize alternate register sets	(PIW)
	MOV	ESI, OFFSET LAST:MoveSegmentList
	ADD	ESI, SIZE sMoveSegment	; Skip _TEXT
	; Update the size for _DATA to equal to the end of register set 0
	MOV	CS:[ESI].MSsize, OFFSET register_set
	ADD	CS:[ESI].MSsize, TYPE RegisterSet_struc
	MOV	EDI, CS:[ESI].MSDest	; Beginning of _DATA
	MOV	EBP, EDI		; Save the beginning of _DATA
	MOV	ESI, OFFSET register_set
	ADD	EDI, ESI		; Beginning of Register Set 0


	;
	; All register sets carry with them space for the context save area
	; i.e., the ems_window structure. The ems_window struc will now
	; be accessed via the EMS_window_ptr which will point to one of
	; the ems_window strucs in one of the register sets at any given
	; time. We shall initialize he EMS_window_ptr to point to reg set
	; 0. The actual ems_window struc will start from
	;
	; 	start of reg. set + saved_mapping + 4
	;
	; The 4 bytes is for the number_ems_window and the signature field
	; that is saved in the reg. set. See savewindowmapping in emm40.asm
	;
	; Note that EMS_window struc will have to be copied to EMS_window_ptr
	; This is done further below.
	;


;QEMS	mov	[EMS_window_ptr], offset register_set + saved_mapping+4

	ADD	EDI, TYPE RegisterSet_struc ; Beginning of Alternate Register Sets
	MOV	[SI].active, FALSE

	XOR	EAX, EAX
	MOV	AX, _DATA
	SHL	EAX, 4
	ADD	ESI, EAX
	MOV	DL, [total_register_sets]

	xor	ebx, ebx

init_next_reg_set:
	; CY = DoMoveBlock(ESI, EDI, CX)

	mov	ecx, offset register_set
	mov	eax, [ARPT+ebx*4]
	mov	[ecx].page_table_ptr, eax

	MOV	ECX, SIZE RegisterSet_struc	; Size to move
	CALL	DoMoveBlock
	ADD	EDI, ECX
	inc	ebx
	DEC	DL
	JNZ	init_next_reg_set

	MOV	SI, OFFSET register_set
	MOV	[SI].active, TRUE

	mov	eax, [page_tables]
	mov	[si].page_table_ptr, eax
ifdef QEMS
	mov	ax, [number_ems_windows]
	mov	[si].saved_mapping, ax
	mov	[si].saved_mapping+2, 'C'+'O'+'M'+'P'+'A'+'Q'
endif

; Initialize handle_table
	MOV	AX, LAST
	MOV	ES, AX
	ASSUME	ES: LAST

	MOV	EBX, EDI
	SUB	EDI, EBP		; Get the offset
	MOV	[hndl_tbl_ptr], EDI	; Save the beginning of handle_table
	MOV	DI, OFFSET tempBuffer

	MOV	AX, [end_of_base_memory_PTE]
	MOV	DX, [strtng_bs_wndw_PTE] ; STARTING_BASE_WINDOW_PTE
	SUB	AX, DX
	INC	GS:[handle_count]
	MOV	ES:[DI].base_PTE_index, FIRST_HANDLE_PTE
	MOV	ES:[DI].number_PTEs, 0
	CMP	[xma2ems], TRUE		; Q: Is it in XMA2EMS mode?	;@PIW
	JE	SHORT init_other_handles				;@PIW
	cmp	gs:[NoEMSset],TRUE		;Q: Is it in NoEMS mode?
	je	short init_other_handles	; Y: no base memory			;@PIW
;+++ PIW CUT begin
	MOV	ES:[DI].number_PTEs, AX

ifdef QEMS
	ADD	GS:[total_4k_pages], AX
	ADD	[first_free_handle_space], AX ; The first AX pages used	;@PIW

		; The conventional memory pool below 640k is now set up.
	mov	gs:[starting_conv_mem_PTE], DX ; STARTING_BASE_WINDOW_PTE
	mov	gs:[number_conv_mem_PTEs],ax

	cmp	ax,0
	je	SHORT init_other_handles

		; The window mapping for handle zero is now done.  First
		; the beginning window for handle zero is found.
	xor	esi,esi
	mov	cx,TOTAL_EMS_WINDOWS
EI_find_low_window:
	cmp	EMS_window_location[esi*2], DX ; STARTING_BASE_WINDOW_PTE
	je	short EI_map_in_handle_zero
	inc	si
	loop	EI_find_low_window

		; Now the actual handle and logical 4k page pairs are copied
		; in.  BX is the logical page which is 4 for every window.
EI_map_in_handle_zero:
	mov	cx,ax
	shr	cx,2
	XOR	DX, DX
EI_init_windows:
	mov	BYTE PTR EMS_window[esi*2+ESI].handle,0
	mov	EMS_window[esi*2+ESI].logical_4k_page, DX
	inc	si
	ADD	DX, 4
	loop	EI_init_windows
;+++ PIW CUT end
endif

init_other_handles:

	ADD	DI, TYPE HandleTable_struc
;QEMS	mov	ax,gs:[total_4k_pages]		; init number of handle PTEs;LEO
;QEMS	mov	[number_handle_PTEs],ax                                     ;LEO
	XOR	EAX, EAX
	MOV	AX, FREE			; handle_table <FREE, 0>
	MOV	CX, [total_handles]
	DEC	CX				; Skip handle 0
	cld
	REP	STOSD
	MOV	EDI, EBX
	XOR	ESI, ESI
	MOV	SI, ES
	SHL	ESI, 4
	ADD	ESI, OFFSET tempBuffer
	MOV	EAX, TYPE HandleTable_struc
	MOV	CX, [total_handles]
	MUL	CL
	MOV	CX, AX
	CALL	DoMoveBlock
	ADD	EDI, EAX
; Initialize handle_name
	MOV	EBX, EDI
	SUB	EDI, EBP			; Get the offset
	MOV	[hndl_nam_ptr], EDI		; Save the beginning of handle_name
	MOV	DI, OFFSET tempBuffer
	XOR	EAX, EAX
	MOV	CX, [total_handles]
	SHL	CX, 2				; 8 bytes for each handle
	REP	STOSD
	MOV	EDI, EBX
	XOR	ESI, ESI
	MOV	SI, ES
	SHL	ESI, 4
	ADD	ESI, OFFSET tempBuffer
	MOV	EAX, TYPE HandleName_struc
	MOV	CX, [total_handles]
	MUL	CL
	MOV	CX, AX
	CALL	DoMoveBlock
	ADD	EDI, EAX
; Initialize save_map
	MOV	EBX, EDI
	SUB	EDI, EBP			; Get the offset
	MOV	[save_map_ptr], EDI		; Save the beginning of handle_name
	MOV	DI, OFFSET tempBuffer
	MOV	EAX, FREE
	MOV	CX, [total_handles]
	shl	cx,2
	.errnz	size HandleSaveMap_struc-16
	REP	STOSD
	MOV	EDI, EBX
	XOR	ESI, ESI
	MOV	SI, ES
	SHL	ESI, 4
	ADD	ESI, OFFSET tempBuffer

	mov	EAX,size HandleSaveMap_struc
;QEMS	MOV	EAX, TYPE EMS_window_struc * TOTAL_PF_WINDOWS

	MOV	CX, [total_handles]
	MUL	CL
	MOV	CX, AX
	CALL	DoMoveBlock
	ADD	EDI, EAX

ifdef 910707
;
;  Initialize the allocation bit maps for EMS and VCPI
;
	mov	ebx,edi
	sub	ebx,ebp

	mov	eax,[MaxPTEindex]	; number of bits required for AllocMap
	shr	eax,3			; bytes for AllocMap
	mov	[AllocMapPtr],ebx	; pointer to AllocMap
	add	ebx,eax			; add size to get to
	mov	[VCPIAllocMapPtr],ebx	; pointer for the VCPIAllocMap
;
;  Intialize AllocMap by copying buffer set up by VDMINIT
;
	mov	si,seg LAST
	movzx	esi,si
	shl	esi,4
	add	esi,offset tempAllocMap	; initial information set up by VDMINIT
	mov	ecx,tempAllocMapLen
	call	DoMoveBlock
	add	edi,ecx			; next destination
	add	eax,eax			; size for both alloc maps
	sub	eax,ecx			; remaining amount to clear
	mov	edx,eax
;
;  Clear temporary buffer
;
	push	di
	push	es
	mov	di,cs
	mov	es,di
	mov	di,offset tempBuffer
	xor	eax,eax
	mov	ecx,1024
	rep stosd
	pop	es
	pop	di
;
;  Clear the rest of the allocation maps
;
	mov	si,seg LAST
	movzx	esi,si
	shl	esi,4
	add	esi,offset tempBuffer
WSAMloop:
	mov	ecx,edx			; number of bytes to be cleared
	sub	edx,1000h
	jl	short WSAMcont
	mov	ecx,1000h
WSAMcont:
	call	DoMoveBlock
	add	edi,ecx
	cmp	edx,0
	jg	short WSAMloop
endif

; Initialize whatever is needed
; +++
;
;  Six segments to move up (_TEXT, _DATA, **TSS**, IDT, and GDT) ;TSSLEO QLEO
;
	mov	esi,offset LAST:MoveSegmentList
	mov	cx,NumSegExt
WSNextSegment:
	call	MoveSegment
	add	esi,size sMoveSegment
	loop	WSNextSegment

	;
	; Copy the EMS_window struc to EMS_window_ptr
	;
ifdef QEMS
	mov	si, _DATA
	shl	esi, 4
	add	esi, offset EMS_window
	mov	edi, [EMS_window_ptr]
	add	edi, ebp
	movzx	ecx, [context_save_area_size]
	sub	ecx, 4
	call	DoMoveBlock
endif
;
;  Save page directory address for loading CR3
;
	mov	eax,[page_directory]
	mov	gs:[PageD_addr],eax

;
; The GDT and IDT pointers are setup up so that when CEMM gets turned on,
; the LGDT and LIDT instructions will have their correct pointers.
;
	mov	ax,seg GDT	; DS:SI point to the GDT's entry location.
	mov	ds,ax
	mov	si,GDTD_GSEL


	mov	ax,seg R_CODE	; ES:DI point to data strucutre used by the LGDT.
	mov	es,ax
	assume	es:R_CODE
	mov	di,offset R_CODE:GDT_Ptr

	movsd	; The 8 byte descriptor is copied over as is.
	movsd

;
; Since only the first 6 bytes of the GDT pointer is needed for the base and
; linear address, the upper 8 bits of the linear address must be copied down
; to its proper location.
;
	mov	al,byte ptr es:[GDT_Ptr][7]
	mov	byte ptr es:[GDT_Ptr][5],al

;
; The exact same operations are done for the IDT pointer.
;
	mov	si,IDTD_GSEL
	mov	di,offset R_CODE:IDT_Ptr
	movsd
	movsd
	mov	al,byte ptr es:[IDT_Ptr][7]
	mov	byte ptr es:[IDT_Ptr][5],al

	pop	es
	pop	ds
	popad
	ret
WSMove	endp

;==============================================================================
;==
;==  AllocWS: Allocate work space and initialize GDT selectors with extended
;==	      memory addresses. (NOTE: Make sure address is above HMA so that
;==	      the A20 state will not affect access to these data areas.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of segment (work space)
;==	EBX = Alignment/boundary conditions
;==	EDX = DX is GDT entry, hiword is GDT alias
;==	ESI = MoveSegment structure pointer
;==
;==  Exit:
;==	R_CODE:[msg_flag],MEM_ERR_MSG - set if no memory available
;==
;==============================================================================
AllocWS	proc	near
	push	eax
	push	ebx

ifndef LC910610
AWSGetMemory:
	pop	ebx
	push	ebx
endif
;
;  Get extended memory for segment (work space)
;
	call	MemGet        		;Q: Enough memory available?
	jc	SHORT AWSnoMemory	; N: return error code (don't load)

ifndef LC910610
	cmp	ebx,110000h		;Q: Above the HMA?
	jb	short AWSGetMemory	; N: try again
endif
;
; Update destination address and size in sMoveSegment structure
;
	mov	cs:[esi].MSSize,eax	; save size of segment
	mov	cs:[esi].MSDest,ebx	; new extended memory address

	call	UpdateGDT		; update selector(s) in GDT

	mov	cs:[esi].MSSource,eax	; save old base address
	clc

AWSexit:
	pop	ebx
	pop	eax
	ret

AWSnoMemory:
	or	gs:[msg_flag],MEM_ERR_MSG
	stc
	jmp	short AWSexit

AllocWS	endp

;==============================================================================
;==
;==  UpdateGDT: Updates selector in GDT with new memory address.
;==
;==  Entry: (Real Mode)
;==	EAX = Size of object
;==	EBX = New memory address
;==	EDX = DX is GDT entry, hiword is GDT alias
;==
;==  Exit:
;==	EAX = Old base memory address
;== 	      GDT descriptor(s) updated.
;==
;==============================================================================
UpdateGDT proc	near
	push	ecx
	push	edi
	push	es

;
;  Acces to GDT via ES and get index for descriptor
;
	mov	di,seg GDT		; GDT not moved yet
	mov	es,di
	movzx	edi,dx			; index into GDT (selector)

;
;  Place limit of object in descriptor (assumes byte granularity)
;
	mov	ecx,eax
	dec	ecx
	cmp	ecx,10000h-1		;Q: Limit larger than 64K default?
	jbe	short UGcont		; N: continue
	mov	word ptr es:[edi][0],cx	; Y: limit 0.15
	shr	ecx,16
	or	byte ptr es:[edi][6],cl	; limit 16.19
UGcont:
;
;  Update source addresses in sMoveSegment structure from GDT
;
	movzx	eax,byte ptr es:[edi][4]; base 16.23
	shl	eax,16
	mov	ax,es:[edi][2]		; base 0.15

;
;  Now the descriptor in the GDT is updated.
;
	mov	es:[edi][2],bx		; new extended memory address
	ror	ebx,16
	mov	es:[edi][4],bl
	mov	es:[edi][7],bh
;
;  The alias selector is checked in the upper 16 bits of EDX.
;
	ror	edx,16
	cmp	dx,0			;Q: Alias descriptor?
	je	short UGexit		; N: all done
	movzx	edi,dx			; Y: index into GDT (alias)

;
;  Place limit of object in alias descriptor (assumes byte granularity)
;
	cmp	ecx,10000h-1		;Q: Limit larger than 64K default?
	jbe	short UGcont1		; N: continue
	mov	word ptr es:[edi][0],cx	; Y: limit 0.15
	shr	ecx,16
	or	byte ptr es:[edi][6],cl	; limit 16.19
UGcont1:
;
;  Update base address of alias
;

	mov	es:[edi][4],bl
	mov	es:[edi][7],bh
	ror	ebx,16
	mov	es:[edi][2],bx

UGexit:
	ror	edx,16
	pop	es
	pop	edi
	pop	ecx
	ret
UpdateGDT	endp

;==============================================================================
;==
;==  MoveSegment: Moves a segment to a destination address.
;==
;==  Entry: (Real Mode)
;==	ESI = MoveSegment structure pointer
;==
;==  Exit:
;==	CY set, if an error occured
;==
;==============================================================================
MoveSegment	proc	near
	push	ecx
	push	esi
	push	edi

;
;  Set up for moving segment: source,destination addresses and size
;
	mov	ecx,cs:[esi].MSsize
	mov	edi,cs:[esi].MSdest
	mov	esi,cs:[esi].MSsource

	call	DoMoveBlock		;Q: Was the move succesful?
	jc	SHORT MSerror		; N: error

	pop	edi
	pop	esi
	pop	ecx
	ret

MSerror:
	pop	edi
	pop	esi
	pop	ecx
	ret
MoveSegment	endp


;---------------------------------------------------------------------------
;
;	Procedure Name	: AllocARPTs
;
;	Allcates 1 4K page for each alternate register set and saves the
;	linear address of these pages in the table ARPTs
;
;--------------------------------------------------------------------------

AllocARPTs	proc	near

	push	cx
	xor	cx, cx
	xor	edx,edx

	mov	esi, [page_tables]	; esi - address of page table 0

	mov	cl, [total_register_sets]

ARPTnext:
	mov	eax, 4*1024		; eax - size of block = 4K
	mov	ebx, PAGE_BOUNDARY	; ebx  - need block on 4K boundary

	call	MemGet			; get block
	jc	short ARPTnomem		; not enuff mem
					; ebx - address of block
	mov	edi, ebx
	mov	[ARPT][edx*4], edi	; store address in ARPT
	inc	edx
	push	cx
	mov	cx, PAGE_SIZE
					; move cx bytes from esi to edi
	call	DoMoveBlock  		; initialize this to current PT0
	pop	cx
	jc	short ARPTnomem
	loop	ARPTnext

ARPTdone:
	pop	cx
	ret

ARPTnomem:
	or	gs:[msg_flag], MEM_ERR_MSG
	stc
	jmp	short ARPTdone

AllocARPTs 	endp
LAST	ends				; end of segment

	end				; end of module
