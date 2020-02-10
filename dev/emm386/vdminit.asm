.386p
page	58,132
;******************************************************************************
	title	VDM_Init - VDM initialization module
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;
;   Module:   VDM_Init - VDM initialization routine
;
;   Version:  2.00
;
;   Date:     June 3,1986
;
;   Author:	Steve Preston
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   06/03/86  Original	from VDM MAIN.ASM module
;   06/16/86  0.01	Added code to dword align LL buffer(SBP).
;   06/21/86  0.02	moved cld in init_pages (SBP) and save Eax.
;   06/28/86  0.02	Name change from CEMM386 to CEMM (SBP).
;   07/06/86  0.04	changed assume to _DATA and moved stack out of
;			_DATA (SBP).
;   07/10/86  0.05	Init of RCODEA_GSEL (SBP).
;   07/10/86  0.05	Added PageT_Seg (SBP).
;   05/08/87  2.00	Added init of RCODE_GSEL and RSS_GSEL (SBP).
;   10/12/88  3.32 (*D) Add VCPI code (DJM).
;   10/12/88  3.32 (*D) Fix math error by linker (RDV).
;   08/23/89  4.10      Initialize PAGED_GSEL to point to page directory (LC)
;   08/23/89  4.10	Initialize PTE's so linear = physical addresses (LC)
;
;******************************************************************************
;
;   Functional Description:
;
;	This module is the general initialization module for the Virtual DOS
;   Monitor part of CEMM.  This module initializes the protected mode
;   GDT, IDT, TSS (and I/O BitMap), and the Page Tables.  This module also
;   initializes all variables used by the VDM code.  This module returns
;   to the calling routine in Virtual Mode.
;
;******************************************************************************
.lfcond 				; list false conditionals
;******************************************************************************
;	P U B L I C S
;******************************************************************************
	public	VDM_Init
	public	InitPages
	public	handle0_PTEs
	public	tempBuffer
;QEMS	public	tempAllocMap
;QEMS	public	tempAllocMapLen
;******************************************************************************
;	D E F I N E S
;******************************************************************************
	INCLUDE ALLOCMEM.INC
	INCLUDE EMM386.INC
	include VDMseg.inc
	include VDMsel.inc
	include desc.inc
	include page.inc
	include emmfunct.inc
	include emmdata.inc
;******************************************************************************
;	E X T E R N A L   R E F E R E N C E S
;******************************************************************************

_TEXT SEGMENT

extrn	InitBitMap:far	; (vminit.asm)
EXTRN	TotalFreeMem: NEAR
EXTRN	MemGet: NEAR

_TEXT	ENDS

GDT	SEGMENT
extrn	GDTLEN:abs
GDT	ENDS

IDT	SEGMENT
extrn	IDTLEN:abs
IDT	ENDS

LAST	SEGMENT
extrn	PICInit:near
extrn	SetSegDesc:near
extrn	SegTo24:near
extrn	TotalFreeMem:near
extrn	DoMoveBlock:near
extrn	EstWorkSpace:near
extrn	SaveResetVector:near
extrn	NumOfUMBwindows:byte
extrn	Page4K:byte
extrn	UMBtable:byte
extrn	ext_mem:dword
extrn	HMAonSet:byte
extrn	max_pool_size:word
tempBuffer	DD	1024 DUP (0)
handle0_PTEs	DW	0
ifndef LC910611	; extra room for systems with 256K base memory and require backfill
Handle0_UMBs	dd	128 dup (0)
else
Handle0_UMBs	dd	64 dup (0)
endif

;QEMS tempAllocMap	dw	(((100000h shr 12) shr 3) shr 1) dup (0)
;QEMS tempAllocMapLen	equ	$-tempAllocMap

LAST	ENDS

R1_CODE	segment
	extrn	Int10_Hook: near
	extrn	Int11_Hook: near
	extrn	PrevInt10:dword
	extrn	PrevInt11:dword
R1_CODE	ends


;******************************************************************************
;	S E G M E N T	D E F I N I T I O N S
;******************************************************************************

;
;   code
;
LAST SEGMENT

	assume cs:LAST,ds:_DATA,es:_DATA,gs:R_CODE

;******************************************************************************
;	VDM_Init - VDM initialization routine
;
;
;    ENTRY:	Real Mode
;		DS = _DATA
;		GDT = GDT segment
;TSSQLEO	TSS = TSS segment
;
;    EXIT:	Real Mode
;		VDM Tables initialized
;		TSS initialized
;
;    USED:	none
;
;******************************************************************************
VDM_Init	proc	near
;
	pushf
	pushad
	cli

	call	SaveResetVector	; trap execution at the reset vector (F000:FFF0)
	call	VirHMA		; allocate memory for possible virtual HMA
	call	InitGdt 	; initialize GDT descriptors
;
;  initialize Page Table, I/O Bit Map and LIM h/w emulator
;
	call	InitPages		; initialize paging tables
	call	InitBitMap		; initialize I/O Bit Map
	call	InitWEITEK		; initialize Weitek hook (Int 11h)
	call	PICInit			; trap PIC I/O ports
;
;  initialize TSS,GDTR,IDTR
;
;	set ring 0 SS:SP in the TSS so we can take outer ring traps

	movzx	eax,gs:[StackTop]
	mov	[TSS].TSS386_ESP0,eax
	mov	[TSS].TSS386_SS0,VDMS_GSEL
;
; now set CR3 in the TSS
;
	mov	eax,cr3
	and	eax,0FFFh		; clear address bits
	or	eax,[page_directory]	; EAX = page dir 32 bit addr
	mov	dword ptr [TSS].TSS386_CR3,eax ; mov EAX into CR3 spot in TSS

ifdef TSSQLEO
	mov	ax, seg TSS
	mov	es,ax			; ES -> TSS
	xor	di,di			; ES:DI -> TSS

	movzx	eax,gs:[StackTop]
	mov	es:[di].TSS386_ESP0,eax
	mov	es:[di].TSS386_SS0,VDMS_GSEL

;LEO	mov	word ptr ES:[di.TSS386_ESP0lo],offset STACK:Stack0_top
;LEO	mov	word ptr ES:[di.TSS386_ESP0hi], 0

; now set CR3 in the TSS

	mov	eax,cr3
	and	eax,0FFFh		; clear address bits
	or	eax,[page_directory]	; EAX = page dir 32 bit addr
	mov	dword ptr ES:[di.TSS386_CR3],eax ; mov EAX into CR3 spot in TSS

endif ;TSSQLEO

;	clear the TSS busy flag

	mov	ax,seg GDT
	mov	es, ax			; DS:0 = ptr to gdt

	and	byte ptr es:[TSS_GSEL + 5], 11111101B
;
;  allocate scratch handle space
;
	movzx	eax,[NumberHandlePages]
	or	ax,ax			;Q: Did we allocate a handle space?
	jz	short VIexit		; N: exit
	shl	ax,10			; number of bytes needed
	mov	ebx,PARA_BOUNDARY	; paragraph boundede
	call	MemGet			; enough memory for a PTE per 16K page
	mov	[ScratchHandleSpace],ebx
	jnc	short VIexit
	or	[msg_flag],MEM_ERR_MSG
VIexit:
;
; and return
;
	popad
	popf
	ret
VDM_Init	endp

;**	InitGdt - initialise GDT
;
;	Some of the GDT is statically initialized. This routine
;	initializes the rest, except the LDT pointer which
;	changes dynamically, and the VDM stack which changes too.
;
;	ENTRY	GDT:0 = GDT to use.
;	EXIT	None
;	USES	All except BP
;
;	WARNING This code only works on a 286.
;		Designed to be called from real mode ONLY.

	public InitGdt
InitGdt proc near

	mov	ax,GDT
	mov	es,ax			; ES:0 -> gdt

	mov	ax,GDT
	call	SegTo24
	mov	cx,GDTLEN
	mov	ah,D_DATA0
	mov	bx,GDTD_GSEL
	call	SetSegDesc		; Set up GDT alias descriptor

	mov	ax,IDT
	call	SegTo24
	mov	cx,IDTLEN
	mov	ah,D_DATA0
	mov	bx,IDTD_GSEL
	call	SetSegDesc		; Set up IDT alias descriptor

ifdef TSSQLEO
	mov	ax,TSS
	call	SegTo24
	mov	cx,TSSLEN
	mov	ah,D_386TSS0
	mov	bx,TSS_GSEL
	call	SetSegDesc		; Set up TSS descriptor

	mov	ah,D_DATA0
	mov	bx,TSSD_GSEL
	call	SetSegDesc		; Set up TSS alias descriptor
endif

	mov	ax,seg _TEXT
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_CODE0
	mov	bx,VDMC_GSEL
	call	SetSegDesc		; Set up VDM Code descriptor

	mov	ax,_TEXT
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_DATA0
	mov	bx,VDMCA_GSEL
	call	SetSegDesc		; Set up VDM Code segment alias descr

	mov	ax,seg _DATA
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_DATA0
	mov	bx,VDMD_GSEL
	call	SetSegDesc		; Set up VDM Data descriptor

	mov	ax, seg STACK		; set up Ring 0 stack
	call	SegTo24
	mov	cx, offset STACK:Stack0_top
	mov	ah, D_DATA0
	mov	bx, VDMS_GSEL
	call	SetSegDesc

	mov	ax, seg R_STACK		; set up base protected mode stack
	call	SegTo24
	mov	cx, offset R_STACK:RealStackTop
	mov	ah, D_DATA0
	mov	bx, RMS_GSEL
	call	SetSegDesc

	mov	ax,seg R_CODE
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_CODE0
	mov	bx,RCODE_GSEL
	call	SetSegDesc		; Set up R_CODE Code descriptor

	mov	ax,R_CODE
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_DATA0
	mov	bx,RCODEA_GSEL
	call	SetSegDesc		; Set up R_CODE segment alias descriptor

	mov	ax,R1_CODE
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_CODE0
	mov	bx,R1CODE_GSEL
	call	SetSegDesc		; Set up R_CODE segment alias descriptor

	mov	ax,R1_CODE
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_DATA0
	mov	bx,R1CODEA_GSEL
	call	SetSegDesc		; Set up R_CODE segment alias descriptor


	mov	ax,seg _DATA
	call	SegTo24
	mov	cx,0			; 0 = 64K size
	mov	ah,D_DATA0
	mov	bx,RSS_GSEL
	call	SetSegDesc		; Set up real mode SS alias

	xor	dx,dx							   ;LEO
	mov	al,0C0h			; only low 24 bits of address here!;LEO
	mov	cx,0			; 64k long                         ;LEO
	mov	ah,D_DATA0		; data segment                     ;LEO
	mov	bx,DIAG_GSEL                                               ;LEO
	call	SetSegDesc		; Set up IDT alias descriptor      ;LEO
	mov	byte ptr es:[bx][7],80h	; set high 8 bits of address       ;LEO
									   ;LEO
ifdef BugMode
	mov	ax, dcode		; debugger code segment
	call	SegTo24
	mov	cx,0
	mov	ah,D_CODE0
	mov	bx,DEBC_GSEL
	call	SetSegDesc

	mov	ax, ddata		; debugger data segment
	call	SegTo24
	mov	cx,0
	mov	ah,D_DATA0
	mov	bx,DEBD_GSEL
	call	SetSegDesc
endif

	ret
InitGdt endp

;******************************************************************************
; InitPages
;
; ENTRY
;	DS = _DATA
; EXIT
;	page_directory - 32 bit address for the page directory
;	page_tables - 32 bit address for the page tables
;
; DESCRIPTION
;	This routine initializes a page directory and the page tables.
;	Both of these are aligned on a physical 4k page boundary.
;
;	The page dir and table set up by this routine maps the linear
;	addresses for Virtual mode programs into physical addresses using
;	the following scheme.
;	Linear Addr		Physical Addr
;	00000000h - 000FFFFFh	00000000h - 000FFFFFh
;	00100000h - 0010FFFFh	00000000h - 0000FFFFh  (64k wraparound)
;	00110000h - 0100FFFFh	00100000h - 00FFFFFFh  (top 15Meg of phys)
;
;QLEO: This routine has been patched to death!  Needs to be rewritten!
;
;******************************************************************************
InitPages	proc near
	PUSHAD
	PUSH	DS
	PUSH	ES
	MOV	AX, _DATA
	MOV	DS, AX
	ASSUME	DS: _DATA

	MOV	AX, LAST
	MOV	ES, AX
	ASSUME	ES: LAST		; ES is now LAST

ifndef LC910610
IPStart:
endif
	cmp	gs:[NoEMSset],FALSE	;Q: EMS available?
	jne	short no_base_memory	; N: no base memory

	CMP	[xma2ems], TRUE
	JE	SHORT no_base_memory

	MOV	AX, [end_of_base_memory_PTE]
	SUB	AX, [strtng_bs_wndw_PTE]; STARTING_BASE_WINDOW_PTE
	MOV	ES:[handle0_PTEs], AX	; # of 4K pages allocated to handle 0

no_base_memory:
	; *** Get the size of the unallocated memory
	; EAX = TotalFreeMem();
	CALL	TotalFreeMem
	SHR	EAX, 10			; Size in K bytes

	cmp	es:[ext_mem],FREE	;Q: Is L=ddd set?
	je	short IPcont		; N: no adjustment necessary
	mov	ebx,es:[ext_mem]	; Y: subtract memory that must be left

	sub	eax,ebx                 ;Q: Enough memory left over?
	jbe	allocErr		; N: no memory!

IPcont:
	CMP	EAX, MAX_SIZE		; Is size too big?
	JA	SHORT usePoolSize	; Y: use max_pool_size

	; *** Get the minimum of the two
	; if (EAX > max_pool_size) EAX = max_pool_size;
	CMP	AX,cs:[max_pool_size]	; Q: Is totalFree <= max_pool_size
	JBE	SHORT useTotalFree	; Y: use totalFree
usePoolSize:
	MOVZX	EAX, WORD PTR [max_pool_size] ; N: use max_pool_size

	; *** Get the number of PHs needed
useTotalFree:
	SHR	AX, 2
	ADD	AX, ES:[handle0_PTEs]
	SHR	AX, 2			; Rounded down to multiple of 16K

	ADD	AX, 0FFh		; Number of
	SHR	AX, 8			; PHs needed
	MOV	CX, AX			; Save PHs
	SHL	AX, 10			; Number of entries for PHs	;@PIW
ifdef QEMS
	MOV	[PH_entries], AX	; Save the number of entries	;@PIW
	MOV	[PH_boundary], AX	; Beyond the PHs boundary	;@PIW
	ADD	[PH_boundary], FIRST_HANDLE_PTE ; The limit of PHs	;@PIW
endif
	SHR	AX, 10			; Restore PHs			;@PIW

	; The logical handle PTE space is initialized.
;QEMS	mov	[starting_handle_PTE],FIRST_HANDLE_PTE
;
; Number of handle PTEs are calculated by the number of PHs
;
	mov	[NumberHandlePages],ax	; used to map handle space in PD

	; *** Get the number of PTs needed
	MOV	EBX, TopOfPhysicalMemory; Total system memory
	ADD	EBX, 4*1024*1024-1
	SHR	EBX, 22			; PTs needed

	MOV	DI, BX			; Save PTs

; The number of pages tables actually needed is now determined based on how
; much handle space is required.  AX is the number of unneeded page tables.

; Need to save in [number_page_tables], just the PTs and not the PHs space.

;QEMS	mov	gs:[number_page_tables], BX

; *** Allocate memory for page directory and page tables
; PD_addr = MemGet(PAGE_BOUNDARY, PD_size+PT_size);
	ADD	AX, 3			; PD, PTF0, and PTF1
	ADD	AX, BX			; Total tables (MAX=3+PHs+PTs)
	MOV	DX, AX			; For PAGED_GSEL
	SHL	EAX, 12			; Each table needs 4K bytes
	MOV	EBX, PAGE_BOUNDARY

	CALL	MemGet
	JC	allocErr

	; *** Save page directory address
	MOV	[page_directory], EBX

ifndef LC910610
;
;  Make sure page tables are not in HMA so that A20 state is not a concern
;
	cmp	ebx,110000h		;Q: In HMA?
	jb	IPStart			; Y: try again
endif
	PUSH	EBX

	; *** Set PAGED_GSEL
	MOV	AX, GDT
	MOV	ES, AX
	ASSUME	ES: GDT			; ES is now GDT

	MOV	SI, PAGED_GSEL
	DEC	DX			; Limit = size - 1
	MOV	ES:[SI], DX		; Limit
	MOV	ES:[SI+2], BX		; Base low
	SHR	EBX, 16
	MOV	BYTE PTR ES:[SI+4], BL	; Base medium
	MOV	BYTE PTR ES:[SI+5], D_DATA0 ; P-DPL-S-type
	MOV	BYTE PTR ES:[SI+6], 80h	; Granularity bit set
	MOV	BYTE PTR ES:[SI+7], BH	; Base high

	; *** Get page directory address
	POP	EBX

	; *** Set PAGET_GSEL
	MOV	EAX, 3			; PD, PTF0 and PTF1
	ADD	AX, CX			; Number of PH tables
	SHL	AX, 12			; Each table needs 4K bytes
	ADD	EBX, EAX		; Page table address
	; *** Save page table address
	MOV	[page_tables], EBX
	MOV	SI, PAGET_GSEL
	MOV	DX, DI			; Number of PTs to be initialized
	DEC	DI			; Limit = size - 1
	MOV	ES:[SI], DI		; Limit
	MOV	ES:[SI+2], BX		; Base low
	SHR	EBX, 16
	MOV	BYTE PTR ES:[SI+4], BL	; Base medium
	MOV	BYTE PTR ES:[SI+5], D_DATA0 ; P-DPL-S-type
	MOV	BYTE PTR ES:[SI+6], 80h	; Granularity bit set
	MOV	BYTE PTR ES:[SI+7], BH	; Base high

	MOV	AX, LAST
	MOV	ES, AX
	ASSUME	ES: LAST		; ES is now LAST

	; *** Initialize PD
	; Page directory contains the physical address of page tables
	; DX: the number PTs present
	MOV	DI, OFFSET tempBuffer
	MOV	EAX, [page_tables]
	OR	AX, P_AVAIL
	MOV	CX, DX			; Number of PTs
	CLD
initPD:
	STOSD
	ADD	EAX, PAGE_SIZE
	LOOP	initPD

	; *** Move to extended memory
	; CY = !MoveBlock(ESI, EDI, CX);
	XOR	ESI, ESI
	MOV	SI, LAST
	SHL	ESI, 4
	ADD	ESI, OFFSET tempBuffer
	MOV	EDI, [page_directory]
	MOV	CX, PAGE_SIZE
	CALL	DoMoveBlock
	JC	moveErr

	; *** Initialize PHs
	; Handle 0 has 24 16KB physical pages (256K-640K)
	MOV	DI, OFFSET tempBuffer
	MOVZX	EAX, [strtng_bs_wndw_PTE]
	SHL	EAX, 12
	OR	EAX, P_AVAIL
	cmp	gs:[NoEMSset],TRUE	;Q: Is it in [NoEMS] mode?
	je	initPTx			; Y: no PHs init PTs
	CMP	[xma2ems], TRUE		; Q: Is it in XMA2EMS mode?	;@PIW
	JE	SHORT not_allocated	; Y: Handle allocated bit clear	;@PIW
;QEMS	OR	EAX, HANDLE_ALLOCATED_BIT_SET				;@PIW
not_allocated:								;@PIW
	MOV	CX, ES:[handle0_PTEs]
;QLEO	JCXZ	SHORT no_handle0_PTE					;@PIW
	or	cx,cx
	jz	no_handle0_PTE
initPH0s:
;
; QLEO: Need to get address of back-filled RAM for Handle Space.
;
	cmp	[NumOfUMBwindows],0
	je	PHstep0E

	pushad
	movzx	ecx,cs:[NumOfUMBwindows]
	shr	eax,12
PHstep0A:
	cmp	cs:[UMBtable][ecx-1],al
	je	short PHstep0B
	loop	PHstep0A
	jmp	PHstep0D
PHstep0B:
	push	eax
	call	EstWorkSpace		; work space needed for code/data
	add	ebx,EMS_PAGE_SIZE
	add	ebx,10000h		; leave 64K for EMS

	call	TotalFreeMem		; get free memory available
	cmp	eax,ebx			;Q: Enough memory for UMB?
	pop	eax
	jb	PHstep0C		; N: no more UMB memory available
	shl	eax,12
	mov	esi,eax			; starting linear address for UMB
	mov	eax,EMS_PAGE_SIZE
	mov	ebx,EMS_BOUNDARY
;QEMS	mov	eax,PAGE_SIZE
;QEMS	mov	ebx,PAGE_BOUNDARY

	call	MemGet
	jc	PHstep0C		; if error, No UMB memory
	mov	cs:[Handle0_UMBs][ecx*4-4],ebx
	mov	cs:[Handle0_UMBs][ecx*4],ebx
	mov	cs:[Handle0_UMBs][ecx*4+4],ebx
	mov	cs:[Handle0_UMBs][ecx*4+8],ebx
	add	cs:[Handle0_UMBs][ecx*4+0],1000h
	add	cs:[Handle0_UMBs][ecx*4+4],2000h
	add	cs:[Handle0_UMBs][ecx*4+8],3000h
	push	edi			; save pointer to current PTE
	mov	edi,ebx			; starting physical address for UMB
	mov	ecx,EMS_PAGE_SIZE	; copy original contents to physical RAM
;QEMS	mov	ecx,PAGE_SIZE		; copy original contents to physical RAM
	call	DoMoveBlock
	pop	edi			; restore current PTE entry
	mov	eax,ebx
	shr	esi,12			; linear page
;QEMS	or	ax,HANDLE_ALLOCATED_BIT_SET+P_AVAIL
	or	ax,P_AVAIL+fEMSPageAllocated
	stosd
	and	ax,not fEMSPageAllocated
	add	eax,1000h
	stosd
	add	eax,1000h
	stosd
	add	eax,1000h
	stosd
	popad
	add	di,16
	add	eax,EMS_PAGE_SIZE
	dec	cx
	jnz	initPH0s
	jmp	short no_handle0_PTE
PHstep0C:
	mov	cs:[UMBtable][ecx-1],0	; not a valid UMB
	or	gs:[msg_flag],UMBmem_Msg; error message
PHstep0D:
	popad
;QEMS	jmp	short PHstep0E
PHstep0E:
	test	ah,30h			;Q: 16K boundary?
	jnz	short PHcont		; N: don't mark EMS allocated
	or	ax,fEMSPageAllocated
PHcont:
	STOSD
	and	ax,not fEMSPageAllocated
PHstep0F:
	ADD	EAX, PAGE_SIZE
;QLEO	LOOP	initPH0s
	dec	cx
	jnz	initPH0s
no_handle0_PTE:								;@PIW
	XOR	EAX, EAX
	MOV	CX, 1024		; Number of entries per table
	SUB	CX, ES:[handle0_PTEs]
initPHs0a:
	STOSD
	LOOP	initPHs0a

	; *** Move to extended memory
	; CY = !MoveBlock(ESI, EDI, CX);
	XOR	ESI, ESI
	MOV	SI, LAST
	SHL	ESI, 4
	ADD	ESI, OFFSET tempBuffer
	MOV	EDI, [page_directory]
	ADD	EDI, FIRST_HANDLE_PTE SHL 2
	MOV	CX, PAGE_SIZE
	CALL	DoMoveBlock
	JC	moveErr

;
;  Initialize PH1-7 with zero's (clear buffer)
;
	XOR	EAX, EAX
	MOV	DI, OFFSET tempBuffer
	MOV	CX, 1024		; Number of entries per table
initPHs1:
	STOSD
	LOOP	initPHs1

	; *** Move to extended memory
	; CY = !MoveBlock(ESI, EDI, CX);
	XOR	ESI, ESI
	MOV	SI, LAST
	SHL	ESI, 4
	ADD	ESI, OFFSET tempBuffer
	MOV	EDI, [page_directory]
	ADD	EDI, FIRST_HANDLE_PTE SHL 2
	MOV	CX, [NumberHandlePages]
	DEC	CX
	JCXZ	SHORT initPTx
initPHs1a:
	PUSH	CX
	MOV	ECX,PAGE_SIZE
	ADD	EDI,PAGE_SIZE		; next PH
	CALL	DoMoveBlock
	JC	moveErr
	POP	CX
	LOOP	initPHs1a

initPTx:
	; *** Initialize PTs
	; Page tables contain the physical address of pages
	; DX: the number PTs present
	MOV	EAX, P_AVAIL		; Pages are available
	MOV	EBX, [page_tables]	; Page table directory
initPTs:
	MOV	CX, 1024		; Number of entries per table
	MOV	DI, OFFSET tempBuffer	; Temporary buffer

	cmp	gs:[NoEMSset],TRUE	;Q: Is it in [NoEMS] mode?
	je	step1			; Y: skip this part

	CMP	[xma2ems], TRUE		; Q: Is it in XMA2EMS mode?	;@PIW
	JE	step1			; Y: Skip this part		;@PIW
; +++
	MOVZX	ESI, [strtng_bs_wndw_PTE]; Base window starts at 10..40
	SHL	ESI, 12			; It's in 4K chunk
	OR	SI, P_AVAIL		; Set it to available
initPT:
	CMP	EAX, ESI		; Q: Have we reach the end?	;@PIW
	JNE	step1			; N: Not yet			;@PIW

	MOV	SI, ES:[handle0_PTEs]	; Number of base window PTEs	;@PIW
	or	si,si
	jz	step1

;QEMS	OR	AX, PAGE_ALLOCATED_BIT_MASK ; Basw windows are allocated;@PIW
	SUB	CX, SI
step0:
	push	eax
	push	ecx

	cmp	[NumOfUMBwindows],0
	je	short step0C

	movzx	ecx,cs:[NumOfUMBwindows]
	shr	eax,12
step0A:
	cmp	cs:[UMBtable][ecx-1],al
	je	short step0B
	loop	step0A
	jmp	short step0C
step0B:
	mov	eax,cs:[Handle0_UMBs][ecx*4-4]
	or	ax,P_AVAIL
;QEMS	or	ax,P_AVAIL+PAGE_ALLOCATED_BIT_MASK ; RAM
	stosd
	shr	eax,12
;QEMS	bts	[tempAllocMap],ax	; Mark page allocated

	pop	ecx
	pop	eax
	jmp	short step0E
step0C:
	pop	ecx
	mov	eax,[esp]
	shr	eax,12
;QEMS	bts	[tempAllocMap],ax	; Mark page allocated
	pop	eax

	STOSD
step0E:
	ADD	EAX, PAGE_SIZE
	DEC	SI
	JNZ	short step0
;QEMS	AND	AX, NOT PAGE_ALLOCATED_BIT_MASK

step1:
	CMP	EAX, 0C0000h OR P_AVAIL	; Q: Is this C0000?		;@PIW
	JNE	SHORT step3		; N: Skip this part		;@PIW

	; SI = Check_CEGA();		; SI is number of pages to remap;@PIW
	CALL	Check_CEGA		; Q: Is the ROM there?		;@PIW
	JNC	SHORT step3		; N: Keep going			;@PIW
	SUB	CX, SI
	ADD	EAX, 20000h		; Change from C0000h to E0000h
step2:
	STOSD
	ADD	EAX, PAGE_SIZE
	DEC	SI
	JNZ	SHORT step2
	SUB	EAX, 20000h		; Change from E0000h to C0000h
step3:
	cmp	eax,100000h or P_AVAIL
	jae	step4

	cmp	[NumOfUMBwindows],0
	je	step4

	pushad
	movzx	ecx,cs:[NumOfUMBwindows]
	shr	eax,12
step3A:
	cmp	cs:[UMBtable][ecx-1],al
	je	short step3B
	loop	step3A
;QLEO	jmp	short step3F
	jmp	step3F
step3B:
	push	eax
	call	EstWorkSpace		; work space needed for code/data
	add	ebx,PAGE_SIZE
	cmp	gs:[NoEMSset],TRUE	;Q: NoEMS set?
	je	short step3C		; Y: memory not a factor
	add	ebx,10000h		; N: leave 64K for EMS
step3C:
	call	TotalFreeMem		; get free memory available
	cmp	eax,ebx			;Q: Enough memory for UMB?
	pop	eax
	jb	short step3E		; N: no more UMB memory available
	shl	eax,12
	mov	esi,eax			; starting linear address for UMB
	mov	eax,PAGE_SIZE
	mov	ebx,PAGE_BOUNDARY

	call	MemGet
	jc	short step3E		; if error, No UMB memory
	push	edi			; save pointer to current PTE
	mov	edi,ebx			; starting physical address for UMB
	mov	ecx,PAGE_SIZE		; copy original contents to physical RAM
	call	DoMoveBlock
	pop	edi			; restore current PTE entry
	mov	eax,ebx
	shr	esi,12			; linear page
	or	ax,P_AVAIL		; assume RAM
	test	cs:[Page4K][si],ROM	;Q: ROM: need write protection?
	jz	short Step3D		; N: continue
	and	ax,not P_WRITE		; Y: write protect the page
Step3D:
	stosd
	popad
	add	di,4
	jmp	short nextPTE
step3E:
	mov	cs:[UMBtable][ecx-1],0	; not a valid UMB
	or	gs:[msg_flag],UMBmem_Msg; error message
step3F:
	popad
	;jmp	 short step4
step4:
	cmp	eax,100000h
	jb	short step4A
	cmp	eax,110000h
	jae	short storePTE
	sub	eax,100000h		; create possible virtual HMA
	add	eax,gs:[HMAptr]
	stosd
	add	eax,100000h
	sub	eax,gs:[HMAptr]
	jmp	short nextPTE
step4A:

; EAX has a PTE for a non UMB page somewhere in the 1st Meg.  Check if
; this page as been selected as a WINdows page, and set the fWINpage
; bit if so.

	push	esi
	mov	esi, eax
	shr	esi, 12
	test	cs:[Page4K][si], WIN	; Q: WIN page?
	jz	short notWINpage
	or	ax, fWINpage		; Y: set fWINpage bit
	inc	[cntWinPages]		; and count how many there are
	stosd
	and	ax, NOT fWINpage
	pop	esi
	jmp	short nextPTE
notWINpage:
	pop	esi

storePTE:
	STOSD

nextPTE:
	ADD	EAX, PAGE_SIZE
	dec	cx
	jnz	initPT
	; *** Move to extended memory
	; CY = !MoveBlock(ESI, EDI, CX);
	XOR	ESI, ESI
	MOV	SI, LAST
	SHL	ESI, 4
	ADD	ESI, OFFSET tempBuffer
	MOV	EDI, EBX
	ADD	EBX, PAGE_SIZE
	MOV	CX, PAGE_SIZE
	CALL	DoMoveBlock
	JC	SHORT moveErr
	DEC	DX
	JNZ	initPTs
	JMP	SHORT return
moveErr:
	; To be continued...
allocErr:
	OR	GS:[msg_flag], MEM_ERR_MSG
return:
	POP	ES
	POP	DS
	POPAD
	RET
InitPages endp

;------------------------------------------------------------------------------
;	Check for E000h segment used for page frame
;	and for CEGA in system.
; EXIT: CY - if remap is necessary
;	SI - the number of pages to remap, valid only if CY is set.
;------------------------------------------------------------------------------
Check_CEGA	PROC
;	cmp	[PF_Base],0D000h			;Q: page frame > D000h ?
;	ja	short IT_E000 				;  Y: E000h used
;	jmp	IT_chkpt				;  N: E000h not used
;IT_E000:
	; Test would clear the carry flag, so no CLC is needed
	test	gs:[Current_State],fState_CEGAinst	;Q: CEGA installed ?
	jz	short IT_chkpt				;  N: nothing special
	PUSH	EAX
	push	fs
	mov	ax,seg R1_CODE
	mov	fs, ax
	assume	fs:R1_CODE

	or	gs:[Current_State],fState_CEGAmove	;  Y: we must move it
	;
	; Patch INT 10
	xor	ax,ax
	mov	ds,ax
	ASSUME	DS:ABS0
	mov	eax,dword ptr [int10] 		; EAX = old int10 vector
	mov	gs:[Int10_Save],eax	; save old vector

	; set int10 -> CEMM hook code

	mov	fs:[PrevInt10],eax	; set chain vector
	mov	ax, fs
	shl	eax,16
	mov	ax,offset R1_CODE:Int10_Hook
	mov	dword ptr [int10],eax 		; set int10 vector

	;
	; change page table entries for C0000-CxFFFh -> E0000h - ExFFFh
	;
	mov	ax,seg _DATA
	mov	ds,ax
	assume	ds:_DATA
	mov	SI,gs:[CROM_Length]	; length of opt rom in paras
	shr	SI,8			; length of opt rom in pages
	pop	fs
	assume	fs:nothing
	POP	EAX
	STC
IT_chkpt:
	RET
Check_CEGA	ENDP

;------------------------------------------------------------------------------
;	Check for Weitek installed
;------------------------------------------------------------------------------
InitWEITEK proc	near
	push	ds

	test	gs:[Weitek_State],fWeitek_Inst	;Q: Weitek installed ?
	jz	short IWexit			; N: don't patch int 11
						; Y: patch int 11h
	xor	ax,ax
	mov	ds,ax				; DS -> 0
	ASSUME	DS:ABS0

	push	fs
	mov	ax,seg R1_CODE
	mov	fs, ax
	assume	fs:R1_CODE

	shl	eax,16
	mov	ax,offset R1_CODE:Int11_Hook
	xchg	eax,dword ptr [int11] 		; EAX = old int11 vector
	mov	dword ptr fs:[PrevInt11],eax	; set chain vector

	pop	fs
	assume	fs:nothing

IWexit:
	pop	ds
	ret
InitWEITEK	endp

;==============================================================================
;==
;==  VirHMA: If virtual HMA has been specified, allocate 64K for it.
;==
;==  Entry: (Real Mode)
;==
;==  Exit:
;==	R_CODE:[msg_flag],MEM_ERR_MSG - set if no memory available
;==
;==============================================================================
VirHMA	proc	near
	push	eax
	push	ebx
	cmp	cs:[HMAonSet],TRUE	;Q: Virtual HMA desired?
	jne	short VHexit
;
;  Check if enough extended memory for virtual HMA
;
	call	EstWorkSpace		; work space needed for code/data
	add	ebx,8*1024*4		; space for PTs(4),PFs(2),PD(1),PH(1)
	add	ebx,10000h		; assume NoEMS set: 64K for for HMA
	cmp	gs:[NoEMSset],TRUE	;Q: NoEMS set?
	je	short VHcont		; Y: memory not a factor
	add	ebx,10000h		; N: leave 64K for EMS
VHcont:
	call	TotalFreeMem		; get free memory available
	cmp	eax,ebx			;Q: Enough memory for virtual HMA?
	jb	short VHnoMemory	; N: error
;
;  Get exteneded memory for virtual HMA
;
	mov	eax,10000h		; get 64K
	mov	ebx,PAGE_BOUNDARY	; on a page boundary

	call	MemGet        		;Q: Enough memory available?
	jc	short VHnoMemory	; N: return error code (don't load)
;
; Save physical pointer
;
	mov	gs:[HMAptr],ebx		; physical address
	mov	gs:[UMBHMA],TRUE	; don't allow CEMM to turn off

VHexit:
	pop	ebx
	pop	eax
	ret

VHnoMemory:
	or	gs:[msg_flag],UMBmem_MSG
	stc
	jmp	short VHexit
VirHMA	endp

LAST	ends

END

