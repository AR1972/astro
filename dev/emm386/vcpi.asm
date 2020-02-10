.386p
page	58,132
;******************************************************************************
	title	VCPI - VCPI function handler
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1988-1991
; (C) Copyright COMPAQ Computer Corp. 1988-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;		EMMLIB.LIB - Expanded Memory Manager Functions Library
;
; Module:	VCPI Function Handler
;
; Version:	1.00
;
; Date:		August 11, 1988
;
; Authors:	Dan Mazina
;		Dick Vail
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   8/11/88	1	initial code
;   3/26/88   4.01	Improve performance
;******************************************************************************
;
;   Functional Description:
;	This module implements all of the functions for the VCPI interface.
;
;******************************************************************************

;******************************************************************************
; PUBLICS
;******************************************************************************
public	VCPIDispatcher
public	VCPISwitchToProtected
public	VCPIPresenceDetection
public	VCPIModeInterface
public	VCPIMaxPhysicalAddress
public	VCPICountFreePages
public	VCPIAllocatePage
public	VCPIFreePage
public	VCPIPagePhysicalAddress
public	VCPIReadCR0
public	VCPIReadDebugRegisters
public	VCPILoadDebugRegisters
public	VCPIRead8259Mappings
public	VCPISet8259Mappings
public	VCPISwitchToProtected

public	VCPIProtectedDispatcher
public	VCPIInvalidFunction
public	VCPICountFreePagesProt
public	VCPIAllocatePageProt
public	VCPIFreePageProt
public	VCPISwitchToV86
public	VCPIDispReturn
public	VCPIProtDispReturn
public	VCPIReturnFromProt

ifdef PICtrap
public	VCPIProgPIC
endif

;******************************************************************************
; INCLUDES
;******************************************************************************
include vdmseg.inc
include vdmsel.inc
include vm386.inc
include oemdep.inc
include emm386.inc
include emmfunct.inc
include emmdata.inc
include page.inc
ifdef BETA
include dbg.inc
endif
include desc.inc
include debmac.inc
page
;******************************************************************************
; DEFINES
;******************************************************************************

page
;******************************************************************************
; EXTERNALS
;******************************************************************************

_TEXT	segment
ifdef PICtrap
	extrn	ProgramPIC:near
endif
	extrn	pIRQ0Handler:far
	extrn	pIRQ1Handler:far
	extrn	pIRQ2Handler:far
	extrn	pIRQ3Handler:far
	extrn	pIRQ4Handler:far
	extrn	pIRQ5Handler:far
	extrn	pIRQ6Handler:far
	extrn	pIRQ7Handler:far
	extrn	pIRQ8Handler:far
	extrn	pIRQ9Handler:far
	extrn	pIRQ10Handler:far
	extrn	pIRQ11Handler:far
	extrn	pIRQ12Handler:far
	extrn	pIRQ13Handler:far
	extrn	pIRQ14Handler:far
	extrn	pIRQ15Handler:far

	extrn	AddFreeEMS:near
	extrn	GetFreeEMS:near
	extrn	GrowEMSPool:near
	extrn	ShrinkEMSpool:near
	extrn	AddFreeEMS_far:far
	extrn	GetFreeEMS_far:far
	extrn	GrowEMSPool_far:far
	extrn	ShrinkEMSpool_far:far
ifdef QEMS
	extrn	SetPT0Cntxt:near
	extrn	RestorePT0Cntxt:near
endif
ifdef DEBUG
	extrn	pTestDbgIns:far
	extrn	pDebugPrintf:far
endif
_TEXT	ends

R_CODE	segment
	extrn	MasterPICVec:word
	extrn	SlavePICVec:word

ifdef PICtrap
	extrn	VirMasterPICVec:word
	extrn	VirSlavePICVec:word
	extrn	MasterIS:word
	extrn	LastOCW3:byte
endif

	public	VCPIClientData
VCPIClientData	label	dword
client_cr2	dd	?	; save area for VCPI client context
client_cr3	dd	?
client_jmpfar	label	dword			    ; keep these 3 in order
		dw	offset R1_CODE:VCC_load_seg ; keep these 3 in order
client_cs	dw	?			    ; keep these 3 in order
client_ss	dw	?
client_esp	dd	?
client_gdtr	df	?
client_idtr	df	?
client_ldtr	dw	?
client_tr	dw	?
ret_addr	dw	?

R_CODE	ends

R1_CODE segment
	extrn	RealStack2_top:byte
R1_CODE ends

page
;******************************************************************************
; SEGMENTS
;******************************************************************************

_TEXT	SEGMENT
assume	cs:_TEXT,ds:_DATA,es:nothing,gs:R_CODE,ss:STACK,fs:nothing
;***********************************************************************
; This is the jump table used by VCPIDispatcher.
;***********************************************************************
dispatch_vector label	word
	dw	OFFSET	VCPIPresenceDetection	; function 0h
	dw	OFFSET	VCPIModeInterface	; function 1h
	dw	OFFSET	VCPIMaxPhysicalAddress	; function 2h
	dw	OFFSET	VCPICountFreePages	; function 3h
	dw	OFFSET	VCPIAllocatePage	; function 4h
	dw	OFFSET	VCPIFreePage 		; function 5h
	dw	OFFSET	VCPIPagePhysicalAddress	; function 6h
	dw	OFFSET	VCPIReadCR0		; function 7h
	dw	OFFSET	VCPIReadDebugRegisters	; function 8h
	dw	OFFSET	VCPILoadDebugRegisters	; function 9h
	dw	OFFSET	VCPIRead8259Mappings 	; function 0Ah
	dw	OFFSET	VCPISet8259Mappings	; function 0Bh
;	dw	OFFSET	VCPISwitchToProtected	; function 0Ch

;***********************************************************************
; VCPIDispatcher - entry point for int 67 VCPI calls
;
; ENTRY
;	AL = function number
;	All other registers depend on the function.
; EXIT
;	Return registers depend on the function.
; DESCRIPTION
;	This procedure is called by EMM_pEntry when a VCPI function code
;	is detected.  This code checks for a valid function number and
;	calls the appropriate function to execute it.  The stack frame
;	is set up here.
;***********************************************************************
VCPIDispatcher	proc	far

		; The input registers are saved on the stack with BP pointing
		; to the stack frame.  All returned register values should be
		; placed in the stack frame.  NOTE: the BP value pushed is
		; CEMM's value, not the original program's BP.
		; Validate the function code for a number between 0 and 0Ch
		; If the function is invalid, then return an invalid error code.
	cmp	al,0Ch
	ja	short VD_Invalid_Function
	jb	short VD_Normal_Function
	PJmp	R1CODE_GSEL,R1_CODE:VCPISwitchToProtected

VD_Invalid_Function:
	mov	ah,INVALID_SUBFUNCTION
	ret

VD_Normal_Function:
	pushad
	movzx	ebp,sp

		; Call through the jump table where AL is the index into the
		; jump table. This code destroys the SI register and any
		; function needing it must reload it later.
	movzx	eax,al
	call	cs:dispatch_vector[eax*2]

VCPIDispReturn:
		; The return registers are set up from the stack frame.
		; NOTE: The calling programs BP will be restored in EMM_pEntry.
	popad
	retf
VCPIDispatcher	endp

page
;***********************************************************************
; VCPIPresenceDetection - Indicates that VCPI is supported
;
; ENTRY
;	GS = RCODEA_GSEL
; EXIT
;	AH = 0 (no error encountered)
;	BX = version number
; DESCRIPTION
;	This procedure loads the current version number and returns a
;	success code so that VCPI programs can detect VCPI support in CEMM.
;	The initialization is also done here for debug sake but should be
;	moved elsewhere before actual release.
;***********************************************************************
VCPIPresenceDetection	proc	near

		; The version number is loaded.
	mov	eax,gs:[VCPI_version]
	mov	word ptr [ebp.reg_EBX],ax
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
VCPIPresenceDetection	endp

page
;***********************************************************************
; VCPIModeInterface - Sets up the VCPI interface
;
; ENTRY
;	[EBP] ES:DI - ptr to 4k page table buffer
;	[EBP] DS:SI - ptr to three descriptor table entries in the GDT
; EXIT
;	AH = 0 (no error encountered)
;	DI - points to first PTE in the buffer that is not used
;	EBX - offset of the protected mode code entry point
; DESCRIPTION
;	This procedure sets up the protected mode interface that VCPI
;	uses.  This includes coping the first MB's PTE plus the CEMM
;	page table PTE's to the VCPI program's PT's.  Then three descriptors
;	are set up in the VCPI program's GDT: this code segment, this data
;	segment, and a descriptor to CEMM's PT's.
;
;	The location of CEMM's page tables are placed in the VCPI programs
;	page tables starting at linear address 1MB so that this VCPI code
;	can access them.  A descriptor is set up in the VCPI program's GDT
;	that will generate a base address for that 1MB linear address.
;	If the location of the PTE entries is changed then the descriptor's
;	base address must also be changed.
;***********************************************************************
VCPIModeInterface	proc	near

		; The 4k page table pointer is loaded into ES:EDI.
	movzx	edi,[ebp.reg_ES]
	shl	edi,4
	movzx	eax,word ptr [ebp.reg_EDI]
	add	edi,eax

	mov	bx,[NumberHandlePages]	; get this while we have access to _DATA
;
;  The first MB of linear memory has its PTE's copied over to the buffer.
;  There are 100h PTE's to be copied and each PTE uses 4 bytes.  ES:EDI are
;  the VCPI's PTE's while DS:SI are CEMM's PTE's. The PTE's User Bits must
;  be cleared.
;
	mov	ax,PAGET_GSEL
	mov	ds,ax
assume ds:nothing
	xor	esi,esi
	mov	ecx,100h
	cld

MI_copy_1MB_PTEs:
	lods	dword ptr ds:[esi]
	and	eax,0FFFFF1FFh		; clear user bits in PTE's
	STOS_DWORD_PTR_ES_EDI
	loop	MI_copy_1MB_PTEs

;
;  The base addresses for CEMM's PT's are now written to the VCPI program's
;  next PTE's.  The page tables plus the page directory is all lumped together.
;
	mov	eax,[PageD_Addr]
	mov	cx,bx			; number of pages for handle space
	add	cx,3			; add directory and page fault tables
	or	ax,P_AVAIL		; make it available
MIHandleSpace:
	STOS_DWORD_PTR_ES_EDI
	add	eax,1000h
	loop	MIHandleSpace
;
;  DI is set to the next PTE not being used.  To do this the segment value
;  must be subtracted off from the EDI address.
;
	movzx	eax,[ebp.reg_ES]
	shl	eax,4
	sub	edi,eax
	mov	word ptr [ebp.reg_EDI],di
;
;  A new work descriptor is set up in ES:EDI for the VCPI program's GDT.
;
	movzx	edi,[ebp.reg_DS]
	shl	edi,4
	movzx	eax,word ptr [ebp.reg_ESI]
	add	edi,eax
;
;  The three provided descriptors are used for designating the VCPI code
;  segment, VCPI data segment and CEMM page tables.  DS is still the GDT
;  alias, ES:EDI is the program's GDT.
;
	push	GDTD_GSEL
	pop	ds
;
; CEMM's descriptor info for the code segment is copied over.
;
	mov	esi,R1CODE_GSEL
	MOVS_DWORD_USING_DS_ESI
	MOVS_DWORD_USING_DS_ESI
;
; CEMM's descriptor info for the data segment is copied over.
;
	mov	esi,RCODEA_GSEL
	MOVS_DWORD_USING_DS_ESI
	MOVS_DWORD_USING_DS_ESI
;
; CEMM's descriptor info for the page table segment is copied.
;
	mov	esi,PAGED_GSEL
	MOVS_DWORD_USING_DS_ESI
	MOVS_DWORD_USING_DS_ESI
;
;  The actual base address is hard coded to coincide with the location in the
;  PT's that was loaded earlier at 1MB.
;
	mov	byte ptr es:[edi-4],10h
	mov	word ptr es:[edi-6],0000h
;
; The protected mode entry point is returned.
;
	mov	eax,OFFSET R1_CODE:VCPIProtectedDispatcher
	mov	[ebp.reg_EBX],eax
;
; All done
;
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
VCPIModeInterface	endp
assume ds:_DATA

page
;***********************************************************************
; VCPIMaxPhysicalAddress - return the maximum allocatable physical address
;
; ENTRY
;	GS = RCODEA_GSEL
; EXIT
;	AH - 0
;	EDX - The maximum physical address for VCPI memory pages.
; DESCRIPTION
;	This procedure returns the maximum physical address for allocatable
;	memory pages.  This is based on the assumption that all VCPI memory
;	is being allocated out of extended memory, not HIMEM.
;***********************************************************************
VCPIMaxPhysicalAddress	proc	near
;QEMS assume	ds:_DATA
;
; Should return top of physical memory. Should be intialized at INIT time.
;
	mov	eax,[TopOfPhysicalMemory] ; top of physical memory    ;LEO
	and	eax,not 0FFFh		  ; last PTE entry index      ;LEO

MPA_return_code:
	mov	[ebp.reg_EDX],eax
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
;QEMS assume	ds:R_CODE
VCPIMaxPhysicalAddress	endp

page
;***********************************************************************
; VCPICountFreePages - return the number of free 4k pages
;
; ENTRY
;	DS = R_CODE selector
;	ES = PAGET_GSEL selector
; EXIT
;	EDX = number of free pages
;	AH = 0
; DESCRIPTION
;	This procedure returns the number of current free 4k pages.  This
;	number is initialized and changed by AllocatePage and
;	FreePage.
;***********************************************************************
VCPICountFreePages	proc	near
ifdef QEMS
	mov	ax,gs
	mov	ds,ax
	movzx	eax,[free_4k_pages]
endif
	call	GetFreeEMS		; ax = # free 16k EMS pages

	movzx	eax, ax
	shl	eax, 2			; 16k to 4k pages

	movzx	ebx, [TopOfHandleSpace]
	sub	bx, [BotOfVCPIspace]
	sub	bx, [UsedVCPIPages]	; bx = # free 4k pages in VCPI space

	add	eax, ebx

	;;;pDebugF "VCPICountFreePages: returning %ld free 4k pages\n", <eax>

	mov	[ebp.reg_EDX],eax
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
VCPICountFreePages	endp

page
;==============================================================================
;==
;== VCPIAllocatePage: allocates a 4K page
;==
;== Entry
;==	DS = _DATA selector
;==	ES = DATA32_GSEL selector
;==	GS = R_CODE
;==
;== Exit
;==	EDX = physical address of page
;==	AH = 0 or FFFF
;==
;==============================================================================
	public	VCPIAllocatePage

VCPIAllocatePage proc	near

	AssertSegReg	gs, RCODEA_GSEL

;  Check if any VCPI pages are free

	mov	ax,PAGED_GSEL
	mov	es,ax
	assume	es:NOTHING

	mov	ax, [TopOfHandleSpace]
	sub	ax, [BotOfVCPIspace]
	sub	ax, [UsedVCPIPages]
	jbe	VAP_grow_vcpi_space

;  Should be a free page in VCPI space, find it

	movzx	esi, [TopOfHandleSpace]
	movzx	edi, [BotOfVCPIspace]
VAPloop1:
	bts	word ptr es:[esi*4-4], fVCPIPageAllocatedBit
	jnc	short VAPpageFound
	dec	si
	cmp	si,di			;Q: End of VCPI space?
	jne	short VAPloop1

;  No free pages in VCPI space, try to grow it

VAP_grow_vcpi_space:

	movzx	esi, [TopOfFreeEMSspace]
	cmp	si, [TopOfUsedEMSspace] ;Q: any easy to grab EMS pages free?
	jbe	short VAP_try_compact	;N: try compacting EMS handle space

	mov	cx, 4
	movzx	edi, [BotOfVCPIspace]
	sub	[TopOfFreeEMSspace], cx ; lower EMS space by 4 4k pages
	sub	[BotOfVCPIspace], cx	; increase VCPI space by the same

	cmp	si, di		    ;Q: [TopOfFreeEMSspace] = [BotOfVCPIspace]?
	je	VAP_mark_page	    ;Y: no need to move PTEs around

;  There is a gap between the bottom of VCPI space and the top of free EMS
;  space so the free EMS PTEs have to be moved up to VCPI land

	mov	bx, di			; save old [BotOfVCPIspace]

	cld
	movzx	ecx, cx 		; make sure only 16 bits used
	lea	esi, [esi*4-4*4]
	lea	edi, [edi*4-4*4]
	REP_MOVS_DWORD_USING_ES_ESI

	mov	cx, 4			; clear the old PTEs at old
	xor	eax, eax		;   [TopOfFreeEMSspace]
	lea	edi, [esi-4*4]
	REP_STOS_DWORD_PTR_ES_EDI

	movzx	esi, bx 		; following code expects SI = old bottom

VAP_mark_page:
	or	word ptr es:[esi*4-4], fVCPIPageAllocated

VAPpageFound:
	inc	[UsedVCPIPages]
	mov	edx,es:[esi*4-4]
	and	dx,not 0FFFh
	mov	byte ptr [bp][reg_EAX+1], SUCCESS
	mov	[bp][reg_EDX], edx
VAPexit:
	ret

;  No free EMS pages between [TopOfFreeEMSspace] and [TopOfUsedEMSspace],
;  can some be freed by compacting handle space?

VAP_try_compact:

	mov	ax, [TopOfFreeEMSspace]
	sub	ax, FIRST_HANDLE_PTE
	shr	ax, 2
	sub	ax, [UsedEMSpages]
	jbe	VAP_grow_pool

;  There is some free EMS, compact handle space

	pDebugF "VCPIAllocPage_near: compacting handle space, untested\n"
	pDebugBreak

	mov	ax,DATA32_GSEL
	mov	es,ax
	mov	ax,-1
	call	AddFreeEMS
	mov	ax,PAGED_GSEL
	mov	es,ax
	jmp	VAP_grow_vcpi_space

;  No free EMS to steal anywhere, try to grow the EMS pool from XMS

VAP_grow_pool:

	mov	ax,DATA32_GSEL
	mov	es,ax
	mov	bx, 1
	call	GrowEMSpool
	mov	ax,PAGED_GSEL
	mov	es,ax
	jnc	VAP_grow_vcpi_space

;  No memory available at all, tell caller

	mov	byte ptr [bp][reg_EAX+1], NOT_ENOUGH_FREE_MEM
	jmp	short VAPexit

VCPIAllocatePage   endp


ifdef QEMS
page
;***********************************************************************
; VCPIAllocatePage - return the address of a 4k page to be allocated
;
; ENTRY
;	DS = RCODEA_GSEL selector
;QEMS	ES = PAGET_GSEL selector
; EXIT
;	EDX = physical address of page
;	AH = 0 or FFFF
; DESCRIPTION
;	This procedure searches through the CEMM's page tables looking for
;	an unallocated 4k page.  It does this by checking the system reserved
;	bit which it sets when it allocates a page.  This code assumes that
;	all of VCPI's allocatable memory is contiguous in extended memory.
;
;***********************************************************************
VCPIAllocatePage	proc	near

		; Check to see if there are any free pages available.
	mov	ax,RCODEA_GSEL
	mov	fs,ax
ifdef QEMS
	mov	ax,PAGET_GSEL
	mov	es,ax
endif
	mov	ax, VDMD_GSEL
	mov	ds, ax
	cmp	fs:[free_4k_pages],0
	je	AP_no_pages
		; The beginning of the extended memory PTE's is loaded into
		; DS:SI and CX is initialized to the number of pages.
AP_do_setup:
ifdef QEMS
	;
	; AR
	; Ensure that that the PT 0 associated with Alternate Register set
	; 0 is mapped in at PAGET_GSEL base address.
	;
	call	SetPT0Cntxt
endif
	mov	bx,word ptr [AllocMapPtr]
	xor	si,si
	mov	cx,fs:[number_ext_mem_PTEs]
	jcxz	short AP_try_high_memory
	movzx	esi,fs:[starting_ext_mem_PTE]

AP_check_extended:
	bts	[bx],si
	jnc	short AP_found_page
	inc	ax
	inc	esi
	loop	AP_check_extended

		; The beginning of the high memory PTE's is loaded into
		; ES:SI and CX is initialized to the number of pages.
AP_try_high_memory:
	mov	cx,fs:[number_high_mem_PTEs]
	jcxz	short AP_try_conventional_memory
	movzx	esi,fs:[starting_high_mem_PTE]

AP_check_high:
	bts	[bx],si
	jnc	short AP_found_page
	inc	esi
	loop	AP_check_high

		; The conventional memory pool is tried.
AP_try_conventional_memory:
	mov	cx,fs:[number_conv_mem_PTEs]
	jcxz	short AP_no_pages
	movzx	esi,fs:[starting_conv_mem_PTE]
AP_check_conventional:
	bts	[bx],si
	jnc	short AP_found_page
	inc	esi
	loop	AP_check_conventional

AP_found_page:
	dec	fs:[free_4k_pages]
	mov	bx,word ptr [VCPIAllocMapPtr]
	bts	[bx],si
	shl	esi,P_SHIFT
	mov	[ebp.reg_EDX],esi
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
ifdef QEMS
	;
	; AR
	; Ensure that the current PT0 as indicated by the 0th page directory
	; entry is mapped in at PAGET_GSEL's base address.
	;
	call	RestorePT0Cntxt
endif
	ret

AP_no_pages:
	mov	byte ptr [ebp.reg_EAX + 1],NOT_ENOUGH_FREE_MEM
	ret

VCPIAllocatePage	endp
endif

page
;==============================================================================
;==
;== VCPIFreePage: Frees a previously allocated 4K page
;==
;== Entry
;==	EDX = physcial address of page to free up
;==
;== Exit
;==	AH = 0 or FFFF
;==
;==============================================================================
VCPIFreePage	proc	near

;
;  Search for page in VCPI space
;
	movzx	esi,[TopOfHandleSpace]
	movzx	edi,[BotOfVCPIspace]

	cmp	si,di			;Q: Any VCPI space defined yet?
	je	short VFPerr		; N: can't deallocate

	mov	ax,PAGED_GSEL
	mov	es,ax

	and	dx,not 0FFFh
	or	dx,P_AVAIL+fVCPIPageAllocated
VFPloop:
	mov	eax, es:[esi*4-4]
	and	ax, NOT fXMSPageAllocated	; ignore XMS bit
	cmp	edx, eax
	je	short VFPpageFound
	dec	si
	cmp	si,di			;Q: End of VCPI space?
	jne	short VFPloop		; N: continue
	jmp	short VFPerr		; Y: error, could not find page

VFPpageFound:
	dec	si
	and	word ptr es:[esi*4],not fVCPIPageAllocated

	mov	ax,gs		; DS = R_CODEA
	mov	ds,ax
	assume  ds:R_CODE
;
;  Check if this freed up a 16K page: If so, place it in the free EMS space
;
	PCall	R1CODE_GSEL,R1_CODE:VCPItoEMSspaceFar	; eax = page PTE if yes,
							;	0 if no
	dec	gs:[UsedVCPIPages]
	mov	byte ptr [bp][reg_EAX+1],SUCCESS

	bt	eax, fXMSPageAllocatedBit	;Q: was an XMS allocated page
	jnc	short VFPexit			;   moved to free EMS space?

	mov	ax, VDMD_GSEL
	mov	ds, ax
	mov	ax, DATA32_GSEL
	mov	es, ax
	call	ShrinkEMSpool			;Y: try to give back XMS memory

VFPexit:
	ret
	assume  ds:_DATA,es:nothing
;
;  If this is reached then no match was found. Set the error code and return.
;
VFPerr:
	mov	byte ptr [bp][reg_EAX+1],LOG_PAGE_RANGE
	jmp	short VFPexit

VCPIFreePage	endp


ifdef QEMS
;***********************************************************************
; VCPIFreePage - frees up a previously allocated 4K page
;
; ENTRY
;	EDX = physcial address of page to free up
;	DS = RCODEA_GSEL selector
;	ES = PAGET_GSEL selector
; EXIT
;	AH = 0 or FFFF
; DESCRIPTION
;	This procedure frees up a previously allocated page.  It does this by
;	checking the system reserved bit and the physical address for a match.
;	This code assumes that all of VCPI's allocatable memory is contiguous
;	in extended memory.
;***********************************************************************
VCPIFreePage	proc	near

	mov	ax,RCODEA_GSEL
	mov	fs,ax
ifdef QEMS
	mov	ax,PAGET_GSEL
	mov	es,ax
endif
	mov	ax, VDMD_GSEL
	mov	ds, ax
ifdef QEMS
	;
	; AR
	; Ensure that that the PT 0 associated with Alternate Register set
	; 0 is mapped in at PAGET_GSEL base address.
	;
	call	SetPT0Cntxt
endif
	mov	bx,word ptr [VCPIAllocMapPtr]
	shr	edx,P_SHIFT

	btr	[bx],dx
	jnc	short FP_fail

		; If the page is found then reset its in use bits.
	mov	bx,word ptr [AllocMapPtr]
	btr	[bx],dx
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	inc	fs:[free_4k_pages]
FP_done:
ifdef QEMS
	;
	; AR
	; Ensure that the current PT0 as indicated by the 0th page directory
	; entry is mapped in at PAGET_GSEL's base address.
	;
	call	RestorePT0Cntxt
endif
	ret
		; If this is reached then no match was found.
		; Set the error code and return.
FP_fail:
	mov	byte ptr [ebp.reg_EAX + 1],LOG_PAGE_RANGE
	jmp	short FP_done

VCPIFreePage	endp
endif
page
;***********************************************************************
; VCPIPagePhysicalAddress - return a pages physical address
;
; ENTRY
;	CX = page linear address shifted right by 12
; EXIT
;	AH = 0
;	EDX = physical address
; DESCRIPTION
;	This procedure returns the physcial address of a linear page.
;	This is done by using the page index as a lookup into CEMM's page
;	tables and reading the base address.
;***********************************************************************
VCPIPagePhysicalAddress	proc	near

		; The page index is checked to see if it is within the
		; first MB.  If not then set the error return.
	cmp	cx,100h
	jae	short PPA_invalid_index
	mov	ax,PAGET_GSEL
	mov	es,ax
	movzx	esi,cx
	mov	edx,es:[esi*4]
	and	edx,PTE_ADDRESS_BIT_MASK
	mov	[ebp.reg_EDX],edx
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret

PPA_invalid_index:
	mov	byte ptr [ebp.reg_EAX + 1],PHYS_PAGE_RANGE
	ret

VCPIPagePhysicalAddress	endp

page
;***********************************************************************
; VCPIReadCR0
;
; ENTRY
;	none
; EXIT
;	AH = 0
;	EBX = CR0
; DESCRIPTION
;	This procedure reads the current value of CR0 and returns it.
;***********************************************************************
VCPIReadCR0 proc	near

		; Store the CR0 value, set the status code and return.
	mov	eax,cr0
	mov	[ebp.reg_EBX],eax
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
VCPIReadCR0 endp

page
;***********************************************************************
; VCPIReadDebugRegisters
;
; ENTRY
;	ES:DI - ptr to array of 8 dwords for debug registers
; EXIT
;	AH = 0
; DESCRIPTION
;	This procedure reads the current value of the debug registers and
;	writes them into the provided data buffer.
;***********************************************************************
VCPIReadDebugRegisters	proc	near

		; A work selector is set up for ES:EDI.
	mov	ax,DATA32_GSEL
	mov	es,ax
	movzx	edi,[ebp.reg_ES]
	shl	edi,4
	movzx	eax,word ptr [ebp.reg_EDI]
	add	edi,eax

		; The debug registers are read and stored.
	cld
	mov	eax,dr0
	mov	es:[edi],eax
	add	edi,4
	mov	eax,dr1
	mov	es:[edi],eax
	add	edi,4
	mov	eax,dr2
	mov	es:[edi],eax
	add	edi,4
	mov	eax,dr3
	mov	es:[edi],eax
	add	edi,4
	xor	eax,eax
	mov	es:[edi],eax
	add	edi,4
	mov	es:[edi],eax
	add	edi,4
	mov	eax,dr6
	mov	es:[edi],eax
	add	edi,4
	mov	eax,dr7
	mov	es:[edi],eax
	add	edi,4

	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
VCPIReadDebugRegisters	endp

page
;***********************************************************************
; VCPILoadDebugRegisters
;
; ENTRY
;	ES:DI - pointer to an array of 8 dwords with the values
; EXIT
;	AH = 0
; DESCRIPTION
;	This procedure loads the debug registers with the values provided.
;***********************************************************************
VCPILoadDebugRegisters	proc	near

		; A work selector is set up for ES:ESI.
	mov	ax,DATA32_GSEL
	mov	es,ax
	movzx	esi,[ebp.reg_ES]
	shl	esi,4
	movzx	eax,word ptr [ebp.reg_EDI]
	add	esi,eax

		; The values are read in and loaded.
	cld
	lods	dword ptr es:[esi]
	mov	dr0,eax
	lods	dword ptr es:[esi]
	mov	dr1,eax
	lods	dword ptr es:[esi]
	mov	dr2,eax
	lods	dword ptr es:[esi]
	mov	dr3,eax
	lods	dword ptr es:[esi]
	lods	dword ptr es:[esi]
	lods	dword ptr es:[esi]
	mov	dr6,eax
	lods	dword ptr es:[esi]
	mov	dr7,eax

	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
VCPILoadDebugRegisters	endp

page
;***********************************************************************
; VCPIRead8259Mappings
;
; ENTRY
;	GS = RCODEA_GSEL
; EXIT
;	AH = 0
;	BX = vector mappings for master 8259
;	CX = vector mappings for slave 8259
; DESCRIPTION
;	This procedure returns the current values of the 8259 vector mappings.
;	There is no way to actually read the values so the original default
;	values are the DOS values.  However, if the 8259 mappings are changed
;	thru Set8259Mappings, then the new values will be returned.
;QLEO:  The values returned will be values that were programmed by CEMM.
;***********************************************************************
VCPIRead8259Mappings	proc	near
;QEMS assume	ds:_DATA

		; The stored values are loaded and returned.
	mov	ax,gs:[MasterPICVec]
	mov	word ptr [ebp.reg_EBX],ax
	mov	ax,gs:[SlavePICVec]
	mov	word ptr [ebp.reg_ECX],ax
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret
VCPIRead8259Mappings	endp

page
;***********************************************************************
; VCPISet8259Mappings
;
; ENTRY
;	BX = vector mappings for master 8259
;	CX = vector mappings for slave 8259
;	GS = RCODEA_GSEL
; EXIT
;	AH = 0
; DESCRIPTION
;	This procedure is for notification purposes only.  The code doesn't
;	do anything except save the values for later use during a read of
;	the 8259 mapping.  The VCPI program may have already set the new
;	values up before this call.
;QLEO:  Need to return an error if this VCPI call is attempted.  CEMM has
;	already reprogrammed the PICs to new values, and thus this call is
;	no longer valid.
;***********************************************************************
VCPISet8259Mappings proc	near

ifdef PICtrap
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret

VCPIProgPIC:
;
;  Program PIC if the ICWs are being virtualized
;
	call	ProgramPIC
endif

		; The data alias for the IDT is load.
	mov	ax,IDTD_GSEL
	mov	es,ax

		; The slave's value is saved in dx.
	mov	dx,cx

		; If the current 8259 vector is DOS's value, then the IDT
		; entries are left alone as CEMM has processor interrupts
		; connected into here.
	cmp	gs:[MasterPICVec],DOS_MASTER_VECTOR
	je	short SM_check_master_vector

	cmp	gs:[MasterPICVec],50h		; leave handlers at 50h
	je	short SM_check_master_vector

		; If the current 8259 is different than DOS, clear out the
		; old entries so they will generate GP's if accessed.
	movzx	esi,gs:[MasterPICVec]
	mov	cx,8
SM_clear_master_IDTs:
	mov	dword ptr es:[esi*8],0
	mov	dword ptr es:[esi*8 + 4],0
	inc	esi
	loop	SM_clear_master_IDTs

		; If the new 8259 vector is DOS's value, then the IDT
		; entries are left alone as they are already set up.
SM_check_master_vector:
 	mov	gs:[MasterPICVec],bx
;
;  Assume master PIC is being programmed to base vector locations
;
ifdef PICtrap
	mov	gs:[VirMasterPICVec],bx
endif

	cmp	bx,DOS_MASTER_VECTOR
	je	SM_check_slave_IDTs

	cmp	bx,50h			; use handlers at 50h
	je	SM_check_slave_IDTs

		; If the new 8259 vector is different than DOS's, the IDT
		; entries are mapped into the special trap routines that
		; handle hardware interrupts.
	mov	cx,8
	movzx	esi,bx
SM_load_master_IDTs:
	mov	word ptr es:[esi*8 + 2],VDMC_GSEL
	mov	byte ptr es:[esi*8 + 5],D_386INT0
	inc	esi
	loop	SM_load_master_IDTs

	movzx	esi,bx
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ0Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ1Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ2Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ3Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ4Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ5Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ6Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ7Handler

		; If the current 8259 vector is DOS's value, then the IDT
		; entries are left alone as CEMM has these entries set up.
SM_check_slave_IDTs:
	cmp	gs:[SlavePICVec],DOS_SLAVE_VECTOR
	je	short SM_check_slave_vector

	cmp	gs:[SlavePICVec],50h		; leave 50h handlers
	je	short SM_check_slave_vector

		; If the current 8259 is different than DOS, clear out the
		; old entries so they will generate GP's if accessed.
	movzx	esi,gs:[SlavePICVec]
	mov	cx,8
SM_clear_slave_IDTs:
	mov	dword ptr es:[esi*8],0
	mov	dword ptr es:[esi*8 + 4],0
	inc	esi
	loop	SM_clear_slave_IDTs

		; If the new 8259 vector is DOS's value, then the IDT
		; entries are left alone as they are already set up.
SM_check_slave_vector:
 	mov	gs:[SlavePICVec],dx
ifdef PICtrap
	mov	gs:[VirSlavePICVec],dx
endif

	cmp	dx,DOS_SLAVE_VECTOR
	je	SM_exit

	cmp	dx,50h		; use 50h handlers
	je	SM_exit

		; If the new 8259 vector is different than DOS's, the IDT
		; entries are mapped into the special trap routines that
		; handle hardware interrupts.
	mov	cx,8
	movzx	esi,dx
SM_load_slave_IDTs:
	mov	word ptr es:[esi*8 + 2],VDMC_GSEL
	mov	byte ptr es:[esi*8 + 5],D_386INT0
	inc	esi
	loop	SM_load_slave_IDTs

	movzx	esi,dx
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ8Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ9Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ10Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ11Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ12Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ13Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ14Handler
	inc	si
	mov	word ptr es:[esi*8],offset _TEXT:pIRQ15Handler

SM_exit:
ifndef PICtrap
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
endif
	ret
VCPISet8259Mappings endp


_TEXT	ends

	page
R1_CODE	segment
	assume	cs:R1_CODE,ds:R_CODE,es:nothing,ss:STACK,fs:NOTHING,gs:NOTHING

;***********************************************************************
; This is the jump table used by VCPIProtectedDispatcher.
;***********************************************************************
protected_vector	label	word
	dw	OFFSET	VCPIInvalidFunction 	; function 0h
	dw	OFFSET	VCPIInvalidFunction 	; function 1h
	dw	OFFSET	VCPIInvalidFunction 	; function 2h
	dw	OFFSET	VCPICountFreePagesProt	; function 3h
	dw	OFFSET	VCPIAllocatePageProt	; function 4h
	dw	OFFSET	VCPIFreePageProt	; function 5h
	dw	OFFSET	VCPIInvalidFunction 	; function 6h
	dw	OFFSET	VCPIInvalidFunction 	; function 7h
	dw	OFFSET	VCPIInvalidFunction 	; function 8h
	dw	OFFSET	VCPIInvalidFunction 	; function 9h
	dw	OFFSET	VCPIInvalidFunction 	; function 0Ah
	dw	OFFSET	VCPIInvalidFunction 	; function 0Bh
page
;***********************************************************************
; VCPIProtectedDispatcher - entry point for protected mode VCPI calls
;
; ENTRY
;	AL = function number
;	All other registers depend on the function.
; EXIT
;	All registers depend on the function.
; DESCRIPTION
;	This procedure is called by the VCPI program directly while in
;	protected mode, no INT 67h is done.  The stack frame
;	is set up here.
;***********************************************************************
VCPIProtectedDispatcher	proc	far

		; Check for the function 'Switch to Protected Mode'.  This
		; call never returns to here so a stack frame is not set up.
	cmp	al,0Ch
	ja	short PVD_Invalid_Function
	jb	short PVD_Normal_Function
	jmp	VCPISwitchToV86

PVD_Invalid_Function:
	mov	ah,INVALID_SUBFUNCTION
	db	66h
	ret
		; The input registers are saved on the stack with BP pointing
		; to the stack frame.  All returned register values should be
		; placed in the stack frame.  NOTE: The BP value pushed here
		; is the original BP since the procedure is called directly.
		; The segment registers are saved to be safe.
PVD_Normal_Function:
	push	ds
	push	es
	pushad
	mov	ebp,esp

		; Since these procedures can be called by either GDT, the
		; ES and DS registers are setup here to hold PAGET_GSEL
		; selector and R_CODE selector respectively.

		; It is known that the
		; data selector follows the code selector so the size of a
		; descriptor entry is added to the code selector.  The same
		; is true of the page table selector.
	ror	eax,8			;save al in upper part

	mov	ax,cs			;cs is our code selector
	add	ax,8			;next GDT desc. is data
	mov	ds,ax			;R_CODE data
;QEMS	assume	ds:R_CODE
	add	ax,8			;next GDT desc. is ABS0/DATA32_GSEL
	mov	es,ax			;_DATA
;QEMS	ASSUME	ES:_DATA

		; The function is called through the jump table.
	shr	eax,24			;recover al for jump
	call	cs:protected_vector[eax*2]

VCPIProtDispReturn:

		; Restore the return registers.
	popad
	pop	es
	pop	ds

		; A far USE32 bit return is done to the VCPI code.
	db	66h
	retf

VCPIProtectedDispatcher	endp

;***********************************************************************
; VCPIInvalidFunction
;
; ENTRY
;	none
; EXIT
;	EAX = FAILURE
; DESCRIPTION
;	This procedure is for internal use only.  It sets up the return
;	value to indicate that an invalid function number was specified.
;	This is done in a separate routine for handling invalid functions
;	in the jump table.
;***********************************************************************
VCPIInvalidFunction proc	near
	mov	byte ptr [ebp.reg_EAX + 1],INVALID_SUBFUNCTION
	ret
VCPIInvalidFunction endp

page
;***********************************************************************
; VCPICountFreePagesProt - return the number of free 4k pages
;
; ENTRY
;	DS = R_CODE selector
;	ES = PAGET_GSEL selector
; EXIT
;	EDX = number of free pages
;	AH = 0
; DESCRIPTION
;	This procedure returns the number of current free 4k pages.  This
;	number is initialized and changed by AllocatePage and
;	FreePage.
;***********************************************************************
	public	VCPICountFreePagesProt
	assume	ds:R_CODE,es:nothing,fs:NOTHING,gs:NOTHING

VCPICountFreePagesProt	proc	near
;QEMS	movzx	eax,[free_4k_pages]

	pushf
	call	VCPIServerContext	; disables interrupts
	assume	ds:_DATA,es:NOTHING,gs:R_CODE

	;;;pDebugF "VCPICountFreePagesProt, untested code\n"
	;;;pDebugBreak

	Pcall	VDMC_GSEL,_TEXT:GetFreeEMS_far	; ax = # free 16k EMS pages

	movzx	eax, ax
	shl	eax, 2			; 16k to 4k pages

	movzx	ecx, [TopOfHandleSpace]
	sub	cx, [BotOfVCPIspace]
	sub	cx, [UsedVCPIPages]	; cx = # free 4k pages in VCPI space

	add	ecx, eax

	;;;pDebugF "VCPICountFreePagesProt: returning %ld free 4k pages\n", <ecx>

	call	VCPIClientContext
	assume	ds:R_CODE,es:nothing,gs:NOTHING
	popf

	mov	[ebp.reg_EDX],ecx
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret

VCPICountFreePagesProt	endp

page
;==============================================================================
;==
;== VCPIAllocatePageProt: allocates a 4K page
;==
;== Entry
;==	DS = R_CODE
;==	ES = PAGED_GSEL selector
;==
;== Exit
;==	EDX = physical address of page
;==	AH = 0 or FFFF
;==
;==============================================================================
VCPIAllocatePageProt	proc	near

;
;  Check to see if any pages are available
;
	mov	ax,[TopOfHandleSpace]
	sub	ax,[BotOfVCPIspace]
	sub	ax,[UsedVCPIPages]		; free pages in VCPI space

	mov	bx, [TopOfFreeEMSspace]
	sub	bx, [TopOfUsedEMSspace] 	; free pages in EMS space
	add	ax, bx				;   that we can get at

	or	ax,ax			;Q: Any VCPI pages left?
	jz	VAPP_grow_VCPI_space

;
;  Allocate a VCPI page
;
	movzx	esi,[TopOfHandleSpace]
	movzx	edi,[BotOfVCPIspace]
	cmp	si,di			;Q: Any VCPI space defined yet?
	je	short VAPPcont
VAPPloop1:
	bts	word ptr es:[esi*4-4],fVCPIPageAllocatedBit
	jnc	short VAPPpageFound
	dec	si
	cmp	si,di			;Q: End of VCPI space?
	jne	short VAPPloop1
VAPPcont:
	movzx	esi, [TopOfFreeEMSspace]
	cmp	si, [TopOfUsedEMSspace]     ;Q: any easy to get EMS pages free?
	jbe	short VAPP_grow_VCPI_space  ;N:

	mov	cx, 4
	movzx	edi, [BotOfVCPIspace]
	sub	[TopOfFreeEMSspace], cx ; lower EMS space by 4 4k pages
	sub	[BotOfVCPIspace], cx	; increase VCPI space by the same

	cmp	si, di		    ;Q: [TopOfFreeEMSspace] = [BotOfVCPIspace]?
	je	VAPP_mark_page	    ;Y: no need to move PTEs around

;  There is a gap between the bottom of VCPI space and the top of free EMS
;  space so the free EMS PTEs have to be moved up to VCPI land

	mov	bx, di			; save old [BotOfVCPIspace]

	cld
	movzx	ecx, cx 		; make sure only 16 bits used
	lea	esi, [esi*4-4*4]
	lea	edi, [edi*4-4*4]
	REP_MOVS_DWORD_USING_ES_ESI

	mov	cx, 4			; clear the old PTEs at old
	xor	eax, eax		;   [TopOfFreeEMSspace]
	lea	edi, [esi-4*4]
	REP_STOS_DWORD_PTR_ES_EDI

	movzx	esi, bx 		; following code expects SI = old bottom

VAPP_mark_page:
	or	word ptr es:[esi*4-4],fVCPIPageAllocated

VAPPpageFound:
	inc	[UsedVCPIPages]
	mov	eax,es:[esi*4-4]
	and	ax,not 0FFFh
	mov	[ebp][reg_EDX],eax
	mov	byte ptr [ebp][reg_EAX+1],SUCCESS

VAPPexit:
	ret

VAPPnoPages:
	mov	byte ptr [ebp][reg_EAX+1],NOT_ENOUGH_FREE_MEM
	jmp	short VAPPexit

VAPP_grow_vcpi_space:

	mov	ax, [TopOfFreeEMSspace] ; If there are free EMS pages, compact
	sub	ax, FIRST_HANDLE_PTE	;   handle space so they can be used.
	shr	ax, 2
	sub	ax, [UsedEMSpages]
	jbe	VAPP_grow_pool

	pushf
	call	VCPIServerContext	; disables interrupts
	assume	ds:_DATA,es:NOTHING,gs:R_CODE

	pDebugF "VCPIAllocPageProt: compacting handle space, untested\n"
	pDebugBreak

	mov	ax, -1
	Pcall	VDMC_GSEL,_TEXT:AddFreeEMS_far	; Compact EMS handle space

	call	VCPIClientContext
	assume	ds:R_CODE,es:nothing,gs:NOTHING
	popf
	jmp	VCPIAllocatePageProt	; The allocation will work now

VAPP_grow_pool:

	pushf
	call	VCPIServerContext	; disables interrupts
	assume	ds:_DATA,es:NOTHING,gs:R_CODE

	mov	bx, 1
	Pcall	VDMC_GSEL,_TEXT:GrowEMSpool_far ; Add mem to the EMS pool
	rcl	cl, 1				; save CY in cl

	call	VCPIClientContext
	assume	ds:R_CODE,es:nothing,gs:NOTHING
	popf

	test	cl, 1			; Q: did GrowEMSpool_far set carry?
	jz	VCPIAllocatePageProt	; N: try to alloc again
	jmp	short VAPPnoPages	; Y: can't allocate a page


VCPIAllocatePageProt	endp

ifdef QEMS
;***********************************************************************
; VCPIAllocatePageProt - return the address of a 4k page to be allocated
;
; ENTRY
;	DS = RCODEA_GSEL selector
;	ES = PAGET_GSEL selector
; EXIT
;	EDX = physical address of page
;	AH = 0 or FFFF
; DESCRIPTION
;	This procedure searches through the CEMM's page tables looking for
;	an unallocated 4k page.  It does this by checking the system reserved
;	bit which it sets when it allocates a page.  This code assumes that
;	all of VCPI's allocatable memory is contiguous in extended memory.
;
;***********************************************************************
VCPIAllocatePageProt	proc	near
	cmp	[free_4k_pages],0
	je	short PAP_no_pages
		; The beginning of the extended memory PTE's is loaded into
		; DS:SI and CX is initialized to the number of pages.
PAP_do_setup:
	mov	bx,word ptr es:[AllocMapPtr]
	xor	esi,esi
	mov	cx,[number_ext_mem_PTEs]
	jcxz	short PAP_try_high_memory
	movzx	esi,[starting_ext_mem_PTE]

PAP_check_extended:
	bts	es:[bx],si
	jnc	short PAP_found_page
	inc	esi
	loop	PAP_check_extended

		; The beginning of the high memory PTE's is loaded into
		; ES:SI and CX is initialized to the number of pages.
PAP_try_high_memory:
	mov	cx,[number_high_mem_PTEs]
	jcxz	short PAP_try_conventional_memory
	movzx	esi,[starting_high_mem_PTE]

PAP_check_high:
	bts	es:[bx],si
	jnc	short PAP_found_page
	inc	esi
	loop	PAP_check_high

		; The conventional memory pool is tried.
PAP_try_conventional_memory:
	mov	cx,[number_conv_mem_PTEs]
	jcxz	short PAP_no_pages
	movzx	esi,[starting_conv_mem_PTE]
PAP_check_conventional:
	bts	es:[bx],si
	jnc	short PAP_found_page
	inc	esi
	loop	PAP_check_conventional

PAP_found_page:
	mov	bx,word ptr es:[VCPIAllocMapPtr]
	dec	[free_4k_pages]
	bts	es:[bx],si
	shl	esi,P_SHIFT
	mov	[ebp.reg_EDX],esi
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	ret

PAP_no_pages:
	mov	byte ptr [ebp.reg_EAX + 1],NOT_ENOUGH_FREE_MEM
	ret

VCPIAllocatePageProt	endp
endif

page
;==============================================================================
;==
;== VCPIFreePageProt: Frees a previously allocated 4K page
;==
;== Entry
;==	EDX = physcial address of page to free up
;==
;== Exit
;==	AH = 0 or FFFF
;==
;==============================================================================
VCPIFreePageProt proc	near
;
;  Search for page in VCPI space
;
	movzx	esi,[TopOfHandleSpace]
	movzx	edi,[BotOfVCPIspace]

	cmp	si,di			;Q: Any VCPI space defined yet?
	je	short VFPPerr		; N: can't deallocate

	and	dx,not 0FFFh
	or	dx,P_AVAIL+fVCPIPageAllocated
VFPPloop:
	mov	eax, es:[esi*4-4]
	and	ax, NOT fXMSPageAllocated	; ignore XMS bit
	cmp	edx, eax
	je	short VFPPpageFound
	dec	si
	cmp	si,di			;Q: End of VCPI space?
	jne	short VFPPloop		; N: continue
	jmp	short VFPPerr		; Y: error, could not find page

VFPPpageFound:
	dec	si
	and	word ptr es:[esi*4],not fVCPIPageAllocated

;
;  Check if this freed up a 16K page: If so, place it in the free EMS space
;
	call	VCPItoEMSspace		; eax = page PTE if yes, 0 no

	dec	[UsedVCPIPages]
	mov	byte ptr [ebp][reg_EAX+1],SUCCESS

	bt	eax, fXMSPageAllocatedBit	;Q: was an XMS allocated page
	jnc	short VFPPexit			;   moved to free EMS space?

	pushf
	call	VCPIServerContext	; disables interrupts
	assume	ds:_DATA,es:NOTHING,gs:R_CODE

	;;;pDebugF "VCPIFreePageProt: shrinking EMS pool, untested\n"
	;;;pDebugBreak

	Pcall	VDMC_GSEL,_TEXT:ShrinkEMSpool_far   ; try to give back XMS mem

	call	VCPIClientContext
	assume	ds:R_CODE,es:nothing,gs:NOTHING
	popf

VFPPexit:
	ret
;
;  If this is reached then no match was found. Set the error code and return.
;
VFPPerr:
	mov	byte ptr [ebp][reg_EAX+1],LOG_PAGE_RANGE
	jmp	short VFPPexit
VCPIFreePageProt	endp

ifdef QEMS
;***********************************************************************
; VCPIFreePageProt - frees up a previously allocated 4K page
;
; ENTRY
;	EDX = physcial address of page to free up
;	DS = RCODEA_GSEL selector
;	ES = PAGET_GSEL selector
; EXIT
;	AH = 0 or FFFF
; DESCRIPTION
;	This procedure frees up a previously allocated page.  It does this by
;	checking the system reserved bit and the physical address for a match.
;	This code assumes that all of VCPI's allocatable memory is contiguous
;	in extended memory.
;***********************************************************************
VCPIFreePageProt	proc	near
	shr	edx,P_SHIFT

	mov	bx,word ptr es:[VCPIAllocMapPtr]
	btr	es:[bx],dx
	jnc	short PFP_fail

		; If the page is found then reset its in use bits.
	mov	bx,word ptr es:[AllocMapPtr]
	btr	es:[bx],dx
	mov	byte ptr [ebp.reg_EAX + 1],SUCCESS
	inc	[free_4k_pages]
	ret
		; If this is reached then no match was found.
		; Set the error code and return.
PFP_fail:
	mov	byte ptr [ebp.reg_EAX + 1],LOG_PAGE_RANGE
	ret

VCPIFreePageProt	endp
endif
;***********************************************************************
; VCPISwitchToV86
;
; ENTRY
;	Interrupts disabled
;	SS:ESP - stack frame with valid selectors
;	DS - selector used for protected mode interface
; EXIT
;	interrupts disabled
; DESCRIPTION
;	This procedure restores CEMM's system registers and transfers
;	control back to the VCPI program's V86 code.  This code turns off
;	interrupts for the duration of the call.  The stack is cleaned up
;	since this does not return back out to the protected mode code
;	that called it.
;
;	The nested task bit is not explicitly set since this code is
;	reached by a far call, not an interrupt or task gate.
;***********************************************************************
VCPISwitchToV86	proc	near


		; The interrupts and NMI is disabled.
	mov	al,DISABLE_NMI
	out	NMI_CMD,al

		; The stack is cleaned up by incrementing past the
		; 8 byte return address to the protected OS.
	add	esp,8

		; The EFLAGS value on the stack is set up so that V86
		; mode will be done.
	mov	dword ptr ss:[esp + 8],00023000h ; VM=1 + IOPL=3

		; The current data segment is loaded by incrementing the
		; code selecter by the size of one descriptor entry in the
		; GDT.	The data descriptor always follows the code desc.
	mov	ax,cs
	add	ax,8
	mov	ds,ax
	assume	ds:R_CODE

ifdef PICtrap
;QLEO  Initialize the Master PIC In Service Register
	mov	ax,1011b		; read ISR
	out	20h,al
	in	al,20h			; read ISR from master PIC
	mov	[MasterIS],ax		; save ISR state
	mov	al,[LastOCW3]
	out	20h,al			; restore last OCW3
;QLEO
endif
;
;  Insure that CR2 is 0: This allows determination of Page Faults
;
	mov	eax,cr2			; preserve CR2 across VCPI interface
	mov	[saved_cr2],eax		; this is for PharLap's DOS extenders
	xor	eax,eax
	mov	cr2,eax

		; The saved page table directory address must be loaded
		; to return to CEMM's linear space.
	mov	eax,cr3
	and	eax,0FFFh			; clear address
	or	eax,dword ptr [saved_cr3]       ; address
	mov	cr3,eax

		; The previously saved system registers are restored.  Even
		; though the new GDT and IDT are not addressable through the
		; VCPI program's page tables yet, its okay because all of the
		; selectors have their hidden parts still intact.
	db	66h
	lgdt	fword ptr [saved_gdtr]	; -> 16 bit limit : 32 bit base
	db	66h
	lidt	fword ptr [saved_idtr]	; -> 16 bit limit : 32 bit base

		; Now the LDT and TSS can be loaded.
	lldt	word ptr [saved_ldtr]
	ltr	word ptr [saved_tr]

		; NMI's are reenabled.
	mov	al,ENABLE_NMI
	out	NMI_CMD,al

VCPIReturnFromProt:

		; The control is passed back using the stack frame provided.
	iretd
VCPISwitchToV86	endp

;***********************************************************************
; VCPISwitchToProtected
;
; ENTRY
;	ESI = linear address of data struct to be used.
; EXIT
;	Interrupts disabled
; DESCRIPTION
;	This procedure changes the system registers and switches control
;	to the VCPI program.  Interrupts are left disabled and the current
;	CEMM stack is cleaned up as this routine does not return.  This
;	code saves the current system registers GDT, LDT, IDT, TSS and CR3
;	for restoration later on.  The busy bit in the TSS descriptor is
;	also reset.
;***********************************************************************

Switch_to_protected_struc	struc
	Switch_CR3		dd	?
	Switch_GDTR_ptr		dd      ?	; -> fword
	Switch_IDTR_ptr		dd      ?	; -> fword
	Switch_LDT_sel		dw      ?
	Switch_TR_sel		dw      ?
	Switch_EIP		dd      ?
	Switch_CS		dw      ?
Switch_to_protected_struc	ends

VCPISwitchToProtected	proc	near

		; The interrupts and NMI is disabled.
	mov	al,DISABLE_NMI
	out	NMI_CMD,al

		;restore users BP from stack frame
	mov	ebp,[bp]

		; The stack is cleared up since this doesn't return by
		; resetting the stack pointer.	This code assumes that a
		; transfer to the ring 0 stack was done when executing the
		; INT 67h.
;LEO	mov	sp,OFFSET STACK:stack0_top

	push	RMS_GSEL
	pop	ss
	mov	sp,OFFSET R_STACK:RealStackTop

		; The current GDT,IDT,LDT, CR3 and TSS are saved.
		; We cannot use CS for this since it is a write operation.
	mov	ax,RCODEA_GSEL
	mov	ds,ax
	assume	ds:R_CODE
	db	66h
	sgdt	fword ptr [saved_gdtr]
	db	66h
	sidt	fword ptr [saved_idtr]
	sldt	word ptr [saved_ldtr]
	str	word ptr [saved_tr]
	mov	eax,cr3
	and	ax,NOT 0FFFh			; clear control bits
	mov	dword ptr [saved_cr3],eax

;
;  Restore CR2 for the VCPI client
;
	mov	eax,[saved_cr2]			; this is for PharLap's DOS extenders
	mov	cr2,eax

		; The current TSS's busy bit is reset
	mov	ax,GDTD_GSEL
	mov	ds,ax
	assume	ds:GDT
	and	byte ptr ds:[TSS_GSEL + 5],TSS_RESET_BUSY

		; A special 4GB zero based descriptor is set up for ds:ESI.
	mov	ax,DATA32_GSEL
	mov	ds,ax
	assume	ds:nothing

		; CR3 is loaded from the table.  This will cause new memory
		; fetches to go through the new page tables but since the
		; VCPI code is located in 640k memory which is duplicated in
		; both versions of page tables, this is no problem.
	push	ebx
	mov	eax,cr3
	and	eax,0FFFh		; clear address
	mov	ebx,[esi.Switch_CR3]
	and	bx,NOT 0FFFh		; clear control bits
	or	eax,ebx
	mov	cr3,eax

		; Turn off the busy bit for the TSS entry in the GDT. This
		; is done by loading the base address of the GDT.
		; Then the TR selector and the GDT address are used with
		; the zero based DS selector to reset the busy bit.
	mov	eax,[esi.Switch_GDTR_ptr]
	mov	eax,dword ptr [eax + 2]
	movzx	ebx,word ptr [esi.Switch_TR_sel]
	and	bl,0F8h				;remove TI and RPL bits
	and	byte ptr [eax + ebx + 5],TSS_RESET_BUSY
	pop	ebx

		; The GDT and IDT pointers are loaded from the table.
		; NOTE: even though the GDT has changed, the current selectors
		; are still valid because of the hidden parts that were loaded
		; with the base address and limit.
	mov	eax,[esi.Switch_GDTR_ptr]
	db	66h
	lgdt	fword ptr [eax]		; -> 16 bit limit : 32 bit base

	mov	eax,[esi.Switch_IDTR_ptr]
	db	66h
	lidt	fword ptr [eax]		; -> 16 bit limit : 32 bit base

		; The LDT and TR are loaded from the table.
	lldt	word ptr [esi.Switch_LDT_sel]
	ltr	word ptr [esi.Switch_TR_sel]

		; The return address is pushed on the stack.  This returns
		; into the VCPI program's code.
	push	0
	push	word ptr [esi.Switch_CS]
	push	dword ptr [esi.Switch_EIP]

		; NMI's are reenabled.
	mov	al,ENABLE_NMI
	out	NMI_CMD,al

		; A far USE32 bit return is done.
	db	66h
	retf
VCPISwitchToProtected	endp
page
;***********************************************************************
; VCPIServerContext:	This routine saves the VCPI client's system
;	context, and switches to EMM386's context.
;
; ENTRY
;	System tables and segment regs setup with client's context
;
; EXIT
;	System tables setup for EMM386 context
;	CS  = R1_CODE
;	DS  = _DATA
;	ES  = ABS0
;	FS  = NULL
;	GS  = R_CODE
; Used:
;	EAX
;
;***********************************************************************
	public	VCPIServerContext

VCPIServerContext proc	near

	cli
	mov	al,DISABLE_NMI
	out	NMI_CMD,al

	mov	ax, cs			; make sure DS -> R_CODE
	add	ax, 8
	mov	ds,ax
	assume	ds:R_CODE

	pop	ax			; save near return address
	mov	[ret_addr], ax

;  Save the client's context so it can be restored later

	mov	[client_cs], cs 	; save CS selector client is using

	push	ebp			; save more client state on it's stack
	push	fs
	push	gs

	mov	[client_ss], ss 	; save client stack
	mov	[client_esp], esp

		; The current GDT,IDT,LDT, CR2, CR3 and TSS are saved.
	db	66h
	sgdt	[client_gdtr]
	db	66h
	sidt	[client_idtr]
	sldt	word ptr [client_ldtr]
	str	word ptr [client_tr]
	mov	eax, cr2
	mov	[client_cr2], eax
	mov	eax,cr3
	and	ax,NOT 0FFFh			; clear control bits
	mov	dword ptr [client_cr3],eax

;  Now setup the server's (EMM386) context

	xor	eax, eax
	mov	cr2, eax

		; The saved page table directory address must be loaded
		; to return to EMM386's linear space.
	mov	eax,cr3
	and	eax,0FFFh			; clear address
	or	eax,dword ptr [saved_cr3]       ; address
	mov	cr3,eax

		; The previously saved system registers are restored.  Even
		; though the new GDT and IDT are not addressable through the
		; VCPI program's page tables yet, its okay because all of the
		; selectors have their hidden parts still intact.
	db	66h
	lgdt	fword ptr [saved_gdtr]	; -> 16 bit limit : 32 bit base
	db	66h
	lidt	fword ptr [saved_idtr]	; -> 16 bit limit : 32 bit base

		; Now the LDT and TSS can be loaded.
	lldt	word ptr [saved_ldtr]
	ltr	word ptr [saved_tr]

		; Now setup segment registers using our GDT
	Pjmp	R1CODE_GSEL,R1_CODE:VSC_load_seg
VSC_load_seg:
	mov	ax, VDMD_GSEL
	mov	ds, ax			; ds = _DATA
	mov	ax, DATA32_GSEL
	mov	es, ax			; es = 4 Gb
	xor	ax, ax
	mov	fs, ax			; fs = NULL
	mov	ax, RCODEA_GSEL
	mov	gs, ax			; gs = R_CODE
	assume	DS:_DATA,es:ABS0,fs:NOTHING,gs:R_CODE

		; setup stack frame from EMM386 TSS
	mov	ss, [TSS].TSS386_SS0
	mov	esp, [TSS].TSS386_ESP0
	sub	esp, [VTFO][size VM_TRAP_FRAME]
	mov	ebp, esp

		; dummy up some values in the VM TRAP FRAME
	xor	eax, eax
	mov	dword ptr [bp][VTFO].VMTF_GS, eax
	mov	dword ptr [bp][VTFO].VMTF_FS, eax
	mov	dword ptr [bp][VTFO].VMTF_DS, eax
	mov	dword ptr [bp][VTFO].VMTF_ES, eax
	mov	ax, [segR1_CODE]
	mov	dword ptr [bp][VTFO].VMTF_SS, eax	; a stack to use if
	mov	ax, offset R1_CODE:RealStack2_top	;   V86 mode used
	mov	dword ptr [bp][VTFO].VMTF_ESP, eax
	mov	dword ptr [bp][VTFO].VMTF_EFLAGS, (FLAGS_VM SHL 16) OR FLAGS_IOPL

		; NMI's are reenabled.
	mov	al,ENABLE_NMI
	out	NMI_CMD,al

	jmp	[ret_addr]		; 'return' to caller in EMM386 context

VCPIServerContext endp

;***********************************************************************
; VCPIClientContext:	This routine restores the VCPI client's saved
;	system context.
;
; ENTRY
;
; EXIT
;	System tables and segment registers setup for client context
; Used:
;	EAX, EBX
;
;***********************************************************************
	public	VCPIClientContext
	assume	DS:_DATA,es:NOTHING,fs:NOTHING,gs:R_CODE

VCPIClientContext proc	 near

	cli
	mov	al,DISABLE_NMI
	out	NMI_CMD,al

	pop	ax			; save near return address
	mov	[ret_addr], ax

		; Restore CR2 for the VCPI client
	mov	eax,[client_cr2]	; this is for PharLap's DOS extenders
	mov	cr2,eax

		; The current TSS's busy bit is reset
	mov	ax,GDTD_GSEL
	mov	ds,ax
	assume	ds:GDT
	and	byte ptr ds:[TSS_GSEL + 5],TSS_RESET_BUSY

	mov	ax, DATA32_GSEL
	mov	ds, ax
	assume	ds:NOTHING

		; CR3 is loaded from the table.  This will cause new memory
		; fetches to go through the new page tables but since the
		; VCPI code is located in 640k memory which is duplicated in
		; both versions of page tables, this is no problem.
	mov	eax,cr3
	and	eax,0FFFh		; clear address
	or	eax, [client_CR3]
	mov	cr3,eax

		; The GDT and IDT pointers are loaded.
		; NOTE: even though the GDT has changed, the current selectors
		; are still valid because of the hidden parts that were loaded
		; with the base address and limit.
	db	66h
	lgdt	[client_gdtr]		; -> 16 bit limit : 32 bit base

	db	66h
	lidt	[client_idtr]		; -> 16 bit limit : 32 bit base

		; Turn off the busy bit for the TSS entry in the GDT. This
		; is done by loading the base address of the GDT.
		; Then the TR selector and the GDT address are used with
		; the zero based DS selector to reset the busy bit.
	mov	eax, dword ptr [client_gdtr + 2]
	movzx	ebx, [client_tr]
	and	bl,0F8h				;remove TI and RPL bits
	and	byte ptr [eax + ebx + 5],TSS_RESET_BUSY

		; The LDT and TR are loaded.
	lldt	[client_ldtr]
	ltr	[client_tr]

		; Restore client stack and segment registers

	jmp	dword ptr [client_jmpfar]   ; loads CS, goes to VCC_load_seg
VCC_load_seg:

	mov	ss, [client_ss]
	mov	esp, [client_esp]

	mov	ax, cs
	add	ax, 8
	mov	ds, ax
	add	ax, 8
	mov	es, ax
	pop	gs
	pop	fs
	pop	ebp
	assume	ds:R_CODE,es:NOTHING,fs:NOTHING,gs:NOTHING

		; NMI's are reenabled.
	mov	al,ENABLE_NMI
	out	NMI_CMD,al

	jmp	[ret_addr]		; 'return' to caller in client context

VCPIClientContext endp

page
assume cs:R1_CODE,ds:R_CODE,es:nothing,fs:nothing,gs:nothing
;==============================================================================
;==
;== VCPItoEMSspace: Moves a 16K EMS page to the free EMS handle space
;==
;== Entry
;==	ESI = index into a 4K VCPI page
;==	DS  = R_CODE
;==	ES  = PAGED_GSEL
;==
;== Exit
;==	EAX = base address of EMS page if page moved, 0 otherwise
;==
;==============================================================================
VCPItoEMSspaceFar proc	far
	call	VCPItoEMSspace
	ret
VCPItoEMSspaceFar	endp

VCPItoEMSspace proc	near
;
;  Check if this freed up a 16K page: If so, place it in the free EMS space
;
	xor	ebx, ebx
	and	si,not 3		; 16K boundary

	test	word ptr es:[esi*4],fVCPIPageAllocated
	jnz	VTEexit
	test	word ptr es:[esi*4+4],fVCPIPageAllocated
	jnz	VTEexit
	test	word ptr es:[esi*4+8],fVCPIPageAllocated
	jnz	VTEexit
	test	word ptr es:[esi*4+12],fVCPIPageAllocated
	jnz	VTEexit

	mov	ebx, es:[esi*4] 	; base address for EMS page
;
;  Is this 16K page already in top
;
	cmp	si, [TopOfFreeEMSspace] ;Q: Move anything?
	je	short VTEdone		; N: exit
	jb	short VTEerror

;
;  Move VCPI space up 4 entries
;
	movzx	ecx,si			; start of source
	sub	cx,[BotOfVCPIspace]	; number of entries to move
	jbe	short VTEclearFree
	add	si,3			; start index of destination
	shl	si,2			; offset
	mov	edi,esi			; destination
	sub	esi,4*4			; source

	std				; reverse move
	REP_MOVS_DWORD_USING_ES_ESI

VTEclearFree:
	cld				; clear direction flag
	mov	cx, 4
	xor	eax, eax
	movzx	edi, [BotOfVCPIspace]	; zero 4 invalid entries
	shl	di, 2			; offset
	REP_STOS_DWORD_PTR_ES_EDI

	mov	eax, ebx
	movzx	edi, [TopOfFreeEMSspace]; store page entries in EMS space
	shl	di, 2			; offset
	STOS_DWORD_PTR_ES_EDI		; eax has base address for EMS page
	add	eax,1000h
	STOS_DWORD_PTR_ES_EDI
	add	eax,1000h
	STOS_DWORD_PTR_ES_EDI
	add	eax,1000h
	STOS_DWORD_PTR_ES_EDI
VTEdone:
;
;  Add a 16K EMS page to the free EMS space, remove it from VCPI space
;
	add	[TopOfFreeEMSspace],4
	add	[BotOfVCPIspace], 4
VTEexit:
	mov	eax, ebx		; base address or 0
	ret
VTEerror:
ifdef BETA
int 1;beta
endif
	jmp	short VTEexit
VCPItoEMSspace	endp
R1_CODE	ends
	END

