.386p
page	58,132
;******************************************************************************
	title	emmfunct.asm - EMM function handlers
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1988-1991
; (C) Copyright COMPAQ Computer Corp. 1988-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;		EMMLIB.LIB - Expanded Memory Manager Functions Library
;
; Module:	EMS Function Handler
;
; Version:	1.00
;
; Date:	November 1, 1988
;
; Author:	Dan Mazina (original)
;		Leo Cohen (designed and implemented memory management and
;			   mapping functions to improve performance) LC
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   8/11/88	1	initial code
;******************************************************************************
;
;   Functional Description:
;	This module implements all of the functions for the EMM interface.
;
;******************************************************************************

page
;******************************************************************************
; PUBLICS
;******************************************************************************
public	EMSDispatcher
public	GetStatus
public	GetPageFrameAddress
public	GetUnallocatedPageCount
public	AllocatePages
public	MapHandlePage
public	DeallocatePages
public	GetEMMVersion
public	SavePageMap
public	RestorePageMap
public	GetPageMappingRegisterIOArray
public	GetLogicalToPhysicalPageTrans
public	GetEMMHandleCount
public	GetEMMHandlePages
public	GetAllEMMHandlePages
public	GetSetPageMap

;******************************************************************************
; INCLUDES
;******************************************************************************
include	vdmseg.inc
include vdmsel.inc
include	page.inc
include	emmfunct.inc
include emmdata.inc
include emm386.inc

ifdef BETA
include	emmfunct.pub
endif
;******************************************************************************
; DEFINES
;******************************************************************************

;******************************************************************************
; EXTERNALS
;******************************************************************************
_TEXT	segment
	extrn	SaveWindowMapping:near
	extrn	RestoreWindowMapping:near
	extrn	AllocEMSMem:near
	extrn	ValidateHandle:near
	extrn	GetSetPartialPageMap:near
	extrn	MapUnmapMultipleHandlePages:near
	extrn	GetSetHandleAttribute:near
	extrn	GetSetHandleName:near
	extrn	GetHandleDirectory:near
	extrn	AlterPageMapAndJump:near
	extrn	AlterPageMapAndCall:near
	extrn	MoveExchangeMemoryRegion:near
	extrn	GetMappablePhysicalAddress:near
	extrn	GetExpandedMemoryHWInfo:near
	extrn	AllocateStandardRawPages:near
	extrn	AlternateMapRegisterSet:near
	extrn	PrepareForWarmBoot:near
	extrn	EnableDisableOSFunctions:near
	extrn	ReallocatePages:near

	extrn	GetFreeEMS:near
	extrn	CheckFreeEMS:near
	extrn	ShrinkEMSpool:near

ifdef QEMS
	extrn	Get4kPages:near
	extrn	GarbageCollect:near
	extrn	GetHandleSpace:near
	extrn	SetWindows:near
	extrn	SetPT0Cntxt:near
	extrn	RestorePT0Cntxt:near
endif
_TEXT	ends
R_CODE	segment
	extrn	CEMM_Entry:word
R_CODE	ends
page
;******************************************************************************
; SEGMENTS
;******************************************************************************

_TEXT	segment
assume	cs:_TEXT,ds:_DATA,ss:_DATA,es:ABS0,gs:R_CODE
even
;******************************************************************************
; EMS_dispatcher
;
; ENTRY:
;	DS - VDMD_GSEL
;	GS - RCODEA_GSEL
;	BP - if CEMM was on during call, offset to the fault stack frame
;	all other registers have the user's values except for BP
; EXIT:
;	the return registers are set up correctly
; DESCRIPTION:
;	This routine checks the function number in AH and calls the appropriate
;	subroutine.  The user's register values are pushed on the stack and
;	accessed through the 'RegisterStack_struc' structure.
;******************************************************************************
dispatch_vector	label word
	dw	OFFSET GetStatus
	dw	OFFSET GetPageFrameAddress
	dw	OFFSET GetUnallocatedPageCount
	dw	OFFSET AllocatePages
	dw	OFFSET MapHandlePage
	dw	OFFSET DeallocatePages
	dw	OFFSET GetEMMVersion
	dw	OFFSET SavePageMap
	dw	OFFSET RestorePageMap
	dw	OFFSET GetPageMappingRegisterIOArray
	dw	OFFSET GetLogicalToPhysicalPageTrans
	dw	OFFSET GetEMMHandleCount
	dw	OFFSET GetEMMHandlePages
	dw	OFFSET GetAllEMMHandlePages
	dw	OFFSET GetSetPageMap
	dw	OFFSET GetSetPartialPageMap
	dw	OFFSET MapUnmapMultipleHandlePages
	dw	OFFSET ReallocatePages
	dw	OFFSET GetSetHandleAttribute
	dw	OFFSET GetSetHandleName
	dw	OFFSET GetHandleDirectory
	dw	OFFSET AlterPageMapAndJump
	dw	OFFSET AlterPageMapAndCall
	dw	OFFSET MoveExchangeMemoryRegion
	dw	OFFSET GetMappablePhysicalAddress
	dw	OFFSET GetExpandedMemoryHWInfo
	dw	OFFSET AllocateStandardRawPages
	dw	OFFSET AlternateMapRegisterSet
	dw	OFFSET PrepareForWarmBoot
	dw	OFFSET EnableDisableOSFunctions
cEMSFunc	equ	(($ - dispatch_vector)/2)

align 16
EMSDispatcher	proc	far
;
;  The 32-bit registers are pushed and the BP is set up for the new stack frame.
;  The direction flag is cleared.
;
	pushad
	mov	bp,sp
	cld
;
;  The function in AH is placed in ESI
;
	cmp	ah,40h + cEMSFunc
	jae	short ED_invalid_function

	movzx	si,ah
	sub	si,40h
	jb	short ED_invalid_function

	cmp	gs:[NoEMSset],TRUE	;Q: Is EMS available?
	je	short EDSWmalfunc	; N: S/W malfunction

ifdef QEMS
	bts	gs:[GenFlags],fEMSsemBit;Q: Are EMS functions being re-entered?
	jc	short EDSWmalfunc	; Y: S/W malfunction
endif
;
;  The function is changed into an index and executed. The only registers not
;  in their original states are ESI, EBP, DS and ES.
;
	add	si,si
	call	cs:dispatch_vector[si]

ifdef QEMS
	and	gs:[GenFlags],not fEMSsem ; Reset EMS re-entrancy semaphore
endif
ED_return_code:
	popad
	ret
		; If this point is reached then an invalid function
		; code was seen.
ED_invalid_function:
	cmp	ax,0FFA5h
	je	short ED_special_function

EDinvalid:
	mov	byte ptr [bp.reg_EAX + 1],INVALID_FUNCTION
	jmp	short ED_return_code

EDSWmalfunc:
	mov	byte ptr [bp.reg_EAX+1],EMM_SW_MALFUNCTION
	jmp	short ED_return_code

ED_special_function:
	ror	al,4
	mov	byte ptr [bp.reg_EAX],al
	mov	byte ptr [bp.reg_EAX + 1],INVALID_FUNCTION
	push	gs:[CEMM_Entry]
	pop	word ptr [bp.reg_ECX]
	mov	word ptr [bp.reg_EBX],SEG R_CODE
	jmp	short ED_return_code

EMSDispatcher	endp

page
align 16
;******************************************************************************
; GetStatus
;
; ENTRY
;	none
; EXIT
;	AH - OK
; DESCRIPTION
;	This function returns the current status of the EMM subsystem which is
;	always success.
;******************************************************************************
GetStatus	proc	near
	mov	byte ptr [bp.reg_EAX + 1],OK
	ret
GetStatus	endp

page
align 16
;***********************************************************************
; GetPageFrameAddress
;
; ENTRY
;	none
; EXIT
;	BX - segment address of the page frame
;	AH - OK
; DESCRIPTION
;	This routine returns the segment address of the page frame base.
;	This is window index 0 of the EMS windows.
;***********************************************************************
GetPageFrameAddress	proc	near
;
;  Get base address of the first EMS physical window
;
	mov	bx,EMS_window_location[0]	; page number
	shl	bx,8				; convert to paragraph

	mov	word ptr [bp][reg_EBX],bx	; return to client
	mov	byte ptr [bp][reg_EAX+1],OK	; assume no error

	cmp	gs:[PF_Base],FREE	;Q: Is there a full LIM 3.2 page frame?
	je	short GPFAerr		; N: return error
	ret

GPFAerr:
	mov	byte ptr [bp][reg_EAX+1],EMM_SW_MALFUNCTION ; XMA2EMS compatible
	ret
GetPageFrameAddress	endp

page
align 16
;==============================================================================
;==
;== GetUnallocatedPageCount: This routine returns the number of free 16k
;==			     pages and total number of 16k pages in the
;==			     system.
;==
;== Entry: (Protected Mode)
;==
;== Exit:  (Protected Mode)
;==	[BP]:BX = count of free 16k pages
;==	[BP]:DX = total number of 16k pages (free and allocated)
;==	[BP]:AH = OK
;==                                                                    LC
;=============================================================================
GetUnallocatedPageCount	proc	near
;
;  Get total EMS pages
;
	mov	ax,[TopOfHandleSpace]
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2
	mov	word ptr [bp][reg_EDX],ax
;
;  Get free EMS pages
;
	call	GetFreeEMS
	mov	word ptr [bp][reg_EBX],ax

	mov	byte ptr [bp][reg_EAX+1],OK
	ret
GetUnallocatedPageCount	endp

page
align 16
;==============================================================================
;==
;== AllocatePages: This routine allocates EMS pages to the user.
;==
;== Entry: (Protected Mode)
;==	BX = allocation size requested in 16k pages
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==	[BP]:DX = handle
;==                                                                    LC
;=============================================================================
AllocatePages	proc	near
;
;	If the number of requested pages is zero then exit.
;
	or	bx,bx
	je	short APzeroPagesReq
;
;  Get total EMS pages
;
	mov	ax,[TopOfHandleSpace]	; top of handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2			; convert to 16K pages

	cmp	ax,bx			;Q: Enough free EMS pages?
	jb	short APnotEnoughTotEMS	; N: error

;
;  Get free EMS pages (at this time!)
;
	call	CheckFreeEMS
	jc	short APnotEnoughFreeEMS

;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
;
;  Search handle table for an empty handle structure that can be used.
;
	xor	dx,dx
	mov	cx,[total_handles]
	mov	esi,[hndl_tbl_ptr]
	mov	di,4
	inc	dx
	add	si,di				; skip handle 0
APfindHandle:
	cmp	ds:[si].base_PTE_index,FREE
	je	short APhandleFound
	add	si,di
	inc	dx
	cmp	dx,cx
	jb	short APfindHandle
;ExitCrit				; END CRITICAL SECTION
popf
;
;  If this is reached then no free handles could be found.
;
	mov	byte ptr [bp][reg_EAX+1],NO_MORE_HANDLES
	jmp	short APexit
;
;  The PTE's are searched to find a location for the handle and its pages.
;  BX is the number of requested PTE's.	SI is the index into the handle space.
;
APhandleFound:
	mov	ds:[si].base_PTE_index,0	; mark used
	mov	ds:[si].number_PTEs,0		; with zero pages

;ExitCrit				; END CRITICAL SECTION
popf
	push	dx
	call	AllocEMSMem
	pop	dx
;
;  If CY is set, not enough EMS pages were found!  Even though we checked above,
;  because this routine is re-entrant, it could have changed.
;
	jc	short APclearHandle

	inc	[handle_count]
	mov	word ptr [bp][reg_EDX],dx
	mov	byte ptr [bp][reg_EAX+1],OK
APexit:
	ret

APzeroPagesReq:
	mov	byte ptr [bp][reg_EAX+1],ZERO_PAGES
	jmp	short APexit
APnotEnoughTotEMS:
	mov	byte ptr [bp][reg_EAX+1],NOT_ENOUGH_EXT_MEM
	jmp	short APexit
APclearHandle:
	mov	[si].base_PTE_index,FREE		; mark free
APnotEnoughFreeEMS:
	mov	byte ptr [bp][reg_EAX+1],NOT_ENOUGH_FREE_MEM
	jmp	short APexit
AllocatePages	endp

page
align 16
;==============================================================================
;==
;== MapHandlePage: This routine maps a 16K EMS logical page into a physical
;==		   EMS window.
;==
;== Entry: (Protected Mode)
;==	AL = window index
;==	BX = logical page index
;==	DX = EMM handle
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                     LC
;=============================================================================
MapHandlePage	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK	; assume error
;
;  Validate window index
;
	cmp	[xma2ems],TRUE		;Q: Is it in XMA2EMS mode?
	je	MHPxma2ems		; Y: remap windows

MHPindex:
	movzx	edi,al
	cmp	di,[number_EMS_windows]	;Q: Valid window index?
	jae	MHPinvPhyPage		; N: error

;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	MHPinvHandle		; N: error

	movzx	ecx,bx			; CX=logical page
	movzx	ebx,dx			; access handle structure
	shl	bx,2
	add	ebx,[hndl_tbl_ptr]	; (ESI) = pointer to handle table
	.errnz	(SIZE HandleTable_struc-4)

	cmp	ds:[bx].base_PTE_index,FREE ;Q: Handle in use?
	je	MHPinvHandle		    ; N: error

;
;  Access PTE
;
	add	di,di
	movzx	eax,[EMS_window_location][di] ; (EAX) = linear page # of window
	mov	di,ax
	shl	di,2
	add	edi,[page_tables]	; EDI = PTE address

	cmp	cx,UNMAP_WINDOW_OPCODE
	je	short MHPunmap
;
;  Validate logical page range
;
	test	ch,0C0h			;Q: Is BX range too large?
	jnz	short MHPinvLogPage	; Y: error
	shl	cx,2			; N: convert to 4K pages

	cmp	cx,ds:[bx].number_PTEs	;Q: Is logical page within valid range?
	jae	short MHPinvLogPage	; N: error


;
;  Access handle space, and map logical page
;
;EnterCritical			; base PTE index for handle may change
pushf
cli
	movzx	esi,ds:[bx].base_PTE_index
	add	si,cx
	shl	si,2			; quick esi*4
	add	esi,[page_directory]	; access handle space

	mov	cx,4
	REP_MOVS_DWORD_USING_ES_ESI
;
;debug	ExitCritical			; base PTE index for handle may change
popf
	mov	eax,cr3
	mov	cr3,eax
	ret
align 16
;
;  Unmap physical window by mapping one-to-one
;
MHPunmap:
	mov	dx,P_SIZE		; page
	shl	eax,P_SHIFT		; physical address
	or	ax,P_AVAIL		; accessible
	STOS_DWORD_PTR_ES_EDI
	add	eax,edx
	STOS_DWORD_PTR_ES_EDI
	add	eax,edx
	STOS_DWORD_PTR_ES_EDI
	add	eax,edx
	STOS_DWORD_PTR_ES_EDI

	mov	eax,cr3
	mov	cr3,eax

MHPexit:
	ret

MHPxma2ems:
	sub	al,250			;Q: P254 or P255?
	ja	short MHPx2aCont	; Y: continue
	add	al,250			; N: restore original window number
MHPx2aCont:
	cmp	al,5			;Q: Is the window in 0..5?
	ja	short MHPinvPhyPage	; N: no, error
	jmp	MHPindex

MHPinvPhyPage:
	mov	byte ptr [bp][reg_EAX+1],PHYS_PAGE_RANGE
	jmp	short MHPexit
MHPinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE
	jmp	short MHPexit
MHPinvLogPage:
	mov	byte ptr [bp][reg_EAX+1],LOG_PAGE_RANGE
	jmp	short MHPexit
MapHandlePage	endp

page
align 16
;==============================================================================
;==
;== DeallocatePages: This routine freess EMS pages belonging to a handle.
;==
;== Entry: (Protected Mode)
;==	DX = handle to deallocate
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==                                                                     LC
;=============================================================================
DeallocatePages	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK

;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	DPinvHandle		; N: error
	movzx	ebx,dx			; Y: handle index in EBX
	mov	edx,ebx
	mov	ax,FREE

;
;  Make sure save area is not being used.
;
	cmp	save_flag[bx],al	;Q: Is this save area FREE?
	jne	DPsaveAreaInUse		; N: error

;
;  Address the handle structure
;
	shl	bx,2
	.errnz	(SIZE HandleTable_struc-4)
	add	ebx,[hndl_tbl_ptr]	; (ESI) = pointer to handle table

;EnterCrit			; BEGIN CRITICAL SECTION
pushf
cli
;
;  Free handle
;
	xchg	ds:[bx].base_PTE_index,ax	; free handle
	cmp	ax,FREE				;Q: Handle already free?
	je	DPinvHandleX		    	; Y: error
	dec	gs:[handle_count]		; N: one less handle
;
;  If the handle is index 0, then it cannot actually be freed up,
;  so make it point somewhere innocuous.
;
	or	dx,dx
	jne	short DPnullName
	mov	ds:[bx].base_PTE_index,dx
	inc	gs:[handle_count]

;
;  The handles name is reset to all nulls
;
DPnullName:
	xor	ecx,ecx
	mov	edi,[hndl_nam_ptr]
	mov	[edi][edx*8],ecx
	mov	[edi][edx*8+4],ecx

;
;  Free its pages
;

	movzx	edi,ax			; start index of handle space
	xor	cx,cx			; free all pages for this handle
	xchg	cx,ds:[bx].number_PTEs	; get number of pages and zero it
	jcxz	short DPexitX		; if no pages, we're done
	add	ax,cx			; end of this handle space

	cmp	ax,[TopOfUsedEMSspace]	;Q: Last handle in the handle space?
	jne	short DPfPcont		; N: free all its pages
	mov	[TopOfUsedEMSspace],di	; Y: reflect change to top of used EMS
DPfPcont:
	shl	di,2			; index to offset
	add	edi,[page_directory]	; address of handle space
	shr	cx,2			; 4K to EMS pages
	sub	[UsedEMSPages],cx	; increase number of used EMS pages
	mov	ax,not fEMSPageAllocated; clear the user allocated bit

	mov	bx, cx			; 16k page count to bx
	xor	cx, cx			; cx = flag if XMS pool used for EMS

	; Free EMS pages--check each EMS page to see if allocated from XMS pool,
	; if so free XMS pages when done

DPfreePagesChkXMS:

	bt	dword ptr es:[edi], fXMSPageAllocatedBit	; from XMS?
	jnc	short DPfreePages

	inc	cx			; there is XMS memory to free

DPfreePages:
	and	word ptr es:[edi], ax
	add	edi, 10h
	dec	bx
	jz	short DPexitX
	jcxz	short DPfreePagesChkXMS ; don't check for XMS if already know
	jmp	short DPfreePages

DPexitX:
;ExitCrit			; END CRITICAL SECTION
popf
	; cx != 0 if XMS memory was used for the EMS allocation

	jcxz	short DPexit		; Q: any XMS to free?

	call	ShrinkEMSpool		;  Y:
DPexit:
	ret

DPinvHandleX:
;ExitCrit			; END CRITICAL SECTION
popf
DPinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE
	jmp	short DPexit
DPsaveAreaInUse:
	mov	byte ptr [bp][reg_EAX+1],SAVED_PAGE_DEALLOC
	jmp	short DPexit

DeallocatePages	endp

page
align 16
;***********************************************************************
; GetEMMVersion
;
; ENTRY
;	none
; EXIT
;	AH - OK
;	AL - version number
; DESCRIPTION
;	This routine returns the current version number.
;***********************************************************************
GetEMMVersion	proc	near
	mov	ah,OK
	mov	al,EMM_VERSION
	mov	word ptr [bp.reg_EAX],ax
	ret
GetEMMVersion	endp

page
align 16
;==============================================================================
;==
;== SavePageMap: This routine saves the LIM 3.2 page frame map context for a
;==		 handle.  This includes only windows 0-3.  The data is saved
;==		 in the save_map array internally.
;==
;== Entry: (Protected Mode)
;==	DX = EMM handle
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                     LC
;=============================================================================
SavePageMap	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK

	cmp	gs:[PF_Base],FREE	;Q: Is there a page frame?
	je	short SaPMswErr		; N: error

;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	short SaPMinvHandle	; N: error

	movzx	ebx,dx			; access handle structure
	mov	edi,[hndl_tbl_ptr]	; (ESI) = pointer to handle table

	.errnz	(SIZE HandleTable_struc-4)
	cmp	ds:[edi+ebx*4].base_PTE_index,FREE  ;Q: Handle in use?
	je	short SaPMinvHandle      ; N: error

;
;  Insure the save area for this handle is not presently being used
;
	mov	dl,NOT FREE
	xchg	save_flag[bx],dl	; mark used

	cmp	dl,FREE			;Q: Already saved?
	jne	short SaPMprevMapSaved	; Y: error

;
;  Access save area
;
	mov	edi,ebx			; esi = bx * 16
	shl	di,4
	.errnz	size HandleSavemap_struc-16

	add	edi,[save_map_ptr]
	add	edi,[p_DATA]

;
;  Access page table
;
	movzx	esi,[EMS_window_location][0]	; get index for page frame
	shl	si,2
	add	esi,[page_tables]

	mov	dx,0Ch
	mov	cx,4
SaPMloop:
	MOVS_DWORD_USING_ES_ESI
	add	si,dx
	dec	cx
	jnz	short SaPMloop

SaPMexit:
	ret

SaPMswErr:
	mov	byte ptr [bp][reg_EAX+1],EMM_SW_MALFUNCTION ; assume error
	jmp	short SaPMexit
SaPMinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE ; assume invalid handle
	jmp	short SaPMexit
SaPMprevMapSaved:
	mov	byte ptr [bp][reg_EAX+1],MAP_PREV_SAVED ; assume error
	jmp	short SaPMexit
SavePageMap	endp

page
align 16
;==============================================================================
;==
;== RestorePageMap: This routine restores the LIM 3.2 page frame map context
;==		    for a handle.  This includes only windows 0-3.  The
;==		    data is restored from the internal save_map array.
;==
;== Entry: (Protected Mode)
;==	DX = EMM handle
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                      LC
;=============================================================================
RestorePageMap	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK

	cmp	gs:[PF_Base],FREE	;Q: Is there a page frame?
	je	RePMswErr		; N: error

;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	short RePMinvHandle	; N: error

	movzx	ebx,dx			; access handle structure
	mov	edi,[hndl_tbl_ptr]	; (ESI) = pointer to handle table

	.errnz	(SIZE HandleTable_struc-4)
	cmp	ds:[edi+ebx*4].base_PTE_index,FREE  ;Q: Handle in use?
	je	short RePMinvHandle 		    ; N: error

;
;  Insure the save area for this handle is not empty
;
	mov	dl,FREE
	xchg	save_flag[bx],dl	; mark used

	cmp	dl,FREE			;Q: Already saved?
	je	short RePMnoMapSaved	; Y: error

;
;  Access save area
;
	mov	esi,ebx			; esi = bx * 16
	shl	si,4
	.errnz	size HandleSavemap_struc-16

	add	esi,[save_map_ptr]
	add	esi,[p_DATA]

;
;  Access page table
;

	movzx	edi,[EMS_window_location][0]	; get index for page frame
	shl	di,2
	add	edi,[page_tables]

	mov	bx,1000h
	mov	cx,4
RePMloop:
	LODS_DWORD_PTR_ES_ESI
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI
	dec	cx
	jnz	short RePMloop

	mov	eax,cr3
	mov	cr3,eax

RePMexit:
	ret

RePMswErr:
	mov	byte ptr [bp][reg_EAX+1],EMM_SW_MALFUNCTION ; assume error
	jmp	short RePMexit
RePMinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE ; assume invalid handle
	jmp	short RePMexit
RePMnoMapSaved:
	mov	byte ptr [bp][reg_EAX+1],NO_MAP_SAVED ; assume error
	jmp	short RePMexit

RestorePageMap	endp

page
align 16
;***********************************************************************
; GetPageMappingRegisterIOArray
;
; ENTRY
;	ES:DI - pointer to the users table
; EXIT
;	AH - INVALID_FUNCTION
; DESCRIPTION
;	This routine is no longer supported.
;***********************************************************************
GetPageMappingRegisterIOArray	proc	near

	mov	byte ptr [bp.reg_EAX + 1],INVALID_FUNCTION
	ret
GetPageMappingRegisterIOArray	endp

page
align 16
;***********************************************************************
; GetLogicalToPhysicalPageTrans
;
; ENTRY
;	ES:DI - pointer to the users table
;	DX - EMM handle
; EXIT
;	AH - INVALID_FUNCTION
; DESCRIPTION
;	This routine is no longer supported.
;***********************************************************************
GetLogicalToPhysicalPageTrans	proc	near

	mov	byte ptr [bp.reg_EAX + 1],INVALID_FUNCTION
	ret
GetLogicalToPhysicalPageTrans	endp

page
align 16
;***********************************************************************
; GetEMMHandleCount
;
; ENTRY
;	none
; EXIT
;	BX - handle count
;	AH - OK
; DESCRIPTION
;	This routine returns the number of active handles.
;***********************************************************************
GetEMMHandleCount	proc	near
	mov	bx,gs:[handle_count]
	mov	word ptr [bp.reg_EBX],bx
	mov	byte ptr [bp.reg_EAX + 1],OK
	ret
GetEMMHandleCount	endp

page
align 16
;***********************************************************************
; GetEMMHandlePages
;
; ENTRY
;	DX - handle
; EXIT
;	BX - number of 16k pages allocated to this handle
;	AH - return code
; DESCRIPTION
;	This routine returns the number of 16k EMS pages allocated to the
;	provided handle.
;***********************************************************************
GetEMMHandlePages	proc	near

		; The handle is validated.  SI has the handle struct index.
	call	ValidateHandle
	jc	short GHP_return_code
	movzx	esi,dx

		; The number of PTE indexes allocated for this handle is loaded
		; and divided by 4 to change it into 16k pages from 4k pages.
	MOV	EBX, [hndl_tbl_ptr]
	MOV	BX, DS:[EBX+ESI*4].number_PTEs
	shr	bx,2
	mov	word ptr [bp.reg_EBX],bx
	mov	byte ptr [bp.reg_EAX + 1],OK

GHP_return_code:
	ret
GetEMMHandlePages	endp

page
align 16
;***********************************************************************
; GetAllEMMHandlePages
;
; ENTRY
;	ES:DI - pointer to the users table
; EXIT
;	BX - number of handles in the table
;	AH - return code
; DESCRIPTION
;	This routine returns an array which lists all of the active handles
;	and how many 16k EMS pages are allocated to each of them.  The data
;	is placed in a user provided buffer.
;***********************************************************************
GetAllEMMHandlePages	proc	near

;	The user's segment value is changed into a selector and
;	the address is placed into ES:DI.

	movzx	eax,word ptr [bp.reg_ES]
	shl	eax,4
	movzx	edi,di
	add	edi,eax
;QEMS	push	DATA32_GSEL
;QEMS	pop	es

;EnterCrit			; BEGIN CRITICAL SECTION
pushf
cli
	mov	cx,gs:[handle_count]
	mov	word ptr [bp.reg_EBX],cx
	xor	esi,esi
	dec	si
;QEMS	mov	esi,0FFFFh		; (SI) = -1, (ESI hi) = 0
	mov	ebx,[hndl_tbl_ptr]

;	If this handle is not being used then don't report it.

GAHP_loop_top:
	inc	si
	cmp	[ebx+esi*4].base_PTE_index,FREE
	je	short GAHP_loop_top

;	The handle index is converted into its appropriate user
;	handle number and stored.

	mov	es:[edi],si
	add	edi,2

;	The number of 16k pages for this handle is returned.

	mov	ax,[ebx+esi*4].number_PTEs
	shr	ax,2
	mov	es:[edi],ax
	add	edi,2
	loop	GAHP_loop_top
;ExitCrit			; END CRITICAL SECTION
popf
	mov	byte ptr [bp.reg_EAX + 1],OK
	ret
GetAllEMMHandlePages	endp

page
align 16
;==============================================================================
;==
;== GetSetPageMap: This routine saves/restores the complete mapping context
;==		   to/from an external buffer.
;==
;== Entry: (Protected Mode)
;==	AL = Subfunction code
;==	     0 : get page map
;==	     1 : set page map
;==	     2 : get and Set page map
;==	     3 : return size of page map
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                      LC
;=============================================================================
GSPMvecTable	label	word
  dw offset _TEXT:GSPM_get_function
  dw offset _TEXT:GSPM_set_function
  dw offset _TEXT:GSPM_set_get_function
  dw offset _TEXT:GSPM_return_function
GSPMFuncNum	equ	($-GSPMvecTable)/2
align 16
GetSetPageMap	proc	near
;
;  The subfunction number is checked to see if it is valid.
;
	cmp	al,GSPMFuncNum			;Q: Invalid function?
	jae	short GSPMinvFunc		; Y: error
	mov	byte ptr [bp][reg_EAX+1],OK	; N: assume success
;
;  The get and set functions will use a zero based selector in ES for addressing
;  the user's buffer.  The jump table is used to execute the subfunction.
;
	movzx	esi,al
	add	si,si
	jmp	cs:GSPMvecTable[si]
align 16
;
;  If the subfunction just wants the size of the save area then just return it.
;
GSPM_return_function:
	mov	al,byte ptr [context_save_area_size]
	mov	byte ptr [bp.reg_EAX],al
GSPMexit:
	ret
align 16
;
;  If a Get function, save mapping in user buffer
;
GSPM_get_function:
	movzx	eax,word ptr [bp.reg_ES]
	shl	eax,4
	movzx	edi,di
	add	edi,eax
	jmp	SaveWindowMapping
align 16
GSPM_set_get_function:
	movzx	eax,word ptr [bp.reg_ES]
	shl	eax,4
	movzx	edi,di
	add	edi,eax

	call	SaveWindowMapping

;	If this is a Set function then set up the user's buffer in
;	ES:ESI and make the call.

GSPM_set_function:
	movzx	esi,word ptr [bp.reg_ESI]
	movzx	eax,word ptr [bp.reg_DS]
	shl	eax,4
	add	esi,eax
	jmp	RestoreWindowMapping

GSPMinvFunc:
	mov	byte ptr [bp][reg_EAX+1],INVALID_SUBFUNCTION ; assume error
	jmp	short GSPMexit
GetSetPageMap	endp

_TEXT	ends
END


