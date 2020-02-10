.386p
page	58,132
;******************************************************************************
	title	emmutils.asm - EMM utility functions
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1988-1991
; (C) Copyright COMPAQ Computer Corp. 1988-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;		EMMLIB.LIB - Expanded Memory Manager Functions Library
;
; Module:	EMM utility functions
;
; Version:	1.00
;
; Date:		November 1, 1988
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
;	This module implements all of the utilities for the EMM interface.
;
;******************************************************************************

page
;******************************************************************************
; PUBLICS
;******************************************************************************
public	AllocEMSMem
public	AddFreeEMS
public	MoveHandleSpace
public	SaveWindowMapping
public	RestoreWindowMapping
public	GetModifyUsersStack
public	MapInWindows
public	ActivateRegSet

public	ValidateHandle
ifdef QEMS
public	GetHandleSpace
public	Get4kPages
public	GarbageCollect
public	ShiftHandles
public	SetWindows
public	FindWindowIndex
public	SetPT0Cntxt
public	RestorePT0Cntxt
public	FindNextFreeHandleSpace
endif
;******************************************************************************
; INCLUDES
;******************************************************************************
include	vdmseg.inc
include vdmsel.inc
include vm386.inc
include	page.inc
include	emmfunct.inc
include	emmdata.inc
include emm386.inc
include debmac.inc

ifdef BETA
include emmutils.pub
endif
;******************************************************************************
; EXTERNALS
;******************************************************************************
_TEXT	segment
	extrn	AllocateXMSpages:near
	extrn	QueryXMSpages:near
	extrn	FreeXMSpages:near
ifdef DEBUG
	extrn	pTestDbgIns:far
	extrn	pDebugPrintf:far
endif
_TEXT	ends

R_CODE	segment
R_CODE	ends

page
;=============================================================================
;==	C O D E  S E G M E N T
;=============================================================================
_TEXT	segment
assume	cs:_TEXT,ds:_DATA,ss:STACK,es:ABS0,gs:R_CODE
page
align 16
;=======================================================================
;==
;== AllocEMSMem: This routine searches the handle space for free EMS memory.
;==		 When found, it updates the handle structure of its findings.
;==
;== Entry: (Protected Mode)
;==	BX  = Number of EMS pages requested
;==	ESI = handle structure pointer which wants EMS memory
;==	    = -1, just compact the EMS space!
;==
;== Exit:  (Protected Mode)
;==	CY  = set, handle structure is not updated
;==	    = clear, handle structure is updated with EMS memory
;==	SI remains the same
;==                                                                 LC
;=======================================================================
AllocEMSMem	proc	near
;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
;
;  Get number of available existing EMS pages
;
	mov	ax,[TopOfFreeEMSspace]	; top of free EMS handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2			; convert to 16K pages
	sub	ax,[UsedEMSPages]	; number of free EMS pages

	cmp	ax,bx			;Q: Enough free EMS pages?
	jb	short AEMnotEnoughEMS	; N: error
;
;  Check if enough contiguous handle space is available
;
	movzx	edi,[TopOfUsedEMSspace]
	mov	ax,[TopOfFreeEMSspace]
	sub	ax,di
	shl	bx,2

	sub	ax,bx		;Q: Enough free EMS pages already contiguous?
	jb	short AEMGarbage; N: Oh no, reorganize handle space!
;
;  Give away the ems memory
;

	add	ds:[si].number_PTEs,bx
	xchg	ds:[si].base_PTE_index,di
	or	di,di			;Q: Did the handle already have pages?
	jz	short AEMcont		; N: continue
	mov	ds:[si].base_PTE_index,di; give back his old base
AEMcont:

;
;  Update the Handle space management data structures
;
	add	[TopOfUsedEMSspace],bx
;
;  Update amount of used EMS pages
;
	shr	bx,2
	add	[UsedEMSPages],bx
;
;  Set the allocated bit on the PTEs
;
	mov	di,ds:[si].base_PTE_index
	mov	bx,ds:[si].number_PTEs
	shr	bx,2
	mov	ax,fEMSPageAllocated
	mov	edx,[page_directory]
	mov	cx,4
AEMloop:
	or	word ptr es:[edx][edi*4],ax
	add	di,cx			; need to mark the first page only

	dec	bx			;Q Last EMS page?
	jnz	short AEMloop		; N: next
;ExitCrit				; END CRITICAL SECTION
popf
	clc
	ret

AEMGarbage:
;ExitCrit				; END CRITICAL SECTION
popf
	shr	bx,2
	neg	ax			; number of pages needed
	call	AddFreeEMS		; get enough free EMS space
	jmp	short AllocEMSMem	; allocate them

AEMnotEnoughEMS:
;ExitCrit				; END CRITICAL SECTION
popf
	push	bx
	sub	bx, ax			; # additional EMS pages needed
	call	GrowEMSPool
	pop	bx
	jnc	AllocEMSMem

	ret				; CY already set for failure

AllocEMSMem	endp

;===============================================================================
;==
;== GrowEMSPool:  This routine attempts to grow the EMS pool by allocating
;==		  XMS pages and adding them to the handle space.
;==
;== Entry: (Protected Mode)
;==	BX = Number of additional EMS pages needed
;== Exit:
;==	CY = clear if desired # EMS pages added, set if failed (none added)
;==	[TopOfFreeEMSspace] = updated
;== Used:
;==	AX
;==
;===============================================================================
	public	GrowEMSPool, GrowEMSPool_far

GrowEMSPool_far proc	far
	call	GrowEMSPool
	ret
GrowEMSPool_far endp

GrowEMSPool	proc	near

	AssertSegReg	gs, RCODEA_GSEL

	;;;pDebugF "GrowEMSPool: need %d EMS pages\n", bx

;  Make sure there is enough handle space for the additional EMS pages

	mov	ax, [BotOfVCPIspace]
	sub	ax, [TopOfFreeEMSspace]
	shr	ax, 2			; 4k pages to 16k pages
	cmp	ax, bx			; bx already set for 4k pages
	jb	short GEP_exit		; CY is already set

;  Verify that enough XMS memory is available

	call	QueryXMSpages		; Q: enough XMS around?
	jc	short GEP_exit		; N:

;  Allocate additional XMS pages and update PTEs

	call	AllocateXMSpages	; allocate BX 16k pages

	; CY set or cleared by AllocateXMSpages

ifdef DEBUG
	jnc	short @f
	pDebugF "GrowEMSPool failed!\n"
@@:
endif

GEP_exit:
	ret

GrowEMSPool	endp

;===============================================================================
;==
;== ShrinkEMSpool:	This routine attempts to shrink the EMS pool by
;==		  releasing XMS pages in the free EMS space.
;==
;== Entry: (Protected Mode)
;==
;== Exit:
;==
;== Used:
;==	AX
;==
;===============================================================================
	public	ShrinkEMSpool, ShrinkEMSpool_far

ShrinkEMSpool_far proc	far
	call	ShrinkEMSpool
	ret
ShrinkEMSpool_far endp


ShrinkEMSpool	proc	near

	AssertSegReg	gs, RCODEA_GSEL

	push	bx

;  Garbage collect free EMS so all free EMS page PTEs are together

	mov	ax, [TopOfFreeEMSspace]
	sub	ax, FIRST_HANDLE_PTE
	shr	ax, 2
	sub	ax, [UsedEMSPages]
	shl	ax, 2			; ax = # free 4k pages in EMS pool

	mov	bx, [TopOfFreeEMSspace] ; skip garbage collection if free
	sub	bx, [TopOfUsedEMSspace] ;   space is already collected
	cmp	ax, bx
	jbe	short SEP_free_xms_pages

	call	AddFreeEMS		; garbage collect free EMS space

SEP_free_xms_pages:

	call	FreeXMSpages		; release free XMS pages

	pop	bx

	ret

ShrinkEMSpool	endp

page
align 16
;===============================================================================
;==
;== AddFreeEMS: This routine searches the used EMS space for free EMS pages and
;==		moves them to the free EMS space.
;==
;== Entry: (Protected Mode)
;==	AX = number of 4K pages needed in free EMS space
;==
;== Exit:  (Protected Mode)
;==	[TopOfUsedEMSspace] = updated
;==	EBX,ESI (are not modified)
;==                                                                     LC
;===============================================================================
	public	AddFreeEMS, AddFreeEMS_far

AddFreeEMS_far	proc	far
	call	AddFreeEMS
	ret
AddFreeEMS_far	endp

AddFreeEMS proc	near
	push	bx
	push	si
;
;  Start at the top of the used EMS space
;
	mov	si,FIRST_HANDLE_PTE
	mov	dx,fEMSPageAllocated
	mov	ecx,[page_directory]
;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	movzx	edi,[TopOfUsedEMSspace]
	mov	bx,4
AFEfindUsed:
	test	word ptr es:[ecx][edi*4-4*4],dx	;Q: Is it free?
	jnz	short AFEfoundUsed		; N: find one

	sub	di,bx
	cmp	si,di			;Q: Reached end of handle space?
	jb	short AFEfindUsed	; N: continue
;
;  Found a used page (or reached end); look until a free page is found.
;
AFEfoundUsed:
	mov	bx,di
	xchg	bx,[TopOfUsedEMSspace]
;ExitCrit				; END CRITICAL SECTION
popf
	sub	bx,di			; number of free PTEs found on top
	sub	ax,bx			;Q: Enough to satisfy request?
	jle	short AFEexit		; Y: done

AFEfindFreeX:
	cmp	si,di			;Q: Reached end of handle space?
	jae	short AFEexit		; Y: we can't do anything else

	mov	bx,4
AFEfindFree:
	test	word ptr es:[ecx][edi*4-4*4],dx	;Q: Is it free?
	jz	short AFEfoundFree		; Y: move space

	sub	di,bx
	cmp	si,di			;Q: Reached end of handle space?
	jb	short AFEfindFree	; N: continue
	jmp	short AFEexit		; Y: can't do anything else!
align 16
;
;  A free entry has been found, need to find out how many contiguous pages
;  it represents, shift all handles down, and add this to free EMS pages.
;
AFEfoundFree:
;EnterCrit				; BEGIN CRITICAL SECTION
pushf
cli
	push	di
AFEfindUsed2:
	test	word ptr es:[ecx][edi*4-4*4],dx	;Q: Is it used?
	jnz	short AFEmoveHandles		; Y: move handles

;QLEO	sub	ax,bx               	;Q: Enough to satisfy request?
;QLEO	jz	short AFEmoveHandles	; Y: exit

	sub	di,bx
	cmp	si,di			;Q: Reached end of handle space?
	jb	short AFEfindUsed2	; N: continue
;
;  Move the handle space down
;
AFEmoveHandles:
	pop	bx
	sub	bx,di			; number of entries to move PTEs

	call	MoveHandleSpace

;ExitCrit				; END CRITICAL SECTION
popf
	sub	ax,bx			;Q: Enough to satisfy request?
	jg	short AFEfindFreeX	; N: continue searching down
;QLEO	or	ax,ax			;Q: Enough to satisfy request?
;QLEO	jnz	short AFEfindFreeX	; N: continue searching down
AFEexit:
	pop	si
	pop	bx
	ret

AddFreeEMS	endp

page
align 16
;==============================================================================
;==
;== MoveHandleSpace: This routine moves the handle space and updates the handle
;==		     data structures to reflect this move.  It will move all
;==		     the pages in the range from DI to [TopOfUsedEMSspace] down by
;==		     BX entries.  It will relocate the PTEs from DI-BX to DI
;==		     to the [TopOfUsedEMSspace]-BX region.  Finally, it will update
;==		     all the EMS handles which were affected by this move.
;==
;== Entry: (Protected Mode)
;==	BX  = number of slots to move the PTEs
;==	DI  = starting PTE entry to move
;==	ECX = page directory linear address
;==	[TopOfUsedEMSspace] = last PTE entry to move
;==	This routine should be called with interrupts OFF (critical section)
;==
;== Exit:  (Protected Mode)
;==	[TopOfUsedEMSspace] = entry [TopOfUsedEMSspace] - BX
;==	EAX,BX,CX,ESI,EDX,EDI: same
;==
;==                                                                    LC
;==============================================================================
MoveHandleSpace	proc near
	push	eax
	push	ecx
	push	edx
	push	esi
	push	edi
;
;  Make sure we have something to move!
;
	or	bx,bx		;Q: Move anything?
	jz	MHSexit		; N: exit
;
;  Is this handle already on top?
;
	add	di,bx			; entry after this handle
	cmp	di,[TopOfUsedEMSspace]	;Q: Is this handle on top?
	jae	MHSexit			; Y: don't need to move handle
	sub	di,bx			; start index of this handle
;
;  Move handle space range from DI=>DI+BX-1 to ScratchHandleSpace
;
	push	bx
	mov	esi,[ScratchHandleSpace]
	xchg	esi,edi
	mov	dx,4
MHSloop1:
	mov	eax,es:[ecx][esi*4]
	STOS_DWORD_PTR_ES_EDI
	add	si,dx
	sub	bx,dx
	jnz	short MHSloop1
;
;  Move handle space range DI+BX=>[TopOfUsedEMSspace] to DI
;
	pop	bx
	push	si
	mov	di,si			; source index
	shl	si,2			; source offset
	add	esi,ecx			; source address
	movzx	ecx,[TopOfUsedEMSspace]	; source end index
	sub	cx,bx			; destination end index
	mov	[TopOfUsedEMSspace],cx	; will be the new top of used EMS space
	add	cx,bx			; again, source end index
	sub	cx,di			; number of entries to transfer
	movzx	edi,bx			; index distance to destination
	shl	di,2			; offset to destination
	sub	edi,esi
	neg	edi			; destination address
	REP_MOVS_DWORD_USING_ES_ESI
;
;  Move entries from scratch handle space to the free EMS space
;
	mov	esi,[ScratchHandleSpace]
	mov	edx,1000h
	mov	cx,bx
	shr	cx,2
MHSloop3:
	LODS_DWORD_PTR_ES_ESI
	STOS_DWORD_PTR_ES_EDI
	and	ax,not fEMSPageAllocated
	add	eax,edx
	STOS_DWORD_PTR_ES_EDI
	add	eax,edx
	STOS_DWORD_PTR_ES_EDI
	add	eax,edx
	STOS_DWORD_PTR_ES_EDI
	dec	cx
	jnz	short MHSloop3
;
;  Go through the handle table structures and update the ones we moved down
;
	pop	si			; starting index for handle space move
	movzx	eax,[total_handles]
	mov	edx,[hndl_tbl_ptr]
MHSfindHandle:
	mov	cx,[edx][eax*4-4].base_PTE_index
	cmp	cx,FREE
	je	short MHSnextHandle
	cmp	cx,si
	jb	short MHSnextHandle
	sub	cx,bx
	mov	[edx][eax*4-4].base_PTE_index,cx
MHSnextHandle:
	dec	ax
	jnz	short MHSfindHandle
MHSexit:
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	eax
	ret
MoveHandleSpace endp

;***********************************************************************
; CheckFreeEMS: Determine if a desired number of free EMS pages exist,
;		or if enough EMS pages can be made available.
;
; Entry: (Protected Mode)
;	BX = Number of free EMS (16k) pages needed
; Exit:
;	CY = clear if desired # EMS pages are free, set if not
; Uses:
;	AX
;
;***********************************************************************
	public	CheckFreeEMS
CheckFreeEMS	proc	near

	AssertSegReg	gs, RCODEA_GSEL

	mov	ax,[TopOfFreeEMSspace]	; top of free EMS handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2			; convert to 16K pages
	sub	ax,[UsedEMSPages]	; number of free EMS pages

	sub	ax, bx			; Q: already have enough pages?
	jnc	short CFE_ret		; Y: exit now

	; Don't have enough EMS available, see if the rest can be allocated
	; from XMS.

	neg	ax			; number additional EMS pages needed
	push	bx
	mov	bx, ax
	call	QueryXMSPages		; sets CY if needed pages unavailable
	pop	bx

CFE_ret:
	ret

CheckFreeEMS	endp

;***********************************************************************
; GetFreeEMS:	Return the current # of free EMS pages, and the # of EMS
;		pages that can be grabbed from XMS.
;
; Entry: (Protected Mode)
;	None.
; Exit:
;	AX = number of free EMS (16k) pages
; Used:
;	BX
;
;***********************************************************************
	public	GetFreeEMS, GetFreeEMS_far

GetFreeEMS_far	proc	far
	call	GetFreeEMS
	ret
GetFreeEMS_far	endp

GetFreeEMS	proc	near

	AssertSegReg	gs, RCODEA_GSEL

	mov	ax,[TopOfFreeEMSspace]	; top of free EMS handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2			; convert to 16K pages
	sub	ax,[UsedEMSPages]	; number of free EMS pages

	mov	bx, [BotOfVCPIspace]
	sub	bx, [TopOfFreeEMSspace]
	jbe	short GF_EMS_exit
	shr	bx, 2			; bx = # additional 16k XMS pages that
					;   could be added to EMS pool

	push	ax			; save free EMS pages

	call	QueryXMSPages		; Q: are bx 16k pages free?
	jc	GF_EMS_under_max	;  N: ax has # available pages

	mov	ax, bx			;  Y: set ax = # we can use (may be
					;     more free than can be used)
GF_EMS_under_max:
	pop	bx			; bx = # free EMS pages

	add	ax, bx			; free EMS + available XMS

GF_EMS_exit:

	;;;pDebugF "GetFreeEMS: %d free EMS pages.\n", ax

	ret

GetFreeEMS	endp

align 16
;***********************************************************************
; ValidateHandle
;	CY = !ValidateHandle(DX);
;
; ENTRY	(BP) = stack frame pointer
;	(DX) = handle
; EXIT 	(EDX) = validated handle
;	CY = set if the handle is invalid, reset otherwise
; DESCRIPTION:
;	This routine checks the handle in DX and verifies if it is valid.
;	If it is not valid then the carry flag is set.
; CRITICAL SECTIONS:
;	This procedure may be called with interrupts enabled.  It is
;	the user's responsibilty to insure that a handle does not
;	become invalid between the time it is validated and the time
;	it is used.
;***********************************************************************
	ASSUME	DS: _DATA
ValidateHandle	proc	near

	cmp	dx,[total_handles]	; Handle in range?
	jae	short vh0		;  no, error

;	The PTE index is checked to insure that it is valid and
;	that the handle is active.

	movzx	edx,dx
	push	eax
	mov	eax, [hndl_tbl_ptr]	; (EAX) = pointer to handle table
	cmp	word ptr ds:[eax+edx*4].base_PTE_index, FREE ; Handle in use?
	pop	eax
	je	short vh0		;  no, error
	clc				; Handle is valid
	ret

;	If this point is reached than an error occured. The
;	error code is set and the carry flag reset.

vh0:	mov	byte ptr [bp.reg_EAX + 1],INVALID_HANDLE
	stc
	ret

ValidateHandle	endp

align 16
;==============================================================================
;==
;== SaveWindowMapping: This function saves the current mapping register state
;==		       into the save area specified.  The current mapping state
;==		       consists of the number of window mappings followed by
;==		       the mappings themselves.
;==
;==                      ÉÍÍÍÍÍÍÍÍÍÍÍÍÍ»
;==                      º  Pn page 0  º  4h*(n+1)
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==			 |             |
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                      º  P2 page 0  º  0Ch
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                      º  P1 page 0  º  08h
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                      º  P0 page 0  º  04h
;==                      ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                      º Head ³  -1  º  00h (-1: indicates full map)
;==                      ÈÍÍÍÍÍÍÏÍÍÍÍÍÍ¼
;==
;== Entry: (Protected Mode)
;==	ES:EDI = pointer to the save area buffer
;==
;== Exit:  (Protected Mode)
;==	EAX,EBX,CX,ESI,EDI destroyed (note: EDX remains the same)
;==                                                                    LC
;=============================================================================
SaveWindowMapping proc	near
;
;  Header 4 bytes requested by Microsoft
;
	mov	ax, 'C'+'O'+'M'+'P'+'A'+'Q'
	shl	eax,16
;
;  Indicate this to be a full mapping (-1)
;
	dec	ax

	STOS_DWORD_PTR_ES_EDI

	mov	ebx,[page_tables]
	movzx	ecx,[number_EMS_windows]	   ; number of windows to save
SWMloop:
	movzx	esi,[EMS_window_location][ecx*2-2] ; get EMS window location
	lea	esi,es:[ebx][esi*4]		   ; address of PTE

	MOVS_DWORD_USING_ES_ESI

	dec	cx			;Q: Anymore EMS pages?
	jnz	short SWMloop		; Y: continue
 	ret				; N: all done
SaveWindowMapping	endp

align 16
;==============================================================================
;==
;== RestoreWindowMapping: This function saves the current mapping register
;==			  state into the save area specified.  The current
;==			  mapping state consists of the number of window
;==			  mappings followed by the mappings themselves.
;==
;==                      ÉÍÍÍÍÍÍÍÍÍÍÍÍÍ»
;==                      º  Pn page 0  º  4h*(n+1)
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==			 |             |
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                      º  P2 page 0  º  0Ch
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                      º  P1 page 0  º  08h
;==                      ÇÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
;==                      º  P0 page 0  º  04h
;==                      ÇÄÄÄÄÄÄÂÄÄÄÄÄÄ¶
;==                      º Head ³  -1  º  00h (-1: indicates full map)
;==                      ÈÍÍÍÍÍÍÏÍÍÍÍÍÍ¼
;==
;== Entry: (Protected Mode)
;==	ES:ESI = pointer to the restore buffer
;==
;== Exit:  (Protected Mode)
;==	EAX,EBX,CX,ESI,EDI destroyed (note: EDX remains the same)
;==
;== CRITICAL SECTIONS:
;==	The copy from user space is a critical section, because we
;==	do not want to be interrupted when we have copied in a partial
;==	EMS_window entry, as the locgical page number might not be
;==	appropriate for the handle.  Once the copy is done, interruptions
;==	are allowable given that, if we are interrupted, the interruptor
;==	must return control to us with our mapping data structures in
;==	the same state that they were in when we were interrupted.
;==	There ARE critical sections inside SetWindows, since
;==	manipulates the page tables, and GarbageCollects and Reallocates
;==	can cause the data in these page tables to move around.
;==                                                                     LC
;=============================================================================
RestoreWindowMapping proc near
;
;  Make sure this is a full mapping buffer
;
	lods	dword ptr es:[esi]		; (EAX) = header

	inc	ax            			;Q: Is this a valid full map?
	jnz	short RWMerror			; N: error
	shr	eax,16
	cmp	ax, 'C'+'O'+'M'+'P'+'A'+'Q'	;Q: Expected signature?
	jne	short RWMerror			; N: error

	mov	bx,P_SIZE
	movzx	ebx,bx
 	movzx	ecx,[number_EMS_windows]		; number of windows to save
RWMloop:
	movzx	edi,[EMS_window_location][ecx*2-2]	; get EMS window location
	shl	di,2
	add	edi,[page_tables]		; address of PTE

	LODS_DWORD_PTR_ES_ESI
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI

	dec	cx			;Q: Anymore EMS pages?
	jnz	short RWMloop		; Y: continue

	mov	eax,cr3
	mov	cr3,eax

 	ret				; N: all done

RWMerror:
	mov	byte ptr [bp][reg_EAX+1],WINDOW_INDEX_RANGE
	stc
	ret
RestoreWindowMapping	endp


page
align 16
;***********************************************************************
; GetModifyUsersStack
;
; ENTRY:
;	AX - the signed integer amount to modify the stack by
; EXIT:
;	EDI - a 32 bit linear address pointing to the user's stack
; DESCRIPTION:
;	This routine is used for generating and manipulating the user's
;	stack.  This is needed because the location of the user's stack
;	differs depending on how the Int 67h call was done.  If the user
;	had CEMM on (in protected mode) then the stack address is located
;	on the fault stack frame on the ring zero stack.  If CEMM was off
;	(in real mode) then the stack address is in a special data variable.
; CRITICAL SECTIONS:
;	Assuming that we can only enter EMM386.EXE once from real mode
;	(meaning if we were to reenter it, we'd have to be in protected
;	mode), then this procedure is reentrant.
;***********************************************************************
GetModifyUsersStack proc near
	push	eax

;	Check the ring zero stack pointer to see if CEMM is in
;	auto mode with cemm inactive.
;	QLEO Need new mechanism to determine AUTO mode.

	cmp	sp,VMT_STACK - size RegisterStack_struc
	ja	short gmus1

;	If CEMM is on, then pull the user's SS:SP out of the fault
;	stack frame and put it into EDI.

	push	bp
	mov	bp,word ptr [bp.stack_frame_EBP]
	movzx	edi,word ptr [bp.VTFO+VMTF_SS]
	shl	edi,4
	add	[bp.VTFO+VMTF_ESP],ax
	movzx	eax,word ptr [bp.VTFO+VMTF_ESP]
	add	edi,eax
	pop	bp

gmus0:	pop	eax
	ret

;	If CEMM is Inactive, then pull the user's SS:SP out of the
;	special saved locations and put it into EDI.

gmus1:
;LEO	movzx	edi,gs:[EMM_SS_Save]
;LEO	shl	edi,4
;LEO	add	gs:[EMM_SP_Save],ax
;LEO	movzx	eax,gs:[EMM_SP_Save]

;
;  Get base address from GDT selector for user stack (RSS_GSEL)
;
	push	ds
	push	GDTD_GSEL
	pop	ds

	add	gs:[UserSP],ax		; fix user stack pointer
	mov	di,gs:[UserSS]		; get selector
	and	di,0FFF8h		; make it an offset into GDT

	mov	eax,[di+1]		; get base (bits 0-23) in high 3 bytes
	mov	al,[di+7]		; (AL) = base bits 24-31
	ror	eax,8			; (EAX) = base

	movzx	edi,gs:[UserSP]
	add	edi,eax			; (EDI) = user stack linear address
	jmp	short gmus0		; Go return

GetModifyUsersStack endp

even
align 16
;==============================================================================
;==
;== MapInWindows:  This routine takes an array of window/page pairs and maps
;==		   them into the current window mapping.  The array consists
;==		   of either a logical window index or window segment address
;==		   followed by the EMS page to be mapped to it.  The format of
;==		   the window index/window segment is determined by the value
;==		   passed in AL.
;==
;== Entry: (Protected Mode)
;==	[BP]:AL = window location format, 0-window index, 1-window segment
;==	ES:ESI  = a pointer to an array of pages/windows to map in
;==	EDX     = handle strucutre pointer
;==	CX      = number of pages to map in
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH= return code (if error ocurred)
;==	All registers destroyed!
;==                                                                     LC
;=============================================================================
MapInWindows	proc	near
;
;  If number of entries is zero, we're done!
;
	jcxz	short MIWexit		; done if nothing to map

	cmp	cx,[number_EMS_windows]	;Q: Too many windows?
	ja	MIWinvPhyPage		; Y: error

MIWloop:
;
;  Get the logical page and segment/index for the physical EMS 16k page
;
	lods	dword ptr es:[esi]
	movzx	ebx,ax			; (EBX) = logical page number
	shr	eax,16			; (EDI) = physical page number
	mov	edi,eax

	test	byte ptr [bp][reg_EAX],1;Q: Segment or index?
	jz	short MIWidxToPTE
;
;  Verify window segment
;
	or	al,al			;Q: Is this segment bounded by 16K?
	jnz	short MIWinvPhyPage	; N: error
	shr	di,8
	cmp	[EMSsegLoc][di],-1	;Q: Is this a valid window segment?
	je	short MIWinvPhyPage	; N: error

MIWsource:
	mov	ax,di
	shl	di,2			; PTE offset
	add	edi,[page_tables]	; EAX = PTE address

	cmp	bx,UNMAP_WINDOW_OPCODE
	je	short MIWunmap
;
;  Validate logical page range
;
	test	bh,0C0h			;Q: Is logical page too large?
	jnz	short MIWinvLogPage	; Y: error
	shl	bx,2			; N: convert to 4K pages

	cmp	bx,ds:[edx].number_PTEs	;Q: Is logical page within valid range?
	jae	short MIWinvLogPage	; N: error

	xchg	eax,ecx
	xchg	ebx,esi
	mov	cl,4

;EnterCritical			; base PTE index for handle may change
pushf
cli
;
;  Access handle space, and map logical page
;
	add	si,ds:[edx].base_PTE_index
	shl	si,2			; quick edi*4
	add	esi,[page_directory]	; access handle space

	REP_MOVS_DWORD_USING_ES_ESI

;debug	ExitCritical			; recalculate base PTE index for handle
popf
	mov	esi,ebx
	mov	cx,ax

	dec	cx
	jnz	short MIWloop

	mov	eax,cr3
	mov	cr3,eax
MIWexit:
	clc
	ret
align 16
MIWidxToPTE:
;
;  Verify window index
;
	cmp	[xma2ems],TRUE		;Q: Is it in XMA2EMS mode?
	je	MIWxma2ems		; Y: remap windows

	cmp	di,[number_EMS_windows]	;Q: Invalid window?
	jae	MIWinvPhyPage		; Y: error
MIWindex:
	add	di,di			; N: offset for PTE index
	mov	di,[EMS_window_location][di]
	jmp	short MIWsource

MIWinvPhyPage:
	stc
	mov	byte ptr [bp][reg_EAX+1],PHYS_PAGE_RANGE
	jmp	short MIWclearTLB

MIWinvLogPage:
	stc
	mov	byte ptr [bp][reg_EAX+1],LOG_PAGE_RANGE
	jmp	short MIWclearTLB
align 16
;
;  Unmap physical window by mapping one-to-one
;
MIWunmap:
	mov	bx,P_SIZE		; page
	shl	eax,P_SHIFT		; physical address
	or	ax,P_AVAIL		; accessible
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI
	add	eax,ebx
	STOS_DWORD_PTR_ES_EDI

	dec	cx
	jnz	MIWloop
	clc

MIWclearTLB:
	mov	eax,cr3
	mov	cr3,eax
	ret

MIWxma2ems:
	sub	di,250			;Q: P254 or P255?
	ja	short MIWx2cont 	; Y: continue
	add	di,250			; N: restore original window number
MIWx2cont:
	cmp	di,5			;Q: Is the window in 0..5?
	jbe	MIWindex		; Y: ok
	jmp	MIWinvPhyPage		; N: error

MapInWindows endp

align 16
;==============================================================================
;==
;== ActivateRegSet:  This routine switches to the new alternate register set.
;==		     If the set is zero or the dummy, then the current state
;==		     is not saved but the pointer provided is used to restore
;==		     the current mapping and the current register set becomes
;==		     zero.  If the set is not zero, then the current register
;==		     state is saved before restoring the state of the new
;==		     register set.
;==
;== Entry: (Protected Mode)
;==	BL = register set to activate
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==	EDX = same, all others destroyed
;==                                                                      LC
;=============================================================================
ActivateRegSet	proc	near
;
;  Access AR context
;
	movzx	si,bl
	shl	si,3
	.errnz size RegisterSet_struc-8

;
;  Each AR set has a 32 bit pointer to a 4K block. This 4K block is initialized
;  to the current PT 0 at the time the AR set was allocated. Now we need to make
;  this PT0 the current one.  This is done by updating the first Page Directory
;  entry to point to this AR's PT0.  In addition, we make the PTE associated
;  with the current PT0 to point to this AR's PT 0. This is done as the selector
;  PAGET_GSEL is used through out the code to access the page tables. By
;  changing the PTE associated with the current PT 0 the access thru
;  PAGET_GSEL will now go this AR's PT.  Also note that when the current PT 0
;  is below 4M then the PTE in this AR's PT 0 has to be modified. Else the PTE
;  in the appropriate PT should be modifed.
;
	mov	edi,[register_set][si].page_table_ptr
	mov	eax,[page_tables]


	mov	ecx,eax			; ecx = address of current PT 0
	shr	eax,10			; offset in PT for PT0

	add	eax,ecx       		; EAX = address for PT0 PTE (above 4M)

	cmp	ecx,4*1024*1024		;Q: Is current PT0 below 4M
	jae	short ARScont		; N: update appropriate PTE
	sub	eax,ecx			; Y: restore EAX to PT0 offset
	add	eax,edi   		; EAX = address for PT0 PTE (below 4M)
ARScont:
;EnterCritical
pushf
cli
	or	di,P_AVAIL		; make PTE available
	mov	es:[eax],edi		; make PTE for PT0

	mov	eax,[page_directory]
	mov	es:[eax],edi		; set PT0 address in page directory

;
;  The state of the A20 line at the time the AR was allocated was copied into
;  the AR set. If the current state of the A20 line is different from what is
;  specifed in the AR set then we must update the appropriate PT 0 entries.
;  If the A20 state has not changed then we're done.
;
	mov	[current_register_set],bl
	mov	ax,gs:[Current_State]
	and	ax,fState_A20Ena
	cmp	[register_set][si].a20_state,ax
	jne	short ARSupdateHMA

ARSexit:
;
;  The TLB is flushed since the page tables were changed.
;
	mov	eax,cr3
	mov	cr3,eax
;debug ExitCritical
popf
	ret

;
; At this point edi has the 32 bit address of the current PT 0
;
ARSupdateHMA:
	mov	[register_set][si].a20_state,ax

	mov	eax,0C0000000h		; assume WEITEK map on

	test	gs:[Weitek_State],fWeitek_Map ;Q: WEITEK on?
	jnz	short ARS_HMAcont	      ; Y: WEITEK map

	xor	eax,eax			; assume A20 disabled

	test	gs:[Current_State],fState_A20Ena ;Q: Enable A20?
	jz	short ARS_HMAcont  	         ; N: wrap with page tables
	mov	eax,gs:[HMAptr]		         ; Y: no wrap (functional HMA)

ARS_HMAcont:
	and	di,not (P_SIZE-1)	; clear status bits
	or	ax,P_AVAIL		; make PTE's accessible
	mov	esi,P_SIZE
	mov	ebx,100h		; 1 Meg index
	mov	cx,10h			; 64k worth of entries
ARS_HMAloop:
	mov	es:[edi][ebx*4],eax
	add	eax,esi
	inc	bx
	dec	cx			; dec/jnz faster than loop!!!
	jnz     short ARS_HMAloop	; loop until done with PTEs?
	jmp	short ARSexit

ActivateRegSet	endp

_TEXT	ends

END

