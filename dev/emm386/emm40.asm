.386p
page	58,132
;******************************************************************************
	title	 emm40.asm - EMM function handlers
;******************************************************************************
;
; (C) Copyright MICROSOFT Corp. 1988-1991
; (C) Copyright COMPAQ Computer Corp. 1988-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;		EMMLIB.LIB - Expanded Memory Manager Functions Library
;
; Module:	EMS 4.0 Function Handler
;
; Version:	1.00
;
; Date: 	December 15, 1988
;
; Authors:	Dan Mazina (original)
;		Dick Vail
;		Leo Cohen (designed and implemented memory management and
;			   mapping functions to improve performance) LC
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   12/15/88	1	initial code
;   03/16/89  4.00      Change GetMappablePhysicalArray for new window order.
;
;   01/14/91  M001	In performdatamove fix up pointers if the direction
;			of the move is reversed after completing the dword
;			moves
;******************************************************************************
;
; AlterPageMapAndCall bug fix from Toshiba
;
; DTS0514 - 05/18/92
;
;	One recent change to the driver is the way the EMS function
;       calls eventually get into protected mode and into this source.
;
;       In the previous method, the INT 67h issued in virtual mode caused
;       the driver to enter the EMM_rEntry routine in EMM.ASM.  That routine
;       simply reflected the interrupt to the protected mode handler
;       AlterPageMapAndCall (in this routine).  This routine makes various
;       assumptions about the layout of user's (virtual mode) stack 
;       in order to modify it.
;
;       In the new method, the INT 67h issued in virtual mode once again 
;       takes the flow of execution to the EMM_rEntry routine in EMM.ASM.  
;       However, this routine has been rewritten to push GS on the
;       user's stack as well as make a far call to the EMM_rEntryFar
;       routine in the new source file UMBSEG.ASM.  These actions place
;       3 extra words on the user's stack, 3 words that AlterPageMapAndCall
;       (APMC) was not aware of.  Thus, this routine screwed up the return 
;       addresses on the user's stack and quickly took the machine into
;       the weeds.
;
;       In order to correctly modify this source you have to fully understand
;       the flow of execution for the APMC service in the old EMM386.
;       When APMC is entered the first time, it modifies the user's stack
;       so that the requested far address is at the time followed by the
;       user's flags register, the idea being that an eventual IRET
;       (executed in virtual mode) will take the flow of execution to the
;       desired EMS page location where the user's program is (e.g. 
;       D000:xxxx).  APMC also saves the EMS window mapping on the stack.
;       Farther down on the stack APMC places another return address
;       on the stack, that of the AlterAndCallEntry routine in EMM.ASM.  
;       Thus, when the user's called program issues a RETF, control passes
;       to this routine which issues another INT 67h in virtual mode
;       with AX=56FF.  APMC is once again entered by the same route
;       (i.e., through EMM_rEntry and EMM_rEntryFar, etc.) placing yet
;       another 3 extra words on the user's stack.  When APMC sees AL=FF,
;       it knows that it is time to clean up the user's stack, so it
;       throws away the return address to AlterAndCallEntry's 
;       AlterAndCallReturn label (where a friendly HLT instruction awaits
;       all who dare to pass).  Next, the saved EMS window mapping is
;       restored and removed from the user's stack.  Then APMC exits and
;       eventually returns to virtual mode, where the application's
;       return address is waiting to be popped and transferred to.
;
;	The way I fixed this major bug is as follows.  MS really screwed
;       up 2 things.  1) APMC always expected that an IRET would be 
;       executed once the protected mode interrupt handler returned the
;       CPU to virtual mode.  However, MS changed the EMM_rEntry routine
;       to call EMM_rEntryFar, so a RETF is actually executed (thus
;       leaving the flags on the stack).  In order to fix this problem
;       (and make changes local to APMC), I modified APMC to "push"
;       a copy of GS and the return address of EMM_rEntry so that
;       control will eventually pass back to that routine where an
;       IRET will take control (in virtual mode) to the user's
;       program (in the EMS page frame).  Then, when that program
;       issues a RETF control will pass on to the AlterAndCallEntry
;       subroutine which issues another INT 67h to restore the
;       saved window mapping and clean up the user's stack.
;
;       2) The APMC routine needs to be aware of the 3 extra words 
;       on the user's stack due to the push of GS and the far call from 
;       the EMM_rEntry routine.  Furthermore, when APMC is entered 
;       the second time (with AX=56FF) there are 2 copies of EMM_rEntry's 
;       return address and GS on the user's stack (one before and one 
;       after the saved window mapping data, 6 words in all).  So, I
;       modified APMC in a couple places to be aware of the "new"
;       stack layout so that it can modify return addresses correctly.
;
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
public	GetSetPartialPageMap
public	MapUnmapMultipleHandlePages
public	ReallocatePages
public	GetSetHandleAttribute
public	GetSetHandleName
public	GetHandleDirectory
public	AlterPageMapAndJump
public	AlterPageMapAndCall
public	MoveExchangeMemoryRegion
public	GetMappablePhysicalAddress
public	GetExpandedMemoryHWInfo
public	AllocateStandardRawPages
public	AlternateMapRegisterSet
public	GetRegisterSet
public	SetRegisterSet
public	PrepareForWarmBoot
public	EnableDisableOSFunctions
public	FormLinearAddress
;******************************************************************************
; INCLUDES
;******************************************************************************
include vdmseg.inc
include vdmsel.inc
include vm386.inc
include	emm386.inc
include emmfunct.inc
include emmdata.inc
include	page.inc

ifdef BETA
include emm40.pub
endif
;******************************************************************************
; DEFINES
;******************************************************************************

;******************************************************************************
; EXTERNALS
;******************************************************************************
R_CODE	segment
	extrn	AlterAndCallEntry:far
	extrn	AlterandCallReturn:far
R_CODE	ends

_TEXT	segment
	extrn	AllocatePages:near
	extrn	AllocEMSMem:near
	extrn	AddFreeEMS:near
	extrn	MoveHandleSpace:near
	extrn	MapInWindows:near
	extrn	SaveWindowMapping:near
	extrn	RestoreWindowMapping:near
	extrn	GetUnallocatedPageCount:near
	extrn	GetModifyUsersStack:near
	extrn	ActivateRegSet:near
	extrn	ValidateHandle:near
	extrn	GrowEMSPool:near
	extrn	ShrinkEMSpool:near
ifdef QEMS
	extrn	ShiftHandles:near
	extrn	GarbageCollect:near
	extrn	Get4kPages:near
	extrn	FindWindowIndex:near
	extrn	SetWindows:near
	extrn	FindNextFreeHandleSpace:near
	extrn	SetPT0Cntxt:near
	extrn	RestorePT0Cntxt:near
endif
_TEXT	ends

page
;******************************************************************************
; SEGMENTS
;******************************************************************************

_TEXT	segment
	assume cs:_TEXT,ds:_DATA,es:_DATA,gs:R_CODE
even
;==============================================================================
;==
;== GetSetPartialPageMap: This routine saves/restores a partial mapping context
;==		          to/from an external buffer.
;==
;== Entry: (Protected Mode)
;==	AL = Subfunction code
;==	     0 : get page map
;==	     1 : set page map
;==	     2 : return save size
;==	BX = number of entries to save for function 2
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==	AL = size of the save area for function 2
;==                                                                   LC
;=============================================================================
GSPPMvecTable	label	word
  dw OFFSET _TEXT:GetPartialMap
  dw OFFSET _TEXT:SetPartialMap
  dw OFFSET _TEXT:GSPPM_return_function
GSPPMFuncNum	equ	($-GSPPMvecTable)/2
align 16
GetSetPartialPageMap	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK
;
;  The subfunction number is checked to see if it is valid
;
	cmp	al,GSPPMFuncNum
	jae	short GSPPMinvFunc
;
;  The jump table is used to execute the subfunction
;
	movzx	si,al
	add	si,si
	jmp	GSPPMvecTable[si]
align 16
;
;  The number of windows is checked to see if it is valid.
;
GSPPM_return_function:
	cmp	bx,[number_EMS_windows]
	ja	short GSPPMinvPhyPage
;
;  There is a 4 byte header and 16 bytes per window
;
	.errnz (size EMSmap_struc)-4
	shl	bl,2			; Each segment needs 4 bytes
	add	bl,4			; Plus 4 bytes header
	mov	byte ptr [bp.reg_EAX],bl

GSPPMexit:
	ret

GSPPMinvFunc:
	mov	byte ptr [bp][reg_EAX+1],INVALID_SUBFUNCTION ; assume invalid
	jmp	short GSPPMexit
GSPPMinvPhyPage:
	mov	byte ptr [bp][reg_EAX+1],PHYS_PAGE_RANGE ; assume invalid
	jmp	short GSPPMexit
GetSetPartialPageMap	endp

align 16
;==============================================================================
;==
;== GetPartialMap: This routine gets a partial page map as defined by
;==		   ES:ESI and saves it into the user's buffer at ES:EDI.
;==
;== Entry: (Protected Mode)
;==	[BP] DS:SI = user's pointer to a list of windows to get
;==	[BP] ES:DI = user's pointer to save the page map in
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                  LC
;=============================================================================
GetPartialMap	proc	near
;
;  The user's save buffer is changed into a 32 bit
;
	movzx	eax,[bp.reg_ES]
	shl	eax,4
	movzx	edi,di
	add	edi,eax
;
;  The user's window list is changed into a 32 bit
;
	movzx	esi,[bp.reg_DS]
	shl	esi,4
	movzx	eax,word ptr [bp.reg_ESI]
	add	esi,eax
;
;  Header information is placed in buffer
;
	mov	ax,'C'+'O'+'M'+'P'+'A'+'Q'
	shl	eax,16

	lods	word ptr es:[esi]
	STOS_DWORD_PTR_ES_EDI

	mov	cx,ax
	jcxz	short GPMexit

	cmp	cx,[number_EMS_windows]
	ja	short GPMinvWinIndex

	mov	ebx,[page_tables]
	xor	eax,eax
GPMloop:
;
;  Save specified windows' context
;
	lods	word ptr es:[esi]	; get window segment address

	or	al,al			;Q: Is it 4K bounded?
	jnz	short GPMinvPhyPage	; N: error
	xchg	ah,al			; Y: PTE index

	cmp	EMSsegLoc[eax],-1	;Q: Is this a valid window segment?
	je	short GPMinvPhyPage	; N: error

	mov	edx,es:[ebx][eax*4]	; get PTE
	xor	dl,dl
	or	eax,edx			; EAX = PTE + AL=16K block

	STOS_DWORD_PTR_ES_EDI		; save window mapping
	xor	eax,eax

	dec	cx
	jnz	short GPMloop

GPMexit:
	ret

GPMinvWinIndex:
	mov	byte ptr [bp][reg_EAX+1],WINDOW_INDEX_RANGE ; assume bad range
	jmp	short GPMexit
GPMinvPhyPage:
	mov	byte ptr [bp][reg_EAX+1],PHYS_PAGE_RANGE ; assume invalid
	jmp	short GPMexit

GetPartialMap	endp

align 16
;==============================================================================
;==
;== SetPartialMap: This routine restore a partial page map as defined by
;==		   ES:ESI.
;==
;== Entry: (Protected Mode)
;==	[BP] DS:SI = user's pointer to a list of windows to get
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                  LC
;=============================================================================
SetPartialMap	proc	near
;
;  The user's restore buffer pointed to by a 32 bit linear address
;
	movzx	esi,[bp.reg_DS]
	shl	esi,4
	movzx	eax,word ptr [bp.reg_ESI]
	add	esi,eax
;
;  The number of entries to read is loaded into CX and validated.
;
	lods	dword ptr es:[esi]
	mov	cx,ax			; save the number of entries

	shr	eax,16
	cmp	ax,'C'+'O'+'M'+'P'+'A'+'Q'
	jne	short SPMinvWinIndex

	jcxz	short SPMexit		; if no entries, nothing to map

	cmp	cx,[number_EMS_windows] ;Q: Is it out of range?
	ja	short SPMinvWinIndex	; Y: error

	mov    	ax,P_SIZE
	mov	edx,eax
	mov	ebx,[page_tables]
	push	bp
	mov	bp,P_AVAIL
SPMloop:
;
;  The window index is loaded into DI first and verified.
;
	lods	dword ptr es:[esi]	; get PTE index and PTE
	movzx	edi,al			; PTE index

	cmp	[EMSsegLoc][di],-1	;Q: Is this a valid window segment?
	je	short SPMinvPhyPage	; N: exit

	xor	al,al
	or	ax,bp

	mov	es:[ebx][edi*4],eax
	inc	di
	add	eax,edx
	mov	es:[ebx][edi*4],eax
	inc	di
	add	eax,edx
	mov	es:[ebx][edi*4],eax
	inc	di
	add	eax,edx
	mov	es:[ebx][edi*4],eax

	dec	cx
	jnz	short SPMloop
	pop	bp

SPMclearTLB:
	mov	eax,cr3
	mov	cr3,eax

SPMexit:
	ret

SPMinvWinIndex:
	mov	byte ptr [bp][reg_EAX+1],WINDOW_INDEX_RANGE ; assume invalid
	jmp	short SPMexit
SPMinvPhyPage:
	pop	bp
	mov	byte ptr [bp][reg_EAX+1],PHYS_PAGE_RANGE ; assume bad range
	jmp	short SPMclearTLB

SetPartialMap	endp


page
align 16
;==============================================================================
;==
;== MapUnmapMultipleHandlePages:  This routine takes the entries in DS:SI and
;==				  maps them into the current windows.  The first
;==				  value in DS:SI is the EMS page number followed
;==				  by the window index/window segment address.
;==				  The format is determined by AL.  If the
;==				  logical page is 0FFFFh then the window is
;==				  unmapped by changing its mapping to be 1 to 1.
;==
;== Entry: (Protected Mode)
;==	AL     	   = window location format, 0-window index, 1-window segment
;==	DX 	   = handle
;==	CX 	   = number of entries to map into the windows
;==	[BP] DS:SI = pointer to the array of entries to map in
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                   LC
;=============================================================================
MapUnmapMultipleHandlePages	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK
;
;  The subfunction number is checked.
;
	cmp	al,2
	jae	short MUMHPinvFunc

;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	short MUMHPinvHandle	; N: error

	movzx	edx,dx			; access handle structure
	shl	dx,2
	add	edx,[hndl_tbl_ptr]	; (EDX) = pointer to handle structure
	.errnz	(SIZE HandleTable_struc-4)

	cmp	ds:[edx].base_PTE_index,FREE ;Q: Handle in use?
	je	short MUMHPinvHandle	     ; N: error

;
;  The user's segment value is changed into a linear address and placed into
;  ES:ESI for the source.
;
	movzx	esi,[bp.reg_DS]
	shl	esi,4
	movzx	ebx,word ptr [bp.reg_ESI]
	add	esi,ebx
;
;  ES:ESI points to the array to map in.  AL is the subfunction code.
;  DX is a pointer to the handle structure.  CX is the # of entries to process.
;
	call	MapInWindows

MUMHPexit:
	ret

MUMHPinvFunc:
	mov	byte ptr [bp][reg_EAX+1],INVALID_SUBFUNCTION ; invalid
	jmp	short MUMHPexit
MUMHPinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE ; invalid handle
	jmp	short MUMHPexit

MapUnmapMultipleHandlePages	endp

page
align 16
;==============================================================================
;==
;== ReallocatePages: This routine adjusts the number of 16k EMS pages allocated
;==		     to a handle to match the value in BX.  This may include
;==		     decreasing the current amount, increasing it or not
;==		     modifing it at all.
;==
;== Entry: (Protected Mode)
;==	BX = new number of 16k EMS pages
;==	DX = the EMS handle
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==                                                                   LC
;=============================================================================
ReallocatePages	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK

;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	RPinvHandle		; N: error

	movzx	esi,dx			; handle index in ESI
	mov	edx,esi
	shl	si,2
	.errnz	(SIZE HandleTable_struc-4)
	add	esi,[hndl_tbl_ptr]	; (ESI) = pointer to handle table

	cmp	ds:[si].base_PTE_index,FREE	;Q: Is handle free?
	je	RPinvHandle		    	; Y: error
;
;  Check reallocation count
;
	test	bh,0C0h
	jnz	RPnotEnoughTotEMS
;
;  Number of needed pages is calculated in CX
;
	mov	cx,ds:[si].number_PTEs
	shr	cx,2
	sub	cx,bx			;Q: Count grow?
	jl	short RPgrow		; Y: growing
	je	RPexit			; N: done (stayed the same!)

;EnterCrit			; BEGIN CRITICAL SECTION
pushf
cli
	shl	bx,2
	xchg	bx,[si].number_PTEs	; set new size, maintain old one
	sub	[UsedEMSPages],cx	; increase number of used EMS pages
	movzx	edi,[si].base_PTE_index	; start index for this handle
	add	di,bx			; end of old size
	cmp	di,[TopOfUsedEMSspace]	;Q: Is this handle on top?
	jne	short RPcont1		; N: continue
	sub	di,bx			; Y: re-adjust top of used EMS space
	add	di,[si].number_PTEs	; new top of used EMS space
	mov	[TopOfUsedEMSspace],di
	jmp	short RPcont2
RPcont1:
	sub	di,bx			; start of handle space
	add	di,[si].number_PTEs	; start index for deallocation
RPcont2:
	shl	di,2			; make it an offset
	add	edi,[page_directory]	; make it an address
	mov	edx,10h			; size of 4 PTEs
	xor	bx, bx
RPfreePages:
	mov	ax,not fEMSPageAllocated
	xchg	ax, word ptr es:[edi]
	and	word ptr es:[edi], ax
	bt	ax, fXMSPageAllocatedBit; from dynamic XMS?
	rcl	bl, 1			; CY to bl
	or	bh, bl			; bh = XMS flag
	add	edi,edx			; next EMS page
	dec	cx			;Q: Any more?
	jnz	short RPfreePages	; Y: deallocate more

;ExitCrit			; END CRITICAL SECTION
popf
	or	bh, bh			;Q: Any XMS pages released?
	jz	short RPexit		; N:

	call	ShrinkEMSpool		; Y: give them back
	jmp	short RPexit		; we're done!
;
;  Need to grow handle by CX 4K pages
;
RPgrow:
;
;  Get total EMS pages
;
	mov	ax,[TopOfHandleSpace]	; top of handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2

	cmp	ax,bx			;Q: Enough free EMS pages?
	jb	short RPnotEnoughTotEMS	; N: error
;
;  Get free EMS pages (at this time!)
;
	mov	ax,[TopOfFreeEMSspace]	; top of free EMS handle space
	sub	ax,FIRST_HANDLE_PTE	; subtract start of handle space
	shr	ax,2			; convert to 16K pages
	sub	ax,[UsedEMSPages]	; number of free EMS pages

	neg	cx			; number of EMS pages needed to grow
	cmp	ax,cx			;Q: Enough free EMS pages to grow?
	jae	short RPgotEnough	; Y:

	mov	bx, cx			; N: Try to grow EMS space by CX pages
	sub	bx, ax			;    less already free pages
	call	GrowEMSPool
	jc	short RPnotEnoughFreeEMS

RPgotEnough:

;EnterCrit			; BEGIN CRITICAL SECTION
pushf
cli
;
;  If this handle is on top of the used EMS space, just go to add memory
;
	mov	dx,cx			 	; save number of EMS pages to grow
	movzx	edi,ds:[si].base_PTE_index	; starting index for handle
	mov	bx,ds:[si].number_PTEs		; number of PTEs
	add	di,bx			; entry after this handle
	cmp	di,[TopOfUsedEMSspace]	;Q: Is this handle on top?
	jae	short RPallocEMS	; Y: don't need to move handle
;
;  Move handle space for this handle to the top of used EMS space
;	BX  = number of slots to move the PTEs
;	DI  = starting PTE entry to move
;	ECX = page directory linear address
;
	sub	di,bx			; starting index for this handle
	mov	ecx,[page_directory]
	call	MoveHandleSpace
	mov	di,[TopOfUsedEMSspace]
	mov	ds:[si].base_PTE_index,di	; starting index for handle
	add	di,bx				; new top of Used EMS space
	mov	[TopOfUsedEMSspace],di

RPallocEMS:
;
;  Allocate necessary pages to this handle
;	BX  = Number of PTE's requested
;	SI  = handle structure pointer
;
;  because AllocEMSMem is being called with INTs OFF, there is no chance that
;  another handle space will be located above the reallocated handle
;
	mov	bx,dx
	call	AllocEMSMem
;
;  If CY is set, not enough EMS pages were found!  Even though we checked above,
;  because this routine is re-entrant, it could have changed.
;
	jc	short RPnotEnoughFreeEMSX

;ExitCrit			; END CRITICAL SECTION
popf

RPexit:
	ret

RPnotEnoughTotEMS:
	mov	byte ptr [bp][reg_EAX+1],NOT_ENOUGH_EXT_MEM
	jmp	short RPoriginalCount
RPnotEnoughFreeEMSX:
;ExitCrit			; END CRITICAL SECTION
popf
RPnotEnoughFreeEMS:
	mov	byte ptr [bp][reg_EAX+1],NOT_ENOUGH_FREE_MEM
RPoriginalCount:
	mov	bx, [si].number_PTEs		; on failure, return original
	shr	bx, 2				; page count in BX
	mov	word ptr [bp][reg_EBX], bx
	jmp	short RPexit
RPinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE
	jmp	short RPexit
ReallocatePages	endp

page
align 16
;******************************************************************************
; GetSetHandleAttribute
;
; ENTRY
;	AL - subfunction number, 0 - Get, 1 - Set, 2 - capabilities
;	DX - handle for functions 0 & 1 (get/set attributes)
; EXIT
;	AH - return code
;	AL - zero for function 2 (get attribute capabilities)
; DESCRIPTION
;	This function returns invalid for the Get and Set subfunctions
;	as CEMM only supports volatile handles. For function 3, AL is set
;	to zero to indicate that all handles are volatile.
;***********************************************************************
GetSetHandleAttribute	proc	near

		; The subfunction number is checked.
	cmp	al,2
	ja	short GSHA_invalid_function
	jb	short GSHA_not_supported

		; If the function is to get the attribute capabilities
		; then return 0 in AL to indicate all handles are volatile.
	mov	byte ptr [bp.reg_EAX],0h
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	short GSHA_return_code

		; Functions 0 and 1 are not supported.
GSHA_not_supported:
	mov	byte ptr [bp.reg_EAX + 1],NOT_SUPPORTED
	jmp	short GSHA_return_code

		; Function numbers greater then 2 are not defined.
GSHA_invalid_function:
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SUBFUNCTION

GSHA_return_code:
	ret
GetSetHandleAttribute	endp

page
align 16
;******************************************************************************
; GetSetHandleName
;
; ENTRY
;	AL - subfunction number, 0 - Get, 1 - Set
;	DX - handle index
;	ES:DI - user's data area for the handle's name in function 0
;	DS:SI - user's data area for the handle's name in function 1
; EXIT
;	AH - return code
; DESCRIPTION
;	This routine either set or gets the name of a handle.  All names
;	are 8 bytes long where all nulls in the name indicate that it has
;	not been set.
;***********************************************************************
GetSetHandleName	proc	near

		; The subfunction number is checked to see if it is valid.
	cmp	al,2
	jb	short GSHN_check_function
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SUBFUNCTION
	jmp	GSHN_return_code

GSHN_check_function:
		; The handle is validated.  SI has the handle struct index.
	; CY = !ValidateHandle(DX);
	call	ValidateHandle
	jc	GSHN_return_code
	mov	esi,edx
	cmp	al,0
	je	short GSHN_get_name

		; The Set Handle's Name function is being done.
		; The handle index in SI is shifted to DI.
	mov	edi,esi

		; The user's segment:offset value is changed into a 32 bit
		; linear address.  ESI has the address.  ES is zero based.
	movzx	esi,[bp.reg_DS]
	shl	esi,4
	movzx	eax,word ptr [bp.reg_ESI]
	add	esi,eax
ifdef QEMS
	push	DATA32_GSEL
	pop	es
endif
		; The new handle name is loaded into EAX,EBX.  There are 8
		; characters placed into 2 dwords.
	lods	dword ptr es:[esi]
	mov	ebx,eax
	lods	dword ptr es:[esi]

	MOV	ESI, [hndl_nam_ptr]

		; The new handle name is checked for all zeros which is used
		; to clear names and is always legal
	or	eax,eax
	jnz	short GSHN_check_name
	or	ebx,ebx
	jz	short GSHN_set_name

		; The handle table is now searched to see if this handle name
		; is already being used. DX is the current handle index.
GSHN_check_name:
	mov	cx,[total_handles]
	xor	edx,edx
GSHN_compare_name:
	CMP	EBX, DS:[ESI+EDX*8]
	jne	short GSHN_next_handle
	CMP	EAX, DS:[ESI+EDX*8]+4
	je	short GSHN_invalid_name
GSHN_next_handle:
	inc	dx
	loop	GSHN_compare_name

		; If this is reached then the name is valid so copy it in.
GSHN_set_name:
	MOV	DS:[ESI+EDI*8], EBX
	MOV	DS:[ESI+EDI*8]+4, EAX
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	short GSHN_return_code

		; If this is reached then the name is already being used so
		; set the appropriate error message.
GSHN_invalid_name:
	mov	byte ptr [bp.reg_EAX + 1],INVALID_NAME
	jmp	short GSHN_return_code

		; The Get Handle's Name function is being done.
		; The user's segment:offset value is changed into a 32 bit
		; linear address.  EDI has the address.  ES is zero based.
GSHN_get_name:
	movzx	eax,word ptr [bp.reg_ES]
	shl	eax,4
	movzx	edi,di
	add	edi,eax
	push	DATA32_GSEL
	pop	es
	MOV	EBX, [hndl_nam_ptr]

		; The 8 bytes are read as two dwords and written in the buffer.
	MOV	EAX, DS:[EBX+ESI*8]
	mov	es:[edi],eax
	add	edi,4
	MOV	EAX, DS:[EBX+ESI*8]+4
	mov	es:[edi],eax
	add	edi,4

		; The success return code is set.
	mov	byte ptr [bp.reg_EAX + 1],OK

GSHN_return_code:
	ret
GetSetHandleName	endp

page
align 16
;******************************************************************************
; GetHandleDirectory
;
; ENTRY
;	AL - subfunction, 0 - Get Directory, 1 - Name Search, 2 - Total Handles
;	ES:DI - pointer to user's data buffer for function 0
;	DS:SI - pointer to user's data buffer for function 1
; EXIT
;	AH - return code
;	AL - number of handles active for function 0 (get directory)
;	DX - handle index for function 1 (search for named handle)
;	BX - number of total handles for function 2 (get handle count)
; DESCRIPTION
;	This routine will either write all active handles and their names into
;	the user's buffer, search the handles for a specific name and return
;	its handle index or return the count of all allocated handles.
;***********************************************************************
GetHandleDirectory	proc	near

		; The subfunction number is checked to see if it is valid.
	cmp	al,2
	ja	short GHD_invalid_function
	jb	short GHD_check_function

		; If this is reached then the function is 2, Get Total Handles.
		; Set the BX return value for the number of handles.
	mov	ax, [total_handles]
	mov	word ptr [bp.reg_EBX],ax
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	GHD_return_code

		; If the the function code is invalid then set the error code.
GHD_invalid_function:
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SUBFUNCTION
	jmp	GHD_return_code

GHD_check_function:
	cmp	al,1
	je	short GHD_search_function

		; If this is reached then the user is doing function 0,
		; Get Handle Directory.

		; The user's segment:offset value is changed into a 32 bit
		; linear address.  EDI has the address.  ES is zero based.
GHD_load_users_buffer:
	movzx	eax,word ptr [bp.reg_ES]
	shl	eax,4
	movzx	edi,di
	add	edi,eax
	push	DATA32_GSEL
	pop	es

		; BX is the handle index being checked. CX is the number of
		; handles to process.  The upper 16 bits of EAX are used.
	mov	cx,[total_handles]
	xor	ebx,ebx
	MOV	ESI, [hndl_tbl_ptr]
	MOV	EDX, [hndl_nam_ptr]
GHD_check_handle:
	CMP	DS:[ESI+EBX*4].base_PTE_index, FREE
	je	short GHD_next_handle

		; If the handle is active then write out the handle index.
	mov	es:[edi],bx
	add	edi,2

		; The handle's name is now written out as two dwords.
	MOV	EAX, DWORD PTR DS:[EDX+EBX*8]
	mov	es:[edi],eax
	add	edi,4
	MOV	EAX, DWORD PTR DS:[EDX+EBX*8]+4
	mov	es:[edi],eax
	add	edi,4

GHD_next_handle:
	inc	bx
	loop	GHD_check_handle

		; The number of handles in the directory is now written out.
		; The return code is set for success.
	mov	al,byte ptr gs:[handle_count]
	mov	byte ptr [bp.reg_EAX],al
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	short GHD_return_code


		; This code handles the search for named handle function.
GHD_search_function:

		; The user's segment:offset value is changed into a 32 bit
		; linear address.  ESI has the address.  ES is zero based.
	movzx	esi,[bp.reg_DS]
	shl	esi,4
	movzx	eax,word ptr [bp.reg_ESI]
	add	esi,eax
	push	DATA32_GSEL
	pop	es

		; The handle name is loaded into EAX,EBX.  There are 8
		; characters placed into 2 dwords.
	lods	dword ptr es:[esi]
	mov	ebx,eax
	lods	dword ptr es:[esi]

		; The handle table is now searched. DX is the handle index.
	mov	cx, [total_handles]
	xor	edx,edx
	MOV	EDI, [hndl_nam_ptr]
GHD_compare_name:
	CMP	EBX, DS:[EDI+EDX*8]
	jne	short GHD_next_search_handle
	CMP	EAX, DS:[EDI+EDX*8]+4
	je	short GHD_found_handle
GHD_next_search_handle:
	inc	dx
	loop	GHD_compare_name

		; If this is reached then the handle name could not be found.
	mov	byte ptr [bp.reg_EAX + 1],NAME_NOT_FOUND
	jmp	short GHD_return_code

		; The handle index is returned in DX.
GHD_found_handle:
	mov	word ptr [bp.reg_EDX],dx
	mov	byte ptr [bp.reg_EAX + 1],OK

GHD_return_code:
	ret
GetHandleDirectory	endp

page
MapAndJump_struc	struc
	jump_offset	dw	?
	jump_segment	dw	?
	map_size	db	?
	map_offset	dw	?
	map_segment	dw	?
MapAndJump_struc	ends
align 16
;==============================================================================
;==
;== AlterPageMapAndJump:  This routine modifies the window page mapping based
;==			  on the array in ES:EDI and then jumps to the provided
;==			  address.  The jump is done by	modifing the return
;==			  address on the user's stack so that instead of
;==			  returning to the caller's next instruction, the
;==			  return goes to the new jump address.
;==
;== Entry: (Protected Mode)
;==	AL 	   = window location format, 0-window index, 1-window segment
;==	DX 	   = handle index
;==	[BP] DS:SI = pointer to the map and jump structure
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                    LC
;=============================================================================
AlterPageMapAndJump	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK
;
;  The subfunction number is checked.
;
	cmp	al,2
	jae	short APMJinvFunc

;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	short APMJinvHandle	; N: error

	movzx	edx,dx			; access handle structure
	shl	dx,2
	add	edx,[hndl_tbl_ptr]	; (EDX) = pointer to handle structure
	.errnz	(SIZE HandleTable_struc-4)

	cmp	ds:[edx].base_PTE_index,FREE ;Q: Handle in use?
	je	short APMJinvHandle	     ; N: error

;
;  The user's segment value is changed into a linear address and put into EDI
;
	movzx	edi,[bp.reg_DS]
	shl	edi,4
	movzx	ebx,word ptr [bp.reg_ESI]
	add	edi,ebx
;
;  The target address is saved temporarily in case it is lost by the map call.
;
	push	es:[edi].jump_segment
	push	es:[edi].jump_offset
;
;  The elements to process are placed in CX and the new DS:SI values are loaded.
;
	movzx	esi,es:[edi].map_segment
	shl	esi,4
	movzx	ecx,word ptr es:[edi].map_offset
	add	esi,ecx
	movzx	cx,es:[edi].map_size
;
;  ES:ESI points to the array to map in.  AL is the subfunction code.
;  DX is a pointer to the handle structure.  CX is the # of entries to process.
;
	push	edi
	call	MapInWindows
	pop	edi
	pop	cx			; (CX) = jump offset
	pop	ax			; (AX) = jump segment
;
;  If error, don't jump
;
	jc	short APMJexit
;
;  The return address to the user's code is changed to the new jump address.
;  ES:EDI points to the user's stack.AX is the amount to modify the stack pointer
;  by.
;
	test	gs:[TrapFlags],fI67noReflect ;Q: Do we need to reflect INT 67h?
	jz	short APMJuser		     ; Y: skip ahead

;
;  Go directly to the target by editing the IRETD frame on the ring 0 stack.
;
	push	bp
	mov	bp,word ptr [bp.stack_frame_EBP]
	movzx	ecx,cx			; Zero-extend the offset
	mov	dword ptr [bp.VTFO+VMTF_EIP],ecx
	mov	[bp.VTFO+VMTF_CS],ax
	pop	bp
	ret				; Save the cost of a jump

APMJinvFunc:
	mov	byte ptr [bp][reg_EAX+1],INVALID_SUBFUNCTION ; invalid
	jmp	short APMJexit
APMJinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE ; invalid handle
	jmp	short APMJexit

;
;  We need to reflect the interrupt, place the target address on user's stack.
;
APMJuser:
	push	ax
	xor	eax,eax
	mov	esi,edi
	call	GetModifyUsersStack
	pop	ax			; (AX) = target segment
	mov	es:[edi],cx
	mov	es:[edi+2],ax
APMJexit:
	ret
AlterPageMapAndJump	endp

page

AlterandCall_Frame	struc

; DTS0514 *******************************************************************
; TAIS - Added the return address and saved GS register of the EMM_rEntry
; routine to the user's return stack so that control can return to that
; routine in order to do the necessary IRET.

        emm_entry_return_IP	dw	?
	emm_entry_return_CS	dw	?
        emm_entry_GS            dw      ?
; DTS0514 *******************************************************************

	call_IP 	dw	?
	call_CS 	dw	?
	call_FLAGS	dw	?
	return_IP	dw	?
	return_CS	dw	?
AlterandCall_Frame	ends

MapAndCall_struc	struc
	call_offset	dw	?
	call_segment	dw	?
	new_map_size	db	?
	new_map_offset	dw	?
	new_map_segment dw	?
	old_map_size	db	?
	old_map_offset	dw	?
	old_map_segment dw	?
MapAndCall_struc	ends
even
;==============================================================================
;==
;== AlterPageMapAndCall:  This routine alters the page map to a new mapping,
;==			  calls an entry point that then returns to CEMM,
;==			  restores the original mapping and maps in another
;==			  page mapping before returning to the original caller.
;==			  The various mappings is handled by first saving the
;==			  current map in a tempory save area.  Then the 'old'
;==			  map that is to be applied after the call is applied
;==			  next.  This new mapping is saved on the calling
;==			  programs stack.  Next the original saved mapping is
;==			  restored and the 'new' mapping is overlaid on it.
;==			  The call can now be made.  Since the call tries to
;==			  perform a far return when done, a special return
;==			  address is pushed on his stack that returns it to a
;==			  special entry point in the R_CODE segment.  This
;==			  special return in 'emm.asm' then performs an Int 67h
;==			  with AL = FFh to get back here.  The saved page
;==			  mapping is now restored from the user's stack and
;==			  control is transfered back to the original calling
;==			  procedure.
;==
;== Entry: (Protected Mode)
;==	AL 	   = if 2, do the Get Stack Space call: otherwise
;==	AL 	   = window location format, 0-window index, 1-window segment
;==	DX 	   = handle index
;==	[BP] DS:SI = pointer to the map and call structure
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==	BX = extra space needed on the user's stack for function 2
;==                                                                   LC
;=============================================================================
APMCdispatch	dw	APMCReturn
		dw	APMCCall
		dw	APMCCall
		dw	APMCStackSpace
APMCcOpts	equ	(($ - APMCdispatch)/2)
align 16
AlterPageMapAndCall	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK
;
;  The subfunction number is checked.
;
	inc	al			; 0FFh=>0, 0=>1, 1=>2, 2=>3
	cmp	al,APMCcOpts		;Q: Valid function?
	jae	short APMCinvFunc	; N: error

	movzx	si,al
	add	si,si
	jmp	cs:APMCdispatch[si]

APMCinvFunc:
	mov	byte ptr [bp][reg_EAX+1],INVALID_SUBFUNCTION ; invalid
	jmp	short APMCexit
APMCinvHandle:
	mov	byte ptr [bp][reg_EAX+1],INVALID_HANDLE ; invalid handle
	jmp	short APMCexit
align 16
;
;  The special return opcode means the called routine is now returning to CEMM.
;
APMCreturn:
; DTS0514 *******************************************************************
; TAIS - Throw away the return address of EMM_rEntry and the saved GS 
; register because it exists farther down on the user's stack (beyond
; the saved window mapping information).  This return address is never
; used or needed.

        mov     ax, 6
; DTS0514 *******************************************************************

	call	GetModifyUsersStack

	cmp	word ptr es:[edi],offset R_CODE:AlterandCallReturn
	jne	short APMCinvFunc

	cmp	word ptr es:[edi][2],seg R_CODE
	jne	short APMCinvFunc
	mov	word ptr [bp][reg_EAX],OK SHL 8

;
;  The user's stack is loaded into ES:ESI.
;
	mov	ax,6			; add room for an IRET frame
	call	GetModifyUsersStack
	mov	esi,edi
;
;  The saved window mapping is now restored.
;
	call	RestoreWindowMapping
	mov	ax,[context_save_area_size]

	call	GetModifyUsersStack
	jmp	short APMCexit
align 16
;
;  The size needed is a word for each window, 1 word for the number of
;  windows stored, and 4 words for the call return and original CS:IP.
;
APMCStackSpace:
	mov	ax,[context_save_area_size]

; DTS0514 *******************************************************************
; TAIS - When reporting the needed stack space, we have to add an *extra*
; 6 bytes (on top of the 6 added to AlterAndCall_Frame) since the
; return address of EMM_rEntry and its saved GS register will be on the
; user's stack twice at one point in time.

	add	ax,size AlterandCall_Frame + 6
; DTS0514 *******************************************************************

	mov	word ptr [bp][reg_EBX],ax
	mov	byte ptr [bp][reg_EAX+1],OK

APMCexit:
	ret

APMCexitPopX:
	add	sp,[context_save_area_size]
APMCexitPop:
	add	sp,4
	ret
align 16
;
;  Alter Page Map and Call
;
APMCCall:
;
;  Validate handle
;
	cmp	dx,[total_handles]	;Q: Handle in range?
	jae	short APMCinvHandle	; N: error

	movzx	edx,dx			; access handle structure
	shl	dx,2
	add	edx,[hndl_tbl_ptr]	; (EDX) = pointer to handle structure
	.errnz	(SIZE HandleTable_struc-4)

	cmp	ds:[edx].base_PTE_index,FREE ;Q: Handle in use?
	je	short APMCinvHandle	     ; N: error

;
;  The user's segment:offset is changed into a 32 bit linear address. (ESI)
;
	movzx	esi,[bp.reg_DS]
	shl	esi,4
	movzx	eax,word ptr [bp.reg_ESI]
	add	esi,eax

;
;  The target address is saved temporarily in case it is lost by the map call.
;
	push	es:[esi].call_segment
	push	es:[esi].call_offset

;
;  If the old map is zero, then just save the current window mapping without
;  adding the old map in.
;
	cmp	es:[esi].old_map_size,0
	je	short APMC_save_current_plus_old_map

;
;  Save original mapping on protected mode stack
;
	sub	sp,[context_save_area_size]
	mov	edi,esp
	add	edi,[pSTACK]

	push	esi
	call	SaveWindowMapping
	pop	esi

;
;  The pointer to the user's buffer is swapped to EDI so that the pointer to
;  the old map can be placed into ESI.
;
	mov	edi,esi
	movzx	esi,es:[edi].old_map_segment
	shl	esi,4
	movzx	eax,word ptr es:[edi].old_map_offset
	add	esi,eax
;
;  CX is the number of entries to process. ES:ESI is a pointer to the windows
;  to map in.  A CY set means an error occured.
;
	movzx	cx,es:[edi].old_map_size

	push	edx
	push	edi
	call	MapInWindows
	pop	edi
	pop	edx
	jc	APMCexitPopX
;
;  Restore the user's buffer pointer to ESI.
;
	mov	esi,edi
;
;  The current mapping context is saved on the user's stack as pointed to by
;  ES:EDI. AX is the amount to adjust the stack pointer by.
;
APMC_save_current_plus_old_map:
	mov	ax,[context_save_area_size]
	neg	ax
	call	GetModifyUsersStack
	push	esi
	call	SaveWindowMapping
	pop	esi
;
;  If the old map is zero then skip straight to the loading of the new map.
;
	cmp	es:[esi].old_map_size,0
	je	short APMC_add_new_page_map

;
;  Restore original mapping from protected mode stack
;
 	mov	edi,esi
	mov	esi,esp
	add	esi,[pSTACK]

	push	edi
	call	RestoreWindowMapping
	pop	esi
	add	sp,[context_save_area_size]

APMC_add_new_page_map:
	mov	edi,esi
	movzx	esi,es:[edi].new_map_segment
	shl	esi,4
	movzx	eax,word ptr es:[edi].new_map_offset
	add	esi,eax
	movzx	cx,es:[edi].new_map_size

	push	edi
	call	MapInWindows
	pop	esi
;
;  The return code is checked to see if the call is done. If a failure
;  occured then no call.
;
	jnc	short APMC_continue_call
	mov	ax,[context_save_area_size]
	call	GetModifyUsersStack
	jmp	APMCexitPop
;
;  The user's stack is loaded into EDI.  AX=size to modify the stack pointer by.
;
APMC_continue_call:
	mov	ax,-(size AlterandCall_Frame)
	call	GetModifyUsersStack
;
;  The return address to the user's code is changed to the new jump address.
;
	pop	es:[edi].call_IP	; target offset
	pop	es:[edi].call_CS	; target segment
;
;  The original FLAGS from the calling procedure are given to the new
;  procedure to be called.
;
	mov	ebx,size AlterandCall_Frame
	add	bx,[context_save_area_size]

; DTS0514 *******************************************************************
; TAIS - Modified this section to be aware of the "new" stack layout and
; to "push" some extra words on the user's stack so that control returns
; to EMM_rEntry before going to the user's called program.

; Get a copy of the return address and saved GS register for EMM_rEntry 
; and "push" it on the user's stack.
	mov	ax, es:[ebx][edi]
	mov	es:[edi].emm_entry_return_IP, ax
	mov	ax, es:[ebx][edi][2]
	mov	es:[edi].emm_entry_return_CS, ax
	mov	ax, es:[ebx][edi][4]
	mov	es:[edi].emm_entry_GS, ax

; Get a copy of the user's flag register saved on the stack so that we
; can "push" it on again.  Then, when the IRET in EMM_rEntry is
; executed it will transfer control the user's program in the EMS
; page frame area and restore the user flags.
	mov	ax, es:[ebx][edi][10]
; DTS0514 *******************************************************************

	mov	es:[edi].call_FLAGS,ax

	mov	es:[edi].return_IP,offset R_CODE:AlterAndCallEntry
	mov	es:[edi].return_CS,seg R_CODE
	ret
AlterPageMapAndCall	endp

page
MoveLocation_struc	struc
	memory_type	db	?
	handle_id	dw	?
	start_offset	dw	?
	page_segment	dw	?
MoveLocation_struc	ends

MoveMemory_struc	struc
	region_length	dd	?
	start_source	db	7 dup(?)
	start_dest	db	7 dup(?)
MoveMemory_struc	ends

EXPANDED_MEMORY_FORMAT	=	1
SOURCE_IS_LOW		=	0
SOURCE_IS_HIGH		=	1
NO_OVERLAP		=	0
OVERLAP 		=	1

align 16
;******************************************************************************
; MoveExchangeMemoryRegion
;
; ENTRY
;	AL - subfunction number, 0 - Move, 1 - Exchange
;	DS:SI - pointer to user's move data structure
; EXIT
;	AH - return code
; DESCRIPTION
;	This routine either moves or exchanges 2 memory locations.  The
;	locations can be either conventional or expanded memory.  Conventional
;	memory is classified as anything under 1MB and no checks are made for
;	overlapping EMS windows.  It is assumed that the user knows what is
;	going to happen if a conventional address overlaps a used window.
;***********************************************************************
MoveExchangeMemoryRegion   proc    near

		; The subfunction number is checked to see if it is valid.
	cmp	al,2
	jb	short MXMR_form_buffer_address
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SUBFUNCTION
	jmp	short MXMR_return_code

		; The user's segment:offset value is changed into a 32 bit
		; linear address.  EBX has the address.  ES is zero based.
		; The upper 16 bits of EBX, ECX are being used so watch out.
MXMR_form_buffer_address:
	movzx	ebx,word ptr [bp.reg_DS]
	shl	ebx,4
	movzx	eax,word ptr [bp.reg_ESI]
	add	ebx,eax
	push	DATA32_GSEL
	pop	es

		; The length of the operation is loaded into ECX.
	mov	ecx,es:[ebx].region_length

		;and checked for max of 1MB
	cmp	ecx,100000h
	jbe	short MXMR_check_more
	mov	byte ptr [bp.reg_EAX + 1],INVALID_REGION
	jmp	short MXMR_return_code

MXMR_check_more:
	or	ecx,ecx
	jnz	short MXMR_load_source
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	short MXMR_return_code

		; The source's linear address is loaded into ESI.
MXMR_load_source:
	add	ebx,start_source
	call	FormLinearAddress
	jc	short MXMR_return_code
	mov	esi,eax

		; The destination's linear address is loaded into EDI.
	add	ebx,size MoveLocation_struc
	call	FormLinearAddress
	jc	short MXMR_return_code
	mov	edi,eax

		; The two pointers are now checked to see who is lower as
		; stored in AH and if overlap occurs as stored in AL.
	call	CheckForOverlap

		; The function code is now checked to see which one is done.
	cmp	byte ptr [bp.reg_EAX],0
	je	short MXMR_move_function

		; If the exchange is being done then no overlap is allowed.
	cmp	al,NO_OVERLAP
	je	short MXMR_perform_exchange
	mov	byte ptr [bp.reg_EAX + 1],INVALID_OVERLAP
	jmp	short MXMR_return_code
MXMR_perform_exchange:
	call	PerformDataExchange

	jmp	short MXMR_return_code

MXMR_move_function:
	call	PerformDataMove

MXMR_return_code:
	call	UnMapHandleSpace                                        ;LEO
	ret
MoveExchangeMemoryRegion   endp

page
align 16
;******************************************************************************
; FormLinearAddress
;
; ENTRY
;	ES:EBX - buffer pointer to the location's data
;	ECX - the size of the transfer
; EXIT
;	EAX - the 32 bit linear address
;	CY - set if an error occured
; DESCRIPTION
;	This routine takes the provided data and generates a 32 bit linear
;	address in EAX from either a segment:offset pair or a handle:page
;	pair.  The format is defined within a byte in the data structure.
;***********************************************************************
FormLinearAddress	proc	near

	push	edx
	push	esi
	PUSH	EDI

		; The type of memory is checked.
	cmp	es:[ebx].memory_type,EXPANDED_MEMORY_FORMAT
	je	short FLA_load_expanded_address
	jb	short FLA_conventional_memory
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SOURCE_TYPE
	stc
	jmp	FLA_return_code

		; If the address is conventional, then form a 32 bit
		; linear address and place it in EAX.
FLA_conventional_memory:
	movzx	eax,es:[ebx].page_segment
	shl	eax,4
	movzx	edx,es:[ebx].start_offset
	add	eax,edx

		; Verify that the address does not wrap at 1MB.
	add	eax,ecx
	cmp	eax,100000h
     ;;;jae	short FLA_1MB_wrap
	ja	short FLA_1MB_wrap	;doesn't wrap if src + len == 1 meg
	sub	eax,ecx
	clc
	jmp	FLA_return_code

FLA_1MB_wrap:
	mov	byte ptr [bp.reg_EAX + 1],WRAP_AT_1MB
	stc
	jmp	SHORT FLA_return_code

		; The handle is validated.  SI is the handle's index.
FLA_load_expanded_address:
	mov	dx,es:[ebx].handle_id
	; CY = !ValidateHandle(DX);
	call	ValidateHandle
	jc	short FLA_return_code
	movzx	esi,dx

		; The EMS page index is converted into a 4k page count and
		; checked to see if it is within range.
	movzx	eax,es:[ebx].page_segment
	test	ah,0C0h
	jnz	SHORT FLA_page_range_error
	shl	ax,2
	MOV	EDI, [hndl_tbl_ptr]
	CMP	AX, DS:[EDI+ESI*4].number_PTEs
	jb	short FLA_check_limit

		; If the page is out of range then set the error code.
FLA_page_range_error:
	mov	byte ptr [bp.reg_EAX + 1],LOG_PAGE_RANGE
	stc
	jmp	short FLA_return_code

		; The offset into the page is checked to see if it is valid.
FLA_check_limit:
	mov	dx,es:[ebx].start_offset
	cmp	dx,EMS_PAGE_SIZE
	jb	short FLA_check_length
	mov	byte ptr [bp.reg_EAX + 1],INVALID_OFFSET
	stc
	jmp	short FLA_return_code

		; The length of the request is added in to insure
		; that the request does not exceed the handle's memory.
FLA_check_length:
	add	edx,ecx
	add	edx,0FFFh
	shr	edx,12
	add	ax,dx
	CMP	AX, DS:[EDI+ESI*4].number_PTEs
	jbe	short FLA_form_address

	mov	byte ptr [bp.reg_EAX + 1],INSUFFICIENT_MEMORY
	stc
	jmp	short FLA_return_code

		; The PTE index is generated and then converted into a linear
		; 32 bit address.
FLA_form_address:
;
; Need to find an empty entry in PD and point it to corresponding PH.
; Then fix linear address to use it.
;
	sub	ax,dx
	ADD	AX, DS:[EDI+ESI*4].base_PTE_index
	sub	ax,FIRST_HANDLE_PTE ; offset from start of handle space ;LEO
	shl	eax,12
	movzx	edx,es:[ebx].start_offset
	add	eax,edx
	call	MapHandleSpace	    ; returns linear address		;LEO
	clc

FLA_return_code:
	POP	EDI
	pop	esi
	pop	edx
	ret
FormLinearAddress	endp

page
align 16
;******************************************************************************
; CheckForOverlap
;
; ENTRY
;	ECX - the size of the transfer
;	ESI - the source linear address
;	EDI - the destination linear address
; EXIT
;	AH - specifies if the source is high or low in memory
;	AL - specifies is overlap occured
; DESCRIPTION
;	This routine checks the two address to see if the data space defined
;	by either address with the length will overlay.  If overlay does occur
;	then the routine also specifies if the source address is starting lower
;	in memory then the destination or not.
;***********************************************************************
CheckForOverlap proc	near

		; The source and destination are compared to see who is
		; higher in memory and if there is any overlap.
	cmp	esi,edi
	ja	short CFO_source_is_high

		; AH is used for holding the state of who is lower.
		; If the source is lower then next check for overlap.
	mov	ah,SOURCE_IS_LOW
	add	esi,ecx
	cmp	esi,edi
	ja	short CFO_overlap_src_low

		; The state of overlap is stored in AL.
	sub	esi,ecx
	mov	al,NO_OVERLAP
	jmp	short CFO_return_code
CFO_overlap_src_low:
	sub	esi,ecx
	mov	al,OVERLAP
	jmp	short CFO_return_code

		; AH is used for holding the state of who is lower.
		; If the source is higher then next check for overlap.
CFO_source_is_high:
	mov	ah,SOURCE_IS_HIGH
	add	edi,ecx
	cmp	edi,esi
	ja	short CFO_overlap_src_high
	sub	edi,ecx
	mov	al,NO_OVERLAP
	jmp	short CFO_return_code

CFO_overlap_src_high:
	sub	edi,ecx
	mov	al,OVERLAP

CFO_return_code:
	ret
CheckForOverlap endp

page
align 16
;******************************************************************************
; PerformDataExchange
;
; ENTRY
;	ECX - the size of the transfer
;	ESI - the source linear address
;	EDI - the destination linear address
; EXIT
;	EAX,EBX - trashed
; DESCRIPTION
;	This routine exchanges the data pointed to by the two addresses.
;	This is done using dword moves at first and then finishing any
;	remainder off with byte sized exchanges.
;***********************************************************************
PerformDataExchange	proc	near

		; The data will be moved in whole dword pieces with any
		; remainder done in bytes.  BX holds the bytes to do.
	mov	bx,cx
	and	bx,3
	shr	ecx,2
	jecxz	short PDE_finish_remainder

		; The whole dword parts of the data is exchanged.
PDE_exchange_dwords:
	mov	eax,es:[edi]
	xchg	eax,es:[esi]
	mov	es:[edi],eax
	add	edi,4
	add	esi,4
	dec	ecx
	jnz	short PDE_exchange_dwords

PDE_finish_remainder:
	mov	cx,bx
	jcxz	short PDE_return_code

PDE_exchange_bytes:
	mov	al,es:[edi]
	xchg	al,es:[esi]
	mov	es:[edi],al
	inc	edi
	inc	esi
	loop	PDE_exchange_bytes

PDE_return_code:
	mov	byte ptr [bp.reg_EAX + 1],OK
	ret
PerformDataExchange	endp

page
align 16
;******************************************************************************
; PerformDataMove
;
; ENTRY
;	ECX - the size of the transfer
;	ESI - the source linear address
;	EDI - the destination linear address
; EXIT
;	EAX - the 32 bit linear address
;	EBX - trashed
;	CY - clear if an error occured
; DESCRIPTION
;	This routine moves the data from the source to the destination address.
;	If overlap occurs, then the routine will insure that the data will be
;	moved correctly by seeing who is lower in memory so that it knows which
;	direction to move from.  The data is moved in dword size chunks with
;	byte sized moves for any remainder.
;***********************************************************************
PerformDataMove proc	near

		; First overlap is checked for. If there is none then do
		; the default move direction with no overlap warning.
	cmp	al,OVERLAP
	je	short PDM_fixup_direction
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	short PDM_move_data

		; If there is overlap and the source is higher then dest.
		; then leave the move direction in the default.
		; The special overlap return code is set up here.
PDM_fixup_direction:
	mov	byte ptr [bp.reg_EAX + 1],SUCCESSFUL_OVERLAP
	cmp	ah,SOURCE_IS_LOW
	je	short PDM_reverse_direction
	jmp	short PDM_move_data

		; If there is overlap and the source is lower then destination
		; then change the direction and start at the end.
PDM_reverse_direction:
	std
	add	edi,ecx
	dec	edi
	add	esi,ecx
	dec	esi

		; The number of dwords to process is checked to see if it
		; is zero. If it is then the pointers are correct.
	mov	ebx,ecx
	shr	ebx,2
	cmp	ebx,0
	je	short PDM_move_data

		; If there are dwords to move then the pointers must be
		; backed up another 3 bytes so they are pointing at the
		; beginning of the dword to move.
	sub	esi,3
	sub	edi,3

		; The number of whole dwords to move is put in ECX while
		; BX gets the remainder number of bytes to move.
PDM_move_data:
	mov	bx,cx
	and	bx,3
	shr	ecx,2
	REP_MOVS_DWORD_USING_ES_ESI

		; Now the remaining bytes are moved.
	mov	cx,bx

		; M001 - Start
	cmp	ax, ( (SOURCE_IS_LOW SHL 8) + OVERLAP)
	jne	short PDM_pointers_OK
		; We have reversed direction. So the pointers must be
		; fixed up.
	add	esi, 3
	add	edi, 3
		; M001 - End

PDM_pointers_OK:
	REP_MOVS_BYTE_USING_ES_ESI

		; Clear the direction flag in case it has been set.
PDM_return_code:
	cld
	ret
PerformDataMove endp

page
align 16
;==============================================================================
;==
;== MapHandleSpace:  This routine finds a location in the page directory to
;==		     temporarily map the handle space for a memory transfers.
;==
;== Entry
;==	EAX = Offset in the handle space into an EMS logical page
;==	 DS = VDMD_GSEL  (_DATA)
;== Exit
;==	EAX = Created linear address for entry address
;==	DS:[CurrentHandleMap] = Current map of handle space.
;==                                                                   LC
;==============================================================================
MapHandleSpace	proc	near
	push	edx
	push	cx
	push	di
	push	es

	mov	edx,eax			; save handle space offset

	cmp	[CurrentHandleMap],0	;Q: Handle space mapped?
	jnz	short MHSlinear		; Y: get linear address
					; N: map handle space

	mov	di,PAGED_GSEL		; page directory/handle space
	mov	es,di                   ; and clear hi word of EDI

	mov	di,[LastHandleMap]	; location of last mapping
	add	di,4

;
; Search through the page directory entries for an empty range
;
MHSsearch:
	mov	cx,[NumberHandlePages]
	sub	di,4
	and	di,0FFCh		; mask to insure proper wrap
	mov	[CurrentHandleMap],di	; save location to be used

MHSnext:
	cmp	dword ptr es:[di],0	;Q: Is this being used?
	jnz	short MHSsearch		; Y: search for another range

	sub	di,4			;Q: Have we reached entry 0
	jz	short MHSsearch		; Y: search for another range
	loop	MHSnext

;
;  An empty range has been found
;
	add	di,4			; starting index
	mov	cx,[NumberHandlePages]  ; number of pages to be mapped
	mov	eax,[page_directory]	; starting physical address
	add	eax,FIRST_HANDLE_PTE shl 2; of handle space
	or	eax,P_AVAIL
;
;  Fill in the page directory entries
;
MHSfill:
	stosd
	add	eax,PAGE_SIZE
	loop	MHSfill
;
;  Adjust the linear address
;
MHSlinear:
	movzx	eax,[CurrentHandleMap]	; location of mapping
	mov	[LastHandleMap],ax	; save it for next time

	mov	cx,[NumberHandlePages]
	dec	cx
	shl	cx,2
	sub	ax,cx

	shl	eax,20			; 4M boundary for handle space
	add	eax,edx			; linear address

	pop	es
	pop	di
	pop	cx
	pop     edx
	ret
MapHandleSpace	endp

align 16
;==============================================================================
;==
;== UnMapHandleSpace:  This routine clears the current handle mapping in the
;==		       page directory.
;==
;== Entry
;==	DS:[CurrentHandleMap] = Current map of handle space.
;==
;== Exit
;==	DS:[CurrentHandleMap] = 0
;==                                                                   LC
;==============================================================================
UnMapHandleSpace proc	near
	push	eax
	push	cx
	push	di
	push	es

	xor	di,di			; clear current handle map
	xchg	di,[CurrentHandleMap]	; location of mapping in di

	or	di,di			;Q: Was there a mapping?
	jz	short UMHSexit          ; N: handle space not used

	mov	ax,PAGED_GSEL		; page directory/handle space
	mov	es,ax                   ; and clear hi word of EDI

	mov	cx,[NumberHandlePages]
	dec	cx
	shl	cx,2
	sub	di,cx
	and	di,0FFCh		; offset into page directory

;
;  Clear the page directory entries
;
	mov	cx,[NumberHandlePages]
	xor	eax,eax
UMHSclear:
	stosd
	jcxz	UMHSexit
	loop	UMHSclear

UMHSexit:
	pop	es
	pop	di
	pop	cx
	pop	eax
	ret
UnMapHandleSpace endp

page
align 16
;==============================================================================
;==
;== GetMappablePhysicalAddress:  This routine copies the mapping between the
;==				 window indexes and their segment locations
;==				 to the user's buffer.  All of the windows
;==				 are done.
;==
;== Entry
;==	AL    = subfunction number, 0-Get Array, 1-Get Size of Array
;==	ES:DI = the user's buffer for function 0, Get Address Array
;==
;== Exit
;==	[BP]:AH = return code
;==	[BP]:CX = number of windows in the array
;==                                                                    LC
;==============================================================================
GetMappablePhysicalAddress	proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK
;
;  The subfunction number is checked to see if it is valid.
;  If the function is 1 then just return the size only.
;
	cmp	al,1
	je	GMPAretSize
	ja	short GMPAinvFunc
;
;  Function 0 has been selected which returns the array.
;  The user's segment:offset value is changed into a 32 bit linear address.
;
	movzx	eax,word ptr [bp].reg_ES
	shl	eax,4
	movzx	edi,di
	add	edi,eax

	mov	dl,-1			; no EMS window token
	mov	cx,0FCh			; last possible EMS window
	mov	bx,[strtng_bs_wndw_PTE]	; first possible EMS window
	sub	cx,bx
	shr	cx,2			; number of possible EMS windows
GMPAloop:
	movzx	eax,EMSsegLoc[bx]	; get possible EMS window
	cmp	al,dl			;Q: Is it an EMS window?
	je	short GMPAnext		; N: next window
	shl	eax,16			; Y: physical window number in hi word
	mov	ah,bl			; segment in low word
	STOS_DWORD_PTR_ES_EDI		; place in user buffer
GMPAnext:
	add	bl,4			; next 16K address (possible EMS window)
	dec	cx			;Q: Any more EMS windows?
	jnz	short GMPAloop		; Y: next
;
;  The number of windows in the system is returned in CX.
;
GMPAretSize:
	mov	ax,[number_EMS_windows]
	mov	word ptr [bp].reg_ECX,ax

GMPAexit:
	ret

GMPAinvFunc:
	mov	byte ptr [bp][reg_EAX+1],INVALID_SUBFUNCTION
	jmp	short GMPAexit

GetMappablePhysicalAddress	endp

page
align 16
;******************************************************************************
; GetExpandedMemoryHWInfo
;
; ENTRY
;	AL - subfunction number, 0 - Get Array Info, 1 - Get Page Info
;	ES:DI - user's buffer for function 0, Get Hardware Info
; EXIT
;	AH - return code
;	BX - unallocated raw pages for function 1
;	DX - total raw pages for function 2
; DESCRIPTION
;	This routine returns some standard information about the features
;	supported by CEMM.  This routine is protected by the OS/E enable
;	function.
;***********************************************************************
GetExpandedMemoryHWInfo proc	near

		; The permission is checked to see if this function is enabled.
	bt	[OS_functions],OS_ENABLED_BIT
	jc	short GEMI_check_subfunction
	mov	byte ptr [bp.reg_EAX + 1],ACCESS_DENIED
	jmp	short GEMI_return_code

		; The subfunction number is verified.
GEMI_check_subfunction:
	cmp	al,1
	je	short GEMI_raw_page_info
	jb	short GEMI_load_information

		; If the subfunction is invalid then set the correct error code.
GEMI_invalid_subfunction:
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SUBFUNCTION
	jmp	short GEMI_return_code

		; Function 1 is being done.  Since the raw page size equals the
		; normal page size, this call is just passed to Function 3.
GEMI_raw_page_info:
	call	GetUnallocatedPageCount
	jmp	short GEMI_return_code

		; Function 0 is being done.
GEMI_load_information:

		; The user's segment:offset value is changed into a 32 bit
		; linear address.  EDI has the address.  ES is zero based.
	movzx	eax,word ptr [bp.reg_ES]
	shl	eax,4
	movzx	edi,di
	add	edi,eax
	push	DATA32_GSEL
	pop	es

		; The first info is the raw page size in paragraphs.
	mov	ax,400h
	mov	es:[edi],ax
	add	edi,2

		; Next the number of alternate register sets is stored.
	movZX	ax, BYTE PTR [total_register_sets]
	dec	ax
	mov	word ptr es:[edi],ax
	add	edi,2

		; The size of the context save area is stored.	The first byte
		; in the map is used for storing the number of elements in the
		; map.
	mov	ax,[context_save_area_size]
	mov	es:[edi],ax
	add	edi,2

		; The number of DMA register sets is stored.
	mov	ax,0h
	mov	es:[edi],ax
	add	edi,2

		; The DMA channel operation mode is stored.
	mov	ax,0h
	mov	es:[edi],ax
	add	edi,2

	mov	byte ptr [bp.reg_EAX + 1],OK

GEMI_return_code:
	ret
GetExpandedMemoryHWInfo endp

page
align 16
;******************************************************************************
; AllocateStandardRawPages
;
; ENTRY
;	AL - subfunction number, 0 - standard pages, 1 - raw pages
;	BX - number of pages to allocate
; EXIT
;	DX - handle index
; DESCRIPTION
;	This routine allocates either standard or raw pages to a new handle.
;	However, since CEMM's standard page size is the same as its raw page
;	size, these calls are identical.  This call is normally passed off to
;	the 3.2 version of AllocatePages except for the special case where
;	the number of pages is zero.  In this case then a handle is allocated
;	with no pages assigned to it.
;***********************************************************************
AllocateStandardRawPages	proc	near

		; The subfunction number is checked to see if it is valid.
	cmp	al,1
	ja	short ASRP_invalid_subfunction

		; The number of pages to be allocated is checked to see if
		; it is zero.  If it is then perform the following special code.
	cmp	bx,0
	ja	short ASRP_pass_call_thru

		; The handle table is now searched for an empty
		; handle structure that can be used.
ASRP_get_handle:
	mov	esi,1
	mov	cx,[total_handles]
	MOV	EDI, [hndl_tbl_ptr]
ASRP_find_handle:
	CMP	DS:[EDI+ESI*4].base_PTE_index, FREE
	je	short ASRP_got_handle
	inc	si
	loop	ASRP_find_handle

		; If this is reached then no free handles could be found.
	mov	byte ptr [bp.reg_EAX + 1],NO_MORE_HANDLES
	jmp	short ASRP_return_code

ASRP_got_handle:
	MOV	[EDI+ESI*4].base_PTE_index, 0
	MOV	[EDI+ESI*4].number_PTEs, 0
	mov	word ptr [bp.reg_EDX],si
	inc	gs:[handle_count]
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	short ASRP_return_code

		; If the number of requested pages is not zero then just
		; pass the call thru to the AllocatePages function.
ASRP_pass_call_thru:
	call	AllocatePages
	jmp	short ASRP_return_code

ASRP_invalid_subfunction:
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SUBFUNCTION

ASRP_return_code:
	ret
AllocateStandardRawPages	endp

page
align 16
;==============================================================================
;==
;== AlternateMapRegisterSet:  This code manipulates the alternate register
;==			      sets.  See the individual	routines for further
;==			      descriptions.
;==
;== Entry: (Protected Mode)
;==	AL = subfunction number, 0-Get, 1-Set, 2-Get Save Area Size
;==	     3-Allocate Set, 4-Deallocate Set, 5,6,7- DMA functions
;==
;== Exit:  (Protected Mode)
;==	AH = return code
;==                                                                    LC
;=============================================================================
AMRSdispatch	label	word
	dw	offset GetRegisterSet
	dw	offset SetRegisterSet
	dw	offset AMRS_get_save_area_size
	dw	offset AllocateAltRegisterSet
	dw	offset DeallocateAltRegisterSet
	dw	offset AMRS_allocate_DMA_set
	dw	offset AMRS_enable_DMA_functions
	dw	offset AMRS_other_DMA_functions
	dw	offset AMRS_other_DMA_functions
AMRScOpts	equ	($-AMRSdispatch)/2

AlternateMapRegisterSet proc	near
;
;  Assume success
;
	mov	byte ptr [bp][reg_EAX+1],OK
;
;  The OS is checked to see if this function is currently enabled
;
	bt	[OS_functions],OS_ENABLED_BIT
	jnc	short AMRSinvAccess
;
;  The subfunction number is checked.
;
	cmp	al,AMRScOpts
	jae	short AMRSinvFunc
;
;  The subfunction number is used as an index into the jump table.
;
   	movzx	si,al
	add	si,si
	jmp	cs:AMRSdispatch[si]
;
;  Size for save area
;
AMRS_get_save_area_size:
	mov	ax,[context_save_area_size]
	mov	word ptr [bp.reg_EDX],ax
	jmp	short AMRSexit
;
;  Since there are no alternate DMA register sets, return 0.
;
AMRS_allocate_DMA_set:
	mov	byte ptr [bp.reg_EBX],0
	jmp	short AMRSexit
;
;  For these functions if BL=0 & DL=0 then just return ok.
;  Otherwise an error message is needed.
;
AMRS_enable_DMA_functions:
	cmp	dl,0
	je	short AMRS_other_DMA_functions
	mov	byte ptr [bp][reg_EAX+1],NO_ALT_DMA_REG_SETS
	jmp	short AMRSexit

AMRS_other_DMA_functions:
	cmp	bl,0
	je	short AMRSexit
	mov	byte ptr [bp][reg_EAX+1],NO_ALT_DMA_REG_SETS
	jmp	short AMRSexit

AMRSexit:
	ret

AMRSinvAccess:
	mov	byte ptr [bp][reg_EAX+1],ACCESS_DENIED
	jmp	short AMRSexit
AMRSinvFunc:
	mov	byte ptr [bp][reg_EAX+1],INVALID_SUBFUNCTION
	jmp	short AMRSexit

AlternateMapRegisterSet endp


align 16
;==============================================================================
;==
;== GetRegisterSet:  This routine returns the current alternate register set
;==		     that has been selected.  If the current set is zero or
;==		     the dummy set, then the current window mapping is copied
;==		     into the buffer area at ES:EDI.
;==
;== Entry: (Protected Mode)
;==	BL 	   = register set index
;==	[BP] ES:DI = pointer to a map register context save area
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==                                                                    LC
;=============================================================================
GetRegisterSet	proc	near

	cmp	[current_register_set],0	;Q: Current AR 0?
	jne	short GRSgetSet			; N: don't worry about mapping
;
;
;
	mov	eax,[saved_pointer]
	mov	word ptr [bp.reg_EDI],ax
	movzx	edi,ax
	shr	eax,16
	mov	bx,word ptr [bp.stack_frame_EBP]
	mov	ss:[bx][VTFO].VMTF_ES,ax
	shl	eax,4
	add	edi,eax
	cmp	edi,0
	je	short GRSgetSet

	call	SaveWindowMapping

GRSgetSet:
	mov	al,[current_register_set]
	mov	byte ptr [bp][reg_EBX],al
	ret

GetRegisterSet	endp

align 16
;==============================================================================
;==
;== SetRegisterSet:  This routine switches to the new alternate register set.
;==		     If the set is zero or the dummy, then the current state
;==		     is not saved but the pointer provided is used to restore
;==		     the current mapping and the current register set becomes
;==		     zero.  If the set is not zero, then the current register
;==		     state is saved before restoring the state of the new
;==		     register set.
;==
;== Entry: (Protected Mode)
;==	BL 	   = the new alternate register set
;==	[BP] ES:DI = a pointer to a context save area
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==                                                                    LC
;=============================================================================
SetRegisterSet	proc	near
;
;  First check it to see if it is within the valid range.
;
	cmp	bl,[total_register_sets]
	jae	short SARSunsSet
;
;  Check to see if the register set specified is active.
;
	movzx	si,bl
	shl	si,3
	.errnz size RegisterSet_struc-8

	cmp	[register_set][si].active,TRUE
	jne	short SARSinvSet
;
;  Mappings for current register set are saved. ES:EDI points to the save area.
;
	or	bl,bl			;Q: AR set 0?
	jnz	short SARsetRegSet	; N: set reg set
;
;  Get context area address
;
	movzx	eax,word ptr [bp.reg_ES]
	mov	word ptr [saved_pointer+2],ax
	shl	eax,4
	movzx	esi,word ptr [bp.reg_EDI]
	mov	word ptr [saved_pointer],si
	add	esi,eax

	or	esi,esi			;Q: ES:DI pointer passed?
	jz	short SARsetRegSet	; N: set register set

	mov	eax,es:[esi]		; read header/CRC

	inc	ax			;Q: Valid restore context buffer?
	jnz	short SARSinvBuff	; N: error
	shr	eax,16

	cmp	ax,'C'+'O'+'M'+'P'+'A'+'Q';Q: Check CRC, valid?
	jne	short SARSinvBuff	  ; N: corrupted source

;
;  At this point ESI points to user buffer. Copy buffer to local stack.
;
	movzx	ecx,[context_save_area_size]
	sub	sp,cx
	mov	edi,esp
	add	edi,[pSTACK]
	mov	edx,edi

	mov	ah,cl
	shr	cx,2
	REP_MOVS_DWORD_USING_ES_ESI
	mov	cl,ah
	and	cl,3
	jcxz	short SARScont
	REP_MOVS_BYTE_USING_ES_ESI
SARScont:
	call	ActivateRegSet		; AL = AR to activate

	mov	esi,edx
	call	RestoreWindowMapping
	add	sp,[context_save_area_size]
	ret

SARSunsSet:
	mov	byte ptr [bp][reg_EAX+1],UNSUPPORTED_REGISTER_SET
	jmp	SARSexit
SARSinvSet:
	mov	byte ptr [bp][reg_EAX+1],INVALID_REGISTER_SET
	jmp	SARSexit
SARSinvBuff:
	mov	byte ptr [bp][reg_EAX+1],WINDOW_INDEX_RANGE
	jmp	SARSexit

SARsetRegSet:
	call	ActivateRegSet
SARSexit:
	ret
SetRegisterSet	endp

align 16
;==============================================================================
;==
;== AllocateAltRegisterSet:  This routine finds an unused alternate register
;==			     set and allocates it to the calling program.
;==			     The current window mapping is saved in its context
;==			     save area.
;==
;== Entry: (Protected Mode)
;==	none
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==	[BP]:BL = allocated alternate register set
;==                                                                     LC
;=============================================================================
AllocateAltRegisterSet	proc	near
;
;  The register_set[] array is searched for a non-active register set.
;  BX is the offset of the register set.
;
	movzx	ecx,byte ptr [total_register_sets]
	xor	bx,bx
	mov	al,TRUE
AARS_check_for_active:
	xchg	[register_set][bx].active,al
	cmp	al,FALSE			;Q: Is this AR set active?
	je	short AARSallocSet		; N: use it!
	add	bx,size RegisterSet_struc	; Y: try the next one
	dec	cx
	jnz	short AARS_check_for_active
;
;  If this is reached then no free register sets were found.
;
	mov	byte ptr [bp][reg_EAX+1],NO_FREE_REGISTER_SETS
	jmp	short AARSexit

AARSallocSet:
;
;  We need to copy the current PT 0 into the 4k block pointed to by this
;  alternate register set. We shall also copy the the A20state specifed in
;  the Current_state flag into the AR set
;
	mov	di,gs:[Current_State]
	and	di,fState_A20Ena
	mov	[register_set][bx].a20_state,di
	mov	edi,[register_set][bx].page_table_ptr

	mov	esi,[page_directory]
	mov	esi,es:[esi]		; current PT0 address

	and	si,not 0fffh		; clear bits 0-11

	mov	cx,1024			; move 4K bytes (high word of ECX cleared above)
	REP_MOVS_DWORD_USING_ES_ESI
;
; The register set index is determined from the offset in BX.
;
	shr	bx,3
	.errnz size RegisterSet_struc-8
	mov	byte ptr [bp][reg_EBX],bl

AARSexit:
	ret
AllocateAltRegisterSet	endp

align 16
;==============================================================================
;==
;== DeallocateAltRegisterSet:  This routine deallocates the specified register
;==			       set.  If the dummy register set is specified
;==			       then nothing is done.  The current register set
;==			       can not be deallocated.
;==
;== Entry: (Protected Mode)
;==	BL = alternate register set
;==
;== Exit:  (Protected Mode)
;==	[BP]:AH = return code
;==                                                                     LC
;=============================================================================
DeallocateAltRegisterSet	proc	near
;
;  OS may not deallocate AR 0, but an error will not be returned
;
	cmp	bl,0			;Q: Deallocate AR 0?
	je	short DARSexit		; Y: done

;
;  The current register set cannot be deallocated so check it.
;
	cmp	bl,[current_register_set]
	je	short DARSinvRegSet
;
;  Check the range of the register set index.
;
	cmp	bl,[total_register_sets]
	jae	short DARSbadRegSet
;
;  Check to see if the specified set is currently allocated.
;
	xor	bh,bh
	shl	bx,3
	.errnz size RegisterSet_struc-8

	mov	al,FALSE
	xchg	al,[register_set][bx].active

	cmp	al,FALSE		;Q: Allocated?
	je	short DARSinvRegSet	; N: error

DARSexit:
	ret

DARSbadRegSet:
	mov	byte ptr [bp][reg_EAX+1],NO_ALT_REG_SETS ; assume no ARs
	cmp	[total_register_sets],1		;Q: Any alternate register sets supported?
	je	short DARSexit			; N: assumption correct
DARSinvRegSet:
	mov	byte ptr [bp][reg_EAX+1],INVALID_REGISTER_SET
	jmp	short DARSexit

DeallocateAltRegisterSet	endp


page
;******************************************************************************
; PrepareForWarmBoot
;
; ENTRY
;	none
; EXIT
;	AH - OK
; DESCRIPTION
;	This routine returns success as CEMM does not support non-volatile
;	handles.
;***********************************************************************
PrepareForWarmBoot	proc	near

		; Set the success return code and return.
	mov	byte ptr [bp.reg_EAX + 1],OK
	ret
PrepareForWarmBoot	endp

page
;******************************************************************************
; EnableDisableOSFunctions
;
; ENTRY
;	AL - subfunction number, 0 - Enable, 1 - Disable, 2 - Return Key
;	BX,CX - validation key
; EXIT
;	BX,CX - validation key
; DESCRIPTION
;	This routine manages the OS functions for enabling and disabling some
;	of the LIM 4.0 special functions.
;***********************************************************************
EnableDisableOSFunctions	proc	near

		; The key's value is placed into EDX for ease.
	mov	dx,cx
	shl	edx,16
	mov	dx,bx

		; The subfunction number is checked.
	cmp	al,2
	ja	short EDF_invalid_subfunction
	je	short EDF_return_access_key

		; Check to see if the key is already allocated.
	bts	[OS_functions],OS_KEY_OUT_BIT
	jc	short EDF_check_key

		; If the key is not out then return it in BX,CX.
	push	DATA32_GSEL
	pop	es
	mov	edx,dword ptr es:[046Ch]	;seed key with timer ticks
	mov	[OS_key],edx			;save the key
	mov	word ptr [bp.reg_EBX],dx
	shr	edx,16
	mov	word ptr [bp.reg_ECX],dx
	jmp	short EDF_parse_subfunction

		; Verify the provided key's value.
EDF_check_key:
	cmp	edx,[OS_key]
	jne	short EDF_invalid_access

		; The enable/disable functions are now done.
		; The return code is also set up.
EDF_parse_subfunction:
	mov	byte ptr [bp.reg_EAX + 1],OK
	cmp	al,1
	je	short EDF_disable_subfunction
	bts	[OS_functions],OS_ENABLED_BIT
	jmp	short EDF_return_code
EDF_disable_subfunction:
	btr	[OS_functions],OS_ENABLED_BIT
	jmp	short EDF_return_code

		; Function 2 is being done, Return Access Key.
EDF_return_access_key:

		; The key is validated.  The key is in EDX.
	cmp	edx,[OS_key]
	jne	short EDF_invalid_access


	btr	[OS_functions],OS_KEY_OUT_BIT ; The key allocated bit is reset.
	bts	[OS_functions],OS_ENABLED_BIT ; the functions re-enabled
	mov	byte ptr [bp.reg_EAX + 1],OK
	jmp	short EDF_return_code

EDF_invalid_access:
	mov	byte ptr [bp.reg_EAX + 1],ACCESS_DENIED
	jmp	short EDF_return_code

EDF_invalid_subfunction:
	mov	byte ptr [bp.reg_EAX + 1],INVALID_SUBFUNCTION

EDF_return_code:
	ret
EnableDisableOSFunctions	endp

_TEXT	ends
END

