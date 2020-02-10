PAGE 58,132
;******************************************************************************
TITLE INSTANCE.ASM -
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1986-1991
;
;   Title:	INSTSWAP.ASM -
;
;   Version:	2.00
;
;   Date:	18-Sep-1986
;
;   Author:	RRH, RAL, AAR
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   18-SEP-1986 RRH Original
;   12-Jul-1988 RAL Changed from VMSWITCH.ASM to INSTSWAP.ASM & optimized
;   13-Jul-1988 RAL Updated to new data structures / Page must be owned
;   02-Aug-1988 AAR Added _MMGR_Toggle_HMA call for A20 wrap management
;   25-Aug-1988 RAL Fixed high memory instance faults
;   09-Mar-1989 RAL Non-dirty inst pages not copied out, New PFAULT.ASM
;   24-Jul-1990 AC  Adapted for LoadHi VXD
;
;==============================================================================
	.386p

;******************************************************************************
;		    P U B L I C   D E C L A R A T I O N S
;******************************************************************************

	PUBLIC	Swap_LoadHi_Instance_Page


;******************************************************************************
;			  I N C L U D E   F I L E S
;******************************************************************************

	INCLUDE VMM.inc
	INCLUDE Debug.Inc
	INCLUDE instance.Inc
	INCLUDE Opttest.Inc


;******************************************************************************
;		  E X T E R N A L   D E C L A R A T I O N S
;******************************************************************************

VxD_DATA_SEG

Extrn	ActualAddInstanceItem:dword		;original _AddInstanceItem service
Extrn	LoadHi_Instance_Map_Table_Ptr_Actual:dword
Extrn	LoadHi_Instance_Map_Table_Size_Actual:byte
Extrn	LoadHi_Instance_SnapLaddr:dword
Extrn	CB_LoadHi_Inst_Buf_ptr:dword		;offset in CB for Instance data buffer
Extrn	CB_LoadHi_Inst_Hand:dword		;handle of above buffer

; define contants to BIAS the MAP tables to start at page 0

LoadHi_Instance_Map_Table_Ptr equ LoadHi_Instance_Map_Table_Ptr_Actual - 0A0H * 4
LoadHi_Instance_Map_Table_Size equ LoadHi_Instance_Map_Table_Size_Actual - 0A0H


	public	LoadHi_Inst_Page_Owner_Actual

; define the actual array to start from page 0A0H onwards, but define an equate
; to back bias the start relative to page 0 so that we can index into the 
; array directly.

LoadHi_Inst_Page_Owner_Actual     dd	(MAX_INST_PAGES- 0A0H) dup (0)
LoadHi_Inst_Page_Owner equ LoadHi_Inst_Page_Owner_Actual - 0A0H * 4

;----------------------------------------------------------------------------;
VxD_DATA_ENDS
;----------------------------------------------------------------------------;
VxD_CODE_SEG

	Extrn	Get_Mapped_Page_Num:near

;******************************************************************************
;
;   Swap_LoadHi_Instance_Page
;
;   DESCRIPTION:
;
;
;   WARNING:
;	THIS PROCEDURE IS CALLED BY VMTRAP TO SWAP PAGE 0 FOR FAST INTERRUPT
;	REFLECTION!  The EAX and EDX parameters passed by the page fault
;	code will not be valid when called from VmTrap.
;
;   ENTRY:
;	EBX = Current VM handle
;	EAX = Page index of fault
;
;   EXIT:
;
;   USES:
;	EAX, EBX, ECX, EDX, ESI, EDI, Flags
;
;==============================================================================

BeginProc Swap_LoadHi_Instance_Page, High_Freq, PUBLIC

	enter	4,0

PageTableBits	EQU	DWORD PTR [EBP-4]

	mov	ecx,eax
	cmp	ecx,MAX_INST_PAGES
	jae	SIP_Fatal

comment ~
IFDEF DEBUG
	cmp	LoadHi_Inst_Page_Owner[ecx*4],0
	jne	short SIPD10
	debug_out "Swap_LoadHi_Instance_Page, 0 in LoadHi_Inst_Page_Owner for page #ecx"
SIPD10:
	testmem [ebx.CB_MMGR_Flags],CB_MMGR_Resumed
	jnz	short SIP_Not_Suspended
	testmem [ebx.CB_VM_Status], VMStat_Creating
	jnz	SHORT SIP_Not_Suspended
	Debug_Out "ERROR:  LoadHi_Instance fault on suspended VM #EBX"
SIP_Not_Suspended:
ENDIF
end comment ~

	mov	eax, ebx
	xchg	eax, LoadHi_Inst_Page_Owner[ecx*4]	; Get old owner / Set new owner

    ;
    ; eax is OLD owners Control Block
    ; ebx is NEW owners Control Block
    ; ecx is page number
    ;

    ;
    ; map the page NOT_PRESENT for the old owner and PRESENT for the 
    ; new owner. Before doing this get the page table entry information
    ; for the old owner.
    ;

    	mov	edx,ecx				;get the page number
	shl	edx,12				;convert to a linear address
	add	edx,[eax.CB_High_Linear]	;where currently mapped
	shr	edx,12				;convert back to a page number
	push	eax
	push	ebx
	push	ecx
	push	edx
	lea	esi,PageTableBits
	VMMcall	_CopyPageTable,<edx,1,esi,0>
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	mov	esi,NOT P_PRES
	push	eax
	push	ebx
	push	ecx
	push	edx
    	VMMcall	_ModifyPageBits,<eax,ecx,1,esi,0,PG_HOOKED,0>
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax

	push	eax
	push	ebx
	push	ecx
	push	edx
	mov	eax,ecx				;get the UMB page number
	call	Get_Mapped_Page_Num		;edx has mapped page number
	VMMcall	_PhysIntoV86,<edx,ebx,ecx,1,0>
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax

comment ~
	mov	esi, [eax.CB_PgTable_Ptr]				; ESI -> Old owner's page table
	mov	edi, [ebx.CB_PgTable_Ptr]				; EDI -> New owner's page table
	cmp	ecx, WRAP_MAX_V86_PAGE					; Wrap area page?
	jae	SIP_Wrap1						; Yes, special stuff
	mov	dl, BYTE PTR [esi][ecx*4]				; Get page info for dirty test
	and	BYTE PTR [esi][ecx*4],NOT P_AVAIL			;Old owner can't access
	or	BYTE PTR [edi][ecx*4], P_AVAIL				; New owner can
	cmp	ecx,NOWRAP_MAX_V86_PAGE - WRAP_MAX_V86_PAGE		; Q: Page possibly mirrored above 1 meg?
	jae	SHORT SIP_Owners_Swapped				;    N: Ownership set
	testmem [eax.CB_MMGR_Flags],CB_MMGR_NoWrap			; Old guy has wrap?
	jnz	short SIPOLDNoWrp					; N:
	or	dl, BYTE PTR [esi+(WRAP_MAX_V86_PAGE*4)][ecx*4] 	; + Dirty info for high page
	and	BYTE PTR [esi+(WRAP_MAX_V86_PAGE*4)][ecx*4], NOT P_AVAIL; Old owner can't access
SIPOLDNoWrp:
	testmem [ebx.CB_MMGR_Flags],CB_MMGR_NoWrap			; New guy has wrap?
	jnz	short SIP_Owners_Swapped				; N:
	or	BYTE PTR [edi+(WRAP_MAX_V86_PAGE*4)][ecx*4], P_AVAIL	; New owner can
	and	BYTE PTR [edi+(WRAP_MAX_V86_PAGE*4)][ecx*4], NOT P_DIRTY; New owner NOT dirty
SIP_Owners_Swapped:

end comment ~

	mov	edi, cr3			; Reload CR3 to reset the TLB
	mov	cr3, edi
	mov	edi, PageTableBits		; Move dirty info to EDI

    ;
    ; eax is OLD owners Control Block
    ; ebx is NEW owners Control Block
    ; ecx is page number
    ; edi original page bits for OLD owner
    ;

	push	ecx

	mov	edx, LoadHi_Instance_Map_Table_Ptr[ecx*4]
	movzx	ecx, LoadHi_Instance_Map_Table_Size[ecx]
	or	ecx,ecx
IFDEF DEBUG
	jnz	short CIOD10
	debug_out "LoadHi_Instance fault on page with 0 IMT_LoadHi_Inst_Map_Size"
CIOD10:
ENDIF
	jz	short CIODone

;
;   If the page was not dirty then no need to save old owner's inst buffer.
;

	testreg edi, P_DIRTY
;	jz	SHORT CILoop

;
;   Copy old LoadHi_Instance Owner's data out
;
	push	ecx
	push	edx
COLoop:
	push	ecx

	mov	edi, CB_LoadHi_Inst_Buf_ptr
	mov	edi, [eax][edi]		; Point into instance buffer
	add	edi, [edx.IMap_Inst_Buf_Offset]
	mov	esi, [edx.IMap_VM_Address]
	add	esi, [ebx.CB_High_Linear]
	movzx	ecx, [edx.IMap_Lead_Byte_Count]
	rep	movsb

    ; NOTE here that we know ECX == 0

	or	cx, [edx.IMap_Field_Length]
	jz	short CONext
	shr	ecx,2
	rep	movsd

    ; NOTE here that we know ECX == 0

	or	cx, [edx.IMap_Field_Length]
	and	ecx,011b		; Mask to byte part
	rep	movsb

CONext:
	pop	ecx
	add	edx, size LoadHi_Instance_Map_struc
	loop	COLoop

	pop	edx
	pop	ecx

;
;   Copy new LoadHi_Instance Owner's data in
;
CILoop:
	push	ecx

	mov	esi, CB_LoadHi_Inst_Buf_ptr
	mov	esi, [ebx][esi]		; Point into instance buffer
	add	esi, [edx.IMap_Inst_Buf_Offset]
	mov	edi, [edx.IMap_VM_Address]
	add	edi, [ebx.CB_High_Linear]
	movzx	ecx, [edx.IMap_Lead_Byte_Count]
	rep	movsb

    ; NOTE here that we know ECX == 0

	or	cx, [edx.IMap_Field_Length]
	jz	short CINext
	shr	ecx,2
	rep	movsd

    ; NOTE here that we know ECX == 0

	or	cx, [edx.IMap_Field_Length]
	and	ecx,011b		; Mask to byte part
	rep	movsb
CINext:
	pop	ecx
	add	edx, size LoadHi_Instance_Map_struc
	loop	CILoop

CIODone:
	pop	ecx
	mov	edi, cr3
	mov	cr3, edi

	leave
	ret

SIP_Not_LoadHi_Inst_Page:
	Debug_Out "ERROR:  LoadHi_Instance fault on non-instance page"
	VMMjmp	Crash_Cur_VM

SIP_Fatal:
	debug_out "Got an instance page fault on #ecx >= MAX_INST_PAGES in a VM"
	VMMjmp	Crash_Cur_VM

EndProc Swap_LoadHi_Instance_Page

;******************************************************************************
;
;   LoadHi_Instance_VMDestroy/LoadHi_Instance_VMSuspend - Make any instance pages owned by
;					       EBX owned by SYS_VM_Handle
;
;   ENTRY:
;	EBX is VM Handle of VM being destroyed
;
;   EXIT:
;	None
;
;   USES:
;	EAX, ECX, EDX, ESI, EDI, Flags
;
;==============================================================================

BeginProc LoadHi_Instance_VMDestroy, PUBLIC
	push	1
	jmp	short VMSuspDestCommon

EndProc LoadHi_Instance_VMDestroy

BeginProc LoadHi_Instance_VMSuspend, PUBLIC

	push	0

VMSuspDestCommon:
IFDEF DEBUG
	cmp	dword ptr [esp],0	; Only invalid to DESTROY current VM
	jz	short IVD_Handle_OK
;
;   NOTE: You can not use Assert_VM_Handle ebx here since the VM handle
;	  has already been removed from the list of valid handles.
;
	;;;;;;;;Assert_VM_Handle ebx
	push	ebx
	push	eax
	mov	eax,ebx
	VMMcall	Get_Cur_VM_Handle
	cmp	eax,ebx
	pop	eax
	pop	ebx
	jne	SHORT IVD_Handle_OK
	Debug_Out "FATAL ERROR:  LoadHi_Instance_VMDestroy called with EBX = Cur_VM_Handle"
	Fatal_Error
IVD_Handle_OK:
ENDIF

	mov	ecx, MAX_INST_PAGES		; # of pages to test
IVD_Loop:
	cmp	ecx,MIN_LOADHI_INST_PAGE	; this is where we stop
	jbe	SHORT IVD_Exit			; done.
	dec	ecx				; otherwise dec page counter
;
; NOTE On non-instance pages where there is a 0 in LoadHi_Inst_Page_Owner array
;	we will ALWAYS take the JNE.
;
	cmp	LoadHi_Inst_Page_Owner[ecx*4], ebx; Q: Owned by this VM?
	jne	SHORT IVD_Loop			;    N: GOOD!  Ignore this page
	push	ebx				;    Y: Force it to be loaded
	push	ecx				;	for the system VM
	VMMcall	Get_Sys_VM_Handle		;Sys_VM_Handle in EBX
	push	eax				
	mov	eax,ecx
	call	Swap_LoadHi_Instance_Page
	pop	eax
	pop	ecx
	pop	ebx
	jmp	SHORT IVD_Loop

IVD_Exit:
	clc					; success
	pop	eax				; Discard susp/destroy flag
	ret

EndProc LoadHi_Instance_VMSuspend
;******************************************************************************
;
;   Load_LoadHi_Instance_Pages - Make sure all instance pages are present and owned
;			  by Cur_VM_Handle.
;
;   ENTRY:
;	None
;
;   EXIT:
;
;   USES:
;	EAX, EBX, ECX, EDX, ESI, EDI, Flags
;
;==============================================================================
comment ~
BeginProc Load_LoadHi_Instance_Pages, PUBLIC

	VMMcall	Get_Cur_VM_Handle 		;EBX has Cur_VM_Handle
	mov	ecx, MAX_INST_PAGES
LIP_Loop:
	dec	ecx
	jz	SHORT LIP_Exit
	cmp	LoadHi_Inst_Page_Owner[ecx*4], ebx	; Owned by Cur VM already?
	je	SHORT LIP_Loop			; Yes, skip it
	cmp	LoadHi_Inst_Page_Owner[ecx*4], 0	; LoadHi_Instance page?
	je	SHORT LIP_Loop			; No, skip it
	push	ebx
	push	ecx
	push	eax				
	mov	eax,ecx
	call	Swap_LoadHi_Instance_Page
	pop	eax
	pop	ecx
	pop	ebx
	jmp	SHORT LIP_Loop
LIP_Exit:
	ret

EndProc Load_LoadHi_Instance_Pages
end comment ~
;******************************************************************************
;
;   LoadHi_Instance_WIN386_Exit - Set up instance data for WIN386 exit. Swap in
;			   contents of instance snap shot buffer.
;
;   ENTRY:
;	None
;
;   EXIT:
;	None
;
;   USES:
;	ALL but EBP
;
;==============================================================================

comment ~
BeginProc LoadHi_Instance_WIN386_Exit, PUBLIC

	mov	ecx, MAX_INST_PAGES	; Do this many pages
CIPLp:
	cld
	push	ecx

	dec	ecx			; Count is 1-MAX_INST_PAGES,
					;  pages are 0-(MAX_INST_PAGES - 1)
	mov	edx, LoadHi_Instance_Map_Table_Ptr[ecx*4]
	movzx	ecx, LoadHi_Instance_Map_Table_Size[ecx]
	jecxz	CICont
CILp:
	push	ecx

	mov	esi, [LoadHi_Instance_SnapLaddr]
	add	esi, [edx.IMap_Inst_Buf_Offset]
    ;
    ; For HMA instance pages IMap_VM_Address is in the Phys Linear region so
    ;	it doesn't matter if the HMA is currently ON or OFF in the current
    ;	VM.
    ;
	mov	edi, [edx.IMap_VM_Address]
	movzx	ecx, [edx.IMap_Lead_Byte_Count]
	rep	movsb
    ; NOTE here that we know ECX == 0
	or	cx, [edx.IMap_Field_Length]
	jz	short COutNext
	shr	ecx,2
	rep	movsd
    ; NOTE here that we know ECX == 0
	or	cx, [edx.IMap_Field_Length]
	and	ecx,011b		; Mask to byte part
	rep	movsb
COutNext:
	pop	ecx
	add	edx, size LoadHi_Instance_Map_struc
	loop	CILp

CICont:
	pop	ecx
	loop	CIPLp
	ret

EndProc LoadHi_Instance_WIN386_Exit
end comment ~
;----------------------------------------------------------------------------;
VxD_CODE_ENDS
;----------------------------------------------------------------------------;
VxD_ICODE_SEG

VxD_ICODE_ENDS
;----------------------------------------------------------------------------;
	END
