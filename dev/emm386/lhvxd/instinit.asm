	TITLE	Win386 LoadHi_Instance Data Manager Initialization Routines
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;
;   Module:   INSTINIT.ASM - LoadHi_Instance Data manager init
;
;   Version:  0.00
;
;   Date:     7/7/88
;
;   Author:   ARR   -  created for VMM/MMGR
;
;   Change log:	 AC   7/24/90  Adapted for LoadHi VXD.
;	
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   04/21/88  0.00
;
.lfcond 				; list false conditionals
.386p

	include VMM.inc
	include opttest.inc
	include debug.inc
	include instance.inc

VxD_DATA_SEG

Extrn	LoadHi_Inst_Page_Owner_Actual:dword	;Array of instance page owners
Extrn   FirstUMBPage:dword			;first UMB page (lowest addr)
Extrn	ActualAddInstanceItem:dword		;original _AddInstanceItem service
Extrn	CB_LoadHi_Inst_Buf_ptr:dword		;offset in CB for Instance data buffer
Extrn	CB_LoadHi_Inst_Hand:dword		;handle of above buffer

; define an equate 'LoadHi_Inst_Page_Owner' so that we can access the owner
; array as if it was defines from page numbers starting at 0 rather than 0A0H.

LoadHi_Inst_Page_Owner equ LoadHi_Inst_Page_Owner_Actual - 0A0h * 4


	public	LoadHi_Instance_Map_Table_Size_Actual
	public	LoadHi_Inst_VM_Buf_Size
	public	LoadHi_Instance_MapLaddr
	public	LoadHi_Instance_Map_Table_Ptr_Actual
	public	LoadHi_Instance_MapHand
	public	LoadHi_Instance_SnapLaddr
	public	LoadHi_Instance_SnapHand

ALIGN 4

LoadHi_Inst_VM_Buf_Size	  dd	0	; Size of VM instance data buffer in pages

LoadHi_Instance_MapLaddr  dd	0	; Linear address of instance description
					;  buffer
LoadHi_Instance_MapHand	  dd	0	; hMem of instance description buffer

LoadHi_Instance_SnapLaddr dd	0	; Linear address of instance snap shot
					;  buffer
LoadHi_Instance_SnapHand  dd	0	; hMem of instance snap shot buffer


;
; These arrays are used to manage instance data page faults. It is indexed by page
;   numbers from 0-(MAX_INST_PAGES-1).
;
;   For pages which are not instance pages both array entries will be 0.
;
;   LoadHi_Instance_Map_Table_Ptr contains pointer to the first LoadHi_Instance_Map_Struc
;	for the page.
;
;   The actusl tables will have slots only from page 0A0H onwards, but we will
;   use an equate to offset the start to page 0 so we can index into the arrays
;   directly.


LoadHi_Instance_Map_Table_Ptr_Actual	 dd (MAX_INST_PAGES-0A0h) dup (0)
LoadHi_Instance_Map_Table_Ptr equ LoadHi_Instance_Map_Table_Ptr_Actual - 0A0h * 4


;
;   LoadHi_Instance_Map_Table_Size specifies the number of LoadHi_Instance_Map_Strucs
;	for the page
;

LoadHi_Instance_Map_Table_Size_Actual 	 db (MAX_INST_PAGES-0A0H) dup (0)
LoadHi_Instance_Map_Table_Size equ LoadHi_Instance_Map_Table_Size_Actual - 0A0h

IFDEF DEBUG

public	LoadHi_InstDataListHd

ENDIF

LoadHi_Inst_VM_Buf_End 	   dd	0	; Size of VM instance data buffer in bytes
LoadHi_InstDataListHd	   dd  -1	; Head of INIT instance list
LoadHi_Instance_MapCurrOff dd	0	; Current end of Instance description buffer
LoadHi_Instance_MapLimit   dd	0	; Limit of Instance description buffer
LoadHi_InstanceAnalysisDone dd  0	; set to -1 after doing analysis


VxD_DATA_ENDS
;----------------------------------------------------------------------------;

VxD_IDATA_SEG


VxD_IDATA_ENDS
;----------------------------------------------------------------------------;
VxD_CODE_SEG

	Extrn	LoadHi_Instance_VMDestroy:near
	Extrn	LoadHi_Instance_VMSuspend:near
	Extrn	LoadHi_Map_Pages:near


;
; LoadHi_Instance_Create_VM - Create a new VMs instance data buffer
;
; ENTRY:
;	EBX -> New VM CB
;
;	NOTE THAT WE CANNOT DO AN Assert_VM_Handle on this VM handle
;	    yet
;
; EXIT:
;	Carry Clear
;	    OK
;	Carry Set
;	    Error (insufficient memory)
; USES:
;	All but EBX,ESI,EDI,EBP
;

BeginProc LoadHi_Instance_Create_VM,PUBLIC

	push	esi
	push	edi

    	call	LoadHi_Map_Pages	;map the pages in

    ; if loadhi instance data is present, exit with success.


	mov	esi,[LoadHi_InstDataListHd]
	inc	esi
	jz	SHORT ICVOK


    ;
    ; Allocate the instance buffer.  Note that the buffer will be locked when
    ; the VM is initially resumed.
    ;

	xor	eax,eax
	VMMCall _PageAllocate,<[LoadHi_Inst_VM_Buf_Size],PG_VM,ebx,eax,eax,eax,eax,0>
	or	eax,eax
	jz	short ICVFail
	mov	edi,CB_LoadHi_Inst_Buf_Ptr
	mov	[ebx][edi],edx
	mov	edi,CB_LoadHi_Inst_Hand
	mov	[ebx][edi],eax

    ;
    ; Initialize the instance buffer from the instance snap shot
    ;

	mov	edi,edx
	mov	ecx,[LoadHi_Inst_VM_Buf_Size]

;;;;	    shl     ecx,12		    ; * 4096 bytes per page
;;;;	    shr     ecx,2		    ; / 4 for size in DWORDS

    ; Combine above instructions

	shl	ecx,10
	mov	esi,[LoadHi_Instance_SnapLaddr]
	cld
	rep	movsd

     ;
     ; Now we want to reset the P_PRES bit of all the pages that are in 
     ; include any LoadHi_Instance data.
     ;
	
     	mov	ecx,MAX_INST_PAGES - 0A0H	;possible number of pages
	mov	esi,OFFSET32 LoadHi_Instance_Map_Table_Size
	mov	edx,MIN_LOADHI_INST_PAGE	;array starts at this page

VMMakeNotPresentLoop:

	cmp	byte ptr [esi][edx],0		;num inst items in page
	jz	SHORT VMNoInstItemInThisPage	;not instance page
	mov	eax,NOT P_PRES
	push	edx
	push	ecx
	VMMcall	_ModifyPageBits,<ebx,edx,1,eax,0,PG_HOOKED,0>
	pop	ecx
	pop	edx

VMNoInstItemInThisPage:

       inc	dx
       loop	VMMakeNotPresentLoop

ICVOK:

	clc
ICVDone:
	pop	edi
	pop	esi
	ret

ICVFail:
	stc
	jmp	short ICVDone

EndProc LoadHi_Instance_Create_VM
;******************************************************************************
;									     
;   LoadHi_Instance_Resume_VM						
;										
;   Lock the per VM instance data buffer

;   ENTRY:
;	EBX is VM Handle of VM being destroyed
;
;   EXIT:
;	None
;
;   USES:
;	EAX, Flags
;*****************************************************************************
BeginProc LoadHi_Instance_Resume_VM, PUBLIC

	push	esi
	push	edi

    ;
    ; Lock instance buffer
    ;

	mov	edi,CB_LoadHi_Inst_Hand
	mov	esi,[ebx][edi]
	or	esi,esi			;any instance data buffer allocated?
	jz	SHORT RV_NoInstanceData	;no.
	VMMCall _PageGetSizeAddr,<esi,0>
	or	eax,eax
IFDEF DEBUG
	jnz	short MRVM30
	debug_out "_PageGetSizeAddr failed on INST handle _LoadHi_Resume_VM"
MRVM30:
ENDIF
	jz	SHORT IVR_Fails
	VMMCall _PageLock,<esi,eax,0,0>
	or	eax,eax
	jz	SHORT IVR_Fails

RV_NoInstanceData:

	clc				;success
	jmp	SHORT IVR_Ret

IVR_Fails:

	trace_out "Couldn't lock Instance buffer _MMGR_Resume_VM"
	stc

IVR_Ret:

	pop	edi
	pop	esi	
	ret

EndProc LoadHi_Instance_Resume_VM
;----------------------------------------------------------------------------;
; LoadHi_Suspend_VM:							     ;
;									     ;
; Reassigns instance page ownership to sys vm for all pages owned by this    ;
; instance and also unlocks the pages containing the VM's copy of the        ;
; instace areas.							     ;
;									     ;
; ENTRY:								     ;
;	EBX -> VM CB							     ;
; EXIT:									     ;
;	None								     ;
; USES:									     ;
;	All but EBX,ESI,EDI,EBP						     ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Instance_Suspend_VM,PUBLIC

    ;
    ; reassign owner ship
    ;

    	push	esi
	push	edi
	call	LoadHi_Instance_VMSuspend


; at this point unlock the instance pages

	mov	edi,CB_LoadHi_Inst_Hand
	mov	esi,[ebx][edi]
	or	esi,esi			;any instance data ?
	jz	SHORT IVD_Exit		;no.
	VMMCall _PageGetSizeAddr,<esi,0>
	or	eax,eax
IFDEF DEBUG
	jnz	short MSVM30
	debug_out "_PageGetSizeAddr failed on INST handle _MMGR_Suspend_VM"
MSVM30:
ENDIF
	jz	SHORT IVD_Fail
	VMMCall _PageUnLock,<esi,eax,0,PageMarkPageOut>
	or	eax,eax
	jnz	SHORT IVD_Exit

IVD_Fail:

	trace_out "Couldn't unlock Instance buffer _MMGR_Suspend_VM"
	stc
	jmp	SHORT IVD_Ret

IVD_Exit:
	clc					; success
IVD_Ret:
	pop	edi
	pop	esi
	ret

EndProc LoadHi_Instance_Suspend_VM
;----------------------------------------------------------------------------;
;
; LoadHi_Instance_Destroy_VM - Destroy VMs instance data buffer
;
; ENTRY:
;	EBX -> VM CB
; EXIT:
;	None
; USES:
;	All but EBX,ESI,EDI,EBP
;----------------------------------------------------------------------------;
BeginProc LoadHi_Instance_Destroy_VM,PUBLIC

    ;
    ; Remove VM from instance owner list
    ;

    	push	esi
	push	edi
	call	LoadHi_Instance_VMDestroy

    ;
    ; Free the instance buffer
    ;
	xor	eax,eax
	mov	edi,CB_LoadHi_Inst_Hand
	xchg	eax,[ebx][edi]
	mov	edi,CB_LoadHi_Inst_Buf_ptr
	mov	[ebx][edi],0FFFFFFFFh    ; Addr that will page fault

	or	eax,eax			;was this allocated at all ?
	jz	SHORT @f		;no.
	VMMCall _PageFree,<eax,0>
@@:

	pop	edi
	pop	esi
	ret

EndProc LoadHi_Instance_Destroy_VM
;----------------------------------------------------------------------------;
; LoadHi_Take_Instance_SnapShot:					     ;
;									     ;
; This routine is called at Sys_VM_Init	and creates a snap shot of the 	     ;
; instance buffer.							     ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Take_Instance_SnapShot,PUBLIC


    ; if there is no loadhi instance data to be instanced than get out.

	mov	esi,[LoadHi_InstDataListHd]
	inc	esi
	jz	LHTISS_OK


    ;
    ; Allocate the LoadHi_Instance Snap Shot buffer.
    ;
	xor	esi,esi
    ;
    ; NOTE THAT THIS BUFFER SHOULD BE FIXED. If we crash, we restore the state
    ;	of instance data to the state saved in the snap shot buffer. We do not
    ;	want to get page faults while we're doing this in LoadHi_Instance_WIN386_Exit.
    ;
	VMMCall _PageAllocate,<[LoadHi_Inst_VM_Buf_Size],PG_SYS,esi,esi,esi,esi,esi,PageFixed>

	or	eax,eax
	jz	LHTISS_Fatal2
	mov	[LoadHi_Instance_SnapLaddr],edx
	mov	[LoadHi_Instance_SnapHand],eax
    ;
    ; Init the snap shot from current low memory of SYS_VM_Handle
    ;
	mov	ecx,MAX_INST_PAGES	; one beyond last page number
COPLp:
	cld
	push	ecx

	dec	ecx			; Count is 1-MAX_INST_PAGES,
					;  pages are 0-(MAX_INST_PAGES - 1)
	mov	edx, LoadHi_Instance_Map_Table_Ptr[ecx*4]
	movzx	ecx, LoadHi_Instance_Map_Table_Size[ecx]
	jecxz	COCont
COLp:
	push	ecx

	mov	edi, [LoadHi_Instance_SnapLaddr]
	add	edi, [edx.IMap_Inst_Buf_Offset]
    ;
    ; For HMA instance pages IMap_VM_Address is in the Phys Linear region so
    ;	it doesn't matter if the HMA is currently ON or OFF in the SYS VM
    ;
	mov	esi, [edx.IMap_VM_Address]
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
	loop	 COLp

COCont:
	pop	ecx
	dec	ecx
	cmp	ecx,0A0H		 ;loop goes from 100h to A1H
	ja	SHORT COPLp		 ;dec ECX at top of loop makes it go
					 ;from FFh to A0H

    ;
    ; Allocate SYS_VM_Handle's instance buffer
    ;
    	VMMcall	Get_Sys_VM_Handle		; EBX has SYS_VM_Hanlde
	xor	eax,eax

	VMMCall _PageAllocate,<[LoadHi_Inst_VM_Buf_Size],PG_VM,ebx,eax,eax,eax,eax,PageLocked>

	or	eax,eax
	jz	SHORT LHTISS_Fatal3
	mov	esi,CB_LoadHi_Inst_Buf_ptr
	add	esi,ebx
	mov	[esi],edx
	mov	esi,CB_LoadHi_Inst_Hand
	add	esi,ebx
	mov	[esi],eax
    ;
    ; Copy instance snap shot into SYS_VM_Handle's instance buffer
    ;
	mov	esi,[LoadHi_Instance_SnapLaddr]
	mov	edi,edx
	mov	ecx,[LoadHi_Inst_VM_Buf_End]
	add	ecx,3
	shr	ecx,2
	cld
	rep	movsd

LHTISS_OK:

	xor	eax,eax
	dec	eax
LHTISS_Done:
	ret

LHTISS_Fatal2:
	debug_out "Allocation failure LoadHi_Inst snap _LoadHi_InstanceInitComplete"
IFDEF DEBUG
	jmp	short LHTISS_FXX
ENDIF

LHTISS_Fatal3:
	debug_out "Allocation failure VM1 LoadHi_Inst _LoadHi_InstanceInitComplete"

LHTISS_FXX:

	FATAL_ERROR



EndProc LoadHi_Take_Instance_SnapShot

IFDEF	DEBUG
;----------------------------------------------------------------------------;
; LoadHi_Debug_Query:							     ;
;									     ;
; Displays debug information about the data that is being instanced.	     ;
;----------------------------------------------------------------------------;

BeginProc	LoadHi_Debug_Query,PUBLIC

	mov	edi,[LoadHi_InstDataListHd]
	inc	edi			;is there a list
	jz	LHDQ_None		;no.
	xor	ebx,ebx
	Trace_Out "  Start     Length"
	Trace_Out " "


	mov	ecx,MAX_INST_PAGES	; last page number

LHDQ_NextPage: 

	cld
	push	ecx
	dec	ecx			; Count is 1-MAX_INST_PAGES,
					;  pages are 0-(MAX_INST_PAGES - 1)
	mov	edx, LoadHi_Instance_Map_Table_Ptr[ecx*4]
	movzx	ecx, LoadHi_Instance_Map_Table_Size[ecx]
	jecxz	LHDQ_TryNextPage

LHDQ_NextItemInPage:

	push	ecx

	mov	esi, [edx.IMap_VM_Address]
	movzx	ecx, [edx.IMap_Lead_Byte_Count]
	movzx	eax, [edx.IMap_Field_Length]
	add	eax,ecx

	cmp	ebx,1111b
	jne	SHORT @f
	VMMcall In_Debug_Chr
	jz	SHORT LHDQ_Exit
	Trace_Out " "
	Trace_Out " "
	Trace_Out "  Start     Length"
	Trace_Out " "
	xor	ebx,ebx
@@:
	Trace_Out "#ESI   #EAX"
	inc	ebx

	pop	ecx
	add	edx, size LoadHi_Instance_Map_struc
	loop	LHDQ_NextItemInPage

LHDQ_TryNextPage:

	pop	ecx
	dec	ecx
	cmp	ecx,0A0H
	ja	LHDQ_NextPage
	jmp	SHORT LHDQ_Exit

LHDQ_None:
	Trace_Out "No Instance data in UMBs"
LHDQ_Exit:
	ret

EndProc LoadHi_Debug_Query

ENDIF	;DEBUG
;----------------------------------------------------------------------------;
VxD_CODE_ENDS
;----------------------------------------------------------------------------;
VxD_ICODE_SEG


IFDEF DEBUG

public	AllocateLoadHi_InstanceMapStruc

ENDIF

;----------------------------------------------------------------------------;
;
; LoadHi_Add_Instance_Item
;
;  Add LoadHi_Instance item to instance list
;
;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Add_Instance_Item,PUBLIC


	enter	0,0

AIILoadHi_InstPTR equ	dword ptr [ebp+8]	; Pointer to instance structure
AIIflags   equ	dword ptr [ebp+12]		; Flags

	push	esi
	push	edi
	push	ebx

IFDEF DEBUG
	cmp	AIIflags,0
	jz	short AII10
	debug_out "Non-Zero flags _AddLoadHi_InstanceItem"
AII10:
ENDIF

	mov	edx,AIILoadHi_InstPTR

; If the linear address is below the linear address of the first UMB page
; then chain off the next guy.

	mov	eax,[edx.InstLinAddr]
	shr	eax,12				;make it a page number
	cmp	eax,[FirstUMBPage]		;is it in UMB area
	jae	SHORT AIIDataInUMBArea		;yes.

	pop	ebx
	pop	edi
	pop	esi
	leave
	jmp	[ActualAddInstanceItem]	;chain on

AIIDataInUMBArea:

;
; Check the type field
;
	mov	eax,[edx.InstType]
	cmp	eax,INDOS_Field
	jz	short AIITypeOK
	cmp	eax,ALWAYS_Field
	jz	short AIITypeOK
	debug_out "_AddLoadHi_InstanceItem bad InstType #eax in #edx"
	mov	[edx.InstType],ALWAYS_Field	; Set unknown type to always
AIITypeOK:
IFDEF DEBUG
	xor	eax,eax
	cmp	[edx.InstLinkF],eax
	jnz	short AII12
	cmp	[edx.InstLinkB],eax
	jz	short AII13
AII12:
	debug_out "_AddLoadHi_InstanceItem Links not 0 #edx"
AII13:
ENDIF

    ;
    ; Instance items cannot live above or = page MAX_INST_PAGES
    ;
	mov	eax,[edx.InstLinAddr]
	cmp	eax,MAX_INST_PAGES SHL 12
	jae	AIIFail1

    ; If the Instance item is below the page that we will manage, we will
    ; do nothing with this.

    	cmp	eax,MIN_LOADHI_INST_PAGE SHL 12
	jb	AIIChainON

    ; check to see if the end of the Instance area goes past what we can manage

	add	eax,[edx.InstSize]
	cmp	eax,MAX_INST_PAGES SHL 12
	ja	AIIFail1

	mov	ebx,0FFFFFFFFh

    ;
    ; Sort this item onto instance list
    ;

	mov	esi,[LoadHi_InstDataListHd]
	cmp	esi,ebx
	jz	short AIINewHd			; Fist entry
	mov	eax,[edx.InstLinAddr]
	cmp	eax,[esi.InstLinAddr]
	jbe	short AIINewHd
AIIPutLp:
	mov	ecx,[esi.InstLinkF]
	cmp	ecx,ebx
	jz	short AIINewTail
	mov	esi,ecx
	cmp	eax,[esi.InstLinAddr]
	ja	short AIIPutLp
	mov	[edx.InstLinkF],esi
	mov	ecx,edx
	xchg	ecx,[esi.InstLinkB]
IFDEF DEBUG
	cmp	ecx,0FFFFFFFFh
	jnz	short AII20
	debug_out "Missed head detection _AddLoadHi_InstanceItem"
AII20:
ENDIF
	mov	[ecx.InstLinkF],edx
	mov	[edx.InstLinkB],ecx
	jmp	short AIIOK

AIINewTail:
	mov	[edx.InstLinkB],esi
	mov	[edx.InstLinkF],ebx
	mov	[esi.InstLinkF],edx
	jmp	short AIIOK

AIINewHd:
	mov	[LoadHi_InstDataListHd],edx
	mov	[edx.InstLinkB],ebx
	mov	[edx.InstLinkF],esi
	inc	esi
	jz	short AIIOK
	dec	esi
	mov	[esi.InstLinkB],edx
AIIOK:

; if the Inatance data analysys has already been done, we should redo the 
; analysis at this point.

	cmp	[LoadHi_InstanceAnalysisDone],0 ;is it still to be done ?
	jz	SHORT AIIOK1			 ;yes

; we are going to call 'LoadHi_InstanceInitComplete' again, we must initialize
; all tables that the first call to the routine had changed and also free 
; up temporary pages that it had allocated.


; initialize the LoadHi_Instance_Map_Table_Ptr_Actual & _Size_Actual tables

	mov	edi,OFFSET32 LoadHi_Instance_Map_Table_Ptr_Actual
	xor	eax,eax
	mov	ecx,MAX_INST_PAGES - 0A0H
	rep	stosd
	mov	ecx,MAX_INST_PAGES - 0A0H
	mov	edi,OFFSET32 LoadHi_Instance_Map_Table_Size_Actual
	rep	stosb

; free the instance description buffer pages.

	mov	eax, [LoadHi_Instance_MapHand]
	VMMcall	_PageFree,<eax,0>

; now to the instance buffer analysis all over again.

	call	Loadhi_InstanceInitComplete


AIIOK1:

	xor	eax,eax
	dec	eax
AIIDone:
	clc				;we have taken care of this call
AIIRet:
	pop	ebx
	pop	edi
	pop	esi
	leave
	ret

AIIChainON:
	stc				;must chain this call on
	jmp	SHORT AIIRet

AIIFail1:
	debug_out "_AddLoadHi_InstanceItem LoadHi_Instance above MAX_INST_PAGES not supported #edx"
AIIFail:
	xor	eax,eax
	jmp	short AIIDone

EndProc LoadHi_Add_Instance_Item
;----------------------------------------------------------------------------;
;
; LoadHi_InstanceInitComplete - Complete initialization of CB and instance info
;
;     Finish LoadHi_Instance data init. Data list is complete.
;     Build and Set instance info in VM1 CB including LoadHi_Instance data buffer
;     CONTROL BLOCK NOW AT FINAL SIZE
;     Set owner of all instance pages to SYS_VM_Handle
;     Build and Set LoadHi_Instance snap shot buffer
;     Initialize VM1 state to final instance form
;     GLOBAL VM AREA NOW SET UP In FINAL form and First_VM_Page is now set
;     FREE the instance structures allocated by _Allocate_Global_VM_Data_Area
;----------------------------------------------------------------------------;
BeginProc LoadHi_InstanceInitComplete,PUBLIC

	enter	12,0

IICFlags 	equ  dword ptr [ebp-4]
IICLinAddr	equ  dword ptr [ebp-8]
IICSize		equ  dword ptr [ebp-12]

IICContRec	equ	00000000000000000000000000000001b
IICContRecBit	equ	0
IICNewTab	equ	00000000000000000000000000000010b
IICNewTabBit	equ	1

	mov	IICFlags,IICNewTab		; Init frame var

;
; First make a pass over the instance list coalescing adjacent blocks
;
	mov	esi,[LoadHi_InstDataListHd]
	inc	esi
IFDEF DEBUG
	jnz	short IIC10
	trace_out "_LoadHi_InstanceInitComplete no instance list"
IIC10:
ENDIF
	jz	IICOK
	dec	esi
IICCoalLp:
	mov	edi,[esi.InstLinkF]
	inc	edi
	jz	DEBFAR IICCoalDn
	dec	edi
	mov	eax,[esi.InstType]
	cmp	eax,[edi.InstType]	; Must be same type to coalesce
	jnz	short IICNext
	mov	eax,[esi.InstLinAddr]	; eax is start of esi
	mov	ebx,eax
	add	ebx,[esi.InstSize]	; ebx is end of esi
	mov	ecx,[edi.InstLinAddr] 	; ecx is start of edi
	mov	edx,ecx
	add	edx,[edi.InstSize]	; edx is end of edi
IFDEF DEBUG
	cmp	ebx,eax
	ja	short IIC20
	debug_out "_LoadHi_InstanceInitComplete entry is 0 size #esi"
IIC20:
	cmp	edx,ecx
	ja	short IIC30
	debug_out "_LoadHi_InstanceInitComplete entry is 0 size #edi"
IIC30:
	cmp	eax,ecx
	jbe	short IIC40
	debug_out "_LoadHi_InstanceInitComplete entries not sorted #esi > #edi"
IIC40:
ENDIF
	cmp	ebx,ecx 		; Exactly adjacent, or overlap?
	je	short IICAdjacent	; Exactly adjacent
	ja	short IICOverLap	; Overlap
IICNext:
	mov	esi,edi
	jmp	IICCoalLp

IICOverLap:
IFDEF DEBUG
	push	eax
	push	ecx
	mov	eax,[esi.InstLinAddr]
	mov	ecx,[esi.InstSize]
	trace_out "Odd Case, instance overlap #esi @#EAX len #ECX"
	mov	eax,[edi.InstLinAddr]
	mov	ecx,[edi.InstSize]
	trace_out "                           #edi @#EAX len #ECX"
	pop	ecx
	pop	eax
ENDIF
	cmp	eax,ecx
	je	short IICMaxSize	; Start at same address, take Max size
	cmp	ebx,edx
	jae	short IICNextUnlink	; esi completely surrounds edi just
					;   eliminate edi
    ;
    ; new end of combined block at edx
    ;
	sub	edx,eax 		; edx is new size
	mov	[esi.InstSize],edx	; set new larger size
	jmp	short IICNextUnlink	; eliminate edi

IICMaxSize:
	mov	eax,[edi.InstSize]
	cmp	eax,[esi.InstSize]
	jbe	short IICNextUnlink	; esi completely surrounds edi just
					;   eliminate edi
	mov	[esi.InstSize],eax	; edi is bigger, replace esi size
	jmp	short IICNextUnlink	; eliminate edi

IICAdjacent:
	mov	eax,[edi.InstSize]
	add	[esi.InstSize],eax
IICNextUnlink:
	mov	eax,[edi.InstLinkF]
	cmp	eax,0FFFFFFFFh
	jz	short IICNNxt
	mov	[eax.InstLinkB],esi
IICNNxt:
	mov	[esi.InstLinkF],eax
	jmp	IICCoalLp

IICCoalDn:
    ;
    ; Allocate The first page of the LoadHi_Instance description buffer.
    ;
	xor	eax,eax
	VMMCall _PageAllocate,<1,PG_SYS,eax,eax,eax,eax,eax,<(PageFixed + PageZeroInit)>>
	or	eax,eax
	jz	IICFatal
	mov	[LoadHi_Instance_MapLimit],4096
	add	[LoadHi_Instance_MapLimit],edx
	mov	[LoadHi_Instance_MapCurrOff],edx
	mov	[LoadHi_Instance_MapLaddr],edx
	mov	[LoadHi_Instance_MapHand],eax
    ;
    ; Scan the instance list setting up instance data
    ;
	mov	esi,[LoadHi_InstDataListHd]
	mov	edi,[esi.InstLinAddr]
	shr	edi,12			; Prev page = Current page
	inc	esi			; Counter next instruction
IICBuildLp:
	dec	esi
	mov	ecx,[esi.InstLinAddr]
	mov	IICLinAddr,ecx
	mov	ecx,[esi.InstSize]
	mov	IICSize,ecx
IICBuildLp2:
	mov	ecx,IICLinAddr
	mov	ebx,ecx
	add	ebx,IICSize
	dec	ebx
	shr	ebx,12			; Page of last byte of item
	shr	ecx,12			; Page of first byte of item
	cmp	ecx,edi 		; New instance in same page as previos?
	jz	short IICContPg 	; Yes
    ;
    ; Starting new page in LoadHi_Instance_Map_Table arrays
    ;
IFDEF DEBUG
	cmp	ecx,MAX_INST_PAGES
	jb	short IICD15
	debug_out "IIC new Page #ecx out of range"
IICD15:
	cmp	LoadHi_Instance_Map_Table_Size[ecx],0
	jz	short IICD20
	debug_out "IIC Starting new Page #ecx, table struct not empty"
IICD20:
ENDIF
	or	IICFlags,IICNewTab
	mov	edi,ecx 		; Prev page = Current page
IICContPg:
	cmp	ecx,ebx 		; Crosses page bound?
	mov	ecx,IICLinAddr		; Assume it doesn't
	mov	ebx,IICSize
	je	short IICInPg		; NO
	mov	ebx,ecx
	shr	ebx,12
	inc	ebx
	shl	ebx,12
	mov	IICLinAddr,ebx		; Rest of item starts here
	sub	ebx,ecx 		; Size to page bound
	sub	IICSize,ebx
IFDEF DEBUG
	ja	short IICD30
	debug_out "IIC adjusted inst rec to 0 or neg size #esi"
IICD30:
ENDIF
	or	IICFlags,IICContRec
IICInPg:
	call	AllocateLoadHi_InstanceMapStruc
	mov	[edx.IMap_Flags],0
	mov	[edx.IMap_VM_Address],ecx
	shr	ecx,12

NotWrpLoadHi_Inst:
IFDEF DEBUG
	cmp	LoadHi_Instance_Map_Table_Size[ecx],0FFh
	jne	short IICD35
	debug_out "IIC overflowing size count page #ecx"
IICD35:
ENDIF
	inc	LoadHi_Instance_Map_Table_Size[ecx]
	btr	IICFlags,IICNewTabBit	; Set pointer?
	jnc	short IICNoPtr		; No
	mov	LoadHi_Instance_Map_Table_Ptr[ecx*4],eax
IICNoPtr:
	cmp	ebx,4			; Less than a dword?
	jb	SHORT ICCAllByte  	; Yes, do all with byte move
	testmem [edx.IMap_VM_Address],0011b	; Dword aligned in VM?
	jz	short ICCAllDWord	; Yes no lead stuff

	mov	eax,[edx.IMap_VM_Address]
	and	eax,0000011b
	sub	eax,4
	neg	eax			; Need to go this far to get to DWORD
					;  Align in VM
	mov	ecx,ebx
	sub	ecx,eax
	cmp	ecx,4			; Only worth doing if get at least
					;   one DWORD out of it.
	jb	short ICCAllByte	; Forget it, do all with byte move
	mov	[edx.IMap_Lead_Byte_Count],al ; To get to Dword align
	sub	ebx,eax 		; What's left
	mov	[edx.IMap_Field_Length],bx
	mov	ecx,[LoadHi_Inst_VM_Buf_End]
	mov	ebx,ecx
	and	ebx,0000011b
	sub	ebx,4
	neg	ebx			; Need to go this far to get to DWORD
					;  in LoadHi_Inst buf
	cmp	ebx,eax
	je	short IICSmAlign	; What luck
	ja	short IICSmAdjust	; Just need move inst buf offset
					;   up a little
	add	ecx,ebx 		; ecx now dword aligned
	add	ecx,4			; up to next dword
	sub	ecx,eax 		; and down to same align as VM address
	jmp	short IICSmAlign

IICSmAdjust:
	sub	ebx,eax 		; Move inst buf offset by this much
	add	ecx,ebx
IICSmAlign:
	mov	[edx.IMap_Inst_Buf_Offset],ecx
	movzx	eax,[edx.IMap_Field_Length]
	add	ecx,eax
	movzx	eax,[edx.IMap_Lead_Byte_Count]
	add	ecx,eax
	xor	ebx,ebx
	jmp	short ICCDnStEFl

ICCAllDWord:
	mov	ecx,[LoadHi_Inst_VM_Buf_End]
	add	ecx,3			; Go to dword align in inst buf
	and	ecx,11111111111111111111111111111100b
	mov	[edx.IMap_Inst_Buf_Offset],ecx
	mov	[edx.IMap_Field_Length],bx
	add	ecx,ebx
	xor	ebx,ebx
	mov	[edx.IMap_Lead_Byte_Count],bl
ICCDnStEFl:
	mov	[LoadHi_Inst_VM_Buf_End],ecx
	jmp	short ICCDnStFl

ICCAllByte:
	mov	[edx.IMap_Lead_Byte_Count],bl
	mov	ecx,[LoadHi_Inst_VM_Buf_End]
	mov	[edx.IMap_Inst_Buf_Offset],ecx
	add	[LoadHi_Inst_VM_Buf_End],ebx
	xor	ebx,ebx
	mov	[edx.IMap_Field_Length],bx
ICCDnStFl:
	btr	IICFlags,IICContRecBit
	jc	IICBuildLp2
	mov	esi,[esi.InstLinkF]
	inc	esi
	jnz	IICBuildLp
IICListDone:
    ;
    ; Set LoadHi_Inst_VM_Buf_Size from LoadHi_Inst_VM_Buf_End
    ;
	mov	eax,[LoadHi_Inst_VM_Buf_End]
	add	eax,0FFFh
	shr	eax,12
IFDEF DEBUG
	or	eax,eax
	jnz	short IICD10
	debug_out "Computed LoadHi_Inst_VM_Buf_Size of 0 _LoadHi_InstanceInitComplete"
IICD10:
ENDIF
	mov	[LoadHi_Inst_VM_Buf_Size],eax
    ;
    ; The instance map description buffer is now in its final linear address
    ;	location (in LoadHi_Instance_MapLaddr), we now need to set all of the
    ;	entries in the LoadHi_Instance_Map_Table_Ptr array by re-locating them.
    ;

	mov	edi,[LoadHi_Instance_MapLaddr]
	mov	esi,OFFSET32 LoadHi_Instance_Map_Table_Ptr + 0A0H*4
	mov	edx,OFFSET32 LoadHi_Instance_Map_Table_Size + 0A0H
	mov	ecx,MAX_INST_PAGES - 0A0H
	xor	eax,eax
IICRelocLp:
	cmp	byte ptr [edx],al		; Entry here?
	jz	short IICRelocSkp		; No, skip it
	add	dword ptr [esi],edi		; ReLocate
IICRelocSkp:
	add	esi,4
	inc	edx
	loop	IICRelocLp


    ; Set the instance owner of all instance pages to SYS_VM_Handle
    ;	Non-instance pages have owner 0.
    ;
    	VMMcall	Get_Sys_VM_Handle	; EBX = Sys_VM_Handle
	mov	eax, ebx		; EAX = Cur owner of Inst pages
	xor	ebx, ebx		; Non-Inst pages get 0 value
	mov	ecx, MAX_INST_PAGES	; # of pages to initialize
	mov	edi, offset32 LoadHi_Inst_Page_Owner ; EDI -> Base of table
	cld
IICStOwnLP:
	dec	ecx			; Page numbers are 0-10Fh
	cmp	LoadHi_Instance_Map_Table_Size[ecx],0 ; Instance page?
	jnz	short IICStOwnSet	; Yes, store SYS_VM_Handle
	xchg	eax,ebx 		; No, store 0
IICStOwnSet:
	mov	[edi][ecx*4], eax	; Set owner in table
	jnz	short IICOwnOK
	xchg	eax,ebx 		; Return values to correct regs
IICOwnOK:
	cmp	ecx,0A0H
	ja	SHORT IICStOwnLP

IICOK:

; mark a flag to record that we have completed the instance buffer analysis.
; If we get an AddInstanceItem call after this, we will have to redo the 
; analysis allover again.  This flag should be set even if we have no 
; instance data areas till now.

	mov	[LoadHi_InstanceAnalysisDone],-1
	xor	eax,eax
	dec	eax
	leave
	ret

IICFatal4:
	debug_out "_LoadHi_InstanceInitComplete Temp_VM_Data_Area outstanding"

	FATAL_ERROR

IICFatal3:
	debug_out "Allocation failure VM1 LoadHi_Inst _LoadHi_InstanceInitComplete"
IFDEF DEBUG
	jmp	short IICFXX
ENDIF
IICFatal2:
	debug_out "Allocation failure LoadHi_Inst snap _LoadHi_InstanceInitComplete"
IFDEF DEBUG
	jmp	short IICFXX
ENDIF
IICFatal:
	debug_out "Allocation failure LoadHi_Inst Descrip _LoadHi_InstanceInitComplete"
IICFXX:

	FATAL_ERROR

EndProc LoadHi_InstanceInitComplete
;----------------------------------------------------------------------------;

     

;
; AllocateLoadHi_InstanceMapStruc - Allocate an LoadHi_Instance_Map_Struc in the LoadHi_Instance
;		description buffer
;
; ENTRY:
;	None
;
; EXIT:
;	edx is (current) linear address of LoadHi_Instance_Map_Struc allocated
;	eax is offset into LoadHi_Instace description buffer of LoadHi_Instance_Map_Struc
;		allocated
;
; USES:
;	eax,edx,flags
;
BeginProc AllocateLoadHi_InstanceMapStruc,PUBLIC

	push	esi
	push	edi
	push	ebx
	push	ecx

AIMSTestAllo:
	mov	edx,[LoadHi_Instance_MapCurrOff]
	add	edx,SIZE LoadHi_Instance_Map_Struc
	cmp	edx,[LoadHi_Instance_MapLimit]
	jbe	short AIMSDoAllo
    ;
    ; Convert lAddrs to offsets
    ;
	mov	eax,[LoadHi_Instance_MapLaddr]
	sub	[LoadHi_Instance_MapCurrOff],eax
	sub	[LoadHi_Instance_MapLimit],eax

	VMMCall _PageGetSizeAddr,<[LoadHi_Instance_MapHand],0> ; Get size
	or	eax,eax
	jz	short AIMSFail
	inc	eax			; grow by one page
	VMMCall _PageReAllocate,<[LoadHi_Instance_MapHand],eax,PageZeroInit> ; Grow
	or	eax,eax
	jz	short AIMSFail
    ;
    ; Convert offsets back into lAddrs
    ;
	add	[LoadHi_Instance_MapLimit],4096
	add	[LoadHi_Instance_MapLimit],edx
	add	[LoadHi_Instance_MapCurrOff],edx
	mov	[LoadHi_Instance_MapLaddr],edx
	mov	[LoadHi_Instance_MapHand],eax
	jmp	short AIMSTestAllo

AIMSDoAllo:
	mov	edx,[LoadHi_Instance_MapCurrOff]
	add	[LoadHi_Instance_MapCurrOff],SIZE LoadHi_Instance_Map_Struc	; Do alloc
	mov	eax,edx
	sub	eax,[LoadHi_Instance_MapLaddr] ; Offset into LoadHi_Instance description buf
AIMSDone:
	pop	ecx
	pop	ebx
	pop	edi
	pop	esi
	ret

AIMSFail:
	debug_out "Fail grow LoadHi_Instance desc buff AllocateLoadHi_InstanceMapStruc"
	FATAL_ERROR


EndProc AllocateLoadHi_InstanceMapStruc
;----------------------------------------------------------------------------;
VxD_ICODE_ENDS

END
