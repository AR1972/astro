PAGE 58,132
;******************************************************************************
TITLE LoadHi -- Support for Load Hi under Windows 3.0x
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990-1991
;
;   Title:	LoadHi -- Support for Load Hi under Windows 3.0x
;
;   Version:	1.00
;
;   Date:	20-Jul-1990
;
;   Author:	AC
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;    08-27-1990	AC  Hooked "Allocate_Global_V86_Data_Area" call to fix WIN3.0 bug
;    10-01-1990 ARR added some V86MMGR bug fix work around code.
;    10-03-1990 ARR Remove _Allocate_Global_V86_Data_Area work around (due to
;		      errata 10 handling). This work around is now sole job
;		      of LA20HMA device
;    02-20-1991 AC  Will not load if duplicate or not from INT 2F chain.
;
;==============================================================================
;
;   DESCRIPTION:
;
;******************************************************************************

	.386p


;******************************************************************************
;			      I N C L U D E S
;******************************************************************************



	.XLIST
	INCLUDE VMM.Inc
	INCLUDE DOSMGR.Inc
	INCLUDE Debug.Inc
	INCLUDE Instance.Inc
	INCLUDE v86mmgr.Inc
	.LIST

	Create_VDD2_Service_Table EQU TRUE
	INCLUDE .\VDD2.Inc
	LoadHi_Service_Table EQU VDD2_Service_Table
	Num_LoadHi_Services EQU Num_VDD2_Services

;******************************************************************************
;		 V I R T U A L	 D E V I C E   D E C L A R A T I O N
;******************************************************************************


Declare_Virtual_Device LoadHi, 1, 0, LoadHi_Control, VDD2_Device_ID, VMM_Init_Order

;******************************************************************************
;				 E Q U A T E S
;******************************************************************************

MONO_DISP_LO	equ	0B0H		;start page in mono display area
MONO_DISP_HI	equ	0B7H		;end page in mono display area

;******************************************************************************
;			     S T R U C T U R E S
;******************************************************************************

LimInstanceData	STRUC
LimInstanceDataPtr	dd	?	;pointer to instance data start
LimInstanceDataSize	dw	?	;size in bytes
LimInstanceData	ENDS

;******************************************************************************
;			   F L A G   E Q U A T E S
;******************************************************************************


;******************************************************************************
;		   I N I T I A L I Z A T I O N	 D A T A
;******************************************************************************

VxD_IDATA_SEG

;;;;Real_AGVDA_Addr	    dd	    ?	    ;original Allocate_Global_V86_Data_Area

; define a copyright string

MS_CopyRight		db	'(C) Copyright MICROSOFT Corp., 1990',0
LoadHi_Device_Name	db	'LoadHi',0,0
CrashMsg		db	'LoadHi:Sys_Critical_Init Fails',0


; define strings for 'LOCAL=' and 'GLOBAL=' section names in [386Enhanced]
; section of SYSTEM.INI. 

LH_DevNameBuf		db	8 dup (?);for storing device name
LH_Local_String		db	'LOCAL',0
LH_Global_String	db	'GLOBAL',0

Prev_GPS_Pointer	dd	?	; previous GetProfileString pointer

VxD_IDATA_ENDS


;******************************************************************************
;	      R E A L	M O D E   I N I T I A L I Z A T I O N
;******************************************************************************

VxD_REAL_INIT_SEG

	Extrn	Real_Mode_Hook:near

;******************************************************************************
;
;   LoadHi_Real_Mode_Init
;
;   DESCRIPTION:
;
;	The VxD will not load if this is a duplicate load or if it is not
;	being loaded from the INT 2F chain.
;
;	This VxD should only be loaded for version 3.0 of Windows/386.
;	The real mode initialization portion of this device will abort its
;	load if the VMM version is less than version 3.10 (hex 30A)
;				
;       If our test succeedes, we need to call a Limulator specific routine
;       to do test for them and this routine would setup registers such that
;       we could do a return to VMM directly. Look at the default code in
;       UMB.ASM
;
;   ENTRY:
;	CS, DS = Real mode segment
;	AX = VMM version (AH=Major, AL=Minor)
;	BX = Flags
;
;   EXIT:
;	BX = Null pointer (no pages to exclude)
;	SI = Null pointer (no instance data)
;	EDX = Reference data to pass to protected mode portion of VxD
;
;   USES:
;	AX, BX, DX, SI, Flags
;
;==============================================================================

BeginProc LoadHi_Real_Mode_Init

; check for duplicate device load.

	test	bx, Duplicate_From_INT2F OR Duplicate_Device_ID
	jnz	SHORT LH_RMI_Fail_Load

; this is not a duplicate load, but was the device loaded from INT 2F chain ?

	test	bx,Loading_From_INT2F
	jnz	SHORT LH_RMI_Loading_Ok		;loading type is OK.

LH_RMI_Fail_Load:

; fail this device load without any error message.

	xor	bx,bx				;no pages to exclude
	xor	si,si				;no instance data
	mov	ax, Abort_Device_Load + No_Fail_Message
	jmp	SHORT LH_RMI_Exit

LH_RMI_Loading_Ok:

;
;   Make sure that we're running on Windows/386 version 3.10 or later.
;   If not, abort the load of this device with a warning message.
;

	cmp	ax, 30Ah			; Q: Is this version < 3.10?
	jae	SHORT LH_RMI_Abort_Load 	;    N: Don't load me
						;    Y: Return success flags

; call Lim specific routines and return with whatever registers they setup

	call	Real_Mode_Hook			;Lim specific real mode hook

	ret

LH_RMI_Abort_Load:

	mov	ax, Abort_Device_Load 

LH_RMI_Exit:

	ret

EndProc LoadHi_Real_Mode_Init
VxD_REAL_INIT_ENDS
;******************************************************************************
;			    L O C A L	D A T A
;******************************************************************************

VxD_DATA_SEG

			public FirstUMBPage
			public NumUMBPages
			public CB_LoadHi_Inst_Hand			
			public CB_LoadHi_Inst_Buf_ptr
			public GlobalDeviceList
			public InstanceDataList 

FirstUMBPage		dd	? 	;first UMB page number
NumUMBPages	 	dd	? 	;number of pages that we have to deal with
CB_LoadHi_Inst_Buf_ptr	dd	?	;Int buf ptr offset in CB
CB_LoadHi_Inst_Hand	dd	?	;Int buf handle offset in CB
DeviceHeaderStartPtr	dd	?	;start of device header chain
GlobalDeviceList	dd	?	;list handle for GLOBAL= List
InstanceDataList	dd	?	;list handle for inst data for devices
MonoDispAreaHasUMB	dd	0	;UMB in mono display area or not.
LoadHiDeviceActive	dd	-1	;we are active by default

	;------------------------------------------------;
	; save chain addresses for the hooked functions. ;
	;------------------------------------------------;

			public ActualAddInstanceItem	
			public ActualTGV86Mem	 	
			public ActualDOSMGRInstanceDev	

ActualAddInstanceItem	dd	?	;original _AddInstanceItem service
ActualTGV86Mem	 	dd	?	;original _TestGlobalV86Mem
ActualDOSMGRInstanceDev	dd	?	;original DOSMGR_InstanceDevice

LoadHi_OwnCall		dd	0	;INT 21H hook flag
UMBLinkState		dd	?	;original state of UMB links

V86_CB_Offset_Addr	dd	0	;Addr of Control block offset for V86MMGR
EMM_Patch_Ret_Addr	dd	0
EMM_Patch_NotPartial_Addr dd	0
V86_ImprtPtr_Addr	dd	0	;Addr of variable for V86MMGR
EMM_Patch_Ret_Addr2	dd	0
EMM_Patch_Ret_Addr2a	dd	0

	;------------------------------------------------;

VxD_DATA_ENDS



;******************************************************************************
;	       D E V I C E   C O N T R O L   P R O C E D U R E
;******************************************************************************

VxD_CODE_SEG

	Extrn	LoadHi_Instance_Create_VM:near
	Extrn	LoadHi_Instance_Destroy_VM:near
	Extrn	LoadHi_Instance_Suspend_VM:near
	Extrn   LoadHi_Instance_Resume_VM:near
	Extrn	LoadHi_InstanceInitComplete:near
IFDEF	DEBUG
	Extrn	LoadHi_Debug_Query:near
ENDIF
	Extrn	LoadHi_Take_Instance_SnapShot:near
	Extrn	Swap_LoadHi_Instance_Page:near
	Extrn	Get_Mapped_Page_Num:near
	Extrn	Control_Call_Hook:near

;******************************************************************************
;
;   LoadHi_Control
;
;   DESCRIPTION:
;
;   ENTRY:
;	EAX = Control call ID
;
;   EXIT:
;	If carry clear then
;	    Successful
;	else
;	    Control call failed
;
;   USES:
;	EAX, EBX, ECX, EDX, ESI, EDI, Flags
;
;==============================================================================

BeginProc LoadHi_Control,PUBLIC

; call 'Control_Call_Hook' so that LIMulators can hook any of the control 
; calls that they want to do additional processing on.

	pushad			
	call	Control_Call_Hook
	popad
	jc	SHORT LoadHi_Control_Ret;LIMulator failed the call

; if our part of the device is turned off because we are running on a newer
; version of windows, we should not call our hooks.

	cmp	LoadHiDeviceActive,0	;is it active ?
	jz	SHORT LoadHi_Control_Ret;no, carry is clear, return

; trap the control calls that we want to hook our selves.

	Control_Dispatch Sys_Critical_Init, LoadHi_Sys_Critical_Init
	Control_Dispatch Device_Init, LoadHi_Device_Init
	Control_Dispatch Create_VM, LoadHi_Instance_Create_VM
	Control_Dispatch Destroy_VM, LoadHi_Instance_Destroy_VM
	Control_Dispatch Init_Complete, LoadHi_InstanceInitComplete
	Control_Dispatch VM_Suspend, LoadHi_Instance_Suspend_VM
	Control_Dispatch VM_Resume, LoadHi_Instance_Resume_VM
	Control_Dispatch Sys_VM_Init, LoadHi_Sys_VM_Init
	Control_Dispatch System_Exit, <LoadHi_SystemExit>


IFDEF	DEBUG
	Control_Dispatch Debug_Query, LoadHi_Debug_Query
ENDIF	;DEBUG

	clc					; Ignore other control calls

LoadHi_Control_Ret:

	ret

EndProc LoadHi_Control

;----------------------------------------------------------------------------;
; LoadHi_Map_Pages:							     ;
;								             ;
; This routine maps in the UMB pages into the VM's address space. 	     ;
;----------------------------------------------------------------------------;

BeginProc LoadHi_Map_Pages,PUBLIC

	mov	eax,[FirstUMBPage]	;first page in the array
	mov	ecx,[NumUMBPages]	;number of pages to map in.

MapPagesLoop:

	call	Get_Mapped_Page_Num	;edx has the mapped page no.
	or	edx,edx			;is it an UMB ?
	jz	SHORT MapPagesCont	;no.
	push	ecx
	push	eax
	VMMcall _PhysIntoV86, <edx,ebx,eax,1,0>
	pop	eax
	pop	ecx

IFDEF	DEBUG
	or	eax,eax			;was the mapping successful ?
	jnz	SHORT @f 		;yes.
	debug_out "LoadHi:PhysIntoV86 fails to map phys page number #EDX into page number #EAX"
@@:
ENDIF	;DEBUG


MapPagesCont:

	inc	eax			;next V86 page
	loop	SHORT MapPagesLoop	;continue

	ret

EndProc LoadHi_Map_Pages
;----------------------------------------------------------------------------;
VxD_CODE_ENDS


;******************************************************************************
;		    I N I T I A L I Z A T I O N   C O D E
;******************************************************************************

VxD_ICODE_SEG

	Extrn	LoadHi_Add_Instance_Item:near
	Extrn	Get_UMB_Info:near
	Extrn	Get_Device_Info:near
	Extrn	Get_Ptr_To_Instance_Data_List:near

;----------------------------------------------------------------------------;
; LoadHi_Sys_Critical_Init:						     ;
;									     ;
; Entry:								     ;
;	 EDX  - 32 bit linear address of a table which has the UMB Page to   ;
;		physical page mapping information.			     ;
;									     ;
;        EBX  - VM handle						     ;
;	 EBP  - Client register frame for VM				     ;
;									     ;
; Action:								     ;
;	 . Saves the address of the mapping table for later use.   	     ;
;	 . Allocate space in Control Block for a couble of DWORDS to keep    ;
;	   track of each VM's instance data.				     ;
;	 . Hooks relevant VMM and DOSMGR services that this device must      ;
;	   monitor.							     ;
;									     ;
; Exit:									     ;
;									     ;
; Uses:									     ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Sys_Critical_Init,PUBLIC

    ;
    ; Implement the work arounds for some bugs in the 3.00 V86MMGR device.
    ;
	pushad
	xor	eax,eax

	VxDCall V86MMGR_Get_Version	      	

; if the version is above or equal 3.0A, the LoadHi device should be turned
; off.

	cmp	eax,030Ah		      	;Q: Is this version < 3.0A
	jb	SHORT LH_SCI_OK			;it is OK

; turn ourselves off.

	mov	LoadHiDeviceActive,0		;inactive

LH_SCI_OK:

	cmp	eax,0300h
	jne	LH_NoFix

    ;
    ; This first fix fixes the problem with the marking of ROM pages in the
    ;	V86MMGR memory scan code
    ;
	mov	eax, V86MMGR_Set_Mapping_Info
	mov	esi, 0FFFFFFFFh

	VMMcall Hook_Device_Service

IFDEF DEBUG
	jnc	short LHD05
	debug_out "Couldn't patch V86MMGR 1"
LHD05:
ENDIF
	jc	LHSCI_Fail
	push	esi
	mov	eax, V86MMGR_Set_Mapping_Info

	VMMcall Hook_Device_Service

	pop	esi
IFDEF DEBUG
	jnc	short LHD05a
	debug_out "Couldn't patch V86MMGR 1a"
LHD05a:
ENDIF
	jc	LHSCI_Fail
	mov	eax,0EDFh
	cmp	dword ptr [esi.eax],0107C766h
	jne	short TryDebug
	cmp	byte ptr [esi.eax+4],04h
	je	short GotPatchLoc

TryDebug:
	mov	eax,14DCh
	cmp	dword ptr [esi.eax],0107C766h
IFDEF DEBUG
	je	short LHD10
	debug_out "Did not find patch instruction 1 LH_Sys_Critical_Init"
LHD10:
ENDIF
	jne	short LH_SecondFix
	cmp	byte ptr [esi.eax+4],04h
IFDEF DEBUG
	je	short LHD20
	debug_out "Did not find patch instruction 2 LH_Sys_Critical_Init"
LHD20:
ENDIF
	jne	short LH_SecondFix
GotPatchLoc:
	lea	edx,[esi.eax+5] 		; EIP after patched instruction
	mov	ecx,offset32 LH_V86_Fix_Proc	; Patch Target
	sub	ecx,edx
	mov	byte ptr [esi.eax],0E8h 	; Near call
	mov	dword ptr [esi.eax+1],ecx
    ;
    ; This fix is for some problems in the EMM code.
    ;
LH_SecondFix:
	mov	eax, V86MMGR_Get_EMS_XMS_Limits
	mov	esi, 0FFFFFFFFh

	VMMcall Hook_Device_Service

IFDEF DEBUG
	jnc	short LHD30
	debug_out "Couldn't patch V86MMGR 2"
LHD30:
ENDIF
	jc	LHSCI_Fail
	push	esi
	mov	eax, V86MMGR_Get_EMS_XMS_Limits

	VMMcall Hook_Device_Service

	pop	esi
IFDEF DEBUG
	jnc	short LHD30a
	debug_out "Couldn't patch V86MMGR 2a"
LHD30a:
ENDIF
	jc	LHSCI_Fail
	mov	eax,27E6h
	cmp	dword ptr [esi.eax],75014338h
	jne	short TryDebug2
	cmp	byte ptr [esi.eax+4],48h
	je	short GotPatchLoc2
TryDebug2:
	mov	eax,3F24h
	cmp	dword ptr [esi.eax],75014338h
IFDEF DEBUG
	je	short LHD40
	debug_out "Did not find patch instruction 3 LH_Sys_Critical_Init"
LHD40:
ENDIF
	jne	short LH_ThirdFix
	cmp	byte ptr [esi.eax+4],48h
IFDEF DEBUG
	je	short LHD50
	debug_out "Did not find patch instruction 4 LH_Sys_Critical_Init"
LHD50:
ENDIF
	jne	short LH_ThirdFix
GotPatchLoc2:
	mov	word ptr [esi.eax+3],9090h	; NOP jne instruction
LH_ThirdFix:
    ;
    ; ESI still -> V86MMGR_Get_EMS_XMS_Limits address
    ;
	mov	eax,1B86h
	cmp	word ptr [esi.eax],358Bh
	jne	TryDebug3
	cmp	dword ptr [esi.eax+6],1D45B60Fh
	jne	TryDebug3
	mov	edx,dword ptr [esi.eax+2]
	mov	[V86_CB_Offset_Addr],edx
	mov	eax,26E9h
	cmp	dword ptr [esi.eax],7E800788h
IFDEF DEBUG
	je	short LHD55
	debug_out "Did not find patch instruction 5 LH_Sys_Critical_Init"
LHD55:
ENDIF
	jne	LH_NoFix
	cmp	word ptr [esi.eax+4],0401h
IFDEF DEBUG
	je	short LHD60
	debug_out "Did not find patch instruction 6 LH_Sys_Critical_Init"
LHD60:
ENDIF
	jne	LH_NoFix
	cmp	byte ptr [esi.eax+6],74h
IFDEF DEBUG
	je	short LHD70
	debug_out "Did not find patch instruction 7 LH_Sys_Critical_Init"
LHD70:
ENDIF
	jne	LH_NoFix
	jmp	GotPatchLoc3

TryDebug3:
	mov	eax,2974h
	cmp	word ptr [esi.eax],358Bh
IFDEF DEBUG
	je	short LHD90
	debug_out "Did not find patch instruction 8 LH_Sys_Critical_Init"
LHD90:
ENDIF
	jne	LH_FourthFix
	cmp	dword ptr [esi.eax+6],1D45B60Fh
IFDEF DEBUG
	je	short LHD100
	debug_out "Did not find patch instruction 9 LH_Sys_Critical_Init"
LHD100:
ENDIF
	jne	LH_FourthFix
	mov	edx,dword ptr [esi.eax+2]
	mov	[V86_CB_Offset_Addr],edx
	mov	eax,3DC0h
	cmp	dword ptr [esi.eax],7E800788h
IFDEF DEBUG
	je	short LHD110
	debug_out "Did not find patch instruction 10 LH_Sys_Critical_Init"
LHD110:
ENDIF
	jne	short LH_FourthFix
	cmp	word ptr [esi.eax+4],0401h
IFDEF DEBUG
	je	short LHD120
	debug_out "Did not find patch instruction 11 LH_Sys_Critical_Init"
LHD120:
ENDIF
	jne	short LH_FourthFix
	cmp	byte ptr [esi.eax+6],74h
IFDEF DEBUG
	je	short LHD130
	debug_out "Did not find patch instruction 12 LH_Sys_Critical_Init"
LHD130:
ENDIF
	jne	short LH_FourthFix
GotPatchLoc3:
	lea	edx,[esi.eax+8] 		; EIP after modified instructions
	mov	[EMM_Patch_Ret_Addr],edx
	movzx	ecx,byte ptr [esi.eax+7]	; Target of trailing JE
	add	ecx,edx
	mov	[EMM_Patch_NotPartial_Addr],ecx
	lea	edx,[esi.eax+2+5]		; EIP after instruction we insert
	mov	ecx,offset32 LH_V86_EMM_Fix_Proc; Patch Target
	sub	ecx,edx
	mov	byte ptr [esi.eax+2],0E9h	; Near unconditional jump
	mov	dword ptr [esi.eax+2+1],ecx
	mov	byte ptr [esi.eax+2+1+4],90h	; NOP left over byte
LH_FourthFix:
    ;
    ; ESI still -> V86MMGR_Get_EMS_XMS_Limits address
    ;
	mov	eax,16BEh
	cmp	word ptr [esi.eax], 3D8Bh
	jne	TryDebug4
	cmp	dword ptr [esi.eax+6], 510A7F8Dh
	jne	TryDebug4
	mov	ecx,dword ptr [esi.eax+2]
	mov	[V86_ImprtPtr_Addr],ecx
	cmp	dword ptr [esi.eax+2Eh], 6602E8C1h
	jne	TryDebug4
	cmp	dword ptr [esi.eax+2Eh+4], 59024189h
	jne	short TryDebug4
DoP4Retail:
	lea	edx,[esi.eax+5] 		; Jump back address
	mov	[EMM_Patch_Ret_Addr2],edx
	inc	[EMM_Patch_Ret_Addr2]		; Skip trailing NOP
	mov	ecx,offset32 LH_V86_EMM_Fix_Proc2 ; Patch Target
	sub	ecx,edx
	mov	byte ptr [esi.eax],0E9h 	; Near unconditional jump
	mov	dword ptr [esi.eax+1],ecx
	mov	byte ptr [esi.eax+5],90h	; NOP left over byte

	lea	edx,[esi.eax+2Eh+3+5]		; Jump back address
	mov	[EMM_Patch_Ret_Addr2a],edx
	mov	ecx,offset32 LH_V86_EMM_Fix_Proc2a ; Patch Target
	sub	ecx,edx
	mov	byte ptr [esi.eax+2Eh+3],0E9h	; Near unconditional jump
	mov	dword ptr [esi.eax+2Eh+3+1],ecx

	mov	eax,16E3h
	cmp	dword ptr [esi.eax], 4188028Ah
	jne	TryDebug4a
	cmp	byte ptr [esi.eax+4], 01h
	jne	TryDebug4a
	cmp	dword ptr [esi.eax+5], 0442B70Fh
	jne	TryDebug4a
DoP4aRetail:
	mov	byte ptr [esi.eax+1], 06h	; change EDX to ESI
	mov	byte ptr [esi.eax+7], 46h	; change EDX to ESI
	jmp	LH_NoFix

TryDebug4:
	mov	eax,23C9h
	cmp	word ptr [esi.eax], 3D8Bh
IFDEF DEBUG
	je	short LHD140
	debug_out "Did not find patch instruction 9 LH_Sys_Critical_Init"
LHD140:
ENDIF
	jne	TryDebug4a
	cmp	dword ptr [esi.eax+6], 510A7F8Dh
IFDEF DEBUG
	je	short LHD150
	debug_out "Did not find patch instruction 10 LH_Sys_Critical_Init"
LHD150:
ENDIF
	jne	TryDebug4a
	mov	ecx,dword ptr [esi.eax+2]
	mov	[V86_ImprtPtr_Addr],ecx
	cmp	dword ptr [esi.eax+95h], 6602E8C1h
IFDEF DEBUG
	je	short LHD160
	debug_out "Did not find patch instruction 11 LH_Sys_Critical_Init"
LHD160:
ENDIF
	jne	short TryDebug4a
	cmp	dword ptr [esi.eax+95h+4], 59024189h
IFDEF DEBUG
	je	short LHD170
	debug_out "Did not find patch instruction 12 LH_Sys_Critical_Init"
LHD170:
ENDIF
	jne	short TryDebug4a
DoP4Debug:
	lea	edx,[esi.eax+5] 		; Jump back address
	mov	[EMM_Patch_Ret_Addr2],edx
	inc	[EMM_Patch_Ret_Addr2]		; Skip trailing NOP
	mov	ecx,offset32 LH_V86_EMM_Fix_Proc2 ; Patch Target
	sub	ecx,edx
	mov	byte ptr [esi.eax],0E9h 	; Near unconditional jump
	mov	dword ptr [esi.eax+1],ecx
	mov	byte ptr [esi.eax+5],90h	; NOP left over byte

	lea	edx,[esi.eax+95h+3+5]		; Jump back address
	mov	[EMM_Patch_Ret_Addr2a],edx
	mov	ecx,offset32 LH_V86_EMM_Fix_Proc2a ; Patch Target
	sub	ecx,edx
	mov	byte ptr [esi.eax+95h+3],0E9h	; Near unconditional jump
	mov	dword ptr [esi.eax+95h+3+1],ecx
TryDebug4a:
	mov	eax,2414h
	cmp	dword ptr [esi.eax], 4188028Ah
IFDEF DEBUG
	je	short LHD180
	debug_out "Did not find patch instruction 13 LH_Sys_Critical_Init"
LHD180:
ENDIF
	jne	short LH_NoFix
	cmp	dword ptr [esi.eax+4], 01428A01h
IFDEF DEBUG
	je	short LHD190
	debug_out "Did not find patch instruction 14 LH_Sys_Critical_Init"
LHD190:
ENDIF
	jne	short LH_NoFix
	cmp	dword ptr [esi.eax+27h], 0442B70Fh
IFDEF DEBUG
	je	short LHD200
	debug_out "Did not find patch instruction 15 LH_Sys_Critical_Init"
LHD200:
ENDIF
	jne	short LH_NoFix
DoP4aDebug:
	mov	byte ptr [esi.eax+1], 06h	; change EDX to ESI
	mov	byte ptr [esi.eax+6], 46h	; change EDX to ESI
	mov	byte ptr [esi.eax+27h+2], 46h	; change EDX to ESI
LH_NoFix:
	popad

; if we are inactive (newer version of windows) we should proceed further
; with the call.

	cmp	LoadHiDeviceActive,0		;are we active ?
	jz	LH_SCI_Ret			;no.

; get information about the first UMB page and the number of pages that we
; could expect UMBs to be in starting with the first page. EDX has a pointer
; to a structure that has this information and the following routine can
; interpret it.




; EDX was the linear address in EMM386's environment. We need to get the 
; coressponding linear address in WIN386's space. We can't touch the block
; to figure out how big it is, for now we are going to go with the max size
; of the block, which is 4 + 4 + (100 - A0)* 4 = 392.

	

	VMMcall	_MapPhysToLinear,<edx, 400, 0>
	mov	edx,eax				;get it back into edx.
	

	call	Get_UMB_Info
	mov	FirstUMBPage,eax	;save the first UMB page number
	mov	NumUMBPages,edx		;number of pages containing UMBs

; check to see whether the start page is above the minimum that we can work with.

	cmp	eax,MIN_LOADHI_INST_PAGE
	jae	SHORT @f     		;it is OK

IFDEF	DEBUG
	debug_out "LoadHi: First UMB page is #EAX, this is unacceptable"
ENDIF	;DEBUG

	jmp	LHSCI_Fail		;fail the call
@@:

; create LIST handles for the lists that we will need for handling memory
; needed for instancing devices and for maintaining the list of GLOBAL= 
; devices.

	mov	eax,LF_Use_Heap
	mov	ecx,8			;for device names
	VMMcall	List_Create		
	mov	GlobalDeviceList,esi	;for global= device list

	mov	eax,LF_Use_Heap
	mov	ecx,SIZE InstDataStruc	;size of 'AddInstanceItem' datastruct
	VMMcall	List_Create		
	mov	InstanceDataList,esi	;for instancing devices


; allocate space worth 2 DWORDs in the Control Block and save the two offsets

	VMMcall	_Allocate_Device_CB_Area,<8,0>
	or	eax,eax			;did call fail ?

IFDEF	DEBUG
	jnz	SHORT @f 		;no.
	debug_out "LoadHi: Allocate_Device_CB_Area fails to allocate 8 bytes"
	jmp	LHSCI_Fail		;fail the call
@@:
ELSE
	jz	LHSCI_Fail		;yes, fail this call
ENDIF   ;DEBUG

	mov	CB_LoadHi_Inst_Buf_Ptr,eax
	add	eax,4			;next dword offset
	mov	CB_LoadHi_Inst_Hand,eax

; hook some of the services. Start with the _AddInstanceItem service.

	mov	eax,_AddInstanceItem	
	mov	esi,OFFSET32 LoadHi_Add_Instance_Item
	VMMcall	Hook_Device_Service

IFDEF	DEBUG
	jnc	SHORT @f		;successful
	debug_out "LoadHi: Hook_Device_Service fails to hook AddInstanceItem"
	jmp	LHSCI_Fail		;fail the call
@@:
ELSE
	jc	LHSCI_Fail		;fail the call
ENDIF	;DEBUG

	mov	[ActualAddInstanceItem],esi

; next hook the TestGlobalV86Mem service.

	mov	eax,_TestGlobalV86Mem
	mov	esi,OFFSET32 LoadHi_TestGlobalV86Mem
	VMMcall	Hook_Device_Service

IFDEF	DEBUG
	jnc	SHORT @f		;successful
	debug_out "LoadHi: Hook_Device_Service fails to hook TestGlobalV86Mem"
	jmp	LHSCI_Fail		;fail the call
@@:
ELSE
	jc	LHSCI_Fail 	;fail the call
ENDIF	;DEBUG

	mov	[ActualTGV86Mem],esi

; hook the DOSMGR_Instance_Device service

	mov	eax,DOSMGR_Instance_Device
	mov	esi,OFFSET32 LoadHi_Instance_Device
	VMMcall	Hook_Device_Service

IFDEF	DEBUG
	jnc	SHORT @f		;successful
	debug_out "LoadHi: Hook_Device_Service fails to hook DOSMGR_Instance_Device"
	jmp	LHSCI_Fail		;fail the call
@@:
ELSE
	jc	DEBFAR LHSCI_Fail  	;fail the call
ENDIF	;DEBUG

	mov	[ActualDOSMGRInstanceDev],esi

;;;;; hook the Allocate_Global_V86_Data_Area service
;;;;
;;;;	    mov     eax,_Allocate_Global_V86_Data_Area
;;;;	    mov     esi,OFFSET32 LoadHi_AGVDA_Hook
;;;;	    VMMcall Hook_Device_Service
;;;;
;;;;IFDEF   DEBUG
;;;;	    jnc     SHORT @f		    ;successful
;;;;	    debug_out "LoadHi: Hook_Device_Service fails to hook Allocate_Global_V86_Data_Area"
;;;;	    jmp     LHSCI_Fail		    ;fail the call
;;;;@@:
;;;;ELSE
;;;;	    jc	    DEBFAR LHSCI_Fail	    ;fail the call
;;;;ENDIF   ;DEBUG
;;;;
;;;;	    mov     [Real_AGVDA_Addr],esi

; assign the UMB pages & hook them.

	mov	eax,[FirstUMBPage]	;start UMB page.
	mov	ecx,[NumUMBPages]	;no of entries in the table

AssignPagesLoop:

; get the mapping page number.

	call	Get_Mapped_Page_Num	;edx returns the mapped page number
	or	edx,edx			;is it a UMB page ?
	jz	SHORT AssignPagesCont  	;no.

; if the page is in the range for a monochrome adapter, set a flag.

	call	ProcessPageInMonoDispArea

	push	ecx			;save loop count
	push	eax			;save current page number
	VMMcall _Assign_Device_V86_Pages, <eax,1,0,0>
	or	eax,eax			;did the call fail ?
	pop	eax			;get back page number
	jnz	SHORT @f 		;successful
	pop	ecx			;balance the stack

IFDEF	DEBUG
	debug_out "LoadHi: Assign_Device_V86_Pages fails on page number #EAX"
ENDIF	;DEBUG
	jmp	SHORT LHSCI_Fail   	;fail the call
@@:

; hook the page for monitoring page fault.

	push	eax			;save the current page number
	mov	esi,OFFSET32 Swap_LoadHi_Instance_Page
	VMMcall	Hook_V86_Page
	pop	eax
  	pop	ecx

IFDEF	DEBUG
	jnc	SHORT @f		;call was successful
	debug_out "LoadHi: Hook_V86_Page fails on page number #EAX"
	jmp	SHORT LHSCI_Fail     	;fail the call
@@:
ELSE
	jc	DEBFAR LHSCI_Fail	;fail the call
ENDIF 	;DEBUG
	
AssignPagesCont:

	inc	eax			;next V86 page number
	loop	AssignPagesLoop		;go through the table.

AssignPagesDone:

; map the pages in for the SYSTEM VM

    	VMMcall	Get_Sys_VM_Handle	;EBX = Sys_VM_Handle
	call	LoadHi_Map_Pages	;map pages in for SYS_VM
	
; we are done.

LH_SCI_Ret:
	
	clc
	ret

LHSCI_Fail:

    	Fatal_Error <OFFSET32 CrashMsg>

EndProc LoadHi_Sys_Critical_Init
;----------------------------------------------------------------------------;
; ProcessPageInMonoDispArea:						     ;
;									     ;
; ENTRY:								     ;
;       EAX  - V86 page number (0A0H - 10FH)			 	     ;
;	EDX  - Mapped page (0 if EAX is not a UMB page)		             ;
;                                                                            ;
; ACTION:							             ;
;       Sets MonoDispAreaHasUMB to -1 if EAX is within the mono display      ;
;       area page range and EDX is not 0.				     ;
;									     ;
; EXIT:									     ;
;	None.								     ;
;									     ;
; USES:									     ;
;	Flags.								     ;
;----------------------------------------------------------------------------;
BeginProc ProcessPageInMonoDispArea,PUBLIC

	cmp	eax,MONO_DISP_LO	;lower than mono display area ?
	jb	SHORT @f		;yes
	cmp	eax,MONO_DISP_HI	;higher than mono display area ?
	ja	SHORT @f		;yes.
	cmp	edx,0			;is there an UMB in this page ?
	jz	SHORT @f 		;no.
	mov	MonoDispAreaHasUMB,-1   ;there is an UMB in the mono display area
@@:
	ret

EndProc ProcessPageInMonoDispArea
;----------------------------------------------------------------------------;
; LoadHi_Device_Init:							     ;
;									     ;
;   DESCRIPTION:						             ;
;									     ;
;       At first this routine get the current UMB state and saves it, it then;
;       unliks the UMB and sets its own INT 21 hook which will prevent the   ;
;       UMBs from being linked back in.					     ;
;							                     ;
;       The routine then builds up a list of all 'GLOBAL=' devices that are  ;
;	specified in the [386Enhanced] section of system.ini.	     	     ;
;									     ;
;	It then scans [386Enhanced] section of SYSTEM.INI for 'LOCAL='       ;
;	devices and finds out whether they are LoadHi devices or not. If they;
;	are, then DOSMGR_Instance_Device is called. Ofcourse this call will  ;
;       be intercepted by the hook in this device and will not be chained on.;
;	This routine will do nothing for devices that are not loaded high.   ;
;									     ;
;   ENTRY:							  	     ;
;									     ;
;   EXIT:								     ;
;									     ;
;   USES:							             ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Device_Init,PUBLIC


; Get the UMB Link state, save it and turn the link state off.

	Push_Client_State
	VMMcall Begin_Nest_Exec 	;Get ready for software ints

; get the link state.

	mov	[ebp.Client_AX], 05802h ;get UMB link state
	mov	eax, 21h		;INT 21H to be done
	VMMcall Exec_Int		;do the INT
	movzx	eax,[ebp.Client_AX]	;return is in AL
	mov	UMBLinkState,eax	;save it

; reset the link state.

	mov	[ebp.Client_AX], 05803h ;set UMB link state
	mov	[ebp.Client_BX], 0	;unlink them
	mov	eax, 21h		;INT 21H to be done
	VMMcall Exec_Int		;do the INT

; end the nested exec calls.

	VMMcall End_Nest_Exec
	Pop_Client_State

; now hook INT 21H in Ring0.

	mov	eax, 21h		;want to hook INT 21H
	mov	esi, OFFSET32 Loadhi_Int21ISR
	VMMcall Hook_V86_Int_Chain	;INT 21H hooked.

; Process the "GLOBAL=" SYSTEM.INI strings

	mov	edi,offset32 LH_Global_String
	xor	esi, esi		;read [386ENH] section
	xor	edx,edx			;no default
	VMMCall Get_Profile_String	;Get first occurance
	mov	Prev_GPS_Pointer,edx	;save return value
	jc	short LHDI_GlobalDone	;Not found
	jmp	short LHDI_DoGlobal

LHDI_NextGlobal:

; get the next occurrence of 'GLOBAL='

	xor	esi, esi		;read [386ENH] section
	mov	edx,Prev_GPS_Pointer	;get last pointer
	mov	edi,offset32 LH_Global_String
	VMMCall Get_Next_Profile_String ;Get next occurance
	mov	Prev_GPS_Pointer,edx	;save return value
	jc	short LHDI_GlobalDone

LHDI_DoGlobal:

; allocate a new node for this.

	mov	esi,GlobalDeviceList	;get the handle
	VMMcall	List_Allocate		;allocate a new node
	VMMcall	List_Attach		;attach node to the list
	mov	edi,eax			;get a pointer to the new node.

	mov	esi,edx			;get a pointer to the name
	mov	ecx,8			;Max length of device names
	cld

LHDI_CopyName:

; copy the device name over to the new node. Pad it with spaces if name is
; less than 8 characters.

	lodsb

LHDI_TestEnd:

	or	al,al
	jz	short LHDI_PadSpace
	stosb
	loop	SHORT LHDI_CopyName
	jmp	short LHDI_NextGlobal

LHDI_PadSpace:

	mov	al,20h			;space pad next of the name
	rep	stosb
	jmp	short LHDI_NextGlobal

LHDI_GlobalDone:

; Process the 'LOCAL=' devices. For each such device find out if the device 
; is loaded high, if it is then instance it.


	mov	edi,offset32 LH_Local_String
	xor	esi, esi		;search [386ENH] section
	xor	edx,edx			;no default
	VMMCall Get_Profile_String	;Get first occurance
	mov	Prev_GPS_Pointer,edx	;save return value
	jc	short LHDI_LocalDone 	;Not found
	jmp	short LHDI_DoLocal

LHDI_NextLocal:

	xor	esi, esi		;read [386ENH] section
	mov	edx,Prev_GPS_Pointer	;get last pointer
	mov	edi,offset32 LH_Local_String
	VMMCall Get_Next_Profile_String ; Get next occurance
	mov	Prev_GPS_Pointer,edx	;save return value
	jc	short LHDI_LocalDone

LHDI_DoLocal:

; prepare a space padded copy of the name in a temporary buffer.

	mov	edi,offset32 LH_DevNameBuf
	mov	eax,20202020h
	mov	dword ptr [edi],eax
	mov	dword ptr [edi+4],eax
	mov	esi,edx
	mov	ecx,8			; Max length of device names

LHDI_CopyLocalName:

	cld
	lodsb
	or	al,al
	jz	short LHDI_TestDev
	stosb
	loop	LHDI_CopyLocalName

LHDI_TestDev:

	mov	esi,offset32 LH_DevNameBuf

; find out if this is in the Global Device List or not.

	call	LoadHi_IsDeviceGlobal	
	jc	SHORT LHDI_NextLocal	;yes, search for the next one.

; find out if the device is a LoadHi device.

	call	Get_Device_Info		;is this a LoadHi device ?
	jc	SHORT LHDI_NextLocal	;no, search for the next one.

; instance the device

	call	LoadHi_Instance_From_EDX_Size_EAX
	jmp	SHORT LHDI_NextLocal	;carry on search

LHDI_LocalDone:

; obtain a pointer to the list of instance data that the LIMulator has obtained
; from WIN386.

	call	Get_Ptr_To_Instance_Data_List

; EDI has a pointer to an array of nodes. Each node in the array is of 6 bytes
; The first DWORD is a seg:off of data to instance (will be in UMB area) and 
; the next WORD will be its size. The array is terminated by a DWORD of 0.
; Instance all of these areas.

LHDI_InatanceAdditionalLoop:

	cmp	[edi.LimInstanceDataPtr],0;end of array ?
	jz	SHORT LHDI_Ret		;yes
	mov	edx,[edi.LimInstanceDataPtr]

; convert the DWORD from seg:off form to a linear address.

	mov	eax,edx			;save it
	and	edx,0ffff0000h		;retain the segment
	shr	edx,12			;get it to start from bit 20.
	and	eax,0ffffh		;just the offset
	add	edx,eax			;EDX - linear address
	movzx	eax,[edi.LimInstanceDataSize]

; Instance the area

	call	LoadHi_Instance_From_EDX_Size_EAX
	add	edi,SIZE LimInstanceData ;next node
	jmp	SHORT LHDI_InatanceAdditionalLoop

LHDI_Ret:

	 clc
	 ret

EndProc LoadHi_Device_Init
;----------------------------------------------------------------------------;
; LoadHi_Instance_Device:						     ;
;									     ;
;   DESCRIPTION:							     ;
;	Instance the indicated INSTALLED DOS device driver only if it is     ;
;	loaded after higher than the start UMB page else pass it on to the   ;
;	original handler - DOSMGR_Instance_Device.			     ;
;	This service is only valid at Init_Complete device call time.	     ;
;									     ;
;   ENTRY:								     ;
;	ESI -> 8 character device name string				     ;
;	    THIS STRING MUST EXACTLY MATCH THE NAME AS IT WILL BE FOUND	     ;
;	    IN THE DEVICE HEADER IN LOW MEMORY!!!!!!!			     ;
;	    No case conversion is performed (name in dev hdr is upper case)  ;
;	    Length is 8 (name must be space padded as DOS device names are)  ;
;	    NO ':'s!! There is no ':' in the name in the device header.	     ;
;									     ;
;   EXIT:								     ;
;	Carry set if device could not be instanced			     ;
;	    o No device with this name in device list			     ;
;	    o User has overidden call with "GLOBAL=" in system.ini	     ;
;	    o The device does not have an arena associated with it.	     ;
;	Carry clear							     ;
;	    device instanced						     ;
;									     ;
;   USES:								     ;
;	FLAGS								     ;
;									     ;
;   NOTES:								     ;
;	This call applies only to INSTALLED character devices (device	     ;
;	    segment != 70h). It cannot instance devices that are in the DOS  ;
;	    RAM BIOS because there is no way to deliniate their start and    ;
;	    end addresses. It is the job of the DOS instancing to correctly  ;
;	    instance things related to character devices in the DOS RAM BIOS.;
;									     ;
;	This call instances the ENTIRE device. It cannot differentiate code  ;
;	    and data.							     ;
;									     ;
;	This service is available at Init_Complete time only.		     ;
;	    o Service cannot work until DOSMGR_Device_Init is complete.	     ;
;	    o Service is in ICODE segment so it becomes invalid after	     ;
;		Init_Complete.						     ;
;	    o Calls to _AddInstanceItem are invalid after Init_Complete.     ;
;									     ;
;	WARNING!! DO NOT USE THIS SERVICE TO INSTANCE DEVICES ADDED WITH THE ;
;	    DOSMGR_Add_Device SERVICE!!!!! This will not work properly. If   ;
;	    you need to instance one of these devices you must call	     ;
;	    _AddInstanceItem yourself on it, or use the GVDAInstance flag    ;
;	    on your _Allocate_Global_V86_Data_Area call.		     ;
;									     ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Instance_Device,PUBLIC


; find out if the device is in the global device list. If it is then we will
; not try to instance this device. We could actually end this call in this 
; routine, but for the time being will chain it on to DOSMGR.

	call	LoadHi_IsDeviceGlobal	;is it declared to be a global device?
	jc	SHORT LHID_Chain_On	;yes, chain call on.

; find out if the device is loaded high and if so, get its address and length

	call	Get_Device_Info		
	jc	SHORT LHID_Chain_On	;don't know about this, chain call on.

; instance the device

	call	LoadHi_Instance_From_EDX_Size_EAX
	clc				;device has been instanced
	ret

LHID_Chain_On:

; chain this call to DOSMGR_Instance_Device and let it deal with it in what
; ever way it want's to.

	jmp	[ActualDOSMGRInstanceDev]

EndProc LoadHi_Instance_Device
;----------------------------------------------------------------------------;
; LoadHi_Instance_From_EDX_Size_EAX:					     ;
;									     ;
;   DESCRIPTION:						             ;
;	Instances a particular device whose starting address and location    ;
;	are passed in.							     ;
;  									     ;
;   ENTRY:								     ;
;	EDX -  Start of the device.				             ;
;	EAX -  Length of the device.					     ;
;									     ;
;   EXIT:								     ;
;	NONE.								     ;
;									     ;
;   USES:								     ;
;	Flags.							             ;
;----------------------------------------------------------------------------;

BeginProc LoadHi_Instance_From_EDX_Size_EAX,PUBLIC

	pushad				;save all registers

; EDX has the start of the device and EAX has it's size.  Allocate an item for
; the add_instance_item call and fill in the information.

	mov	edi,eax			;size of device
	mov	esi,InstanceDataList	;get the handle for the list
	VMMcall	List_Allocate		;get a new list
	VMMcall	List_Attach		;attach the list on

; fill in the information for this node

	mov	[eax.InstLinkF],0	;initialize
	mov	[eax.InstLinkB],0	;initialize
	mov	[eax.InstLinAddr],edx	;save linear address of device
	mov	[eax.InstSize],edi	;size in bytes of the device
	mov	[eax.InstType],ALWAYS_Field

; call service to instance the data.

	VMMcall	_AddInstanceItem,<eax,0>
	popad				;restore all registers
	ret

EndProc LoadHi_Instance_From_EDX_Size_EAX
;----------------------------------------------------------------------------;
; LoadHi_IsDeviceGlobal:						     ;
;									     ;
;   DESCRIPTION:						             ;
;	Finds out if a passed in device name is in the Global device list    ;
;       or not.								     ;
;  									     ;
;   ENTRY:								     ;
;	ESI -  pointer to device name.	      				     ;
;									     ;
;   EXIT:								     ;
;	CARRY SET: if match obtained in global list.			     ;
;	CARRY CLEAR: otherwise.						     ;
;									     ;
;   USES:								     ;
;	EAX, Flags.						             ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_IsDeviceGlobal,PUBLIC					

	push	esi
	push	edi
	push	ecx
	mov	edi,esi			;get the name pointer

; get a pointer to the first node in the Global device list.

	mov	esi,GlobalDeviceList	;get the list handle
	VMMcall	List_Get_First		;get a pointer to the first node.
	jz	SHORT LHIDG_No_Match 	;empty list, no match found

LHIDG_Match_Name:

	mov	esi,eax			;get a pointer to the node
	cld
	mov	ecx,8			;8 characters to match for
	repe	cmpsb			;do the names match ?
	jz	SHORT LHIDG_Match_Found	;match obtained

; get to the next node.

	mov	esi,GlobalDeviceList	;get the list handle
	VMMcall	List_Get_Next		;get a pointer to the first node.
	jnz	SHORT LHIDG_Match_Name	;continue to match

LHIDG_No_Match:

	clc				;not in global list
	jmp	SHORT LHIDG_Ret

LHIDG_Match_Found:

	stc				;device is in global list

LHIDG_Ret:

	pop	ecx
	pop	edi
	pop	esi			;restore thrashed registers
	ret

EndProc LoadHi_IsDeviceGlobal
;;;;;----------------------------------------------------------------------------;
;;;;;	LoadHi_AGVDA_Hook							 ;
;;;;;										 ;
;;;;;	DESCRIPTION:								 ;
;;;;;	    Hook to fix Allocate_Global_V86_Data_Area so that for page aligned	 ;
;;;;;	    reclaim allocations it will not return an address < 10000h. 	 ;
;;;;;										 ;
;;;;;	ENTRY:									 ;
;;;;;	    As for Allocate_Global_V86_Data_Area				 ;
;;;;;										 ;
;;;;;	EXIT:									 ;
;;;;;	    As for Allocate_Global_V86_Data_Area				 ;
;;;;;										 ;
;;;;;	USES:									 ;
;;;;;	    'C' calling 							 ;
;;;;;										 ;
;;;;;----------------------------------------------------------------------------;
;;;;
;;;;BeginProc LoadHi_AGVDA_Hook
;;;;
;;;;	    enter   0,0
;;;;
;;;;AGVDAnBytes   equ  dword ptr [ebp+8]	    ; Number of bytes
;;;;AGVDAflags	  equ  dword ptr [ebp+12]	    ; Flags
;;;;
;;;;	    push    esi
;;;;	    push    edi
;;;;	    push    ebx
;;;;
;;;;	    VMMCall _GetFirstV86Page
;;;;
;;;;	    cmp     eax,00000010h
;;;;	    jae     short LoadHi_AGV_ChainThrough
;;;;	    mov     esi,AGVDAflags
;;;;	    test    esi,GVDAPageAlign
;;;;	    jz	    short LoadHi_AGV_ChainThrough
;;;;	    test    esi,GVDAReclaim
;;;;	    jz	    short LoadHi_AGV_ChainThrough
;;;;	    and     esi,NOT(GVDAReclaim)	    ; Turn off reclaim
;;;;	    mov     ecx,00000010h
;;;;	    sub     ecx,eax			    ; Off by this many pages
;;;;	    shl     ecx,12			    ; Pages to bytes
;;;;
;;;;	    cCall   [Real_AGVDA_Addr],<ecx,esi>
;;;;
;;;;	    jmp     short LoadHi_ChkRet
;;;;
;;;;LoadHi_AGV_Again:
;;;;
;;;;	    cCall   [Real_AGVDA_Addr],<AGVDAnBytes,esi>
;;;;
;;;;LoadHi_ChkRet:
;;;;	    or	    eax,eax			    ; Error?
;;;;	    jz	    short LoadHi_AGV_Done	    ; Yes, finished
;;;;	    cmp     eax,00010000h
;;;;	    jb	    short LoadHi_AGV_Again
;;;;LoadHi_AGV_Done:
;;;;	    pop     ebx
;;;;	    pop     edi
;;;;	    pop     esi
;;;;	    leave
;;;;	    ret
;;;;
;;;;LoadHi_AGV_ChainThrough:
;;;;	    pop     ebx
;;;;	    pop     edi
;;;;	    pop     esi
;;;;	    leave
;;;;	    jmp     [Real_AGVDA_Addr]
;;;;
;;;;EndProc LoadHi_AGVDA_Hook

;******************************************************************************
;
;   LH_V86_Fix_Proc
;
;   DESCRIPTION:
;	Patch V86MMGR MEM_SCAN behavior
;
;   ENTRY:
;	None
;
;   EXIT:
;	None
;
;   USES:
;	None
;
;==============================================================================


BeginProc LH_V86_Fix_Proc

	test	word ptr [edi],0000001110100000b
	jnz	short SkipPg
	or	word ptr [edi],0000010000000001b
SkipPg:
	ret

EndProc LH_V86_Fix_Proc

;----------------------------------------------------------------------------;
VxD_ICODE_ENDS


VxD_CODE_SEG

;******************************************************************************
;
;   LH_V86_EMM_Fix_Proc
;
;   DESCRIPTION:
;	Hook to V86MMGR EMS driver.
;
;   ENTRY:
;	EBX is VM handle
;	THIS ROUTINE IS JUMPED TO NOT CALLED!!!!
;
;   EXIT:
;	None
;
;   USES:
;	None
;
;==============================================================================

BeginProc LH_V86_EMM_Fix_Proc

	push	ebx
	push	edi
	mov	edi,[V86_CB_Offset_Addr]
	add	ebx,dword ptr [edi]
	mov	ebx,dword ptr [ebx+50h]
	or	ebx,ebx
	jz	short NoBase
	test	byte ptr [ebx+691h],04h
NoBase:
	pop	edi
	pop	ebx
	jnz	short NormCode
	push	ebx
	mov	ebx,esi
	sub	ebx,dword ptr [esp+(1*4).Pushad_ESI]
;;;;	    shr     ebx,3	    ; index of entry
;;;;	    shl     ebx,2	    ; Times 4 4k pages per 16k page
;;;; Combine all of above yields
	shr	ebx,1
	cmp	ebx,0A0h
	pop	ebx
	jae	short NormCode
JNPart:
	jmp	[EMM_Patch_NotPartial_Addr]

NormCode:
	cmp	byte ptr [esi+01],4
	je	short JNPart
	jmp	[EMM_Patch_Ret_Addr]

EndProc LH_V86_EMM_Fix_Proc


;******************************************************************************
;
;   LH_V86_EMM_Fix_Proc2 and 2a
;
;   DESCRIPTION:
;	Hook to V86MMGR EMS driver.
;
;   ENTRY:
;	THIS ROUTINE IS JUMPED TO NOT CALLED!!!!
;
;   EXIT:
;	None
;
;   USES:
;	None
;
;==============================================================================

BeginProc LH_V86_EMM_Fix_Proc2

	push	edi
	mov	edi,[V86_ImprtPtr_Addr]
	mov	edi,dword ptr [edi]
	jmp	[EMM_Patch_Ret_Addr2]

EndProc LH_V86_EMM_Fix_Proc2

BeginProc LH_V86_EMM_Fix_Proc2a

	mov	word ptr [ecx+2],ax
	pop	ecx
	pop	edi
	jmp	[EMM_Patch_Ret_Addr2a]

EndProc LH_V86_EMM_Fix_Proc2a

;----------------------------------------------------------------------------;
; LoadHi_Sys_VM_Init:							     ;
;									     ;
; Takes the instance snap shot and releases all the list memory that was used;
; in the initialization phase.						     ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Sys_VM_Init,PUBLIC

; prepare the snap shot for the instance buffers

	call	LoadHi_Take_Instance_SnapShot

; release all initialization list memories.

	mov	esi,GlobalDeviceList	;list used to store 'GLOBAL=' devices
	VMMcall	List_Destroy
	mov	esi,InstanceDataList	;list used to allocate instance data
	VMMcall	List_Destroy

	clc				;call successful
	ret

EndProc LoadHi_Sys_VM_Init
;----------------------------------------------------------------------------;
; Loadhi_Int21ISR:							     ;
;									     ;
; Traps the SetUMBLink INT 21H call and returns failure if the call tries to ;
; link the UMBs in.							     ;
;									     ;
;   ENTRY:								     ;
;	EAX = Interrupt # (21h)						     ;
;	EBX = VM handle							     ;
;									     ;
;   EXIT:								     ;
;									     ;
;   USES:								     ;
;	All registers and flags						     ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_Int21ISR, High_Freq,PUBLIC

	cmp	[ebp.Client_AX],5803h	;Set Link state call ?
	jnz	SHORT LoadHi_Int21Chain ;no.
	cmp	[ebp.Client_BX],1	;Link UMB's in ?
	jnz	SHORT LoadHi_Int21Chain ;no.
	cmp	LoadHi_OwnCall,-1	;from ourselves ?
	jz	SHORT LoadHi_Int21Chain ;yes.

; fail the set link call.

	or	[ebp.Client_Flags], CF_Mask
	mov	[ebp.Client_AX],1	;invalid function (at this time)
	clc				;Don't reflect this int
	ret

LoadHi_Int21Chain:

	stc				;chain interrupt on
	ret

EndProc LoadHi_Int21ISR 
;----------------------------------------------------------------------------;
; LoadHi_SystemExit:							     ;
;									     ;
; This is called at system exit time and the UMB link information is reset.  ;
;----------------------------------------------------------------------------;
BeginProc LoadHi_SystemExit,PUBLIC

	Push_Client_State
	VMMcall Begin_Nest_Exec 	;Get ready for software ints

; reset the link state.

	mov	LoadHi_OwnCall,-1	;we should nor trap it ourselves
	mov	[ebp.Client_AX], 05803h ;get UMB link state
	mov	ebx,UMBLinkState	;get the saved state
	and 	bx,0ffh			;only BL is significant
	mov	[ebp.Client_BX],bx	;save the state to restore
	mov	eax, 21h		;INT 21H to be done
	VMMcall Exec_Int		;do the INT
	mov	LoadHi_OwnCall,0	;just to make it clean.

; end the nested exec calls.

	VMMcall End_Nest_Exec
	Pop_Client_State

	clc				;call successful
	ret

EndProc LoadHi_SystemExit
;----------------------------------------------------------------------------;
;******************************************************************************
;			    S E R V I C E S
;******************************************************************************

BeginProc VDD2_Get_Version, Service

; if there is atleast one UMB in the mono display area then we will return
; success on this call.

	cmp	MonoDispAreaHasUMB,0	;is there any UMB in MONO area ?
	jnz	SHORT @f		;yes, return MONO disp VXD exits.
	xor	eax,eax
	xor	esi,esi	
	stc				;no secondary display VXD
	ret
@@:
	mov	esi,OFFSET32 LoadHi_Device_Name
	mov	eax, 100h
	clc
	ret

EndProc VDD2_Get_Version

;******************************************************************************
;		     S E R V I C E   H O O K S
;******************************************************************************

;----------------------------------------------------------------------------;
; LoadHi_TeastGlobalV86Mem:					 	     ;
;									     ;
; Entry:								     ;
;	unsigned	LH_TGVLinAdd	(linear address)  		     ;
;	unsigned	LH_TGVnBytes	(length of address in bytes)	     ;
;	unsigned	LH_TGVflags					     ;
;									     ;
; Action:								     ;
;	. If the Linear address is above 0A0000H, this routine returns a 1   ;
;	  in EAX to imply that the address is global, else it will chain     ;
;	  the call on to the original service handler.			     ;
;									     ;
; Exit:									     ;
;	. EAX = 1 if the linear address is above 0A0000H, else it chains the ;
;	  call along.							     ;
;									     ;
; Uses:									     ;
;----------------------------------------------------------------------------;

BeginProc LoadHi_TestGlobalV86Mem,PUBLIC

	enter	0,0

	LH_TGVLinAdd equ	dword ptr [ebp+8]
	LH_TGVnBytes equ	dword ptr [ebp+12]
	LH_TGVflags  equ	dword ptr [ebp+16]

	mov	eax,FirstUMBPage	;get the first UMB page
	shl	eax,12			;convert into a linear address
	cmp	LH_TGVLinAdd,eax	;in UMB range ?
	jae	SHORT LH_TGVGlobalAdd	;yes.
	leave
	jmp	[ActualTGV86Mem]	;chain it on

LH_TGVGlobalAdd:

	mov	eax,1			;address is globa (in UMB range)
	leave
	ret

EndProc LoadHi_TestGlobalV86Mem
;******************************************************************************
;	       D E V I C E   C O N T R O L   H A N D L E R S
;******************************************************************************


;******************************************************************************
;			L O C A L   P R O C E D U R E S
;******************************************************************************


;******************************************************************************
;		       D E B U G G I N G   C O D E
;******************************************************************************


IFDEF DEBUG

ENDIF

VxD_CODE_ENDS


END LoadHi_Real_Mode_Init
