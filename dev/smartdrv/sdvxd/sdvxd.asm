PAGE 58,132
;******************************************************************************
TITLE SDVxD.ASM - VxD to display error message for SmartDrv.Exe
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1991
;
;   Title:	SDVxD.ASM - VxD to display error message for SmartDrv.Exe
;
;   Version:	1.00
;
;   Date:	22-Nov-1991
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   22-Nov-1991 RAL Original
;
;==============================================================================

	.386p


;******************************************************************************
;			      I N C L U D E S
;******************************************************************************

	.XLIST
	INCLUDE VMM.Inc
	INCLUDE Debug.Inc
	INCLUDE SHELL.Inc
	.LIST

;******************************************************************************
;		 V I R T U A L	 D E V I C E   D E C L A R A T I O N
;******************************************************************************

Declare_Virtual_Device SDVXD, 3, 0, SDVxD_Control


;******************************************************************************
;				 E Q U A T E S
;******************************************************************************

;******************************************************************************
;			     S T R U C T U R E S
;******************************************************************************


;******************************************************************************
;		   I N I T I A L I Z A T I O N	 D A T A
;******************************************************************************


;******************************************************************************
;			    L O C A L	D A T A
;******************************************************************************

VxD_DATA_SEG

EXTRN SDVxD_Error_Title_Msg:BYTE
EXTRN SDVxD_Write_Error_Msg:BYTE
EXTRN SDVxD_Write_Drive_Letter:BYTE


SD_Ref_Data_Ptr 	dd	?
SD_Orig_Msg_CSIP	dd	0	    ; 0 indicates not initialized

VxD_DATA_ENDS


;******************************************************************************
;	       D E V I C E   C O N T R O L   P R O C E D U R E
;******************************************************************************

VxD_CODE_SEG

;******************************************************************************
;
;   SDVxD_Control
;
;   DESCRIPTION:
;	This is SDVxD's control procedure.
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

BeginProc SDVxD_Control

	Control_Dispatch Init_Complete, SDVxD_Init_Complete
	Control_Dispatch System_Exit, <SHORT SDVxD_System_Exit>

	clc					; Ignore other control calls
	ret

EndProc SDVxD_Control

VxD_CODE_ENDS


;******************************************************************************
;		    I N I T I A L I Z A T I O N   C O D E
;******************************************************************************

VxD_ICODE_SEG


;******************************************************************************
;
;   SDVxD_Init_Complete
;
;   DESCRIPTION:
;	Initializes real mode SDVxD's error call-back to point to a V86
;	call-back address.
;
;   ENTRY:
;	EDX = Reference data (ptr to dword to patch to call-back break point)
;
;   EXIT:
;	Carry clear
;
;   USES:
;
;
;==============================================================================

BeginProc SDVxD_Init_Complete

	movzx	eax, dx
	shr	edx, 16
	shl	edx, 4
	add	edx, eax

	mov	[SD_Ref_Data_Ptr], edx

	mov	esi, OFFSET32 SDVxD_Error_Msg_Call_Back
	VMMcall Allocate_V86_Call_Back
IFDEF DEBUG
	jnc	SHORT SD_IC_Got_CB
	Debug_Out "WARNING:  Unable to allocate call-back address for SDVxD.  VxD not loading."
	stc
	jmp	SHORT SDVxD_IC_Exit
SD_IC_Got_CB:
ELSE
	jc	SHORT SDVxD_IC_Exit
ENDIF

	xchg	eax, DWORD PTR [edx]
	mov	[SD_Orig_Msg_CSIP], eax

	clc
SDVxD_IC_Exit:
	ret

EndProc SDVxD_Init_Complete



VxD_ICODE_ENDS

;******************************************************************************
;	       D E V I C E   C O N T R O L   H A N D L E R S
;******************************************************************************

VxD_CODE_SEG

;******************************************************************************
;
;   SDVxD_System_Exit
;
;   DESCRIPTION:
;
;   ENTRY:
;
;   EXIT:
;
;   USES:
;
;==============================================================================

BeginProc SDVxD_System_Exit

	xor	ecx, ecx
	xchg	[SD_Orig_Msg_CSIP], ecx
	jecxz	SD_SE_Done
	mov	edi, [SD_Ref_Data_Ptr]
	mov	DWORD PTR [edi], ecx

SD_SE_Done:
	clc
	ret

EndProc SDVxD_System_Exit


;******************************************************************************
;			L O C A L   P R O C E D U R E S
;******************************************************************************

;******************************************************************************
;
;   SDVxD_Error_Msg_Call_Back
;
;   DESCRIPTION:
;
;   ENTRY:
;
;   EXIT:
;
;   USES:
;
;==============================================================================

BeginProc SDVxD_Error_Msg_Call_Back

	mov	ecx, (Block_Svc_Ints OR Block_Enable_Ints)
	VMMcall Begin_Critical_Section

	mov	al, [ebp.Client_AL]
	add	al, "A"
	mov	[SDVxD_Write_Drive_Letter], al

	mov	edi, OFFSET32 SDVxD_Error_Title_Msg
	mov	eax, MB_SYSTEMMODAL OR MB_ASAP OR MB_ICONEXCLAMATION OR MB_OK
	mov	ecx, OFFSET32 SDVxD_Write_Error_Msg

	VxDcall SHELL_SYSMODAL_Message

	VMMcall End_Critical_Section
	VMMjmp	Simulate_Far_Ret

EndProc SDVxD_Error_Msg_Call_Back


;******************************************************************************
;		       D E B U G G I N G   C O D E
;******************************************************************************

VxD_CODE_ENDS

	END
