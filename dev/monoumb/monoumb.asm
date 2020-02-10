PAGE 58,132
;******************************************************************************
TITLE MonoUMB -- Dummy VDD2 device to fool old VDDs
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1992
;
;   Title:	MonoUMB -- Dummy VDD2 device to fool old (3.00) VDDs into
;			   not claiming the MONOChrome adapter region.
;			   This allows a UMB in the MONOChrome region
;			   for LIM/UMBulators (like EMM386) that do not
;			   load the LoadHi VxD on WIN386 versions >= 3.10
;			   because they specify the UMBs using only the
;			   Paging Import to the V86MMGR.
;
;   Version:	1.00
;
;   Date:	07-Jan-1992
;
;   Author:	ARR
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;    01-07-1992 ARR Original
;
;==============================================================================
;
;   DESCRIPTION: See Title ABove
;
;******************************************************************************

	.386p


;******************************************************************************
;			      I N C L U D E S
;******************************************************************************



	.XLIST
	INCLUDE VMM.Inc
	.LIST

	Create_VDD2_Service_Table EQU TRUE
	INCLUDE .\VDD2.Inc
	MonoUMB_Service_Table EQU VDD2_Service_Table
	Num_MonoUMB_Services EQU Num_VDD2_Services

;******************************************************************************
;		 V I R T U A L	 D E V I C E   D E C L A R A T I O N
;******************************************************************************

Declare_Virtual_Device MonoUMB, 1, 0, MonoUMB_Control, VDD2_Device_ID, Undefined_Init_Order

;******************************************************************************
;				 E Q U A T E S
;******************************************************************************

;******************************************************************************
;			     S T R U C T U R E S
;******************************************************************************

;******************************************************************************
;			   F L A G   E Q U A T E S
;******************************************************************************


;******************************************************************************
;		   I N I T I A L I Z A T I O N	 D A T A
;******************************************************************************

VxD_IDATA_SEG

; define a copyright string

MS_CopyRight		db	'(C) Copyright MICROSOFT Corp., 1992',0
MonoUMB_Device_Name	 db	'MonoUMB',0,0

VxD_IDATA_ENDS

;******************************************************************************
;			    L O C A L	D A T A
;******************************************************************************

VxD_DATA_SEG

VxD_DATA_ENDS



;******************************************************************************
;	      R E A L	M O D E   I N I T I A L I Z A T I O N
;******************************************************************************

VxD_REAL_INIT_SEG

;******************************************************************************
;
;   MonoUMB_Real_Mode_Init
;
;   DESCRIPTION:
;
;	The VxD will not load if this is a duplicate load.
;
;   ENTRY:
;	CS, DS = Real mode segment
;	AX = VMM version (AH=Major, AL=Minor)
;	BX = Flags
;
;   EXIT:
;	BX = Null pointer (no pages to exclude)
;	SI = Null pointer (no instance data)
;	EDX = 0
;
;   USES:
;	AX, BX, DX, SI, Flags
;
;==============================================================================

BeginProc MonoUMB_Real_Mode_Init

; check for duplicate device load.

	test	bx, Duplicate_From_INT2F OR Duplicate_Device_ID
	jnz	SHORT MU_RMI_Fail_Load
MU_RMI_Loading_Ok:
	mov	ax, Device_Load_Ok
	jmp	SHORT MU_RMI_Exit

MU_RMI_Fail_Load:

; fail this device load without any error message.

	mov	ax, Abort_Device_Load + No_Fail_Message
MU_RMI_Exit:
	xor	bx,bx				;no pages to exclude
	xor	si,si				;no instance data
	xor	edx,edx 			;Ref data 0
	ret

EndProc MonoUMB_Real_Mode_Init

VxD_REAL_INIT_ENDS

;******************************************************************************
;	       D E V I C E   C O N T R O L   P R O C E D U R E
;******************************************************************************

VxD_CODE_SEG

;******************************************************************************
;
;   MonoUMB_Control
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

BeginProc MonoUMB_Control,PUBLIC

	clc					; Ignore other control calls
	ret

EndProc MonoUMB_Control

;----------------------------------------------------------------------------;
; VDD2_Get_Version:							     ;
;									     ;
;   DESCRIPTION:							     ;
;	MonoUMB VxD acts like a secondary VDD. This prevents the VGA and     ;
;	EGA VDDs from getting upset about the fact that someone has	     ;
;	grabbed the ownership of the MONO video pages.			     ;
;									     ;
;   ENTRY:								     ;
;	None								     ;
;									     ;
;   EXIT:								     ;
;	If UMBs in MONO region, carry clear				     ;
;	    EAX != 0							     ;
;	    ESI -> device name string					     ;
;	else, carry set 						     ;
;	    EAX = ESI = 0						     ;
;									     ;
;   USES:								     ;
;	FLAGS,EAX,ESI							     ;
;									     ;
;----------------------------------------------------------------------------;
BeginProc VDD2_Get_Version, Service

	mov	esi,OFFSET32 MonoUMB_Device_Name
	mov	eax, 200h
	clc
	ret

EndProc VDD2_Get_Version

VxD_CODE_ENDS

END MonoUMB_Real_Mode_Init
