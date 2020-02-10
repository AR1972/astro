;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1988-1991
;
;   Title:	V86MMGR.Inc - Public services for V86MMGR
;
;   Version:	2.00
;
;   Date:	29-Nov-88
;
;   Author:	ARR
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   29-Nov-1988 ARR Original
;   26-Apr-1989 RAL New mapper services
;
;==============================================================================

Begin_Service_Table V86MMGR

V86MMGR_Service 	V86MMGR_Get_Version,		LOCAL
V86MMGR_Service 	V86MMGR_Allocate_V86_Pages,	LOCAL
V86MMGR_Service 	V86MMGR_Set_EMS_XMS_Limits,	LOCAL
V86MMGR_Service 	V86MMGR_Get_EMS_XMS_Limits,	LOCAL
;
;   Services used for API mapping.
;
V86MMGR_Service 	V86MMGR_Set_Mapping_Info, VxD_ICODE
V86MMGR_Service 	V86MMGR_Get_Mapping_Info
V86MMGR_Service 	V86MMGR_Xlat_API
V86MMGR_Service 	V86MMGR_Load_Client_Ptr
V86MMGR_Service 	V86MMGR_Allocate_Buffer
V86MMGR_Service 	V86MMGR_Free_Buffer
V86MMGR_Service 	V86MMGR_Get_Xlat_Buff_State
V86MMGR_Service 	V86MMGR_Set_Xlat_Buff_State
V86MMGR_Service 	V86MMGR_Get_VM_Flat_Sel
V86MMGR_Service 	V86MMGR_Map_Pages
V86MMGR_Service 	V86MMGR_Free_Page_Map_Region

End_Service_Table V86MMGR

;
; BITS of ECX flags for V86MMGR_Allocate_V86_Pages
;
AV86PLocked			EQU	00000000000000000000000000000001B
AV86PLockedBit			EQU	0	; VM memory is to be
						;   ALWAYS LOCKED regardless of
						;   pager type or whether VM
						;   is suspended.
;
; BITS of ECX flags for V86MMGR_Set_EMS_XMS_Limits
;
; NOTE: If neither Limit_DisableHMA or Limit_EnableHMA is set, the HMA
;	state is not altered.
;
EMS_XMS_Limit_DisableHMA	EQU	00000000000000000000000000000001B
EMS_XMS_Limit_DisableHMABit	EQU	0
EMS_XMS_Limit_EnableHMA 	EQU	00000000000000000000000000000010B
EMS_XMS_Limit_EnableHMABit	EQU	1
EMS_XMS_Limit_XMS_Is_Locked	EQU	00000000000000000000000000000100B
EMS_XMS_Limit_XMS_Is_LockedBit	EQU	2
EMS_XMS_Limit_EMS_Is_Locked	EQU	00000000000000000000000000001000B
EMS_XMS_Limit_EMS_Is_LockedBit	EQU	3



;
;   API mapper equates and macros
;

Xlat_Exec_Int	    EQU     000h
Xlat_Fixed_Len	    EQU     001h
Xlat_Var_Len	    EQU     002h
Xlat_Calc_Len	    EQU     003h
Xlat_ASCIIZ	    EQU     004h
Xlat_Jmp_To_Proc    EQU     005h
Xlat_Return_Ptr     EQU     006h
Xlat_Return_Seg     EQU     007h


Xlat_API_Exec_Int MACRO Int_Number
	db	Xlat_Exec_Int
	db	Int_Number
	ENDM

Xlat_API_Fixed_Len MACRO Ptr_Seg, Ptr_Off, Length
	db	Xlat_Fixed_Len
	dw	Length
	dw	(Client_&Ptr_Seg*100h)+Client_&Ptr_Off
	ENDM

Xlat_API_Var_Len MACRO Ptr_Seg, Ptr_Off, Len_Reg
	db	Xlat_Var_Len
	db	Client_&Len_Reg
	dw	(Client_&Ptr_Seg*100h)+Client_&Ptr_Off
	ENDM

Xlat_API_Calc_Len MACRO Ptr_Seg, Ptr_Off, Calc_Proc_Addr
	db	Xlat_Calc_Len
	dd	OFFSET32 Calc_Proc_Addr
	dw	(Client_&Ptr_Seg*100h)+Client_&Ptr_Off
	ENDM

Xlat_API_ASCIIZ  MACRO Ptr_Seg, Ptr_Off
	db	Xlat_ASCIIZ
	dw	(Client_&Ptr_Seg*100h)+Client_&Ptr_Off
	ENDM

Xlat_API_Jmp_To_Proc MACRO Proc_Name
	db	Xlat_Jmp_To_Proc
	dd	OFFSET32 Proc_Name
	ENDM


Xlat_API_Return_Ptr MACRO Ptr_Seg, Ptr_Off
	db	Xlat_Return_Ptr
	dw	(Client_&Ptr_Seg*100h)+Client_&Ptr_Off
	ENDM

Xlat_API_Return_Seg MACRO Ptr_Seg
	db	Xlat_Return_Seg
	db	Client_&Ptr_Seg
	ENDM
