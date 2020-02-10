;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990
;
;   Title:	INT13.INC - Services for Int13.386 virtual device
;
;   Version:	1.00
;
;   Date:	30-Sep-1990
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   30-Sep-1990 RAL
;
;==============================================================================


Begin_Service_Table Int13

Int13_Service	    Int13_Get_Version, LOCAL
Int13_Service	    Int13_Device_Registered, LOCAL
Int13_Service	    Int13_Translate_VM_Int, LOCAL
Int13_Service	    Int13_Hooking_BIOS_Int, LOCAL
Int13_Service	    Int13_Unhooking_BIOS_Int, LOCAL

End_Service_Table   Int13



;
;   Structure defines the fixed disk parameter tables used by the BIOS Int 13h.
;
Fixed_Disk_Parameter_Table STRUC
FDPT_Max_Cyl		dw	?
FDPT_Max_Heads		db	?
			dw	?
FDPT_Write_Precom_Cyl	dw	?
FDPT_Max_ECC_Burst	db	?
FDPT_Drive_Control	db	?
			db	3 dup (?)
FDPT_Landing_Zone_Cyl	dw	?
FDPT_Sec_Per_Track	db	?
			db	?
Fixed_Disk_Parameter_Table ENDS


;
;   Fixed Disk Drive Status Code Equates
;
I13Stat_Success 	EQU	00h
I13Stat_Bad_Command	EQU	01h
I13Stat_Mark_Not_Found	EQU	02h
I13Stat_Sec_Not_Found	EQU	04h
I13Stat_Reset_Failed	EQU	05h
I13Stat_Params_Failed	EQU	07h
I13Stat_64K_DMA_Error	EQU	09h
I13Stat_Bad_Block	EQU	0Ah
I13Stat_Fatal_ECC_Error EQU	10h
I13Stat_ECC_Corrected	EQU	11h
I13Stat_Ctrl_Failed	EQU	20h
I13Stat_Seek_Failed	EQU	40h
I13Stat_Drive_Failed	EQU	80h
I13Stat_Not_Ready	EQU	0AAh
I13Stat_Undefined_Error EQU	0BBh
I13Stat_Write_Fault	EQU	0CCh
I13Stat_Sense_Failed	EQU	0FFh
