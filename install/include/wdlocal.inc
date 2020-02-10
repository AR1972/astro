;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990
;
;   Title:	WDLOCAL.INC -- Equates and structures for WDCTRL.386
;
;   Version:	1.00
;
;   Date:	27-Sep-1990
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   27-Sep-1990 RAL
;
;==============================================================================


;
;   Private data structure for Block Device Descriptor
;

WDCtrl_Private_BDD	STRUC
			db	SIZE BlockDev_Device_Descriptor dup (?)
BDP_Base_IO_Port	dw	?
BDP_Status_Port 	dw	?
BDP_Write_Precom	db	?
BDP_Drive_Control	db	?
WDCtrl_Private_BDD	ENDS



;******************************************************************************
;			       E Q U A T E S
;******************************************************************************

WDIO_Def_Base_Port	EQU	1F0h


WDIO_Data_Off		EQU	0
WDIO_Error_Off		EQU	1
WDIO_Precomp_Off	EQU	1
WDIO_Sector_Cnt_Off	EQU	2
WDIO_Sector_Num_Off	EQU	3
WDIO_Cly_Low_Off	EQU	4
WDIO_Cly_High_Off	EQU	5
WDIO_Drive_Sel_Off	EQU	6
WDIO_Status_Off 	EQU	7
WDIO_Command_Off	EQU	7
WDIO_Alt_Stat_Off	EQU	3F6h-WDIO_Def_Base_Port
WDIO_Drive_Control_Off	EQU	3F6h-WDIO_Def_Base_Port
WDIO_Drive_Address_Off	EQU	3F7h-WDIO_Def_Base_Port


WDStat_Error		EQU	00000001b
WDStat_Index		EQU	00000010b
WDStat_ECC_Corrected	EQU	00000100b
WDStat_DRQ		EQU	00001000b
WDStat_Seek_Complete	EQU	00010000b
WDStat_Write_Fault	EQU	00100000b
WDStat_Ready		EQU	01000000b
WDStat_Busy		EQU	10000000b



;
;   Largest legal sector count for one transfer
;
WD_Max_Sector_Xfer	EQU	7Fh



;
;   Version numbers
;
WD_Major_Ver		EQU	03h
WD_Minor_Ver		EQU	0Ah



;
;   Reference data flags
;
RF_Drive_80h_Ours	EQU	00000001b
RF_Drive_80h_Ours_Bit	EQU	0
RF_Drive_81h_Ours	EQU	00000010b
RF_Drive_81h_Ours_Bit	EQU	1
RF_Use_Alt_Stat_80	EQU	00000100b
RF_Use_Alt_Stat_80_Bit	EQU	2
RF_Use_Alt_Stat_81	EQU	00001000b
RF_Use_Alt_Stat_81_Bit	EQU	3


;
;   Time-out values (in Ms)
;
WD_Write_Ready_Timeout	EQU	500
WD_Busy_Timeout 	EQU	500


;
;   Fatal controller error numbers
;
FATAL_TIME_OUT          EQU     1
FATAL_BAD_STATUS        EQU     2
FATAL_DATA_BAD_COMPARE  EQU     3


;
;   Maximum number of retries for an operation before it is failed
;
WD_Max_Retries		EQU	3
