;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1990
;
;   Title:	BLOCKDEV.INC -- Services and equates for BlockDev VxD
;
;   Version:	3.10
;
;   Date:	23-Aug-1990
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   23-Aug-1990 RAL Original
;
;==============================================================================

;
;   Service Declarations
;
Begin_Service_Table BlockDev

BlockDev_Service    BlockDev_Get_Version, LOCAL
BlockDev_Service    BlockDev_Register_Device, LOCAL
BlockDev_Service    BlockDev_Find_Int13_Drive, LOCAL
BlockDev_Service    BlockDev_Get_Device_List, LOCAL
BlockDev_Service    BlockDev_Send_Command, LOCAL
BlockDev_Service    BlockDev_Command_Complete, LOCAL
BlockDev_Service    BlockDev_Synchronous_Command, LOCAL

End_Service_Table   BlockDev


;
;   Block Device Desctiptor data structrue
;

BD_Priv_Data_Size	EQU	30h

BD_Major_Version	EQU	3
BD_Minor_Version	EQU	0Ah

BlockDev_Device_Descriptor STRUC
BDD_Next		dd	?
BDD_BD_Major_Ver	db	BD_Major_Version
BDD_BD_Minor_Ver	db	BD_Minor_Version
BDD_Device_Type 	db	?
BDD_Int_13h_Number	db	?
BDD_Flags		dd	?
BDD_Name_Ptr		dd	?
BDD_Max_Sector		dq	?
BDD_Sector_Size 	dd	?
BDD_Num_Heads		dd	?
BDD_Num_Cylinders	dd	?
BDD_Num_Sec_Per_Track	dd	?
BDD_Sync_Cmd_Proc	dd	?
BDD_Command_Proc	dd	?
BDD_Hw_Int_Proc 	dd	0
BDD_Reserved_BlockDev	db	BD_Priv_Data_Size dup (?)
BlockDev_Device_Descriptor ENDS


;
;   Flags for BDD_Flags field
;
BDF_Int13_Drive 	EQU	00000001b
BDF_Int13_Drive_Bit	EQU	0
BDF_Writeable		EQU	00000010b
BDF_Writeable_Bit	EQU	1
BDF_Removable		EQU	00000100b
BDF_Removable_Bit	EQU	2
BDF_Remote		EQU	00001000b
BDF_Remote_Bit		EQU	3
BDF_Serial_Cmd		EQU	00010000b
BDF_Serial_Cmd_Bit	EQU	4
BDF_Cache		EQU	00100000b
BDF_Cache_Bit		EQU	5


;
;   Device types for BDD_Device_Type field
;
BDT_360K_5_Inch_Floppy	EQU	00h
BDT_1200K_5_Inch_Floppy EQU	01h
BDT_720K_3_Inch_Floppy	EQU	02h
BDT_Single_Dens_8_Inch	EQU	03h
BDT_Double_Dens_8_Inch	EQU	04h
BDT_Fixed_Disk		EQU	05h
BDT_Tape_Drive		EQU	06h
BDT_Other		EQU	07h


;
;   Command Block for Block Devices
;
BlockDev_Command_Block	STRUC
BD_CB_Next		dd	?
BD_CB_Command		dw	?
BD_CB_Cmd_Status	dw	?
BD_CB_Flags		dd	?
BD_CB_Cmd_Cplt_Proc	dd	?
BD_CB_Sector		dq	?
BD_CB_Count		dd	?
BD_CB_Buffer_Ptr	dd	?
BD_CB_Reserved_Client	dd	?
BD_CB_Reserved_BlockDev dd	?
BD_CB_Reserved_FastDisk dd	?
BlockDev_Command_Block	ENDS


;
;   Commands
;   NOTE:  Commands 8000h-FFFFh are reserved for device specific commands
;
BDC_Read		EQU	0
BDC_Write		EQU	1
BDC_Verify		EQU	2
BDC_Cancel		EQU	3


;
;   Equates for command status. All codes below 10h imply successful completion
;   Error code value are > 10h
;
BDS_Success		  EQU	0		; Completed successfully
BDS_Success_With_Retries  EQU	1		; Completed successfully after retries
BDS_Success_With_ECC	  EQU	2		; Successful after error correction

BDS_First_Error_Code	  EQU	10h		; first error code value
BDS_Invalid_Sector_Number EQU	10h		; Invalid sector number
BDS_Canceled		  EQU	11h		; Command was canceled
BDS_Cmd_In_Progress	  EQU	12h		; Can't cancel cmd in progress
BDS_Invalid_Cmd_Ptr	  EQU	13h		; Cancel of invalid cmd pointer
BDS_Media_Error		  EQU	14h		; Read/Write failed
BDS_Device_Error	  EQU 	15h		; Device/Adapter failed
BDS_Invalid_Command	  EQU	16h


;
;   Flags for commands
;
BDCF_High_Priority	EQU	00000001b
BDCF_High_Priority_Bit	EQU	0
BDCF_Scatter_Gather	EQU	00000010b
BDCF_Scatter_Gather_Bit EQU	1
BDCF_Dont_Cache 	EQU	00000100b
BDCF_Dont_Cache_Bit	EQU	2


;
;   Equates for synchronous commands
;
BD_SC_Get_Version	EQU	0000h


;
;   Equates for error returns from synchronous command
;
BD_SC_Err_Invalid_Cmd	EQU	0001h


;
;   Value specified in CX register when API call-out Int 2Fh executed
;   The Hw_Detect_Start and End APIs are used by block devices to notify
;   TSR and DOS device driver software that they are performing hardware
;   detection.	This may, for example, disable a write-behind cache.
;
BlockDev_API_Hw_Detect_Start	EQU	0001h
BlockDev_API_Hw_Detect_End	EQU	0002h
;
;   The Int13_Chain_Check API is executed by Int 13h block devices to check
;   to see if the software that has hooked DOS's ROM BIOS int 13h address
;   is aware of BlockDev and it is OK to load block device drivers.  If it
;   is OK for block devices such as WDCTRL to load even though the DOS Int 13h
;   chain has been modified, the hook code should zero the CX register and
;   iret from this call.
;
BlockDev_API_Int13_Chain_Check	EQU	0003h
