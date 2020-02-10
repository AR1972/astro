;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1989-1990
;
;   Title:	INT2FAPI.INC - Windows/386 V86 Application Program Interface
;
;   Version:	3.00
;
;   Date:	10-Mar-1989
;
;   Author:	RAL
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   10-Mar-1989 RAL Original for 3.0
;   07-Apr-1989 RAL Added device broadcast equate
;
;==============================================================================
;
;   For inforamtion on these APIs please refer to the Windows/386 DDK
;   appendix on the Int 2Fh Application Program Interface.
;
;------------------------------------------------------------------------------

;
;   Interrupt 2Fh is used for Windows/386 API calls.
;
W386_API_Int		EQU	2Fh

;
;   All Windows/386 API Int 2Fh calls must be issued with AH = 16h
;
W386_Int_Multiplex	EQU	16h

;
;   Values for AL for all Windows/386 API calls
;
W386_Get_Version	EQU	00h		; Install check/Get version
W386_Old_Get_VMID_API	EQU	02h		; Version 2.xx get VMID API call
W386_Startup		EQU	05h		; Broadcast when Win386 starting
W386_Exit		EQU	06h		; Broadcast when Win386 exited
W386_Device_Broadcast	EQU	07h		; Broadcast by virtual device
W386_Startup_Complete	EQU	08h		; Broadcast when Win386 start is complete
W386_Begin_Exit 	EQU	09h		; Broadcast when Win386 is starting
						;   a NORMAL exit sequence
W386_Windows_ID 	EQU	0Ah		; Identify windows ver/type
W386_TSR_Identify	EQU	0Bh		; Identify TSRs
W386_ROM_Detect 	EQU	0Ch		; Used by ROM win to detect ROMs

W386_Release_Time	EQU	80h		; Release cur VM's time-slice
W386_Begin_Critical	EQU	81h		; Begin critical section
W386_End_Critical	EQU	82h		; End critical section
W386_Get_Cur_VMID	EQU	83h		; Returns BX = ID of current VM
W386_Get_Device_API	EQU	84h		; Returns ES:DI -> Device API
W386_Switch_And_Call	EQU	85h		; Change VMs and call-back
W386_Test_Int31_Avail	EQU	86h		; Returns AX=0 if Int 31 avail
W386_Get_PM_Switch_Addr EQU	87h		; Get call-back addr for PM
W386_Get_LDT_Base_Sel	EQU	88h		; Get selector to LDT
W386_Win_Kernel_Idle	EQU	89h		; Windows kernel idle call
W386_DPMI_Extension	EQU	8Ah		; DPMI extension Int 2Fh
W386_Set_Focus		EQU	8Bh		; Set focus to specified VM
W386_Restart_Cmd	EQU	8Ch		; Win.Com execs specified app

;
;   Structure for real mode device initialization API.
;
Win386_Startup_Info_Struc STRUC
SIS_Version		db	3, 0		; Structure version
SIS_Next_Ptr		dd	?		; Seg:Off of next dev in list
SIS_Virt_Dev_File_Ptr	dd	0		; Ptr to ASCIZ file name to load
SIS_Reference_Data	dd	?		; Data to be passed to device
SIS_Instance_Data_Ptr	dd	0		; Ptr to instance data list
Win386_Startup_Info_Struc ENDS

;
;   Structure for instance data list.  (List terminated with 0 dword).
;
Instance_Item_Struc STRUC
IIS_Ptr 		dd	?		; Seg:Off of instance item
IIS_Size		dw	?		; Size of instance item in bytes
Instance_Item_Struc ENDS

;
;   Flags passed to the Win_Kernel_Idle call to indicate state of Windows
;   in the BX register.
;
Win_Idle_Mouse_Busy	EQU	00000001b
Win_Idle_Mouse_Busy_Bit EQU	0

;
; Structure for TSR <-> Windows communication
; (W386_TSR_Identify call, AL=0Bh)
;
TSR_Info_Struc	STRUC
    TSR_Next		    dd	?
    TSR_PSP_Segment	    dw	?
    TSR_API_Ver_ID	    dw	100h
    TSR_Exec_Flags	    dw	0
    TSR_Exec_Cmd_Show	    dw	0
    TSR_Exec_Cmd	    dd	0
    TSR_Reserved	    db	4 dup (0)
    TSR_ID_Block	    dd	0
    TSR_Data_Block	    dd	0
TSR_Info_Struc ENDS

;
; TSR_Exec_Flags equates
;
TSR_WINEXEC	EQU	1
TSR_LOADLIBRARY EQU	2
TSR_OPENDRIVER	EQU	4
