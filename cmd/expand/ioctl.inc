;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1988-1991
;   Microsoft Confidential
;   All Rights Reserved.
;
;   Title:	smartdrv.inc - smartdrv equates & structures
;
;   Version:	1.00
;
;   Date:	15-Sep-1989
;
;   Author:	ARR
;
;------------------------------------------------------------------------------
;
;   Change log:
;
;      DATE	REV		    DESCRIPTION
;   ----------- --- -----------------------------------------------------------
;   15-Sep-1989 ARR Original
;   30-Oct-1989 MC  Borrowed from AAR for use in win.com. Pared down to only
;                   SD_IOCTL_Read because thats all I need. I need only look
;                   at the size of SD. I do not have to shrink it.
;   11-Dec-1989 MC  Add needed struc's and support for talking to LIM drivers
;
;==============================================================================
;
;
; Device name of SMARTDRV
;
SD_DEV_NAME	equ	"SMARTAAR",0    ; Name for SD driver IOCTL opens.
EMM_DEV_NAME    equ     "EMMXXXX0",0    ; Name for EMM driver IOCTL opens.
;
;
; Structure of the data returned on the SMARTDRV IOCTL read call
;

SD_IOCTL_Read	struc

    SD_IR_Write_Through   db	?   ; Write-through flag, currently always 1
    SD_IR_Write_Buff	  db	?   ; Buffer write flag, currently always 0
    SD_IR_Enable_Int13	  db	?   ; Cache enable flag, 1 = enabled
    SD_IR_Driver_Type     db	?   ; Type 1 (XMS cache) or 2 (EMS cache)
    SD_IR_Tick_Setting	  dw	?   ; Ticks between flushes, currently unused
    SD_IR_Lock_Cache	  db	?   ; Non-zero if cache is locked
    SD_IR_Reboot_Flush	  db	?   ; Non-zero if cache should be flushed on reboot
    SD_IR_Cache_All	  db	?   ; Zero if full track writes are not cached
    SD_IR_Double_Buffer   db	?   ; Dbl buff state 0=Off, 1=On, 2=Dynamic
    SD_IR_Original_INT13  dd	?   ; INT 13 address SMARTDRV hooked
    SD_IR_Minor_Ver	  db	?   ; Minor version number
    SD_IR_Major_Ver	  db	?   ; Major version number
			  dw	?   ; Unused
                    ; Next three values are used for computing
                    ; cache statistics.  They may be scaled rather
                    ; than absolute values
    SD_IR_Total_Sec_Read  dw	?   ; Count of sectors read
    SD_IR_Cache_Sec_Read  dw	?   ; Count of sectors read from cache
    SD_IR_Track_Sec_Read  dw	?   ; Count of sectors read from track buffer
                    ; Cache statistics
    SD_IR_Cache_Hit_Ratio db	?   ; = Cache_Sec_Read * 100 / Total_Sec_Read
    SD_IR_Track_Hit_Ratio db	?   ; = Track_Sec_Read * 100 / Total_Sec_Read

    SD_IR_Total_Tracks	  dw	?   ; Number of tracks in the cache
    SD_IR_Total_Used	  dw	?   ; Tracks currently in use
    SD_IR_Total_Locked	  dw	?   ; Tracks currently locked
    SD_IR_Total_Dirty	  dw	?   ; Tracks currently dirty
                    ; Information for resizing cache
                    ; All values are in terms of 16Kb pages.
    SD_IR_Current_Size	  dw	?   ; Current size of the cache, in 16Kb pages
    SD_IR_Dev_Size	  dw	?   ; Original size requested by user
    SD_IR_Min_Cache_Size  dw	?   ; Minimum allowable size

SD_IOCTL_Read	ends

SMRTDRVInfo	SD_IOCTL_Read  <>

;
; Structure of the data returned on the IOCTL call to the EMM driver
;
EMM_Import_IOCTL_Data	struc

    EMM_Import_Data_Phys_Ptr	dd	?   ; PHYSICAL 32 bit address of the
					    ;	EMM_Import_Data structure
    EMM_Import_API_Version_Maj	db	?   ; API version #
    EMM_Import_API_Version_Min	db	?

EMM_Import_IOCTL_Data	ends

;
; For the IOCTL to get the data and version number of importable EMM info
;   the IOCTL read subfunction is encoded in the first byte of the IOCTL
;   read buffer.
;
EMM_IMPRT_IOCTL_FUNC	equ	1

EMM_OK	                equ	0
;
; Current version number restriction of WIN386 on EMM_Import_API_Version
;
EMM_IMPORT_MIN_VER	equ	0100h
EMM_IMPORT_MAX_VER	equ	0100h

EMMIoctlBuf	db	EMM_IMPRT_IOCTL_FUNC
		db	(SIZE EMM_Import_IOCTL_Data) dup (0)

SMRTDRVName	db SD_DEV_NAME     ; Name for smartdrv IOCTL opens.
EMM_Name        db EMM_DEV_NAME    ; Name for EMM IOCTL opens.

