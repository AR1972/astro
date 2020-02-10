ifndef INC_LIST
.xlist
endif
;=============================================================================
;==
;== (C) Copyright MICROSOFT Corp. 1989-1991
;== (C) Copyright COMPAQ Computer Corp. 1989-1991
;==
;==	Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386 Driver
;==
;==	Module: VDISK.INC - VDISK equates and structures for detecting them
;==
;==	Version: 1.00
;==
;==	Date:	October 5, 1989
;==
;==	Author: Leo Cohen
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;==     08/22/89 0.00	        Original
;==
;=============================================================================

;=============================================================================
;==			VDISK Header Structure
;== 	(used only for searching for previously installed VDISK's)
;=============================================================================
VDISKheader  	struc
		dd	-1		; Pointer to next device
		dw 0000000000000000b	; Attribute word
;		   ^^^ ^----------------- 11: O/C/RM not supported
;		   |||------------------- 13: IBM format
;	 	   ||-------------------- 14: IOCTL not supported
;	 	   |--------------------- 15: Block device
		dw	0		; Entry point of strategy routine
		dw	0		; Entry point of 'interrupt' routine
		db	8 dup (0)	; Name field - 0 units
  V_VDISK 	db	'VDISK'		; VDISK Header label
  V_VER   	db	'  V2.0'        ; VDISK version
  V_TYPE	db	28h,0,0,0,0,0,0,0,0,0,0,0,60h,086h,09h
  V_ADDRLO 	dw	0000h		; Bits 15:00
  V_ADDRHI 	db	10h		; Bits 23:16
VDISKheader  	ends

;=============================================================================
;==			Structure for BOOT record
;== 	(not used, left over from old CEMM by Steve Preston)
;=============================================================================
BOOTS	struc
		db	3 dup(?)	; Jump instr or 0,0,0 if non-bootable
  OEMS		db	8 dup(?)	; Oem name
  BYTBS		dw	?		; Bytes per sector
  SECAU		db	?		; Sectors per allocation unit
  RSSEC		dw	?		; Reserved sectors
  FTNUM		db	?		; Number of fats
  DENUM		dw	?		; Number of directory entries
  TLSEC		dw	?		; Total sectors
  MEDES		db	?		; Media descriptor
  NUMFT		dw	?		; Number of fat sectors
  SECTK		dw	?		; Sectors per track
  NUMHD		dw	?		; Number of heads
  NUMHS		dw	?		; Number of hidden sectors
  BPBEXT	dw	?		; Extended memory allocated
BOOTS	ends				; End of structure

.list ; end of VDISK.inc
