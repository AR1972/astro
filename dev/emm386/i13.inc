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
;==	Module: I13.INC  - Include for int 13 support
;==
;==	Version: 1.00
;==
;==	Date:	July 24, 1991
;==
;==
;=============================================================================
;==
;==	Change Log:
;==
;==	DATE	 REVISION	Description
;==	-------- --------	--------------------------------------------
;== 	07/24/91 0.00	        Original
;==
;=============================================================================

FixedDrives	equ	8		; Fixed disks supported
FloppyDrives	equ	2 		; Floppy disks supported
TotalDrives	equ	FixedDrives+FloppyDrives

READ		equ	2
WRITE		equ	3
LONG		equ	8

.list	; end of i13.INC

