; ========================================================

COMMENT #

	BOOT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	;================================================

	File to include partition and master
	boot records.

	;================================================

	johnhe - 02-14-90

END COMMENT #

;========================================================

INCLUDE model.inc

;========================================================

.DATA

	PUBLIC NewBootRec
NewBootRec LABEL BYTE

INCLUDE	BOOT.INC

	PUBLIC	MasterBootRec
MasterBootRec LABEL BYTE
 
INCLUDE	FDBOOT.INC

	PUBLIC	EverexMbr
EverexMbr LABEL BYTE

INCLUDE EVEREX.INC

;========================================================

END
