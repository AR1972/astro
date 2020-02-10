;========================================================
COMMENT #

	TMPBOOT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Data area contianing a tmp boot record which
	displays a message prompting the user to insert
	a recovery disk into drive A: and press any key.
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

.DATA

	PUBLIC	TmpBoot

TmpBoot	LABEL	BYTE

	include tboot.inc

END
