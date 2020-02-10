;========================================================
COMMENT #

	BIOSRSET.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Does a ROM BIOS drive reset on the specified drive.

	void ResetDrv( int Drive );

	ARGUMENTS:	Drive	- Physical disk drive number.

	RETURNS:	void
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

ResetDrv PROC Drive:BYTE

	mov	AL,00
	mov	DL,Drive
	int	13
	ret

ResetDrv ENDP

; =======================================================

	END

; =======================================================
