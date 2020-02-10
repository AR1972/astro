;========================================================
COMMENT #

	DSK_RSET.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Instructs DOS to do a reset of all it's disk buffers.
	Also sets the DiskChange variable to 1. The address
	of this is in DiskChangePtr initialized in upginit.c.
	This variable is checked by the retail upgrades int13
	handler in ..\common\newint13.asm .

	int	_dos_dskreset( void )

	RETURNS: void

	================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

;INCLUDE	disk_io.inc
INCLUDE	model.inc
 	
; =======================================================


EXTRN	DiskChangePtr:DWORD

.CODE


; =======================================================

_dos_dskreset PROC USES ES BX

	les	bx, [DiskChangePtr]
	mov	byte ptr es:[bx], 1
	mov	AH,0dh
	int	21h
	ret

_dos_dskreset ENDP

; =======================================================

	END

; =======================================================
