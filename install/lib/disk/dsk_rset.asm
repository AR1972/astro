;========================================================
COMMENT #

	DSK_RSET.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Instructs DOS to do a reset of all it's disk buffers.

	int	_dos_dskreset( void )

	RETURNS: void

	================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc
 	
; =======================================================

.DATA
	PUBLIC	DiskChange
DiskChange db	0

.CODE

; =======================================================

_dos_dskreset PROC

	mov	DiskChange,1
	mov	AH,0dh
	int	21h
	ret

_dos_dskreset ENDP

; =======================================================

	END

; =======================================================
