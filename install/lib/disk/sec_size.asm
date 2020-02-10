;========================================================
COMMENT #

	SEC_SIZE.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Uses the DPB to return the number of bytes per
	sector on the specified drive.

	unsigned GetSectorSize( Drive );

	ARGUMENTS: Drive - DOS drive number (0=default, 1=A, 2=B, ...)
	RETURNS:   int	- TRUE if disk is removeable else false

	================================================

	johnhe - 12-11-90

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

GetSectorSize PROC USES DS, Drive:BYTE

	mov	AH,32h			; Get DPB request
	mov	DL,Drive		; Drive in BL (0=default,1=A...)
	int	21h

	cmp	AL,0ffh			; Check for invalid drive error
	je	BadDrive
					; Bytes per sector is offset 2
					; in the DBP
	mov	AX,[BX+2]		; AX = Bytes per sector
	ret

BadDrive:
	mov	AX,200h			; Default to 512 bytes sectors size
	ret	

GetSectorSize ENDP

END

