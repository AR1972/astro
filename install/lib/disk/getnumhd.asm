;========================================================
COMMENT #

	GETNUMHD.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Returns the total number of hard disks installed
	in the system as reported by the ROM BIOS at
	boot time.

	int GetNumHardDisks( void )

	ARGUMENTS:	NONE
	RETURN: 	int	- Number of hard disks
				  installed
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

GetNumHardDisks PROC

	mov	AH,8			; AH = Get disk info function
	mov	dl,80h			; DL = First hard drive number

	int	13h			; Bios disk int
	mov	AX,0			; Assume no drives found

	jc	ReturnNumDisks		; Error check
	mov	AL,DL			; AL = Number of hard drives
ReturnNumDisks:
	cbw				; AX = number of hard drives
	ret

GetNumHardDisks ENDP

; =======================================================

	END

; =======================================================
