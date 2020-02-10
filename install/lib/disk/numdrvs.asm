;========================================================
COMMENT #

	NUMDRVS.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Uses function 8h of int 13h or if this is not
	available, int 11h to return the total number of
	floppy drives in the system.

	int	GetNumberOfDrives( void );

	ARGUMENTS:	NONE
	RETURN: 	int	- Number of drives installed

	NOTE:	10/26/90
		Must save DS because of bug in Leading
                Edge 8088 3.10 BIOS.

	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

GetNumberOfDrives PROC

	mov	AH,8
	xor	DX,DX

	push	BP			; Bug in leading Edge requires
	push	DI			; saving all of these registers
	push	SI
	push	DS
	push	ES	

	int	13h

	pop	ES
	pop	DS
	pop	SI
	pop	DI
	pop	BP

	jc	TrySwitches
	xor	AX,AX
	mov	AL,DL
	ret

TrySwitches:
	int	11h			; Get BIOS equipment flags
	mov	CL,6			; Shift over floppy disk bits
	shr	AX,CL
	and	AX,11b			; Mask off the disk bits
	inc	AX			; Add 1
	ret

GetNumberOfDrives ENDP

; =======================================================

	END

; =======================================================
