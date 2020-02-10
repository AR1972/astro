;========================================================
COMMENT #

	SETDSKTYP.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Extended ROM BIOS call which sets a disk type for
	formatting using function 17h of int 13h. The function
	must do 1 retry in case disk changed status is returned.

	int SetDiskType( unsigned uDrv, unsigned char DiskType );

	ARGUMENTS:	uDrv	- Physical floppy drive number
			DiskType - Extended BIOS disk type
			0	- Not used
			1	- 320/360K in 360K drive
			2	- 320/360K in 1.2M drive
			3	- 1.2M in 1.2M drive
			4	- 720K in 720K drive
	RETURN:		int	- 0 if ok else INVALID_DRIVE_TYPE
	================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

SetDiskType PROC USES ES DI, Drive:BYTE, DiskType:BYTE, 

	mov	AH,17h
	mov	AL,DiskType
	mov	DL,Drive
	push	AX
	int	13h
	pop	AX
	jnc	OkExit2
	int	13h
	jnc	OkExit2
	mov	AX,INVALID_DRIVE_TYPE
	ret
OkExit2:
	xor	AX,AX		; Move OK into AX
	ret

SetDiskType ENDP

; =======================================================

	END

; =======================================================
