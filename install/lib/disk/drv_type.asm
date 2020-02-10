;========================================================
COMMENT #

	DRV_TYPE.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	int GetDriveType( unsigned char Drive, void *Buffer )

	Determines the type of drive installed.

	ARGUMENTS: Drive	- Physical drive number
		Buffer	- Ptr to work buffer at least 100 bytes

	RETURNS:	Drive type as defined by int 13h function 8.
		1	   360K   5.25"
		2	   1.2M   5.25"
		3	   720K   3.5"
		4	   1.44M  3.5"

	=================================================

	STRATEGY:

	if ( DOS IOCTL Supported )
		got drive	type
	else if ( bios int 13 function 8 != CARRY )
		got drive	type
	else if ( bios int 13 function 15h != CARRY )
	{
		if ( changeline supported )
		drive type = 1.2 meg
		else
		drive tyupe = 360K
	}
	else
		drive type = 360K

	=================================================

	johnhe - 06/06/89

END COMMENT #

;========================================================

INCLUDE	model.inc

;========================================================

.CODE

; =======================================================

IF @DataSize
  GetDriveType PROC USES DS ES DI, Drive:BYTE, Buffer:PTR
ELSE
  GetDriveType PROC USES    ES DI, Drive:BYTE, Buffer:PTR
ENDIF

TryIoctl:

;	les	DI,Buffer	; ES:DI is ptr to buffer
	LoadPtr	ES, DI, Buffer	; ES:DI is ptr to buffer

	push	DI		; Save ptr in DI for later use
	mov	CX,50		; Get length of buffer in words
	xor	AX,AX		; Buffer fill bytes
	cld 			; Set direction to forward
	rep	stosw 		; Set the buffer to all zeros

	mov	AX,440dh	; DOS generic IOCTL
	mov	BL,Drive	; Set drive number
	inc	BL		; Convert to DOS drive number
	mov	CX,0860h	; CH = block dev, CL = Get parameters

;	lds	DX,Buffer	; Ptr to buffer in DS:DX
	LoadPtr	DS, DX, Buffer	; Ptr to buffer in DS:DX

	int	21h		; DOS call
	pop	DI		; Get ptr to buffer off stack

	jc	TryBios08	; This IOCTL call not supported

ConvertToBiosType:
	inc	DI		; Point to device type byte
	mov	AL,[DI]		; Get the device type byte
 	inc	AL		; Convert to BIOS type device
	cmp	AL,08h		; Check for special 1.44 meg case
	jne	GotDrive	; Finished so jump to exit
	shr	AL,01		; Convert from DOS 1.44 drive # to BIOS #
	jmp	SHORT GotDrive	; Finished so jump to exit

TryBios08:
	mov	AH,08h		; Get disk parameters function
	mov	DL,Drive	; DL = physical drive number

	push	DX 		; This function may fail so keep DX
	int	13h 		; Bios disk interupt
	pop	DX		; Restore drive number in DX

	jc	TryBios15	; Function not support so try function 15h
	mov	AL,BL		; Move drive type into AL
	jmp	SHORT GotDrive 	; Finished so jump to exit


TryBios15:
	mov	AH,15h		; Get change line support, remember
	int	13h		; that DX already has drive number
	jc	Is360K		; Function not supported so must be 360K
	test	AH,1		; Test no changeline bit
	jnz	Is360K		; No change line so default to 360K
	mov	AL,02		; Has change lines so assume 1.2 meg drive
	jmp	SHORT GotDrive	; Finished so jump to exit

Is360K:
	mov	AL,01		; Default drive size

GotDrive:
;	cbw			; Clear high byte of AX
	xor	AH,AH		; M00 Clear high byte of AX

	cmp	AL,4		; M125 Error check for unsupported drive type
	jbe	@f		; M125 such as 2.88 meg drives, in such cases
	mov	AL,4		; M125 revert to 1.44 drive type
@@:				; M125
	ret

GetDriveType ENDP

;========================================================

	END

;========================================================
