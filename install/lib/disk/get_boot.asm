;========================================================
COMMENT #

	GET_BOOT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Reads the boot record on the first sector of the
	specified drive into the specified buffer

	int GetBootSector( int Drive, char *Buffer )

	ARGUMENTS:	Drive	- Physical drive number
			Buffer	- Ptr to sector size buffer

	RETURNS:	int	- OK (0) if successfull
				  else error code
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

GetBootSector PROC USES ES, Drive:BYTE, Buffer:PTR

	mov	DH,0		; Head 0
	mov	DL,Drive	; Get drive number

;	les	BX,Buffer	; Set ES:BX == ptr to buffer
	LoadPtr	ES, BX, Buffer	; ES:BX --> Caller's buffer

	mov	CX,5		; Retry count

ReadSector:
	push	CX		; Save retry count
	mov	AX,0201h	; Read 1 disk sector
	mov	CX,1		; Track 0 sector 1
	int	13h		; BIOS disk interrupt
	pop	CX		; Recover CX for possible retry

CheckStatus:
	jnc	GotSector	; No error so break loop
	xor	AX,AX		; Drive reset function
	int	13h		; Do a disk reset
	loop	ReadSector	; Now retry the operation
	mov	AX,-1		; Signal an error
	jmp	SHORT GetBootExit ; Return error in AX

GotSector:
	xor	AX,AX		; Return OK - no errors

GetBootExit:
	ret

GetBootSector ENDP

; =======================================================

	END

; =======================================================
