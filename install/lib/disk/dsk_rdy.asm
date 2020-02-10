;========================================================
COMMENT #

	DSK_RDY.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Returns TRUE if a disk is in the specified drive
	else returns FALSE. Uses BIOS int 13h to try to
	verify the first sector on the disk and then checks
	the return code if an error is detected and returns
	TRUE if error != 0x80 else returns FALSE if time out
	error is returned. If changeline is returned the
	verify is retried to avoid conflicts.

	int IsDiskReady( int Drive )

	ARGUMENTS:	Drive  - Physical drive number

	RETURNS:	int    - TRUE is disk is ready
				 else FALSE
	=================================================


	johnhe - 06/06/89

END COMMENT #
;========================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

;========================================================

.CODE

;========================================================

IsDiskReady PROC Drive:BYTE

	mov	AX,0401h	; Verify 1 disk sector function
	mov	CX,01		; Sector 1 track 0
	mov	DH,00		; Head 0
	mov	DL,Drive	; Caller specified drive number

	push	AX		; Do a disk reset before the
	xor	AX,AX		; sector verify to fix a bug in
	int	13h		; the EPSON ROM BIOS.
	pop	AX		; Restore AX

VerifySector:
	int	13h		; BIOS disk interrupt call
	jnc	IsReady		; No error so disk is ready

	cmp	AH,06		; See if changeline returned
	jne	CheckTimeOut	; Not changeline so check for timeout

	mov	AX,0401h	; Retry the call if changeline error
	int	13h		; BIOS disk interrupt call
	jnc	IsReady		; No error so disk is ready

CheckTimeOut:
	cmp	AH,80h		; See if timeout error
	jne	IsReady		; Not timeout so disk is inserted

NotReady:
	xor	AX,AX		; Single disk not ready
	jmp	SHORT ReadyExit	; Finished so jump to exit

IsReady:
	mov	AX,1		; Return TRUE

ReadyExit:
	ret
	
IsDiskReady ENDP

;========================================================

	END

;========================================================
