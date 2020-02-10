;========================================================
COMMENT #

	INT13RTR.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Does an int 13h and resets the disk and does a retry
	if the first operation fails with an error other than
	a timeout error returns all register in the condition
	returned by the int 13h call.

	Int13WithRetry PROC NEAR
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

.CODE


; =======================================================
; =======================================================

Int13WithRetry PROC NEAR

	push	CX			; Save needed registers
	push	DI
	push	SI

	mov	DI,AX			; Put function call in DI
	mov	SI,CX			; Put head and track in SI
	mov	CX,DISK_RETRIES		; Put number of retries in CX
	
DoInt13:
	push	CX			; Save loop counter
	mov	CX,SI			; Put head and track back in CX
	int	13h			; BIOS disk call
	pop	CX			; Restore the loop counter
	jc	IsItTimeOut		; If error then retry the operation
	jmp	SHORT FunctReturn	; Else we're finished

IsItTimeOut:
	cmp	AH,DISK_TIMEOUT		; See if this is a timeout error
	je	SetCarry		; If timeout don't retry
	cmp	CX,1			; See if retries are exhausted
	je	SetCarry		; If exhaust don't do reset

DiskReset:
	xor	AX,AX			; Set AX == 0 for retry call
	int	13h			; We can assume no timeout error
	mov	AX,DI			; Restore function call AX
	loop	DoInt13			; Exhuast all retries

SetCarry:
	stc				; Set the carry flag to show error

FunctReturn:
	pop	SI			; Restore the used registers
	pop	DI
	pop	CX
	ret

Int13WithRetry ENDP

END
