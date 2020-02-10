;========================================================
COMMENT #

	FAIL24.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	This function is can be called by an int 24h handler to
	simulate DOS failing the previous int 21h call. This
	function returns CPU control to the instruction following
	the last int 21h call. The registers are set exactly as
	they were when the int 21h was issued except the carry
	is set to signal an error and AL contains an error
	code to signal disk failure.

	void Int24Fail( void );

	ARGUMENTS:	NONE
	RETURN: 	void	- Does not return to caller
	=================================================

	johnhe - 10/01/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

Int24Fail PROC FAR		; Must be far to keep stack count correct

	add	SP,20h		; Adjust stack to point to int 21 regs

	pop	AX
	pop	BX
	pop	CX
	pop	DX
	pop	SI
	pop	DI
	pop	BP
	pop	DS
	pop	ES

	mov	AX,27
	stc

	retf	02

Int24Fail ENDP

END
