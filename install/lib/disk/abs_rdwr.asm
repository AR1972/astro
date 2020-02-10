;========================================================
COMMENT #

	ABS_RDWR.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential


	=================================================
	Does an absolute sector read or write using int
	25h or 26h. Special care is taken to be sure the
	function will work under any version of DOS
	2.0 - 4.x.

	int AbsReadWrite( int Drive, struct ABSIO_PACKET far *absPack,
			  int ReadWrite );

	ARGUMENTS:	absPack - Ptr to DOS 4.x int 25,26 access packet
			Drive	- Drive number (A=0,B=1,C=2,...)
			ReadWrite- Flags whether to do a read or write
				   operation, ie:
				   0  = READ
				   !0 = WRITE
	RETURNS:	int	- 0 if successful else error code
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

ABSIO_PACKET	STRUC

	StartSec	dd	?
	NumSecs		dw	?
	SecBuffer	dd	?

ABSIO_PACKET	ENDS

INCLUDE	model.inc


; =======================================================

.CODE

; =======================================================


AbsReadWrite PROC uses ES DS SI DI, Drive:BYTE, absPack:PTR, ReadWrite:WORD
	
LoadRegs:
	mov	AL,Drive		; Put DOS drive # in AL

;	lds	BX,absPack		; DS:BX ptr to packet structure
	LoadPtr	DS, BX, absPack

	mov	CX,-1			; Test DOS it's a packet call

SetupForNewMethod:
	push	AX			; Save registers in case first
	push	BX			; try fails and we need to try
	push	CX			; the new type of packet call
	push	DS

UseOldMethod:
	mov	CX,[BX].NumSecs		; Number of sectors to read or write
	mov	DX,WORD PTR [BX].StartSec ; Starting sector #
	lds	BX,[BX].SecBuffer	; Ptr to buffer in DS:BX

TestReadWrite:
	push	CX
	or	ReadWrite,0 		; 0 == READ else WRITE
	jz	DoRead

DoWrite:
	int	26h			; DOS absolute sector write
	jmp	SHORT CheckCarry

DoRead:
	int	25h			; DOS absolute sector read

CheckCarry:
	pop	BX			; Adjust the stack (see MS Prog Ref)
	pop	CX
	pushf				; Save return status
	cmp	CX,-1			; See if first try
	jne	DoSecondTry 		; Was first try do second method
	popf				; Restore error status
	jc	ConvertToInteger	; If carry return error code
	jmp	SHORT NoError

DoSecondTry:
	popf				; Restore error status
	pop	DS 			; Restore packet segment
	pop	CX			; Restore packet signal word
	pop	BX			; Restore packet offset
	pop	AX			; Restore drive number
	jnc	NoError			; First try was success 
	jmp	SHORT TestReadWrite	; Retry the operation

NoError:
	xor	AX,AX			; No error so return OK

ConvertToInteger:
	cbw				; Extend the sign bit into AH

	or	ReadWrite,0 		; If this was a write we
	jz	Done			; need to do a disk reset
					; to be sure buffers are flushed
	push	AX			
	mov	AH,0dh			; DOS disk reset function
	int	21h
	pop	AX

Done:
	ret

AbsReadWrite ENDP

; =======================================================

	END

; =======================================================
