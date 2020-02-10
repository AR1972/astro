;========================================================
COMMENT #

	DMA_CHK.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Checks to see if an area of memory will cross a DMA
	boundary. Returns OK if not on a boundary else return
	!OK.

	int CheckDmaBound( void *Buffer, unsigned Bytes )

	ARGUMENTS:	Buffer	- Ptr to memory area
			Bytes	- Lenght of memory area in byts
	RETURN:	int		- OK if not on boudary else !OK
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

CheckDmaBound PROC USES ES, Buffer:Ptr, Bytes:WORD

;	les	BX,Buffer		; ES:BX = Buffer address
	LoadPtr	ES, BX, Buffer		; ES:BX = Buffer address

TestSegmentBound:

	mov	AX,ES			; Put buffer offset in AX
	shl	AX,1			; Convert segment to 20 bit address
	shl	AX,1 			; while ingoring the high 4 bits
	shl	AX,1
	shl	AX,1

	add	AX,BX			; Add offset to 20 bit address

	add	AX,Bytes		; Add number of bytes to 20 bit addr
	jnc	NotOnBound		; If no carry then no DMA overrun
	mov	AX,1			; Signal DMA error
	jmp	SHORT DmaBoundExit	; Return to caller

NotOnBound:
	xor	AX,AX			; Signal OK

DmaBoundExit:
	ret

CheckDmaBound ENDP

; =======================================================

	END

; =======================================================
