;========================================================
COMMENT #

	DRV_ACC.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	void EnableDiskAccess( unsigned char Drive )

	Sets the drive access bit using undocumented
	function SET_ACCESS_FLAG (47h) of IOCtrl function
	44h.

	ARGUMENTS: Drive - DOS drive letter (A=0, B=1, C=2, ...)
	RETURNS:   VOID

	=================================================

	johnhe - 06/04/90

END COMMENT #

;========================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

;========================================================

.CODE

; =======================================================

IF @DataSize
  EnableDiskAccess PROC USES DS, Drive:BYTE
ELSE
  EnableDiskAccess PROC          Drive:BYTE
ENDIF

	LOCAL	AccessPacket:WORD

	mov	Accesspacket,0100h		; Setup the packet
	lea	DX,AccessPacket

IF @DataSize
	push	SS
	pop	DS
ENDIF

	mov	AX,4411h			; Generic ioctl	support chk
	xor	BX,BX				; Clear BX
	mov	BL,Drive			; Get Drive letter
	inc	BL				; Make it 1 based
	mov	CX,847h				; Allow access to disk
	int	21h
	jc	SetAccessExit			; If function not supported
						; we know access is allowed
	mov	AX,440dh
	int	21h

SetAccessExit:
	ret

EnableDiskAccess ENDP

;========================================================

	END

;========================================================
