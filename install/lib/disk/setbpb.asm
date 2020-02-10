;========================================================
COMMENT #

	SET_BPB.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Uses the generic IOCTL call to set the default bpb for
	specified a disk drive

	int SetNewBpb( struct BPB *Bpb, char *WorkBuffer, int Drive );

	ARGUMENTS:	Bpb	- The new BPB structure
			Buffer	- A work buffer for use by the function
			Drive	- The disk drive to set the new bpb for
	RETURNS:	int	- 0 if successfull else -1
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

IF @DataSize
  SetNewBpb PROC USES DS ES DI SI, Bpb:Ptr, Buffer:Ptr, Drive:WORD
ELSE
  SetNewBpb PROC USES    ES DI SI, Bpb:Ptr, Buffer:Ptr, Drive:WORD
ENDIF
	mov	AX,440dh		; IOCTL  function function 0dh
	mov	BX,Drive		; Drive number in BL
	mov	CX,860h			; CH=Device Type - CL=minor funct. #

;	lds	SI,Buffer		; Load pointer to buffer
	LoadPtr	DS, SI, Buffer		; DS:SI --> Caller supplied buffer

	mov	DX,SI			; Move offset pointer to DX
	mov	BYTE PTR [SI],0		; Set for get default params
	int	21h
	jc	Error			; Check for error

SavePtr:
	push	DS			; Save DS:DX
	push	DX

;	les	DI,Buffer		; Set ES:DI for string copy
	LoadPtr	ES, DI, Buffer		; ES:DI --> Caller supplied buffer

	add	DI,07			; Point to bpb in params

;	lds	SI,bpb			; Point DS:SI to new bpb
	LoadPtr	DS, SI, Bpb		; DS:SI --> Bpb structure

	mov	CX,BPB_SIZE		; Number of bytes in the bpb
	cld
	rep	movsb			; Copy new bpb to params
	pop	DX			; Restore DS:DX
	pop	DS

SetNewCall:	
	mov	AX,440dh		; IOCTL function 0dh
	mov	BX,Drive		; Put drive number in BL
	mov	CX,840h			; CH=Device Type - CL=minor funct. #

	mov	SI,DX			; Move offset pointer to SI
	or	BYTE PTR [SI],101b	; Set bit 0 for set default params

	int	21h
	jc	Error			; Check for errors
	xor	AX,AX			; No errors so return OK
	jmp	SHORT SetBpbReturn

Error:
	mov	AX,-1

SetBpbReturn:
	ret

SetNewBpb ENDP
	
; =======================================================

	END

; =======================================================
