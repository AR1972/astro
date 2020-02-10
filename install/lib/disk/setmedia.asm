;========================================================
COMMENT #

	SETMEDIA.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Extended ROM BIOS call to set the disk layout for a
	format operation. The function must do 1 retry in case
	a disk change status is returned.

	void *SetMediaType( int Drive, int Tracks, int Sectors )

	ARGUMENTS: Drive	- Physical drive number
		   Tracks	- Total tracks on the floppy disk
		   Sectors	- Number of sectors per track

	RETURNS:		  Ptr to DASD for this type of drive
				  is successfull else NULL ptr
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

INCLUDE	disk_io.inc
INCLUDE	model.inc

; =======================================================

.CODE

; =======================================================

SetMediaType PROC USES ES DI, Drive:BYTE, Tracks:BYTE, Sectors:BYTE

	mov	AH,18h		; ROM BIOS get drive parameters function
	mov	CH,Tracks	; Load total number of disk track
	mov	CL,Sectors	; Load sectors per track
	dec	CH		; Convert track to 0 based
	mov	DL,Drive	; Load physical drive number
	push	AX 		; Save AX in case of a retry
	int	13h		; ROM BIOS disk call
	pop	AX		; Retore function number
	jnc	SetOK		; Everything OK 
	int	13h 		; Got an error so do a retry
	jc	SetError	; Now everything is OK

SetOK:
	mov	AX,DI 		; Put DASD ptr in DX:AX
	mov	DX,ES
	jmp	SHORT SetMediaExit ; Everything is ready for return

SetError:
	xor	AX,AX		; Put NULL ptr in DX:AX	to signal error
	cwd			; Extend sign into DX

SetMediaExit:
	ret

SetMediaType ENDP

; =======================================================

	END

; =======================================================
