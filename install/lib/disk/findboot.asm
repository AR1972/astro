;========================================================
COMMENT #

	FINDBOOT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	Returns the first harddisk drive letter. Determine
	which logical drive is the first hard disk drive
	letter by setting up a dummy int 13h handler and
	then doing an int 25h to read the first sector from
	each drive starting at C:. The first harddisk drive
	letter will be the first access which is to drive
	80h.


	int FindFirstHd( void );
	ARGUMENTS:	NONE
	RETURNS:	int	- First harddisk drive letter
				  or -1 if no harddisk drive
				  letter is found.
	=================================================

	johnhe - 10/28/90

END COMMENT #
; =======================================================


MAX_DOS_DRIVES	EQU	26

ABSIO_PACKET	STRUC

	StartSec	dd	?
	NumSecs		dw	?
	SecBuffer	dd	?

ABSIO_PACKET	ENDS

INCLUDE	model.inc

; =======================================================

.DATA

; =======================================================

DriveNum	db	(?)

; =======================================================

.CODE

; =======================================================

EXTRN		AbsReadWrite:PROC

OldInt13	DD	(?)

; =======================================================

FindFirstHd	PROC USES SI, Buffer:PTR
	LOCAL	Dsk[ SIZE ABSIO_PACKET ]:BYTE

	push	ES
	mov	AX,3513h		; Get old int 13h vector
	int	21h
	mov	WORD PTR CS:OldInt13,BX	   ; Save old offset value
	mov	WORD PTR CS:OldInt13[2],ES ; Save old segment value
	pop	ES

	push	DS
	mov	DX,OFFSET DummyInt13	; DS:DX --> Dummy int handler
	mov	AX,@CODE		; Set up new int 13h handler
	mov	DS,AX
	mov	AX,2513h
	int	21h
	pop	DS

SetupInt25_Struc:
	xor	AX,AX
	mov	WORD PTR Dsk.StartSec,AX	; Start sector == 0L
	mov	WORD PTR Dsk.StartSec[2],AX
	mov	Dsk.NumSecs,1			; Read 1 sector

	push	ES
;	les	AX,Buffer			; ES:BX --> read buffer
	LoadPtr	ES, AX, BUFFER			; ES:BX --> read buffer
	mov	WORD PTR Dsk.SecBuffer,AX	; Move the buffer address
	mov	WORD PTR Dsk.SecBuffer[2],ES	; into the structure.
	pop	ES

	mov	SI,2				; SI == drive to scan .. C:

ScanDrives:
	mov	DriveNum,0ffh		; Initialize drive value to -1
	xor	AX,AX
	push	AX			; Read/write value == READ

IF @DataSize
	push	SS			; Segment of disk structure
ENDIF
	lea	AX,Dsk			; Offset address of disk structure
	push	AX			; Push the offset
	push	SI			; Push drive to read
 	call	AbsReadWrite

IF @DataSize
	add	SP,8			; Adjust the stack for all pushes
ELSE
	add	SP,6
ENDIF

TestDriveNum:
	mov	AL,DriveNum
	cmp	AL,80h
	jne	TestForLastDrive

	mov	AX,SI			; Put drive number in AX for return
	add	AX,'A'			; Convert to asci drive letter
	jmp	SHORT RestoreInt13

TestForLastDrive:
	cbw
	cmp	AX,0ffffh
	je	RestoreInt13

SetNextDrive:
	inc	SI			; Set to next logical drive
	cmp	SI,MAX_DOS_DRIVES	; Have we scan all DOS drives?
	jb	ScanDrives		; If not keep looping
	mov	AX,0ffffh		; Didn't find drive 80h

RestoreInt13:
	push	AX
	push	DS			; Restore original int 13h handler

	lds	DX,CS:OldInt13		; DS:DX --> Original int 13h handler
	mov	AX,2513h		; Get old int 13h vector
	int	21h

	pop	DS
	pop	AX

	ret

FindFirstHd	ENDP

; =======================================================

DummyInt13:

	cmp	AH,02			; See if READ sectors function
	jne	TestForHd

	push	AX
	push	DS
	mov	AX,@DATA		; Set DS to address data segment
	mov	DS,AX
	mov	DriveNum,DL		; Save the drive number
	pop	DS
	pop	AX
	
TestForHd:
	test	DL,80h			; See if high bit set for hard drive
	jz	FloppyExit
	jmp	DWORD PTR CS:[OldInt13]

FloppyExit:
	xor	AX,AX			; Signal no errors
	clc				; Clear the carry for no error
	retf	02			; Far return and throw flags away


; =======================================================

END

; =======================================================

