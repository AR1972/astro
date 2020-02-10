;========================================================
COMMENT #

	NEWINT13.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================
	New interrupt 13h handler and support functions
	which check for DMA boundary errors and if detected
	will break the read into 3 pieces to eliminate
	error.
	=================================================

	johnhe - 06/06/89

END COMMENT #
; =======================================================

; =======================================================
; DWORD address of original int 13h handler. Initialize
; by InitNew13() and then used by RestoreOld13() to
; to restore the vector before the program exits back
; to DOS.
; =======================================================

	public	OldInt13
OldInt13	dd	(?)

OldAX		dw	 (?)

SplitBuffer	dd	(?)

; =======================================================
; Sets int vector 13h to point to the NewInt13 function
; which does a check for DMA errors and corrects the
; error. Saves the original vector for later restoration.
;
; void InitNew13( char *Buffer );
;
; ARGUMENTS:	Buffer	- Ptr to min 1024 byte buffer
;			  to be used for if a disk read
;			  needs to be split to avoid a
;			  DMA boundary error
; RETURNS:	void
; =======================================================

InitNew13 PROC USES DS ES, Buffer:PTR

	mov	AX,3513h			;  Get int vector 13h
	int	21h
	mov	WORD PTR CS:OldInt13,BX	; Save the offset
	mov	WORD PTR CS:OldInt13[2],ES ; Save the segment

	mov	AX,2513h		; Set vector 13h
	mov	DX,offset NewInt13	; Load new vector offset
	push	CS			; Load new vector segment
	pop	DS
	int	21h			; DOS call

SaveBuffer:
	lds	BX,Buffer;		; DS:BX --> 1024 byte buffer

TestForSegmentBound:

	mov	AX,DS			; Put buffer segment in AX
	shl	AX,1			; Convert segment to 20 bit address
	shl	AX,1 			; while ingoring the high 4 bits
	shl	AX,1
	shl	AX,1

	add	AX,BX			; Add offset to 20 bit address

	add	AX,512			; Add sector size to 20 bit address
	jnc	GotBuffer		; If no carry then no segment overrun
	add	BX,512			; Else move to next segment

GotBuffer:
	mov	WORD PTR CS:SplitBuffer,BX
	mov	WORD PTR CS:SplitBuffer[2],DS

	ret

InitNew13 ENDP

; =======================================================
; Replaces the int 13h vector with the original pointer.
; =======================================================

RestoreOld13 PROC USES DS

	mov	AX,2513h		; Set vector 13h
	mov	DX,WORD PTR CS:OldInt13[2] ; Load old segment
	mov	DS,DX
	mov	DX,WORD PTR CS:OldInt13	; Load old offset
	int	21h
	ret

RestoreOld13 ENDP

; =======================================================
; New int 13h handler which will correct DMA errors if
; they are detected on reads or writes.
; =======================================================
	
	public	NewInt13
NewInt13:

	push	AX			; Save original request

	cmp	DL, FIRST_HARD_DISK	; Only do disk change check
	jae	FakeIntCall		; if drive in AL < 80h


;;	push	DS			; First see if we need to fake
;;	mov	AX,@DATA		; a changeline.
;;	mov	DS,AX
	xor	AX,AX
	cmp	AL,BYTE PTR CS:DiskChange
	mov	BYTE PTR CS:DiskChange,AL
;;	pop	DS

	pop	AX			; Restore AX but keep original
	push	AX

	je	FakeIntCall
	mov	AX,0600h
	stc
	jmp	SHORT TestError

FakeIntCall:
	pushf				; Simulate an int request
	call	DWORD PTR CS:[OldInt13]	; of the old int 13h handler
	jc	TestError		; Error so check for DMA error
	
IntExit:
	pop	CS:OldAX
	retf	02			; Far return and pop flags

TestError:
	cmp	AH,9			; Check for DMA bound error
	je	ServiceError
	stc				; Reset error condition	
	jmp	SHORT IntExit	

ServiceError:
	pop	AX			; Get original function call in AX
	cmp	AL,0			; Make sure of valid number sectors
	je	RestoreError
	cmp	AH,2			; Check was it a read operation
	je	SplitOperation		; If yes try splitting the read
	cmp	AH,3			; Check was it a write operation
	je	SplitOperation		; If yes try splitting the write

RestoreError:
	mov	AX,900h			; Retore original error code
	stc				; Restore the original err condition
	retf	02			; Far return and pop the flags

SplitOperation:
	call	NEAR PTR Int13Error	; Try to avoid the DMA boundary
	retf	02			; Return to caller


; =======================================================
; Breaks a disk read or write into individual sectors
; using another memory location which is not on a DMA
; boundary. Returns BIOS int 13h error code if carry set
; else AX == 0.	Preserves all registers execpt AX
; =======================================================

Int13Error PROC NEAR

	push	BX			; Save registers we need to use
	push	CX
	push	DI
	push	SI
	push	DS
	push	ES

	lds	SI,CS:SplitBuffer	; Put error buffer offset in SI

	cmp	AH,02			; See if this is a read
	jne	IsWrite			; If not read must be a write
IsRead:
	call	ReadSecs		; Call proc to read in the sectors
	jmp	SHORT CleanUp		; Finished

IsWrite:
	call	WriteSecs		; Call proc to write the sectors

CleanUp:
	pop	ES			; Restore all caller's registers
	pop	DS			; excpet AX and flags
	pop	SI
	pop	DI
	pop	CX
	pop	BX
	ret

Int13Error ENDP


; =======================================================
; Uses the BIOS int 13h call to read the number of sectors
; in AL into the buffer in DS:SI and then moves them to
; to the buffer in ES:BX. Reads and moves one sector at
; a time. AX, BX, CX, DX, ES must be setup on on entry
; for a normal BIOS read and DS:SI must point to a 
; buffer at least 512 bytes long which is not on a 
; DMA boundary.
; =======================================================

ReadSecs PROC NEAR

	mov	DI,BX			; ES:DI == start of caller's buffer
	mov	BX,SI			; Point BX to new buffer

ReadLoop:
	push	AX			; Save remaining sectors to do
	mov	AL,1			; Set number of sector to 1

	push	ES			; Save caller's buffer segment
	push	DS 			; Set ES to new buffer
	pop	ES

	pushf				; Simulate and int call
	call	DWORD PTR CS:[OldInt13]	; of the old int 13h handler
	pop	ES			; Retore caller's buffer segment

ErrorTest:
	jnc	CopyFromBuf		; No error so copy sector to user buf
	add	SP,2			; Adjust stack for pushed AX
	stc				; Signal we got an error
	jmp	SHORT ReadReturn	; Finished

CopyFromBuf:
	push	SI			; Save ptr to start of new buffer
	push	CX			; Save next sector number
	mov	CX,(512/2)		; Set count of words in buffer 
	cld				; Set direction to forward
	rep	movsw			; Move 1 sector to new buffer
	pop	CX			; Restore next sector number
	pop	SI			; Restore ptr to start of buffer

	pop	AX			; Restore remaining sectors
	inc	CL			; Increment to next sector
	dec	AL			; One less sector to do
	jnz	ReadLoop		; Loop until AL == 0
	xor	AX,AX			; No error code	also clears carry
	jmp	SHORT ReadReturn	; Finished

ReadReturn:
	ret

ReadSecs ENDP

; =======================================================
; Uses the BIOS int 13h call to write the number of sectors
; in AL	by moving them to a new buffer one sector at a
; time and then writing them one at a time.
; AX, BX, CX, DX, ES must be setup on on entry
; for a normal BIOS read and DS:SI must point to a 
; buffer at least 512 bytes long which is not on a 
; DMA boundary.
; =======================================================

WriteSecs PROC NEAR

	push	ES			; Set DS:SI to caller's buffer
	push	DS			; and ES:DI to new buffer
	pop	ES
	pop	DS
	mov	DI,SI
	mov	SI,BX
	mov	BX,DI			; Point BX to new buffer

WriteLoop:
	push	CX			; Save next sector number

CopyToBuf:
	mov	DI,BX			; Set ptr to new buffer
	mov	CX,(512/2)		; Set count of words in buffer 
	cld				; Set direction to forward
	rep	movsw			; Move 1 sector to new buffer
	pop	CX			; Restore next sector number

WriteTheSector:
	push	AX			; Save remaining sectors to do
	mov	AL,1			; Set number of sector to 1

	pushf				; Simulate and int call
	call	DWORD PTR CS:[OldInt13]	; of the old int 13h handler

ErrorTest:
	jc	WriteError		; Got an error so return

	pop	AX			; Restore remaining sectors
	inc	CL			; Increment to next sector
	dec	AL			; One less sector to do
	jnz	WriteLoop		; Loop until AL == 0
	xor	AX,AX			; No error code	also clears carry
	jmp	SHORT WriteReturn	; Finished

WriteError:
	add	SP,2			; Adjust stack for pushed AX
	stc				; Signal we got an error

WriteReturn:
	ret

WriteSecs ENDP

