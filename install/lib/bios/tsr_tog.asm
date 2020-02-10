; ========================================================

COMMENT #

	TSR_TOG.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 03/03/89

END COMMENT #

;========================================================

MPLX			EQU	2fh

APPEND_ID		EQU	0b7h
APPEND_GET_STATE	EQU	6
APPEND_SET_STATE	EQU	7

ASSIGN_ID		EQU	6

APPEND_GET_DIRLIST	EQU	4
ASSIGN_GET_SEGMENT	EQU	1

MPLX_GET_STATE		EQU	0

APPEND_FLAG		EQU	1b
ASSIGN_FLAG		EQU	10b

DISK_TABLE_LEN		EQU	13	; 26 bytes converted to words

;========================================================

include BIOS_IO.INC
include	MODEL.INC


;========================================================

.DATA

MultiplexFlags		db	0

DisableBytes		db	';', 0	; Replacment 1st 2 bytes of dir list
AppendPtr		dd	(?)	; Address of APPEND dirlist
AssignPtr		dd	0103h
DiskTable		db	1,2,3,4,5,6,7,8,9,10,11,12,13, \
				14,15,16,17,19,20,21,22,23,24,25,26
AppendState		dw	0

; ========================================================

.CODE

; ========================================================
; void DisableAppend( void )
;
; Checks to see if APPEND has been installed and if it has
; disables it.
; ========================================================

DisableAppend PROC USES ES DI

	call	NEAR PTR TestForInt2f
	jz	ProcExit

InstallCheck:
	mov	AX,(APPEND_ID SHL 8) OR MPLX_GET_STATE  ; 0B700h
	int	2fh			; int 2fh
	cmp	AL,0ffh			; See if installed.
	jne	ProcExit		; Not installed so just return

	mov	AX,(APPEND_ID SHL 8) + APPEND_GET_STATE ; 0b706h
	mov	BX,0ffffh
	mov	CX,BX
	int	2fh
	cmp	BX,CX			; Did value in BX change
	je	UseOldMethod
	mov	AppendState,BX

NewMethod:
	xor	BX,BX
	mov	AX,(APPEND_ID SHL 8) + APPEND_SET_STATE ; 0b707h
	int	2fh
	jmp	SHORT ProcExit

UseOldMethod:
	mov	ES,CX			; Set ES to 0xffff
	mov	AX,(APPEND_ID SHL 8) OR APPEND_GET_DIRLIST ; 0B704h
	int	2fh

	mov	BX,ES			; See if ES got changed
	cmp	BX,CX
	jne	GotPtr			; If yes then save ES:DI

	mov	AX,0b701h		; This gets DIR list in 
	int	2fh			; append for DOS 3.2
	mov	BX,ES
	cmp	BX,CX
	je	ProcExit

GotPtr:
	mov	WORD PTR AppendPtr,DI	; Save offset of directory list
	mov	WORD PTR AppendPtr[2],ES ; Save segment of directory list
	mov	AX,WORD PTR DisableBytes ; Put disable bytes in AX
	xchg	AX,WORD PTR ES:[DI]	 ; Do a swap to disable APPEND
	mov	WORD PTR DisableBytes,AX ; Save original first 2 bytes.

ProcExit:
	ret	

DisableAppend ENDP

; ========================================================
; void EnableAppend( void )
;
; Checks to see if APPEND has been installed and if it has
; toggles it active or inactive depending on it's current
; state.
; ========================================================

EnableAppend PROC USES ES DI

	call	NEAR PTR TestForInt2f
	jz	ProcExit

InstallCheck:
	mov	AX,(APPEND_ID SHL 8) OR MPLX_GET_STATE  ; 0B700h
	int	MPLX			; int 2fh
	cmp	AL,0ffh			; See if installed.
	jne	ProcExit		; Not installed so just return

	mov	AX,(APPEND_ID SHL 8) + APPEND_GET_STATE ; 0b706h
	mov	BX,0ffffh
	mov	CX,BX
	int	2fh
	cmp	BX,CX			; Did value in BX change
	je	UseOldMethod

NewMethod:
	mov	BX,AppendState
	mov	AX,(APPEND_ID SHL 8) + APPEND_SET_STATE ; 0b707h
	int	2fh
	jmp	SHORT ProcExit

UseOldMethod:
	mov	ES,CX			; Set ES to 0xffff
	mov	AX,(APPEND_ID SHL 8) OR APPEND_GET_DIRLIST ; 0B704h
	int	2fh

	mov	BX,ES			; See if ES got changed
	cmp	BX,CX
	jne	GotPtr			; If yes then save ES:DI

	mov	AX,0b701h		; This gets DIR list in 
	int	2fh			; append for DOS 3.2
	mov	BX,ES
	cmp	BX,CX
	je	ProcExit

GotPtr:
	mov	WORD PTR AppendPtr,DI	; Save offset of directory list
	mov	WORD PTR AppendPtr[2],ES ; Save segment of directory list
	mov	AX,WORD PTR DisableBytes ; Put disable bytes in AX
	xchg	AX,WORD PTR ES:[DI]	 ; Do a swap to disable APPEND
	mov	WORD PTR DisableBytes,AX ; Save original first 2 bytes.

ProcExit:
	ret	

EnableAppend ENDP
; ========================================================
; void ToggleAssign( void )
;
; Checks to see if ASSIGN has been installed and if it 
; has toggles it active or inactive depending on it's
; current state.
; ========================================================

ToggleAssign PROC USES ES

	call	NEAR PTR TestForInt2f
	jz	ProcExit

	test	MultiplexFlags,ASSIGN_FLAG
	jnz	DoSwap

InstallCheck:
	mov	AX,(ASSIGN_ID SHL 8) OR MPLX_GET_STATE  ; 0600h
	int	MPLX			; int 2fh
	cmp	AL,0ffh			; See if installed.
	jne	ProcExit		; Not installed so just return
	or	MultiplexFlags,ASSIGN_FLAG ; Show Assign is installed
	mov	AX,(ASSIGN_ID SHL 8) OR ASSIGN_GET_SEGMENT ; 0601h
	int	MPLX			; int 2fh
	mov	WORD PTR AssignPtr[2],ES; Save ASSIGN segment

DoSwap:
	call	NEAR PTR SwapDiskTable	; Switch disk table

ProcExit:
	ret	

ToggleAssign ENDP

; ========================================================
; Swaps the 26 bytes in Assign's disk table with those
; in our disk table.
; ========================================================

SwapDiskTable PROC NEAR  USES DS SI ES DI

	les	DI,AssignPtr		; Set up ES:DI

	mov	SI,offset DiskTable	; Set up DS:SI
	mov	CX,DISK_TABLE_LEN	; Number of words in disktable
	cld				; Set direction to forward
StartLoop:
	mov	AX,ES:[DI]		; Put 2 entries into AX
	xchg	AX,[SI]			; Swap with our Disk Table
	stosw				; Put our entries in Assign's table
					; also increments DI twice
	inc	SI			; Increment SI twice
	inc	SI
	loop	StartLoop		; Loop until entire table is swapped
ProcExit:
	ret

SwapDiskTable ENDP


; ========================================================
; Tests to see if the int 2fh vector has been initialized
; by making sure it's not pointing to 00:00
;
; RETURNS:	NZ - If vector is initialize
;		ZR - If vector is not initialized
; ========================================================

TestForInt2f	PROC NEAR

	push	DS
	xor	AX,AX		; Make sure vector 0x2f is initialized
	mov	BX,2fh*4
	mov	DS,AX	
	mov	AX,[BX]
	or	AX,[BX+2]	; Is vector 2fh initialized ?
	pop	DS
	ret

TestForInt2f	ENDP

; ========================================================

	END



