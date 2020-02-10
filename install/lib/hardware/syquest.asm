;========================================================
COMMENT #

	SYQUEST.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	 Function that checks for a Syquest removeable
	 hard disk device driver.

	 int far SyquestCheck ( void )
 
	 ARGUMENTS:	NONE
	 RETURNS:	int - TRUE if Syquest device
			      driver is installed
			      else FALSE.

	================================================

	johnhe - 08-20-90

END COMMENT #
; =======================================================

INCLUDE model.inc

; =======================================================

.DATA

SyquestStr	db	'SYQ'

LEN_SYQUEST_STR	EQU	$-SyquestStr

.Code

; =======================================================

SyquestCheck PROC FAR USES SI DI DS ES
	ASSUME	ES:NOTHING

	mov	AH,52h
	int	21h			; ES:BX --> first DBP

	push	BX			; Save offset
	mov	AH,30h
	int	21h			; AL == Major version
	pop	DI			; Restore DPB offset to BX

	add	DI,17h			; DOS 2.x offset of NULL device is 17h
	cmp	AL,2			; See if version is really 2.x
	jle	@f
	add	DI,0bh			; Offset for DOS > 2.x is 22h
@@:
	mov	AX,@DATA
	mov	DS,AX

	mov	SI,OFFSET SyquestStr
	mov	CX,LEN_SYQUEST_STR
	cld

NameCmpLoop:
	cmp	DI,0ffffh		; See if ES:DX is xxxx:ffff
	je	NoSyquest

SaveSetup:
	push	CX			; Save name length
	push	DI			; Save ptr to current device
	push	SI			; Save ptr to Syquest string
	add	DI,0ah			; ES:DI --> Device name
	
	repe	cmpsb
	pop	SI
	pop	DI
	pop	CX

	je	IsSyquest
	les	DI,ES:[DI]		; Load ptr to next device.
	jmp	SHORT NameCmpLoop

NoSyquest:
	xor	AX,AX
	jmp	SHORT SyquestReturn

IsSyquest:
	mov	AX,1

SyquestReturn:
	ret

SyquestCheck ENDP

; =======================================================

	END


