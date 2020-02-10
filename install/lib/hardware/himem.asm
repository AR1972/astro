;========================================================
COMMENT #

	HIMEM.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	=================================================

	 Function returns the HIMEM version number if it's present else 0
	 If a driver is installed and it's not HIMEM.SYS we return
         a version number of 10 to make sure Himem will not be installed.

	 unsigned GetHimemVer( void )
 
	 ARGUMENTS:	NONE
	 RETURNS:	unsigned - Himem internal revision number or 0

	================================================

	Change Log:

    	Date      Who   No.		Description
  	--------  ---  ----    -------------------------------------
        08/20/90  JAH           Created
        01/23/90  DLB  M002     Close XMSXXXX0 device properly.

END COMMENT #

; =======================================================

INCLUDE	model.inc

; =======================================================


XMS_DEV_NAME    EQU     "XMSXXXX0",0; Name for XMS driver IOCTL open.

VER_10		EQU	1000h
; =======================================================

.DATA

XmsEntry	DD	(?)

DeviceName	db	XMS_DEV_NAME	; Name for EMM IOCTL opens.

.Code

; =======================================================

GetHimemVer PROC USES ES BX SI DI
;	LOCAL	XmsEntry:DWORD

	push	DS
	xor	AX,AX		; Make sure vector 0x2f is initialized
	mov	BX,2fh*4
	mov	DS,AX	
	mov	AX,[BX]
	or	AX,[BX+2]	; Is vector 2fh initialized ?
	pop	DS
	jz	NoXmsDriver

DoXmsCall:
	xor	DX,DX		; Init DX to zero.
	mov	AX,4300h	; Hello XMS?
	int	2fh		; multiplex call
	cmp	AL,80h		; If XMS does not answer with 80h were done.
	jnz	NoXmsDriver


IsDriverHimemSys:
	mov	DX,OFFSET DeviceName ; DS:DX pointer to device name
	mov	AX,3d02h	; Try to open the device
	int	21h
	jnc	CloseDevice
	mov	AX,VER_10	; Return bogus extreme high version number
	jmp	SHORT HimemVerReturn

CloseDevice:
        mov     BX,AX           ; M002: BX = device handle.
	mov	AX,3E00h	; Close device, handle in BX.
	int	21h

	mov	AX,4310h	; Get XMS function pointer
	int	2fh
	mov	WORD PTR XmsEntry,BX
	mov	WORD PTR XmsEntry[2],ES
	mov	AX,00		; Get XMS version request
	call	DWORD PTR [XmsEntry]

	cmp	AH,2		; Must be > version 2.x for BX to be valid
	jl	HimemVerReturn	; internal revision number

	cmp	AH,4		; BUGBUG need to change this when 
	jge	HimemVerReturn	; MS himem driver gets to 4.x

	mov	AX,BX
	jmp	SHORT HimemVerReturn

NoXmsDriver:
	xor	AX,AX

HimemVerReturn:
	ret

GetHimemVer ENDP

; =======================================================

	END
