
; ========================================================

PUBLIC rebootsystem

COMMENT #

	REBOOT.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	johnhe - 12/01/89

END COMMENT #

;
;OVERVIEW
;
;	This module contains a routine for rebooting the machine.  The
;code was take from the DOS 5.0 setup program.  Reboot is called when
;the user has requested that the machine be rebooted (ctrl+alt+delete),
;but the cache was not commited at that time so the reboot was delayed 
;until all dirty buffers are commited

;-----------------------------------------------------------------------------;
;	K E Y B O A R D   S C A N   C O D E S				      ;
;-----------------------------------------------------------------------------;

KB_INTERCEPT	EQU	4fh

DEL_KEY		EQU	53h
ALT_SHIFT	EQU	08h
CTL_SHIFT	EQU	04h

WARM_BOOT_CODE	EQU	1234h	

;-----------------------------------------------------------------------------;
;	BIOS DATA AREA LOCATED AT 40:00
;-----------------------------------------------------------------------------;

ROM_DATA SEGMENT AT 040h

	org	17h
KB_FLAG		LABEL BYTE


	org	072h
WarmBootFlag	LABEL WORD

ROM_DATA ENDS

;-----------------------------------------------------------------------------;
;	CPU POWER-ON STARTUP LOCATION AT ffff:00
;-----------------------------------------------------------------------------;

ROM_BIOS SEGMENT AT 0ffffh
	org	0

PowerOnReset	LABEL FAR

ROM_BIOS ENDS

;-----------------------------------------------------------------------------;

;include	MODEL.INC

;-----------------------------------------------------------------------------;

;.CODE
zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing


RebootSystem PROC

	mov	AX,3515h
	int	21h			; Get int 15h vector in ES:BX
	mov	AX,ES			; AX == Segment
	or	AX,BX			; Is this a NULL ptr
	jz	WarmBoot		; If zero we can't do an int 15h

DoInt15:
	mov	ax, seg WarmBootFlag
	mov	ds, ax
	assume	DS:ROM_DATA

	mov	KB_FLAG,ALT_SHIFT OR CTL_SHIFT
	mov	AX,(KB_INTERCEPT SHL 8) OR DEL_KEY
	int	15h			; Put Ctrl/Alt/Del into key buffer

WarmBoot:
	cli
	cld

	mov	ax, seg WarmBootFlag
	mov	ds, ax
	assume	DS:ROM_DATA
	mov	WarmBootFlag, WARM_BOOT_CODE
	jmp	PowerOnReset
		; Jump to the processor power-on address FFFF:0000h

RebootSystem	ENDP


zseg ends

end
