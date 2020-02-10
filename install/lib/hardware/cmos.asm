;===========================================================================
COMMENT	#

 CMOS.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

 Functions to read information from CMOS RAM
 Converted from Windows 3.0 installation code

END COMMENT #
;===========================================================================

CMOS_PORT	EQU	70H
CMOS_DATA       EQU	71H
CMOS_REG_D2     EQU	1AH
EXT_MEM_LOW	EQU	17h
EXT_MEM_HIGH	EQU	18h

;***************************************************************************

INCLUDE model.inc

.CODE

;---------------------------------------------------------------------------
; Reads into AL into the byte location in CMOS RAM as specified in
; in AH on entry
;
; ENTRY:	AH = Byte offset in CMOS RAM
; RETURNS:	AL = Byte from CMOS memeory
;
;---------------------------------------------------------------------------

ReadCmos   PROC

	mov	AL,AH
	cli
	out	CMOS_PORT, AL
	jmp	SHORT @F
@@:
	in	AL,CMOS_DATA
	sti
	ret

ReadCmos ENDP

;---------------------------------------------------------------------------
; Writes the byte in AH into the byte location in CMOS RAM as specified
; in AL
;
; ENTRY:	AH = Byte offset in CMOS RAM
;		AL = Byte to write at CMOS RAM offset
;
;---------------------------------------------------------------------------

WriteCmos   PROC

	xchg	AH,AL

	cli

	out	CMOS_PORT,AL
	jmp	SHORT @F
@@:
	xchg	AH,AL
	out	CMOS_DATA,AL

	sti
	ret

WriteCmos ENDP

;---------------------------------------------------------------------------

END
