;========================================================

COMMENT #

	VIDEO.INC

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential

	Definitions for assembly language video fuctions.
	Must be include in BIOS_IO.ASM

	johnhe - 03/03/89
		 01/12/90	Converted to lib form

END COMMENT #

;========================================================

VIDEO_INT         EQU     10h         ; Video interrupt
KB_INT            EQU     16h         ; Keyboard interrupt
SET_VIDEO_MODE    EQU     00h         ; Set video mode function
SET_CURS_SIZE     EQU     01h         ; Set cursor size
SET_CURS_POS      EQU     02h         ; Modify cursor posn
GET_CURS_POS      EQU     03h         ; Read current cursor posn
READ_LIGHT_PEN    EQU     04h         ; Read light pen position
SET_VIDEO_PAGE    EQU     05h         ; Set screen page
SCROLL_UP         EQU     06h         ; Scroll screen up
SCROLL_DOWN       EQU     07h         ; Scroll screen down
READ_CHAR_ATTR    EQU     08h         ; Get char & attrib from scr
WRITE_CHAR_ATTR   EQU     09h         ; Write character & attrib
WRITE_CHAR_ONLY   EQU     0ah         ; Write character to scr
SET_GRAPHIC_PAL   EQU     0bh         ; Set graphics palete
WRITE_GRAPHIC     EQU     0ch         ; Write a graphic pixel
READ_GRAPHIC      EQU     0dh         ; Read a graphic pixel
WRITE_TTY         EQU     0eh         ; Write char & move cursor
GET_VIDEO_MODE    EQU     0fh         ; Get video mode & disp pg
MAX_SCR_ROW       EQU     24          ; Max row # on screen
ZERO_FLAG         EQU     40h         ; Bit 6 of the flags
BEL			          EQU     07h         ; ASCII BEL character

ASCII_CR	EQU	13
ASCII_LF	EQU	10

; =======================================================
; Equates
; =======================================================
KBD_INT           EQU   16h     ; Bios keyboard interrupt
KBD_GET_CHAR      EQU   00      ; Get char from keyboard buffer
KBD_IS_WAITING    EQU   01      ; See if character is waiting
KBD_GET_STATUS    EQU   02      ; Return shift status

COLOR_BIT         EQU     000001b
EGA_BIT           EQU     000010b
FAST_BIT          EQU     000100b


; =========================================================================
; Macro which does an explicit NEAR or FAR call based on the memory
; model.
; =========================================================================

call_M	MACRO	Function

IF 	@codesize			; Call adjusted for memory model

	call	FAR PTR Function	; FAR call if LARGE, MED, or HUGE
ELSE
	call	NEAR PTR Function	; Else NEAR call
ENDIF

ENDM
