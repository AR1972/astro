;================= BIOS_IO.INC =========================
COMMENT #

	BIOS_IO.INC

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential


	johnhe - 03/05/89

END COMMENT #
;========================================================


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
