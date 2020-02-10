;*
;*	CW : Character Windows Drivers
;*
;*	keyddl3.asm : DOS 3 keyboard special shift handling tables
;*
;****************************************************************************
;
; The following two data objects are bit-packed boolean arrays
;  whose indices are key scan codes (sc's).
; mpscffAltDiddle[sc] = 1 indicates that ALT-sc needs special 
;  handling to get it past the BIOS.
; mpscffCtrlDiddle[sc] is the same thing for CTRL-sc.

mpscffAltDiddle equ this byte	; Which ALT- keys require diddling
	db	00000000b	; 0-
	db	01100000b	; 8-	'=', Backspace
	db	00000000b	; 16-
	db	00000000b	; 24-
	db	00000000b	; 32-
	db	00000000b	; 40-
	db	00000000b	; 48-
	db	00000000b	; 56-
	db	00000000b	; 64-
	db	01000100b	; 72-	SUBTRACT, LEFT_ARROW, RIGHT_ARROW, ADD
	db	00000000b	; 80-
	db	00000000b	; 88-
	db	00000000b	; 96-
	db	00000000b	; 104-
	db	00000000b	; 112-
	db	00000000b	; 120-

mpscffCtrlDiddle equ this byte	; Which CTRL- keys require diddling
	db	11111100b	; 0-	'1', '2', '3', '4', '5', '6'
	db	10101111b	; 8-	'7', '8', '9', '0', '=', TAB
	db	00000000b	; 16-
	db	00000000b	; 24-
	db	10000000b	; 32-	';'
	db	00000001b	; 40-	"'"
	db	00011000b	; 48-	',' '.'
	db	00000000b	; 56-
	db	00000000b	; 64-
	db	01010101b	; 72-	Up, SUBTRACT, Num5, ADD
	db	00001101b	; 80-	Down, Ins, Del
	db	00000000b	; 88-
	db	00000000b	; 96-
	db	00000000b	; 104-
	db	00000000b	; 112-
	db	00000000b	; 120-

mpscffAltCtrlDiddle equ this byte	; Which ALT+CTRL keys to diddle
	db	00000000b	; 0-
IFNDEF LANGUAGE_SWISS
	db	01010000b	; 8-	'-', Backspace
ELSE ;LANGUAGE_SWISS
	db	01000000b	; 8-	Backspace
ENDIF ;LANGUAGE_SWISS
	db	00000000b	; 16-
	db	00000000b	; 24-
	db	00000000b	; 32-
	db	00000000b	; 40-
	db	00000000b	; 48-
	db	00000000b	; 56-
	db	00000000b	; 64-
	db	00000000b	; 72-
	db	00000000b	; 80-
	db	00000000b	; 88-
	db	00000000b	; 96-
	db	00000000b	; 104-
	db	00000000b	; 112-
	db	00000000b	; 120-

;****************************************************************************
