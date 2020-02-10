;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; defines equates for key codes, shift states and WOA used flags             ;
;----------------------------------------------------------------------------;

; define codes for keys which WOA is interested in.

KEY_DUP		equ	0e0h		;duplicate key predecessorcode
KEY_ALT		equ	038h		;ALT key code
KEY_TAB		equ	0fh		;TAB key code
KEY_ESC		equ	01h		;ESC key code
KEY_PRTSC	equ	54h		;PRINT SCRN key code
KEY_OTHERPRTSC  equ	37h		;alternate PRINT SCRN key code
KEY_BREAK	equ	080h		;'realese a key' code

; define the location of the KeyBoard state flag in BIOS data area

KEYSTATE	equ	417h		;0:417h has the state

; define the key states

ST_RSHIFT	equ	00000001b	;right SHIFT pressed
ST_LSHIFT	equ	00000010b	;left SHIFT pressed
ST_SHIFT	equ	00000011b	;mask for shift state
ST_CTRL		equ	00000100b	;CTRL key pressed
ST_ALT		equ	00001000b	;ALT key pressed

; define the hot key bit codes that WOA uses

WOA_ALT_TAB		equ	00000001b	;ALT+TAB hit
WOA_SHIFT_ALT_TAB	equ	00000010b	;SHIFT+ALT+TAB hit
WOA_ALT_ESC		equ	00000100b	;ALT+ESC hit
WOA_SHIFT_ALT_ESC	equ	00001000b	;SHIFT+ALT+ESC hit
WOA_CTRL_ESC		equ	00010000b	;CTRL+ESC
WOA_ALT_PRTSC		equ	00100000b	;ALT+PRINTSCRN
WOA_SPECIAL_SWITCH	equ	01000000b	;let win decide on next task
WOA_ALT_BREAK		equ	10000000b	;ALT key released
WOA_SWITCH		equ	00111111b	;switch masker
WOA_SWITCH_PREV         equ     00001010b       ;mask for switch to previous
WOA_SWITCH_NEXT		equ	00000101b	;mask for switch to nextcopy
WOA_SWITCH_ESC          equ	00001100b       ;switch on escape
WOA_SWITCH_TAB          equ     00000011b	;switch on tab

; define the EDIT key codes

ED_STARTEDIT	equ	00		;start of 2 char edit sequence
ED_F6		equ	64		;F6
ED_RIGHT_ARROW	equ	77		;-->
ED_F1		equ	59		;F1
ED_DEL		equ	83		;DEL
ED_F2		equ	60		;F2
ED_F4		equ	62		;F4
ED_F3		equ	61		;F3
ED_F5		equ	63		;F5
ED_LEFT_ARROW	equ	75		;<--
ED_INS		equ	82		;INS
ED_F7		equ	65		;F7
ED_CANCELEDIT	equ	27		;ESC

