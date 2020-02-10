;*
;*	CW : Character Windows Drivers
;*
;*	tables5.asm : OS/2 tables for key lookup
;*****************************************************************************
;*  Adapted from PM "atvkey.asm"
;*   Copyright (C) 1987 by Microsoft Inc.
;*	Adaptations include:
;*		change to old windows VK names
;*		compact everything to 1 byte
;*		removal of general tables
;************************************************************************

NormalTranslateTable:
	DB	5			; #scan code ranges
	DB	01h			; first scan code in range
	DB	01h			; last scan code in range
	DB	VwOfVk VK_ESCAPE	; 01h	Esc

if 0	; No translation for these
	DB	XL_NOXLATE		; 02h	1
	DB	XL_NOXLATE		; 03h	2
	DB	XL_NOXLATE		; 04h	3
	DB	XL_NOXLATE		; 05h	4
	DB	XL_NOXLATE		; 06h	5
	DB	XL_NOXLATE		; 07h	6
	DB	XL_NOXLATE		; 08h	7
	DB	XL_NOXLATE		; 09h	8
	DB	XL_NOXLATE		; 0Ah	9
	DB	XL_NOXLATE		; 0Bh	0
	DB	XL_NOXLATE		; 0Ch	-
	DB	XL_NOXLATE		; 0Dh	=
endif

	DB	0Eh			; first scan code in range
	DB	0Fh			; last scan code in range
	DB	VwOfVk VK_BACK		; 0Eh	Backspace
	DB	VwOfVk VK_TAB		; 0Fh	Tab


if 0	; No translation for these
	DB	XL_NOXLATE		; 10h	Q
	DB	XL_NOXLATE		; 11h	W
	DB	XL_NOXLATE		; 12h	E
	DB	XL_NOXLATE		; 13h	R
	DB	XL_NOXLATE		; 14h	T
	DB	XL_NOXLATE		; 15h	Y
	DB	XL_NOXLATE		; 16h	U
	DB	XL_NOXLATE		; 17h	I
	DB	XL_NOXLATE		; 18h	O
	DB	XL_NOXLATE		; 19h	P
	DB	XL_NOXLATE		; 1Ah	[
	DB	XL_NOXLATE		; 1Bh	]
endif

	DB	1Ch			; first scan code in range
	DB	1Ch			; last scan code in range
	DB	VwOfVk VK_RETURN	; 1Ch	Enter

if 0	; No translation for these
	DB	VwOfVk VK_CONTROL	; 1Dh	Ctrl
	DB	XL_NOXLATE		; 1Eh	A
	DB	XL_NOXLATE		; 1Fh	S
	DB	XL_NOXLATE		; 20h	D
	DB	XL_NOXLATE		; 21h	F
	DB	XL_NOXLATE		; 22h	G
	DB	XL_NOXLATE		; 23h	H
	DB	XL_NOXLATE		; 24h	J
	DB	XL_NOXLATE		; 25h	K
	DB	XL_NOXLATE		; 26h	L
	DB	XL_NOXLATE		; 27h	;
	DB	XL_NOXLATE		; 28h	'
	DB	XL_NOXLATE		; 29h	`
endif

	DB	2Ah			; first scan code in range
	DB	2Ah			; last scan code in range
	DB	VwOfVk VK_SHIFT		; 2Ah	Left Shift

if 0	; No translation for these
	DB	XL_NOXLATE		; 2Bh	\
	DB	XL_NOXLATE		; 2Ch	Z
	DB	XL_NOXLATE		; 2Dh	X
	DB	XL_NOXLATE		; 2Eh	C
	DB	XL_NOXLATE		; 2Fh	V
	DB	XL_NOXLATE		; 30h	B
	DB	XL_NOXLATE		; 31h	N
	DB	XL_NOXLATE		; 32h	M
	DB	XL_NOXLATE		; 33h	,
	DB	XL_NOXLATE		; 34h	.
endif

	DB	35h			; first scan code in range
	DB	54h			; last scan code in range
	DB	VwOfVk VK_DIVIDE	; 35h	/
	DB	VwOfVk VK_SHIFT		; 36h	Right Shift
	DB	VwOfVk VK_PRINT		; 37h	PrtSc
	DB	VwOfVk VK_MENU		; 38h	Alt
	DB	VwOfVk VK_SPACE		; 39h	Space
	DB	VwOfVk VK_CAPLOCK	; 3Ah	Caps Lock
	DB	VwOfVk VK_F1		; 3Bh	F1
	DB	VwOfVk VK_F2		; 3Ch	F2
	DB	VwOfVk VK_F3		; 3Dh	F3
	DB	VwOfVk VK_F4		; 3Eh	F4
	DB	VwOfVk VK_F5		; 3Fh	F5
	DB	VwOfVk VK_F6		; 40h	F6
	DB	VwOfVk VK_F7		; 41h	F7
	DB	VwOfVk VK_F8		; 42h	F8
	DB	VwOfVk VK_F9		; 43h	F9
	DB	VwOfVk VK_F10		; 44h	F10
	DB	VwOfVk VK_NUMLOCK	; 45h	Num Lock
	DB	VwOfVk VK_SCRLOCK	; 46h	Scroll Lock
	DB	VwOfVk VK_HOME		; 47h	Home
	DB	VwOfVk VK_UP		; 48h	Up
	DB	VwOfVk VK_PRIOR		; 49h	PgUp
	DB	VwOfVk VK_SUBTRACT	; 4Ah	Minus
	DB	VwOfVk VK_LEFT		; 4Bh	Left
	DB	VwOfVk VK_NUMPAD5	; 4Ch	numpad-5
	DB	VwOfVk VK_RIGHT		; 4Dh	Right
	DB	VwOfVk VK_MULTIPLY	; 4Eh	Multiply
	DB	VwOfVk VK_END		; 4Fh	End
	DB	VwOfVk VK_DOWN		; 50h	Down
	DB	VwOfVk VK_NEXT		; 51h	PgDn
	DB	VwOfVk VK_INSERT	; 52h	Ins
	DB	VwOfVk VK_DELETE	; 53h	Del
	DB	VwOfVk VK_CANCEL	; 54h	Sys Req

;*	* end of normal table

;*****************************************************************************
