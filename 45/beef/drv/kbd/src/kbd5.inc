;*
;*	CW : Character Windows Drivers
;*
;*	kbd5.inc : OS/2 keyboard header
;*****************************************************************************

.286p		;* use 286 instructions (push immediate)

; Keystroke monitor input and output buffers length

cbBuffer	equ	128
cbStack		equ	2048


; Keystroke monitor record structure
;
KBRD	STRUC
MonFlags	dw	?       ; Open, Flush, Close info and original scan code
XlatedChar	db	?       ; Ascii code
XlatedScan	db	?       ; Translated scan code
DBCSStatus	db	?       ; Interim flag
DBCSShift	db	?       ; Reserved (also defined as NLS Shift Status)
ssKbrd		dw	?       ;
timeKbrd	dd	?       ; Time stamp of the keystroke
KbdFlags	dw	?       ;
KBRD	   ENDS

; Keystroke monitor record flags (Scan code is in upper byte)
;
fOpen	equ    0000000000000001b   ; documentation wrong, not used ?
fClose	equ    0000000000000010b   ; documentation wrong, not used ?
fFlush	equ    0000000000000100b   ; documentation wrong, not used ?
mScan	equ    0111111100000000b   ; scan code (hardware)

; Keystroke monitor keyboard flags
;
fBreak	equ    0000000001000000b   ; key is released
fSecond equ    0000000010000000b   ; scan is extended
fAccent equ    0000001000000000b   ; scan is accented
mAction equ    0000000000111111b   ; action field

; Keyboard status (KbdSet/GetStatus) structure
;
KBST	STRUC
cbKbst		DW	0Ah
fsMask		DW	0
chTurnAround	DW	0
fsInterim	DW	0
fsState		DW	0
KBST	ENDS

;*****************************************************************************

;*	* Data for the DOS 3 driver

KDDATA	STRUC		;* Keyboard driver data

;*	* Common driver items:
	pinkbCur	DW	0	;* pointer to INKB
	ssOld		DW	0	;* old state (for translation)
	pmpscvwShift	DW	0	;* shift keys
	pmpscvwPlain	DW	0	;* plain keys

;*	* Driver data	OS/2
	fEnabled	DB	0	;* => keyboard enabled
	fEnableMonitor	DB	0	;* => monitor enabled

	idThread	DW	?	; Thread ID

	hDevice		DW	?	; handle for keyboard device
	hMonitor	DW	?	; handle for keyboard device monitor

	selKthd		DW	?	;* selector for KTHD segment

	ckeyRepeat	DW	?	; count of artificial key repeats
	chPrev		DB	?	; last key hit, CH
	scPrev		DB	?	; last key hit, SC (must follow CH)
	ssPrev		DW	?	; last key hit, SS

KDDATA	ENDS

;*****************************************************************************

;* Thread extra data (pointed to by SS in thread)
KTHD	STRUC
	selAppKthd	DW	?	;* stack segment for main program

	kbrdKthd	DB	(size KBRD) dup (?)
					; kbrdCur for keyboard records

	cbMonKthd	DW	?	; keyboard monitor record length

	rgbBufferInKthd DB	cbBuffer dup (?); Monitor buffer In
	rgbBufferOutKthd DB	cbBuffer dup (?); Monitor buffer Out

;*	* stack (must be at end of KTHD structure)
	rgbStackKthd	DB	cbStack dup (?)
KTHD	ENDS

;*****************************************************************************

PushArg	MACRO	arg_list		;; Push multiple registers
	irp	x,<arg_list>
		push	x
	endm
ENDM

;*****************************************************************************
