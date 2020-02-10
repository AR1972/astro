;*
;*	CW : Character Windows Drivers
;*
;*	kbd3.inc : DOS 3 keyboard header
;*****************************************************************************

QUEUE	STRUC		;* queue for BIOS keys
pStartQueue	dw	?
pEndQueue	dw	?
pHeadQueue	dw	?
pTailQueue	dw	?
semInsertQueue	db	?
semRemoveQueue	db	?
QUEUE	ENDS

semQueue	EQU	word ptr (semInsertQueue)
clKbdBuff	EQU	16

RST_IDLE	EQU	0		;* autorepeat states
RST_WAITING	EQU	1
RST_REPEATING	EQU	2

;*****************************************************************************

;*	* Data for the DOS 3 driver

KDDATA	STRUC		;* Keyboard driver data
	pinkbCur	DW	0		;* pointer to INKB

;*	* old pointers
	pfnOldInt09	DD	0		;* old INT 9 handler
	pfnOldInt1b	DD	0		;* old INT 1B handler
	pfnOldInt08	DD	0
	pfnOldInt16	DD	0		;* old INT 16 handler

	ssLastInt	DW	0		;* shift state at last interrupt
	ssOld		DW	0		;* old state (for translation)

;*	* Queue for keys
	queueKb		DB SIZE QUEUE DUP (?)

;*	* Extra buffer for BIOS keys :
	rglKbBuffer	DD	clKbdBuff dup(?)

	pmpscvwShift	DW	0		;* shift keys
	pmpscvwPlain	DW	0		;* plain keys

;;;*	* Misc BYTE variables:
;;	semCopyQueue	DB	0		;* semaphore for CopyQueue
	
	fKeyboardEnabled DB	0		;* => already enabled
	fShiftStateDiddled DB	0		;* => shift state diddled

	;* NOTE: The following two bytes must be together in this order because
	;* they are sometimes loaded as a word (ssCur)
	sslMirror	DB	0		;* mirror of Bios
	sshExtra	DB	0		;* our own extra states

	Int16_CmdBase	DB	0

	rstCur		DB	RST_IDLE
	scRepeatWhich	DB	0
	ckeyRepeat	DW	0
	fRepeatToggle	DB	0
	chPrev		DB	0
	scPrev		DB	0
	ssPrev		DW	0

	fInt15_4F_Supported	DB	0
	fUnhook16	DB	0

KDDATA	ENDS


;*	* Aliases for double bytes
ssCur		EQU	WORD PTR (sslMirror)
wTsrRepeat	EQU	WORD PTR (fKeyIsUpInkb)
keyPrev		EQU	WORD PTR (chPrev)

;*****************************************************************************
