;*
;*	CW : Character Windows
;*
;*	keyboard.asm : keyboard interface layer for installable keyboard drivers

	include kernel.inc
	include	indrv.inc
	include	inkbd.inc

;*****************************************************************************

;*	* App supplied callbacks
externFP <UpdateShiftKk>
;*	* CW entries for callbacks
externFP <KeyboardMessage, FTestKeyboardEmpty, SpecialTsrAbort>

;*****************************************************************************

sBegin	DATA
    assumes DS,DGROUP

;*	* INKJ structure
externB	inkj


;*	* INKB structure (maps to structure in "ihnkbd.h")
;*	* exported flags are mapped in to middle of structure !!!!
	PUBLIC	inkb
	PUBLIC	fAbort, fPollKeyboard, fKeyIsUp, fKeyWasUp	;* PUBLIC
	PUBLIC	fNormalKeyboard, fNonAltKey, wRateKeyRepeat

inkb	EQU	THIS BYTE

;*	* Function pointers
		DD	KeyboardMessage
		DD	FTestKeyboardEmpty
		DD	SpecialTsrAbort

fAbort		DW	0
fPollKeyboard	DW	1			;* do it at least once
fKeyIsUp	DB	1
fKeyWasUp	DB	1
wRateKeyRepeat	DW	-1			;* key rate (-1 => default)

fNormalKeyboard	DW	1			;* => non-TSR
fNonAltKey	DB	1			;* => non alt key hit

fDisableExtended DW	0			;* => disable extended

sEnd	DATA


;*****************************************************************************


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,DGROUP
    assumes SS,DGROUP

;*****************************************************************************
;* Init/Enable/Poll


;********** EnableKeyboard **********
;*	entry:	fOn => enable else disable
;*	* Enable or disable the keyboard
;*	exit:	n/a

cProc	EnableKeyboard, <PUBLIC,FAR,ATOMIC>
    parmW	fOn
    localW	hModule
cBegin	EnableKeyboard

	mov	bx,dataOffset inkb
	cCall	inkj.pfnEnableKeyboardKbdInkj, <bx, fOn, sp>
					;* (pinkb, fOn, fExit)
cEnd	EnableKeyboard


;********** PollKeyboard **********
;*	entry:	n/a
;*	* poll the keyboard (for non-interrupt driven systems)
;*	exit:	n/a

labelFP	<PUBLIC, PollKeyboard>
	jmp	inkj.pfnPollKeyboardKbdInkj



;********** DisableExtendedKeyboard **********
;*	entry/exit: n/a
;*	* disable extended keyboard operation
;*	* will be valid after next EnableKeyboard(TRUE)

cPublic	DisableExtendedKeyboard, <ATOMIC>
cBegin	DisableExtendedKeyboard

	mov	fDisableExtended,sp		;* set flag

cEnd	DisableExtendedKeyboard

;*****************************************************************************


;********** MkGetShiftStates **********
;*	entry:	n/a
;*	* get current shift states
;*	exit:	AX = current shift states (MK_ format)

labelFP	<PUBLIC, MkGetShiftStates>
	jmp	inkj.pfnMkGetShiftStatesKbdInkj


;********** SetShiftKk **********
;*	entry:	kk = new shift states
;*	* set shift states
;*	exit:	n/a

labelFP	<PUBLIC, SetShiftKk>
	jmp	inkj.pfnSetShiftKkKbdInkj

;*****************************************************************************
;* Kanji support

IFDEF KANJI
;********** ChAlternateKeytop **********
;*	entry:	chIn
;*	* return alternate key for "alt"ed key
;*	exit:	AL = other VK (or 0 if none)

labelFP	<PUBLIC, ChAlternateKeytop>
	jmp	inkj.pfnChAlternateKeytopKbdInkj

ENDIF ;KANJI


sEnd	KERNEL

;*****************************************************************************

	END
