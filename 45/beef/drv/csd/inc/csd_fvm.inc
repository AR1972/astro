;*
;*	CW : Character Windows Drivers
;*
;*	csd_fvm.inc : Standard fvm values


;*****************************************************************************

;*	* flags for IBM adapters
fvmMDA		EQU	00000001B
fvmCGA		EQU	00000010B
fvm64KEGA	EQU	00000100B
fvmEGA		EQU	00001000B
fvmEGAM		EQU	00010000B
fvmMCGA		EQU	00100000B
fvmVGA		EQU	01000000B
fvmMouse	EQU	10000000B	; check EGA.SYS (extend int10)

;*	* flags for IBM monitors
fvmMD		EQU	00000001B
fvmCD		EQU	00000010B
fvmECD		EQU	00000100B

;*	* flags for Hercules adapters
fvmHerc102	EQU	00000001B	;* Graphics
fvmHerc112	EQU	00000010B	;* Graphics Plus (RAMfont)
fvmHerc222	EQU	00000100B	;* In-Color

;*	* flags for Amstrad adapters
fvmAmstradMD	equ	00000001B	;* Monochrome/Hercules compatible
fvmAmstradCD	equ	00000010B	;* CGA compatible
fvmAmstradECD	equ	00000100B	;* EGA compatible
fvmAmstradCM	equ	00001000B	;* PC1512 color monitor

;*****************************************************************************
