;*	CW : Character Windows
;*
;*	bios.inc : IBM specific keyboard BIOS/hardware equates
;*****************************************************************************

KbDataPort	equ	60H
KbCtrlPort	equ	61H

KbType		equ	496H		;* absolute address (off ES == 0)

BUFFER_HEAD	EQU	41AH
BUFFER_TAIL	EQU	41CH
BUFFER_START	EQU	480H
BUFFER_END	EQU	482H
