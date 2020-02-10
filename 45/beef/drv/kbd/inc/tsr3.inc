;*	CW : Character Windows
;*
;*	tsr3.inc : low level TSR interface for DOS 3 keyboard
;*****************************************************************************

;*****************************************************************************
;*	* types :
;*	TSRC : TSR Commands (in AX)
;*	VER  : Version parameters (in DX)
;*	ACK  : acknowledge return (in AX)
;*	TSRR : TSR Request (TSR want's something from App)
;*****************************************************************************

;*	* init/termination
tsrcInitTerm	EQU	55FEH
verTsr		EQU	0		;* version # for init
verTsrTerm	EQU	-1		;* version for term

;*	* acknowledge to init
ackTsr		EQU	4D4BH			;* "MK"

;*	* getting key & shift state
tsrcInkey	EQU	5500H		;* special get key request

;*	* TSR Requests
tsrrPoll	EQU	0		;* poll TSR for input
tsrrIgnoreAltUp	EQU	1		;* ignore next ALT up
tsrrAbort	EQU	2		;* ESCAPE

;*****************************************************************************
