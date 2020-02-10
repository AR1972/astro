;*
;*	CW : Character Windows
;*
;*	_inos.asm : the INOS structure data for OS/2


;*****************************************************************************

INOS_ENTRY MACRO label
	LOCAL	lpfnHere
	externFP <label>
lpfnHere DD	label
	Assert	<(lpfnHere-inosDrv) EQ lpfn&label&Inos>
ENDM	; INOS_ENTRY

	Assert	<($-inosDrv) EQ sdGlisInos>
	DW		0			;* sdGlis

	Assert	<($-inosDrv) EQ sdLoisInos>
	DW		0			;* sdLois

	DW		cpfnInosMin		;* # of entries in INOS

	;* Configuration info
	INOS_ENTRY	DosGetVersion
	INOS_ENTRY	DosGetEnv
	INOS_ENTRY	DosDevConfig
	INOS_ENTRY	DosGetCtryInfo
	INOS_ENTRY	DosGetDBCSEv
	INOS_ENTRY	DosGetInfoSeg

	;* Low level hardware access
	INOS_ENTRY	DosDevIOCtl
	INOS_ENTRY	DosPortAccess

	;* Linkage to DLL entries (for ones not listed here)
	INOS_ENTRY	DosGetModHandle
	INOS_ENTRY	DosLoadModule
	INOS_ENTRY	DosGetProcAddr

	;* Threads
	INOS_ENTRY	DosCreateThread
	INOS_ENTRY	DosSetPrty
	INOS_ENTRY	DosExit

	;* File Routines
	INOS_ENTRY	DosOpen
	INOS_ENTRY	DosClose
	INOS_ENTRY	DosRead
	INOS_ENTRY	DosWrite

	;* Monitor Routines
	INOS_ENTRY	DosMonOpen
	INOS_ENTRY	DosMonClose
	INOS_ENTRY	DosMonReg
	INOS_ENTRY	DosMonRead
	INOS_ENTRY	DosMonWrite

	;* Memory Allocation
	INOS_ENTRY	DosAllocSeg
	INOS_ENTRY	DosReAllocSeg
	INOS_ENTRY	DosFreeSeg

	;* Common VIO Routines
	INOS_ENTRY	VioGetBuf
	INOS_ENTRY	VioShowBuf
	INOS_ENTRY	VioGetConfig
	INOS_ENTRY	VioGetMode
	INOS_ENTRY	VioSetMode
	INOS_ENTRY	VioGetState
	INOS_ENTRY	VioSetState

	INOS_ENTRY	VioGetCurType
	INOS_ENTRY	VioSetCurType
	INOS_ENTRY	VioGetCurPos
	INOS_ENTRY	VioSetCurPos

	INOS_ENTRY	VioGetFont
	INOS_ENTRY	VioSetFont
	INOS_ENTRY	VioGetCP
	INOS_ENTRY	VioSetCP
	INOS_ENTRY	VioScrollUp

	;* Common Kbd Routines
	INOS_ENTRY	KbdOpen
	INOS_ENTRY	KbdClose
	INOS_ENTRY	KbdCharIn
	INOS_ENTRY	KbdGetStatus
	INOS_ENTRY	KbdSetStatus

	;* Misc
	INOS_ENTRY	DosBeep
	INOS_ENTRY	VioGetPhysBuf

	;* NOTE: can't use INOS_ENTRY for internal CW routines
	DD	CwBeginIO
	DD	CwEndIO

cpfnInosMin	equ	($ - inosDrv) / (cbInosMin)	;* cpfn

;*****************************************************************************

