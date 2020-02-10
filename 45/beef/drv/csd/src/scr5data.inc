;*
;*	CW : Character Windows
;*
;*	scr5.inc : screen driver data info (OS/2)


;*****************************************************************************
;*	* Structure describing Vio Modes
DM	STRUC
	fvmReqAdapDm	DB	?		;* hardware requirements
	fvmReqDispDm	DB	?
						;*  LO == adapter, HI == monitor
	finstDm		DW	?		;* flags
	coMacDm		DB	?		;* coMac

	;* font info
	dxCharDm	DB	?
	dyCharDm	DB	?
	dyBaseDm	DB	?
	ifontDm		DB	?

	mdDm		DB	size MD dup (?)	;* mode data
	psVideoDm	DW	?
	vparmCursOnDm	DW	?		;* cursor on param (INT 10/#1)
DM	ENDS

fvmRequired	EQU	[word ptr fvmReqAdapDm]	;* fvmReqAdap & Disp
axMacDm		EQU	[byte ptr mdDm + 4]	;* really a word in mdDm
ayMacDm		EQU	[byte ptr mdDm + 6]	;* really a word in mdDm
;psVideoDm	EQU	[word ptr 0]		;* junk, not used

cbDmMin	EQU	size DM

;*****************************************************************************

SDDATA2	STRUC		;* Screen driver data (0 at driver load)

	;* filler for SDDATA data
	DB	(SIZE SDDATA) DUP (?)

	fUpdating	DB	0		;* between prep and doneupdate
			DB	0		;* align

	atrCurOn	DW	0		;* cursor on parameter
	offMin		DW	0		;* buffer update min
	offMac		DW	0		;* buffer update mac
	curType		DB	cbCtMin dup (?)
						;* cursor parms
	SEG_lpbVideo	DW	0		;* video segment
SDDATA2	ENDS

	Assert		<NOT ((SIZE SDDATA2) AND 1)>

cbDataCsd	EQU	(size SDDATA2)

IFDEF DUALOS2
Assert	<SIZE SDDATA2 GE SIZE SDDATA>
ENDIF ;DUALOS2

;*****************************************************************************

	include csd_fvm.inc

;*****************************************************************************
