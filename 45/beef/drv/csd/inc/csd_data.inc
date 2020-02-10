;*
;*	CW : Character Windows Drivers
;*
;*	csd_data.inc : Standard CSD data (and DM structure)


;*****************************************************************************

SDDATA	STRUC		;* Screen driver data
	pinstDrv	DW	0		;* driver's pointer to instCur
	vparmCursOn	DW	0		;* cursor on parameter
	vparmCursSize	DW	0		;* cursor size (underline)
	posCurs		DW	0		;* cursor position
	wExtra		DW	0		;* extra value
	fvmCurAdap	DB	0		;* current adapter and monitor
	fvmCurDisp	DB	0		;* current adapter and monitor
	fCurs		DB	0		;* graphics cursor On/off
			DB	0		;* align

;*	* font drawing variables
	ayBox		DW	0		;* # points for a char
	OFF_lpbFont	DW	0		;* first 128 char table
	SEG_lpbFont	DW	0		;*  "
IFDEF	TWINCSD
;for int10 hook, see saveega.asm
	rgbEGAPaletteMirror	DB	16 DUP (0)	;* attr ctrl regs
	bOverScanMirror	DB	0
	bCGAPaletteMirror	DB	0		;* 4-color graphics
	fBlinkEnable	DW	0			;enable blink attribute
ENDIF	;TWINCSD
SDDATA	ENDS

Assert	<(SIZE SDDATA AND 1) EQ 0>		;* MUST BE EVEN SIZE

ifndef SDDATA_NonDefault
cbDataCsd	equ	(size SDDATA)		;* size of data structure
endif ;standard SDDATA

fmemDataCsd	equ	fmemNear		;* must be in near space

;*****************************************************************************

ifndef DM_NonDefault
;*****************************************************************************
;*	* Structure describing Standard Display Modes (in CS)
DM	STRUC
    ;* Mode availability info
	fvmReqAdapDm	DB	?		;* hardware requirements
	fvmReqDispDm	DB	?
	modeDm		DB	?		;* the mode to set

    ;* INST info:
	;* copy the following to INST
	finstDm		DW	?
	axMacDm		DB	?
	ayMacDm		DB	?
	coMacDm		DB	?

	;* font info
	dxCharDm	DB	?
	dyCharDm	DB	?
	dyBaseDm	DB	?
	ifontDm		DB	?

	;* misc
	psVideoDm	DW	?		;* video address

    ;* Extra info
	vparmCursOnDm	DW	?		;* cursor on param (INT 10/#1)
	wExtraDm	DW	?		;* extra info
DM	ENDS
;*****************************************************************************
endif ;DM_NonDefault

pdmInst		EQU	wDriver1Inst		;* pointer to DM info (in CS)

;*****************************************************************************

	include csd_fvm.inc

;*****************************************************************************

