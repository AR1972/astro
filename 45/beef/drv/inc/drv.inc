;*
;*	CW : Character Windows
;*
;*	drv.inc : Main Driver Header

;*****************************************************************************

memM = 1
?WIN = 0
?PLM = 1
?TF = 1
?DF = 1
?NODATA = 1

	;* standard macros
	include cmacros.inc
	include std.inc

	;* installable driver interface
	include	indrv.inc

;*****************************************************************************
;* Segments

;* all driver code goes in the DRV segment (use drvOffset for offsets)
createSeg   DRV_FIXED,DRV,WORD,PUBLIC,CODE		;* driver

;*****************************************************************************
;* Misc Macros

;********** NonStandard **********
;*	entry: symbol = symbol to define as non-standard ???_NonDefault

NonStandard	MACRO	symbol
symbol&_NonDefault  = 1
ENDM

;*****************************************************************************
