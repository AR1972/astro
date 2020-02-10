; File: UTIL.INC
; NOTE: When making changes to this file, be sure to make equivalent
;	changes to file UTIL.H

UTIL_INC = ON

;============================================================================
;the following EXTRN's are included in ALL files EXCEPT util.asm
;============================================================================
ife	UTIL_ASM
sBegin CP
	EXTRN	ZeroFill:far
	EXTRN	FillUndef:far
	EXTRN	CopyBlk:far
	EXTRN	CbSz:far

	EXTRN	CmpPsdIns:near
	EXTRN	CmpStrIns:near
	EXTRN	MapCpW:near
	EXTRN	MapEsW:near
	EXTRN	RelTempSd_CP:near
sEnd CP
endif
