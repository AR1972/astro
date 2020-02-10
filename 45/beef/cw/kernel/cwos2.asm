
;*
;*	CW : Character Windows
;*
;*	cwos2.asm : jump table data for OS/2 (copied to inosDrv)

	include kernel.inc
	include indrv.inc

;*****************************************************************************

IFNDEF DUAL ;!DUAL
	Assert					;* DUAL only
ENDIF ;!DUAL

;*****************************************************************************


externFP	<CWBeginIO,CWEndIO>


;*****************************************************************************

createSeg   CWOS2,CWOS2,BYTE,PUBLIC,CODE

sBegin	CWOS2
	assumes CS,CWOS2
	assumes DS,CWOS2

	PUBLIC	inosDrvOS2
inosDrv: ;* this label is not exported but referenced by _inos.asm
inosDrvOS2:
	include	_inos.asm

sEnd	CWOS2


;*****************************************************************************

	END
