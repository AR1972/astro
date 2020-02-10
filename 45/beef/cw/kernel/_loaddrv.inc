;*	_loaddrv.inc


cINDSBlock	EQU	1
cbBlock		EQU	cINDSBlock * cbIndsMin
cbBuff		EQU	cbIndtMin + cbBlock - cbIndsMin
					; table header plus service block
					;  (INDT contains one INDS)

cbDataInsh	EQU	LO_lpwDataInsh	; from indrv.inc
fNearDataInsh	EQU	HI_lpwDataInsh

IFDEF DOS5
openRO		EQU	00000000000100000b
			;DWFrrrrrISSSrAAA
ENDIF ;DOS5
