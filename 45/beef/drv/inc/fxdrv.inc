;*
;*	CW : Character Windows
;*
;*	fxdrv.inc : Special Driver stuff for fixed (linked-in) drivers
;*****************************************************************************

;*****************************************************************************

;*	* Create DGROUP
createSeg _DATA,DATA,WORD,PUBLIC,DATA,DGROUP
createSeg _BSS,BSS,WORD,PUBLIC,BSS,DGROUP
defGrp DGROUP,DATA,BSS

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes DS,DRV

externW pinos			;* code segment pointer to INOS

sEnd	DRV

;*****************************************************************************
