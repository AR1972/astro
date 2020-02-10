;*
;*	CW : Character Windows Drivers
;*
;*	fx_data.asm : linked in 3 SYD data
;*****************************************************************************

sBegin	DATA
    assumes DS,DGROUP
    assumes CS,DGROUP

labelB	<PUBLIC, inyj>			;* jump table (see header for INYJ)

	DD	DoSoundSyd
	DD	LGetTimeSyd

	Assert <(($ - inyj) / 4) EQ cpfnSydMin>

sEnd	DATA

;*****************************************************************************
;* Zero initialized data

sBegin	BSS
    assumes DS,DGROUP

IF cbDataSyd NE 0
IFDEF DUALOS2
externW	rgwDataSyd
ELSE ;!DUALOS2
;*	* NOTE: Assertion cbDataSyd(fxdsyd3) >= cbDataSyd(fxdsyd5)
globalW	rgwDataSyd, <((cbDataSyd + 1)/ 2) DUP (?)>	;* Screen driver data
ENDIF ;!DUALOS2
ENDIF

sEnd	BSS

;*****************************************************************************
