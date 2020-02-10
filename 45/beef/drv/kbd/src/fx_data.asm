;*
;*	CW : Character Windows Drivers
;*
;*	fx_data.asm : Fixed driver data (linked in)
;*****************************************************************************

;*	* Special DATA

sBegin	DATA
    assumes DS,DGROUP
    assumes CS,DGROUP

labelB	<PUBLIC, inkj>

	DD	EnableKeyboardKbd
	DD	PollKeyboardKbd
	DD	FlushKeyRgchKbd
	DD	MkGetShiftStatesKbd
	DD	SetShiftKkKbd
	DD	ChAlternateKeytopKbd
	Assert <(($ - inkj) / 4) EQ cpfnKbdMin>

sEnd	DATA

;*****************************************************************************
;* Zero initialized data

sBegin	BSS
    assumes DS,DGROUP

IFDEF DUALOS2
externW	rgwDataKbd
ELSE ;!DUALOS2
;*	* NOTE: Assertion (SIZE KDDATA(fxdkbd3)) >= (SIZE KDDATA(fxdkbd5))
globalW	rgwDataKbd, <((cbDataKbd + 1)/ 2) DUP (?)>	;* Keyboard driver data
ENDIF ;!DUALOS2

sEnd	BSS

;*****************************************************************************
