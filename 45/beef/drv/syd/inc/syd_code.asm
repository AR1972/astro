;*
;*	CW : Character Windows
;*
;*	syd_code.asm : Start of SYD code

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

	ORG	0H			;* start of .SYD file

;*****************************************************************************

;*	* Format of Start of SYD file
;*	* DO NOT CHANGE THIS FORMAT !!!

lpwDataSyd		label	dword		;* allocated by driver loader
OFF_lpwDataSyd	DW	cbDataSyd	;* at load time:  cbData
					;* after loading: OFF_lpwDataSyd
SEG_lpwDataSyd	DW	fmemDataSyd	;* at load time:  fmemData
					;* after loading: SEG_lpwDataSyd

pinos		DW	0		;* pinos
pincs		DW	0		;* pincs

		DW	cpfnSydMin	;* # of entries in table

rgpfn:
		DW	DoSoundSyd
		DW	LGetTimeSyd

	Assert <(($ - rgpfn) / 2) EQ cpfnSydMin>

;*****************************************************************************

