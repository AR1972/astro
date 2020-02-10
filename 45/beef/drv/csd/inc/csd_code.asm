;*
;*	CW : Character Windows
;*
;*	csd_code.asm : Start of CSD code

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

	ORG	0H			;* start of .CSD file

;*****************************************************************************

;*	* Format of Start of CSD file
;*	* DO NOT CHANGE THIS FORMAT !!!

lpwDataCsd		label	dword		;* allocated by driver loader
OFF_lpwDataCsd	DW	cbDataCsd	;* at load time:  cbData
					;* after loading: OFF_lpwDataCsd
SEG_lpwDataCsd	DW	fmemDataCsd	;* at load time:  fmemData
					;* after loading: SEG_lpwDataCsd

pinos		DW	0		;* pinos
pincs		DW	0		;* pincs

		DW	cpfnCsdMin	;* # of entries in table

rgpfn:
;*	* init/term
		DW	ImodeGuessCurrentCsd
		DW	FQueryInstCsd
		DW	FInitCsd
		DW	TermCsd	
;*	* special
		DW	MoveHwCursCsd
		DW	FQueryInftCsd
		DW	FGetColorPaletteCsd
		DW	SetColorPaletteCsd
;*	* update
		DW	PrepUpdateCsd
		DW	DoUpdateCsd
		DW	DoneUpdateCsd
		DW	SpecialUpdateCsd
;*	* screen saving routines
		DW	CbSizeVidsCsd
		DW	FSaveVidsCsd
		DW	FRestoreVidsCsd
		DW	SaveVidDataCsd
		DW	RestoreVidDataCsd
		DW	EnableVidsMonitorCsd

		DW	BltArcCsd
		DW	GetCharMapCsd
	Assert <(($ - rgpfn) / 2) EQ cpfnCsdMin>

;*****************************************************************************

