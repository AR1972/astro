;*
;*	CW : Character Windows
;*
;*	kbd_code.asm : Start of KBD code

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

	ORG	0H			;* start of .KBD file

;*****************************************************************************

lpwDataKbd		label	dword		;* allocated by driver loader
OFF_lpwDataKbd	DW	cbDataKbd	;* at load time:  cbData
					;* after loading: OFF_lpwDataKbd
SEG_lpwDataKbd	DW	fmemDataKbd	;* at load time:  fmemData
					;* after loading: SEG_lpwDataKbd

pinos:		DW	0		;* pinos
pincs:		DW	0		;* pincs

		DW	cpfnKbdMin	;* # of entries in table
rgpfn:
		DW	EnableKeyboardKbd
		DW	PollKeyboardKbd
		DW	FlushKeyRgchKbd
		DW	MkGetShiftStatesKbd
		DW	SetShiftKkKbd
		DW	ChAlternateKeytopKbd

	Assert <(($ - rgpfn) / 2) EQ cpfnKbdMin>

;*****************************************************************************

