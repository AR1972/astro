;
; Real Mode call structure for Simulating Real Mode Interrupts using DPMI
;
RealModeCallStruc	Struc

RegEDI	dd	?
RegESI	dd	?
RegEBP	dd	?
Regres	dd	?
RegEBX	dd	?
RegEDX	dd	?
RegECX	dd	?
RegEAX	dd	?
RegFlg	dw	?
RegES	dw	?
RegDS	dw	?
RegFS	dw	?
RegGS	dw	?
RegIP	dw	?
RegCS	dw	?
RegSP	dw	?
RegSS	dw	?

RealModeCallStruc	Ends

DPMI			EQU	031h

ALLOC_LDT_DESC		EQU	0000h
SEG_TO_DESC		EQU	0002h
SET_SEG_BASE_ADDR	EQU	0007h
SET_SEG_LIMIT		EQU	0008h
ALLOC_DOS_MEM_BLOCK	EQU	0100h
FREE_DOS_MEM_BLOCK	EQU	0101h
SET_REALMODE_INTVEC	EQU	0201h
GET_REALMODE_INTVEC	EQU	0200h
SIMULATE_RELMODE_INT	EQU	0300h
CALL_REALMODE_PROC      EQU     0301h

