;*
;*	CW : Character Windows Drivers
;*
;*	bios.inc : Screen BIOS addresses


;*****************************************************************************
;*	* Magic values we are peeking/poking around the BIOS variable with


BIOS_cursor_posn	EQU	450H		;* 8 cursor positions
BIOS_addr_6845		EQU	463H		;* base address
BIOS_crt_mode_set	EQU	465H		;* mode register out
BIOS_info		EQU	487H		;* mode options

;*****************************************************************************

;*	* PORT_ relative to BIOS_addr_6845

PORT_mode		EQU	4

;*****************************************************************************
