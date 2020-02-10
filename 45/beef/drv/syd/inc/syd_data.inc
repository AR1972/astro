;*
;*	CW : Character Windows Drivers
;*
;*	syd_data.inc : Standard SYD data


;*****************************************************************************

SYDATA	STRUC		;* System driver data
	;* nothing needed yet
SYDATA	ENDS

cbDataSyd	equ	(size SYDATA)		;* size of data structure
fmemDataSyd	equ	fmemNear		;* must be in near space

;*****************************************************************************
