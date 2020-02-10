;*
;*	CW : Character Windows Drivers
;*
;*	ibmcvt.asm : PC convertible (LCD) CSD
;*
;*****************************************************************************

	include csd_head.inc
	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:

;* #0 - standard text mode
	DB	0ffh 
	DB	0ffh 
	DB	3				;* mode
	DW	finstText or finstMonoChrome	;* flags
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0B800h				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

;* #1 - mono graphics text 
	DB	0ffh
	DB	0ffh
	DB	6				;* mode
	DW	finstGraphics or finstFont OR finstMonochrome or finstFastScroll
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* reserved

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

segROM	equ	0F000H

;*****************************************************************************

	include update1.asm		;* finitcsd,doupdatecsd
	include cpfont.asm		;* ChrisPFont with 8x8 only

;*****************************************************************************

	include	csd_std.asm		;* standard init/term
	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************

	include	csd_vram.asm		;* default procs for direct video I/O
	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************

	END

