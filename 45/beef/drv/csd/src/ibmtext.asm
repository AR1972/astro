;*
;*	CW : Character Windows Drivers
;*
;*	ibmtext.asm : general purposes (MDA/CGA/EGA/MCGA/VGA, Text) CSD
;*
;*****************************************************************************

	include	csd_head.inc
	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table

	MDACSD = 1		;enable all drivers
	CGACSD = 1
	EGACSD = 1
	MCGACSD = 1
	VGACSD = 1

	include	genmodes.asm
	include	geninit.asm			;* general FInitCsd


;*****************************************************************************

	include	csd_std.asm		;* standard init/term
	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************
;*	* only include one of the following, as appropriate

	include	csd_vram.asm		;* default procs for direct video I/O
	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************


	END

