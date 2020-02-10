;*
;*	CW : Character Windows Drivers
;*
;*	fx_csd3.asm : Dos 3 Fixed screen driver (IBM or compatible) (linked in)
;*	* DOES NOT INCLUDE "csd_code"
;*	* has data in application's data segment
;*****************************************************************************

	include	csd_head.inc

	include	fxdrv.inc

	include	csd_data.inc

;*****************************************************************************

	include	fx_data.asm

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

;*****************************************************************************

;*	* There is no low memory structure for the linked driver
OFF_lpwDataCsd	DW	dataOffset rgwDataCsd


;*	* Display modes table

	MDACSD = 1		;enable all drivers
	CGACSD = 1
	EGACSD = 1
	MCGACSD = 1
	VGACSD = 1

	include genmodes.asm

	include geninit.asm		;* general FInitCsd


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