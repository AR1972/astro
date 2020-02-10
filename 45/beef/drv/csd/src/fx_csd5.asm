;*
;*	CW : Character Windows Drivers
;*
;*	fx_csd5.asm : Fixed screen driver, OS/2 (linked in)
;*	* DOES NOT INCLUDE "csd_code"
;*	* has data in application's data segment
;*****************************************************************************

	include	csd_head.inc

	include	fxdrv.inc

DM_NonDefault = 1
SDDATA_NonDefault = 1
	include	csd_data.inc			;* standard data

	include scr5.inc

	include scr5data.inc			;* extra data

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

;*****************************************************************************

	include scr5.asm
	include csd_std.asm
	include csd_vram.asm
	include csd_save.asm

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************

	END
