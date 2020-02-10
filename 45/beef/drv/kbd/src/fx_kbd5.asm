;*
;*	CW : Character Windows Drivers
;*
;*	fx_kbd5.asm : OS/2 Fixed keyboard driver (linked in)
;*	* DOES NOT INCLUDE "kbd_code"
;*	* has data in application's data segment
;*****************************************************************************

DOS5	= 1			;* special OS/2 flag for table variations

	include	kbd_head.inc
	include	fxdrv.inc

	include scan3.inc
	include	kbd5.inc

	include	kbd_data.inc

;*****************************************************************************

;*	* Special DATA
	include	fx_data.asm

;*****************************************************************************

sBegin	DRV
    assumes CS,DRV
    assumes ds,NOTHING
    assumes ss,NOTHING

;*	* There is no low memory structure for the linked driver
OFF_lpwDataKbd	DW	dataOffset rgwDataKbd

;*****************************************************************************

;*	* keyboard tables go here

	include keytable.asm

;*****************************************************************************

;*	* Main routines

	include	keymon5.asm			;* monitor for keyboard

	include	keyxlat.asm			;* key translations

	include	kbd_ibm.asm			;* IBM helpers etc

;*****************************************************************************

	include	kbd_std.asm		;* standard init/term

;*****************************************************************************

	include	kbd_tail.asm		;* tail file

;*****************************************************************************

	END
