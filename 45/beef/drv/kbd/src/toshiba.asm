;*
;*	CW : Character Windows Drivers
;*
;*	toshiba.asm : Toshiba ??? DOS 3 installed keyboard driver
;*****************************************************************************

	include	kbd_head.inc

	include	kbd3.inc
	;* special stuff for DOS 3 driver
	include scan3.inc
	include tsr3.inc
	include bios.inc

	include	kbd_data.inc

;*****************************************************************************

	include	kbd_code.asm

TOSHIBA_NUMPAD = 1	; special numpad handling

;*****************************************************************************

;*	* keyboard tables go here

	include keytable.asm			;* Scan code tables

	include	kbd_ibm.asm			;* IBM helpers etc

;*****************************************************************************

;*	* Main routines

	include keyacc3.asm			;* accessory routines

	include	keyddl3.asm			;* shift diddle tables

	include	keyint3.asm			;* interrupt

	include	keyxlat.asm			;* polling translations

;*****************************************************************************

	include	kbd_std.asm		;* standard init/term

;*****************************************************************************

	include	kbd_tail.asm		;* tail file

;*****************************************************************************

	END
