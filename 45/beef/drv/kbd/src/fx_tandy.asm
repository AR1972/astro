;*
;*	CW : Character Windows Drivers
;*
;*	fx_tandy.asm : Tandy 1000 DOS 3 LINKED-IN keyboard driver
;*	DOES NOT include kbd_code
;*	has data in application's data segment
;*****************************************************************************

	include	kbd_head.inc

	include	fxdrv.inc

	include	kbd3.inc
	;* special stuff for DOS 3 driver
	include scan3.inc
	include tsr3.inc
	include bios.inc

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
lpwDataKbd	EQU	THIS DWORD
OFF_lpwDataKbd	DW	dataOffset rgwDataKbd
SEG_lpwDataKbd	DW	SEG DGROUP

;STD_NUMPAD = 1		; normal DOS3 numpad handling (eats '5')
TANDY_1000 = 1	; special key interrupt handling

;*****************************************************************************

;*	* keyboard tables go here

	include tantable.asm			;* Scan code tables

	include	kbd_ibm.asm			;* IBM helpers etc

;*****************************************************************************

;*	* Main routines

	include keyacc3.asm			;* accessory routines

	include	keyint3.asm			;* interrupt

	include	tanxlat.asm			;* polling translations

;*****************************************************************************

	include	kbd_std.asm		;* standard init/term

;*****************************************************************************

	include	kbd_tail.asm		;* tail file

;*****************************************************************************

	END
