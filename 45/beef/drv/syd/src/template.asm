;*
;*	CW : Character Windows Drivers
;*
;*	template.asm : template for a SYD file
;*		items marked with "||--" must be changed
;*****************************************************************************

	include	syd_head.inc

;||-- OPTIONAL: standard data for driver
	include	syd_data.inc

;*****************************************************************************

	include	syd_code.asm			;* first part of code

;*****************************************************************************

;||-- put any non-standard procedures here

;*****************************************************************************

	include	syd_std.asm		;* standard init/term

;*****************************************************************************

	include	syd_tail.asm		;* tail file

;*****************************************************************************

	END

