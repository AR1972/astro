;*
;*	CW : Character Windows Drivers
;*
;*	vio.asm : VIO safe OS/2 installable driver (T+G text)
;*****************************************************************************

	include	csd_head.inc

DM_NonDefault = 1
SDDATA_NonDefault = 1
	include	csd_data.inc		;* standard data

	include scr5.inc
	include scr5data.inc

;*****************************************************************************

	include	csd_code.asm

;*****************************************************************************
					
	GRAPHICSTEXT = 1		;* include graphcis text code
	include scr5.asm		;* scr5.asm + graphcis text
	include csd_std.asm
	include csd_vram.asm
	include csd_save.asm

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************

	END
