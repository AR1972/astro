;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;
;OVERVIEW
;	
;	This module contains the first bytes of the .exe file.  The
;first bytes indicate that the .exe is a DEVICE DRIVER.  Thus, this is
;a device driver header.  See devini.asm for how device driver calls
;are handled.  (basically just prints a message that the cache cannot
;be loaded as a device driver)

zseg    segment	public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

PUBLIC  header
PUBLIC  devattr

extrn	tempstrategy		:far
extrn	tempinterrupt		:far

	org     0

header:
	dd      -1                      ;device chain link--filled in by dos
devattr dw      0C840h                  ;character device attribute word
	dw      tempstrategy            ;Strategy entry point
	dw      tempinterrupt           ;Interrupt entry point
	db      'bambino'	        ;logical device name

zseg ends

end
