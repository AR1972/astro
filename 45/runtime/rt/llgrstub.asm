	TITLE	LLGRSTUB - Link helper for graphics modules
;***
; LLGRSTUB - Link helper for graphics modules
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	When the compiler sees a SCREEN statement with a constant for the
;	screen mode, it will emit a reference to a public label defined
;	in the low level module corresponding to that screen mode. (ie: if
;	SCREEN 1 is seen, a reference to B$CGAUSED will be emitted.)
;	But if the SCREEN statement contains a variable, the compiler will
;	emit a reference to B$GRPUSED which should cause all the screen
;	modules to be linked in.  B$GRPUSED is defined here and this
;	module will reference the mode-specific labels to force linkage.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	GR_TEXT
	useSeg	_DATA

	INCLUDE seg.inc
	INCLUDE idmac.inc

sBegin	GR_TEXT
assumes CS,GR_TEXT

labelNP <PUBLIC,B$GRPUSED>


externNP B$CGAUSED

externNP B$EGAUSED

externNP B$VGAUSED

externNP B$HRCUSED

externNP B$OLIUSED	

sEnd	GR_TEXT

	END
