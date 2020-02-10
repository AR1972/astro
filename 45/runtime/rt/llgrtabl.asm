	TITLE	llgrtabl - graphics mode dispatch table
;***
;llgrtabl - graphics mode dispatch table
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
; Provides the dispatch table that includes all graphics screen modes, if
; graphics statements are used in a users program.
;
;******************************************************************************
	INCLUDE switch.inc	;Product switches
	INCLUDE rmacros.inc	;runtime macros

	useSeg	_DATA		
	useSeg	GR_TEXT
	useSeg	XIB		
	useSeg	XI		
	useSeg	XIE		

	INCLUDE seg.inc 	;segment definitions

	INITIALIZER B$xGRINITTAB    ;Put B$xGRINITTAB in initializer list

sBegin	GR_TEXT			
externNP B$ErrorReturn		
externNP B$Screen0		
sEnd	; GR_TEXT		

sBegin	_DATA			; needs to be in data seg

externW b$ScreenX		; NEAR pointer to screen mode table

labelW	<PUBLIC,b$ScreenTab>	;mode-dependent SCREEN set-up
	DB	MaxScrMode	;Maximum screen mode
	DW	B$Screen0
	DW	B$ErrorReturn	; for screen 1
	DW	B$ErrorReturn	; for screen 2
	DW	B$ErrorReturn	; for screen 3
	DW	B$ErrorReturn
	DW	B$ErrorReturn
	DW	B$ErrorReturn
	DW	B$ErrorReturn	; for screen 7
	DW	B$ErrorReturn	; for screen 8
	DW	B$ErrorReturn	; for screen 9
	DW	B$ErrorReturn	; for screen 10
	DW	B$ErrorReturn	; for screen 11
	DW	B$ErrorReturn	; for screen 12
	DW	B$ErrorReturn	; for screen 13
MaxScrMode  =	($-b$ScreenTab)/2-1   ;max BASIC screen mode

sEnd				; end _DATA

sBegin	GR_TEXT 		
assumes CS,GR_TEXT

cProc	B$xGRINITTAB,<FAR>	
cBegin
	MOV	[b$ScreenX],OFFSET DGROUP:b$ScreenTab	
cEnd

sEnd

	END
