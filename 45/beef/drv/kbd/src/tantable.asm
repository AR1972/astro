;*
;*	CW : Character Windows Drivers
;*
;*	tantable.asm : Tandy 1000 Scan code translation tables
;*		(see magic in TANXLAT.ASM for more details)
;*****************************************************************************

;*	* MPSCVK : scan code -> virtual key
MPSCVW	STRUC	;* one element in array
	scMpVw	DB	?		;* Scan Code
	vwMpVw	DB	?		;* Windows Virtual key
MPSCVW	ENDS

;*	* MPSCCH : scan code -> ascii
MPSCCH	STRUC	;* one element in array
	scMpCh	DB	?		;* Scan Code
	chMpCh	DB	?		;* ASCII Character
MPSCCH	ENDS

;* Scan Code to whatever maps *

mpscvwAlt:
	MPSCVW	<007h,	'6'>			;* Alt Ctrl 6 is special
	MPSCVW	<046h,	VwOfVk(VK_MULTIPLY) >
	MPSCVW	<068h,	VwOfVk(VK_F1)	>
	MPSCVW	<069h,	VwOfVk(VK_F2)	>
	MPSCVW	<06Ah,	VwOfVk(VK_F3)	>
	MPSCVW	<06Bh,	VwOfVk(VK_F4)	>
	MPSCVW	<06Ch,	VwOfVk(VK_F5)	>
	MPSCVW	<06Dh,	VwOfVk(VK_F6)	>
	MPSCVW	<06Eh,	VwOfVk(VK_F7)	>
	MPSCVW	<06Fh,	VwOfVk(VK_F8)	>
	MPSCVW	<070h,	VwOfVk(VK_F9)	>
	MPSCVW	<071h,	VwOfVk(VK_F10)	>
	MPSCVW	<06Ah,	VwOfVk(VK_TAB)	>
	MPSCVW	<078h,	'1'>
	MPSCVW	<079h,	'2'>
	MPSCVW	<07Ah,	'3'>
	MPSCVW	<07Bh,	'4'>
	MPSCVW	<07Ch,	'5'>
	MPSCVW	<07Dh,	'6'>
	MPSCVW	<07Eh,	'7'>
	MPSCVW	<07Fh,	'8'>
	MPSCVW	<080h,	'9'>
	MPSCVW	<081h,	'0'>
	MPSCVW	<08Bh,	VwOfVk(VK_ESCAPE) >
	MPSCVW	<08Ch,	VwOfVk(VK_BACK)	>
	MPSCVW	<08Eh,	VwOfVk(VK_TAB)	>
	MPSCVW	<08Fh,	VwOfVk(VK_RETURN) >
	MPSCVW	<091h,	VwOfVk(VK_UP)	>
	MPSCVW	<092h,	VwOfVk(VK_LEFT)	>
	MPSCVW	<097h,	VwOfVk(VK_DOWN)	>
	MPSCVW	<09Eh,	VwOfVk(VK_SUBTRACT) >
	MPSCVW	<0A0h,	VwOfVk(VK_ADD)	>
	MPSCVW	<0A5h,	VwOfVk(VK_DECIMAL) >
	MPSCVW	<0A6h,	VwOfVk(VK_HOME)	>
	MPSCVW	<0B6h,	VwOfVk(VK_F11)	>
	MPSCVW	<0B7h,	VwOfVk(VK_F12)	>
	MPSCVW	<0EAh,	VwOfVk(VK_RIGHT) >
	MPSCVW	<16,	'Q'>
	MPSCVW	<17,	'W'>
	MPSCVW	<18,	'E'>
	MPSCVW	<19,	'R'>
	MPSCVW	<20,	'T'>
	MPSCVW	<21,	'Y'>
	MPSCVW	<22,	'U'>
	MPSCVW	<23,	'I'>
	MPSCVW	<24,	'O'>
	MPSCVW	<25,	'P'>
	MPSCVW	<30,	'A'>
	MPSCVW	<31,	'S'>
	MPSCVW	<32,	'D'>
	MPSCVW	<33,	'F'>
	MPSCVW	<34,	'G'>
	MPSCVW	<35,	'H'>
	MPSCVW	<36,	'J'>
	MPSCVW	<37,	'K'>
	MPSCVW	<38,	'L'>
	MPSCVW	<44,	'Z'>
	MPSCVW	<45,	'X'>
	MPSCVW	<46,	'C'>
	MPSCVW	<47,	'V'>
	MPSCVW	<48,	'B'>
	MPSCVW	<49,	'N'>
	MPSCVW	<50,	'M'>
	MPSCVW	<0,	0>

mpscchAlt:
	MPSCVW	<046h,	'*'>		;* PRINT
	MPSCCH	<082h,	'-'>
	MPSCCH	<083h,	'='>
	MPSCCH	<089h,	','>
	MPSCCH	<08Ah,	'.'>
	MPSCCH	<09Eh,	'+'>
	MPSCCH	<0A0h,	'-'>
	MPSCCH	<0F1h,	39>
	MPSCCH	<0F2h,	'/'>
	MPSCCH	<0F8h,	';'>
	MPSCCH	<0,	0>

mpscvwCtrl:
	MPSCVW	<001h,	VwOfVk(VK_ESCAPE) >
	MPSCVW	<003h,	'2'>
	MPSCVW	<007h,	'6'>
	MPSCVW	<00Eh,	VwOfVk(VK_BACK)	>
	MPSCVW	<01Ch,	VwOfVk(VK_RETURN) >	;* main ENTER
	MPSCVW	<057h,	VwOfVk(VK_RETURN) >	;* numpad ENTER
	MPSCVW	<05Eh,	VwOfVk(VK_F1)	>
	MPSCVW	<05Fh,	VwOfVk(VK_F2)	>
	MPSCVW	<060h,	VwOfVk(VK_F3)	>
	MPSCVW	<061h,	VwOfVk(VK_F4)	>
	MPSCVW	<062h,	VwOfVk(VK_F5)	>
	MPSCVW	<063h,	VwOfVk(VK_F6)	>
	MPSCVW	<064h,	VwOfVk(VK_F7)	>
	MPSCVW	<065h,	VwOfVk(VK_F8)	>
	MPSCVW	<066h,	VwOfVk(VK_F9)	>
	MPSCVW	<067h,	VwOfVk(VK_F10)	>
	MPSCVW	<06Ah,	VwOfVk(VK_TAB)	>
	MPSCVW	<073h,	VwOfVk(VK_LEFT)	>
	MPSCVW	<074h,	VwOfVk(VK_RIGHT) >
	MPSCVW	<075h,	VwOfVk(VK_END)	>
	MPSCVW	<076h,	VwOfVk(VK_NEXT)	>
	MPSCVW	<077h,	VwOfVk(VK_HOME)	>
	MPSCVW	<084h,	VwOfVk(VK_PRIOR) >
	MPSCVW	<08Dh,	VwOfVk(VK_TAB)	>
	MPSCVW	<090h,	VwOfVk(VK_UP)	>
	MPSCVW	<093h,	VwOfVk(VK_NUMPAD7) >
	MPSCVW	<094h,	VwOfVk(VK_NUMPAD8) >
	MPSCVW	<095h,	VwOfVk(VK_NUMPAD4) >
	MPSCVW	<096h,	VwOfVk(VK_DOWN)	>
	MPSCVW	<09Ah,	VwOfVk(VK_NUMPAD2) >
	MPSCVW	<09Ch,	VwOfVk(VK_NUMPAD0) >
	MPSCVW	<09Dh,	VwOfVk(VK_DELETE) >
	MPSCVW	<09Fh,	VwOfVk(VK_INSERT) >
	MPSCVW	<0A4h,	VwOfVk(VK_DECIMAL) >
	MPSCVW	<0ACh,	VwOfVk(VK_F11)	>
	MPSCVW	<0ADh,	VwOfVk(VK_F12)	>
	MPSCVW	<0E0h,	'0'>
	MPSCVW	<0E1h,	'1'>
	MPSCVW	<0E3h,	'3'>
	MPSCVW	<0E4h,	'4'>
	MPSCVW	<0E5h,	'5'>
	MPSCVW	<0E7h,	'7'>
	MPSCVW	<0E8h,	'8'>
	MPSCVW	<0E9h,	'9'>
	MPSCVW	<0FCh,	VwOfVk(VK_CLEAR) >
	MPSCVW	<0FDh,	VwOfVk(VK_NUMPAD6) >
	MPSCVW	<0,	0>

mpscchCtrl:
	MPSCCH	<001h,	27>			;* ESC
	MPSCCH	<00Ch,	'-'>
	MPSCCH	<00Eh,	127>			;* Backspace
	MPSCCH	<01Ch,	10>			;* main ENTER
	MPSCCH	<057h,	10>			;* numpad ENTER
	MPSCCH	<09Dh,	127>			;* DELETE
	MPSCCH	<0F2h,	'/'>
	MPSCCH	<0F5h,	'='>
	MPSCCH	<0F6h,	';'>
	MPSCCH	<0F7h,	39>			;* '
	MPSCCH	<0F9h,	','>
	MPSCCH	<0FAh,	'.'>
	MPSCCH	<0FBh,	'/'>
	MPSCCH	<0,	0>

	public mpscvwShift
mpscvwShift:
	MPSCVW	<001h,	VwOfVk(VK_ESCAPE) >
	MPSCVW	<00Eh,	VwOfVk(VK_BACK)	>
	MPSCVW	<00Fh,	VwOfVk(VK_TAB)	>
	MPSCVW	<01Ch,	VwOfVk(VK_RETURN) >
	MPSCVW	<049h,	VwOfVk(VK_PRIOR) >
	MPSCVW	<04Ah,	VwOfVk(VK_HOME)	>
	MPSCVW	<04Fh,	VwOfVk(VK_END)	>
	MPSCVW	<051h,	VwOfVk(VK_NEXT)	>
	MPSCVW	<054h,	VwOfVk(VK_F1)	>
	MPSCVW	<055h,	VwOfVk(VK_F2)	>
	MPSCVW	<056h,	VwOfVk(VK_F3)	>
	MPSCVW	<057h,	VwOfVk(VK_F4)	>
	MPSCVW	<058h,	VwOfVk(VK_F5)	>
	MPSCVW	<059h,	VwOfVk(VK_F6)	>
	MPSCVW	<05Ah,	VwOfVk(VK_F7)	>
	MPSCVW	<05Bh,	VwOfVk(VK_F8)	>
	MPSCVW	<05Ch,	VwOfVk(VK_F9)	>
	MPSCVW	<05Dh,	VwOfVk(VK_F10)	>
	MPSCVW	<085h,	VwOfVk(VK_UP)	>
	MPSCVW	<086h,	VwOfVk(VK_DOWN)	>
	MPSCVW	<087h,	VwOfVk(VK_LEFT)	>
	MPSCVW	<088h,	VwOfVk(VK_RIGHT) >
	MPSCVW	<09Bh,	VwOfVk(VK_NUMPAD0) >
	MPSCVW	<0A1h,	VwOfVk(VK_DECIMAL) >
	MPSCVW	<0A2h,	VwOfVk(VK_F11)	>
	MPSCVW	<0A3h,	VwOfVk(VK_F12)	>
	MPSCVW	<0F3h,	VwOfVk(VK_CLEAR) >
	MPSCVW	<0F4h,	VwOfVk(VK_NUMPAD6) >
	MPSCVW	<0,	0>

mpscchShift:
	MPSCCH	<001h,	27>			;* ESC
	MPSCCH	<00Eh,	8>			;* Backspace
	MPSCVW	<01Ch,	13>			;* ENTER
	MPSCVW	<09Bh,	'0'>
	MPSCVW	<0A1h,	'.'>
	MPSCVW	<0F4h,	'6'>
	MPSCCH	<0,	0>

	public	mpscvwPlain
mpscvwPlain:
	MPSCVW	<001h,	VwOfVk(VK_ESCAPE) >
	MPSCVW	<00Eh,	VwOfVk(VK_BACK)	>
	MPSCVW	<00Fh,	VwOfVk(VK_TAB)	>
	MPSCVW	<01Ch,	VwOfVk(VK_RETURN) >
	MPSCVW	<037h,	VwOfVk(VK_MULTIPLY) >
	MPSCVW	<03Bh,	VwOfVk(VK_F1)	>
	MPSCVW	<03Ch,	VwOfVk(VK_F2)	>
	MPSCVW	<03Dh,	VwOfVk(VK_F3)	>
	MPSCVW	<03Eh,	VwOfVk(VK_F4)	>
	MPSCVW	<03Fh,	VwOfVk(VK_F5)	>
	MPSCVW	<040h,	VwOfVk(VK_F6)	>
	MPSCVW	<041h,	VwOfVk(VK_F7)	>
	MPSCVW	<042h,	VwOfVk(VK_F8)	>
	MPSCVW	<043h,	VwOfVk(VK_F9)	>
	MPSCVW	<044h,	VwOfVk(VK_F10)	>
	MPSCVW	<047h,	VwOfVk(VK_HOME)	>
	MPSCVW	<048h,	VwOfVk(VK_UP)	>
	MPSCVW	<049h,	VwOfVk(VK_PRIOR) >
	MPSCVW	<04Bh,	VwOfVk(VK_LEFT)	>
	MPSCVW	<04Dh,	VwOfVk(VK_RIGHT) >
	MPSCVW	<04Fh,	VwOfVk(VK_END)	>
	MPSCVW	<050h,	VwOfVk(VK_DOWN)	>
	MPSCVW	<051h,	VwOfVk(VK_NEXT)	>
	MPSCVW	<052h,	VwOfVk(VK_INSERT) >
	MPSCVW	<053h,	VwOfVk(VK_DELETE) >
	MPSCVW	<098h,	VwOfVk(VK_F11)	>
	MPSCVW	<099h,	VwOfVk(VK_F12)	>
	MPSCVW	<09Bh,	VwOfVk(VK_NUMPAD0) >
	MPSCVW	<0A1h,	VwOfVk(VK_DECIMAL) >
	MPSCVW	<0F3h,	VwOfVk(VK_CLEAR) >
	MPSCVW	<0F4h,	VwOfVk(VK_NUMPAD6) >
	MPSCVW	<0, 0>

mpscchPlain:
	MPSCVW	<001h,	27>		;* Escape
	MPSCVW	<00Eh,	8>		;* Backspace
	MPSCVW	<00Fh,	9>		;* Tab
	MPSCVW	<01Ch,	13>		;* main ENTER
	MPSCVW	<037h,	'*'>		;* PRINT
	MPSCCH	<053h,	127>		;* DELETE
	MPSCCH	<057h,	13>		;* numpad ENTER
	MPSCVW	<09Bh,	'0'>
	MPSCVW	<0A1h,	'.'>
	MPSCVW	<0F4h,	'6'>
	MPSCCH	<0,	0>

;*****************************************************************************
