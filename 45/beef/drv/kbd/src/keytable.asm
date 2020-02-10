;*
;*	CW : Character Windows Drivers
;*
;*	keytable.asm : IBM Scan code translation tables
;*		(see magic in KEYPOLL.ASM for details)
;*****************************************************************************

;*	* MPSCVK : scan code -> virtual key
MPSCVW	STRUC	;* one element in array
	scMp	DB	?		;* Scan Code
	vwMp	DB	?		;* Windows Virtual key
MPSCVW	ENDS

;*	* DVW : define VW (from VK)
DVW	MACRO	vk	;; define VW
	DB	VwOfVk(vk)
ENDM

rgvwNumpad	EQU THIS BYTE
	DVW	VK_HOME
	DVW	VK_UP
	DVW	VK_PRIOR
	DVW	VK_SUBTRACT
	DVW	VK_LEFT
	DVW	VK_CLEAR
	DVW	VK_RIGHT
	DVW	VK_ADD
	DVW	VK_END
	DVW	VK_DOWN
	DVW	VK_NEXT
	DVW	VK_INSERT
	DVW	VK_DELETE

rgvwNumeric	EQU THIS BYTE
	DVW	VK_NUMPAD7
	DVW	VK_NUMPAD8
	DVW	VK_NUMPAD9
	DVW	VK_SUBTRACT
	DVW	VK_NUMPAD4
	DVW	VK_NUMPAD5
	DVW	VK_NUMPAD6
	DVW	VK_ADD
	DVW	VK_NUMPAD1
	DVW	VK_NUMPAD2
	DVW	VK_NUMPAD3
	DVW	VK_NUMPAD0
	DVW	VK_DECIMAL

;* Scan Code to Virtual Key map *

mpscvwAlt:
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
	MPSCVW	<129,	'0'>
	MPSCVW	<139,	VwOfVk(VK_F11)	>		;/* RONCO only */
	MPSCVW	<140,	VwOfVk(VK_F12)	>		;/* RONCO only */
	MPSCVW	<165,	VwOfVk(VK_TAB)	>		;/* RONCO only */
ifdef OAX
	MPSCVW	<0aaH,	VwOfVk(VK_CONVERT) >		;/* OAX only */
	MPSCVW	<0aeH,	VwOfVk(VK_NONCONVERT) > 	;/* OAX only */
	MPSCVW	<0d5H,	VwOfVk(VK_OAX)	>		;/* OAX only */
endif
	MPSCVW	<0,	0>


mpscvwAltNoChar:	;* alt cases if no char
	MPSCVW	<26,	'['>
	MPSCVW	<27,	']'>
	MPSCVW	<39,	';'>
	MPSCVW	<40,	39>			;* '''
	MPSCVW	<41,	'`'>
	MPSCVW	<43,	'\'>
	MPSCVW	<51,	','>
	MPSCVW	<52,	'.'>
	MPSCVW	<53,	'/'>
	MPSCVW	<130,	'-'>
	MPSCVW	<131,	'='>
	MPSCVW	<0,	0>

mpscvwCtrl:
	MPSCVW	<115,	VwOfVk(VK_LEFT)	>
	MPSCVW	<116,	VwOfVk(VK_RIGHT)>
	MPSCVW	<117,	VwOfVk(VK_END)	>
	MPSCVW	<118,	VwOfVk(VK_NEXT)	>
	MPSCVW	<119,	VwOfVk(VK_HOME)	>
	MPSCVW	<132,	VwOfVk(VK_PRIOR)>
	MPSCVW	<137,	VwOfVk(VK_F11)	>		;/* RONCO only */
	MPSCVW	<138,	VwOfVk(VK_F12)	>		;/* RONCO only */
	MPSCVW	<141,	VwOfVk(VK_UP)	>		;/* RONCO only */
	MPSCVW	<143,	VwOfVk(VK_CLEAR) >		;/* RONCO only */
	MPSCVW	<145,	VwOfVk(VK_DOWN)	>		;/* RONCO only */
	MPSCVW	<146,	VwOfVk(VK_INSERT) >		;/* RONCO only */
	MPSCVW	<147,	VwOfVk(VK_DELETE) >		;/* RONCO only */
ifdef OAX
	MPSCVW	<0a9H,	VwOfVk(VK_CONVERT) >		;/* OAX only */
	MPSCVW	<0adH,	VwOfVk(VK_NONCONVERT) > 	;/* OAX only */
	MPSCVW	<0d4H,	VwOfVk(VK_OAX)	>		;/* OAX only */
endif
	MPSCVW	<0,	0>

	public mpscvwAltRonco
mpscvwAltRonco:
	MPSCVW	<151,	VwOfVk(VK_HOME)	>
	MPSCVW	<152,	VwOfVk(VK_UP)	>
	MPSCVW	<153,	VwOfVk(VK_PRIOR) >
	MPSCVW	<155,	VwOfVk(VK_LEFT)	>
	MPSCVW	<157,	VwOfVk(VK_RIGHT) >
	MPSCVW	<159,	VwOfVk(VK_END)	>
	MPSCVW	<160,	VwOfVk(VK_DOWN)	>
	MPSCVW	<161,	VwOfVk(VK_NEXT)	>
	MPSCVW	<162,	VwOfVk(VK_INSERT) >
	MPSCVW	<163,	VwOfVk(VK_DELETE) >
	MPSCVW	<0,	0>

	public mpscvwShiftRonco
mpscvwShiftRonco:
	MPSCVW	<135,	VwOfVk(VK_F11)	>		;/* RONCO only */
	MPSCVW	<136,	VwOfVk(VK_F12)	>		;/* RONCO only */
ifdef OAX
	MPSCVW	<0a8H,	VwOfVk(VK_CONVERT) >		;/* OAX only */
	MPSCVW	<0acH,	VwOfVk(VK_NONCONVERT) > 	;/* OAX only */
	MPSCVW	<0d3H,	VwOfVk(VK_OAX)	>		;/* OAX only */
endif
	MPSCVW	<0, 0>

	public mpscvwShift
mpscvwShift:
ifdef OAX
	MPSCVW	<0a8H,	VwOfVk(VK_CONVERT) >		;/* OAX only */
	MPSCVW	<0acH,	VwOfVk(VK_NONCONVERT) > 	;/* OAX only */
	MPSCVW	<0d3H,	VwOfVk(VK_OAX)	>		;/* OAX only */
endif
	MPSCVW	<0,	0>

	public	mpscvwPlain
mpscvwPlain:
ifdef OAX
	MPSCVW	<0a7H,	VwOfVk(VK_CONVERT) >		;/* OAX only */
	MPSCVW	<0abH,	VwOfVk(VK_NONCONVERT) > 	;/* OAX only */
	MPSCVW	<0d2H,	VwOfVk(VK_OAX)	>		;/* OAX only */
endif
	MPSCVW	<0,	0>


	public	mpscvwPlainRonco
mpscvwPlainRonco:
	MPSCVW	<133,	VwOfVk(VK_F11)	>		;/* RONCO only */
	MPSCVW	<134,	VwOfVk(VK_F12)	>		;/* RONCO only */
ifdef OAX
	MPSCVW	<0a7H,	VwOfVk(VK_CONVERT) >		;/* OAX only */
	MPSCVW	<0abH,	VwOfVk(VK_NONCONVERT) > 	;/* OAX only */
	MPSCVW	<0d2H,	VwOfVk(VK_OAX)	>		;/* OAX only */
endif
	MPSCVW	<0, 0>

;*****************************************************************************
