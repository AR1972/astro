;*	CW : Character Windows
;*
;*	scan3.inc : DOS 3 scan codes
;*****************************************************************************

SS_RSHIFT	equ	00001h
SS_LSHIFT	equ	00002h
SS_CONTROL	equ	00004h
SS_ALT		equ	00008h
SS_SCRLOCK	equ	00010h
SS_NUMLOCK	equ	00020h
SS_CAPLOCK	equ	00040h
SS_INSERT	equ	00080h
;; Extra shift states
SS_SPACE	equ	00100h		;* for space bar keyup
SS_EXTENDED	equ	00200h		;* for extended keypad codes
ifdef KANJI
SS_KANA		equ	08000h
endif
SS_SHIFT	equ	(SS_LSHIFT or SS_RSHIFT)

SC_NO_KEY	equ	0
SC_ESC		equ	1
SC_1		equ	2
SC_2		equ	3
SC_3		equ	4
SC_4		equ	5
SC_5		equ	6
SC_6		equ	7
SC_7		equ	8
SC_8		equ	9
SC_9		equ	10
SC_0		equ	11
SC_MINUS	equ	12
SC_EQUALS	equ	13
SC_BACKSPACE	equ	14
SC_TAB		equ	15
SC_Q		equ	16
SC_W		equ	17
SC_E		equ	18
SC_R		equ	19
SC_T		equ	20
SC_Y		equ	21
SC_U		equ	22
SC_I		equ	23
SC_O		equ	24
SC_P		equ	25
SC_LEFT_BRACKET	equ	26
SC_RIGHT_BRACKET	equ	27
SC_ENTER	equ	28
SC_CTRL		equ	29
SC_A		equ	30
SC_S		equ	31
SC_D		equ	32
SC_F		equ	33
SC_G		equ	34
SC_H		equ	35
SC_J		equ	36
SC_K		equ	37
SC_L		equ	38
SC_SEMI_COLON	equ	39
SC_CLOSE_QUOTE	equ	40
SC_OPEN_QUOTE	equ	41
SC_LEFT_SHIFT	equ	42
SC_BACK_SLASH	equ	43
SC_Z		equ	44
SC_X		equ	45
SC_C		equ	46
SC_V		equ	47
SC_B		equ	48
SC_N		equ	49
SC_M		equ	50
SC_COMMA	equ	51
SC_PERIOD	equ	52
SC_SLASH	equ	53
SC_RIGHT_SHIFT	equ	54
SC_MULTIPLY	equ	55
SC_ALT		equ	56
SC_SPACE	equ	57
SC_CAPS_LOCK	equ	58
SC_F1		equ	59
SC_F2		equ	60
SC_F3		equ	61
SC_F4		equ	62
SC_F5		equ	63
SC_F6		equ	64
SC_F7		equ	65
SC_F8		equ	66
SC_F9		equ	67
SC_F10		equ	68
SC_NUM_LOCK	equ	69
SC_SCROLL_LOCK	equ	70
SC_HOME		equ	71
SC_UP_ARROW	equ	72
SC_PAGE_UP	equ	73
SC_SUBTRACT	equ	74
SC_LEFT_ARROW	equ	75
SC_GOTO		equ	76
SC_RIGHT_ARROW	equ	77
SC_ADD		equ	78
SC_END		equ	79
SC_DOWN_ARROW	equ	80
SC_PAGE_DOWN	equ	81
SC_INSERT	equ	82
SC_DELETE	equ	83
SC_SYS_REQ	equ	84
SC_EXTENDED	equ	96
SC_NUMPAD_MIN	equ	SC_HOME
SC_NUMPAD_MAX	equ	SC_DELETE+1
SC_NUMBER_MIN	equ	SC_1
SC_NUMBER_MAX	equ	SC_0+1
ifdef KANJI
SC_KANJI	equ	58
endif
