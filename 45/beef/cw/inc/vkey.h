/*
	vkey.h : COW virtual keys
	NOTE : same as Windows except VK values are > 255
*/

/***BEGIN_PUBLIC***/	
#ifndef NOVIRTUALKEYCODES	
#define	VK_MIN		0x100

#define	VK_LBUTTON	0x101
#define	VK_RBUTTON	0x102
#define	VK_CANCEL	0x103
#define	VK_MBUTTON	0x104	/* NOT contiguous with L & RBUTTON */
#define	VK_BACK		0x108
#define	VK_TAB		0x109
#define	VK_CLEAR	0x10C
#define	VK_RETURN	0x10D
#define	VK_SHIFT	0x110
#define	VK_CONTROL	0x111
#define	VK_ALT		0x112
#define	VK_PAUSE	0x113
#define	VK_CAPLOCK	0x114

/* special VK_s for Kanji and Kana-Kanji conversion */
#define	VK_KANA		0x115
#define	VK_ROMAJI	0x116
#define	VK_ZENKAKU	0x117
#define	VK_HIRAGANA	0x118
#define	VK_KANJI	0x119
/* note: hole for 1A and 1B */
#define	VK_CONVERT	0x11C
#define	VK_NONCONVERT	0x11D
#define	VK_ACCEPT	0x11E
#define	VK_MODECHANGE	0x11F

#define	VK_ESCAPE	0x11B
#define	VK_SPACE	0x120

#define	VK_PRIOR	0x121
#define	VK_NEXT		0x122
#define	VK_END		0x123
#define	VK_HOME		0x124
#define	VK_LEFT		0x125
#define	VK_UP		0x126
#define	VK_RIGHT	0x127
#define	VK_DOWN		0x128

#define	VK_0		0x130
/* 1..8 */
#define	VK_9		0x139
#define	VK_A		0x141
/* B..Y */
#define	VK_Z		0x15A

#define	VK_SELECT	0x129
#define	VK_PRINT	0x12A
#define	VK_EXECUTE	0x12B
#define	VK_INSERT	0x12D
#define	VK_DELETE	0x12E
#define	VK_HELP		0x12F

#define	VK_NUMPAD0	0x160
#define	VK_NUMPAD1	0x161
#define	VK_NUMPAD2	0x162
#define	VK_NUMPAD3	0x163
#define	VK_NUMPAD4	0x164
#define	VK_NUMPAD5	0x165
#define	VK_NUMPAD6	0x166
#define	VK_NUMPAD7	0x167
#define	VK_NUMPAD8	0x168
#define	VK_NUMPAD9	0x169
#define	VK_MULTIPLY	0x16A
#define	VK_ADD		0x16B
#define	VK_SEPARATOR	0x16C
#define	VK_SUBTRACT	0x16D
#define	VK_DECIMAL	0x16E
#define	VK_DIVIDE	0x16F

#define	VK_F1		0x170
#define	VK_F2		0x171
#define	VK_F3		0x172
#define	VK_F4		0x173
#define	VK_F5		0x174
#define	VK_F6		0x175
#define	VK_F7		0x176
#define	VK_F8		0x177
#define	VK_F9		0x178
#define	VK_F10		0x179
#define	VK_F11		0x17A
#define	VK_F12		0x17B
#define	VK_F13		0x17C
#define	VK_F14		0x17D
#define	VK_F15		0x17E
#define	VK_F16		0x17F

#define VK_OAX		0x180

#define	VK_NUMLOCK	0x190
#define	VK_SCRLOCK	0x191

/* alternative names */
#define	VK_MENU		VK_ALT
#define	VK_CAPITAL	VK_CAPLOCK
#define	VK_OEM_NUMBER	VK_NUMLOCK
#define	VK_OEM_SCROLL	VK_SCRLOCK
#define	VK_SEPARATER	VK_SEPARATOR

#endif /*!NOVIRTUALKEYCODES*/	
/***END_PUBLIC***/	
