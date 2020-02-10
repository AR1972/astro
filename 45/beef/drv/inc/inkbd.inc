KK_EXTENDED	equ	08000h
KK_CAPLOCK	equ	04000h
KK_NUMLOCK	equ	02000h
KK_SCRLOCK	equ	01000h
KK_ALT		equ	00800h
KK_CONTROL	equ	00400h
KK_SHIFT	equ	00200h
KK_VK		equ	001FFh
KK_SC		equ	000FFh

ifdef KANJI
KJ_SC		equ	0ff00h
KJ_KANA 	equ	00080h
KJ_OTHER	equ	00040h
KJ_KK		equ	000b0h
KJ_COUNT	equ	0003fh
endif

lpfnKeyboardMessageInkb	equ	[dword ptr 00000h]
OFF_lpfnKeyboardMessageInkb	equ	[word ptr 00000h]
SEG_lpfnKeyboardMessageInkb	equ	[word ptr 00002h]
lpfnFTestKeyboardEmptyInkb	equ	[dword ptr 00004h]
OFF_lpfnFTestKeyboardEmptyInkb	equ	[word ptr 00004h]
SEG_lpfnFTestKeyboardEmptyInkb	equ	[word ptr 00006h]
lpfnSpecialAbortInkb	equ	[dword ptr 00008h]
OFF_lpfnSpecialAbortInkb	equ	[word ptr 00008h]
SEG_lpfnSpecialAbortInkb	equ	[word ptr 0000Ah]
fAbortInkb	equ	[word ptr 0000Ch]
fPollKeyboardInkb	equ	[word ptr 0000Eh]
fKeyIsUpInkb	equ	[byte ptr 00010h]
fKeyWasUpInkb	equ	[byte ptr 00011h]
wRateKeyRepeatInkb	equ	[word ptr 00012h]
fNormalKeyboardInkb	equ	[word ptr 00014h]
fNonAltKeyHitInkb	equ	[byte ptr 00016h]
fDisableExtendedInkb	equ	[word ptr 00018h]
cbInkbMin	equ	0001Ah

pfnEnableKeyboardKbdInkj	equ	[dword ptr 00000h]
OFF_pfnEnableKeyboardKbdInkj	equ	[word ptr 00000h]
SEG_pfnEnableKeyboardKbdInkj	equ	[word ptr 00002h]
pfnPollKeyboardKbdInkj	equ	[dword ptr 00004h]
OFF_pfnPollKeyboardKbdInkj	equ	[word ptr 00004h]
SEG_pfnPollKeyboardKbdInkj	equ	[word ptr 00006h]
pfnFlushKeyRgchKbdInkj	equ	[dword ptr 00008h]
OFF_pfnFlushKeyRgchKbdInkj	equ	[word ptr 00008h]
SEG_pfnFlushKeyRgchKbdInkj	equ	[word ptr 0000Ah]
pfnMkGetShiftStatesKbdInkj	equ	[dword ptr 0000Ch]
OFF_pfnMkGetShiftStatesKbdInkj	equ	[word ptr 0000Ch]
SEG_pfnMkGetShiftStatesKbdInkj	equ	[word ptr 0000Eh]
pfnSetShiftKkKbdInkj	equ	[dword ptr 00010h]
OFF_pfnSetShiftKkKbdInkj	equ	[word ptr 00010h]
SEG_pfnSetShiftKkKbdInkj	equ	[word ptr 00012h]
pfnChAlternateKeytopKbdInkj	equ	[dword ptr 00014h]
OFF_pfnChAlternateKeytopKbdInkj	equ	[word ptr 00014h]
SEG_pfnChAlternateKeytopKbdInkj	equ	[word ptr 00016h]
cbInkjMin	equ	00018h
cpfnKbdMin	equ	6
