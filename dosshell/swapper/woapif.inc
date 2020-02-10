;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */



; flags for pifBehavior.

fNoSwitch	equ	00000001b
fGraphics	equ	00000010b
fDestroy	equ	00000100b

; define a flag which says grabber will not need to be loaded, based on
; above flags.

fNoGrabber	equ	00000001b	;fNoGrab + fNoSwitch


; the hot key disabled flags

fNoAltTab	equ	00000001b
fNoAltEsc	equ	00000010b
fNoCtrlEsc	equ	00000100b
fNoAlt		equ	00000011b
fSwitchDisabled equ	00000111b
