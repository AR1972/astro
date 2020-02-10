;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


; SaveState

SAVE_STATE	struc
VectorTable	db	(V_WINDOWS+6) dup (?)
LTouch		db	LDESCRIBE_SIZE dup (?)
LPointer	db	LDESCRIBE_SIZE dup (?)
LTablet		db	LDESCRIBE_SIZE dup (?)
HPSoftkeys	dw	2 dup (?)
HPCCP		dw	2 dup (?)	   
Devices		db	14 dup (?)
DeviceMapping	db	14 dup (?)
Translators	db	6 dup (?)
Click		db	?
Speed		db	?
SAVE_STATE	ends

FALSE 		equ	0
TRUE		equ	-1

NUM_HP_VECTORS	equ	85

; Size in bytes of undefined areas: HPmouse and HPTrack

HPMOUSE_SIZE	equ	0A0H
TRACK_SIZE	equ	280H

;       Macros for disabling and restoring hardware interrupt enable flag
;
;       The LeaveCrit macro has been updated for the mask problem on
;       the 80286 processor.


EnterCrit   MACRO
        pushf
        cli
ENDM

LeaveCrit   MACRO
        POPFF
ENDM

POPFF   MACRO
        local   a
        jmp     $+3
a       label   near
        iret
        push    cs
        call    a
ENDM				       


HPENTRY 	equ	6FH		; HPSYSTEM interrupt vector
V_WINDOWS	equ	168H		; Windows private vector in the
;					; ..HPSYSTEM vector table
F_SYSTEM	equ	02		; SYSTEM function
SF_START	equ	02		; Start Application subfunction
SF_REPORT_STATE equ	04		; Status inquiry subfunction
;
ROM_SEGMENT	equ	0F000H		; ROM BIOS segment on Vectra
ROM_AREA	equ	0E000H		; ROM BIOS area (0E000h-0FFFFh)

