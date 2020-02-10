;***
;mm.inc - macros to write memory model dependent code
;
;	Copyright (c) 1987-1990, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	This file contains definitions of a number of macros which
;	make the writing of memory model dependent code for the
;	8086 a little easier and more portable.
;
;Revision History:
;	05-18-89  SKS	Removed ES references from pointer macros -- DS is used
;	09-01-89  GJF	Fixed copyright date.
;
;*******************************************************************************

; the macro below is used by the assigning long arithmetic routines (almul,
; etc) to handle the model dependent data addressing - in small and middle
; models data addresses default to short (2 byte) addresses. in large model,
; data address are long (4 bytes).

LOAD	macro	to,from
	mov	to,from
	endm


; general code & data size constants & macros

DAT_ADDR_SZ =	2
BDAT_ADDR_SZ =	4

TXT_ADDR_SZ =	2


; Big/Little Endian Definitions for Long Integers

LOWORD	equ	[0]
HIWORD	equ	[2]

; All Model Definitions

BPARGBAS equ	TXT_ADDR_SZ+2	; offset from BP to first argument


; macro for windows fixup with retf #

return	macro	n
	org	$-1
	db	0c2h
	dw	n
	endm
