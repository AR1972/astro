;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1988 - 1991
; *                      All Rights Reserved.
; */
;
; Check the mode of the console, and return the number of lines available.
; This function is used in MEM's /P command, to determine the number of lines
; to be printed before a keypress is requested.
;
; On error, return 0.

.MODEL	SMALL

include rombios.inc

.CODE

public _get_lines

_get_lines	proc	near
	push	ds
	mov	ax, ROMBIOS_DATA

	mov	ds, ax
	assume	ds:ROMBIOS_DATA

	mov	ah, 0
	mov	al, CRT_ROWS

	pop	ds
	assume	ds:DGROUP

ifndef JAPAN
	inc	al
endif

	cmp	al, 1
	ja	@f
	mov	al, 0	; Indicate an error (like what?)
@@:	ret

_get_lines	endp

end

