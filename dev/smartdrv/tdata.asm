;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

PUBLIC	drives_to_cache
PUBLIC	first_instance
PUBLIC	ending_address
PUBLIC  shutupmsdosflag
PUBLIC  warnmsdosmessage

zseg    segment public 'CODE'

	assume  cs:zseg
	assume	ds:nothing

ending_address	dw	?

;default to a 2 meg cache with 8k elements
warnmsdosmessage		db	0
drives_to_cache			db	26	dup(0)
first_instance			db	1	;0 if bambi is already resident
shutupmsdosflag			db	0

sseg	segment para stack 'STACK'
		dw	768	dup ('!')		;stack for write-behind
transient_stack	dw	0
public transient_stack
sseg	ends
zseg ends

end
