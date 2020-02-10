;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
debug_version	=	00000h		; enables alt-f8

LINE_LEN	=	128

;	command flags

INS_MODE	=	00000001B
EDITED		=	00000010B
OLDINP		=	00000100B	; there's still old input to scan
MAC_EXPAND	=	00001000B	; valid when OLDINP is, expanding macro

;	some common ascii values

bell		=	7
tab		=	9
lf		=	10
cr		=	13
ctrl_t		=	'T' and 1fh
ctrl_u		=	'U' and 1fh


; DOS INTERRUPTS

DOS_INT		=	21h
I2F_INT		=	2fh
IDLE_INT	=	28h
TSR_INT		=	27h

; DOS INT21H SUBFUNCTIONS

; Cpmio functions

CHAR_OUTPUT		=	02h
CHAR_INP_NO_ECHO	=	08h
BUFFERED_INPUT		=	0Ah
CHECK_INPUT_STATUS	=	0BH

ifdef	DBCS
ifdef   KOREA
; We can not use this functions because of lack of function
; ECS call related functions
;ECS_CALL                =       63h
;SET_GET_interim         =       01h             ; Subfunction of ECS call
;SET_interim             =       01h
CHECK_INPUT_INTERIM     =       0f1h
CHAR_INPUT_INTERIM      =       0f0h
CHAR_INTERIM            =       0f0h
CHAR_FINAL              =       0f1h
HANJA_AT_CCP            =       0f2h
endif	; KOREA
endif	; DBCS


;	note:  the our_version value is reported by our resident portion's
;	  int2f(4800h) along with its resident data segment.  This allows
;	  the transient part to verify that it's working with a compatible
;	  resident portion.  The version number needs to change only when the
;	  data variables which describe the workspace change.  The only
;	  thing that really matters right now about choosing a version
;	  number is that it is NOT 4800h, because a return code of 4800h
;	  is the indication that nobody hooked that interrupt.


our_version	=	0aa02h	; version stamp

