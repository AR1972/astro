;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;
; device driver headder
;
CHAR_DEV	equ	1000000000000000b

;
; device driver commands
;
DEV_INIT	equ	0
DEV_REST	equ	20

;
; device driver error
;
DEV_INVALID	equ	3

DEV_ERROR	equ	10000000b
DEV_DONE	equ	00000001b

;
; device driver request headder
;
request		struc
req_len		db	?
unit		db	?
command		db	?
status		dw	?
reserve		db	8 dup (?)
count		db	?
break_adrs	dd	?
bpb_adrs	dd	?
dev_num		db	?
request		ends

;
; KKCFUNC return code
;
SUCCESS		equ	0

OVER_ERR	equ	-1
HANDLE_ERR	equ	-2

;
; dos function call definition
;
BUSY		equ	1
NOT_BUSY	equ	0

ERR		equ	-1

MAX_KKC_NUM	equ	16

DOS_SEG		equ	70h
DOS_SAVE_ST	equ	564h
DOS_SAVE_END	equ	920h
DOS_SAVE_LEN	equ	DOS_SAVE_END - DOS_SAVE_ST + 1

;
; dos function call error code
;
INVALID_FUNC	equ	1
FUNC_BUSY	equ	2
CRITICAL_ERR	equ	3

