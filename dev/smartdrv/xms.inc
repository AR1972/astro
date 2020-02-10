;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

XMS_MULTIPLEX		equ	43h	; XMS multiplex number of INT 2Fh
XMS_INSTALL_CHECK	equ	00h	; install check call
XMS_FUNCTION_ADDR	equ	10h	; get control function address call
;
; XMS functions
;
XMS_GET_VERSION		equ	00h
XMS_REQUEST_HMA		equ	01h
XMS_RELEASE_HMA		equ	02h
XMS_GLOBAL_ENABLE_A20	equ	03h
XMS_GLOBAL_DISABLE_A20	equ	04h
XMS_LOCAL_ENABLE_A20	equ	05h
XMS_LOCAL_DISABLE_A20	equ	06h
XMS_QUERY_A20		equ	07h
XMS_QUERY_FREE_EXTMEM	equ	08h
XMS_ALLOC_EMB		equ	09h
XMS_FREE_EMB		equ	0Ah
XMS_MOVE_EMB		equ	0Bh
XMS_LOCK_EMB		equ	0Ch
XMS_UNLOCK_EMB		equ	0Dh
XMS_GET_EMB_INFO	equ	0Eh
XMS_REALLOC_EMB		equ	0Fh
XMS_REQUEST_UMB		equ	10h
XMS_RELEASE_UMB		equ	11h
;
; Valid error codes in BL have this bit set.
; But, only check BL if AX = 0.
;
XMS_ERROR_BIT		equ	80h

;
; struct pass to XMS_MOVE_EMB in DS:SI
;
ExtMemMoveStruct	struc
	mov_length	dd	?	; 32 bit number of bytes to transfer
	src_handle	dw	?	; handle of source block
	src_offset	dd	?	; 32 bit offset into source
	dst_handle	dw	?	; handle of destination block
	dst_offset	dd	?	; 32 bit offset into destination
ExtMemMoveStruct	ends



