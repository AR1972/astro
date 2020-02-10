;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file defines the structure for windows arena header and related       ;
; constants.								     ;
;----------------------------------------------------------------------------;

WinArena STRUC

ARENA_LOCK_COUNT	db	?	;arena lock count
ARENA_OWNER		dw	?	;owner of the arena
ARENA_SIZE		dw	?	;size in paragraphs
ARENA_FLAGS		db	?	;flag byte (type of block)
ARENA_PREV		dw	?	;selector for the previous arena
ARENA_NEXT		dw	?	;selector for next arena
ARENA_HANDLE		dw	?	;handle of the associated block
ARENA_FPREV		dw	?	;previous free arena
ARENA_FNEXT		dw	?	;next free arena

WinArena ENDS

SENTINNEL_ARENA		equ	-1	;sentinnel marker in owner field

;----------------------------------------------------------------------------;	
