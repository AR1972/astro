;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	segment ordering for MSDOS
;
;

;hkn; START		SEGMENT BYTE PUBLIC 'START'
;hkn; START		ENDS
;hkn; 
;hkn; CONSTANTS	SEGMENT WORD PUBLIC 'CONST'
;hkn; CONSTANTS	ENDS
;hkn; 
;hkn; DATA		SEGMENT WORD PUBLIC 'DATA'
;hkn; DATA		ENDS
;hkn; 
;hkn; TABLE		SEGMENT BYTE PUBLIC 'TABLE'
;hkn; TABLE		ENDS
;hkn; 
;hkn; CODE		SEGMENT BYTE PUBLIC 'CODE'
;hkn; CODE		ENDS
;hkn; 
;hkn; LAST		SEGMENT PARA PUBLIC 'LAST'
;hkn; LAST		ENDS
;hkn; 
;hkn; DOSGROUP    GROUP   START,CONSTANTS,DATA,TABLE,CODE,LAST

; If this is the disk based DOS, then there are just two (real) segments,
; DOSCODE and DOSDATA.  All the initialization and startup code in in the
; DOSCODE segment.  For ROMDOS, the initialization code cannot be in the
; same segment as the resident code, so a new segment, DOSINIT is included
; between the two.
;


DOSCODE		SEGMENT	BYTE PUBLIC 'CODE'
DOSCODE		ENDS

DUM	SEGMENT	PARA PUBLIC 'DUM'
DUM	ENDS

ifndef IS_SHARE	; M038 Removes segment definition in SHARE.EXE
ifdef ROMDOS

DOSINITSEG	SEGMENT WORD PUBLIC 'DOSINITSEG'
DOSINITSEG	ENDS

DUM2	SEGMENT	PARA PUBLIC 'DUM2'
DUM2	ENDS

endif ; ROMDOS
endif		; M038

DOSDATA		SEGMENT	WORD PUBLIC 'DATA'
DOSDATA		ENDS

DOSDATALAST	SEGMENT	WORD PUBLIC 'DATA'
DOSDATALAST	ENDS

DOSGRP		GROUP DOSDATA,DOSDATALAST

