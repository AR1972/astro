;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; define the function number for the grabber calls that we need.	     ;
;----------------------------------------------------------------------------;

; main functions

GR_INQUIREGRAB		equ	0
GR_ENABLEGRAB		equ	3
GR_DISABLEGRAB		equ	6
GR_PROMPTGRAB		equ	9
GR_INQUIRESAVE		equ	12
GR_SAVESCREEN		equ	15
GR_RESTORESCREEN	equ	18
GR_INITSCREEN		equ	21

; sub functions

GR_DISABLESAVE		equ	0fffbh
GR_ENABLESAVE		equ	0fffch
GR_SETSWAPDRIVE		equ	0fffdh
GR_GETBLOCK		equ	0fff8h
GR_GETINFO		equ	0fffeh
GR_ENDPAINT		equ	0fff4h
GR_BEGINPAINT 		equ	0fff5h

