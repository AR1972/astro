;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.

;	ROM DOS Locations
;	Being the locations and sizes (in para's) of ROM DOS ROM modules.

ROMSEG		equ	08000h		; ROM recorded at 70:0

ROM1SEG		equ	08000h		; 1st module contains resident
ROM1SIZ		equ	01000h		; BIOS and DOS

ROM2SEG		equ	09000h		; 2nd module contains complete
ROM2SIZ		equ	01000h		; ROM COMMAND


RESBIOSSEG	equ	08000h
RESBIOSSIZ	equ	00800h

RESDOSSEG	equ	08800h
RESDOSSIZ	equ	00800h

RESCOMSEG	equ	09000h
RESCOMSIZ	equ	01000h
