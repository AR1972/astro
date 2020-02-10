;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

;**	OemConf.inc - Equates configurable by ROM DOS Oems
;
ifdef	ROMEXEC

ROMEXEC_SEG	equ	0C000h		; ROM start address of ROM module
					;  where ROM executable programs
					;  are stored
					;
					; This is where ROM scans start
					; when looking for ROM executable
					; programs.  If set to A000h, 
					; Windows may think we're dickering
					; with video memory and call us
					; a high resolution video app.

endif

DEFAULT_BUFFERS	equ	-1		; if initialized to -1 sysinit
					; will use the logic described
					; in "dodefaultbuff" in sysinit1.asm
DEFAULT_FILES	equ	8
DEFAULT_FCBS	equ	4
DEFAULT_CDSS	equ	5


