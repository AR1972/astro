	TITLE	DVPIPE - Support for the PIPE: device
;***
; DVPIPE - device IO support for the PIPE: device
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains the device specific interface for the pipe
;	device.
;
;	The following is the pipe dispatch table.  Each entry is the addr.
;	of the actual working routine.	The routine name is composed by the
;	prefix "PIPE_" and the function name, e.g., PIPE_EOF is the routine
;	name for EOF function.
;
;	B$D_PIPE:
;		 ________
;		|	 |
;		| EOF	 |
;		|--------|
;		| LOC	 |
;		|--------|
;		| LOF	 |
;		|--------|
;		| CLOSE  |
;		|--------|
;		| WIDTH  | *
;		|--------|
;		| RANDIO | = B$ERR_BFM
;		|--------|
;		| OPEN	 |
;		|--------|
;		| BAKC	 |
;		|--------|
;		| SINP	 |
;		|--------|
;		| SOUT	 | = B$PIPE_SOUT in dkio.asm
;		|--------|
;		| GPOS	 | *
;		|--------|
;		| GWID	 | *
;		|--------|
;		| DWID	 | = B$ERR_FC
;		|--------|
;		| BLKIN  | = B$ERR_BFM
;		|--------|
;		| BLKOUT | = B$ERR_BFM
;		|________|
;	* = could use disk io routine with marginal savings.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	USESEG	ER_TEXT
	USESEG	DV_TEXT
	USESEG	DK_TEXT
	USESEG	NH_TEXT
	USESEG	RT_TEXT
	USESEG	OS_TEXT
	USESEG	_DATA
	USESEG	_BSS

	INCLUDE seg.inc
	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE oscalls.inc
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE idmac.inc	; Debugging Macros


sBegin	ER_TEXT
	externNP B$ERR_DNA	;device not available
sEnd

assumes CS,DV_TEXT
sBegin	DV_TEXT

DSPMAC	MACRO	FUNC
	DW	B$ERR_DNA
	ENDM


;Map all dispatch routines to device not available

labelNP <PUBLIC, B$D_PIPE>	
	DSPNAM

sEnd	DV_TEXT

	END
