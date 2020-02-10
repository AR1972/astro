ifndef INC_LIST
.xlist
endif
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;   (C) Copyright COMPAQ Computer Corp. 1986-1991
;
;   Title:    EMM386 - MICROSOFT Expanded Memory Manager 386
;
;   Module:   PAGE.INC - Definitions for paging on 386
;
;   Version:  0.02
;
;   Date:     January 31, 1986
;
;   Author:
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   01/31/86  Original
;   05/12/86  A-RRH	Cleanup and segment reorganization
;   06/28/86  0.02	Name change from CEMM386 to CEMM (SBP).
;
;******************************************************************************
;
;   Functional Description:
;
;******************************************************************************

P_SHIFT		equ	12							; page shift
P_SIZE		equ	(1 SHL P_SHIFT)	; page size

P_PRES		equ	01h		; page present bit
P_WRITE 	equ	02h		; write access bit
P_USER		equ	04h		; access bit for User mode
P_PWT		equ	08h		; page write transparent bit (only)
P_PCD		equ	10h		; page cache disable bit (486 only)
P_ACC		equ	20h		; page accessed bit
P_DIRTY 	equ	40h		; page dirty bit


P_AVAIL 	equ	(P_PRES+P_WRITE+P_USER+P_ACC+P_DIRTY) ; avail to everyone & present


;******************************************************************************
;	MACROS
;******************************************************************************
PAGE_ENTRY	macro	addr,stat
		dd	addr		; access & status in low 12
					; address in high 20
endm

;************************
; PDOff - page dir offset
;   ENTRY: (E)reg = register containing 32 bit linear addr
;	       = AX,BX,CX,DX,SI,DI
;   EXIT: (E)reg = offset for page dir entry for this addr
;************************
PDOff	macro	reg
	shr	E&reg,22	; EREG[0:9] = EREG[22:31] = high 10 bits
	shl	&reg,2		; *4 for dword indexing into table
endm

;************************
; PTOff - page table offset
;   ENTRY: (E)reg = register containing 32 bit linear addr
;	       = AX,BX,CX,DX,SI,DI
;   EXIT: (E)reg = offset for page table entry for this addr
;************************
PTOff	macro	reg
	shr	E&reg,12	; EREG[0:9] = EREG[12:21] = middle 10 bits
	and	&reg,3FFh ; only EREG[0:9]
	shl	&reg,2		; *4 for dword indexing into table
endm

.list		; end of PAGE.INC
