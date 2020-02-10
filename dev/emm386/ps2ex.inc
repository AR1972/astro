;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp. 1986-1991
;
;   Title:    PS2EX.INC Emm386 memory manager
;
;   Module:   PS2EX.INC - Equates and structs for PS/2 adaptor Exclusion
;
;   Version:  0.00
;
;   Date:     7/27/88
;
;   Author: ARR
;
;   Note: Ported from win386 3.0 sources.
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;   07/27/88  0.00
;
;******************************************************************************

;
; Maximum number of cards in a PS/2
;
MAX_CARDS	equ	8

;
; Maximum size of the WIN386 PS/2 adaptor exclude file (win386.ps2)
;
MAXPS2EXFILESZ	equ	8192

;
; Structure of the PS2DATA in ps2table.asm
;
POSCardTable   struc
	TotalOptions	dw	?	; Number of entries
	TabelRev	dw	?	; REV of table definition
	OptionTables	db	?	; Start of OptionStruc structures
POSCardTable   ends

;
; Current value of TabelRev
;
CUR_PS2_TABLE_REV equ	0001h

;
;	    This table describes how to convert the adaptor specific POS
;	    bytes into the list of pages in the SegArray to mark as used
;	    for this adaptor:
;
;	     LookUpPTR = LookUpTab;
;	     SegPTR = pSegArray;
;	     index  = (POS2Byte & POS2Mask) >> POS2Shft;
;	     index |= (POS3Byte & POS3Mask) >> POS3Shft;
;	     index |= (POS4Byte & POS4Mask) >> POS4Shft;
;	     index |= (POS5Byte & POS5Mask) >> POS5Shft;
;	     if(index < LookUpCnt) {
;		LookUpPTR += index;
;		SegPtr += LookUpPTR->StartAddr;
;		for(i=0;i<LookUpPTR->PGLen;i++)
;			*SegPtr++=1;
;	     }
;
;	     POS bytes to be ignored are indicated by POSnMask = POSnShft = 0.
;	     Invalid index values should set StartAddr = PGLen = 0.
;
;	     NOTE to support adaptors with multiple discontiguous memory use
;	     blocks, make more than one entry for the card ID
OptionStruc struc
	OptID	    dw	    ?	    ; 16 bit adaptor ID
	POS2Mask    db	    ?
	POS2Shft    db	    ?
	POS3Mask    db	    ?
	POS3Shft    db	    ?
	POS4Mask    db	    ?
	POS4Shft    db	    ?
	POS5Mask    db	    ?
	POS5Shft    db	    ?
	LookUpCnt   dw	    ?	    ; Number of entries following
	LookUpTab   db	    ?	    ; start of MemAddr Structures
OptionStruc ends

MemAddr struc
	StartPg     db	    ?	    ; 0 based 4K page number of first
				    ;	4k page occupied by adaptor.
	PGLen	    db	    ?	    ; Number of 4K pages occupied
				    ;	(May be 0).
MemAddr ends
