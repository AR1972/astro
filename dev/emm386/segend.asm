	PAGE	, 132
.386p
;******************************************************************************
;
; (C) Copyright MICROSFT Computer Corp. 1989-1991
; (C) Copyright COMPAQ Computer Corp. 1989-1991
;
; Title:	EMM386.EXE - MICROSOFT Expanded Memory Manager 386
;
; Module:	SEGEND.ASM - For defining the end location of the segments
;
; Version:	0.01
;
; Date:		January 18, 1989
;
; Author:	Daniel J. Mazina
;
;******************************************************************************
;
;   Change log:
;
;     DATE    REVISION			DESCRIPTION
;   --------  --------	-------------------------------------------------------
;	      Original
;******************************************************************************

;******************************************************************************
; PUBLICS
;******************************************************************************
public	register_set

;******************************************************************************
; INCLUDES
;******************************************************************************
include	vdmseg.inc
include emmfunct.inc

; The following label definitions are used for locating the ends of the various
; CEMM segments.  This is needed so that CEMM can move individual segments up
; into extended memory or the ROM space easily.  This module must be linked in
; last to insure that these labels occur at the end.

	; This is the code that must remain in the lower 640k of memory.
public	end_of_R_CODE
R_CODE	segment
	end_of_R_CODE	label	byte
R_CODE	ends

	; This is the code that must remain in the lower 640k of memory.
public	end_of_R_STACK
R_STACK segment
	end_of_R_STACK	label	byte
R_STACK	ends

public	end_of_R1_CODE
R1_CODE	segment
	end_of_R1_CODE	label	byte
R1_CODE	ends


	; This used for the common protected mode data area.
public	end_of_DATA
_DATA		segment
	ASSUME	cs:_DATA,ds:_DATA

register_set	label	word
	RegisterSet_struc	<>

	end_of_DATA	label	byte
_DATA		ends

	; This used for the ring zero stack frame.
public	end_of_STACK
STACK	segment
	end_of_STACK	label	byte
STACK	ends

	; This used for the common protected mode code area.
public	end_of_TEXT
_TEXT	segment
	end_of_TEXT	label	byte
_TEXT	ends

	; This is used for the initialization code that gets overlaid.
public	end_of_LAST
LAST	segment
	end_of_LAST	label	byte
LAST	ends

	; This is the code that must remain in the lower 640k of memory.
public	end_of_L_STACK
L_STACK segment
	end_of_L_STACK	label	byte
L_STACK	ends

END
