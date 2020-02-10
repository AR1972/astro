	TITLE	LLHGCSUP - LowLevel HGC/CGA support (shared routines)
;***
; LLHGCSUP - LowLevel HGC/CGA support
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This module contains support routines extracted from LLCGA.ASM
;	(and modified) which are shared by CGA and HGC functions.
;
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_DATA
	USESEG	_BSS
	USESEG	GR_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE llgrp.inc
	INCLUDE idmac.inc

sBegin	_BSS
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$MaskC
externW b$OffC
externW B$VTOFST
externW B$VBOFST
externW B$VLOFST
externW B$VROFST
externW B$LEOFST
externW B$REOFST
externW b$BytesPerRow
externW b$UpSub
externW b$DnSub
externW b$UpDnAdd

sEnd	_BSS

assumes CS,GR_TEXT
sBegin	GR_TEXT

;***
; B$CgaLeftC2/B$CgaLeftC1
;
;Purpose:
;	Move graphics cursor left 1 pixel (2 bits for Screen 1, 1 bit for
;	Screen 2 or 3).  No test is made for screen boundaries.
;Entry:
;	None
;Exit:
;	b$MaskC, b$OffC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaLeftC2,<PUBLIC,NEAR>
cBegin
	;2 bits per pixel in screen 1
	rol	b$MaskC,1	;rotate mask one pixel left
	;1 bit per pixel in screen 2
labelNP <PUBLIC,B$CgaLeftC1>
	rol	b$MaskC,1
	jc	LeftCnext	;go if not in same byte
	ret
LeftCnext:
	dec	b$OffC 	;move byte pointer
	ret
cEnd

;***
; B$CgaChkUpC
;
;Purpose:
;	Move graphics cursor up 1 pixel.  A test is made for boundaries.
;	If it is a boundary then PSW.C is set upon return and no move is
;	made.
;Entry:
;	None
;Exit:
;	PSW.C set if original cursor was on top screen edge.
;	b$MaskC, b$OffC updated otherwise.
;Uses:
;	per conv.
;Exceptions:
;	exits through UpC if not on edge
;******************************************************************************
cProc	B$CgaChkUpC,<PUBLIC,NEAR>
cBegin
	MOV	AX,b$OffC	;[AX] = cursor offset
	XOR	AX,B$VTOFST	;cursor and VTOFST SHOULD BE same
				;half or quadrant of the screen
	AND	AH,60H		;will be non zero if not in same
				;half or quadrant of the screen
	JNZ	B$CgaUpC	;move cursor up by one
	MOV	AX,b$OffC	;[AX] = cursor offset
	CMP	AX,B$VTOFST	;already on top of viewport ?
	JAE	B$CgaUpC	;less than VTOFST means on top of
				;viewport
	STC			;STC to indicate that cursor already
cEnd				;on top of viewport

;***
; B$CgaUpC
;
;Purpose:
;	Move graphics cursor up 1 pixel.  No test is made for screen
;	boundaries.
;Entry:
;	None
;Exit:
;	b$MaskC, b$OffC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaUpC,<PUBLIC,NEAR>
cBegin
	mov	AX,b$OffC
	sub	AX,b$UpSub	;assume not going up MOD 0 to MOD 3
	jnb	UpExit		;go if so
	add	AX,b$UpDnAdd	;undo SUB + move up from MOD 0 to MOD 3
	clc			;(no error for ChkUpC)
UpExit:
	mov	b$OffC,AX
cEnd

;***
; B$CgaChkDownC
;
;Purpose:
;	Move graphics cursor down 1 pixel for Screen 1 or Screen 2.
;	If beyond the bottom edge, PSW.C is set upon return and no
;	move is made.
;Entry:
;	None
;Exit:
;	PSW.C set if original cursor was on bottom screen edge.
;	b$MaskC, b$OffC updated otherwise.
;Uses:
;	per conv.
;Exceptions:
;	exits through DownC if not on edge
;******************************************************************************
cProc	B$CgaChkDownC,<PUBLIC,NEAR>
cBegin
	MOV	AX,b$OffC	;[AX] = cursor offset
	XOR	AX,B$VBOFST	;cursor and VBOFST SHOULD BE in same
				;half or quadrant of screen
	AND	AH,60H		;will be non zero if not in same half
				;or quadrant of the screen
	JNZ	B$CgaDownC	;move cursor down by one
	MOV	AX,b$OffC	;[AX] = cursor offset
	CMP	AX,B$VBOFST	;already at the bottom of viewport?
	JB	B$CgaDownC	;Brif not
	STC			;STC to indicate that cursor is
cEnd				;on bottom of viewport

;***
; B$CgaDownC
;
;Purpose:
;	Move graphics cursor down 1 pixel for Screen 1 or Screen 2.
;	No test is made for screen boundaries.
;Entry:
;	None
;Exit:
;	b$MaskC, b$OffC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaDownC,<PUBLIC,NEAR>
cBegin
	mov	AX,b$OffC
	sub	AX,b$DnSub	;assume not going down from MOD 0 to MOD 3
	jnb	DownExit	;go if so
	add	AX,b$UpDnAdd	;undo SUB + move down from MOD 0 to MOD 3
	clc			;(no error for ChkDownC)
DownExit:
	mov	b$OffC,AX
cEnd

;***
; B$CgaPaintBound
;
;Purpose:
;	Called by PAINT before painting each scan line to facilitate
;	fast viewport edge detection for Screen 1 or Screen 2.	Set
;	VIEW left and right cursor addresses and masks.
;Entry:
;	None
;Exit:
;	B$LEOFST = left edge offset
;	B$REOFST = right edge offset
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaPaintBound,<PUBLIC,NEAR>
cBegin

;  Map all rows to the first quadrant/half so that the first pixel in each
;  row will be a multiple of b$BytesPerRow.  Since each half/quadrants starts
;  at a power of two, and b$BytesPerRow has a 5 factor in it, rows in the upper
;  half/quadrants are not evenly divisible by b$BytesPerRow.

	MOV	AX,b$OffC	;get cursor position
	MOV	CH,AH		;b$OffC offset will be needed to remap
	AND	CH,01100000B	;mask in quadrant/half offset bits
	AND	AH,00011111B	;map to lower quadrant/half to make addr a
				;multiple of b$BytesPerRow

;  Compute the addr of the 1st pixel by doing an
;  INT(b$OffC/b$BytesPerRow)*b$BytesPerRow

	XOR	DX,DX		
	DIV	b$BytesPerRow	;[AL]= INT(b$OffC/b$BytesPerRow)
	MUL	b$BytesPerRow	;[AL]= INT(b$OffC/b$BytesPerRow)*b$BytesPerRow
	OR	AH,CH		;map back to proper quadrant/half
	XCHG	AX,DX

;  b$OffC addr of 1st pixel in current row is now in DX - compute boundries

	MOV	AX,B$VLOFST
	ADD	AX,DX
	MOV	B$LEOFST,AX	;Left margin= (x1,0)+b$OffC
	MOV	AX,B$VROFST
	ADD	AX,DX
	MOV	B$REOFST,AX	;Right margin= (x2,0)+b$OffC
cEnd

sEnd	GR_TEXT

	END
