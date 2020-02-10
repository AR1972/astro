	TITLE	VWCLR - IAPX 88/86 CLEAR GRAPHICS VIEWPORT SUPPORT
;***
; VWCLR - IAPX 88/86 CLEAR GRAPHICS VIEWPORT SUPPORT
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc	;Rutime switch file
	INCLUDE rmacros.inc	; General runtime macros

	useSeg	_DATA		
	useSeg	_BSS		
	useSeg	GR_TEXT 	

	INCLUDE seg.inc 	

sBegin	_DATA			

	externB b$ScreenMode	; defined in LLCGRP.ASM
	externW B$GXPOS
	externW B$GYPOS
	externW B$VXMIN
	externW B$VXMAX
	externW B$VYMIN
	externW B$VYMAX
	externW B$LINSTL

sEnd	_DATA			

sBegin	_BSS			
;
;============================================================================
; External low-level function vectors
;============================================================================
;
externW b$SetAttr		
externW b$MapXYC		
externW b$NSetC 		
externW b$DownC 		
;============================================================================

	externW B$GXHPOS	;defined in GWDATA.ASM
	externW B$GXLPOS	;defined in GWDATA.ASM
	externW B$GYHPOS	;defined in GWDATA.ASM
	externW B$GYLPOS	;defined in GWDATA.ASM
sEnd	_BSS			

sBegin	GR_TEXT 		
assumes CS,GR_TEXT		

	externNP B$ERR_FC	


	externNP B$GRMODE		


	externNP B$VWMAPC

	externNP B$GETFBC

cProc	B$VWCLR,<PUBLIC,NEAR>
cBegin
	cCall	B$GRMODE	
	JZ	NO_VWCLR	;if not, just return
	PUSH	BX		;Save text pointer
	STC			;Flag to get graphics colors
	CALL	B$GETFBC 	;Get forground/background colors
	PUSH	AX		;Save current colors
	MOV	AL,BL		;Default Fill is Background
	CALL	B$VWFILL
	POP	AX		;Restore original colors
	CALL	[b$SetAttr]	
	POP	BX		;Retrieve text pointer
NO_VWCLR:
cEnd

cProc	B$VWFILL,<PUBLIC,NEAR>
cBegin
	CALL	[b$SetAttr]	;Set fill attr

	JNB	VWFIL0		;ERROR IF ILLEGAL ATTRIBUTE
	JMP	B$ERR_FC	; Function call error
VWFIL0:
	MOV	AX,WORD PTR B$VXMAX
	MOV	WORD PTR B$GXPOS,AX ;vx2
	MOV	AX,WORD PTR B$VYMAX
	MOV	WORD PTR B$GYPOS,AX ;vy2
	MOV	CX,WORD PTR B$VXMIN ;[CX]= vx1
	MOV	DX,WORD PTR B$VYMIN ;[DX]= vy1
	CALL	B$DOBOXF 	;Box Fill..
	JMP	B$VWMAPC 	;Set view "C"'s and GAC
cEnd	<nogen>

cProc	B$DOBOXF,<PUBLIC,NEAR>
cBegin				;ENTRY FOR CALL FROM VWFIL0 IN ADVG86

	CALL	BOXTST
	JB	B$LINEXT	;Brif box out of view

	CALL	B$SCALXY 	;SCALE FIRST POINT
	XCHG	CX,[B$GXPOS]	;ensure CX = MIN(X1,X2)
	XCHG	DX,[B$GYPOS]	;ensure DX = MIN(Y1,Y2)
				;CALL	 $XCHGAC		  ;SWITCH POINTS
	CALL	B$SCALXY 	;SCALE SECOND POINT


	CALL	B$YDELT		;SEE HOW MANY LINES AND SET CARRY
	JNB	DOBOX1
	XCHG	DX,[B$GYPOS]	;ensure DX = MIN(Y1,Y2)
DOBOX1:
	INC	BX		;MAKE [BX] INTO A COUNT
	push	bp		;save bp across MapXYC call
	PUSH	BX		;SAVE COUNT OF LINES
	CALL	B$XDELT		;GET WIDTH AND SMALLEST X
	JNB	DOBOX2
	XCHG	CX,[B$GXPOS]	;ensure CX = MIN(X1,X2)
DOBOX2:
	INC	BX		;MAKE [BX] INTO A WIDTH COUNT
	PUSH	BX		;SAVE WIDTH COUNT
	CALL	[b$MapXYC]	;MAP INTO A "C"
	POP	BX		;GET WIDTH COUNT
	POP	CX		;GET LINE COUNT

BOXLOP:
	PUSH	CX		;[speed]
	PUSH	BX		;[speed]
	CALL	[b$NSetC]	;IN [BX] ORF POINTS TO $SETC
	CALL	[b$DownC]	;MOVE TO NEXT LINE DOWN IN Y
	POP	BX		;[speed]
	POP	CX		;GET BACK NUMBER OF LINES
	LOOP	BOXLOP		;[speed] COUNT DOWN LINES
				;KEEP DRAWING MORE LINES

	pop	bp		;[speed]

labelNP <PUBLIC,B$LINEXT>	;EXIT LINE STATEMENT
	MOV	WORD PTR B$LINSTL,-1 ;RESTORE LINE STYLE TO SOLID
cEnd


cProc	BOXTST,<NEAR>,<DX>
cBegin
	MOV	AX,WORD PTR B$GXPOS
	MOV	DX,CX
	CMP	AX,DX
	JGE	BOXTS1
	XCHG	AX,DX
BOXTS1:
	MOV	WORD PTR B$GXHPOS,AX
	MOV	WORD PTR B$GXLPOS,DX
	POP	DX
	PUSH	DX
	MOV	AX,WORD PTR B$GYPOS
	CMP	AX,DX
	JGE	BOXTS2
	XCHG	AX,DX
BOXTS2:
	MOV	WORD PTR B$GYHPOS,AX
	MOV	WORD PTR B$GYLPOS,DX
	CALL	BOXCLP
cEnd

cProc	BOXCLP,<NEAR>
cBegin
	MOV	AX,WORD PTR B$GXHPOS
	CMP	AX,WORD PTR B$VXMIN ;Is Xmax .LT. B$VXMIN?
	JL	BOXCLX		;Yes: exit with carry set since fill won't show
	MOV	AX,WORD PTR B$VXMAX
	CMP	AX,WORD PTR B$GXLPOS
	JL	BOXCLX		
	MOV	AX,WORD PTR B$GYHPOS
	CMP	AX,WORD PTR B$VYMIN
	JL	BOXCLX		
	MOV	AX,WORD PTR B$VYMAX
	CMP	AX,WORD PTR B$GYLPOS
	JL	BOXCLX		
	CLC			;Clear carry if any fill will show
BOXCLX:
cEnd

cProc	B$CLIPCK,<PUBLIC,NEAR>
cBegin
	XOR	AL,AL		;Clear view bits
	CMP	WORD PTR B$VXMIN,CX ;B$VXMIN .lt. coord?
	JLE	CLIPC2		;Yes, in view
	INC	AX		;Set D0 if out of view left
CLIPC2:
	CMP	WORD PTR B$VXMAX,CX ;B$VXMAX .gt. coord?
	JGE	CLIPC3		;Yes, in view
	OR	AL,BYTE PTR 2	;Set D1 if out of view right
CLIPC3: 			;See if y is in view
	CMP	WORD PTR B$VYMIN,DX ;B$VYMIN .lt. coord?
	JLE	CLIPC4		;Yes, in view
	OR	AL,4		;Set D2 if out of view top
CLIPC4:
	CMP	WORD PTR B$VYMAX,DX ;B$VYMAX .gt. coord?
	JGE	CLIPCX		;Yes, in view
	OR	AL,8		;Set D3 if out of view bottom
CLIPCX:
cEnd				;return view status in [AL]

;***
;
;B$SCALXY		 Force (x,y) coordinates within viewport boundaries
;
;Purpose:
;	  Test the (x,y) coordinates in (CX,DX) against the current viewport
;	  boundaries.  If either coordinate is outside the viewport, replace it
;	  with the nearest viewport boundary value (B$VXMIN or B$VXMAX for X,
;	  B$VYMIN or B$VYMAX for Y).  The carry flag is cleared if either
;	  coordinate was outside the viewport boundary.
;
;Entry:
;	  CX=x coordinate DX=y coordinate
;
;Exit:
;	CX=updated x, DX=updated y
;	carry set if no coords changed, clear if either coord changed
;
;Modifies: AX,BX
;
;****
cProc	B$SCALXY,<PUBLIC,NEAR>
cBegin
	CMP	b$ScreenMode,0	; graphics mode?
	JNZ	SCALX0		;Brif Graphics mode
	JMP	B$ERR_FC	; else Illegal
SCALX0:
	CALL	B$CLIPCK 	;[AL] non-zero if x or y out of bounds
	OR	AL,AL		;check for no clipping
	JZ	NOSCAL		;no clipping required
	MOV	BX,WORD PTR B$VXMIN
	TEST	AL,BYTE PTR 1
	JNZ	SCALX1		;Brif x is less than min
	MOV	BX,WORD PTR B$VXMAX
	TEST	AL,BYTE PTR 2
	JZ	SCALY0		;Brif x is within min/max
SCALX1:
	MOV	CX,BX		;Return min or max x
SCALY0:
	LAHF			;[AH]=flags
	MOV	BX,WORD PTR B$VYMIN
	TEST	AL,BYTE PTR 4
	JNZ	SCALY1		;Brif y is less than min
	MOV	BX,WORD PTR B$VYMAX
	TEST	AL,BYTE PTR 8
	JZ	SCALY2		;Brif y is within min/max
SCALY1:
	LAHF			;[AH]=flags
	MOV	DX,BX		;Return min or max y
SCALY2:
	SAHF			;Update flags
NOSCAL: 			;no clipping required
	STC
	JZ	SCALY3
	CMC			;Return no-carry if out of bounds
SCALY3:
cEnd


	PAGE
	SUBTTL	UTILITY ROUTINES FOR LINE CODE

;
; B$XDELT SETS [BX]=ABS(B$GXPOS-[CX]) AND SETS CARRY IF [CX].GT.B$GXPOS
; NOTE: [BX] WILL BE A DELTA BETWEEN B$GXPOS AND [CX] - ADD 1 FOR AN X "COUNT"
;
cProc	B$XDELT,<PUBLIC,NEAR>
cBegin
	MOV	BX,WORD PTR B$GXPOS ;GET ACCUMULATOR POSITION
	SUB	BX,CX
CNEGHL:
	JNB	CNEGHX		;IF NO CARRY, NO NEED TO NEGATE COUNT
	NEG	BX
CNEGHX:
cEnd

; B$YDELT SETS [BX]=ABS(B$GYPOS-[DX]) AND SETS CARRY IF [DX].GT.B$GYPOS
;

cProc	B$YDELT,<PUBLIC,NEAR>
cBegin
	MOV	BX,WORD PTR B$GYPOS
	SUB	BX,DX
	JMP	SHORT CNEGHL
cEnd	<nogen>

sEnd	GR_TEXT 		

	END
