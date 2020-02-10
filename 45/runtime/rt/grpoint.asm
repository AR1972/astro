	TITLE	GRPOINT - POINT statements & functions
	PAGE	56,132
;***
;GRPOINT - POINT statements & functions
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - POINT Function - There are two forms of this, one which takes 2 arguments
;		     (x and y coordinates), and one which takes only 1.
;		     The first form generates 1 of 3 possible calls depending
;		     on the types of the x and y arguments ('I2' and 'R4'
;		     represent 2-byte integer and 4-byte real arguments,
;		     respectively.  The R4 entry point is in grfpinit.asm )
;		     The second form is given in gw2grp.asm.
;
;      v = POINT(I2, I2)
;	     |
;	   B$PNI2
;
; - PSET Statement - the PSET statement calls different runtime routines
;		   depending on whether a color has been specified:
;
;    Examples:
;
;      PSET (x, y)[,color]	     PSET (x, y)
;	 |				 |
;      B$PSTC			       B$PSET
;
;
; - PRESET Statement
;
;      PRESET (x, y)[,color]
;	 |
;      B$PRST
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_DATA
	useSeg	_BSS
	useSeg	GR_TEXT

	INCLUDE seg.inc
	INCLUDE idmac.inc

sBegin	_BSS
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
externW b$ReadC		
externW b$MapXYC		
externW b$SetC 		
externW b$SetAttr		


	EXTRN	B$GRAFACC:WORD	;defined in GWDATA.ASM
	externB b$ScreenMode	; defined in LLCGRP.ASM
	EXTRN	B$SIZEACC:ABS	;defined in GWDATA.ASM

	EXTRN	B$WNDWSW:WORD	;defined in GWDATA.ASM

	EXTRN	B$VXOFF:WORD	;defined in GWDATA.ASM
	EXTRN	B$VYOFF:WORD	;defined in GWDATA.ASM

externB	b$fpoint		; defined in grcoord.asm to indicate whether
				; we are processing point function or not.
sEnd	_BSS

sBegin	GR_TEXT
assumes CS,GR_TEXT

externNP B$INVIEW		;coordinate within current viewport?
externNP B$COORD		; process coordinate pair
externNP B$COORD1		; process coordinate pair
externFP B$I2POINT		;specify an I2 point

	EXTRN	B$ERR_FC:near

externNP B$SCINIT		; Performs screen initialization

;low-level routines:
	EXTRN	B$GETFBC:NEAR
externNP B$FetchC		
externNP B$StoreC		


externNP B$GRMODE			



	SUBTTL	B$PSTC - Set point with color
	PAGE
;***
;B$PSTC - PSET statement with attribute
;void pascal B$PSET(I2 color)
;
;Purpose:
; Process the PSET statement. Set the specified point to the specified color.
; PSTC_ENTRY is a local routine that takes the arg in BX.
;
;Input:
; color = color to be set
;
;Output:
; Graphics accumulators updated
;
;Modifies:
; Per convention
;
;******************************************************************************
cProc	B$PSTC,<FAR,PUBLIC>
parmW	color
cBegin

	CALL	B$SCINIT	; initialize screen if not already done
	MOV	AX,color	; get color to set
	cCall	DO_PSET 	; and process

cEnd

;***
;DO_PSET - PSET statement with attribute
;
;Purpose:
; Common code to process the PSET and PRESET statements. Set the specified
; point to the specified color.
;
;Input:
; [AX]	= color to be set
;
;Output:
; Graphics accumulators updated
;
;Modifies:
; Per convention
;
;******************************************************************************
cProc	DO_PSET,NEAR		
cBegin				

	CMP	b$ScreenMode,0	; text mode?
	JE	ERRIFC		; Brif so, function call error
	CALL	[b$SetAttr]	;SET COLOR ATTRIBUTE
	JC	ERRIFC		;ABORT IF INVALID COLOR
	CALL	B$COORD1	; PROCESS COORDINATES

	JNC	PSE_RET 	;EXIT IF POINT OUT OF BOUNDS
	CALL	[b$MapXYC]	;MAP X,Y COORDS TO SCREEN POSITION
	CALL	[b$SetC]	;DRAW THE POINT
PSE_RET:

cEnd

ERRIFC: JMP	B$ERR_FC	;ILLEGAL FUNCTION CALL

	SUBTTL	B$PSET - Set point with default color
	PAGE
;***
;B$PSET - PSET statement with no attribute
;void pascal B$PSET()
;
;Purpose:
; Process the PSET statement. Set the specified point to the default color.
;
;Input:
; None
;
;Output:
; Graphics accumulators updated
;
;Modifies:
; Per convention
;
;******************************************************************************
cProc	B$PSET,<FAR,PUBLIC,FORCEFRAME> 
cBegin
	CALL	B$SCINIT	; initialize screen if not already done
	STC			; SET CARRY
	CALL	B$GETFBC 	; GET FOREGRD/BACKGRD COLORS
PSET_5: 			
	CALL	DO_PSET 	; and do it
cEnd

	PAGE
	SUBTTL	B$PNI2 - POINT(X,Y) FUNCTION WHERE BOTH PARAMETERS ARE INTEGER
;***
;B$PNI2 - POINT(X,Y) FUNCTION WHERE BOTH PARAMETERS ARE INTEGER
;
;Purpose:
; This routine returns the color attribute of the point specified by the given
; (x,y) coordinate pair.  The graphics accumulators are preserved across calls
; to this function so that cases like line (x1,y1)-(x2,y2), point(x,y) will
; work.
;
;Input:
; x	= x coord
; y	= y coord
;
;Output:
; AX	= attribute
;
;Modifies:
; Per convention
;
;******************************************************************************
cbPoint EQU	13		;local copy of b$cbPoint

cProc	B$PNI2,<FAR,PUBLIC>
parmW	x			;x coordinate
parmW	y			;y coordinate
localV	point,cbpoint		;place to put coordinate pair, if needed
cBegin

DbAssertRel  b$cbPoint,Z,cbPoint,GR_TEXT,<b$cbPoint not equal cbPoint in B$PNI2>
	CALL	B$SCINIT	; initialize screen if not already done

	MOV	CX,x		;[CX] = X
	MOV	DX,y		;[DX] = Y
	cCall	B$GRMODE	; are we in a text mode?
	JE	ERRFC		; Brif so, function call error
	CMP	BYTE PTR B$WNDWSW,0 ; Is window active?
	JZ	PNI2_5		;No, go do stuff directly

	PUSH	CX		;parameters to B$I2POINT
	PUSH	DX
	LEA	BX,point	;[BX] = pointer to local storage
	MOV	CL,11H		;[CL] = flag saying both are integers
	cCall	B$I2POINT	;format data at [point]
	cCall	B$DO_POINT	; go perform point function calling coord
	JMP	SHORT PNI2_90	;and exit

PNI2_5:
	ADD	CX,WORD PTR B$VXOFF ;;ABSx= Vx1 +x
	ADD	DX,WORD PTR B$VYOFF ;;ABSy= Vy1 +y
	cCall	DO_POINT2	;perform point function on (cx,dx)
PNI2_90:

cEnd

ERRFC:	JMP	B$ERR_FC	; ILLEGAL FUNCTION CALL (centrally located)

	SUBTTL	PRESET statement with no attribute
	PAGE
;***
;B$PRST - PRESET statement with no attribute
;void pascal B$PRST()
;
;Purpose:
; Process the PRESET statement. Set the specified point to the backround color.
;
;Input:
; None
;
;Output:
; Graphics accumulators updated
;
;Modifies:
; Per convention
;
;******************************************************************************
cProc	B$PRST,<FAR,PUBLIC,FORCEFRAME> 
cBegin
	CALL	B$SCINIT	; initialize screen if not already done
	STC			; SET CARRY
	CALL	B$GETFBC 	; GET FOREGRD/BACKGRD COLORS
	XCHG	AX,BX		; [AX] = backround color
	JMP	SHORT PSET_5	; and set point
cEnd	nogen			


	SUBTTL	POINT helper routines
	PAGE
;***
;B$DO_POINT
;
;Purpose:
; Perform point function, saving the "B$GRAFACC", and calling B$COORD.
;
;Input:
; [BX]	= pointer to coordinate structure.
;
;Output:
; AX=attribute
;
;Modifies:
; per convention
;
;******************************************************************************
SIZEACC EQU	11		;local copy of B$SIZEACC

cProc	B$DO_POINT,<PUBLIC,NEAR>,<SI,DI,DS,ES> 
localV	savedacc,SIZEACC*2
cBegin


DbAssertRel  B$SIZEACC,Z,SIZEACC,GR_TEXT,<B$SIZEACC not equal SIZEACC in DO_POINT>

	MOV	CX,B$SIZEACC	;external constant
	MOV	SI,OFFSET DGROUP:B$GRAFACC ;point at graphics accum data
	PUSH	SS
	POP	ES		; ES = SS
	LEA	DI,savedacc
	PUSH	ES
	PUSH	DI		;Save interesting values
	PUSH	DS
	PUSH	SI
	PUSH	CX
	REP	MOVSW		;Save all graphics accums

	mov	b$fpoint,1	;indicate to B$COORD we are processing POINT
	.erre	ID_SSEQDS
	cCall	B$COORD	;[CX], [DX] = point
	mov	b$fpoint,0

	cCall	DO_POINT2

	POP	CX		;[CX] = restoration move count
	POP	DI
	POP	ES		;[ES:DI] = far ptr to B$GRAFACC
	POP	SI
	POP	DS		;[DS:SI] = far ptr to local storage
	REP	MOVSW		;Restore all graphics accums

cEnd


;***
;DO_POINT2
;
;Purpose:
; Perform point function, saving the graphics cursor
;
;Input:
; [CX]	= x
; [DX]	= y
;
;Output:
; AX=attribute
;
;Modifies:
; per convention
;
;******************************************************************************
cProc	DO_POINT2,<NEAR>,<SI>	
cBegin
	MOV	SI,CX		;Preserve CX around call to B$FetchC
	CALL	B$FetchC	;Get Graphics Cursor
	PUSH	CX		
	PUSH	BX		;Preserve the graphics cursor so cases like
	PUSH	AX		;LINE (x1,y1)-(x2,y2),POINT(x3,y3) will work
	MOV	CX,SI		;Restore X coordinate

	CALL	B$INVIEW 	;See if within viewport


	MOV	AX,-1		;ASSUME ILLEGAL POINT
	JNB	PNTNOT		;NOT LEGAL : RETURN -1
	CALL	[b$MapXYC]	;C:=(X,Y)
	CALL	[b$ReadC]	;READ OUT THE ATTRIBUTE
	xor	ah,ah		;AH:=AL7
PNTNOT:
	XCHG	DX,AX		;SAVE ATTRIBUTE
	POP	AX
	POP	BX
	POP	CX		
	CALL	B$StoreC	
	XCHG	DX,AX		;restore attribute

cEnd

sEnd	GR_TEXT

	END
