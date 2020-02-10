	TITLE	GRCOORD - graphics point specification
	PAGE	56,132
;***
;GRCOORD - graphics point specification
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
;Purpose:
; This module contains point specification routines, and routines which will
; transform and convert those points as required.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	useSeg	_BSS		;Uninitialized data
	useSeg	_DATA		; Initialized data
	useSeg	GR_TEXT		;Graphics segments
	useSeg	RT_TEXT

	INCLUDE seg.inc 	;segment definitions & cmacros
	INCLUDE baslibma.inc	;useful macros

;******************************************************************************
;
; point data structure
;
;******************************************************************************
POINT	STRUC
fPoint	DB	(?)		;content flags
xI2	DW	(?)		;x coordinate as an I2
xR4	DW	2 DUP (?)	;x coordinate as an I4
yI2	DW	(?)		;y coordinate as an I2
yR4	DW	2 DUP (?)	;y coordinate as an I4
POINT	ENDS

fxI2	EQU	00000001B	;xI2 is valid
fxR4	EQU	00000010B	;xR4 is valid
fxStep	EQU	00001000B	;x represents a STEP value
fyI2	EQU	00010000B	;yI2 is valid
fyR4	EQU	00100000B	;yR4 is valid
fyStep	EQU	10000000B	;y represents a STEP value

b$cbPoint EQU	SIZE POINT
	PUBLIC	b$cbPoint	;Size of the point structure

;******************************************************************************
;
; point data storage
;
;******************************************************************************
sBegin	_BSS

globalB b$point1,,<b$cbPoint> ;first coordinate pair
globalB b$point2,,<b$cbPoint> ;second coordinate pair

globalB b$fpoint,,1		; point statement being processed flag

externB b$ScreenMode		
externB B$COPTFL			; CIRCLE option flag
externW B$GRPACX 		;graphics accumulator X
externW B$GRPACY 		;graphics accumulator Y
externW B$GXPOS
externW B$GYPOS
externW B$VXOFF			;Viewport offset X
externW B$VYOFF			;Viewport offset Y

externB	B$WNDWSW		; flag indicates WINDOW active

externB B$DFRACX 		;8 bit fraction x after

sEnd	_BSS

sBegin	_DATA			
globalW	b$pFPCOORD,B$ERR_FC,1	; vector to B$FPCOORD
globalW	b$pFPCOORDW,B$ERR_FC,1	; vector to B$FPCOORDW
sEnd	_DATA			

sBegin	RT_TEXT
externNP B$ERR_FC
sEnd

sBegin	GR_TEXT
assumes CS,GR_TEXT

externNP B$INVIEW		;determine if points within viewport


	SUBTTL	Integer point specification entrypoints
	PAGE
;***
; B$N1I2, B$S1I2, B$N2I2, B$S2I2 - Integer point specification
; void pascal B$N1I2(I2 x, I2 y)
;
;Purpose:
; Specify integer coordinate pairs.
;	B$N1I2 	- "normal" first coordinate pair
;	B$S1I2 	- STEP first coordinate pair
;	B$N2I2 	- "normal" second coordinate pair
;	B$S2I2 	- STEP second coordinate pair
;
;Entry:
; x,y	= integer x and y values.
;
;Exit:
; None.
;
;Uses:
; per convention
;
;******************************************************************************
cProc	B$S1I2,<FAR,PUBLIC>	;First STEP pair
cBegin	nogen

	MOV	CL,fxI2+fxStep+fyI2+fyStep	;indicate type of values
	SKIP	2				;fall into next routine

cEnd	nogen


cProc	B$N1I2,<FAR,PUBLIC>	;First pair
cBegin	nogen

	MOV	CL,fxI2+fyI2			;indicate type of values
	MOV	BX,OFFSET DGROUP: b$point1	;point at first pair table

cEnd	nogen


cProc	B$I2POINT,<FAR,PUBLIC> ;Common routine to set point struct
parmW	x
parmW	y
cBegin	nogen

	POP	AX
	POP	DX		;[DX:AX] = return address
	POP	[BX].yI2	;store y coordinate
	POP	[BX].xI2	;store x coordinate
labelNP	<PUBLIC,B$STOREFLAGS>	; entry point from R4 code
	MOV	[BX].fPoint,CL	;store flags
	MOV	B$COPTFL,0	; Reset it - CIRCLE may have left it set.
	PUSH	DX
	PUSH	AX		;put return address back on stack
	RET			;and we are done

cEnd	nogen


cProc	B$S2I2,<FAR,PUBLIC>	;Second STEP pair
parmW	x
parmW	y
cBegin	nogen
	MOV	CL,fxI2+fxStep+fyI2+fyStep	;indicate type of values
	SKIP	2				;fall into next routine

cEnd	nogen


cProc	B$N2I2,<FAR,PUBLIC>	;Second pair
parmW	x
parmW	y
cBegin	nogen

	MOV	CL,fxI2+fyI2			;indicate type of values
	MOV	BX,OFFSET DGROUP: b$point2	;point at second pair table
	JMP	B$I2POINT

cEnd	nogen

	SUBTTL	B$COORD - process & return integer coordinates
	PAGE
;***
;B$COORD, B$COORD1, B$COORD2 - get a coordinate pair
;
;Purpose:
; Calculate the physical screen coordinates of the given point. Relative
; coordinates, viewports and windowing are all considered in determining the
; final coordinates. Clipping is not done in this routine.
;
;Entry:
; [BX]	= pointer to point to be processed (B$COORD only)
;
;Exit:
; [CX]	= x value
; [DX]	= y value
; Graphics accumulators updated
;
;Modifies:
; per convention
;
;Notes:
; This routine used to be $COORDS, and contained switches for FG_SCRNROT
;
;******************************************************************************
cProc	B$COORD2,<NEAR,PUBLIC> ;Get second point in coord pair
cBegin	<nogen> 		
	MOV	BX,OFFSET DGROUP:b$point2 
	JMP	SHORT B$COORD	
cEnd	<nogen> 		

cProc	B$COORD1,<NEAR,PUBLIC> ;Get first point
cBegin	<nogen> 		
	MOV	BX,OFFSET DGROUP:b$point1 
cEnd	<nogen> 		

cProc	B$COORD,<NEAR,PUBLIC>
cBegin
	MOV	AL,[BX].fPoint	;[AL] = flags relating to coordinate pair
	OR	AL,AL		;See if there is a point to process
	JNZ	COORD_5 	;Jump if there is
	MOV	CX,B$GRPACX	;Return Graphics Accumulator x
	MOV	DX,B$GRPACY	;Return Graphics Accumulator y
	RET

COORD_5:
	PUSH	SI		;Save me
	MOV	SI,BX		;[SI] = pointer to point struct

	CMP	[B$WNDWSW],0	; Is window active?
	JZ	CORINT		;Jump if not, we should have INT's
	JMP	[b$pFPCOORDW]	; resumes at B$COORD_NOSTEP
;
; No window is active, so we expect to do integer calculations. Make sure that
; we have I2 representations for both our points.
;
CORINT:
	TEST	AL,fxR4+fyR4	;Is either coord an R4?
	JZ	CORINT1		;brif not - no conversion needed
	CALL	[b$pFPCOORD]	; make coords I2
CORINT1:
	MOV	CX,[SI].xI2	;[CX] = I2 representation of X
	MOV	DX,[SI].yI2	;[DX] = I2 representation of Y

	CMP	b$fpoint,1	;processing point function ?
	JZ	B$COORD_NOSTEP	; if so do not add the bases
	TEST	AL,fxStep	;only need to test one, they come in pairs
	JZ	B$COORD_NOSTEP	; If not step, then don't add base
	ADD	CX,B$GRPACX	;Add current graphics accum to create step.
	ADD	DX,B$GRPACY

	JMP	SHORT COORD_STEP
labelNP	<PUBLIC,B$COORD_NOSTEP>	; B$FPCOORDW returns here
	ADD	CX,B$VXOFF	;ABSx = Vx1 +x
	ADD	DX,B$VYOFF	;ABSy = Vy1 +y
COORD_STEP:

	MOV	B$GRPACX,CX	;Update Graphics Accumulator x
	MOV	B$GXPOS,CX	;Copy
	MOV	B$GRPACY,DX	;Update Graphics Accumulator y
	MOV	B$GYPOS,DX	;Copy

	MOV	WORD PTR B$DFRACX,8080H ;Set Fractional x,y to 1/2
	XOR	AX,AX		
	MOV	[SI].fPoint,AL	;clear point flag for next time...

	CMP	b$ScreenMode,0	; graphics mode?
	JZ	FC_ERRR 	;Illegal function call
	POP	SI
	JMP	B$INVIEW 	;See if point in viewport

cEnd	nogen

FC_ERRR:
	JMP	B$ERR_FC	;ILLEGAL FUNCTION CALL

sEnd	GR_TEXT

	END
