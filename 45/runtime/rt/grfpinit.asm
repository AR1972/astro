	TITLE	GRFPINIT - FP & WINDOW GRAPHICS COMPONENT INITIALIZATION
;***
; GRFPINIT - FP & WINDOW GRAPHICS COMPONENT INITIALIZATION
;
;	Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - POINT Function - R4 entry point for 2-argument POINT function.
;
;      v = POINT(R4,R4)
;	     |
;	   B$PNR4
;
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	_DATA
	useSeg	_BSS
	useSeg	CONST
	useSeg	GR_TEXT
	useSeg	<XIB>		; XIB and XIE must bracket XI!
	useSeg	<XI>		; initializer segment
	useSeg	<XIE>

	INCLUDE seg.inc
	INCLUDE idmac.inc	; Internal debugging macros
	INCLUDE baslibma.inc	; useful macros


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

	INITIALIZER	B$xFPGRINI ; Put B$xFPGRINI in initializer list

sBegin	_DATA
	externW b$pFPGRINI	; pointer to B$FPGRINI
	externW b$pFPCOORD	; pointer to B$FPCOORD
sEnd	_DATA

sBegin	_BSS

	externB b$point1	;first coordinate pair
	externB b$point2	;second coordinate pair
	externB b$ScreenMode	; defined in LLCGRP.ASM

	externW B$VXSIZ 	;defined in GRINIT.ASM
	externW B$VYSIZ 	;defined in GRINIT.ASM
	externW B$VXMIN 	;defined in GRINIT.ASM
	externW B$VYMIN 	;defined in GRINIT.ASM
	externW B$VXMAX 	;defined in GRINIT.ASM
	externW B$VYMAX 	;defined in GRINIT.ASM
	externW B$VXOFF 	;defined in GRINIT.ASM
	externW B$VYOFF 	;defined in GRINIT.ASM
	externW B$GRPACX	;defined in GWDATA.ASM
	externW B$GRPACY	;defined in GWDATA.ASM

	externB B$VIEWSC	; defined in GWDATA.ASM

	globalQ B$PXDIF,,1	;Used to Map Physical
	globalQ B$PYDIF,,1	;to Logical coordinates
	globalQ B$LXDIF,,1	; Used to Map Logical
	globalQ B$LYDIF,,1	; to Physical coordinates
	globalQ B$WXDIF,,1	;Window x displacement
	globalQ B$WYDIF,,1	;Window y displacement
	externD B$GRFACX	;defined in GWDATA.ASM
	externD B$GRFACY	; defined in GWDATA.ASM

	globalQ B$WXMIN,,1	; Window x min (*) MUST BE IN THIS ORDER
	globalQ B$WYMIN,,1	; Window y min (*)
	globalQ B$WXMAX,,1	; Window x max (*)
	globalQ B$WYMAX,,1	; Window y max (*)
	externW B$VXDIF 	;defined in GRINIT.ASM
	externW B$VYDIF 	;defined in GRINIT.ASM
	externB B$WNDWSW 	; defined in GRINIT.ASM
	externB B$WNDWSC 	; defined in GRINIT.ASM

	globalD B$ASPRF,,1	;Aspect ratio in s.p. floating format

sEnd	_BSS

sBegin	CONST

	externD b$FP_1		; s.p. constant   1.0
	externD b$FP_256	; s.p. constant 256.0

sEnd	CONST


sBegin	GR_TEXT
assumes CS,GR_TEXT

	externNP B$ftolrnd	
	externNP B$STOREFLAGS
	externNP B$DO_POINT

;OEM routines:
	externNP B$GetAspect

	externNP B$GRMODE	

	externNP B$fmldw
	externNP B$SCINIT	; Performs screen initialization
	externNP B$ERR_FC	; illegal function call

	PAGE
	SUBTTL	Graphics initialization routines
;***
;B$xFPGRINI - Initializer for the fp and WINDOW graphics component
;PLM B$xFPGRINI()
;
;Purpose:
;
;Entry:
;	None.
;
;Exit:
;	Appropriate dispatch vectors filled.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xFPGRINI,<FAR>
cBegin

	MOV	[b$pFPGRINI],GR_TEXTOFFSET B$FPGRINI
	MOV	[b$pFPCOORD],GR_TEXTOFFSET B$FPCOORD
cEnd

;***
; B$FPGRINI - combine FP stuff from B$GRPINI and VWINI with B$CLCWXY
; B$CLCWXY makes window transformation calculations go fast by
; eliminating the following common sub-expressions when Window is active:
;
; Given that VIEW set:
;
; B$VXDIF = Vx2-Vx1
; B$VYDIF = Vy2-Vy1
;
; then calculate:
;
; B$WXDIF = (Wx2-Wx1)
; B$WYDIF = (Wy2-Wy1)
;
; B$LXDIF = (Vx2-Vx1)/(Wx2-Wx1)
; B$LYDIF = (Vy2-Vy1)/(Wy2-Wy1)
;
; B$PXDIF = (Wx2-Wx1)/(Vx2-Vx1)
; B$PYDIF = (Wy2-Wy1)/(Vy2-Vy1)
;****

cProc	B$FPGRINI,<PUBLIC,NEAR>	; replaces all FP code from grinit.asm
cBegin
; following taken from B$VWINI
	CALL	B$GRMODE	;Legal graphics mode?
	JZ	GRPIN0		;If not Graphics mode, don't bother to calc
				; floating aspect ratio.
	CALL	B$GetAspect	;[BX] = OEM's aspect ratio*256 for this screen mode
	CALL	B$fmldw		; ST0 = aspect ratio*256
	FDIV	b$FP_256	; ST0 = aspect ratio
	FSTP	B$ASPRF		; Save floating aspect ratio, pop from ST0
	FWAIT
GRPIN0:

; following taken from B$VEWINI
labelNP	<PUBLIC,B$CLCWXY>	
	CMP	B$WNDWSW,0	; Don't reset Window to View
	JNZ	B$WNDINX 	;bounds if Window is active
	XOR	CX,CX		;make CXDX = s.p. zero
	XOR	DX,DX
	MOV	BX,OFFSET DGROUP:B$WXMIN
	CALL	B$MOV$MR 	;B$WXMIN= 0
	ADD	BX,4		
	CALL	B$MOV$MR 	; R8 zero!
	MOV	BX,OFFSET DGROUP:B$WYMIN
	CALL	B$MOV$MR 	;B$WYMIN= 0
	ADD	BX,4		
	CALL	B$MOV$MR 	; R8 zero!
	MOV	BX,B$VXSIZ	;BX:=screen size x direction
	CMP	[B$VIEWSC],0	; Is a VIEW screen stmt active?
	JNZ	CLCWXS		;Yes: window applies to entire screen
	MOV	BX,B$VXMAX	;Otherwise window applies to viewport
	SUB	BX,B$VXMIN	;therefore BX:=(B$VXMAX-B$VXMIN)
CLCWXS:
	CALL	B$fmldw		; push integer in BX onto numeric stack (ST0)

	FSTP	B$WXMAX		; B$WXMAX= FLOAT(BX)
	MOV	BX,B$VYSIZ	; BX:=screen size y direction
	CMP	[B$VIEWSC],0	; Is a VIEW screen stmt active?
	JNZ	CLCWYS		; Yes: window applies to entire screen
	MOV	BX,B$VYMAX	; Otherwise window applies to viewport
	SUB	BX,B$VYMIN	; therefore BX:=(B$VXMAX-B$VXMIN)
CLCWYS:
	CALL	B$fmldw		; push integer in BX onto numeric stack (ST0)
	FSTP	B$WYMAX		; B$WYMAX= FLOAT(BX)

labelNP	<PUBLIC,B$WNDINX>	
	FLD	B$WXMIN
	FSUBR	B$WXMAX		; ST0 = B$WXMAX - B$WXMIN
	FST	B$WXDIF		; B$WXDIF= (Wx2-Wx1), ST0 unchanged

	FIDIVR	B$VXDIF		; ST0 = B$VXDIF/B$WXDIF
	FST	B$LXDIF		; B$LXDIF= (Vx2-Vx1)/(Wx2-Wx1), ST0 unchanged
	FDIVR	b$FP_1 		; ST0 = 1.0/B$LXDIF = (Wx2-Wx1)/(Vx2-Vx1)
	FSTP	B$PXDIF		; B$PXDIF= (Wx2-Wx1)/(Vx2-Vx1)

	FLD	B$WYMIN
	FSUBR	B$WYMAX		; ST0 = B$WYMAX - B$WYMIN
	FST	B$WYDIF		; B$WYDIF= (Wy2-Wy1), ST0 unchanged

	FIDIVR	B$VYDIF		; ST0 = B$VYDIF/B$WYDIF
	FST	B$LYDIF		; B$LYDIF= (Vy2-Vy1)/(Wy2-Wy1), ST0 unchanged
	FDIVR	b$FP_1 		; ST0 = 1.0/B$LYDIF = (Wy2-Wy1)/(Vy2-Vy1)
	FSTP	B$PYDIF		; B$PYDIF= (Wy2-Wy1)/(Vy2-Vy1)

	MOV	BX,B$GRPACX	; [BX]= Physical x coordinate
	CALL	B$MAPBXL 	; ST0=  World	x coordinate
	FSTP	B$GRFACX 	; Store floating x coordinate
	MOV	BX,B$GRPACY	; [BX]= Physical y coordinate
	CALL	B$MAPBYL 	; ST0 =  World   y coordinate
	FSTP	B$GRFACY 	; Store floating y coordinate
	FWAIT

cEnd	

PAGE
SUBTTL	Transformation routines used at initialization

;The following transformation routines comprise part of the PMAP function.
;They have been separated out into this module to reduce the size of compiled
;programs which do not use graphics since they are always called at initializa-
;tion.


labelNP	<PUBLIC, B$MAPBXL
	SUB	BX,B$VXOFF

labelNP	<PUBLIC, B$MAPPX2>
	MOV	CX,OFFSET DGROUP:B$WXMIN ;CX points at B$WXMIN
	MOV	DX,OFFSET DGROUP:B$PXDIF ;DX points at B$PXDIF
	PUSH	CX		;Save ptr to B$WXMIN
	CALL	MAPSUB		; Do mapping
	JMP	SHORT MAPYLX	;ST0= ST0+ Wc1 (or Wc2 if WINDOW)

labelNP	<PUBLIC, B$MAPBYL>
	SUB	BX,B$VYOFF

labelNP	<PUBLIC, B$MAPPY2>
	MOV	CX,OFFSET DGROUP:B$WYMIN ;CX points to B$WYMIN
	CALL	B$ISWNDW 	;Check window status flags
	JB	MAPYL1		;No need to invert y coord
	MOV	CX,OFFSET DGROUP:B$WYMAX ;Must invert y coord since current

MAPYL1:
	PUSH	CX
	MOV	DX,OFFSET DGROUP:B$PYDIF ;DX points to sub expr B$PYDIF
	CALL	MAPSUB		; Do mapping
	CALL	B$ISWNDW 	; Check window status flags
	JB	MAPYLX		; No need to invert y coord

	FCHS			; Negate expression for window( )-( ) form
MAPYLX:
	POP	BX		; Wy1 or Wy2
	FADD	QWORD PTR [BX]	; ST0 = ST0 + Wc1 (or Wc2 if WINDOW)
	RET

;***
;MAPSUB 	Mapper multiply subroutine
;
;Purpose:	Convert operand to single precision then multiply by single
;	precision number in [CXDX] returning single precision result on top
;	of numeric stack (ST0)
;
;Entry:
;	DX = ptr to s.p. operand 1
;	BX = integer operand 2
;
;Exit:
;	ST0 = (operand 1 * float(operand 2))
;
;Modifies:
;****
cProc	MAPSUB,<NEAR>
cBegin
	PUSH	DX		;Save ptr to B$PYDIF or B$PXDIF
	CALL	B$fmldw		;push integer in BX on numeric stack
	POP	BX		;B$PXDIF OR B$PYDIF
	FMUL	QWORD PTR [BX]	; ST0 = ST0 * d.p. number pointed to by BX
cEnd

;***
;B$MOV$MR	Move registers to memory
;Purpose:
;		Move contents of [CXDX] to memory location referenced by BX.
;Entry:
;		[CXDX] = bytes to be stored
;		BX = ptr to memory
;Exit:
;		referenced memory location <= [CXDX]
;Modifies:
;		none.
;****

cProc	B$MOV$MR,<PUBLIC,NEAR>	; entire routine
cBegin
	MOV	[BX],DX
	MOV	[BX+2],CX
cEnd

; B$ISWNDW -  Returns NC if WINDOW active
;	    Returns CY if no window or WINDOW SCREEN
cProc	B$ISWNDW,<PUBLIC,NEAR>	; entire routine
cBegin
	CMP	[B$WNDWSW],0	;WINDOW active?
	JZ	ISWNDC		;Brif not
	CMP	[B$WNDWSC],0	;WINDOW SCREEN active?
	JZ	ISWNDX		;brif not
ISWNDC:
	STC			;set carry flag
ISWNDX:
cEnd

	SUBTTL	Real point specification entrypoints
	PAGE
;***
; B$N1R4, B$S1R4, B$N2R4, B$S2R4 - Integer point specification
; void pascal B$N1R4(R4 x, R4 y)
;
;Purpose:
; Specify real coordinate pairs.
;	B$N1R4 	- "normal" first coordinate pair
;	B$S1R4 	- STEP first coordinate pair
;	B$N2R4 	- "normal" second coordinate pair
;	B$S2R4 	- STEP second coordinate pair
;
;Entry:
; x,y	= real x and y values.
;
;Exit:
; None.
;
;Uses:
; per convention
;
;******************************************************************************
cProc	B$S1R4,<FAR,PUBLIC>	;First STEP pair
cBegin	nogen

	MOV	CL,fxR4+fxStep+fyR4+fyStep	;indicate type of values
	SKIP	2				;fall into next routine

cEnd	nogen


cProc	B$N1R4,<FAR,PUBLIC>	;First pair
cBegin	nogen

	MOV	CL,fxR4+fyR4			;indicate type of values
	MOV	BX,OFFSET DGROUP: b$point1	;point at first pair table

cEnd	nogen


cProc	B$R4POINT,<FAR,PUBLIC> ;Common routine
parmD	x
parmD	y
cBegin	nogen

	POP	AX
	POP	DX		;[DX:AX] = return address
	POP	[BX].yR4	;store y coordinate
	POP	[BX].yR4+2	;store y coordinate
	POP	[BX].xR4	;store x coordinate
	POP	[BX].xR4+2	;store x coordinate
	JMP	B$STOREFLAGS	;go store flags and return

cEnd	nogen


cProc	B$S2R4,<FAR,PUBLIC>	;Second STEP pair
parmD	x
parmD	y
cBegin	nogen
	MOV	CL,fxR4+fxStep+fyR4+fyStep	;indicate type of values
	SKIP	2				;fall into next routine

cEnd	nogen


cProc	B$N2R4,<FAR,PUBLIC>	;Second pair
parmD	x
parmD	y
cBegin	nogen

	MOV	CL,fxR4+fyR4			;indicate type of values
	MOV	BX,OFFSET DGROUP: b$point2	;point at second pair table
	JMP	B$R4POINT

cEnd	nogen


	SUBTTL	B$PNR4 - POINT(X,Y) FUNCTION WHERE BOTH PARAMETERS ARE R4
	PAGE
;***
;B$PNR4 - POINT(X,Y) FUNCTION WHERE BOTH PARAMETERS ARE R4
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
; AX=attribute
;
;Modifies:
; per convention
;
;******************************************************************************
cbPoint	EQU 	13		;local copy of b$cbPoint
cProc	B$PNR4,<FAR,PUBLIC>
parmD	x			;x coordinate
parmD	y			;y coordinate
localV	point1,cbPoint		;place to put coordinate pair, if needed
cBegin

DbAssertRel  b$cbPoint,Z,cbPoint,GR_TEXT,<b$cbPoint not equal cbPoint in B$PNR4>
	CALL	B$SCINIT	;initialize screen if not already done

	LEA	BX,point1	;[BX] = pointer to local storage
	MOV	CL,22H		;[CL] = flag saying both are real
	cCall	B$GRMODE	; deturmine if we are in a text mode
	JE	ERRFC		; Brif so, function call error
	cCall	B$R4POINT,<seg_x,off_x,seg_y,off_y> ;format data at [point]
	cCall	B$DO_POINT	; go perform point function calling coord
cEnd

ERRFC:	JMP	B$ERR_FC	; ILLEGAL FUNCTION CALL

;***
;B$FPCOORD - Convert coords into integers if necessary.
;
;Purpose:
;
;Entry:
;	[SI]  = pointer to coordinate pair struct
;	[AL]  = structure flags
;
;Exit:
;	[AL]  = updated structure flags
;	structure updated
;
;Uses:
;
;Exceptions:
;	None.
;****
cProc	B$FPCOORD,<NEAR>
cBegin
	TEST	AL,fxI2		; does x need conversion?
	JNZ	COR2FP		; brif not

	FLD	DWORD PTR [SI].xR4
	CALL	B$ftolrnd	;[AX] = INT(ST0)
	MOV	[SI].xI2,AX	;Update I2 representation
	OR	[SI].fPoint,fxI2 ;Update flags
	MOV	AL,[SI].fPoint	; [AL] = updated flags
	TEST	AL,fyI2		; does y need conversion?
	JNZ	C2FP_EXIT	; brif not
;
;COR2FP - Convert 2nd coord into integer.
;
COR2FP:
	FLD	DWORD PTR [SI].yR4
	CALL	B$ftolrnd	;[AX] = INT(ST0)
	MOV	[SI].yI2,AX	;Update I2 representation
	OR	[SI].fPoint,fyI2 ;Update flags
	MOV	AL,[SI].fPoint	; [AL] = updated flags
C2FP_EXIT:
cEnd


sEnd	GR_TEXT

	END
