	TITLE	GRWINDOW - FP & WINDOW GRAPHICS COMPONENT INITIALIZATION
;***
; GRWINDOW - FP & WINDOW GRAPHICS COMPONENT INITIALIZATION
;
;	Copyright <C> 1988, Microsoft Corporation
;
;
; - WINDOW Statement - Generates 1 call if no parameters, or 2 calls:
;
;      WINDOW [[SCREEN] (x1,y1)-(x2,y2)]
;
;    Possibilities:
;
;      WINDOW [SCREEN] (x1,y1)-(x2,y2)
;	  |
;      B$WIND
;
;
;      WINDOW
;	  |
;      B$WIN0
;
;Purpose:
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


	INITIALIZER	B$xWINDINI ; Put B$xWINDINI in initializer list

sBegin	_DATA
externW	b$pFPCOORDW		; pointer to B$FPCOORDW
sEnd	_DATA


sBegin	_BSS

	externQ B$LXDIF		; Used to Map Logical
	externQ B$LYDIF		; to Physical coordinates
	EXTRN	B$GRFACX:DWORD	;defined in GWDATA.ASM
	EXTRN	B$GRFACY:DWORD	; defined in GWDATA.ASM

	externQ B$WXMIN		; Window x min (*) MUST BE IN THIS ORDER
	externQ B$WYMIN		; Window y min (*)
	externQ B$WXMAX		; Window x max (*)
	externQ B$WYMAX		; Window y max (*)
	externW B$WNDWSW 	;defined in GRINIT.ASM
	externW B$WNDWSC 	;defined in GRINIT.ASM


sEnd	_BSS

	externP B$FCMP
	externP	B$FIST		; round to integer in DX:AX
	externFP B$SWP8		; swaps 2 DP reals

sBegin	GR_TEXT
assumes CS,GR_TEXT

; Generic Feature selection list

	externNP B$ERR_FC
	externNP B$SCINIT	; initialize screen
	externNP B$ISWNDW


;OEM routines:
	externNP B$GRMODE

	EXTRN	B$fmldw:near
	externNP B$WNDINX	; defined in grfpinit.asm
	externNP B$COORD_NOSTEP
	externNP B$CLCWXY 	; defined in grfpinit.asm

	PAGE
	SUBTTL	Graphics initialization routines
;***
;B$xWINDINI - Initializer for the fp and WINDOW graphics component
;PLM B$xWINDINI()
;
;Purpose:
;
;Entry:
; None.
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
cProc	B$xWINDINI,<FAR>
cBegin

	MOV	[b$pFPCOORDW],GR_TEXTOFFSET B$FPCOORDW
cEnd

	SUBTTL	WINDOW - Define Window transformation

; SYNTAX: WINDOW [ [SCREEN] (Wx1,Wy1)-(Wx2,Wy2) ]

;	WINDOW defines the "Window" transformation from
;	Wx1,Wy1 (upper left x,y coordinates) to Wx2,Wy2
;	(lower right x,y coordinates).	The x and y coordinates
;	may be any Single Precision floating point number and
;	define the rectangle within the screen that graphics
;	will map into.
;	Initially, RUN (or WINDOW with no arguments) define the
;	entire screen as the Window.

;***
;B$WIND - process coordinate pairs, SCREEN flag, and execute WINDOW Statement
;
;Purpose:
;	Process coordinate pairs for WINDOW by storing in B$WXMAX
;	and B$WYMAX.  Make sure B$WXMIN<B$WXMAX and B$WYMIN<B$WYMAX. If
;	B$WXMIN=B$WXMAX or B$WYMIN=B$WYMAX issue a function call error.  If
;	B$WXMIN>B$WXMAX then exchange them. If B$WYMIN>B$WYMAX then exchange
;	them. If Screen flag<>0 then set B$WNDWSC=1. Set B$WNDWSW=1.
;Entry:
;	x1,y1,x2,y2 - R4 coordinate pairs defining new viewport.
;	fScreen     - SCREEN flag    NZ if SCREEN option
;Exit:
;	B$WXMAX,B$WYMAX,B$WNDWSW,B$WNDWSC updated
;Uses:
;	Per convention.
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****

cProc	B$WIND,<PUBLIC,FAR>,<ES,DI>
parmD	X1
parmD	Y1
parmD	X2
parmD	Y2
parmW	fScreen
cBegin
	CALL	B$SCINIT	; init screen if not already done
	MOV	B$WNDWSW,0	;Initially no WINDOW
	CALL	B$GRMODE 	;Legal graphics mode?
	JZ	WINDFC		;Error if not Graphics mode

;	Process first coord pair

	LEA	BX,X1		;point to first coord pair
	LEA	DX,Y1
	CALL	XYLOAD		
	MOV	BX,OFFSET DGROUP:B$WXMIN ; BX = address of B$WXMIN
	CALL	B$XYSAVE 	; SAVE x and y in B$WXMIN and B$WYMIN
	PUSH	BX		; save ptr to B$WXMIN

;	Process second coord pair

	LEA	BX,X2		;point to second coord pair
	LEA	DX,Y2
	CALL	XYLOAD		
	MOV	BX,OFFSET DGROUP:B$WXMAX ; BX = address of B$WXMAX
	CALL	B$XYSAVE	; SAVE x and y in B$WXMAX and B$WYMAX

	FLD	QWORD PTR [BX]	; ST0 = B$WXMAX
	POP	BX		; recover ptr to B$WXMIN
	FLD	QWORD PTR [BX]
	cCall	B$FCMP		; COMPARE B$WXMIN to B$WXMAX
	JZ	WINDFC		; Error if Wx1 = Wx2
	JC	WX1LESS 	; Brif Wx1 .lt. Wx2
	CALL	B$XCHGF		; exchange Wx1,Wx2
WX1LESS:
	FLD	B$WYMAX		; ST0 = B$WYMAX
	MOV	BX,OFFSET DGROUP:B$WYMIN
	PUSH	BX		; save ptr to B$WYMIN
	FLD	QWORD PTR [BX]
	cCall	B$FCMP		; COMPARE B$WYMIN to B$WYMAX

	POP	BX		; get back ptr to B$WYMIN
	JZ	WINDFC		; Error if Wy1 = Wy2
	JC	WY1LESS 	; Brif Wy1 .lt. Wy2
	CALL	B$XCHGF		; exchange Wy1,Wy2
WY1LESS:
	MOV	AL,1		; WARNING:  DON'T change this!  It has to
				; WARNING:  be a 1, not just non-zero!
	CMP	fScreen,0	; SCREEN option specified?
	JZ	NOTSCR		; Branch if not SCREEN option
	MOV	BYTE PTR B$WNDWSC,AL ; Flag Window SCREEN active
NOTSCR:
	MOV	BYTE PTR B$WNDWSW,AL ; Flag WINDOW as active
	CALL	B$WNDINX 	; recalculate window vars, update
				;logical accumulators B$GRFACX,B$GRFACY
cEnd				;and return

WINDFC:
	JMP	B$ERR_FC

;*** 
;XYLOAD - load s.p. X and Y on numeric stack
;
;Purpose:
;	Load X and Y on numeric stack
;
;Entry:
;	[BX] = pointer to X
;	[DX] = pointer to Y
;
;Exit:
;	[ST1] = X
;	[ST0] = Y
;
;Uses:
;	Per convention.
;
;******************************************************************************
cProc	XYLOAD,<NEAR>
cBegin				; entire routine
	FLD	DWORD PTR [BX]	; x coord
	XCHG	BX,DX
	FLD	DWORD PTR [BX]	; y coord
cEnd

;***
;B$WIN0 - process WINDOW statement with no parms
;
;Purpose:
;	Set WINDOW boundary variables (B$WXMIN,B$WXMAX,B$WYMIN,B$WYMAX) equal to
;	the floating point equivalent of current viewport boundaries
;	(B$VXMIN,B$VXMAX, B$VYMIN,B$VYMAX).
;Entry:
;	None.
;Exit:
;	B$WXMIN,B$WXMAX,B$WYMIN,B$WYMAX updated
;Uses:
;	Per convention
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	B$WIN0,<PUBLIC,FAR>,<BP>
cBegin
	MOV	BP,SP		;set up frame
	MOV	B$WNDWSW,0	;Initially no WINDOW
	CALL	B$GRMODE 	;Legal graphics mode?
	JZ	WINDFC		;Error if not Graphics mode
	CALL	B$CLCWXY 	;init window vars
cEnd


	SUBTTL	B$FPCOORDW - process & return integer coordinates
	PAGE
;***
;B$FPCOORDW
;
;Purpose:
; Calculate the physical screen coordinates of the given point. Relative
; coordinates, viewports and windowing are all considered in determining the
; final coordinates. Clipping is not done in this routine.
;
;Entry:
;	SI = pointer to point to be processed
;
;Exit:
; 	CX = physical X
; 	DX = physical Y
; 	JMPs to B$COORD_NOSTEP in grcoord.asm
;
;Modifies:
; 	AX,BX
;
;******************************************************************************

cProc	B$FPCOORDW,<NEAR>
cBegin

;
; If windows are active, we will be doing f.p. calcs. Make sure we have R4
; representations of our coordinate
;
	TEST	AL,fxR4 	;Is the first coord an R4?
	JZ	COR1INT 	;No, go convert it
COR3:
	TEST	AL,fyR4 	;Is the 2nd coord an R4?
	JZ	COR2INT 	;No, go convert it
COR4:
	LEA	CX,[SI].xR4	;[CX] = pointer to r4 value of X
	LEA	DX,[SI].yR4	;[DX] = pointer to r4 value of Y

	PUSH	DX		;SAVE PTR TO Y
	MOV	BX,CX		;[BX] = pointer to R4 value of x

	FLD	DWORD PTR [BX]	; [ST0] = X COORDINATE VALUE REF'D BY BX
	MOV	BX,OFFSET DGROUP:B$GRFACX
	TEST	[SI].fPoint,fxStep ; Now, were we stepping?
	JZ	COORINT_5	; jump if not
	FADD	DWORD PTR [BX]	; ST0 = ST0 + B$GRFACX
COORINT_5:
	FSTP	DWORD PTR [BX]	; B$GRFACX = ST0

	POP	BX		; [BX] = PTR TO Y
	FLD	DWORD PTR [BX]	; [ST0] = Y COORDINATE VALUE REF'D BY DX
	MOV	BX,OFFSET DGROUP:B$GRFACY
	TEST	[SI].fPoint,fyStep ; Now, were we stepping?
	JZ	COORINT_10	; jump if not
	FADD	DWORD PTR [BX]	; ST0 := ST0 + B$FY_BASE
COORINT_10:
	FSTP	DWORD PTR [BX]	; B$GRFACY := ST0

	CALL	MAPLPX		;MAP LOGICAL TO PHYSICAL X, RESULT IN AX
	PUSH	AX		;SAVE PHYSICAL X ON STACK
	CALL	MAPLPY		;MAP LOGICAL TO PHYSICAL Y, RESULT IN AX
	XCHG	DX,AX		; DX := PHYSICAL Y
	POP	CX		;CX := PHYSICAL X
	JMP	B$COORD_NOSTEP	;and continue

;
;COR1INT - Convert first coord into s.p.
;
;Parameters:
; [SI] = pointer to point structure
;
;Returns:
; [AL] = updated structure flags
; structure contents updated
;
COR1INT:
	MOV	BX,[SI].xI2	;[BX] = X in I2 format
	CALL	B$fmldw		;[ST0] = X
	FSTP	DWORD PTR [SI].xR4 ;save x coord
	OR	[SI].fPoint,fxR4 ;indicate R4 present
	MOV	AL,[SI].fPoint
	JMP	SHORT COR3
;
; COR2INT - Convert 2nd coord into s.p.
;
;Parameters:
; [SI] = pointer to point structure
;
;Returns:
; [AL] = updated structure flags
; structure contents updated
;
COR2INT:
	MOV	BX,[SI].yI2	;[BX] = Y in I2 format
	CALL	B$fmldw		;[ST0] = Y
	FSTP	DWORD PTR [SI].yR4 ; save x coord
	OR	[SI].fPoint,fyR4 ;indicate R4 present
	MOV	AL,[SI].fPoint
	JMP	SHORT COR4
cEnd	<nogen>			; B$FPCOORDW


;*** 
;B$XYSAVE - save X and Y as doubles pointed to by DI
;
;Purpose:
;	Save X and Y as doubles pointed to by DI
;
;Entry:
;	[ST1] = X
;	[ST0] = Y
;	[BX] points to first of two consecutive doubles.
;
;Exit:
;	[BX]   = X  (d.p. if MI_EMULATOR, else s.p.)
;	[BX+8] = Y  (d.p. if MI_EMULATOR, else s.p.)
;
;Uses:
;	Per convention.
;
;Preserves:
;	BX.
;
;******************************************************************************

cProc	B$XYSAVE,<PUBLIC,NEAR>
cBegin				; entire routine
	PUSH	BX		; save X pointer
	ADD	BX,8
	FSTP	QWORD PTR [BX]	; [BX].Y = d.p. ST0
	FWAIT			; don't change BX until FSTP finished
	POP	BX
	FSTP	QWORD PTR [BX]	; [BX].X = d.p. ST0
cEnd

;***
;B$XCHGF - xchg 2 DP numbers
;
;Purpose:
;	Given a pointer (ptr) to the start of a 8-byte d.p. number, exchange
;	it's contents with that of the  8-byte d.p. number at ptr+16
;
;Entry:
;	[BX] - points to dp buffer
;Exit:
;	entries swapped
;Uses:
;	Per Convention
;Exceptions:
;	None.
;****
cProc	B$XCHGF,<PUBLIC,NEAR>
cBegin
	PUSH	DS
	PUSH	BX		;far ptr to first number
	ADD	BX,16		; advance to second number
	PUSH	DS
	PUSH	BX		;far ptr to second number
	cCall	B$SWP8		;swap the numbers
cEnd

;***
;MAPLPY & B$MAPLY1
;
;Purpose:
;
;     [AX] = ( Y - B$WYMAX) * ((Vy2-Vy1)/(Wy2-Wy1))
;
; If WINDOW ( )-( ):
;
;     [AX] = -( Y - B$WYMIN) * ((Vy2-Vy1)/(Wy2-Wy1))
;
; where "Y" is either B$GRFACY (MAPLPY), or passed by reference (B$MAPLY1).
;
; The following sub-expressions are calculated and saved every time the VIEW
; or window changes so that these mapping routines will go faster:
;
;	Variable name	      Formula
;	B$LXDIF		      (Vx2-Vx1)/(Wx2-Wx1)
;	B$LYDIF		      (Vy2-Vy1)/(Wy2-Wy1)
;
;Entry:
; [BX]	= pointer to R4 Y coordinate (B$MAPLY1 only).
;
;Exit:
;	AX = result.
;
;Uses:
;	Per convention.
;
;******************************************************************************
cProc	MAPLPY,NEAR
cBegin
	MOV	BX,OFFSET DGROUP:B$GRFACY ;BX points to logical y accum
cEnd	<nogen>			; Fall into B$MAPLY1

cProc	B$MAPLY1,<NEAR,PUBLIC>
cBegin
	FLD	DWORD PTR [BX]	; ST0 = R4 window Y
	MOV	BX,OFFSET DGROUP:B$WYMIN ;BX points to window minimum y
	CALL	B$ISWNDW 	;Check window status flags
	PUSHF
	JB	MAPLY2		;Current window is not WINDOW ( )-( ) form
	MOV	BX,OFFSET DGROUP:B$WYMAX ;BX points to window maximum y
MAPLY2:
	MOV	CX,OFFSET DGROUP:B$LYDIF ;BX points to B$LYDIF ratio (see above)
	CALL	MAPMUL		;[AX]:=physical y (also BX for BASIC)
	POPF			;Check window status flags
	JB	MAPLY3		;Current window is not WINDOW ( )-( ) form 
	NEG	AX		; AX:=-AX (implements minus sign in $MAPLPY
				;expression - see above)
MAPLY3: 			;Return viewport relative integer coord
cEnd

;***
;MAPLPX
;
;Purpose:
;
;Entry:
;
;Exit:
; 	AX = INT( (B$GRFACX - B$WXMIN) * B$LXDIF )
;
;Uses:
;	Per convention.
;
;******************************************************************************
cProc	MAPLPX,NEAR
cBegin
	MOV	BX,OFFSET DGROUP:B$GRFACX ;BX points to logical x accum
cEnd	<nogen>			; Fall into B$MAPLX1

cProc	B$MAPLX1,<NEAR,PUBLIC>
cBegin
	FLD	DWORD PTR [BX]
	MOV	BX,OFFSET DGROUP:B$WXMIN 
	MOV	CX,OFFSET DGROUP:B$LXDIF ;BX points to B$LXDIF ratio (see above)
cEnd	<nogen>			; Fall into MAPMUL

;***
;MAPMUL - Mapper multiply subroutine
;
;Purpose:
;Exit:
;	AX = INT(([ST0] - [BX]) * [CX])
;
;******************************************************************************
cProc	MAPMUL,NEAR
cBegin
	FSUB	QWORD PTR [BX]	; ST0 = B$GRFACX-B$WXMIN
	XCHG	BX,CX
	FMUL	QWORD PTR [BX]	; ST0:=s.p. @ [BX] * ST0
	cCall	B$FIST		;Round to integer in DX:AX
cEnd

sEnd	GR_TEXT

	END
