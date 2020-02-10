	TITLE	GRLINE - LINE STATEMENT SUPPORT
	PAGE	56,132
;***
;GRLINE - LINE STATEMENT SUPPORT
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - LINE Statement:
;
;      LINE [(x1,y1)] -(x2,y2) [,[color] [,B[F]] [,style]]
;	|	|	  |
;	|    Coordinate Routines			   B$LINE
;	|						       |
;	+------------------------------------------------------+
;
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_BSS
	useSeg	GR_TEXT

	INCLUDE seg.inc


sBegin	_BSS
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
externW b$NSetC		
externW b$MapXYC		

externW B$VXMIN
externW B$VXMAX
externW B$VYMIN
externW B$VYMAX
externW B$GRPACX
externW B$GRPACY
externW B$GXPOS
externW B$GYPOS
externW B$GX_OLD
externW B$GY_OLD
externW B$MAXDEL
externW B$MINDEL
externW B$MINUPD
externW B$MAXUPD

externW B$LINSTL


externW	b$PenC			
externW	b$OffC			
externW b$Incr1			; major axis update value for Line
externW b$Incr2			; minor axis update value for Line
externW b$IncrY			; change for Y movement for Line
externW b$LineX			
externW b$LineY			
externW b$LineV			
externW	b$SetPixLastC		
externW	b$SetPixFirstC		
externW	b$BytesPerRow		

sEnd	_BSS



sBegin	GR_TEXT
assumes CS,GR_TEXT

externNP B$XDELT
externNP B$YDELT
externNP B$DOBOXF
externNP B$CLRATR
externNP B$COORD1		; process first coordinate pair
externNP B$COORD2		; process second coordinate pair
externNP B$ERR_FC
externNP B$ERR_DV0
externNP B$ERR_OV


externNP B$CLIPCK



;low-level routines:
externNP B$SCINIT		; Performs screen initialization

	SUBTTL	B$LINE - Line statement entry point
	PAGE
;***
;B$LINE - Line statement entry point
;void pascal B$LINE(I2 color, I2 style, I2 fBF)
;
;Purpose:
; Draw line, box or filled box, with or without line styling, according to
; the requested form of line statement.
;
;Input:
; color = color spec (-1 if default)
; style = line style (-1 if default to solid line)
; fBF	= box spec:	0 if normal line (no box)
;			1 if box with no fill
;			2 if filled box
;
;Output:
; none
;
;Modifies:
; Per convention
;
;Exceptions:
; Control may be transfered to an error routine such as B$ERR_FC
;
;******************************************************************************
cProc	B$LINE,<FAR,PUBLIC>
parmW	color
parmW	style
parmW	fBf
cBegin

	CALL	B$SCINIT	; inititalize screen if not already done

	cCall	B$COORD1	; process first coord pair
	MOV	AX,B$GRPACX	;move first x coordinate into B$GX_OLD
	MOV	B$GX_OLD,AX	;B$GX_OLD:=B$GXPOS
	MOV	AX,B$GRPACY	;move first y coordinate into B$GY_OLD
	MOV	B$GY_OLD,AX	;B$GY_OLD:=B$GYPOS
	cCall	B$COORD2	; process second coord pair

	MOV	AX,style
	MOV	B$LINSTL,AX	; Set LINE STYLE
	MOV	AX,color	; [AX] = COLOR ATTRIBUTE
	CALL	B$CLRATR 	;SET COLOR
	MOV	AX,B$GX_OLD	; [AX] = B$GX_OLD
	MOV	DX,B$GY_OLD	; [DX] = B$GY_OLD
	MOV	CX,fBF		; [CX] = box fill flag
	JCXZ	REGLIN		; 0 = NORMAL LINE
	LOOP	BOXFIL		; 2 = DO BOX WITH FILL
	JMP	SHORT BOXNF1	; 1 = DO BOX, NO FILL

REGLIN: 			;DO NORMAL LINE
	XCHG	AX,CX		; [CX] = B$GX_OLD
	CALL	B$LINDRW 	;DRAW LINE FR.(CX,DX) TO (B$GXPOS,B$GYPOS)
	JMP	SHORT LINE_90	; and exit

BOXNF1:
	XCHG	AX,CX		; [CX] = B$GX_OLD
	CALL	B$BOXNOF 	; draw box
	JMP	SHORT LINE_90	;EXIT LINE STATEMENT

BOXFIL:
	LOOP	ERRFC		; else FUNCTION CALL ERROR
	XCHG	AX,CX		; [CX] = B$GX_OLD
	CALL	B$DOBOXF 	; do the box fill

LINE_90:

	MOV	B$LINSTL,-1	; RESTORE LINE STYLE TO SOLID

cEnd

ERRFC:	JMP	B$ERR_FC	;ILLEGAL FUNCTION CALL


	PAGE
	SUBTTL	Line Statement Support Routines
;***
;B$BOXNOF
;
;Purpose:
; Draw a BOX.
;
;Entry:
;
;Exit:
;
;Uses:
;
;******************************************************************************
cProc	B$BOXNOF,<NEAR,PUBLIC>
cBegin
	MOV	BX,B$GYPOS
	PUSH	BX		;SAVE Y2
	PUSH	DX		;SAVE Y1
	XCHG	DX,BX		;MOVE Y2 TO Y1
	CALL	B$LINDRW 	;DO TOP LINE
	POP	BX		;MOVE Y1 TO Y2
	MOV	B$GYPOS,BX
	XCHG	DX,BX		;RESTORE Y1 TO [DX]
	CALL	B$LINDRW
	POP	BX		;GET BACK Y2
	MOV	B$GYPOS,BX	;AND RESTORE
	MOV	BX,B$GXPOS	;GET X2
	PUSH	CX		;SAVE X1
	MOV	CX,BX		;SET X1=X2
	CALL	B$LINDRW
	POP	BX
	MOV	B$GXPOS,BX	;SET X2=X1
	MOV	CX,BX		;RESTORE X1 TO [CX]
cEnd	nogen			;fall into B$LINDRW

;***
;B$LINDRW
;
;Purpose:
; Draw a line.
;
;Entry:
;
;Exit:
;
;Uses:
;
;******************************************************************************
cProc	B$LINDRW,<NEAR,PUBLIC>,<SI,DI> 
cBegin
	PUSH	CX		;SAVE COORDINATES
	PUSH	DX
	CALL	DOGRPH
	MOV	AX,B$GRPACX	;RESTORE ORIGINAL SECOND COORDINATE
	MOV	B$GXPOS,AX
	MOV	AX,B$GRPACY	;FOR B$BOXNOF CODE
	MOV	B$GYPOS,AX
	POP	DX
	POP	CX
cEnd


;***
;HorizLine		Draw a horizontal line
;
;Purpose:
; Draw a horizontal line using.
;
;Entry:
; [DX]	= B$GYPOS = y1 = y2
; [cx]	= x1
; B$GXPOS = x2
;
;Exit:
; Graphics accumulators updated.
;
;Modifies:
; bx,si,ax,cx,dx
;
;******************************************************************************
HorizLine:
	call	B$XDELT		; bx = abs(x2-x1)
	pushf			; remember flag setting
	push	cx		; save x
	jnc	noxchg		; x1 < x2
	xchg	cx,[B$GXPOS]	;ensure CX = MIN(X1,X2)
noxchg:
	inc	bx		; bx = # of pixels to draw
	push	bx		; preserve bx across mapxyc
	call	[b$MapXYC]	;set graphics cursor to (x1,y1)
	pop	bx		; restore pixel count
	call	[b$NSetC]	;draw the line
	pop	cx		; restore x
	popf			; recall if exchange took place
	jnc	nosort		; x1 and x2 were not exchanged
	xchg	cx,[B$GXPOS]	;ensure CX = MIN(X1,X2)
nosort:
	ret

;***
;DOGRPH
;
;Purpose:
; DRAWS A LINE FROM ([CX],[DX]) TO (B$GXPOS,B$GYPOS)
;
;Entry:
;
;Exit:
;
;Uses:
;
;******************************************************************************
DOGRPH:
	CALL	CLIPP		; Clip the line before plotting
	JZ	DOGRP0		; Brif line is visible, plot it
	RET
DOGRP0:
	XCHG	CX,[B$GXPOS]	;ensure CX = MIN(X1,X2)
	XCHG	DX,[B$GYPOS]	;ensure DX = MIN(Y1,Y2)

	cmp	B$LINSTL,-1	; line style active ?
	jnz	nohorizontal	; brif so (do not special case
				; horizontal line if line style
				; active)
	cmp	dx,B$GYPOS	; y1 = y2?
	je	HorizLine	; special case horizontal line
nohorizontal:
	MOV	AX,B$GXPOS	
	MOV	BX,B$GYPOS	
	MOV	DI,B$LINSTL	
	;	draw a line between (AX,BX) and (CX,DX) with linestyle (DI),
	;	color in b$AttrC
	CALL	B$LineXYV	;call low-level

DOGRPHX:			
	mov	[B$LINSTL],di	;[speed]
	RET

	PAGE
	SUBTTL	LINE CLIPPING ROUTINES

;***
;CLIPP
;
;Purpose:
; Line clipping routine
;
;Entry:
; [CX]	= x1
; [DX]	= y1
; B$GXPOS= x2
; B$GYPOS= y2
;
;Exit:
; ZF set if the line is visible.
; NZ if the line is not visible.
;
;Uses:
;
;
;******************************************************************************
CLIPP:
	CALL	CLIPC		;status of visibility, [AL]=p1, [AH]=p2
CLIPP0:
	OR	AX,AX
	JZ	CLIPPX		;Brif points visible
	TEST	AL,AH		;Any octant common?
	JNZ	CLIPPX		;Brif line outside, not visible
	OR	AL,AL		;Line may be visible, try to clip
	JNZ	CLIPP1		;Brif point 1 not visible
	XCHG	CX,[B$GXPOS]	;ensure CX = MIN(X1,X2)
	XCHG	DX,[B$GYPOS]	;ensure DX = MIN(Y1,Y2)
	XCHG	AL,AH		; exchange point 1,2 status
CLIPP1:
	MOV	BX,B$VXMIN
	SAR	AL,1
	JB	CLIPX1		;If x1 .lt. vx1, modify x1,y1
	MOV	BX,B$VXMAX
	SAR	AL,1
	JB	CLIPX1		;If x1 .lt. vx2, modify x1,y1
	MOV	BX,B$VYMIN
	SAR	AL,1
	JB	CLIPY1		;If y1 .lt. vy1, modify x1,y1
	SAR	AL,1
	JNB	CLIPP2
	MOV	BX,B$VYMAX	;If y1 .gt. vy2, modify x1,y1
CLIPY1:
	CALL	MODXY2
	JMP	SHORT CLIPP2
CLIPX1:
	CALL	MODXY1
CLIPP2:
	CALL	B$CLIPCK 	;[AL]= new status for current point
	JMP	SHORT CLIPP0	;Try for further clipping

CLIPC:
	PUSH	CX		;Save x1
	PUSH	DX		;Save y1
	CALL	B$CLIPCK 	;Get status of p(x1,y1)
	XCHG	AH,AL		;[AH]= p1 status
	MOV	CX,B$GXPOS	;x2
	MOV	DX,B$GYPOS	;y2
	CALL	B$CLIPCK 	;Get status of p(x2,y2)
	XCHG	AH,AL		;[AH]= p2 status, [AL]= p1 status
	POP	DX		;y1
	POP	CX		;x1
CLIPPX:
	RET

;***
;MODXY1
;
;Purpose:
; Modify (clip) x,y into VIEW range
; Given: x1,y1 and x2,y2 - clip x1,y1
;
; x is set to VIEW boundary: vx1 or vx2
; y= y1+(y2-y1)/(x2-x1)*(vx1 or vx2-x1)
;
;Entry:
; [BX]= View boundary vx1, vx2, vy1, or vy2
; [CX]= x, [DX]= y
;
;Exit:
;
;Uses:
;
;******************************************************************************
MODXY1:
	PUSH	AX		;Save VIEW status
	PUSH	BX		;Save boundary
	SUB	BX,CX		;[BX]= boundary-x1
	MOV	SI,BX		;[SI]= Dxb
	MOV	BX,B$GXPOS
	SUB	BX,CX		;[BX]= (Dx)=x2-x1
	MOV	CX,SI		;[CX]= Dxb
	MOV	SI,DX		;[SI]= y1
	MOV	AX,B$GYPOS
	SUB	AX,DX		;[AX]= (Dy)=y2-y1
	CWD			;Extend sign into [DX]
	CALL	B$IDIVBX 	;[AX]= (D)=Dy\Dx
	PUSH	DX		;[DX]= (R)=Dy remainder Dx
	IMUL	CX		;[AX]= D*Dxb
	ADD	SI,AX		;[SI]= y1+D*Dxb
	POP	AX
	IMUL	CX		;[DXAX]= R*Dxb
	SAL	AX,1
	RCL	DX,1		;[DXAX]= R*Dxb*2
	CALL	B$IDIVBX 	;[AX]= (R*Dxb*2)\Dx
	OR	AX,AX		; Simulate CINT(R*Dxb/Dx)
	JS	modxyc		;don't inc if neg
	INC	AX		;[AX]= (R*2*Dxb)\Dx+1
modxyc:
	SAR	AX,1		;[AX]= ((R*2*Dxb)\Dx+1)\2
	ADD	AX,SI		;[AX]= y1+D*Dxb+((R*2*Dxb)\Dx+1)\2
	MOV	DX,AX		;[DX]= New Y
	POP	CX		;[CX]= New X
	POP	AX		;View Status
	RET


;***
;B$IDIVBX - Simulate IDIV instruction
;
;Purpose:
; Perform an "IDIV BX" after making sure neither a /0 or overflow error will
; occur. The test for overflow is that the next power of two larger than each
; of the two numbers is found and if the difference is more than 2^14, an
; overflow is flagged.
;
;Entry:
; [BX]	  = denominator
; [DX:AX] = numerator
;
;Exit:
; [AX]	  = quotient
; [DX]	  = remainder
;
;****

cProc	B$IDIVBX,<NEAR,PUBLIC>
cBegin
	OR	BX,BX		;Is denominator 0?
	JZ	ERR0		;Error if so.

	PUSH	AX
	OR	AX,DX		;Is numerator 0?
	POP	AX
	JZ	RET0		;If so, return 0 for quotient & remainder.

;	Both numerator and denominator are non-zero.  Now take the absolute
;	values of each and figure the difference in the magnitudes of the
;	two numbers by finding the next power of two larger than each.
;	If the difference in magnitudes is more than 2^14, give B$ERR_OV.


	PUSH	AX
	PUSH	BX
	PUSH	CX
	PUSH	DX

	XOR	CX,CX		;CH=magnitude of numerator
				;CL=magnitude of denominator
	OR	BX,BX		;Is BX negative?
	JNS	BXPOS		;If so, negate it
	NEG	BX
BXPOS:
	OR	DX,DX		;Is DX:AX negative?
	JNS	DXAXPOS 	;If so, negate it
	NOT	AX
	NOT	DX
	ADD	AX,1
	ADC	DX,0
DXAXPOS:

BXLOP:
	INC	CL		;Figure BX's magnitude
	SHR	BX,1
	JNZ	BXLOP
				;Figure DX:AX's magnitude
	OR	DX,DX
	JZ	AXLOP
	MOV	CH,16d
	MOV	AX,DX
AXLOP:
	INC	CH
	SHR	AX,1
	JNZ	AXLOP

	SUB	CH,CL		;Figure difference in magnitudes
	CMP	CH,14d		;More than 2^14?
	JG	GOVERR		;Error if so

	POP	DX
	POP	CX
	POP	BX
	POP	AX

	IDIV	BX		;Now that /0 errors and overflows
				;have been ruled out, it's OK to
				;do an IDIV.
RET0:
cEnd


ERR0:
	JMP	B$ERR_DV0
GOVERR:
	JMP	B$ERR_OV


MODXY2:
	CALL	INTXY2		;xchg x1,y1 B$GXPOS,B$GYPOS
	CALL	MODXY1		;Clip x1,y1
INTXY2:
	XCHG	CX,DX		;xchg x1,y1 B$GXPOS,B$GYPOS
	PUSH	B$GXPOS
	PUSH	B$GYPOS
	POP	B$GXPOS
	POP	B$GYPOS
	RET


	PAGE
	SUBTTL	LINE UTILITIES
;***
;B$CLINE2
;
;Purpose:
;
;Entry:
;
;Exit:
;
;Modifies:
;
;******************************************************************************
cProc	B$CLINE2,<NEAR,PUBLIC>
cBegin
	MOV	AX,B$GRPACX	;DRAW LINE FROM [CX],[DX]
	MOV	B$GXPOS,AX	;TO B$GRPACX,Y
	MOV	AX,B$GRPACY
	MOV	B$GYPOS,AX
	JMP	B$LINDRW 	;GO DRAW THE LINE
cEnd	nogen


;***
;B$GTABSC - GET ABSOLUTE COORDS
;
;Purpose:
; ([CX],[DX])=(B$GRPACX+[CX],B$GRPACY+[DX])
;
;Entry:
; B$GRPACX = x center
; B$GRPACY = y center
;
;Exit:
; [cx]	 =  x center
; [dx]	 =  y center
;
;Modifies:
;	cx,dx
;
;******************************************************************************
cProc	B$GTABSC,<NEAR,PUBLIC>
cBegin
	ADD	CX,[B$GRPACX]	;[CX]=X CENTER + [CX]
	ADD	DX,[B$GRPACY]	;[DX]=Y CENTER + [DX]
cEnd


;***
; B$LineXYV - Draw a line
;
;Purpose:
;	Moved here from LLCGRP with revision [16].
;	Draw a line in a given style between two points.  The line will
;	be drawn according to the pattern given in the Line style mask
;	(DI).  For each pixel that is going to be set for the line,
;	the corresponding bit in DI is tested.	If it is 1, the pixel
;	is set, otherwise that pixel is skipped.  If the line is longer
;	than 16 pixels, the line style mask is repeated.
;
;Entry:
;	DI = Line style mask
;	AX,BX = one end point X,Y
;	CX,DX = other end point X,Y
;
;Exit:
;	DI = Line style mask
;
;Uses:
;	DI is used as an input parameter and may be modified as it is
;	rotated to get successive bits to test.
;
;Exceptions:
;	none
;******************************************************************************
cProc	B$LineXYV,<NEAR>,<BP,SI,ES>
cBegin
	CMP	AX,CX		;make sure X <= X2
	JLE	Line1		;go if (AX,BX) left of (CX,DX)
	XCHG	AX,CX		;ensure leftmost point in (AX,BX)
	XCHG	BX,DX		;line will always move left to right
Line1:				
	SUB	CX,AX		;CX <- X distance (will be >= 0)
	SUB	DX,BX		;DX <- Y distance
	MOV	SI,DX		;SI <- abs(Y distance)
	JGE	Line2		
	NEG	SI		
Line2:				
	PUSH	CX		;save axes directed distances
	PUSH	DX		
	MOV	CX,AX		;set up raster offset and pixel bit mask
	MOV	DX,BX		;  (point in CX,DX for MAPXYC)
	CALL	[b$MapXYC]	
	CALL	[b$SetPixFirstC] ;set up ES and EGA, if applicable
	POP	DX		;restore axes distances
	POP	CX		
				
	OR	DX,DX		;Y distance negative?
	MOV	DX,b$BytesPerRow   ;bytes per pixel line (assume positive)
	JNS	Line3		;go if positive
	NEG	DX		;negate direction
Line3:
	MOV	b$IncrY,DX	;save for Y update of BP
	MOV	BP,b$OffC	;BP <- raster offset
	CMP	SI,CX		;is X or Y the major axis?
	JGE	LineY		;go if Y is major
				;fall thru if X is major
LineX:				;X-major lines, CX <- major (X) axis length
	MOV	AX,SI		;Incr1 <- minor * 4
	SHL	AX,1		
	SHL	AX,1		
	MOV	b$Incr1,AX	
	SUB	AX,CX		;SI <- Incr1 - major
	XCHG	AX,SI		
	SUB	AX,CX		;Incr2 <- (minor - major) * 4
	SHL	AX,1		
	SHL	AX,1		
	MOV	b$Incr2,AX	
	INC	CX		;#points <- distance + 1
	MOV	AX,b$PenC	;AL <- bit accumulator
				;AH <- color info
	MOV	BH,AL		;BH <- current bit mask
	XOR	AL,AL		;clear pixel accumulator
	call	[b$LineX]	;draw X-major line
	jmp	short LineExit	
LineY:				;Y-major lines
	OR	CX,CX		;vertical line?
	JZ	LineV		;go if so
	MOV	AX,CX		
	SHL	CX,1		;Incr1 <- minor * 4
	SHL	CX,1		
	MOV	b$Incr1,CX	
	XCHG	CX,SI		;CX <- major (Y) axis length
	SUB	SI,CX		;SI <- Incr1 - major
	SUB	AX,CX		;Incr2 <- (minor - major) * 4
	SHL	AX,1		
	SHL	AX,1		
	MOV	b$Incr2,AX	
	INC	CX		;#points <- distance + 1
	MOV	AX,b$PenC	;AL <- bit accumulator
				;AH <- color info
	call	[b$LineY]	;draw Y-major line
	jmp	short LineExit	
LineV:				;vertical lines
	MOV	CX,SI		;#points <- distance + 1
	INC	CX		
	MOV	AX,b$PenC	;AL <- bit accumulator
				;AH <- color info
	call	[b$LineV]	;draw vertical line
LineExit:			
	CALL	[b$SetPixLastC] ;reset EGA for BIOS
cEnd
sEnd	GR_TEXT

	END
