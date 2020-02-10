	TITLE	GRVIEW - Graphics VIEW Support
;***
; GRVIEW -  Graphics VIEW Support
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - VIEW Statement - Generates 1 call if no parameters, or 3 calls:
;
;      VIEW [[SCREEN][(x1,y1)-(x2,y2) [,[color] [,[boundary]]]] ]
;
;    Possibilities:
;
;      VIEW [SCREEN] (x1,y1)-(x2,y2) [,[color] [,[boundary]]]
;	 |
;     B$VIEW
;
;
;      VIEW
;	 |
;     B$VEW0
;
;
; - VIEW PRINT Statement
;
;
;      VIEW PRINT [top screen line TO bottom screen line]
;	      |
;	   B$VWPT
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	GR_TEXT 	

	INCLUDE seg.inc 	; segment definitions
	INCLUDE const.inc	; b$IOFLAG field definitions


sBegin	_DATA			

	externB b$LINCNT 	; support VIEW PRINT statement
	externB b$WDOTOP 	
	externB b$WDOBOT 	
	externB	b$IOFLAG	; Misc. IO flags.  Defined in GWINI.ASM

sEnd	_DATA			

sBegin	_BSS			
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
externW b$SetAttr		

	externB b$KEY_SW 	;defined in GWDATA.ASM
	externW B$VXSIZ		;defined in GRINIT.ASM
	externW B$VYSIZ		;defined in GRINIT.ASM
	externW B$VXMIN		;defined in GRINIT.ASM
	externW B$VYMIN		;defined in GRINIT.ASM
	externW B$VXMAX		;defined in GRINIT.ASM
	externW B$VYMAX		;defined in GRINIT.ASM
	externW B$VXOFF		;defined in GRINIT.ASM
	externW B$VYOFF		;defined in GRINIT.ASM
	externW B$GXPOS		;defined in GRINIT.ASM
	externW B$GYPOS		;defined in GRINIT.ASM
	externW B$GRPACX 	;defined in GWDATA.ASM
	externW B$GRPACY 	;defined in GWDATA.ASM

	externB B$VIEWSW 	;defined in GWDATA.ASM
	externB B$VIEWSC 	;defined in GWDATA.ASM

sEnd	_BSS			

assumes CS,GR_TEXT		
sBegin	GR_TEXT 		


	externNP B$ERR_FC	
	externNP B$ERR_AFE	
	externNP B$SCINIT	; initialize screen






	SUBTTL	VIEW - Define Viewport transformation

; SYNTAX: VIEW [SCREEN] [(vx1,vy1)-(vx2,vy2) [,[fill] [,[border]]]]

;	VIEW defines the "viewport" transformation from
;	vx1,vy1 (upper left x,y coordinates) to vx2,vy2
;	(lower right x,y coordinates).	The x and y coordinates
;	must be within the physical bounds of the screen  and
;	define the rectangle within the screen that graphics
;	will map into.
;	Initially, RUN (or VIEW with no arguments) define the
;	entire screen as the viewport.	The 'fill' attribute
;	allows the user to fill the VIEW area with a color.
;	If fill is omitted, the VIEW area is not filled.
;	The 'border' attribute allows the user to draw a line
;	surrounding the viewport if space for a border is
;	available.  If border is omitted, no border is drawn.

	externNP B$VWMAPC	
	externNP B$VEWINI	
	externNP B$VWFILL	
	externNP B$GRMODE	
	externNP B$BOXNOF	
	externNP B$SCNLOC	; update and display user cursor

;***
;ViewCoord1 - Process first coordinate pair for VIEW statement
;
;Purpose:
;	Process first coordinate pair for VIEW statement by saving in
;	B$VXMIN and B$VYMIN.  CALL B$VEWINI to initialize VIEW statement variables.
;Entry:
;	BX=first x coordinate = Vx1
;	DX=first y coordinate = Vy1
;Exit:
;	View variables updated
;Uses:
;	Per convention
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	ViewCoord1,<NEAR>	
cBegin				
	PUSH	BX		;Save x coord
	PUSH	DX		;Save y coord
	CALL	B$VEWINI 	;initialize VIEW variables
	POP	DX		;Restore x coord
	POP	BX		;Restore y coord
	MOV	B$VXMIN,BX	;B$VXMIN:=BX=vx1
	MOV	B$VYMIN,DX	;B$VYMIN:=DX=vy1
cEnd				;Return

;***
;ViewCoord2 - process second coordinate pair for VIEW statement
;
;Purpose:
;	Process second coordinate pair for VIEW statement.  Compare
;	Vx1 and Vx2, storing the largest of the two in B$VXMAX and the smallest
;	in B$VXMIN.  Compare vy1 and vy2, storing the largest of the two in
;	B$VYMAX and the smallest in B$VYMIN.  Issue a function call error if
;	VX1=VX2 or if VY1=VY2.
;Entry:
;	BX=second x coordinate = VX2
;	DX=second y coordinate = VY2
;Exit:
;	B$VXMIN,B$VYMIN,B$VXMAX,B$VYMAX updated
;Uses:
;	Per convention.
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	ViewCoord2,<NEAR>	
cBegin				
				;Make Vx1 .lt. Vx2 and Vy1 .lt. Vy2
				;If Vx1=Vx2 or Vy1=Vy2, then error.
	MOV	AX,B$VYMIN	;AX:=B$VYMIN
	CMP	AX,DX		;Compare B$VYMIN to VY2
	JZ	VEWFCE		;Error if Vy1=Vy2
	JB	VY1LT2		;Brif Vy1 .lt. Vy2
	XCHG	AX,DX		; else exchange
VY1LT2:
	MOV	B$VYMIN,AX	;Store Vy1
	MOV	B$VYMAX,DX	; and  Vy2
	CMP	B$VYSIZ,DX	;Screen y max cannot be
	JB	VEWFCE		;less than Vy2
	MOV	AX,B$VXMIN
	CMP	AX,BX		;Compare B$VXMIN(=VX1) to Vx2
	JZ	VEWFCE		;Error if Vx1=Vx2
	JB	VX1LT2		;Brif Vx1 .lt. Vx2
	XCHG	AX,BX		; else exchange
VX1LT2:
	MOV	B$VXMIN,AX	;Store Vx1
	MOV	B$VXMAX,BX	; and  Vx2
	CMP	B$VXSIZ,BX	;Screen x max cannot be
	JB	VEWFCE		;less than Vx2
cEnd				

VEWFCE:
	JMP	B$ERR_FC	;Error

;***
;B$VIEW - Process VIEW statement with parameters
;
;Purpose:
;	Set viewport as specified by current values of (B$VXMIN,B$VYMIN) and
;	(B$VXMAX,B$VYMAX) which get initialized in ViewCoord1 and ViewCoord2.
;	Update B$VXOFF and B$VYOFF if the SCREEN option was not specified.
;	If fill was requested, fill the viewport with the desired color
;	by calling B$VWFILL.  If a border was requested, draw the border
;	by calling B$BOXNOF in the LINE statement code in GENG86.
;	Update B$VIEWSC and B$VIEWSW to reflect the current active VIEW option.
;	B$VIEWSC = 0 means the most recent VIEW statement was not a VIEW SCREEN
;		= 1 means the most recent VIEW statement was a VIEW SCREEN
;	B$VIEWSW = 0 means the most recent VIEW statement was a VIEW w/ no parms
;		= 1 means the most recent VIEW statement was a VIEW with parms
;Entry:
;	x1,y1,x2,y2 - I2 coordinate pairs defining new viewport.
;	Color	    - fill color or -1 if default
;	Border	    - border color or -1 if default
;	fScreen     - screen flag: NZ if VIEW SCREEN option
;Exit:
;	Variables updated as noted above
;Uses:
;	Per convention.
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	B$VIEW,<PUBLIC,FAR>	
parmW	X1			
parmW	Y1			
parmW	X2			
parmW	Y2			
parmW	Color			
parmW	Border			
parmW	fScreen 		
cBegin				
	CALL	B$SCINIT	; init screen if not already done
	MOV	BX,X1		
	MOV	DX,Y1		
	cCall	ViewCoord1	;process first coord pair
	MOV	BX,X2		
	MOV	DX,Y2		
	cCall	ViewCoord2	;process second coord pair

	CALL	B$GRMODE 	;Legal graphics mode?
	JZ	VEWFCE		;Function call error if not in graphics mode
	MOV	BL,1		;default is VIEW SCREEN active
	CMP	fScreen,0	;VIEW SCREEN option specified?
	JNZ	VEWSCR		;Brif so, don't set offsets
	DEC	BL		;no VIEW SCREEN
	MOV	AX,B$VXMIN	;AX:=B$VXMIN
	MOV	B$VXOFF,AX	;B$VXOFF:=B$VXMIN for VIEW no SCREEN
	MOV	AX,B$VYMIN	;AX:=B$VYMIN
	MOV	B$VYOFF,AX	;B$VYOFF:=B$VYMIN for VIEW no SCREEN
VEWSCR:
	MOV	B$VIEWSC,BL	;Set VIEW SCREEN active
	MOV	AX,Color	;get color attribute
	INC	AX		;default fill attribute?
	JZ	VBRDR		;Yes: don't fill but check border
	DEC	AX		;recover correct attribute
	CALL	B$VWFILL 	;No: fill viewport with color
VBRDR:				;Check border parameter
	MOV	AX,Border	;get border color
	INC	AX		;default border color?
	JZ	VEWRET		;Yes: don't draw border
	DEC	AX		;recover real border color
	CALL	[b$SetAttr]	;set border attr
	JB	VEWFCE		;ERROR IF ILLEGAL ATTRIBUTE
	CALL	DrawBorder	;draw border to view region
VEWRET:
	MOV	B$VIEWSW,1	;Set Viewport active
	CALL	B$VWMAPC 	;Set up View "C" bounds for PAINT.
				; and Graphics cursor to VIEW center
cEnd				

;***
;DrawBorder - Draw border for viewport
;
;Purpose:
;	 Draw viewport border by calling B$BOXNOF in GENG86.  B$BOXNOF
;	 is a routine in the code for the LINE statement which FG_DRAW
;	 an unfilled box.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per convention
;Exceptions:
;	None.
;****
cProc	DrawBorder,<NEAR>	
cBegin				
	MOV	AX,B$VXMAX
	PUSH	AX
	INC	AX		;Line outside viewport
	MOV	B$GRPACX,AX
	MOV	B$GXPOS,AX	;vx2
	MOV	AX,B$VYMAX
	PUSH	AX
	INC	AX		;Line outside viewport
	MOV	B$GRPACY,AX
	MOV	B$GYPOS,AX	;vy2
	MOV	CX,B$VXMIN
	PUSH	CX
	DEC	CX		;Line outside viewport
	MOV	DX,B$VYMIN
	PUSH	DX
	DEC	DX		;Line outside viewport
	XOR	AX,AX
	MOV	B$VXMIN,AX	;Vx1= 0
	MOV	B$VYMIN,AX	;Vy1= 0
	MOV	AX,B$VXSIZ
	MOV	B$VXMAX,AX	;Vx2= x max
	MOV	AX,B$VYSIZ
	MOV	B$VYMAX,AX	;Vy2= Y max
	CALL	B$BOXNOF 	;Draw Viewport Border
	POP	B$VYMIN		;vy1
	POP	B$VXMIN		;vx1
	POP	B$VYMAX		;vy2
	POP	B$VXMAX		;vx2
cEnd				

;***
;B$VEW0 - process VIEW statement with no parameters
;
;Purpose:
;	Process a VIEW statement that has no parameters.
;	Call B$VEWINI to initialize viewport to the entire screen.
;Entry:
;	None.
;Exit:
;	None.
;Uses:
;	Per Convention.
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	B$VEW0,<PUBLIC,FAR,FORCEFRAME> 
cBegin				
	CALL	B$SCINIT	; init screen if not already done
	CALL	B$GRMODE 	;Legal graphics mode?
	JZ	VWFERR		;Function call error if not in graphics mode
	CALL	B$VEWINI 	;viewport := screen
cEnd				

VWFERR:
	JMP	B$ERR_FC	;Function call error


;***
;B$VWPT - VIEW PRINT statement
;
;Purpose:
;	VIEW PRINT statement for setting the boundaries of the text
;	window.
;
;	SYNTAX:
;	   VIEW PRINT [<top line> TO <bot line>][,<left col> TO <right col>]
;
;	Note that each TO clause which is present must have both numbers
;	present.
;	   "VIEW PRINT" alone will reset the text window to whole screen.
;	   "VIEW PRINT x TO y" will set top and bottom only.
;	   "VIEW PRINT ,x TO y" will set left and right only.
;	An illegal function call will result
;	   -if <top line> is .GT. <bot line>,
;	   -if <left col> is .GT. <right col>,
;	   -if any parameter is larger than current screen dimensions,
;
;	------------------------ NOTE -----------------------------
;	Since the screen editor does not yet support the arbitrary
;	setting of the right and left margins of the screen window,
;	the second TO clause of the VIEW PRINT statement is not yet
;	allowed and will make for a syntax error if present.
;	-----------------------------------------------------------
;
;Entry:
;	TopLine = Top line (default -1)
;	BotLine = Bottom line (default -1)
;Exit:
;	WDOTOP and WDOBOT
;Uses:
;	Per convention.
;Exceptions:
;	Control may be transfered to B$ERR_FC
;****
cProc	B$VWPT,<PUBLIC,FAR>	
parmW	TopLine 		
parmW	BotLine 		
cBegin

	TEST	b$IOFLAG,RED_OUT ;	Is output redirected?
	JNZ	VWPT_XIT	; brif so -- do nothing.

	MOV	AH,b$KEY_SW	
	MOV	AL,b$LINCNT	
	MOV	BX,TopLine	
	MOV	DX,BotLine	
	INC	BX		;default?
	JZ	DEFAULT 	;brif so
	DEC	BX		;top line = 0?
	JZ	VPFCE		;brif so
	JS	VPFCE		;or if < 0
	CMP	BX,DX		;top line > bottom line
	JA	VPFCE		;brif so .... error
	PUSH	DX		;save [dx]
	ADD	DL,AH		;make sure bottom line is not off screen and
	CMP	DL,AL		;does'nt include function key display line (if ON)
	POP	DX		;restore [dx]
	JA	VPFCE		;brif above .... error
	JMP	SHORT NOTDEF
DEFAULT:
	INC	BX		; [bx] = 1
	XOR	DX,DX
	MOV	DL,AL		;[dl] = line count
	SUB	DL,AH		;[dl] = line count or line count - 1
NOTDEF:
	MOV	b$WDOTOP,BL	;set text window top
	MOV	b$WDOBOT,DL	;set text window bottom
	MOV	DL,BL		;[dl] = window top , cursor top
	MOV	DH,1		;[dh] = window left , cursor left
	CALL	B$SCNLOC 	; update b$CURSOR and display user cursor

VWPT_XIT:			
cEnd				

VPFCE:
	JMP	B$ERR_FC	;function call error

	externNP B$PCOPYS	

;***
;B$PCPY - PCOPY STATEMENT
;
;Entry:
;	FromPage - 'FROM' PAGE
;	ToPage	 - 'TO' PAGE
;Exit:
;	None.
;Uses:
;	Per convention.
;Exceptions:
;	B$ERR_AFE,B$ERR_FC
;	Illegal Function Call Error in case of any error if PCOPY is supported
;	Else Advanced Function Error is issued
;
;****
cProc	B$PCPY,<PUBLIC,FAR>	
parmW	FromPage		
parmW	ToPage			
cBegin				

;No range checks are necessary - $PCOPY does it
	MOV	AX,FromPage	; AX = source page number
	MOV	CX,ToPage	; AX = destination page number
	CALL	B$PCOPYS 	;Do the move
	JNC	OKRET		; everything ok so return
				; carry is set so something is wrong
	JMP	B$ERR_FC	; Illegal function call
				;(never returns)
OKRET:
cEnd				

sEnd	GR_TEXT 		
	END
