	TITLE	CIRCLE - iAPX 88/86 CIRCLE STATEMENT SUPPORT
;***
; CIRCLE - iAPX 88/86 CIRCLE STATEMENT SUPPORT
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - CIRCLE Statement:
;
;      CIRCLE (x,y),r [,color [,start,end [,aspect]]]
;	 |	|
;	 |	|
;	 |    Coord routines B$CSTT B$CSTO B$CASP B$CIRC
;	 |						 |
;	 +-----------------------------------------------+
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions
	INCLUDE const.inc	;constant definitions

	USESEG	CONST		
	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	GR_TEXT 	

	INCLUDE seg.inc 	; Segment definitions
	INCLUDE idmac.inc	

sBegin	_BSS			
;
;****************************************************************************
; External low-level function vectors
;****************************************************************************
;
	externW b$MapXYC	
	externW b$SetPixFirstC	
	externW b$SetPixC	
	externW b$SetPixLastC	

	externW B$A_END		;defined in GWDATA.ASM
	externW B$A_START	;defined in GWDATA.ASM
	externW b$ASPECTR	;defined in GWDATA.ASM
	externW B$CSTCNT 	;defined in GWDATA.ASM
	externW B$CENCNT 	;defined in GWDATA.ASM
	externW B$CRCSUM 	;defined in GWDATA.ASM
	externB B$CPLOTF 	;defined in GWDATA.ASM
	externB B$CLINEF 	;defined in GWDATA.ASM
	externW B$CNPNTS 	;defined in GWDATA.ASM
	externW B$CPCNT		;defined in GWDATA.ASM
	externW B$CPCNT8 	;defined in GWDATA.ASM
	externB B$COPTFL 	;defined in GWDATA.ASM
	externW B$GX_OLD 	;defined in GWDATA.ASM
	externW B$GXPOS		;defined in GWDATA.ASM
	externW B$GRPACX 	;defined in GWDATA.ASM
	externW B$GRPACY 	;defined in GWDATA.ASM

	externQ B$LXDIF		; defined in GRFPINIT.ASM
	externB B$WNDWSW 	;defined in GWDATA.ASM


	externW B$VXMIN		;defined in GWDATA.ASM
	externW B$VYMIN		;defined in GWDATA.ASM
	externW B$VXMAX		;defined in GWDATA.ASM
	externW B$VYMAX		;defined in GWDATA.ASM
	externW B$CXOFF		;defined in GWDATA.ASM
	externW B$CYOFF		;defined in GWDATA.ASM

sEnd	_BSS			

sBegin	_DATA			

	externB B$CLIPF

sEnd	_DATA			

sBegin	CONST			

	externD b$FP_1 	; s.p. constant   1.0
	externD b$FP_256	; s.p. constant 256.0

	.8087			
FP_4OPI 	DD	1.273239545 ; 4/PI
FP_3PIO2	DD	4.71238898  ; 3*PI/2
FP_2PI		DD	6.283185306 ; 2*PI
FP_SQR2O2	DD	0.707106781 ; SQR(2)/2

sEnd	CONST			

	externFP B$fcomp	
	externFP B$fcompz	
	externFP B$fcompp	
	externFP B$FIX8		
	externFP B$COS4 	
	externFP B$SIN4 	

assumes CS,GR_TEXT		
sBegin	GR_TEXT 		


	externNP B$CLRATR


	externNP B$GetAspect	

	externNP B$INVIEW
	externNP B$CLINE2

	externNP B$ERR_FC
	externNP B$fmlds	
	externNP B$fmldw	
	externNP B$ftolrnd	
	externNP B$COORD1	
	externNP B$SCINIT	; Performs screen initialization
	externNP B$ERR_OV	;overflow error


	SUBTTL	B$CSTT - process optional start angle for CIRCLE statement
	PAGE
;***
;B$CSTT - process optional start angle for CIRCLE statement
;Purpose:
;	Process optional start angle for CIRCLE by saving it for later
;	in B$A_START.  Set bit 2 in flag variable B$COPTFL to
;	1 to indicate that a start angle was specified.
;Entry:
;	sAngle	= S.P. start angle in radians
;Exit:
;	B$A_START contains start angle
;Uses:
;	Per Convention
;****
cProc	B$CSTT,<PUBLIC,FAR>	
parmD	sAngle			
cBegin				
	PUSH	DI		;protect registers
	MOV	AL,4		;note start angle spec flag
	MOV	DI,OFFSET DGROUP:B$A_START ;POINT DI AT B$A_START
	JMP	SHORT CIR_SAVE	;save start angle and return
cEnd	<nogen> 		

	PAGE
	SUBTTL	B$CSTO -  process optional end angle for CIRCLE statement
;***
;B$CSTO -  process optional end angle for CIRCLE statement
;Purpose:
;	Process optional end angle for CIRCLE by saving it for later
;	in B$A_END.  Set bit 1 in flag variable B$COPTFL to
;	1 to indicate that an end angle was specified.
;Entry:
;	sAngle = S.P. end angle in radians
;Exit:
;	B$A_START contains end angle
;Uses:
;	Per Convention
;****
cProc	B$CSTO,<PUBLIC,FAR>	
parmD	sAngle			
cBegin				
	PUSH	DI
	MOV	AL,2		;note end angle spec flag
	MOV	DI,OFFSET DGROUP:B$A_END ;POINT DI AT B$A_END

CIR_SAVE:
	PUSH	ES		
	PUSH	DS		
	POP	ES		;Set ES = DS
	OR	B$COPTFL,AL	;set appropriate flag
	PUSH	SI
	LEA	SI,sAngle	;INIT SOURCE REGISTER
	CLD			;CLEAR DIRECTION FLAG
	MOVSW
	MOVSW			;save specified parameter
	POP	SI
	POP	ES		
	POP	DI
cEnd				


	SUBTTL	B$CASP - process optional aspect ratio for CIRCLE statement
	PAGE
;***
;B$CASP - process optional aspect ratio for CIRCLE statement
;
;Purpose:
; Process optional aspect ratio for CIRCLE.  Bit 3 in B$COPTFL is set to 1 to
; indicate non-default aspect ratio. Compare aspect ratio to 1. If greater than
; one,then set bit 0 in B$COPTFL to 1 to indicate that x axis must be scaled
; instead of y axis, and invert the aspect ratio (an aspect ratio greater than
; 1 is repre- sented as 256/n but is inverted to n/256 after setting flag bit
; in B$COPTFL). Multiply aspect ratio by 256 (i.e.,store numerator n only of
; aspect ratio (n/256)) and save as integer in b$ASPECTR.
;
;Entry:
;	aRatio = S.P. aspect ratio
;Exit:
;	None
;Uses:
;	Per convention
;****
cProc	B$CASP,<PUBLIC,FAR>,ES	
parmD	aRatio			
cBegin				

	OR	B$COPTFL,8	;FLAG NON-DEFAULT ASPECT RATIO


	FLD	aRatio		; ST0 = aspect ratio
	FLD	b$FP_1		; ST0 = 1.0, ST1 = aspect ratio
	CALL	B$fcomp 	; compare
	JNC	CIRC11		; brif aspect ratio already < 1.0
	OR	B$COPTFL,1	; Set scaling x flag
	FDIVR	b$FP_1		; ratio = 1.0/ratio
				; MAKE NUMBER FRACTION OF 256
CIRC11: 			
	FMUL	b$FP_256	; ratio = 256.0 * ratio


	CALL	B$ftolrnd	;round, pop off numeric stack, result in DX:AX
;
; This odd slice of code monkeys with negative aspect ratios so that we get the
; same results as produced by our interpreters (IBM and GW) when the specified
; aspect ratio is negative(!). At this point, BX contains an integer value (256
; * aspect ratio). Any value which has a low byte of zero and a non-zero high
; byte is mapped to 256 (aspect ratio of 1); otherwise the high byte is forced
; to zero.
;
	OR	AL,AL		;is low byte zero?
	JNZ	ZSTOASP 	;low byte is nonzero
				;so zero out high byte and store
	OR	AH,AH		;is high byte also zero?
	JZ	ZSTOASP 	;zero out high byte and store
	MOV	AH,1		;force aspect ratio to 1
	JMP	SHORT STOASP	;store the aspect ratio
ZSTOASP:
	XOR	AH,AH		;zero out high byte of aspect
STOASP:
	MOV	b$ASPECTR,AX	 ;SAVE ASPECT RATIO
cEnd				

	SUBTTL	B$CIRC - process the CIRCLE statement
	PAGE
;***
;B$CIRC, $CI0	   process the CIRCLE statement
;
;Purpose:
;	Draw the circle at the specified center coordinates with the
;	specified radius and of the specified color.  Arcs may be
;	drawn by specifying the start and end angles; segments may
;	be drawn by specifying these angles as negative.  The circle
;	may be scaled by specifying the aspect ratio.
;
;  flag bytes:
;
;  B$CLINEF:		   circle line to center flag
;	  lo bit (bit 0) set means draw line to ctr from start angle
;	  bit 1 set means line to ctr from start angle already drawn (bug fix)
;	  hi bit (bit 7) set means draw line to ctr from end angle
;	  bit 6 set means line to ctr from end angle already drawn (bug fix)
;
;  B$COPTFL:		   circle option flag
;	  bit 0 set means scale x axis by aspect ratio
;	  bit 0 clear means scale y axis by aspect ratio
;	  bit 1 set means end angle specified
;	  bit 2 set means start angle specified
;	  bit 3 set means aspect specified
;	  bits 4-7 unused
;
;  B$CPLOTF:  indicates which points to plot if start and end angles specified
;
;
;Entry:
;	Radius	= radius (spexp)
;	Color	= color specification
;
;Exit:
;	graphics accumulators set at center
;Uses:
;	Per convention
;****
CRCERR:
	JMP	B$ERR_FC	;ALLOW ONLY POSITIVE NOS

CRCOV:				
	JMP	B$ERR_OV	;jump to overflow error

cProc	B$CIRC,<PUBLIC,FAR>,<ES,DI,SI> 
parmD	Radius			
parmW	Color			
cBegin				

	CALL	B$SCINIT	; init screen if not already done
	cCall	B$COORD1	;Convert points
	LEA	BX,Radius	; Get ptr to radius
	MOV	AX,Color	; Get specified Color

;	Give error if color parm < -1.	We should be giving an error
;	for any negative color value, but -1 is what is passed if the
;	user defaulted the color parm.

	CMP	AX,-1		; is color < -1 ?
	JL	CRCERR		; yes, give IFC

	CALL	OLD_CIR 	; Perform circle function
	MOV	B$COPTFL,AL	; Reset options.
cEnd				



OLD_CIR:			
	MOV	CX,BX
	PUSH	AX		;SAVE COLOR ATTRIBUTE

	MOV	AH,[BX+3]	;get sign byte
	OR	AH,AH		;IS RADIUS POSITIVE?
	JS	CRCERR		;NO: issue error message
	FLD	DWORD PTR [BX]	; ST0 = value ref'd by bx

	CMP	B$WNDWSW,0	
	JZ	CIRCL1		; Scale only if Window active
	FMUL	[B$LXDIF]	
CIRCL1:
	CALL	B$ftolrnd	;round & pop ST0, convert to integer in DX:AX
	OR	DX,DX		; test if over 64K
	JNZ	CRCOV		; if so, then overflow error
	OR	AX,AX		; test if over 32K
	JS	CRCOV		; if so, then overflow error
	MOV	B$GX_OLD,AX	;SAVE RADIUS

	FILD	B$GX_OLD 	; load onto numeric stack (ST0)


	FMUL	FP_SQR2O2	; [ST0] = radius * SQR(2)/2 = # Pt.s to plot

	CALL	B$ftolrnd	;convert to integer in AX (pop from num. stack)
	MOV	B$CNPNTS,AX	;B$CNPNTS=RADIUS*SQR(2)/2=# PTS TO PLOT

	TEST	B$COPTFL,8	;Is aspect ratio defaulted?
	JNZ	NDFASP		;NO: don't get default aspect ratio

	CALL	B$GetAspect	;Yes: get default aspect ratio
;GetAspect returns BX=256*aspect ratio and DX=256/aspect ratio
	OR	BH,BH		;Is aspect ratio > 1?
	JZ	SAVASP		;NO: scale y and save BX in b$ASPECTR
	OR	B$COPTFL,1	;Set scaling x flag
	XCHG	BX,DX		;Scaling x, using 1/aspect ratio
SAVASP: 			;Save aspect ratio
	MOV	b$ASPECTR,BX	;b$ASPECTR:=BX
NDFASP: 			;Non default aspect ratio
	MOV	CX,B$GRPACX	;Get coords saved by $CI0
	MOV	DX,B$GRPACY
	POP	AX		;RESTORE COLOR ATTRIBUTE
	CALL	B$CLRATR 	;SET COLOR ATTRIBUTE OF CIRCLE
	PUSH	B$GX_OLD 	;B$GX_OLD CONTAINS RADIUS
	POP	B$GXPOS		;B$GXPOS:=RADIUS
	XOR	BX,BX		;BX:=0 (DEFAULT START ANGLE)
	MOV	B$CLINEF,BL	;CLEAR B$CLINEF

;If optional circle generation is supported (OGCircle = 1) but the
;routine does not support clipping (OGCircleClip = 0), then software
;circle generation must be used when the aspect ratio is specified or
;when clipping is required.

	MOV	B$CLIPF,BL	;Assume clipping not required
				;B$ClipCheck always called now
	CALL	B$ClipCheck	;check if clipping will occur
;B$CLIPF = FF means clipping required
;B$CLIPF = 0 means clipping not required


	XOR	AX,AX		;CLEAR FLOATING COUNT
	XOR	CX,CX		;CLEAR FLOATING COUNT
	XOR	DX,DX		;CLEAR INTEGER COUNT
ANGLES: TEST	B$COPTFL,4	;START ANGLE SPECIFIED?
	JZ	NOSTRT		;NO: USE DEFAULT COUNTS

	FLD	DWORD PTR B$A_START 

	MOV	CL,1		;LO BIT SET FOR LINE TO CNTR
	CALL	ANGLE_CALC	;GO DO ANGLE CALCULATIONS - result in AX
	XCHG	AX,DX		;put start result in DX


NOSTRT:


	TEST	B$COPTFL,2	;END ANGLE SPECIFIED?
	JZ	NOEND		;NO: USE DEFAULT COUNT

	PUSH	DX		;SAVE INTEGER COUNT
	MOV	DX,-1		;DEFAULT END COUNT = INFINITY
OGCircle2:			

	FLD	DWORD PTR B$A_END 

	JMP	SHORT ENDFND	;Use user specified end angle
NOEND:				;Provide end angle default
	mov	ax,-1		;integer end count = infinity
	mov	bl,0		;polarity is always counterclockwise
	JMP	SHORT cstplt	;go plot the circle
ENDFND:
	MOV	CL,128		;SET HI BIT IN B$CLINEF FOR LINE TO CNTR
	CALL	ANGLE_CALC	;GO DO ANGLE CALCULATIONS - result in AX



	;AX = integer end count
	POP	DX		;Restore integer start count
	CMP	AX,DX
	MOV	BL,0
	JAE	CSTPLT		;PLOT POINTS BETWEEN STRT & END if end >= start

	MOV	BL,B$CLINEF	;fetch center line flag byte
	OR	BL,BL		;was either flag set ?
	JZ	CIRREV		;no, just reverse polarity
	XOR	BL,129		;yes, were they both set ?
	JZ	CIRREV		;yes, just reverse polarity
	MOV	B$CLINEF,BL	;no, reverse center line flags also
CIRREV:
	MOV	BL,-1		;PLOT POINTS ABOVE & BELOW
	XCHG	DX,AX		;SWAP START AND END SO START .LT. END
CSTPLT:
	MOV	B$CPLOTF,BL	;SET UP PLOT POLARITY FLAG
	MOV	B$CSTCNT,DX	;STORE START COUNT
	MOV	B$CENCNT,AX	;AND END COUNT
	CMP	AX,DX		;Does START .EQ. END?
	JNE	CIRCPL		;No: GO PLOT CIRCLE
	MOV	AL,B$CLINEF	;Yes: check for special case
	TEST	AL,129		;   if either center line flag set then
	JZ	CIRCPL		;(BOTH FLAGS ALREADY SET)
	OR	AL,129		;set the BOTH flags so order of checks
	MOV	B$CLINEF,AL	;in CPLOT4 won't miss a center line needed
CIRCPL: 			;PLOT THE CIRCLE
	JMP	B$DrawCircle	;NOW DRAW THE CIRCLE


;***
;B$ClipCheck
;
;Purpose:
;	Detect if the circle must be clipped.
;
;Entry:
;	([B$VXMIN],[B$VYMIN]) = left upper window boundary
;	([B$VXMAX],[B$VYMAX]) = right lower window boundary
;	[b$ASPECTR] = integer aspect ratio
;	[B$COPTFL] = circle option flag
;	  bit 0 set means scale x axis by aspect ratio
;	  bit 0 clear means scale y axis by aspect ratio
;	[B$GRPACX] = center X
;	[B$GRPACY] = center Y
;	[B$GXPOS] = radius
;
;Exit:
;      [B$CLIPF] = FF indicates clipping will be required.
;
;Modifies:
;	none
;****

cProc	B$ClipCheck,<NEAR>,<AX,CX,DX>	
cBegin
	mov	dx,B$GXPOS	;move radius to dx for cplscx
	call	cplscx
	MOV	AX,B$GRPACX	;Get the X center
	ADD	AX,CX		;Radius
	SUB	AX,B$VXMAX	;Test for off to the right
	NEG	AX
	JS	CLPCLP		;Clipping required
	MOV	AX,B$GRPACX
	SUB	AX,CX		;Radius
	SUB	AX,B$VXMIN	;Test for off to the left
	JS	CLPCLP		;Clipping required
	MOV	AX,B$GRPACY	;Now check for the Y direction
	ADD	ax,dx		
	SUB	AX,B$VYMAX	;Test for off the bottom
	NEG	AX
	JS	CLPCLP		;Clipping required
	MOV	AX,B$GRPACY
	sub	ax,dx		
	SUB	AX,B$VYMIN	;Test for off the top

CLPCLP:
	RCL	AX,1		;Rotate sign bit to PSW.C
	SBB	AL,AL		;Map PSW.C to 0 or FF
	MOV	B$CLIPF,AL	;Store the new flag
cEnd


;*** 
; ANGLE_CALC
;
;Purpose:
;
;Entry:
; [ST0] = angle
; [CL]	= LO or HI bit set for line to cntr
;
;Exit:
; [AX]	= result of calculation
;
;Uses:
;
;Preserves: (optional)
;
;Exceptions:
;
;******************************************************************************
;
; Note:  This routine may exit by jumping to CGETXY
;

DbPub ANGLE_CALC
cProc ANGLE_CALC,<NEAR>
cBegin

;
; check sign of angle - make positive if not
;
	PUSH	CX		
	CALL	B$fcompz	; see if zero
	POP	CX		; restore input
	JZ	CHKCONT 	; brif angle = 0.0
	JC	CHKCONT 	; brif angle > 0.0

	MOV	BX,OFFSET DGROUP:B$CLINEF ; SET BIT IN [C] IN B$CLINEF
	OR	[BX],CL 	
	FCHS			; change sign of angle to positive
CHKCONT:			

	FLD	FP_2PI		; [ST0] = 2*PI
	CALL	B$fcomp	; Ensure angle within range
	JC	AngleBad	; Jump if > 2*PI


	FLD	ST(0)		; duplicate input angle on numeric stack
	FMUL	FP_4OPI 	; [ST0] = 4/PI * input angle
	CALL	B$FIX8		; MUST be truncated towards zero
	CALL	B$ftolrnd	; AX = trunc(octant(angle))  (0-7)
	INC	AX		; convert to 1-8
	SHR	AL,1		; AL = octant/2
	RCR	AL,1		; set up flags
	PUSHF			; PSW.V: 1=+,0=-; PSW.C: 1=COS,0=SIN
	RCL	AL,1		; restore octant/2, then multiply by 2:
	SHL	AL,1		; 0,2,2,4,4,6,6,8 = octant base index
	MUL	WORD PTR [B$CNPNTS] ; AX = (2 * INT((OCT+1)/2)) * CNPNTS
	POPF			; get flags: PSW.C: 1=COS, 0=SIN
	PUSH	AX		; save scaled count base
	PUSHF			; save PSW.V
	JC	UseCOS		; brif PSW.C = 1, use COSINE
	CALL	B$SIN4		; [ST0] = SIN(input angle)
	JMP	SHORT FixSign	; skip the COS stuff
UseCOS:				
	CALL	B$COS4		; [ST0] = COS(input angle)
FixSign:			
	FABS			; [ST0]=ABS(f(angle)), where f = SIN or COS
	POPF			; retrieve PSW.V
	JO	SignOK		; brif PSW.V set, otherwise simulate SUB
	FCHS			; NEGATE [ST0]
SignOK:				
	FIMUL	WORD PTR [B$GXPOS] ; [ST0] = Radius * ABS(f(angle)
	MOV	BX,SP		; [BX] points to scaled count base
	FIADD	WORD PTR [BX]	; Add octant base value for point count
	CALL	B$ftolrnd	; put integer equivalent in AX
	POP	BX		; remove count base from stack


cEnd				

AngleBad:
	JMP	B$ERR_FC	;angle too big




;
cProc	CPLSCX,<NEAR>		
cBegin				
	mov	cx,dx		;save unscaled value
	mov	ax,[b$ASPECTR]	;get 256*aspect ratio
	mul	dx		;dx:ax = dx * aspect *256
	add	ax,128D 	
	ADC	DH,DL		;(DH was 0 after mul)
	mov	dl,ah		;result is in dx
tstxchg:
	TEST	B$COPTFL,1	;SEE WHETHER ASPECT WAS .GT. 1
	JZ	CPLRET		;DON'T SWAP IF ZERO
	XCHG	cx,DX		;Greater, use 1/Aspect
CPLRET:
cEnd				

;***
;
;	CIRCLE ALGORITHM
;
;   [SI] = X = RADIUS
;   [DI] = Y = 0
;   SUM  = 1 - RADIUS
;   LOOP
;	plot 8 reflections of the point around the circle's center
;	IF Y >= X THEN
;	    EXIT LOOP
;	IF SUM > 0
;	    SUM = SUM - 2*X + 2
;	    X = X - 1
;	SUM = SUM + 2*Y + 3
;	Y = Y + 1
;
; 26-Jan-87 Major revision for speed
;
;****

cProc	B$DrawCircle,<NEAR>,<DI,SI,ES>   
cBegin
	CALL	[b$SetPixFirstC] ;low-level circle initialization
	XOR	DI,DI		;INIT Y = 0
	MOV	SI,B$GXPOS	;X = RADIUS
	MOV	AX,1		;SUM = 1-RADIUS
	SUB	AX,SI		
	MOV	B$CRCSUM,AX	
CIRCLP: 			
	PUSH	DI		;save X,Y
	PUSH	SI		
	CALL	CPLOT8		;reflect the point 8 ways
	POP	SI		;restore X,Y
	POP	DI		
	CMP	DI,SI		;finished when Y >= X
	JAE	CRCLPX		;go if done
	MOV	AX,B$CRCSUM	
	OR	AX,AX		;check sum for whether to move X too
	JS	CNODEX		;go, no X change this time, only Y
	SUB	AX,SI		;SUM = SUM - 2*X + 2
	SUB	AX,SI		
	ADD	AX,2		
	DEC	SI		;X = X - 1
CNODEX: 			;always move Y
	ADD	AX,DI		;SUM = SUM + 2*Y + 3
	ADD	AX,DI		
	ADD	AX,3		
	INC	DI		;Y = Y + 1
	MOV	B$CRCSUM,AX	;update sum
	JMP	SHORT CIRCLP	;loop

CRCLPX: 			;circle finished
	CALL	[b$SetPixLastC] ;low-level circle termination
cEnd				

TestClip:			;check point for clipping
	CALL	B$INVIEW 	;See if point outside of viewport
	JNC	CPLFIN		;go if pt not visible (carry reset)
	JMP	SHORT NoClip	;go if pt visible (carry set)

;
; REFLECT THE POINTS AROUND CENTER
; [SI] = X OFFSET FROM CENTER, [DI] = Y OFFSET FROM CENTER
;
cProc	CPLOT8,<NEAR>		
cBegin				
	MOV	B$CPCNT,DI	;octant point count is Y
	MOV	B$CPCNT8,0	;circle point count is 0

	MOV	AX,b$ASPECTR	;BX = round(Y * (aspect*256) / 256)
	MOV	CX,AX		
	MUL	DI		
	ADD	AX,128D 	
	ADC	DH,DL		;(DH was 0 after mul)
	MOV	DL,AH		
	MOV	BX,DX		

	MOV	AX,CX		;DX = round(X * (aspect*256) / 256)
	MUL	SI		
	ADD	AX,128D 	
	ADC	DH,DL		;(DH was 0 after mul)
	MOV	DL,AH		

	TEST	B$COPTFL,1	;which axis is being scaled?
	JNZ	NoSwap		;go if scaling X
	XCHG	BX,DI		;swap scaled/unscaled values
	XCHG	DX,SI		
NoSwap: 			
	MOV	B$CXOFF,BX	; B$CXOFF = scaled(unscaled) Y
				; DI     = unscaled(scaled) Y
	MOV	B$CYOFF,SI	; B$CYOFF = unscaled(scaled) X
	MOV	SI,DX		; SI     = scaled(unscaled) X

	NEG	DI		;negate Y to start
	CALL	CPLOT4		;plot (X,-SY) (-Y,-SX) (-X,SY) (Y,-SX)

	MOV	AX,B$CNPNTS	;odd octant counts start at end of octant
	MOV	B$CPCNT8,AX	
	SUB	AX,B$CPCNT	;later calculate point count in odd octant
	MOV	B$CPCNT,AX	;  as PNTCNT = PNTS/OCT - PNTCNT
	NEG	B$CXOFF		;negate B$CXOFF and Y to start
	NEG	DI		
cEnd	<nogen> 		

	; fall through to plot (Y,-SX) (-X,-SY) (-Y,SX) (X,SY)

;

cProc	CPLOT4,<NEAR>		
cBegin				
	MOV	AX,4		;plotting four points
CPLOT:
	PUSH	AX		;save count
	MOV	CX,SI		;get X,Y from saved values
	MOV	DX,DI		
	ADD	CX,B$GRPACX	;actual points are offset from
	ADD	DX,B$GRPACY	;  the circle's center
	test	B$COPTFL,6	;start or end angles specified?
	JNZ	TestAng 	;go if so
CPLTIT:
	CMP	B$CLIPF,0	;is clipping required?
	JNZ	TestClip	;go if so to check point
NoClip: 			;point gets plotted here
	CALL	[b$MapXYC]	;convert X,Y to address and mask
	CALL	[b$SetPixC]	;plot it
CPLFIN:
	POP	AX		;restore count
	DEC	AX		;finished?
	JZ	CIRRET		;go if so
	XCHG	SI,B$CXOFF	;swap values for next octant
	NEG	SI		
	XCHG	DI,B$CYOFF	
	NEG	DI		
	JMP	SHORT CPLOT	;loop for four octants/points
CIRRET:
cEnd				

TestAng:			;check point against start/end angles
	MOV	BX,B$CPCNT8	;current octant point count
	MOV	AX,B$CNPNTS	;update for next octant/point
	SAL	AX,1		
	ADD	AX,BX		
	MOV	B$CPCNT8,AX	
	ADD	BX,B$CPCNT	;BX = current point's count
	CMP	BX,B$CSTCNT	;at start point?
	JZ	CLINSC		;go if so
	JB	CNBTWN		;if less then point is not between
	CMP	BX,B$CENCNT	;at end point?
	JZ	CLINEC		;go if so
	JB	CBTWEN		;if less then point is between
CNBTWN:
	CMP	B$CPLOTF,0	;SEE WHETHER TO PLOT OR NOT
	JNZ	CPLTIT		;NEED TO PLOT NOT-BETWEEN POINTS
	JMP	SHORT CPLFIN	;DON'T PLOT - FIX UP STACK & RETURN
CLINEC:
	MOV	al,B$CLINEF	;get ptr to center line flag byte
	test	al,128
	JZ	CPLTIT		;NO LINE REQUIRED - JUST PLOT POINT
	test	al,64
	jnz	CPLFIN		;Don't draw line or point on arc.
	or	[B$CLINEF],64	;don't plot the line twice
	jmp	short CLINE	;plot the line from end angle to ctr

CLINSC:
	MOV	al,B$CLINEF	;get ptr to center line flag byte
	test	al,1
	JZ	CPLTIT		;NO LINE REQUIRED - JUST PLOT POINT
	test	al,2
	jnz	CPLFIN		;Don't draw line or point on arc.
	or	[B$CLINEF],2	;don't plot the line twice
CLINE:
	PUSH	SI		;save X,Y
	PUSH	DI		
	CALL	B$CLINE2 	;DRAW LINE FROM [CX],[DX] TO CENTER
	CALL	[b$SetPixFirstC] ;re-initialize for circle (EGA)
	POP	DI		;restore X,Y
	POP	SI		
GOCPLFIN:			
	JMP	SHORT CPLFIN

CBTWEN:
	CMP	B$CPLOTF,0	;SEE WHETHER PLOTTING BETWEENS OR NOT
	JNZ	GOCPLFIN	;relative jump out of range
	JMP	CPLTIT		;IF Z, THEN DOING BETWEENS


sEnd	GR_TEXT 		
	END
