	TITLE	RANDOM - Random number generator and RANDOMIZE
	page	,132
;***
; RANDOM - Random number generator and RANDOMIZE
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - RANDOMIZE Statement - calls B$RNZ1 if parm
;
;    Example:
;
;	RANDOMIZE 1
;	-----------
;	    |
;	  B$RNZ1
;
;
; - RND Function - calls B$RND0 if no parm, B$RND1 otherwise:
;
;      v = RND[(x)]
;
;    Examples:
;
;      RND			RND(1)
;      ---			------
;	|			   |
;    B$RND0			 B$RND1
;
;
; Algorithm:
;
; We use the "linear congruential" method for random numnber generation. The
; formula is:
;
;	x1 = (x0 * a + c) mod 2^24
;
; where
;
;	x1 = is a new random number in the range [0..1^24]
;	x0 = the previous random number (or the seed, for the first one)
;	a  = 214,013
;	c  = 2,531,011
;
; The RND function returns a floating point number:
;
;	x1 / (2^24)
;
; which changes the range to [0..1].
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

;Code segments			
	useSeg	INIT_CODE	
	useSeg	MT_TEXT 	
	useSeg	DV_TEXT 	
;Data segments			
	useSeg	CONST		
	useSeg	_DATA		
	useSeg	<XIB>		; XIB and XIE must bracket XI!
	useSeg	<XI>		; initializer segment
	useSeg	<XIE>		

	INCLUDE seg.inc 	
	INCLUDE compvect.inc	

	INITIALIZER B$xMTINI	;put B$xMTINI in initializer list
	INCLUDE messages.inc	; message symbols


	SUBTTL	Data definitions
	page			

sBegin	CONST			

	staticW RndA,<43FDH,0003H>	;[4] Double word -  343FDH =  214013D
	staticW RndC,<9EC3H,0026H>	;[4] Double word - 269EC3H = 2531011D
	.8087			
	staticD FP_2T24,16777216.0 

sEnd	;CONST			

sBegin	_DATA			
	externD b$RndVar	; Random number seed
	externW B$AC		
	externW b$run_disp	; Runtime initialization dispatch table
sEnd	;_DATA			

	SUBTTL	code externals
	page

sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 			



	assumes CS,INIT_CODE	
sBegin	INIT_CODE		

;***
;B$xMTINI - Math initializer
;PLM B$xMTINI()
;
;Purpose:
;	Initializer for Math component.  This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the Math routines.  This
;	insures that the only time that Math is accessed is when
;	this module is linked into the user program.
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
cProc	B$xMTINI,<FAR>
cBegin
;
;	update "RUN" time initialization dispatch address to B$MTRUNINI
;
	MOV	WORD PTR [b$run_disp].MT_RVEC,MT_TEXTOFFSET B$MTRUNINI
cEnd

sEnd	INIT_CODE		

	SUBTTL	Math RUNtime interpreter initialization
	PAGE
	assumes CS,MT_TEXT	
sBegin	MT_TEXT 		

;***
;B$MTRUNINI - Do RUN time initialization for Math.
;void pascal B$MTRUNINI()
;
;Purpose:
;	Initializes random number seed.  This routine
;	routine gets called during initialization and during RUN.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$MTRUNINI,<PUBLIC,NEAR>
cBegin				
	MOV	[WORD PTR b$RndVar],0	 ;re-init random number
	MOV	[WORD PTR b$RndVar+2],5  ;seed
cEnd				

	SUBTTL	RND interfaces -- B$RND0 & B$RND0
	page
;*** [2]
;B$RND1, B$RND0 - RND function
;R4 B$RND1(R4 RndParm)
;R4 B$RND0(void)
;
;Purpose:
;	Both B$RND1 & B$RND0 return a random number.	If the argument given
;	by the user is positive (or B$RND0), this routine returns the next
;	random in sequence.  If the argument is zero, it returns the same as
;	the previous one.  If the argument is negative, it starts a new pseudo
;	random sequence.
;Entry:
;	None if B$RND0.
;	R4 RndParm on stack if B$RND1.
;Exit:
;	[AX]	= *R4 random number in [0,1)
;Exceptions:
;
;*******************************************************************************

cProc	B$RND0,<PUBLIC,FAR,FORCEFRAME> 
cBegin				
	cCall	GetNextRnd	; on return, AX=*Rndnum
cEnd				; exit

	page

cProc	B$RND1,<PUBLIC,FAR>	
	ParmD	RndParm 	;[2] R4
cBegin				; set up the stack frame
	MOV	AX,SEG_RndParm	; fetch high word of R4 parm
	OR	AX,AX		; Microsoft IEEE: if high word of s.p. is 0,
				;	the number is 0
	JZ	SameNum 	;input is 0; return same result as last time
	JNS	NewRnd		; Brif input positive, give next in sequence

	MOV	DX,OFF_RndParm	; fetch low	word of R4 parm
	ADD	DL,AH		;[4] fold m.s.b. into l.s.b.
	ADC	DH,0
	ADC	AL,0
	XOR	AH,AH		; new 24-bit integer seed in AX:DX
	MOV	[WORD PTR b$RndVar],DX	 
	MOV	[WORD PTR b$RndVar+2],AX 
NewRnd: 			
	cCall	GetNextRnd	; on return AX=*RndNum
	JMP	SHORT RndExit	
SameNum:			
	cCall	MakeFloat	; on return AX=*RndNum
RndExit:			
cEnd				; clean stack and exit to caller

	SUBTTL	RND supporting routine -- GetNextRnd & MakeFloat
	page
;***
;GetNextRnd -- get next random number
;MakeFloat -- make the number in [b$RndVar] into a R4
;
;Purpose:
;	Get next random number in sequence.
;Entry:
;	[b$RndVar] has the seed.
;Exit:
;	[AX]	= *B$AC which contains the R4 result
;Exceptions:
;	none
;*******************************************************************************

cProc	GetNextRnd,<NEAR>	

cBegin				
	PUSH	DI		
	MOV	AX,[WORD PTR b$RndVar]	 ;low half of previous number
	MOV	CX,[RndA]	;low half of A
	MUL	CX
	XCHG	AX,DI		;save low half in DI
	MOV	BX,DX		;  high half in BX
	MOV	AX,[WORD PTR b$RndVar+2] ;high half of previous
	MUL	CX
	ADD	BX,AX		;sum partial products
	MOV	AX,[RndA]
	MUL	[WORD PTR b$RndVar]	 
	ADD	BX,AX		;last partial product (since we're mod 2^24)
	ADD	DI,[RndC]	;add in constant C
	ADC	BL,BYTE PTR [RndC]
	XOR	BH,BH		;extended 24-bit number to 32 bits for NORM
	MOV	DX,DI		;number in BX:DX
	MOV	[WORD PTR b$RndVar],DX	 ;save for next time
	MOV	[WORD PTR b$RndVar+2],BX 
	POP	DI		
MakeFloat:			

	FILD	b$RndVar	; put 24-bit integer on numeric stack
	FDIV	FP_2T24 	; ST0 = seed/2^24
	MOV	BX,OFFSET DGROUP:B$AC 
	FSTP	DWORD PTR [BX]	; put s.p. equivalent into FAC
	XCHG	AX,BX		; result is *R4 in AX
	FWAIT			; ensure result in RAM prior to return

cEnd				; exit to caller

	SUBTTL	RANDOMIZE interface - B$RNZ1
	page
;***[6]
;B$RNZP - RANDOMIZE statement
;void B$RNZP (R8 SeedNum)
;
;Purpose:
;	The number is set into the middle word of the current random
;	number as the seed for the next one.
;Entry:
;	R8 SeedNum
;Exit:
;	A new seed is created in RndVar, based on the seed value at entry
;	and the least significant 2-words of the input parameter.
;Exceptions:
;	none
;*******************************************************************************

cProc	B$RNZP,<PUBLIC,FAR>	
	ParmQ	SeedNum 	; R8 seed number
cBegin				
	LEA	BX,SeedNum+4	; get MOST significant digits
	MOV	AX,[BX] 	; get word of D.P. number
	XOR	AX,[BX+2]	; xor with the next word
	MOV	[WORD PTR b$RndVar+1],AX ; replace middle word of current s.p. seed
				;	with this value - - now we're reseeded.
cEnd				; exit


sEnd	;MT_TEXT		
	END
