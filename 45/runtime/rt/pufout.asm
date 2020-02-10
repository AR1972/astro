	TITLE	PUFOUT - PRINT USING output formatter
	page	56,132
;***
; PUFOUT - PRINT USING output formatter
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	

;Code segment
	useSeg	ST_TEXT 	
	useSeg	MT_TEXT 	
;Data segment
	useSeg	_DATA		
	useSeg	_BSS		

	INCLUDE seg.inc 	
	INCLUDE idmac.inc	
	INCLUDE	rtps.inc	

	SUBTTL	local constant definitions
	page

	CURNCY= "$"			;Currency symbol

;Definition of bits in flag byte

	COMMABIT =	40H	;Commas every 3 digits in integer part
	STARBIT =	20H	;Fill with leading "*" instead of blanks
	DOLLARBIT =	10H	;Floating "$" in front of number
	PLUSBIT =	08H	;Force "+" in positive
	TRAILING =	04H	;Put sign at end of number
	SCIBIT	=	01H	;Use scientific notation

	BIGSCI =	01H	; 3 digit sci notation

	SUBTTL	data definitions
	page
				; whole section redifined
sBegin	_DATA
	externW B$AC
sEnd

sBegin	_BSS

	externB b$FOBUF		; defined in CONASC.ASM
	externB b$SIGN 		; defined in CONASC.ASM
	externB b$VTYP		
	externB b$DIGCNT	;defined in PRTU.ASM as word
	DECDIG	EQU	b$DIGCNT	;No. of digits to right of D.P.
	INTDIG	EQU	b$DIGCNT+1	;No. of digits to left of D.P.

	externW b$PUFLG	; defined in PRTU.ASM

sEnd	;_BSS

sBegin	MT_TEXT
	externNP B$FOUTBX	; Integer formating routine
sEnd	MT_TEXT

	SUBTTL	code segment externals
	page
sBegin	ST_TEXT
	EXTRN	B$ASCRND:NEAR	

	externNP B$FloatCONASC	

sEnd

	assumes CS,ST_TEXT
sBegin	ST_TEXT
	SUBTTL	convert the number into a formatted ASCII string
	page

;***[1]
;B$PUFOUT -- convert the number into a formatted ASCII string according to
;		b$PUFLG
;Purpose:
;	PRINT USING output formatter.
;
;	Note: can't assume ES=DS
;Entry:
;	[BX] = *I2, *I4, *R4 or *R8
;	[b$DIGCNT+1] = field width to left of decimal point
;	[b$DIGCNT] = field width to right & including decimal point
;	[b$PUFLG] = Formatting flags (see equates above)
;	[b$VTYP] = Type of number
;Exit:
;	[SI] = Address of first character of formatted output
;	[AX] = Count of characters (not including terminating zero)
;Uses:
;	DI
;Exceptions:
;	none
;*******************************************************************************

cProc	B$PUFOUT,<PUBLIC,NEAR>,<ES>	;save ES
localW	EXP			
localW	RestartEXP		; value of DX from B$CONASC if we need to
				; restart the conversion.
cBegin
	PUSH	DS		
	POP	ES		; can't assume ES=DS, set them equal
	MOV	AL,[b$VTYP]	
	CBW			; convert to word for use as count
	MOV	DI,OFFSET DGROUP:B$AC 
	CMP	AL,VT_I2	
	JZ	PUFOUT_I2	
	AND	AX,0FH		; Number of bytes to move

	ADD	DI,4		;[ ]
	SUB	DI,AX		; AC or DAC
PUFOUT_I2:			
	MOV	SI,BX		
	XCHG	AX,CX		
	SHR	CX,1		; Count of words
	REP	MOVSW		
	CALL	B$CONASC	; Convert to ASCII digits

	MOV	RestartEXP,DX	; save DX if we need to jump to PUFOUT

; Number has been converted to ASCII and is sitting at buffer pointed
;	to by DI.
;	SI = address of result buffer
;	CX = number of significant figures
;	DX = base 10 exponent of the right end of the digit string.

PUFOUT:
DbAssertRel	DX,e,RestartEXP,ST_TEXT,<B$PUFOUT:Exponent not restored>

	XOR	BX,BX		;No position-eating symbols yet
	MOV	AL,BYTE PTR [b$PUFLG]	; Get PRINT USING flags
	TEST	AL,DOLLARBIT	;Need floating dollar sign?
	JZ	NODOL
	INC	BX		;Chalk up a position for the "$"
NODOL:
	CMP	[b$SIGN],"-"	; Is number negative?
	JZ	HAVSGN		;If so, leave sign alone
	TEST	AL,PLUSBIT	;Do we force "+"?
	JZ	NOSGN		;No - leave it a blank
	MOV	[b$SIGN],"+"	
HAVSGN:
	TEST	AL,TRAILING	;Is it in front, eating a digit position?
	JNZ	NOSGN
	INC	BH		;Count one position for the sign
NOSGN:
	TEST	AL,SCIBIT	;Scientific notation?
	JNZ	SCILEN
	ADD	BL,BH		;Sum positions needed for sign and "$"
	MOV	AX,DX		; Power of 10 of digit string
	ADD	AX,CX		; Plus count of digits gives number of digits
				;to left of decimal point
				;This is assuming the number would be printed
				;in the form 0.xxxxx   where xxxx are the digits
				;In actuality, we need to print in the form
				; (INTDIG).(DECDIG-1), possibly with exponents,
				;commas, etc.

	DEC	[DECDIG]	; Number of digits requested to right of d.p.
	JL	ROUND		; Any decimal digits?
	ADD	AL,[DECDIG]	; Add to count of digits needed
	ADC	AH,0		
ROUND:
	INC	[DECDIG]	; Restore
	MOV	DI,OFFSET DGROUP:b$FOBUF ;In case B$ASCRND not called
	OR	AX,AX		
	JL	NOROUND 	;Don't ask for negative digits
	CALL	B$ASCRND	; Round ASCII digits
NOROUND:
	CMP	BYTE PTR [SI],"0" ;Is number zero?
	JNZ	NOTZERO
	MOV	DX,-1		; If so, set exponent to no integer part
NOTZERO:
	MOV	AX,CX		;Number of digits in number now
	ADD	AX,DX		; New count of integer digits

	MOV	DH,AL		;Save it for output loop
	MOV	AH,0
	JG	COMCHK		;If we have integer digits, do comma check
	MOV	AL,0		;No integer digits
	JMP	SHORT GETFILL

SCILEN:
;Compute digit counts and round for scientific notation
	MOV	AL,[INTDIG]
	SUB	AL,BL		;Count a digit position if "$" needed
	ADD	BL,BH		;Sum positions needed for "$" and sign
	TEST	[b$PUFLG],TRAILING ; test if trailing sign
	JNZ	SCI1		;if so, no sign in front
	DEC	AL		;Always take one for the sign
	JNS	SCI1		;If there's room, that is
	XOR	AL,AL
SCI1:
	MOV	BH,AL		;Save count of digits to left of d.p.
	MOV	AL,[DECDIG]
	DEC	AL		;Count one position for d.p. itself
	JNS	SCI2
	XOR	AL,AL
SCI2:
	ADD	AL,BH		;Sum total digits needed
	JG	SCI3		;Need at least one
	INC	AX		;Request one digit
	INC	BH		;And print it to left of d.p.
SCI3:
	CALL	B$ASCRND	; Round to correct number of digits
	CMP	BYTE PTR [SI],"0" ;Is result zero?
	JNZ	FIGEXP		;If not, we're OK - go figure exponent
	XOR	AL,AL		;Set exponent to zero
	OR	BH,BH		;Print digits to left of d.p.?
	JZ	SAVEXP		;If not, that's O.K.
	MOV	BH,1		;If so, print at most 1
FIGEXP:
	MOV	AX,CX
	ADD	AX,DX		; AX = Number of integer digits
	SUB	AL,BH		;Subtract number to be printed left of d.p.
	SBB	AH,0		
SAVEXP:
	MOV	[EXP],AX	;Exponent to be printed later
	MOV	AL,BH		;Set up digit count
	MOV	DH,BH
	JMP	SHORT GETFILL

COMCHK:
	TEST	[b$PUFLG],COMMABIT ; Need commas?
	JZ	GETFILL

;Perform comma computation. We need a comma every 3 digits, so we'll divide
;the number of digits by 3 to see how many we need. Since we don't need the
;first comma until we have 4 digits, the digit count is decremented before
;the division. The remainder of the division represents the number of digits
;to print before the first comma is needed.

	DEC	AX
	MOV	BH,3
	DIV	BH		;AL=number of commas
	ADD	AL,DH		;Add up number of positions needed
	INC	AH		;Adjust delay to first comma to range 1-3

;Let's see if all this stuff will fit. If so, compute amount of extra room
;for leading filler. Otherwise, print a "%".

GETFILL:
	TEST	[b$PUFLG],2	; Already had field overflow?
	JZ	FITCHK
	MOV	BYTE PTR [DI],"%"
	INC	DI
FITCHK:
	ADD	AL,BL		;Add digits needed by "$" and sign
	SUB	AL,[INTDIG]	;Do we have enough room?
	JBE	FILCNT		;AL is negative of amount of extra room
	CMP	AL,4		;Exceeding width by more that 4?
	JBE	OVERFIELD
	OR	[b$PUFLG],SCIBIT+2
				; scientific notation and field overflow

	MOV	DX,RestartEXP	; restore DX to proper value
	JMP	PUFOUT		;Try again
OVERFIELD:
	MOV	AL,"%"		;Didn't fit - print field overflow character
	STOSB
	XOR	AL,AL		;Zero fill count

;Leading-fill field with blanks or "*"s as requested

FILCNT:
	MOV	DL,AH		;Save delay to first comma (zero if no commas)
	JZ	GETCNT		;Any filling necessary?
	OR	DH,DH		;And do we have integer digits to print?
	JG	GETCNT		;If not, we'll add a leading zero
	INC	AL		;Make room by reducing fill count
	INC	CH		;Set leading zero flag
GETCNT:
	NEG	AL		;Make fill count positive
	CBW
	XCHG	AX,CX		;Put count in CX
	XCHG	AX,BX		;Save digit count (from CX) in BX
	MOV	AL," "		;Fill character
	MOV	AH,BYTE PTR [b$PUFLG]	; Get flag byte
	TEST	AH,STARBIT	;Fill with "*" instead?
	JZ	FILL
	MOV	AL,"*"
FILL:
	REP	STOSB		;Leading fill

;Print leading sign, if needed

	TEST	AH,TRAILING	;Leading or trailing sign?
	JNZ	DOLLARCHK
	MOV	AL,[b$SIGN]	; Pick up leading sign
	CMP	AL," "		;If blank, already counted in fill count
	JZ	DOLLARCHK
	STOSB			;Store sign

;Print floating "$" if requested

DOLLARCHK:
	TEST	AH,DOLLARBIT	;Floating "$"
	JZ	NODOLLAR
	MOV	AL,CURNCY
	STOSB
NODOLLAR:

;Copy integer digits, if any

	MOV	CL,DH		;Count of digits to left of d.p.
	NEG	DH		;Count of zeros to fill after d.p.
	JL	NOCOM		;Don't check for comma first time through
	DEC	BH		;Force leading zero?
	JNZ	DPCHK
	MOV	AL,"0"
	STOSB
	JMP	SHORT DPCHK

INTDIGITS:
	DEC	DL		;Need a comma yet?
	JNZ	NOCOM
	MOV	AL,","
	STOSB
	MOV	DL,3		;Reset comma count down
NOCOM:
	MOV	AL,"0"		;In case we're out of digits, use trailing "0"
	DEC	BL		;Any digits left?
	JS	STOINTDIG	;No - go store trailing zero
	LODSB			;Yes - get next digit
STOINTDIG:
	STOSB
	LOOP	INTDIGITS

;Check for fraction part and print decimal point if so

DPCHK:
	MOV	CL,[DECDIG]	;Number of digits to right of d.p.
	DEC	CX		;Count off decimal point itself
	JS	SCICHK		;No decimal point?
	MOV	AL,"."
	STOSB
	JZ	SCICHK		;A d.p., but no decimal digits?

;Print the zeros, if any, between after the decimal point but before the MSD

	OR	DH,DH		;Any fill zeros after d.p.?
	JLE	MOVFRAC
	MOV	AL,"0"
AFTDP:
	STOSB
	DEC	DH		;Limit to fill count
	LOOPNZ	AFTDP		;Limit to field width

;Print the fraction digits, if any

MOVFRAC:
	JCXZ	SCICHK		;Filled out field width?
	OR	BL,BL		;Any digits left?
	JLE	POSTFILL
MOVDECDIG:
	MOVSB			;Copy a digit
	DEC	BL		;Limit to digits available
	LOOPNZ	MOVDECDIG	;Limit to field width

;Fill out the field with zeros

POSTFILL:
	MOV	AL,"0"
	REP	STOSB		;Force fill with zeros

;Print exponent if in scientific notation

SCICHK:
	TEST	AH,SCIBIT	;Scientfic notation?
	JZ	SIGNCHK

	PUSH	AX		;Save flags from AH
	MOV	AX,"+E"
	CMP	[b$VTYP],VT_R8	; Is it double precision?
	JNZ	SCISNGL
	DEC	AX		; Convert "E" to "D"
SCISNGL:
	MOV	BX,[EXP]	; Get exponent
	OR	BX,BX		
	JNS	EXPSGN
	NEG	BX		;Get magnitude of exponent
	MOV	AH,"-"		;Display sign
EXPSGN:
	STOSW
	XCHG	AX,BX		; [AX] = exponent
	MOV	BL,100		
	DIV	BL		; [AL] = exp div 100, [AH] = exp mod 100
	TEST	BYTE PTR [b$PUFLG+1],BIGSCI	; see if 3 digit desired
	JZ	NOT_3DIGIT	; jump if not
	OR	AL,"0"		; ascii'ize the digit to be stored
	STOSB			; Put the high order digit in.
	XOR	AL,AL		; so test below will pass
NOT_3DIGIT:			
	OR	AL,AL		; set flags NZ if exp >= 100
	PUSHF			
	XCHG	AL,AH		; AL = exp mod 100
	AAM			;Convert exponent to unpacked BCD
	OR	AX,"00" 	;Add ASCII bias
	POPF			; get result of that compare again
	JZ	NO_EXP_OVERFLOW ; jump if not
	MOV	AH,"%"		; else replace with overflow char
NO_EXP_OVERFLOW:		
	XCHG	AL,AH		;MSD in AL
	STOSW			;Save both digits at once
	POP	AX		;Restore flags to AH

;Print trailing sign if needed

SIGNCHK:
	TEST	AH,TRAILING	;AH still has flags
	JZ	ENDNUM		;Need a trailing sign?
	MOV	AL,[b$SIGN]	
	STOSB

;Finish up with trailing 00 and compute line length

ENDNUM:
	MOV	BYTE PTR[DI],0	;Terminating zero
	MOV	SI,OFFSET DGROUP:b$FOBUF 
	XCHG	AX,DI
	SUB	AX,SI		;Length of string
cEnd				;pop ES, and exit to caller

;
; Added with revision [9]
;
;***
;B$CONASC - Convert number to ASCII
;
;Purpose:
;	Convert number to a string of ASCII digits with no leading or
;	trailing zeros. Return base 10 exponent and count of digits.
;
;Inputs:
;	B$AC has SP number OR B$DAC has DP or CY number
;	b$VTYP has type
;	No integer values will be passed to this routine.
;
;Outputs:
;	CX = number of significant figures (decimal point to right)
;	DL = base 10 exponent
;	SI = Address of first digit (non-zero unless number is zero)
;	[b$SIGN] has sign - blank if positive, "-" if negative
;
;Registers:
;	Uses all.
;****

cProc	B$CONASC,<NEAR> 	;NOTE: Prolog doesn't put anything on stack
cBegin
	MOV	CL,b$VTYP	;get val typ in CX
	CMP	CL,VT_I2	;We only handle I2 and I4
	JE	GotNum
	CMP	CL,VT_I4
	JNE	FloatingCONASC	;Do the floating point variety
GotNum:
	MOV	BX,OFFSET DGROUP:B$AC ;Get pointer to number
	CALL	B$FOUTBX	;And convert it to a string
	MOV	SI,BX		;We want to return the string in SI
	MOV	CX,AX		;CX = Length of string
	LODSB			;Get the sign
	MOV	b$Sign,AL	;And store it for later use
	XOR	DL,DL		;No base 10 exponent (a simplification)
	DEC	CX		;Don't count the sign
cEnd

FloatingCONASC:
	JMP	B$FloatCONASC	;Call the routine directly

sEnd	ST_TEXT
	END
