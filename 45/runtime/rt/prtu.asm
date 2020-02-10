	TITLE	PRTU - PRINT USING Driver
	page	56,132
;***
; PRTU - PRINT USING Driver
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	When USING clause encountered, B$USNG is called and sets b$PUSG
;	with the address of B$PREN.  For each print item, B$PREN is
;	indirectly called to perform the job of PRINT USING.
;
;	Note: this is a terrible module.  There had only one routine $$PREN
;		and it was unreadable.	I split it into B$PREN, PUSCAN &
;		PLSPRT according its logic and make the modifications which
;		are needed.  Even though, PUSCAN is still awful.  Be patient
;		to read it.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc
	INCLUDE rtps.inc	; constants shared with QBI

;Code segment
	useSeg	DK_TEXT 	; disk I/O
	useSeg	ST_TEXT
	useSeg	ER_TEXT
;Data segment
	useSeg	CONST
	useSeg	_DATA
	useSeg	_BSS

	INCLUDE seg.inc
	INCLUDE string.inc	

	SUBTTL	local constant definitions
	page

	USING	EQU	2	; using
	NOPEND	EQU	0	; no type pending (used by USING)

	CURNCY= "$"		;Currency symbol
	CSTRNG= "\"             ;String USING symbol
	VSTRNG= "&"		;Whole string symbol

	SCIENCE EQU	00000001B
	STRING	EQU	00000010B
	SIGN	EQU	00000100B
	PLUS	EQU	00001000B
	DOLLAR	EQU	00010000B
	STAR	EQU	00100000B
	COMMA	EQU	01000000B
	PUSING	EQU	10000000B

	BIGSCI	EQU	00000001B	; 3 digit sci-notation

	SUBTTL	data segment definitions
	page

sBegin	_DATA
	externB b$PRFG 	; flag for PRINT/LPRINT/WRITE [#][USING]
	externW b$VECS 	;defined in PRNVAL.ASM
	VPOS	EQU	b$VECS+2*0
	VWID	EQU	b$VECS+2*1
	VWCLF	EQU	b$VECS+2*2
	VWCH	EQU	b$VECS+2*3
	VTYP	EQU	b$VECS+2*4
	VTYPCNT EQU	b$VECS+2*5
	VOUTCNT EQU	b$VECS+2*6
sEnd	;_DATA

sBegin	_BSS

	globalW b$DIGCNT,,1	;Count of digits before and after d.p.

	globalW b$PUFLG,,1	; Flag word
;
;Bits of the flag word are used as follows:
;
;bit 0	1=Scientific notation		0=Fixed format
;bit 1	1=Print string			0=Print number
;bit 2	1=Place sign after number	0=No sign after number
;bit 3	1=Print "+" for positive number 0=No "+"
;bit 4	1=Print "$" in front of number	0=No "$"
;bit 5	1=Pad with leading "*"		0=Pad with leading spaces
;bit 6	1=Put commas every three digits 0=No commas
;bit 7	1=Print using output,		0=Free format output
;bit 8	1=3 digit scientific notation (bit 0 is one also)
;
	staticW PUSC,,1 	;PRINT USING scan count
	staticB PUVS,,1 	;PRINT USING value seen flag

	externW b$PUSG 	; store the actual address for PRINT USING

	externW b$PUDS 	;defined in GOSTOP.ASM

	externB b$VTYP 	; defined in GLOBAL.INC
	externW b$pSTDALCTMP	; indirect B$STDALCTMP vector

sEnd	;_BSS

	SUBTTL	code segment externals
	page


sBegin	ST_TEXT
	externFP	B$SASS 	
	externNP	B$PUFOUT	;in pufout.asm
	externNP	B$OutBlanks	
sEnd	;ST_TEXT

sBegin	ER_TEXT
	externNP	B$ERR_FC
	externNP	B$ERR_TM
sEnd	;ER_TEXT

	assumes CS,ST_TEXT
sBegin	ST_TEXT

	SUBTTL	B$PREN -- main body of PRINT USING
	page
;***
;B$PREN -- PRINT USING
;
;Purpose:
;	Perform the format print according to b$PUFLG.  This routine first
;	prints the item according to the format in b$PUFLG, if it is nonzero,
;	and then sets the b$PUFLG for next item.  b$PUFLG is reset to zero
;	by B$USNG (the preamble of PRINT USING), and then is set each time
;	this routine is called.
;
;	Actually, except the field size, there is not much to consider if
;	printing a string.  For printing a number, there are a lot of
;	varieties.  However, the formatting is done in B$PUFOUT.
;Entry:
;	[BX]		= *SD, *I2, *I4, *R4, or *R8
;	[b$PUFLG]	= print using format flag
;	[b$PUDS]	= USING string descriptor
;Exit:
;	[b$PUFLG] is set for next item (or 0 if none)
;Uses:
;	none
;Exceptions:
;	illegal function call -- B$ERR_FC
;	type mismatch -- B$ERR_TM
;*******************************************************************************

cProc	B$PREN,<PUBLIC,NEAR>,<SI,DI>	;save di,si

cBegin
	MOV	AX,[b$PUFLG]	; test if type pending
	OR	AX,AX		; is this the first print using item ?
	JNZ	PrtItem 	;Brif not, go print it
	MOV	PUVS,AL 	; clear PUVS
	PUSH	BX		;save item to be printed
	MOV	AX,[b$PUDS]	;get length of PRINT USING string
	MOV	PUSC,AX 	;save scan count remaining
	OR	AX,AX		;null string?
	JZ	FARG		;If so, illegal function call
	CALL	PUSCAN		;scan the format for the first item
				;[b$PUFLG] is set on return
	POP	BX		;get item back
	MOV	AL,BYTE PTR [b$PUFLG]	; AL has the format of the printing item
PrtItem:
	MOV	PUVS,AL 	;save information in PUVS
	MOV	CX,b$DIGCNT	;load [CX]
	TEST	AL,STRING	;does the field define a string?
	JNZ	PrtStr		;go printing a string
	CMP	b$VTYP,VT_SD	;is it a string ?
	JZ	TMER		;Brif yes, give "type mismatch"
	cCall	B$PUFOUT	;translate and format the printing number
				; needs [BX] = *I2, *I4, *R4 or *R8
				; and [b$PUFLG] = format
				;on return,
				; [SI] point to the digit string
				; [AX] count of length
				; DI is used
	XCHG	AX,CX		;cut count in CX
	CALL	[VOUTCNT]	;output string with CX=length
	JMP	SHORT ScanNext	;try next, set b$PUFLG for next item
PrtStr:

	; Throughout the following code, CX = length of string to print
	; and DX = number of spaces to follow it.

	CMP	b$VTYP,VT_SD	;type better be string
	JNZ	TMER		;Brif not, give "type mismatch"
	MOV	DX,CX		;save length of print field into DX
	MOV	CX,[BX] 	;get length of string
	SUB	DX,CX		;DX = padding count

;-------------------------------
;Note that if we have a variable length string field, DX will be -1.
;The result of the above subtraction will never carry (so we will take the
;JAE below), but the number left in DX will always be negative. Thus when
;we get to the padding check, no blanks will be added.
;-------------------------------

	JAE	PrtIt		;if field is big enough, proceed
	ADD	CX,DX		;CX = CX + (old DX-CX) = old DX = size of field


PrtIt:
	PUSH	DX		; save DX, DX is used by [VTYPCNT]
	CALL	[VTYPCNT]	;output CX bytes of string
	CALL	[b$pSTDALCTMP]	;deallocate the temp string if it is
	POP	CX		; padding count in CX (could be negative!)
	CALL	B$OutBlanks	; output CX spaces
ScanNext:
	CALL	PUSCAN		;scan & set the format for next item
cEnd				;pop si,di and exit to caller

FARG:	JMP	B$ERR_FC		;Illegal function call
TMER:	JMP	B$ERR_TM		;Type mismatch

	SUBTTL	scan one print format from USING string
	page
;***
;PUSCAN -- scan and set up the print format for one item
;
;Purpose:
;	This routine scans the Using string, b$PUDS, and sets up the flag
;	b$PUFLG, so the next print item may be formated.
;
;	The possible formats for a print item are:
;
;	for string item:
;		!	print first character of that string
;		\...\ 	print 2+n characters, n is the spaces between two "\"
;		&	print variable length (b$DIGCNT is set to -1)
;	for numeric item
;		#	print one digit
;		.	print "."
;		+	print sign
;		-	print minus sign at the end of the number
;		**	print "*" as the leading character
;		$$	print "$" preceeding the number
;		,	print "," each three digits
;		^^^^	print scientific notation
;		^^^^^	print scientific notation, 3 digits
;		_	print next character as a literal character
;
;	Also, a "%" will be printed preceeding the number if the field specified
;	is not big enough.
;Entry:
;	[PUSC]		= scan count
;	[b$PUDS]	= SD of Using string
;Exit:
;	[b$PUFLG] is set
;Uses:
;	SI
;Exceptions:
;	illegal function call -- B$ERR_FC
;*******************************************************************************

PUSCAN:
	MOV	[b$PUFLG],0	;reset the format flag
	MOV	SI,[b$PUDS+2]	;start of string (could change if G.C.!)
	MOV	AX,[b$PUDS]	;length of the string
	MOV	CX,PUSC 	;get scan count
	SUB	AX,CX		;offset into string
	ADD	SI,AX		;move the string pointer
	JCXZ	RETL		;at end of string?
	JMP	SHORT PRCCHR	;If not, scan for next value (or EOS)

REUSIN: cCall	PLSPRT		;print a "+" if necessary
	CALL	[VWCH]		;output the character in AL
REUSN1:
	MOV	PUSC,CX 	;save scan count ( = 0 )
	MOV	BYTE PTR [b$PUFLG],CL	; set the b$PUFLG
	CMP	CL,PUVS 	;any values seen in string?
	JZ	FARG		;if not, we'll never get anywhere
RETL:	RET			;exit to caller

;-------------------------------
; Here to handle a literal character in the using string preceded by "_".
;-------------------------------

LITCHR: cCall	PLSPRT		;print previous "+" if there is any
	LODSB			;fetch literal character

	CALL	[VWCH]		;output that literal character
	LOOP	PRCCHR		;decrement count (CX)
	JMP	SHORT REUSN1	;Brif no more

;-------------------------------
; Here to handle variable length string field specified with "&".
;-------------------------------

VARSTR:
	MOV	BX,-1		;SET LENGTH TO MAXIMUM POSSIBLE
ISSTRF:
	DEC	CX		;DECREMENT THE "USING" STRING CHARACTER COUNT
	cCall	PLSPRT		;PRINT A "+" IF ONE CAME BEFORE THE FIELD
	MOV	PUSC,CX 	;Save scan count
	MOV	b$DIGCNT,BX	;Save field width
	MOV	BYTE PTR [b$PUFLG],2	; Flag string type
	RET

BGSTRF:
	;MOV	BP,CX		;SAVE THE "USING" STRING CHARACTER COUNT
				;can't use BP, but we exhaust all registers
				; we have to use another way to solve it
	MOV	DI,SI		;SAVE THE POINTER INTO THE "USING" STRING
	MOV	BX,2		;THE \\ STRING FIELD HAS 2 PLUS
				;NUMBER OF ENCLOSED SPACES WIDTH

;There does not have to be a test for KANJI characters in this loop, as
;We know that we are not in the middle of one, and no KANJI can start with
;either a CSTRNG or a SPACE

LPSTRF:
	LODSB			;GET THE NEXT CHARACTER
	CMP	AL,CSTRNG	;THE FIELD TERMINATOR?
	JZ	ISSTRF		;GO EVALUATE A STRING AND PRINT
	INC	BX		;INCREMENT THE FIELD WIDTH
	CMP	AL," "		;A FIELD EXTENDER?
	LOOPZ	LPSTRF		;KEEP SCANNING FOR THE FIELD TERMINATOR

;-------------------------------
; Since  string field wasn't found, the "using" string character count and the
; pointer into it's data must be restored and the "\" printed.
;
; The way to restore CX has to pay a little bit attention, since we can't
; use BP as tempopary saving register.	Note that the SI incremented by one
; and CX decrement by one in the LPSTRF loop, if we don't find the string
; field.  So that, when enter NOSTRF, the amount of SI's increment is equal
; to the amoutn of the CX's decrement.  We may then restore CX using this
; calculation.	How lucky we are !
;-------------------------------

NOSTRF:
	SUB	SI,DI		;calculate the bytes we have scaned, put in SI
	ADD	CX,SI		;restore the count in CX
	MOV	SI,DI		;RESTORE THE POINTER INTO "USING" STRING'S DATA
	;MOV	CX,BP		; can't use BP
	MOV	AL,CSTRNG	;RESTORE THE CHARACTER

;-------------------------------
; Here to print the character in [al] since it wasn't part of any field.
;-------------------------------

NEWUCH: cCall	PLSPRT		;IF A "+" CAME BEFORE THIS CHARACTER
				;MAKE SURE IT GETS PRINTED
	CALL	[VWCH]		;Print the char that wasn't part of a field
;-------------------------------
; Scan one value field from USING string, and set up print using flags.
; On entry:
;	[SI]	= Points to next byte in PRINT USING string
;	[CX]	= Count of characters left in PRINT USING string
;	[DH]	= Flag byte used by B$PUFOUT (stored in b$PUFLG - see above)
;	[BH]	= Number of digits to left of decimal point (numbers only)
;	[BL]	= Number of digits to right of decimal point (numbers only)
;	[BX]	= Length of string field (strings only)
;-------------------------------

PRCCHR: XOR	AL,AL		;SET [DH]=0 SO IF WE DISPATCH
	MOV	DH,AL		;DON'T PRINT "+" TWICE
PLSFIN: cCall	PLSPRT		;ALLOW FOR MULTIPLE PLUSES IN A ROW
	MOV	DH,AL		;SET "+" FLAG
	LODSB			;GET A NEW CHARACTER

	CMP	AL,"!"		;CHECK FOR A SINGLE CHARACTER
	MOV	BX,1		;Set string length to 1
	JZ	ISSTRF		;STRING FIELD
	CMP	AL,"#"		;CHECK FOR THE START OF A NUMERIC FIELD
	JZ	NUMNUM		;GO SCAN IT
	CMP	AL,VSTRNG	;See if variable string field
	JZ	VARSTR		;GO PRINT ENTIRE STRING
	DEC	CX		;ALL THE OTHER POSSIBILITIES
				;REQUIRE AT LEAST 2 CHARACTERS
	JZ	REUSIN		;IF THE VALUE LIST IS NOT EXHAUSTED
				;GO REUSE "USING" STRING
	CMP	AL,"+"		;A LEADING "+" ?
	MOV	AL,PLUS 	;SETUP [DH] WITH THE PLUS-FLAG ON IN
	JZ	PLSFIN		;CASE A NUMERIC FIELD STARTS
	MOV	AL,[SI-1]	;GET BACK THE CURRENT CHARACTER
	CMP	AL,"."		;NUMERIC FIELD WITH TRAILING DIGITS
	JZ	DOTNUM		;IF SO GO SCAN WITH [BH]=
				;NUMBER OF DIGITS BEFORE THE "."=0
	CMP	AL,"_"		;CHECK FOR LITERAL CHARACTER DECLARATION
	JZ	LITCHR
	CMP	AL,CSTRNG	;CHECK FOR A BIG STRING FIELD STARTER
	JZ	BGSTRF		;GO SEE IF IT REALLY IS A STRING FIELD
	CMP	AL,[SI] 	;SEE IF THE NEXT CHARACTER MATCHES THE
				;CURRENT ONE
	JNZ	NEWUCH		;IF NOT, CAN'T HAVE $$ OR ** SO ALL THE
				;POSSIBILITIES ARE EXHAUSTED
	CMP	AL,CURNCY	;IS IT $$ ?
	JZ	DOLRNM		;GO SET UP THE FLAG BIT
	CMP	AL,"*"		;IS IT ** ?
	JNZ	NEWUCH		;IF NOT, ITS NOT PART
				;OF A FIELD SINCE ALL THE POSSIBILITIES
				;HAVE BEEN TRIED
	OR	DH,STAR 	;Set "*" bit
	INC	SI
	CMP	CX,2		;SEE IF THE "USING" STRING IS LONG
				;ENOUGH FOR THE SPECIAL CASE OF
	JC	SPCNUM		; **$
	MOV	AL,[SI]
	CMP	AL,CURNCY	;IS THE NEXT CHARACTER $ ?
	JNZ	SPCNUM		;IF IT NOT THE SPECIAL CASE, DON'T
				;SET THE DOLLAR SIGN FLAG
	DEC	CX		;DECREMENT THE "USING" STRING CHARACTER COUNT
				;TO TAKE THE $ INTO CONSIDERATION
	INC	BH		;INCREMENT THE FIELD WIDTH FOR THE
				;FLOATING DOLLAR SIGN
DOLRNM:
	OR	DH,DOLLAR	;SET BIT FOR FLOATING DOLLAR SIGN FLAG
	INC	SI		;POINT BEYOND THE SPECIAL CHARACTERS
SPCNUM: INC	BH		;SINCE TWO CHARACTERS SPECIFY
				;THE FIELD SIZE, INITIALIZE [BH]=1
NUMNUM: INC	BH		;INCREMENT THE NUMBER OF DIGITS BEFORE
				;THE DECIMAL POINT
	MOV	BL,0		;SET THE NUMBER OF DIGITS AFTER
				;THE DECIMAL POINT = 0
	DEC	CX		;SEE IF THERE ARE MORE CHARACTERS
	JZ	NOTSCI		;IF NOT, WE ARE DONE SCANNING THIS
				;NUMERIC FIELD
	LODSB			;GET THE NEW CHARACTER
	CMP	AL,"."		;DO WE HAVE TRAILING DIGITS?
	JZ	AFTDOT		;IF SO, USE SPECIAL SCAN LOOP
	CMP	AL,"#"		;MORE LEADING DIGITS ?
	JZ	NUMNUM		;INCREMENT THE COUNT AND KEEP SCANNING
	CMP	AL,","		;DOES HE WANT A COMMA
				;EVERY THREE DIGITS?
	JNZ	FINNUM		;NO MORE LEADING DIGITS, CHECK FOR ^^^
	OR	DH,COMMA	;TURN ON THE COMMA BIT
	JMP	NUMNUM		;GO SCAN SOME MORE

;-------------------------------
; Here when a "." is seen in the "using" string.  It starts a numeric field
; if and only if it is followed by a "#".  Once again, we do not have to
; test for KANJI characters because "#" and "^" can not be a legal first
; character for a double byte character.
;-------------------------------

DOTNUM:
	MOV	AL,[SI] 	;GET THE CHARACTER THAT FOLLOWS
	CMP	AL,"#"		;IS THIS A NUMERIC FIELD?
	MOV	AL,"."		;IF NOT, GO BACK AND PRINT "."
	JZ	DOTNU1		;[RDK]
	JMP	NEWUCH		;[RDK]cannot reach by short jump
DOTNU1: 			;[RDK]
	MOV	BL,1		;INITIALIZE THE NUMBER OF
				;DIGITS AFTER THE DECIMAL POINT
	INC	SI
AFTDOT: INC	BL		;INCREMENT THE NUMBER OF DIGITS
				;AFTER THE DECIMAL POINT
	DEC	CX		;SEE IF THE "USING" STRING HAS MORE
	JZ	NOTSCI		;CHARACTERS, AND IF NOT, STOP SCANNING
	LODSB			;GET THE NEXT CHARACTER
	CMP	AL,"#"		;MORE DIGITS AFTER THE DECIMAL POINT?
	JZ	AFTDOT		;IF SO, INCREMENT THE COUNT AND KEEP
				;SCANNING

;-------------------------------
; Check for the "^^^^" that indicates scientific notation.
;-------------------------------

FINNUM:
	DEC	SI		;Point back to current character
	CMP	WORD PTR [SI],"^^" ;Two "^"s in a row?
	JNZ	NOTSCI
	CMP	WORD PTR [SI+2],"^^" ;Four "^"s in a row?
	JNZ	NOTSCI
	CMP	CX,4		;WERE THERE ENOUGH CHARACTERS FOR "^^^^"?
	JC	PUSCAX
	SUB	CX,4
	ADD	SI,4
	INC	DH		;TURN ON THE SCIENTIFIC NOTATION FLAG

	JCXZ	NOTSCI		; jump if no more using to look at
	CMP	BYTE PTR [SI],"^" ; check for 5th "^"
	JNZ	NOTSCI		; no need to play games
	INC	SI		; eat the character
	DEC	CX		
	MOV	[b$PUFLG+1],BIGSCI ; Indicate 3 digit scientific

NOTSCI:
	INC	BH		;INCLUDE LEADING "+" IN NUMBER OF DIGITS
	TEST	DH,PLUS 	;DON'T CHECK FOR A TRAILING SIGN
	JNZ	ENDNUM		;ALL DONE WITH THE FIELD IF SO
				;IF THERE IS A LEADING PLUS
	DEC	BH		;NO LEADING PLUS SO DON'T INCREMENT THE
				;NUMBER OF DIGITS BEFORE THE DECIMAL POINT
	JCXZ	ENDNUM		;SEE IF THERE ARE MORE CHARACTERS
	LODSB			;GET THE CURRENT CHARACTER
	CMP	AL,"-"		;TRAIL MINUS?
	JZ	SGNTRL		;SET THE TRAILING SIGN FLAG
	CMP	AL,"+"		;A TRAILING PLUS?
	JNZ	ENDNUM		;IF NOT, WE ARE DONE SCANNING
	OR	DH,PLUS 	;TURN ON THE POSITIVE="+" FLAG
SGNTRL:
	OR	DH,SIGN 	;TURN ON THE TRAILING SIGN FLAG
	DEC	CX		;DECREMENT THE "USING" STRING CHARACTER
				;COUNT TO ACCOUNT FOR THE TRAILING SIGN
ENDNUM:
	MOV	PUSC,CX 	;Save scan count
	MOV	b$DIGCNT,BX
	ADD	BH,BL		;Digit count must not exceed 24
	CMP	BH,25
	JNC	ARGERR		;IF SO, "ILLEGAL FUNCTION CALL"
	OR	DH,PUSING	;TURN ON THE "USING" BIT
	MOV	BYTE PTR [b$PUFLG],DH	
PUSCAX:
	RET			;GET OUT

ARGERR: JMP	B$ERR_FC

	SUBTTL	check/print "+"
	page
;***
;PLSPRT -- decide whether a "+" should be printed or not
;
;Purpose:
;	If the "+" flag in [DH] is set, and a character which is decided
;	not part of a numeric field is found, a "+" is printed.
;
;	When a "+" is detected in the "using" string, if a numeric field
;	follows, a bit in [DH] should be set, otherwise a "+" should be
;	printed.  Since deciding whether a numeric field follows is very
;	difficult, the bit is always set in [DH].  At the point a character
;	which is decided not part of a numeric field is found, this routine
;	is called to see if the bit in [DH] is set.  If it is, then a plus
;	is printed.
;Entry:
;	[DH]	= print using flag (refer the definition of b$PUFLG)
;Exit:
;	none
;Uses:
;	none
;Preserves:
;	[AX]	= current character
;Exceptions:
;	none
;*******************************************************************************

cProc	PLSPRT,<NEAR>

cBegin
	OR	DH,DH		;check the plus bit in DH
	JZ	RT
	PUSH	AX		;save current character
	MOV	AL,"+"		;print "+"
	CALL	[VWCH]		;do it
	POP	AX		;get back character
RT:
cEnd				;exit to caller
sEnd	;ST_TEXT

; Moved B$PUSG here from PR0A.ASM to increase /O modularity
; Revision applies to entire routine B$PUSG.
	assumes CS,DK_TEXT	
sBegin	DK_TEXT			

	SUBTTL	interface for USING preamble
	page
;***
;B$USNG -- USING preamble [7]
;void B$USNG(sd *psdPUexp)
;
;Purpose:
;	This is the preamble for USING clause found in PRINT/LPRINT stmt.
;	It ORs the flag, b$PRFG, with USING (=2) to indicate an USING
;	clause is on going.  It also set up the b$PUSG with the actual
;	working routine, B$PREN.  The using expression is copied into
;	b$PUDS for further use.
;Entry:
;	Parameter is in stack:
;	sd	*psdPUexp
;Exit:
;	[b$PRFG] OR USING (=2)
;	[b$PUFLG] = NOPEND (set to no type pending)
;	[b$PUDS] = string of the print using
;	[b$PUSG] = address of actual working routine (B$PREN)
;Uses:
;	none
;Exceptions:
;	none
;*******************************************************************************

cProc	B$USNG,<PUBLIC,FAR>		
ParmSD	sdPUexp 			; sd of using expression
cBegin
	MOV	AX,OFFSET DGROUP:b$PUDS 
	cCall	B$SASS,<sdPUexp,AX>	;copy "using" string
	OR	[b$PRFG],USING 		;set up USING flag
	MOV	[b$PUSG],OFFSET B$PREN
					;set up the entry address for USING
	MOV	[b$PUFLG],NOPEND 	;set to no type pending
cEnd					;exit to caller

sEnd	;DK_TEXT			
	END
