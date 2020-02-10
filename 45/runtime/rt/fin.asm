	TITLE	FIN - String and numeric input
;***
; FIN - String and numeric input
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; BASIC Syntax mapping to included runtime entry points:
;
; - VAL Function:
;
;      v = VAL(x$)
;	    |
;	B$FVAL
;
;******************************************************************************
	INCLUDE rmacros.inc	; Runtime Macro Defintions
	INCLUDE switch.inc

	useSeg	_DATA		
	useSeg	_TEXT		
	useSeg	MT_TEXT 	
	useSeg	NH_TEXT 	
	useSeg	_BSS		

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc	; SKIP macro
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE const.inc	; Values of Flags for KANJI support
	INCLUDE idmac.inc	
	INCLUDE string.inc	

;
; The following constants are from the mathpack interface to $i8_input
; (Note: s16inv is a flag we synthesize.)
;
l_s16inv=	80h		; 16 bit signed value is invalid
l_ind=		80h		; indefinite
l_inv=		40h		; invalid (no digits or syntax error)
l_s32inv=	20h		; 32 bit signed value is invalid
l_u32inv=	10h		; 32 bit unsigned value is invalid
l_long= 	08h		; l_Dexp or more than 7 digits
l_Dexp= 	04h		; explicit 'D' or 'd' seen
l_Rexp= 	02h		; explicit 'E' or 'e' seen
l_inf=		01h		; DP overflow
h_curr= 	  2		;currency value was valid

sBegin	_DATA			

	externD B$AC		
	externQ B$DAC		
	externB $i8_input_ws   ; white space skipping flag

sEnd	_DATA			

sBegin	_BSS			

	externB b$VTYP		


sEnd	_BSS			

externFP B$LDFS 		; Load fixed length string




sBegin	MT_TEXT

externNP B$STPUTZ		
externNP B$STDALCTMP		
externNP B$ERR_OV		
externNP B$ERR_TM		
externNP B$ftolrnd		; Convert TOS to long integer in DX:AX
externNP B$UPCASE		; Convert AL to an uppercase character



assumes CS,MT_TEXT

	SUBTTL	B$FVAL - VAL function
	PAGE
;***
; B$FVAL - VAL function
;
;Function:
;	Compute numeric equivalent of the string
;
;Inputs:
;	sdNum = Address of string descriptor
;
;Outputs:
;	Double precision result in DAC
;
;******************************************************************************
cProc	B$FVAL,<FAR,PUBLIC>,<SI> 
parmSD	sdNum			
cBegin				
	MOV	BX,sdNum	
	CALL	B$STPUTZ	
	MOV	SI,[BX+2]	;Get address of string data
	PUSH	BX		;Save string descriptor address
	MOV	[B$VTYP],VT_R8	; Force to double precision
	CALL	B$FIN		
	POP	BX
	CALL	B$STDALCTMP	; Delete if temp string
	MOV	AX,OFFSET DGROUP: B$DAC	
cEnd				

	SUBTTL	B$FIN - Floating point and string input

;***
; B$FIN - Floating point and string input
; Largely rewritten
;
;Purpose:
;	Perform floating point and string input.  The string that
;	is passed must be zero terminated.
;
;
;	If FK_FULLMAP then the calling routine must allocate B$Buf1
;	and B$Buf2 before calling B$FIN and must deallocate them
;	after the call.  The way that these buffers are used in this
;	routine will NOT affect their use in B$INPP to hold data. Right
;	before calling FAR_I8_INPUT, we convert the rest of this data
;	item (up to a termination comma or zero terminator) from double
;	byte to single byte and store it in the buffers.  This will only
;	overwrite data that is no longer needed for INPUT.
;
;Entry:
;	[b$VTYP] = Type of value required
;	[SI]  = Address of character stream to analyze for the value.
;	[ES]  = segment of character stream IF REQUESTED TYPE IS STRING
;		Otherwise the requested segment is DS.
;Exit:
;	AC    = Address of string descriptor ([B$VTYP] = VT_SD)
;	AC    = integer value ([B$VTYP] = VT_I2)
;	AC    = long integer value ([B$VTYP] = VT_I4)
;	AC    = S.P. value ([B$VTYP] = VT_R4)
;	DAC   = D.P. value ([B$VTYP] = VT_R8)
;	DAC   = currency value ([B$VTYP] = VT_CY)
;	[AX]  = Delimiter Character
;	[SI]  = Address to start scanning for next item (if any) in list
;
;Uses:
;	SI is updated [3]
;
;Preserves:
;	None
;
;Exceptions:
;	If a runtime error occurs, this routine doesn't return to caller
;******************************************************************************
cProc	B$FIN,<PUBLIC,NEAR>,<DI,ES,BP>	; Warning!	Alternate entry
cBegin					;		through B$SimpleFin.
;
; If we are evaluating TO a string, call B$STRSCAN to do that, and then just
; assign the result to a string temp.
;
	CMP	[B$VTYP],VT_SD	; Getting a string?
	JNZ	FIN_5		;Jump if not
	CALL	B$STRSCAN	;[ES:DX] = start, [CX]=length [AX]=delimiter
	PUSH	AX		;Save delimiter Character
	cCall	B$LDFS,<ES,DX,CX>	;[AX] = address of string temp
	MOV	[WORD PTR B$AC],AX	; Put address in AC for safekeeping
	POP	AX		;Restore Delimiter Character
	JMP	FIN_Str_Exit	;go exit

FIN_5:
;
; Determine the target base. This is a function of the leading characters:
;	<none>	Decimal
;	&	Octal integer
;	&O	Octal Integer
;	&H	Hex Integer
;
; where integers may be short or long.
;
	PUSH	DS		;If not string,
	POP	ES		;assumed below that ES = DS

	XOR	BX,BX		;[BL] = radix. 0 is default
	MOV	[$i8_input_ws],1       ; skip white space is default
	CALL	B$GETCH 	;[AL] = next char (skipping white space)
	CMP	AL,'&'		;check for special radix constant
	jne	FIN_10		;jump if normal number
;
;	read special radix numbers
;
	MOV	[$i8_input_ws],BL ; reset skip white space for non-decimal
	CALL	B$GETCH_NS	;[AL] = next char (not skipping white space)
	MOV	BX,10H		;assume hex
	CMP	AL,'H'
	JE	FIN_15
	MOV	BL,8H		;assume octal
	CMP	AL,'O'
	JE	FIN_15

FIN_10:
	DEC	SI		; must be octal - move back pointer
;
; At this point, BX contains the desired conversion base, if one was specified,
; or zero for a default.
;
FIN_15:
;
; This odd piece of code ensure that the max length (passed in cx) when added
; to the start address of the string does not overflow the datasegment. The
; length is just a maximum, and we should stop conversion prior to exceding
; that in all cases by virtue of a zero terminator.
;
; We determine if the string start address is in the range FF00 to FFFF. If
; not, the max length is set to 255. If it is in that range, we set the max
; length to 1000H - start address - 1, i.e. the amount of room left in dgroup.
;

	MOV	CX,SI		;[CX] = string start address
	INC	CH		;Zero set if in 0FFxxH range
	JNZ	FIN_16		;Jump if not in that range
	XOR	CX,CX		;[CX] = 0
FIN_16:
	SUB	CX,SI		;[CX] = either 100H, or space left in DGROUP
	DEC	CX		;[CX] = either 0FFH, or space -1


;
; Set up for, and call the math-pack's conversion routine.
;
InvokeMathPack:

	MOV	DI,OFFSET DGROUP:B$DAC	;[DI] = location to place R8 result
	XOR	AX,AX			;[AX] = 0 (FORTRAN garbage)
	CWD				;[DX] = 0 (FORTRAN garbage)
	PUSH	BX			; Save radix
	PUSH	SI			; Save starting point
	PUSH	BP			;$i8_input trashes BP
	CALL	FAR PTR FAR_I8_INPUT	;Call math pack via aother seg
	POP	BP
	MOV	[$i8_input_ws],0  ;reset skip white space for C

	POP	DX			; DX = pointer to start location
	CMP	SI,DX			; did we use any characters
	JNE	ParsedNumber		; brif so, compute value

; If the mathpack did not like the first character it saw, it will return
; with l_inv false but l_s32inv and l_u32inv true.  We want to accept the
; number as valid, but 0. So munge the flags and the return value here.
; Note that it will have walked on the 8 byte value (B$DAC) also.

	AND	CL,NOT (l_s32inv OR l_u32inv) ;[42] make the number look valid
	XOR	AX,AX		; Set the number to zero
	XOR	BX,BX
	STOSW			; Store it as an R8 (will be 0 for all
	STOSW			;  possible lengths). DI is return
	STOSW			;  as part of B$FAR_I8_INPUT, and ES=DS.
	STOSW

ParsedNumber:

; After calling the math pack, we have two results. The R8 result is in B$DAC,
; unless a radix was specified. If the result is a valid U2, it is in AX, if
; it is a valid U4, it is in BX:AX. Flags are returned in CX.
; Possible 8-byte CY value is in $i8_input_cy.				    [33]
;
; Create the "16 bit value invalid" flag
;
	CWD			;Sign extend I2 in AX into DX
	CMP	BX,DX		;Same as 32 bite high word?
	JZ	FIN_18		;Jump if 16 bit signed value okay
	OR	CL,l_s16inv	;else 16 bit signed value is in error
FIN_18:
	TEST	CL,l_s32inv	;check if valid 32-bit integer
	JZ	FIN_19		;Jump if it is...
	OR	CL,l_s16inv	;else it's also a bad 16 bit'er
FIN_19:
	XCHG	BX,DX		; [DX:AX] = possible long result
	POP	BX		; [BX] = Originally requested base
	MOV	BH,BL		; [BH] = Originally requested base


;
; Check for a type character following the number. Modify the returned flags
; such that when we try to store the result, overflows will be detected.
;
	XCHG	AX,BX		;[DX:BX] = possible long result, [AH] = base
	CALL	B$GETCH_NS	;see if there's a suffix character
;
; If a non-long constant was specified, and the value falls in the range 32768
; to 65535, convert to a negative short integer.
;
	CMP	AL,'&'		; Z if long const
	JZ	FIN_43		; Long constant, do nothing
	OR	AH,AH		
	JZ	FIN_43		; Not any constant, do nothing
	
	;We also have to test for l_inv, as there is an altmath bug where
	;$I8_INPUT may set l_inv but forget to set l_s32inv or l_u32inv.
	
	TEST	CL,l_s32inv OR l_inv 
	JNZ	FIN_43		; Not a valid int, do nothing
	OR	DX,DX		; In range 0-65535?
	JNZ	FIN_43		; If not, nothing to do
	XCHG	AX,BX		; [DX:AX] = possible long return
FIN_GOT_CONST:
	CWD			; New high order work is value sign extend
	AND	CL,NOT l_s16inv ; result is a valid 16-bit integer
	MOV	[WORD PTR B$DAC],AX ; save low word
	XCHG	AX,BX		; [DX:BX] = possible long return

	FILD	WORD PTR B$DAC	; [ST0] = R8
	FSTP	B$DAC		; Save as R4

FIN_43: 			

	CMP	AL,'#'		;double precision?
	JNE	FIN_20
	OR	CL,l_long+l_s32inv+l_s16inv	;force it double
	JMP	SHORT FIN_40
FIN_20:

	CMP	AL,'!'		;single precision?
	JNE	FIN_25
	AND	CL,NOT l_long	;force it single
	OR	CL,l_s32inv+l_s16inv
	JMP	SHORT FIN_40
FIN_25:

	CMP	AL,'%'		;integer?
	JNE	FIN_30
	TEST	CL,l_s16inv	;check if valid 16-bit integer
	JNZ	FIN_TM		; no - type character error
	JMP	SHORT FIN_40

FIN_30:
	CMP	AL,'&'		;long?
	JNE	FIN_35
	TEST	CL,l_s32inv	;check if valid 32-bit integer
	JNZ	FIN_TM		;  no - type character error
	OR	CL,l_s16inv	;force it long
	JMP	SHORT FIN_40

FIN_TM: JMP	B$ERR_TM	;invalid type character

FIN_35:

	DEC	SI		;back up

FIN_40:
;
; Since we couldn not parse any number at all, we have to explicitly
; return a zero ($I8_INPUT will return garbage).  We can also just
; return without further tests, which will avoid an altmath bug where
; certain invalid flags are not being set if l_inv is set.
;
; This check could not be done earlier, as we need to do the same type
; character parsing as before this bug fix.
;
	TEST	CL,l_inv	; was the number invalid?
	JNZ	InValidNumber	; yes, zero out our number and return
;
; Now we attempt to store the result, based on the user-requested data type.
;
	XCHG	AX,BX		; [DX:AX] = possible long result
	MOV	BL,[B$VTYP]    
;
; If the result is a valid 16 bit int, and the user wants an int, just store
; the result (in AX) into the fac.
;
	TEST	CL,l_s16inv	;result a valid 16-bit integer?
	JNZ	FIN_45		;Jump if not
	CMP	BL,VT_I2	; User wants an I2?
	JZ	FIN_50		;brif so - same logic as when have I4
;
; If the result is a valid 32 bit int (or a fall through valid 16 bit int), and
; the user want's an I4, then just store the result (in BX:AX) into B$AC.
;
FIN_45:
	TEST	CL,l_s32inv	;result a valid 32 bit int?
	JNZ	FIN_55		;Jump if not
	CMP	BL,VT_I4	; caller wants an I4?
	JNZ	FIN_55		;brif not
	MOV	[WORD PTR B$AC+2],DX	 ; save high word
FIN_50:
	MOV	[WORD PTR B$AC],AX	 ; save low word
	jmp	SHORT FIN_EXIT		 ;done - exit
;
;If the result is a valid currency, and the user wants currency, just
;store the result (at $i8_input_cy) into B$DAC.
;
FIN_55: 			
;
; The number is not a valid int, thus only the R8 in B$DAC is valid. Determine
; what type the user wants, and coerce to that by loading up on the numeric
; stack, and storing as the desired type.
;
	TEST	CL,l_inf	; Waaaayyyyyy too big?
	JNZ	FIN_OV		; then go complain.
	XCHG	BX,DX		; [DL] = valtype
	CMP	DL,VT_R8	;Does the user perhaps want R8?
	JZ	FIN_EXIT	;In that case, we're done!

	FLD	B$DAC		; [ST0] = R8

	CMP	DL,VT_R4	;Does the user want an R4?
	JNZ	FIN_60		;Jump if not, probably wants an int

	FSTP	B$AC		; Save as R4

	JMP	SHORT FIN_EXIT
;
; Here we have an Integer result type, with only an R8 in B$AC. Convert to the
; appropriate type, with rounding, and make sure the result fits.
;
FIN_60:
	CMP	DL,VT_I2
	JZ	FIN_65		;Must be either I2 or I4 or CY at this point
	CMP	DL,VT_I4
	JNZ	FIN_OV		;Jump if error
FIN_65:
	PUSH	DX		;Save the user resuested type
	cCALL	B$ftolrnd	;Round nearest or even & Store it in DX:AX
	MOV	[WORD PTR B$AC],AX	; Store low word
	MOV	[WORD PTR B$AC+2],DX	; Store Hi-word
	MOV	BX,DX		;Get a copy of Hi-word
	CWD			;Needed to check for overflow in SINT
	CMP	DX,BX		;If they are equal, then all is well
	POP	DX		;[DL] = valtype
	JE	FIN_EXIT	;No complaint
	CMP	DL,VT_I4	;User wanted a long integer?
	JE	FIN_EXIT	;Brif so - no error

FIN_OV: JMP	B$ERR_OV	;Overflow error

InValidNumber:
	MOV	DI,OFFSET DGROUP:B$DAC	 
	XOR	AX,AX		; Get a Zero to store
	STOSW			; Store it as an R8 (will be 0 for all
	STOSW			;  possible lengths)
	STOSW			
	STOSW			

FIN_EXIT:
	CALL	B$GETCH 	; provide return value

FIN_Str_Exit:			 
	FWAIT			; Always wait for 87 to complete.
cEnd				



	SUBTTL	B$STRSCAN
	PAGE
;***
;B$STRSCAN
;
;Purpose:
;	This routine will scan a zero terminated string for the
;	presence of a string input by the user.  If the first
;	character of the string is a double quote ("), then the
;	string is composed of all characters from the double quote
;	to the next double quote not including the quotes.  Otherwise
;	the string is composed of all the characters from the first
;	character until the first comma.
;
;	If the double quote or comma does not appear before the terminating
;	zero, then the zero byte is used as a legal terminator. On return,
;	[ES:SI] will point to the byte following the string seperator.	The
;	seperator, which is returned in AL, is determined as follows:
;
;	1.) If the string was terminated by a zero byte, then the
;	    seperator is the zero byte
;	2.) If the string was not quoted, then the seperator is
;	    the comma that ended the string
;	3.) If the string was quoted, then the seperator is the
;	    first non-blank character following the second double quote.
;
;
;Parameters:
;	[ES:SI]       = Pointer to text string to analyze
;
;Returns:
;	[ES:DX]       = updated pointer to start of string
;	[ES:SI]       = updated pointer to char after string seperator.
;	[CX]	      = Length of string (number of bytes in string)
;	[AL]	      = Seperator byte
;****

cProc	B$STRSCAN,<NEAR,PUBLIC>
cBegin

assumes DS,NOTHING		
assumes ES,NOTHING		
	PUSH	DS		
	PUSH	ES		; CAUTION: We switch DS to point to string
	POP	DS		; CAUTION: data references won't work.

	CALL	B$GETCH
	MOV	AH,AL		;In case first char is "
	CMP	AL,'"'		;Is it?
	JZ	COUNTSTR
	MOV	AH,","		;Terminate with comma if not quoted
	DEC	SI		;Scan first char again
COUNTSTR:
	MOV	DX,SI		;Save starting address of string data
	XOR	CX,CX		;Character count
RDSTR:
	LODSB			;get byte from DS:SI and increment SI
	CMP	AL,AH		;End of string?
	JZ	NEGCNT
	OR	AL,AL		;End of line?
	LOOPNZ	RDSTR		;Count char and loop if not EOL
	INC	CX		;Don't count the EOL character
	CMP	AH,','		;Quoted string?
	JE	NEGCNT		;  No
	DEC	SI		;  Yes - point back to 00 terminator
NEGCNT:
	NEG	CX		;Make character count positive
	CMP	AH,','		;Unquoted string?
	JNZ	GETRTN
;Not a quoted string. Trim off trailing blanks.
	DEC	SI		;Point back at termination character
	MOV	DI,SI
	DEC	DI		;Point to last char of string
	MOV	AL," "
	CMP	AL,AL		;Set zero flag in case CX=0
	STD			;Set direction DOWN
	REPE	SCASB		;Scan of blanks backward
	CLD			;Restore direction
	JZ	GETRTN		;If null string, leave zero in CX
	INC	CX		;Last char scanned not blank - count it
GETRTN:
	CALL	B$GETCH 	;Get Delimiter for return
	POP	DS		;Restore Data Segment
cEnd


page

;***
; B$GETCH, B$GETCH_NS - get next byte from buffer
;
;Purpose:
;	These routines will get the next byte from a zero terminated
;	string of characters.  B$GETCH will skip blank spaces, tabs, and
;	linefeeds, B$GETCH_NS will not.  If the character is a letter, it
;	will be converted to upper case.  If FV_DBCS and FK_FULLMAP
;	then any two byte characters will first be converted to their
;	one byte format if at all possible otherwise any double double
;	byte character will be returned one character at a time.
;
;	If FV_DBCS, then b$SISAVE will be set to the offset (from DS)
;	of the character that is being returned.  This is used by other
;	code to back up a single character.  It is possible for this routine
;	to return only the first half of a Double Byte Character.  This
;	will cause no problems, as it will always be invalid for the purpose
;	that it is used (this will desynchronize the character order
;	but everyone who gets a double byte character from here will die
;	with an error at one point or another).
;
;	NOTE: DS may not point to DGROUP!
;
;Entry:
;	DS:SI - points to a zero terminated string
;
;Exit:
;	AL - next legal character in the string
;	DS:SI - points to next character
;
;Uses:
;	SI is used as the index into a string.
;
;Preserves:
;	AH, BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$GETCH_NS,<PUBLIC,NEAR>
cBegin
	LODSB			;get byte from DATA statement segment
	JMP	SHORT UPPER	;continue on with processing in B$GETCH
cEnd	<nogen>

cProc	B$GETCH,<PUBLIC,NEAR>
cBegin

GET_ANOTHER:			
	LODSB			;get byte from DATA statement segment

	CMP	AL," "		;Ignore spaces
	JZ	GET_ANOTHER	
	CMP	AL,9		;Ignore tabs
	JZ	GET_ANOTHER	
	CMP	AL,10		;Ignore linefeeds
	JZ	GET_ANOTHER	
UPPER:
	CALL	B$UPCASE	;Convert to upper case
cEnd



assumes DS,DGROUP		
assumes ES,DGROUP		


sEnd	MT_TEXT

	SUBTTL	FAR_I8_INPUT - Far Interface to $i8_input
	PAGE
;***
;FAR_I8_INPUT - far interface to $i8_input
;
;Purpose:
; This interface routine is required because we can't be sure that the B$FIN
; portion of MT_TEXT will be within 64k of $i8_input in _TEXT, and $i8_input
; must be called NEAR
;
;Entry:
; [AX] = FORTRAN scale factor
; [BX] = Radix desired (1..36), integer only to be returned.
; [CX] = Maximum character count for scanning input stream.
; [DX] = FORTRAN decimal point format factor
; [DI] = DS offset of space for 8 byte result
; [SI] = points to first digit in number to input
; [BX] = radix desired, integer only to be returned (0 is default)
; [ES] = DS
;
;Exit:
; [SI] = DS offset of first byte after decoded string value
; [DI] = DS offset of 8-byte result
; [AX] = low order part of integer value (32-bit integer)
; [BX] = high order part of integer value (32-bit integer)
; [CX] = Return flags - - - defined elsewhere
;
; Outputs:
;
;****
sBegin	_TEXT			; $i8_input must be called NEAR from here
	ASSUMES CS,_TEXT

externNP $i8_input	       


FAR_I8_INPUT PROC    FAR
	CALL	$i8_input      ;mathpack call
	RET
FAR_I8_INPUT	ENDP

sEnd	_TEXT

	END
