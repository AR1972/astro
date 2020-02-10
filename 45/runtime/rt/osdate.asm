	TITLE	OSDATE - MS-DOS DATE$ and TIME$ Support
;***
; OSDATE - MS-DOS DATE$ and TIME$ Support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - DATE$ Statement:
;
;      DATE$ = datestring
;	 |
;      B$SDAT
;
;
; - DATE$ Function:
;
;      stringvar = DATE$
;		     |
;		   B$FDAT
;
;
; - TIME$ Function:
;
;      stringvar = TIME$
;		     |
;		   B$FTIM
;
;
; - TIME$ Statement:
;
;      TIME$ = string
;	 |
;      B$STIM
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	CONST		
	USESEG	_BSS
	USESEG	OS_TEXT 	
	USESEG	NH_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE files.inc	
	INCLUDE idmac.inc
	INCLUDE oscalls.inc
	INCLUDE string.inc	

sBegin	CONST			

C_GETDAT	EQU	42	;Get Date Function
C_SETDAT	EQU	43	;Set Date Function
C_GETTIM	EQU	44	;Get Time Function
C_SETTIM	EQU	45	;Set Time Function


sEnd	CONST			


sBegin	_BSS

sEnd	_BSS


sBegin	NH_TEXT 		
	externNP B$STDALCTMP
	externNP B$STALCTMP
sEnd	NH_TEXT 		

sBegin	OS_TEXT 		
	externNP B$ERR_FC

	assumes CS,OS_TEXT

	SUBTTL	B$SDAT - Set Date.
	PAGE

;***
; B$SDAT - Set Date
;
; Purpose:
;	Call DOS to set the system DATE
; Input:
;	sdDat -  is a string descriptor for the new date string
; Output:
;	NONE
; Modifies:
;	NONE
; Exceptions:
;	B$ERR_FC may be called
;****
cProc	B$SDAT,<PUBLIC,FAR>,<SI>
parmSD	sdDate			
cBegin
	MOV	BX,sdDate	;put date string desc in BX
	PUSH	BX
	MOV	SI,[BX+2]	;[SI] has addr of string.
	MOV	CX,[BX] 	; [CX] has string length
	JCXZ	DAT_ERR 	; Brif null str
	CALL	GETNUM		;Get 1 or 2 digit number
	MOV	DH,AH		;[DH] = month
	CALL	DATSEP
	CALL	GETNUM
	MOV	DL,AH		;[DL] = day
	CALL	DATSEP
	CALL	GETNUM
	MOV	BX,1900 	; [BX] = Bias in case only 2-digit year
	JCXZ	DATE2		; Brif End-of-string
	MOV	AL,100
	MUL	AH		;Mult decade by 100
	XCHG	BX,AX		; Save bias for year add
	CALL	GETNUM		;Get year (digits 3 & 4).
	JC	DAT_ERR		; jif only digit 3 was there (3-digit year)
DATE2:
	MOV	AL,AH
	MOV	AH,0
	ADD	BX,AX		; Add 2 digits of base to century

	XCHG	BX,CX		; [CX] = year

	CALLOS	SETDAT		;Give date to DOS.
$TIM_RET:
	OR	AL,AL		;Date (or Time) OK?
	JNZ	DAT_ERR 	;Brif not.
	POP	BX
	CALL	B$STDALCTMP	;delete the temp
cEnd

DAT_ERR:
	JMP	SHORT DATE_ERROR


	SUBTTL	B$FDAT - Get Date.
	PAGE

;***
; B$FDAT - Get Date
;
; Input:
;	NONE
; Output:
;	[AX] == ptr to string descriptor for country-specific date string
; Modifies:
;	BX
; Exceptions:
;	Could call $ERC_OM
;****
cProc	B$FDAT,<PUBLIC,FAR,FORCEFRAME>,SI 
cBegin
	MOV	BX,10
	CALL	B$STALCTMP	;Get space for 10 char string
	PUSH	BX		;Save String Descriptor
	PUSH	DX		;Save addr of string
	CALLOS	GETDAT		;Get Date from DOS
	POP	BX		;Restore addr of String
	MOV	AL,DH
	CALL	PUTCHR		;Store ascii month
	MOV	AL,"-"
	CALL	PUTCH2
	MOV	AL,DL
	CALL	PUTCHR		;Store ascii day
	MOV	AL,"-"
	CALL	PUTCH2
	SUB	CX,1900 	;Reduce year by two digits
	CMP	CL,100		;See if in 20th century
	MOV	CH,19		;Setup 20th century in case
	JB	DATEF2		;Brif so.
	SBB	CL,100		;subtract into next century
	INC	CH		;21st century
DATEF2:
	MOV	AL,CH
	CALL	PUTCHR		;Store ascii century
	MOV	AL,CL
	CALL	PUTCHR		;Store ascii year.
	POP	AX		;return descriptor in AX
cEnd

DATE_ERROR:
	JMP	B$ERR_FC	;Complain


	SUBTTL	B$STIM - Set Time.
	PAGE
;***
; B$STIM - Set Time
;
; Input:
;	sdTime == string descriptor for time string to set
; Output:
;	NONE
; Modifies:
;	NONE
; Exceptions:
;	B$ERR_FC could be called
;****
cProc	B$STIM,<PUBLIC,FAR>,<SI>
parmSD	sdTime			
cBegin
	MOV	BX,sdTime	;[BX] = time string desc.
	PUSH	BX
	MOV	SI,[BX+2]	;[SI] has addr of string.
	MOV	CX,[BX] 	; [CX] has string length
	JCXZ	DATE_ERROR	; Brif null str
	CALL	GETNUM		;Get 1 or 2 digit number
	MOV	BH,AH		; [BH] = hours
	CALL	TIMSEP
	CALL	GETNUM
	MOV	BL,AH		; [BL] = minutes
	CALL	TIMSEP
	CALL	GETNUM
	MOV	DH,AH		;[DH] = seconds.
	XOR	DL,DL		;Zero 100ths.
	XCHG	BX,CX		; [CX] = hours/minutes
	CALLOS	SETTIM		;Give Time to DOS.
	JMP	$TIM_RET	;Check Time Ok & Exit.
cEnd	<nogen>

	SUBTTL	B$FTIM - Get Time.
	PAGE
;***
; B$FTIM - Get Time
;
; Input:
;	NONE
; Output:
;	[AX] == ptr to time string obtained from DOS
; Modifies:
;	NONE
; Exceptions:
;	Can call $ERC_OM
;****
cProc	B$FTIM,<PUBLIC,FAR,FORCEFRAME>,SI	
cBegin
	MOV	BX,8
	CALL	B$STALCTMP	;Get space for 8 char string
	PUSH	BX		;Save String Descriptor
	PUSH	DX		;Save addr of string
	CALLOS	GETTIM		;Get Time from DOS
	CMP	DL,50
	JB	TIMEF2		;Brif .lt. 1/2 sec
	INC	DH		;Bump secs.
	CMP	DH,60
	JB	TIMEF2		;Brif no ovf
	MOV	DH,0
	INC	CL		;Bump mins.
	CMP	CL,60
	JB	TIMEF2		;Brif no ovf
	MOV	CL,0
	INC	CH		;Bump hrs.
TIMEF2:
	POP	BX		;Restore addr of String
	MOV	AL,CH
	CALL	PUTCHR		;Store ascii hours
	MOV	AL,":"
	CALL	PUTCH2
	MOV	AL,CL
	CALL	PUTCHR		;Store ascii minutes
	MOV	AL,":"
	CALL	PUTCH2
	MOV	AL,DH
	CALL	PUTCHR		;Store ascii seconds.
	POP	AX		;return psd to time string in AX
cEnd

TIME_ERROR:
	JMP	DATE_ERROR	;Complain

	SUBTTL	$DATE and $TIME Utility Subroutines
	PAGE

	PAGE
;***
;DATTSEP - Searches for valid date seperator ( '-' or '/')
;Purpose:
;	Checks next char in search string for a valid date seperator.
;Entry:
;	CX - char count for search string
;	SI - pointer to search string
;Exit:
;	CX - count of chars left in search string
;	SI - points to next char in string
;	AL - date seperator char
;Uses:
;	SI
;Exceptions:
;	B$ERR_FC - if end of string or next char wasn't a valid seperator.
;****
cProc	DATSEP,<NEAR>
cBegin
	JCXZ	TIM_ERR 	; error if string empty
	DEC	CX		; Length -1
	LODSB
	CMP	AL,"/"
	JZ	GOT_DATSEP
	CMP	AL,"-"
	JNZ	TIM_ERR
GOT_DATSEP:
cEnd

	PAGE
;***
;TIMSEP - Searches for valid time seperator ( ':' or '.')
;Purpose:
;	Checks next char in search string for a valid time seperator.
;Entry:
;	CX - char count for search string
;	SI - pointer to search string
;Exit:
;	CX - count of chars left in search string
;	SI - points to next char in string
;	AL - time seperator char
;Uses:
;	SI
;Exceptions:
;	B$ERR_FC - if next char wasn't a valid seperator.
;****
cProc	TIMSEP,<NEAR>
cBegin
	JCXZ	GOT_TIMSEP	
	DEC	CX
	LODSB
	CMP	AL,":"
	JZ	GOT_TIMSEP
	CMP	AL,"."
	JNZ	TIM_ERR
GOT_TIMSEP:
cEnd

TIM_ERR:
	JMP	TIME_ERROR

	PAGE
;***
;GETNUM - Gets an ascii number, 1 or 2 characters long, and converts
;	  it to an integer.
;
;Purpose:
; Gets the next 1 or 2 ascii chars from string in SI and converts
; them to an unsigned intger which is returned in AH.  It is also returned in
; AL.
;
;Entry:
; [CX]	= char count for search string
; [SI]	= pointer to search string
;
;Exit:
; [AH]	= integer representation of ascii digit
; [CX]	= count of chars left in search string
; [SI]	= points to next char in string
; PSW.C set if only one character read, clear if two characters read
;
;Uses:
; Per convention
;
;Preserves:
; BX
;
;Exceptions:
; B$ERR_FC - if next char wasn't a valid digit.
;
;******************************************************************************
cProc	GETNUM,<NEAR>
cBegin
	CALL	DIGIT
	JB	TIM_ERR 	;required to handle relative jump
				;beyond 128 bytes
	MOV	AH,AL
	STC			; default: only one digit
	JCXZ	GETNUX		; Number complete if end of string.
	CALL	DIGIT
	JB	GETNUX
	AAD			;Convert BCD to Binary
	MOV	AH,AL
	CLC			; PSW.C clear for 2 characters read
GETNUX:
cEnd

	PAGE
;***
;DIGIT - Gets an ascii character and checks if it is in the range '0' - '9'
;
;Purpose:
; Gets the next ascii char from string in SI and checks to see if it is a
; character representation of a valid digit. If valid then the char is
; converted to BCD representation of the digit. Returns 0 if no chars are left
; in the string.
;
;Entry:
; [CX]	= char count for search string
; [SI]	= pointer to search string
;
;Exit:
; [AL]	= BCD representation of ascii digit
; [CX]	= count of chars left in search string
; [SI]	= points to next char in string
;
;Uses:
; Per convention
;
;Preserves:
; BX
;
;******************************************************************************
cProc	DIGIT,<NEAR>
cBegin
	MOV	AL,CL		; Init to 0 if string exhausted
	JCXZ	DIGIX		; Brif so, returns 0
	MOV	AL,[SI]
	SUB	AL,"0"		
	JB	DIGIX
	CMP	AL,10
	CMC
	JB	DIGIX
	DEC	CX		;Length -1
	INC	SI
DIGIX:
cEnd

	PAGE
;***
;PUTCHR - Converts a packed BCD number in AX into a char string and puts in BX
;Purpose:
;	Converts the packed BCD number into an ascii character string which
;	is then placed into the buffer pointed to by [BX].  BX is updated
;	to point past the added chars.
;Entry:
;	AX - pack BCD number
;	BX - buffer to place string into
;Exit:
;	BX - points past added chars
;Uses:
;	None.
;Exceptions:
;	None.
;****
cProc	PUTCHR,<NEAR>
cBegin
	AAM			;Convert to unpacked BCD
	OR	AX,"00" 	;Add "0" bias to both digits.
	MOV	[BX],AH 	; store char in string
	INC	BX
PUTCH2:
	MOV	[BX],AL 	; store char in string
	INC	BX
cEnd

sEnd	OS_TEXT 		
	END
