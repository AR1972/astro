	TITLE	STRFCN - String function package
	PAGE	56,132
;***
; STRFCN - String function package
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	BASIC intrinsic string function support.
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; INSTR Function - calls B$INS3 if optional parameter given, otherwise B$INS2:
;
;   v = INSTR(5, a$, b$)  v = INSTR (x$, y$)
;	   |			 |
;	B$INS3		      B$INS2
;
;
; LEFT$ Statement:	LEN Function:		LSET Statement:
;
;   v$ = LEFT$(x$,n)	  v = LEN(x$)		  LSET v$ = x$
;	  |		       |		       |
;	B$LEFT		    B$FLEN		     B$LSET
;
;
; MID$ Function:	RIGHT$ Function:	SPACE$ Function:
;
;   v$ = MID$(x$,n,[m])   v$ = RIGHT$(x$,n)	  v$ = SPACE$(n)
;	  |			|			 |
;      B$FMID		     B$RGHT		      B$SPAC
;
;
; STRING$ Function - Two possible syntaxes map to two runtime entry points:
;
;   v$ = STRING$(n,m)	  v$ = STRING$(n,x$)
;	    |			 |
;	B$STRI		      B$STRS
;
;
; VARPTR$ Function:-	       ASC Function:
;
;   v$ = VARPTR$(a)		 v = ASC(string)
;	    |			       |
;	B$VARP(addr, type)	    B$FASC
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_DATA		
	useSeg	NH_TEXT 	
	useSeg	ST_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE devdef.inc
	INCLUDE const.inc	

sBegin	_DATA

externW b$nuldes 		;[6]

sEnd	_DATA			

sBegin	ST_TEXT 		
assumes CS,ST_TEXT		

externNP B$STALCTMP		
externNP B$STALCTMPSUB		

externNP B$ERR_FC		
externNP B$ERR_FC		
externNP B$STDALCTMP		


	SUBTTL	B$SADD & B$FLEN - get address & length of string
	PAGE
;***
;B$SADD - get address of string
;char * pascal B$SADD(sd *psd)
;
;Function:
; Implement SADD function by returning the address of the specified string.
;
;Inputs:
; psd	= ptr to sdesc
;
;Outputs:
; [AX]	= integer address (0 if null string)
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$SADD,<FAR,PUBLIC>	
parmW	psd			; Pointer to sd to get address from
cBegin				
	MOV	BX,psd		; [BX] = pointer to sd
	MOV	CX,[BX] 	; [CX] = length of string
	JCXZ	FLEN_10 	; If zero length, just return 0
	PUSH	[BX+2]		; Put address on stack
	JMP	SHORT FLEN_20	; go share some code
cEnd	nogen			

;***
;B$FLEN - Compute LEN function
;I2 pascal B$FLEN(sd *psd)
;
;Inputs:
; psd	= Address of string descriptor
;
;Outputs:
; [AX]	= Value of LEN function
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$FLEN,<FAR,PUBLIC>	
parmW	psd			
cBegin				
	MOV	BX,psd		; get string descriptor
FLEN_10:
	PUSH	[BX]		;Put length on stack
FLEN_20:			
	CALL	B$STDALCTMP	;Delete if temp string
	POP	AX		; [AX] has length/addr
FLEN_90:			
cEnd				

	SUBTTL	B$FASC - Compute ASC function
	PAGE
;***
;B$FASC - Compute ASC function
;I2 pascal B$FASC(sd *psd)
;DBCS-callback
;
;Purpose:
;	This routine is used to support the ASC() function.
;
;Entry:
;	psd   = Address of string descriptor
;
;Exit:
;	[AX]  = Value of ASC function
;
;Uses:
;	Per convention
;
;Preserves:
;	None.
;
;Exceptions:
;	B$ERR_FC for an empty string or illegal double byte character.
;******************************************************************************
cProc	B$FASC,<FAR,PUBLIC>,SI 
parmW	psd			
cBegin				
	MOV	BX,psd		
	CMP	WORD PTR[BX],0	;See if nul string
	JZ	ERRFC


	MOV	SI,[BX+2]	;Get pointer to string data
	MOV	SI,[SI] 	;Save first char of string
	CALL	B$STDALCTMP	;Delete if temp string
	XCHG	AX,SI		; [AL] = first character of string


ONEBYT: XOR	AH,AH		; [AX] = character
TWOBYT:

cEnd				
ERRFC:	JMP	B$ERR_FC	  ;Report illegal function call


	SUBTTL	B$INS3 - INSTR function for three arguments
	PAGE
;***
;B$INS3 - INSTR function for three arguments
;I2 pascal B$INS3(I2 start, sd *psdSource, sd *psdMatch)
;
;Function:
; Perform INSTR(I,A$,B$).
;
;Inputs:
; start     = Starting position in A$ of search (I)
; psdSource = Address of descriptor of string to be searched (A$)
; psdMatch  = Address of search string descriptor (B$)
;
;Outputs:
; [AX]	    = Position of B$ in A$; zero if not found.
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$INS3,<FAR,PUBLIC>,<DI,SI,ES> 
parmW	start			
parmW	psdSource		
parmW	psdMatch		
cBegin				
	MOV	BX,start	
	OR	BX,BX
	JLE	ERRFC		

	MOV	DI,psdSource	;Get A$ descriptor where we can address with it
	MOV	SI,psdMatch	;Get B$ descriptor where we can address with it
	LODSW			;Length of B$
	XCHG	AX,DX		;Length of B$ in DX
	MOV	SI,[SI] 	;Address of B$ data
	MOV	CX,[DI] 	;Length of A$
	JCXZ	NOTFND		;IF	LEN(A$) = 0  - return zero
	DEC	DX		;DX = LEN(B$)-1
	JNS	instr1		;  B$ is not null string
	CMP	BX,CX		;Offset : LEN(A$)
	JG	NOTFND		;  > - return 0
	JMP	short EXITFND	; <= - return Offset
instr1:
	MOV	DI,[DI+2]	;Address of A$ data
	DEC	BX		;Starting offset in A$
	ADD	DI,BX		;Add in starting offset
	SUB	CX,DX		;Last pos. search position
	XCHG	AX,BX		;AX = offset
	MOV	BX,CX		;Save in BX
	SUB	CX,AX		;Number of 1st chars to search
	JLE	NOTFND		;If search string won't fit, say not found
	LODSB			;Get first character of search string

; Top of search loop. The following conditions exist here:
;	AL = First character of search string (B$).
;	BX = Last position in A$ for 1st char match
;	CX = Amount of A$ left for first-character scan.
;	DX = Length of B$-1, used for string compares.
;	SI = Address of B$+1, used for string compares.
;	DI = Current position to scan in A$.

SEARCH:
	PUSH	DS		
	POP	ES		; [ES] = [DS]
	JCXZ	NOTFND		;If nothing left in A$, not found
	REPNE	SCASB		;Scan for first letter
	JNZ	NOTFND		;Was first letter found?

;Found a match in first letter. Do string compare on remaining letters

	PUSH	CX		;Save condition should compare be unsuccessful
	PUSH	SI		;  and we need to continue first-character
	PUSH	DI		;  search.
	MOV	CX,DX		;Get length of B$-1 for string compare
	REPE	CMPSB
	POP	DI
	POP	SI
	POP	CX
	JNZ	SEARCH		;If it didn't match, just continue searching
	SUB	BX,CX		;Compute position of match from start of string
EXITFND:
	XCHG	BX,DX		;[DX] = result return
	MOV	BX,psdSource	
	CALL	B$STDALCTMP	;Get rid of temporary strings
	MOV	BX,psdMatch	
	CALL	B$STDALCTMP	
	XCHG	AX,DX		; [AX] = result

cEnd

NOTFND:
	XOR	BX,BX
	JMP	EXITFND

	SUBTTL	B$INS2 - INSTR function with 2 arguments
	PAGE
;***
;B$INS2 - INSTR function with 2 arguments
;I2 pascal B$INS2(sd *psdSource, sd *psdMatch)
;
;Function:
; Perform INSTR(A$,B$).
;
;Inputs:
; psdSource = Address of descriptor of string to be searched (A$)
; psdMatch  = Address of search string descriptor (B$)
;
;Outputs:
; [AX]	    = Position of B$ in A$; zero if not found.
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$INS2,<FAR,PUBLIC>	
parmW	psdSource		
parmW	psdMatch		
cBegin				
	MOV	BX,1		;Start with position 1
	cCall	<FAR PTR B$INS3>,<BX,psdSource,psdMatch>	
cEnd				

	SUBTTL	B$LEFT & B$RGHT - Compute LEFT$ & RIGHT$ functions
	PAGE
;***
;B$LEFT - Compute LEFT$ function
;sd * pascal B$LEFT(sd * pas, I2 len)
;
;Function:
; Return a string made up of the leftmost ($LEF) characters of the source
; string. If the requested length is less than zero, then give error; if
; greater than the length of the source string, the source string itself is
; returned; otherwise, a temporary string is created.
;
; NOTE: The characters are assumed to be 1 byte characters.  It is possible
;	for the user to split a KANJI character with this function.
;
;Inputs:
; psd	= Address of string descriptor
; len	= Length of string wanted
;
;Outputs:
; [AX]	= Address of result string descriptor
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$LEFT,<FAR,PUBLIC>	
parmW	psd			
parmW	len			
cBegin				
	XOR	CX,CX		;Start at left end
	MOV	BX,psd		
	MOV	DX,len		
	JMP	SHORT RIG1	;Go to common code
cEnd	nogen			

;***
;B$RGHT - Compute RIGHT$ function
;sd * pascal B$RGHT(sd * pas, I2 len)
;
;Function:
; Return a string made up of the rightmost ($RIG) characters of the source
; string. If the requested length is less than zero, then give error; if
; greater than the length of the source string, the source string itself is
; returned; otherwise, a temporary string is created.
;
; NOTE: The characters are assumed to be 1 byte characters.  It is possible
;	for the user to split a KANJI character with this function.
;
;Inputs:
; psd	= Address of string descriptor
; len	= Length of string wanted
;
;Outputs:
; [AX]	= Address of result string descriptor
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$RGHT,<FAR,PUBLIC>	
parmW	psd			
parmW	len			
cBegin				
	MOV	BX,psd		
	MOV	DX,len		
	MOV	CX,[BX] 	;Get length
	SUB	CX,DX		;Starting point (from left)
RIG1:
	CALL	LEFRIG
cEnd				

ARGER2: JMP	B$ERR_FC	  ;Clean stack and report illegal function call

	SUBTTL	B$FMID - Compute MID$ function
	PAGE
;***
;B$FMID - Compute MID$ function
;sd * pascal B$FMID(sd *psdSource, I2 iStart, I2 cbLen)
;
;Function:
; If (iStart < 1) or (cbLen < 0) then give error; if (iStart = 1) and
; (cbLen >= length of source string) then return source string; otherwise
; create a temp string of length CX with the specified portion of the source
; string.
;
; NOTE: The characters are assumed to be 1 byte characters.  It is possible
;	for the user to split a KANJI character with this function. See the
;	MID\ (MID yen) function for a version that works with double byte
;	characters.
;
;Inputs:
; psdSource = Address of string descriptor
; iStart    = Starting offset in string (1 is first)
; cbLen     = Length of string desired
;
;Outputs:
; [AX] = Address of result string descriptor
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$FMID,<FAR,PUBLIC>	
parmW	psdSource		
parmW	iStart			
parmW	cbLen			
cBegin				

	MOV	BX,psdSource	
	MOV	CX,cbLen	
	OR	CX,CX		;test string length
	JL	ARGER2		;error if negative
	MOV	DX,iStart	
	DEC	DX		;Offset should start at zero, not 1
	XCHG	DX,CX		;Set up for future call
	JL	ARGER2		;Error if now negative
	JNZ	MID_10		
	CALL	LEFRIG		; Offset is zero, so treat just like LEFT$
	JMP	SHORT MID_90	
MID_10: 			
	MOV	AX,[BX] 	;Get length of string
	SUB	AX,CX		;Reduce length by the offset
	JNLE	MID_20		
	CALL	B$STDALCTMP	;Delete if temp string
	MOV	AX,OFFSET DGROUP:b$nuldes ; Null string descriptor
	JMP	SHORT MID_90	
MID_20: 			
	CMP	AX,DX		;Need smaller of request and amount remaining
	JGE	MIDLEN
	MOV	DX,AX		;Limited by length of string
MIDLEN:
	CALL	B$STALCTMPSUB	;Create temp string from descriptor
	XCHG	AX,BX		; [AX] has return value
MID_90:

cEnd				


;***
; LEFRIG - support routine
;
;Purpose:
; This subroutine is used for $LEF, $RIG, and some $MID.
;
;Entry:
; [BX]	 = Address of string descriptor
; [DX]	 = Length requested
; [CX]	 = Starting offset
;
;Exit:
; [AX]	 = new descriptor
;
;Uses:
; CX   - only for special RIGHT$ case
;
;Preserves: (optional)
;
;Exceptions:
;
;******************************************************************************
cProc	LEFRIG,NEAR
cBegin

	OR	DX,DX		;Test requested length
	JL	ARGERR		;If negative, give error
	JNZ	LEFRIG_10	;If zero, return null string
	CALL	B$STDALCTMP	;ensure we get rid of any temps
	MOV	AX,OFFSET DGROUP:b$nuldes 
	RET
LEFRIG_10:
	CMP	DX,[BX] 	;See if requested length is shorter than string
	JLE	MAKTMP		;  Yes - create temp string
	MOV	DX,[BX] 	;  No - use actual string length
	OR	CX,CX		;If offset is negative from RIGHT$
	JGE	MAKTMP		;  No - OK
	XOR	CX,CX		;  Yes - just make it zero
MAKTMP:
	CALL	B$STALCTMPSUB	;Create temp from string descriptor
	XCHG	AX,BX		; [AX] = new descriptor
cEnd				


	SUBTTL	B$SPAC - SPACE$ function
	PAGE
;***
;B$SPAC - SPACE$ function
;sd * pascal B$SPAC(I2 cbDesired)
;
;Function:
; Produce temporary string with a blank (' ') character repeated BX times.
;
;Inputs:
; cbDesired = Length of string desired
;
;Outputs:
; [AX]	= Address of result temp string descriptor
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$SPAC,<FAR,PUBLIC>	
parmW	cbDesired		
cbegin

	MOV	DL," "		;Provide a space to be repeated
	MOV	BX,cbDesired	; [BX] = count
	CALL	STRING		; [AX] = pointer to sd

cEnd

;***
; STRING - Create a temp string of length n containing something.
;
;Purpose:
;	Creates a temp string of n characters, where character is specified.
;	If we are compiled with KANJI enabled, DL contains the first byte
;	of the character to go into the string.  DH contains the second byte
;	unless it is 0 in which case it is a single byte character.
;
;Entry:
; [DL]	= Character (DX for kanji)
; [BX]	= desired length
;
;Exit:
; [AX]	= pointer to temp sd
;
;Uses:
; Per convention
;
;******************************************************************************
cProc	STRING,NEAR,ES
cBegin
	MOV	AX,OFFSET DGROUP:b$nuldes ;[7]
	OR	BX,BX		;Check character count
	JZ	STRING_90	; Return null string if zero
	JL	ARGERR		;Error if negative


NOTKAN: MOV	CX,BX		;Put count in count register
	MOV	AL,DL		;Character to copy
	MOV	AH,DL		;Repeat it by words for speed
	CALL	B$STALCTMP	;Allocate string
	INC	CX		;Round up to whole word if odd
ISKAN:	XCHG	DI,DX		;Get address of data in destination register
	SHR	CX,1		;Divide byte count by 2 for word move
	PUSH	DS		
	POP	ES		; [ES] = [DS]
	REP	STOSW
	MOV	DI,DX		;Restore DI
	XCHG	AX,BX		;[AX] has string desc
STRING_90:
cEnd

ARGERR: JMP	B$ERR_FC	  ;Clean stack and report illegal function call

	SUBTTL	B$STRI & B$STRS - STRIN& functions
	PAGE
;***
;B$STRI & B$STRS - STRING$ functions
;sd * pascal B$STRI(I2 cbdesired, I2 ascCode)
;sd * pascal B$STRS(I2 cbDesired, sd *psdChar)
;
;Function:
; Produce temporary string with a single character repeated BX times. B$STRI
; provides the character to be repeated, and B$STRS provides a string
; descriptor, the first character of which is to be repeated.
;
;Inputs:
; cbDesired = Length of string desired
; ascCode   = Character to repeat (B$STRI only)
; psdChar   = Descriptor of string with character to repeat (B$STRS only)
;
;Outputs:
; [AX]	= Address of result temp string descriptor
;
;Registers:
; Per convention
;
;******************************************************************************
cProc	B$STRI,<FAR,PUBLIC>	
parmW	cbDesired		
parmW	ascCode 		
cBegin

	MOV	DX,ascCode	
	JMP	SHORT STRI_10	
cEnd	nogen

cProc	B$STRS,<FAR,PUBLIC>	
parmW	cbDesired		
parmW	psdChar 		
cBegin

	MOV	BX,psdChar	; Put descriptor where we can address with it
	CMP	WORD PTR[BX],0	;Length of string must not be zero
	JZ	ARGERR
	PUSH	BX		
	MOV	BX,[BX+2]	;Get address of string data
	POP	AX		; [AX] = pointer to descriptor

	PUSH	[BX]		; Push first character in string
	XCHG	AX,BX		; [BX] = pointer to descriptor
	CALL	B$STDALCTMP	;Delete if temp string
	POP	DX		; [DX] = character
STRI_10:			
	MOV	BX,cbDesired	; [BX] = length to do it
	CALL	STRING		; [AX] = pointer to sd
cEnd

	SUBTTL	B$VARP - VARPTR$ function
	PAGE

;***
;B$VARP - VARPTR$ function
;sd * pascal B$VARP(U2 addr, I2 type)
;
;Function:
; Create a 3 byte string of following:
;	Byte 1		contains variable type, one of:
;		2 - Integer
;		3 - String
;		4 - Single
;		8 - Double
;	       14h- Long
;	Byte 2 & 3	contain the address of the variable
;
;Inputs:
; addr	= Address of Variable
; typ	= type of the variable
;
;Outputs:
; [AX]	= Address of String Descriptor
;
;Registers:
; Per convention
;******************************************************************************
cProc	B$VARP,<FAR,PUBLIC>	
parmW	addr			
parmW	typ			
cBegin				
	MOV	BX,3
	CALL	B$STALCTMP	;Get a 3 byte string
	XCHG	BX,DX		;[BX] has its address
	MOV	AX,typ		
	MOV	[BX],AL 	
	MOV	AX,addr 	
	MOV	[BX+1],AX	
	XCHG	AX,DX		; [AX]= String Descriptor

cEnd



	SUBTTL	B$LCAS/B$UCAS - convert string to lower/upper case
	PAGE
;***
; B$UCAS - Convert string to upper case
; B$LCAS - Convert string to lower case
;
;Purpose:
; String case conversion.
;
;Entry:
; psd	= Pointer to string descriptor to be converted
;
;Exit:
; [AX]	= Pointer to string descriptor of result
;
;Uses:
; Per convention
;
;******************************************************************************
cProc	B$UCAS,<FAR,PUBLIC>
cBegin
	MOV	AX,'za'
	JMP	SHORT	CASE_5	;go perform case conversion
cEnd	nogen

cProc	B$LCAS,<FAR,PUBLIC>
cBegin
	MOV	AX,'ZA'
cEnd	nogen			

cProc	CASE_5,FAR,<DI,SI,ES>	
parmW	psd			
cBegin				

	MOV	BX,psd		;get string descriptor
	MOV	DX,[BX] 	;(DX) = length of string to convert
	OR	DX,DX		;is this a null string ?
	JZ	uca_ret 	;yes, just return it
	XOR	CX,CX
	CALL	B$STALCTMPSUB	;no, copy string to temporary
	PUSH	BX
	XCHG	CX,DX		;(CX) = length of string to convert
	MOV	SI,[BX+2]	;(SI) = ptr to string data
	MOV	DI,SI		;ditto for DI
	XCHG	AX,BX		;(BX) = chars to check
	PUSH	DS		; set ES=DS
	POP	ES
uca_loop:
	LODSB			;fetch next char from string
	CMP	AL,BL
	JB	uca_cont	;ignore if not wrong case
	CMP	AL,BH
	JA	uca_cont	;ignore if not wrong case
	XOR	AL,20H		;flip upper case bit
uca_cont:
	STOSB			;put character back
	LOOP	uca_loop	;repeat until entire string is checked
uca_abort:
	POP	BX		;restore descriptor address
uca_ret:
	XCHG	AX,BX		;return sd in AX

cEnd

	SUBTTL	$TRL/$TRM/$TRR - Trim blanks off string
	PAGE
;***
; B$TRIM, B$LTRM, B$RTRM - Trim family
;
;Purpose:
; Trim leading, trailing or both blanks from a string.
;
;Entry:
; psd	= Pointer to string descriptor to be converted
;
;Exit:
; [AX]	= Pointer to string descriptor of result
;
;Uses:
; Per convention
;
;******************************************************************************
TR_LEFT= 1
TR_RIGHT= 2

; cProc	B$TRIM,<PUBLIC,FAR>
; cBegin
; 	MOV	AH,TR_LEFT+TR_RIGHT
; 	SKIP	2		; fall into TRIM
; cEnd	nogen

cProc	B$LTRM,<PUBLIC,FAR>
cBegin				
	MOV	AH,TR_LEFT
	SKIP	2		; fall into TRIM
cEnd	nogen

cProc	B$RTRM,<PUBLIC,FAR>
cBegin				
	MOV	AH,TR_RIGHT
cEnd	nogen			; fall into TRIM

cProc	TRIM,FAR,<DI,ES>	
parmW	psd			
cBegin				

;When scanning for a blank character, we know that we will never take
;any part of a KANJI character as a space, as this value is illegal in
;a KANJI.

	PUSH	DS		; set es=ds
	POP	ES		
	MOV	BX,psd		;Get source SD
	MOV	CX,[BX] 	; get string length
	JCXZ	trim_ret	;   null string - return
	MOV	DI,[BX+2]	; (DI) = address of string contents
	MOV	AL,' '		; AL = blank - trim char
	TEST	AH,TR_LEFT	; trim left blanks?
	JZ	trim_right	;   no - trim right
trim_left:			
	REPE	SCASB		; scan blanks off front
	JZ	trim_done	;   all blanks

	INC	CX		; backup one character
	DEC	DI

trim_right:
	MOV	DX,DI		; (DX) = 1st nonblank address
	SUB	DX,[BX+2]	; (DX) = offset of 1st nonblank
	TEST	AH,TR_RIGHT	; trim right blanks?
	JZ	trim_done	;   no - done
	ADD	DI,CX
	DEC	DI		; (DI) = last char in string
re_trim_right:
	STD			; direction is down
	REPE	SCASB		; scan blanks off back
	CLD			; direction is up
	JZ	trim_done	;   all blanks
	INC	CX		; backup one character

trim_done:
	XCHG	CX,DX		; CX=offset , DX=length , BX=sdesc
	CALL	B$STALCTMPSUB	; copy into temp string

trim_ret:
	XCHG	AX,BX		;Return sd in AX
cEnd

sEnd	ST_TEXT

	END
