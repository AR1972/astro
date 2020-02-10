	TITLE	MCLPRC - MACRO LANGUAGE DRIVER
;***
; MCLPRC - MACRO LANGUAGE DRIVER
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************


	INCLUDE switch.inc
	INCLUDE	rmacros.inc	; Runtime Macro Definitions

	useSeg	_DATA		
	useSeg	_BSS		
	useSeg	MT_TEXT		
	useSeg	RT_TEXT		
	useSeg	NH_TEXT 	

	INCLUDE	seg.inc		
	INCLUDE	rtps.inc	
	INCLUDE string.inc	
	INCLUDE idmac.inc	

sBegin	_DATA			

	externW	B$AC		; floating-point accumulator
	externW	B$DAC		; used as aux. fp. acc.

	externW	b$pendchk	

sEnd	_DATA			

sBegin	_BSS			


	externW	B$MCLLEN	; defined in GWDATA.ASM
	externW	B$MCLPTR	; defined in GWDATA.ASM
	externW	B$MCLTAB	; defined in GWDATA.ASM
	externW	B$MCLPSD	; defined in GWDATA.ASM

	externB	b$VTYP		; defined in IODATA.ASM


sEnd	_BSS			



sBegin	RT_TEXT

	ASSUME	CS:CODE,DS:DGROUP,ES:DGROUP,SS:DGROUP ; Fix this up

;MICROSOFT GRAPHICS AND SOUND MACRO LANGUAGES
;
;MCLPRC -      Is the Basic (8086) Macro Language Driver for
;	       Music (MML) and Graphics (GML).
;
;--------- --- ---- ---- ---- -- ---------
;COPYRIGHT (C) 1982 1983 1988 BY MICROSOFT
;--------- --- ---- ---- ---- -- ---------
;
;Written by:	 M Courtney	 Microsoft
;Reorganized by: T Corbett	 Microsoft  Feb. 25, 1983
;Revised by:	 K Simonsen	 Microsoft  Jan. 12, 1988
;

	.SALL
	.RADIX	10

	externNP	B$UPCASE	

	externNP	B$ERR_FC
	externNP	B$ERR_TM
	externNP	B$ERR_OSS
	externNP	B$STDALCTMP	
	externNP	B$FRCINT

;***
;B$MACLNG
;
;Purpose:
;	Parses the Macro Text String and execute
;	commands from the Command Table (passed in [DX])
;	until the string is exhausted.	Some connectives
;	are eaten (+,-,;) and lower case converted to upper
;	case.  Signed numeric arguments are converted to
;	16 bit integer.
;
;Entry:
;	[DX] = Pointer to the Macro Command Table (CS relative).
;	[BX] = Pointer to descriptor for macro language string
;
;Exit:
;	String is parsed.
;
;Uses:
;	Per convention.
;
;****
cProc	B$MACLNG,<PUBLIC,NEAR>
cBegin
	MOV	WORD PTR B$MCLTAB,DX ;SAVE POINTER TO COMMAND TABLE
	MOV	B$AC,BX		;B$AC:=ptr to string descriptor
	PUSH	BX		; save ptr to string descriptor
	CALL	MACSCN		;scan contents of string, dispatching to
				; routines in B$MCLTAB to carry out work.
	POP	BX		; [BX] = ptr to string descriptor
	CALL	B$STDALCTMP	; compiler temp string deallocator
RET01:				; used as a NEAR RET
cEnd

;***
;MACSCN
;Purpose:
;	Parses entire Macro Text String pointed to by [B$AC].
;	Dispatches to routines for commands defined in the Command Table
;	pointed to by B$MCLTAB until string is exhausted.
;Input:
;	[B$MCLTAB] = Ptr to the Macro Command Table (CS relative).
;	BX = Ptr to string descriptor.
;Outputs:
;	[B$MCLPTR] = Ptr beyond end of string
;	[B$MCLLEN] = 0
;****
MACSCN:
	CALL	B$SETMCL 	;set B$MCLPTR, B$MCLLEN based on string
				;in FAC
MACSCL:
	CMP	[B$MCLLEN],0	;until entire string is consumed
	JE	RET01		;return if string is empty
	CALL	B$MACCMD 	;comsume and dispatch to 1 command in string
	JMP	SHORT MACSCL

;***
;B$MACCMD
;
;Purpose:
;	Parses one command in the Macro Text String pointed to by B$MCLPTR
;	which is B$MCLLEN bytes in length.
;	Dispatches to routines for commands defined in the Command Table
;	pointed to by B$MCLTAB until the string is exhausted.
;
;Entry:
;	[B$MCLTAB] = Ptr to the Macro Command Table (CS relative).
;	[B$MCLPTR] = Ptr to 1st byte of string to be interpreted (DS relative).
;	[B$MCLLEN] = Length of string to be interpreted.
;
;Exit:
;	[B$MCLPTR] = Ptr beyond command which was interpreted.
;	[B$MCLLEN] = Updated length of string (after command is consumed).
;
;Uses:
;	Per convention.
;
;****
cProc	B$MACCMD,<PUBLIC,NEAR>
cBegin
	CALL	B$FETCHR 	;GET A CHAR FROM STRING
	JZ	MSCNX		;END OF STRING - Exit
	ADD	AL,AL		;PUT CHAR * 2 INTO [CL]
	MOV	CL,AL
	MOV	BX,B$MCLTAB	;POINT TO COMMAND TABLE
MSCNLP:
	MOV	AL,BYTE PTR CS:[BX] ;GET CHAR FROM COMMAND TABLE
	ADD	AL,AL		;CHAR = CHAR * 2 (CLR HI BIT FOR CMP)
GOFCER:
	JZ	GFCERR		;END OF TABLE.
	CMP	CL,AL		;HAVE WE GOT IT?
	JZ	MISCMD		;YES - dispatch to this command's executor
	INC	BX		;MOVE TO NEXT ENTRY
	INC	BX
	INC	BX
	JMP	SHORT MSCNLP
MISCMD:
	MOV	AL,BYTE PTR CS:[BX] ;SEE IF A VALUE NEEDED
	MOV	CL,AL		;PASS GOTTEN CHAR IN [C]
	ADD	AL,AL
	JNB	MNOARG		;COMMAND DOESN'T REQUIRE ARGUMENT
	SHR	AL,1		;MAKE IT A CHAR AGAIN
	MOV	CL,AL		;PUT IN [CL]
	PUSH	CX
	PUSH	BX		;SAVE PTR INTO CMD TABLE
	CALL	B$FETCHR	;GET A CHAR
	JZ	VSNAR0		;NO ARG IF END OF STRING
	CALL	LETTER		;SEE IF POSSIBLE LETTER
	JNB	VSNARG
	CALL	SCN_WORD	; get the value
DoCmd:				
	JNC	VSNAR1		; brif none specified -- use default
	JMP	SHORT ISCMD3


VSNARG:
	CALL	B$DECFET 	;PUT CHAR BACK INTO STRING
VSNAR0:
	CLC			
VSNAR1:				
	MOV	DX,1		; DEFAULT ARG=1
ISCMD3:
	POP	BX
	POP	CX		;GET BACK COMMAND CHAR
MNOARG:
	JMP	WORD PTR CS:[BX+1] ; Dispatch to Macro command, then return
				; to B$MACCMD's caller
MSCNX:				; No command, just return to B$MACCMD's
cEnd				; caller



GFCERR:				
	JMP	B$ERR_FC	

;***
;B$MCLXEQ
;
;Purpose:
;	This routine is dispatched to by the X command in DRAW or
;	single voice PLAY statements.  It is equivalent to a
;	macro-language subroutine call, in that it specifies a
;	variable which is to be inserted in the Macro String.  It:
;	 1)  calls B$GETSTKC to make sure we haven't eaten all stack space
;	 2)  stacks the current string pointer & length,
;	 3)  sets B$MCLPTR & B$MCLLEN to point to new nested string,
;	 4)  calls MACSCN to scan and process the nested string,
;	 5)  restores B$MCLPTR & B$MCLLEN from the stack
;	 6)  returns to its caller
;	NOTE: This routine may be called recursively.
;
;Entry:
;	B$MCLPSD points to current string descriptor
;	B$MCLPTR points to next element of current string
;	B$MCLLEN = remaining length of current string
;	b$VTYP = VT_SD, else error
;	B$AC = ptr to string descripter of sub-string to parse
;
;Exit:
;	Sub-string is fully parsed.
;	B$MCLPSD and B$MCLLEN are restored.
;	B$MCLPTR is recalculated based on B$MCLPSD and B$MCLLEN.
;
;Uses:
;	Per convention.
;
;Exceptions:
;	Will generate "Out of Stack Space" error if not enough room on stack.
;
;****
cProc	B$MCLXEQ,<PUBLIC,NEAR>
cBegin
	MOV	CL,100		;MAKE SURE OF ROOM ON STACK
	CALL	B$GETSTKC
	CALL	B$SCNVAR 	;scan variable name, [B$AC] points to
				;desc.
	CMP	[b$VTYP],VT_SD	; Is this a string?
	JNZ	TYPERR		;Type mismatch error if not a string
	PUSH	B$MCLPSD	; save pointer to current string descriptor
;	PUSH	B$MCLPTR	;save pointer to current string
	PUSH	B$MCLLEN	; length of current string
	MOV	BX,OFFSET DGROUP:B$AC ;BX:= ptr to string descriptor
	PUSH	BX		; save pointer to string descriptor
	CALL	MACSCN		;scan new string pointed to by [B$AC]

	POP	BX		; restore string descriptor
	CALL	B$STDALCTMP	; deallocate temp string
	POP	B$MCLLEN	; restore parent string's length
;	POP	B$MCLPTR	;restore parent string's pointer
	POP	BX		; restore saved PSD
	MOV	B$MCLPSD,BX	; restore parent string's pointer
	MOV	CX,[BX]		; CX = string length
	ADD	CX,[BX+2]	; CX = pointer to end of string
	SUB	CX,B$MCLLEN	; CX = original B$MCLPTR offset
	MOV	B$MCLPTR,CX	; restore parent string's pointer
cEnd

TYPERR: 			;Type mismatch error
	JMP	B$ERR_TM	; Return

;***
;B$SETMCL
;
;Purpose:
;	Set B$MCLPTR and B$MCLLEN to the data pointer and length of a
;	given string. If the string data pointer is NULL (0),
;	B$MCLLEN will be set to 0.
;
;Entry:
;	BX=ptr to string descriptor
;
;Exit:
;	B$MCLPSD ptr to string descripter
;	B$MCLPTR points to start of string's data
;	B$MCLLEN = number of bytes in string
;
;Uses:
;	
;	AX, CX, DX, ES, PSW
;
;Preserves:
;	BX.
;
;****
cProc	B$SETMCL,<PUBLIC,NEAR>
cBegin
	XOR	DX,DX		; default length is zero
	MOV	CX,[BX+2]	; CX := ptr
	JCXZ	PTNOT0		; branch if null string pointer
	MOV	DX,[BX]		; DX := length
PTNOT0:
	MOV	B$MCLLEN,DX	;Save length
	MOV	B$MCLPTR,CX	;SET UP POINTER
	MOV	[B$MCLPSD],BX	; save pointer to string descripter
cEnd

;***
;B$FETCHZ
;
;Purpose:
;	Get next byte from string [B$MCLPTR, B$MCLLEN]
;
;Entry:
;	B$MCLPTR points to character to be parsed from string.
;
;Exit:
;	[AL]=byte fetched
;	B$MCLPTR incremented, B$MCLLEN decremented.
;
;Uses:
;	ES, PSW.
;
;Preserves:
;	BX.
;
;Exceptions:
;	Generates an "Illegal function call" error if end-of-string.
;
;****
cProc	B$FETCHZ,<PUBLIC,NEAR>
cBegin
	CALL	B$FETCHR 	;GET A CHAR FROM STRING
	JZ	GFCERR		;GIVE ERROR IF END OF LINE
cEnd

;***
;B$FETCHR - Get next byte from string [B$MCLPTR, B$MCLLEN]
;
;Purpose:
;	Get next byte from string [B$MCLPTR, B$MCLLEN].
;
;Entry:
;	B$MCLPTR points to character to be parsed from string.
;
;Exit:
;	[AL]=byte fetched
;	PSW.Z set if end-of-string, reset if not.
;	B$MCLPTR incremented, B$MCLLEN decremented.
;
;Uses:
;	AX, PSW.
;
;Preserves:
;	BX.
;
;****
cProc	B$FETCHR,<PUBLIC,NEAR>,<BX>
cBegin
FETCH2:
	CMP	B$MCLLEN,0	;POINT TO STRING LENGTH
	JZ	POPTRT		;RETURN TZ IF END OF STRING
	DEC	B$MCLLEN	;UPDATE COUNT FOR NEXT TIME
	MOV	BX,B$MCLPTR	;GET PTR TO STRING
	MOV	AL,BYTE PTR [BX] ;GET CHARACTER FROM STRING
	INC	B$MCLPTR
	CMP	AL,' '		; SKIP SPACES
	JZ	FETCH2
	CMP	AL,9		; Skip TAB
	JZ	FETCH2
	Call	B$UPCASE	; Convert lowercase to UPPERCASE
POPTRT:
cEnd

;***
;B$DECFET
;
;Purpose:
;	Rescind the last byte fetched from the string [B$MCLPTR, B$MCLLEN]
;	by decrementing B$MCLPTR and incrementing B$MCLLEN.
;
;Entry:
;	B$MCLPTR, B$MCLLEN valid.  B$MCLPTR must not point to start of string.
;
;Exit:
;	B$MCLPTR decremented, B$MCLLEN incremented.
;	PSW.C = 1 if CX < 5
;
;Uses:
;	PSW.
;
;****
cProc	B$DECFET,<PUBLIC,NEAR>
cBegin
	INC	B$MCLLEN	;INCREMENT LENGTH
	DEC	B$MCLPTR	;BACK UP POINTER
CRET:				
	CMP	CX,5		; for B$VALSCN, set CARRY if digit read
cEnd

;***
;B$VALSCN
;Purpose:
;	Fetch a value from the string [B$MCLPTR, B$MCLLEN]
;Entry:
;	B$MCLPTR could be pointing to an ASCII numeric constant,
;	A string generated by VARPTR$, or an ASCII variable name.
;Exit:
;	[DX] = 2's complement signed integer result
;	Generates an "Illegal function call" error if end-of-string.
;Uses:
;	Per convention.
;Exceptions:
;	Generates an "Illegal function call" error if unexpected end-of-string
;	or other unrecognized syntax.
;****
	PUBLIC	B$VALSCN
B$VALSCN:
	CALL	B$FETCHZ 	;GET FIRST CHAR OF ARGUMENT
SCN_WORD:			; temp fix
	MOV	DL,VT_I2	; want integer value
	CMP	AL,'='		;NUMERIC?
	JZ	B$VARGET
	CMP	AL,'+'		;PLUS SIGN?
	JZ	B$VALSCN 	;THEN SKIP IT
	CMP	AL,'-'		;NEGATIVE VALUE?
	JNZ	B$VALSC2 	;Brif not
	CALL	B$VALSCN 	; else eat the '-'
	NEG	DX		; and negate value before returning
SRET:
	RET
	PUBLIC	B$VALSC2
B$VALSC2:
 	MOV	[b$VTYP],VT_I2	; want I2
	XOR	DX,DX		;INITIAL VALUE OF ZERO
	MOV	CX,5		;MAXIMUM 4 DIGITS.
NUMLOP:
	CMP	AL,','
	JZ	B$DECFET 	;If comma, backup and return
	CMP	AL,';'
	JZ	CRET		; Retif semicolon
	CMP	AL,'9'		; NOW SEE IF ITS A DIGIT
	JA	B$DECFET 	; IF NOT, BACK UP AND RETURN
	CMP	AL,'0'
	JB	B$DECFET
	MOV	BX,DX		;ARG=ARG*10+DIGIT
	ADD	BX,BX		;*5
	ADD	BX,BX
	ADD	BX,DX
	ADD	BX,BX		;*5*2
	SUB	AL,'0'		;ADD IN THE DIGIT
	MOV	AH,0
	ADD	BX,AX
NUMLO2:
	XCHG	DX,BX		;VALUE SHOULD BE IN [DX]
	CALL	B$FETCHR 	;GET NEXT CHAR
	STC			
	JZ	SRET		;Done if end of string
	LOOP	NUMLOP		;If more than 4 digits, then..
	JMP	SHORT SCNFC	;Complain since too many


labelNP	<PUBLIC,B$SCNVAR>
	CALL	VARPTR		;won't return if VARPTR$ string...
SCNFC:
	JMP	B$ERR_FC	; ERROR - VARIABLE TOO LONG


;*** 
;B$VARGET - scan variable and return either I2 or R4
;
;Purpose:
;	Scan variable and return either I2 or R4.
;
;Entry:
;	B$MCLPTR points to the varptr variable embedded in the string.
;	B$MCLLEN > 2 else error.
;	DL = 4 to return R4, else return I2
;
;Exit:
;	[b$VTYP] = VT_I2 and DX = I2 or
;	[b$VTYP] = VT_R4 and B$AC = CX:DX = R4.
;	B$MCLPTR += 3.
;	B$MCLLEN -= 3.
;	PSW.C set if non-default argument was scanned
;
;Uses:
;	Per convention.
;
;Preserves:
;	SI.
;
;Exceptions:
;	Type mismatch error if STRING variable.
;
;******************************************************************************
cProc	B$VARGET,<PUBLIC,NEAR>,<SI> ; preserve SI for PLAY code
cBegin
	CALL	B$SCNVAR 	;SCAN & EVALUATE VARIABLE
	CALL	B$FRCINT 	;MAKE IT AN INTEGER
	XCHG	DX,BX		;IN [DX]
	STC			; indicate value was scanned
cEnd


;*** 
;VARPTR - parse variable from varptr string.
;
;Purpose:
;	Parse variable from varptr string.
;
;Entry:
;	B$MCLPTR points to the varptr variable embedded in the string.
;	B$MCLLEN > 2 else error.
;
;Exit:
;	Variable returned in B$FAC/B$AC.
;	B$MCLPTR += 3.
;	B$MCLLEN -= 3.
;
;Uses:
;	Per convention.
;
;Exceptions:
;	Illegal function call if not varptr string.
;
;******************************************************************************
cProc	VARPTR,<NEAR>
cBegin
	MOV	BX,[B$MCLPTR]	;Get pointer into string
	CMP	BYTE PTR [BX],9 ;If length .gt. 8 then not VARPTR$ string
	JNB	SCNFC		;must be varptr$ string; if not, issue IFC
	CMP	B$MCLLEN,3	;Must be at least 3
	JB	SCNFC		;Not enough data: issue function call error
	POP	DX		;Trash ret
	MOV	AL,[BX] 	;assume Byte 1 is Type
	MOV	[b$VTYP],AL	; store in FAC
	MOV	DX,[BX+1]	;[DX] = Var address.
	ADD	B$MCLPTR,3	;New pntr
	SUB	B$MCLLEN,3	;New Length
	JMP	SHORT B$RETVARC	; Go return value in FAC
cEnd	<nogen>

;***
;LETTER
;
;Purpose:
;	See if ASCII character in [AL] is in the range [A..Z]
;
;Entry:
;	[AL] = ASCII code
;
;Exit:
;	Carry is set if not in the range [A..Z]
;	Carry is clear if it is in the range [A..Z]
;
;Uses:
;	PSW.
;
;****
cProc	LETTER,<NEAR>
cBegin
	CMP	AL,LOW "A"
	JB	ISLETX		;TOO SMALL FOR LETTER
	CMP	AL,"Z"+1
	CMC			;SET CARRY IF NOT LETTER
ISLETX:
cEnd


;***
;B$GETSTKC	Get stack space
;
;Purpose:
;	Make sure a minimum amount of stack space is available for recursive
;	calls.
;	B$GETSTKC and GETSTK, its interpreter equivalent, are not entirely plug-
;	compatible. GETSTK checks for a minimum amount of stack space (in bytes)
;	using the following formula:
;		minimum stack space = 2*CL + 2*NUMLEV,
;		where  CL is an entry condition
;	   	and NUMLEV is a switch (=110 decimal in features.h)
;
;	B$GETSTKC checks for a minimum amount of stack space using the following
;	formula:
;
;		minimum stack space = CL
;		where CL is an entry condition
;
;	If the minimum stack space is not available GETSTK and B$GETSTKC
;	issue out of memory errors.
;
;Entry:
;	CL= number of stack bytes needed
;
;Exit:
;	CX= number of stack bytes needed
;
;Uses:
;	None.
;
;Exceptions:
;	Will generate "Out of Stack Space" error if no room on stack.
;
;****
cProc	B$GETSTKC,<PUBLIC,NEAR>	; Make sure of room on the stack
cBegin
	PUSH	BX		;Save BX
	MOV	BX,[b$pendchk] ; Stack can grow down to b$pendchk
	MOV	CH,0		;Zero out high byte of CX
	ADD	BX,CX		;Add CX to bottom of stack
;This will provide 100 bytes for one recursion:  should be more than enough
	JB	ERR_OM		;Out of memory if BX overflowed
	CMP	BX,SP		;Compare stack ptr to b$pendchk + CX
	JNB	ERR_OM		;If BX>SP issue out of memory error
	POP	BX		;Restore BX
cEnd

ERR_OM:
	JMP	B$ERR_OSS	; out of memory error routine

;***
;B$RETVARC - Return variable in accumulator (compiler version)
;
;Purpose:
;	Return variable in accumulator.  Use variable type to determine
;	how many bytes to move and where to move them.  Double precision
;	variables begin at B$DAC (an 8 byte accumulator), all others begin
;	at B$AC.  If the variable is double precision, move 8 bytes, if string
;	or single precision, move 4 bytes, if integer move 2 bytes.
;	This routine is plug-compatible with the compiler routine RETVAR.
;
;Entry:
;	DX=ptr to variable (ES:DX if FV_FARSTR)
;	AL=variable type
;
;Exit:
;	If var is integer: B$AC contains integer (2 bytes)
;	If var is s.p.	 : B$AC contains s.p. #	(4 bytes)
;	If var is d.p.	 : B$DAC contains d.p. # (8 bytes)
;	If var is string : B$AC contains string descriptor (4 bytes)
;
;Uses:
;	None.
;	(the interpreter equivalent routine, RETVAR, may modify all regs
;	except BX, and segment registers.  Its effects are not documented.)
;
;****
cProc	B$RETVARC,<PUBLIC,NEAR>,<CX,SI,DI,AX,ES> ; return var in accumulator
cBegin
	MOV	SI,DX		;Move var ptr to source index reg
	CBW			;extend var type to full word in AX
	XCHG	CX,AX		;CX:=AX (saves 1 byte over mov cx,ax)
	CMP	CL,VT_R8	; Is var double precision?
	JNE	NOTDP		;No: load DI differently
	MOV	DI,OFFSET DGROUP:B$DAC ;Use 8 byte accumulator
	JMP	SHORT MOVVAR	;go move the var to accumulator
NOTDP:				;not a double precision variable
;assume var type in CL is either 2 (integer), 3(string) or 4(single precision)
	MOV	DI,OFFSET DGROUP:B$AC ;Use 4 byte accumulator
	CMP	CL,VT_I2	; Is var integer?
	JE	MOVVAR		;Yes: CX contains correct value for string move
	MOV	CX,4		;CX:=4 (must move four bytes)
MOVVAR: 			;CX=# bytes to move, SI=source, DI=destination
				;repeat until CX=0: move a byte
	PUSH	DS		
	POP	ES		; ES = DS
	REP	MOVSB		; copy variable
cEnd

sEnd	RT_TEXT			
	END
