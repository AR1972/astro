	TITLE	IFOUT - Integer Free-format numeric output
;***
; IFOUT - Integer Free-format numeric output
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Integer only versions of the routines in FOUT.ASM.  These routines
;	will call the floating point versions only if the floating point
;	versions have been linked in.  This prevents a print statement from
;	automatically pulling in the math pack.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	USESEG _DATA
	USESEG _BSS
	USESEG MT_TEXT

	INCLUDE seg.inc
	INCLUDE rtps.inc


sBegin	_DATA


	externB B$AC		;Floating accumulator
sEnd	_DATA

sBegin	_BSS
	externB b$VTYP

	;NOTE: The buffer consists of b$SIGN, b$FOBUF, and BUFFER_END
	;NOTE: I.E. the buffer is 36 bytes long

	globalB b$SIGN,,1	;Sign of Value returned from B$CONASC
	globalB b$FOBUF,,34	;Numeric output buffer (integer + floating)
	staticB BUFFER_END,,1	;Last byte of buffer
sEnd	_BSS


sBegin	MT_TEXT
	assumes CS,MT_TEXT

	externNP B$ERR_FC


	externNP B$FloatFOUTBX	

;***
;B$IFOUT - Free-format numeric output (FAR interface)
;
;Purpose:
;	Format number for printing, using BASIC's formatting rules.
;Entry:
;	BX = ptr to Number to be formatted
;	AL = Val type - VT_I2, VT_I4, VT_R4, VT_R8 or VT_CY
;Exit:
;	BX = Address of ASCII string, terminated by 00
;	AX = Length of string (not including terminating 00)
;Uses:
;	Per convention. (DS, ES, SI, DI, BP preserved. PSW.D clear)
;Exceptions:
;	None
;****
cProc	B$IFOUT,<PUBLIC,FAR>,<BP>
cBegin
	MOV	[b$VTYP],AL
	cCall	B$FOUTBX	;call common fouter
cEnd


;***
;B$FOUTBX - Free-format numeric output
;
;Purpose:
;	Format number for printing, using BASIC's formatting rules.
;
;Entry:
;	BX = ptr to Number to be formatted
;	[b$VTYP] = VT_I2, or  VT_I4
;
;Exit:
;	BX = Address of ASCII string, terminated by 00
;	AX = Length of string (not including terminating 00)
;
;Uses:
;	Per convention.
;
;Exceptions:
;	B$ERR_FC if invalid b$VTYP.
;****

cProc	B$FOUTBX,<PUBLIC,NEAR>	;NOTE: Prolog doesn't put anything on stack
cBegin

	MOV	AX,[BX] 	;AX = low word of I4 or I2
	CWD			;Assume I2 (and convert to I4)
	CMP	b$VTYP,VT_I2	; Is it an I2
	JE	Got_Num 	;Yes, check if negative
	CMP	b$VTYP,VT_I4	; Is it an I4
	JNE	FloatingFoutBX	;No, pass it on to the floating point fout.
	MOV	DX,[BX+2]	;DX = High word of I4
Got_Num:
	XOR	BX,BX	;Get a Zero
	XCHG	AX,DX	;AX:DX = I4
	MOV	CL,' '	;Assume that number is positive
	AND	AX,AX	;Check for positive
	JGE	Positive	;It is!
	MOV	CL,'-'	;Flag it as a negative number
	NEG	DX	;And convert it to positive
	ADC	AX,BX	;Add in carry (BX = 0)
	NEG	AX
Positive:
	PUSH	SI	;Preserve original SI
	PUSH	CX	;Save Sign for later
	MOV	SI,OFFSET DGROUP:BUFFER_END
	MOV	BYTE PTR [SI],BL ;Zero Terminate the buffer.
	PUSH	SI	;Save this value for later
	MOV	CX,10	;Dividing by 10

Div_Loop:
	PUSH	DX	;Save Low part of I4
	XOR	DX,DX	;DX:AX = High Word of I4 (extended to 4 bytes)
	DIV	CX	;DX = Remainder, AX = Quotent
	POP	BX	;Restore Low part of I4
	XCHG	AX,BX	;DX:AX = Low Word of I4,  BX = High Quotent
	DIV	CX
	XCHG	AX,DX	;AX = Remainder of I4/10, BX:DX = Quotent of I4/10
	ADD	AL,"0"	;adjust for ASCII
	DEC	SI
	MOV	BYTE PTR [SI],AL
	MOV	AX,BX	;AX:DX = I4 again
	OR	BX,DX	;Are we done yet?
	JNZ	Div_Loop	;Nope, get next digit.
;
;Finish up
;

	POP	AX	;Restore starting point
	SUB	AX,SI	;AX = # digits
	POP	CX	;Restore Sign
	DEC	SI
	MOV	BYTE PTR [SI],CL;Put sign on number
	MOV	BX,SI	;BX = Start of string
	INC	AX	;Include sign in count
	POP	SI	;Restore original SI
cEnd

FloatingFoutBX:
	JMP	B$FloatFOUTBX	; Call the routine directly

;***
; B$ASCRND - Round ASCII digits
;
;Purpose:
;	Round number to the specified number of digits. Eliminate trailing
;	zeros from digit count.
;
;Inputs:
;	AL = Number of digits wanted
;	CX = Number of digits presently in number
;	DX = Base 10 exponent (D.P. to right of digits)
;	SI = Address of first digit
;
;Outputs:
;	CX = Number of digits now in number (always <= request)
;	DX = Base 10 exponent of rounded number
;	SI = Address of first digit of rounded number
;	DI = Address of formatting buffer, b$FOBUF
;
;Registers:
;	Only ES, BX, BP preserved.
;****
cProc	B$ASCRND,<NEAR,PUBLIC>,<ES>
cBegin

	PUSH	DS		;ES=DS
	POP	ES
	MOV	DI,SI
	CBW			;Zero AH (AL <= 18)
	ADD	DI,CX		;Point past last digit
	CMP	AX,CX		;Any extra digits?
	JAE	ZSCAN		;If not, no rounding
	XCHG	AX,CX		;Say we'll return number requested
	SUB	AX,CX		;See how many digits we're trimming
	ADD	DX,AX		;Increase exponent accordingly
	SUB	DI,AX		;Point to first extra digit
	MOV	AL,"0"
	XCHG	AL,[DI] 	;Get rounding digit and replace it with zero
	CMP	AL,"5"		;Do we need to round?
	JB	ZSCAN
	JCXZ	RNDALL
RND:
	DEC	DI		;Point to digit to round
	MOV	AL,[DI] 	;Get a digit that needs incrementing
	INC	AL
	CMP	AL,"9"+1	;Did we overflow this digit position?
	JB	STORND		;If not, store it and we're done
	INC	DX		;Otherwise exponent must be adjusted
	LOOP	RND		;We'll need to round next digit
RNDALL:
	INC	CX		;Must have at least one digit
	MOV	AL,"1"		;If we rounded all digits, must be to 1
STORND:
	STOSB			;Save rounded digit
ZSCAN:
;DI points just past the digits we want. Check for trailing zeros.
	DEC	DI		;Point to last digit
	MOV	AH,CL		;Remember how many digits we started with
	MOV	AL,"0"
	STD			;Scan DOWN
	REPE	SCASB		;Scan for "0"s
	CLD			;Restore direction UP
	INC	CX		;Number of digits left
	SUB	AH,CL		;Number of digits skipped
	ADD	DL,AH		;Increase base 10 exponent accordingly
	ADC	DH,0
	MOV	DI,OFFSET DGROUP:b$FOBUF
cEnd

sEnd	MT_TEXT
	END
