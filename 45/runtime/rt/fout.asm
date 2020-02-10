	TITLE	FOUT - Free-format numeric output
;***
; FOUT - Free-format numeric output
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Floating Point versions of the routines in IFOUT.ASM
;
;	This file is dragged in by the label B$FloatCONASC
;
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	_TEXT		
	USESEG	RT_TEXT 	
	USESEG	XIB		; XIB and XIE must bracket XI!
	USESEG	XI		;initializer segment
	USESEG	XIE		
	USESEG	INIT_CODE	

	INCLUDE seg.inc 	
	INCLUDE	baslibma.inc	
	INCLUDE rtps.inc	; constants shared with QBI
	INCLUDE idmac.inc	



sBegin	_DATA			

	externW B$AC		
	externW B$DAC		

sEnd	_DATA			

sBegin	_BSS			

	externB b$SIGN 		; defined in CONASC.ASM
	externB b$VTYP		; defined in GWDATA.ASM

sEnd	_BSS			




assumes CS,RT_TEXT		
sBegin	RT_TEXT 		

	externNP B$ASCRND	

;***
;B$FloatFOUTBX - Free-format numeric output for non-integers
;
;Purpose:
;	Format number for printing, using BASIC's formatting rules.
;
;Entry:
;	BX = ptr to Number to be formatted
;	[b$VTYP] = VT_R4, VT_R8 or VT_CY
;
;Exit:
;	BX = Address of ASCII string, terminated by 00
;	AX = Length of string (not including terminating 00)
;
;Uses:
;	Per convention.
;
;Exceptions:
;	None.
;****
PUBLIC	B$FloatFOUTBX		
cProc	B$FloatFOUTBX,<NEAR>,<ES,SI,DI>       
cBegin				
	PUSH	DS		
	POP	ES		
	MOV	AL,[b$VTYP]	
	AND	AX,0FH		;mask I4s to count of bytes to move
	MOV	SI,BX
	MOV	DI,OFFSET DGROUP:B$AC ;point to B$AC
	CMP	AX,4		;<= 4 byte quantity?
	JBE	FourByteVar	 ;brif so
	SUB	DI,4		;use DAC instead of AC
FourByteVar:
	XCHG	AX,CX
	SHR	CX,1		;Count of words
	REP	MOVSW

	CALL	B$FloatCONASC	;Most of the work's done here

;Number has been converted to ASCII and is sitting at buffer pointed to by DI.
;	SI = address of result buffer
;	CX = number of significant figures
;	DL has the base 10 exponent of the right end of the digit string.

	MOV	BX,7		; If SP, use 7 digits, but limit 7 to right
	CMP	[b$VTYP],VT_R8	; What type?
	JB	RNDDIG		; If I2 or R4, use R4 parameters
	MOV	BX,16		; For R8 or I4, 16 digits, limit 16 to right
RNDDIG:
	MOV	AL,BL
	CALL	B$ASCRND	;Round to AL digits
	MOV	AX,DX		; [AX] = exponent
	PUSH	DX		; save it also
	CWD			; sign extend
	XOR	AX,DX		; [AX] = ABS(exponent)
	SUB	AX,DX		; adjust
	POP	DX		; [DX] = exponent
	CMP	AX,BX		;Too many digits to right of decimal point?
	JG	SCINOT		;If so, use scientific notation
	MOV	AX,DX		
	ADD	AX,CX		; AL=number of digits to left of dec.pt.
	CMP	AX,BX		; Too many?
	JG	SCINOT		;If so, use scientific notation
	XCHG	AX,CX		;CX has digits needed to left of dec. pt.
	XCHG	AX,BX		;BX has digits available from buffer
	OR	CX,CX		;Any to left?
	JLE	LESS1		;If not, start with decimal point
LEFTDP:
	MOVSB			;Move digits to final buffer
	DEC	BX		;Limit to available digits
	LOOPNZ	LEFTDP
	MOV	AL,"0"
	REP	STOSB		;Fill in with place-holding zeros if needed
	JZ	PUTEND		;If out of digits, were done(flags from DEC BX)
PUTDP:
	MOV	AL,"."
	STOSB			;Put in decimal point
	MOV	AL,"0"		;Used only if we came from LESS1
	REP	STOSB		;Add leading zeros if a small number
	MOV	CX,BX		;Number of digits left
	REP	MOVSB
PUTEND:
	MOV	BX,OFFSET DGROUP:b$SIGN ;Leave pointer to buffer
	MOV	AX,DI
	SUB	AX,BX		;Length of string
	MOV	BYTE PTR[DI],0	;Put in terminating zero
cEnd				

LESS1:
;Come here if number is less than one and therefore has no digits to
;left of the decimal point.
	NEG	CX		;Number of place-holding zeros needed
	JMP	PUTDP		;   after the decimal point

SCINOT:
	MOVSB			;Move first digit to final buffer
	DEC	CX		;Account for digit already moved
	JZ	EXP		;Skip decimal point if only one digit
	MOV	AL,"."
	STOSB
	ADD	DX,CX		; Correct exponent for decimal point position
	REP	MOVSB		;All other digits go after decimal point
EXP:
	MOV	AX,"+E" 	;Prepare "E+" if positive exponent
	OR	DX,DX		; Check exponent sign
	JNS	POSEXP
	NEG	DX		; Force exponent positive
	MOV	AH,"-"
POSEXP:
	CMP	[b$VTYP],VT_R8	; Is type double precision?
	JNZ	SCISNGL
	DEC	AX		;Convert the "E" to "D"
SCISNGL:
	STOSW
	MOV	AX,DX		; [AX] = exponent
	MOV	BL,100		
	DIV	BL		; [AL] = hundreds count
	OR	AL,AL		; See if any
	JZ	SCISMALL	; jump if not
	OR	AL,"0"		; turn into hundreds digit
	STOSB			; add leading digit
SCISMALL:			
	XCHG	AL,AH		; get remainder here
	AAM			;Convert binary to unpacked BCD
	XCHG	AL,AH
	OR	AX,"00" 	;Add ASCII bias
	STOSW
	JMP	PUTEND

;
; Rewritten and moved here from CONASC.ASM
;
;***
;B$FloatCONASC - Convert number to ASCII
;
;Purpose:
;	Convert number to a string of ASCII digits with no leading or
;	trailing zeros. Return base 10 exponent and count of digits.
;	No integer values will be passed to this routine.
;
;Entry:
;	B$AC has SP number OR B$DAC has DP or CY number
;
;Exit:
;	CX = number of significant figures (decimal point to right)
;	DL = base 10 exponent
;	SI = Address of first digit (non-zero unless number is zero)
;	[b$SIGN] has sign - blank if positive, "-" if negative
;
;Uses:
;	Uses all.
;****

cProc	B$FloatCONASC,<NEAR,PUBLIC>
cBegin
	MOV	SI,OFFSET DGROUP:B$DAC ;address of location to put final R8
	CMP	b$VTYP,VT_R8	;Is it an R8
	JE	CONASC2 	;brif so, we are ready to convert

GOT_R4: 			
DbAssertRelB  b$VTYP,E,VT_R4,RT_TEXT,<Unknown value for b$VTYP in CONASC>

	MOV	BX,OFFSET DGROUP:B$AC ;point to AC for R4s
	fld	dword ptr [bx]	;Load the R4
	fstp	qword ptr [si]	;And store it as an R8
	FWAIT			;ensure stored


CONASC2:
	Call   FAR PTR B$I8_OUTPUTHack ;math coversion from R8 to ascii string

;	At this point $i8_output returns the following conditions
;		DS:SI	- pointer to converted string (first byte is length)
;		AX	- 1 if number was ok, 0 if indefinite
;		BL	- sign character: either ' ' or '-'
;		DX	- base 10 exponent (left of digits)
;		CX,BH	- smashed

CONASC3:
	MOV	b$SIGN,BL	;save sign char
	LODSB			;AL = length of strings
	CBW
	CMP	AX,1		;See if single digit
	JNZ	ModExp		;If not, go modify exponent
	CMP	BYTE PTR [SI],"0" ;Else see if number is zero
	JZ	NoModExp	;If so, leave exponent alone
ModExp:
	SUB	DX,AX		;DX = base 10 exponent (right of digits)
NoModExp:
	XCHG	AX,CX		;return num digits in CX
cEnd


sEnd	RT_TEXT

assumes CS,_TEXT
sBegin	_TEXT

	externNP $i8_output	;math pack string conversion

;***
;B$I8_OUTPUTHack  - Hack to call B$I8_OUTPUT near from _TEXT.
;
;Purpose:
;	Hack to call B$I8_OUTPUT near from _TEXT.
;	Convert number to a string of ASCII digits.
;
;Entry:
;	DS:SI	- ptr to 8 byte double precision number.
;
;Exit:
;	DS:SI	- pointer to converted string (first byte is length)
;	AX	- 1 if number was ok, 0 if indefinite
;	BL	- sign character: either ' ' or '-'
;	DX	- base 10 exponent
;	CX,BH - smashed
;
;Uses:
;	Uses all but BP,DI.
;****

cProc	B$I8_OUTPUTHack,<FAR>,<BP,DI>
cBegin
	cCall	$i8_output	;math coversion from R8 to ascii string

;	At this point $i8_output returns the following conditions
;		DS:SI	- pointer to converted string (first byte is length)
;		AX	- 1 if number was ok, 0 if indefinite
;		BL	- sign character: either ' ' or '-'
;		DX	- base 10 exponent
;		DI,CX,BH - smashed

cEnd

sEnd	_TEXT


	END
