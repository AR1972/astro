	TITLE	HEXOCT - OCT, HEX functions
;***
; HEXOCT - OCT, HEX functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - HEX$ Function - calls B$FHEX with 4-byte integer parm.
;
;      v$ = HEX$(n)
;	     |
;	 B$FHEX
;
;
; - OCT$ Function - calls B$FOCT with 4-byte integer parm.
;
;      v$ = OCT$(n)
;	     |
;	 B$FOCT
;
; - BIN$ Function - calls B$FBIN with 4-byte integer parm.
;
;      v$ = BIN$(n)
;	     |
;	 B$FBIN
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	ST_TEXT
	USESEG	NH_TEXT
	USESEG	_DATA

	INCLUDE seg.inc

sBegin	NH_TEXT
	externNP B$STALCTMPCPY
sEnd	NH_TEXT

assumes CS,ST_TEXT
sBegin	ST_TEXT

	externNP B$FrameAFE	; advanced feature error + frame setup

;***
;B$FBIN,B$FHEX,B$FOCT - Convert I4 number to binary, hex, or octal string
;
;Purpose:
;	Runtime Entry points.
;	Create a string of minimum length (no leading zeros) that represents
;	the value of the number in binary, octal or hex.
;Entry:
;	parmD = I4 Integer value to be converted to a string
;Exit:
;	AX = Address of string descriptor
;Uses:
;	Per convention.
;Exceptions:
;	B$ERR_OM
;****
cProc	B$FBIN,<PUBLIC,FAR>	;BIN$ function
cBegin	<nogen> 		
	JMP	B$FrameAFE	; advanced feature error + frame setup
cEnd	<nogen> 		;and convert to string

cProc	B$FHEX,<PUBLIC,FAR>	;HEX$ function
cBegin	<nogen> 		
	MOV	CX,0F04H	;CH=mask, CL=shift count
	JMP	SHORT CONVERT	;use common conversion routine to pick param
cEnd	<nogen> 		;and convert to string

cProc	B$FOCT,<PUBLIC,FAR>	;OCT$ function
cBegin	<nogen> 		
	MOV	CX,0703H	;CH=mask, CL=shift count
cEnd	<nogen> 		;fall into conversion routine

.erre	ID_SSEQDS		;assumes SS=DS

cProc	CONVERT,<FAR>,<ES,DI>
parmD	Val			;I4 parameter for above routines
localW	BufTop			;Top of string buffer
localV	Buf,30			;32 byte buffer for string
cBegin
	PUSH	DS		;set ES=DS
	POP	ES
	MOV	BX,WORD PTR[Val];DX:BX = I4 to be converted
	MOV	DX,WORD PTR[Val+2]
	XOR	AH,AH		;init char count
	LEA	DI,BufTop	;set up to build string on stack
	STD			;move from high to low

; At this point the following conditions exist:
;	AH = Character count
;	CH = Mask
;	CL = Shift count
;	DX:BX = I4 to convert
;	DI = pointer to digit buffer
; Perform the conversion by shifting DX:BX by CL bits and masm out
; unused bits with CH.	Take this number and convert to ascii char
; representing digit. Stuff the char in the buffer, bump the char
; count and continue until no non-zero digits remain.

CONVERT_LOOP:
	MOV	AL,BL		;Bring number to accumulator
	AND	AL,CH		;Mask down to the bits that count
;Trick 6-byte hex conversion
	ADD	AL,90H
	DAA
	ADC	AL,40H
	DAA			;Number in hex now
	STOSB			;Save in string
	INC	AH		;Count the digits
	PUSH	CX		;Save mask/shift count
	XOR	CH,CH		;zero out mask, leaving shift count

SHIFT_LOOP:
	SHR	DX,1		;shift low bit into carry, zero high bit
	RCR	BX,1		;rotate carry into low word
	LOOP	SHIFT_LOOP	;repeat shift count times

	POP	CX		;recover mask/shift count
	PUSH	BX
	OR	BX,DX		;is rest of I4 = 0?
	POP	BX
	JNZ	CONVERT_LOOP	;brif not, convert next digit

	CLD			;Restore direction UP
	INC	DI		;Point to most significant digit
	MOV	BL,AH		;Digit count in BX (BH already zero)
	MOV	DX,DI		;Put string pointer in DX
	CALL	B$STALCTMPCPY	;Allocate string and copy data in
	XCHG	AX,BX		;return string descriptor in AX
cEnd
sEnd	ST_TEXT
	END
