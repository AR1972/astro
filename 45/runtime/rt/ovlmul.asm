	TITLE	ovlmul - I4 Multiply with overflow checking
;***
;ovlmul - I4 Multiply with overflow checking
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; 8086 Signed Long Multiply Routine, which checks overflows
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	UseSeg	_TEXT

	INCLUDE seg.inc

sBegin	_TEXT			
assumes CS,_TEXT		

;*** 
; ovlmul - I4 Multiply with overflow checking
;
;Purpose:
; 8086 Signed Long Multiply Routine, which checks overflows
;
;Entry:
; A & B 	= Numbers to multiply
;
;Exit:
; [DX:AX]	= result
; C		= set on overflow
;
;Uses:
;
;Preserves: (optional)
;
;Exceptions:
;
;******************************************************************************

labelFP <PUBLIC,__aFlmul>	
cProc  __aFulmul,<PUBLIC>,<si,di>

parmD	A
parmD	B

cBegin
	MOV	DX,SEG_A
	MOV	AX,OFF_A	;DX:AX == A
	MOV	SI,SEG_B
	MOV	DI,OFF_B	;SI:DI == B

;first get result sign and absolute value of numbers

	MOV	BX,DX		; will hold result sign flag
	OR	DX,DX		; is A negative ?
	JNS	MULABA		; jump if positive
	NEG	AX		; neg A lo
	ADC	DX,0		; inc A hi
	NEG	DX		; neg A lo

MULABA: XOR	BX,SI		; BP Most signifcant bit = result sign
	OR	SI,SI		; is B negative ?
	JNS	MULABB		; jump if positive
	NEG	DI		; neg B lo
	ADC	SI,0		; inc B hi
	NEG	SI		; neg B hi
;
;four cases: A hi is significant, and/or B hi is significant
MULABB: OR	SI,SI		; anything in B hi ?
	JNZ	MULYHI		; if not, fall through
	XCHG	SI,DX		; swap hi parts
	OR	SI,SI		; anything in A hi ?
	JNZ	MULXHI		; if not, fall through
	MUL	DI		; just multiply lo parts
	OR	DX,DX		; set sign flag for result
;
; Result in DX:AX, sign flag is for DX, sign wanted is in BP
MULFIN: JS	MULOVR		; check later signed overflow
	OR	BX,BX		; check sign bit
	JNS	MULEND		; jump if positive
	NEG	AX		; neg result lo
	ADC	DX,0		; inc result hi
	NEG	DX		; neg result hi
MULEND:
	CLC
	JMP	SHORT MULEAVE	; finished
;
MULYHI: OR	DX,DX		; anything in A hi ?
	JNE	MULOVR		; if so, go overflow
	XCHG	AX,DI		; correct lo parts
;
;Result wanted is DI times SI:AX, times sign
MULXHI: XCHG	AX,SI		; swap lo and hi parts
	MUL	DI		; one lo times the other hi
	JC	MULOVR		; check for hi part nonzero
	XCHG	AX,SI		; swap lo and result parts
	MUL	DI		; one lo times the other lo
	ADD	DX,SI		; final accumulate
	JNC	MULFIN		; check unsigned overflow
;Overflow, set carry flag and return
MULOVR:	STC

MULEAVE:

cEnd

sEnd
	END
