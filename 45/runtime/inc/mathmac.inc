;***
; mathmac.inc - 20-Mar-86 - Helper macroes for elementary functions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************


S_MOV	MACRO	op1,op2
	MOV	SI,offset DGROUP:&op2
	MOV	DI,offset DGROUP:&op1
	MOVSW
	MOVSW
	ENDM

S_PUSH	MACRO	op1
	PUSH	[op1]
	PUSH	[op1+2]
	ENDM

S_POP	MACRO	op1
	POP	[op1+2]
	POP	[op1]
	ENDM

D_MOV	MACRO	op1,op2
	MOV	SI,offset DGROUP:&op2
	MOV	DI,offset DGROUP:&op1
	MOVSW
	MOVSW
	MOVSW
	MOVSW
	ENDM

D_PUSH	MACRO	op1
	PUSH	[op1]
	PUSH	[op1+2]
	PUSH	[op1+4]
	PUSH	[op1+6]
	ENDM

D_POP	MACRO	op1
	POP	[op1+6]
	POP	[op1+4]
	POP	[op1+2]
	POP	[op1]
	ENDM

OPER	MACRO	ty,op
&ty&_&op MACRO	op1,op2
	MOV	SI,offset DGROUP:&op1
	MOV	DI,offset DGROUP:&op2
	CALL	$&ty&&op
	&ENDM
	ENDM

OPER	S,ADD
OPER	S,SUB
OPER	S,MUL
OPER	S,DIV
OPER	S,CMP

OPER	D,ADD
OPER	D,SUB
OPER	D,MUL
OPER	D,DIV
OPER	D,CMP

	.LIST
