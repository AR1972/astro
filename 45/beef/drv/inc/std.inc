;*****************************************************************************
;*
;*	* * * STD.INC : Standard extensions to CMACROS.INC * * *
;*	(must include CMACROS.INC first)
;*	WARNING:
;*	This file is source controlled in the \LIB\INC directory !
;*****************************************************************************

by	equ	byte ptr
wo	equ	word ptr
dwo	equ	dword ptr
qwo	equ	qword ptr

;*	* external Absolute values
IFNDEF externA	;* not defined in old CMACROS
externA macro n
?ex1 <n>,0,<ABS>
endm
ENDIF

MovSeg	MACRO	segDest,segSrc			;* slow move segment
	push	segSrc
	pop	segDest
ENDM	;MovSeg

SkipW	MACRO
	DB	03DH		;;* skip word operand (CMP AX,...)
ENDM

SkipB	MACRO
	DB	03CH		;;* skip byte operand (CMP AL,...)
ENDM

IFNDEF RETF
RETF	MACRO	cbParam
    IFNB <cbParam>
	DB	0CAH		;;* return far add to stack
	DW	cbParam
    ELSE
	DB	0CBH		;;* return far
    ENDIF
ENDM
ENDIF


ASSERT	MACRO	expr		;;* Assembly time assertion check
    IFE expr
	%OUT Assertion Failed !expr!
	.ERR
    ENDIF
ENDM


BREAKPOINT MACRO		;;* insert breakpoint if debugging
    IFDEF DEBUG
	INT	3
    ENDIF
ENDM

;*****************************************************************************

;* NOTE : in adding extra code for Debugging, short jumps may no longer fit:
;*   hence the use of the SHORT_ macros

;********** SHORT_Jcc **********
;* MACROS
;*	* if non-debugging : perform a "Jcc" address (Short jump)
;*	* if debugging : perform a short jump around a long jump.
;*	* NOTE : the extra time required for the debugging case is NOT taken
;*	*  into consideration - hence profiling is not exact.

SHORT_JE MACRO	addr
	LOCAL around
    IFDEF DEBUG
	JNE	around
	JMP	addr
    ELSE
	JE	addr
    ENDIF
around:
ENDM

SHORT_JNS MACRO	addr
	LOCAL around
    IFDEF DEBUG
	JS	around
	JMP	addr
    ELSE
	JNS	addr
    ENDIF
around:
ENDM

SHORT_JNZ MACRO	addr
	LOCAL around
    IFDEF DEBUG
	JZ	around
	JMP	addr
    ELSE
	JNZ	addr
    ENDIF
around:
ENDM

SHORT_JA MACRO	addr
	LOCAL around
    IFDEF DEBUG
	JBE	around
	JMP	addr
    ELSE
	JA	addr
    ENDIF
around:
ENDM

SHORT_JAE MACRO	addr
	LOCAL around
    IFDEF DEBUG
	JB	around
	JMP	addr
    ELSE
	JAE	addr
    ENDIF
around:
ENDM

SHORT_JMP MACRO addr
    IFDEF DEBUG
	JMP	addr
    ELSE
	JMP	SHORT addr
    ENDIF
ENDM

JMP_SHORT MACRO addr
    IFDEF DEBUG
	JMP	addr
    ELSE
	JMP	SHORT addr
    ENDIF
ENDM

;*****************************************************************************
;* Runtime Asserts

IFDEF DEBUG
;********** debugging **********
AssertEQ MACRO	a,b		;* assert 2 values are equal
	LOCAL	ok
	CMP	a,b
	JE	ok
	BREAKPOINT
ok:
ENDM
AssertNE MACRO	a,b		;* assert 2 values are not equal
	LOCAL	ok
	CMP	a,b
	JNE	ok
	BREAKPOINT
ok:
ENDM
AssertTest MACRO a,cond,b	;* assert test condition
	LOCAL	ok
	TEST	a,b
	J&cond	ok
	BREAKPOINT
ok:
ENDM
AssertEven MACRO a		;* assert something is even
	LOCAL	ok
	TEST	a,1
	JZ	ok
	BREAKPOINT
ok:
ENDM
AssertOdd MACRO a		;* assert something is odd
	LOCAL	ok
	TEST	a,1
	JNZ	ok
	BREAKPOINT
ok:
ENDM
AssertCmp MACRO	a,cond,b	;* assert comparison condition
	LOCAL	ok
	CMP	a,b
	J&cond	ok
	BREAKPOINT
ok:
ENDM
AssertData MACRO sreg		;* assert a segment register is set to DDS (SS)
	LOCAL	ok
	PUSH	AX
	PUSH	BX
	MOV	AX,sreg
	MOV	BX,SS
	CMP	AX,BX
	POP	BX
	POP	AX
	JZ	ok
	BREAKPOINT
ok:
	assumes	sreg,DGROUP
ENDM
AssertZR MACRO			;* assert the zero flag is ZR.
	LOCAL	ok
	JZ	ok
	BREAKPOINT
ok:
ENDM
AssertNZ MACRO			;* assert the zero flag is NZ.
	LOCAL	ok
	JNZ	ok
	BREAKPOINT
ok:
ENDM
AssertCY MACRO			;* assert the carry flag is CY.
	LOCAL	ok
	JC	ok
	BREAKPOINT
ok:
ENDM
AssertNC MACRO			;* assert the carry flag is NC.
	LOCAL	ok
	JNC	ok
	BREAKPOINT
ok:
ENDM
ELSE
;********** non-debugging **********
AssertEQ MACRO	a,b
ENDM
AssertNE MACRO	a,b
ENDM
AssertCmp MACRO	a,cond,b
ENDM
AssertTest MACRO a,cond,b
ENDM
AssertEven MACRO a
ENDM
AssertOdd MACRO a
ENDM
AssertData MACRO sreg
	assumes	sreg,DGROUP
ENDM
AssertZR MACRO
ENDM
AssertNZ MACRO
ENDM
AssertCY MACRO
ENDM
AssertNC MACRO
ENDM
ENDIF ;!DEBUG

;*****************************************************************************

;********** Lbl **********
;*	entry:	name = name to make public at the current location
;*	* make a debugging public entry point
Lbl	MACRO	name
	PUBLIC	name
name:
ENDM	;Lbl
	
;*****************************************************************************

