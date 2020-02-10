	TITLE	OSTIMER - MS-DOS TIMER support
;***
; OSTIMER - MS-DOS TIMER Support
;
;	Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - TIMER Function:
;
;      var = TIMER
;	       |
;	     B$TIMR
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	USESEG	OS_TEXT
	USESEG	MT_TEXT

	USESEG	_BSS
	USESEG	_DATA

	INCLUDE seg.inc
	INCLUDE oscalls.inc
	INCLUDE files.inc


C_GETTIM	EQU	44	;Get Time Function (for CALLOS)

sBegin	_BSS
	externB b$Buf1		;Floating point temporary storage
sEnd	_BSS

sBegin	MT_TEXT
	externNP B$fmldw
sEnd	MT_TEXT


sBegin	OS_TEXT
assumes CS,OS_TEXT


	SUBTTL	B$TIMR - elapsed time since midnight
	PAGE

;***
; B$TIMR - returns number of seconds past midnight of current day.
;
; Input:
;	NONE
; Output:
;	AX = ptr to s.p. number of seconds since midnight
; Modifies:
;	NONE
; Exceptions:
;****
cProc	B$TIMR,<PUBLIC,FAR,FORCEFRAME>,<SI,ES>
cBegin
	CALLOS	GETTIM		;CH=hour, CL=min, DH=sec, DL=Hundreths of sec
	MOV	AL,60		;convert hours to minutes, stay int as long as
	MUL	CH		;as possible, result in ax
	XOR	CH,CH		;cx = minutes
	ADD	AX,CX		;ax = number of minutes past midnight
	MOV	BX,AX		;bx = number of minutes past midnight
	PUSH	DX
	CALL	B$fmldw 	;ST0 = number of minutes past midnight
	POP	DX
	MOV	AX,60		;ah = 0 so "op" will be "*" in ST0 = ST0 op AL
	CALL	ST0OpAl 	;  (convert minutes to seconds)
	MOV	AL,DH		;al = number of seconds
	MOV	AH,1		;ah is 1 so "op" will be "+" in
	CALL	ST0OpAl 	;   ST0 = ST0 op AL
	MOV	AX,100		;[ah] = 0 so "op" will be "*" in ST0OpAl
	CALL	ST0OpAl 	;convert ST0 to hundreths of seconds
	MOV	AL,DL		;[al] = hundreths of seconds
	MOV	AH,1		;[ah] is 1 so "op" will be "+"
	CALL	ST0OpAl 	;in ST0 = ST0 op AL
	MOV	AX,0200H + 100d ;[ah] = 2 so "op" will be "/" in ST0OpAl
	CALL	ST0OpAl 	;convert ST0 to seconds from hundreths


	MOV	BX,OFFSET DGROUP:b$Buf1 ; BX = ptr to s.p. temp store loc.
				;of seconds
	FSTP	DWORD PTR [BX]	;put s.p. equivalent into FAC
	XCHG	AX,BX		;restore pointer to result (retval)
	FWAIT			;ensure result in RAM prior to return

cEnd

	PAGE
;***
;ST0OpAl - Add or multiply contents of AL and the ST0, returning result in ST0
;Purpose:
;	Either adds or mulitplies the contents of ST0 by the value in AL.
;	The result is returned in the ST0.
;Entry:
;	AH  -	0 then multiply
;		1 then add
;		else divide ST0 by AL
;	AL  - value to multiply/add to or divide into ST0
;Exit:
;	ST0 - contains result
;Preserves:
;	SI, DI, DX
;Exceptions:
;	None.
;****
cProc	ST0OpAl,<NEAR>,<DX>
cBegin
	PUSH	AX
	MOV	BL,AL
	XOR	BH,BH
	CALL	B$fmldw 	; ST0 = input value
	POP	AX
	OR	AH,AH
	JZ	ST0MUL		; brif we're to multiply ST1 by ST0
	DEC	AH
	JZ	ST0ADD		; brif we're to add ST0 to ST1


	FDIV			; ST0 = ST1/ST0
	JMP	SHORT ST0Ret
ST0MUL:
	FMUL			; ST0 = ST1 * ST0
	JMP	SHORT ST0Ret
ST0ADD:
	FADD			; ST0 = ST1 + ST0


ST0Ret:

cEnd	ST0OpAl


sEnd	OS_TEXT
	END
