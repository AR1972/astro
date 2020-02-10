;*** 
; FILE: ARCHITEC.INC - architecture-specific items for QBI
;
;	Copyright <C> 1985, 1986, 1987  Microsoft Corporation
;
;Purpose:
;
;   This file contains macros that allow us to optimize certain Intel-family
;   instructions depending on the version or context in which the
;   instructions occur.
;
;
;*******************************************************************************

ARCHITEC_INC = -1



;***
;PUSHI macro - push immediate data word
;
;Entry:
;	reg - register to use as intermediate in the case we're not
;		building for a 286 environment
;	wData - a word of data to be pushed
;Exit:
;	none.
;Modifies:
;	Will alter the given register if not HP_286.
;**************************************************************************


PushI	MACRO	reg,wData
	mov	reg,wData
	push	reg
	ENDM



;***
;SHIFT macro - SHL/SHR/SAL/SAR macro
;
;Purpose:
;	The 186 and 286 instruction sets allow shift instructions of the
;	form 'SHL AX,4', whereas the 8088/8086 instruction set would
;	require either four 'SHL AX,1' instructions or 'MOV CL,4' and
;	'SHL AX,CL' to do the same thing.
;	Note that, in the case where we're not counting on 286 instructions,
;	the code produced depends on the shift count. If the count is 4 or
;	less, individual 'SHL loc,1' instructions are generated. If the
;	shift count is greater, however, the shift is done via CL for size.
;Entry:
;	type - an 'H' or an 'A', depending on whether a Logical or Arithmetic
;		shift is desired
;	dir - an 'L' or an 'R', depending on whether a Left or a Right
;		shift is desired
;	loc - register or memory byte or word to shift. If memory, this
;		parameter should include the WORD PTR or BYTE PTR directive.
;	count - number of times to shift.
;Exit:
;	The given 'loc' is shifted in the given direction 'count' times.
;Modifies:
;	Will modify CL iff the shift count > 4 and not HP_286.
;**************************************************************************
SHIFT	MACRO	type,dir,loc,count
    if count LE 4		;;not HP_286 - can't assume 286 instruction set
	REPT	count
	S&type&dir	loc,1
	ENDM
else
	MOV	CL,count
	S&type&dir	loc,CL
endif
	endm

	;SKIP1_PSW skips 1 byte of code, altering PSW

SKIP1_PSW   equ     <db 3Ch>		;CMP AL,...

	;SKIP2_PSW skips 2 bytes of code, altering PSW

SKIP2_PSW   equ     <db 3Dh>		;CMP AX,...

	;SKIP1_AL skips 1 byte of code, altering AL (leaving PSW intact)

SKIP1_AL    equ     <db 0B0h>		;MOV AL,...

	;SKIP2_AX skips 2 bytes of code, altering AX (leaving PSW intact)

SKIP2_AX    equ     <db 0B8h>		;MOV AX,...


;***
;TESTX macro - optimize test immediate with a register to save bytes.
;
;Purpose:
;
;	Optimize test immediate with AX, BX, CX, or DX to limit the
;	immediate operand to a single byte when either the low byte or
;	the high byte of the immediate value is zero.
;	This is particularly useful in those cases where the immediate operand
;	is a symbolic constant - - - it can be changed and this macro will
;	adjust and optimize the code accordingly.
;
;	In the case of the AX register, TEST AH,Value is not generated.  The
;	TEST AX,Value instruction is the same size (3 bytes) and is also one
;	cycle faster on 8086/8088.  It is the same speed for the 286 and 386.
;
;	Note that the TESTM macro does the same thing, but for the case where
;	the immediate value is tested against a word in memory.
;
;Entry:
;	reg -	A register; must be one of: AX, BX, CX, or DX
;	value -	An immediate value.
;Exit:
;	none.
;Modifies:
;	PSW flags only
;**************************************************************************
TestX	macro	Reg,Value
	local	RegLet
RegLet	substr	<Reg>,1,1		;; First letter of register

	ife	    HIGH (Value)
%	    test    RegLet&&l,Value
	elseifidni  <Reg>,<ax>		;; test ax,? is better than test ah,?
	    test    Reg,Value
	elseife     LOW (Value)
%	    test    RegLet&&h,HIGH (Value)
	else
	    test    Reg,Value
	endif
	endm

;***
;TESTM macro - optimize test immediate with memory to save bytes.
;Purpose:
;	Optimize test immediate with word-sized memory locations to limit 
;	the immediate operand to a single byte when either the low byte or
;	the high byte of the immediate value is zero.
;	This is particularly useful in those cases where the immediate operand
;	is a symbolic constant - - - it can be changed and this macro will
;	adjust and optimize the code accordingly.
;
;	Note that the TESTX macro does the same thing, but for the case where
;	the immediate value is tested against a register.
;
;	Added as revision [1].
;Entry:
;	addr -	A word address which the value is to be tested against.
;		Note: Do not enclose this in square brackets.
;	value -	An immediate value.
;Exit:
;	none.
;Modifies:
;	PSW flags only
;**************************************************************************
TestM	macro	Addr,Value
	ife	HIGH (Value)
	    test    byte ptr [Addr],Value		;;Just test low byte
	elseife low (value)
	    test    byte ptr [Addr+1],HIGH (Value)	;;Just test high byte
	else
	    test    [Addr],Value			;;Test both bytes
	endif
	endm
