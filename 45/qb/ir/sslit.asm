page	,132
	TITLE	sslit	-	Scan Support for Literals
;***
;sslit - Scan Support for Literals
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   Scan literal pcodes.
;   Literal scan routines mark their stack entries for two reasons:
;	1. At procedure call time literals always coerce when the definition
;	   or declaration is available.  This means that users are not 
;	   required to use an explicit type character when calling a declared 
;	   or defined procedure.
;	2. For execution speed, I2 literals 0 through 10 are coerced to R4
;	   by replacing the executor.
;
;
;****************************************************************************

	.xlist
	include		version.inc
	IncludeOnce	scanner
	IncludeOnce	ssint
	IncludeOnce	pcode
	IncludeOnce	optables
	.list

	extrn	exLitDI2:far		

assumes cs, SCAN
assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA

sBegin	SCAN

;***
;Ss_Lit - Scan Literals 
;
;Purpose:
;
;   Scan literal opcodes opLit...
;   Stack entries for literals indicate type and the fact that they
;   are literals.  This difference allows literals to be coerced
;   rather than typechecked in procedure invocations for procedures
;   that are declared.
;   Furthermore, opLit0 through opLit10 may be coerced from I2 to R4
;   by changing the opcode.
;
;Input:
;
;Output:
;
;************************************************************************


mpLitI2OpExe	label	word
	DWEXT	exLitI20

I2Lit	=	1			;Next literal is one

	rept	opLitI2Max
	DWEXT	exLitI2%I2Lit		;;Generate table entry
I2Lit	=	I2Lit + 1		;;Bump literal value
	endm


SsProc	LitI2
	mov	al,byte ptr es:[si-1]	; Get MSB of opcode
	and	ax,HIGH (NOT OPCODE_MASK)
	.erre	OPCODE_MASK EQ 03ffh	; Assure SHR is correct
	shr	ax,1			; AX = Literal value * 2
	xchg	ax,bx			; AX = Opcode * 2, BX = Literal * 2
	mov	bx,mpLitI2OpExe[bx]	; BX = Executor address
	xchg	ax,bx			; AX = Executor, BX = Opcode * 2
	SKIP2_PSW			; Fall into scan routine below

SsProc	Lit
	;Emit the executor

	call	EmitExCopyOps		;Emit the executor and copy operands

	;Make the stack entry

	push	di			;FRAME: oTx of end of exp fragment
	mov	al,mpOpRule[bx] 	;Type is in the rule table
	mov	ah,al			;Type and flags in both bytes

	.errnz	ST_Lit AND 0FFFh	; Assure literal flags in range
	.errnz	ST_LitX AND 0FFFh	; Assure literal flags in range
	.erre	ET_MAX LT 0fh		; Assure ET type in range
	and	ax,0f00fh
	push	ax			;FRAME: Type with literal bits

	;Exit to the main loop

	jmp	[ScanRet]		;Return to the current scan loop


sEnd	SCAN
end
