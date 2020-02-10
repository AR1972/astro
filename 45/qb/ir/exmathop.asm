page	49,132
	TITLE	exMathOp - I2 Math Operators
;***
;exmathop.asm - executors for math operators and functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module includes:
;	- the I2 calculator functions.
;	- interface to the mathpack for all mathpack resident functions.
;
;
;****************************************************************************

	.8087

	.xlist
	include 	version.inc
EXMATHOP_ASM = ON
	IncludeOnce	architec
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opcontrl
	IncludeOnce	opintrsc
	IncludeOnce	opaftqb4
	IncludeOnce	rtinterp
	IncludeOnce	context
	IncludeOnce	qbimsgs
	IncludeOnce	extort
	IncludeOnce	ssint
	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA



PREC53	MACRO				;NOOP if NOT FV_PREC53
	ENDM

PREC64	MACRO				;NOOP if NOT FV_PREC53
	ENDM



extrn	B$CMI4:far	
extrn	__aFlmul:far

sBegin	CODE
;===============================================================================
subttl	I2 Math Support
page
MakeExe exAddI2,opAdd
	pop	ax
	mov	bx,sp
	add	DGROUP:[bx],ax
	jo	MulI2OVF
	DispMac

MakeExe exSubI2,opSub
	pop	ax		;Right hand arg
	mov	bx,sp
	sub	DGROUP:[bx],ax
	jo	MulI2OVF
	DispMac

MakeExe exMulI2,opMul
	pop	cx
	pop	ax
	imul	cx
	jo	MulI2OVF
	push	ax
	DispMac
MulI2OVF:
FnAbsI2OVF:				
UMiI2OVF:				
	jmp	exMathErrOVF

MakeExe exIdvI2,opIdv
	pop	cx		;Right hand arg
	pop	ax		;Left hand arg
	cwd
	jcxz	IdvI2DV0
	idiv	cx
	push	ax
	DispMac
exMod0:
IdvI2DV0:
	jmp	exMathErrDV0

MakeExe exUMiI2,opUMi
	mov	bx,sp
	neg	word ptr DGROUP:[bx]
	jo	UMiI2OVF		
	DispMac

MakeExe exModI2,opMod
	pop	cx
	pop	ax
	cwd
	jcxz	exMod0
	idiv	cx
	push	dx
exModDisp:
	DispMac

MakeExe exModI4,opMod
	pop	ax
	pop	bx
	pop	cx
	pop	dx
	push	bx
	push	ax
	push	dx
	push	cx			;MS Math(?)
	CALLRT	B$RMI4,DispMovDxAx	;MOD function

MakeExe exIDvI4,opIDv
	pop	ax
	pop	bx
	pop	cx
	pop	dx
	push	bx
	push	ax
	push	dx
	push	cx			;MS Math(?)
	CALLRT	B$DVI4,DispMovDxAx

MakeExe exFnAbsI2,opFnAbs
	pop	ax
	cwd
	xor	ax,dx
	sub	ax,dx
	jo	FnAbsI2OVF		
	push	ax
	DispMac

MakeExe exStCaseEQI2,opStCaseEQ
	SkipExHeader
MakeExe exStCaseI2,opStCase
	SkipExHeader
MakeExe exEQI2,opEQ
	pop	ax
	pop	cx
	xor	bx,bx
	cmp	ax,cx
	jnz	@F
	dec	bx
@@:
	push	bx
	DispMac

MakeExe exStCaseNEI2,opStCaseNE
	SkipExHeader
MakeExe exNEI2,opNE
	pop	ax
	pop	cx
	xor	bx,bx
	cmp	ax,cx
	jz	@F
	dec	bx
@@:
	push	bx
	DispMac

MakeExe exStCaseLEI2,opStCaseLE
	SkipExHeader
MakeExe exLEI2,opLE
	pop	cx		;Right arg
	pop	ax		;Left arg
	xor	bx,bx
	cmp	ax,cx
	jg	@F
	dec	bx
@@:
	push	bx
	DispMac

MakeExe exStCaseGEI2,opStCaseGE
	SkipExHeader
MakeExe exGEI2,opGE
	pop	cx		;Right arg
	pop	ax		;Left arg
	xor	bx,bx
	cmp	ax,cx
	jl	@F
	dec	bx
@@:
	push	bx
	DispMac

MakeExe exStCaseGTI2,opStCaseGT
	SkipExHeader
MakeExe exGTI2,opGT
	pop	cx		;Right arg
	pop	ax		;Left arg
	xor	bx,bx
	cmp	ax,cx
	jle	@F
	dec	bx
@@:
	push	bx
	DispMac

MakeExe exStCaseLTI2,opStCaseLT
	SkipExHeader
MakeExe exLTI2,opLT
	pop	cx		;Right arg
	pop	ax		;Left arg
	xor	bx,bx
	cmp	ax,cx
	jge	@F
	dec	bx
@@:
	push	bx
	DispMac

MakeExe exNotI2,opNot
	mov	bx,sp
	not	word ptr DGROUP:[bx]
	DispMac

MakeExe exAndI2,opAnd
	pop	ax
	pop	cx
	and	ax,cx
	push	ax
	DispMac

MakeExe exOrI2,opOr
	pop	ax
	mov	bx,sp
	or	DGROUP:[bx],ax
	DispMac

MakeExe exXorI2,opXor
	pop	ax
	mov	bx,sp
	xor	DGROUP:[bx],ax
	DispMac

MakeExe exEqvI2,opEqv
	pop	ax		;Right operand
	pop	cx		;Left operand
	xor	ax,cx
	not	ax
	push	ax
	DispMac

MakeExe exImpI2,opImp
	pop	ax		;Right hand arg
	pop	cx		 ;Left hand arg
	not	cx
	or	ax,cx
	push	ax
	DispMac

MakeExe exFnIntInt,opFnInt		
	SkipExHeader
MakeExe exFnFixInt,opFnFix		
	DispMac

MakeExe exFnSgnI2,opFnSgn
	xor	ax,ax			;Assume 0
	pop	bx
	or	bx,bx
	jz	PushAxDisp
	js	SgnSgn
	inc	ax
	jmp	short PushAxDisp
SgnSgn:
	dec	ax
	jmp	short PushAxDisp

;===============================================================================
subttl	I4 Math Support
page
MakeExe exAddI4,opAdd
	pop	ax
	pop	dx
	mov	bx,sp
	add	DGROUP:[bx],ax
	adc	DGROUP:[bx+2],dx
	jo	SubI4OVF
	DispMac

MakeExe exSubI4,opSub
	pop	ax
	pop	dx
	mov	bx,sp
	sub	DGROUP:[bx],ax
	sbb	DGROUP:[bx+2],dx
	jo	SubI4OVF
	DispMac
SubI4OVF:
FnAbsI4OVF:				
UMiI4OVF:				
	jmp	exMathErrOVF

MakeExe exMulI4,opMul
	call	__aFlmul		;Perform the multiply
	jb	SubI4OVF		;Declare overflows
	push	dx
	push	ax
	DispMac

MakeExe exFnSgnI4,opFnSgn
	pop	ax
	pop	dx
	or	ax,dx			;Test for zero
	jz	PushAxDisp		;Result = 0
	shl	dx,1			;PSW.C
	sbb	ax,ax			;PSW.NZ indicates input was neg
	jnz	PushAxDisp		; so return -1
	inc	ax			;Must have been positive
	jmp	short PushAxDisp

MakeExe exFnAbsI4,opFnAbs
	pop	ax
	pop	dx
	or	dx,dx
	jns	PushDxAxDisp
	not	ax			
	not	dx			
	add	ax,1			
	adc	dx,0
	jo	FnAbsI4OVF		
PushDxAxDisp:
	push	dx
PushAxDisp:
	push	ax
	DispMac

MakeExe exNotI4,opNot
	mov	bx,sp
	not	word ptr DGROUP:[bx]
	not	word ptr DGROUP:[bx+2]
	DispMac

MakeExe exUMiI4,opUMi
	pop	ax
	pop	dx
	not	ax			
	not	dx			
	add	ax,1			
	adc	dx,0
	jo	UMiI4OVF		
	jmp	short PushDxAxDisp

MakeExe exAndI4,opAnd
	pop	ax
	pop	dx
	mov	bx,sp
	and	DGROUP:[bx],ax
	and	DGROUP:[bx+2],dx
	DispMac

MakeExe exOrI4,opOr
	pop	ax
	pop	dx
	mov	bx,sp
	or	DGROUP:[bx],ax
	or	DGROUP:[bx+2],dx
	DispMac

MakeExe exXorI4,opXor
	pop	ax
	pop	dx
	mov	bx,sp
	xor	DGROUP:[bx],ax
	xor	DGROUP:[bx+2],dx
	DispMac

MakeExe exEqvI4,opEqv
	pop	ax
	pop	dx
	pop	cx
	pop	bx
	xor	ax,cx
	not	ax
	xor	dx,bx
	not	dx
	jmp	short PushDxAxDisp

MakeExe exImpI4,opImp
	pop	ax
	pop	dx		;Right hand arg
	pop	cx
	pop	bx		;Left hand arg
	not	cx
	not	bx
	or	ax,cx
	or	dx,bx
	jmp	short PushDxAxDisp

;***
;CompareI4 - I4 signed comparison routine
;Purpose:
;	Signed compare of two I4 numbers.
;
;Inputs:
;	ParmD Left	left hand argument
;	ParmD Right	right hand argument
;
;Function:
;	Set Carry and Zero flags based on input arguments.
;	Left > Right	Zero clear and carry clear
;	Left < Right	Zero clear and carry set
;	Left = Right	Zero set
;
;	Note: These are the flag conventions used by
;	      unsigned jumps, NOT signed jumps.
;
;Outputs:
;	Zero and carry flags have result (set up for signed jmp)
;***************************************************************************
Public	CompareI4
CompareI4:
	pop	ax		;Near
	push	cs		; to far
	push	ax		;   return address
	jmp	B$CMI4		;Perform and return to caller

MakeExe exStCaseLTI4,opStCaseLT
	SkipExHeader
MakeExe exLTI4,opLT
	call	CompareI4
	jb	I4TrueDisp	
	jmp	short I4FalseDisp

MakeExe exStCaseLEI4,opStCaseLE
	SkipExHeader
MakeExe exLEI4,opLE
	call	CompareI4
	ja	I4FalseDisp	
	jmp	short I4TrueDisp

MakeExe exStCaseGEI4,opStCaseGE
	SkipExHeader
MakeExe exGEI4,opGE
	call	CompareI4
	jae	I4TrueDisp	
	jmp	short I4FalseDisp

MakeExe exStCaseGTI4,opStCaseGT
	SkipExHeader
MakeExe exGTI4,opGT
	call	CompareI4
	ja	I4TrueDisp	
	jmp	short I4FalseDisp

MakeExe exStCaseEQI4,opStCaseEQ
	SkipExHeader
MakeExe exStCaseI4,opStCase
	SkipExHeader
MakeExe exEQI4,opEQ
	call	CompareI4
	jz	I4TrueDisp
I4FalseDisp:
	PUSHI	AX,FALSE
	DispMac

MakeExe exStCaseNEI4,opStCaseNE
	SkipExHeader
MakeExe exNEI4,opNE
	call	CompareI4
	jz	I4FalseDisp
I4TrueDisp:
	PUSHI	AX,<NOT FALSE>
	DispMac

;===============================================================================

	subttl	R8 math Support
	page

MovAxDisp MACRO RtEnt
if LOW (P&RtEnt+1)
	mov	ax,P&RtEnt+1
else
	mov	ax,(HIGH P&RtEnt)+100h
endif
	ENDM

;Rewritten with [21]
MakeExe exAddR8,opAdd
	PREC53				
	fadd
	PREC64				
	DispMac

MakeExe exSubR8,opSub
	PREC53				
	fsub
	PREC64				
	DispMac

MakeExe exMulR8,opMul
	PREC53				
	fmul
	PREC64				
	DispMac

MakeExe exDivR8,opDiv
	PREC53				
	fdiv
	PREC64				
	DispMac

MakeExe exPwrR8,opPwr
	call	CheckCONST		;Declare error if in CONST
	fxch				;Reverse order for runtime
	MovAxDisp	B$POW8		;Runtime entrypoint
	jmp	short DispR8		;Go do rt call, and exit


	subttl	R8 Math Support - Single argument executors
	page

MakeExe exFnSin,opFnSin
	MovAxDisp	B$SIN8
DispR8:
	call	ExToRtCall		;Perform the function in ax
	DispMac

	;End of [21]
	

MakeExe exFnCos,opFnCos
	MovAxDisp	B$COS8
	jmp	DispR8


MakeExe exFnTan,opFnTan
	MovAxDisp	B$TAN8
	jmp	DispR8


MakeExe exFnAtn,opFnAtn
	MovAxDisp	B$ATN8
	jmp	DispR8


MakeExe exFnExp,opFnExp
	MovAxDisp	B$EXP8
	jmp	DispR8



MakeExe exFnSqr,opFnSqr
	PREC53				
	fsqrt				;Do the root
	PREC64				
	DispMac



MakeExe exFnLog,opFnLog
	MovAxDisp	B$LOG8
	jmp	DispR8


MakeExe exFnIntR8,opFnInt
	MovAxDisp	B$INT8
	jmp	DispR8


MakeExe exFnFixR8,opFnFix
	MovAxDisp	B$FIX8
	jmp	DispR8


	;Rewritten with [21]

MakeExe exUMiR8,opUMi
	fchs
	DispMac

MakeExe exFnAbsR8,opFnAbs
	fabs
	DispMac

MakeExe exFnSgnR8,opFnSgn
	fldz			;Put zero on stack
	fcompp			;Compare with zero and remove operands
	fstsw	[Stat]		;Save result of comparison
	fwait
	mov	ax,[Stat]
	sahf
;Unsigned flags now set like 0 - operand
	mov	ax,1
	jb	SgnDone		;If operand was +, return 1
	jz	DecOnce
	dec	ax
DecOnce:
	dec	ax
SgnDone:
	push	ax
	DispMac

	subttl	R8 Compare Routines
	page

	DbPub	CompareR8
CompareR8:
	fxch				;Compare in right order
	fcompp				;Compare to other arg on the stack
	fstsw	[Stat]
	fwait
	mov	ax,[Stat]		;Load 87 status to ax
	sahf
	ret

	;End of [21]

;EQ
MakeExe exStCaseEQR8,opStCaseEQ
	SkipExHeader
MakeExe exStCaseR8,opStCase
	SkipExHeader
MakeExe exEQR8,opEQ
	call	CompareR8
	mov	ax,0
	jnz	@F		;NE
	dec	ax
@@:
	push	ax
	DispMac

;NE
MakeExe exStCaseNER8,opStCaseNE
	SkipExHeader
MakeExe exNER8,opNE
	call	CompareR8
	mov	ax,0
	jz	@F		;EQ
	dec	ax
@@:
	push	ax
	DispMac

;GT
MakeExe exStCaseGTR8,opStCaseGT
	SkipExHeader
MakeExe exGTR8,opGT
	call	CompareR8
	mov	ax,-1
	ja	@F		;GT
	inc	ax
@@:
	push	ax
	DispMac

;GE
MakeExe exStCaseGER8,opStCaseGE
	SkipExHeader
MakeExe exGER8,opGE
	call	CompareR8
	mov	ax,-1
	jnb	@F		;GE
	inc	ax
@@:
	push	ax
	DispMac

;LE
MakeExe exStCaseLER8,opStCaseLE
	SkipExHeader
MakeExe exLER8,opLE
	call	CompareR8
	mov	ax,0
	ja	@F		;GT
	dec	ax
@@:
	push	ax
	DispMac

;LT
MakeExe exStCaseLTR8,opStCaseLT
	SkipExHeader
MakeExe exLTR8,opLT
	call	CompareR8
	mov	ax,-1
	jb	@F		;LT
	inc	ax
@@:
	push	ax
	DispMac

;===============================================================================
subttl	CY Math Support
page


;===============================================================================
subttl	SD Math Support
page

MakeExe exAddSD,opAdd
	call	CheckCONST		;Declare error if in CONST
	CALLRT	B$SCAT,DispMovSd	;add strings

;***
;CompareSD, CompareSDNoRel - Compare for type ET_SD
;Purpose:
;	Near interface to B$SCMP.
;	Near interface to B$ISCMPNoRel.
;
; Function:
;	Set sign and Zero flags based on comparing left and right strings
;	[left] > [right]     carry clear and zero clear
;	[left] < [right]     carry set	 and zero clear
;	[left] = [right]     carry clear and zero set
;Input:
;	Two SDs on the stack.
;Output:
;	ax = FALSE
;	PSW set as per compare
;
;******************************************************************************

sEnd	CODE
sBegin	DATA

Vector	DW	(?)

sEnd	DATA
sBegin	CODE



	DbPub	CompareSDNoRel	    ;string compare without releasing temps
CompareSDNoRel:
	pop	[Vector]	    ;remove retaddr to access other values on stack
	    call    B$ISCMPNoRel    ;compare without releasing left side if temp.
	jmp	short CompareSDCom

	DbPub	CompareSD
CompareSD:
	pop	[Vector]	    ;remove retaddr to access other values on stack
	    call    B$SCMP
CompareSDCom:
	mov	ax,FALSE	    ;in case we want to return FALSE
	jmp	[Vector]	    ;return to caller

;CASE string comparisons

MakeExe exStCaseGESD,opStCaseGE
	call	CompareSDNoRel
	jae	SD_TrueRet	;brif greater than or equal to
	jmp	short SD_Ret

MakeExe exStCaseGTSD,opStCaseGT
	call	CompareSDNoRel
	ja	SD_TrueRet	;brif greater than (z, s clear)
	jmp	short SD_Ret

MakeExe exStCaseLTSD,opStCaseLT
	call	CompareSDNoRel
	jb	SD_TrueRet	;brif less than
	jmp	short SD_Ret

MakeExe exStCaseLESD,opStCaseLE
	call	CompareSDNoRel
	jna	SD_TrueRet	;brif less than or equal to
	jmp	short SD_Ret	;brif greater than - return false

MakeExe exStCaseEQSD,opStCaseEQ
	SkipExHeader
MakeExe exStCaseSD,opStCase
	call	CompareSDNoRel
	je	SD_TrueRet	;brif strings are equal
	jmp	short SD_Ret

MakeExe exStCaseNESD,opStCaseNE
	call	CompareSDNoRel
	jne	SD_TrueRet	;brif strings are not equal
	jmp	short SD_Ret

;String comparisons

MakeExe exGESD,opGE
	call	CompareSD
	jae	SD_TrueRet	;brif greater than or equal to
	jmp	short SD_Ret

MakeExe exGTSD,opGT
	call	CompareSD
	ja	short SD_TrueRet;brif greater than (z, c clear)
	jmp	short SD_Ret

MakeExe exLTSD,opLT
	call	CompareSD
	jb	SD_TrueRet	;brif less than
	jmp	short SD_Ret

MakeExe exLESD,opLE
	call	CompareSD
	jbe	SD_TrueRet	; brif less than or equal to
	jmp	short SD_Ret	

MakeExe exEQSD,opEQ
	call	CompareSD
	je	SD_TrueRet	;brif strings are equal
	jmp	short SD_Ret

MakeExe exNESD,opNE
	call	CompareSD
	je	SD_Ret		;brif strings are equal
SD_TrueRet:
	dec	ax
SD_Ret:
	push	ax
	DispMac

;CASE text comparisons



;===============================================================================
subttl	CASE TO comparison support for I2/I4/R4/R8/SD
page
;exStCaseToI2, exStCaseToI4, exStCaseToR4, exStCaseToR8, exStCaseToSD
;
;The stack contains three values coerced to the same type.  These
;routines emit a NOT FALSE onto the stack if exp1 is in inclusive
;range defined by [exp2,exp3], else a FALSE is emitted.

MakeExe exStCaseToR8,opStCaseTo
	fld	st(2)		;Duplicate input value
	call	CompareR8	;is upper bound < val?
	jb	ToR8_FalsePop	;brif so
	call	CompareR8	;is val < lower bound?
	jb	ToI4_False	;brif so
	jmp	short ToI4_True

ToR8_FalsePop:
	fpoptwo			;[21]Pop two values
	jmp	short ToI4_False

MakeExe exStCaseToI2,opStCaseTo
	pop	bx		;upper bound
	pop	cx		;lower bound
	pop	dx		;value
	xor	ax,ax		;initially FALSE
	cmp	dx,cx		;is val < lower bound?
	jl	ToI2_Ret	;brif so
	cmp	dx,bx		;is val > upper bound?
	jg	ToI2_Ret	;brif so
	dec	ax		;TRUE
ToI2_Ret:
	push	ax
	DispMac

MakeExe exStCaseToI4,opStCaseTo
	mov	bx,sp		;set up frame ptr
	push	DGROUP:[bx+10]	;push High word of val
	push	DGROUP:[bx+8]	;push low word of val
	call	CompareI4	;is upper bound < val?
	jb	ToI4_FalsePop	;brif so - pop off lower bound/val
	call	CompareI4	;is val < lower bound ?
	jb	ToI4_False	;brif so
ToI4_True:
	mov	ax,NOT FALSE
	jmp	short ToI2_Ret
ToI4_FalsePop:
	add	sp,8		;pop off lower bound and value
ToI4_False:
	xor	ax,ax
	jmp	short ToI2_Ret




MakeExe exStCaseToSD,opStCaseTo
	mov	bx,sp
	    push    DGROUP:[bx+4]   ;duplicate string descriptor
	    push    DGROUP:[bx]     ;duplicate upper bound
	call	CompareSDNoRel	    ;is val > upper bound?
	    pop     ax		    ;(pop off copy of upper bound)
	ja	ToSD_FalsePop	    ;brif so
	call	CompareSDNoRel	    ;is val < lower bound?
	jb	ToI4_False	    ;brif so
	jmp	short ToI4_True

ToSD_FalsePop:
	call	B$FLEN		    ;dealloc lower bound if it was a temp
	    pop     ax		    ;pop off val
	jmp	short ToI4_False

;======================================================================
	subttl	Like operator support (EB only)
	page


;======================================================================
	subttl	Math Error Handler Support
	page
;***
;exMathErr
;Purpose:
;	Route math errors to the BASIC error handler.
;
;Input:
;	bx = BASIC error number
;	si = <garbage>
;	di = <garbage>
;	sp = <garbage>
;	bp = <garbage>
;	es = <garbage>
;
;	grs.GRS_oTxCur = oTx of error
;
;Output:
;	none
;
;Exit:
;	B$SERR
;
;******************************************************************************
sEnd	CODE
extrn	B$SERR:far
sBegin	CODE

	;Declare error if in a CONST statement

CheckCONST:
	test	[SsFlags],SSF_ScanAndExec;Implies CONST is in progress
	jnz	exMathErrFC		;Inside ScanAndExec implies CONST
	ret				;evaluation is in progress

exMathErrFC:
	mov	bx,ER_FC		;Illegal Function call error
	jmp	short Err

	public	exMathErrOVF
exMathErrOVF:
	mov	bx,ER_OV		;Overflow error
	jmp	short Err

	public	exMathErrDV0
exMathErrDV0:
	mov	bx,ER_DV0		;/0 error
Err:
	mov	[grs.GRS_oTxCur],si	;Save oTx of error
exMathErr:
	cCall	B$SERR,bx		;Declare the error
	DbHalt	CODE,<exMathOp.asm - B$SERR Returned!>


;===============================================================================

	subttl	Any type executors needed for Forms
	page


;===============================================================================

	subttl	opcode to executor maps for math routines
	page

	;These tables are used by scan routines to map opcodes to executors.

sEnd	CODE
sBegin	SCAN				
assumes cs, SCAN			

NOTIMP	MACRO
	DW	0
	ENDM

	public	mUMiOpExe
mUMiOpExe:
	DW	exUMiI2
	DW	exUMiI4
	DW	exUMiR8
	DW	exUMiR8

	public	mNEOpExe
mNEOpExe:
	DW	exNEI2
	DW	exNEI4
	DW	exNER8
	DW	exNER8
	DW	exNESD

	public	mGEOpExe
mGEOpExe:
	DW	exGEI2
	DW	exGEI4
	DW	exGER8
	DW	exGER8
	DW	exGESD

	public	mLEOpExe
mLEOpExe:
	DW	exLEI2
	DW	exLEI4
	DW	exLER8
	DW	exLER8
	DW	exLESD

	public	mEQOpExe
mEQOpExe:
	DW	exEQI2
	DW	exEQI4
	DW	exEQR8
	DW	exEQR8
	DW	exEQSD

	public	mGTOpExe
mGTOpExe:
	DW	exGTI2
	DW	exGTI4
	DW	exGTR8
	DW	exGTR8
	DW	exGTSD

	public	mLTOpExe
mLTOpExe:
	DW	exLTI2
	DW	exLTI4
	DW	exLTR8
	DW	exLTR8
	DW	exLTSD

	public	mOrOpExe
mOrOpExe:
	DW	exOrI2
	DW	exOrI4

	public	mAndOpExe
mAndOpExe:
	DW	exAndI2
	DW	exAndI4

	public	mXorOpExe
mXorOpExe:
	DW	exXorI2
	DW	exXorI4

	public	mNotOpExe
mNotOpExe:
	DW	exNotI2
	DW	exNotI4

	public	mFnSgnOpExe
mFnSgnOpExe:
	DW	exFnSgnI2
	DW	exFnSgnI4
	DW	exFnSgnR8
	DW	exFnSgnR8

	public	mImpOpExe
mImpOpExe:
	DW	exImpI2
	DW	exImpI4

	public	mEqvOpExe
mEqvOpExe:
	DW	exEqvI2
	DW	exEqvI4

	public	mPwrOpExe
mPwrOpExe:
	DW	exPwrR8 		; Need I2 entry in case of type mismatch
	NOTIMP
	DW	exPwrR8
	DW	exPwrR8

	public	mMulOpExe
mMulOpExe:
	DW	exMulI2
	DW	exMulI4
	DW	exMulR8
	DW	exMulR8

	public	mAddOpExe
mAddOpExe:
	DW	exAddI2
	DW	exAddI4
	DW	exAddR8
	DW	exAddR8
	DW	exAddSD

	public	mSubOpExe
mSubOpExe:
	DW	exSubI2
	DW	exSubI4
	DW	exSubR8
	DW	exSubR8

	public	mDivOpExe
mDivOpExe:
	DW	exDivR8 		; Need I2 entry in case of type mismatch
	NOTIMP
	DW	exDivR8
	DW	exDivR8

	public	mIdvOpExe
mIdvOpExe:
	DW	exIdvI2
	DW	exIdvI4

	public	mModOpExe
mModOpExe:
	DW	exModI2
	DW	exModI4

	public	mFnAbsOpExe
mFnAbsOpExe:
	DW	exFnAbsI2
	DW	exFnAbsI4
	DW	exFnAbsR8
	DW	exFnAbsR8

	public	mFnIntOpExe
mFnIntOpExe:
	DW	exFnIntInt		
	DW	exFnIntInt		
	DW	exFnIntR8
	DW	exFnIntR8

	public	mFnFixOpExe
mFnFixOpExe:
	DW	exFnFixInt		
	DW	exFnFixInt		
	DW	exFnFixR8
	DW	exFnFixR8


sEnd	SCAN				
end
