page	49,132
	TITLE	exfor - FOR/NEXT executors
;***
;exfor - FOR/NEXT executors
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains all executors for FOR and NEXT.
;
;	In general, these executors are very speed critical.
;
;	FOR executors are:
;
;	exStFor<Step|><I2|I4|R4|R8|CY>
;
;	<Step|> - indicates whether there is a STEP clause.  This variant has
;		  one more argument then other variants.
;
;	<type> - FOR receives stack arguments of a specific type
;
;	NEXT executors are:
;
;	exStNext<Step|><Id|><type>
;
;	<Step|> - NEXT in the case that no STEP clause was found can be made
;		  faster, as the STEP need not be loaded, and no checking is
;		  necessary for the STEP sign.
;
;	<Id|> - This variation is for listability.  Due to memory movement,
;		it is always necessary to execute a exIdRf before the NEXT.
;		The lister can't tell whether to list this op without help.
;		This is a usability issue.  The variants without Id are made
;		to share code with Id variants to conserve size.
;
;	<type> - NEXT is specific by type.  R8 data type FOR/NEXT are less
;		 speed critical than other types.  Size considerations require
;		 that R8 NEXT variants share code.
;
;
;****************************************************************************

	.8087

	.xlist
	include 	version.inc
EXFOR_ASM	=	ON
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opcontrl
	IncludeOnce	rtinterp
	IncludeOnce	variable
	.list

extrn	B$CMI4:far			



GetpFrame   MACRO			;Re-defining macro in exint.inc
	xchg	bx,ax			;;Get oBP to bx
	add	bx,bp
	ENDM



assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

sBegin	CODE

	subttl	FOR I2 Variants
	page
MakeExe exStForI2,opStFor
	LODSWTX 			;Pick up offset to FOR block
	GetpFrame
	pop	cx
	mov	[pFrame],cx 		;Limit to FOR block
	pop	ax			;Initial value for variable
	pop	bx			;Address of variable value
	mov	si,PTRTX[si]		;Text branch to the operand of NEXT
	jmp	StNextForI2		;FOR entry in matching NEXT executor


MakeExe exStForStepI2,opStForStep
	LODSWTX 			;Pick up offset to FOR block
	GetpFrame
	pop	cx			;Step
	mov	[pFrame],cx 		;Step to FOR block
	pop	ax			;Limit
	mov	[pFrame+2],ax		;Limit to FOR block
	pop	dx			;Initial value
	pop	bx			;Address of index variable value
	mov	[bx],dx 		;Assign initial value to variable
	mov	si,PTRTX[si]		;Branch to the NEXT operand
	jmp	StNextStepForI2 	;FOR entry in matching NEXT executor

subttl	FOR I4 Variants
page
MakeExe exStForI4,opStFor
	xor	ax,ax			;Supply the STEP for the user
	push	ax
	inc	ax
	push	ax			; push an (I4)1 and use the same NEXT
	SkipExHeader
MakeExe exStForStepI4,opStForStep
	LODSWTX 			;Pick up offset to FOR block
	GetpFrame
	pop	[pFrame]		;Step second word
	pop	[pFrame+2]		;Step first word
	pop	[pFrame+4]		;Limit second word
	pop	[pFrame+6]		;Limit first word
	pop	cx			;Initial value second word
	pop	dx			;Init value first word
	xchg	ax,FrameReg		;ax = FOR block address
	pop	bx			;pVar
	mov	si,PTRTX[si]		;Branch to the NEXT operand
	jmp	StNextForI4		;Exit through NEXT code

;ax    = FOR block address
;ds:bx = var address
;dx/cx = initial value

subttl	FOR R4 Variants
page
;NonSTEP variants
MakeExe exStForR4,opStFor
	fld1				;NonSTEP variants push a 1 for STEP
	SkipExHeader
;Step variants
MakeExe exStForStepR4,opStForStep
	LODSWTX 			;Pick up offset to FOR block
	GetpFrame
	fstp	dword ptr [pFrame]	;Get step
	fstp	dword ptr [pFrame+4]	;Get limit
	xchg	ax,FrameReg		;ax = FOR block address
	mov	si,PTRTX[si]		;Skip to NEXT
	pop	bx			;pVar
	jmp	StForNextR4		;Exit through NEXT with
					;  pFrame = FOR block address
					;  bx = variable

subttl	FOR R8 Variants
page

MakeExe exStForR8,opStFor
;R8 without step is fast enough and smaller if FOR simply supplies the step
	fld1
	SkipExHeader
MakeExe exStForStepR8,opStForStep
	LODSWTX 			;Pick up offset to FOR block
	GetpFrame
	fstp	qword ptr [pFrame]	;Get step
	fstp	qword ptr [pFrame+8]	;Get limit
	xchg	ax,FrameReg		;ax = FOR block address
	mov	si,PTRTX[si]		;Skip to NEXT
	pop	bx			;Variable address
	jmp	StForNextR8		;Exit through NEXT with
					;  pFrame or ax = FOR block address
					;  bx = variable

subttl	FOR CY Variants
page


subttl	NEXT I2 Variants Without STEP
page

MakeExe exStNextI2,opStNext
	SkipExHeader
MakeExe exStNextIdI2,opStNextId
	LODSWTX 			;Pick up oBp of parameters
	GetpFrame
	mov	cx,[pFrame] 		;Pick up Limit
	pop	bx			;Var ref
	mov	ax,[bx] 		;Load index value
	inc	ax			;Perform the step
	jo	NextI2OverflowErr	;Error - overflow
StNextForI2:				;FOR entry in matching NEXT executor
	mov	[bx],ax 		;Store in variable
	cmp	ax,cx			;Test var against limit
	jg	ExitLoop		;Loop end conditions met
NextCont:
	mov	si,PTRTX[si]		;Branch to beyond FOR
	DispMac 			;And on with the show

ExitLoop:
	inc	si			;Skip operand
	inc	si
	DispMac 			; and go

NextI2OverflowErr:
	jmp	exMathErrOVF

	subttl	NEXT I2 Variants With STEP
	page

StepDown:
	cmp	[bx],ax 		;Test var against limit
	jge	NextCont		;Take another lap
	jmp	short ExitLoop		;Skip operand and continue

MakeExe exStNextStepI2,opStNext
	SkipExHeader
MakeExe exStNextIdStepI2,opStNextId
	LODSWTX 			;Pick up oBp of parameters
	GetpFrame
	mov	cx,[pFrame] 		;Pick up step
	mov	ax,[pFrame+2]		;Pick up Limit
	pop	bx			;Var ref
	add	[bx],cx 		;Add step to variable
NextI2OverflowErrJ:
	jo	NextI2OverflowErr	;Error - overflow
StNextStepForI2:			;FOR continuation
	or	cx,cx			;Test sign of step
	js	StepDown		;Negative step
	cmp	[bx],ax 		;Test var against limit
	jg	ExitLoop		;Loop end conditions met
	mov	si,PTRTX[si]		;Branch to beyond FOR
	DispMac 			;And on with the show

	subttl	Next I4 Variants
	page

MakeExe exStNextStepI4,opStNext
	SkipExHeader			;Extra op for listability
MakeExe exStNextIdStepI4,opStNextId
	LODSWTX 			;Pick up oBp of parameters
	GetpFrame
	mov	cx,[pFrame]
	mov	dx,[pFrame+2]		;dx/ax = step
	xchg	ax,FrameReg		;ax = FOR block address
	pop	bx			;Var ref

;Add in step - error if overflow.
	add	cx,[bx] 		;Add in low word of value
	adc	dx,[bx+2]		;Add in high word of value + PSW.C
	jo	NextI2OverflowErrJ	;Error if overflow

	;Entry for FOR code.
	;dx/cx = index value
	;ds:bx = pVar
	;ax    = pFOR block
StNextForI4:

	;Perform assignment to variable

	mov	[bx+2],dx
	mov	[bx],cx 		;Assignment complete

;Compare var to limit.
	push	dx
	push	cx			;Push current value
	xchg	FrameReg,ax		;Restore FOR block address
	push	[pFrame+6]
	push	[pFrame+4]		;Push limit
	call	B$CMI4			; Compare two I4s on the stack
	    lahf			;Put flags in AH

	test	byte ptr [pFrame+3],80H	;Test sign of step
	jnz	NextNegStepI4		;Negative step
	sahf
	ja	NextLeave		;Leave loop if index > limit
	jmp	short NextContI4R4R8	;Exit through shared code

NextNegStepI4:				
	sahf				
	jb	NextLeave		;Leave loop if index < limit
	jmp	short NextContI4R4R8	


subttl	NEXT R4 Variants
page
;There are no nonSTEP versions of R4 NEXT as FOR simply supplies the STEP
; when the user doesn't.

MakeExe exStNextStepR4,opStNext
	SkipExHeader
MakeExe exStNextIdStepR4,opStNextId
	LODSWTX 			;Pick up oBp of parameters
	GetpFrame
	pop	ax			;Address of variable

;Need to:
; 1. add STEP to the variable whose address is in ax
; 2. compare LIMIT to variable whose value is on the stack.
; 3. compare sign of step with this previous compare to get the
;    final disposition.

;Add STEP.
;   The mathpack entry point does not modify the variable in the case
;   that an error (such as overflow or underflow) is detected.
;   ax     = index variable address
;   pFrame = FOR block address

	fld	dword ptr [pFrame]	;Load the step
	xchg	ax,bx			;bx points to variable
	fadd	dword ptr [bx]		;Add variable to step

DbPub StForNextR4
StForNextR4:				;Entry from R4 FOR executors

;Compare index variable with limit
;   bx = index variable address
;   ax or pFrame = FOR block address

	fst	dword ptr [bx]		; and store in variable
	xchg	ax,FrameReg
	fld	dword ptr [pFrame+4]	;Load the limit
	fcompp				;Compare to the variable
	fstsw	DGROUP:stat		;fstsw	ax is 80287 only
	fwait
	mov	ax,DGROUP:stat 		;Put status word in ax
	.errnz	LOW MATH_R4SignMask
	test	byte ptr[pFrame+3],HIGH MATH_R4SignMask	;Test for Sign of step

NextComR4R8X:
	jnz	NextNegStep		;Negative step
	sahf
	jb	NextLeave		;Leave loop if si > di
NextContI4R4R8:
	mov	si,PTRTX[si]		;Continue loop
	DispMac

NextLeave:
	inc	si		;Skip oTxt field
	inc	si
	DispMac 		; and continue

NextNegStep:
	sahf
	ja	NextLeave	;Leave loop
	jmp	short NextContI4R4R8

sEnd	CODE
sBegin	DATA
	public	Stat
Stat	dw	(?)
sEnd	DATA
sBegin	CODE

subttl	EXIT FOR
page
;exStExitFor and exStExitDo are simply a GOTO at execution time.

MakeExe exStExitDo,opStExitDo
	SkipExHeader
MakeExe exStExitFor,opStExitFor
	jmp	short NextContI4R4R8	;Branch to the exit point
					; like a NEXT loop continuation

subttl	NEXT R8 Variants
page
;There are no nonSTEP versions of R8 NEXT as FOR simply supplies the STEP
; when the user doesn't.

MakeExe exStNextStepR8,opStNext
	SkipExHeader
MakeExe exStNextIdStepR8,opStNextId
	LODSWTX 			;Pick up oBp of parameters
	GetpFrame
	pop	ax			;Address of variable

;Need to:
; 1. add STEP to the variable whose address is in ax.
; 2. compare LIMIT to variable whose value is on the stack.
; 3. compare sign of step with this previous compare to get the
;    final disposition.

;Add STEP.
;   The mathpack entry point does not modify the variable in the case
;   that an error (such as overflow or underflow) is detected.
;
;   ax     = index variable address
;   pFrame = FOR block address

	fld	qword ptr [pFrame]	;Load the step
	xchg	ax,bx			;bx points to variable
	fadd	qword ptr [bx]		;Add variable to step

StForNextR8:				;Entry from R8 FOR executors
;Compare index variable with limit
;   bx = index variable address
;   ax or pFrame = FOR block address

	fst	qword ptr [bx]		;Store in variable
	xchg	ax,FrameReg
	fld	qword ptr [pFrame+8]	;Load the limit
	fcompp				;Compare to the variable
	fstsw	stat			;fstsw	ax is 80287 only
	fwait
	mov	ax,stat 		;Put status word in ax
.errnz	LOW MATH_R8SignMask
	test	byte ptr[pFrame+7],HIGH MATH_R8SignMask ;Test for Sign of step check
	jmp	NextComR4R8X

subttl	NEXT CY Variants
page

;There are no nonSTEP versions of CY NEXT as FOR simply supplies the STEP
; when the user doesn't.


sEnd
end
