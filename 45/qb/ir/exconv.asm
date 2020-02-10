page	49,132
	TITLE	exConv.asm - Data Type Conversion Functions
;***
;exConv.asm - Data type conversion functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module includes:
;	- all implicit and explicit data type conversion functions
;
;
;****************************************************************************

	.8087

	.xlist
	include 	version.inc
EXCONV_ASM = ON
	IncludeOnce	architec
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	pcode
	.list



assumes ss, DATA


sBegin	CODE
assumes cs, CODE



	subttl	Coercion executors
	page

	;Identity transformations first

MakeExe exFnCSngR8,opCoerce,ET_R4	
	SkipExHeader
MakeExe exFnCDblR8,opCoerce,ET_R8	
	SkipExHeader
MakeExe exFnCLngI4,opCoerce,ET_I4	
	SkipExHeader
MakeExe exFnCIntI2,opCoerce,ET_I2	
	DispMac

	;Coercions from I2

MakeExe exCoII4I2,opNoList0
	SkipExHeader			
MakeExe exFnCLngI2,opCoerce,ET_I4
	pop	ax
	cwd				;Convert to I4
	push	dx
	push	ax
	DispMac

MakeExe exFnCSngI2,opCoerce,ET_R4	
	SkipExHeader
MakeExe exCoIR8I2,opNoList0
	SkipExHeader
MakeExe exFnCDblI2,opCoerce,ET_R8	
	mov	bx,sp			;Source operand
	fild	word ptr DGROUP:[bx]	;Load source
	fwait				
	pop	ax			;Throw away I2
	DispMac




	;Coercions from I4


MakeExe exCoII2I4,opNoList0
	SkipExHeader
MakeExe exFnCIntI4,opCoerce,ET_I2	
	pop	ax
	cwd
	pop	cx
	push	ax			;Assume success
	cmp	cx,dx
	jnz	ErrOVF			;Overflow error
	DispMac

ErrOVF:
	jmp	exMathErrOVF		;Declare overflow error


MakeExe exFnCSngI4,opCoerce,ET_R4	
	SkipExHeader
MakeExe exCoIR8I4,opNoList0
	SkipExHeader
MakeExe exFnCDblI4,opCoerce,ET_R8	
	mov	bx,sp			;Source address
	fild	dword ptr DGROUP:[bx]	;Load
	fwait				
	add	sp,4			
	DispMac				



	;Coercions from R4/R8 and Any

MakeExe exCoII2R8,opNoList0
	SkipExHeader
MakeExe exFnCIntR8,opCoerce,ET_I2	
	sub	sp,2			;Destination address
i87ToI2:
	mov	bx,sp
	fistp	word ptr DGROUP:[bx]	;And xlat the chip value to the stack
	fwait				;Wait for the coprocessor to cool
	DispMac 			; and continue

MakeExe exCoII4R8,opNoList0
	SkipExHeader
MakeExe exFnCLngR8,opCoerce,ET_I4	
	sub	sp,4			
i87ToI4:				
	mov	bx,sp
	fistp	dword ptr DGROUP:[bx]	
	fwait
	DispMac





	;Coercions from Currency



	;Coercions from String




sEnd	CODE

	subttl	Table of Implicit Coercions
	page

;This table is entered by the scanner to get executors for performing
;implicit coercions.  The table is a two dimension array entered by current
;type and required result type.  Identity coercion occur with R4/R8, since
;these are the same on the 8087 stack.  The 0 entry flags the scanner not
;to insert an excutor.

sBegin	SCAN				    
assumes cs, SCAN			    

	;The table below is index as tImpCo[Source][Target], assuming the
	;right index varies most rapidly.

	;The table contains either an executor address or the special values
	;0 and 1.  ) and 1 can not be value executor addresses because each
	;executor must be preceeded by the opcode it maps to.  The special
	;value 0 indicates that the two types have identical representations
	;on the runtime stack and need no conversion.  The value 1 indicates
	;that the values are not compatible and a type mismatch error should
	;be generated at scan time.


	public	tImpCo
tImpCo	label	word

	Dw	0
	Dw	exCoII4I2
	Dw	exCoIR8I2
	Dw	exCoIR8I2
	Dw	1

	Dw	exCoII2I4
	Dw	0
	Dw 	exCoIR8I4
	Dw	exCoIR8I4
	Dw	1


	Dw	exCoII2R8
	Dw	exCoII4R8
	Dw	0
	Dw	0
	Dw	1


	Dw	exCoII2R8
	Dw	exCoII4R8
	Dw	0
	Dw	0
	Dw	1


	Dw	1
	Dw	1
	Dw	1
	Dw	1
	Dw	1
	Dw	0




sEnd	SCAN				
end
