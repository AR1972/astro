page	49,132
	TITLE	ExLit	- exLiteral Executors
;***
;exLit.asm - executors for literals
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains all literal executors.
;
;   Numeric literal executors include:
;	- support for arbitrary literals.  This includes literal executors
;	  of each numeric data type with an operand of the size of the data
;	  type.
;	- special support for common numeric literals.  For text size reasons
;	  I2 and R4 literal executors for the literals 0 through 10 are
;	  provided.  These executors are also considered  speed critical.
;
;   The string literal executor has two operands - a word sized byte
;   count and the actual string literal.  The text pointer must be
;   updated to the next even byte boundary by the string literal executor
;   as all text is on even byte boundaries.
;
;
;****************************************************************************


	.xlist
	include		version.inc
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	variable
	IncludeOnce	opintrsc
	IncludeOnce	opaftqb4
	IncludeOnce	pcode
	IncludeOnce	debug
	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

sBegin	CODE

I2	dw	2
I3	dw	3
I4	dw	4
I5	dw	5
I6	dw	6
I7	dw	7
I8	dw	8
I9	dw	9
I10	dw	10

;Rewritten with [9]
MakeExe exLitR80,opLitI2,0		
	fldz
	DispMac

MakeExe exLitR81,opLitI2,1		
	fld1
	DispMac

MakeExe exLitR82,opLitI2,2		
	mov	bx,codeOFFSET I2
LitR8:
	fild	word ptr cs:[bx]
	DispMac

MakeExe exLitR83,opLitI2,3		
	mov	bx,codeOFFSET I3
	jmp	LitR8

MakeExe exLitR84,opLitI2,4		
	mov	bx,codeOFFSET I4
	jmp	LitR8

MakeExe exLitR85,opLitI2,5		
	mov	bx,codeOFFSET I5
	jmp	LitR8

MakeExe exLitR86,opLitI2,6		
	mov	bx,codeOFFSET I6
	jmp	LitR8

MakeExe exLitR87,opLitI2,7		
	mov	bx,codeOFFSET I7
	jmp	LitR8

MakeExe exLitR88,opLitI2,8		
	mov	bx,codeOFFSET I8
	jmp	LitR8

MakeExe exLitR89,opLitI2,9		
	mov	bx,codeOFFSET I9
	jmp	LitR8

MakeExe exLitR810,opLitI2,10		
	mov	bx,codeOFFSET I10
	jmp	LitR8
;End of [9]

MakeExe	exNull,opNull
	SkipExHeader			;fall through to share exLitI20 code
MakeExe exLitI20,opLitI2,0		
	xor	ax,ax
	push	ax
	DispMac

MakeExe exLitI21,opLitI2,1		
	mov	al,1
LitI2_Gen:
	xor	ah,ah
	push	ax
	DispMac

MakeExe exLitI22,opLitI2,2		
	mov	al,2
	jmp short LitI2_Gen
MakeExe exLitI23,opLitI2,3		
	mov	al,3
	jmp short LitI2_Gen
MakeExe exLitI24,opLitI2,4		
	mov	al,4
	jmp short LitI2_Gen
MakeExe exLitI25,opLitI2,5		
	mov	al,5
	jmp short LitI2_Gen
MakeExe exLitI26,opLitI2,6		
	mov	al,6
	jmp short LitI2_Gen
MakeExe exLitI27,opLitI2,7		
	mov	al,7
	jmp short LitI2_Gen
MakeExe exLitI28,opLitI2,8		
	mov	al,8
	jmp short LitI2_Gen
MakeExe exLitI29,opLitI2,9		
	mov	al,9
	jmp short LitI2_Gen
MakeExe exLitI210,opLitI2,10		
	mov	al,10
	jmp short LitI2_Gen

MakeExe	exLitHI2,opLitHI2
	SkipExHeader			;Fall through to the next executor
MakeExe	exLitOI2,opLitOI2
	SkipExHeader			;Fall through to the next executor
MakeExe	exLitDI2,opLitDI2
LitI2:
	LODSWTX
Disp_Ax:
	push	ax
	DispMac

MakeExe	exUndef,opUndef
	mov	ax,UNDEFINED
	jmp	short Disp_Ax		;push UNDEFINED and dispatch



MakeExe	exLitR4,opLitR4
	fld	dword ptr es:[si]	
	add	si,4			
	DispMac				

MakeExe	exLitR8,opLitR8
	fld	qword ptr es:[si]	
	add	si,8			
	DispMac				

MakeExe	exLitHI4,opLitHI4
	SkipExHeader			;Fall through to the next executor
MakeExe	exLitOI4,opLitOI4
	SkipExHeader			;Fall through to the next executor
MakeExe	exLitDI4,opLitDI4
	LODSWTX
	xchg	ax,dx
	LODSWTX
Lit4:
	push	ax
Lit4_Exit:
	push	dx
	DispMac

MakeExe	exLitSD,opLitSD
	LODSWTX				;ax = length of string in bytes
	push	es			;3 parms to B$LDFS
	push	si
	push	ax
	inc	ax			;round count up - - - pcodes always
	and	al,0FEh			;  fall on even byte boundaries, so if
					;  string size is odd, a byte of garbage
					;  filler is put in pcode here
	add	si,ax			;move text ptr to next opcode
	CALLRT	B$LDFS,DispMovSd

sEnd	CODE
end
