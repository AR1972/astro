page	49,132
	TITLE	excase - executors for SELECT CASE varients
;***
;excase - executors for SELECT CASE varients
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Executors for SELECT CASE varients.
;	Note that the actual comparison executors for case are in
;	exmathop.asm.
;
;
;****************************************************************************

	.xlist
	include		version.inc
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opcontrl
	IncludeOnce	opcodes
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

sBegin	CODE

subttl	SELECT CASE
page
;exStSelectCase2,exStSelectCase4,exStSelectCase8
;
;Purpose:
;      Syntax:	SELECT CASE <exp>
;
;      Pcode:	<exSelexp> opStSelectCase(oTx to <exp> before first CASE)
;
;	<exp> exStSelectCase<2|4|8> (oText)
;	      - Push an additional copy of <exp> on the stack
;		and unconditionally branch to oText.
;
;	The additional copy of the expression will be removed when a
;	successful CASE branch is taken.
;


MakeExe exStSelectCaseR8,opStSelectCase	
	fld	st			;Make additional copy
	jmp	short exStCaseDisp

MakeExe exStSelectCase4,opStSelectCase	
	pop	ax			;Pop top four bytes
	pop	dx
	push	dx			;Restore copy
	push	ax
	push	dx			;Make additional copy
	push	ax
	jmp	short exStCaseDisp

MakeExe exStSelectCase2,opStSelectCase	
	pop	ax			;Pop top two bytes
	push	ax			;Restore copy
	push	ax			;Make additional copy

exStCaseDisp:
	mov	si,PTRTX[si]		;Set up for dispatch
	DispMac

subttl	CASE
page
;exStCase<Lt|Le|Eq|Ge|Gt|Ne|><I2|I4|R8|SD>, exStCaseTo<I2|I4|R8|SD>
;
;Purpose:
;      Syntax:	CASE [IS <relop>] <const>
;
;      Pcode:	[opBol] <const> opStCase[<relop>]
;
;				 +-to beyond END SELECT
;				 |
;      Bound:	[exBol exBranch(oTx)] <const> exStCase[<relop>]<type>
;		exCaseBranch<type>(oTxF, oTxT)
;				    |	  |
;				    |	  +-To next exBol
;				    |
;				    +-To next CASE,ELSE CASE,or END SELECT
;
;      Syntax:	CASE IS <const> TO <const>
;
;      Pcode:	[opBol] <const> <const> opStCaseTo
;
;				 +-to beyond END SELECT
;				 |
;      Bound:	[exBol exBranch(oTx)] <const> <const> exStCaseTo<type>
;		exCaseBranch<type>(oTxF, oTxT)
;				    |	  |
;				    |	  +-To next exBol
;				    |
;				    +-To next CASE,ELSE CASE,or END SELECT
;
;      NOTE: The scanner inserts the non-listable exBranch and exStCaseBranch
;	     pcodes.
;
;
;
;	<exp> <exp1> exStCase<Lt|Le|Eq|Ge|Gt|Ne|><I2|I4|R4|R8|SD>
;	      - Evaluates and consumes top two expressions on stack
;		and emits TRUE or FALSE on stack based upon result.
;		These executors share code with the MathOp executors,
;		except for the SD variants which will not cause
;		the <exp> SD to be released if it was a temp.
;
;	<exp> <exp1> <exp2> exStCaseTo<I2|I4|R4|R8|SD>
;	      - Evaluates <exp> and determines if it falls within
;		the range defined by <exp1> and <exp2>.  All three
;		expressions are consumed, and a TRUE or FALSE is
;		emitted to the stack based on the result of the
;		evaluation.
;
;BIG NOTE:  These executors live in exmathop.asm, since they are very
;	    similar to many existing math executors.



subttl	Case Branch
page
;exCaseBranch2,exCaseBranch4,exCaseBranch8,exCaseBranchSD
;
;Purpose:
;	<exp> exCaseBranch<2|4|8|SD> (oTextF, oTextT)
;	      - Branches to oTextF or oTextT based on TRUE or FALSE
;		condition on stack.  Before taking a false branch, an
;		additional copy of the exStSelectCase expression is
;		placed on the stack. Before taking a TRUE branch, the
;		saved copy of the exStSelectCase exp is consumed and
;		deallocated if it is a string temp. This is non-listable
;		and inserted by the scanner.
;


MakeExe exCaseBranchR8,opNoList2 	
	pop	cx			;Pop off Boolean
	jcxz	exStSelectCaseR8 	;brif false, Copy <exp> and branch
	fstp	st(0)			;Remove operand
	jmp	short exCaseBranchTrueDispSd	

MakeExe exCaseBranch4,opNoList2 	
	pop	cx			;Pop off Boolean
	jcxz	exStSelectCase4 	;brif false, Copy <exp> and branch
exCaseBranchCom:
	pop	ax			;Remove 1/2 of <exp> copy
	jmp	short exCaseBranchTrueDisp  ;and take true branch

MakeExe exCaseBranch2,opNoList2 	
	pop	cx			;Pop off Boolean
	jcxz	exStSelectCase2 	;brif false, Copy <exp> and branch
exCaseBranchTrueDisp:
	pop	ax			;Remove <exp>
exCaseBranchTrueDispSd:
	inc	si			; skip false branch
	inc	si			
	jmp	short exStCaseDisp	;and take true branch

MakeExe exCaseBranchSd,opNoList2
	pop	cx			;Pop off Boolean
	    jcxz    exStSelectCase2	;brif false, Copy <exp> and branch
	call	B$FLEN			;dealc string if temp, <psd> on stack
	jmp	short exCaseBranchTrueDispSd ;take the TRUE path

subttl	CASE ELSE
page

;exStCaseElse2,exStCaseElse4,exStCaseElse8,exStCaseElseSD
;
;Purpose:
;	exStCaseElse<2|4|8|SD>
;	      - Consume both the copy of and the exStSelectCase exp.
;		Deallocate if it is a string temp.

MakeExe exStEndSelectR8,opStEndSelect	
	SkipExHeader			
MakeExe exStCaseElseR8,opStCaseElse	
	fpoptwo				;[6]Eat two operands
	DispMac

MakeExe exStEndSelect4,opStEndSelect	
	SkipExHeader			
MakeExe exStCaseElse4,opStCaseElse	
	add	sp,4
	SkipExHeader
MakeExe exStEndSelect2,opStEndSelect	
	SkipExHeader			
MakeExe exStCaseElse2,opStCaseElse	
	add	sp,4
exStCaseElseCom:
	DispMac

MakeExe exStEndSelectSD,opStEndSelect	
	SkipExHeader			
MakeExe exStCaseElseSD,opStCaseElse
	    pop     ax		    	;pop off val
	call	B$FLEN		    	;dealc string if temp, <psd> on stack
	jmp	short exStCaseElseCom


sEnd	CODE
end
