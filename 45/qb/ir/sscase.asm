page	49,132
TITLE	sscase - scan support for SELECT/CASE related opcodes
;***
;sscase.asm
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Scan SELECT CASE statement opcodes.
;
;   Runtime behavior of SELECT CASE executors:
;   ------------------------------------------
;	<exp> exStSelectCase<2|4|8> (oText)
;	      - Push an additional copy of <exp> on the stack
;		and unconditionally branch to oText.
;
;	<exp> <exp1> exStCase<Lt|Le|Eq|Ge|Gt|Ne|><I2|I4|R4|R8|CY|SD|TX>
;	      - Evaluates and consumes top two expressions on stack
;		and emits TRUE or FALSE on stack based upon result.
;		These executors share code with the MathOp executors,
;		except for the SD variants which will not cause
;		the <exp> SD to be released if it was a temp.
;
;	<exp> <exp1> <exp2> exStCaseTo<I2|I4|R4|R8|CY|SD|TX>
;	      - Evaluates <exp> and determines if it falls within
;		the range defined by <exp1> and <exp2>.  All three
;		expressions are consumed, and a TRUE or FALSE is
;		emitted to the stack based on the result of the
;		evaluation.
;
;	<exp> exCaseBranch<2|4|8|SD> (oTextF, oTextT)
;	      - Branches to oTextF or oTextT based on TRUE or FALSE
;		condition on stack.  Before taking a false branch, an
;		additional copy of the exStSelectCase expression is
;		placed on the stack. Before taking a TRUE branch, the
;		saved copy of the exStSelectCase exp is consumed and
;		deallocated if it is a string temp. This is non-listable
;		and inserted by the scanner.
;
;	exStCaseElse<2|4|8|SD>
;	      - Consume copy of exStSelectCase exp and deallocate if it
;		is a string temp.
;
;	exStEndSelect
;	      - Consume copy of exStSelectCase exp and deallocate if it
;		is a string temp.
;
;	exBranch (oText)
;	      - Unconditionally branch to oText.  This is non-listable
;		and inserted by the scanner at the beginning of each
;		line containing an exStCase* executor.
;
;
;   SELECT CASE/END SELECT statement syntax to pcode mappings:
;   ----------------------------------------------------------
;
;      Syntax:	SELECT CASE <exp>
;
;      Pcode:	<exSelexp> opStSelectCase(oTx to <exp> before first CASE)
;
;      ============================================================
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
;      NOTE: The scanner inserts the non-listable exBranch and exStCaseBranch
;	     pcodes.
;
;      ============================================================
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
;      ============================================================
;      Syntax:	CASE ELSE
;
;      Pcode:	opBol opStCaseElse
;
;      ============================================================
;      Syntax:	END SELECT
;
;      Pcode:	opBol opStEndSelect
;
;
;
;****************************************************************************

	.xlist
	include		version.inc
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	.list

assumes ds, DATA
assumes es, NOTHING
assumes ss, DATA
assumes cs, SCAN

sBegin	SCAN

	subttl	SELECT scan support.
	page
;***
;Ss_Select
;Purpose:
;	Scan entries for SELECT.
;
;	Scan tasks for SELECT include:
;	- ensuring the entry type is a fundamental data type.
;	- selecting the SELECT executor varient for the argument data type.
;	- pushing a SELECT CASE frame on the scan stack as follows:
;		push  oTx of SELECT operand for oTxFalse branch
;		push  UNDEFINED for start of oTxTrue chain
;		push  UNDEFINED for start of exBranch chain
;		push  oTyp of Select expression
;		push  CASE frame label
;Input:
;	Standard scan entrypoint
;Output:
;	Standard scan exit
;***************************************************************************

SsProc	Select
	pop	ax		;Get oTyp of select expression (Record = ET_RC)

	if	ET_MaxStr NE ET_MAX	; Something defined beyond ET_Fx
	    .erre   ST_Typ_Mask EQ 0ffh ; Assure we can use AL
	    cmp     al,ET_MaxStr	
	    jbe     @F			
	    .erre   ET_RC EQ 0		; Assure XOR is sufficient
	    xor     ax,ax		; Treat as if a record
@@:					
	endif				; ET_MaxStr NE ET_MAX
	and	ax,ST_Typ_Mask		
	.erre	ET_RC EQ 0		; Assure JNZ is sufficient
	jnz	@F			
	call	TMError 		
	inc	ax			; Force valid type (ET_I2)
@@:					
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_FS		
	jb	@F			

	    .erre   ET_FS EQ ET_MaxStr	;[1]
	    .erre   ET_SD EQ ET_FS-1	; Assure difference is 1
	    dec     ax			; Map fixed to non-fixed types
@@:					
	push	ax			; Save for later but clear flags
	call	MapEmitExe		;Map and emit executor
	pop	ax			;oTyp of Select expression
	pop	cx			;Throw away exp address
	push	di			;FCASE_oTxFalse

	; initially bind FALSE branch to after this executor in case of
	; multiple case items on a single line.

	MOVSWTX 		;skip operand for SELECT
	mov	PTRTX[di-2],di	;bind operand to next executor
	mov	cx,UNDEFINED
	push	cx		;start of FCASE_oTxTrue chain
	push	cx		;start of FCASE_oTxBranch chain
	push	ax		;FCASE_oTyp of select expression
	PUSHI	ax,STYP_Case	;FCASE_Id - SELECT CASE frame identifier
	or	[SsFlags],SSF_StSelect ;We need to verify no executable
				;statements come before nexe CASE, CASE ELSE,
				;or END SELECT
	jmp	[ScanRet]

subttl	CASE item scan support.
page
;***
;Ss_Case, Ss_CaseTo, Ss_CaseElse
;Purpose:
;	Scan entries for CASE [IS <relop>] const, CASE IS const TO const,
;	and CASE ELSE.
;
;	Scan tasks for CASE and CASE TO include:
;	- ensuring correct CASE item nesting.
;	- coercing arguments to SELECT CASE expression oTyp.
;	- selecting the CASE item executor variant.
;	- If this is first CASE item after BOS
;	  +  Insert an exBranch after BOS
;	  +  link exBranch operand into exBranch chain.
;	  +  binding previous CASE item (SELECT CASE) false branch.
;	     This is only necessary for the BOS case, The false
;	     branch is initially bound to the immediately following executor.
;	- Insert exCaseBranch variant with two operands.
;	- link True branch operand into oTxTrue branch chain.
;	- set oTxFalse branch to False branch operand and bind operand to next
;	  executor.
;	- set CaseItem processed flag
;
;	Note:  The exBranch operand chain is bound at END SELECT.  The
;	exCaseBranch chain is bound at BOS.
;
;	Scan tasks for CASE ELSE include:
;	- ensuring correct CASE item nesting.
;	- selecting the CASE ELSE executor variant.
;	- If this is first CASE item after BOS
;	  +  Insert an exBranch after BOS
;	  +  link exBranch operand into exBranch chain.
;	  +  binding previous CASE item (SELECT CASE) false branch.
;	     This is only necessary for the BOS case, The false
;	     branch is initially bound to the immediately following executor.
;Input:
;	Standard scan entrypoint
;Output:
;	Standard scan exit
;***************************************************************************
SsProc	CaseElse
	xor	bx,bx			;no expressions on stack
	mov	cx,bx			;this is a CASE ELSE varient
	jmp	short CaseCommon

SsProc	CaseTo
	mov	bx,2*(SIZE FEXP)	;we have 2 expressions on the stack
	mov	cl,STYP_CaseTo		;this is a CASE TO Varient
	jmp	short CaseCommon

SsProc	Case
	mov	bx,SIZE FEXP		;we have 1 expression on the stack for CASE
	mov	cl,STYP_CaseRel 	;normal CASE varient

CaseCommon:
	add	bx,sp			;point past expressions on stack to Select frame
	cmp	[bx].FCASE_Id,STYP_Case ;is this a select case frame?
	jnz	CaseScopeError		;brif not
	mov	ax,[bx].FCASE_oTyp	;get oTyp of SELECT expression
	DbAssertRel ax,be,ET_MAX,SCAN,<CaseCommon: Invalid oTyp>  
	jcxz	NoCoerce		;brif CASE ELSE, no coersion of operands

	cmp	cl,STYP_CaseRel 	;is this a standard CASE?
	je	Coerce1Op		;brif so, only one op to coerce
	call	EnsureArgType		;coerce the arg to the requested type

Coerce1Op:
	call	EnsureArgType		;coerce the arg

NoCoerce:
	push	cx			;preserve CASE type
	push	bx			;preserve frame ptr
	call	MapEmitExe		;map and emit Case executor varient
	pop	bx			;recover frame ptr
	pop	cx
	call	InsertCaseBranches	;insert exBranches/exCaseBranches
CaseX:
	jmp	[ScanRet]

CaseScopeError:
	mov	sp,bx			;eat the stack expressions
	mov	ax,MSG_Case		;Case without Select error
CaseErrorExit:				
	call	SsError
	mov	ax,ET_I2		;emit I2 varient...
	call	MapEmitExe		;...of executor...
	jmp	short CaseX		;...and return

subttl	END SELECT scan support.
page
;***
;Ss_EndSelect
;Purpose:
;	Scan entry END SELECT.
;
;	Scan tasks for END SELECT include:
;	- emitting the END SELECT executor.
;	- ensuring correct SELECT nesting.
;	- If this is item after BOS
;	  +  Insert an exBranch after BOS
;	  +  link exBranch operand into exBranch chain.
;	  +  binding previous CASE item (SELECT CASE) false branch.
;	     This is only necessary for the BOS case, The false
;	     branch is initially bound to the immediately following executor.
;	- Bind the exBranch chain.
;	- remove the SELECT CASE frame.
;Input:
;	Standard scan entrypoint
;Output:
;	Standard scan exit
;***************************************************************************
SsProc	EndSelect
	xor	cx,cx			; Rule table byte is 0
	mov	bx,sp			;set up frame ptr
	cmp	[bx].FCASE_Id,STYP_Case ;is this a select case frame?
	mov	ax,MSG_EndSelect	
	jnz	CaseErrorExit		; Brif not a select case
	mov	ax,[bx].FCASE_oTyp	; AX = oTyp of Select expression
	call	MapEmitExe		; Emit executor
	mov	bx,sp			; Restore frame pointer
	xor	cx,cx			;looks like CASE ELSE varient
	call	InsertCaseBranches	;insert exBranch before BOS for previous
					;true branch
	mov	bx,[bx].FCASE_oTxBranch ;start of exBranch chain
	call	BindExitCur		;bind list of address to current emit address
	add	sp,SIZE FCASE		;pop off Case frame
	jmp	[ScanRet]

subttl	SELECT CASE helpers
page
;***
;InsertCaseBranches - insert exBranch and exCaseBranch varients
;
;Purpose:
;	Inserts an exBranch after BOS if this is first SELECT CASE item
;	called since last BOS was processed, and binds the preceding false
;	branch after the inserted exBranch. Also inserts exCaseBranch
;	varients for CASE and CASE TO executors.
;
;	Checks to see if a Select Case without any intervening CASE*, or
;	END SELECT statements have been scanned.  If this is true, then
;	the pcode from the preceding SELECT statement is scanned to
;	ensure that only REMs and BOS/BOL opcodes are present.	If not,
;	then an Expected Case error is generated.
;Input:
;	bx = CASE frame ptr
;	cx = CASE varient (STYP_CaseTo, STYP_CaseRel, or 0 for CASE ELSE/END
;		SELECT.
;Output:
;	none.
;Preserves:
;	bx
;****************************************************************************
InsertCaseBranches:

	test	[SsFlags],SSF_StSelect ;was a select processed last?
	jz	CheckBosBranch	;brif not, continue

	and	[SsFlags],NOT SSF_StSelect ;reset SELECT processed flag
	mov	ax,[bx].FCASE_oTxFalse ;get oTx of SELECT CASE operand
	dec	ax
	dec	ax		;backup to Select opcode and start with next.
	PUSH_ES 		
	push	bx		
	push	cx		
	cCall	TxtChkValidOpsExec,<ax,SsoTxBos> ;check for valid ops between
				;end of SELECT CASE and start of Cur statement.
	pop	cx		
	pop	bx		
	POP_ES			
	or	dx,dx		
	jz	CheckBosBranch	;brif opcodes ok

	push	bx		;save frame ptr
	xchg	ax,bx		;set error address returned by TxtChkValidOps
	mov	ax,MSG_ExpectedCase ;issue Expected CASE error
	call	SsErrorBx	;remember error
	pop	bx		;recover source address

CheckBosBranch:
	test	[SsBosFlags],SSBOSF_StCase ;has a case item been processed since BOS?
	jnz	NoInsertBranch	;brif so, exBranch has already been inserted.

	push	cx		;save Case varient
	push	bx		;preserve frame ptr
	mov	cx,[bx].FCASE_oTxBranch ;ptr to start of exBranch chain
	call	InsertBranchBos ;insert exBranch after BOS; BX = oTx after BOS
	xchg	ax,bx		;...exbranch chain for block if
	pop	bx		;recover frame ptr
	pop	cx		;recover case varient
	jc	NoBranchInserted
	dec	ax
	dec	ax		;point to operand for exBranch
	mov	[bx].FCASE_oTxBranch,ax ;update new Branch chain start
	inc	ax
	inc	ax		;point to op after exBranch operand
NoBranchInserted:
	push	bx		;save frame ptr
	mov	bx,[bx].FCASE_oTxFalse ;get ptr to preceding false operand
	inc	bx
	jz	NoFalseBranchBind ;brif no operand to fix up
	dec	bx
	mov	PTRTX[BX],ax	;bind preceding false branch to this CASE op.

NoFalseBranchBind:
	pop	bx		;recover frame ptr

NoInsertBranch:
	mov	[bx].FCASE_oTxFalse,UNDEFINED ;previous false branch has been bound
	jcxz	NoInsertCaseBranch	;don't insert CaseBranch for CASE ELSE

	mov	cx,6			;we need to insert 6 bytes
	mov	bx,[bx].FCASE_oTyp	;get type variant
	dec	bx			;make type zero relative
	shl	bx,1			;and a word index
	mov	ax,WORD PTR cs:mCaseBranch[bx]	;get case branch executor variant
	mov	bx,di			;insert at current position
	call	InsertCx		;insert the executor, bx = oTx after insertion
	jc	NoInsertCaseBranch	;brif if out of memory

	sub	bx,4			;backup to caseBranch oTxFalse operand
	mov	PTRTX[bx],di		;initially bind false branch to next executor
	xchg	ax,bx			;ax = oTx of false operand
	mov	bx,sp			;recover frame ptr
	inc	bx
	inc	bx			;adjust for return address
	mov	[bx].FCASE_oTxFalse,ax	;save ptr to False branch operand in case we
					;need to patch it later
	inc	ax
	inc	ax			;advance to True branch operand
	mov	cx,[bx].FCASE_oTxTrue	;link True branch into true branch chain
	xchg	ax,bx
	mov	PTRTX[bx],cx
	xchg	ax,bx
	mov	[bx].FCASE_oTxTrue,ax
	or	[SsBosFlags],SSBOSF_StCase ;we have binding to do at next BOS
	mov	[SsOTxStart],di 	; Mark clear stack location

NoInsertCaseBranch:
	ret

	page
;***
;MapEmitExe - type explode and emit executor, then return
;
;Purpose:
;
;   Type explode and emit the executor.
;
;Input:
;
;   ax = expression oTyp
;   dx = executor map address
;
;Output:
;
;   none.
;
;****************************************************************************

MapEmitExe:
	dec	ax			;To zero relative
	add	ax,ax			;Convert to word offset
	add	ax,dx			;Offset into executor map
	xchg	ax,bx
	mov	ax,word ptr cs:[bx]	;Executor address
	STOSWTX 			;Emit executor
	ret

	subttl	CASE opcode to executor map tables

	public	mStSelect		;SELECT CASE executors
mStSelect:
	DWEXT	exStSelectCase2 		
	DWEXT	exStSelectCase4 		
	DWEXT	exStSelectCaseR8
	DWEXT	exStSelectCaseR8 		
	DWEXT	exStSelectCase2

	public	mCaseBranch		;Case branch inserted by scanner
mCaseBranch:
	DWEXT	exCaseBranch2			
	DWEXT	exCaseBranch4			
	DWEXT	exCaseBranchR8
	DWEXT	exCaseBranchR8			
	DWEXT	exCaseBranchSD

public mStCaseElse		;CASE ELSE executors
mStCaseElse:
	DWEXT	exStCaseElse2
	DWEXT	exStCaseElse4			
	DWEXT	exStCaseElseR8
	DWEXT	exStCaseElseR8			
	DWEXT	exStCaseElseSD

public mStCaseTo		;CASE IS <const> TO <const> executors
mStCaseTo:
	DWEXT	exStCaseToI2
	DWEXT	exStCaseToI4
	DWEXT	exStCaseToR8
	DWEXT	exStCaseToR8
	DWEXT	exStCaseToSD

public mStCase			;CASE <const> executors
mStCase:
	DWEXT	exStCaseI2
	DWEXT	exStCaseI4
	DWEXT	exStCaseR8
	DWEXT	exStCaseR8
	DWEXT	exStCaseSD

public mStCaseEq		;CASE IS = <const> executors
mStCaseEq:
	DWEXT	exStCaseEqI2
	DWEXT	exStCaseEqI4
	DWEXT	exStCaseEqR8
	DWEXT	exStCaseEqR8
	DWEXT	exStCaseEqSD

public mStCaseNe		;CASE IS <> <const> executors
mStCaseNe:
	DWEXT	exStCaseNeI2
	DWEXT	exStCaseNeI4
	DWEXT	exStCaseNeR8
	DWEXT	exStCaseNeR8
	DWEXT	exStCaseNeSD

public mStCaseLt		;CASE IS < <const> executors
mStCaseLt:
	DWEXT	exStCaseLtI2
	DWEXT	exStCaseLtI4
	DWEXT	exStCaseLtR8
	DWEXT	exStCaseLtR8
	DWEXT	exStCaseLtSD

public mStCaseLe		;CASE IS <= <const> executors
mStCaseLe:
	DWEXT	exStCaseLeI2
	DWEXT	exStCaseLeI4
	DWEXT	exStCaseLeR8
	DWEXT	exStCaseLeR8
	DWEXT	exStCaseLeSD

public mStCaseGt		;CASE IS > <const> executors
mStCaseGt:
	DWEXT	exStCaseGtI2
	DWEXT	exStCaseGtI4
	DWEXT	exStCaseGtR8
	DWEXT	exStCaseGtR8
	DWEXT	exStCaseGtSD

public mStCaseGe		;CASE IS >= <const> executors
mStCaseGe:
	DWEXT	exStCaseGeI2
	DWEXT	exStCaseGeI4
	DWEXT	exStCaseGeR8
	DWEXT	exStCaseGeR8
	DWEXT	exStCaseGeSD

public mStEndSelect		;End Select executors
mStEndSelect:
	DWEXT	exStEndSelect2			
	DWEXT	exStEndSelect4			
	DWEXT	exStEndSelectR8
	DWEXT	exStEndSelectR8			
	DWEXT	exStEndSelectSD 		

sEnd	SCAN
end
