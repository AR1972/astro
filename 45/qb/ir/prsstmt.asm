	TITLE	prsstmt.asm - Parser Statement Parsing

;==========================================================================
;
;  Module:  prsstmt.asm - Parser Statement Parsing
;  Subsystem:  Parser
;  System:  Quick BASIC Interpreter
;  Copyright <C> 1985, Microsoft Corporation
;
;  Note:
;	See general comments at top of prsnt.asm
;
;=========================================================================

	include version.inc
	PRSSTMT_ASM = ON
	includeOnce architec
	includeOnce context
	includeOnce msgshort
	includeOnce opmin
	includeOnce opstmt
	includeOnce parser
	includeOnce prsirw
	includeOnce prstab
	includeOnce psint
	includeOnce util
	includeOnce variable			;needed for mkVar

sBegin	DATA





sEnd	DATA

sBegin	CP
assumes	CS,CP
assumes	DS,DATA
assumes	SS,DATA
assumes ES,NOTHING

;**********************************************************************
; PARSE_RESULT NEAR NtStatement()
;
; Purpose:
;	Parse the start of a sequence of statements.
;
; Entry:
;	pTokScan = 1st token of statement list.
;
; Exit:
;	If no statement is recognized, no tokens are consumed and the
;	   return value is PR_NotFound.
;	If the statement head is recognized, the token is consumend,
;	   code is emitted and the entire statement is parsed.
;	   If it was good syntax, return value is PR_GoodSyntax.
;	   If it was bad syntax, return value is PR_BadSyntax.
;	Condition codes set based on value in al
;
;******************************************************************
cProc	NtStatement <PUBLIC,NODATA,NEAR>,<si,di>
	localW	pState
	localW	pCgFunc			
	localW	cgArg
cBegin	NtStatement
	sub	ax,ax			;ax = 0
	mov	[mkVar.MKVAR_flags],ax
	mov	[cIdArgs],ax		;cIdArgs = 0 (used by NtId())
	mov	[pCgFunc],ax		;assume no post-stmt code generator 
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_id
	jne	NotImplied		;branch if token wasn't an id
	sub	ax,ax			;allow LET or CALL
	push	ax
	call	NtImpliedLetOrCall	;parse implied LET or CALL 
	jg	J_CheckForEos		;brif PR_GoodSyntax, parse end-of-stmt
	jmp	NtStmtExit		;branch if PR_NotFound or PR_BadSyntax

J_CheckForEos:
	jmp	CheckForEos

;bx points to 1st token in statement
NotImplied:
	mov	[pCurStkMark],MAX_STK_MARK
					;reset stack used by MARK bnf directive
	cmp	[bx.TOK_class],CL_resWord
	jne	NtStmtNotFound
	mov	di,[bx.TOK_rw_rwf]	;di = reserved word flags
	mov	cx,[bx.TOK_rw_iRw]	;cx = reserved word's id
	test	di,RWF_NSTMTS
	jne	GotStmt

	;Reserved word table says this res word can't begin a statement.
	;Check for special cases. 
	
	mov	ax,opStData
	cmp	cx,IRW_DATA
	je	EmitTheRem		;branch if stmt is DATA
	mov	ax,opStRem
	cmp	cx,IRW_REM		;branch if stmt is REM
	je	EmitTheRem
	cmp	cx,IRW_SQuote
	jne	NtStmtNotFound		;branch if stmt isn't '<rem>
GotSQuote:
	mov	ax,opQuoteRem
EmitTheRem:
	push	ax			;pass opcode to be emitted
	call	NtEmitRem		;emit REM/DATA, al = PR_GoodSyntax
	jle	J1_NtStmtExit		;brif bad syntax (return AL as result)
	jmp	CheckForEos		;brif good syntax

NtStmtNotFound:
	sub	al,al			;ax = PR_NotFound
J1_NtStmtExit:
	jmp	NtStmtExit

;got a statement reserved word 
; bx points to 1st token in statement (current token)
; di = reserved word flags
; cx = reserved word's id
;
GotStmt:
	;Fetch info for a particular intrinsic function out of the
	;parser's reserved word table 'tRw'.
	
	.errnz	IRW_DEFINT - IRW_DEFDBL - 1
	.errnz	IRW_DEFLNG - IRW_DEFDBL - 2
	.errnz	IRW_DEFSNG - IRW_DEFDBL - 3
	.errnz	IRW_DEFSTR - IRW_DEFDBL - 4
	xchg	ax,cx			;ax = iRw
	cmp	ax,IRW_DEFDBL
	jb	NotDefType
	cmp	ax,IRW_DEFSTR
	ja	NotDefType
	or	[psFlags],PSIF_NoCaseChg
	;no identifiers in rest of this statement will affect nammgr's
	;upper/lower case of identifiers.  Otherwise, DEFINT A-Z would
	;change statements like a=z to A=Z
NotDefType:
	test	[psFlags],PSIF_fNot1stStmt
	je	FirstStmt		;brif this is 1st stmt on line
	cmp	ax,IRW_ELSE
	je	NtStmtNotFound		;brif got ELSE
	cmp	ax,IRW_ELSEIF
	je	NtStmtNotFound		;brif not ELSEIF
FirstStmt:
	mov	si,[bx.TOK_rw_pArgs]	;si -> pRwArgs in tRw
	test	di,RWF_FUNC
	je	NoFunc			;brif this res word is not an Intrinsic
	lodsw				;skip intrinsic's oState (lodsw is a
					; small-code way to bump si by 2.
					; Speed doesn't matter in this case)
	test	di,RWF_FUNC_CG
	je	NoFunc			;branch if no code generator for func
	lodsw				;skip intrinsic's code gen info
	lodsw
NoFunc:
	;There are 2 or more different statements which can begin
	; with this reserved word.  We now call a function which
	; looks ahead in the pcode to resolve the ambiguity.
	
	mov	ax,di			;ax = reserved word flags
	and	ax,RWF_NSTMTS		;ax = num stmts beginning with res word
	dec	ax
	je	NoAmbiguity		;branch if only 1
	lods	WORD PTR cs:[si]	;ax=adr of look-ahead function
	call	ax			;invoke it to resolve ambiguity
					;ax = 0,6,12,... for correct stmt
	add	si,ax			;advance to proper stmt's info
NoAmbiguity:
	lods	WORD PTR cs:[si]	;ax=state table offset for stmt's syntax
	add	ax,OFFSET CP:tState	;pState = &(tState[oState])
	mov	[pState],ax
	test	di,RWF_STMT_CG
	je	NoStmtCg		;branch if no code generator for stmt
	lods	WORD PTR cs:[si]	;ax=adr of code generation func
	mov	[pCgFunc],ax
	lods	WORD PTR cs:[si]	;ax=arg to pass to code generation func
	mov	[cgArg],ax
NoStmtCg:
	cmp	[grs.GRS_fDirect],0
	je	NotProtDirect		;brif we're not parsing a direct mode
					; statement 
	test	di,RWF_NO_DIRECT
	jne	ErrorId			;branch if stmt illegal in direct mode
NoScanner:

;Beyond this point, we can only return PR_GoodSyntax or PR_BadSyntax 
;because we've consumed something.
;
NotProtDirect:
	call	ScanTok			;skip keyword token 
	mov	ax,[pState]
	mov	[pStateLastScan],ax
	call	NtParse			;parse the statement @ ax
	jle	StmtNotGood		;branch if result wasn't PR_GoodSyntax
	cmp	[ps.PS_errCode],0
	jne	CheckForEos		;brif got some error like out-of-memory
	mov	dx,[pCgFunc]
	or	dx,dx
	je	CheckForEos		;brif no code generator to be called
	mov	ax,[cgArg]		;pass cgArg (opcode) in ax
	call	dx			;invoke code generation routine 
CheckForEos:
	or	[psFlags],PSIF_fNot1stStmt ;no longer 1st stmt on line
	jmp	SHORT NtStmtGoodSyntax

StmtNotGood:
	jl	NtStmtExit		;branch if Parse() returned PR_BadSyntax
	call	PErrState		; or <b> or ..." 
					;al = PR_BadSyntax
	SKIP2_PSW			;skip  mov al,PR_GoodSyntax  instr
NtStmtGoodSyntax:
	mov	al,PR_GoodSyntax
NtStmtExit:
	and	[psFlags],NOT PSIF_NoCaseChg
	or	al,al			;set condition codes for caller
cEnd	NtStatement

ErrorId:
	mov	ax,ER_ID		;Error: Stmt is illegal in Direct Mode 
	call	PErrMsg_AX		; al = PR_BadSyntax
	jmp	SHORT NtStmtExit

;**********************************************************************
; PARSE_RESULT NEAR NtStatementList()
;
; Purpose:
;	Parse 0 or more statements of the form: Statement {: [Statement]}
;
; Entry:
;	pTokScan = 1st token of statement list.
;
; Exit:
;	If no statement is recognized, no tokens are consumed and the
;	   return value is PR_NotFound.
;	If the statement head is recognized, the token is consumend,
;	   code is emitted and the entire statement is parsed.
;	   If it was good syntax, return value is PR_GoodSyntax.
;	   If it was bad syntax, return value is PR_BadSyntax.
;	Condition codes set based on al (return value)
;
;******************************************************************
; NOTE: Control flow may appear strange, but is optimized for "Typical case"
;
;NtStatementList0 is called by ParseLine() for the source line.
;NtStatementList is called for blocks within a 1 line THEN or ELSE clause
;
PUBLIC	NtStatementList, NtStatementList0
NtStatementList PROC NEAR
	or	[psFlags],PSIF_fNot1stStmt ;no longer 1st stmt on line
NtStatementList ENDP
;fall into NtStatementList0
NtStatementList0 PROC NEAR
	push	si			;preserve caller's si
	sub	si,si			;prepare to return PR_NotFound
StmtLoop:
	call	NtStatement		;parse 1 statement
	jle	NtStmtNotGood		;branch if result != PR_GoodSyntax
	inc	si			;remember we've consumed something
					; assumes no more than 32767 statements
					; per line (very safe)
TryColon:
	mov	ax,IRW_Colon
	call	TestScan_AX		;bx points to current token
	jne	NotColon		;branch if didn't get a ':'
	push	[bx.TOK_oSrc]		;save column of :
	inc	si			;remember we've consumed something
	call	ScanTok			;skip ':', bx points to next token
	pop	ax			;ax = column of :
	sub	ax,[bx.TOK_oSrc]	;ax = -#spaces between : and next token
	cmp	al,-2
	jl	BosSp			;brif more than 1 space
	mov	ax,opBos		;emit statement separator 
	call	Emit16_AX
	jmp	SHORT StmtLoop		;test return value

BosSp:
	push	[bx.TOK_oSrc]		;pass column of token after : to Emit16
	mov	ax,opBosSp		;emit statement separator 
	call	Emit16_AX
	call	Emit16			;emit column
	jmp	SHORT StmtLoop		;test return value

NotColon:
	mov	ax,IRW_SQuote
	call	TestScan_AX
	je	StmtLoop		;branch if got a 'comment
	mov	al,PR_GoodSyntax	;if we've consumed anything, return
	or	si,si			; PR_GoodSyntax
	jne	NtStmtListExit
	sub	al,al			;return PR_NotFound
	jmp	SHORT NtStmtListExit	;return PR_BadSyntax (cond codes set)

;al = PR_NotFound or PR_BadSyntax after calling NtStatement
NtStmtNotGood:
	je	TryColon		;brif NtStatement returned PR_NotFound
NtStmtListExit:
	pop	si			;restore caller's si
	ret
NtStatementList0 ENDP

;**********************************************************************
; PARSE_RESULT NEAR NtEndStatement()
;
; Purpose:
;	Parse End-of-Statement or End-of-Line, but don't consume any
;	tokens or emit any opcodes.
;
; Exit:
;	Returns PR_GoodSyntax if pTokScan is either End-Statement or End-Line.
;	Otherwise returns PR_NotFound.
;	condition codes are set via or al,al.
;
;******************************************************************
PUBLIC NtEndStatement
NtEndStatement PROC NEAR
	call	NtEndLine
	jg	NtEndExit		;brif got end-of-line
	cmp	cx,IRW_Colon		;NtEndLine set cx=token's IRW_xxx
	je	NtEndGood		;branch if token is ':'
	cmp	cx,IRW_ELSE
	jne	NtEndExit		;branch if token isn't 'ELSE' (returning
					; PR_NotFound as set by NtEndLine)
NtEndGood:
	mov	al,PR_GoodSyntax	;return PR_GoodSyntax for ':'
NtEndExit:
	or	al,al			;set condition codes for caller
	ret
NtEndStatement ENDP

;**********************************************************************
; PARSE_RESULT NEAR NtEndLine()
;
; Purpose:
;	Parse End-of-Line, but don't consume any tokens or emit any opcodes.
;
; Exit:
;	Returns PR_GoodSyntax if pTokScan is End-of-Line.
;	Otherwise returns PR_NotFound.
;	If token is res word, its IRW_xxx is returned in cx
;	condition codes are set via or al,al.
;
;******************************************************************
PUBLIC	NtEndLine
NtEndLine PROC NEAR
	mov	bx,[pTokScan]		;bx points to current token
	sub	cx,cx			;cx = impossible IRW_xxx
	cmp	[bx.TOK_class],CL_resWord
	jne	NotEol			;branch if not a reserved word token
	mov	al,PR_GoodSyntax	;prepare to return Good Syntax
	mov	cx,[bx.TOK_rw_iRw]	;cx = reserved word's id
	cmp	cx,IRW_NewLine
	je	GotEol			;branch if token is newline
	cmp	cx,IRW_SQuote
	je	GotEol			;branch if token is single quote rem
NotEol:
	sub	al,al			;return PR_NotFound
GotEol:
	or	al,al			;set condition codes for caller
	ret
NtEndLine ENDP


CP	ENDS

end
