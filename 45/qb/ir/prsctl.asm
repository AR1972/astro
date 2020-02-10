	TITLE	prsctl.asm - Parser Control-Flow NonTerminal Functions

;==========================================================================
;
;  Module:  prsctl.asm - Parser Control-Flow NonTerminal Functions
;  Subsystem:  Parser
;  System:  Quick BASIC Interpreter
;  Copyright <C> 1985, Microsoft Corporation
;
;  NOTE:
;	See prsnt.asm for general comments
;
;=========================================================================

	include version.inc
	PRSCTL_ASM = ON
	includeOnce architec
	includeOnce context
	includeOnce names
	includeOnce opcontrl
	includeOnce parser
	includeOnce prsirw
	includeOnce prstab
	includeOnce psint
	includeOnce qbimsgs

	assumes	DS,DGROUP
	assumes	ES,DGROUP
	assumes	SS,DGROUP

sBegin	DATA

fLastWasLabel DB 0	;flag which gets set by IfClause 
			;non-zero if last call emitted a label,
			;zero if last call emitted a statement list
sEnd	DATA

sBegin	CP
assumes	CS,CP

;===========================================================================
;                  C O N T R O L    F L O W
;    P A R S E R    R E C O G N I Z I N G    F U N C T I O N S
;
;=======================================================================

;**********************************************************************
; ushort NEAR TestLn()
;
; Purpose:
;	See if the current token could be considered a line number
;	or numeric label.  A line number is any integer from 0 to 65529.
;	A numeric label is any number other than an integer from 0 to 65529
;	that does not use E or D notation.
; Entry:
;	pTokScan points to current token
; Exit:
;	If an error (like OVERFLOW or Out-Of-Memory) occurs
;	   error is logged, al = PR_BadSyntax, carry is set
;	Else
;	   carry is clear
;	   if lineNumber was found (i.e. 0 to 65529)
;	      ax its oNam, cx=0
;	   else if numeric label was found (i.e. any num other than 0 to 65529)
;	      ax its oNam, cx=non-zero
;	   else, return 0 (with condition codes set).
;	The token is not consumed in any case.
;
;******************************************************************
cProc	TestLn <PUBLIC,NODATA,NEAR>
cBegin	TestLn
	mov	bx,[pTokScan]		;di points to current token
	cmp	[bx.TOK_class],CL_lit
	jne	NotLn			;branch if token is not a literal
	mov	ax,[bx.TOK_lit_value_I2];ax = potential line number
	mov	cl,[bx.TOK_lit_litType]	;cl = literal type
	cmp	cl,LIT_I2
	jne	NotI2			;branch if we got an integer
	or	ax,ax
	js	NotLn			;branch if literal was -1 .. -32768
;ax = integer line number (0..65529)
TlExit2:
	call	ONamOfLn		;ax = offset in name table for linenum
	je	TlOmErr			;brif out-of-memory
	sub	cx,cx			;return lineNumber indication
;ax = oNam for line number or numeric label
TlExit1:
	or	ax,ax			;set condition codes for caller
;ax = error code if carry set, oNam if carry clear
TlExit:
cEnd	TestLn

NotLn:
	sub	ax,ax
	jmp	SHORT TlExit1

;bx = pointer to token descriptor
;ax = integer constant value, cl = literal type
NotI2:
	cmp	cl,LIT_I4
	jne	NotI4			;branch if not a long integer
	cmp	[bx.TOK_lit_value_I2+2],0
	jnz	GotNum			;branch if high word was not 0
					; it is still a numeric label
	cmp	ax,MAX_LN
	jbe	TlExit2			;branch if 0..65529
	jmp	SHORT GotNum		;I4's are valid numeric labels

;bx = pointer to token descriptor
;cl = literal type.  See if its an R4 or R8 numeric label
NotI4:
	cmp	cl,LIT_R4
	je	GotNum
	cmp	cl,LIT_R8
	jne	NotLn			;brif it is not I2,I4,R4 or R8
;bx = pointer to token descriptor, scan numeric label
GotNum:
	sub	ax,ax
	or	al,[bx.TOK_lit_errCode]	;ax = lexical analyzer's error code
	jne	TlErr			;brif lexical analyzer found an error
					; in literal's format
	test	[bx.TOK_lit_flags],FLIT_exp
	jne	NotLn			;brif E+nnn or D+nnn exponent seen
	mov	ax,[ps.PS_bdpSrc.BDP_pb] ;ax points to start of parser buffer
	add	ax,[bx.TOK_oSrc]	;ax points to 1st byte of id
					;  (parm to ONamOfPbCb)
	mov	cx,[ps.PS_bdpSrc.BDP_pbCur] ;cx = pointer beyond end of number
	sub	cx,ax			;cx = byte count of number (parm)
	cmp	cx,CB_IDNAM_MAX		
	ja	NotLn			;brif more than 40 chars long
	call	ONamOfPbCb		;nammgr returns ax = oNam, dl = flags
	mov	cl,1
	jne	TlExit1			;brif not out-of-memory
TlOmErr:
	call	ParseErrOm		;Error "Out of memory"
	jmp	SHORT TlErr1
TlErr:
	call	PErrMsg_AX		; al = PR_BadSyntax
TlErr1:
	mov	al,PR_BadSyntax
	stc				;return error code
	jmp	SHORT TlExit

;**********************************************************************
; PARSE_RESULT NEAR NtLn()
;
; Purpose:
;	Parse a Line Number.
; Exit:
;	If line number is found,
;	   consume the token, emit the 16 bit name table offset
;	   for the label/line number, and return al=PR_GoodSyntax.
;	Otherwise,
;	   return al=PR_NotFound.
;	Condition codes set based on value in al
;
;******************************************************************
cProc	NtLn <PUBLIC,NODATA,NEAR>
cBegin	NtLn
	call	TestLn			;ax = value of pTokScan's line number
	jc	NtLnExit		;brif error (Overflow, out-of-memory)
	je	NoLineNum		;branch if token isn't a line number
	call	Emit16_AX		;emit oNam
	call	ScanTok			;consume the line number
	or	[ps.PS_flags],PSF_fRef+PSF_fLabelRef ;so text mgr knows
					; to scan program if in direct mode 
	mov	al,PR_GoodSyntax
	SKIP2_PSW
NoLineNum:
	sub	ax,ax			;return PR_NotFound
NtLnExit:
	or	al,al			;set condition codes for caller
cEnd	NtLn

;**********************************************************************
; PARSE_RESULT NEAR NtLabLn()
;
; Purpose:
;	Parse a Label or Line Number.
;
; Exit:
;	If it is found, consume the token, emit the 16 bit name table offset
;	   for the label/line number, and return PR_GoodSyntax.
;	Otherwise,
;	   return PR_NotFound.
;	Condition codes set based on value in al
;
;******************************************************************
cProc	NtLabLn <PUBLIC,NODATA,NEAR>
cBegin	NtLabLn
	call	NtLn			;see if current token is line num
	jne	GotLn			;branch if parsed a line number

	;we're not looking at a line number, maybe its a label 
	call	IdTokPeriodImp		;next token can have "." in it
					; but must have no explicit type char
	je	NtLabExit		;branch if PR_NotFound
	mov	ax,[bx.TOK_id_oNam]	;emit its oNam
	call	Emit16_AX
	call	ScanTok			;consume the label
	or	[ps.PS_flags],PSF_fRef+PSF_fLabelRef ;so text mgr knows
					; to scan program if in direct mode 
GotLn:
	mov	al,PR_GoodSyntax	;return PR_GoodSyntax
	SKIP2_PSW
NtLabExit:
	or	al,al			;set condition codes for caller
cEnd	NtLabLn

;**********************************************************************
; PARSE_RESULT NEAR NtIfStmt()
;
; Purpose:
;	Parse a block or single line IF statement
;
; Entry:
;	IF <exp> has just been parsed.
;	'pTokScan' points to the current token, which may be THEN/GOTO
;
; Exit:
;	If a Block THEN is recognized, the next token is scanned,
;	   and the return value is PR_GoodSyntax.
;	else
;	   the return value is PR_NotFound.
;
;   Runtime behavior of IF-THEN-ELSE opcodes:
;   ----------------------------------------
;      <exp> opStIf(oText) - branch to oText if exp is zero (false)
;      <exp> opStIfLab(label) - branch to label if exp is non-zero (true)
;      <exp> opStIfLabDirect(label) - branch to label if exp is non-zero (true)
;      opStElse(oText) - unconditionally branch to oText
;      opStElseLab(label) - unconditionally branch to label
;      opStElseLabDirect(label) - unconditionally branch to label
;      opStElseNop - nop
;      <exp> opStIfBlock(oText) - branch to oText if exp is zero (false)
;      <exp> opStElseIf(oText) - branch to oText if exp is zero (false)
;      opStEndIfBlock - nop
;
;   NOTE: When in direct mode, Parser emits opStIfLabDirect instead of
;         opStIfLab, and opStElseLabDirect instead of opStElseLab.
;
;   BASICA allows IF <exp> ,THEN ... ,ELSE ...
;   Since BASCOM does not allow the comma, neither does QBI
;   
;   Single line IF statement syntax to pcode mappings:
;   -------------------------------------------------
;
;      Syntax:  IF <exp> GOTO <label>
;   
;      Pcode:   <exp> opStIfLabDirect(label)  (if direct mode)
;               <exp> opStIfGotoLab(label)    (if not direct mode - for listing)
;
;      ============================================================
;      Syntax:  IF <exp> THEN <label>
;   
;      Pcode:   <exp> opStIfLab(label)
;   
;      ============================================================
;      Syntax:  IF <exp> THEN <stmt list>
;   
;                            +--------------+
;      Pcode:   <exp> opStIf(|) <stmt list> |
;   
;      ============================================================
;      Syntax:  IF <exp> THEN <label> ELSE <label>
;   
;      Pcode:   <exp> opStIfLab(label) opStElseNop opStElseLab(label)
;
;      NOTE:    Lister emits no 'ELSE' for opStElseLab, just the ASCII label
;   
;      ============================================================
;      Syntax:  IF <exp> THEN <label> ELSE <stmt list>
;   
;      Pcode:   <exp> opStIfLab(label) opStElseNop <stmt list>
;
;      NOTE:    <stmt list> can contain more single line IF stmts
;   
;      ============================================================
;      Syntax:  IF <exp> THEN <stmt list> ELSE <label>
;   
;                            +--------------------------+
;      Pcode:   <exp> opStIf(|) <stmt list> opStElse(|) | opStElseLab(label) |
;                                                    +-----------------------+
;
;      NOTE:    Lister emits no 'ELSE' for opStElseLab, just the ASCII label
;   
;      ============================================================
;      Syntax:  IF <exp> THEN <stmt list> ELSE <stmt list>
;   
;                            +--------------------------+
;      Pcode:   <exp> opStIf(|) <stmt list> opStElse(|) | <stmt list> |
;                                                    +----------------+
;   
;   Block IF statement syntax to pcode mappings:
;   -------------------------------------------
;      Syntax:  IF <exp> THEN
;   
;      Pcode:   <exp> opStIfBlock(oText to beyond ELSEIF/ELSE/END IF opcode)
;   
;      ============================================================
;      Syntax:  ELSEIF <exp> THEN
;   
;      Pcode:   <exp> opStElseIf(oText to beyond next ELSEIF/ELSE/END IF opcode)
;
;      NOTE:    Scanner inserts exBranch(oText to beyond END IF opcode)
;               after the opBol and before <exp>, so code falling into the
;               ELSEIF will branch beyond END IF without evaluating <exp>.
;   
;      ============================================================
;      Syntax:  ELSE 
;   
;      Pcode:   opStElse(oText to beyond END IF opcode)
;   
;      ============================================================
;      Syntax:  END IF
;   
;      Pcode:   opStEndIfBlock
;   
; RESTRICTIONS:
;	Block IF must be 1st stmt on line
;	Block ELSEIF must be 1st stmt on line
;	Block ELSE must be 1st stmt on line
;	Block END IF must be 1st stmt on line
;	Block IF must be last stmt on line (or else its a single line IF)
;	Block ELSEIF need not be last stmt on line
;	Block ELSE need not be last stmt on line
;	Block END IF need not be last stmt on line
;
;******************************************************************
cProc	NtIfStmt <PUBLIC,NODATA,NEAR>,<si,di>
cBegin	NtIfStmt
	mov	ax,IRW_GOTO
	call	TestScan_AX
	jne	NotIfGoto1		;branch if current token is not 'GOTO'
	call	ScanTok			;consume 'GOTO' token 
	mov	ax,UNDEFINED		;parse label or stmtlist 
	call	IfClause		;parse label, generate code
	jg	TryElseClause		;branch if result == PR_GoodSyntax
					; IfClause never returns PR_NotFound
	jmp	SHORT NtIfExit		;return error result

NotIfGoto1:
	mov	ax,IRW_THEN		;Consume THEN
	call	ConsumeRw_AX
	jc	NtIfExit		;brif not THEN - return PR_BadSyntax
	call	NtEndLine
	je	TryThenClause		;brif something follows THEN

	;Got a BLOCK IF statement 
	call	NtErrIfNot1st
	jl	NtIfExit		;brif not 1st stmt on line
	mov	ax,opStIfBlock
	call	Emit16_AX
	call	Emit16_0		;leave room for the operand
NtIfGoodSyntax:
	mov	al,PR_GoodSyntax
	jmp	SHORT NtIfExit

;Consume the label/linenum/statement-list following the THEN token
TryThenClause:
	mov	ax,opStIf
	call	IfClause		;parse label or stmtlist 
	jle	NtIfExit		;branch if result = PR_BadSyntax
					; IfClause never returns PR_NotFound

;we've just parsed THEN clause of a single line if
; Check for an ELSE clause 
;
TryElseClause:
	mov	ax,IRW_Colon		;Allow stmts IF e THEN 10 : ELSE ...
	call	TestScan_AX		; since both BASICA and BASCOM do
	jne	NotColonElse		;branch if no ':'
	call	Peek1Tok		;peek (don't consume) token after ':'
	mov	ax,IRW_ELSE
	call	TestPeek_AX
	jne	NtIfGoodSyntax		;branch if not ': ELSE'
	call	ScanTok			;Skip colon
NotColonElse:
	mov	ax,IRW_ELSE
	call	TestScan_AX
	jne	NtIfGoodSyntax		;branch if current token isn't ELSE
	call	ScanTok			;consume 'ELSE' token
	mov	ax,opStElse
	call	IfClause		;parse label | stmtlist 
					;al = result
NtIfExit:
cEnd	NtIfStmt

;-----------------------------------------------------------------
; NOTE: IfClause() assumes the following contiguous relative order
;	of opcodes:
;	opStIf, opStIfLab, opStIfLabDirect
;	opStElse, opStElseLab, opStElseLabDirect
;
;-----------------------------------------------------------------
if opStIfLab - opStIf - 1
	Error: code assumes opStIfLab == opStIf+1
endif
if opStIfLabDirect - opStIfLab - 1
	Error: code assumes opStIfLabDirect == opStIfLab+1
endif
if opStElseLab - opStElse - 1
	Error: code assumes opStElseLab == opStElse+1
endif
if opStElseLabDirect - opStElseLab - 1
	Error: code assumes opStElseLabDirect == opStElseLab+1
endif

;**********************************************************************
; PARSE_RESULT NEAR IfClause(ax=opcode)
;
; Purpose:
;	Parse the clause [<label> | <line number> | <statementlist>]
;	which can follow THEN or ELSE in a single line IF statement.
;
; Entry:
;	ax = opStIf if we are parsing a THEN clause,
;            opStElse we are parsing an ELSE clause,
;	     UNDEFINED if we are parsing an IF <exp> GOTO clause.
;	fLastWasLabel is NonZero if the last time IfClause() was called,
;	it parsed a statement-list and not a label.  This is used
;	to decide what type of opcodes to generate for ELSE ...
;	<label> ELSE <label> ==> opStElseNop opStElseLab(<label>)
;	<label> ELSE <stmtlist> ==> opStElseNop <stmtlist> 
;	<stmtlist> ELSE <label> ==> opStElse(oText) opStElseLab(<label>)
;	<stmtlist> ELSE <stmtlist> ==> opStElse(oText) <stmtlist> 
;
; Exit:
;	Returns PR_GoodSyntax or PR_BadSyntax with condition codes
;	set accordingly (NEVER returns PR_NotFound)
;
;******************************************************************
IfClause PROC NEAR
	push	si
	push	di
	xchg	di,ax			;di isn't changed for rest of IfClause
					; it is the opcode to emit
	mov	si,dataOFFSET fLastWasLabel
					;si isn't changed for rest of IfClause
	call	TestLn			;see if pTokScan points to a line num
	jc	J1_IfClExit		;brif error (Overflow, out-of-memory)
	je	NotLabel1		;branch if it wasn't a line number
					; or a numeric label
	jcxz	GotLabel		;brif linenum, not numeric label
	mov	ax,MSG_ExpLn		;Error: expected line number
IfPErrMsg:				
	call	PErrExpMsg_AX		;al = PR_BadSyntax
	jmp	SHORT J1_IfClExit

NotLabel1:
	cmp	di,UNDEFINED		;test opcode for UNDEFINED
	je	TestForAlphaLab		;brif IF <exp> GOTO
	cmp	di,opStElse		;check 'opcode'
	jne	EmitTheOpcode
	cmp	BYTE PTR [si],0		;see if fLastWasLabel == FALSE
	je	EmitTheOpcode		;branch if last clause wasn't a label
	mov	ax,opStElseNop		;EmitOpcode(opStElseNop)
	jmp	SHORT DoEmit16

EmitTheOpcode:
	mov	ax,di			;ax = opStIf or opStElse
	call	Emit16_AX		;emit opStIf or opStElse 
	sub	ax,ax			;leave room for oText operand 
DoEmit16:
	call	Emit16_AX

	call	NtStatementList		;parse statement list, may recurse
					; back to this function 
	mov	BYTE PTR [si],0		;fLastWasLabel = FALSE
	jne	IfClExit		;return PR_GoodSyntax/PR_BadSyntax
	;optional stmt-list omitted. i.e. IF 1 THEN ELSE  is a valid stmt
IfClGood:
	mov	al,PR_GoodSyntax	;stmt list is optional
J1_IfClExit:
	jmp	SHORT IfClExit

TestForAlphaLab:			
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_ID	;test to see if it's an identifier
	je	GotLabel		;it's a valid label
	mov	ax,MSG_ExpLabLn		;ax = "Label or Line Number"
	jmp	SHORT IfPErrMsg		;generate "Expected ..." err msg

;At this point, we know we're looking at a label/linenum and not stmt list
;di = opcode to emit (or UNDEFINED if IF <exp> GOTO ...)
GotLabel:
	cmp	di,opStElse		;check 'opcode'
	jne	NotElse
	cmp	BYTE PTR [si],0		;see if fLastWasLabel == FALSE
	je	LastWasntLabel		;branch if last time we were called, we
					; emitted a statement list and not
					; a label, in which case, there is
					; no need to emit an opStElseNop
	mov	ax,opStElseNop
	jmp	SHORT EmitTheElse

LastWasntLabel:
	mov	ax,opStElse
	call	Emit16_AX
	sub	ax,ax			;leave room for oText operand 
EmitTheElse:
	call	Emit16_AX
NotElse:
	mov	BYTE PTR [si],1		;fLastWasLabel = NonZero (true)
	xchg	ax,di			;ax = the opcode
					;NOTE: di is no longer opcode
	inc	ax			;map opStIf to opStIfLab,
					;    opStElse to opStElseLab,
					;    UNDEFINED (goto) to 0
	jne	NotGotoLabel		;branch if not GOTO <label>
	mov	ax,opStIfGotoLab
	cmp	[grs.GRS_fDirect],FALSE
	je	NotGotoLabel

	;In direct mode, convert IF <exp> GOTO => IF <exp> THEN label,
	;so we don't need special opStIfGotoLabDirect opcode.  The
	;only reason we don't map opStIfGotoLab to opStIfLab is for listing
	
	mov	ax,opStIfLab
NotGotoLabel:
	call	Emit16_AX
	call	NtLabLn			;emit 16 bit oNam operand for label 
					;NtLabLn is guarenteed to return
					; PR_GoodSyntax since we've already
					; peeked at the following token
IfClExit:				;al = result
	pop	di
	pop	si
	or	al,al			;setup condition codes for caller
	ret
IfClause ENDP

;-----------------------------------------------------------------------
; NOTE: NtCaseRelation() assumes the following contiguous relative order
;	of opcodes:
;	  opStCaseEq,opStCaseLt,opStCaseGt,opStCaseLe,opStCaseGe,opStCaseNe
;
if opStCaseLt - opStCaseEq - 1
	Error: code assumes opStCaseLt == opStCaseEq+1
endif
if opStCaseGt - opStCaseLt - 1
	Error: code assumes opStCaseGt == opStCaseLt+1
endif
if opStCaseLe - opStCaseGt - 1
	Error: code assumes opStCaseLe == opStCaseGt+1
endif
if opStCaseGe - opStCaseLe - 1
	Error: code assumes opStCaseGe == opStCaseLe+1
endif
if opStCaseNe - opStCaseGe - 1
	Error: code assumes opStCaseNe == opStCaseGe+1
endif

;**********************************************************************
; PARSE_RESULT NEAR NtCaseRelation()
;
; Purpose:
;	Parse '<relational operator> <constant>'
;	and emit <constant> opStCaseXX(oTextT,oTextF)
;	where XX = {Lt,Le,Eq,Ne,Ge,Gt}
;
; Entry:
;	CASE IS has just been parsed.
;	'pTokScan' points to the current token (relational operator)
;
; Exit:
;	Returns either PR_GoodSyntax, PR_NotFound or PR_BadSyntax
;
;******************************************************************
cProc	NtCaseRelation <PUBLIC,NODATA,NEAR>
cBegin	NtCaseRelation
	;First, map {=, <, >, <=, >=, <>} to {1..6}  
	call	RelOp			;parse relational operator, ax = result
	or	ax,ax
	je	NtCaseExit		;branch if we didn't get one, with ax=0
					; (PR_NotFound)
	push	ax			;save relop
	call	ScanTok			;skip relational operator
	call	NtConsumeExp		;parse an expression
	pop	dx			;restore relop
	jle	NtCaseExit		;branch if no or bad expression
					; al = PR_BadSyntax
	xchg	ax,dx			;ax = relop
	add	ax,opStCaseEq - 1	;EmitOpcode(opStCaseEq - 1 + relop)
	call	Emit16_AX
	mov	al,PR_GoodSyntax
NtCaseExit:
cEnd	NtCaseRelation

;**********************************************************************
; PARSE_RESULT NEAR NtErrIfNot1st()
; Purpose:
;	Checks to see if current statement is 1st stmt on line.
; Entry:
;	PSIF_fNot1stStmt is set in psFlags if no statements have been
;	completed yet
; Exit:
;	if this is the 1st stmt on this line
;	   returns al=PR_GoodSyntax
;	else
;	   generates error (Must be 1st stmt on line)
;	   returns al=PR_BadSyntax
;	Condition codes set based on value in al
;
;**********************************************************************
PUBLIC	NtErrIfNot1st
NtErrIfNot1st PROC NEAR
	test	[psFlags],PSIF_fNot1stStmt
	mov	al,PR_GoodSyntax
	je	NtErrExit		;brif we're in 1st stmt on line
	mov	ax,MSG_1stStmt		;Error: "Must be 1st statement on line"
	call	PErrMsg_AX		; al = PR_BadSyntax
NtErrExit:
	or	al,al			;set condition codes for caller
	ret
NtErrIfNot1st ENDP


sEnd	CP

end
