	TITLE	prsexp.asm - Parser Expression Recognizer

;==========================================================================
; Module:  prsexp.asm - Parser Expression Recognizer
; Subsystem:  Parser
; System:  Quick BASIC Interpreter
;
;  NOTE:
;	See prsnt.asm for general comments
;
;===========================================================================

	include version.inc
	PRSEXP_ASM = ON 
	includeOnce opaftqb4
	includeOnce prstab
	includeOnce opintrsc
	includeOnce qbimsgs
	includeOnce parser
	includeOnce pcode
	includeOnce psint
	includeOnce variable

	assumes DS,DATA
	assumes SS,DATA
	assumes ES,NOTHING

sBegin	DATA

PREC_lPar EQU	1	;operator precedence for '('
PREC_rPar EQU	1	;operator precedence for ')'
PREC_mark EQU	0	;minimum operator precedence


FOP_unary EQU	1	;flag indicating resword can be a unary operator

;NOTE: order of this table assumes values of IOP_mark through IOP_LParen
; as defined in psint.inc
;
.errnz	IOP_mark	- 0
.errnz	IOP_RParen	- 1
.errnz	IOP_Imp		- 2
.errnz	IOP_Eqv		- 3
.errnz	IOP_Xor		- 4
.errnz	IOP_Or		- 5
.errnz	IOP_And		- 6
.errnz	IOP_Not		- 7
.errnz	IOP_EQ		- 8
.errnz	IOP_LT		- 9
.errnz	IOP_GT		- 10
.errnz	IOP_LE		- 11
.errnz	IOP_GE		- 12
.errnz	IOP_NE		- 13
.errnz	IOP_Add		- 14
.errnz	IOP_Minus	- 15
.errnz	IOP_Mod		- 16
.errnz	IOP_Idiv	- 17
.errnz	IOP_Mult	- 18
.errnz	IOP_Div		- 19
.errnz	IOP_Plus	- 20
.errnz	IOP_UMinus	- 21
.errnz	IOP_Pwr		- 22
.errnz	IOP_LParen	- 23

mpIopOpcode LABEL	WORD
	DW	0			;stack marker
	DW	0			;)			
	DW	opImp			;IMP		
	DW	opEqv			;EQV		
	DW	opXor			;XOR		
	DW	opOr			;OR			
	DW	opAnd			;AND		
	DW	opNot			;NOT		
	DW	opEQ			;=			
	DW	opLT			;<			
	DW	opGT			;>			
	DW	opLE			;<=			
	DW	opGE			;>=			
	DW	opNE			;<>			
	DW	opAdd			;binary+	
	DW	opSub			;binary-	
	DW	opMod			;MOD		
	DW	opIDv			; \			
	DW	opMul			;*			
	DW	opDiv			;/			
	DW	0			;unary+	 (never emitted)
	DW	opUMi			;unary-	
	DW	opPwr			;^			
	DW	opLParen		;(

mpIopPrecedence	LABEL	BYTE
	DB	2*PREC_mark		;stack marker
	DB	2*PREC_rPar		;)			
	DB	2*2			;IMP		
	DB	2*3			;EQV		
	DB	2*4			;XOR		
	DB	2*5			;OR			
	DB	2*6			;AND		
	DB	2*7 + FOP_unary		;NOT		
	DB	2*8			;=			
	DB	2*8			;<			
	DB	2*8			;>			
	DB	2*8			;<=			
	DB	2*8			;>=			
	DB	2*8			;<>			
	DB	2*9			;binary+	
	DB	2*9			;binary-	
	DB	2*10			;MOD		
	DB	2*11			; \			
	DB	2*12			;*			
	DB	2*12			;/			
	DB	2*13 + FOP_unary	;unary+	
	DB	2*13 + FOP_unary	;unary-	
	DB	2*14			;^			
	DB	2*PREC_lPar + FOP_unary;(

	PUBLIC	pExpTos, stkExpInit
;Expression stack constants (see prsexp.asm)
CB_EXP_STK	EQU 64			;number of bytes in expression stack
					; 4 bytes per entry, 16 entries
stkExp		DB CB_EXP_STK DUP (?)	;parse-time expression stack
stkExpMin	EQU stkExp+4		;minimum legal offset for pExpTos
stkExpInit	LABEL BYTE		;value of pExpTos when initialized
pExpTos		DW 0			;points to cur top of expression stack

sEnd	DATA


sBegin	CP
assumes CS,CP

;*********************************************************************/
; ushort NEAR RelOp()
;
; Purpose:
;	Called by NtExp() and NtCaseArg() to parse a relational operator.
;	If a 2-token relational operator is parsed, ScanTok() is called
;	once to consume 1st token.  Caller must always call once ScanTok()
;	to skip past token(s).  It is done this way so NtExp() can be faster
;	& smaller.
;
; Entry:
;	pTokScan points to potential relational operator token
;
; Exit:
;	returns:
;		0 if token is not a relational operator
;		1 for = 
;		2 for < 
;		3 for > 
;		4 for <= 
;		5 for >= 
;		6 for <> 
; 	Condition codes set based on value in ax
;
;*********************************************************************/
;Register usage:
;	di = iOperator
;	si = points to current token
;
cProc	RelOp	<PUBLIC,NEAR,NODATA>,<di>
cBegin	RelOp
	sub	di,di			;default return value to 0
	mov	bx,[pTokScan]		;bx points to current token
RelOpLoop:
	cmp	[bx.TOK_class],CL_resword
	jne	RelOpExit		;brif token isn't a reserved word
	mov	ax,[bx.TOK_rw_iOperator];ax = operator's index (IOP_xxx)
	inc	ax			;test for UNDEFINED
	je	RelOpExit		;brif token isn't an operator

	;Got an operator, see if its a relational operator
	;IOP_xxx is always way less than 255, we can deal with low byte of ax
	sub	al,9			;map =,<,> to 0,1,2
	cmp	al,2
	ja	RelOpExit		;brif token isn't a relational operator
	inc	ax			;map =,<,> to 1,2,3
	or	di,di
	jne	Got2ndChar		;brif we're dealing with 2nd char
					; or relational operator
	xchg	di,ax			;save partial return value in di

	;got a relational operator, see if it is a 2-token
	;relational operator like <>, <=, or >=
	
	call	Peek1Tok		;examine beyond current token
					; bx points to that token
	jmp	SHORT RelOpLoop		;examine 2nd char

;di = 1..3 for 1st char in {=,<,>}
;ax = 1..3 for 2nd char in {=,<,>}
Got2ndChar:
	cmp	ax,di
	je	RelOpExit		;brif same char as 1st (<<, >> or ==)
	inc	ax			;map 2nd char {=,<,>} to 2,3,4
	add	di,ax			;map <=, >=, <> to 4,5,6
	call	ScanTok			;skip 1st relational operator
RelOpExit:
	xchg	ax,di			;ax = return value
	or	ax,ax			;set condition codes for caller
cEnd	RelOp

;*********************************************************************
; STATICF(boolean) PopTillLParen()
;
; Purpose:
;	This is called when we have encountered a right paren while
;	parsing an expression.  It causes all operators which have been
;	stacked to be emitted, up to the matching stacked left paren.
;	If no matching left paren is found on the stack, it means we
;	parsed a sub-expression like x+y), so NtExp() should exit
;	and let its caller parse the right paren.  Maybe it marks the
;	end of a function, sub, or array arg list.
;
; Exit:
;	If 1 left paren was popped of the stack, returns psw.EQ,
;  else if no left parens were found on stack, returns psw.NE
;
;*********************************************************************/
cProc	PopTillLParen	<NEAR,NODATA>,<si,di>
cBegin	PopTillLParen
	mov	si,[pExpTos]		;si points to top of expression stack

PopLoop:				;while (PREC_lPar < *pExpTosReg) {
	cmp	WORD PTR [si],PREC_lPar
	jbe	PopDone
	inc	si			;pop stacked operator's precedence
	inc	si
	lodsw				;pop and emit stacked operator's opcode
	call	Emit16_AX
	jmp	SHORT PopLoop

PopDone:
	mov	[pExpTos],si		;save pointer to top-of-stack

	;if top-of-stack is left paren, return psw.EQ
	cmp	WORD PTR [si],PREC_lPar
cEnd	PopTillLParen

;*********************************************************************
; PARSE_RESULT NEAR NtExp()
;
; Purpose:
;	Parse an expression and emit code for it.
;	Guarenteed to give Expression To Complex error before
;	more than 16 (CB_EXP_STK/4) entries get pushed onto the expression
;	stack.  This controls unrestricted stack (SS) usage.
;
; Entry:
;	pTokScan points to 1st token of expression to be parsed
;	If the static variable [oNamConstPs] is non-zero, intrinsic
;	   functions are not allowed
;
; Exit:
;	pTokScan points to 1st token after expression
;	cIdArgs is bumped by 1 (no matter how much recursion takes place).
;	The return value is PR_GoodSyntax, PR_NotFound or PR_BadSyntax.
;	If the result is not PR_BadSyntax, mkVar.flags is preserved across
;	the call
;	Condition codes set based on value in al
;
;*******************************************************************
cProc	NtExp	<PUBLIC,NEAR,NODATA>,<si,di>
	localB	fConsumed
cBegin	NtExp
	push	[mkVar.MKVAR_flags]	;preserve this for caller

	;Push a low-precedence stopper onto the stack which prevents any
	;operators already on the stack from being emitted as a result of
	;this recursive invocation of NtExp.
	
	sub	[pExpTos],2		;make room for marker on exp stack
	mov	bx,[pExpTos]
	mov	WORD PTR [bx],PREC_mark	;push minimum precedence
	mov	[fConsumed],0		;we haven't consumed anything yet


;-------------------------------------------------------------------
;State which expects a term (function, constant, or variable).
; If we don't get one, we either return PR_BadSyntax if we've consumed
; 1 or more tokens, or PR_NotFound if we've consumed no tokens
;
State1:
	mov	bx,[pTokScan]		;bx points to current token
	mov	ax,[bx.TOK_class]	;ax = token's class
	cmp	al,CL_id
	je	GotId			;brif token is an id
	cmp	al,CL_resword
	je	GotResWord		;brif token is a reserved word
	cmp	al,CL_lit
	jne	NotTerm			;brif token is not a constant
	call	NtLit			;try to parse a constant
					; It is guarenteed that NtLit cannot
					; return PR_NotFound if the token's
					; class is CL_lit
	jmp	SHORT CheckResult

GotId:	call	NtIdAryElem		;Try to parse an id (may recurse)
					; It is guarenteed that NtIdAryElem
					; cannot return PR_NotFound
					; if the token's class is CL_id
	dec	[cIdArgs]		;NtIdAryElem() bumped cIdArgs,
					; NtExp() bumps it on exit, and we are
					; only supposed to bump it once per
					; invocation of NtExp().
CheckResult:
	or	al,al			;test return code
	jg	State2			;brif PR_GoodSyntax
	jmp	NtExpExit		;return PR_BadSyntax result

;bx points to current token's descriptor
GotResWord:
	mov	ax,[bx.TOK_rw_iOperator]
	inc	ax			;test for UNDEFINED
	je	NotOperator		;brif didn't get an operator
	dec	ax			;ax = IOP_xxx for operator
	cmp	al,IOP_Add
	je	Scan_State1		;brif unary plus
					; no need to emit a unary +
	cmp	al,IOP_Minus
	jne	NotMinus		;brif not if unary minus
	mov	al,IOP_UMinus		;convert to unary form of -

;ax = operator index (IOP_xxx) for current token
NotMinus:
	mov	di,ax			;di = operator index
	test	mpIopPrecedence[di],FOP_unary
	je	NotTerm			;brif not a unary operator
	cmp	al,IOP_LParen
	jne	ConsumeOp		;brif token is not '('
					; -- consume & stack/emit operator

	sub	[pExpTos],2
	mov	bx,[pExpTos]
	mov	WORD PTR [bx],PREC_lPar	;push precedence for '('
					; this precedence can only be popped
					; by right paren
	cmp	bx,dataOFFSET stkExpMin
	jb	ExpTooComplex		;brif stack overflow
Scan_State1:
	call	ScanTok			;skip current token
	mov	[fConsumed],1		;Now we can't return PR_NotFound
	jmp	State1			; because we've consumed something

NotOperator:
	call	NtIntrinsic		;try to parse intrinsic function
	jg	SHORT State2		;brif PR_GoodSyntax (change state)
	jl	J1_NtExpExit		;brif PR_BadSyntax
NotTerm:
	cmp	[fConsumed],1
	je	ExpectedExp		;error if we needed to see a term
					; i.e. we've consumed anything
	;else we never even consumed 1 token, return NotFound
	sub	al,al			;return(PR_NotFound)
	jmp	SHORT J1_NtExpExit

;-------------------------------------------------------------------
;Error handler's (placed here so they can be reached by SHORT jumps)
;
ExpTooComplex:
	mov	ax,MSG_ExpTooComplex	;Error: expression too complex
	call	PErrMsg_AX		;produce parser error msg
					; al = PR_BadSyntax
	jmp	SHORT J1_NtExpExit

;we've encountered something like <term><operator><garbage>
;
ExpectedExp:
	mov	ax,MSG_ExpExp		;Error: Expected expression
ExpErrMsg:
	call	PErrExpMsg_AX		;Error: Expected <ax>
					; al = PR_BadSyntax
J1_NtExpExit:
	jmp	NtExpExit

;-------------------------------------------------------------------
;This code is for the state where we are expecting a binary operator
; or end-of-expression
;
State2:
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_resword
	jne	EndOfExp		;brif not reserved word
	mov	ax,[bx.TOK_rw_iOperator];ax = IOP_xxx for token
	inc	ax			;test for UNDEFINED
	je	EndOfExp		;brif its not an operator
	dec	ax			;ax = operator's IOP_xxx
	mov	di,ax			;di = operator's IOP_xxx
	test	mpIopPrecedence[di],FOP_unary
	jne	EndOfExp		;brif not binary operator (exit)
	cmp	al,IOP_RParen		;check for right paren
	jne	NotRightParen		;brif not

	;Now we call PopTillLParen to cause all operators stacked
	; since the last scanned left paren to be emitted.
	; It also detects if the parenthesis for this expression
	; don't balance, i.e.  the expression (a)), in which
	; case we return, because the right paren we're looking
	; at may be for an array reference.  If it is an error,
	; it will be caught by a higher level.
	
	call	PopTillLParen
	jne	EndOfExp		;brif we got a right paren, but it
					; was beyond the expression we were
					; called to parse.  Exit without
					; consuming this right paren.
	add	[pExpTos],2		;pop left paren's precedence
	mov	ax,opLParen		;emit opLParen pcode
	call	Emit16_AX
	call	ScanTok			;skip right paren
	jmp	SHORT State2		;state remains ExpBinaryOp

;Check for relational operator
; di = IOP_xxx for operator
;
NotRightParen:
	call	RelOp			;see if its a relational operator
	je	ConsumeOp		;branch if not
	
	;iop = RelOp() + IOP_EQ - 1
	
	add	al,IOP_EQ - 1		;ax = operator index - IOP_EQ - 1
	xchg	di,ax			;di = IOP for relational operator

;This is executed when we have scanned an operator while parsing
; an expression.  All stacked operators with precedence greator or
; equal to the scanned operator are emitted, then the scanned operator
; is stacked.  This is how we convert infix to postfix (or reverse polish).
; di = IOP_xxx for operator
;
ConsumeOp:
	mov	si,[pExpTos]		;si points to top of exp stack
	mov	al,mpIopPrecedence[di]	;al = operator's precedence
	sub	ah,ah			;ax = operator's precedence
	shl	di,1			;di = IOP_xxx * 2
	push	mpIopOpcode[di]		;save current operator's opcode
	mov	di,ax			;di = operator's precedence
	test	al,FOP_unary
	jne	EmitDone		;brif unary operator (must be stacked
					; until we emit the term it applies to)
EmitLoop:
	cmp	[si],di
	jb	EmitDone		;brif stacked operand's precedence
					; is less than precedence of
					; current operator
					; (i.e. leave relatively high precedence
					;  operators on the stack)
	inc	si			;pop stacked operator's precedence
	inc	si
	lodsw				;pop and emit stacked operator's opcode
	call	Emit16_AX		;emit the stacked opcode
	jmp	SHORT EmitLoop	

EmitDone:
	sub	si,4			;make room for new entry
	mov	[si],di			;push current operator's precedence
	pop	[si+2]			;push current operator's opcode
	mov	[pExpTos],si		;save exp stack ptr
	cmp	si,dataOFFSET stkExpMin
	jbe	J_ExpTooComplex		;brif exp stack overflow
	jmp	Scan_State1		;scan token, advance state

J_ExpTooComplex:
	jmp	ExpTooComplex		;Error: Expression too complex

;Now we call PopTillLParen to cause all operators stacked by this
; recursive invocation of NtExp to be emitted.  It also detects
; if the parenthesis for this expression don't balance, i.e.
; the expression ((a+5)
;
EndOfExp:
	call	PopTillLParen
	jne	ParensBalance		;brif paranthesis are balanced
	mov	ax,MSG_RightParen	;Error: Expected ')'
	jmp	ExpErrMsg

;Now we pop the minimum precedence operator stack marker which was
;stacked when we entered this recursive invocation of NtExp
;
ParensBalance:
	inc	[cIdArgs]
	mov	al,PR_GoodSyntax	;This is (and must remain) the only
					; exit which returns PR_GoodSyntax
NtExpExit:
	add	[pExpTos],2		;pop off initial stopper
	pop	[mkVar.MKVAR_flags]	;restore caller's mkVar.flags
	or	al,al			;set condition codes for caller
cEnd	NtExp

subttl	Intrinsic Function Nonterminal

;**********************************************************************
; PARSE_RESULT NEAR NtIntrinsic()
;
; Purpose:
;	Parse an intrinsic function.
;
; Entry:
;	If the static variable [oNamConstPs] is non-zero, intrinsic
;	   functions are not allowed
;
; Exit:
;	The value of cIdArgs is preserved
;	If no intrinsic is found, no tokens are consumed, no opcodes
;	   are emitted, and the return value is PR_NotFound.
;	If it is found, a corresponding opcode is emitted and
;	   Parse() is called to check the syntax and generate code
;	   for it.  If the syntax for the intrinsic is good, the
;	   return code is PR_GoodSyntax.  If not the return code
;	   is PR_BadSyntax.
;	Condition codes set based on value in al
;
;******************************************************************
cProc	NtIntrinsic <PUBLIC,NODATA,NEAR>,<si,di>
cBegin	NtIntrinsic
	sub	al,al			;prepare to return PR_NotFound 
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_resWord
	jne	NtIntrExit		;brif not a reserved word
	mov	dx,[bx.TOK_rw_rwf]	;dx = reserved word flags
	test	dx,RWF_FUNC
	je	NtIntrExit		;brif token isn't for intrinsic func
	cmp	[oNamConstPs],0
	je	NotInCONST		;brif not in CONST a=<expression> stmt
	mov	ax,MSG_InvConst		;Error: Invalid Constant
	call	PErrMsg_AX		; al = PR_BadSyntax
	jmp	SHORT NtIntrExit

NotInCONST:
	push	[pCurStkMark]		;preserve caller's pCurStkMarker
	push	[cIdArgs]		;preserve caller's cIdArgs
	mov	[cIdArgs],0		;reset cIdArgs to 0 for this
					; intrinsic function's code generator
	;Fetch info for a particular intrinsic function out of the
	;parser's reserved word table 'tRw'.
	
	mov	si,[bx+TOK_rw_pArgs]	;si -> pRwArgs in tRw
	lods	WORD PTR cs:[si]	;ax=state table offset for func's syntax
	mov	cx,ax			;cx=state table offset
	sub	di,di			;default to no code generator
	test	dx,RWF_FUNC_CG
	je	NoFuncCg		;branch if no code generator for func
	lods	WORD PTR cs:[si]	;ax=adr of code generation func
	mov	di,ax			;di=adr of code generation func
	lods	WORD PTR cs:[si]	;ax=arg to pass to code generation func
	mov	si,ax			;si=code generation arg
NoFuncCg:
	push	cx			;pass oState to Parse
	call	ScanTok			;skip keyword token 
	pop	ax			;ax = oState
	add	ax,OFFSET CP:tState	;ax = pState = &(tState[oState])
	mov	[pStateLastScan],ax
	call	NtParse			;try to parse intrinsic function
	jle	NtIntrNotGood		;branch if result isn't PR_GoodSyntax
	or	di,di
	je	NtIntrGoodSyntax	;branch if no function code generator
	mov	ax,si			;pass arg to code generation routine
					; (usually, this is an opcode)
	call	di			;invoke code generation routine 
NtIntrGoodSyntax:
	mov	al,PR_GoodSyntax	;return PR_GoodSyntax
	jmp	SHORT NtIntrRestore

NtIntrNotGood:
	jl	NtIntrRestore		;branch if result == PR_BadSyntax
	call	PErrState		;Generate error message "Expected
					; <a> or <b> or ..." 
					;al = PR_BadSyntax
NtIntrRestore:
	pop	[cIdArgs]		;restore caller's cIdArgs
	pop	[pCurStkMark]		;restore caller's pCurStkMarker
NtIntrExit:
	or	al,al			;set condition codes for caller
cEnd	NtIntrinsic

subttl	Literal Nonterminals

UNARY_LIT EQU 0
	;	Used when CASE could only be followed by literal instead of Exp.
	;	May easily be useful for some future construct.
	;	Handles up to 1 unary minus.  Could easily be changed
	;	to handle unary +, we would just need to add the opcode.

;**********************************************************************
; PARSE_RESULT NEAR NtLit()
;
; Purpose:
;	Parse any form of literal and, if found, generate a corresponding
;	literal opcode.
;
; Exit:
;	Returns either PR_GoodSyntax, PR_NotFound or PR_BadSyntax
;	Condition codes set based on value in al
;
;******************************************************************
cProc	NtLit <PUBLIC,NODATA,NEAR>,<si,di>
cBegin	NtLit
	mov	di,[pTokScan]		;di points to current token
	cmp	[di.TOK_class],CL_lit
	jne	LitNotFound		;brif already got a unary op
	sub	ax,ax
	or	al,[di.TOK_lit_errCode]	;ax = lexical analyzer's error code
	jne	LitSnErr		;brif lexical analyzer found an error
					; in literal's format
	lea	si,[di.TOK_lit_value_I2];si points to literal's value
	mov	bl,[di.TOK_lit_litType]	;bl = LIT_xxx for literal
	cmp	bl,LIT_STR
	je	GotLitSD
	cmp	bl,LIT_I2
	jne	@F			;branch if not a decimal integer
	mov	ax,[si] 		;ax = value
	cmp	ax,opLitI2Max		; Is value within pcode limit
	ja	@F			;branch if value isn't 0..10
	.erre	OPCODE_MASK EQ 03ffh	; Assure following code is ok
	mov	ah,al			
	mov	al,0			; AX = literal * 0100h
	shl	ax,1			; AX = literal * 0200h
	shl	ax,1			; AX = literal * 0400h
	add	ax,opLitI2		;opcode = opLitI2 w/value in upper bits
	call	Emit16_AX
	jmp	SHORT NtLitGoodSyntax

@@:
	sub	bh,bh			;bx = LIT_xxx for literal
	mov	al,[tLitCwValue + bx]	;al = # words in literal's value
	sub	ah,ah			;ax = # words in literal's value
	mov	di,ax			;di = # words in literal's value
	shl	bx,1			;bx = 2 * LIT_xxx for literal
	mov	ax,[tLitOpcodes + bx]	;ax = opcode
	call	Emit16_AX		;emit the opcode
EmitLitLoop:
	lodsw				;ax = next word of literal's value
	call	Emit16_AX
	dec	di
	jne	EmitLitLoop		;branch if more words to emit
	jmp	SHORT NtLitGoodSyntax

;Got a string constant like "xxxxxxx"
;Emit all source characters between the double quotes.
;If <cbText> is odd, <cbText> is emitted as an odd value,
;and an extra pad byte is appended to keep pcode even-byte alligned.  
;
GotLitSD:
	mov	ax,opLitSD
	call	Emit16_AX
	mov	ax,[di.TOK_oSrc]	;ax = column token started in
	inc	ax			;ax = oSrc + 1 (skip ")
	push	ax			;pass it to EmitSrc
	mov	ax,[si]			;ax = length of string literal in bytes
					;TOK_lit_value_cbStr
	push	ax			;pass size of string to EmitSrc
	call	Emit16_AX		;emit size of the string
	call	EmitSrc			;emit the string itself
NtLitGoodSyntax:
	call	ScanTok			;skip literal token
	mov	al,PR_GoodSyntax
NtLitExit:
	or	al,al			;set condition codes for caller
cEnd	NtLit

LitNotFound:
	sub	ax,ax			;prepare to return PR_NotFound
	jmp	SHORT NtLitExit		;brif we didn't consume unary opcode

;ax = error encountered by lexical analyzer when scanning number
LitSnErr:
	call	PErrMsg_AX		; al = PR_BadSyntax
	jmp	SHORT NtLitExit

sEnd CP
sBegin DATA

;Tables used by NtLit

;Following tables assume following constants:
OrdConstStart 0
OrdConst LIT_I2		; % suffix
OrdConst LIT_O2		; &O prefix
OrdConst LIT_H2		; &H prefix
OrdConst LIT_I4		; & suffix
OrdConst LIT_O4		; &&O prefix
OrdConst LIT_H4		; &&H prefix
OrdConst LIT_R4		; ! suffix
OrdConst LIT_R8		; # suffix
OrdConst LIT_STR	; "xxx"
tLitOpcodes LABEL WORD
	DW	opLitDI2		;LIT_I2	 (% suffix)
	DW	opLitOI2		;LIT_O2	 (&O prefix)
	DW	opLitHI2		;LIT_H2	 (&H prefix)
	DW	opLitDI4		;LIT_I4	 (& suffix)
	DW	opLitOI4		;LIT_O4	 (&&O prefix)
	DW	opLitHI4		;LIT_H4	 (&&H prefix)
	DW	opLitR4			;LIT_R4	 (! suffix)
	DW	opLitR8			;LIT_R8	 (# suffix)

tLitCwValue LABEL BYTE
	DB	1			;LIT_I2	 (% suffix)
	DB	1			;LIT_O2	 (&O prefix)
	DB	1			;LIT_H2	 (&H prefix)
	DB	2			;LIT_I4	 (& suffix)
	DB	2			;LIT_O4	 (&&O prefix)
	DB	2			;LIT_H4	 (&&H prefix)
	DB	2			;LIT_R4	 (! suffix)
	DB	4			;LIT_R8	 (# suffix)

sEnd DATA
sBegin CP

;**********************************************************************
; PARSE_RESULT NEAR NtLitI2() - Parse & emit 16-bit integer
; Purpose:
;	Parse and emit a 16-bit signed integer.  Note this is very
;	different from NtLit() in that it emits no opcode, just
;	a 16 bit value.  It is the responsibility of the caller
;	to emit the opcode before calling this function.
;	If a numeric literal is found, but it is > 32k,
;	an Overflow error message is generated.
; Exit:
;	Returns PR_GoodSyntax, PR_BadSyntax or PR_NotFound
;	Condition codes set based on value in al
;
;******************************************************************
PUBLIC	NtLitI2
NtLitI2	PROC NEAR
	sub	al,al			;prepare to return PR_NotFound
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_lit
	jne	NtLitI2Exit		;branch if token isn't a literal
	cmp	[bx.TOK_lit_type],ET_I2
	jne	NtLitI2Ov		;brif token isn't a signed 16 bit int
	mov	ax,[bx.TOK_lit_value_I2];ax = value
	call	Emit16_AX		;emit it
	call	ScanTok			;consume token
	mov	al,PR_GoodSyntax	;return PR_GoodSyntax
NtLitI2Exit:
	or	al,al			;set condition codes for caller
	ret

NtLitI2Ov:
	mov	ax,ER_OV		;Overflow
	jmp	PErrMsg_AX		;al = PR_BadSyntax
					; return to caller
NtLitI2	ENDP

;**********************************************************************
; PARSE_RESULT NEAR NtLit0() - Parse the literal 0, emit nothing
;******************************************************************
PUBLIC	NtLit0
NtLit0	PROC NEAR
	sub	cx,cx			;expect constant 0
NtLit1Shared:
	sub	al,al			;prepare to return PR_NotFound
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_lit
	jne	NtLit0Exit		;branch if token isn't a literal
	cmp	[bx.TOK_lit_type],ET_I2
	jne	NtLit0Exit		;brif token isn't a signed 16 bit int
	cmp	[bx.TOK_lit_value_I2],cx
	jne	NtLit0Exit		;branch if token isn't 0
	call	ScanTok			;consume token
	mov	al,PR_GoodSyntax	;return PR_GoodSyntax
NtLit0Exit:
	ret
NtLit0	ENDP

;**********************************************************************
; PARSE_RESULT NEAR NtLit1() - Parse the literal 1, emit nothing
;******************************************************************
PUBLIC	NtLit1
NtLit1	PROC NEAR
	mov	cx,1			;expect constant 1
	jmp	SHORT NtLit1Shared
NtLit1	ENDP

;**********************************************************************
; PARSE_RESULT NEAR NtLitString() - Parse a string literal
;******************************************************************
cProc	NtLitString <PUBLIC,NODATA,NEAR>
cBegin	NtLitString
	sub	al,al			;prepare to return PR_NotFound
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_lit_type],ET_SD
	jne	NtLitStringExit		;branch if token isn't string constant
	call	NtLit			;ax = result of parsing the string
NtLitStringExit:
cEnd	NtLitString

sEnd	CP

end
