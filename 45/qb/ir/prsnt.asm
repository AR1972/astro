	TITLE	PRSNT - Parser NonTerminal Functions

;==========================================================================
;
;  Module:  prsnt.asm - Parser NonTerminal Functions
;  Subsystem:  Parser
;  System:  Quick BASIC Interpreter
;  Copyright <C> 1985, Microsoft Corporation
;
;	RULES FOR WRITING NON-TERMINAL FUNCTIONS:
;	----- --- ------- --- -------- ---------
;
;	A 'TERMINAL' is a syntactic item which can be described by
;	   1 token.  Examples include reserved words, constants, variables.
;	A 'NON-TERMINAL' is a complicated syntactic item which is
;	   described by a sequence of TERMINALS and other NON-TERMINALS.
;	   Examples include statement (NtStatement), expressions (NtExp) etc.
;
;	Whenever possible, we try to parse non-terminals with the function
;	Parse(), which uses state-table information in prsstate.asm.
;	Non-Terminal parsing functions are used instead of state-table
;	driven non-terminal entries (in bnf.prs) for the following reasons:
;	-	When we need to look ahead (i.e. PeekNextTok()) to decide
;		which of several similar non-terminals the token stream is.
;	-	When we need to generate complicated pcode.
;	-	When we want to setup state for the code generator which
;		will eventually be called for this statement/intrinsic
;		(See NtACTIONxxx() for an example of this).
;
;	The functions in this module (and in module PrsExp.asm) are responsible
;	for parsing NON-TERMINALs which could not be parsed by Parse().
;	These non-terminal parsing functions must return 1 of 3 values:
;
;	-	PR_NotFound, meaning the non-terminal was not recognized, but
;		no tokens were consumed and no pcode was emitted.  This
;		allows the caller (which is almost always Parse()) to
;		try alternate non-terminals before generating a syntax error.
;
;	-	PR_GoodSyntax, meaning the non-terminal was successfully
;		recognized, tokens were consumed, and pcode was emitted.
;
;	-	PR_BadSyntax, meaning the non-terminal was not recognized, and
;		tokens were consumed and/or pcode was emitted.  An error
;		message must also be generated, like ("Expected <non-terminal>")
;		by calling PErrExpMsg() or similar routines.  This will
;		cause the caller to return all the way up to ParseLine()
;		with the generated syntax error.
;
;=========================================================================

	include version.inc
	PRSNT_ASM = ON
	includeOnce architec
	includeOnce context
	includeOnce opstmt
	includeOnce opid
	includeOnce parser
	includeOnce prsirw
	includeOnce prstab
	includeOnce psint
	includeOnce qbimsgs
	includeOnce rtinterp
	includeOnce rtps

	assumes	CS,CP
	assumes	DS,DGROUP
	assumes	ES,DGROUP
	assumes	SS,DGROUP

sBegin	DATA

	EXTRN	b$ErrInfo:byte		;extended error code

	PUBLIC	pStateCur, pStateLastScan, pStateLastGood
pStateCur	DW 0		;see comments below in NtParse
pStateLastScan	DW 0		;see comments below in NtParse
pStateLastGood	DW 0		;see comments below in NtParse

	PUBLIC	pCurStkMark, minStkMark, maxStkMark
	MARK_STACK_SIZE EQU 10
	;The size of this constant must exceed the maximum number
	;of MARK meta-commands that can occur for any statement
	;in the bnf (see bnf.prs).  Since MARK never appears within
	;a repeating construct (i.e. {id MARK(n)}), we can easily
	;compute the worst case number of MARKs in 1 statement.
	;It is currently 6 for OPEN, so 10 gives plenty of breathing
	;room.  It would be nice at some point to change buildprs
	;(the program that reads bnf.prs) to produce this constant.
	
pCurStkMark LABEL WORD
	DW	0
minStkMark LABEL WORD
	DW	MARK_STACK_SIZE DUP(?)
maxStkMark LABEL WORD


fNeedPrintEos DB 1			;used by NtEndPrint

sEnd	DATA


;*====================================================================
;*    P A R S E   S T A T E   T A B L E   I N T E R P R E T E R
;*
;*    This function make up the interpreter which emulates the
;*    push-down-automata for the recursive descent parser.
;*
;*====================================================================

sBegin	CP
assumes	CS,CP

PUBLIC	RtErrOmStack
RtErrOmStack PROC NEAR
	mov	[b$ErrInfo],OMErr_STK	;so user interface can distinguish
					; this from other types of Out-of-memory
					; when reporting the error
	mov	al,ER_OM		;"Out of memory"
	jmp	RtErrorNoSi
RtErrOmStack ENDP

;*********************************************************************
; PARSE_RESULT NEAR NtParse(ax:pState)
;
; Purpose:
;	Try to match (parse) the token stream which begins with 'pTokScan'
;	to the regular expression identified by pState.
;
; Entry:
;	ax=pState: pointer into tState[] for the syntax to be recognized
;
; Exit:
;	If no errors are encountered, on exit 'pTokScan' should be a valid
;	   syntax terminator.
;	   Return value is PR_GoodSyntax.
;	If the 1st token was not even consumed, the return value is
;	   PR_NotFound.
;	Otherwise, the error(s) are reported and 'pTokScan' is the next
;	   statement terminator.
;	   Return value is PR_BadSyntax.
;	Condition codes set based on value in al
;
; Example:
;	If the state tree pointed to by 'pState' looked like:
;
;           A
;          / \
;         B   C
;        / \   \
;       D   E   F
;
; Then the input token stream had better look like one of the following:
;  ACF or BE or D
;
; Tricky statements to report errors for (i.e. test these if you
; ever alter the error reporting logic):
;  KEY(1)="this"
;  CIRCLE (1,2)),3
;  DO STOP --> make sure error is expected WHILE or end-of-stmt
;  NEXT STOP --> make sure error is expected id or end-of-stmt
;
;********************************************************************
;Register usage:
;	si = pState

;in response to EMIT(opcode) directive, emit the opcode
GotEmitNode:
	lods	WORD PTR cs:[si]	;ax = EMIT's operand
	call	Emit16_AX
	jmp	SHORT ParseLoop

cProc	NtParse,<PUBLIC,NEAR>,<si,di>
	localW	nodeId
	localB	cTokFirst
cBegin	NtParse
	DbChkPsStk			;see if this is a new high-water stack
	cmp	sp,[stkChkParse]
	jbe	RtErrOmStack		;brif almost out of stack space
	xchg	si,ax			;si = pState
	mov	al,[cTokScan]
	mov	[cTokFirst],al		;cTokFirst = cTokScan
ParseLoop:
	;fetch state transition arc from encoded state tree 'tState'
	lods	BYTE PTR cs:[si]	;al = nodeId
	cmp	al,ND_EMIT
	ja	NotDirective		;brif not ACCEPT/REJECT/MARK/EMIT
	je	GotEmitNode		;branch if EMIT node
.errnz	ND_ACCEPT
	or	al,al
	je	J1_GotAcceptNode	;branch if ACCEPT node
	cmp	al,ND_MARK
	je	GotMarkNode		;branch if MARK node
	jmp	GotRejectNode		;else it has to be a REJECT node

J1_GotAcceptNode:
	jmp	GotAcceptNode

;In response to MARK(const) directive, push current
;pcode offset and const onto stack.  This information
;will be used by stmt or func code generator
;
GotMarkNode:
	mov	bx,[pCurStkMark]
	DbAssertRel bx,a,MIN_STK_MARK,CP,<NtParse: marker stack overflow>
	mov	ax,WORD PTR ps.PS_bdpDst.BDP_pbCur
	sub	ax,WORD PTR ps.PS_bdpDst.BDP_pb ;ax = cur offset into out buffer
	dec	bx			;push pcode offset onto MARK stack
	dec	bx			; for use by Code Generator function
	mov	[bx],ax
	dec	bx			;push MARK's operand as well
	dec	bx
	lods	BYTE PTR cs:[si]	;al = MARK's operand
	sub	ah,ah
	mov	[bx],ax
	mov	[pCurStkMark],bx
	jmp	SHORT ParseLoop

Got_STT_Accept:
	sub	ax,ax			;indicates unconditional acceptance
	jmp	SHORT PsHandleNode

;got a branch, token, or nonterminal - fetch its id
;al = 1st byte of nodeId
;
NotDirective:
	sub	ah,ah
	cmp	al,ENCODE1BYTE
	jb	PsOneByteNodeId
	;nodeId = ((nodeId-ENCODE1BYTE) << 8) + (*pState++) + ENCODE1BYTE
	mov	ah,al
	lods	BYTE PTR cs:[si]	;ax = nodeId * 256 + next byte
	sub	ax,255 * ENCODE1BYTE
;ax = nodeId
;node is followed by 1 or 2 byte branch operand, fetch it
;
PsOneByteNodeId:
	mov	[nodeId],ax
	lods	BYTE PTR cs:[si]	;al = 1st byte of operand
	cmp	al,ENCODE1BYTE
	jb	PsOneByteOperand
	cmp	al,255
	je	Got_STT_Accept
	;operand is 2 byte offset into tState, pick up 2nd byte
	;pStateTrue = &tState[((operand-ENCODE1BYTE)<<8) + *pState++]
	
	mov	ah,al
	lods	BYTE PTR cs:[si]	;al=2nd byte of operand
	add	ax,OFFSET CP:tState-256*ENCODE1BYTE
	jmp	SHORT PsHandleNode

;operand is 1 byte relative branch in state table
;al = 1st byte of node's operand
;
PsOneByteOperand:
	sub	ah,ah			;ax = operand
	mov	bl,al			;bl = operand
	add	ax,si
	cmp	bl,ENCODE1BYTE/2
	jbe	PsHandleNode		;brif positive relative branch
	sub	ax,ENCODE1BYTE		;negative relative branch

;ax = state to branch to if we recognize(consume) what we're expecting
PsHandleNode:
	mov	di,ax			;di = pStateTrue
	mov	[pStateCur],ax		;set static var for error reporting
	mov	ax,[nodeId]
	cmp	ax,ND_BRANCH
	je	GotBranch		;brif got unconditional branch
	;got a token or nonterminal node
	sub	ax,ND_BRANCH + 1
	cmp	ax,NUMNTINT
	jae	PsNotIntNt
	;we expect a non-terminal described by a state tree
	mov	bx,ax
	shl	bx,1
	mov	ax,WORD PTR cs:tIntNtDisp[bx]
	add	ax,OFFSET CP:tState	;ax = pState = &(tState[oState])
	call	NtParse			;result = Parse(tIntNtDisp[nodeId])
	jmp	SHORT CheckResult

;ax = node id - (ND_BRANCH + 1)
PsNotIntNt:
	sub	ax,NUMNTINT
	cmp	ax,NUMNTEXT
	jae	GotResWord		;brif expecting a reserved word token
	;we expect a non-terminal defined by a C function
	mov	bx,ax
	shl	bx,1
	call	cs:tExtNtDisp[bx]	;result = (*tExtNtDisp[nodeId])()

;al = PR_NotFound, PR_BadSyntax, or PR_GoodSyntax
CheckResult:
	or	al,al
	jg	RecognizedToken		;brif result was PR_GoodSyntax
	jl	ParseExit		;brif result was PR_BadSyntax
					; non-terminal already generated
					; complete error message
	jmp	ParseLoop		;else result == PR_NotFound, continue
					; scanning by taking FALSE branch from
					; current node

;Unconditional branch to a new parse-state
GotBranch:
	mov	si,di			;si = pStateTrue
					;unconditional branch to another state,
J1_ParseLoop:
	jmp	ParseLoop		;continue scanning

;Expect a reserved word
;ax = node id - (ND_BRANCH + 1) - NUMNTINT
GotResWord:
	sub	ax,NUMNTEXT		;ax=expected res word's IRW_xxx
	mov	bx,[pTokScan]		;bx -> current token
	cmp	WORD PTR [bx],CL_RESWORD
	jne	J1_ParseLoop		;brif token isn't a reserved word
	cmp	[bx.TOK_rw_iRw],ax	;ax = current token's IRW_xxx
	jne	J1_ParseLoop		;brif expected token was not found,
					; continue scanning by taking FALSE
					; branch from current node
	call	ScanTok			;skip recognized token
RecognizedToken:
	or	di,di			;test for pStateTrue for NULL
	je	GotAcceptNode		;brif got acceptance state transition
					;else take TRUE transition out of this
	mov	si,di			; state to another node
	mov	[pStateLastGood],di	;save for error reporting
J2_ParseLoop:
	jmp	ParseLoop

;Got to a leaf node of the parse tree which is not an accepting node,
; ie. we were unable to recognize what Parse() was called to recognize.
; If we didn't get what was expected, but didn't emit code (see note below)
; or consume any
; tokens, its not necessarily a syntax error yet.  If Parse() was called to
; consume some non-terminal, let the caller continue trying other options
; (if any), and when they are all exausted, the caller will generate an
; error msg.
;NOTE: NtParse does not actually test whether any pcode was emitted and so
;NOTE: will still return PR_NotFound even if some pcode has been emitted
;NOTE: as long as no tokens have been consumed. This is used to advantage
;NOTE: in the NonTerminal "AsClause" which will return PR_NotFound for
;NOTE: 		"AS REAL" even though it will emit an opcode.
;
GotRejectNode:
	mov	al,[cTokScan]
	cmp	[cTokFirst],al
	mov	al,PR_NotFound
	je	ParseExit
	call	PErrState		;Produce error message "Expected <a>
					; or <b> or ..."
					;al = PR_BadSyntax
	jmp	SHORT ParseExit

;We recognized parse tree described by initial pState
GotAcceptNode:
	mov	al,PR_GoodSyntax
ParseExit:
	or	al,al			;set condition codes for caller
cEnd	NtParse

;===========================================================================
;    M I S C    P A R S E R    R E C O G N I Z I N G    F U N C T I O N S
;
;    NOTE:  These functions are arranged alphabetically
;
;=======================================================================

;**********************************************************************
; PARSE_RESULT NEAR NtAssignment()
;
; Purpose:
;	Called for the 'LET "var=exp"' statement
;
;******************************************************************
PUBLIC	NtAssignment
NtAssignment PROC NEAR
	mov	bx,[pTokScan]		;bx points to current token
	cmp	[bx.TOK_class],CL_id
	jne	NtAssBadSyntax
	push	bx			;Tell NtImpliedLetOrCall that
					; it must be LET, not CALL
	call	NtImpliedLetOrCall	;al = result
NtAssExit:
	ret
NtAssignment ENDP

NtAssBadSyntax:
	sub	al,al			;al = PR_NotFound
	jmp	SHORT NtAssExit

;*******************************************************************************
; PARSE_RESULT NEAR NtCommaNoEos()
;
; Purpose:
;	Special nonterminal for a comma which is followed by optional arguments.
;	Rather than generate oodles of state table, this nonterminal is used to
;	make sure that the comma, if present, is NOT followed by end of line or
;	end of statement. Generates an opComma if comma found and all is okay.
;
; Returns:
;	PR_NotFound	- no comma found
;	PR_GoodSyntax	- comma found, followed by something.
;	PR_BadSyntax	- comma found, followed by end of statement.
;
;***************************************************************************
cProc	NtCommaNoEos <PUBLIC,NEAR>
cBegin
	mov	ax,IRW_Comma
	call	TestScan_AX
	jne	NoComma			;branch if current token isn't comma
	call	ScanTok			;consume the comma
	call	NtEndStatement
	mov	al,PR_GoodSyntax
	je	NtComExit		;branch if not end-of-statement
	jmp	PErrExpExpr		;Error: expected expression
					; return al = PR_BadSyntax
NoComma:
	sub	al,al			;return PR_NotFound
NtComExit:
cEnd

;**********************************************************************
; PARSE_RESULT NEAR NtEndPrint()
;
; Purpose:
;	Called during a PRINT expression list.
;
; Exit:
;	Always returns PR_NotFound.
;
;******************************************************************
cProc	NtEndPrint <PUBLIC,NEAR>
cBegin	NtEndPrint
	mov	ax,IRW_ELSE
	call	TestScan_AX
	je	GotPrintEos		;branch if current token is ELSE
	call	NtEndStatement		;check for end-of-statement
	je	NotPrintEos		;branch if not end-of-statement
GotPrintEos:
	cmp	[fNeedPrintEos],FALSE
	je	NotPrintEos		;branch if we don't need to emit
	mov	ax,opPrintEos		; opPrintEos to terminate the PRINT
	call	Emit16_AX
NotPrintEos:
	mov	[fNeedPrintEos],0FFh	;set flag to TRUE (non-zero)
	sub	al,al			;return(PR_NotFound)
cEnd	NtEndPrint

;**********************************************************************
; PARSE_RESULT NEAR NtEndPrintExp()
;
; Purpose:
;	Called during a PRINT expression list after an expression has been
;	parsed.
;
; Exit:
;  Always returns PR_GoodSyntax.
;
;******************************************************************
cProc	NtEndPrintExp <PUBLIC,NEAR>
cBegin	NtEndPrintExp
	mov	ax,IRW_ELSE
	call	TestScan_AX		;see if current token is ELSE
	je	GotPrExpEos		;branch if it is
	call	NtEndStatement		;see if current token is end-of-stmt
	je	NotPrExpEos		;branch if not
GotPrExpEos:
	mov	ax,opPrintItemEos
	call	Emit16_AX		;emit end-of-stmt print terminator
	mov	[fNeedPrintEos],FALSE	;so NtEndPrint() won't terminate
	jmp	SHORT NtEndPrGoodSyntax	; print item list with opPrintEos

NotPrExpEos:
	mov	ax,opPrintItemSemi
	call	Emit16_AX
NtEndPrGoodSyntax:
	mov	al,PR_GoodSyntax
cEnd	NtEndPrintExp

;**********************************************************************
; STATICF(uchar) TestLet()
;
; Purpose:
;	Test to see if 'pTokScan' is a 1 letter identifier from 'A' - 'Z'
;
; Exit:
;	al = 0..25 for A-Z and a-z.
;	     26 if its not an ID from A-Z.
;
;******************************************************************
cProc	TestLet <PUBLIC,NEAR>
cBegin	TestLet
	mov	bx,[pTokScan]		;bx points to current token
	mov	ax,26D			;prepare for FALSE return
	cmp	[bx.TOK_class],CL_id
	jne	TestLetExit		;branch if not an id token
	mov	al,[bx.TOK_id_charFirst]
					;al = id's 1st char
TestLetExit:
cEnd	TestLet

;**********************************************************************
; PARSE_RESULT NEAR NtDeflistXX()
;
; Purpose:
;	Parse a letter range like "A-F,X" for statements like DEFINT.
;	If it is not recognized at all, return value is PR_NotFound.
;	If it is only partially found, like "A-", return value is PR_BadSyntax.
;	If it is found, a 32 bit mask is emitted, the tokens are consumed
;	   and the return value is PR_GoodSyntax.
;
; Exit:
;	Emits <opStDefType><link field><low-word><high-word>
;	where
;	   <high-word> has 1 bit set for each letter from A..P
;	   <low-word> has 1 bit set for each letter from Q..Z in the
;		high bits, and type (ET_I2..ET_SD) in the low 3 bits.
;	Updates ps.tEtCur[]
;	It never returns PR_NotFound because the bnf guarentees that
;	nothing else can follow DEFINT.
;
;******************************************************************
SKIP2_BX MACRO
	db	0BBH		;mov bx,<immediate word>
	ENDM

PUBLIC	NtDefListR4
NtDefListR4 PROC NEAR
	mov	al,ET_R4
	SKIP2_BX			;load next 2 bytes to bx (fall through)
NtDefListR4 ENDP
	;fall through



PUBLIC	NtDefListI2
NtDefListI2 PROC NEAR
	mov	al,ET_I2
	SKIP2_BX			;load next 2 bytes to bx (fall through)
NtDefListI2 ENDP
	;fall through

PUBLIC	NtDefListI4
NtDefListI4 PROC NEAR
	mov	al,ET_I4
	SKIP2_BX			;load next 2 bytes to bx (fall through)
NtDefListI4 ENDP
	;fall through

PUBLIC	NtDefListR8
NtDefListR8 PROC NEAR
	mov	al,ET_R8
	SKIP2_BX			;load next 2 bytes to bx (fall through)
NtDefListR8 ENDP
	;fall through

PUBLIC	NtDefListSD
NtDefListSD PROC NEAR
	mov	al,ET_SD
NtDefListSD ENDP
	;fall through
;register conventions:
; bl = 1st char of A-Z pair
; bh = 2nd char of A-Z pair
; si points to maskCur
;
cProc	NtDeflist <PUBLIC,NEAR>,<si,di>
	localW	maskSumLOW
	localW	maskSumHIGH
	localB	oTyp
cBegin	NtDeflist
	mov	[oTyp],al		;save input parm
	sub	ax,ax
	lea	si,[maskSumHIGH]	;si doesn't change for rest of NtDeflist
	mov	[si],ax			;maskSumHIGH = 0
	mov	[si+2],ax		;maskSumLOW = 0
DefListLoop:
	call	TestLet			;al = 1st letter of current token
	cmp	al,26D
	jne	GotValidLetter		;branch if got a valid letter
DefExpLetter:
	;Got something like DEFINT <end-of-stmt> or DEFINT A-B,
	mov	ax,MSG_Letter		;Error: expected letter
	call	PErrExpMsg_AX		; al = PR_BadSyntax
	jmp	short DefListExit

GotValidLetter:
	mov	bl,al			;bl = 1st char
	mov	bh,al			;default bh 2nd char to 1st char
	mov	di,bx			;save letters in di
	call	ScanTok			;skip 1st letter
	mov	ax,IRW_Minus		;check for '-'
	call	TestScan_AX
	jne	NoDefDash		;branch if no '-'
	call	ScanTok			;skip '-'
	call	TestLet			;al = 1st letter of current token
	cmp	al,26D
	je	DefExpLetter
	mov	bx,di			;restore bl = 1st char
	mov	bh,al			;bh = 2nd char of A-Z pair
	mov	di,bx
	call	ScanTok			;skip 2nd letter
NoDefDash:
	mov	bx,di			;restore bl,bh = 1st,2nd chars
	cmp	bl,bh
	jbe	SetTheBits		;branch if 1st char <= 2nd char
	xchg	bl,bh			;Treat DEFINT S-A same as DEFINT A-S

;bl = 1st char to set, bh = last char to set (0..25)
;cl = current letter (0..25)
;ax:dx = maskCur
;
SetTheBits:
	mov	ax,08000H
	sub	dx,dx			;maskCur = 0x80000000
	mov	cl,dl			;current letter = 0
BitSetLoop:
	cmp	cl,bh			;compare current with final
	ja	BitSetEnd		;branch if end of loop
	cmp	cl,bl
	jb	DontSetThisOne
	or	[si],ax			;or ax:dx bits into maskSum
	or	[si+2],dx
DontSetThisOne:
	shr	ax,1			;shift ax:dx right by 1
	rcr	dx,1
	inc	cl			;bump current char
	jmp	SHORT BitSetLoop

BitSetEnd:
	mov	ax,IRW_Comma
	call	TestScan_AX
	jne	DefEndOfList		;branch if no comma
	call	ScanTok			;skip comma
	jmp	DefListLoop

DefEndOfList:
	mov	ax,opStDefType
	call	Emit16_AX
	call	Emit16_0		;leave room for link field
	mov	ax,[si+2]		;ax = low word
	or	al,[oTyp]		;or oTyp into low 6 bits
	call	Emit16_AX		;emit low word first
	mov	ax,[si]			;emit high word second
	call	Emit16_AX

	;update ps.tEtCur[] for source lines like DEFINT A-Z:b=1
	
	lodsw				;ax = maskSumHIGH
	push	ax			
	push	[si]			; maskSumLOW
	mov	bl,[oTyp]		;bl = ET_xxx to set
	push	bx			
	call	far ptr SetDefBits	; update ps.tEtCur table
	mov	al,PR_GoodSyntax
DefListExit:
cEnd	NtDeflist

;**********************************************************************
; SetDefBits
; Purpose:
;	Update the current default-type array as a result of scanning
;	a DEFINT..DEFSTR statement
;	This is used by both the Parser and Static Scanner
; Entry:
;	maskSumHIGH:maskSumLOW = DEFTYPE bit mask, as would appear in
;				 opStDefType's operand
;	defET = ET_xxx to store in si for each bit set in ax:dx
;	ps.tEtCur filled with current default types (one ET_xxx for each letter)
; Exit:
;	ps.tEtCur is updated according to mask
;
;**********************************************************************
cProc	SetDefBits,<PUBLIC,FAR>,<SI>	
	parmW	maskSumLOW
	parmW	maskSumHIGH
	parmB	defET
cBegin
	mov	si,dataOFFSET ps.PS_tEtCur
					;si points to table to be updated
	mov	cx,26D			;cx = repeat count (1 for each letter)
	mov	dx,[maskSumHIGH]	
	mov	ax,[maskSumLOW] 	
	mov	bl,[defET]		
BitTestLoop:
	shl	dx,1			;shift ax:dx left 1
	rcl	ax,1
	jnc	BitNotSet		;branch if this bit is not set
	mov	[si],bl			;store new type in type table
BitNotSet:
	inc	si			;advance to next byte
	loop	BitTestLoop
cEnd



;======================================================================
;	  Ambiguous Statement resolving functions.
;
;	  These functions are needed where the BNF specifies 2 or more
;		statements which begin with the same keyword.  These functions
;		look ahead in the pcode to decide which of the statements we're
;		really looking at.
;
;==================================================================

OSTMT1 EQU 0	; offset from current position in reserved word table
		; for the first (as ordered in bnf.prs) 
		; of the possible statements.
OSTMT2 EQU 6	; offset for the second possible statement

;*******************************************************************************
; ushort NEAR AmDEF()
;
; Purpose:
;	Determine if statement is a DEF FN or a DEF SEG statement.
; Exit:
;	ax = offset into reserved word entry for correct statement
;
;***************************************************************************
PUBLIC	AmDef
AmDef	PROC NEAR
	mov	ax,IRW_SEG
	jmp	SHORT AmCommon
AmDef	ENDP

;*******************************************************************************
; ushort NEAR AmGET(), AmPut()
;
; Purpose:
;	Determine if statement is a graphics or I/O GET/PUT.
;
; Method:
;	If GET/PUT is followed by a left paren or STEP, this must be
;	a graphics GET/PUT.
;
;***************************************************************************
PUBLIC	AmGet
AmGet	PROC NEAR
AmGet	ENDP
	;fall into AmPut
PUBLIC	AmPut
AmPut	PROC NEAR
	call	Peek1Tok
	mov	ax,IRW_STEP
	call	TestPeek_AX
	je	AmGotExpected		;brif followed by STEP
	mov	ax,IRW_LParen
	jmp	SHORT AmTestPeek	;see if followed by '('
AmPut	ENDP

;*******************************************************************************
; ushort NEAR AmLINE()
;
; Purpose:
;	Determine whether statement is a LINE INPUT or a graphics
;	LINE statement.
;
; Method:
;	We just look at the next token in the input stream. If it is INPUT, then
;	we know it's a LINE INPUT, else it's a graphics line command.
;
;***************************************************************************
PUBLIC	AmLine
AmLine	PROC NEAR
	mov	ax,IRW_INPUT
AmLine	ENDP
	;fall into AmCommon

; Entry:
;	ax = reserved word id (IRW_xxx) for token for 2nd stmt in list
; Exit:
;	ax = OSTMT2 if reserved word found, OSTMT1 (0) if not
;
AmCommon PROC NEAR
	push	ax
	call	Peek1Tok		;peek at next token
	pop	ax			;ax = token we're looking for
AmTestPeek:
	call	TestPeek_AX
AmGotExpected:
	mov	ax,OSTMT2		;assume next token is [ax]
	je	AmExit			;branch if next token is [ax]
	sub	ax,ax			;ax = 0
AmExit:
	ret
AmCommon ENDP

;*******************************************************************************
; ushort NEAR AmPLAY()
;
; Purpose:
;	Determine if we're starting a PLAY event switch statement,
;	or the standard PLAY statement.
;
;***************************************************************************
PLAY_STMT EQU 0
PLAY_EVENT EQU 6
PUBLIC	AmPlay
AmPlay	PROC NEAR
	call	Peek1Tok		;peek at next token
	mov	ax,IRW_ON
	call	TestPeek_AX
	je	AmPlayEvent		;branch if next token is ON
	mov	ax,IRW_OFF
	call	TestPeek_AX
	je	AmPlayEvent		;branch if next token is OFF
	mov	ax,IRW_STOP
	call	TestPeek_AX
	je	AmPlayEvent		;branch if next token is STOP
	sub	ax,ax			;ax = PLAY_STMT
	jmp	SHORT AmPlayExit

AmPlayEvent:
	mov	ax,PLAY_EVENT
AmPlayExit:
	ret
AmPlay	ENDP

CP	ENDS
end
