	TITLE	prsmain.asm - Parser Main Module

;==========================================================================
;
;  Module:  prsmain.asm - Parser Main Module
;  Subsystem:  Parser
;  System:  Quick BASIC Interpreter
;
;==========================================================================

	include		version.inc
	PRSMAIN_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	names
	includeOnce	opcontrl
	includeOnce	opid
	includeOnce	opmin
	includeOnce	parser
	includeOnce	pcode
	includeOnce	prsirw
	includeOnce	prstab
	includeOnce	psint
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	rtps
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	util
	includeOnce	stack2			

;--------------------------------------------------------------------------
;
;  The BIC Parser is a table driven recursive descent parser.
;  Because it is used by the Interpreter, it can make no assumptions
;  about the order in which it sees statements.  This means it has
;  to consider each statement as an atomic unit.  Checking syntax
;  which spans multiple statements, like matching FOR NEXT statements,
;  is left to the static scanner, a separate BIC component.
;
;  The parser receives tokens from FetchToken().  For the interpreter,
;  it produces an infix pcode stream.  For the compiler, it returns
;  the top node of a syntax tree.
;
;  The fundamental Control hierarchy of the parser component is as follows:
;
;                      (ParseLine)
;                            |
;             +-----------------------------+
;             |                             |
;       (ParseLabelOrLineNum)            (Parse)
;             |       |                     |
;             +----+  | +--------+----------|
;                  |  | |        |          |
;                (ScanTok)  (Peek1Tok)      |
;                     |          |          |
;                     +-------+--+          |
;                             |         (NtXXXX)
;                         (FetTok)
;                             |
;                        (FetchToken)
;                         | |  |
;                         | |  |
;             +-----------+ |  |
;             |             |  |
;      [ONamOfPbCb] ($i8input) |
;                              |
;                          (FindRw)
;
;=========================================================================*
;
;  The tables which this module uses to perform its parsing are produced
;  by the utility program 'buildprs' (a recursive descent parser generator).
;  See 'parser.doc' for a definition of how these parse tables are built.
;  The following is an example of how they are used.
;  The lexical analyzer recognizes all reserved words.  When it encounters
;  one, it returns a pointer to a structure which shows all the opcodes
;  which map to this reserved word, as well as whether or not it is
;  a legal keyword to start a statement or intrinsic function.
;  If it can start a statement or intrinsic function, this list also contains
;  an offset into the parser state table which describes the syntax for
;  the statement/function.  The following is an example of how CALL would
;  be parsed.
;
;    Reserved-Word-Table    Syntax-State-Table
;      CALL---------+
;                   |
;                   +-----> s24: [id->s27]  [error]
;                           s27: ["("->s31]  [empty->accept]
;                           s31: [svar->s36]  [exp->s43]  [error]
;                           s36: ["("->s40]  [empty->s43]
;                           s40: [")"->s43]  [error]
;                           s43: [","->s31]  [")"->accept]  [error]
;
;    This state table represents the following state transition graph:
;
;               ID        (        svar       (         )         )
;    CALL (s24)---->[s27]---->(s31)---->(s36)---->(s40)---->(s43)---->[]
;                         +-->   |         +------------+-->   |
;                         |      |  exp                 |      |
;                         |      +----------------------+      |
;                         |                          ,         |
;                         +------------------------------------+
;
;
;
;    Where [sn] is a "final" or "accepting" state, and (sn) is not.
;
;--------------------------------------------------------------------

assumes	ds,DATA
assumes	ss,DATA
assumes	es,NOTHING


sBegin	DATA
PUBLIC	stkChkParse, psFlags
stkChkParse	DW 0
psFlags		DB 0			;general purpose parser internal flags

bdParseUndo	DB SIZE bd DUP(0)	;buffer used by ParseUndo()


sEnd	DATA


sBegin	CP
assumes	cs,CP

;--------------------------------------------------------------------
;           P A R S E - A - L I N E    F U N C T I O N S
;--------------------------------------------------------------------

;*********************************************************************
;EmitLabel
;	Emit a label definition (i.e. opBol, opBolSp, opBolLabSp etc.)
; Entry:
;	di = 0 for line number, 1 for alpha label:
;	si = name table offset for label/line number
;Exit:
;	appropriate label opcode is emitted
;	carry set if duplicate label error
;Uses:	di
;
;*********************************************************************
EmitLabel PROC NEAR
	push	si			;pass label to FlagOfONam
	call	FlagOfONam		;ax = name's flags
	test	al,NM_fLineNumLabel
	jne	DupLabel		;brif linenum already declared
	mov	ax,[ps.PS_bdpSrc.BDP_pbCur]
	sub	ax,[ps.PS_bdpSrc.BDP_pb] ;ax = number of leading blanks
	push	ax
	call	ScanTok			;skip label
	dec	di
	jnz	LineNum			;brif not alpha label
	call	ScanTok			;skip ':'
LineNum:
	pop	ax			;ax = number of leading blanks
	mov	bx,[pTokScan]
	mov	di,[bx.TOK_oSrc]
	sub	di,ax			;di = updated # of leading blanks
	cmp	di,1
	jbe	NoLeadingBlanks		;brif not 2 or more leading blanks
	cmp	[ps.PS_bdpDst.BDP_cbLogical],0
	mov	ax,opBolLabSp
	je	Bol1			;brif we're at beginning of line
					; i.e. no pcode has been emitted yet
	mov	ax,opLabSp
Bol1:
	call	Emit16_AX		;emit the opcode
	call	Emit16_0		;leave room for link field
	push	si
	call	Emit16			;emit oNam field
	push	di			;emit count of leading blanks
	jmp	SHORT ElEmit

NoLeadingBlanks:
	cmp	[ps.PS_bdpDst.BDP_cbLogical],0
	mov	ax,opBolLab
	je	Bol2			;brif we're at beginning of line
	mov	ax,opLab
Bol2:
	call	Emit16_AX		;emit the opcode
	call	Emit16_0		;leave room for link field
	push	si
ElEmit:
	call	Emit16			;emit oNam field
	clc
ElExit:
	ret

DupLabel:
	mov	ax,ER_DL OR PSERR_fAlert ;report duplicate label
	call	ParseErrTokScan		;ParseErr(ax)
	stc				;set carry to indicate error
	jmp	SHORT ElExit
EmitLabel ENDP

;*********************************************************************
;EmitBol
;Purpose:
;	Emit an opBol, opBolSp, opBolInclude, or opBolIncludeSp
;Entry:
;	ax = opBolInclude or opBol
;
;*********************************************************************
EmitBol	PROC NEAR
	mov	bx,[pTokScan]
	mov	cx,[bx.TOK_oSrc]	;ax = #bytes leading spaces
	jcxz	NoSpc			;brif no leading blanks
	.errnz	opBol
	or	ax,ax
	jne	NotOpBol		;brif not opBol
	cmp	cx,24d
	ja	NotOpBol		;brif too many spaces
	.errnz	OPCODE_MASK - 3FFh
	mov	ah,cl			;ax = 256 * cSpaces
	shl	ax,1			;ax = 512 * cSpaces
	shl	ax,1			;ax = 1024 * cSpaces
	jmp	SHORT NoSpc

NotOpBol:
	push	cx			;pass cb to Emit16 (below)
	inc	ax			;map opBol->opBolSp
					;    opBolInclude->opBolIncludeSp
	call	Emit16_AX
	mov	al,[cInclNest]		;al = $INCLUDE nesting depth (0 if none)
	and	ax,0FFh			;ax=al, can't use cbw--doesn't set flags
	je	NotIncl1		;brif not an included line
	call	Emit16_AX		;emit $INCLUDE nesting depth
NotIncl1:
	pop	ax			;ax = #leading blanks
	jmp	Emit16_AX		;emit the #leading blanks pushed above
					; and return to caller

NoSpc:
	call	Emit16_AX		;emit opcode
	mov	al,[cInclNest]		;al = $INCLUDE nesting depth (0 if none)
	and	ax,0FFh			;ax=al, can't use cbw--doesn't set flags
	je	NotIncl2		;brif not an included line
	call	Emit16_AX		;emit $INCLUDE nesting depth
NotIncl2:
	ret
EmitBol	ENDP

;*********************************************************************
; STATICF(VOID) ParseLineNumAndLabel()
;
; Purpose:
;	Parse an optional line number and/or label definition and emit
;	pcode for them.  If no label, emit an opBol.
;
; Exit:
;	appropriate opBolXXX opcode(s) are emitted
;	if error occurred, ps.errCode = error code, carry set
;	else carry is clear on exit
;
;*********************************************************************
ParseLineNumAndLabel PROC NEAR
	push	si
	push	di
	cmp	[cInclNest],0
	je	NoInclude		;brif source line isnt from include file
	mov	ax,opBolInclude
	call	EmitBol
NoInclude:
	call	TestLn			;ax = oNam for line number or 0
	jc	PlabExit		;brif error (Overflow, out-of-memory)
	je	NoLineNum		;brif no line number
	xchg	si,ax			;save oNam in si
	sub	di,di			;tell EmitLabel its a line number label
	call	EmitLabel		;emit label
	jc	PlabExit		;brif duplicate label
NoLineNum:
	call	IdTokPeriodImp		;next token can have "." in it
					; but must have no explicit type char
	je	NoLabel1		;branch if PR_NotFound
	call	Peek1Tok		;pTokPeek -> token after pTokScan
	mov	ax,IRW_Colon
	call	TestPeek_AX
	jne	NoLabel1		;brif not ':'

	mov	bx,[pTokScan]
	mov	si,[bx.TOK_id_oNam]
	mov	di,1			;tell EmitLabel its an alpha label
	call	EmitLabel		;emit the label definition
NoLabel:
	cmp	[ps.PS_bdpDst.BDP_cbLogical],0
	jne	PlabExit		;brif a label or linenum was emitted
	.errnz	opBol
	sub	ax,ax			;mov	ax,opBol
	call	EmitBol			;emit an opBol or opBolSp
	jmp	SHORT PlabGood

NoLabel1:
	call	LexReset		;rescan pTokScan ("." is terminator)
	jmp	SHORT NoLabel

PlabNoSpc:
	.errnz	opBol
	call	Emit16_0		;emit an opBol
PlabGood:
	clc				;indicate no error
PlabExit:
	pop	di
	pop	si
	ret	
ParseLineNumAndLabel ENDP

;*********************************************************************
; boolean NEAR ParseLine()
; Purpose:
;	Parse a line of BASIC source, producing pcode and or
;	error message text.
; Entry:
;	ps.bdpSrc contains the zero-byte terminated ASCII source line to
;	   be parsed.
;	grs.fDirect is true if we're parsing a direct mode statement.
;	if grs.fDirect is FALSE, grs.oMrsCur and grs.oPrsCur identify
;	   the module/procedure being edited.
;	ps.bdpDst describes destination buffer to receive generated pcode.
;	ps.bdErr describes destination buffer for error message text.
;	ps.PS_flags & PSF_fParseExp is non-zero if parser is to parse just
;	   an expression, zero if it is to parse a source line
;	other ps.PS_flags must be 0
; Exit:
;	condition codes set based on value in ax
;	If no errors were encountered,
;	   PSW.C is not set,
;	   ps.errCode=0
;	   ps.bdpDst contains generated pcode.
;	   If any labels or variables were referenced, on output,
;	      ps.flags & PWF_fRef is set true, so the text manager knows
;	      to scan the whole program if the parsed statement was in
;	      direct mode.
;	   grs.oPrs is updated if a SUB/FUNCTION/DEF statement for an
;	      as yet undefined procedure is parsed (during ASCII Load), in which
;	      case, the text manager inserts the text at the beginning of the
;	      new module.
; 
;	If any error was encountered,
;	   PSW.C is set,
;	   ps.flags PSF_UndoEdit is set if caller should back out of
;	      current edit.
;	   ps.flags PSF_fRetry is set if caller should call ParseLine
;	      again for the current edit.
;	   ps.flags PSF_fRudeIfErr is non-zero if ModuleRudeEdit is to be called
;	      if for any reason, this line's pcode is stored as opReParse,
;	      or not inserted at all.
;	   ps.oSrcErr contains the offset into ps.bdpSrc to the offending text.
;	   ps.bdpDst contains garbage.
;	   If a syntax error was encountered,
;	      ps.errCode & PSERR_fAsciiMsg is set non-zero and ps.bdErr contains
;	      a parser-built ASCII error message
;	   Else
;	      ps.errCode & PSERR_errCode contains an offset into the
;	      QBI Message Table (MSG_xxx or ER_xxx)
;	   If the variable manager returns an error code which
;	      means a RudeEdit is being performed, ps.errCode & PSERR_fRude
;	      is non-zero.  If the user wants to go through with the edit,
;	      TxtChange() will cause the module's value table to be destroyed
;	      and the module to be de-scanned to SS_RUDE
;	   If the error was a serious error, i.e. the kind of error which
;	      we want to flag as soon as the user enters it,
;	      ps.errCode & PSERR_fAlert is set non-zero.  An example of
;	      when this wouldn't be set is if the user referenced an as-yet
;	      undefined TYPE, causing the variable manager to return a
;	      'ReParse' error code.  This allows the text mgr to remember
;	      the pcode in an opReParse, but not report the error to the user.
;	      The reason this is not reported as an error is because
;	      the user may define the TYPE before a RUN is attempted.  If
;	      it is still an error when TxtDirect() is going through
;	      its ReParse list before a RUN, the error is reported to the
;	      user at that time.
;
;********************************************************************/

ParseExp:
	or	[psFlags],PSIF_fNot1stStmt
					;so we give "expected end-of-statement
					; error" instead of "expected statement"
					; if expression isn't terminated
					; by end-of-line
	call	NtConsumeExp		;parse expression (error if not found)
	jmp	short CheckResult	;check result in al

PUBLIC	ParseLine
ParseLine PROC NEAR
;Static variable stkChkParse assumes b$pend never moves.
;If this ever becomes invalid, just move code from ParseInit to ParseLine.
DbAssertRel [b$pend],e,[initBpEnd],CP,<ParseLine: b$pend moved>
	sub	ax,ax			;ax = 0
	mov	[ps.PS_errCode],ax
	mov	[psFlags],al
	mov	[pStateCur],ax
	mov	[pStateLastScan],ax
	mov	[pStateLastGood],ax
	cmp	[grs.GRS_fDirect],al
	jne	PlNoBind		;brif we're in direct mode
	cmp	[txdCur.TXD_scanState],SS_RUDE
	je	PlNoBind
	or	[psFlags],PSIF_fBindVars ;bind var refs
PlNoBind:

	;reset all parser buffers to their start
	mov	ax,[ps.PS_bdpSrc.BDP_pb]
	mov	[ps.PS_bdpSrc.BDP_pbCur],ax
	call	ResetDstPbCur		;discard anything in this buffer

	call	ScanTok			;pick up 1st token on line
	test	[ps.PS_flags],PSF_fParseExp
	jne	ParseExp		;brif called to parse a Watch Expression
	call	ParseLineNumAndLabel	;consume and emit line number and
					; label definition
	jc	PlErr			;brif some error in label

	call	NtStatementList0	;parse a list of statements
					; [:] stmt [: stmt [: ...]]
CheckResult:
	jl	PlErr			;brif bad syntax, ps.bdErr already
					; contains ASCII error message,
					;PR_NotFound is ok (empty stmt list)
	mov	ax,IRW_NewLine
	call	TestScan_AX		;test for end-of-line
	jne	NoEndOfLine		;brif didn't get expected end-of-line
	test	[psFlags],PSIF_fLineHasPeriodId
	je	NoAdotB			;brif line contains no A.B identifiers
	mov	ax,opNoType		;emit an opEot to terminate pcode
	call	Emit16_AX
NoAdotB:
	mov	ax,opEot		;emit an opEot to terminate pcode
	call	Emit16_AX
	cmp	[ps.PS_errCode],0
	jne	PlErr			;brif got some error like out-of-memory
					; or variable creation error
	clc				;return carry clear (no error)
PlExit:
	ret	

NoEndOfLine:
	mov	ax,MSG_ExpStatement	;Error "Expected statement"
	test	[psFlags],PSIF_fNot1stStmt
	je	PlReportErr		;brif never got 1st statement on line
					;else didn't get expected end-of-line
	mov	ax,IRW_ELSE
	call	TestScan_AX
	je	BadElse			;brif got ELSE
	mov	ax,IRW_ELSEIF
	call	TestScan_AX
	jne	PlExpEos
;Tried to put ELSE or ELSEIF after 1st statement on line,
BadElse:
	mov	ax,MSG_1stStmt		;Error: "Must be 1st statement on line"
	call	PErrMsg_AX
	jmp	SHORT PlErr

PlExpEos:
	call	PErrState		;generate "Expected A or B or C based
					; on parse table state where last
					; token was scanned.
	mov	ax,MSG_eos		;Error "Expected End-of-Statement"
;ax = text for what we expected
PlReportErr:
	call	PErrExpMsg_AX

;This point is only reached if the line entered had bad syntax,
;or we ran out of memory during some stage,
;or the variable manager detected an error.
PlErr:
	;Halt if non-release version, and user entered -TglParseErrs
	call	far ptr ParseErrInit
	DbAssertRel [ps.PS_errCode],ne,0,CP,<prsmain.asm: errcode != 0>
	stc				;return with carry set (error)
	jmp	SHORT PlExit
ParseLine ENDP



;*********************************************************************
; boolean FAR SetPsBufSz(szStmt)
; Purpose:
;	Set the content of the parser's source buffer.
; Entry:
;	szStmt points to a 0-byte terminated string.
; Exit:
;	If out-of-memory,
;	   returns FALSE
;	else
;	   The line is copied to the global buffer ps.bdpSrc
;	   returns TRUE (non-zero)
;
;********************************************************************/
cProc	SetPsBufSz,<PUBLIC,FAR>
	parmW	szStmt
cBegin
	push	[szStmt]
	call	CbSz			;ax = length of string
	xchg	dx,ax			;dx = length of string
	inc	dx			;include room for 0-byte terminator
	sub	ax,ax			;prepare to return FALSE
	cmp	dx,[ps.PS_bdpSrc.BDP_cbLogical]
	ja	SetSzExit		;brif no room for command

	;Now copy the block from szStmt to the parser's buffer
	push	[szStmt]
	push	[ps.PS_bdpSrc.BDP_pb]
	push	dx
	call	CopyBlk
	mov	ax,sp			;return non-zero (success)
SetSzExit:
cEnd

;*********************************************************************
; SetDstPbCur()
; Purpose:
;	Set parser's output pcode buffer's current pointer field
;	(ps.bdpDst.pbCur) pointing to the end of the buffer.
;	This is called after ps.bdpDst.cbLogical has been altered.
;
; Preserves:
;	All registers except ax, flags
;
;*********************************************************************
PUBLIC	ResetDstPbCur
ResetDstPbCur PROC NEAR
	mov	[ps.PS_bdpDst.BDP_cbLogical],0
	mov	[bdParseUndo.BD_cbLogical],0
ResetDstPbCur ENDP
	;fall into SetDstPbCur
PUBLIC	SetDstPbCur
SetDstPbCur PROC NEAR
	mov	ax,[ps.PS_bdpDst.BDP_pb]
	add	ax,[ps.PS_bdpDst.BDP_cbLogical]
	mov	[ps.PS_bdpDst.BDP_pbCur],ax
	ret	
SetDstPbCur ENDP

;*********************************************************************
; void FAR ParseInit()
; Purpose:
;	Called once during initialization to initialize the parser
;
;*********************************************************************

AllocBd PROC NEAR
	mov	dl,IT_NO_OWNERS
	sub	cx,cx			;byte count = 0
AllocBd ENDP
	;fall into AllocBd1
AllocBd1 PROC NEAR
	push	ax			;pass ptr to buffer
	push	cx			;pass byte count
	push	dx			;pass flags
	call	BdAlloc
	or	ax,ax
	je	J1_RtErrorOM_INI	;fatal out-of-memory error
	ret
AllocBd1 ENDP
	

J1_RtErrorOM_INI:
	jmp	RtErrorOM_INI		;fatal error, never returns

STKCHK_NtParse EQU 400d	;actually 242, add 158 bytes for maintenance/uncertainty
;
;STKCHK_ToNtParse is the number of bytes of stack space needed to get from
; UserInterface (where caller ensures STACK_CHECK bytes exist between sp
; and b$pend) and NtParse().
;
STKCHK_ToNtParse EQU 350d ;actually 228d, add 122 for maintenance/uncertainty

cProc	ParseInit,<FAR,PUBLIC>
cBegin	ParseInit
	;Runtime ensures we never enter the user interface with less
	;than STACK_CHECK bytes free.  Make sure that STACK_CHECK is big enough
	;to satisfy parser's requirements.
	
DbAssertRel <STKCHK_ToNtParse+STKCHK_NtParse>,b,STACK_CHECK,CP,<ParseInit stk>

	;Set static variable which prevents the recursive parser from
	;over-running the memory allocated for stack space (b$pend)
	
	mov	ax,[b$pend]
	add	ax,STKCHK_NtParse
	mov	[stkChkParse],ax

	; BdAlloc(&ps.bdpSrc, 0, (char)IT_NO_OWNERS_BDP)
	mov	ax,dataOFFSET ps.PS_bdpSrc
	mov	dl,IT_NO_OWNERS_BDP
	sub	cx,cx			;byte count = 0
	call	AllocBd1

	; BdAlloc(&ps.bdpDst, 0, (char)IT_NO_OWNERS_BDP)
	mov	ax,dataOFFSET ps.PS_bdpDst
	mov	dl,IT_NO_OWNERS_BDP
	mov	cx,CB_PCODE_MIN		;never let bdpDst get smaller than
	call	AllocBd1		; CB_PCODE_MIN, so we can always
					; execute a SYSTEM, SETMEM, CLEAR stmt
					; in direct mode.

	; BdAlloc(&bdEMScratch, 0, (char)IT_NO_OWNERS)
	mov	ax,dataOFFSET bdEMScratch
	call	AllocBd

	; BdAlloc(&ps.bdErr, 0, (char)IT_NO_OWNERS)
	mov	ax,dataOFFSET ps.PS_bdErr
	call	AllocBd

	; BdAlloc(&bdParseUndo, 0, (char)IT_NO_OWNERS)
	mov	ax,dataOFFSET bdParseUndo
	call	AllocBd
	jmp	SHORT ParseErrInitStart	
ParseInit ENDP

;Called during initialization and after errors
cProc	ParseErrInit,<FAR>		
cBegin	ParseErrInit			
ParseErrInitStart:
	;set all token pointers start of circular token queue
	mov	ax,dataOFFSET tLookAhead
	mov	[pTokLast],ax
	mov	[pTokScan],ax
	mov	[pTokPeek],ax
	mov	[pExpTos],dataOFFSET stkExpInit
					;reset NtExp's stack
					; (for expression parsing)
cEnd	ParseErrInit			

;*********************************************************************
; void FAR ParseNewInit()
; Purpose:
;	Called once during initialization and for NEW statement
;	to change size of parser's source buffer to 256.
;	ASCII Load can increase the size of the parser's source
;	buffer to the length of the longest line loaded.
; Exit:
;	ax = zero if out-of-memory
;
;*********************************************************************
cProc	ParseNewInit,<PUBLIC,FAR>
cBegin
	PUSHI	ax,<dataOFFSET ps.PS_bdpSrc>
	PUSHI	ax,MIN_EDITLINE
	call	BdRealloc
	or	ax,ax
	jz	ParseNew_Exit

	PUSHI	ax,<dataOFFSET bdEMScratch>
	PUSHI	ax,MIN_EDITLINE
	call	BdRealloc
ParseNew_Exit:
cEnd

;*********************************************************************
; RudeIfErr
; Purpose:
;	If current line gets any kind of error, we will descan to ss-rude.
;	First, ask user if he wants to back out of edit for Edit & Continue.
;	This is called before calling MakeVariable for CONST ID=1
;	because if the line is never inserted, the variable table
;	still contains the now bogus entry for ID.
; Exit:
;	If user wants to back out of edit,
;	   ps.errCode = ER_IER (any error code would do other than ER_CN
;	                as long as PSERR_fAlert bit is not set, user will
;			never see the error.  Any non-zero value prevents
;			us  from calling MakeVariable for rest of this line.
;	   ps.flags PSF_UndoEdit bit is set, telling caller to back out of edit.
;	else
;	   ps.flags PSF_RudeIfErr is set, telling caller of ParseLine
;	   to call ModuleRudeEdit if any error occurs before this
;	   line's pcode gets inserted into the text table.
;
;*********************************************************************
PUBLIC	RudeIfErr
RudeIfErr PROC NEAR
	call	AskCantCont_CP		;see if user wants to back out of edit
	jne	RiNoBackOut		;brif not
	mov	ax,ER_IER
	call	ParseErr0		;stop's subsequent calls to MakeVariable
	or	[ps.PS_flags],PSF_UndoEdit
	ret
RiNoBackOut:
	or	[ps.PS_flags],PSF_fRudeIfErr
	ret
RudeIfErr ENDP

;*********************************************************************
; void ParseUndoLog()
; Purpose:
;	Called to remember something ParseUndo must handle if
;	for any reason, this statement's pcode doesn't make it
;	into a text table without errors.
; Entry:
;	al = entry type (PUNDO_xxx)
;	dx = type specific argument (oNam, oPrs, etc.)
; Exit:
;	if out-of-memory, ps.errCode = ER_OM, [QB4]
;	   or MSG_LineTooLong [EB]
;	   ax = zero if out-of-memory, PSW set accordingly
;
;*********************************************************************
cProc	ParseUndoLog,<PUBLIC,NEAR>
cBegin
	push	ax			;save entry type
	push	dx			;save entry argument
	PUSHI	ax,<dataOFFSET bdParseUndo>
	PUSHI	ax,3			;size of 1 entry
	call	BdGrow			;allocate space for entry
	pop	dx			;pop entry argument
	or	ax,ax			;test return value from BdGrow()
	pop	ax			;al = entry type
	je	PulOm			;brif BdGrow returned out-of-memory
	mov	bx,[bdParseUndo.BD_pb]
	add	bx,[bdParseUndo.BD_cbLogical]
	mov	[bx-2],dx		;store entry argument
	mov	[bx-3],al		;store entry type
	DbAssertRel ax,ne,0,CP,<ParseUndoLog: ax=0>
;ax = return value, condition codes already set
PulExit:
cEnd

PulOm:
	call	ParseErrOm		;set ps.errCode to ER_OM
	sub	ax,ax			;return 0 (out-of-memory)
	jmp	SHORT PulExit

;*********************************************************************
; void ParseUndo()
; Purpose:
;	Called when line which was partially parsed by ParseLine is
;	found to have an error.  It undoes any static actions (like
;	setting of name table bits) caused by ParseLine. It scans
;	the entries created by ParseUndoLog and takes following actions:
;	  PUNDO_oNamAs - Call ChkLastAs to see if no other refs to
;	    oNam AS in pcode.  If so, oNam's NM_fAs name table bit is reset.
;	  PUNDO_oPrsRef - call ChkDelPrs to see if PrsFree should
;	    be called for this prs, since no other refs to this prs exist.
;
;*********************************************************************
cProc	ParseUndo,<PUBLIC,NEAR>,<si>
cBegin
	mov	si,[bdParseUndo.BD_pb]
PudLoop:
	mov	ax,[bdParseUndo.BD_pb]
	add	ax,[bdParseUndo.BD_cbLogical]
	cmp	si,ax
	jae	PudDone
	lodsb				;al = entry type
	dec	al
	lodsw				;ax = oNam/oPrs (flags unaffected)
	jne	NotAsType
	.errnz	PUNDO_oNamAs - 1
	call	ChkLastAs		;reset NM_fAs bit if appropriate
	jmp	SHORT PudLoop
NotAsType:

;ax = oPrs if SUB/FUNCTION/DECLARE
	.errnz	PUNDO_oPrsRef - 2
	call	UndefPrs		;tell txtmgr we deleted "defining" ref
	jmp	SHORT PudLoop

PudDone:
	mov	[bdParseUndo.BD_cbLogical],0
					;so if we're called twice for
					; the same line, the 2nd call will
					; be a nop
cEnd

;*********************************************************************
; VOID NEAR MakeOpReParse()
; Purpose:
;	This is called when some parse-time error is encountered.
;	Discard current contents of ps.bdpDst and replace it with an
;	opReParse token for the current source line.
; Entry:
;	ps.bdpSrc contains the current 0-byte terminated source line
; Exit:
;	If an out-of-memory error occured,
;	   ps.errCode = ER_OM
;	   ps.bdpDst contains garbage
;	else
;	   ps.bdpDst contains opBol, opReParse(cTxt, link, text), opEot
;	   (the 0-byte terminator is not included in the opReParse)
;
;********************************************************************/
PUBLIC	MakeOpReParse
MakeOpReParse PROC NEAR
	call	ResetDstPbCur		;discard any output produced thus far
	.errnz	opBol
	mov	al,[cInclNest]		;al = $INCLUDE nesting depth (0 if none)
	and	ax,0FFh			;ax=al, can't use cbw--doesn't set flags
	je	NotIncl			;brif source line isnt from include file
	push	ax
	mov	ax,opBolInclude
	call	Emit16_AX		;emit opBolInclude
	pop	ax			;ax = $INCLUDE nesting depth (0 if none)
NotIncl:
	call	Emit16_AX		;emit an opBol (or opBolIncl operand)
	mov	ax,opReParse
	call	Emit16_AX		;emit an opReParse

	;Copy all of source buffer ps.bdpSrc to pcode buffer ps.bdpDst
	PUSHI	ax,0			;EmitSrc(0, cbText)

	push	[ps.PS_bdpSrc.BDP_pb]	;pass ptr to 1st byte of source line
	call	CbSz			;ax = length of stmt
	push	ax			;pass to EmitSrc (below)

	inc	ax			;include room for link field
	inc	ax
	call	Emit16_AX		;emit count
	call	Emit16_0		;leave room for link field
	call	EmitSrc			;parms were pushed several lines above
	mov	ax,opEot
	jmp	Emit16_AX		;emit ax and return to caller
MakeOpReParse ENDP

sEnd	CP
end
