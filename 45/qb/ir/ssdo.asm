page	49,132
TITLE	ssdo - scan support for DO/WHILE related opcodes
;***
;ssdocase.asm
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Scan DO/LOOP, and WHILE/WEND statement opcodes.
;
;   Runtime behavior of LOOP opcodes:
;   ---------------------------------
;      <exp> opStDoWhile(oText)     - branch to oText if exp is zero (false)
;      <exp> opStWhile(oText)	    - branch to oText if exp is zero (false)
;      <exp> opStDoUntil(oText)     - branch to oText if exp is non-zero (true)
;      opStLoop(oText)		    - unconditionally branch to oText
;      opStWend(oText)		    - unconditionally branch to oText
;      opStDo			    - nop
;      <exp> opStLoopWhile(oText)   - branch to oText if exp is non-zero (true)
;      <exp> opStLoopUntil(oText)   - branch to oText if ext is zero (false)
;      <exp> opStExitDo(oText)	    - unconditionally branch to oText
;
;   DO [WHILE | UNTIL]/LOOP and WHILE/WEND statement syntax to pcode mappings:
;   --------------------------------------------------------------------------
;
;      Syntax:	DO WHILE <exp> ... LOOP
;   
;					  +-----------------+
;      Pcode:	opBol | <exp> opStDoWhile(|) ... opStLoop(|)|
;		      +-----------------------------------+
;
;      ============================================================
;      Syntax:	WHILE <exp> ... WEND
;   
;					+-----------------+
;      Pcode:	opBol | <exp> opStWhile(|) ... opStWend(|)|
;		      +---------------------------------+
;
;      Note: WHILE/WEND and DO WHILE/LOOP have different opcodes for
;	     listability, but are functionally equivilent.  They both
;	     map to the same runtime executors for size reduction.
;
;      ============================================================
;      Syntax:	DO UNTIL <exp> ... LOOP
;   
;					  +-----------------+
;      Pcode:	opBol | <exp> opStDoUntil(|) ... opStLoop(|)|
;		      +-----------------------------------+
;
;   DO/LOOP [WHILE | UNTIL] statement syntax to pcode mappings:
;   -----------------------------------------------------------
;
;      Syntax:	DO ... LOOP WHILE <exp>
;   
;
;      Pcode:	opStDo| ... <exp> opStLoopWhile(|)
;		      +-------------------------+
;
;      ============================================================
;      Syntax:	DO ... LOOP UNTIL <exp>
;   
;
;      Pcode:	opStDo| ... <exp> opStLoopUntil(|)
;		      +-------------------------+
;
;      ============================================================
;      Syntax:	EXIT DO
;   
;      Pcode:	opStExitDo(otext) to beyond opStLoop*
;
;      Note the scan routine for EXIT DO is the same as EXIT FOR and is
;      in ssfor.asm.
;
;
;****************************************************************************

	.xlist
	include		version.inc
	IncludeOnce	qbimsgs
	IncludeOnce	ssint
	.list

assumes ds, DATA
assumes es, NOTHING
assumes ss, DATA
assumes cs, SCAN

sBegin	SCAN
subttl	Static data area definitons.
page
;***
;Ss_Do, Ss_DoLoop, Ss_While
;Purpose:
;	Scan entries for DO, DO WHILE, DO UNTIL, and WHILE.
;
;	Scan tasks for DO WHILE, DO UNTIL, and WHILE include:
;	- ensuring the entry type is a fundamental, non string data type.
;	- selecting the DO executor varient for the argument data type.
;	- pushing a DO/WHILE frame on the scan stack as follows:
;		push  oTx of DO operand
;		push  oTx of opcode after opBol for return branch from LOOP/WEND
;		push  DO frame label (identifying DO WHILE, DO UNTIL, WHILE)
;
;	Scan tasks for DO
;	- push a DO frame on the scan stack as follows:
;		push junk
;		push oTx of opCode after DO for return branch
;		push DO frame label
;Input:
;	Standard scan entrypoint
;Output:
;	Standard scan exit
;***************************************************************************
SsProc	While				;WHILE entry point
	mov	dh,high STYP_While	;Specify a WHILE frame
	jmp	short DoLoopCom

SsProc	DoLoop				;DO WHILE, and DO UNTIL scan
	mov	dh,high STYP_Do 	;Specify a DO frame
DoLoopCom:
	pop	ax			;Get oTyp of last expression
	pop	cx			;Discard coercion point (None used)
	push	bx			;Save opcode * 2
	cCall	MapOpToExeNumeric	;Type explode the executor
	STOSWTX 			;Emit the executor
	pop	bx
	shr	bx,1			;bx = opcode (byte offset to mpOpRule)
	mov	dl,mpOpRule[bx] 	;dl = Do varient
	mov	bx,[SsOTxStart] 	; BX = oTx after BOS
	mov	ax,di			;di = oTx of exit branch
	jmp	short SsDoCom

SsProc	Do				;DO scan
	STOSWTX 			;Emit the executor
	mov	bx,di
	mov	ax,UNDEFINED		;No initial exit branch
	mov	dx,STYP_Do		;just a plain old DO frame
SsDoCom:
	push	ax			;Push head of exit branch chain
	push	bx			;Push return branch oTx
	push	dx			;Push frame type
	cmp	dx,STYP_Do		;is this a DO...LOOP?
	jz	SsDoSkip		;brif so, no operand to emit
	mov	ax,UNDEFINED		;UNDEFINED will terminate Exit chain
	STOSWTX 			;emit Exit chain terminator
	inc	si
	inc	si			;skip source operand
SsDoSkip:
	jmp	[ScanRet]

	page
;***
;Ss_LoopWhile, Ss_Loop
;Purpose:
;	Scan entries for LOOP WHILE, LOOP UNTIL, LOOP, and WEND.
;
;	Scan tasks for LOOP WHILE and LOOP UNTIL include:
;	- ensure the entry type is a fundamental, non string data type.
;	- emit the LOOP executor varient for the argument data type.
;	- pop DO frame, check errors
;	  + DO nesting error if top frame is not a DO
;	  + DO nesting error if frame is DO WHILE, DO UNTIL
;	- bind LOOP to DO
;	- bind EXIT DO chain to end of LOOP
;
;	Scan tasks for LOOP and WEND
;	- emit executor
;	- pop DO/WHILE frame, check errors
;	  + nesting error if top frame is not a matching DO/WHILE
;	  + DO nesting error if frame is plain DO
;	- bind LOOP to DO opBol
;	- bind EXIT DO chain and DO operand to end of LOOP
;
;Input:
;	Standard scan entrypoint
;Output:
;	Standard scan exit
;***************************************************************************
SsProc	LoopWhile
	pop	ax			;Get oTyp of last expression
	pop	cx			;Discard coercion point (None used)
	push	bx
	cCall	MapOpToExeNumeric	;Type explode the executor
	pop	bx
	jmp	short LoopCom

SsProc	Wend
	mov	dh,high STYP_While	;need to look for a While stack entry
	jmp	short WendCom

SsProc	Loop
LoopCom:
	mov	dh,high STYP_Do 	;need to find a DO on the stack
WendCom:
	STOSWTX 			;Emit the executor
	shr	bx,1			;bx = opcode (byte offset to mpOpRule)
	mov	dl,mpOpRule[bx] 	;dx = Do varient
	pop	ax			;ax = frame type

	mov	cx,MSG_Loop		;assume a LOOP w/o DO error
	cmp	dh,ah			;is this a matching DO/WHILE frame?
	jne	LoopError		;brif not - scoping error
	cmp	dh,high STYP_While	;is this a WHILE/WEND match?
	je	LoopScopeOk		;brif so - scope ok

	mov	dh,dl
	or	dh,al
	jz	LoopScopeOk		;have a DO -> LOOP
	jpe	LoopErrMsg		;scope error - either
					;DO WHILE -> LOOP UNTIL, or
					;DO UNTIL -> LOOP WHILE
	cmp	dl,al			;is this DO WHILE -> LOOP WHILE,
					;or DO UNTIL -> LOOP UNTIL?
	je	LoopErrMsg		;brif so - scope error

LoopScopeOk:
	pop	ax			;oTx for Loop branch
	STOSWTX 			;bind LOOP to DO
	inc	si
	inc	si			;skip over operand in source
	pop	bx			;oTx of DO [WHILE|UNTIL],WHILE operand
	call	BindExitCur		;bind all loop EXIT paths

LoopExit:
	jmp	[ScanRet]

LoopError:
	cmp	dh,high STYP_Do 	;is this a LOOP without DO?
	jz	LoopErrMsg		;brif so
	mov	cx,ER_WE		;WEND without WHILE
LoopErrMsg:
	push	ax			;put back frame type on stack
	xchg	ax,cx
	call	SsError
	MOVSWTX 			;skip operand
	jmp	short LoopExit		;exit

subttl	Opcode to executor maps for Do
page
public mStWhileOpExe
mStWhileOpExe:
	DWEXT exStI2While
	DWEXT exStI4While
	DWEXT exStR8While
	DWEXT exStR8While

public mStDoWhileOpExe
mStDoWhileOpExe:
	DWEXT exStDoI2While
	DWEXT exStDoI4While
	DWEXT exStDoR8While
	DWEXT exStDoR8While

public	mStDoUntilOpExe
mStDoUntilOpExe:
	DWEXT exStDoI2Until
	DWEXT exStDoI4Until
	DWEXT exStDoR8Until
	DWEXT exStDoR8Until

public mStLoopWhileOpExe
mStLoopWhileOpExe:
	DWEXT exStLoopI2While
	DWEXT exStLoopI4While
	DWEXT exStLoopR8While
	DWEXT exStLoopR8While

public	mStLoopUntilOpExe
mStLoopUntilOpExe:
	DWEXT exStLoopI2Until
	DWEXT exStLoopI4Until
	DWEXT exStLoopR8Until
	DWEXT exStLoopR8Until

sEnd	SCAN
end
