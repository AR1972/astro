page	,132
	TITLE	ssstmts - Scan general statements
;***
;ssstmts - scan general statements
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Scan statements other than control flow, procedures, declarations,
;	or I/O or graphics specific formats.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSSTMTS_ASM	=	ON
	IncludeOnce	qbimsgs
	IncludeOnce	rtps
	IncludeOnce	ssint
	IncludeOnce	variable
	.list

assumes ds, DATA
assumes es, DATA
assumes ss, DATA


sBegin	SCAN

assumes cs, SCAN

	page
;***
;Ss_NotDefined - Scan routine for unsopported opcodes
;
;Purpose:
;
;   This routine should never be executed since all invalid opcodes should be
;   caught by the rude scanner.
;
;******************************************************************

SsProc	Unsupported,Rude,Local		
	DbHalt	SCAN,<Ss_Unsupported>	
SsD_Unsupported:
	DbHalt	SCAN,<SsD_Unsupported>	

	page
;***
;Ss_StCnt - Scan routine for statements with cnt args of same type
;
;Purpose:
;
;   Scan statement (non-emitting) opcodes with 1 operand which is
;   the count of arguments of the same type (like SCREEN).  The
;   rule table index is the type required.
;
;   The mpOpExe entry is the executor address.	There is one executor
;   for each of the opcodes that use this scan routine.
;
;Input:
;
;   es:si   = pcode emission address
;   es:di   = pcode emission address
;
;Output:
;
;   si updated
;
;Preserves:
;
;   Ss_ChrN requires that dx be preserved [2]
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************

SsProc	StCnt
	STOSWTX 			;emit the executor
	LODSWTX 			;Pick up argument count
	STOSWTX 			;Emit the argument count
	xchg	cx,ax
	jcxz	StCntX			;No arguments - exit.
CheckTyps:
	shr	bx,1			;bx = opcode
	mov	al,mpOpRule[bx]		;Load rule table offset
	cbw				;Zero ah
StCntChkLoop:
	call	EnsureArgType
	loop	StCntChkLoop 		;loop to process cx arguments
StCntX:
	jmp	[ScanRet]		;Return to scan loop


;***
;StView[Screen]
;
;	These two opcodes have 6 operands, so a special scan routine is
;	required.
;
;******************************************************************


SsProc	StView
	STOSWTX
	mov	cx,6
	jmp	CheckTyps


	page
;***
;Ss_ChrN - Scan routine for functions with cnt args of same type
;Purpose:
;	Scan function opcodes with 1 operand which is 
;	the count of arguments of the same type.  The
;	rule table index is the type required.
;
;	The mpOpExe entry is the executor address. 
;	Currently this scan routine is only used for QBJ's CHR$
;	function.
;
;Input:
;	es:si	= pcode emission address
;	es:di	= pcode emission address
;Output:
;	si updated
;Modifies:
;Exceptions:
;	Ss_Error
;******************************************************************


	page
;***
;Ss_4ET_ET - Scan routine for case where rule word has 4 ET types
;
;Purpose:
;
;   Scan statements that have independently coerced simple input
;   types, cause no coercion, and have either no operands or where
;   the operands simply get copied.
;
;   This scan routine expects the rule table index to point to a rule
;   entry as follows:
;   Rule Word: Contains 1 to 4 ET types (1 per nibble)
;	       Each nibble describes the next argument type
;   Rule Byte: Emit type or LOWUND
;
;   The mpOpExe entry is the executor address.	There is only one
;   executor for each of the opcodes that use this scan routine.
;
;Input:
;
;   ax	    = executor
;   es:si   = pcode emission address
;   es:di   = pcode emission address
;
;Output:
;
;   si updated
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************

SsProc	4ET_ET
	;Get EXE for this opcode

	call	EmitExCopyOps		;Emit executor and copy operands
					; bx = opcode on exit
	call	GetRuleInfo		;Get rule table info for opcode bx
					;ax = rule byte
					;bx = rule word
GotRuleInfo:
	xchg	dx,ax			;Save emit type in dx

	;bx = up to 4 oTyps, nybble-packed
	;dx = result oTyp, 0 if none

	or	bx,bx			;Test for work to do
	jz	ss4ETX			;No work to do
NextArg:
	mov	ax,bx
	and	ax,0fH			;ax = required type
	call	EnsureArgType		;Perform the coercion as required

	;Shift to next rule

	mov	cl,4
	shr	bx,cl			;Move to arg type in next nibble
	jnz	NextArg 		;Go coerce next arg if there is one

ss4ETX:
	;Emit output type if there is one

	or	dl,dl			;Test for indication of no emit type
	jz	UpdateOTxStart		; No type to emit
	push	di			;Emit the expression location
	push	dx			;Emit the expression type
ScanX:
	jmp	[ScanRet]		;Return to scan loop

UpdateOTxStart: 			
	mov	[SsOTxStart],di 	; Update clear stack location
	jmp	ScanX			

	subttl	LOCK/UNLOCK
	page
;***
;Ss_StLock - LOCK/UNLOCK Statement Scan Support
;
;Purpose:
;
;   Scan support for the LOCK and UNLOCK statements.
;
;   Pcodes for these statements have a word operand that contains
;   a bit pattern indicating the count of arguments.  This is unlike
;   other statements, and requires specific scan support.
;
;   Syntax:
;
;    LOCK | UNLOCK <filenum> [, (exp [TO exp]) | (TO exp)]
;
;	Pcode:
;
;    (I2channel, [I4RecNo [,I4RecNoTo]]) opSt<Un|>Lock (U2mode)
;
;	U2Mode has following bits set:
;
;	    LOCK_UNLOCK      set if operation is UNLOCK, not LOCK
;	    LOCK_1stToLast   set if only part of file locked
;	    LOCK_RtMask      bits which are meaningful to runtime
;	    LOCK_Def1stArg   set if 1st arg defaulted - used by list
;	    LOCK_DefLastArg  set if last arg defaulted- used by exec
;
;	    bit 1:  set if user specified one or both record numbers,
;		    clear if both record numbers defaulted
;	    bit 15: set if user specified the first record number and
;		    defaulted the second (QBI use only)
;
;Input:
;
;   ax	    = executor
;   es:si   = pcode emission address
;   es:di   = pcode emission address
;
;Output:
;
;   si updated
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************

	.errnz	LOCK_DefLastArg -  8000H

SsProc	StLock
	STOSWTX 			;Emit the executor
	LODSWTX 			;Load operand
	STOSWTX 			; and emit it

	test	al,LOCK_1stToLast	;see if user said 'All records'
	jz	StLockAll		;Lock whole file case

	mov	cx,2			;Assume FROM and TO
	test	ah,080H 		;Are they both present?
	jz	StLockLoop		;Both present
	dec	cx			;Back to 1 arg only

StLockLoop:
	mov	ax,ET_I4		;Ensure all I4 arguments
	call	EnsureArgType		;Force arg to I4
	loop	StLockLoop

StLockAll:
	mov	ax,ET_I2
	call	EnsureArgType		;File number argument is an I2

	jmp	[ScanRet]		;Return to main loop


	page
;***
;Ss_FieldItem - Scan routine for Field statement clauses in QB
;
;Purpose:
;
;   This scans the opFieldItem opcodes that appear for each clause in
;   the Field statement.  It is necessary to distinguish fixed strings
;   from variable length strings and report an error for fixed strings.
;   Also, only variables are allowed.  An error is issued for constants.
;
;Input:
;
;   es:si   = pcode emission address
;   es:di   = pcode emission address
;
;Output:
;
;   si updated
;
;Preserves:
;
;Modifies:
;
;Exceptions:
;
;   Ss_Error
;
;******************************************************************

	;Start of [10]


SsProc	FieldItem
	mov	cx,bx			;Save opcode * 2
	pop	ax			;Get type of field item
	pop	bx			;Get oTx for error
	push	bx			;Restore to scan stack
	push	ax
	TestX	ax,ST_Var?		; Is this a variable?
	jnz	@F			; Brif yes, no problem
	mov	ax,ER_VarReq		; Variable is required
	jmp	short FieldError	; Report error
@@:
	cmp	al,ET_FS		;Is this a fixed string?
	jne	FieldOk
	mov	ax,Msg_InvFixStr
FieldError:
	call	SsErrorBx
FieldOk:
	xchg	ax,dx			;AX = Executor
	mov	bx,cx			;BX = Opcode * 2
	jmp	Ss_4ET_ET		;Continue with scanning


	;End of [10]

sEnd	SCAN
end
