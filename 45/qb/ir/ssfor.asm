page	49,132
TITLE	ssfor	- scan support for For/Next
;***
;ssfor.asm
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Scan For/Next.
;
;	For executors utilize a For block that is allocated by the scanner.
;	The allocation is from the frame.
;
;	For opcode variants are:
;	  (IdLd,exp,exp[,exp]) opStFor<Step|> (UNDEFINED,UNDEFINED)
;
;	For executor variants are:
;	  (IdRf,exp,exp[,exp]) exStFor<Step|><I2|I4|R4|R8|CY> (oFrame,oTxNext)
;
;	where:
;
;	oFrame	   is the offset to the For block, which contains step and limit
;	oTxNext    is the oTx of the oTx operand of the Next associated with
;		   this For.  The For executor uses this oTx to branch to the
;		   Next executor to do the limit test for the first iteration.
;
;	Next opcode variants are:
;	   ([IdLd|]) opStNext<Id|> (UNDEFINED,UNDEFINED)
;
;	where:
;	<Id|>	indicates whether the user labeled the Next.
;
;	The statement:
;	Next i,j,k
;	maps to several opStNextId opcodes.
;
;	The Next executor variants are:
;	   (IdRf) exStNext<Id|><Step|><I2|I4|R4|R8|CY>
;
;	The IdLd's of both For and Next are converted to IdRf.
;	The scanner supplies the IdRf in the case that the Next is a
;	opStNext.  However, this Id is not listed and is removed at descan.
;
;	Not all executor variants are unique.  For example, the R8 variant
;	of For supplies a Step of 1 if the user does not so specify.  There
;	need be no Step versions of R8 Next.
;
;Exceptions:
;	Errors detected during For/Next scanning are:
;	- Nesting errors (For without Next and Next without For).
;	- Variable type errors (SD or user data types, arrays, or
;	  array elements
;
;For to Next binding:
;			+------------------------+
;			|			 V
;	exStFor (oFrame,oTx) ... exStNext (oFrame,oTx)
;			    ^			   |
;			    +----------------------+
;
;
;****************************************************************************

	.xlist
	include 	version.inc
	IncludeOnce	context
	IncludeOnce	qbimsgs
	IncludeOnce	scanner
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	variable
	.list

assumes ds, DATA
assumes es, NOTHING
assumes ss, DATA
assumes cs, SCAN

sBegin	SCAN
subttl	For
page
;***
;Ss_For - Scan For statement
;Purpose:
;
;   Scan For statement.  The following tasks are performed:
;	1. Make a For scan stack entry - each item is a word:
;	    For identifier flags
;		STYP_For
;		STYP_Step
;	    oTx of For statement
;	    oTx for start of EXIT For chain (initially UNDEFINED)
;	    oTx of exIdRf opcode
;	    oTyp of For index
;
;	2. Convert IdLd to IdRf.  Coerce step, limit, and initial value
;	   to the type of the index variable.
;
;	3. Map and emit the executor
;	   Executor calculation involves these factors:
;	    1. opcode to exe map For this opcode
;	       From mpOpExe
;	    2. Direct mode or main level code
;	       From grsCur
;	    3. Data type of index variable
;	       From Ld stack entry
;	    4. Whether Step is present.
;	       From RULE table index
;
;***************************************************************************
.erre	low STYP_Step	;The following code and PEROPCOD.TXT assume that the
			;STYP_Step bit is one of the bits in the low byte.
			;This flag is obtained from mpOpRule where it is set
			;as <low STYP_Step> and the mpOpRule byte is loaded into
			;the low byte below.
SsProc	For,Rude
;Calculate STYP_Step for this For
	push	bp			; Set up local frame pointer to ease
	mov	bp,sp			;	access to index variable oTyp
	shr	bx,1
	test	mpOpRule[bx],STYP_Step	; Step clause present?
	jz	NoStep			; Brif no step clause
	mov	ax,[bp+14]		; Get oTyp of index (Record = ET_RC)
	mov	cx,3			; Coerce three values
	jmp	short CoerceFor		

NoStep:					
	mov	ax,[bp+10]		; Get oTyp of index (Record = ET_RC)
	mov	cx,2			; Coerce two values
CoerceFor:				
	pop	bp			; Restore BP.  Discard frame pointer.
	.erre	ST_Typ_Mask EQ 0ffh	; Assure CBW is sufficient
	.erre	ET_MAX LT 80h		; Assure CBW is sufficient
	cbw				; Clear flags in scan stack
	call	SsCoerceN		

	xor	ch,ch
	mov	cl,mpOpRule[bx] 	;cx now has correct value for STYP_Step
	shl	bx,1			;Back to opcode * 2
	mov	dx,bx			; Save in dx
;Obtain the IdLd executor address, check type of For index.
	pop	ax			; oTyp of For index (Record=ET_RC)
	pop	bx			; oTx of For index
	push	ax
	call	MakeRef 		;Convert IdLd to IdRf
	pop	ax			;Get type back
	and	ax,ST_Typ_Mask		; Map to ET_type
	.erre	ET_RC EQ 0		; Assure JZ is sufficient
	jz	ForTypeBad		; For index is a record
	.erre	ST_Typ_Mask EQ 0ffh	; Assure we can use AL
	cmp	al,ET_MaxNum		; Numeric type?
	jbe	ForTypeOK		
ForTypeBad:				
	mov	al,ET_I2		; Leave a valid type
ForTypeOK:
	push	ax			;FRAME gets type
	push	bx			; FRAME gets IdRf executor oTx

;Begin mapping to executor
	dec	ax
	shl	ax,1			;AX = zero relative word offset
	mov	bx,dx			; opcode*2 back to bx
	mov	bx,mpOpExe[bx]		;Address of executor map
	add	bx,ax			;Address of executor for this For
	mov	ax,word ptr cs:[bx]	;Load ...
	STOSWTX 			;... and emit the executor
	MOVSWTX 			;Copy current For block offset

.errnz	UNDEFINED - 0FFFFH
	mov	dx,UNDEFINED		;Get an UNDEFINED to push as the
					; EXIT For link head
	push	dx			;FRAME gets initial EXIT For link
	push	di			;FRAME gets address of oTx for this For
	or	cx,STYP_For		;cx = scan frame identifier
	push	cx			;FRAME gets frame identifier
	MOVSWTX				;Skip over oTx operand
	jmp	[ScanRet]		; and back to the scan loop


subttl	ForBlockAlloc - allocate the For block
page

;***
;ForBlockAlloc - allocate the For block
;Purpose:
;	For/Next executors require a For block to store limit and Step.
;	This block is allocated when scanning For.
;
;	The block is allocated from the current frame (the same as
;	frame space for dynamic variables).
;
;	A For may be scanned several times during the course of user program
;	development.  For blocks may be active during user edition.  For
;	block allocation supports CONT by utilizing the currently allocated
;	block for the For if one exists.
;
;Input:
;	cx = STYP_Step
;	dx = data type of For
;
;Output:
;	ax = new For block offset
;Preserves:
;	bx,cx,dx
;***************************************************************************
ForBlockAlloc:
;Calculate bytes required for this For block
	mov	ax,dx
	call	CbTypOTypSCAN		; bytes for data type
	cmp	dx,ET_I2		;Only I2 can have no Step
	jnz	ForStepCom		;  as For supplies Step for other types
	test	cl,STYP_Step		;Is Step present?
	jz	ForNoStepCom		;No Step, so ax = For block size
ForStepCom:
	shl	ax,1			;Times 2 for Step
ForNoStepCom:
;ax = bytes for For block
	test	byte ptr [grs.GRS_oRsCur+1],80H	;Module or procedure level?
	jnz	ProcLevel
	add	ax,mrsCur.MRS_cbFrameVars	;Get current top of frame
	mov	mrsCur.MRS_cbFrameVars,ax	;Put back new top of frame
	jmp	short MakeOBP

ProcLevel:
;Allocate the For block from the frame of the current procedure.
	add	ax,prsCur.PRS_cbFrameVars	;Get current top of frame
	mov	prsCur.PRS_cbFrameVars,ax	;Put back new top of frame
MakeOBP:
	neg	ax				;oBP
	ret

subttl	For Opcode to Executor maps
page
public	mStForOpExe
mStForOpExe:
	DWEXT exStForI2
	DWEXT exStForI4
	DWEXT exStForR4
	DWEXT exStForR8

public	mStForStepOpExe
mStForStepOpExe:
	DWEXT exStForStepI2
	DWEXT exStForStepI4
	DWEXT exStForStepR4
	DWEXT exStForStepR8

subttl	Next
page
;***
;Ss_Next, Ss_NextId
;Purpose:
;	Scan For/Next.
;
;	Next opcode variants are:
;	   ([IdLd|]) opStNext<Id|> (UNDEFINED,UNDEFINED)
;
;	where:
;	<Id|>	indicates whether the user labeled the Next.
;
;	The statement:
;	Next i,j,k
;	maps to several opStNextId opcodes.
;
;	The Next executor variants are:
;	   (IdRf) exStNext<Id|><Step|><I2|I4|R4|R8|CY>
;
;	The scanner supplies the IdRf in the case that the Next is a
;	opStNext.  However, this Id is not listed and is removed at descan.
;	Bit 0 of the oBP field is set if the IdRf was not inserted (due
;	to out-of-memory or a previous error).
;
;	Not all executor variants are unique.  For example, the R8 variant
;	of For supplies a Step of 1 if the user does not so specify.  There
;	need be only a Step versions of R8 Next.
;
;	For blocks are allocated at Next scan time.  If the For already
;	has a valid oBP (i.e., not -1), then it is used.  Otherwise,
;	if the Next has a valid oBP, it is used.  If neither are valid,
;	then a new oBP is allocated and CantCont is set.  This method allows
;	either a For or Next (but not both) to be edited and still retain
;	their previous For block.
;
;	However, a previous For block can only be used if its type and size
;	have not changed.  This can only happen when the For is edited, not
;	the Next.  In order to tell, the oText field of the Next is set at
;	descan time to have the oTyp and the Step flag.  (Step only matters for
;	I2, where a step of 1 uses a separate executor instead of a word in
;	the For block.)
;
;For to Next binding:
;			+------------------------+
;			|			 V
;	exStFor (oFrame,oTx) ... exStNext (oFrame,oTx)
;			    ^			   |
;			    +----------------------+
;
;Scan time tasks include:
;	1. Detect nesting error (Next w/o For)
;	2. Change IdLd to IdRf, or insert IdRf if not an Id variant of Next.
;	3. Calculate and emit the executor
;	   Factors include:
;		<Id|>		separate executor map
;				(mStNextOpExe or mStNextIdOpExe)
;		type		from scan stack entry/previous IdRf
;		<Step|> 	from scan stack entry and type
;	4. Link For to Next and Next to For
;
;	([IdLd|]) opStNext<Id|> (UNDEFINED,UNDEFINED)
;	(IdRf) exStNext<Id|><Step|><I2|I4|R4|R8|CY>
;Input:
;	Standard Scanner dispatch entrypoint
;	[SP] =	For stack frame
;		For identifier flags
;			STYP_For
;			STYP_Step
;		oTx of For statement
;		oTx for EXIT For chain start
;		For block allocation
;		oTx of exIdLd executor
;		oTyp of for index
;Output:
;
;Exceptions:
;	Errors detected during Next scanning are:
;	- Nesting errors (Next without For).
;
;*******************************************************************
SsProc	NextId,Rude
;Make the preceding IdLd an IdRf
	xchg	cx,ax			;Save executor map in cx
	pop	ax			;Get oTyp of index (Record = ET_RC)
	pop	bx			;IdLd operand address + 2
	call	MakeRef 		;Convert IdLd to IdRf
	mov	ax,PTRTX[bx-2]		; Fetch operand

;Frame the stack for easy For entry referencing
	push	bp
	mov	bp,sp
	push	cx			;Save executor map
	mov	cx,6			;Bind EXIT For beyond the Next executor
	call	BindExitFor		;Find For entry on stack, binding EXITs
	jz	NextWOForErrNoFrame	;For entry not found - error

;Check for compatible IdRf between For and Next
	mov	bx,[bp+2].FFor_oTxIdRf	;oTx of IdRf executor
	sub	ax,PTRTX[bx-2]		; Same operand (variable)?
	jz	NextCom 		;Next matches For - cont through Ss_Next

NextWOForErr:
	pop	bx			;Get executor map
	call	NextErrorCommon
	jmp	Unframe

NextErrorCommon:
	mov	ax,ER_NF		;Next without For error
	call	SsError
	mov	ax,word ptr cs:[bx]	;Get any old executor
	STOSWTX				;And emit it
	mov	ax,-1
	STOSWTX				;Indicate no oBP
	STOSWTX 			;Flag Next without IdRf
	add	si,4			;Skip oBP and oTx in source
	ret

NextWOForErrNoFrame:			;No For Frame to tear down
	pop	bx			;Get executor map
	call	NextErrorCommon
	pop	bp
	jmp	[ScanRet]


SsProc	Next,Rude
;Frame stack for easy reference
	push	bp
	mov	bp,sp
	push	ax			;Save executor map

;Get For stack entry
	mov	cx,10			;bind EXIT For past IdRf executor and
					;Next executor
	call	BindExitFor		;Find For entry on stack, binding EXITs
	jz	NextWOForErrNoFrame	;For entry not found - error

;Emit IdRf
	mov	bx,[bp+2].FFor_oTxIdRf	;Get the IdRf executor address
	mov	ax,PTRTX[bx-4]		;Load the IdRf executor
	mov	cx,PTRTX[bx-2]		;Load the IdRf operand
	mov	bx,di			;Insert at emit oTx
	call	Insert1Op
	mov	al,1			;Set flag that no IdRf is present
	jc	NextCom
	dec	al			;Success, so zero al

;Calculate and emit the executor
;	bp+2 = pointer to For frame
;	al = 1 if Next with no Id and insertion of Id failed, else 0
;
;Uses:
;	type from the For frame
;	flags from For frame to distinguish between
;		<Step|> 	from scan stack entry and type
;	map on top of stack (distinguishes between exStNext and exStNextId)
NextCom:
	pop	bx			;Executor map
	push	ax
	mov	dx,[bp+2].FFor_oTyp	;For/Next type
	dec	dx			;Zero relative for indexing
	shl	dx,1			;To word offset
	test	[bp+2].FFor_Id,STYP_Step	;<Step|> variant differentiation
	jz	NextGotStepInfo 	;Not Step
	inc	dx			;Offset compensation for Step variants
NextGotStepInfo:
	shl	dx,1
	add	bx,dx			;bx = cs relative Next executor offset
	mov	ax,word ptr cs:[bx]	;ax = executor
	STOSWTX 			;Emit the executor

;Emit the frame offset for this Next
	mov	bx,[bp+2].FFor_oTx	;For oTx operand address
	mov	ax,PTRTX[bx-2]		;Get oBP from For
	cmp	ax,-1			;Valid?
	jnz	SetOBp
;See if Next has a valid oBP
	mov	cx,[bp+2].FFor_Id	;Step flag
	mov	dx,[bp+2].FFor_oTyp	;   and oTyp needed to allocate a block
	mov	ax,PTRTX[si]		;Get Next oBP
	inc	ax			;Valid?
	jz	NewForBlock
	dec	ax			;Restored oBP
	cmp	dl,es:[si+2]		;Has For type changed?
	jnz	NewForBlock
	cmp	dx,ET_I2		;I2 may or may not have step
	jnz	SetOBp
.errnz	HIGH STYP_Step			;Verify Step flag in low byte
	mov	ch,cl			;Copy Step flag
	xor	ch,es:[si+3]		;Step flag match?
	test	ch,STYP_Step
	jz	SetOBp
NewForBlock:
	call	ForBlockAlloc
;New For block means can't continue if it's in an active procedure or module
	push	ax
	push	bx
	mov	bx,dataOffset b$CurFrame
	PUSH_ES 			
	cCall	ActiveORs_Frame,<bx>	; See if frame on stack
	POP_ES
	or	ax,ax			
	jnz	Active			; brif frame is (probably) active

	mov	ax,[grs.GRS_oRsCONT]	
	cmp	ax,[grs.GRS_oRsCur]	; Is current one active?
	jnz	StillCont
Active:
	or	[SsFlags],SSF_CantCont	;Call CantCont at end of scan
StillCont:
	pop	bx
	pop	ax
SetOBp:
	STOSWTX 			;Set For block oBP in Next
	mov	PTRTX[bx-2],ax		;Set oBP in For

;Link For to Next and Next to For
	mov	PTRTX[bx],di		;Link For to Next oTx operand address
	mov	ax,bx
	inc	ax
	inc	ax			;Move to address beyond For operand
	pop	bx			;Get IdRf flag
	or	al,bl			;Set bit 0 if no IdRf was inserted
	STOSWTX 			;Link Next to executor after For
	add	si,4			;Skip source pointer over operands

Unframe:
;Now unframe the stack, pop the For frame, and exit
	pop	bp
	add	sp,SIZE FFor		;Size of For stack frame entry
	jmp	[ScanRet]		; and back to main loop

subttl	Next Opcode to Executor maps
page
public	mStNextOpExe
mStNextOpExe:
	DWEXT exStNextI2
	DWEXT exStNextStepI2
	DWEXT exStNextStepI4
	DWEXT exStNextStepI4
	DWEXT exStNextStepR4
	DWEXT exStNextStepR4
	DWEXT exStNextStepR8
	DWEXT exStNextStepR8

public	mStNextIdOpExe
mStNextIdOpExe:
	DWEXT exStNextIdI2
	DWEXT exStNextIdStepI2
	DWEXT exStNextIdStepI4
	DWEXT exStNextIdStepI4
	DWEXT exStNextIdStepR4
	DWEXT exStNextIdStepR4
	DWEXT exStNextIdStepR8
	DWEXT exStNextIdStepR8

subttl	EXIT For Support
page
;***
;Ss_Exit
;Purpose:
;	Scan EXIT For and EXIT DO.
;
;	These cases are handled by building a linked list of EXIT
;	entries in the associated For or DO stack frame.  These
;	entries will be bound at Next / DO time, when the opcode
;	that closes the block is bound.  For example, EXIT For is
;	bound at Next.
;
;	Ss_Exit ensures that there is a stack entry to match the
;	current block type that is reachable from the context of the
;	EXIT.  This requires walking frames back on the stack until
;	The appropriate control structure is found, or until the end
;	of the stack is encountered.  There is no stack entry type
;	that would cause the search to stop other than finding the
;	base of the scan stack.
;
;	The rule table index byte contains the bits for the current
;	EXIT structure type.
;
;	The mpOpExe table word carries the executor for the EXIT.
;
;	There is no other required context.
;
;Input:
;	Standard scanner dispatch.
;Output:
;	Standard scanner exit.
;***************************************************************************

;The following is an error as the bit must be in the specified byte
; as placed in PEROPCOD.TXT
.erre	low STYP_Step

SsProc	Exit
;Fetch EXIT type
	STOSWTX 			;Emit the executor
	LODSWTX 			;Skip over operand in source
	shr	bx,1			;bx = opcode (byte offset to mpOpRule)
	xor	ax,ax
	mov	ah,mpOpRule[bx] 	;Load rule byte for this For
					;ax now has correct value for STYP_For
	call	FindFrame		;Find frame type ax
	xchg	ax,cx			;cx = frame type
	jnz	ScopeOK 		;Frame type found
	mov	ax,MSG_ExitDo		;assume it's a DO frame
	cmp	cx,STYP_Do		;is it a DO?
	jz	SsExitErr		;brif so, issue error
	mov	ax,MSG_ExitScope	;EXIT not within For/Next
SsExitErr:
	call	SsError
ScopeOK:
;assert that Exit chains are at same frame offset for For and DO
.errnz	FFor_oTxExit - FDO_oTxExit

	mov	ax,[bx].FFor_oTxExit	;link this exit into the Exit chain
	mov	[bx].FFor_oTxExit,di	;new start of list is this EXIT For
	STOSWTX 			;store previous start in pcode.
	jmp	[ScanRet]		; and on to next opcode

page
;BindExitFor - bind stack entries back to For
;Purpose:
;	Look at the last scan stack frame to determine if it is a For.
;	If not a For, then a nesting error has occurred.
;
;	If a For entry is found, then bind the EXIT For list to the
;	pcode location of the opcode after the current Next.
;
;Input:
;	bp = frame of For entry (if present)
;	cx = offset from current emit address (di) for end of this Next
;
;Output:
;	PSW.Z if For block not found
;
;Preserves:
;	ax,dx

BindExitFor:
	push	ax
	test	[bp+2].FFor_Id,STYP_For ;Is it a For entry?
	jz	BindNoForErr		;No For found

;Bind EXIT For
	add	cx,di			;Address of opcode past Next
	mov	bx,[bp+2].FFor_oTxExit	;Load head pointer of EXIT list
	call	BindExit		;Jmp to common code to bind Exit chains
	or	sp,sp			;PSW.NZ
BindNoForErr:
	pop	ax
	ret

sEnd	SCAN
end
