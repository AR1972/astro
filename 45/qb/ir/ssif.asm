page	49,132
TITLE	ssif	- scan support for IF related opcodes
;***
;ssif.asm
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Scan IF statement opcodes.
;
;   Runtime behavior of IF-THEN-ELSE opcodes:
;   ----------------------------------------
;      <exp> opStIf(oText) 	    - branch to oText if exp is zero (false)
;      <exp> opStIfLab(label) 	    - branch to label if exp is non-zero (true)
;      <exp> opStIfLabDirect(label) - branch to label if exp is non-zero (true)
;      opStElse(oText) 		    - unconditionally branch to oText
;      opStElseLab(label) 	    - unconditionally branch to label
;      opStElseLabDirect(label)     - unconditionally branch to label
;      opStElseNop 		    - nop
;      <exp> opStIfBlock(oText)     - branch to oText if exp is zero (false)
;      <exp> opStElseIf(oText)      - branch to oText if exp is zero (false)
;      opStEndIfBlock 		    - nop
;
;   NOTE: When in direct mode, Parser emits opStIfLabDirect instead of
;         opStIfLab, and opStElseLabDirect instead of opStElseLab.
;
;   Single line IF statement syntax to pcode mappings:
;   -------------------------------------------------
;
;      Syntax:  IF <exp> GOTO <label>
;   
;      Pcode:   <exp> opStIfLab(label)
;
;      ============================================================
;      Syntax:  IF <exp> THEN <stmt list>
;   
;                            +--------------+
;      Pcode:   <exp> opStIf(|) <stmt list> | opBol
;   
;      ============================================================
;      Syntax:  IF <exp> THEN <label> ELSE <label>
;   
;      Pcode:   <exp> opStIfLab(label) opStElseNop opStElseLab(label)
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
;                            +-------------------------+
;      Pcode:   <exp> opStIf(|) <stmt list> opStElse(|)|opStElseLab(label)|opBol
;                                                    +--------------------+
;
;      ============================================================
;      Syntax:  IF <exp> THEN <stmt list> ELSE <stmt list>
;   
;                            +--------------------------+
;      Pcode:   <exp> opStIf(|) <stmt list> opStElse(|) | <stmt list> | opBol
;                                                    +----------------+
;   
;   Block IF statement syntax to pcode mappings:
;   -------------------------------------------
;      Syntax:  IF <exp> THEN
;   
;      Pcode:	<exp> opStIfBlock(oText to <exp> before ELSEIF, or oText
;		      beyond ELSE/END IF opcode)
;   
;      ============================================================
;      Syntax:  ELSEIF <exp> THEN
;   
;      Pcode:   opBol <exp> opStElseIf(oTx beyond ELSEIF/ELSE/END IF)
;
;			       +- to beyond END IF	
;      Bound:   opBol exBranch(|) <exp> opStElseIf(oTx past ELSEIF/ELSE/END IF) 
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
;	The scan routines within this module depend on the BOL scan routine
;	to bind unbound line IF and ELSE operands.  The BOL scan routine
;	binds all IF/ELSE frame entries until it discovers a Block IF/ELSE
;	entry, some other control structure frame that can't be bound, or
;	the bottom of the stack.
;
;	For speed, the IF and ELSE scan routines set a flag to let BOL know
;	that a line version of IF or ELSE has been pushed.  The flag is
;	cleared by BOL after binding all line IF and ELSE entries.
;
;	The IF and ELSE scan routines will not push a Block frame on the stack 
;	when there is a Line frame already on the stack.
;	
;
;****************************************************************************

	.xlist
	include		version.inc
SSIF_ASM = ON
	IncludeOnce	qbimsgs
	IncludeOnce	opmin		
	IncludeOnce	ssint
	IncludeOnce	variable
	.list

assumes ds, DATA
assumes es, NOTHING
assumes ss, DATA
assumes cs, SCAN

sBegin	SCAN
subttl	Static data area definitons.
page
;***
;Ss_If, Ss_IfBlock
;Purpose:
;	Scan entries for IF.
;
;	Scan tasks for IF include:
;	- ensuring the entry type is a fundamental, non string data type.
;	- selecting the IF executor varient for the argument data type.
;	- binding the label of opStIfLab<Direct|>
;	- pushing an IF frame on the scan stack as follows:
;		push  UNDEFINED for start of block IF exBranch chain
;		push  oTx of IF operand
;		push  IF frame label (identifying Standard, Block, or Label IF)
;
;Input:
;	Standard scan entrypoint
;Output:
;	Standard scan exit
;***************************************************************************
SsProc	IfLab
	mov	dx,STYP_IfLab		;Type of stack entry for IF w/ a label
	jmp	short SsIfCom

SsProc	IfBlock
	mov	dx,STYP_IfBlock		;Type of stack entry for block IF
	jmp	short SsIfCom		;Common IF code

SsProc	If
	mov	dx,STYP_IF		;Push an IF entry on the stack
SsIfCom:
	or	SsFlags,SSF_If		;Set end of line IF handling flag
	pop	ax			;Get type of last expression
	pop	cx			;Remove expression address
	cCall	MapOpToExeNumeric	;Type explode the executor
	STOSWTX				;Emit the executor
	PUSHI	ax,UNDEFINED		;Push end of Block IF ExBranch chain
	push	di			;Push operand address
	push	dx			;Push frame type
	test	dx,STYP_Lab		;Is this an IF w/ a label 
	jnz	SsIfLab 		;Brif yes
	MOVSWTX				;Skip the operand in source and dest.
SsIfComX:
	mov	bx,sp			
	sub	bx,2			
	mov	[SsBosStack],bx 	; Reset BOS SP mark for 1 Line If
	mov	[SsOTxStart],di 	; Reset DOS oTx mark for 1 Line If
	jmp	[ScanRet]


SsIfLab:				
	mov	ax,PTRTX[si+2]		
	sub	ax,opBos		; Is this an opBos?
	jz	@F			; Brif yes
	dec	ax			; Is this an opBosSp
	jne	ElsePresent		; Brif yes.	Works ok!
@@:					
	push	[ScanRet]		; Save exit address
	mov	[ScanRet],scanOFFSET BindIfLab	; We need to clean up

ElsePresent:				
	;Bind the label for this If

	mov	cx,1			;need to bind 1 label
	jmp	LabelBindCur		;Bind label to current scope and return to
					;to main scan loop

BindIfLab:				
	pop	[ScanRet]		; Restore loop address
	mov	bx,di			; Insert at current location
	call	InsertBranch		; Insert exBranch.  CY set if error.
	jc	SsIfComX		; Brif error
	mov	bx,sp			; Get stack frame pointer
	lea	ax,[di-2]		; oTx of exBranch operand
	mov	[bx].FIF_Id,STYP_Else	; Allow BOL update
	mov	[bx].FIF_oTx,ax 	; Update in frame
	jmp	SsIfComX		; Exit

;***
;Ss_Else,Ss_ElseNop
;Purpose
;	Scan ELSE varients.
;
;	opElseLab<|Direct> requires label binding only.  These are always
;	immediately preceeded by an opStElseNop or opStElse.  Therefore,
;	opElseLab<|Direct> does not need to examine or push a stack entry.
;	So, opElseLab<|Direct> are scanned as label reference opcodes like
;	GOTO.
;
;	Scan tasks for other Else variants are:
;	- emit executor
;	- walk stack frames to find first non-Else/non-ElseNop frame
;	- check for if frame, check errors:
;	  + IF nesting error if frame is not an IF
;	- change IF stack frame to ELSE stack frame
;	  + frame type is Block if IF was a Block type
;	  + set ELSE variant frame type
;	  + opStElseNop just changes frame to Set STYP_Lab bit -
;	    it requires no further binding.
;	  + leave address of exBranch chain, will be bound at ENDIF time
;	  + set address of ELSE operand
;	- bind IF to address of ELSE if it was not a label varient of IF
;
;	Scan tasks for ELSEIF are:
;	- ensure the entry type is a fundamental, non string data type.
;	- select and emit the ELSEIF executor varient for the argument 
;	  data type.
;	- insert an exBranch at BOS for exit from IF clause.
;	- check for an IF frame, check errors:
;	  + IF nesting error if top frame is not an IF
;	  + BLOCK IF error if IF is not a BLOCK IF.
;	- insert new exBranch into chain
;	- change Block IF frame to an ELSEIF frame on the scan stack as follows:
;		set  new start of exBranch chain
;		set  oTx of ELSEIF operand
;		leave IF frame id so next ELSEIF sees a BLOCK IF entry
;
;Input:
;	Standard scanner convention
;Output:
;	Standard scanner convention
;***************************************************************************
SsProc	ElseIf
	pop	ax			;Get type of last expression
	pop	dx			;Remove expression address
	cCall	MapOpToExeNumeric	;Type explode the executor
					; ax = executor
	STOSWTX				;Emit it

	mov	bx,sp			;point to stack frame
	mov	cx,[bx.FIF_oTxBranch]	;get next entry in ExBranch chain
	call	InsertBranchBos 	;insert exBranch after opBos
					;bx = oTx after exBranch operand
	jc	ElseIfScopeError	;Quit if OME
	xchg	ax,bx			;ax = oTx after exBranch operand
	mov	bx,sp			;point to stack frame
	cmp	[bx.FIF_Id],STYP_IfBlock ;ELSEIF must match block IF
	jnz	ElseIfScopeError	;brif nesting error
	dec	ax
	dec	ax			;point to exBranch operand
	mov	[bx.FIF_oTxBranch],ax	;set next link in exBranch chain
	mov	cx,[bx.FIF_oTx] 	;get if operand address
	mov	[bx.FIF_oTx],di 	;set addr of else operand
	xchg	ax,bx			;bx = ptr to txt
	MOVSWTX 			;skip operand in source and destination
	inc	bx
	inc	bx			;move past exBranch operand
	xchg	bx,cx			;bx = ptr to if operand, cx points past
					;inserted exbranch
	mov	PTRTX[bx],cx		;Bind if to ElseIf expression
SsElseIfX:
	jmp	short SsIfComX		

;Error processing for Else opcodes.

ElseIfScopeError:
	mov	cx,STYP_Else		;push an ELSE frame for ELSEIF error
ElseScopeError:
	mov	ax,MSG_ElseWI		;ELSE without IF error
	call	SsError
	cmp	cx,STYP_ElseNop 	;is this ElseNop?
	jz	SsElseIfX		;brif ElseNop - don't push a frame
	PushI	ax,UNDEFINED		;Flag no exbranch chain
	push	di			;push else operand location
	push	cx			;push else frame
	MOVSWTX 			;skip operand in source and dest
	or	[SsFlags],SSF_IF	;need BOS If processing to remove frame
ElseScopeErrX:
	jmp	short SsElseIfX 	;continue scanning

SsProc	ElseNop
	mov	cx,STYP_ElseNop 	;Signal no stack entry
	jmp	short SsElseCom

SsProc	Else
	mov	cx,STYP_Else		;Prepare to build ELSE frame
SsElseCom:
	STOSWTX				; and emit it
	mov	bx,sp			;set up Frame addressablity
	sub	bx,SIZE FIF		;Adjust for following add

; Skip all nested single line else's currently on scan stack. Walk
; back to first non-single else frame.	If this frame is not an IF/ELSEIF
; frame, then we have a scoping error. Multiple Single else frames can
; be on the stack if a nested single if occurs in a single line else clause.


SkipSingleElseLoop:
	add	bx,SIZE FIF		;point to next stack frame
	mov	ax,[bx.FIF_Id]		;ax = frame type
	cmp	ax,STYP_Else		;is this a single line else frame?
	jz	SkipSingleElseLoop	;brif so, examine next frame
	cmp	ax,STYP_ElseNop 	;is this a frame left from an if <lab>?
	jz	SkipSingleElseLoop	;brif so, examine next frame

; bx - points to first non-single else stack frame.
; ax - frame type of stack frame
; cx - else variant we are processing

	test	ax,STYP_If or STYP_Block
	jz	ElseScopeError		;Not an IF on the stack - scoping error.
	mov	dx,[bx.FIF_oTx] 	;dx = oTx of preceding if operand
	test	ax,STYP_Block		;is frame for BLOCK IF variant?
	jz	NotBlockElse		;brif not - single line variant
	or	cx,STYP_Block		;make this a block else

NotBlockElse:
DbAssertRel cx,nz,<STYP_IfBlock OR STYP_Lab>,SCAN,<Ss_ElseNop:saw opElseNop after BLOCK IF>
	or	SsFlags,SSF_IF		;we may need BOL If processing
	mov	[bx.FIF_Id],cx		;set frame as processed
	test	cx,STYP_Lab		;is this ElseNop?
	jnz	SsElseX 		;brif so, no binding needed
	mov	[bx.FIF_oTx],di 	;emit else op location for else binding
	MOVSWTX 			;skip operand in source and dest
	mov	bx,dx			;bx = If operand location
	mov	PTRTX[bx],di		;bind previous IF/ELSEIF to ELSE
SsElseX:
	jmp	SsIfComX		

EndIfScopeError:
	push	ax			;Restore stack state
	mov	ax,MSG_EWI		;ENDIF without block IF
	call	SsError
	jmp	short EndifX

SsProc	EndIf
	STOSWTX 			; and emit it
	pop	ax
	test	ax,STYP_Block		;Is it a Block type frame entry
	jz	EndIfScopeError		;Not a block IF or ELSE
	pop	bx			;Pop stack frame
	mov	PTRTX[bx],di		;Bind block IF or ELSE to this location
	pop	bx			;get ptr to start of exBranch chain
	call	BindExitCur		;call common EXIT chain binder
EndifX:
	jmp	[ScanRet]		; and continue

;MapOpToExeNumeric
;Purpose:
;	Type explode to executor for this opcode.
;
;	This routine works for full type explosion for numeric
;	data types only
;Input: 
;	ax = type from stack entry
;	bx = opcode * 2
;Output:
;       ax = executor
;Preserves:
;	cx,dx

	public	MapOpToExeNumeric
MapOpToExeNumeric:
	mov	bx,mpOpExe[bx]		;Load the exe map address
	.erre	ST_Typ_Mask EQ 0ffh	; Assure CBW is sufficient
	cbw				; Clear flags in scan stack
	.erre	ET_RC EQ 0		; Assure that DEC is sufficient
	dec	ax			; ET_RC -> 0FFFFh
	cmp	al,ET_MaxNum		; Detect user defined type
	jb	MapType 		;SD and record version not in language

	call	TMError 		
	.erre	ET_I2 EQ 1		; Assure XOR is sufficient
	xor	ax,ax			; Use I2 executor
MapType:
	add	bx,ax
	add	bx,ax			;Index to executor for this type
	mov	ax,word ptr cs:[bx]	; Load executor, zero relative
	ret

subttl	Opcode to executor maps for IF
page
public mStIfBlockOpExe
mStIfBlockOpExe:
	DWEXT	exStIfBlockI2
	DWEXT	exStIfBlockI4
	DWEXT	exStIfBlockR8
	DWEXT	exStIfBlockR8

public	mStIfLabDirectOpExe
mStIfLabDirectOpExe:
	DWEXT	exStIfLabDirectI2
	DWEXT	exStIfLabDirectI4
	DWEXT	exStIfLabDirectR8
	DWEXT	exStIfLabDirectR8

public	mStIfOpExe
mStIfOpExe:
	DWEXT	exStIfI2
	DWEXT	exStIfI4
	DWEXT	exStIfR8
	DWEXT	exStIfR8

public	mStIfLabOpExe
mStIfLabOpExe:
	DWEXT	exStIfLabI2
	DWEXT	exStIfLabI4
	DWEXT	exStIfLabR8
	DWEXT	exStIfLabR8

public	mStIfGotoLabOpExe
mStIfGotoLabOpExe:
	DWEXT	exStIfGotoLabI2
	DWEXT	exStIfGotoLabI4
	DWEXT	exStIfGotoLabR8
	DWEXT	exStIfGotoLabR8

public	mStElseIfOpExe
mStElseIfOpExe:
	DWEXT	exStElseIfI2
	DWEXT	exStElseIfI4
	DWEXT	exStElseIfR8
	DWEXT	exStElseIfR8

sEnd	SCAN
end
