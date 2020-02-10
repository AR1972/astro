page	49,132
	TITLE	ssbos - scan support for begin of statement opcodes
;***
;ssbos - scan support for begin of statement opcodes
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains scan dispatches for label definitions and
;   label reference opcodes.
;
;   Labels refer to line numbers or alpha-numeric labels.  There is
;   no difference notable in the scanner.
;
;   The scanner is responsible for the following tasks:
;
;	1. Reference scope.  For each label reference opcode thereis only one
;	   scope in which the label may be legally defined.  Scope checking is
;	   simply a matter of searching the reference chain in the appropriate
;	   link list.
;	   NOTE: The main level code must be scanned first.  This allows
;	   immediate binding without fixups whenever a procedure references
;	   a label at the main level.  The main level may never reference a
;	   procedure label.
;
;	2. Binding.  Label references are bound to the text table
;	   in SS_EXECUTE state only.  In other states, they are bound to the
;	   name table.
;	   Binding involves replacing the oName in the label reference with
;	   the oTx for the label.  References to the main level from within
;	   a procedure are never ambiguous, so no oPrs or other flag is needed.
;
;	   Backward references are bound simply by searching
;	   the label chain.
;
;	   Forward references are handled by linking the
;	   reference into the label chain at the definition point.  The
;	   backward pointer is identifiable when scanning the label definition
;	   opcodes, and the reference is bound when the definition is bound.
;
;	3. Debinding.  Debinding label references is done in a separate pass
;	   on the text at descan time.
;
;	4. Duplicate label detection. Duplicate labels are detected by the
;	   text manager.
;
;
;****************************************************************************

	.xlist
	include 	version.inc
SSBOS_ASM = ON
	IncludeOnce	context
	IncludeOnce	optables
	IncludeOnce	opcontrl
	IncludeOnce	opstmt
	IncludeOnce	pcode
	IncludeOnce	qbimsgs
	IncludeOnce	scanner
	IncludeOnce	ssint
	IncludeOnce	txtmgr
	IncludeOnce	ui
	IncludeOnce	variable
	.list

assumes es,nothing
assumes ds,DATA
assumes ss,DATA
assumes cs, SCAN

sBegin	DATA
	externW	pSsCOMcur		;defined in SsDeclar.asm
	oRsExpected DW	?		;expected oRs when binding labels
	fRestoreLab DB	0		;non-zero if binding RESTORE
sEnd	DATA

extrn	exNoList1:near
extrn	exStRestore1:near

extrn	exStDefFn:near


sBegin	SCAN

DWBOL	MACRO	cSpace
	DWEXT	exBol&cSpace
	endm

CBOL_EXECS EQU 25  ;number of opBol executors (for indentation 0..24)
mpBol	label	word
cSpace	=	0
	REPT	CBOL_EXECS
	DWBOL	%cSpace
cSpace	=	cSpace+1
	endm


;***
;Ss_BolLabDef,Ss_LabDef - scan Bos opcode varients
;Purpose:
;	Scan general begin of statement opcodes.
;	This task is simply opcodes to executors and copying any operands.
;
;	Update the begin of statement pointer, and reset scanner flags.
;
;	Update the pointer to the pcode following begin of statement.
;	This pointer is used to isolate DIMs for $STATIC arrays.  See the
;	comments in exarray.asm.
;
;	Scan label definition varients.
;
;	Label definition tasks include:
;	1. If link points to code on emit side then the link is to a label
;	   reference that has not yet been bound (forward reference problem).
;		bind ref at link side
;		pick up ref in link side item
;		goto 1 (there may be more than one forward ref to this label)
;	2. maintain the link list.
;
;	A link points to an opcode that has an operand which is a scanned
;	reference if the low bit is set.
;
;	Label links point to the opcode that has a link field of the next
;	label, or are UNDEFINED.  Label offsets to the unscanned source are
;	not relocated when text expands.
;
;	The label list control structure is in struc TXLNK at [SsLinkCtl].
;	It contains fields as follows:
;	TXLNK_LabDefNext - offset of next label definition
;			   (unrelocated for expansion)
;	TXLNK_LabDefLast - offset of last bound label definition
;
;Input:
;	ax	= opcode
;	bx	= opcode * 2
;	es:si	= source code address of operands (if any)
;	es:di	= destination code address.
;Output:
;	si/di	updated
;Modifies:
;	ax,bx,cx,dx modifies
;***********************************************************************
SsProc	LabDef
	mov	dx,di			;Preserve the emit address
	call	EmitExCopyOps		;Emit executor and operand(s)
	jmp	short LabDef		;Now handle the label definition

SsProc	BolLabDef,rude
	mov	dx,di			;Preserve the emit address
	call	Bos0_0Com		;Process standard BOS issues
LabDef:
	xchg	dx,di			;Preserve next emit address
					; and move to link field on emit side
	mov	cx,di			;Save oTx for forward reference linking
	inc	di			;Move to link field on emit side
	inc	di
	mov	bx,di
	mov	ax,PTRTX[bx]		;Load link operand of current label

;Reenter here if we fixed up some previous forward ref.
BosForwardRefChk:
	cmp	ax,UNDEFINED		;Test for end of link list
	jz	NotForwardRefFixup	;End of list - can't be a fixup
	test	ax,1			;Is a link to a forward reference?
	jz	NotForwardRefFixup	;This is not a previous forward ref.

;Fix up the previous forward ref
	dec	ax			;Remove link flag
	mov	bx,ax
	mov	ax,PTRTX[bx]		;Load link from referenced location
	mov	PTRTX[bx],cx		;Bind to beginning of BOL varient
	jmp	short BosForwardRefChk	;Check if this is another old forward

NotForwardRefFixup:
	mov	bx,[SsLinkCtl]		;Address of TXLNK control struc
	mov	[bx].TXLNK_LabDefNext,ax;oTx for next label before any
					; expansion is accounted for.
	mov	cx,di
	xchg	cx,[bx].TXLNK_LabDefLast;Address of last label link
	mov	bx,cx
	jcxz	BolFirst		;This is the first defined label.
					; as oTxLastLabDef starts at 0
	mov	PTRTX[bx],di		;Link last label to this one
BolFirstLabDefCont:			;Continue here after handling first
					; label definition
	or	al,1			;set low bit on emitted side to
					;specify end of Bound label chain
	mov	PTRTX[di],ax		;Emit link to next label def
	dec	di			;Back to BOL executor address
	dec	di
	jmp	short BolCom		;Handle general BOL considerations


;Here if this is the first label definition encountered.
BolFirst:
	mov	txdCur.TXD_otxLabLink,di;Update text descriptor to point to
					; the first label in the link chain
	jmp	short BolFirstLabDefCont;Continue label definition handling


;***
;Ss_Bos,Ss_Bol,SsEot,Ss_0_0 - Scan opcodes with no args, emit nothing, executor in mpOpExe
;Purpose:
;	Scan opcodes which:
;	1. have no arguments (consume nothing)
;	2. emit no value
;	3. have no special operand processing.
;	   (operand count is in mpOpAtr)
;	4. executor is in mpOpExe
;
;	For example, this includes:
;	opBos, opBol, opBolSp, opBolCont, opWatch, opInclude
;
;Input:
;	ax	= opcode
;	bx	= opcode * 2
;	es:si	= source code address of operands (if any)
;	es:di	= destination code address.
;Output:
;	si/di	updated
;Modifies:
;	ax,bx,cx,dx modified
;***********************************************************************
SsProc	Bos,rude
	push	[ScanRet]		;Set return address
	DJMP	jmp SHORT Bos0_0Com	;Continue through shared BOS code

SsProc	Eot,rude
	test	[SsFlags],SSF_ScanAndExec
	jnz	ScanExExitJ
	mov	[ScanRet],scanOFFSET ScanExit	;Terminate scan dispatching
	jmp	short SsBol			; after performing BOL work

ScanExExitJ:
	jmp	SsScanExExit

SsProc	BolEmit,rude
	jmp	short SsBol

SsProc	Bol,rude
	mov	al,es:[si-1]		;Get high byte of opcode
	and	ax,HIGH (not OPCODE_MASK) ;Get count of spaces * 2
	xchg	ax,bx			;Preserve opcode*2 in bx
.errnz	OPCODE_MASK - 3FFH
	shr	bx,1			;cSpace is shifted left one bit
DbAssertRel	bx,be,2*CBOL_EXECS,SCAN,<Ss_Bol: cSpace too large>
	mov	bx,[bx].mpBol		;Get appropriate executor
	xchg	ax,bx
SsBol:
	mov	dx,di			;Preserve BOL executor address
	call	Bos0_0Com		;Handle end of statement
	xchg	dx,di			;Back to BOL executor, preserving next emit addr
BolCom:
	mov	ax,[SsLineCount]
	inc	ax
	mov	[SsLineCount],ax
	test	al,LineUpdate-1	;Time to update line count on screen?
	jnz	NoLineUpdate
	push	dx
	cCall	UpdStatusLn,<ax>
	GETSEGTXTCUR		
	pop	dx
NoLineUpdate:
	test	SsFlags,SSF_If	;Is there special per line work?
	jnz	BolControlBind	
BolComX:
	mov	di,dx		;Back to next emit address
	jmp	[ScanRet]

;BolControlBind
;Purpose:
;	Bind control structure frames found on the stack.
;
;	This routine binds all non-block, non-label IF and ELSE entries
;	to this BOL.
;
;	Label varients of IF are discarded.  They were on the stack in order
;	to allow the scanner to correctly check IF/ELSE scoping.
;
;	The label and nop varients of ELSE cause no stack entry.
;
;	Block varients of ELSE and IF are popped by the matching block type
;	opcodes only.

BolDoBind:
	pop	bx		;Get IF operand address from frame
	mov	[SsBosStack],sp ; SP at BOS
	pop	cx		;throw away block if Brach chain

	test	ax,STYP_Lab	;Label IFs and ElseNops are popped, but don't require binding
	jnz	BolControlBind	;Label IF frame - go check next stack entry

	mov	PTRTX[bx],di	;Store address of this BOL

BolControlBind:
	pop	ax			;Get frame type
	testx	ax,STYP_If+STYP_Else	;Bind If and Else
	jz	BolControlBindX
	testx	ax,STYP_Block		;Don't bother with block If/Else
	jz	BolDoBind		;Not Block IF or ELSE - perform the binding
BolControlBindX:
	push	ax			;Replace frame type
	and	SsFlags,not SSF_If	;Clear IF flag
	jmp	short BolComX		;exit through BolCom


;Bos0_0Com
;Purpose:
;	Handle standard BOS issues
;
;	SsCbFrameTemp is the number of bytes needed for the statement
;	just scanned.  It is used to keep a high water mark for the
;	number of temps needed by any statement.  This count is zeroed
;	in preparation for the next statement.
;
;	If we CAN continue, then there may be oTx's in the stack, in
;	MrsCur, etc., that need to be updated to account for text
;	expansion during scan.  References to these oTexts have been
;	marked with opNoList1, whose operand points to the reference.
;	The scan routine for opNoList1 updates the reference to point to
;	the current emit address.  However, a subsequent insertion (for
;	coercion, for example) could cause the point being referenced to
;	move again.  For this reason, the opNoList1 is left in the pcode
;	(as exNoList1) and two flags are maintained in SsBosFlags.
;	If both SSBOSF_Inserted and SSBOSF_PcUpdate are true, then an
;	insertion might have moved an opNoList1.  This routine will search
;	for exNoList1 and re-patch the references.
;
;	If a COMMON statement was being scanned, then there is a frame
;	on the stack with the owners of the Type and Value tables.  These
;	owners are moved back to the COM structure in the COMMON block
;	table.
;
;NOTE: This routine is fallen into by Ss_BOS and is also called.
;Input:
;	Standard Scan entry convention.
;Preserves:
;	dx
Bos0_0Com:
	push	di			;Save current emit oTx
	call	EmitExCopyOps		;Handle 0 consume 0 emit issues

;Report delayed Argument Count Mismatch errors
	mov	ax,UNDEFINED
	xchg	ax,[SsDelayCnt]		;Get count, reset to -1
	inc	ax			;Any errors?
	jz	UpdateTemp
	push	si
	mov	si,[SsDelayLoc]		;Source oTx of error
	mov	ax,[SsDelayErr]
	call	SsError
	pop	si

UpdateTemp:
;Update count of temporaries
	xor	ax,ax
	xchg	ax,[SsCbFrameTemp]	;Get count of temps needed
	mov	bx,dataOFFSET prsCur.PRS_cbFrameTemp
	test	byte ptr [grs.GRS_oRsCur+1],80H	;MRS?
	jnz	CheckTemps
	mov	bx,dataOFFSET mrsCur.MRS_cbFrameTemp
CheckTemps:
	cmp	ax,[bx]			;Using more temps?
	jb	FewerTemps
	mov	[bx],ax 		;Set new max temp count
FewerTemps:

;Now check for PC update
	test	[SsBosFlags],SSBOSF_Inserted + SSBOSF_PcUpdate
	jz	NoPcUpdate
	jpo	NoPcUpdate		;If only 1 set, no work
;Have inserted pcode on line with PC Update pcode
	push	dx
	mov	bx,[SsOTxBos]		;Start of previous line
UpdateLoop:
	mov	ax,codeOFFSET exNoList1 ;pc update executor
	call	SsFindOpNoList1
	jc	ExitPcUpdate
	mov	ax,PTRTX[bx-2]		;Get offset of reference
	xchg	ax,bx
	mov	[bx],ax			;Adjust to new location
	xchg	ax,bx
	jmp	UpdateLoop

ExitPcUpdate:
	pop	dx
NoPcUpdate:
	pop	ax			; AX = oTx of BOS in emitted code
	mov	[SsOTxBos],ax		; Save oTx of BOS
	mov	[SsOTxStart],di 	;New first location for DIM
	mov	bx,UNDEFINED		; No patch indicator
	xchg	bx,[SsOTxPatchBos]	; BX = Address to be patched ?
	.erre	UNDEFINED EQ 0ffffh	; Assure INC/JZ is sufficient
	inc	bx			; Any?
	jz	@F			; No
	mov	PTRTX[bx+1],ax		; Patch Bos address
@@:					
	test	[SsBosFlags],SSBOSF_StCommon	;Finishing up COMMON?
	jz	CheckCase
;Have COMMON entry on stack to clean up
	push	dx
	push	es
	mov	ax,[bp-SsCom].COM_oCom
	add	ax,[grs.GRS_bdtComBlk.BD_pb] ;oCommon --> pCommon
	xchg	bx,ax			;pCommon to bx
	mov	ax,[bp-SsCom].COM_oValCur
	mov	[bx].COM_oValCur,ax
	mov	ax,[bp-SsCom].COM_oTypCur
	mov	[bx].COM_oTypCur,ax
.errnz	SsCom - COM_bdType
	add	bx,COM_bdType
	push	bp			;Current owner
	push	bx			;New owner
	add	bx,COM_bdValue - COM_bdType
	cmp	[bp-SsCom].COM_bdValue.BD_cbPhysical,UNDEFINED ;User Library?
	jz	CopyTypOwner
	lea	ax,[bp-SsCom].COM_bdValue
	push	ax			;Current owner
	push	bx			;New owner
	call	BdChgOwner		;Copy BD back to COMMON table
CopyTypOwner:
	call	BdChgOwner
	mov	[pSsCOMcur],0		;reset to default
	pop	es
	pop	dx
	pop	cx			;Return address
	add	sp,SsComSize+4		; Eat oCommon and cbFixed
	pop	bp
	push	cx			;Return address back
CheckCase:

;
;	Binds CASE true branches to BOS.  The line may only consist of
;	constant expressions and case executors, thus the CASE frame
;	must be on the top of the scan stack.  If this is not the case,
;	it was the result of a CASE without SELECT error which is detected
;	by Ss_Case.

	test	[SsBosFlags],SSBOSF_StCase ;Need to bind TRUE Case branch?
	jz	ResetFlags

	pop	cx			;pop return address
	pop	ax			;get frame type
	test	ax,STYP_Case		;is this a CASE frame?
	push	ax
	push	cx
	jz	ResetFlags		;brif not, must have been an error (CASE w/o SELECT)

	mov	bx,sp			;get ptr to CASE frame
	inc	bx
	inc	bx			;skip return address
	mov	ax,UNDEFINED		;reset start of TRUE branch chain
	xchg	ax,[bx].FCASE_oTxTrue	;get ptr to start of TRUE branch chain
	xchg	ax,bx			;bx = start of TRUE branch chain
	mov	cx,[SsoTxBos]		;bind to start of BOS
	call	BindExit		;bind the chain

ResetFlags:
	mov	SsBosFlags,0		;Reset statement flags
	mov	[SsBosStack],sp 	; SP at BOS
	ret

SsProc	OptionBase1
	or	[mrsCur].MRS_flags,FM_OptionBase1
	jmp	short CheckOption

SsProc	OptionBase0			;Already set by default
CheckOption:
	test	[SsFlags],SSF_HaveDimmed;DIM already occured?
	jz	SetDimmed		;If not, it's OK
	xchg	cx,ax			;Save executor in cx
	mov	ax,MSG_OBA		;Array already dimensioned
	jmp	short NestError

SetDimmed:
	or	[SsFlags],SSF_HaveDimmed;Don't allow another OPTION BASE
	jmp	short NotInProc


SsProc	Shared,rude
	jmp	short NotInProc

SsProc	StShared,rude
	or	SsBosFlags,SSBOSF_StShared
	jmp	short Ss_StDim		

SsProc	StStatic,rude
	or	SsBosFlags,SSBOSF_StStatic
	jmp	short Ss_StDim		


SsProc	StDim,rude

	mov	[SsOTxPatchBos],di	; Patch this with next Bos address
	add	di,4			; Address after this opcode
	mov	[SsOTxStart],di 	; New first location for DIM
	sub	di,4			; Restore current emit address
	jmp	short Ss_0_0


SsProc	NotInProc
NotInProc:
	test	byte ptr [grs.GRS_oRsCur+1],80H	;In a procedure?
	jz	Ss_0_0			;If not, it's OK
	xchg	cx,ax			;Save executor in cx
	mov	ax,MSG_InvProc		;Illegal in procedure
NestError:
	call	SsError
NestCont:
	xchg	ax,cx			;Restore executor to ax
	jmp	short Ss_0_0

SsProc	ElemRef
	test	[SsFlags],SSF_InType	;In a TYPE declaration?
	jnz	Ss_0_0			;If so, it' OK
	xchg	cx,ax			;Save executor in cx
	mov	ax,MSG_InType
	jmp	short NestError


SsProc	StConst,rude

	mov	cl,[SsExecFlag]
	mov	[SsExecTmp],cl		    ;Save current state of OPA_fExecute
	or	[SsBosFlags],SSBOSF_Const   ;Flag that we're in a CONST statement
	jmp	short Ss_0_0

SsProc	AsType,rude
	jmp	short Ss_0_0


SsProc	Static
	mov	f_Static,TRUE
	jmp	short Ss_0_0

SsProc	Dynamic
	mov	f_Static,FALSE
	jmp	short Ss_0_0

SsProc	0_0
	push	[ScanRet]	;Push address of main scan loop
				;And fall into EmitExCopyOps to handle standard
				; 0 consume 0 emit issues
;EmitExCopyOps - Emit executor and copy operands
;Purpose:
;	Emit the executor for the current opcode.
;	Copy all operands from source to destination.
;
;NOTE:	SsProc 0_0 falls into this code.
;
;Input:
;	ax	 = executor
;	bx	 = opcode * 2
;	es:si/di = scan source and destination
;Output:
;	bx    = opcode
;	si/di	updated
;
;Preserves:
;	dx
Public	EmitExCopyOps
EmitExCopyOps:
	STOSWTX 			;Emit the executor
;	jmp	short CopyOperands	;Fall into CopyOperands

;***
;CopyOperands
;Purpose:
;	Copy the operands for opcode in bx from si to di.
;
;	This routine handles the following special cases:
;	- no operands for this opcode
;	- operand count is the first operand
;
;NOTE:	EmitExCopyOps falls into this code.
;
;Input:
;	bx = opcode * 2
;	si = source of copy
;	di = destination
;	es = segment of copy
;
;Output:
;	bx = opcode
;	si/di updated
;
;Preserves:
;	dx
;*****************************************************************
Public	CopyOperands
CopyOperands:
	shr	bx,1			;Back to opcode
	mov	cl,mpOpAtr.[bx] 	;Load atribute byte
	and	cx,OPA_CntMask		;Get the operand count from atribute
.errnz	OPA_CntMask AND 0FF00H		;must use cx in next line if non-zero
	cmp	cl,OPA_CntMask		;Check for cnt field in operand
	jne	CopyOp	 		;No cnt field
	LODSWTX 			;Load the cnt field
	STOSWTX 			;Emit the byte cnt field
	mov	cx,ax
	inc	cx			;Round to even byte count
CopyOp:
	shr	cx,1			;Move to word count
	cli				;Double prefix! No interrupts!
rep	movs	PTRTX[si],PTRTX[di]	;Copy the operands
	sti
	ret


subttl	Label Reference Scanning
page
;***
;Ss_MrsMrsLabRef - scan dispatch for RESUME/RETURN <line/label>
;Purpose:
;	Scope and bind RESUME/RETURN <line/label> reference to definition.
;	Check to ensure that the RESUME/RETURN statement was at the module
;	level.	If not, issue an Illegal in PROC or DEF FN error.
;	bind the RESUME/RETURN to a module level label.  If the label
;	definition is in a DEF FN, SUB, or FUNCTION, issue a
;	scoping error.
;Entry:
;	standard scan entry
;Exit:
;	standard scan exit
;Exceptions:
;	Illegal in proc or DEF FN.
;	Label not defined.
;****************************************************************************


SsProc	MrsMrsLabRef
	STOSWTX 			;emit executor
	test	grs.GRS_oRsCur,8000H	;are we in DEF FN, SUB, or FUNCTION?
	jz	LabelBindMrs		;brif not, at main level
	mov	ax,MSG_InvProc		;Illegal in PROC or DEF FN
	call	SsError 		;remember error
	jmp	short LabelBindMrs	;bind to module level label


;***
;Ss_MrsLabelRef - scan dispatch for binding labels to module level
;Purpose:
;	Scanner entry point to scope and bind labels which must always
;	be bound to the module level.  The statements that get bound
;	here include ON event GOSUB <lab/line>, ON ERROR GOTO <lab/line>,
;	RESTORE <lab/line>, and RUN <lab/line>.
;
;	NOTE: RESTORE <lab/line> may bind within a DEF FN, or to the
;	Module level.
;
;	If the label definition is in a DEF FN, SUB, or FUNCTION, issue a
;	scoping error.
;Entry:
;	standard scan entry
;Exit:
;	standard scan exit
;Exceptions:
;	Label not defined.
;****************************************************************************
SsProc	MrsLabelRef
	STOSWTX 				;Emit executor
	    cmp     ax,CODEOffset exStRestore1	;Is this a RESTORE <lab/line>
	    jnz     LabelBindMrs		;Brif not
	mov	fRestoreLab,TRUE		;Set special RESTORE flag
;fall into LabelBindMrs

page
;LabelBindMrs, LabelBindMrsCx - bind label refs to module level
;Purpose:
;	Binds one(LabelBindMrs), or more (LabelBindMrsCx) label
;	references to module level label defs.	Scoping errors
;	and undefined label refs are checked.
;Entry:
;	cx - count of labels to bind
;	es:si - start of label oNam list to bind.
;	es:di - emit address for bound label oTx.
;Exit:
;	source and emit addresses advances appropriately.
;Exceptions:
;	Label not defined.


;public  LabelBindMrs			 ;Entry point to bind to a module level
LabelBindMrs:				;label
	mov	cx,1			;will bind 1 label

;public  LabelBindMrsCx 		 ;Entry point to bind a list of label
LabelBindMrsCx: 			;refs to module level label defs
	mov	ax,grs.GRS_oMrsCur	;scope it to MODULE level
	GETSEG	dx,[mrsCur.MRS_txd.TXD_bdlText_seg],,<SIZE,LOAD> ;[3] get module text table
	mov	bx,mrsCur.MRS_txd.TXD_otxLabLink  ;get module label chain

	test	txdCur.TXD_flags,FTX_mrs ;are we in a module level text table?
	jz	LabelBind		;brif not, use module text table
	jmp	short LabelBindCom	;bind to current text table

page
;***
;Ss_nLabelRef - scan dispatch for binding a list of labels to same scope
;Purpose:
;	Scanner entry point to scope and bind labels which must always
;	be bound to the same scoping level. This scanner dispatch
;	handles a list of labels.  Statemtents that are bound by this
;	routine include ON <exp> GOSUB <lab/line, ...>, and
;	ON <exp> GOTO <lab/line, ...>.
;
;	If the label definition is not at the same scoping level, (e.g.
;	into or out of DEF FN) then a scoping error is generated.
;Entry:
;	standard scan entry
;Exit:
;	standard scan exit
;Exceptions:
;	Label not defined.
;****************************************************************************
SsProc	nLabelRef
	STOSWTX 			;emit executor
	mov	ax,ET_I2		;enumerated GOTO, GOSUB must have I2 arg
	call	EnsureArgType		;coerce to I2 if necessary
	LODSWTX 			;get operand byte count
	STOSWTX 			;and emit it
	shr	ax,1			;byte => word count
	xchg	ax,cx			;set up label count
	jmp	short LabelBindCur

;***
;Ss_LabelRef - scan dispatch for binding a labels to the same scope
;Purpose:
;	Scanner entry point to scope and bind labels which must always
;	be bound to the same scoping level. This scanner dispatch
;	handles a list of labels.  Statemtents that are bound by this
;	routine include GOSUB <lab/line, ...>, GOTO <lab/line>, and
;	RETURN <lab/line>
;
;	If the label definition is not at the same scoping level, (e.g.
;	into or out of DEF FN) then a scoping error is generated.
;Entry:
;	standard scan entry
;Exit:
;	standard scan exit
;Exceptions:
;	Label not defined.
;****************************************************************************
SsProc	LabelRef
	STOSWTX 			;emit executor
	mov	cx,1			;will bind one label
;fall into LabelBindCur

page
;LabelBindCur, LabelBindCom, LabelBind - label binders
;Purpose:
;	Label binder routines for the various styles of label
;	binding.
;
;	LabelBindCur - binds the label ref and def to the same
;	    scope, using the current text table.
;	LabelBindCom - binds the label ref to a label def in
;	    the current text table.  The scoping rule for the
;	    the target label has already been determined.
;	LabelBind - binds the label ref to the specified text
;	    table, with the specified target scoping rule.
;
;	The label ref scope is defined by grs.GRS_oRsCur, which defines
;	the oRs for the Module, DEF FN, SUB, or FUNCTION of the label
;	ref.  If we are scanning the direct mode buffer, then the
;	current CONTINUE context is used to define the label ref scope.
;
;	The label def scope is the expected oRs of the actual
;	definition.  This may, or may not be within the current text
;	table.	The label def scope is kept in the variable oRsExpected.
;	If bit 0 of oRsExpected is set then the label def (if found)
;	was in a SUB or FUNCTION.  If the label was found and was
;	in a module level text table, oRsExpected is compared against
;	the oRs of the label def to ensure that illegal binding
;	across DEF FN boundaries has not occurred.
;
;	The actual binding occurs as follows:
;
;	- If the oRsExpected text table is the same as the current emit
;	  text table, then the label links are searched from the scanner
;	  source address to the end of the pcode.
;
;	  If found in this range, link the emit address into the label list.
;	  This label reference will be bound at label definition time.
;	  The low bit of the label definition link is set to indicate a link
;	  in the label definition list is pointing to an unbound reference.
;
;	- If not found in the step above, or if the text table we are
;	  searching is not the emit text table, then search from the
;	  root of the label chain in the appropriate text table for a
;	  definition of the referenced label.
;
;	  If the search text table is the same as the emit text table,
;	  the search terminates at the scanner emit address.
;
;	  If the label is found in this search, bind the label.  Not finding
;	  the label is declared as an error.
;
;	RESUME 0 , ON ERROR GOTO 0, and ON <event> GOSUB 0 have the special
;	label oNam UNDEFINED.  This label is emitted as UNDEFINED.
;
;
;Entry:
;	ax - oRs of expected scope (LabelBindCom, LabelBind)
;	bx - oTx of label chain start (LabelBind)
;	cx - count of labels to bind
;	dx - test seg containing label chain (LabelBind)
;	es:si - start of label oNam list to bind.
;	es:di - emit address for bound label oTx.
;Exit:
;	source and emit addresses advances appropriately.
;Exceptions:
;	Label not defined.

; CX = count of labels to bind

public	LabelBindCur			;Entry point to bind a label to
LabelBindCur:				;current context
	mov	ax,grs.GRS_oRsCur	;expected same scope

	    test    txdCur.TXD_flags,FTX_mrs	;are we in a module text table?
	    jnz     LabelBindCom		;brif so

	or	al,1			;set low bit to indicate that we are
					;binding to SUB or FUNCTION


;	AX = oRs of expected scope (low bit set if binding TO a SUB/FUNCTION)
;	CX = count of labels to bind

;	Common entry for LabelBindMrs

LabelBindCom:
	GETSEG	dx,[txdCur.TXD_bdlText_seg],,<SIZE,LOAD> ;[3] set text table to current
					; text tbl in case we are already 
					; at module level
	mov	bx,txdCur.TXD_otxLabLink ;get cur label link chain


;At this point, DX = text table to bind to, BX = start of label def chain
;		AX = oRs of expected scope, CX = count of labels to bind
;		ES:SI = pcode addr of oNam/ref label ref list to be bound

;	Common entry for LabelBindMrs

LabelBind:
	mov	oRsExpected,ax		;remember expected scope
LabelBindLoop:
	push	cx			;save label count
	LODSWTX 			;get label oNam
	cmp	[SsErr],0		;Any errors so far?
	jne	@F			;If so,don't bind labels
	cmp	ax,0fffeh		; Is this RESUME 0/NEXT or ON ERROR/EVENT
	jae	@F			;GOTO/GOSUB 0? skip bind if so.
	push	bx			;remember start of label chain
	call	LabelSearch		;Search for label in specified text table
	pop	bx
@@:
	STOSWTX 			;bind label/or emit oNam
	pop	cx			;restore label count
	loop	LabelBindLoop
	mov	fRestoreLab,cl		;reset Binding RESTORE flag
	jmp	[ScanRet]		;return to main scan loop

page

;LabelSearch - search for label definition
;
;Purpose:
;	This routine searches for labels in the specified context.
;	The label definition will either be at the same scope as
;	the label reference, or at the module level.  If this is not
;	the case, either an Undefined Label, or Subprogram error
;	will be generated.  If the direct mode buffer is being bound,
;	then the current Continue context will be used to determine
;	scoping errors.
;Entry:
;	 oRsExpected = oRs of expected scope.
;	 ax  = oNam of label to be found
;	 bx  = start of label def chain
;	 dx  = Text table seg to search.
;	 es:si	= text table addr of label ref
;	 es:di	= text table emit addr
;Exit:
;	 ax  = oTx of found label, or oNam if not found.
;Preserves:
;	 dx

LabelSearch:
	push	ds
	mov	cx,es
	cmp	cx,dx			;are we searching in current table?
	jnz	LabelNotCurTxt		;brif not, table already bound

;Searching for a label definition in same txt table as label reference.

	mov	cx,[SscbTxExpand] 	;get table expansion factor
	mov	bx,[SsLinkCtl]		;address of label control structure
	mov	bx,[bx].TXLNK_LabDefNext ;Next unbound label reference
	mov	ds,dx			;set ds to txt table of search
	jmp	short LabSearchSrc	;Search forward through unbound source

SkipRef:
	dec	bx			;Reset LSB of reference
NextLink:
	mov	bx,[bx]			;Get link to next
LabSearchSrc:
	cmp	bx,-1			;End of chain?
	jz	LabelNotForwardRef	;Label not found - go look in already
					; bound section of current text table
	test	bl,1			;Just a forward reference?
	jnz	SkipRef
	add	bx,cx			;Adjust by SsCbTxExpand
	cmp	ax,[bx+2]		;oNames match?
	jnz	NextLink

	mov	ax,bx
;Forward Reference Case
	pop	ds			;recover dgroup
	push	bx			;preserve ptr to def
	call	CheckLabelScope 	;check for scoping errors before
					;linking into forward ref chain
	pop	bx			;recover ptr to def
	jnz	LabelSearchX		;brif invalid scope

	push	ds
	mov	ds,dx			;set ds to label def txt table
	mov	cx,[bx] 		;Load link field from definition
	mov	ax,di
	inc	ax			;Mark as forward reference link
	mov	[bx],ax 		;Emit forward reference link to current
					; label reference
	xchg	ax,cx			;AX = pcode address of next
					; unbound label definition offset
	pop	ds			;recover DGroup

LabelSearchX:				;finished was a forward ref
	ret

LabelNotForwardRef:
;Search the section of the current text table that is already bound.
assumes	ds,nothing
	mov	bx,txdCur.TXD_otxLabLink;Start at first label
LabelNotCurTxt:
	mov	ds,dx			;search in specified table
	jmp	short LabSearchBound	;Search from start through bound source

BoundLoop:
	cmp	ax,[bx+2]		;oNames match?
	jz	LabFound
	mov	bx,[bx]			;Link to next
LabSearchBound:
	test	bl,1			;End of chain (either unbound or -1)
	jz	BoundLoop
	pop	ds			;Label not found

LabelScopeError:
	mov	ax,ER_UL		;"Undefined Label" or incorrect scoping
	dec	si
	dec	si			;Back up to oName (sets error location)
	call	SsError
	LODSWTX 			;recover oName
	or	sp,sp			;reset psw.z
	ret


LabFound:
	mov	ax,bx
;   ax = address of definition
;es:di = address of reference
	dec	ax			;Move back to oTx of BOL pcode
	dec	ax
	pop	ds			;recover ds = dgroup
assumes	ds,data

; fall into CheckLabelScope.

page
;CheckLabelScope - find and check oRs for passed oTx, verify expected scope
;
;Purpose:
;	This routine verifies that the found label is in the expected
;	scope.	If not, an Undefined label error is issued.
;Entry:
;	 oRsExpected = oRs of expected scope.
;	 ax  = oTx
;	 dx  = Text table seg of of label def.
;	 es:si	= text table addr of label ref
;Exit:
;	 psw.z - indicates valid scope.
;	 psw.nz - indicates scoping error detected, and Undefined label error
;		  has been recorded.
;	 ax  = oTx of label if valid scope, or oNam if invalid scope.
;Preserves:
;	dx
;
;NOTE: CheckLabelScope is called from forward ref case, and fallen into.
;      from the already backwards ref/not current table case.

CheckLabelScope:

; Find oRs of found oTx and check against expected oRs.  If mismatch,
; then we have a scoping error.

	mov	bx,oRsExpected		;get expected oRs
	shr	bx,1			;see if in sub or function
	jc	LabelScopeX		;brif so, scope is ok

; oRs could be in a DEF FN or at module level.	Verify it is at expected
; scope.
	shl	bx,1			;restore expected oRs
	call	ScanORsOfOtx		;cx = oRs of label def.
	cmp	bx,cx			;is oRs expected oRs?
	jz	LabelScopeX		;brif so, no error

; Check for the special case of RESTORE <lab/line> which can bind
; to either the current DEF FN, or to the Module level.
	cmp	fRestoreLab,0		;is this special case of RESTORE?
	jz	LabelScopeError 	;brif not, issue error

; We only can get here if the RESTORE statement was in a DEF FN and
; the label definition was also in a DEF FN.  Make sure that the label
; def and the RESTORE statement are in the same DEF FN.
	cmp	cx,grs.GRS_oRsCur	;is this the same DEF FN?
	jnz	LabelScopeError 	;brif not - Label Scoping error

LabelScopeX:
	xor	bx,bx			;return with psw.z set - scope ok
	ret

page
;ScanORsOfOtx - find oRs of passed oTx.
;
;Purpose:
;	Given an  offset within the text table table defined by
;	DX, return the oRs of the DEF FN, or module which it falls
;	within.  This routine understands how defs are bound in execute
;	and non execute states, and can bridge the Scan gap if
;	this is the current text table.
;
;	The DEF FN chain is preserved in SS_EXECUTE state.  If a table
;	has been fully bound, then the DEF FN chain for the module is
;	directly walkable.  If the text table is partially bound, then
;	if SsLinkCtl.TXLNK_DefFn is 0 then no DEF FNs have been bound
;	yet.  The DEF FN chain is walkable by adjusting all entries
;	by the size of the scan gap.  If the SsLinkCtl.TXLNK_DefFn
;	is non-zero, then the scan bound/unbound gap is defined by
;	a single entry in the DEF FN chain with the low order bit set.
;	This entry indicates that the NEXT entry starts the unbound
;	DEF FN chain.  The unbound entries are walkable by applying
;	the size of the scan gap as an adjustment value.
;	The DEF FN chain is terminated by an UNDEFINED ptr.
;
;Entry:
;	ax = otx
;	dx = text table seg
;Exit:
;	cx = oRs of module or DEF FN containing the oTx.
;Preserves:
;	ax, bx, dx.
;

ScanORsOfOtx:
DbAssertRel ax,nz,UNDEFINED,SCAN,<Invalid oTx passed to ScanORsOfOtx>
	push	es
	push	di
	push	si
	push	bx
	push	dx			;save text seg of bind
	mov	cx,[grs.GRS_oMrsCur]	;default return value = oMrsCur
	mov	di,CODEOffset exStDefFn ;search is for DEF FN executors
	mov	si,es
	mov	es,dx			;es = seg adr of search txt tbl
	mov	bx,[mrsCur.MRS_otxDefFnLink] ;start at head of DEF FN list

	cmp	dx,si			;are we searching txdCur?
	mov	dx,0			;default adjustment for scan gap.
					;we start out assuming that we are
					;searching bound pcode.
	jnz	DefFnLoop		;brif not, table has already been bound
	push	bx			;save start of DefFn chain
	mov	bx,SsLinkCtl
	cmp	[bx].TXLNK_DefFn,0	;have any DEF FN entries been bound?
	pop	bx			;recover start of DEF FN list
	jnz	DefFnLoop		;brif so, search bound pcode first


	or	bl,1			;flag that we are crossing scan gap
					;search unbound pcode
DefFnLoop:
	cmp	bx,UNDEFINED		;are we at the end of DEF FN list?
	jz	DefFnExit		;brif so
	shr	bx,1			;clear lsb - Are we crossing scan gap?
	jnc	NotCrossingGap		;brif not


	mov	di,opStDefFn		;search for DEF FN opcodes in
					;unbound pcode
	mov	dx,SscbTxExpand 	;get table expansion factor for unbound
					;def fn links
NotCrossingGap:
	shl	bx,1			;recover oTx of next DEF FN link
	add	bx,dx			;adjust for text expansion
	cmp	bx,ax
	ja	DefFnExit		;brif beyond otx of interest
	mov	cx,[grs.GRS_oMrsCur]	;default return value = oMrsCur

	cmp	WORD PTR es:[bx-4],opNoList0 ;is this the CONT statement?
	je	GotContContext		;brif so - exceptional case
	cmp	WORD PTR es:[bx-4],di	;is this a DEF FN executor/opcode?
	jne	NotInDef		;brif its an END DEF
ContInDef:
	mov	cx,es:[bx+2]		;ax = oPrs of this DEF FN
	or	ch,80H			;make it into an oRs
NotInDef:
	DbChk	DefFnLink		;perform sanity check on this link.
	mov	bx,es:[bx]		;bx points to next in linked list
	jmp	SHORT DefFnLoop 	;applies standard BOUND adjustment

; Handle very special case where we are in Edit and Continue, AND the
; current statment is a DEF FN or END DEF that has yet to be scanned.
; In this case opNoList0 was inserted into the pcode for the opStDefFn
; or opStEndDef.  The real opcode is in SsErrOpcode.

GotContContext:
       cmp	[SsErrOpcode],di	;is this a DEF FN?
       je	ContInDef		;brif so
       jmp	short NotInDef		;must be END DEF

DefFnExit:
	pop	dx
	pop	bx
	pop	si
	pop	di
	pop	es
	ret

page
;DebChkDefFnLink - perform sanity check on Def Fn chain link.
;
;Purpose:
;	Verifies that current link in Def Fn chain is valid.
;Entry:
;	es:bx - pcode addr of chain link
;Exit:
;	none.
;Uses:
;	none.
;Exceptions:
;	Invalid DefFn chain link.


sEnd	SCAN
end
