	TITLE	prscg.asm - Parser Code Generation Functions

;==========================================================================
;
;  Module:  prscg.asm - Parser Code Generation Functions
;  Subsystem:  Parser
;  System:  Quick BASIC Interpreter
;
;==========================================================================

	include		version.inc
	PRSCG_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	opmin
	includeOnce	opcontrl
	includeOnce	opstmt
	includeOnce	opintrsc
	includeOnce	parser
	includeOnce	pcode
	includeOnce	prstab
	includeOnce	psint
	includeOnce	qbimsgs
	includeOnce	rtps
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	util
	includeOnce	variable


;--------------------------------------------------------------------------
;			Code Generation Overview
;
; During the course of interpreting the parse state tables, NtParse()
; encounters MARK(nnn) directives.  These cause NtParse to push
; the current pcode offset and the constant nnn onto a stack as follows:
;
; Given BNF of
;     exp MARK(1) exp MARK(2) exp
;
; Before parsing the statement, the marker stack looks like:
;    high memory:
;	maxStkMark-->        <--pCurStkMark
;           :
;	minStkMark-->
;    low memory:
;
; After parsing the statement, but before calling the code generation
; function for the statement, the marker stack looks like:
;    high memory:
;	maxStkMark-->[oDstPcode]
;
;                    [oDstPcode]
; <--pCurStkMark
;           :
;	minStkMark-->
;    low memory:
;
; Code generation functions use the information on the marker stack to
; decide how to alter pcode already emitted to the pcode buffer during
; parsing.
; An Argument may be passed to a code generation function in ax.
;
;--------------------------------------------------------------------------

assumes	ds,DATA
assumes	ss,DATA
assumes	es,NOTHING

sBegin	DATA
sEnd	DATA

sBegin	CP
assumes	cs,CP

;*********************************************************************
; VOID InsertOp(ax:opcode, bx:oDst)
;
; Purpose:
;	Insert an opcode at a given offset into the pcode buffer
;	If out-of-memory, ps.errCode = ER_OM on exit
;
; Entry:
;	bx = offset into ps.bdpDst where word is to be inserted
;	ax = word to be inserted
; Exit:
;	Caller's can depend on bx being preserved
;	If an out-of-memory error occurs, ps.errCode = ER_OM
;
;*********************************************************************
InsertOp PROC NEAR
	;make room for 2 bytes in pcode buffer before oDst
	; BdShiftRight((bd *)&ps.bdpDst, oDst, (ushort)2))
	
	push	bx			;save caller's bx
	push	ax			;save opcode
	push	bx			;save oDst

	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	push	bx			;pass oDst
	PUSHI	ax,2
	call	BdShiftRight		;grow buf, can cause heap movement
	or	ax,ax
	je	InsOpOmErr		;brif out-of-memory
	call	SetDstPbCur		;update ps.bdpDst.pbCur,

	pop	bx			;restore bx = oDst
	add	bx,[ps.PS_bdpDst.BDP_pb]
	pop	[bx]			;pop and store opcode
InsOpExit:
	pop	bx			;restore caller's bx
	ret

InsOpOmErr:
	call	ParseErrOm		;Error "Out of memory"
	pop	bx
	pop	ax
	jmp	SHORT InsOpExit
InsertOp ENDP

;*********************************************************************
; VOID NEAR CgOn(ax:opcode)
;
; Purpose:
;	Called after the RESTORE/RETURN statement has been parsed.
;	It generates code for the statement.
;
; Entry:
;	The top of the MARK stack (*pCurStkMark) is 1 or 2 for
;	   1 for RESTORE
;	   2 for RESTORE <label>
;	opcode = the opcode to emit if top of MARK stack = 1
;	   This can be opStRestore0 or opStReturn0
;
;	The bnf which causes this to occur is:
;	  tkON (event (tkGOSUB ((Lit0 EMIT(opEvGosub) EMIT(UNDEFINED)) |
;			(EMIT(opEvGosub) LabLn)))) |
;	    (tkERROR tkGOTO ((Lit0 EMIT(opStOnError) EMIT(UNDEFINED)) |
;			(EMIT(opStOnError) LabLn))) |
;	    (Exp (tkGOTO MARK(1) | tkGOSUB MARK(2)) LabLn {tkComma LabLn})
;	<CgOn()>
;
;*********************************************************************
PUBLIC	CgOn
CgOn	PROC NEAR
	mov	bx,[pCurStkMark]
	cmp	bx,MAX_STK_MARK
	je	OnExit			;brif no MARK directives from BNF
	push	[bx]			;save markId

	mov	bx,[bx+2]		;bx = offset into pcode which preceeded
					; markId
	mov	ax,[ps.PS_bdpDst.BDP_cbLogical]
	sub	ax,bx			;ax = byte count of operands
	call	InsertOp		;insert word AX at offset BX
					; (bx is preserved)
	pop	ax			;restore ax = markId
	cmp	al,1			;markId
	mov	ax,opStOnGoto
	je	GotGoto			;brif if MARK(1) directive (GOTO)
	mov	ax,opStOnGosub		;else it must be MARK(2) (GOSUB)
GotGoto:
	call	InsertOp		;insert word AX at offset BX
OnExit:
	ret
CgOn	ENDP

;*********************************************************************
; VOID NEAR CgInsert0or1(opcode)
;
; Purpose:
;	Called after the RESTORE/RETURN statement has been parsed.
;	It generates code for the statement.
;	If out-of-memory, ps.errCode = ER_OM on exit
;
; Entry:
;	The top of the MARK stack (*pCurStkMark) is 1 or 2 for
;	   1 for RESTORE/RETURN/RESUME
;	(generated pcode = opStRestore0/opStReturn0/opStResume0)
;	   2 for RESTORE/RETURN/RESUME <label>
;	(generated pcode = opStRestore1/opStReturn1/opStResume <label>)
;	   3 for RESUME 0
;	(generated pcode = opStResume <UNDEFINED>)
;	   4 for RESUME NEXT
;	(generated pcode = opStResumeNext)
;
;	opcode = the opcode to emit if top of MARK stack = 1
;	   This can be opStRestore0, opStReturn0, or opStResume0
;
;	The bnf which causes this to occur is:
;	   tkRESTORE MARK(1) [LabLn MARK(2)]
;	 <CgInsert0or1(opStRestore0)>
;	   tkRETURN MARK(1) [LabLn MARK(2)]
;	 <CgInsert0or1(opStReturn0)>
;	   tkRESUME MARK(1) [(LabLn MARK(2)) | (Lit0 MARK(3)) |
;	                     (tkNEXT MARK(4))]
;	<CgResume(opStResume0)>
;
;*********************************************************************
PUBLIC	CgInsert0or1
CgInsert0or1 PROC NEAR
	xchg	dx,ax			;save opcode in dx
	mov	bx,[pCurStkMark]
	mov	al,[bx]			;al = markId
	cmp	al,1
	je	InsMark1		;brif got RESUME or RETURN or RESTORE
					; with no parameter
	cmp	al,2
	je	InsMark2
	cmp	al,3
	je	InsMark3		;brif got RESUME 0

;else it must be MARK(4) RESUME NEXT
	mov	ax,opStResumeNext
	jmp	SHORT InsEmit

;got RESUME or RETURN or RESTORE with no parameter
InsMark1:
	xchg	ax,dx			;ax = opcode
InsEmit:
	call	Emit16_AX
	jmp	SHORT InsExit

InsMark2:
	push	dx			;save opcode

	;make room for 2 more bytes at end of pcode buffer
	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	PUSHI	ax,2
	call	BdGrow			;grow buf, can cause heap movement
	or	ax,ax
	je	InsOmErr
	;move label's oNam forward in buffer by 2 bytes
	mov	bx,[ps.PS_bdpDst.BDP_pbCur]
	mov	ax,[bx-2]
	mov	[bx],ax

	;Insert opcode before label's oNam
	pop	ax			;ax = opcode
	inc	ax			;map to opcode variant with no parm
					; opStResumeLab opStRestoreLab or
					; opStReturnLab
	mov	[bx-2],ax		;store opcode
	call	SetDstPbCur		;update ps.bdpDst.pbCur
	jmp	SHORT InsExit

;map RESUME 0 to opStResume(UNDEFINED)
InsMark3:
	mov	ax,opStResume
	call	Emit16_AX
	mov	ax,UNDEFINED
	call	Emit16_AX
InsExit:
	ret

InsOmErr:
	jmp	ParseErrOm		;Error "Out of memory"
					; and return to caller
CgInsert0or1 ENDP

;*********************************************************************
; ErrIfPrsHasTxtTbl()
; Purpose:
;	If the current prs (prsCur) has a text table, generate an error.
;	This is called by functions which are about to do something which
;	can only be done to a "compiled" (external) procedure, not a
;	pcoded procedure.
;
; Exit:
;	returns FALSE if prsCur has a text table (condition codes set)
;
;*********************************************************************
ErrIfPrsHasTxtTbl PROC NEAR
	sub	ax,ax			;prepare to return FALSE
	test	[txdCur.TXD_flags],FTX_mrs
	jne	ErrNoText		;brif prs has no text table
	mov	ax,MSG_InvDecl OR PSERR_fAlert
	call	ParseErr0
	mov	ax,sp			;return TRUE (non-zero)
ErrNoText:
	or	ax,ax			;set condition codes for caller
	ret	
ErrIfPrsHasTxtTbl ENDP

;*********************************************************************
; VOID NEAR CgDeclare(opcode)
;
; Purpose:
;	Called after the DECLARE, SUB, FUNCTION or DEF FN statement has
;	been parsed.  It generates code for the statement.
;	The prs has already been created (by MakeProc in prsid.asm),
;	and is active for all statements except DECLARE.
;
; Entry:
;	Structure pdcl is filled in by parser terminal recognizers like
;	   NtIdSubDecl, NtIdFn [QB4], etc. to describe to prs being declared/defined
;	The MARK stack (*pCurStkMark) contains entries built by the bnf:
;	   MARK 1    indicates CDECL was present
;	   MARK 2 -> ALIAS's string literal
;	   MARK 3 -> start of formal parm list
;	   MARK 4    indicates STATIC was found
;	   MARK 5 -> single line DEF FN's definition expression
;	   MARK 6    indicates ([parmlist]) was seen
;	   MARK 7 -> LIB's string literal	[EB specific] [07]
;	   MARK 8    indicates AUTO was found	[EB specific] [07]
;
;	BNF which builds entry pcode:
;	   tkDECLARE
;	      (tkFUNCTION IdFuncDecl [tkCDECL MARK(1)]
;	         [tkALIAS MARK(2) LitString] MARK(3) parms) |
;	      (tkSUB IdSubDecl [tkCDECL MARK(1)] 
;	         [tkALIAS MARK(2) LitString] MARK(3) parms)
;	     <CgDeclare(opStDeclare)>
;	   tkDEF IdFn MARK(3) parms [tkEQ MARK(5) Exp]
;	     <CgDeclare(opStDefFn)>
;	   tkFUNCTION IdFuncDef MARK(3) parms [tkSTATIC MARK(4)]
;	     <CgDeclare(opStFunction)>
;	   tkSUB IdSubDef MARK(3) parms [tkSTATIC MARK(4)]
;	     <CgDeclare(opStSub)>
;
;	For the statement DECLARE SUB X CDECL ALIAS "abc" (BYVAL A(), B, ...)
;	The pcode buffer contains:
;	              <"abc"> <idA> <idB> ...
;	MARK(1)MARK(2)^MARK(3)^
;
;	For the statement SUB X (BYVAL A(), B, ...) STATIC
;	The pcode buffer contains:
;	       <idA> <idB> ...
;	MARK(3)^MARK(8)
;
;	Where <idX> is 3 16 bit words:  oPrs, oNamProc, oTypProc
;
;*********************************************************************
cProc	CgDeclare,<PUBLIC,NEAR,NODATA>,<si,di>
	localW	opcode
	localW	oDstParms
	localW	oDstAlias
	localW	oDstEndDef
	localW	cbLibInfo
	localW	procAtr
	procAtr_LO EQU  BYTE PTR (procAtr)
	procAtr_HI EQU  BYTE PTR (procAtr+1)
cBegin
	mov	[opcode],ax
	mov	ax,[ps.PS_bdpDst.BDP_cbLogical]
	mov	[oDstEndDef],ax		;save current size of output

	sub	ax,ax
	mov	[procAtr],ax
	mov	[oDstAlias],ax
	mov	[cbLibInfo],ax
	mov	al,[pdcl.PDCL_procType]
	.errnz	DCLA_procType - 0300h
	or	[procAtr_HI],al		;save procType in pcode field
	mov	al,[pdcl.PDCL_oTyp]	;al = value for low byte of ProcAtr
					;  word which DCLA_Explicit,
					;  DCLA_AsClause, and DCLA_oTyp
	mov	[procAtr_LO],al		;save oTyp in pcode field
	sub	ax,ax
	cmp	[pdcl.PDCL_fDeclare],al
	je	NotDeclare		;brif not DECLARE stmt
	cmp	[pdcl.PDCL_cParms],ax
	jne	MarkDisp		;brif got a parm list
	dec	[pdcl.PDCL_cParms]	;set to UNDEFINED so scanner knows
					; to not use this for parm
					; type/count checking
	jmp	SHORT MarkDisp

NotDeclare:
	;If we don't get MARK(4), this SUB/FUNCTION has no STATIC keyword
	and	[prsCur.PRS_flags],NOT FP_STATIC

;-------------------------------------------------
;walk through MARK(xxx) entries from left to right
;-------------------------------------------------
MarkDisp:
	mov	si,MAX_STK_MARK
DeclMarkLoop:
	cmp	[pCurStkMark],si
	jne	DeclNextMark
	jmp	SHORT DeclEndOfMarks

DeclNextMark:
	dec	si
	dec	si
	mov	di,[si]			;di = oDstOpcode
	dec	si
	dec	si
	mov	ax,[si]			;ax = al = markId
	dec	ax
	DbAssertRel ax,be,9,CP,<Unexpected MARK in CgDeclare()>
	shl	ax,1
	xchg	ax,bx			;bx = 2 * (markId - 1)
	jmp	WORD PTR cs:DeclDispTbl[bx]	;dispatch based on markId

DeclDispTbl:
	DW	DeclMark1		;CDECL
	DW	DeclMark2		;ALIAS
	DW	DeclMark3		;parms (before left paren)
	DW	DeclMark4		;STATIC
	DW	DeclMark5		;single line DEF
	DW	DeclMark6		;1st parm (after left paren)

;MARK(1):  Got CDECL directive
DeclMark1:
	call	ErrIfPrsHasTxtTbl
	jne	DeclMarkLoop		;brif prs has a text table
	.errnz	DCLA_cdecl - 8000h
	or	[procAtr_HI],80h	;remember we got CDECL
	jmp	SHORT DeclMarkLoop

;MARK(2):  Got ALIAS directive
DeclMark2:
	call	ErrIfPrsHasTxtTbl
	mov	[oDstAlias],di		;save offset to opLitSD("<alias>")
	jmp	SHORT DeclMarkLoop

;MARK(3):  got offset to formal parm list
DeclMark3:
	mov	[oDstParms],di
	jmp	SHORT DeclMarkLoop

;MARK(4):  got STATIC keyword at end of proc definition
DeclMark4:
	or	[prsCur.PRS_flags],FP_STATIC
	jmp	SHORT DeclMarkLoop

;MARK(5):  got single line DEF FN
DeclMark5:
	mov	[oDstEndDef],di
	mov	ax,opEndSingleDef
	call	Emit16_AX
	mov	ax,2
	call	Emit16_AX		;emit cntEos word
	call	Emit16_0		;emit space for link field
	jmp	SHORT DeclMarkLoop

;MARK(6):  got ( [parmlist] )
DeclMark6:
	cmp	[pdcl.PDCL_cParms],UNDEFINED ;so scanner knows to not use this
					; for parm type/count checking
	jne	DeclMarkLoop		;brif got a parm list
	inc	[pdcl.PDCL_cParms]	;so scanner knows to use this declare
					; for parm type/count checking
	jmp	SHORT DeclMarkLoop


DeclEndOfMarks:
	;Copy Alias text after formal parms
	mov	ax,[oDstAlias]
	or	ax,ax
	je	NoAliasArg
	call	CopyLit			;ax = cbAlias
	mov	[cbLibInfo],ax
	shl	al,1
	shl	al,1
	.errnz	DCLA_cbAlias - 7C00h
	or	[procAtr_HI],al		;save cbAlias in pcode field


	;squeeze source of ALIAS lit out of pcode buffer
	;BdShiftLeft((bd *)&ps.bdpDst, oDstAlias, oDstParms - oDstAlias)
	
	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	push	[oDstAlias]
	mov	ax,[oDstParms]
	sub	ax,[oDstAlias]		;ax = number of bytes to delete
	push	ax
	sub	[oDstParms],ax		;update for the left shift
	sub	[oDstEndDef],ax		;update for the left shift
	call	BdShiftLeft		;grow buf, can cause heap movement

NoAliasArg:
	;Now make room for things to insert before parm list:
	; opcode, byte-count-till-end-of-stmt, link field for DEF FNs, oPrs
	mov	si,10			;assume we need to insert 10 bytes
	cmp	[pdcl.PDCL_procType],PT_DEFFN
	jne	NotDefFn1		;brif not DEF FN stmt
	inc	si			;need 2 extra bytes for link field
	inc	si
NotDefFn1:
	;BdShiftRight((bd *)&ps.bdpDst, oDstParms, cbInsert)
	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	push	[oDstParms]
	push	si			;pass cbInsert parm
	call	BdShiftRight
	or	ax,ax
	jne	ShiftOk			;brif no out-of-memory error
	call	ParseErrOm		;Error "Out of memory"
	jmp	SHORT DeclExit

ShiftOk:
	call	SetDstPbCur		;update ps.bdpDst.pbCur after BdShift...
	push	ds
	pop	es			;es = ds for stosw below
	mov	di,[oDstParms]
	add	di,[ps.PS_bdpDst.BDP_pb]
	mov	ax,[opcode]
	stosw				;store opcode in pcode buffer

	;emit cntEos operand
	mov	ax,[oDstEndDef]
	sub	ax,[oDstParms]
	add	ax,si			;include cbInsert
	add	ax,[cbLibInfo]		;add #bytes in LIB and ALIAS clause
	sub	ax,4
	inc	ax			;round up to even byte count
	and	al,0FEH
	stosw				;store cntEos operand

	cmp	[pdcl.PDCL_procType],PT_DEFFN
	jne	NotDefFn2		;brif not DEF FN stmt
	stosw				;leave room for DEF FN link field
NotDefFn2:
	mov	ax,[pdcl.PDCL_oPrs]
	or	ax,ax
	jns	NotUnboundDefFn
	mov	ax,[pdcl.PDCL_oNam]	;for [DECLARE] DEF FNs, in SS_RUDE
					;state, emit the oNam
NotUnboundDefFn:

	stosw				;store oPrs/oNam operand
	mov	ax,[procAtr]
	stosw				;store proc's oTyp
	mov	ax,[pdcl.PDCL_cParms]
	stosw				;store cParms operand
DeclExit:
cEnd

;*********************************************************************
; CopyLit(ax:oDstLit)
; Purpose:
;	Move the ASCII text of an opLitSD to the end of the pcode buffer.
;	If string is longer than 255 bytes, it is truncated.
; Entry:
;	ax = offset into pcode buffer to opLitSD opcode
;	[EB] bx = TRUE iff length of string is to output as byte preceeding
;		  text of string. 
; Exit:
;	string is moved to end of pcode buffer and the opLitSd is removed
;	ax = length of string
;
;*********************************************************************
CopyLit	PROC NEAR
	push	si
	mov	si,ax
	add	si,[ps.PS_bdpDst.BDP_pb]
	lodsw				;skip opLitSD opcode
	DbAssertRel ax,e,opLitSD,CP,<CopyLit: expected opLitSD> 
	lodsw				;ax = cb operand from opLitSD
	or	ah,ah
	je	LenOk
	mov	ax,255			;truncate string
LenOk:
	push	ax			;save for return value
	inc	ax			;round up to word count
	shr	ax,1
	mov	cx,ax
	jcxz	CLitExit		;brif entire string has been copied
CLitLoop:
	push	cx			;save word count
	lodsw				;ax = next 2 bytes of string

	sub	si,[ps.PS_bdpDst.BDP_pb] ;Emit16 can cause heap movement
	call	Emit16_AX
	add	si,[ps.PS_bdpDst.BDP_pb] ;reconvert offset to pointer

	pop	cx			;restore word count
	loop	CLitLoop
CLitExit:
	pop	ax			;ax = string length
	pop	si
	ret
CopyLit	ENDP

;*********************************************************************
; CgCall(opcode)
; Purpose:
;	Called to generate pcode for the following bnf:
;	 tkCALL (MARK(1) IdImplicit
;	   [tkLParen IdCallArg {tkComma IdCallArg} tkRParen])
;	 tkCALLS MARK(1) IdImplicit
;	   [tkLParen IdCallArg {tkComma IdCallArg} tkRParen]
;
;*********************************************************************
PUBLIC	CgCall
CgCall	PROC NEAR
	push	si			;save caller's si
	mov	bx,MAX_STK_MARK
	mov	si,[bx-2]		;si=offset in pcode for item after MARK
	push	ax			;save opcode for Emit16 below
	mov	bx,[ps.PS_bdpDst.BDP_pb]
	mov	ax,[bx][si]		;ax = oNamIdSub
	call	SubRef			;ax = oPrs for sub being called
					;We can ignore error results, because
					;if error occurs, no code will ever
					;try to activate this oPrs, because
					;line will be stored as opReParse

	;delete the information emitted by NtIdImplicit()
	;BdShiftLeft((bd *)&ps.bdpDst, oDstCur, 2)
	
	PUSHI	dx,<dataOFFSET ps.PS_bdpDst>
	push	si
	PUSHI	dx,2
	mov	si,ax			;si = oPrs
	call	BdShiftLeft		;grow buf, can cause heap movement
	call	SetDstPbCur		;set ps.bdpDst.pbCur after BdShiftLeft
	call	Emit16			;emit opcode pushed ~15 lines above
	push	[cIdArgs]
	call	Emit16			;emit arg count
	push	si			;push oPrs
	call	Emit16
	pop	si			;restore caller's si
	ret
CgCall	ENDP

;*********************************************************************
; VOID NEAR CgRun(opcode)
; Purpose:
;	Invoked to generate code for the following bnf:
;	 tkRUN [(Ln MARK(1)) | (Exp MARK(2))]; <CgRun()>
;
;*********************************************************************
PUBLIC	CgRun
CgRun	PROC NEAR
	mov	bx,[pCurStkMark]
	cmp	bx,MAX_STK_MARK
	mov	ax,opStRunMain		;ax = opcode to emit for RUN
	je	RunEmitExit		;brif simple RUN (no MARKs)
	DbAssertRelB [bx],be,2,CP,<Invalid MARK id in CgRun()>
	cmp	BYTE PTR [bx],1
	jne	RunFile			;brif markId != 1  (RUN <filename>)

	;Got RUN <line number>, insert opStRunLabel before Ln
	mov	bx,[bx+2]		;bx = pcode offset
	dec	bx
	dec	bx			;bx = pcode offset where opcode is to go
	mov	ax,opStRunLabel
	call	InsertOp		;insert word AX at offset BX
	jmp	SHORT RunExit

RunFile:
	mov	ax,opStRunFile
RunEmitExit:
	call	Emit16_AX
RunExit:
	ret
CgRun	ENDP

;*********************************************************************
; VOID NEAR CgInput(opcode)
; Purpose:
;	Invoked to generate code for the following bnf:
;
;	tkINPUT 
;	 [(lbsInpExpComma MARK(16)) |
;	  (tkSColon MARK(2) [LitString MARK(4) (tkSColon | (tkComma MARK(1)))])|
;	  (LitString MARK(4) (tkSColon | (tkComma MARK(1))))]
;	 MARK(8) IdAryElemRef EMIT(opStInput) {tkComma IdAryElemRef
;                                              EMIT(opStInput)}
;	 EMIT(opInputEos)
;	   <CgInput(opInputPrompt)>
;
;	tkLINE tkINPUT
;	 [(lbsInpExpComma MARK(16)) |
;	  (tkSColon MARK(2) [LitString MARK(4) (tkSColon | (tkComma MARK(1)))]) |
;	  (LitString MARK(4) (tkSColon | (tkComma MARK(1))))]
;	 IdAryElemRef
;	   <CgInput(opStLineInput)>
;
;	It maps syntax to pcode as follows:
;	   INPUT [;] [prompt (,|;) <list> =>
;	    [sdExp] opInputPrompt(cnt,mask,<typelist>)
;	If prompt is followed by a semicolon, FINP_QSupress is not ORed into
;          'mask' which causes a question mark to be appended to the prompt
;          string.
;	The optional semicolon after INPUT causes FINP_CrLf not to be ORed into
;	   'mask' which causes the user's terminating carriage return not to be
;	   echoed.
;
;*********************************************************************
PUBLIC	CgInput
CgInput	PROC NEAR
.errnz	FINP_QSupress - 1
.errnz	FINP_CrLf - 2
.errnz	FINP_Prompt - 4
	push	si			;save caller's si
	push	di			;save caller's di
	sub	di,di			;init bit mask
	mov	bx,MAX_STK_MARK

;OR bit mask with to 1 for comma after prompt (MARK(1)),
;		     2 for semicolon after INPUT (MARK(2)),
;		     4 for prompt (MARK(4))
InpMarkLoop:
	cmp	[pCurStkMark],bx
	je	InpMarkLoopDone
	dec	bx
	dec	bx
	mov	si,[bx]			;si = oDstOpcode from mark stack
	dec	bx
	dec	bx
	cmp	WORD PTR [bx],16
	jne	NotMark16		;brif markId != 16
	cmp	ax,opStLineInput
	je	InpMarkLoop		;brif not INPUT #n
	jmp	SHORT InpExit		;If INPUT #n, pcode is already complete

NotMark16:
	or	di,[bx]
	DbAssertRel di,b,16,CP,<Invalid markId in CgInput>
	jmp	SHORT InpMarkLoop

;di = bit mask (built by ORing markIds)
;ax = opcode
;
InpMarkLoopDone:
	cmp	ax,opStLineInput
	jne	NotLineInput
	call	Emit16_AX		;emit opcode
	push	di			;emit bit mask
	call	Emit16
	jmp	SHORT InpExit

;It was INPUT, not LINE INPUT
;insert opStInputPrompt[cbOperands:16,mask:8,types:8]
;don't count string literal in cInputItems (its in cIdArgs)
;count mask in cInputItems (cancels string literal)
;di = bit mask (built by ORing markIds)
;ax = opcode
;
NotLineInput:
	push	ax			;save opcode on stack
	mov	ax,[cIdArgs]
	shr	ax,1			;round down to word count
	mov	cx,ax			;cx = word count
	mov	bx,si			;bx = oDstOpcode (place to insert ops)
	jcxz	InpLoopDone

;emit garbage typelist (scanner will fill in)
InpLoop:
	push	cx			;save word count
	call	InsertOp		;insert word AX at offset BX
					;any value in ax would do, scanner fills
					; (bx is preserved)
	pop	cx			;restore word count
	loop	InpLoop

;now emit mask and space for 1st entry in type list
InpLoopDone:
	mov	ax,di
	and	al,7			;mask off bits > 7
	call	InsertOp		;insert word AX at offset BX
					; (bx is preserved)
	mov	ax,[cIdArgs]
	inc	ax
	call	InsertOp		;insert word AX at offset BX
					; (bx is preserved)
	pop	ax			;ax = opcode (pushed ~25 lines above)
	call	InsertOp		;insert word AX at offset BX
InpExit:
	pop	di			;save caller's di
	pop	si			;save caller's si
	ret
CgInput	ENDP

;*********************************************************************
; CgStmtCnt(opcode)
; Purpose:
;	Called for statements like CLEAR, CLOSE, COLOR, ERASE, FIELD, LOCATE
;	and SCREEN, which take as an operand the number of arguments
;	preceding them.
;
;*********************************************************************
PUBLIC	CgStmtCnt
CgStmtCnt PROC NEAR
	call	Emit16_AX		;emit the opcode
	mov	ax,[cIdArgs]
	jmp	Emit16_AX		;emit the arg count
					;and return to caller
CgStmtCnt	ENDP

;*********************************************************************
; CgCntHigh(opcode)
;
; Purpose:
;
;   Called for the Format$ function.
;
;*********************************************************************

	;Added with [11]


	;End of [11]

;*********************************************************************
; CgLineStmt(opcode)
;
; Purpose:
;	Invoked to generate code for the following bnf:
;	   tkLINE [coordStep] tkMinus coord2Step
;	[tkComma [Exp MARK(1)]
;	  [tkComma [(RwBF MARK(3)) | (RwB (RwF MARK(3)) | MARK(2))]
;	    [tkComma Exp MARK(4)]]]
;	<CgLineStmt(opStLine)>
;
;*********************************************************************
PUBLIC	CgLineStmt
CgLineStmt PROC NEAR
	push	si			;save caller's si
	sub	dx,dx			;operand = 0
	mov	cx,ax			;cx = opcode
	mov	si,[pCurStkMark]
LineLoop:
	cmp	si,MAX_STK_MARK
	je	LineLoopDone
	lodsw				;ax = markId
	inc	si			;skip oDstPcode
	inc	si
	dec	ax
	je	LineMark1		;brif MARK(1)
	dec	ax
	je	LineMark2		;brif MARK(2)
	dec	ax
	je	LineMark3		;brif MARK(3)

;MARK(4) means line style parm was specified
	inc	cx			;convert opStLine, opStLineColor->
					; opStLineStyle, opStLineColorStyle
	;fall into case 1
;MARK(1) means color parm was specified
LineMark1:
	inc	cx			;convert opStLine to opStLineColor
	jmp	SHORT LineLoop

;MARK(2) means B parm was specified
LineMark2:
	mov	dl,1			;operand = 1
	jmp	SHORT LineLoop

LineMark3:
;MARK(3) means BF parm was specified
	mov	dl,2			;operand = 2
	jmp	SHORT LineLoop

LineLoopDone:
	push	dx			;pass operand to Emit16 below
	xchg	ax,cx			;ax=opcode for Emit16_AX
	call	Emit16_AX		;emit the opcode
	call	Emit16			;emit the operand
	pop	si			;restore caller's si
	ret
CgLineStmt ENDP

;*********************************************************************
; CgOpen(opcode)
;
; Purpose:
;	Invoked to generate code for the following bnf:
;  tkOPEN Exp
;   ([(tkFOR ((tkAPPEND  MARK(1)) |
;             (tkINPUT   MARK(2)) | 
;             (tkOUTPUT  MARK(3)) |
;             (tkRANDOM  MARK(4)) |
;             (tkBINARY  MARK(5))))]
;    [tkACCESS ((tkREAD  MARK(6) [tkWRITE MARK(8)]) | (tkWRITE MARK(7)))]
;    [(tkLOCK ((tkREAD ((tkWRITE MARK(11)) | MARK(9))) |
;              (tkWRITE MARK(10)))) |
;     (tkSHARED MARK(12))]
;    tkAS optFilenum [tkLEN tkEQ Exp MARK(13)])  |
;   (tkComma optFilenum exp12 MARK(14))
;     <CgOpen(opStOpen2)>
;
;*********************************************************************
tModeMask LABEL WORD
	DW MD_APP		; MARK(1) means APPEND was specified
	DW MD_SQI		; MARK(2) means INPUT was specified
	DW MD_SQO		; MARK(3) means OUTPUT was specified
	DW MD_RND		; MARK(4) means RANDOM (or default) was speced
	DW MD_BIN		; MARK(5) means BINARY was specified
	DW ACCESS_READ * 256	; MARK(6) means READ was specified
	DW ACCESS_WRITE * 256	; MARK(7) means WRITE was specified
	DW ACCESS_BOTH * 256	; MARK(8) means READ WRITE was specified
	DW LOCK_READ * 256	; MARK(9) means LOCK READ was specified
	DW LOCK_WRITE * 256	; MARK(10) means LOCK WRITE was specified
	DW LOCK_BOTH * 256	; MARK(11) means LOCK READ WRITE was specified
	DW LOCK_SHARED * 256	; MARK(12) means SHARED was specified

PUBLIC	CgOpen
CgOpen	PROC NEAR
	push	si			;save caller's si
	mov	cx,ax			;cx = opcode
	sub	dx,dx			;mode = 0
	mov	si,[pCurStkMark]
OpenLoop:
	cmp	si,MAX_STK_MARK
	je	OpenLoopDone
	lodsw				;ax = markId
	inc	si			;skip pcode offset
	inc	si
	cmp	al,14
	je	OpenMark14
	cmp	al,13
	je	OpenMark13
	;mode |= tModeMask[markId - 1]
	xchg	bx,ax			;bx = markId
	shl	bx,1			;convert to word index
	mov	ax,tModeMask - 2[bx]	;ax = mask
	or	dx,ax			;or mask into mode
	jmp	SHORT OpenLoop

;MARK(13) means LEN=nnn was specified
OpenMark13:
	inc	cx			;convert opStOpen2 to opStOpen3
	jmp	SHORT OpenLoop

OpenLoopDone:
	push	dx			;save mode
	xchg	ax,cx			;emit opcode
	call	Emit16_AX
	pop	ax			;ax = open mode
	test	al,MD_APP OR MD_SQI OR MD_SQO OR MD_RND OR MD_BIN
	jne	OpenEmitExit		;brif open mode was specified
	or	al,MD_DEFAULT		;default open mode
OpenEmitExit:
	call	Emit16_AX		;emit open mode
	pop	si			;restore caller's si
	ret

;MARK(14) means old open syntax
OpenMark14:
	mov	ax,[cIdArgs]
	add	ax,opStOpenOld3 - 3	;ax = old open opcode
	jmp	SHORT OpenEmitExit

CgOpen	ENDP

;*********************************************************************
; CgLock(opcode)
;
; Purpose:
;	Invoked to generate code for the following bnf:
;	tkLOCK optFileNum
;	 (tkComma (Exp MARK(1) [tkTO MARK(2) Exp]) | (tkTO MARK(3) Exp]))
;	   <CgLock(opStLock)>
;	tkUNLOCK optFileNum
;	 (tkComma (Exp MARK(1) [tkTO MARK(2) Exp]) | (tkTO MARK(3) Exp]))
;	   <CgLock(opStLock)>
;
;*********************************************************************
PUBLIC	CgLock
CgLock	PROC NEAR
	push	si			;save caller's si
	push	di			;save caller's di
	mov	di,ax			;di = opcode
	sub	dx,dx			;mode = markId = 0
	cmp	ax,opStUnLock
	jne	NotUnlock
	mov	dl,LOCK_UNLOCK		;mode = LOCK_UNLOCK
NotUnlock:
	mov	si,[pCurStkMark]
LockLoop:
	cmp	si,MAX_STK_MARK
	je	LockLoopDone		;brif done with MARK directives
	lodsw				;al = markId
	xchg	cx,ax			;cl = markId
	lodsw				;ax = oDstOpcode
	cmp	cl,1
	jne	NotMark1		;brif not MARK(1)
	test	dl,LOCK_1stToLast
	jne	NotMark1		;brif MARK(2) was seen
	or	dh,LOCK_DefLastArg/256	;tell executor to default last record
NotMark1:
	cmp	cl,3
	jne	NotMark3		;brif not MARK(3)
	xchg	bx,ax			;bx = oDstOpcode
	;Emit default 1st record before the 2nd Exp
	push	dx			;save dx
	.erre	opLitI2Max GE 1 	; Assure 1 is allowed
	mov	ax,opLitI2+OPCODE_MASK+1; pass opLitI2 with value of 1
	call	InsertOp		;insert word AX at offset BX
					; (bx is preserved)
	pop	dx			;restore dx = mode & markId
	or	dh,LOCK_Def1stArg/256	;tell lister to default 1st record
NotMark3:
	or	dl,LOCK_1stToLast
	jmp	SHORT LockLoop

LockLoopDone:
	push	dx			;pass mode to 2nd call of Emit16
	xchg	ax,di			;pass opcode to Emit16_AX
	call	Emit16_AX		;emit the opcode
	call	Emit16			;emit the mode operand
	pop	di			;restore caller's di
	pop	si			;restore caller's si
	ret
CgLock	ENDP

PUBLIC	Cg0or1Args, Cg1or2Args, Cg2or3Args, Cg3or4Args
Cg3or4Args PROC NEAR
	dec	ax
Cg3or4Args ENDP				;fall into Cg2or3Args
Cg2or3Args PROC NEAR
	dec	ax
Cg2or3Args ENDP				;fall into Cg1or2Args
Cg1or2Args PROC NEAR
	dec	ax
Cg1or2Args ENDP				;fall into Cg0or1Args
Cg0or1Args PROC NEAR
	mov	dx,ax			;save base opcode in dx
	add	ax,[cIdArgs]		;ax = opcode + cIdArgs
	cmp	dx,opStMid_2 - 3
	je	CgMoveOpsToEnd		;brif MID$ statement
J1_Emit16_AX:
	jmp	Emit16_AX		;emit ax and return to caller
Cg0or1Args ENDP


;*********************************************************************
; CgMoveOpsToEnd(opcode)
;
; Purpose:
;	Invoked to generate code for the following bnf:
;	  tkLSET MARK(1) idAryElemRef MARK(2) tkEQ Exp
;	    CgMoveOpsToEnd(opStLset)
;	  tkRSET MARK(1) idAryElemRef MARK(2) tkEQ Exp
;	    CgMoveOpsToEnd(opStRset)
;	  tkMID_ tkLParen MARK(1) idAryElemRef MARK(2) exp12 tkRParen tkEQ Exp
;	    Cg3or4Args(opStMid_2)
;	Moves the pcode for idAryElemRef to the end of the buffer
;
;*********************************************************************
cProc	CgMoveOpsToEnd,<PUBLIC,NEAR,NODATA>,<si,di>
cBegin
	push	ax			;pass opcode to Emit16 (at end of proc)
	mov	si,[pCurStkMark]
	lodsw				;al = markId (2)
	lodsw				;ax = text offset for MARK(2)
	xchg	di,ax			;bx = text offset for MARK(2)
	lodsw				;al = markId (1)
	lodsw				;ax = text offset for MARK(1)
	xchg	si,ax			;si = text offset for MARK(1)

	;setup for BdShiftLeft((bd *)&ps.bdpDst, oDstCur, cbMoved)
	
	PUSHI	ax,<dataOFFSET ps.PS_bdpDst>
	push	si
	mov	ax,di
	sub	ax,si
	push	ax			;pass cbMoved
					;call to BdShiftLeft is after loop
MoveLoop:
	cmp	si,di
	je	MoveDone
	add	si,[ps.PS_bdpDst.BDP_pb] ;si = ptr to next word to be moved
	lodsw				;ax = next word to be moved
	sub	si,[ps.PS_bdpDst.BDP_pb] ;si = offset to next word to be moved
	call	Emit16_AX		;copy word to end of buffer
	jmp	SHORT MoveLoop

MoveDone:
	;delete the source of copied words - parms pushed several lines above
	call	BdShiftLeft		;grow buf, can cause heap movement
	call	SetDstPbCur		;update ps.bdpDst.pbCur for BdShiftLeft
	;emit opcode - parm pushed several lines above
	call	Emit16
cEnd

PUBLIC	CgCircle
CgCircle PROC NEAR
	mov	dx,MAX_STK_MARK
	cmp	[pCurStkMark],dx
	je	NoCircleMark
	inc	ax			;got a MARK(1), color parm included
					;map opStCircle to opStCircleColor
NoCircleMark:
	jmp	Emit16_AX		;emit ax and return to caller
CgCircle ENDP


sEnd	CP
end
