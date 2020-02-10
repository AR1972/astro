	TITLE	TXTUTIL - Text Table Management Routines

;============================================================================
;
; Module: TxtUtil.asm - Text Table Management Routines
; System: Quick BASIC Interpreter
;
;	--------- --- ---- -- ---------- ----
;	COPYRIGHT (C) 1985 BY MICROSOFT, INC.
;	--------- --- ---- -- ---------- ----
;
;
;============================================================================

	.xlist
	include version.inc
	TXTUTIL_ASM = ON
	includeOnce architec
	includeOnce context
	includeOnce executor
	includeOnce heap
	includeOnce lister
	includeOnce msgshort
	includeOnce names
	includeOnce optables
	includeOnce opcontrl
	includeOnce opid
	includeOnce opmin
	includeOnce opstmt
	includeOnce opaftqb4
	includeOnce parser
	includeOnce pcode
	includeOnce qbimsgs
	includeOnce rtinterp	;just needed for CbStrucErrRet
	includeOnce rtps
	includeOnce scanner
	includeOnce txtint
	includeOnce txtmgr
	includeOnce ui
	includeOnce variable

	.list

assumes DS,DATA
assumes SS,DATA
assumes ES,NOTHING

externFP B$GetCompSwitches	; runtime routine for QuickLIB switch info


;-------------------------------------------------------------------------
;		DATA Segment Declarations
;-------------------------------------------------------------------------

sBegin	DATA

PUBLIC	fViewInclude
fViewInclude	DB 0	;non-zero if included source lines are visible
PUBLIC	fLnNotIncl
fLnNotIncl	DB 0	;static return value of LnOfOtx, OtxOfLn, OtxBolOfOtx
			;zero if given line was an INCLUDEd line
PUBLIC	cInclNest
cInclNest	DB 0	;$INCLUDE nesting depth.  Non-zero if currently loading
			;an $INCLUDE file

;used to pass info to DoDescan()
descanTo	DB 0

PUBLIC	compSwitches
compSwitches	DB 0	;Set by SetCompSwitches()
fPreScanAsChg	DB 0	;non-zero when PreScanAsChg is active

;statics needed by ChkLastAs()
oNamAs		DW 0
oRsAs		DW 0
otxAsDelStart	DW 0
otxAsDelEnd	DW 0

externB fLoadInclude	



sEnd	DATA


;-------------------------------------------------------------------------
;		CODE (execution-time) Segment Functions
;-------------------------------------------------------------------------

sBegin CODE
assumes CS,CODE

;Table of all the opcodes that are searched for by PreScanAsChg
;
tOpAs	LABEL WORD
	opTabStartAll	AS
	opTabEntry	AS,opOffLd
	opTabEntry	AS,opOffSt
	opTabEntry	AS,opNoType	;assumed to be 2nd to last
	opTabEntry	AS,opEot

;Table of all opcodes which represent  AS <usertype>
tOpAsType LABEL WORD
	opTabStart	ASTYPE
	opTabEntry	ASTYPE,opAsType
	opTabEntry	ASTYPE,opStDefFn    ;must preceed Declare,Sub,Function
	opTabEntry	ASTYPE,opStDeclare
	opTabEntry	ASTYPE,opStSub
	opTabEntry	ASTYPE,opStFunction
	opTabEntry	ASTYPE,opEot

;Table of all the opcodes that are op_Include (or opStInclude [03])
;
tOpReInclude LABEL WORD
	opTabStart	RI
	opTabEntry	RI,op_Include
	opTabEntry	RI,opEot

;Table of all the opcodes which affect the setting of Compiler switches
;Can't use opTab... macros because we need opEot as 1st entry in table.
;
tOpCompSw LABEL WORD
	opTabStart	CSW
	opTabEntry	CSW,opStOnError
	opTabEntry	CSW,opEvGosub
	opTabEntry	CSW,opEvOff
	opTabEntry	CSW,opEvOn
	opTabEntry	CSW,opEvStop
			CSW_EventMax EQU CSW_opEvStop
	opTabEntry	CSW,opStResume
	opTabEntry	CSW,opStResume0
	opTabEntry	CSW,opStResumeNext
	opTabEntry	CSW,opEot

;These functions are included in the CODE segment, because they
; are called by runtime code, and we wouldn't want to swap in
; the entire CP segment just to do these functions.

;*************************************************************************
; DescanOpcode(opcode)
;
; Purpose:
;	Given the address of an executor for an opcode, return the opcode's
;	id.  For example, give, ExStStop, it would return opStStop.
;
; Entry:
;	parm1: ushort - opcode's executor offset into CODE segment
;
; Exit:
;	AX = opcode id
;
;*************************************************************************
cProc	DescanOpcode,<PUBLIC,FAR,ATOMIC>	
	parmW	opcode
cBegin
	mov	bx,[opcode]
	mov	ax,cs:[bx-2]
cEnd

sEnd	CODE

;-------------------------------------------------------------------------
;		CP (compile) Segment Functions
;-------------------------------------------------------------------------


					; the current text table

sBegin	CP
assumes CS,CP

;*************************************************************************
; TxtTblSegCurCP(), TxtSegCurCP()
;
; Purpose:
;	Return the segment address for the start of the current text table.
;
; NOTE: TxtTblSegCurCP never returns the adr of the direct mode buffer.
;	TxtSegCurCP return the adr of the direct mode buffer if
;	  grs.fDirect is TRUE.
;
; Entry:
;	The structure txdCur identifies the current text table.
;	For TxtSegCurCP, if grs.fDirect is TRUE, the direct mode
;	   text table (grs.bdlDirect) is used instead.
;
; Exit:
;   TxtTblSegCurCP:  ES = segment address,
;	             cx preserved (callers depend on this)
;   TxtSegCurCP:     ES = segment address
;	             All other registers preserved (including flags)
;		     (callers depend on all being preserved)
;
;*************************************************************************
PUBLIC	TxtTblSegCurCP
TxtTblSegCurCP PROC NEAR
	DbChk	ConStatStructs		;ensure static structures
	GETSEG	es,[txdCur.TXD_bdlText_seg],,<SPEED,LOAD> ;[2]
	ret
TxtTblSegCurCP ENDP

PUBLIC	TxtSegCurCP
TxtSegCurCP PROC NEAR
	pushf
	push	bx
	DbChk	TxdCur			;perform sanity check on txdCur
	DbChk	ConStatStructs		;ensure static structures
	mov	bx,dataOFFSET txdCur.TXD_bdlText
	cmp	[grs.GRS_fDirect],FALSE
	je	NotDirectMode		;branch if not in direct mode
	mov	bx,dataOFFSET grs.GRS_bdlDirect
NotDirectMode:
	GETSEG	es,[bx.BDL_seg],bx,<SPEED,LOAD> ;[2] es = seg adr of text table
	pop	bx
	popf
	ret
TxtSegCurCP ENDP

sEnd	CP				

sBegin	SCAN				
assumes CS,SCAN 			

;***
; TxtSegCurSCAN
;
; Purpose:
;	Return the segment address for the start of the current text table.
;	Added as revision [20].
;
;	TxtSegCurSCAN returns the adr of the direct mode buffer if
;	  grs.fDirect is TRUE.
;
; Entry:
;	The structure txdCur identifies the current text table.
;	if grs.fDirect is TRUE, the direct mode
;	   text table (grs.bdlDirect) is used instead.
;
; Exit:
;   TxtSegCurSCAN:   ES = segment address
;	             All other registers preserved (including flags)
;		     (callers depend on all being preserved)
;
;*************************************************************************
PUBLIC	TxtSegCurSCAN
TxtSegCurSCAN PROC NEAR
	assumes ds,NOTHING		
	pushf
	push	bx
	DbChk	TxdCur			;perform sanity check on txdCur
	DbChk	ConStatStructs		;ensure static structures
	mov	bx,dataOFFSET txdCur.TXD_bdlText
	cmp	[grs.GRS_fDirect],FALSE
	je	@F			;branch if not in direct mode

	mov	bx,dataOFFSET grs.GRS_bdlDirect
@@:
	GETSEG	es,ss:[bx.BDL_seg],bx,<SPEED,LOAD> ;[2] es = seg adr of text table
	pop	bx
	popf
	ret
	assumes DS,DATA 		
TxtSegCurSCAN ENDP

sEnd	SCAN				


sBegin	CP				
assumes CS,CP				

;**************************************************************
; void DoDescan()
; Purpose:
;	Called indirectly by SystemDescan and ModuleRudeEdit.
;	See comments in those functions.
;
; Entry:
;	descanTo = SS_PARSE or SS_RUDE
; Exit:
;	grs.fDirect = FALSE
;	returns ax = non-zero (so ForEach... continues)
;
;**************************************************************
DoDescan PROC NEAR
	DbAssertRelB [descanTo],ne,SS_EXECUTE,CP,<DoDescan - bad descanTo>
	SetfDirect al,FALSE		;turn off direct mode if on
	and	[grs.GRS_flags],NOT FG_allSsExecute
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jae	DdExit			; brif text table is already SS_RUDE
					; or lower (we never descan lower
					; than SS_RUDE)
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	jne	NotExec			;we always descan at least to SS_PARSE
	call	SsDescan		;descan from SS_EXECUTE to SS_PARSE
	DbChk	TxdOps			;see if rude scanner inserted bad opcode
	;Only call TxtMoved if we descanned the text table, if we call
	; it every time we call DoDescan, Txt Edits slow down because we
	; have lost the cache unnecessarily.
	call	TxtMoved		;can't depend on cached text offsets
					;reset History and Watch info too
NotExec:
	cmp	[descanTo],SS_RUDE
	jne	DdExit			;brif only descanning to SS_PARSE
	push	WORD PTR ([descanTo])	;pass SS_RUDE to SsRudeScan
	call	SsRudeScan
	DbChk	TxdOps			;see if rude scanner inserted bad opcode

DdExit:
	mov	ax,sp			;return non-zero result for ForEachCP
	ret
DoDescan ENDP

;**************************************************************
; SystemDescan()
; Purpose:
;	This is called to descan all module and procedure text table's
;	to SS_PARSER or lower.  It occurs after some edits like
;	the definition of a function, to force the scanner to re-check
;	all references to the function for type compatability.  It
;	is also called just before a Binary SAVE is executed, so we
;	write opcodes to the file instead of executor addresses.
; SystemDescanRude() is identical, but all text tables are descanned
;	to SS_RUDE instead of SS_PARSE.
;
; This function never results in an error (not even out-of-memory)
;
; Exit:
;	grs.fDirect = FALSE
;
;**************************************************************
PUBLIC	SystemDescanCP
SystemDescanCP PROC NEAR
	test	[flagsTm],FTM_NoSsExecute ;speed opt - skip if all
	jnz	SystemDescanCPX 	;txt tables are already SS_PARSE
					;or lower
	mov	al,SS_PARSE
	mov	[descanTo],al

	;descan each prs in a module before descanning the module itself
	mov	al,FE_PcodeMrs+FE_CallMrsAfter+FE_PcodePrs+FE_SaveRs
	mov	bx,CPOFFSET DoDescan
	call	ForEachCP		;DoDescan returns no error codes
	or	[flagsTm],FTM_NoSsExecute ;all txt tables at least in
SystemDescanCPX:			;SS_PARSE scan state
	SetfDirect al,FALSE		;turn off Direct mode
	ret
SystemDescanCP ENDP

cProc	SystemDescanRudeCP,<PUBLIC,NEAR>
cBegin
;	If a def fn is active, we need to deactivate it.  ForEachCP
;	saves/restores oPrsCur.  ModuleRudeEdit frees all def fn's.
;	The combination of these two would cause ForEachCP to attempt
;	to activate a freed prs if oPrsCur is for a def fn.

	mov	ax,[grs.GRS_oPrsCur]	
	inc	ax			; have an active procedure?
	jz	NotDefFn		; brif not
	dec	ax			
	cCall	FieldsOfPrs,<ax>	; returns dl = proctype
	cmp	dl,PT_DEFFN		; is it a def fn?
	jne	NotDefFn		; brif not
	call	PrsDeactivate		; deactivate active def fn
NotDefFn:				

	mov	al,FE_PcodeMrs+FE_CallMrs+FE_SaveRs
	mov	bx,CPOFFSET ModuleRudeEdit
	call	ForEachCP		;ModuleRudeEdit returns no error codes
cEnd

cProc	SystemDescanRude,<PUBLIC,FAR>
cBegin
	call	SystemDescanRudeCP
cEnd


;**************************************************************
; ModuleRudeEdit()
; Purpose:
;	This is called after some edit has occurred which could have
;	made existing variable tables and/or pcode inconsistent.
;	For example, after a DEFINT statement is inserted in a text
;	table.  It descans to SS_RUDE all text tables in this module.
;	It then eliminates the module's variable table (which
;	holds all procedure variables for this module as well).
;
;	Resets the NM_fShared name table bit and for each name table
;	entry, maps NMSP_Variable to NMSP_UNDEFINED
;
; Exit:
;	This function never results in an error (not even out-of-memory)
;	ax = non-zero (so it can be called by ForEachCP
;	grs.fDirect = FALSE
;
;**************************************************************
cProc	ModuleRudeEdit,<PUBLIC,NEAR>
cBegin

	test	[mrsCur.MRS_flags2],FM2_NoPcode OR FM2_Include
	DJMP	jne  NoVarTbl		;brif this mrs has no variable tables

	test	[mrsCur.MRS_flags],FM_AllSsRude ;speed opt - skip if all module
	jnz	FreeVarTbl		;text tables are already in SS_RUDE

	test	[txdCur.TXD_flags],FTX_mrs
	je	GotSubOrFunc		;brif a SUB or FUNC txt tbl is active
	call	PrsDeactivate		;deactivate DEF FN or DECLARE (if any)
					; since DEF FNs are discarded.  This
					; lets us call ForEachCP with FE_SaveRs
					; without crashing when we try to
					; restore a discarded DEF FN.
GotSubOrFunc:
	call	VarDealloc		;must free all owners in module var
					;  table BEFORE rude descan, because
					;  rude descan resets the oVarHash field
					;  in each prs to UNDEFINED
	mov	[descanTo],SS_RUDE

	;descan each prs in a module before descanning the module itself
	mov	al,FE_CallMrsAfter+FE_PcodePrs+FE_SaveRs
	mov	bx,CPOFFSET DoDescan
	call	ForEachCP		;DoDescan can return no error codes

;start of revision [39]
	;Note that the below scheme depends on the fact that NextPrsInMrs
	;finds the first prs in the table if grs.GRS_oPrsCur is UNDEFINED, and
	;then subsequent prs's based on grs.GRS_oPrsCur.  It is not safe to
	;call ForEachCP to do this, because that scheme depends on walking
	;the prs chain, and PrsFree causes PrsCur to be Unlinked.  The next
	;call ForEach pass uses the unlinked entry, and thinks that it must
	;be looking at prsCur.
	;In essense we are starting from the top of the prs chain each time
	;through the loop below after freeing a Def Fn. If not a Def Fn we
	;walk from prs to prs, not freeing them.
	push	[grs.GRS_oRsCur]		;remember oRsCur for reacivation
FreeDefFn_Loop:
	call	far ptr NextPrsInMrs		;activate next prs in this mrs
	inc	ax				;no more prs's in this module?
	jz	FreeDefFn_Done			;  brif so
	cmp	[prsCur.PRS_procType],PT_DEFFN
	jne	FreeDefFn_Loop			;brif not a DEF FN (must be
						; DECLARE, SUB, or FUNCTION)
	call	PrsFree 			;release DEF FN's prs entry
	jmp	short FreeDefFn_Loop		; resets grs.oRsCur to UNDEFINED
FreeDefFn_Done:
	cCall	RsActivate			;reactivate oRsCur - already
						; on stack
;end of revision [39]

	mov	al,NOT (NM_fShared OR NMSP_Variable)
	call	ResetTNamMask
FreeVarTbl:
	call	VarRudeReset		;erase module's variable & type tables
NoVarTbl:
	or	[mrsCur.MRS_flags],FM_AllSsRude ;all tables are now SS_RUDE
	SetfDirect al,FALSE		;turn off Direct mode
	mov	ax,sp			;return non-zero (for ForEachCP)
cEnd

cProc	ModuleRudeEditFar,<PUBLIC,FAR>
cBegin
	call	ModuleRudeEdit
cEnd

;**************************************************************
; TxtDescan()
; Purpose:
;	Descan the current text table to SS_PARSE in preparation
;	for an edit (i.e. a call to TxtDelete or TxtChange).
;	If it is a module's text table being descanned, all procedures
;	within the module are descanned as well, because they could
;	contain text offsets into module's text table which are now
;	invalid.  For example, RESTORE <label>, ON ERROR GOTO <label>,
;	ON <event> GOSUB <label>.
;
;**************************************************************
cProc	TxtDescanCP,<PUBLIC,NEAR>
cBegin
	mov	[descanTo],SS_PARSE
	test	[txdCur.TXD_flags],FTX_mrs
	je	ProcOnly	;brif descanning a procedure text table
	mov	al,FE_PcodePrs+FE_SaveRs
	mov	bx,CPOFFSET DoDescan
	call	ForEachCP		;DoDescan can return no error codes
ProcOnly:
	call	DoDescan
cEnd

;**************************************************************
; TxtModified()
; Purpose:
;	Descan the current text table to SS_PARSE in preparation
;	for an edit (i.e. a call to TxtDelete or TxtChange).
;	It also sets FM2_Modified bit in current module, so user
;	will be prompted to save it before next NEW or LOAD
; Exit:
;	current module's fModified bit is set TRUE
;
;**************************************************************
cProc	TxtModified,<FAR,PUBLIC>		
cBegin	TxtModified				
	test	[mrsCur.MRS_flags2],FM2_File
	je	TmExit			;brif this mrs has no FILE
TmMod:
	or	[mrsCur.MRS_flags2],FM2_Modified or FM2_ReInclude 
					;This call is always followed
					; by a call to TxtChange/TxtDelete
TmExit:
	jmp	SHORT StartTxtDescan		
TxtModified ENDP

cProc	TxtDescan,<FAR,PUBLIC>			
cBegin	TxtDescan				
StartTxtDescan:					
	call	TxtDescanCP		;far to near call gate
cEnd	TxtDescan				

;*********************************************************************
; AskCantCont()
;
; Purpose:
;  AskCantCont() is called by TextMgr when it is about to make an
;  edit which would prevent continuing program execution.
;  This routine can not be called during execution.
;  If already impossible to continue (i.e. grs.otxCONT ==
;     UNDEFINED) AskCantCont returns TRUE.  Otherwise, the user is warned
;     with a dialog box that this edit will prevent continuing.
;  If the user says OK, grs.otxCONT is set to UNDEFINED
;     and the context manager's CantCont() is called (which
;     sets grs.otxCONT to UNDEFINED among other things.
;     AskCantCont() then returns TRUE.
;  If the user says CANCEL, the Debug screen is refreshed (discarding
;     the current edit) and AskCantCont() returns FALSE.
;
; Exit:
;  Returns FALSE if user wants to abort current edit, with
;  condition codes set based on value in ax.
;
;*********************************************************************
cProc	AskCantCont_CP,<PUBLIC,NEAR>
cBegin
	call	AskCantCont
	or	ax,ax			;set condition codes for caller
cEnd	;AskCantCont_CP


;**************************************************************
; AskRudeEdit
; Purpose:
;	Ask if user wants to back out of what will be a RUDE edit
; Note:
;	This function can cause heap movement
; Exit:
;	If user wants to back out, ax = 0
;	else ModuleRudeEdit is performed, ax = nonzero
;	condition codes set based on value in ax
;
;**************************************************************
cProc	AskRudeEdit,<PUBLIC,NEAR>
cBegin
	call	AskCantCont_CP		;ask user "Want to back out?"
	je	AskRudeExit		;brif user wants to back out of edit
	call	ModuleRudeEdit		;descan module to SS_RUDE, discard
					; module's variable & type tables
	mov	ax,sp
AskRudeExit:
	or	ax,ax			;set condition codes for caller
cEnd

;Far gateway to AskRudeEdit
cProc	AskRudeEditFar,<PUBLIC,FAR>
cBegin
	call	AskRudeEdit
cEnd

;**************************************************************
; UpdatePcs(otxEditStart, cbIns, cbDel, fTestOnly)
; Purpose:
;	Update program counter due to the insertion or deletion of text.
;	If pc is deleted, AskCantCont.
;	If pc moves (because of insert/delete),
;	   and fTestOnly=FALSE, update the pc.
; Entry:
;	grs.oRsCur identifies text table being edited
;	otxEditStart = offset into text table to 1st byte inserted/deleted
;	cbIns = # bytes inserted
;	cbDel = # bytes deleted
;	fTestOnly = non-zero if we're testing for Edit & Continue
;	   not really updating pc
;
; Exit:
;	Carry is set if edit would prevent CONT
;
;NOTE: exit conditions of UpdatePcs never return
; with carry set.  Some code could be saved.
;
;**************************************************************
cProc	UpdatePcs,<PUBLIC,NEAR>,<si>
	parmW	otxEditStart
	parmW	cbIns
	parmW	cbDel
	parmW	fTestOnly
cBegin
	call	ORsCurTxtTbl		;ax = oRs of current text table
	cmp	ax,[grs.GRS_oRsContTxtTbl]
	jne	UpcUnaffected		;brif edit didn't affect PC
	mov	ax,[grs.GRS_otxCONT]	;ax = current program counter
	inc	ax			;test for UNDEFINED
	je	UpcUnaffected		;brif can't continue
	dec	ax			;restore ax = otxCONT
	mov	dx,[otxEditStart]
	cmp	dx,ax
	je	SetToBol
	ja	UpcUnaffected		;brif PC was below edit (unaffected)

	;This edit is having an effect on the current instruction pointer
	add	dx,[cbDel]		;dx points beyond end of delete
	cmp	dx,ax
	jbe	UpcNotDel		;brif PC wasn't deleted by edit
SetToBol:
	mov	ax,[otxEditStart]	;Reset program counter to start
					; of edited line
	jmp	SHORT UpcUpdated

;line with program counter has been moved up or down in memory
UpcNotDel:
	add	ax,[cbIns]
	sub	ax,[cbDel]
UpcUpdated:
	cmp	[fTestOnly],FALSE
	jne	UpcUnaffected		;brif just testing for Edit & Cont
	mov	[grs.GRS_otxCONT],ax
UpcUnaffected:
	clc				;indicate no error
UpcExit:
cEnd

;*************************************************************************
; ORsCurTxtTbl
; Purpose:
;	Get oRs of current text table.  Only time this is different from
;	grs.oRsCur is when grs.oRsCur is for a DEF FN (which uses module's
;	text table).
;
; Exit:
;	ax = oRs of current text table.
;
;*************************************************************************
cProc	ORsCurTxtTbl,<PUBLIC,NEAR>
cBegin
	mov	ax,[grs.GRS_oMrsCur]	;ax = oRs of module's text table
	test	[txdCur.TXD_flags],FTX_mrs
	jne	OctExit			;brif module's txt tbl is active
	mov	ax,[grs.GRS_oRsCur]	;ax = oRs of procedure's text table
OctExit:
cEnd

;*************************************************************************
; OtxDefType(otx), OtxDefTypeCur, OtxDefType0, OtxDefTypeEot
;
; Purpose:
;	This causes  the text  manager to  traverse the linked
;	list of  DEFxxx statements  for the current text table
;	and accumulate	the current  state  for  a  particular
;	offset into  the  text	table.	  If  the  text  table
;	contains no  DEFxxx statements,  on exit, the array is
;	filled with 26 * ET_R4 (ET_R8 for EB).
;	A opStDefType opcode looks like:
;
;		<opStDefType><link field><high-word><low-word>
;	where
;		<high-word> has 1 bit set for each letter from A..P
;		<low-word> has 1 bit set for each letter from Q..Z in the
;			high bits, and type (ET_I2..ET_SD) in the low 3 bits.
;
; Entry:
;	OtxDefType, OtxDefTypeCur: ax = otx - byte offset into text table
;	OtxDefType: bx = pointer to table of 26 bytes to be filled
;
; Exit:
;	OtxDefType: parm2's table is filled with result
;	OtxDefTypeCur: fills tEtCur with result
;	OtxDefType0: fills tEtCur with ET_R4 (ET_R8 for EB)
;	OtxDefTypeEot: fills tEtCur with the default types at the end
;	   of the current text table.
;	grs.fDirect is preserved in all cases
;
;*************************************************************************
PUBLIC	OtxDefTypeEot
OtxDefTypeEot PROC NEAR
	mov	ax,[txdCur.TXD_bdlText_cbLogical] ;go until end-of-text
	SKIP2_PSW			;skip following sub ax,ax
OtxDefTypeEot ENDP

OtxDefType0 PROC NEAR
	SetStartOtx ax			;ax = start of text
OtxDefType0 ENDP

PUBLIC	OtxDefTypeCur
OtxDefTypeCur PROC NEAR
	mov	bx,dataOFFSET ps.PS_tEtCur
OtxDefTypeCur ENDP

PUBLIC	OtxDefType

cProc	OtxDefType,<NEAR>,<si,di>
	localW	EndOtx
	localW	EtTable
cBegin	OtxDefType
	mov	[EndOtx],ax		;initialize Endotx for loop
	mov	[EtTable],bx		;init ptr to top of table for loop


; Initialize table to all ET_R4 (ET_R8 for EB)
	mov	di,bx	 		;di -> type table
	push	ds			;need es=ds for rep stosb
	pop	es

	mov	cx,26			;26 letters in alphabet
	mov	al,ET_R4		;default type is single precision
	rep stosb


	DbChk	TxdCur			;perform sanity check on txdCur

	;Now go through text table, altering table for each DEFxxx
	; NOTE: this need not be done if parser never builds var table entries
	
	GETSEG	es,[txdCur.TXD_bdlText_Seg],,<SIZE,LOAD>
	mov	bx,[txdCur.TXD_otxDefTypeLink]
					;bx points to start of this text
					; table's linked list of DEFxxx stmts
					; or = FFFF if linked list is empty
;bx = otxCur
DefLoop1:
	mov	ax,[EndOtx]		;ax = otx parm
	mov	di,[EtTable]		;di = ptr to table
	cmp	bx,ax			;see if we're beyond place of interest
	jae	DefTypeEnd		;branch if so

	mov	si,bx			;si points to next DefType link
					; or FFFF if end of linked list
	lods	WORD PTR es:[si]	;ax points to next DefType link
					; or = FFFF if end of linked list
	xchg	bx,ax			;bx points to next DefType link
	mov	dl,es:[si]		;dl = low byte of args
	and	dl,02FH			;dl = type (ET_I2..ET_SD)
	mov	cx,16			;examine 16 bits in 1st word
	mov	dh,1			;go through DefLoop2 twice
	mov	ax,es:[si+2]		;ax = high mask of bits
	jmp	SHORT DefLoop3

DefLoop2:
	lods	WORD PTR es:[si]	;ax = low mask of bits
DefLoop3:
	shl	ax,1
	jnc	BitNotSet		;brif bit not set for this letter
	mov	[di],dl 		;save type in type table
BitNotSet:
	inc	di			;advance to next entry in type table
	loop	DefLoop3		;advance to next bit in mask
	mov	cx,10			;examine 10 bits in 2nd word
	dec	dh			;test DefLoop2 flag
	je	DefLoop2		;brif need to do 2nd word
	jmp	SHORT DefLoop1		;advance to next DEFxxx stmt

DefTypeEnd:
cEnd	OtxDefType


cProc	OtxDefType0Far,<PUBLIC,FAR>	;added as part of revison [20]
cBegin
	call	OtxDefType0
cEnd

cProc	OtxDefTypeCurFar,<PUBLIC,FAR>	;added as part of revison [20]
	parmW	oTx
cBegin
	mov	ax,[oTx]
	call	OtxDefTypeCur
cEnd

;**********************************************************************
; EtDiff
; Purpose:
;	Determine the difference between two tables of ET_xxx's
;	Used by ASCII Load and ASCII Save for inserting DEFxxx statements
;	which let each procedure text table appear to be independant of
;	the module's text table's DEFxxx statements.
; Entry:
;	parm1 points to a table of 26 bytes, on ET_xxx for each letter
;	parm2 points to another table of 26 bytes, on ET_xxx for each letter
;	parm3 = ET_xxx
; Exit:
;	ax:dx = DEFTYPE bit mask, as would appear in opStDefType's operand,
;	representing the difference between table 1 and table2 with
;	respect to parm3's type.
; Example:
;	parm1 contains ET_I2, ET_I4, ET_I2, ET_R4, ..., ET_R8
;	parm2 contains ET_I4, ET_I4, ET_I4, ET_R4, ..., ET_R8
;	parm3 contains ET_I4
;	result = 0xA0000002
;
;**********************************************************************
cProc	EtDiff, <NEAR, PUBLIC, NODATA>,<si,di>
	parmW	tEtBase
	parmW	tEtNew
	parmB	etNew
cBegin	EtDiff
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	di,[tEtBase]		;di points to old deftype table
	mov	si,[tEtNew]		;si points to new deftype table
	mov	cx,26			;cx = repeat count (1 for each letter)
	sub	bx,bx			;init mask dx:bx to 0
	sub	dx,dx
EtCmpLoop:
	lodsb				;al = base type
	cmp	al,[di] 		;compare with new type
	je	NoDiff			;branch if no difference
	cmp	al,[etNew]		;compare with type we're interested in
	jne	NoDiff			;branch if we don't care about this type
	or	bl,20H			;set bit which represents Z+1
NoDiff:
	inc	di
	shl	bx,1			;shift dx:bx left 1
	rcl	dx,1
	loop	EtCmpLoop		;repeat for all letters a..z
	mov	ax,bx			;dx:ax = result
	or	bx,dx			;bx = high word ORed with low word
	je	EtDiffX 		;brif no difference between 2 tables
					; with respect to type etNew
	or	al,[etNew]		;dx:ax = opStDefType's operand
EtDiffX:
cEnd	EtDiff



;--------------------------------------------------------------
;	Re-Parsing Functions
;
; Many errors are ignored at edit-time, on the assumption that
; the user will repair the problem before attempting to execute.
; The general solution when one of these errors are encountered
; at edit time is to store the entire source line in the pcode
; within an opReParse opcode.  Then, when the user is about to
; execute, we re-parse these lines and report any errors encountered
; with the following call-tree:
;
;                       ReParseTbl
;                           |
;                      +----+-----+
;                      |          |
;                  DoReParse  PreScanAsChg
;                      |
;                 TxtReEnter
;                      |
;                  +---+---+
;                  |       |
;              ListLine TxtChange
;
;--------------------------------------------------------------

;**************************************************************
; TxtReEnter, TxtReEnterBol
; Purpose:
;	Convert a line of pcode to source, parse it to pcode
;	and replace old pcode with new pcode.  This is done
;	for each line in the ReParse list before execution.
;	It is also done for each $INCLUDE line for FILE/REINCLUDE menu.
; Entry:
;	ax = offset into current text table anywhere within source line
;	     (points to opBol opcode for TxtReEnterBol variant)
; Exit:
;	same as for TxtChange
; Alters:
;	ps.bdpSrc (parser's source buffer)
;
;**************************************************************
PUBLIC	TxtReEnter
TxtReEnter PROC NEAR
	DbChk	Otx,ax			;error if ax > txdCur.bdlText.cbLogical
	push	ax
	call	OtxBolOfOtx		;ax = otx for start of INCLUDE line
	;fall into TxtReEnterBol
TxtReEnter ENDP

TxtReEnterBol PROC NEAR
	DbChk	Otx,ax			;error if ax > txdCur.bdlText.cbLogical
	push	ax			;pass otx to start of line to TxtChange

	push	ax			;pass otx to ListLine
	PUSHI	ax,<DATAOFFSET ps.PS_bdpSrc>	;pass dst adr to ListLine
	call	ListLine
	inc	ax			;test for UNDEFINED
	je	TrOmErr			;brif out-of-memory
	dec	ax			;restore ax = cb

	;If this line was included, set cInclNest non-zero, so parser
	;will generate an opBolInclude instead of an opBol for this line
	
	mov	al,[fLsIncluded]
	mov	[cInclNest],al

					;otxStart pushed several lines above
	push	[otxListNextInc]	;pass offset beyond end of line
	sub	ax,ax			;clear fNoInsert flag
	push	ax			;We have txt to insert
	call	TxtChange
	mov	[cInclNest],0		;restore: we know we're not loading
					; an INCLUDE file
TrExit:
	ret
TxtReEnterBol ENDP

;Out-of-memory error
TrOmErr:
	mov	ax,ER_OM		;ax = std error code for out-of-memory
	mov	[txtErr.TXER_errCode],ax
	jmp	SHORT TrExit

;**************************************************************
; DoReParse
; Purpose:
;	Re-Parse the 1st line after a given point in the current
;	tables 'reparse' list.
; Entry:
;	ax = text offset to start looking for lines to reparse
;	bx = text offset to stop looking for lines to reparse
; Exit:
;	ax = same as for TxtChange
;	If line was reparsed without errors
;	   carry clear on exit
;	else
;	   carry set on exit
;	   if error occurred (i.e. didn't just reached end of reparse list)
;	      txtErr struct is filled in.
;
;**************************************************************
PUBLIC	DoReParse
DoReParse PROC NEAR
	push	si			;save caller's si
	mov	si,[txdCur.TXD_otxReParseLink] ;si = otxLink
	xchg	dx,ax			; dx = starting otx, ax = garbage
	GetSegTxtTblCur			;es = seg addr of current text tbl
DrNext:
	sub	ax,ax			;prepare to return 0 (no TxtChange err)
	cmp	si,bx
	jae	DrEOL			;brif end of reparse list
	cmp	si,dx			;compare with otxStart parm
	jae	DrAbove
	mov	si,es:[si]		;si points to next opReParse link
	jmp	SHORT DrNext		;brif below otxStart
DrEOL:
	DJMP	jmp SHORT DrEndOfList

;si = otxLink = text offset to next re-parse opcode's link field.
;List this line and re-parse it.
;
;NOTE: It is real tempting to just call OtxBolOfOtx to get the
; offset to the start of the reparse line.  This SIGNIFICANTLY
; slows down paste.  We are guaranteed that the only opcodes that
; we may see before an opReParse are opBol, opBolInclude, opBolLab, and
; opBreakPoint.  Therefore the following opcode sequences could be seen:
;			  opBol  [opBreakPoint]
;	     opBolInclude(depth) [opBreakPoint]
;    opBolLab(otxNextLink, oNam) [opBreakPoint]
;
; The following code assumes the following:
;    opBol = 0
;    depth for include files is [1-5].
;    oNam for opBolInclude > 5
;    oTxNextLink > opBolInclude
;

DrAbove:
	mov	bx,-6			;amount to backup past count and
					; opcode field for opReparse

	mov	ax,si			;pass otxLink in ax
	cmp	word ptr es:[si+bx],opBreakPoint ;is there a BP set at the start
					; of this line?
	jne	DrGotBol		;brif not, already at opBol
	dec	bx			;back up past opBreakPoint
	dec	bx			; to opBol

; Parser guarantees only opBol, opBolLab, or opBolInclude before an opReparse.

DrGotBol:
	mov	cx,es:[si+bx]		;cx = opcode
	and	cx,OPCODE_MASK		;upper bits = leading spaces
	.errnz	opBol
					;have a standard opBol?
	jcxz	DrReenter		;brif so, reenter line

; We must have a line starting with opBolInclude, or opBolLab
	DbAssertRel oNamFirst,a,INCLUDE_DEPTH_MAX,CP,<DoReParse: err2>
	dec	bx
	dec	bx			;back up over include depth/oNam
	mov	cx,es:[si+bx]		;cx = opcode
	and	cx,OPCODE_MASK		;upper bits = leading spaces

	DbAssertRel opBolInclude,be,5,CP,<DoReParse: err3>
	cmp	cx,opBolInclude 	;is this an opBolInclude?
	je	DrReenter		;brif so

; We must have a line starting with opBolLab
	dec	bx
	dec	bx			;back up over include otxNextLink

DrReenter:
	add	ax,bx			;back up to bol opcode

	call	TxtReEnterBol		;reparse line ax
	cmp	[txtErr.TXER_errCode],0 ;tell caller a line was successfully
					; reparsed
	je	DrExit			;brif no error (carry is clear)
DrEndOfList:
	stc				;return carry set (no reparse, or error)
DrExit:
	pop	si			;restore caller's si
	ret
DoReParse ENDP

;**************************************************************
; ReParseTbl
; Purpose:
;	Called by SystemScan via ForEachMrs.  Finds all lines
;	that contain opReParse, and re-parses them until current
;	module contains no opReParse opcodes.
;	Caller should reset FM_asChg bit in mrsCur.MRS_flags
;	after all text tables in this module have no opReParse opcodes.
; Exit:
;	Same as ScanTxtTbl
;
;**************************************************************
cProc	ReParseTbl,<PUBLIC,NEAR>,<si>
cBegin

RptLoop0:
	;change any pcode which results from insertion/deletion of
	;AS clauses (due to their affect on ids with .)
	
	mov	al,[mrsCur.MRS_flags]
	mov	si,ax			
	and	[mrsCur.MRS_flags],NOT FM_asChg

RptLoop1:
	test	si, FM_asChg		
	je	NoAsChg			;brif no 'x AS' clauses have been
					; inserted or deleted in module.
	call	PreScanAsChg		;Re-Parse every line in current
					; text table with AS <user type>
					; or any non-record var with a
					; period in its name.
NoAsChg:
	call	ReParseTxdCur		
	jz	RptExit			
	call	NextTextPrsInMrs	;activate next procedure in module
	inc	ax
	jne	RptLoop1		;brif not at end of proc list
	dec	ax			;ax = UNDEFINED (return TRUE)
	test	[mrsCur.MRS_flags],FM_asChg
	jne	RptLoop0		;brif ReParse loop made added any
					; X AS clauses.
RptExit:
cEnd


;**************************************************************
; ReParseTxdCur
; Purpose:
;	Finds all lines in txdCur that contain opReParse,
;	and re-parses them.
;
;	This code was split out of ReParseTbl in revision [32].
; Exit:
;	Same as ScanTxtTbl
;	NZ and AX != 0	- Ok
;	Z and AX == 0	- Fail
;
;**************************************************************
cProc ReParseTxdCur,<NEAR,PUBLIC>,<DI>
cBegin
	mov	di,[grs.GRS_oRsCur]	;remember current oRs

;Now re-parse every line in current text table with opReParse opcode
;
RptLoop2:
	cmp	di,[grs.GRS_oRsCur]	;did we change our Rs?
	je	RptSameRs		;brif not

;A call to TxtRenEnter caused us to change to a new Rs.  This can only
; happen in the extremely rare case where a SUB or FUNCTION definition
; is a reparse (e.g. could already have one with the same name), and
; it can now be reparsed successfully, which causes a new Rs to be
; created and activated (e.g. the user Deleted the duplicate SUB or
; function before trying to execute).  In this case, we reactivate
; the original Rs continue reparsing it to completion.	If we don't
; do this, we could try to scan a text table containing reparses.
;
	extrn	RsActivateIfNotFree:near
	mov	ax,di			;ax = oRs to activate.
	call	RsActivateIfNotFree	;reactivate previous oRs (in AX)
					; unless ParseLine just renamed

RptSameRs:
	mov	ax,[txdCur.TXD_otxReParseLink]
	inc	ax			;test for UNDEFINED
	je	RptDone			;brif done with ReParse lines
	dec	ax
	mov	bx,0FFFFh		;search until end of text table
	call	DoReParse		;parse next opReParse in this text tbl
	jnc	RptLoop2		;brif progress was made (i.e. a line
					; was re-parsed with no errors)
	;TxtReEnter encountered an error.  txtErr struct was set up by TxtChange
	sbb	ax,ax			;ax = -1

RptDone:
	inc	ax			;Return 0 for fail, 1 for Ok
cEnd



;--------------------------------------------------------------
;		INCLUDE file support functions
;--------------------------------------------------------------

;**************************************************************
; SetViewInclude
; Purpose:
;	Enable or disable the visibility of $INCLUDEd source lines
; Entry:
;	parm uchar fEnable = TRUE if lines are to be visible
;
;**************************************************************
cProc	SetViewInclude,<PUBLIC,FAR>
	parmB	fEnable
cBegin
	mov	al,[fEnable]
	mov	[fViewInclude],al
	call	TxtFlushCache
cEnd

;**************************************************************
; TblReInclude
; Purpose:
;	Called for each text table to re-invoke $INCLUDE directives
;	in this text table.
; Exit:
;	returns ax = 0 if fatal error occurred (like out-of-memory,
;	or File-not-found) else returns non-zero
;
;**************************************************************
cProc	TblReInclude,<PUBLIC,NEAR>
cBegin
	PUSHI	ax,0
	call	CmdViewInclude		;make $INCLUDEd lines invisible
	DbAssertRelB [fViewInclude],e,0,CP,<TblReInclude fViewInclude != 0>
	and	[mrsCur.MRS_flags2], NOT FM2_ReInclude 
	sub	ax,ax
TblLoop:
	push	ax			;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpReInclude>
	call	TxtFindNextOp		;ax = otx to next op_Include ($INCLUDE)
					;		(or OpStInclude [01])
					;dl = [txtFindIndex]
	cmp	dl,RI_opEot
	je	TblDone			;brif done with this text table


	;NOTE: next 3 lines must remain contiguous
	push	ax			;pass otx to OtxNoInclude
	call	TxtReEnter		;re-evaluate it, ignoring all errors
	call	OtxNoInclude		;ax = offset to next opBol/opEot
					; which was not included
	cmp	[txtErr.TXER_errCode],ER_OM
	jne	TblLoop			;brif TxtReEnter was successful
	sub	ax,ax			;return 0 (out-of-memory return code)
	jmp	SHORT TblExit

TblDone:
	sub	ax,ax
	mov	[txtErr.TXER_errCode],ax ;any errors encountered are in the
					; form of opReParse
	dec	ax			;return TRUE (non-zero)
TblExit:
cEnd

;**************************************************************
; TxtReInclude
; Purpose:
;	Called when user tries to execute a program.  If no loaded
;	INCLUDE files have been modified since last saved, it just
;	returns.  Else, it re-invokes the $INCLUDE directives in all
;	loaded text tables.  Errors are not reported, but reparsed.
;	The user will see any errors when he attempts to execute.
; Entry:
;	none
; Exit:
;	grs.fDirect = FALSE
;	ax = txtErr.errCode = error code if any error occurred while loading
;
;**************************************************************
cProc	TxtReInclude,<PUBLIC,FAR>
cBegin
	mov	[txtErr.TXER_errCode],0
	test	[flagsTm],FTM_reInclude
	je	NoReInclude
	and	[flagsTm],NOT FTM_reInclude
	call	SystemDescanCP		;descan all tables to SS_PARSE
	mov	al,FE_PcodeMrs+FE_CallMrs+FE_PcodePrs+FE_SaveRs
	mov	bx,CPOFFSET TblReInclude
	call	ForEachCP
NoReInclude:
	mov	ax,[txtErr.TXER_errCode]
cEnd

;*********************************************************************
; TxtFLnIncluded
; Entry:
;	parm1 = ln
; Exit:
;	ax = 0 if line was from $INCLUDE file
;
;*********************************************************************
cProc	TxtFLnIncluded,<PUBLIC,FAR>
	parmW	ln
	localV	bdBuf,<size BD>
cBegin
	push	[ln]			;pass line # to OtxOfLn
	call	OtxOfLn			;ax = otx for line
	push	ax			;pass otx to ListLine
	lea	bx,bdBuf		;pass pbd to ListLine
	mov	[bx.BD_pb],DATAOFFSET bufStdMsg
	mov	[bx.BD_cbLogical],0
	push	bx			;can't use bdpSrc, because editor
					; may have dirty copy of a line in it.
	call	ListLine		;set fLsIncluded for current line
					; since cbLogical is < 80, ListLine will
					; not try to grow buffer (it assumes
					; it is static). We just need the
					; 1st pass of the lister to execute,
					; not Stage2, since fLsIncluded is
					; set up during Stage1.
	mov	al,[fLsIncluded]
	cbw				;ax = result
cEnd

;*********************************************************************
; ushort TxtViewIncl(lnIncl, fDoIt)
; Purpose:
;	Called when user selects View/Include while insertion point
;	is on a line that has $INCLUDE in it.  This function creates
;	a new mrs by that name, and loads the file into that mrs.
; Entry:
;	grs.oRsCur is loaded with active window's register set.
;	lnIncl = the current line number in the active list window.
;	fDoIt = TRUE if the file is to actually be loaded (FALSE
;	   if we're just checking to see if current line contains $INCLUDE)
; Exit:
;	If fDoIt was FALSE on entry
;          if the line contains no $INCLUDE opcode, the function returns
;	      0
;	   else
;             it returns non-zero
;	   if lnIncl came from an $INCLUDE file, fLsIncluded is set
;	      non-zero, else it is set to 0.
;	else
;	   exit conditions are the same as LoadFile
; Alters:
;	ps.bdpDst (parser's pcode result buffer)
;
;*********************************************************************
cProc	TxtViewIncl,<PUBLIC,FAR>,<si>
	parmW	lnIncl
	parmW	fDoIt
	localV	filenameInc,FILNAML64
	localV	sdFilenameInc,<SIZE SD>
	localV	bdBuf,<size BD>
cBegin
	lea	si,sdFilenameInc
	lea	ax,filenameInc
	mov	[si.SD_pb],ax
	sub	ax,ax
	mov	[si.SD_cb],ax
	mov	[bdBuf.BD_cbLogical],ax ;pass cbMax to ListLine
					; since it is < 80, ListLine will
					; not try to grow buffer (it assumes
					; it is static). We just need the
					; 1st pass of the lister to execute,
					; not Stage2, since psdLsIncl is
					; set up during Stage1.
	mov	[bdBuf.BD_pb],DATAOFFSET bufStdMsg
					;pass ptr to buffer to ListLine

	push	[lnIncl]		;pass current line
	call	OtxOfLn			;ax = text offset to start of line
	push	ax
	call	OtxNoInclPrev		;ax = otx of $INCLUDE line if this
					; line is from an include file

	push	ax			;pass text offset
	lea	ax,bdBuf
	push	ax			;can't use bdpSrc, because editor
					; may have dirty copy of a line in it.
	mov	[psdLsIncl],si		;causes ListLine to load sdFilenameInc
	call	ListLine		; with arg to $INCLUDE
	sub	ax,ax
	mov	[psdLsIncl],ax
	cmp	[fDoIt],ax
	mov	ax,[si.SD_cb]		;ax = non-zero if $INCLUDE was found
	je	TviExit			;return if fDoIt was FALSE

	;should never be called with fDoIt true if line contains no $INCLUDE
	DbAssertRel ax,ne,0,CP,<TxtViewIncl: err2>
	push	si			;pass &sdFilenameInc
	PUSHI	ax,LF_ViewIncl		;pass file type
	inc	[fLoadInclude]		; inform LoadFile of $INCLUDE
	call	LoadFile		;ax = error code
	dec	[fLoadInclude]		; reset flag
TviExit:
cEnd

;**************************************************************
; SetCompSwitches
; Purpose:
;	Called by user interface when it is about to invoke
;	Compiler to produce an EXE file.  This function
;	scans the current text table's pcode, and sets the
;	following masks in the global static variable
;	'compSwitches' as follows:
;	  COMP_SW_E is set for each module which has ON ERROR stmt
;	  COMP_SW_X is set for each module which has RESUME [NEXT] stmt
;	  COMP_SW_V is set if any module has ON <event> stmt
;	  COMP_SW_W is set if any module has ON <event> stmt
;
; Exit:
;	ax is always non-zero (so it can be called by ForEach...)
;
;**************************************************************
cProc	SetCompSwitches,<PUBLIC,FAR>
cBegin
	call	B$GetCompSwitches	; AX nonzero if QLB used /V or /W
	or	ax,ax			
	jz	ScLoop			;brif no /V or /V in QLB
	or	[compSwitches],COMP_SW_V + COMP_SW_W ;set /V/W switches

	sub	ax,ax			;start at text offset 0
ScLoop:
	push	ax			;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpCompSw>
	call	TxtFindNextOp		;ax = otx to next opcode of interest
					;dl = txtFindIndex
	cmp	dl,CSW_opEot
	je	ScExit			;brif reached opEot

	mov	bl,COMP_SW_E
	.errnz	CSW_opStOnError
	or	dl,dl
	je	ScOrMask

	mov	bl,COMP_SW_V + COMP_SW_W
	cmp	dl,CSW_EventMax
	jbe	ScOrMask		;brif ON <event>  or  <event> ON

;	If we fall through to here we have some sort of RESUME statement.
;	All variants except one require /X.  The exception is opStResume
;	with an operand other than UNDEFINED.  In this case /E is sufficient,
;	/X is overkill.  Because generated code for /E is significantly
;	smaller than /X we check for that special case here.

	mov	bl,COMP_SW_X		; assume not special case
	cmp	dl,CSW_opStResume	; found an opStResume?
	jne	ScOrMask		; no, can't be special case
	GetSegTxtTblCur 		; es = seg adr of current txt tbl
	xchg	ax,bx			; es:bx = oTx of opStResume
	cmp	word ptr es:[bx+2], UNDEFINED ; "RESUME 0" statement?
	xchg	ax,bx			; bl = /X again
	je	ScOrMask		; brif it's "RESUME 0", need /X
	mov	bl,COMP_SW_E		; got it! only need /E
ScOrMask:
	or	[compSwitches],bl
	jmp	SHORT ScLoop

;we know ax is non-zero because there is always an opEndProg before an opEot
; in every text table
ScExit:
cEnd

;**************************************************************
; ONamOVarRudeOrParse
; Purpose:
;	Get the oNam for a variable
; Entry:
;	parm1 = oNam if txdCur.scanState == SS_RUDE
;	      = oVar if txdCur.scanState == SS_PARSE
; Exit:
;	ax = variable's oNam
;
;**************************************************************
cProc	ONamOVarRudeOrParse,<FAR,PUBLIC> 
	ParmW	oNamoVar		
cBegin	ONamOVarRudeOrParse		
	mov	ax,[oNamoVar]		
	DbChk	ConStatStructs		;ensure static structures
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jae	NoVarConv		; brif table is in rude-edit state
	DbChk	oVar,ax
	xchg	ax,bx			;bx is now the oVar
	add	bx,[mrsCur.MRS_bdVar.BD_pb] ;bx points to entry for variable
	mov	ax,VAR_oNam[bx] 	;fetch oNam from variable table
NoVarConv:				;ax = variable's oNam
	DbChk	oNam,ax			;pass oNam from op[A]Idxxx opcode
cEnd	ONamOVarRudeOrParse		


;--------------------------------------------------------------
; Functions which let identifiers with periods (like A.B)
; be either a scalar/array (for compatibility) or a record
; element (to support records with C-like syntax).
; In some future version of BASIC, it is conceivable that
; A.B will always mean 'element of record'.
;--------------------------------------------------------------

;**************************************************************
; ONamOfAs
; Purpose:
;	Given a pointer to an  opAsType(oNamTyp) op[A]VtRef(oNamId),
;	which is produced for syntax like 'id as user-type' in a DIM
;	type statement,	return the oNam for id.
; Entry:
;	ax = text offset into current text table to opAsType pcode
;	current text table's scan state is either SS_RUDE or SS_PARSE
; Exit:
;	carry is set if opAsType is from expression in TYPE/END TYPE block
;	else ax = oNam of id
;
;**************************************************************
ONamOfAs PROC NEAR
DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<ONamOfAs: bad scanState>
	xchg	bx,ax			;bx = text offset
	GetSegTxtTblCur			;[42]es = seg addr of current txt tbl
	cmp	WORD PTR es:[bx-4],opElemRef 
	stc				;assume we are in a TYPE block
					;note JE ONLY checks psw.z.
	je	OaExit			;brif in TYPE/END TYPE block
	
	mov	ax,es:[bx+6]		;ax = opcode following opAsType
	mov	dx,es:[bx+8]		;dx = oNam/oVar operand if scalar
.erre	opIdLd LT opVtRf		
.erre	opAIdLd GT opVtRf		
.erre	opAVtRf	GT opVtRf		
	cmp	ax,opVtRf		
	jle	OaGotONam		;brif got opVtRf or opIdLd
	mov	dx,es:[bx+10d]		;dx = oNam/oVar operand for array

OaGotONam:
	push	dx			;pass parm = oNam/oVar
	call	ONamOVarRudeOrParse 	;ax = variable's oNam
	clc
OaExit:
	ret
ONamOfAs ENDP


;**************************************************************
;ChkAsInTbl
;Purpose:
;	Checks to see if there are any refs to oNam AS in this
;	text table, for a particular oNam.  Any pcode within
;	a range which is being deleted is ignored.
;	If oNam AS is found within a TYPE block, it is ignored,
;	because type elements have their own name space.
;Entry:
;	oNamAs - oNam in question
;	oRsAs - identifies text table where text is being deleted.
;	otxAsDelStart - offset to 1st byte being deleted
;	otxAsDelEnd - offset to last byte being deleted
;Exit:
;	If oNam AS is found, returns 0
;
;**************************************************************
ChkAsInTbl PROC NEAR
	push	si			;preserve caller's si
	sub	si,si			;initial text offset = 0
CiLoop:
	push	si
	PUSHI	ax,<CODEOFFSET tOpAsType>
	call	TxtFindNextOp		;ax points to next opcode of interest
	cmp	dl,ASTYPE_opEot
	je	CiDone			;brif done with text table
	mov	si,ax			;si points to current opcode
	mov	cx,[oRsAs]
	cmp	cx,[grs.GRS_oRsCur]
	jne	CiCountIt		;brif it's not being deleted
	cmp	ax,[otxAsDelStart]
	jb	CiCountIt		;brif it's not being deleted
	cmp	ax,[otxAsDelEnd]
	jb	CiLoop			;brif it's being deleted
CiCountIt:
	cmp	dl,ASTYPE_opStDefFn
	jb	CiNotDeclare		;brif its not SUB/FUNC/DEFFN/DECLARE op

;Walk through parm list seeing if there are any oNam AS ... in parm list
	push	si			;save ptr to opStDeclare/Sub/Func
	jne	CiNotDefFn
	inc	si			;skip link field
	inc	si
CiNotDefFn:
	add	si,DCL_cParms+2		;si points to parm count field
	GETSEG	es,[txdcur.TXD_BDLTEXT_SEG] 
	lods	WORD PTR es:[si]	;ax = parm count (UNDEFINED same as 0)
	xchg	cx,ax			;cx = parm count
CiParmLoop:
	dec	cx
	js	CiParmDone		;brif done with parm list
	lods	WORD PTR es:[si]	;ax = parm's oNam/oVar
	xchg	bx,ax			;bx = parm's oNam/oVar
	lods	WORD PTR es:[si]	;ax = parm's atr flags
	xchg	dx,ax			;dx = parm's atr flags
	lods	WORD PTR es:[si]	;ax = parm's oTyp (oNam of type if rude)
	test	dx,PATR_asClause
	je	CiParmLoop		;brif parm has no AS clause
	cmp	ax,ET_MAX
	jbe	CiParmLoop		;brif ANY,INTEGER,...,STRING
	xchg	ax,bx			;ax = parm's oNam/oVar
	cmp	[txtFindIndex],ASTYPE_opStDeclare
	je	CiDeclare		;brif we're looking at DECLARE parm
					; list - no oVars, just oNams
	push	ax			;pass parm1 = oNam/oVar
	call	ONamOVarRudeOrParse	;ax = variable's oNam
CiDeclare:
	sub	ax,[oNamAs]		;see if it is id of interest
	jne	CiParmLoop		;brif not
	pop	si			;restore ptr to opStDeclare/Sub/Func
	jmp	SHORT CiDone		;return 0 - terminates ForEach...

CiParmDone:
	pop	si			;restore ptr to opStDeclare/Sub/Func
	jmp	SHORT CiLoop

CiNotDeclare:
	call	ONamOfAs		;ax = oNam of id
	jc	CiLoop			;brif AS clause was within TYPE/END TYPE
	sub	ax,[oNamAs]		;see if it is id of interest
	jne	CiLoop			;brif not
					;return 0 - terminates ForEach...
CiDone:
	pop	si			;restore caller's si
	ret
ChkAsInTbl ENDP

;**************************************************************
; ChkLastAs
; Purpose:
;	Called when opAsType op[A]Idxxx(oNam) pcode sequence is deleted.
;	If there are no other refs to oNam AS in pcode, oNam's NM_fAs
;	name table bit is reset.
;	This causes lexer to know all future references to oNam.xxx are not
;	user-type record element references.
; Entry:
;	ax = oNam
; Exit:
;	carry set if no more  <oNam> AS  constructs exist in pcode
;
;**************************************************************
PUBLIC	ChkLastAs
ChkLastAs PROC NEAR
	mov	[oRsAs],UNDEFINED
ChkLastAs ENDP
ChkLastAs1 PROC NEAR
	mov	[oNamAs],ax		;pass oNam to ChkAsInTbl
	;scan all text tables in module to see if any more AS x
	mov	bx,CPOFFSET ChkAsInTbl
	call	ForEachTxtTblInMrs
	or	ax,ax			;test return value
	je	NotLastAs		;brif found an AS x
					; exit with carry clear
	mov	bx,[oNamAs]		;pass oNam in bx
	mov	al,NM_fAS		;pass mask for bit to be reset
	call	ResetONamMask
	stc				;set return code
NotLastAs:
	ret
ChkLastAs1 ENDP

;**************************************************************
; ChkLastAsText
; Purpose:
;	Same as ChkLastAs except for input parm.
;	Pcode is assumed to be in SS_RUDE state.
; Entry:
;	ax = text offset to opAsType opcode being deleted from
;	     current text table
;	bx = text offset to 1st byte being deleted
;	cx = text offset beyond last byte being deleted
;
; Pcode is:
;  For id AS type: opAsType(oNamType) opVtRf(oNamId)
;  For id(10,20) AS type: 10,20,opAsType(oNamType) opAVtRf(2,oNamId)
;  For typeElem AS type: opElemRef(oNamElem) opAsType(oNamType)
; si is pointing to opAsType in all cases
;
;**************************************************************
cProc	ChkLastAsText,<PUBLIC,NEAR>
cBegin
	;if PreScanAsChg is active, 'AS <userType>' is not really
	;being deleted, just re-parsed, so don't reset name table bit.
	
	cmp	[fPreScanAsChg],0
	jne	ClatExit

	;save ptr to opAsType being deleted, so ChkAsInTbl doesn't
	;count one being deleted when looking for other occurances
	;of  x AS <usertype>
	
	mov	[otxAsDelStart],bx
	mov	[otxAsDelEnd],cx
	mov	dx,[grs.GRS_oRsCur]
	mov	[oRsAs],dx
	call	ONamOfAs		;ax = oNam of x for  x as <type>
	jc	ClatExit		;brif not  x as <type> pcode
	call	ChkLastAs1
	jnc	ClatExit		;brif not last  x as <type>  in pcode
	or	[mrsCur.MRS_flags],FM_asChg
					;remember to call PreScanAsChg
					; before trying to scan/run program
ClatExit:
cEnd

;**************************************************************
; PreScanAsChg
; Purpose:
;	Called just before we scan a text table in preparation
;	for execution.  Only called when text table's FM_asChg
;	bit is set, so we know that either:
;	   an 'x AS y' clause has been inserted in this table, or
;	   an 'x AS y' clause has been deleted from this table and
;	      the module contained no more 'x AS <user type>' clauses
;	It walks through every pcode in current text table.
;	For every record element x.a, if x's NM_fAs bit is not set,
;	   the line is re-parsed.
;	If the line contains an 'opNoTyp' opcode, meaning the line
;	   contains an identifier other than a record element with
;	   a period in it's name, the line is re-parsed.
;
;**************************************************************
cProc	PreScanAsChg,<PUBLIC,NEAR>,<si,di>
cBegin
	mov	[fPreScanAsChg],1
	SetStartOtx si			;si = offset to start of text table
	mov	di,si			
PsLoop:
	push	si
	PUSHI	ax,<CODEOFFSET tOpAs>
	call	TxtFindNextOp		;ax = offset to next opcode of interest
					;dl = [txtFindIndex]
	xchg	si,ax			;si = offset to opcode
	cmp	dl,AS_opNoType
	ja	PsDone			;brif got opEot
.errnz	AS_opEot - AS_opNoType - 1
	je	PsReParse		;brif opcode is opNoType
	;else opcode must be opOff...
	xchg	ax,di			;ax = ptr beyond last opOffxxx opcode
	mov	di,si			;di = ptr to current opOffxxx opcode
	add	di,4			;di = ptr beyond current opOffxxx opcode
	DbAssertRel ax,be,si,CP,<PreScanAsChg: err2>
	cmp	si,ax
	je	PsLoop			;this is just an offset modifier, like
					; c in a.b.c
	GETSEG	es,[txdCur.TXD_bdlText_seg] 
	push	es:[si-2]		;push oNam/oVar from op[A]Idxxx opcode
	call	ONamOVarRudeOrParse	;ax = variable's oNam
	push	ax			;pass oNam from op[A]Idxxx opcode
	call	FlagOfONam		;al = oNam's flags
	test	al,NM_fAS
	jne	PsLoop			;brif valid record element
	;Else, what used to look like a record element should now be a simple id
PsReParse:
	push	si			;pass opcode offset to LnOfOtx
	call	LnOfOtx			;ax = current line #
	inc	ax			;ax = line after current line
	push	ax			;pass to OtxOfLn below
	mov	ax,si			;ax = offset to opcode of interest
	call	TxtReEnter		;re list & parse line @ ax
	call	OtxOfLn			;ax = offset to next opBol/opEot
					;     (Must be done after TxtReEnter)
	xchg	si,ax			;si = offset to next opBol/opEot
	jmp	SHORT PsLoop

PsDone:
	mov	[fPreScanAsChg],0
cEnd




sEnd	CP

;**************************************************************
;      Support for GetSegAddr MACRO
;
;**************************************************************

;seg_cp = segment address for the CP segment
;It can be referenced from any module as follows:
	;	EXTRN	Seg_CP:abs
	;	mov	ax,SEG Seg_CP
	; These statements are generated by the macro call:
	;       GetSegAddr CP

	Seg_CP	=	SEG ModuleRudeEdit
	PUBLIC	Seg_CP



assumes DS,DATA 			
sEnd	CP

end
