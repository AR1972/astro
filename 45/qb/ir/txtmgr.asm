;==========================================================================
;
;Module:  txtmgr.asm - Text Management Functions
;System:  Quick BASIC Interpreter
;
;=========================================================================

	.xlist
	include		version.inc
	TXTMGR_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	lister
	includeOnce	names
	includeOnce	opcodes
	includeOnce	parser
	includeOnce	pcode
	includeOnce	qbimsgs
	includeOnce	rtps
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	txtint
	includeOnce	ui
	includeOnce	util
	includeOnce	variable
	.list

assumes	ds,DATA
assumes	ss,DATA
assumes	es,NOTHING

;-----------------------------------------------------------------------
; Scoping of legal statements:
;
; Some statements are invalid in protected-direct mode (i.e. only
;   valid in a program or unprotected-direct-mode).  These are
;   caught by the parser's NtStatement() function which checks
;   a list of statements which ARE legal in protected direct mode
;   (a much smaller list than those which are illegal).
;
; Some statements are invalid in direct mode (i.e. only valid within
;   a program).  Those which can be identified by Reserved Word are
;   caught by the parser's NtStatement() function which checks the
;   'no_direct' flag as set in file 'bnf.prs'.
;   Those which can be identified by opcode are caught by TxtDirect()
;   when it finds entries for them in the table tOpDirect[].
;
; Some statements are only valid at module level.  This includes
;   DECLARE, DEF FNxxx, which are caught by the parser, and
;   END DEF, DATA which are caught by TxtChange.
;
; Some statements are only valid within a SUB/FUNCTION.  This includes
;   SUB/FUNCTION which is caught by the parser, and
;   END SUB/FUNCTION which is caught by TxtChange.
;
; Some opcodes are only valid after the defintion line of a SUB/FUNCTION.
;   These are caught by TxtChange.  Those which can precede a SUB/FUNCTION
;   include opBol, opBolSp, opBolInclude, opStRem, opQuoteRem,
;   opStDefType, opStInclude
;   (and op_Static, op_Dynamic in QB4 and the like [4]).
;
;-----------------------------------------------------------------------

; Typical text table:
; ------------------
;			         opBol:...
;    txdCur.otxLabLink---------->opBolLab(x)
;			         opBol:...
;				    :
;			         opBol:...
;				 opEndProg:...
;    				 opEot
;    txdCur.bdlText.cbLogical--->
; 
; 
; Text table with active Watch expressions:
; ----------------------------------------
; 			         opBol:...
;			 	    :
; 			         opBol:...
;    				 opEndProg
;			         <exp>:opWatchExp
;			         <exp>:opWatchStop
;			         <exp>:opWatchExp
;			         opEot
;    txdCur.bdlText.cbLogical--->
;
;----------------------------------------------------------------------------



sBegin	DATA

	extrn	fMergeInSub:byte	;non-zero if MERGING into SUB/FUNCTION
					;only valid when FLoadActive is TRUE

	extrn	b$SdBuf1:word		; SD to b$Buf1 (from runtime)

;see comments in txtint.inc for bigEditState flag values, and TxtStartBigEdit
;for further details.
;
DbPub	bigEditState
bigEditState DB BIG_EDIT_FALSE

cForDel		DB 0			;1+num of FOR stmts deleted by TxtDelete
cForStepDel	DB 0			;1+num of FOR ... STEP stmts deleted

;bdlTxtScrap holds deleted pcode in case the user wants to back out of the
;edit (for Edit & Continue).
;
PUBLIC	bdlTxtScrap			
bdlTxtScrap	BDL <0,NOT_OWNER,0,0> ;initializes status field to NOT_OWNER

;fFillScrap is FALSE if TxtDelete should not copy deleted text to bdTxtScrap
;	(see comments for Big Edit)
;
fFillScrap	DB 0

;otxScrapStart & otxScrapEnd represent the offsets into the current text table
;where pcode was deleted and copied into bdTxtScrap
;
otxScrap	DW 0

;otxBigIns and cbBigIns identify all pcode inserted since the BigEdit
;started.  If the user wants to back out of the BigEdit, they represent
;the text which needs to be deleted.
;
otxBigIns	DW 0
cbBigIns	DW 0

;fBindStatic is input parm to TxtBindPrs
;
fBindStatic	DB 0

;bindErrCode is result parm of TxtBindPrs
;
bindErrCode	DW 0

;text mgr flags - FTM_xxx
PUBLIC	flagsTm
flagsTm 	DB 0
flagsTm2	DB 0	;must immediately follow flagsTm
FTM2_PrsRefDeleted EQU 01h


externB fLoadInclude	


sEnd	DATA

sBegin	CODE

;Table of all the opcodes that contain oPrs operands
;The order of entries in this table is VERY important
;
tOpPrsDef LABEL WORD
	opTabStart	PRSDEF
	opTabEntry	PRSDEF,opStDeclare
	opTabEntry	PRSDEF,opStSub
	opTabEntry	PRSDEF,opStFunction
	opTabEntry	PRSDEF,opStDefFn
		PRSDEF_DefineMax EQU PRSDEF_opStDefFn
	opTabEntry	PRSDEF,opStCall
	opTabEntry	PRSDEF,opStCallS
	opTabEntry	PRSDEF,opStCallLess
	opTabEntry	PRSDEF,opEot

;These opcodes are of special interest BEFORE they are deleted:
; Reasons why we have to descan to RUDE when certain opcodes are deleted:
;  opStSub, opStFunction, opStDefFn :
;    MakeVariable would be called again to evaluate the parm list, and
;    would give duplicate definition errors for each parm.
;
;  If line containing opStDimTo, opStRedimTo, opShared, opStShared,
;     opStStatic, opAsType, opAsTypeExp, or opAsTypeFixed 
;     (or opCmdAsType in EB) is edited, the variable
;     manager would give a duplicate definition error when new edited line
;     is inserted.
;
;  If a break point is being toggled on the current line, then edit and
;     continue is allowable, as only the opBreakPoint will be inserted
;     on the line.
;
tOpPreDel LABEL WORD
	opTabStart	PREDEL
	; opBol and opBolLab are in this list as TxtFind terminators
	; They will keep us from Searching to opEot if no other interesting
	; opcodes are found after the otx range deleted.  This significantly
	; speeds up edits for generic programs with large text tables.
	
	opTabEntry	PREDEL,opBol
	opTabEntry	PREDEL,opBolLab
		PREDEL_BolMax EQU PREDEL_opBolLab
	opTabEntry	PREDEL,op_Include
		PREDEL_IncludeMax EQU	PREDEL_op_Include
		;The following opcodes cause rude-edit when deleted
	opTabEntry	PREDEL,opStDefType
		PREDEL_rudeMin EQU PREDEL_opStDefType
	; opStDeclare is included here to ensure that variable tables get
	; recreated in case a DECLARE for a quicklib function is deleted.
	; e.g. DECLARE FUNCTION FOO () : x = foo where foo is in a qlb.
	; deleting the DECLARE will cause a variable named foo to be defined.
	
	opTabEntry	PREDEL,opStDeclare
	opTabEntry	PREDEL,opStSub
	opTabEntry	PREDEL,opStFunction
	opTabEntry	PREDEL,opStDefFn
	opTabEntry	PREDEL,opEndSingleDef
	opTabEntry	PREDEL,opStEndDef
	opTabEntry	PREDEL,opStConst
	opTabEntry	PREDEL,opStCommon
	opTabEntry	PREDEL,opAVtRf	
	opTabEntry	PREDEL,opStReDimTo
	opTabEntry	PREDEL,opShared
	opTabEntry	PREDEL,opStShared
	opTabEntry	PREDEL,op_Static  
	opTabEntry	PREDEL,op_Dynamic 
	opTabEntry	PREDEL,opStStatic
	opTabEntry	PREDEL,opStType
	opTabEntry	PREDEL,opStEndType
	opTabEntry	PREDEL,opAsType
	opTabEntry	PREDEL,opAsTypeExp
	opTabEntry	PREDEL,opAsTypeFixed 
	opTabEntry	PREDEL,opNoList1
	;Scanner inserts opNoList1 in a statement whenever the stack
	; contains a return address into that statement.  Deleting
	; an opNoList1 means we're deleting a return address - Cant Continue.
	; opNoList1 could just be AskCantCont instead of AskRudeEdit,
	; but its not worth the extra code to let users print variables
	; after deleting a return address, and other than that,
	; AskCantCont is identical to AskRudeEdit

	opTabEntry	PREDEL,opStOptionBase0	
	opTabEntry	PREDEL,opStOptionBase1	
	opTabEntry	PREDEL,opEot

;These opcodes are of special interest AFTER they have been deleted:
;
tOpDel	LABEL WORD
	opTabStart	DEL
	opTabEntry	DEL,opLab
	opTabEntry	DEL,opLabSp
	opTabEntry	DEL,opBolLab
		DEL_bolMin EQU DEL_opBolLab
	opTabEntry	DEL,opBolLabSp
		DEL_labMax EQU DEL_opBolLabSp
	opTabEntry	DEL,opBol
	opTabEntry	DEL,opBolSp
	opTabEntry	DEL,opBolInclude
	opTabEntry	DEL,opBolIncludeSp
		DEL_bolInclMin EQU DEL_opBolInclude
		DEL_bolMax EQU DEL_opBolIncludeSp
	opTabEntry	DEL,opStFor
	opTabEntry	DEL,opStForStep
		DEL_forMax EQU DEL_opStForStep
	opTabEntry	DEL,opWatchExp
	opTabEntry	DEL,opWatchStop
		DEL_watchMax EQU DEL_opWatchStop
	opTabEntry	DEL,opAsType
	opTabEntry	DEL,opStEndProc
	opTabEntry	DEL,opStCommon
	opTabEntry	DEL,opAVtRf	
	opTabEntry	DEL,opBreakPoint
		DEL_NonSubRefMax EQU DEL_opBreakPoint 
;Remaining entries in table are all the opcodes that could contain references
; to SUBs.  We need to look for these in order to correctly reset the SUB
; name space bit.
	opTabEntry	DEL,opStDeclare 	
	opTabEntry	DEL,opStSub		
	opTabEntry	DEL,opStCall		
	opTabEntry	DEL,opStCallS		
	opTabEntry	DEL,opStCallLess	
	opTabEntry	DEL,opEot


;These opcodes are of special interest BEFORE they are inserted
; NOTE: if any entries in this table change, update PiDispatch
;
tOpPreIns LABEL WORD
	opTabStart	PI
	opTabEntry	PI,op_Static
	opTabEntry	PI,op_Dynamic
	opTabEntry	PI,op_Include
	opTabEntry	PI,opStSub
	opTabEntry	PI,opStFunction
	opTabEntry	PI,opStEndProc
	opTabEntry	PI,opEndSingleDef
	opTabEntry	PI,opStEndDef
	opTabEntry	PI,opStData
	opTabEntry	PI,opStShared
	opTabEntry	PI,opStDefType
	opTabEntry	PI,opStCommon
	opTabEntry	PI,opStEndType
	opTabEntry	PI,opStFor
	opTabEntry	PI,opStForStep
	opTabEntry	PI,opStConst
	opTabEntry	PI,opShared
	opTabEntry	PI,opStOptionBase0	
	opTabEntry	PI,opStOptionBase1	

	opTabEntry	PI,opElemRef
	opTabEntry	PI,opEot

;These opcodes are of special interest AFTER they are inserted
; NOTE: if any entries in this table change, update AiDispatch
;
tOpAftins LABEL WORD
	opTabStart	AFTINS
	opTabEntry	AFTINS,opBolInclude
	opTabEntry	AFTINS,opBolIncludeSp
		AFTINS_bolInclMax EQU AFTINS_opBolIncludeSp
	opTabEntry	AFTINS,opBol
	opTabEntry	AFTINS,opBolSp
	opTabEntry	AFTINS,opBolLab
		AFTINS_dispMin EQU AFTINS_opBolLab
	opTabEntry	AFTINS,opBolLabSp
		AFTINS_bolMax EQU AFTINS_opBolLabSp
	opTabEntry	AFTINS,opLab
	opTabEntry	AFTINS,opLabSp
	opTabEntry	AFTINS,opStFunction
	opTabEntry	AFTINS,opStSub
	opTabEntry	AFTINS,opStDeclare
	opTabEntry	AFTINS,opStCall
	opTabEntry	AFTINS,opStCallS
	opTabEntry	AFTINS,opStCallLess
	opTabEntry	AFTINS,opStEndProc
	opTabEntry	AFTINS,opEndSingleDef
	opTabEntry	AFTINS,opStEndDef
	opTabEntry	AFTINS,opStData
	opTabEntry	AFTINS,opStRestore1
	opTabEntry	AFTINS,opEot

;List of opcodes which are legal in SUB/FUNCTION window before
;the procedure's definition
;
tOpPreProc LABEL WORD
	opTabStart	PREPROC
	opTabEntry	PREPROC,opBol
	opTabEntry	PREPROC,opBolSp
	opTabEntry	PREPROC,opBolInclude
	opTabEntry	PREPROC,opBolIncludeSp
		PREPROC_bolMax EQU PREPROC_opBolIncludeSp
	opTabEntry	PREPROC,opStDefType
	opTabEntry	PREPROC,opStRem
	opTabEntry	PREPROC,opQuoteRem
	opTabEntry	PREPROC,op_Static
	opTabEntry	PREPROC,op_Dynamic
	opTabEntry	PREPROC,opNoType	;for SUB x.y
	opTabEntry	PREPROC,opReParse
	opTabEntry	PREPROC,opStEndProc	;only if SUB opStSub or
						; opStFunction precedes it.
	opTabEntry	PREPROC,opStSub
		PREPROC_SubOrFuncMin EQU PREPROC_opStSub
	opTabEntry	PREPROC,opStFunction
	opTabEntry	PREPROC,opEot

tOpEndProc LABEL WORD
	opTabStart	ENDPROC
	opTabEntry	ENDPROC,opStEndProc
	opTabEntry	ENDPROC,opEot

sEnd	CODE



;--------------------------------------------------------------------
; Functions which keep track of where the current "defining" reference
; to a Prs is.  If there are any references to a prs in any loaded pcode,
; we keep track of the "most powerful" or "defining" reference in
; prs.oRsDef, prs.otxDef.
;
; A SUB/FUNCTION/DEF FN statement is the "most powerful" possible
;   definition for a prs entry.
; If no SUB/FUNCTION/DEF FN statements refer to a prs, the next most
;   powerful definition is a DECLARE statement.
; If no DECLARE statements refer to a prs, the only remaining possible
;   definition is a CALL statement.
;
; When all pcode references to a prs have been deleted,
; and the prs has no text table, PrsFree is called to remove the entry.
;
; The management of "defining" references happens in two stages during
; an edit.  When a "defining" reference is deleted, prs.otxDef is set
; to undefined by UndefPrs.  At the end of the edit operation, RedefOrDelPrs
; is called to search for a new "defining" reference, or free the prs.
;
; This is done in two stages as a speed optimization for text edits.
; This will help speed up the cases where you are discarding text
; tables, or terminating ASCII load because of an out of memory error.
; This way we accumulate and process all prs defining reference changes
; at one time.
;
;			Call Trees
;			==========
;
; Deleting a defining reference
; -----------------------------
; MrsDiscard   PrsDiscard
;	  \    /
;          \  /
;       TxtDiscard                     ParseLine
;           |                             |
;       TxtDelete             ParseUndo MakeProc (when renaming a sub/function)
;           |                        |    |
;   ForEachPrsInPlaceCP(UpdatePrs)   |	  |
;			      |      |	  |
;			      +------+----+
;				     |
;				  UndefPrs
;
; End of edit - find new defining reference
; -----------------------------------------
;    TxtChange	    LoadFile
;	 |	       |
;	 |	       |
;	 |	       |
;	 |	      MrsDiscard   PrsDiscard
;	 |	       |      \     /	  |
;	 |	       |     PrsDiscard1  |
;	 |	       |		  |
;	 +-------------+------------------+
;		  |
;	   ChkAllUndefPrs		 
;		  |			 
;		  --------
;		  |	 |
;	          | 	 
;	   ForEachCP(TryToDefPrs)
;
;--------------------------------------------------------------------

sBegin	DATA

;** the static structure uPrs is defined in SetPrsDefn header
DPRS_ST	STRUC
DPRS_oRs		DW 1 DUP (?)
DPRS_oMrs		DW 1 DUP (?)
DPRS_otx		DW 1 DUP (?)
DPRS_flags		DB 1 DUP (?)
DPRS_ST	ENDS

dprs	DB size DPRS_ST DUP (?)

;** the static structure uPrs is defined in UpdatePrs's header
UPRS_ST	STRUC
UPRS_oRsEdit		DW 1 DUP(?)
UPRS_otxEdit		DW 1 DUP(?)
UPRS_cbDel		DW 1 DUP(?)
UPRS_cbIns		DW 1 DUP(?)
UPRS_oPrsDel		DW 1 DUP (?)
UPRS_fPrsDelFound	DW 1 DUP (?)
UPRS_fNoRefSameMod	DW 1 DUP (?)
UPRS_oMrsRefDel		DW 1 DUP (?)
UPRS_ST	ENDS

uprs	DB size UPRS_ST DUP (?)

sEnd	DATA

sBegin	CP
assumes	cs,CP

;**************************************************************
; void SetPrsDefn
; Purpose:
;	Called when a prs reference is inserted in pcode.  If this is the
;	"most powerful" definition we've seen so far for the current prs,
;	the the prs entry is changed to remember this text offset
;	as the "defining" reference.
;
; Entry:
;	dprs.DPRS_oRs,oMrs,otx identify where in the pcode
;	  the reference lives.
;	dprs.DPRS_flags = FP_DEFINED if this is for a SUB/FUNCTION/DEF FN stmt.
;	                = FP_DECLARED if this is for a DECLARE stmt.
;	                = 0 if this is for a CALL stmt.
;	grs.oPrsCur identifies the prs being referenced.
;
;**************************************************************
SetPrsDefn PROC NEAR
	test	[dprs.DPRS_flags],FP_DEFINED
	jne	SpNewDef		;brif reference is SUB,FUNC,DEF FN stmt
	test	[prsCur.PRS_flags],FP_DEFINED
	jne	SpNoNewDef		;brif prs is already defined by SUB,...
	test	[dprs.DPRS_flags],FP_DECLARED
	jne	SpNewDef		;brif reference is DECLARE
	test	[prsCur.PRS_flags],FP_DECLARED
	jne	SpNoNewDef		;brif prs is already declared
SpNewDef:
	;Got a SUB, FUNCTION or DEF FN   OR
	;    a DECLARE for prs with no definition  OR
	;    a CALL to an as yet undeclared, undefined SUB.
	; This is the "most powerful" reference seen so far,
	; make this text reference the new owner of the prs entry.
	
	; We need to set prsCur.oMrs even though in most cases, it
	; is redundant given prsCur.oRsDef.  The case where it is
	; valuable info is for DECLAREd prs's with no text tables.
	; If it were not set here, there would be cases when ForEachPrsInMrs
	; would not pick up this prs
	
	mov	ax,[dprs.DPRS_oRs]
	mov	[prsCur.PRS_oRsDef],ax
	mov	ax,[dprs.DPRS_otx]
	mov	[prsCur.PRS_otxDef],ax
	mov	al,[dprs.DPRS_flags]
	or	[prsCur.PRS_flags],al
	test	[txdCur.TXD_flags],FTX_mrs
	je	SpNoNewDef		;brif prs has text table
					;If we didn't, commenting out
					;a SUB stmt would move the prs to
					;another module.
	mov	ax,[dprs.DPRS_oMrs]
	mov	[prsCur.PRS_oMrs],ax
	cmp	[prsCur.PRS_procType],PT_DEFFN
	jne	SpNoNewDef		;brif not a DEF FN
	mov	[prsCur.PRS_oRsDef],ax	;for DEF FNs, the oRsDef is the
					; module's text table.
SpNoNewDef:
	ret
SetPrsDefn ENDP

;**************************************************************
; boolean TryToDefPrs()
; Purpose:
;	Called to see if current text table contains any references
;	to prs uprs.UPRS_oPrsDel.
;
; Entry:
;	uprs.UPRS_oPrsDel is the prs whose reference has been deleted
;uprs.UPRS_oMrsRefDel is the mrs from which the reference was deleted
;uprs.UPRS_fNoRefSameMod: if TRUE then TryToDefPrs must search for
;	a reference to oPrsDel in text tables of module oMrsDel.
;dprs.DPRS_flags: if set to 0 then TryToDefPrs must search for any
;	ref's to oPrsDel. 
; Exit:
;uprs.UPRD_fNoRefSameMod is set to FALSE if it is TRUE on entry and 
;	the text table being searched is in module uprs.UPRS_oMrsRefDel
;	and a ref to procedure uprs.UPRS_oPrsDel is found.
;	uprs.UPRS_fPrsDelFound = TRUE if any other references were found
;	if a "defining" reference is found (i.e. anything but a CALL),
;	   dprs.DPRS_oRs,otx are set to identify the text table and text
;	   offset for the new "defining" reference.
;dprs.DPRS_flags is set to FP_DECLARED if a DECLARE for the deleted
;	Prs is found.
;FALSE is returned when no more searching is required, i.e. when
;	dprs.DPRS_flags = FP_DECLARED and uprs.UPRS_fNoRefSameMod
;	is FALSE.
;	
; 
;**************************************************************
TryToDefPrs PROC NEAR
	push	si			;save caller's si,di
	push	di			
	cmp	[txdCur.TXD_ScanState],SS_SUBRUDE 
	je	JE_TryDone		;	
	mov	di,[uprs.UPRS_fNoRefSameMod] 
	or	di,di			
	jz	TrySearch		;brif already found another ref
					; to the proc in the module with 
					; the deleted reference
	mov	di,[uprs.UPRS_oMrsRefDel] ;di = oMrs of module from which
					; a reference was deleted
	sub	di,[grs.GRS_oMrsCur]	;di = 0 iff the text table is in
					; the same module as the text tbl
					; from which a ref was deleted
	jz	TrySearch		;brif in same module as deleted ref
	test	[dprs.DPRS_flags],FP_DECLARED 
	jnz	TryDone			;exit if we don't need to search
					; this text table for a declare
TrySearch:	 			
	call	TxtDescanCP		;descan current txt tbl to SS_PARSE
	sub	ax,ax
	push	ax
	PUSHI	ax,<CODEOFFSET tOpPrsDef>
	call	TxtFindOp		;ax = offset to 1st opcode in cur text
					; table with prs operand
					;dl = [txtFindIndex]
TryLoop:
	mov	si,ax			;si = otxCur
	cmp	dl,PRSDEF_opEot
JE_TryDone:				
	je	TryDone			;brif done with text table
	add	ax,4			;advance to oPrs operand
	cmp	dl,PRSDEF_opStDefFn
	jne	TryNotDefFn		;brif ref is not a DEF FN
	inc	ax			;skip DEF FN's link field
	inc	ax
TryNotDefFn:
	call	GetWOtx			;ax = oPrs of reference (ax)
	cmp	ax,[uprs.UPRS_oPrsDel]
	jne	TryNext			;brif not ref to prs of interest
	mov	[uprs.UPRS_fNoRefSameMod],di  ;set flag indicating whether
					      ; or not another ref was found
					      ; in module of deleted ref
	test	[dprs.DPRS_flags],FP_DECLARED 
	jnz	TryExit1		;brif not searching for defining ref
	mov	[uprs.UPRS_fPrsDelFound],sp
					;tell caller we found a reference
	;remember where new reference is in static dprs struct
	mov	ax,[grs.GRS_oRsCur]
	mov	[dprs.DPRS_oRs],ax
	mov	ax,[grs.GRS_oMrsCur]
	mov	[dprs.DPRS_oMrs],ax
	mov	[dprs.DPRS_otx],si	;save otxCur
	cmp	[txtFindIndex],PRSDEF_DefineMax
	ja	TryNext			;brif reference was a CALL

	;Since this is only called when the current "defining"
	;reference has been deleted, we know we're never going
	;to find a SUB or FUNCTION pcode.  A DECLARE is the
	;strongest definition we can hope to find, so there's
	;no need to search any further
	
	test	[mrsCur.MRS_flags2],FM2_INCLUDE ;is this an INCLUDE mrs?
	jne	TryNext 		;force it to a "weak" owner

	mov	[dprs.DPRS_flags],FP_DECLARED
TryExit1:
	xchg	ax,di			;ax = 0 iff no longer need to search
	jmp	SHORT TryExit

TryNext:
;if we've just seen a CALL ref, keep searching for DECLARE
	push	si
	PUSHI	ax,<CODEOFFSET tOpPrsDef>
	call	TxtFindNextOp
	jmp	SHORT TryLoop

TryDone:
	mov	ax,sp			;return TRUE (non zero)
TryExit:
	pop	di			;restore caller's di
	pop	si			;restore caller's si
	ret	
TryToDefPrs ENDP

;**************************************************************
; FreeAllUndefinedPrs, ChkAllUndefPrs, ChkAllUndefPrsSaveRs
; Purpose:
;	FreeAllUndefinedPrs has two purposes:
;	(1) It is called to check all prs's to see
;	if the the "defining" text reference to a prs has
;	been marked as deleted. If so, it calls RedefOrDelPrs
;	to search for a new "definition" if none are found, then
;	the Prs is released via PrsFree.
;	(2) It is called to check all prs's to see
;	if the last reference to the prs has been deleted from the
;	module being editted. 
;	ChkAllUndefPrs differs from FreeAllUndefPrs in that it 
;	knows that searching only needs to be performed when a 
;	reference is deleted.
;
;	FreeAllUndefinedPrs is called to Free prs entries created
;	by undefined direct mode references.
;
;	The FTM_PrsDefDeleted bit of flagsTm is used to
;	to determine if we need to search for a new "defining"
;	reference for the prs.
;
; Entry:
;	flagsTm.FTM_PrsDefDeleted set if we need to search
;	  for a new "defining" reference.
;
; Exit:
;	Deleted prs entries with no new definitions are freed
;	Deleted prs entries with new definitions have otxDef
;	  and oRsDef set to the new definition.
;**************************************************************
PUBLIC	ChkAllUndefPrsSaveRs
ChkAllUndefPrsSaveRs PROC NEAR
	push	[grs.GRS_oRsCur]	;preserve current oRs
	call	ChkAllUndefPrs		;check all undefed prs entries
	call	RsActivateCP		;Reactivate current oRs
	ret
ChkAllUndefPrsSaveRs ENDP

PUBLIC	ChkAllUndefPrs
ChkAllUndefPrs	PROC NEAR
	test	WORD PTR [flagsTm],FTM_PrsDefDeleted OR FTM2_PrsRefDeleted*100h
					;have any definitions been deleted?
	jne	FreeAllUndefinedPrs	;brif so - Redef or Del all UNDEFINED
					; prs entries
	ret
ChkAllUndefPrs	ENDP

PUBLIC	FreeAllUndefinedPrs
FreeAllUndefinedPrs PROC NEAR
	push	si			
	push	di			
	mov	ax,[grs.GRS_oMrsCur]	
	mov	[uprs.UPRS_oMrsRefDel],ax	;store mrs of deleted text
	mov	si,UNDEFINED		
FreeAllUndef_Cont:			
	cCall	PrsDeActivate		; ensure all proc.s are in Rs table
FreeAllUndef_Cont1:			
	mov	di,si			
FreeAllUndef_Cont2:			
	mov	ax,di			
	call	GetNextPrs		; PTRRS:ax points to first prs
					;	ax == UNDEFINED ==> no prs's
	inc	ax			
	jnz	FAU_DontExit		
	jmp	FreeAllUndef_Exit	
FAU_DontExit:				

	dec	ax			
	mov	si,ax			; cache, so we don't end up starting
					; from the beginning of the prs
					; chain each time we free a prs
	mov	bx,ax			
	RS_BASE add,bx			; bx = oPrs

; assign cx = 0 if no reference to the Prs has been deleted otw non-zero
	xor	cx,cx			
	test	PTRRS[bx.PRS_flags2],FP_RefDeleted ;[38]
	jz	NoRefDeleted		;brif no reference has been deleted
	and	PTRRS[bx.PRS_flags2],NOT FP_RefDeleted ;[38]clear flag
	not	cx			;set cx = non-0 since ref was deleted
NoRefDeleted:				
	mov	[uprs.UPRS_fNoRefSameMod],cx ;initialize fNoRefSameMod
					;     to TRUE if we must search for
					;     a ref to the Prs in the module

; assign ax = 0 if otxDef is defined otw non-zero
	mov	al,1			;assume otxDef is undefined
	cmp	PTRRS[bx.PRS_otxDef],UNDEFINED
	je	otxDefUnDefined		;brif otxDef is UnDefined
	jcxz	FreeAllUndef_Cont1	;brif otxDef is Defined and 
					; no ref to the prs has been
					; deleted -- nothing to do
	xor	ax,ax			;set ax = 0 since otxDef is defined
otxDefUnDefined:
	push	cx			;preserve condition flag
	push	ax			;preserve condition flag

	push	si			; pass to PrsActivateCP
	call	PrsActivateCP		;activate prs which may be freed

	pop	cx			;cx = 0 iff otxDef is defined
	pop	ax			;ax = 0 iff no ref to proc was del'd
	jcxz	DontSearchForDeclare	;brif otxDef of proc is defined
	xor	dx,dx			;assume that we will need to search
					; for a new defining ref
	test	[flagsTm],FTM_PrsDefDeleted 
	jnz	SearchForRef		;brif a defining ref has been del'd
	or	ax,ax			;ax = 0 if no ref to proc was del'd
	jz	ChkDeletePrs		;This branch is taken if no ref to
					; this proc has been deleted and
					; no defining ref has been deleted
DontSearchForDeclare:	     		
	mov	dl,FP_DECLARED		
SearchForRef:				
	mov	[dprs.DPRS_flags],dl	;set dprs.flags to FP_DECLARED
					; if we don't need to search for
					; a new defining ref otw set to 0
	mov	[uprs.UPRS_fPrsDelFound],dx ;set fPrsDelFound to 0 if we
					; are searching for a defining ref
	mov	dx,[grs.GRS_oPrsCur]	;set up global specifying which proc
	mov	[uprs.UPRS_oPrsDel],dx	;  we are searching for references to
	push	cx			;preserve condition flag

	;Note that we search even text mrs's because the reference
	;to the prs may be within an INCLUDE mrs.  It is tempting to
	;make INCLUDE mrs's be FM2_NoPcode mrs's, but doing so would
	;prevent the ability to select the 'View/Include File' menu
	;item when an INCLUDE mrs is active, because the $INCLUDE
	;lines would not have been parsed.
	
	;This leads to the situation that an INCLUDE mrs can be
	;the owner of a prs.  This keeps the prs from being deleted until
	;all references to the prs are gone.  However, we must also insure
	;that the include mrs does not become an owner of a prs when other
	;"stronger" definitions exist.	Therefore, all prs references from
	;an include mrs will be treated as the "weakest" possible owners.
	
	mov	bx,CPOFFSET TryToDefPrs
	mov	al,FE_PcodeMrs+FE_TextMrs+FE_CallMrs+FE_PcodePrs+FE_SaveRs
	call	ForEachCP
	cmp	[uprs.UPRS_fNoRefSameMod],FALSE	
	je	GotRefInSameModule	;brif another ref in module found

;We have not found another reference to the Prs in the module from which
;the ref was deleted. Clear the NMSP_sub name table bit of the sub's oNam
;in that module so that it can be used now as a variable name.
	push	[grs.GRS_oRsCur]	;save current oRs for reactivation
	push	[uprs.UPRS_oMrsRefDel]	
	call	MrsActivateCP		;activate the module 
	cCall	FieldsOfPrs,<si>	;ax = oNam of proc
	xchg	bx,ax			;pass bx = oNam of proc
	mov	al,NMSP_SUB		;pass al = flag to clear
	call	ResetONamMask		;clear sub bit of oNam of Mrs
	call	RsActivateCP		;reactivate the Rs we were searching
GotRefInSameModule:			
	pop	cx			;cx = 0 iff otxDef is defined
	jcxz	J1_FreeAllUndef_Cont	;brif otxDef is defined - i.e. we
					; were only searching for another
					; ref in module of deletion
	cmp	[uprs.UPRS_fPrsDelFound],0 ;was a new defining reference found
	je	ChkDeletePrs		;brif didn't find new "defining" ref

	;found new "defining" reference
	
	call	SetPrsDefn		;parms are in static struct dprs
J1_FreeAllUndef_Cont:			
	jmp	FreeAllUndef_Cont 	;[37] go on to next prs

ChkDeletePrs:
	;Prs has no "defining" reference
	;If prs has no text table, release it via PrsFree
	;  By the time this function has been called by PrsDiscard,
	;    any text table that this prs might have owned will have
	;    been released.
	
	test	[txdCur.TXD_flags],FTX_mrs ;does this prs have a text table?
	je	J1_FreeAllUndef_Cont	; brif so, will eventually be freed
					; by PrsDiscard

	call	PrsFree 		;free the prs
	DbAssertRel [grs.GRS_oPrsCur],z,UNDEFINED,CP,<FreeAllUndefinedPrs err>
	jmp	FreeAllUndef_Cont2 	;[37] go on to look for next prs,
					 ; starting from last prs that
					 ; we did not free ...
FreeAllUndef_Exit:			
	pop	di			
	pop	si			
	and	WORD PTR[flagsTm],NOT(FTM_PrsDefDeleted+FTM2_PrsRefDeleted*100h)
					;all Prs entries are clean
	ret
FreeAllUndefinedPrs ENDP

;**************************************************************
; UndefPrs(ax:oPrsDelete)
; Purpose:
;	This function is called whenever the "defining"
;	text reference to a prs is deleted.  It clears
;	the prs definition flags and sets prs.otxDef to
;	undefined.
;
; NOTE: The caller is responsible for calling ChkAllUndefPrs
;	to search for a new defining reference, or release
;	the prs entry if no more references to the prs exist.
; NOTE: Some callers expect no heap movement from this routine.
;
; Entry:
;	ax is the oPrs whose reference is being deleted
;
; Exit:
;	prs.otxDef = UNDEFINED
;	prs definition flags are cleared.
;	restores caller's active register set.
;
;**************************************************************
PUBLIC	UndefPrs
UndefPrs PROC NEAR
	push	[grs.GRS_oRsCur]	;save caller's active register set

	push	ax			;pass to PrsActivateCP
	call	PrsActivateCP		;activate prs which may be freed
	or	[flagsTm],FTM_PrsDefDeleted ;we have deleted a defining reference
	mov	[prsCur.PRS_otxDef],UNDEFINED ;remember that current "defining"
					;reference has been deleted
	Msk EQU NOT (FP_DEFINED OR FP_DECLARED OR FP_CDECL)
	and	[prsCur.PRS_flags],Msk	;turn off these flag bits
					;SetPrsDefn will be called to turn
					;one or more back on if another ref
					;is found in the pcode
	call	RsActivateCP		;activate caller's register set
					; oRs parm is already on stack
	ret
UndefPrs ENDP

;**************************************************************
; boolean UpdatePrs()
; Purpose:
;	This is called after text has been inserted into, or deleted
;	from any text table.  It is called for each prs which could be
;	declared in this text table (even if by CALL <compiled sub>),
;	If the prs's "defining" reference is in this text table:
;	   If the reference was deleted by the edit:
;	      Set its Otx field to UNDEFINED
;		a new defining reference will be searched for at completion
;		of the current edit operation
;	   else if the "defining" reference was moved by the edit:
;	      its otx field is updated
;	This function will flag many prs entries if the
;	deleted text included many DECLAREs, where the DECLARE was the
;	only remaining reference.
;
;	NOTE: to drastically improve text edit performance, this routine
;	gets called by ForEachPrsInPlaceCP, which marches through the
;	prs table without activating the PRS.  Thus, this routine MUST
;	NOT cause heap movement which would invalidate the pointer
;	used to march the prs table.
;
; Entry:
;	si = ptr to prs entry being examined
;	uprs.UPRS_oRsEdit = text table in which edit occurred
;	uprs.UPRS_otxEdit = text offset where text was inserted/deleted
;	uprs.UPRS_cbDel = number of bytes deleted
;	uprs.UPRS_cbIns = number of bytes inserted
;
;**************************************************************
DbPub	UpdatePrs
UpdatePrs PROC NEAR
	GETRS_SEG es,dx,<SIZE,LOAD>	;[11]
	mov	dx,PTRRS[si.PRS_otxDef] ; dx = text offset to prs's "defining"
					; reference
	inc	dx			;test for UNDEFINED
	je	UpExit			;brif prs has no references
	dec	dx			;restore dx = otxDef
	mov	ax,PTRRS[si.PRS_oRsDef] 
	cmp	BPTRRS[si.PRS_procType],PT_DEFFN 
	jne	UpdNotDefFn		;brif not a DEF FN
	mov	ax,PTRRS[si.PRS_oMrs]	; DEF FNs live in a module's text table
UpdNotDefFn:
	cmp	ax,[uprs.UPRS_oRsEdit]
	jne	UpExit			;brif not text table being edited
	;prs is currently defined by reference in edited text table
	mov	ax,[uprs.UPRS_otxEdit]
	cmp	dx,ax
	jb	UpExit			;brif reference unaffected by edit
	add	ax,[uprs.UPRS_cbDel]
	cmp	ax,dx
	jbe	UpMoved			;brif defining ref moved by edit

	;Defining reference has been deleted.
	;Mark Prs to indicate that defining reference has been deleted.
	
	mov	ax,si		;ax = ptr to prs entry
	RS_BASE sub,ax		;ax=oPrs for entry, parm to UndefPrs
	call	UndefPrs	;Mark Prs as "defining" reference as
				;deleted
	jmp	SHORT UpExit

;defining reference has been moved up or down in memory
UpMoved:
	mov	ax,[uprs.UPRS_cbIns]
	sub	ax,[uprs.UPRS_cbDel]
	add	PTRRS[si.PRS_otxDef],ax ; update otx based on edit
UpExit:
	or	ax,sp		; return ax != 0 and NZ
	ret
UpdatePrs ENDP



;**************************************************************
; boolean NEAR TxtDelete(otxDelFirst, otxDelLast)
; Purpose:
;	Delete some text from the current text table.
;	Doesn't update linked lists if ASCII load is active.
;
; NOTE:
;	If a Prs's "defining" reference is being deleted, this function
;	will cause the prs to be marked as UNDEFINED.  It is up to
;	the caller to insure that orphaned "New" defining references
;	the prs do not exist by calling ChkAllUndefPrs if the possibility
;	of deleting a "defining" reference has occurred during the
;	edit operation.
;
; Entry:
;	Same as for TxtChange:
;
; Exit:
;	Same as for TxtChange
;	[cForDel] = 1 + number of FOR statements deleted
;	[cForStepDel] = 1 + number of FOR...STEP statements deleted
;	Condition codes are set based on value in ax
;
;**************************************************************
cProc	TxtDelete,<PUBLIC,NEAR,NODATA>,<si,di>
	parmW	otxDelFirst
	parmW	otxDelLast
	localW	otxNext
	localW	otxTop
	localW	otxBottom
	localW	oTyp
	localW	cbDel
cBegin
 DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<TxtDelete err 1>
	DbChk	Otx,otxDelFirst		;error if > txdCur.bdlText.cbLogical
	DbChk	Otx,otxDelLast		;error if > txdCur.bdlText.cbLogical
	DbAssertRel [otxDelFirst],be,[otxDelLast],CP,<TxtDelete err 2>
	mov	di,[otxDelFirst]
	mov	al,[bigEditState]
	cmp	al,BIG_EDIT_CANCEL
	jne	DelNoCancel
	jmp	DelGoodExit		;backout of BigEdit if user CANCELed

DelNoCancel:
	cmp	[txdCur.TXD_SCANSTATE],SS_SUBRUDE 
	jne	DelNotSubRude		;	
	jmp	DelNoThreads		;don't need to examine text before
					; deletion if no oPrs's in code
DelNotSubRude:				;	
	cmp	al,BIG_EDIT_FALSE
	jne	DelInBigEdit		;brif in a BigEdit
	mov	[cForDel],1
	mov	[cForStepDel],1
DelInBigEdit:

;-------------------------------------------------------------------
;Examine the block of pcode being deleted opcode by
;opcode, looking for a RUDE EDIT
;   
	mov	cx,[otxDelLast]
	sub	cx,di			;compute cb to be deleted
	mov	[cbDel],cx
	jcxz	DelPreDone		;brif no text to delete, destroy scrap

	test	[mrsCur.MRS_flags2],FM2_Include OR FM2_NoPcode
	jne	DelNotTextMrs		;brif document/include file is active

	cmp	[b$cNonQBIFrames],0	;nonzero when non-QBI frames are on 
					; the stack.  Any edit at this point
					; could invalidate return addresses on 
					; the stack.  At this point we have 
					; decided to askcantcont instead of 
					; trying to fix up the return addresses
					; on the stack.
					;are non qbi return addresses on stack?
	jne	DelAskCantCont		;brif so, check for possible back out
DelNotTextMrs:
	test	[mrsCur.MRS_flags2],FM2_Include
	je	DelNotIncl1		;brif not editing an include file
DelAskCantCont:
	call	AskCantCont_CP		;ask user "Want to back out?"
	je	J1_DelBackOut		;return FALSE if user wants to back out
DelNotIncl1:
	push	di			;pass [otxDelFirst]
	PUSHI	ax,<CODEOFFSET tOpPreDel>
	call	TxtFindOp		;find 1st opcode of interest

;ax = offset to 1st opcode of interest
;dl = [txtFindIndex]
DelPreLoop:
	cmp	ax,[otxDelLast]
	jae	DelPreDone		;brif no significant opcodes were in
					;deleted range
	xchg	si,ax			;si = text offset to current opcode
	;opBol and opBolLab are in the PREDEL list to keep us from unnecessarily
	; searching all the way to opEot for innocuous edits of large "simple"
	; txt tables.  This can significantly speed up simple edits.
	
	cmp	dl,PREDEL_BolMax	;is this just a Bol?
	jbe	J1_DelPreNext		;brif so, search for next opcode.

	cmp	dl,PREDEL_IncludeMax
	jbe	DelInclude		;brif INCLUDE (or $INCLUDE [QB4])

	;It must be PREDEL_rudeMin or greater - rude edit
	push	dx			;save txtfindindex in case AskRudeEdit
					;calls TxtFind
	call	AskRudeEdit		;see if user wants to back out of edit
	pop	ax			;al = txtfindindex
	je	J1_DelBackOut		;return FALSE if user wants to
					; back out of edit
	FloadActive			;don't descan if Loading
	jne	DelPreNext
	cmp	al,PREDEL_opStDefType
	jne	DelPreNext		;brif not DEFINT..DEFSTR stmt
	call	SystemDescanCP		;implicit parms in DECLARE, SUB,
					; FUNCTION stmts need to be rechecked
					; by the scanner
DelPreNext:
	push	si
	PUSHI	ax,<CODEOFFSET tOpPreDel>
	call	TxtFindNextOp		;find next opcode of interest
	jmp	SHORT DelPreLoop

DelInclude:
	push	si			;pass otx
	call	OtxNoInclude		;ax = otx to opBol/opEot for next line
					; which has no $INCLUDE
	cmp	ax,[otxDelLast] 	;is end of $INCLUDE pcode beyond
					; requested delete?
	jbe	J1_DelPreNext		;brif not
	mov	[otxDelLast],ax		;delete all included lines as well
	sub	ax,di			;ax = new cbDel
	mov	[cbDel],ax
J1_DelPreNext:
	jmp	SHORT DelPreNext


J1_DelBackOut:
	jmp	DelBackOut		;return FALSE - user wants to
					; back out of edit

;-------------------------------------------------------------------
; If fFillScrap and bdTxtScrap is empty,
;    fFillScrap = FALSE
;    copy deleted text to bdTxtScrap
;    otxScrap = otxDelFirst
; If cbBigIns > 0, cbBigIns -= cbDel
;
;Note that cbDel may actually be zero but that we still create the scrap
;because we may subsequently be called during a big edit with fFillScrap
;TRUE but we don't want to fill the scrap.

DelPreDone:
	cmp	[fFillScrap],FALSE
	je	DelNoCopy		;brif no need to copy text to scrap
	cmp	[bdlTxtScrap.BDL_status],NOT_OWNER	
	jne	DelNoCopy		;brif scrap is already in use
	mov	[fFillScrap],FALSE	;reset one-shot flip-flop
	mov	[otxScrap],di		;remember text offset of deleted text

;  CALL TxtCopyScrap(otxDelFirst, 0, cbDel, FALSE)	[9]
	push	di			;push otxDelFirst
	xor	ax,ax			
	push	ax			;offset of where to copy into Scrap
	push	[cbDel]
	push	ax			;push FALSE meaning don't delete text
	call	TxtCopyScrap		;ax = 0 if out of memory
	jnz	CopiedOK		;brif buffer allocated successfully

	call	AskRudeEdit		;Give user a chance to back out of edit
					; if CONT is currently possible.
	je	J1_DelBackOut		;return FALSE if user wants to
					; back out of edit
	jmp	SHORT DelNoCopy

CopiedOK:

;-------------------------------------------------------------------
;NOTE: At this point, the user CANNOT back out of the edit.
;      All actions from this point on are irreversible.
;
;Examine the block of pcode being deleted opcode by
;opcode, taking special action for each opcode of interest.
;   
; Register usage for loop: si = otxCur, di = otxDelFirst
;
DelNoCopy:
	cmp	[cbDel],0		;was any text deleted?
	jne	DelAfterDel		;brif so
					; - look for special deleted opcodes
	jmp	DelGoodExit		;no pcode deleted - exit

DelAfterDel:
	push	di			;pass [otxDelFirst]
	PUSHI	ax,<CODEOFFSET tOpDel>
	call	TxtFindOp		;ax = offset to 1st interesting opcode

;ax = offset to 1st opcode of interest
;dl = [txtFindIndex]
DelLoop:
	cmp	ax,[otxDelLast]
	jae	J1_DelDone		;brif reached end of deleted range
	xchg	si,ax			;si = offset to current opcode (otxCur)
	xchg	ax,dx			;al = [txtFindIndex]
	cmp	al,DEL_bolMax
	ja	DelNotBol		;brif not a label or bol opcode
	cmp	al,DEL_labMax
	ja	DelNotLab		;brif not a label definition opcode
	lea	ax,[si+4]		;ax = text offset to oNam field
	call	GetWOtx			;ax = oNam for label
	xchg	bx,ax			;pass oNam in bx
	mov	al,NM_fLineNumLabel	;pass mask to reset in al
	call	ResetONamMask		;reset bit that says the label by
					; this name is defined
DelNotLab:
	mov	al,[txtFindIndex]
	cmp	al,DEL_bolMin
	jb	J1_DelNext		;brif not a begin of line opcode
	dec	[txdCur.TXD_cLines]	;decrement text table's line count
	cmp	al,DEL_bolInclMin
	jb	J1_DelNext			;brif not included line
	dec	[txdCur.TXD_cLinesIncl]	;decrement text table's INCLUDE line cnt
	jmp	SHORT J1_DelNext

DelNotBol:
	cmp	al,DEL_NonSubRefMax	
	jle	DelNotSubRef		;brif not a procedure reference
DbAssertRelb [txdCur.TXD_SCANSTATE],ne,SS_SUBRUDE,CP,<TxtDelete: tbl in SUBRUDE>
	lea	ax,[si+4]		
	call	GetWOtx			;ax = oPrs referenced
	call	pPrsOPrs		;es:bx points to Rs
	cmp	PTRRS[bx.PRS_procType],PT_SUB 
	jne	J1_DelNext		;brif not a sub Prs
	or	PTRRS[bx.PRS_flags2],FP_RefDeleted ;[38]set flag indicating that
					; a ref has been deleted
	or	[flagsTm2],FTM2_PrsRefDeleted ;set flag indicating
					; that a Prs Ref has been deleted
	jmp	SHORT J1_DelNext	

J1_DelDone:
DJMP    jmp	SHORT DelDone

DelNotSubRef:				
	cmp	al,DEL_forMax
	ja	DelNotFor		;brif FOR pcode not being deleted
	.errnz DEL_forMax - DEL_opStForStep
	je	DelForStep		;brif deleted a FOR STEP
	inc	[cForDel]
	jmp	SHORT J1_DelNext

DelForStep:
	inc	[cForStepDel]
	jmp	SHORT J1_DelNext

DelNotFor:
	cmp	al,DEL_watchMax
	ja	NotDelWatch		;brif WATCH pcode not being deleted
	call	WatchDeleted		;reduce number of lines allocated to
					; watch window, remember to redraw
					; DebugScreen
J1_DelNext:
	jmp	SHORT DelNext

NotDelWatch:
	cmp	al,DEL_opStEndProc
	jne	DelNotEndProc		;brif not END DEF/SUB/FUNCTION
	;user is deleting an END SUB or END FUNCTION
	and	[prsCur.PRS_flags],NOT FP_ENDPROC
	jmp	SHORT DelNext

DelNotEndProc:
	cmp	al,DEL_opAVtRf		;[1]
	je	DoResetCommon		; brif deleting a DIM statement
	cmp	al,DEL_opStCommon
	jne	DelNotCommon		;brif not COMMON
DoResetCommon:				
	DbAssertRelB cChansOpen,e,0,CP,<TxtDelete: Tried to delete COMMON while load is active>
	call	ResetCommon		;Eliminate all common type tables
	call	SystemDescanCP		;scanner will rebuild common tables
	jmp	SHORT DelNext		; for each txt tbl next scan

DelNotCommon:
	cmp	al,DEL_opBreakPoint
	jne	DelNotBreakPoint	;brif no Break Point set on this line
	or	[flagsTm],FTM_BpDeleted ;we deleted a break point
	jmp	SHORT DelNext

DelNotBreakPoint:
	DbAssertRelB cChansOpen,e,0,CP,<TxtDelete: Tried to delete AS usrtyp while load is active>
	DbAssertRelB al,e,DEL_opAsType,CP,<TxtDelete err 3>
	;DELETING AS <usertyp>

	;remember to call PreScanAsChg before scanning program
	mov	ax,si			;pass text offset in ax
	mov	bx,di			;pass otxDelFirst in bx
	mov	cx,[otxDelLast]		;pass otxDelLast  in cx
	call	ChkLastAsText		;if last instance of 'x AS user-type'
					; in module, reset x's NM_fAs name
					; table bit and set module's
					; FM_asChg bit so we'll call
					; PreScanAsChg before scanner
DelNext:
	push	si
	PUSHI	ax,<CODEOFFSET tOpDel>
	call	TxtFindNextOp
	jmp	DelLoop

DelDone:
	mov	cx,[cbDel]
	cmp	[cbBigIns],0
	je	DelNotBig
	sub	[cbBigIns],cx
DelNotBig:

	FloadActive			;don't update linked lists if Loading
	jne	DelNoThreads

	push	di			;pass otxDelFirst
	push	cx			;pass cbMove
	call	TxtDelThread		;update linked lists for delete

DelNoThreads:
	mov	si,[otxDelLast]
	push	si			;pass otxDelLast
	push	di			;pass otxDelFirst
	call	TxtMoveDown		;Actually delete text from text table
	sub	si,di			;si = cbDel

	test	[flagsTM],FTM_SaveProcHdr
	jne	DelGoodExit		;brif SaveProcHdr was in critical
					; section. A temp txt table is active,
					; which is NOT oRsCur.

	;Update program counter and any other runtime text pointers
	push	di			;pass otxDelFirst
	sub	ax,ax
	push	ax			;pass cbIns (0)
	push	si			;pass cbDel
	push	ax			;fTestOnly = FALSE
	call	UpdatePcs

	;pass information about the edit to UpdatePrs in static struct updPrs
	mov	ax,[grs.GRS_oRsCur]
	mov	[uprs.UPRS_oRsEdit],ax
	mov	[uprs.UPRS_otxEdit],di	;save otxDelFirst
	mov	[uprs.UPRS_cbDel],si
	mov	[uprs.UPRS_cbIns],0

	mov	bx,CPOFFSET UpdatePrs
	call	ForEachPrsInPlaceCPSav	;Preserve callers oRs
DelGoodExit:
	mov	ax,sp			;return TRUE (non zero)
DelExit:
	or	ax,ax			;set condition codes for caller
cEnd

;-------------------------------------------------------------------
;    If delete would prevent continuing, & user wants to back out of edit,
;       if bigEditState != BIG_EDIT_FALSE, then bigEditState = BIG_EDIT_CANCEL
;       return without changing anything
;
DelBackOut:
	cmp	[bigEditState],BIG_EDIT_FALSE
	je	DelNotBigEdit		;brif not in a BigEdit
	mov	[bigEditState],BIG_EDIT_CANCEL
DelNotBigEdit:
	sub	ax,ax			;return FALSE
	jmp	SHORT DelExit



;**************************************************************
; ushort FAR TxtChange(otxDelFirst, otxDelLast, fNoInsert)
;
; Purpose:
;	The editor or ASCII Loader calls TxtChange() to
;	replace zero or more lines with zero or 1 line of text
;	in the current text table.  If no new text is to be inserted,
;	but only deleted, call TxtChange with fNoInsert <> 0.
;	TxtDescan() should be called before this, to ensure that
;	the text table is descanned to SS_PARSE state.
;
; Note: This function need not worry about the case where the
;	user is trying to insert something between a line with $INCLUDE
;	and an included line below it, because the user interface
;	does not allow ANY editting while 'View/Include Files' is active.
;
; Entry:
;	grs.oMrsCur, grs.oPrsCur have their usual meaning
;	ps.bdpSrc contains source line to be inserted
;	parm1: ushort otxDelFirst - text table offset to opBol
;	   opcode for 1st line to delete.  It also indicates where
;	   new line is to be inserted.
;	parm2: ushort otxDelLast - text table offset to opBol
;	   opcode beyond last line to delete
;	parm3: ushort fNoInsert - non-zero if no pcode should be inserted
;	   (i.e. only text deletion should occur
;
; Exit:
;	If no errors were encountered,
;	   the return value = txtErr.errCode = 0.
;	Else if an error occurred which we will overlook at entry time,
;	   but which must be considered fatal when we are going through
;	   each module's ReParse list in preparation to execute the program,
;	   return value = 0,
;	   txtErr.errCode = an offset into the QBI Message Table
;	   (MSG_xxx) or, if high bit is set, ps.bdpError contains the
;	   parser-built ASCII error message,
;	   The text is inserted in text table in an opReParse opcode.
;	   txtErr.fDirect is set to FALSE,
;	   txtErr.oMrs identifies the module with the error,
;	   txtErr.oPrs identifies the procedure (if any) with the error,
;	   txtErr.otx is an offset into the text table where the error
;	 was detected (otxDelFirst).
;	   txtErr.oSrcErr contains the column offset into the source line
;	 to the offending token.
;	Else if its a really serious error (like out-of-memory or syntax error),
;	   all txtErr fields are set as above, and return value = txtErr.errCode
;
; Major Steps of Algorithm:
;	 Delete the pcode to be replaced (taking some special action for
;	    some opcodes being deleted), giving user a chance to
;	    back out of edit if edit would prevent continuing.
;	 Parse line to be inserted, checking for variable manager/syntax errors,
;	    again giving user a chance to back out of the edit
;	 Scan pcode to be inserted for rude edits, again giving user a chance
;	    to back out of the edit.  This pcode scan can result in calling
;	    CantCont(), ModuleRudeEdit(), SystemDescan().
;	 If statement contains variable mgr/syntax errors, change pcode to
;	    be inserted to an opReParse, which has the actual ASCII source
;	    as an operand.
;	 Insert the new pcode (taking some special action for some opcodes
;	    being inserted).
;
;**************************************************************
cProc	TxtChange,<PUBLIC,FAR,NODATA>,<si,di>
	parmW	otxDelFirst
	parmW	otxDelLast
	parmW	fNoInsert
	localW	fInclude		
 	localW	cbIns
	localW	result
	localW	otxMrsDelFirst
	localW	oRsPreParse
	localW	oPrsPreParse
	localW	otxEndProc
	localB	flagsPrsPreParse
	localB	flagsTc
		FTC_GotEndProc		EQU 1
		FTC_GotEnterProc	EQU 2
cBegin
 DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<TxtChange err1>


	DbChk	Otx,otxDelFirst		;error if > txdCur.bdlText.cbLogical
	DbChk	Otx,otxDelLast		;error if > txdCur.bdlText.cbLogical

	mov	si,[otxDelFirst]
	sub	ax,ax
	mov	[txtErr.TXER_errCode],ax
	mov	[ps.PS_errCode],ax
	mov	[result],ax

	;We need to init these vars in case we don't call ParseLine.
	;(i.e. maybe the caller is only deleting text)
	mov	[flagsTc],al		;default local flags to 0
	mov	[fInclude],ax		;assume we'll see no $INCLUDE directive

	mov	ax,[grs.GRS_oPrsCur]
	mov	[oPrsPreParse],ax

	cmp	[bigEditState],BIG_EDIT_ACTIVE
	je	BigEditActive		;BpDeleted set by first BigEdit Call
					; to TxtChange.
	and	[flagsTm],NOT FTM_BpDeleted ;clear BP deleted flag

BigEditActive:
	cmp	[bigEditState],BIG_EDIT_CANCEL
	jne	TcNotBECancel		;brif not backing out of BigEdit
	jmp	TcRet			;backout of BigEdit if user CANCELed

TcNotBECancel:
	FLoadActive
	je	TcNotLoading		;brif not loading a file
	jmp	TcLoading1		;brif LOADing a file - makes ASCII
					; load MUCH faster.

	;We're not loading, make sure there is enough memory for the
	;inserted and deleted text, otherwise, it would be possible for
	;Search/Change to loose an existing line entirely - too rude.
	;We loosely approximate size of pcode being inserted as 200.
	;Don't check it for edits of immediate (FM2_File=0), since that
	;could prevent users from executing a SYSTEM statement in
	;the immediate window.
	
TcNotLoading:
	test	[mrsCur.MRS_flags2],FM2_File
	je	NotOmErr		;brif not editing a file
	cmp	[fNoInsert],0		;is there any text to parse and insert?
	jne	NotOmErr		;brif not - no need to reserve space
					; otherwise, user could get out of
					; memory, and not be able to delete
					; any text to recover - locked up.
	PUSHI	ax,200d
	call	TxtFree
	jne	NotOmErr
	jmp	TcOmErr
NotOmErr:


;********************* start of revision [56]
;No edits on pcode tables are allowed if there is a return address to
;the direct mode buffer and the direct mode buffer contains a label reference
	mov	al,[grs.GRS_flags]	
	and	al,FG_RetDir+FG_OtxInDir
	cmp	al,FG_RetDir+FG_OtxInDir
	jne	@F			;brif NOT (RetDir & OtxInDir)
	test	[mrsCur.MRS_flags2],FM2_NoPcode 
	jnz	@F			;brif this not a pcode buffer
	call	AskCantCont_CP		;ask user if he wants to continue
djmp	jz	TcRetGoDirect		;brif user wants to backout
@@:
;*********************** end of revision [56]

	;we're not loading, set DEFtypes etc. based on insert point.
	;if in module, traverse DEF-FN chain to otxDelFirst
	;and if we're inside a DEF-FN, PrsActivate that prs
	
	mov	bx,si			;bx = otxDelFirst
	cmp	[bigEditState],BIG_EDIT_ACTIVE 
	jne	TcFindPrs		;If not in a big edit, use passed
					; otxDelFirst.

	;If a big edit is active, we need to stop searching the DEF FN chain
	; at otxBigIns, since txt change gets called multiple times for a
	; big edit.  If we didn't do this, splitting a line immediately
	; prior to a DEF FN could cause us to search a Bogus DEF FN chain.

	mov	ax,[otxBigIns]		;get oTx for start of big edit
.errnz	UNDEFINED+1			
	inc	ax			;UNDEFINED if first call to txtchg
	je	TcFindPrs		;brif so, use oTxDelFirst
	dec	ax			;get back otxBigIns
	xchg	ax,bx			;bx = oTxBigIns
TcFindPrs:				
	push	bx			;pass offset to 1st byte of edit
	call	OPrsOfOtx		;ax = oPrs if si falls in DEF FN
	inc	ax			;test for UNDEFINED
	je	TcNotDefFn	

	;if we are in a big edit, we can't depend upon DEF-FN chain after
	; the first call to TxtChange.	Therefore, ask rude edit, to ensure
	; that there is no prs for the DEF-FN.	Subsequent calls will use
	; the DEF FNs mrs.
	cmp	[bigEditState],BIG_EDIT_ACTIVE
	jne	TcNotBigE		;DEF-FN chain is ok.

; Assert that TcUndo won't take unexpected actions for early termination.
DbAssertRel [cbBigIns],e,0,CP,<TxtChange: DEF FN-error1>
DbAssertRel [bdlTxtScrap.BDL_status],e,NOT_OWNER,CP,<TxtChange: DEF FN-error 2>
	call	AskRudeEdit		;see if user wants to back out of edit
	jne	short TcNotDefFn	;use module Rs.
	jmp	TcUndo			;brif user wanted to back out of edit


TcNotBigE:
	dec	ax			;restore ax = oPrs we're in
	push	ax			;otxDelFirst falls within a DEF FN
	call	PrsActivateCP		;activate it (for var mgr)

;     *--------------------------------------------------------------
;     * Examine the block of pcode being deleted opcode by
;     * opcode, taking special action for each opcode of
;     * interest.  May result in AskCantCont, ModuleRudeEdit, SystemDescan.
;     * 
;     *---------------------------------------------------------------

TcNotDefFn:
	test	[mrsCur.MRS_flags2],FM2_NoPcode
	jne	TcInBigEdit		;brif editing document or immediate
					; window - no need to ever back out
					; of edit.  If we didn't do this,
					; we could get out-of-memory error
					; when trying to delete immediate window
					; history, which we do when we're
					; trying to recover from tight memory
	cmp	[bigEditState],BIG_EDIT_FALSE
	jne	TcInBigEdit		;brif we're in a BigEdit, if so
					;TxtStartBigEdit already set this
					;flag, and TxtDelete may have been
					;called independantly of TxtChange
	mov	[fFillScrap],1		;tell TxtDelete to copy text to scrap
					; so we can back-out of edit
TcInBigEdit:
	push	si			;pass otxDelFirst
	push	[otxDelLast]
	call	TxtDelete		;delete the text
	mov	[fFillScrap],FALSE
	jne	TcDelOk			;brif TxtDelete returned FALSE in ax

TcRetGoDirect:				
	mov	[result], MSG_GoDirect	

J1_TcRet:
	jmp	TcRet			;brif user wants to back out of edit
					;just return without doing anything

;Set up ps.tEtCur[] with the default types for this point in the source
;
TcDelOk:
					;save current oRs for reactivation
					; after Redefining/Deleting undefined
					; prs entries. It is guaranteed that
					; that this is safe, since SUB/FUNCs
					; only get PrsFreed at ?rsDiscard time.
					; DEF FNs have the Mrs active at edit
					; time.
	call	ChkAllUndefPrsSaveRs	;Find new "defining" references for all
					; Prs entries which had defining.
					; references deleted. If no more
					; references to prs exist, free it.
	cmp	[fNoInsert],0		;is there any text to parse and insert?
	jne	J1_TcRet		;brif not - finished

; We can't call OtxDefTypeCur if accumulating opReparses for BigEdit,
; since the linked list are not updated until we start processing the
; reparse list at TxtEndBigEdit time.  The processing of the reparse
; list will insure the the DEF type state is accurate.

	cmp	[bigEditState],BIG_EDIT_ACTIVE
	je	TcDoReParse		;brif we're accumulating BigEdit
					;changes, but TxtEndBigEdit has not
					;been called yet
	mov	ax,si			;ax = text offset
	call	OtxDefTypeCur

TcLoading1:
	test	[mrsCur.MRS_flags2],FM2_NoPcode
	je	TcNotDoc		;brif not a document module

	;this module is not BASIC source, just text (maybe Scrap,
	; command window, document file)
	
TcDoReParse:
	call	ResetDstPbCur		;so ParseUndo won't do anything
	jmp	TcReParse1

;Parse the source line to pcode
TcNotDoc:
	;tell parser (and type manager) to recognize any TYPEs which have been
	; declared before the place this line is being inserted.  This
	; prevents forward referencing of types, which BASCOM cannot support.
	
	mov	[ps.PS_otxLine],si	

	;Call the parser to parse the source line.  grs.oPrs is updated
	;if a SUB/FUNCTION/DEF statement for an as yet undefined procedure
	;is parsed, in which case, the we insert the text at the beginning
	;of the new text table.
	
	mov	ax,[grs.GRS_oPrsCur]
	mov	[oPrsPreParse],ax
	mov	ax,[grs.GRS_oRsCur]	;save Entry so we can tell UI to
	mov	[oRsPreParse],ax	; display new text table if we create
					; a new SUB/FUNCTION.
	mov	al,[prsCur.PRS_flags]	;get prs flags in case we insert a
	mov	[flagsPrsPreParse],al	;SUB/FUNCTION statement.  If we are
					; just renaming then FP_DEFINED will
					; have be cleared when the Sub/Func
					; was deleted.

Retry:
	mov	[ps.PS_flags],0
	call	ParseLine
	jnc	TcNoParseErr		;brif parser encountered no error

	;ParseLine encountered some error
	mov	ax,[oPrsPreParse]
	call	TxtParseUndo
	test	[ps.PS_flags],PSF_UndoEdit
	jne	J1_TcUndo		;brif user said he wants to back out
					; of the edit while we were in ParseLine
					; (i.e. ParseLine called AskCantCont)
	test	BYTE PTR [ps.PS_errCode + 1],PSERR_fRude / 100h
	je	TcNotRude		;brif error isn't cause for a rude-edit

	;Variable manager returned an error code which
	;means a RudeEdit is being performed.
	;Save the line in an opReParse, but do not report the error to
	;the user.  SsDescan the module to SS_Rude (if the user wants
	;to go through with the edit).
	;See if user wants to back out of edit or descan to SS_RUDE
	
	call	AskRudeEdit		;see if user wants to back out of edit
	jne	TcNotRude		;brif user didn't back out of edit
J1_TcUndo:
	jmp	TcUndo			;undo delete (if any) and return

;See if the parser wants us to try parsing this line again.  This can
;happen when:
; We saw something that made us need to ModuleRudeEdit, but part
;     of the line's pcode had already been emitted in SS_PARSE
; Variable manager could not add a variable, because variable heap
;     was locked (because we can CONTinue).  Parser called AskCantCont
;     and now wants us to try again (much easier than trying to call
;     varmgr again from within parser).
;
TcNotRude:
	test	[ps.PS_flags],PSF_fRetry
	jne	Retry			;brif ParseLine wants us to try again
	jmp	SHORT TcParseErr	

TcNoParseErr:				
	
;     *--------------------------------------------------------------
;     * At this point, source line has been parsed with no errors.
;     * Examine the block of pcode being inserted opcode by
;     * opcode, calling a text-mgr routine for each opcode of interest.
;     * Each of these routines in addition to doing other work,
;     * returns a value which will cause either
;     * No Action, ModuleRudeEdit(), SystemDescan(), or CantCont().
;     * 
;     * NOTE: When reviewing changes to this block, make sure it isn't possible
;     * to take some state-changing action, then encounter an error which
;     * causes the edit to be discarded (AskCantCont or MakeOpReParse).
;     * In general, it is safer to 'remember' state changing actions to
;     * be taken, and take them when the pcode has actually been inserted.
;     *
;     *--------------------------------------------------------------

	call	ChkAllUndefPrsSaveRs	;check all undefed prs entries
					;in case this was a rename

	push	[ps.PS_bdpDst.BDP_pb]
	PUSHI	ax,<CODEOFFSET tOpPreIns>
	call	TxtFindOpDS		;ax = ptr to 1st interesting opcode
PiLoop:
	cmp	dl,PI_opEot
	je	J1_PiDone		;brif done with loop
	xchg	di,ax			;di = ptr to current opcode
	sub	dh,dh			;dx = dispatch index
	shl	dx,1			;dx = dispatch offset
	mov	ax,di			;ax = pointer to opcode of interest
	sub	di,[ps.PS_bdpDst.BDP_pb] ;convert ptr to offset in case
 					; dispatched function causes
					; ps.bdpDst to move
	mov	bx,dx			;bx = dispatch offset
	jmp	WORD PTR cs:PiDispatch[bx]

;All the PiOpxxx dispatches either branch to an error handler, or PiNext
PiNext:
	add	di,[ps.PS_bdpDst.BDP_pb] ;convert offset back to pointer
	push	di			;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpPreIns>
	call	TxtFindNextOpDS		;ax = pointer to next opcode of interest
	jmp	SHORT PiLoop

J1_PiDone:
	jmp	SHORT PiDone		;finished with loop

TcParseErr:				
	mov	ax,[ps.PS_oSrcErr]
	mov	[txtErr.TXER_oSrc],ax
	mov	ax,[ps.PS_errCode]
	mov	dx,ax
	.errnz  PSERR_fAsciiMsg - 8000h	;TxtChange callers assume it is high bit
	and	ah,(PSERR_fAsciiMsg + PSERR_errCode) / 100h
					;mask off parser flags
	;pass error code in ax to TcReParse or TcAlert
	test	dh,PSERR_fAlert / 100h
	jne	TcAlert			;serious error, event at entry time,
					; let alone reparse time
	jmp	SHORT TcReParse		;brif not a serious error

;ax = error code
TcAlert0:
	mov	[txtErr.TXER_oSrc],UNDEFINED
					;for txtmgr detected errors, we cant 
					;detect the column, compute from otx
	
	;Encountered some error, make sure entry prs is active before
	;Undoing the change.  This solves the bug where you enter a
	;SUB line and the SUB already exists, but is undefined
	; (SUB stmt is a reparse).  The line parses without error, and
	;the parser changes to the new prs.  When we call PrsMake, we
	;get a duplicate definition error.  We were getting here with
	;the wrong prs active.
	
	mov	bx,[oPrsPreParse]
	cmp	bx,[grs.GRS_oPrsCur]
	je	TcAlert 		;brif parser didn't move into a new PRS
	push	ax			;save error code
	push	bx
	call	PrsActivateCP		;get back to module level/old prs
	pop	ax			;recover error code
TcAlert:
	cmp	[fSyntaxCheck],0
	je	TcReParse		;brif user has disabled editor
					; syntax error reporting
	mov	[result],ax		;serious error - return code in ax
TcReParse:
	mov	[txtErr.TXER_errCode],ax
TcReParse1:
	mov	ax,[oPrsPreParse]
	call	TxtParseUndo		;undo any changes to prs table or
					; name table which parsing this
					; line caused.

	test	[ps.PS_flags],PSF_fRudeIfErr
	je	MakeOpRp

	;Some irreversible action took place, like calling varmgr to
	;create a CONSTant, and then some error took place.  The user
	;already said he didn't want to CONT before PSF_fRudeIfErr was
	;set, so its ok to blow away the module's variable table.
	
	call	ModuleRudeEdit	
MakeOpRp:
	call	MakeOpReParse
	and	[flagsTc],NOT (FTC_GotEndProc+FTC_GotEnterProc)
	jmp	SHORT TcPrescanDone	;don't call LoadEnterProc, LoadExitProc

;si = otxDelFirst
PiDone:
	FLoadActive
	jne	TcPrescanDone		;brif LOADing a file
	test	[txdCur.TXD_flags],FTX_mrs
	jne	TcPrescanDone		;brif we're in a module's text table

	call	ChkInsProc		;see if valid line to insert in proc
	je	TcPrescanDone		;brif valid line
	jns	TcAlert0		;brif illegal
	jmp	J1_TcUndo		;user wants to back out of edit

;MakeOpReParse could have resulted in an out-of-memory error
;check for it
;si = otxDelFirst
;
TcPrescanDone:
	mov	ax,[ps.PS_errCode]
	cmp	al,ER_OM
	je	J1_TcOmErr		;brif out-of-memory error

;     *--------------------------------------------------------------
;     * At this point, the user does not want to back out of the edit
;     * for the sake of edit & continue, and we are not going to
;     * convert the line to an opReParse due to errors.
;     *--------------------------------------------------------------

	sub	[ps.PS_bdpDst.BDP_cbLogical],2 ;don't count opEot as part
					   ; of the line to be inserted

;make room for new text by copying old text up in memory
;making sure there's enough free space in the current text table
;to insert the pcode we want to insert
;si = otxDelFirst
;
TcDoMove1:
	mov	ax,[ps.PS_bdpDst.BDP_cbLogical]
	mov	[cbIns],ax
	call	TxtInsert		;insert ps.bdpDst in text table @ si
	je	J1_TcOmErr		;brif out-of-memory

;     *--------------------------------------------------------------
;     * If we've gotten a SUB or FUNCTION statement
;     *    Move SUB/FUNCTION statement to a new PRS text table,
;     *    generating synthetic DEFxxx statements, and keeping leading
;     *    comments in SUB/FUNCTION's text table.
;     *
;     * NOTE: LoadEnterProc modifies ps.bdpDst
;     * It also moves REMs from the module table to the start of
;     * the procedure text table, since these REMs may be a proc header.
;     * otxDelFirst (si) is updated to reflect this insert.
;     * 
;     *--------------------------------------------------------------
;
TcAfterIns:
	test	[flagsTc],FTC_GotEnterProc
	je	TcNoEnterProc		;brif not inserting SUB/FUNC stmt
	sub	si,[txdCur.TXD_bdlText_cbLogical]
	push	[otxMrsDelFirst]
	call	LoadEnterProc		;move pcode from module to proc tbl
	jne	TcNotOm3
	call	PrsDeactivate		;make module active for error recovery
J1_TcOmErr:
	jmp	TcOmErr			;brif out-of-memory error

TcNotOm3:
	;update otxDelFirst after copying leading remarks, defints etc. to prs
	add	si,[txdCur.TXD_bdlText_cbLogical]

;     *--------------------------------------------------------------
;     * Call TxtInsUpdate to examine every opcode which was inserted
;     * in the text table, taking any opcode specific action required.
;     * 
;     *--------------------------------------------------------------
;
TcNoEnterProc:
	mov	bx,si			;bx = updated otxDelFirst
	add	bx,[cbIns]		;bx = offset beyond inserted pcode
	call	TxtInsUpdate		;update program counter and other
					; static entries which are affected
					; by pcode movement.
	je	J1_TcOmErr		;brif out-of-memory

	cmp	[bigEditState],BIG_EDIT_FALSE
	je	TcNotInBigEdit		;brif we're not accumulating BigEdit
					; changes
	test	[flagsTc],FTC_GotEnterProc 
	jnz	TcNoIncr		;don't increment cbBigIns if proc
	mov	ax,[cbIns]		
	add	[cbBigIns],ax		;remember how much text we've inserted
TcNoIncr:
	cmp	si,[otxBigIns]
	jae	TcNotInBigEdit
	mov	[otxBigIns],si		;remember base of inserted pcode
					; (so we can later back out of BigEdit)

;     *--------------------------------------------------------------
;     * If we've gotten an END SUB or END FUNCTION statement
;     * restore the module's text table.
;     * 
;     *--------------------------------------------------------------
;
TcNotInBigEdit:
	test	[flagsTc],FTC_GotEndProc
	je	TcNoExitProc
	FLoadActive
	je	TcNoExitProc		;brif not LOADing a file
	call	LoadExitProc		;clean up, return to Module level
	jnc	TcNoExitProc
	jmp	TcOmErr			;brif out of memory error

TcNoExitProc:
	test	[flagsTm],FTM_BpDeleted
	je	TcNoBp			;brif breakpoint was not deleted
	push	si			;pass otxDelFirst
	call	LnOfOtx			;ax = line text was inserted
	push	ax
	call	FAR PTR ToggleBp	;set breakpoint
	and	[flagsTm],NOT FTM_BpDeleted ;clear BP deleted flag
	cmp	[bigEditState],BIG_EDIT_ACTIVE
	jne	TcNoBp			;brif we're not accumulating BigEdit
					; changes
	add	[cbBigIns],2		;2 more bytes for inserted BP

;     *--------------------------------------------------------------
;     * If this line had an $INCLUDE 'filename' directive, recurse here.
;     *--------------------------------------------------------------
;
DbPub TcNoBp
TcNoBp:
	cmp	[fInclude],NULL
	je	J_TcRet 	       ;brif line contained no $INCLUDE
	cmp	[txtErr.TXER_errCode],0
	je     @F
J_TcRet:
	jmp	SHORT TcRet		; brif $INCLUDE line has error in it
@@:

	PUSHI	ax,<dataOFFSET b$SdBuf1>;bx = SD for filename.
	inc	[cInclNest]
	mov	ax,si			;ax = otxDelFirst
	add	ax,[cbIns]		;ax = offset beyond inserted text
	push	ax			;pass insertion point to LoadFile
	inc	[fLoadInclude]		; inform LoadFile of $INCLUDE
	call	LoadFile		;merge the $INCLUDE file
	dec	[fLoadInclude]		; reset flag
	dec	[cInclNest]
	or	ax,ax
	jz	TcNoErr			;brif no $INCLUDE error
					; If we fell into TcRet, we would
					; be over-writing errors already
					; recorded by recursive calls to
					; TxtChange via LoadFile.
TcIncErr:				
	push	ds
	pop	es			;ES=DGROUP for stosw
	mov	di,[ps.PS_bdpDst.BDP_pb]
	mov	[ps.PS_bdpDst.BDP_cbLogical],6
	mov	ax,opReParse
	stosw				;insert opReParse in text table
	mov	ax,2
	stosw				;insert cb field in text table
					;garbage link field is ok for now

	xchg	ax,si			;pass otxDelFirst in ax
	call	TxtSkipOp		;ax = adr of 1st opcode after Bol
	xchg	si,ax			;si = adr of place to insert opReParse
	call	TxtInsert
	mov	bx,si
	add	bx,6			;bx = offset beyond inserted pcode
	call	TxtInsUpdate

TcRet:
	cmp	[txtErr.TXER_errCode],0
	je	TcNoErr

	;tell caller which Mrs & Prs the error occurred in
	mov	[txtErr.TXER_fDirect],FALSE 
	mov	ax,[grs.GRS_oRsCur]
	mov	[txtErr.TXER_oRs],ax
	mov	[txtErr.TXER_otx],si
TcNoErr:
	cmp	[bigEditState],BIG_EDIT_FALSE
	jne	TcExit			;brif in a BigEdit, accumulate changes
	cmp	[bdlTxtScrap.BDL_status],NOT_OWNER	
	je	TcExit
	PUSHI	ax,<dataOFFSET bdlTxtScrap> 
	call	BdlFree			;release DELETE scrap
TcExit:
	test	[txdCur.TXD_flags],FTX_mrs ;make sure current oRs has a
					; text table
	je	TcExit1 		;brif sub or function is active
	call	PrsDeactivate		;deactivate DEF FN if any

TcExit1:
	DbChk	TxdOps			;see if TxtChange inserted bad opcode
	mov	ax,[result]		;return error code or 0 in ax
cEnd

TcOmErr:
	mov	ax,ER_OM		;out-of-memory error
	mov	[txtErr.TXER_errCode],ax
	mov	[result],ax
J2_TcRet:
	jmp	SHORT TcRet		;just return

;The user has decided to back out of the edit, because it would prevent CONT
;reinsert deleted text (if any).
;
; If bigEditState != BIG_EDIT_FALSE
;    If cbBigIns > 0, TxtDelete(otxBigIns, otxBigIns + cbBigIns)
;    bigEditState = BIG_EDIT_CANCEL
; If bdTxtScrap is not empty, Insert bdTxtScrap at otxScrap
;
TcUndo:
	mov	ax,MSG_GoDirect
	mov	[result],ax		;tell editor to stop the edit
	mov	[txtErr.TXER_errCode],ax
	cmp	[bigEditState],BIG_EDIT_FALSE
	je	NotInBigEdit		;brif not in BigEdit
	mov	cx,[cbBigIns]
	jcxz	TcNoIns			;brif no text has been inserted
	mov	ax,[otxBigIns]
	push	ax	
	add	ax,cx			;ax = offset beyond inserted text
	push	ax
	call	TxtDelete		;delete the inserted text
					; doesn't place deleted text in scrap
					; either because fFillScrap is false
					; or scrap is already in use
TcNoIns:
	mov	[bigEditState],BIG_EDIT_CANCEL
NotInBigEdit:
	cmp	[bdlTxtScrap.BDL_status],NOT_OWNER 
	je	J2_TcRet		;brif no text deleted - done with undo

	mov	ax,[bdlTxtScrap.BDL_cbLogical] ;ax = number of bytes in scrap
	cmp	ax,2			; check for empty scrap
	jb	J2_TcRet		; bfir empty scrap - done.

	mov	[cbins],ax		;store this in [cbins] static
;  CALL TxtInsScrap(si = otxScrap)	[9]
	mov	si,[otxScrap]		;si = insertion point in 
					;	current text table
	call	TxtInsScrap		;copy scrap back into text table and
					;	release bdlTxtScrap
	jz	TcOmErr
	jmp	TcAfterIns


;
;**************************************************************
; ChkInsProc
; Purpose:
;	We're not loading and we're inserting a line in a SUB/FUNCTION
;	text table.  This function sees if it is legal.
; Entry:
;	si = text offset where line is being inserted in SUB/FUNCTION
; Exit:
;	ax = error code if illegal line to insert in a SUB/FUNCTION
;	   = 0 if no error
;	   = negative number if user wants to back out
;	condition codes set based on value in ax
;
;**************************************************************
DbPub	ChkInsProc
ChkInsProc PROC NEAR
	DbChk	Otx,si			;error if > txdCur.bdlText.cbLogical
	push	di			;preserve caller's di
	test	[prsCur.PRS_flags],FP_DEFINED
	je	CiNotDefined		;brif proc has no SUB/FUNCTION stmt
	cmp	[prsCur.PRS_otxDef],si
	jae	CiNotDefined		;brif insertion is before definition
	jmp	CiDone			;insertion is after definition
CiNotDefined:
	;Trying to insert something into SUB/FUNCTION window before
	;the SUB/FUNCTION definition.  Only certain things can
	;be inserted before the definition - make sure this is
	;one of them
	
	push	si			;save si
	sub	si,si			;set fNonBolSeen=fSubOrFuncSeen=false
					; the high bit set means an opStSub,
					; or opStFunction has been seen,
					; when si is non-zero then we have
					; seen some non-bol/non opEot opcode.
	mov	di,[ps.PS_bdpDst.BDP_pb] ;di->start of pcode to insert (in DS)
;di = otxCur (into parser's DS pcode buffer ps.bdpDst)
CiLoop:
	mov	ax,[ps.PS_bdpDst.BDP_pb]
	add	ax,[ps.PS_bdpDst.BDP_cbLogical]
	dec	ax
	dec	ax
	cmp	di,ax
	jae	CiLoopExit		;brif done scanning line to be inserted
	push	di			;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpPreProc>
	call	TxtFindNextOpDS		;ax = offset to next interesting opcode

	cmp	dl,PREPROC_bolMax	;are we looking at a bol opcode?
	jbe	CiLoopNext		;brif so
	cmp	dl,PREPROC_opEot	;at end of text?
	je	CiLoopNext		;brif so
	cmp	dl,PREPROC_opStEndProc	;is this an opStEndproc?
	jne	CiNotEndProc		;brif not
	or	si,si			;have we seen preceding OpStSub/Function
	jns	CiInvBeforeProc 	;brif not, illegal before SUB/FUNCTION
CiNotEndProc:
	cmp	dl,PREPROC_SubOrFuncMin ;is this opStSub or opStFunction?
	jb	CiNotSubOrFunc
	or	si,8000H		;set sub of function seen flag
CiNotSubOrFunc:
	or	si,1			;set fNonBolSeen flag

CiLoopNext:
	push	di			;pass otxCur
	mov	di,ax			;di = otxNext
	sub	ax,ax			;just get next opcode (not next found in
	push	ax			; some list)
	call	TxtFindNextOpDS		;ax = offset to next opcode
	cmp	ax,di
	je	CiLoop			;brif opcode is in list
	or	si,si			;have we seen a SUB or FUNCTION?
	js	CiLoop			;brif so - ok if after SUB.

	;some invalid opcode was found in line
CiInvBeforeProc:
	pop	si			;recover otx
	mov	ax,MSG_InvBeforeProcDef
	jmp	SHORT CiExit

CiLoopExit:
	or	si,si			;did we see a non-bol?
	pop	si			;recover otx
	jne	CiDone			;brif non-Bol seen
	cmp	[txdCur.TXD_bdlText_cbLogical],CB_EMPTY_TEXT ;is txt tbl empty?
	je	CiDone			;brif so, allow an opBol

;  Inserting a blank line, see if user will allow a single quote rem instead
;  We do this so ascii save/ascii load will be able to migrate
;  comment blocks correctly with text tables.
;

	sub	ax,[ps.PS_bdpDst.BDP_pb] ;convert ptr to offset, AskMakeRem
					; can cause heap movement
	xchg	ax,di			;di = offset to opEot
	call	AskMakeRem		;ask if usr would like a comment instead
	add	di,[ps.PS_bdpDst.BDP_pb] ;convert offset back to ptr to opEot
	or	ax,ax			;does user want to back out of edit?
	mov	ah,-1			;prepare for back-out
	je	CiExit			;return high bit set for backout
	mov	ax,opQuoteRem		;get single quote rem opcode
	xchg	ax,[di] 		;xchg opQuoteRem for opEot
DbAssertRel ax,e,opEot,CP,<ChkInsProc: expected opEot>
	push	ax			;push opEot (will be emitted last)
	sub	ax,ax
	push	ax			;push column for opQuoteRem
	inc	ax
	inc	ax			;2 bytes of REM (for column field)
	call	Emit16_AX		;emit cbRem
	call	Emit16			;emit Column (already on stack)
	call	Emit16			;emit opEot to ps.pbDst
					; (already on stack)
CiDone:
	test	[prsCur.PRS_flags],FP_ENDPROC
	je	CiOk			;brif proc has no END SUB/FUNCTION
	cmp	[ps.PS_bdpDst.BDP_cbLogical],4
	jbe	CiOk			;brif not inserting a blank line

	;user is trying to append something more than a blank
	;line (opBol,opEot).  See if its beyond the END SUB/FUNCTION statement
	
	sub	ax,ax			;search from top of table
	push	ax
	PUSHI	ax,<CODEOFFSET tOpEndProc>
	call	TxtFindOp		;ax = offset to END SUB/FUNCTION
	cmp	ax,si
	jae	CiOk			;brif inserted before END SUB/FUNCTION
	mov	ax,MSG_EndProc
	SKIP2_PSW			;skip next instruction
CiOk:
	sub	ax,ax			;return FALSE (no error)
CiExit:
	or	ax,ax			;set condition codes for caller
	pop	di			;restore caller's di
	ret
ChkInsProc ENDP



;-------------------------------------------------------------------
; Opcode specific code which gets executed BEFORE pcode is inserted
; in text table.  Pixxx stands for Pre Insert <opcode name>
; All of this 'functions' enter with:
;     si = otxDelFirst
;     di = offset into pcode buffer 'ps.bdpDst'
;     ax = pointer into pcode buffer 'ps.bdpDst'
; When they've completed their work, they branch to one of:
;   PiNext - normal exit when everything looks ok
;   TcUndo - when user has been warned that the edit would prevent
;     continuing, and the user responded CANCEL.
;   TcAlert0 - ax = error code, when error is to be reported to user
;     and line is to be saved as an opReParse
;   TcReParse - ax = error code, when error is NOT to be reported to user
;     immediately (unless we're going through the reparse loop in preparation
;     to execute a direct mode statement).  The line is saved as an opReParse.
;   TcOmErr - if some memory allocation couldn't be satisfied.
;
;-------------------------------------------------------------------

;extract filename out of OpStInclude(cnt,filename) and/or op_Include
;di points to (opcode,cb,text)
DbPub	PiInclude
PiInclude:
	push	ax
	call	AskCantCont_CP		;see if user wants to back out of edit
	pop	ax
	jne	PiInclCont		;brif ok to edit
	jmp	TcUndo			;brif user wanted to back out of edit

PiInclCont:
	cmp	[cInclNest],INCLUDE_DEPTH_MAX
	jb	NotTmf
	or	[flagsTm],FTM_reInclude	;cause all $INCLUDE lines to be
					;reparsed before next RUN
					;so we'll report the error again.
	mov	ax,ER_TMF		;error: Too many files
J2_TcAlert0:
	jmp	TcAlert0

NotTmf:
	push	si			;preserve otxDelFirst
	push	di			;save offset of opcode
	add	di,[ps.PS_bdpDst.BDP_pb] ;convert offset to pointer
	add	di,4			;point to first byte of operand
	push	di			;pass pSrc to CopyBlk

	push	ds
	pop	es			;set es = dgroup
	mov	cx,-1
	mov	al,27H			;search for terminating ' in filename
	repne	scasb
	not	cx			;cx = length of filename including '
	dec	cx			;cx = length of filename
PiOpStInc:
	cmp	cx,FILNAML		;compare length of name 	[8]
					; to length of static buffer
	jb	NotTooBig		;brif OK
	mov	ax,ER_IFN		;report bad filename error
	jmp	SHORT J2_TcAlert0
NotTooBig:
	mov	[fInclude],sp		;set flag for loading of include file
	DbChk	HoldBuf1		;ensure availability of Buf1 [8]
	mov	si,dataOFFSET b$SdBuf1	;si ptr to sd for B$Buf1
	mov	[si.SD_cb],cx		;save filename length
	push	[si.SD_pb]		;pass ptr to B$Buf1
	push	cx			;pass cb to copy
	call	CopyBlk 		;copy $INCLUDE's operand to temp buffer
					; we just 0-terminated filename for
					; FileExists, set cbLogical to real len
	pop	di			;recover opcode offset
	pop	si			;recover otxDelFirst
J1_PiNext:
	jmp	PiNext

DbPub	PiOpStSub
PiOpStSub:
PiOpStFunction:
	FLoadActive
	jne	CheckNestedProcs	;brif loading
	;Parser ensures that we will be in SS_RUDE when an insertion
	;of a SUB or FUNCTION statement is seen.
	cmp	[oPrsPreParse],UNDEFINED ;were we in a sub or function?
	je	NotNestedProcs		;brif not - user entered SUB/FUNCTION
					;statement in module level text table

	;If we are renaming a SUB/FUNCTION, then the deletion of the
	;SUB/FUNCTION line caused the FP_DEFINED flag to be reset.
	;If the user just entered a SUB/FUNCTION statement in a SUB or
	;FUNCTION, the FP_DEFINED bit will still be set.  If the user
	;happened to be trying to insert a SUB or FUNCTION, in a SUB
	;or FUNCTION that has been commented out, then this new SUB/FUNCTION
	;statement will inherit the text table instead of creating a new
	;SUB.
	
	test	[flagsPrsPreParse],FP_DEFINED ;is this a rename?
	je	J1_PiNext		;brif so, prs already exists.
	jmp	SHORT NotNestedProcs

CheckNestedProcs:
	;See if we were in an unterminated SUB/FUNCTION.  If so,
	;then we need to call LoadExitProc to clean up the previous
	;call to LoadEnterProc.
DbAssertRelb [fMergeInSub],e,0,CP,<TxtChange: fMergeInSub TRUE when sub/func ins>

NotMergingInSub:
	cmp	[oPrsPreParse],UNDEFINED ;were we in a SUB/FUNCTION?
	je	NotNestedProcs		;brif not
	push	[grs.GRS_oPrsCur]	;push oPrsCur for PrsActivate after
					;call to load exit proc
	push	[oPrsPreParse]
	call	PrsActivateCP		;activate the previous PRS
	call	LoadExitProc		;clean up it's state
	jnc	NotOmErr2		;brif not out of memory error
PiTcOmErr:				
	jmp	TcOmErr
NotOmErr2:
	xchg	si,ax			;si = otx of insertion point in
					; module's text table
	call	PrsActivateCP		;activate new Prs(oPrs already on stack)

NotNestedProcs:

	;If in ASCII Load, or the user entered SUB or FUNCTION in
	;the current window. Parser called PrsDefine to
	;create the proc's context and make it current.
	;Now make the SUB/FUNCTION's text table
	
	push	[prsCur.PRS_ogNam]	
	push	WORD PTR([prsCur.PRS_procType])
	call	PrsMake
	or	ax,ax			
	jne	JNE_J1_TcAlert0 	;brif PrsMake err

	;Proc definition will be inserted at start of newly created text
	;table.  Remember where we were in module's text table so we
	;can tell LoadEnterProc()
	
	mov	[otxMrsDelFirst],si
	SetStartOtx si			;set otxDelFirst to 0
	or	[flagsTc],FTC_GotEnterProc ;set TRUE
	FloadActive			;don't change window contents if
	jne	J1_PiNext		; ascii loading
	push	[grs.GRS_oRsCur]
	call	WnResetMove		;tell UI that new SUB/FUNC is active
	mov	[txdCur.TXD_lnCursor],1 ;position new window's cursor at line 1

	mov	ax,[ps.PS_bdpDst.BDP_pb] ;bx->start of pcode for cur line
	push	ax			
	PUSHI	ax,<CODEOFFSET tOpEndProc> 
	call	TxtFindOpDS		;dl = index into tOpEndProc table
	cmp	dl,ENDPROC_opStEndProc	;does parse buf already have an
					;end proc (e.g. was it
					;SUB FOO:END SUB)?
	je	J3_PiNext		;brif so - don't insert endproc


	mov	cx,6			;size of 2 bols and opStEndProc
	call	CheckFreeDstBuf 	;see if enough room in parser buf
	jz	PiTcOmErr		;for inserted pcode. brif not

	sub	[ps.PS_bdpDst.BDP_cbLogical],2 ;eat the opEot
	sub	[ps.PS_bdpDst.BDP_pbCur],2 ;eat the opEot
	.errnz	opBol
	sub	ax,ax			;ax = opBol
	call	Emit16_AX		;emit opBol to parser buf to create
					; a blank line
	.errnz	opBol
	sub	ax,ax			;ax = opBol
	call	Emit16_AX		;emit opBol to parser buf
	mov	ax,opStEndProc
	call	Emit16_AX		;emit opStEndProc to parser buf
	mov	ax,opEot
	call	Emit16_AX		;emit opEot to parser buffer
J3_PiNext:
	jmp	SHORT J2_PiNext

PiOpStEndProc:
	mov	ax,MSG_InvMain		;Illegal in main program
	test	[txdCur.TXD_flags],FTX_mrs
JNE_J1_TcAlert0:
	jne	J1_TcAlert0		;brif we're looking at module text table

	mov	ax,MSG_InvIncl		;error msg = illegal in include
	test	[mrscur.MRS_flags2],FM2_Include 
	jne	J1_TcAlert0		;error if this is an include module
	cmp	[cInclNest],0		
	jne	J1_TcAlert0		;error if merging include file

	test	[prsCur.PRS_flags],FP_ENDPROC
	jne	LastNotEndErr		;brif already has END SUB
	call	OtxEndProg		;ax = otxEndProg (end of user pcode)
	cmp	si,ax
	je	PiGotEndProc		;brif inserted at end of text
DbAssertRel	ax,a,si,CP,<TxtChange: insertion point of End Proc beyond opEndProg>
	sub	ax,si			;ax = cbDelta from Insertion point
					; to opEndProg
	push	ax
	GetSegTxtCur			;es = text segment
	pop	bx			;bx = cbDelta

	;look at all opcodes between opEndProg (exclusive) and insertion point
	;don't allow any opcode except opBol (i.e. only allow trivial blank
	;lines from END SUB/FUNCTION to opEndProg. Start search at opcode
	;prior to opEndProg, and terminate at insertion point.
	

EndProcBlankSearch:
	mov	ax,es:[si+bx-2] 	;ax = opcode

.errnz	opBol
	and	ax,OPCODE_MASK		;mask out spaces
	jne	LastNotEndErr		;brif found non opBol opcode
	dec	bx
	dec	bx			;advance to next opcode
	jne	EndProcBlankSearch

PiGotEndProc:
	FLoadActive
	je	NotMergingInSub1	;brif not loading
	cmp	[fMergeInSub],0		;non-zero if MERGING a file into a SUB
	je	NotMergingInSub1
MrgInSubErr:				
	mov	ax,MSG_InvProc		;illegal in SUB/FUNCTION
	jmp	TcReParse		

NotMergingInSub1:
	or	[flagsTc],FTC_GotEndProc ;set TRUE
J2_PiNext:
	jmp	PiNext

;user is inserting END SUB in window that has one, or before end of table
LastNotEndErr:
	mov	ax,MSG_EndProc
J1_TcAlert0:
	jmp	TcAlert0

PiOpStShared:
	test	[txdCur.TXD_flags],FTX_mrs
	je	J2_PiNext		;ok if in procedure text table
	mov	ax,MSG_InvMain		;illegal outside SUB/FUNCTION/DEF FN
	jmp	SHORT J1_TcAlert0

;See if DATA statement is in a SUB or FUNCTION.  If we're ASCII loading,
;the statement will eventually be moved to module level.  If the user
;is editing, give an error.
PiOpStData:
	cmp	[fLoadInclude],FALSE	;[J2]
	jne	InvProcLevel		;[J2]
	FLoadActive
	jne	J2_PiNext		;brif loading (DATA stmts will be
					; moved from prs(s) to mrs at end of
					; load
InvProcLevel:				;[J2]
	test	[txdCur.TXD_flags],FTX_mrs
	jne	J2_PiNext		;ok if not in procedure text table
	jmp	short InvProcErr	;invalid in SUB or FUNCTION

;Opcodes which cause ModuleRudeEdit
;DEF FN within SUB/FUNCTION is caught in prsid.asm's MakeProc
PiOpEndSingleDef:
PiOpStEndDef:
	test	[txdCur.TXD_flags],FTX_mrs
	jne	NotInSub		;ok if not in procedure text table

InvProcErr:
	mov	ax,MSG_InvProc		;illegal in SUB/FUNCTION
	jmp	SHORT J1_TcAlert0

PiOp_Dynamic:
	mov	[fDynArrays],1		;set TRUE for ASCII Load
	jmp	SHORT PiOpStEndType	

PiOp_Static:
	mov	[fDynArrays],FALSE	;set FALSE for ASCII Load
	jmp	SHORT PiOpStEndType	

;Opcodes which cause SystemDescan
; The reason we can't depend on the variable manager to cause a rude edit
; when a NEW common variable is created (as it does for SHARED etc.)
; is because it is legal to have DIM x(1) followed by COMMON x().
; 
PiOpStCommon:				;scanner will rebuild common tables
PiOpStOptionBase0:			
PiOpStOptionBase1:			
PiOpStDefType:
	call	SystemDescanCP		;implicit parms in DECLARE, SUB,
					; FUNCTION stmts need to be rechecked
					; by the scanner

;When we insert an A as B in a TYPE/END TYPE block we need to take
; the system to SS_RUDE to force reevaluation of the TYPE block.
;When we insert a SHARED statement, we need to take the system to SS_RUDE
; for the Variable Manager.
;When inserting a CONST statement, we need to take the system to SS_RUDE,
; so that the rude scanner will evaluate the CONST expression.

NotInSub:
PiOpElemRef:
PiOpShared:
PiOpStConst:
PiOpStEndType:
	cmp	[txdCur.TXD_scanState],SS_RUDE
	jae	J2_PiNext		; speed opt. - we're already rude
	call	AskRudeEdit		;see if user wants to back out of edit
	je	J2_TcUndo		;brif user backed out of edit
	jmp	Retry			;brif user didn't back out of edit
					;re-parse line, won't get back here,
					; because now text table is in SS_RUDE

J2_TcUndo:
	jmp	TcUndo			;undo delete (if any) and return

PiOpStFor:
	dec	[cForDel]		;cForDel = 1+#FOR stmts deleted 
					; by TxtDelete.
	jmp	SHORT PiInsFor

PiOpStForStep:
	dec	[cForStepDel]		;cForStepDel = 1+#FOR STEP stmts deleted
					; by TxtDelete.
PiInsFor:
	je	AskForCont		;brif we've inserted more FORs than
					; were deleted
	call	FindORsFrame		;see if active prs (if any) has a
					; frame on the stack
	or	ax,ax			
	je	J2_PiNext		;brif not found
					;else (can't edit buried prs FOR)
					; because it could require change in
					; frame size
AskForCont:
	call	AskCantCont_CP		;see if user wants to back out of edit
	je	J2_TcUndo		;brif user backed out of edit
	jmp	SHORT J2_PiNext		;brif user didn't back out of edit

; dispatch table used for processing inserted opcodes.
PiDispatch:
	DW	PiOp_Static
	DW	PiOp_Dynamic
	DW	PiInclude		
	DW	PiOpStSub
	DW	PiOpStFunction
	DW	PiOpStEndProc
	DW	PiOpEndSingleDef
	DW	PiOpStEndDef
	DW	PiOpStData
	DW	PiOpStShared
	DW	PiOpStDefType
	DW	PiOpStCommon
	DW	PiOpStEndType
	DW	PiOpStFor
	DW	PiOpStForStep
	DW	PiOpStConst
	DW	PiOpShared
	DW	PiOpStOptionBase0	
	DW	PiOpStOptionBase1	
	DW	PiOpElemRef



;**************************************************************
; TxtParseUndo
; Purpose:
;	We are about to make a line into an opReParse.
;	Any prs's that were created by this call to ParseLine
;	need to be freed.
;	We need to deactivate the active prs if it is
;	going to be freed.
; Entry:
;	ax = oPrs that was active before ParseLine was called.
;
;**************************************************************
cProc	TxtParseUndo,<PUBLIC,NEAR>	
cBegin
	cmp	ax,[grs.GRS_oPrsCur]
	je	NoDeact			;brif parser didn't move into a new PRS
	test	[txdCur.TXD_flags],FTX_mrs
	je	NoDeact			;brif new prs has text table (is
					; probably the result of renaming
					; a prs, and discovering an error
					; after the prs was renamed).
	push	ax
	call	PrsActivateCP		;get back to module level/old prs
NoDeact:
	call	ParseUndo		;We must call this before ModuleRudeEdit
					; or else we will try to free some
					; DEF FN prs's which no-longer exist
	call	ChkAllUndefPrsSaveRs	;check all undefed prs entries
					;in case parser created one before error
cEnd

;**************************************************************
; TxtInsert
; Purpose:
;	Insert a block of pcode in the current text table
; Entry:
;	si = text offset where new pcode is to be inserted
;	ps.bdpDst = buffer to be inserted
; Exit:
;	If out-of-memory, returns ax=0 and psw.z set
;
;**************************************************************
PUBLIC	TxtInsert
TxtInsert PROC NEAR
	push	si			;pass otxInsert
	push	[ps.PS_bdpDst.BDP_cbLogical] ;pass cbIns
	call	TxtMoveUp
	je	TInsExit		;brif out-of-memory error

	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
	push	si			;pass otxInsert
	push	[ps.PS_bdpDst.BDP_pb]	;pass ptr to pcode to insert
	push	[ps.PS_bdpDst.BDP_cbLogical] ;pass cbIns
	call	BdlCopyTo		;copy from DS to text table
	mov	ax,sp
TInsExit:
	or	ax,ax			;set condition codes for caller
	ret
TxtInsert ENDP

;**************************************************************
; TxtInsBdl [40]
; Purpose:
;	Insert text from a bdl into current text table.
;
; Entry:
;	si = text offset where new pcode is to be inserted
;	di -> bdl to be inserted
;	ax = # bytes to be inserted. 
;		The start of the text to be copyied
;		is always StartOtx
; Exit:
;	If out-of-memory, returns ax=0 and psw.z set
;
;**************************************************************
PUBLIC	TxtInsBdl
TxtInsBdl PROC NEAR
	push	ax			;preserve cbInsert
	push	si			;pass otxInsert
	push	ax			;pass cbInsert
	call	TxtMoveUp		;move text starting at otxInsert
					; up by cbInsert bytes if possible
	pop	cx			;cx = cbInsert
	jz	TIBExit			;brif Out-of-Memory
	push	di			;pass source bdl
	SetStartOtx ax			
	push	ax			;pass otxSrc
	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText> ;pass bdlDst
	push	si			;pass otxDst
	push	cx			;pass cbInsert
	call	BdlCopyFromTo		;copy text from the bdl into gap
	mov	ax,sp			;return non-zero
	or	ax,ax			;set condition flag
TIBExit:
	ret
TxtInsBdl ENDP

;*****************************************************************************
; TxtCopyScrap
; Purpose:
;	Copy a block of pcode from the current text table to bdlTxtScrap
;
;	Added as part of revision [9]
;	
; Entry:
;	parm1: otxStart: offset into text table for 1st byte to move
;	parm2: oDst: offset of where to place text in Scrap - if non-zero
;		this leaves a gap at the start of the buffer
;	parm3: cbMove: number of bytes to move
;	parm4: fDelete: true if TxtDelete is to be called to delete the text
;	FLoadActive should return FALSE if links in deleted text are
;	       up-to-date
; Exit:
;	If out-of-memory error, returns ax = 0, else ax = non-zero
;	   condition codes set based on value in ax
;
;*****************************************************************************
cProc	TxtCopyScrap,<PUBLIC,NEAR>,<di>
	parmW	otxStart
	parmW	oDst
	parmW	cbMove
	parmW	fDelete
cBegin
DbAssertRel [otxStart],ae,StartOtx,CP,<TxtCopyScrap: bad otxStart>
DbAssertRel [bdlTxtScrap.BDL_status],e,NOT_OWNER,CP,<bdlTxtScrap in use> 

	mov	di,[cbMove]		;di = cbMove for speed

;  CALL bdlAlloc(&bdlTxtTemp, cbMove + oDst)
	PUSHI	ax,<dataOFFSET bdlTxtScrap>
	mov	ax,di
	add	ax,[oDst]		;ax = cbMove + oDst
	jnz	TcsNotZero
	inc	ax			;cb can't be zero - this is needed
					;by TxtDelete
TcsNotZero:
	push	ax			;total number of bytes to allocate
	PUSHBDL_TYPE  pgtypEBGeneric,ax ; pass sb type for EB version
	call	bdlAlloc
	or	ax,ax
	jz	TcsExit			;exit if out-of-memory	

;  CALL bdlCopyFromTo(&txtCur.bdlText, otxStart, 	- source
;		      &bdlTxtscrap    , oDst,		- destination
;		      cbMove)				- number of bytes
	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
	push	[otxStart]
	PUSHI	bx,<dataOFFSET bdlTxtScrap>
	push	[oDst]
	push	di			;push cbMove
	call	BdlCopyFromTo		;no error is possible (checked above)

;  if (fDelete) CALL TxtDelete(otxStart, otxStart + cbMove)
	cmp	[fDelete],FALSE
	je	TcsReturnTrue		;brif we're not to delete text
	mov	ax,[otxStart]
	push	ax			;pass otxStart
	add	ax,di			;ax = otxStart + cbMove
	push	ax			;pass otxEnd
	call	TxtDelete		;delete pcode from prs's text table

TcsReturnTrue:
	mov	ax,sp			;return TRUE (no error)
TcsExit:
	or	ax,ax			;set condition codes for caller
cEnd

;**************************************************************
; TxtInsScrap
; Purpose:
;	Insert text in Scrap buffer into text table and release 
;	the Scrap buffer.
;
;	Added as part of revision [9]
;
; Entry:
;	si = text offset where new pcode is to be inserted
;	bdlTxtScrap = buffer to be inserted
; Exit:
;	If out-of-memory, returns ax=0 and psw.z set
;
;**************************************************************
PUBLIC	TxtInsScrap
TxtInsScrap PROC NEAR
	push	di			;preserve caller's di
	DbChk	Otx,si			

	mov	di,dataOFFSET bdlTxtScrap 
	mov	ax,[di.BDL_cbLogical]	
DbAssertRel	ax,ae,2,CP,<TxtInsScrap: tried to insert empty scrap> 
					
	call	TxtInsBdl		
	jz	TisExit			

; CALL BdlFree(&bdlTxtScrap)
	PUSHI	ax,<dataOFFSET bdlTxtScrap>
	call	BdlFree	

	mov	ax,sp
TisExit:
	or	ax,ax			;set condition codes for caller
	pop	di			;restore caller's di
	ret
TxtInsScrap ENDP

;--------------------------------------------------------------------
;
; Management of linked lists through a text table's pcode:
;
; TxtInsUpdate is called after ANY pcode has been inserted into
; any text table to update static variables and structures affected
; by the pcode inserted (prs definitions etc.).  Unfortunately,
; we cannot call UpdateLinks every time TxtChange is called during
; ASCII load, because it would be far too slow.  Instead, we call
; it for a block of lines from LoadEnterProc, LoadExitProc, and AsciiMerge.
;
;
; TxtChange TDataEnd TxtPrsInit SaveDeclares ToggleBp
;    |           |      |           |           |
;    +-----------+------+-----------+-----------+
;                       |
;	       TxtInsUpdate LoadEnterProc LoadExitProc AsciiMerge TxtEndBigEdit
;		     |	|	   |	       |	     |	      |
;	   +---------+	+----------+-----------+-------------+--------+
;	   |	     |		   |
;    UpdatePcs UpdatePrs       UpdateLinks
;
;--------------------------------------------------------------------

;**************************************************************
; TxtInsUpdate
; Purpose:
;	Called after TxtInsert to update program counter and other
;	static entries which are affected by pcode movement.
; Entry:
;	si = text offset where new pcode was inserted
;	bx = text offset beyond last byte of pcode inserted
;	If Loading a file (i.e. FLoadActive), or a big edit is active,
;	   UpdateLinks is not called.
;	   This makes ASCII Load and BLOCK CUT/COPY/PASTE MUCH faster
; Exit:
;	If out-of-memory, returns ax=0 and psw.z set
;	NOTE: Out-of-memory is only possible if opStData or opStRestore1
;	      is being inserted
;
;**************************************************************
cProc	TxtInsUpdate,<PUBLIC,NEAR,NODATA>,<di>
	localW	otxEndInsert
	localW	oRsSave
cBegin
	DbChk	Otx,si			;error if > txdCur.bdlText.cbLogical
	DbChk	Otx,bx			;error if > txdCur.bdlText.cbLogical
	mov	[otxEndInsert],bx

	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	sub	ax,CB_EMPTY_TEXT-StartOtx 
	cmp	bx,ax
	jae	TiDontUpd		;brif inserting before end-of-text
					; Could be true even if we're loading,
					; in case of FILE/MERGE, or $INCLUDE.

;     *--------------------------------------------------------------
;     * pass information about the edit to UpdatePrs in static struct uprs.UPRS_
;     * UpdatePrs will update the prs.otx field for all prs entries affected
;     * by this text insert.
;     * 
;     *--------------------------------------------------------------
;
	mov	ax,[grs.GRS_oRsCur]
	mov	[uprs.UPRS_oRsEdit],ax
	mov	[uprs.UPRS_otxEdit],si
	sub	ax,ax			;cbDel = 0
	mov	[uprs.UPRS_cbDel],ax
	sub	bx,si			;bx = cbIns
	mov	[uprs.UPRS_cbIns],bx	;pass cbIns

	;Update program counter and any other runtime text pointers
	push	si			;pass otxInsert
	push	bx			;pass cbIns
	push	ax			;pass cbDel (0)
	push	ax			;fTestOnly = FALSE
	call	UpdatePcs		;update program counter and any
					; stacked return addresses

	mov	bx,CPOFFSET UpdatePrs
	call	ForEachPrsInPlaceCPSav	;preserve callers oRs

; If we are in a text paste, then the inserted pcode can only
;  contain opBols and opReparses.  TxtPaste updates the line
;  count, so no further processing is needed.
;
TiDontUpd:
	test	[flagsTm],FTM_TxtPaste	;are we in a paste operation
	jne	TiExitTrue		;if so, exit

	push	si
	PUSHI	ax,<CODEOFFSET tOpAftIns>
	call	TxtFindOp		;ax = offset to 1st inserted opcode
					;dl = [txtFindIndex]
AiLoop:
	xchg	di,ax			;di = otxCur
	cmp	di,[otxEndInsert]
	jae	AiDone			;brif beyond inserted pcode
DbAssertRelB dl,b,AFTINS_opEot,CP,<TxtInsUpdate - found EOT before expected>
	cmp	dl,AFTINS_bolInclMax
	ja	AiChkBol		;brif not opBolInclude opcode
	inc	[txdCur.TXD_cLinesIncl] ;bump include line count
AiChkBol:
	cmp	dl,AFTINS_bolMax
	ja	AiDisp			;brif not opBol opcode
	inc	[txdCur.TXD_cLines]	;bump line count
AiDisp:
	sub	dl,AFTINS_dispMin	;adjust index for non-dispatched opcodes
	jc	AiNext			;brif no more work for this opcode
	sub	dh,dh			;dx = dispatch index
	shl	dx,1			;dx = dispatch offset
	mov	bx,dx			;bx = dispatch offset
	jmp	WORD PTR cs:AiDispatch[bx]
;All the AiOpxxx dispatches either branch to an error handler, or AiNext
AiNext:
	push	di			;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpAftIns>
	call	TxtFindNextOp
	jmp	SHORT AiLoop

;     *--------------------------------------------------------------
;     * If we're not loading, update all linked lists which traverse
;     * the pcode that were affected by the edit.
;     * If we did this for every line during ASCII Load, load time
;     * would increase about 1000 percent.  For ASCII Load, we do
;     * it at the end of every procedure's text table, and finally
;     * at end of file.
;     * 
;     * This also applies to Big Edits.  To speed up block cut/copy/paste
;     * we will only update links at the end of a Big Edit.
;     *--------------------------------------------------------------
;
AiDone:
	FLoadActive
	jne	TiExitTrue		;brif LOADing a file
					; (speed optimization)
	cmp	[bigEditState],BIG_EDIT_ACTIVE
	je	TiExitTrue		;brif in a BigEdit

	push	si			;pass otxInsert
	push	[otxEndInsert]		;pass offset beyond inserted text
	call	UpdateLinks		;update linked lists through pcode
	DbChk	TxdOps			;check for bad linked lists through
					; pcode, bad opcodes, etc.
TiExitTrue:
	mov	ax,sp			;return non-zero (success)
TiExit:
	or	ax,ax			;set condition codes for caller
cEnd	;TxtInsUpdate

AiDispatch:
	DW	AiOpBolLab
	DW	AiOpBolLabSp
	DW	AiOpLab
	DW	AiOpLabSp
	DW	AiOpStSub
	DW	AiOpStFunction
	DW	AiOpStDeclare
	DW	AiOpStCall
	DW	AiOpStCallS
	DW	AiOpStCallLess
	DW	AiOpStEndProc
	DW	AiOpEndSingleDef
	DW	AiOpStEndDef
	DW	AiOpStData
	DW	AiOpStRestore1


;-------------------------------------------------------------------
; Opcode specific code which gets executed AFTER pcode is inserted
; in text table.  Aixxx stands for After Insert <opcode name>
;
; di = offset into text table to opcode
;
;-------------------------------------------------------------------

;set bit that says the label by this name is defined
AiOpLab:
AiOpLabSp:
AiOpBolLab:
AiOpBolLabSp:
	lea	ax,[di+4]		;pass ptr to oNam field
	call	GetWOtx			;ax = label's oNam
	push	ax			;pass it
	PUSHI	ax,NM_fLineNumLabel
	call	SetONamMask		;set bit used for duplicate label check
	jmp	SHORT J1_AiNext

;See if this reference to oPrs is the "strongest" seen so far.
;If so, make it the owner of the prs entry, that is, make the
;prs entry refer back to this text offset.
;
AiOpStSub:
AiOpStFunction:
AiOpStDeclare:
AiOpStCall:
AiOpStCallS:
AiOpStCallLess:
	lea	ax,[di+4]		;ax = offset to oPrs field
definePrs:
	call	GetWOtx			;ax = referenced oPrs
	push	ax			;pass to PrsActivateCP

	mov	ax,[grs.GRS_oRsCur]	;ax = rs containing new reference
	mov	[oRsSave],ax
	mov	[dprs.DPRS_oRs],ax	;fill dprs structure for SetPrsDefn
	mov	ax,[grs.GRS_oMrsCur]
	mov	[dprs.DPRS_oMrs],ax
	mov	[dprs.DPRS_otx],di
	test	[mrsCur.MRS_flags2],FM2_INCLUDE ;is this an INCLUDE mrs?
	jne	SetRef			;force it to a "weak" owner

	mov	al,[txtFindIndex]
	mov	dl,FP_DEFINED
	cmp	al,AFTINS_opStDeclare	;compare with txtFindIndex
	jb	SetFlags		;brif not DECLARE or CALL reference
	mov	dl,FP_DECLARED
	je	SetFlags		;brif DECLARE reference
SetRef:
	sub	dl,dl			;else it must be a CALL reference
SetFlags:
	mov	[dprs.DPRS_flags],dl

					;parm was pushed several lines above
	call	PrsActivateCP		;activate referenced prs

	call	SetPrsDefn		;set its defn to dprs.xxx if strongest

	push	[oRsSave]
	call	RsActivateCP		;reactivate txt tbl containing the ref
J1_AiNext:
	jmp	AiNext

;If we're ASCII loading, DATA statements need to be moved from procedure
;level to module level, and RESTORE statements need to be adjusted to
;point to the module level label.  Call TDataEntry to mark where this
;DATA/RESTORE statement is.  The actual movement will be done by TDataEnd
;when the ASCII load completes.
;
AiOpStData:
	mov	al,DT_Data
	SKIP2_PSW			;skip next 2 byte instr (mov al,const)
AiOpStRestore1:
	mov	al,DT_Restore
	mov	bx,di			;bx = otxCur
	call	TDataEntry		;add entry to table, so we know
					; what needs to be moved at end-of-load
	jne	J1_AiNext		;brif no error
	jmp	TiExit			;return FALSE (out of memory)


AiOpEndSingleDef:
AiOpStEndDef:
AiOpStEndProc:
	;we got an END SUB/FUNCTION.
	;Remember we've seen an end-proc for this procedure
	
	or	[prsCur.PRS_flags],FP_ENDPROC
	jmp	SHORT J1_AiNext	
	


;**************************************************************
; ushort TxtBindPrs
; Purpose:
;	Called by TxtBindPrsS for each text prs.  See comments
;	in TxtBindPrsS.
;
;**************************************************************
TxtBindPrs PROC NEAR
	push	si			;save caller's si,di
	push	di
	DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<TxtBindPrs err1>
	mov	ax,[grs.GRS_oRsCur]	;Setup for call to SetPrsDefn
	mov	[dprs.DPRS_oRs],ax
	mov	ax,[grs.GRS_oMrsCur]
	mov	[dprs.DPRS_oMrs],ax

	sub	ax,ax
	push	ax
	PUSHI	ax,<CODEOFFSET tOpPrsDef>
	call	TxtFindOp		;ax=off to 1st opcode with oPrs operand
					;dl = [txtFindIndex]
TbLoop:
	mov	[dprs.DPRS_otx],ax	;pass otx to SetPrsDefn
	xchg	si,ax			;si = otxCur
	cmp	dl,PRSDEF_opEot
	jne	TbNotDone
	jmp	TbExit			;brif done with loop

TbNotDone:
	lea	si,[si+4]		;si = otxPrs
	cmp	dl,PRSDEF_opStDefFn
	jne	TbNotDefFn1		;brif not a DEF FN
	inc	si
	inc	si			;skip DEF FN's link field
; assert that scan state is SS_RUDE or SS_SUBRUDE
DbAssertRelb [txdCur.TXD_scanState],ae,SS_RUDE,CP,<TxtBindPrs: invalid scan state>
	jmp	TbNext			;leave DEF FN's operand as an oNam
					; when scanState is SS_RUDE
TbNotDefFn1:
	mov	ax,si
	call	GetWOtx			;ax = oNam or oPrs
	.errnz DCL_oPrs	- 2
	xchg	di,ax			;di = oNam or oPrs
	cmp	[fBindStatic],TBIND_Save1
	jne	TbBindNamToPrs
	jmp	TbBindPrsToNam		;brif binding oPrs to oNam


;convert oNam to oPrs for Binary Load
TbBindNamToPrs:
	mov	ax,si			;ax = otxPrs
	inc	ax
	inc	ax
	call	GetWOtx			;ax = procAtr field
	.errnz	DCL_atr - 4
	and	al,DCLA_oTyp		;al = proc's oTyp
	mov	dx,ax
	sub	cx,cx			;cx = FALSE
	mov	al,[txtFindIndex]	;al = opcode type
	cmp	al,PRSDEF_DefineMax
	DJMP	ja TbCall		; brif not DECLARE/SUB/FUNC/DEFFN

;Got a SUB/FUNCTION/DEFFN or DECLARE
	.errnz	PRSDEF_opStDeclare
	or	al,al
	jne	TbNotDeclare		;brif not DECLARE stmt

;Got a DECLARE stmt, dx = procAtr field
;
	mov	al,dh
	and	al,03h			;al = procType (PT_SUB, etc.)
	.errnz	DCLA_procType - 0300h
	cbw				;ax = prsType
	sub	dh,dh			;dx = oTyp
	push	di			;pass oNam
	push	ax			;pass proc type
	push	dx			;pass oTyp
	mov	al,1			;(can't push sp because PrsDefine
					; expects a byte parm)
	push	ax			;pass TRUE (for fDeclare)
	call	PrsDefine		;ax = error code
	or	ax,ax			
	jne	JNE1_TbErr		;brif error
	mov	al,FP_DECLARED		;al = flags for SUB/FUNCTION/DEF FN
	jmp	SHORT TbGotPrs

;Got a SUB/FUNCTION/DEF FN stmt
;al = txtFindIndex = proc type (PT_SUB etc.)
;dx = oTyp
;
TbNotDeclare:
	cmp	al,PRSDEF_opStDefFn
	jne	TbNotDefFn		;brif not DEF FN statement
					;SUBs and FUNCTIONs already have
					;a prs, no need to create one
	.errnz	ET_IMP
	or	dl,dl			;test dl for ET_IMP
	jne	TbNotDefType		;brif explicitly typed
	mov	ax,si			;pass text offset in ax
	call	OtxDefTypeCur		;fill tEtCur with default types for
					;this text offset

	cCall	OTypOfONamDefault,<di>	; ax = default oTyp
	xchg	dx,ax			;dx = default type
TbNotDefType:
	push	di			;pass oNam
	PUSHI	ax,PT_DEFFN		;pass procType (PT_DEFFN)
	DbAssertRelB dh,e,0,CP,<TxtBindPrs dh!=0>
	push	dx			;pass oTyp

	;pass fDeclare as FALSE if TBIND_Load, TRUE if TBIND_Save2
	;If we passed FALSE for TBIND_Save2, we'd get duplicate definition err
	;since prs is already declared.  In TBIND_Save2, we don't want to
	;create a prs entry, we just want to map an oNam to oPrs.
	
	sub	ax,ax
	mov	al,[fBindStatic]
	.errnz	TBIND_Load
	push	ax			;pass fDeclare
	call	PrsDefine
	or	ax,ax			
JNE1_TbErr:
	jne	TbErr			;brif error
TbNotDefFn:
	mov	al,FP_DEFINED		;al = flags for SUB/FUNCTION/DEF FN
TbGotPrs:
	mov	di,[grs.GRS_oPrsCur]	;di = oPrs
	DbAssertRel di,ne,UNDEFINED,CP,<TxtBindPrs err2>
	jmp	SHORT TbSetDefn

TbCall:
	;Got some kind of CALL statement
	push	di			;pass oNam
	PUSHI	ax,PT_SUB
	PUSHI	ax,UNDEFINED
	call	PrsRef			;ax = oPrs (or error code if MSB set)
	js	TbErr			;brif error (like out-of-memory)
	mov	di,ax			;save di = oPrs
	push	ax			;pass oPrs
	call	PrsActivateCP
	sub	al,al			;SetPrsDefn flags = 0 (CALL)
TbSetDefn:
	mov	[dprs.DPRS_flags],al
	call	SetPrsDefn		;definition is in static struct dprs
	push	[dprs.DPRS_oRs]
	call	RsActivateCP
	jmp	SHORT TbPut

;convert oPrs to oNam for Binary Save
TbBindPrsToNam:
	push	di			;pass oPrs
	call	FieldsOfPrs		;ax = prs's oNam.
	mov	di,ax			;di = oNam
TbPut:
	push	si			;pass otxPrs
	push	di			;pass oNam/oPrs
	call	PutWOtx
TbNext:
	push	[dprs.DPRS_otx]		;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpPrsDef>
	call	TxtFindNextOp
	jmp	TbLoop

;Error is not necessarily fatal, for example, during Binary Save
;TxtBindPrsS(0) is called, then TxtBindPrsS(TRUE) is called.  In this
;case, TxtBindPrsS(TRUE) will always report duplicate definition errors.
;The caller (binary Save) ignores errors - It is only meaningful
;during binary load.
TbErr:
	and	ah,7FH			;turn off MSB (set by PrsDefine)
	mov	[bindErrCode],ax
	jmp	SHORT TbNext

TbExit:
	mov	[txdcur.TXD_ScanState],SS_RUDE ;if binding change scanstate
					;from SUBRUDE to RUDE otw it will
					;already be in rude & this is a NOP
	mov	ax,sp			;return TRUE (non-zero)
	pop	di			;save caller's si,di
	pop	si
	ret
TxtBindPrs ENDP

;**************************************************************
; ushort TxtBindPrsS(ax:fBind)
; Purpose:
;	Called during Binary Load with fBind==TRUE to go through all
;	text tables just loaded, and for each opcode which refers to
;	a prs (opStSub/Function/DefFn/Declare/Call/CallS) convert
;	the prs id from oNam to oPrs.
;
;	Called during Binary Save with fBind==FALSE to go through all
;	text tables about to be saved, and for each opcode which refers
;	to a prs (opStSub/Function/DefFn/Declare/Call/CallS) convert
;	the prs id from oPrs to oNam.
;	This is because, when the module is re-loaded, offsets into the
;	procedure table may not be the same.
;
; Entry:
;	al = TBIND_Load:  map pcode oNam->oPrs, call PrsDefine
;	     TBIND_Save1: map pcode oPrs->oNam - called before writing to file
;	     TBIND_Save2: map pcode oNam->oPrs - called after writing to file
;	grs.fDirect must be FALSE
;	grs.oMrsCur identifies module just loaded.
;	All text tables in this mrs are assumed to be in SS_RUDE or SS_PARSE
;
; Exit:
;	returns 0 if no error, else a standard Basic error code.
;	condition codes are set based on value in ax
;	NOTE: If called with ax = zero, no error is possible, and no
;	heap movement will occur - Binary Save depends on this.
;
;**************************************************************
PUBLIC	TxtBindPrsS
TxtBindPrsS PROC NEAR
	mov	[fBindStatic],al
	mov	[bindErrCode],0

	DbAssertRelB [grs.GRS_fDirect],e,FALSE,CP,<TxtBindPrsS:fDirect TRUE>
	;convert the module's text table and each procedure's text table
	mov	bx,CPOFFSET TxtBindPrs
	call	ForEachTxtTblInMrs
	mov	ax,[bindErrCode]	;return error code to caller
	or	ax,ax			;set condition codes for caller
	ret
TxtBindPrsS ENDP


;----------------------------------------------------------------------
;			BigEdit Related Functions
;
; These EditMgr operations translate into several Text Manager calls:
;   Split Line:
;	ReplaceLineBuf -> TxtChange(1st half of line, delete split line)
;	InsertLineBuf -> TxtChange(2nd half of line, no delete)
;
;   Join Lines:
;	DeleteLinesInBuf -> TxtChange(delete 2 lines, no insert)
;	InsertLineBuf -> TxtChange(joined lines, no delete)
;
;   Block Paste:
;	DeleteLinesInBuf(selected lines)
;	InsertLineBuf -> TxtChange(1st line, no delete)
;	   :
;	InsertLineBuf -> TxtChange(nth line, no delete)
;
; The text manager takes advantage of the fact that no big edit does more
; than 1 delete of any of the user's original pcode.  It would need more
; than 1 bdTxtScrap if this were not the case.
;
; This presents a problem for Edit & Continue, since the nth text manager
; call may be the 1st operation that would prevent continuing.
; When we know we're in a BigEdit (by examining 'bigEditState'),
; all calls to TxtChange are done as a ReParse.  When the BigEdit is done,
; (i.e. when TxtEndBigEdit is called), we ReParse all lines entered
; during the BigEdit, and if any would prevent continuing, and the user
; indicates he wants to back out of the edit:
;   The inserted text (otxBigIns, cbBigIns) is deleted (via TxtDelete)
;
; As a speed optimization for block paste, we only update the txt links
; at the end of a big edit.  This significantly reduces the over head for
; large edit operations.
;   
; The following shows the BigEdit related actions performed by various routines:
;
; bigEditState is initialized to BIG_EDIT_FALSE
;
; TxtStartBigEdit:
;    bigEditState = BIG_EDIT_ACTIVE
;    fFillScrap = TRUE
;    cbBigIns = 0
;    otxBigIns = UNDEFINED
;
; TxtDelete(otxDelFirst, otxDelLast)
;    If bigEditState == BIG_EDIT_CANCEL, return without changing anything
;    If delete would prevent continuing, & user wants to back out of edit,
;       if bigEditState == BIG_EDIT_ACTIVE, then bigEditState = BIG_EDIT_CANCEL
;       return without changing anything
;    If fFillScrap and bdTxtScrap is empty,
;       fFillScrap = FALSE
;	copy deleted text to bdTxtScrap
;       otxScrap = otxDelFirst
;    If cbBigIns > 0, cbBigIns -= cbDel
;
; TxtChange(otxDelFirst, otxDelLast, fNoInsert)
;    If bigEditState == BIG_EDIT_CANCEL, return without changing anything
;    fFillScrap = TRUE
;    If otxDelFirst != otxDelLast
;       if TxtDelete(otxDelFirst, otxDelLast) == FALSE
;          return without changing any pcode (user wants to back out of edit)
;    If fNoInsert return
;    fFillScrap = FALSE
;    If bigEditState == BIG_EDIT_ACTIVE
;       insert line as an opReParse
;	cbBigIns += cbInsert
;       if otxDelFirst < otxBigIns
;	   otxBigIns = otxDelFirst
;    else
;	Parse source line to pcode
;       If insert would prevent continuing, and user wants to back out of edit,
;          If cbBigIns > 0, TxtDelete(otxBigIns, otxBigIns + cbBigIns)
;	   If bdTxtScrap is not empty, Insert bdTxtScrap at otxScrap
;          bigEditState = BIG_EDIT_CANCEL
;
; TxtEndBigEdit:
;    bigEditState = BIG_EDIT_REPARSE
;    otxCur = otxBigIns
;    While bigEditState != BIG_EDIT_CANCEL && otxCur < otxBigIns + cbBigIns
;       call TxtChange to reparse line at otxCur
;       otxCur = next opReParse(otxCur)
;    release bdTxtScrap
;    call update links to rethread the pcode linked lists.
;
;-------------------------------------------------------

;**************************************************************
; TxtStartBigEdit()
; Purpose:
;	Called by the Edit Mgr at the start of some operation which results
;	multiple calls to InsertLineInBuf/ReplaceLineBuf/DeleteLinesBuf.
;	Examples include multi-line-paste/cut/copy, line-split, line-join.
;
;**************************************************************
cProc	TxtStartBigEdit,<PUBLIC,FAR>	
cBegin	TxtStartBigEdit 		
	DbAssertRelB [bigEditState],e,BIG_EDIT_FALSE,CP,<TxtStartBigEdit:Already in big edit>
	mov	[bigEditState],BIG_EDIT_ACTIVE
	and	[flagsTm],NOT FTM_BpDeleted ;clear BP deleted flag
	mov	[cForDel],1
	mov	[cForStepDel],1
	sub	ax,ax			;ax = 0
	mov	[cbBigIns],ax		;cbBigIns = 0
	dec	ax			;ax = FFFF
	mov	[otxBigIns],ax		;otxBigIns = UNDEFINED
	mov	[fFillScrap],al		;fFillScrap = TRUE
cEnd	TxtStartBigEdit 		

;**************************************************************
; bool TxtEndBigEdit()
; Purpose:
;	Called by the Edit Mgr at the end of some operation which results
;	multiple calls to InsertLineInBuf/ReplaceLineBuf/DeleteLinesBuf.
; Exit:
;	same as TxtChange
;
;**************************************************************
cProc	TxtEndBigEdit,<PUBLIC,FAR>,<si,di> 
cBegin	TxtEndBigEdit			
DbAssertRelB [bigEditState],ne,BIG_EDIT_FALSE,CP,<TxtEndBigEdit:Not in big edit>
	sub	ax,ax			;prepare to return 0
	mov	si,[otxBigIns]		;si = otxStart of big edit
	inc	si			;did any edit occur?
	jz	TeDone			;brif not
	dec	si
	mov	di,si
	add	di,[cbBigIns]		;di = otxEnd of big edit
	cmp	[bigEditState],BIG_EDIT_ACTIVE ;did we successfully complete
	jne	TeLoop			;the big edit - brif backed out
	push	si
	push	di
	call	UpdateLinks		;update txt threads before processing
					;op reparse list.
	DbChk	TxdOps			;check for bad linked lists through
					; pcode, bad opcodes, etc.
	mov	cx,[grs.GRS_oRsCur]	;cx = oRsCur on entry
TeLoop:
	sub	ax,ax			;prepare to return 0
	test	[mrsCur.MRS_flags2],FM2_NoPcode
	jne	TeDone			;if document or command window, done
	cmp	[bigEditState],BIG_EDIT_CANCEL
	je	TeDone			;brif user backed out of BigEdit
	cmp	cx,[grs.GRS_oRsCur]	;did we change from one SUB/FUNCION
					; into a different SUB/FUNCTION?
	jne	TeDone			;brif so, leave the rest of edit as
					; opReparses.
	mov	[bigEditState],BIG_EDIT_REPARSE
	mov	ax,si			;pass otxStart to DoReParse
	mov	bx,di			;bx = otx to stop looking for opReParse
	sub	di,[txdCur.TXD_bdlText_cbLogical]
					;prepare for add di,_cbLogical below
	push	cx			;preserve entry oRs
	call	DoReParse		;parse next opReParse in text table
					;ax = non-zero if serious error
	pop	cx			;recover entry oRs
	jc	TeDone			;brif this edit caused an error
	add	di,[txdCur.TXD_bdlText_cbLogical]
					;adjust terminating otx for delta this
					; edit caused.
	jmp	SHORT TeLoop		;brif 1 line was successfully reparsed

;ax = error code (0 if none)
TeDone:
	cmp	[bdlTxtScrap.BDL_status],NOT_OWNER
	je	TeDontFree
	push	ax			;save return value from DoReParse
	PUSHI	ax,<dataOFFSET bdlTxtScrap>
	call	BdlFree			;release DELETE scrap
	pop	ax			;ax = DoReParse (and TxtChange's)
					; return value
TeDontFree:
	mov	[bigEditState],BIG_EDIT_FALSE
	cmp	ax,ER_OM
	je	TeExit
	cmp	ax, MSG_GoDirect	; Also, allow this error
	je	TeExit			
	sub	ax,ax			;all errors except Out-of-memory
					; are ignored.  User may be joining
					; or spliting two lines, which will
					; temporarily result in a syntax error,
					; which the user fully intends to fix.
					; Giving the error when the user
					; moves off the line is soon enough.
					; Anything else is very annoying.
TeExit:
cEnd	TxtEndBigEdit			

;**************************************************************
; ushort FAR TxtPaste(otxInsert, oRsSrc)
;
; Purpose:
;	The editor calls TxtPaste() to insert a block of
;	opReparses into a text table.  This is called in
;	association with BigEdits, after TxtStartBigEdit,
;	and prior to TxtEndBigEdit.  This speeds up block
;	pastes since the opReparses that are in the scrap
;	do not have to be listed to ascii, and multiple
;	TxtChange calls are avoided.
;	TxtDescan() should be called before this, to ensure that
;	the text table is descanned to SS_PARSE state.
;
; Entry:
;	grs.oMrsCur, grs.oPrsCur have their usual meaning
;	parm1: ushort otxInsert - text table offset to opBol
;	   opcode for where lines should be inserted.
;	parm2: ushort oMrsSrc - Describes the MRS and text table
;	   containing the block to be inserted.
;
; Exit:
;	ax = 0 if out of memory
;
;**************************************************************
; This version of TxtPaste will not work in EB since it pushes pointers
; to text tables onto the stack and calls RsActivate.
cProc	TxtPaste,<PUBLIC,FAR>,<si,di>
parmW	otxInsert
parmW	oRsSrc
localW	cbIns
cBegin


	mov	[fFillScrap],0		;just paste, nothing to put in Scrap
	push	[grs.GRS_oRsCur]	;save destination oRs
	push	[oRsSrc]
	call	RsActivateCP		;activate Src txt table
	mov	ax,[txdCur.TXD_bdlText_cbLogical] ;get size of text to copy
	sub	ax,CB_EMPTY_TEXT-StartOtx ;don't include opEndProg and opEot
	mov	[cbIns],ax		;save amount to move.  We can't assign
					; to cbBigIns until after we have called
					; TxtMoveUp in case of OM.
	mov	di,[txdCur.TXD_cLines]	;grap number of lines in source buf

	call	RsActivateCP		;reactivate oRsDst (already on stack)

; Make room for reparse block
	mov	ax,[otxInsert]		;get insertion point
	mov	[otxBigIns],ax		;remember for TxtEndBigEdit
	push	ax			;pass insertion point
	push	[cbIns] 		;pass amount of space to free
	call	TxtMoveUp
	je	TxtPasteExit		;brif out-of-memory error
	mov	ax,[cbIns]
	mov	[cbBigIns],ax		;save amount moved for TxtEndBigEdit
	add	[txdCur.TXD_cLines],di	;adjust dest for inserted lines

; Copy bytes to txdcur from source txt table
	DbSegMoveOff			;assert no far calls
	push	[grs.GRS_oRsCur]	;save destination oRs for reactivation
					; after move
	push	ds			;save dgroup
	GetSegTxtCur			;es = seg adr of current txt tbl
	push	es			;save dest seg

	push	[oRsSrc]
	call	RsActivateCP		;activate Src txt table
	GetSegTxtCur			;es = seg adr of current txt tbl
	mov	cx,[cbIns]
	SetStartOtx si			;start of src txt table
	mov	di,[otxInsert]		;insert at otxInsert in cur text table
	push	es
	pop	ds			;ds = txt seg of src
	pop	es			;es = txt seg of dst
	shr	cx,1			;move words
	rep	movsw			;insert opReparses
	pop	ds			;recover ds = dgroup
	DbSegMoveOn			;Far calls are OK again

DbAssertFlags nc,CP,<TxtPaste: err2>

	call	RsActivateCP		;reactivate oRsDst (already on stack)

	mov	si,[otxInsert]
	mov	bx,si			;si = otxInsert
	add	bx,[cbIns]		;bx = otxAfterInsert
	or	[flagsTm],FTM_TxtPaste	;we are pasting, so don't update line
					; count.
	call	TxtInsUpdate		;update line count, IP, etc.
	and	[flagsTm],NOT FTM_TxtPaste ;clear paste flag

; OpReparse links will be updated by call to TxtEndBigEdit from
;   otxBigIns to otxBigIns+cbBigIns
;
	DbChk	TxdOps			;check for bad linked lists through
					; pcode, bad opcodes, etc.
	mov	ax,sp			;return True
TxtPasteExit:
cEnd


sEnd	CP

end
