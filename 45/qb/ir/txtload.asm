	TITLE	txtload.asm - ASCII Load Functions

;==========================================================================
;
;Module:  txtload.asm - ASCII Load Functions
;System:  Quick BASIC Interpreter
;
;=========================================================================*/

	include		version.inc
	TXTLOAD_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	conint		
	includeOnce	fdb
	includeOnce	heap
	includeOnce	names		
	includeOnce	opid
	includeOnce	opmin
	includeOnce	opstmt
	includeOnce	opaftqb4
	includeOnce	parser
	includeOnce	pcode
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	rtps
	includeOnce	rttemp
	includeOnce	sb
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	txtint
	includeOnce	ui
	includeOnce	util
	includeOnce	stack2		

	includeOnce	edit		


	ASC_TAB		EQU 09h
	ASC_LF		EQU 0Ah
	ASC_CR		EQU 0Dh
	ASC_EOF 	EQU 1AH
	ASC_DBL_QUOTE	EQU 22h
	ASC_QUOTE	EQU 27h

	assumes	DS,DATA
	assumes	SS,DATA
	assumes ES,NOTHING


	externFP	B$ISTALCTMPSUB	
	externFP	B$ISTDALCTMP	


sBegin	DATA
	extrn	b$PTRFIL:WORD	;Read only for QBI - channel Ptr

	extrn	szUntitled:BYTE 	


fInQuote DB 0
PUBLIC	fMergeInSub
fMergeInSub DB 0		;non-zero if MERGING a file into a SUB
				; in this case, LoadEnterProc & LoadExitProc
				; will never be called.

;temp buffers for filename before and after Normalization [7]
PUBLIC NormFname
NormFname 	DB	FILNAML DUP (?)	;buffer for temp storage of filename 

	EVEN				; SD must be word-aligned!
PUBLIC sdNormFname
sdNormFname	SD	<0,dataOFFSET NormFname> ;string descriptor 

	externB	b$BAS_EXT		;".bas" constant
	externW	b$PN_NAME		

	globalB	fLoadInclude,0		; Nonzero when loading include file

	externB fInitialized		; non-zero if we've completed
					; initialization of QB

sEnd	DATA

	EXTRN	B$FREF:FAR
	EXTRN	B$OPEN:FAR
	EXTRN	B$IDISK_SINP:FAR
	EXTRN	B$CLOS:FAR
	EXTRN	B$SEEKSTART:FAR
	EXTRN	B$IDISK_BAKC:FAR
	EXTRN	B$IValidatePath:FAR	
	EXTRN	B$CHAN:FAR


sBegin	CODE

;Table of opcodes which make up '$STATIC, '$DYNAMIC and DEFINT
;and INCLUDE statements.
;The order of entries in this table is VERY important
;
tOpProcHdr LABEL WORD
	opTabStart PROCHDR
	opTabEntry PROCHDR,opStDefType
	opTabEntry PROCHDR,op_Dynamic
	opTabEntry PROCHDR,op_Static
	opTabEntry PROCHDR,opStRem
	opTabEntry PROCHDR,opQuoteRem
	opTabEntry PROCHDR,opBol
		PROCHDR_opBolMin equ PROCHDR_opBol
	opTabEntry PROCHDR,opBolSp
	opTabEntry PROCHDR,opBolInclude
	opTabEntry PROCHDR,opBolIncludeSp
	opTabEntry PROCHDR,opBolLab		
	opTabEntry PROCHDR,opBolLabSp		
	opTabEntry PROCHDR,opEndProg
		;must be 2nd to last entry in table
	opTabEntry PROCHDR,opEot
		;must be last entry in table


sEnd	CODE

sBegin	DATA

;-------------------------------------------------------------------
; During ASCII Load/Merge, we don't update threaded linked lists
; through the pcode for each line, because it is very slow.
; Much faster to update them all at once when we're done.
; This must be done at the following points:
;
;	  1 - when we enter a procedure, since we are moving into
;	      a new text table.
;	  2 - when we exit a procedure, since we are returning to
;	      the module's text table.
;	  3 - When the Load reaches end-of-file
;
; otxUpdLinks represents an offset into the current text table to the
; 1st byte of pcode which UpdateLinks() has not been called for yet.
;
;-------------------------------------------------------------------

	EXTRN	tabStops:WORD	;defined in edit manager

PUBLIC	otxUpdLinks
otxUpdLinks	DW 0

;fDynArrays is initialized to FALSE by AsciiMerge, set TRUE when $DYNAMIC is
;seen by TxtChange, set FALSE when $STATIC is seen by TxtChange
;
PUBLIC	fDynArrays
fDynArrays	DB 0

;fDynArraysMod is set to the value of fDynArrays when we enter a procedure
; (LoadEnterProc), so we can restore the $DYNAMIC/$STATIC state when we
; return back to module level (LoadExitProc).
;
fDynArraysMod	DB 0

;fProcDyn is TRUE if a $DYNAMIC deleted later than a $STATIC,
;i.e. the state at the end of the deleted range was $DYNAMIC
;and not $STATIC.
;
PUBLIC	fProcDyn
fProcDyn	DB 0

;otxLastProc = text offset in module's text table where we were
;when we entered the most recent SUB/FUNCTION.  One way to think
;of it is, when were in a procedure, its the module's text offset
;where the 1st line after the END SUB/FUNCTION will be inserted
;into the module's text table.
;
otxLastProc	DW 0

;otxNewInsert is initialized to UNDEFINED by AsciiMerge,
;and set by LoadEnterProc and LoadExitProc to tell AsciiMerge
;that we switched text tables
;
otxNewInsert	DW 0

;cbAftMerge is initialized to 0 by LoadFile,
;if we're doing anything except a MERGE.  If we're doing a MERGE,
;it marks the insertion point in the main module.
;
cbAftMerge DW 0

;otxDefStart and otxDefEnd represent the range of text in the module's
;text table for DEFtype statements synthetically emitted by LoadExitProc.
;If, when the Load completes, this range is at the end of the module,
;it is discarded.  If we didn't do this, the module would grow with
;every subsequent LOAD/SAVE
;
otxDefStart	DW 0
otxDefEnd	DW 0

;used to keep procedure DEFtype stmts independent of module's DEFtype statements
;
PUBLIC	tEtTemp
tEtTemp		DB 26 DUP(0)

PUBLIC	chanCur
chanCur		DW 0	;current channel # used by Load/Save
PUBLIC	cChansOpen	;only referenced by FLoadActive macro
cChansOpen	DB 0	;number of recursive calls to LoadFile active, bumped
			; every time file is opened, decremented when closed.
cAsciiLoadsActive DB 0	;number of recursive calls to LoadFile active for
			; ASCII files

sEnd	DATA


		;---------------------------------------
		; SUB/FUNCTION Window Support Functions 
		;---------------------------------------

externFP FindFile		

sBegin	CP
assumes	cs,CP

;*************************************************************
; boolean NEAR InsertEtDiff(ax:otxInsert, ptEtBase, ptEtNew)
; Purpose:
;	Emit opBol and opStDefType statements necessary to move from
;	one set of default-types to another.
;	The new pcode is emitted at otxInsert in the current module.
;
; NOTE: This routine doesn't update the line count for any inserted BOLs.
;	The caller is responsible for updating the line count if it is
;	needed.
;
; Entry:
;	ax:otxInsert = offset into current module's text table where
;	   synthetic pcode is to be emitted.
;	ptEtBase and ptEtNew point to an arrays of 26 bytes, one ET_xx for each
;	   letter from A to Z.
;
; Exit:
;	If ptEtBase is identical to ptEtNew, no pcode is emitted.
;	else the pcode emitted is the most efficient way to get from
;	   ptEtBase to ptEtNew.
;	If out-of-memory, returns ax = 0 (and psw.z set)
;	else returns ax non-zero
;
;*************************************************************
DEFLN	STRUC
DEFLN_bolOpcode		DW	0
DEFLN_defOpcode		DW	0
DEFLN_defLinkField	DW	0
DEFLN_defOperandLO	DW	0
DEFLN_defOperandHI	DW	0
DEFLN	ENDS

cProc	InsertEtDiff,<PUBLIC,NEAR>,<si,di>
	parmW	ptEtBase
	parmW	ptEtNew
	localV	deflnNew,<SIZE DEFLN>
cBegin
	xchg	di,ax			;di = insertion point (otxInsert)
	mov	[deflnNew.DEFLN_bolOpcode],opBol
	mov	[deflnNew.DEFLN_defOpcode],opStDefType
	mov	si,CBASETYPEMAX		;init etCur
InsEtLoop:
	dec	si			;decrement etCur
	js	InsEtEnd		;brif we've done all primitive types

	push	[ptEtBase]
	push	[ptEtNew]
	push	si			;pass etCur
	call	EtDiff			;[dx:ax] = bit mask representing
					; difference between base and new type
					; with respect to si (etCur)
	mov	[deflnNew.DEFLN_defOperandLO],ax
	mov	[deflnNew.DEFLN_defOperandHI],dx
	or	ax,dx
	je	InsEtLoop		;brif no difference between base & new

	push	di			;pass otxInsert
	PUSHI	ax,<SIZE DEFLN>
	call	TxtMoveUp		;make room for pcode to be inserted
	je	InsEtExit		;return FALSE if out-of-memory

	;insert new pcode in current text table
	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
	push	di			;pass otxInsert
	lea	ax,[deflnNew]
	push	ax
	PUSHI	ax,<SIZE DEFLN>
	call	BdlCopyTo
	jmp	SHORT InsEtLoop

InsEtEnd:
	mov	ax,sp			;return TRUE (non-zero)
InsEtExit:
	or	ax,ax			;set condition codes for caller
cEnd

;*************************************************************
; boolean InsertDynDiff(ax:otxInsert, dh:fDynBase, dl:fDynNew)
; Purpose:
;	Emit pcode for REM $STATIC or REM $DYNAMIC if fDynBase <> fDynNew.
;	The new pcode is emitted at otxInsert in the current module.
;
; NOTE: This routine doesn't update the line count for any inserted BOLs.
;	The caller is responsible for updating the line count if it is
;	needed.
;
; Entry:
;	ax = otxInsert = offset into current module's text table where
;	   synthetic pcode is to be emitted.
;	dh = fDynBase = 1 if we were already in $DYNAMIC mode.
;	dl = fDynNew = TRUE if we want to move into $DYNAMIC mode.
; Exit:
;	ax = 0 if out-of-memory error (psw.z set as well)
;
;*************************************************************
PUBLIC	InsertDynDiff
InsertDynDiff PROC NEAR
	push	si			;save caller's si,di
	push	di
	xchg	di,ax			;di = text offset where pcode is to go
	cmp	dh,dl
	je	InsDynGoodExit		;brif we're already in mode we want

	mov	si,op_Static
	or	dl,dl
	je	ItsStatic		;brif new state is $STATIC
	mov	si,op_Dynamic
ItsStatic:
	;insert the pcode into text table in reverse order
	push	di			;push source of move to TxtMoveUp
	PUSHI	ax,10d			;push number of bytes to insert
	call	TxtMoveUp
	or	ax,ax
	je	InsDynExit		;return FALSE if out-of-memory

	GetSegTxtTblCur es		;es = seg adr of heap entry for txt tb
	mov	ax,opBol
	stosw				;insert beginning-of-line opcode
	mov	ax,opStRem
	stosw				;insert REM opcode
	sub	ax,ax
	stosw				;insert rem's cb operand
	mov	ax,si			;ax = op_Static or op_Dynamic
	stosw
	sub	ax,ax
	stosw				;insert op_Dynamic/op_Static's operand
InsDynGoodExit:
	mov	ax,sp			;return TRUE (non-zero)
InsDynExit:
	pop	di			;restore caller's si,di
	pop	si
	or	ax,ax			;set condition codes for caller
	ret
InsertDynDiff ENDP

;*************************************************************
; void SqueezeDefs(si:otxCur)
; Purpose:
;	Squeeze out all opStDefType, op_Dynamic and op_Static opcodes
;	from otxCur to the end of the current text table.
;	We squeeze them out and synthetically generate new ones
;	to eliminate redundant statements which could be introduced
;	as a result of moving SUBs and FUNCTIONs to their own text tables
;	during ASCII load.
; Entry:
;	si = text offset where we are to start deleting opcodes
; Exit:
;	The static variable [fProcDyn] is TRUE if a $DYNAMIC deleted
;	later than a $STATIC, i.e. the state at the end of the deleted
;	range was $DYNAMIC and not $STATIC.
;
;*************************************************************
cProc	SqueezeDefs,<PUBLIC,NEAR>,<si,di>
	localW	otxLastBol
	localB	fDeleted
	localB	opIndex
cBegin
	sub	ax,ax
	mov	[fProcDyn],al		;default state is $STATIC
	mov	[fDeleted],al		;so far we've deleted nothing
	dec	ax			;ax = UNDEFINED
	mov	[otxLastBol],ax

	push	si			;pass otxCur
	PUSHI	ax,<CODEOFFSET tOpProcHdr>
	call	TxtFindOp		;ax = text offset to 1st found opcode
					;dl = [txtFindIndex]
	cmp	dl,PROCHDR_opEot
	jb	NotEot
	jmp	SqExit			;brif end of table

NotEot:
	xchg	si,ax			;update si = otxCur
	cmp	dl,PROCHDR_opBolMin
	jb	SqLoop			;brif not opBol or opBolSp
	mov	[otxLastBol],si		;save offset to last start of line

;NOTE that TxtFindNextOp(x, p) sets txtFindIndex but
;TxtSkipOp() does not
;dl = txtFindIndex from last call to TxtFind[Next]Op
;
SqLoop:
	mov	[opIndex],dl		;save opcode's id (PROCHDR_xxx)
	push	si			;pass search start text offset
	PUSHI	ax,<CODEOFFSET tOpProcHdr>
	call	TxtFindNextOp		;ax=txt offset to next opcode from list
	xchg	di,ax			;di=txt offset

	mov	ax,si			;pass search start text offset in ax
	call	TxtSkipOp		;ax = offset to opcode after si
	cmp	ax,di
	je	SameOp			;brif this opcode is in list of
					;opcodes which could be deleted

	;some opcode not found in tOpProcHdr is on this line,
	;remember that we can't delete this line.
	
	mov	[otxLastBol],UNDEFINED
SameOp:
	mov	al,[opIndex]		;al = id of previous opcode
	cmp	al,PROCHDR_opStRem
	jae	GotRem			;brif REM or bol opcode
	;current opcode is opStDefType, op_Static, or op_Dynamic, delete it.
	push	ax			;save id of previous opcode

	push	di			;pass ptr beyond opcode to be deleted
	push	si			;pass ptr to opcode to be deleted
	call	TxtMoveDown		;delete current opcode

	pop	ax			;restore al = id of previous opcode
	mov	[fDeleted],1		;remember that something on this
					; line has been deleted
	mov	di,si			;otxCur remains unchanged for next
					; iteration of loop (because we
					; deleted the opcode)
	cmp	al,PROCHDR_op_Dynamic
	jne	NotDyn			;brif not $DYNAMIC
	mov	[fProcDyn],1
NotDyn:
	cmp	al,PROCHDR_op_Static
	jne	ChkNextOp
	mov	[fProcDyn],FALSE
	jmp	SHORT ChkNextOp

;al = PROCHDR_xxx for current opcode (we know its a REM or Begin Of Line opcode)
GotRem:
	cmp	al,PROCHDR_opBolMin
	jae	ChkNextOp		;brif opcode isnt opQuoteRem or opStRem
	lea	dx,[si+6]		;dx = otxCur + 6
	cmp	di,dx
	jbe	ChkNextOp		;brif REM has no text after it (kill it)
	;REM has significant text after it - can't delete it
	mov	[otxLastBol],UNDEFINED	;remember we can't delete this line
;Now test the opcode after this one
ChkNextOp:
	cmp	[txtFindIndex],PROCHDR_opBolMin
	jb	SqNext
	;next opcode is an opBol or opBolSp or opEot
	cmp	[otxLastBol],UNDEFINED
	je	SqNoDel			;brif we can't delete this line
	cmp	[fDeleted],FALSE
	je	SqNoDel			;brif some opcodes on this line has
					;not been deleted
	cmp	al,PROCHDR_opBolMin
	jae	SqNoDel

	;we have deleted all opcodes on this line, delete opBol as well.
	push	word ptr [TxtFindIndex] ;preserve index of last TxtFind call

	push	[otxLastBol]
	push	di
	call	TxtDelete
	mov	di,[otxLastBol]
	pop	ax
	mov	[TxtFindIndex],al	;restore TxtFindIndex for last TxtFind
					; call prior to TxtDelete
SqNoDel:
	mov	[fDeleted],FALSE	;so far, we've deleted nothing on line
	mov	[otxLastBol],di		;remember where last start of line
					; was so we can delete it later if
					; nothing significant is on the line
SqNext:
	mov	dl,[txtFindIndex]
	mov	si,di			;advance to next op (otxCur = otxNext)
	cmp	dl,PROCHDR_opEndProg
	jae	SqExit			;brif end of table
	jmp	SqLoop

SqExit:
cEnd


;*************************************************************
; boolean NEAR LoadEnterProc(otxProcDef)
; Purpose:
;	Called during ASCII Load when we encounter a SUB or FUNCTION
;	statement.  SUB/FUNCTION definition line has already been emitted.
;	Insert synthetically generated DEFxxx statements to make this
;	text table in the same state as the module's table where the
;	SUB/FUNCTION statement was seen.
;
; Entry:
;	procedure's text table is active
;	otxProcDef = where we were in module's text table when the procedure
;	   definition was seen.  We need this so we can update
;	   linked lists which thread through the module's text table.
;
; Exit:
;	modifies ps.bdpDst
;	grs.fDirect = FALSE
;	If out-of-memory,
;	    returns 0 (FALSE),
;	    text table which generated OM error is active.
;	else
;	    ax is non-zero
;	    procedure's text table is active
;	condition codes set based on value in ax
;
;*************************************************************
;.errnz ET_SD - ET_I2 - 4	 ;code assumes ET_I2...ET_SD
cProc	LoadEnterProc,<PUBLIC,NEAR>,<si,di>
	parmW	otxProcDef
	localW	oPrsProc
	localW	otxStartRem
	localW	otxStartBlank
	localW	otxPrev
	localB	prevType
	localB	fInRemBlock
cBegin
	mov	ax,[grs.GRS_oPrsCur]
	mov	[oPrsProc],ax		;remember which proc we just entered

	mov	si,[txdCur.TXD_bdlText_cbLogical]
					;si = current size of text table
					;     before synthetic code emitted
	SetStartOtx ax			;ax = start of text
	mov	bx,dataOFFSET tEtTemp
	call	OtxDefType		;fill tEtTemp with default types
					; i.e. ET_R4 for all letters


	;If we've seen a $DYNAMIC more recently than a $STATIC before
	;place where this procedure occurred in source file,
	;emit a $DYNAMIC at start of procedure's text table
	
	FLoadActive
	je	NotLoading		;brif being called for an edit, not load
	SetStartOtx ax			;ax = start of text
	mov	dh,al			;old state was $STATIC
	mov	dl,[fDynArrays]		;dl = new state ($STATIC or $DYNAMIC)
	mov	[fDynArraysMod],dl	;save so LoadExitProc can restore
	call	InsertDynDiff
	je	J1_LentEx		;return FALSE if out-of-memory error

NotLoading:
	;emit opStDefType opcodes which represent text table's initial state.
	PUSHI	ax,<dataOFFSET tEtTemp>
	PUSHI	ax,<dataOFFSET ps.PS_tEtCur>
	SetStartOtx ax			;ax = start of text
	call	InsertEtDiff
	je	J1_LentEx		;return FALSE if out-of-memory error
	neg	si
	add	si,[txdCur.TXD_bdlText_cbLogical]
					;si = txdCur.bdlText.cbLogical
					;   - cbLogical before insertion
					;   = #bytes of synthetic pcode emitted
	push	si
	mov	bx,si
	SetStartOtx si			;si = start of text
	call	TxtInsUpdate		;update line count from 0 to si for
DbAssertRel ax,ne,0,CP,<Unexpected OM error in LoadEnterProc>
	pop	si			;si = number of bytes inserted
	FloadActive			;test if we are currently loading -
					;  if not then we don't migrate rem's
	jne	InsOk			
J1_LentEx:
	jmp	LentEx			;return value in ax
InsOk:

	;Find the start of a contiguous block of Comments/Bol opcodes
	;that precedes the SUB definition.  Keep this block with the
	;procedure window, not the module window.  This allows users
	;to put comment blocks before their SUB/FUNCTION definitions
	
	;When done, we'll move the comment block, and delete the blank
	;lines preceeding the comment block.  If we didn't delete these
	;blank lines, several contiguous subs with 1 blank line between
	;would leave a huge block of blank lines.
	
	call	PrsDeactivate		;make module's txt table active
	mov	di,[otxLastProc]	;otxCur = otxLastProc
	mov	[otxStartRem],di
	mov	[otxStartBlank],di
	mov	[prevType],PROCHDR_opBol
	mov	[fInRemBlock],0
LentLoop:
	push	di			;pass otxCur to TxtFindNextOp
	PUSHI	ax,<CODEOFFSET tOpProcHdr>
	call	TxtFindNextOp
	cmp	dl,PROCHDR_opEot
	je	LentLoopEx
	mov	cx,[txdCur.TXD_bdlText_cbLogical]
	sub	cx,[cbAftMerge]		;cx = otx beyond any MERGEd text
	cmp	ax,cx
	jb	NotPastMerge		;brif not beyond MERGE's insertion point
	mov	dl,PROCHDR_opEndProg	;treat it like reaching opEndProg
	xchg	ax,cx			;use otx of merge insertion point
NotPastMerge:
	mov	dh,[prevType]		;dh = txtFindIndex for prev iteration
	mov	[prevType],dl		;dl=type of cur opcode, dh=type of prev
	push	dx

	xchg	di,ax			;update di = otxCur, ax = otxPrev
	mov	[otxPrev],ax
	call	TxtSkipOp		;ax = otx beyond opcode otxPrev

	pop	dx			;restore dl=type of cur, dh=type of prev
	cmp	ax,di
	mov	ax,di			;setup for branch to StartBlankBlock
	jne	StartBlankBlock		;brif next opcode wasn't in tOpProcHdr

	cmp	dh,PROCHDR_opBolInclude
	jae	StartBlankBlock		;brif last line came from $INCLUDE file
	cmp	dl,PROCHDR_opBolMin
	jb	InCommentBlk		;brif current op isn't a BOL, bolSp, or
					; opEndProg opcode
	cmp	dh,PROCHDR_opBolMin	;check type of previous opcode
	jb	InCommentBlk		;brif not looking at blank line

	;We now know that di points to opBol that terminates a blank line
	;If any non-blank lines have been seen since the last blank line,
	;reset the start of the blank line block.
	
	cmp	[fInRemBlock],0
	je	StartRemBlock		;brif not start of new block of blanks
	mov	ax,[otxPrev]		;ax points to start of blank line

;we saw blank line - don't include it with SUB's comment header block
; but include it in block of leading blank lines to be deleted.
; ax either points to start of blank line, or within line that is to
; remain at module level.
;
StartBlankBlock:
	mov	[fInRemBlock],0
	mov	[otxStartBlank],ax	 ;reset start of blank line block

;Found an opcode not in tOpProcHdr, or a blank line.
;di points to next opcode found which was in table (di > ax).
;
StartRemBlock:
	mov	[otxStartRem],di	;otxStartRem = otxCur
	jmp	SHORT LentNext

InCommentBlk:
	mov	[fInRemBlock],1		;causes any subsequent blank lines
					; to reset otxStartBlank
LentNext:
	cmp	[prevType],PROCHDR_opEndProg
	jb	LentLoop		;brif end of text table

;[otxStartRem] points to 1st opcode before opStSub which could be
; included with SUB.  For example, if pcode was:
;  opBol opStPrint opQuoteRem
;  opBol opRem
;  opBol opStSub
; otxStartRem would point to the opQuoteRem opcode, meaning the 2nd
; line should be included with the SUB.
; Move otxStartRem to the start of the next line.
;
LentLoopEx:
	push	[otxStartBlank]		; push otx arg.
	call	OtxBolNext0		;ax points to next opBolxxx if ax
	mov	[otxStartBlank],ax	; doesn't currently point to opBolxxx

	push	[otxStartRem]		; push otx arg.
	call	OtxBolNext0		;ax points to next opBolxxx if ax
	mov	[otxStartRem],ax	; doesn't currently point to opBolxxx
	mov	di,[otxProcDef]
	sub	di,ax			;di = cbMove (otxProcDef - otxStartRem)
	je	NoMove			;brif there is any text to be moved

; move cbMove(di) bytes from module's to proc's text table
; Start by making sure there is room for temp buffer & in proc's text table
;
	push	[oPrsProc]
	call	PrsActivateCP		;make new proc's txt table active

	push	di			;pass cbMove
	call	TxtFree			;make sure there's room in prs txt tbl
	je	JE1_LentEx		;brif out-of-memory error
	call	PrsDeactivate		;make module's txt table active

	;Move pcode from module's text table to bdlScrap	[8]
	;  CALL TxtCopyScrap(otxStartRem, StartOtx, di, TRUE)		[8]
	push	[otxStartRem]		;pass otx for start of move
	SetStartOtx ax			;[39]ax = StartOtx
	push	ax			;copy to start of scrap. 0 offset
	push	di			;pass cbMove
	push	sp			;push TRUE so text will be deleted
					;	from text table
	call	TxtCopyScrap		;move bytes from txdCur to bdlScrap
JE1_LentEx:
DJMP	je	LentEx			;return FALSE if out-of-memory

	push	[oPrsProc]
	call	PrsActivateCP		;make new proc's txt table active

	;note that we already called TxtFree to ensure we have enough memory
	;so no error is possible.
	
	call	TxtInsScrap		;insert bdlScrap in txdCur @ si

	mov	bx,di			;bx = cbMove
	add	bx,si			;bx = offset beyond pcode insertion
	call	TxtInsUpdate		;update line count for inserted lines
DbAssertRel ax,ne,0,CP,<Unexpected OM error 2 in LoadEnterProc>

	;squeeze all $DYNAMIC, $STATIC and DEFxxx statements out
	;of block copied from module.  They are redundant after
	;calling InsertEtDiff and InsertDynDiff
	
	call	SqueezeDefs		;takes parm in si
NoMove:
	call	PrsDeactivate		;make module's txt table active

;Delete redundant synthetic DEFxxx statments generated by last LoadExitProc()
;if no user-generated statements were loaded in between.
;If we didn't do this, each time SOME programs are Ascii loaded and saved
;and they contain one or more blank lines between subs, and so
;they grow by the introduction of these redundant DEFtype statements.
;
	mov	ax,[otxDefEnd]
	cmp	ax,[otxStartBlank]
	jne	NoDeadDefTypes		;brif non-synthetic lines exist
					; between end of synthetic lines and
					; start of leading blank lines
	mov	ax,[otxDefStart]
	mov	[otxStartBlank],ax	;else delete synthetic ones
NoDeadDefTypes:
	;Now delete blank lines that preceeded the comment block.
	mov	ax,[otxStartBlank]
	mov	[otxLastProc],ax
	push	[otxUpdLinks]		;pass otxUpdLinks to UpdateLinks
	push	ax			;pass otxLastProc to UpdateLinks
	;NOTE: parms to UpdateLinks are on stack
	push	ax			;pass start of block to delete
	push	[otxStartRem]		;pass end of block to delete
	call	TxtDelete		;delete blank lines from mrs text table
	call	UpdateLinks		;update linked lists which thread
					; through module's pcode up to where
					; SUB/FUNCTION line was encountered
	DbChk	TxdOps			;check for bad linked lists through
					; pcode, bad opcodes, etc.

	;save current def type state so we can bring module
	;back up to date at LoadExitProc (if any DEFxxx stmts occur
	;within the procedure being loaded)
	
	mov	ax,[otxLastProc]	;text offset for new end of module
	mov	bx,dataOFFSET tEtTemp
	call	OtxDefType		;fill tEtTemp with default types
					; at end of module

	mov	[otxUpdLinks],StartOtx	;next UpdateLinks will start at
					; start of new proc
	push	[oPrsProc]
	call	PrsActivateCP		;make new proc's txt table active

	;Tell AsciiMerge that we've changed text tables, and
	;where to insert next line in the procedure's text table
	
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	sub	ax,CB_EMPTY_TEXT - StartOtx ;ax = offset to opEndProg
	mov	[otxNewInsert],ax
	DbAssertRel ax,ne,0,CP,<LoadEnterProc: return value zero> 
LentEx:
	mov	[ps.PS_bdpDst.BDP_cbLogical],0 ;release temp buffer
	or	ax,ax			;set condition codes for caller
cEnd

;*************************************************************
; boolean NEAR LoadExitProc()
; Purpose:
;	Called during ASCII Load when we encounter an END SUB or
;	END FUNCTION statement.
;
; Entry:
;	procedure's text table is active
;
; Exit:
;	If out-of-memory
;	   returns psw.c set
;	else
;	   ax = otx where next stmt should be inserted in module's
;	        text table
;	module's text table is active
;
;*************************************************************
PUBLIC	LoadExitProc
LoadExitProc PROC NEAR
	push	si			;save caller's si
	push	[otxUpdLinks]
	push	[txdCur.TXD_bdlText_cbLogical]
	call	UpdateLinks		;update linked lists which thread
					; through procedure's pcode
	DbChk	TxdOps			;check for bad linked lists through
					; pcode, bad opcodes, etc.
	mov	ax,[otxLastProc]
	mov	[otxUpdLinks],ax
	xchg	bx,ax			;bx = otxLastProc
	mov	al,DT_EndProc
	call	TDataEntry		;add entry to table so we can
					;move DATA stmts from prs to mrs
	pushf				;preserve error status
	call	PrsDeactivate		;make module's txt table active
	popf				;recover error status
	je	LexExit 		;return PSW.C if out-of-memory

	;emit DEFxxx statements to module, bringing its state up
	;to the state at the end of the procedure, for BASCOM compatibility.
	;In BASCOM, DEFxxx statements are globally scoped, but in QBI,
	;we want to give the user the feeling that each procedure source
	;window stands independently of the module's source window
	
	mov	si,[txdCur.TXD_bdlText_cbLogical]

	PUSHI	ax,<dataOFFSET tEtTemp>
	PUSHI	ax,<dataOFFSET ps.PS_tEtCur>
	mov	ax,[otxLastProc]	;ax = insertion point
	call	InsertEtDiff
	je	LexExit 		;return PSW.C if out-of-memory

	mov	ax,[otxLastProc]	;ax = insertion point
	mov	dh,[fDynArraysMod]	;dh = old state for module level code
	mov	dl,[fDynArrays]		;dl = new state ($STATIC or $DYNAMIC)
	call	InsertDynDiff
	je	LexExit			;return PSW.C if out-of-memory error

	neg	si			;si = -(old cbLogical)
	add	si,[txdCur.TXD_bdlText_cbLogical]
					;si = txdCur.bdlText.cbLogical-(old)
 					; = #bytes of synthetic pcode emitted
	mov	ax,[otxLastProc]
	cmp	[otxDefEnd],ax
	je	GotDeadCode

	;some module level statements have been emitted since the
	;last synthetically emitted DEFxxx statements, so those
	;DEFxxx statements can't be deleted at the end of AsciiMerge.
	;The reason we try to delete them is to keep the file from
	;growing with every ASCII Save/Load by useless DEFxxx statements.
	
	mov	[otxDefStart],ax
;ax = otxLastProc
;si = #bytes of synthetic pcode generated
GotDeadCode:
	xchg	ax,si			;si = otx of 1st inserted DEFxxx
	xchg	ax,bx			;bx = cbInserted
	add	bx,si			;bx = otx after inserted DEFxxx
	push	bx			;preserve across call
	call	TxtInsUpdate		;update line count for inserted lines
DbAssertRel ax,ne,0,CP,<Unexpected OM error in LoadExitProc>
	pop	ax			;recover oTx after inserted text

	;Tell AsciiMerge that we've changed text tables, and
	;where to insert next line in the module's text table
	
	mov	[otxNewInsert],ax
	mov	[otxDefEnd],ax
	stc				;not out-of-memory
LexExit:
	cmc				;return psw.c set if out of memory
	pop	si			;restore caller's si
	ret
LoadExitProc ENDP


			;***************
			;* ASCII Load  *
			;***************


;*********************************************************************
; ushort NEAR GetLineBd(bx:pbd, cx=cbAppend)
; Purpose:
;	Input an arbitrarily long line into the buffer pointed to by pbd.
;
; Entry:
;	pbd points to destination buffer descriptor
;	cx = number of bytes already in buffer to preserve
;          = 0 if append is not desired
; Exit:
;	returns 0 if got a line, dx = length of line in bytes
;	if out-of-memory,
;	   returns ax = ER_OM
;	if end-of-file
;	   returns UNDEFINED
;	If new line is longer than pbd->cbLogical,
;	   pbd->cbLogical = new length
;	Line is 0-byte terminated
;
; Exceptions:
;	Can cause runtime error (Out of memory, I/O errors)
;
;*********************************************************************
PUBLIC GetALine				;for profiling only
cProc	GetALine,<NEAR>,<si,di>
	parmW	pBufDst			;destination buffer
	parmW	cbMax			;# of free bytes in dest. buffer
cBegin
	;Register usage:
	;	SI contains b$PTRFIL, i.e., ptr to FDB for input channel
	;	DI contains a ptr to the destination buffer
	;	BX contains the offset to next unread char in buffer
	;	CX contains cbMax (input)
	;	DX contains count of chars read in so far

	mov	si,[b$PTRFIL]
	or	si,si			;is the current channel valid?
	jne	GlStart 		;brif so, use it
	call	UpdChanCur		;refresh channel in case of error
	mov	si,[b$PTRFIL]
GlStart:
	FDB_PTR es,si,si		;(ES:)[SI] = *FDB
	mov	bx,FileDB.FD_BUFCNT	;offset to next unread char in FDB
	mov	di,[pBufDst]		;di = start of dest buffer
	sub	dx,dx			;dx = 0 (count of chars read so far)
GlStartLoop:
	mov	cx,[cbMax]
GlLoop:
	cmp	cx,dx
	jbe	J1_GlEol		;brif reached end of dest. buffer

	cmp	bx,FileDB.FD_INBUFCNT	;any unread chars left?
	jz	FillBuff		;  brif not
	mov	al,FileDB.FD_BUFFER[BX] ;fetch char from FDB

	inc	bx			;update offset to next unread char
GlCont1:
	cmp	al,ASC_DBL_QUOTE	;test for ", CR, TAB, LF, CTRL-Z
	jbe	GlQuoteOrLess		;brif TAB, or end-of-line
GlCont:
	mov	[di],al			;put char in dst buffer
	inc	di			;bump dst ptr
	inc	dx			;bump count
	jmp	SHORT GlLoop

J1_GlEol:
	jmp	short GlEol

FillBuff:
	push	dx			;preserve count
	mov	FileDB.FD_BUFCNT,bx	;update based on chars we've read so far
	call	B$IDISK_SINP		;al = next byte from file
	FDB_PTR es			;make sure ES still in FDB segment
	mov	bx,FileDB.FD_BUFCNT	;restore offset to next char in FDB
	pop	dx
	jcxz	GlEof			;brif EOF (cx set by B$IDISK_SINP)
	mov	cx,[cbMax]		;refresh
	jmp	short GlCont1


GlQuote:
	not	[fInQuote]		;toggle "quoted string" state
	jmp	SHORT GlCont

GlQuoteOrLess:
	je	GlQuote
	cmp	al,ASC_EOF		;test for CR, TAB, LF, CTRL-Z
	ja	GlCont			;brif not TAB or end-of-line
	jne	GlCrOrLess		;brif Not CtrlZ
	or	dx,dx			;was CtrlZ at beginning of line?
	je	GlEof			;brif so - treat as EOF
					;fall thru and loop to GlCont
GlCrOrLess:
	cmp	al,ASC_CR		;test for carriage return
	je	GlLoop			;brif carriage return - ignore
	cmp	al,ASC_LF		;test for line feed
	je	GlEol			;brif end-of-line
	cmp	al,ASC_TAB		;test for TAB
	jne	GlCont			;brif not TAB
	cmp	[fInQuote],0
	jne	GlCont			;brif within "quoted string"

	;expand TABs to spaces as follows:
	; emit (tabs - (col MOD tabs)) spaces
	; where col is current column (0..n) and
	;       tabs is #columns per tab stop (from user interface)
	; if buffer gets full before done expanding tabs, don't
	; worry about continuing into next buffer.
	
	or	[mrsCur.MRS_flags2],FM2_EntabSource ;remember to reentab at
						    ;ascii save time
	push	dx			;save current column
	xchg	ax,dx			;ax = current column
	cwd				;DX:AX = current column
	mov	cx,[tabStops]		;gets value from user interface

	; User interface should ensure that 0 is not a valid value
	; for tabstops.
	;
DbAssertRel	cx,nz,0,CP,<tab stops = 0 in ascii load>
	div	cx			;dx = remainder = modulus
	sub	cx,dx			;cx = # spaces to emit
	pop	dx			;restore current column
	mov	al,' '			;for now, map TAB to space
TabLoop:
	mov	[di],al
	inc	di			;bump dst ptr
	inc	dx			;bump count
	cmp	[cbMax],dx
	jbe	GlEol			;brif reached end-of-buffer
	loop	TabLoop
	jmp	GlStartLoop

;Reached end-of-file while reading a line
GlEof:
	or	dx,dx
	jne	GlEol			;brif line had valid data before EOF
	dec	dx			;return -1 for EOF

;Reached end-of-line while reading a line
GlEol:
	mov	FileDB.FD_BUFCNT,bx	;update based on chars we read
	xchg	ax,dx			;return byte count in ax
cEnd

CB_LINE EQU 80
cProc	GetLineBd,<PUBLIC,NEAR>,<si,di>
cBegin
	mov	[fInQuote],0		;toggle "quoted string" state
	mov	si,bx			;si points to buffer descriptor
	mov	di,cx			;di = cbLogical


;The following loop only executes 1 iteration for lines less than
;or equal to CB_LINE bytes in length
;
GlBdLoop:
	;make sure there's room for at least CB_LINE+1 more bytes
	push	si			;pass pointer to buffer descriptor
	lea	ax,[di+CB_LINE+1]	;ax = number bytes we need
	push	ax
	call	BdCheckFree
	or	ax,ax
	mov	ax,ER_OM		;prepare for out-of-memory error
	je	GlBdExit		;brif out-of-memory error
	mov	ax,[si.BD_pb]		;ax points to start of result buffer
	add	ax,di			;add cbLogical
	push	ax			;pass ptr to destination
	mov	ax,[si.BD_cbPhysical]	;ax = size of buffer
	sub	ax,di			;ax = number of free bytes in buffer
	dec	ax			;leave room for 0-byte terminator
	push	ax			;pass maximum byte count
	call	GetALine		;read bytes from the file, stop at CR
	cmp	ax,UNDEFINED		;test for end-of-file
	jne	GotData			;brif not end-of-file
	or	di,di
	jne	GotData1		;brif we've already got part of a line
	jmp	SHORT GlBdExit		;return end-of-file indication
					; ax = UNDEFINED (eof)

GotData:
	add	di,ax			;di = total # bytes read so far
GotData1:
	mov	ax,[si.BD_cbPhysical]
	dec	ax
	cmp	ax,di
	je	GlBdLoop		;brif read stopped because of end-of-buf
	mov	bx,[si.BD_pb]
	mov	BYTE PTR [bx][di],0	;zero terminate the line
	mov	dx,di			;return line length in dx
	inc	di			;include 0-byte terminator in count
	cmp	[si.BD_cbLogical],di
	jae	NotNewHigh
	mov	[si.BD_cbLogical],di	;cbLogical = length of longest
					;line read into this bd
NotNewHigh:
	sub	ax,ax			;not end-of-file return code
GlBdExit:
cEnd

;Near gateway to StatusMsgFar - display msg on status line
PUBLIC	StatusMsgCP, StatusMsg0CP
StatusMsg0CP PROC NEAR
	mov	ax,MSG_StatusEdit	; display edit window status line
StatusMsg0CP ENDP			; fall into StatusMsgCP

StatusMsgCP PROC NEAR
	cCall	StatusMsgFar,<ax>
	ret
StatusMsgCP ENDP

;*************************************************************
; ushort NEAR AsciiMerge(ax:otxInsert)
;
; Purpose:
;	Merge the contents of an ASCII source file into the current
;	text table.
;
; Entry:
;	ax = otxInsert = place to insert text
;	grs.oMrsCur, grs.oPrsCur have their usual meaning
;	Current input channel has been opened to file to be merged.
;	cAsciiLoadsActive == 1 if this file is being loaded/merged,
;		           > 1 if this file is being $INCLUDEd
;
; Exit:
;	grs.fDirect = FALSE
;	al = 0 if no error, else Standard BASIC error code (i.e. ER_xxx)
;	ps.bdpSrc is used
;
; Exceptions:
;	Can cause runtime error (Out of memory, I/O errors)
;
;*************************************************************
DbPub	AsciiMerge
cProc	AsciiMerge,<NEAR>,<si,di>
cBegin
	DbChk	Otx,ax			
	mov	si,ax			;ax = si = otxInsert
	sub	di,di			;initial line count = 0
	inc	[cAsciiLoadsActive]
	cmp	[cAsciiLoadsActive],1
	jne	AmLoop

	;this isn't a recursive $INCLUDE call, init static variables
	mov	[otxUpdLinks],ax
	mov	[otxDefEnd],ax
	mov	[otxDefStart],ax
	mov	[otxLastProc],ax
	sub	ax,ax
	mov	[fDynArrays],al		;default to $STATIC
	mov	[fMergeInSub],al
	dec	ax			;ax = UNDEFINED
	mov	[otxNewInsert],ax
	test	[txdCur.TXD_flags],FTX_mrs
	jne	NotMergingInSub
	mov	[fMergeInSub],al	;set fMergInSub non-zero
NotMergingInSub:
	test	[mrsCur.MRS_flags2],FM2_NoPcode
	jne	NoStatusMsg		;dont display "precompiling" if document
	mov	ax,-MSG_Loading		;display Loading msg in intense video
	call	StatusMsgCP		; to tell user we're loading
NoStatusMsg:

	;init default ps.tEtCur[] (default type array)
	mov	ax,si			;ax = text offset
	call	OtxDefTypeCur

;Get source lines and append them until EOF
AmLoop:
	mov	bx,dataOFFSET ps.PS_bdpSrc
	sub	cx,cx			;don't append, just fill buffer
	call	GetLineBd
	inc	ax			;test for UNDEFINED
	je	AmLoopExit		;brif got End-of-file (with ax=0)
	dec	ax			;ax = error code
	jne	AmLoopExit		;brif out-of-memory error

	test	di,1Fh			;update status line every 32 lines
	jne	NotYet			; just to tell user we're not hung
	cCall	UpdStatusLn,<di>
NotYet:
	inc	di			;bump line count
	push	[txdCur.TXD_bdlText_cbLogical]
					;save current size of text table

	test	[mrsCur.MRS_flags2],FM2_NoPcode ; Document file?
	je	@F			;brif not, insert into text table
	push	WORD PTR [mrsCur.MRS_pDocumentBuf] ; ptr to buffer info
	PUSHI	ax,<dataOFFSET ps.PS_bdpSrc.BD_pb> ; ptr to ptr to text
	call	AppendLineBuf		;Append line to document buffer
	and	ax, ER_OM		; convert 0, -1 to 0, ER_OM
	jmp	short InsertDone	
@@:					
	push	si			;insert the line in the text table
	push	si
	sub	ax,ax			;clear fNoInsert flag
	push	ax			;We have text to insert
	call	TxtChange		;al = error code

InsertDone:				
	pop	cx			;cx = old size of text table
	cmp	al,ER_OM
	je	AmLoopExit		;brif out-of-memory (only fatal error)
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	sub	ax,cx			;ax = size of inserted line
	add	si,ax			;update insertion point
	cmp	[otxNewInsert],UNDEFINED
	je	AmLoop			;brif we're still in same text table

	;We just got a SUB, FUNCTION, END SUB, or END FUNCTION statement
	;and as a result, have switched text tables
	
	mov	si,[otxNewInsert]	;si = new insertion point
	mov	[otxNewInsert],UNDEFINED
	jmp	SHORT AmLoop

;We are done loading this file.
;[al] = 0 if we've reached end-of-file.
;[al] = error code if fatal error occurred while loading.
;	Fatal errors (like out-of-memory) cause mrs to be discarded
;	unless they occurred within an INCLUDE file.
;
AmLoopExit:
	dec	[cAsciiLoadsActive]
	jne	StillLoading		;brif we're still in nested INCLUDE

	;this isn't a recursive $INCLUDE call
	push	ax			;save error code

	call	StatusMsg0CP		;tell user interface we're done loading

	;Now delete any redundant DEFxxx statements which were synthetically
	;generated by LoadExitProc.
	
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	sub	ax,CB_EMPTY_TEXT-StartOtx 
	cmp	ax,[otxDefEnd]
	je	GotRedundant		;brif module-level-code couldn't have
					; been inserted after last loaded proc
	dec	ax			;don't treat a blank line as significant
	dec	ax			; only a blank line could be 2 bytes
	cmp	ax,[otxDefEnd]
	jne	NoRedundant		;brif module-level-code was inserted
					; after last loaded procedure
GotRedundant:
	push	[otxDefStart]
	push	ax			;pass otxDefEnd
	call	TxtDelete
	mov	si,[otxDefStart]	;update links up to deleted text
NoRedundant:
	;update linked lists which thread through pcode
	
	push	[otxUpdLinks]
	push	si
	call	UpdateLinks
	DbChk	TxdOps			;check for bad linked lists through
					; pcode, bad opcodes, etc.

	pop	ax			;restore al = error code

;al = fatal error code (or 0)
StillLoading:
cEnd

;*************************************************************
; MakFilename
; Purpose:
;	Fill a buffer with <filename>.MAK
; Entry:
;	bx points to string descriptor for source of filename
;	di points to destination (must be FILNAML bytes long)
;	si points to destination string descriptor
; Exit:
;	ax = 0, or error code
;	es = ds = dgroup
;	buffer pointed to by di contains 0-terminated filename.MAK
;	si.pb points to 1st byte of destination buffer
;
;	Flags set on value in ax
;*************************************************************
cProc	MakFilename,<PUBLIC,NEAR>,<di>
cBegin
	call	NormFilename		;Normalize path of filename and add
					; .bas extension 
					; if filename doesn't already have ext.
					;We now know that the filename has
					; an extension, either the one supplied
					; by user or .BAS
	jne	BadMakFileName		;brif error - AX = error code

	add	di,[si.SD_cb]		;di points to 0-byte terminator
;search backward for start of extension
OMakLoop:
	cmp	di,[si.SD_pb]
	jbe	OMakDone		;brif got illegal filename like "/"
	dec	[si.SD_cb]
	dec	di
	cmp	BYTE PTR [di],'.'
	jne	OMakLoop		;brif haven't found extention yet
OMakDone:
	mov	WORD PTR [di+1],'AM'	;append "MAK" extension
	mov	WORD PTR [di+3],'K'	;put K and 0-terminator in buf
	add	[si.SD_cb],4
BadMakFileName:
	or	ax,ax			;set flags
cEnd

;*************************************************************
; OpenChan
; Purpose:
;	Call BASIC Runtime to open a named file.
; Entry:
;	dx = open mode
;	si points to string descriptor for filename
; Exit:
;	[chanCur] = channel #
;	al = standard error code if error (0 if not)
;	condition codes set based on value in al
; Exceptions:
;	assumes caller has called SetRtTrap to trap runtime errors
;	like File not found, etc.
;
;*************************************************************
PUBLIC	OpenChan
OpenChan PROC NEAR

	call	RtPushHandler		;save caller's runtime error handler
					; (NOTE: alters stack pointer)
					; (preserves dx)
	mov	ax,CPOFFSET OcExit	;if any runtime errors occur,
	call	RtSetTrap		; branch to OcExit with sp,di =
					; current values  (preserves dx)
	push	dx			;save open mode
	call	B$FREF			;ax = free channel number

	pop	dx			;dx = open mode
	push	ax			;preserve channel #
	push	si			;pass &sdFilenameNew
	push	ax			;pass channel
	sub	ax,ax
	mov	[chanCur],ax		;so CloseChan won't be called
					; if error in B$OPEN
	dec	ax			;ax = UNDEFINED
	push	ax			;no record size specified
	push	dx			;pass open mode
	call	B$OPEN			;open the file. errors trapped OcExit
	inc	[cChansOpen]		;no error, bump # opened channels
	pop	[chanCur]		;save channel
	call	UpdChanCur		;Tell runtime to use [chanCur] for
					; following I/O calls, test device type
	or	al,al			;is this a block device?
	mov	al,0
	jns	OcExit			;brif it is - no error
	mov	al,MSG_NotBlock		;"Can't load from non-block device"
;al = error code (0 if no error)
OcExit:
	call	RtPopHandler		;restore caller's runtime error handler
					; (saved on stack by RtPushHandler)
					; (preserves ax)
	or	al,al			;set condition codes for caller
	ret
OpenChan ENDP


;*************************************************************
; CloseChan/CloseMfh (EB)
; Purpose:
;	Call BASIC Runtime/OMEGA to close a file
; Entry:
;	[chanCur] = the channel-#/module-file-handle to be closed
;
;*************************************************************
cProc	CloseChan,<PUBLIC,NEAR>
cBegin
	mov	cx,[chanCur]
	jcxz	CcNeverOpened		;brif error before got file opened
	sub	ax,ax
	mov	[chanCur],ax		;remember channel is closed

	dec	[cChansOpen]		; assume close will work
	push	cx			;pass channel number of current load
	inc	ax			;ax = 1
	push	ax			;number of channels on stack
	call	B$CLOS			;close current file - no error possible
					; EXCEPT int 24's.
CcNeverOpened:
cEnd

;***
;UpdChanCur
;
;Purpose:
;	Near interface that uses the global 'chanCur' to make sure b$ptrfil
;	is updated to reflect the current input/output channel. Should be
;	called prior to using save/load I/O whenever the channel is changed,
;	or ANY heap movement could have occured.
;Entry:
;	chanCur assumed to be set up.
;Exit:
;	AL = the device number (as returned by B$CHAN).
;	The runtime b$ptrfil is updated.
;Exceptions:
;	Same as B$CHAN
;*******************************************************************************
PUBLIC	UpdChanCur
UpdChanCur	PROC NEAR
	push	[chanCur]
	call	B$CHAN			;ensure current output channel correct
	ret
UpdChanCur	ENDP


;*******************************************************************************
;ModulePermanent
;Purpose:
;	Set mrsCur.flags fTemporary bit to FALSE.
;	This is called via ForEachCP at the successful conclusion of a LOAD
;	for each text table (module and procedures) in every module.
;Entry:
;	none.
;Exit:
;	AX = TRUE (for ForEachCP)
;
;*******************************************************************************
ProcPerm PROC NEAR
	call	ClrBpTxt		;clear all breakpoints in this
					; text table

	;delete all Watch expressions in this text table
	or	[flagsTM],FTM_WatchPcode ;assume we have watch pcode
	call	OtxEndProg		;ax = otx to Watch pcode
	inc	ax			;skip opEndProg
	inc	ax
	push	ax			;pass start of block to delete
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
.erre	CB_EMPTY_TEXT - 4 EQ StartOtx
	dec	ax			;don't count opEot
	dec	ax
	push	ax			;pass end of block to delete
	call	TxtDelete

	call	TblReInclude
	mov	ax,sp			;return non-zero for ForEachCP
ProcPerm ENDP

ModulePermanent	PROC NEAR
	test	[mrsCur.MRS_flags],FM_TEMPORARY
	je	MpExit			;brif this isn't one of the modules
					; we just loaded
	and	[mrsCur.MRS_flags],NOT FM_TEMPORARY
	and	[mrsCur.MRS_flags2],NOT (FM2_Modified or FM2_ReInclude) 
					;reset modified and temp flags

	test	[mrsCur.MRS_flags3],FM3_Translated ; module binary xlated?
	jz	NotTranslated		; brif not -- FM2_Modified stays 0
	and	[mrsCur.MRS_flags3],NOT FM3_Translated	; reset temp flag
	or	[mrsCur.MRS_flags2],FM2_Modified	; mark as modified
					; so it will be saved in new format
NotTranslated:				

	test	[mrsCur.MRS_flags2],FM2_AsciiLoaded	
	jnz	MpExit			;brif module was not binary loaded

	;re-include all $INCLUDE files in this binary module
	;If we did it right after calling BinaryLoad, TxtChange would
	;have screwed up because it would think a LOAD was still active.
	
	mov	al,FE_CallMrs+FE_PcodePrs+FE_SaveRs
	mov	bx,OFFSET CP:ProcPerm
	call	ForEachCP
MpExit:
	mov	ax,sp			;return TRUE for ForEach...
	ret
ModulePermanent	ENDP


;**********************************************************************
; TxtGrowPsSrcEmScratch (pBd, cbNew)
; Purpose:
;	Called whenever we need to grow ps.bdpSrc or bdEmScratch.  The
;	editmgr REQUIRES that the logical size of these buffers are
;	the same.  This routine takes a bd and checks to see if the passed
;	bd is either ps.bdpSrc or bdEmScratch.	If so, both bds are grown
;	to be the same size.  If the bd is not either of these then we just
;	call BdRealloc.  If we get OM while trying to grow either ps.bdpSrc
;	or bdEmScratch, we will trim the cbLogical
;	routine is a nop.
;
;	Added with revision vision [68].
;
; Entry:
;	pBd - ptr to bd.
;	cbNew - new size of Bd
; Exit:
;	AX - 0 if OM error code.
;
;**********************************************************************
cProc	TxtGrowPsSrcEmScratch,<PUBLIC,FAR>,<si,di>
parmW	pBd
parmW	cbNew
cBegin
	mov	si,dataOffset bdEmScratch
	mov	di,dataOffset ps.PS_bdpSrc
	push	pBd			;pass pbdDst to BdRealloc
	push	cbNew			;pass cbNew to BdRealloc
	call	BdRealloc
	or	ax,ax
	je	TGPSES_OM		;brif out-of-memory

	mov	bx,di			;assume ps.bdpSrc needs grown too
	cmp	pBd,si			;is this bdEmScratch?
	je	GrowSpecBuf		;brif so - grow ps.bdpSrc too
	mov	bx,si			;assume bdEmScratch should grow
	cmp	pBd,di			;is this ps.bdpSrc?
	jne	TGPSES_X		;brif not - nothing special to do

; We get here if we have to grow either ps.bdpSrc or bdEmScratch.  These
; two buffers must grow together for the edit mgr.


GrowSpecBuf:
	push	bx
	push	cbNew
	call	BdRealloc		;realloc other buffer too
	or	ax,ax
	je	TGPSES_OM		;brif OM error

TGPSES_X:
DbAssertRel ps.PS_bdpSrc.BDP_cbLogical,e,bdEmScratch.BD_cbLogical,CP,<TxtGrowPsSrcEmScratch:bdEmScratch and ps.bdpSrc not same size>
cEnd

;if we get an out of memory while growing ps.bdpSrc or bdEmScratch.  Trim them
;both back to the smallest cbLogical.

TGPSES_OM:
	cmp	pBd,si			;is this bdEmScratch?
	je	TrimCbLogical		;brif so - trim back size of both
	cmp	pBd,di			;is this ps.bdpSrc?
	jne	TGPSES_X		;brif not - nothing special to do

TrimCbLogical:
	mov	bx,[si].BD_cbLogical	;get size of bdEmScratch
	cmp	bx,[di].BD_cbLogical
	jb	TrimPs			;trim Ps since it is larger
	mov	bx,[di].BD_cbLogical	;get size of ps.bdpSrc
	mov	[si].BD_cbLogical,bx	;set new size of bdEmScratch
	jmp	short TGPSES_X

TrimPs:
	mov	[di].BD_cbLogical,bx	;set new size of ps.bdpSrc
	jmp	short TGPSES_X


STKCHK_LoadFile EQU 350d ;actually 208d, add 142 for maintenance/uncertainty
;
;STKCHK_ToLoadFile is the number of bytes of stack space needed to get from
; UserInterface (where caller ensures STACK_CHECK bytes exist between sp
; and b$pend) and LoadFile().
;   UserInterface->LoadFile (188d) ([67] 174d)
;
STKCHK_ToLoadFile EQU 400d ;actually 188, add 212 for maintenance/uncertainty

;*************************************************************
; ushort LoadFile(psdFilename, otxInsert)
; Purpose:
;	This is called by LOAD executor, File/Load&Merge menu items,
;	and recursively from TxtChange in response to $INCLUDE.
;	If the filename has no extension, .BAS is appended to
;	a copy of psdFilename.  The copy is then 0-byte terminated.
;	It opens the specified file and loads it, optionally doing
;	a NEW.  Based on 1st byte in file, does an ASCII or BINARY load.
;	If a runtime error is encountered at any INCLUDE level,
;	this function backs all the way out, returning directly to
;	the caller for the 1st level of INCLUDE via RtSetTrap.
;
; Entry:
;	psdFilename points to string descriptor for filename to be loaded.
;	   (the filename itself need not be 0-byte terminated, and
;	    psdFilename.cb MUST not include any 0-byte terminator)
;	otxInsert = LF_NewProg if file may be ASCII or BINARY and if
;	            a NEW is to be performed before the load
;	          = LF_NewModule if file may be ASCII or BINARY and if
;	            a NEW is NOT to be performed before the load
;	          = LF_NewDoc if file is ASCII document
;	          = LF_ViewIncl if file is $INCLUDE file which is not
;	 	    to be loaded in-place, but into another mrs so user
;		    can edit it.
;	          = text offset into current module where text is to be inserted
;	            for MERGE or INCLUDE.  File must be ASCII format for this.
;	            For example, if otxInsert == 0, file's content will be
;	            inserted before text offset 0.
;		    It is important to call TxtDescan before computing
;		    the text offset for the insert.
;	stFileName [only used in EB] Fully qualified FileName in st format.
;	OpenMode = [only used in EB] Specifies whether the input file is to
;		    be loaded from the active database, from a file, or 
;		    whether this is unknown. This parameter is just passed
;		    through to OpenChan.
;
; Exit:
;	ax = Standard BASIC error code (i.e. ER_xxx) if error occurred.
;	   Possible errors include Out-of-memory (ER_OM)
;	   file I/O errors, File-not-found (ER_FNF), Duplicate Mrs
;	   (MSG_DupMrs), Duplicate Prs (MSG_DupPrs)
;	For all errors detected by LoadFile, txtErr.oRs is set to UNDEFINED,
;	   meaning no cursor positioning will take place when error is reported.
;	   Errors which can be represented with an opReParse are not reported
;	   until the user attempts to RUN the program, at which time, TxtDirect
;	   will re-parse and report them.
;	If called by an executor, (i.e. ExStRunFile) and an untrappable error
;	   occurred, grs.otxCur is set to 2, in which case, the module with
;	   the error is grs.GRS_oRsCur and the text offset is grs.otxCur.
;	If called by an executor, and a trappable error occurred,
;	   grs.otxCur is not altered, and the module with the error
;	   is grs.GRS_oRsCur and the text offset is grs.otxCur.
;	   
;	txtErr structure is filled in with error info
;	ps.bdpSrc is used.
;	rsNew is set by MrsMake for all cases but Merge (this tells the
;	  user interface to show this rs in a list window)
;	Preserves grs.fDirect in all cases (needed at least by exStRunFile
;	  when entered in direct mode and error (like file not found) occurs)
;	Preserve's caller's runtime-error-trap handler (if any) and [chanCur]
;
;*************************************************************
J1_RtErrOmStack:
	EXTRN	RtErrOmStack:NEAR
	jmp	RtErrOmStack

cProc	LoadFile,<PUBLIC,FAR>,<si,di>
	parmW	psdFilename
	parmW	otxInsert
	localW	oRsSave
	localW	otxCurSave
cBegin
	DbAssertRel [b$CurFrame],a,bp,CP,<LoadFile: b$CurFrame .GE. bp>
	;otherwise, runtime error recovery fails

	push	WORD PTR ([grs.GRS_fDirect]) ;save caller's fDirect
	push	[chanCur]		;save caller's current chan
					; (for recursive calls)
	call	RtPushHandler		;save caller's runtime error handler
					; (NOTE: alters stack pointer)
	mov	ax,CPOFFSET LfDone	;if any runtime errors occur,
	call	RtSetTrap		; branch to LfDone with sp,di =
					; current values

	;Runtime ensures we never enter the user interface with less
	;than STACK_CHECK bytes free.  Make sure that STACK_CHECK is big enough
	;to satisfy parser's requirements.
	
DbAssertRel <STKCHK_ToLoadFile+STKCHK_LoadFile>,b,STACK_CHECK,CP,<LoadFile stk>
	mov	ax,[b$pend]
	add	ax,STKCHK_LoadFile
	cmp	sp,ax
	jbe	J1_RtErrOmStack		;brif almost out of stack space

	mov	ax,[grs.GRS_oRsCur]
	mov	[oRsSave],ax

	mov	ax,[grs.GRS_otxCur]
	mov	[otxCurSave],ax

	sub	ax,ax
	mov	[chanCur],ax		;in case we don't get file opened
					; before we branch to LfDone (error)
	SetfDirect al			;we're dealing with text tables
					;       not direct stmt buffer
	dec	ax			;ax = UNDEFINED
	FLoadActive
	jne	Not1stLoad		;brif we're being called for
					; recursive load ($INCLUDE or
					; from LoadMakFile)
	mov	[oRsDupErr],ax		;so we can tell if any Duplicate
					; Prs or Mrs errors occur during
					; load

	mov	[cbAftMerge],0
	test	[txdCur.TXD_flags],FTX_mrs
	je	LfNotMerge		;brif we're in a SUB or FUNCTION
	mov	ax,[otxInsert]
	cmp	ax,LF_NewDoc
	jae	LfNotMerge		;brif we're not MERGEing a file
	sub	ax,[txdCur.TXD_bdlText_cbLogical]
	neg	ax			;ax = #bytes beyond insertion point
	mov	[cbAftMerge],ax

LfNotMerge:
Not1stLoad:

	mov	bx,[psdFilename]	;bx points to source filename sd
	push	ss			
	pop	es			; assure es==dgroup

	cmp	[fLoadInclude],NULL	; loading include file?
	jz	SkipSearch		; no, go process as usual

	mov	al,ER_ADF		; "advanced feature unavailable"
	jmp	short J1_LfDone 	; exit
SkipSearch:				
	mov	di,dataOFFSET NormFname
	mov	si,dataOFFSET sdNormFname

	call	NormFilename		;normalize path of filename and
					; append .bas to sd pointed to by si
	jne	J1_LfDone		;brif bad filename

Lf_Cont:				
	cmp	[fInitialized],FALSE
	je	InInit			;if we're initializing we want to
					;create the mrs for the module even
					;if the file isn't found.

;Before we toss the current text table (via NewStmt), make sure
;file exists, so we can report an error.  Note: this doesn't cover
;case where one of the modules in a multi-module load was not found.
;It is VERY expensive to handle the multi-module case, and
;handling the single module case lets the user test any error
;trapping code the same way as it would work in the compiler,
;since in the compiler, even a multi-module program consists of
;a single EXE file.  Handling multi-modules would be of little
;value if we didn't handle missing $INCLUDE files, which
;would be more than prohibitive.
;
	cCall	FileExists,<di>		;see if file [di] found
	or	ax,ax
	mov	al,ER_FNF
	jne	InInit
J1_LfDone:
	jmp	LfDone			;brif couldn't find filename

InInit:
	;Any errors encountered after this point are untrappable.
	call	CantCont		;loading/re-including a file prevents
					; CONTinue (because we can't back out
					; of it after the n'th line)
					; NOTE: CantCont closes all files
					; if CONT was possible, so this
					; must be called before OpenChan.
					; It also calls _BRUNINI which
					; shows and clears the user screen.
	mov	ax,[otxInsert]
	.errnz	LF_NewDoc - 0FFFCh
	cmp	ax,LF_NewDoc
	jb	LoadFile_Cont		;brif doing a MERGE or $INCLUDE
	.errnz	LF_NewProg - 0FFFFH
DbAssertRelB [cChansOpen],e,0,CP,<NewStmt called for recursive LoadFile()>
	call	NewStmt			;erase content of this module
	call	StatusMsg0CP		;NewStmt erases function key line (25)
					; which is same as status line
	mov	[otxCurSave],2		;so we will position cursor @
					; top of new window if ExStRunFile
					; calls RtErrorCODE
	mov	[oRsSave],UNDEFINED	;remember that we can't restore
					; the caller's oRs for error
					; recovery.  NewStmt also sets
					; grs.otxCont to UNDEFINED,
					; for runtime error recovery.
NoNewStmt:
	cCall	OgNamOfPsd,<si>		; get ogNam of given filename
	or	ax,ax			; OM error return?
	jnz	GotOgNam		;   brif not

	mov	al,ER_OM		
	jmp	SHORT J1_LfDone

GotOgNam:				
	push	ax			; parm to MrsMake
	mov	cx,FM_TEMPORARY + (100h * FM2_File) 
					;set TEMP in case LOAD fails somewhere
					;so we'll know to discard the mrs
	mov	ax,[otxInsert]
	cmp	ax,LF_ViewIncl
	jne	NotIncl
	or	ch,FM2_Include		;so we'll know to do a ReInclude
					;if/when this file gets saved.
NotIncl:
	cmp	ax,LF_NewDoc
	jne	NotDoc			;brif not loading a Document file
	or	ch,FM2_NoPcode
NotDoc:
	push	cx			;pass flags
	call	MrsMake			;create new mrs for this file
	or	ax,ax
	jne	J1_LfDone		;brif error, al = error code
	or	[mrsCur.MRS_flags3], FM3_NotFound 
	call	ModuleRudeEdit
LoadFile_Cont:
	push	[rsNew] 		;DoDrawDebugScr resets rsNew,
					; which was set by MrsMake.
					; We need to preserve this so correct
					; rs is active after MakeExe, or
					; MakeLib.
	call	DoDrawDebugScrFar	;so screen isn't blank with just
					; title bar for too long during a
					; binary load. (It was cleared by
					; CantCont above, and title may
					; have changed by MrsMake above)

	pop	[rsNew]
	;If we could assume DOS 3.0 or greater, (we can't yet) we could set
	;dx to (ACCESS_READ OR LOCK_WRITE) SHL 8 OR MD_SQI
	
	fLoadActive			
	jne	@F			
	call	TDataStart		;init for movement of DATA stmts
					; from prs(s) to mrs
	jne	LfDone			;[37] brif no error occurred
@@:					

	mov	dx,MD_SQI
	call	OpenChan		;Sets [chanCur] which tells LfDone to
					; close the file. Tells AsciiLoad/Binary
					; Load to use this channel
	jne	LfDone			;brif error
	and	[mrsCur.MRS_flags3],NOT FM3_NotFound 
	call	B$IDISK_SINP		;al = 1st byte from file
					; errors trapped @ LfDone
	jcxz	Empty_File		;brif trying to load an empty file
	push	ax			;save 1st char
	call	B$IDISK_BAKC		;put the char back in the buffer
	pop	ax			;restore al = 1st char of file
	cmp	al,BINSAV_1stByte	
	jne	DoAsciiLoad		;brif not binsav flag
	call	fEditorActive		; is this the Editor?
	jnz	DoAsciiLoad		; brif so, no binary files
	mov	al,ER_BFM		; otherwise, it is an error
	jmp	LfDone			

Empty_File:
	xchg	ax,cx			;ax = 0 == no error
	jmp	SHORT LfDone

DoAsciiLoad:
	mov	ax,[otxInsert]		;ax = line where text is to be inserted
	cmp	ax,LF_NewDoc
	jb	DoMerge			;brif doing a MERGE or $INCLUDE
	or	[mrsCur.MRS_flags2],FM2_AsciiLoaded
	SetStartOtx ax			;start load at offset 0
DoMerge:
	
DbAssertRelB [txdCur.TXD_scanState],ne,SS_EXECUTE,CP,<LoadFile: bad scan state>
	call	AsciiMerge		;merge file at offset [ax]
					;Note: AsciiLoad can make recursive
					; calls to LoadFile()
					; some errors trapped @ LfDone

;al = error code (0 if no error)
LfDone:
	mov	[txtErr.TXER_oRs],UNDEFINED
					;don't try to position cursor to
					; offending stmt for errors caused
					; by LoadFile.  Values of txtErr.otx
					; and txtErr.oSrc are unimportant
					; if txtErr.oRs = UNDEFINED
	cmp	al,ER_FNF		;test for FileNotFound
	jne	NotInInit		;brif not
	cmp	[fInitialized],FALSE
	jne	NotInInit
	FLoadActive
	jne	NotInInit
	cmp	[cInclNest],0		;test for INCLUDE file not found
	jne	NotInInit		; brif include file not found
	test	[cmdSwitches],CMD_SW_RUN ;Want to Run program, or just load it?
	jne	NotInInit		;brif /RUN <filename>
	and	[mrsCur.MRS_flags3],NOT FM3_NotFound 
	sub	ax,ax			;qb <filename> when filename not
					; found should just create file
;al = error code (0 if no error)
NotInInit:
	sub	ah,ah			;ax = result
	xchg	si,ax			;si = result
	call	CloseChan		;close [chanCur]
					; don't care about int 24 errors
					; here, since we were reading
	FLoadActive
	jne	LfExit1			;brif not done with multi-module load
					; or recursive $INCLUDE
	call	TDataEnd		;move DATA stmts from prs(s) to mrs
	je	NoDataMoveErr
	mov	si,ER_OM		;return out-of-memory error
NoDataMoveErr:
	call	LfDupRs			;if duplicate mrs or prs encountered
					; during load, set si=errcode
					; if dup mrs, (sets txtErr.oRs),
					; it will be reset to UNDEFINED if mrs
					; is discarded.
	or	si,si			;test result
	je	MakeMods_Perm		; brif no error

	or	[flagsTm],FTM_PrsDefDeleted
					;make sure discarded PRSs are freed
	call	MrsDeactivate		; required so NextMrsFile starts
					; at the beginning
TempMrs_Discard_Loop:			
	call	far ptr NextMrsFile	; activate next file mrs
	inc	ax			; no more file mrs's?
	jz	@F			; brif so - exit loop

	test	[mrsCur.MRS_flags],FM_TEMPORARY
	je	TempMrs_Discard_Loop

	mov	ax,[txtErr.TXER_oRs]
	cmp	ax,[grs.GRS_oMrsCur]
	je	DmNotErr

	mov	[txtErr.TXER_oRs],UNDEFINED
					;don't try to position cursor to
					; offending stmt for error.
DmNotErr:
	call	MrsDiscard
	jmp	TempMrs_Discard_Loop	

MakeMods_Perm:				
	mov	al,FE_PcodeMrs+FE_CallMrs+FE_SaveRs+FE_TextMrs 
	mov	bx,OFFSET CP:ModulePermanent ;reset 'fTemporary' in each mrs
	call	ForEachCP
@@:					

; We call ChkAllUndefPrsSaveRs here to make sure that FTM_PrsDefDeleted
; flag is reset.  If this flag didn't get reset, then the next PRS
; that is created would accidentally be freed in TxtChange.  The case
; where this flag doesn't get reset is when we don't find any Temp mrs's.

	call	ChkAllUndefPrsSaveRs	;search for new defining
					;references for Prs entries
					;which had their "defining"
					;reference deleted.

LfExit1:
	mov	ax,[oRsSave]
	inc	ax
	je	LfExit2			;brif entry mrs has been discarded
	dec	ax			;ax = oRs to restore
	push	ax
	call	RsActivateCP		;activate it (for ExStRun)
LfExit2:
	mov	ax,[grs.GRS_oRsCur]
	inc	ax			;test for UNDEFINED
	jne	LfExit3			;brif not - we need an oMrsCur
	DbAssertRel [grs.GRS_oMrsMain],ne,UNDEFINED,CP,<LoadFile: no oMrsMain>
	mov	ax,[grs.GRS_oMrsMain]
	mov	[rsNew],ax		;if UserInterface has not yet been
					; called, we need rsNew non-zero
					; to cause windows to be initialized
	push	ax
	call	RsActivateCP		;activate oRsMain
LfExit3:
	mov	ax,[otxCurSave]
	mov	[grs.GRS_otxCur],ax	;restore caller's grs.otxCur (or
					; set it to 2 if NewStmt was done)
					; This is done for ExStRunFile.
	call	RtPopHandler		;restore caller's runtime error handler
					; (saved on stack by RtPushHandler)
	pop	[chanCur]
	cmp	[chanCur],0		;do we need to update channel for
	jz	NoChanUpd		;recursive opens?
	call	UpdChanCur		;Update channel
NoChanUpd:

	; The user interface needs the source and scratch buffers to be the
	; same size.  If the scratch is smaller, then horizontal scrolling
	; (like CTRL-PGDN) could cause garbage characters to be displayed.
	; This does not cause any data or code corruption, but is visually
	; disconcerting.  Since this is only a visual bug, special out-of-
	; memory handling is not necessary.

	PUSHI	ax,<dataOFFSET bdEMScratch>;pass Scratch buffer
	mov	ax,[ps.PS_bdpSrc.BDP_cbLogical] ; get current size
	push	ax
	call	TxtGrowPsSrcEmScratch	;grow bdEmScratch
	;can ignore OM errors here since cbLogical will have been
	;trimmed to the same value for both buffers.

	PopfDirect ax			;restore direct mode status

	call	fEditorActive		; Did we start with /EDITOR
	jz	@F			; no, ok to exit
	test	[mrsCur.MRS_flags2],FM2_NoPcode ; is it a document table?
	jnz	@F			; brif so, it is ok

	; We must insure that we are using a document table, so create
	; a new one

	mov	ax,DATAOFFSET szUntitled; psz of title
	push	ax			; first parm to OgNamofPsd
	push	ax			; parm to CbSz
	call	CbSz			; get the length
	push	ax			; second parm to OgNamofPsd
	call	OgNamofPbCb		; convert to OgNam
	xchg	bx,ax			; save OgNam in BX
	or	bx,bx			; did we make the OgNam?
	mov	ax,ER_OM		; assume not, prepare OM error
	jz	NoOgName		; no, give OM Error
	push	bx			; ogNam
	PUSHI	ax,<(FM2_File + FM2_NoPCode) * 100h> ; flags
	call	MrsMake 		; create the MRS
NoOgname:
	or	ax,ax			; is there an error
	jz	@F			; brif not, all ok
	or	si,si			; is there already an error?
	jz	SaveError		; brif not, use the new error
@@:					
	xchg	ax,si			;ax = result
	or	ax,ax			;set condition codes for caller
	je	LfExit			;brif no error occurred
SaveError:
	mov	[txtErr.TXER_errCode],ax
LfExit:
cEnd



;*********************************************************************
; SetPsErrMsg
; Purpose:
;	Set the parser's error message buffer to the name of a
;	register set (module or procedure).
; Entry:
;	ax = oRs (condition codes set based on value in ax)
; Exit:
;	ps.bdErr contains ASCII name of module/procedure.
;	ax = 0 if out-of-memory (condition codes set based on value in ax)
;
;*********************************************************************
cProc	SetPsErrMsg,<PUBLIC,NEAR>
cBegin
	push	ax			;preserve the oRs
	cCall	RsActivateCP,<ax>	;activate the oRs
	pop	ax			;ax = oRs
	mov	bx,[prsCur.PRS_ogNam]	
	or	ax,ax			;test the oRs
	js	ItsPrs			;brif procedure (not module)
	mov	bx,[mrsCur.MRS_ogNam]	
ItsPrs:
	;set ps.PS_bdErr contents to mrs/prs name
	mov	[ps.PS_bdErr.BD_cbLogical],0 ;set bdErr buffer to empty
	PUSHI	dx,<DATAOFFSET ps.PS_bdErr> ;pass param1 = ptr to Bd
	push	bx			;pass param2 = ogNam 

	call	BdAppendOgNam		; copy name of proc/module to err 
					;   buf, alloc err buf if required
	or	ax,ax			;test for out-of-memory error
cEnd

;*********************************************************************
; LfDupRs
; Purpose:
;	Report duplicate prs or mrs during LOAD
; Entry:
;	if [oRsDupErr] <> UNDEFINED, it identifies duplicate PRS
;	or MRS encountered during a Load
; Exit:
;	if duplicate mrs was found
;	   si = MSG_DupMrs
;	   txtErr.oRs = oRs of existing mrs, txtErr.otx = 0
;	else if duplicate prs was found
;	   si = MSG_DupPrs
;	else
;	   si is unchanged
;
;*********************************************************************
cProc	LfDupRs,<PUBLIC,NEAR>
cBegin
	mov	ax,[oRsDupErr]
	inc	ax
	je	DupErrExit		;brif no duplicate mrs or prs
	dec	ax			;restore ax = oRsDupErr
	push	ax			;save oRsDupErr
	call	SetPsErrMsg		;set ps.bdErr to name of oRs ax
	pop	ax			;restore oRsDupErr
	;if static err buf, OM err not possible
	mov	si,ER_OM
	je	DupErrExit		;return ER_OM if SetPsErrMsg error
	or	ax,ax
	mov	si,MSG_DupPrs
	js	LFDGotPrs		;brif ax is prs, not mrs
	.errnz	MSG_DupPrs - MSG_DupMrs - 1
	dec	si			;si = MSG_DupMrs
	mov	[txtErr.TXER_oRs],ax	;as convenience to user, position
	mov	[txtErr.TXER_otx],UNDEFINED ; cursor at top of module that was
	mov	[txtErr.TXER_oSrc],0
					; already loaded.
LFDGotPrs:
DupErrExit:
cEnd

;*************************************************************************** 
; NormFileName
;
;Purpose:
;	Normalizes the path of a filename and appends ".bas" if there 
;	previously was no extension. Uses runtime routines B$IValidatePath
;	Generates the fully-qualified pathname from a filename.
;	Converts name to upper case, and removes "\..\" and "\.\" from the
;	pathname.  Resulting pathname is null-terminated.  Appends the drive
;	letter and a ":" to the front if it doesn't have one already.
;
;	Names within pathname do not have '*'s expanded to '?'s.  No checks
;	are done for too many slashes or length.  These checks are not
;	necessary, since the DOS calls that take as input this processed
;	pathname should perform those checks.  Both forward and backward
;	slashes are accepted as path characters.
;
;	Filenames without extentions that are longer than 8 characters have
;	a "." inserted after the 8th position.  An error is given if a filename
;	has more than one dot.
;	
;
;Entry:
;	bx = string descriptor of filename
;	di = destination for processed pathname 
;		(for QB must be FILNAML bytes long)
;	     For EB this is filled with an st for the module name
;	        and the SD_pb field of bx must point to the second 
;		byte in this buffer.
;	si = destination string descriptor
;	grs.GRS_oMrsCur: [in EB] the name of the current module 
;			is passed to OMEGA
;
;Exit:
;	ax = standard error code if error (0 if not)
;	condition codes set based on value in al
;	si.SD_cb = length of pathname (NOT including the null byte)
;	es = ds
;	Upper case, fully-specified pathname filled in.
;
;******************************************************************************
DbPub	NormFileName
cProc	NormFileName,<NEAR>
cBegin

	call	RtPushHandler		;save caller's runtime error handler
					; (NOTE: alters stack pointer)
					; (preserves ax,bx, dx)
	mov	ax,CPOFFSET NfnExit	;if any runtime errors occur,
	call	RtSetTrap		; branch to NfnExit with sp,di =
					; current values  (preserves bx,cx, dx)

	push	[bx.SD_pb]		;parm1 = near pointer to filename
	push	[bx.SD_cb]		;parm2 = filename length
	push	di			;parm3 = near pointer to destination
	PUSHI	ax,<dataOFFSET b$BAS_EXT> ;parm4 = near pointer to ".bas"
	call	B$IValidatePath		; ax = length of resulting filename
					; NOT including 0 byte
	mov	[si.SD_cb],ax		;set byte count field of SD
	xor	ax,ax			;clear ax to indicate no error

;al = error code (0 if no error)
NfnExit:		
	call	RtPopHandler		;restore caller's runtime error handler
					; (saved on stack by RtPushHandler)
					; (preserves ax)
	xor	ah,ah			;clear ah so ax is condition code
	or	al,al			;set condition codes for caller
cEnd


sEnd	CP



end
