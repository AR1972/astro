	TITLE	txtmove.asm - Contains text manager's text movement functions

;======================================================================
; Module: txtmove.asm - Contains text manager's text movement functions
;
;	--------- --- ---- -- ---------- ----
;	COPYRIGHT (C) 1985 BY MICROSOFT, INC.
;	--------- --- ---- -- ---------- ----
;     
;
;=======================================================================*/

	.xlist
	include version.inc
	TXTMOVE_ASM = ON	;don't include EXTRNs defined in this file
	includeOnce architec
	includeOnce heap
	includeOnce conint			
	includeOnce context
	includeOnce opcontrl
	includeOnce opmin
	includeOnce optables
	includeOnce parser
	includeOnce pcode
	includeOnce rtps
	includeOnce scanner
	includeOnce txtint
	includeOnce txtmgr
	includeOnce util
	.list

assumes	ds,DATA
assumes	ss,DATA

sBegin	DATA

	externB b$errinfo		;defined by runtime - extended Out of
					; memory error code.

sEnd	DATA

sBegin	CP
assumes	cs,CP


;*************************************************************************
; TxtCurInit()
;
; Purpose:
;	Sets the txdCur.xxx fields to reflect a newly initialized text table.
;	It is called by the context manager functions MrsMake, PrsMake.
;	An empty text table is allocated for the text descriptor.
;	If we're creating a PRS and not loading, the text table contains:
;	   opBol opStSub/opStFunc(cntEos,oPrs,oTyp,0)
;	   opBol opStEndProc
;	   opEndProg
;	   opEot
;	else the text table contains
;	   opEndProg
;	   opEot
;
; Entry:
;	Assumes no txdCur is active
;
; Exit:
;	All fields of the global structure 'txdCur' are filled in.
;	If no error occurred, returns Non-Zero else FALSE
;	NOTE: If an error occurs, this function guarentees caller need not
;	deallocate txdCur.bdlText.
;	Condition codes set based on value in ax
;	
;*************************************************************************
cProc	TxtCurInit,<PUBLIC,NODATA,NEAR>,<di>
	localW	tempBuf
cBegin
	and	[grs.GRS_flags],NOT FG_allSsExecute
	call	TxtFlushCache		;can't depend on cached text offsets
	PUSHI	ax,<dataOffset txdCur>
	PUSHI	ax,<SIZE TXD/2>		;bx = #words in txtCur
	call	FillUndef		;fill whole struct with UNDEFINED
	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
	PUSHI	ax,CB_EMPTY_TEXT	;ask for minimal req'd size initially.
					;  Important that we ask for no more,
					;  so that when we discard an existing
					;  mrs, we can always find enough room
					;  to create an empty unnamed mrs again.
	PUSHBDL_TYPE  pgtypEBPcode,ax	; pass sb type for EB version
	call	BdlAlloc
	or	ax,ax
	jne	GotMem
	jmp	short TxtCurExit	;return FALSE if out-of-memory
GotMem:
	mov	[txdCur.TXD_scanState],SS_PARSE
	sub	ax,ax
	mov	[txdCur.TXD_cLines],ax
	mov	[txdCur.TXD_cLinesIncl],ax
	mov	[txdCur.TXD_flags],al
	mov	ax,[grs.GRS_oPrsCur]
	inc	ax			;test for UNDEFINED
	jne	TxtCurPrs		;brif we're creating a prs text table
	or	[txdCur.TXD_flags],FTX_mrs
					;remember that this is a module txt tbl
	test	[mrsCur.MRS_flags2],FM2_NoPcode OR FM2_Include
	je	TxtCurExitGood		;brif not an $INCLUDE or Document mrs
	inc	[txdCur.TXD_scanState]	;convert SS_PARSE to SS_RUDE
	.errnz SS_RUDE - SS_PARSE - 1
	jmp	SHORT TxtCurExitGood

TxtCurPrs:
	;If module is in SS_RUDE state, and we're creating a procedure,
	;put that procedure in SS_RUDE state as well.
	
	mov	al,[mrsCur.MRS_txd.TXD_scanState]
	cmp	al,SS_RUDE
	jb	TxtCurExitGood		; brif module's not in SS_RUDE state
	mov	[txdCur.TXD_scanState],al
TxtCurExitGood:
	call	TxtInitTxtTbl		;set up initial txt table.
DbAssertRel ax,ne,0,CP,<TxtCurInit:Unexpected OM error>
; ax should be non-zero after call to TxtInitTxtTbl
TxtCurExit:
	or	ax,ax			;set condition codes for caller
cEnd

;*************************************************************************
; TxtInitTxtTbl()
;
; Purpose:
;	Added with revision [26].
;	Inits a minium txt table.  Inserts opEndProg and opEot.
; Entry:
;	txdCur defines current text table.
; Exit:
;	AX = 0 if OM. Condition codes set via or ax,ax
;
;*************************************************************************
cProc	TxtInitTxtTbl,<NEAR>,<DI>
cBegin
	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
					;pass adr of bdl for text table
	PUSHI	ax,CB_EMPTY_TEXT	;make sure we have enough free
	call	BdlCheckFree		;ax = result
	or	ax,ax
	je	TITT_Exit		;brif out of memory

;We use GetSeg to get the seg addr of the text table here so that
;sbCur does not need to preserved to create a text table.
	GETSEG	es,[txdCur.TXD_bdlText_seg],,<SIZE,LOAD> ;es points to text tbl
	SetStartOtx di			;otxInsert = start of text
	mov	ax,opEndProg
	stosw
	mov	ax,opEot
	stosw
.erre CB_EMPTY_TEXT - 4 EQ StartOtx	;empty text table has 4 bytes of pcode
	mov	[txdCur.TXD_bdlText_cbLogical],CB_EMPTY_TEXT
.erre	opEot
TITT_Exit:
	or	ax,ax
cEnd

;*************************************************************************
; TxtDelAllFar()
;
; Purpose:
;	Added with revision [26].
;	Deletes all text in the module, leaving a minimal text table.
;	Called by the user interface to remove all text from a text table,
;	leaving opEndProg and opEot.
; Entry:
;	grs.oMrsCur indicates current module
;	grs.oMrsPrs indicates current procedure (UNDEFINED if none)
; Exit:
;	none.
;
;*************************************************************************
PUBLIC	TxtDelAll
TxtDelAll PROC FAR
	call	TxtDeleteAll		;delete all text in the text
					; table except endprog and eot
	call	ChkAllUndefPrsSaveRs	;discard any prs's with last
					;reference deleted by delete
	ret
TxtDelAll ENDP

;*************************************************************************
; TxtDeleteAll()
;
; Purpose:
;	Deletes all text in the module, leaving a minimal text table.
; Entry:
;	grs.oMrsCur indicates current module
;	grs.oMrsPrs indicates current procedure (UNDEFINED if none)
; Exit:
;	none.
;
;*************************************************************************
PUBLIC	TxtDeleteAll
TxtDeleteAll PROC NEAR
	cmp	[txdCur.TXD_bdlText_cbLogical],CB_EMPTY_TEXT
	jbe	EmptyText		;brif text table is empty (TxtLoad
					;    does this for temp text tables)
					;Don't need delete OpEndProg OpEot
	test	[grs.GRS_flagsDir],FDIR_new
	je	NotNewStmt		;brif NewStmt is not active (speed
					; optimization - NEW takes forever
					; if this is not done)
	mov	[prsCur.PRS_otxDef],UNDEFINED
					;if prs is active, this will cause
					;PrsDiscard to call PrsFree
					;if mrs is active, this has no effect
	jmp	SHORT EmptyText

NotNewStmt:
	call	TxtModified		;descan current text table to SS_PARSE
					;this may alter txdCur.bdlText.cbLogical
					;It also sets FM2_Modified
	PUSHI	ax,StartOtx		;delete from start of text table
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	dec	ax
	dec	ax
	push	ax			;entire text table is being discarded
	call	TxtDelete		;ax = return code
					;Note: TxtDelete may cause TxtActivate/
					; TxtDeactivate to be called via
					; ForEachPrsInMrs, but before it
					; returns, it restores whatever text
					; table was active when it was called.

	call	TxtInitTxtTbl		;set up default txt table
DbAssertRel ax,ne,0,CP,<TxtDeleteAll: unexpected OM error>
EmptyText:
	ret
TxtDeleteAll ENDP

;*************************************************************************
; TxtDiscard()
;
; Purpose:
;	Takes any text-mgr action needed before a text table is discarded
;	by the context manager.  This includes prompting the user with
;	"This action will prevent CONT" if appropriate.
;
; Sequence of events for NewStmt, as it affects text tables:
; NewStmt calls MrsDiscard for each mrs in system which has file associated
;    with it (i.e. doesn't call it for command window or scrap mrs's)
; MrsDiscard calls PrsDiscard for each prs which has a text table and
;    whose PRS_oMrs field = mrs.  It then calls TxtDiscard to discard
;    module's text table.
; PrsDiscard calls TxtDiscard to discard the procedure's text table.
;
; Entry:
;	grs.oMrsCur indicates current module
;	grs.oMrsPrs indicates current procedure (UNDEFINED if none)
; Exit:
;	if no error, returns 0
;
;*************************************************************************
PUBLIC	TxtDiscard
TxtDiscard PROC NEAR
	;NOTE - we can't call DbChk TxdCur here because txtload.c calls
	; it for a temporary buffer where it would fail.
	

	call	TxtDeleteAll		;delete all text in text table

; If we are discarding a prs, make sure that the defining ref deleted
; flag gets set.  TxtDelete won't do this if the there are no references
; to the prs, and the prs has a text table.  This could happen if the
; SUB or FUNCTION statment is a reparse, and no other reference exists.

	test	[txdCur.TXD_flags],FTX_Mrs  ; are we discarding an PRS
	jne	MrsTxtTbl		; text table?  brif so
	or	[flagsTM],FTM_PrsDefDeleted 
MrsTxtTbl:
	PUSHI	ax,<dataOFFSET txdCur.TXD_bdlText>
					;pass ptr to bdl structure for txt tbl
	call	BdlFree			;release it (note that this sets
					; bdlText_status to NOT_OWNER in txdCur)
	inc	ax			;return 'no-error' (0) result
	ret
TxtDiscard ENDP

;*************************************************************************
; TxtActivate()
;
; Purpose:
;	Sets the txdCur.xxx fields to reflect the current text table.
;	This involves copying the txd struct from mrsCur or the global Rs table.
;	If grs.oPrsCur is UNDEFINED, or represents a procedure with
;	no text table (i.e. for a DECLARE or DEF FN prs), then the
;	copy is from the mrsCur.txd, else it is from the global Rs table.
;	It is called by the context manager functions MrsActivate & PrsActivate
;	NOTE: Callers can (and do) depend on this function causing
;	      NO heap movement.
;
; Entry:
;	grs.oMrsCur indicates current module
;	grs.oMrsPrs indicates current procedure (UNDEFINED if none)
;
; Exit:
;	All fields of the global structure 'txdCur' are copied from
;	mrsCur or the global Rs table.
;	
;*************************************************************************
PUBLIC	TxtActivate
TxtActivate PROC NEAR
	call	TxtFlushCache		;can't depend on cached text offsets
	sub	al,al			;copy mrs/prs -> txd
	sub	cx,cx			;assume we're activating module txt tbl
	mov	bx,[grs.GRS_oPrsCur]
	inc	bx
	.errnz	UNDEFINED - 0FFFFH
	jz	TxtAct1			;brif no prs is active

	dec	bx
	RS_BASE add,bx			; bx pointer to start of prs in table
	GETRS_SEG es,cx,<SIZE,LOAD>	;[3]
	mov	cx,PTRRS[bx.PRS_txd.TXD_bdlText_status] 
	sub	cx,NOT_OWNER		;cx = 0 if prs doesn't own a text table
	jmp	SHORT TxtAct1
TxtActivate ENDP

;*************************************************************************
; TxtDeactivate()
;
; Purpose:
;	Copies the txdCur.xxx fields to the txd struct in mrsCur or the
;	appropriate place in the global Rs table.
;	If grs.oPrsCur is UNDEFINED, or represents a procedure with
;	no text table (i.e. for a DECLARE or DEF FN prs), then the
;	copy is to the mrsCur.txd, else it is to the global Rs table.
;	It is called by the context manager functions MrsDeactivate &
;	PrsDeactivate.
;	NOTE: Callers can (and do) depend on this function causing
;	      NO heap movement.
;
; Entry:
;	grs.oMrsCur indicates current module
;	grs.oMrsPrs indicates current procedure (UNDEFINED if none)
;
; Exit:
;	All fields of the global structure 'txdCur' are copied to mrsCur
;	or the global Rs table.
;	
;*************************************************************************
PUBLIC	TxtDeactivate
TxtDeactivate PROC NEAR

	DbChk	TxdCur			;perform sanity check on txdCur

	;eliminate any free space at the end of current text table,
	;so if far heap mgr needs space, the only places it needs to
	;look are static structures txdCur and mrsCur.  This is smaller
	;code than having to walk the far heap.  If it becomes a performance
	;bottleneck, we'll have to delete this and walk the far heap.
	
	PUSHI	ax,<DATAOFFSET txdCur.TXD_bdlText>
	call	BdlTrim

	mov	al,1			;tell common code we're deactivating
	sub	cx,cx
	test	[txdCur.TXD_flags],FTX_mrs
	jne	TxtAct1			;brif deactivating module text table
	dec	cx			;we're deactivating prs text table

;This code is shared between TxtActivate and TxtDeactivate
; al = 0 for Activate, non-zero for Deactivate
; cx = 0 if activating/deactivating module text table
;    = non-zero for procedure text table
;
TxtAct1:
assumes ds,nothing			
     DbAssertRel grs.GRS_oMrsCur,ne,UNDEFINED,CP,<Txt[De]Activate mrs=UNDEFINED>
	push	si			;save caller's si/di
	push	di
	push	ss			
	pop	es			; assume DGROUP->DGROUP copy
	mov	di,dataOFFSET mrsCur.MRS_txd  ; di -> current mrs's txd struct
	jcxz	NoPrs			      ;brif text table not for prsCur
	GETRS_SEG es,di,<SPEED,LOAD>	;[3] assume we're deactivating
	mov	di,[grs.GRS_oPrsCur]	
	RS_BASE add,di			
	add	di,PRS_txd		; di -> current prs's txd struct
     DbAssertRel grs.GRS_oPrsCur,ne,UNDEFINED,CP,<Txt[De]Activate prs=UNDEFINED>
NoPrs:
	mov	si,dataOFFSET txdCur	; si -> cur text tbl descriptor
	or	al,al
	jnz	DoTheCopy		; brif we're deactivating
	xchg	si,di			; we're activating
DoTheCopy:
.errnz	(SIZE TXD) MOD 2		;block copy assumes even # of bytes
	mov	cx,(SIZE TXD) SHR 1	;cx = # words to copy

	push	si			;pass src BdlChgOwner
	push	di			;pass dst to BdlChgOwner
	rep movsw			; do the copy
	call	BdlChgOwner		
	pop	di			;restore caller's si/di
	pop	si
assumes ds,DATA 			
	ret
TxtDeactivate ENDP

;*************************************************************************
; TxtMoveUp(otxSrcLow, cbIns)
;
; Purpose:
;	Move a block of text from a low address to a higher address
;	within the current text table.  Typically used to make room
;	for text to be inserted.
;
; Entry:
;	parmW otxSrcLow: offset of source of lowest byte to be moved
;	parmW cbIns: number of bytes to make room for
;	if grs.fDirect is TRUE,
;	   grs.bdlDirect (the direct mode pcode buffer) is the buffer to
;	   be shifted, and grs.bdlDirect.cbLogical = current logical
;	   size of text table
;	else txdCur.bdlText is the buffer to be shifted and
;	   txdCur.bdlText.cbLogical = current logical size of text table
;
; Exit:
;	returns ax = zero if out-of-memory, else non-zero
;	condition codes set based on value in ax
;	if grs.fDirect is TRUE
;	   grs.bdlDirect.cbLogical is updated for the move
;	else
;	   txdCur.bdlText.cbLogical is updated for the move
;
; Example:
;   Before:
;	high memory 	
;	  txdCur.TXD_bdlText_cbLogical->+-----+
;					|  E  |
;	  otxSrcLow + cbIns------------>|  D  |
;					|  C  |
;	  otxSrcLow-------------------->|  B  |
;					|  A  |
;	low memory			+-----+
;
;   After:
;	high memory 	
;	  txdCur.TXD_bdlText_cbLogical->+-----+
;					|  E  |
;					|  D  |
;					|  C  |
;	  otxSrcLow + cbIns------------>|  B  |
;					|  -  |
;	  otxSrcLow-------------------->|  -  |
;					|  A  |
;	low memory			+-----+
;
;*************************************************************************
cProc	TxtMoveUp,<PUBLIC,NODATA,NEAR>,<si,di>
	parmW otxSrcLow
	parmW cbIns
cBegin	TxtMoveUp
	DbChk	Otx,otxSrcLow		;assert otxSrcLow is valid
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	si,[otxSrcLow]		;si = source offset
					; (is parm for TxtAdjust)
	call	TxtChkCache		;adjust/reset cached text offsets
					;reset History and Watch info too
	push	[cbIns]
	call	TxtFree			;make sure we have enough free space
	je	TxtMoveUpExit		;brif out-of-memory
	GetSegTxtCur			;es = seg adr of current txt tbl
					; (this may be direct mode buffer)
	mov	dx,[cbIns]		;dx = #bytes to insert
	mov	bx,dataOFFSET grs.GRS_bdlDirect_cbLogical
	cmp	[grs.GRS_fDirect],FALSE
	jne	Up_InDirMode		;branch if in direct mode
	mov	bx,dataOFFSET txdCur.TXD_bdlText_cbLogical
Up_InDirMode:
	mov	di,si			;di = source offset
	add	di,dx			;di = destination offset
	mov	cx,[bx]			;cx = current size of text table
	add	[bx],dx			;update cbLogical for move
	sub	cx,si			;cx = # bytes to copy
	add	di,cx			;start copying at top of block
	dec	di			; so we don't overwrite end of
	dec	di			; source before we've copied it
	add	si,cx
	dec	si
	dec	si
	shr	cx,1			;we will move words
DbAssertFlags	nc,CP,<TxtMoveUp: err1>
	push	ds			;save caller's ds
	push	es			;push seg adr of current txt tbl
	pop	ds			;set up source seg
	std				;copy moving down
	rep movsw
	cld				;always leave D flag clear by convention
	pop	ds			;restore caller's ds
	mov	ax,sp			;success result
TxtMoveUpExit:
	or	ax,ax			;set condition codes for caller
cEnd	TxtMoveUp

cProc	TxtMoveUpFar,<PUBLIC,NODATA,FAR> 
	parmW otxSrcLow 		
	parmW cbIns			
cBegin	TxtMoveUpFar			
	cCall	TxtMoveUp,<otxSrcLow, cbIns>  
cEnd	TxtMoveUpFar			

;*************************************************************************
; TxtMoveDown(otxSrcLow, otxDstLow)
;
; Purpose:
;	Move a block of text from a high address to a lower address
;	within the current text table.  Typically used to delete text.
;
; Entry:
;	parmW otxSrcLow: offset of source of lowest byte to be moved
;	parmW otxDstLow: offset of destination of lowest byte to be moved
;	if grs.fDirect is TRUE,
;	   grs.bdlDirect (the direct mode pcode buffer) is the buffer to
;	   be shifted, and grs.bdlDirect.cbLogical = current logical
;	   size of text table
;	else txdCur.bdlText is the buffer to be shifted and
;	   txdCur.bdlText.cbLogical = current logical size of text table
;
; Exit:
;	if grs.fDirect is TRUE
;	   grs.bdlDirect.cbLogical is updated for the move
;	else
;	   txdCur.bdlText.cbLogical is updated for the move
;
; Example:
;   Before:
;	high memory 	
;	  txdCur.TXD_bdlText_cbLogical->+-----+
;					|  F  |
;					|  E  |
;	  otxSrcLow-------------------->|  D  |
;					|  C  |
;	  otxDstLow-------------------->|  B  |
;					|  A  |
;	low memory			+-----+
;
;   After:
;	high memory 	
;	  txdCur.TXD_bdlText_cbLogical->+-----+
;	  otxSrcLow-------------------->|  F  |
;					|  E  |
;	  otxDstLow-------------------->|  D  |
;					|  A  |
;	low memory			+-----+
;
;*************************************************************************
cProc	TxtMoveDown,<PUBLIC,NODATA,NEAR>,<si,di>
	parmW otxSrcLow
	parmW otxDstLow
cBegin	TxtMoveDown
	DbChk	Otx,otxDstLow		
	DbChk	Otx,otxSrcLow		
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	si,[otxDstLow]		;si = destination offset
					; (is parm for TxtAdjust)
	call	TxtChkCache		;adjust/reset cached text offsets
					;reset History and Watch info too
	GetSegTxtCur			;es = seg adr of cur txt tbl
	mov	di,si			;di = destination offset
	mov	si,[otxSrcLow]		;si = source offset
	mov	dx,si			;dx = source offset
	sub	dx,di			;dx = number of bytes being deleted
	mov	bx,dataOFFSET grs.GRS_bdlDirect_cbLogical
	cmp	[grs.GRS_fDirect],FALSE
	jne	Dn_InDirMode		;branch if in direct mode
	mov	bx,dataOFFSET txdCur.TXD_bdlText_cbLogical
Dn_InDirMode:
	mov	cx,[bx]			;cx = current size of text table
	sub	[bx],dx			;update cbLogical for move
	sub	cx,si			;cx = # bytes to copy
	shr	cx,1			;move words instead of bytes
DbAssertFlags nc,CP,<TxtMoveDown: err1>
	push	ds			;save caller's ds
	push	es			;push seg adr of current text tbl
	pop	ds			;set up source seg
	rep movsw			;copy block
	pop	ds			;restore caller's ds
cEnd	TxtMoveDown

;*************************************************************************
; TxtFree
;
; Purpose:
;	Ensure that the current text table's cbPhysical exceeds its cbLogical
;	by a given value, i.e. guarentee a given amount of free space.
; Entry:
;	parm1: ushort cbFree
;	if grs.fDirect is TRUE,
;	   table grs.bdlDirect is checked
;	else
;	   table txdCur.bdlText is checked
; Exit:
;	returns ax = zero if out-of-memory, else non-zero
;	condition codes set based on value in ax
;
;*************************************************************************
cProc	TxtFree,<PUBLIC,NODATA,NEAR>
	parmW	cbFree
	localW	fDirectMode
cBegin	TxtFree
	DbChk	TxdCur			;perform sanity check on txdCur
	mov	[fDirectMode],0 	;default not directmode buffer
	mov	ax,dataOFFSET txdCur.TXD_bdlText
	cmp	[grs.GRS_fDirect],FALSE
	je	NotDirMode		;branch if not in direct mode
	mov	ax,dataOFFSET grs.GRS_bdlDirect
	mov	[fDirectMode],sp
NotDirMode:
	push	ax			;pass adr of bdl for text table
	push	cbFree
	call	BdlCheckFree		;ax = result
	or	ax,ax			;set condition codes for caller
	jne	TxtFreeExit		;brif not out of memory

; We have an out of memory error.  Determine what caused the error so
; we can give the user a better idea of how to resolve the problem.
; If the we attempted to grow the text table beyond 64k then we will
; set the variable b$errinfo to one of the following values:
;	Not changed - Was direct mode buffer, or legit out of memory.
;	OMErr_Mod   - Module level code blew past 64k.
;	OMErr_Proc  - Procedure level code blew past 64k.
;	OMErr_Inc   - Include file blew past 64k.
;	OMErr_Doc   - Document blew past 64k.

	cmp	[fDirectMode],0 	;was it the direct mode buffer?
	jne	TxtFreeOmExit		;brif so, standard OM error
	mov	ax,[txdCur.TXD_bdlText_cbLogical] ;get current size of text table
	add	ax,[cbFree]		;add requested increment
	jnc	TxtFreeOMExit		;brif request wasn't > 64K

.errnz	OMErr_Mod-OMErr_Proc-1
.errnz	OMErr_Inc-OMErr_Mod-1
.errnz	OMErr_Doc-OMErr_Inc-1
	mov	cx,OMErr_Proc		;assume we are in a SUB/FUNC
	test	[txdCur.TXD_flags],FTX_mrs ;is this a SUB/FUNC?
	je	TxtFreeOMSet		;brif so, set b$errinfo
	mov	al,[mrsCur.MRS_flags2]
	inc	cx			;assume in a MODULE
	test	al,FM2_NoPcode OR FM2_Include ;is this an Include/Document?
	je	TxtFreeOMSet		;brif not, module text table
	inc	cx			;assume an INCLUDE file
	test	al,FM2_Include		;is this an include?
	jne	TxtFreeOMSet		;brif so
	inc	cx			;must be a document
DbAssertTst al,ne,FM2_NoPcode,CP,<TxtFree: Expected a Document txt table>

TxtFreeOMSet:
	mov	[b$errinfo],cl 	;set extended OM error code

TxtFreeOMExit:
	sub	ax,ax			;set ax and condition codes for caller
TxtFreeExit:
cEnd	TxtFree

cProc	TxtFreeFar,<PUBLIC,NODATA,FAR>	
	parmW	cbFree			
cBegin	TxtFreeFar			
	cCall	TxtFree,<cbFree>	
cEnd	TxtFreeFar			

;*************************************************************************
; TxtPrsInit
; Purpose:
;	This is called by User Interface code after File/New/Sub
;	to put the following pcode into the newly created text table:
;	   DEFxxx state from the end of the module so user who has
;	   DEFINT A-Z in module will get it in SUB as well.
;	   SUB/FUNCTION name
;	   END SUB/FUNCTION
; Entry:
;	prs in question is active
;	otype - If proc is a FUNCTION then otype is its return type
; Exit:
;	No error return.  If out of memory, the user just gets an
;	empty text table.
;
;*************************************************************************
cProc	TxtPrsInit,<PUBLIC,FAR>,<si,di>
	parmB	oType				
cBegin
	;Following is guarenteed by by PrsMake()
	DbAssertRel [grs.GRS_oMrsCur],e,[prsCur.PRS_oMrs],CP,<TxtPrsInit: err2>
	DbAssertTst [txdCur.TXD_flags],Z,FTX_mrs,CP,<TxtPrsInit: got an mrs>

	and	[prsCur.PRS_flags],NOT FP_STATIC
					;in case this prs used to have a
					; txt tbl, with FP_STATIC bit set,
					; and then prs was discarded.  FP_STATIC
					; would still be set.
	SetStartOtx di			;otxInsert = 0
	push	di			;pass otxInsert
	PUSHI	ax,16d			;we're about to insert 16 bytes
	call	TxtMoveUp		;make room for pcode to be inserted
	je	TxtPExit		;brif out-of-memory

	GetSegTxtTblCur	es		;es = seg adr of txt tbl
	sub	ax,ax			;ax = opBol
	.errnz	opBol
	stosw				;emit opBol
	mov	ax,opStSub
	cmp	[prsCur.PRS_procType],PT_SUB
	je	TxtSub
	mov	ax,opStFunction
TxtSub:
	stosw				;emit opStSub/opStFunction
	mov	ax,6
	stosw				;emit cntEos operand
	mov	ax,[grs.GRS_oPrsCur]
	stosw				;emit oPrs operand
.errnz	ET_IMP
	sub	ax,ax			;al = ET_IMP
	mov	ah,[prsCur.PRS_procType];ah = PT_SUB or PT_FUNCTION
	cmp	ah, PT_FUNCTION 	
	jne	@F			
	mov	al, [otype]		
	.errnz	ET_IMP			
	or	al,al			;ET_IMP?
	jz	@F			;brif yes.
	or	ax, DCLA_Explicit	
@@:					
	stosw				;emit oTypFn operand
	sub	ax,ax
	stosw				;emit parm count operand (0)
.errnz	opBol				;ax = 0 = opBol
	stosw				;emit opBol
	mov	ax,opStEndProc
	stosw
	or	[mrsCur.MRS_flags2],FM2_Modified
	push	[grs.GRS_oPrsCur]	;save caller's oPrs
	call	PrsDeactivate
	call	OtxDefTypeEot		;fill ps.tEtCur with module's def state
	call	PrsActivateCP		;reactivate caller's prs

	;init default ps.tEtCur[] (default type array)
	SetStartOtx ax			;ax = start of text
	mov	bx,dataOFFSET tEtTemp
	call	OtxDefType		;fill tEtTemp with default types
					; i.e. ET_R4 for all letters

	PUSHI	ax,<DATAOFFSET tEtTemp>
	PUSHI	ax,<DATAOFFSET ps.PS_tEtCur>
	SetStartOtx ax			;ax = otxInsert = start of text
	call	InsertEtDiff
	SetStartOtx si			;otx for inserted pcode
	mov	bx,[txdCur.TXD_bdlText_cbLogical]
	sub	bx,CB_EMPTY_TEXT-StartOtx ;otx beyond end of inserted pcode
	call	TxtInsUpdate		;update any static structures
					; as a result of inserting this pcode.
TxtPExit:
cEnd

;*************************************************************************
; GetWOtx
;
; Purpose:
;	Given an  offset within the current text table, return
;	the 16 bit value stored at that offset.
; Entry:
;	ax = otx
; Exit:
;	ax = 16 bit value from text table
;
;*************************************************************************
PUBLIC	GetWOtx
GetWOtx	PROC NEAR
	DbChk	Otx,ax			;check for invalid text offset
	;NOTE - we can't call DbChkTxdCur here because DebChkTxdCur
	; calls GetWOtx, which causes infinite recursion.
	

DbAssertTst [conFlags],nz,F_CON_StaticStructs,CP,<GetWOtx: static structs not enabled>

	GetSegTxtTblCur			;es = seg addr of current text table
	xchg	bx,ax			;bx = offset into text table
	mov	ax,es:[bx]		;ax = value at that offset
	ret
GetWOtx	ENDP

;*************************************************************************
; PutWOtx
;
; Purpose:
;	Store a 16 bit value at a given offset within the current text table
; Entry:
;	parm1: ushort otx
;	parm2: ushort value
;
;*************************************************************************
cProc	PutWOtx,<PUBLIC,NODATA,NEAR>
	parmW	otx
	parmW	value
cBegin	PutWOtx
	DbChk	Otx,otx			;check for invalid text offset
	DbChk	TxdCur			;perform sanity check on txdCur
	GetSegTxtTblCur es		;es = seg adr of txt tbl
	mov	bx,[otx]		;bx = offset into text table
	mov	ax,[value]		;ax = value to store at that offset
	mov	es:[bx],ax		;store it
cEnd	PutWOtx


;-----------------------------------------
;	Non-RELEASE CP segment Functions
;-----------------------------------------
sEnd	CP



end
