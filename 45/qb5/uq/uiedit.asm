	TITLE	uiedit.asm - Misc user interface functions.
;*** 
;uiedit.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Interface between CW and parsing code.
;
;
;*******************************************************************************

	.xlist
	include version.inc
	UIEDIT_ASM = ON	
	CONTEXT_NOFUNCS = ON

	include cw/version.inc
	include cw/windows.inc
	include cw/edityp.inc

	IncludeOnce architec
	IncludeOnce context
	IncludeOnce lister
	IncludeOnce heap
	IncludeOnce parser
	IncludeOnce pcode
	IncludeOnce qbimsgs
	IncludeOnce rtps
	IncludeOnce txtmgr
	IncludeOnce ui
	IncludeOnce uiint
	IncludeOnce uinhelp	
	IncludeOnce util
	IncludeOnce edit	

	.list

assumes	DS,DATA
assumes	SS,DATA
assumes	ES,NOTHING


;-------------------------------------------------------------------------
;		DATA Segment Declarations
;-------------------------------------------------------------------------
sBegin	DATA

; oRs of 'command window'
	globalW	hbufOfCmd,UNDEFINED

; oRs of 'scrap' used as temporary buffer
; for Cut/Copy/Paste operations
	globalW	hbufOfScrap,UNDEFINED

; since we list one line after another when refreshing the content
; of a window, these static variables are used to remember where the
; pcode offset for the next line to be listed.  It saves us from
; having to search pcode for the nth line.  It must be discarded
; whenever text changes, or another text table is activated.
	staticW	otxLnLast,0
	staticW	otxGetNext,0
	staticW	lnGetNext,0
	staticW	saveLnGetNext,0
	staticW	saveOtxGetNext,0
	staticW	saveRsCur,UNDEFINED

; The number of the last line entered in the command windows is kept here.
; For error reporting.
	staticW	lnCmdLast,0

; Set to TRUE when a command in the parse buffer should be executed.
	globalW	fDoCmd,0

; Set to TRUE when we have an error that may need the box to be repositioned
; depending upon where the cursor position is (see uimsgbox.c)
	globalB fAdjustMsgBox,0

;cbGetLineBuf sets the following two words for LineAttr
	staticW	colStmtStart,0
	staticW	colStmtEnd,0
	staticB fWndHelp,0

; rgLineAttr is used in GetLineAttrs
	public rgLineAttr
rgLineAttr	label	word
rgLineAttr0	db	size LINEATTR dup(?)
rgLineAttr1	db	size LINEATTR dup(?)
rgLineAttr2	db	size LINEATTR dup(?)
rgLineAttr3	db	size LINEATTR dup(?)


;BOOKMARKS:
;	Each bookmark is specified in the following structure.	Different
;	fields are UNDEFINED (-1) to indicate different conditions or
;	situation with the bookmark
;
;	if BM_oRs == UNDEFINED
;	    bookmark is invalid
;
;	if BM_Nc == NOT_HELPTOPIC
;	    BM_oRs = register set for non-help window
;	    BM_oln = line number in the window
;	    BM_col = column number in the window
;	else if BM_oln == UNDEFINED && FV_VARHELP
;	    BM_oRs = register set of variable help item
;	    BM_Nc  = oName of variable help item
;	    BM_col = column number in the window
;	else
;	    BM_Nc:BM_oRs = context number of help item
;	    BM_oln = line number in the window
;	    BM_col = column number in the window
;
;NOTE:	it is Nc:oRs for help engine help but  oRs:Nc for Variable help
;	(so that we can deallocate based on the oRs).

BookMark struc
BM_Nc		dw   UNDEFINED	; oRs for non-help window.  Otherwise,
BM_oRs		dw   UNDEFINED
BM_oln		dw	0
BM_col		dw	0
BookMark ends

.errnz	(-1) - (UNDEFINED)

NOT_HELPTOPIC	equ	UNDEFINED
MAXBOOKMARK	equ	4

BookMarks	BookMark	MAXBOOKMARK dup(<>)

psSaveLen	dw	0
psSaveChar	db	0
;
; ldEMScratch is used by TWIN's EditMgr.
; Since we want is to be a movable heap entry, the structure overlaps a bd.
;
	public	ldEMScratch
	public	bdEMScratch
	.errnz	(size ld) - 8
	.errnz	(size bd) - 6
ldEMScratch	dw	0	; ld.flags           ---+
		dw	0	; ld.cb                 |--------- ld structure
bdEMScratch	dw	0	; ld.cbMax/bd.cbLogical | ----+
		dw	0	; ld.prgch/bd.pb     ---+     |--- bd Structure
		dw	0	; bd.cbPhysical      ---------+

	;next 4 locations are used to construct special dialog for
	;runtime errors in compiled code (in ReportError()).
	
	EXTRN	b$fCompErr:word	;non-zero if error was in compiled code
	EXTRN	b$ErrLin:word		;line # of last runtime error (ERL)
	EXTRN	b$ErrMod:dword		;far ptr to 0-terminated module name
	EXTRN	b$ErrAdr:dword		;far ptr where runtime call was made
	EXTRN	b$ErrInfo:byte		;extended error code

	EXTRN	iHelpId:word	
	externB HelpFlags	

sEnd	DATA


sBegin UI
assumes CS,UI

	externNP WndHelpOpen		; Open help window to a given size
	externNP DisplayHelpOOM 	

;Extended out-of-memory error messages.
; Moved FAR to save DGROUP
twExtOmErr LABEL WORD
	dw	ER_OM				;"Out of memory"
	dw	MSG_OutNearMem			;"Out of data space"
	dw	ER_OM				;"Out of memory"
	dw	MSG_OutOfStack			;"Out of stack space"
	dw	MSG_ProcTooLarge		;"Procedure too large"
	dw	MSG_ModTooLarge			;"Module level code too large"
	dw	MSG_InclTooLarge		;"Include file too large"
	dw	MSG_DocTooLarge			;"Document too large"
	.errnz	OMErr_OM  -0	;generic OM error
	.errnz	OMErr_NH  -1	;out of near heap space ( DS > 64k )
	.errnz	OMErr_FH  -2	;out or far heap space	( out of system memory )
	.errnz	OMErr_STK -3	;out of stack space
	.errnz	OMErr_Proc-4	;out of Procedure text table space ( > 64k )
	.errnz	OMErr_Mod -5	;out of Module text table space ( > 64k )
	.errnz	OMErr_Inc -6	;out of Include file text table space ( > 64k )
	.errnz	OMErr_Doc -7	;out of Document file text table space ( > 64k )

;**************************************************************************
; EditMgrFlush
; Purpose:
;	Flush the current line out of the editor's internal buffer if
;	it is dirty.
;	Since replacing the line could cause a procedure to be renamed
;	which gives it a different oRs, this routine does not preserve
;	grs.oRsCur (and the caller shouldn't either).  The reason
;	the oRs of the procedure needs to change is because there
;	may be references to the old prs in pcode, so the old prs has
;	to stay.
; EditMgrFlush1 has same entry conditions and purpose, only it
;	flushes the edit mgr's buffer even if edit mgr's buffer isn't dirty.
;	This is used when:
;	 [1] window allocation changes on the screen, so the edit mgr
;	     forces the cursor within the current window.
;	 [2] when QB is going to use the edit mgr's buffer for its own
;	     purposes, so it doesn't want subsequent edit mgr calls
;	     to think what it has cached in this buffer represents its
;	     current line.
; Exit:
;	ReplaceLineBuf will be called if line was dirty, which can
;	   modify ps.bdpSrc among other things...
;	Active window's oRs is active.
;
;**************************************************************************
PUBLIC	EditMgrFlush, EditMgrFlush1
EditMgrFlush1 PROC NEAR
	sub	cx,cx
	SKIP2_PSW
EditMgrFlush1 ENDP
EditMgrFlush PROC NEAR
	mov	cl,1

	DbAssertRel [pwndAct],ne,0,UI,<EditMgrFlush: pwndAct=NULL> 

; brif we're to do it even if no dirty buffer (it causes editmgr
; to force the cursor within the current window).
	jcxz	EfFlushFocus

	;While edit mgr is active, it has local copy of ldFlags.
	DbAssertTst [emFlags],e,EMF_IN_EDITMGR,UI,<EditMgrFlush: EditMgr active>

	mov	bx,[pwndAct]		
	mov	bx,[bx.pefExtra]	
	mov	bx,[bx.EF_pldCur]
	test	[bx.LD_flags],LD_fDirty	;test fDirty flag in window's structure
	je	EfExit			;brif not dirty (speed optimization)
EfFlushFocus:
	cCall	FlushFocus
SkipFocus:
	cCall	UiRsActivateWnd 	;activate active window's oRs
EfExit:
	ret
EditMgrFlush ENDP

cProc	FlushFocus,<NEAR>,<si>
cBegin
	mov	si,[pwndAct]
	mov	bx,WM_FLUSHFOCUS	; New EditMgr message
	sub	cx,cx
	cCall	SendMessage,<si,bx,cx,cx,cx>
cEnd

;**************************************************************************
; void FAR UiFlushCache()
; Purpose:
;  Called by text manager whenever text is changed, or cached text offsets
;  become invalid for any other reason.
;  Also called every time a context change occurs.
;  It resets any variables which speed performance by assuming we're
;  in the same context we were the last time the variables were set.
;  We flush our cache so we don't look at invalid data.
;
; Entry:
;  none
;
; Exit:
;  lnGetNext = 0
;
;**************************************************************************
cProc	UiFlushCache,<PUBLIC,FAR>
cBegin
	mov	[lnGetNext],0		;Clear cache
cEnd

;**************************************************************************
; void KludgePs()
; Purpose:
;  Null terminates ps.bdpSrc.pb and saves info to restore it
;  later via UnKludgePs.
;
; Entry:
;   [ps.bdpSrc.pb]
;   cx - length of string at ps.bdpSrc.pb
; Exit:
;   [psSaveLen]
;   [psSaveChar]
;
;**************************************************************************
cProc KludgePs,<NEAR>
cBegin
	mov	bx,[ps.PS_bdpSrc.BDP_pb]
	add	bx,cx
	xor	al,al
	xchg	al,[bx]
	mov	[psSaveChar],al
	mov	[psSaveLen],cx
cEnd

;**************************************************************************
; void UnKludgePs()
; Purpose:
;  Reverses effects of KludgePs.
;
; Entry:
;   [ps.bdpSrc.pb]
;   [psSaveLen]
;   [psSaveChar]
; Exit:
;   ps.bdpSrc.pb is restored to state before KludgePs
;
;**************************************************************************
cProc UnKludgePs,<NEAR>
cBegin
	mov	bx,[ps.PS_bdpSrc.BDP_pb]
	add	bx,[psSaveLen]
	mov	al,[psSaveChar]
	mov	[bx],al
cEnd

;**************************************************************************
; void near UpdAltListWnd(ax=lnTop, dx=delta)
; Purpose:
;	Update the alternate-list-window if 2 list windows are active
;	and both refering to the same text table.
; Entry:
;	ax=lnTop is the top line affected by the edit
;	dx=delta is the number of lines inserted or deleted
;	For example: inserting 1 line after line 0: lnTop=0, delta=1
;	             deleting 1 line after line 0: lnTop=0, delta=-1
;	             replacing line 5: lnTop=5, delta=0
;
;**************************************************************************
DbPub UpdAltListWnd
cProc	UpdAltListWnd,<NEAR>
cBegin
	cmp	[fWndSplit], 0		
	je	UAltExit		
	mov	bx, DATAOffset wnd1	
	cmp	bx, [pwndAct]		
	jne	@f			
	mov	bx, DATAOffset wnd2	
@@:					
	mov	bx,[bx.pefExtra]	
	mov	cx,[grs.GRS_oRsCur]
	cmp	cx,[bx.EF_hBuffer]
	jne	UAltExit		;brif both don't refer to same txt tbl
	or	dx,dx
	jns	NotDel			;brif lines are not being deleted
	mov	cx,ax			;cx = lnTop
	sub	cx,dx			;cx = 1st line above deleted lines
	cmp	cx,[bx.EF_pdCur.pd_olntop]
	jbe	NotDel			;brif entire delete is below window top
	mov	dx,ax			;cx = lnTop
	sub	dx,[bx.EF_pdCur.pd_olntop]
					;dx = -#lines deleted below window top
NotDel:
	cmp	ax,[bx.EF_pdCur.pd_olntop]
	jae	UAltNoChg		;brif top line is unaffected by edit
	add	[bx.EF_pdCur.pd_olntop],dx
UAltNoChg:
	call	DrawDebugScr		;sledge hammer ok for uncommon state
UAltExit:
cEnd

;**************************************************************************
; void near ActivateHbuf
; Purpose:
;  Tell the context manager to make the module/procedure associated with
;  the list window identified by hbuf active.
;
; Algorithm:
;	If hbuf is not already active
;	   if hbuf is for scrap
;	      saved = current
;	   else if hbuf <> saved
;	      saved = undefined
;	   Activate hbuf
;	   if hbuf = saved
;	      current = saved
;	      saved = undefined
; Entry:
;  BX = hbuf - is the handle for a text table.
;       It is really an oRs (see context.h) for the text table .
;
; Exit:
;  The text table associated with hbuf is activated.
;
;**************************************************************************
cProc	ActivateHbuf,<NEAR>
cBegin
	DbAssertRel bx,ne,UNDEFINED,UI,<ActivateHbuf(UNDEFINED)>
	mov	[grs.GRS_fDirect],FALSE
	cmp	bx,[grs.GRS_oRsCur]
	je	AHB_End 		;brif hbuf is already active
					;This optimization is important,
					;because UiRsActivate calls UiFlushCache
					;which zeros lnGetNext, which is used
					;to speed up cbGetLine.

	;Now we do a speed optimization for CUT and COPY so cbGetLineBuf
	;can be fast even though we are swapping a user's and scrap's text
	;tables on a per-line basis.
	
	cmp	bx,[hbufOfScrap]
	jne	NotScrap1		;brif not activating the scrap
	mov	ax,[lnGetNext]		;save cached variables used to
	mov	[saveLnGetNext],ax	; make cbGetLineBuf fast
	mov	ax,[otxGetNext]
	mov	[saveOtxGetNext],ax
	mov	ax,[grs.GRS_oRsCur]
	mov	[saveRsCur],ax
	jmp	SHORT NotScrap2

NotScrap1:
	cmp	bx,[saveRsCur]
	je	NotScrap2		;brif activating rs other than scrap
	mov	[saveRsCur],UNDEFINED	; or saveRsCur - end of Cut/Copy/Paste
NotScrap2:
	cCall	UiRsActivate,<bx>
	mov	ax,[grs.GRS_oRsCur]
	cmp	ax,[saveRsCur]
	jne	NotScrap3		;brif not activating rs which was active
					; before scrap was last activated.
	mov	ax,[saveLnGetNext]	;restore cached variables that make
	mov	[lnGetNext],ax		; cbGetLineBuf fast
	mov	ax,[saveOtxGetNext]
	mov	[otxGetNext],ax
	mov	[saveRsCur],UNDEFINED
NotScrap3:
AHB_End:
cEnd

;**************************************************************************
; ModifyingHbuf
; Purpose:
;	Same as ActivateHbuf but also does TxtModified
;
; Entry:
;	BX = hbuf - is the handle for a list window (an oRs).
;
; Exit:
;	The text table associated with hbuf is activated and descanned.
;
;**************************************************************************
ModifyAndGrab PROC NEAR
	cmp	bx,[hbufOfCmd]
	je	ModifyingHbuf		;brif edit is in Command Window
					; user should be able to type
					; CLEAR even if he's low on memory
	push	bx
	call	UiGrabSpace		;don't let user enter such a long
					; program that he can't even execute
					; a CLEAR statement.
	pop	bx
ModifyAndGrab ENDP
	;fall into ModifyingHbuf
ModifyingHbuf PROC NEAR
	call	ActivateHbuf		;activate oRs [bx]
	cCall	TxtModified		;remember to save this file
MhExit:
	ret
ModifyingHbuf ENDP

;**************************************************************************
;UnGrab
;Purpose:
;	Release space allocated by ModifyAndGrab
;Entry:
;	ax = hbuf (space not released if hbufOfCmd)
;
;**************************************************************************
UnGrab	PROC NEAR
	cmp	ax,[hbufOfCmd]
	je	NotGrabbed		;brif edit is in Command Window
	call	UiReleaseSpace
NotGrabbed:
	ret
UnGrab	ENDP

; Added with [51]
;***
;fDocumentBuf - Is current text table a document buffer
;
;Purpose:
;	Deturmines if the currently activated text table should be
;	manipulated as a document buffer.  This should be done only
;	if we are started with the /EDITOR switch and this is not
;	a PCODE text table.
;
;	NOTE: we can not assert that all items that we are editing with
;	/EDITOR are document tables, as we will initialize with a
;	pCode table.
;
;Entry:
;	None.
;
;Exit:
;	psw.Z set if not a document
;
;Uses:
;	None.
;****
cProc	fDocumentBuf,<NEAR>
cBegin

	test	[cmdSwitches],CMD_SW_ED ;Started with /editor?
	jz	NotDocument		;no, can't be a large buffer
	test	[mrsCur.MRS_flags2],FM2_NoPcode ; Is it a document?
NotDocument:
cEnd

; Added with [51]
;****
;fEditorActive : test wether we were started with /editor
;
;Purpose:
;	Exports the checking for the /EDITOR switch to the components
;	of the product that do not have access to cmdSwitches.
;
;Entry:
;	None.
;
;Exit:
;	psw.z set if not started with /EDITOR  (jnz EditorActive)
;****
cProc	fEditorActive,<FAR, PUBLIC>
cBegin
	test	[cmdSwitches],CMD_SW_ED ;Started with /editor
cEnd


;****
;fQhelpActive : test wether we were started with /QHELP
;
;Purpose:
;	Exports the checking for the /QHELP switch to the components
;	of the product that do not have access to cmdSwitches.
;
;Entry:
;	None.
;
;Exit:
;	psw.z set if not started with /QHELP  (jnz QhelpActive)
;****
cProc	fQhelpActive,<FAR, PUBLIC>
cBegin
	test	[cmdSwitches],CMD_SW_QHELP ;Started with /QHELP
cEnd


;**************************************************************************
; DeleteLinesBuf - Delete a range of lines from the buffer
; Purpose:
;  Called by  TextWin's editor  when it needs to delete a
;  range of source lines.
;  cln lines are deleted from buffer hbuf starting at line ln.
;
; Entry:
;  hbuf  - buffer to delete lines from.
;  ln    - line number to delete.
;  cln   - Number of lines to delete.
;
; Exit:
;
;**************************************************************************
cProc	DeleteLinesBuf,<PUBLIC,FAR>,<si>
	parmW	hbuf
	parmW	ln
	parmW	cln
cBegin
	mov	bx,[hbuf]
	call	ModifyingHbuf

	call	fDocumentBuf		; Is this a large document buffer?
	jz	DLB_NotDocument 	; brif not, let txtmgr handle it
	push	[mrsCur.MRS_pDocumentBuf] 
	push	ln			
	push	cln			
	call	S_DeleteLinesBuf	; Delete line from document buffer
	jmp	SHORT DLB_Exit		

DLB_NotDocument:
;*** Don't need to do this because TxtChange calls UiFlushCache.
;***	mov	[lnGetNext],0		; clear cache

; TxtChange(OtxOfLn(ln),OtxOfLn(ln+cln),fNoInsert)

	mov	si,[ln]
	cCall	OtxOfLn,<si>
	push	ax			; 1st parm for TxtChange (otxStart)

	add	si,[cln]
	cCall	OtxOfLn,<si>
	push	ax			; 2nd parm for TxtChange (otxEnd)

	push	sp			; 3rd parm for TxtChange (fNoInsert)

	call	TxtChange		; just delete, don't insert anything

	mov	ax,[ln]			; pass line# above edit in ax
	mov	dx,[cln]
	neg	dx			; pass -#lines deleted in dx
	call	UpdAltListWnd

DLB_Exit:
cEnd

;**************************************************************************
; ReplaceLineBuf(hbuf, ln, cbNewSrc, pbNewSrc)
; Purpose:
;  Replace the line ln in the buffer hbuf with the text pbNewSrc.
;
; Entry:
;  hbuf - the buffer containing the line to be replaced.
;  ln   - the line to replace.
;  cbNewSrc - the length of the new line.
;  pbNewSrc - the text of the new line.
;
; Exit:
;  uierr is set non-zero if any error occurred.
;  returns zero if we changed hBufs (ie - line was a SUB or FUNCTION
;	definition). This tells the editmgr to stop any futher edits
;	to the entry hBuf (keeps us from inserting a BOL for a CR at
;	a random location in the text table.
;
;**************************************************************************
cProc	ReplaceLineBuf,<PUBLIC,FAR>,<si,di>
	parmW	hbuf
	parmW	ln
	parmW	cbNewSrc
	parmDP	pbNewSrc
	LocalW	cbTextOld
cBegin
DbAssertRel [pbNewSrc],e,[ps.PS_bdpSrc.BDP_pb],UI,<ReplaceLineBuf: bad pbNewSrc>
	mov	si,[ln]

	mov	bx,[hbuf]
	call	ModifyAndGrab

	call	fDocumentBuf		; Is this a large doc buffer?
	jz	RLB_NotDocument 	; brif not, let txtmgr handle it
	push	[mrsCur.MRS_pDocumentBuf] 
	push	ln			
	push	cbNewSrc		
	PUSHI	ax,<dataOffset ps.PS_bdpSrc.BDP_pb> ; Ptr to ptr to buffer
	call	S_ReplaceLineBuf	; Replace line in document buffer
	jmp	SHORT RLB_UnGrab	

RLB_NotDocument:
	mov	cx,[cbNewSrc]
	cCall	KludgePs

;*** Don't need to do this because TxtChange calls UiFlushCache.
;***	mov	[lnGetNext],0		; clear cache

	cCall	OtxOfLn,<si>		; Get otx of begining of line
	xchg	di,ax			; mov di,ax

	inc	si
	cCall	OtxOfLn,<si>		; Get otx of next line
	xchg	si,ax			; mov si,ax

	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	mov	[cbTextOld],ax

; If there is currently an error and it is on the same line as we
; are about to change, clear the error under the assumption it's about
; to be fixed (if it isn't fixed then a new error is generated).
;
	cmp	di,[txtErr.TXER_otx]
	jne	RLB1
	cmp	[uierr],ER_OM
	je	RLB1			;don't clear of uierr=ER_OM
	mov	[uierr],0
RLB1:

;
; Now make the change.
;
	sub	ax,ax			;clear fNoInsert flag
	cCall	TxtChange,<di,si,ax>
	or	ax,ax
	je	RLB2

;
; There was an error
;
	mov	[uierr],ax
	jmp	SHORT RLB_End

RLB2:
; There was no error this time.
;
	cmp	[uierr],ax		; ax is zero
	je	RLB_End
;
; There was an error before this call to ReplaceLineBuf.
; Make sure that the otx reflects the changes this call has made.
;
	cmp	di,[txtErr.TXER_otx]
	ja	RLB_End 		; brif edit passed error loc

	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	sub	ax,[cbTextOld]
	add	[txtErr.TXER_otx],ax

RLB_End:
	mov	ax,[ln]			; pass line# in ax
	sub	dx,dx			; pass line count in dx
	call	UpdAltListWnd
	cCall	UnKludgePs
RLB_UnGrab:
	mov	ax,[hbuf]
	call	UnGrab			;free space reserved by ModifyAndGrab
	sub	ax,ax			; Assume we changed hBufs
	mov	bx,[hBuf]		
	cmp	bx,[grs.GRS_oRsCur]	;see if hBuf on entry = hBuf on Exit
	jne	RLB_Exit		;return FALSE if not
	dec	ax			;return TRUE (same hBuf)
RLB_Exit:
cEnd

;**************************************************************************
; InsertLineBuf(hbuf, ln, cbNewSrc, pbNewSrc)
; Purpose:
;  Inserts the line specified by pbNewSrc and cbNewSrc into the buffer
;  specified by hbuf before the line ln.
;
; Entry:
;  hbuf     - buffer to insert line into
;  ln       - line to insert before
;  cbNewSrc - length of new line
;  pbNewSrc - text to insert
;
; Exit:
;  if the line was inserted, returns non-zero
;  else sets [uierr] and returns FALSE
;
;**************************************************************************
cProc	InsertLineBuf,<PUBLIC,FAR>
	parmW	hbuf
	parmW	ln
	parmW	cbNewSrc
	parmDP	pbNewSrc
cBegin
DbAssertRel [pbNewSrc],e,[ps.PS_bdpSrc.BDP_pb],UI,<InsertLineBuf: bad pbNewSrc>
	mov	bx,[hbuf]
	call	ModifyAndGrab

	call	fDocumentBuf		; is this a large document buffer
	jz	ILB_NotDocument 	; brif not, let txtmgr handle it
	push	[mrsCur.MRS_pDocumentBuf] 
	push	ln			
	push	cbNewSrc		
	PUSHI	ax,<dataOffset ps.PS_bdpSrc.BDP_pb> ; Ptr to ptr to buffer
	PUSHI	ax,0			; FALSE -> check # lines
	call	S_InsertLineBuf 	; Insert line in doc buffer
	push	ax			; save result for ILB_UnGrab
	jmp	SHORT ILB_UnGrab	

ILB_NotDocument:
	mov	cx,[cbNewSrc]
	cCall	KludgePs

	;Speed optimization so we don't call OtxOfLn for scrap.
	;We only insert lines at the end of the scrap.  This makes
	;Cut/Copy MUCH faster.
	
	mov	ax,[hbuf]
	cmp	ax,[hbufOfScrap]
	jne	NotScrap4
	mov	ax,[txdCur.TXD_bdlText_cbLogical]
	sub	ax,CB_EMPTY_TEXT
	jmp	SHORT GotScrap

NotScrap4:
	cCall	OtxOfLn,<ln>		;ax = text offset to line
GotScrap:
	sub	bx,bx			;clear fNoInsert flag
	cCall	TxtChange,<ax,ax,bx>
	or	ax,ax
	je	ILB_End			;brif no error

	mov	[uierr],ax

	mov	ax,-1
ILB_End:
	inc	ax			; return 0 if fail, 1 if Ok

	push	ax			; save return value

	;The next few lines treat a special case bug.
	;If the editor is extending beyond the end of the current window,
	;calling UpdAltListWnd will cause DrawDebugScr to cause the
	;edit mgr to Draw the debug screen, which will cause its partial
	;line to be dumped to ReplaceLineBuf.
	
	cmp	[cbNewSrc],0
	jne	NotExtend
	mov	ax,[txdCur.TXD_cLines]
	dec	ax
	cmp	ax,[ln]			; pass line# in ax
	je	ItsExtend
NotExtend:
	mov	ax,[ln]			; pass line# in ax
	mov	dx,1			; pass line count in dx
	call	UpdAltListWnd
ItsExtend:

	cCall	UnKludgePs

ILB_UnGrab:
	mov	ax,[hbuf]
	call	UnGrab			;free space reserved by ModifyAndGrab
	pop	ax			;restore return value
ILB_Exit:
	or	ax,ax			;set condition codes for caller
cEnd

;**************************************************************************
; fReadOnlyBuf(hbuf)
; Purpose:
;  Tells the EditMgr whether a buffer {is, is not} read only.
;  A buffer is read only if it is code and we are viewing include file lines.
;
; Entry
;  hbuf - The buffer
;
; Exit:
;  if hbuf is read only, Returns TRUE and sets uierr to:
;     MSG_InclOff if current line didn't come from $INCLUDE file
;      (causes ReportError to ask user if he wants View/IncludeLines turned off)
;     MSG_EditIncl if current line came from $INCLUDE file
;      (causes ReportError to ask user if he wants to edit $INCLUDE file)
;
;**************************************************************************
cProc	fReadOnlyBuf,<PUBLIC,FAR>
	parmW	hbuf
cBegin
	mov	bx,[hbuf]
	cmp	bx,hbufHelp		; Check for Help buffer.
	jne	@F
	mov	ax,sp			; If so, is help window, return TRUE
	jmp	short ExitFReadOnlyBuf	

@@:
	call	ActivateHbuf
	xor	ax,ax			;prepare to return FALSE
ExitFReadOnlyBuf:
cEnd


;**************************************************************************
; LinesInBuf(hbuf)
; Purpose:
;  Return number of lines in a given edit buffer
;
; Entry
;  hbuf - The buffer
;
; Exit:
;  return the number of lines in the buffer.
;
;**************************************************************************
cProc	LinesInBuf,<PUBLIC,FAR>
	parmW	hbuf
cBegin

	mov	bx,[hbuf]
	cmp	bx,hbufHelp			; test for help buffer
	jne	@F
	PUSHI	ax,WM_HELPFILESIZE		; message to send
	push	ax				; garbage parameter
	cCall	SendHelpMsg			; AX = # lines
	jmp	short LIB_End			

@@:
; Return 1 is hbuf is UNDEFINED (for single line edit fields).

	mov	ax,1
	inc	bx
	jz	LIB_End
	dec	bx

; else return the number of lines in the buffer.

	call	ActivateHbuf

	mov	ax,[txdCur.TXD_cLines]

	call	fDocumentBuf		; Is this a document buffer?
	jz	LIB_END 		; brif not, AX = # lines
	push	[mrsCur.MRS_pDocumentBuf] 
	call	S_LinesInBuf		; get # lines in document buffer

LIB_End:
cEnd


;**************************************************************************
; ListAxSiDi
; Entry:
;	ax = otx to next opcode to be executed by interpreter, or
;	     FFFF if next opcode is not in this text table.
;	si = otx to start of line to be listed.
;	di = ptr to buffer descriptor for destination.
; Exit:
;	ax = number of bytes listed (as returned by ListLine)
;	dx = column offset that corresponds to entry's ax
;
;**************************************************************************
ListAxSiDi PROC NEAR
	mov	[otxLsCursor],ax
	cCall	ListLine,<si,di>	; cbLine = ListLine(otx, pbd)
	ret
ListAxSiDi ENDP

;**************************************************************************
; ushort cbGetLineBuf(hbuf, ln, cbMax, szDst)
; Purpose:
;  Called by TextWin's editor, even when the current buffer has been
;  modified and the line needs to be refreshed.  This routine gets
;  the specified ASCII line of text even it is the current line (i.e. UNDO).
;
; Entry:
;  hbuf     - buffer handle.
;  ln       - line to copy
;  cbMax    - Max number of character to copy to buffer.
;  szDst    - where to put the zero terminated line
;  grs.otxCONT = text offset to next instruction to be executed
;
; Exit:
;  if grs.otxCONT points into line being listed:
;     colStmtStart = column of start of next statement
;     colStmtEnd = column of end of next statement
;  else
;     colStmtStart = colStmtEnd = UNDEFINED
;  return number of characters put in buffer (excluding terminating zero)
;  If out-of-memory, sets [uierr] to ER_OM and returns 0
;
;**************************************************************************
cProc	cbGetLineBuf,<PUBLIC,FAR>,<si,di>
	parmW	hbuf
	parmW	ln
	parmW	cbMax
	parmDP	szDst
cBegin
	;Caller can only call us to list to 1 of 2 buffers:
	; ps.bdpSrc or bdEMScratch, see which it is
	; The reason twin passes an absolute pointer is because twin is used
	; by other products that have static buffers.
	
	mov	di,DATAOFFSET ps.PS_bdpSrc
	mov	ax,[szDst]
	cmp	ax,[di.BD_pb]
	je	GotBdPtr		; brif ps.bdpSrc is our buffer
	mov	di,DATAOFFSET bdEMScratch
	DbAssertRel ax,e,[di.BD_pb],UI,<cbGetLineBuf called with bad pointer>

;di = ptr to output buffer descriptor
GotBdPtr:
	mov	bx,[hbuf]
	cmp	bx,hbufHelp		; Check for help buffer
	jne	@F			; if not, skip ahead

	mov	ax,WM_HELPLINE		; message to send
	mov	bx,OFFSET DGROUP:wndHelp ; window to send to
	cCall	SendMessage,<bx,ax,[ln],[szDst],[cbMax]>
	inc	[fWndHelp]		; Set flag for GetLineAttrs
	jmp	GlbExit 		

@@:
	mov	[fWndHelp],0		; make sure flag is reset
	call	ActivateHbuf

	call	fDocumentBuf		; Is this a large document buffer?
	jz	GLB_NotDocument		; brif not, let txtmgr handle it
	push	[mrsCur.MRS_pDocumentBuf] 
	push	ln			
	push	cbMax			
	push	szDst			
	call	S_cbGetLineBuf		; Get line from document buffer
	jmp	SHORT GlbExit		

GLB_NotDocument:
; [otxGetNext] is a cached otx for line [lnGetNext].
; See if we can use it.
	mov	si,[otxGetNext]
	mov	ax,[ln]
	mov	cx,[lnGetNext]
	inc	ax			;cache the otx for [ln]+1
	mov	[lnGetNext],ax
	dec	ax

	jcxz	RefreshCache		;brif cached otx is invalid
	cmp	ax,cx
	je	GotOtx

; Cached otx was no good, we have to search for the otx.
;
RefreshCache:
	cCall	OtxOfLn,<ax>
	xchg	si,ax				; mov si,ax

;Save cached otx for next call
GotOtx:
	mov	[otxLnLast],si

; ListLine checks to see if otxLsCursor is in the current line, and if so
; it returns dx=the equivalent column for the opcode at that otx.
; If the current text table is the `active text table' (i.e. the text table
; containing the next statement to be executed), then otxLsCursor is setup
; appropriately.  grs.oRsContTxtTbl gets set whenever grs.oRsCONT gets set.
; The only time it is different then grs.oRsCONT is when grs.oRsCONT is
; for a DEF FN, in which case, this gets set to the oRs for the module
; that contains the DEF FN.
;
	mov	ax,UNDEFINED		;assume line doesn't contain next stmt
					; to be executed by interpreter
	mov	[colStmtStart],ax
	mov	[colStmtEnd],ax
	xchg	dx,ax			;dx = UNDEFINED
	mov	ax,[grs.GRS_oRsContTxtTbl]
	cmp	ax,[grs.GRS_oRsCur]
	jne	ListTheLine		;brif this isn't the oRs with next
					; stmt to exec
	mov	ax,[grs.GRS_otxCONT]
	inc	ax			;test for UNDEFINED
	jz	ListTheLine		;brif grs.GRS_otxCONT == UNDEFINED
	xchg	dx,ax			;dx = otxCONT
	dec	dx			;undo above inc

	;Don't be tempted to enable this...  Keep the comment around
	; as a reminder, because it is very tempting...
	; This causes bug where if you edit the line with
	; the current instruction, afterward, it looks like the
	; previous line contains the next instruction.
	; Maybe fix this another way, or always keep a NOP after
	; opStSub, opStFunction, opStDefFn, opStStop
	
	;;;;Set dx to otxCONT - 1, so if STOP and BreakPoint after DEF FN,
	;;;; SUB, or FUNCTION will highlight the STOPPED stmt.
	
	;;;;je	ListTheLine		;brif grs.GRS_otxCONT == 0
	;;;;dec	dx

;dx = new otxLsCursor, si = otx to list, di = bdDestination
ListTheLine:
	xchg	ax,dx			; ax = new otxLsCursor
	call	ListAxSiDi		; cbLine = ListLine(otx, pbd)
	inc	ax			; test for UNDEFINED
	je	GlbOmErr		; return 0 if out-of-memory
	dec	ax			; restore ax = cbLine
	push	ax			; save cbLine (return value)
	mov	ax,[otxListNext]	; cache this value for next time
	mov	[otxGetNext],ax
	inc	dx			; test for UNDEFINED
	je	NotOnLine		; brif line doesn't contain next stmt

	;Since we know this line contains next stmt to be executed,
	;call ListLine twice more to find column offset for start and
	;end of stmt, LineAttr can highlight the statement.
	
	push	[otxLsCursor]
	call	OtxBosOfOtx		; ax = otx for next stmt's bos
	call	ListAxSiDi		; cbLine = ListLine(otx, pbd)
					;  since this line was just listed,
					;  we are assured not out-of-memory
	mov	[colStmtStart],dx
	inc	dx
	je	NotOnLine


	push	[otxLsCursor]		; push otx arg.
	call	OtxBosNext		; ax = otx for next stmt's eos
	call	ListAxSiDi		; cbLine = ListLine(otx, pbd)
					;  since this line was just listed,
					;  we are assured not out-of-memory
	mov	[colStmtEnd],dx
	inc	dx
	jne	NotOnLine		; brif not UNDEFINED
	dec	dx			; dx = UNDEFINED
	mov	[colStmtStart],dx	; if colStmtEnd is UNDEFINED,
					; so is colStmtStart
NotOnLine:
	pop	ax			; restore ax = cbLine (return value)
GlbExit:
cEnd

GlbOmErr:
	call	SetUiErrOm		; report error in msg loop
	jmp	SHORT GlbExit		; return ax=0


;**************************************************************************
; LineAttr(attrDefault)
; Purpose:
;  Returns an array discribing the screen attributes for the last line
;  fetched by cbGetLineBuf.
;
; Entry:
;  attrDefault is the screen attribute to use for non-standout text.
;  colStmtStart and colStmtEnd are as set by last call to cbGetLineBuf.
;
; Exit:
;  returns pointer to array of LineAttr's.
;  Each LineAttr consists of a count (cb) and an attribute (attr)
;  attr is the attribute to be used for the next cb characters in the line.
;  The array is terminated with an attr of 0xffff
;  cb may be zero.
;  If cb is 0xffff then attr is used for the rest of the line.
;
;**************************************************************************
cProc	GetLineAttrs,<PUBLIC,FAR>,<si,di>
	parmW	attrDefault
cBegin
	cmp	[fWndHelp],0		; Is being called for help window
	je	@F
	mov	[fWndHelp],0		; Clear fWndHelp
	mov	ax,WM_HELPATTR		; message to send
	cCall	SendHelpMsg,<ax,ax>	; Send message to help system
	jmp	short AttrEnd		

@@:
	mov	si,[attrDefault]
	mov	di,dataOFFSET rgLineAttr

	cCall	fCodeWnd
	jz	UseDefaultAttrs		;brif DOCUMENT text table

	cmp	[fLsIncluded],0
	je	NotIncluded
	mov	si,isaIncludeFileHilite 
NotIncluded:

	cCall	fBpSet,<otxLnLast>
	or	ax,ax
	je	NoBpSet
	mov	si,isaBreakpoint
NoBpSet:
	mov	ax,[colStmtStart]
	mov	dx,[colStmtEnd]
	inc	ax			;test for UNDEFINED
	je	UseDefaultAttrs
	inc	dx			;test for UNDEFINED
	je	UseDefaultAttrs
	dec	ax			;ax = [colStmtStart]
					;dx = [colStmtEnd] + 1
	

; Ahh, this line has the current statement on it.
; We must setup so that it gets highlighted properly
; ax = [colStmtStart], dx = [colStmtEnd]
;
	mov	[di.LA_cb],ax
	mov	[di.LA_attr],si
	sub	dx,ax			;dx = [colStmtEnd]+1-[colStmtStart]
	mov	ax,isaCurBreakpoint
	cmp	si,isaBreakpoint
	je	GotCurBp		;brif current stmt has breakpoint too
	mov	ax,isaCurStmt
GotCurBp:
	mov	[di + (size LINEATTR) + LA_cb],dx
	mov	[di + (size LINEATTR) + LA_attr],ax
	mov	ax,UNDEFINED
	mov	[di + (2*(size LINEATTR)) + LA_cb],ax
	mov	[di + (2*(size LINEATTR)) + LA_attr],si	
	mov	[di + (3*(size LINEATTR)) + LA_attr],ax
	jmp	short GLA_End

UseDefaultAttrs:
	mov	ax,UNDEFINED
	mov	[di + LA_cb],ax
	mov	[di + LA_attr],si
	mov	[di + (size LINEATTR) + LA_attr],ax
GLA_End:
	xchg	ax,di			;return ax = &rgLineAttr
AttrEnd:
cEnd


;**************************************************************************
; StartBigEdit
; Purpose:
;  Called by the Edit Mgr at the start of some operation which results
;  multiple calls to InsertLineBuf/ReplaceLineBuf/DeleteLinesBuf.
;  Examples include multi-line-paste/cut/copy, line-split, line-join.
;
; Entry:
;
; Exit:
;**************************************************************************
cProc	StartBigEdit,<PUBLIC,FAR>
cBegin
	cCall	TxtStartBigEdit
cEnd


;**************************************************************************
; EndBigEdit
; Purpose:
;  Called by the Edit Mgr at the end of some operation which results
;  multiple calls to InsertLineBuf/ReplaceLineBuf/DeleteLinesBuf.
;  Since during a BIG EDIT, all lines are emitted within opReParse
;  operands, and not really parsed to pcode until TxtEndBigEdit,
;  syntax errors may be discovered during TxtEndBigEdit().
;
; Exit:
;  if error encountered, uierr is set to standard error code
;
;**************************************************************************
cProc	EndBigEdit,<PUBLIC,FAR>
cBegin
	cCall	TxtEndBigEdit		;ax = non-zero if serious error
	or	ax,ax
	je	EndBigEdit_End
	mov	[uierr],ax
EndBigEdit_End:
cEnd

;**************************************************************************
; InsertBufInBuf()
; Purpose:
;  Insert the contents of 1 buffer (usually the scrap) into another buffer
;
; Entry:
;  hbufDst - buffer to insert other buffer into.
;  lnDst   - line before which the buffer is to be inserted.
;  hbufSrc - the buffer the be inserted.
;
; Exit:
;  returns zero if out-of-memory, non-zero if success
;
;**************************************************************************
cProc	InsertBufInBuf,<PUBLIC,FAR>,<si,di>
	parmW	hbufDst
	parmW	lnDst
	parmW	hbufSrc
cBegin

	call	StartBigEdit		;so we don't stop inserting just
					; because of a syntax error
	mov	bx,[hbufDst]
	call	ModifyAndGrab

	call	fDocumentBuf		; Is this a large document buffer
	jz	IBIB_NotDocument	; brif not, let txtmgr handle it
	push	[mrsCur.MRS_pDocumentBuf] 
	push	lnDst			

	mov	bx,hbufSrc		; bx = oPrs
	and	bh,7Fh			; mask off bit saying it is PRS.
	RS_BASE add,bx			
	push	[bx.MRS_pDocumentBuf]	
	call	S_InsertBufInBuf	
	jmp	SHORT IBIB_TestRetVal	

IBIB_NotDocument:
	push	[lnDst]
	call	oTxOfLn 		;get oTxInsert for destination buf
	push	ax			;pass oTxInsert
	push	[hBufSrc]		;pass src hBuf
	call	TxtPaste		;insert src buffer at insertion point
IBIB_TestRetVal:
	or	ax,ax			;Out of memory
	jne	IBIB_NotOm		;brif not out of memory
IBIB_OM:
	call	SetUiErrOm		;set out of memory error
IBIB_NotOm:
	call	EndBigEdit		;destination text table is active
	sub	ax,ax			;prepare to return 0 (error)
	cmp	[uierr],ax
	jne	IBIB_Done		;brif got an error
	dec	ax			;return non-zero (success)
IBIB_Done:
	push	ax
	mov	ax,[hbufDst]
	call	UnGrab			;free space reserved by ModifyAndGrab
	pop	ax
cEnd

;**************************************************************************
; ushort near hbufScrap()
; Purpose:
;  Return the handle for the Scrap buffer, allocating the buffer if necessary.
;
; Exit:
;  If out-of-memory, returns NULL
;
;**************************************************************************
cProc	hbufScrap,<PUBLIC,FAR>
	localV	sdName,<size sd>
cBegin
	mov	ax,[hbufOfScrap]
	cmp	ax,UNDEFINED
	jne	hbufScrap_End

; Make a text table (the name is unimportant, but cbName must be non-zero -
; otherwise it is the UNTITLED module), so use the 1 byte string starting
; at address 1 as the name of the scrap.
	lea	bx,sdName
	mov	[bx.SD_pb],DATAOFFSET hbufOfScrap
					;pb -> FF (impossible mrs name)
	mov	[bx.SD_cb],1
	push	bx			;pass &sdName
	PUSHI	ax,RS_scrap		;rsType
	cCall	RsMake			;ax = oRs of scrap
	mov	[hbufOfScrap],ax
	mov	[rsNew],UNDEFINED	;don't tell WnReset to display 
					; scrap in a list window

	DbAssertRel ax,ne,0,UI,<hbufScrap oRs==0>
hbufScrap_End:
cEnd

;**************************************************************************
; UiDelAll
; Purpose:
;	Delete all text from a specified text table
; Entry:
;	bx = hbuf
;
;**************************************************************************
cProc	UiDelAll,<NEAR>
cBegin
	call	ActivateHbuf		;activate text table bx
	call	TxtDelAll		;delete all text in the text
					; table except endprog and eot

	call	fDocumentBuf		; is this a large document
	jz	DelAllExit		; brif not, all done
	push	[mrsCur.MRS_pDocumentBuf] 
	call	FreeBuf 		; Release buffer
	call	NewBuf			; get a new one
	mov	[mrsCur.MRS_pDocumentBuf],ax ; and use that one instead
	DbAssertRel ax,ne,0,UI,<UiDelAll:NewBuf failed>

DelAllExit:
cEnd

;**************************************************************************
;FreeCmdHistory
;Purpose:
;	This is called when we're getting real tight on memory
;	and something needs to be done to free up enough to do
;	the most primitive operations
;Exit:
;	Caller's grs.oRsCur and txtErr are preserved
;
;**************************************************************************
cProc	FreeCmdHistory,<PUBLIC,NEAR>
cBegin
	mov	bx,[hbufOfCmd]
	inc	bx			;if hbufOfCmd is UNDEFINED, exit
	je	EndFreeCmdHistory
	dec	bx

	push	[grs.GRS_oRsCur]	;pass oRsCur to UiRsActivate below
	push	[txtErr.TXER_errCode]
	push	[txtErr.TXER_otx]
	push	[txtErr.TXER_oRs]
	push	WORD PTR [txtErr.TXER_fDirect]

	call	UiDelAll
	PUSHI	ax,<DATAOFFSET txdCur.TXD_bdlText>
	call	BdlTrim			;If we don't trim the buffer,
					; the space isn't made available
					; to other callers until TxtDeactivate
	pop	ax
	mov	[txtErr.TXER_fDirect],al
	pop	[txtErr.TXER_oRs]
	pop	[txtErr.TXER_otx]
	pop	[txtErr.TXER_errCode]
	call	UiRsActivate		;parm pushed on entry
EndFreeCmdHistory:
cEnd

;**************************************************************************
; void near FreeScrap()
; Purpose:
;  Release the contents of the 'scrap' buffer.
;
;**************************************************************************
cProc	FreeScrap,<PUBLIC,FAR>
cBegin
	mov	bx,[hbufOfScrap]
	inc	bx			;test for UNDEFINED
	jz	FreeScrap_End		;brif no scrap is allocated
	dec	bx			;restore bx=hbufOfScrap
	call	UiDelAll		;Delete all the lines in hbufOfScrap
FreeScrap_End:
cEnd

;**************************************************************************
; CutAll(hbufSrc)
; Purpose:
;	Cut the entire active text table to the scrap.
; Entry:
;	hbufSrc = hbuf (i.e. oRs) of source text table.
; Exit:
;  returns zero if out-of-memory, non-zero if success
;
;**************************************************************************
cProc	CutAll,<PUBLIC,NEAR>,<si,di>
	parmW	hbufSrc
	localW	lnDst
cBegin
	call	FreeScrap
	DbAssertRel [hbufOfScrap],ne,UNDEFINED,UI,<CutAll: no hbufOfScrap>

	call	StartBigEdit		;so we don't stop inserting just
					; because of a syntax error

; Cycle through all the lines in the source buffer, inserting each line in
; the Scrap buffer.
	sub	di,di
	mov	[lnDst],di		;init lnDst to 0
CA_NextLine:
	cCall	LinesInBuf,<hbufSrc>
	cmp	ax,di
	jna	CA_Done 		;brif done with loop

	;Use the line buffer of the source buffer as a scratch buffer.
	push	[hbufSrc]		;pass hbuf
	push	di			;pass line to get
	push	[ps.PS_bdpSrc.BDP_cbLogical] ;pass cbMax
	push	[ps.PS_bdpSrc.BDP_pb]	;pass pbDst
	call	cbGetLineBuf		;ax = #bytes fetched, uierr may = ER_OM
	inc	di			;bump line count

	push	[hbufOfScrap]		;pass Scrap as destination buffer
	push	[lnDst]
	push	ax			;pass cb
	push	[ps.PS_bdpSrc.BDP_pb]	;pass pbSrc
	call	InsertLineBuf
	je	CA_Done 		;brif line not inserted (out of memory)
	inc	[lnDst]
	jmp	short CA_NextLine

CA_Done:
	mov	bx,[hbufOfScrap]	;activate Scrap text table
	call	ActivateHbuf		; for EndBigEdit
	call	EndBigEdit
	sub	ax,ax			;prepare to return 0 (error)
	cmp	[uierr],ax
	jne	CA_Exit 		;brif got an error

; Now delete all source from text table

	mov	bx,[hbufSrc]
	call	UiDelAll		;delete all text in text table
CA_Exit:
cEnd

;**************************************************************************
; PasteAll
; Purpose:
;	Copy the entire scrap to the active text table.
; Entry:
;	grs.oRsCur identifies source text table.
;
;**************************************************************************
cProc	PasteAll,<PUBLIC,NEAR>
cBegin
	push	[grs.GRS_oRsCur]	;pass hbufSrc
	PUSHI	ax,0			;lnInsert = 0
	push	[hbufOfScrap]		;pass hbufDst
	call	InsertBufInBuf
cEnd



;*********************************************************************
; void NEAR MoveCursorPwndCur(ln, col)
; Purpose:
;  Position cursor to a particular line/column within the active window.
;
; Entry:
;  pwndAct points to active window's structure
;  ln  - line to position cursor to.
;  col - column to position cursor to.
;
; Exit:
;
;*********************************************************************
cProc	MoveCursorPwndCur,<PUBLIC,NEAR>
	parmW	ln
	parmW	col
cBegin
	DbAssertRel [pwndAct],ne,0,UI,<MoveCursorPwndCur: pwndAct=0>

	cCall	MoveCursorPwnd,<pwndAct,ln,col>
cEnd


;*********************************************************************
; void NEAR MoveCursorPwnd(pwnd, ln, col)
; Purpose:
;  Position cursor to a particular line/column within the window
;  specified
;
; Entry:
;  pwnd - points to window's structure
;  ln  - line to position cursor to.
;  col - column to position cursor to.
;
; Exit:
;
;*********************************************************************
cProc	MoveCursorPwnd,<PUBLIC,NEAR>
	parmW	pwnd
	parmW	ln
	parmW	col
cBegin
	DbAssertRel [ln],ne,0ffffH,UI,<MoveCursorPwnd: ln=ffff>
	DbAssertRel [col],ne,0ffffH,UI,<MoveCursorPwnd: col=ffff>
	DbAssertRel [pwnd],ne,0,UI,<MoveCursorPwnd: pwnd=0>

	push	[pwnd]
	PUSHI	ax,EM_SELCHARS		; message
	push	[ln]			; wParam - line.
	mov	ax,[col]
	push	ax			; lParam - start and end column
	push	ax
	call	SendMessage
cEnd


;*********************************************************************
; MoveTxdCursor
; Purpose:
;	Position cursor where it was last time text was visible.
; Entry:
;	pUwinAct points to active window's structure
;	pUwinAct->lnCursor = line to position window at
;
;*********************************************************************
cProc	MoveTxdCursor,<PUBLIC,NEAR>
cBegin
	DbAssertRel [pwndAct],ne,0,UI,<MoveTxdCursor: pwndAct=0>
	call	UiRsActivateWnd 	;activate active window's register set
	mov	ax,[txdCur.TXD_lnCursor]
	inc	ax			;test for UNDEFINED
	je	MtxdFirst		;brif 1st time this txd has been shown
					; line = 0
	dec	ax			;restore ax = last line cursor was on
MtxdFirst:
	mov	[txdCur.TXD_lnCursor],ax ;if it was UNDEFINED, set it to 0
					; now, so we don't think its the first
					; time we've seen it on subsequent calls
	push	ax			;pass line
	PUSHI	ax,0			;pass column = 0
	call	MoveCursorPwndCur	;move pwndCur's cursor
cEnd



;SetUiErrCond added with revision [47]
;*********************************************************************
; SetUiErr, SetUiErrCond
; Purpose:
;	Set uierr, and txterr properly.
;	SetUiErrCond conditionally sets uierr and txterr if:
;	    1. error code is non-zero
;	    2. uierr is not already set.
;
; Entry:
;	errCode is the error code
; Exit:
;	Sets uierr and txterr
;	callers assume that only AX is used.
;	AX = error code
;
;*********************************************************************
cProc	SetUiErrCond,<PUBLIC,NEAR>	; Same parms as SetUiErr
	parmW	errCode
cBegin
	mov	ax,[errCode]		; to return error in AX
	or	ax,ax			; got an error?
	jz	NoError			; brif not -- just
	cmp	uierr,0			; already got another error?
	je	SetUiErr_Start		; brif not -- go ahead & set uierr
NoError:
cEnd

cProc	SetUiErr,<PUBLIC,NEAR>
	parmW	errCode
cBegin
SetUiErr_Start:				; entry point for SetUiErrCond
	mov	[txtErr.TXER_oRs],UNDEFINED
	mov	ax,[errCode]
	mov	[uierr],ax
cEnd

cProc	SetUiErrOm,<PUBLIC,NEAR>
cBegin
	PUSHI	ax,ER_OM
	call	SetUiErr
cEnd

;*********************************************************************
; SetUiErrFar
;	Added with revision [26].
; Purpose:
;	Set uierr, and txterr properly.
; Entry:
;	AX is the error code
; Exit:
;	Sets uierr and txterr
;	callers assume that only AX is used.
;
;*********************************************************************

cProc	SetUiErrFar,<PUBLIC,FAR>
cBegin
	cCall	SetUiErr,<ax>
cEnd

;*********************************************************************
; void near ReportError()
; Purpose:
;  Display an error dialog box and, if appropriate, position the cursor
;  where the error was encountered.
;
; Entry:
;  uierr contains an offset into the standard BASIC message table.
;     If uierr=ER_GoDirect, this function just returns.
;     If high bit is set, the ASCII error message is contained in the
;     parser's global structure ps.bdErr.
;     If txtErr.oRs is UNDEFINED
;	 the cursor will not be positioned
;     Else
;        if txtErr.fDirect is TRUE
;           The cursor is put on the last line entered in the command window
;	    txtErr.oSrc = column where cursor is to be positioned (0..n)
;	 else
;	    A list window with register set txtErr.oRs is made active
;	    The cursor is put on the line indicated by txtErr.otx
;	    if txtErr.oSrc = UNDEFINED,
;	       cursor is placed at start of statement indicated by txtErr.otx
;	    else
;	       txtErr.oSrc = column where cursor is to be positioned (0..n)
;     If b$fCompErr is non-zero, error was in compiled code
;	 b$ErrLin = line # of last runtime error (ERL)
;	 b$ErrMod = far ptr to 0-terminated module name
;	 b$ErrAdr = far ptr where runtime call was made
;     Else
;	 error was in interpreted pcode, a simple dialog is displayed
;
;     If uierr == MSG_MrsNotFound
;        an OK/Cancel dialog box will be put up.  If the user selects
;	 OK, the current mrs will be unloaded.
;
;*********************************************************************
cProc	ReportError,<PUBLIC,NEAR>,<si>
	localW	pwndOldFocus
	localW	colSelect
	localW	lnSelect
	localW	cbSelect
	localW	fSelect
cBegin
ReWhileErr:
	sub	si,si
	mov	[pwndOldFocus],si
	mov	[fSelect],0ffffH	;initialize to UNDEFINED
	xchg	si,[uierr]		;si=uierr, uierr=0
	cmp	si,ER_OM
	jne	NotOmErr
	sub	bx,bx
	mov	bl,[b$ErrInfo]		;bx = extended error code
	shl	bx,1			;bx = 2*extended error code
	mov	si,cs:twExtOmErr[bx]	; si = extended error msg

	;If user wants to enter a direct mode stmt (like SYSTEM), make sure
	; there is room in the command window's buffer to hold the command.
	; If we didn't do this, it could be an infinite out-of-memory loop.
	
	call	FreeCmdHistory
NotOmErr:
	cCall	FlushMsgs		;Don't let any pending keys
					;foul us up.
	cmp	si,MSG_GoDirect
	jne	NotGoDirect
	jmp	ReEnd			;brif user pressed CANCEL
					; button in some Dialog box
NotGoDirect:
	cmp	si,MSG_HelpOOM		; Memory problems in help?
	jne	NotHelpOOM		; no, continue checking
	cCall	DisplayHelpOOM		; handled in shared code
	jmp	ReEnd			; exit
NotHelpOOM:				

	mov	ax,[txtErr.TXER_oRs]
	cmp	[hbufOfScrap],ax	;see if error during cut/paste
					; (like out-of-memory)
	je	ReNoCursor		;can't position to scrap
	cmp	[hbufOfCmd],ax
	je	ReCmdWndErr		;brif error in command window
					; since no syntax checking
					; in cmd window, probably
					; out-of-memory
	inc	ax			;test for UNDEFINED
	je	ReNoCursor		;brif can't position cursor

	cmp	[txtErr.TXER_fDirect],FALSE
	je	ReListCursor		;brif error wasn't in cmd wnd

ReCmdWndErr:
	;Position cursor at offending point in command window
	mov	ax,[txtErr.TXER_oSrc]	; if txtErr.TXER_oSrc=UNDEFINED
	inc	ax			;  then column = 0
	je	SkipDec1
	dec	ax			; otherwise, restore its value
SkipDec1:
	cCall	MoveCursorPwndCur,<lnCmdLast, ax> 
	jmp	SHORT ReNoCursor

ReListCursor:
	;Position cursor at offending point in list window
	mov	ax,txtErr.TXER_otx	;if txtErr.TXER_otx=UNDEFINED
	mov	[fSelect],ax		;set Select flag
	inc	ax			;then otx=0
	jz	short SkipDec
	dec	ax
SkipDec:
	cCall	ShowTok,<txtErr.TXER_oRs,ax,txtErr.TXER_oSrc>

;Since activation of a dialog box kills an edit window's input focus,
;which causes it to turn off the current selection, we have to play
;some tricks to cause the offending token to be highlighted while the
;dialog box is up.  First we find out what line, column and word size the
;cursor is currently on.  Then call FlushFocus() to kill the input focus.
;Then select the text.
;
	cmp	[fSelect],0ffffH	;See if we have selection
	je	short ReNoCursor	;if not, don't highlight
	inc	[fAdjustMsgBox] 	;set flag for moving error box
	call	GetEditColumn		;ax = edit column
	mov	[colSelect],ax
	call	GetEditLine		;ax = edit line
	mov	[lnSelect],ax
	PUSHI	ax,<DATAOFFSET bufStdMsg>
	PUSHI	ax,41d			;40 + 0-byte terminator
	PUSHI	ax,GEW_NODOTMASK	; do not include . in search
	call	GetEditWordMask 	; ax = #chars in current word
	mov	[cbSelect],ax
	cCall	FlushFocus

	push	[lnSelect]
	push	[colSelect]
	push	[cbSelect]
	call	SelectTextPwndCur	;select offending token

; Display a message box containing the error
; if high bit of si is set, the message is in ps.PS_bdErr
; else si is the error number.
;
ReNoCursor:
	cmp	[b$fCompErr],0
	je	NotCompCodeErr
	jmp	CompCodeErr		;brif err was in compiled code

NotCompCodeErr:
	mov	dx,dataOFFSET ps.PS_bdErr
	or	si,si
	jns	ReStdMsg		;brif caller didn't build msg
	cmp	[ps.PS_bdErr.BD_cbLogical],76d
	mov	si,ER_SN		;if msg too long to fit on
	ja	ReStdMsg		; one line, Syntax error
	mov	iHelpId,9999
	PUSHI	ax,MB_OK
	push	dx			;pass <dataOFFSET ps.PS_bdErr>
	call	MsgBoxBd		
J1_ReEnd:
	jmp	SHORT ReEnd		

ReStdMsg:
	mov	ax,MB_OK
	push	ax			;pass dialog box type
					; to MsgBoxStd, MsgBoxStd2
					; and MsgBoxStdBd


NotMsg2:
	push	si			;pass iMsg to MsgBoxStd
	test	HelpFlags,HLP_RTERR	; did this error come from RT?
	jz	NotRtError		; no, call MsgBoxStd
	call	MsgBoxStdRt		; don't muck with iHelpId
	jmp	SHORT ReEnd		; we are done with error
NotRtError:				
	call	MsgBoxStd
	jmp	SHORT ReEnd

;runtime error was in compiled code
CompCodeErr:
	mov	[uierr],si		;set uierr properly
	call	MsgBoxCompErr
	mov	[b$fCompErr],0
	cmp	[uierr],si		;see if uierr is the same
	jne	ReEnd			;if not, then we have new error
	mov	[uierr],0		;otherwise, reset uierr
ReEnd:

	;Turn off highlighted text
	cmp	[fSelect],0ffffH
	je	short ReExit1
	cCall	MoveCursorPwndCur,<lnSelect, colSelect>
	
;See if another error has occurred while reporting this error
;For example, we ask user if he wants to edit an include file,
;user responds YES, and we load include file which causes error.
;
ReExit1:
	cmp	[uierr],0
	je	ReExit
	jmp	ReWhileErr

ReExit:
	sub	ax,ax
	mov	[b$ErrInfo],al			;reset extended error code
	mov	[ps.PS_bdErr.BD_cbLogical],ax	;release ASCII msg buffer
	mov	[fAdjustMsgBox],FALSE
	and	HelpFlags,NOT HLP_RTERR 	; clear the flag
cEnd


;*********************************************************************
; void near ShowStmt(oRs, otx, col)
; Purpose:
;  Makes a statement visible, activating list window if necessary.
;
; Entry:
;   oRs  - Text table containing the line to show
;   otx  - Offset in text table of line to display
;   col  - Column to position cursor on.  If UNDEFINED,
;	   otx is used to compute column for start of stmt.
;
; Exit:
;   alters contents of ps.bdpSrc
;
;*********************************************************************
cProc	ShowStmt,<PUBLIC,NEAR>
	parmW	oRs
	parmW	otx
	parmW	col
cBegin
	push	[oRs]
	cCall	UiRsActivate

	push	[oRs]		;pass to ShowTok
	push	[otx]		;pass to OtxBosOfOtx
	call	OtxBosOfOtx	;ax = otx for beginning of stmt
	push	ax		;pass otx to ShowTok
	push	[col]		;pass col to ShowTok
	call	ShowTok
cEnd

cProc	ShowTok,<PUBLIC,NEAR>,<si>
	parmW	oRs
	parmW	otx
	parmW	col
	localW	line
	localW	oldCol
cBegin
	mov	si, [oRs]

	;If oRs is for a DECLARE or DEF FN prs, which has no text
	;table, use the module's test table instead
	
	cCall	UiRsActivate,<si>	;grs.oRsCur = si
	test	[txdCur.TXD_flags],FTX_mrs
	je	NotMrs			;brif cur txt tbl is for procedure
	mov	si,[grs.GRS_oMrsCur]	;use module's oRs, so we don't
					; activate oRs of DEF FN or DECLARE
	cCall	UiRsActivate,<si>	;grs.oRsCur = si
NotMrs:
	cCall	WndAssignList1		 ;make grs.oRsCur visible in list window

; Which line is the statement on?

	cCall	LnOfOtx,<otx>

	cmp	[fLnNotIncl],0
	jne	SS_NotIncludedLine

; Line was in an include file.
; Turn on viewing of include files, and recalulate the line.
;
	PUSHI	ax,TRUE			; push TRUE
	call	CmdViewInclude

	cCall	UiRsActivate,<si>	; CmdViewInclude activates
					; other text tables.
	cCall	LnOfOtx,<otx>		; ax = line

;Call cbGetLineBuf to set colStmtStart and colStmtEnd to mark the
;columns for the start and end of statement that surround grs.otxCONT
;
SS_NotIncludedLine:
	mov	[line],ax
	cCall	cbGetLineBuf,<si,ax,[ps.PS_bdpSrc.BDP_cbLogical],[ps.PS_bdpSrc.BDP_pb]>
	mov	dx,[col]
	cmp	dx,UNDEFINED
	jne	SS_SetCursor		; brif caller know's column

	;We need to map otx to column, by listing the pcode.
	mov	ax,[otx]
	mov	[otxLsCursor],ax	; tell ListLine to find column
					;  offset for stmt
	cCall	OtxOfLn,<[line]>	; ax = otx for start of line
	push	ax			; pass otx to ListLine
	PUSHI	ax,<DATAOFFSET ps.PS_bdpSrc>
	cCall	ListLine		; ax = cbLine
					; dx = column for otxLsCursor
	inc	ax			; test for UNDEFINED (out of memory)
	jne	NotOmErr1		; brif not out-of-memory
					; ax = 0 (just list a blank line)
	call	SetUiErrOm		; report error in msg loop
					; dx is preserved
NotOmErr1:
	inc	dx			; test for UNDEFINED
	je	SS_SetCursor		; if UNDEFINED, column=0
	dec	dx			; restore dx=column equivalent to [otx]

;If showing current statement, force entire statement to be visible if possible
;by first positioning to end-of-stmt, then to start-of-stmt
;
SS_SetCursor:
	mov	[oldCol],dx		; save the column equivalent to [otx]
	cmp	dx,[colStmtStart]	; if we are at the start of a statement
	jne	SkipAddCursor		; then ensure entire statement visible
	mov	dx,[colStmtEnd]
SkipAddCursor:
	cCall	MoveCursorPwndCur,<[line],dx>
	cCall	MoveCursorPwndCur,<[line],[oldCol]> ; move back to old column
	call	DoDrawDebugScr		; update debug screen if a
	or	al,al			;  new register set has been
					;  made active.
	jne	SS_Exit			; brif debug screen was redrawn

	;If only change has been current line in active window, at least
	; draw that window.  We could have just called DrawDebugScr before
	; DoDrawDebugScr and eliminated this code, but this is faster for
	; animation.
	
	cCall	DrawWindow,<[pwndAct]>
SS_Exit:
cEnd


;**************************************************************************
; DoCmd(szCmd)
; Purpose:
;  Setup to execute a direct mode command.
;
; Entry:
;  szCmd points to a 0-byte terminated ASCII command to be executed.
;  if szCmd == NULL, command has already been copied to ps.bdpSrc.
;
; Exit:
;  If command contains no parser/scanner errors,
;     fGotCmd == TRUE
;     grs.bdlDirect contains pcode to be executed
;  else
;     uierr = error to be reported by GetCmd's loop
;
;**************************************************************************
cProc	DoCmd,<PUBLIC,NEAR>
	parmDP	szCmd
cBegin
	mov	ax,[grs.GRS_oMrsMain]
	inc	ax			;test for UNDEFINED
	jne	DoCmdGotMain		;brif no main module
	PUSHI	ax,MSG_NoMainProg
	call	SetUiErr
	jmp	SHORT DoCmd_Exit

DoCmdGotMain:
	;so edit mgr doesn't think it already has current line cached.
	cCall	EditMgrFlush1

	mov	cx,[szCmd]
	jcxz	DoCmd_EndOk
	cCall	SetPsBufSz,<cx>
DoCmd_EndOk:
	mov	[fDoCmd],sp		; sp is TRUE
	mov	[fGotCmd],sp
DoCmd_Exit:
cEnd

;**************************************************************************
; boolean NEAR CmdEnter()
; Purpose:
;  Called by the main message pump to filter Carriage Return events
;  out of the event stream when the command window is active.  This
;  allows CR to have a different meaning in the command window than it
;  does in list windows.  In a list window, it means split the line.
;  In the command window, it means execute this entire line.
;
; Exit:
;  If command window is active,
;     Causes command to be executed
;     If command contains no parser/scanner errors,
;        fGotCmd == TRUE
;        grs.bdlDirect contains pcode to be executed
;     else
;        uierr = error to be reported by GetCmd's loop
;     returns TRUE
;  else
;     returns FALSE
;
;**************************************************************************
cProc	CmdEnter,<PUBLIC,NEAR>,<si,di>
cBegin
	mov	bx,offset DGROUP:wndCmd	; bx -> command window's UWIN
	cmp	bx,[pwndAct]
	je	CE1			;brif command window is active

; Ignore if command window is not active.

	sub	ax,ax			;tell caller it wasn't in Cmd Window
	jmp	CE_End

; Copy the line that ENTER was pressed on, and setup to execute it.
;
CE1:
	mov	cl,[bx.arcClipping.ayBottomArc]
	sub	cl,[bx.arcClipping.ayTopArc]; See if window has zero lines
	jnz	CmdEnterOk		
	jmp	CE_ExitTrue		; return true
CmdEnterOk:

	mov	bx,[bx.pefExtra]	
	mov	di,[bx.EF_ipCur_oln]	; Keep lnCmd in di

	;flush dirty line in active window.  Also makes sure edit mgr
	; doesn't think it already has current line cached
	call	EditMgrFlush1

	cCall	cbGetLineBuf,<[hbufOfCmd],di,[ps.PS_bdpSrc.BDP_cbLogical],[ps.PS_bdpSrc.BDP_pb]>
	sub	ax,ax
	cCall	DoCmd,<ax>

	mov	si,[txdCur.TXD_cLines]
	dec	si			; si = number of last line (0..n)
	js	CE_BottomLine		; brif cmd window has 0 lines
	cmp	di,si
	je	CE_BottomLine		; brif editing bottom line
	mov	si,di			; si = command line
	inc	si			; si = line after command line
	jmp	SHORT CE5		;      (place to move cursor to)

; ENTER was pressed on the last line of the COMMAND window, so add a blank
; line to the end for the next command to be entered into.
;
CE_BottomLine:
	sub	ax,ax
	inc	si			; bump # lines in window
	cCall	InsertLineBuf,<[hbufOfCmd],si,ax,[ps.PS_bdpSrc.BDP_pb]>
	je	CE_ExitTrue		; brif line not inserted (out of memory)

; If there are more than ten lines in the command window then
; get rid of the first.
;
	mov	di,offset DGROUP:wndCmd ; di points to cmd wnd from now on
	mov	ax,si			; ax = #lines in window
	sub	ax,10d
	jbe	CE4			; brif not more than 10

	push	ax			; pass oldest line to be deleted
	cCall	OtxOfLn			; ax = text offset to top line to delete
	sub	bx,bx
	cCall	TxtChange,<bx,ax,sp>	; delete top line of command window
	mov	bx,[di.pefExtra]	
	dec	[bx.EF_pdCur_olnTop]
	dec	si			; decrement current line

; If there are more lines in the buffer than in the window, make sure that
; when the cursor is put on the last line (at CE5:), that that line is
; at the bottom of the window (so that the most possible history is displayed).
;
CE4:
	sub	ch,ch
	mov	cl,[di.arcClipping.ayBottomArc]
	sub	cl,[di.arcClipping.ayTopArc]
	cmp	cx,si
	ja	CE5

	sub	ax,ax
	cCall	MoveCursorPwndCur,<si,ax>

	mov	ax,si
	sub	ch,ch
	mov	cl,[di.arcClipping.ayBottomArc]
	sub	cl,[di.arcClipping.ayTopArc]
	sub	ax,cx
	inc	ax
	sub	bx,bx
	cCall	MoveCursorPwndCur,<ax,bx>

; Put the cursor on the last line of the buffer (the blank line we inserted).
CE5:
	sub	ax,ax
	cCall	MoveCursorPwndCur,<si,ax>
	or	si,si			;if the line is zero,
	jz	CE6			;old line has disappeared
	dec	si
CE6:
	mov	[lnCmdLast],si		;save for error reporting
CE_ExitTrue:
	mov	ax,sp			;mov ax,TRUE

;ax = TRUE if Enter key was handled, FALSE if ignored (cmd window not active)
CE_End:
cEnd


;**************************************************************************
; fCodeWnd()
; Purpose:
;  returns TRUE if the active window contains pcode.
;
; Entry:
;
; Exit:
;  returns TRUE (nonzero in al/ax) if the active window contains code.
;  condition codes set based on value in ax
;
;**************************************************************************
cProc	fCodeWnd,<PUBLIC,NEAR>
cBegin
	mov	al,[mrsCur.MRS_flags2]
	mov	dx,FM2_NoPcode
	and	ax,dx			;isolate FM2_NoPcode bit
	xor	ax,dx			;ax = non-zero iff FM2_NoPcode 0
cEnd


;**************************************************************************
; GetSelText
; Purpose:
;  Copies the selected text in the current window to the specified buffer.
;
; Entry:
;  pb       - Near pointer to where text is to go.
;  cbMax    - max number of chars to copy
;
; Exit:
;  returns the number of chars copied.
;  The string is copied to pb, and is null terminated.
;
;**************************************************************************
cProc	GetSelText,<PUBLIC,NEAR>
	parmW	pb
	parmW	cbMax
cBegin
	cCall	GetEditText,<[pwndAct],[pb],[cbMax]>
cEnd


;***************************************************************************
; SetBookMark
; Purpose:
;  Sets to the specified bookmark (0 to 3)
;
; Entry:
;  cMark    - integer in range 0 to 3 specifying bookmark
;
; Exit:
;  Sets the specified bookmark to the current register set, otx, and column
;
;***************************************************************************

cProc	SetBookMark,<PUBLIC,NEAR>,<si>
	parmW	cMark
cBegin
DbAssertRel	cMark,b,MAXBOOKMARK,UI,<SetBookMark:cMark out of range>

	mov	si,dataOFFSET BookMarks
	mov	ax,size BookMark
	mov	cx,[cMark]
BookMarkValid:
	mul	cl
	add	si,ax
	cCall	GetEditLine
	cmp	pwndAct,OFFSET DGROUP:wndHelp	; displaying in help window
DJMP	jne	SetBookMarkNormal		; no, continue as normal

	
	;Since pwndAct = &wndHelp, the help window must be open.  Thus
	;we must be displaying a topic, hense the buffers must be allocated.
	
DbAssertTst	HelpFlags,nz,HLP_GOTBUF,UI,<SetBookMark:HLP_GOTBUF is FALSE>

	push	ax				; save edit line
	cCall	RetrieveHelpHistory		; pop last item
DbAssertRel	cx,ne,0,UI,<SetBookMark:Help Open with no help history>
	cCall	RecordHelpHistory		; put it back
	xchg	ax,cx				; CX = low word of item
						; DX = high word of item
	pop	ax				; AX = line #

	jmp	short SaveBookMarkVal
SetBookMarkNormal:
	mov	dx,NOT_HELPTOPIC		; flag as not a help topic
	mov	cx,[grs.GRS_oRsCur]		; Get oRs
SaveBookMarkVal:
	mov	[si].BM_Nc,dx			; save Nc
	mov	[si].BM_oRs,cx			; save oRs
	mov	[si].BM_oln,ax
	cCall	GetEditColumn
	mov	[si].BM_col,ax
SetBookMark_Exit:
cEnd

;***************************************************************************
; GotoBookMark
; Purpose:
;  Moves to the specified bookmark (0 to 3)
;
; Entry:
;  cMark    - integer in range 0 to 3 specifying bookmark
;
; Exit:
;  moves cursor to the specified bookmark
;
;***************************************************************************

cProc	GotoBookMark,<PUBLIC,NEAR>,<si,di>
	parmW	cMark
cBegin
DbAssertRel	cMark,b,MAXBOOKMARK,UI,<GotoBookMark:cMark out of range>

	mov	si,dataOFFSET BookMarks
	mov	ax,size BookMark
	mov	cx,[cMark]
	mul	cl
	add	si,ax
	cmp	[si].BM_oRs,UNDEFINED	; is this bookmark defined?
	jne	CheckHelpBookmark	; yes, go process it
Bleep_n_Exit:
	cCall	CowMoo			; inform user of problem
DJMP	jmp	short Nomark		; and exit
CheckHelpBookmark:			
	mov	dx,[si].BM_Nc		; get high word of help id
	cmp	dx,NOT_HELPTOPIC	; reference a help topic?
	je	GotoBookMarkNormal	; no, process as normally

	; we do not try to start the help system, as it is impossible
	; to have a help bookmark unless the help system is running.
	; Whenever the help system gets shut down, we are suspose to
	; clear all bookmarks into help related code.

DbAssertTst	HelpFlags,nz,HLP_GOTBUF,UI,<GotoBookMark:Help Bookmark exists when help system not started>

	mov	ax,[si].BM_oRs		; set DX:AX to context number
	mov	di,[si].BM_oLn		; DI = line # (engine) or -1 (varhlp)
	xor	bx,bx			; BX = 0
	;di = line # (engine) or 0  (varhelp)
	;bx = 0      (engine) or -1 (varhelp)

	PUSHI	cx,<OFFSET DGROUP:wndHelp> ; window to send message to
	PUSHI	cx,WM_HELPDISP		; message to send
	push	bx			; varhelp/engine help flag
	push	dx			; (long)context number
	push	ax			
	cCall	SendMessage		; redisplay the topic
	cmp	al,HELP_HANDLED 	; was it a handled error?
DJMP	je	NoMark			; yes, exit quietly
	or	al,al			; did we succeed?
DJMP	jne	Bleep_n_Exit		; no, inform user and exit


	PUSHI	ax,<OFFSET DGROUP:WndHelp> ; window to activate
	cCall	WndActivate		; goto help window

	mov	bx,di			; BX = line number
	jmp	short GotHelpBookMark	; move cursor

GotoBookMarkNormal:
	mov	ax,[si].BM_oRs
	cCall	UiRsActivate,<ax>
	cmp	[mRsCur.MRS_ogNam],OGNAM_IMMEDIATE ; is it immediate wnd?
	jne	PutInListWnd			   ; no, process as normal
	PUSHI	ax,<OFFSET DGROUP:WndCmd>	   ; window to activate
	cCall	WndActivate			   ; goto immediate window
	jmp	short GetBookMarkLine		   ; go move cursor
PutInListWnd:
	cCall	WndAssignList1

GetBookMarkLine:				   
	mov	bx,[si].BM_oln
GotHelpBookMark:
	mov	cx,[si].BM_col

;This is just MoveCursorPwndCur with a PostMsg instead of a SendMsg
	DbAssertRel [pwndAct],ne,0,UI,<GotoBookMark: pwndAct=0>
	DbAssertRel bx,ne,0ffffH,UI,<GotoBookMark: ln=ffff>
	DbAssertRel cx,ne,0ffffH,UI,<GotoBookMark: col=ffff>

	PUSHI	ax,[pwndAct]		; pwnd
	PUSHI	ax,EM_SELCHARS		; message
	push	bx			; wParam - line.
	push	cx			; lParam - start and end column
	push	cx

;We must post the message as opposed to sending it, because since the
;EditMgr can't be called recursively, it doesn't work properly.
	call	PostMessage
NoMark:
cEnd


;Added with [20]
;***
;ReAssignBookMark - change the oRs of bookmarks
;
;Purpose:
;	Changes the oRs of all bookmarks that match oRsOld to oRsNew.
;	This routine can be used to invalidate any bookmarks with a given
;	oRs by specifying UNDEFINED for oRsNew.
;
;Entry:
;	oRsOld	: oRs of bookmarks to be matched
;	oRsNew	: replacement value for oRs
;
;Exit:
;	None.
;
;Uses:
;	Per C Convention
;
;****

cProc	ReAssignBookMark,<PUBLIC,NEAR>
parmW	oRsOld
parmW	oRsNew
cBegin
	mov	bx,OFFSET DGROUP:BookMarks
	mov	cx,MAXBOOKMARK
	mov	ax,oRsOld		; put in AX for comparisons
	mov	dx,oRsNew		; put in DX for move
DiscardBookMarks:
	cmp	[bx].BM_oRs,ax		; does this bookmark have the oRs?
	jne	NextBookMark		; no, try the next one
	cmp	[bx].BM_Nc,NOT_HELPTOPIC; is it help engine help (not real ors)
	je	ChangeBookMark		; no, go reassign it
ChangeBookMark: 			
	mov	[bx].BM_oRs,dx		; reassign the bookmark
NextBookMark:
	add	bx,size BookMark
	loop	DiscardBookMarks
cEnd



;Added with [22]
;***
;DiscardHelpBookMarks - discard all bookmarks dealing with help
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per C Convention
;
;****

cProc	DiscardHelpBookMarks,<PUBLIC,NEAR>
cBegin
	mov	bx,OFFSET DGROUP:BookMarks
	mov	cx,MAXBOOKMARK
DiscardHelpBM:
	cmp	[bx].BM_Nc,NOT_HELPTOPIC	; is it help topic ?
	je	DiscardNext			; no, skip it
	mov	[bx].BM_oRs,UNDEFINED		; indicate no longer in use
DiscardNext:
	add	bx,size BookMark		; point to next bookmark
	loop	DiscardHelpBM			; check next one
cEnd


;***
;CowMoo: Beep the speaker
;
;Purpose:
;	Beeps the speaker by doing DoSound(0).
;
;Entry:
;	None.
;
;Exit:
;	None
;
;Preserves:
;	AX,BX,DX
;Uses:
;	CX,ES
;
;****

cProc	CowMoo,<PUBLIC,NEAR>,<AX,BX,DX>
cBegin
	xor	ax,ax			; do a regular beep, not a click
	cCall	DoSound,<ax>		; beep the speaker
cEnd

sEnd	UI

END
