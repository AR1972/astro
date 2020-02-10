	TITLE	txtdeb.asm - Text Mgr's Debugger support

;==========================================================================
;
;	Module:  txtdeb.asm - Text Mgr's Debugger support
;	System:  Quick BASIC Interpreter
;
;=========================================================================*/

	.xlist
	include		version.inc
	TXTDEB_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	heap
	includeOnce	lister
	includeOnce	opcontrl
	includeOnce	opid
	includeOnce	opmin
	includeOnce	opstmt
	includeOnce	opintrsc
	IncludeOnce	opaftqb4
	includeOnce	parser
	includeOnce	pcode
	includeOnce	qbimsgs
	includeOnce	scanner
	includeOnce	txtint
	includeOnce	txtmgr
	includeOnce	ui
	.list

	EXTRN	B$SASS:FAR

	assumes	DS,DATA
	assumes	SS,DATA
	assumes	ES,NOTHING

sBegin	DATA
PUBLIC		cWatch
cWatch		DW	0	;number of Watch expressions in system
pWatch		DW	0	;used during WatchInfo
fWatchBuild	DB	0	;non-zero if tWatch contains out-of-date info

;Watch table contains 1 entry for every watch expression/watchpoint
WT_ST		STRUC
WT_oRs		DW	0	;identifies text table with watch expression
WT_otxStart	DW	0	;text offset to start of watch expression
WT_otxEnd	DW	0	;text offset to end of watch expression
WT_filler	DB	0	;WT_value needs to be even byte aligned
WT_valTyp	DB	0	;type of value
WT_value	DB 8 DUP(?)	;holds R8,R4,I4,I2, or SD
WT_ST		ENDS

		EVEN
;runtime needs string descriptors on even byte boundaries.
tWatch		DB (WATCH_MAX+1) * SIZE WT_ST DUP(0) ; "+1" is for instant watch
tWatchEnd	LABEL WORD

sEnd	DATA

sBegin	CODE
;These opcodes mark the start and end of WATCH expressions
;
PUBLIC	tOpWatch
tOpWatch LABEL WORD
	opTabStart	W
	opTabEntry	W,opEndProg
	opTabEntry	W,opWatchExp
	opTabEntry	W,opWatchStop
	opTabEntry	W,opEot

;Table of all the opcodes (1) that are opBreakPoint
;
tOpBp	LABEL WORD
	opTabStart	BP
	opTabEntry	BP,opBreakPoint
	opTabEntry	BP,opEot

;These opcodes can cause screen I/O
; opcodes opPrintSpc opPrintTab opPrintComma opPrintSemi opPrintEos
; opPrintItemComma opPrintItemSemi and opPrintItemEos are all caught
; by executors, so we don't cause screen flicker on File I/O.
;
; END/EXIT FUNCTION/DEF can cause I/O if their caller included
; the function invocation within any I/O stmt, i.e.
;    PAINT (x,y),FNx
; This causes screen flicker for every END/EXIT DEF/FUNCTION/SUB but
; fNextStmtDoesIO() would have to add a lot of slow special-case code
; to catch the cases where the function was not within an I/O stmt.
;
;Certain control structure opcodes must also be included in the
;list since they branch past the bol/bos of other statements
;which may contain I/O opcodes.


tOpIO	LABEL WORD
	opTabStartAll	IO
	opTabEntry	IO,opStElseLabDirect
	;The reason EndDef, EndProc, ExitProc are listed here is
	;for the case when the user does something like:
	; PAINT (x,y),FNx(z)
	
	opTabEntry	IO,opStEndDef
	opTabEntry	IO,opEndSingleDef
	opTabEntry	IO,opStEndProc
	opTabEntry	IO,opStExitProc
	opTabEntry	IO,opStGotoDirect
	opTabEntry	IO,opStGosubDirect
	opTabEntry	IO,opStRunMain
	opTabEntry	IO,opStRunLabel
	opTabEntry	IO,opStBload1
	opTabEntry	IO,opStBload2
	opTabEntry	IO,opStCircle
	opTabEntry	IO,opStCircleColor
	opTabEntry	IO,opStCls
	opTabEntry	IO,opStColor
	opTabEntry	IO,opStDraw
	opTabEntry	IO,opStFiles0
	opTabEntry	IO,opStFiles1
	opTabEntry	IO,opStGraphicsGet
	opTabEntry	IO,opStGraphicsPut
	opTabEntry	IO,opStInput
	opTabEntry	IO,opStKey
	opTabEntry	IO,opStKeyMap
	opTabEntry	IO,opStLine
	opTabEntry	IO,opStLineColor
	opTabEntry	IO,opStLineStyle
	opTabEntry	IO,opStLineStyleColor
	opTabEntry	IO,opStLineInput
	opTabEntry	IO,opStLocate
	opTabEntry	IO,opStPaint2
	opTabEntry	IO,opStPaint3
	opTabEntry	IO,opStPalette0
	opTabEntry	IO,opStPalette2
	opTabEntry	IO,opStPaletteUsing
	opTabEntry	IO,opStPoke
	opTabEntry	IO,opStPreset
	opTabEntry	IO,opStPresetColor
	opTabEntry	IO,opStPset
	opTabEntry	IO,opStPsetColor
	opTabEntry	IO,opStRandomize1
	opTabEntry	IO,opStScreen
	opTabEntry	IO,opStShell0
	opTabEntry	IO,opStShell1
	opTabEntry	IO,opStSystem
	opTabEntry	IO,opStView
	opTabEntry	IO,opStView0
	opTabEntry	IO,opStViewPrint0
	opTabEntry	IO,opStViewPrint2
	opTabEntry	IO,opStViewScreen
	opTabEntry	IO,opStWidth2
	opTabEntry	IO,opStWindow
	opTabEntry	IO,opStWindow0
	opTabEntry	IO,opStWindowScreen
	opTabEntry	IO,opFnInkey_
	opTabEntry	IO,opFnInput_1
	opTabEntry	IO,opFnInput_2
	opTabEntry	IO,opFnPeek
	opTabEntry	IO,opFnPoint2
	opTabEntry	IO,opFnScreen2
	opTabEntry	IO,opFnScreen3
	opTabEntry	IO,opFnShell
	opTabEntry	IO,opStLoop		
	opTabEntry	IO,opStWend		
	opTabEntry	IO,opStNext		
	opTabEntry	IO,opStNextId		
	opTabEntry	IO,opStSelectCase	
	opTabEntry	IO,opStRandomize0	
	opTabEntry	IO,opBol
		IO_bosMin EQU IO_opBol
	opTabEntry	IO,opBolSp
	opTabEntry	IO,opBolInclude
	opTabEntry	IO,opBolIncludeSp
	opTabEntry	IO,opBolLab
	opTabEntry	IO,opBolLabSp
	opTabEntry	IO,opBos
	opTabEntry	IO,opBosSp
	opTabEntry	IO,opEot


;Table of opcodes used by ToggleBp to ensure that BP line is not a 
;CASE, CASE ELSE, or END CASE statement.

tOpCaseStmt LABEL WORD
	opTabStart	CASESTMT
	opTabEntry	CASESTMT,opStCase
	opTabEntry	CASESTMT,opStCaseTo
	opTabEntry	CASESTMT,opStCaseElse
	opTabEntry	CASESTMT,opStEndSelect
		CASESTMT_CASEMAX = CASESTMT_opStEndSelect
	opTabEntry	CASESTMT,opBol
	opTabEntry	CASESTMT,opBolLab
	opTabEntry	CASESTMT,opBolSp
	opTabEntry	CASESTMT,opBolLabSp
	opTabEntry	CASESTMT,opBolInclude
	opTabEntry	CASESTMT,opBolIncludeSp
	opTabEntry	CASESTMT,opEot

sEnd	CODE

sBegin	CP
assumes	cs,CP

;--------------------------------------------------------------
;		BASIC Debugging support functions
;--------------------------------------------------------------

;**************************************************************
; boolean fBpSet(otxBol)
; Purpose:
;	Return TRUE if indicated line has a breakpoint set.
; Entry:
;	otxBol = text offset to beginning of line opcode for line
;	grs.fDirect must be FALSE
; Exit:
;	al = ax = non-zero if breakpoint was set
;	dx = text offset to opBreakPoint, or where opBreakPoint
;	     could be inserted
;
;**************************************************************
cProc	fBpSet,<PUBLIC,FAR>
	parmW	otxBol
cBegin
	DbAssertRelB [grs.GRS_fDirect],e,FALSE,CP,<fBpSet: fDirect TRUE>
	mov	dx,[otxBol]		
	DbChk	Otx,dx			;error if bx > txdCur.bdlText.cbLogical

;bx = otx to opcode to skip
FbSkipLoop:
	xchg	ax,dx			;pass otx to TxtSkipOp in ax
	call	TxtSkipOp		;skip opBol[Lab][Sp] (or opStSub) opcode
	push	ax			;save ax = otx to potential opBreakPoint
	call	GetDescannedOpcode	;ax = opcode at text offset ax
	pop	dx			;dx = otx to potential opBreakPoint
	cmp	ax,opNoList1
	je	FbSkipLoop		;brif got an opNoList1
	cmp	ax,opStSub
	je	FbSkipLoop		;brif we want to skip a SUB opcode
	cmp	ax,opStFunction
	je	FbSkipLoop		;brif we want to skip a FUNCTION opcode
	cmp	ax,opStDefFn
	je	FbSkipLoop		;brif we want to skip a DEF FN opcode
	sub	cx,cx			;prepare to return FALSE
	cmp	ax,opBreakPoint
	jne	FbExit			;brif breakpoint is not set
	dec	cx			;return TRUE
FbExit:
	xchg	ax,cx			;return boolean in ax
cEnd

;**************************************************************
; GetDescannedOpcode
; Entry:
;	ax = text offset into current text table
; Exit:
;	ax = opcode
;
;**************************************************************
GetDescannedOpcode PROC NEAR
	call	GetWOtx			;ax = opcode at text offset ax
	cmp	[txdCur.TXD_scanState],SS_EXECUTE
	jne	GdNotExec		;brif not scanned to SS_EXECUTE
	push	ax
	call	DescanOpcode		;ax = descanned opcode
GdNotExec:
	and	ah,HIGH OPCODE_MASK
	ret
GetDescannedOpcode ENDP

;**************************************************************
; ToggleBp(ln)
; Purpose:
;	Sets or Reset a breakpoint at the indicated line.
;	For most lines, the opBreakPoint is inserted immediately
;	after the opBolXXX.  For SUB, FUNCTION, and DEF FNs,
;	the opBreakPoint is inserted immediately after the
;	opStSub/Function/DefFn opcode so when the procedure is
;	invoked, the breakpoint will be executed.
; Exit:
;	[17]if no error occurs then ax = 0 otherwise
;	[17] ax = txtErr.errcode = error code
;	cBreakpoints - Incremented or decremented.
;
;**************************************************************
cProc	ToggleBp,<PUBLIC,FAR>,<si>
	parmW	ln
cBegin
	call	TxtDescanCP
	push	[ln]
	call	OtxOfLn			;ax = text offset to start of line
	cCall	fBpSet,<ax>		;ax = 0 if line has no breakpoint
	mov	si,dx			;si = dx = otx to opBreakPoint
	or	ax,ax			
	jne	TglBpSet		;brif breakpoint is set

;if target statement of break point is CASE, CASE ELSE, or END SELECT
;then report an error
	push	si			
	PUSHI	ax,<CODEOFFSET tOpCaseStmt> 
	call	TxtFindOp		;dl = txtFindIndex
	cmp	dl,CASESTMT_CASEMAX	
	ja	SetBp			;brif hit end of line before case
	mov	[txtErr.TXER_fDirect],0	;error not in direct mode
	mov	[txtErr.TXER_oRs],UNDEFINED ;don't try to position cursor
	mov	ax,MSG_NoBpCase		;"no bp on case clause or end select"
	mov	[txtErr.TXER_errCode],ax 
	jmp	SHORT TgbErrExit	
SetBp:

;si = text offset to where BreakPoint is to be inserted
;make room for new breakpoint opcode by copying old text up in memory
;
	push	si
	PUSHI	ax,2
	call	TxtMoveUp
	je	TgbExit			;return FALSE if no memory

	push	si
	PUSHI	ax,opBreakPoint
	call	PutWOtx			;insert breakpoint pcode in text table

	;Update program counter, prs.otxDef fields for affected PRSs,
	; and linked lists through this text table's pcode
	
	mov	bx,si			;bx = updated otxInsert
	inc	bx
	inc	bx			;bx = offset beyond inserted pcode
	call	TxtInsUpdate
	jmp	SHORT TgbExit

;si = text offset to breakpoint to be deleted
TglBpSet:
	push	si
	inc	si
	inc	si
	push	si
	call	TxtDelete
TgbExit:
	xor	ax,ax			;return ax = 0 for no error
TgbErrExit:				
cEnd

;**************************************************************
; ClrBpAll()
; Purpose:
;	Reset all breakpoints in the loaded program.
;
; Input:
;	fCountBp - If fCountBp is TRUE then the breakpoints are only counted.
;
;**************************************************************
PUBLIC	ClrBpTxt
ClrBpTxt PROC NEAR
	push	si			;save caller's si
	call	TxtDescanCP
	sub	si,si			;start at beginning of text table
CbLoop:
	push	si
	PUSHI	ax,<CODEOFFSET tOpBp>
	call	TxtFindOp		;ax = offset to next opBreakPoint
					;dl = [txtFindIndex]
	cmp	dl,BP_opEot
	je	CbDone			;brif done with loop
	xchg	si,ax			;si = otxNext
	push	si
	lea	ax,[si+2]		;ax = offset beyond opBreakPoint
	push	ax
	call	TxtDelete		;delete the break point
	jmp	SHORT CbLoop

CbDone:
	mov	ax,sp			;return TRUE (non-zero)
	pop	si			;restore caller's si
	ret	
ClrBpTxt ENDP

PUBLIC	ClrBpAll
cProc	ClrBpAll,<FAR,PUBLIC>		
cBegin	ClrBpAll			
	mov	bx,CPOFFSET ClrBpTxt
	call	ForEachTxtTbl


cEnd	ClrBpAll			



;**************************************************************
; SkipStop
; Purpose:
;	If the next opcode to be executed is an opStStop
;	or opBreakPoint, skip it.  This is called by the
;	user interface for SingleStep and GoTillCursor
;	so the user doesn't have to step twice for a stmt
;	that has a STOP or BREAK-POINT.
; Entry:
;	grs.oRsCur, grs.otxCONT and grs.fDirect identify 
;	next opcode to be executed
; Exit:
;	grs.otxCONT is bumped by 2 if it was opStStop or opBreakPoint
;
;**************************************************************
cProc	SkipStop,<PUBLIC,FAR>
cBegin
	mov	ax,[grs.GRS_otxCONT]
	inc	ax
	je	DontSkipIt		;brif can't CONT
	dec	ax
	call	GetDescannedOpcode	;ax = opcode at text offset ax
	cmp	ax,opBreakPoint
	je	SkipIt
	cmp	ax,opStStop
	jne	DontSkipIt
SkipIt:
	add	[grs.GRS_otxCONT],2
DontSkipIt:
cEnd


;Table of opcodes skipped over while tracing
tNops	LABEL WORD
	DW opLabSp			; Moved these 2 to start of table
	DW opLab			; to make special case easier.
	DW opStConst
	DW opStData
	DW opStDeclare
	DW opStDefType
	DW opStRem 
	DW opQuoteRem
	DW opStShared
	DW opStStatic
	DW opStType
	DW opStEndType
	DW op_Static
	DW op_Dynamic
	DW op_Include
	DW opBol
	DW opBolSp
	DW opBolInclude
	DW opBolIncludeSp
	DW opBolLab
	DW opBolLabSp
	DW opBos
	DW opBosSp
	DW opEndProg
CW_TNOPS EQU ($-tNops) SHR 1

;**************************************************************
; boolean FAR FExecutable()
; Purpose:
;	Called by User Interface code to determine if we need to
;	show the tracing of the next statement.
;
;	WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING
;
;	Slime.	This routine is currently only called from
;	DebugTrace to determine whether or not we should stop
;	on the current line.  For this case we need to skip past
;	opLab[Sp] so we recognize that we need to break on lines
;	like "10 foo: print" or on included lines with line #'s
;	(which get opBolInclude + opLab).  If this routine is
;	ever called from anywhere else this will need to be
;	reexamined.
;
;	WARNING !!! WARNING !!! WARNING !!! WARNING !!! WARNING
;
; Entry:
;	grs.oRsCur, grs.otxCur identify next statment to be executed.
; Exit:
;	Returns false if next statement to be executed contains no
;	executable pcode.  Condition codes are set based on value in ax.
;
;**************************************************************
cProc	FExecutable,<PUBLIC,FAR>,<di>
cBegin
	DbAssertRelB [grs.GRS_fDirect],e,0,CP,<FExecutable: grs.fDirect=0>

	mov	ax,[grs.GRS_otxCur]
FExLoop:				
	push	ax			; save otx we're about to use
	call	GetDescannedOpcode	;ax = opcode at text offset ax
	mov	di,CPOFFSET tNops	;di -> 1st opcode in table to search
	mov	cx,CW_TNOPS
	push	cs
	pop	es
	repne scasw			;search for opcode ax in table pointed
					; to by es:di
	jnz	fExExit 		; brif no match (return AX != 0)
	sub	cx,(CW_TNOPS - 3)	; cx <= 0 if not special match
	jbe	fExMatch		; brif normal match
	pop	ax			; get otx we just used
	inc	cx			; cx = 2 if opLab, 3 if opLabSp
	inc	cx			; cx = 3 if opLab, 4 if opLabSp
	shl	cx,1			; cx = length of opLab[Sp] (6 or 8)
	add	ax,cx			; bump otx past opLab[Sp]
	jmp	short FExLoop		; try next opcode
fExMatch:				
	xor	ax,ax			; return false
fExExit:
	pop	cx			; clean up stack
	or	ax,ax			; set condition codes
cEnd


					; in EB it must be converted to a cProc
;**************************************************************
; boolean FAR fNextStmtDoesIO()
; Purpose:
;	Called by User Interface code to determine if we need to
;	show the Output Screen before tracing the next statement.
;	For example, the following would return TRUE where ^ represents
;	grs.otxCur:
;	   opBol^opPrintEos opBol
;	And the following would return FALSE:
;	  ^opBol opPrintEos opBol
;	   opBol opPrintEos^opBol
;	  ^opBol opStClose opBol
;	   opBol^opStClose opBol
;	   opBol opStClose^opBol
;	Because sometimes, the user interface calls this
;	when it is looking beyond the opBos.
; Entry:
;	grs.fDirect, grs.oMrsCur, grs.oPrsCur, grs.otxCur
;	  indicate the next statement to be executed.
;	If grs.fDirect is TRUE, this function assumes the
;	direct mode buffer is in SS_EXECUTE scan state.
;
; Exit:
;	returns TRUE if any opcodes in the next statement can
;	cause I/O to the screen/keyboard.
;
;**************************************************************
PUBLIC	fNextStmtDoesIO
fNextStmtDoesIO PROC FAR
	push	[grs.GRS_otxCur]
	PUSHI	ax,<CODEOFFSET tOpIO>
	call	TxtFindOp
	sub	ax,ax			;prepare to return FALSE
	cmp	dl,IO_bosMin
	jae	FnExit			;brif stmt can't perform I/O
	dec	ax			;return UNDEFINED (TRUE)
FnExit:
	ret	
fNextStmtDoesIO ENDP

;************************************************************
; WatchMoved
; Purpose:
;	Called every time text is inserted/deleted/scanned/descanned
;	to remember to build table 'tWatch' by scanning all pcode.
;
;************************************************************
PUBLIC	WatchMoved
WatchMoved PROC NEAR
	mov	[fWatchBuild],1
	ret
WatchMoved ENDP

;**************************************************************************
; WatchInfo
; Purpose:
;	Given an index for a watch expression, load registers with info
;	about that watch expression from table tWatch.
; Entry:
;	bx = watch index (0 to n)
; Exit:
;	ax = oRs for text table containing expression
;	cx = otx to start of expression
;	dx = otx to end of expression
;	bx points to 1 byte valTyp, 8 byte value structure
;	grs.fDirect is set to FALSE (because TxtFindNextOp is called)
;	[cWatch] = number of watch expressions in all loaded modules
;
;**************************************************************************
cProc	WatchInfo,<PUBLIC,FAR>,<si>
cBegin
	sub	ax,ax
	cmp	[fWatchBuild],FALSE
	je	GotWatchInfo
	test	[flagsTM],FTM_WatchPcode ;is there any watch pcode anywhere?
	je	GotWatchInfo		;brif not - get out quick
	SetfDirect al			;[19]turn off direct mode
	and	[flagsTM],NOT FTM_WatchPcode ;clear flag, gets reset after
					; WatchBuildNext if any pcode is found
	push	bx			;preserve iWatch

	mov	[fWatchBuild],al
	mov	[cWatch],ax
	mov	ax,dataOFFSET tWatch
	mov	[pWatch],ax
	mov	bx,CPOFFSET WatchBuildNext
	call	ForEachTxtTbl

	pop	bx			;bx = iWatch
	cmp	[cWatch],0		;was any watch pcode found?
	je	GotWatchInfo		;brif not
	or	[flagsTM],FTM_WatchPcode ;we have some watch pcode
GotWatchInfo:
	mov	cl,4
	shl	bx,cl			;bx = iWatch * 16
.errnz	(SIZE WT_ST) - 16
	add	bx,DATAOFFSET tWatch	;bx points to entry
	xchg	si,bx			;si points to watch entry
	lodsw				;ax = oRs of watch entry
.errnz	WT_oRs - 0
	push	ax
	lodsw				;ax = otxStart of watch entry
.errnz	WT_otxStart - 2
	push	ax
	lodsw				;ax = otxEnd of watch entry
.errnz	WT_otxEnd - 4
	xchg	dx,ax			;dx = otxEnd
	pop	cx			;cx = otxStart
	pop	ax			;ax = oRs
	xchg	bx,si			;restore caller's si
	inc	bx			;bx points to WT_valTyp entry
.errnz	WT_valTyp - 7
cEnd

;**************************************************************************
; WatchBuildNext
; Purpose:
;	Called for every text table by WatchInfo - finds WATCH pcode
;	and copies relevant information into table tWatch.
;
;**************************************************************************
cProc	WatchBuildNext,<NEAR>,<si,di>
cBegin
	sub	si,si			;start at beginning of table
	mov	di,[pWatch]
NwLoop:
	push	si			;pass otxCur to TxtFindNextOp
	PUSHI	ax,<CODEOFFSET tOpWatch>
	call	TxtFindOp		;ax = offset to next watch pcode
					;dl = [txtFindIndex]
	inc	ax
	inc	ax			;ax now points beyond opWatch opcode
					;i.e. it now ax points to expression
	xchg	si,ax			;si = otxNext, ax=otxPrev
	cmp	dl,W_opEot
	je	NwExit			;brif reached opEot
	cmp	dl,W_opEndProg
	je	NwLoop			;opEndProg just marks start of table,
					;doesn't mean we have a Watch exp
	inc	[cWatch]		;bump # watch expressions in system
	push	ds
	pop	es			;es = DGROUP (for stosw)
	push	ax			;save otxPrev to start of watch exp
	mov	ax,[grs.GRS_oRsCur]
	stosw				;save oRsCur in WT_oRs field
.errnz	WT_oRs - 0
	pop	ax			;ax = otxPrev
	stosw				;save in WT_otxStart field
.errnz	WT_otxStart - 2
	mov	ax,si			;ax = otxNext
	stosw				;save in WT_otxEnd field
.errnz	WT_otxEnd - 4
	inc	di			;di points to WT_valTyp field
	cmp	dl, W_opWatchStop	; Is this a watchpoint?
	jne	@F			
	mov	al, 12H 		; Yes, mark it in watch info.
	stosb				
	dec	di			
@@:					
.errnz	WT_valTyp - 7
	add	di,(SIZE WT_ST) - WT_valTyp	
	DbAssertRel di,be,<dataOFFSET tWatchEnd>,CP,<WatchInfo err 1>
	jmp	SHORT NwLoop

NwExit:
	mov	[pWatch],di		;save for next call
cEnd

;************************************************************
; ZeroWatchVal
; Purpose:
;	Zero a particular Watch value, releasing string if necessary
;	Called from uidebug.asm and WatchRelease below.
; Entry:
;	parm1 points to WT_valTyp field of entry in tWatch
;
;************************************************************
cProc	ZeroWatchVal,<PUBLIC,FAR>	
	ParmW	WatchEntry		
cBegin  ZeroWatchVal			
	mov	bx,[WatchEntry]		
	sub	ax,ax
	cmp	BYTE PTR [bx],24h	;test for string value
	mov	[bx],al	
	je	FreeString		;brif str descriptor needs to be freed
	.errnz WT_value - WT_valTyp - 1
	mov	[bx+1],ax		;zero value so if we later assign
	mov	[bx+3],ax		; won't think garbage is str descriptor
	jmp	SHORT NotString		; a string to this slot, string mgr

FreeString:
	push	ax			;push NULL string descriptor
	push	ax
	mov	ax,sp			;cant push sp, works different on 86/286
	push	ax			;pass ptr to NULL string descriptor
	.errnz WT_value - WT_valTyp - 1
	inc	bx			;bx points to WT_value field
	push	bx			;pass ptr to watch SD
	call	B$SASS			;release string
	pop	ax			;pop NULL string descriptor
	pop	ax
NotString:
cEnd	ZeroWatchVal			

;************************************************************
; WatchRelease
; Purpose:
;	Called before B$RUNINI (runtime initialization),
;	and whenever a watch expression is added or deleted.
;	It sets all values to 0 and releases any strings
;	owned by the WATCH table.
;
;************************************************************
cProc	WatchRelease,<PUBLIC,FAR>,<di>
cBegin
	mov	di,dataOFFSET tWatch
	add	di,WT_valTyp
ZeroLoop:
	cmp	di,DATAOFFSET tWatchEnd
	jae	ZeroDone
	push	di			;push watch entry
	call	ZeroWatchVal		;release it
	add	di,SIZE WT_ST
	jmp	SHORT ZeroLoop

ZeroDone:
cEnd

;************************************************************
; DebugReset
; Purpose:
;	Called by RunInit (just before we call B$RUNINIT) in
;	preparation for program execution.  It does the following:
;	- release any string descriptors owned by Watch table
;	- resets history buffer
; Exit:
;	Preserves grs.fDirect
;
;************************************************************
cProc	DebugReset,<PUBLIC,NEAR>
cBegin
	push	word ptr [grs.GRS_fDirect] ;preserve for caller
	call	WatchRelease		;release any existing WATCH values
					;causes tWatch strings to be released
	sub	bx,bx
	call	WatchInfo		;make sure watch info is up to date
					; since we can't build it after
					; program execution gets under way,
					; because it needs to call TxtFind...
	call	HistReset		;release History buffer
	PopfDirect ax			;restore input setting of this flag
cEnd

;************************************************************
; WatchAdd(fWatchType)
; Purpose:
;	Called whenever the user wants to add a WATCH expression
; Entry:
;	fWatchType = WT_Watch if watch expression is being added
;		     WT_WatchPoint if watch point is being added
;if	FV_INSTWAT
;		     WT_InstantWatch if instant watch is being displayed
;endif
;	ps.bdpSrc contains the 0-terminated source line to be parsed.
;	grs.oMrsCur and grs.oPrsCur identify the scope of the expression.
; Exit:
;	flagsTM.FTM_WatchPcode is set to indicate WatchPcode exists
;	Same as TxtDirect, except ax is meaningless on exit
;
;************************************************************
cProc	WatchAdd,<PUBLIC,FAR>,<si>
	parmB	fWatchType
cBegin

	call	WatchRelease		;release any existing WATCH values
	call	TxtDescanCP		;descan current text table to SS_PARSE
TwRetry:
	call	OtxDefTypeEot		;fill ps.tEtCur[] with default types
					; from end-of-text-table
					;While it is tempting to set
					; the default types to those of
					; grs.otxCONT, we can't do this
					; because if we descan to SS_RUDE,
					; rude-scanner would use def-types
					; of EOT.
	mov	[ps.PS_flags],PSF_fParseExp
	call	ParseLine
	jnc	TwNoParseErr		;brif ParseLine got no error

	;See if the parser wants us to try parsing this line again.  This can
	;happen when:
	; We saw something that made us need to ModuleRudeEdit, but part
	;     of the line's pcode had already been emitted in SS_PARSE
	; Variable manager could not add a variable, because variable heap
	;     was locked (because we can CONTinue).  Parser called AskCantCont
	;     and now wants us to try again (much easier than trying to call
	;     varmgr again from within parser).
	
	test	[ps.PS_flags],PSF_fRetry
	jne	TwRetry			;brif ParseLine wants us to try again
	test	[ps.PS_flags],PSF_UndoEdit
	je	TwErr			;brif user didn't say he wants to back
					; out of the edit while we were in
					; ParseLine (i.e. ParseLine called
					; AskCantCont)
	mov	ax,UNDEFINED
	jmp	SHORT TwErrCode		;tell caller to back out of operation

TwNoParseErr:
	sub	[ps.PS_bdpDst.BDP_pbCur],2
	sub	[ps.PS_bdpDst.BDP_cbLogical],2

	mov	ax,opWatchExp
	cmp	[fWatchType],WT_WatchPoint ; doing a watchpoint?
	jne	TwEmit			; no, emit opWatchExp
	mov	ax,opWatchStop		; emit opWatchStop for watchpoint
TwEmit:
	call	Emit16_AX
	mov	si,[txdCur.TXD_bdlText_cbLogical]
	dec	si
	dec	si			;si = otx to opEot (watch insert point)
	call	TxtInsert		;insert ps.bdpDst in text table at si
	mov	ax,ER_OM		;prepare to return out-of-memory error
	je	TwErrCode		;brif out-of-memory
					;else, ps.errCode = 0, return it
TwErr:
	mov	ax,[ps.PS_errCode]
	and	ah,(PSERR_fAsciiMsg + PSERR_errCode) / 100h
					;mask off parser internal flags
.errnz  PSERR_fAsciiMsg - 8000h		;caller assumes this is high bit
TwErrCode:
	mov	[txtErr.TXER_errCode],ax
	mov	[txtErr.TXER_oRs],UNDEFINED ;don't try to position cursor
	or	[flagsTM],FTM_WatchPcode ;We have watch pcode in system
cEnd

;**************************************************************************
; WatchDel
; Purpose:
;	Given an index for a watch expression, delete pcode for it
;	TxtDelete calls WatchDeleted which causes entry to be discarded
;	from DebugScreen
; Entry:
;	parm1 = watch index (0 to n)
;	grs.fDirect is set to FALSE
; Exit:
;	preserves bx
;
;**************************************************************************
cProc	WatchDel,<PUBLIC,FAR>,<bx>
	parmW	idWatch
cBegin
	mov	bx,[idWatch]
	push	bx			;save idWatch

	call	WatchInfo		;ax = oRs
	push	ax
	call	RsActivateCP		;activate the text table
	call	TxtDescanCP		;descan to SS_PARSE before delete pcode

	pop	bx			;bx = idWatch
	call	WatchInfo		;cx = otxStart, dx = otxEnd
	push	cx			;pass otxStart to TxtDelete
	push	dx			;pass otxEnd to TxtDelete
	call	TxtDelete		;delete the Watch pcode
cEnd

;**************************************************************************
;ChkWatchErr
;Purpose:
;	Called by SystemScan when an error is encountered.
;	Modifies txtErr if error is in Watch expression, so
;	error will be reported to user in better form.
;Entry:
;	txtErr structure contains error information
;Exit:
;	txtErr structure contains updated error information
;
;**************************************************************************
cProc	ChkWatchErr,<PUBLIC,NEAR>,<si>
cBegin
	sub	si,si
CwLoop:
	mov	bx,si			;bx = iWatch for next watch expression
	call	WatchInfo		;ax = oRs for expression's text table
					;bx points to 1 byte valTyp
					;cx = otx to start of expression
					;dx = otx to end of expression

	;we can't do this comparison before calling WatchInfo, because
	;calling WatchInfo can update cWatch
	
	cmp	si,[cWatch]
	jae	CwDone

	inc	si			;bump si for next watch expression
	cmp	ax,[txtErr.TXER_oRs]
	jne	CwLoop			;brif not oRs with error
	cmp	cx,[txtErr.TXER_otx]
	ja	CwLoop			;brif can't be expression of interest
	cmp	dx,[txtErr.TXER_otx]
	jbe	CwLoop			;brif isn't be expression of interest
	mov	BYTE PTR [bx],0FFh	;valtyp = std error code
	mov	ax,[txtErr.TXER_errCode]
	mov	[bx+1],ax		;save error code.  Causes it to be
					; displayed in watch window.
	mov	[txtErr.TXER_oRs],UNDEFINED
					;so we don't try to position the
					; cursor where the error is
CwDone:
cEnd

sEnd	CP

end
