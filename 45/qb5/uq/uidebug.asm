	TITLE	uidebug.asm - User interface to debugger.
;*** 
;uidebug.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Debugging support.
;
;
;*******************************************************************************

	.xlist
	include		version.inc
	UIDEBUG_ASM = ON

	;Include twin interface headers
	include 	cw/version.inc
	include 	cw/windows.inc
	include 	cw/edityp.inc

	;Next, include QBI's headers
	includeOnce	architec
	includeOnce	context
	includeOnce	executor
	includeOnce	heap
	includeOnce	lister
	includeOnce	opmin
	includeOnce	opstmt
	includeOnce	parser
	includeOnce	qbimsgs
	includeOnce	scanner
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	uiint
	includeOnce	util
	includeOnce	uimenu			
	.list

;---------------------------------------------------------------------------
;
; The following demonstrates how the user interface code interacts with
; the executor code to handle watchpoints and watch expressions.
; We start the discussion in UserInterface with the following program loaded:
;   A=1
;   A=2
; The pcode for this program is:
;   opBol
;   opLit1
;   opIdSt(A)
;   opBol
;   opLit2
;   opIdSt(A)
;   opEndProg
; 
; - User presses single step key, causing UserInterface to exit
; - opBol executes, sees DEBUG_TRACE bit set in debugFlags, enters UserInterface
; - User selects Watchpoint... menu, CmdWatchAdd(WT_Watch) prompts the user
;   for an expression, parses it to pcode, and inserts it at the end of the
;   text table.
; - User selects Watch... menu, CmdWatchAdd(WT_WatchPoint) prompts the user
;   for an expression, parses it to pcode, and inserts it at the end of the
;   text table.
; - We now have with a WATCHPOINT of "A=1" and a WATCH EXPRESSION of "A" and
;   the pcode looks like:
;   opBol
;   opLit1
;   opIdSt(A)
;   opBol
;   opLit2
;   opIdSt(A)
;   opEndProg
;   opIdLd(A)
;   opLit1
;   opEQ
;   opWatchStop
;   opIdLd(A)
;   opWatchExp
;   opEot
; 
; - User presses single step key.  When UserInterface exits, it sees active
;   Watch expressions, and sets DEBUG_WATCH in debugFlags.
; - opLit1 and opIdSt(A) execute
; - opBol executes, sees bits set in debugFlags, enters UserInterface
; - UserInterface immediately dispatches to DebugWatch.  DebugWatch sees
;   iWatch (static counter variable) is UNDEFINED (meaning we weren't executing
;   Watch pcode), saves current pc in otxWatchSave, sets the
;   current pc to the 1st watch expression's pcode, sets iWatch to 0,
;   returns to UserInterface with flags telling it to dispatch.
; - opIdLd(A), opLit1, and opEQ execute.
; - opWatchStop executes, sees a TRUE (non-zero) argument on the stack, sets
;   DEBUG_BREAK in debugFlags.  opWatchStop then falls into opBol which sees
;   debugFlags non-zero and enters UserInterface.
; - UserInterface immediately dispatches to DebugWatch.  DebugWatch sees
;   iWatch is 0, bumps it to 1, sets the current pc to the 2nd watch
;   expression's pcode, returns to UserInterface with flags telling it to dispatch.
; - opIdLd(A) executes.
; - opWatch executes, saves value at [pWatchVal], opWatch then falls into
;   opBol which sees debugFlags non-zero and enters UserInterface.
; - UserInterface immediately dispatches to DebugWatch.  DebugWatch sees
;   iWatch is 1, restores the current pc to otxWatchSave,
;   returns to UserInterface.
; - UserInterface then sees DEBUG_BREAK is set, and would have stopped program
;   execution even if we weren't tracing.  UserInterface then interacts with
;   the user until the user gives another command.
; - User presses single step key.  When UserInterface exits, it sees active
;   Watch expressions, and sets DEBUG_WATCH in debugFlags.
; - opLit2 and opIdSt(A) execute
; - opEndProg executes, causing us to once again enter UserInterface
;
;---------------------------------------------------------------------------


assumes	DS,DATA
assumes	ES,NOTHING
assumes	SS,DATA

	EXTRN	B$BEEP:FAR
	EXTRN	B$IFOUT:FAR
	EXTRN	B$Pause:FAR

sBegin	DATA

	EXTRN	b$ErrNum:word		;error code for last runtime error
	EXTRN	b$CurFrame:word	;current frame ptr (for PTRACE)
	EXTRN	b$MainFrame:word	;base frame ptr

	EXTRN	fRecord:byte		;set TRUE is /RCD switch seen
	EXTRN	fPlayBack:word		;TRUE if we're playing back.
					; an event playback file.
	EXTRN	axMac:byte

	externB HelpFlags		;	uiint.inc/h

;Points to space allocated by CallsMenuInit, released by CallsMenuTerm
;
pbCallsRelease DW 0

;iWatch is used by several functions involved in drawing the content
; of the watch window.  It indicates which line in the window is currently
; being drawn (0 to n)
;
DbPub	iWatch
iWatch	DW	UNDEFINED

; iInstWatch is used to keep track of where an Instant Watch entry is
; in the tWatch table.  The value of iInstWatch is only significant if
; the value of cInstWatch is non-zero.
DbPub	iInstWatch			
iInstWatch DW	0			

;fRefreshWatch is set TRUE whenever a WATCH entry is added or removed.
;It causes us to refresh the WATCH window.
;
PUBLIC	fRefreshWatch
fRefreshWatch	DB	0

;pWatchVal points to valtyp/value structure to be filled in by Watch executors
PUBLIC		pWatchVal
pWatchVal	DW	0

;1 entry per watch expression - number of bytes in expression's title
tCbWatchTitle	DB (WATCH_MAX+1) DUP (?) ; "+1" is for Instant Watch

otxWatchSave	DW 0	;copy of grs.otxCur while WATCH pcode executes
errnumSave	DW 0	; copy of b$errnum while WATCH pcode executes
bpWatchSave	DW 0	;copy of bp for start of current WATCH expression
fDirectSave	DB 0	;copy of grs.fDirect while WATCH pcode executes

;Trace modes
;
TRACE_OFF	EQU 0
TRACE_HISTORY	EQU 1	;Debug/History menu item is ON
TRACE_ANIMATE	EQU 2	;Debug/Trace menu item is ON
TRACE_STEP	EQU 3	;F8 was pressed last time in UserInterface
TRACE_PSTEP	EQU 4	;F10 was pressed last time in UserInterface
TRACE_WATCHSTEP	EQU 5	;Used by watch to force watch code to execute.

traceMode	DB TRACE_OFF

pStepFrame	DW 0
nonStickyBpRs	DW 0
nonStickyBpLn	DW UNDEFINED

CHIST	EQU 20	;we will remember last 20 statements executed
CBHIST	EQU 4	;number of bytes in 1 history entry
bdHist	DB size BD DUP (0)

oHistNext	DW 0
oHistShow	DW 0
histFlags	DB 0
	FH_Full		equ	01H
	FH_NotEmpty	equ	02H

szRun	DB	"RUN",0

sEnd	DATA

sBegin	CODE

;Table of opcodes(1) which descanner inserts where it finds return
; addresses on the stack:
;
tOpNoList LABEL WORD
	opTabStart	NOLIST
	opTabEntry	NOLIST,opNoList1
	opTabEntry	NOLIST,opEot
sEnd	CODE

sBegin	UI
assumes	cs,UI


;**************************************************************************
; DoRunOrCont(ax:fFromStart)
; Purpose:
;	Setup so program will continue execution.
;	If can't continue then start at begining.
;
; Entry:
;	ax = fFromStart - if TRUE start program from the beginning.
; Exit:
;	fGotCmd is set TRUE (which terminates GetCmd)
;
;**************************************************************************
DoCont	PROC NEAR
	sub	ax,ax
DoCont	ENDP
	;fall into DoRunOrCont
cProc	DoRunOrCont,<NEAR>
cBegin
	or	ax,ax
	jne	FromStart
	call	fCanContUI
	jne	NotFromStart		;brif can continue

; FNextStmtDoesIO uses grs.otxCONT to determine if the next statement to
; execute will do output.  ContContext is called just before FNextStmtDoesIO.
; if grs.otxCONT is UNDEFINED, ContContext will
; set grs.otxCONT to the begining of the main module.
;
FromStart:
	call	CantCont		;sets [grs.GRS_otxCONT] to UNDEFINED
	PUSHI	ax,<dataOFFSET szRUN>
	call	DoCmd
NotFromStart:
	mov	[fGotCmd],sp
cEnd

;**************************************************************************
; SetTronTroff
; Purpose:
;	Set [traceMode] and set DEBUG_TRACE bit in [debugFlags] if not TRACE_OFF
;	Called by TRON and TROFF opcode executors after they set [fTraceOn]
; Entry:
;	[fTraceOn] = non-zero if TRON is active
;	[fHistOn] = non-zero if HISTORY is active
;
;**************************************************************************
PUBLIC	SetTronTroff
SetTronTroff PROC FAR
	.errnz	TRACE_ANIMATE - 2
	mov	ax,TRACE_ANIMATE
	cmp	[fTraceOn],ah
	jne	NoTrace			;brif TRON (Trace ON) is not active
	dec	ax			;al = TRACE_HISTORY
	.errnz TRACE_HISTORY - 1
	cmp	[fHistOn],ah
	jne	NoTrace			;brif History ON is not active
	dec	ax			;al = TRACE_OFF
	.errnz TRACE_OFF
NoTrace:
	call	SetTraceMode
	ret				;can't fall into SetTraceMode, far->near
SetTronTroff ENDP

;**************************************************************************
; SetTraceMode
; Purpose:
;	Set [traceMode] and set DEBUG_TRACE bit in [debugFlags] if not TRACE_OFF
; Entry:
;	al = new trace mode (TRACE_STEP etc.)
;
;**************************************************************************
SetTraceMode PROC NEAR
	mov	[traceMode],al
	or	al,al
	je	GotTroff
	.errnz TRACE_OFF
	or	[debugFlags],DEBUG_TRACE
GotTroff:
	ret
SetTraceMode ENDP

;**************************************************************************
; CmdGo(fFromStart)
; Purpose:
;	Continue or start program execution taking into account trace state.
;	This procedure is called in response to the menu items `Run/Run' and
;	`Run/Continue'
;
; Entry:
;	fTraceOn - non-zero if TRON is active
;	fFromStart - TRUE if we want to RUN else we want to CONT.
;
;**************************************************************************
cProc	CmdGo,<PUBLIC,NEAR>
	parmW	fFromStart
cBegin
	call	SetTronTroff		;setup based on [fTraceOn]
	mov	ax,[fFromStart]
	call	DoRunOrCont		;execute a RUN or CONT command
cEnd

;**************************************************************************
; CmdSetNextStmt()
; Purpose:
;	Reset instruction pointer to statement at cursor.
;
;**************************************************************************
cProc	CmdSetNextStmt,<PUBLIC,NEAR>,<si,di>
cBegin
	call	TxtDescan		;descan to SS_PARSE
	call	GetEditLine		;ax = current edit line (from EditMgr)
	push	ax
	call	OtxOfLn 		;ax = text offset to start of line
	xchg	ax, bx			; bx = otx of pcode to skip.
	call	TxtSkipOpFar		;ax = otx beyond the opBol[xx] opcode
					;We always enter direct mode after
					;execution of the opBol
	xchg	si,ax			;si = otx
	mov	di,[grs.GRS_oRsCur]	;di = oRs of current text table
	test	[txdCur.TXD_flags],FTX_mrs
	je	NotInDefFn		;brif we're in a sub/function window
	push	si			;pass otx
	call	OPrsOfOtx		;ax = otx of DEF FN
	inc	ax			;test for UNDEFINED
	je	NotInDefFn		;brif we're in main-level code
	dec	ax			;ax = oPrs for DEF FN
	or	ah,80h			;ax = oRs of DEF FN
	xchg	di,ax			;di = oRs of DEF FN
NotInDefFn:
	call	NeedContContext		;grs.oRsCur=grs.oRsCONT or grs.oMrsMain
					;if no main module, uierr is set
	je	NxtStmtExit		;brif no main module
	cmp	di,[grs.GRS_oRsCur]
	je	BranchOk		;brif branching within text table
BadBranch:
	PUSHI	ax,MSG_BadNextStmt	;"Cannot cross module/procedure boundary"
	call	SetUiErr
	jmp	SHORT NxtStmtExit

BranchOk:
	mov	[grs.GRS_otxCONT],si
	call	DrawDebugScr		;so new current stmt will be hilighted
NxtStmtExit:
cEnd

;**************************************************************************
; CmdRestart()
; Purpose:
;	Restart program. Like a Step but always starts at begining of program.
;
;**************************************************************************
cProc	CmdRestart,<PUBLIC,NEAR>
cBegin
	mov	al,TRACE_STEP
	call	SetTraceMode
	mov	ax,sp			;ax = TRUE (non-zero)
	call	DoRunOrCont
cEnd

;**************************************************************************
; PStepReset
; Purpose:
;	Called whenever executor resets stack pointer.  This guarentees
;	that DebugTrace will stop the next time it is called (even for F10).
;
;**************************************************************************
cProc	PStepReset,<PUBLIC,FAR>
cBegin
	mov	[pStepFrame],0
cEnd

;**************************************************************************
; CmdStep(fPStep)
; Purpose:
;	Setup to execute the next program statement.
;	This routine is called in response to the Step and PStep
;	accelerator keys.
;
; Entry:
;	fPStep - if TRUE count procedure calls as one statement (F10).
;
; Exit:
;	PStepFrame, debugFlags, traceMode, fGotCmd are all updated
;
;**************************************************************************
cProc	CmdStep,<PUBLIC,NEAR>
	parmW	fPStep
cBegin
	call	ContContext		;activate program counter's context
	call	SkipStop		;skip past opBreakPoint or opStStop

	mov	ax,[grs.GRS_otxCONT]
	inc	ax			;test for UNDEFINED
	je	UseBp			;if Can't continue, stop at next entry
	mov	ax,[b$CurFrame] 	;get current frame
	mov	cx,[pGosubLast] 	;get gosub stack value
	jcxz	UseBp			;zero, use current frame as frame

	cmp	cx,ax			
	ja	UseBp			; brif no gosubs for this frame

	xchg	ax,cx			;else use top of stack
UseBp:
	mov	[pStepFrame],ax 	;and move it in

	mov	al,TRACE_STEP
	cmp	[fPStep],FALSE
	je	CsNotPStep		;brif F8 was pressed (step into proc)
	.errnz TRACE_STEP - 3
	.errnz TRACE_PSTEP - 4
	inc	ax			;al = TRACE_PSTEP (F10=step around proc)
CsNotPStep:
	call	SetTraceMode
	call	DoCont			;continue program execution
cEnd

;**************************************************************************
; CmdToggleBp()
; Purpose:
;	Toggles the breakpoint setting of the current line.
;	Note: will only be called if the current window contains code.
;
; Exit:
;	If an edit window is active a breakpoint is set/reset at the
;	current line.
;	ax active edit line (UNDEFINED if none)
;
;**************************************************************************
cProc	CmdToggleBp,<PUBLIC,NEAR>
cBegin
	call	UiGrabSpace
	call	GetEditLine		;ax = current edit line (from EditMgr)
	push	ax			;save for return value
	inc	ax			;test for UNDEFINED
	je	CsNoEditLine
	dec	ax
	push	ax			;pass to ToggleBp
	call	ToggleBp
	mov	[uierr],ax		;store possible error code
	call	DrawDebugScr		;Refresh any lines containing
					;this breakpoint
CsNoEditLine:
	call	UiReleaseSpace
	pop	ax			;ax = edit line
cEnd

;**************************************************************************
; CmdGoUntilHere()
; Purpose:
;  Sets a non-sticky breakpoint at the current line and runs until it
;  gets there. When the breakpoint is reached it will automatically be cleared.
;  Note: will only be called if the current window contains code.
;
;**************************************************************************
cProc	CmdGoUntilHere,<PUBLIC,NEAR>
cBegin
	call	UiRsActivateWnd 	;activate active window's context
	test	[mrsCur.MRS_flags2],FM2_Include OR FM2_NoPcode
	jne	CgExit			;brif not a pcode window
	call	GetEditLine		;get line number
	cCall	OtxOfLn,<ax>		;get otx
	cCall	fBpSet,<ax>		;see if bp is set
	or	ax,ax			
	jnz	CgGo			;do nothing if bp already set
	call	CmdToggleBp		;set break point on current line
	inc	ax			;test for UNDEFINED
	je	CgExit			;brif no active edit line
	dec	ax			;restore ax = line number
	mov	[nonStickyBpLn],ax	;remember to reset break point
	mov	ax,[grs.GRS_oRsCur]
	mov	[nonStickyBpRs],ax
CgGo:
	call	ContContext		;activate program counter's context
	call	SkipStop		;skip past the opBreakPoint
	xor	ax,ax
	cCall	CmdGo,<ax>		;continue program execution
CgExit:
cEnd

;**************************************************************************
; ClrNonStickyBp()
; Purpose:
;	Called on entry to GetCmd()
;	Clears any breakpoint set by CmdGoUntilHere().
; Exit:
;	Context may be switched to another text table.
;
;**************************************************************************
cProc	ClrNonStickyBp,<PUBLIC,NEAR>
cBegin
	mov	ax,[nonStickyBpLn]
	inc	ax			;test for UNDEFINED
	je	ClrNoBp
	dec	ax
	push	ax			;pass to ToggleBp
	push	[nonStickyBpRs]		;pass to UiRsActivate
	call	UiRsActivate
	call	ToggleBp
	mov	[nonStickyBpLn],UNDEFINED
	call	DrawDebugScr		;Refresh any lines containing
					;this breakpoint
ClrNoBp:
cEnd


;**************************************************************************
; DebugError()
; Purpose:
;  Setup to report a runtime error.
;
; Exit:
;  uierr and txtErr structure are setup so error will be reported in GetCmd.
;  Returns mask which tells UserInterface to stop for commands.
;
;**************************************************************************
PUBLIC	DebugError
DebugError PROC NEAR
	mov	ax,[b$ErrNum]
	cmp	ax,MSG_GoDirect
	je	DeExit			;Ignore MSG_GoDirect - it is used
					; when user presses CANCEL button
					; in some dialog.  Easy way to
					; back all the way out of the dialog.
	or	HelpFlags,HLP_RTERR	; this error came from execution

	;Setup so GetCmd() will call ReportError()
	mov	dx,ER_UE		;dx = max standard runtimer error code
	cmp	ax,dx			;see if error was generated by user
					; via ERROR(x) statement
	jbe	StdRtErr		;brif std runtime error code
	xchg	ax,dx			;ax = "Unprintable error"
StdRtErr:
	mov	[uierr],ax		;error will be reported by GetCmd
					; calling ReportError()
	mov	al,[grs.GRS_fDirect]
	mov	[txtErr.TXER_fDirect],al
	mov	ax,[grs.GRS_oRsCur]
	mov	[txtErr.TXER_oRs],ax
	mov	ax,[grs.GRS_otxCur]
	mov	[txtErr.TXER_otx],ax
	mov	[txtErr.TXER_oSrc],0
DeExit:
	mov	ax,FDM_GetCmd
	jmp	SHORT TrOffExit
DebugError ENDP

;**************************************************************************
; DebugStop()
; Purpose:
;  Setup to report stop statement.
;
; Exit:
;  returns mask which tells UserInterface to show the current statement
;    and stop for commands.
;
;**************************************************************************
PUBLIC	DebugStop
DebugStop PROC NEAR
	mov	ax,FDM_GetCmd OR FDM_ShowStmt
	jmp	SHORT TrOffExit
DebugStop ENDP

;**************************************************************************
; DebugEnd()
; Purpose:
;  Called when the program reaches end-of-pcode or END statement
;
; Exit:
;  returns mask which tells UserInterface to stop for commands.
;
;**************************************************************************
PUBLIC	DebugEnd
DebugEnd PROC NEAR
	cmp	[fDebugScr],FALSE
	jne	ShowDeb			;brif debug screen is visible
	cmp	[fRecord],FALSE
	jne	ShowDeb			;brif recording a playback file
	; brif not reading a playback file.  Since RUNTIME consumes the
	; keystroke in B$Pause, this would hang the playback file up.
	cmp	[fPlayBack],FALSE
	jne	ShowDeb

	; The user has just finished running a program up to an END stmt.
	; The output screen is still visible.  Display the prompt:
	; 'Press any key to return to editor'
	; on the bottom line of the output window and wait for the
	; user to press a key.
	
	call	B$Pause
ShowDeb:
	cmp	[grs.GRS_fDirect],FALSE
	jne	DeDirMode		;brif end-of-direct mode execution
	call	EnsShowDebugScr		;init debug screen, make it visible
	call	DrawDebugScr		;turn off old current stmt highlight
DeDirMode:
	mov	ax,FDM_GetCmd
TrOffExit:
	mov	[traceMode],TRACE_OFF
	ret
DebugEnd ENDP

;**************************************************************************
; DebugTrace()
; Purpose:
;   This is invoked when we enter UserInterface with DEBUG_TRACE set in
;   debugFlags.
;   Setup to show current statement and depending on the trace mode stop
;   for commands.
;
; Entry:
;   traceMode = TRACE_HISTORY if we are to do nothing more than record
;		   the pcode address of the instruction for History Playback
;	        TRACE_ANIMATE if we are to show stmt and exit direct mode,
;               TRACE_STEP if we are stopping for each statement (F8),
;               TRACE_PSTEP if we are stepping around procedure calls (F10).
;
; Exit:
;   The current statement's location is recorded in the history buffer.
;   Returns mask to tell UserInterface to show the current statement
;   and/or tell it to stop and interact with the user.
;
;**************************************************************************
cProc	DebugTrace,<PUBLIC,NEAR>,<di>
cBegin
	call	FExecutable		;see if next stmt to be executed
					; contains any executable code
	jne	ItsExecutable		;brif stmt is executable
	sub	di,di			;tell UserInterface to take no action
J1_TrSetTraceFlag:
	jmp	SHORT TrSetTraceFlag

ItsExecutable:

;set debugFlags, traceMode, fGotCmd, and al (return value) as follows:
;switch (traceMode)
;  case TRACE_OFF:
;	fGotCmd remains 0
;       return al = 0
;  case TRACE_HISTORY:
;	fGotCmd remains 0
;       return al = 0
;  case TRACE_ANIMATE:
;	if alternateListWindow is showing new oRs, activate it
;	if fCantCont, directModeCmd = RUN
;	fGotCmd = TRUE
;       return al = FDM_ShowStmt
;  case TRACE_PSTEP:
;	if ([pGosubLast] != 0 && ([pGosubLast] < [pStepFrame])) ||
;	   ([pGosubLast] == 0 && ([b$CurFrame] < [pStepFrame]))
;	   fGotCmd = TRUE
;          return al = 0
;	else
;	   fall into case TRACE_STEP
;  case TRACE_WATCHSTEP:
;  case TRACE_STEP:
;	fGotCmd remains 0
;	traceMode = TRACE_OFF
;	return al = FDM_GetCmd OR (case WATCHSTEP ? 0 :FDM_ShowStmt)
;end switch
;if traceMode != TRACE_OFF
;   debugFlags != DEBUG_TRACE
;
	sub	di,di			;tell UserInterface to take no action
	sub	cx,cx
	mov	cl,[traceMode]		;cx = traceMode
	dec	cx
	.errnz	TRACE_OFF
	js	SHORT TrSetTraceFlag	;brif traceMode == TRACE_OFF
	.errnz	TRACE_HISTORY - 1
	je	TrSetTraceFlag		;brif traceMode == TRACE_HISTORY
	loop	TrNotAnim		;brif traceMode != TRACE_ANIMATE
	.errnz	TRACE_ANIMATE - 2
	sub	ax,ax
	push	ax
	call	CmdGo
TrShow:
	or	di,FDM_ShowStmt
	jmp	SHORT TrSetTraceFlag

TrNotAnim:
	loop	TrNotStep		;brif traceMode != TRACE_STEP
	.errnz	TRACE_STEP - 3
	.errnz	TRACE_WATCHSTEP - 5
TrStep:
	mov	di,FDM_ShowStmt		
TrWatchStep:				
	or	di,FDM_GetCmd		
	mov	[traceMode],TRACE_OFF
	jmp	SHORT TrSetTraceFlag	

	.errnz	TRACE_PSTEP - 4
;traceMode == TRACE_PSTEP
TrNotStep:
	loop	TrWatchStep		
	mov	ax,[b$CurFrame] 	;get current frame
	mov	cx,[pGosubLast] 	;get gosub stack value
	jcxz	TrUseBp			;zero, use current frame as frame
	
	cmp	cx,ax			
	ja	TrUseBp 		; brif no gosubs for this frame

	xchg	ax,cx			;else use top of stack
TrUseBp:
	cmp	[pStepFrame],ax 	;compare, and if pstepframe
	jbe	TrStep			;watermark is below or equal, step

TrSkip:
	call	DoCont			;continue program execution
;di = FDM_xxx actionMask return value
TrSetTraceFlag:
	cmp	[traceMode],TRACE_OFF
	je	TrExit			;leave trace flag 0 if not tracing
	or	[debugFlags],DEBUG_TRACE
TrExit:
	xchg	ax,di			;ax = return value
cEnd	;DebugTrace





;**************************************************************************
; HistReset()
; Purpose:
;   This is called when we Scan/Descan a program to flush the history
;   buffer.
;
;**************************************************************************
cProc	HistReset,<PUBLIC,FAR>
cBegin
cEnd






;**************************************************************************
; CmdWatchDelAll()
; Purpose:
;	Called when 'View/Watch Delete' menu is selected.
;	Delete all active WATCH expressions
;
;**************************************************************************
cProc	CmdWatchDelAll,<PUBLIC,FAR>
cBegin
WDelLoop:
	call	WatchInfoUI		;make sure cWatch is up-to-date
	mov	bx,[cWatch]
	dec	bx
	js	WDelExit		;brif no watch expressions left
	cCall	WatchDel,<bx>		;delete watch expression [bx]
	jmp	SHORT WDelLoop

WDelExit:
cEnd

;**************************************************************************
; WatchDeleted
; Purpose:
;	Called by Text Mgr whenever a Watch expression is deleted.
;	It releases 1 line from the Watch Window in the Debug Screen
;	and remembers to redraw the debug screen.
;
;**************************************************************************
cProc	WatchDeleted,<PUBLIC,FAR>
cBegin
	call	WatchRelease		;release any existing WATCH values
	call	DrawDebugScr		; remember to redraw screen
cEnd

;**************************************************************************
; WatchInfoUI
; Purpose:
;	Given an index for a watch expression, load registers with info
;	about that watch expression.
; Entry:
;	iWatch = watch index (0 to n)
; Exit:
;	ax = oRs for text table containing expression
;	bx = pointer to 1 byte valTyp, 8 byte value structure
;	cx = otx to start of expression
;	[cWatch] = number of watch expressions in all loaded modules
;	grs.fDirect is set to FALSE
;
;**************************************************************************
PUBLIC	WatchInfoUI
WatchInfoUI PROC NEAR
	mov	bx,[iWatch]
WatchInfoUI ENDP
WatchInfoBx PROC NEAR
	call	WatchInfo
	ret
WatchInfoBx ENDP

;**************************************************************************
; WatchName
; Purpose:
;	Fill bufStdMsg with the title of a watch expression.
;	The text table containing the watch expression is also activated.
; Entry:
;	ax = oRs for watch pcode
;	cx = otx for watch pcode
; Exit:
;	entry's register set is activated
;	bufStdMsg = '<context> <expression>: '
;	bx = &bufStdMsg, ax = #bytes in message
;
;**************************************************************************
cProc	WatchName,<PUBLIC,NEAR>,<si>
	localV	bdBuf,<size BD>
cBegin
	dec	cx
	dec	cx			;back up to opEndProg or prev opWatch
					; ListLine treats these like opBol
	mov	si,cx			;si = otx for expression

	push	ax			;preserve ax
	push	ax			;pass oRs to UiRsActivate
	call	UiRsActivate
	pop	ax			;ax = oRs

	push	ax			;pass oRs to GetRsName
	mov	al,RSN_fIndent
	push	ax			;pass GetRsName flags
	mov	al,20d
	push	ax			;cbMax = 20
	call	GetRsName		;fill bufStdMsg with name
					;ax = cbName
	mov	bx,dataOFFSET bufStdMsg ;bx points to buffer being filled
	add	bx,ax			;ax points beyond end of context's name
	mov	BYTE PTR [bx],' '
	inc	bx			;bx points to dst for expression's name
	mov	dx,CB_bufStdMsg - 3	;dx = max chars in context + expression
	DbAssertRel CB_bufStdMsg,ae,40d,UI,<WatchName: msg buffer too small>
	sub	dx,ax			;dx = max chars left for expression
	mov	[bdBuf.BD_cbLogical],dx ;pass cbMax to ListLine
	mov	[bdBuf.BD_pb],bx	;pass ptr to buffer to ListLine
	push	bx			;save ptr to expression's name
	lea	bx,bdBuf
	push	si			;pass otx to ListLine
	push	bx
	call	ListLine		;ax = #bytes listed
	pop	bx			;bx points to szExpression
	add	bx,ax			;bx points to 0-byte terminator
	mov	ax,dataOFFSET bufStdMsg + 40d
	cmp	bx,ax
	jb	LengthOk		;brif length of context+name < 40
	xchg	bx,ax			;truncate length of name to 40
					; 20 for context, 20 for expression
LengthOk:
	mov	WORD PTR [bx]," :"	;store ": " at end of expression
	inc	bx
	inc	bx			;return ax = new cbMsg
	mov	BYTE PTR [bx],0		;store 0-byte terminator
	mov	ax,dataOFFSET bufStdMsg ;ax points to start of name
	xchg	ax,bx			;bx points to start of name (ret value)
					;ax points to end of name
	sub	ax,bx			;ax = length of name (return value)
cEnd

;**************************************************************************
; WatchNameId(idWatch)
; Purpose:
;	Fill bufStdMsg with the title of a watch expression.
;	The text table containing the watch expression is also activated.
; Entry:
;	idWatch = id of watch expression (0..WATCH_MAX)
; Exit:
;	entry's register set is activated
;	bufStdMsg = '<context> <expression>: '
;
;**************************************************************************
cProc	WatchNameId,<PUBLIC,NEAR>
	parmW	id
cBegin
	mov	bx,[id]
	call	WatchInfoBx		;ax = oRs, cx = otx for id
					;bx = ptr to valTyp, value
	call	WatchName		;fill bufStdMsg with title (id)
					;bx = pbMsg, ax = cbMsg
cEnd

;**************************************************************************
; WatchPrint
; Purpose:
;	Display some information in the Watch window
;
; Entry:
;	[iWatch] = 0 to n and identifies an entry in the watch window
;	bx = pbMsg
;	ax = cbMsg
;	cx = ALERT flag (0 for normal, non-zero for WatchBreak value)
;	dx = the type of information as follows:
;	     0:  expression's title
;	     1:  expression's value
;
;**************************************************************************
cProc	WatchPrint,<PUBLIC,NEAR>,<si,di>
	localV	rrc,<size _ARC>
cBegin

	;preserve window's current attribute, set it to Watch Window's attr
	xchg	si,bx			;si = pbMsg
	mov	di,isaWatchWindow	; [17] di = current isa
	jcxz	NotAlert
	mov	di,isaWatchpointHilite	
NotAlert:
	mov	bx,dataOFFSET tCbWatchTitle
	add	bx,[iWatch]		;bx points to cbTitle field
	or	dx,dx
	je	Type0			;brif not printing Watch Value
	mov	dl,[bx]			;dx = 1 column beyond title
	jmp	SHORT DisplayIt

Type0:
	mov	[bx],al			;tCbWatchTitle[iWatch] = cbMsg

;ax = byte count
;dx = 1st column to write
;si = ptr to string to write
;
DisplayIt:
	mov	cx,80d
	mov	bx,ax
	add	bx,dx
	sub	bx,cx
	jbe	Fits
	sub	ax,bx
	jb	WpExit			;if new starting column is negative
Fits:
	PUSHI	cx,<dataOFFSET wndDebug>
	push	dx			;pass rx
	push	[iWatch]		;pass ry
	push	si			;pass ptr to string
	push	ax			;pass cch (byte count)
	push	di			;pass isa
	call	TextOut

WpExit:
cEnd



;**************************************************************************
; WatchValue
; Purpose:
;	Called by DebugWatch and DrawWatch to list
;	an expression's value into a buffer in ASCII format.
;
;	[23] Modified to not call WatchPrint (just list to buffer).
;
; Entry:
;	bx points to 1 byte valTyp, 8 byte value structure
;	[iWatch] = watch id (0..n)
; Exit:
;	bx - points to value in display (ASCII) format
;	ax - cb - length of string pointed to by bx.
;	cx - fAlert - TRUE if watch is a watchpoint.
;
;**************************************************************************
DbPub	WatchValue			
cProc	WatchValue,<NEAR>
cBegin
	mov	al,[bx]			;al = value type
	inc	bx			;bx points to value
	xor	ah,ah			;ax = 0 if no value
	or	al,al
	je	NoValue
	js	WatchErr		;brif value = standard error code
	cmp	al,12h
	je	WValPoint		;brif got watch point
	cmp	al,24h
	je	WValStr			;brif got string
	push	ds
	pop	es			;es = DGROUP (for B$IFOUT)
	;inputs to B$IFOUT are bx->number, al = type, es=ds
	call	B$IFOUT 		;bx = adr of ascii string
					;ax = byte count
	jmp	SHORT WValNoAlert

WValStr:
	mov	ax,[bx.SD_cb]		;ax = byte count for WatchValue
	mov	bx,[bx.SD_pb]		;bx = ptr to WatchValue
;bx = pbMsg, ax = cbMsg
WValNoAlert:
	sub	cx,cx			;normal, not Alert attribute
	jmp	SHORT WValPrint

WatchErr:
	sub	cx,cx			;cx = FALSE (no alert)
	mov	ax,[bx]			;ax = MSG_xxx (standard error code)
	jmp	SHORT PrintStdMsg

WValPoint:
	sub	cx,cx			;cx = FALSE (no alert)
	mov	ax,MSG_FALSE
	cmp	WORD PTR [bx],0		;test boolean's value
	je	PrintStdMsg		;brif boolean expression is TRUE
	.errnz	MSG_TRUE - MSG_FALSE - 1
	inc	ax			;ax = MSG_TRUE
	dec	cx			;cx = TRUE (alert)
PrintStdMsg:
	push	cx			;preserve alert flag
	push	ax			;pass msg id to ListStdMsg
	call	ListStdMsg		;ax = byte count of msg
	pop	cx			;restore alert flag
	mov	bx,dataOFFSET bufStdMsg	;bx points to start of msg

;bx = pbMsg, ax = cbMsg
;cx = ALERT flag (0 for normal, non-zero for WatchBreak value)
WValPrint:
NoValue:
cEnd

;**************************************************************************
; DrawWatch()
; Purpose:
;	Called to draw Watch Window whenever the debug screen is being drawn.
;
;**************************************************************************
cProc	DrawWatch,<PUBLIC,NEAR>
cBegin
	mov	ax,[cWatch]		;ax = # watch active expressions
;if      FV_INSTWAT 		     
;	     sub     ax,[cInstWatch]	     ; Leave out any instant watch.
;endif   ;FV_INSTWAT		     
	mov	[iWatch],ax		;start with bottom WATCH line
DwLoop:
	mov	ax,[iWatch]
	dec	ax
	mov	[iWatch],ax
	js	DwExit			;brif end of watch expressions
					;i.e. if iWatch == UNDEFINED

	call	WatchInfoUI		;ax = oRs, cx = otx for iWatch
					;[bx] = ptr to valTyp, value
	push	bx			;save ptr to watch value
	call	WatchName		;fill bufStdMsg with title (iWatch)
					;bx = pbMsg, ax = cbMsg
	sub	dx,dx			;watchInfo = TITLE
	sub	cx,cx			;not ALERT
	call	WatchPrint		;display Watch title
	pop	bx			;bx = ptr to watch value
	call	WatchValue		;display Watch value on screen
					;bx = pbMsg, ax = cbMsg, cx = fAlert
	mov	dx,1			;print value, not title
	call	WatchPrint		
	jmp	SHORT DwLoop

DwExit:
cEnd

;**************************************************************************
; DebugWatch()
; Purpose:
;	This is called when we enter direct mode while any watch expressions
;	are active.  It updates the display by executing the watch opcodes.
; Note:
;	Currently, the only watch expressions which get updated are those
;	from the current context (i.e. if a SUB is active, only expressions
;	entered while the SUBs list window was active are updated).
;	If you are tempted to allow all watch expressions to be updated
;	no matter which oRs is active, be warned:  The scanner must
;	prevent function and DEF FN references in WATCH pcode, the
;	most recent BP for the sub must be loaded into BP temporarily
;	(so the most recent incarnation of the SUBs variables can be
;	accessed), error and event trapping would have to be disabled.
; Entry:
;	[iWatch] = UNDEFINED if UserInterface was entered after executing
;		   user pcode.
;	[iWatch] = 0..n if UserInterface was entered after executing
;		   watch expression [iWatch]
; Exit:
;	if next pcode to execute is Watch pcode
;	   iWatch is set to 0..n, indicating which Watch pcode is being executed
;	   grs.otxCur is set pointing to the watch pcode to execute
;	   grs.fDirect is set FALSE.
;	   grs.GRS_flags.FG_WatchActive is set
;	   returns TRUE (non-zero)
;	if next pcode to execute is user pcode
;	   iWatch is set to UNDEFINED
;	   grs.otxCur, grs.fDirect, grs.oRsCur are restored from the
;	      static values which were saved by DebugWatch when the last
;	      user pcode finished executing
;	   grs.GRS_flags.FG_WatchActive is clear
;	   returns FALSE
;	Condition codes are set based on value in ax
;
;**************************************************************************
;Note:
;	The code is organized so that the most common path makes no branches
;	so it is as fast as possible.
;

;NOTE:	This isn't the main entry --- DebugWatch is below.  This is here
;	for optimal short-jump space.
;	dl = [debugFlags]
UserPcode:
	;If we execute Watch pcode after each direct mode statement, we could
	;never execute RUN or SYSTEM if one of the Watchpoints was TRUE.
	;So, we just execute it at the end (opEot) of the direct mode stmt.
	
	test	dl,DEBUG_ERROR OR DEBUG_STOP
	jne	J1_DWatchFalse		;brif runtime error in user-pcode
	mov	ax,[grs.GRS_otxCur]	;capture cur execution state in ax,bl
	mov	bl,[grs.GRS_fDirect]
	or	bl,bl
	je	SaveState1		;brif not executing Direct Mode stmts
	test	[grs.GRS_flags],FG_allSsExecute
	je	J1_DWatchFalse		;can't execute WATCH pcode if some
					; text table isn't scanned to SS_EXECUTE
	inc	ax			;test for UNDEFINED
	je	SaveState		;brif just executed an dir mode opEot
J1_DWatchFalse:
	jmp	DWatchFalse		;brif user stmt had an error

SaveState:
	;preserve state of user's pcode before we begin executing WATCH pcode
	dec	ax			;restore ax = grs.otxCur
SaveState1:
	mov	[bpWatchSave],bp
	mov	[otxWatchSave],ax
	mov	ax,[b$errnum]		
	mov	[errnumSave],ax 	
	mov	[fDirectSave],bl
	sub	ax,ax			;initial iWatch is 0
	jmp	SHORT ExWatchPcode


cProc	DebugWatch,<PUBLIC,FAR>
cBegin
NextWatch:
	mov	dl,[debugFlags]
	or	dl,dl
	.errnz	DEBUG_CANT_CONT - 80h
	js	J1_DWatchFalse		;don't try to watch if can't continue
					; otherwise it screws up error reporting
	mov	ax,[iWatch]
	inc	ax			;test for UNDEFINED
	je	UserPcode		;brif just executed user pcode
;ax = index for next WATCH pcode to execute
ExWatchPcode:
	cmp	bp,[bpWatchSave]
	jb	WatchInUserFunc		;brif Watch expression is evaluating
					; a user-pcode function.  Don't execute
					; watch expressions recursively.
	test	[debugFlags],DEBUG_ERROR
	je	NoWatchErr		;brif no error in previous watch-pcode
	push	ax			;save iWatch for next watch expression
	and	[debugFlags],NOT DEBUG_ERROR
	call	WatchInfoUI		;bx = ptr to valTyp, value
					; for watch expression with error
	push	bx			;save ptr to watch entry
	push	bx			;pass parm = ptr to watch entry
	call	ZeroWatchVal		;release string descriptor if any
	pop	bx			;bx = ptr to watch entry
	mov	BYTE PTR [bx],0FFh	;valtyp = std error code
	sub	ax,ax
	xchg	ax,[b$ErrNum]
	mov	[bx+1],ax		;save error code
	pop	ax			;restore ax = iWatch for next expression
;ax = index for next WATCH pcode to execute
NoWatchErr:
	cmp	ax,[cWatch]
	jae	EndWatch		;brif last watch exp has been executed
	mov	[iWatch],ax
	xchg	bx,ax			;bx = iWatch
	call	WatchInfo		;ax = oRs for text table for iWatch
					;cx = otx to start of watch pcode
					;bx = ptr to valTyp, value
					;grs.fDirect = FALSE
	cmp	ax,[grs.GRS_oRsCur]
	jne	DOutOfScope		;brif exp isn't from current context
	mov	[pWatchVal],bx		;tell executor where to store value
	mov	[grs.GRS_otxCur],cx
	mov	[grs.GRS_fDirect],0	
	or	[grs.GRS_flags],FG_WatchActive
	DbAssertTst [grs.GRS_flags],ne,FG_allSsExecute,UI,<DebugWatch err1>
DWatchTrue:
	mov	ax,sp			;return TRUE - tells caller we're
					; about to execute watch-pcode
	or	ax,ax			;set condition codes for caller
DWatchExit:
cEnd

;bx points to entry in watch table for current watch expression
DOutOfScope:
	DbHalt	UI,<DOutOfScope: Watch code not enabled for LQB>	
	or	[debugFlags],DEBUG_ERROR
	jmp	NextWatch		;go get next watch expression

;Watch expression is evaluating a user supplied function
;If runtime error occurs within a watch expression, it is untrappable
; and reported.
;Otherwise, just return and continue executing the function.
;This causes TRACE to be disabled during WATCH pcode.
;
WatchInUserFunc:
	test	[debugFlags],DEBUG_ERROR OR DEBUG_STOP
	je	DWatchTrue		;brif watch expression had no error
	or	[debugFlags],DEBUG_CANT_CONT
					;don't allow user to continue after
					; error in Watch function
	jmp	SHORT DWatchFalse	;let error/breakpoint/stop within
					;user-function be reported.

EndWatch:
	cmp	[fDebugScr],FALSE
	je	NoDebugScr		;brif debug screen not visible
DebDrawLoop:
	call	WatchInfoUI		;bx = ptr to valTyp, value
	call	WatchValue		;display Watch value on screen
	mov	dx,1			;print value, not title
	call	WatchPrint		
	dec	[iWatch]
	jns	DebDrawLoop

;Restore state of user-pcode that was interrupted to execute watch-pcode
NoDebugScr:
	mov	al,[fDirectSave]
	mov	[grs.GRS_fDirect],al
	mov	ax,[errnumSave] 	;restore value of b$errnum from
	mov	[b$errnum],ax		;  before WATCH pcode execution
	mov	ax,[otxWatchSave]
	mov	[grs.GRS_otxCur],ax
	DbAssertRel bp,e,[bpWatchSave],UI,<DebugWatch: invalid bp>
DWatchFalse:
	and	[grs.GRS_flags],NOT FG_WatchActive
	mov	ax,UNDEFINED
	mov	[iWatch],ax
	inc	ax			;return FALSE - tells caller we're
	jmp	SHORT DWatchExit	;about to execute user-pcode
					;Condition codes are also set for caller


sEnd	UI

end
