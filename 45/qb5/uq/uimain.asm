	TITLE	uimain.asm - BASIC's top level interface to user interface.
;***
;uimain.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Main user interface, and support routines.
;
;
;*******************************************************************************

	include	version.inc

	UIMAIN_ASM = ON

	;Next, include COW's interface headers
	include cw/version.inc
	include cw/windows.inc
	include cw/edityp.inc

	;Next, include QBI's headers
	includeOnce	architec
	includeOnce	context
	includeOnce	executor
	includeOnce	heap
	includeOnce	parser
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	uiint
	includeOnce	uimenu	

assumes	ds,DATA
assumes	ss,DATA
assumes	es,NOTHING

	; external procedures used by uictl	
						
	externFP EnableMenuItem 		
	externFP CheckMenuItem			
	externNP fCanContUI			
	externNP GetEditLine			
	externFP SetBlinkBit
IFNDEF ROMBASIC
        externFP FCheckTandy1000
ENDIF

	;Runtime supplied functions used by this module:
	
	EXTRN	B$UnhookKbd:FAR
	EXTRN	B$hookKbd:FAR


sBegin	DATA

	externW fIsProgram 		
	externB fPasteOK		
	externB fSyntaxCheck		
	externW cWatch			
	externB fInsertMode		;EditMgr insert state.

	EXTRN	b$fCompErr:word	;non-zero if error was in compiled code
	EXTRN	fRefreshWatch:byte	;non-zero if WATCH window needs to
					; be refreshed

	externW	szDialogTitle		; * current dialog box title, or
					; 0 if no title.
;fUiActive is TRUE when user-interface keyboard and mouse interrupt
;handlers are active.
PUBLIC  fUiActive
fUiActive	dw	0
fUIInit 	db	0		; non-zero if UI Initialized.

;When we can CONTinue, and we execute a direct mode stmt that causes a ret
; adr to the direct mode buffer to be pushed on the stack (call/gosub/func),
; and then enter direct mode to save the otx of the called routine, we save the
; old grs.otxCont in otxContProg, so that after the direct mode call finishes,
; we can still CONTinue the stopped program.
;This variable gets set to UNDEFINED if we do any EDITs (because otxContProg
; isn't updated as pcode is moved by the text mgr and static-scanner).
;It is also set to UNDEFINED if we execute another direct mode stmt
; while there is a return address to the direct mode buffer on the stack,
; because we choose not to remember an arbitrary number of these, and
; it would be difficult for the user to keep track of them anyway.
;TxtDirect guarentees that if user enters a direct mode statement
; while there is a return address to the direct-mode-buffer on the
; stack (FG_RetDir), that CantCont will occur.
;
otxContProg	DW UNDEFINED

cGrab	db	0		;number of active callers of UiGrabSpace
sEnd	DATA

sBegin	UI
assumes cs,UI


;**************************************************************************
; UiInit
; Purpose:
;	In QB, the RUNTIME gets control first (since in stand-alone EXEs
;	there is no user-interface).  This is called after the runtime
;	has initialized.  It initializes the user interface.
; Entry:
;	cmdSwitches has flags set to indicate command-line switch settings
; Exit:
;	the file QB.INI is read
;	If command line didn't contain /RUN <filename>, the debug screen
;	   is shown.
;	AX nonzero if Out of Memory
;	
;**************************************************************************
cProc	UiInit,<PUBLIC,FAR>
cBegin
; Set colors at beginning so first screen writes are the same color.
	call	ReadQbIni
	call	B$UnhookKbd
IFNDEF ROMBASIC
        call    FCheckTandy1000         ;check for/install TANDY 1000 KBD
ENDIF
	call	CwInit
	call	B$HookKbd
	test	[cmdSwitches],CMD_SW_RUN ;/RUN filename given on command line?
	jnz	NoDebugScr		;  brif so, no debug screen
	call	EnsShowDebugScr
NoDebugScr:
	call	GrabSpace		;make sure we can successfully
	or	ax,ax			;  GrabSpace (ax = 0 if failed)
	not	ax			;0 -> -1 if failed, preserves flags
	jz	UiInitExit		;brif failure, exit with ax<>0
	call	ReleaseSpace		;release what we grabbed
	xor	ax,ax			;exit with ax = 0 for success
UiInitExit:				
	inc	fUIInit 		;indicate UiInit complete
cEnd

;**************************************************************************
; UiTerm
; Purpose:
;    Called just before we are about to leave the QB for good.
;    Calls COW and KKIF to tell it to terminate.
;
;    Also called before a SHELL is performed.  The runtime calls
;    this routine prior to SHELL, or at termination.  After a SHELL,
;    The runtime calls UiReInit so COW can re-initialize.
; Entry:
; Exit:
;	
;**************************************************************************
cProc	UiTerm,<PUBLIC,FAR>
cBegin
	cmp	fUIInit,0	; were we ever initialized?
	jz	UTExit		; brif not, exit now

	cCall	WriteQbIni	; write the qb.ini file
	cCall	CwTerm		
	mov	al, 1			
	cCall	SetBlinkBit,<ax>
UTExit:
cEnd

;**************************************************************************
; UiReInit
; Purpose:
;	In QB, the RUNTIME needs to reinitialize the user interface
;	at certain points (primarily after SHELL).  This entry point
;	allows the user interface to reestablish any state (such as
;	hooking interrupts) that is necessary.
; Entry:
;	none.
; Exit:
;	none.
;	
;**************************************************************************
cProc	UiReInit,<PUBLIC,FAR>
cBegin
cEnd

;**************************************************************************
; UiPause
; Purpose:
;    Added with [24].
;    Called before a SHELL is performed.  The runtime calls
;    this routine prior to SHELL, or at termination.  After a SHELL,
;    The runtime calls UiReInit so COW can re-initialize.
; Entry:
; Exit:
;	
;**************************************************************************
cProc	UiPause,<PUBLIC,FAR>
cBegin
cEnd

;**************************************************************************
; UserInterface()
; Purpose:
;  UserInterface() is the primary interface the user-interface
;  provides to the rest of BASIC.
;  It is called by the beginning-of-statement/line
;  executor when it sees bosMask & BOS_DEBUG non-zero,
;  by opEot and opStEnd (end-of-program), opBreakPoint, and opStStop.
;
;  For example, if tracing was active for the program:
;    CLS : PRINT
;    STOP
;  UserInterface would be invoked between the opcodes seperated by '^':
;    opBol ^ opStCls opBos ^ opBreakPoint ^ opStPrint opBol ^ opStStop ^ opEot ^
;
; Entry:
;  grs.oRsCur, otxCur identify the next statement to be executed.
;  If grs.otxCur == UNDEFINED, it means we've just executed an ExEot,
;     and thus cannot continue.  Otherwise, it points just beyond the
;     ExBos opcode which caused UserInterface to be called.
;  If the parser's input buffer (ps.bdpSrc) contains any text, that
;     text is executed as if the user entered it as a direct mode stmt.
;     This allows executors like RUN <filename> to load the file, then
;     re-invoke UserInterface after stuffing the command "RUN" in the
;     parser's input buffer.  This allows there to be only 1 control path
;     to scan all loaded text tables.
;
;  The actions performed by UserInterface() are determined by which
;  bits are set in debugFlags as follows:
;
;  DEBUG_EXEC_CMD - set by some executor like RUN <file>/CHAIN <file>,
;     which has loaded pcode, and now wants the pcode to be scanned
;     and a direct mode statement executed (like RUN or CONT).  The
;     executor for the direct mode stmt has been loaded into grs.bdlDirect.
;
;  DEBUG_ERROR - set when a runtime error occurs and
;     is not trapped.  The runtime error code restores SI to
;     the beginning of statement, sets this flag, then re-
;     executes the statement, which invokes the debugger.
;     The error code is passed in the same static variable
;     examined by the ERR intrinsic function.  Causes
;     DebugError() to be invoked.
;
;  DEBUG_STOP - set when a STOP statement is executed,
;     when Ctrl-BREAK is pressed and not trapped, or
;     when a breakpoint is executed.
;     Causes DebugStop() to be invoked.
;
;  DEBUG_WATCHPOINT - set when a Stop-Watch-Expression evaluates to TRUE
;     Causes DebugStop() to be invoked.
;
;  DEBUG_END - set by the executors for opEot and
;     opStEnd to indicate end-of-program.  Causes DebugEnd()
;     to be invoked.
;
;  DEBUG_TRACE - set while tracing statement execution
;     either because of TRON, single-step, or procedure-
;     step.  Causes DebugTrace() to be invoked.
;
;  DEBUG_WATCH - set when any Watch Expressions are
;     active in the program.  Causes DebugWatch() to be invoked.
;
;  DEBUG_CANT_CONT - Causes UserInterface() to set grs.otxCONT to
;     UNDEFINED the next time it is called.
;     This is used by executors, which cannot call CantCont
;     because UserInterface sets grs.otxCONT every time
;     we enter UserInterface, thus undoing their change.
;     Since it sets otxCONT rather than calling CantCont(),
;     stack tracing and variable printing are still possible
;     from direct mode, just not continuing.
;
; Exit:
;	grs.fDirect, grs.oRsCur, grs.otxCur indicate where pcode
;	   execution is to commence
;
;**************************************************************************
cProc	UserInterface,<PUBLIC,FAR>
cBegin
	DbAssertRelB [cGrab],e,0,UI,<UserInterface: cGrab non-zero>
	test	[debugFlags],DEBUG_WATCH
	je	NoWatch			;brif no WATCH expressions visible
	call	DebugWatch		;get next watch expression, or
					; normal program pcode
	jne	J1_TestRestore		;brif the next pcode to be executed
					; is WATCH pcode, DebugWatch has
					; set grs up so it's ready to go
	cmp	[fDebugScr],FALSE
	jne	NoWatch			;brif debug screen is visible
					; need to print values in watch window
	test	[debugFlags],NOT DEBUG_WATCH
	jne	NoWatch			;brif WATCH isn't only thing to do
					;speeds up WATCHPOINTs significantly
J1_TestRestore:
	jmp	SHORT TestRestore
	;If we didn't care about Watch functions that write to the screen
	; we could branch to ExecStmt and be faster, but since most watchpoints
	; are handled by the executor calling DebugWatch directly, the only
	; watch points we are slowing down are when the debug screen is active.

NoWatch:
	cmp	[grs.GRS_fDirect],FALSE
	je	NotInDirect		;brif not executing direct mode stmts

	;Following test prevents tracing statements in direct mode.
	test	[debugFlags],NOT (DEBUG_TRACE OR DEBUG_WATCH)
	je	GotNextStmt		;brif nothing other than TRACE/WATCH

	;We've returned to direct mode, potentially after having debugged
	;a GOSUB/SUB/FUNCTION/DEF FN.  If we don't set otxCur to
	;otxContProg, we'd show the RETURN or END DEF/SUB/FUNCTION
	;stmt as the next stmt to be executed, instead of the statement
	;that was active before the direct mode statement executed.
	
	mov	ax,[otxContProg]
	mov	[grs.GRS_otxCur],ax	;will be stored in grs.otxCONT

NotInDirect:
	;we were not executing in direct mode, setup for CONT executor.
	;If grs.otxCur == UNDEFINED (as set by opEot), we cannot continue
	
	cmp	[debugFlags],0
	je	GotNextStmt		;brif nothing to do.  This is true
					;for the 1st non-direct-mode-buffer
					;pcode we execute.  Once we branch
					;out of the direct mode buffer,
					;there's no need to call UserInterface
					;between statements.
	mov	ax,[grs.GRS_oRsCur]
	mov	[grs.GRS_oRsCONT],ax
	test	[txdCur.TXD_flags],FTX_mrs
	je	NotInDefFn		;brif definately not within a DEF FN
	mov	ax,[grs.GRS_oMrsCur]
NotInDefFn:
	mov	[grs.GRS_oRsContTxtTbl],ax

	mov	ax,[grs.GRS_otxCur]
	mov	[grs.GRS_otxCONT],ax
GetNextStmt:
	;Call NextStmt() to determine what stmt should be executed next.
	;NextStmt() may interact with user, letting user edit program etc.
	
	call	NextStmt

	;If user added or removed a WATCH expression, we need to refresh
	;the values in the Watch window.
	
	cmp	[fRefreshWatch],FALSE
	je	GotNextStmt
	mov	[fRefreshWatch],FALSE
	call	DebugWatch
	je	GetNextStmt		;brif can't execute WATCH pcode
					; for any reason
GotNextStmt:
	cmp	[cWatch],0
	je	TestRestore		;brif no WATCH expressions are active
	or	[debugFlags],DEBUG_WATCH; tell Executor to call UserInterface
					; after executing next statement

;The reason we enter direct mode for each statement, regardless of
;the state of debugFlags, is so we can toggle to the output screen
;for direct mode lines like:
;    FOR I=1 TO 10: PRINT A(I): NEXT I
;but leave the debug screen active for direct mode lines like:
;    FOR I=1 TO 10: A(I) = 1: NEXT I
;
;Make the output screen visible IF:
; we're tracing or watching variables AND next stmt does screen I/O OR
; we're NOT tracing or watching variables AND next stmt is in program
;   (as opposed to direct mode statement buffer)
;
TestRestore:
	cmp	[fDebugScr],FALSE
	je	ExecStmt		;brif output screen already visible
					;If this test is removed, callers
					;who set DebugFlags.DEBUG_EXEC_CMD
					;need to put an ExEot in bdlDirect
					;for FNextStmtDoesIO
	test	[debugFlags],DEBUG_WATCH OR DEBUG_TRACE
	jne	TestForIO		;brif we're watching or tracing
	cmp	[grs.GRS_fDirect],FALSE
	je	ShowUserScr		;brif not executing direct mode stmts
TestForIO:
	call	FNextStmtDoesIO
	or	ax,ax
	je	ExecStmt		;brif next stmt causes no screen I/O
ShowUserScr:
	call	EnsShowOutSaveRs	;show output screen, don't alter
					; grs.oRsCur

ExecStmt:


;***** Begin revision [17]
	
	;Clear FBOSSTOP in bosFlags now, because we have just had the
	;chance to responde to it so if it is still set it is because
	;we stopped for some other reason.  There for, the FBOSSTOP
	;has already been serviced.
	
	and	[bosFlags], NOT FBOSSTOP
;***** End revision [17]

	;If we are debugging or we are executing a direct mode statement,
	;make sure we re-enter user interface for each opBos.
	
	mov	al,[debugFlags]
	or	al,[grs.GRS_fDirect]
	je	DontReEnter
	or	[bosFlags],FBOSDEBUG	;tell executor to call UserInterface
					; at the next beginning of stmt
DontReEnter:
	;ContReinitStat needs to be called every time we exit the
	;user interface if any editing/loading took place.  It ensures
	;there is no unused space within BDs and no free space between
	;near-heap entries.  If there was any, a string allocation could fail
	;even though there is free space, because arrays of string descriptors
	;can only be moved at statement level, because the runtime
	;could have several pointers to them, i.e. x$=a$(1)+a$(2)
	
	call	ContReinitStat

;Return to the executor to execute pcode indicated by:
;grs.fDirect, grs.oRs, grs.otx.
;If the Quit menu item is selected, or the SYSTEM command is entered
;UserInterface returns to let the executor execute ExStSystem.
;Returning from this function causes the pcode-executor to begin.
;

cEnd	;UserInterface

;**************************************************************************
; EnterUserInterface()
; Purpose:
;  Enable all user interface interrupt handlers.
;
;**************************************************************************
cProc	EnterUserInterface,<PUBLIC,NEAR>
cBegin
	cmp	[fUiActive],0
	jne	EntDmExit		;brif already in user interface
	mov	[fUiActive],sp
KbLoadOk:
	cCall	DrawDebugScr		
	cCall	B$UnhookKbd		;tell runtime to disable its handler
	cCall	HookInt24		;enable interrupt-24 interrupt handler
	cCall	CwHook
	call	FlushMsgs		;flush any type-ahead keystrokes or
					; mouse clicks
EntDmExit:
	;Set the BIOS INSERT flag to match what we will be using in
	;this invocation of the UI.  This will keep us in ssync with
	;what speach software thinks our insert state is.

	xor	bx, bx
	mov	es, bx
	cli				; don't allow interrupts (might
					; change BIOS flags on us)
	mov	al, es:[417h]		; get BIOS shift state flags
	and	al, 7fh			; assume insert mode off
	cmp	fInsertMode, bl 	; is CW in an insert mode
        je      @F                      ; brif not, assumption good
	or	al, 80h			; set BIOS insert mode on
@@:
	mov	es:[417h], al		; update BIOS flags
	sti				; reenable interrupts

cEnd	;EnterUserInterface

;**************************************************************************
; ExitUserInterface()
; Purpose:
;  Unhooks all user interface interrupt handlers.
;
;**************************************************************************
cProc	ExitUserInterface,<PUBLIC,NEAR>
cBegin
	call	FlushMsgs		;flush any type-ahead keystrokes or
					; mouse clicks
DbAssertRel [fUiActive],ne,0,UI,<ExitUserInterface: not in user interface>
	mov	[fUiActive],0
	cCall	CwUnHook
	cCall	UnHookInt24
	cCall	B$hookKbd		;tell runtime to enable its handler
cEnd	;ExitUserInterface

;**************************************************************************
; DebugExecCmd
; Purpose:
;	Invoked when we enter NextStmt with DEBUG_EXEC_CMD bit set in
;	debugFlags
; Exit:
;	Returns actionFlag bit which will cause the work to get done.
;
;**************************************************************************
DebugExecCmd PROC NEAR
	mov	[grs.GRS_otxCONT],UNDEFINED
					;RUN <filename> needs otxCont UNDEFINED
					; so scanner can grow variable tables.
	mov	al,FDM_ExecCmd
	ret
DebugExecCmd ENDP

;**************************************************************************
; NextStmt
; Purpose:
;  Called by UserInterface() to determine the next opcode to be executed
;  It is only called when UserInterface has something significant to do.
;
; Entry:
;  same as for UserInterface()
;	
;**************************************************************************
.errnz	DEBUG_ERROR	- 01h
.errnz	DEBUG_EXEC_CMD	- 02h
.errnz	DEBUG_STOP	- 04h
.errnz	DEBUG_END	- 08h
.errnz	DEBUG_WATCHPOINT - 10h
.errnz	DEBUG_TRACE	- 20h
DebugDispTbl LABEL WORD
	DW	UIOFFSET DebugError
	DW	UIOFFSET DebugExecCmd
	DW	UIOFFSET DebugStop
	DW	UIOFFSET DebugEnd
	DW	UIOFFSET DebugStop
	DW	UIOFFSET DebugTrace
DebugDispTblEnd LABEL WORD

DbPub	NextStmt
cProc	NextStmt,<NEAR>
	localB	actionFlags
	localB	debugFlagsTmp
cBegin
	DbAssertRelB [cGrab],e,0,UI,<NextStmt: cGrab non-zero>
	;remember if the debug screen was visible when we entered NextStmt()
	mov	al,[debugFlags]
	mov	[debugFlagsTmp],al
	test	al,DEBUG_ERROR
	jne	GotRtErr

	;Set b$fCompErr 0 so any errors generated from within the user
	; interface don't look like they were generated in QuickLibrary.
	
	mov	[b$fCompErr],0
GotRtErr:

	;reset any variables that assume we're in same context we were
	;in last time they were set.
	
	call	UiFlushCache
	sub	ax,ax
	mov	[actionFlags],al
	mov	[debugFlags],al
	mov	[fDoCmd],ax

;  Dispatch to function based on debugFlagsTmp.  Function
;  returns ushort mask with 0 or more of FDM_xxx bits set
;  indicating what action is to be performed.
;  Function also sets various bits in debugFlagsTmp to cause
;  UserInterface to be entered after the next statement is executed.
;
;  NOTE: The order these flags are tested is IMPORTANT
;
	mov	al,[debugFlagsTmp]
	and	al,DEBUG_EXEC_CMD+DEBUG_ERROR+DEBUG_STOP+DEBUG_END+DEBUG_WATCHPOINT+DEBUG_TRACE
	je	DispRet			;brif nothing interesting
	mov	bx,UIOFFSET DebugDispTbl-2
DispLoop:
	inc	bx			;advance to next exception handler
	inc	bx
DbAssertRel bx,b,<UIOFFSET DebugDispTblEnd>,UI,<uimain.asm: Bad debugFlags>
	shr	al,1
	jnc	DispLoop
	call	cs:[bx]			;dispatch to routine based on debugFlags
					;al = bit mask of actions to carry out
DispRet:
 	mov	[actionFlags],al
 	test	al,FDM_ShowStmt
	je	TestGetCmd		;brif cursor not to be positioned
	call	EnsShowDebugScr		;make debug screen visible
	call	DoDrawDebugScr		;actually draw debug screen, can't
					; wait for next call in GetCmd() in the
					; case where user just executed a TRON
					; stmt, we don't call GetCmd at all
	call	ContContext		;activate "program counter's" context
	je	TestGetCmd		;brif can't continue & no main program

	;otx may point to beginning of next statement, get it back to current
	;map UNDEFINED,0,1,2,3,... to 0,0,0,1,2,3,...
	
	mov	ax,[grs.GRS_otxCONT]
	inc	ax			;test for UNDEFINED
	je	DoShowStmt		;if can't CONT, cur stmt is 0 of main
	dec	ax			;restore ax = otxCONT
	je	DoShowStmt		;map 0 to 0
	dec	ax			;map 1,2,... to 0,1,...
DoShowStmt:
	push	[grs.GRS_oRsCur]
	push	ax			;pass otxCur
	PUSHI	ax,UNDEFINED		;determine column from otx
	call	ShowStmt

TestGetCmd:
	;See if executor told user interface to not allow continuing.
	;This is done when it wants to show where execution stopped
	;(i.e. for error reporting) so it leaves grs.otxCur not UNDEFINED
	;on entry to UserInterface.  It is important that we not call CantCont
	;before calling ShowStmt, or else ShowStmt has nothing to show.
	
	test	[debugFlagsTmp],DEBUG_CANT_CONT
	je	CantContNotSet
	call	CantCont
	mov	[actionFlags],FDM_GetCmd
CantContNotSet:
	test	[actionFlags],FDM_GetCmd
	je	TestDoCmd

;Init debug screen if necessary, make it visible.  This is necessary
;because DEBUG_END causes FDM_GetCmd to be set, but not FDM_ShowStmt.
;Continue calling GetCmd() until we get a command which we can execute
;without errors.  GetCmd() interacts with user until user enters a
;direct-mode command (which may take the form of a menu selection, or
;accelerator key like SingleStep).
;This can cause context switches to different module and
;procedure text tables.  It also invokes the text
;manager (which invokes the parser) to alter program text.
;
GetCmdLoop:
	;Turn off CMD_SW_RUN switch in case user specified /RUN <filename>
	;in qb command line, and then got a runtime error.  This will
	;cause the SYSTEM statement to return to user interface rather than
	;terminating qb.
	
	and	[cmdSwitches],NOT CMD_SW_RUN

	call	EnsShowDebugScr
	call	EnterUserInterface	;enable userInterface interrupt handlers
	mov	[otxContProg],UNDEFINED	;since this call to GetCmd can cause
					; editing, and otxContProg isn't updated
					; for pcode movement, we can't depend
					; on it after this call.
	call	GetCmd
	call	WatchInfoUI		;update cWatch if necessary
WatchWnOk:
	call	ExitUserInterface	;restore runtime's interrupt handlers

;Make sure TxtDirect and SystemScan are unable to tie up so much
;variable table space that we are unable to execute very simple
;direct mode statements.  This block of memory will be freed
;by UiReleaseSpace below.  Be very careful when changing control
;flow below this point that all UiGrabSpace calls are balanced by
;UiReleaseSpace calls.
TestDoCmd:
	call	UiGrabSpace		; UiGrabSpace can take a lot of time...
					; slows down tracing, and WATCH (when
					; debug screen is visible)
	cmp	[fDoCmd],0		; fDoCmd can only be set as a result
					;  of calling GetCmd().  It is not
					;  set as a if user wants to TRACE
	je	NoDirect		; brif all scanned and ready to execute
					;  (speed optimization for WATCH/TRACE)
	mov	[fDoCmd],0
	call	ContContext		;activate "program counter's" context
					; for TxtDirect
	call	TxtDirect		;Now parse & scan direct mode statement
	mov	ax,[grs.GRS_otxCONT]
	mov	[otxContProg],ax
	jmp	SHORT CheckErr

;no direct mode stmt (we're tracing), just scan all text tables
;and set current context to grs.otxCONT
NoDirect:
	call	SystemScan
	test	[actionFlags],FDM_ExecCmd
	je	NoExecCmd

	;some executor setup direct mode buffer, i.e. ExRunFile
	;loaded program and put ExRunMain in grs.bdlDirect
	
	cmp	[txtErr.TXER_errCode],0
	jne	CheckErr		;dont set otxCONT if scan error
	mov	ax,[grs.GRS_oMrsMain]
	mov	[grs.GRS_oRsCONT],ax
	push	ax
	call	UiRsActivate		;activate oRs to continue
	sub	ax,ax
	mov	[grs.GRS_otxCur],ax
	mov	[grs.GRS_otxCONT],ax	;ExStChain causes ExStCont to be
					; invoked, which needs otxCONT=0
	dec	ax
	mov	[grs.GRS_fDirect],al	;execute pcode out of direct mode buf

	;If we were tracing when we entered with DEBUG_EXEC_CMD,
	; continue tracing.  The only reason we entered DirectMode
	; was to SystemScan and report errors.
	
	mov	al,[debugFlagsTmp]
	and	al,DEBUG_TRACE
	or	[debugFlags],al
	jmp	SHORT CheckErr		;report any parser/scanner errors

NoExecCmd:
	call	NeedContContext		;activate "program counter's" context
					; if no main module, uierr is set
	je	SHORT GetNextCmd	;brif CONT is not possible

;now see if TxtDirect/SystemScan encountered any errors
CheckErr:
	mov	ax,[txtErr.TXER_errCode]
	or	ax,ax
	jne	CmdErr			;brif parser/scanner errors

	;We could get here without having called UiGrabSpace (if we were
	;tracing, and all text tables were already scanned to SS_EXECUTE)
	;If we didn't call UiGrabSpace, we are guarenteed that cGrab=0
	;by assertion at entry of NextStmt().
	
	cmp	[cGrab],0
	je	NoneGrabbed1
	call	UiReleaseSpace
NoneGrabbed1:
cEnd	;NextStmt

CmdErr:
	inc	ax			;test for UNDEFINED
	je	GetNextCmd		;brif user wants to back out of cmd
					; because it would prevent CONT
	dec	ax			;restore ax=errCode
	mov	[uierr],ax		;remember to report error in GetCmd's
GetNextCmd:				; structure txtErr.xxx
	call	UiReleaseSpace		;at this point, we know we called
					; UiGrabSpace.  Release it
	jmp	GetCmdLoop

;*********************************************************************
; AskCantCont()
;
; Purpose:
;  AskCantCont() is called by TextMgr when it is about to make an
;  edit which would prevent continuing program execution.
;  This function should not be called during exection.	It is not
;  valid (or useful) while the executor is in operation.
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
cProc	AskCantCont,<PUBLIC,FAR>
cBegin
	call	fCanContUI
	je	AcCantCont		;brif already can't continue

	;display "This will prevent CONT, proceed anyway?"
	PUSHI	ax,MB_OKCANCEL
	PUSHI	ax,MSG_CantCont
	call	MsgBoxStd
	cmp	al,IDOK
	mov	ax,0			;prepare to return FALSE
	jne	AcExit			; brif user wants to backout of edit
	call	CantCont		;disable CONT
AcCantCont:
	mov	ax,sp			;return TRUE
AcExit:
	or	ax,ax			;set condition codes for caller
cEnd	;AskCantCont

;*********************************************************************
; AskMakeRem()
; Purpose:
;  The user tried to insert a blank line before a SUB/FUNCTION line.
;  This would cause us grief when we tried to ASCII save/load the
;  file, because blank lines delimit the block of comments that are
;  to remain with the SUB/FUNCTION.  The case where this is important
;  is when the user has a module with no module level code, but still
;  wants to have a block of comments at module-level.
;  If we allowed blank lines in the block of comments that we move
;  into the SUB during ASCII load, this block of module comments
;  would be moved as well.
;
; Exit:
;  Returns FALSE if user wants to abort current edit, with
;  condition codes set based on value in ax.
;
;*********************************************************************
cProc	AskMakeRem,<PUBLIC,FAR>
cBegin
	;"Blank lines not allowed before SUB/FUNCTION line.  Is remark ok?"
	PUSHI	ax,MB_OKCANCEL
	PUSHI	ax,MSG_MakeRem
	call	MsgBoxStd
	sub	ax,IDCANCEL		;ax=0 if user said CANCEL
cEnd

;*********************************************************************
; NotSaved(), NotSavedInc(), NotSavedIncSav()
;
; Purpose:
;  Called by Context Mgr's NewStmt and SYSTEM executor when it is about
;  to discard module(s).
;  If any module has been modified since last saved, a dialog box asks
;     the user if the module(s) are to be saved.
;     If user selects Yes,
;        CmdFileSaveAll is invoked
;        If any errors occur during CmdFileSaveAll, an error code is returned.
;        else the function returns 0.
;     If user selects Cancel,
;        it returns MSG_GoDirect, which is not trappable,
;        and eventually gets us back into user interface, which does not
;        report this special MSG_GoDirect runtime error.
;     If user selects No,
;        the function returns -2.
;  Else if no modules need to be saved,
;     it just returns -1.
;
;  NotSavedInc just does this for INCLUDE mrs's with the prompt:
;   "Modified $INCLUDE files must be saved before running. Save them now?"
;
;  NotSavedIncSav just does this for INCLUDE mrs's with the prompt:
;   "Save modified $INCLUDE files first?"
;
; Entry:
;  none
;
; Exit:
;  returns 0 if all modified files were saved without error,
;  returns -1 if no files were modified (and thus not saved)
;  returns -2 if user responded NO to the prompt
;  returns MSG_GoDirect if user responded CANCEL to the prompt
;  returns error code in ax if an I/O error occurred while trying to save.
;
;*********************************************************************
PUBLIC	NotSaved, NotSavedInc, NotSavedIncSav
NotSavedIncSav PROC FAR
	mov	al,2
	SKIP2_PSW			;skip following mov al,1 instruction
NotSavedIncSav ENDP
NotSavedInc PROC FAR
	mov	al,1
	SKIP2_PSW			;skip following mov al,0 instruction
NotSavedInc ENDP
NotSaved PROC FAR
	mov	al,0

;Common entry for NotSaved, NotSavedInc
	push	si			;save caller's si,di
	push	di
	cbw				;ax = fInclude
	xchg	di,ax			;di = fInclude

	mov	[uierr],0		;need to init uierr, because it
					; could be non-zero if user selected
					; File/Exit with dirty line in editor.
	call	EnStaticStructs		;activate static prsCur, mrsCur, txdCur
					;cx = 0 if no action taken
	push	ax			;remember if we need to call DisStatic..
	push	WORD PTR([grs.GRS_fDirect])
	push	[grs.GRS_oRsCur]	;save caller's oRs (for UiRsActivate)

	mov	si,UNDEFINED
	.erre	IDCANCEL - UNDEFINED	; (it will be tested below)
	.erre	IDNO - UNDEFINED	; (it will be tested below)
	.erre	IDYES - UNDEFINED	; (it will be tested below)

	;pass UNDEFINED to UiRsActivate so no mrs is active
	cCall	UiRsActivate,<si>	
NsLoop:
	call	NextMrsFile_All		; activate next mrs that has a FILE
					; NextMrsFile doesn't pick up an
					; empty <Untitled> module.
	inc	ax			;test for UNDEFINED
	je	NsDone			;brif done with all mrs's
	test	[mrsCur.MRS_flags2],FM2_Modified
	je	NsLoop			;brif module is unchanged
	or	di,di
	je	SaveIt			;brif saving ALL files, not just INCLUDE
	test	[mrsCur.MRS_flags2],FM2_Include
	je	NsLoop			;brif this isn't an INCLUDE file

SaveIt: ;Got at least 1 module which needs to be saved - ask user
	PUSHI	ax,MB_YESNOCANCEL
	mov	ax,MSG_NotSavedAll
	.errnz MSG_NotSavedInc - MSG_NotSavedAll - 1
	.errnz MSG_NotSavedIncSav - MSG_NotSavedAll - 2
	add	ax,di			;bump to MSG_NotSavedInc if fInclude
	push	ax
	call	MsgBoxStd		;al = reply
	cmp	al,IDYES
	xchg	si,ax			;save user's response in si
	jne	NsNoSave		;brif NO or CANCEL reply

	xor	ax,ax			; no title for subsequent dialog
	xchg	ax,szDialogTitle	; boxes (save all)
	push	ax			; save current dialog box title title
	cCall	CmdFileSaveAll,<di>	;save all modified modules
	pop	szDialogTitle		; restore current dialog box title

	or	ax,ax
	jne	NsNoSave		;brif user didn't select CANCEL in
					; Save dialog, and no I/O errors
	mov	si,IDCANCEL
NsNoSave:
;At this point, si = UNDEFINED if no files needed to be saved,
;		     IDNO if user chose to not save unsaved files,
;		     IDCANCEL if user wants to back out of current operation,
;		     IDYES if all files were saved,
;	                (uierr = non-zero if any errors occurred while saving)
;	
NsDone:
	call	UiRsActivate		;activate stacked register set.
	pop	ax			;ax = saved value of grs.fDirect
	mov	[grs.GRS_fDirect],al	;restore it
	pop	cx			;cx=0 if static structs were already
	jcxz	NsStatic		; enabled when NotSaved was called
	call	DisStaticStructs	;ensure static structs deactivated
NsStatic:
	;If CANCEL button was pressed, return untrappable runtime error
	;which will get us back into user interface if we were in the
	;middle of CHAIN/RUN <file>
	
	mov	ax,[uierr]
	or	ax,ax
	jne	NsExit			;brif I/O error during save

	mov	al,MSG_GoDirect
	cmp	si,IDCANCEL
	je	NsExit			;brif user responded CANCEL

	sub	ax,ax			;ax = 0
	cmp	si,IDYES
	je	NsExit			;return 0 if any/all files saved
	dec	ax			;ax = -1
	inc	si			;test for UNDEFINED
	je	NsExit			;return -1 if no files need to be saved
	dec	ax			;if user said NO, return -2
NsExit:
	pop	di			;restore caller's si,di
	pop	si
	or	ax,ax			;set condition codes for caller
	ret
NotSaved ENDP

;**************************************************************************
; UiGrabSpace
; Purpose:
;	Don't let user enter such a long program that he can't even execute
;	a SYSTEM, CLEAR, or SETMEM statement.
;	VERY IMPORTANT: Every call to UiGrabSpace MUST be balanced by
;	a subsequent call to UiReleaseSpace.  Make sure the code in
;	between can't take pathological branches (RtError) or non-pathological
;	branches around the UiReleaseSpace call.
;
;	The goal of user-interface tight-memory-management is to never
;	let the user get so low on memory that they get locked up
;	(i.e. so tight they can't even delete some text or unload a module
;	or execute a direct-mode clear/setmem statement).  If they
;	get locked up to the point where they can't save their file,
;	it is as bad as crashing.
;	  Part of the strategy involves reserving memory whenever the
;	user could potentially be making long-term memory commitments
;	(i.e. loading a new module or inserting a new line of text)
;	  Another part of it is having UiGrabSpace release any
;	"discretionary memory" it can when we run out of memory.
;
;**************************************************************************
cProc	UiGrabSpace,<PUBLIC,NEAR>
cBegin
	inc	[cGrab]
	call	GrabSpace
	or	ax,ax
	jne	GotSpace		;brif got space

	;We're very low on memory
	;Release any discretionary info we can to give us the space we need.
	call	AlphaORsFree		;release sorted alphabetised list
					; of modules/procedures - we build
					; it whenever we need it.
	extrn	FreeCmdHistory:near
	call	FreeCmdHistory		;release command window's history
	call	GrabSpace		;should never fail.
GotSpace:
cEnd

;**************************************************************************
; UiReleaseSpace
; Purpose:
;	We're not in an area where we could make long-term memory commitments
;	that would prevent us from doing a SYSTEM, CLEAR, or SETMEM statement.
;	Unlike ReleaseSpace(), this function doesn't release the space
;	unless all nested callers of UiGrabSpace have released their hold.
;
;**************************************************************************
cProc	UiReleaseSpace,<PUBLIC,NEAR>
cBegin
	DbAssertRelB [cGrab],ne,0,UI,<UiReleaseSpace: cGrab=0>
	dec	[cGrab]
	jne	NotDone			;brif not everyone that called
	call	ReleaseSpace		; UiGrabSpace has called UiReleaseSpace
NotDone:
cEnd

;**************************************************************************
; UiAlphaORsBuild
; Purpose:
;	Since AlphaORsBuild allocates a heap entry, we need to make sure
;	it doesn't encroach upon the minimum heap the user interface needs.
; Exit:
;	ax = 0 if out-of-memory
;
;**************************************************************************
cProc	UiAlphaORsBuild,<PUBLIC,NEAR>
cBegin
	call	AlphaBuildORs		
cEnd


;**************************************************************************

;**************************************************************************
;EnMenuCall
;Purpose:
;	Makes a far call to EnableMenuItem.
;Entry:
;	AL = Menu Item Name
;	DI = Enable / Disable item
;Exit:
;	None
;Usage:
;	Saves AX,BX,CX,DX.
;**************************************************************************
cProc	EnMenuCall,<NEAR>,<AX,BX,CX,DX>
cBegin
	xor	ah,ah			; AX = MenuItemName
	cCall	EnableMenuItem,<ax,di>
cEnd

;*************************************************************************
;ChMenuCall
;Purpose:
;	Makes a far call to CheckMenuItem
;Entry:
;	AL = Menu Item Name
;	DI = Enable / Disable item
;Exit:
;	None
;Usage:
;	Per Convention
;*************************************************************************
cProc	ChMenuCall,<NEAR>		
cBegin
	xor	ah,ah			; AX = MenuItemName
	cCall	CheckMenuItem,<ax,di>
cEnd


;**************************************************************************
; MenuEnable - rewritten in assembler from c code in uictl.c
; Purpose:
;	Enables Display in Main Window.
; Entry:
;	None
; Exit:
;	None
; Uses:
;	PerConvention
;
;**************************************************************************
cProc	MenuEnable,<PUBLIC,FAR>,<DI,SI>
cBegin
	cCall	UiRsActivateWnd     ; Tell context manager to activate
				    ; active window's register set

	; The Edit Field Structure is updated by every cursor movement
	; indicating system's current state, in terms of options available.
	; For example at the begining of qb sesbx on with blank screen, i.e.
	; no files loaded, you would not have any 'Edit' options available.
	; the moment you type anything 'Undo' becomes available. Further
	; until you select and 'cut', 'paste' would not be availble.

	mov	bx,pwndAct	    ; bx  = pointer to active window's reg set
	mov	bx,[bx.pefExtra]    ; bx  = * to edit field structure

	push	bx		     ; Save for later use
	mov	bl,[bx.EF_fSelection]
	xor	bh,bh		     ; BX = fSelection from edit field structure

	xor	cx,cx		    ; if pwndAct != wndCmd then ListWnd=False
	cmp	pwndAct,DATAOFFSET wndCmd;		       else ListWnd=True
	je	fIsListWndSet	    ; CX = ListWnd Status
	dec	cx
fIsListWndSet:

	xor	dx,dx			    ; So no changes like cut, paste
	cmp	pwndAct,DATAOFFSET wndHelp  ; are allowed. fChangeable flag
	je	fChangeableSet		    ; specifies if user is in Help.
	dec	dx			
fChangeableSet: 		    ; DX = fChangeable
	mov	si,dx			;SI = fChangeable


	mov	di,bx		    ; DI = fSelection
	mov	al,midEditCopy	    ; Set up Edit/Copy
	cCall	EnMenuCall	

	and	di,si		    ; DI = fSelection AND fChangeable
	mov	al,midEditCut	    ; Setup Edit/Cut
	call	EnMenuCall	
	mov	al,midEditClear     ; Set up Edit/Clear
	cCall	EnMenuCall	

	mov	al,fPasteOk	
	cbw			
	xchg	di,ax		    ; DI = Paste Command availability status
	and	di,si		    ; DI = fPasteOK AND fChangeable
	mov	al,midEditPaste     ; Set up Edit/Paste
	cCall	EnMenuCall	

	mov	di,cx		    ; DI = ListWindow Status
	mov	al,midSearchFind    ; Set up Search/Find
	cCall	EnMenuCall	
	mov	al,midSearchNext    ; Set up Search/Next
	cCall	EnMenuCall	

	and	di,si		    ; DI = ListWnd AND fChangeable
	mov	al,midSearchChange  ; Set up Search/Change
	cCall	EnMenuCall	
				    ; bx  was save way above.
	pop	bx		    ; Restore bx  = * Edit Field Structure

	mov	al,fSyntaxCheck
	cbw			    ; AX = Syntax Check options Selected?
	xchg	di,ax		    ; DI = Syntax Check options
	mov	al,midOptnsSyntax   ; in Options of menus
	cCall	ChMenuCall

	xor	cx,cx
	xor	ah,ah			
	mov	al,[mrsCur].MRS_flags2	;get mrs file flags
	and	al,FM2_Include+FM2_NoPcode ;is this a pcoded window?
	jnz	SetFalse	    ; brif not
	dec	cx		
SetFalse:
	mov	dx,si			; DX = fChangable
	and	dx,cx			; fIsCodeWnd = Pcode & Changeable
					; DX = fIsCodeWnd, CX = fPcode


	mov	di,dx		    ; DI = IsCodeWnd
	mov	al,midEditNewSub    ; Setup Edit/New Sub
	cCall	EnMenuCall	
	mov	al,midEditNewFunc   ; Setup Edit/New Function
	cCall	EnMenuCall	
	mov	al,midDebugToggleBp ; Set up Debug/Toggle BreakPoint
	cCall	EnMenuCall	
	mov	al,midGoUntilCursor
	cCall	EnMenuCall	

	mov	di,fIsProgram		; DI = TRUE if prog, FALSE if doc

	mov	al,midViewSubs		; Setup View/Subs
	cCall	EnMenuCall	    	
	mov	al,midRunStart		; Setup Run/Start
	cCall	EnMenuCall	    	
	mov	al,midRunRestart	; Setup Run/Retart
	cCall	EnMenuCall	    	
	mov	al,midRunContinue	; Setup Run/Continue
	cCall	EnMenuCall	    	
	mov	al,midStep		; Setup Debug/Step
	cCall	EnMenuCall	    	
	mov	al,midPStep		; Setup Debug/PStep
	cCall	EnMenuCall	    	
	mov	al,midDebugClearAllBp	; Setup Debug/Clear all Bp
	cCall	EnMenuCall	    	
	mov	al,midDebugTraceOn 	; Setup Debug/Trace On
	cCall	EnMenuCall	    	


	mov	di,dx			; di = fIsCodeWnd
	or	di,di			; is this a code window?
	je	NotCodeWnd		; brif not
	cCall	fCanContUI	    ; returns AX = Execution State
	xchg	ax,di		    ; False if Not in Code Window
NotCodeWnd:				
	mov	al,midDebugSetNextStmt; Set up Set Next Stmt command
	cCall	EnMenuCall	    ;	in debug options of menus

	mov	al,fTraceOn	
	cbw			
	xchg	di,ax		    ; di = Trace On Flag
	mov	al,midDebugTraceOn  ; Setup Debug/Trace On
	cCall	ChMenuCall	

cEnd


sEnd	UI

end
