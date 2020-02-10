	TITLE	RTERROR - interpreter-specific error handling code
;***
;rterror.asm - interpreter-specific error handling code
;
;	Copyright <C> 1986, 1987 Microsoft Corporation
;
;Purpose:
;	This module contains interpreter-specific code to restore context
;	to a clean state after some runtime error occurs, or to invoke a
;	users error trap if one exists.
;
;
;*******************************************************************************

	.xlist
	include 	version.inc

;set these runtime switches, just so we can include messages.inc (which
;we include just so we can map their internal error constants to ours)
FE_EVENT = TRUE
OM_DOS2  = TRUE
OX_XENIX = FALSE

	RTERROR_ASM = ON
	includeOnce	architec
	includeOnce	context
	includeOnce	executor
	includeOnce	exint
	includeOnce	heap
	include 	messages.inc
	includeOnce	names
	includeOnce	opcontrl
	includeOnce	qbimsgs
	includeOnce	rtinterp
	includeOnce	rtps
	includeOnce	scanner			
	includeOnce	txtmgr
	includeOnce	ui
	includeOnce	util
	.list

assumes DS, DATA
assumes SS, DATA
assumes ES, NOTHING

sBegin	DATA
	externW	b$fInt24Err	; 0 ==> rt gives error on int 24
				; -1 ==> rt saves int24 error code and
				;        ignores it. No errors have occurred
				; NZ ==> an int24 error has occurred
	externB	b$inonerr	;TRUE if currently executing in an error trap
	externW	b$errnum	;standard BASIC error code for error
	externW	b$errlin	;line number of error
	externD	b$erradr	;far address of error if in compiled code
	externD	b$errmod	;far pointer to name of module in which error
				;  occurred
	externW	b$fcomperr	;Set 0 by B$IONERR to tell user interface that
				;  error occured in QB4 code; if non-zero, tells
				;  UI to use b$errmod, b$erradr.
	externW	b$cCSubs	;count of invocations of compiled BASIC code
	externB	b$ErrInfo	;extended error codes (UI <--> Runtime)
	externW RtDispVecHigh	;must be reset on runtime error
	staticW	otxSave,0	
	staticW	oRsSave,0	
	staticW	pGosubSave,0	
	globalB fBreakOnErr,0	
sEnd	DATA

	EXTRN	B$RUNERRINFO:FAR
	EXTRN	B$END:FAR	;call if internal error to force QB4 termination
	EXTRN	B$PUTS:NEAR	;used to output err msg for really fatal errors
	EXTRN	B$ClearRange:FAR ;frees all owners in a given range
	EXTRN	SetSpFar:FAR	;used to restore BOS/BOL SP based on BP
	EXTRN	B$ASSN:FAR	;used to obtain SEG address of runtime entry pts
	EXTRN	B$STDALCALLTMP:NEAR	
				; frees temp strings at or above current levl
	EXTRN	B$RESUMED:far	; RT support code for the RESUME stmt

;	Message numbers used by the UL loader.

	PUBLIC	ER_DMA		;Dos Arena error
	PUBLIC	ER_ULM		;Memory allocation error
	PUBLIC	ER_ULO		;Out of memory error
	PUBLIC	ER_ULI		;Invalid user library format error
	PUBLIC	ER_ULD		;Disk IO error
	PUBLIC	ER_ULF		;Cannot find file error (if redirected IO)
	PUBLIC	ER_ULE		;Error message preamble
	PUBLIC	ER_ULG		;Error message postamble
	PUBLIC	ER_ULP		;Path prompt preamble
	PUBLIC	ER_ULT		;Path prompt postamble

	.errnz	FE_QBINITBASE AND 00FFH
				;assuming that low-byte of FE_QBINITBASE is 0
	QB_RTERR_MASK EQU FE_QBINITBASE SHR 8
				;byte mask to OR into a normal error message
				;  to make it a QB-specific initialization
				;  error code (but print out w/same string)

sBegin	RT
assumes CS, RT

;***
; B$PUTNUM - Print numbered message to the screen
;
;Purpose:
; Prints a string to the console device (B$PTRFIL = 0), as referenced by the
; passed message number
;
;Entry:
; [AX] = Message number
;
;Exit:
; none.
;
;Uses:
;
;******************************************************************************
cProc	B$PUTNUM,<NEAR,PUBLIC>
cBegin
	or	ah,ah			;internal error?
	jz	Got_QB4_Err		;brif not - error number o.k.

	cmp	ah,QB_RTERR_MASK	;error during init noted in own code?
	jnz	Not_QB_Init_Error	; brif not one of ours

	xor	ah,ah			;mask of high byte; al contains a
					; standard runtime error message
	jmp	short Got_QB4_Err
Not_QB_Init_Error:
	DbAssertRel  ax,nz,FE_NOLINEBASE,RT,<B$PUTNUM Error>
	mov	bx,FE_NOTHINGBASE	;first Runtime message # in fourth set
	mov	cx,ER_D21		;first QB4 message # in fourth set
	cmp	ax,bx
	jnb	Get_QB4_Err		;brif in last of 4 internal msg sets

	mov	bx,FE_BASE		;first Runtime message # in middle set
	mov	cx,ER_SSC		;first QB4 message # in middle set
	cmp	ax,bx
	jnb	Get_QB4_Err		;brif in first of 4 internal msg sets

In_First_Set:
	mov	bx,MS_BASE		;first Runtime message # in first set
	mov	cx,ER_RFS		;first QB4 message # in first set
Get_QB4_Err:
	sub	ax,bx			;ax = offset into appropriate group
	add	ax,cx			;add start of QB4 message group
Got_QB4_Err:
	cCall	ListStdMsgFar,<ax>	;put error msg in bufStdMsg
	mov	dx,ds
	mov	ax,dataOFFSET bufStdMsg	;dx:ax points to error message
	cCall	B$PUTS
cEnd

;***
;B$IErrSaveState
;Purpose:
;	This routine added as part of revision [5].
;
;	Save key QB-specific state variables away so that a later call to
;	B$IErrRestState can restore them.
;Entry:
;	none.
;Exit:
;	none.
;*******************************************************************************
cProc	B$IErrSaveState,<PUBLIC,NEAR,NODATA>
cBegin
	mov	ax,[pGosubLast]
	mov	[pGosubSave],ax
	mov	ax,[grs.GRS_otxCur]
	mov	[otxSave],ax
	mov	ax,[grs.GRS_oRsCur]
	mov	[oRsSave],ax
cEnd

;***
;B$IErrRestState
;Purpose:
;	This routine added as part of revision [5].
;
;	Restore QB-specific state variables saved by B$IErrSaveState
;Entry:
;	none.
;Exit:
;	none.
;*******************************************************************************
cProc	B$IErrRestState,<PUBLIC,NEAR,NODATA>
cBegin
	mov	ax,[pGosubSave]
	mov	[pGosubLast],ax
	mov	ax,[otxSave]
	mov	[grs.GRS_otxCur],ax
	push	[oRsSave]
	call	RsActivate
cEnd

;***
;B$IONERR - branch to user's error trap if there is one
;Purpose:
;	This routine greatly reworked as part of revision [5]
;
;	When a runtime error occurs, the runtime first calls B$IErrSaveState
;	so we can preserve grs.oRsCur, grs.otxCur, and pGosubLast. It also
;	preserves such variables as b$curlevel, b$curframe, and b$cNonQBIFrames.
;	Then the stack is walked, using the most recent BASIC frame (via
;	b$curframe); the BASIC-specific chain is used to find each BASIC
;	frame, and b$cCSubs is used to indicate whether the frame is for
;	compiled or interpreted BASIC code (and thus whether B$CONERR or
;	B$IONERR should be called). 
;	
;	When this routine is called, b$curframe, oRsCur, etc. are all set up
;	for the QB-specific frame of current interest. If this frame is for
;	a context for which there is an active error handler (and barring any
;	special cases), all frames below (more recent to) this one get their
;	owners released (i.e., that part of the stack is "blasted"), and
;	the error handler is invoked; ERL is set as if the error occured in
;	this context, and RESUME [NEXT | line#] act the same way.
;
;	If there is no error handler, we return to the runtime; we return
;	a flag that indicates whether the runtime code should keep walking
;	the stack or not. Currently, we say "quit looking" if the current
;	frame is an event frame.
;
;	Eventually, either a frame is found (QB or BC) that has an active
;	error handler and this is invoked, OR, we find an event frame or
;	the end (top) of the stack. In this latter case, the runtime restores
;	the context at which the error occured (with help from the QB callback
;	routine B$IErrRestState) and calls B$FERROR.
;
;	In the event that we find an error trap to invoke, we clean the stack.
;	In order to clean the stack, we set BP to b$curframe, depending on
;	this always being the current frame pointer. Based on the current 
;	context	(and with the help of the procmgr), we restore SP to what 
;	it was at the beginning of the statement in which the error occured 
;	by calculating what it should be based on BP. We also release any
;	owners left on the stack below what should be there for the current
;	frame.
;
;	In order to save the otx for RESUME, we know that, on input to this
;	routine, grs.GRS_otxCur is the value of SI on entry to the runtime 
;	entry point from which the runtime error code was called.
;	Note that the RESUME state cannot get the otx directly; we must
;	call a routine to move that otx back to the last opBOS, and save
;	THAT otx in the RESUME state.
;
;	Note that, to invoke an error trap, this routine will have to
;	deactivate the current prs, if any, load DI and ES for mrsCur,
;	set SI to the otx for the trap, clean the stack (restoring SP
;	and BP from b$curframe), as well as set up the RESUME state.
;
;
;	An exception to the above is the case where some runtime entry point
;	was called from a place in the interpreter that expects to get the
;	error code back, rather than have a runtime error occur. In this
;	case, the global errCodeRet.SPsave will be non-zero. SI, DI, SP, and
;	BP will be restored from the errCodeRet structure, and the interpreter
;	routine at CP:[errCodeRet.retAddr] will be JMP'd to, with the error
;	code in AX.
;
;Entry:
;	Error code is in b$errnum
;	b$curframe assumed valid
;	grs.GRS_otxCur contains the otx prior to runtime call.
;	pFrameErr is the frame pointer 1 level below error time
;Exit:
;	b$errlin	= line number of error
;	AX = 0 if the error must be treated as a fatal error, perhaps because
;		b$errnum > 255, or the current frame is an event frame and
;		has no handler, or ...
;	AX <> 0 means it's okay for the runtime to continue walking the frame
;		chain looking for an invokeable error handler.
;Uses:
;	SI and DI.
;Exceptions:
;	If the user has no error trap, this routine returns, but if
;	there is an error trap or in certain other instances, we'll clean the 
;	stack and never return.
;
;*******************************************************************************
cProc	B$IONERR,<PUBLIC,NEAR,NODATA>
parmW	pFrameErr			;frame pointer 1 level below error time
cBegin
	DbAssertRel  [b$cCSubs],z,0,RT,<B$IONERR: b$cCSubs not 0>
	mov	[executorFlags],0	;reset to BOS state
	mov	[RtDispVecHigh],SEG B$ASSN ;restore default in case of math err
	mov	ax,[b$errnum]
	or	ah,ah			;error number > 255?
	jz	BIONERR_Cont		;  brif not - not an internal error

	DbAssertRel  ax,ae,FE_BASE,RT,<B$IONERR Got bad internal error from RT>
	DbAssertRel  ax,nz,FE_NOLINES,RT,<B$IONERR: FE_NOLINES found> 
	cmp	ax,FE_GODIRECT
	jnz	J1_BIONERR_NoCont_Exit	;internal error, exit QBI
BIONERR_Cont:
	mov	cx,[errCodeRet.ERRRET_saveSP]
	jcxz	No_Err_Ret		;brif not trying to fake an error return

	cmp	ax,FE_GODIRECT
	jnz	RtTrapRet_Jump

	mov	ax,MSG_GoDirect
RtTrapRet_Jump:
	mov	[b$errnum],ax
	jmp	far ptr RtTrapRet	;return to caller from CP segment

No_Err_Ret:
	mov	[b$errnum],ax		;in case of a modified message number
	;we should only find internal errors here when fInitialized is false:
	DbAssertRelB  [fInitialized],nz,0,RT,<B$IONERR: fInitialized is True>

	xor	cx,cx
	mov	[DimAtScanType],cl	; In case of error during Dim
	mov	[grs.GRS_flagsDir],cl	;reset flags which get reset every
					; time we begin executing pcode,
					; or when a runtime error occurs.

	cmp	ax,FE_GODIRECT
	jnz	NotGoDirect

; Some special untrappable error has occurred which gets us back to
;  the user interface.	This currently happens with:
;	   MSG_GoDirect - User presses CANCEL button in response
;	   to some dialog boxes. (NOTE: this is a case where B$IONERR
;	   can be called from the U.I.)

	mov	[b$errnum],MSG_GoDirect
	jmp	short J2_BIONERR_NoCont_Exit

NotGoDirect:
	cmp	[grs.GRS_fDirect],FALSE	;in direct mode?
	jz	BIONERR_Cont1		;  brif not
					;don't trap errors in direct mode 
					;  statements
J2_BIONERR_NoCont_Exit:
 	mov	[otxSave],0		; reset otxCur to zero, since we're
 					; in direct mode
J1_BIONERR_NoCont_Exit:
	sub	ax,ax			;tell runtime to quit looking for 
					;  an error handler to invoke
	jmp	BIONERR_Exit		;brif special error which gets us
					; back to user interface.
BIONERR_Cont1:
	test	[grs.GRS_flags],FG_WatchActive
	jz	@F			; brif WATCH pcode not executing

	call	far ptr B$FERROR	; don't return to runtime, because
					; that would cause b$inonerr to
					; be reset (and user code might
					; be in an error handler)
@@:					
					
	cmp	[grs.GRS_otxCur],UNDEFINED
	jz	J2_BIONERR_NoCont_Exit	; error must not be trapped,
					; report error at start of text tbl
					; (error occurred after end of text
					; or an END or SYSTEM statement)
	cmp	[b$inonerr],FALSE
	jnz	J1_BIONERR_NoCont_Exit	;brif we're already in an error trap

	cmp	[b$errnum],ER_OM	;Out of memory error?
	jnz	BIONERR_Cont3		;  brif not

	cmp	[b$errinfo],OMErr_STK	;was it really a stack overflow error?
	jz	J1_BIONERR_NoCont_Exit	;  brif so - - - untrappable
BIONERR_Cont3:

	mov	di,[grs.GRS_oMrsCur]	; di points to active mrs in table
	RS_BASE add,di			
	GETRS_SEG es			
	cmp	PTRRS[di.MRS_otxHandler],UNDEFINED	
	jnz	BIONERR_Cont3a		;brif there is a trap to invoke

	;Now, use pGosubLast chain to determine if the current frame is
	;  an event frame. If so, set ax = 0 and exit, else set ax <> 0,
	;  and update otxCur, oRsCur, and pGosubLast for previous qb
	;  frame and exit.  Exception: If b$mainframe == b$curframe,
	;  then don't bother - - - leave ax set as it is and just exit
	;  (because there IS no previous qb frame, and runtime will
	;  immediately see that and quit looping).
	mov	si,[b$curframe]
	cmp	si,[b$mainframe]
	jz	BIONERR_Exit1		;brif this is the first frame on stack
					;  (don't care what ax is here ...)
	mov	bx,[pGosubLast]
Examine_Gosubs:
	or	bx,bx
	jz	NotEventFrame		;brif no gosubs (left)

	cmp	bx,si
	ja	NotEventFrame		; brif no gosubs for current frame

	cmp	word ptr [bx+2],1	;is this an event frame?
	jz	J1_BIONERR_NoCont_Exit	;  brif so - runtime should quit looking

	mov	bx,[bx]			;loop for each gosub in current frame
	jmp	Examine_Gosubs

NotEventFrame:
	;At this point, we know that 
	;	(1) the current frame has no error handler,
	;	(2) the current frame is not an event frame
	;	(3) there is at least one QB frame on the stack previous (above)
	;		this one
	;Therefore, we use our knowledge of what QB frames look like to restore
	;oRsCur, otxCur, and pGosubLast for the next previous frame, set ax to
	;tell the runtime to keep looking for an error handler to activate, 
	;and exit.
	mov	ax,[si].FR_pGosubLast	;pGosubLast for previous frame
	mov	[pGosubLast],ax
	push	[si].FR_oRsRet		;oRs part of return address to next
					;	previous frame
	call	RsActivate		;activate that register set
	mov	ax,[si].FR_otxRet	;otx part of return address to next
					;	previous frame
	mov	[grs.GRS_otxCur],ax	
	mov	ax,sp			;signal runtime "okay to keep looking"
BIONERR_Exit1:
	jmp	BIONERR_Exit		
BIONERR_Cont3a:
	;There is an error trap to invoke; start it going - - -

	;release any owners on the stack below those we should keep for
	;  the current frame. This includes possible frame temp owners for the
	;  current frame.
	call	far ptr ResetSP_IONERR	;release all owners on stack below
					;  most recent QBI frame
	push	dx
	popf
	ja	BIONERR_Cont4		;  brif sufficient stack for context

	mov	[b$errnum],ER_OM
	mov	[b$errinfo],OMErr_STK	;note that this is an out of stack err
	jmp	J1_BIONERR_NoCont_Exit
BIONERR_Cont4:
	mov	sp,ax
	call	far ptr SetERL		;set b$errlin, activate static structs
					;sets grs.otxCur beyond opBos of
					; stmt that caused the error.
					; AX = otx of stmt that caused error


	call	DisStaticStructs	;deactivate static structs again
					; (activated by SetERL)

	mov	[b$inonerr],TRUE
	mov	[fNonQBI_Active],0	;regardless of where error occurred,
					; QB code is active now
	push	[pGosubLast]		;save so RESUME can restore
	push	[grs.GRS_oRsCur]	;oRs of return address	
	mov	di,[grs.GRS_oMrsCur]	; di points to active mrs in table
	RS_BASE add,di			
	GETRS_SEG es			
	mov	ax,PTRRS[di.MRS_otxHandler]	


	xchg	ax,[grs.GRS_otxCur]	;Set error trap context, fetch oTx
					;  of BOS/BOL of line error occured in
	push	ax			;oTx of return address (for RESUME)
	push	bp
	mov	bp,sp
	push	[b$curframe]		
	mov	[b$curframe],bp
	push	[grs.GRS_oMrsCur]
	cCall	RsActivate		;deactivate prsCur if a procedure is
					;  active (must use RsActivate here, 
					;  since static structs are inactive)
	;NOTE: No reason to copy existing module frame vars+temps to and back
	;NOTE: from this new frame, nor to zero them. Frame temps are only
	;NOTE: meaningful within the context of a statement. Frame vars are
	;NOTE: only used by FOR loops; not too worried about what happens if
	;NOTE: user jumps into the middle of a FOR loop; we can't match what
	;NOTE: BC does for that case anyway.

	jmp	StartGrsContext 	;jmp to dispatch first opcode in trap
					;  note that this also sets SP based
					;  on BP and current context, and
					;  checks for stack overflow ...

BIONERR_Exit:				;no trap or in trap; ret to RT
cEnd

;***
;ResetSP_BP_Far
;Purpose:
;	Common code called in case non-QBI code is executing and the user
;	hits ctl-BREAK, a STOP statement is executed, or a runtime
;	error occurs that is not trapped by the compiler.
;
;	If fNonQBI_Active == FALSE, just resets SP and BP based on b$curframe
;	and current context and returns.
;
;	If fNonQBI_Active != FALSE, sets BP to fNonQBI_Active (last active
;	QBI frame), sets SP based on this BP, releases all owners on
;	stack below this frame, restores proper b$curlevel value for this 
;	frame, and decrements the count of non-QBI frames on the stack.
;
;	NOTE: doesn't actually set SP, for ease in returning, and because
;	that hoses caller from B$IONERR. Caller must set sp if desired.
;Entry:
;	b$curframe gives pointer to reset BP to if fNonQBI_Active == FALSE.
;	fNonQBI_Active == FALSE or value to reset BP to.
;	if fNonQBI_Active != FALSE, bcurlevel_QBI assumed to contain the
;		value that b$curlevel is to be reset to.
;	grs.GRS_oRsCur assumed to be set for most recently active QBI frame.
;Exit:
;	AX is set to appropriate location for SP to be set to at each
;		BOS based on BP.
;	dx is a copy of PSW flags, because windows can trash
;		PSW on exit from far routines.
;Uses:
;	di,bp
;Exceptions:
;	none.
;*******************************************************************************
cProc	ResetSP_IONERR,<FAR>
cBegin
	mov	di,[b$curframe]
	jmp	short ResetSP_BP_Cont
cEnd	<nogen>

cProc	ResetSP_BP_Far,<PUBLIC,FAR>	
cBegin	ResetSP_BP_Far			
	mov	di,[b$curframe]
	xor	cx,cx
	xchg	[fNonQBI_Active],cx	;error in non-QBI code?
	jcxz	ResetSP_BP_Cont		;  brif not

	mov	di,cx			;reset bp to most recent QBI frame
	push	[bcurlevel_QBI]		;restore b$curlevel to what it was
	pop	[b$curlevel]		;  when most recent QBI frame was active
	DbAssertRel b$cNonQBIFrames,nz,0,RT,<ResetSP_BP: b$cNonQBIFrames == 0>
	dec	[b$cNonQBIFrames]	;decrement count of non-QBI frames on
					;  the stack
ResetSP_BP_Cont:
	mov	[b$curframe],di
	mov	bp,di

	;Don't release owners in local var space on stack if a procedure
	;  is active
	;Note that MODULE frame var space cannot have owners
	sub	bx,bx			
	RS_BASE add,bx			
	GETRS_SEG es			
	mov	ax,[grs.GRS_oRsCur]	
	DbAssertRel ax,nz,UNDEFINED,RT,<ResetSP_BP: grs.GRS_oRsCur == UNDEFINED>
	or	ax,ax
	jns	Proc_Not_Active		;brif oRsCur is a module, not a proc

	and	ah,07FH			; oRs --> oPrs
	DbChk	ConNoStatStructs	;following statement counts on finding
					;  current prs in table, not prsCur
Proc_Not_Active:
	add	bx,ax			
	.errnz	MRS_cbFrameVars - PRS_cbFrameVars	
	sub	di,PTRRS[bx.PRS_cbFrameVars] ; don't release any local proc variable
					;  owners
	jnc	@F			;[J2]
	mov	di,bp			;[J2]
@@:					;[J2]
	dec	di			; point to first potential value
	dec	di			; to release
	push	[b$pend]		;ptr to low word on stack
	push	di			;ptr to top of range to clear
	call	B$ClearRange		;free all owners below latest QBI frame

	mov	ax,[b$curlevel]
	call	B$STDALCALLTMP		;deallocate all string temp.s that were
					; created above the level for this frame
	call	SetSpFar		;ax = clean BOS SP value based on BP
					;return PSW flags as returned by
					;SetSpFar (in dx)
cEnd	ResetSP_BP_Far			

;***
;B$FERROR - interpreter-specific clean-up when runtime error occurs
;Purpose:
;	The runtime calls this routine when a runtime error occurs that is
;	not trapable, or there is no error trap, or the error occured in
;	an error trap. It cleans the stack frame back to b$curframe and
;	the stack pointer back to where it was at the last BOS/BOL,
;	resets si to the opBOS prior to the otx in grs.otxCur (set by
;	B$IONERR), gives the appropriate error message to the user, and puts
;	QBI in direct mode.
;Entry:
;	b$curframe gives pointer to reset BP to.
;	b$errnum contains the BASIC error number.
;	b$cCSubs non-0 if error occured in compiled BASIC code.
;Exit:
;	none.
;Exceptions:
;	Never returns, just sets the stack up, and jumps ...
;*******************************************************************************
cProc	B$FERROR,<PUBLIC,NEAR,NODATA>
cBegin	B$FERROR
	mov	ax,[b$errnum]
	or	ah,ah			;some internal error?
	jz	Normal_Error		; brif not
	cmp	ax,FE_NOSTACK		; convert FE_NOSTACK to ER_OM?
	jne	Term_Error		; brif not
	mov	[b$errnum],ER_OM	; convert to Out of Memory Error
	mov	[b$errinfo],OMErr_STK	; and set Out of Stack Space flag
Normal_Error:				

	jmp	far ptr BFERROR_CONT	;do the rest of this work in CP
Term_Error:
	call	B$PUTNUM		;print error message to stdout

	;NOTE: We don't try to print out module name, ERL, etc. of message
	;NOTE: here, partly to save code (this is an unusual case), and partly
	;NOTE: because it's pretty risky trying to do much of anything after
	;NOTE: an error such as String Space Corrupt, DOS Arena trashed, etc.
	;NOTE: For the same reason we don't give the user the chance to save
	;NOTE: his program after such an error either; we probably couldn't
	;NOTE: do it, and it's just too risky.

	jmp	B$END			;terminate BASIC

cEnd	<nogen>

sEnd	RT

sBegin	CP
assumes CS, CP

BFERROR_CONT:
	call	ReleaseSpace		;just in case GrabSpace had been called
					;  in exStClear
	call	EnStaticStructs 	;required prior to calling txtmgr stuff
	mov	ax,[b$errnum]
	cmp	ax,ER_OM		;out of memory error?
	jz	BFErr_Clear		;  brif so

	cmp	ax,ER_OS		;out of string space error?
	jnz	BFErr_Cont		;  brif not - only clear if need space
BFErr_Clear:
	call	ClearStmt		;clear all variables to free up memory
	mov	ax,[b$mainframe]
	mov	[b$curframe],ax	;reset this now in case of Out of Stack
					;  space error - - - this ensures
					;  enough stack space for UserInterface
	mov	[BosFlags],0		;don't want to leave the 'reset the
					;  stack' bit set ...
	or	[debugFlags],DEBUG_CANT_CONT
					;remember that we can't continue now
BFErr_Cont:
	xor	cx,cx			;note that we're resetting b$cCSubs
	xchg	[b$cCSubs],cx		;here in addition to testing it; this
					;is necessary in case another runtime
					;error occurs (so B$CONERR doesn't
					;get called when it shouldn't ...)
	mov	si,[grs.GRS_otxCur]	
	cmp	[grs.GRS_fDirect],FALSE ; error in Direct Mode?
	jnz	BFerr_Cont2		;	brif so - leave otx alone

	jcxz	BFErr_InQB_Code		;brif error occured in QB code

	;In the event that the most recent BASIC frame on the stack is
	;  for compiled BASIC code and the error was untrapped, we must
	;  leave ERL alone, but must reset oTxCur back to the previous BOS 
	mov	ax,[grs.GRS_otxCur]
	DbAssertRel ax,nz,UNDEFINED,CP,<B$FERROR: otxCur=FFFF, b$cCSubs non-0>

	dec	ax			;move back to opcode that caused the
	dec	ax			;  error, to ensure we're in the same
					;  statement/line.
	push	ax
	cCall	OtxBosOfOtx		;ax = oTx of BOS/BOL of stmt in which 
					;  the error occured
	mov	[grs.GRS_otxCur],ax
	jmp	short BFErr_Cont1
BFErr_InQB_Code:
 	mov	si,[grs.GRS_otxCur]	
 	cmp	[grs.GRS_fDirect],FALSE	; error in Direct Mode?
 	jnz	BFErr_Cont2		;   brif so - leave otx alone
 
	call	far ptr SetERL		;set b$errlin, activate static structs
					;sets grs.otxCur beyond opBos of
					; stmt that caused the error.
BFErr_Cont1:
	sub	ax,ax
	push	[grs.GRS_otxCur]	;oTx of BOS/BOL
	push	ax			;0 = "skip to next pcode, please"
	call	TxtFindNextOp		;ax = oTx of first opcode past BOS/BOL
	xchg	ax,si			;si = otx of first opcode past BOS/BOL

BFErr_Cont2:				
	sub	ax,ax
	mov	[DimAtScanType],al	; In case of error during Dim
	mov	[grs.GRS_flagsDir],al	;reset flags which get reset every
					; time we begin executing pcode,
					; or when a runtime error occurs.
	call	DisStaticStructs	;deactivate stat structs for ResetSP_BP
	call	ResetSP_BP_Far		;reset SP and BP
	push	dx			;[J2]
	popf				;[J2]
	jbe	B$FERROR_Exit		;brif insufficient stack space for
					;  the module frame (special case)
	mov	sp,ax
B$FERROR_Exit:
	or	[debugFlags],DEBUG_ERROR;tell UserInterface an error occured
	mov	[grs.GRS_flagsDir],0
	jmp	far ptr StopGrsContext 	;back to user interface

;***
;SetERL - set up b$errlin for ERL and otxCur of BOS/BOL where error occured.
;Purpose:
;	updates b$errlin and updates GRS_otxCur to the BOS of stmt in which 
;	the error occurred.  If the error occurred during a single line if
;	the otx past the opStIf or opStElse operand will be used.
;	This function is in CP, since it calls txtmgr routines which are
;	NEAR in CP.
;	Note that, if no line number is found prior to the oTx where the error
;	occured, b$errlin is set to 0.
;Entry:
;	[grs.GRS_otxCur] = oTx just past the opcode which caused the error
;Exit:
;	b$errlin set appropriately
;	Static structs active
;	AX = otx of stmt that caused the error
;Modifies:
;	SI,ES
;*******************************************************************************
cProc	SetERL,<FAR>
cBegin	SetERL
	call	EnStaticStructs 	;required prior to calling OtxLabOfOtx
	mov	si,[grs.GRS_otxCur]
	cmp	si,UNDEFINED
	jnz	SetERL_Cont
	;error occured after end of text or an END or SYSTEM statement
	inc	si
	mov	[b$errlin],si		;ERL = 0 in this case
	jmp	short SetERL_Exit	;this will set otxCur to zero, i.e.,
					;  the error will be reported as if it
					;  occured at the start of the txt tbl
SetERL_Cont:
	or	si,si
	jz	No_oTx_Dec		;special case - - - if we've done a
					;  GOSUB back to oTx 0 and then see
					;  we're out of stack space ...
	dec	si			;move back to opcode that caused the
	dec	si			;  error, to ensure we're in the same
					;  statement/line.
No_oTx_Dec:
	DbAssertRel si,b,0FFFDh,CP,<SetERL: bad grs.otxCur>
	push	si			;popped below by OtxResume
LoopForERL:
	xchg	ax,si			;put current oTx in ax, garbage in si
	cCall	OtxLabOfOtx		;ax = oTx of preceding opBolLab or opLab
	mov	si,ax			;si = ax == oTx op preceding opBolLab
	inc	ax			;no more opBolLab's or opLab's?
.errnz	UNDEFINED - 0FFFFH
	jz	ERL_Set 		;  brif so; leave ERL == 0

	GetSegTxtTblCur			;ES = txdCur.TXD_bdlText_seg
	mov	bx,es:[si+2]		;pass oNam of Ln/Lab from pcode in bx
	cCall	LnOfOnam,<bx>		; ax = line # or UNDEFINED if label
	inc	ax
.errnz	UNDEFINED - 0FFFFH
	jz	LoopForERL		;found a label, not a Ln - try again

	dec	ax			;set this back to actual line number
ERL_Set:
	mov	[b$errlin],ax
	cCall	OtxResume		;oTx of error pcode still on stack
	;now ax = oTx of BOS/BOL of statement in which the error occured
SetERL_Exit:
	mov	[grs.GRS_otxCur],ax
cEnd	SetERL

sEnd	CP

sBegin	CODE
assumes CS, CODE

;***
;RtErrorCODE - Near interface for runtime error in CODE segment
;Purpose:
;	This entry point is jumped to from any point in the CODE segment where
;	a runtime error is detected. Having a NEAR interface like this saves
;	code.
;Entry:
;	AL is the error code of the runtime error to generate
;Exit:
;	None - doesn't return
;*******************************************************************************

PUBLIC	RtError_INI
PUBLIC	RtErrorOM_INI
;here for the special case of some internal error
RtError_Initialization	PROC FAR
RtError_INI:
	DbAssertRelB  ah,z,0,CODE,<RtError_INI called with ax GT 255>
	xchg	al,bl			;put error code in bl
	SKIP2_PSW
RtErrorOM_INI:
	mov	bl,ER_OM
					;OM error during initialization
	mov	bh,QB_RTERR_MASK	;special mask - - - fatal error, but
					; allows us to use an existing message
					; string
	push	ss			;in case error occured before DS was
	pop	ds			;  set up originally
	call	B$RUNERRINFO		;doesn't return
	DbHalt	CODE,<B$RUNERRINFO returned when passed a fatal error (1)>
RtError_Initialization	ENDP

PUBLIC	RtErrorCODE
RtErrorCODE PROC NEAR
RtErrorCODE ENDP
	;fall into RtError
PUBLIC	RtError
RtError PROC FAR
	mov	[grs.GRS_otxCur],si	;just as executors save this away prior
					;  to calling runtime ... for error
RtError ENDP				;  recovery
	;fall into RtErrorNoSi
PUBLIC	RtErrorNoSi
RtErrorNoSi PROC FAR
	xor	ah,ah			;so callers only have to set AL
	cmp	al,MSG_GoDirect
	jnz	RtErrorNoSi_Cont1

	mov	ax,FE_GODIRECT
RtErrorNoSi_Cont1:
	xchg	ax,bx
	call	B$RUNERRINFO
	DbHalt	CODE,<B$RUNERRINFO returned when passed a fatal error (2)>
RtErrorNoSi ENDP

;***
;exStOnError, exStResume, exStResume0, exStResumeNext
;
;	Moved to this module as part of revision [18].
;
;****
	EXTRN	SetProgramMode:NEAR	

MakeExe exStOnError,opStOnError
	LODSWTX 			;load oTx of error handler to ax
	mov	bx,[grs.GRS_oMrsCur]
	RS_BASE add,bx			;bx points to mrsCur in the Rs table
	GETRS_SEG es
	mov	PTRRS[bx.MRS_otxHandler],ax  ;save ON ERROR oTx (or UNDEFINED)
	inc	ax
	jz	OnError_GoTo_0		;brif line number 0 specified
OnError_Exit:
	jmp	DispMov 		;ensure es gets restored in dispatch

OnError_GoTo_0:
	cmp	[b$inonerr],0
	jz	OnError_Exit		;brif not in an error handler
					; otxHandler is already set to UNDEFINED
	;ON ERROR GOTO 0 in an error handler - - - cause runtime error.
	mov	al,byte ptr [b$errnum]
	call	RtErrorCODE


MakeExe exStResume,opStResume
	LODSWTX 			;fetch oTx for resume Lab/Ln
	cmp	ax,UNDEFINED
	jz	exStResume0		;brif oTx is UNDEFINED - RESUME 0

	test	[bp+4],08000H		;did error occur in a procedure?
	jnz	Resume_Proc		;  brif so - leave this frame active

	mov	[bp+2],ax		;oTx we want to resume to
	jmp	short exStResume0	;pop back to previous module frame
					;Note that we need to do this so that
					;  a subsequent CLEAR or RUN won't cause
					;  a runtime error.
Resume_Proc:
	push	ax			;ax = oTx to RESUME to (@ module level)
	xor	ax,ax			;tell B$RESUMED to dealloc all string
	mov	[grs.GRS_oTxCur],si	;in case of runtime error
	call	B$RESUMED		;call RT to reset b$inonerr, b$errnum.
					;  if not currently in handler, gives
					;  RT error and doesn't return (note
					;  that AX is a parm to B$RESUMED)
	pop	si			;put oTx to resume at in SI
	jmp	short Resume_Common


MakeExe exStResumeNext,opStResumeNext
	mov	di,sp			;remember this is RESUME NEXT
	jmp	short Resume_Common1
	
MakeExe exStResume0,opStResume0
	xor	di,di			;remember this is RESUME0
Resume_Common1:
	mov	[grs.GRS_oTxCur],si	;in case of runtime error
	DbAssertRel ax,nz,0,CODE,<exStResume<Next|0>: ax == 0>
	call	B$RESUMED		;call RT to reset b$inonerr, b$errnum.
					;  if not currently in handler, gives
					;  RT error and doesn't return (note
					;  that AX is a parm to B$RESUMED)
	mov	sp,bp
	pop	bp	
	pop	si			;oTx part of return address
	mov	[b$curframe],bp
	pop	ax
	call	RsActivateCODE		;activate Rs where error occured
	pop	[pGosubLast]		;update in case any GOSUBs occured and
					;  were not returned from
	or	di,di			;RESUME NEXT, or RESUME [0] ?	
	jz	Resume_Common2		;  brif RESUME [0]

	call	SetProgramMode		;in case of a RETURN from direct mode
					;  must do this prior to call to 
					;  OtxResumeNext to ensure ES setup
					;  for module text table, not directmode
	call	EnStaticStructs		;bring txdCur up to date
	xchg	ax,si
	push	ax			;push otx arg.
	call	OtxResumeNext		;ax = oTx of BOS/BOL of following stmt
	xchg	ax,si
	call	DisStaticStructs
Resume_Common2:	
	call	SetSP			;set SP to where it should be at BOS
					;  based on bp
	mov	sp,ax
Resume_Common:
	call	SetProgramMode		;in case of a RETURN from direct mode
					;  also updates ES & DI for execution
	cmp	[grs.GRS_fDirect],FALSE ;RESUME from Direct Mode?
	jnz	DirectMode_Resume	;  brif so
Disp1:
	DispMac 			; and on with the show.

CantCont_Err:
	mov	al,ER_CN		;"Cant Continue" error
	call	RtErrorCODE


	EXTRN	Cont_Otx:NEAR		;part of exStCont code

DirectMode_Resume:
	cmp	[grs.GRS_otxCONT],UNDEFINED
					;exists context that can be CONTinued?
	jz	CantCont_Err		;  brif not - - issue 'Cant Continue'

	jmp	Cont_Otx		;share code with exStCont

sEnd	CODE
;===============================================================================
subttl	Error Trap Code for Direct Mode Runtime Users
page

sBegin	CP
assumes CS, CP

;***
;CallRtTrap, CallRtTrap_Parm, CallRtTrap_RT, CallRtTrap_CODE
;Purpose:
;	This calls a specified function, trapping runtime errors.
;
;	The current trap handler as set by RtSetTrap is preserved.
;Entry:
;	pFunc = far address of function to call (must be FAR function)
;	si,di	passed to specified function
;	For CallRtTrap_Parm, bx is a parm that must be pushed on the stack,
;		i.e., passed to pFunc.
;Exit:
;	ax =  0 if no runtime error occurred
;	      or
;	      standard error code if runtime error occurred
;	si,di as returned from the specified function if no error.
;	      or garbage
;Exceptions:
;	none
;
;*******************************************************************************
	PUBLIC CallRtTrap_Parm
	PUBLIC CallRtTrap
CallRtTrap_Parm:
	mov	dx,sp			;signal that bx contains a parm to pass
	SKIP2_PSW
CallRtTrap:
	xor	dx,dx			;no parm to pass to pFunc
cProc	CallRtTrap_Common,<FAR>
	parmD	pFunc
cBegin
	call	RtPushHandler		;save current handler on stack
					; NOTE: alters SP
	mov	ax,CPOFFSET CallTrapped
	call	RtSetTrap
	or	dx,dx			;parm to pass to pFunc?
	jz	CallRt_NoParm		;brif not

	push	bx			;pass given parm on to pFunc
CallRt_NoParm:
	call	[pFunc]
	sub	ax,ax			;error code = 0
CallTrapped:
	call	RtPopHandler		;restore caller's handler from stack
					; NOTE: alters SP
cEnd
;start of revision [10]
sEnd	CP

sBegin	RT
assumes CS, RT
cProc	CallRtTrap_RT,<PUBLIC,FAR>
	parmW	pFunc
cBegin
	push	cs
	push	[pFunc]
	call	far ptr CallRtTrap
cEnd
sEnd	RT

sBegin	CODE
assumes CS, CODE
cProc	CallRtTrap_CODE,<PUBLIC,FAR>
	parmW	pFunc
cBegin
	push	cs
	push	[pFunc]
	call	far ptr CallRtTrap
cEnd
sEnd	CODE

sBegin	CP
assumes CS, CP
;end of revision [10]

;***
;RtSetTrap
;Purpose:
;	This is called by a function which is about to make a series
;	of runtime calls, and wants to identify one location to
;	branch to if any of them result in runtime errors.  In BASIC
;	terminology, this is equivalent to doing an ON ERROR GOTO [ax].
;	The handler remains active until RtSetTrap is called again,
;	RtFreeTrap is called, or a runtime error occurs.
;
;	When the handler is branched to, the sp, es and di registers
;	are set as though it had just returned from RtSetTrap.
;	The si and ds registers are set as though it had just returned
;	from the runtime call which resulted in the error.
;	Register al contains the standard QB4 error code.
;
;	NOTE: Internal errors such as String space corrupt ALWAYS cause
;	NOTE: QB4 to print the error message, and terminate.
;Entry:
;	ax = offset into CP segment to error handler
;	bp = value to restore bp to if error is trapped
;	si = value to restore si to if error is trapped (optional)
;	di = value to restore di to if error is trapped (optional)
;Exit:
;	none
;Preserves:
;	bx,cx,dx
;Exceptions:
;	none
;
;*******************************************************************************
PUBLIC	RtSetTrap
RtSetTrap PROC NEAR
;Make sure we aren't over-writing someone else's trap.
;If so, should use RtPushHandler and RtPopHandler
DbAssertRel [errCodeRet.ERRRET_saveSP],e,0,CP,<RtSetTrap: trap already set>
	mov	[errCodeRet.ERRRET_saveBP],bp	;to restore bp on error
	mov	[errCodeRet.ERRRET_saveSI],si	;to restore si on error
	mov	[errCodeRet.ERRRET_saveDI],di	;to restore di on error
	mov	[errCodeRet.ERRRET_retAddr],ax	;return address to handler
DbAssertRel [b$fInt24Err],ne,0,CP,<RtSetTrap: int24 trap already set>	
	mov	[b$fInt24Err],0		; have runtime handle int24 errors
	pop	ax				;ax = ret adr to caller
	mov	[errCodeRet.ERRRET_saveSP],sp	;set SP back to here on error
	jmp	ax				;return to caller
RtSetTrap ENDP

;***
;RtTrapRet
;Purpose:
;	Jumped to from B$IONERR when an error occurs with trap set
;Entry:
;	ax = standard runtime error code
;	cx = errCodeRet.ERRRET_saveSP
;Exit:
;	none
;Exceptions:
;	exits via RtFreeTrap to save code
;
;*******************************************************************************
RtTrapRet PROC FAR

	push	ax			;save error code for return
	push	cx			;save new sp
	push	[b$pend]		;ptr to low word on stack
	dec	cx			;clear range is inclusive...
	dec	cx			;...so adjust to not trash TOS.
	push	cx			;ptr to top of range to clear
	call	B$ClearRange		;free all owners on stack
	pop	cx			;get back new sp
	pop	ax			;recover error code

	;restore bp,si,di,sp as they were on runtime entry
	mov	si,[errCodeRet.ERRRET_saveSI]
	mov	di,[errCodeRet.ERRRET_saveDI]
	mov	bp,[errCodeRet.ERRRET_saveBP]
	mov	sp,cx
	mov	[b$errnum],0			;so ERR is 0 after CHAIN and
						;  RUN <filespec> and to prevent
						;  similar potentially confusing
						;  values of ERR.
	push	[errCodeRet.ERRRET_retAddr]	;set up for near return
	;fall into RtFreeTrap, to free the trap and return to caller with
	;  error code in ax
RtTrapRet ENDP

;***
;RtFreeTrap
;Purpose:
;	Removes the error handler installed by RtSetTrap.  In BASIC
;	terminology, this is equivalent to doing an ON ERROR GOTO 0.
;Entry:
;	none
;Exit:
;	none
;Preserves:
;	AX
;Exceptions:
;	none
;
;*******************************************************************************
PUBLIC	RtFreeTrap
RtFreeTrap PROC NEAR
	mov	[errCodeRet.ERRRET_saveSP],0	;reset, so not used incorrectly
;Comment out this assertion so we can do two RtFreeTrap's in a row
;DbAssertRel [b$fInt24Err],e,0,CP,<RtFreeTrap: int 24 trap not set> 
	mov	[b$fInt24Err],UNDEFINED	; don't have runtime handle int24
					; error processing anymore (trashes
					; any pending int 24 errors)
	ret
RtFreeTrap ENDP

;***
;RtPushHandler
;Purpose:
;	Save the state of the runtime-error-handler (as set by RtSetTrap)
;	on the stack.
;Preserves:
;	ax,bx,dx
;NOTE:
;	Exit SP < Entry SP
;
;*******************************************************************************
PUBLIC	RtPushHandler
RtPushHandler PROC NEAR
	pop	cx			;cx = return address
	push	[b$fInt24Err]		; also save int 24 error handler 
	sub	sp,size ERRRET		;Make space for handler
	push	cx
	push	si
	push	di
	push	es
	mov	si,DATAOFFSET errCodeRet
	mov	cx,size ERRRET/2
	mov	di,sp			;Copy into stack
	add	di,8			;Add 4 pushes above
	push	ss
	pop	es
rep	movsw				;Copy handler into stack frame
	pop	es
	pop	di
	pop	si
	ret
RtPushHandler ENDP

;***
;RtPopHandler
;Purpose:
;	Restore the state of the runtime-error-handler that was saved
;	by RtPushHandler
;Preserves:
;	ax,bx,dx
;NOTE:
;	Exit SP < Entry SP
;
;*******************************************************************************
PUBLIC	RtPopHandler
RtPopHandler PROC NEAR
	push	si
	push	di
	push	es
	mov	di,DATAOFFSET errCodeRet
	mov	cx,size ERRRET/2
	mov	si,sp			;Copy from stack
	add	si,8			;Add 3 pushes above + return addr
	push	ss
	pop	es
rep	movsw				;Copy handler from stack frame
	pop	es
	pop	di
	pop	si
	pop	cx			;Return address
	add	sp,size ERRRET		;Remove space for handler
	pop	[b$fInt24Err]		; also restore int 24 error handler 
	jmp	cx			;Return to caller
RtPopHandler ENDP
sEnd	CP

	end
