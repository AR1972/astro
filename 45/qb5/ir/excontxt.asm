page	49,132
	TITLE	EXCONTXT - context-related executors
;***
;excontxt.asm - context-related executors
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;Special Information regarding [re]initialization:
;     - This module contains the fundamental entry point to which
;	1-time initialization branches.
;     - When UserInterface() is called, this represents a major transfer
;	of control; the User Interface takes over. It saves away the
;	keyboard interrupt handler that the runtime installed at
;	1-time initialization, and restores it prior to returning here.
;	Within the User Interface (UI), ES is no longer preserved, and
;	one may not assume that SI or DI represent anything in particular.
;     - Some executors in this module will behave differently in our
;	full shell version than in the "Runtime Module" version, i.e.,
;	the version which can just binary load QBI programs and run them.
;	Executors which terminate execution are at the head of this list
;	rather than returning to the shell, in the Runtime Module version,
;	these will cause QBI to terminate and exit to DOS.
;     - Note that anyone that changes grs.fDirect sets up a 'critical
;	section' situation: if a runtime error occurs after grs.fDirect
;	has been changed, and grs.oTxCur is not pointing to some exBol/Bos
;	opcode in the text table owned by txdCur, runtime error handling
;	will go out to lunch.
;     - The way we allow for RUN <filename> and CHAIN to load a file and
;	get it scanned is to have them call LoadFile, then put an actual
;	executor address in the direct mode buffer, set a bit in debugFlags,
;	and just dispatch. We know that the following BOS/BOL/EOT will cause
;	UserInterface() to regain control; UserInterface() will see the
;	debugFlags bit set, and cause the direct mode buffer to be executed.
;
;****************************************************************************

	.xlist
	include 	version.inc
EXCONTXT_ASM = ON
	IncludeOnce	architec
	IncludeOnce	conint
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opcontrl
	IncludeOnce	opmin
	IncludeOnce	opstmt
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	rtps
	IncludeOnce	rttemp
	IncludeOnce	scanner
	IncludeOnce	txtmgr
	IncludeOnce	ui
	IncludeOnce	variable
	.list

assumes DS, DATA
assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
	externW b$cCSubs	;count of compiled BASIC frames on stack
	externB b$inonerr	;used to check for No RESUME error @ implied END
	externW	b$errnum
	externW b$ErrInfo	;extended error codes (UI <--> Runtime)
	externB fInitialized	;non-zero if we've completed initialization of
				;  quick basic
	externB b$CtrlFlags	;byte of runtime-specific flag bits
	staticW oTxRunSave,0	;!#@$ static location to save oTxCur before
				;RunFile or CHAIN.

sEnd	DATA

	EXTRN	B$IEND:FAR
	EXTRN	B$IEndSt:FAR
	EXTRN	B$SCLR:FAR
	EXTRN	B$ClearRange:FAR
	EXTRN	B$STACKINIT:FAR
	EXTRN	RtError_INI:FAR

	EXTRN	fEditorActive:FAR 

sBegin	CODE
assumes cs, CODE

	EXTRN	DoFBosResetStk:near
	extrn	I4ToU2:near

;***
;exStop, exStEnd, exStSystem - STOP, END, & SYSTEM Statement Executors
;
;	Note that, for STOP, UserInterface will save the CONTinue context
;	we don't have to do that here.
;
;	Note that for compiled BASIC STOP and END/SYSTEM, the runtime
;	will do a jump to B$IStop or exStEnd.
;
;	Note that SI, DI, & ES may not be correctly set on entry to B$IStop.
;
;***************************************************************************
	PUBLIC	B$IStop		;runtime call-back if ctrl-Break is
B$IStop:				;  seen, or compiled Basic STOP stmt
	mov	al,[grs.GRS_fDirect]	
	or	al,al			; in direct mode window?
	jnz	SkipCursorStuff 	; yes, skip cursor positioning
	push	ax			; save direct mode flag because
					;	OtxBosOfOtx hoses fDirect

	call	EnStaticStructs		;bring txdCur up to date
	push	[grs.GRS_otxCur]	;parm to OtxBosOfOtx
	call	OtxBosOfOtx		;ax = oTx of BOS/BOL of current stmt
					;we do this so we don't try to continue
					;execution in the middle of a statement
	xchg	ax, bx			; bx = otx of pcode to skip.
	call	TxtSkipOpFar		;ax = oTx of pcode following BOS/BOL.
					;  we do this so txtmgr can tell for
					;  sure which line to put the cursor on
	xchg	ax,si
	pop	ax
	mov	[grs.GRS_fDirect],al	;in case OtxBosOfOtx changed fDirect
	call	DisStaticStructs
SkipCursorStuff:			
	and	[BosFlags],NOT FBOSSTOP
	mov	bp,[b$curframe]
	SkipExHeader
MakeExe exStStop,opStStop
	or	[debugFlags],DEBUG_STOP ;indicate program stopped
	cmp	[fNonQBI_Active],FALSE	;was QBI code active when we stopped?
	jz	StopGrsContext		;brif so

	;We were stopped in QuickLib code. We cannot CONTinue, but we
	;do want to be able to allow the user to print variables, and stack 
	;trace from the most recent QBI frame back.
	or	[debugFlags],DEBUG_CANT_CONT
	call	ResetSP_BP_Far		; reset SP & BP based as appropriate
					;  depending on whether QBI code
					;  was active or not.
	mov	sp,ax			;set SP based on retval
	jmp	SHORT StopGrsContext

MakeExe exStSystem,opStSystem
	test	[cmdSwitches],CMD_SW_RUN ;/Z filename given on command line?
	jnz	Exit_Basic		 ;  brif so - - always exit

	cmp	[grs.GRS_fDirect],FALSE ;in Direct Mode?
	jz	exStEnd 		;  brif not - - act just like END stmt

Exit_Basic:
	or	[debugFlags],DEBUG_END	; indicate end-of-program, to ensure
					;  error not trappable in case of
					;  I/O error
	cCall	NotSaved		;notify user, get his response.
					; if CANCEL, ax=MSG_GoDirect
					; if user said NO, ax=FFFF
					; if I/O error, ax = error code
					; if files saved ok, ax=0
	jg	J2_RtErrorCODE		;brif runtime error while
					; saving files, or CANCEL
	call B$IEND			;tell runtime to terminate - - never
					;  returns
MakeExe exStEnd,opStEnd
	or	[debugFlags],DEBUG_CANT_CONT
					;make sure UserInterface() doesn't
					; let us continue execution
	mov	[grs.GRS_otxCur],si	; in case of error in closing files
	or	[debugFlags],DEBUG_END	; indicate end-of-program, to ensure
					;  errors aren't trappable
	call	B$IEndSt		;close all user files
	SkipExHeader			;fall through except for closing files,
					;  exEndProg is an implied END executor
;***
;exEot, exEndProg - End of Program Executor
;Purpose:
;	exEndProg is the main interpreter loop control.  It calls the
;	front end to obtain work.  It then executes the current text
;	table.	All text tables contain an exEndProg after the user's pcode.
;	The loop is completed by execution of the exEndProg at the end of some
;	text table.
;
;***************************************************************************
MakeExe exEot,opEot
	SkipExHeader
MakeExe exEndProg,opEndProg
	cmp	[b$inonerr],FALSE
	jz	EndProg1		;brif not in an error trap

	cmp	[grs.GRS_fDirect],FALSE
	jnz	EndProg1		;brif EOT in Direct Mode buffer

	mov	al,ER_NR		;No RESUME error
J2_RtErrorCODE:
	call	RtErrorCODE		;runtime error - doesn't return
EndProg1:
	;if something like NEW happened as the last statement, the stack won't
	;  have been cleared yet (and who knows, we might have something
	;  meaningful on the stack when we hit our next BOS/BOL after
	;  UserInterface(?)
	test	[BosFlags],FBOSRESETSTK
	jz	Ep_Cont

	call	DoFBosResetStk		;reset the stack
Ep_Cont:
	or	[debugFlags],DEBUG_END	;indicate end-of-program
	mov	si,UNDEFINED		;Save context (end of program)
	;fall into StopGrsContext
PUBLIC	StopGrsContext
StopGrsContext:
	mov	[grs.GRS_otxCur],si	;Save text pointer
	mov	[b$curframe],bp	;required by RT error handling code

	DbChk	BpChain			;ensure the bp chain is walkable
	DbAssertRel  sp,a,b$pendchk,CODE,<StopGrsContext: SP below b$pendchk>
					;ensure sufficient stack space for U.I.
	cmp	[b$errnum],ER_OM
	jz	Enter_UI		;brif might already be short on space
	call	GrabSpace		;ensure there's enough free space to
					;  at LEAST do a CLEAR ...
	or	ax,ax
	jnz	Enter_UI		;brif allocations successful
OM_Err:
	mov	al,ER_OM		;don't enter UI if we can't even
	jmp	J2_RtErrorCODE		;  get enough space to do a CLEAR - -
Enter_UI:
	call	ReleaseSpace		;release special space allocated by
					;  GrabSpace
	call	EnStaticStructs 	;activate static prsCur, mrsCur, txdCur

	test	[debugFlags],DEBUG_ERROR OR DEBUG_STOP OR DEBUG_END
	jz	Enter_UI_1		;brif not going back into UI to stay
					;  a while ...
	call	FreeAllUnrefdPrs	;Remove any prs's that arn't ref'd
					;  anywhere AND that don't have text
					;  tables.  This is needed in case
					;  the user called a prs in a QuickLib
					;  from direct mode, and this was the
					;  only reference to the prs.
Enter_UI_1:
	mov	bx,dataOFFSET prsCur
	test	byte ptr [grs.GRS_oRsCur+1],080H
	jnz	PrsIsActive

	mov	bx,dataOFFSET mrsCur
PrsIsActive:
	.errnz	MRS_cbFrameTemp - PRS_cbFrameTemp
	mov	ax,[bx.MRS_cbFrameVars]
	push	ax			;save in case more vars added to frame
	add	ax,[bx.MRS_cbFrameTemp]
	push	ax			;save in case frame size grows
	DbMessTimer	CODE,<Enter UserInterface() - >
	cCall	UserInterface		;Call direct mode for more work
	DbMessTimer	CODE,<Leave UserInterface() - >
	cmp	[grs.GRS_fDirect],FALSE
	jnz	No_Stk_Reinit		;  brif Direct mode

	mov	bx,sp			;reset empty words in stack for FRE(-2).
	sub	bx,4			;so cbFrameVars and cbFrame on stack 
					;  arn't hosed ...
	call	B$STACKINIT		;Must do this here as UserInterface
					;  is called one last time AFTER RunInit
No_Stk_Reinit:
	pop	si			;previous frame size for oRsCur
	pop	di			;previous cbFrameVars for oRsCur
					;(popping this here, as B$STACKINIT 
					;  trashes di ... pfthhhhhhhhhhp!)
	xor	bx,bx
	mov	[fNonQBI_Active],bx	;ensure this flag is reset
	cmp	[grs.GRS_otxCONT],UNDEFINED
	jnz	GetpRsCur		;brif can CONT

	;we must set SP based on BP so direct mode statments have the 
	;required amount of temp and var space on the frame. Prior to
	;doing this, however, we must release any owners on the stack - - -
	;we can't wait for the first BOS/BOL to occur, because resetting
	;sp means that we could overwrite an owner ...
	cmp	[grs.GRS_fDirect],FALSE	;executing in the direct mode buffer?
	jz	GoStartGrsContext    ; brif not - - - can't be any owners
					;  that we could hose here, and parms
					;  from Direct Mode CALL must not be
					;  released (example: call foo("bar")
					;  from direct mode)
	push	[b$pend]		;lomem of range to clear
	push	[b$mainframe]		;himem of range to clear
	call	B$ClearRange		;release any owners in given range
GoStartGrsContext:			; got relative jump out of range
	jmp	short StartGrsContext	

GetpRsCur:
	mov	bx,dataOFFSET prsCur
	test	byte ptr [grs.GRS_oRsCur+1],080H
	jnz	PrsIsActive1

	mov	bx,dataOFFSET mrsCur
PrsIsActive1:
	push	ds			;for later rep mov's and/or sto's
	pop	es
	cmp	[pGosubLast],0		;active pGosubLast chain?
	jz	Check_FrameVar_Size	;  brif not
	cmp	bp,[pGosubLast] 	; is the chain in this proc?
	jb	Check_FrameVar_Size	; no, then it's sure not active!

	.errnz	MRS_cbFrameVars - PRS_cbFrameVars
	.errnz	MRS_cbFrameTemp - PRS_cbFrameTemp
	mov	ax,[bx.MRS_cbFrameVars]
	add	ax,[bx.MRS_cbFrameTemp]
	mov	dx,ax
	sub	dx,si
	jbe	Check_FrameVar_Size	;brif sufficient frame space exists

	;Frame size has been increased; must move all gosub frames down to
	;account for the difference
	;Do this in two steps:
	;	(1) update pGosubLast chain to account for movement
	;	(2) move all gosub frames down.
	;Note that there can be an event gosub frame here, but if so, it
	;	has to be the active mrs frame - - - we can treat this the
	;	same way.
	push	di
	mov	di,[pGosubLast]
Update_Gosub_Ptrs:			;loop for step (1)
	mov	cx,[di]			;fetch ptr to next frame
	jcxz	Move_Gosub_Frames	;brif no more gosub frames
	sub	[di],dx			;update current frame ptr by amount
					;  we're moving frames down by
	mov	di,cx			;go on to next frame
	jmp	short Update_Gosub_Ptrs

Move_Gosub_Frames:			;now step (2), move frames down
	mov	cx,bp
	sub	cx,si			;cx points just past the end (himem)
					;  of range to be moved down
	xchg	ax,dx			;ax = amt to move gosub frames down by
	mov	si,[pGosubLast]		;start of range to move
	mov	di,si
	sub	di,ax
	mov	[pGosubLast],di
	sub	cx,si			;cx = number of bytes to move down
	pop	dx			;must pop this BEFORE we alter sp
	sub	sp,ax			;so interrupts won't overwrite gosub
					;  frames
	rep	movsb
	mov	di,dx			;di = old cbFrameVars
Check_FrameVar_Size:
	.errnz	MRS_cbFrameVars - PRS_cbFrameVars
	mov	dx,di			;dx = old cbFrameVars
	mov	cx,[bx.MRS_cbFrameVars]
	sub	cx,dx                   ;if cx > 0 then new vars have been added
	DbAssertFlags  ae,CODE,<excontxt: cbFrameVars got smaller!>
	jbe	Vars_Okay		;brif none have been added

	;cx = number of bytes worth of new frame vars added to current frame
	mov	al,0
	mov	di,bp
	sub	di,[bx.MRS_cbFrameVars]	;di points to start of new frame vars,
	cmp	sp,di
	jb	Got_SP			;brif sp was moved down when gosub
					;  frames were
	mov	sp,di			;so interrupts won't overwrite this
					;  after the stosb
Got_SP:
	rep	stosb			;initialize new frame vars

Vars_Okay:
	;fall into StartGrsContext
public	StartGrsContext
StartGrsContext:
	call	DisStaticStructs	;ensure static structs deactivated
	call	SetSP			;set SP based on BP	
	ja	StartGrs_Cont		;brif SP is in valid stack range

	mov	[b$ErrInfo],OMErr_STK	;out of stack space error
	jmp	OM_Err
StartGrs_Cont:
	mov	sp,ax		
	mov	si,[grs.GRS_otxCur]	;Load the continuation address
	xor	ax,ax
	mov	[grs.GRS_flagsDir],al	;reset flags which get reset every
					; time we begin executing pcode,
					; or when a runtime error occurs.
public	Start
Start:
	call	GetEsDi
Disp2:
	mov	[b$curframe],bp	;in case b$curframe got changed by
					;  the U.I. and not restored
	DbChk	BpChain			;ensure the bp chain is walkable

	DispMac 			; and begin execution

;***
;exStRunMain - Start program execution of the main module text
;Purpose:
;	Perform the RUN statement (no arguments) - begin program execution.
;
;Input:
;Output:
;Modifies:
;***************************************************************************
MakeExe exStRunMain,opStRunMain
	xor	di,di			;Set context to beginning of text tabl
					;  (put this in di in case of runtime
					;   error)
	mov	ax,[grs.GRS_oMrsMain]
	DbAssertRel ax,ne,UNDEFINED,CODE,<exStRunMain: grs.oMrsMain UNDEFINED>
Run_Otx:
	cCall	RsActivateCODE		;activate the main module
	cmp	[grs.GRS_fDirect],FALSE
	jz	RunMain1		;brif from pcode - - - must reinit stack

	or	[b$CtrlFlags],NoSTACKINIT
					;speed optimization for the case where
					;  RUN is given from UI. Don't bother
					;  to init. stack in RunInit, since
					;  we will go back into UI and init.
					;  the stack on exit when we actually
					;  start running this program.
RunMain1:
	cCall	RunInit 		;context mgmt work to prepare to RUN
	and	[b$CtrlFlags],NOT NoSTACKINIT
					;reset to default
	mov	si,di			;put oTx in si where it belongs
	mov	bp,[b$mainframe]	;reset frame and stack pointers
 	mov	word ptr [bp],0		;reinitialize end of bp chain
 	mov	word ptr [bp].FR_basBpLink,0	
					; reinitialize end of BASIC bp chain
	mov	sp,bp
	mov	[b$curframe],bp	;required by RT error handling code
	mov	bx,[grs.GRS_oMrsCur]	
	RS_BASE add,bx			
	GETRS_SEG es			
	mov	cx,PTRRS[bx.MRS_cbFrameTemp]	
	add	cx,PTRRS[bx.MRS_cbFrameVars]	
	sub	sp,cx			;make room for module level frame stuff
	DbAssertTst   sp,z,1,CODE,<excontxt.asm: (1) SP contains an odd number>
	or	ax,ax			;error return from RunInit?
	jz	ContOTx1		;  brif not
	jmp	short J1_RtErrorCODE	;error occured in RunInit - abort RUN

ContOtx1:
	mov	[grs.GRS_fDirect],FALSE ;Move to program mode
	jmp	Start			;Load segment & msv, check BosFlags & go

;***
; Cont_Otx
; Entry:
;	si = otx to start executing pcode
;****
PUBLIC	Cont_Otx
Cont_Otx:
DbAssertRel grs.GRS_oRsCur,z,grs.GRS_oRsCONT,CODE,<excontxt.asm: attempt to CONTinute where oRsCur != oRsCONT>
	mov	al,ER_CN		;"Cannot Continue" error
	inc	si			;test for UNDEFINED
	je	J1_RtErrorCODE		;brif otx = FFFF (can't continue)
	dec	si			;ax = otx
	call	ContReinit		;compress Bd's & Bdl's, reinit 80[2]87
	jmp	SHORT ContOtx1

page


;***
;SetProgramMode
;Purpose:
;	Common initialization for all executors which (could) cause
;	start of execution in the current text table from Direct Mode.
;Input:
;	global state variables grs.GRS_fDirect, grs.GRS_otxCONT,
;	mrsCur
;Output:
;	CX is set to what grs.GRS_fDirect HAD been on input (fDirect is always
;	set false).
;	ES & DI set by a call to GetEsDi.
;Modifies:
;	ES, DI, BX, CX
;Preserves:
;	ax, si
;**********************************************************************
PUBLIC	SetProgramMode
SetProgramMode	PROC NEAR
	cmp	[grs.GRS_fDirect],FALSE
	jz	Direct_Exit		;brif fDirect already was false

	push	ax			;preserve across call
	call	ContReinit
	pop	ax
Direct_Exit:
	xor	cx,cx
	xchg	cl,[grs.GRS_fDirect]	;set fDirect to FALSE, cl to previous
	call	GetEsDi 		;es = text segment, di = pTVar
					; ax, cx and dx unmodified
					;  fDirect value
	ret
SetProgramMode	ENDP

;***
;exStRunLabel - RUN <line #> executor
;Purpose:
;	Run the current module from the given line number.
;Input:
;	An oTxt in the pcode stream at which to begin execution.
;Output:
;	none.
;Modifies:
;***************************************************************************
MakeExe exStRunLabel,opStRunLabel
	LODSWTX 			;ax = oTx of Ln|Label
	xchg	ax,di			;put oTx in di (not si, in case a
					;  runtime error occurs in Run_Otx)
	mov	ax,[grs.GRS_oMrsCur]	;in case we're in a procedure
	jmp	Run_Otx			;oTx set - start program

J4_RtErrorCODE: 			
; If we have gotten an error from LoadFile before we threw away
; our current context then the error is still trappable.  We
; don't want to reset the stack because an error handler could
; be invoked.
; otxRunSave = otxCur before LoadFile  si = otxCur after LoadFile.
; If they are the same then LoadFile didn't toss our context.

	cmp	[otxRunSave],si 	;can we trap this error?
	je	J1_RtErrorCODE		;brif so, don't blast stack

	mov	[b$ErrNum],ax		;preserve error code across call
	call	DoFBosResetStk		;bash stack so we don't try to
					;  return to code we've thrown out
	mov	ax,[b$ErrNum]		;restore error code

J1_RtErrorCODE:
	call	RtErrorCODE		;error code already in ax

;	Note: Above call does not return

GrabFailed:				
	mov	ax,ER_OM		; set up out of memory return value
	cmp	[fInitialized],FALSE	
J5_RTErrorCODE: 			
	jnz	J4_RtErrorCODE		; brif not a command-line run
	push	ax			; save error code
	call	ShowOutScr		; erase debug screen
	pop	ax			; restore ax = error code
	call	RtError_INI		

;	Note: Above call does not return


;***
;exStChain - CHAIN <filename> executor
;Purpose:
;	Chain to the given program
;Input:
;	psdFileName is on the stack
;Output:
;	none.
;Modifies:
;***************************************************************************
MakeExe exStChain,opStChain
	mov	[fChaining],TRUE
	call	ChainCommon		;replace all user-defined oTyps in
					;  blank COMMON type table with the
					;  size of the record (since we trash
					;  all existing module type tables
					;  prior to loading the new program)
	SkipExHeader			;fall into exStRunFile
;***
;exStRunFile - RUN <filename> executor
;Purpose:
;	Reset the existing context, Load the given file, and run it.
;
;	NOTE: There are two ways into this code -
;		(1) Normal pcode dispatch of 'RUN <filename>'
;		(2) From init.asm in the case that the user specified a
;		    filename on the command line.
;	      In case (2), we must handle an error from LoadFile as a
;	      fatal error.
;Input:
;	psdFileName is on the stack (used by LoadFile).
;Output:
;	none.
;Modifies:
;***************************************************************************
MakeExe exStRunFile,opStRunFile
	call	EnStaticStructs 	;activate static prsCur, mrsCur, txdCur
					; (required for calling LoadFile)
	mov	[grs.GRS_otxCur],si	;LoadFile will reset this to 0002
					; if an error occurs after the existing
					; text table is discarded.
	mov	[otxRunSave],si 	;remember otxCur for error recov
	or	[conFlags],F_CON_RunFile
					;don't make debug screen active

	call	GrabSpace		; make sure we don't overcommit
	jz	GrabFailed		; brif GrabSpace didn't work

	mov	ax,LF_NewDoc		; assume document file
	call	fEditorActive		; is the editor active?
	jnz	RunFileLoad		; brif so, load a document
	mov	ax,LF_NewProg		; else it is a program
RunFileLoad:				
	push	ax			; tell LoadFile what type of file
	call	LoadFile		; and all loaded programs should be
					; discarded before the load
	and	[conFlags],NOT F_CON_RunFile	;reset to default
	mov	si,[grs.GRS_otxCur]
	push	ax			;save retval across call
	call	DisStaticStructs	;ensure static structs deactivated again
	call	ReleaseSpace		; release space grabbed above
	pop	ax
	xor	cx,cx			
	xchg	[fChaining],cl		; ensure flag is set to FALSE here,
					;   and remember (in cx) whether
					;   or not we're chaining
	or	ax,ax			;0 if no error in load/save
	jz	RunFile_No_Error

	cmp	[fInitialized],FALSE
	jnz	J5_RtErrorCODE		;brif not a command-line run [17]
	push	ax			;save error code
	call	ShowOutScr		;erase debug screen before reporting err
	pop	ax			;restore ax = error code
	call	RtError_INI		;fatal error

RunFile_No_Error:
	cmp	[fInitialized],FALSE	;did we get here direct from init.asm?
	jnz	Not_Initializing	;  brif not

	and	[BosFlags],not FBOSRESETSTK
	mov	[fInitialized],TRUE	 ;ensure this flag gets set in case
					 ;we're command-line loading [& running]
					 ;a program
	test	[cmdSwitches],CMD_SW_RUN ;Want to Run program, or just load it?
	jnz	Not_Initializing	 ;  brif we do want to run it
	jmp	EndProg1		 ;just wanted to load program
Not_Initializing:
	mov	ax,codeOFFSET exStRunMain
	jcxz	Run_Or_Chain		;brif this is for RUN, not CHAIN
	mov	ax,codeOFFSET exCont	;so RunInit won't take place
Run_Or_Chain:
	;Don't bother to realloc the direct mode buffer - - - assume there's at
	;  least 2-bytes there, for an opEot which we can simply replace.
	DbAssertRel grs.GRS_bdlDirect_cbLogical,ae,2,CODE,<exStRunFile error>
	GETSEG	es,grs.GRS_bdlDirect_seg ; ax = seg adr of far heap entry
	mov	es:[0],ax
	or	[debugFlags],DEBUG_EXEC_CMD
					;Tell UserInterface to execute what it
					;  finds in the direct mode buffer
	and	[grs.GRS_flagsDir],NOT FDIR_cleared
					;note that world is no longer clear
	mov	[grs.GRS_fDirect],TRUE	;must set this back in case of
					;  something like NEW : PRINT
	jmp	EndProg1		;ends up triggering UserInterface

;***
;exCont - (pseudo) executor for CONTinue
;Purpose:
;	CONTinue program execution.  NOTE: This is not really a
;	statement, and there is no opcode for it.  The only time
;	this can ever execute is after the CHAIN stmt executor
;	stuffs exCont into the direct-mode buffer.
;Entry:
;	grs.oRsCONT, grs.otxCONT indicates where next statement
;	to be executed is.
;Exit:
;	none
;Uses:
;	none
;Exceptions:
;	Can cause "Cant continue" error
;
;***************************************************************************
MakeExe exCont,opStRunMain
;NOTE: opStRunMain for exCont is no mistake; there is no CONT opcode, but
;NOTE: we need this MakeExe because the executor is inserted into the Direct
;NOTE: mode buffer, and there is code which needs the MakeExe to properly
;NOTE: (and safely) determine if exCont can cause execution or not (so the
;NOTE: output screen is made active as appropriate, I believe ... see tomc)
	mov	si,[grs.GRS_otxCONT]
	jmp	Cont_Otx

;***
;exStClear - CLEAR statement executor
;Purpose:
;	Perform the CLEAR statement.
;Input:
;	all of the arguments are already on the stack, except for the
;		(2-byte) count of arguments.
;	Note that both flags and values are I4's. The count of arguments
;	is a count of I4's on the stack.
;
;Input Examples (Stack contents):
;
;	CLEAR ,2000				CLEAR 1000,,3000
;
;              +----+                                 +----+
;              |0000|                                 |FFFF|
;              +----+  Flag  (FALSE)                  +----+  Flag  (TRUE)
;              |0000|                                 |FFFF|
;              +----+                                 +----+
;              |FFFF|                                 |0000|
;              +----+  Flag  (TRUE)                   +----+  Value (1000)
;              |FFFF|                                 |03E8|
;              +----+                                 +----+
;              |0000|                                 |0000|
;              +----+  Value (2000)                   +----+  Flag  (FALSE)
;              |07D0|                                 |0000|
;              +----+                                 +----+
;                                                     |FFFF|
;                                                     +----+  Flag  (TRUE)
;                                                     |FFFF|
;                                                     +----+
;                                                     |0000|
;                                                     +----+  Value (3000)
;                                                     |0BB8|
;                                                     +----+
;
;	Note that this executor blasts and possibly alters the location
;	of the stack.
;Output:
;	none.
;Modifies:
;	sp, bp, b$mainframe, b$curframe, b$pend, b$pendchk
;***************************************************************************
MakeExe exStClear,opStClear
	;must check for Illegal Function Call due to current frame not being
	;set for main level code - - - this duplicates runtime functionality,
	;but we do it here so we don't end up in a partially cleared state
	;when the error occurs (and this way, user can still continue).
	mov	bx,bp
	cmp	[b$inonerr],FALSE	;in an error handler?
	jz	Got_Frame_Ptr		;  brif not

	mov	bx,[bx]			;if so, account for the fact that we've
					;  pushed a frame for the handler
Got_Frame_Ptr:
	cmp	bx,[b$mainframe]
	jz	Clear_Cont

	mov	al,ER_FC		;"Illegal Function Call"
J3_RtErrorCODE:
	jmp	J1_RtErrorCODE

Clear_Cont:
	call	WatchRelease		
	call	GetEsDi			
	call	PStepReset		;If stackSize changes, and user
					; is pStepping (F10) through a
					; CLEAR ,,0 stmt, make sure we stop.
					; Assumes CLEAR stmt never occurs within
					; a procedure (very safe assumption)
	xor	cx,cx			;assume no stack parameter
	xor	bl,bl			;count of parm flags seen so far
	LODSWTX 			;fetch count of args on stack
	or	ax,ax
	jz	StClear1		;brif no parms

	shl	ax,1
	shl	ax,1			;convert cI4Parms to cbParms on stack
	mov	di,sp
	add	di,ax			;di points to word above 1st parm

	;Stack contains I4 flags and I4 values - - - when I4 flag is FALSE,
	;I4 value is not present. The below code moves to the first flag
	;and loops for each flag, fetching or skipping value as appropriate
	;when flag is non-zero.
	;At this point, ONLY the cbStack value is kept, and CX is set to
	;non-zero if it is found.
Clear_Parm_Loop:
	dec	di
	dec	di
	mov	dx,[di]			;get high word of I4 flag
	dec	di
	dec	di
	mov	ax,[di]			;get low word of I4 flag
	inc	bl			;inc count of parm flags seen
	cmp	bl,4			;More than 3 parms?
	jb	Clear_Parm_Cont		;  brif not

	mov	al,ER_ADF		;Advanced feature unavailable error
	jmp	J3_RtErrorCODE
Clear_Parm_Cont:
	or	ax,dx			;test to see if I4 flag is zero or not
					;  if it is, ax will end up non-zero
	jz	No_Value		;brif flag is zero; no value to fetch

	cmp	bl,3			;is cbStack value available?
	jz	Fetch_CbStack		;brif so

	sub	di,4
	jmp	short No_Value		;ignore this value
Fetch_CbStack:
	dec	di
	dec	di
	mov	dx,[di]			;get high word of I4 value
	dec	di
	dec	di
	mov	ax,[di]			;get low word of I4 value
	push	dx
	push	ax
	call	I4toU2			;replace I4 on stack with U2
	pop	ax
	mov	cx,sp			;set fStack non-zero
No_Value:
	cmp	di,sp
	ja	Clear_Parm_Loop		;brif more parms to fetch

	DbAssertRel di,z,sp,CODE,<exStClear: sp and di not the same>
StClear1:
	push	ax			;cbStack or garbage
	push	cx			;fStack
	call	ClearStmt
	call	GrabSpace		;don't let the user grab ALL of avail.
					;  DGROUP for his stack
	mov	[grs.GRS_otxCur],si	;update in case of runtime error
	call	B$SCLR
	mov	[b$mainframe],sp	;reset the main level frame ptr - -
					; (other reset tasks are done by
					;  exBos/exBol)
	mov	bp,sp			;sp set to start of stack by B$SCLR
					;  (whether the stack parm given or not)
	call	ReleaseSpace		;return temporarily grabbed space to
					;  system
	jmp	DispMov			;jump to common dispatch point

sEnd	CODE
end
