	page	49,132
	TITLE	EXMISC - Miscellaneous Executors
;***
;exmisc.asm - executors for simple id references.
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   This module contains:
;	- all BOS executor varients.
;	- nonspeed-critical NOP executors.  They are kept in one
;	  place for size reduction.
;	- WATCH statement executors.
;
;   This module also contains the following debugging aids:
;	- Dispatch - nonRELEASE versions jmp to a single dispatch entrypoint
;	  in this module for easier debugging.
;
;
;****************************************************************************

	.xlist

	NOTEXT = 1
	NORASTOPS = 1
	NOMB = 1
	NOWM = 1
	NOMST = 1
	include 	windows.inc

	include 	version.inc
EXMISC_ASM = ON
	IncludeOnce	architec
	IncludeOnce	conint
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	heap
	IncludeOnce	opmin
	IncludeOnce	opintrsc
	IncludeOnce	opstmt
	IncludeOnce	opcontrl
	IncludeOnce	opid
	IncludeOnce	opaftqb4
	IncludeOnce	pcode
	IncludeOnce	ui
	IncludeOnce	variable
	.list

assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
	staticW tempWord,0		;temp storage for a word value
sEnd	DATA

	externFP B$EVCK
	externFP B$Break
	externFP B$StackReset
	externFP B$ClearRange

sBegin	CODE
assumes cs, CODE


;-----------------------------------------------------------------------------
; Dispatch - non-RELEASE versions dispatch each pcode from here
;-----------------------------------------------------------------------------
Public	Dispatch
Dispatch:
	mov	cx,es		; for later verification
	SETSEG_EQ_SS es 	; in case fDirect is TRUE
	lea	bx,[grs.GRS_bdlDirect_seg]
	cmp	[grs.GRS_fDirect],FALSE
	jnz	Dispatch_Cont	;brif active text table is bdlDirect


	GETRS_SEG es,bx,<SIZE,LOAD>  ;[14]
	mov	bx,[grs.GRS_offsetTxdSeg]	
	RS_BASE add,bx		; bx points to appropriate Rs
Dispatch_Cont:
	cmp	si,PTRRS[bx.BDL_cbLogical - BDL_seg]	  ;[14]
	jae	SI_Error

	GETSEG	ax,PTRRS[bx],,<SIZE,LOAD> ;[14][9] ax == segment address for current seg
	cmp	ax,cx		
	jnz	ES_Error	;brif ES incorrect
ES_Okay:			
	xchg	ax,cx		; ensure cx has correct seg address
	mov	bx,[grs.GRS_pMrsCur]
	mov	ax,PTRRS[bx.MRS_bdVar.BD_pb]	
	add	ax,VAR_value
	cmp	di,ax
	jz	DispEx_Cont	; brif di is okay

DI_Error:
	DbHalt	CODE,<DI Error in exmisc.asm at Dispatch>
DispEx_Cont:			
	DbChk	Heaps		;Check the heap if -tglCheckHeaps set
	mov	es,cx		; restore pcode segment
	LODSWTX 		;Pick up an executor
Public	DispEx
DispEx:				;useful public for debugging
	jmp	ax		;And dispatch

ES_Error:
	DbHalt	CODE,<ES Error in exmisc.asm at Dispatch>
SI_Error:
	DbHalt	CODE,<Error in exmisc.asm at Dispatch: si past end of text tbl>


subttl	Begin of Statement Handling
page

;Table of exception handlers
ExDispTbl LABEL WORD
	DW	codeOFFSET DoFBosResetStk
	DW	codeOFFSET DoFBosEvent
	DW	codeOFFSET DoFBosStop
	DW	codeOFFSET DoFBosDebug
;ExDispTbl assumes following mask values:
	.errnz	FBOSRESETSTK - 01h		
	.errnz	FBOSEVENT    - 02h		
	.errnz	FBOSSTOP     - 04h		
	.errnz	FBOSDEBUG    - 08h		

;***
;B$IEvSet - set the FBOSEVENT bit in BosFlags
;
; NOTE: This routine will be called with interrupts disabled.
;
;Input:
;	none.
;Output:
;	The FBOSEVENT bit is set in BosFlags
;Preserves:
;	ALL but flags
;*******************************************************************************
cProc	B$IEvSet,<PUBLIC,FAR,NODATA>
cBegin
	.errnz	HIGH FBOSEVENT		      ;assuming FBOSEVENT in low byte of word
	or	BYTE PTR [BosFlags],FBOSEVENT ; set the bit
cEnd

;***
;B$IEvReset - reset the FBOSEVENT bit in BosFlags
;
; NOTE: This routine will be called with interrupts disabled.
;
;Input:
;	none.
;Output:
;	The FBOSEVENT bit is reset in BosFlags
;Preserves:
;	ALL but flags
;*******************************************************************************
cProc	B$IEvReset,<PUBLIC,FAR,NODATA>
cBegin
	and	[BosFlags],NOT FBOSEVENT
cEnd


;*******************************************************************************
;DoFBosStop - handle BOS flag FBOSSTOP
;Purpose:
;	This routine is invoked the runtime calls us back to inform us that
;	the user hit Ctl-Break.
;
;	This routine does not return - - execution is halted, control is
;	returned to Direct Mode.
;
;Input:
;	ax contains the FBOSSTOP bit
;*******************************************************************************
DoFBosStop:
	pop	ax			;Throw away the return address
FBosStop_Event:				;(entry from DoFBosEvent if untrapped
					;  ctl-break detected)
	and	[BosFlags],NOT FBOSSTOP	;turn off bit
	jmp	B$Break 		;ends up jumping to exStStop

;***
;B$IBreak - call-back from runtime to cause us to STOP at next BOS/BOL
;
;Input:
;	none.
;Exit:
;	none.
;Preserves:
;	ALL but PSW
;
;***************************************************************************
cProc	B$IBreak,<PUBLIC,FAR,NODATA>
cBegin
	.errnz	HIGH FBOSSTOP		     ;assuming FBOSSTOP in low byte of word
	or	BYTE PTR [BosFlags],FBOSSTOP ; set the bit
cEnd


;*******************************************************************************
;DoFBosEvent - handle BOS flag FBOSEVENT
;Purpose:
;	This routine is invoked when an event has occured during the previous
;	statement (and the runtime set FBOSEVENT via B$IEVSet).
;
;	We push our current context on the stack, and call the runtime; the
;	runtime will in turn call back to QBI code if there's an event handler
;	in QBI text, or start a compiled-basic event handler, or just return
;	if there's no handler for the posted event. 
;	If there is a handler invoked, as long as it returns, we'll get control
;	back here, to take up execution where it was left off.
;
;	NOTE: the other Bos exception handlers turn off their own bit in 
;	NOTE: BosFlags. This had been done just prior to the call, but it's
;	NOTE: important that our code NEVER change the FBOSEVENT bit in
;	NOTE: BosFlags, or we introduce the possiblity of missing events.
;
;*******************************************************************************
DoFBosEvent:
	pop	dx			;Throw away the return address.
					;  Must not re-execute BOS code; to be
					;  compatable with BC3, must allow
					;  at least one statement to execute
					;  between event traps
	cmp	[grs.GRS_fDirect],FALSE	;Are we executing in Direct Mode?
	jnz	BosEvent_Exit		;  brif so - - - ignore events
					;    (note that ax, bx, cx are still
					;     set up from BOS exception loop)
	test	[grs.GRS_flags],FG_WatchActive
	jnz	BosEvent_Exit		;Ignore events while WATCH expression
					; is being evaluated.

	mov	[grs.GRS_oTxCur],si	;must do this for case where QBI event
					;  handler fires up - - - IT preserves
					;  oTxCur and oRsCur in event frame
	mov	[fNonQBI_Active],bp	;in case non-QBI code is envoked
	push	[b$curlevel]
	pop	[bcurlevel_QBI]		;in case we must later restore 
					;  b$curlevel to what it is now
	push	[BosFlags]		;remember if FBOSDEBUG bit was on
	and	[BosFlags],NOT FBOSDEBUG ;don't allow user to trace into an
					 ;  event handler
	call	B$EVCK			;runtime invokes event handler if
					;  there is one.
					;B$EVCK returns:
					;	al = non-zero if an untrapped 
					;		ctl-break occured
	pop	bx			;old BosFlags
	and	bx,FBOSDEBUG
	or	[BosFlags],bx		;if FBOSDEBUG bit had been set prior to
					;  calling B$EVCK, turn it back on. If
					;  it was turned on in a QBI event 
					;  handler, leave it on

	mov	[fNonQBI_Active],0	;reset (QBI code is definitely active
					;  now)
	mov	si,[grs.GRS_oTxCur]	;in case a non-QBI event occured
	or	al,al
	jnz	FBosStop_Event		;brif untrapped ctl-break

	;go back to BOS Flag handling loop, but only if one or more flags
	;of lower priority (than the Event flag) are set. This ensures that 
	;we'll allow at least one statement to be executed between event 
	;handler invocations (which is compatable with the compiler design), 
	;and that we'll allow break-points to function correctly after an event 
	;handler returns
	mov	ax,FBOSEVENT
	.errnz	FBOSEVENT - 2		
	mov	bx,ax			;FBOSEVENT == 2^N, then bx must be 2*N
					;  here we're depending on the fact that
					;  for N = 1, 2^N == 2*N (N is the bit#)
	mov	cx,[BosFlags]
BosEvent_Exit:
	TESTX	cx,<NOT (FBOSEVENT OR (FBOSEVENT - 1))>
					;are any lower priority bits set?
	jnz	BosContLoop		;  brif so - handle other bit(s)

	jmp	DispMov			;  brif not - - - infinite loop would
					;	result if we went back to 
					;	BosContLoop without any other
					;	bits set

DbCheckBos	MACRO
	DispMac
	ENDM


;***
;exBreakPoint
;Purpose:
;	This pcode follows the opBol[Lab][Sp] opcode on any line which
;	has a breakpoint set.  It causes the executor to call UserInterface().
;Exit:
;	DEBUG_STOP flag is set in [debugFlags]
;
;******************************************************************************

;***
;BosException - Handle the beginning of statement flags

;Purpose:

;   Some executor has set a begin of statement flag.  This routine
;   handles the case that BosFlags have been set.  Flag specific
;   handlers may be invoked.
;
;   Some flags have no handlers.

;Entry:

;   si points beyond the bos/bol for current statement

;Exit:
;Modifies:
;*******************************************************************************

;***
;exBos,exBosSp,exBol,exBolSp,exBolLab,exBolLabSp,exBolInclude,exBolIncludeSp
;Pupose:
;	Handle begin of statement.
;	Each of these handlers performs any unique work (skipping operands, etc)
;	and Then checks end of statement flags.
;Inpup:
;Output:
;Modifies:
;******************************************************************************
;NOTE: Even though we could share code in following executors, we don't
;      for the sake of execution speed.
;

MakeBol	MACRO	cSpace
MakeExe	exBol&cSpace,opBol+cSpace*(OPCODE_MASK+1)
	endm

MakeBol	23
	SkipExHeader
MakeBol	22
	SkipExHeader
MakeBol	21
	SkipExHeader
MakeBol	20
	SkipExHeader
MakeBol	19
	SkipExHeader
MakeBol	18
	SkipExHeader
MakeBol	17
	SkipExHeader
MakeBol	24
	SkipExHeader
MakeBol	16
	cmp	BosFlags,0
	jnz	BosException1
	DbCheckBos

MakeBol	15
	SkipExHeader
MakeBol	14
	SkipExHeader
MakeBol	13
	SkipExHeader
MakeBol	12
	SkipExHeader
MakeBol	11
	SkipExHeader
MakeBol	10
	SkipExHeader
MakeBol	9
	SkipExHeader
MakeBol	8
	cmp	BosFlags,0
	jnz	BosException
	DbCheckBos

NotBosDebug:
	sub	bx,bx			;offset into table of exception handlers
	mov	ax, 1
BosExLoop:
	test	cx, ax
	jnz	GotExc
BosContLoop:				;possible reentry point from DoFBosEvent
	DbAssertRel ax,nz,0,CODE,<exmisc: ax = 0 in BosException handling loop>
	inc	bx			;advance to next exception handler
	inc	bx
	shl	ax,1			;check next bit from BosFlags
	jmp	BosExLoop

GotExc:
	call	ExDispTbl[bx]		; call handler if bit was set
	SkipExHeader			;fall into exBos
MakeExe exBos,opBos
	cmp	BosFlags,0
BosException1:				
	jnz	BosException
	DbCheckBos	

MakeExe exBolLab,opBolLab
	add	si,4			;skip operands
	cmp	BosFlags,0
	jnz	BosException
	DbCheckBos


MakeExe exBreakPoint,opBreakPoint
	or	[debugFlags],DEBUG_STOP ;tell UserInterface() we've hit a
	or	[BosFlags],FBOSDEBUG	; breakpoint
	;fall into BosException code
BosExcpt:
BosException:
	mov	cx,BosFlags		;Load flags
	DbAssertRel cx,b,FBOSDEBUG*2,CODE,<invalid BosFlags at BosException>
	cmp	cx,FBOSDEBUG
	DJMP	jne NotBosDebug
;Come here if FBOSDEBUG is only bit set at opBol
; Optimization for fast WatchPoints.
	cmp	[debugFlags],DEBUG_WATCH
NotBosDebugJne:
	DJMP	jne NotBosDebug		;brif need to do more than just Watch
	cmp	[fDebugScr],FALSE
	DJMP	jne NotBosDebug
	mov	[grs.GRS_otxCur],si
	call	DebugWatch
	mov	si,[grs.GRS_otxCur]	;get value as updated by DebugWatch
	RestorePcodeVar 		;es->cur txt tbl, di->cur var tbl
	DispMac 			;and resume execution


MakeExe exBolInclude,opBolInclude
	inc	si			;skip $INCLUDE nesting depth operand
	inc	si
	SkipExHeader			;this one need not be super fast
MakeBol	7
	SkipExHeader
MakeBol	6
	SkipExHeader
MakeBol	5
	SkipExHeader
MakeBol	4
	SkipExHeader
MakeBol	3
	SkipExHeader
MakeBol	2
	SkipExHeader
MakeBol	1
	SkipExHeader
MakeBol	0
	cmp	BosFlags,0
BosExcpt2:				
	jnz	BosExcpt
	DbCheckBos



MakeExe exBolIncludeSp,opBolIncludeSp
	inc	si			;skip $INCLUDE nesting depth operand
	inc	si
	SkipExHeader			;this one need not be super fast
MakeExe exBosSp,opBosSp
	SkipExHeader			;this one need not be super fast
MakeExe exBolSp,opBolSp
	inc	si			;skip operand
	inc	si
	cmp	BosFlags,0
	jnz	BosExcpt2		
	DbCheckBos

MakeExe exBolLabSp,opBolLabSp
	add	si,6			;skip operands
	cmp	BosFlags,0
BosExcpt1:
	jnz	BosExcpt2		
	DbCheckBos


;*******************************************************************************
;DoFBosDebug - handle BOS flag FBOSDEBUG
;Purpose:
;	This routine is invoked when a runtime error occurs in a module for
;	which there is no active error handler. [debugFlags] should already
;	be set up at this point, so we just have to transfer control to
;	UserInterface().
;	NOTE: since FBOSDEBUG is the last bit checked, we don't need to return
;	      to BosException
;
;Input:
;	ax contains the FBOSDEBUG bit
;
;*******************************************************************************
DoFBosDebug:
	xor	[BosFlags], ax		; turn off bit.
	pop	ax			; Throw away the return address.
	jmp	StopGrsContext		;save si in grs, call UserInterface()

;*******************************************************************************
;DoFBosResetStk - handle BOS flag FBOSRESETSTK
;Purpose:
;	This routine causes the stack pointers to be reset to their
;	[re]initialization values.
;
;Preserves:
;	si
;*******************************************************************************
DoFBosResetStk:
PUBLIC DoFBosResetStk			;called from exEndProg if bit is set
	pop	[tempWord]		;altering stack so save retval
	and	[BosFlags],NOT FBOSRESETSTK		
					; turn off bit. Can't expect ax set up
					;  as this is called from exEndProg too
	mov	ax,sp
	push	ax			;points to bottom of range that could
					;  have owners
	mov	bp,[b$mainframe]
	push	bp			;points to top of range that could have
					;  owners
	call	B$ClearRange		;release any owners found in given range
	test	[conFlags],F_CON_ResetStkSize
	jz	StackSize_Okay		;brif don't want to reset stack size

	and	[conFlags],NOT F_CON_ResetStkSize
	call	B$StackReset		;Reset the stack to initial location
	mov	[b$mainframe],sp
	mov	bp,sp
StackSize_Okay:
	mov	word ptr [bp],0		;reinitialize end of bp chain
	mov	word ptr [bp-2],0	
	mov	sp,bp

	mov	[b$curframe],bp

	GETRS_SEG es,bx,<SIZE,LOAD>	;[14] es == Rs table seg, trashes bx
	mov	bx,[grs.GRS_oMrsCur]	
	RS_BASE add,bx			
	mov	cx,PTRRS[bx.MRS_cbFrameTemp] 
	add	cx,PTRRS[bx.MRS_cbFrameVars] 
	sub	sp,cx			;make room for module level frame stuff
	DbAssertTst   sp,z,1,CODE,<DoFBosResetStk: SP contains an odd number>
	push	ss			
	pop	es
	mov	di,sp
	mov	al,0
	rep	stosb			;initialize module frame vars
	call	GetEsDi
	mov	[b$cNonQBIFrames],0	;reset count of non-QBI frames on stack
	jmp	[tempWord]		;stack reset - - return to BOS/BOL code

;***
;exLab,exLabSp,exInclude,ex_Static,ex_Dynamic,ex_Include,exReParse
;Purpose:
;	These executors have no work other than to skip their operands.
;Input:
;Output:
;Modifies:
;*******************************************************************************
MakeExe exLab,opLab
	add	si,4			;skip operands (link, oNam)
	DispMac

MakeExe exStDefType,opStDefType
	SkipExHeader			;skip operands (link,I4mask)
MakeExe exLabSp,opLabSp
	add	si,6			;skip operands (link, oNam, cSpaces)
	DispMac

MakeExe exReParse,opReParse
	DbHalt	CODE,<exReParse executed ...>

MakeExe ex_Static,op_Static
	SkipExHeader
MakeExe ex_Dynamic,op_Dynamic
	SkipExHeader
MakeExe ex_Include,op_Include
	SkipExHeader
MakeExe exStDeclare,opStDeclare
	SkipExHeader
MakeExe exStData,opStData
	SkipExHeader
MakeExe exStRem,opStRem
	SkipExHeader
MakeExe exQuoteRem,opQuoteRem
SkipNAtrs:				;this is pretty speed critical, due to
					;  the REM executors
	LODSWTX 			;fetch cntEos
	inc	ax			;cntEos could be odd - - round up if so
	and	ax,not 1
	add	si,ax			;skip to next pcode and dispatch
	DispMac

;***
;exShared, exStatic, exVtRfxx, exAsType<Fixed|Exp|>
;Purpose:
;	These are executors requiring absolutely no work, and are
;	not speed critical.
;Input:
;Output:
;Modifies:
;*************************************************************************

MakeExe exAsTypeFixed,opAsTypeFixed	
	inc	si			
	inc	si			
	SkipExHeader			
MakeExe exAsType,opAsType
	SkipExHeader
MakeExe exAsTypeExp,opAsTypeExp
	inc	si
	inc	si
	SkipExHeader
MakeExe exVtRfI4,opVtRf,ET_I4
	SkipExHeader
MakeExe exVtRfR4,opVtRf,ET_R4
	SkipExHeader
MakeExe exVtRfR8,opVtRf,ET_R8
	SkipExHeader
MakeExe exVtRfSD,opVtRf,ET_SD
	SkipExHeader
MakeExe exVtRfI2,opVtRf,ET_I2
	SkipExHeader
MakeExe exVtRfImp,opVtRf,ET_Imp
	inc	si			;skip operand
	inc	si
	SkipExHeader
MakeExe	exNoType,opNoType
	SkipExHeader
MakeExe	exStConst,opStConst
	SkipExHeader
MakeExe exShared,opShared
	SkipExHeader			
MakeExe exNop0,opNoList0		
	SkipExHeader			;Fall through to exLParen

;***
;exLParen - left paren executor
;Purpose:
;	exLParen is present for listability.  No execution time work is
;	performed.
;****
MakeExe exLParen,opLParen
	DispMac 			;speed critical - dispatch immediately

subttl	Executors for non-release code only
page

; exInvalid - Called only when an error occured in the scanner
;
;   Several of the executor maps in the scanner have slots needed for
; filler only.	If for some reason, one of these is referenced, the
; executor exInvalid will be written to the execute state code.
;

MakeExe exInvalid,opEot 	    ;[?] opcode is arbitrary
	DbHalt	CODE, <exInvalid has been encountered>

sEnd	CODE

sBegin	UI
assumes	CS,UI
;***
;B$EnsShowOutputScr - ensure the output screen is active
;Purpose:
;	Called from the runtime at the point where an event handler is
;	about to be invoked; this is to ensure that an event handler doesn't
;	end up printing output to our debug screen.
;****
extrn	EnsShowOutSaveRs:near
cProc	B$EnsShowOutputScr,<PUBLIC,FAR,NODATA>,<ES,BX>
cBegin
	cCall	EnsShowOutSaveRs
cEnd
sEnd	UI

	end
