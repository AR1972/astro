page	49,132
	TITLE	EXGOTO - executors for goto and gosub varients
;***
;exgoto - executors for goto and gosub varients
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include 	version.inc
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	debug
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opcontrl
	IncludeOnce	opstmt
	IncludeOnce	opid
	IncludeOnce	opaftqb4
	IncludeOnce	qbimsgs
	IncludeOnce	rtinterp
	IncludeOnce	rtps
	IncludeOnce	txtmgr
	.list

assumes cs, CODE
assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
	externB	b$ErrInfo
sEnd	DATA

	extrn	B$EVTRET:far		;RT reentry point for event RETURN

sBegin	CODE
subttl	GOTO for program and direct mode

	EXTRN	SetProgramMode:NEAR
page

;------------------------------------------------------------------------------
; GOSUB Frames:
;	There are three different GOSUB frames used by QBI -
;	(1) Standard frame
;	(2) Event GOSUB frame
;	(3) GOSUB frame when GOSUB statement occurs in Direct Mode buffer
;
; 	Standard Frame:		 Event Frame:           Direct Mode Frame:
;  Himem       
;                                +--------------+         
;                                |     oRs      |                           
;                                +--------------+                        
;                                |     oTx      |                        
;                                +--------------+                         
;                                |     BP       |                         
;                                +--------------+                         
;                                |  b$curframe  |
;                                +--------------+                         
;                                |    Module    |
;                                |    Frame     |                        
;                                |    Temps     |                         
;                                +--------------+                       
;                                | bcurlevel_QBI|                       
;                                +--------------+                       
;                                |fNonQBI_Active|                       
;       +--------------+         +--------------+       +--------------+
;       |     oTx      |         |      1       |       |   oTx OR 1   |
;       +--------------+         +--------------+       +--------------+
;       |  pGosubLast  |         |  pGosubLast  |       |  pGosubLast  |
;       +--------------+         +--------------+       +--------------+
;  Lomem
;
;	Note that only the QBI specific part of the Event Frame is shown above,
;	the part that's pushed at the point we know an event has occured and
;	the handler is interpreted code. 
;	Note that valid oTx's will never have the low bit set, so if the
;	word on the frame above pGosubLast has the low bit set, it must be
;	for an Event GOSUB or a GOSUB from Direct Mode.
;------------------------------------------------------------------------------

;***
;exStGoto,exStGotoDirect, exStGosubDirect, exStGosub
;Purpose:
;	Handle direct and program mode versions of GOTO and GOSUB.
;
;	For speed, GOTO and GOSUB have special executors for direct and
;	program mode.
;
;	The GOTO and GOSUB operand is fully bound, that is, it is the oTx
;	of the label definition.
;
;	Direct mode versions must...
;
;Input:
;	es:si = pcode address of operand
;Output:
;	es:si	updated
;**********************************************************************
MakeExe exStGotoDirect,opStGotoDirect
	mov	si,PTRTX[si]		;Load destination offset
GotoDirect:
	call	SetProgramMode		;Preserves si
	jmp	short DoDisp


MakeExe exStCommon,opStCommon
	SkipExHeader
MakeExe exStShared,opStShared		
	SkipExHeader			
MakeExe exStStatic,opStStatic		
	SkipExHeader			
MakeExe	exStExitProc,opStExitProc
	SkipExHeader
MakeExe exBranch,opNoList1
	SkipExHeader
MakeExe exStGoto,opStGoto
	mov	si,PTRTX[si]		;Make the branch
DoDisp:
	DispMac 			; and on with the show

MakeExe exBranchRel,opNoList1		
	add	si,PTRTX[si]		;Do a relative branch
	jmp	SHORT DoDisp		

;***
;exStOnGosub,exStOnGoto
;Purpose:
;	Handle both direct mode and program mode ON GOTO and ON GOSUB.
;
;	ON GOTO and ON GOSUB fall through to the next statement if the
;	I2 index argument is out of range, unless the argument is negative
;	or greater than 255, in which case an Illegal Function Call error
;	is issued.
;
;	ON GOSUB pushes a standard GOSUB entry on the stack.  See exStGosub.
;
;	On entry the pcode contains a word operand which is the count of
;	bytes of bound label operands.	Following this is the next executor.
;
;Input:
;	es:si = pcode address of first operand
;	parmW	I2 index
;Output:
;	es:si updated
;	stack cleaned
;***********************************************************************
MakeExe exStOnGosub,opStOnGosub
	mov	cx,sp			;cx non-zero ON GOSUB
	jmp	short StOnShared	;to code shared by ON GOSUB/ON GOTO

MakeExe exStOnGoto,opStOnGoto
	xor	cx,cx			;cx zero indicates ON GOTO
StOnShared:
	LODSWTX 			;Load operand byte count
	pop	bx			;I2 index into label list
	or	bh,bh			;negative number or > 255d?
	jnz	Ill_Fcn_Call		;  brif so

	dec	bx			;Index is 1-based
	shl	bx,1			;To byte index
	cmp	ax,bx			;Test for branch within range
	jbe	OnGoRange		;Not in range

	add	ax,si			;Address of return executor (if GOSUB)
	mov	si,PTRTX[si+bx]		;setup for Branch
	jcxz	GotoDirect		;Not an ON GOSUB - skip building a frame

	call	SetProgramMode		;initialization work if we're coming
					; from direct mode. cx = previous value
					; of fDirect flag
	xchg	ax,si			;si = return address oTx, ax = new oTx
	jcxz	StGosub_Common1		;  brif not Direct Mode GOSUB

	jmp	short Direct_Gosub
OnGoRange:
	add	si,ax			;Index to end
	jmp	short Disp1

Ill_Fcn_Call:
	mov	al,ER_FC		;Illegal Function Call
	SKIP2_PSW			;  skip the next MOV instruction
Stack_Overflow:				;insufficient stack space left for
	mov	al,ER_OM		; recursion
	mov	[b$ErrInfo],OMErr_STK	;note that this is really Out of Stack
	call	RtErrorCODE

;***
;exStGosubDirect, exStGosub
;Purpose:
;	Handle direct mode and program mode GOSUB.
;
;	GOSUB pushes a GOSUB frame on the stack. See top of module for
;	description of frame contents.
;
;	The static variable pGosubLast identifies the last pushed
;	GOSUB frame for the current scope.  When a procedure is invoked,
;	this static variable is preserved as part of the stack frame, to
;	be used for:
;	1. detection of RETURN without GOSUB error.
;	2. walking the stack for edit and continue.
;
;	At procedure return, pGosubLast is updated to reflect the GOSUB
;	frames active in the return scope.
;
;	The location pGosubLast is also used for detecting the lowest
;	stack address valid across user error handlers.
;
;******************************************************************************
MakeExe exStGosubDirect,opStGosubDirect
	LODSWTX 			;Get branch offset
	call	SetProgramMode		;initialize as necessary (can reset stk)
Direct_Gosub:
	or	si,1			;signal that this is a Direct Mode gosub
	or	[grs.GRS_flags],FG_RetDir
					;remember there's a ret adr to
					; direct mode buffer on stack.
	jmp	short StGosub_Common1

MakeExe exStGosub,opStGosub
	LODSWTX 			;Get destination offset
StGosub_Common1:
	push	si			;oTx - return address
	mov	si,ax			;Branch
StGosub_Common:
	push	pGosubLast		;Frame - last previous GOSUB frame
	mov	pGosubLast,sp		;Update frame head pointer
	cmp	sp,[b$pendchk]		;check for stack overflow
	jbe	Stack_Overflow
	DispMac 			; and on with the show.

;***
;exStReturn
;Purpose:
;	Handle RETURN statement without line number.
;
;Input:
;Output:
;***************************************************************************
MakeExe exStReturn0,opStReturn0
	cmp	[pGosubLast],sp 	;Make sure that's a gosub on the stack
	jnz	RetWithoutGosub 	;NULL,so Return without gosub error
	pop	[pGosubLast]		;Frame - Update last active gosub frame
	pop	si			;Frame - Text branch to return
	test	si,1
	jnz	EventOrDirectModeReturn	;Handle ON <event> return or 
					;  return to direct mode buffer
Return_Exit:				
	cmp	[grs.GRS_fDirect],FALSE ;RETURN from Direct Mode?
	jnz	DirectMode_Return	;  brif so
Disp1:
	DispMac 			; and on with the show.

RetWithoutGosub:
	mov	al,ER_RG		;RETURN without GOSUB error
	SKIP2_PSW
CantCont_Err:
	mov	al,ER_CN		;"Cant Continue" error
	call	RtErrorCODE


	EXTRN	Cont_Otx:NEAR		;part of exStCont code

DirectMode_Return:
	cmp	[grs.GRS_otxCONT],UNDEFINED
					;exists context that can be CONTinued?
	jz	CantCont_Err		;  brif not - - issue 'Cant Continue'

	jmp	Cont_Otx		;share code with exStCont


MakeExe exStReturn1,opStReturn1
	;NOTE: if event frame is on stack, can never return to
	;NOTE: context of where event occured now? That seems okay.
	cmp	[pGosubLast],sp 	;Make sure that's a gosub on the stack
	jnz	RetWithoutGosub 	;NULL,so Return without gosub error

	pop	[pGosubLast]		;Frame - Update last active gosub frame
	LODSWTX 			;load oTx of line to return to
	pop	si			;Frame - Text branch to return
	xchg	ax,si			;si = oTx to RETURN to
	dec	ax			;Event GOSUB?
	jnz	Return_Exit		; brif not

	cmp	WORD PTR [bp+8],0	; did event take place in QB code?
	jnz	KeepThisFrame		; brif not - keep this module frame

	mov	ax,[grs.GRS_oRsCur]	
	cmp	ax,[bp+4]		; did event take place at mod. level,
					;  and in the same module as this
					;  handler lives in?
	jz	EventReturn_LineNo	; brif so - - - pop this frame, so
					;  module-level frame and any active
					;  gosub frames are useable again ...
KeepThisFrame:				
	pop	ax			;pop off all but the module frame.
	pop	ax
	jmp	short Return_Exit	

EventReturn_LineNo:			
	call	SetProgramMode		; in case of RETURN from direct mode
	pop	[fNonQBI_Active]	; restore to previous setting	
	pop	[bcurlevel_QBI]		; restore to previous setting	
	mov	sp,bp			
	pop	bp			
	pop	ax			; discard oTx part of return address
	jmp	short EventReturn	

EventOrDirectModeReturn:
	cmp	si,1
	ja	DirectReturn		;brif a RETURN to Direct Mode buffer

	call	SetProgramMode		;in case of a RETURN from direct mode
	pop	[fNonQBI_Active]	;restore to previous setting	
	pop	[bcurlevel_QBI]		;restore to previous setting	
	mov	sp,bp
	pop	bp
	pop	si
EventReturn:
	mov	[b$curframe],bp
	mov	[grs.GRS_oTxCur],si	;in case of an event to QBI code from
					;  non-QBI code now
	pop	ax			;ax = oRs to restore
	call	RsActivateCODE
	jmp	B$EVTRET		;transfer control back to runtime -
					;  runtime returns control to the
					;  context where the event was detected

DirectReturn:
	;Returning into direct mode buffer
	and	si,NOT 1		;make this a normal oTx
	mov	[grs.GRS_fDirect],TRUE	;make Direct Mode text buffer active
	and	[grs.GRS_flags],NOT FG_RetDir
					;remember there's no ret adr to
					; direct mode buffer on stack.
	jmp	DispMov 		;set up ES and dispatch

;***
;B$IEvHandler - runtime call-back to start an event gosub
;
;Purpose:
;	When an event has occured, and the runtime determines that the
;	handler exists and is in QBI code, it calls this routine to start
;	the event gosub.
;Input:
;	DS:BX points to the oTx of the event handler
;	DS:BX+2 points to the oMrs of the event handler
;Exit:
;	none - just starts the subroutine going. A RETURN executor (above)
;	will be responsible returning control back to runtime arbitration.
;***************************************************************************
cProc	B$IEvHandler,<FAR,PUBLIC,NODATA>
cBegin
	push	[grs.GRS_oRsCur]	;so RETURN can restore
	push	[grs.GRS_oTxCur]	;so RETURN can restore
	mov	si,[bx] 		;fetch gosub oTx from RT event table
	mov	ax,[bx+2]		;oMrs to activate for event handler
	call	RsActivateCode

	push	bp			;push a new module level frame
	mov	bp,sp
	push	[b$curframe]		
	mov	[b$curframe],bp
	mov	bx,[grs.GRS_oMrsCur]	
	RS_BASE add,bx			; bx points to mrsCur in the Rs table
	GETRS_SEG es			
	xor	ax,ax
	xchg	ax,[fNonQBI_Active]	;must reset this here in case of error
					; i.e., remember QBI code is active now
	mov	dx,sp
	mov	cx,PTRRS[bx.MRS_cbFrameTemp]	
	add	cx,PTRRS[bx.MRS_cbFrameVars]	
	dec	cx			; pushed b$curframe already accounted
	dec	cx			; for in sp (cbFrameVars cnts it too
	sub	dx,cx
	jc	EvHandler_Overflow	;brif proposed sp value wraps around

	cmp	dx,[b$pendchk]		;check for stack overflow - - - MUST
					; check for this now, so SP can never
					; be set to an illegal value
	ja	EvHandler_Cont		;brif no overflow
EvHandler_Overflow:
	jmp	Stack_Overflow		;note that the rest of the key context
					;  (oRsCur, SI) has been set up to
					;  report the error correctly. Stack
					;  overflow causes stack to be blasted
					;  and a CLEAR to occur anyway, so no
					;  problem that all items didn't get
					;  pushed on the stack.
EvHandler_Cont:
	mov	sp,dx			;make room for module level frame stuff
	DbAssertTst   sp,z,1,CODE,<B$IEvHandler: SP contains an odd number>

	;NOTE: No reason to copy existing module frame vars+temps to and back
	;NOTE: from this new frame, nor to zero them. Frame temps are only
	;NOTE: meaningful within the context of a statement. Frame vars are
	;NOTE: only used by FOR loops; not too worried about what happens if
	;NOTE: user jumps into the middle of a FOR loop; we can't match what
	;NOTE: BC does for that case anyway.

	push	[bcurlevel_QBI]		;save in case this is modified
	push	ax			;previous value of fNonQBI_Active - 
					; remember whether QBI code is active
					; or not at context we're to return
					; to later
	PUSHI	ax,1			;1 (instead of oTx) says "event gosub"
	call	GetEsDi 		;ensure ES & DI setup for execution
	jmp	StGosub_Common
cEnd	<nogen>

sEnd	CODE
end
