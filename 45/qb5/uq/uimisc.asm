	TITLE	uimisc.asm - miscellaneous user interface functions.
;*** 
;uimisc.asm
;
;	Copyright <C> 1985-1988, Microsoft Corporation
;
;Purpose:
;	Miscellanious Assember user interface functions
;
;
;*******************************************************************************

	.xlist
	include		version.inc
	UIMISC_ASM = ON	
	CONTEXT_NOFUNCS = ON

	include 	cw\version.inc
	include 	cw\windows.inc
	include 	cw\edityp.inc

	includeOnce	architec
	IncludeOnce	context
	includeOnce	qbimsgs
	includeOnce	ui
	includeOnce	uiint

	.list

assumes	DS,DATA
assumes	ES,DATA
assumes	SS,DATA

;-------------------------------------------------------------------------
;		DATA Segment Declarations
;-------------------------------------------------------------------------
sBegin	DATA

	globalW	fAbortKeyPressed,0

	externW fUiActive		; [17]
	externW iHelpId 		
	externW fOptionsChanged 	
	externW sdNMALLOC		

	globalB	fNoHighlight,0		; TRUE if no hilite should be done
	externW	szDialogTitle		
	externW	cbDialogTitle		
	externW	iMsgStatusLine		
	externB HelpFlags		
	externB fMessageBox		

sEnd	DATA

externFP B$ULGetCommon			
externFP _strcpy


sBegin UI
assumes CS,UI




;Added with [9]
;***
;MsgBoxStdRt - Special version of MsgBoxStd
;
;Purpose:
;	This routine is identical to MsgBoxStd, except that if
;	the error is a runtime error (0 < x < ER_UE), then
;	the helpId is treated specially so that help on this
;	error is different.  This allows us to have seperate
;	contexts for Interpreter generated runtime errors
;	(such as BadFileMode) and execution time runtime errors
;
;Entry:
;	Identical to MsgBoxStd
;
;Exit:
;	Identical to MSgBoxStd
;
;Uses:
;	Per C Convention
;
;****
cProc	MsgBoxStdRt,<PUBLIC,NEAR>
parmW	mbType
parmW	iMsg
cBegin
	mov	ax,iMsg
	mov	bx,ax
	jmp	short MsgBoxCommon	; jump into MsgBoxStd
cEnd	<nogen>



;Added with [9]
;***
;MsgBoxStd - Standard 1 line message box from Msg Number
;
;Purpose:
;	Displays a generic one line dialog box and waits for the user's
;	replay.  The text comes from QBIMSGS.TXT.
;
;	If the error code is less than or equal to ER_UE (i.e. it is
;	a runtime error), we will map the help id with HELP_INTERPBASE
;	so that we will assume the error was generated in the user
;	interface (for when we get help).  Use MsgBoxStdRt to allow
;	runtime help on a runtime error.
;
;Entry:
;	mbType = MB_OK, MB_YESNOCANCEL, or MB_RETRYCANCEL
;	iMsg   = standard message id from qbimsgs.h/inc
;
;Exit:
;	AX = IDOK, IDYES, IDNO, IDCANCEL, or IDRETRY  (id of button pushed)
;
;Uses:
;	Per C Convention
;****
cProc	MsgBoxStd,<PUBLIC,NEAR> 	;NOTE: branched into from MsgBoxStdRt
parmW	mbType
parmW	iMsg
cBegin
	mov	ax,iMsg 		; get the message number
	mov	bx,ax			; get a copy of the number
	cmp	bx,ER_UE		; is it a runtime error?
	ja	MsgBoxCommon		; no, use it as-is
	add	bx,HELP_INTERPBASE	; map it to a special help id
MsgBoxCommon:				; -Entry point for MsgBoxStdRt-
	mov	iHelpId,bx		; and save it
	push	ax			; parameter to ListStdMsg

	Call	ListStdMsg		; Convert Msg # to string

	push	si			; save si
	sub	sp,CB_bufStdMsg 	; Allocate space for bufStdMsg
	mov	si,sp			
	mov	ax,OFFSET DGROUP:bufStdMsg 
	cCall	_strcpy,<ax,si> 	; and copy it
	add	sp,4			
	xor	bx,bx
	cCall	UIMessageBox,<si,BX,BX,mbType> ; display the message box
	add	sp,CB_bufStdMsg 	; release space
	pop	si			; restore si


cEnd

;Added with [13]
;***
;UIMessageBox - Interface to COW routine MessageBox
;
;Purpose:
;	Saves code by doing proceessing required before each MessageBox
;	routine.
;
;	NOBODY should be calling MessageBox() directly, except from within
;	this routine!
;
;	Closes the current help file, if any, before user input.  This
;	prevents them from doing something stupid, like switching floppy
;	disks, while we have the file open.
;
;Entry:
;	msg1	= first line of message
;	msg2	= second line of message (0 if none)
;	msg3	= third line of message (0 if none)
;	mbType	= MB_OK, MB_YESNOCANCEL, or MB_RETRYCANCEL
;
;Exit:
;	AX = IDOK, IDYES, IDNO, IDCANCEL, or IDRETRY  (id of button pushed)
;
;Uses:
;	Per C Convention
;****
cProc	UIMessageBox,<NEAR,PUBLIC>
parmW	msg1
parmW	msg2
parmW	msg3
parmW	mbType
	localW	wRet
cBegin
	push	[grs.GRS_oRsCur]	; [14] save the current grs.oRsCur

	mov	ax,[fUiActive]
	push	ax			; save value of fUiActive
	push	[b$fInt24Err]		; save old int 24 flag (0, -1)
					; (will be set to -1 by
					; EnterUserInterface)

; We should not call DoDrawDebugScr unless necessary.  If we do call it
; for example, when we pop up a message box in a dialog, EnableMenuBar
; will assert that pfnFilter is not DummyFilter, but the dialog filter
; procedure is active instead.  In general, it is just safer to skip
; the unnecessary call.
	or	ax,ax			; See if we are active
	jnz	@F			; If we are, Skip
	call	EnsShowDebugScr 	; make debug screen visible
	call	EnterUserInterface	; enable userInterface interrupt handlers
	call	DoDrawDebugScr		; actually draw debug screen, can't
					;  wait for next call in GetCmd() in the
					;  case where user just executed a TRON
					;  stmt, we don't call GetCmd at all
@@:

	cCall	CloseCurHelpFile	; close any open help files before
					; user input
	push	[iMsgStatusLine]	; push current status line as parm
					;	for StatusLineMsg
	mov	ax,MSG_StatusDialog	; display dialog box status line
	cCall	StatusLineMsg,<ax>	; draw the new status line
	mov	ax,mbType		; AX = type of msgbox
	test	HelpFlags,HLP_INHELP	; are we in the help system
	jz	NotInHelp		; no, don't make any changes
	or	ax,MB_NOHELP		; yes, don't have a <HELP> button
NotInHelp:
	inc	fMessageBox		; flag that we are in a message box
	cCall	MessageBox,<msg1, msg2, msg3, AX>	; display the msgbox
	mov	[wRet],ax		; [14] save return value
	dec	fMessageBox		; clear message box flag
	call	StatusLineMsg		; restore original status line
					;	parm pushed above

	pop	b$fInt24Err		; restore old int 24 flag (0, -1)
	pop	ax			; ax = old value of fUiActive

	or	ax,ax			
	jne	@F			; brif we were already in user interface
	call	ExitUserInterface	; restore runtime's interrupt handlers
	call	EnsShowOutputScr	; restore output screen
@@:
	cCall	UiRsActivate		; [14] Restore oRs (already on stack)
	mov	ax,[wRet]		; [14] restore return value
cEnd


;***********************************************************************
; HookInt24
; Purpose:
;	Save code by setting b$fInt24Err to UNDEFINED, causing the runtime
;	int 24 handler to ignore int 24 errors.  It also clears out
;	any int 24's that we may have previously ignored.
; Preserves:
;	ALL
;
;***********************************************************************
cProc	HookInt24,<PUBLIC,NEAR>
cBegin
	mov	[b$fInt24Err],UNDEFINED
cEnd

;***********************************************************************
; UnHookInt24
; Purpose:
;	Save code by setting b$fInt24Err to 0, causing int 24's to be
;	treated as runtime errors.
; Preserves:
;	ALL
;
;***********************************************************************
cProc	UnHookInt24,<PUBLIC,NEAR>
cBegin
	mov	[b$fInt24Err],0
cEnd

;***********************************************************************
; fInt24Err
; Purpose:
;	See if an Interrupt 24 error has occurred since the last
;	call to HookInt24() or fInt24Err().
;
;	If no error, returns 0
;
;	If int 24 error, returns appropriate error code
;
; Preserves:
;	ES
;
;***********************************************************************
cProc	fInt24Err,<PUBLIC,NEAR>
cBegin
	mov	ax,[b$fInt24Err]
	inc	ax
	je	fInt24Exit		;brif no Int 24 error has occurred
					; (return 0)
	and	ax,000Fh		;al = errcode + 1
	mov	dx,ER_OP		;9 ==> Out of Paper
	cmp	al,9+1			;remember, al=INT24 errcode + 1
	je	GotErrCode

	mov	dx,ER_PRM		;0 ==> Permission Denied
	dec	ax
	je	GotErrCode

	mov	dx,ER_DNA		;1 ==> Device Unavailable
	dec	ax
	je	GotErrCode

	mov	dx,ER_DNR		;2 ==> Disk not Ready
	dec	ax
	je	GotErrCode

	mov	dx,ER_IOE		;all others ==> Device I/O Error
GotErrCode:

	mov	ah,0dh			; disk reset, to trash any bogus
	int	21h			; DOS buffers.  Runtime normally
					; does this, but we are ignoring
					; errors, so it didn't (it can't).
	cCall	HookInt24		; reset int 24 flag
	xchg	ax,dx			; return ax = error code

fInt24Exit:
cEnd


RetryTable:			; table of errors that are retryable
	DW	ER_OP			; Out of Paper
	DW	ER_PRM			; Permission Denied
	DW	ER_DNA			; Device Unavailable
	DW	ER_DNR			; Disk not ready
	DW	ER_IOE			; Device I/O error
	DW	ER_DF			; Disk full

NUM_RETRYABLE	EQU ($ - RetryTable)/2	; size of table

;***********************************************************************
; RetryError
;
; Purpose:
;	Determine if an error is retry-able.
;	if it is, then prompt the user to retry or cancel.
;
; 	Added with revision [4].
;
; Entry:
;	errCode = error we got
;
; Exit:  ax =	0 ==> no error, or not retryable
;		IDRETRY ==> retry
;		IDCANCEL ==> don't retry
; Modifies:
;	Per convention
;
;***********************************************************************
cProc	RetryError,<PUBLIC,NEAR>,<DI>	
parmW	errCode
cBegin
	mov	ax,errCode		; ax = error we got
	push	cs			; ES = CS
	pop	es
	mov	di,UIOFFSET RetryTable	; table of retryable errors
	mov	cx,NUM_RETRYABLE	; # to look at
	repne	scasw			; search for error AX in table
	jne	RetryExit		; brif not found -- exit with ax = 0

GiveRetryBox:
	PUSHI	bx,MB_RETRYCANCEL	; pass dialog-box type
	push	ax			; pass std error code
	call	MsgBoxStd		; return AX = IDRETRY or IDCANCEL

	Skip2_PSW			; skip the XOR AX,AX
RetryExit:
	xor	ax,ax			; can't retry
cEnd

;***********************************************************************
; EnsureFullMenu
; Purpose:
;	Added with revision [2].
;	Called by file loader in txtload.asm when loading a .mak file.
;	Ensure that full menus are active because multiple module
;	programs are not allowed with EZ-Menus.  Ask the user whether
;	he would like to activate full menus or abort the load.
;
; Entry:
;	None
; Exit:
;	AX = 0 if load should be aborted, otherwise non-zero
;
;***********************************************************************

;***********************************************************************
; FindNMalloc
; Purpose:
;	Added with revision [6].
;	Called by UI when preparing for MakeExe.  If a QLB is loaded,
;	MakeExe has to make a decision based on the presence and size
;	of an NMALLOC block in the QLB.
;
; Entry:
;	None
; Exit:
;	AX = size of QLB's NMALLOC block (0 if none)
;
;***********************************************************************
cProc	FindNMalloc,<PUBLIC,FAR>
cBegin
	PUSHI	ax,<dataOFFSET sdNMALLOC>
	call	B$ULGetCommon	;ax = 0,bx <> 0 if found,dx = size if bx <> 0
	or	bx,bx		;NMALLOC found in QLB?
	jz	FindNMallocExit ;no, exit with ax=0
	xchg	ax,dx		;yes, exit with ax = size of NMALLOC block
FindNMallocExit:
cEnd

;	Ported from C with revision [7].
;***********************************************************************
;TMC NEAR PASCAL TmcDoDlgFar (lpDlg, cbDlg, hcab)
;
;Purpose:
;
;	Copy far Dlg data into near data to call TmcDoDlg.
;	Used to minimize DGROUP usage.
;
;Entry:
;	lpDlg		far pointer to the dialog structure.
;	cbDlg		byte count fo the structure.
;	hcab		handle of the cab structure.
;
;Exit:
;	returns TmcDoDlg's return value.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;
;***********************************************************************
cProc	TmcDoDlgFar,<PUBLIC,NEAR>,<SI,DI>
parmD	lpDlg
parmW	cbDlg
parmW	hcab
	localW	oRsSave 		
cBegin

	mov	ax,[grs.GRS_oRsCur]	; save the current grs.oRsCur
	mov	[oRsSave],ax		; as screen redraw changes it.

bpTitle	EQU	4	; offset of title pointer in dialog structure
				; do a bunch of crap before calling tmcDoDlg
	mov	ax,cbDlg		; CX = dialog image size
	inc	ax			; round up to nearest word
	and	ax,0FFFEh

	DbAssertRel	ax,be,17ch,UI,<TmcDoDlgFar: dialog has grown>	

	; NOTE: if you get this assertion, you must verify that there is still
	; NOTE: enough stack space left in the worst case for the biggest
	; NOTE: dialog, plus a help dialog box.  Currently, the biggest dialog
	; NOTE: is the greeting box, but we have our biggest stack usage when
	; NOTE: doing File/Load, due to COW directory list boxes.

	sub	sp,ax			; allocate space for dialog image
	mov	si,sp			; si = *dialog image
	push	ax			; save dialog image size

					; copy FAR dialog image to stack
	cCall	fmemcpy,<ss, si, seg_lpDlg, off_lpDlg, ax>

	mov	ax,iMsgStatusLine	; AX = current status line message #
	push	ax			; save old status line message

	mov	ax,MSG_StatusDialog	; display dialog box status line
	cCall	StatusLineMsg,<ax>	; draw the new status line

	xor	ax,ax			; AX = 0 ==> no char to restore
	mov	di,szDialogTitle	; DI = *new dialog box title
	mov	cx,di			; CX = *new dialog box title
	jcxz	SaveTitleChar		; brif null pointer -- no new title
	sub	cx,si			; CX = offset of title from dialog
					; 	image start
	mov	[si].bpTitle,cx		; use this new title

	mov	bx,cbDialogTitle	; BX = # chars to use of title
	or	bx,bx			; (NZ ==> menu name needs truncating) 
	jz	SaveTitleChar		; brif no trucation needed

	xchg	al,[di+bx]		; truncate string, AL = old character

SaveTitleChar:
	push	ax			; save old character, 0 if no need
					; to restore

	mov	ax,isaHilite		; isa to Get (and reset later)
	push	ax			; parm 1 for SetIsaColor @@@@
	push	ax			; space for coForeSave (parm #2) @@@@
	mov	bx,sp			; bx = *coForeSave
	push	ax			; space for coBackSave (parm #3) @@@@
	mov	cx,sp			; cx = *coBackSave

	push	ax			; parm 1 for SetIsaColor ****

	cCall	GetIsaColor,<ax,bx,cx>	; save old isaHilite values

	push	ax			; space for coFore (parm #2) ****
	mov	bx,sp			; bx = *coFore
	push	ax			; space for coBack (parm #3) ****
	mov	cx,sp			; cx = *coBack

	mov	ax,isaListBoxHilite	; assume normal highlighting

	cmp	fNoHighlight,0		; highlighting?
	jz	SetHighlight		; brif so -- set it
	mov	ax,isaDialogBox		; no highlighting
SetHighlight:
	cCall	GetIsaColor,<ax,bx,cx>	; get the highlight isa to use

					; SetIsaColor (isaHilite,coFore,coBack)
	cCall	SetIsaColor		; 3 parms pushed at "****" above

				; call COW to put up the dialog
	cCall	tmcDoDlg,<si,hcab>	; actually put up the dialog
	xchg	si,ax			; save return value in SI

				; put things back to normal

				; SetIsaColor (isaHilite,coForeSave,coBackSave)
	cCall	SetIsaColor		; 3 parms pushed at "@@@@" above

	pop	cx			; CL = old menu name character
	jcxz	NoRestore		; brif no need to restore
	
	mov	bx,cbDialogTitle	; BX = # chars of title used
	mov	[di+bx],cl		; restore menu name character
NoRestore:

	pop	ax			; AX = old status line message
	cCall	StatusLineMsg,<ax>	; restore original status line

	pop	ax			; AX = dialog image size
	add	sp,ax			; clean dialog image off stack

	cCall	UiRsActivate,<[oRsSave]>; Restore oRs
	xchg	ax,si			; AX = tmcDoDlg return value
	cmp	uierr,0			; got an error someplace?
	jz	NoUierr			; brif not -- return tmcDoDlg result
	mov	ax,tmcCancel		; got an error -- return tmcCancel
NoUierr:
cEnd

sEnd	UI

	end
