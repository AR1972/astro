page	49,132
	TITLE	EXDEBUG - Debugging executors

;***
;exdebug.asm - Debugging executors
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include		version.inc
	EXDEBUG_ASM = ON
	IncludeOnce	architec
	IncludeOnce	context
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	extort
	IncludeOnce	opmin
	IncludeOnce	opstmt
	IncludeOnce	opcontrl
	IncludeOnce	ui
	IncludeOnce	rtps
	.list

assumes es, NOTHING
assumes ss, DATA

	EXTRN	B$SASS:FAR

sBegin	CODE
assumes cs, CODE



	extrn	ExBos:near

;==============================================================================
;			Watch Window executors
;==============================================================================

;***********************************************************************
; exWatchExpI2, exWatchExpI4, exWatchExpR4, exWatchExpR8, exWatchExpSD
; Purpose:
;	evaluate a Watch expression and display its value in watch window
; Entry:
;	[top of stack] = number or ptr to string descriptor
; Exit:
;	exWatchExpSD releases string if it was a temporary
;
;***********************************************************************
MakeExe	exWatchStop,opWatchStop
	mov	bx,[pWatchVal]
	mov	BYTE ptr DGROUP:[bx],12h; indicate value is a WatchPoint
	pop	ax
	pop	dx
	or	ax,dx
	je	DontBreak		;brif not TRUE
	cmp	word ptr DGROUP:[bx+1],0 ; If expression was already true
	jne	DontBreak		;    then don't break.
	or	[debugFlags],DEBUG_WATCHPOINT
					;tell next opBos/Bol to stop program,
					; we've hit a Watch Point
DontBreak:
	mov	DGROUP:[bx+1],ax	; store value
	jmp	SHORT WatchExit


MakeExe	exWatchExpI2,opWatchExp
	mov	al,VT_I2		; Runtime code for ET_I2
DebugDisplay:
	push	ss			
	pop	es			;es = DGROUP for movsw
	mov	di,[pWatchVal]		;Note: di restored by GetEsDi
	stosb				;save type of value
	and	ax,0Fh			;ax = length of value in bytes
	shr	ax,1			;ax = length of value in words
	xchg	cx,ax			;cx = length of value in words
WatchLoop:
	pop	ax
	stosw
	loop	WatchLoop
WatchExit:
	call	GetEsDi			;reload es,di for current context
	jmp	ExBos			; return to User Interface

MakeExe	exWatchExpI4,opWatchExp
	mov	al,VT_I4		; Runtime code for ET_I4
	jmp	SHORT DebugDisplay

	;Rewritten with [7]


MakeExe	exWatchExpR4,opWatchExp
	mov	di,[pWatchVal]		;Note: di restored by GetEsDi
	fstp	dword ptr DGROUP:[di+1]
	mov	byte ptr DGROUP:[di],VT_R4  ; Runtime code for ET_R4
	fwait
	jmp	SHORT WatchExit


MakeExe	exWatchExpR8,opWatchExp
	mov	di,[pWatchVal]		;Note: di restored by GetEsDi
	fstp	qword ptr DGROUP:[di+1]
	mov	byte ptr DGROUP:[di],VT_R8  ; Runtime code for ET_R8
	fwait
	jmp	SHORT WatchExit

	;End of [7]


MakeExe	exWatchExpSD,opWatchExp
	mov	bx,[pWatchVal]
	mov	BYTE ptr DGROUP:[bx],24h; indicate value is a string
	inc	bx			;bx points to value
	push	bx			;pass to B$SASS
	CALLRT	B$SASS			;assign string(psdSrc, psdDst)
	jmp	SHORT WatchExit



;***********************************************************************
; ExStTron, ExStTroff
; Purpose:
;	Executors for TRON/TROFF statements.  Cause animation to be
;	turned on/off.  Does not stop program execution.
;
;***********************************************************************


MakeExe	exStTron,opStTron
	mov	al,1
	jmp	SHORT SetTraceMode

MakeExe	exStTroff,opStTroff
	sub	ax,ax
SetTraceMode:
	mov	[fTraceOn],al
	call	SetTronTroff
	or	[BosFlags],FBOSDEBUG
	DispMac


sEnd	CODE

	end
