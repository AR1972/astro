;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

?WIN = 0                ;Not windows;
?PLM =	  1		;DO use PL/M calling conventions

include cmacros.inc

; Following are C-fns that use CW to put up/take down swap message
extrn	PutUpSwapMsg:far
extrn	TakeDownSwapMsg:far

ifdef	KANJI

extrn	PeekMessage:far
extrn	HookKeyboardMessage:far
extrn	InsertKeyboardMessage:far

extrn	msg:word

;*
;*	CW : Character Windows
;*
;*	cwindows.inc : public include file for CW routines/structures
; Created Mon Feb 18 00:10:50 1991 */ 

pwndMsg		equ	[word ptr 00000h]
messageMsg	equ	[word ptr 00002h]
wParamMsg	equ	[word ptr 00004h]
lParamMsg	equ	[dword ptr 00006h]
LO_lParamMsg	equ	[word ptr 00006h]
HI_lParamMsg	equ	[word ptr 00008h]
timeMsg		equ	[dword ptr 0000Ah]
LO_timeMsg		equ	[word ptr 0000Ah]
HI_timeMsg		equ	[word ptr 0000Ch]

WM_CHAR		equ	258
WM_LBUTTONDOWN	equ	513
WM_LBUTTONUP	equ	514
WM_LBUTTONDBLCLK	equ	515
WM_RBUTTONDOWN	equ	516
WM_RBUTTONUP	equ	517
WM_RBUTTONDBLCLK	equ	518

endif

; Pointers to our near buffer -- multiplexed -- used in C & for temp stack here
extrn	pbBuffer:near
extrn	pbBufferEnd:near


STACKREQ EQU 512		; Stack size requested so that we can make C-calls from
						; the int 2f handler and not worry about stack overflows

sBegin	data
	OldSP			DW	?
	OldSS			DW	?
sEnd	data

sBegin code
    assumes cs, code
    assumes ds, DGROUP

OldInt2fVector	DD	 ?			; Location to store old int 2f vector

;
;
;
int2fswaphandler	proc
	cmp		ax, 4A00h		; Is this a swap disk call?
	je		DoDialog

ChainThru:
	jmp		[OldInt2fVector]

DoDialog:
	or		cx, cx			; Has any one already interacted with user?
	jnz		ChainThru		; YES, Some one else has handled it!

	sti						; BUGBUG -- can I be restore interrupts now? - hari

	;; Try to switch stacks by checking if we have a big enuf stack
	push	bx
	push	es
	push	ds
	mov		ax, DGROUP
	mov		ds, ax
	assumes	ds, DGROUP
	mov		es, ax		; for safety make ES = DS

	mov		ax, word ptr pbBufferEnd	; AX = ptr to last char of our near buf
	mov		bx, word ptr pbBuffer		; BX = ptr to free loc of near buffer
	add		bx, (STACKREQ-1)			; We might need all of STACKREQ bytes!
	cmp		ax, bx						; Is this stack big enuf?

	jb 		NoMessage					; NO, We can't handle swap msg as C-fn
										; call later might need big stack!

	dec		cx							; Mark that we handled swap message!!

	;;;; Put up message to swap disks -- a C-function call to be made!

	; Save all registers, as C-fn call will screw them up!
	; Note that BX, ES, DS have already been saved!!	
	push	cx
	push	dx
	push	bp
	push	si
	push	di

	mov		OldSP,	sp	; Store old stack ptr (SS:SP) in our temp location
	mov		OldSS,	ss

	push	ds			; Perform stack switch
	pop		ss
    mov		sp, ax		; SS:SP points to our near stack now!

	;;;; Push in parameter to the C-fn call below
	push	dx		; DH has drive letter for disk in drive
					; DL has requested drive letter 0 for A, 1 for B, etc

	call	far ptr PutUpSwapMsg	; C-fn call! PLM calling convention

	; flush all characters from keyboard queue

ifdef	KANJI
	push	bx			; save
	mov	ax,1			; TRUE
	lea	bx,KeyboardHook		;
	cCall	HookKeyboardMessage,<ax,cs,bx>	; Hook keyboard input

WaitKeyInput:
	lea	ax,msg			;
	push	ax			; save
	cCall	PeekMessage,<ax>	; get key
	pop	bx			; restore
	or	ax,ax			;
	jz	WaitKeyInput		; key input, FALSE

	mov	ax,[bx].messageMsg	;
	cmp	ax,WM_CHAR		; charactor input ?
	jz	KeyIn			; yes
	cmp	ax,WM_LBUTTONDOWN	; mouse L button ?
	jz	KeyIn			; yes
	cmp	ax,WM_RBUTTONDOWN	; mouse R button ?
	jnz	WaitKeyInput		; no
KeyIn:
	xor	ax,ax			; FALSE
	lea	bx,KeyboardHook		;
	cCall	HookKeyboardMessage,<ax,cs,bx>	; Unhook keyboard input
	pop	bx			;
else

FlushKbdLoop:
	mov	ah,1			; command code for check status
	int	16h				; Is there a char in the key board buffer?
	jz	FlushDone		; No, Flush complete

	xor	ah,ah			; Remove the character from Keyboard buffer
	int	16h				; Get char
	jmp	FlushKbdLoop

FlushDone:
	; Wait till user hits any key -- He is expected to do this after
	; the floppy for the new drive is in place.
	xor	ah,ah				; set command to read character
	int	16h					; Wait  until a key board char hit
endif

	;;;; Take down swap disk message and restore screen -- a C-function
	call	far	ptr	TakeDownSwapMsg	; C-fn call
	
	mov		ss,	OldSS		; Restore DOS' old stack ptr
	mov		sp,	OldSP

	;;;; Restore all registers now
	pop		di
	pop		si
	pop		bp
	pop		dx
	pop		cx

NoMessage:
	pop		ds
	pop		es
	pop		bx

	mov		ax, 4A00h		; Restore original function value to chain thru
	jmp		[OldInt2fVector];

int2fswaphandler	endp


;
; void InstallSwapHandler(void)
;
cProc  InstallSwapHandler, PUBLIC,  <si,di,ds,es>
cBegin	InstallSwapHandler

	mov		ax, 352fh		; Get current contents of
	int		21h				; int 2f vector in ES:BX

	; Save this vector so that we can restore it when our program terminates
	mov		word ptr OldInt2fVector, bx
	mov		word ptr OldInt2fVector+2, es

	; Set our int 2f handler in place
	mov		dx, offset int2fswaphandler
	push	cs
	pop		ds				; DS:DX = address of our int 2f handler
	assumes	ds, nothing

	mov		ax, 252fh		; Set new int 2f vector 
	int		21h				; that is, the one in DS:DX

cEnd	InstallSwapHandler

;
; void RemoveSwapHandler(void)
;
cProc  RemoveSwapHandler, PUBLIC,  <si,di,ds,es>
cBegin	RemoveSwapHandler
		lds		dx, OldInt2fVector
		assumes	ds, nothing

		mov		ax, 252fh
		int		21h							; Restore old int 2f handler
cEnd	RemoveSwapHandler

ifdef	KANJI
cProc	KeyboardHook,FAR,PUBLIC,<ax>
	parmW	message
	parmW	wParam
	parmD	lParam

cBegin
	mov	ax,WM_CHAR		; convert to space code
	mov	message,ax		;
	mov	ax,' '			;
	mov	wParam,ax		;
	cCall	InsertKeyBoardMessage,<message, wParam, lParam>
cEnd	KeyboardHook
endif
sEnd   code

end
