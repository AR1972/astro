;
;	CW : Character Windows Drivers
;
;	keyint3.asm : DOS 3 key interrupt
;
;----------------------------------------------------------------------------

; Code space constants

staticD	lpsslBios,417H			;* BIOS shift state 0040:0017 !!!!

; Code space data!  Horrors!

staticD	pfnOldInt15,0			; old int 15 handler

;----------------------------------------------------------------------------

;********** EnableKeyboardKbd **********
;*	entry:	pinkb => INKB data
;*		fEnable => whether to enable or disable
;*		fExiting => exiting if !fEnable
;*	* enable or disable the keyboard handler
;*	exit : n/a

cProc	EnableKeyboardKbd, <FAR, PUBLIC, ATOMIC>, <DI>
    parmDP pinkb
    parmW  fEnable
    parmW  fExiting
cBegin	EnableKeyboardKbd

;*	* save pointer to INKB data
	mov	di,OFF_lpwDataKbd		;* Data in data segment
	mov	bx,pinkb
	mov	[di].pinkbCur,bx

	mov	ax,fEnable
	mov	[bx].fPollKeyboardInkb,ax	;* to poll or not to poll?
	or	ax,ax
	jnz	enable_keyboard

	jmp	disable_keyboard

enable_keyboard:
;*	* Enable the keyboard
	test	[di].fKeyboardEnabled,0ffh
	jz	enable_keyboard2
	jmp	end_enable_keyboard		;* already enabled

enable_keyboard2:
;*	* Initialize globals
;;	mov	[di].semCopyQueue,-1
	
	lea	ax,[di].rglKbBuffer
	mov	[di].queueKb+pStartQueue,ax
	mov	[di].queueKb+pHeadQueue,ax
	mov	[di].queueKb+pTailQueue,ax
	add	ax,clKbdBuff * 4		;* point to end
	mov	[di].queueKb+pEndQueue,ax
	mov	[di].queueKb+semQueue,-1	; initialize both semaphores

;*	* set globals to reflect initial state AND
;*	*  query to see if TSR is present
	mov	ax,tsrcInitTerm
	mov	[di].fKeyboardEnabled,al		;* enabled
	mov	bx,[di].pinkbCur
	mov	[bx].fKeyIsUpInkb,al			;* set up state

IFDEF WORDTSR
	;* Word protocal
	mov	ax,55FFh
	mov	bx,5
	xor	cx,cx
	int	16H
	xor	ax,4D53h
	jnz	@F
	mov	bx,[di].pinkbCur
	mov	[bx].fNormalKeyboardInkb,ax	;* 0 => proper TSR present
@@:
ELSE ;!WORDTSR
	;* Works (default) protocal
	lea	cx,[di].wTsrRepeat			;* DS:CX => two flags
	Assert	<verTsr EQ 0>
	xor	dx,dx
	mov	bx,drvOffset TsrRequest

	push	cs
	pop	es				;* es:bx => TsrRequest
	int	16H				;* nothing to return

	xor	ax,ackTsr			;* acknowledge ?
	mov	bx,[di].pinkbCur
	mov	[bx].fNormalKeyboardInkb,ax	;* 0 => proper TSR present
ENDIF ;!WORDTSR

;*	* Hook the INT 1B handler
	mov	al, 1Bh
	mov	bx, pfnOldInt1B		;* offset into KBD data
	mov	dx, drvOffset Int1BHandler
	cCall	Hooker

	mov	bx,[di].pinkbCur
	mov	cx,[bx].fNormalKeyboardInkb
	jcxz	end_enable_keyboard

;*	* Hook and initialize the INT 08 handler
	mov	al, 08h
	mov	bx, pfnOldInt08
	mov	dx, drvOffset Int08Handler
	cCall	Hooker

	mov	[di].rstCur, RST_IDLE
	mov	[di].ckeyRepeat, 0

;*	* Hook INT 16
	mov	al, 16h
	mov	bx, pfnOldInt16
	mov	dx, drvOffset Int16Handler
	cCall	Hooker
	mov	[di].fUnhook16, 0

;*	* Hook INT 9 (TSR not present)
	mov	al, 09h
	mov	bx, pfnOldInt09
	mov	dx, drvOffset Int09Handler
	cCall	Hooker

	call	HookInt15

	les	bx,lpsslBios
	mov	al,es:[bx]
	mov	[di].sslMirror,al
	mov	byte ptr [di].ssLastInt,al		;* set low byte only

;*	* check for extended (ronco) keyboard
	mov	[di].pmpscvwPlain,drvOffset mpscvwPlain
	mov	[di].pmpscvwShift,drvOffset mpscvwShift
IFNDEF	TANDY_1000	;* don't have to worry about this
	test	byte ptr es:[KbType],10H
	jz	not_ronco				;* not extended
	mov	bx,[di].pinkbCur
	cmp	[bx].fDisableExtendedInkb,0
	jne	not_ronco				;* not allowed
	mov	[di].Int16_CmdBase,10H
	mov	[di].pmpscvwPlain,drvOffset mpscvwPlainRonco
	mov	[di].pmpscvwShift,drvOffset mpscvwShiftRonco
ENDIF	; !TANDY_1000
not_ronco:



end_enable_keyboard:

cEnd	EnableKeyboardKbd

;*	* Disable keyboard *

disable_keyboard:
	test	[di].fKeyboardEnabled,0ffh
	jz	end_enable_keyboard
	mov	[di].fKeyboardEnabled,0

;*	* disable TSRs (even if they did not register with us)
IFDEF WORDTSR
	;* inform of departure
	mov	ax,55FFh
	mov	bx,5
	mov	cx,1
	int	16H
ELSE ;!WORDTSR
	mov	ax,tsrcInitTerm
	mov	dx,verTsrTerm			;* signal termination
	xor	bx,bx
	cmp	fExiting,0
	jne	exiting_forever
	inc	bx
exiting_forever:
	int	16H				;* nothing to return
ENDIF ;!WORDTSR

;*	* remove hooks
	push	ds
	mov	bx,[di].pinkbCur
	mov	cx,[bx].fNormalKeyboardInkb
	lds	dx,[di].pfnOldInt1B
	mov	ax,251BH
	int	21H

	jcxz	int9_not_hooked
	lds	dx,ss:[di].pfnOldInt08		;* get old int 08 handler
	mov	ax, 2508h
	int	21h

	lds	dx,ss:[di].pfnOldInt09		;* get old int 9 handler
	mov	ax,2509H
	int	21H

	lds	dx,ss:[di].pfnOldInt16
	mov	ax, 2516h
	int	21h

	cmp	ss:[di].fInt15_4F_Supported,0
	je	int15_not_hooked

	lds	dx,pfnOldInt15			;* get old int 15 handler
	mov	ax,2515H
	int	21H

int15_not_hooked:

int9_not_hooked:
	pop	ds
	jmp	short end_enable_keyboard


;----------------------------------------------------------------------------
;
; ::: Decide if we're going to hook int15 (to watch for int9 4F callback) :::
; 
; On the PS/2, it's risky to directly poll the keyboard with IN AL,60.
; The BIOS int9 provides a solution; it performs an int15 w/AH=4F and AL=
; the scan code.  An application can hook int15/4F and see the scan code
; without doing the direct polling.
;
; The only app we've seen that actually \required/ this was Word 4.0.  If
; you hold down the Alt key and roll the mouse around, Word 4.0 goes bongos.
; The int15/4F callback was implemented for Word 4.00A.
;
;;; We decided with Word 4.00A to only implement the callback with 
;;; DOS >= 3.30 but not 4.x.  This may be overly restrictive, but it 
;;; guarentees we catch all the PS/2s, which was the intent.
;
; We had decided the above.  But with MS-DOS 4.0 that changed, and we now
; implement for it also, meaning for all DOS >= 3.30.
;
; Note that if any TSR grabs int9 and does the IN AL,60, we're suspect to the
; same behaviour even if we're using the callback method.  The UB Network
; software is an example.

HookInt15:

	mov	ax,3000h
	mov	[di].fInt15_4F_Supported,al
	int	21h
	xchg	ah,al

	cmp	ax,031Eh		; If DOS < 3.30, then don't hook.
	jb	NoInt15

	mov	ax,0C000h
	int	15h			; Get system configuration parms.

	jc	NoInt15			; According to PS/2 BIOS TechRef:
	or	ah,ah			;   support    -> NC and AH == 0
	jnz	NoInt15			;   nonsupport -> CY and AH != 0

	test	byte ptr es:[bx+5],00010000b	; If kbd intercept sequence
	jz	NoInt15				; is alive, install our Int15

	mov	ax,3515h
	int	21h
	mov	word ptr pfnOldInt15,bx		; Should use CS override.
	mov	word ptr pfnOldInt15+2,es

	push	ds
	push	cs
	pop	ds
	mov	dx,drvOffset Int15Handler
	mov	ax,2515h
	int	21H
	pop	ds

	dec	[di].fInt15_4F_Supported
NoInt15:
	ret

;---------------------------------
; Hooker
;	Hooks an interrupt and saves the old handler's address in [DI].bx
;
;	entry:	AL = interrupt number
;		BX = kbd data offset of dword to contain old address
;		DX = drvOffset of new handler
;
cProc	Hooker, <NEAR, ATOMIC>
cBegin	Hooker

	AssertEQ di, OFF_lpwDataKbd

	push	dx
	push	ax
	push	bx

	mov	ah, 35h
	int	21h
	mov	ax, bx
	pop	bx
	add	bx, di
	mov	[bx], ax
	mov	[bx+2], es

	pop	ax
	mov	ah, 25h
	pop	dx
	push	ds
	push	cs
	pop	ds
	int	21h
	pop	ds

cEnd	Hooker

;----------------------------------------------------------------------------
;
;   TsrRequest
;
;	entry : AH = request code
;	* called by TSR programs to request action
;	exit : n/a (trashes normal registers)

cProc	TsrRequest, <FAR, ATOMIC>, <DS, DI>
cBegin	TsrRequest

	mov	cx,SEG_lpwDataKbd
	mov	ds,cx
	mov	di,OFF_lpwDataKbd		;* Data in data segment
	mov	bx,[di].pinkbCur
;*	* DS:DI => data, DS:BX => INKB

	cmp	ah,tsrrIgnoreAltUp
	jne	not_ignore_altup
	mov	[bx].fNonAltKeyHitInkb,1	;* => ignore next
not_ignore_altup:
	cmp	ah,tsrrAbort
	je	tsr_special_abort
end_tsr_request_poll:
;*	* after any request, poll the TSR
	mov	[bx].fPollKeyboardInkb,1
end_tsr_request_nopoll:

cEnd	TsrRequest


tsr_special_abort:
;*	* we have an escape key
	cCall	[bx].lpfnSpecialAbortInkb	;* call SpecialTsrAbort
	jmp	short end_tsr_request_nopoll

;----------------------------------------------------------------------------
;
; Int1BHandler()
;
; INT 1B is invoked when the ROM BIOS sees CTRL-BREAK.
; We hook this interrupt so that DOS never sees the CTRL-BREAK.
;
; Inputs:	none.
; Outputs:	none.
; Uses:		none, everything is saved.

Int1BHandler:
IFDEF KBD_CTRL_C_ABORT
	push	ds
	push	di
	push	ax
	mov	ax,SEG_lpwDataKbd
	mov	ds,ax
	mov	di,OFF_lpwDataKbd	;* Data in data segment
	mov	di,[di].pinkbCur	;* point to INKB
	mov	[di].fAbortInkb,1	;* Weird Abort (no message)
	pop	ax
	pop	di
	pop	ds
ENDIF
	iret


;----------------------------------------------------------------------------
;
;  InKey
;	entry : n/a
;	* Get an input key
;	exit : DX:AX = long value
;		DX = shift states
;		AH = scan code
;		AL = char
;		DS:DI => driver data
;	Note : a null character state is returned as AX==0

cProc	InKey,<PUBLIC,NEAR,ATOMIC>,<SI>
cBegin	InKey

	AssertEQ di,OFF_lpwDataKbd

	mov	bx,[di].pinkbCur
	mov	cx,[bx].fNormalKeyboardInkb
	jcxz	inkey_tsr

	xor	ax,ax
	cCall	LockRemoveQueue
	mov	dx,[di].ssCur
	jnz	end_inkey			; queue already locked
	mov	bx,[si].pHeadQueue
	cmp	bx,[si].pTailQueue
	jne	ik_readq
	cCall	FnzGetBiosKey
	jnz	ik_releaseq
	xor	ax,ax
	jmp	short ik_releaseq

ik_readq:
	mov	ax,[bx+0]
	mov	dx,[bx+2]
	cCall	IncQueuePtr
	mov	[si].pHeadQueue,bx

ik_releaseq:
	cCall	ReleaseRemoveQueue

end_inkey:

cEnd	InKey

inkey_tsr:
	xor	bx,bx
	mov	ax,tsrcInkey			;* special get key request
	int	16H				;* returns AH = sc, AL = char
						;* BX = ss
	mov	dx,bx
	jnz	end_inkey			;* got one (dx always valid)
	xor	ax,ax
	jmp	short end_inkey
	

;----------------------------------------------------------------------------


;********** FnzGetBiosKey **********
;*	entry : DS:DI => driver data
;*	* check for a bios character
;*	exit : z=> no key,
;*		else AX = char + scan code, DX = shift states
;*	TRASHES : AX/DX

cProc	FnzGetBiosKey, <NEAR>
cBegin	FnzGetBiosKey

	AssertEQ di,OFF_lpwDataKbd

	push	bx			;* Some TSRs clobber this
	push	cx			;* possibly this too

	mov	ah,[di].Int16_CmdBase	; Is there anything to get?
	inc	ah
	pushf
	call	[di].pfnOldInt16	; call the real BIOS, not our hook
	jz	end_bios_get_key	; nope return z

					; Liar!  Check again!

	mov	ah,[di].Int16_CmdBase	; Burn some time; get kbd flags.
	inc	ah
	inc	ah
	pushf
	call	[di].pfnOldInt16	; call the real BIOS, not our hook

	mov	ah,[di].Int16_CmdBase	; Is there anything to get?
	inc	ah
	pushf
	call	[di].pfnOldInt16	; call the real BIOS, not our hook
	jz	end_bios_get_key	; nope return z

	mov	ah,[di].Int16_CmdBase	; Get char + scan code
	pushf
	call	[di].pfnOldInt16

	mov	dx,[di].ssCur
	or	sp,sp			; set NZ flag, we got a key!

end_bios_get_key:
	pop	cx
	pop	bx
cEnd	FnzGetBiosKey


;----------------------------------------------------------------------------

Int15Handler:

	cmp	ah,4Fh
	jne	@F

	push	ax
	push	bx
	push	cx
	push	si
	push	di
	push	ds
	push	es

	mov	di,SEG_lpwDataKbd
	mov	ds,di
	mov	di,OFF_lpwDataKbd	; DS:DI -> Data in data segment
	mov	si,[di].pinkbCur	; DS:SI -> INKB data
	les	bx,lpsslBios		; ES:BX -> kbd data in low ram

	call	ScanCodeStuff		; al = the scan code.

	stc				; We don't want to eat the keystroke.

	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	bx
	pop	ax

@@:
	jmp	[pfnOldInt15]

;----------------------------------------------------------------------------
;
;	The keyboard hardware interrupt handler

Int09Handler:

	push	ax
	push	bx
	push	cx
	push	dx
	push	si
	push	di
	push	ds
	push	es

	mov	di,SEG_lpwDataKbd
	mov	ds,di
	mov	di,OFF_lpwDataKbd	; DS:DI -> Data in data segment
	mov	si,[di].pinkbCur	; DS:SI -> INKB data
	les	bx,lpsslBios		; ES:BX -> kbd data in low ram

	cmp	[di].fInt15_4F_Supported,0
	jne	@F

	in	al,KbDataPort
	cCall	ScanCodeStuff			; Do the scan-code stuff.

@@:	mov	[di].fUnhook16, 1		; Give normal int16 to TSRs
	pushf
	call	[di].pfnOldInt09		; Daisy chain.
	mov	[di].fUnhook16, 0

; Restore anything we did to the BiosShiftState in ScanCodeStuff.

	test	[di].fShiftStateDiddled,0ffH
	jz	@F				; Jmp if not diddled
	mov	al,[di].sslMirror
	mov	es:[bx],al
	mov	[di].fShiftStateDiddled,0
@@:
	mov	al,es:[bx]
	mov	[di].sslMirror,al

ifdef KANJI
	mov	ah, 51h			; AX bios: get KANA shift status
	int	16h

	and	[di].ssCur, not SS_KANA
	test	ah, 00000010b		; test KANA bit
	jz	@F
	or	[di].ssCur, SS_KANA
@@:
endif	; KANJI

	cmp	[di].rstCur, RST_REPEATING
	jne	@F
	call	KillQueue
	jmp	short ExitInt09
@@:
	call	CopyQueue

ExitInt09:
	mov	[si].fPollKeyboardInkb,1    ;; poll to see if anything happened

	pop	es
	pop	ds
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	iret

;----------------------------------------------------------------------------
;
; Input:     AL = the scan code (this is all-important)
;            DS:SI = ptr to keyboard data.
;            DS:DI = ptr to Data in data segment.
;            ES:BX = ptr to keyboard data in low RAM.
;
; Crunches:  AX,CX
;
; Preserves: BX,DX,SI,DI,DS,ES

cProc	ScanCodeStuff, <NEAR>
cBegin	nogen	; ScanCodeStuff

	cld

	mov	ah,al
	and	ah,080H			; make/break in ah

;*	* In order to detect repeating key sequences, the "fKeyIsUp" global
;*	*  variable is clear whenever a key is down (set with any keyup).
;*	* The "fKeyWasUp" variable is set on each key up event.
;*	* (i.e. fKeyIsUp is the state, fKeyWasUp is the transition).
;*	* NOTE : this is not fool-proof but handles most cases well, see the
;*	*  API document for example usage of these two flags


	cmp	al,0E0H
	je	dont_touch_fkey		;* skip for extended key
	mov	[si].fKeyIsUpInkb,ah
	cmp	al,0F0H
	je	dont_touch_fkey		;* skip for break
	or	[si].fKeyWasUpInkb,ah
dont_touch_fkey:

	and	al,07fH			; scan code in al

IFDEF	DEBUG

;  From windows: (minor alterations)
;
;  The SYS REQ key is used to simulate an NMI.
;  This is done by clearing up the stack and doing a far jump to the NMI
;  interrupt vector location.  If the NMI interrupt points into the ROM,
;  we don't jump, since the ROM treats NMIs as parity errors.
;
;  On the RT keyboard SYS REQ is generated by hitting Alt-PrintScreen,
;  badly this combination also has a special meaning in Windows (transfert
;  screen contents to the clipboard). To survive we'll keep this 2nd
;  behaviour and the NMI feature will be reached by hitting Crt-Alt-PrintScreen.
;

SYSREQ  EQU     054h    ; PC-AT SYS REQ key
cPrint  EQU     055h

OFF_nmi	EQU	2*4
SEG_nmi	EQU	2*4+2

	or	ah,ah		    ; sysreq on key down
	jnz	notsys
        cmp     al,SYSREQ           ; SYSREQ key?
        jne     notsys

	push	es		; check for RT keyboard
	xor	cx,cx
	mov	es,cx
	test	byte ptr es:[KbType],10H
	pop	es
	jz	not_ronco_int09
	test	[di].ssCur,SS_CONTROL	; else, is Ctrl down ?
	mov	al, cPrint		; new scan code (only used if no nmi)
	jz	notsys			; if not, skip (code = Print)
not_ronco_int09:

	push	es		; get NMI vector segment
	mov	es,cx			; cx is still 0
        mov     ax,word ptr es:[SEG_nmi]
	pop	es
        cmp     ax,0F000H         	; see if it points to ROM
        jne     do_nmi
        jmp     ExitScanCodeStuff

do_nmi:

ack_port equ    20h         ; 8259 acknowledge port
eoi      equ    20h         ; 8259 end of interrupt
kb_ctl   equ    61h

	in	al,kb_ctl
	mov	ah,al
	or	al,80h
	out	kb_ctl,al
	xchg	al,ah
	out	kb_ctl,al
	mov	al,eoi			; don't pass this on, eat it
	out	ack_port,al

	int	2
;;;	jmp	ExitInt09
	jmp	ExitScanCodeStuff

notsys:	; end of sysreq test

ENDIF	;DEBUG

;
; Software autorepeat control
;	This is all bypassed unless 0 < wRateKeyRepeat <= 10.
;	There are three states: IDLE, WAITING, and REPEATING.
;	IDLE is the default state, when no keydowns have been
;	received yet.  If a keydown is received in the IDLE state,
;	the state is changed to WAITING, indicating that we are
;	waiting for another copy of the same key code (when the
;	keyboard indicates enough time has passed to begin actually
;	repeating).  When we receive the same key again in the
;	WAITING state, we move on to the REPEATING state and init
;	the repeat delay counter.  This cues the timer-interrupt
;	to dump in wRateKeyRepeat extra keys every 1/18 second.  If
;	a keydown of a different key is received in the WAITING or
;	REPEATING states, the WAITING state is restarted as above.
;	A keyup with the same scan code as the last keydown changes 
;	the state to IDLE.
;
	or	ah,ah			;* key break => maybe stop repeat
	jns	a_key_down
	cmp	al, [di].scRepeatWhich	;* ignore if not same key
	jne	repeat_done
	mov	[di].rstCur, RST_IDLE
	jmp	short kill_pending	;* kill pending repeats
a_key_down:
	cmp	[di].rstCur, RST_IDLE
	jne	not_first
	cmp	[si].wRateKeyRepeatInkb, 0  ;* skip if rate = default
	jle	repeat_done
start_waiting:
	mov	[di].rstCur, RST_WAITING  ;* waiting to start repeating
	mov	[di].scRepeatWhich, al
kill_pending:
	mov	[di].ckeyRepeat, 0	;* kill pending repeats
	jmp	short repeat_done
not_first:
	cmp	al, [di].scRepeatWhich
	jne	start_waiting
	cmp	[di].rstCur, RST_REPEATING
	je	repeat_done		;* and later ignore char
	mov	cx, [si].wRateKeyRepeatInkb  ;* get CW key repeat rate
	mov	[di].ckeyRepeat, cx
	mov	[di].fRepeatToggle, 0
	mov	[di].rstCur, RST_REPEATING
repeat_done:

;
; Look for space bar down/up, and maintain its state in ssCur
;
	cmp	al,SC_SPACE
	jne	not_space_key
;*	* update the extra shift state for the spacebar
	or	[di].ssCur,SS_SPACE		;* assume down
	test	ah,080H
	jz	shift_is_down
	and	[di].ssCur,NOT SS_SPACE		;* shift is off
shift_is_down:

not_space_key:
	or	ah, ah
	jnz	ExitScanCodeStuff

;*	* if ALT or Extended key then pass on
	cmp	al,SC_ALT
	je	ExitScanCodeStuff
	cmp	al,SC_EXTENDED
	je	ExitScanCodeStuff
;*	* otherwise we have a non-alt key
	or	[si].fNonAltKeyHitInkb,ah	;* set if key down

;
; The BIOS eats a lot of CTRL- and ALT- keys that we need to preserve.
; So if CTRL or ALT is held down, we might have to fool the BIOS into
; passing the key, which we do by turning off the CTRL or ALT shift 
; state, chaining to the BIOS, then resetting the shift state.  So now
; we look up our key in a bit-packed table to see if it requires diddling...
; We also do some weird diddling (temporarily turn on the Shift key)
; for numpad 5, cause the BIOS eats that, too.
;
	mov	ah, es:[bx]		; current shift state

IFDEF	STD_NUMPAD
	cmp	al, 76			; numpad 5 scan code
	jne	CheckAltCtrl
	test	ah, SS_ALT
	jnz	ExitScanCodeStuff	; don't diddle Alt-numpad
	test	ah, SS_NUMLOCK
	jnz	NumLock5
	or	ah, SS_LSHIFT		; NumLock off ==> turn on shift
	jmp	short Num5Diddle
NumLock5:
	test	ah, SS_SHIFT
	jz	Num5Diddle		; NL, no shift ==> don't diddle
	and	ah, not SS_NUMLOCK	; NL, shift ==> turn off NL
Num5Diddle:
	and	ah, not SS_CONTROL	; turn off Ctrl
	mov	es:[bx], ah
	jmp	short SetDiddle
ENDIF	; STD_NUMPAD

IFNDEF TANDY_1000	; Tandy 1000 doesn't require any diddling
CheckAltCtrl:	
	and	ah, SS_ALT or SS_CONTROL
	jz	ExitScanCodeStuff
	push	bx
	mov	bl, al
	mov	cl, al
	and	cl, 7
	mov	al, 1
	shl	al, cl			; bit mask
	mov	cl, 3
	shr	bl, cl			; byte address
	xor	bh, bh
	cmp	ah, SS_ALT
	je	CheckAlt
	cmp	ah, SS_CONTROL
	je	CheckCtrl
	add	bx, offset mpscffAltCtrlDiddle
	jmp	short CheckDiddle
CheckCtrl:
	add	bx, offset mpscffCtrlDiddle
	jmp	short CheckDiddle
CheckAlt:
	add	bx, offset mpscffAltDiddle
CheckDiddle:
	test	cs:[bx], al
	pop	bx
	jz	ExitScanCodeStuff
DoDiddle:
	mov	byte ptr es:[bx], 0
SetDiddle:
	mov	[di].fShiftStateDiddled,ah
ENDIF ;TANDY_1000

ExitScanCodeStuff:

	ret

cEnd	nogen	; ScanCodeStuff


;----------------------------------------------------------------------------

;*	* * * QUEUE CONTROL * * *

;---------------------------------------
;
; LockInsertQueue
; LockRemoveQueue
;
;   Access the driver's keyboard queue for Insertion and Removal
;
;   entry : DS:DI => driver data
;
;   exit  : DS:SI => queue structure
;           ZR => queue available and now locked
;           NZ => queue already locked, unavailable

cProc	LockInsertQueue, <PUBLIC, NEAR, ATOMIC>
cBegin	LockInsertQueue

	AssertEQ di,OFF_lpwDataKbd

	lea	si,[di].queueKb
	inc	[si].semInsertQueue
	jz	@F
	cCall	ReleaseInsertQueue
@@:

cEnd	LockInsertQueue

cProc	LockRemoveQueue, <PUBLIC, NEAR, ATOMIC>
cBegin	LockRemoveQueue

	AssertEQ di,OFF_lpwDataKbd

	lea	si,[di].queueKb
	inc	[si].semRemoveQueue
	jz	@F
	cCall	ReleaseRemoveQueue
@@:

cEnd	LockRemoveQueue

;---------------------------------------
;
; ReleaseInsertQueue
; ReleaseRemoveQueue
;
;   Release the queue (make it available to other threads)
;
;   entry : DS:SI => queue structure
;
;   exit  : all registers (including flags) preserved

cProc	ReleaseInsertQueue, <PUBLIC, NEAR, ATOMIC>
cBegin	ReleaseInsertQueue

	pushf
	dec	[si].semInsertQueue
	popf

cEnd	ReleaseInsertQueue

cProc	ReleaseRemoveQueue, <PUBLIC, NEAR, ATOMIC>
cBegin	ReleaseRemoveQueue

	pushf
	dec	[si].semRemoveQueue
	popf

cEnd	ReleaseQueue

;;********** IncQueuePtr **********
;*	entry : BX = current queue pointer
;*		SI => QUEUE structure
;*	* bump pointer
;*	exit : BX = new pointer

cProc IncQueuePtr, <NEAR, ATOMIC>
cBegin	IncQueuePtr

	add	bx,4				;* bump long
	cmp	bx,[si].pEndQueue
	jne	inc_ok
	mov	bx,[si].pStartQueue		;* wrap around
inc_ok:
cEnd	IncQueuePtr



;********** CopyQueue **********
;*	entry:	DS:DI => driver data
;*	* copy item from bios Q to our buffer QUEUE (ignore if no room)
;*	exit : n/a

cProc	CopyQueue,<NEAR>, <si>
cBegin	CopyQueue

	AssertEQ di,OFF_lpwDataKbd

	cCall	LockInsertQueue
	jnz	cq_exit
	mov	bx,[SI].pTailQueue		;* add after tail

CopyNextKey:
	push	bx
	cCall	IncQueuePtr
	cmp	bx,[SI].pHeadQueue		;* is there room ?
	pop	bx
	jz	DoneQueueCopy			;* no room

	cCall	FnzGetBiosKey
	jnz	EnqueueKey

;*	* test to see if shift states have changed
	mov	ax,[di].ssCur
	mov	dx,ax
	xchg	ax,[di].ssLastInt		;* set new, get old
	cmp	ax,dx
	je	DoneQueueCopy			;* shift states the same
	xor	ax,ax				;* no char, ss changed though

EnqueueKey:
	;* DX:AX = key
	mov	[bx+0],ax
	mov	[bx+2],dx
	cCall	IncQueuePtr
	jmp	short CopyNextKey

DoneQueueCopy:
	mov	[SI].pTailQueue,bx
	cCall	ReleaseInsertQueue

cq_exit:

cEnd	CopyQueue

;----------------------------------------------------------------------------
;
;  KillQueue
;
;	entry: DS:DI => driver data
;	* empty the bios key queue
;	exit: n/a

cProc	KillQueue,<NEAR>, <si>
cBegin	KillQueue

	AssertEQ di, OFF_lpwDataKbd
KillKeys:
	cCall	FnzGetBiosKey
	jnz	KillKeys

cEnd	KillQueue

;----------------------------------------------------------------------------
;
; Int08Handler()
;
; INT 08 is invoked 18.2 times per second by the timer chip.  We hook
; this to get a regular interval for software auto-repeat.

Int08Handler:
	push	ds
	push	di
	push	ax
	mov	ax, SEG_lpwDataKbd
	mov	ds, ax
	mov	di, OFF_lpwDataKbd
	pushf					;* first chain to Dos
	call	[di].pfnOldInt08
	cmp	[di].rstCur, RST_REPEATING	;* skip if not repeating
	jne	int08_done
	push	si
	mov	si, [di].pinkbCur
	mov	ax, [si].wRateKeyRepeatInkb
	add	[di].ckeyRepeat, ax
	mov	[si].fPollKeyboardInkb, 1	;* tell CW about it
	pop	si
int08_done:
	pop	ax
	pop	di
	pop	ds
	iret

;----------------------------------------------------------------------------
;
; Int16Handler()
;
; BIOS int 16h (keyboard services) emulator.
; Traps AH = 0, 1, 2, 10h, 11h, or 12h; the rest are passed
;  to the default handler.
;
Int16Handler proc far
	push	ax	;(cs)	; sp +	16	;* this is for chaining
	push	ax	;(ip)	; 	14	;* (see below)
	push	ds		; 	12
	push	di		; 	10
	push	si		; 	8
	push	dx		; 	6
	push	cx		; 	4
	push	bx		; 	2
	push	ax		; 	0

	mov	di, SEG_lpwDataKbd
	mov	ds, di
	mov	di, OFF_lpwDataKbd
	cmp	[di].fUnhook16, 0
	jnz	int16_chain
	or	ah, ah
	je	int16_0
	cmp	ah, 1
	je	int16_1
	cmp	ah, 2
	je	int16_2
	cmp	ah, 10h
	je	int16_0
	cmp	ah, 11h
	je	int16_1
	cmp	ah, 12h
	je	int16_2

; Since we need to chain to the previous int 16 handler, but can't
; afford to mess up any registers to do it (e.g., via an indirect
; call or jmp using DS and DI) we poke the handler's address into a 
; convenient blank space in the stack frame, pop all registers, 
; and "return" to the old int 16 handler.  (I.e., pop+jmp an address
; on the stack.)
;
int16_chain:
	mov	bx, sp
	mov	ax, word ptr [di].pfnOldInt16	;* segment
	mov	ss:[bx+14], ax
	mov	ax, word ptr [di].pfnOldInt16+2	;* offset
	mov	ss:[bx+16], ax
	pop	ax
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	ds
	ret	;far				;* to [pfnOldInt16]

int16_0:
	cCall	InKey				;* copy queue, get key
	or	ax, ax
	jnz	int16_0done			;* go return the key
	or	dx, dx				;* make sure we've got 
	jnz	int16_0				;*  all the SS events too
	cmp	[di].ckeyRepeat, 0		;* check for repeating keys
	jz	int16_0				;* keep polling if none
	dec	[di].ckeyRepeat
	mov	ax, word ptr [di].chPrev	;* use the previous key
int16_0done:
	mov	word ptr [di].chPrev, ax	;* save for repeats
	jmp	short int16_done

int16_1:
	cmp	[di].ckeyRepeat, 0		;* check artificial repeats
	jnz	int16_1ret
	lea	si, [di].queueKb
	mov	bx, [si].pHeadQueue
int16_1loop:
	cmp	bx, [si].pTailQueue
	jz	int16_1nomo			;* no mo', go check BIOS
	cmp	word ptr [bx], 0
	jnz	int16_1ret			;* true if non-nil key
	cCall	IncQueuePtr			;* skip SS-only event
	jmp	short int16_1loop		;* otherwise keep looking
int16_1nomo:
	pop	ax				;* get int 16 function #
	push	ax				;* don't mess up stack frame
	pushf
	call	[di].pfnOldInt16		;* check the BIOS buffer too
int16_1ret:
	lahf					;* ha ha ha
	and	ah, 40h				;* ZF mask in cpu flags
	mov	bx, sp
	add	bx, 22				;* offset to caller's flags
	and	byte ptr ss:[bx], not 40h
	or	byte ptr ss:[bx], ah		;* poke in ZF
	jmp	short int16_done

int16_2:
	lds	si, lpsslBios
	lodsb

int16_done:
	add	sp, 2				;* incoming ax
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	ds
	add	sp, 4				;* empty space
	iret
Int16Handler endp

;************************************************************************
