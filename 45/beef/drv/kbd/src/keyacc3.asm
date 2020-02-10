;
;	CW : Character Windows Drivers
;
;	keyacc3.asm : DOS 3 accessory functions
;----------------------------------------------------------------------------

NonStandard	PollKeyboardKbd
NonStandard	FlushKeyRgchKbd
NonStandard	SetShiftKkKbd


;********** PollKeyboardKbd **********
;*	entry / exit : n/a
;*	* Poll the keyboard, send keyboard messages as needed

cProc	PollKeyboardKbd,<FAR, PUBLIC, ATOMIC>,<DI>
cBegin	PollKeyboardKbd

	mov	di,OFF_lpwDataKbd		;* Data in data segment

retry_polling:
;{{
;	fPollKeyboard = FALSE;
;	if (!FTestKeyboardEmpty())
;		/* keyboard is not empty enough */
;		return;
;}}
	mov	bx,[di].pinkbCur
IFNDEF KANJI
;*	* don't poll all the time
	mov	[bx].fPollKeyboardInkb,0
ELSE
;*	* OAX keyboard is weird -- poll all the time
	mov	[bx].fPollKeyboardInkb,1
ENDIF ;KANJI
	cCall	[bx].lpfnFTestKeyboardEmptyInkb
	or	ax,ax
	jz	end_polling			; Jump if no room in queue.

;{{
;	// note : for MSKEY and TSRs that introduce waits, we will poll 3
;		times before deciding we don't have anything
;	c = 3;
;	while (c--)
;		if ((key = InKey()) != 0 || ss != ssOld)
;			goto got_something;
;}}

	mov	cx,3
retry_inkey:
	Save	<cx>
	cCall	InKey				;* DX:AX = value
	or	ax,ax
	jnz	got_something
;*	* are the shift states changed ??
	cmp	dx,[di].ssOld
	jne	got_something
	loop	retry_inkey

;{{	/* Now that we've taken care of all "real" keys (including TSRs),
;		we handle artificial keys from software autorepeat */
;	if (ckeyRepeat > 0)
;		{
;		ss = ssPrev;
;		sc = scPrev;
;		ch = chPrev;
;		ckeyRepeat--;
;		XlateKey(key);
;		}
;	return;
;}}
	cmp	[di].ckeyRepeat, 0
	jle	end_polling
	mov	ax, word ptr [di].chPrev
	mov	dx, [di].ssPrev
	dec	[di].ckeyRepeat
	cCall	XlateKey
	jmp	short end_polling

got_something:

;{{	/* Save the key to prepare for software autorepeat */
;	scPrev = sc;
;	chPrev = ch;
;	ssPrev = ss;
;}}
	mov	word ptr [di].chPrev, ax
	mov	[di].ssPrev, dx

;{{
;	XlateKey(key);
;}}
	cCall	XlateKey			;* parameters in AX/DX
	jmp	retry_polling			;* keep going till Queue empty

end_polling:

cEnd	PollKeyboardKbd

;*****************************************************************************

;********** FlushKeyRgchKbd **********
;*	* KBD entry point (see documentation for interface)
;*	* poke key events into BIOS buffer

cProc	FlushKeyRgchKbd, <FAR, PUBLIC, ATOMIC>, <SI, DI>
    parmDP  rgchBuffer
cBegin	FlushKeyRgchKbd

	mov	di,OFF_lpwDataKbd		;* Data in data segment

	AssertEQ [di].fKeyboardEnabled,0	;* keyboard must be disabled

;*	* First flush the CW event queue buffer
	mov	si,rgchBuffer
loop_flush_1:
	lodsb
	or	al,al
	jz	done_flush_1
	xor	ah,ah				;* scan code info gone
	cCall	PokeCh
	jmp	short loop_flush_1

done_flush_1:
;*	* now flush the low level buffer
loop_flush_2:
	lea	si,[di].queueKb
	mov	bx,[si].pHeadQueue
	cmp	bx,[si].pTailQueue
	je	done_flush_2

	mov	ax,[bx]
	cCall	IncQueuePtr
	mov	[si].pHeadQueue,bx
	cCall	PokeCh
	jmp	short loop_flush_2
done_flush_2:

cEnd	FlushKeyRgchKbd



;********** PokeCh **********
;*	entry:	al = character, ah = scan code
;*	* Poke key event back into BIOS buffer
;*	exit: n/a

cProc	PokeCh, <NEAR, ATOMIC>, <DS>
cBegin	PokeCh

	CLI

	xor	cx,cx
	mov	ds,cx
	mov	cx,DS:[BUFFER_TAIL]
	mov	bx,cx
;*	* increment buffer pointer
	inc	cx
	inc	cx
	cmp	cx,DS:[BUFFER_END]
	jne	@F
	mov	cx,DS:[BUFFER_START]
@@:
	cmp	cx,DS:[BUFFER_HEAD]		;* full ??
	je	end_poke			;* no room!
	mov	DS:[BUFFER_TAIL],cx
	add	bx,400H				;* bias for seg 40:
	mov	DS:[bx],ax

end_poke:
	STI

cEnd	PokeCh

;*****************************************************************************

BIOS_SHIFT	equ	417h

;*********** SetShiftKkKbd *************
;*	* KBD entry point
;*	* set the BIOS shift states given a KK

cProc	SetShiftKkKbd, <FAR, PUBLIC, ATOMIC>, <DI>
	parmW	kkParm
cBegin	SetShiftKkKbd

	mov	di,OFF_lpwDataKbd

	;* translate KK to SS
	mov	ax, (kkParm)
	mov	al, ah
	and	al, SS_CAPLOCK or SS_NUMLOCK or SS_SCRLOCK or SS_ALT or SS_CONTROL or SS_LSHIFT

	;* modify BIOS shift state
	xor	cx, cx
	mov	es, cx
	mov	es:[BIOS_SHIFT], al

	xor	ah, ah
	cCall	DoShift, <ax>

cEnd	SetShiftKkKbd

