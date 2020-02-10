;*
;*	keymon5.asm : OS/2 keyboard monitor
;*****************************************************************************

SetBxInkb MACRO				;* address inkb struct with bx
	mov	bx,[di].pinkbCur	;* assume [di] == rgwKbdData
ENDM

SetBxInos MACRO				;* address inosDrv struct with bx
	mov	bx,pinos
ENDM

PushFld	MACRO	fld			;* assume [di] == structure
	push	[di+fld]		;* [di].fld won't replace fld!!
ENDM

Os2Call	MACRO	ofn			;* Call through link table
	call	[bx+lpfn&ofn&Inos]
ENDM

;* Special call-back if in thread (DS:0 => KTHD)
ThreadCall MACRO	ofn			;* Call through link table
	mov	es,ds:[selAppKthd]
	mov	bx,pinos			;* load BX from code segment
	call	es:[bx+lpfn&ofn&Inos]
ENDM


;*****************************************************************************


;********** EnableKeyboardKbd **********
;*	entry:	pinkb => INKB data
;*		fEnable => whether to enable or disable
;*		fExiting => exiting if !fEnable
;*	* enable or disable the keyboard handler
;*	exit : n/a

cProc	EnableKeyboardKbd,<ATOMIC>,<DI>
    parmDP pinkb
    parmW  fEnable
    parmW  fExiting
cBegin	EnableKeyboardKbd

;*	* save pointer to INKB data
	mov	di,OFF_lpwDataKbd		;* Data in data segment
	mov	bx,pinkb
	mov	[di].pinkbCur,bx

	mov	bx,drvOffset Disable
	mov	cx,fEnable
	jcxz	do_it
	mov	bx,drvOffset Enable
do_it:
	call	bx				;* call Enable or Disable

cEnd	EnableKeyboardKbd


;*****************************************************************************

;********** Disable **********
;*	entry: DS:DI => driver data
;*	* since OS/2 will not kill buffers properly, we turn the keyboard
;*	*  monitor into a literal copy monitor when disabled
;*	exit: n/a

cProc	Disable,<NEAR, PUBLIC>
cBegin	Disable

	AssertEQ di,OFF_lpwDataKbd
	xor	ax,ax
	mov	[di].fEnabled,al	;* disable -> get old status
	mov	[di].fEnableMonitor,al	;* disable monitor -- keep running

cEnd	Disable


;*****************************************************************************

;********** Enable **********
;*	entry: DS:DI => driver data
;*	* opens a new keyboard monitor and creates a thread to manage
;*	  this monitor.
;*	exit:	n/a

szKbd	db	'KBD$',0	 ; device string name (Keyboard)

cProc	Enable,<NEAR, PUBLIC>
    localW  wFiller
cBegin	Enable

	AssertEQ di,OFF_lpwDataKbd
	SetBxInos				; prepare for indirect call

	cmp	[di].fEnabled,0
	je	@F				;* enable it
	jmp	enable_done
@@:

;*	* if we have started the monitor then skip
	cmp	[di].selKthd,0
	je	@F
	jmp	enable_ok			;* already have thread
@@:

;*	* One-shot initialization follows
;*	* OPEN Keyboard device
	PushArg <cs,drvOffset(szKbd)>
	lea	ax,[di].hDevice
	PushArg <ds,ax>
	lea	ax,wFiller
	PushArg <ss,ax>
	xor	ax,ax
	PushArg <ax,ax>			; filesize (zero)
	PushArg <ax>			; file attribute (zero)
	push	1			; open flag (open existing file)
	push	0000000011000000b	; open mode
;		DWFRRRRRISSSRAAA	  (private, deny none, read only)
	PushArg <ax,ax>			; reserved
	Os2Call	DosOpen
	or	ax,ax
	jnz	init_error2

;*	* Open Keyboard monitor
	PushArg <cs, drvOffset(szKbd)>
	lea	ax,[di].hMonitor
	PushArg	<ds,ax>
	Os2Call	DosMonOpen
	or	ax,ax
	jnz	init_error2

;*	* allocate KTHD segment
	lea	ax,[di].selKthd		;* segment selector
	PushArg	<(size KTHD), ds, ax, 0>
	Os2Call	DosAllocSeg
	or	ax,ax
	jnz	init_error2
@@:
	mov	es,[di].selKthd

;*	* Register monitor
	PushFld	hMonitor
	mov	ax,offset rgbBufferInKthd
	PushArg <es, ax>
	cCall	InitBuffer, <ax>
	mov	ax,offset rgbBufferOutKthd
	PushArg <es, ax>
	cCall	InitBuffer, <ax>
	PushArg <2>			; Posflag (back)
	PushArg <-1>			; index = -1 (current group)
	Os2Call	DosMonReg
	or	ax,ax
init_error2:
	jnz	init_error


;*	* create keyboard thread, ES:0 => KTHD
	PushArg <cs, drvOffset(KbdThread)>
	lea	ax,[di].idThread
	PushArg	<ds,ax>
	mov	es:[selAppKthd],ss	;* point to main App stack
	PushArg <es, (rgbStackKthd + cbStack - 10H)>	;* leave space before end of segment
	Os2Call	DosCreateThread
	or	ax,ax
	jnz	init_error

;*	* bump priority of thread
	PushArg <2>		       ; scope = 2 (thread)
	PushArg <3>		       ; class = 3 (time-critical)
	PushArg <15>		       ; delta = 15(???)
	PushFld idThread
	Os2Call	DosSetPrty
	or	ax,ax
	jnz	init_error

;*	* set for keyboard type (no problem in assuming enhanced)

	mov	[di].pmpscvwPlain,drvOffset mpscvwPlainRonco
	mov	[di].pmpscvwShift,drvOffset mpscvwShiftRonco

enable_ok:	;* ok to enable
	mov	[di].fEnabled,1		;* we are now enabled
	mov	[di].fEnableMonitor,1	;* enable monitor
	jmp	short enable_done

init_error:
	PushArg	<1, 08001H>
	Os2Call DosExit	;* exit, kill process (return 0x8001).

;*	* can't accept input => we're screwed
	mov	[di].fEnabled,0
enable_done:

cEnd	Enable


;********** InitBuffer **********
;*	entry:	ES => segment containing monitor buffer
;*		pbBuff = near pointer to start of monitor buffer
;*	* Initialize monitor buffer
;*	exit:	n/a (ES retained)

cProc	InitBuffer, <NEAR, ATOMIC>, <DI>
    parmW  pbBuff
cBegin	InitBuffer

	mov	di,pbBuff
	mov	ax,cbBuffer
	stosw			;* set length of buffer

;*	* OS/2 1.0 has a problem registering non-zero initialized buffers
	mov	cx,(cbBuffer - 2)/2
	xor	ax,ax
	rep stosw

cEnd	InitBuffer


;*****************************************************************************
;* KbdThread - Keyboard monitor thread


;*	* literal monitor mode (propagate key -- leave it alone)
literal_monitor:
	call	XmitKey
	jmp	short ThreadMain

	PUBLIC	KbdThread
KbdThread PROC FAR
	push	ss
	pop	ds
	xor	di,di			;* DS:DI => KTHD
ThreadMain:

	mov	[di].cbMonKthd,SIZE KBRD

;*	* read record from monitor
	PushArg	<ds,offset(rgbBufferInKthd)>
	push	0			; WaitFlag=0 (wait)
	PushArg <ds,offset(kbrdKthd)>
	PushArg <ds,offset(cbMonKthd)>
	ThreadCall DosMonRead		;* this may put us to sleep

	or	ax,ax
	je	@F
	jmp	Close
@@:
	mov	es,ds:[selAppKthd]
	mov	bx,OFF_lpwDataKbd

;*	* see if in we are closing
	cmp	es:[bx].fEnableMonitor,0	;* monitor enabled ?
	je	literal_monitor
	cmp	es:[bx].fEnabled,0
	je	literal_monitor			;* keyboard is disabled

	lea	si,[di].kbrdKthd
	mov	ax,[si].MonFlags

	test	ax,fOpen OR fFlush OR fClose
	jnz	noDefault

;*	* a normal event, process it
	mov	ax,[si].KbdFlags
	and	al,mAction
	jz	TakeNormalKey
	cmp	al,7			;* shift ??
	je	TakeShiftKey
	cmp	al,12H			;* pseudo-break (CTRL-C) ?
	jz	TakeNormalKey		;* just like control C
	cmp	al,2			;* extend alone ?
	je	ThreadMain		;* eat it
	cmp	al,9			;* CTRL-S ?
	je	TakeNormalKey		;* treat like normal
	cmp	al,15H			;* CTRL-P ?
	je	TakeNormalKey		;* treat like normal
	cmp	al,0AH			;* wake-up?
	je	ThreadMain		;* eat it
	cmp	al,011H			;* Break?
	je	ThreadMain		;* eat it
	cmp	al,3fH			;* not-translated
	jne	xmit_key
;*	* take a shot at translating ALT key
	test	[si].ssKbrd,SS_ALT	;* ALT down
	jz	TakeNormalKey
	cmp	[si].XlatedScan,1	;* ALT-ESC ?
	jne	TakeNormalKey		;* try if not ALT-ESC
;*	* transmit all other keys
xmit_key:
	call	XmitKey
	jmp	short ThreadMain2

TakeShiftKey:
	mov	word ptr [si].XlatedChar,0 	;* set no character (just to be sure)
TakeNormalKey:
	call	TakeKey
	jmp	short ThreadMain2

noDefault:
	test	ax,fOpen
	jz	noOpen
	jmp	short ThreadMain2	; do nothing if Open

noOpen:
	test	ax,fFlush
	jz	Close		; can be nothing except close

	call	XmitKey		; Xmit Flush to next monitor (if any)

;*	* end after flushing (since we do not get a close packet).

	or	ax,ax
	jne	Close
ThreadMain2:
	jmp	ThreadMain

;*	* Die
Close:
	xor	ax,ax
	PushArg	<ax,ax>
	ThreadCall DosExit	;* exit, return 0, kill just this thread

KbdThread ENDP


;*****************************************************************************


; XmitKey - Transmit monitor record to next
XmitKey	proc	near
	PushArg	<ds,offset(rgbBufferOutKthd)>
	PushArg <ds,offset(kbrdKthd)>
	PushArg	[di].cbMonKthd
	ThreadCall DosMonWrite
	ret
XmitKey	endp


;*****************************************************************************

;********** TakeKey **********
;*	entry:	DS:SI => keyboard monitor record
;*		DS:0 => KTHD
;*	* translate and send scan code to event proc
;*	* (don't transmit to next monitor)

cProc	TakeKey, <NEAR, ATOMIC>, <DS, DI, SI>
cBegin	TakeKey

	mov	es, ds:[selAppKthd]
	mov	di,OFF_lpwDataKbd

;*	* load up registers for call to XlateKey
	Assert	<XlatedScan EQ XlatedChar+1>
	mov	ax,word ptr [si].XlatedChar
	mov	dx,[si].ssKbrd

	or	al, al			;* some scan codes need to be
	jnz	no_convert		;*  converted for DOS3 compatibility
	or	ah, ah			;* (only check key events)
	jz	no_convert
	test	dl, SS_CONTROL OR SS_ALT
	jz	no_convert
	cmp	ah, SC_1
	jb	no_convert
	cmp	ah, SC_9
	ja	no_cvt1_9
	mov	al, ah
	add	al, '1' - SC_1
	jmp	short no_convert
no_cvt1_9:
	mov	al, '0'
	cmp	ah, SC_0
	je	cvt_char
	mov	al, '='
	cmp	ah, SC_EQUALS
	je	cvt_char
	mov	al, ','
	cmp	ah, SC_COMMA
	je	cvt_char
	mov	al, '.'
	cmp	ah, SC_PERIOD
	je	cvt_char
	mov	al, '-'
	cmp	ah, SC_SUBTRACT		;* Alt SUBTRACT
	je	cvt_char
	mov	ch, SC_SUBTRACT
	cmp	ah, 8Eh			;* Ctrl SUBTRACT
	je	cvt_scan
	mov	al, '+'
	cmp	ah, SC_ADD		;* Alt ADD
	je	cvt_char
	mov	ch, SC_ADD
	cmp	ah, 90h			;* Ctrl ADD
	je	cvt_scan
	cmp	ah, SC_ADD		;* Alt ADD
	je	cvt_scan
	mov	al, 9
	mov	ch, SC_TAB
	cmp	ah, 94h			;* Ctrl TAB
	je	cvt_scan
	xor	al, al
	jmp	short no_convert
cvt_scan:
	mov	ah, ch

cvt_char:
no_convert:
	and	dx,NOT SS_EXTENDED		;* fix left Alt key
	or	dx,SS_SPACE			;* assume on

	test	[si].KbdFlags,fBreak		;* break ?
	push	es				;* (switch to data seg)
	pop	ds
	jnz	take_keyup

	;* Fast key repeat
	mov	si, [di].pinkbCur
	mov	[si].fKeyIsUpInkb, 0
	cmp	ax, word ptr es:[di].chPrev	;* wait until 2nd same key
	jne	take_keydown
	mov	cx, [si].wRateKeyRepeatInkb
	or	cx, cx
	jle	take_keydown			;* default, don't mess w/ it
	shl	cx, 1				;* to emulate DOS3 timer
	add	[di].ckeyRepeat, cx		;* add a bunch of repeats
	mov	[di].ssPrev, dx			;* keep SS's up-to-date
take_repeat:
	cCall	[si].lpfnFTestKeyboardEmptyInkb
	or	ax, ax
	jnz	@F
	mov	[di].ckeyRepeat, ax	;0	;* minimize overruns
	jmp	short take_done
@@:	mov	ax, word ptr [di].chPrev
	mov	dx, [di].ssPrev
	cCall	XlateKey
	dec	[di].ckeyRepeat
	jnz	take_repeat
	jmp	short take_done

take_keyup:
;*	* for key up, just update shift states
	mov	[di].ckeyRepeat, 0		;* cancel pending repeats
	mov	si, [di].pinkbCur
	mov	[si].fKeyIsUpInkb, 1
	mov	[si].fKeyWasUpInkb, 1
	cmp	al,' '
	jne	not_space_up
	and	dx,NOT SS_SPACE			;* generate space up
not_space_up:
	xor	ax,ax				;* no key
take_keydown:

	mov	word ptr [di].chPrev, ax	;* save prev ch and sc
	mov	[di].ssPrev, dx
	cCall	XlateKey

take_done:
cEnd	TakeKey

;*****************************************************************************
;* Key flush

NonStandard	FlushKeyRgchKbd


;********** FlushKeyRgchKbd **********
;*	* KBD entry point (see documentation for interface)
;*	* send keys to next monitors
;*	* the keyboard monitor is assumed to be still alive

cProc	FlushKeyRgchKbd, <FAR, PUBLIC, ATOMIC>, <SI, DI>
    parmDP  rgchBuffer
cBegin	FlushKeyRgchKbd

	mov	di,OFF_lpwDataKbd		;* Data in data segment

	SetBxInos				; prepare for indirect call

	mov	cx,[di].selKthd
	jcxz	end_flush			;* no extra segment
	mov	es,cx

;*	* Flush the CW event queue buffer
	mov	si,rgchBuffer
loop_flush_1:
	lodsb
	or	al,al
	jz	done_flush_1
;*	* put that 1 character into the keyboard monitor
	mov	dl,al				;* character
	mov	es,[di].selKthd

	mov	bx,offset kbrdKthd		;* es:bx => KBRD

;*	* clear the KBRD record
	push	di
	mov	di,bx
	xor	ax,ax
	mov	cx,size KBRD / 2
	rep stosw
	pop	di

;*	* set character (other fields 0)
	mov	es:[bx].XlatedChar,dl

;*	* write the buffer
	PushArg	<es,offset(rgbBufferOutKthd)>
	PushArg <es,bx>
	PushArg	es:[cbMonKthd]
	SetBxInos				; prepare for indirect call
	Os2Call DosMonWrite

	or	ax,ax
	jz	loop_flush_1			;* go while no error
	int	3				;* write error !

done_flush_1:

end_flush:

cEnd	FlushKeyRgchKbd


;**************************************************************************
;* Set shift states

NonStandard	SetShiftKkKbd

;*********** SetShiftKkKbd *************
;*	* KBD entry point
;*	* set the actual shift states given a KK
;
cProc	SetShiftKkKbd, <FAR, PUBLIC, ATOMIC>, <SI, DI>
	parmW	kkParm
	Assert	<(size KBST) EQ 10>
	localT	kbstTmp
cBegin	SetShiftKkKbd

	mov	di, OFF_lpwDataKbd
	SetBxInos

	;* get the current keyboard status
	lea	si, (kbstTmp)
	mov	ss:[si].cbKbst, (size KBST)
	PushArg	<ss, si>
	push	0		;* system keyboard handle
	Os2Call	KbdGetStatus

	;* set the we're-gonna-change-shift-states flag
	mov	ax, ss:[si].fsMask
	or	ax, 10h
	mov	ss:[si].fsMask, ax
	
	;* translate new KK to SS
	mov	ax, (kkParm)
	and	ah, SS_CAPLOCK or SS_NUMLOCK or SS_SCRLOCK or SS_ALT or SS_CONTROL or SS_LSHIFT
	mov	byte ptr ss:[si].fsState, ah

	;* reset the keyboard status to new shift state
	PushArg	<ss, si>
	push	0		;* system keyboard handle
	Os2Call	KbdSetStatus

	mov	ax, ss:[si].fsState
	cCall	DoShift, <ax>

cEnd	SetShiftKkKbd

;**************************************************************************
