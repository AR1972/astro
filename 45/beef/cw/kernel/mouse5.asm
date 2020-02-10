;*
;*	COW : Character Oriented Windows
;*
;*	mouse5.asm : mouse input control (DOS 5)

	title	mouse - Mouse control code for COW KERNEL (DOS 5)

	include kernel.inc
	include uevent.inc			;* MK_ values & mouse messages
	include inscr.inc
	include inmou.inc			;* MK_ values & mouse messages

	include	mouse5.inc			;* DOS 5 specific info

;*****************************************************************************

IFDEF DEBPUB
	PUBLIC	MouseThread, ProcessRecord
ENDIF ;DEBPUB

;*****************************************************************************

cbBuffer	EQU	128

;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DGROUP

externB <axMac, ayMac>				;* screen size
externB	<ayMouse>
externB <instCur>
externB	<ayDrawing, fMouseLockedOut>		; for drawing lockout

staticB fEnableMonitor,0			;* TRUE => monitor is on

	EVEN
areamouOff	AreaMou	<0, 0, 0, 0>


;*	* Info for Mouse monitor thread *
staticW idThread,?				;* Thread ID
	dw	64 dup (?)
labelW	ThreadStack				;* Thread stack


DataBuffer MouseRcd <>				;* DataBuffer for mouse records
	EVEN

staticW Bytecnt,?	  			;* mouse monitor record length

staticW	hDevice,?				;* mouse device handle
staticW hMonitor,?				;* mouse monitor handle
staticW hPointer,?				;* mouse pointer handle
staticB BufferI, ,cbBuffer			;* Monitor buffer In
staticB BufferO, ,cbBuffer			;* Monitor buffer Out

staticB mstateOld, 0				;* old mouse state

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	BSS
    assumes DS,DGROUP

;* PUBLIC
externB	<fMousePresent>				;* TRUE => mouse present

;* PRIVATE
externB	<axMouse>
externW	<sstMouse>

externB	<fMouseOn>				;* TRUE => mouse on

IFDEF MOUSE_SWAP
externB <fSwapButton>				;* TRUE => reverse buttons
ENDIF ;MOUSE_SWAP

externB	<instCur>

sEnd	BSS

;----------------------------------------------------------------------------

;*****************************************************************************

sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP

szMouse		db     'MOUSE$',0			;* device string name
szPointer	db     'POINTER$',0			;* pointer string name

;********** FInitMouse **********
;*	entry : n/a
;*	* Initialize mouse if present
;*	exit : TRUE (1) => ok (even if mouse not present), FALSE (0) => error

init_noMouse:
	xor	ax,ax
	jmp	init_ok

cProc	FInitMouse,<FAR,PUBLIC>
    localW  wAction		;* ignored
    localV  rgwPtrPos,4		;* row, col
cBegin	FInitMouse

;*	* if second time initialize, just enable mouse -- the monitor
;*	*   and monitor thread should be still running
	cmp	fMousePresent,0
	je	@F			;* try init
	jmp	done_thread_start	;* thread already started
@@:

;*	* OPEN Mouse device
	PushArg	<cs, initOffset(szMouse)>
	PushArg	<ds, dataOffset(hDevice)>
	lea	ax,wAction
	PushArg <ss, ax>
	xor	ax,ax
	PushArg	<ax, ax>		; filesize (zero)
	PushArg	<ax>			; file attribute (zero)
	push	1			; open flag (open existing file)
	push	0000000011000000b	; open mode
;		DWFRRRRRISSSRAAA	  (private, deny none, read only)
	PushArg	<ax, ax>		; reserved
	call	DosOpen
        or      ax,ax
	jnz	init_noMouse

;*	* Open monitor
	PushArg	<cs, initOffset(szMouse)>
	PushArg	<ds, dataOffset(hMonitor)>
	cCall	DosMonOpen
	or	ax,ax
	jnz	init_error2

;*	* Register monitor
	mov	WORD PTR BufferI,cbBuffer
	mov	WORD PTR BufferO,cbBuffer
	PushArg	<hMonitor>
	PushArg	<ds, dataOffset(BufferI)>
	PushArg	<ds, dataOffset(BufferO)>
	PushArg	<2>			; Posflag = 2 (back)
	PushArg	<-1>			; index = -1 (current group)
	cCall	DosMonReg
	or	ax,ax
init_error2:
	jnz	init_error

;*	* create mouse thread
	PushArg	<SEG kernelBase, kernelOffset(MouseThread)>
	PushArg	<ds, dataOffset(idThread)>
	PushArg	<ds, dataOffset(ThreadStack)>
	cCall	DosCreateThread
	or	ax,ax
	jnz	init_error

;*	* bump priority of mouse thread
	PushArg	<2>		       ; scope = 2 (thread)
	PushArg	<3>		       ; class = 3 (time-critical)
	PushArg	<15>		       ; delta = 15(???)
	PushArg	<idThread>
	cCall	DosSetPrty
	or	ax,ax
	jnz	init_error

done_thread_start:
;*	* open pointer
	PushArg	<cs, initOffset(szPointer)>
	PushArg	<ds, dataOffset(hPointer)>
	cCall	MouOpen
	or	ax,ax
	jnz	init_error

;*	* get current mouse position
	lea	bx,rgwPtrPos
	cCall	MouGetPtrPos,<ds, bx, hPointer>	;* (ds == ss)
	or	ax,ax
	jnz	init_error
	mov	al,byte ptr [bx]
	mov	ayMouse,al
	mov	al,byte ptr [bx]+3
	mov	axMouse,al

;*	* everything enabled
	mov	al,1
init_ok:
	mov	fEnableMonitor,al
	mov	fMousePresent,al
	mov	ax,sp			;* success
init_end:
	mov	fMouseOn,0

cEnd	FInitMouse

init_error:
	xor	ax,ax			;* return error
;--	mov	fMousePresent,0		;* already off
	jmp	init_end


sEnd	INIT

;*****************************************************************************

sBegin	EXIT
    assumes CS,EXIT
    assumes DS,DATA


;********** EndMouse **********
;*	entry : n/a
;*	* Reset mouse if present
;*	* change thread into a literal copy monitor
;*	*  we can't kill the mouse monitor thread at this time so we
;*	*  just nullify the effect of the monitor -- the monitor stays
;*	*  hooked and will remain in literal copy mode until we re-init
;*	*  the mouse or the main process terminates.
;*	exit : n/a

cProc	EndMouse,<PUBLIC, FAR>
    localV   mrcdT,<SIZE MouseRcd>		;* mouse record
cBegin	EndMouse

	cmp	fMousePresent,0
	je	mouse_already_off

	cCall	DisableMouse
	mov	fEnableMonitor,0

;*	* end use of pointer
	cCall	MouClose,<hPointer>

;*	* NOTE: Keep fMousePresent flag set
	mov	fMouseOn,0
mouse_already_off:

cEnd	EndMouse

sEnd	EXIT

;*****************************************************************************

;*	* Discardable Mouse Functions *

sBegin	MOUSE
    assumes CS,MOUSE
    assumes DS,DATA

;********** FEnableMouse **********
;*	entry : fOn => whether mouse should be on or off
;*	* Change state of mouse according to fOn
;*	exit : AX = previous mouse state, always FALSE if no mouse

cPublic	FEnableMouse,<>
    parmW fOn

cBegin	FEnableMouse

	mov	al,fMousePresent
	or	al,al
	jz	already_ok			; no driver => ret FALSE

	mov	al,fMouseOn			; old state
	or	al,al				; ZR/NZ -> now off/on.

	mov	cx,fOn				; cx == 0 => turn off
	jcxz	mouse_off

	jnz	already_ok			; Bail if it's already on.

	test	instCur.finstInst,finstDisableMouse
	jnz	already_ok			; on not allowed now.

	cCall	MouDrawPtr,<hPointer>		; Actually turn mouse on.

	mov	al,0ffh				; New state.
	jmp	short mouse_new_state

mouse_off:
	jz	already_ok			; Bail if it's already off.

	cCall	DisableMouse
	xor	al,al

mouse_new_state:
	mov	fMouseOn,al
	not	al				;* old state

already_ok:
	xor	ah,ah			; clear high order portion

cEnd	FEnableMouse



;********** DisableMouse **********
;*	entry/exit: n/a
;*	* Disable the mouse

cProc	DisableMouse, <NEAR>
cBegin	DisableMouse

;*	* fill in record for turning mouse off (invalidate entire region)
	xor	ah,ah
	mov	al,ayMac
	dec	al
	mov	areamouOff.dayArea,ax		;* not real height
						;* documentation wrong
	mov	al,axMac
	dec	al
	mov	areamouOff.daxArea,ax		;* not real width
						;* documentation wrong
	PushArg	<ds, dataOffset(areamouOff)>
	PushArg	<hPointer>
	cCall	MouRemovePtr			; Actually turn mouse off.

cEnd	DisableMouse


;********** SetMousePos **********
;*	entry : axNewPos
;*		ayNewPos
;*	* Place the mouse cursor somewhere on the screen.
;*	exit : n/a

cPublic	SetMousePos,<>
    parmW axNewPos
    parmW ayNewPos
cBegin	SetMousePos

	mov	ax,ayNewPos
	AssertEq	ah,0

	cmp	fMousePresent,ah
	jz	PM99

	mov	ayMouse,al

	mov	ax,axNewPos
	AssertEq	ah,0
	mov	axMouse,al

	lea	ax,ayNewPos
	cCall	MouSetPtrPos,<ss,ax,hPointer>
	AssertEq	ax,0
PM99:

cEnd	SetMousePos

;********** SetMouseCursor **********
;
;	entry : pmcb = far pointer to a Mouse Cursor Block
;
;	Set the mouse cursor
;
; Amazingly, there is no OS/2 support for the Graphics Mouse Cursor in
; protect mode.

cPublic	SetMouseCursor,<>,<DS>
parmD	pmcb
localV	moupsInfo,<size PtrShapeMou>

;---------------
;localV	pbGetBuffer,100
;localV	moupsGetInfo,<size PtrShapeMou>
;---------------

cBegin	SetMouseCursor

;---------------
;
; These two calls are intended to Get the current mouse shape.  I was hoping
; to what the proper structure is for protect mode, but hey - no such luck.
;
;	mov	cx,hPointer
;	mov	ax,0FFFFh			; cb is bogus - it'll give 
;	mov	moupsGetInfo.cbPtrShapeMou,ax	;   us the size.
;	lea	ax,pbGetBuffer
;	lea	bx,moupsGetInfo
;	cCall	MouGetPtrShape,<ss,ax,ss,bx,cx>
;
;	mov	cx,hPointer
;	lea	ax,pbGetBuffer
;	lea	bx,moupsGetInfo
;	cCall	MouGetPtrShape,<ss,ax,ss,bx,cx>
;
;---------------

	xor	ax,ax				; Set up for text mode.

	cmp	fMousePresent,al
	jz	Nix

	test	instCur.finstInst,finstGraphics
	jnz	Nix

	mov	cx,hPointer		; Get this before we lose ds.

	lds	bx,pmcb			; ds:bx -> Mouse Cursor Block.
	assumes	ds,nothing

	mov	moupsInfo.colHotPtrShapeMou,ax
	mov	moupsInfo.rowHotPtrShapeMou,ax
	inc	ax
	mov	moupsInfo.colPtrShapeMou,ax
	mov	moupsInfo.rowPtrShapeMou,ax
	shl	ax,1
	shl	ax,1
	mov	moupsInfo.cbPtrShapeMou,ax

	lea	ax,[bx].wAndMaskTextMcb
;	jmp	short DoIt
;
;ItsGfx:
;	mov	ax,16				; Set up for graphics mode.
;	mov	moupsInfo.colPtrShapeMou,ax
;	mov	moupsInfo.rowPtrShapeMou,ax
;	shl	ax,1
;	shl	ax,1
;	mov	moupsInfo.cbPtrShapeMou,ax
;	mov	ax,[bx].colHot
;	mov	moupsInfo.colHotPtrShapeMou,ax
;	mov	ax,[bx].rowHot
;	mov	moupsInfo.rowHotPtrShapeMou,ax
;
;	lea	ax,[bx].rgwAndMaskGfxMcb
;
;DoIt:
	lea	dx,moupsInfo

	cCall	MouSetPtrShape,<ds,ax,ss,dx,cx>
	AssertEq	ax,0

Nix:

cEnd	SetMouseCursor


;********** MouseConditionalOff **********
;
;	entry : pmcob = near pointer to a Mouse Conditional Off Block
;
;	Defines a region on the screen for mouse updating
;
; Here's a great catch-22.  This call is only needed in graphics mode, but
; OS/2 doesn't support a graphics mouse cursor.  So it's a big NilFactor.

    assumes CS,KERNEL
    assumes SS,DATA
    assumes DS,DATA
    assumes ES,nothing

cPublic	MouseConditionalOff,<ATOMIC>
;parmW	pmcob
cBegin	nogen	; MouseConditionalOff
	retf	2				; Clean up stack of parm.
cEnd	nogen	; MouseConditionalOff


;********** MouseOn **********
;
;	Un-does the MouseConditionalOff, which means also a big NilFactor.
;
    assumes CS,KERNEL
    assumes SS,DATA
    assumes DS,DATA
    assumes ES,nothing

cPublic	MouseShowCursor,<ATOMIC>
cBegin	MouseShowCursor
cEnd	MouseShowCursor

;********** SetMouseDoubleSpeed **********
;*	entry : mps = mickey / second speed
;*	* Set mouse double speed threshold
;*	exit : n/a

cPublic	SetMouseDoubleSpeed,<>
    parmW mps
cBegin	SetMouseDoubleSpeed

IFDEF LATER
	test	fMousePresent,0ffH
	jz	set_speed_end

	mov	dx,mps
	mov	ax,19
	int	33H			;* Set double speed
set_speed_end:
ENDIF ;LATER

cEnd	SetMouseDoubleSpeed


sEnd	MOUSE

;*****************************************************************************

externFP MouseMessage			;* from event.c

sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,DATA


; MouseThread - Mouse monitor thread
;
; IMPLEMENTATION:
;	Start
;	    Read Input from Monitor
;	    switch (FLAGS record)
;		case OPEN
;		    break;
;		case FLUSH
;		    Write output to monitor structure
;		    break;
;		case CLOSE
;		    Exit to Dos
;		default
;		    Analyse record and Call Windows Event procedure
;		    break;
;	    goto Start

cProc	MouseThread,<FAR, ATOMIC>
cBegin	nogen ;MouseThread

ThreadStart:
	mov	[Bytecnt],SIZE DataBuffer

	PushArg	<ds, dataOffset(BufferI)>
	PushArg	<0>			; WaitFlag=0 (wait)
	PushArg	<ds, dataOffset(DataBuffer)>
	PushArg	<ds, dataOffset(Bytecnt)>
	cCall	DosMonRead		;* read (do not propagate)
	or	ax,ax
	jnz	Close

	mov	si,dataOffset DataBuffer
	mov	ax,[si].flags
	test	ax,fClose
	jnz	Close			;* close it down

	cmp	fEnableMonitor,0
	jne	not_literal_monitor

;*	* in literal monitor mode
	PushArg	<ds, dataOffset(BufferO)>
	PushArg	<ds, dataOffset(DataBuffer)>
	PushArg	Bytecnt
	cCall	DosMonWrite
	jmp	short ThreadStart

not_literal_monitor:
	test	ax,fOpen OR fFlush OR fClose
	jnz	no_default

	cCall	ProcessRecord  ; default 
	jmp	short ThreadStart
no_default:
	test	ax,fClose
	jz	ThreadStart		;* keep going

Close:				; return will end this thread
	ret

cEnd	nogen ;MouseThread


;  ProcessRecord - Analyze records contents and send event to Windows
;
;  Note: Replace what used to be the hardware mouse service
;
;  IMPLEMENTATION:
;	Set motion mask bit
;	Set buttons mask bits
;	Set Time stamps
;	Call Windows event proc
;

cProc	ProcessRecord, <NEAR, ATOMIC>
cBegin	nogen ;ProcessRecord

	mov	ax,[si].Eventmsk      ; get events mask

	mov	bx,[si].axMouCur
	mov	axMouse,bl
	mov	bx,[si].ayMouCur
	mov	ayMouse,bl

;*	* convert move-with-button-down to button-down
	mov	ah,al
	and	ah,mouButMotion		;* move-with-button-down
	shl	ah,1			;* convert to button-down (only)
	or	al,ah
;*	* figure out the sstMouse states
	xor	dx,dx
	test	al,mouBut1Only
	jz	left_not_down
	or	dl,MK_LBUTTON
left_not_down:
	test	al,mouBut2Only
	jz	right_not_down
	or	dl,MK_RBUTTON
right_not_down:
	mov	sstMouse,dx
	mov	dx,ax			;* save value in dx

not_mouse_move:
;*	* convert state into transitions
	mov	ah,al
	not	ah			;* ah = !new, al = new
	mov	bh,al
	xchg	bh,mstateOld		;* update state, get old (bh = old)
	mov	bl,bh
	not	bl			;* bh = old, bl = !old
	and	ax,bx			;* ah = !new & old = key up
					;* al = new & !old = key down
	mov	dx,ax

;*	* al = dl = key down transitions
;*	* ah = dh = key up transitions
MouseMove?:
	test	ax,(mouBut1Only OR mouBut2Only) * 101H
	jnz	LeftButtonDown?
;*	* no buttons set (just move)

	mov	ax,WM_MOUSEMOVE
	cCall	DoMouseMessage

LeftButtonDown?:
	test	al,mouBut1Only
	jz	LeftButtonUp?

	mov	ax,WM_LBUTTONDOWN
	cCall	DoMouseMessage

LeftButtonUp?:
	test	ah,mouBut1Only
	jz	RightButtonDown?

	mov	ax,WM_LBUTTONUP
	cCall	DoMouseMessage

RightButtonDown?:
	test	al,mouBut2Only
	jz	RightButtonUp?

	mov	ax,WM_RBUTTONDOWN
	cCall	DoMouseMessage

RightButtonUp?:
	test	ah,mouBut2Only
	jz	test_for_mouse_lock

	mov	ax,WM_RBUTTONUP
	cCall	DoMouseMessage

test_for_mouse_lock:
;*	* Check for mouse lockout
;*	* If ABS(ayDrawing - ayMouse{new}) = FE,FF,0,1,2 then kill mouse
	mov	al,ayMouse
	sub	al,ayDrawing
	add	al,2				;* 0, 1, 2, 3, 4 => kill it
	cmp	al,5
	jae	lockout_done			;* no conflict
;*	* drawing conflict, turn mouse of if not already turned off
	test	fMouseOn,0ffh
	jz	lockout_done			;* already off
	test	fMouseLockedOut,0ffh
	jnz	lockout_done			;* already locked out

;*	* turn off mouse
	PushArg	<ds, dataOffset(areamouOff)>
	PushArg	<hPointer>
	cCall	MouRemovePtr			;* turn off
	mov	fMouseOn,0			;* mouse is off
	mov	fMouseLockedOut,0ffh		;* set locked out flag

lockout_done:
	ret

cEnd	nogen ;ProcessRecord




;********** DoMouseMessage **********
;*	entry : ax = Message type
;*		dx = mouse info
;*	* Call MouseMessage with parameter
;*	exit : dx retained
;*		ax restored with dx

cProc	DoMouseMessage,<NEAR,ATOMIC>	;* all code FIXED !
cBegin	DoMouseMessage
	push	dx
	cCall	MouseMessage,<ax>
	pop	dx
	mov	ax,dx
cEnd	DoMouseMessage


sEnd	KERNEL

;*****************************************************************************

	END
