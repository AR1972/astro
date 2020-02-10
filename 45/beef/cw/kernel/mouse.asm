;*
;*	COW : Character Oriented Windows
;*
;*	mouse.asm : mouse input control

	title	mouse - Mouse control code for COW KERNEL INPUT

	include kernel.inc
	include	uevent.inc			;* for mouse messages
	include	inscr.inc			;* INST info
	include	inmou.inc

PROJECT_QB_QC = 0
ifdef PROJECT_QB
PROJECT_QB_QC = -1
endif
ifdef PROJECT_QC
PROJECT_QB_QC = -1
endif

PROJECT_QB_QC = -1			;UNDONE - Remove this

if PROJECT_QB_QC
    %out Mouse driver for QuickBASIC or QuickC
endif


;----------------------------------------------------------------------------
;*	* Mouse Status

MOUSE_MOVE		equ	01H
LEFT_BUTTON_DOWN	equ	02H
LEFT_BUTTON_UP		equ	04H
RIGHT_BUTTON_DOWN	equ	08H
RIGHT_BUTTON_UP		equ	10H
MOUSE_MASK		equ	1FH

;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DGROUP

externB <axMac, ayMac>
externB <instCur>

;*	* For drawing lockout
externB	<ayDrawing, fMouseLockedOut>

externB	<ayMouse>

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

if PROJECT_QB_QC
staticW cmdInitMouse,0
endif

sEnd	BSS

;----------------------------------------------------------------------------

sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** FInitMouse **********
;*	entry : n/a
;*	* Initialize mouse if present
;*	exit : TRUE => ok (even if not present), FALSE => initialize error

cProc	FInitMouse,<FAR,PUBLIC,ATOMIC>,<SI>
cBegin	FInitMouse

;*	* Test for mouse vector present
	mov	ax,3533H
	int	21H		; Get interrupt vector for int 33H
	mov	ax,es
	or	ax,bx
	jnz	@F
J_noMouse:
	jmp	noMouse
@@:
ifdef KANJI
	; this is not for KANJI, but some dos makes int33 going to halt
	; it is not enough to check verctor is zero or not.
	; we have to find out mouse driver be or not.
	push	di		; save DI
	push	ds		; save DS
	push	cs
	pop	ds
	mov	si, initOffset MouseID  ; points to mouse secret message
	mov	cx, offset MouseIDend - offset MouseID ; strlen of message
	mov	di, bx			; before entry point
	sub	di, cx			; back to the message
	cld
	rep	cmpsb
	pop	ds		; restore DS
	pop	di		; restore DI
	jz	MouseIDend
J_noMouse_0:
	mov	al, 0		; make sure NOMOUSE
	jmp	short J_noMouse
MouseID		DB	"*** This is Copyright 1983 Microsoft ***"
MouseIDend:
	xor	di, di
	mov	es, di		; clear es and di
	mov	ax, 'm'
	int	33h
	mov	ax, es
	or	ax, di
	jz	J_noMouse_0	; too old version
	cmp	byte ptr es:[di], 6	; check version number
	jb	J_noMouse_0	; ver6 or later?
endif ; KANJI

;*	* Test for mouse driver present
if PROJECT_QB_QC
	mov	ax, [cmdInitMouse]
else
	xor	ax,ax
endif
	int	33H
	or	ax,ax
	jz	J_noMouse

if PROJECT_QB_QC
	mov	ax,33			; try software reset
	cmp	[cmdInitMouse],ax	; already initialized to software?
	je	@F			; brif so -- got one

;
; 33 is an undocumented mouse call, if the mouse driver is not our's,
; then it may ignore `software reset (33)' so do a full hardware reset
; if we fail the software reset.
;
	int	33h			;
	inc	ax			; -1 means Microsoft mouse
	jnz	@F			; brif not Microsoft mouse

	mov	[cmdInitMouse],33	; From now on a software reset is
					;    sufficient.
@@:
endif

;*	* Set initial mouse mapping type
	xor	bx,bx
	mov	cx,077FFH
	mov	dx,07700H
	mov	ax,10
	int	33H

;*	* Set mouse event handler
	mov	ax,SEG kernelBase
	mov	es,ax
	mov	dx,offset MouseInterrupt
	mov	cx,MOUSE_MASK
	mov	ax,12			;* set mouse extension
	int	33H

	mov	si,8			; 8 is used a lot
	mov	ax,si
	mul	axMac
	sub	ax,si
	mov	dx,ax			; max = 8 pixels from the right.
	mov	cx,3			; min = 3 pixels from the left.
	mov	ax,7
	int	33h			; Set horizontal range.

	xor	cx,cx
	mov	cl,instCur.inftInst.dyCharInft
	or	cl,cl
	jnz	@F
	mov	cx,si			; assume 8 char high (text mode)
@@:					; cx = dyChar
	mov	al,ayMac
	mul	cl
	shr	cx,1
	sub	ax,cx			; max = (dyChar / 2)
	mov	dx,ax			;          pixels from the bottom.
	mov	cx,4			; min = 4 pixels from the top.
	mov	ax,si
	int	33h			; Set vertical range.
	AssertNE	al,0

if PROJECT_QB_QC
	mov	cl, 3
	xor	dx,dx
	mov	dl, [ayMouse]
	shl	dx, cl
	xor	ax,ax
	mov	al, [axMouse]
	shl	ax, cl
	mov	cx, ax
	mov	ax, 4
	int	33H
endif

noMouse:
	mov	fMousePresent,al
	mov	ax,0001			;* always return ok
	mov	fMouseOn,ah		;* mouse is off

cEnd	FInitMouse

sEnd	INIT

;----------------------------------------------------------------------------

sBegin	EXIT
    assumes CS,EXIT
    assumes SS,DATA
    assumes DS,DATA


;********** EndMouse **********
;*	entry : n/a
;*	* Reset mouse if present
;*	exit : n/a

cProc	EndMouse,<PUBLIC, FAR, ATOMIC>
cBegin	EndMouse

	xor	ax,ax
	cmp	fMousePresent,al
	jz	EndMouseExit
	mov	fMouseOn,al

if NOT PROJECT_QB_QC
	mov	fMousePresent,al
	mov	ax,33
	int	33H			; turn off mouse by resetting it.
else
	mov	ax, 33			;Try Software reset first
	int	33H
	inc	ax
	jz	@F
	xor	ax,ax			;If that fails do a hardware reset
	int	33H
@@:
endif

EndMouseExit:
cEnd	EndMouse

sEnd	EXIT

;----------------------------------------------------------------------------

;*	* Fixed : Mouse enable called by MessageBox etc.

sBegin	KERNEL
    assumes CS,KERNEL
    assumes SS,DATA
    assumes DS,DATA

;********** FEnableMouse **********
;*	entry : fOn => whether mouse should be on or off
;*	* Change state of mouse according to fOn
;*	exit : AX = previous mouse state, always FALSE if no mouse

cPublic	FEnableMouse,<ATOMIC>
    parmW fOn
cBegin	FEnableMouse

	StartPublic
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

	mov	ax,1				; Actually turn mouse on.
	int	33H
;*	* find current mouse position to set axMouse & ayMouse
	mov	ax,3
	int	33H			;* CX = horiz, DX = vertical position
	cCall	SetMousePixelPos

	mov	al,0ffh				;* new state
	jmp	short mouse_new_state

mouse_off:
	jz	already_ok			; Bail if it's already off.

	mov	ax,2				; Actually turn mouse off.
	int	33H
	xor	al,al

mouse_new_state:
	mov	fMouseOn,al
	not	al				;* old state

already_ok:
	xor	ah,ah			; clear high order portion
	StopPublic

cEnd	FEnableMouse


;********** SetMousePos **********
;*	entry : axNewPos
;*		ayNewPos
;*	* Place the mouse cursor somewhere on the screen.  Text only so far.
;*	exit : n/a

cPublic	SetMousePos,<ATOMIC>
    parmW axNewPos
    parmW ayNewPos
cBegin	SetMousePos

	StartPublic
	mov	ax,ayNewPos
	AssertEq	ah,0

	cmp	fMousePresent,ah
	jz	PM99

	mov	ayMouse,al
	mov	cl,instCur.inftInst.dyCharInft
	or	cl,cl
	jnz	@F
	mov	cl,8				;* default 8 pixels high
@@:
	mul	cl
	mov	dx,ax

IFDEF LATER
	-- characters other than 8 pixels wide (graphics modes)
ENDIF
	mov	cx,axNewPos
	AssertEq	ch,0
	mov	axMouse,cl
	shl	cx,1
	shl	cx,1
	shl	cx,1

	mov	ax,4
	int	33H			;* Set Mouse Cursor Position.
PM99:
	StopPublic

cEnd	SetMousePos

;********** SetMouseCursor **********
;
;	entry : pmcb = far pointer to a Mouse Cursor Block
;
;	Set the mouse cursor shape, both text and graphics.
;
; Hercules 48K RAMFont mode means that the 16 bits of the current character 
; are interpreted as the top 4 bits being attribute, and the lower 12 bits 
; being character.  And, those high 4 bits aren't color attributes, they're 
; character attributes: boldface, reverse, overstrike, underline.  So, for 
; 48K RAMFont, we want the mouse cursor to be able to set the character (low
; word) but not screw up the attribute.  Thus we make the mouse cursor toggle
; bit 6 of the high word.

    assumes CS,KERNEL
    assumes SS,DATA
    assumes DS,DATA
    assumes ES,nothing

cPublic	SetMouseCursor,<ATOMIC>,<DS>
parmD	pmcb

cBegin	SetMouseCursor

	StartPublic

	mov	ax,10			; ah = 0
	cmp	fMousePresent,ah
	jz	@F

	mov	dx,instCur.finstInst

	lds	bx,pmcb			; ds:bx -> Mouse Cursor Block.
	assumes	ds,nothing

	push	bx

	and	dx,(finstText OR finstFont)	; This will be true only in
	cmp	dx,(finstText OR finstFont)	;   Hercules RAMFont mode.

	mov	cx,[bx].wAndMaskTextMcb
	mov	dx,[bx].wXorMaskTextMcb

ifndef KANJI
	jne	NotRAMFont		; If 48K RAMFont, then make sure that
	mov	ch,0BFh			;   only bit 6 of the high word gets
	mov	dh,040h			;   toggled (the reverse video bit).
NotRAMFont:
endif	; not KANJI

	xor	bx,bx			; 0/1 -> Soft/hardware text cursor.
	int	33h			; Set Text Cursor
	pop	bx

	; Do not set both cursor, because Japanese machine has super-impose.
ifndef KANJI
	mov	cx,ds
	mov	es,cx
	assumes	es,nothing
	lea	dx,[bx].rgwAndMaskGfxMcb ; es:dx -> And & Xor blocks.
	mov	cx,[bx].rowHotMcb
	mov	bx,[bx].colHotMcb
	dec	al
	AssertEq	ax,9
	int	33h			; Set Graphics Cursor Block
endif	; not KANJI

@@:
	StopPublic

cEnd	SetMouseCursor


;********** MouseConditionalOff **********
;
;	entry : pmcob = near pointer to a Mouse Conditional Off Block
;
;	Defines a region on the screen for mouse updating
;
; This is needed by Word in its Page Preview mode, when Word is working with
; the GSDs.  When Word is in character-graphics mode (with CSDs), the CSD
; driver takes care of turning the mouse off and back on.
; 
; So, we can assume that we're in graphics mode when this gets called.

    assumes CS,KERNEL
    assumes SS,DATA
    assumes DS,DATA
    assumes ES,nothing

cPublic	MouseConditionalOff,<ATOMIC>,<si,di>
parmW	pmcob

cBegin	MouseConditionalOff

	StartPublic

	mov	ax,16			; ah = 0
	cmp	fMousePresent,ah
	jz	@F

	mov	bx,pmcob
	mov	cx,[bx].xLeftMCOB
	mov	dx,[bx].yTopMCOB
	mov	si,[bx].xRightMCOB
	mov	di,[bx].yBottomMCOB
	int	33h
@@:
	StopPublic

cEnd	MouseConditionalOff


;********** MouseOn **********
;
;	Un-does the MouseConditionalOff
;
    assumes CS,KERNEL
    assumes SS,DATA
    assumes DS,DATA
    assumes ES,nothing

cPublic	MouseShowCursor,<ATOMIC>

cBegin	MouseShowCursor

	StartPublic

	mov	ax,1			; ah = 0
	cmp	fMousePresent,ah
	jz	@F
	int	33h
@@:
	StopPublic

cEnd	MouseShowCursor

sEnd	KERNEL

;----------------------------------------------------------------------------

IFDEF MOUSE_EXTRAS 

sBegin	MOUSE			;* discardable MOUSE segment
    assumes CS,MOUSE
    assumes SS,DATA
    assumes DS,DATA
    assumes ES,nothing

;********** SetMouseDoubleSpeed **********
;*	entry : mps = mickey / second speed
;*	* Set mouse double speed threshold
;*	exit : n/a

cPublic	SetMouseDoubleSpeed,<ATOMIC>
    parmW mps
cBegin	SetMouseDoubleSpeed

	StartPublic
	mov	ax,19
	cmp	fMousePresent,ah
	jz	@F

	mov	dx,mps
	int	33H			;* Set double speed
@@:
	StopPublic

cEnd	SetMouseDoubleSpeed



IFDEF MOUSE_SWAP
;********** SwapMouseButton **********
;*	entry : fSwap : TRUE => swap buttons
;*	* Set fSwapButton to state specified
;*	exit : AX = old value of fSwapButton

cPublic	SwapMouseButton,<ATOMIC>
    parmW fSwap
cBegin	SwapMouseButton

	StartPublic
	mov	al,fSwapButton
	xor	ah,ah			;* old value 0 or 1
	mov	cx,fSwap
	jcxz	turn_swap_off
	mov	fSwapButton,1
	jmp	short swap_end
turn_swap_off:
	mov	fSwapButton,0
swap_end:
	StopPublic

cEnd	SwapMouseButton
ENDIF ;MOUSE_SWAP


sEnd	MOUSE
ENDIF ;MOUSE_EXTRAS 


;----------------------------------------------------------------------------

;*	* Mouse Interrupt (in FIXED KERNEL segment)


externFP	MouseMessage			;* from event.c

sBegin	KERNEL
    assumes CS,KERNEL
    assumes SS,NOTHING
    assumes DS,NOTHING

;********** MouseInterrupt **********
;* MOUSE USER DEFINED SUB-ROUTINE (called from mouse driver)
;*	entry : AX = condition mask
;*		BX = button state
;*		CX = horiz. coordinate
;*		DX = vert. coordinate
;*	* Process a mouse event
;*	* If necessary perform mouse lock out
;*	exit : n/a

cProc	MouseInterrupt,<FAR, ATOMIC>,<DS,ES>
cBegin	MouseInterrupt

	CLI			;* DISABLE INTERRUPTS TO AVOID RECURSION
; Establish ds addressing
	push	ax
	mov	ax,SEG dataBase
	mov	ds,ax
    assumes ds,DATA

; Save Button State
	mov	sstMouse,bx

; convert mouse location from pixel to character based.
	cCall	SetMousePixelPos

	pop	ax
; Save interrupt conditions
	mov	dx,ax

IFDEF MOUSE_SWAP
;*	* First see if buttons reversed
	test	fSwapButton,0ffh
	jz	dont_swap_buttons

;*	* Swap Mouse buttons for interrupt state
	Assert	<LEFT_BUTTON_DOWN EQ 2>
	Assert	<LEFT_BUTTON_UP	EQ 4>
	Assert	<RIGHT_BUTTON_DOWN EQ 8>
	Assert	<RIGHT_BUTTON_UP EQ 10H>

	mov	ah,al				;* copy in AH
	and	dl,11100001B			;* clear 4 bits in question
	shl	al,1
	shl	al,1
	and	al,011000B			;* map left to right
	shr	ah,1
	shr	ah,1
	and	ah,000110B			;* map right to left
	or	al,ah
	or	dl,al				;* put back together

;*	* Swap sstMouse (ls2bits)
	mov	ax,sstMouse
	mov	bl,al
	mov	bh,al
	and	al,11111100B			;* clear ls2b
	shr	bl,1
	shl	bh,1
	or	bl,bh
	and	bl,11B
	or	al,bl
	mov	sstMouse,ax

;*	* DX = mouse state
dont_swap_buttons:
ENDIF ;MOUSE_SWAP

; Post Messages according to the events

	mov	al,dl			;* test in AL shorter

MouseMove?:
	test	al,MOUSE_MOVE
	jz	LeftButtonDown?

	mov	ax,WM_MOUSEMOVE
	cCall	DoMouseMessage

LeftButtonDown?:
	test	al,LEFT_BUTTON_DOWN
	jz	LeftButtonUp?

	mov	ax,WM_LBUTTONDOWN
	cCall	DoMouseMessage

LeftButtonUp?:
	test	al,LEFT_BUTTON_UP
	jz	RightButtonDown?

	mov	ax,WM_LBUTTONUP
	cCall	DoMouseMessage

RightButtonDown?:
	test	al,RIGHT_BUTTON_DOWN
	jz	RightButtonUp?

	mov	ax,WM_RBUTTONDOWN
	cCall	DoMouseMessage

RightButtonUp?:
	test	al,RIGHT_BUTTON_UP
	jz	done_messages

	mov	ax,WM_RBUTTONUP
	cCall	DoMouseMessage

done_messages:
;*	* Check for mouse lockout
;*	* If ABS(ayDrawing - ayMouse{new}) = FE,FF,0,1,2 then kill mouse
	mov	al,ayMouse
	sub	al,ayDrawing
	add	al,2				;* 0, 1, 2, 3, 4 => kill it
	cmp	al,5
	jae	lockout_done			;* no conflict
do_lockout:
;*	* drawing conflict, turn mouse of if not already turned off
	test	fMouseOn,0ffh
	jz	lockout_done			;* already off
	test	fMouseLockedOut,0ffh
	jnz	lockout_done			;* already locked out
	mov	ax,2
	int	33H				;* turn off mouse
	mov	fMouseOn,0			;* mouse is off
	mov	fMouseLockedOut,0ffh		;* set locked out flag

lockout_done:
	STI			;* RE-ENABLE INTERRUPTS

cEnd	MouseInterrupt



;********** DoMouseMessage **********
;*	entry : ax = Message type
;*		dx = mouse mask
;*	* Call MouseMessage with parameter
;*	exit : dx retained
;*		al restored with dl

cProc	DoMouseMessage,<NEAR,ATOMIC>	;* all code FIXED !
cBegin	DoMouseMessage
	push	dx
	cCall	MouseMessage,<ax>
	pop	dx
	mov	al,dl
cEnd	DoMouseMessage

;*****************************************************************************


;********** SetMousePixelPos **********
;*	entry:	cx = xMouse (pixels)
;*		dx = yMouse (pixels)
;*	* convert to character coords and store in (axMouse, ayMouse)

cProc	SetMousePixelPos, <NEAR, ATOMIC>
cBegin	SetMousePixelPos

IFDEF LATER
	-- characters other than 8 pixels wide (graphics modes)
ENDIF
	mov	ax,cx
	mov	cl,3				;* 8 pixels wide
	shr	ax,cl
	mov	axMouse,al

	mov	ax,dx				;* vertical
	mov	cl,instCur.inftInst.dyCharInft
	or	cl,cl
	jnz	@F
	mov	cl,8				;* default 8 pixels high
@@:
	div	cl
	mov	ayMouse,al

cEnd	SetMousePixelPos

sEnd	KERNEL

	END
