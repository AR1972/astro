;*
;*	COW : Character Oriented Windows
;*
;*	int24.asm : DOS Hard error control

	title	int24.asm : DOS Hard error control

	include kernel.inc
	include	vkey.inc

IFDEF DEBPUB
ifndef INT24_ALTERNATE
	PUBLIC	HandlerInt24
endif
ENDIF ;DEBPUB

;----------------------------------------------------------------------------

sBegin	BSS

    assumes DS,DGROUP

globalB	fSingleFloppy,0		; If one floppy, set to True
globalB	drivePhantom,0		;                set to 2 (drive code for B)
globalW	chDrivePhantom,0	;                set to "b:" 

globalB fInt24Error,0

globalB	fGrabInt05,0		; Re-route if requested.

labelD	pfnHdlrInt05		; old vector
globalW	OFF_pfnHdlrInt05,0
globalW	SEG_pfnHdlrInt05,0

staticB	bCtrlBrkOld,0		; control break state

ifndef INT24_ALTERNATE
labelD	pfnHdlrInt24		; old vector
staticW	OFF_pfnHdlrInt24,0
staticW	SEG_pfnHdlrInt24,0
endif

sEnd	BSS

;----------------------------------------------------------------------------

;externFP	DoSound
externFP	KeyboardMessage

;----------------------------------------------------------------------------

sBegin	INIT

;----------------------------------------------------------------------------
;
;  Hook int24 vector, figure out system specs
;
;	entry : n/a
;	exit : n/a

    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP
    assumes ES,NOTHING

cProc	InitInt24Etc,<FAR,PUBLIC,ATOMIC>
cBegin	InitInt24Etc

ifndef	INT24_ALTERNATE

	mov	ax,3524h			; Get Interrupt Vector 24h
	int	21H
	mov	OFF_pfnHdlrInt24,bx
	mov	SEG_pfnHdlrInt24,es

	push	ds
	mov	ax,SEG kernelBase		;* both in KERNEL segment
	mov	ds,ax
    assumes ds,NOTHING
	mov	dx,kernelOffset HandlerInt24
	mov	ax,2524h			; Set Interrupt Vector 24h
	int	21H
	pop	ds
    assumes ds,DATA

endif	; !INT24_ALTERNATE

;---------------------------------------
;
; Figure out interrupt 05 (^PrtSc) stuff.

	mov	ax,3505h			; Get Interrupt Vector 05h
	int	21h
	mov	SEG_pfnHdlrInt05,es
	mov	OFF_pfnHdlrInt05,bx

	cmp	fGrabInt05,0			; Only reroute if requested.
	je	@F

	push	ds
	mov	ax,SEG kernelBase
	mov	ds,ax
    assumes ds,NOTHING
	mov	dx,kernelOffset HandlerInt05Key
	mov	ax,2505h			; Set Interrupt Vector 05h
	int	21h
	pop	ds
    assumes ds,DATA

@@:

;---------------------------------------
;
; Figure out floppy info.

	mov	fSingleFloppy,0
	mov	drivePhantom,-1		; Guess that neither A nor B are
	mov	chDrivePhantom,-1	;   phantom drives.

	int	11h			; Get BIOS equipment list
	test	al,11000000B
	jnz	lots_o_floppy

	dec	fSingleFloppy

; On a single floppy system, the byte at 0040:0104 is 00 if the floppy is
; currently acting as A:, and 01 if the floppy is currently acting as B:.
;
; This is using a totally undocumented DOS data location; it's the sleaziest
; CW operation that I know of.  For safety's sake (ha!), we check the 
; obscure case of 1, and assume B: is the phantom for !1.
;
; If this breaks in a future version of DOS, I would suggest adding a DOS
; check, and not just punting this stuff altogether.

	xor	ax,ax
	mov	es,ax
    assumes es,NOTHING

	mov	bl,1			; Guess it's currently acting as B:,
	mov	ax,":a"			;   so that A: is the phantom drive.
	cmp	es:[0504h],bl
	jz	@F
	inc	ax			; Guessed wrong: B: is the phantom
	inc	bx			;   drive.
@@:
	mov	chDrivePhantom,ax	; "a:" if A is phantom, "b:" if B is.
	mov	drivePhantom,bl		;   1  if A is phantom,   2  if B is.

; We should do something to reselect to avoid phantom drives (???)

lots_o_floppy:

;---------------------------------------
;
; Get state of control-break, save it and turn off

	mov	ax,3300h			;* request current state
	int	21h
	mov	bCtrlBrkOld,dl			;* save old state
	xor	dl,dl				;* state off !!!
	mov	ax,3301h			;* set state
	int	21h

cEnd	InitInt24Etc


sEnd	INIT

;----------------------------------------------------------------------------

sBegin	EXIT

    assumes CS,EXIT
    assumes SS,DATA
    assumes DS,DATA
    assumes ES,NOTHING


;********** EndInt24 **********
;*	entry : n/a
;*	* Unhook the timer interrupt
;*	exit : n/a

cProc	EndInt24,<NEAR,PUBLIC,ATOMIC>
cBegin	EndInt24

ifndef	INT24_ALTERNATE

	push	ds
	lds	dx,pfnHdlrInt24
    assumes ds,NOTHING
	mov	ax,2524H			; Set Interrupt Vector 24h
	int	21H
	pop	ds
    assumes ds,DATA

endif	; !INT24_ALTERNATE

	push	ds
	lds	dx,pfnHdlrInt05
    assumes ds,NOTHING
	mov	ax,2505H			; Set Interrupt Vector 05h
	int	21H
	pop	ds
    assumes ds,DATA

	mov	dl,bCtrlBrkOld			; restore old ctrl-break 
	mov	ax,3301h			;   state
	int	21h

cEnd	EndInt24

sEnd	EXIT

;----------------------------------------------------------------------------

sBegin	KERNEL

    assumes CS,KERNEL
    assumes SS,NOTHING
    assumes DS,NOTHING	; interrupt handler may be called from anywhere
    assumes ES,NOTHING

ifndef	INT24_ALTERNATE

;---------------------------------------
;
; Interrupt 24 (DOS Hard Error) handler
;

cProc	HandlerInt24,<FAR,ATOMIC>
cBegin	nogen	; HandlerInt24

    assumes DS,NOTHING
	push	ds
	mov	ax,SEG dataBase
	mov	ds,ax
    assumes ds,DATA
	mov	al,3			;* return error
	mov	fInt24Error,al
	pop	ds
    assumes DS,NOTHING
	iret

cEnd	nogen	; HandlerInt24
endif

;---------------------------------------
;
; Interrupt 05 (^PrtSc) handler to generate a keystroke (for CBT)
;

cProc	HandlerInt05Key,<FAR,ATOMIC>
cBegin	nogen	; HandlerInt05Key

    assumes DS,NOTHING

	mov	ax,VK_PRINT
	xor	cx,cx
	cCall	KeyboardMessage,<ax,ax,cx,cx>

	iret

cEnd	nogen	; HandlerInt05Key

;---------------------------------------
;
; Interrupt 05 (^PrtSc) handler to disable the functionality
;

cProc	HandlerInt05Dead,<FAR,PUBLIC,ATOMIC>
cBegin	nogen	; HandlerInt05Dead

    assumes DS,NOTHING

	xor	ax,ax
;	cCall	DoSound,<ax>
	iret

cEnd	nogen	; HandlerInt05Dead

sEnd	KERNEL

;----------------------------------------------------------------------------

	END
