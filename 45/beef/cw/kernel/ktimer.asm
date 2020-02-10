;*
;*	COW : Character Oriented Windows
;*
;*	ktimer.asm : kernel timer routine (for LRU sweep only at present).

	include kernel.inc

ctickLruMax	EQU	6			;* 6/18 = 1/3 s (approx.)


IFDEF DEBPUB
	PUBLIC	HandlerTimer
ENDIF ;DEBPUB


sBegin	DATA
    assumes DS,DGROUP

externDP pwndAlarm				;* != NULL if alarm set
externW  fMessage
globalW  wTimer,0

sEnd	DATA

sBegin	BSS
    assumes DS,DGROUP

labelD	pfnHdlrTimer				;* old int8 handler
staticW	OFF_pfnHdlrTimer,?
staticW	SEG_pfnHdlrTimer,?

staticB	ctickLruCur,0

sEnd	BSS

;*	* Entries to initialize
sBegin	KERNEL

;*	* LRU sweep
externNP    <SweepLru>				;* from ldthunk.asm

sEnd	KERNEL


sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** FInitSysTimer **********
;*	entry : n/a
;*	* Called to initialize the timer routine
;*	exit : AX == true

cProc	FInitSysTimer,<FAR,PUBLIC,ATOMIC>
cBegin	FInitSysTimer

	mov	ax,3508H			; Get Interrupt Vector 8H
	int	21H
	mov	OFF_pfnHdlrTimer,bx
	mov	SEG_pfnHdlrTimer,es

;*	* Set new interrupt handler
	push	ds
	mov	ax,SEG kernelBase		;* both in KERNEL segment
	mov	ds,ax
    assumes ds,nothing

	mov	dx,kernelOffset HandlerTimer
	mov	ax,2508H			; Set Interrupt Vector 8
	int	21H
	pop	ds
    assumes ds,DATA
	mov	ax,sp				;* always return true

cEnd	FInitSysTimer


sEnd	INIT

;*****************************************************************************

sBegin	EXIT
    assumes CS,EXIT
    assumes SS,DATA
    assumes DS,DATA


;********** EndSysTimer **********
;*	entry : n/a
;*	* Unhook the timer interrupt
;*	exit : n/a

cProc	EndSysTimer,<FAR,PUBLIC,ATOMIC>
cBegin	EndSysTimer

	push	ds
	lds	dx,pfnHdlrTimer
    assumes ds,NOTHING
	mov	ax,2508H			; Set Interrupt Vector 8H
	int	21H
	pop	ds
    assumes ds,DATA

cEnd	EndSysTimer

sEnd	EXIT

;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes SS,NOTHING
    assumes DS,NOTHING	;* interrupt handler may be called from anywhere

;********* HandlerTimer **********
;* INTERRUPT HANDLER
;*	entry : hardware timer interrupt (every ~1/18 of a second).
cProc	HandlerTimer,<FAR,ATOMIC>
cBegin	nogen;HandlerTimer

	push	ax
	push	ds

	mov	ax,SEG DGROUP
	mov	ds,ax
    assumes ds,DATA

;*	* call the old int8 handler
	pushf				;* make it look like an interrupt
	call	[pfnHdlrTimer]

;*	* if alarm set, set fMessage
	mov	ax,pwndAlarm
	or	fMessage,ax

;*	* see if we must decrement the pCode's timer counter
	cmp	wTimer,0
	jne	DoDecr
DoneDecr:

;*	* see if we must sweep the LRU
	mov	al,ctickLruCur
	inc	al
	mov	ctickLruCur,al		;* update count
	cmp	al,ctickLruMax
	jb	end_sweep
;*	* Sweep the LRU : Enable interrupts first !!!
	mov	ctickLruCur,0		;* start over
	STI
	Save	<bx,cx,dx,es>		;* save trashable registers
	cCall	SweepLru
end_sweep:

	pop	ds
    assumes ds,nothing
	pop	ax			;* restore everything
	iret

cEnd	nogen;HandlerTimer

; --------------------------------------

	assumes ds,DATA
DoDecr:					;* The pCode has set wTimer != 0.
	dec	wTimer			;* It's our job to decrement it once
	jmp	short DoneDecr		;*   per timer tick until back to 0.
	assumes ds,nothing

; --------------------------------------

sEnd	KERNEL

;*****************************************************************************

	END

