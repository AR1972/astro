;*
;*	COW : Character Oriented Windows
;*
;*	ktimer5.asm : DOS 5 kernel timer (for ticks)

	include kernel.inc

IFNDEF DOS5
	.error	-- only DOS 5
ENDIF


sBegin	DATA
    assumes DS,DGROUP

externDP pwndAlarm				;* != NULL if alarm set
externD  semaMessage				;* message semaphore

labelD dmsKick					;* time to sleep
staticW	OFF_dmsKick,-1
staticW	SEG_dmsKick,-1

sEnd	DATA

sBegin	BSS
    assumes DS,DGROUP

staticD semaTimer, 0				;* timer semaphore
staticW	idThread, 0				;* id of timer thread
staticB	fTimerLive, 0				;* TRUE => timer alive


;*	* stack
	DB	64 DUP (?)
labelB	wStackMax



sEnd	BSS

;*****************************************************************************

;*	* DOS 5 calls

externFP  <DosCreateThread, DosSemClear, DosSemSetWait, DosSemClear, DosExit>


;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes SS,NOTHING
    assumes DS,NOTHING	;* interrupt handler may be called from anywhere

;********* TimerThread **********
;*	entry : semaEvent and semaTimer valid
;*	* THREAD for processing timer
;*	exit : n/a (dies when killed)

labelFP	TimerThread
    assumes DS,DATA
;*	* when timer semaphore gets cleared, clear the message semaphore
;*	* (this thread is necessary since we want 1 semaphore for all
;*	*  input events).

	test	fTimerLive,-1
	jz	TimerDies

	PushArg	<ds, <dataOffset semaTimer>>	;* hsemaTimer
	PushArg	<SEG_dmsKick, OFF_dmsKick>	;* wait time
	cCall	DosSemSetWait
;*	* timer has elapsed, clear message semaphore
	PushArg	<ds, <dataOffset semaMessage>>
	cCall	DosSemClear
	jmp	TimerThread			;* forever

TimerDies:
	xor	ax,ax
	cCall	DosExit, <ax, ax>		;* kill this thread
;*	* NOTREACHED



;********** KickTimer **********
;*	entry : dms = time in ms to kick timer
;*	*
cPrivate KickTimer, <>
    parmD dms
cBegin	KickTimer

	mov	ax,OFF_dms
	mov	dx,SEG_dms
	mov	OFF_dmsKick,ax
	mov	SEG_dmsKick,dx
;*	* now kick the timer to install the new time
	PushArg	<ds, <dataOffset semaTimer>>	;* hsemaTimer
	cCall	DosSemClear

cEnd	KickTimer


sEnd	KERNEL

;*****************************************************************************

sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** InitSysTimer **********
;*	entry : n/a
;*	* Called to initialize the timer routine
;*	exit : ax != 0 => ok

cProc	FInitSysTimer,<FAR,PUBLIC,ATOMIC>
cBegin	FInitSysTimer

;*	* create the timer thread
	mov	ax,-1
	mov	OFF_dmsKick,ax
	mov	SEG_dmsKick,ax		;* longest time to sleep
	mov	fTimerLive,al

IFNDEF COW_SWAPPED
	mov	dx,cs
ELSE
	mov	dx,SEG kernelBase
ENDIF
	mov	ax,kernelOffset TimerThread
	PushArg	<dx, ax>
	PushArg	<ds, <dataOffset idThread>>
	PushArg	<ss, <dataOffset wStackMax>>
	cCall	DosCreateThread
	or	ax,ax
	jnz	init_timer_error

	ExitTrueUnless init_timer_error

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

;*	* kill thread
	mov	fTimerLive,0
;*	* kick the thread (timer semaphore) so the thread dies
	PushArg	<ds, <dataOffset semaTimer>>	;* hsemaTimer
	cCall	DosSemClear

cEnd	EndSysTimer

sEnd	EXIT

;*****************************************************************************

	END
