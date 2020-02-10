;*
;*	COW : Character Oriented Windows
;*
;*	startup.asm : kernel init for DOS 3/5 non-swapped

	include kernel.inc
	include	indrv.inc
	include inscr.inc		;* for inst

;----------------------------------------------------------------------------

IFDEF DUAL

externFP	<FInitScreen>
externFP	<DosExit>

ENDIF ;DUAL

;----------------------------------------------------------------------------

sBegin	DATA

IFDEF	DOS5

ifdef	Debug
staticB		fVersionPrinted,0
externB		<szVerCow, szVerCowMax>
endif	; Debug

externB		inosDrv			;* INOS info (from loaddrv.asm)

ENDIF	; DOS5

externDP	<pwndFocus>		;* for restoring focus

sEnd	DATA

;----------------------------------------------------------------------------

externFPublic	<EnableKeyboard>
externFP	<FlushKeyEvents>

externFPublic	<FEnableMouse>
externFPublic	<EndScreen>				;* (private)

externFP	<FInitMouse>				;* from mouse.asm
externFP	<EndMouse>				;* from mouse.asm

externFPublic	<FInitScreenInternal>			;* (private)

IFDEF	DOS5

externFP	<DosGetInfoSeg>
externFP	<FInitSysTimer>				;* from ktimer5.asm
externFP	<EndSysTimer>				;* from ktimer5.asm

ifdef	Debug
externFP	<VioWrtTty>
endif	; Debug

ENDIF	; DOS5

;----------------------------------------------------------------------------

sBegin	INIT
    assumes CS,INIT
    assumes SS,DATA
    assumes DS,DATA


;********** FInitCow **********
;*	entry : n/a
;*	* hook in all Cow things
;*	exit : AX = TRUE (1) if ok, FALSE (0) if error

cPublic	FInitCow,<FAR,ATOMIC>
cBegin	FInitCow

IFDEF	DOS5
;*	* for DOS5, get Global info (before any possible messages)
	mov	bx,dataOffset inosDrv.sdGlisInos
	PushArg	<ds, bx>			;* lpsdGlis
	mov	ax,dataOffset inosDrv.sdLoisInos
	PushArg	<ds, ax>			;* lpsdLois
	cCall	DosGetInfoSeg
	or	ax,ax
	jnz	init_error
ENDIF	;DOS5

IFDEF	DOS5
ifdef	Debug					; Print COW Version stamp.
	TEST	fVersionPrinted,0FFH
	JNZ	skip_message
	MOV	fVersionPrinted,1

	mov	bx,dataOffset szVerCow
	mov	cx,dataOffset szVerCowMax
	sub	cx,bx				;* cx = cch + 1
	xor	ax, ax
	cCall	VioWrtTty,<dsbx, cx, ax>
	or	ax,ax
	jnz	init_error
;;;;;	int	3			;* start in the debugger !!!

skip_message:
endif	; Debug
ENDIF	;DOS5

IFDEF DOS5
	cCall	FInitSysTimer
	or	ax,ax
	jz	init_error
ENDIF ;DOS5

	ExitTrueUnless init_error

cEnd	FInitCow

;----------------------------------------------------------------------------

IFDEF	DOS5
ifdef	Debug

cProc	Trap,<PUBLIC, FAR, ATOMIC>
cBegin
	BREAKPOINT
cEnd

endif	; Debug
ENDIF	;DOS5

IFNDEF DUAL ;!DUAL
;********** FInitScreen **********
;* COW PUBLIC ENTRY
;*	entry: pinst => INST structure (NULL => re-init previous mode)
;*	* Initialize screen & mouse
;*	exit: AX = 1 if ok, 0 if error

cProc	FInitScreen,<PUBLIC, FAR, ATOMIC>	;* all calls atomic
    parmW pinst
cBegin	FInitScreen

	cCall	FInitScreenInternal, <pinst>
	or	ax,ax
	jz	@F
	cCall	FInitMouse
	mov	bx,pinst
	test	[bx].finstInst,finstDisableMouse
	jz	@F
	xor	ax,ax
	cCall	FEnableMouse,<ax>		;* turn mouse off
	mov	ax,sp
@@:

cEnd	FInitScreen
ENDIF ;!DUAL

;********** BackToCow **********
;*	* after a LeaveCow -- re-establish COW environment

cPublic BackToCow, <ATOMIC>
parmW	fRestoreScreenMode

cBegin	BackToCow

	cCall	FInitCow
	AssertNE	ax,0

	mov	cx,fRestoreScreenMode
	jcxz	@F

	xor	ax,ax
	cCall	FInitScreen,<ax>		;* bring screen back

	cCall	FEnableMouse,<sp>

;;;	cCall	SetFocus, <pwndFocus>	; ??? Is this needed ???
@@:
cEnd	BackToCow

sEnd	INIT


;*****************************************************************************

sBegin	EXIT
    assumes CS,EXIT
    assumes SS,DATA
    assumes DS,DATA



IFNDEF DUAL ;!DUAL
;********** EndCow **********
;*	entry : n/a
;*	* Exit from COW system (unhook everything)
;*	exit : n/a

labelFP	<PUBLIC, EndCow>
ENDIF ;!DUAL


;********** LeaveCow **********
;*	entry : fClearScreen => should we clear the screen
;*	* leave COW, like EndCow but perhaps clear the screen
;*	exit : n/a

cPublic	LeaveCow,<ATOMIC>
    parmW fClearScreen
cBegin	LeaveCow

	xor	ax,ax
	cCall	EnableKeyboard, <ax>
	cCall	FlushKeyEvents
IFDEF DOS5
	cCall	EndSysTimer
ENDIF ;DOS5
	cCall	EndMouse
	cCall	EndScreen,<fClearScreen>

cEnd	LeaveCow


IFDEF DUAL
;********** ExitKernel / exit (ex) **********
;*	entry : ex = exit code
;*	* Exit from COW system (everything should already be unhooked)
;*	exit : never return to caller - return to system

cPublic ExitKernel,<ATOMIC>
   parmB ex
cBegin	ExitKernel
	cCall	DosExit,<1,ex>
cEnd	ExitKernel
ENDIF ;DUAL


sEnd	EXIT


;*****************************************************************************

	END
