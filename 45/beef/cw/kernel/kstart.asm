;*
;*	COW : Character Oriented Windows
;*
;*	kstart.asm : extra kernel initialization / exit procedures

	include kernel.inc
	include galloc.inc		;* for hi_hexpand


;----------------------------------------------------------------------------

sBegin	BSS
    assumes DS,DGROUP

externB <iexeCur> 		;* set to 255 when closing EXE file

externW <psLom> 		;* ps of LOM data initialized by loader

IFDEF	STARTUP_SAVE_DIR
;* szDirOrig : x:\dir....
staticB szDirOrig,<(3+64) DUP (?)>		;* original directory
ENDIF	;STARTUP_SAVE_DIR

sEnd	BSS

;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DGROUP

externDP    <pwndFocus> 			;* for restoring focus

IFDEF DEBUG
externB     <ayMac>
ENDIF ;*DEBUG

sEnd	DATA

;----------------------------------------------------------------------------


externFP	<FInitScreen>

externFPublic	<EnableKeyboard>
externFP	<FlushKeyEvents>

externFPublic	<FEnableMouse>
externFPublic	<EndScreen>				;* (private)

externFP    <FInitSysTimer>			;* from ktimer.asm
externFP    <EndSysTimer>			;* from ktimer.asm
externFP    <FInitMouse>			;* from mouse.asm
externFP    <EndMouse>				;* from mouse.asm
externFP    <InitInt24Etc>			;* from int24.asm

IFDEF DUAL
externFP	<ExitKernelOSSPEC>
ENDIF ;DUAL

;----------------------------------------------------------------------------


sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** BackToCow **********
;*	* after a LeaveCow -- re-establish COW environment

cPublic BackToCow, <ATOMIC>
parmW	fRestoreScreenMode

cBegin	BackToCow

	cCall	FInitSysTimer
	cCall	InitInt24Etc			; Get back on int24

	mov	cx,fRestoreScreenMode
	jcxz	@F

	xor	ax,ax
	cCall	FInitScreen,<ax>		;* bring screen back

	cCall	FEnableMouse,<sp>

;;;	cCall	SetFocus, <pwndFocus>	; ??? Is this needed ???

@@:
	cCall	EnableKeyboard, <sp>

cEnd	BackToCow


sEnd	INIT

;*****************************************************************************

sBegin	EXIT
    assumes CS,EXIT
    assumes SS,DATA
    assumes DS,DATA

externNP    <EndInt24>				;* from int24.asm


;********** LeaveCow **********
;*	entry : fClearScreen => should we clear the screen
;*	* leave COW for a short while (running subshell)
;*	exit : n/a (COW disabled)

cProc	LeaveCow, <FAR,PUBLIC,ATOMIC>
    parmW fClearScreen
cBegin	LeaveCow

;*	* Unhook int24
	cCall	EndInt24
;*	* end keyboard before unhooking system timer
	xor	ax,ax
	cCall	EnableKeyboard, <ax>
	cCall	FlushKeyEvents

;*	* end system timers
	cCall	EndSysTimer
;*	* end mouse (NOTE:FInitMouse may not have been called)
	cCall	EndMouse
;*	* close the swap file (to be nice)
	mov	ax,-1
	xchg	iexeCur,al			;* set iexeCur = 255, get old
	cmp	al,ah
	je	no_file_open			;* iexeCur == 255
	mov	es,psLom
	mov	bx,es:[fdExeLom]
	AssertNE bx,-1
	mov	ah,3eh
	int	21h				;* close file
no_file_open:

	cCall	EndScreen,<fClearScreen>

cEnd	LeaveCow


;********** ExitKernel alias **********

LabelFP <PUBLIC, cw_exit>			;* special label for Binding
IFNDEF	FOR_QC				;* qc has a routine named _exit
LabelFP <PUBLIC, _exit>
ENDIF	; !FOR_QC
IFDEF DUAL
	jmp	ExitKernelOSSPEC
ELSE ;!DUAL
;*	* fall through to ExitKernel
ENDIF ;!DUAL

;********** ExitKernel / exit (ex) **********
;*	entry : ex = exit code
;*	* Exit from COW system (everything should already be unhooked)
;*	exit : never return to caller - return to system

cPublic ExitKernel,<ATOMIC>
   parmB ex
cBegin	ExitKernel

;*	* restore starting drive / directory
IFDEF	STARTUP_SAVE_DIR
	mov	bx,dataOffset szDirOrig
	mov	dl,[bx]
	sub	dl,'A'				;* A => 0.
	mov	ah,0eH
	int	21h				;* set drive

	mov	dx,bx				;* full drive / path
	mov	ah,3Bh
	int	21h				;* set directory
ENDIF	;STARTUP_SAVE_DIR

;*	* exit to DOS
	mov	al,ex
	mov	ah,4ch				;* terminate a process
	int	21h
IFDEF DUAL
done_exit:
ELSE ;!DUAL
;*	*NOTREACHED*
ENDIF ;!DUAL

cEnd	ExitKernel


sEnd	EXIT

;*****************************************************************************

	END
