;*
;*	COW : Character Oriented Windows
;*
;*	kernel : kernel init for stand alone kernel
;REVIEW:	this file is massively out of date !!

	include kernel.inc


IFDEF DEBUG
neLomOffset	=	OFFSET neLom
	PUBLIC	neLomOffset	;* address for Pcode Debugger
ENDIF ;DEBUG


sBegin	BSS
    assumes DS,DGROUP

externB	iexeCur			;* set to 255 when closing EXE file

globalW	psLom,0			;* ps of LOM data initialized by loader

globalW hGlobalHeap,0	    ; Handle to master object
globalW pGlobalHeap,0	    ; Current physical address

;;;;;globalB	rgbKernelBuff,<cbKernelBuff DUP (?)>	;* General buffer

sEnd	BSS


sBegin	DATA
    assumes DS,DGROUP

externW     <pStackTop, pStackMin, pStackBot>

sEnd	DATA



;*	* Far entry provided by application
externFPublic <_main>

;*	* Global free
externFP    <GlobalFree>

externFP    <FInitSysTimer>			;* from ktimer.asm
externFP    <EndSysTimer>			;* from ktimer.asm

sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP

externNP    <InitThunk>				;* from ldthunk.asm

;********** BeginKernel **********
;*	entry : es = psLom
;*		ax = handle of LOADER
;*		si = start of DDS local heap
;*		di = end of DDS local heap
;*		sp = Bottom of stack
;*	* Called by loader to start up the kernel
;*	exit : NEVER EXITED ! (see ExitKernel()).
cProc	BeginKernel,<FAR,PUBLIC,ATOMIC>
cBegin	BeginKernel

	push	ax				;* save hmemLoader

	mov	psLom,es			;* save all important psLom
;*	* Copy very important variables from LOM into DS
	mov	ax,es:[pGlobalHeapLom]
	mov	pGlobalHeap,ax
	mov	ax,es:[hGlobalHeapLom]
	mov	hGlobalHeap,ax

;*	* Initialize DDS for stack limits
	mov	ax,sp
	mov	pStackBot,ax
	mov	pStackMin,ax
	sub	ax,es:[neLom.ne_stack]		;* less stack size
	mov	pStackTop,ax

;*	* Initialize entry table for Thunk control
	cCall	InitThunk

;*	* Initialize the system timer
	cCall	FInitSysTimer

;*	* Free the loader
	cCall	GlobalFree			;* handle already on the stack
IFDEF DEBUG
	or	ax,ax
	jz	ok_free_loader
	cCall	CowAssertFailed
	DB	"freeing loader$"
ok_free_loader:
ENDIF ;DEBUG

;*	* call the Application's main entry point
	xor	ax,ax			;* no parameters
	cCall	_main,<ax,ax>

cEnd	BeginKernel

;*******************************************************

sEnd	INIT

;*****************************************************************************

sBegin	EXIT
    assumes CS,EXIT
    assumes SS,DATA
    assumes DS,DATA



;********** ExitKernel alias **********

	PUBLIC _exit
_exit:
;*	* fall through to ExitKernel

;********** ExitKernel / exit (ex) **********
;*	entry : ex = exit code
;*	* Exit from COW system (unhook everything)
;*	exit : never return to caller - return to system

cProc	ExitKernel,<FAR,PUBLIC,ATOMIC>
   parmB ex
cBegin	ExitKernel

;*	* unhook system timers
	cCall	EndSysTimer
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
	mov	iexeCur,255			;* no file open
no_file_open:

;*	* exit to DOS
	mov	al,ex
	mov	ah,4ch				;* terminate a process
	int	21h
;*	*NOTREACHED*

cEnd	ExitKernel

sEnd	EXIT

;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL

;*	* stub for prompting for swap disk (never called)
cProc	PromptSwapDisk, <NEAR, PUBLIC, ATOMIC>
cBegin
	mov	ax,999				;* never called !!
	cCall	ExitKernel,<ax>
cEnd

sEnd	KERNEL


;*****************************************************************************

	END	BeginKernel

