;*
;*	COW : Character Oriented Windows
;*
;*	kerninit : kernel initialization procedure / kernel exit procedure

	include kernel.inc
	include galloc.inc		;* for hi_hexpand
	include indrv.inc		;* for inos
	include inscr.inc		;* for insj
	include inkbd.inc		;* for inkj
	include insyd.inc		;* for inyj


neLomOffset	=	neLom
	PUBLIC	neLomOffset	;* address for Pcode Debugger

;----------------------------------------------------------------------------

IFDEF DUAL
createSeg   CWOS2,CWOS2,BYTE,PUBLIC,CODE
createSeg   OSSPEC_TEXT,OSSPEC_TEXT,BYTE,PUBLIC,CODE

sBegin	DATA

externW	<inosDrv,insj,inkj,inyj>
externW	<insjOS2,inkjOS2,inyjOS2>
externW	<_eData>

globalW	fProtectMode,?

sEnd	DATA

sBegin	CWOS2

externW	    <inosDrvOS2>

sEnd	CWOS2

sBegin	OSSPEC_TEXT

externW	    <rgfnosspec,rgfnosspec3>

sEnd	OSSPEC_TEXT

sBegin	DRV

externW	pinos

sEnd	DRV
ENDIF ;DUAL

createSeg KERNEL_BUFF,KERNEL_BUFF,WORD,PUBLIC,BEFORE_BSS,DGROUP

;----------------------------------------------------------------------------

sBegin	KERNEL_BUFF
    assumes DS,DGROUP

globalB rgbKernelBuff,<2 DUP (?)>
;* Kernel buffer placed before BSS.
;* see KNULL.ASM for special kludge

sEnd	KERNEL_BUFF

;----------------------------------------------------------------------------

sBegin	BSS
    assumes DS,DGROUP

externB iexeCur 		;* set to 255 when closing EXE file

globalW psLom,0 		;* ps of LOM data initialized by loader

globalW hGlobalHeap,0		; Handle to master object
globalW pGlobalHeap,0		; Current physical address

globalB	fShellPresent,0 	;* TRUE if DOS SHELL present

IFDEF	STARTUP_SAVE_DIR
;* szDirOrig : x:\dir....
staticB szDirOrig,<(3+64) DUP (?)>		;* original directory
ENDIF	;STARTUP_SAVE_DIR

sEnd	BSS

;----------------------------------------------------------------------------

sBegin	DATA
    assumes DS,DGROUP

externW     <pStackTop, pStackMin, pStackBot>
externDP    <pwndFocus> 			;* for restoring focus

externW     <fColorModeOld>			;* last color mode

IFDEF DEBUG
externB     <sdVerCow>
externB     <ayMac>
ENDIF ;*DEBUG

sEnd	DATA

;----------------------------------------------------------------------------

;*	* Far entry provided by application
externFPublic	<WinMain>
externFPublic	<GlobalFree>
externFP	<LeaveCow>

IFDEF DUAL
externFP	<FInitCow>
externFP	<ExitKernel>
ENDIF ;DUAL

;*	* screen init - reinit must be fixed !!
externFPublic	<FInitScreenInternal>			;* (private)
externFP	<FInitMouse>			;* from mouse.asm
externFPublic	<FEnableMouse>
externFP	<FInitSysTimer>			;* from ktimer.asm
externFP	<InitThunk> 			;* from ldthunk.asm
externFP	<InitInt24Etc>			;* from int24.asm
externFP	<ReroutePrtSc>

;----------------------------------------------------------------------------

sBegin	KERNEL
externNP  <ghexpand>			; Near procedure in Kernel segment.
sEnd	KERNEL

;----------------------------------------------------------------------------

sBegin	INIT

IFDEF	KEEP_SYMDEB_CODE
externNP    <DebugInit> 			;* from lddebug.asm
ENDIF	; KEEP_SYMDEB_CODE

ifdef	CBox
externNP	<TouchUpTables>
endif	; CBox

;********** BeginKernel **********
;*	entry : es = psLom
;*		ax = handle of LOADER
;*		si = start of DDS local heap
;*		di = end of DDS local heap
;*		sp = Bottom of stack
;*	* rgbKernel filled with rlbMove (relocations for main swap file).
;*	* Called by loader to start up the kernel
;*	exit : NEVER EXITED ! (see ExitKernel()).

    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP
    assumes ES,NOTHING

IFDEF DUAL
os2_startup:

;*	entry:	es == 0
;*		ds == DGROUP
;*		ax == env seg
;*		bx == offset of command line in env seg
;*		cx == cbDGROUP
;*		bp == 0

	mov	pStackBot,sp

	push	ax				;* push parms for WinMain
	push	bx

;*	* Initialize OS specific tables

;*	* NOTE:	No OS specific calls can be made before this operation!!

	lea	ax,_eData
	mov	pStackTop,ax

	push	ds
	pop	es				;* ES => DGROUP
	mov	ax,CWOS2
	mov	ds,ax				;* DS => CWOS2
	assumes	DS,CWOS2
	assumes	ES,DGROUP
;*	* Initialize the OS/2 form of inosDrv
	mov	di,dataOffset inosDrv
	mov	si,cwos2Offset inosDrvOS2
	mov	cx,(cbInosMin + 1) / 2
	Assert	<(cbInosMin + 1) / 2 EQ cbInosMin / 2>
	rep	movsw

	push	es
	pop	ds
	assumes	DS,DGROUP
	assumes	ES,nothing
;*	* insj
	mov	di,dataOffset insj
	mov	si,dataOffset insjOS2
	mov	cx,(cbInsjMin + 1) / 2
	Assert	<(cbInsjMin + 1) / 2 EQ cbInsjMin / 2>
	rep	movsw
;*	* inkj
	mov	di,dataOffset inkj
	mov	si,dataOffset inkjOS2
	mov	cx,(cbInkjMin + 1) / 2
	Assert	<(cbInkjMin + 1) / 2 EQ cbInkjMin / 2>
	rep	movsw
;*	* inyj
	mov	di,dataOffset inyj
	mov	si,dataOffset inyjOS2
	mov	cx,(cbInyjMin + 1) / 2
	Assert	<(cbInyjMin + 1) / 2 EQ cbInyjMin / 2>
	rep	movsw

;*	* OS calls now available

	mov	fProtectMode,sp			;* we are in protect mode

;	cCall	FInitCow
;	or	ax,ax
;	jz	init_err

	cCall	WinMain	;* <ax,bx> already pushed

IFDEF DEBUG
	cCall	CowAssertFailed
	DB	"Return from WinMain$"
ENDIF ;DEBUG

init_err:
	mov	ax,2
	cCall	ExitKernel
ENDIF ;DUAL

LabelFP <PUBLIC, cw_init>			;* special label for Binding
cProc	BeginKernel,<FAR,PUBLIC,ATOMIC>
cBegin	BeginKernel

	push	ax				;* save hmemLoader
	mov	psLom,es			;* save all important psLom
IFDEF DUAL
;*	* check for OS/2 startup
	mov	cx,es
	jcxz	os2_startup

ENDIF ;DUAL

IFDEF DEBUG
IFDEF NOT_WANTED
;*	Print COW version stamp
	mov	dx,dataOffset sdVerCow
	mov	ah,9
	int	21h
ENDIF ;NOT_WANTED
ENDIF ;DEBUG

IFDEF DUAL

;*	* we are in real mode!
	xor	cx,cx
	mov	fProtectMode,cx

;*	* Initialize OS specific tables

;*	* NOTE:	No OS specific calls can be made before this operation!!

	push	ds
	push	es
	push	si
	push	di
;*	* Set OS specific jump table to DOS 3 form
	mov	ax,OSSPEC_TEXT
	mov	ds,ax					;* DS,ES => OSSPEC_TEXT
	mov	es,ax
	assumes	DS,OSSPEC_TEXT
	assumes	ES,OSSPEC_TEXT
	mov	si,osspec_textOffset rgfnosspec3
	mov	di,osspec_textOffset rgfnosspec
	mov	cx,si
	sub	cx,di
	inc	cx
	shr	cx,1
	rep	movsw
;*	* Set pinos to 0,  indicating DOS 3
	mov	ax,DRV_FIXED
	mov	ds,ax					;* DS => DRV
	assumes	DS,DRV_FIXED
	xor	cx,cx
	mov	ds:[pinos],cx
	pop	di
	pop	si
	pop	es
	pop	ds
	assumes	DS,DGROUP
	assumes	ES,nothing

;*	* OS calls now available

ENDIF ;DUAL
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

;*	* Set global handle expansion proc
	mov	es,pGlobalHeap
	mov	es:[hi_hexpand],kernelOffset ghexpand

IFDEF KEEP_SYMDEB_CODE
;*	* Initialize Debugger
	cCall	DebugInit
ENDIF ;KEEP_SYMDEB_CODE

;*	* Initialize entry table for Thunk control
	cCall	InitThunk

;*	* Initialize the system timer
	cCall	FInitSysTimer

;*	* Initialize INT24 hook
	cCall	InitInt24Etc

;*	* Free the loader
	cCall	GlobalFree			;* handle already on the stack
IFDEF DEBUG
	or	ax,ax
	jz	ok_free_loader
	cCall	CowAssertFailed
	DB	"freeing loader$"
ok_free_loader:
ENDIF ;DEBUG

;*	* save the start-up drive / directory
IFDEF	STARTUP_SAVE_DIR
	mov	si,dataOffset szDirOrig
	mov	ah,19h
	int	21h

	add	al,'A'
	mov	[si],al
	mov	word ptr [si+1], '\:'	;* drive:\
	lea	si,[si+3]		;* after drive

	xor	dl,dl			;* current drive
	mov	ah,47h			;* get directory (current drive)
	int	21h
ENDIF	;STARTUP_SAVE_DIR

;*	* test for shell present
	mov	ah,30h
	int	21h			;* check DOS major version
	cmp	al,3
	jb	ancient_dos
;*	* DOS 3 or higher, we can use the INT 2F multiplex 18H test.
	mov	ax,1800h		;* check shell present
	int	2fh			;* 0 if not present
					;* 80H if stub present
					;* FFH if shell present
	and	al,1			;* TRUE if present
	mov	fShellPresent,al
ancient_dos:

;*	* call the Application's main entry point
	mov	ax,lomOffset szCmdLine		;* address of command line
	cCall	WinMain,<psLom,ax>

IFDEF DEBUG
	cCall	CowAssertFailed
	DB	"Return from WinMain$"
ENDIF ;DEBUG

cEnd	BeginKernel

;----------------------------------------------------------------------------
;
; COW PUBLIC ENTRY
;	entry: pinst => INST structure (NULL => re-init previous mode)
;	* Initialize screen & mouse
;	exit: AX = 1 if ok, 0 if error

    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP
    assumes ES,NOTHING

IFNDEF PROFILE
cProc	FInitScreen,<PUBLIC, FAR, ATOMIC>	;* all calls atomic
ELSE
cProc	FInitScreen,<PUBLIC, FAR>		;* profile needs BP saved
ENDIF ;PROFILE
    parmW pinst					;* mode (pass to internal init)

cBegin	FInitScreen

	StartPublic
	cCall	FInitScreenInternal, <pinst>
	or	ax,ax
	jz	@F
	cCall	FInitMouse			;* ax = success code
	or	ax,ax
	jz	@F

	mov	bx,pinst

ifdef	CBox
	cCall	TouchUpTables
endif	; CBox

	cCall	ReroutePrtSc

	test	[bx].finstInst,finstDisableMouse
	jz	@F
	xor	ax,ax
	cCall	FEnableMouse,<ax>		;* turn mouse off
	mov	ax,sp
@@:
	Save	<ax>
	StopPublic

cEnd	FInitScreen

sEnd	INIT

;----------------------------------------------------------------------------

sBegin	EXIT

;----------------------------------------------------------------------------
;
;  EndCow
;	prepare CW for termination
;
;	entry : fClearScreen => should we clear the screen
;	exit  : n/a (COW disabled)

    assumes CS,EXIT
    assumes SS,DGROUP
    assumes DS,DGROUP
    assumes ES,NOTHING

cPublic EndCow, <ATOMIC>
    parmW fClearScreen
cBegin	EndCow

	cCall	LeaveCow,<fClearScreen>

cEnd	EndCow

sEnd	EXIT

;----------------------------------------------------------------------------

	END	BeginKernel
