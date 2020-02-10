;*
;*	COW : Character Oriented Windows
;*
;*	krun5.asm : Run/Exec

	TITLE	KRUN - Kernel RUN

	include	kernel.inc
	include uevent.inc

;----------------------------------------------------------------------------

cbArgMax	equ	79+1+3+128+2	; Max size pszArg to DosExecPgm.
					;   79 = max comspec filename size
					;    1 = sepearating zero.
					;    3 = "/C "
					;  128 = max real argument size
					;    2 = 00 00 terminating bytes.

;----------------------------------------------------------------------------

sBegin	KERNEL
    assumes DS,KERNEL


;*	* data that MUST be in code space

;*	* code strings
staticB	szComspec, <"COMSPEC", 0>	; COMSPEC name (note: no '=' for OS/2)

staticB	szCmdProg, <"\CMD.EXE", 0>	; if COMSPEC not found


sEnd	KERNEL

;----------------------------------------------------------------------------

externFP	<PeekMessage>
externFPublic	<LeaveCow,BackToCow>		;* in INIT
externFP	<DosExecPgm>
externFP	<DosScanEnv>
externFP	<VioWrtTty>
externFP	<EnableKeyboard>

sBegin	KERNEL

	assumes CS,KERNEL
	assumes DS,NOTHING
	assumes SS,DATA

;********** RerrExec **********
;*	entry : szCmd = program name (or NULL => shell)
;*		pchParm = parameter string:
;*		   Byte-length prefixed, 0Dh terminated.
;*		   Null is invalid, use 01,"0Dh" for subshell.
;*		   Use <length> "/C xxxxx" when szCmd == NULL.
;*		rgchPrompt = "Press a key to resume MangoSoft$"; -1 if none.
;*		fClearScreen = whether to clear it or not.
;*		fRestoreScreenMode = whether to reset mode on way back in.
;*	exit : AX = 0 if ok, AX != 0 if error (interpreted as "rerr" code,
;*		(see kmem.h).
;*	       DX = return code of child process (undefined if AX != 0)

	assumes CS,KERNEL
	assumes DS,DGROUP
	assumes ES,NOTHING
	assumes SS,DATA

cPublic	RerrExec, <>, <SI,DI,DS>		;* NOT ATOMIC !

	parmDP	szCmd
	parmDP	pchParm
	parmDP	rgchPrompt		; Dollar-sign terminated!
	parmW	fClearScreen
	parmW	fRestoreScreenMode

	localV	msgScreen,cbMsgMin
	localV	pszArgs,cbArgMax	; Where we'll build arg string.
	localD	lszProgram		; far pointer to Comspec
	localD	ReturnCodes	; 1st word = Termination code from OS/2.
				; 2nd word = Result code from child process.

cBegin	RerrExec

	AssertData	DS			; Make sure that ss=ds.

;------ Push the first three arguments to DosExecPgm.
	xor	bx,bx
	push	ss			; pchFailName.  We don't use the name
	push	bx			;   in any case, so give null buffer.
	push	bx			; cbFailName = 0.
	push	bx			; AsyncTraceFlags = synchronous.

	mov	SEG_lszProgram,ss	; If szCmd is not Null, 
	mov	ax,szCmd		;   then that's the program 
	mov	OFF_lszProgram,ax	;   that we'll exec.
	or	ax,ax
	jnz	@F			; Otherwise, we go for a Comspec.

	push	cs
	push	kernelOffset szComspec	; Hey OS/2, go look for "COMSPEC".
	push	ss
	lea	ax,lszProgram		; Where OS/2 will put ptr to Comspec.
	push	ax
	call	DosScanEnv
	or	ax,ax			; Jump if OS/2 found it.
	jz	@F

	mov	SEG_lszProgram,cs			; Guess: use \CMD.EXE
	mov	OFF_lszProgram,kernelOffset szCmdProg	;   for Comspec.
@@:

; At this point, all we've done is get lszProgram containing the segment and
; offset of the program we'll be Exec'ing, whether it be our program itself,
; the Comspec, or our best guess at a Comspec.
;
; Now build the pszArgs string.  Non-direct / Comspec types look like this:
;
;    "c:\os2\pbin\cmd.exe" 00 "/c ourprog /arg1 /arg2" 00 00
;
; Direct / non-Comspec types look like this:
;
;    "ourprog" 00 "/arg1 /arg2" 00 00

	push	ds
	mov	ax,ss
	mov	es,ax
	assumes	es,DATA
	lea	di,pszArgs		; es:si -> buffer.

	lds	si,lszProgram		; ds:si -> program we'll exec
	assumes DS,NOTHING

@@:	lodsb				; Transfer over the entire program
	stosb				;   name, and the 0 byte at the end.
	or	al,al
	jnz	@B
	pop	ds
	assumes DS,DGROUP

	AssertNE pchParm,0		; Null is bogus.
	mov	si,pchParm		; ss:si = ds:si -> the parameters
	xor	ax,ax
	lodsb				; al = count, ah = 00.
	AssertNe	al,0		; Have to at least have the 0D byte.
	dec	ax			; Don't transfer the 0D at the end.
	mov	cx,ax			; cx = count
	rep	movsb			; Move over the argument string.

	mov	es:[di],cx		; Finish it off with a double 0 byte.
	
; --------------------------------------

	push	ss			; Our mondo big and complicated
	lea	ax,pszArgs		;   local buffer
	push	ax

	push	cx			; EnvPointer = 0000:0000 -
	push	cx			;   inherit the parent's env.

	push	ss			; Address of DWord buffer where OS/2
	lea	ax,ReturnCodes		;   will put Termination Code and
	push	ax	    		;   Result Code of the child.

	push	SEG_lszProgram		; Address of ASCIIZ string specifying
	push	OFF_lszProgram		;   drive and dir of prog to execute
	mov	ax,2

; -----------------------			; Do at the last minute:
	cCall	LeaveCow, <fClearScreen>	;   Leave & maybe clear 
; -----------------------			;   the screen.

	call	DosExecPgm		; Do it.

	push	ax
	cCall	EnableKeyboard, <sp>
	pop	cx			; DosExecPgm return code.
	jcxz	@F

	mov	ax,2			; Guess it'll be one of these errors:
	cmp	cx,3			; If "Path not found", then no error
	je	NoFile			;   messages.
	cmp	cx,ax			; If "File not found", then no error
	je	NoFile			;   messages.
	cmp	cx,00BFh		; If ERROR_INVALID_EXE_SIGNATURE
	je	NoFile			;   then treat like "file not found".
	AssertEq	cx,0
@@:

; -----------------------

	mov	di,rgchPrompt		; ds:di -> "Press a key$"
	cmp	di,-1
	je	NoPrompt

	mov	ax,ds
	mov	es,ax
	assumes	es,DGROUP
	push	ax			; ax:di -> "Press a key$"
	push	di

	mov	ax,0124h		; al = "$"
	mov	cx,ax			; Assumption: the "Press a key to
	repne	scasb			;   "resume" string is < 292 bytes.
	AssertZR
	sub	ax,cx
	dec	ax			; Don't count the "$".
	push	ax			; Length of the string.

	push	0			; VioHandle

	call	VioWrtTty
	AssertEq	ax,0		; Shouldn't err.

@@:	AssertData	DS		; Make sure that ss=ds.
	lea	ax,msgScreen		; Wait until we get a message
	push	ax
	call	PeekMessage		;   that's a character.
	or	ax,ax
	jz	@B
	cmp	[msgScreen.messageMsg],WM_CHAR
	jne	@B

NoPrompt:
	mov	ax,OFF_ReturnCodes	; return 1st word = termination code

NoFile:

; -----------------------
	push	ax				; This calls an INIT proc
	cCall	BackToCow,<fRestoreScreenMode>	;   in the INIT segment.
	pop	ax
; -----------------------

	mov	dx,SEG_ReturnCodes	; return 2nd word = child result code

cEnd	RerrExec

sEnd	KERNEL

	END

