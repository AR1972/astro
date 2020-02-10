;*
;*	COW : Character Oriented Windows
;*
;*	krun.asm : Run/Exec

	TITLE	KRUN - Kernel RUN

	include	kernel.inc
	include	galloc.inc


ifdef	Exec_Alternate
	include	krun2.asm		;* REVIEW -- integrate better !!
else	; Use this file
	.xlist
	include pbi.inc
	.list

;----------------------------------------------------------------------------

MovSeg	MACRO	srDest, srSrc
	push	srSrc
	pop	srDest
ENDM

;----------------------------------------------------------------------------

sBegin	DATA

ifdef	Debug
externB	fCheckCwHeap
endif	; Debug

externW	psLom
externW	pGlobalHeap

externB	fShellPresent				;* from kerninit.asm

ifndef	NopCode
externW	 fNew				;* from interpreter
endif	; !NopCode

ifdef	WINDOWS_OLD_APP
externW	fWoaPresent
externW	psDosRealloc
endif	; WINDOWS_OLD_APP

sEnd	DATA

;----------------------------------------------------------------------------

IFDEF DEBPUB
	PUBLIC	FzGetEnv, ShrinkGlobalHeap, RestoreGlobalHeap
	PUBLIC	PromptMissingExec
ENDIF ;DEBPUB

IFDEF	WINDOWS_OLD_APP
externFP	<WoaToDos, WoaFromDos>
ENDIF	; WINDOWS_OLD_APP

;----------------------------------------------------------------------------

sBegin	KERNEL
    assumes DS,KERNEL


;*	* data that MUST be in code space

ifdef	QC_LINKER
globalD	ShlLinkSave,?			;* save SS:SP
else	; ! QC_LINKER
staticW	ssSave, ?			;* save SS
staticW	spSave, ?			;* save SP
endif	; QCLINKER


;*	* code strings
staticB	szComspec, <"COMSPEC=">		;* COMSPEC name
cchComspec	 EQU	$-szComspec

staticB	szCmdProg, <"\COMMAND.COM", 0>	;* if COMSPEC not found


sEnd	KERNEL

;----------------------------------------------------------------------------

externFPublic <AccessSwapFile>			;* for closing
externFPublic <PromptSwapDisk>			;* from App or stub.


;----------------------------------------------------------------------------

sBegin	KERNEL

    assumes CS,KERNEL
    assumes DS,DGROUP
    assumes SS,DATA
    assumes ES,NOTHING

;********** PromptMissingExec **********
;*	entry : lszPath = far pointer to path name
;*	* Prompt for a missing EXEC program
;*	* NOTE : this function must be in the EXIT module
;*	exit : n/a (trashes SI/DI)

cProc	PromptMissingExec, <FAR, ATOMIC>
    parmD lszPath
cBegin	PromptMissingExec
    assumes DS,DGROUP

;*	* assumes INIT module loaded (from call to BackToCow)
;*	* this routine gets loaded from the EXIT module

;*	* we will not swap from this point on (both INIT and EXIT resident)
;*	* we must close the swap file in order to use the Kernel buffer
	xor	ax,ax
	cCall	AccessSwapFile,<ax>

ifndef	NopCode
	push	fNew
endif	; !NopCode

	mov	es,psLom
	lds	si,lszPath
    assumes DS,NOTHING
	mov	di,es:[offRlbLom]		;* ES:DI => far kernel buffer
	push	es
	push	di				;* TOS = lszPath

@@:	lodsb
	stosb
	or	al,al
	jnz	@B

	MovSeg	ds,ss
    assumes DS,DGROUP

;*	* (TOS) = lszPath
	mov	ax,-1				;* special iexe for exec
	push	ax
	cCall	PromptSwapDisk			;* PromptSwapDisk(lszPath, -1);

	mov	ah,0DH
	int	21h				;* reset disk to try again
ifndef	NopCode
	pop	fNew				;* pcode may have changed fNew
endif	; !NopCode

cEnd	PromptMissingExec

sEnd	KERNEL

;----------------------------------------------------------------------------

sBegin	EXIT
    assumes CS,EXIT
    assumes DS,NOTHING
    assumes SS,DATA

;*	* NOTE :
;*	*  GetEnv is in EXIT segment since LeaveCow will be called first

;********** FzGetEnv **********
;*	entry : ES:DI => environment string to look for (with ending '=')
;*		CX = length of string
;*	* scan environment for Variable
;*	exit : Z => DS:SI => contents on ENV variable
;*	    else NZ=> not found
;*	* NOTE : uses AX/SI/DS
;*
cProc	FzGetEnv,<FAR, ATOMIC>
cBegin	FzGetEnv
	AssertNE	cx,0			; Would produce a "found".
	mov	ds,psLom
	mov	ds,ds:[pdbLom.PDB_environ]	;* psEnvironment
    assumes ds,nothing
	xor	si,si				;* ds:si => environment
	cld
getenv_lp:
	push	di
	push	cx
	repz	cmpsb
	pop	cx
	pop	di
	jz	getenv_end
getenv_skip:
	lodsb
	or	al,al
	jnz	getenv_skip
	cmp	byte ptr ds:[si],al	;* is this the real end ?
	jnz	getenv_lp
	or	cx,cx			;* NZ => not found
getenv_end:
cEnd	FzGetEnv
    assumes ds,nothing



sEnd	EXIT

;----------------------------------------------------------------------------

externFP	GlobalCompact
externFPublic	<LeaveCow, BackToCow>		;* in INIT

;----------------------------------------------------------------------------

sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING
    assumes SS,DATA

externNP	<genter>			; GINTERF.ASM
externNP	<gjoin,gmarkfree,gcheckfree>	; GALLOC.ASM
externNP	<gnotify>		 	; GINTERF.ASM

;-----------------------------------------------

run_special_shell:		; It's up here to be within jcxz range.
	mov	ax,1801h			;* run shell
	int	2fh				;* al = return code
	xor	ah,ah
	jmp	done_run_shell

;********** RerrExec **********
;*	entry : szCmd = program name (or NULL => shell)
;*		pchParm = parameter string:
;*		   Byte-length prefixed, 0Dh terminated.
;*		   Null is invalid, use 01,"0Dh" for subshell.
;*		   Use <length> "/C xxxxx" when szCmd == NULL.
;*		rgchPrompt = "Press a key to resume MangoSoft$"; -1 if none.
;*		fClearScreen = whether to clear it or not.
;*		fRestoreScreenMode = whether to reset mode on way back in.
;*	* shrink memory / run subshell / restore memory
;*	exit : AX = 0 if ok, AX != 0 if error (interpreted as "rerr" code,
;*		(see kmem.h).
;*	       DX = return code of child process (undefined if AX != 0)

cPublic	RerrExec, <>, <DS, SI, DI>		;* NOT ATOMIC !
    parmDP  szCmd
    parmDP  pchParm
    parmDP  rgchPrompt				;* Dollar-sign terminated!
    parmW   fClearScreen
    parmW   fRestoreScreenMode
    localD  lszPath				;* far pointer to path
    localV  pbiT,<SIZE PBI>			;* parameter block
    localW  ChildCode
    localW  ExecCode
cBegin	RerrExec
    assumes DS,DGROUP

ifdef	Debug
	xor	ax,ax				; Set fCheckCWHeap to false,
	xchg	al,fCheckCWHeap			;   and push its old value.
	push	ax
endif	; Debug

;RetryRerrExec:
	AssertNE pchParm,0			;* NULL invalid

	MovSeg	es,cs
	mov	di,kernelOffset szComspec	;* ES:DI => string
	mov	cx,cchComspec
	cCall	FzGetEnv
    assumes DS,NOTHING
	jz	got_comspec			;* DS:SI => comspec

	MovSeg	ds,cs
	mov	si,kernelOffset szCmdProg ;* use COMMAND.COM
got_comspec:	;* ds:si => path/file for COMMAND shell

	mov	OFF_lszPath,si
	mov	SEG_lszPath,ds

	MovSeg	ds,ss
    assumes DS,DGROUP
	cCall	LeaveCow, <fClearScreen>	;* leave & maybe clear screen

	cCall	ShrinkGlobalHeap

	mov	di,pchParm			;* ss:di => string
;*	* lszPath => cmd, SS:DI => parm.

;*	* if szCmd != NULL then exec named program
	mov	cx,szCmd
	jcxz	exec_a_shell
	mov	OFF_lszPath,cx
	mov	SEG_lszPath,ss			;* szCmd
	jmp	short exec_command

exec_a_shell:  ;* (ch == 0)
;*	* if pchParm == "" then we can run a special shell if present
	mov	cl,fShellPresent		;* ch == 0.
	jcxz	exec_command
	mov	cl,ds:[di]			;* ds == ss, ch == 0.
ifdef	QC_LINKER
	jcxz	F@
	jmp	short exec_command
F@:	jmp	run_special_shell
else	; !QC_LINKER
	jcxz	run_special_shell
endif	; QC_LINKER

exec_command:
;*	* Exec a command (SS:DI => command line, lszPath => command).
;*	* set up PBI
	mov	es,psLom
	lea	bx,pbiT
	mov	ax,es:[pdbLom+PDB_environ]
	mov	[bx].psEnviron,ax
	mov	[bx].offCmdLine,di
	mov	[bx].psCmdLine,ss		;* command (on stack)
	mov	[bx].offFcb1,5CH
	mov	[bx].psFcb1,es
	mov	[bx].offFcb2,6CH
	mov	[bx].psFcb2,es

;*	* save the important part of the world
	push	ds
	push	bp
;*	* Save DWORD PTR SS:[2E] since DOS 2.0 has a bug which thinks that SS
;*	*  is pointing to a PSP (and hence stuffs the old stack at this address)
	push	word ptr ss:[2EH]
	push	word ptr ss:[30H]

ifdef	QC_LINKER
	mov	WORD PTR (ShlLinkSave+2),ss	;* Save SS
	mov	WORD PTR (ShlLinkSave),sp	;* Save SP
	sub	WORD PTR (ShlLinkSave),6	;* Add some slop
else	; !QC_LINKER
	mov	ssSave,ss
	mov	spSave,sp
endif	; QC_LINKER

	push	ss
	pop	es

	lds	dx,lszPath			;* command path

	mov	ax,4B00H			;* exec : load + execute
	int	21h

	jc	exec_error
	xor	ax,ax				;* no error
exec_error:

	cli
ifdef	QC_LINKER
	mov	ss, WORD PTR (ShlLinkSave+2)	;* Restore ss
	mov	sp, WORD PTR (ShlLInkSave)	;* Restore sp
	add	sp,6				;* Remove slop
else	; ! QC_LINKER
	mov	ss,ssSave
	mov	sp,spSave
endif	; QC_LINKER
	sti

	pop	word ptr ss:[30H]
	pop	word ptr ss:[2EH]		;* Dos 2.0 tromping restored
	pop	bp
	pop	ds
    assumes DS,DGROUP

	mov	ExecCode,ax			;* save the exec return code
	mov	ChildCode,0			;* init the child return code

	cmp	ax,2				;* If file not found, then
	je	done_run_shell			;*   don't bother with prompt
;	je	run_shell_not_found

	mov	dx,rgchPrompt			;* ds:dx -> "Press a key$"

	or	ax,ax				;* Don't get the child code
	jnz	CheckPrompt			;*   if Exec failed.

	mov	ah,4Dh
	int	21h
	mov	ChildCode,ax			;* save the child return code

ifndef	FOR_QC			;* This should be ifdef'ed for project
	cmp	ax,4				;* This is Word's special
	je	DoPrompt			;*   "Do Prompt" return code.
endif	; !FOR_QC

CheckPrompt:
	cmp	dx,-1
	je	done_run_shell

DoPrompt:
	mov	ah,09h
	int	21h			; Prompt to press any key
	mov	ax,0C07h
	int	21h			; Wait for key input

done_run_shell:
;*	* restore everything (and repaint screen)

	cCall	RestoreGlobalHeap

;*	* the following call calls an INIT procedure
	cCall	BackToCow,<fRestoreScreenMode>	;* in INIT segment

ifdef	Debug
	pop	ax				; Restore fCheckCWHeap to 
	mov	fCheckCWHeap,al			;   its old value.
endif	; Debug

	mov	ax,ExecCode			;* restore these for 
	mov	dx,ChildCode			;*   return values.

cEnd	RerrExec

;*	* special case if exec program is not found

;run_shell_not_found:
;	cCall	RestoreGlobalHeap
;;*	* re-init CW for windowing
;	cCall	BackToCow			;*  Load in INIT module
;	cCall	PromptMissingExec, <lszPath>
;
;	jmp	RetryRerrExec			;* retry


;-----------------------------------------------


;********** ShrinkGlobalHeap **********
;*	entry : n/a
;*	* shrink the global heap
;*	exit : n/a
;*	* NOTE : this code must be fixed since it throws everything out.

cProc	ShrinkGlobalHeap, <NEAR, ATOMIC>, <DS, SI, DI>
cBegin	ShrinkGlobalHeap

    assumes DS,DGROUP

IFDEF	WINDOWS_OLD_APP
	;*
	;*	If win-old-app model is present then claim as much as 
	;*	possible from it.  It will also modify the address
	;*	which is to be used for reallocing the segment.
	;*
	cmp	fWoaPresent,0
	jz	@F
	cCall	WoaToDos			;* Shut down win-old-app code
@@:
ENDIF	; WINDOWS_OLD_APP

;*	* compact the heap (removing any free gaps) -- throw out code as well

	mov	es,pGlobalHeap
	xor	di,di
	mov	ax,1			;* 1 reserved para
					;* NOTE : kludge to get all code
					;*   discarded !!!
	xchg	ax,es:[di].gi_reserve	;* get old reserve
	push	ax
	mov	ax,-1
	cCall	GlobalCompact,<ax, ax>	;* (-1) throw everything out
	mov	es,pGlobalHeap
	pop	es:[di].gi_reserve	;* restore reserve size

	mov	es,es:[di].hi_last	;* es => end sentinal
	mov	dx,es
;*	* scan from end till finding free block
find_free_loop:
	mov	es,es:[di].ga_prev	;* next block
	mov	cx,es:[di].ga_owner
	jcxz	found_free		;* 0 owner => free
	inc	cx
	jnz	find_free_loop		;* -1 owner => MOB or sentinal (stop)
	jmp	end_shrink		;* can't shrink

found_free:
;*	* es:0 => free block, dx:0 => end sentinal
	cmp	es:[di].ga_next,dx
	je	dont_move_bound

;*	* move the bound segments from high to low memory
	mov	ds,es:[di].ga_next	;* 1 after block
    assumes ds,NOTHING
	mov	dx,es:[di].ga_prev
move_bound_seg_loop:
	;* ds:0 => source, es:0 => dest
	;* dx = prev link
	mov	ds:[di].ga_prev,dx
	mov	dx,ds
	mov	cx,ds:[di].ga_next
	sub	cx,dx
	mov	ax,es
	add	ax,cx			;* ax = new dest
;*	* move from source to dest (dest before source => move up)
	xor	si,si
	shl	cx,1
	shl	cx,1
	shl	cx,1			;* cpara -> cw (<64K blocks)
	rep movsw			;* move arena + data
	xor	di,di
;*	* notify everyone
	push	ds
	push	es
	push	ax
	mov	al,GN_MOVE
	mov	bx,es:[di].ga_handle	;* handle
	mov	cx,es
	inc	cx			;* psNew
	mov	dx,es:[di].ga_owner	;* set up owner for gnotify
	mov	ds,pGlobalHeap
	push	cx			;* new address
	push	bx			;* handle
	call	gnotify 		; Call global notify procedure
	pop	bx
	pop	ds:[bx].he_address
	pop	ax
	pop	es
	pop	ds
;*	* update link to next
	mov	dx,es			;* next link
	mov	bx,ax
	xchg	ax,es:[di].ga_next
	mov	es,bx			;* next destination
	mov	ds,ax			;* next block
	cmp	ds:[di].ga_owner,-1	;* stop at sentinal
	jne	move_bound_seg_loop
;*	* ds:0 => end sentinal, es:0 => where sentinal should be
	mov	bx,es
	mov	ds,bx
	mov	ds:[di].ga_prev,dx	;* back link
	jmp	update_end_sentinal

dont_move_bound:
;*	* at the end of the heap MUST be a free block of a reasonably
;*	* large size (the free block due to freeing all code)
	cCall	genter			;* DS:DI => mob
    assumes ds,NOTHING
	mov	es,ds:[di].hi_last	;* pointer to sentinal
	mov	cx,es			;* last block
	mov	bx,es:[di].ga_prev	;* must be a free block
	mov	ds,bx
	cmp	ds:[di].ga_owner,0
	jne	end_shrink
;*	* es:di => free block that should be sentinal
	mov	es,cx			;* es:di =>old sentinal

ifdef	Debug
;*	* check out state of old sentinal
	cmp	es:[di].ga_sig,GA_ENDSIG
	je	ok_old_sent
bad_old_sent:
bad_new_sent:
	int	3
ok_old_sent:
	cmp	es:[di].ga_owner,-1
	jne	bad_old_sent
	cmp	es:[di].ga_size,GA_ALIGN
	jne	bad_old_sent
	cmp	es:[di].ga_flags,0
	jne	bad_old_sent
;*	* check new sentinal
	cmp	ds:[di].ga_sig,GA_SIGNATURE
	jne	bad_new_sent
endif	; Debug

update_end_sentinal:	; ds==bx == para address of end block
	mov	ds:[di].ga_sig,GA_ENDSIG
	mov	ds:[di].ga_owner,-1
	mov	ds:[di].ga_size,GA_ALIGN
	mov	ds:[di].ga_flags,0
	mov	ds:[di].ga_next,ds		;* link to self

	cCall	genter			;* DS:DI => mob
	mov	ds:[di].hi_last,bx	;* now the last block
	dec	ds:[di].hi_count	;* remove free object

;*	* now free up anything to DOS
IFDEF	WINDOWS_OLD_APP
	mov	ax,psDosRealloc
ELSE	; !WINDOWS_OLD_APP
	mov	ax,psLom		;* 1 big block
ENDIF	; WINDOWS_OLD_APP
	mov	es,ax
	sub	bx,ax			;* size of new block (in para)
	add	bx,1+cparaRunShell	;*  add slush (+1 to keep end sentinal)
	mov	ah,4ah			;* modify allocated memory
	int	21h

;*	* resume with smaller global heap

end_shrink:

cEnd	ShrinkGlobalHeap


;-----------------------------------------------


;********** RestoreGlobalHeap **********
;*	entry : n/a
;*	* restore global heap after ShrinkGlobalHeap
;*	exit : n/a

cProc	RestoreGlobalHeap, <NEAR, ATOMIC>, <DS, SI, DI>

cBegin	RestoreGlobalHeap
    assumes ds,DGROUP

;*	* modify block to make as large as possible
IFDEF	WINDOWS_OLD_APP
	mov	es,psDosRealloc
ELSE	; !WINDOWS_OLD_APP
	mov	es,psLom			;* 1 big block
ENDIF	; WINDOWS_OLD_APP
	mov	bx,0ffffh			;* i want it all
	mov	ah,4ah
	int	21h

ifdef	Debug
	jc	ok_we_asked_for_too_much
	int	3				;* we got 1MB ???
ok_we_asked_for_too_much:
endif	; Debug

;*	* now do it for real
	mov	ah,4ah
	int	21h

ifdef	Debug
	jnc	ok_we_got_it_back
	int	3				;* we got 1MB ???
ok_we_got_it_back:
endif	; Debug

;*	* get address of new end sentinal
	mov	ax,es
	add	ax,bx				;* ax = new end
;*	* check to make sure we are within the useable limits
IFDEF	WINDOWS_OLD_APP
	mov	es,psLom
ENDIF	; WINDOWS_OLD_APP
	cmp	ax,es:[psUseMax]
	jb	have_high_limit
;*	* we must adjust our block size (free the rest to DOS)
;*	* (not efficient -- clean up later)
	mov	bx,es:[psUseMax]
	push	bx				;* limit
	mov	ax,es
	sub	bx,ax				;* size we will use
	mov	ah,4AH
	int	21h				;* modify memory size
	pop	ax
	AssertEQ ax,es:[psUseMax]
have_high_limit:
	sub	ax,GA_ALIGN			;* room for arena
	and	al,LOW(GA_MASK)			;* make even
	mov	ds,ax				;* es => new sentinal
    assumes ds,NOTHING
;*	* create new sentinal
	xor	bx,bx
	mov	ds:[bx].ga_sig,GA_ENDSIG
	mov	ds:[bx].ga_owner,-1		;* sentinal
	mov	ds:[bx].ga_size,GA_ALIGN
	mov	ds:[bx].ga_flags,bl
	mov	ds:[bx].ga_handle,bx		;* no handle
	mov	ds:[bx].ga_next,ds		;* link to self
	mov	cx,ax				;* psNew

	cCall	genter				;* DS:DI => mob
	xchg	ax,ds:[bx].hi_last		;* set new last, get old
	inc	ds:[di].hi_count		;* adding free object

	mov	ds,ax
	mov	es,cx
	mov	es:[bx].ga_prev,ds		;* link to other free

ifdef	Debug
	cmp	ds:[bx].ga_sig,GA_ENDSIG
	je	ok_end_sig
	int	3
ok_end_sig:
endif	; Debug

	sub	cx,ax				;* psNew - psOld
	sub	cx,GA_ALIGN			;* less overhead

	mov	ds:[bx].ga_sig,GA_SIGNATURE
	mov	ds:[bx].ga_owner,bx		;* FREE !!!
	mov	ds:[bx].ga_flags,GA_MOVEABLE
	mov	ds:[bx].ga_size,cx		;* new size
	mov	ds:[bx].ga_next,es		;* point to new sentinal

IFDEF	WINDOWS_OLD_APP
	;* Restore win-old-app model if present
	cmp	fWoaPresent,0		;* Is win-old-app segment present?
	jz	@F			;* No -- skip around
	mov	ax,DGROUP		;* Calling c procedure
	mov	ds,ax			;*
	cCall	WoaFromDos		;* Restore win-old-app code
@@:
ENDIF	; WINDOWS_OLD_APP


cEnd	RestoreGlobalHeap

sEnd	KERNEL

endif	; !Exec_Alternate


;*****************************************************************************

sBegin	INIT
    assumes CS,INIT
    assumes DS,DGROUP
    assumes SS,DGROUP


;********** GetProgDir **********
;*	entry : szBuff => string buffer to put path name
;*		(must be 66 characters or longer)
;*	* copy startup directory to this near buffer
;*	exit : n/a

    assumes DS,DGROUP

cPublic GetProgDir, <ATOMIC>, <DS, SI, DI>
    parmDP szBuff
cBegin	GetProgDir

	push	ds
	pop	es				;* destination a near pointer
	assumes ES,NOTHING

	mov	di,szBuff
	mov	ds,psLom
	assumes DS,NOTHING

	mov	si,lomOffset szBootPathLom
@@:	lodsb
	stosb
	or	al,al
	jnz	@B		 		;* assumes zero terminated

szBPL		equ	lomOffset szBootPathLom
szBPLRoot	equ	szBPL + 4

	cmp	si,szBPLRoot
	je	AtRoot
	mov	es:[di-2],al
AtRoot:

cEnd	GetProgDir


sEnd	INIT


;*****************************************************************************

	END

