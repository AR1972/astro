TOGGLE  EQU     1       ; degine the toggle        
;*
;*	COW : Character Oriented Windows
;*
;*	krun2.asm : Alternate Run/Exec (for shell)
;*	*KLUDGE* included by "krun.asm" if EXEC_ALTERNATE.

.xlist
include togequ.inc
.list

;*****************************************************************************

MovSeg	MACRO	srDest, srSrc
	push	srSrc
	pop	srDest
ENDM

;*****************************************************************************

sBegin	DATA

externW	<psLom>
externW	<pGlobalHeap>

externB	<fShellPresent>			;* from kerninit.asm
externB <fCompactLowerHeap>             ;* from gcompact.asm
externB fToggle 		        ; Flag indicating toggle state

IFNDEF NOPCODE
externW	 fNew				;* from interpreter
ENDIF ;!NOPCODE

sEnd	DATA




;*****************************************************************************

IFDEF DEBPUB
	PUBLIC	FzGetEnv, ShrinkGlobalHeap, RestoreGlobalHeap
ENDIF ;DEBPUB



sBegin	KERNEL
    assumes DS,KERNEL


;*	* data that MUST be in code space

staticW	ssSave, ?			;* save SS
staticW	spSave, ?			;* save SP
externW MgrSP                           ;* Manager Stack pointer


sEnd	KERNEL

;*****************************************************************************

externFPublic <AccessSwapFile>			;* for closing
externFPublic <PromptSwapDisk>			;* from App or stub.

IFDEF DEBUG
externFPublic <PrintGlobalHeap> 		;* Print contents of heap
ENDIF ;DEBUG

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

sEnd    EXIT


;*****************************************************************************

externFP GlobalCompact
externFPublic <LeaveCow, BackToCow>		;* in INIT


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,NOTHING
    assumes SS,DATA

externNP    <genter>			; GINTERF.ASM
externNP    <gjoin,gmarkfree,gcheckfree>; GALLOC.ASM
externNP    <gnotify>		 	; GINTERF.ASM


;*****************************************************************************



;********** RerrExecProg **********
;*	entry : szCmd = program name (or NULL => shell)
;*		bCmdTail = Command Tail.
;*		(see kmem.h).
;*	* NOTE : it is the application's responsibility to ShrinkLocalHeap()
;*		all used local heaps.
;
; M003 - Only three arg pointers now passed.
; execl (szName, szArgs, szTokArg1, szTokArg2)
;
cProc   RerrExecProg,<FAR,PUBLIC>,<SI, DI>
        parmDP  szName
        parmDP  bCmdTail
cBegin  RerrExecProg

;
; * First lets Leave Cow and shrink the global Heap
	cCall	LeaveCow, <sp>			;* leave + clear screen

	cCall	ShrinkGlobalHeap, <sp>  ;* Make sure we discard everything

	PUSH	DS			; M004 - ES must be equal to DS for
	POP	ES			; M004 - STOS's and system calls
        
        mov     ax,4b00h                ; setup command tail
        xor     cx,cx                   ; zero out
        MOV     DX,szName               ; Get command name.
	MOV	BX,bCmdTail             ; Have command tail
	CLC

IFDEF   TOGGLE
	cmp	fToggle,FT_LOCKED	; If toggle entries are allowed during
	jne	EXint			;  execs but they are locked out now,
	cli				;  set fToggle to allow them during
	mov	fToggle,FT_NOTTOGGLED	;  this exec.  Only save SP when toggle
	mov	MgrSP,sp		;  entries are allowed.
ENDIF   ;TOGGLE

EXint:	INT	21h

IFDEF   TOGGLE
	push	ax			; Now if toggle entries were allowed
	lahf				;  during the exec, lock them out
	cmp	fToggle,FT_NOTTOGGLED	;  again because the exec has finished.
	jne	EXcln
	mov	fToggle,FT_LOCKED	; ***> HACK ALERT <***
EXcln:					; The CF flag must be saved and restored
	sahf				;  around the fToggle setting so that
	pop	ax			;  the success of the exec can be
	sti				;  determined.	This can't be done with
					;  pushf/popf because of a 286 chip bug
					;  so lahf/sahf were used instead.
					;  Gee - this is the 1st time I ever
					;  used those instructions!

ENDIF   ;TOGGLE
	JC	ExecErr

	mov	ah, 4dh			;Get the child return code.
	int	21h			;to return to the caller.
	
	jmp	ExecvDone
ExecErr:
	MOV	AX,-1
ExecvDone:


;*	* restore everything (and repaint screen)
	push	ax				;* save return value
	cCall	RestoreGlobalHeap

	pop	ax				;* return value

;*      * now return to the caller.
cEnd    RerrExecProg


;********** ShrinkGlobalHeap **********
;*	entry : n/a
;*	* shrink the global heap
;*	exit : n/a
;*	* NOTE : this code must be fixed since it throws everything out.

cProc	ShrinkGlobalHeap, <PUBLIC, NEAR, ATOMIC>, <SI, DI>
        parmW    fDiscardCode

cBegin	ShrinkGlobalHeap

    assumes DS,DGROUP

;*	* compact the heap (removing any free gaps) -- throw out code as well

	push	ds
	mov	es,pGlobalHeap
	xor	di,di
        
        mov     cx,fDiscardCode
        jcxz    NoDiscard               ;* We dont need to discard
	mov	ax,1			;* 1 reserved para
					;* NOTE : kludge to get all code
					;*   discarded !!!
	xchg	ax,es:[di].gi_reserve	;* get old reserve
	push	ax
	mov	ax,-1
	cCall	GlobalCompact,<ax, ax>	;* (-1) throw everything out
	mov	es,pGlobalHeap
	pop	es:[di].gi_reserve	;* restore reserve size

;*      * now lets try to compact everything to the lower heap including
NoDiscard:
;*      * fixed code blocks.
        mov     fCompactLowerHeap,1     ;* turn on special mode
        mov     ax,-1                   ;* It does not mater what number
        cCall   GlobalCompact,<ax, ax>  ;* move everything down

;*      * everything is as low as it can get, now we need to
;*      * shrink memory allocations or release them if they are not needed        
	mov	es,pGlobalHeap
        mov     fCompactLowerHeap,0     ;* Now turn it off

	mov	es,es:[di].hi_last	;* es => end sentinal
	mov	dx,es

;*	* scan from end till finding free block
find_free_loop:
	mov	es,es:[di].ga_prev	;* next block
        cmp     es:[di].ga_sig, GA_HOLE ;* do we have a hole
        jnz     ffl_not_hole            ;* No not a hole
        mov     dx,es                   ;* Save for now we have hole
        jmp     short find_free_loop    ;* continue the search
ffl_not_hole:
	mov	cx,es:[di].ga_owner
	jcxz	found_free		;* 0 owner => free
        mov     ax,es
	cmp     ax,es:[di].ga_prev      ;* previous pointer
	jnz	find_free_loop		;* -1 owner => MOB or sentinal (stop)
	jmp	end_shrink		;* No more to process

found_free:
;*	* es:0 => free block, dx:0 => end sentinal
;*      * see if special case, where previous is Hole and next is sentinal
;*      * or hole, in which case we can get rid of allocation.
;;;;        cmp     es:[di].ga_next,dx      ;* see if free space at end
;;;;        jnz     not_at_end              ;* no?
        mov     ds,es:[di].ga_prev      ;* see if special case
    assumes ds,NOTHING
        cmp     ds:[di].ga_sig,GA_HOLE  ;* See if we can purge the complet allocation
        jnz     part_is_used            ;* part of allocation still used.
;*
;*      * We have a memory allocation that can be freed to system
;*      * first turn hole into endsigniture. (or hole)        
;*      * ds:0 => hole, es:0 => free; dx:0 =>end sentinal        
        mov     es,dx                   ;* es:0 => End sentinal (or hole)
        mov     dx,ds:[di].ga_newpara   ;* dx => now has the allocated paragraph

        mov     al,es:[di].ga_sig       ;* get it signature
        mov     ds:[di].ga_sig,al       ;* put it in the holes.
        cmp     ax,GA_HOLE              ;* see if it was a hole
        jnz     no_hole_after_this_one  ;* no, not hole

;*      * have previous allocation (hole) still in use, need to fixup
        mov     ax,es:[di].ga_newpara   ;* need to save old holes starting pos
        mov     ds:[di].ga_newpara,ax   ;* 
        
;*      * also setup pointer to first allocation
        mov     ax,es:[di].ga_next      ;* Get the next pointer
        mov     ds:[di].ga_next,ax      ;* set as our next pointer
        mov     es,ax                   ;* we need to set its previous pointer
        mov     es:[di].ga_prev,ax      ;* links now setup
;*      * calculate new size of hole
        mov     bx,ds                   ;* pointer to 
        sub     ax,bx                   ;*
        dec     ax                      ;* new size
        mov     ds:[di].ga_size,ax      ;*

        jmp     short free_unused_alloc

;*      * No previous hole, we have the new end signature for now
no_hole_after_this_one:
        mov     ds:[di].ga_size,1       ;* assume it was an endsignature
        mov     ds:[di].ga_next,ds      ;* point to ourself

;*      * now lets free the unused segments.
free_unused_alloc:
        mov     es,dx                   ;* The allocation to delete
        mov     ah,49h
        int     21h                     ;* deleted the allocation.
        mov     dx,ds                   ;* setup to continue
        mov     es,dx                   ;* es:di =>hole

	cCall	genter			;* DS:DI => mob
        cmp     es:[di].ga_sig,GA_ENDSIG
        jnz     free_010
	mov	ds:[di].hi_last,dx	;* now the last block
free_010:
	sub	ds:[di].hi_count,2	;* remove old sentinal and free block
        jmp     short find_free_loop    ;* continue to process

;*****************************
;*      * Part of the allocation is still used.  See if we are in our
;*      * base, or in an extra allocated segment.
;*      * we can find this out, by, scanning backward until we find
;*      * a hole, or the beginning.
;*      * es:0 => free, ds:0 => previous, dx:0 => end of group
part_is_used:
;*      * convert the free block into either sentinal or hole
        push    ds                      ;* save previous pointer
        mov     ds,dx                   ;* End of allocation
        mov     bl,ds:[di].ga_sig       ;* see if hole or endsiz
	mov	es:[di].ga_sig,bl       ;* the signature
	mov	es:[di].ga_owner,-1
	mov	es:[di].ga_flags,0
        cmp     bl,GA_ENDSIG            ;* end or hole?
        jz      piu_end_was_end         ;* end was a end

;*      * End was hole, need to setup size and pointer
        mov     ax,ds:[di].ga_newpara   ;* need to save old holes starting pos
        mov     es:[di].ga_newpara,ax   ;* 
        
;*      * also setup pointer to allocation
        mov     ax,ds:[di].ga_next      ;* Get the next pointer
        mov     es:[di].ga_next,ax      ;* set as our next pointer
        mov     ds,ax                   ;* we need to set its previous pointer
        mov     ds:[di].ga_prev,ax      ;* links now setup
;*      * calculate new size of hole
        mov     cx,es                   ;* pointer to 
        sub     ax,cx                   ;*
        dec     ax                      ;* new size
        mov     es:[di].ga_size,ax      ;*
        jmp     short piu_find_begin    ;* find beginning of memory unit

;*      * No previous hole, we have the new end signature for now
piu_end_was_end:
        mov     es:[di].ga_size,1       ;* assume it was an endsignature
        mov     es:[di].ga_next,es      ;* point to ourself

;*      * now lets find the beginning of the memory unit.
piu_find_begin:
        pop     ds                      ;* restore pointer to previous
                
piu_loop: 
        cmp     ds:[di].ga_sig,GA_HOLE  ;* do we have a hole
        jz      piu_endloop             ;* We are processing hole segment
        mov     ax,ds                   ;* get previous pointer
        cmp     ax,ds:[di].ga_prev      ;* see if beginning of memory
        jz      piu_endloop             ;* we have processed the holes.
        mov     ds,ds:[di].ga_prev      ;* still looking
        jmp     short piu_loop          ;*
piu_endloop:
        push    ds                      ;* save starting point
        cCall   genter                  ;* get DS;DI => mob

;*	* at the end of the heap MUST be a free block of a reasonably
;*	* large size (the free block due to freeing all code)
    assumes ds,NOTHING
        dec     ds:[di].hi_count        ;* decrement the count
        mov     dx,es                   ;* dx = address of sentinal
        cmp     bl,GA_ENDSIG            ;* see if we need to update last ptr
        jnz     piu_not_last
        mov     ds:[di].hi_last,dx      ;* new last pointer
piu_not_last:
        pop     ds                      ;* restore first in block
        cmp     ds:[di].ga_sig,GA_HOLE  ;* see if hole or orignal
        jz     piu_hole                 ;

;*      * the part in use is from the original allocation
	mov	ax,psLom		;* 1 big block
        jmp     short piu_resize        ;* goto resize the block

;*      * was part that was added on
piu_hole:
        mov     ax,ds:[di].ga_newpara   ;* get the starting segment

;*      * now freeup the free part of the segment
piu_resize:
;*	* now free up anything to DOS
	mov	es,ax
        mov     bx,dx                   ;*
	sub	bx,ax			;* size of new block (in para)
	add	bx,1+cparaRunShell	;*  add slush (+1 to keep end sentinal)
	mov	ah,4ah			;* modify allocated memory
	int	21h

;*      * now setup to try to process any other blocks
        mov     dx,ds                   ;* save dx=end of group
        mov     es,dx                   ;* 
        jmp     find_free_loop          ;* see if anymore groups to process

;*	* resume with smaller global heap

end_shrink:
	pop	ds
IFDEF DEBUG
	call	FAR PTR PrintGlobalHeap 	;*!! look at heap on exit
ENDIF ;DEBUG

cEnd	ShrinkGlobalHeap




;********** RestoreGlobalHeap **********
;*	entry : n/a
;*	* restore global heap after ShrinkGlobalHeap
;*	exit : n/a
cProc	RestoreGlobalHeap, <PUBLIC, NEAR, ATOMIC>, <DS, SI, DI>

cBegin	RestoreGlobalHeap
    assumes ds,DGROUP

;*	* modify block to make as large as possible
;*      * first see if we have fragmented memory.  If so, reallocate
;*      * only the top one (for now).
;*
        push    ds                              ;* save for now
	cCall	genter				;* DS:DI => mob
        mov     es,ds:[di].hi_last              ;* Point to last segment
        mov     cx,ds:[di].hi_count             ;* get count
        pop     ds                              ;* restore ds segment
rst_loop:
        cmp     es:[di].ga_sig,GA_HOLE          ;* look for hole
        jz      rst_hole                        ;* found a hole segment
        mov     es,es:[di].ga_prev              ;* nope, lets try the next one
        loop    rst_loop                        ;*
;*      * if we got here, we have only one memory allocation
	mov	es,psLom			;* 1 big block
        jmp     short rst_try_1meg              ;* lets try to get 1 meg

rst_hole:
;*      * found a hole, get its segment number and try to expand
        mov     es,es:[di].ga_newpara           ;* address of memory allocation

rst_try_1meg:
;*      * now try to allocate 1 meg, such that we can find out how much we can get

	mov	bx,0ffffh			;* i want it all
	mov	ah,4ah
	int	21h
IFDEF DEBUG
	jc	ok_we_asked_for_too_much
	int	3				;* we got 1MB ???
ok_we_asked_for_too_much:
ENDIF ;DEBUG

;*	* now do it for real
	mov	ah,4ah
	int	21h
IFDEF DEBUG
	jnc	ok_we_got_it_back
	int	3				;* we got 1MB ???
ok_we_got_it_back:
ENDIF ;DEBUG


;*	* get address of new end sentinal
	mov	ax,es
        mov     cx,es                           ;* have cx save es
	add	ax,bx				;* ax = new end
;*	* check to make sure we are within the useable limits
        mov     es,psLom
	cmp	ax,es:[psUseMax]
	jb	have_high_limit
;*	* we must adjust our block size (free the rest to DOS)
;*	* (not efficient -- clean up later)
	mov	bx,es:[psUseMax]
	push	bx				;* limit
	sub	bx,cx				;* size we will use
        mov     es,cx                           ;* we need our segment
	mov	ah,4AH
	int	21h				;* modify memory size
	pop	ax
;*	AssertEQ ax,es:[psUseMax]
have_high_limit:
        mov     es,cx                           ;* restore our seg
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
        cmp     ax,ds:[bx].hi_last
        je      NoFreeMem                       ;* TSR Got us
	xchg	ax,ds:[bx].hi_last		;* set new last, get old
	inc	ds:[di].hi_count		;* adding free object

	mov	ds,ax
	mov	es,cx
	mov	es:[bx].ga_prev,ds		;* link to other free
IFDEF DEBUG
	cmp	ds:[bx].ga_sig,GA_ENDSIG
	je	ok_end_sig
	int	3
ok_end_sig:
ENDIF ;DEBUG

	sub	cx,ax				;* psNew - psOld
	sub	cx,GA_ALIGN			;* less overhead

	mov	ds:[bx].ga_sig,GA_SIGNATURE
	mov	ds:[bx].ga_owner,bx		;* FREE !!!
	mov	ds:[bx].ga_flags,GA_MOVEABLE
	mov	ds:[bx].ga_size,cx		;* new size
	mov	ds:[bx].ga_next,es		;* point to new sentinal
NoFreeMem:

cEnd	RestoreGlobalHeap



;*****************************************************************************


;********** CompactToUpperHeap **********
;*	entry : n/a
;*	* compact to upper heap
;*	exit : n/a
;*	* NOTE : this code must be fixed since it throws everything out.

cProc	CompactToUpperHeap, <FAR,PUBLIC, ATOMIC>, <DS, SI, DI>
cBegin	CompactToUpperHeap

    assumes DS,DGROUP

;*	* compact the heap (removing any free gaps) -- throw out code as well

	mov	es,pGlobalHeap
	xor	di,di
	xor	ax,ax			;*   discarded !!!

;* setup to compact to upper heap
					;* NOTE : kludge to get all code
	xchg	ax,es:[di].gi_reserve	;* get old reserve
	push	ax
	xor	ax,ax
	cCall	GlobalCompact,<ax, ax>	;* Just get biggest chunk.
	mov	es,pGlobalHeap
	pop	es:[di].gi_reserve	;* restore reserve size

cEnd	CompactToUpperHeap



sEnd	KERNEL
