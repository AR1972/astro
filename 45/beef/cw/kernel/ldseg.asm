;*
;*	COW : Character Oriented Windows
;*
;*	ldseg.asm : segment loader

	.xlist
	include kernel.inc
	include	galloc.inc
	include rlb.inc			;* relocation info
	.list


IFDEF DEBPUB
	PUBLIC	OpenSwapFile
ENDIF


sBegin	DATA
    assumes DS,DGROUP

externW     <psLom>
externW	    <fColorModeOld>		;* last color mode

IFNDEF NOPCODE
externW	 fNew				;* from interpreter
ENDIF ;!NOPCODE

sEnd	DATA

sBegin	BSS
    assumes DS,DGROUP

globalB	    iexeCur, 0		;* current .EXE file open
				;* loader only uses the first file
				;* set to 255 if closing file

globalW	    fFreeOpen, 0	;* TRUE => 1 free open (no warning)
				;* (i.e. don't prompt before opening file)


sEnd	BSS


externFPublic <PromptSwapDisk>			;* from App or stub.

externFP    <GlobalReAlloc, GlobalHandle>
externFPublic <FInitScreenInternal>
externFPublic <ExitKernel>			;* from kerninit.asm
					;* in CORE segment (USER screen)

IFDEF KEEP_SYMDEB_CODE
externFP    <DebugDefineSegment>
ENDIF ; KEEP_SYMDEB_CODE

;*****************************************************************************

sBegin	KERNEL
    assumes CS,KERNEL
    assumes SS,DATA
    assumes DS,NOTHING		;* Called by Reload Segment !

externNP    <FRelocSegment>			;* from ldreloc.asm
externNP    <PatchThunkLoaded>			;* from ldthunk.asm
externNP    <PatchStackLoaded>			;* from ldstack.asm




;********** LoadSegment **********
;*	entry : segno = segment to load
;*	* Load in the segment & relocate
;*	exit : ax = ps of loaded segment

cProc	LoadSegment,<NEAR,PUBLIC>,<si,di>
    parmW segno
    localW psDest			;* destination ps
cBegin	LoadSegment

	mov	fFreeOpen,sp		;* allow 1 free open
;*	* calculate the pnewseg1
	mov	es,psLom
	Assert	<SIZE NEW_SEG1 EQ 10>
	mov	si,segno
	AssertReset si,0ff00h		;* must be byte
	AssertSet   si,000ffh		;* must be non-zero
	dec	si			;* zero base for calculation
	shl	si,1
	mov	ax,si
	shl	si,1
	shl	si,1
	add	si,ax			;* times 10
	add	si,es:[neLom.ne_segtab]		;* ES:SI => NEW_EXE1

;*	* Segment MUST be MOVEABLE CODE
	AssertSet es:[si].ns_flags,NSMOVE
	AssertReset es:[si].ns_flags,NSDATA

;*	* Allocate enough space to read in
        xor     ax,ax
	cCall   GlobalReAlloc,<es:[si].ns_handle,ax,es:[si].ns_minalloc,ax>
;*      * Abort if error
        or      ax,ax
        jnz     lds_ReAllocOK
	BREAKPOINT
        mov     ax,255
        cCall   ExitKernel,<ax>                ;* Fatal error
lds_reAllocOK:
	AssertNe ax,0
	cCall	GlobalHandle,<ax>		;* return dx = ps
;*	* save where loaded
	mov	psDest,dx			;* ps where loaded
;*	* Stuff in the correct flags
	dec	dx				;* point to arena
	mov	es,dx				;* es:0 => arena
	mov	es:[ga_flags],GA_MOVEABLE OR GA_DISCCODE

    assumes DS,NOTHING	;!!!
	push	ds				;* random DS
	mov	ds,psLom

;*	* Make sure we have the correct file open
IFDEF DUAL
loads_retry:
	mov	di,lomOffset neLom
	mov	ax,segno
	cCall	GetRle
	mov	ah,ds:[bx].re_iexe
	cmp	ah,iexeCur
	je	correct_file_open
	Save	<bx>
	cCall	OpenSwapFile
	jmp	short load_rlb
correct_file_open:
	cmp	bx,ds:[pbRleCurLom]
	je	correct_rlb_loaded
load_rlb:
	Save	<bx>
	cCall	LoadRle
correct_rlb_loaded:
ELSE ;!DUAL
	mov	ax,ds:[si].ns_flags
	Assert	<SHIFTDPL EQ 10>
	and	ah,HIGH(NSDPL)
	shr	ah,1
	shr	ah,1
	and	ah,3		;* iexe
	cmp	ah,iexeCur
	je	correct_file_open
loads_retry:
	cCall	OpenSwapFile
correct_file_open:
ENDIF ;!DUAL

;*	* Seek to the proper position in the file
	mov	dx,ds:[si].ns_sector		;* "sector"
	xor	ax,ax				;* ax:dx used for sector
	mov	cx,ds:[neLom.ne_align]		;* shift left count
loads_lp1:
	shl	dx,1
	rcl	ax,1
	loop	loads_lp1
	mov	cx,ax				;* cx:dx = lfa
IFDEF DUAL
IFDEF DEBUG
	AssertEQ bx,ds:[pbRleCurLom]
ENDIF ;DEBUG
	sub	dx,ds:[bx].LO_re_dlfaBias		;* adjust lfa
	sbb	cx,ds:[bx].HI_re_dlfaBias
ENDIF ;DUAL

	mov	bx,ds:[fdExeLom]
	mov	ax,4200h			;* seek device, from start
	int	21h
	jc	loads_error			;* seek error ?

;*	* Read it all in (bx = fd)
	mov	cx,ds:[si].ns_cbseg		;* file length
	push	ds
	mov	ds,psDest			;* where to load
	xor	dx,dx				;* ds:dx = ds:0 = read address
	mov	ah,3fh				;* read from device
	int	21h
	pop	ds
	jc	loads_error			;* read error ?
	cmp	ax,cx
	jne	loads_error			;* read them all ?

;*	* Test to see if we must zero fill the end of the memory block
	mov	di,ds:[si].ns_cbseg		;* offset past end
	mov	cx,ds:[si].ns_minalloc		;* may be larger
	sub	cx,di
	jcxz	loads_end
	cld
	mov	es,psDest
	xor	al,al
	rep	stosb				;* erase it
loads_end:	;* all loaded

	cCall	FRelocSegment,<si, psDest, segno> ;* relocate it, DS == psLom !
	or	ax,ax
	jz	loads_error

	pop	ds				;* restore original DS

;*	* Fix up thunks etc
	cCall	PatchThunkLoaded,<segno, psDest>

;*	* Fix up stack
	cCall	PatchStackLoaded,<segno, psDest>

;*	* Optional inform debugger
IFDEF KEEP_SYMDEB_CODE
        mov     cx,segno
        dec     cx				;* zero based seg #
	xor	ax,ax				;* always code
        cCall   DebugDefineSegment,<cx,psDest,ax>
ENDIF ;KEEP_SYMDEB_CODE

	mov	ax,psDest			;* return value

cEnd	LoadSegment

loads_error:
;*	* close file and try to open again
;*	* if retrying twice, prompt with error line
	mov	ah,iexeCur			;* re-open it
	jmp	loads_retry

;*****************************************************************************


;********** StrCpySiDi **********
;*	entry : ds:si => source
;*		es:di => destination
;*	* copy and zero terminate
;*	exit : es:di => next destination

cProc	StrCpySiDi, <NEAR, ATOMIC>
cBegin	StrCpySiDi

copy_lp:
	lodsb
	stosb
	or	al,al
	jnz	copy_lp

cEnd	StrCpySiDi



;********** OpenSwapFile **********
;*	entry : ah = iexe to open
;*		DS = psLom
;*		fFreeOpen => open before prompt
;*			(FALSE => prompt before open)
;*	* open the swap file (prompt and don't return till open)
;*	exit : n/a (fdExeLom and iexeCur updated).
;*		Kernel buffer filled with RLB info
;*	* TRASHES : DI
;*	* NOTE : will always open !!

cProc	OpenSwapFile, <NEAR, ATOMIC>, <SI>
cBegin	OpenSwapFile

    assumes DS,NOTHING
	xchg	ah,iexeCur			;* set iexeCur, get old
	or	ah,ah
	js	no_file_open			;* negative => 0FFH => closed
;*	* close old file first
	mov	bx,ds:[fdExeLom]
	AssertNE bx,-1
	mov	ah,3eh
	int	21h				;* close file, ignore errors !!
no_file_open:

;*	* otherwise open swap file (ds == psLom, iexeCur set up)
retry_open:	;* old file already closed

	cCall	GetProgFileName

	mov	cx,fFreeOpen
	jcxz	open_file_error			;* prompt first

	;* ds:dx => string
	mov	ax,3d00h
	int	21h				;* open for reading

	jnc	open_file_ok			;* open success

;*	* error when trying to find file (DS = psLom)
open_file_error:
	cCall	GetProgFileName
	push	ds				;* save psLom
	Assert	<?PLM>
IFNDEF NOPCODE
	push	fNew
ENDIF ;!NOPCODE
	;* push lszPath
	push	ds
	push	dx
	;* push ifile
	xor	ax,ax
	mov	al,iexeCur
	push	ax				;* parm.
	;* restore DS to DDS
	push	ss
	pop	ds
    assumes DS,DGROUP
	xor	ax,ax				;* pinst == NULL => prev mode
	cCall	FInitScreenInternal, <ax>	;* to text mode !!
	cCall	PromptSwapDisk			;* tell the user
	mov	ah,0DH
	int	21h				;* reset disk to try again
IFNDEF NOPCODE
	pop	fNew
ENDIF ;!NOPCODE
	mov	fFreeOpen,sp			;* allow retry
	pop	ds				;* restore psLom
    assumes DS,NOTHING
	jmp	retry_open

open_file_ok:
	AssertNE ax,-1
	mov	ds:[fdExeLom],ax		;* save fd

IFDEF DUAL
;*	* reading rlb is done by LoadSegment
ELSE ;!DUAL
;*	* now read in rlb for moveable segments (rlbMove or rlbSwap)
;*	* assume main swap file
	mov	bx,ax					;* fd
	mov	dx,word ptr ds:[neLom.ne_lfaRlbMove]	;* low word
	mov	cx,word ptr ds:[neLom.ne_lfaRlbMove+2]	;* high word
	mov	ax,ds:[neLom.ne_cbRlbMove]		;* size
	cmp	iexeCur,0
	je	reload_rlb				;* all set to go
;*	* auxiliary swap file : read EXH header
	xor	cx,cx
	xor	dx,dx				;* lfa = 0 (start of file)
	mov	ax,SIZE EXH
	cCall	ReadKernelBuff			;* return ss:bx => buffer
	jc	after_open_error
;*	* Validate swap file (must have proper signature, application version
;*	*   stamp and swap file #).
	cmp	ds:[bx].magicExh,magicNe
	jne	after_open_error		;* bad signature
	mov	si,ds:[neLom.ne_restab]		;* start of offsets
	mov	al,ds:[si-1]			;* chStamp
	cmp	ds:[bx].chStampExh,al
	jne	after_open_error		;* bad version stamp
	mov	al,iexeCur
	cmp	ds:[bx].ifileExh,al
	jne	after_open_error		;* bad swap file #
	mov	dx,word ptr ds:[bx].lfaRlbExh	;* low word
	mov	cx,word ptr ds:[bx].lfaRlbExh+2	;* high word
	mov	ax,ds:[bx].cbRlbExh		;* size
	mov	bx,ds:[fdExeLom]		;* restore fd

reload_rlb:	;* Load/Reload RLB into kernel work buffer
	;* cx:dx = lfa, ax = cb, bx = fd
;*	* now read in RLB info (into kernel buffer), ax = cb, bx = fd
	cCall	ReadKernelBuff			;* return ss:bx => buffer
						;* cx = fd
	jc	after_open_error
	cmp	ds:[bx].magicRlb,magicNe
	jne	after_open_error			;* bad signature
ENDIF ;!DUAL
;*	* success !!

cEnd	OpenSwapFile

after_open_error:	;* opened 
;*	* close file
	mov	bx,ds:[fdExeLom]
	AssertNE bx,-1
	mov	ah,3eh
	int	21h				;* close file, ignore errors !!
	jmp	open_file_error



;********** GetProgFileName **********
;*	entry : n/a (DS == psLom)
;*	* copy program drive/dir + file_name into far kernel buffer
;*	exit : ds:dx = start of string (in far kernel buffer)
;*	NOTE : Trashes SI & DI.

cProc	GetProgFileName, <NEAR, PUBLIC, ATOMIC>
cBegin	GetProgFileName

	mov	di,ds:[offRlbLom]
	mov	dx,di				;* start of string
	push	ds
	pop	es				;* es:di => rlb buffer
						;* (far kernel buffer)
	mov	si,lomOffset szBootPathLom

	cCall	StrCpySiDi
	dec	di				;* don't terminate

	mov	al,iexeCur
	mov	si,lomOffset szExeLom
	or	al,al
	jnz	@F				;* use szExeLom if 0
	test	byte ptr ds:[si],0ffh
	jnz	use_sz				;* ...and szExeLom is set
@@:
	xor	ah,ah
	shl	ax,1
	mov	bx,ax
	mov	si,ds:[neLom.ne_restab]		;* start of offsets
	add	si,ds:[si+bx]			;* to proper string
use_sz:
	cCall	StrCpySiDi

cEnd	GetProgFileName


IFDEF DUAL
;********** GetRle ***************
;*	entry:	ds:di => neLom
;*		al == segno
;*	* Return pointer to Rlb entry in Rlbtab for this segment
;*	exit:	bx => Rle
;*	* only bx changed

cProc	GetRle,<PUBLIC,NEAR>
cBegin	GetRle

;*	* Check correct RLB is loaded
	mov	bx,ds:[di].ne_rlbtab
grle_next_rlb_ent:
	cmp	al,ds:[bx].re_segLast
	jbe	grle_rlb_done
	add	bx,SIZE RLB_ENT
	jmp	short grle_next_rlb_ent
grle_rlb_done:

cEnd	GetRle


;********** LoadRle ***************
;*	entry:	ds == psLom
;*		bx => Rle
;*	* Load rlb for given rle
;*	exit:	see ReadKernelBuff for return values

cProc	LoadRle,<PUBLIC,NEAR>
cBegin	LoadRle

;*	* load correct rlb
	mov	ds:[pbRleCurLom],bx		;* set new marker
	mov	dx,ds:[bx].LO_re_lfaRlb
	mov	cx,ds:[bx].HI_re_lfaRlb
	sub	dx,ds:[bx].LO_re_dlfaBias
	sbb	cx,ds:[bx].HI_re_dlfaBias
	mov	ax,ds:[bx].re_cbRlb
	mov	bx,ds:[fdExeLom]
	cCall	ReadKernelBuff

cEnd	LoadRle
ENDIF ;DUAL



;********** ReadKernelBuff **********
;*	entry : ax = # of bytes to read
;*		cx:dx = lfa
;*		bx = fd
;*		ds == psLom
;*	* seek and read # of bytes into far kernel buffer
;*	exit : CY => error
;*	   else: ds:bx => start of kernel buffer,
;*		cx = fd
;*	* DS retained (access buffer off of ss:bx).

cProc	ReadKernelBuff, <NEAR, ATOMIC>
cBegin	ReadKernelBuff

	AssertEQ bx,ds:[fdExeLom]
	push	ax
	mov	ax,4200h			;* seek device, from start
	int	21h
	pop	cx				;* cx = # of bytes
	jc	read_kern_fail			;* seek error

	mov	dx,ds:[offRlbLom]
	mov	ah,3fh				;* read from file
	int	21h
	jc	read_kern_fail			;* read error
	cmp	ax,cx
	stc
	jne	read_kern_fail			;* didn't read it all

	mov	cx,bx				;* save fd
	mov	bx,dx				;* ds:bx => buffer
	clc

read_kern_fail:

cEnd	ReadKernelBuff


;*****************************************************************************


;********** AccessSwapFile **********
;*	entry : fOpen : TRUE => be sure default swap file is open
;*		FALSE => be sure all swap files are closed
;*	* enable / disable the swap file
;*	exit : n/a

cPublic	AccessSwapFile, <ATOMIC>, <DS, DI>
    parmW  fOpen
cBegin	AccessSwapFile

    assumes DS,DATA
	mov	ds,psLom
    assumes DS,NOTHING
	mov	cx,fOpen
	jcxz	close_swap_file

;*	* if iexeCur != 0, then open swap file #0
	xor	ah,ah				;* swap file # 0
	cmp	ah,iexeCur
	je	end_access_swap_file		;* already open

;*	* open the default swap file (ah == 0)
	mov	fFreeOpen,sp			;* allow 1 free open
	cCall	OpenSwapFile

end_access_swap_file:

cEnd	AccessSwapFile

close_swap_file:
	mov	ax,-1
	xchg	iexeCur,al		;* stuff 255 into iexeCur, get old
	cmp	ah,al
	je	end_access_swap_file		;* already closed
	mov	bx,ds:[fdExeLom]
	AssertEQ iexeCur,255
	AssertNE bx,-1
	mov	ah,3eh
	int	21h				;* close file
	jmp	short end_access_swap_file
	



;*****************************************************************************


sEnd	KERNEL

;*****************************************************************************

	END

