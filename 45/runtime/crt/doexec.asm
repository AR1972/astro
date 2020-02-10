	page	,132
	title	doexec	-- (xenix) exec a child process
;***
;doexec.asm - execute a child process
;
;	Copyright (c) 1985-1988, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	defines _doexec() - execute a child process (overlay existing process)
;	This is complicated and involves some knowledge of DOS
;	arena headers.
;
;*******************************************************************************

?DF	=	1	; tell cmacros.inc we want to define our own segments

.xlist
include	version.inc
include	cmacros.inc
include	msdos.inc
.list

assumesdata	macro	seg	;;[1] Newer versions of CMACROS reject
assumes	seg,DGROUP		;;[1]
endm				;;[1]

createSeg _TEXT, code,	word,	public, CODE,	<>
createSeg _DATA, data,	word,	public, DATA,	DGROUP
createSeg EXEC,	eseg,	word,	common, DATA,	DGROUP

defGrp	DGROUP			; define DGROUP

codeOFFSET equ	offset _TEXT:
dataOFFSET equ	offset DGROUP:



l	macro	nam		;;[1] Conditionally make label public
nam:				;;[1]
	endm			;;[1]


arena	struc			; first 5 bytes of the arena header
	sig	db	0		; 'M' or 'Z' for last block
	own	dw	0		; PSP value of owner process or 0 if free
	asiz	dw	0		; size of block (not including header)
arena	ends

extrn	b$EX_MSG_BEG:FAR	;[1]
extrn	b$EX_MSG_END:FAR	;[1]

sBegin	data
assumesdata	ds		;[1]

externW	_psp			; psp segment
externB	_osmajor 		; dos major version number
externW	_sigintseg 		; SIGINT default signal handler (segment)
externW	_sigintoff 		; SIGINT default signal handler (offset)

globalW	_p_overlay,2 		; OLD_P_OVERLAY value

staticW	freepsp,0 		; arena of last free segment contiguous with PSP



staticW emsg,<2+ OFFSET b$EX_MSG_BEG> ;[1] +2 to skip first msg number
staticW emsgseg,<SEG b$EX_MSG_BEG>    ;[1]
staticW	emsgln,<OFFSET b$EX_MSG_END>;[1] must subtract emsg from it

staticD	target,0 		; for long jump to 'exec code'

sEnd


FCB1	=	5ch		; offset in psp
FCB2	=	6ch

DOS2CMD= 280h			; size of DOS 2.0 non-resident COMMAND.COM


externP execve

sBegin	eseg

	dd	execve		; force in execve() if called from spawnve

sEnd

sBegin	code
assumes	cs,code
assumesdata	ds		;[1]


page
;***
;_doexec - execute a child process (overlay existing)
;
;Purpose:
;
;Entry:
;
;Exit:
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

cProc	_doexec,<PUBLIC>,<si,di>

	parmw	flag
	parmdp	nam
	parmw	nlength
	parmdp	command
	parmdp	envblock
	parmw	elength
	parmw	siz
	parmw	initss
	parmw	initsp
	parmw	initcs
	parmw	initip
	parmw	fsiz

cBegin

;	exec overlays the current process using its PSP and all memory
;	above it.

	push	ds		; save DGROUP

;	check for trashed arena before we go on - will assume OK from here on
;	DGROUP is on the top of the stack

	assumes	ds,nothing
l	checkmem
	pop	ds		; restore DGROUP
	assumesdata	ds	;[1]

	mov	bx,-1		; request max memory
	callos	allocmem	; will always fail
	cmp	al,E_arena
	je	ov0		;   arena is trashed

;	find out how much contiguous memory there is that is either free
;	or belongs to the current process

	mov	bx,[_psp]
	mov	dx,bx		; dx = current owner
	dec	bx		; bx = current arena header
	xor	cx,cx		; last free block in contiguous area

	push	ds		; save DGROUP
l	maxloop
	assumes	ds,nothing
	mov	ds,bx		; ds = current arena
	mov	ax,ds:[own]
	cmp	ax,dx		; do we own it?
	je	maxadd		;   yes - count it
	or	ax,ax		; is it free?
	jne	maxend		;   no - end of contiguous memory

	mov	cx,bx		; last free block of memory

l	maxadd
	inc	bx
	add	bx,ds:[asiz]
	jc	checkmem	;   carry - assuming arena is trashed
	mov	al,ds:[sig]	; get arena signature
	cmp	al,'M'		; are we at end of memory?
	je	maxloop		;   no - have good next block
	cmp	al,'Z'		;
	jne	checkmem	;   unknown sig byte - assuming arena is trashed

;	bx = top of contiguous area for current process
;	cx = last free segment contiguous with PSP

l	maxend
	sub	bx,dx		; bx = size
	pop	ds		; restore DS
	assumesdata	ds	;[1]
	mov	[freepsp],cx	; save last free segment contiguous with PSP

	cmp	[_osmajor],2	; see if 2.x or above
	ja	nohole		;   3.x or above, don't need hole for loader

	sub	bx,DOS2CMD+1	; leave 280h paras (10K) for the system's loader
	jnc	nohole		;   enough space

;-----	memory overflow or error before user memory is released

l	ov
	mov	ax,E_nomem	; not enough memory
l	ov0

	mov	AH,4CH		;[1]get code to terminate
	INT	21H		;[1]immediate termination

;-----

l	nohole

; see how big the exec code will be. it must sit in hi mem. it consists of
; code between a: and z: below (or b: and y: for .coms), the filename, (in the
; case of .exe files) a para of data (initial register values), and some room
; (160 bytes) for a local stack


STKSIZ=	160
exesz=	z-a
exeln=	exesz	+ STKSIZ	; .exe loader size + stack
comsz	=	y-b
comln=	comsz	+ STKSIZ	; .com loader size + stack

	push	bx		; save max length possible


	sub	[emsgln],offset b$EX_MSG_BEG ;[1] compute message length
	mov	ax,[emsgln]	;[1] need emsgln in calculations
	

	add	ax,BYTE PTR exeln	;[2] assume .exe
	cmp	flag,0
	je	join0		;   was .exe
___tmp=	comln-exeln
	add	ax,BYTE PTR ___tmp	;[2] was .com
l	join0

	mov	cl,4
	add	ax,nlength	; plus filename
	add	ax,0fh		; exec code rounded up to next para
	shr	ax,cl		; (ax) = paras for exec code

	mov	dx,elength	; new env length (in bytes)
	add	dx,0fh		; env rounded up to next para
	shr	dx,cl		; (dx) = paras for new env

	pop	bx		; restore the maximum length to bx

;	ax = exec code + data size
;	dx = environment size
;	bx = PSP contiguous size

	sub	bx,dx		; reduce PSP by environment segment
	jbe	ov

	sub	bx,2		; reduce by 2 (arena header + DOS 4.0 slop)
	jc	ov

;	bx = PSP contiguous size

	mov	cx,siz		; cx = assumed .exe size

	cmp	flag,0		; .exe or .com?
	je	join1		;   was .exe

	mov	cx,fsiz		; size of .com child in paras
	inc	cx		; plus para for stack
	jz	ov

l	join1
	add	cx,10h		; child must have a PSP
	jc	ov

;	ax = exec code + data size
;	cx = minimum size of child
;	bx = PSP contiguous size

	cmp	bx,cx		; enough for child?
	jb	ov		;   no

	push	ax		; save exec code size
	push	dx		; save env size

;	allocate and link all free memory in the system
;	assume the arena is good here and all errors are not enough memory

	xor	cx,cx		; cx = free link

l	allocloop
	mov	bx,1		; allocate 1 paragraph block
	callos	allocmem
	jc	allocall	;   all linked up

	mov	es,ax		; es = segment to allocate
	mov	bx,-1		; bx = maximum request
l	allocretry
	callos	setmem		; grow segment
	jc	allocretry	;  force it to be allocated

	mov	es:[0],cx	; save last free block
	mov	cx,es		; cx = current free link
	mov	dx,bx		; dx = size of last block
	jmp	allocloop	; keep allocating

l	allocall
	mov	bx,dx

;	es = cx = free segment linked list
;	bx = size of last free block

	cmp	[_osmajor],2	; check for DOS 2.0
	jne	allocenv	;   no - just try allocing environment

	sub	bx,DOS2CMD+1	; size to cut back
	jnc	shrinkdos2

;-----	release all "free" blocks because of error

l	freefree 		; free all linked up "free" memory
	jcxz	allfree
	mov	es,cx
	mov	cx,es:[0]	; next free block
	callos	freemem
	jnc	freefree	; keep freeing linked blocks

l	allfree			; if freemem error, just give up
	jmp	ov		; fail with no memory error

;-----

l	shrinkdos2
	callos	setmem
	jc	freefree	; any error at this point is no memory

;	es = cx = free segment linked list
;	bx = size of last free block

l	allocenv
	pop	dx		; dx = environment size
	push	dx
	inc	dx
	inc	dx		; dx = envsize + 2 (for arena header + DOS 4.0)
	sub	bx,dx
	jnc	haveenv		; have space in this block for environment

;	no space for environment in this block - try end of PSP contiguous area

	mov	ax,[freepsp]	; ax = last free segment in PSP-contiguous area
	or	ax,ax
	jz	tryotherfree	;   none here - try other free blocks

	inc	ax		; convert arena address to segment address
	mov	es,ax
	mov	bx,-1
	callos	setmem		; find size of segment
	cmp	al,E_nomem
	jne	freefree	;   must be something like arena trashed
	sub	bx,dx		; enough room?
	jnc	haveenv		;   yes - do actual environment allocation
				; else go try somewhere else

;	try other free blocks if above attempts fail

l	tryotherfree
	jcxz	allfree
	mov	ax,cx
;
;	dx = environment size in paragraphs + 2 (for arena hdr + DOS 4.0 slop)
;	cx = pointer to beginning of "free" list (segment address)
;	ax = pointer into "free" list (segment address)
;

l	tryotherloop
	dec	ax		;[1] AX = header address (temporarily)
	mov	es,ax		;    header of current free block
	mov	bx,es:[asiz]	;    size of current free block (in header)
	inc	ax		;[1] convert header addr back into seg addr
	mov	es,ax		;[1] AX = ES = segment address
	sub	bx,dx		; will environment fit?
	jnc	haveenv		;   yes - do actual environment allocation
	mov	ax,es:[0]	; else go on to next free block
	or	ax,ax		; end of list?
	jz	freefree	;   yes - not enough memory; clean up and fail
	jmp	short tryotherloop ; keep trying

;	es = segment to shrink for environment
;	bx = size to which to shrink chosen "free" segment

l	haveenv
	callos	setmem
	jc	freefree	; any error at this point is no memory

	pop	bx
	callos	allocmem	; allocate environment segment
	jc	freefree	; any error at this point is no memory

;	ax = environment segment

	mov	es,[_psp]
	mov	bx,es
	mov	es:[DOS_envp],ax ; new env segment
	mov	es,ax
	xor	di,di		; es:di points to new env

if	sizeD
	push	ds		; save DGROUP
	lds	si,envblock
else
	mov	si,envblock
endif
	mov	cx,elength
	rep	movsb		; move env to new segment
if	sizeD
	pop	ds		; restore DGROUP
endif

	mov	dx,es		; dx = environment segment

;	bx = PSP
;	dx = environment segment

	push	dx		; save needed registers
	push	bx

	cmp	_sigintseg,0	; has ^C been hooked ?
	je	letsgo		; no, do the exec

	push	ds
	mov	dx,_sigintoff	; load system default address
	mov	ds,_sigintseg	; load system default segment
	mov	ax,DOS_setvector shl 8 + 23H ; reset ^C vector (INT 23H)
	callos
	pop	ds		; restore registers
l	letsgo


;
;	IMPORTANT NOTE:
;
;	DOS 3.30 and later allow the user to increase the size of the
;	file handle table.  If the file handle count > 20, then a far
;	segment is allocated to store the table.  This segment must be
;	freed up before the child program is exec-ed.  This code relies
;	on the fact that DOS will free up that far segment if the handle
;	count is set back to 20.  If DOS ever changes, this code will fail.
;
	mov	ax,word ptr [_osmajor]
	xchg	ah,al		; AH = _osmajor, AL = _osminor
	cmp	ax,(3 shl 8) + 30
	jb	pre_DOS330	; _osmajor:_osminor < 3:30?

	mov	bx,20		; IF >= DOS 3.30 THEN
	mov	ah,67h		; Set Handle Count on principle
	callos			; set number of handles to default value
pre_DOS330:

	pop	bx
	pop	dx

;	WARNING - any errors past this point - must go to execpanic

;	now free all blocks of memory belonging to this process
;	except the PSP and new environment block

;	bx = PSP
;	dx = environment segment

l	freemore
	push	bx		; save PSP
	mov	ah,52h		; magic DOS call
	int	21h
	mov	ax,es:[bx-2]	; ax = first arena segment!!!
	pop	bx		; restore PSP

;	ax = current segment to check

l	findused
	mov	es,ax
	inc	ax
	cmp	ax,bx		; is it PSP?
	je	atend		;   yes - skip
	cmp	ax,dx		; is it environment?
	je	atend		;   yes - skip

	cmp	es:[own],bx	; do we own it?
	jne	atend		;   no -skip

	mov	es,ax		; es = must be one past arena header
	callos	freemem		; free up the segment
	jmp	freemore	;   and start over - even if error

l	atend
	add	ax,es:[asiz]	; add in segment size
	jc	freemore	;   error - changed arena - retry
	cmp	es:[sig],'Z'	; last segment?
	jne	findused	;   no - check next segment

;	increase size of PSP to maximum amount available
;
;	bx = PSP

	mov	es,bx		; es= PSP
	mov	bx,-1
	callos	setmem
	callos	setmem		; allocates on second try
	jnc	bigPSP

l	execpanic		; error in exec after memory freed
	mov	dx,word ptr [emsg]	;[1] DS:DX = "Not enough memory"
	mov	ax,word ptr [emsgseg]	;[1]
	mov	ds,ax		;[1]
	callos	message
	mov	ax,DOS_terminate shl 8 + 255
	callos			; error code = 255

l	bigPSP
	mov	ax,es
	add	ax,bx		; bx = size of PSP block
	mov	es:[DOS_maxpara],ax ; set new memory limit in PSP

;	ax = top of PSP segment
;	((sp)) = (exec code size)

	pop	cx		; cx = exec code size
	sub	bx,cx		; reduce PSP size
	jc	execpanic
	sub	ax,cx
	mov	word ptr [target+2],ax ; set up final jump address

;	move the exec code and its data up to hi mem
;
;	ax = exec code segment

	mov	es,ax		; es = exec code segment
	push	es		; save exec code segment

	push	ds		; save DGROUP

	push	cs
	pop	ds		; ds points into code segment
	xor	di,di
	mov	cx,BYTE PTR exesz ;[1] assume exe file
	mov	si,codeOFFSET a
	cmp	flag,0		; .exe or .com?
	je	exe4		;   .exe
	mov	cx,BYTE PTR comsz ;[1]
	mov	si,codeOFFSET b

l	exe4
	rep	movsb		; move code to exec code segment

	pop	ds		; restore DGROUP

	push	ds		;[1] save DGROUP
	mov	cx,[emsgln]	;[1] count of bytes to move
	mov	si,word ptr [emsg]	;[1] DS:SI = start of 3 error messages
	mov	ax,word ptr [emsgseg]	;[1]
	mov	ds,ax		;[1]
	rep	movsb		;[1] move 3 error messages
	pop	ds		;[1] restore DGROUP

	mov	bx,di		; (bx) = offset of data/name (save)
	cmp	flag,0		; .exe or .com?
	jne	com5		;   .com

; for .exe save initial register values

	mov	ax,initss
	stosw
	mov	ax,initsp
	stosw
	mov	ax,initcs
	stosw
	mov	ax,initip
	stosw
l	com5

if	sizeD
	push	ds		; save DGROUP
	lds	si,nam
else
	mov	si,nam
endif

	mov	cx,nlength
	rep	movsb		; move name

if	sizeD
	pop	ds		; restore DGROUP
endif

; exec code and its data are in hi mem. set the dma and fix the psp (the
; command line at offset 80h, and the fcbs)

	mov	es,[_psp]	; psp seg

; set the dma to psp:80h

	push	ds		; save DGROUP
	push	es
	pop	ds		; psp segment
	mov	dx,80h		; default dma=psp:80h
	callos	setdma
	pop	ds		; restore DGROUP

; set up the command line

	mov	di,DOS_cmdline

if	sizeD
	push	ds		; save DGROUP
	lds	si,command
else
	mov	si,command
endif
	mov	cl,[si]		; length of command
	inc	cx		; (ch) = 0 from last rep movsb (name)
	inc	cx		; count byte & terminating <cr>
	rep	movsb		; move the command line into psp:80h

; set up the fcbs

	mov	dx,bx		; (dx) = data/name offset

	mov	di,FCB1		; fcb at offset 5ch
	xchg	ax,cx		; null byte to store
				; (cx) = 0 from last rep movsb (command line)
	mov	cx,20h		; length of 2 fcbs
	rep	stosb		; zero the fcbs
	mov	di,FCB1		; fcb at offset 5ch

if	sizeD
	lds	si,command
else
	mov	si,command
endif
	inc	si		; si points to arg1 (or space before it)
	mov	ax,DOS_fcbparse shl 8 + 1
	callos			; parse filename
	cmp	al,0ffh		; see if invalid drive letter
	je	bad1
	xor	al,al
l	bad1
	mov	bl,al		; first drive letter
	mov	di,FCB2		; second fcb in psp
	mov	ax,DOS_fcbparse shl 8 + 1
	callos			; parse filename
	cmp	al,0ffh		; see if invalid drive letter
	je	bad2
	xor	al,al
l	bad2
	mov	bh,al		; bx = drive letter flags

if	sizeD
	pop	ds		; restore DGROUP
endif

; setup stack in exec code segment

	pop	cx		; exec code segment
	mov	si,dx		; data/name offset in exec code
	add	si,STKSIZ	; push it up to produce a stack
	cmp	flag,0		; .exe or .com? (last frame reference)
	mov	bp,bx		; initial ax
	cli			; disable interrupts until stack set up
	mov	ss,cx
	mov	sp,si
	sti			; temporary stack set up

	jne	join6		;   was .com

	push	dx		; save data offset to init. register values
	add	dx,8		; name offset in hi mem

l	join6

	mov	ax,es		; (ax) = paras from 0:0 to psp:0
	add	ax,10h		; (ax) = paras from 0:0 to parameter block
	mov	bx,100h		; psp:100h is parameter block for load/exec
	mov	es:[bx],ax	; word segment address to load at
	mov	es:[bx+2],ax	; word relocation factor to be applied
	push	ax		; save relocation factor
	mov	ax,DOS_exec shl 8 + 3

;	(ax) = dos code for load/exec (4b03h)
;	(dx) = name offset in exec segment
;	(es:bx) = psp:100 (parameter block)
;	(ds) = DGROUP seg
;	(bp) = initial ax
;
;	di and si are free

	jmp	dword ptr [target] ; and away we go!

;-----------------------------------------------------------------------------

NOMEM	equ	5
BADFORM	equ	6
BADENV	equ	7

execom	macro	lab,fil
l	lab
	push	cs
	pop	ds		; DS = exec code segment

	callos			; overlay parent with child
	jnc	allok&lab

	mov	bx,BADFORM - NOMEM
	cmp	al,E_ifunc	; invalid format
	je	die&lab

	mov	bl,BADENV - NOMEM
	cmp	al,E_badenv	; bad environment
	je	die&lab

	mov	bl,BADFORM - NOMEM
	cmp	al,E_badfmt	; bad format
	je	die&lab

	xor	bx,bx		; NOMEM - NOMEM

l	die&lab
	push	cs
	pop	es		; es = exec code segment

	xor	ax,ax
	mov	cx,-1
	mov	di,BYTE PTR fil&sz	;[2]
	or	bx,bx
	jz	outmsg&lab
	jmp	short nmsg&lab


;	Warning if you change this section above you must
;	leave the following bytes for DOS 2.0 to walk all
;	over without walking on data DOS walks on DS:2E to
;	DS:31  extra slop just incase!

	org	lab + 32h
l	nmsg&lab
	repne	scasb
	inc	di		; skip past message number
	inc	di
	dec	bx
	jnz	nmsg&lab

l	outmsg&lab
	mov	dx,di
	callos	message
	mov	ax,DOS_terminate shl 8 + 255
	callos			; terminate (255)

l	allok&lab
	endm


;-----------------------------------------------------------------------------
;
;	.exe  exec code

	execom	a,exe		; expand for .exe file

	pop	di		; relocation factor
	pop	si		; data offset of initial register values

	lodsw			; initss
	add	ax,di		; relocation factor (paras) from 0:0
	xchg	dx,ax
	lodsw			; initsp
	cli
	mov	ss,dx		; reloc'd ss
	mov	sp,ax
	sti

	lodsw			; initcs
	add	di,ax
	lodsw
	push	di		; save INIT CS
	push	ax		; save INIT IP

	push	es
	pop	ds		; es = ds = PSP
	mov	ax,bp		; ax = valid drive letter flags

xglobal	proc	far
	ret			; force far return to dos int 21h handler
xglobal	endp

z:

;-----------------------------------------------------------------------------
;
;	.com  exec code

	execom	b,com		; expand for .com file

	pop	di		; burn relocation factor (not used)
	mov	bx,cs		; get exec code segment
	mov	ax,es		; get PSP segment
	sub	bx,ax		; bx = size of program area (in paragraphs)

	mov	cl,4
	test	bx,0f000h	; will we lost precision when we shift?
	jz	doit
	mov	bx,1000h	;   yes, set ss:sp = psp:0000
doit:
	shl	bx,cl		; make byte offset from psp:0
	dec	bx
	dec	bx
	cli			; disable interrupts until stack's ok
	mov	ss,ax
	mov	sp,bx		; reserved para offset (stack grows down)
	sti			; enable interrupts; stack's ok now
	mov	word ptr ss:[bx],0 ; put 0000 on top of stack for return

	push	es
	mov	ax,100h
	push	ax		; INIT CS:IP = PSP:100h
	push	es
	pop	ds		; DS = ES = CS = PSP
	mov	ax,bp		; ax = valid drive letter flags

cglobal	proc	far
	ret			; force far return
cglobal	endp

y:

cEnd	nogen


sEnd
	end
