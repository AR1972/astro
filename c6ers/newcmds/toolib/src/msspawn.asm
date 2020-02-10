;
;	--- Version 3.0 91-05-27 17:56 ---
;
;	SPAWN.ASM - Main function for memory swapping spawn call.
;
;	Public Domain Software written by
;		Thomas Wagner
;		Ferrari electronic GmbH
;		Beusselstrasse 27
;		D-1000 Berlin 21
;		Germany
;
;
; Assemble with
;
; ?asm  spawn;		  		- C, default model (small)
; ?asm  /DMODL=large spawn  		- C, large model
;
;	NOTE:	For C, change the 'model' directive below according to your
;		memory model, or define MODL=xxx on the command line.
;
;
; Main function:
;
;   	int do_spawn (int swapping,
;		      char *execfname, 
;		      char *cmdtail,
;		      unsigned envlen, 
;		      char *envp)
;
;   Parameters:
;
;	swapping - swap/spawn/exec function:
;			< 0: Exec, don't swap
;		  	  0: Spawn, don't swap
;			> 0: Spawn, swap
;			     in this case, prep_swap must have 
;			     been called beforehand (see below).
;
;	cmdtail - command tail for EXEC.
;
;	execfname - name and path of file to execute.
;
;	envlen - length of environment copy (may be 0).
;
;	envp -  pointer to environment block (must be aligned on
;		paragraph boundary). Unused if envlen is 0.
;
;   Returns:
;	0000..00ff:	Returncode of EXECed program
;	03xx:		DOS-Error xx calling EXEC
;	0500:		Swapping requested, but prep_swap has not 
;			been called or returned an error
;	0501:		MCBs don't match expected setup
;	0502:		Error while swapping out
;
;
; For swapping, the swap method must be prepared before calling do_spawn.
;
;	int prep_swap (unsigned method, char *swapfname)
;
;   Parameters:
;
;	method	- bit-map of allowed swap devices:
;			01 - Allow EMS
;			02 - Allow XMS
;			04 - Allow File swap
;			10 - Try XMS first, then EMS
;			40 - Create file as "hidden"
;			80 - Use "create temp" call for file swap
;		       100 - Don't preallocate file
;		       200 - Check for Network, don't preallocate if net
;		      4000 - Environment block will not be swapped
;
;	swapfname - swap file name (may be undefined if the
;		    "method" parameters disallows file swap).
;
;   Returns:
;
;   	A positive integer on success:
;		1 - EMS swap initialized
;		2 - XMS swap initialized
;		4 - File swap initialized
;	A negative integer on failure:
;		-1 - Couldn't allocate swap space
;		-2 - The spawn module is located too low in memory
;
;
	IFDEF	?LARGE
	.model	large,c
	%out	  large model
	ELSE
	IFDEF	?MEDIUM
	.model	medium,c
	%out	  medium model
	ELSE
	.model	small,c
	%out	  small model
	ENDIF
	ENDIF
;
ptrsize	=	@DataSize
;
	extrn	_psp: word
;
	public	do_spawn
	public	prep_swap
;
stacklen	=	256		; local stack
;
;	"ems_size" is the EMS block size: 16k.
;
ems_size	=	16 * 1024	; EMS block size
ems_parasize	=	ems_size / 16	; same in paragraphs
ems_shift	=	10		; shift factor for paragraphs
ems_paramask	=	ems_parasize-1	; block mask
;
;	"xms_size" is the unit of measurement for XMS: 1k
;
xms_size	=	1024		; XMS block size
xms_parasize	=	xms_size / 16	; same in paragraphs
xms_shift	=	6		; shift factor for paragraphs
xms_paramask	=	xms_parasize-1	; block mask
;
;	Method flags
;
USE_EMS		=	01h
USE_XMS		=	02h
USE_FILE	=	04h
XMS_FIRST	=	10h
HIDE_FILE	=	40h
CREAT_TEMP	=	80h
NO_PREALLOC	=	100h
CHECK_NET	=	200h
DONT_SWAP_ENV	=	4000h
;
;	Return codes
;
RC_TOOLOW	=	0102h
RC_BADPREP	=	0500h
RC_MCBERROR	=	0501h
RC_SWAPERROR	=	0502h
;
EMM_INT		=	67h
;
;	The EXEC function parameter block
;
exec_block	struc
envseg	dw	?		; environment segment
ppar	dw	?		; program parameter string offset
pparseg	dw	?		; program parameter string segment
fcb1	dw	?		; FCB offset
fcb1seg	dw	?		; FCB segment
fcb2	dw	?		; FCB offset
fcb2seg	dw	?		; FCB segment
exec_block	ends
;
;	Structure of an XMS move control block
;
xms_control	struc
lenlo		dw	?	; length to move (doubleword)
lenhi		dw	?
srchnd		dw	?	; source handle (0 for standard memory)
srclo		dw	?	; source address (doubleword or seg:off)
srchi		dw	?
desthnd		dw	?	; destination handle (0 for standard memory)
destlo		dw	?	; destination address (doubleword or seg:off)
desthi		dw	?
xms_control	ends
;
;	The structure of the start of an MCB (memory control block)
;
mcb		struc
id		db	?
owner		dw	?
paras		dw	?
mcb		ends
;
;	The structure of an internal MCB descriptor.
;	CAUTION: This structure is assumed to be no larger than 16 bytes
;	in several places in the code, and to be exactly 16 bytes when
;	swapping in from file. Be careful when changing this structure.
;
mcbdesc		struc
addr		dw	?	; paragraph address of the MCB
msize		dw	?	; size in paragraphs (excluding header)
swoffset	dw	?	; swap offset (0 in all blocks except first)
swsize		dw	?	; swap size (= msize + 1 except in first)
num_follow	dw	?	; number of following MCBs
		dw	3 dup(?) ; pad to paragraph (16 bytes)
mcbdesc		ends
;
;	The variable block set up by prep_swap
;
prep_block	struc
xmm		dd	?		; XMM entry address
first_mcb	dw	?		; Segment of first MCB
psp_mcb		dw	?		; Segment of MCB of our PSP
env_mcb		dw	?		; MCB of Environment segment
noswap_mcb	dw	?		; MCB that may not be swapped
ems_pageframe	dw	?		; EMS page frame address
handle		dw	?		; EMS/XMS/File handle
total_mcbs	dw	?		; Total number of MCBs
swapmethod	db	?		; Method for swapping
swapfilename	db	81 dup(?)	; Swap file name if swapping to file
prep_block	ends
;
;----------------------------------------------------------------------
;
;	Since we'll be moving code and data around in memory,
;	we can't address locations in the resident block with
;	normal address expressions. MASM does not support
;	defining variables with a fixed offset, so we have to resort
;	to a kludge, and define the shrunk-down code as a structure.
;	It would also be possible to use an absolute segment for the
;	definition, but this is not supported by the Turbo Pascal linker.
;
;	All references to low-core variables from low-core itself 
;	are made through DS, so we define a text macro "lmem" that 
;	expands to "ds:". When setting up low core from the normal
;	code, ES is used to address low memory, so this can't be used.
;
lmem	equ	<ds:>
;
;	The memory structure for the shrunk-down code, excluding the
;	code itself. The code follows this block.
;
parseg		struc
		db	2ch dup(?)
psp_envptr	dw	?
		db	5ch-2eh dup(?)	; start after PSP
;
save_ss		dw	?		; 5C - saved global ss
save_sp		dw	?		; 5E - saved global sp
xfcb1		db	16 dup(?)	; 60..6F - default FCB
xfcb2		db	16 dup(?)	; 70..7F - default FCB
zero		dw	?		; 80 Zero command tail length (dummy)
;
expar		db	TYPE exec_block dup (?) ; exec-parameter-block
spx		dw	?		; saved local sp
div0_off	dw	?		; divide by zero vector save
div0_seg	dw	?
filename	db	82 dup(?)	; exec filename
progpars	db	128 dup(?)	; command tail
		db	stacklen dup(?)	; local stack space
mystack		db	?
lprep		db	TYPE prep_block dup(?)	; the swapping variables
lcurrdesc	db	TYPE mcbdesc dup(?)	; the current MCB descriptor
lxmsctl		db	TYPE xms_control dup(?)
eretcode	dw	?		; EXEC return code
retflags	dw	?		; EXEC return flags
cgetmcb		dw	?		; address of get_mcb
;
parseg	ends
;
param_len	=	((TYPE parseg + 1) / 2) * 2	; make even
codebeg		=	param_len
;
	.code
;
;------------------------------------------------------------------------
;
lowcode_begin:
;
;       The following parts of the program code will be moved to
;	low core and executed there, so there must be no absolute 
;	memory references.
;	The call to get_mcb must be made indirect, since the offset
;	from the swap-in routine to get_mcb will not be the same
;	after moving.
;
;
;	get_mcb allocates a block of memory by modifying the MCB chain
;	directly.
;
;	On entry, lcurrdesc has the mcb descriptor for the block to
;		  allocate.
;
;	On exit,  Carry is set if the block couldn't be allocated.
;
;	Uses 	AX, BX, CX, ES
;	Modifies lprep.first_mcb
;
get_mcb	proc	near
;
	mov	ax,lmem lprep.first_mcb
	mov	bx,lmem lcurrdesc.addr
;
getmcb_loop:
	mov	es,ax
	cmp	ax,bx
	ja	gmcb_abort		; halt if MCB > wanted
	je	mcb_found		; jump if same addr as wanted
	add	ax,es:paras		; last addr
	inc	ax			; next mcb
	cmp	ax,bx
	jbe	getmcb_loop		; Loop if next <= wanted
;
;
;	The wanted MCB starts within the current MCB. We now have to
;	create a new MCB at the wanted position, which is initially
;	free, and shorten the current MCB to reflect the reduced size.
;
	cmp	es:owner,0
	jne	gmcb_abort		; halt if not free
	mov	bx,es			; current
	inc	bx			; + 1 (header doesn't count)
	mov	ax,lmem lcurrdesc.addr
	sub	ax,bx			; paragraphs between MCB and wanted
	mov	bx,es:paras		; paras in current MCB
	sub	bx,ax			; remaining paras
	dec	bx			; -1 for header
	mov	es:paras,ax		; set new size for current
	mov	cl,es:id		; old id
	mov	es:id,4dh		; set id: there is a next
	mov	ax,lmem lcurrdesc.addr
	mov	es,ax
	mov	es:id,cl		; and init to free
	mov	es:owner,0
	mov	es:paras,bx
;
;	We have found an MCB at the right address. If it's not free,
;	abort. Else check the size. If the size is ok, we're done 
;	(more or less).
;
mcb_found:
	mov	es,ax
	cmp	es:owner,0
	je	mcb_check		; continue if free
;
gmcb_abort:
	stc
	ret
;
mcb_check:
	mov	ax,es:paras		; size
	cmp	ax,lmem lcurrdesc.msize	; needed size
	jae	mcb_ok			; ok if enough space
;
;	If there's not enough room in this MCB, check if the next
;	MCB is free, too. If so, coalesce both MCB's and check again.
;
	cmp	es:id,4dh
	jnz	gmcb_abort		; halt if no next
	push	es			; save current
	mov	bx,es
	add	ax,bx
	inc	ax			; next MCB
	mov	es,ax
	cmp	es:owner,0		; next free ?
	jne	gmcb_abort		; halt if not
	mov	ax,es:paras		; else load size
	inc	ax			; + 1 for header
	mov	cl,es:id		; and load ID
	pop	es			; back to last MCB
	add	es:paras,ax		; increase size
	mov	es:id,cl		; and store ID
	jmp	mcb_check		; now try again
;
;	The MCB is free and large enough. If it's larger than the
;	wanted size, create another MCB after the wanted.
;
mcb_ok:
	mov	bx,es:paras
	sub	bx,lmem lcurrdesc.msize
	jz	mcb_no_next		; ok, no next to create
	push	es
	dec	bx			; size of next block
	mov	ax,es
	add	ax,lmem lcurrdesc.msize
	inc	ax			; next MCB addr
	mov	cl,es:id		; id of this block
	mov	es,ax			; address next
	mov	es:id,cl		; store id
	mov	es:paras,bx		; store size
	mov	es:owner,0		; and mark as free
	pop	es			; back to old MCB
	mov	es:id,4dh		; mark next block present
	mov	ax,lmem lcurrdesc.msize	; and set size to wanted
	mov	es:paras,ax
;
mcb_no_next:
	mov	es:owner,cx		; set owner to current PSP
;
;	Set the 'first_mcb' pointer to the current one, so we don't
;	walk through all the previous blocks the next time.
;	Also, check if the block we just allocated is the environment
;	segment of the program. If so, restore the environment pointer
;	in the PSP.
;
	mov	ax,es
	mov	lmem lprep.first_mcb,ax
	cmp	lmem lprep.env_mcb,ax
	jne	getmcb_finis
	inc	ax
	mov	lmem psp_envptr,ax
;
getmcb_finis:
	clc
	ret				; all finished (whew!)
;
get_mcb	endp
;
;
ireti:
	iret
;
;
;	The actual EXEC call.
;	Registers on entry:
;		BX	= paragraphs to keep (0 if no swap)
;		CX 	= length of environment to copy (words) or zero
;		DS:SI	= environment source
;		ES:DI	= environment destination
;		(ES = our low core code segment)
;
;
;	copy environment buffer down if present
;
doexec:
	jcxz	noenvcpy
	rep movsw
;
noenvcpy:
	push	es			; DS = ES = low core = PSP
	pop	ds
	or	bx,bx
	jz	no_shrink
;
;	first, shrink the base memory block down.
;
        mov	ah,04ah
	int     21h                     ; resize memory block
;
;	Again walk all MCBs. This time, all blocks owned by the 
;	current process are released.
;
	mov	si,lmem lprep.first_mcb
	or	si,si
	jz	no_shrink
	mov	dx,lmem lprep.psp_mcb
	mov	bx,dx
	inc	bx			; base PSP (MCB owner)
	mov	di,lmem lprep.noswap_mcb
;
free_loop:
	cmp	si,dx
	je	free_next		; don't free base block
	cmp	si,di
	je	free_next
	mov	es,si
	cmp	bx,es:owner		; our process?
	jne	free_next		; next if not
	cmp	si,lmem lprep.env_mcb	; is this the environment block?
	jne	free_noenv
	mov	ds:psp_envptr,0		; else clear PSP pointer
;
free_noenv:
	inc	si
	mov	es,si
	dec	si
	mov	ah,049h			; free memory block
	int	21h
;
free_next:
	mov	es,si
	cmp	es:id,4dh		; normal block?
	jne	free_ready		; ready if end of chain
	add	si,es:paras		; start + length
	inc	si			; next MCB
	jmp	free_loop
;
free_ready:
	mov	ax,ds
	mov	es,ax
;
no_shrink:
	mov	dx,filename		; params for exec
	mov	bx,expar
	mov	ax,04b00h
	int	21h			; exec
;
;	Return from EXEC system call. Don't count on any register except
;	CS to be restored (DOS 2.11 and previous versions killed all regs).
;
	mov	bx,cs
	mov	ds,bx
	mov	es,bx
	mov	ss,bx
	mov	sp,lmem spx
	cld
	mov	lmem eretcode,ax	; save return code
	pushf
	pop	bx
	mov	lmem retflags,bx	; and returned flags
;
	cmp	lmem lprep.swapmethod,0
	je	exec_memok
	jg	exec_expand
;
;	Terminate.
;
	test	bx,1			; carry?
	jnz	exec_term		; use EXEc retcode if set
	mov	ah,4dh			; else get program return code
	int	21h
;
exec_term:
	mov	ah,4ch
	int	21h
;
;
exec_expand:
	mov	ah,4ah			; expand memory
	mov	bx,lmem lcurrdesc.msize
	int	21h
	jnc	exec_memok
	mov	ax,4cffh
	int	21h			; terminate on error
;
;	Swap memory back
;
	nop
;
exec_memok:
;
;	FALL THROUGH to the appropriate swap-in routine
;
;
getmcboff	=	offset get_mcb - offset lowcode_begin
iretoff		=	offset ireti - offset lowcode_begin
doexec_entry	=	offset doexec - offset lowcode_begin
base_length	=	offset $ - offset lowcode_begin
;
;-----------------------------------------------------------------------
;
;	The various swap in routines follow. Only one of the routines
;	is copied to low memory.
;	Note that the routines are never actually called, the EXEC return
;	code falls through. The final RET thus will return to the restored
;	memory image.
;
;	On entry, DS must point to low core.
;	On exit to the restored code, DS is unchanged.
;
;
;	swapin_ems:	swap in from EMS.
;
swapin_ems	proc	far
;
	xor	bx,bx
	mov	si,ems_parasize
	mov	dx,lmem lprep.handle	; EMS handle
;
swinems_main:
	push	ds
	mov	cx,lmem lcurrdesc.swsize	; block length in paras
	mov	di,lmem lcurrdesc.swoffset	; swap offset
	mov	es,lmem lcurrdesc.addr		; segment to swap
	mov	ds,lmem lprep.ems_pageframe	; page frame address
;
	mov	ax,ems_parasize		; max length
	sub	ax,si			; minus current offset
	jnz	swinems_ok		; go copy if nonzero
;
swinems_loop:
	mov	ax,4400h		; map in next page
	int	EMM_INT
	or	ah,ah
	jnz	swinems_error
	mov	si,0			; reset offset
	inc	bx			; bump up page number
	mov	ax,ems_parasize		; max length to copy
;
swinems_ok:
	cmp	ax,cx			; length to copy
	jbe	swinems_doit		; go do it if <= total length
	mov	ax,cx			; else use total length
;
swinems_doit:
	sub	cx,ax			; subtract copy length from total
	push	cx			; and save
	push	ax			; save the copy length in paras
	push	si
	push	di
	mov	cl,3
	shl	ax,cl			; convert to number of words (!)
	inc	cl
	shl	si,cl			; convert to byte address
	mov	cx,ax
	rep movsw
	pop	di
	pop	si
	pop	cx			; copy length in paras
	mov	ax,es
	add	ax,cx			; add copy length to dest segment
	add	si,cx			; and EMS page offset
	mov	es,ax
	pop	cx			; remaining length
	or	cx,cx			; did we copy everything?
	jnz	swinems_loop		; go loop if not
;
	pop	ds
	cmp	lmem lcurrdesc.num_follow,0	; another MCB?
	je	swinems_complete	; exit if not
;
;	Another MCB follows, read next mcb descriptor into currdesc
;
	cmp	si,ems_parasize
	jb	swinems_nonewpage	; no new block needed
	mov	ax,4400h		; map page, phys = 0
	int	EMM_INT
	or	ah,ah
	jnz	swinems_error1
	mov	si,0
	inc	bx
;
swinems_nonewpage:
	push	si
	push	ds
	mov	ax,ds
	mov	es,ax
	mov	ds,lmem lprep.ems_pageframe	; page frame address
	mov	cl,4
	shl	si,cl			; convert to byte address
	mov	cx,TYPE mcbdesc
	mov	di,lcurrdesc
	rep movsb
	pop	ds
	pop	si
	inc	si			; one paragraph
;
	push	bx
	call	lmem cgetmcb
	pop	bx
	jc	swinems_error1
	jmp	swinems_main
;
swinems_complete:
	mov	ah,45h			; release EMS pages
	int	EMM_INT
	ret
;
swinems_error:
	pop	ds
swinems_error1:
	mov	ah,45h			; release EMS pages on error
	int	EMM_INT
	mov	ax,4cffh
	int	21h			; terminate
;
swapin_ems	endp
;
swinems_length	= offset $ - offset swapin_ems
;
;
;	swapin_xms:	swap in from XMS.
;
swapin_xms	proc	far
;
	mov	ax,lmem lprep.handle	; XMS handle
	mov	lmem lxmsctl.srchnd,ax 	; source is XMS
	mov	lmem lxmsctl.desthnd,0 	; dest is normal memory
	mov	lmem lxmsctl.srclo,0
	mov	lmem lxmsctl.srchi,0
;
swinxms_main:
	mov	ax,lmem lcurrdesc.swsize ; size in paragraphs
	mov	cl,4
	rol	ax,cl			; size in bytes + high nibble
	mov	dx,ax
	and	ax,0fff0h		; low word
	and	dx,0000fh		; high word
	mov	lmem lxmsctl.lenlo,ax	; into control block
	mov	lmem lxmsctl.lenhi,dx
	mov	ax,lmem lcurrdesc.swoffset	; swap offset
	mov	lmem lxmsctl.destlo,ax 		; into control block
	mov	ax,lmem lcurrdesc.addr		; segment to swap
	mov	lmem lxmsctl.desthi,ax
	mov	si,lxmsctl
	mov	ah,0bh
	call	lmem lprep.xmm		; move it
	or	ax,ax
	jz	swinxms_error
	mov	ax,lmem lxmsctl.lenlo	; adjust source addr
	add	lmem lxmsctl.srclo,ax
	mov	ax,lmem lxmsctl.lenhi
	adc	lmem lxmsctl.srchi,ax
;
	cmp	lmem lcurrdesc.num_follow,0	; another MCB?
	je	swinxms_complete
;
	mov	lmem lxmsctl.lenlo,TYPE mcbdesc
	mov	lmem lxmsctl.lenhi,0
	mov	lmem lxmsctl.desthi,ds
	mov	lmem lxmsctl.destlo,lcurrdesc
	mov	si,lxmsctl
	mov	ah,0bh
	call	lmem lprep.xmm		; move it
	or	ax,ax
	jz	swinxms_error
	add	lmem lxmsctl.srclo,16	; one paragraph
	adc	lmem lxmsctl.srchi,0
;
	call	lmem cgetmcb
	jc	swinxms_error
	jmp	swinxms_main
;
swinxms_complete:
	mov	ah,0ah			; release XMS frame
	mov	dx,lmem lprep.handle   	; XMS handle
	call	lmem lprep.xmm
	ret
;
swinxms_error:
	mov	ah,0ah			; release XMS frame on error
	call	lmem lprep.xmm
	mov	ax,4c00h
	int	21h
;
swapin_xms	endp
;
swinxms_length	= offset $ - offset swapin_xms
;
;
;	swapin_file:	swap in from file.
;
swapin_file	proc	far
;
	mov	dx,lprep.swapfilename
	mov	ax,3d00h			; open file
	int	21h
	jc	swinfile_error2
	mov	bx,ax				; file handle
;
swinfile_main:
	push	ds
	mov	cx,lmem lcurrdesc.swsize	; size in paragraphs
	mov	dx,lmem lcurrdesc.swoffset	; swap offset
	mov	ds,lmem lcurrdesc.addr		; segment to swap
;
swinfile_loop:
	mov	ax,cx
	cmp	ah,8h			; above 32k?
	jbe	swinfile_ok		; go read if not
	mov	ax,800h			; else read 32k
;
swinfile_ok:
	sub	cx,ax			; remaining length
	push	cx			; save it
	push	ax			; and save paras to read
	mov	cl,4
	shl	ax,cl			; convert to bytes
	mov	cx,ax
	mov	ah,3fh			; read
	int	21h
	jc	swinfile_error
	cmp	ax,cx
	jne	swinfile_error
	pop	cx			; paras read
	mov	ax,ds
	add	ax,cx			; bump up dest segment
	mov	ds,ax
	pop	cx			; remaining length
	or	cx,cx			; anything left?
	jnz	swinfile_loop		; go loop if yes
;
	pop	ds
	cmp	lmem lcurrdesc.num_follow,0	; another MCB?
	je	swinfile_complete	; ready if not
	mov	cx,16			; read one paragraph
	mov	dx,lcurrdesc
	mov	ah,3fh
	int	21h
	jc	swinfile_error1
	cmp	ax,cx
	jne	swinfile_error1
;
	push	bx
	call	lmem cgetmcb
	pop	bx
	jc	swinfile_error1
	jmp	swinfile_main
;
;
swinfile_complete:
	mov	ah,3eh			; close file
	int	21h
	mov	dx,lprep.swapfilename
	mov	ah,41h			; delete file
	int	21h
	ret
;
swinfile_error:
	pop	cx
	pop	cx
	pop	ds
swinfile_error1:
	mov	ah,3eh			; close file
	int	21h
swinfile_error2:
	mov	dx,lprep.swapfilename
	mov	ah,41h			; delete file
	int	21h
	mov	ax,4cffh
	int	21h
;
swapin_file	endp
;
swinfile_length	= offset $ - offset swapin_file
;
;
;	swapin_none:	no swap, return immediately.
;
swapin_none	proc	far
;
	ret
;
swapin_none	endp
;
;
	IF	swinems_length GT swinxms_length
swcodelen	=	swinems_length
	ELSE
swcodelen	=	swinxms_length
	ENDIF
	IF	swinfile_length GT swcodelen
swcodelen	=	swinfile_length
	ENDIF
;
swap_codelen	=	((swcodelen + 1) / 2) * 2
;
codelen		=	base_length + swap_codelen
reslen		=	codebeg + codelen
keep_paras	=	(reslen + 15) shr 4	; paragraphs to keep
swapbeg		=	keep_paras shl 4	; start of swap space
savespace	=	swapbeg - 5ch	; length of overwritten area
;
;--------------------------------------------------------------------
;
	.data?
;
;
;	Space for saving the part of the memory image below the
;	swap area that is overwritten by our code.
;
save_dat	db	savespace dup(?)
;
;	Variables used while swapping out.
;	The "prep" structure is initialized by prep_swap.
;
prep		prep_block	<>
nextmcb		mcbdesc		<>
currdesc	mcbdesc		<>
xmsctl		xms_control	<>
ems_curpage	dw		?	; current EMS page number
ems_curoff	dw		?	; current EMS offset (paragraph)
;
;--------------------------------------------------------------------
;       
	.code
;
;	swapout_ems:	swap out an MCB block to EMS.
;
;	Entry:	"currdesc" 	contains description of block to swap
;		"nextmcb"	contains MCB-descriptor of next block
;				if currdesc.num_follow is nonzero
;
;	Exit:	0 if OK, != 0 if error, Zero-flag set accordingly.
;
;	Uses:	All regs excpt DS
;
swapout_ems	proc	near
;
	push	ds
	mov	cx,currdesc.swsize	; block length in paras
	mov	si,currdesc.swoffset	; swap offset
	mov	dx,prep.handle		; EMS handle
	mov	bx,ems_curpage		; current EMS page
	mov	di,ems_curoff		; current EMS page offset (paras)
	mov	es,prep.ems_pageframe	; page frame address
	mov	ds,currdesc.addr	; segment to swap
;
	mov	ax,ems_parasize		; max length
	sub	ax,di			; minus current offset
	jnz	swems_ok		; go copy if there's room
;
swems_loop:
	mov	ax,4400h		; map in next page
	int	EMM_INT
	or	ah,ah
	jnz	swems_error
	mov	di,0			; reset offset
	inc	bx			; bump up page number
	mov	ax,ems_parasize		; max length to copy
;
swems_ok:
	cmp	ax,cx			; length to copy
	jbe	swems_doit		; go do it if <= total length
	mov	ax,cx			; else use total length
;
swems_doit:
	sub	cx,ax			; subtract copy length from total
	push	cx			; and save
	push	ax			; save the copy length in paras
	push	si
	push	di
	mov	cl,3
	shl	ax,cl			; convert to number of words (!)
	inc	cl
	shl	di,cl			; convert to byte address
	mov	cx,ax
	rep movsw
	pop	di
	pop	si
	pop	cx			; copy length in paras
	mov	ax,ds
	add	ax,cx			; add copy length to source segment
	add	di,cx			; and EMS page offset
	mov	ds,ax
	pop	cx			; remaining length
	or	cx,cx			; did we copy everything?
	jnz	swems_loop		; go loop if not
;
	pop	ds
	cmp	currdesc.num_follow,0	; another MCB?
	je	swems_complete		; exit if not
;
;	Another MCB follows, append nextmcb to save block.
;
	cmp	di,ems_parasize
	jb	swems_nonewpage		; no new block needed
	mov	ax,4400h		; map page, phys = 0
	int	EMM_INT
	or	ah,ah
	jnz	swems_error1
	mov	di,0
	inc	bx
;
swems_nonewpage:
	push	di
	mov	cl,4
	shl	di,cl			; convert to byte address
	mov	cx,TYPE mcbdesc
	mov	si,offset nextmcb
	rep movsb
	pop	di
	inc	di			; one paragraph
;
swems_complete:
	mov	ems_curpage,bx
	mov	ems_curoff,di
	xor	ax,ax
	ret
;
swems_error:
	pop	ds
swems_error1:
	mov	ah,45h			; release EMS pages on error
	int	EMM_INT
	mov	ax,RC_SWAPERROR
	or	ax,ax
	ret
;
swapout_ems	endp
;
;
;	swapout_xms:	swap out an MCB block to XMS.
;
;	Entry:	"currdesc" 	contains description of block to swap
;		"nextmcb"	contains MCB-descriptor of next block
;				if currdesc.num_follow is nonzero
;
;	Exit:	0 if OK, -1 if error, Zero-flag set accordingly.
;
;	Uses:	All regs excpt DS
;
swapout_xms	proc	near
;
	mov	ax,currdesc.swsize	; size in paragraphs
	mov	cl,4
	rol	ax,cl			; size in bytes + high nibble
	mov	dx,ax
	and	ax,0fff0h		; low word
	and	dx,0000fh		; high word
	mov	xmsctl.lenlo,ax		; into control block
	mov	xmsctl.lenhi,dx
	mov	xmsctl.srchnd,0		; source is normal memory
	mov	ax,currdesc.swoffset	; swap offset
	mov	xmsctl.srclo,ax		; into control block
	mov	ax,currdesc.addr	; segment to swap
	mov	xmsctl.srchi,ax
	mov	ax,prep.handle		; XMS handle
	mov	xmsctl.desthnd,ax
	mov	si,offset xmsctl
	mov	ah,0bh
	call	prep.xmm		; move it
	or	ax,ax
	jz	swxms_error
	mov	ax,xmsctl.lenlo		; adjust destination addr
	add	xmsctl.destlo,ax
	mov	ax,xmsctl.lenhi
	adc	xmsctl.desthi,ax
;
	cmp	currdesc.num_follow,0	; another MCB?
	je	swxms_complete
;
	mov	xmsctl.lenlo,TYPE mcbdesc
	mov	xmsctl.lenhi,0
	mov	xmsctl.srchi,ds
	mov	xmsctl.srclo,offset nextmcb
	mov	si,offset xmsctl
	mov	ah,0bh
	call	prep.xmm		; move it
	or	ax,ax
	jz	swxms_error
	add	xmsctl.destlo,16	; one paragraph
	adc	xmsctl.desthi,0
;
swxms_complete:
	xor	ax,ax
	ret
;
swxms_error:
	mov	ah,0ah			; release XMS frame on error
	mov	dx,prep.handle		; XMS handle
	call	prep.xmm
	mov	ax,RC_SWAPERROR
	or	ax,ax
	ret
;
swapout_xms	endp
;
;
;	swapout_file:	swap out an MCB block to file.
;
;	Entry:	"currdesc" 	contains description of block to swap
;		"nextmcb"	contains MCB-descriptor of next block
;				if currdesc.num_follow is nonzero
;
;	Exit:	0 if OK, -1 if error, Zero-flag set accordingly.
;
;	Uses:	All regs excpt DS
;
swapout_file	proc	near
;
	push	ds
	mov	cx,currdesc.swsize	; size in paragraphs
	mov	bx,prep.handle		; file handle
	mov	dx,currdesc.swoffset	; swap offset
	mov	ds,currdesc.addr	; segment to swap
;
swfile_loop:
	mov	ax,cx
	cmp	ah,8h			; above 32k?
	jbe	swfile_ok		; go write if not
	mov	ax,800h			; else write 32k
;
swfile_ok:
	sub	cx,ax			; remaining length
	push	cx			; save it
	push	ax			; and save paras to write
	mov	cl,4
	shl	ax,cl			; convert to bytes
	mov	cx,ax
	mov	ah,40h			; write
	int	21h
	jc	swfile_error
	cmp	ax,cx
	jne	swfile_error
	pop	cx			; paras written
	mov	ax,ds
	add	ax,cx			; bump up source segment
	mov	ds,ax
	pop	cx			; remaining length
	or	cx,cx			; anything left?
	jnz	swfile_loop		; go loop if yes
;
	pop	ds
	cmp	currdesc.num_follow,0	; another MCB?
	je	swfile_complete		; ready if not
	mov	cx,16			; write one paragraph
	mov	dx,offset nextmcb
	mov	ah,40h
	int	21h
	jc	swfile_error1
	cmp	ax,cx
	jne	swfile_error1
;
swfile_complete:
	xor	ax,ax
	ret
;
swfile_error:
	pop	cx
	pop	cx
	pop	ds
swfile_error1:
	mov	ah,3eh			; close file
	int	21h
	mov	dx,offset prep.swapfilename
	mov	ah,41h			; delete file
	int	21h
	mov	ax,RC_SWAPERROR
	or	ax,ax
	ret
;
swapout_file	endp
;
;--------------------------------------------------------------------------
;--------------------------------------------------------------------------
;
;
do_spawn	PROC	uses si di,swapping: word, execfname:ptr byte,params:ptr byte,envlen:word,envp:ptr byte
	local	datseg,pspseg,currmcb
;
	mov	datseg,ds		; save default DS
;
	mov	bx,_psp
	mov	pspseg,bx
;
;
;	Check if spawn is too low in memory
;
	mov	ax,cs
	mov	dx,offset lowcode_begin
	mov	cl,4
	shr	dx,cl
	add	ax,dx			; normalized start of this code
	mov	dx,keep_paras		; the end of the modified area
	add	dx,bx			; plus PSP = end paragraph
	cmp	ax,dx
	ja	doswap_ok	; ok if start of code > end of low mem
	mov	ax,RC_TOOLOW
	ret
;
doswap_ok:
	cmp	swapping,0
	jle	method_ok
;
;	check the swap method, to make sure prep_swap has been called
;
	mov	al,prep.swapmethod
	cmp	al,USE_EMS
	je	method_ok
	cmp	al,USE_XMS
	je	method_ok
	cmp	al,USE_FILE
	je	method_ok
	mov	ax,RC_BADPREP
	ret
;
;	Save the memory below the swap space.
;	We must do this before swapping, so the saved memory is
;	in the swapped out image.
;	Anything else we'd want to save on the stack or anywhere
;	else in "normal" memory also has to be saved here, any
;	modifications done to memory after the swap will be lost.
;
;	Note that the memory save is done even when not swapping,
;	because we use some of the variables in low core for
;	simplicity.
;
method_ok:
	push	ds
	pop	es
	push	ds
	mov	ds,pspseg		; DS points to PSP
	mov	si,5ch
	mov	di,offset save_dat
	mov	cx,savespace / 2	; NOTE: savespace is always even
	rep movsw
	pop	ds
;
	mov	ax,swapping
	cmp	ax,0
	jg	begin_swap
;
;	not swapping, prep_swap wasn't called. Init those variables in
;  	the 'prep' block we need in any case.
;
	mov	prep.swapmethod,al
	je	no_reduce
;
	mov	ax,pspseg
	dec	ax
	mov	prep.psp_mcb,ax
	mov	prep.first_mcb,ax
	inc	ax
	mov	es,ax
	mov	bx,es:psp_envptr
	mov	prep.env_mcb,bx
	mov	prep.noswap_mcb,0
	cmp	envlen,0
	jne	swp_can_swap_env
	mov	prep.noswap_mcb,bx
;
swp_can_swap_env:
	xor	bx,bx
	mov	es,bx
	mov	ah,52h			; get list of lists
	int	21h
	mov	ax,es
	or	ax,bx
	jz	no_reduce
	mov	es,es:[bx-2]		; first MCB
	cmp	es:id,4dh		; normal ID?
	jne	no_reduce
	mov	prep.first_mcb,es
;
no_reduce:
	jmp	no_swap1
;
;	set up first block descriptor
;
begin_swap:
	mov	ax,prep.first_mcb
	mov	currmcb,ax
	mov	es,prep.psp_mcb		; let ES point to base MCB
	mov	ax,es:paras
	mov	currdesc.msize,ax
	sub	ax,keep_paras
	mov	currdesc.swsize,ax
	mov	currdesc.addr,es
	mov	currdesc.swoffset,swapbeg + 16
;		NOTE: swapbeg is 1 para higher when seen from MCB
	mov	ax,prep.total_mcbs
	mov	currdesc.num_follow,ax
;
;	init other vars
;
	mov	xmsctl.destlo,0
	mov	xmsctl.desthi,0
	mov	ems_curpage,0
	mov	ems_curoff,ems_parasize
;
;	Do the swapping. Each MCB block (except the last) has an 
;	"mcbdesc" structure appended that gives location and size 
;	of the next MCB.
;
swapout_main:
	cmp	currdesc.num_follow,0	; next block?
	je	swapout_no_next		; ok if not
;
;	There is another MCB block to be saved. So we don't have
;	to do two calls to the save routine with complicated
;	parameters, we set up the next MCB descriptor beforehand.
;	Walk the MCB chain starting at the current MCB to find
;	the next one belonging to this process.
;
	mov	ax,currmcb
	mov	bx,pspseg
	mov	cx,prep.psp_mcb
	mov	dx,prep.noswap_mcb
;
swm_mcb_walk:
	mov	es,ax
	cmp	ax,cx
	je	swm_next_mcb
	cmp	ax,dx
	je	swm_next_mcb
;
	cmp	bx,es:owner		; our process?
	je	swm_mcb_found		; found it if yes
;
swm_next_mcb:
	cmp	es:id,4dh		; normal block?
	jne	swm_mcb_error		; error if end of chain
	add	ax,es:paras		; start + length
	inc	ax			; next MCB
	jmp	swm_mcb_walk
;
;	MCB found, set up an mcbdesc in the "nextmcb" structure
;
swm_mcb_found:
	mov	nextmcb.addr,es
	mov	ax,es:paras		; get number of paragraphs
	mov	nextmcb.msize,ax	; and save
	inc	ax
	mov	nextmcb.swsize,ax
	mov	bx,es
	add	bx,ax
	mov	currmcb,bx
	mov	nextmcb.swoffset,0
	mov	ax,currdesc.num_follow
	dec	ax
	mov	nextmcb.num_follow,ax
;
swapout_no_next:
	cmp	prep.swapmethod,USE_EMS
	je	swm_ems
	cmp	prep.swapmethod,USE_XMS
	je	swm_xms
	call	swapout_file
	jmp	short swm_next
;
swm_ems:
	call	swapout_ems
	jmp	short swm_next
;
swm_xms:
	call	swapout_xms
;
swm_next:
	jnz	swapout_error
	cmp	currdesc.num_follow,0
	je	swapout_complete
;
;	next MCB exists, copy the "nextmcb" descriptor into
;	currdesc, and loop.
;
	mov	es,datseg
	mov	si,offset nextmcb
	mov	di,offset currdesc
	mov	cx,TYPE mcbdesc
	rep movsb
	jmp	swapout_main
;
;
swm_mcb_error:
	cmp	prep.swapmethod,USE_FILE
	je	swm_mcberr_file
	cmp	prep.swapmethod,USE_EMS
	je	swm_mcberr_ems
;
	mov	ah,0ah			; release XMS frame on error
	mov	dx,prep.handle		; XMS handle
	call	prep.xmm
	mov	ax,RC_MCBERROR
	jmp	short swapout_error
;
swm_mcberr_ems:
	mov	dx,prep.handle		; EMS handle
	mov	ah,45h			; release EMS pages on error
	int	EMM_INT
	mov	ax,RC_MCBERROR
	jmp	short swapout_error
;
swm_mcberr_file:
	mov	ah,3eh			; close file
	mov	bx,prep.handle
	int	21h
	mov	dx,offset prep.swapfilename
	mov	ah,41h			; delete file
	int	21h
	mov	ax,RC_MCBERROR
;
swapout_error:
	ret
;
;
;	Swapout complete. Close the handle (EMS/file only),
;	then set up low memory.
;
swapout_complete:
	cmp	prep.swapmethod,USE_FILE
	jne	swoc_nofile
;
;	File swap: Close the swap file to make the handle available
;
	mov	bx,prep.handle
	mov	ah,3eh
	int	21h			; close file
	mov	si,offset swapin_file
	jnc	swoc_ready
	mov	ax,RC_SWAPERROR
	jmp	swapout_error
;
swoc_nofile:
	cmp	prep.swapmethod,USE_EMS
	jne	swoc_xms
;
;	EMS: Unmap page
;
	mov	ax,4400h
	mov	bx,-1
	mov	dx,prep.handle
	int	EMM_INT
	mov	si,offset swapin_ems
	jmp	short swoc_ready
;
swoc_xms:
	mov	si,offset swapin_xms
	jmp	short swoc_ready
;
no_swap1:
	mov	si,offset swapin_none
;	
;	Copy the appropriate swap-in routine to low memory.
;
swoc_ready:
	mov	es,pspseg
	mov	cx,swap_codelen / 2
	mov	di,codebeg + base_length
	push	ds
	mov	ax,cs
	mov	ds,ax
	rep movsw
;
;	And while we're at it, copy the MCB allocation routine (which
;	also includes the initial MCB release and exec call) down.
;
	mov	cx,base_length / 2
	mov	di,param_len
	mov	si,offset lowcode_begin
	rep movsw
;
	pop	ds
	mov	bx,es
	dec	bx
	mov	es,bx		; let ES point to base MCB
;
;	Again set up the base MCB descriptor, and copy it as well as
;	the variables set up by prep_swap to low memory.
;	This isn't too useful if we're not swapping, but it doesn't
;	hurt, either. The only variable used when not swapping is
;	lprep.swapmethod.
;
	mov	ax,es:paras
	mov	currdesc.msize,ax
	sub	ax,keep_paras
	mov	currdesc.swsize,ax
	mov	currdesc.addr,es
	mov	currdesc.swoffset,swapbeg + 16
	mov	ax,prep.total_mcbs
	mov	currdesc.num_follow,ax
;
	mov	es,pspseg		; ES points to PSP again
;
	mov	cx,TYPE prep_block
	mov	si,offset prep
	mov	di,lprep
	rep movsb
	mov	cx,TYPE mcbdesc
	mov	si,offset currdesc
	mov	di,lcurrdesc
	rep movsb
;
;	now set up other variables in low core
;
	mov	es:cgetmcb,getmcboff + codebeg
	mov	es:eretcode,0
	mov	es:retflags,0
;
;	Prepare exec parameter block
;
	mov	ax,es
	mov	es:expar.fcb1seg,ax
	mov	es:expar.fcb2seg,ax
	mov	es:expar.pparseg,ax
	mov	es:expar.envseg,0
;
;	The 'zero' word is located at 80h in the PSP, the start of
;	the command line. So as not to confuse MCB walking programs,
;	a command line length of zero is inserted here.
;
	mov	es:zero,0d00h		; 00h,0dh = empty command line
;
;	Init default fcb's by parsing parameter string
;
	IF	ptrsize
	lds	si,params
	ELSE
	mov	si,params
	ENDIF
	push	si
	mov	di,xfcb1
	mov	es:expar.fcb1,di
	push	di
	mov	cx,16
	xor	ax,ax
	rep stosw			; init both fcb's to 0
	pop	di
	mov	ax,2901h
	int	21h
	mov	di,xfcb2
	mov	es:expar.fcb2,di
	mov	ax,2901h
	int	21h
	pop	si
;
;	move command tail string into low core
;
	mov	di,progpars
	mov	es:expar.ppar,di
	xor	cx,cx
	inc	di
cmdcpy:
	lodsb
	or	al,al
	jz	cmdcpy_end
	stosb
	inc	cx
	jmp	cmdcpy
;
cmdcpy_end:
	mov	al,0dh
	stosb
	mov	es:progpars,cl
;
;	move filename string into low core
;
	IF	ptrsize
	lds	si,execfname
	ELSE
	mov	si,execfname
	ENDIF
	mov	di,filename
fncpy:
	lodsb
	stosb
	or	al,al
	jnz	fncpy
;
;	Setup environment copy
;
	mov	bx,keep_paras		; paras to keep
	mov	cx,envlen		; environment size
	jcxz	no_environ		; go jump if no environment
	cmp	swapping,0
	jne	do_envcopy
;
;	Not swapping, use the environment pointer directly.
;	Note that the environment copy must be paragraph aligned.
;
	IF	ptrsize
	mov	ax,word ptr (envp)+2
	mov	bx,word ptr (envp)
	ELSE
	mov	ax,ds
	mov	bx,envp
	ENDIF
	add	bx,15			; make sure it's paragraph aligned
	mov	cl,4
	shr	bx,cl			; and convert to segment addr
	add	ax,bx
	mov	es:expar.envseg,ax	; new environment segment
	xor	cx,cx			; mark no copy
	xor	bx,bx			; and no shrink
	jmp	short no_environ
;
;	Swapping or EXECing without return. Set up the pointers for
;	an environment copy (we can't do the copy yet, it might overwrite
;	this code).
;
do_envcopy:
	inc	cx
	shr	cx,1			; words to copy
	mov	ax,cx			; convert envsize to paras
	add	ax,7
	shr	ax,1
	shr	ax,1
	shr	ax,1
	add	bx,ax			; add envsize to paras to keep
	IF	ptrsize
	lds	si,envp
	ELSE
	mov	si,envp
	ENDIF
;
	mov	ax,es			; low core segment
	add	ax,keep_paras		; plus fixed paras
	mov	es:expar.envseg,ax	; = new environment segment
;
;	Save stack regs, switch to local stack
;
no_environ:
	mov	es:save_ss,ss
	mov	es:save_sp,sp
	mov	ax,es
	mov	ss,ax
	mov	sp,mystack
;
	push	cx			; save env length
	push	si			; save env pointer
	push	ds			; save env segment
;
;	save and patch INT0 (division by zero) vector
;
	xor	ax,ax
	mov	ds,ax
	mov	ax,word ptr ds:0
	mov	es:div0_off,ax
	mov	ax,word ptr ds:2
	mov	es:div0_seg,ax
	mov	word ptr ds:0,codebeg + iretoff
	mov	word ptr ds:2,es
;
	pop	ds			; pop environment segment
	pop	si			; pop environment offset
	pop	cx			; pop environment length
	mov	di,swapbeg		; environment destination
;
;	Push return address on local stack
;
	push	cs			; push return segment
	mov	ax,offset exec_cont
	push	ax			; push return offset
	mov	es:spx,sp		; save stack pointer
;
;	Goto low core code
;
	push	es			; push entry segment
        mov	ax,codebeg + doexec_entry
        push	ax			; push entry offset
;	ret	far			; can't use RET here because
	db	0cbh			; of .model
;
;----------------------------------------------------------------
;
;	Low core code will return to this location, with DS set to
;	the PSP segment.
;
exec_cont:
	push	ds
	pop	es
	mov	ss,ds:save_ss		; reload stack
	mov	sp,ds:save_sp
;
;	restore INT0 (division by zero) vector
;
	xor	cx,cx
	mov	ds,cx
	mov	cx,es:div0_off
	mov	word ptr ds:0,cx
	mov	cx,es:div0_seg
	mov	word ptr ds:2,cx
;
	mov	ax,es:eretcode
	mov	bx,es:retflags
	mov	ds,datseg
;
;	Restore overwritten part of program
;
	mov	si,offset save_dat
	mov	di,5ch
	mov	cx,savespace
	rep movsb
;
	test	bx,1			; carry set?
	jnz	exec_fault		; return EXEC error code if fault
	mov	ah,4dh			; else get program return code
	int	21h
	ret
;
exec_fault:
	mov	ah,3			; return error as 03xx
	ret
;	
do_spawn	ENDP
;
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;
emm_name	db	'EMMXXXX0'
;
;	prep_swap - prepare for swapping.
;
;	This routine checks all parameters necessary for swapping,
;	and attempts to set up the swap-out area in EMS/XMS, or on file.
;	In detail:
;
;	     1) Check whether the do_spawn routine is located
;		too low in memory, so it would get overwritten.
;		If this is true, return an error code (-2).
;
;	     2) Walk the memory control block chain, adding up the
;		paragraphs in all blocks assigned to this process.
;
;	     3) Check EMS (if the method parameter allows EMS):
;		- is an EMS driver installed?
;		- are sufficient EMS pages available?
;		if all goes well, the EMS pages are allocated, and the
;		routine returns success (1).
;
;	     4) Check XMS (if the method parameter allows XMS):
;		- is an XMS driver installed?
;		- is a sufficient XMS block available?
;		if all goes well, the XMS block is allocated, and the
;		routine returns success (2).
;
;	     5) Check file swap (if the method parameter allows it):
;		- try to create the file
;		- pre-allocate the file space needed by seeking to the end
;		  and writing a byte.
;		If the file can be written, the routine returns success (4).
;
;	     6) Return an error code (-1).
;
prep_swap	PROC	uses si di,pmethod:word,swapfname:ptr byte
	LOCAL	totparas: word
;
	mov	ax,_psp
;
	dec	ax
	mov	prep.psp_mcb,ax
	mov	prep.first_mcb,ax	; init first MCB to PSP
;
;	Make a copy of the environment pointer in the PSP
;
	inc	ax
	mov	es,ax
	mov	bx,es:psp_envptr
	dec	bx
	mov	prep.env_mcb,bx
	mov	prep.noswap_mcb,0
	test	pmethod,DONT_SWAP_ENV
	jz	can_swap_env
	mov	prep.noswap_mcb,bx
;
;	Check if spawn is too low in memory
;
can_swap_env:
	mov	bx,cs
	mov	dx,offset lowcode_begin
	mov	cl,4
	shr	dx,cl
	add	bx,dx			; normalized start of this code
	mov	dx,keep_paras		; the end of the modified area
	add	dx,ax			; plus PSP = end paragraph
	cmp	bx,dx
	ja	prepswap_ok	; ok if start of code > end of low mem
	mov	ax,-2
	mov	prep.swapmethod,al
	ret
;
;	Walk the chain of memory blocks, adding up the paragraphs
;	in all blocks belonging to this process.
;	We try to find the first MCB by getting DOS's "list of lists",
;	and fetching the word at offset -2 of the returned address.
;	If this fails, we use our PSP as the starting point.
;
prepswap_ok:
	xor	bx,bx
	mov	es,bx
	mov	ah,52h			; get list of lists
	int	21h
	mov	ax,es
	or	ax,bx
	jz	prep_no_first
	mov	es,es:[bx-2]		; first MCB
	cmp	es:id,4dh		; normal ID?
	jne	prep_no_first
	mov	prep.first_mcb,es
;
prep_no_first:
	mov	es,prep.psp_mcb		; ES points to base MCB
	mov	cx,es			; save this value
	mov	bx,es:owner		; the current process
	mov	dx,es:paras		; memory size in the base block
	sub	dx,keep_paras		; minus resident paragraphs
	mov	si,0			; number of MCBs except base
	mov	di,prep.noswap_mcb
	mov	ax,prep.first_mcb
	mov	prep.first_mcb,0
;
prep_mcb_walk:
	mov	es,ax
	cmp	ax,cx			; base block?
	je	prep_walk_next		; then don't count again
	cmp	ax,di			; Non-swap MCB?
	je	prep_walk_next		; then don't count
;
	cmp	bx,es:owner		; our process?
	jne	prep_walk_next		; next if not
	inc	si
	mov	ax,es:paras		; else get number of paragraphs
	add	ax,2			; + 1 for descriptor + 1 for MCB
	add	dx,ax			; total number of paras
	cmp	prep.first_mcb,0
	jne	prep_walk_next
	mov	prep.first_mcb,es
;
prep_walk_next:
	cmp	es:id,4dh		; normal block?
	jne	prep_mcb_ready		; ready if end of chain
	mov	ax,es
	add	ax,es:paras		; start + length
	inc	ax			; next MCB
	jmp	prep_mcb_walk
;
prep_mcb_ready:
	mov	totparas,dx
	mov	prep.total_mcbs,si
;
	test	pmethod,XMS_FIRST
	jnz	check_xms
;
;	Check for EMS swap
;
check_ems:
	test	pmethod,USE_EMS
	jz	prep_no_ems
;
	push	ds
	mov	al,EMM_INT
	mov	ah,35h
	int	21h			; get EMM int vector
	mov	ax,cs
	mov	ds,ax
	mov	si,offset emm_name
	mov	di,10
	mov	cx,8
	repz cmpsb			; EMM name present?
	pop	ds
	jnz	prep_no_ems
;
	mov	ah,40h			; get EMS status
	int	EMM_INT
	or	ah,ah			; EMS ok?
	jnz	prep_no_ems
;
	mov	ah,46h			; get EMS version
	int	EMM_INT
	or	ah,ah			; AH must be 0
	jnz	prep_no_ems
;
	cmp	al,30h			; >= version 3.0?
	jb	prep_no_ems
;
	mov	ah,41h			; Get page frame address
	int	EMM_INT
	or	ah,ah
	jnz	prep_no_ems
;
;	EMS present, try to allocate pages
;
	mov	prep.ems_pageframe,bx
	mov	bx,totparas
	add	bx,ems_paramask
	mov	cl,ems_shift
	shr	bx,cl
	mov	ah,43h			; allocate handle and pages
	int	EMM_INT
	or	ah,ah			; success?
	jnz	prep_no_ems
;
;	EMS pages allocated, swap to EMS
;
	mov	prep.handle,dx
	mov	ax,USE_EMS
	mov	prep.swapmethod,al
	ret
;
;	No EMS allowed, or EMS not present/full. Try XMS.
;
prep_no_ems:
	test	pmethod,XMS_FIRST
	jnz	check_file		; don't try again
;
check_xms:
	test	pmethod,USE_XMS
	jz	prep_no_xms
;
	mov	ax,4300h		; check if XMM driver present
	int	2fh
	cmp	al,80h			; is XMM installed?
	jne	prep_no_xms
	mov	ax,4310h		; get XMM entrypoint
	int	2fh
	mov	word ptr prep.xmm,bx	; save entry address
	mov	word ptr prep.xmm+2,es
;
	mov	dx,totparas
	add	dx,xms_paramask		; round to nearest multiple of 1k
	mov	cl,xms_shift
	shr	dx,cl			; convert to k
	mov	ah,9			; allocate extended memory block
	call	prep.xmm
	or	ax,ax
	jz	prep_no_xms
;
;	XMS block allocated, swap to XMS
;
	mov	prep.handle,dx
	mov	ax,USE_XMS
	mov	prep.swapmethod,al
	ret
;
;	No XMS allowed, or XMS not present/full. Try File swap.
;
prep_no_xms:
	test	pmethod,XMS_FIRST
	jz	check_file
	jmp	check_ems
;
check_file:
	test	pmethod,USE_FILE
	jnz	prep_do_file
	jmp	prep_no_file
;
prep_do_file:
	push	ds
	IF	ptrsize
	lds	dx,swapfname
	ELSE
	mov	dx,swapfname
	ENDIF
	mov	cx,2			; hidden attribute
	test	pmethod,HIDE_FILE
	jnz	prep_hide
	xor	cx,cx			; normal attribute
;
prep_hide:
	mov	ah,3ch			; create file
	test	pmethod,CREAT_TEMP
	jz	prep_no_temp
	mov	ah,5ah
;
prep_no_temp:
	int	21h			; create/create temp
	jnc	prep_got_file
	jmp	prep_no_file
;
prep_got_file:
	mov	bx,ax			; handle
;
;	save the file name
;
	pop	es
	push	es
	mov	di,offset prep.swapfilename
	mov	cx,81
	mov	si,dx
	rep movsb
;
	pop	ds
	mov	prep.handle,bx
;
;	preallocate the file
;
	test	pmethod,NO_PREALLOC
	jnz	prep_noprealloc
	test	pmethod,CHECK_NET
	jz	prep_nonetcheck
;
;	check whether file is on a network drive, and don't preallocate
;	if so. preallocation can slow down swapping significantly when
;	running on certain networks (Novell)
;
	mov	ax,440ah	; check if handle is remote
	int	21h
	jc	prep_nonetcheck	; assume not remote if function fails
	test	dh,80h		; DX bit 15 set ?
	jnz	prep_noprealloc	; remote if yes
;
prep_nonetcheck:
	mov	dx,totparas
	mov	cl,4
	rol	dx,cl
	mov	cx,dx
	and	dx,0fff0h
	and	cx,0000fh
	sub	dx,1
	sbb	cx,0
	mov	si,dx			; save
	mov	ax,4200h		; move file pointer, absolute
	int	21h
	jc	prep_file_err
	cmp	dx,cx
	jne	prep_file_err
	cmp	ax,si
	jne	prep_file_err
	mov	cx,1			; write 1 byte
	mov	ah,40h
	int	21h
	jc	prep_file_err
	cmp	ax,cx
	jne	prep_file_err
;
	mov	ax,4200h		; move file pointer, absolute
	xor	dx,dx
	xor	cx,cx			; rewind to beginning
	int	21h
	jc	prep_file_err
;
prep_noprealloc:
	mov	ax,USE_FILE
	mov	prep.swapmethod,al
	ret
;
prep_file_err:
	mov	ah,3eh			; close file
	int	21h
	mov	dx,offset prep.swapfilename
	mov	ah,41h			; delete file
	int	21h
;
prep_no_file:
	mov	ax,-1
	mov	prep.swapmethod,al
	ret
;
prep_swap	endp
;
	end
