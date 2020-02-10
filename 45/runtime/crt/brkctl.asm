	page	,132
	title	brkctl - C system call for memory
;***
;brkctl.asm - DOS brkctl memory allocation
;
;	Copyright (c) 1985-1988, Microsoft Corporation.  All rights reserved.
;
;Purpose:
;	DOS brkctl memory allocation
;
;*******************************************************************************

include	version.inc
.xlist
include	cmacros.inc
include	msdos.inc
.list
include	brkctl.inc


sBegin	data
assumes	ds,data

externW _psp			; PSP paragraph number
externW _abrktb
externW	_abrktbe
externW	_abrkp

extrn	b$nmalloc_end:word	;[1] offset of last word of nmalloc space

externW	_aseglo			;[1] defined in CDATA3.INC

sEnd	data

sBegin	code
assumes	ds,data
assumes	cs,code

page
;***
;brkctl(command,incr,saddr) - memory allocation routine
;
;Purpose:
;	Allocate memory from OS.  Based on Xenix brkctl routine.
;	This is the work-horse OS interface routine for the C runtime
;	memory allocation package.
;
;Entry:
;
;	command - brkctl command: BR_ARGSEG or BR_NEWSEG 
;		BR_NEWSEG:	allocate a new data segment
;		BR_ARGSEG:	expand or shrink existing segment
;	incr - increment: for BR_ARGSEG, increment specified seg
;			  for BR_NEWSEG, allocate new seg of size incr
;	saddr - segment: segment address of block to operate on
;
;	command		incr	seg	action
;	-------		----	---	------
;	BR_ARGSEG	0	NULL	report on last seg
;	BR_ARGSEG	other	other	increment specified seg
;	BR_NEWSEG	0	-	allocate new seg, size 0
;	BR_NEWSEG	other	-	allocate new seg, size incr
;					up empty segs
;
;	IMPORTANT note:  The above documentation is taken from the Xenix
;	sources.  It may not be completely accurate for the DOS version
;	of brkctl.  
;
;Exit:
;	DX:AX  	segment:offset address of affected memory block on success
;		-1 on error
;
;Uses:
;
;Exceptions:
;
;*******************************************************************************

cProc	brkctl,<PUBLIC>,<si,di,es>

	parmw	command
	parmd	incr
	parmd	saddr

cBegin
	cmp	word ptr (incr+2),0 ; ???  for now allow 0-FFFF
	jnz	reterr		;   die

	mov	di,dataoffset _abrktb ; di = start of segment table (DGROUP)
	mov	dx,word ptr (incr) ; dx = incr
	mov	ax,command	; ax = command
	errnz	BR_ARGSEG-1
	dec	ax
	jnz	tryNEW

doARG:
	call	argseg		; try specified segment
	jc	reterr
	jmp	short return	; done

tryNEW:
	mov	si,[_abrkp]	; si = current segment entry
	errnz	BR_NEWSEG-2
	dec	ax
	jz	doNEW

doIMP:
	cmp	si,di		; IMP with current segment = DGROUP
	je	doNEW		;   yes - allocate new segment
	mov	ax,[si].sg	; ax = last allocated segment
	mov	Seg_saddr,ax	; use for argseg attempt
	push	si		; save segment for possible NEW
	call	argseg		; try specified segment
	pop	si
	jnc	return		; successful

doNEW:
	add	si,size segrec
	cmp	si,dataoffset _abrktbe ; check for end of segtab
	jae	reterr

	or	dx,dx		; is it 0?
	jnz	okNEW		;   no

reterr:
	mov	ax,-1
	cwd			; dx:ax = -1
	jmp	short return

okNEW:
	mov	bx,dx		; compute # paragraphs
	add	bx,15
	rcr	bx,1
	mov	cl,3
	shr	bx,cl		; bx = # paragraphs
@@:
	callos	allocmem
	jc	reterr

	cmp	ax,_aseglo	;* Check to see if we ignore this segment
	jbe	@B		;* Yes -- get a new segment

	xchg	ax,dx		; dx:ax = seg:size
	mov	[si].sz,ax	; save new segment size
	mov	[si].sg,dx	; save new segment value
	mov	[_abrkp],si	; save new last segment pointer
	xor	ax,ax		; return seg:0

return:

cEnd

page
;***
;argseg - helper routine for changing size of requested segment
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

argseg:
	mov	cx,Seg_saddr	; get segment to find
	mov	si,di		; si = start of segment table

segloop:
	cmp	[si].sg,cx
	je	segfound	;   found it
	add	si,size segrec	; skip to next
	cmp	si,dataoffset _abrktbe
	jne	segloop		; keep looping
	stc			; set 'C' for error
	jmp	short toobig	; bad entry

segfound:
	mov	bx,dx
	add	bx,[si].sz	; add in current size
	jc	toobig		;   carry - too big
	mov	dx,bx		; dx = current size
	mov	es,cx		; set up segment

	cmp	si,di		; check if DS
	jne	getmem		;   no

 	cmp	[b$nmalloc_end],bx ;[1] DS - do we need more memory?
 	jnb	setsize 	;[1] no
;[1] 	stc			;[1]
 	jmp	short toobig	;[1] need to grow near heap -- error

getmem:

	add	bx,15		; round up to paragraph (17 bits)
	rcr	bx,1
	shr	bx,1
	shr	bx,1
	shr	bx,1		; bx = # paragraphs (1-1000h)


	callos	setmem		; set memory block size
	jc	toobig		;   too big


setsize:
	xchg	ax,dx		; ax = newsize
	xchg	[si].sz,ax	; ax = base
	mov	dx,cx		; dx:ax = seg:base
				; carry clear
toobig:
	ret


sEnd	code

	end
