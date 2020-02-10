
;*
;*	COW : Character Oriented Windows
;*
;*	debug5.asm : debug support for OS/2


	include	kernel.inc

IFDEF	DEBUG	;* entire file!!

externFP	<VioWrtTty>


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,nothing


cProc	CwOutSz,<FAR,PUBLIC>,<DI>
    parmW	sz
cBegin	CwOutSz

	PUBLIC	_cwoutsz
_cwoutsz:
	cld
	push	ss
	pop	es
	mov	di,sz
	xor	ax,ax
	mov	cx,0ffffh
	repne	scasb
	neg	cx
	sub	cx,2
	cCall	VioWrtTty,<es, sz, cx, 0>

cEnd	CwOutSz


sEnd	KERNEL

ENDIF	;DEBUG	;* entire file!!

	END
