;*
;*	COW : Character Oriented Windows
;*
;*	gmemory5.asm : Global memory for DOS 5

	TITLE	GMEMORY - Windows interface to DOS memory management DOS 5

	.xlist
	include	kernel.inc
	.list


externFP  <DosAllocSeg, DosReallocSeg, DosFreeSeg>


sBegin	KERNEL
    assumes CS,KERNEL
    assumes DS,DATA


cProc	GlobalAlloc,<PUBLIC,FAR,ATOMIC>
    parmW   flags
    parmD   lcb
    localW  sd			;* segment descriptor - returned as handle
cBegin	GlobalAlloc

;*	* Assume size < 64 K
	mov	dx,word ptr (lcb+2)	;* HI_cb
	or	dx,dx
	jnz	alloc_fail		;* too big
	mov	ax,word ptr (lcb)	;* LO_cb
	lea	bx,sd
	cCall	DosAllocSeg,<ax, ss, bx, 0>	;* don't share
	or	ax,ax
	mov	ax,sd
	jz	alloc_ok
alloc_fail:
	xor	ax,ax			;* return NULL handle
alloc_ok:	;* ax = hgmem (actually a sd (segment descriptor))

cEnd	GlobalAlloc



cProc	GlobalReAlloc,<PUBLIC,FAR,ATOMIC>
    parmW   hgmem
    parmD   lcb
    parmW   flags
cBegin	GlobalReAlloc

;*	* Assume size < 64 K
	mov	dx,word ptr (lcb+2)	;* HI_cb
	or	dx,dx
	jnz	realloc_fail		;* too big
	mov	ax,word ptr (lcb)	;* LO_cb
	cCall	DosReallocSeg,<ax, hgmem>
	or	ax,ax
	mov	ax,hgmem		;* return old handle
	jz	realloc_ok
realloc_fail:
	xor	ax,ax			;* return NULL handle
realloc_ok:	;* ax = hgmem (actually a sd (segment descriptor))

cEnd	GlobalReAlloc



cProc	GlobalFree,<PUBLIC,FAR,ATOMIC>
    parmW   hgmem
cBegin	GlobalFree
	cCall	DosFreeSeg,<hgmem>
;*	* return VOID (Lock state not used).
cEnd	GlobalFree



cProc	GlobalLock,<PUBLIC,FAR,ATOMIC>
   parmW   hgmem
cBegin	GlobalLock
	mov	dx,hgmem
	xor	ax,ax		;* dx:ax => block
cEnd	GlobalLock



cProc	GlobalUnlock,<PUBLIC,FAR,ATOMIC>
   parmW   hgmem
cBegin	GlobalUnlock
;*	* Do nothing
cEnd	GlobalUnlock


cProc	GlobalHandle,<PUBLIC,FAR,ATOMIC>
   parmW   hgmem
cBegin	GlobalHandle

	mov	ax,hgmem			;* return handle/address
	mov	dx,ax				;* in both parts

cEnd	GlobalHandle


cProc	GetCodeHandle,<PUBLIC,FAR,ATOMIC>
   parmW   ps
   parmW   ib
cBegin	GetCodeHandle

	mov	ax,ps				;* return handle/address

cEnd	GetCodeHandle


cProc	ThrowStack,<PUBLIC,FAR,ATOMIC>
   parmW   bpNew
cBegin	ThrowStack
cEnd	ThrowStack


sEnd	KERNEL

	END
