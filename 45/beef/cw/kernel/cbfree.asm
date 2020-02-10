;
;	COW : Character Oriented Windows
;
;	cbfree.asm : return current size of available pool or sb's.

	include	kernel.inc
	include galloc.inc

;----------------------------------------------------------------------------

sBegin	DATA

externW     <pGlobalHeap>

sEnd	DATA

;----------------------------------------------------------------------------

sBegin	KERNEL

    assumes CS,KERNEL
    assumes DS,DGROUP
    assumes ES,NOTHING
    assumes SS,DGROUP

cProc	CbFreeMem, <FAR, PUBLIC, ATOMIC>, <ds,di>
cBegin	CbFreeMem

	xor	ax,ax				; Starting size = 0000.
	mov	ds,pGlobalHeap
	xor	di,di
	mov	dx,ds:[di].hi_last		; Calculate location of
	sub	dx,ds:[di].gi_reserve		;   the fence.

	mov	ds,ds:[di].hi_first		; get to first in the list.

DoNext:
	mov	cx,ds:[di].ga_next		; Does the next block spill
	cmp	cx,dx				;   over the fence?
	jae	HitFence			; Bail if it does.

	mov	bx,ds:[di].ga_owner		; If he's Free or Code
	or	bh,bh				;   then add
	jnz	@F				;   on his size.
	add	ax,ds:[di].ga_size
@@:	mov	ds,cx				; Go on to the Next block.
	jmp	short DoNext
	
HitFence:
	mov	bx,ds:[di].ga_owner		; If the last is Free/Code
	or	bh,bh				;   then add on from start of
	jnz	@F				;   block to start of fence

	add	ax,dx				; size := size +
	mov	bx,ds				;   (
	sub	ax,bx

	mov	bx,ds				; size : = size +
	sub	dx,bx				;    (fence - start of block)
	add	ax,dx

@@:

cEnd	CbFreeMem

sEnd	KERNEL
	END

