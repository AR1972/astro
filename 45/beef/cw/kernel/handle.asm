;*
;*	COW : Character Oriented Windows
;*
;*	handle.asm : handle table control

	TITLE	HANDLE - Handle Table Manager

	include kernel.inc
	include galloc.inc

; This code assumes the following conditions
;
errnz	<he_address>

sBegin	KERNEL
assumes CS,KERNEL

; These are all of the internal subroutines defined in this source file.
;
	PUBLIC	halloc, hthread, hfree, hdref

SUBRS	PROC	NEAR

; HALLOC - Subroutine to allocate a handle for a block.
;
;   Inputs:	AX = block that needs a handle
;		DS:DI = address of local arena infomation structure
;
;   Outputs:	AX,BX = handle for that block
;		DX preserved
;		CX = AX
;		AX = 0 then, DX = original AX and no handle allocated
;
;
halloc: mov	bx,[di].hi_hfree
	or	bx,bx
	jz	hagrow
ha1:
	xor	cx,cx			; Zero lock count and free handle flag
	mov	word ptr [bx].he_flags,cx
	errnz	<2-he_flags>
	errnz	<3-he_count>
	xchg	[bx].he_link,ax 	; Remove handle from head of chain
	errnz	<he_address-he_link>	; and store true address of object
	mov	[di].hi_hfree,ax	; Update head of handle free list
	mov	ax,bx			; Return handle to caller
	mov	cx,ax
	ret

hagrow:
	push	ax
	push	dx
	mov	cx,[di].hi_hdelta
	jcxz	hafail
	call	[di].hi_hexpand
	jcxz	hafail
	mov	bx,ax
	pop	dx
	pop	ax
	jmp	ha1

hafail:
	xor	ax,ax
	pop	dx			; Flush stack
	pop	dx			; Return original AX in DX
	ret				; return error

; HTHREAD - subroutine to thread together a list of free handles
;
;   Inputs:	DI = start of chain
;		CX = #handle entries in chain
;
;   Outputs:	AX = address of first handle entry on free list
;		CX = 0
;		DI = address of first word after handle block
;
hthread:
	push	di			; Save first free handle entry
	push	ds
	pop	es
	cld
ht1:					; Chain entries together via he_link
	errnz	<he_link>
	lea	ax,[di].SIZE HandleEntry
	stosw
	mov	ax,HE_FREEHANDLE
	errnz	<2-he_flags>
	errnz	<3-he_count>
	stosw
	loop	ht1
					; Null terminate free list
	mov	[di-SIZE HandleEntry].he_link,cx
	pop	ax			; Return free handle address
	ret


; HFREE - Subroutine to return a handle to the freelist.
;
;   Inputs:	SI = handle to free
;
;   Outputs:	AX = zero if valid handle or -1 if handle already free
;
hfree:	or	si,si			; Ignore zero handles
	jz	hf1
	mov	ax,HE_FREEHANDLE	; Mark handle as free
	xchg	word ptr [si].he_flags,ax
	errnz	<2-he_flags>
	errnz	<3-he_count>
	inc	ax			; Already free?
	jz	hf2			; Yes, return error
	errnz	<1+HE_FREEHANDLE>
	mov	ax,si			; Push handle on head of freelist
	xchg	[di].hi_hfree,ax
	mov	[si].he_link,ax
hf1:
	xor	ax,ax			; Return zero
	ret
hf2:
	dec	ax
	ret


; HDREF - Subroutine to dereference a handle.
;   Inputs:	SI = handle
;   Outputs:	AX = address of client data or zero for discarded objects
;		CH = lock count
;		CL = zero or HE_DISCARDED flag
;		Z flag set AX is zero and CL is non-zero
;
hdref:
	xor	ax,ax
	mov	cx,word ptr [si].he_flags
	errnz	<2-he_flags>
	errnz	<3-he_count>
	inc	cx
	jz	hdref1
	errnz	<1+HE_FREEHANDLE>
	dec	cx
	and	cl,HE_DISCARDED
	jnz	hdref1
	mov	ax,[si].he_address
hdref1:
	or	ax,ax
	ret

SUBRS	ENDP

sEnd	KERNEL

end
