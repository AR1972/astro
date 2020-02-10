;
; Expand all tabs to spaces
;

.xlist
include ..\h\cmacros.inc
.list

sBegin	code
assumes cs,code

cProc	UnTab,<PUBLIC>,<si,di>
ParmW	src
ParmW	dst
cBegin
	push	ds
	pop	es
	cld
	mov	si,src
	mov	di,dst
	mov	dx,di
	mov	cx,1
	mov	ah,' '
getc:
	lodsb
	cmp	al,9
	jz	tab
store:
	rep	stosb
	inc	cx
	or	al,al
	jnz	getc
	jmp	done
tab:
	mov	cx,di
	sub	cx,dx
	and	cx,7
	sub	cx,8
	neg	cx
	mov	al,ah
	jmp	store
done:
	lea	ax,[di-1]
	sub	ax,dst
cEnd

sEnd

end
