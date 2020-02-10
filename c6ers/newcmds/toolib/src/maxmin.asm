;
; maximum and minimum routines.
;

.xlist
include ..\h\cmacros.inc
.list

sBegin	code
assumes cs,code

cProc	max,<PUBLIC>
parmW	a
parmW	b
cBegin
	mov	ax,a
	cmp	ax,b
	jg	maxdone
	mov	ax,b
maxdone:
cEnd

cProc	min,<PUBLIC>
parmW	a
parmW	b
cBegin
	mov	ax,a
	cmp	ax,b
	jl	mindone
	mov	ax,b
mindone:
cEnd

cProc	lmax,<PUBLIC>
parmD	a
parmD	b
cBegin
	mov	ax, word ptr (a)
	mov	dx, word ptr (a + 2)
	cmp	dx, word ptr (b + 2)
	jg	lmaxdone
	jl	lmaxswap
	cmp	ax, word ptr (b)
	jae	lmaxdone
lmaxswap:
	mov	ax, word ptr (b)
	mov	dx, word ptr (b + 2)
lmaxdone:
cEnd

cProc	lmin,<PUBLIC>
parmD	a
parmD	b
cBegin
	mov	ax, word ptr (a)
	mov	dx, word ptr (a + 2)
	cmp	dx, word ptr (b + 2)
	jl	lmindone
	jg	lminswap
	cmp	ax, word ptr (b)
	jbe	lmindone
lminswap:
	mov	ax, word ptr (b)
	mov	dx, word ptr (b + 2)
lmindone:
cEnd

sEnd

end
