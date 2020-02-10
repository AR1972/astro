; fmemset.asm:  _fmemset() for C 5.1

	.model	small, c

	.code

lcb_fmemset proc   far uses es si di, lpDst:far ptr, val:word, cnt:word

	les	di, lpDst
	mov	ax, val
	mov	cx, cnt
	rep stosb

	mov	dx, es			;returns lpDst
	mov	ax, word ptr lpDst

	ret

lcb_fmemset endp

	end
