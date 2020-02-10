;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include	cmacros.inc

	assumes	cs, code

calldev		struc
	pack	dd	?
	strat	dd	?
	ent	dd	?
calldev		ends

sBegin	code

cProc	bio, <PUBLIC>, <bp,ds,es>
	parmDP	cdev
cBegin
	mov	bp, cdev
	mov	ds, word ptr [bp.strat+2]
	les	bx, [bp.pack]
	call	[bp.strat]
	call	[bp.ent]
cEnd

cProc	checkumb, <PUBLIC>, <es>
	parmDP	mem
cBegin
	mov	ah,62h			; get psp address
	int	21h
	mov	es,bx
	mov	bx,es:[0002]		; get end of dos memory
	mov	ax,1
	cmp	mem,bx
	ja	@f			; if mem is on umb
	mov	ax,0
@@:
cEnd

cProc	check_swapper, <PUBLIC>, <di, es>
cBegin
	xor	bx,bx
	mov	di,bx
	mov	es,bx
	mov	ax,4b02h		; check if swapper exist
	int	2fh
	jc	cs_none			; if swapper not exist
	mov	ax,es
	or	ax,di
	jnz	cs_exist		; if swapper exist
cs_none:
	mov	ax,0
	jmp	short cs_end
cs_exist:
	mov	ax,1
cs_end:
cEnd

sEnd	code

	end
