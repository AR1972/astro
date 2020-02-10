.386p

include emm386.inc
include emmdata.inc
include vdmseg.inc
include vdmsel.inc

ifdef DEBUG

_TEXT	segment
	assume	cs:_TEXT, ds:NOTHING, es:NOTHING, fs:NOTHING, gs:NOTHING

	public	pDebugPrintf

pDebugPrintf	proc	far

	Pcall	VDMC_GSEL,pTestDbgIns
	jnc	pDP_skip_it

	push	bp
	mov	bp, sp
	push	es
	push	ds
	push	si
	push	di
	push	ax
	mov	ax, 074h
	push	VDMD_GSEL
	pop	ds
	mov	si, [bp+6]
	push	ss
	pop	es
	lea	di, [bp+8]
	int	41h
	pop	ax
	pop	di
	pop	si
	pop	ds
	pop	es
	pop	bp

pDP_skip_it:
	ret

pDebugPrintf	endp

	public	pTestDbgIns

pTestDbgIns	proc	far

	push	ds
	push	RCODEA_GSEL
	pop	ds
	bt	ds:[GenFlags], fDebugActiveBit
	pop	ds
	ret

pTestDbgIns	endp

_TEXT	ends


R_CODE	segment
	assume	cs:R_CODE, ds:NOTHING, es:NOTHING, fs:NOTHING, gs:NOTHING

	public	rDebugPrintf

rDebugPrintf	proc	far

	call	far ptr rTestDbgIns
	jnc	rDP_skip_it

	push	bp
	mov	bp, sp
	push	es
	push	ds
	push	si
	push	di
	push	ax
	mov	ah, 057h
	push	cs
	pop	ds
	mov	si, [bp+6]
	push	ss
	pop	es
	lea	di, [bp+8]
	int	68h
	pop	ax
	pop	di
	pop	si
	pop	ds
	pop	es
	pop	bp

rDP_skip_it:
	ret

rDebugPrintf	endp

	public	rTestDbgIns

rTestDbgIns	proc	far

	bt	cs:[GenFlags], fDebugActiveBit
	ret

rTestDbgIns	endp

R_CODE	ends


endif	;DEBUG

if 0	;----------------------------------------------------------------

	public	dbgr
	public	pdbgr

R_CODE	SEGMENT
	assume cs:R_CODE,ds:R_CODE,es:R_CODE

dbgr	proc	near
	
	push	bx
	push	cx
	push	dx
	push	bp

	mov	dx, 02f8h	; com2
;	mov	dx, 03f8h	; com1

	push	ax
	add	dx, 4
	mov	al, 3
	out	dx, al
	inc	dx
	inc	dx
	mov	bh, 30h
	call	wait_status
	je	a9
a7:	pop	cx
	mov	al, cl
a8:	or	ah, 80
	jmp	a3
a9:	dec	dx
a10:	mov	bh, 20h
	call	wait_status
	jne	a7
a11:	sub	dx,5
	pop	cx
	mov	al, cl
	out	dx, al
a3:	pop	bp
	pop	dx
	pop	cx
	pop	bx
	ret

dbgr	endp

wait_status:
	push	bp
	push	bx
	pop	bp
	and 	bp, 0ffh	
	rcl	bp, 1
	rcl	bp, 1
wfs0:	sub	cx,cx
wfs1:	in	al, dx
	mov	ah,al
	and	al, bh
	cmp	al, bh
	je	wfsend
	loop	wfs1
	dec	bp
	jne	wfs0
	or	bh,bh
wfsend:	pop	bp
	ret

R_CODE	ENDS

_TEXT	SEGMENT
	assume cs:_TEXT

pdbgr	proc	near
	
	push	bx
	push	cx
	push	dx
	push	bp

	mov	dx, 02f8h	; com2
;	mov	dx, 03f8h	; com1

	push	ax
	add	dx, 4
	mov	al, 3
	out	dx, al
	inc	dx
	inc	dx
	mov	bh, 30h
	call	pwait_status
	je	pa9
pa7:	pop	cx
	mov	al, cl
pa8:	or	ah, 80
	jmp	pa3
pa9:	dec	dx
pa10:	mov	bh, 20h
	call	pwait_status
	jne	pa7
pa11:	sub	dx,5
	pop	cx
	mov	al, cl
	out	dx, al
pa3:	pop	bp
	pop	dx
	pop	cx
	pop	bx
	ret

pdbgr	endp

pwait_status:
	push	bp
	push	bx
	pop	bp
	and 	bp, 0ffh	
	rcl	bp, 1
	rcl	bp, 1
pwfs0:	sub	cx,cx
pwfs1:	
	in	al, dx
	mov	ah,al
	and	al, bh
	cmp	al, bh
	je	pwfsend
	loop	pwfs1
	dec	bp
	jne	pwfs0
	or	bh,bh
pwfsend:	pop	bp
	ret

_TEXT	ENDS

endif	;----------------------------------------------------------------

	END

