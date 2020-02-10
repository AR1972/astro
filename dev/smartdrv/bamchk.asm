;
;	This little program takes a snapshot of the SmartDrv 4.0
;	cache data structures and dumps it, crudely formatted, to
;	STDOUT for further processing.
;
;masm bamchk;
;link bamchk;
;exe2bin bamchk.exe bamchk.com
;del bamchk.exe
;
zseg	segment
	assume	cs:zseg

	org	100h

com_entry:
	jmp	short com_entry_1

;	local variables

queuelength	dw	0

com_entry_1:
	mov	ax,4a10h	; bambi multiplex interrupt
	mov	bx,10		; get internals
	int	2fh
	cmp	bx,10
	mov	dx,offset no_bambi
	jz	msg_exit	; bomb out if no int2f service

	push	es
	pop	ds		; get bambi internals pointer into ds:bx

	push	cs
	pop	es		; point es:di to our local snapshot buffer
	mov	di,offset our_buffer

;	Take a quick snapshot without doing disk i/o.  If we wanted
;	  to be pure here, we could assure interrupts disabled, but
;	  it is a fairly unlikely case and would simply result in
;	  slightly bad diagnostic data.

	mov	si,word ptr 8[bx] ; get queuelength pointer
	mov	cx,[si]		; get queuelength into cx
	mov	queuelength,cx
	xor	bp,bp		; point to first element

;	**** We're going to assume that our buffer is big enough
;	     to hold all of the data structures.  We know that they
;	     all fit in Bambi, so they should fit in our buffer.

snapshot_loop:
	mov	si,word ptr 6[bx] ; get hiword pointer
	mov	si,word ptr [si]
	mov	ax,ds:[bp+si]	; get hiword data
	stosw			; into snapshot buffer

	mov	si,word ptr 4[bx] ; get loword pointer
	mov	si,word ptr [si]
	mov	ax,ds:[bp+si]	; get loword data
	stosw			; stick it into our snapshot buffer

;	***** ASSUME USING WORD MASKS!!!!  (8K blocks) *****

	mov	si,word ptr 2[bx] ; get dirtyindexoffset
	mov	si,word ptr [si]
	mov	ax,ds:[bp+si]
	stosw

	mov	si,word ptr [bx] ; get validindexoffset
	mov	si,word ptr [si]
	mov	ax,ds:[bp+si]
	stosw

	add	bp,2
	loop	snapshot_loop	; loop queuelength times

;	Now it's time to dump the buffer to STDOUT!!!!

	push	cs
	pop	ds		; point to our_buffer
	mov	si,offset our_buffer ; with ds:si
	mov	cx,queuelength

output_loop:
	lodsw			; get indentifierhiword
	push	ax		; save high part of sector number
	mov	al,ah		; get drive ID (or 0ffh if undefined)
	add	al,'A'		; convert to letter, or @ if undefined
	call	cofa
	pop	ax
	call	hex8
	lodsw
	call	hex16		; dump loword
	lodsw			; get dirtymask
	call	hex16
	lodsw			; get validmask
	call	hex16
	mov	al,13
	call	cofa
	mov	al,10
	call	cofa
	loop	output_loop

	mov	ax,4c00h
	int	21h

msg_exit:
	push	cs
	pop	ds
	mov	ah,9
	int	21h		; display message
	mov	ax,4c01h
	int	21h

hex16:
	push	ax
	mov	al,ah
	call	hex8
	pop	ax
hex8:
	push	ax
	shr	al,1
	shr	al,1
	shr	al,1
	shr	al,1
	call	hexnib
	pop	ax
hexnib:
	and	al,0fh
	add	al,90h
	daa
	adc	al,3ah
	daa
cofa:
	mov	dl,al
	mov	ah,2
	int	21h		; slow output through int21
	ret

no_bambi:
	db	'Bambi not responding!$'

our_buffer:

zseg	ends
	end	com_entry
