	page	,132
;******************************************************************************
title BUFFER.ASM - buffer handler for doskey
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1989
;
;	this is a new ultra-simple version which has minimal overhead
;	  the lines in the buffer are zero terminated.
;                    
;	the buffer is organized as follows:
;
;	the most common layout is the "split" configuration, where
;	  the free space is in the middle somewhere.
;
;	<n><e><3> 0  .  .  . <l><i><n><e><1> 0 <l><i><n><e><2><l><i>
;	 ^           ^        ^                 ^                    ^
;	 |           |        |                 |                    + buf_end
;	 |           |        |                 + buf_current
;	 |           |        + buf_front
;	 |          + buf_back
;	 + buf_base
;
;	at other times, the free space will be at the ends, thus:
;
;	 .  . <l><i><n><e><1> 0 <l><i><n><e><2> 0 <l><i><n><e><3> 0  .
;	 ^     ^                 ^                                   ^  ^
;	 |     |                 |                          buf_back +  |
;	 |     + buf_front       +buf_current                   buf_end +
;	 + buf_base
;
;	notes:
;		There will always be one unusuable byte in the buffer.
;		  This avoids the problem of not being able to distinguish
;		  an empty buffer from a full one.
;
;		By definition, the buf_current pointer will be a null when
;		  the buffer is completely empty.  At all other times, there
;		  will be at least one string, thus there will be at least
;		  one zero terminator.  This fact allows us to avoid
;		  doing lots of messy checks to prevent endless loops.

	include	gendef.inc		; get LINE_LEN

;******************************************************************************
;			I N S T A N C E    D A T A
; (This data is all collectively defined in win386.asm in the instance block)
;******************************************************************************
	extrn	buf_base:word		; base of buffer array
	extrn	buf_end:word		; end of buffer array
	extrn	buf_front:word		; first string in buffer
	extrn	buf_back:word		; after last string in buffer
	extrn	buf_current:word	; current line pointer
					
text 	segment	byte	public	'code'
	assume	cs:text, ds:nothing, es:nothing, ss:nothing

	public	buffer_compare
	public	next_line
	public	prev_line
	public	get_cur_line
	public	ins_new_line
	
;******************************************************************************
;
;   next_line		Advance a line further in the command history
;   uses 		al,cx,si
;
;==============================================================================

next_line	proc	near
	assume	ds:text,es:text
	mov	si,[buf_current]	; get current pointer
	or	si,si			;  current=null?
	jz	nl_return		; exit, nothing needs to be done
	
nl_0:
	cmp	si,[buf_back]		; are we at end of history already?
	jz	nl_4			; don't advance if so

	call	get_buflen		; get length left in buffer segment
	jcxz	nl_1

	xchg	si,di
	xor	al,al			; setup al=terminator for scanning
	repnz	scasb			; look for a null terminator
	xchg	si,di
	jz	nl_3			; found null.  Done.

	cmp	si,[buf_back]
	jz	nl_4			; stop at end of history
nl_1:
	mov	si,[buf_base]		; wrap around to physical buffer base
	jmp	nl_0

nl_3:
	cmp	si,[buf_back]		; don't let nxt_buf screw up when
	jz	nl_4			;  buf_back == buf_end
	call	nxtbuf_x		; prohibit certain end conditions
nl_4:
	mov	[buf_current],si	; save current pointer
nl_return:
	ret

next_line	endp

;******************************************************************************
;
;   prev_line		Retreat a line back in the command history
;   uses   		si,cx,al
;
;==============================================================================


prev_line	proc	near
	assume	ds:text,es:text

	std				; we'll be scanning backwards
	mov	si,buf_current
	or	si,si			; current=null?
	jz	pl_return		; exit, nothing needs to be done
	
pl_0:
	call	decr_pointer		; point back to the null
	jc	pl_4			; done if hit buf_front

pl_01:
	call	decr_pointer		; scan back for previous null
	jc	pl_4			; done if hit buf_front
	mov	al,[si]			; is this the previous null?
	or	al,al
	jnz	pl_01			; loop if not

	inc	si			; point past previous terminator
	call	nxtbuf_x		; make sure we're not at buf_end

pl_4:
	mov	[buf_current],si	; save current pointer
pl_return:
	cld
	ret

prev_line	endp

;	decr_pointer decrements si within the buffer.  It wraps from
;	  buf_base back to buf_end.  If it hits buf_front, it returns
;	  with carry set.

decr_pointer	proc	near
	cmp	si,buf_front
	stc
	jz	decr_pointer_9	; return with carry set if at buf_front

	cmp	si,buf_base
	jnz	decr_pointer_1
	mov	si,buf_end

decr_pointer_1:
	dec	si
	clc				; return with no carry

decr_pointer_9:
	ret

decr_pointer	endp


;******************************************************************************
;
;   get_cur_line  	Copies the current line into the buffer passed
;   entry:   		es:di: Buffer where the line is to be copied
;   exit:	   	cx: Size of line copied
;   uses		ax
;
;==============================================================================

get_cur_line  		proc	near
	assume	ds:text,es:nothing

	push	si
	xor	cx,cx
	mov	si,[buf_current]
	or	si,si		; is there a current line?
	jz	getcurlin_1
	cmp	si,[buf_back]	; or are we at end of buffer?
	jz	getcurlin_1

	push	di
	push	bx		; preserve caller's bx

	xor	bx,bx		; keep our count here
gcl_1:
	call	get_buflen
	jcxz	gcl_3		; skip if at end of buffer
gcl_2:
	lodsb			; load one byte
	or	al,al		; is this the end?
	jz	gcl_4		; skip if so
	stosb			; copy it
	inc	bx		; count it
	loop	gcl_2

;	we come here if we've scanned to end of source buffer segment
;	  without finding a null terminator.  this implies that
;	  we were at the end of the physical buffer in a split buffer
;	  configuration, otherwise we would've been guaranteed to find
;	  the null just before buf_back.

gcl_3:
	mov	si,[buf_base]	; continue scanning other buffer segment
	jmp	gcl_1

gcl_4:
	mov	cx,bx		; return the length
	pop	bx
	pop	di
getcurlin_1:
	pop	si
	ret

get_cur_line	endp	



;******************************************************************************
;
;   ins_new_line	Inserts a line at the end of the history
;   entry:		ds:si line, cx=length
;   trash:		ax, bx
;
;==============================================================================

ins_new_line	proc	near
	assume	ds:text,es:text

	push	di		; preserve caller's registers
	push	si
	push	cx

	inc	cx		; move one extra dummy byte for null

;	the procedure here is to start at buf_back and start copying
;	  the new data in.  By definition, we'll never insert a line
;	  bigger than the buffer.  Therefore, we won't check for it.

;	The tricky part here is to know when we've deleted data.
;	  Accumulate a flag byte in bl as follows:
;	   bits 7-3 same as bit 2
;	   bit 2 - buffer was initially in split configuration
;	   bit 1 - we wrapped around the end of physical buffer with insertion
;	   bit 0 - the final buf_back is >= buf_front (reached or passed it)
;
;	The logical formula for knowing when we've deleted data is as follows:
;	  if (initially split) then if wrapped or reached -> delscan
;	  else if wrapped and passed -> delscan
;
;	Which means bl == 3, 253, 254 and 255 indicate a deletion occurred.
;	  When this is the case, we have to scan forward from the new buf_back
;	  to find the next null, then advance one more place for
;	  the new buf_front

	mov	di,buf_back		; get end of buffer pointer
	cmp	di,buf_front		;  split buffer configuration?
	sbb	bl,bl			; fill bl with initially_split flag

	mov	ax,buf_end		; how much room to end of buffer?
	sub	ax,di
	cmp	ax,cx			; will we wrap 'round end?
	rcl	bl,1			; save the flag in flag byte
	test	bl,1
	jz	ins_nowrap		; skip if no wrap

	xchg	ax,cx
	sub	ax,cx			; this amount will go at front
	rep	movsb			; move the part to the end
	mov	di,buf_base		; and the rest will go here
	mov	cx,ax			; length remaining

ins_nowrap:
	rep	movsb			; move the rest
	cmp	di,buf_front		; did we reach or pass?
	cmc				; put flag into correct state
	rcl	bl,1			; get the reached flag

;	now if bl=3, 253, 254 or 255, we need to delete some data

	cmp	bl,3
	jz	inl_delscan
	cmp	bl,253
	jb	inl_exit1		; done if no deletion needed
inl_delscan:

	push	di			; save new buf_back pointer

;	******* Now scan di forward to the next null and set
;	*******  buf_front just after it

	xor	al,al
	mov	cx,buf_end
	sub	cx,di
	repnz	scasb
	jz	inl_delscan_1		; brif we found it

	mov	di,buf_base
	mov	cx,buf_end
	sub	cx,di
	repnz	scasb			; find null in front part of buffer
inl_delscan_1:
	mov	buf_front,di
	pop	di			; restore buf_back

;	come here to store the new buf_back pointer and place the null

inl_exit1:
	mov	byte ptr [di-1],0	; store null at end
	mov	buf_back,di		; we're done!
	mov	buf_current,di
	pop	cx
	pop	si
	pop	di
	ret

ins_new_line	endp


;******************************************************************************
;
;   buffer_compare	returns zero flag true if the string
;			matches the current line, not case sensitive
;   entry:		di = string in our es:, bx = length
;   exit:		zero true if matched
;   trashes:		ax, bx, cx, di, si
;
;==============================================================================

buffer_compare	proc	near
	mov	si,[buf_current]
bufcom0:
	call	get_buflen	; see how many bytes in that part of buffer
bufcom1:
	or	bx,bx		; have we matched the whole thing?
	jz	bufcom9		; yes!  success!  zero flag true!
	jcxz	bufcom2		; brif need to swap to other buffer half
	lodsb			; get the character
	dec	bx
	dec	cx
	scasb			; does it match?
	jz	bufcom1		; loop if so
	and	al,5fh		; see if it's alpha
	cmp	al,'A'
	jb	bufcom_fail
	cmp	al,'Z'
	ja	bufcom_fail
	xor	al,[di][-1]
	and	al,5fh		; totally non-case sensitive compare
	jz	bufcom1		; still okay if both alpha and match
bufcom_fail:
	or	al,1		; reset zero flag
bufcom9:
	ret
bufcom2:
	mov	si,[buf_base]	; wrap to front of buffer
	jmp	bufcom0
buffer_compare	endp

;******************************************************************************
;			L O C A L    R O U T I N E S
;******************************************************************************


;******************************************************************************
;
;   get_buflen		sets up count in cx of bytes left in buffer
;			segment at si
;   entry:		si->buffer pointer
;   exit:		cx= count remaining in that part of buffer
;
;==============================================================================

get_buflen	proc	near
	mov	cx,buf_back
	sub	cx,si			; get length if not 2nd half of split
	jae	get_buflen_9

	mov	cx,buf_end
	sub	cx,si			; get length in second half of split

get_buflen_9:
	ret

get_buflen	endp

;******************************************************************************
;
;   nxtbuf_x		if buffer pointer in [si] is at the end of the
;			physical buffer (at buf_end), wrap back to front
;			(buf_base)
;   entry:		si = buffer pointer
;   exit:		si = corrected buffer pointer
;
;==============================================================================

;	if (si == buf_end) then si = buf_base;

nxtbuf_x	proc	near

	cmp	si,buf_end
	jnz	nb_02
	mov	si,buf_base
nb_02:
	ret

nxtbuf_x	endp

text	ends
	end
