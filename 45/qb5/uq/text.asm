;***	TEXT.ASM - Low level routines for Text Manager
;*
;* GLOBAL
;*	SegAddr		Linear address -> segmented address
;*	CbFindLine	Find a line in the source buffer
;*	BigMoveUp	Move large blocks of memory up (overlapping)
;*	BigMoveDown	Move large blocks of memory down (overlapping)
;*	LinearAddr	Convert from segmented to linear (32-bit) address
;*
;* LOCAL
;*	$LinearAddr	Convert from segmented to linear (32-bit) address
;*	$SegAddr	Convert from linear (32-bit) to segmented address
;*
;* DESCRIPTION
;*	Low level routines for dealing with large blocks of data.
;*
;------------------------------------------------------------

	PAGE	66,132
	TITLE	TEXT.ASM - Low level routines for Text Manager

	.XLIST


	include	version.inc
	.LIST


;------------------------------------------------------------
; DATA segment definitions
;------------------------------------------------------------

assumes DS, DATA
assumes SS, DATA


;------------------------------------------------------------
; CODE segment definitions
;------------------------------------------------------------

sBegin	UI
assumes	CS,UI

;***	SegAddr - Convert from linear to segmented address
;*
;* SYNOPSIS
;*	lp = SegAddr(addr32)
;*
;* ENTRY
;*	addr32		32-bit (linear) address
;*
;* RETURNS
;*	Far pointer (segmented address)
;*
;* DESCRIPTION
;*	Calls internal routine $SegAddr to do the work
;------------------------------------------------------------

cProc	SegAddr, <NEAR, PUBLIC>

	parmD	addr32

cBegin
	mov	dx, word ptr [addr32 + 2]
	mov	ax, word ptr [addr32]
	call	$SegAddr
cEnd

;***	LinearAddr - Convert from segmented to linear (32-bit) address
;*
;* SYNOPSIS
;*	addr32 = LinearAddr(lp)
;*
;* ENTRY
;*	lp		Far pointer
;*
;* RETURNS
;*	32-bit address of converted far pointer
;*
;* DESCRIPTION
;*	Calls local routine $LinearAddr
;------------------------------------------------------------

cProc	LinearAddr, <NEAR, PUBLIC>

	parmD	lp

cBegin
	mov	ax, word ptr [lp]
	mov	dx, word ptr [lp + 2]

	call	$LinearAddr

cEnd


;***	CbFindLine - Find a line in the source buffer
;*
;* SYNOPSIS
;*	cb = CbFindLine(pAddr32, oln)
;*
;* ENTRY
;*	pAddr32		POINTER TO 32-bit address of source buffer
;*	oln		Line number to find
;*
;* RETURNS
;*	Number of bytes in line found
;*	Also stores 32-bit linear address of line found in pAddr32
;*
;* DESCRIPTION
;*	On entry, *pAddr32 "points to" the word count preceding a line of
;*	text.  Simply moves through the source buffer, using the word
;*	count preceding each line to skip to the next line.  On exit,
;*	*pAddr32 "points to" the line found (actual text, NOT the preceding
;*	word count).
;------------------------------------------------------------

cProc	CbFindLine, <NEAR, PUBLIC>, <DI, SI>

	parmDP	pAddr32
	parmW	oln

cBegin
	mov	bx, pAddr32
	mov	ax, [bx]
	mov	dx, [bx+2]
	call	$SegAddr		; Returns DX:AX as far pointer

	mov	es, dx
	mov	di, ax

	mov	cx, oln
	inc	cx			; Filename is always "line 0"

	xor	si, si

NextLine:
	add	di, si

; Normalize es:di

	mov	ax, di
	shr	ax, 1
	shr	ax, 1
	shr	ax, 1
	shr	ax, 1
	mov	bx, es
	add	ax, bx
	mov	es, ax
	and	di, 000Fh

	mov	si, es:[di]
	inc	di			; Skip...
	inc	di			; ...word count
	loop	NextLine

; Convert back to 32-bit address

	mov	ax, di
	mov	dx, es
	call	$LinearAddr

; Store 32-bit address

	mov	bx, pAddr32
	mov	[bx], ax
	mov	[bx+2], dx

	mov	ax, si			; Number of bytes in line
cEnd


;***	BigMoveUp - Moves large blocks of overlapping memory up
;*
;* SYNOPSIS
;*	BigMoveUp(addr32Dst, addr32Src, cb)
;*
;* ENTRY
;*	addr32Dst	32-bit address "pointer" to dest. buffer
;*	addr32Src	32-bit address "pointer" to source buffer
;*	cb		Number of bytes to move
;*
;* RETURNS
;*	None
;*
;* DESCRIPTION
;*	The algorithm moves the data in chunks of FFF0 bytes.
;*
;*	Before starting to do the move, the number of FFF0 length chunks
;*	is calculated, along with the number of bytes that are left over.
;*	(Find i, j such that cb = i*FFF0 + j where j < FFF0).
;*
;*	So i blocks of size FFF0 and one block of size j must be moved.
;*	The i blocks of size FFF0 are moved first, followed by the one
;*	block of size j.
;*
;*	In order to move FFF0 bytes, each of the source, and the destination
;*	addresses must have an offset portion less than 000F (because
;*	we are moving from bottom to top). Before each chunk is moved,
;*	both addresses are normalized to this form.
;*
;* CAVEAT
;*	This routine will not work for blocks which start anywhere in the
;*	last 64K of physical memory.
;*
;*	This routine makes implicit use of the relationship between segment
;*	number, and physical address that exists in the iApx 86 architecture.
;*	This routing assumes that cb >= 2
;------------------------------------------------------------

cProc	BigMoveUp, <NEAR, PUBLIC>, <DS, SI, DI>

	parmD	addr32Dst
	parmD	addr32Src
	parmD	cb

cBegin
	cCall	SegAddr,<addr32Dst>	; Convert from linear to segmented address
	mov	es, dx
	mov	di, ax

	cCall	SegAddr,<addr32Src>	; Convert from linear to segmented address
	mov	ds, dx
	mov	si, ax

; Calulate the number of FFF0-byte chunks to move.
; (Save the remainder for later).

	mov	ax, word ptr [cb]
	mov	dx, word ptr [cb + 2]
	mov	bx, 0FFF0h
	div	bx			; AX == # of chunks, DX == # of bytes
	xchg	dx, ax
	push	ax

NextChunkUp:
	mov	cl, 4			; Normalize DS:SI to form SSSS:000y
	mov	ax, si
	shr	ax, cl
	mov	bx, ds
	add	ax, bx
	mov	ds, ax
	and	si, 000fH

	mov	ax, di			; Normalize ES:DI to form SSSS:000y
	shr	ax, cl
	mov	bx, es
	add	ax, bx
	mov	es, ax
	and	di, 000fH

	or	dx,dx			; See if done moving big blocks
	je	MoveBytesUp		; Yes, move last partial block

	dec	dx			; Moving next big block
	mov	cx, 0FFF0h / 2
	rep	movsw			; Moving words is faster than bytes

	jmp	Short NextChunkUp

; At this point DS:SI and ES:DI are normalized, so we can move the
; remaining bytes in one chunk.

MoveBytesUp:
	pop	cx			; Get back # of bytes
	shr	cx, 1
	jnc	MoveWordsUp

	movsb				; Move odd byte

MoveWordsUp:
	rep	movsw			; Move last chunk of words

cEnd

;***	BigMoveDown - Moves large blocks of overlapping memory down
;*
;* SYNOPSIS
;*	BigMoveDown(addr32Dst, addr32Src, cb)
;*
;* ENTRY
;*	addr32Dst	32-bit address "pointer" to source buffer
;*	addr32Src	32-bit address "pointer" to dest. buffer
;*	cb		Number of bytes to move
;*
;* RETURNS
;*	None
;*
;* DESCRIPTION
;*	Similar in operation to BigMoveUp().
;------------------------------------------------------------

cProc	BigMoveDown, <NEAR, PUBLIC>, <DS, SI, DI>

	parmD	addr32Dst
	parmD	addr32Src
	parmD	cb

cBegin
	mov	ax, word ptr [addr32Dst] ; Point to end of Target string
	mov	dx, word ptr [addr32Dst + 2]
	add	ax, word ptr [cb]
	adc	dx, word ptr [cb + 2]
	sub	ax, 1			; Address must be zero based
	sbb	dx, 0
	call	$SegAddr		; Convert linear to segmented addr
	mov	di, ax			; ES:DI == end of target string
	mov	es, dx

	mov	ax, word ptr [addr32Src] ;Point to end of Source string
	mov	dx, word ptr [addr32Src + 2]
	add	ax, word ptr [cb]
	adc	dx, word ptr [cb + 2]
	sub	ax, 1			; Address is zero based
	sbb	dx, 0
	call	$SegAddr		; Convert linear to segmented addr
	mov	si, ax			; DS:SI == end of source string
	mov	ds, dx

; Calulate the number of FFF0-byte chunks to move.
; (Save the remainder for later).

	mov	ax, word ptr [cb]
	mov	dx, word ptr [cb + 2]
	mov	bx, 0FFF0h
	div	bx			; AX == # of chunks, DX == # of bytes
	xchg	dx, ax
	push	ax			; Save final chunk size in bytes

	std				; Performing a reverse move operation

NextChunkDown:
	mov	cl, 4			; Normalize DS:SI to form SSSS:FFFy
	mov	ax, si
	shr	ax, cl
	mov	bx, 0FFFh
	sub	bx, ax
	mov	ax, ds
	sub	ax, bx
	mov	ds, ax
	or	si, 0FFF0h

	mov	cl, 4			; Normalize ES:DI to form SSSS:FFFy
	mov	ax, di
	shr	ax, cl
	mov	bx, 0FFFh
	sub	bx, ax
	mov	ax, es
	sub	ax, bx
	mov	es, ax
	or	di, 0FFF0h

	or	dx,dx			; See if done moving big blocks
	je	MoveBytesDown		; Yes, move last partial block and quit

	dec	dx			; Keep track of big blocks moved
	movsb				; Can't do a movsw if either SI or...
	movsb				; ...DI is 0FFFFh so do two movsb's...
					; ...to make sure they are not

	dec	si			; Point at the low byte of the words to be moved
	dec	di			; (We are moving words at a time now)
	mov	cx, 0FFF0h / 2 - 1	; Number of words - 1
	rep	movsw

	inc	si			; Repz goes one byte too far
	inc	di
	jmp	Short NextChunkDown

; At this point DS:SI and ES:DI are normalized, so we can move the
; remaining bytes in one chunk.

MoveBytesDown:
	pop	cx			; Get back # of remaining bytes
	shr	cx, 1			; Convert to words
	jnc	MoveWordsDown		; See if odd byte exists

	movsb				; Move odd byte

MoveWordsDown:
	jcxz	MoveDownDone		; No moves on zero length left

	movsb				; Problem with moving last word of block
	movsb

	dec	cx			; Count the Word move

	dec	si			; Point at the low byte of the words to be moved
	dec	di			; (We are moving words at a time now)
	rep	movsw

MoveDownDone:
	cld				; Always restore UP condition flag
cEnd

;------------------------------------------------------------
; LOCAL routines
;------------------------------------------------------------

;***	$LinearAddr - Convert far pointer to linear (32-bit) address
;*
;* ENTRY
;*	DX		Segment of far pointer
;*	AX		Offset of far pointer
;*
;* RETURNS
;*	DX		High word of 32-bit address
;*	AX		Low word of 32-bit address
;*
;* USES
;*	BX
;*	CL
;*
;* DESCRIPTION
;*	Addr32 = (Segment << 4) + Offset
;------------------------------------------------------------

$LinearAddr	proc	near

	mov	bx, ax
	mov	cl, 4
	rol	dx, cl
	mov	ax, dx
	and	dx, 000Fh
	and	ax, 0FFF0h
	add	ax, bx
	adc	dx, 0
	ret

$LinearAddr	endp

;***	$SegAddr - Convert from linear (32-bit) address to far pointer
;*
;* ENTRY
;*	DX		High word of 32-bit address
;*	AX		Low word of 32-bit address
;*
;* RETURNS
;*	DX		Segment of far pointer
;*	AX		Offset of far pointer
;*
;* USES
;*	BX
;*	CL
;*
;* DESCRIPTION
;*	Addr32 = (Segment << 4) + Offset
;------------------------------------------------------------

$SegAddr	proc	near

	and	dx, 000Fh
	mov	bx, ax
	and	ax, 0FFF0h
	or	dx, ax
	mov	cl, 4
	ror	dx, cl
	mov	ax, bx
	and	ax, 000Fh
	ret

$SegAddr	endp


sEnd	UI

;------------------------------------------------------------
; Memory Support routines
;------------------------------------------------------------

sBegin	RT
assumes	CS,RT

extrn	B$IFHAlloc:near
extrn	B$FHRealloc:near
extrn	B$FHDealloc:near


;***	FhdAlloc - Allocate a fhd
;*
;* SYNOPSIS
;*	success = FhdAlloc(pFhd, cb)
;*
;* ENTRY
;*	pFhd		pointer to fhd to allocate
;*	cb		Initial size of fhd
;*
;* RETURNS
;*	FALSE if allocation failed
;*
;* DESCRIPTION
;*	Allocates the fhd by calling the runtime entrypoint for
;*	far heap allocs.
;------------------------------------------------------------

cProc	FhdAlloc,<PUBLIC,FAR>
parmW	pFhd
parmD	cb
cBegin
	mov	bx,pFhd
	mov	dx,WORD PTR cb+2
	mov	ax,WORD PTR cb
	call	B$IFHAlloc
cEnd

;***	FhdReAlloc - Realloc a fhd
;*
;* SYNOPSIS
;*	success = FhdReAlloc(pFhd, cb)
;*
;* ENTRY
;*	pFhd		pointer to fhd to reallocate
;*	cb		new size of fhd
;*
;* RETURNS
;*	FALSE if reallocation failed
;*
;* DESCRIPTION
;*	Reallocates the fhd by calling the runtime entrypoint for
;*	far heap reallocation.
;------------------------------------------------------------

cProc	FhdRealloc,<PUBLIC,FAR>
parmW	pFhd
parmD	cb
cBegin
	mov	bx,pFhd
	mov	dx,WORD PTR cb+2
	mov	ax,WORD PTR cb
	call	B$FHRealloc
cEnd

;***	FhdDealloc - Deallcates a fhd
;*
;* SYNOPSIS
;*	FhdDealloc(pFhd)
;*
;* ENTRY
;*	pFhd		pointer to fhd to deallocate
;*
;* RETURNS
;*	Nothing
;*
;* DESCRIPTION
;------------------------------------------------------------

cProc	FhdDealloc,<PUBLIC,FAR>
parmW	pFhd
cBegin
	mov	bx,pFhd
	call	B$FHDealloc
cEnd

sEnd	RT

	end
