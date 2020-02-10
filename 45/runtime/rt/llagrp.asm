	TITLE	LLAGRP - GW-BASIC Support for advanced graphics
;***
; LLAGRP - GW-BASIC Support for advanced graphics
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains support for graphics functions required only
;	by programs using graphics drawing statements.	Routines called
;	directly or indirectly by statements not specifically graphic in
;	nature (such as CLS) belong in LLCGRP for runtime granularity.
;
;	Routines in this module are mode-independent in one or more of the
;	following ways:
;	    1) no device-dependent interaction,
;	    2) table driven through mode-dependent data hooks, or
;	    3) calls mode-dependent routines through function hooks.
;
;	Mode-dependent graphics functions and initializers for hooks
;	are segregated in separate modules for better granularity.
;
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_BSS
	USESEG	GR_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc

sBegin	_BSS
;
;#****************************************************************************
; External variables
;#****************************************************************************
;
externW b$PenC
externB b$MaskC
externB b$AttrC
externD b$AddrC
externW b$OffC
externW b$SegC
externB b$BytesPerRow
externB b$BitsPerPixel
externB b$Planes
externB b$PaintBorder
externB b$Tiling
externW b$Incr1
externW b$Incr2
externW b$IncrY
;
;#****************************************************************************
; External function vectors
;#****************************************************************************
;
externW b$SetAttr
externW b$MapXYC
externW b$PutAction
externW b$NReadL
externW b$NWriteL
externW b$SetPixFirstC
externW b$SetPixLastC
;
;#****************************************************************************
; Local variables
;#****************************************************************************
;
staticW BitCount,,1		;count of pixel bits per line for Get/Put
staticW Shift,,1		;lo byte = left shift count to align video
				;	   byte with array byte for Get/Put
staticW Masks,,1		;lo byte = mask to apply to last partial
				;	   byte for Put
				;hi byte = mask to apply to first partial
				;	   byte for Put
labelD	ArrayAddr,,1		;address of Get/Put array
staticW ArrayOff,,1		;  offset of Get/Put array
staticW ArraySeg,,1		;  segment of Get/Put array

sEnd	_BSS

assumes CS,GR_TEXT

sBegin	GR_TEXT

;***
;B$PixSize - Get number of bits per pixel
;OEM-interface routine
;
;Purpose:
;	Get the number of bits per pixel for the current graphics mode.
;	This routine will never be called if the screen is not in a
;	graphics mode.
;
;	This routine is only called when determining whether a graphics
;	PUT will fit on the screen.  Note that on multiple plane
;	systems, this routine should return the number of bits per pixel
;	on a single plane, not the total number of bits on all the planes.
;
;Entry:
;	none
;
;Exit:
;	[AL] = bits / pixel
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	none
;****
cProc	B$PixSize,<PUBLIC,NEAR>
cBegin
	mov	al,b$BitsPerPixel
cEnd

;***
;B$PutGetInit - Initialize variables for B$NReadC and B$NWriteC
;OEM-interface routine
;
;Purpose:
;	Called once for every PUT or GET statement to initialize
;	variables used by B$NReadC and B$NWriteC.
;
;	Following B$PutGetInit, PUT and GT enter a loop which
;	calls B$NReadC or B$NWriteC once for each line of the
;	rectangle being processed.  If B$PutGetInit is being
;	called by PUT, then it must retain which function ([AL])
;	is being requested.  Usually this entails setting up a
;	PUT function dispatch vector.  B$PutGetInit is also
;	responsible for saving the array address and the line
;	length for B$NReadC or B$NWriteC.  Depending on how
;	these routines are implemented, it may be desirable to
;	calculate certain other values in B$PutGetInit for speed
;	considerations.
;
;	Upon entry into B$PutGetInit, the graphics cursor has
;	been set to the upper left hand corner of the rectangle
;	that is to be used.
;
;Entry:
;	[AL] = PUT action [0..4] represent (OR,AND,PRESET,PSET,XOR)
;	[ES:BX] = address of source/destination array
;	[CX] = bits/pixel * pixels/horizontal line
;	PSW.C = set indicates caller is PUT
;	PSW.C = reset indicates caller is GET
;
;Exit:
;	none
;
;Uses:
;	Per Convention
;
;Exceptions:
;	none
;****
cProc	B$PutGetInit,<PUBLIC,NEAR>
cBegin
	pushf
	mov	ArrayOff,bx	;set up array address
	mov	ArraySeg,es
	mov	BitCount,cx	;save bit count
	mov	bl,b$MaskC	;align mask to array byte to get shift count
	xor	dx,dx		;zero shift count
	xor	bh,bh		;mask for PUT
	mov	ch,cl		;get bit count
				;  (only concerned about mod 8 for bit index)
	mov	cl,b$BitsPerPixel  ;shift count
PGILoop:
	shl	bl,cl		;test mask byte alignment
	jc	PGI2		;go if it was
	or	bh,bl		;or pixel to first partial byte PUT mask
	add	ch,cl		;byte-align count for last partial byte PUT mask
	add	dl,cl		;bump shift count of bits
	jmp	short PGILoop
PGI2:
	mov	bl,0FFH 	;set up mask for last partial byte
	popf			
	jc	PGIPut		;go for PUT init
	mov	cx,BitCount	;use bit cnt to determine final mask for GET
	and	cx,7		;bit count in last byte
	jz	PGI4		;go if full byte
	shr	bl,cl		;shift 0's in for mask
	not	bl		;invert for 1's
PGI4:				
	mov	dh,bl		;last byte mask
	mov	Shift,dx	;save align shift count and last byte mask
	jmp	short PGExit	;exit if not PUT init
PGIPut: 			
	not	bh		;mask for 1st partial byte
	mov	cl,ch
	and	cx,7		;bit count in last byte
	jz	PGI3		;go if full byte
	shr	bl,cl		;shift 0's in for mask
	not	bl		;invert for 1's
PGI3:
	mov	Shift,dx	;save alignment shift count
	mov	Masks,bx	;save masks
	call	[b$PutAction]	;set up PUT action
PGExit:
cEnd

;***
;B$NReadC - Read multiple pixels from screen
;OEM-interface routine
;
;Purpose:
;	Transfer bits from the screen to the array specified by
;	the last B$PutGetInit.  This routine is responsible for
;	maintaining a pointer to the appropriate location in the
;	destination array.  The starting point on the screen for
;	the transfer is the graphics cursor.
;
;Entry:
;	B$PutGetInit must have been called.
;
;Exit:
;	Array updated.
;
;Uses:
;	per convention
;
;Exceptions:
;	none
;****
cProc	B$NReadC,<PUBLIC,NEAR>,<DI,SI,BP>
cBegin
	cld
	mov	bl,b$Planes	;number of planes to read for each row
	les	di,ArrayAddr	;array offset and segment
	push	ds
	lds	si,b$AddrC	;screen address

	ASSUME	DS:NOTHING

	xor	bh,bh		;starting plane
NRdLoop:
.erre	ID_SSEQDS		;assumes ss = ds
	mov	cx,ss:Shift	;cl=array align shift, ch=last byte mask
	mov	bp,ss:BitCount	;total bits to read
	push	bx		;save: plane info
	push	si		;   screen address
.erre	ID_SSEQDS		;assumes ss = ds
	call	ss:[b$NReadL]	;read a line from plane [bh]
	pop	si		
	pop	bx
	inc	bh		;next plane
	dec	bl		;one more plane done
	jnz	NRdLoop 	;go for next plane
	pop	ds

	ASSUME	DS:DGROUP

	mov	ArraySeg,es	;save updated array segment
	mov	ArrayOff,di	;  and offset
cEnd

;***
;B$NWriteC - Write multiple pixels to the screen
;OEM-interface routine
;
;Purpose:
;	Retrieve information from the array indicated by the last call
;	to B$PutGetInit, perform the requested function on it, and store
;	the resulting data on the screen.  This routine is responsible
;	for maintaining a pointer to the appropriate location in the
;	source array, so that multiple calls to B$NWriteC will step
;	though the entire array.  The pixels will be written starting
;	at the graphics cursor.
;
;Entry:
;	B$PutGetInit must have been called
;
;Exit:
;	Screen updated.
;
;Uses:
;	per convention
;
;Exceptions:
;	none
;****

cProc	B$NWriteC,<PUBLIC,NEAR>,<DI,SI,BP>
cBegin
	cld
	mov	bl,b$Planes	;number of planes to write for each row
	les	di,b$AddrC	;screen address
	push	ds
	lds	si,ArrayAddr	;array offset and segment

	ASSUME	DS:NOTHING

	xor	bh,bh		;starting plane
NWrLoop:
.erre	ID_SSEQDS		;assumes ss = ds
	mov	cx,ss:Shift	;cx=array align shift
	mov	bp,ss:BitCount	;total bits to read
	mov	dx,ss:Masks	;dl=last byte mask, dh=first byte mask
	push	bx		;save: plane info
	push	di		;      screen address
.erre	ID_SSEQDS		;assumes ss = ds
	call	ss:[b$NWriteL] ;write a line to plane [bh]
	pop	di
	pop	bx
	inc	bh		;next plane
	dec	bl		;one more plane done
	jnz	NWrLoop 	;go for next plane
.erre	ID_SSEQDS		;assumes ss = ds
	mov	ss:ArraySeg,ds	;save updated array segment
	pop	ds

	ASSUME	DS:DGROUP

	mov	ArrayOff,si	;  and offset
cEnd

;***
;B$PaintInit - Initialize for [b$ScanL] and [b$ScanR]
;OEM-interface routine
;
;Purpose:
;	This routine is called once for each user paint request.
;	It must determine if the boarder attribute is legal for
;	the current screen mode, and if it is legal it should be
;	stored for later use.  It should also provide any
;	initialization needed for the PAINT support routines
;	[b$ScanL] and [b$ScanR].
;
;	In general, painting is accomplished by starting from
;	the entry point and painting right to the border then
;	returning to the entry point and painting left to the
;	border.  Then go up one line and repeat the paint right
;	paint left procedure.  Next go down two lines and repeat.
;	Since [b$ScanR] and [b$ScanL] are called so many times
;	for this one function, it is important that they be
;	optimized for speed.  Any precomputations that can be done
;	outside of these routines will have a measurable effect on
;	the running time of a PAINT command.
;
;	Note:  The following OEM routines are only called during
;	a PAINT command, thus will always be proceeded by B$PaintInit.
;
;		[b$ChkDown]
;		[b$ChkUp]
;		[b$LeftC]
;		[b$PaintBound]
;		[b$ScanL]
;		[b$ScanR]
;		[b$SetTile]
;
;Entry:
;	[AL] = prospective border attribute
;
;Exit:
;	PSW.C = set implies legal border attribute
;	PSW.C = reset implies illegal border attribute.
;
;Uses:
;	Per convention.
;
;Exceptions:
;	none
;****
cProc	B$PaintInit,<PUBLIC,NEAR>
cBegin
	; If b$Tiling=0 OR b$Tiling=2 then B$TileMod has not been called for
	;	this PAINT (PAINT without tiling). Set b$Tiling=0.
	; If b$Tiling = 1 then B$TileMod has been called and a PAINT with
	;	tiling. Set b$Tiling=0FFh.
	CMP	b$Tiling,1	;has tilmod been called?
	MOV	b$Tiling,0	;default no tiling
	JNE	NO_TILE_PAINT
	DEC	b$Tiling	;if tiling, then b$Tiling=0FFh
NO_TILE_PAINT:
	MOV	CH,b$AttrC	;GET FILL COLOR
	CALL	[b$SetAttr]	;go calc color mask
	MOV	AL,b$AttrC	;GET ENCODED BORDER ATTRIBUTE
	MOV	b$PaintBorder,AL ;SAVE BORDER ATTRIBUTE
	MOV	b$AttrC,CH	;RESTORE OLD ATTRIB
cEnd

;***
;B$ImageSize - Calculate space needed for a GET command
;OEM-interface routine
;
;Purpose:
;	This routine returns the number of bytes needed to
;	store a rectangle of pixels in an array when a GET is
;	performed.
;
;	Formula used:
;	    Space required := ((xdim*bits/pixel+7)/8)*planes*ydim
;
;	    where:
;
;		xdim = width, in pixels, of the rectangle
;		ydim = height, in pixels, of the rectangle
;		planes = number of planes for current graphics mode
;		bits/pixel = number of bits required to represent
;			     a single pixel
;
;	    The division in the formula is an integer division, with
;	    the remainder ignored.
;
;
;Entry:
;	[BX] = Y-pixel dimension of rectangle
;	[DX] = X-pixel dimension of rectangle
;
;Exit:
;	[CX:BX] = # of bytes necessary to store the rectangle
;	[DX] = number of BITS needed for one row of the rectangle
;	       for a single plane.  (xdim * bits/pixel)
;	PSW.C set if overflow occurred and [BX], [DX] undefined.
;
;Uses:
;	per convention
;
;Exceptions:
;	none
;****
cProc	B$ImageSize,<PUBLIC,NEAR>
cBegin
	mov	al,b$BitsPerPixel
	cbw
	mul	dx		;xdim*bits/pixel
	mov	cx,ax		;save X bit dimension
	add	ax,7		;() + 7
	shr	ax,1		;() / 8
	shr	ax,1
	shr	ax,1
	mov	dl,b$Planes	;() * planes
	xor	dh,dh
	mul	dx
	mul	bx		;() * ydim
	xchg	bx,ax		;result to CX:BX
	xchg	cx,dx		;  w/ X BIT dimension to DX
	clc
cEnd

;***
;B$TileMod - Get modulus of tile string
;OEM-interface routine
;
;Purpose:
;	This routine reports the modulus of the tile string
;	interpretation for this screen mode.  Since the tile
;	string in PAINT is a bit map representation of the tile
;	pattern, its interpretation depends on the number of
;	planes of graphics memory.  In single plane systems
;	(including interlaced, single bit per pixel, or multiple
;	bits per pixel), the tile string is interpreted as an
;	eight bit wide tile, with a vertical dimension equal
;	to the length of the string.  On a multi-plane system,
;	the tile string is interpreted as an eight bit wide
;	tile, with a vertical dimension equal to the length
;	of the string MOD number of planes.  The first byte
;	in the string represents the bits to be set in the first
;	plane, the second byte represents the second plane,
;	and so on.
;
;	For example, given a six byte tile string:
;
;	    A single plane system interprets the string as a
;	    6 pixel high tile.
;
;	    A 3 plane system interprets the string as a 2 pixel
;	    high tile.
;
;	In order to maintain the alignment of the tile string
;	along the Y axis and to pass the right number of tile
;	bytes to the OEM dependent routine [B$SetTile], the runtime
;	needs to know how many bytes are needed to represent a 1
;	pixel high portion of the tile.  This is usually the number
;	of graphics planes available.
;
;	The way that tiling will proceed is thus:
;
;	    For each line in the region to be tiled, pass to the OEM
;	    routines one byte for each plane.  This byte will be
;	    replicated across the entire line by [b$ScanR] and
;	    [b$ScanL].  The bytes should be aligned such that
;	    two lines with the same tiling pattern will be aligned
;	    visually.
;
;Entry:
;	None
;
;Exit:
;	[AL] = modulus of tile string representation
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None
;****
cProc	B$TileMod,<PUBLIC,NEAR>
cBegin
	MOV	AL,b$Planes	;returns the # of bytes needed to represent
				;a 1 pixel high portion of the tile
	MOV	b$Tiling,1	;set tiling flag
cEnd

;***
;B$StoreC - Store values for graphics cursor
;OEM-interface routine
;
;Purpose:
;	Set the graphics cursor to a previous value.
;
;	The cursor specification passed to this routine will always be
;	a copy obtained from B$FetchC or [b$ScanR].  The OEM independent
;	code never modifies or interprets the values comprising cursor.
;
;	If the graphics cursor does not need three words, this routine
;	can just ignore the values that are not needed.  The routines
;	that return a cursor do not have to set the unneeded values.
;
;Entry:
;	AX = first byte of cursor
;	BX = second byte of cursor
;	CX = third byte of cursor
;
;Exit:
;	None.
;
;Uses:
;	Per convention
;
;Preserves:
;	DX
;
;Exceptions:
;	none
;****
;
; In our implementation, we only use 3 bytes for the cursor.  The
; values that are required are:
;
;	AL = mask portion of cursor
;	BX = offset portion of cursor
;
;#***

cProc	B$StoreC,<PUBLIC,NEAR>
cBegin
	MOV	b$OffC,BX
	MOV	b$MaskC,AL
cEnd

;***
;B$FetchC - Get values for the graphics cursor
;OEM-interface routine
;
;Purpose:
;	On return, AX, BX, and CX are loaded with the location of the
;	graphics cursor.  The values returned are not interpreted and
;	are only used to set the cursor at a later point with a call
;	to B$StoreC.
;
;	The OEM has 6 bytes (3 words) in which to implement a graphics
;	cursor. This should accommodate machines with very high resolution
;	screens.  The representation of the cursor is totally up to the
;	OEM, however it is important to put effort into finding a representation
;	that will allow accessing pixels on the screen as fast as
;	possible.
;
;	A couple of possible implementations are:
;
;	   a)	first word:  segment of cursor address in screen memory
;		second word: offset into the segment
;		third word:  mask for position of cursor within byte/word
;
;	   b)	first word:  offset into segment (segment is constant)
;		second word: mask for position of cursor within byte/word
;		third word:  column and row of cursor
;
;	Note that the runtime code will preserve the order of the word
;	for the cursor based on their names and not their registers.
;	Thus for [b$ScanR], SI and DI are used to return parts of the
;	cursor.
;
;Entry:
;	none
;
;Exit:
;	AX = first word of cursor
;	BX = second word of cursor
;	CX = third word of cursor
;
;Uses:
;	Per Convention
;
;Preserves:
;	DX
;
;Exceptions:
;	none
;****
;
; In our implementation, we only use 3 bytes for the cursor.  The
; values that are returned are:
;
;	AL = mask portion of cursor
;	BX = offset portion of cursor
;
; However, the code should still conform to the OEM specifications so
; that we do not have to change things when this is sent to the OEM.
;#****

cProc	B$FetchC,<PUBLIC,NEAR>
cBegin
	MOV	AL,b$MaskC
	MOV	BX,b$OffC
cEnd

sEnd	GR_TEXT

	END
