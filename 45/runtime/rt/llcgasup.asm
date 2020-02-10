	TITLE	LLCGASUP - LowLevel CGA support (shared routines)
;***
; LLCGASUP - LowLevel CGA support
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	This module contains support routines extracted from LLCGA.ASM
;	which are shared by CGA and VGA functions.
;
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_DATA
	USESEG	_BSS
	USESEG	GR_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE llgrp.inc
	INCLUDE idmac.inc

sBegin	_BSS
;
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externW B$LEOFST
externW B$REOFST
externB B$VLMASK
externB B$VRMASK
externD b$AddrC
externB b$AttrC
externB b$BitsPerPixel
externB b$DivShift
externB b$MaskC
externB b$MaskLeft
externB b$MaskRight
externW b$ModMask
externW b$OffC
externB b$PaintBorder
externW b$PenC
externW b$PixelsPerByte
externW b$SegC
;
; ***************************************************************************
; Global variables
; ***************************************************************************
;
globalW b$UpSub,,1		;subtract to try to move up
globalW b$DnSub,,1		;subtract to try to move down
globalW b$UpDnAdd,,1		;add if wrong quadrant/half for subtractor
;
; ***************************************************************************
; External function vectors
; ***************************************************************************
;
externW b$PutVector
externW b$Incr1
externW b$Incr2
;
; ***************************************************************************
; Local variables
; ***************************************************************************
;
staticW SaveCa,,1
staticB SaveCm,,1
staticW PutVectorM,,1		

sEnd	_BSS

assumes CS,GR_TEXT

sBegin	GR_TEXT

externNP B$BumpDS
externNP B$BumpES		
externNP B$DecDS		

;***
; B$CgaSetAttr
;
;Purpose:
;	Replicate the 1 bit attribute for 1-bit-per-pixel modes throughout
;	the attribute byte used by the graphics functions.  If the supplied
;	attribute is greater than 1, use 1.
;Entry:
;	AL = attribute
;Exit:
;	_bAttrC = 00 if AL was 0, else FF
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaSetAttr,<PUBLIC,NEAR>,<AX>
cBegin
	NEG	AL		;set carry iff non-0
	SBB	AL,AL		;AL = (AL==0) ? 0 : -1
	MOV	b$AttrC,al
	CLC			;exit no error
cEnd

;***
; B$CgaReadC
;
;Purpose:
;	Return the attribute of the current pixel as specified by
;	b$MaskC and b$OffC for CGA screen modes.
;Entry:
;	b$MaskC, b$OffC specify pixel to read
;Exit:
;	AL = attribute of specified pixel
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaReadC,<PUBLIC,NEAR>,<ES>
cBegin
	mov	al,b$MaskC	;get cursor mask
	les	bx,b$AddrC	;get memory address of cursor
	mov	cl,b$BitsPerPixel
	mov	ah,es:[bx]	;current cell value
	and	ah,al		;mask out other pixels (using b$MaskC)
RdLoop: shr	ax,cl		;shift right 1 pixel
	jnc	RdLoop		;loop till mask is right justified
	shl	ax,cl		;went once too far
	mov	al,ah		;return with attribute in [al]
cEnd


;***
; B$CgaSetC
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to
;	the current attribute for CGA modes.
;Entry:
;	b$PenC  = cursor mask and attribute
;	b$AddrC = address of pixel
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaSetC,<PUBLIC,NEAR>,<DS>
cBegin
	mov	cx,b$PenC	;[cl] = cursor mask, [ch] = attribute
	lds	bx,b$AddrC	;[BX] = cursor offset, [ES] = segment
	xor	ch,[bx] 	;change masked bits of video byte
	and	ch,cl		;  to color in attribute byte
	xor	[bx],ch
cEnd

;***
; B$CgaSetPixC
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to
;	the current attribute for CGA modes.  This is identical to
;	B$CgaSetC except that this routine assumes ES is set to
;	video segment.
;Entry:
;	ES	= video segment (set up by B$CgaSetPixFirstC)
;	b$PenC = cursor mask and attribute
;	b$OffC = address of pixel
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaSetPixC,<PUBLIC,NEAR>
cBegin
	mov	cx,b$PenC	;[cl] = cursor mask, [ch] = attribute
	mov	bx,b$OffC	;[BX] = cursor offset
				;[ES] = setup by SetPixFirstC
	xor	ch,es:[bx]	;change masked bits of video byte
	and	ch,cl		;  to color in attribute byte
	xor	es:[bx],ch	
cEnd

;***
; B$CgaSetPixFirstC/B$CgaSetPixLastC
;
;Purpose:
;	Set up ES to the video segment for CGA modes (FirstC).
;	LastC just returns as nothing needs to be done here for CGA modes.
;Entry:
;	b$SegC = video segment
;Exit:
;	ES set to video segment
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaSetPixFirstC,<PUBLIC,NEAR>
cBegin
	mov	es,b$SegC	;ES = video segment for b$SetPixC
labelNP <PUBLIC,B$CgaSetPixLastC>
cEnd

labelW	PutTable		;Put Vectors according to put action value
	DW	PutOr, PutAnd, PutPreset, PutPset, PutXor

labelW	PutTableM		;Put Vectors for "Middle" (full mask) PUTs
	DW	PutOrM, PutAndM, PutPresetM, PutPsetM, PutXorM	

;***
; B$CgaPutAction
;
;Purpose:
;	Set b$PutVector to appropriate PUT action routine for CGA modes.
;	Requested action is used to index into a table of entry points.
;Entry:
;	AL = PUT action [0..4] representing (OR, AND, PRESET, PSET, XOR)
;Exit:
;	b$PutVector set to entry point of appropriate PUT action routine
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaPutAction,<PUBLIC,NEAR>
cBegin
	xor	ah,ah		;make word index
	shl	ax,1
	mov	bx,ax
	mov	ax,cs:PutTable[BX]  ;get our vector
	mov	b$PutVector,ax ;save it
	mov	ax,cs:PutTableM[BX] ;get our Middle vector
	mov	PutVectorM,ax	;save it
cEnd

;***
; PutAnd
;
;Purpose:
;	Support routine for CGA PUT.  Write to the specified screen byte
;	the result of ANDing the given attribute with what was already
;	in the screen byte.
;Entry:
;	ES:DI = address of screen byte
;	AH    = attribute to AND, then write
;	DH    = pixel mask
;Exit:
;	ES:DI = address of next screen byte
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	PutAnd,<NEAR>
cBegin
	not	dh		;NEGATE [DH] AND USE NEGATIVE LOGIC
	or	ah,dh		;MAKE NON-SIG BITS ONES
	not	dh		;RESTORE [DH]
	and	es:[di],ah	;AND WITH SCREEN
	inc	di
cEnd

;***
; PutOr
;
;Purpose:
;	Support routine for CGA PUT.  Write to the specified screen byte
;	the result of ORing the given attribute with what was already
;	in the screen byte.
;Entry:
;	ES:DI = address of screen byte
;	AH    = attribute to OR, then write
;	DH    = pixel mask
;Exit:
;	ES:DI = address of next screen byte
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	PutOr,<NEAR>
cBegin
	and	ah,dh		;ISOLATE SIG BITS
	or	es:[di],ah	;OR WITH SCREEN
	inc	di
cEnd

;***
; PutPreset/PutPset/PutXor
;
;Purpose:
;	Support routine for CGA PUT.  Write to the specified screen byte
;	either the specified attribute unchanged (Pset), the one's complement
;	of the specified attribute (Preset), or the result of XORing the
;	specified attribute with what was already in the screen byte,
;	depending on the entry point.
;Entry:
;	ES:DI = address of screen byte
;	AH    = attribute to apply
;	DH    = pixel mask
;Exit:
;	ES:DI = address of next screen byte
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	PutPreset,<NEAR>
cBegin
	not	ah		;NEGATE DATA FOR PRESET
labelNP PutPset
	xor	ah,es:[di]	;XOR DATA WITH EXISTING SCREEN BYTE
labelNP PutXor
	and	ah,dh		;ISOLATE SIG BITS
	xor	es:[di],ah	;XOR WITH SCREEN
	inc	di
cEnd

	ASSUME	DS:NOTHING

;***
; B$CgaNReadL
;
;Purpose:
;	Read a line of pixels from the screen to an array for CGA modes.
;Entry:
;	DS:SI	= screen address
;	ES:DI	= array address
;	CL	= array align shift count
;	CH    = mask for last partial byte
;	BP	= count of bits (not pixels) to read
;Exit:
;	ES:DI	= updated to array byte past point filled
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaNReadL,<PUBLIC,NEAR>
cBegin
	mov	ah,[si] 	;preload hi byte
	inc	si		
NRdLoop:
	lodsb			;fill ax word with video bytes
	mov	bh,al		;this lo byte will become next hi byte
	rol	ax,cl		;align to array
	sub	bp,8		;8 bits done
	jbe	NRdLast 	;go if bit count exhausted
	mov	es:[di],ah	;save full byte
	inc	di		
	mov	ah,bh		;move lo byte (BH) to hi byte (AH)
	jnz	NRdLoop 	;loop if no offset overflow
	call	B$BumpES	;move array pointer over segment boundary
	jmp	short NRdLoop	;go do another
NRdLast:
	and	ah,ch		;strip unused bits from last byte
	mov	es:[di],ah	;save last byte
	inc	di		
	jnz	NRdDone 	
	call	B$BumpES	;move array pointer over segment boundary
NRdDone:
cEnd

;***
; B$CgaNWriteL
;
;Purpose:
;	Write a line of pixels from an array to the screen for CGA modes.
;Entry:
;	ES:DI	= screen address
;	DS:SI	= array address
;	CL	= array align shift count
;	BP	= count of bits (not pixels) to write
;	DL	= last partial byte mask
;	DH	= first partial byte mask
;Exit:
;	DS:SI	= updated to array byte past point used
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaNWriteL,<PUBLIC,NEAR>
cBegin
	mov	ah,[si] 	;preload byte from array
	inc	si
	jnz	NWrOvfl1	
	call	B$BumpDS	;move array pointer over segment boundary
NWrOvfl1:
	ror	ax,cl		;align to video
	add	bp,cx
	sub	bp,8		;account for first partial byte
	jbe	NWrLast 	;go if last byte
.erre	ID_SSEQDS		;assumes ss = ds
	call	ss:[b$PutVector]   ;put the byte
	mov	dh,0FFH 	;mask for whole bytes in the middle
.erre	ID_SSEQDS		;assumes ss = ds
	mov	bx,ss:[PutVectorM]  ;preload the "full mask" put routine
	jmp	short NWrLoop2	;go for more bytes
NWrLoop:
	jmp	bx		;vector to put the byte
;
;The following Put routines are identical to those documented above but
;are optimized for the middle loop where the mask is always 0FFH
;
;
; PutAndM:
;
PutAndM:
	and	es:[di],ah	;AND WITH SCREEN
	jmp	short PutEnd
;
; PutOrM:
;
PutOrM:
	or	es:[di],ah	;OR WITH SCREEN
	jmp	short PutEnd
;
; PutPresetM/PutPsetM
;
PutPresetM:
	not	ah		;NEGATE DATA FOR PRESET
PutPsetM:
	mov	es:[di],ah	;store data for PSET
	jmp	short PutEnd
;
; PutXorM
;
PutXorM:
	xor	es:[di],ah	;OR WITH SCREEN
PutEnd:
	inc	di		;bump to next screen byte
;
NWrLoop2:
	rol	ax,cl		;re-align to array
	lodsb			;fill ax word with array bytes
	or	si,si		;did pointer overflow segment?
	jz	NWrOvfl3	;go if so
NWrOvfl2:
	xchg	ah,al		
	ror	ax,cl		;align to video
	sub	bp,8		;8 bits done
	ja	NWrLoop 	;go if bit count not exhausted
	add	bp,8		;restore BP to #bits in last byte
	cmp	cx,bp		;did we use any of the second byte?
	jb	NWrLast 	;go if so
	or	si,si		;at start of segment?
	jnz	NWrUnfl 	;go if not
	call	B$DecDS	;backup to previous segment
NWrUnfl:			
	dec	si		;move ptr back
NWrLast:
	and	dh,dl		;combine first|middle mask with end mask
.erre	ID_SSEQDS		;assumes ss = ds
	call	ss:[b$PutVector]   ;put the byte
cEnd

NWrOvfl3:			
	call	B$BumpDS	;move array pointer over segment boundary
	jmp	short NWrOvfl2	;back to loop

	ASSUME	DS:DGROUP

;***
; B$CgaNSetC
;
;Purpose:
;	Set a horizontal line of pixels to the current attribute for CGA
;	modes.	The line starts at the current cursor position and moves right.
;
;	Discussion of QCG tiling:
;	Mode 6 tiling is relatively simple; to preserve the background
;	pixels just AND the cursor mask with the byte at BKG_MASK[B$TILNDX].
;	Mode 4/5 tiling is hairy because the fill mask is 8 pixels wide.
;	This covers two bytes per row, which is wider than the current
;	paint tiling code can do.  Because QCG only lets the user supply
;	the pixel on/off mask, as opposed to the actual pixel values, the
;	tile_mask and b$AttrC values are the same for both bytes (actually,
;	for all bytes, in CGA modes).  Only the bkg_mask has to be 16-bits
;	wide -- each row takes two consecutive bytes of bkg_mask.
;
;Entry:
;	b$AddrC specifies start pixel
;	b$PenC = cursor mask and attribute
;	BX	= number of pixels to set
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaNSetC,<PUBLIC,NEAR>,<ES,DI>
cBegin
	les	di,b$AddrC	;graphics cursor address
	mov	dx,b$PenC	;dl = cursor mask, dh = attribute
	or	dl,dl		;left aligned in byte?
	js	NSet2a		;go if so, skip single-bit start
NSet1:
	xor	ah,ah		;zero out new mask accumulator
	mov	cl,b$BitsPerPixel
NSet2:
	or	ah,dl		;include this pixel in mask
	dec	bx		;decrement pixel count
	jz	NSet4		;treat as last byte if bit count exhausted
	ror	dl,cl		;move 1 pixel right
	jnb	NSet2		;continue if not right-most bit
	mov	al,dh		;copy of attribute
	xor	al,es:[di]	;get bits that need to be changed
	and	al,ah		;mask in bits that need to be changed
	xor	es:[di],al	;update pixels
	inc	di		;bump cursor byte pointer
NSet2a:
	mov	ax,bx		;remaining bit count
	mov	cl,b$DivShift	;pixels/byte divisor shift
	shr	ax,cl		;compute full byte count
	jz	NSet3		;go do remaining bits if no full bytes
	xchg	ax,cx		;byte count to cx
	mov	al,dh		;attribute byte
	rep	stosb		;block write full bytes
NSet3:
	and	bx,b$ModMask	;mask in remaining bit count
	jz	NSet5		;no bits remaining - exit
	jmp	NSet1		;go do remaining bits
NSet4:				;update last byte
	xor	dh,es:[di]	;get bits that need to be changed
	and	dh,ah		;mask in bits that need to be changed
	xor	es:[di],dh	;update pixels
NSet5:
cEnd


;***
; B$CgaSetTile
;
;Purpose:
;	This routine stores the internal form of the current tile attribute.
;	This routine is called each time a row is to be painted in CGA modes.
;Entry:
;	BL = tile attribute
;Exit:
;	_bAttrC set to tile attribute
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaSetTile,<PUBLIC,NEAR>
cBegin
	MOV	b$AttrC,BL	 ;set attribute to the tile attribute
cEnd

;***
; ScanInit
;
;Purpose:
;	This routine does some initialization for both ScanL and ScanR
;	for CGA modes.
;Entry:
;	None
;Exit:
;	ES:DI = Video segment address (b$AddrC)
;	CH = cursor mask	      (b$MaskC)
;	DL = border attribute	      (b$PaintBorder)
;	DH = paint attribute	      (b$AttrC)
;	SI = already painted flag     (0)
;	BL = screen byte	      (read from ES:DI)
;	CL = b$BitsPerPixel
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	ScanInit,<NEAR>
cBegin
	les	di,b$AddrC	    ;di=cursor offset, es=video seg
	mov	ch,b$MaskC	    ;ch=cursor mask
	mov	dl,b$PaintBorder   ;dl=border attribute
	mov	dh,b$AttrC	    ;dh=paint attribute
	mov	cl,b$BitsPerPixel  ;shift count
	mov	bl,es:[di]	    ;load the screen byte or word
	xor	si,si		    ;clear already-painted-flag
cEnd

;***
; Helper
;
;Purpose:
;	This routine is used by the beginning and ending parts of
;	SCANL and SCANR. It returns with ZF set if a border pixel was
;	found, else it sets the already-painted-flag appropriately.
;Entry:
;	BL = screen byte
;	DL = border attribute
;	CH = pixel mask
;Exit:
;	If border found
;	    PSW.C set
;	else
;	    AL = screen byte with non-masked pixels set to attribute
;	    SI = non-zero value
;	    BP (paint count) incremented
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	Helper,<NEAR>
cBegin
	mov	al,bl		;get screen byte
	xor	al,dl		;xor with border
	and	al,ch		;mask out unwanted bits
	jz	helpxt1 	;quit if border
	mov	al,bl		;get screen byte in [al]
	xor	al,dh		;xor it with paint attribute
	and	al,ch		;mask out other bits
	or	si,ax		;set already painted flag
	inc	bp		;increment paint count
helpxt1:
cEnd

;***
; B$CgaScanL
;
;Purpose:
;	Scan left beginning with the pixel to the left of cursor,
;	and paint pixels until:
;		(1) the viewport edge is encounteered (edge painted)
;		(2) a border pixel is encountered (border not painted)
;
;Entry:
;	b$AddrC, b$MaskC = pixel to right of starting pixel
;	b$PaintBorder	   = attribute of paint region border
;	b$AttrC	   = attribute to paint
;	B$LEOFST, B$VLMASK   = left viewport edge
;Exit:
;	BX		   = number of pixels scanned
;	CL		   = 0 iff no pixels changed color
;	b$OffC, b$MaskC  = the last non-border pixel examined/painted
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaScanL,<PUBLIC,NEAR>,<SI,DX,DI,BP,ES>
cBegin
	call	ScanInit	;init regs for scanning
	mov	bp,si		;clear paint count

;	Prolog

	cmp	di,B$LEOFST	;cursor offset same as left edge offset ?
	jz	exit4		;Brif so and do the ending
	rol	ch,cl		;move left one pixel
begin1:
	JC	MIDDLE		;go if byte aligned
	call	Helper		;check if this pixel is a border pixel
				;else set the already painted flag
	jz	exit1		;brif if border pixel found
	rol	ch,cl		;move left by one pixel
	JMP	SHORT begin1	;else test the other pixels in this byte
middle:

;	Middle paint loop
;	Middle is used by all screen modes
middle0:

	sub	di,1		;move left one byte
	jb	exit2		;brif underflow
	mov	bl,es:[di]	;get the screen byte
	cmp	di,B$LEOFST	;are we at left edge ?
	je	exit3		;brif so
	mov	al,bl		;screen byte in [al]
	xor	al,dl		;xor it with border
tstbdr1:
	test	al,ch		;border encountered ?
	jz	exit3		;brif border
	rol	ch,cl		;prepare mask to test next pixel
	jnb	tstbdr1 	;test the next pixel
	mov	al,bl		;[al] = screen byte ****
	xor	al,dh		;xor screen byte with paint attribute
	or	si,ax		;set the already-painted-flag
	add	bp,b$PixelsPerByte ;increment paint count by # of
				;pixels in a byte
	JMP	SHORT middle0	;go do the rest

;	Epilog
;	branches here from the beginning part

exit1:
	ror	ch,cl		;back up so not on border
	jnb	final_exit	;brif no carry
	JMP	SHORT exit21

;	branches here if [di] goes < 0

exit2:
	mov	ch,b$MaskLeft	;set mask with the leftmost
				;bit/bits equal to 1
exit21:
	inc	di		;back up
	JMP	SHORT final_exit

;	branches here if [di] is same as B$LEOFST or
;	if a border pixel was found in the middle loop

exit3:
	mov	cl,b$BitsPerPixel
	mov	ch,b$MaskRight ;make the rightmost bit/bits equal to 1
exit31:
	call	Helper
	jz	exit1		;brif border found
	rol	ch,cl		;move left by one pixel
	jb	exit41		;this is a special case
	cmp	ch,B$VLMASK	;compare bit addresses
	ja	exit1		;brif done
	JMP	SHORT exit31	;else continue

;	branches here if we start with cursor offset equal
;	to B$LEOFST (right at the beginning)

exit4:
	rol	ch,cl		;move left by one pixel
;	jnb	exit31		;continue if not the left most pixel
	JB	EXIT41		;if leftmost pixel, then jump
	CMP	CH,B$VLMASK	;test if past the viewport on left
	JNA	EXIT31		;if not, then jump
exit41:
	ror	ch,cl

;	this is where NSetC gets called to do the actual painting

final_exit:
	mov	bx,bp		;paint count in [bx]
	or	bx,bx		;is paint count 0 ?
	jz	fexit1		;branch around NSetC if so
	mov	b$OffC,di	;return cursor offset
	mov	b$MaskC,ch	;return cursor mask
	call	B$CgaNSetC	;paint them
fexit1:
	mov	bx,bp		;return paint count
	mov	cx,si		;return already-painted-flag
cEnd

;***
; B$CgaScanR
;
;Purpose:
;	Starting with the current pixel, search right until:
;		(1) a non-border pixel is found
;		(2) [DX] pixels have been tested
;		(3) the viewport edge is encountered
;
;	If (2) or (3) terminated the scan, exit with:
;		DX = remaining border bount = 0
;
;	If (1) terminated the scan, scan and paint non-border pixels until:
;		(1) the viewport edge is encountered (edge painted)
;		(2) a border pixel is encountered (border not painted)
;
;Entry:
;	DX		   = count of border pixels which may be skipped
;	b$AddrC, b$MaskC = starting pixel
;	b$PaintBorder	   = attribute of paint region border
;	b$AttrC	   = attribute to paint
;	B$REOFST, B$VRMASK   = right viewport edge
;Exit:
;	BX		   = number of pixels painted
;				(whether or not they changed color)
;	CL		   = 0 iff no pixels changed color
;	DX		   = remaining border pixel count
;	b$OffC, b$MaskC  = the last non-border pixel examined/painted
;	SI, AL		   = the first non-border pixel encountered
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaScanR,<PUBLIC,NEAR>,<DI,BP,ES>
cBegin
	mov	bp,dx		;save skip count in [bp]
	call	ScanInit	;init regs for scanning
scnr0:
	mov	al,bl		;screen byte in [al]
	xor	al,dl		;xor it with border
scnr1:
	test	al,ch		;border pixel or not ?
	jnz	begin_paint
	dec	bp		;decrement border count
	jz	scnrxt01	;brif border count zero
	ror	ch,cl		;move right by one pixel
	jnb	scnr1
	inc	di		;move right by one byte
	mov	bl,es:[di]	;screen byte in [bl]
	cmp	di,B$REOFST	;cursor offset same as right edge offset
	jnz	scnr0		;brif so....one more byte to go
	cmp	ch,B$VRMASK	;compare bit addresses
	jae	scnr0		;brif within screen limits
	xor	bp,bp		;pretend to run out of skip count
scnrxt01:
	jmp	scnrxt0

;	We begin painting here. Again the paint loop is in three
;	parts: prolog, main body, and epilog.

begin_paint:
	mov	SaveCa,di	;saving SaveCa and
	mov	SaveCm,ch	;  SaveCm
	mov	b$OffC,di	;set the cursor offset
	mov	b$MaskC,ch	;set the cursor mask
	push	bp		;save border count
	xor	bp,bp		;clear paint count
	test	ch,80h		;cursor byte aligned ?
	JNZ	SRMIDDLE0	;go if so

; PROLOG

begpaint1:
	call	Helper		;calls helper1 increments
				;paint count and sets painted flag
	jz	scnxtn1 	;brif border found
	cmp	di,B$REOFST	; cursor offset = right edge offset?
	jne	begpaint2	; brif not
	cmp	ch,B$VRMASK	; compare bit addresses
	je	scnxtn1		; brif edge of viewport
	ja	begpaint2	; loop if not edge yet
	xor	bp,bp		; special case:  nothing to paint
	jmp	short scnxtn1	
begpaint2:			
	ror	ch,cl		;move right by one pixel
	JNC	BEGPAINT1	;go if not byte aligned

; MAIN BODY

	INC	DI		;to next byte
SRMIDDLE0:
	cmp	b$BitsPerPixel,2
	je	SRMIDDLE2

srmiddle:
	mov	bl,es:[di]	;[bl] = screen byte
	cmp	B$REOFST,di	;are we at the right edge ?
	JBE	SCNRXTCHK	;brif so or beyond
	MOV	AL,BL		;copy screen byte
	NOT	AL		;bitwise equivalence with border color
	XOR	AL,DL		;  a bit in a pixel will be 1 if pixel
				;  is border color
	JNZ	BEGPAINT1	;go if border color found in the byte
	XOR	BL,DH		;detect any differences from paint color
	OR	SI,BX		;combine with the already painted flag
				;only LOByte is significant
	ADD	BP,8		;increment paint count
	INC	DI		;move right by one byte
	JMP	SHORT srmiddle	;go do the rest

SRMIDDLE2:
	MOV	AH,55H
SRMIDDLE3:
	MOV	BL,ES:[DI]	;get screen byte
	CMP	B$REOFST,di	;at the right edge or beyond?
	JBE	SCNRXTCHK	;go if so
	MOV	AL,BL		;copy screen byte
	NOT	AL		;bitwise equivalence with border color
	XOR	AL,DL		;  both bits in a pixel will be 1 if pixel
	MOV	BH,AL		;  is border color
	SHR	BH,1		;shift left bit in copy on to right bit
	AND	BH,AH		;mask off the trash
	AND	BH,AL		;see if both bits set in any pixel
	JNZ	BEGPAINT1	;go if border color found in the byte
	XOR	BL,DH		;detect any differences from paint color
	OR	SI,BX		;combine with the already painted flag
				;only LOByte is significant
	ADD	BP,4		;increment paint count
	INC	DI		;to next byte
	JMP	SHORT SRMIDDLE3 ;loop until border color or edge found

; EPILOG

SCNRXTCHK:
	jz	scnrxt3
	mov	cl,b$BitsPerPixel  ;[cl] = screen bits/pixel
	rol	ch,cl		;back up
	inc	di		;move right a byte
scnxtn1:
	JMP	SHORT scnrxt2	;start painting

scnrxt3:
	mov	ch,b$MaskLeft	;set the leftmost bit/bits
scnrxt31:
	call	Helper		;calls helper1 increments
				;paint count and sets painted flag
	jz	scnrxt2 	;brif border
	ror	ch,cl		;move right by one pixel
	jb	scnrxt1 	;special case ??????
	CMP	DI,B$REOFST	;test if on right viewport byte
	JNE	SCNRXT31	;if not, then continue on
	cmp	ch,B$VRMASK	;compare bit addresses
	jb	scnrxt1 	;brif edge encountered
	JMP	SHORT scnrxt31

scnrxt1:
	rol	ch,cl		;back up

scnrxt2:
	push	cx
	mov	bx,bp		;paint count
	or	bx,bx		;paint count = 0 ?
	jz	no_nset 	;branch around NSetC
	call	B$CgaNSetC
no_nset:
	mov	bx,bp		;return paint count
	pop	cx
	mov	b$OffC,di	;return cursor offset
	mov	b$MaskC,ch	;return cursor mask
	pop	dx		;return skip count
	mov	cx,si		;return already-painted -flag
	JMP	SHORT scnrxt

scnrxt0:
	mov	bx,bp		;paint count = 0
	mov	dx,bp		;skip count  = 0
	mov	cl,bl		;already-painted-flag = 0
	mov	b$OffC,di	;return cursor offset
	mov	b$MaskC,ch	;return cursor mask
scnrxt:
	mov	si,SaveCa	;returning SaveCm and
	mov	al,SaveCm	;  SaveCm
cEnd

;***
; LineSetup
;
;Purpose:
;	This routine is called at the beginning of LineX, LineY, and LineV.
;	It handles the buffer half splitting of the CGA and quadrant row
;	splitting of the HGC.
;	(See discussion of the architecture in the LLCGA and LLHGC headers).
;Entry:
;	DX	 = negative for up, otherwise down
;Exit:
;	DX	 = up/down move assumption
;Uses:
;	none.
;Exceptions:
;******************************************************************************
cProc	LineSetup,<NEAR>
cBegin
	OR	DX,DX		;Y difference negative? (up)
	MOV	DX,b$UpSub	;assume so, use UP subtractor
	JS	LineSetupExit	;go if correct assumption
	MOV	DX,b$DnSub	;no, use DOWN subtractor
LineSetupExit:			
cEnd

;***
; B$CgaLineX
;
;Purpose:
;	Draw an X-major line for CGA modes.
;Entry:
;	AH    = color (b$AttrC)
;	AL    = bit accumulator
;	BX    = point count
;	CH    = bit mask
;	CL    = trash for twiddle and pixel shift count
;	DX    = BP change for Y movement (UpSub or DnSub)
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr1 = major axis delta update value
;	Incr2 = minor axis delta update value
;	UpDnAdd = corrector for DX subtraction
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaLineX,<PUBLIC,NEAR>
cBegin
	CALL	LineSetup	

	XCHG	BX,CX		;point count to BX
	MOV	CL,b$BitsPerPixel

LineCXloop:

	ROL	DI,1		;next line style bit
	JNC	LineCX2 	;go if bit is 0 not to plot

	OR	AL,CH		;OR this bit into byte mask
LineCX2:
	OR	SI,SI		;time to move in Y (+ or 0 delta)?
	JNS	LineCX4 	;go if so
	ADD	SI,b$Incr1	;update delta for X movement
	ROR	CH,CL		;move to next X
	JC	LineCX3 	;go if not still in same byte
	DEC	BX
	JNZ	LineCXloop	;go for more
	JMP	SHORT LineCX7	;  or exit
LineCX3:
	MOV	CL,AH		;get color, dump accumulated pixels
	XOR	CL,ES:[BP]	;change masked bits of video byte
	AND	CL,AL		;  to color in attribute byte
	XOR	ES:[BP],CL	;(twiddle)
	XOR	AL,AL		;clear pixel accumulator
	MOV	CL,b$BitsPerPixel  ;reload bits per pixel
	INC	BP		;go to next byte
	DEC	BX
	JNZ	LineCXloop	;go for more
	JMP	SHORT LineCX7	;  or exit
LineCX4:
	ADD	SI,b$Incr2	;update delta for Y movement
	MOV	CL,AH		;get color, dump accumulated pixels
	XOR	CL,ES:[BP]	;change masked bits of video byte
	AND	CL,AL		;  to color in attribute byte
	XOR	ES:[BP],CL	;(twiddle)
	XOR	AL,AL		;clear pixel accumulator
	MOV	CL,b$BitsPerPixel  ;reload bits per pixel
	ROR	CH,CL		;move to next X
	ADC	BP,0		;(+1 if next X byte)
	sub	BP,DX		;make the assumed Y movement
	jnb	LineCX5 	;go if no problem
	add	BP,b$UpDnAdd	;undo SUB + perform correct movement
LineCX5:
	DEC	BX
	JNZ	LineCXloop	;go for more
LineCX7:			;flush accumulated pixels
	XOR	AH,ES:[BP]	;change masked bits of video byte
	AND	AH,AL		;  to color in attribute byte
	XOR	ES:[BP],AH	;(twiddle)
cEnd

;***
; B$CgaLineY
;
;Purpose:
;	Draw a Y-major line for CGA modes.
;Entry:
;	AH    = color (b$AttrC)
;	AL    = bit mask
;	BX    = point count
;	CH    = trash for twiddle
;	CL    = pixel shift count
;	DX    = BP change for Y movement (UpSub or DnSub)
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr1 = major axis delta update value
;	Incr2 = minor axis delta update value
;	UpDnAdd = corrector for DX subtraction
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaLineY,<PUBLIC,NEAR>
cBegin
	call	LineSetup	

	XCHG	BX,CX		;point count to BX
	MOV	CL,b$BitsPerPixel

LineCYloop:

	ROL	DI,1		;next line style bit
	JNC	LineCY2 	;go if bit is 0 not to plot

	MOV	CH,AH		;get color, dump accumulated pixels
	XOR	CH,ES:[BP]	;change masked bits of video byte
	AND	CH,AL		;  to color in attribute byte
	XOR	ES:[BP],CH	;(twiddle)
LineCY2:
	OR	SI,SI		;time to move in X (+ or 0 delta)?
	JNS	LineCY3 	;go if so
	ADD	SI,b$Incr1	;update delta for Y movement
	sub	BP,DX		;make the assumed Y movement
	jnb	LineCY2A	;go if no problem
	add	BP,b$UpDnAdd	;undo SUB + perform correct movement
LineCY2A:
	DEC	BX
	JNZ	LineCYloop
	ret
LineCY3:
	ADD	SI,b$Incr2	;update delta for X movement
	ROR	AL,CL		;move to next X
	ADC	BP,0		;(+1 if next X byte)
	sub	BP,DX		;make the assumed Y movement
	jnb	LineCY5 	;go if no problem
	add	BP,b$UpDnAdd	;undo SUB + perform correct movement
LineCY5:
	DEC	BX
	JNZ	LineCYloop	;go for more
cEnd

;***
; B$CgaLineV
;
;Purpose:
;	Draw a vertical line for CGA modes.
;Entry:
;	AH = color (b$AttrC)
;	AL = bit mask
;	BH = trash for twiddle
;	BL = unused
;	CX = point count
;	DX = BP change for Y movement (UpSub or DnSub)
;	SI = UpDnAdd = corrector for DX subtraction
;	DI = line style
;	BP = video offset
;	ES = video segment
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$CgaLineV,<PUBLIC,NEAR>
cBegin
	call	LineSetup	

	MOV	SI,b$UpDnAdd	;to register here

LineCVloop:

	ROL	DI,1		;next line style bit
	JNC	LineCV2 	;go if bit is 0 not to plot

	MOV	BH,AH		;get color, dump accumulated pixels
	XOR	BH,ES:[BP]	;change masked bits of video byte
	AND	BH,AL		;  to color in attribute byte
	XOR	ES:[BP],BH	;(twiddle)
LineCV2:
	sub	BP,DX		;make the assumed Y movement
	jnb	LineCV5 	;go if no problem
	add	BP,SI		;undo SUB + perform correct movement
LineCV5:			
	LOOP	LineCVloop	;go for more
cEnd

sEnd	GR_TEXT

	END
