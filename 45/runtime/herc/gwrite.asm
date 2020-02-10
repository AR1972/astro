;	TITLE	GWRITE - Character writing for MSHERC.
;***
;GWRITE
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Paint characters onto the screen for text writes in graphics mode.
;
;******************************************************************************

	include	hgcdefs.inc

code	segment	para	public	'code'

Public	WriteGrChar

Extrn	ZERO:word
Extrn	MapGraphXYToVideoOffset:near ;Graph x,y coord conversion to video off
Extrn	DeltaCols:word
Extrn	BitPos:byte
Extrn	LEdge:byte,REdge:byte,DstEdgeMasks:word
Extrn	HFont:byte
Extrn	CharHeight:byte

assume	cs:code,ds:code

;-------------------------------------------------------------------------------
; 9  WRITE ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION
;	 BH => Display Page
;	 CX => Count of chars to write
;	 AL => Character
;	 BL => Color, bit 7 causes use of XOR operation for write
;-------------------------------------------------------------------------------
WriteGrChar	proc	near
;-----If char count is 0, then terminate procedure-----
	or	cx,cx		;Null char count?
	jnz	GoGWrChar	;No, write chars
	jmp	NoGWrChar	;Yes, exit procedure

GoGWrChar:
	mov	ah,bl		;AH = attribute, AL = character
	push	ax		;save 'em

	mov	dx,Scr0_Buf_Seg		;Assume graphics page 0
	test	bh,1			;Are we on page 1?
	jz	SetVidPtr		;No, pnt to page 0
	mov	dx,Scr1_Buf_Seg		;Yes, pnt to page 1
SetVidPtr:

;------ Calculate the graphic y coordinate ------
	mov	bl,bh		;bx = page number
	xor	bh,bh
	shl	bx,1
	mov	bx,es:BIOSCURS[bx]		;Fetch start coords.
	mov	al,bh				;ax = row coordinate
	xor	ah,ah
	mul	CharHeight			;Calculate y coordinate
	push	ax				;Save y coordinate

	mov	es,dx			;Point es to Video Buffer Segment

;------ Character width is 9 bits, finish calculating write parameters----
	xor	bh,bh		;bx = column coordinate
	mov	ax,bx
	shl	bx,1		;cols*2
	shl	bx,1		;cols*4
	shl	bx,1		;cols*8
	add	bx,ax		;cols*9 = start x coordinate

	mov	dx,cx		;dx = char count
	shl	cx,1		;cnt*2
	shl	cx,1		;cnt*4
	shl	cx,1		;cnt*8
	add	dx,cx		;dx = cnt*9 = width in bits
	mov	cx,dx		;cx = width in bits
	add	cx,bx		;width + start x - 1 = stop x
	dec	cx		;cx = stop x

	mov	ax,bx		;ax = start x
	shr	ax,1		;ax = start x
	shr	ax,1
	shr	ax,1
	mov	dx,cx		;dx = stop x
	shr	dx,1		;dx = stop byte
	shr	dx,1
	shr	dx,1
	sub	dx,ax		;dx = number of bytes effected
	inc	dx		;	(both whole and partial)
	mov	DeltaCols,dx	;save it

;------ Calculate the right edge mask for the background loop----
	xchg	bx,cx		;bx = stop x, cx = start x
	and	bx,7		;bx = x mod 8
	neg	bx		;bx = - x mod 8
	add	bx,7		;bx = 7 - x mod 8 = bit position
	mov	al,REdge[bx]	;Fetch the right edge mask
	mov	byte ptr DstEdgeMasks[1],al	;Save the right edge mask

;------ Calculate the video buffer offset ------
	pop	dx			;Retrieve the y coordinate
	Call	MapGraphXYToVideoOffset	;Calculate buffer offset
	mov	di,cx  			;Pnt di to video offset
	mov	bx,dx			;bx = bit position
	mov	BitPos,bl		;save the start bit position
	mov	al,LEdge[bx]		;Fetch the left edge mask
	mov	byte ptr DstEdgeMasks,al ;Save the edge mask

;------- Set up for the character/attribute write------
	mov	bx,DstEdgeMasks 	;bx = left and right edge masks
	or	bl,bl
	jz	NoLeftEdge
	dec	DeltaCols		;adjust for whole byte count
NoLeftEdge:
	or	bh,bh
	jz	NoRightEdge
	dec	DeltaCols		;adjust for whole byte count
NoRightEdge:
	pop	ax			;refresh char/attr
	push	ax

;------ Clear out the char fill area with the background AND loop ------
	xor	ch,ch
	mov	cl,14			;Fetch font char height
	push	cx			;Save the # scan rows for fore loop

	push	di			;Save 1st scan beg ptr
	test	ah,80H			;test attr for XOR request
	jnz	DoXor9
	xor	al,al			;Use 0 as background
BackLp:
	push	cx			;Save scan row count
	mov	cx,DeltaCols		;Fetch the whole byte count
	mov	dx,di			;Save screen ptr

	or	bl,bl			;Is there a left edge to handle?
	jz	BackMid 		;No, do whole bytes

	and	es:[di],bl		;AND the left edge
	inc	di
BackMid:
    rep stosb				;Clear whole bytes

	or	bh,bh			;Is there a right edge to handle?
	jz	BackNext		;No, do another line
	and	es:[di],bh		;AND the right edge
BackNext:
	mov	di,dx			;Fetch beginning of row ptr
	add	di,GrNextScan		;Inc to next scan
	jns	FetchRCnt		;Go to check for more rows
	sub	di,NextScanAdjust	;Pnt di back on screen
FetchRCnt:
	pop	cx			;Fetch scan row count
	loop	BackLp			;If more, clear next scan

DoXor9:

;----- Now OR in the foreground portion of the character -----
	mov	dx,DstEdgeMasks 	;bx = left and right edge masks
	or	dl,dl
	jz	NoLeftOr
	inc	DeltaCols		;adjust for whole + first byte count
NoLeftOr:
	or	dh,dh
	jz	NoRightOr
	not	dh
NoRightOr:

	pop	di			;Retrieve beg. screen ptr
	pop	cx			;Retrieve scan row count
	mov	bl,BitPos		;Fetch the starting bit position
	inc	bl			;plus 1 for shift count
	pop	ax			;al = character code
	xor	dl,dl			;DL=0 means not block graphic char
	cmp	al,179			;such chars are in range (179-223)
	jbe	NotBlock		;go if not
	cmp	al,223
	ja	NotBlock		;go if not
	inc	dl			;DL=1 means it is block graphic char
NotBlock:
	mul	cl			;scan cnt*char code=char offset
	mov	si,offset HFont 	;Fetch the graphics font ptr
	add	si,ax			;Point to specified character

ForeLp:
	push	cx			;Save the scan row count
	lodsb				;Fetch the current font char byte
	or	al,al			;save time for empty scan lines
	jz	NullByte
	mov	ah,dl			;AH=1 for block graphic chars
	and	ah,al			;copy rightmost bit, if so
	ror	ah,1			;move to where it will rotate in right
	mov	cl,bl
	rol	ax,cl			;Rotate the char byte into position
	push	di			;Save beg. of scan row ptr
	mov	cx,DeltaCols		;Fetch whole byte count
ForeInLp:
	xor	es:[di],ah		;Or in high byte of fore char
	mov	bh,al
	shr	ax,1			;Rotate char to next bit position
	or	ah,bh
	inc	di			;Point to next byte
	loop	ForeInLp		;If more chars, continue
	and	ah,dh
	xor	es:[di],ah		;Or in high byte of fore char

	pop	di			;Retrieve beg of scan ptr
NullByte:
	add	di,GrNextScan		;Pnt to next scan line
	jns	GetRCnt			;Ptr on screen, no extra adjust
	sub	di,NextScanAdjust	;Pnt di back on screen
GetRCnt:
	pop	cx			;Fetch remaining row count
	loop	ForeLp			;Continue if more rows

	push	cs			;restore ES & DS
	pop	ds
	mov	es,ZERO
NoGWrChar:
	Ret	;Finished Write A Character/Attribute
WriteGrChar	endp

code	ends
	end
