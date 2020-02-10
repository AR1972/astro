;	TITLE	RWCHAR - Character read/write for MSHERC.
;***
;RWCHAR
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	Single character read and write.
;
;******************************************************************************

	include	hgcdefs.inc

code            segment para public 'code'
                assume  cs:code,ds:code

Public	GReadAttrChar
Public	GWriteChar

Extrn	WriteGrChar:near
Extrn	MapGraphXYToVideoOffset:near
Extrn	CharBuff:byte
Extrn	HFont:byte
Extrn	ZERO:word
Extrn	CharHeight:byte

;-------------------------------------------------------------------------------
; A  WRITE CHARACTER ONLY AT CURRENT CURSOR POSITION
;	 BH => Display Page
;	 CX => Count of chars to write
;	 AL => Character
;-------------------------------------------------------------------------------
GWriteChar	proc	near
	mov	bh,1		;use attribute 1??
;-----Write the Graphics Character to the screen-----
	jmp	WriteGrChar
GWriteChar	endp

;-------------------------------------------------------------------------------
; 8  READ ATTRIBUTE/CHARACTER AT CURRENT CURSOR POSITION
;	 BH => Display Page
;	 AL <= Character
;-------------------------------------------------------------------------------
GReadAttrChar	proc	near
;------ Calculate the graphic y coordinate ------
	mov	bl,bh				;bx = page number
	xor	bh,bh
	mov	cx,Scr1_Buf_Seg 		;assume page 1
	or	bl,bl
	jnz	RdPg1				;go if was page 1
	mov	cx,Scr0_Buf_Seg 		;use page 0
	push	cx				;save page # segment for later
RdPg1:
	shl	bx,1
	mov	bx,es:BIOSCURS[bx]		;Fetch start coords.
	mov	al,bh				;ax = row coordinate
	xor	ah,ah
	mul	CharHeight			;Calculate y coordinate
	push	ax				;Save y coordinate

;------ Character width is 9 bits ----
	xor	bh,bh		;bx = column coordinate
	mov	cx,bx
	shl	bx,1		;cols*2
	shl	bx,1		;cols*4
	shl	bx,1		;cols*8
	add	cx,bx		;cols*9 = start x coordinate

;------ Calculate the video buffer offset ------
	pop	dx			;Retrieve the y coordinate
	Call	MapGraphXYToVideoOffset	;Calculate buffer offset
	mov	di,cx			;Pnt di to video offset
	mov	bl,dl			;bl = bit position
	neg	bl			;bl = - x mod 8
	add	bl,7			;bl = 7 - x mod 8 = bit position

	pop	es			;set ES for specified page
	xor	cx,cx
	mov	cl,14			;cx = char height
	mov	si,offset CharBuff	;place to store graphics char
	mov	dx,1			;DH=accumulate non-0 9th bits
					;DL=accumulate equal 9th bits
RdCharLp:
	mov	ah,es:[di]		;get first byte of char
	mov	al,es:[di+1]		;  and second
	xchg	cx,bx
	sal	ax,cl			;left justify both bytes in word
	xchg	cx,bx
	mov	[si],ah 		;save first byte for comparison
	inc	si

	rol	al,1			;move 9th bit to least sig bit
	or	dh,al			;non-0 9th bits
	xor	al,ah			;AL=0 where AL==AH
	not	al			;AL=1 where AL==AH
	and	dl,al			;DL stays 1 if 9th bit == 8th bit

	add	di,GrNextScan		;Inc to next scan
	jns	ScanOk			;Go to check for more rows
	sub	di,NextScanAdjust	;Pnt di back on screen
ScanOk:
	loop	RdCharLp		;read entire character

	push	dx			;save 9th bit info
;	setup for font table compare
	xor	cx,cx
	mov	cl,14				;(cx) = font size
	mov	dx,cx				;(dx) = font size copy
	push	ds
	pop	es
	mov	di,offset HFont 		;(es:di) = @font table
	add	di,cx				;skip <NULL> in font table
	mov	ax,1				;(ax) = ascii of current char

;	find match loop
MatchLp:
	push	di				;save current position
	mov	si,offset CharBuff		;(ds:si) = data buffer
   repe cmpsb					;cmp buffer with current char
	pop	di
	jz	Found				;if match, exit loop
	inc	ax				;if not, advance ascii count
	mov	cx,dx				;restore font size
	add	di,cx				;advance to next font entry
	cmp	ax,0ffh 			;at end of font table?
	jbe	MatchLp 			; if not, loop
	xor	ax,ax				;if no match, ascii=0
Found:
	pop	dx				;recover 9th bit info
	cmp	al,179			;such chars are in range (179-223)
	jbe	NotBlock		;go if not
	cmp	al,223
	ja	NotBlock		;go if not
	or	dl,dl			;DL=1 if all 9th bits == 8th bits
	jnz	RdDun			;go if so, OK block character
RdBad:
	xor	ax,ax			;invalid block character
	jmp	short RdDun
NotBlock:
	and	dh,1			;any non-zero 9th bits
	jnz	RdBad			;go if so, not good for normal char
RdDun:
	mov	FunData.FunAX,AX
	mov	es,ZERO
	Ret	;Finished Read A Character/Attribute
GReadAttrChar	endp

code	ends
	end
