;* included in:
;*		tandy, ibmcvt

NonStandard	FvmGetCur
NonStandard	ModeGetCur
NonStandard	FInitCsd
NonStandard	DoUpdateCsd


;********** FvmGetCur **********
;*	entry:	DS:DI => driver data
;*	* if first time called, identify the current screen and set fvmCur
;*	*  subsequent calls will return fvmCur
;*	*  After this call: fvmCur is initialized
;*	exit:	AL == fvm for current screen (0 => no supported screen found)
;*		AH == monitor

cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>, <DS>
cBegin	FvmGetCur

	AssertEQ di,OFF_lpwDataCsd
	mov	ax,-1
	mov	[di].fvmCurAdap,al
	mov	[di].fvmCurDisp,ah

cEnd	FvmGetCur



;********** ModeGetCur *********
;*	entry:	n/a
;*	* get current machine mode
;*	exit:	al = mode, ah = ayMac (or 0 if unknown)

cProc	ModeGetCur, <NEAR, PUBLIC, ATOMIC>, <ES>
cBegin	ModeGetCur

	push	bx
	mov	ah,0fh
	int	10h			;* get current state, return al = mode
	and	al,7Fh			;* mask off clear video buffer bit.
	pop	bx
	mov	ah,25			;default

cEnd	ModeGetCur


;********** FInitCsd **********
;*	entry:
;*		pinch = near pointer to INCH structure to fill
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <ds,di>
    parmDP pinst
    parmDP pinch
    localB modeCur
cBegin	FInitCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment

;*	* save old mode
	cCall	ModeGetCur			;* al = mode, ah = ayMac
	mov	modeCur,al			;* current mode

	mov	bx,pinst
	cmp	ds:[bx].ayMacInst,ah
	je	@F				;* same resolution
	mov	modeCur,0			;* cause mode reset
@@:

;*	* set mode
	mov	[di].pinstDrv,bx
	mov	bx,ds:[bx].pdmInst		;* CS:BX => DM info

;*	* copy mode info into driver globals
	mov	ax,cs:[bx].vparmCursOnDm
	mov	[di].vparmCursOn,ax
	mov	[di].vparmCursSize,ax
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax

	mov	al,cs:[bx].modeDm
	cmp	al,modeCur
	jne	@F				
	jmp	finitdone			;same mode, no reset
@@:
	xor	ah,ah				;* set mode
						;* REVIEW: always clear RGEN ??
	mov	al,cs:[bx].modeDm
	int	10h

	mov	ax,40H
	mov	es,ax
	mov	al,cs:[bx].ayMacDm
	dec	al				; rows - 1
	mov	byte ptr es:[0084H],al		;* update BIOS rows
finitdone:
	test	cs:[bx].finstDm,finstGraphics
	jz	@F

	mov	[di].SEG_lpbFont,segROM		;load PC ROM Font 
	mov	[di].OFF_lpbFont,0FA6Eh
	mov	[di].ayBox,8
	jmp short exitF
@@:
;*	* normally the INCH array would be copied (but since it is already
;*	*  setup in DATA just leave it alone).

;*	* Do other diddling
	cCall	DiddleBlinkBit
exitF:
	mov	ax,sp				;* success
cEnd	FInitCsd


;********** DoUpdateCsd **********
;*	entry:
;*		parameters just like PrepUpdateCsd
;*	* after primary buffer has been updated
;*	* For BIOS version -- send to screen
;*	* for Kanji -- parse for DBCS boundaries (OAX)
;*	exit: n/a

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, ES, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax			;* will never be 0
    parmW offFirst
    parmW fRestoreDbcs		;* => restore DBCS 1/2 characters
    localB axCurs
    localB axCurrent
    localB flastattr		;check repeat attribute
    localB ifgd			;needed for mode A (640x200x4)
    localB ibkgd
    localB curattr
    localW wTemp
    localV LocalChar,8		;storage of char bit map for ffont mode
    localB iMode
cBegin	DoUpdateCsd
	
	cmp	dax,0
	ja	DoUpdateChar

	jmp	end_update
DoUpdateChar:
	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv

	test	ss:[bx].finstInst,finstGraphics	;graphics text 8x14
	jnz	graf_update
	jmp	done_graf_update

graf_update:

	mov	ax,[di].posCurs			;for cursor drawing
	mov	axCurs,-1
	mov	axCurrent,0
	cmp	byte ptr [di].fCurs,0		;cursor on ?
	je	NoCursUpdate

	cmp	ah,ayLine			;check if cursor on ayLine
	jne	NoCursUpdate

	cmp	al,axFirst			;check if cursor outside axFirst
	jb	NoCursUpdate

	mov	axCurs,al			;need to update cursor
	mov	al,axFirst
	mov	axCurrent,al
NoCursUpdate:	
	mov	si,ds:[bx].pdmInst		
	mov	dl,cs:[si].modeDm		;save video mode in dl
	mov	iMode,dl
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer

;*	* DS:SI => start character in primary buffer
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

;***** Graphics Text ****
;*			*
;*			*

	mov	flastattr,0		;initialize with black:black
	mov	cx,ss:[di].ayBox
	shr	cx,1			;memory interleave addressing
	xor	ax,ax			;calculate start of screen address
	mov	al,ayLine
	mul	cx
	mov	cx,80
	mul	cx
	xor	cx,cx
	mov	cl,axFirst
IFDEF TANDYCSD
	cmp	iMode,0Ah
	jne	@F
	shl	cx,1			;Tandy's 4-color mode, 2 bytes/char
@@:
ENDIF
	add	ax,cx			;ax = ayLine x 80 x ayBox + axFirst
	mov	di,ax			;point to video buffer
	xor	cx,cx
	mov	cl,dax			;no. of char to be updated
	mov	si,offFirst

;* 640x200x2(x4) graphics
	
out_next_char1:
	lodsw				;load character+attribute
	mov	curattr,ah		;save color attribute
	push	cx
	mov	wTemp,di		;save di
	mov	di,OFF_lpwDataCsd	
	mov	cx,ss:[di].ayBox	;calculate character table address

	push	ds			;save pointer to primary buffer
	push 	si

	mov	si,ss:[di].OFF_lpbFont	;default
	mov	ds,ss:[di].SEG_lpbFont

	mov	dx,ax			;save char + attribute
	cmp	al,80h
	jb	first128
		     			;use extend font (128 - 255)
	push	cs
	pop	ds
	mov	si,drvOffset rgbCGAExFont8x8	;see cga8x8.inc
	sub	al,80h

first128:
	xor	ah,ah
	mul	cl			
	add	si,ax				;ds:[si] -> char bit map

	mov	ax,ss
	mov	es,ax
	lea	di,LocalChar			;es:[di] -> local storage
	rep	movsb	 			;copy char bit map into local area
	mov	di,OFF_lpwDataCsd		;restore points
	mov	cx,ss:[di].ayBox	
	test	ss:[bx].finstInst,finstMonochrome	;Test for mono
	jz	@F
	cmp	curattr,70H			;highlighting ?
	jne	@F

	lea	si,LocalChar
loopnot:
	not	byte ptr ss:[si]
	inc	si
	loop	loopnot
	mov	cx,ss:[di].ayBox	
@@:	
	mov	ds,ax
	lea	si,LocalChar
	cmp	ss:[bx].psSecInst,0		;is sec. buffer allocated ?
	je	notfont8

	test	ss:[bx].finstInst,finstFont	;ffont mode ?
	jz 	notfont8

	mov	ax,dx			;save char
	pop	di	;si -> di
	push	di
	mov	es,ss:[bx].psSecInst		;es:[di] => ffont buffer
	mov	dx,es:[di-2]			;dx = ffont word
	or	dl,dl				;normal ?
	jz	notfont8

	cmp	al,' ' 			;check spaces
	jne	notspace1
	
	test	dl,ffontUnderline or ffontDoubleUnderline or ffontStrikeThrough or ffontOrCharacter
	jz	notfont8

notspace1:
	cCall	ChrisPFont 
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	
notfont8:
	mov	al,axCurrent		;cursor position ?
	cmp	al,axCurs
	jne	CursOff1

	mov	di,OFF_lpwDataCsd	
	push	si
	mov	cx,ss:[di].vparmCursOn
	xchg	ch,cl
	xor	ch,ch
	add	si,cx
	mov	cx,ss:[di].vparmCursOn
	sub	cl,ch
	xor	ch,ch			;cx = thickness of the cursor

outCurs1:				;display the cursor
	not	byte ptr [si]
	inc	si
	loop	outCurs1
	pop	si
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	

CursOff1:
IFDEF TANDYCSD
	mov	dh,curattr
	cmp	dh,flastattr		;same attr ?
	je	@F
	
	mov	flastattr,dh		;update attribute
	mov	dl,curattr
	mov	al,dl			;al = foreground
	and	al,03h
	mov	cl,4
	shr	dl,cl			;dl = background value
	and	dl,03h
	mov	ifgd,al
	mov	ibkgd,dl
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	
@@:					;* same attribute
ENDIF
	mov	ax,0B800h			;CGA video segment
	mov	es,ax
	mov	di,wTemp			;restore di
IFDEF TANDYCSD
	cmp	iMode,0Ah
	jne	NotMode0Ah

;;;4-color graphics mode	
	push	bx
out_1_byte:
	lodsb				;al=bit pattern
	mov	bl,al			;fgd = 1
	mov	dl,ifgd
	and	dl,1
	jnz	@F
	not	bl
@@:
	mov	bh,bl
	cmp	ifgd,1
	jb	@F
	cmp	ifgd,2
	ja	@F
	not	bh
@@:
	mov	ah,al
	and	bx,ax			;bkgd = 00 (black)
	push	bx			;save fgd pattern
	not	al
	mov	bl,al			;fgd = 1
	mov	dl,ibkgd
	and	dl,1
	jnz	@F
	not	bl
@@:
	mov	bh,bl
	cmp	ibkgd,1
	jb	@F
	cmp	ibkgd,2
	ja	@F
	not	bh
@@:
	mov	ah,al
	and	bx,ax			;fgd = 00 (black)
	pop	ax			;restore fgd pattern
	or	ax,bx			;merge fgd & bkgd
	stosw				;update one line
	add	di,2000h-2		;go to next line
	jns	@F
	add	di,-8000h+160
@@:
	loop	out_1_byte
	pop	bx
	mov	di,wTemp
	inc	di			;next char
	inc	di			;2 bytes/char
	jmp	NextChar

NotMode0Ah:	
ENDIF

;;;mono graphics mode
	mov	ax,1FFFh
	mov	dx,79-2000h
out_one_byte:				;output the character
	movsb
	add	di,ax
	xchg	ax,dx
	loop	out_one_byte

	mov	di,wTemp
	inc	di			;next char
NextChar:
	inc	axCurrent		;bump update position
	pop	si
	pop	ds
	pop	cx			;restore dax
	dec	cx
	jz	out_done1
	jmp	out_next_char1

out_done1:
done_graf_update:
IFDEF KANJI
;*	* For the OAX, we parse what we have printed and set the attribute
;*	*  byte to 0 for the second byte of double byte characters.
	cmp	fRestoreDbcs,0
	jne	end_update
	mov	si,offFirst
	xor	cx,cx
	mov	cl,dax				;* # of bytes
loop_parse_dbcs:
	lodsw					;* get char(al) + attribute(ah)
	JmpNotDbc parse_single
;*	* second byte follows
	inc	si				;* skip 2nd char
	mov	byte ptr ds:[si],0		;* clear attrib
	inc	si
	AssertNE cx,1				;* must be enough
	dec	cx
parse_single:
	loop	loop_parse_dbcs

ENDIF ;KANJI

end_update:

cEnd	DoUpdateCsd

	include cga8x8.inc		; extend font (128 - 255)

NonStandard	BltArcCsd

;********** BltArcCsd **********
;*	* CSD entry point (see documentation for interface)
;*	entry : axSrc, aySrc : upper left of source
;*		axDest, ayDest : upper left of destination
;*		dax, day : shift amount
;*	* Move a rectangle from one portion of the screen to another.
;*	exit : n/a

cProc	BltArcCsd,<FAR, PUBLIC, ATOMIC>, <SI,DI,DS>
    parmB axDest
    parmB ayDest
    parmB dax 
    parmB day
    parmB axSrc
    parmB aySrc
    localW cRows
cBegin	BltArcCsd
	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv

;*	* SS:BX => INST info
;*	* SS:DI => Driver data

;set up Src pointer si
	mov	cx,ss:[di].ayBox
	shr	cx,1			;memory interleave
	mov	al,80			;assume 80 columns (IBM)
	mul	cl			;
	mov	bx,ax			;bx = 80 * charHeight
	mov	cl,aySrc		
	mul	cx
	mov	cl,axSrc
	add	ax,cx			;ax = ay x 80 x Height + ax
	mov	si,ax			;si points to starting byte Src

;set up Dest pointer di
;assume (Scroll down) Hi -> Lo memory
	mov	ax,bx
	mov	cl,ayDest		
	mul	cx
	mov	cl,axDest
	add	ax,cx
	mov	cx,ss:[di].ayBox	;for later use
	mov	di,ax

	mov	al,day		
	mul	cl			;ax = ayBox * day
	mov	cRows,ax		;total scan rows needed to be blt

 	mov	cl,ayDest
	cmp	cl,aySrc
	jb	fwd
	ja	bkwd
;ayDest = aySrc
	mov	cl,axDest
	cmp	cl,axSrc
	jb	fwd		
bkwd:	
;have to blt backward (Lo -> Hi memory)
;(Scroll up) 
	shr	ax,1
	dec	ax
	mov	cx,80
	mul	cx
	mov	cl,dax
	dec	cx
	add	ax,cx			;ax = block offset
	add	ax,2000h		;start blt from odd line
	add	di,ax			;si, di point to end of block
	add	si,ax	
	std				;set direction flag
fwd:
	mov	ax,0B800h
	mov	ds,ax			;ds,es point to video buffer
	mov	es,ax
	mov	cx,cRows				
Blt1:
	mov	ax,si
	mov	bx,di
	mov	dx,cx
;	push	si
;	push	di
;	push	cx	

	xor	ch,ch
	mov	cl,dax
	rep	movsb 			;load latches/store video memory

;	pop	cx
;	pop	di
;	pop	si
	mov	cx,dx
	mov	di,bx
	mov	si,ax
	cmp	di,si
	jb	bf
;blt backward
	cmp	si,2000h
	jb	@F
	mov	dx,-2000h
	jmp short Blt2
@@:
	mov	dx,2000h - 80
	jmp short Blt2
bf:
;blt forward
	cmp	si,2000h
	jb	@F
	mov	dx,-2000h + 80
	jmp short Blt2
@@:
	mov	dx,2000h
Blt2:
	add	si,dx
	add	di,dx
	loop	Blt1

	cld				;clear direction flag

cEnd	BltArcCsd



NonStandard	GetCharMapCsd

;********** GetCharMapCsd **********
;*	entry:	pinft, ch, pbitmap
;*	* copy character bit map into buffer

cProc	GetCharMapCsd, <FAR, ATOMIC, PUBLIC>, <DS,SI,DI>
    parmDP pinft
    parmB  char
    parmDP pbitmap    
cBegin	GetCharMapCsd
	mov	di,pinft			;* ds:di => INFT
	xor	ch,ch
	mov	cl,ds:[di].dyCharInft
	mov	al,char
	cmp	al,128
	jb	@F
	sub	al,128
	mov	dx,cs
	mov	ds,dx
	mov	si,drvOffset rgbCGAExFont8x8	;see cga8x8.inc
	jmp short RetFonts
@@:
	mov	dx,0F000h
	mov	ds,dx
	mov	si,0FA6Eh
RetFonts:
	mul	cl
	add	si,ax				;ds:si => character bit map		

	mov	ax,ss
	mov	es,ax
	mov	di,pbitmap			;es:di => destination
	rep	movsb
	
cEnd	GetCharMapCsd

