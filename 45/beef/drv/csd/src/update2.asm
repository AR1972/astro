;* included in:
;*		 t3100, compaq3, vectra, olivetti, ericsson, genius (.asm)

NonStandard	FvmGetCur
NonStandard	DoUpdateCsd

;*****************************************************************************

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

IFDEF	(T3100CSD OR OLIVETTICSD OR COMPAQ3CSD)
NonStandard	ModeGetCur
;********** ModeGetCur *********
;*	entry:	n/a
;*	* get current machine mode
;*	exit:	al = mode, ah = ayMac (or 0 if unknown)

cProc	ModeGetCur, <NEAR, PUBLIC, ATOMIC>, <ES>
cBegin	ModeGetCur

	mov	ax,40H
	mov	es,ax
	mov	dl,es:[0084H]		;* read BIOS rows
	inc	dl			;* dl = screen height
	cmp	dl,25			;do this since some clones don't
	je	@F			;update BIOS data
	cmp	dl,50
	je	@F
	mov	dl,25			;* default to 25 rows
@@:	
	push	bx
	mov	ah,0fh
	int	10h			;* get current state, return al = mode
	pop	bx
	mov	ah,dl

cEnd	ModeGetCur
ENDIF	;(T3100CSD OR OLIVETTICSD OR COMPAQ3CSD)

;********** DoUpdateCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* BIOS version: update the screen

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax
    parmW offFirst
    parmW fRestoreDbcs
    localB axCurs
    localB axCurrent
    localW wTemp
    localW wSec
IFDEF	GENIUSCSD
    localW wvideoseg
ENDIF	;GENIUSCSD
    localB curattr
    LocalV LocalChar,16
cBegin	DoUpdateCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv
	test	[bx].finstInst,finstGraphics
	jnz	UpdateGrafText
	jmp	DoUpdateExit

	;***************************
	;*     			   *
	;*	Graphics Text	   *
	;*			   *
	;***************************

UpdateGrafText:
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
	mov	dx,ds:[bx].psSecInst		;* DX:SI => secondary buffer
	mov	wSec,dx
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer
	mov	si,offFirst

;*	* DS:SI => start character in primary buffer
;*	* DX:SI => start ffont in secondary buffer

	;find cursor position (pixel starting address)		
IFDEF	GENIUSCSD
	mov	al,ayLine 
	xor	ah,ah
	mov	cl,11
	shl	ax,cl
	mov	wvideoseg,Upper_Screen
	jnc	@F
	mov	wvideoseg,Lower_Screen
@@:
ELSE	;!GENIUSCSD
	mov	cl,ss:[bx].axMacInst		;screen width
	mov	al,ayLine 
	xor	ah,ah
	mul	cl
	mov	cx,ax
	shl	ax,1			;mul 2 (8 bytes/char, 4 way interleave)
	cmp	ss:[di].ayBox,8
	je	@F

	shl	ax,1			;mul 4 (16 bytes/char)
@@:
ENDIF	;GENIUSCSD
	xor	dh,dh
	mov	dl,axFirst
	add	ax,dx
	mov	di,ax			;* ES:DI => video buffer (pixel address)

	xor	ch,ch
	mov	cl,dax			;# of characters to be updated

out_next_char:
	lodsw			;ah - attr, al - char
	mov	curattr,ah		;save color attribute
	push	cx		;save dax
	push	ds		;save pointer to primary buffer
	push	si			
	mov	wTemp,di		;save video address
	mov	di,OFF_lpwDataCsd
	mov	si,ss:[di].OFF_lpbFont
	mov	ds,ss:[di].SEG_lpbFont	;DS:[SI] -> char table
	mov	cx,ss:[di].ayBox
	mov	dx,ax			;save char + attribute
IFNDEF	GENIUSCSD
	cmp	cx,16
	je	@F
	cmp	al,128
	jb	@F
	push	cs
	pop	ds
	mov	si,drvOffset rgbCGAExFont8x8	;see cga8x8.inc
	sub	al,80h
@@:	
ENDIF	;not defined GENIUSCSD
	xor	ah,ah
	mul	cl
	add	si,ax			

	mov	ax,ss
	mov	es,ax
	lea	di,LocalChar			;es:[di] -> local storage
	rep	movsb  	   			;copy char bit map into local area
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
	mov	ds,ax				;DS:[SI] -> local storage
	lea	si,LocalChar

	cmp	wSec,0			;check Sec. buffer
	je	notFont			;****

	test	ss:[bx].finstInst,finstFont	;FFont mode ?
	jz	notFont	

	pop	di			;si -> di (buffer offset)
	push	di	

	mov	ax,wSec
	mov	es,ax			;ES:[DI-2] -> ffont buffer
	mov	ax,dx			;restore character
	mov	dx,es:[di-2]		
	or	dl,dl
	jz	notFont

	cmp	al,' ' 			;check spaces
	jne	notspace1
	
	test	dl,ffontUnderline or ffontDoubleUnderline or ffontStrikeThrough or ffontOrCharacter
	jz	notfont

notspace1:
	cCall	ChrisPFont 
notFont:
IFDEF	GENIUSCSD
	mov	cx,wvideoseg		;video seg
ELSE	;!GENIUSCSD
	mov	cx,0B800h		;video seg
ENDIF	;GENIUSCSD
	mov	es,cx
	mov	di,OFF_lpwDataCsd		;restore points
	mov	cx,ss:[di].ayBox	;restore points

	mov	al,axCurrent		;cursor position ?
	cmp	al,axCurs
	jne	CursOff1

	push	cx
	push	si
	mov	cx,ss:[di].vparmCursOn
	xchg	ch,cl
	xor	ch,ch
	add	si,cx
	mov	cx,ss:[di].vparmCursOn
	sub	cl,ch
	xor	ch,ch

outCurs1:				;display the cursor
	not	byte ptr [si]
	inc	si
	loop	outCurs1
	pop	si
	pop	cx

CursOff1:
IFDEF	GENIUSCSD
	mov	di,wTemp
	mov	ax,(1024 / 8) - 1
OC9Lp:
	movsb				; move one line to screen
	add	di,ax			; go to next scan line
	loop	OC9Lp
ELSE	;!GENIUSCSD
	shr	cl,1
	shr	cl,1			;div 4
	mov	di,wTemp
IFDEF	ERICSSONCSD
;
; The following is used because the Ericsson interlaces memory funny.
;	The usual quad interlacing is 1,5,9,... in first block,
;	2,6,10,... in second block, 3,7,11,... in third block, and
;	4,8,12,... in fourth block.  The Ericsson switches blocks 2 and 3
;	to give:  1,5,9,...  3,7,11,...  2,6,10,...  4,8,12,...
;
	mov	ax,2 * 2000h - 1
OC9Lp:
	movsb				 
	ADD	DI,AX			; move to block 3
	movsb
	add	di,-2000h - 1		; back to block 2
	movsb				
	ADD	DI,AX			; move to block 4 
	movsb
	sub	di,3 * 2000h + 1 - 80	; back to block 1
	loop	OC9Lp
ELSE	;!ERICSSONCSD
	mov	ax,2000h - 1
OC9Lp:
	movsb				; And put back to screen memory
	ADD	DI,AX			; Move down to next scan line
	movsb
	add	di,ax
	movsb				; And put back to screen memory
	ADD	DI,AX			; Move down to next scan line
	movsb
	sub	di,6000h - 79
	loop	OC9Lp
ENDIF	;ERICSSONCSD
ENDIF	;GENIUSCSD
	mov	di,wTemp
	inc	di
	inc	axCurrent		;bump update position
	pop	si
	pop	ds
	pop	cx
	dec	cx
	jz	DoUpdateExit
	jmp	out_next_char
		
DoUpdateExit:

cEnd	DoUpdateCsd

	include cga8x8.inc		; extend font (128 - 255)
	CPF816 = 1
	include cpfont.asm		;* ChrisPFont with 8x8,8x16

;*****************************************************************************

IFNDEF	GENIUSCSD

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
	shr	cx,1			;4-way memory interleave
	shr	cx,1			;
	mov	al,80			;assume 80 columns (IBM)
	mul	cl			;
	mov	bx,ax			;bx = 80 * charHeight/4
	mov	cl,aySrc		
	mul	cx
	mov	cl,axSrc
	add	ax,cx			;ax = ay x 80 x Height/4 + ax
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
	shr	ax,1
	dec	ax
	mov	cx,80
	mul	cx
	mov	cl,dax
	dec	cx
	add	ax,cx			;ax = block offset
	add	ax,6000h		;start blt from last bank
	add	di,ax			;si, di point to end of bank
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
	rep	movsb 			;blt one line

;	pop	cx
;	pop	di
;	pop	si
	mov	cx,dx
	mov	di,bx
	mov	si,ax
IFDEF	ERICSSONCSD
	cmp	di,si
	jb	bfw

	mov	dx,-4000h		;Blt backward
	cmp	si,2000h
	ja	@F
	mov	dx,6000h - 80
	jmp short Blt2
@@:
	cmp	si,4000h
	ja	Blt2
	mov	dx,2000h
	jmp short Blt2
bfw:					;blt foreward
	mov	dx,4000h
	cmp	si,6000h
	jb	@F
	mov	dx,-6000h + 80
	jmp short Blt2
@@:
	cmp	si,4000h
	jb	Blt2
	mov	dx,-2000h
ELSE	;!ERICSSONCSD
	mov	dx,2000h
	cmp	di,si
	jb	@F
	neg	dx			;blt backward
	cmp	si,2000h
	ja 	Blt2
	mov	dx,6000h - 80
	jmp short Blt2
@@:
	cmp	si,6000h		;blt foreward
	jb	Blt2
	mov	dx,80 - 6000h 
ENDIF	;ERICSSONCSD
Blt2:
	add	si,dx			;go to next row
	add	di,dx
	loop	Blt1

	cld				;clear direction flag

cEnd	BltArcCsd

ENDIF	;!GENIUSCSD



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
	cmp	cl,16
	jne	@F
IFDEF	T3100CSD
	mov	dx,segROM			;8x16
	mov	ds,dx
	mov	si,T3100_Font_Off
ELSE	;!T3100CSD
	mov	dx,cs				;8x16
	mov	ds,dx
	mov	si,drvOffset rgbVectFont8x16
ENDIF	;T3100CSD
	jmp short RetFonts
@@:
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