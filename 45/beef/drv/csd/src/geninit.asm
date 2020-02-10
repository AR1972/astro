;*	General Purpose FInitCsd Routine
;*

NonStandard	FInitCsd

;*****************************************************************************

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
	mov	[di].vparmCursOn,ax		;initialize underline cursor
	mov	[di].vparmCursSize,ax
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax

	mov	al,cs:[bx].modeDm
	cmp	al,modeCur
	jne	@F				
IFDEF	GRAPHICSTEXT
	test	cs:[bx].finstDm,finstGraphics	
	jz	ttext
	cmp	[di].ayBox,8
	jne	@F				;need to init drv data
ttext:
ENDIF	;GRAPHICSTEXT
	jmp	finitdone
@@:

IFDEF	GRAPHICSTEXT
	test	cs:[bx].finstDm,finstText
	jz	initvideomode			
ENDIF	;GRAPHICSTEXT

	test	[di].fvmCurAdap,fvmVGA 		;Test for VGA
	jz	initvideomode			

	mov	al,2				;default 400 scan lines
	cmp	cs:[bx].ayMacDm,43		;80x43 text ?
	jne	@F

	dec	al				;350 lines (EGA)
@@:
	mov	ah,12h				;set vertical scan line for VGA
	push	bx
	mov	bl,30h
	int	10h
	pop	bx

initvideomode:	
	xor	ah,ah				;* set mode
						;* REVIEW: always clear RGEN ??
	mov	al,cs:[bx].modeDm
	int	10h

	mov	ax,40H
	mov	es,ax
	mov	al,cs:[bx].ayMacDm
	dec	al				; rows - 1
	mov	byte ptr es:[0084H],al		;* update BIOS rows

IFDEF	GRAPHICSTEXT
	test	cs:[bx].finstDm,finstGraphics
	jz	@F
	jmp	InitGraf
@@:
ENDIF	;GRAPHICSTEXT

	;* if only a CGA (not a dual mode) then skip font load
	test	[di].fvmCurAdap,NOT fvmCGA
	jz	textinitdone

	mov	ax,0812h			;8x8 fonts
	cmp	cs:[bx].ayMacDm,25		
	jne	@F
	
	test	[di].fvmCurAdap,fvmEGA or fvmEGAM or fvm64KEGA or fvmMDA
	jnz	textinitdone

	mov	ax,1014h			;8x16 PS/2		
@@:
	push	bx
	xor	bl,bl
	mov	bh,ah
	mov	ah,11h				;load char set
	int	10h				
	pop	bx
textinitdone:
	jmp	finitdone

IFDEF	GRAPHICSTEXT
InitGraf:
	mov	al,2			;default 25 rows

	cmp	cs:[bx].modeDm,13h	;MCGA 256 color mode ?
	je	font8x8
			
	cmp	cs:[bx].modeDm,6	;CGA mode 4 or 6 ?
	ja	notmode6

	test	[di].fvmCurAdap,fvmCGA
	jz	font8x8			;8x8 -> EGA,MCGA,VGA

	mov	[di].SEG_lpbFont,0F000h		;load PC ROM Font (CGA only)
	mov	[di].OFF_lpbFont,0FA6Eh
	mov	[di].ayBox,8
	jmp	finitdone

notmode6:
	cmp	cs:[bx].ayMacDm,25
	jne	not14font		;8x14 -> EGA,VGA

	cmp	cs:[bx].dyCharDm,14
	je	@F
	xor	al,al			
	mov	dl,25			;25 scan rows
	jmp	short font8x8		;8x8 -> 64KEGA
@@:
	mov	bl,2	
	jmp	load14

not14font:
	cmp	cs:[bx].ayMacDm,30
	jne	not80x30		

	mov	al,6			;8x16 -> MCGA,VGA
	jmp	defaultfont

not80x30:	
	cmp	cs:[bx].ayMacDm,34	
	jne	not80x34	     

	xor	bl,bl			;8x14 -> VGA only
	mov	dl,34
load14:
	mov	ax,1122h		;load 8x14 fonts
	int	10h
	mov	al,2			;return 8x14 fonts
	jmp	defaultfont
	
not80x34:
	cmp	cs:[bx].ayMacDm,43
	jne	not80x43		

	xor	al,al			
	mov	dl,43			;43 scan rows
	jmp	short font8x8		;8x8 -> EGA,VGA

not80x43:
	cmp	cs:[bx].ayMacDm,60
	xor	ax,ax
	jne	finitret		;error, this mode is not supprt !
	
	mov	dl,60
font8x8:
	mov	bl,al			;scan rows 
	mov	ax,1123h		;load 8x8 fonts
	int	10h
	mov	al,3			;return 8x8 fonts

defaultfont:
	mov	bh,al
	mov	ax,1130h
	push	bp
	int	10h
	mov	[di].SEG_lpbFont,es
	mov	[di].OFF_lpbFont,bp
	mov	[di].ayBox,cx
	pop	bp

ENDIF	;GRAPHICSTEXT
finitdone:

;*	* normally the INCH array would be copied (but since it is already
;*	*  setup in DATA just leave it alone).

;*	* Do other diddling
	cCall	DiddleBlinkBit

	mov	ax,sp				;* success
finitret:
cEnd	FInitCsd
