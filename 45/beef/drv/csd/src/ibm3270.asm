;*
;*	CW : Character Windows Drivers
;*
;*	3270.asm : for IBM 3270 PC AT
;*****************************************************************************

	include	csd_head.inc

	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:
;* #0 - standard monochrome text mode (9 x 14 characters)
	DB	0ffh			 	;* hardware needed
	DB	0ffh
	DB	3				;* mode
	DW	finstText 
	DB	80, 25				;* screen size
	DB	8				;* coMac
	DB	0, 0, 0, 0			;* INFT
	DW	0B800H				;* video address
	DW	0C0DH				;* cursor
	DW	0				;* extra (RamFont info)
	Assert	<($-rgdm) EQ SIZE DM>

;* #2 - monochrome graphics text mode (8 x 8 characters)
	DB	0ffh			 	;* hardware needed
	DB	0ffh
	DB	30h				;* mode
	DW	finstGraphics OR finstMonochrome OR finstFont
	DB	90, 25				;* screen size
	DB	2				;* coMac
	DB	8, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0C0DH				;* cursor
	DW	0				;* extra 

;* #1 - standard monochrome graphics text mode (8 x 8 characters)
	DB	0ffh			 	;* hardware needed
	DB	0ffh
	DB	30h				;* mode
	DW	finstGraphics OR finstMonochrome OR finstFont
	DB	90, 43				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* extra 

cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

;*****************************************************************************

NonStandard	FvmGetCur
NonStandard	ModeGetCur
NonStandard	FInitCsd
NonStandard	DoUpdateCsd

;*****************************************************************************


;********** FvmGetCur **********
;*	* Identify the current screen and return the appropriate fvm in AL
;*	* NOTE:	This is a NEAR routine.
;*	exit:	AL == fvm for current screen or 0 if no supported screen found

cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>
cBegin	FvmGetCur

	AssertEQ di,OFF_lpwDataCsd
	mov	ax,-1
	mov	[di].fvmCurAdap,al
	mov	[di].fvmCurDisp,ah
end_fvm_get: ;* ax = fvm
cEnd	FvmGetCur

;********** ModeGetCur *********
;*	entry:	n/a
;*	* get current machine mode
;*	exit:	al = mode, ah = ayMac (or 0 if unknown)

cProc	ModeGetCur, <NEAR, PUBLIC, ATOMIC>, <ES>
cBegin	ModeGetCur

	mov	ax,40H
	mov	es,ax
	mov	dl,es:[0084H]		;* read BIOS rows
	inc	dl
	cmp	dl,25			;do this since some clones don't
	je	@F			;update BIOS data
	cmp	dl,43
	je	@F
	mov	dl,25			;* default to 25 rows
@@:	
	push	bx
	mov	ah,0fh
	int	10h			;* get current state, return al = mode
	mov	bx,es:[004Ah]
	cmp	bx,90
	jne	@F
	mov	al,30h			;90 columns indicates graphics-text mode
@@:
	pop	bx
	mov	ah,dl

cEnd	ModeGetCur


;********** FInitCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <DI>
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
	jmp	initdrv				;* don't reset
@@:
	xor	ah,ah				;* set mode
	int	10h

	mov	cx,40H
	mov	es,cx
	mov	cl,cs:[bx].ayMacDm
	dec	cl				; rows - 1
	mov	byte ptr es:[0084H],cl		;* update BIOS rows
	mov	cl,cs:[bx].axMacDm
	mov	word ptr es:[004Ah],cx		;updates columns
initdrv:
	test	cs:[bx].finstDm,finstGraphics
	jz	finitdone

	xor	ch,ch	
	mov	cl,cs:[bx].dyCharDm
	mov	[di].ayBox,cx			;* points
	cmp	cx,8
	je	@F

	mov	[di].SEG_lpbFont,cs				;8x16 font
	mov	[di].OFF_lpbFont,drvOffset rgbHercFont8x14
	jmp short font1
@@:
	mov	[di].SEG_lpbFont,0F000h		;8x8 font (first 128)
	mov	[di].OFF_lpbFont,0FA6Eh
font1:	
	jmp	short @F

finitdone:	
	test	cs:[bx].finstDm,finstGraphics
	jnz	@F

;*	* the INCH array already contains the standard Code Page 437
;*	*  character set, so it usually can be left the same.

;*	* Do other diddling
	cCall	DiddleBlinkBit
@@:
	mov	ax,sp				;* success
cEnd	FInitCsd



;********** DoUpdateCsd **********
;*	entry:
;*		parameters just like PrepUpdateCsd
;*	* after primary buffer has been updated
;*	exit: n/a

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, ES, SI, DI>
    parmB ayLine
    parmB axFirst
    parmB dax			;* will never be 0
    parmW offFirst
    parmW fRestoreDbcs		;* => restore DBCS 1/2 characters
    localB axCurs
    localB axCurrent
    localW wTemp
    localV LocalChar,14		;storage of char bit map for ffont mode

cBegin	DoUpdateCsd
	
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
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer

;*	* DS:SI => start character in primary buffer
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

	mov	cx,ss:[di].ayBox
	xor	ax,ax			;calculate start of screen address
	mov	al,ayLine
	mul	cx			;ayLine * charHeight
	mov	cx,90
	mul	cx
	xor	cx,cx
	mov	cl,axFirst
	add	ax,cx			;ax = ayLine x 80 x ayBox + axFirst
	mov	di,ax			;point to video buffer

	xor	cx,cx
	mov	cl,dax			;no. of char to be updated

	mov	dx,si
	mov	si,offFirst

out_next_char:
	lodsw				;load character+attribute
	push	cx			;save dax
	push	ds			;save pointer to primary buffer
	push 	si
	mov	dx,ax			;save char
	mov	wTemp,di  		;save di
	mov	di,OFF_lpwDataCsd	;load cx with points
	mov	cx,ss:[di].ayBox	

	mov	si,ss:[di].OFF_lpbFont
	mov	ds,ss:[di].SEG_lpbFont
	cmp	cl,8
	jne	@F
	cmp	al,80h
	jb	@F
		     			;use extend font (128 - 255)
	push	cs
	pop	ds
	mov	si,drvOffset rgbCGAExFont8x8	;see cga8x8.inc
	sub	al,80h
@@:
	xor	ah,ah
	mul	cl			;ax = character table offset
	add	si,ax			;ds:[si] -> char bit map

	mov	ax,ss
	mov	es,ax
	lea	di,LocalChar		;es:[di] -> local storage
	rep	movsb			;copy bitmap to local area
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	
	mov	ds,ax
	lea	si,LocalChar

	cmp	ss:[bx].psSecInst,0		;is sec. buffer allocated ?
	je	notfont14or16

	test	ss:[bx].finstInst,finstFont	;graphics text 8x14, 8x16
	jz	notfont14or16

	mov	ax,dx
	pop	di	;si -> di
	push	di
	mov	es,ss:[bx].psSecInst	;* es:[di] => ffont buffer
	mov	dx,es:[di-2]		
	or	dl,dl			;zero ffont
	jz 	notfont14or16

	cmp	al,' ' 			;check spaces
	jne	@F
	
	test	dl,ffontUnderline or ffontDoubleUnderline or ffontStrikeThrough or ffontOrCharacter
	jz	notfont14or16

@@:
;*	ax = char + attribute
;*	dx = ffont word
	cCall	ChrisPFont
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	
notfont14or16:
	mov	al,axCurrent
	cmp	al,axCurs
	jne	CursOff2

	mov	di,OFF_lpwDataCsd	
	push	cx
	push	si
	mov	cx,ss:[di].vparmCursOn
	xchg	ch,cl
	xor	ch,ch
	add	si,cx
	mov	cx,ss:[di].vparmCursOn
	sub	cl,ch
	xor	ch,ch
outCurs2:
	not	byte ptr [si]
	inc	si
	loop	outCurs2
	pop	si
	pop	cx

CursOff2:
	mov	di,OFF_lpwDataCsd	

	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	
	mov	ax,0B800h		;set video segment
	mov	es,ax
	mov	di,wTemp		;restore di
	mov	ax,90-1			;column

out_char_map:
	movsb
	add	di,ax			;go to next scan line (+90-1)
	loop	out_char_map
	
	mov	di,wTemp		;restore di
	inc	di			;next char
J13:
	inc	axCurrent		;bump update position
	pop	si
	pop	ds
	pop	cx			;restore dax
	dec 	cx
	jz	out_done2
	jmp	out_next_char

out_done2:
done_graf_update:

end_update:

cEnd	DoUpdateCsd

	CPF814 = 1
	include cpfont.asm		;* ChrisPFont with 8x8,8x14,8x16
	
;*****************************************************************************

	include cga8x8.inc
	include	Herc8x14.inc

;*****************************************************************************

	include	csd_std.asm		;* standard init/term

	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************

	include	csd_vram.asm		;* default procs for direct video I/O

	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************

	END

