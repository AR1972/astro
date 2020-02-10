;*
;*	CW : Character Windows Drivers
;*
;*	ibmfont.asm : general purposes (CGA/EGA/MCGA/VGA, Graphics-Text) CSD
;*****************************************************************************

	include	csd_head.inc
	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*****************************************************************************

	include	genmodes.asm			;display mode table
	include	geninit.asm			;finitcsd

;*****************************************************************************
;*	* Special routines

NonStandard	DoUpdateCsd
NonStandard	FvmGetCur

;*****************************************************************************

IF CGACSD OR MCGACSD

;* 320x200 CGA mode 4
rgb4ColorCode	DB	00000000b	; 0
		DB	01010101b	; 1
		DB	10101010b	; 2
		DB	11111111b	; 3

ENDIF	;CGACSD OR MCGACSD

;*****************************************************************************

;********** FvmGetCur **********
;*	entry:	DS:DI => driver data
;*	* if first time called, identify the current screen and set fvmCur
;*	*  subsequent calls will return fvmCur
;*	*  After this call: fvmCur, codepageBase, codepageAlt are initialized
;*	exit:	AL == adapter (0 => no supported screen found)
;*		AH == monitor
cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>
cBegin	FvmGetCur

	AssertEQ di,OFF_lpwDataCsd
	mov	al,[di].fvmCurAdap
	mov	ah,[di].fvmCurDisp
	or	ax,ax
	jz	init_get
	jmp	end_fvm_get

init_get:	;* initial get
	push	bp			; Int10's on some machines crunch bp.

;* set adapter flag

IF MDACSD
	mov	al,fvmMDA
ENDIF

IF CGACSD
	mov	al,fvmCGA
ENDIF

IF MCGACSD
	mov	al,fvmMCGA
ENDIF

IF VGACSD
	mov	al,fvmVGA
ENDIF

IF EGACSD
	mov	ah,12h			;Test for EGA
	mov	bl,10h
	int	10h

	mov	ax,fvmEGAM		; Test for a monochrome EGA display
	cmp	cl,0BH	 
	je	found_EGA
	cmp	cl,0AH
	je	found_EGA
	cmp	cl,04H
	je	found_EGA
	cmp	cl,05H
	je	found_EGA

	mov	ax,fvm64KEGA		; Text for 64K of EGA RAM
	or	bl,bl
	jz	found_EGA
	cmp	cl,3			; HiRes display for settings 9 & 3
	je	highRes
	cmp	cl,9
	jne	found_EGA
highRes:
	mov	ax,fvmEGA		; It's got more than 64K of RAM
found_EGA:
	mov	dx,ax			; allow co-exist of EGA and CGA
notEGA:					; Test for CGA
	mov	ax,0B800h		; The following test must use word
	mov	es,ax			; values.  Otherwise, it screws up on
	assume	es:nothing		; some monochrome systems.
	mov	es:[07FFEH],1234h	; See if offsets 7FFEH and 3FFEH
	mov	es:[03FFEH],5678h	; are the same - if so then we have
	cmp	es:[07FFEH],5678h	; a color card
	jne	@F
	or	dx,fvmCGA
	mov	ax,dx
	jmp	found_fvm
@@:
	mov	ax,dx
found_fvm:	;* al = fvm
ENDIF 	;EGACSD

	mov	dl,al
	mov	ah,0FAh 		; Find out if there is an EGA driver
	xor	bx,bx			; installed
	int	10h
	or	bx,bx
	jz	@F			; Jump if there isn't.
	or	dl,fvmMouse		; EGA.SYS is there (mirror registers)
@@:
	mov	al,dl
	mov	ah,0FFh			; assume no monitor chk
	push	bx    			;* check monochrome

IF MCGACSD
	push	ax			;save al (adapter info)
	mov	ax,1A00h		;read display combination code
	int	10h
	cmp	al,1Ah	
	pop	ax
	jne	ColorMode
	
	mov	ah,fvmECD or fvmCD
	cmp	bl,0Ch			;analog color ?
	je	ColorMode
	mov	ah,fvmMD		;analog mono (0Bh) 
ENDIF 	;MCGSCSD

IF EGACSD OR VGACSD
	test	al,fvmCGA		;CGA co-exist ?
	jnz	ColorMode		;if dual-card then enble all monitor
	push	ax			;save al (adapter info)
	mov	ah,12h
	mov	bl,10h
	int	10h	
	pop	ax
	mov	ah,fvmMD
	or	bh,bh			;0 = color, 1 = mono
	jnz	ColorMode
	mov	ah,fvmCD
	and	cl,00000110b		;switches 2 and 3 
	jnz	ColorMode		;off?
	mov	ah,fvmECD		;on.
ENDIF	;EGACSD OR VGACSD

ColorMode:
	pop	bx		
	mov	[di].fvmCurAdap,al		;* update drivers' data
	mov	[di].fvmCurDisp,ah		;* fill drivers' data
	pop	bp

end_fvm_get: ;* ax = fvm

cEnd	FvmGetCur


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
    localB flastattr		;check repeat attribute
IF CGACSD 
    localW wfgd			;needed for mode 4 (320x200x4)
    localW wbkgd
ENDIF	;CGACSD 
    localB curattr
    localW wTemp
    localV LocalChar,16		;storage of char bit map for ffont mode
IF VGACSD OR EGACSD
    ;* for extend int10 call
    localV rgbGraphicsRegSave,4	;* save buffer
    localV rgbGraphicsRegT,4	;* work buffer
ENDIF 	;VGACSD OR EGACSD

cBegin	DoUpdateCsd
	
	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv

	test	ss:[bx].finstInst,finstGraphics	;graphics text 8x14
	jnz	graf_update

IFDEF	CGASNOW			;specific code for snow protect CGA
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer
	mov	si,offFirst

;*	* DS:SI => start character in primary buffer
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

;*** Snow Protected CGA (Text) **
;*			    	*
;*			    	*	
	xor	cx,cx
	mov	cl,dax			;byte count
	mov	di,offFirst		;store starting address
	mov	ax,0B800h		;video segment
	mov	es,ax
	mov	dx,03DAh		;CGA status reg.
loop_update:
	lodsw				;primary buffer ds:[si] -> ax
	mov	bx,ax			;save char
enable_int_window:			;waitretracedefinte
	sti
	mov	ah,20			;* magic number of iterations
	cli				;* clear interrupts for magic loop
wait_for_noretrace:
	dec	ah
	jz	enable_int_window
	in	al,dx			; fetch status
	test	al,8
	jnz	do_it
	test	al,1			; horizontal retracing?
	jnz	wait_for_noretrace	; yes, wait until no retrace
wait_for_retrace:
	in	al,dx			; fetch status
	test	al,9			; horizontal/vertical retracing?
	jz	wait_for_retrace	; no, wait...
do_it:
	mov	ax,bx			;restore char
	stosw				;ax -> screen buffer es:[di]
	sti				;enable interrupts
	loop	loop_update

ENDIF	;CGASNOW
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
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer

;*	* DS:SI => start character in primary buffer
;*	* SS:BX => INST info
;*	* SS:DI => Driver data

	mov	flastattr,0		;initialize with black:black
	mov	cx,ss:[di].ayBox
	cmp	dl,6			;CGA mode 4 or 6
	ja	@F

	shr	cx,1			;memory interleave addressing
@@:
	xor	dh,dh
	mov	si,dx			;save video mode dl
	xor	ax,ax			;calculate start of screen address
	mov	al,ayLine
	mul	cx			;ayLine * charHeight
	mov	cx,80
	mul	cx
IF MCGACSD
	cmp	si,13h			;MCGA 256-color mode (320x200, 40x25)
	jne	@F
	shl	ax,1			;1 byte/pixel
	shl	ax,1
@@:
ENDIF	;MCGACSD
	xor	cx,cx
	mov	cl,axFirst
IF CGACSD
	cmp	si,4			;CGA 320x200x4 mode
	jne	@F
	shl	cx,1
@@:
ENDIF	;CGACSD
IF MCGACSD
	cmp	si,13h
	jne	@F
	shl	cx,1
	shl	cx,1
	shl	cx,1
@@:
ENDIF	;MCGACSD
	add	ax,cx			;ax = ayLine x 80 x ayBox + axFirst
	mov	di,ax			;point to video buffer

	xor	cx,cx
	mov	cl,dax			;no. of char to be updated

	mov	dx,si
	mov	si,offFirst

IF CGACSD
	cmp	dl,6			;CGA mode 4 or 6
	jbe	out_next_char1

	jmp	notmode6b


;* CGA graphics mode 4 or 6
	

out_next_char1:
	lodsw				;load character+attribute
	mov	curattr,ah
	push	cx			;save dax
	mov	wTemp,di		;save di
	mov	di,OFF_lpwDataCsd	
	mov	cx,ss:[di].ayBox	;calculate character table address

	push	ds			;save pointer to primary buffer
	push 	si

	mov	si,ss:[di].OFF_lpbFont	;default
	mov	ds,ss:[di].SEG_lpbFont

	mov	dx,ax			;save char + attribute
	test	ss:[di].fvmCurAdap,fvmCGA
	jz	first128

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
	mov	ds,ax				;ds=ss
	lea	si,LocalChar
	cmp	ss:[bx].psSecInst,0		;is sec. buffer allocated ?
	je	notfont8

	test	ss:[bx].finstInst,finstFont	;ffont mode ?
	jz 	notfont8

	mov	ax,dx				;save char
	pop	di	;si -> di
	push	di
	mov	es,ss:[bx].psSecInst		;es:[di] => ffont buffer
	mov	dx,es:[di-2]			;dx = ffont word
	or	dl,dl				;normal ?
	jz	notfont8

	cmp	al,' ' 			;check spaces
	jne	@F
	
	test	dl,ffontUnderline or ffontDoubleUnderline or ffontStrikeThrough or ffontOrCharacter
	jz	notfont8
@@:
;*	ax = char + attribute
;*	dx = ffont word
	cCall	ChrisPFont 
notfont8:
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	;restore points
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
	xor	ch,ch

outCurs1:				;display the cursor
	not	byte ptr [si]
	inc	si
	loop	outCurs1
	pop	si
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	

CursOff1:

	mov	dh,curattr
	cmp	dh,flastattr		;same attr ?
	je	@F
	
	mov	flastattr,dh		;update attribute
	push	bx
	mov	dl,curattr
	mov	al,dl			;al = foreground
	and	al,03h
	mov	cl,4
	shr	dl,cl			;dl = background value
	and	dl,03h
	mov	bx,drvOffset rgb4ColorCode
	xlat	cs:rgb4ColorCode	
	mov	ah,al
	mov	wfgd,ax
	mov	al,dl
	xlat	cs:rgb4ColorCode	
	mov	ah,al
	mov	wbkgd,ax	
	pop	bx
	mov	cx,ss:[di].ayBox	
@@:					;* same attribute
	mov	ax,0B800h		;CGA video segment
	mov	es,ax
	mov	ax,1FFFh
	mov	dx,79-2000h
	cmp	ss:[bx].axMacInst,80
	je	@F
	dec	dx
	dec	ax
@@:
	mov	di,wTemp		;restore di
	test	di,2000h
	jz	@F

	xchg	ax,dx 			;exchange inc if 1st pixel lies in
		      			;odd interleave 
@@:
	cmp	ss:[bx].axMacInst,80
	je	out_one_byte

;;;320x200x4 mode (40x25)
out_1_byte:
	push	ax
	push	dx
	lodsb	
	xor	dx,dx
	mov	ah,8
L11:
	shr	al,1
	rcr	dx,1
	sar	dx,1
	dec	ah
	jnz	L11
	mov	ax,dx
	and 	ax,wfgd
	not	dx
	and	dx,wbkgd
	or	ax,dx
	xchg	ah,al
	stosw	
	pop	dx
	pop	ax
	add	di,ax	
	xchg	ax,dx
	loop	out_1_byte
	jmp short D11
	
out_one_byte:				;output the character
	movsb
	add	di,ax
	xchg	ax,dx
	loop	out_one_byte
D11:
	mov	di,wTemp
	inc	di			;next char
	cmp	ss:[bx].axMacInst,80
	je	@F
	inc	di
@@:
	inc	axCurrent		;bump update position
	pop	si
	pop	ds
	pop	cx			;restore dax
	dec	cx
	jz	out_done1
	jmp	out_next_char1

out_done1:
	jmp	done_graf_update	;done

ENDIF 	;CGACSD 

notmode6b:				

IF EGACSD OR VGACSD OR MCGACSD

;******* EGA/MCGA/VGA ********
;*
;*

out_next_char:
	lodsw				;load character+attribute
	mov	curattr,ah
	push	cx			;save dax
	push	ds			;save pointer to primary buffer
	push 	si
	mov	dx,ax			;save char
	mov	wTemp,di  		;save di
	mov	di,OFF_lpwDataCsd	;load cx with points
	mov	cx,ss:[di].ayBox	
	xor	ah,ah
	mul	cl			;ax = character table offset
	
	mov	si,ss:[di].OFF_lpbFont
	mov	ds,ss:[di].SEG_lpbFont
	add	si,ax			;ds:[si] -> char bit map

	mov	ax,ss
	mov	es,ax
	lea	di,LocalChar		;es:[di] -> local storage
	rep	movsb			;copy bitmap to local area
	mov	di,OFF_lpwDataCsd	;restore points
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

IF EGACSD OR VGACSD
	test	ss:[bx].finstInst,finstMonochrome	;Test for mono
	jnz	@F
	mov	dh,curattr
	cmp	dh,flastattr		;same attr ?
	jne	SetAttr
@@:
	jmp	SameAttr
SetAttr:
	push	bx
	mov	flastattr,dh
	mov	di,OFF_lpwDataCsd	;restore points
	test	ss:[di].fvmCurAdap,fvmMouse	;Test for EGA.SYS
	jz	@F
					;* Use extend int10 call
	mov	ax,ss			;save register values
	mov	es,ax
	lea	bx,rgbGraphicsRegSave
	mov	dx,10h			;Graphics control registers port
	mov	ah,0F2h
	mov	cx,4
	int	10h

;*	* move new state into register : al, ah, cl, ch
	mov	ah,curattr
	mov	al,ah			;al = foreground
	mov	cl,4
	shr	ah,cl			;ah = background value
	push	ax			;save attribute
	mov	al,ah			;load S/R with bkgd
	mov	ah,0Fh			;set Enable register
	mov	cl,ss:[bx+2]
	mov	ch,0		 	;data rotate

	lea	bx,rgbGraphicsRegT	;initiate buffer
	mov	ss:[bx],ax
	mov	ss:[bx+2],cx

	mov	ah,0F3h			;write registers
	mov	cx,4
	mov	dx,10h			;Graphics control registers port
	int	10h

	mov	ax,0A000h		;set video segment
	mov	es,ax
	mov	al,0FFh
	mov	di,wTemp		;restore di
	mov	es:[di],al
	mov	al,es:[di]		;load bit plane latches

	lea	bx,rgbGraphicsRegSave
	mov	byte ptr ss:[bx],0	;clear S/R (default)
	pop	ax
	xor	ah,al			;ah = bkgd xor fgd
	not	ah
	and 	ah,0Fh
	mov	byte ptr ss:[bx+1],ah	;load En S/R
	mov	byte ptr ss:[bx+3],18h	;set data rot reg (cpu xor latches)
	mov	ax,ss
	mov	es,ax
	mov	ah,0F3h			;write registers
	mov	cx,4
	mov	dx,10h			;Graphics control registers port
	int	10h
	jmp	SetAttrDone
@@:
					;* Direct hardware I/O
	mov	dx,3CEh			;Graphics control registers port
	mov	ax,0F01h		;set Enable register
	out 	dx,ax

	mov	ax,0003			;data rotate
	out	dx,ax

	mov	bh,curattr
	mov	bl,bh			;bl = foreground
	mov	cl,4
	shr	bh,cl			;bh = background value

	xor	al,al			;load S/R with bkgd
	mov	ah,bh
	out 	dx,ax

	mov	ax,0A000h		;set video segment
	mov	es,ax
	mov	al,0FFh
	mov	di,wTemp		;restore di
	mov	es:[di],al
	mov	al,es:[di]		;load bit plane latches

	xor	ax,ax			;clear S/R (default)
	out	dx,ax

	mov	ah,bh			;ah = bkgd xor fgd
	xor	ah,bl

	not	ah
	and 	ah,0Fh
	mov	al,1			;load En S/R
	out	dx,ax

	mov	ax,1803h		;set data rot reg (cpu xor latches)
	out	dx,ax

SetAttrDone:
	pop	bx

SameAttr:
ENDIF 	;EGACSD OR VGACSD

	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	
	mov	ax,0A000h		;set video segment
	mov	es,ax
	mov	di,wTemp		;restore di
IF MCGACSD
	cmp	ss:[bx].axMacInst,40
	jne	Not256

;;;256-color mode, 40x25
	push	bx
	mov	bh,curattr
	mov	bl,bh			;bl = foreground
	shr	bh,1			;bh = background value
	shr	bh,1
	shr	bh,1
	shr	bh,1
	and	bx,0F0Fh
L10:
	push	cx
	mov	cx,8			;1 byte/pixel (256-color)
	lodsb	
	mov	ah,al			;ah = bit pattern
L11:
	mov	al,bl			;al = foreground
	shl	ah,1			;carry = hi-bit
	jc	L12			;jmp if fgd
	mov	al,bh			;al = bkgd
L12:
	stosb				;update one pixel
	loop	L11
	add	di,320-8		;next line
	pop	cx
	loop	L10	

	pop	bx
	mov	di,wTemp
	add	di,8			;next char
	jmp short J13
Not256:
ENDIF	;MCGACSD
	mov	ax,04Fh			;column(80 - 1)

out_char_map:
	movsb
	add	di,ax			;go to next scan line (+80-1)
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
	mov	di,OFF_lpwDataCsd	;ss:[di] => drive data

IF EGACSD OR VGACSD
			  		;EGA and VGA only
	cmp	flastattr,0
	je	done_graf_update
			 		;restore default Graphics control registers
	mov	di,OFF_lpwDataCsd	
	test	ss:[di].fvmCurAdap,fvmMouse	;Test for EGA.SYS
	;STI
	jz	done_graf_update_no_ega_sys

;*	* Restore state of EGA.SYS
	mov	ax,ss
	mov	es,ax
	lea	bx,rgbGraphicsRegSave
	mov	dx,10h			;Graphics control registers port
	mov	ah,0F3h
	mov	cx,4
	int	10h
	jmp short done_graf_update

done_graf_update_no_ega_sys:
	mov	dx,3CEh			;restore Graphics control registers
	mov	ax,0003			;data rotate
	out	dx,ax
	mov	ax,0001			;zero En S/R
	out 	dx,ax
ENDIF	;EGACSD	OR VGACSD

ENDIF 	;EGACSD OR VGACSD OR MCGACSD

done_graf_update:

end_update:

cEnd	DoUpdateCsd

IF (CGACSD OR MCGACSD OR EGACSD OR VGACSD)
IF CGACSD	
	include cga8x8.inc		; extend font (128 - 255)
ENDIF
	include cpfont.asm		;* ChrisPFont with 8x8,8x14,8x16
ENDIF 	;(CGACSD OR MCGACSD OR EGACSD OR VGACSD)



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

;*	* SS:BX => INST info
;*	* SS:DI => Driver data

;set up Src pointer si
	mov	cx,ss:[di].ayBox
IF CGACSD
	shr	cx,1			;memory interleave
ENDIF	;CGACSD
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
IF CGACSD
	shr	ax,1
ENDIF	;CGACSD
	dec	ax
	mov	cx,80
	mul	cx
	mov	cl,dax
	dec	cx
	add	ax,cx			;ax = block offset
IF CGACSD
	add	ax,2000h		;start blt from odd line
ENDIF	;CGACSD
	add	di,ax			;si, di point to end of block
	add	si,ax	
	std				;set direction flag
fwd:

IF (EGACSD OR VGACSD)
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,ss:[bx].pinstDrv
	test	ss:[bx].finstInst,finstMonochrome
	jnz	@F
	mov	dx,3CEh			;Graphics Ctrl port
;	read mode 0, write mode 1
	mov	ax,0105h
	out	dx,ax
@@:
ENDIF	;(EGACSD OR VGACSD)
IF CGACSD
	mov	ax,0B800h
ELSE
	mov	ax,0A000h
ENDIF	;CGACSD
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
IF CGACSD
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
ELSE	;!CGACSD
	mov	dx,80
	cmp	di,si
	jb	@F
	neg	dx			;blt backward
@@:
ENDIF	;CGACSD
	add	si,dx			;go to next row
	add	di,dx
	loop	Blt1

IF (EGACSD OR VGACSD)
;restore default			
;	read mode 0, write mode 0
	mov	bx,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,ss:[bx].pinstDrv
	test	ss:[bx].finstInst,finstMonoChrome
	jnz	@F
	mov	dx,3CEh
	mov	ax,0005h
	out	dx,ax
@@:
ENDIF	;(EGACSD OR VGACSD)

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
IF CGACSD
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

ELSE	;EGACSD or VGACSD or MCGACSD
	mov	bh,3				;assume 8x8 fonts
	cmp	cl,14
	jne	@F
	dec	bh				;return 8x14 fonts
	jmp short RetFonts
@@:	
	cmp	cl,16
	jne	RetFonts
	mov	bh,6				;return 8x16 fonts
RetFonts:	
	mov	ax,1130h
	push	bp
	int	10h
	mov	si,bp
	pop	bp
	mov	cl,ds:[di].dyCharInft
	mov	al,char
	mul	cl
	add	si,ax
	mov	ax,es
	mov	ds,ax				;ds:si => character bit map
ENDIF	;CGACSD

	mov	ax,ss
	mov	es,ax
	mov	di,pbitmap			;es:di => destination
	rep	movsb
	
cEnd	GetCharMapCsd


;*****************************************************************************


	include	csd_std.asm		;* standard init/term
	include	csd_ibm.asm		;* IBM specific routines

;*****************************************************************************
;*	* only include one of the following, as appropriate

	include	csd_vram.asm		;* default procs for direct video I/O
	include	csd_save.asm		;* default screen save (none)

;*****************************************************************************

	include	csd_tail.asm		;* tail file

;*****************************************************************************


	END

