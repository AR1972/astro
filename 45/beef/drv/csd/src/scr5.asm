;*
;*	CW : Character Windows
;*
;*	scr5g.asm : OS/2 text+graphcis text installable screen driver
;*		included in vio.asm

	.286p
 
;*****************************************************************************

;*	* Display modes table (see scr5data.inc for DM2 and VM structures)

rgdm:

;* #0 text mode 0, monochrone adapter

	DB	fvmMDA					;* fvmRequired
	DB	fvmMD					;* fvmRequired
	DW	finstText OR finstMonochrome		;* flags
	DB	2					;* coMac
	DB	9, 14, 0, 0				;* font info
	MD	<12,00000000b,0,80,25,720,350>		;* mode data
	DW	1					;* video segment
	DW	0607H					;* cursor

	Assert	<($-rgdm) EQ SIZE DM>

;* #1 text mode 2, CGA adapter

	DB	fvmCGA					;* fvmRequired
	DB	fvmCD OR fvmECD				;* fvmRequired
	DW	finstText				;* flags
	DB	16					;* coMac
	DB	8, 8, 0, 0				;* font info
	MD	<12,00000101b,4,80,25,640,200>		;* mode data
	DW	1
	DW	0607H					;* cursor

;* #2 text mode 14, EGA adapter, ECD or CD

	DB	fvmEGA					;* fvmRequired
	DB	fvmCD OR fvmECD				;* fvmRequired
	DW	finstText				;* flags
	DB	16					;* coMac
	DB	8, 8, 0, 0				;* font info
	MD	<12,00000001b,4,80,25,640,200>		;* mode data
	DW	1
	DW	0607H				;* cursor

;* #3 text mode 7, EGA adapter, 25 lines

	DB	fvmEGA					;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstText				;* flags
	DB	16					;* coMac
	DB	8, 14, 0, 0				;* font info
	MD	<12,00000001b,4,80,25,640,350>		;* mode data
	DW	1
	DW	0C0DH					;* cursor

;* #4 text mode 7, EGA/VGA adapter, 43 lines

	DB	fvmEGA or fvmVGA			;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstText				;* flags
	DB	16					;* coMac
	DB	8, 8, 0, 0				;* font info
	MD	<12,00000001b,4,80,43,640,350>		;* mode data
	DW	1
	DW	0607H					;* cursor

;* #5 text mode 7, PS2, 25 lines

	DB	fvmVGA					;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstText				;* flags
	DB	16					;* coMac
	DB	8, 16, 0, 0				;* font info
	MD	<12,00000001b,4,80,25,720,400>		;* mode data
	DW	1
	DW	0E0FH					;* cursor

;* #6 text mode 7, PS2, 50 lines

	DB	fvmVGA			;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstText				;* flags
	DB	16					;* coMac
	DB	8, 8, 0, 0				;* font info
	MD	<12,00000001b,4,80,50,720,400>		;* mode data
	DW	1
	DW	0607H					;* cursor

IFDEF GRAPHICSTEXT

;'''''''''''''''
; Graphics Text
;,,,,,,,,,,,,,,,

;* OS/2 protect mode does not support a mouse under graphics (finstDisableMouse)

;* #7 graphics text mode, EGA adapter, 25 lines

	DB	fvmEGA					;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstGraphics OR finstFont OR finstDisableMouse
							;* flags
	DB	16					;* coMac
	DB	8, 14, 0, 0				;* font info
	MD	<12,00000011b,4,80,25,640,350>		;* mode data
	DW	0
	DW	0C0DH					;* cursor

;* #8 graphics text mode, EGA adapter, 43 lines

	DB	fvmEGA					;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstGraphics OR finstFont OR finstDisableMouse
							;* flags
	DB	16					;* coMac
	DB	8, 8, 0, 0				;* font info
	MD	<12,00000011b,4,80,43,640,350>		;* mode data
	DW	0
	DW	0607H					;* cursor

;* #9 graphics text mode, VGA, 30 lines

	DB	fvmVGA					;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstGraphics OR finstFont OR finstDisableMouse
							;* flags
	DB	16					;* coMac
	DB	8, 16, 0, 0				;* font info
	MD	<12,00000011b,4,80,30,640,480>		;* mode data
	DW	0					
	DW	0E0FH					;* cursor

;* #10 graphics text mode, VGA adapter, 60 lines

	DB	fvmVGA					;* fvmRequired
	DB	fvmECD					;* fvmRequired
	DW	finstGraphics OR finstFont OR finstDisableMouse
							;* flags
	DB	16					;* coMac
	DB	8, 8, 0, 0				;* font info
	MD	<12,00000011b,4,80,60,640,480>		;* mode data
	DW	0
	DW	0607H					;* cursor

ENDIF	;GRAPHICSTEXT

cdmMax	equ	($ - rgdm) / (cbDmMin)		;* # of modes


mpadapfvm:
	DB	fvmMDA,fvmCGA,fvmEGA,fvmVGA
pfvmMaxAdap	EQU	$

mpdispfvm:
	DB	fvmMD,fvmCD,fvmECD
pfvmMaxDisp	EQU	$


;*****************************************************************************

NonStandard 	FvmGetCur
NonStandard	ImodeGuessCurrentCsd
NonStandard	FInitCsd
NonStandard	MoveHwCursCsd
NonStandard	PrepUpdateCsd
NonStandard	DoUpdateCsd
NonStandard	DoneUpdateCsd
NonStandard 	FGetColorPaletteCsd
NonStandard 	SetColorPaletteCsd
NonStandard 	FQueryInftCsd
NonStandard 	CoiCovFromFvm
NonStandard 	GetPFonts
NonStandard 	GetCharMapCsd

;*****************************************************************************


;********** FvmGetCur **********
;*	entry:	DS:DI => driver data
;*	* if first time called, identify the current screen and set fvmCur
;*	*  subsequent calls will return fvmCur
;*	*  After this call: fvmCur will be initialized
;*	exit:	AL == fvm for current adapter (0 => no supported screen found)
;*		AH == fvm for current monitor

cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>
    localV	cdT,cbCdMin
cBegin	FvmGetCur

	mov	bx,pinos			;* prep for Os2Call

	AssertEQ di,OFF_lpwDataCsd
	mov	al,[di].fvmCurAdap
	mov	ah,[di].fvmCurDisp
	or	ax,ax
	jz	@F
	jmp	fgc_end
@@:	
	lea	si,cdT
	mov	cdT.cbCd,10
	PushArg	<0,ss,si,hvioCur>
	Os2Call	VioGetConfig
	or	ax,ax
	jz	@F
	xor	ax,ax				;* no hardware available !?
	jmp	short fgc_end
@@:
	mov	bx,drvOffset mpadapfvm
	add	bx,cdT.adapCd
	cmp	bx,drvOffset pfvmMaxAdap
	mov	al,fvmEGA			;* default if strange OS
	mov	ah,cs:[bx]
	jae	@F
	mov	al,cs:[bx]
@@:
	mov	bx,drvOffset mpdispfvm
	add	bx,cdT.dispCd
	cmp	bx,drvOffset pfvmMaxDisp
	mov	ah,fvmECD			;* default if strange OS
	jae	@F
	mov	ah,cs:[bx]
@@:
	mov	[di].fvmCurAdap,al
	mov	[di].fvmCurDisp,ah

fgc_end: ;* ax = fvm

cEnd	FvmGetCur



;********** ImodeGuessCurrentCsd **********
;*	entry:
;*	* Get information about current mode
;*	exit:	AX == current mode or imodeUnknown

cProc	ImodeGuessCurrentCsd, <FAR, PUBLIC, ATOMIC>, <ds,si,di>
    localW	fvm
    localV	mdT,cbMdMin
cBegin	ImodeGuessCurrentCsd

	mov	bx,pinos			;* prep for Os2Call

	mov	mdT.cbMd,cbMdMin
	lea	ax,mdT
	PushArg	<ss,ax,hvioCur>
	Os2Call	VioGetMode
	or	ax,ax
	jz	@F
imgcc_unknown:
	mov	ax,imodeUnknown
	jmp	short imgcc_end
@@:

;*	* Determine adapter type

	mov	di,OFF_lpwDataCsd
	call	FvmGetCur
	mov	fvm,ax

;*	* Search rgdm for appropriate imode

	push	cs
	pop	es
	mov	bx,drvOffset rgdm
	mov	dx,cdmMax
	xor	ax,ax
	
imgcc_search:
	lea	si,mdT
	mov	di,bx
	add	di,mdDm
	mov	cx,cbMdMin
	repz	cmpsb
	jcxz	@F
	jmp	short imgc_next
@@:
	push	ax				;* modes match, check fvm
	mov	ax,fvm
	test	al,cs:[bx].fvmReqAdapDm
	jz	@F				;* not this one but need pop
	test	ah,cs:[bx].fvmReqDispDm
@@:
	pop	ax
	jnz	short imgcc_end			;* found mode
imgc_next:
	add	bx,cbDmMin
	inc	ax
	dec	dx
	jg	imgcc_search

;*	* mode not present

	mov	ax,imodeUnknown

imgcc_end:

cEnd	ImodeGuessCurrentCsd



;********** FInitCsd **********
;*	entry:
;*		pinch = near pointer to INCH structure to fill
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <ds,di,si>
    parmDP pinstInit
    parmDP pinch
    localD lpwPrim
    localW cbLVB
    localV mdT,cbMdMin
IFDEF	GRAPHICSTEXT
    localV fiT,cbFiMin
    localV pbT,cbPbMin
ENDIF	;GRAPHICSTEXT
cBegin	FInitCsd
	
	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	mdT.cbMd,cbMdMin

;*	* set mode
	mov	bx,pinstInit			; add
	mov	[di].pinstDrv,bx
	mov	[di].fupdating,0
	mov	si,[bx].pdmInst			;* CS:SI => DM info

;*	* copy mode info into driver globals
	mov	ax,cs:[si].vparmCursOnDm
	mov	[di].vparmCursOn,ax
	mov	[di].vparmCursSize,ax
	mov	bx,pinos

;IFDEF DEBUG
	lea	ax,mdT
	PushArg	<ss,ax,hvioCur>
	Os2Call	VioGetMode
;ENDIF
	test	mdT.typeMd,02H			;graphics mode enable ?
	jnz	@F
	test	cs:[si].finstDm,finstGraphics	;text to graphics mode ? 
	jz	@F
; Need to clean up text buffer before switching into graphics mode
	push	si
	push	0	; blank cell
	mov	si,sp
	mov	bx,pinos
	PushArg	<0,0,-1,-1,-1,ss,si,hvioCur>
	Os2Call	VioScrollUp
	pop	ax	; remove cell
	pop	si
@@:
	lea	ax,[si].mdDm
	PushArg	<cs,ax,hvioCur>
	Os2Call	VioSetMode
	or	ax,ax
	jnz	fic_vioerr

IFDEF	GRAPHICSTEXT
	test	cs:[si].finstDm,finstGraphics
	jz	@F
	jmp	InitGraf			;* go to graphics modes
@@:
ENDIF	;GRAPHICSTEXT

	mov	dx,cs:[si].mdDm.axMacMd
	mov	ax,cs:[si].mdDm.ayMacMd
	mul	dl
	shl	ax,1
	mov	cbLVB,ax			;buffer length (bytes)
	lea	si,lpwPrim
	lea	ax,cbLVB
	PushArg	<ss,si,ss,ax,hvioCur>
	Os2Call	VioGetBuf
	or	ax,ax
	jnz	fic_vioerr

;*	* primary buffer must start at offset 0
	les	ax,lpwPrim
	or	ax,ax
	jnz	fic_vioerr

	mov	si,pinstInit
	mov	[si].psPrimInst,es		;* point to primary buffer

	lea	ax,[di].curType
	PushArg	<ds,ax,hvioCur>
	Os2Call	VioGetCurType
	or	ax,ax
	jnz	fic_vioerr

	mov	ax,[di].curType.atrCt		;* cursor is on after SetMode
	mov	[di].atrCurOn,ax

;*	* Clear the screen
	push	0	; blank cell
	mov	si,sp
	mov	bx,pinos
	PushArg	<0,0,-1,-1,-1,ss,si,hvioCur>
	Os2Call	VioScrollUp
	pop	ax	; remove cell
	jmp	short set_text_mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
fic_vioerr:
	xor	ax,ax
	jmp	fic_end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

set_text_mode:
;*	* default (IBM) INCH array is ok

;*	* make a local for the requestblock to VioSetState, init and call

	sub	sp,6			; allocate temp
;!!! REVIEW     ^^^^
	mov	si,sp
	mov	word ptr [si],6		; length
	mov	word ptr [si+2],2	; set blink/background intensity
	mov	word ptr [si+4],1	; enable high intensity background
	PushArg	<ds,si,hvioCur>
	Os2Call	VioSetState
	add	sp,6			; remove temp
;!!! REVIEW     ^^^^

IFDEF	GRAPHICSTEXT
	jmp	finitdone

InitGraf:
	;;;REVIEW: load font should be taken care by FQueryInft
	mov	fiT.cbFi,cbFiMin
	mov	fiT.typeFi,1		;get rom font
	mov	dl,cs:[si].dxCharDm	;== [di].InftInst.dxCharInft
	xor	dh,dh
	mov	fiT.axCellFi,dx
	mov	dl,cs:[si].dyCharDm	;== [di].InftInst.dyCharInft
	xor	dh,dh
	mov	fiT.ayCellFi,dx
	mov	[di].ayBox,dx
	mov	word ptr fiT.pbDataFi,0
	lea	si,fiT			;bios data
	mov	word ptr ss:[si+2].pbDataFi,0
	mov	fiT.cbDataFi,dx
	PushArg	<ss,si,hvioCur>
	Os2Call	VioGetFont		;get font table
	lea	si,fiT			
	mov	ax,word ptr ss:[si].pbDataFi	;load font pointers
	mov	[di].OFF_lpbFont,ax
	mov	ax,word ptr ss:[si+2].pbDataFi
	mov	[di].SEG_lpbFont,ax

	lea	di,pbT				;get video buffer address
	mov	word ptr ss:[di].pBufPb,0
	mov	word ptr ss:[di+2].pBufPb,0Ah
	mov	word ptr ss:[di].cbPb,6D60h	;* 640 * 350 / 8 bytes (EGA)
 	mov	word ptr ss:[di+2].cbPb,0

	mov	si,OFF_lpwDataCsd		;* Data in data segment
	test	[si].fvmCurAdap,fvmVGA or fvmMCGA
	jz	@F
	mov	word ptr ss:[di].cbPb,9600h	;* 640 * 480 / 8 bytes (PS2)
@@:
	test	[si].fvmCurAdap,fvmCGA
	jz	@F
	mov	word ptr ss:[di].pBufPb,8000h
	mov	word ptr ss:[di+2].pBufPb,0Bh
	mov	word ptr ss:[di].cbPb,3E80h	;* 640 * 200 / 8 bytes (CGA)
@@:
	PushArg	<ss,di,hvioCur>
	Os2Call	VioGetPhysBuf
	mov	ax,pbT.sel0Pb
	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	[di].SEG_lpbVideo,ax
						; Clear the bit planes 
	mov	es,ax
	xor	ax, ax
	mov	di, ax
	mov	cx, word ptr pbT.cbPb 		;cx = bits to clean
	cmp	cx,3E80h
	jne	@F
	shr	cx,1				;CGA use memory interleave
@@:
	shr	cx,1				;cx/8/2 = words to clean
	push	cx
	rep	stosw
	pop	cx
	cmp	word ptr pbT.cbPb,3E80h
	jne	finitdone
	mov	di,2000h
	rep	stosw				;clear odd field
finitdone:
ENDIF	;GRAPHICSTEXT

	mov	ax,sp				;* success

fic_end:	; ax == return code

cEnd	FInitCsd




;********** MoveHwCursCsd **********
;*	entry:
;*		axCurs, ayCurs = new absolute cursor position
;*		fOn => whether on or off
;*	* move and enable/disable cursor
;*	* save position in global for drawing
;*	exit:	n/a

cProc	MoveHwCursCsd,<FAR, PUBLIC, ATOMIC>, <DI>
    parmB axCurs
    parmB ayCurs
    parmW fOn
IFDEF	GRAPHICSTEXT
    localB axCursor
    localB ayCursor
ENDIF	;GRAPHICSTEXT
cBegin	MoveHwCursCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,pinos

;*	* move cursor position

	xor	ah,ah
	mov	al,ayCurs
	push	ax
	mov	al,axCurs
	push	ax
	push	hvioCur
	Os2Call	VioSetCurPos

;*	* enable/disable

	mov	ax,atrCurOff			;* assume off (-1)
	mov	byte ptr [di].fCurs,0		;* graphics cursor 
	cmp	fOn,0
	je	setCursorOnOff

	mov	dx,[di].vparmCursSize
	cmp	fOn,1
	je	@F
	xor	dh,dh				;(fOn = 2) Set block cursor
@@:
	mov	[di].vparmCursOn,dx		;* update cursor size
IFDEF	GRAPHICSTEXT
	mov	bx,[di].pinstDrv
	test	ss:[bx].finstInst,finstGraphics	;graphics text 8x14
	jz	DrawCursDone

	mov	dx,[di].posCurs		;dl = axFirst
	mov	ayCursor,dh
	mov	axCursor,dl
	xor	cx,cx			;fRestoreDbcs
	mov	ah,cl
	mov	al,80
	mul	dh
	mov	dh,cl
	add	ax,dx			
	shl	ax,1			;ax = offFirst
	mov	dx,1
	cCall	<Near ptr DoUpdateCsd>, <ayCursor,axCursor,dx,ax,cx,cs>	;erase cursor
	xor	cx,cx
	mov	dh,ayCurs
	mov	dl,axCurs
	mov	[di].posCurs,dx		;update position
	mov	ah,cl
	mov	al,80
	mul	dh
	mov	dh,cl
	add	ax,dx
	shl	ax,1			;ax = offFirst
	mov	dx,1
	mov	byte ptr [di].fCurs,1		;* graphics cursor on
	cCall	<Near ptr DoUpdateCsd>, <ayCurs,axCurs,dx,ax,cx,cs>		;draw cursor

DrawCursDone:
ENDIF	;GRAPHCISTEXT
	mov	ax,[di].atrCurOn		;* turn on
setCursorOnOff:
	mov	[di].curType.atrCt,ax
	
	mov	bx,pinos
	lea	ax,[di].curType
	PushArg	<ds,ax,hvioCur>
	Os2Call	VioSetCurType

cEnd	MoveHwCursCsd



;*****************************************************************************
;* Update


;********** PrepUpdateCsd **********
;*	entry:
;*		ayLine = ay of line drawn
;*		axFirst = first character drawn
;*		dax = # of characters drawn
;*		offFirst = offset in primary buffer where started
;*		fRestoreDbcs = bool to tell us to restore double byte
;*			characters or not.
;*	* prepare for screen update
;*	* For KANJI -- erase any dangling 1/2 characters
;*	exit: n/a

cProc	PrepUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DI>
    parmB ayLine
    parmB axFirst
    parmB dax			;* will never be 0
    parmW offFirst
    parmW fRestoreDbcs		;* => restore DBCS 1/2 characters
cBegin	PrepUpdateCsd

IFDEF KANJI
	Assert
ENDIF ;KANJI

	mov	di,OFF_lpwDataCsd

;*	* set offMac and offMin to initial values

	test	[di].fupdating,0ffh
	jnz	@F			; already updating, ignore this prep
	mov	ax,offFirst
	mov	[di].offMin,ax
	xor	bh,bh
	mov	bl,dax
	shl	bx,1
	add	ax,bx
	mov	[di].offMac,ax
	mov	[di].fupdating,1
@@:

cEnd	PrepUpdateCsd



;********** DoUpdateCsd **********
;*	entry:
;*		parameters just like PrepUpdateCsd
;*	* after primary buffer has been updated
;*	* For BIOS version -- send to screen
;*	* for Kanji -- parse for DBCS boundaries (OAX)
;*	exit: n/a

cProc	DoUpdateCsd,<FAR, PUBLIC, ATOMIC>, <DS, ES, DI, SI, BX>
    parmB ayLine
    parmB axFirst
    parmB dax			;* will never be 0
    parmW offFirst
    parmW fRestoreDbcs		;* => restore DBCS 1/2 characters
IFDEF	GRAPHICSTEXT
    localB axCurs
    localB axCurrent
    localB flastattr		;check repeat attribute
    localB curattr
    localW wTemp
    localV LocalChar,16		;storage of char bit map for ffont mode
ENDIF	;GRAPHICSTEXT
cBegin	DoUpdateCsd

	mov	di,OFF_lpwDataCsd
IFDEF	GRAPHICSTEXT
	mov	bx,[di].pinstDrv
	test	ss:[bx].finstInst,finstGraphics	;graphics text 8x14
	jnz	graf_update
ENDIF	;GRAPHICSTEXT
;*	* enlarge dirty section if necessary

	mov	ax,offFirst
	cmp	ax,[di].offMin
	jae	@F
	mov	[di].offMin,ax
@@:
	xor	bh,bh
	mov	bl,dax
	shl	bx,1
	add	ax,bx
	cmp	ax,[di].offMac
	jbe	@F
	mov	[di].offMac,ax
@@:

IFDEF	GRAPHICSTEXT
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

;***** Graphics Text ****
;*			*
;*			*

	mov	cx,ss:[di].ayBox
	test	ss:[di].fvmCurAdap,fvmCGA
	jz	notmode6a

	shr	cx,1			;memory interleave addressing

notmode6a:
	mov	si,dx			;save video mode dl
	xor	ax,ax			;calculate start of screen address
	mov	al,ayLine
	mul	cx
	mov	cx,80
	mul	cx
	xor	cx,cx
	mov	cl,axFirst
	add	ax,cx			;ax = ayLine x 80 x ayBox + axFirst
	mov	di,ax			;point to video buffer

	xor	cx,cx
	mov	cl,dax			;no. of char to be updated

	mov	dx,si
	mov	si,offFirst
;	cmp	dl,6			;CGA mode 6
;	jmp	out_next_char1

	jmp	notmode6b

;* CGA 640x200, mono, 80x25, 8x8 fonts
	
out_next_char1:
	lodsw				;load character+attribute
	push	cx
	mov	wTemp,di		;save di
	mov	di,OFF_lpwDataCsd	
	mov	cx,ss:[di].ayBox	;calculate character table address

	push	ds			;save pointer to primary buffer
	push 	si

	mov	si,ss:[di].OFF_lpbFont	;default
	mov	ds,ss:[di].SEG_lpbFont

	test	ss:[di].fvmCurAdap,fvmCGA
	jz	first128

	cmp	al,80h
	jb	first128
		     			;use extend font (128 - 255)
	mov	dx,cs
	mov	ds,dx
	mov	si,drvOffset rgbCGAExFont8x8	;see cga8x8.inc
	sub	al,80h
first128:
	mov	dx,ax			;save char + attribute
	xor	ah,ah
	mul	cl			
	add	si,ax				;ds:[si] -> char bit map

	mov	ax,ss
	mov	es,ax
	lea	di,LocalChar			;es:[di] -> local storage
	rep	movsb  				;copy char bit map into local area
	mov	di,OFF_lpwDataCsd		;restore points
	mov	cx,ss:[di].ayBox	
	mov	ds,ax
	lea	si,LocalChar
	cmp	ss:[bx].psSecInst,0		;is sec. buffer allocated ?
	je	notfont8

	test	ss:[bx].finstInst,finstFont	;ffont mode ?
	jz 	notfont8

	mov	al,dl			;save char
	pop	di	;si -> di
	push	di
	mov	es,ss:[bx].psSecInst		;es:[di] => ffont buffer
	mov	dl,es:[di-2]			;dx = ffont + attribute
	or	dl,dl				;normal ?
	jz	notfont8

	cmp	al,' ' 			;check spaces
	jne	notspace1
	
	test	dl,ffontUnderline or ffontDoubleUnderline or ffontStrikeThrough
	jz	notfont8

notspace1:
	cCall	ChrisPFont 
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	;restore points

notfont8:
	mov	al,axCurrent		;cursor position ?
	cmp	al,axCurs
	jne	CursOff1

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
outCurs1:				;display the cursor
	not	byte ptr [si]
	inc	si
	loop	outCurs1
	pop	si
	pop	cx

CursOff1:
	mov	ax,0B800h			;CGA video segment
	mov	es,ax

	mov	ax,1FFFh
	mov	dx,79-2000h
	mov	di,wTemp			;restore di
	test	di,2000h
	jz	out_one_byte

	xchg	ax,dx 			;exchange inc if 1st pixel lies in
		      			;odd interleave 
out_one_byte:				;output the character
	movsb
	add	di,ax
	xchg	ax,dx
	loop	out_one_byte

	mov	di,wTemp
	inc	di			;next char
	inc	axCurrent		;bump update position
	pop	si
	pop	ds
	pop	cx			;restore dax
	dec	cx
	jz	out_done1
	jmp	out_next_char1

out_done1:
	jmp	done_graf_update	;done


;******* EGA/MCGA/VGA ********
;*
;*

notmode6b:				
	mov	flastattr,0		;initialize with black:black

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
; REVIEW: use lines below when FQueryInft is ready
;	mov	si,ss:[bx].inftInst.OFF_lpbFontLower128Inft
;	mov	ds,ss:[bx].inftInst.SEG_lpbFontLower128Inft
	add	si,ax			;ds:[si] -> char bit map

	mov	ax,ss
	mov	es,ax
	lea	di,LocalChar		;es:[di] -> local storage
	rep	movsb	   		;copy bitmap to local area
	mov	di,OFF_lpwDataCsd	;restore points
	mov	cx,ss:[di].ayBox	
	mov	ds,ax
	lea	si,LocalChar

	cmp	ss:[bx].psSecInst,0		;is sec. buffer allocated ?
	je	@F

	test	ss:[bx].finstInst,finstFont	;graphics text 8x14, 8x16
	jz	@F

	mov	ax,dx
	pop	di	;si -> di
	push	di
	mov	es,ss:[bx].psSecInst	;* es:[di] => ffont buffer
	mov	dx,es:[di-2]		;dx = ffont + attribute
	or	dl,dl			;zero ffont
	jz 	@F

	cmp	al,' ' 			;check spaces
	jne	notspace2
	
	test	dl,ffontUnderline or ffontDoubleUnderline or ffontStrikeThrough
	jnz 	notspace2
@@:
	jmp 	notfont14or16

notspace2:
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
	test	ss:[di].fvmCurAdap,fvmEGA or fvmVGA	;Test for EGA or VGA
	jz	same_attr

	test	ss:[bx].finstInst,finstMonochrome	;Test for mono
	jnz	same_attr

	mov	ah,curattr
	cmp	ah,flastattr		;same attr ?
	jne	set_attribute

same_attr:
	jmp	same_attribute

set_attribute:
	mov	flastattr,ah
	mov	cl,ah
	push	bx
	mov	di,wTemp		;restore di
	mov	bx,pinos
	PushArg	<0,0,3CEh,3CFh>		;access EGA graphics ctrl chip
	Os2Call	DosPortAccess
	mov	bx,pinos
	call	[bx].lpfnCwBeginIOInos

	mov	bh,cl			;free ax for i/o

	mov	dx,3CEh			;Graphics control registers port
	mov	ax,0F01h		;set Enable register
	out 	dx,ax

	mov	ax,0003			;data rotate
	out	dx,ax

	mov	bl,bh			;bl = foreground
	mov	cl,4
	shr	bh,cl			;bh = background value

	xor	al,al			;load S/R with bkgd
	mov	ah,bh
	out 	dx,ax

	mov	cx,di
	mov	di,OFF_lpwDataCsd	;load cx with points
	mov	ax,[di].SEG_lpbVideo	;set video segment
	mov	es,ax
	mov	al,0FFh
	mov	di,cx
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

	mov	bx,pinos
	call	[bx].lpfnCwEndIOInos
	PushArg	<0,1,3CEh,3CFh>		;release EGA graphics ctrl chip
	Os2Call	DosPortAccess
	pop	bx

same_attribute:
;;;* output the character
	mov	di,OFF_lpwDataCsd	
	mov	ax,ss:[di].SEG_lpbVideo	;set video segment
	mov	cx,ss:[di].ayBox	
	mov	es,ax
	mov	ax,04Fh			;column(80) - 1
	mov	di,wTemp		;restore di

out_char_map:
	movsb
	add	di,ax			;go to next scan line (+80-1)
	loop	out_char_map
	
	mov	di,wTemp		;restore di
	inc	di			;next char
	inc	axCurrent		;bump update position
	pop	si
	pop	ds
	pop	cx			;restore dax
	dec 	cx
	jz	out_done2
	jmp	out_next_char

out_done2:
	mov	di,OFF_lpwDataCsd	;ss:[di] => drive data
	test	ss:[di].fvmCurAdap,fvmMCGA	;Test for MCGA
	jnz	done_graf_update
			  		;EGA and VGA only
			 		;restore default Graphics control registers
	mov	bx,ss
	mov	ds,bx
	mov	bx,pinos
	PushArg	<0,0,3CEh,3CFh>		;access EGA graphics ctrl chip
	Os2Call	DosPortAccess
	mov	bx,pinos
	call	[bx].lpfnCwBeginIOInos
	mov	dx,3CEh			;restore Graphics control registers
	mov	ax,0003			;data rotate
	out	dx,ax
	mov	ax,0001			;zero En S/R
	out 	dx,ax
	mov	bx,pinos
	mov	bx,pinos
	call	[bx].lpfnCwEndIOInos
	PushArg	<0,1,3CEh,3CFh>		;release EGA graphics ctrl chip
	Os2Call	DosPortAccess

done_graf_update:

ENDIF	;GRAPHICSTEXT

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





;********** DoneUpdateCsd **********
;*	entry: n/a
;*	* Update complete
;*	exit: n/a

cProc	DoneUpdateCsd,<FAR, PUBLIC, ATOMIC>,<DI>
cBegin	DoneUpdateCsd

;*	* restore old cursor position

	mov	di,OFF_lpwDataCsd

	mov	ax,[di].offMin
	mov	cx,[di].offMac
	sub	cx,ax
	mov	bx,pinos
	PushArg	<ax,cx,hvioCur>
	Os2Call	VioShowBuf
	mov	[di].fupdating,0

cEnd	DoneUpdateCsd


;********** FQueryInftCsd **********	(assume called after FQueryInst)
;*	entry:	pinft, ifont
;*	* get font info
;*	exit:	AX = 0 => no more fonts
;*		ax != 0	Success,filled INFT
;*		trash bx,dx

cProc	FQueryInftCsd, <FAR, PUBLIC, ATOMIC>, <si, di>
    parmDP pinft
    parmW  ifont
cBegin	FQueryInftCsd

	mov	si,ifont
	cmp	si,cdmMax
	jb	@F
	jmp	ExitFQueryInft			; no more fonts
@@:
	mov	ax,SIZE DM
	mul	si
	mov	si,ax
	add	si,drvOffset rgdm		;* CS:SI => INST info
	mov	di,pinft			;* ds:di => INFT
	Assert	<dyCharDm EQ dxCharDm+1>
	Assert	<dyCharInft EQ dxCharInft+1>
	mov	dx,word ptr cs:[si].dxCharDm
	mov	wo ds:[di].dxCharInft,dx
					;* move both dxChar and dyChar
	mov	dl,cs:[si].dyBaseDm
	mov	ds:[di].dyBaseLineInft,dl
	
	mov	ax,ifont
	mov	ds:[di].ifontInft,al		;font index

	mov	ax,sp

ExitFQueryInft:
cEnd	FQueryInftCsd


;*****************************************************************************



;********** FGetColorPaletteCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	FGetColorPaletteCsd, <FAR, PUBLIC, ATOMIC>,<DI>
    parmW	coGet
    parmDP	pcovGet
    parmDP	pcoiGet
    localV	prT,cbPr1
cBegin	FGetColorPaletteCsd

	mov	bx,pinos

	lea	di,prT
	mov	prT.cbPr,cbPr1
	xor	ax,ax
	mov	prT.rtPr,ax
	mov	ax,coGet
	test	ax,0fff0h			;* check co range (0-15)
	jnz	fgcp_fail
	mov	cx,pcoiGet
	jcxz	fgcp_cov
fgcp_fail:
	xor	ax,ax
	jmp	short fgcp_end
fgcp_cov:
	mov	prT.coFirstPr,ax
	PushArg	<ss,di,hvioCur>
	Os2Call	VioGetState
	or	ax,ax
	jnz	fgcp_fail
	mov	bx,pcovGet
	mov	ax,prT.rgcovPr
	mov	[bx],ax
	mov	ax,sp				;* success
fgcp_end:

cEnd	FGetColorPaletteCsd



;********** SetColorPaletteCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	SetColorPaletteCsd, <FAR, PUBLIC, ATOMIC>,<DI>
    parmW	coSet
    parmW	covSet
    parmDP	pcoiSet
    localV	prT,cbPr1
cBegin	SetColorPaletteCsd

	mov	bx,pinos

	lea	di,prT
	mov	prT.cbPr,cbPr1
	xor	ax,ax
	mov	prT.rtPr,ax
	mov	ax,coSet
	test	ax,0fff0h			;* check co range (0-15)
	jnz	@F
	mov	prT.coFirstPr,ax
	mov	ax,covSet
	mov	prT.rgcovPr,ax
	PushArg	<ss,di,hvioCur>
	Os2Call	VioSetState
@@:

cEnd	SetColorPaletteCsd





;********** CoiCovFromFvm **********
;*	entry:	AL = fvm
;*	* Produce coiMac, covMac for fvm (AL)
;*	exit:	AL = fvm, AH = covMac, DX = coiMac

cProc	CoiCovFromFvm, <NEAR, PUBLIC, ATOMIC>
cBegin	CoiCovFromFvm

	xor	ah,ah
	xor	dx,dx
	test	al,fvmMDA		;* monochrome coiMac = 0, covMac = 0
	jnz	@F
	test	al,fvmCGA		;* CGA coiMac = 0, covMac = 0
	jnz	@F
	mov	ah,64			;* EGA/VGA coiMac = 0, covMac = 64
@@:

cEnd	CoiCovFromFvm



;********** GetPFonts **********
;*	entry:	AL = ifont
;*	* get font information
;*	exit:	DX:AX = lpbFont for first 128 characters
;*		CX:BX = lpbFont for last 128 characters

cProc	GetPFonts, <NEAR, PUBLIC, ATOMIC>
cBegin	GetPFonts

	xor	ax,ax
	xor	dx,dx
	xor	cx,cx
	xor	bx,bx

IFDEF REVIEW
	.... implement this
ENDIF ;REVIEW

cEnd	GetPFonts


;********** GetCharMapCsd **********
;*	entry:	pinft, ch, pbitmap
;*	* get character bit map

cProc	GetCharMapCsd, <FAR, ATOMIC, PUBLIC>
    parmDP pinft
    parmB  char
    parmDP pbitmap    
cBegin	GetCharMapCsd

cEnd	GetCharMapCsd

;*****************************************************************************

	include	cga8x8.inc

IFDEF	GRAPHICSTEXT
;*****************************************************************************

	CPF814 = 1
	CPF816 = 1
	include cpfont.asm

;*****************************************************************************
ENDIF	;GRAPHICSTEXT
