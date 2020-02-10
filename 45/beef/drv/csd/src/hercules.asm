;*
;*	CW : Character Windows Drivers
;*
;*	hercules.asm : for Hercules monographics, graphics plus, & in-color
;*****************************************************************************

	include	csd_head.inc

	include	csd_data.inc

;*****************************************************************************

	include	csd_code.asm			;* first part of code

;*	* Display modes table
rgdm:
;* for text mode, mode = 0 (9x14), 1 (8x10)
;* for graphics mode, mode = 5 just for identification
;* #0 - standard monochrome text mode (9 x 14 characters)
	DB	fvmHerc102 or fvmHerc112 or fvmHerc222	;* hardware needed
	DB	fvmMD
	DB	7				;* mode
	DW	finstText OR finstMonochrome
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	9, 14, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0b0cH				;* cursor
	DW	0				;* extra (RamFont info)
	Assert	<($-rgdm) EQ SIZE DM>
;* #1 - RamFont 80 x 25 (9 x 14 characters)
	DB	fvmHerc112 or fvmHerc222	;* hardware needed
	DB	fvmMD
	DB	7				;* mode
	DW	finstText OR finstMonochrome OR finstFont
	DB	80, 25				;* screen size
	DB	2				;* coMac
	DB	9, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0b0cH				;* cursor
	DW	14				;* extra (RamFont info)
;* #2 - RamFont 90 x 35 (8x 10 characters)
	DB	fvmHerc112 or fvmHerc222	;* hardware needed
	DB	fvmMD
	DB	7				;* mode
	DW	finstText OR finstMonochrome OR finstFont
	DB	90, 35				;* screen size
	DB	2				;* coMac
	DB	8, 10, 0, 0			;* INFT
	DW	0				;* video address
	DW	0809H				;* cursor
	DW	10				;* extra (RamFont info)
;* #3 - standard color text mode (9 x 14 characters)
	DB	fvmHerc222			;* hardware needed
	DB	fvmECD
	DB	7				;* mode
	DW	finstText
	DB	80, 25				;* screen size
	DB	16				;* coMac
	DB	9, 14, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0b0cH				;* cursor
	DW	0				;* extra (RamFont info)

;* #4 - (4K) RamFont 90 x 35 (8x 10 characters)
	DB	fvmHerc222			;* hardware needed
	DB	fvmECD
	DB	7				;* mode
	DW	finstText 
	DB	90, 35				;* screen size
	DB	16				;* coMac
	DB	8, 10, 0, 0			;* INFT
	DW	0B000H				;* video address
	DW	0809H				;* cursor
	DW	16				;* extra (RamFont info)

;* #5 - standard monochrome graphics text mode (8 x 14 characters)
	DB	fvmHerc102 or fvmHerc112 	;* hardware needed
	DB	fvmMD
	DB	5				;* mode
	DW	finstGraphics OR finstMonochrome OR finstFont
	DB	90, 25				;* screen size
	DB	2				;* coMac
	DB	8, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0C0DH				;* cursor
	DW	0				;* extra (RamFont info)

;* #6 - standard monochrome graphics text mode (8 x 8 characters)
	DB	fvmHerc102 or fvmHerc112 	;* hardware needed
	DB	fvmMD
	DB	5				;* mode
	DW	finstGraphics OR finstMonochrome OR finstFont
	DB	90, 43				;* screen size
	DB	2				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* extra (RamFont info)
  
;* #7 - standard color graphics text mode (8 x 14 characters)
	DB	fvmHerc222			;* hardware needed
	DB	fvmECD
	DB	5				;* mode
	DW	finstGraphics OR finstFont
	DB	90, 25				;* screen size
	DB	16				;* coMac
	DB	8, 14, 0, 0			;* INFT
	DW	0				;* video address
	DW	0C0DH				;* cursor
	DW	0				;* extra (RamFont info)

lastdm:		;* This points to the last (best) DM entry
;* #8 - standard color graphics text mode (8 x 8 characters)
	DB	fvmHerc222			;* hardware needed
	DB	fvmECD
	DB	5				;* mode
	DW	finstGraphics OR finstFont
	DB	90, 43				;* screen size
	DB	16				;* coMac
	DB	8, 8, 0, 0			;* INFT
	DW	0				;* video address
	DW	0607H				;* cursor
	DW	0				;* extra (RamFont info)


cdmMax	equ	($ - rgdm) / (size DM)		;* # of modes

;*****************************************************************************

;*	* initialization bytes for 6845
rgbInit	label	byte
cRegInit	equ	12		;* number of registers to load

rgb9x14	db	61h, 50h, 52h, 0fh		;* 80 x 25 (9 x 14 characters)
	db	19h, 06h, 19h, 19h
	db	02h, 0dh, 0bh, 0ch

rgb8x10	db	6dh, 5ah, 5ch, 0fh		;* 90 x 35 (8 x 10 characters)
	db	24h, 00h, 23h, 23h
	db	02h, 09h, 07h, 08h

GTABLE		EQU	THIS BYTE
		DB	53, 45, 46, 7, 91, 2, 87, 87, 2, 3, 0, 0, 0, 0, 0, 0

;*****************************************************************************
hgcIndex	equ	03b4h		;* 6845 index register
hgcControl	equ	03b8h		;* display mode control port
hgcStatus	equ	03bah		;* Display Status Port
hgcConfig	equ	03bfh		;* configuration port
xmodereg	equ	14h		;* xMode register
scorereg	equ	15h		;* underscore register
strikereg	equ	16h		;* overstrike register
exceptreg	equ	17h		;* exception register
fHercGr		equ	82h		;* bits for turning on Hercules graphics mode
fHercTxt	equ	00h		;* bits for turning on Hercules text mode
fHercVideo	equ	08h		;* bits for turning on Hercules video
idmask_222	equ	01110000b
idcode_222	equ	01010000b
idmask_112	equ	00110000b
idcode_112	equ	00010000b
RamFontON	EQU	00000001B	; 0 - ROM generator (normal text)
					; 1 - RamFont ON
CharacterWidth	EQU	00000010B	; 0 - 9 bits wide, 1 - 8 bits wide
RamFont_48k	EQU	00000100B	; 0 - 4k RamFont, 1 - 48k RamFont
ramfont_48k_9d	equ	RamFont_48k OR RamFontON
ramfont_48k_8d	equ	RamFont_48k OR CharacterWidth OR RamFontON
ramfont_4k_8d	equ	CharacterWidth OR RamFontON
text_buf_seg	equ	0b000h
ramfont_seg	equ	0b400h

;* for RamFont modes:
hbold	equ	80h
hrev	equ	40h
hstrike	equ	20h
hunder	equ	10h
hnormal	equ	0
;each 256 font table eats up 4K, so 48K RamFont can store up to 12 fonts
font1	equ	0	;this is the italics font
font2	equ	1	;this is the italics+overstrike font
font3	equ	2	;this is the superscript font, also minicaps
font4	equ	3	;superscript+overstrike, also minicaps+overstrike
font5	equ	4	;superscript+italics, also minicaps+italics
font6	equ	5	;superscript+italics+overstrike, and mini+ital+strike
font7	equ	6	;the subscript font
font8	equ	7	;contains the normal font
font9	equ	8	;contains the normal+overstrike font
font10	equ	9	;the subscript+overstrike font
font11	equ	10	;the subscript+italics font
font12	equ	11	;the subscript+italics+overstrike font
;*****************************************************************************

NonStandard	FvmGetCur
NonStandard	ImodeGuessCurrentCsd
NonStandard	FQueryInftCsd
NonStandard	FInitCsd
NonStandard	MoveHwCursCsd
NonStandard	DoUpdateCsd
NonStandard	TermCsd			
NonStandard	FQueryInstCsd		;temporary

;*****************************************************************************

;********** ImodeGuessCurrentCsd **********
;*	
;*	*This version starts guessing from the best mode

cProc	ImodeGuessCurrentCsd, <FAR, PUBLIC, ATOMIC>, <SI,DI>
cBegin	ImodeGuessCurrentCsd

	mov	di,OFF_lpwDataCsd

	cCall	FvmGetCur		;* get fvm

	push	ax
	cCall	ModeGetCur		;* al = mode, ah = ayMac (0=>unknown)
	pop	bx			;* bx = fvm

;*	* Search for current mode and fvm in rgdm

	mov	si,drvOffset lastdm	;* start from last (best) mode
	mov	cx,cdmMax
	mov	dx,cx
	dec	dx

;*	* al = current mode, ah = ayMac (or 0=>unknown)
;*	* bx = fvm
;*	* si = pdm
;*	* dx = idm = imode
;*	* cx = loop count

imgc_next:
	cmp	al,cs:[si].modeDm
	jne	@F
	test	bl,cs:[si].fvmReqAdapDm
	jz	@F				
	test	bh,cs:[si].fvmReqDispDm
	jz	@F			;* not available
	or	ah,ah
	jz	imgc_end		;* height unknown => use this one
	cmp	ah,cs:[si].ayMacDm
	jz	imgc_end		;* same height => use this one

@@:
	sub	si,size DM
	dec	dx
	loop	imgc_next
	mov	dx,-1			;* unknown
imgc_end:	;* dx = imode
	mov	ax,dx			;* guess this mode

cEnd	ImodeGuessCurrentCsd


;********** FQueryInstCsd **********
;*	* CSD entry point (see documentation for interface)

cProc	FQueryInstCsd, <FAR, PUBLIC, ATOMIC>, <si, di>
    parmDP pinst
    parmW imode
    localW fvm
cBegin	FQueryInstCsd

	mov	di,OFF_lpwDataCsd
	cCall	FvmGetCur	;* find out what we got ...
				;* will query hardware + codepage first call
	mov	fvm,ax

	mov	cx,[di].codepageBase
	mov	si,imode
	cmp	si,cdmMax
	jb	got_imode
;*	* see if alternate code page
	mov	cx,[di].codepageAlt		; Bail if no alternate
	jcxz	fail_query			;   codepage.

	cmp	cx,[di].codepageBase		; Bail if alternate codepage
	je	fail_query			;   is the same as base.

	sub	si,cdmMax			;* back to mode 0
	cmp	si,cdmMax			;* times 2 for alternate code page
	jb	got_imode
fail_query:
	xor	ax,ax			;* failure
	jmp	end_qmode

got_imode: ;* si = imode, cx = codepage
	mov	ax,SIZE DM
	mul	si
	mov	si,ax
	add	si,drvOffset rgdm		;* CS:SI => INST info

;*	* copy DM info into INST
	mov	di,pinst			;* ds:di => dest
;*	* clear out the INST structure
	push	cx				;* save codepage
	push	di
	push	ds
	pop	es
	mov	cx,cbInstMin / 2
	xor	ax,ax
	rep stosw
	pop	di
	pop	ds:[di].codepageInst

;*	* move information from DM to INST
	;* finst
	mov	ax,fvm
	mov	dx,cs:[si].finstDm
	test	al,cs:[si].fvmReqAdapDm
	jz	@F
	test	ah,cs:[si].fvmReqDispDm
	jz	@F
	or	dx,finstAvailable	;* this mode is currently available
@@: ;* dx = finst
	mov	ds:[di].finstInst,dx

IFDEF EARLIER
	Assert	<ayMacDm EQ axMacDm+1>
	Assert	<ayMacInst EQ axMacInst+1>
	;* axMac, ayMac
	mov	dx,word ptr cs:[si].axMacDm
ELSE
	mov	dl,cs:[si].axMacDm
	mov	dh,cs:[si].ayMacDm
ENDIF
	mov	wo ds:[di].axMacInst,dx		;* move both axMac and ayMac
	;* coMac, covMac, coiMac
	mov	dl,cs:[si].coMacDm
	mov	ds:[di].coMacInst,dl
	cCall	CoiCovFromFvm			;* al = fvm
	mov	ds:[di].covMacInst,ah
	mov	ds:[di].coiMacInst,dx

	;* INFT information
	Assert	<dyCharDm EQ dxCharDm+1>
	Assert	<dyCharInft EQ dxCharInft+1>
	mov	dx,word ptr cs:[si].dxCharDm
	test	cs:[si].finstDm,finstGraphics	;!!!REVIEW
	jz	@F
	mov	word ptr ds:[di].inftInst.dxCharInft,dx
					;* move both dxChar and dyChar
@@:
	mov	dl,cs:[si].dyBaseDm
	mov	ds:[di].inftInst.dyBaseLineInft,dl
	mov	al,cs:[si].ifontDm
	cCall	GetPfonts
	mov	ds:[di].inftInst.OFF_lpbFontLower128Inft,AX
	mov	ds:[di].inftInst.SEG_lpbFontLower128Inft,DX
	mov	ds:[di].inftInst.OFF_lpbFontUpper128Inft,BX
	mov	ds:[di].inftInst.SEG_lpbFontUpper128Inft,CX

	;* Buffer info
	mov	ax,cs:[si].psVideoDm
	mov	ds:[di].psPrimInst,ax
	AssertEQ ds:[di].psSecInst,0
	AssertEQ ds:[di].cwExtraInst,0

;*	* set private info (store pointer to DM in the INST structure)
	mov	[di].pdmInst,si

	mov	ax,sp				;* ok
end_qmode:

cEnd	FQueryInstCsd


;********** FvmGetCur **********
;*	* Identify the current screen and return the appropriate fvm in AL
;*	* NOTE:	This is a NEAR routine.
;*	*	This routine may trash AX, BX, CX, DX, SI, DI, or ES
;*	exit:	AL == fvm for current screen or 0 if no supported screen found

cProc	FvmGetCur, <NEAR, PUBLIC, ATOMIC>, <DS>
cBegin	FvmGetCur

	AssertEQ di,OFF_lpwDataCsd
	mov	al,[di].fvmCurAdap
	mov	ah,[di].fvmCurDisp
	or	ax,ax
	jz	init_get
	jmp	end_fvm_get

init_get:	;* initial get
	MOV	DX,hgcStatus		; First check for a GB112
	XOR	BX,BX			; Start with no 112 or 222 successes
	MOV	CX,100			; Get a majority for 100 tries

Check_GB112:
	IN	AL,DX
	AND	AL,idmask_222		; Strip to just the 222 bits
	CMP	AL,idcode_222		; Check for the GB222 id code
	JNE	Check_112		; No - skip to test for 112
	INC	BH			; Count another 222 success

Check_112:
	AND	AL,idmask_112		; Strip furthur, to just the 112 bits
	CMP	AL,idcode_112		; Check for the GB112 id code
	JNE	Check_Again		; No - skip
	INC	BL			; Count another 112 success

Check_Again:
	LOOP	Check_GB112		; Loop back and check again
	CMP	BL,50			; Did we get a majority of 112 hits?
	JBE	Check_GB102		; If not, check for a Hercules GB102
	mov	ax,fvmHerc112		; Indicate we have a GB112
	CMP	BH,50			; Did we get a majority for the 222?
	JBE	FvmQueryRet
	mov	ax,fvmHerc222		; Indicate we have a GB222
Not_GB222:
	JMP	SHORT FvmQueryRet	; Skip the GB102 check

Check_GB102:
	MOV	DX,hgcStatus 		; Bit 7 at port 3BA changes pretty often
	MOV	CX,0FFFFH		; on a Hercules so check it to see if
	IN	AL,DX			; it flips.
	MOV	BL,AL

INIThLp:
	IN	AL,DX			; Current value of Port 3BA
	XOR	AL,BL			; XOR to see if it changes
	OR	AL,AL			; Did it?
	JS	INITh1			; Yes, we must have a Hercules
	LOOP	INIThLp 		; No, try again
	xor	ax,ax
	JMP	SHORT end_fvm_get	; If we didn't get it, give up

INITh1:
	mov	ax,fvmHerc102		;* Indicate we have a GB102
FvmQueryRet:
	mov	ah,0ffh			;* currently allow all monitors
	mov	[di].fvmCurAdap,al
	mov	[di].fvmCurDisp,ah
	mov	[di].codepageBase,437		;* assume standard ASCII
	mov	[di].codepageAlt,0		;* assume no alternate
end_fvm_get: ;* ax = fvm

cEnd	FvmGetCur


;********** FInitCsd **********
;*	entry:
;*		pinch = near pointer to INCH structure to fill
;*	* Initialize the screen to the given mode
;*	exit:	AX != 0 if ok

cProc	FInitCsd, <FAR, PUBLIC, ATOMIC>, <DI,SI>
    parmDP pinst
    parmDP pinch
    localB bRamFont
cBegin	FInitCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment

;*	* set mode
	mov	bx,pinst
	mov	[di].pinstDrv,bx
	mov	bx,ds:[bx].pdmInst		;* CS:BX => DM info

;*	* copy mode info into driver globals
	mov	ax,cs:[bx].vparmCursOnDm
	mov	[di].vparmCursOn,ax
	mov	ax,cs:[bx].wExtraDm
	mov	[di].wExtra,ax
	mov	bRamFont,al			;* save the RamFont info byte

	mov	dx,hgcConfig		
	mov	al,3				;* use 48K RamFont mode
	out	dx,al

;set hgc full
	mov	dx,hgcIndex
	mov	ax,(20h SHL 8) OR exceptreg	;* reg 17
	test	cs:[bx].fInstDm,fInstMonochrome
	jnz	SetMono
	xor	ah,ah
SetMono:
	out	dx,ax

	test	cs:[bx].finstDm,fInstGraphics
	jz	notGraf

;*** Graphics mode
	
	xor	ah,ah
	mov	al,cs:[bx].dyCharDm		;points
	mov	[di].ayBox,ax

	mov	al,40h			;segBios
	mov	es,ax
	mov	di,49h			;CRT_MODE
	mov	byte ptr es:[di],5	;fake mode 6 for mouse sake
	mov	di,4Ah			;CRT_COLS
	mov	word ptr es:[di],90	;
	mov	di,84h			;CRT_ROWS
	mov	al,cs:[bx].ayMacDm 	;rows - 1
	dec	al
	mov	byte ptr es:[di],al	
		
	mov	dx,hgcControl		;set graphics mode
	mov	al,fHercGr
	out	dx,al
	mov	dx,hgcIndex		;write index register
	mov	cx,16
	xor	bx,bx
IN9hLp:
	mov	ah,cs:GTABLE[bx]	
	mov	al,bl
	out	dx,ax
	jmp	$ + 2
	inc	bx
	loop	IN9hLp

	mov	ch,40h
	mov	ax,0B800h
	mov	es,ax
	xor	di,di
	mov	ax,di
	rep	stosw
	mov	ax,fHercGr
	push	ax
	jmp	FInitRet

;*** Text mode

notGraf:
	xor	ah,ah
	mov	al,40h
	mov	es,ax
	mov	si,49h			;CRT_MODE
	mov	byte ptr es:[si],7	
	mov	si,4Ah			;CRT_COLS
	mov	al,cs:[bx].axMacDm
	mov	word ptr es:[si],ax	
	mov	si,84h			;CRT_ROWS
	mov	al,cs:[bx].ayMacDm 	;rows - 1
	dec	al
	mov	byte ptr es:[si],al	

	mov	si,drvOffset rgbInit	;default 80x25 (9x14 font)
	cmp	cs:[bx].ayMacDm,35	
	jne	@F
	add	si,cRegInit		;use 90x35 (8x10 font) 
@@:
	mov	ax,fHercTxt
	push	ax			; save mode byte
	mov	dx,hgcControl		; set to graphics or text mode...
	out	dx,al			;   ... with video off

	mov	dx,hgcIndex		; initialize 6845 chip with table
	mov	cx,cRegInit		;
	xor	ah,ah			;
	cld				;
SMloop:
	mov	al,ah			;
	out	dx,al			; send register number to index port
	inc	dx			;
	lods	byte ptr cs:[si]	; get byte of data to send
	out	dx,al			; and send it to index+1
	jmp	$ + 2			; WARNING -- THIS DELAY IS REQUIRED
	jmp	$ + 2			; FOR 8MHz ATs!
	inc	ah			;
	dec	dx			;
	loop	SMloop			; loop until all 16 bytes are output

	;dx = hgcIndex
	mov	al,bRamFont
	cmp	al,16
	je	Init8x10Color
	cmp	al,14
	je	Init9x14
	cmp	al,10
	je	Init8x10

;select ROM character generator
	MOV	AX,(0) OR xmodereg
	OUT	DX,AX

;set underscore at 14
	MOV	AX,(13 SHL 8) OR scorereg
	OUT	DX,AX

	jmp	short FInitRet

Init8x10Color:
	cCall	Init4KRamFont8x10
	jmp	short FInitRet

Init8x10:
	cCall	InitRamFont8x10
	jmp	short FInitRet

Init9x14:
	cCall	InitRamFont9x14

;*	* the INCH array already contains the standard Code Page 437
;*	*  character set, so it usually can be left the same.

FInitRet:
	pop	ax			; now turn video on
	or	al,fHercVideo		;
	mov	dx,hgcControl		;
	out	dx,al			;

	mov	ax,sp				;* success
cEnd	FInitCsd


;********** InitRamFont9x14 **********
;*	Loads the RamFont memory with the 9x14 fonts
;*	entry:	DX = hgcIndex

cProc	InitRamFont9x14, <NEAR>, <ds, si, di>
cBegin	InitRamFont9x14

;* This code is from QINTER (MCV9X14.ASM):

;select 48k ramfont
	MOV	AX,(ramfont_48k_9d SHL 8) OR xmodereg
	OUT	DX,AX

;set underscore at 14
	MOV	AX,(13 SHL 8) OR scorereg
	OUT	DX,AX

;set overstrike at 13, it becomes double underline
	MOV	AX,(12 SHL 8) OR strikereg
	OUT	DX,AX

;load the fonts into ramfont memory
;first, clear all of ramfont memory
	mov	bx,ramfont_seg		
	mov	es,bx
	MOV	CX,6000H		;48K bytes
	XOR	AX,AX
	MOV	DI,AX
rep	stosw

;load the normal font into t8
	mov	bx,cs
	mov	ds,bx
	mov	si,offset rgbHercFont9x14
	mov	di,7000h
	mov	bx,256

mak_norm:
	MOV	CL,7			; CH = 0 from last REP
rep	movsw
	INC	DI
	INC	DI
	dec	bx
	jnz	mak_norm

;copy the normal font into t9
	mov	bx,es
	mov	ds,bx
	mov	di,8000h
	mov	si,7000h
	MOV	CH,8			; CL = 0 from last REP
rep	movsw

;create the normal strike font
	mov	di,8000h + 7
	MOV	CH,1			; CL = 0 from last REP
	mov	ax,0ffh

make_str:
	stosb
	add	di,15
	loop	make_str

;create the superscript font
	mov	di,2000h
	mov	si,7000h + 1
	MOV	CH,1			; CL = 0 from last REP

mak_super:
	movsw
	INC	SI
	movsw
	movsw
	INC	SI
	movsw
	movsw
	add	si,4
	add	di,6
	loop	mak_super

;fix a few individual characters to be acceptable
;start with the lower case superscript e
	mov	di,2650h + 6
	mov	al,60h
	stosb

;fix the lower case superscript j
	mov	di,26a0h + 6
	mov	al,36h
	stosb
	mov	al,1ch
	stosb
	XOR	AL,AL
	MOV	CL,3			; CH = 0 from last LOOP
rep	stosb

;fix the lower case superscript s
	mov	di,2730h + 3
	mov	al,3eh
	stosb
	mov	al,60h
	stosb
	mov	al,3eh
	stosb
	mov	al,3
	stosb
	mov	al,3eh
	stosb

;create the superscript+overstrike font
	mov	si,2000h
	mov	di,3000h
	MOV	CH,8			; CL = 0 from last REP
rep	movsw
	mov	di,3000h + 4
	mov	ax,0ffh
	MOV	CH,1			; CL = 0 from last REP

mak_sup_str:
	stosb
	add	di,15
	loop	mak_sup_str

;create the subscript font
	mov	di,6000h + 6
	mov	si,2000h
	mov	cx,800h - 3
rep	movsw

;help a few of the subscripted characters to look better
;start with the subscripted lower case g
	mov	di,6670h + 6 - 2	;shift up by 2
	mov	si,6670h + 6
	MOV	CL,5			; CH = 0 from last REP
	XOR	AX,AX
rep	movsw
	stosw
;fix the subscripted lower case p
	mov	di,6700h + 6 - 2	;shift up by 2
	mov	si,6700h + 6
	MOV	CL,5			; CH = 0 from last REP
rep	movsw
	stosw

;fix the subscripted lower case q
	mov	di,6710h + 6 - 2	;shift up by 2
	mov	si,6710h + 6
	MOV	CL,5			; CH = 0 from last REP
rep	movsw
	stosw

;fix the subscript lower case y
	mov	di,6790h + 6 - 2	;shift up by 2
	mov	si,6790h + 6
	MOV	CL,5			; CH = 0 from last REP
rep	movsw
	stosw

;create the subscript+overstrike font
	mov	di,9000h
	mov	si,6000h
	MOV	CH,8			; CL = 0 from last REP
rep	movsw
	mov	di,9000h + 10
	NOT	AL			; AX = 0FFH
	MOV	CH,1			; CL = 0 from last REP

mak_sub_str:
	stosb
	add	di,15
	loop	mak_sub_str

;create the italic font
mak_ital:
	mov	ax,ramfont_seg
	mov	es,ax
	mov	ds,ax
	cld
	mov	si,7000h
	XOR	DI,DI
	mov	cx,800h
rep	movsw
	mov	bx,256
	XOR	SI,SI
	MOV	DI,SI

itals0:
	MOV	CL,5			; CH = 0 from last REP

itals1:
	lodsb
	shr	al,1
	stosb
	loop	itals1
	add	di,3
	add	si,3
	MOV	CL,4			; CH = 0 from last REP

itals2:
	lodsw
	shl	al,1
	shl	ah,1
	stosw
	loop	itals2
	dec	bx
	jnz	itals0

;create the italics+overstrike font

do_ital_str:
	mov	di,1000h
	XOR	SI,SI
	MOV	CH,8			; CL = 0 from last LOOP
rep	movsw
	MOV	CH,1			; CL = 0 from last REP
	mov	ax,0ffh
	mov	di,1000h + 7

mak_ital_str:
	stosb
	add	di,15
	loop	mak_ital_str

;create the superscript+italics font
	mov	di,4000h
	mov	si,2000h
	MOV	CH,8			; CL = 0 from last LOOP
rep	movsw
	mov	si,4000h
	MOV	DI,SI
	MOV	CH,1			; CL = 0 from last REP

mak_sup_ital:
	push	cx
	lodsw
	shr	ah,1
	shr	al,1
	stosw
	lodsb
	shr	al,1
	stosb
	INC	SI
	INC	SI
	INC	DI
	INC	DI
	mov	cx,4

sh_lft:
	lodsw
	shl	ah,1
	shl	al,1
	stosw
	loop	sh_lft
	add	si,3
	add	di,3
	pop	cx
	loop	mak_sup_ital

;create the superscript+italics+overstrike font
	mov	si,4000h
	mov	di,5000h
	MOV	CH,8			; CL = 0 from last LOOP
rep	movsw
	mov	di,5000h + 4
	mov	ax,0ffh
	MOV	CH,1			; CL = 0 from last REP

mak_sup_istr:
	stosb
	add	di,15
	loop	mak_sup_istr

;create the subscript+italics font
	mov	di,0a000h + 6
	mov	si,4000h
	mov	cx,800h - 3
rep	movsw

;help a few of the subscripted characters to look better
;start with the subscripted lower case g
	mov	di,0a670h + 6 - 2	;shift up by 2
	mov	si,0a670h + 6
	MOV	CL,5			; CH = 0 from last REP
	XOR	AX,AX
rep	movsw
	stosw
;fix the subscripted lower case p
	mov	di,0a700h + 6 - 2	;shift up by 2
	mov	si,0a700h + 6
	MOV	CL,5			; CH = 0 from last REP
rep	movsw
	stosw

;fix the subscripted lower case q
	mov	di,0a710h + 6 - 2	;shift up by 2
	mov	si,0a710h + 6
	MOV	CL,5			; CH = 0 from last REP
rep	movsw
	stosw

;fix the subscript lower case y
	mov	di,0a790h + 6 - 2	;shift up by 2
	mov	si,0a790h + 6
	MOV	CL,5			; CH = 0 from last REP
rep	movsw
	stosw

;create the subscript+italics+overstrike font
	mov	di,0b000h
	mov	si,0a000h
	MOV	CH,8			; CL = 0 from last REP
rep	movsw
	mov	di,0b000h + 10
	NOT	AL			; AX = 0FFH
	MOV	CH,1			; CL = 0 from last REP

mak_sub_ita_str:
	stosb
	add	di,15
	loop	mak_sub_ita_str

;make the mini caps font
	mov	si,2410h
	mov	di,2010h
	XOR	AX,AX
	stosw
	stosb
	MOV	CL,26 * 8 - 2		; CH = 0 from last LOOP
rep	movsw

;make the mini caps+overstrike font
	mov	si,3410h
	mov	di,3010h
	stosw
	stosb
	MOV	CL,26 * 8 - 2		; CH = 0 from last REP
rep	movsw

;make the mini caps italics characters
	mov	si,4410h
	mov	di,4010h
	stosw
	stosb
	MOV	CL,26 * 8 - 2		; CH = 0 from last REP
rep	movsw

;make the mini caps and italics overstrike font
	mov	si,5410h
	mov	di,5010h
	stosw
	stosb
	MOV	CL,26 * 8 - 2		; CH = 0 from last REP
rep	movsw

cEnd	InitRamFont9x14


;********** InitRamFont8x10 **********
;*	Loads the RamFont memory with the 8x10 fonts

cProc	InitRamFont8x10, <NEAR>, <ds, si, di>
cBegin	InitRamFont8x10

;* This code is from QINTER (MCV9X14.ASM):

;select 48k ramfont
	MOV	AX,(ramfont_48k_8d SHL 8) OR xmodereg	
	OUT	DX,AX

;set underscore at 10
	MOV	AX,(9 SHL 8) OR scorereg
	OUT	DX,AX

;set overstrike at 9
	MOV	AX,(8 SHL 8) OR strikereg
	OUT	DX,AX

;load the fonts into ramfont memory
;first clear all of ramfont memory
	mov	bx,ramfont_seg		; 0b400h
	mov	es,bx
	MOV	CH,60H			; CL = 0 from last LOOP
	XOR	AX,AX
	MOV	DI,AX			; start from b000:4000
rep	stosw

;load the normal font into t8
;	mov	bx,segROM
	mov	bx,0f000h		; PC ROM Fonts (0-127)
	mov	ds,bx
;	mov	si,offset rgFont
	mov	si,0fa6eh
	mov	di,7000h + 1		;t8
	mov	bx,128

norm88:
	MOV	CL,4			; CH = 0 from last REP
rep	movsw
	add	di,8
	dec	bx
	jnz	norm88

	mov	bx,cs			; Hard coded Fonts (128-255)
	mov	ds,bx
	mov	si,offset rgbHercExFont8x10
	mov	bx,128
	dec	di

norm188:
	MOV	CL,5			; CH = 0 from last REP
rep	movsw
	add	di,6
	dec	bx
	jnz	norm188

;copy the normal font into t9
	mov	bx,es
	mov	ds,bx
	mov	di,8000h		;t9
	mov	si,7000h
	MOV	CH,8			; CL = 0 from last REP
rep	movsw

;create the normal strike font
	mov	di,8000h + 4
	MOV	CH,1			; CL = 0 from last REP
	NOT	AL			; AX = 0FFH

norm_str88:
	stosb
	add	di,15
	loop	norm_str88

;create the superscript font
	mov	si,7000h + 1
	mov	bx,256
	mov	di,2000h
	XOR	AX,AX

super88:
	movsb				;0 = 1 pos
	inc	si
	movsw
	movsb
	inc	si
	movsw
	MOV	CL,4			; CH = 0 from last LOOP
rep	stosw
	INC	DI
	INC	DI
	add	si,8
	dec	bx
	jnz	super88

;fix some of the superscript characters to look acceptable
	mov	al,30h
	mov	di,2670h + 3
	or	es:[di],al	;fix the superscript g
	mov	di,2700h + 3
	or	es:[di],al	;fix the superscript p
	mov	di,2710h + 3
	or	es:[di],al	;fix the superscript q
	mov	di,2790h + 3
	or	es:[di],al	;fix the superscript y

;fix the superscript Q
	mov	di,2510h
	mov	al,78h
	stosb
	MOV	AX,0CCCCH
	STOSW
	mov	al,0d8h
	stosb
	mov	al,7ch
	stosb
	mov	al,06h
	stosb

;fix the superscript e
	mov	di,2650h
	XOR	AL,AL
	stosb
	mov	al,78h
	stosb
	mov	al,0fch
	stosb
	mov	al,0c0h
	stosb
	mov	al,78h
	stosb

;create the superscript+overstrike font
	mov	di,3000h
	mov	si,2000h
	MOV	CH,8			; CL = 0 from last REP
rep	movsw
	mov	di,3000h + 2
	mov	ax,0ffh
	MOV	CH,1			; CL = 0 from last REP

sup_str88:
	stosb
	add	di,15
	loop	sup_str88

;create the subscript font
	mov	di,6000h + 5
	mov	si,2000h
	mov	cx,800h - 3
rep	movsw

;create the subscript+overstrike font
	mov	di,9000h + 5
	mov	si,3000h
	mov	cx,800h - 3
rep	movsw

;create the italics font using an algorithm

mak8_ital:
	cld
	mov	ax,ramfont_seg
	mov	es,ax
	mov	ds,ax
	XOR	DI,DI
	mov	si,7000h
	mov	cx,800h
rep	movsw
	XOR	DI,DI
	MOV	SI,DI
	mov	bx,256

itals_88:
	lodsb
	shr	al,1
	shr	al,1
	stosb
	lodsw
	shr	al,1
	shr	ah,1
	stosw
	INC	DI
	INC	DI
	mov	si,di
	MOV	CL,3			; CH = 0 from last REP

ital_881:
	lodsw
	shl	al,1
	shl	ah,1
	stosw
	loop	ital_881
	add	di,5
	mov	si,di
	dec	bx
	jnz	itals_88

;create the italics+overstrike font

do8_ital_str:
	mov	di,1000h
	XOR	SI,SI
	MOV	CH,8			; CL = 0 from last LOOP
rep	movsw
	MOV	CH,1			; CL = 0 from last REP
	mov	ax,0ffh
	mov	di,1000h + 4

mak8_ital_str:
	stosb
	add	di,15
	loop	mak8_ital_str

;create the superscript+italics font
	mov	si,2000h
	mov	di,4000h
	MOV	CH,8			; CL = 0 from last LOOP
rep	movsw
	mov	si,4000h
	MOV	DI,SI
	mov	bx,256

ital_sup88:
	lodsw
	shr	al,1
	shr	ah,1
	stosw
	INC	DI
	INC	DI
	INC	SI
	INC	SI
	MOV	CL,3			; CH = 0 from last REP

shl_sup:
	lodsw
	shl	al,1
	shl	ah,1
	stosw
	loop	shl_sup
	add	di,6
	mov	si,di
	dec	bx
	jnz	ital_sup88

;create the superscript+italics+overstrike font
	mov	di,5000h
	mov	si,4000h
	MOV	CH,8			; CL = 0 from last LOOP
rep	movsw
	mov	di,5000h + 2
	MOV	CH,1			; CL = 0 from last REP
	mov	ax,0ffh

sup_ita_ovr:
	stosb
	add	di,15
	loop	sup_ita_ovr

;create the subscript+italics font
	mov	di,0a000h + 5
	mov	si,4000h
	mov	cx,800h - 3
rep	movsw

;create the subscript+italics+overstrike font
	mov	di,0b000h + 5
	mov	si,5000h
	mov	cx,800h - 3
rep	movsw

;make the mini caps font
	mov	si,2410h
	mov	di,2010h
	XOR	AX,AX
	stosw
	stosb
	MOV	CL,26 * 8 - 1		; CH = 0 from last REP
rep	movsw

;make the mini+overstrike font
	mov	si,3410h
	mov	di,3010h
	stosw
	stosb
	MOV	CL,26 * 8 - 1		; CH = 0 from last REP
rep	movsw

;make the mini+italics font
	mov	si,4410h
	mov	di,4010h
	stosw
	stosb
	MOV	CL,26 * 8 - 1		; CH = 0 from last REP
rep	movsw

;make the mini+italics+overstrike font
	mov	si,5410h
	mov	di,5010h
	stosw
	stosb
	MOV	CL,26 * 8 - 1		; CH = 0 from last REP
rep	movsw

Exit8x10:
cEnd	InitRamFont8x10


;********** Init4KRamFont8x10 **********
;*	Loads the RamFont memory with the 8x10 fonts

cProc	Init4KRamFont8x10, <NEAR>, <ds, si, di>
cBegin	Init4KRamFont8x10

;* This code is from QINTER (MCV9X14.ASM):

;select 4k ramfont
	mov	dx,hgcIndex
	MOV	AX,(ramfont_4k_8d SHL 8) OR xmodereg	
	OUT	DX,AX

;make RAM at B000:4000 addressable
	mov	dx,hgcConfig		
	mov	al,1				
	out	dx,al

;set underscore at 10
	MOV	AX,(9 SHL 8) OR scorereg
	OUT	DX,AX

;set overstrike at 9
	MOV	AX,(8 SHL 8) OR strikereg
	OUT	DX,AX

;* load the fonts into ramfont memory

;first clear all of ramfont memory
	mov	bx,ramfont_seg		; 0b400h
	mov	es,bx
	mov	cx,800h			; 4K bytes
	XOR	AX,AX
	MOV	DI,AX			; start from b000:4000
rep	stosw

;load the normal font 
	mov	bx,0f000h		; PC ROM Fonts (0-127)
	mov	ds,bx
	mov	si,0fa6eh
	mov	di,1			
	mov	bx,128
L0_127:
	MOV	CL,4			; 8 bytes-map/char
rep	movsw				
	add	di,8			; 16 bytes/char 
	dec	bx
	jnz	L0_127

;;;	di points to Ex128
	mov	bx,cs			; Hard coded Fonts (128-255)
	mov	ds,bx
	mov	si,offset rgbHercExFont8x10
	mov	bx,128
	dec	di
L128_255:
	MOV	CL,5			; 10 bytes-map/char
rep	movsw
	add	di,6			; 16 bytes/char
	dec	bx
	jnz	L128_255

cEnd	Init4KRamFont8x10


;********** MoveHwCursCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* Standard BIOS call for hardware cursor
;*	* save new position in "posCurs"

cProc	MoveHwCursCsd,<FAR, PUBLIC, ATOMIC>, <DI>
    parmB axCurs
    parmB ayCurs
    parmW fOn
    localB axCursor
    localB ayCursor
cBegin	MoveHwCursCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment

;*	* move cursor position
	mov	bh,0
	mov	dh,ayCurs
	mov	dl,axCurs

	mov	bx,[di].pinstDrv
	mov	al,dh
	mul	[bx].axMacInst
	xor	dh,dh
	add	ax,dx
	mov	dx,hgcIndex
	mov	bl,al
	mov	al,14
	out	dx,ax
	mov	ah,bl
	mov	al,15
	out	dx,ax				;* SetCursorPosition

	mov	ax,2000H			;* assume off
	mov	byte ptr [di].fCurs,0		;* graphics cursor 
	cmp	fOn,0
	je	SetCursorOnOff			;* turn off

	mov	bx,[di].pinstDrv
	test	ss:[bx].finstInst,finstGraphics	;graphics text 8x14
	jz	DrawCursDone

	mov	dx,[di].posCurs		;dl = axFirst
	mov	ayCursor,dh
	mov	axCursor,dl
	xor	cx,cx			;fRestoreDbcs
	mov	ah,cl
	mov	al,90
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
	mov	al,90
	mul	dh
	mov	dh,cl
	add	ax,dx
	shl	ax,1			;ax = offFirst
	mov	dx,1
	mov	byte ptr [di].fCurs,1		;* graphics cursor on
	cCall	<Near ptr DoUpdateCsd>, <ayCurs,axCurs,dx,ax,cx,cs>		;draw cursor

DrawCursDone:
	mov	ax,[di].vparmCursOn		;* turn on
SetCursorOnOff:
	mov	bl,al
	mov	al,10
	mov	dx,hgcIndex
	out	dx,ax
	mov	ah,bl
	mov	al,11
	out	dx,ax				;* SetCursorType

cEnd	MoveHwCursCsd


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
    localB bLastAttr
    LocalV LocalChar,14
cBegin	DoUpdateCsd

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	cmp	[di].wExtra,0
	jne	UpdateRamFont
	jmp	CheckGrafText

UpdateRamFont:
	mov	bx,[di].pinstDrv
	test	ds:[bx].finstInst, finstMonoChrome
	jnz	@F
	jmp	DoUpdateExit			;* 16 color mode (90x35)
@@:
	mov	ah,ds:[bx].axMacInst
	mov	dx,ds:[bx].psSecInst		;* DX:SI => secondary buffer
	mov	ds,ds:[bx].psPrimInst		;* DS:SI => primary buffer
	mov	si,offFirst

	mov	cx,text_buf_seg
	mov	es,cx
	mov	al,ayLine
	mul	ah
	shl	ax,1
	xor	ch,ch
	mov	cl,axFirst
	add	ax,cx
	add	ax,cx
	mov	di,ax				;* ES:DI => video buffer

;*	* DS:SI => start character in primary buffer
;*	* DX:SI => start character in secondary buffer
;*	* ES:DI => start character in video buffer

;*	* copy all characters to screen

	mov	cl,dax
loop_update:
	lodsw					;* al = char, ah = attr

	;*	* Need to adjust attribute byte here
	xor	ch,ch				;* start with normal attributes
	cmp	ah,087h				;* check for reverse video
	jne	NotRevVideo
	or	ch,hrev
NotRevVideo:
	and	dx,dx
	jnz	ChkFonts

	;*	* no secondary buffer, check normal display attributes
	test	ah,40h
	jz	NotBoldAttr
	or	ch,hbold
NotBoldAttr:
	cmp	al,176
	jl	ChkUnderline
	cmp	al,223
	jle	NotUnderlineAttr
ChkUnderline:
	mov	bh,ah
	and	bh,07h
	cmp	bh,01h
	jne	NotUnderlineAttr
	or	ch,hunder
NotUnderlineAttr:
	jmp	jus_normal

ChkFonts:
	push	ds
	mov	ds,dx
	mov	ah,ds:[si-2]			;* get ffont byte
	pop	ds
	test	ah,ffontBold
	jz	NotBold
	or	ch,hbold
NotBold:
	
;deal with graphics characters mapping, first
	cmp	al,176
	jl	not_graph
	cmp	al,223
	jg	not_graph
	jmp	jus_normal

not_graph:
	test	ah,ffontUnderline
	jz	NotUnderline
	or	ch,hunder
NotUnderline:
	test	ah,ffontDoubleUnderline
	jz	NotDoubleUnderline
	or	ch,hstrike OR hunder	; StrikeThrough is the second underline
NotDoubleUnderline:

	test	ah,ffontSuperScript
	jz	chk_sub
	test	ah,ffontSubScript
	jz	chk_control

	;*	* SuperScript+SubScript = MiniCaps
	;*	* check for a valid minicaps character
	cmp	al,'A'
	jl	chk_italics
	cmp	al,'Z'
	jg	chk_italics
	sub	al,40h		; adjust ascii code to point to right position
				; in the superscript font which
				; contains minicaps characters
	jmp	SHORT do_super_mini

;if superscript control code ascii characters then map to normal font

chk_control:
	cmp	al,27
	jle	do_normal

;its superscript or minicaps

do_super_mini:
	test	ah,ffontItalic	;check for super+italics
	jnz	do_sup_ital
	test	ah,ffontStrikeThrough	;check for super+strike
	jz	just_sup
	add	ch,font4	;its super+overstrike
	jmp	SHORT store_char

just_sup:
	add	ch,font3	;its just superscript
	jmp	SHORT store_char

;its superscript+italics

do_sup_ital:
	test	ah,ffontStrikeThrough	;check for super+italics+over
	jz	jus_sup_ita
	add	ch,font6	;its super+italics+overstrike
	jmp	SHORT store_char

jus_sup_ita:
	add	ch,font5	;its super+italics
	jmp	SHORT store_char

;check for subscripted characters

chk_sub:
	test	ah,ffontSubScript
	jz	chk_italics
	test	ah,ffontItalic	;check for subscript+italics
	jnz	do_sub_ital
	test	ah,ffontStrikeThrough	;check for subscript+overstrike
	jz	just_sub
	add	ch,font10	;its the subscript+overstrike font
	jmp	SHORT store_char

;its just subscript

just_sub:
	add	ch,font7
	jmp	SHORT store_char

;its subscript+italics

do_sub_ital:
	test	ah,ffontStrikeThrough	;check for subscript+italics+overstrike
	jz	jus_sub_ita
	add	ch,font12
	jmp	SHORT store_char

;its just subscript+italics

jus_sub_ita:
	add	ch,font11
	jmp	SHORT store_char

;check for normal italics characters

chk_italics:
				; The following = TEST AH,mitalic
	test	ah,ffontItalic	;check for italics characters
	jz	do_normal
	test	ah,ffontStrikeThrough	;check for italics+overstrike
	jz	jus_italics
	add	ch,font2
	jmp	SHORT store_char

;its just the normal italics font

jus_italics:
	add	ch,font1
	jmp	SHORT store_char

;its the normal character set

do_normal:
	test	ah,ffontStrikeThrough	;check for normal+overstrike
	jz	jus_normal
	add	ch,font9	;its normal+overstrike
	jmp	SHORT store_char

;its the normal font

jus_normal:
	add	ch,font8

;store the character and attributes
store_char:
	mov	ah,ch
OColor8x10:
	stosw
	dec	cl
	jz	DoUpdateRet
	jmp	loop_update

DoUpdateRet:
	jmp	DoUpdateExit
	
CheckGrafText:
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
	mov	cl,ss:[bx].axMacInst		;screen width
	mov	al,ayLine 
	xor	ah,ah
	mul	cl
	mov	cx,ax
	shl	ax,1			;mul 2
	xor	dh,dh
	mov	dl,axFirst
	add	ax,dx
	cmp     ss:[di].ayBox,14
	jne	SkipShl
	
	mov	ax,cx
	shl	ax,1
	shl	ax,1
	shr	cx,1
	sub	ax,cx			;mul 3.5
	add	ax,dx
	test	ayLine,1	    	;even - plane 0,odd - plane 2
	jz	SkipShl
	
	add	ax,4000h - 45
SkipShl:
	mov	di,ax			;* ES:DI => video buffer (pixel address)

	mov	bLastAttr,0
	xor	ch,ch
	mov	cl,dax			;# of characters to be updated

out_next_char:
	lodsw			;ah - attr, al - char
	push	cx		;save dax
	push	ds		;save pointer to primary buffer
	push	si			
	mov	wTemp,di		;save video address
	mov	di,OFF_lpwDataCsd
	test	ss:[bx].finstInst,fInstMonoChrome
	jnz	SameAttr

	cmp	ah,bLastAttr
	je	SameAttr		
					;* update color attribute
	mov	bLastAttr,ah
	mov	cx,ax
	mov	dx,hgcIndex
	mov	al,1Ah			;* write color register
	out	dx,ax
	mov	ax,cx			;* restore attribute

SameAttr:
	mov	cx,cs
	mov	ds,cx			;DS:[SI] -> char table
	mov	cx,ss:[di].ayBox
	mov	si,drvOffset rgbHercFont8x14
	cmp	cl,8
	jne	First128

	mov	dx,ax
	mov	ax,0F000h		;System Font Table (8x8)
	mov	ds,ax
	mov	ax,dx
	mov	si,0FA6Eh
	cmp	al,80h
	jb	First128

	sub	al,80h
	mov	si,drvOffset rgbHercExFont8x8
	push	cs
	pop	ds
First128:
	mov	dx,ax			;save char + attribute
	xor	ah,ah
	mul	cl
	add	si,ax			

	mov	ax,ss
	mov	es,ax
	lea	di,LocalChar			;es:[di] -> local storage
	rep	movsb  	   			;copy char bit map into local area
	mov	di,OFF_lpwDataCsd		;restore points
	mov	cx,ss:[di].ayBox	
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
	mov	al,dl			;restore character
	mov	dl,es:[di-2]		
	or	dl,dl
	jz	notFont

	cmp	al,' ' 			;check spaces
	jne	notspace1
	
	test	dl,ffontUnderline or ffontDoubleUnderline or ffontStrikeThrough
	jz	notfont

notspace1:
	cCall	ChrisPFont 
notFont:
	mov	cx,0B800h		;video seg
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
	shr	cl,1
	mov	di,wTemp
	mov	ax,2000h - 1
OC9Lp:
	movsb				; And put back to screen memory
	ADD	DI,AX			; Move down to next scan line
	movsb				; And put back to screen memory
	ADD	DI,AX			; Move down to next scan line
	cmp	di,8000h
	jb	OC9lp0

	ADD	DI,90 - 8000H
OC9lp0:
	loop	OC9Lp

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

	mov	di,OFF_lpwDataCsd		;* Data in data segment
	mov	bx,[di].pinstDrv
	test	ss:[bx].finstInst,fInstMonoChrome
	jnz	@F
	mov	ax,0F1Ah		;* restore color register (1Ah) to
	mov	dx,hgcIndex		;* default color (0Fh = mono)
	out	dx,ax
@@:
cEnd	DoUpdateCsd


	CPF814 = 1
	include cpfont.asm		;* ChrisPFont with 8x8,8x14


;********** FQueryInftCsd **********
;*	* CSD entry point (see documentation for interface)
;*	ax != 0	Success
;*	filled INFT

cProc	FQueryInftCsd, <FAR, PUBLIC, ATOMIC>, <SI, DI>
    parmDP pinft
    parmW ifont
cBegin	FQueryInftCsd

	mov	di,OFF_lpwDataCsd
	mov	di,[di].pinstDrv
	xor	ax,ax
	test	[di].finstInst,finstAvailable
	mov	di,pinft			;* ds:di => dest
	mov	ds:[di].OFF_lpbFontLower128Inft,0	;null pointers
	mov	ds:[di].SEG_lpbFontLower128Inft,0
	jz	ExitFQueryInft

	mov	si,ifont
	cmp	si,cdmMax			;* end of table ?
	jae	ExitFQueryInft

	mov	ax,SIZE DM
	mul	si
	mov	si,ax
	add	si,drvOffset rgdm		;* CS:SI => INST info
	mov	di,pinft			;* ds:di => dest
	Assert	<dyCharDm EQ dxCharDm+1>
	Assert	<dyCharInft EQ dxCharInft+1>
	mov	dx,word ptr cs:[si].dxCharDm
	cmp	dh, 10
	jne	N8
	mov	dh, 8			;* change 8x10 font to 8x8
N8:
	mov	wo ds:[di].dxCharInft,dx
					;* move both dxChar and dyChar
	mov	dl,cs:[si].dyBaseDm
	mov	ds:[di].dyBaseLineInft,dl

	mov	ds:[di].SEG_lpbFontLower128Inft,CS
	mov	ds:[di].SEG_lpbFontUpper128Inft,CS
	cmp	ds:[di].dxCharInft,9
	jne	N9
	;* 9x14
	mov	ds:[di].OFF_lpbFontLower128Inft,drvOffset rgbHercFont9x14
	mov	ds:[di].OFF_lpbFontUpper128Inft,drvOffset rgbHercExFont9x14
	jmp	short FillInst

N9:
	cmp	ds:[di].dyCharInft,14
	jne	N14
	;* 8x14
	mov	ds:[di].OFF_lpbFontLower128Inft,drvOffset rgbHercFont8x14
	mov	ds:[di].OFF_lpbFontUpper128Inft,drvOffset rgbHercExFont8x14
	jmp	short FillInst

N14:
	mov	ds:[di].OFF_lpbFontLower128Inft,0FA6Eh
	mov	ds:[di].SEG_lpbFontLower128Inft,0F000h
	cmp	ds:[di].dyCharInft,8
	jne	ExitFQueryInft		;Failure
	;* 8x8

	mov	ds:[di].OFF_lpbFontUpper128Inft,drvOffset rgbHercExFont8x8

FillInst:
	mov	ax,sp			;Success

ExitFQueryInft:
cEnd	FQueryInftCsd


;********** TermCsd **********
;*	* CSD entry point (see documentation for interface)
;*	* normally a no-op

cProc	TermCsd, <FAR, PUBLIC, ATOMIC>, <di,si>
cBegin	TermCsd

;* resume text mode 80x25
	xor	ah,ah			; update Bios data
	mov	al,40h
	mov	es,ax
	mov	di,49h			;CRT_MODE
	mov	byte ptr es:[di],7
	mov	di,4Ah			;CRT_COLS
	mov	word ptr es:[di],80	
	mov	di,84h			;CRT_ROWS
	mov	byte ptr es:[di],24	

	mov	si,drvOffset rgbInit	;default 80x25 (9x14 font)
	mov	ax,fHercTxt
	mov	dx,hgcControl		; set to graphics or text mode...
	out	dx,al			;   ... with video off

	mov	dx,hgcIndex		; initialize 6845 chip with table
	mov	cx,cRegInit		;
	xor	ah,ah			;
	cld				;
TermLp:
	mov	al,ah			;
	out	dx,al			; send register number to index port
	inc	dx			;
	lods	byte ptr cs:[si]	; get byte of data to send
	out	dx,al			; and send it to index+1
	jmp	$ + 2			; WARNING -- THIS DELAY IS REQUIRED
	jmp	$ + 2			; FOR 8MHz ATs!
	inc	ah			;
	dec	dx			;
	loop	TermLp			; loop until all 16 bytes are output

;select ROM character generator
	MOV	AX,(0) OR xmodereg
	OUT	DX,AX

;set underscore at 14
	MOV	AX,(13 SHL 8) OR scorereg
	OUT	DX,AX

;resume monochrome
	mov	ax,(20h SHL 8) OR exceptreg	;* reg 17
	out	dx,ax

	mov	ax,fHercTxt
	or	al,fHercVideo		;turn video on
	mov	dx,hgcControl		;
	out	dx,al			;

	mov	ah,1			;set cursor shape
	mov	cx,0C0Dh		
	int	10h

cEnd	TermCsd

;*****************************************************************************

	include	Herc9x14.inc
	include	Herc8x10.inc
	include	Herc8x14.inc
	include	Herc8x8.inc

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