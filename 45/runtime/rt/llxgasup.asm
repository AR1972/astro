	TITLE	LLXGASUP - LowLevel graphic support shared by all LL?GA modules
;***
; LLXGASUP - LowLevel graphic support shared by all LL?GA modules
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	This module contains support routines which are shared by CGA, EGA
;	VGA, and TEXT functions.
;
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_BSS
	USESEG	GR_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE llgrp.inc	; constant definitions

sBegin	_BSS
;
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$EgaPalSup
externD b$InitPalette
externB b$MaxAttr
externB b$MaxColor
externB b$Monitor
externB b$BorderColor		;border color (overscan)
;
; ***************************************************************************
; Eternal function vectors
; ***************************************************************************
;
externW b$PalTrans
externW b$PalPut
;
; ***************************************************************************
; Local variables
; ***************************************************************************
;
staticB b$EgaPalette,,17	    

sEnd	_BSS

assumes CS,GR_TEXT

sBegin	GR_TEXT
;
; b$ColorPalette - used to initialize the EGA palette for 16-color
;		    modes with a std Color Display.
;		    Moved from LLTEXT and LLEGA with [5].
;
labelB	<PUBLIC,b$ColorPalette>
	;	 I RGB
	DB	000000B 	;black
	DB	000001B 	;blue
	DB	000010B 	;green
	DB	000011B 	;cyan
	DB	000100B 	;red
	DB	000101B 	;magenta
	DB	000110B 	;brown
	DB	000111B 	;white
	DB	010000B 	;gray
	DB	010001B 	;light blue
	DB	010010B 	;light green
	DB	010011B 	;light cyan
	DB	010100B 	;light red
	DB	010101B 	;light magenta
	DB	010110B 	;light yellow
	DB	010111B 	;bright white
;
; b$EnhPalette - used to initialize the EGA palette for 16-color
;		  modes with an Enhanced Color Display.
;		  Moved from LLTEXT and LLEGA with [5].
;
labelB	<PUBLIC,b$EnhPalette>
	;	RGBrgb
	DB	000000B 	;black
	DB	000001B 	;blue
	DB	000010B 	;green
	DB	000011B 	;cyan
	DB	000100B 	;red
	DB	000101B 	;magenta
	DB	010100B 	;brown
	DB	000111B 	;white
	DB	111000B 	;gray
	DB	111001B 	;light blue
	DB	111010B 	;light green
	DB	111011B 	;light cyan
	DB	111100B 	;light red
	DB	111101B 	;light magenta
	DB	111110B 	;light yellow
	DB	111111B 	;bright white


;***
; B$EgaPalReset
;
;Purpose:
;	Reset the PALETTE to the initial, default colors.
;
;	This routine is used by many modes which use the EGA palette
;	to fill the b$PalReset function vector.
;Entry:
;Exit:
;Uses:
;Exceptions:
;	exit thru B$VgaPalReset for 256K palette in QCG
;******************************************************************************

cProc	B$EgaPalReset,<PUBLIC,NEAR>
cBegin
labelNP <PUBLIC,B$EgaPalResetB>;entry for mono modes not using VGA palette
	cmp	b$EgaPalSup,0	;support for EGA PALETTE?
	jz	PalResX 	;go if not
	mov	al,b$BorderColor;prepare border color for overscan reg.
	push	si		
	push	di		
	push	es		
	push	ds		
	push	ds
	pop	es
	mov	di,OFFSET DGROUP:b$EgaPalette ;destination for palette
	push	di
	lds	si,b$InitPalette   ;ptr to palette initializer
	mov	cx,16
    rep movsb			;copy palette
	stosb			;set overscan to border color
	pop	dx
	pop	ds		
	MOV	AL,2		;BIOS sub-function - set all palette registers
				;and the overscan register (for border color)
	SCNIO	vSetEgaPalette	;set real palette
	pop	es		
	pop	di		
	pop	si		
PalResX:
cEnd

;***
; B$EgaPalPut
;
;Purpose:
;	Change palette entry (with translation/verification done by
;	b$PalTrans.)
;	A color value of negative one indicates that the associated
;	palette entry is not to be modified.
;
;	This routine is used by many modes which use the EGA palette
;	to fill the b$PalPut function vector.
;
;Entry:
;	DX:AX = color
;	BL    = attribute
;Exit:
;	PSW.C reset indicates successful operation
;		set indicates PALETTE function call error
;Uses:
;	per conv.
;Exceptions:
;	exit thru B$VgaPalPut for 256K palette in QCG
;******************************************************************************
cProc	B$EgaPalPut,<PUBLIC,NEAR>
cBegin
labelNP <PUBLIC,B$EgaPalPutB>	;entry for mono modes not using VGA palette
	cmp	b$EgaPalSup,0	;support for EGA PALETTE?
	jz	PalPutErr	;exit if not w/error
	cmp	ax,-1		;lo word of color == -1?
	jne	PalPut1 	;go if not, can't ignore
	cmp	dx,ax		;hi word too?
	je	PalPutExit	;exit if color == -1
PalPut1:			
	call	[b$PalTrans]	;translate to external form
	jc	PalPutErr	;exit if invalid w/error
	XCHG	BH,AL		;BH:BL = color:attribute
	MOV	AL,0		;subfunction "Set Individual Palette Register"
	SCNIO	vSetEgaPalette	;set background color
	clc			;no error
	ret			
PalPutErr:
	stc
PalPutExit:
cEnd


;***
; B$EgaPalTrans
;
;Purpose:
;	Verify and translate an internal palette attribute
;	and color into the actual palette attribute and color.
;
;	This routine is used by many modes which use the EGA palette
;	to fill the b$PalTrans function vector.
;
;	This particular PalTrans routine verifies the Attribute
;	as being in the range from 0 to b$MaxAttr with no translation.
;	The Color range is also verified as 0 to b$MaxColor.  If a
;	standard Color Monitor is being driven, the Color value is
;	translated so that bit 3 of the Color drives the monitor's
;	intensity signal.  In modes supporting the std color monitor,
;	Color values range from 0 to 15, and this translation causes
;	the upper 8 colors to be intensified versions of the lower 8
;	for the EGA palette.
;
;Entry:
;	DX:AX = internal color
;	BL    = internal attribute
;Exit:
;	PSW.C reset if entry OK
;	DX:AX = actual color
;	BL    = actual attribute
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPalTrans,<PUBLIC,NEAR>
cBegin
	cmp	bl,b$MaxAttr	;is legal attribute ?
	ja	PalPutErr	;error return
	or	dh,dl		;hi 3 bytes of color must be 0
	or	dh,ah		
	jnz	PalPutErr	;error if not
	cmp	al,b$MaxColor	;is legal color ?
	ja	PalPutErr	;error return
	cmp	b$MaxColor,15	;16 color mode?
	jne	PalTrExit	;go if not
	MOV	AH,8		;adjust to 4-bit color
	AND	AH,AL		;get the intensified bit (bit 3)
	SHL	AH,1		;slide to bit 4
	OR	AL,AH		;put it in bit 4
	xor	ah,ah		; clear AH:
PalTrExit:
	clc			;no error
cEnd

;***
; B$EgaPalSet
;
;Purpose:
;	Set the entire palette from an array where an element value
;	of -1 indicates the entry should be left alone.
;
;	This routine is used by many modes which use the EGA or VGA palette
;	to fill the b$PalSet function vector.
;
;	This particular routine uses the mode-dependent function vector
;	b$PalTrans to verify all entries before any are set using
;	b$PalPut.  Thus mode and even palette (EGA or VGA) independence
;	is maintained.
;	Special consideration is made for the number of bytes per
;	element in the array.  And that enough elements exist to fill
;	the palette.
;
;Entry:
;	AX    = size of the array in elements
;	ES:SI = address of the array
;	CX    = size of each array element in bytes (2 or 4) (unused if QCG)
;Exit:
;	PSW.C set indicates function call error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$EgaPalSet,<PUBLIC,NEAR>,<DI>
cBegin
	cld			
	mov	di,cx		;save element size
	MOV	CL,b$MaxAttr	;max attribute
	inc	cx		;number of attributes in array
	CMP	AX,CX		;elements in array vs required size
	JB	PalSetErr	;go if not big enough
CHKETY: 			;check each entry
	lods	word ptr es:[si];get lo (only?) word
	cwd			;dx:ax = I2 color
	cmp	di,4		;I4 element?
	jne	GotColor	;go if not
	xchg	ax,dx		;save lo word
	lods	word ptr es:[si];get hi word
	xchg	ax,dx		;dx:ax = I4 color
GotColor:			
	cmp	dx,-1		;hi word of color == -1?
	jne	CheckIt 	;go if not, can't ignore
	cmp	dx,ax		;lo word too?
	je	CHKNXT		;ok (ignore) if color == -1
CheckIt:			
	xor	bx,bx		;attr=0 (always valid) for PalTrans checks
	push	cx		;save loop count
	call	[b$PalTrans]	;use PalTrans to verify color value
	pop	cx		
	jc	PalSetErr	;exit if error
CHKNXT: 			;next entry
	LOOP	CHKETY

	MOV	CL,b$MaxAttr	;max attribute
	inc	cx		;number of attributes in array
	MOV	BX,CX
	DEC	SI
	DEC	SI		;now point to the last entry
	STD
EXCETY: 			;execute set up each palette entry
	DEC	bx		;BL=attribute number
	lods	word ptr es:[si];get hi (only?) word
	cwd			;dx:ax = I2 color
	cmp	di,4		;I4 element?
	jne	GotColor1	;go if not
	xchg	ax,dx		;save hi word
	lods	word ptr es:[si];get lo word, dx:ax = I4 color
GotColor1:			
	PUSH	BX
	push	cx
	CALL	[b$PalPut]	;set the palette register
	pop	cx
	pop	bx
	jc	PalSetErr	;go if error
	LOOP	EXCETY
	CLC			;indicate no error
	jmp	SHORT PalSetExit
PalSetErr:
	stc
PalSetExit:
	CLD
cEnd

sEnd	GR_TEXT

	END
