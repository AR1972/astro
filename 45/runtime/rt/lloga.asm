	TITLE		LLOGA Olivetti CGA graphics mode support
;***
;LLOGA - Olivetti CGA graphics mode support
;
;	Copyright <C> 1988, Microsoft Corporation
;
;Purpose:
;	Support for Olivetti Color Graphics Adapter (OGA) graphics
;	screen mode (BIOS 40h).
;
;	This module sets hooks in the mode-independent
;	modules to routines here for mode-dependent
;	graphics support.  See the mode-independent
;	modules for more precise descriptions of the
;	purposes and interfaces of these routines.
;
;	The following table summarizes the information for
;	the modes and configurations covered:
;
;    C		  |	A			     B
;    O		  |  B	T			     I
;    L		  |  I	T		 P	     T
;    O	     A	M |  O	R		 A    C      S
;  S R	     D	O |  S	I    C		 G    H      / P
;  C B	     A	N |	B    O		 E    A   P  P L
;  R U	C  R P	I |  M	U    L	 X   Y	 S    R   A  I A
;  E R	O  O T	T |  O	T    O	 R   R	 I    B   G  X N
;  E S	L  W E	O |  D	E    R	 E   E	 Z    O   E  E E
;  N T	S  S R	R |  E	S    S	 S   S	 E    X   S  L S
; -- - -- -- - -- | -- --- ---- --- --- --- ---- --- - -
;  4 0 80 25 O	C | 40	2   16  640 400  32 8x16  1  1 1
;
;   This mode uses 32K starting at B8000h and is organized in four 8K banks.
;   It is identical to CGA 640x200 mode except that the bank is determined
;   by looking at the least-significant 2 bits of Y:
;   0  -   bank 0  at B8000    (same as CGA)
;   1  -   bank 1  at BA000    (same as CGA)
;   2  -   bank 2  at BC000
;   3  -   bank 3  at BE000
;   The formula to map Y to the correct line in each bank is:
;   BASE = B8000h + (Y AND 3) * 2000h
;   Y = Y AND NOT 3
;   ADDR = BASE + Y * 20
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_DATA
	USESEG	_BSS
	USESEG	GR_TEXT
	USESEG	XIB
	USESEG	XI
	USESEG	XIE

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE llgrp.inc	;Constant definitions
	INCLUDE idmac.inc
	INCLUDE grmac.inc	;ModeData macros

	INITIALIZER B$xINITOGA ;Put B$xINITOGA in initializer list

sBegin	_BSS
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$Adapter
externB	b$MaxColor
externW b$UpSub
externW b$DnSub
externW b$UpDnAdd
externW b$ScreenTab
externW	b$InsCsrTyp	
externB b$ActPage
externW	b$pChkOlivetti
externW	b$pOgaCsr
sEnd	_BSS

assumes CS,GR_TEXT
sBegin	GR_TEXT

	staticW OGAALTTAB,22,1	;alternate master table
	staticD	,?,3
OGAALTCSR DD OGACSRTAB		; ptr to OGA cursor table
	staticB OGACSRTAB,0,8	;8x16 half cursor plus
	staticB ,255,24		; 8x16 solid cursor

externNP B$SCNIO		;used in SCNIOS macro

externNP B$GetParm
externNP B$MapXYC2_4		; shared with CGA code
externNP B$CgaSetPages		; shared with CGA code
externNP B$InitModeData
externNP B$ErrorReturn
externNP B$CgaSetAttr
externNP B$CgaLeftC1
externNP B$CgaChkUpC
externNP B$CgaUpC
externNP B$CgaChkDownC
externNP B$CgaDownC
externNP B$CgaReadC
externNP B$CgaSetC
externNP B$CgaSetPixC
externNP B$CgaSetPixFirstC
externNP B$CgaSetPixLastC
externNP B$CgaPutAction
externNP B$CgaNReadL
externNP B$CgaNWriteL
externNP B$CgaNSetC
externNP B$CgaSetTile
externNP B$CgaPaintBound
externNP B$CgaScanL
externNP B$CgaScanR
externNP B$CgaLineX
externNP B$CgaLineY
externNP B$CgaLineV
externNP b$Mode2Palette

;===========================================================================
mModeData	Mode4Data
;
; SCREEN 4, BIOS mode 40h
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	4
mBiosMode	40h
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	400
mVideoBase	0B800H
mMaxAttr	1
mMaxColor	15		
mPageSize	32			;page size in K
mCurrPSize	<(32*1024) shr 4>	;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	1
mBackColor	0
mEgaWrMd	0
mInitPalette	b$Mode2Palette
mInitVgaPal	b$Mode2VgaPal
mAlphaDim	AlphaDim4
mSetMode	SetMode4
mSetPages	B$CgaSetPages
mPalReset	B$ErrorReturn
mPalPut 	B$ErrorReturn
mPalTrans	B$ErrorReturn
mPalSet 	B$ErrorReturn
mSetColor	SetColor4
mForeMapped	1
mBitsPerPixel	1
mPlanes 	1
mMapXYC 	MapXYC4
mLeftC		B$CgaLeftC1
mChkUpC 	B$CgaChkUpC
mUpC		B$CgaUpC
mChkDownC	B$CgaChkDownC
mDownC		B$CgaDownC
mSetAttr	B$CgaSetAttr
mReadC		B$CgaReadC
mSetC		B$CgaSetC
mSetPixC	B$CgaSetPixC
mSetPixFirstC	B$CgaSetPixFirstC
mSetPixLastC	B$CgaSetPixLastC
mLineX		B$CgaLineX
mLineY		B$CgaLineY
mLineV		B$CgaLineV
mPutAction	B$CgaPutAction
mNReadL 	B$CgaNReadL
mNWriteL	B$CgaNWriteL
mNSetC		B$CgaNSetC
mPaintBound	B$CgaPaintBound
mSetTile	B$CgaSetTile
mScanL		B$CgaScanL
mScanR		B$CgaScanR
mEnd		GraphDataLen
;===========================================================================

labelNP <PUBLIC, B$OLIUSED>

;*** 
; B$Screen4
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 4.
;Entry:
;	AL = screen mode (4)
;	AH = burst (N/A)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen4,<PUBLIC,NEAR>
cBegin
	test	[b$Adapter],OGA	;check for adapter support
	stc
	jz	ScrExit 	;exit w/error if not supported
	mov	bx,GR_TEXTOFFSET Mode4Data  ;mode-specific data
	mov	cx,GraphDataLen
	call	B$InitModeData	;initialize table data
	mov	b$UpSub,8192	; Add 8K to move up one line
	mov	b$DnSub,24576-80 ; Add 24K and subtract line length to go down
	mov	b$UpDnAdd,8192+24576-80 ; Sum of the two offsets
	clc			;indicate no error
ScrExit:
cEnd

;***
; AlphaDim4
;
;Purpose:
;	Validate the proposed text dimensions for Screen 4.
;	  If 80x25 is requested, this mode satisfies the request
;	  else suggest screen mode 0.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	AL = -1 if this mode satisfies the request, otherwise
;		AL is suggested screen mode to invoke for desired dimensions
;	PSW.C clear
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	AlphaDim4
cProc	AlphaDim4,<NEAR>
cBegin
	xor	al,al		;try Screen 0 if not 80x25
	cmp	bx,80+25*256	;80x25?
	jne	ADim4Exit	;brif not, try text mode
	dec	al		;al = -1 indicates dimensions are supported
ADim4Exit:
	clc			;no error
cEnd

;***
; SetMode4
;
;Purpose:
;	Set the screen mode according to the characteristics established
;	by previous call to B$Screenx and b$AlphaDim.
;Entry:
;
;Exit:
;
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	SetMode4
cProc	SetMode4,<NEAR>
cBegin
	mov	al,40h	;set BIOS mode 40h
	SCNIOS	vSetMode
cEnd

;*** 
;SetColor4 - Set the foreground color for Screen 4
;
;Purpose:
;	Process the COLOR statement for BIOS mode 40h.  Syntax is:
;
;	COLOR <foreground color>
;
;	where <foreground color> is from 0-15.
;
;Entry:
;	parameter list.
;
;Exit:
;	PSW.C set if error.
;
;Uses:
;	Per convention.
;
;******************************************************************************
cProc	SetColor4,<NEAR>
cBegin
	cCall	B$GetParm	; get the foreground color
	jz	SetColDone	; done if no parameters
	xor	bx,bx		; bh = 0 (used below)
	cmp	bx,cx		; make sure no more parameters
	jc	SetColDone	; brif illegal syntax
	cmp	[b$MaxColor],al ; make sure 0-15
	jc	SetColDone	; error if al > b$MaxColor
	mov	bl,al		; bh=0; bl = foreground color
	SCNIOS	vSetPalette
	clc			; indicate no error
SetColDone:
cEnd

;***
; MapXYC4
;
;Purpose:
;	May given X and Y coordinates to the graphics cursor for Screen 4.
;Entry:
;	CX = X coordinate
;	DX = Y coordinate
;Exit:
;	b$OffC, b$MaskC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	MapXYC4
cProc	MapXYC4,<NEAR>
cBegin

	mov	ax,3
	and	ax,dx
	xor	dx,ax		; AND DX, NOT 3
	ror	ax,1		;ax=8K multiple for quadrant displacement
	ror	ax,1
	shr	ax,1
	mov	bx,dx		;multiply y by 20 to compute row displacement
	shl	dx,1		;dx=2*Y
	shl	dx,1		;dx=4*Y
	add	dx,bx		;dx=5*Y
	jmp	B$MapXYC2_4	;use common code from Screen mode 2
cEnd	<nogen>

;*** 
;B$ChkOlivetti - Check if Olivetti 640x400 graphics mode is supported
;
;Purpose:
;	See if this is an Olivetti version of CGA, EGA, or VGA which supports
;	their 640x400 color graphics mode. 
;
;Entry:
;	Adapter = CGA or EGA or VGA
;
;Exit:
;	If mode is supported, PSW.Z = 0, AL = OGA.
;	If mode is not supported, PSW.Z = 1, AL = 0.
;
;Uses:
;	AX.
;
;Preserves:
;	DS.
;
;******************************************************************************
cProc	B$ChkOlivetti,<PUBLIC,NEAR>,<DS>
cBegin
	mov	ax,0FC00h	; BIOS ROM
	mov	ds,ax		; look for "OLIVETTI"
	cmp	word ptr DS:[50h],"LO" ; DS:50h  = "OL"?  (MASM reverses chars!)
	jne	NotOGA
	cmp	word ptr DS:[52h],"VI" ; DS:52h  = "IV"?
	jne	NotOGA

	mov	ah,0F0h		; This is an Olivetti machine
	mov	ds,ax		; DS = F000h -- SYSTEM ROM
	mov	ax,DS:[0FFFDh]	; DS:FFFDh -- Exclude M15 and M19 models:
	cmp	ax,0FF46h	; Is it M15?
	je	NotOGA		; brif - done
	cmp	ax,0FE00h	; Is it M19?
	je	NotOGA		; brif - done
	mov	ax,0C000h	; look for 2nd graphics card installed
	mov	ds,ax
	cmp	word ptr DS:[0],0AA55h ; DS:0 -- check for option ROM
	jne	GotOGA		; brif no option ROM -- just OGA

; See if option card supports the Olivetti mode
	cmp	word ptr DS:[3Ch],"AP" ; = "PA" ? -- check for PARADISE card
	je	GotOGA		; brif found - use OGA
	cmp	word ptr DS:[10h],"LO" ; DS:10h = "OL" -- Olivetti EGA/VGA card?
	jne	NotOGA		; brif a non-Olivetti card is installed
	mov	ax,DS:[22h]	; DS:22h -- make sure EGA or VGA
	cmp	ax,"GV"		; = "VG" -- Is if Olivetti VGA?
	je	GotOGA		; brif -- use OGA
	cmp	ax,"GE"		; = "EG" -- Is if Olivetti EGA?
	jne	NotOGA		; brif not -- non-Olivetti graphics card

; Olivetti EGA Card 2
	xor	ax,ax		; Must have OEC2 card -- check switches
	mov	ds,ax		
	mov	ax,DS:[488h]	; make sure OEC2 640x400 mode enabled
	and	ax,0A0h		; mask out irrelevent bits
	cmp	ax,0A0h		; If bits 7 and 5 are set, no 640x400 mode
	je	NotOGA		; brif mode not enabled

GotOGA:				; use OGA screen mode!
	mov	al,OGA-1
	inc	al		; al = OGA; PSW.Z = 0
	jmp	short RetValue
NotOGA:
	xor	al,al		; default return value
RetValue:
cEnd

;*** 
; OgaCsr -- Toggle a generated graphics mode text cursor (DOS 3 only)
;
;Purpose:
;	Mode-specific portion of GRPCSR from llscnio.asm.
;
;Entry:
;	AX = cursor type
;	CX = ES = 0
;
;Exit:
;	None
;Uses:
;	AX,BX,CX
;
;******************************************************************************

cProc	OgaCsr,<NEAR>		; version of GRPCSR for Olivetti graphics mode
cBegin
	mov	bx,cs
	xchg	bx,es:[486h]	; segment of alternate table
	push	bx		; save original master table pointer segment

	mov	bx,GR_TEXTOFFSET OGAALTTAB ; offset of alternate table
	xchg	bx,es:[484h]	; change master table pointer temporarily
	push	bx		; save original master table pointer offset

	CMP	AX,[b$InsCsrTyp] ; is it an insert cursor?
	MOV	AL,128		; Char code for Box
	JZ	OGACS1		; brif it is insert cursor
	INC	AL		; Map to 128 or 129..
OGACS1: 			; Half Box (128) If INS_MODE
				; Whole Box (129) for normal (NOT INS_MODE)
	MOV	BL,81H		; Select Invert (xor) Mode
				; bit 7 of [BL] = 1 for XOR mode and
				; low nibble =1, foreground attribute
	MOV	BH,[b$ActPage]	; Active Page
	INC	CX		; CX = 1 -- One Character
	SCNIOS	vWriteChar

	pop	word ptr es:[484h] ; restore original master table pointer
	pop	word ptr es:[486h]
cEnd

;***
; B$xINITOGA - initialize OGA 640 x 400 mode.
;
;Purpose:
;	Put the address of Olivetti screen mode support routine into the
;	dispatch table used by the screen statement.
;
;Entry:
;	None
;Exit:
;	ScreenTab updated
;Uses:
;	None
;Exceptions:
;******************************************************************************
cProc	B$xINITOGA,<FAR,PUBLIC>
cBegin
	MOV	WORD PTR [b$ScreenTab + (4*2) + 1],OFFSET B$Screen4
	MOV	[b$pChkOlivetti], GR_TEXTOFFSET B$ChkOlivetti
	MOV	[b$pOgaCsr], GR_TEXTOFFSET OgaCsr
cEnd

sEnd	GR_TEXT

	END
