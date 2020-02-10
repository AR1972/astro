	TITLE		LLCGA - CGA screen mode support
;***
;LLCGA - CGA screen mode support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Support for CGA graphics screen modes (BIOS 4,5,6).
;	Note that this module module contains support
;	code for all adapters capable of handling these
;	screen modes and also attempts to compensate for
;	the subtle differences in their treatment.
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
;  1 0 40 25 C	x |  4	 4  N/A 320 200  16 8x8   1  2 1
;  1 0 40 25 E	x |  "   "  16	 "   "	  "  "	  "  " "
;  1 1 40 25 C	x |  5	 "  N/A  "   "    "  "    "  " "
;  1 x 40 25 I	x |  "   "   "   "   "    "  "    "  " "
;  1 1 40 25 E	x |  "   "  16	 "   "	  "  "	  "  " "
;  1 x 40 25 S	x |  "   "   "   "   "    "  "    "  " "
;
;  2 x 80 25 C	x |  6	 2  N/A 640 200  16 8x8   1  1 1
;  2 x 80 25 I	x |  "   "   "   "   "    "  "    "  " "
;  2 x 80 25 E	x |  "   "  16	 "   "	  "  "	  "  " "
;  2 x 80 25 S	x |  "   "   "   "   "    "  "    "  " "
;
; Discussion of CGA odd/even line archtecture:
;	CGA video memory is organized into two banks.  The first bank,
;	starting at B800, contains all the even scan lines.  The second
;	bank, starting at B800+8K, contains all the odd scan lines:
;
;	Scan	Offset
;	Line	from B800
;	----	---------
;	0	0
;	1	8K
;	2	80
;	3	8K+80
;	.	.
;	.	.
;	.	.
;
;	To move UP   from odd  scan line to even, add -8K    to current address
;	To move UP   from even scan line to odd,  add +8K-80 to current address
;	To move DOWN from odd  scan line to even, add -8K+80 to current address
;	To move DOWN from even scan line to odd,  add +8K    to current address
;
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_DATA
	USESEG	_BSS
	USESEG	GR_TEXT
	USESEG	CN_TEXT
	USESEG	XIB		
	USESEG	XI		
	USESEG	XIE		

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE llgrp.inc	; Constant definitions
	INCLUDE idmac.inc
	INCLUDE grmac.inc	;ModeData macros

	INITIALIZER B$xINITCGA	;Put B$xINITCGA in initializer list


sBegin	_BSS
;
; ***************************************************************************
; Local variables
; ***************************************************************************
;
staticB BackColor,,1		;save background color for COLOR stmt
				;__bBackColor is actually an attribute,
				;and should stay 0 in graphics modes
;
; ***************************************************************************
; External function vectors
; ***************************************************************************
;
externW b$PalPut
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externW b$CurPages		; current active and visual page
externB b$BiosMode
externB b$Adapter
externB b$Monitor
externW b$ModeBurst
externB b$ScreenMode
externW b$VideoBase
externB b$MaskC
externB b$AttrC
externW b$OffC
externW b$SegC
externB b$MaxAttr
externB b$EgaPalSup		
externW B$VTOFST
externW B$VBOFST
externW B$VLOFST
externW B$VROFST
externW B$LEOFST
externW B$REOFST
externB b$NullColor
externB b$BitsPerPixel
externW b$BytesPerRow

externW b$UpSub 		
externW b$DnSub 		
externW b$UpDnAdd		
externW b$ScreenTab		
sEnd	_BSS

assumes CS,GR_TEXT
sBegin	GR_TEXT

externNP B$SCNIO 		;used in SCNIO macro

externNP B$InitModeData
externNP B$GetParm
externNP B$EgaPalReset
externNP B$EgaPalPut
externNP B$EgaPalTrans
externNP B$EgaPalSet
externNP B$ErrorReturn
externNP B$CgaSetAttr
externNP B$CgaLeftC1		
externNP B$CgaLeftC2		
externNP B$CgaChkUpC		
externNP B$CgaUpC		
externNP B$CgaChkDownC		
externNP B$CgaDownC		
externNP B$CgaPaintBound	
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
externNP B$CgaScanL
externNP B$CgaScanR
externNP B$CgaLineX
externNP B$CgaLineY
externNP B$CgaLineV


;===========================================================================
mModeData	Mode1Data
;
; SCREEN 1, BIOS modes 4 & 5
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	1
mBiosMode	4		;BIOS mode and burst may be adjusted later
mBurst		0
mScrWidth	40
mScrHeight	25
mHorzRes	320
mVertRes	200
mVideoBase	0B800H
mMaxAttr	3
mMaxColor	15
mPageSize	16		    ;page size in K
mCurrPSize	<(16*1024) shr 4>   ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	3
mBackColor	0
mEgaWrMd	0
mInitPalette	Mode1Palette
mInitVgaPal	Mode1VgaPal
mAlphaDim	AlphaDim1
mSetMode	SetMode
mSetPages	B$CgaSetPages		
mPalReset	B$EgaPalReset
mPalPut 	PalPut1
mPalTrans	B$EgaPalTrans
mPalSet 	B$EgaPalSet
mSetColor	SetColor1
mForeMapped	3
mBitsPerPixel	2
mPlanes 	1
mMapXYC 	MapXYC1
mLeftC		B$CgaLeftC2	
mChkUpC 	B$CgaChkUpC	
mUpC		B$CgaUpC	
mChkDownC	B$CgaChkDownC	
mDownC		B$CgaDownC	
mSetAttr	SetAttr1
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

;===========================================================================
mModeData	Mode2Data
;
; SCREEN 2, BIOS mode 6
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	2
mBiosMode	6
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	200
mVideoBase	0B800H
mMaxAttr	1
mMaxColor	15
mPageSize	16		    ;page size in K
mCurrPSize	<(16*1024) shr 4>   ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	1
mBackColor	0
mEgaWrMd	0
mInitPalette	b$Mode2Palette		
mInitVgaPal	b$Mode2VgaPal		
mAlphaDim	AlphaDim2
mSetMode	SetMode
mSetPages	B$CgaSetPages		
mPalReset	B$EgaPalReset
mPalPut 	B$EgaPalPut
mPalTrans	B$EgaPalTrans
mPalSet 	B$EgaPalSet
mSetColor	B$ErrorReturn
mForeMapped	1
mBitsPerPixel	1
mPlanes 	1
mMapXYC 	MapXYC2
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

;
; Mode1Palette - used to initialize the EGA palette for SCREEN 1
;		 (BIOS mode 4 or 5).
;
labelB	Mode1Palette
	;	RGBrgb
	DB	000000B 	;black
	DB	111011B 	;bright cyan
	DB	111101B 	;bright magenta
	DB	111111B 	;bright white
	;DB	    12 DUP (0)
;
; Mode2Palette - used to initialize the EGA palette for SCREEN 2
;		 (BIOS mode 6).
;
PUBLIC	b$Mode2Palette		
labelB	b$Mode2Palette		
	;	RGBrgb
	DB	000000B 	;black
	DB	111111B 	;bright white
	;DB	    14 DUP (0)



labelNP <PUBLIC, B$CGAUSED>	


;*** 
; B$Screen1
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 1.
;Entry:
;	AL = screen mode (1)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen1,<PUBLIC,NEAR>
cBegin
	test	b$Adapter,VGA + MCGA + EGA + CGA ;check for adapter support
	je	ScrErr		;exit w/error if not supported
	test	b$Monitor,AnalogColor + EnhColor + StdColor
				;check for monitor support
	je	ScrErr		;exit w/error if not supported
	mov	al,ah		;new BIOS mode = 4 if no burst
	add	al,4		;		 5 if burst
	mov	bx,GR_TEXTOFFSET Mode1Data  ;mode-specific data
	mov	cx,GraphDataLen 
	push	ax
	call	B$InitModeData ;initialize table data
	pop	ax
	mov	b$ModeBurst,ax ;save new mode and burst
	jmp	short ScrCommon ;common exit
ScrErr:
	stc
ScrExit:
JustReturn:
cEnd

;*** 
; B$Screen2
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 2.
;Entry:
;	AL = screen mode (2)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;	can exit through B$Screen1's error exit.
;******************************************************************************
cProc	B$Screen2,<PUBLIC,NEAR>
cBegin
	test	b$Adapter,VGA + MCGA + EGA + CGA ;check for adapter support
	je	ScrErr		;exit w/error if not supported
	test	b$Monitor,AnalogColor + EnhColor + StdColor
				;check for monitor support
	je	ScrErr		;exit w/error if not supported
	mov	bx,GR_TEXTOFFSET Mode2Data  ;mode-specific data
	mov	cx,GraphDataLen 
	call	B$InitModeData ;initialize table data
ScrCommon:				
	mov	b$UpSub,8192		
	mov	b$DnSub,8192-80 	
	mov	b$UpDnAdd,8192-80+8192	
	clc			;indicate no error
cEnd

;***
; AlphaDim1
;
;Purpose:
;	Validate the proposed text dimensions for Screen 1.
;	  If 40x25 is requested, this mode satisfies the request
;	  elseif 80x25 is requested, suggest screen mode 2
;	  else suggest screen mode 0.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	AL = -1 if this mode satisfies the request, otherwise
;		AL is suggested screen mode to invoke for desired dimensions
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	AlphaDim1
cProc	AlphaDim1,<NEAR>
cBegin
	mov	al,-1		;flag request satisfied (maybe)
	cmp	bx,40+25*256	;40x25?
	je	ADim1Exit	;exit if so, standard stuff
	xor	al,al		;flag request for screen 0
	cmp	bx,80+25*256	;80x25?
	jne	ADim1Exit	;if not, let text mode try
	mov	al,2		;otherwise use screen 2
ADim1Exit:
	clc			;no error
cEnd

;***
; AlphaDim2
;
;Purpose:
;	Validate the proposed text dimensions for Screen 2.
;	  If 80x25 is requested, this mode satisfies the request
;	  elseif 40x25 is requested, suggest screen mode 1
;	  else suggest screen mode 0.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	AL = -1 if this mode satisfies the request, otherwise
;		AL is suggested screen mode to invoke for desired dimensions
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	AlphaDim2
cProc	AlphaDim2,<NEAR>
cBegin
	mov	al,-1		;flag request satisfied (maybe)
	cmp	bx,80+25*256	;80x25?
	je	ADim1Exit	;exit if so, standard stuff
	xor	al,al		;flag request for screen 0
	cmp	bx,40+25*256	;40x25?
	jne	ADim2Exit	;if not, let text mode try
	inc	al		;otherwise use screen 1
ADim2Exit:
	clc			;no error
cEnd

;***
; SetMode
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
DbPub	SetMode
cProc	SetMode,<NEAR>
cBegin
	mov	al,b$BiosMode	;set BIOS mode
	SCNIOS	vSetMode	
	cmp	b$ScreenMode,1 ;remainder for SCREEN 1 only
	jne	SetModeExit	;exit if not
	test	b$Adapter,EGA+VGA  ;EGA or VGA?
	jz	SetModeExit	;go if not
;
;	The following is provided only to overcome a bug in the EGA BIOS
;	routines which support the graphics "compatibility mode" (BIOS 4)
;	so that the two calls related to PALETTE (INT10 AH = 0BH and
;	INT10H AH = 10H) work correctly.  If we use the first call once
;	when user invokes SCREEN 1 to set the background color, then the
;	BIOS will subsequently reference the correct (low-intensity)
;	color values for the 4 palette attributes whenever the call using
;	INT10H, AH = 0BH is used to toggle the palette, and whenever the
;	call INT10H, AH = 10H is used to set an individual palette regis-
;	ter.  In the absence of this initialization, the high-intensity
;	color values for both palettes are referenced.
;
	xor	bx,bx		;set background to 0
	SCNIOS	vSetPalette	;INT10H, AH=0BH "set color palette"
SetModeExit:
cEnd

;***
; B$CgaSetPages
;
;Purpose:
;	Set the current active and visual pages and calculate page size
;	and video segment offset.
;Entry:
;	AL = active page (will always be 0 for these modes)
;	AH = visual page (will always be 0 for these modes)
;Exit:
;	b$CurPages set to new active and visual pages
;	b$SegC set to video segment
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
PUBLIC	B$CgaSetPages		
cProc	B$CgaSetPages,<NEAR>	
cBegin
DbAssertRel	AX,E,0,GR_TEXT,<Non-zero page requested in B$CgaSetPages (LLCGA)>
	mov	[b$CurPages],ax	; save page numbers
	mov	ax,[b$VideoBase] ;set video segment
	mov	[b$SegC],ax
cEnd

;***
; PalPut1
;
;Purpose:
;	Change palette entry for Screen 1 with translation/verification.
;	A color value of -1 indicates that the associated palette
;	entry is not to be modified.
;Entry:
;	[DX:AX] = color
;	BL	= attribute
;Exit:
;	PSW.C reset indicates successful operation
;		set indicates PALETTE function call error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	PalPut1
cProc	PalPut1,<NEAR>
cBegin
	cmp	ax,-1		;lo word of color == -1?
	jne	PutPalA 	;go if not, can't ignore
	cmp	dx,ax		;hi word too?
	je	PutPalX 	;exit if color == -1
PutPalA:			
	push	ax
	push	dx		
	push	bx
	call	B$EgaPalPut	;put palette value
	pop	bx
	pop	dx		
	pop	ax
	jc	PutPalX 	;go if error
	or	bl,bl		;is for background?
	jnz	PutPalX 	;exit if not
	call	B$EgaPalTrans	;translate color value again!!
	cmp	b$EgaPalSup,0	;have we an EGA palette?
	je	PutPalX 	;exit if not
	mov	bh,al		;overscan color
	mov	al,1		;subfunction "Set Overscan (Border) Register"
	SCNIO	vSetEgaPalette	;set the border color identically
PutPalX:
cEnd


;***
; MapXYC1
;
;Purpose:
;	May given X and Y coordinates to the graphics cursor for Screen 1.
;Entry:
;	CX = X coordinate
;	DX = Y coordinate
;Exit:
;	b$OffC, b$MaskC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	MapXYC1
cProc	MapXYC1,<NEAR>
cBegin
	xor	ax,ax
	shr	dx,1		;dx=row within odd or even half, carry=1 if odd
	rcr	ax,1		;ax=8K if dx was odd, 0 if was even
	shr	ax,1		;  computing offset to proper buffer half
	shr	ax,1
	mov	bx,dx		;multiply y by 80 to compute row displacement
	shl	dx,1		;dx=2*Y
	shl	dx,1		;dx=4*Y
	add	dx,bx		;dx=5*Y
	shl	dx,1		;dx=10*Y
	shl	dx,1		;dx=20*Y
	shl	dx,1		;dx=40*Y
	shl	dx,1		;dx=80*Y
	add	dx,ax		;odd rasters are displaced 8k
	mov	ax,cx		;save x
	shr	ax,1		;div by PixelsPerByte (4)
	shr	ax,1		;  to get byte index
	add	dx,ax		;add x byte offset to y row address
	mov	b$OffC,dx	;save byte offset
	and	cl,3		;mask in x bit addr
	shl	cl,1		;  *2 for pixel addr in byte
	mov	ch,11000000B	;leftmost pixel on in shift mask
	shr	ch,cl		;move over to get mask
	mov	b$MaskC,ch	;store cursor mask
cEnd

;***
; MapXYC2
;
;Purpose:
;	May given X and Y coordinates to the graphics cursor for Screen 2.
;Entry:
;	CX = X coordinate
;	DX = Y coordinate
;Exit:
;	b$OffC, b$MaskC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	MapXYC2
cProc	MapXYC2,<NEAR>
cBegin
	xor	ax,ax
	shr	dx,1		;dx=row within odd or even half, carry=1 if odd
	rcr	ax,1		;ax=8K if dx was odd, 0 if was even
	shr	ax,1		;  computing offset to proper buffer half
	shr	ax,1
	mov	bx,dx		;multiply y by 80 to compute row displacement
	shl	dx,1		;dx=2*Y
	shl	dx,1		;dx=4*Y
	add	dx,bx		;dx=5*Y
	shl	dx,1		;dx=10*Y
	shl	dx,1		;dx=20*Y
labelNP	<PUBLIC,B$MapXYC2_4>	; common to MAPXYC4 routine (lloga.asm)
	shl	dx,1		;dx=40*Y
	shl	dx,1		;dx=80*Y
	add	dx,ax		;odd rasters are displaced 8k
	mov	ax,cx		;save x
	shr	ax,1		;div by PixelsPerByte (8)
	shr	ax,1		;  to get byte index
	shr	ax,1
	add	dx,ax		;add x byte offset to y row address
	mov	b$OffC,dx	;save byte offset
	and	cl,7		;mask in x bit addr
	mov	ch,10000000B	;leftmost pixel on in shift mask
	shr	ch,cl		;move over to get mask
	mov	b$MaskC,ch	;store cursor mask
cEnd

;***
; SetAttr1
;
;Purpose:
;	Replicate supplied attribute throughout the attribute byte for
;	use by Screen 1 functions.  If supplied attribute is beyond
;	legal range the maximum legal attribute is used.
;Entry:
;	AL = attribute
;Exit:
;	b$Attr updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	SetAttr1
cProc	SetAttr1,<NEAR>,<AX>
cBegin
	cmp	al,b$MaxAttr	;test against maximum attribute
	jbe	SetAttr1Ok	;Brif legal
	mov	al,b$MaxAttr	;limit to max
SetAttr1Ok:
	MOV	CL,2		;2 bits per pixel
	MOV	AH,AL		;attr mask in ??????xx
	SHL	AH,CL
	OR	AL,AH		;attr mask in ????xxxx
	SHL	AH,CL
	OR	AL,AH		;attr mask in ??xxxxxx
	SHL	AH,CL
	OR	AL,AH		;attr mask in xxxxxxxx
	MOV	b$AttrC,al	;exit no error
cEnd

;***
; SetColor1
;
;Purpose:
;	Process the color statement for Screen 1.  Syntax for Screen 1
;	color statement is as follows:
;
;		COLOR [background],[fg palette],[fg override]
;
;	where "background"  is a color number 0-255 which gets mapped to 0-15,
;	      "fg palette"  is a number which selects CGA palette 0 if even,
;						   or CGA palette 1 if odd,
;	  and "fg override" (if present) replaces and functions identically
;			    to "fg palette".
;
;	Any omitted parameter(s) indicate no change for that parameter.
;Entry:
;	parameter list
;		WORD 1 = flag 0 if param not present
;		WORD 2 = parameter if WORD 1 <> 0, else second param flag
;		etc.
;Exit:
;	PSW.C set if too many parameters, reset if Ok
;	b$NullColor set to background attribute value (always 0 for screen 1)
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	SetColor1
cProc	SetColor1,<NEAR>
cBegin
	cCall	B$GetParm	;AL=background parameter
	push	ax		;save background and flag
	cCall	B$GetParm	;AL=1st palette parameter
	xchg	ax,bx		;save 1st palette and flag
	cCall	B$GetParm	;AL=2nd palette parameter
	jnz	SetCol1 	;jump if param found
				;  which overrides the 1st one
	xchg	ax,bx		;restore 1st palette param
	or	ah,ah		;is it defaulted too?
	jnz	SetCol2 	;go if so
SetCol1:
;	If palette parameter, then user gets bogus palette reset to
;	0 or 1.
	push	ax		;save palette select value
	test	b$Adapter,EGA + VGA ;EGA or VGA?
	jnz	IsEga		;go if so
	xor	bx,bx		;set background subfunction (0)
	mov	bl,BackColor	;get chosen background color
	SCNIOS	vSetPalette	;set background
IsEga:				
	pop	bx		;restore palette select value
	mov	bh,1		;subfunction - set bogus palette
	and	bl,bh		;force palette number to 0 or 1
	SCNIOS	vSetPalette	
SetCol2:
	pop	bx		;the background parameter is on the stack
	or	bh,bh		;was there background parameter?
	jnz	SetColExit	;no, only palette
	and	bl,0FH		;Allow 0-255, mask to 0-15.
	test	bl,8		;If Bgnd to be intensified
	jz	SetCol3 	;Brif not
	or	bl,10H		;Set Intensity Bit
SetCol3:
	mov	BackColor,bl	;save it
	test	b$Adapter,EGA + VGA ; EGA card present? [10] or VGA?
	jz	NoEga		; No, use CGA BIOS call to set background
	mov	al,bl		;AL=color
	and	al,0FH		;strip intensity (PalPut will translate)
	cbw			;AX=color
	cwd			;DX:AX=color
	mov	bl,bh		;BL=attribute(0)
	call	[b$PalPut]	;make the EGA do what WE want
	jmp	short SetColExit 
NoEga:				
	SCNIOS	vSetPalette	;set background color
SetColExit:
	mov	b$NullColor,0	;use background for null color
	clc			;indicate no error
	jcxz	SetColDun	;if we got all params, thats true
	stc			;otherwise set error
SetColDun:
cEnd

;***
; B$xINITCGA - initialize CGA modes
;
;Purpose:
;	Added with revision [14].
;	Put the addresses of CGA screen mode support routines into the
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
cProc	B$xINITCGA,<FAR,PUBLIC> 
cBegin
	MOV	WORD PTR [b$ScreenTab + (1*2) + 1],OFFSET B$Screen1
	MOV	WORD PTR [b$ScreenTab + (2*2) + 1],OFFSET B$Screen2
cEnd


sEnd	GR_TEXT

	END
