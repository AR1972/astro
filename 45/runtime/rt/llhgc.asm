	TITLE		LLHGC - HGC screen mode support
;***
;LLHGC - HGC screen mode support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Support for Hercules Graphics Card (HGC) graphics
;	screen mode (BIOS 8).
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
;  3 x 80 25 H	M |  8	 2  N/A 720 348  32 9x14  2*  1 1
;
;  * only 1 page allowed if color card installed
;
; Discussion of HGC memory interleave line archtecture:
;	HGC video memory is organized into four interleaved banks of 8K
;	each.
;
;	Scan	Offset
;	Line	from B800
;	----	---------
;	0	0
;	1	8K
;	2	16K
;	3	24K
;	4	90
;	5	8K+90
;	6	16K+90
;	7	24K+90
;	.	.
;	.	.
;	.	.
;
;	To move  from scan line  to scan line  add to current address
;	-------  --------------  ------------  ----------------------
;	 UP	     MOD 0	     MOD 3	     24K-90
;	 UP	     MOD 1	     MOD 0	     -8K
;	 UP	     MOD 2	     MOD 1	     -8K
;	 UP	     MOD 3	     MOD 2	     -8K
;	 DOWN	     MOD 0	     MOD 1	      8K
;	 DOWN	     MOD 1	     MOD 2	      8K
;	 DOWN	     MOD 2	     MOD 3	      8K
;	 DOWN	     MOD 3	     MOD 0	    -24K+90
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
	INCLUDE llgrp.inc	;Constant definitions
	INCLUDE idmac.inc
	INCLUDE grmac.inc	;ModeData macros

	INITIALIZER B$xINITHERC ;Put B$xINITHERC in initializer list

sBegin	_BSS
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$BiosMode
externB b$Adapter
externW b$VideoBase
externB b$MaskC
externW b$OffC
externW b$SegC
externW b$CurrPSize
externW b$CurPages
externW b$UpSub
externW b$DnSub
externW b$UpDnAdd
externB	b$MaxPage		; maximum number of pages allowed - 1
externW	b$VideoMem		; used to calculate b$MaxPage
externW b$ScreenTab		
sEnd	_BSS

assumes CS,GR_TEXT
sBegin	GR_TEXT

externNP B$SCNIO		;used in SCNIO macro

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

;===========================================================================
mModeData	Mode3Data
;
; SCREEN 3, BIOS mode 8
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	3
mBiosMode	8
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	720
mVertRes	348
mVideoBase	0B000H
mMaxAttr	1
mMaxColor	-1		    ;no palette
mPageSize	32		    ;page size in K
mCurrPSize	<(32*1024) shr 4>   ;page size in paragraphs (1 plane)
mMaxPage	1
mNullColor	0
mForeColor	1
mBackColor	0
mEgaWrMd	0
mInitPalette	0
mInitVgaPal	0
mAlphaDim	AlphaDim3
mSetMode	SetMode3
mSetPages	SetPages
mPalReset	B$ErrorReturn
mPalPut 	B$ErrorReturn
mPalTrans	B$ErrorReturn
mPalSet 	B$ErrorReturn
mSetColor	B$ErrorReturn
mForeMapped	1
mBitsPerPixel	1
mPlanes 	1
mMapXYC 	MapXYC3
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

labelNP <PUBLIC, B$HRCUSED>	

;*** 
; B$Screen3
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 3.
;Entry:
;	AL = screen mode (3)
;	AH = burst (N/A)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen3,<PUBLIC,NEAR>
cBegin
	test	b$Adapter,HGC	;check for adapter support
	stc
	jz	ScrExit 	;exit w/error if not supported
	mov	bx,GR_TEXTOFFSET Mode3Data  ;mode-specific data
	mov	cx,GraphDataLen
	call	B$InitModeData	;initialize table data
	mov	b$UpSub,8192
	mov	b$DnSub,24576-90
	mov	b$UpDnAdd,8192+24576-90
	mov	ax,[b$VideoMem]
	rol	al,1		; divide by 32K
	rol	al,1		
	and	[b$MaxPage],al	; Max pages = b$Videomem / 32K
; NOTE:  Clear carry here -- currently done by the AND instruction
;	clc			;indicate no error
ScrExit:
cEnd

;***
; AlphaDim3
;
;Purpose:
;	Validate the proposed text dimensions for Screen 3.
;	  If 80x25 is requested, this mode satisfies the request
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
DbPub	AlphaDim3
cProc	AlphaDim3,<NEAR>
cBegin
	mov	al,-1		;flag request satisfied (maybe)
	cmp	bx,80+25*256	;80x25?
	je	ADim3Exit	;exit if so, standard stuff
	xor	al,al		;flag request for screen 0, let text mode try
ADim3Exit:
	clc			;no error
cEnd

;***
; SetMode3
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
DbPub	SetMode3
cProc	SetMode3,<NEAR>
cBegin
	mov	al,b$BiosMode	;set BIOS mode 8
	SCNIOS	vSetMode
cEnd

;***
; SetPages
;
;Purpose:
;	Set the current active and visual pages and calculate page size
;	and video segment offset for EGA modes.
;Entry:
;	AL = active page
;	AH = visual page
;Exit:
;	b$CurPages set to new active and visual pages.
;	b$SegC set to start of new active page.
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	SetPages,<NEAR>
cBegin
	mov	b$CurPages,ax	;save page numbers
	push	ax
	mov	al,ah
	SCNIO	vSelActivePage	;set visual page
	pop	ax
	cbw			;extend active page to word
	mul	b$CurrPSize	;times page size in paras
	add	ax,b$VideoBase ;set video segment
	mov	b$SegC,ax
cEnd

;***
; MapXYC3
;
;Purpose:
;	May given X and Y coordinates to the graphics cursor for Screen 3.
;Entry:
;	CX = X coordinate
;	DX = Y coordinate
;Exit:
;	b$OffC, b$MaskC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	MapXYC3
cProc	MapXYC3,<NEAR>
cBegin
	xor	ax,ax
	shr	dx,1		;dx=row within quadrant
	rcr	ax,1		;ax=8K multiple for quadrant displacement
	shr	dx,1
	rcr	ax,1
	shr	ax,1
	mov	bx,dx		;multiply y by 90 to compute row displacement
	shl	dx,1		;dx=2*Y
	shl	dx,1		;dx=4*Y
	add	dx,bx		;dx=5*Y
	shl	dx,1		;dx=10*Y
	mov	bx,dx
	shl	dx,1		;dx=20*Y
	shl	dx,1		;dx=40*Y
	shl	dx,1		;dx=80*Y
	add	dx,bx		;dx=90*Y
	add	dx,ax		;add in quadrant displacement
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
; B$xINITHERC - initialize HERC modes
;
;Purpose:
;	Added with revision [2].
;	Put the address of Hercules screen mode support routine into the
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
cProc	B$xINITHERC,<FAR,PUBLIC> 
cBegin
	MOV	WORD PTR [b$ScreenTab + (3*2) + 1],OFFSET B$Screen3
cEnd

sEnd	GR_TEXT

	END
