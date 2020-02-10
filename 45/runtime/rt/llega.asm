	TITLE		LLEGA - EGA screen mode support
;***
;LLEGA - EGA screen mode support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Support for EGA graphics screen modes (BIOS D,E,F,10).
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
;  7 x 40 25 E	C |  D	16  16	320 200  32 8x8  (2) 1 4
;  7 x 40 25 E	E |  "   "   "   "   "    "  "    "  " "
;  7 x 40 25 S	x |  "   "   "   "   "    "  "    "  " "
;
;  8 x 80 25 E	C |  E	16  16	640 200  64 8x8  (2) 1 4
;  8 x 80 25 E	E |  "   "   "   "   "    "  "    "  " "
;  8 x 80 25 S	x |  "   "   "   "   "    "  "    "  " "
;
;  9 x 80 25 E	E | 10	 4  64	640 350  64 8x14  1  1 2  (64K video memory)
;  9 x 80 43 E	E |  "   "   "   "   "    " 8x8   "  " "
;
;  9 x 80 25 E	E | 10	16  64	640 350 128 8x14 (2) 1 4  (>64K video memory)
;  9 x 80 25 S	x |  "   "   "   "   "    "  "    "  " "
;  9 x 80 43 E	E |  "   "   "   "   "    " 8x8   "  " "
;  9 x 80 43 S	x |  "   "   "   "   "    "  "    "  " "
;
; 10 x 80 25 E	M |  F	 4   9	640 350  64 8x14 (1) 1 2  (>64K video memory)
; 10 x 80 25 S	x |  "   "   "   "   "    "  "    "  " "
; 10 x 80 43 E	M |  "   "   "   "   "    " 8x8   "  " "
; 10 x 80 43 S	x |  "   "   "   "   "    "  "    "  " "
;
;-----------------------------------------------------------------------
;NOTES:  (1) PAGES = <video memory> / 2 / PAGESIZE (max 8 pages)
;	 (2) PAGES = <video memory> / PAGESIZE	   (max 8 pages)
;-----------------------------------------------------------------------
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
	INCLUDE baslibma.inc
	INCLUDE llgrp.inc	; Constant definitions
	INCLUDE idmac.inc
	INCLUDE grmac.inc	;ModeData macros

	INITIALIZER B$xINITEGA	;Put B$xINITEGA in initializer list


sBegin	_BSS
;
; ***************************************************************************
; External function vectors
; ***************************************************************************
;
externW b$PutVector
externW b$PalPut
externW b$SetAttr		
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$BiosMode
externB b$Adapter
externB b$Monitor
externW b$VideoMem
externW b$CurPages
externW b$VideoBase
externB b$MaskC
externB b$AttrC
externW b$PenC 		
externW b$OffC
externW b$SegC
externB b$MaxAttr
externB b$MaxColor
externW B$LEOFST
externW B$REOFST
externB B$VLMASK
externB B$VRMASK
externB b$ForeColor
externB b$PageSize
externW b$CurrPSize
externB b$ScrHeight
externB b$MaxPage
externB b$Tiling
externB b$ForeMapped		
externW b$CURSOR		
externW b$CSRTYP		
externW b$SaveCa
externB b$SaveCm
externB b$PlaneMask
externW b$ScreenTab		

;
; ***************************************************************************
; Local variables
; ***************************************************************************
;
staticB MapMask,,1		;bit mask for Map Mask Register odd/even map
staticB BasePlane,,1		;base plane for odd/even modes
sEnd	_BSS

sBegin	CN_TEXT
externNP B$USRCSR		
sEnd	CN_TEXT

assumes CS,GR_TEXT
sBegin	GR_TEXT

externNP B$OutWord
externNP B$BumpDS
externNP B$BumpES		
externNP B$InitModeData
externNP B$GetParm
externNP b$ColorPalette	
externNP b$EnhPalette		
externNP B$EgaPalReset
externNP B$EgaPalResetB	
externNP B$EgaPalPut
externNP B$EgaPalTrans
externNP B$EgaPalSet
externNP B$EgaMapXYC
externNP B$EgaMapXYC_D
externNP B$EgaLeftC
externNP B$EgaChkUpC
externNP B$EgaUpC
externNP B$EgaChkDownC
externNP B$EgaDownC
externNP B$EgaSetAttr
externNP B$EgaReadC
externNP B$EgaReadC_F
externNP B$EgaReadC_64K
externNP B$EgaSetC
externNP B$EgaSetPixC
externNP B$EgaSetPixFirstC
externNP B$ResetEGA
externNP B$EgaLineX
externNP B$EgaLineY
externNP B$EgaLineV
externNP B$EgaPutAction
externNP B$EgaPutAction_F
externNP B$EgaPutAction_64K
externNP B$EgaNReadL
externNP B$EgaNReadL_F
externNP B$EgaNWriteL
externNP B$EgaNWriteL_F
externNP B$EgaNSetC
externNP B$EgaPaintBound
externNP B$EgaPaintBound_D
externNP B$EgaSetTile
externNP B$EgaScanL
externNP B$EgaScanR
externNP B$EgaCHKBTL
externNP B$EgaCHKBTR
externNP B$EgaPAINPX
externNP B$EgaPIXCNT
externNP B$EgaScanInit
externNP B$EgaSETCMP
externNP B$EgaTILLFT
externNP B$EgaTILRGT

;===========================================================================
mModeData	ModeDData
;
; SCREEN 7, BIOS mode D
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	7
mBiosMode	0DH
mBurst		0
mScrWidth	40
mScrHeight	25
mHorzRes	320
mVertRes	200
mVideoBase	0A000H
mMaxAttr	15
mMaxColor	15
mPageSize	32		    ;page size in K
mCurrPSize	<(32/4*1024) shr 4> ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	15
mBackColor	0
mEgaWrMd	2
mInitPalette	b$ColorPalette 	;moved to LLXGASUP for sharing
mInitVgaPal	b$VgaPalette		
mAlphaDim	AlphaDim_D
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalReset
mPalPut 	B$EgaPalPut
mPalTrans	B$EgaPalTrans	
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mForeMapped	15
mBitsPerPixel	1
mPlanes 	4
mMapXYC 	B$EgaMapXYC_D
mLeftC		B$EgaLeftC
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
mSetAttr	B$EgaSetAttr
mReadC		B$EgaReadC
mSetC		B$EgaSetC
mSetPixC	B$EgaSetPixC
mSetPixFirstC	B$EgaSetPixFirstC
mSetPixLastC	B$ResetEGA
mLineX		B$EgaLineX
mLineY		B$EgaLineY
mLineV		B$EgaLineV
mPutAction	B$EgaPutAction
mNReadL 	B$EgaNReadL
mNWriteL	B$EgaNWriteL
mNSetC		B$EgaNSetC
mPaintBound	B$EgaPaintBound_D
mSetTile	B$EgaSetTile
mScanL		B$EgaScanL
mScanR		B$EgaScanR
mEnd		GraphDataLen
;===========================================================================

;===========================================================================
mModeData	ModeEData
;
; SCREEN 8, BIOS mode E
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	8
mBiosMode	0EH
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	200
mVideoBase	0A000H
mMaxAttr	15
mMaxColor	15
mPageSize	64		    ;page size in K
mCurrPSize	<(64/4*1024) shr 4> ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	15
mBackColor	0
mEgaWrMd	2
mInitPalette	b$ColorPalette 	;moved to LLXGASUP for sharing
mInitVgaPal	b$VgaPalette
mAlphaDim	AlphaDim_E
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalReset
mPalPut 	B$EgaPalPut
mPalTrans	B$EgaPalTrans	
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mForeMapped	15
mBitsPerPixel	1
mPlanes 	4
mMapXYC 	B$EgaMapXYC
mLeftC		B$EgaLeftC
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
mSetAttr	B$EgaSetAttr
mReadC		B$EgaReadC
mSetC		B$EgaSetC
mSetPixC	B$EgaSetPixC
mSetPixFirstC	B$EgaSetPixFirstC
mSetPixLastC	B$ResetEGA
mLineX		B$EgaLineX
mLineY		B$EgaLineY
mLineV		B$EgaLineV
mPutAction	B$EgaPutAction
mNReadL 	B$EgaNReadL
mNWriteL	B$EgaNWriteL
mNSetC		B$EgaNSetC
mPaintBound	B$EgaPaintBound
mSetTile	B$EgaSetTile
mScanL		B$EgaScanL
mScanR		B$EgaScanR
mEnd		GraphDataLen
;===========================================================================

;===========================================================================
mModeData	ModeFData
;
; SCREEN 10, BIOS mode F (with > 64K video memory>
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	10
mBiosMode	0FH
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	350
mVideoBase	0A000H
mMaxAttr	3
mMaxColor	8
mPageSize	64		    ;page size in K
mCurrPSize	<(64/2*1024) shr 4> ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	3
mBackColor	0
mEgaWrMd	2		    ;no even/odd with > 64K
mInitPalette	ModeFPalette
mInitVgaPal	0			;not applicable
mAlphaDim	AlphaDim_F
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalResetB
mPalPut 	PalPut_F
mPalTrans	PalTrans_F
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mForeMapped	5		    ;attr 3 maps to hardware attr 5
mBitsPerPixel	1
mPlanes 	2
mMapXYC 	B$EgaMapXYC
mLeftC		B$EgaLeftC
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
mSetAttr	SetAttr_F_10_64K
mReadC		B$EgaReadC_F
mSetC		B$EgaSetC
mSetPixC	B$EgaSetPixC
mSetPixFirstC	B$EgaSetPixFirstC
mSetPixLastC	B$ResetEGA
mLineX		B$EgaLineX
mLineY		B$EgaLineY
mLineV		B$EgaLineV
mPutAction	B$EgaPutAction_F
mNReadL 	B$EgaNReadL_F
mNWriteL	B$EgaNWriteL_F
mNSetC		B$EgaNSetC
mPaintBound	B$EgaPaintBound
mSetTile	B$EgaSetTile
mScanL		ScanLX
mScanR		ScanRX
mEnd		GraphDataLen
;===========================================================================

;===========================================================================
mModeData	ModeFData_64K
;
; SCREEN 10, BIOS mode F with 64K video memory
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	10
mBiosMode	0FH
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	350
mVideoBase	0A000H
mMaxAttr	3
mMaxColor	8
mPageSize	64		    ;page size in K
mCurrPSize	<(64/2*1024) shr 4> ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	3
mBackColor	0
mEgaWrMd	12H
mInitPalette	ModeFPalette
mInitVgaPal	0			;not applicable
mAlphaDim	AlphaDim_F
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalResetB
mPalPut 	PalPut_F
mPalTrans	PalTrans_F
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mForeMapped	5		    ;attr 3 maps to hardware attr 5
mBitsPerPixel	1
mPlanes 	2
mMapXYC 	B$EgaMapXYC
mLeftC		B$EgaLeftC
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
mSetAttr	SetAttr_F_10_64K
mReadC		B$EgaReadC_64K
mSetC		B$EgaSetC
mSetPixC	B$EgaSetPixC
mSetPixFirstC	B$EgaSetPixFirstC
mSetPixLastC	B$ResetEGA
mLineX		B$EgaLineX
mLineY		B$EgaLineY
mLineV		B$EgaLineV
mPutAction	B$EgaPutAction_64K
mNReadL 	NReadL_64K
mNWriteL	NWriteL_64K
mNSetC		B$EgaNSetC
mPaintBound	B$EgaPaintBound
mSetTile	B$EgaSetTile
mScanL		ScanLX
mScanR		ScanRX
mEnd		GraphDataLen
;===========================================================================

;===========================================================================
mModeData	Mode10Data
;
; SCREEN 9, BIOS mode 10 (with > 64K video memory)
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	9
mBiosMode	10H
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	350
mVideoBase	0A000H
mMaxAttr	15
mMaxColor	63
mPageSize	128		    ;page size in K
mCurrPSize	<(128/4*1024) shr 4>;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	15
mBackColor	0
mEgaWrMd	2
mInitPalette	b$EnhPalette		;moved to LLXGASUP for sharing
mInitVgaPal	b$VgaPalette		
mAlphaDim	AlphaDim_10
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalReset
mPalPut 	B$EgaPalPut
mPalTrans	B$EgaPalTrans
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mForeMapped	15
mBitsPerPixel	1
mPlanes 	4
mMapXYC 	B$EgaMapXYC
mLeftC		B$EgaLeftC
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
mSetAttr	B$EgaSetAttr
mReadC		B$EgaReadC
mSetC		B$EgaSetC
mSetPixC	B$EgaSetPixC
mSetPixFirstC	B$EgaSetPixFirstC
mSetPixLastC	B$ResetEGA
mLineX		B$EgaLineX
mLineY		B$EgaLineY
mLineV		B$EgaLineV
mPutAction	B$EgaPutAction
mNReadL 	B$EgaNReadL
mNWriteL	B$EgaNWriteL
mNSetC		B$EgaNSetC
mPaintBound	B$EgaPaintBound
mSetTile	B$EgaSetTile
mScanL		B$EgaScanL
mScanR		B$EgaScanR
mEnd		GraphDataLen
;===========================================================================

;===========================================================================
mModeData	Mode10Data_64K
;
; SCREEN 9, BIOS mode 10 (with 64K video memory)
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	9
mBiosMode	10H
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	350
mVideoBase	0A000H
mMaxAttr	3
mMaxColor	63
mPageSize	64		    ;page size in K
mCurrPSize	<(64/2*1024) shr 4> ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	3
mBackColor	0
mEgaWrMd	12H
mInitPalette	Mode10Palette_64K
mInitVgaPal	Mode10VgaPal_64K	
mAlphaDim	AlphaDim_10
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalReset
mPalPut 	B$EgaPalPut
mPalTrans	PalTrans_10_64K
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mForeMapped	15			
mBitsPerPixel	1
mPlanes 	2
mMapXYC 	B$EgaMapXYC
mLeftC		B$EgaLeftC
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
mSetAttr	SetAttr_F_10_64K
mReadC		B$EgaReadC_64K
mSetC		B$EgaSetC
mSetPixC	B$EgaSetPixC
mSetPixFirstC	B$EgaSetPixFirstC
mSetPixLastC	B$ResetEGA
mLineX		B$EgaLineX
mLineY		B$EgaLineY
mLineV		B$EgaLineV
mPutAction	B$EgaPutAction_64K
mNReadL 	NReadL_64K
mNWriteL	NWriteL_64K
mNSetC		B$EgaNSetC
mPaintBound	B$EgaPaintBound
mSetTile	B$EgaSetTile
mScanL		ScanLX
mScanR		ScanRX
mEnd		GraphDataLen
;===========================================================================

;
; ModeFPalette - used to initialize the EGA palette for SCREEN 10
;		 (BIOS mode F).
;
labelB	ModeFPalette		;EGA palette for BIOS mode 0FH
	;	RGBrgb
	DB	000000B 	;black
	DB	001000B 	;video
	DB	000000B 	;black
	DB	000000B 	;black
	DB	011000B 	;intensified
	DB	011000B 	;intensified
	DB	000000B 	;black
	DB	000000B 	;black
	DB	000000B 	;black
	DB	001000B 	;video
	DB	000000B 	;black
	DB	000000B 	;black
	DB	000000B 	;black
	DB	011000B 	;intensified
	DB	000000B 	;black
	DB	000000B 	;black
;
; Mode10Palette_64K - used to initialize the EGA palette for SCREEN 9
;		  (BIOS mode 10) with 64K of video memory.
;
labelB	Mode10Palette_64K	;EGA palette for BIOS mode 10H with only 64K
	;	RGBrgb
	DB	000000B 	;black
	DB	111011B 	;light cyan
	DB	000000B 	;black (not used)
	DB	000000B 	;black (not used)
	DB	111101B 	;light magenta
	DB	111111B 	;bright white
	;DB      10 DUP (0)      ;brown


labelNP <PUBLIC,B$EGAUSED>	

;*** 
; B$Screen7
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 7.
;Entry:
;	AL = screen mode (7)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen7,<PUBLIC,NEAR>
cBegin
	mov	bx,GR_TEXTOFFSET ModeDData  ;mode-specific data
	jmp	short ScrCommon2	    ;common routine
cEnd	<nogen>

;*** 
; B$Screen8
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 8.
;Entry:
;	AL = screen mode (8)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen8,<PUBLIC,NEAR>
cBegin
	mov	bx,GR_TEXTOFFSET ModeEData  ;mode-specific data
ScrCommon2:
	test	b$Monitor,AnalogColor + EnhColor + StdColor
				;check for monitor support
	je	ScrErr		;exit w/error if not supported
	jmp	short ScrCommon ;common routine
cEnd	<nogen>

;*** 
; B$Screen9
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 9.
;Entry:
;	AL = screen mode (9)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen9,<PUBLIC,NEAR>
cBegin
	test	b$Monitor,AnalogColor + EnhColor ;check for monitor support
	je	ScrErr		;exit w/error if not supported
	mov	bx,GR_TEXTOFFSET Mode10Data ;mode-specific data
	cmp	b$VideoMem,64	;check for mode 10 w/only 64K
	ja	ScrCommon	;go if greater, otherwise use diff table
	mov	bx,GR_TEXTOFFSET Mode10Data_64K ;mode-specific data
ScrCommon:
	test	b$Adapter,VGA + EGA ;check for adapter support
	je	ScrErr		;exit w/error if not supported
	mov	cx,GraphDataLen 
	call	B$InitModeData ;initialize table data
	mov	ax,b$VideoMem	;MaxPage = VideoMem / PageSize - 1
	div	b$PageSize	
	dec	al		;MaxPage = #pages-1
	mov	b$MaxPage,al
	clc			;indicate no error
cEnd

;*** 
; B$Screen10
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 10.
;Entry:
;	AL = screen mode (10)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen10,<PUBLIC,NEAR>
cBegin
	test	b$Monitor,AnalogMono + Monochrome ;check monitor support
	je	ScrErr		;exit w/error if not supported
	mov	bx,GR_TEXTOFFSET ModeFData  ;mode-specific data
	cmp	b$VideoMem,64	;check for mode F w/only 64K
	ja	Scr10Common	;go if greater, otherwise use diff table
	mov	bx,GR_TEXTOFFSET ModeFData_64K ;mode-specific data
Scr10Common:
	test	b$Adapter,VGA + EGA ;check for adapter support
	je	ScrErr		;exit w/error if not supported
	mov	cx,GraphDataLen 
	call	B$InitModeData ;initialize table data
	mov	ax,b$VideoMem	; MaxPage = VideoMem / 2 / PageSize - 1
	shr	ax,1		; divide VideoMem by 2
	div	b$PageSize	
	sub	al,1		;MaxPage = #pages-1
	adc	al,0		;if MaxPage < 0 (64K EGA), force back to 0
	mov	b$MaxPage,al
	clc			;indicate no error
	jmp	short ScrExit	
ScrErr:
	stc
ScrExit:			
cEnd

;***
; AlphaDim_D
;
;Purpose:
;	Validate the proposed text dimensions for Screen 7.
;	  If 40x25 is requested, this mode satisfies the request
;	  elseif 80x25 is requested, suggest screen mode 8
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
DbPub	AlphaDim_D
cProc	AlphaDim_D,<NEAR>
cBegin
	mov	al,-1		;flag request satisfied (maybe)
	cmp	bx,40+25*256	;40x25?
	je	ADimSet 	;exit if so, standard stuff
	mov	al,8		;prepare request for screen 8, JIC
	cmp	bx,80+25*256	;80x25?
	je	ADimDOk 	;exit if so, try screen 8
	xor	al,al		;flag request for screen 0
ADimDOk:
cEnd

;***
; AlphaDim_F
;
;Purpose:
;	Validate the proposed text dimensions for Screen 10.
;	  If 40 columns are requested, treat as if 80 were requested.
;	  Fall through to AlphaDim_10 for validation.
;Entry:
;	BL = number of lines
;	BH = number of columns
;Exit:
;	falls through to AlphaDim_10
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	AlphaDim_F
cProc	AlphaDim_F,<NEAR>
cBegin
	cmp	bl,40		;40 columns?
	jne	AlphaDim_10	;go if not, as if mode 10H
	mov	bl,80		;treat 40 as if request for 80
cEnd	<nogen> 		;fall thru to AlphaDim_10

;***
; AlphaDim_10/AlphaDim_E
;
;Purpose:
;	Validate the proposed text dimensions for Screen 9 or Screen 8,
;	depending on entry point.
;	  If 80x25 or 80x43 is requested, this mode satisfies the request
;	  elseif 40x25 is requested, suggest screen mode 7
;	  else suggest screen mode 0.
;Entry:
;	BL = number of lines
;	BH = number of columns
;Exit:
;	If this mode satisfies request
;	  AL = -1
;	  b$ScrHeight is set to value in BL
;	else
;	  AL = suggested screen mode to invoke for desired dimensions
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	AlphaDim_10
cProc	AlphaDim_10,<NEAR>
cBegin
	mov	al,-1		;flag request satisfied (maybe)
	cmp	bx,80+43*256	;80x43?
	je	ADimSet 	;exit if so, standard stuff

labelNP AlphaDim_E

	mov	al,-1		;flag request satisfied (maybe)
	cmp	bx,80+25*256	;80x25?
	je	ADimSet 	;exit if so, standard stuff
	mov	al,7		;prepare request for screen 7, JIC
	cmp	bx,40+25*256	;40x25?
	je	ADimOk		;exit if so, try screen 7
	xor	al,al		;flag request for screen 0
	jmp	short ADimOk	;  and exit
ADimSet:
	mov	b$ScrHeight,bh ;set alpha rows
ADimOk:
	clc			;no error
cEnd

;***
; SetMode
;
;Purpose:
;	Set EGA screen mode according to the characteristics established
;	by previous call to B$Screenx and b$AlphaDim.  Set 8x8 character
;	font if new mode is to be 43 lines.
;Entry:
;	b$BiosMode is mode to set
;	b$ScrHeight is number of lines
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	SetMode,<NEAR>
cBegin
	mov	al,b$BiosMode	;set BIOS mode
	SCNIO	vSetMode
	cmp	b$ScrHeight,43 ;43 lines?
	jne	SetMode25	;go if not
	mov	dl,43		; char gen call wants # of lines in DL
	mov	ax,1123H	;character generator request
	xor	bl,bl		;  to load 8x8 font
	SCNIO			;  which gets 43 lines
SetMode25:
	call	B$ResetEGA
	MOV	DX,b$CURSOR	; Get current cursor position
	MOV	BYTE PTR b$CSRTYP,-1 ; invalidate present cursor type so it
				; will get changed
	CALL	B$USRCSR	; display user cursor
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
; PalPut_F
;
;Purpose:
;	Change palette entry for Screen 10 with translation/verification.
;	A color value of -1 indicates that the associated palette
;	entry is not to be modified.
;Entry:
;	DX:AX = color
;	BL = attribute
;Exit:
;	PSW.C reset indicates successful operation
;		set indicates PALETTE function call error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	PalPut_F
cProc	PalPut_F,<NEAR>
cBegin
	cmp	ax,-1		;lo word of color == -1?
	jne	PalPut1 	;go if not, can't ignore
	cmp	dx,ax		;hi word too?
	je	PalPutExit	;exit if color == -1
PalPut1:			
	call	PalTrans_F	;translate to external form
	jc	PalPutExit	;exit if invalid w/error
	mov	bh,al		;BH:BL = color:attribute
	mov	al,bl		;ah:al = 2nd pair
	add	al,8
	push	ax		;save 2nd values in pair
	xor	al,al		;subfunction "Set Individual Palette Register"
	SCNIO	vSetEgaPalette
	pop	ax
	xchg	bx,ax		;2nd pair
	xor	al,al		;subfunction "Set Individual Palette Register"
	SCNIO	vSetEgaPalette
	clc			;no error
PalPutExit:
cEnd

labelW	TransF
	DB	0,0,0,8,0,24	;9 possible value pairs of mode 0FH
	DB	8,0,8,8,8,24	;0 - black, 8 - video, 18H - intensified
	DB	24,0,24,8,24,24

;***
; PalTrans_F
;
;Purpose:
;	Translate a user supplied attribute/color pair for Screen 10 to
;	the corresponding hardware values after verifying that they are
;	in the legal range.
;	    Attribute mapping:	0 --> 0
;				1 --> 1
;				2 --> 4
;				3 --> 5
;	    Color pair mapping per TransF table.
;Entry:
;	DX:AX = user supplied color value
;	BL    = user supplied attribute value
;Exit:
;	PSW.C set if illegal value, reset if Ok
;	AX = actual color pair value
;	BL = actual attribute value
;Uses:
;	per conv.
;Exceptions:
;	can exit through PalTrans_10_64K error exit.
;******************************************************************************
DbPub	PalTrans_F
cProc	PalTrans_F,<NEAR>
cBegin
	cmp	bl,b$MaxAttr	;is legal attribute ?
	ja	PalTrErr	;error return
	or	dh,dl		;hi 3 bytes of color must be 0
	or	dh,ah		
	jnz	PalTrErr	;error if not
	cmp	al,b$MaxColor	;is legal color ?
	ja	PalTrErr	;error return
	cmp	bl,2		;attributes 0 and 1 map directly
	jb	PalTrNxt	;go if so
	add	bl,2		;attribute 2 maps to 4, 3 to 5
PalTrNxt:
	push	di
	MOV	DI,GR_TEXTOFFSET TransF ;translate to color pair
	XOR	AH,AH
	SHL	AL,1		;word index
	ADD	DI,AX
	MOV	AX,CS:[DI]	;translate color AX to color pair AX
	pop	di
	clc			;no error
cEnd

;***
; PalTrans_10_64K
;
;Purpose:
;	Translate a user supplied attribute for Screen 9/64K to the
;	corresponding hardware value after verifying that the attribute
;	value and the color value are in the legal ranges.
;	    Attribute mapping:	0 --> 0
;				1 --> 1
;				2 --> 4
;				3 --> 5
;Entry:
;	DX:AX = user supplied color value
;	BL    = user supplied attribute value
;Exit:
;	PSW.C set if illegal value, reset if Ok
;	DX:AX = unchanged (user supplied color value)
;	BL    = actual attribute value
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	PalTrans_10_64K
cProc	PalTrans_10_64K,<NEAR>
cBegin
	call	B$EgaPalTrans	;check attribute and translate color
	jc	PalTrErr	;error return
	cmp	bl,2		;attributes 0 and 1 map directly
	jb	PalTrExit	;go if so
	add	bl,2		;attribute 2 maps to 4, 3 to 5
PalTrExit:
	clc			;no error
	ret
PalTrErr:
	STC			;indicate error
cEnd

labelB	AttrX
	DB	0,3,0CH,0FH	;map to 0,3,C,F for odd/even

;***
; SetAttr_F_10_64K
;
;Purpose:
;	Map attribute value to the value needed by odd/even mode functions.
;		0 --> 00
;		1 --> 03
;		2 --> 0C
;		3 --> 0F
;	If supplied attribute is outside legal range, use maximum legal value.
;Entry:
;	AL = attribute
;Exit:
;	b$Attr updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	SetAttr_F_10_64K
cProc	SetAttr_F_10_64K,<NEAR>,<AX,BX>
cBegin
	cmp	al,b$MaxAttr	;test against maximum attribute
	jbe	SetAttrXOk	;Brif legal
	mov	al,b$MaxAttr	;limit to max
SetAttrXOk:
	mov	bx,GR_TEXTOFFSET AttrX	;translate for odd/even mode
	xlat	cs:[bx]
	mov	b$AttrC,al
	clc			;exit no error
cEnd

;***
; SetColor
;
;Purpose:
;	Process the color statement for Bios modes 0Dh - 010h (BASIC Screen
;	modes 7-10).  Syntax for Screen 7-10 color statement is as follows:
;
;		COLOR	[foreground],[background]
;
;	where "foreground" is the attribute to be used for the foreground
;	  and "background" is the color to be used for the background
;
;	Any omitted parameter(s) indicate no change for that parameter.
;Entry:
;	parameter list
;		WORD 1 = flag 0 if param not present
;		WORD 2 = parameter if WORD 1 <> 0, else second param flag
;		etc.
;Exit:
;	PSW.C set if error, reset if Ok.
;	b$ForeColor is set to foreground attribute
;	b$ForeMapped is set to foreground attribute mapped to internal value
;
;Uses
;	per conv.
;Exceptions:
;*****************************************************************************
cProc	SetColor,<NEAR>
cBegin
	cCall	B$GetParm	;AL=foreground parameter
	mov	bh,b$ForeColor ;use old values as default
	mov	bl,b$ForeMapped
	jz	GotFore 	;go if none supplied
	cmp	b$MaxAttr,al	;check for valid range
	jc	SetColDun	;go if error
	push	ax		;save: unmapped attr [14]
	push	cx		;      param count
	push	b$PenC 	;  b$AttrC
	call	[b$SetAttr]	;translate the attribute
	mov	bl,b$AttrC	;translated attr in BL
	pop	b$PenC 	
	pop	cx
	pop	ax		;restore unmapped attr
	mov	bh,al		;unmapped attr to BH
GotFore:
	cCall	B$GetParm	;AL=background parameter
	jz	NoBack		;go if none supplied
	push	bx		;preserve foreground info
	xor	bx,bx		;attribute 0 is background
	cbw			;AX=color
	cwd			;DX:AX=color
	call	[b$PalPut]	;set background
	pop	bx
	jc	SetColDun	;go if error
NoBack:
	mov	b$ForeColor,bh ;save foreground values
	mov	b$ForeMapped,bl
	clc			;indicate no error
	jcxz	SetColDun	;if we got all params, thats true
	stc			;otherwise set error
SetColDun:
cEnd

;***
; PIXLF3
;Purpose:
;	Check for non-paint pixels left, odd/even EGA modes.
;	Look through entire range of non-border pixels right to left
;	to determine whether any will actually change color.
;	Use AL as Color Don't Care mask.
;
;	If using EGAINT10 interface, this routine must access the CDCReg
;	and perform the associated color compare read with ints disabled,
;	then reenable them to allow other processes to access the card
;	before the next byte is checked.  Since another process may have
;	modified the graphics chip index reg as well as the data reg, we
;	must send both index and data for each odd/even toggle.
;Entry:
;	SI = byte address of rightmost byte in range
;	BH = bit mask for leftmost byte
;	BL = bit mask for rightmost byte
;	DI = total number of whole bytes between first and last
;	ES = video segment
;Exit:
;	CL = 0 if no pixels found to change, non-zero if pixels to change
;Uses:
;	per conv.
;Exceptions:
;****
DbPub	PIXLF3
cProc	PIXLF3,<NEAR>,<DX>
cBegin
	MOV	DX,GRPADD	;index port for graphics register
	MOV	AL,LOW CDCREG	;specify Color Don't Care
	EGAINT10CLI		;disable ints if using EGAINT10
	OUT	DX,AL
	INC	DX
	MOV	AL,10101010B	;initially assume odd byte address
	TEST	SI,1
	JNZ	STMSKL		;if correct, set this mask
	ROR	AL,1		;else rotate for other pair of planes
STMSKL:
	OUT	DX,AL		;set up for initial color compare

;	read first byte

	MOV	CL,ES:[SI]	;bit pattern of first byte with 0's where
				;color not paint attribute
	AND	CL,BL		;AND now produces difference if non-paint
				;bit in significant position
	XOR	CL,BL		;for significant bits in our first byte (bit
				;set in BH), then non-paint will be one
	JNZ	SETBTL		;found a bit to change
	OR	BH,BH		;see if only one byte
	JZ	NOSETL		;nothing to paint
	OR	DI,DI		;see if only a "last byte"
	JZ	LSBYT3

;	Look at whole bytes within viewport range until non-paint color found.

LKPTL3:
	DEC	SI
	ROR	AL,1
	OUT	DX,AL		;set up opposite Don't Care planes each time
	MOV	CL,ES:[SI]
	EGAINT10STI		;read is done, reenable ints if EGAINT10
	NOT	CL		;check if all bits set (all paint color)
	OR	CL,CL		;NOT does not affect flags
	JNZ	SETBTL
	DEC	DI
	EGAINT10CLI		;disable ints for next OUTs (loop
				;	or fall through)
	JNZ	LKPTL3		;keep looking until search of complete
				;bytes is exhausted

;	On last byte now, mask in BH.

LSBYT3:
	DEC	SI
	ROR	AL,1
	OUT	DX,AL		;last Don't Care
	MOV	CL,ES:[SI]	;do last compare
	AND	CL,BH		;significant bit = 0 if not paint color
	XOR	CL,BH		;if different, non-paint pixel(s) present
	JNZ	SETBTL
NOSETL:
	XOR	CL,CL		;no bits to set, so zero pixels-changed flag
SETBTL:
	EGAINT10STI		;reenable ints at common exit
cEnd

;***
; PIXRT3
;Purpose:
;	Check for non-paint pixels right with odd/even EGA modes.
;	Look through entire range of non-border pixels left to right
;	to determine whether any will actually change color.
;	AL is used for Color Don't Care mask
;
;	If using EGAINT10 interface, this routine must access the CDCReg
;	and perform the associated color compare read with ints disabled,
;	then reenable them to allow other processes to access the card
;	before the next byte is checked.  Since another process may have
;	modified the graphics chip index reg as well as the data reg, we
;	must send both index and data for each odd/even toggle.
;Entry:
;	SI = byte address of leftmost byte in range
;	BH = bit mask for rightmost byte
;	BL = bit mask for leftmost byte
;	DI = total number of whole bytes -1
;	ES = video segment
;Exit:
;	CL = 0 if no pixels found to change, non-zero if pixels to change
;Uses:
;	per conv.
;Exceptions:
;****
DbPub	PIXRT3
cProc	PIXRT3,<NEAR>,<DX>
cBegin
	MOV	DX,GRPADD	;index port for graphics
	MOV	AL,LOW CDCREG	;set up for Color Don't Care register
	EGAINT10CLI		;disable ints if using EGAINT10
	OUT	DX,AL
	INC	DX
	MOV	AL,10101010B	;initially assume odd byte mask
	TEST	SI,1		;check whether first byte odd or even
	JNZ	SETMSK		;if odd, branch to set mask
	ROR	AL,1		;else rotate mask 1 right
SETMSK:
	OUT	DX,AL

;	read first byte

	MOV	CL,ES:[SI]	;bit pattern of first byte with 0's where
				;color not paint attribute
	AND	CL,BL		;AND now produces difference if non-paint
				;bit in significant position
	XOR	CL,BL		;for significant bits in our first byte (bit
				;set in BH), then non-paint will be one
	JNZ	PNTBIT		;found a bit to change
	OR	DI,DI		;any more bytes?
	JZ	PIXLAST 	;no full bytes, go try last


;	Look at whole bytes within viewport range until non-paint color found.

LKPTR3:
	INC	SI
	ROR	AL,1		;shift Don't Care mask
	OUT	DX,AL
	MOV	CL,ES:[SI]
	EGAINT10STI		;read is done, reenable ints if EGAINT10
	NOT	CL		;check if all bits set (all paint color)
	OR	CL,CL		;NOT does not affect flags
	JNZ	PNTBIT
	DEC	DI
	EGAINT10CLI		;disable ints for next OUTs (loop
				;	or fall through)
	JNZ	LKPTR3		;keep looking until search of complete
				;bytes is exhausted

;	On last byte now, mask in BH.
PIXLAST:			
	INC	SI
	ROR	AL,1
	OUT	DX,AL
	MOV	CL,ES:[SI]	;do last compare
	AND	CL,BH		;significant bit = 0 if not paint color
	XOR	CL,BH		;if different, non-paint color pixels
	JNZ	PNTBIT		;leave CL non-zero as flag
NOBTR3:
	XOR	CL,CL		;else zero the flag
PNTBIT:
	EGAINT10STI		;reenable ints at common exit
cEnd

;***
; ScanLX
;Purpose:
;	Scan left beginning with the pixel to the left of cursor,
;	and paint pixels until:
;		(1) the viewport edge is encounteered (edge painted)
;		(2) a border pixel is encountered (border not painted)
;
;	This version supports PAINT for the odd/even EGA modes (bios mode
;	10H with 64K of memory, and monochrome bios mode F).  It differs
;	from SCANL2 in its use of the Color Don't Care register in
;	conjunction with screen reads.	This is necessary because if the
;	planes representing the even bytes and those representing the odd
;	bytes are not disabled during color compares for odd and even bytes,
;	respectively, the color compare is made as the sum of the bits set
;	for each even byte and its odd successor, all four planes at one
;	address.
;Entry:
;	b$AddrC, b$MaskC = pixel to right of starting pixel
;	b$PaintBorder	   = attribute of paint region border
;	b$AttrC	   = attribute to paint
;	B$LEOFST, B$VLMASK   = left viewport edge
;Exit:
;	BX		   = number of pixels scanned
;	CL		   = 0 iff no pixels changed color
;	b$OffC, b$MaskC  = the last non-border pixel examined/painted
;Uses:
;	per conv.
;Exceptions:
;*****************************************************************************
DbPub	ScanLX
cProc	ScanLX,<NEAR>,<ES>
cBegin
	CALL	B$EgaScanInit
	ROL	CH,1		;see if cursor is left edge of byte
	JNB	VWBYT1		;if not, proceed to viewport checks
	DEC	SI		;if so, start next byte left
	JS	SCNOUT		;if negative, hit corner of screen
VWBYT1:				;start on-screen viewport checks
	CMP	SI,B$LEOFST	;see if off edge of viewport to left
	JNB	VWBYT2
SCNOUT:
	JMP	BRDEX3		;else do nothing, exit
VWBYT2:
	JNZ	NOTBYT		;if not edge byte, skip bit checks
	CMP	CH,B$VLMASK	;else check for pixel left too far and
	JNA	NOTBYT
	JMP	BRDEX3		;thus off viewport edge -- exit if so
NOTBYT:
	MOV	DI,SI		;extra copy of first byte address
	MOV	BL,CH		;extra copy of initial bit mask
	MOV	BP,-1		;this will be count of whole bytes
	XOR	AH,AH		;initialize this byte's viewport mask to 0

;	First task is to set up initial Color Don't Care mask depending
;	on whether first byte is odd or even.

	MOV	CL,10101010B	;initially assume odd byte -- 10101010
	TEST	SI,1
	JNZ	STCDC3
	ROR	CL,1		;if even, set up 01010101
STCDC3:
	MOV	DX,GRPADD
	MOV	AL,LOW CDCREG	;index to Color Don't Care register

	OUT	DX,AL
	INC	DX
	MOV	AL,CL
	OUT	DX,AL

;	read first byte off the screen

	MOV	AL,ES:[DI]
	EGAINT10STI		;read is done, reenable ints if EGAINT10
	TEST	AL,CH		;see whether initial pixel is border
	JZ	SRCLF3		;if not, start search left
	XOR	CL,CL		;else set pixels-changed flag back to 0
	MOV	BL,CL		;zero out 8-bit register used
	JMP	BRDEX3		;and exit gracefully
SRCLF3:

;	look for border or viewport in first byte

	CMP	DI,B$LEOFST	;is this in fact viewport edge byte?
	JNZ	NTVWL3
	MOV	AH,B$VLMASK	;if so, set up viewport mask in AH
NTVWL3:

;	while not border

	TEST	AL,CH
	JNZ	HAVPX3

;	and not viewport edge

	TEST	AH,CH
	JNZ	HAVPX3

;	and not off the edge of the byte

	ROL	CH,1
	JNB	NTVWL3

;	keep moving left - edge of first byte

	DEC	DI		;next byte address left
	INC	BP		;count of intermediate bytes
	ROR	CL,1		;rotate Color Don't Care mask
	MOV	AL,CL
	OUT	DX,AL
	MOV	AL,ES:[DI]	;read next byte left
	EGAINT10STI		;reenable ints between bytes if EGAINT10
	JMP	SHORT SRCLF3	;check next byte

;	Here when border or viewport edge found.
;	Set up bit mask for first (possibly only) byte
;	SI = rightmost byte
;	DI = leftmost byte (possibly same byte)
;	BL = mask for rightmost bit in rightmost byte

;	If viewport edge was found, AH will contain the viewport bit
;	mask, and DI is the viewport edge byte.  If SI=DI=viewport edge
;	byte, we need to retain the viewport mask in AH.  Otherwise
;	clear AH and fetch the mask again later if needed for DI.

HAVPX3:
	CMP	SI,B$LEOFST	;see if rightmost byte is LEFT viewport
	JZ	SINTV2		;if so, don't clear viewport mask
				; register
	XOR	AH,AH		;else clear AH for CHKBTL on
SINTV2:				; rightmost byte
	MOV	CH,BL		;initial bit position in CH
	MOV	CL,10101010B	;need to reset Color Don't Care mask
	TEST	SI,1		;see if odd byte
	JNZ	SISET
	ROR	CL,1
SISET:
	MOV	AL,CL
	OUT	DX,AL		;set up Color Don't Care
	MOV	AL,ES:[SI]	;get border bits if any
	XOR	DX,DX		;this will be #pixels painted
	CALL	B$EgaCHKBTL	;set up bit mask for first byte
	MOV	BL,BH		;store in BL
	XOR	BH,BH		;there may be only one byte

;	see if more than 1 byte to paint

	PUSH	SI		;save a copy of rightmost address
	INC	BP		;see if still -1
	JZ	ONEALN		;"one alone"
	DEC	BP		;if not, recover real value
	PUSH	DX		;store pixel count
	MOV	DX,GRPADD+1	;data port for Color Don't Care
	MOV	CL,10101010B	;assume mask 10101010
	TEST	DI,1		;check DI for odd or even address
	JNZ	DISET
	ROR	CL,1		;if even, set up 01010101
DISET:
	MOV	AL,CL
	OUT	DX,AL
	POP	DX		;restore pixel count
	MOV	AL,ES:[DI]	;get border bits if any
	MOV	CH,1		;set up mask for final byte
	CMP	DI,B$LEOFST	;was this viewport byte?
	JNZ	DINTV2		;no -- don't need viewport mask
	MOV	AH,B$VLMASK	;yes -- get viewport mask for CHKTBL
DINTV2:
	CALL	B$EgaCHKBTL	;set up leftmost byte bit mask in BH
ONEALN:
	MOV	b$OffC,DI
	MOV	b$MaskC,CH	;update cursor
	PUSH	DI		;save a copy of leftmost address
	MOV	DI,BP		;store whole byte count for PIXLFT
	CMP	b$Tiling,0	;see if tiling is on
	JZ	COLCM7
	CALL	B$EgaTILLFT
	JMP	SHORT COLCM8
COLCM7:
	CALL	B$EgaSETCMP	;set color compare register to paint attribute
	CALL	PIXLF3		;see whether any pixels in range will change
COLCM8:
	POP	SI		;restore leftmost address to SI
	POP	DI		;restore rightmost address to DI
	OR	CL,CL		;returns CL non-zero if changes needed
	JZ	BRDEX2

;	we found at least 1 pixel to change, so set entire range
;	set pixels-changed flag, set up write mode 2

	XOR	CH,CH
	NOT	CH		;set to FF as decrement flag
	STD			;for SCANL, decrement from DI
	CALL	B$EgaPAINPX
	CLD
BRDEX2:
	CALL	B$EgaPIXCNT	;returns # pixels "painted" in BX
BRDEX3:
	CALL	B$ResetEGA
cEnd

;***
; ScanRX
;Purpose:
;	Starting with the current pixel, search right until:
;		(1) a non-border pixel is found
;		(2) [DX] pixels have been tested
;		(3) the viewport edge is encountered
;
;	If (2) or (3) terminated the scan, exit with:
;		DX = remaining border bount = 0
;
;	If (1) terminated the scan, scan and paint non-border pixels until:
;		(1) the viewport edge is encountered (edge painted)
;		(2) a border pixel is encountered (border not painted)
;
;	This version supports PAINT for the odd/even EGA modes (bios mode
;	10H with 64K of memory, and monochrome bios mode F).  It differs
;	from SCANR2 in its use of the Color Don't Care register in
;	conjunction with screen reads.	This is necessary because if the
;	planes representing the even bytes and those representing the odd
;	bytes are not disabled during color compares for odd and even bytes,
;	respectively, the color compare is made as the sum of the bits set
;	for each even byte and its odd successor, all four planes at one
;	address.
;Entry:
;	DX		   = count of border pixels which may be skipped
;	b$AddrC, b$MaskC = starting pixel
;	b$PaintBorder	   = attribute of paint region border
;	b$AttrC	   = attribute to paint
;	B$REOFST, B$VRMASK   = right viewport edge
;Exit:
;	BX		   = number of pixels painted
;				(whether or not they changed color)
;	CL		   = 0 iff no pixels changed color
;	DX		   = remaining border pixel count
;	b$OffC, b$MaskC  = the last non-border pixel examined/painted
;	SI, AL		   = the first non-border pixel encountered
;Uses:
;	per conv.
;Exceptions:
;
;****************************************************************************
DbPub	ScanRX
cProc	ScanRX,<NEAR>,<ES>
cBegin
;	set up EGA registers for color compare read
;	point ES:[SI] to screen memory, b$MaskC in CH
;	CL = 0 (pixels changed flag)

	CALL	B$EgaScanInit	;setup

;	Initial task is to set up a mask for specifying which planes
;	we want to read when doing color compare reads.	For even bytes,
;	we need to specify 0's in bits 1 and 3 (color don't care planes
;	1 and 3), vice versa for odd bytes.  For convenience in coding,
;	we set up an 8-bit mask in CL and rotate right as we move across
;	the screen.

	MOV	BX,DX		;decrement BX instead of DX (needed for OUTs)
	MOV	CL,10101010B	;assume start mask 10101010
	TEST	SI,1		;check whether odd or even byte
	JNZ	COLCOM		;if odd, we're in business
	ROR	CL,1		;else 010101
COLCOM:
	MOV	DX,GRPADD	;address of index port
	MOV	AL,LOW CDCREG	;Color Don't Care register

	OUT	DX,AL
	INC	DX		;data port address
	MOV	AL,CL		;mask indicating planes to ignore (=0)
	OUT	DX,AL		;set up initial Color Don't Care planes

;	perform color compare on first byte

	MOV	AL,ES:[SI]	;bits set where border found
	EGAINT10STI		;read is done, reenable ints if EGAINT10

;	Starting at entry cursor, search right looking for non-border,
;	viewport edge, or end-of-byte as long as DX does not decrement to 0.

	XOR	AH,AH		;initialize viewport mask to 0
SRCRT3:
	CMP	SI,B$REOFST	;check whether we are in viewport edge byte
	JNZ	NOTVP3
	MOV	AH,B$VRMASK	;if so, get viewport edge mask
NOTVP3:

;	While border...

	TEST	AL,CH		;compare color compare mask with b$MaskC
	JZ	ENDRT3		;if pixel not border, exit loop

;	and not viewport edge...

	TEST	AH,CH		;compare viewport edge mask with b$MaskC
	JNZ	ENDRT3		;if edge found, exit

;	and BX is greater than 0...

	DEC	BX		;contains # pixels which can be skipped
	JZ	ENDRT3		;in search for non-border pixel

;	and not off the edge of the byte...

	ROR	CH,1		;shift bit mask right

;	repeat the search

	JNB	NOTVP3

;	end of first byte.

	INC	SI		;next byte address
	ROR	CL,1		;rotate mask for next Color Don't Care
	MOV	AL,CL
	OUT	DX,AL		;next compare with alternate planes disabled
	MOV	AL,ES:[SI]
	EGAINT10STI		;reenable ints between bytes if EGAINT10
	MOV	CH,80H		;mask now 1000/0000 for next search
	JMP	SHORT SRCRT3

;	either (not border) OR (viewport edge) OR (DX = 0)

ENDRT3:
	MOV	DX,BX		;return decremented value to proper register
	TEST	AL,CH		;border?
	JZ	NTBRD3		;if so, we are either at viewport edge
	XOR	DX,DX		;or have skipped DX pixels and therefore
	MOV	BX,DX		;should exit with info as initialized
	XOR	CL,CL		;restore old value to flag
	JMP	SHORT SCNEX3	

;	Look for viewport edge to determine how many bytes to look
;	through for border pixel.

NTBRD3:
	PUSH	DX		;store skipcount for later
	XOR	DX,DX		;use to count pixels painted
	MOV	b$SaveCa,SI	;we have a new CSAVE
	PUSH	SI		;store copy of first byte address
	MOV	b$SaveCm,CH
	CALL	B$EgaCHKBTR	;set up byte for write, and count some pixels
				;(AH = viewport edge mask if any)
	MOV	BL,BH		;store first bit mask in BL
	XOR	BH,BH		;zero BH until last byte bit mask if any
	XOR	BP,BP		;start whole byte count at 0
	MOV	DI,B$REOFST
	SUB	DI,SI		;viewport edge address - first byte address
	TEST	BL,1		;if last bit not set, we found border for sure
	JZ	WRTPX3		;if just one byte, we're done
	OR	DI,DI		;check also if we hit viewport edge
	JZ	WRTPX3		;if so, also done

;	else look through DI bytes for border (this includes viewport
;	edge byte)

	DEC	BP		;start increment at -1
	MOV	CH,80H		;start each byte at left edge
	PUSH	DX		;save accumulating bit count
	MOV	DX,GRPADD+1	;prepare to send Color Don't Care data
SCANM3:
	INC	BP		;whole byte count
	INC	SI		;point to byte
	ROR	CL,1		;rotate plane mask
	MOV	AL,CL
	OUT	DX,AL
	MOV	AL,ES:[SI]	;read each byte for color compare
	EGAINT10STI		;read is done, reenable ints if EGAINT10
	OR	AL,AL		;check for occurrence of border pixel(s)
	JNZ	BRDPX3		;set up last byte
	DEC	DI		;decrement to 0 to include last byte
	JNZ	SCANM3		;go check out this byte
;	MOV	AH,B$VRMASK	;if edge of viewport, get viewport mask
				;and proceed to set up byte for write
BRDPX3:
				;may have found border, viewport
				; edge, or have both in same byte
	CMP	SI,B$REOFST	;heck if this is edge byte
	JNZ	BRDFD3
	MOV	AH,B$VRMASK	;if found, install viewport edge mask
BRDFD3:
	POP	DX		;restore pixel count
	CALL	B$EgaCHKBTR	;set up byte for write

;	most recent call to CHKBTR has generated new cursor location and mask

WRTPX3:
	MOV	b$OffC,SI
	MOV	b$MaskC,CH
	POP	DI		;restore leftmost byte address
	PUSH	DI		;save a copy for leftmost add. for painting
	PUSH	SI		;save copy of rightmost address also
	MOV	SI,DI		;leftmost byte address in SI for PIXRGT
	MOV	DI,BP		;PIXRGT will use DI to count whole bytes
	CMP	b$Tiling,0	;see whether tiling is on
	JZ	COLCM3
	CALL	B$EgaTILRGT
	JMP	SHORT COLCM4
COLCM3:
	CALL	B$EgaSETCMP	;set color compare register to paint attribute
	CALL	PIXRT3		;routine to determine whether any pixels change
COLCM4:
	POP	SI		;restore rightmost
	POP	DI		;and leftmost byte addresses
	OR	CL,CL		;non-zero indicates at least one must change
	JZ	NPNTR3
	XOR	CH,CH		;zero as increment flag
	CLD			;for SCANR, paint routine should increment REP
	CALL	B$EgaPAINPX	;set line
NPNTR3:
	CALL	B$EgaPIXCNT	;return # pixels "painted" in BX
	POP	DX		;skipcount in DX
SCNEX3:
	CALL	B$ResetEGA	;reset EGA registers for BIOS write mode 0
	MOV	SI,b$SaveCa	;return CSAVE
	MOV	AL,b$SaveCm
cEnd

	ASSUME	DS:NOTHING

;***
; Read_64K
;
;Purpose:
;	Support routine for NReadL_64K, reads one byte from screen
;	memory into AL.  Read from even plane even address, from
;	odd plane if odd address.
;Entry:
;	DS:SI = screen address
;Exit:
;	AL    = screen contents from address at ES:DI
;	DI    = incremented to next screen byte
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	Read_64K
cProc	Read_64K,<NEAR>
cBegin
	EGAINT10CLI		;disable ints if using EGAINT10
	mov	al,RMPREG	;select read map select register
	out	dx,al
	inc	dx
	xor	al,al		;for plane computation
	ror	di,1		;carry = 1 if odd address
	adc	al,BasePlane	;base plane +1 iff odd address
	rol	di,1		;restore address
	out	dx,al		;set plane to read
	dec	dx
	lodsb			;read it (finally!!)
	EGAINT10STI		;reenable ints if using EGAINT10
cEnd

;***
; NReadL_64K
;
;Purpose:
;	Read a line of pixels from a specified plane to an array for
;	64K Screen mode 9 (odd/even color mode).
;Entry:
;	DS:SI = screen address
;	ES:DI = array address
;	CL    = array align shift count
;	CH    = mask for last partial byte
;	BP    = count of bits to read
;	BH    = plane to read from
;Exit:
;	ES:DI = updated to array byte past point used
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	NReadL_64K
cProc	NReadL_64K,<NEAR>
cBegin
	MOV	DX,GRPADD	;address graphics controller
;the next 2 statements appear to be unnecessary, but I'm not totally sure.
;	MOV	AX,RWMREG	;r/w mode, [ah] = 0
;	OutWord 		;non color compare read
	shl	bh,1		;plane 0 = maps 0/1, plane 1 = maps 2/3
	mov	BasePlane,bh
	call	Read_64K	;preload hi byte
	mov	ah,al		;  to ah
NRdLoopX:
	call	Read_64K	;fill ax word with video bytes
	mov	bh,al		;this lo byte will become next hi byte
	rol	ax,cl		;align to array
	sub	bp,8		;8 bits done
	jbe	NRdLastX	;go if bit count exhausted
	mov	es:[di],ah	;save full byte
	inc	di		
	mov	ah,bh		;move lo byte (BH) to hi byte (AH)
	jnz	NRdLoopX	;loop if no offset overflow
	call	B$BumpES	;move array pointer over segment boundary
	jmp	short NRdLoopX	;go do another
NRdLastX:
	and	ah,ch		;strip unused bits from last byte
	mov	es:[di],ah	;save last byte
	inc	di		
	jnz	NRdDoneX	
	call	B$BumpES	;move array pointer over segment boundary
NRdDoneX:
cEnd

;***
; Write_64K
;
;Purpose:
;	Support routine for NWriteL_64K, writes one byte to screen
;	memory from AL.  Initializes EGA regs to appropriate plane and
;	vectors through [b$PutVector] which writes the byte after
;	applying any bitwise logic necessary.
;Entry:
;	ES:DI = screen address
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	Write_64K
cProc	Write_64K,<NEAR>,<AX,DX>
cBegin
	push	ax
	MOV	DX,GRPADD	;address graphics controller
	mov	al,RMPREG	;select read map select register
	out	dx,al
	inc	dx
	xor	al,al		;for plane computation
	ror	di,1		;carry = 1 if odd address
	adc	al,BasePlane	;base plane +1 iff odd address
	rol	di,1		;restore address
	out	dx,al		;set plane to read
	MOV	DX,SEQADD	;address the sequencer
	MOV	AL,MMREG	;  map mask register
	out	dx,al
	inc	dx
	mov	al,b$PlaneMask ;get base plane mask
	and	al,MapMask	;with even/odd map mask
	and	al,0FH		;strip to nibble
	out	dx,al		;set plane to write
	rol	MapMask,1	;rotate even/odd mask for next byte
	pop	ax
.erre	ID_SSEQDS		;assumes ss = ds
	call	ss:[b$PutVector]   ;put the byte (finally!!)
cEnd

;***
; NWriteL_64K
;
;Purpose:
;	Write a line of pixels from an array to a specified plane for
;	64K Screen mode 9 (odd/even color mode).
;Entry:
;	ES:DI = screen address
;	DS:SI = array address
;	CX    = array align shift count
;	BP    = count of bits to write
;	BH    = plane to write to
;	DL    = last partial byte mask
;	DH    = first partial byte mask
;Exit:
;	DS:SI = updated to array byte past point used
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	NWriteL_64K
cProc	NWriteL_64K,<NEAR>
cBegin
	rol	b$PlaneMask,1	;shift to next plane
	rol	b$PlaneMask,1
	mov	MapMask,01010101B	;setup map mask for even access
	test	di,1		;is it even?
	jz	IsEvenX 	;go if so
	rol	MapMask,1
IsEvenX:
	shl	bh,1		;plane 0 = maps 0/1, plane 1 = maps 2/3
	mov	BasePlane,bh
	push	dx
	mov	ah,dh		;first byte bit mask
	MOV	DX,GRPADD	;address graphics controller
	mov	al,BMKREG	;  bit mask register
	EGAINT10CLI		;disable ints if using EGAINT10 interface
	OutWord 		;set first partial byte mask
	pop	dx

	mov	ah,[si] 	;preload byte from array
	inc	si
	jnz	NWrOvfl1X	
	call	B$BumpDS	;move array pointer over segment boundary
NWrOvfl1X:
	ror	ax,cl		;align to video
	add	bp,cx
	sub	bp,8		;account for first partial byte
	jbe	NWrLastX	;go if last byte
	call	Write_64K
	mov	dh,0FFH 	;mask for whole bytes in the middle
	push	ax
	push	dx
	mov	ah,dh		;middle byte bit mask
	MOV	DX,GRPADD	;address graphics controller
	mov	al,BMKREG	;  bit mask register
	OutWord 		;set full byte mask for middle bytes
	EGAINT10STI		;reenable ints if using EGAINT10 interface
	pop	dx
	pop	ax
	jmp	short NWrLoopX2
NWrLoopX:
.erre	ID_SSEQDS		;assumes ss = ds
	EGAINT10CLI		;disable ints if using EGAINT10 interface
	call	Write_64K	;put the byte
	EGAINT10STI		;reenable ints if using EGAINT10 interface
NWrLoopX2:
	rol	ax,cl		;re-align to array
	xchg	ah,al
	cmp	cx,bp		;enough bits in this byte to finish
	jae	NWrOvfl2X	;go if so, don't load another
	mov	ah,[si] 	;fill ax word with array bytes
	inc	si
	jnz	NWrOvfl2X	
	call	B$BumpDS	;move array pointer over segment boundary
NWrOvfl2X:
	ror	ax,cl		;align to video
	sub	bp,8		;8 bits done
	ja	NWrLoopX	;go if bit count not exhausted
NWrLastX:
	push	ax
	and	dh,dl		;combine first|middle mask with end mask
	mov	ah,dh		;last byte bit mask
	MOV	DX,GRPADD	;address graphics controller
	mov	al,BMKREG	;  bit mask register
	EGAINT10CLI		;disable ints if using EGAINT10 interface
	OutWord 		;set first partial byte mask
	pop	ax
.erre	ID_SSEQDS		;assumes ss = ds
	call	Write_64K	;put the last byte
	EGAINT10STI		;reenable ints if using EGAINT10 interface
cEnd

	ASSUME	DS:DGROUP

;***
; B$xINITEGA - initialize EGA modes
;
;Purpose:
;	Added with revision [26].
;	Put the addresses of EGA screen mode support routines into the
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
cProc	B$xINITEGA,<FAR,PUBLIC> 
cBegin
	MOV	WORD PTR [b$ScreenTab + (7*2) + 1],OFFSET B$Screen7
	MOV	WORD PTR [b$ScreenTab + (8*2) + 1],OFFSET B$Screen8
	MOV	WORD PTR [b$ScreenTab + (9*2) + 1],OFFSET B$Screen9
	MOV	WORD PTR [b$ScreenTab + (10*2)+ 1],OFFSET B$Screen10
cEnd

sEnd	GR_TEXT

	END
