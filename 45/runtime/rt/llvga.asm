	TITLE		LLVGA - VGA screen mode support
;***
;LLVGA - VGA screen mode support
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Support for VGA graphics screen modes (BIOS 11,12,13).
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
; 11 x 80 30 S	x | 11	 2 256K 640 480  64 8x16  1  1 1
; 11 x 80 30 I	x |  "   "   "   "   "    "  "    "  " "
; 11 x 80 60 S	x |  "   "   "   "   "    " 8x8   "  " "
; 11 x 80 60 I	x |  "   "   "   "   "    "  "    "  " "
;
; 12 x 80 30 S	x | 12	16 256K 640 480 256 8x16  1  1 4
; 12 x 80 60 S	x |  "   "   "   "   "    " 8x8   "  " "
;
; 13 x 40 25 S	x | 13 256 256K 320 200  64 8x8   1  8 1
; 13 x 40 25 I	x |  "   "   "   "   "    "  "    "  " "
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
	INCLUDE baslibma.inc
	INCLUDE llgrp.inc
	INCLUDE idmac.inc
	INCLUDE grmac.inc	;ModeData macros

	INITIALIZER B$xINITVGA	;Put B$xINITVGA in initializer list

sBegin	_BSS
;
; ***************************************************************************
; External function vectors
; ***************************************************************************
;
externW b$PalTrans
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$BiosMode
externB b$Adapter
externB b$Monitor
externB b$VGAmodesH		; hardware support for BIOS modes 10h-13h
externW b$CurPages
externW b$VideoBase
externB b$MaskC
externB b$AttrC
externD b$AddrC
externW b$OffC
externW b$SegC
externB b$MaxAttr
externW B$VLOFST
externW B$VROFST
externW B$LEOFST
externW B$REOFST
externB b$ForeColor
externB b$ScrHeight
externW b$PutVector
externB b$PaintBorder
externW b$Incr1
externW b$Incr2
externW b$IncrY
externB b$ForeMapped		
externW b$CURSOR		
externW b$CSRTYP		
externB b$EgaPalSup
externW b$SaveCa

externW b$UpSub 		
externW b$DnSub 		
externW b$UpDnAdd		
externW b$ScreenTab		

;
; ***************************************************************************
; Local variables
; ***************************************************************************
;
sEnd	_BSS

sBegin	CN_TEXT
externNP B$USRCSR		
sEnd	CN_TEXT

assumes CS,GR_TEXT
sBegin	GR_TEXT

externNP B$ErrorReturn
externNP B$BumpDS
externNP B$BumpES		
externNP B$InitModeData
externNP B$GetParm
externNP B$ResetEGA
externNP B$EgaPalSet
externNP B$EgaMapXYC
externNP B$EgaSetAttr
externNP B$EgaSetC
externNP B$EgaSetPixC
externNP B$EgaSetPixFirstC
externNP B$EgaLineX
externNP B$EgaLineY
externNP B$EgaLineV
externNP B$EgaNSetC
externNP B$EgaDownC
externNP B$EgaLeftC
externNP B$EgaLeftC_13
externNP B$EgaChkUpC
externNP B$EgaReadC
externNP B$EgaPutAction
externNP B$EgaNReadL
externNP B$EgaNWriteL
externNP B$EgaPaintBound
externNP B$EgaPaintBound_11
externNP B$EgaSetTile
externNP B$EgaScanL
externNP B$EgaScanR
externNP B$EgaUpC
externNP B$EgaChkDownC
externNP B$CgaSetC
externNP B$CgaSetAttr
externNP B$CgaSetPixC
externNP B$CgaSetPixFirstC
externNP B$CgaSetPixLastC
externNP B$CgaNSetC
externNP B$CgaReadC
externNP B$CgaPutAction
externNP B$CgaNReadL
externNP B$CgaNWriteL
externNP B$CgaSetTile
externNP B$CgaScanL
externNP B$CgaScanR
externNP B$CgaLineX
externNP B$CgaLineY
externNP B$CgaLineV

;===========================================================================
mModeData	Mode11Data
;
; SCREEN 11, BIOS mode 11
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	11
mBiosMode	11H
mBurst		0
mScrWidth	80
mScrHeight	30
mHorzRes	640
mVertRes	480
mVideoBase	0A000H
mMaxAttr	1
mMaxColor	-1		    ;maximum color  (unused for this mode)
mPageSize	64		    ;page size in K
mCurrPSize	<(64*1024) shr 4>   ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	1
mBackColor	0
mEgaWrMd	0
mInitPalette	b$VgaPalette
mInitVgaPal	b$VgaPalette
mAlphaDim	AlphaDim_11_12
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$VgaPalReset
mPalPut 	B$VgaPalPut
mPalTrans	B$VgaPalTrans11
mPalSet 	PalSet
mSetColor	B$ErrorReturn
mForeMapped	1
mBitsPerPixel	1
mPlanes 	1
mMapXYC 	B$EgaMapXYC
mLeftC		B$EgaLeftC
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
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
mPaintBound	B$EgaPaintBound_11
mSetTile	B$CgaSetTile
mScanL		B$CgaScanL
mScanR		B$CgaScanR
mEnd		GraphDataLen
;===========================================================================

;===========================================================================
mModeData	Mode12Data
;
; SCREEN 12, BIOS mode 12
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	12
mBiosMode	12H
mBurst		0
mScrWidth	80
mScrHeight	30
mHorzRes	640
mVertRes	480
mVideoBase	0A000H
mMaxAttr	15
mMaxColor	-1			;maximum color (unused for this mode)
mPageSize	-1			;page size in K (unused for this mode)
mCurrPSize	<(256/4*1024) shr 4>	;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	15
mBackColor	0
mEgaWrMd	2
mInitPalette	b$VgaPalette
mInitVgaPal	b$VgaPalette
mAlphaDim	AlphaDim_11_12
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$VgaPalReset
mPalPut 	B$VgaPalPut
mPalTrans	PalTrans_12
mPalSet 	PalSet
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
mModeData	Mode13Data
;
; SCREEN 13, BIOS mode 13
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	13
mBiosMode	13H
mBurst		0
mScrWidth	40
mScrHeight	25
mHorzRes	320
mVertRes	200
mVideoBase	0A000H
mMaxAttr	255
mMaxColor	-1		    ;maximum color (unused for this mode)
mPageSize	64		    ;page size in K
mCurrPSize	<(64*1024) shr 4>   ;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	0
mForeColor	15
mBackColor	0
mEgaWrMd	2
mInitPalette	b$VgaPalette
mInitVgaPal	b$VgaPalette
mAlphaDim	AlphaDim_13
mSetMode	SetMode
mSetPages	SetPages
mPalReset	PalReset_13
mPalPut 	B$VgaPalPut
mPalTrans	PalTrans_13
mPalSet 	PalSet
mSetColor	SetColor
mForeMapped	15
mBitsPerPixel	8
mPlanes 	1
mMapXYC 	MapXYC_13
mLeftC		B$EgaLeftC_13
mChkUpC 	B$EgaChkUpC
mUpC		B$EgaUpC
mChkDownC	B$EgaChkDownC
mDownC		B$EgaDownC
mSetAttr	SetAttr_13
mReadC		ReadC_13
mSetC		SetC_13
mSetPixC	SetPixC_13
mSetPixFirstC	B$CgaSetPixFirstC
mSetPixLastC	B$CgaSetPixLastC
mLineX		LineX_13
mLineY		LineY_13
mLineV		LineV_13
mPutAction	PutAction_13
mNReadL 	NReadL_13
mNWriteL	NWriteL_13
mNSetC		NSetC_13
mPaintBound	PaintBound_13
mSetTile	B$CgaSetTile
mScanL		ScanL_13
mScanR		ScanR_13
mEnd		GraphDataLen
;===========================================================================
;
; Mode11Palette/Mode12Palette
;   These default palettes are used in VGA modes 11H & 12H (screen 11 & 12)
;   to make the EGA palette transparent.  EGA palette entries 0-16 (includes
;   overscan) are mapped directly to the VGA palette.  Any palette
;   manipulations are then done only to the VGA palette.
;
labelB	Mode11Palette		;[24] designed to also work on certain clones
	DB	0,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,0 ;[24]
;
labelB	Mode12Palette		;[24]
	DB	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16	;[24]

;
; b$VgaPalette
;   is identical to that setup by the BIOS for mode 13H (screen 13).  The
;   first 17 entries happen to match what we want for modes 11H & 12H as
;   well.  This includes entry #16 which is black for overscan.
;   NOTE: It would be nice to find a way to avoid keeping such a huge
;   table around, even in CS!
;

labelB	b$VgaPalette
	;	RED GREEN BLUE
	;	--- ----- ----
	DB	00H, 00H, 00H
	DB	00H, 00H, 2AH
	DB	00H, 2AH, 00H
	DB	00H, 2AH, 2AH
	DB	2AH, 00H, 00H
	DB	2AH, 00H, 2AH
	DB	2AH, 15H, 00H
	DB	2AH, 2AH, 2AH
	DB	15H, 15H, 15H
	DB	15H, 15H, 3FH
	DB	15H, 3FH, 15H
	DB	15H, 3FH, 3FH
	DB	3FH, 15H, 15H
	DB	3FH, 15H, 3FH
	DB	3FH, 3FH, 15H
	DB	3FH, 3FH, 3FH
	DB	00H, 00H, 00H
	DB	05H, 05H, 05H
	DB	08H, 08H, 08H
	DB	0BH, 0BH, 0BH
	DB	0EH, 0EH, 0EH
	DB	11H, 11H, 11H
	DB	14H, 14H, 14H
	DB	18H, 18H, 18H
	DB	1CH, 1CH, 1CH
	DB	20H, 20H, 20H
	DB	24H, 24H, 24H
	DB	28H, 28H, 28H
	DB	2DH, 2DH, 2DH
	DB	32H, 32H, 32H
	DB	38H, 38H, 38H
	DB	3FH, 3FH, 3FH
	DB	00H, 00H, 3FH
	DB	10H, 00H, 3FH
	DB	1FH, 00H, 3FH
	DB	2FH, 00H, 3FH
	DB	3FH, 00H, 3FH
	DB	3FH, 00H, 2FH
	DB	3FH, 00H, 1FH
	DB	3FH, 00H, 10H
	DB	3FH, 00H, 00H
	DB	3FH, 10H, 00H
	DB	3FH, 1FH, 00H
	DB	3FH, 2FH, 00H
	DB	3FH, 3FH, 00H
	DB	2FH, 3FH, 00H
	DB	1FH, 3FH, 00H
	DB	10H, 3FH, 00H
	DB	00H, 3FH, 00H
	DB	00H, 3FH, 10H
	DB	00H, 3FH, 1FH
	DB	00H, 3FH, 2FH
	DB	00H, 3FH, 3FH
	DB	00H, 2FH, 3FH
	DB	00H, 1FH, 3FH
	DB	00H, 10H, 3FH
	DB	1FH, 1FH, 3FH
	DB	27H, 1FH, 3FH
	DB	2FH, 1FH, 3FH
	DB	37H, 1FH, 3FH
	DB	3FH, 1FH, 3FH
	DB	3FH, 1FH, 37H
	DB	3FH, 1FH, 2FH
	DB	3FH, 1FH, 27H
	DB	3FH, 1FH, 1FH
	DB	3FH, 27H, 1FH
	DB	3FH, 2FH, 1FH
	DB	3FH, 37H, 1FH
	DB	3FH, 3FH, 1FH
	DB	37H, 3FH, 1FH
	DB	2FH, 3FH, 1FH
	DB	27H, 3FH, 1FH
	DB	1FH, 3FH, 1FH
	DB	1FH, 3FH, 27H
	DB	1FH, 3FH, 2FH
	DB	1FH, 3FH, 37H
	DB	1FH, 3FH, 3FH
	DB	1FH, 37H, 3FH
	DB	1FH, 2FH, 3FH
	DB	1FH, 27H, 3FH
	DB	2DH, 2DH, 3FH
	DB	31H, 2DH, 3FH
	DB	36H, 2DH, 3FH
	DB	3AH, 2DH, 3FH
	DB	3FH, 2DH, 3FH
	DB	3FH, 2DH, 3AH
	DB	3FH, 2DH, 36H
	DB	3FH, 2DH, 31H
	DB	3FH, 2DH, 2DH
	DB	3FH, 31H, 2DH
	DB	3FH, 36H, 2DH
	DB	3FH, 3AH, 2DH
	DB	3FH, 3FH, 2DH
	DB	3AH, 3FH, 2DH
	DB	36H, 3FH, 2DH
	DB	31H, 3FH, 2DH
	DB	2DH, 3FH, 2DH
	DB	2DH, 3FH, 31H
	DB	2DH, 3FH, 36H
	DB	2DH, 3FH, 3AH
	DB	2DH, 3FH, 3FH
	DB	2DH, 3AH, 3FH
	DB	2DH, 36H, 3FH
	DB	2DH, 31H, 3FH
	DB	00H, 00H, 1CH
	DB	07H, 00H, 1CH
	DB	0EH, 00H, 1CH
	DB	15H, 00H, 1CH
	DB	1CH, 00H, 1CH
	DB	1CH, 00H, 15H
	DB	1CH, 00H, 0EH
	DB	1CH, 00H, 07H
	DB	1CH, 00H, 00H
	DB	1CH, 07H, 00H
	DB	1CH, 0EH, 00H
	DB	1CH, 15H, 00H
	DB	1CH, 1CH, 00H
	DB	15H, 1CH, 00H
	DB	0EH, 1CH, 00H
	DB	07H, 1CH, 00H
	DB	00H, 1CH, 00H
	DB	00H, 1CH, 07H
	DB	00H, 1CH, 0EH
	DB	00H, 1CH, 15H
	DB	00H, 1CH, 1CH
	DB	00H, 15H, 1CH
	DB	00H, 0EH, 1CH
	DB	00H, 07H, 1CH
	DB	0EH, 0EH, 1CH
	DB	11H, 0EH, 1CH
	DB	15H, 0EH, 1CH
	DB	18H, 0EH, 1CH
	DB	1CH, 0EH, 1CH
	DB	1CH, 0EH, 18H
	DB	1CH, 0EH, 15H
	DB	1CH, 0EH, 11H
	DB	1CH, 0EH, 0EH
	DB	1CH, 11H, 0EH
	DB	1CH, 15H, 0EH
	DB	1CH, 18H, 0EH
	DB	1CH, 1CH, 0EH
	DB	18H, 1CH, 0EH
	DB	15H, 1CH, 0EH
	DB	11H, 1CH, 0EH
	DB	0EH, 1CH, 0EH
	DB	0EH, 1CH, 11H
	DB	0EH, 1CH, 15H
	DB	0EH, 1CH, 18H
	DB	0EH, 1CH, 1CH
	DB	0EH, 18H, 1CH
	DB	0EH, 15H, 1CH
	DB	0EH, 11H, 1CH
	DB	14H, 14H, 1CH
	DB	16H, 14H, 1CH
	DB	18H, 14H, 1CH
	DB	1AH, 14H, 1CH
	DB	1CH, 14H, 1CH
	DB	1CH, 14H, 1AH
	DB	1CH, 14H, 18H
	DB	1CH, 14H, 16H
	DB	1CH, 14H, 14H
	DB	1CH, 16H, 14H
	DB	1CH, 18H, 14H
	DB	1CH, 1AH, 14H
	DB	1CH, 1CH, 14H
	DB	1AH, 1CH, 14H
	DB	18H, 1CH, 14H
	DB	16H, 1CH, 14H
	DB	14H, 1CH, 14H
	DB	14H, 1CH, 16H
	DB	14H, 1CH, 18H
	DB	14H, 1CH, 1AH
	DB	14H, 1CH, 1CH
	DB	14H, 1AH, 1CH
	DB	14H, 18H, 1CH
	DB	14H, 16H, 1CH
	DB	00H, 00H, 10H
	DB	04H, 00H, 10H
	DB	08H, 00H, 10H
	DB	0CH, 00H, 10H
	DB	10H, 00H, 10H
	DB	10H, 00H, 0CH
	DB	10H, 00H, 08H
	DB	10H, 00H, 04H
	DB	10H, 00H, 00H
	DB	10H, 04H, 00H
	DB	10H, 08H, 00H
	DB	10H, 0CH, 00H
	DB	10H, 10H, 00H
	DB	0CH, 10H, 00H
	DB	08H, 10H, 00H
	DB	04H, 10H, 00H
	DB	00H, 10H, 00H
	DB	00H, 10H, 04H
	DB	00H, 10H, 08H
	DB	00H, 10H, 0CH
	DB	00H, 10H, 10H
	DB	00H, 0CH, 10H
	DB	00H, 08H, 10H
	DB	00H, 04H, 10H
	DB	08H, 08H, 10H
	DB	0AH, 08H, 10H
	DB	0CH, 08H, 10H
	DB	0EH, 08H, 10H
	DB	10H, 08H, 10H
	DB	10H, 08H, 0EH
	DB	10H, 08H, 0CH
	DB	10H, 08H, 0AH
	DB	10H, 08H, 08H
	DB	10H, 0AH, 08H
	DB	10H, 0CH, 08H
	DB	10H, 0EH, 08H
	DB	10H, 10H, 08H
	DB	0EH, 10H, 08H
	DB	0CH, 10H, 08H
	DB	0AH, 10H, 08H
	DB	08H, 10H, 08H
	DB	08H, 10H, 0AH
	DB	08H, 10H, 0CH
	DB	08H, 10H, 0EH
	DB	08H, 10H, 10H
	DB	08H, 0EH, 10H
	DB	08H, 0CH, 10H
	DB	08H, 0AH, 10H
	DB	0BH, 0BH, 10H
	DB	0CH, 0BH, 10H
	DB	0DH, 0BH, 10H
	DB	0FH, 0BH, 10H
	DB	10H, 0BH, 10H
	DB	10H, 0BH, 0FH
	DB	10H, 0BH, 0DH
	DB	10H, 0BH, 0CH
	DB	10H, 0BH, 0BH
	DB	10H, 0CH, 0BH
	DB	10H, 0DH, 0BH
	DB	10H, 0FH, 0BH
	DB	10H, 10H, 0BH
	DB	0FH, 10H, 0BH
	DB	0DH, 10H, 0BH
	DB	0CH, 10H, 0BH
	DB	0BH, 10H, 0BH
	DB	0BH, 10H, 0CH
	DB	0BH, 10H, 0DH
	DB	0BH, 10H, 0FH
	DB	0BH, 10H, 10H
	DB	0BH, 0FH, 10H
	DB	0BH, 0DH, 10H
	DB	0BH, 0CH, 10H
	DB	00H, 00H, 00H
	DB	00H, 00H, 00H
	DB	00H, 00H, 00H
	DB	00H, 00H, 00H
	DB	00H, 00H, 00H
	DB	00H, 00H, 00H
	DB	00H, 00H, 00H
	DB	00H, 00H, 00H

labelNP <PUBLIC,B$VGAUSED>	

;***
; B$Screen11
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 11.
;Entry:
;	AL = screen mode (11)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen11,<PUBLIC,NEAR>
cBegin
	MOV	BX,GR_TEXTOFFSET Mode11Data  ;mode-specific data
	TEST	b$Monitor,AnalogColor ;must be analog supporting color
	JZ	ScrErr		;exit w/error if not
	TEST	[b$VGAmodesH],VGAmode11h ; hardware supports BIOS mode 11h? 
	JZ	ScrErr		; exit w/error if not
	;set up variables for shared CGA/HGC line code
	mov	b$UpSub,80	;subtract 80 to move up a line
	mov	b$DnSub,-80	;subtract -80 (add 80) to move down a line
	mov	b$UpDnAdd,0	;no correction necessary
	JMP	SHORT ScrCommon ;common routine
cEnd	<nogen>

;***
; B$Screen12
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 12.
;Entry:
;	AL = screen mode (12)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen12,<PUBLIC,NEAR>
cBegin
	MOV	BX,GR_TEXTOFFSET Mode12Data  ;mode-specific data
	TEST	b$Monitor,AnalogColor ;must be analog supporting color
	JZ	ScrErr		;exit w/error if not
;	TEST	b$Adapter,VGA	;must be VGA
	TEST	[b$VGAmodesH],VGAmode12h ; hardware supports BIOS mode 12h? 
	JZ	ScrErr		;exit w/error if not
ScrCommon:
	MOV	CX,GraphDataLen 
	CALL	B$InitModeData ;initialize table data
	CLC			;indicate no error
cEnd

;*** 
; B$Screen13
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 13.
;Entry:
;	AL = screen mode (13)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen13,<PUBLIC,NEAR>
cBegin
	MOV	BX,GR_TEXTOFFSET Mode13Data  ;mode-specific data
	TEST	b$Monitor,AnalogColor ;must be analog supporting color
	JZ	ScrErr		; exit w/error if not
	TEST	[b$VGAmodesH],VGAmode13h ; hardware supports BIOS mode 13h? 
	JNZ	ScrCommon	;out through common exit if ok
SCrErr:
	STC			;signal error and exit
cEnd

;***
; AlphaDim_11_12
;
;Purpose:
;	Validate the proposed text dimensions for Screen 11 or Screen 12.
;	  If 80x30 or 80x60 is requested, this mode satisfies the request
;	  else suggest screen mode 0.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	AL = -1 if this mode satisfies the request, otherwise
;		AL is suggested screen mode to invoke for desired dimensions
;	b$ScrHeight set to value in BH if this mode satisfies request
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	AlphaDim_11_12
cProc	AlphaDim_11_12,<NEAR>
cBegin
	MOV	AL,-1		;flag request satisfied (maybe)
	CMP	BX,80+30*256	;80x30?
	JE	ADimSet 	;exit if so, standard stuff
	CMP	BX,80+60*256	;80x60?
	JE	ADimSet 	;exit if so, standard stuff
	XOR	AL,AL		;flag request for screen 0
	JMP	SHORT ADimOk	;  and exit
cEnd	<nogen>

;***
; AlphaDim_13
;
;Purpose:
;	Validate the proposed text dimensions for Screen 13.
;	If 40x25 is requested, this mode satisfies the request
;	else suggest screen mode 0.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	AL = -1 if this mode satisfies the request, otherwise
;		AL is suggested screen mode to invoke for desired dimensions
;	b$ScrHeight set to value in BH if this mode satisfies request
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	AlphaDim_13
cProc	AlphaDim_13,<NEAR>
cBegin
	MOV	AL,-1		;flag request satisfied (maybe)
	CMP	BX,40+25*256	;40x25?
	JE	ADimSet 	;exit if so, standard stuff
	XOR	AL,AL		;flag request for screen 0
	JMP	SHORT ADimOk	;  and exit

ADimSet:
	mov	b$ScrHeight,bh ;set alpha rows
ADimOk:
	clc			;no error
cEnd

;***
; SetMode
;
;Purpose:
;	Set VGA screen mode according to the characteristics established
;	by previous call to B$Screenx and b$AlphaDim.  Set 8x8 character
;	font if new mode is to be 60 lines.
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
	MOV	AL,b$BiosMode	;set BIOS mode
	SCNIO	vSetMode
	CMP	b$ScrHeight,60 ;60 lines?
	JNE	NormalHeight
	MOV	DL,60		; char gen call wants # of lines in DL
	MOV	AX,1123H	;character generator request
	XOR	BL,BL		;  to load 8x8 font
	SCNIO			;  which gets 60 lines
NormalHeight:
	MOV	DX,b$CURSOR	; Get current cursor position
	MOV	BYTE PTR b$CSRTYP,-1	; invalidate present cursor type so
				; it will get changed
	CALL	B$USRCSR	; display user cursor
cEnd

;***
; SetPages
;
;Purpose:
;	Set the current active and visual pages and video segment for
;	Screen modes 11, 12, and 13.
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
cProc	SetPages,<NEAR>
cBegin
DbAssertRel	AX,E,0,GR_TEXT,<Non-zero page requested in VGA SetPages (LLVGA)>
	mov	b$CurPages,ax	;save page numbers
	mov	ax,b$VideoBase ;set video segment
	mov	b$SegC,ax
cEnd

;***
; SetColor
;Purpose:
;	Process the color statement for Bios mode 12h-13h (BASIC Screen
;	modes 12-13).  Syntax for Screen 12-13 color statement is as follows:
;
;		COLOR	[foreground]
;
;	where "foreground" is the attribute to be used for the foreground
;
;Entry:
;	parameter list
;Exit:
;	PSW.C set error, reset if Ok.
;	b$ForeColor is set to foreground attribute
;	b$ForeMapped is set to foreground attribute mapped to internal value
;
;Uses
;	per conv.
;Exceptions:
;***************************************************************************
cProc	SetColor,<NEAR>
cBegin
DbAssertRelB	b$ForeColor,E,b$ForeMapped,GR_TEXT,<b$ForeColor NE b$ForeMapped in SetColor (LLVGA)>
	cCall	B$GetParm	;AL=foreground parameter
	MOV	BH,b$ForeColor ;use old values as default
	JZ	GotFore 	;go if none supplied
	CMP	AL,b$MaxAttr	;legal attribute?
	JA	ColorError	;branch if out of range
	MOV	BH,AL		;attribute to BH
GotFore:
	MOV	b$ForeColor,BH ;save foreground values
	MOV	b$ForeMapped,BH
	CLC			;indicate no error
	JCXZ	SetColDun	;if we got all params, thats true
ColorError:
	STC			;otherwise set error
SetColDun:
cEnd

;***
; PalReset_13
;
;Purpose:
;	Reset the PALETTE to the initial, default colors for Screen 13.
;Entry:
;	None
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;	exits through B$VgaPalReset common exit
;******************************************************************************
DbPub	PalReset_13
cProc	PalReset_13,<NEAR>,<ES>
cBegin
	push	cs
	pop	es
	mov	cx,256		;set 256 color palette registers
	jmp	short PalResetCommon
cEnd	<nogen>

;***
; B$VgaPalReset
;
;Purpose:
;	Reset the PALETTE to the initial, default colors for Screen 11
;	or Screen 12.  If EGA palette is supported, initialize it to
;	be map directly to the VGA DAC so it is transparent to the user.
;Entry:
;	None
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$VgaPalReset,<NEAR>,<ES>  
cBegin
	push	cs
	pop	es
	cmp	b$EgaPalSup,0	;support for EGA PALETTE?
	jz	NoEgaPal	;go if not
	mov	al,10H		;sub-function 10H, set VGA palette entry
	mov	bx,16		;VGA palette index to be used for overscan
	xor	dx,dx		;clear color to black
	xor	cx,cx		
	SCNIO	vSetEgaPalette	;set the overscan register in VGA palette
	;if there is an EGA palette we need to make it transparent
	mov	dx,GR_TEXTOFFSET Mode11Palette
	cmp	[b$biosMode], 11h ;[16] correct bios mode for this table?
	je	PalOk		;[16] brif so
	mov	dx, GR_TEXTOFFSET Mode12Palette ;[16] else use Mode 12 table
PalOk:

	mov	al,2		;BIOS sub-function - set all palette registers
				;and the overscan register (for border color)
	SCNIO	vSetEgaPalette	;set the palette
NoEgaPal:
	mov	cx,16		;set 16 color palette registers
PalResetCommon:
	xor	bx,bx		;starting at register 0
	mov	dx,GR_TEXTOFFSET b$VgaPalette
	mov	al,12H		;BIOS sub-function - set block of VGA DAC
				;palette registers
	SCNIO	vSetEgaPalette	;set the palette
cEnd

;***
; B$VgaPalPut
;
;Purpose:
;	Change palette entry for VGA modes with translation/verification.
;	A color value of -1 indicates that the associated palette entry
;	is not to be modified.
;Entry:
;	AX = color
;	BL = attribute
;Exit:
;	PSW.C reset indicates successful operation
;		set indicates PALETTE function call error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$VgaPalPut,<NEAR>	
cBegin
	cmp	ax,-1		;lo word of color == -1?
	jne	PalPut1 	;go if not, can't ignore
	cmp	dx,ax		;hi word too?
	je	PalPutExit	;exit if color == -1
PalPut1:
	call	[b$PalTrans]	;translate to external form
	jc	PalPutExit	;exit if invalid w/error
	mov	bh,dh		;bx=attribute (bh=0)
	xchg	dh,al		;dh=red
	xchg	ch,ah		;ch=green
	xchg	cl,dl		;cl=blue
	mov	al,10H		;subfunction "Set Individual Color Register"
				;  for VGA DAC color palette
	SCNIO	vSetEgaPalette	;set VGA palette entry
	clc			;no error
PalPutExit:
cEnd

;***
; B$VgaPalTrans11/PalTrans_12/PalTrans_13
;
;Purpose:
;	Verify that user supplied attribute value and color value are
;	within the legal range for Screen 12 or Screen 13, depending
;	on entry point.
;	An attribute translation is required for mode 11.
;	It somehow always maps the 1-bit pixel to palette index 0FH
;	(which is bright-white on the VGA palette).  Thus to change the
;	color for attribute 1 we must manipulate entry 0FH of the VGA
;	palette.
;	NOTE: B$VgaPalTrans11 is also used by mode 6 (SCREEN 2) in QCG
;	      with an MCGA.
;Entry:
;	DX:AX = user supplied color value
;	BL    = user supplied attribute value
;Exit:
;	PSW.C set if illegal value, reset if Ok
;	DX:AX = unchanged
;	BL    = unchanged
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************

cProc	B$VgaPalTrans11,<PUBLIC,NEAR>
cBegin
	cmp	bl,1		;is legal attribute ?
	ja	PalTrErr	;error return
	neg	bl		;leave 0 alone
	and	bl,0Fh		;0=0 1=0F
	jmp	short PalTrExit
cEnd	<nogen>

DbPub	PalTrans_12
DbPub	PalTrans_13
cProc	PalTrans_12,<NEAR>
cBegin
	cmp	bl,15		;is legal attribute ?
	ja	PalTrErr	;error return

labelNP PalTrans_13		;any byte value for mode 13 is OK!!

PalTrExit:
	test	dx,0FFC0H	;only lo 6 bits of color hi word allowed
	jnz	PalTrErr	;error return
	test	ax,0C0C0H	;only lo 6 bits of bytes in lo word allowed
	jnz	PalTrErr	;error return
	clc			;no error
	ret
PalTrErr:
	stc			;indicate error
cEnd

;***
; PalSet
;
;Purpose:
;	Set the entire VGA palette for Screens 11/12/13 from an array where
;	an element value of -1 indicates the entry should be left alone.  All
;	entries are verified before any are set.
;
;	This routine doesn't really do any of the work; it just verifies that
;	the specified array is of I4 (long integers) and then passes off to
;	B$EgaPalSet which does the actual palette setting.
;Entry:
;	AX    = size of array in elements
;	ES:SI = address of array
;	CX    = size of each array element in bytes (2 or 4)
;Exit:
;	PSW.C set if array is not I4
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	PalSet
cProc	PalSet,<NEAR>
cBegin
	cmp	cx,4		;must be array of I4 (long) words
	jne	PalTrErr	;exit if not w/error
	jmp	B$EgaPalSet	;B$EgaPalSet uses b$PalTrans and b$PalPut
				;  for generality and works for us too
cEnd	<nogen>


;NOTE: b$MaskC should be set to 0FFH and left there!!!

;***
; MapXYC_13
;
;Purpose:
;	Map given X and Y coordinates to the graphics cursor for Screen 13.
;Entry:
;	CX = X coordinate
;	DX = Y coordinate
;Exit:
;	b$OffC, b$MaskC updated
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	MapXYC_13
cProc	MapXYC_13,<NEAR>
cBegin
	mov	ax,cx		;save x
	mov	bx,dx		;multiply y by 320 to compute row displacement
	shl	dx,1		;dx=2*Y
	shl	dx,1		;dx=4*Y
	add	dx,bx		;dx=5*Y
	mov	cl,6		
	shl	dx,cl		;dx=5*Y*64=320*Y
	add	ax,dx		;add x byte offset to y row address
	mov	b$OffC,ax	;save byte offset

	;NOTE:	b$MaskC should be set to 0FFH and left there for mode 13!!!
	;	But, some higher level code uses it for generality.

	mov	b$MaskC,0FFH
cEnd

;***
; SetAttr_13
;
;Purpose:
;	Set current attribute to user-supplied value for Screen 13.
;	No error checking needs to be done because the input value is
;	a byte and the legal range for Screen 13 is 0-255.
;Entry:
;	AL = attribute to set
;Exit:
;	b$AttrC set to user-supplied attribute
;Uses:
;	per conv
;Exceptions:
;******************************************************************************
DbPub	SetAttr_13
cProc	SetAttr_13,<NEAR>
cBegin
	mov	b$AttrC,al
	clc			;exit no error
cEnd

;NOTE:	LeftC, ChkUpC, UpC, ChkDownC, DownC, SetAttr use EGA
;NOTE:	SetColor use VGA

;***
; ReadC_13
;
;Purpose:
;	Return the attribute of the pixel defined by the current
;	graphics cursor.
;Entry:
;	b$AddrC specifies pixel to read
;Exit:
;	AL = attribute of pixel
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	ReadC_13
cProc	ReadC_13,<NEAR>,<DS>
cBegin
	lds	bx,b$AddrC	;get memory address of cursor
	mov	al,[bx] 	;return with attribute in [al]
cEnd

;***
; SetC_13
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to the
;	current attribute.
;Entry:
;	b$AddrC specifies pixel to set
;	b$AttrC = attribute to use
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	SetC_13
cProc	SetC_13,<NEAR>,<DS>
cBegin
	mov	al,b$AttrC	;[al] = attribute
	lds	bx,b$AddrC	;[BX] = cursor offset, [DS] = segment
	mov	[bx],al 	;set color value
cEnd

;***
; SetPixC_13
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to the
;	current attribute.  This is identical to SetC_13 except that
;	this routine assumes ES is set to video segment.
;Entry:
;	ES = video segment (set up by b$SetPixFirstC)
;	b$OffC specifies pixel to set
;	b$AttrC = attribute to use
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	SetPixC_13
cProc	SetPixC_13,<NEAR>
cBegin
	mov	al,b$AttrC	;[al] = attribute
	mov	bx,b$OffC	;[BX] = cursor offset
				;[ES] = setup by SetPixFirstC
	mov	es:[bx],al	;set color value
cEnd

;NOTE:	SetPixFirstC, SetPixLastC use CGA

;***
; LineX_13
;
;Purpose:
;	Draw an X-major line for Screen 13.
;Entry:
;	AH    = color (b$AttrC)
;	AL    = bit accumulator (0)
;	BX    = major axis delta update value (Incr1)
;	CX    = point count
;	DX    = BP change for Y movement
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr2 = minor axis delta update value
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	LineX_13
cProc	LineX_13,<NEAR>
cBegin
	mov	bx,b$Incr1	;to register here

Line13Xloop:

	ROL	DI,1		;next line style bit
	JNC	Line13X2	;go if bit is 0 not to plot

	mov	es:[bp],ah	;set this pixel
Line13X2:
	OR	SI,SI		;time to move in Y (+ or 0 delta)?
	JNS	Line13X4	;go if so
	ADD	SI,BX		;update delta for X movement
	INC	BP		;go to next byte
	loop	Line13Xloop
	ret
Line13X4:
	ADD	SI,b$Incr2	;update delta for Y movement
	inc	bp		;move to next X
	add	BP,DX		;move to next Y
	loop	Line13Xloop	;go for more
cEnd

;***
; LineY_13
;
;Purpose:
;	Draw a Y-major line for Screen 13.
;Entry:
;	AH    = color (b$AttrC)
;	BX    = major axis delta update value (Incr1)
;	CX    = point count
;	DX    = BP change for Y movement (swapped with IncrY)
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr2 = minor axis delta update value
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	LineY_13
cProc	LineY_13,<NEAR>
cBegin
	mov	bx,b$Incr1	;to register here

Line13Yloop:

	ROL	DI,1		;next line style bit
	JNC	Line13Y2	 ;go if bit is 0 not to plot

	mov	es:[bp],ah	;set this pixel
Line13Y2:
	OR	SI,SI		;time to move in X (+ or 0 delta)?
	JNS	Line13Y3	;go if so
	ADD	SI,BX		;update delta for Y movement
	ADD	BP,DX		;move to next Y
	loop	Line13Yloop
	ret
Line13Y3:
	ADD	SI,b$Incr2	;update delta for X movement
	inc	bp		;move to next X
	add	BP,DX		;move to next Y
	loop	Line13Yloop	;go for more
cEnd

;***
; LineV_13
;
;Purpose:
;	Draw a vertical line for Screen 13.
;Entry:
;	AH    = color (b$AttrC)
;	CX    = point count
;	DX    = BP change for Y movement
;	SI    = IncrY = BP change for Y movement
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	LineV_13
cProc	LineV_13,<NEAR>
cBegin
	MOV	SI,b$IncrY	;to register here

Line13Vloop:

	ROL	DI,1		;next line style bit
	JNC	Line13V2	;go if bit is 0 not to plot

	mov	es:[bp],ah	;set this pixel
Line13V2:
	ADD	BP,DX		;to next Y
	LOOP	Line13Vloop	;go for more
cEnd

labelW	PutTable_13		;Put Vectors according to put action value
	DW	PutOr_13, PutAnd_13, PutPreset_13, B$BumpDS ,PutXor_13 
	;B$BumpDS entry is signal for PSET to use faster movsb and
	;becomes the array segment bumper for NRWMove

;***
; PutAction_13
;
;Purpose:
;	Set b$PutVector to appropriate PUT action routine for Screen 13.
;	Requested action is used to index into a table of entry points.
;Entry:
;	AL = PUT action [0..4] representing (OR, AND, PRESET, PSET, XOR)
;Exit:
;	b$PutVector set to entry point of appropriate PUT action routine
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	PutAction_13
cProc	PutAction_13,<NEAR>
cBegin
	xor	ah,ah		;make word index
	shl	ax,1
	mov	bx,ax
	mov	ax,cs:PutTable_13[BX]	;get our vector
	mov	b$PutVector,ax ;save it
cEnd

	ASSUME	DS:NOTHING

;***
; NReadL_13
;
;Purpose:
;	Read a line of pixels from the screen to an array for Screen 13.
;Entry:
;	DS:SI = screen address
;	ES:DI = array address
;	BP    = count of bits (not pixels) to read
;Exit:
;	ES:DI = updated to array byte past point filled
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	NReadL_13
cProc	NReadL_13,<NEAR>
cBegin
	mov	cx,bp		;convert bit count to byte count in cx
	shr	cx,1
	shr	cx,1
	shr	cx,1

	mov	bx,GR_TEXTOFFSET B$BumpES  ;array segment bumper
	mov	dx,di		;dx = array offset
NRWMove:			
	neg	dx		;compute space remaining in array's segment
	cmp	cx,dx		;enough room?
	ja	NRdOvl		;go if not, segment will overflow
NRWNoOvl:			
    rep movsb			;move remainder
cEnd

NRdOvl: 			
	sub	cx,dx		;compute overflow count
	xchg	cx,dx		;use remaining segment space as move count
    rep movsb			;move to end of segment
	xchg	cx,dx		;use overflow count for second move
	;NOTE: only two moves are required since a MODE 13 screen is < 64K
	;	  and could only cross one segment boundary
	call	bx		;move array pointer over segment boundary
	jmp	short NRWNoOvl	

;***
; NWriteL_13
;
;Purpose:
;	Write a line of pixels from an array to the screen for Screen 13.
;Entry:
;	ES:DI = screen address
;	DS:SI = array address
;	BP    = count of bits (not pixels) to write
;Exit:
;	DS:SI = updated to array byte past point used
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	NWriteL_13
cProc	NWriteL_13,<NEAR>
cBegin
.erre	ID_SSEQDS		;assumes ss = ds
	mov	bx,ss:[b$PutVector]	;preload the put routine address
	mov	dx,si		;dx = array offset for NRWMove
	mov	cx,bp		;convert bit count to byte count in cx
	shr	cx,1
	shr	cx,1
	shr	cx,1
	cmp	bx,GR_TEXTOFFSET B$BumpDS  ;is this for PSET?
	jz	NRWMove 	;go if so
NWrLoop13:
	lodsb			;load byte from array
	or	si,si		;array pointer overflowed to next segment?
	jz	NWrOvfl2	;go if so
	jmp	bx		;put the byte
;
;The following Put routines perform the actual logical operation where
;	[AL]	= data value
;	[ES:DI] = screen address
;
;
; PutAnd_13
;
PutAnd_13:
	and	es:[di],al	;AND WITH SCREEN
	jmp	short PutEnd
;
; PutOr_13
;
PutOr_13:
	or	es:[di],al	;OR WITH SCREEN
	jmp	short PutEnd
;
; PutPreset_13
;
PutPreset_13:
	not	al		;NEGATE DATA FOR PRESET
	stosb			;set screen pixel
	jmp	short PutEnd2
;
; PutXor_13
;
PutXor_13:
	xor	es:[di],al	;XOR WITH SCREEN
PutEnd:
	inc	di
PutEnd2:
	loop	NWrLoop13
cEnd

NWrOvfl2:			
	call	B$BumpDS	;move array pointer over segment boundary
	jmp	bx		;put the byte

	ASSUME	DS:DGROUP

;***
; NSetC_13
;
;Purpose:
;	Set a horizontal line of pixels to the current attribute for
;	Screen 13.  The line starts at the current cursor position
;	and moves right.
;Entry:
;	b$AddrC specifies start pixel
;	b$AttrC specifies attribute to use
;	BX = pixel count
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	NSetC_13
cProc	NSetC_13,<NEAR>,<ES,DI>
cBegin
	les	di,b$AddrC	;graphics cursor address
	mov	al,b$AttrC	;al = attribute
	mov	cx,bx		;cx = pixel count
	rep	stosb		;block write full bytes
NSetExit:
cEnd

;***
; PaintBound_13
;
;Purpose:
;	Called by PAINT before painting each scan line to facilitate
;	fast viewport edge detection.  Set VIEW left and right cursor
;	addresses.
;Entry:
;	b$OffC specifies current cursor position
;Exit:
;	B$LEOFST = left edge offset
;	B$REOFST = right edge offset
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	PaintBound_13
cProc	PaintBound_13,<NEAR>
cBegin
	MOV	AX,b$OffC	 ;get cursor position

;  Compute the addr of the 1st pixel on the line by computing the
;  line number and then multiplying by BytesPerRow
;	LineNumber = INT(OffC/BytesPerRow)
;	first pixel in line = LineNumber * BytesPerRow

	xor	dx,dx
	MOV	cx,320		;LineNumber = INT(OffC/BytesPerRow)
	DIV	cx
	mov	bx,ax		;multiply y by 320 to compute row displacement
	shl	ax,1		;dx=2*Y
	shl	ax,1		;dx=4*Y
	add	ax,bx		;dx=5*Y
	mov	cl,6		
	shl	ax,cl		;dx=5*Y*64=320*Y
	mov	dx,ax

;  b$OffC addr of 1st pixel in current row is now in DX - compute boundries

	MOV	AX,B$VLOFST
	ADD	AX,DX
	MOV	B$LEOFST,AX	;Left margin= (x1,0)+b$OffC
	add	dX,B$VROFST
	MOV	B$REOFST,dx	;Right margin= (x2,0)+b$OffC
cEnd

;NOTE:	SetTile use CGA

;***
; ScanL_13
;
;Purpose:
;	For Screen 13, scan left beginning with the pixel to the left
;	of cursor, and paint pixels until:
;		(1) the viewport edge is encounteered (edge painted)
;		(2) a border pixel is encountered (border not painted)
;
;Entry:
;	b$AddrC       = pixel to right of starting pixel
;	b$PaintBorder = attribute of paint region border
;	b$AttrC       = attribute to paint
;	B$LEOFST        = left viewport edge
;Exit:
;	BX	       = number of pixels scanned
;	CL	       = 0 iff no pixels changed color
;	b$OffC        = the last non-border pixel examined/painted
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	ScanL_13
cProc	ScanL_13,<NEAR>,<DX,DI,ES>
cBegin
	les	di,b$AddrC	;di=cursor offset, es=video seg
	mov	dl,b$PaintBorder   ;dl=border attribute
	mov	dh,b$AttrC	;dh=paint attribute
	xor	cx,cx		;cx=clear already-painted-flag
	xor	bx,bx		;bx=clear paint count

;	The registers are set up and used as follows:
;	ES:DI = Video segment address (b$AddrC)
;	DL    = border attribute
;	DH    = paint attribute
;	CL    = already painted flag
;	AL    = screen byte
;	BX    = paint count

;	Scan left beginning with the pixel to the left of the cursor,
;	and paint pixels until:
;		1. edge of screen/VIEWPORT is encountered (edge painted)
;		2. a border pixel is encountered (border not painted)

SL13Loop:
	cmp	di,B$LEOFST	;are we at left viewport edge?
	je	SL13Edge	;exit if so, can't go left
	dec	di		;move left one byte
	mov	al,es:[di]	;get the screen byte
	cmp	al,dl		;screen byte == border?
	je	SL13Border	;go if border
	xor	al,dh		;AL = 0 if screen byte == paint attribute
	or	cl,al		;set the already-painted-flag
	inc	bx		;increment paint count
	jmp	short SL13Loop	;keep going
SL13Border:
	inc	di		;back up away from the border
SL13Edge:
	mov	b$OffC,di	;paint start point
	or	bx,bx		;any pixels to paint
	jz	SL13NoSet	;go if none
	push	bx		
	push	cx		
	call	NSetC_13	;paint 'em all at once (and tile)
	pop	cx		
	pop	bx		
SL13NoSet:			

;	Scan encountered a border pixel or viewport edge,
;	now exit with:
;		BX = number of pixels scanned
;		CL = 0 iff no pixels changed color

cEnd

;***
; ScanR_13
;
;Purpose:
;	For Screen 13, starting with the current pixel, search right until:
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
;Entry:
;	DX	       = count of border pixels which may be skipped
;	b$AddrC       = starting pixel
;	b$PaintBorder = attribute of paint region border
;	b$AttrC       = attribute to paint
;	B$REOFST        = right viewport edge
;Exit:
;	BX	       = number of pixels painted
;			    (whether or not they changed color)
;	CL	       = 0 iff no pixels changed color
;	DX	       = remaining border pixel count
;	b$OffC        = the last non-border pixel examined/painted
;	SI, AL	       = the first non-border pixel encountered
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
DbPub	ScanR_13
cProc	ScanR_13,<NEAR>,<DI,BP,ES>
cBegin
	mov	bx,dx		;bx=skip count
	les	di,b$AddrC	;di=cursor offset, es=video seg
	mov	dl,b$PaintBorder   ;dl=border attribute
	mov	dh,b$AttrC	;dh=paint attribute
	xor	cx,cx		;cl=clear already-painted-flag

;	The registers are set up as follows:
;	ES:DI = Video segment address (b$AddrC)
;	DL = border attribute
;	DH = paint attribute
;	CL = already painted flag
;	AL = screen byte

;	Starting with the current pixel, scan right until:
;		1. [BX] pixels have been tested
;		2. edge of screen/VIEWPORT is encountered
;		3. a non-BORDER pixel is found

SR13Loop1:
	mov	al,es:[di]	;get screen byte
	cmp	al,dl		;screen byte == border?
	jnz	SR13Paint	;go if not
	dec	bx		;decrement border count
	jz	SR13NoCnt	;brif border count zero
	inc	di		;move right one pixel
	cmp	di,B$REOFST	;past viewport edge?
	jbe	SR13Loop1	;go if not, keep scanning
	dec	di		;back to edge
	xor	bx,bx		;pretend to run out of skip count
SR13NoCnt:

;	[BX] pixels have been tested, or the viewport edge encountered,
;	now exit with:
;		BX = number of pixels painted = 0
;		CL = already painted flag     = 0
;		DX = border count	      = 0

	mov	dx,bx		;return skip count = 0, and no pixels painted
	jmp	short SR13Exit	;go exit

;	Original scan encountered a non-border pixel.
;	Now scan and paint non-border pixels until:
;		1. edge of screen/VIEWPORT is encountered (edge painted)
;		2. a border pixel is encountered (border not painted)

SR13Paint:
	mov	b$SaveCa,di	;saving _bSaveCa
	mov	b$OffC,di	;set the cursor offset for paint start
	push	bx		;save border count
	xor	bx,bx		;clear paint count
SR13Loop2:
	cmp	di,B$REOFST	;past viewport edge?
	ja	SR13Edge	;go if so
	mov	al,es:[di]	;[al] = screen byte
	cmp	al,dl		;screen byte == border?
	je	SR13Border	;go if so
	XOR	al,DH		;detect any differences from paint color
	OR	cl,al		;combine with the already painted flag
	inc	bx		;increment paint count
	INC	DI		;move right by one byte
	JMP	SHORT SR13Loop2 ;keep scanning and counting
SR13Edge:
	dec	di		;back up
SR13Border:
	or	bx,bx		;any pixels to paint
	jz	SR13NoSet	;go if none
	push	bx		
	push	cx		
	call	NSetC_13	;paint 'em all at once (and tile)
	pop	cx		
	pop	bx		
SR13NoSet:			
	pop	dx		;return skip count

;	Second scan encountered a border pixel or viewport edge,
;	now exit with:
;		BX = count of pixels painted
;		     (whether or not they changed color)
;		CL = 0 iff no pixels changed color
;		DX = count of BORDER pixels searched in the first search
;		b$OffC = the last pixel examined/painted
;		SI = specifies the first non-BORDER pixel encountered

SR13Exit:
	mov	b$OffC,di	;return cursor offset
	mov	si,b$SaveCa	;returning b$SaveCa
cEnd

;***
; B$xINITVGA - initialize VGA modes
;
;Purpose:
;	Added with revision [10].
;	Put the addresses of VGA screen mode support routines into the
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
cProc	B$xINITVGA,<FAR,PUBLIC> 
cBegin
	MOV	WORD PTR [b$ScreenTab + (11*2) + 1],OFFSET B$Screen11
	MOV	WORD PTR [b$ScreenTab + (12*2) + 1],OFFSET B$Screen12
	MOV	WORD PTR [b$ScreenTab + (13*2) + 1],OFFSET B$Screen13
cEnd

sEnd	GR_TEXT

	END
