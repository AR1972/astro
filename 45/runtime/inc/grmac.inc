;***
; GRMAC - ModeData macros
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This macro set is used to create the "ModeData" tables.
;
;	The "master" ModeData table is called "b$ModeData" and resides in
;	DGROUP in LLCGRP.  Each mode-dependent module constructs a separate
;	table for each different mode in the Code Segment using these
;	macros. A mode-dependent ModeData table is then copied to the
;	master table when establishing a new screen mode.
;
;	All the tables are created using this macro set to insure
;	consistency in their structure and to provide a central point (the
;	macros) for modifying that structure.
;
;	The mModeData macro should be followed by individual data item
;	macros (in any order) and terminated using the mEnd macro.
;	The general format and list of data items is as follows:
;
;	mModeData	<table name>
;	mScreenMode	<screen mode>
;	mBiosMode	<BIOS mode>
;	mBurst		<burst>
;	mScrWidth	<text screen width>
;	mScrHeight	<text screen height>
;	mHorzRes	<horizontal pixel resolution>
;	mVertRes	<vertical pixel resolution>
;	mVideoBase	<base address of video segment>
;	mMaxAttr	<maximum attribute value>
;	mMaxColor	<maximum color value (for palette assignment)>
;	mPageSize	<page size (a "rough" size in K)>
;	mCurrPSize	<page size in paragraphs for 1 plane>
;	mMaxPage	<maximum page number>
;	mNullColor	<null character color attribute>
;	mForeColor	<foreground color attribute>
;	mBackColor	<background color attribute>
;	mEgaWrMd	<value for EGA write mode register (odd/even control)>
;	mInitPalette	<pointer to initial palette>
;	mInitVgaPal	<pointer to initial VGA palette>
;	mAlphaDim	<function vector to change screen text dimensions>
;	mSetMode	<function vector to set screen mode>
;	mSetPages	<function vector to change active/visual pages>
;	mPalReset	<function vector to reset palette>
;	mPalPut 	<function vector to set single palette entry>
;	mPalTrans	<function vector to translate attribute/color pair>
;	mPalSet 	<function vector to set entire palette>
;	mSetColor	<function vector to process COLOR statment>
;
;	--- following data items required for GRAPHICS modes only ---
;
;	mForeMapped	<forecolor mapped to hardware value>
;	mBitsPerPixel	<bits per pixel (per plane)>
;	mPlanes 	<number of graphics planes>
;	mMapXYC 	<function vector to map X,Y to mask & offset>
;	mLeftC		<function vector to move graphics cursor left>
;	mChkUpC 	<function vector to move graphics cursor up w/check>
;	mUpC		<function vector to move graphics cursor up>
;	mChkDownC	<function vector to move graphics cursor down w/check>
;	mDownC		<function vector to move graphics cursor down>
;	mSetAttr	<function vector to set current color attribute>
;	mReadC		<function vector to read pixel value>
;	mSetC		<function vector to set pixel value>
;	mSetPixC	<function vector to set pixel multiple times>
;	mSetPixFirstC	<function vector to prologue SetPixC>
;	mSetPixLastC	<function vector to epilogue SetPixC>
;	mLineX		<function vector to draw X-major lines>
;	mLineY		<function vector to draw Y-major lines>
;	mLineV		<function vector to draw vertical lines>
;	mPutAction	<function vector to establish PUT action verb>
;	mNReadL 	<function vector to read a line of pixels>
;	mNWriteL	<function vector to write a line of pixels>
;	mNSetC		<function vector to set a line of pixels>
;	mPaintBound	<function vector to set PAINT boundaries for a line>
;	mSetTile	<function vector to set PAINT tile>
;	mScanL		<function vector to scan left during PAINT>
;	mScanR		<function vector to scan right during PAINT>
;
;	--- mEnd required for ALL modes ---
;
;	mEnd		<graphics length>,<text length>,<define PUBLICS>
;
;******************************************************************************

;*** 
; mModeData (macro)
;
;Purpose:
;	When invoked, mModeData creates a macro called ?mModeData.  This
;	macro generates the label name for the table.  All ?m macros
;	and data values are for internal use by the macro set only.
;	?mModeData purges itself after use (by mEnd).
;
;Arguments:
;	<table name>
;
;******************************************************************************

mModeData macro n
	?mModeData  macro
	    if	?mDEF
		labelW	<PUBLIC,n>
	    else
		labelW	n
	    endif
		purge	?mModeData	;avoid residue from one table to next
		endm
	endm

;*** 
; mEnd (macro)
;
;Purpose:
;
;	This macro actually builds the table.  The data items supplied by the
;	individual macros are placed appropriately in the table. Additional
;	items calculated from the supplied values are also computed and built
;	into the table. The "define" macro (below) is used to check and build
;	each numeric table item. Macros created by invocation of the vector
;	data item macros are used to build the pointers and function vectors.
;
;	Specifying a symbol for the <graphics length> parameter generates a
;	graphics mode table and sets the value of the symbol to the length of
;	the graphics table.
;
;	Specifying a symbol for the <text length> parameter generates a text
;	mode table and sets the value of the symbol to the length of the text
;	table.
;
;	Specifying a symbol for both generates a graphics table, but sets
;	each symbol as above.
;
;	Specifying TRUE for the <define PUBLICS> (third) parameter generates
;	PUBLIC names (prefixed with "b$") for each data item. This also
;	creates the GraphVectStart LABEL and GraphVectCnt ABSOLUTE used by
;	B$InitModeData to reset the graphics function vectors for text
;	modes.	This should be used only to define the "master" table.
;
;TABLE MAINTENANCE:
;	To add new entries in the table, add an entry to the MACRO CREATION
;	section.  Then insert a line in "mEnd" using "define" for numeric
;	items, or invoking the "?mXX" macro for vector items.
;
;Arguments:
;	<graphics length>
;	<text length>
;	<define PUBLICS>
;******************************************************************************

mEnd	macro	glen,tlen,def
    ifb     <def>
	?mDEF		=	0		;no PUBLICS
    else
	?mDEF		=	def		;define PUBLICS
    endif
	?mBurst 	= (?mBurst NE 0) AND 1	;strip Burst to one bit
    ifb     <tlen>
	?mLEN		=	12		;packet len for graphics modes
	?mType		=	10B		;DOS5 graphics flag on
    else
	?mLEN		=	8		;packet len for text modes
	?mType		=	0		;DOS5 graphics flag off
    endif
    if	(?mBiosMode NE 7) AND (?mBiosMode NE 0FH) ;if not a mono mode, set
	?mType		=	?mType + 1	;DOS5 color adapter flag
    endif
    ifnb    <glen>
;
;compute those graphics values which are derivable from the others
;
	?mPixelsPerByte = 8 / ?mBitsPerPixel
	?mBytesPerRow	= ?mHorzRes / ?mPixelsPerByte
	?mMaskLeft	= LOW (0FFH SHL (8-?mBitsPerPixel))
	?mMaskRight	= 0FFH SHR (8-?mBitsPerPixel)
	;
	;DivShift is the shift count used to divide by PixelsPerByte
	;
    if	?mPixelsPerByte EQ 8
	?mDivShift	= 3
    else
    if	?mPixelsPerByte EQ 4
	?mDivShift	= 2
    else
    if	?mPixelsPerByte EQ 1
	?mDivShift	= 0
    else
	.ERR
	%OUT BitsPerPixel: invalid value
    endif
    endif
    endif
	;
	;ModMask is the mask used to MOD by PixelsPerByte
	;
	?mModMask	= ?mPixelsPerByte - 1
	;
	;Aspect is 256*4*(VertRes/HorzRes)/3
	;AspectInv is the inverse of Aspect
	;   The "*256" produces a "fixed-point" decimal value.
	;   The rest of the funniness is to round the value and to
	;   avoid overflow of 16-bit computations during assembly.
	;
	; These are the correct values:
	;
	;  mode(s)	b$Aspect	b$AspectInv
	;  -------	--------	-----------
	;  1,4,7,13	   213		    307
	;    2,8	   107	            614
	;     3		   165		    397
	;    9,10	   187		    351
	;   11,12	   256		    256
	;
	; Note:  Since these formula are not exact, be sure that the correct
	; values are obtained for any new screen modes that are added.
	;
	AspTmp = (?mVertRes MOD ?mHorzRes * 64+?mHorzRes/8)/(?mHorzRes/4) ;;[4]
	?mAspect	= ((?mVertRes/?mHorzRes*256+AspTmp)*8+3)/6	;;[4]
	AspTmpInv = (?mHorzRes MOD ?mVertRes * 128)/(?mVertRes/2)	;;[4]
	?mAspectInv	= ?mHorzRes/?mVertRes*192+(AspTmpInv*3+2)/4	;;[4]
    endif

	?mStart = $

	?mModeData

	define	B,ScreenMode		;current BASIC screen mode
    if	    ?mDEF
	labelW	<PUBLIC,b$ModeBurst>	;BiosMode and Burst in one word
    endif
	define	B,BiosMode		;current BIOS screen mode
	define	B,Burst 		;color burst

;****************************************************************************
;START OF DOS5 VIOSETMODE request packet (keep in this format!!!!)
;****************************************************************************

	define	B,ScrWidth		;otherwise we only need to
	define	B,ScrHeight		;  use bytes
	define	W,HorzRes		;horizontal resolution
	define	W,VertRes		;vertical resolution

;****************************************************************************
;END OF DOS5 VIOSETMODE request packet
;****************************************************************************

	;text mode-dependent data
	define	W,VideoBase		;video segment base
	define	B,MaxAttr		;maximum attribute
	define	B,MaxColor		;maximum color
	define	B,PageSize		;page size in K
	define	W,CurrPSize		;page size in paragraphs (1 plane)
	define	B,MaxPage		;maximum page number
;b$NullColor:
;In text modes, low nibble = b$ForeColor and high nibble = b$BackColor
;In graphics modes, either b$ForeColor or b$BackColor, depending on the mode
	define	B,NullColor		;null character attribute
    if	    ?mDEF
	labelW	<PUBLIC,b$FBColors>	;fore and back colors in one word
    endif
	define	B,ForeColor		;foreground color
	define	B,BackColor		;background color
	define	B,EgaWrMd		;EGA write mode 2 register value,
					;   bit 4=odd/even
	?mInitPalette			;initial (reset) palette
	;text mode-dependent function hooks
	?mAlphaDim
	?mSetMode
	?mSetPages
	?mPalReset
	?mPalPut
	?mPalTrans
	?mPalSet
	?mSetColor
    ifnb    <tlen>
	tlen	 =   $ - ?mStart	;length of text mode tables
    endif
    ifnb    <glen>
	;graphic mode-dependent data
	define	B,ForeMapped		;fore color mapped to external value
					;  for text ops in graphics modes
	define	B,BitsPerPixel		;bits per pixel
	define	B,Planes		;number of graphic planes
	define	W,BytesPerRow		;number of bytes per row
	define	B,MaskLeft		;mask with leftmost bit set
	define	B,MaskRight		;mask with rightmost bit set
	define	W,PixelsPerByte 	;pixels per byte
	define	B,DivShift		;shift count for x DIV b$PixelsPerByte
	define	W,ModMask		;mask for x MOD b$PixelsPerByte
	define	W,Aspect		;aspect ratio * 256
	define	W,AspectInv		;inverse aspect ratio * 256
	;graphic mode-dependent function hooks
    if	    ?mDEF
	labelW	GraphVectStart		;start of graphics vectors
    endif
	?mMapXYC
	?mLeftC
	?mChkUpC
	?mUpC
	?mChkDownC
	?mDownC
	?mSetAttr
	?mReadC
	?mSetC
	?mSetPixC
	?mSetPixFirstC
	?mSetPixLastC
	?mLineX
	?mLineY
	?mLineV
	?mPutAction
	?mNReadL
	?mNWriteL
	?mNSetC
	?mPaintBound
	?mSetTile
	?mScanL
	?mScanR
    endif
    ifnb    <glen>
	glen	 =   $ - ?mStart	;length of graphics mode tables
    endif
    if	    ?mDEF
	GraphVectCnt = ($-GraphVectStart)/2 ;count of graphics vectors
    endif
	endm

;*** 
; define (internal macro)
;
;Purpose:
;	Build a numeric data item into the table, given a name and type.
;
;	If ?mDEF is set TRUE by "mEnd", "define" generate a public label
;	for the item.
;
;	Afterwards, the symbol is set to a known invalid value.  (Symbols
;	cannot be purged.)  This value is tested for to prevent subsequent
;	tables from inheriting values from a previous table by omitting
;	an entry.
;
;	NOTE:  Be sure, when modifying the table structure in "mEnd" that
;	       a symbol's value is used in any computations before use
;	       with "define", since it invalidates the value.
;
;Arguments:
;	<data storage type (B or W)>
;	<numeric item name>
;
;******************************************************************************

define	macro	t,n
    if	?mDEF
	label&t <PUBLIC,b$&n>
    endif
	D&t	?m&n
    if	?m&n EQ 7FFFH
	.ERR
	%OUT &n: invalid (residual) value
    endif
	?m&n	= 07FFFH	;set value to "invalid" to trap residue
	endm

;*** 
; mmd (internal macro) "make macro data"
;
;Purpose:
;	This macro is used internally to generate each of the macros
;	for use in recording the values of numeric data items.
;	"mmd ScreenMode", for example generates a macro named "mScreenMode".
;	When "mScreenMode" is later invoked with a value as an argument, the
;	symbol "?mScreenMode" will be set to that value for later use by
;	the "mEnd" macro.
;
;Arguments:
;	<numeric item name>
;
;******************************************************************************

mmd	macro	n
	mmdi	m&n,?m&n
	endm

;*** 
; mmdi (internal macro) "make macro data item"
;
;Purpose:
;	Used by macro mmd to create a macro (e.g. mScreenMode) which will
;	set a symbol (e.g. ?mScreenMode) to the value of its argument.
;
;Arguments:
;	<macro name>
;	<value symbol>
;******************************************************************************

mmdi	macro	n1,n2
	n1	macro	v
		n2	= v
		endm
	endm

;*** 
; mmwv (internal macro) "make macro word vector"
;
;Purpose:
;	This macro is used internally to generate each of the macros
;	for use in recording the values of word vector data items.
;	"mmwv AlphaDim", for example generates a macro named "mAlphaDim".
;	When "mAlphaDim" is later invoked with a label as an argument, a
;	new macro "?mAlphaDim" will be created.  "mEnd" invokes the
;	"?mAlphaDim" macro to build the "DW <label>" item.
;
;	This method is used as the only mechanism to "store" label
;	names for later use.  (The label name is essentially "stored"
;	in the final macro.)  With MASM (4.0), EQU's cannot be purged
;	or reassigned, and "="'s appear not to be able to handle
;	assignments involving external labels.
;
;Arguments:
;	<label item name>
;
;******************************************************************************

mmwv	macro	n
	mmv	m&n,?m&n,b$&n,W
	endm

;*** 
; mmdv (internal macro)
;
;Purpose:
;	This macro is used internally to generate each of the macros
;	The method is identical to "mmwv" except that a double-word
;	vector is ultimately generated.
;
;Arguments:
;	<label item name>
;
;******************************************************************************

mmdv	macro	n
	mmv	m&n,?m&n,b$&n,D
	endm

;*** 
; mmv (internal macro)
;
;Purpose:
;	Used by macros "mmwv" and "mmdv" to create a macro (e.g. mAlphaDim)
;	which will create a macro (e.g. ?mAlphaDim) to build the appropriate
;	short or double word pointer table entry.
;
;	The final macro (e.g. ?mAlphaDim) purges itself after use (by mEnd)
;	so that a subsequent table definition omitting a value will not
;	pick up residual data from a previous table construction.
;
;Arguments:
;	<first macro name (mXX)>
;	<second macro name (?mXX)>
;	<public name for item (b$XX)>
;	<data storage type (W or D)>
;******************************************************************************

mmv	macro	n1,n2,n3,t
	n1	macro	v
		n2	macro
		    if	?mDEF
			label&t <PUBLIC,n3>
		    endif
			D&t	v
			purge	n2	;avoid residue from one table to next
			endm
		endm
	endm
;
; MACRO CREATION:
; --------------
; Now create all the macros for all the data items (using the macros
; described above).  These macros, all named "mXX" where XX is the
; name argument, are then used to specify values for the table items.
;
mmdv	InitPalette
mmdv	InitVgaPal		;[1]
mmwv	AlphaDim
mmwv	SetMode
mmwv	SetPages
mmwv	PalReset
mmwv	PalPut
mmwv	PalTrans
mmwv	PalSet
mmwv	SetColor
mmwv	MapXYC
mmwv	LeftC
mmwv	ChkUpC
mmwv	UpC
mmwv	ChkDownC
mmwv	DownC
mmwv	SetAttr
mmwv	ReadC
mmwv	SetC
mmwv	SetPixC
mmwv	SetPixFirstC
mmwv	SetPixLastC
mmwv	LineX
mmwv	LineY
mmwv	LineV
mmwv	PutAction
mmwv	NReadL
mmwv	NWriteL
mmwv	NSetC
mmwv	PaintBound
mmwv	SetTile
mmwv	ScanL
mmwv	ScanR
mmd	ScreenMode
mmd	BiosMode
mmd	Burst
mmd	ScrWidth
mmd	ScrHeight
mmd	HorzRes
mmd	VertRes
mmd	VideoBase
mmd	MaxAttr
mmd	MaxColor
mmd	PageSize
mmd	CurrPSize
mmd	MaxPage
mmd	NullColor
mmd	ForeColor
mmd	BackColor
mmd	EgaWrMd
mmd	ForeMapped
mmd	BitsPerPixel
mmd	Planes

purge	mmd, mmwv, mmdv, mmv, mmdi  ;no longer needed
