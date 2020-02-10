	TITLE	LLCGRP - Core GW-BASIC Graphics Interface
;***
; LLCGRP - Core GW-BASIC Graphics Interface
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains support for graphics functions required
;	by programs that may not use graphics drawing statements.
;	Some non-graphic statements (such as CLS) require some graphic
;	support.  Routines directly supporting only graphics drawing
;	statements are placed in LLAGRP to improve runtime granularity
;	and reduce the size of programs not directly using graphics.
;
;	Routines in this module are mode-independent in one or more of the
;	following ways:
;	    1) no device-dependent interaction,
;	    2) table driven through mode-dependent data vectors, or
;	    3) calls mode-dependent routines through function vectors.
;
;	Mode-dependent graphics functions and initializers for vectors
;	are segregated in separate modules for better granularity.
;
;******************************************************************************

	INCLUDE switch.inc	;switch file
	INCLUDE rmacros.inc

	USESEG	_BSS
	USESEG	_DATA
	USESEG	GR_TEXT 	

	INCLUDE seg.inc
	INCLUDE idmac.inc	
	INCLUDE grmac.inc	
	INCLUDE ibmunv.inc

sBegin	_BSS
;
;#***************************************************************************
; External variables
;#***************************************************************************
;
externW b$HugeDelta		;OS dependent selector increment
;
;#***************************************************************************
; Global variables
;#***************************************************************************
;
labelW	<PUBLIC,b$PenC>	;cursor "pen" is mask + attribute
globalB b$MaskC,,1		;  cursor mask byte
globalB b$AttrC,,1		;  each pixel in byte set to color attribute

labelD	<PUBLIC,b$AddrC>	;address of cursor in video memory
globalW b$OffC,,1		;  cursor address offset
globalW b$SegC,,1		;  cursor address segment

globalB b$PaintBorder,,1	;color attribute of PAINT border
globalB b$Tiling,,1		;tiling flag
globalW b$SaveCa,,1		;cursor values for first
globalB b$SaveCm,,1		;  non-border pixel during PAINT
globalB b$PlaneMask,,1 	;bit mask for Map Mask Reg for PUT

globalW b$Incr1,,1		;major axis update value for Line
globalW b$Incr2,,1		;minor axis update value for Line
globalW b$IncrY,,1		;change for Y movement for Line

globalW B$VTOFST,,1		;second row down from top of viewport
globalW B$VBOFST,,1		;bottom edge of viewport
globalW B$VLOFST,,1		;left edge of viewport and row 0
globalW B$VROFST,,1		;right edge of viewport and row 0
globalB B$VLMASK,,1		;corresponding mask for left edge
globalB B$VRMASK,,1		;corresponding mask for right edge
globalW B$LEOFST,,1		;left edge offset of viewport at current row
globalW B$REOFST,,1		;right edge offset of viewport at current row
globalB b$BorderColor,,1	;border color (overscan)
sEnd	_BSS			

sBegin	_DATA			
;
;#***************************************************************************
; Mode-dependent data and function vectors
;
; This data is filled by B$InitModeData from matching tables in the mode-
; dependent modules.  All are created using the same macro set to insure
; consistency in their structure and to provide a central point (the macros)
; for modifying that structure.  The "mEnd" macro actually builds the table.
; Specifying TRUE for its "def" (third) parameter causes it to also create
; PUBLIC names (prefixed with "b$") for each data item.  This also
; creates the GraphVectStart LABEL and GraphVectCnt ABSOLUTE used by
; B$InitModeData to reset the graphics function vectors for text modes.
;
; Refer to GRMAC.INC for more details on the actual structure and the
; variables provided.  To aid in the textual search for these names,
; omit the "b$" in the search.  This will locate all of the macro-generated
; sources for the data item in all the modules.  (e.g.: searching for
; "ScreenMode" instead of "b$ScreenMode" will find all of the "mScreenMode"
; macro invocations.)
;
;This data table is pre-initialized to resemble a 40-column (worst case)
;    text mode to account for its possible usage even before any screen mode
;    initialization takes place.  Function vectors are initialized to
;    B$ErrorReturn, which just sets the carry (as an error indication) and
;    returns.
;#***************************************************************************
;
mModeData	b$ModeData
mScreenMode	0
mBiosMode	0
mBurst		0
mScrWidth	40
mScrHeight	25
mHorzRes	320
mVertRes	200
mVideoBase	0B800H
mMaxAttr	15
mMaxColor	0
mPageSize	2		    ;page size in K
mCurrPSize	<(2*1024) shr 4>    ;page size in paragraphs
mMaxPage	0
mNullColor	7
mForeColor	7
mBackColor	0
mEgaWrMd	0
mInitPalette	0
mInitVgaPal	0
mAlphaDim	B$ErrorReturn
mSetMode	B$ErrorReturn
mSetPages	B$ErrorReturn
mPalReset	B$ErrorReturn
mPalPut 	B$ErrorReturn
mPalTrans	B$ErrorReturn
mPalSet 	B$ErrorReturn
mSetColor	B$ErrorReturn
mForeMapped	7
mBitsPerPixel	1
mPlanes 	1
mMapXYC 	B$ErrorReturn
mLeftC		B$ErrorReturn
mChkUpC 	B$ErrorReturn
mUpC		B$ErrorReturn
mChkDownC	B$ErrorReturn
mDownC		B$ErrorReturn
mSetAttr	B$ErrorReturn
mReadC		B$ErrorReturn
mSetC		B$ErrorReturn
mSetPixC	B$ErrorReturn
mSetPixFirstC	B$ErrorReturn
mSetPixLastC	B$ErrorReturn
mLineX		B$ErrorReturn
mLineY		B$ErrorReturn
mLineV		B$ErrorReturn
mPutAction	B$ErrorReturn
mNReadL 	B$ErrorReturn
mNWriteL	B$ErrorReturn
mNSetC		B$ErrorReturn
mPaintBound	B$ErrorReturn
mSetTile	B$ErrorReturn
mScanL		B$ErrorReturn
mScanR		B$ErrorReturn
mEnd		GraphDataLen,TextDataLen,TRUE
;
; End of table-initialized mode-dependent data
;
;#***************************************************************************
; Mode-dependent functions set-up and used by other mode-dependent functions
;	(see detailed descriptions below)
;#***************************************************************************
;
globalW b$PutVector,B$ErrorReturn,1	
;
;#***************************************************************************
; Mode-dependent variables are described in detail here
;#***************************************************************************
;

;***
;b$ScreenMode - Current Basic Screen Mode
;OEM-interface routine (variable)
;
;Purpose:
;	This variable contains the BASIC mode number of the current
;	screen mode.  This is the number as given in the SCREEN
;	statement, not the BIOS mode number.
;
;Allocated:
;	b$ScreenMode is a BYTE value allocated in _DATA by the OEM.
;
;Values:
;	This variable is initially set to 0 for a worst case scenario
;	when an error message has to be printed before the screen is
;	initialized.  After initialization, it may take on the value
;	of any legal screen mode (0-2, 7-13 if all possible modes are
;	supported).
;
;Initially Set:
;	b$ScreenMode is statically initialized to 0.
;
;Modified By:
;	This variable is only modified by the OEM-Dependent routines.
;	It should be updated when the screen is initialized, and when
;	a SCREEN statement is given.
;
;Used By:
;	This routine is used by the OEM-Dependent code to keep track
;	of which screen mode is current.  It is used by the OEM-Independent
;	Code to determine if a graphics mode is in effect.
;	(b$ScreenMode = 0 <=> non graphics mode)
;******************************************************************************

;
;#***************************************************************************
; Mode-dependent functions called indirectly are described in detail here
;#***************************************************************************
;

;*** 
; b$AlphaDim
;
;Purpose:
;	Validate the proposed text dimensions and establish mode-dependent
;	variables accordingly.	This routine may indicate that the
;	dimensions are invalid or unsupportable, or may request a change
;	in screen mode where the dimensions are supported.
;	NOTE:  No actual change in screen mode occurs until SetMode is called!
;
;Entry:
;	[BH] = number of lines
;	[BL] = number of columns
;
;Exit:
;	PSW.C = set if error
;	[AL] = -1 if dimensions set in current mode or error,
;	       otherwise AL is SCREEN mode to invoke for desired dimensions
;
;Uses:
;Exceptions:
;******************************************************************************

;
;*** 
; b$SetMode
;
;Purpose:
;	Set the screen mode according to the characteristics established
;	by previous calls to B$Screenx and b$AlphaDim
;
;Entry:
;Exit:
;Uses:
;Exceptions:
;******************************************************************************

;*** 
; b$SetPages
;
;Purpose:
;	Set the current active and visual pages and calculate page size
;	and video segment offsets.
;
;Entry:
;	[AL] = active page
;	[AH] = visual page
;
;Exit:
;Uses:
;Exceptions:
;******************************************************************************

;***
; b$PalReset - Reset PALETTE to default state.
;OEM-interface routine
;
;Purpose:
;	Reset the PALETTE to the initial, default colors.  This routine
;	is only called for the PALETTE command and when the screen modes
;	change (note: screen initialization is a mode change).
;
;	NOTE: b$PalReset is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None
;
;Exit:
;	PSW.C is set to indicate an error.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************

;***
; b$PalPut - Change palette entry
;OEM-interface routine
;
;Purpose:
;	Change palette entry with translation/verification.
;	A color value of negative one indicates that the associated
;	palette entry is not to be modified.
;
;	Before setting the palette, the color and attribute values
;	are verified as legal and translated, if necessary, to the
;	internal/hardware values.  Attribute values are seen by the
;	BASIC user as being contiguous, when in fact they may not
;	be.  Color values are similarly manipulated.
;
;	NOTE: b$PalPut is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[DX:AX] = color
;	[BL]	= attribute
;
;Exit:
;	PSW.C reset indicates successful operation
;		set indicates PALETTE function call error
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************

;***
; b$PalTrans
;
;Purpose:
;	User supplied color and attribute values are verified as legal
;	and translated, if necessary, to internal/hardware values.
;	Attribute values are seen by the BASIC user as being contiguous
;	from 0 to b$MaxAttr, when in fact they may not be.  This
;	routine vector provides the opportunity for modes to map from the
;	user supplied attribute to that which is actually used as a pixel
;	value in video memory or as a palette index.
;	Color values are similarly disguised and may also be manipulated
;	in support of different monitors.
;
;	NOTE: b$PalTrans is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[DX:AX] = internal color
;	[BL]	= internal attribute
;
;Exit:
;	PSW.C reset if entry OK
;	[DX:AX] = actual color
;	[BL]	= actual attribute
;
;Uses:
;Exceptions:
;******************************************************************************

;***
; b$PalSet - Set entire palette from an array
;OEM-interface routine
;
;Purpose:
;	Set the entire palette from an array specified by the user.  Each
;	element of the array is a color value for the attribute which is
;	the current index of the array.   A color value of
;	of -1 indicates the entry should be left alone.  All entries are
;	verified before any are set and are also translated, as needed.
;	Verification and Translation are discussed in b$PalTrans (a non
;	OEM interface routine) and b$PalPut.
;
;	Special consideration must be made for the number of bytes per
;	element in the array.  Also, if there are not enough elements in
;	the array to fill the palette, the return with PSW.C set for
;	an Illegal Function Call.
;
;	NOTE: b$PalSet is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[AX]	= size of the array in elements
;	[ES:SI] = address of the array
;	[CX]	= size of each array element in bytes (2 or 4)
;
;Exit:
;	PSW.C set indicates function call error
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************

;***
; b$MapXYC - Map coordinates to graphics cursor
;OEM-interface routine
;
;Purpose:
;	Move the graphics cursor to the pixel specified by the given
;	coordinates.  Both coordinates will always be within the screen
;	boundaries and will already have been clipped to ensure that
;	they are within the viewport.
;
;	NOTE: b$MapXYC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[CX] = X coordinate
;	[DX] = Y coordinate
;
;Exit:
;	Graphics Cursor location updated.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************
;Our Exit Condition:
;	   b$OffC, b$MaskC updated


;***
; b$LeftC - Move graphics cursor left a pixel
;OEM-interface routine
;
;Purpose:
;	Move graphics cursor left 1 pixel.  No test is made for
;	screen boundaries.
;
;	NOTE: b$LeftC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	none
;
;Exit:
;	Graphics Cursor location updated.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;******************************************************************************
;Our Exit Condition:
;	   b$OffC, b$MaskC updated

;***
; b$ChkUpC - Move graphics cursor up a pixel, checking for upper boundary
;OEM-interface routine
;
;Purpose:
;	This routine will move the graphics cursor up one pixel if it
;	would stay within the viewport boundaries.  If this move would
;	make it leave the viewport, then PSW.C is set upon the return
;	and no move is made.  The viewport boundaries are specified by
;	a previous call to B$MapVWC.  Note that there does not have
;	to be a separate check for screen boundaries, as the viewport
;	defaults to the screen boundary.
;
;	NOTE: b$ChkUpc is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None.
;
;Exit:
;	PSW.C = set if original graphics cursor was on screen top edge.
;	PSW.C = reset if cursor could move.  Graphics Cursor location updated.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;******************************************************************************
;Our Exit Condition:
;	   b$OffC, b$MaskC updated if cursor not on boundary.

;***
; b$UpC - Move graphics cursor up 1 pixel.
;OEM-interface routine
;
;Purpose:
;	Move graphics cursor up 1 pixel.  No test is made for
;	screen or viewport boundaries.
;
;	NOTE: b$UpC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None
;
;Exit:
;	Graphics Cursor location updated.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************
;Our Exit Condition:
;	   b$OffC, b$MaskC updated

;***
; b$ChkDownC - Move Graphics Cursor down a pixel, checking for bottom edge
;OEM-interface routine
;
;Purpose:
;	This routine will move the graphics cursor down one pixel if
;	it would stay within the viewport boundaries.  If this move
;	would make it leave the viewport, then PWS.C is set upon return
;	and no move is made.  The viewport boundaries are specified by
;	a previous call to B$MapVWC.  Note that there does not have
;	to be a separate check for screen boundaries, as the viewport
;	defaults to the screen boundary.
;
;	NOTE: b$ChkDownC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None.
;
;Exit:
;	PSW.C = set    if original cursor was on screen edge.
;	PSW.C = reset  otherwise, Graphics Cursor moved
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************
;Our Exit Condition:
;	   b$OffC, b$MaskC updated if not originally on boundary.

;***
; b$DownC - Move graphics cursor down a pixel
;OEM-interface routine
;
;Purpose:
;	Move graphics cursor down 1 pixel.  No test is made for
;	screen boundaries.
;
;	NOTE: b$DownC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None.
;
;Exit:
;	Graphics Cursor location updated.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************
;Our Exit Condition:
;	   b$OffC, b$MaskC updated

;***
; b$SetAttr - Set attribute value
;OEM-interface routine
;
;Purpose:
;	Map the supplied attribute value to that required for use by
;	the mode-dependent module.  If the value is legal, then update
;	the OEM state to indicate the new attribute to use.  Otherwise,
;	return with PSW.C set to indicate an error.
;
;	NOTE: b$SetAttr is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[AL] = attribute
;
;Exit:
;	PSW.C = set indicates an error with the attribute value.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX
;
;Exceptions:
;	None
;******************************************************************************

;***
; b$SetColor
;
;Purpose:
;	Process the COLOR statement.  Because different modes have different
;	reactions to the color statement parameters, they use the parameters
;	directly according to their mode-specific functions.
;
;	NOTE: b$SetColor is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[SI] = address of the parameter list
;	[CX] = count of words in parameter list
;	(get parameters using B$GetParm)
;
;Exit:
;	PSW.C = set indicates that an error was encountered
;
;Uses:
;Exceptions:
;******************************************************************************

;***
; b$ReadC - Return attribute of current pixel
;OEM-interface routine
;
;Purpose:
;	Return the attribute of the current pixel as specified by
;	by the location of the graphics cursor.  This routine is
;	used by the BASIC function POINT().  The attribute that is
;	is returned should be in the form as it would be passed to
;	[b$SetAttr], not in an internal form.
;
;	NOTE: b$ReadC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None
;
;Exit:
;	[AL] = attribute of pixel
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None
;******************************************************************************

;***
;b$SetC - Set pixel under graphics cursor to current attribute
;OEM-interface routine
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to
;	the current attribute.
;
;	NOTE: b$SetC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None
;
;Exit:
;	None
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None
;******************************************************************************

;***
; b$SetPixC - Set pixel under graphics cursor to current attribute (fast)
;OEM-interface routine
;
;Purpose:
;	Set the pixel defined by the current graphics cursor to
;	the current attribute.	This is identical to b$SetC with the
;	initialization and termination code extracted to make it
;	faster when multiple pixels are being set in a graphics
;	function (i.e. CIRCLE).  A call to b$SetPixFirstC should proceed
;	the first call to b$SetPixC.  A call to b$SetPixLastC should follow
;	the last.  b$SetPixFirstC sets up ES which should be preserved for
;	all b$SetPixC calls.
;
;	NOTE: b$SetPixC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[ES] = the current video segment (set up by b$SetPixFirstC)
;
;Exit:
;	None
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************

;***
; b$SetPixFirstC - Initialize for b$SetPixC
;OEM-interface routine
;
;Purpose:
;	Set up ES to the video segment and perform any other mode-dependent
;	functions required to optimize setting a number of pixels. A call
;	to b$SetPixFirstC should proceed the first call to b$SetPixC.
;
;	NOTE: b$SetPixFirstC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None.
;
;Exit:
;	[ES] = video segment
;
;Uses:
;	ES is modified to point to the video segment
;
;Exceptions:
;	None.
;******************************************************************************

;***
; b$SetPixLastC - Termination for b$SetPixC
;OEM-interface routine
;
;Purpose:
;	Perform any mode-dependent functions required after setting a number
;	of pixels.  A call to b$SetPixLastC should follow the last call
;	to b$SetPixC.
;
;	NOTE: b$SetPixLastC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None.
;******************************************************************************

;***
; b$LineX
;
;Purpose:
;	Draw an X-major line.
;
;	NOTE: b$LineX is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	AH    = color (__AttrC)
;	AL    = bit accumulator (0)
;	CX    = point count
;	BH    = bit mask
;	DX    = BP change for Y movement (b$IncrY)
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr1 = major axis delta update value
;	Incr2 = minor axis delta update value
;	IncrY = BP change for Y movement
;
;Exit:
;Uses:
;Exceptions:
;******************************************************************************

;***
; b$LineY
;
;Purpose:
;	Draw a Y-major line.
;
;	NOTE: b$LineY is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	AH    = color (__AttrC)
;	CX    = point count
;	AL    = bit mask
;	DX    = BP change for Y movement (b$IncrY)
;	SI    = delta decision value
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	Incr1 = major axis delta update value
;	Incr2 = minor axis delta update value
;	IncrY = BP change for Y movement
;
;Exit:
;Uses:
;Exceptions:
;******************************************************************************

;***
; b$LineV
;
;Purpose:
;	Draw a vertical line.
;
;	NOTE: b$LineV is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	AH    = color (__AttrC)
;	CX    = point count
;	AL    = bit mask
;	DX    = BP change for Y movement (b$IncrY)
;	DI    = line style
;	BP    = video offset
;	ES    = video segment
;	IncrY = BP change for Y movement
;
;Exit:
;Uses:
;	any
;Exceptions:
;******************************************************************************

;***
; b$PutAction
;
;Purpose:
;	Allow the mode-dependent module to established which PUT action
;	to apply for the next call to b$NWriteL.
;
;	NOTE: b$PutAction is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	AL = PUT action [0..4] represent (OR,AND,PRESET,PSET,XOR)
;
;Exit:
;Uses:
;	per convention
;
;Exceptions:
;******************************************************************************

;***
; b$NReadL
;
;Purpose:
;	Read a line of pixels from a specified plane to an array.
;
;	NOTE: b$NReadL is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	ES:DI	= screen address
;	DS:SI	= array address
;	CX	= array align shift count
;	BP	= count of bits (not pixels) to read
;	BH	= plane to read from
;
;Exit:
;	DS:SI	= updated to array byte past point filled
;
;Uses:
;Exceptions:
;******************************************************************************

;***
; b$NWriteL
;
;Purpose:
;	Write a line of pixels from an array to a specified plane.
;
;	NOTE: b$WriteL is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	ES:DI	= screen address
;	DS:SI	= array address
;	CX	= array align shift count
;	BP	= count of bits (not pixels) to write
;	BH	= plane to write to
;	DL	= last partial byte mask
;	DH	= first partial byte mask
;
;Exit:
;	DS:SI	= updated to array byte past point used
;
;Uses:
;Exceptions:
;******************************************************************************

;***
; b$NSetC - Set horizontal line of pixels
;OEM-interface routine
;
;Purpose:
;	Set a horizontal line of pixels to the current attribute.
;	The line starts at the current cursor position and moves right.
;	This routine need not check for the end of the screen.
;
;
;	NOTE: b$NSetC is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[BX] = number of pixels to set
;
;Exit:
;	None
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;******************************************************************************

;***
; b$PaintBound - Setup for a scan line of PAINT
;OEM-interface routine
;
;Purpose:
;	Called by PAINT before painting each scan line, to
;	facilitate fast viewport edge detection.  This routine will
;	normally set VIEW left and right cursor addresses and masks.
;
;	The VIEW statement is handled primarily in the OEM independent
;	code but in the interest of speed, it is up to the OEM dependent
;	code to check for the edges of the viewport during painting.
;	The OEM is passed the pixel coordinates of the viewport in
;	a call to B$MapVWC, however converting from this form to
;	the internal representation may be too slow to do repeatedly.
;	Thus, for each row of a region that is going to be painted
;	[b$PaintBound] is called.  This routine should set up conditions
;	so that [b$ChkDownC], [b$ChkUpc], [b$ScanR], and [b$ScanL]
;	can do fast tests for detecting the edges of the viewport.
;
;	The calculations done by b$PaintBound are entirely up to the
;	OEM.  Note however, that the part of the screen that can be
;	changed by graphics statements after a VIEW statement includes
;	the "edges" that the user specified.  Thus VIEW(10,10)-(11,11)
;	has for pixels that can be changed by graphics statements.
;
;	NOTE: b$PaintBound is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	Graphics Cursor is positioned on line to be painted
;
;Exit:
;	Viewport left edge address of current row computed.
;	Viewport right edge address of current row computed.
;	Possible OEM state variables updated.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;******************************************************************************
;Our Entry Condition:
;	b$OffC = address of current pixel
;
;Our Exit Condition:
;	B$LEOFST = left edge offset
;	B$REOFST = right edge offset
;

;***
;  b$SetTile - Set tile attribute
;OEM-interface routine
;
;Purpose:
;	This routine stores the internal form of the current tile
;	attribute for a specific plane.  This routine is called
;	N times for each line to be tiled, where N is the number
;	of planes (actually the value returned by B$TILEMOD).	If
;	not all the bytes for multiple plane systems are specified
;	by the BASIC programmer [b$SetTile] will be called with
;	a value of 0 for the remaining planes.
;
;	The tile attribute should be aligned to the current graphics
;	cursor before actually setting pixels in [b$ScanR] or [b$ScanL].
;
;	NOTE: b$SetTile is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	[BL] = internal form of the tile attribute
;	[BH] = which plane the attribute is for
;
;Exit:
;	b$AttrC set to tile attribute
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None
;******************************************************************************

;***
;  b$ScanL - Paint pixels from cursor to the left
;OEM-interface routine
;
;Purpose:
;	Scan left beginning with the pixel to the left of cursor,
;	and paint pixels with the current attribute until:
;		(1) the viewport edge is encountered (edge painted)
;	     or (2) a border pixel is encountered (border not painted)
;
;	The border value is set in the call to B$PaintInit.  The viewport
;	edges are set when B$MapVWC is called.  Since this routine
;	is called multiple times for a single PAINT statement, it is
;	important that this routine be optimized for speed.  Any values
;	that can be precomputed should be done so in the routines
;	B$PaintInit and [b$PaintBound].
;
;	NOTE: b$ScanL is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	Graphics Cursor    = pixel to right of starting pixel
;Exit:
;	BX		   = number of pixels scanned
;	CL		   = 0 iff no pixels changed color
;	Graphics Cursor    = the last non-border pixel examined/painted
;
;Uses:
;	per convention
;
;Exceptions:
;	None
;******************************************************************************
;Our Entry Conditions:
;	b$PaintBorder	   = attribute of paint region border
;	b$AttrC	   = attribute to paint
;	B$LEOFST, B$VLMASK   = left viewport edge
;

;***
;  b$ScanR - Paint pixels from cursor to the right
;OEM-interface routine
;
;Purpose:
;	Starting with the current pixel, search right until:
;		(1) a non-border pixel is found
;		(2) [DX] pixels have been tested
;		(3) the viewport edge is encountered
;
;	If (2) or (3) terminated the scan, exit with:
;		DX = remaining border count   = 0
;		CL = pixels modified flag     = 0
;		BX = number of pixels painted = 0
;		Graphics Cursor returned to starting point.
;		Returned Cursor = Last cursor returned by [b$ScanR]
;
;	If (1) terminated the scan, start a new scan and paint non-border
;	pixels until:
;		(1) the viewport edge is encountered (edge painted)
;		(2) a border pixel is encountered (border not painted)
;
;	Return with:
;		DX = entry DX - # pixels searched before non-border found
;		CL = pixels modified flag
;		BX = number of pixels painted (even if no color change)
;		Graphics Cursor is at the last pixel examined
;			(either border or viewport edge)
;		Returned Cursor = Cursor Position at which the original
;				  scan was terminated.
;
;	NOTE: b$ScanR is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	DX		   = count of border pixels which may be skipped
;	Graphics Cursor    = starting pixel
;
;Exit:
;	BX		   = number of pixels painted
;				(whether or not they changed color)
;	CL		   = 0 iff no pixels changed color
;	DX		   = remaining border pixel count
;	Graphics Cursor    = the last non-border pixel examined/painted
;	AX, SI, DI	   = the first non-border pixel encountered
;			      AX = first byte of cursor
;			      SI = second byte of cursor
;			      DI = third byte of cursor
;Uses:
;	SI and DI are used for return values.
;
;Exceptions:
;	None
;******************************************************************************
;Our Entry Conditions:
;	b$PaintBorder	   = attribute of paint region border
;	b$AttrC	   = attribute to paint
;	B$REOFST, B$VRMASK   = right viewport edge
;

;***
; b$PutVector
;
;Purpose:
;	PUT action vectors. The PUT code vectors through this routine
;	to write bytes to the screen with the specified PUT action.
;	This vector is generally set up by the b$PutAction routine.
;
;	NOTE: b$PutVector is a variable through which the routine is
;	      indirectly called.
;
;Entry:
;	AH =	byte to write to screen.
;	DH =	mask
;	ES:DI = pointer to video memory
;
;Exit:
;	DI =	updated to point to next video byte
;
;Uses:
;Exceptions:
;******************************************************************************

sEnd	_DATA			

sBegin	GR_TEXT 		
	ASSUMES CS,GR_TEXT	

externNP B$GETDS		
externNP B$ErrorReturn 	

;*** 
; B$InitModeData
;
;Purpose:
;	Initialize mode-dependent data from a table.
;
;Entry:
;	CS:BX = pointer to table to match b$ModeData.
;	CX    = table length
;
;Exit:
;Uses:
;Exceptions:
;******************************************************************************

cProc	B$InitModeData,<PUBLIC,NEAR>,<SI,DI,DS,ES>
cBegin
	push	ds		;to DS:ModeData
	pop	es
	cmp	cx,TextDataLen	;a text entry?
	jne	InitTable	;go if not
	
	;when only the text data is initialized, set all of the graphics
	;function vectors to B$ErrorReturn which will just set carry
	;and return
	
	push	cx		
	mov	di,OFFSET DGROUP:GraphVectStart 
	mov	cx,GraphVectCnt 		
	mov	ax,GR_TEXTOFFSET B$ErrorReturn 
    rep stosw			
	pop	cx		
InitTable:
	mov	di,OFFSET DGROUP:b$ModeData
	push	cs		;from CS:BX
	pop	ds
	mov	si,bx
	cld
    rep movsb			;move it
; initialize foreground attribute to default
	push	es		
	pop	ds		;set DS=DGROUP for SetAttr
	mov	al,b$ForeColor ;get default foreground attribute
	call	[b$SetAttr]	;set attribute
	mov	b$BorderColor,0 ;clear border
cEnd


;***
; B$MapVWC - Change OEM viewport
;OEM-interface routine
;
;Purpose:
;	This routine is called each time the viewport is changed
;	to notify OEM dependent code of the current viewport
;	boundaries. Whenever B$MapVWC is called constants are
;	calculated and these constants will be used in other
;	OEM dependent routines to help speed up the PAINT routine.
;
;Entry:
;	[AX] = Minimum horizontal co-ordinate of viewport. (left edge)
;	[BX] = Maximum horizontal co-ordinate of viewport. (right edge)
;	[CX] = Minimum vertical co-ordinate of viewport. (top edge)
;	[DX] = Maximum vertical co-ordinate of viewport. (bottom edge)
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None.
;****
cProc	B$MapVWC,<PUBLIC,NEAR>,<DI>
cBegin
	PUSH	b$OffC 	;SAVE CURSOR OFFSET
	push	bx		;save viewport_right
	PUSH	AX
	XOR	AH,AH
	MOV	AL,b$MaskC	;SAVE CURSOR MASK
	MOV	DI,AX		;IN [DI]
	PUSH	DX		;SAVE DX, BCOS MAPXYC USES DX
	MOV	DX,CX		;get offset of (o,viewport-topedge)
	XOR	CX,CX
	CALL	[b$MapXYC]
	MOV	CX,b$OffC	;add b$BytesPerRow to this offset
	ADD	CX,b$BytesPerRow 
	MOV	B$VTOFST,CX	;and save in VTOFST (used in b$ChkUpC)
	POP	DX		;RESTORE DX
	XOR	CX,CX
	CALL	[b$MapXYC]	;get offset of (0,viewport-bottomedge)
	MOV	CX,b$OffC	;and svae it in VBOFST
	MOV	B$VBOFST,CX	;used in $TDOWNC
	POP	AX		;restore viewport_left
	MOV	CX,AX		;GET OFFSET AND MASK FOR THE
	XOR	DX,DX		;POINT (VIEWPORT_LEFT,0)
	CALL	[b$MapXYC]
	MOV	CX,b$OffC	;TRANSFER OFFSET AND MASK TO
	MOV	B$VLOFST,CX	;VLOFST AND VLMASK
	MOV	CL,b$MaskC
	MOV	B$VLMASK,CL
	pop	bx		;restore viewport_right
	MOV	CX,BX		;GET OFFSET AND MASK FOR THE
	XOR	DX,DX		;POINT (VIEWPORT_RIGHT,0)
	CALL	[b$MapXYC]
	MOV	CX,b$OffC	;TRANSFER OFFSET AND MASK TO
	MOV	B$VROFST,CX	;VROFST AND VRMASK
	MOV	CL,b$MaskC
	MOV	B$VRMASK,CL
	MOV	AX,DI		;RESTOR OLD CURSOR MASK
	MOV	b$MaskC,AL	;AND STORE IT BACK IN b$MaskC
	POP	b$OffC 	;RESTORE OLD OFFSET
cEnd

;***
; B$GetAspect - Get screen aspect ratio
;OEM-interface routine
;
;Purpose:
;	Return the screen aspect ratio.  Aspect ratio is used by CIRCLE
;	and DRAW to compensate for the possibility that pixels are of
;	unequal dimensions.  The ratio is returned as a fraction of 256.
;
;Entry:
;	None.
;
;Exit:
;	BX = 256 * aspect ratio
;	DX = 256 / aspect ratio
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, CX
;
;Exceptions:
;	None.
;****
cProc	B$GetAspect,<PUBLIC,NEAR>
cBegin
	MOV	BX,b$Aspect	;get aspect ratio
	MOV	DX,b$AspectInv ;get inverse aspect ratio
cEnd

;***
;B$BumpDS - Advances array segment in DS to next valid segment
;
;Purpose:
;	This routine is called by the low level PUT code when it is
;	detected that an indexed PUT has crossed a segment boundary
;	This routine knows that the Low Levels access the array through
;	DS.  It will advance the selector to the next legal 64K chunk.
;
;Entry:
;	DS = Array segment
;
;Exit:
;	DS = Next 64K Array segment
;
;Uses:
;	None!
;
;Exceptions:
;	None.
;****
cProc	B$BumpDS,<PUBLIC,NEAR>,<AX>	
cBegin
	MOV	AX,DS		;save array selector
.erre	ID_SSEQDS		;assumes ss = ds
	add	ax,ss:b$HugeDelta ;add in HUGE segment delta
	MOV	DS,AX		;update DS
cEnd

;***
;B$BumpES - Advances array segment in ES to next valid segment
;
;Purpose:
;	This routine is called by the low level GET code when it is
;	detected that an indexed GET has crossed a segment boundary
;	This routine knows that the Low Levels access the array through
;	ES.  It will advance the selector to the next legal 64K chunk.
;	Added with [10].
;
;Entry:
;	ES = Array segment
;
;Exit:
;	ES = Next 64K Array segment
;
;Uses:
;	None!
;
;Exceptions:
;	None.
;****
cProc	B$BumpES,<PUBLIC,NEAR>,<AX>
cBegin
	MOV	AX,ES		;save array selector
.erre	ID_SSEQDS		;assumes ss = ds
	add	ax,ss:b$HugeDelta ;add in HUGE segment delta
	MOV	ES,AX		;update ES
cEnd

;***
;B$DecDS - Decrements array segment in DS to prev valid segment
;
;Purpose:
;	This routine has the opposite effect of B$BumpDS.
;	Added with [10].
;
;Entry:
;	DS = Array segment
;
;Exit:
;	DS = Prev 64K Array segment
;
;Uses:
;	None!
;
;Exceptions:
;	None.
;****
cProc	B$DecDS,<PUBLIC,NEAR>,<AX>
cBegin
	MOV	AX,DS		;save array selector
.erre	ID_SSEQDS		;assumes ss = ds
	sub	ax,ss:b$HugeDelta ;sub HUGE segment delta
	MOV	DS,AX		;update DS
cEnd

;***
; B$OutWord
;
;Purpose:
;	Output the word in AX to the port at DX.  This is equivalent
;	to "OUT DX,AX" except that AH and AL are swapped when done.
;	This routine exists to support machines (specifically the
;	ATT 6300+) whose word output capability malfunctions.
;
;Entry:
;	[DX] = address of output port
;	[AX] = output data
;
;Exit:
;Uses:
;Exceptions:
;****
cProc	B$OutWord,<PUBLIC,NEAR>
cBegin
	OUT	DX,AL		;set index register
	XCHG	AL,AH		;set up data in AL
	INC	DX		;data port is one byte above index register
	OUT	DX,AL		;set up data in relevant register
	DEC	DX		;restore address of index register
cEnd

;***
; B$ResetEGA
;
;Purpose:
;	To reset EGA Registers to values expected by the BIOS.
;
;Entry:
;Exit:
;
;Uses:
;	none
;
;Exceptions:
;****
cProc	B$ResetEGA,<PUBLIC,NEAR>,<AX,DX>
cBegin
	MOV	DX,SEQADD
	
	;converted OutWord macros to in-line byte outs here for speed
	
	MOV	AL,MMREG	;enables all planes for 32-bit write
	OUT	DX,AL
	INC	DX
	MOV	AL,0FH
	OUT	DX,AL
	MOV	DX,GRPADD	;address of graphics index register
	MOV	AL,ENBREG	;make sure Set/Reset is NOT enabled
	OUT	DX,AL
	INC	DX
	XOR	AL,AL
	OUT	DX,AL
	DEC	DX
	MOV	AL,CLCREG	;select color compare register, and set color 0
	OUT	DX,AL
	INC	DX
	XOR	AL,AL
	OUT	DX,AL
	DEC	DX
	MOV	AX,DTRREG	;reset logical operations
	OUT	DX,AL
	INC	DX
	XOR	AL,AL
	OUT	DX,AL
	DEC	DX
	MOV	AL,BMKREG	;activate all bits in the mask
	OUT	DX,AL
	INC	DX
	MOV	AL,0FFH
	OUT	DX,AL
	DEC	DX
	MOV	AL,CDCREG	;Color Don't Care Register
	OUT	DX,AL
	INC	DX
	MOV	AL,0FH
	OUT	DX,AL
	DEC	DX
	MOV	AL,RWMREG	;index to Mode Register
	OUT	DX,AL
	INC	DX
	MOV	AL,b$EgaWrMd	;then set bit 4 for odd/even addressing
	AND	AL,10H		;use only odd/even bit
	OUT	DX,AL
cEnd

sEnd	GR_TEXT 		

	END
