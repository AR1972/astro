	TITLE		LLTEXT - text screen mode support
;***
;LLTEXT - text screen mode support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Support for text screen modes (BIOS 0,1,2,3,7).
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
;  0 0 40 25 C	x |  0	16  N/A 320 200   2 8x8   8
;  0 x 40 25 m	x |  "   "   "  320 400   " 8x16  "
;  0 0 40 25 E	C |  "   "  16	320 200   " 8x8   "
;  0 x 40 25 E	E |  "   "  64	320 350   " 8x14  "
;  0 x 40 25 V	x |  "   "   "  360 400   " 9x16  "
;  0 x 40 43 E	E |  "   "   "  320 350   4 8x8   "	(4 pgs w/64K)
;  0 x 40 43 V	x |  "   "   "  320 350   " 8x8   "
;  0 x 40 50 V	x |  "   "   "  320 400   8 8x8   4
;  0 1 40 25 C	x |  1	 "  N/A 320 200   2 8x8   8
;  0 1 40 25 E	C |  "   "  16	320 200   " 8x8   "
;  0 0 80 25 C	x |  2	 "  N/A 640 200   4 8x8   4
;  0 x 80 25 m	x |  "   "   "  640 400   " 8x16  8
;  0 0 80 25 E	C |  "   "  16	640 200   " 8x8   "	(4 pgs w/64K)
;  0 x 80 25 E	E |  "   "  64	640 350   " 8x14  "	(4 pgs w/64K)
;  0 x 80 25 V	x |  "   "   "  720 400   " 9x16  "
;  0 x 80 43 E	E |  "   "   "  640 350   8 8x8   4     (2 pgs w/64K)
;  0 x 80 43 V	x |  "   "   "  640 350   " 8x8   "
;  0 x 80 50 V	x |  "   "   "  640 400   " 8x8   "
;  0 1 80 25 C	x |  3	 "  N/A 640 200   4 8x8   "
;  0 1 80 25 E	C |  "   "  16	640 200   " 8x8   8     (4 pgs w/64K)
;  0 x 80 25 M	M |  7	 "   3  720 350   " 9x14  1
;  0 x 80 25 E	M |  "   "   "  720 350   " 9x14  8	(4 pgs w/64K)
;  0 x 80 25 V	x |  "   "   "  720 400   " 9x16  "
;  0 x 80 43 E	M |  "   "   "  720 350   8 8x8   4     (2 pgs w/64K)
;  0 x 80 43 V	x |  "   "   "  720 350   " 8x8   "
;  0 x 80 50 V	x |  "   "   "  720 400   " 8x8   "
;
;******************************************************************************

	INCLUDE switch.inc	;feature switches
	INCLUDE rmacros.inc

	USESEG	_BSS
	USESEG	_DATA		
	USESEG	GR_TEXT
	USESEG	CN_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE llgrp.inc	; Constant definitions
	INCLUDE grmac.inc	;ModeData macros
	INCLUDE oscalls.inc	;Dos 5 structures



sBegin	_DATA			
externW b$UsrCsrTyp		
externB b$InsCsrStop		
sEnd	_DATA			

sBegin	_BSS
;
; ***************************************************************************
; External variables
; ***************************************************************************
;
externB b$BiosMode
externW b$ModeBurst
externW b$CurPages
externB b$ScreenMode
externB b$ScrWidth
externD b$InitPalette		
externB b$MaxAttr
externB b$MaxColor
globalW b$PageTable,,8		;Offset to start of each video page
externB b$OrgBiosMode
externB b$Monitor
externB b$Adapter
externW b$VideoMem
externB b$PageSize
externB b$MaxPage
externB b$ScrHeight
externB b$ForeColor
externW b$FBColors
externB b$CharColor
externW b$CurrPSize
externB b$NullColor		
externW b$DSP
externW b$CURSOR		
externW b$CSRTYP		
;
; ***************************************************************************
; Local variables
; ***************************************************************************
;
externB b$BorderColor		;border color (overscan)
sEnd	_BSS

sBegin	CN_TEXT
externNP B$USRCSR		
sEnd	CN_TEXT

assumes CS,GR_TEXT
sBegin	GR_TEXT

externNP B$InitModeData
externNP B$GetParm
externNP B$EgaPalReset
externNP B$EgaPalResetB	
externNP B$EgaPalPut
externNP B$EgaPalPutB		
externNP B$EgaPalTrans
externNP B$EgaPalSet
externNP b$ColorPalette	
externNP b$EnhPalette		

;
; PAGE SIZES for each screen dimension possibility:
;
; These are calculated here as the BIOS initializes them (for the 25-line
; cases), or as it calculates them (for the 43/50-line cases).	The added
; "slop" separates the pages and apparently eases scrolling troubles
; while increasing our troubles.
;
; NOTE: 4 pages of 80x50 is 8100H bytes, 100H over the 8000H max for the
;	memory map, but 100H is the 256 byte slop between pages, so its OK.
;	BUT, 8 pages of 40x50 is 8500H which is more than the slop will
;	allow so we cut the supported pages in half to 4 in the code.
;	(40x50 could actually support 7 pages, but 4 is a nicer number??)
;

P40x25	EQU	((40*25*2+255) AND (NOT 255)) SHR 4	;0800H bytes
P80x25	EQU	((80*25*2+255) AND (NOT 255)) SHR 4	;1000H bytes
P40x43	EQU	(40*43*2+256) SHR 4			;0E70H bytes
P80x43	EQU	(80*43*2+256) SHR 4			;1BE0H bytes
P40x50	EQU	(40*50*2+256) SHR 4			;10A0H bytes [4]
P80x50	EQU	(80*50*2+256) SHR 4			;2040H bytes [4]

;===========================================================================
mModeData	Mode0Data
;
; SCREEN 0, BIOS modes 0 & 1
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	0
mBiosMode	0		;BIOS mode and burst may be adjusted later
mBurst		0
mScrWidth	40
mScrHeight	25
mHorzRes	320
mVertRes	200
mVideoBase	0B800H
mMaxAttr	15
mMaxColor	63
mPageSize	2		;page size in K
mCurrPSize	P40x25		;page size in paragraphs (1 plane)
mMaxPage	7
mNullColor	7
mForeColor	7
mBackColor	0
mEgaWrMd	0
mInitPalette	b$ColorPalette ;moved to LLXGASUP for sharing
mInitVgaPal	b$VgaPalette	;use regular VGA palette
mAlphaDim	AlphaDim0
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalReset
mPalPut 	B$EgaPalPut
mPalTrans	B$EgaPalTrans
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mEnd		,TextDataLen
;===========================================================================

;===========================================================================
mModeData	Mode2Data
;
; SCREEN 0, BIOS modes 2 & 3
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	0
mBiosMode	2		;BIOS mode and burst may be adjusted later
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	640
mVertRes	200
mVideoBase	0B800H
mMaxAttr	15
mMaxColor	63
mPageSize	4		;page size in K
mCurrPSize	P80x25		;page size in paragraphs (1 plane)
mMaxPage	3
mNullColor	7
mForeColor	7
mBackColor	0
mEgaWrMd	0
mInitPalette	b$ColorPalette ;moved to LLXGASUP for sharing
mInitVgaPal	b$VgaPalette	;use regular VGA palette
mAlphaDim	AlphaDim2
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalReset
mPalPut 	B$EgaPalPut
mPalTrans	B$EgaPalTrans
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mEnd		,TextDataLen
;===========================================================================

;===========================================================================
mModeData	Mode7Data
;
; SCREEN 0, BIOS mode 7
;
; Mode-dependent data follows to initialize the the "b$ModeData" table
; in LLCGRP.
;
;===========================================================================
mScreenMode	0
mBiosMode	7
mBurst		0
mScrWidth	80
mScrHeight	25
mHorzRes	720
mVertRes	350
mVideoBase	0B000H
mMaxAttr	15
mMaxColor	2
mPageSize	4		;page size in K
mCurrPSize	P80x25		;page size in paragraphs (1 plane)
mMaxPage	0
mNullColor	7
mForeColor	7
mBackColor	0
mEgaWrMd	0
mInitPalette	MonoPalette
mInitVgaPal	0		;not applicable
mAlphaDim	AlphaDim7
mSetMode	SetMode
mSetPages	SetPages
mPalReset	B$EgaPalResetB
mPalPut 	B$EgaPalPutB
mPalTrans	PalTrans7
mPalSet 	B$EgaPalSet
mSetColor	SetColor
mEnd		,TextDataLen
;===========================================================================

;
; MonoPalette - used to initialize the EGA palette for SCREEN 0,
;		BIOS mode 7, for a Mono Display.
;
labelB	MonoPalette		;EGA palette for Mono Display, mode 7
	DB	0		;black
	DB	7 DUP (001000B) ;video
	DB	0		;black
	DB	7 DUP (011000B) ;intensified

;*** 
; B$Screen0
;
;Purpose:
;	Establish all relevent mode dependent data values and function
;	vectors for BASIC screen mode 0.
;	NOTE:  No actual change in screen mode occurs until SetMode is called!
;Entry:
;	AL = screen mode (0)
;	AH = burst (0 or 1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates error
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	B$Screen0,<PUBLIC,NEAR>
cBegin
	mov	bx,GR_TEXTOFFSET Mode7Data
	mov	al,7		;assume mono mode
	test	b$Monitor,Monochrome ;is this a monochrome monitor?
	jnz	Scr7		;go if so
	cmp	b$Monitor,AnalogMono ; VGA working only in MONO modes?
	je	Scr7		; go if so
	and	al,b$OrgBiosMode;get entry bios mode and mask down to 3 bits
	cmp	al,7		;if AL=7 then entry mode was 7 or F (both mono)
	je	Scr7		;branch if entry mode was mono.  use mode 7
	xor	al,al		;mode base = 0
	mov	bx,GR_TEXTOFFSET Mode0Data
	cmp	cl,40		;40 columns?
	je	Got40		;go if so w/mode base = 0
	mov	bx,GR_TEXTOFFSET Mode2Data
	mov	al,2		;mode base = 2
Got40:
	add	al,ah		;new BIOS mode = 0/2 if no burst
				;		 1/3 if burst
Scr7:
	mov	cx,TextDataLen	
	push	ax
	call	B$InitModeData ;initialize table data
	pop	ax
	mov	b$ModeBurst,ax ;save new mode and burst
	test	b$Adapter,CGA + MDPA + HGC  ;CGA or MDPA or HGC adapters?
	jnz	SkipPage	;go if so
	mov	ah,7		;8 (max=7) pages for EGA/VGA/MCGA w/25 lines
	test	b$Adapter,EGA	; EGA adapter?
	jz	MaxPg		; brif not, 8 pages is right
	cmp	b$VideoMem,64	; only 64K EGA?
	ja	MaxPg		; go if more, 8 pages is right
	cmp	al,1		;BIOS mode 0 or 1? (40 columns)
	jbe	MaxPg		;go if so, 8 pages is right
	shr	ah,1		;limit to 4 pages (max=3) if 64K EGA & 80x25
MaxPg:
	mov	b$MaxPage,ah	;save corrected max page value
	test	b$Monitor,StdColor ;std color monitor?
	jz	SkipPage	;go if not
	mov	b$MaxColor,15	;it can only handle 16 colors
SkipPage:
	cmp	al,7		;BIOS mode 7?
	jz	ScrExit 	;if mono, don't setup enhanced palette
	test	b$Monitor,AnalogColor + AnalogMono + EnhColor
				;enhanced or analog monitor?
	jz	ScrExit 	;go if not
	;set up enhanced palette for >= enhanced color monitor
	mov	WORD PTR b$InitPalette,GR_TEXTOFFSET b$EnhPalette 
ScrExit:
	mov	al,b$ForeColor ;set character color for text mode
	mov	b$CharColor,al
	mov	b$DSP,770H	;text mode function key attributes
	clc			;indicate no error
cEnd


;***
; AlphaDim0
;
;Purpose:
;	Validate the proposed text dimensions for 40 column Screen mode 0
;	and set up several screen-dependent variables if dimensions are OK.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	If dimensions ok
;	   PSW.C reset
;	   AL = -1
;	   b$MaxPage, b$BiosMode, b$VideoMem, b$CurrPSize,
;	      and b$ScrHeight set appropriately.
;	Else (error)
;	   PSW.C set
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	AlphaDim0,<NEAR>
cBegin
	mov	cl,80		;80 is "other" columns
ADim0_2:			;common entry point from AlphaDim2
	xor	al,al		;flag request for screen 0
	cmp	bl,cl		;"other" columns?
	je	ADim0Exit	;exit if so w/request to reenter screen 0
ADim0_7:			;common entry point from AlphaDim7
	cmp	bl,b$ScrWidth	;same as current?
	stc
	jne	ADim0Exit	;go if not w/error, only 80 or 40 columns
	cmp	bh,25		;25 lines?
	je	ADim0Ok 	;go if so, w/request to reenter screen 0
				;  to get back to 25 lines
	dec	al		;flag request satisfied (or error)
	test	b$Adapter,EGA + VGA ;EGA or VGA?
	stc			;set error, just in case
	jz	ADim0Exit	;go if not w/error, only 25 lines
	test	b$Monitor,StdColor ;disallow StdColor monitor for 43 lines
	stc			;set error, just in case
	jnz	ADim0Exit	;go if StdColor w/error
	cmp	bh,43		;43 lines?
	je	LinesOk 	;43 lines is ok
	test	b$Adapter,VGA	;VGA?
	stc			;set error, just in case
	jz	ADim0Exit	;exit with error if not VGA
	cmp	bh,50		;50 lines?
	stc			;set error, just in case
	jne	ADim0Exit	;exit with error if not 50 lines
	mov	cx,P40x50	;larger page size
	cmp	bl,40		;40 columns?
	je	ADim0_50	;go if so
	mov	cx,P80x50	;larger page size
	jmp	short ADim0_50	
LinesOK:			
	mov	cx,P40x43	;larger page size
	cmp	bl,40		;40 columns?
	je	ADim0_43	;go if so
	mov	cx,P80x43	;larger page size
ADim0_50:			
	mov	b$MaxPage,3	;only 4 pages in 80x43 or 80x50 or 40x50
ADim0_43:
	;64K EGA has half the pages for all text dimensions except
	;40x25, which is handled by B$Screen0 and never reaches here
	cmp	b$VideoMem,64	;only 64K?
	ja	PgOk		;go if more, MaxPage is right
	shr	b$MaxPage,1	;halve number of pages if 64K mode 7
PgOk:				

ADim0Ok:
	mov	b$ScrHeight,bh ;set alpha rows
	clc			;no error
ADim0Exit:
cEnd

;***
; AlphaDim2
;
;Purpose:
;	Validate the proposed text dimensions for 80 column Screen mode 0
;	and set up several screen-dependent variables if dimensions are OK.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	exits through AlphaDim0
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	AlphaDim2,<NEAR>
cBegin
	mov	cl,40		;40 is "other" columns
	jmp	ADim0_2		;rest is just like mode 0 case
cEnd	<nogen>

;***
; AlphaDim7
;
;Purpose:
;	Validate the proposed text dimensions for monochrome Screen mode 0
;	and set up several screen-dependent variables if dimensions are OK.
;Entry:
;	BH = number of lines
;	BL = number of columns
;Exit:
;	exits through AlphaDim0
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	AlphaDim7,<NEAR>
cBegin
	xor	al,al		;setup for request to reenter screen 0
	jmp	ADim0_7		;rest is just like mode 0 case
cEnd	<nogen> 		


;***
; B$FixTextPage
;
;Purpose:
;	Fix up b$CurrPSize (for 43/50 lines) and initialize b$PageTable.
;
;Entry:
;	b$CurrPSize initialized to 25-line default.
;	b$ScrHeight must be initialized (call Alphadim).
;
;Exit:
;	b$CurrPSize updated.
;	b$PageTable initialized.
;
;Uses:
;	per conv.
;
;Preserves:
;	DI.
;
;******************************************************************************
cProc	B$FixTextPage,<PUBLIC,NEAR>,<DI>
cBegin
	xor	ax,ax		; get a zero
	cmp	b$ScreenMode,al	; make sure we're in text mode
	jnz	PSizeExit	; else just return
	push	es
	cmp	b$ScrHeight,25	; 25-line mode?
	je	PSizeOkay	; brif so--b$CurrPSize already set correctly

	mov	es,ax
	mov	ax,es:[CRT_LEN]	; get actual page size from BIOS, in bytes
	mov	cl,4
	shr	ax,cl		; convert to paragraphs
	mov	b$CurrPSize,ax	; and update current page size
PSizeOkay:
	mov	di,offset DGroup:b$PageTable
	mov	bx,[b$CurrPSize]	; size of page in paragraphs
	mov	cl,4
	shl	bx,cl			; convert to size in bytes
	xor	ax,ax			; first page always at offset zero
	mov	cx,8			; assume 8 pages
	push	ds
	pop	es			; es=ds
SetPageTable:
	stosw				; set offset for this page
	add	ax,bx			; compute offset of next page
	loop	SetPageTable
	pop	es
PSizeExit:
cEnd

;***
; SetMode
;
;Purpose:
;	Set screen mode according to the characteristics established
;	by previous call to B$Screenx and b$AlphaDim.  Set 8x8 character
;	font if new mode is to be 43 or 50 lines.
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
	test	b$Adapter,VGA + MCGA ;MCGA or VGA?
	jz	SkipScanSet	;no, don't set scan lines
	mov	ax,1202H	;assume we want 400 scan lines
	cmp	b$ScrHeight,43 ;43 lines mode?
	jne	SetScans	;no, go set to 400 scan lines
	mov	ax,1201H	;set 350 scan lines for 43 line mode
SetScans:			
	mov	bl,30H		;subfunction for set scan lines
	SCNIO			;set the scan lines
SkipScanSet:			
	mov	al,b$BiosMode	;set BIOS mode
	SCNIO	vSetMode
	cmp	b$ScrHeight,43 ;43 lines?
	je	Set8x8		;branch if so
	cmp	b$ScrHeight,50 ;50 lines?
Set8x8: 			
	jne	SetMode25	;go if not
	mov	ax,1112H	;character generator request
	xor	bl,bl		;  to load 8x8 font
	SCNIO			;  which gets 43 lines [2]or 50 lines
	MOV	AX,0707H	; cursor type for 8x8 font
	JMP	SHORT sm_1	
SetMode25:

	MOV	AX,0707H	; assume 7,7 cursor (all bios modes but 7)
	CMP	b$BiosMode,AL	; bios mode 7?  (AL = 7)
	JNE	StoreCsr	; no, use normal cursor
	MOV	AX,0C0CH	; bios mode 7 cursor is 12,12
sm_1:				
StoreCsr:			
	MOV	b$UsrCsrTyp,AX	; store cursor type
	MOV	b$UsrCsrTyp,AX	; store cursor type
	MOV	b$InsCsrStop,AL ; and make insert cursor match
	MOV	DX,b$CURSOR	; Get current cursor position
	MOV	BYTE PTR b$CSRTYP,-1 ; invalidate present cursor type so it
				; will get changed
	CALL	B$USRCSR	; display user cursor
NoStore:
cEnd

;***
; SetPages
;
;Purpose:
;	Set the current active and visual pages for text modes.
;Entry:
;	AL = active page
;	AH = visual page
;Exit:
;	None
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	SetPages,<NEAR>
cBegin
	mov	b$CurPages,ax	;save page numbers
	mov	al,ah
	SCNIO	vSelActivePage	;set visual page
cEnd

;***
; PalTrans7
;
;Purpose:
;	Translate a user supplied color for monochrome Screen mode 0
;	to the corresponding hardware value after verifying that the
;	attribute value and the color value are in the legal ranges.
;	    Color mapping:  0 --> 0
;			    1 --> 01000B
;			    2 --> 11000B
;Entry:
;	DX:AX = user supplied color value
;	BL    = user supplied attribute value
;Exit:
;	PSW.C set if illegal value, reset if Ok
;	DX:AX = translated color value
;	BL    = unchanged
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	PalTrans7,<NEAR>
cBegin
	cmp	bl,b$MaxAttr	;is legal attribute ?
	ja	PalTrErr	;error return
	or	dh,dl		;hi word of color 0?
	or	dh,ah		;hi byte of lo word of color 0?
	jnz	PalTrExit	;error if not
	cmp	al,b$MaxColor	;is legal color ?
	ja	PalTrErr	;error return
	cmp	al,1		;translate color
	jb	PalTrExit	;  0 ->     0
	mov	al,01000B	;  1 -> 01000B
	je	PalTrExit
	mov	al,11000B	;  2 -> 11000B
PalTrExit:
	clc			;no error
	ret
PalTrErr:
	STC			;indicate error
cEnd

;***
; SetColor
;
;Purpose:
;	Process the color statement for text modes (BASIC Screen mode 0).
;	Syntax for text mode color statement is as follows:
;
;		COLOR	[foreground],[background],[border]
;
;	where "foreground" is the attribute to be used for the foreground
;			   (0-15 are normal, 16-31 are 0-15 plus blinking)
;	  and "background" is the attribute to be used for the background
;	  and "border"	   is the color to be used for the overscan border
;
;	Any omitted parameter(s) indicate no change for that parameter.
;Entry:
;	parameter list
;		WORD 1 = flag 0 if param not present
;		WORD 2 = parameter if WORD 1 <> 0, else second param flag
;		etc.
;Exit:
;	PSW.C set if error, reset if Ok.
;	b$FBColors is set to foreground/background attributes
;	b$CharColor is set to character attribute (fg & bg in one byte)
;	b$NullColor is set to null attribute (same as character attribute)
;Uses:
;	per conv.
;Exceptions:
;******************************************************************************
cProc	SetColor,<NEAR>
cBegin
	MOV	DX,b$FBColors	;dl=ForeColor dh=BackColor
	MOV	BH,b$BorderColor
	JCXZ	SetColErr	;error if no params
	jmp	short SetCol1
SetColErr:
	stc
	jmp	SetColExit	
SetCol1:
	cCall	B$GetParm	;[AL] = parm if not psw.z
	JZ	TXTCL1		;brif no parm given
	CMP	AL,31		;is foreground color in range [0-31]?
	JA	SetColErr	;error if not
	XCHG	AL,DL		;save new foreground color
TXTCL1:
	cCall	B$GetParm	;get bkgrnd color if specified
	JZ	TXTCL2		;brif no parm given
	CMP	AL,15		;range check background color [0-15]
	JA	SetColErr	;brif out of range
	XCHG	AL,DH		;save new background color
TXTCL2:
	cCall	B$GetParm	;get border color if specified
	JZ	TXTCL3		;brif no parm given
	JCXZ	NoError
	JMP	SHORT SetColErr
NoError:
	CMP	AL,15		;range check border color [0-15]
	JA	SetColErr	;brif out of range
	XCHG	AL,BH		;save new border color
TXTCL3:
	cmp	b$BiosMode,7	;mode 7?
	jne	SetColOk	;skip if not
	test	b$Adapter,EGA	;EGA?
	jz	SetColOk	;skip if not
	;this fixes a bug in the EGA card mode 7 ???
	;if Fg=0/8/16/24 and Bg=7/15 let it go else force Bg to 0
	mov	al,dh		;BackColor
	not	al		;low 3 bits = 0 iff BackColor = 7|15
	or	al,dl		;low 3 bits = 0 iff ForeColor = 0|8|16|24
	and	al,7		;low 3 bits = 0 iff both
	jz	SetColOk
	xor	dh,dh		;in all other cases make Bg = 0
SetColOk:
	MOV	AX,DX		;get fg/bk colors in AX
	MOV	b$FBColors,AX	;new fore and back
	XCHG	DH,DL		;swap forground/background colors
	AND	DH,0FH		;strip off blink
	SHL	DL,1		;shift background intensity bit into d4
	AND	DL,10H		;leave intensity
	OR	DL,BH		;[DL] = border color + background intensity
	AND	AH,7		;strip background intensity from char attribute
	MOV	CL,4
	SHL	AH,CL		;move background color bits to d6-d4 for char
				;attribute
	TEST	AL,10H		;is blink enabled?
	JZ	NOBLNK		;br. if not
	OR	AH,80H		;add blink
NOBLNK:
	OR	AH,DH		;add foreground color
	MOV	CH,AH		;place char attribute in CH
	MOV	b$NullColor,AH ;char attribute
	MOV	b$CharColor,AH ;null attribute
	TEST	b$Monitor,AnalogColor + AnalogMono + EnhColor
				;enhanced or analog monitor?
	jnz	NO_BORDER	;Brif so

	MOV	b$BorderColor,BH;new border color
	MOV	BL,DL		;need border color in BL
	XOR	BH,BH		;zero BH
	SCNIO	vSetPalette	;set border color
NO_BORDER:
	clc			;indicate no error
SetColExit:
cEnd

sEnd	GR_TEXT

	END
