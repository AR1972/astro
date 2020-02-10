	TITLE	LLCSCN - GW-BASIC Core Screen Interface
;***
; LLCSCN - GW-BASIC Core Screen Interface
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This file contains code for screen manipulations that is needed
;	for all programs.
;
;*****************************************************************************
	INCLUDE switch.inc	;switches
	INCLUDE rmacros.inc	

	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	CN_TEXT 	
	USESEG	CONST		
	USESEG	GR_TEXT
	USESEG	RT_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE oscalls.inc	
	INCLUDE idmac.inc	
	INCLUDE intmac.inc
	INCLUDE llgrp.inc
	INCLUDE ibmunv.inc

sBegin	_BSS

	externB b$Adapter	
	externB b$ScrHeight


	externB b$OrgEquip	
	externB b$BiosMode	
	externB b$ScreenMode
	externB b$Burst
	externB b$ScrWidth
	externB b$MaxPage
	externW b$OrgScrBst	
	externW b$OrgScrDim	
	externW b$OrgCsrTyp	
	externW b$OrgPages	
	externB b$KEY_SW	
	externW b$SetMode
	externW b$AlphaDim
	externW b$PalReset
	externW b$SetPages

;***
;b$ScreenRestored - Flag to indicate that the screen mode has been changed
;OEM-interface routine (variable)
;
;Purpose:
;	This flag is used to signal the interpreter that we have changed
;	the screen mode in B$RESETSCN.	The interpreter UI uses it during
;	RUN time initialization to decide if it needs to throw away its
;	saved image of the user output screen.
;
;	This variable is only needed in a version of the runtime that
;	supports the QB Interpreter.
;
;Allocation:
;	b$ScreenRestored is a BYTE value declared in the _BSS segment
;	by the OEM.
;
;Values:
;	0 - Screen Mode did not change
;	1 - Screen Mode changed
;
;
;Initially Set:
;	This variable is initialized to 0 with the rest of the _BSS segment.
;
;Modified By:
;	The variable should only be modified by the runtime during execution
;	of the routine B$RESETSCN.  It may be modified by the Interpreter
;	at any point.
;
;Used by:
;	b$ScreenRestored is used by the interpreter for screen management.
;****
globalB b$ScreenRestored,,1	;non-0 if screen changed in B$RESETSCN


sEnd	_BSS


sBegin	_DATA

	globalW b$CSRTYP,-1,1	; CURRENT cursor type (start and stop lines)
				; Low byte = -1 if actual cursor type is
				; unknown.

;***
;b$CurPages, b$ActPage - Display pages
;OEM-interface routine (variable)
;
;Purpose:
;	b$CurPages contains the current active and visual display
;	page.  The visual display page is also accessible by the
;	variable b$ActPage.  The high byte of b$CurPages is the
;	number of the current visual page and its low byte is the
;	number of the current active page.
;
;	This value is only used by OEM independent code during termination
;	when trying to display the message "Hit any key to return to system".
;	When this is done, we have to make sure that the message gets
;	displayed on the visual page.  To do this, we copy the high order
;	byte of b$CurPages (the visual page) to b$ActPage.  A call to
;	B$GETCSRDATA is then done to get the cursor position of the
;	(possibly new) active page. After displaying the message, the visual
;	page is restored by another change to b$ActPage.
;
;	One major effect of this use of b$ActPage is that the OEM-dependent
;	code has to be written such that it can handle an asynchronious
;	change to this variable.
;
;Allocation:
;	b$CurPages is a WORD value declared in the _DATA segment
;	by the OEM. b$ActPage is a BYTE value.	It is the low order
;	byte of b$CurPages.
;
;Values:
;	The values of the two halves of b$CurPages are dependent on
;	the current screen mode and the hardware installed in the
;	computer.
;
;Initially Set:
;	The OEM-Dependent code is responsible for initially setting
;	this variable. It must be initialized such that, at termination,
;	if the active display page is not the visual display page (as
;	indicated by b$CurPages (HI) != b$CurPages (LOW)), changing
;	the value of b$ActPage will be sufficient to indicate that the
;	pages must be changed.
;
;Modified By:
;	By OEM only, except during termination as noted above.
;
;Used By:
;	OEM and termination code.
;****

labelW	<PUBLIC,b$CharPage>	;b$CharColor and b$ActPage in one word
	globalB b$CharColor,7,1 ;encoded color attributes for character
labelW	<PUBLIC,b$CurPages>	;b$ActPage and b$VisPage in one word
	globalB b$ActPage,0,1	;active display page
	globalB b$VisPage,0,1	;visual display page

	externW b$CurrPSize	;Current page size in paras
	externW b$UsrCsrTyp	; user-defined cursor


globalW b$ScreenX,<OFFSET DGROUP:ScreenTb>,1 ; Indirect pointer to screen mode table
				; in DGROUP

labelW	ScreenTb		; mode-dependent SCREEN set-up
	DB	0		; Maximum screen mode (default=0)
	DW	B$Screen0

	externW b$CURSOR
	externW b$vKEYDSP	; vector for indirect call to B$KEYDSP
	externB b$UsrCsrStart	; user-defined cursor start line
	externB b$UsrCsrStop	; user-defined cursor stop line
	externW b$InsCsrTyp	; insert mode cursor (init to 1/2 block)
	externB b$InsCsrStop	; insert cursor stop line
	externW b$MapXYC	
	externD b$AddrC 	
	externB b$ForeMapped	
	externB b$WDOTOP	; top line of text window (1-relative)
	externB b$WDOBOT	; bottom line of text window (1-relative)
	externB b$CRTWIDTH	; rightmost column (1-relative)
	externB b$NullColor	;encoded color attributes for blank/_BSCROLL
	globalB b$UsrCsrOn,0,1	; user cursor status (1=on, 0=off)
	globalW b$pOgaCsr,B$NearRet ; pointer to Olivetti version of GRPCSR

sEnd	_DATA

OffCsrTyp EQU 2707H		; valid cursor start and stop lines, with
				; bit 5 set to indicate cursor off



sBegin	GR_TEXT
externNP B$Screen0
externNP B$FixTextPage				
sEnd	GR_TEXT

sBegin	CONST			

	staticB BLTLOT,0,4	;Half Box Character
	staticB ,255,12 	;Whole Box Character

sEnd	CONST			



sBegin	RT_TEXT 		
	externNP B$GRMODE	
	externNP B$VIEWINIT	
	externNP B$ChkMonitor	; set VGA mono/color monitor

sEnd	RT_TEXT 		


sBegin	CN_TEXT 		
	assumes CS,CN_TEXT	



;=============== Code from here to next marker moved from LLSCNIO.ASM

	externNP B$OutWord

	externNP B$SCINIT	; Delayed screen initializer
	externNP B$SCNSWI
	externNP B$ERR_FC
	externNP B$SCNCLR
	externNP B$NearRet	; a public NEAR RET
	externNP B$SetAdapter





;***
;B$SCNIO
;
;Purpose:
;
;Entry:
;
;Exit:
;
;Uses:
;
;Exceptions:
;
;****
cProc	B$SCNIO,<PUBLIC,NEAR>
cBegin
	SCNIO			;Screen I/O saving: BP,DI,SI
cEnd


;***
;B$SELAPG - select active page
;B$SELVPG - select visual page
;
;Purpose:
;	Set the current BIOS active page to be either the page number in
;	ActPage (for B$SELAPG) or VisPage (for B$SELVPG).
;
;Entry:
;	b$ActPage
;	b$VisPage
; WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
; WARNING!                                                       WARNING!
; WARNING!		 DS may not always = DGROUP		 WARNING!
; WARNING!                                                       WARNING!
; WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING! WARNING!
;Exit:
;	[AL] = active page
;Uses:
;
;Preserves:
;	BX,CX,DX
;
;Exceptions:
;	none
;
;******************************************************************************

cProc	B$SELVPG,<PUBLIC,NEAR>
cBegin
	MOV	AL,SS:[b$VisPage]
	JMP	SHORT SELPAG
cEnd	<nogen> 		;End of B$SELVPG

cProc	B$SELAPG,<PUBLIC,NEAR>
cBegin
	MOV	AL,SS:[b$ActPage] ; [al] = active page #
SELPAG:


	PUSH	DX
	MOV	DX,SS:[b$CurrPSize] ;Get page size (set by B$SCRSTT) in paras
	SHL	DX,1		; Make it byte count
	SHL	DX,1		; * 4
	SHL	DX,1		; * 8
	SHL	DX,1		; * 16
	XOR	AH,AH
	PUSH	DS		; save current data segment
	PUSH	AX		; save new active page number
	MUL	DX		; [ax] = page No. * page size
	MOV	DS,DX		; [ES] = 0, since [DX] = 0 after multiply
	MOV	DS:CRT_START,AX ; store active page offset
	POP	AX		; restore page number
	MOV	DS:ACTIVE_PAGE,AL ; and store it in BIOS data area
	POP	DS		; restore data segment
	POP	DX
SELPAX:

cEnd				;End of B$SELAPG/B$SELVPG


;***
;B$GETCSRDATA -- Get current cursor position and type.
;OEM-interface routine
;
;Purpose:
;	Returns in DX the current screen cursor position and cursor type
;	for the current active page as reported by BIOS/VIO.  The cursor
;	type is returned with the low order byte containing the stop line
;	and the high order byte containing the line of cursor block in
;	which to start the cursor.
;
;	When in a graphics mode under OS/2, the cursor position returned
;	is the last cursor position (b$CURSOR), and the type returned in
;	CX is invalid.	Note that under DOS, the cursor type must always
;	be a legal even when bogus values are returned from DOS (i.e.
;	for a graphics screen).
;
;Entry:
;	None.
;
;Exit:
;	DX = Current screen cursor position.
;	CX = Current screen cursor start and stop lines.
;
;Uses:
;	Per Convention.
;
;Preserves:
;	AX, BX
;
;Exceptions:
;	None.
;**********************************************************************
cProc	B$GETCSRDATA,<PUBLIC,NEAR>,<AX,BX>
cBegin
	MOV	BH,b$ActPage	;return cursor for current active page
	SCNIOS	vReadCursorPos	; get cursor position and type from BIOS
	INC	DX		; make row 1-relative
	XCHG	DH,DL		; want row in DL, column in DH
	INC	DX		; make column 1-relative

	CMP	CH,CL		; is cursor type read valid? (CH <= CL)
	JBE	CURSOR_OK	; brif so
	MOV	CX,b$UsrCsrTyp	; return a known cursor
CURSOR_OK:
	MOV	BYTE PTR b$CSRTYP,-1 ; invalidate low-level current cursor
				; type so it will get reset.
cEnd


;***
;B$SCNLOC - Locate cursor on physical screen, saving new coordinates
;OEM-interface routine
;
;Purpose:
;	Moves the cursor on the screen and save the final location
;	in b$CURSOR.  The coordinates are 1,1-relative to the physical
;	screen.  The cursor is displayed only if it is currently visible.
;
;Entry:
;	DH=column, DL=line (1,1 is home)
;
;Exit:
;	Updates b$CURSOR.
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****
;***
;B$OFFCSR, B$OVWCSR, B$INSCSR, B$USRCSR - Set Cursor Type and Location
;OEM-interface routine
;
;Purpose:
;	These routines display the appropriate cursor at a specified
;	location as given by the input parameters.  A request can also
;	be made to not display a cursor.  There are currently three
;	different types of cursors that are used, User Cursor, Insert
;	Cursor, and Overwrite Cursor.  The user cursor, which can be
;	modified by the LOCATE statement, is the one that is displayed
;	for all but editing.  When editing a line of input, either the
;	Overwrite or Insert cursor will be used depending on whether
;	we are overwriting or inserting characters.  The Overwrite
;	cursor is identical to the User Cursor, except that it can
;	not be turned off with the LOCATE statement.
;
;
;	Initially, the User Cursor and Overwrite cursors are a full
;	block, and the insert cursor is a 1/2 block.  Under DOS, the
;	cursor has to be handled separately if we are in a graphics
;	mode since the IBM BIOS does not support a cursor font in
;	graphics mode.	Note that none of this has anything to do with
;	the graphics cursor, which is a one pixel location on the
;	current graphics screen and is never displayed.
;
;	NOTE: If you are supporting delayed screen initialization,
;	      B$SCNINIT has to be called before any changes are
;	      made.
;
;Algorithm:
;
;    B$INSCSR - Turn insert cursor on and move to specified location
;
;    B$OFFCSR - Turn cursor off and move invisible cursor to specified
;		 location
;
;    B$USRCSR - Turn on user cursor if specified as being visible in
;		 the last call to B$CSRATR, otherwise do not display
;		 a cursor.  Move (possibly invisible) cursor to specified
;		 location if the cursor is on.  If the cursor is off,
;		 DO NOT move the cursor (for speed).
;
;    B$OVWCSR - Turn overwrite cursor on and move to specified location.
;		 The overwrite cursor has the same specifications as the
;		 user cursor, but does not obey the visibility parameter
;		 specified in B$CSRATR.
;
;Entry:
;	[DL] = new 1-relative line number
;	[DH] = new 1-relative column number
;	It is assumed that the values in DX are in range.
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX,CX,DX
;
;Exceptions:
;	None.
;****
;***
;B$CSRDSP - Change Cursor Type and Location
;
;Purpose:
;	This routines display the specified cursor at a particular
;	location as given by the input parameters.  It may also
;	request to not display a cursor.  The text mode graphics cursor
;	will be implemented as a full block for user cursor requests,
;	and as a 1/2 block for insert modes.  The graphics cursor will
;	be handled separately (for DOS3) since the IBM BIOS does not
;	support a cursor font in graphics mode.
;
;	Entry points B$OFFCSR, B$OVWCSR, B$INSCSR, and B$USRCSR	[16]
;	exist to set AX to the appropriate value before falling through	[16]
;	into B$CSRDSP, in order to save code space. (Documented above)
;
;Algorithm:
;
;    B$CSRDSP:
;	if not OS/2
;	  if graphics request then
;	    if previous cursor valid
;	      if previous cursor is not an off cursor
;	        erase previous cursor using xor function on previous cursor
;	        and the screen.
;
;	convert 1-relative row and col to 0-relative row and column
;	swap row and col registers for bios call
;	get active screen page number
;	move cursor to desired location
;	swap start and stop lines
;	if text mode or OS/2
;	  if cursor type request != current cursor type
;	    change cursor to new type
;	else (DOS3 graphics modes)
;	  if requested cursor is not for an off cursor
;	    display requested cursor using an xor function on requested cursor
;		and the screen.
;	store new cursor position and type
;
;Entry:
;	b$CSRTYP = current cursor position
;	[AX] = new cursor type
;	[DL] = new 1-relative line number
;	[DH] = new 1-relative column number
;	It is assumed that the values in DX are in range.
;
;Exit:
;	b$CSRTYP = current cursor type (start and stop lines)
;
;Modifies:
;	None
;
;Preserves:
;	BX,CX,DX
;
;Exceptions:
;	None.
;****
CSRGRPH:
				;here if screen mode graphics
	PUSH	AX		;save csr request for call
	MOV	AX,b$CSRTYP	; get previous cursor type
	CMP	AL,-1		; unknown previous cursor type?
	JZ	NO_CURSOR	; brif so -- no cursor to erase
	CALL	GRPCSR		;erase previous cursor
NO_CURSOR:
	POP	AX		;restore csr request
	JMP	SHORT TXTCSR

labelNP <PUBLIC,B$SCNLOC>	; update cursor position variable
	MOV	b$CURSOR,DX	; and fall into SLOWUSRCSR.

labelNP <PUBLIC,B$SlowUsrCsr>
	CMP	[B$UsrCsrOn],0	; should users cursor be displayed?
	JZ	B$OFFCSR	; brif not -- turn off cursor

USRCSR:
	CMP	b$ScreenMode,0	; is screen mode graphics?
	JNZ	B$OFFCSR	; brif so -- use off cursor

labelNP <PUBLIC,B$OVWCSR>	; display overwrite cursor
	MOV	AX,b$UsrCsrTyp	; get user defined cursor
	JMP	SHORT B$CSRDSP	; and display it

labelNP <PUBLIC,B$INSCSR>	; display insert mode cursor
	MOV	AX,b$InsCsrTyp	; get insert mode cursor
	JMP	SHORT B$CSRDSP	; and display it

labelNP <PUBLIC,B$USRCSR>	; conditionally display user cursor
	CMP	[B$UsrCsrOn],0	; should users cursor be displayed?
	JNZ	USRCSR		; brif so -- check for graphics mode
	CMP	b$CSRTYP,OffCsrTyp ; cursor already off?
	JNE	B$OFFCSR	; brif not -- turn it off now
	RET			; otherwise, don't position or check type.
				; check type.

; This REALLY speeds up print statements, but will usually NOT update the
; hardware cursor!  To FORCE the cursor to get reset, invalidate the cursor
; type by moving -1 into b$CSRTYP.

labelNP <PUBLIC,B$OFFCSR>	; turn cursor off
	MOV	AX,OffCsrTyp	;  get off cursor


cProc	B$CSRDSP,<NEAR,PUBLIC>,<BX,CX>
cBegin

	PUSH	AX		; save registers
	PUSH	DX
	CALL	B$SCINIT	; make sure screen is initialized.
	cmp	b$ScreenMode,0	;is screen mode graphics?
	JNZ	CSRGRPH 	;br. if so
TXTCSR:

	DEC	DX		; DL = 0-relative row
	XCHG	DH,DL		;swap for BIOS call
	DEC	DX		; DL,DH = 0-relative column,row
	MOV	BH,b$ActPage	;get active page
	PUSH	AX		;save cursor type
	SCNIOS vSetCursorPos	; issue BIOS call to move cursor
	POP	AX		;recover cursor type
	cmp	b$ScreenMode,0	;Text mode ?
	JNZ	NOT_TEXT_MODE	; Brif not -- generate graphics cursor
	CMP	AX,b$CSRTYP	; (speed) need to change the cursor type?
	JE	DSPRET		; (speed) brif not -- just return
	XCHG	AX,CX		; CX = cursor type
				; CH = start line, CL = stop line


	SCNIOS	vSetCursorType	; display requested cursor
; The bios does not set the cursor type right for IBM EGA cards
; when trying to get an underline cursor when not in 25-line mode.  So
; we have to set it ourselves.  We do the BIOS call beforehand to update
; the internal BIOS cursor type variable.
	TEST	CH,20H		; cursor off request?
	JNZ	NoChangeType	; brif so -- don't reset type
	TEST	b$Adapter,EGA	; EGA adapter?
	JZ	NoChangeType	; brif not -- don't reset type
	CMP	b$ScrHeight,25	; 25-line mode?
	JZ	NoChangeType	; brif not -- don't reset type
	MOV	DX,03D4h	; EGA port
	XCHG	AX,CX		; AH = start line
	MOV	AL,0Ah		; select start line register
	OutWord 		; write correct value - macro for AT&T 6300
NoChangeType:

DSPRET:
	POP	DX		; restore new position and type
	POP	AX
	MOV	b$CSRTYP,AX	; store current cursor type
cEnd


NOT_TEXT_MODE:
	CALL	GRPCSR		; generate graphics cursor
	JMP	DSPRET		; exit

;*** 
; GRPCSR -- Toggle a generated graphics mode text cursor (DOS 3 only)
;
;Purpose:
;
;Entry:
;	AX = cursor type
;
;Exit:
;	None
;Uses:
;	AX,BX,CX
;
;Preserves:
;	DX
;
;Exceptions:
;	None
;
;******************************************************************************

cProc	GRPCSR,<NEAR>
cBegin
	CMP	AX,OffCsrTyp	; cursor off request?
	JE	GRPRET		; brif so -- return without doing anything
	PUSH	ES		; save ES
	mov	bl,[b$BiosMode] ; check BIOS mode
	cmp	bl,13h		; VGA mode 13h?
	je	Cursor13	; go if so
	XOR	CX,CX		; CX = 0
	MOV	ES,CX		; ES = 0
	CMP	BL,40h		; Is it Olivetti 640x400 mode?
	JNE	NotCsr40h	; brif not

; The Olivetti VGA handles character fonts normally in mode 40h, but we
; special-case their CGA and EGA which use a MASTER TABLE POINTER at 40:84.

	TEST	[b$Adapter],CGA+EGA; Is it CGA or EGA Olivetti?
	JZ	NotCsr40h	; brif not (ie. Olivetti VGA)

;Note: B$OgaCsr requires ES = CX = 0 on entry

	CALL	[b$pOgaCsr]	; use Olivetti version of routine
	JMP	SHORT GrpRet2
NotCsr40h:
	PUSH	ES:CHREXT	;Save Old Char Extention Ptr
	PUSH	ES:CHREXT+2
	MOV	ES:WORD PTR CHREXT,OFFSET DGROUP:BLTLOT
	MOV	ES:CHREXT+2,DS	;Segment Addr of Box Char

	CMP	AX,b$InsCsrTyp	; is it an insert cursor?

	PUSHF			; save check for insert mode
	cmp	b$BiosMode,8	;Hercules graphics mode?
	je	UseEga		;use EGA code if so
	CMP	b$BiosMode,0Dh	;current screen mode EGA?
	JB	NotEga		;brif not Ega mode
UseEga:
	MOV	BL,b$ForeMapped ;use current color
	OR	BL,080H 	;set XOR bit
	MOV	AL,0DCh 	;half height block character for EGA
	POPF			;insert mode?
	JZ	GRPCS2		;brif insert cursor
	DEC	AL		;make overwrite cursor
	JMP	SHORT GRPCS2
NotEga:
	POPF			;insert mode?


	MOV	AL,128		;Char code for Box
	JZ	GRPCS1		;brif it is insert cursor
	INC	AL		;Map to 128 or 129..
GRPCS1: 			;Half Box (128) If INS_MODE
				;Whole Box (129) for normal (NOT INS_MODE)
	MOV	BL,87H		;Select Invert (xor) Mode
				;bit 7 of [BL] = 1 for XOR mode and
				;low nibble =7, bcos we need low intensity white

GRPCS2:
	MOV	BH,b$ActPage	; Active Page
	INC	CX		; CX = 1 -- One Character
	SCNIOS	vWriteChar
	POP	ES:CHREXT+2
	POP	ES:CHREXT	;Restore Char Extention ptr
GrpRet2:
	POP	ES		;restore register
GRPRET:
cEnd

;
;BIOS mode 13H (SCREEN 13) does not perform XOR drawing of characters.
;We must therefore draw the cursor the hard way, XORing the 8x8 character
;cell with the desired cursor color.
;
Cursor13:
	push	dx		;preserve DX
	push	ax		;save cursor type
	SCNIOS	vReadCursorPos	;DH=cursor row, DL=column
	xor	ax,ax
	xchg	al,dh		;AX=row, DX=col
	mov	cl,3		;*8
	shl	ax,cl		;AX=Y coordinate of character
	shl	dx,cl		;DX=X coordinate of character
	mov	cx,dx		;CX=X
	mov	dx,ax		;DX=Y
	call	[b$MapXYC]	;convert X,Y to buffer offset
	mov	al,b$ForeMapped ;get cursor color
	mov	ah,al		;replicate for 2 pixels
	les	bx,b$AddrC	;buffer address for character start
DbAssertRel    ES,E,0A000H,RT_TEXT,<Buffer segment invalid in B$CSRDSP> 
	mov	cx,8		;8 raster lines down per character
	pop	dx		;restore cursor type
	cmp	dx,b$InsCsrTyp	;is it an insert cursor?
	jne	NextRaster	;go if not for full-block cursor
	shr	cx,1		;half-block cursor
	add	bx,320*4	;start 4 rasters down, do half as many
NextRaster:
	push	cx
	mov	cx,4		;8 pixels (4 words) across per character
NextWord:
	xor	word ptr es:[bx],ax ;xor 2 pixels with color
	inc	bx		;to next word
	inc	bx
	loop	NextWord	;do all 8 pixels
	pop	cx
	add	bx,320-8	;down to next raster and back to left
	loop	NextRaster
	pop	dx		;restore DX
	jmp	short GrpRet2


;***
;B$SCROLL - Scroll the screen by one line
;OEM-interface routine
;
;Purpose
;	Scrolls up one line an area of the screen with a left,top corner of
;	(1,b$WDOTOP), and a right,bottom corner of (b$CRTWIDTH,b$WDOBOT)
;
;Entry:
;	b$WDOTOP, b$WDOBOT, b$CRTWIDTH set.
;
;Exit:
;	None
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$SCROLL,<PUBLIC,NEAR>,<AX,BX,CX,DX>
cBegin

	XOR	CX,CX		; CL = left column (0-relative)
	MOV	CH,b$WDOTOP	; CH = top row
	DEC	CH		; make it 0-relative
	MOV	BL,b$CRTWIDTH	; BL = right column
	MOV	BH,b$WDOBOT	; BH = bottom row
	DEC	BX		; make column 0-relative
	DEC	BH		; make row 0-relative

OLD_SCROLL:			; former entry point
;	[BH] = Window bottom row (0-relative)
;	[BL] = Right column of scroll window (0-relative)
;	[CH] = Window top row (0-relative)
;	[CL] = Left column of scroll window (0-relative)

	cCALL	B$SELAPG	; Active Visual Page

;	Determine if scrolling is Horizontal or Vertical by comparing
;	source line and destination line
;	Source line will can be equal to destination line if VIEW PRINT n TO n
;	is used.

;	Use BIOS/DOS support to scroll a window of any size

	MOV	DX,BX		;copy bottom right coordinates
	MOV	BH,b$NullColor	;get scroll attribute
	XOR	AX,AX		; zero lines default	

	CMP	CH,DH		; only one line to scroll?
	JE	ScrollSet	; brif so -- just clear screen
	INC	AX		; Set number of lines to scroll as 1
ScrollSet:			
	SCNIOS	vScrollPageUp	;SCROLL page up

SCROLL_RET:			; All done .. Exit after setting current
	cCALL	B$SELVPG	; Active Visual Page

cEnd				; End of B$SCROLL

;===================================


;*** 
; B$ErrorReturn
;
;Purpose:
;	Return carry set, indicating error.
;
;Entry:
;	None.
;
;Exit:
;	PSW.C = set
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;******************************************************************************

cProc	B$ErrorReturn,<PUBLIC,NEAR>
cBegin
	stc
cEnd

;*** 
; B$ScreenN
;
;Purpose:
;	Invoke b$ScreenX[AL] to initialize mode data for the screen mode
;	specified in AL.
;
;Entry:
;	AL = Screen Mode
;	AH = Burst (0/1)
;	CL = alpha columns
;Exit:
;	PSW.C = set indicates an error (mode not supported)
;Uses:
;	per convention
;Preserves:
;	AX, BX, CX, DX.
;Exceptions:
;******************************************************************************

cProc	B$ScreenN,<PUBLIC,NEAR>,<AX,BX,CX,DX> 
cBegin
	MOV	BX,[b$ScreenX]  ; BX = DGROUP offset of screen mode table
	CMP	[BX],AL 	; invalid mode?
	JB	ScrNExit	; brif so -- exit with carry set
	MOV	DL,AL		; Form screen mode in DX
	XOR	DH,DH		; [DX] = screen mode
	ADD	BX,DX
	ADD	BX,DX		; [DS:BX] = pointer-1 to pointer to routine
	CALL	word ptr [BX+1]	; set mode-dependent data
ScrNExit:
cEnd

;***
;B$GetParm - get a parameter from the list passed from high levels
;OEM-interface routine
;
;Purpose:
;	This routine gets a parameter from the list pointed to by SI.
;	The first word of the list is a flag specifying if a parameter
;	was specified.	If the flag is non-zero the next word in the
;	list is the parameter.	Otherwise the next word in the list
;	is the flag for the next parameter.  The list grows from high
;	memory down.  This routine will not modify the list.
;
;	For example, if CX = 11,
;
;	---------------------------------------------------------
;	| 51 |	~0|  4 | ~0 | 255|  ~0 |  0 |  0 | 23 | ~0 | 0	|
;	---------------------------------------------------------
;							      ^
;							     SI
;	will return the following on successive calls:
;
;		   CX	   AL	   AH	   PSW.Z
;
;		   10	    -	    1	     reset
;		    8	   23	    0	     reset
;		    7	    -	    1	     reset
;		    6	    -	    1	     reset
;		    4	  255	    0	     reset
;		    2	    4	    0	     reset
;		    0	   51	    0	     reset
;		    0	    -	    1	     set
;
;	NOTE: Although both the flags and values are WORDs, any value
;	      that is greater than 255 will cause an Illegal Function
;	      Call. It is assumed that the list will be in a proper
;	      format.
;
;Entry:
;	[SI] - points to high end of parameter list
;	       (parameters run from high to low memory)
;	[CX] - count of words left in list
;
;Exit:
;	[SI] - points to next parameter in list
;	[CX] - modified count of words left in parameter list
;	[AL] - parameter, if one was specified
;	[AH] - 1 if parameter was missing or defaulted, 0 otherwise
;	PSW.Z - set means no parameter was found
;
;Uses:
;	SI is used as a return value
;
;Preserves:
;	BX, DX
;
;Exceptions:
;	B$ERR_FC if the value of the parameter > 255.
;******************************************************************************
cProc	B$GetParm,<PUBLIC,NEAR>
cBegin
	mov	ah,1		;ah=1 means (missing or) defaulted
	STD			;from high to low....
	OR	CX,CX		;unfortunately, JCXZ doesn't set 0 flag
	JZ	GetParmExit	;brif list empty
	LODSW			;get parameter flag

	DEC	CX		;one less word in list....
	OR	AX,AX		;was param defaulted?
	mov	ah,1		;ah=1 means (missing or) defaulted
	JZ	GetParmExit	;brif so...
DbAssertRel    CX,NZ,0,CN_TEXT,<Parameters out of sync in B$GetParm>

	LODSW			;get parameter
	OR	AH,AH		
	JNZ	BadParm 	;Don't allow parms > 255
	DEC	CX		;one less word in list...
	OR	SP,SP		;got a parameter...
GetParmExit:
	CLD
cEnd

BadParm:			
	CLD			
	JMP	B$ERR_FC	;illegal function call


;***
;B$SETSCNDATA -- Set mode-dependent data for initialization.
;
;PURPOSE:
;	This sets up all the mode-dependent data, both high and low-level.
;	The startup screen mode or special mode characteristics (such as
;	43-line mode) may not be supported due to runtime version features
;	or mode-dependent module stubbing.  Default mode characteristics
;	are used if the screen mode is available.  Screen 0 is used if the
;	startup mode itself is not supported.
;	Major rewrite with [22].
;
;ENTRY:
;	AL = Screen Mode ... all gleened from the initialization state
;	AH = Burst
;	CL = Screen Width
;	CH = Screen Height
;	DL = Active Page
;	DH = Visual Page
;
;EXIT:
;	Most internal screen variables set.
;
;USES:
;****

cProc	B$SETSCNDATA,<NEAR,PUBLIC>
cBegin
	call	B$ScreenN	;set mode-dependent data
	jnc	ScrOk		;go if mode supported
	cmp	al,1		;trying for screen 1?
	jne	BurstOk 	;go if not
	xor	ah,al		;invert burst if going from screen 1 to 0
BurstOk:
	xor	al,al		;default to screen 0
	call	B$ScreenN	;set up screen 0
ScrOk:
	xchg	bx,cx		;width/height to BX
	push	dx		;save pages
	call	[b$AlphaDim]	;set text dimensions
	call	B$FixTextPage	
	mov	al,b$ScrWidth	
	mov	cl,b$ScrHeight 
	cCALL	B$SCNSWI	;Inform Hi-Level about dimensions change
	pop	ax		;AX=act/vis pages for b$SetPages
	cmp	al,b$MaxPage	;Active Page OK?
	ja	ResetPage	;go if not
	cmp	ah,b$MaxPage	;Visual Page OK?
	jbe	PageOk		;go if not
ResetPage:
	xor	ax,ax		;clear both pages if either invalid
PageOk:
	call	[b$SetPages]	;set page values
cEnd



;***
;B$RESETSCN -- Reset the screen startup mode for termination.
;OEM-interface routine
;
;Purpose:
;	This routine sets up the mode dependent data and the screen
;	to the startup mode for termination.  The startup mode was
;	saved by B$GWINI in a form that our standard routines would
;	be able to use it for termination.  If delayed screen initialization
;	is being implemented and the physical screen has not yet
;	been changed, then this routine should just change internal
;	variables.  However if the screen has been changed, this
;	routine must change the physical screen parameters to their
;	entry value.
;
;Entry:
;	None.
;
;Exit:
;	Most internal screen variables set.
;	b$ScreenRestored set properly
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;****
;
;NOTE:
;	The startup mode and characteristics were filtered by B$SETSCNDATA
;	to values that we can support here.
;
;Our Entry Conditions:
;
;	b$OrgScrBst = original startup screen mode and burst
;	b$OrgScrDim = original startup screen dimensions
;	b$OrgEquip  = original startup equipment flags
;	b$OrgCsrTyp = original startup cursor type
;	b$OrgPages  = original startup active/visual pages
;

cProc	B$RESETSCN,<NEAR,PUBLIC>
cBegin
	mov	b$ScreenRestored,0  ;first assume no screen change
; With a VGA, the BIOS actually changes the BiosEquip flags to reflect
; the current mode.  If we change it back (or the user POKEs it)
; the BIOS can get awfully confused!  So DON'T mess with it!
	test	b$Adapter,VGA	;using VGA?
	jnz	ChkMode 	;avoid Equipment flag check if so
				;carry clear means must check mode
	xor	ax,ax		;use seg = 0
	mov	es,ax		
	mov	al,b$OrgEquip	;check the original equipment flags
	cmp	al,es:[BiosEquip] ;were they changed?
	mov	es:[BiosEquip],al ;restore original equipment flags
	push	ds		
	pop	es		
	je	ChkMode 	;no change, go check for mode change
				;carry clear means must check mode
	call	B$SetAdapter	;make sure the adapter info is current
	stc			;carry set means force SetMode
ChkMode:			
	mov	ax,b$OrgScrBst	;original screen mode and burst
	mov	cx,b$OrgScrDim	;	       width and height
	jc	SetMode 	;go if decision already made to SetMode
	cmp	al,b$ScreenMode;mode changed?
	jne	ChgMode 	;go if so
	cmp	ah,b$Burst	;burst changed?
	jne	ChgMode 	;go if so
	cmp	ch,b$ScrHeight ;height changed?
	jne	ChgMode 	;go if so
	cmp	cl,b$ScrWidth	;width changed?
	je	SetMode 	;go if not
ChgMode:			
	stc			;carry set means force SetMode
SetMode:			
	pushf			;save SetMode decision
; Always set screen mode data with B$ScreenN and b$AlphaDim which do
; not change the physical screen.
; B$SCNSWI must be called to reset the text (VIEW PRINT) window.
	call	B$ScreenN	;set mode-dependent data
	xchg	bx,cx		;width/height to BX
	call	[b$AlphaDim]	;set text dimensions
	mov	al,b$ScrWidth	
	mov	cl,b$ScrHeight 
	cCALL	B$SCNSWI	;Inform Hi-Level about dimensions change
	popf			;should we SetMode
	jnc	NoChgMode	;go if not
	mov	b$ScreenRestored,1  ;Set flag for QB because screen changed
	call	B$ChkMonitor	; Set VGA monitor type before setting mode
	call	[b$SetMode]	;actually set the mode
	call	B$SCNCLR 	
	jmp	short Cleanup	
NoChgMode:			
	call	B$VIEWINIT	;reset viewports
Cleanup:			
	call	B$FixTextPage	
	call	[b$PalReset]	; reset the palette to default
	mov	dx,b$CURSOR	; force positioning of cursor
	call	B$OFFCSR	
	mov	ax,b$OrgPages	;restore startup page selections
	call	[b$SetPages]	
	call	B$GETCSRDATA	; DX = cursor positon for this page
	mov	b$CURSOR,dx	; update cursor position in case pages
				; changed (QB can re-start after this
	xor	cx,cx		; reset b$KEY_SW to 0
	xchg	[b$KEY_SW],cl	; KEY OFF (equivalent to call B$TKEYOF)
	jcxz	no_keys		; brif already off
	call	[b$vKEYDSP]	; turn function key display off if on
no_keys:			; (clears status line).  This should be
				; done in the final ACTIVE=VISUAL page
	cCall	B$GRMODE	
	jz	TextMode	; brif so
	call	B$OFFCSR	; turn off cursor for graphics modes
	jmp	short ResetExit	
TextMode:
	mov	ax,b$OrgCsrTyp	; get startup cursor type
	call	B$CSRDSP	; display startup cursor type
ResetExit:			
cEnd


;*** 
;B$SetDOS5Mode - Set screen mode for OS/2
;OEM-interface routine
;
;Purpose:
;	Set the screen mode for OS/2.
;
;	In order to perform a CHAIN command under OS/2 and pass COMMON
;	variables, the normal initialization has to be avoided.  What
;	happens is that the parent process does not terminate immediately,
;	but waits for a signal from the child process.	When this occurs,
;	the parent process will set some shared variables and signal the
;	child, which will start a signal handler.  This handler copies all
;	the data from the parents data space into its own data space, using
;	the shared variables set up by the parent to delimit how much
;	data has to be moved.  At this point, all the data is set up for
;	executing the program, but the machine state has to be updated.
;	Everything but the screen is set by the standard calls.   This
;	routine will update the screen state so that it matches the state
;	of the parent, who has now terminated.	To do this, it must have
;	kept a record of the old state.
;
;	In our implementation, we save all the information that needs to
;	be passed to GIOSETMODE which is called in this routine and we
;	also calculate the Segment descriptor for the video memory.
;
;	This is a OS/2 only routine.
;
;Entry:
;	b$Dos5Packet set as OS/2 VIO/GIO set mode data packet
;
;Exit:
;	None.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;******************************************************************************



sEnd	CN_TEXT 		

	END
