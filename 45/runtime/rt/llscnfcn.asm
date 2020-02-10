	TITLE	LLSCNFCN - Low Level Screen Function Support
;***
; LLSCNFCN - Low Level Screen Function Support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module has the following low level routines:
;	1. B$SWIDTH .... screen width support
;	2. B$SCREEN .... screen FUNCTION support
;	3. B$SETCLR .... color statement support
;	4. B$CSTATR .... Cursor support for LOCATE statement
;
;*****************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	USESEG	_DATA
	USESEG	_BSS
	USESEG	CN_TEXT
	USESEG	RT_TEXT

	INCLUDE seg.inc
	INCLUDE oscalls.inc
	INCLUDE idmac.inc
	INCLUDE ibmunv.inc
	INCLUDE intmac.inc
	INCLUDE llgrp.inc

sBegin	_BSS

	globalW b$OldScrnBurst,,1	;old Screen Mode and burst
	staticW OldDim,,1		;old screen dimensions

	externB b$ScreenMode
	externB b$Burst
	externB b$ActPage
	externB b$ScrWidth
	externB b$ScrHeight
	externW	b$FBColors		; defined in llcgrp.asm
	externB	b$ForeColor		; defined in llcgrp.asm
	externB	b$NullColor		; defined in llcgrp.asm
	externB	b$CharColor		; defined in llcscn.asm
	externB	b$BorderColor		; border color (overscan)
	externW b$SetMode
	externW b$AlphaDim
	externW b$SetPages
	externW b$SetColor
	externW	b$SetAttr		

sEnd	_BSS


sBegin	_DATA

	externW b$CURSOR
	externW b$CSRTYP
	externB b$UsrCsrOn	; Is the cursor being displayed?
	externB b$UsrCsrStart	; user-defined cursor start line
	externB b$UsrCsrStop	; user-defined cursor stop line
	externB b$InsCsrStop	; insert cursor stop line

sEnd	_DATA



sBegin	RT_TEXT
	externNP B$FixTextPage	
	externNP B$ChkMonitor	; set VGA mono/color monitor

sEnd	RT_TEXT


sBegin	CN_TEXT
	assumes CS,CN_TEXT

	externNP B$SCNCLR
	externNP B$CSRDSP	;Display cursor
	externNP B$OFFCSR	;Turn off cursor
	externNP B$SCINIT
	externNP B$SCNSWI
	externNP B$ScreenN	;Initialize screen mode AL

;***
; B$SWIDTH - support for WIDTH statement
;OEM-interface routine
;
;Purpose:
;	This routine supports the width statement by attempting to set
;	the screen dimensions to those specified.  This routine is called
;	only if a change in the number of lines or columns is needed.
;
;	If the parameters are legal, the following must occur:
;	1.) Set hardware to reflect new conditions.
;	2.) Update OEM state variables.
;	3.) Call B$SCNSWI to inform runtime of dimension changes.
;	4.) Call B$SCNCLR to initialize screen, viewport, and cursor.
;	5.) Clear the screen (OS/2 only) (Call B$CLRSCN with a parameter of 0)
;
;	NOTE: If delayed screen initialization is being supported,
;	      B$SCNINIT must be called before any changes are made.
;
;Entry:
;	AL = prospective number of columns
;	CL = prospective number of lines
;
;Exit:
;	PSW.C = set to indicate error
;	PSW.C = reset to indicate update completed
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;****

cProc	B$SWIDTH,<PUBLIC,NEAR>
cBegin
	CALL	B$SCINIT	;init screen if not already done
	mov	dl,b$ScrWidth	;get old width and height
	mov	dh,b$ScrHeight
	mov	OldDim,dx	;save for later restorations
	mov	bh,b$Burst	;get burst
	mov	bl,b$ScreenMode    ;get screen mode
	mov	b$OldScrnBurst,bx ;save old values for later
	xchg	bl,al		;bl=columns
	xchg	bh,cl		;bh=lines
	cmp	bl,dl		;width change?
	jne	DimChg		;go if so
	cmp	bh,dh		;height change?
	je	WidthExit	;go if not
DimChg:
	push	bx		;save dimensions
	call	[b$AlphaDim]	;change to new dimensions
	pop	cx
	jc	WidthError	;go if errors
	cmp	al,-1		;width satisfied
	je	WidthMode	;go setup mode if so
	;this mode suggested trying another for these dimensions
	mov	ah,b$Burst	;setup burst for B$ScreenN
	cmp	al,b$ScreenMode ;going to a different mode?
	je	NoBstChg	;leave burst alone if not
	xor	ah,ah		;clear burst
	cmp	al,ah		;going to mode 0?
	jne	NoBstChg	;go if not, with burst = 0 for modes > 0
	inc	ah		;burst = 1 for mode 0
NoBstChg:			
	call	C_ScreenN	; try that one (save colors)
	jc	WidthError	;go if error

	xchg	bx,cx		;width/height to BX
	cmp	bh,b$ScrHeight ;different height?
	jne	DimChg		;go if so
	cmp	bl,b$ScrWidth	;diferent width?
	jne	DimChg		;go if so

WidthMode:
	call	B$ChkMonitor	; Set VGA monitor type before setting mode
	call	[b$SetMode]	;set the actual mode
	mov	al,b$ScrWidth
	mov	cl,b$ScrHeight
	cCALL	B$SCNSWI	;Inform Hi-Level about dimensions change
	CALL	B$SCNCLR	; initialize the screen (homes cursor, etc.)
	call	B$FixTextPage	
	xor	ax,ax		;clear both pages
	call	[b$SetPages]	;set current page data
	clc			;no error
	JMP	SHORT WidthExit

WidthError:
	mov	cx,OldDim	; restore old screen mode data
	mov	ax,b$OldScrnBurst
	call	C_ScreenN	; reset mode-dependent data (save colors)
	xchg	bx,cx		; width/height to BX
	call	[b$AlphaDim]	; reset text dimensions
	call	B$FixTextPage	
	STC			; return with error
WidthExit:
cEnd

;*** 
;C_ScreenN - Save colors and call B$ScreenN
;
;Purpose:
;	Preserve Colors over call to B$Screen.  Can only be used if
;	the actual screen mode is not changing.  (just dimensions)
;
;Entry:
;	AX = Screen mode and burst
;	CX = Screen Dimensions
;
;Exit:
;	PSW.C set indicates error in B$ScreenN.
;
;Uses:
;	Per convention.
;
;Preserves:
;	CX.
;
;******************************************************************************
cProc	C_ScreenN,<NEAR>,<CX> ; entire routine
cBegin
	MOV	BH,[b$BorderColor] ; save colors
	MOV	BL,[b$NullColor]
	MOV	DL,[b$CharColor]
	PUSH	[b$FBColors]
	CALL	B$ScreenN	; AX = mode & burst (preserves AX,BX,CX,DX)
	POP	[b$FBColors]	; restore colors
	MOV	[b$CharColor],DL
	MOV	[b$NullColor],BL
	MOV	[b$BorderColor],BH
	MOV	AL,[b$ForeColor]
	PUSHF			; save PSW.C
	CALL	[b$SetAttr]	; ignore error return
	POPF			; restore error status from B$ScreenN
cEnd

;***
;B$SCREEN - SCREEN function support
;OEM-interface routine
;
;Purpose:
;	This routine provides OEM dependent support for the SCREEN function
;	by returning the character and its encoded attributes at a specified
;	position on the screen.	For the IBM-PC the attributes are encoded
;	as follows:
;
;		1. bits 0-3 contain the foreground color of the character
;		2. bits 4-6 contain the background color of the character
;		3. bit 7 contains the blinking attribute
;
;	The value returned for the encoded attributes should conform
;	to your machine's standard, as this is the value that is passed
;	back to the user.  The coordinates are guaranteed to be legal
;	for the current screen mode.
;
;	For systems with a hardware character generator, this routine
;	can act directly upon the CRT RAM.  Systems that have software
;	character generation and that do not have two byte characters
;	may have a separate buffer or may reverse translate the
;	characters from the video RAM.	For systems that have software
;	character generation and support two byte character codes, a
;	separate buffer or some other data structure must be maintained
;	so that two byte characters can be read from the "screen".
;
;	If the current screen mode support double byte characters,
;	it is possible that this routine will ask for the value
;	of either half of the character.
;
;	For FL_JAPAN, B$SCREEN should return the value of the entire
;	character (as defined for B$SCROUT). The OEM Independent code
;	will negate the MS-Continuous code for the character if it is
;	the second half of a double byte character.  All other
;	OEMs should return for a character code the value that
;	they want the SCREEN() function to return to the user.	No
;	further mapping or manipulation will be done for it.
;
;	NOTE: If you are supporting delayed screen initialization,
;	      B$SCNINIT has to be called before any attempt to read
;	      from the screen.
;
;Entry:
;	DH = requested column position (1-relative)
;	DL = requested row position (1-relative)
;
;Exit:
;	AX = character			(as defined for B$SCROUT)
;	BX = encoded attributes		(Hi-byte = 0)
;	PSW.C = set if we tried to read the middle of a double byte
;		character
;	PSW.C = reset in all other cases
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;****
;ALGORITHM:
;
;	save current cursor type
;	move cursor to requested position
;	read character &attributes at that position
;	replace cursor to original position and type
;

cProc	B$SCREEN,<PUBLIC,NEAR>,<SI>
cBegin

	CALL	B$SCINIT	;init screen if not already done

	MOV	CX,b$CSRTYP	; save current cursor type in CX
	CALL	B$OFFCSR	; position invisible cursor at (DH,DL)
	MOV	BH,b$ActPage	; get active display page

	SCNIO	vReadChar	;read encoded attributes at current csr position

	OR	AL,AL		; vReadChar returns NULL in graphics modes
	JNZ	NOT_NULL	;  if no char is present in char cell
	MOV	AL,' '		; so we must compensate here
NOT_NULL:			
	XOR	BX,BX		; zero BX
	XCHG	AH,BL		; BX = Attribute
	XCHG	AX,CX		; AX = b$CSRTYP (Old)
				; CX = Character
	MOV	DX,b$CURSOR	; get original cursor position
	CALL	B$CSRDSP	; restore original cursor position and type

	XOR	CH,CH		; Set Hi Byte zero
	XCHG	AX,CX		; AX = Character


cEnd


;***
;B$SETCLR - COLOR statement support
;OEM-interface routine
;
;Purpose:
;	This routine provides support for the COLOR statement.
;	The parameters to this function are implementation and
;	screen mode dependent.	For all machines, the first
;	parameter is the ForeGround Color and the second parameter
;	is the Border Color.  If it is desired to indicate an
;	error from this routine (such as wrong number of parameters,
;	parameter out of range, etc), return with PSW.C set.
;
;	If delayed screen initialization is being supported,
;	the routine B$SCNINT must be called before any
;	changes are made.
;
;Entry:
;	[SI] = address of the high end of the parameter list
;	       (parameters run from high to low memory).
;	[CX] = count of words in parameter list
;
;	This list is in the format for B$GetParm to parse.
;
;Exit:
;	PSW.C = set indicates that an error was encountered
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;****
;	Since the meaning and number of parameters for the COLOR
;	statement is dependent on the screen mode, this routine
;	just calls the mode-dependent routine and hands off the
;	parameters entirely.
;

cProc	B$SETCLR,<PUBLIC,NEAR>
cBegin
	CALL	B$SCINIT	; init screen if not already done
	call	[b$SetColor]	; call mode-dependent module
cEnd

;***
;B$CSRATR - Set cursor attribute
;OEM-interface routine
;
;Purpose:
;	This routine handles a request to set the cursor attribute of
;	the user cursor.  There are three cursor attribute bytes.  They
;	correspond to parameters three, four, and five of the LOCATE
;	statement.
;
;	These parameters are:
;
;	  3.  cursor - is a value indicating whether the cursor is
;		       visible or not. 0 (zero) for off, 1 (one) for
;		       on.
;	  4.  start  - is the cursor starting scan line. Must be in
;		       the range [0,31].
;	  5.  stop   - is the cursor stop scan line.  Must be in the
;		       range [0,31].
;
;	B$SCRATR must check the start and stop values to make sure that
;	they are in the range [0,31].  If they are not in that range,
;	return with PSW.C set to indicate an error.  If these values are
;	legal, but will produce unusual results when the cursor is set
;	to them (i.e. the cursor is invisible or split), the cursor
;	should be updated anyway.  After all, the user asked for it.
;	Note that changing the stop line of the user cursor will also
;	change the stop line of the insert cursor to the same value.
;
;Entry:
;	[AH] - LOCATE parameter 3 existence flag
;		(0 iff the user specified no value)
;	[AL] - LOCATE parameter 3 value
;	[BH] - LOCATE parameter 4 existence flag
;	       (0 iff the user specified no value)
;	[BL] - LOCATE parameter 4 value
;	[CH] - LOCATE parameter 5 existence flag
;	       (0 iff the user specified no value)
;	[CL] - LOCATE parameter 5 value
;
;Exit:
;	PSW.C - set indicates a cursor attribute parameter error
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;****
;Algorithm:
;	if param 3 exists then
;	  if in valid range then store cursor toggle
;	    else error
;	if param 4 exists then
;	    if in valid range then
;		update user cursor start line
;		if param 5 exists then
;		    if in valid range then
;			update user cursor stop line
;			update insert cursor stop line
;		      else error
;		  else
;		    update user cursor stop line := user cursor start line
;		    update insert cursor stop line := user cursor start line
;	   else
;	     if param 5 exists then error
;Modified:
;	b$InsCsrTyp - insert mode cursor attributes possibly changed
;	b$UsrCsrTyp - user mode cursor attributes possibly changed
;
;#**

cProc	B$CSRATR,<PUBLIC,NEAR>
cBegin

	OR	AH,AH		;see if value is given for cursor toggle
	JZ	CSR4TH		;br. if no value is given
	CMP	AL,1		;check valid range [0,1]
	JA	CATRER		;br. if out of range error
	MOV	B$UsrCsrOn,AL	;store cursor toggle
CSR4TH:
	OR	BH,BH		;see if value is given for cursor start line
	JZ	CSR5TH		;br. if no value is given
	CMP	BL,31D		;check valid range [0,31]
	JA	CATRER		;br. if out of range error
	MOV	b$UsrCsrStart,BL ;store startline for user cursor

	OR	CH,CH		;see if value is given for cursor stop line
	JZ	NO5VAL		;br. if no value is given
	CMP	CL,31D		;check valid range [0,31]
	JA	CATRER		;br. if out of range error
	MOV	b$InsCsrStop,CL ; store insert cursor stop line
	MOV	b$UsrCsrStop,CL ; store user cursor stop line
	JMP	SHORT CSRDON	; valid return
NO5VAL:
	MOV	b$InsCsrStop,BL ;default insert csr stop line = stop line
	MOV	b$UsrCsrStop,BL ;default user csr stop line = stop line
CSR5TH: 			;no value was given for cursor start line
	OR	CH,CH		;see if value was given for cursor stop line
	JNZ	CATRER		;error if stop value given without start value
CSRDON:
	CLC			;clear flag for valid return
	RET
CATRER:
	STC			;set flag for parameter error
cEnd

sEnd	CN_TEXT

	END
