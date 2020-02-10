	TITLE	LLSCREEN - Screen Statement Interface
;***
;LLSCREEN - Screen Statement Interface
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
; This module contains B$SCRSTT, the screen statement support routine.
;
;*****************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	USESEG	_DATA
	USESEG	_BSS
	USESEG	GR_TEXT
	USESEG	RT_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc

sBegin	_BSS
; Warning!!!  These two bytes must be contiguous!!!
staticB ModeSpec,,1		;Set = 1 iff Mode/burst specified in SCREEN
staticB PageSpec,,1		; Set iff Act/Vis page specified in SCREEN
; Warning!!!  These two bytes must be contiguous!!!

staticB OldBiosMode,,1		;old BIOS mode

externW b$AlphaDim
externB b$BiosMode
externW b$CurPages
externB b$MaxPage
externW b$ModeBurst
externW b$OldScrnBurst		;old Screen Mode and burst
externB b$ScreenMode
externB b$ScrHeight
externB b$ScrWidth
externW b$SetMode
externW b$SetPages

externW b$PalReset
externB b$CurEquip		
sEnd

sBegin	_DATA
externW b$CURSOR		; current cursor location
sEnd

sBegin	RT_TEXT
externNP B$FixTextPage		
externNP B$ChkMonitor		; set VGA monitor type
sEnd

sBegin	GR_TEXT
assumes CS,GR_TEXT

externNP B$GETCSRDATA
externNP B$GetParm
externNP B$SCINIT
externNP B$SCNCLR
externNP B$SCNLOC
externNP B$SCNSWI
externNP B$ScreenN
externNP B$OFFCSR		

externNP B$SetAdapter

;***
;B$SCRSTT - SCREEN Statement parsing and execution support
;OEM-interface routine
;
;Purpose:
;	This routine provides mode-independent support for the SCREEN
;	statement.
;
;Syntax:
;	The syntax of the IBM screen statement is:
;
;		SCREEN [mode] [,[burst] [,[apage] [,vpage]]]
;
;	The semantics of the statement are as follows:
;
;	mode  = requested BASIC screen mode
;
;	burst = 0 - request black and white screen (configuration dependent)
;		x - request color screen
;
;	apage = desired active display page
;
;	vpage = requests visual page to display
;
;	These values are interpreted only in this routine, so
;	an alternate set of semantics can easily be assigned.
;
;Algorithm:
;	Each mode is supported in a mode-dependent module whose only
;	PUBLIC symbols are of the form "B$ScreenX", where X is the
;	BASIC screen mode.  B$SCRSTT calls these routines to set up
;	mode-dependent data and function vectors to be used later by
;	various higher-level functions in as mode-independent a manner
;	as possible.
;
;	This routine must also do the following:
;	1.) If delayed screen initialization is being supported,
;	    B$SCNINIT must be called before any changes.
;	2.) lets BASIC know new screen dimensions via B$SCNSWI
;	3.) Home cursors and clear screen via B$SCNCLR
;	    (must be done after #2 and after all screen state variables
;	     have been set)
;	4.) Set the default foreground and background colors.
;	5.) (OS/2 only) Clear the screen by calling B$CLRSCN with
;	    a parameter of 0.
;
;Entry:
;	[SI] = address of the high end of the parameter list
;	       (parameters run from high to low memory)
;	[CX] = count of words in parameter list
;	(uses B$GetParm to get parameters)
;
;Exit:
;	PSW.C = set indicates that an error was encountered
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;****

cProc	B$SCRSTT,<PUBLIC,NEAR>	;NOTE: B$SCRSTT and B$WIDTH share a common
				;      exit at ErrorRestore
cBegin

	CALL	B$SCINIT	;init screen if not already done
	XOR	AX,AX
	MOV	WORD PTR ModeSpec,AX ; assume mode for screen not specified

;	Check to see if the user has executed a POKE statement to change
;	the equipment list.

;	NOTE: This could be better done through the POKE filter.

	PUSH	DS		;look at equipment flag in low memory
	MOV	DS,AX		;(located at 0:410) to see whether user
	MOV	AL,DS:[BiosEquip] ;has POKEd a new value to toggle monitors
	POP	DS
	CMP	AL,b$CurEquip	;this compares with previous setting
	JZ	SCREN0		;BRIF no change; continue
	MOV	b$CurEquip,AL	;otherwise update BASIC's current equip
	CALL	B$SetAdapter	;check out new hardware
SCREN0:
;
; Start of new parameter parsing
; For BASICA compatibility, if the mode is specified, the default burst is not
; the present one, but 1 for screen mode 0, 0 for screen modes 1 and above.
; This always sets the color burst on for modes 0 & 1 and ignores the burst of
; rmodes 2 and above. The following table gives the logic coded below:
;
; --------------  --------------- -----------
; Mode specified  Burst specified Value in BH
; --------------  --------------- -----------
; Yes		  Yes		  Specified Burst
; Yes		  No		  BH = 1/0 for text/mode 1
; No		  Yes		  Specified Burst
; No		  No		  Old burst value
;
	JCXZ	ScrError	;Error if no args passed
	cCALL	B$GetParm	;Get first parameter - mode value
	mov	bx,b$ModeBurst	;get old BIOS mode and burst
	mov	OldBiosMode,bl	;save old BIOS mode for later
	mov	bl,b$ScreenMode ;Get old screen mode
	mov	b$OldScrnBurst,bx ;save old values for later
	MOV	DX,b$CurPages
	JZ	GET_BURST	;Brif no mode specified
	MOV	ModeSpec,1	;Mode was specified
	OR	AL,AL		; graphics mode?  (AH = 0 after B$GetParm)
	JNZ	NO_BURST	; brif so -- default to zero burst
	INC	AH		; burst defaults to 1 if text
NO_BURST:			
	XCHG	BX,AX		;BX = BURST:MODE
GET_BURST:
	cCALL	B$GetParm	;See if burst is also specified
	JZ	GET_APAGE	;Brif not
	MOV	ModeSpec,1	;Mode was specified
	NEG	AL		;PSW.C = AL==0?0:1
	rcl	bh,1		;carry to LSB of BH
GET_APAGE:
	AND	BH,01		;Retain only LSB of burst
	cCALL	B$GetParm	;Check if active-page given
	JZ	GET_VPAGE	;Brif not
	INC	PageSpec	; pages were specified
	XCHG	AX,DX		;DL = Active-page
	MOV	DH,DL		;DH = default visual-page
GET_VPAGE:
	cCALL	B$GetParm	;Check if visual-page specified
	JZ	CHECK_REST	;Brif not
	INC	PageSpec	; pages were specified
	MOV	DH,AL		;DH = visual-page
CHECK_REST:			;At this point CX should be zero
	JCXZ	PROCESS_SCRSTT	;No more arguments - all is well
ScrError:
	jmp	SCREEN_ERROR	;exit w/error if not

PROCESS_SCRSTT:
	xchg	ax,bx		;ax = new BURST:MODE
;
;at this point:
;	AL = new Mode
;	AH = new Burst
;	DL = new Active-page
;	DH = new Video-page
;
; To be compatible with QB, let us first check if any of the input parameters
; changed. This needs to be done only if the caller is not WIDTH statement
; (where at least the screen height would have changed).
;
	cmp	b$CurPages,dx	; Apage/Vpage changed?
	jne	SCRSTT_CONT	;go if so
	cmp	b$OldScrnBurst,ax ; Mode/Burst changed?
	je	SCRSTT_RETURN	;go if not - simply exit
	cmp	PageSpec,0	; were pages specified?
	jnz	SCRSTT_CONT	; brif so - use specified pages
	xor	dx,dx		; else use page zero in new mode
SCRSTT_CONT:
;
; Now for establishing the new mode. But one problem: We want to error check
; the new active and visual pages before we set the new mode. But we can't
; determine valid ranges in a mode-independent way until the mode has been
; established. To do that, b$ScreenX and b$AlphaDim function vectors only set
; up the mode-dependent data and make no actual screen changes. Then we can
; verify the page ranges and, if valid, set the mode via the b$SetMode function
; vector. If invalid, we must restore the old mode data again and exit with an
; error.
;
	push	ax		
	push	dx		
	mov	dx,b$CURSOR	; turn cursor off at present location, in
	call	B$OFFCSR	; order to physically move the cursor prior
				; to possibly changing pages
	pop	dx		
	pop	ax		
	mov	cl,b$ScrWidth	;Get current width of the screen
	mov	ch,b$ScrHeight	;  and height
	call	B$ScreenN	;invoke screen AL to set mode-dependent data
	jc	SCREEN_ERROR	;go if error, (nothing was done)
	xchg	bx,cx		;width/height to BX
	push	bx		;save:	old width/height
	push	dx		;	pages
	mov	bl,b$ScrWidth	;new width is fine
	call	[b$AlphaDim]	;set text dimensions
	pop	dx
	pop	cx
	cmp	dl,b$MaxPage	;check active page range
	ja	ErrorRestore	;go if bad to restore old mode data
	cmp	dh,b$MaxPage	;check visual page range
	ja	ErrorRestore	;go if bad to restore old mode data
	push	dx		;save pages
	mov	al,b$ScrWidth
	mov	cl,b$ScrHeight
	cCALL	B$SCNSWI	;Inform Hi-Level about dimensions change
	cmp	ModeSpec,0	;SetMode only if
	je	NoSetMode	;  mode was specified
	mov	al,b$BiosMode	;and
	cmp	al,OldBiosMode	;  BIOS mode actually changed
	je	NoSetMode
	call	B$ChkMonitor	; Set VGA monitor type before setting mode
	call	[b$SetMode]	;set the actual mode
	CALL	B$SCNCLR	;setup the screen (homes user cursor)
	call	[b$PalReset]	;initialize the palette
NoSetMode:			; jump past the call to [b$PalReset]
	call	B$FixTextPage	
	pop	ax
	call	[b$SetPages]	;set current page data
	CALL	B$GETCSRDATA	;get cursor position for this page, since
				;the active page might have changed.
	CALL	B$SCNLOC	;update high-level cursor postion and turn
				;on the user cursor

SCRSTT_RETURN:
	CLC			;Indicate no error and exit
	jmp	short SCRSTT_EXIT

ErrorRestore:			;restore old screen mode data and return w/error
	mov	ax,b$OldScrnBurst
	call	B$ScreenN	;reset mode-dependent data
	xchg	bx,cx		;width/height to BX
	call	[b$AlphaDim]	;reset text dimensions
	call	B$FixTextPage	

SCREEN_ERROR:			;indicate error and exit
	STC
SCRSTT_EXIT:
cEnd

sEnd

	END
