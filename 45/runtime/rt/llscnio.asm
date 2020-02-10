	TITLE	LLSCNIO - screen I/O initialization
;***
; LLSCNIO.ASM - screen I/O initialization
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

	INCLUDE switch.inc	;switch file
	INCLUDE rmacros.inc	;useseg macro

	USESEG	RT_TEXT 	;core runtime segment
	USESEG	CN_TEXT 	;Console IO
	USESEG	_DATA		
	USESEG	_BSS		

	INCLUDE seg.inc 	;segment definitions
	INCLUDE idmac.inc	;internal debug macros
	INCLUDE oscalls.inc	;Dos 5 structures
	INCLUDE ibmunv.inc
	INCLUDE intmac.inc
	INCLUDE baslibma.inc	; SKIP macro
	INCLUDE llgrp.inc	; constant definitions
	INCLUDE const.inc	; bit flag definitions
	INCLUDE files.inc	; CALLOS macro



sBegin	_DATA			

	externB b$ForeColor	
	externB b$NullColor	;encoded color attributes for blank/_BSCROLL
	externB b$ForeMapped	;graphics foreground
	externW b$CURSOR 	; high-level cursor position (where we
	externB b$WDOTOP	; top line of text window (1-relative)
	externB b$WDOBOT	; bottom line of text window (1-relative)
	externB b$CRTWIDTH	; rightmost column (1-relative)
	externW b$CharPage	; b$CharColor and b$ActPage in one word
	externB b$ActPage	; active display page.
	externW b$CSRTYP	; Current Cursor type


	externW b$VideoBase	

sEnd	_DATA			


sBegin	_BSS			

	globalB b$MUSIC,,1	; music currently active or not byte

	externB b$ScrWidth	
	externB b$ScrHeight	
	externB b$ScreenMode	
	externB b$Adapter	

	externW b$PageTable	

sEnd	_BSS			

sBegin	RT_TEXT 		
	assumes CS,RT_TEXT	

	externNP B$SCNIO	


	externNP B$SCINIT	; Performs screen initialization
	externNP B$SELAPG	; Select active screen page
	externNP B$SELVPG	; Select visual screen page
	externNP B$OFFCSR	
	externNP B$SlowUsrCsr	

;***
;B$CLRSCN - Clear screen and home cursor
;OEM-interface routine
;
;Purpose:
;	Low level support for the CLS statement and ^L function.
;	This routine supports the clearing of the text window.
;
;	Note that since the graphics cursor in DOS is generated
;	by the OEM code, it may have to be redrawn on the new screen.
;
;	The parameters that this routine is passed are only indirectly
;	related to the ones to the CLS statement.  The CLS statement
;	parameter may be modified to reflect the current screen/graphics/
;	viewport/window that is in effect.
;
;	NOTE: If you are supporting delayed screen initialization,
;	      B$SCNINIT has to be called before any changes are
;	      made.
;
;Entry:
;	[AL] = 0 indicates clear all text and graphics	(entire screen)
;	       1 indicates clear graphics viewport only (do nothing)
;	       2 indicates clear text window only
;
;Exit:
;	[AL] = 0, 1 or 2 as described above
;	PSW.C - set indicates a function call error
;
;Uses:
;	Per convention
;
;Preserves:
;	AX,BX,CX,DX
;
;Exceptions:
;	None.
;****
;Algorithm:
;	if parameter passed is 0 then
;	    get current screen mode
;	    get 0-relative width
;	    get background color
;	    blank screen from bottom up
;	else
;	    if parameter passed is 1 then
;		return with parameter unchanged
;	    else
;		if parameter passed is 2 then
;		    get text window limits
;		    fill text window with ASCII spaces
;		else
;		    set [AL] to 0
;		    set PSW.C to indicate function call error
;#**

cProc	B$CLRSCN,<PUBLIC,NEAR> 
cBegin
	PUSH	DX
	PUSH	CX
	PUSH	BX
	PUSH	AX
	CALL	B$SCINIT	; make sure screen is initialized.
	PUSH	AX		
	cCALL	B$SELAPG 	; select active page
	POP	AX		
	CMP	AL,1
	JE	CLRET_1 	;return with parameter unchanged
	JA	TXONLY		;clear text only
TXNGRA: 			;clear the whole screen
	MOV	DX,101H
	PUSH	DX

	MOV	DL,b$ScrWidth	;screen width
	dec	dl		;make 0-relative
	MOV	DH,b$ScrHeight ;Get screen height
	DEC	DH		; Ignore function key display line
	DEC	DH		; Make it 0-relative
	XOR	CX,CX		;upper left hand corner (0,0)
	JMP	SHORT CLR_IT

CLRET_1:			
	JMP	SHORT CLRET	

TXONLY:
	XOR	CX,CX		; CH = left column (0-relative)
	MOV	CL,b$WDOTOP	; get window top (1-relative)
	PUSH	CX		; save for B$CSRDSP
	DEC	CX		; CL = top row (0-relative)
	XCHG	CH,CL		; (CH,CL) = (top row, left column)
	MOV	DL,b$CRTWIDTH	; get right column (1-relative)
	MOV	DH,b$WDOBOT	; get window bottom (1-relative)
	DEC	DX		; DL = right column (0-relative)
	DEC	DH		; DH = bottom row (0-relative)
CLR_IT:

	MOV	BH,b$NullColor	;get current background color

	XOR	AL,AL		;blank entire window
	SCNIOS	vScrollPageUp	;from bottom up

	POP	DX		;unsave window top
	MOV	DH,1		; column = 1
	MOV	BYTE PTR b$CSRTYP,-1 ; invalidate previous cursor type so
				; that it will get reset
	CALL	B$SlowUsrCsr	; display user cursor, forcing relocation
CLRET:
	cCALL	B$SELVPG	; select visual page
	POP	AX
	POP	BX
	POP	CX
	POP	DX
	CMP	AL,2		;was the parameter <= 2 ?
	JA	CLRERR		;Brif not
	CLC			;else carry clear to indicate no error
	RET
CLRERR:
	MOV	AL,0		;entry parameter altered, CLS 0 is
				;supported
	STC			;carry set to indicate error
cEnd				

;
;	Rewritten [35] to use B$STRSOUT.
;***
;B$SCROUT - Print a character on the screen
;OEM-interface routine
;
;Purpose:
;	This routine puts the specified character at the specified screen
;	position.  For systems with support for 16 bit printable characters,
;	the OEM-independent code ensures that double byte characters will
;	not be half-overwritten.
;
;	B$SCROUT must use a nonredirectable function call to write
;	characters.
;
;Entry:
;	[AX] = character (printable, not control)
;	[DH] = one relative column number
;	[DL] = one relative line number
;
;Exit:
;	Cursor turned off.
;
;Uses:
;	Per convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;****
cProc	B$SCROUT,<NEAR,PUBLIC>,<CX,DX,SI>
cBegin
	PUSH	AX		;place character in memory
	MOV	SI,SP		;[SI] = pointer to it
	MOV	CX,1		;[CX] = string length
	call	B$STRSOUT	;direct video access if text mode
	POP	AX		;Restore character (even stack)
cEnd


;***
;B$STRSOUT - output a 'normal' string to the screen
;OEM-interface routine
;
;Purpose:
;	Print a string to the screen. We know that there are no control
;	characters in the string, and that wrap-around will not occur.
;	This routine is provided to allow fast printing of strings
;	through a OS call or by direct access to the screen memory if
;	it is allowed.	If this speed improvement can not be implemented,
;	this routine can be implemented as multiple calls to B$SCROUT.
;
;	Note that for systems that have double byte character support
;	this routine takes the number of bytes in the string to
;	be output, not the number of characters.  It is guarenteed that
;	this string will not have any double byte characters in it.
;
;Entry:
;	[CX] == number of bytes to output
;	[DH] == one relative column number
;	[DL] == one relative line number
;	[SI] == pointer to first char in string
;
;Exit:
;	[DH] == (updated) one relative column number
;	[DL] == one relative line number
;	Cursor turned off if graphics mode
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX
;
;Exceptions:
;	None.
;****

cProc	B$STRSOUT,<NEAR,PUBLIC>,<BX,CX,SI>	
cBegin				

	CALL	B$SCINIT	; make sure screen is initialized.
	MOV	BX,b$CharPage	; [BL] = char attribute, [BH] = active page
	CMP	b$ScreenMode,0 ;Text mode ?
	jz	OUT_CHAR	;BRIF not
	CALL	B$OFFCSR 	; request cursor off and position
	MOV	BL,b$ForeMapped;char color is graphics foreground color
OUT_CHAR:


; Note that the string output function of the BIOS can't be used,
; since the original PC and XT didn't have it.

	DEC	DX		; make 0-based
	XCHG	DH,DL		; DH = 0-based row
	DEC	DX		; DL = 0-based column	
	PUSH	DI		
	call	B$FastOutStr	;direct video access if text mode
	POP	DI		; restore registers
	INC	DX		; make 1-based
	XCHG	DH,DL		; DH = 1-based column
	INC	DX		; DL = 1-based row	

cEnd				

; (start)
;***
;B$FastOutStr
;
;Purpose:
;	Write a block of characters to video RAM.  The characters must
;	fit on one line (no scrolling)
;Inputs:
;	bh = display page
;	bl = attribute/color
;	cx = count of characters
;	dl = 0-based column number
;	dh = 0-based row number
;	ds:si = pointer to characters
;	If graphics mode, cursor position must already be set with BIOS
;Outputs:
;	dl updated for next column (dl=dl+cl)
;	ds:si points past end of string
;	BIOS data area updated for next column
;Preserves:
;	dh,es
;Modifies:
;	si,di
;****
assumes	DS,NOTHING			; WARNING: DS not always DGROUP!
assumes	ES,NOTHING
cProc	B$FastOutStr,<PUBLIC,NEAR>	
cBegin
	cmp	[b$ScreenMode],0	;In text mode?
	jnz	GraphOutStr
TextOutStr:
	push	es			; must preserve ES if modified
	call	GetPos			;es:di points to video RAM
	mov	ah,bl			;Attribute to ah
	test	[b$Adapter],CGA		;CGA card needs video sync
	jz	NoSyncLoop
;Wait for start of horizontal sync, or anywhere in vertical sync
	push	dx
	mov	dx,CGA_STATUS
CheckStatus:
	sti
	nop				;One instruction delay on STI
	cli				;Can't interrupt between IN and STOSB
	in	al,dx			;Get status port
	test	al,VERT_SYNC		;In vertical sync?
	jnz	VsyncOut		;If so, do whole string right now
	test	al,HORZ_SYNC		;In horizontal sync?
	jnz	CheckStatus		;Wait till we're out of it
	lodsb				;Get character
	xchg	bx,ax			;Save char & attrib in bx
WaitHorz:
	in	al,dx
	test	al,HORZ_SYNC
	jz	WaitHorz		;Wait for start of horizontal sync
	xchg	ax,bx			;Get char & attrib to ax
	stosw				;Store in video RAM
	loop	CheckStatus
	sti
	pop	dx
	pop	es			
cEnd					

VsyncOut:
	sti
	pop	dx
NoSyncLoop:
	lodsb				;Get character to output
	stosw
	loop	NoSyncLoop
	pop	es			
	ret

GraphOutStr:
	mov	di,cx			;Keep char. count in di
	mov	cx,1			;1 character per BIOS call
GraphLoop:
	lodsb
	mov	ah,vWriteChar		;Write character and attribute
	int	10H
	inc	dx			; Increment column
	mov	ah,vSetCursorPos
	int	10H			;Bump cursor position
	dec	di			;Count characters
	jnz	GraphLoop
	ret

; (continued)

;***
;GetPos
;
;Inputs:
;	bh = display page
;	cx = count of characters
;	dl = 0-based column number
;	dh = 0-based row number
;Outputs:
;	es:di = pointer to video RAM location
;	dl updated for next column (dl=dl+cl)
;	BIOS data area updated for next column
;Preserves:
;	bx,cx,dh,si
;****
cProc	GetPos,<NEAR>			
cBegin
	xor	ax,ax
	mov	es,ax			;Access BIOS data area

;Compute ending cursor position for BIOS
	mov	al,bh			;Page no.
	xchg	di,ax			;   to di
	shl	di,1			;Use page no. as word index
	mov	ax,dx
	add	al,cl			;Compute ending column
	mov	es:[di].BIOS_CursorPos,ax ;Set final cursor position for BIOS

;Compute video RAM address
	mov	al,[b$ScrWidth]
	mul	ah			;Start of row
	add	al,dl			; Add column
	adc	ah,0
	shl	ax,1			;Two bytes/character
	add	ax,[di].b$PageTable	;Add start address for this page
	xchg	di,ax			;Video RAM offset to di
	mov	es,[b$VideoBase]	;Start segment of video

	add	dl,cl			; Compute ending column return value
cEnd					
; (end)

assumes	DS,DGROUP			
assumes	ES,DGROUP
;***
;B$CLREOL - Clear from specified position to end of line
;OEM-interface routine
;
;Purpose:
;	This OEM routine is responsible for clearing a line from
;	the given row and column position to the width of the screen.
;
;	Some systems support two byte printable characters by using
;	both a hardware and a software character generator.  In these
;	systems, it is necessary to clear both the character RAM and
;	the corresponding area of the graphics RAM.
;
;Entry:
;	[DL] = line number
;	[DH] = column number
;
;Exit:
;	none
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
;Algorithm:
;	determine number of spaces to write
;	set up params for bios call
;	erase to end of line
;	display off cursor at b$CURSOR  
;
;#**
cProc	B$CLREOL,<PUBLIC,NEAR>,<AX,BX,CX,DX> 
cBegin

	CALL	B$OFFCSR 	; request off cursor and position
	MOV	CL,b$ScrWidth	;get current screen width
	SUB	CL,DH		;[CL] = number of spaces to end of line
	INC	CL
	XOR	CH,CH		;zero CH
	MOV	BH,b$ActPage	;get active display page
	MOV	BL,[b$NullColor] ; b$NullColor must be used in text modes!
	CMP	[b$ScreenMode],4 ; Is this Olivetti 640x400 mode?
	JNE	NotMode4	; brif not
	MOV	BL,[b$ForeColor] ;  must use foreground color!
NotMode4:			
	MOV	AL," "		;load a blank
	SCNIOS	vWriteChar	;erase to end of line
	MOV	DX,b$CURSOR	; DX = previous cursor position
	CALL	B$OFFCSR 	; request off cursor and re-position
cEnd				

; Moved here with revision [40].
;
;***
;B$BLEEP -- Beep the speaker.
;OEM-interface routine
;
;Purpose:
;	Beep the speaker.  This routine has been separated from
;	B$DONOTE so that the entire sound support does not have
;	to exist in order to Beep the speaker.	This routine
;	should wait until any music currently playing (foreground
;	or background) has finished.
;
;	Moved here from LLQUE.ASM for increased /O modularity.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	None
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$BLEEP,<PUBLIC,NEAR>,<AX,DX>
cBegin


	MOV	DL,07H		; [DL] = ^G (beep the speaker)
	MOV	AX,0601H	; [AH] = Console I/O
				; [AL] = 1 for comparison purposes
BEEP_WAIT:			
	CMP	b$MUSIC,AL	; is music running ?
	JE	BEEP_WAIT	; wait until all notes are played
	INT	21H		

cEnd				; End of B$BLEEP

sEnd	RT_TEXT 		


sBegin	CN_TEXT

;Moved here from IOCONS.ASM with [41]
;
;***
;B$CON_SOUT - Write one byte to the console.
;OEM-Interface routine
;
;Purpose:
;	This routine will send a single byte to the CONS: device.
;	This is a redirectable output only device.
;
;Entry:
;	AL = Byte to output
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX,BX,CX,DX
;
;Exceptions:
;	Device unavailable if a detached process
;****
cProc	B$CON_SOUT,<PUBLIC,NEAR>,<AX,BX,CX,DX>
cBegin
	CMP	AL,255		
	JZ	CONSOX		;Don't allow outputing FFH
	MOV	DL,AL
	CALLOS	DCIO		;Direct Console I/O
CONSOX:
cEnd


sEnd	CN_TEXT
	END
