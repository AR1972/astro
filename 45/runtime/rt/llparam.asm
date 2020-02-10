	TITLE	LLPARAM - Parameters
;***
; LLPARAM.ASM - parameters
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

	INCLUDE switch.inc	;switch file [new]
	INCLUDE rmacros.inc	;useseg macro

	USESEG	RT_TEXT 	;core runtime segment
	USESEG	_BSS		
	USESEG	_DATA		

	INCLUDE seg.inc 	;segment definitions
	INCLUDE idmac.inc	;internal debug macros
	INCLUDE oscalls.inc	;Dos 5 structures


sBegin	_BSS			

	externB b$ScreenMode	
	externW b$HorzRes	
	externW b$VertRes	
	externW b$FBColors	
	externB b$BackColor	
	externB b$CharColor	
	externB b$BLDSP 	
	externB b$WHDSP 	

;***
;b$ComPort - Memory Address of COM ports
;OEM-interface routine (variable)
;
;Purpose:
;	This variable stores the memory locations of the two COM
;	ports.	It may actually be used for any purpose, as long
;	as the restrictions mentioned below are followed.
;
;Allocation:
;	b$ComPort is 2 consecutive WORDS allocated in the _BSS segment
;	by the OEM independent code.
;
;Values:
;	As defined by the operating system
;
;Initally Set:
;	Both words are statically initialized to 0.
;
;Modified by:
;	These values are only modified by the OEM-dependent code.
;
;Used by:
;	The OEM-Independent code uses these values to determine if a
;	COM port needs to be initialized after a SHELL command.  At the
;	time of the shell, it copies both words to a safe location and
;	if a word is non-zero the corresponding COM port is disabled by
;	a call to B$TRMCOM.  After the SHELL returns, all the COM ports
;	that were disabled are re-enabled with calls to B$INICOM.
;****
	globalW b$ComPort,,2	;communications port addresses
	staticW BUF,,1		;Two byte buffer for B$STDGET and B$STDPUT

sEnd	_BSS			

sBegin	RT_TEXT 		
	assumes CS,RT_TEXT	



;***
;B$STDGET - Get a character from STDIN
;OEM-interface routine
;
;Purpose:
;	Gets a character from the Operating System's standard input.
;	This routine is used to read a character from an input that
;	might have been redirected.
;
;	The existence of this routine allows the OEM to do pre-
;	processing of characters.  If this feature is not needed,
;	the routine will just ask the Operating System for a character.
;	Under DOS, this would be function 3FH of interrupt 21H.  Under
;	OS/2, this function DOSREAD will perform the action.
;
;	For system that support double byte charters, this routine should
;	return a double byte character as a single character, not as two
;	separate bytes as in previous versions.
;
;Entry:
;	None
;
;Exit:
;	[AX] = character
;	PSW.Z set indicates no character was available
;	PSW.C set if the character is two bytes
;	PSW.C reset if the character is only one byte ([AH] ignored)
;Uses:
;	Per convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****
cProc	B$STDGET,<PUBLIC,NEAR>,<BX,CX,DX> 
cBegin
	XOR	BX,BX		;file handle in BX
	MOV	CX,1		;no. of bytes in CX
	MOV	DX,OFFSET DGROUP:BUF ;offset of BUF in DX
	MOV	AH,3FH		;Function 3FH and
	INT	21H		;INT 21H
	OR	AX,AX		;is AX = 0?
	JE	STDRET		;Brif so PSW.Z is set (error)
	MOV	AL,BYTE PTR BUF ;move byte into AL
STDRET:

cEnd



;***
;B$STDPUT - Write a character to STDOUT
;OEM-interface routine
;
;Purpose:
;	Writes a character to the Operating System's standard output.
;	This routine is used to write a character to an output that
;	might have been redirected.
;
;	The existence of this routine allows the OEM to do pre-
;	processing of characters.  If this feature is not needed,
;	the routine will just send the character to the Operating
;	System.
;
;Entry:
;	[AX] = character
;	PSW.C set indicates a two byte character
;
;Exit:
;	None
;
;Uses:
;	Per convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None
;****

cProc	B$STDPUT,<PUBLIC,NEAR>,<BX,CX,DX> 
cBegin					   
	MOV	BX,1		;BX contains the file handle
	MOV	CX,1		;CX contains the no. of bytes
	MOV	BYTE PTR BUF,AL ;move the byte into BUF
	JNC	ONEBYT		;if no carry then write one byte
	INC	CX		;else it is two bytes
	XCHG	AL,AH		; set proper byte order
	MOV	BUF,AX		;move the byte into AX
ONEBYT:
	MOV	DX,OFFSET DGROUP:BUF ;DS:DX contains address of the data
	MOV	AH,40H		;Function 40H and
	INT	21H		;INT 21H
PUTRET:
cEnd

;
;	Rewritten with [7].
;
;***
;B$GrScreenSize - Get size of Graphics Screen in Pixels
;OEM-interface routine
;
;Purpose:
;	This routine returns the pixel dimensions of the current graphics
;	screen.  The dimension are returned in 0-relative form as the
;	number of pixels per line and the number of pixel lines per
;	screen page.
;
;Entry:
;	none
;
;Exit:
;	[CX] = the number of pixels per pixel (0-relative)
;	[DX] = the number of pixel lines per screen page (0-relative)
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX
;
;Exceptions:
;	none
;****

cProc	B$GrScreenSize,<PUBLIC,NEAR>
cBegin
	mov	cx,b$HorzRes
	dec	cx		    ;make 0-relative
	mov	dx,b$VertRes
	dec	dx		    ;make 0-relative
cEnd
;
;	Rewritten with [7].
;
;***
;B$GRMODE - Check for a graphics mode
;OEM-interface routine
;
;Purpose:
;	This routine returns the current screen mode. PSW.Z will
;	be set if the screen is in text only mode. PSW.Z will be
;	cleared if the screen is in a graphics mode. This routine
;	is called whenever a graphics routine is to be executed to
;	make sure that graphics are allowed in the current screen
;	mode.
;
;	To disable all graphics statements with a Function Call
;	Error, always return with PSW.Z set.
;
;Entry:
;	None.
;
;Exit:
;	PSW.Z set if in text only screen mode
;	PSW.Z cleared if in a graphics screen mode
;
;Uses:
;	Per convention
;
;Preserves:
;	All registers
;
;Exceptions:
;	None.
;****

cProc	B$GRMODE,<PUBLIC,NEAR>
cBegin
	cmp	b$ScreenMode,0
cEnd

;***
;B$GETFBC - Get foreground and background attributes
;OEM-interface routine
;
;Purpose:
;	The routine B$GETFBC retrieves the current foreground and
;	background attributes of the screen.  It supports graphics
;	and function key display capability.  The attributes are in
;	external format (single byte integer corresponding to the
;	documented color encoding) and are returned in the low byte
;	of the register.
;
;	On entry, the Carry flag determines whether the color attributes
;	will be for text or for graphics.  In most systems, these will
;	be the same.
;
;Entry:
;	PSW.C = set implies graphics attribute request
;	PSW.C = reset implies text attribute request
;
;Exit:
;	AX = foreground attribute
;	BX = background attribute
;
;Uses:
;	Per convention
;
;Preserves:
;	CX, DX
;
;Exceptions:
;	None.
;****
;NOTE:	the current version of this routine will not distinguish
;	between text or graphics requests
;
cProc	B$GETFBC,<PUBLIC,NEAR> 
cBegin
	mov	ax,b$FBColors	;al=fore, ah=background color
	xor	bx,bx		;HOBytes 0 after xchg
	xchg	bl,ah		;bx=background
cEnd

;***
;B$SETFBC - Set foreground and background colors
;OEM-interface routine
;
;Purpose:
;	The routine B$SETFBC sets the current foreground and
;	background attributes for effecting reverse video
;	on the function key display line.   The attributes are in
;	external format (single byte integer corresponding to the
;	documented color encoding) and are passed in the low byte
;	of the register.
;
;	On entry, the Carry flag determines whether the color attributes
;	will be for text or for graphics.  In most systems, these will
;	be the same.
;
;	Since the attributes from B$GETFBC are not saved across a
;	statement boundary, the attributes passed to B$SETFBC should
;	be valid with respect to the current configuration.
;
;Entry:
;	[AX] = foreground attribute
;	[BX] = background attribute
;	PSW.C = set implies graphics attribute request
;	PSW.C = reset implies text attribute request
;
;Exit:
;	none
;
;Uses:
;	Per Convention
;
;Preserves
;	All registers
;
;Exceptions:
;	none
;****

cProc	B$SETFBC,<PUBLIC,NEAR>,<CX> 
cBegin
	JB	SETRET		;br. if graphics
	MOV	CL,b$CharColor	;get current attributes
	CMP	b$BackColor,0	;is background black?
	JA	CLRTBG		;br. if not
	XCHG	CL,b$BLDSP	;swap reverse video for black and current attributes
	JMP	SHORT SetExit	;finished swap for black background, return.
CLRTBG:
	XCHG	CL,b$WHDSP	;swap reverse video for color bg and current attributes
SetExit:
	MOV	b$CharColor,CL	;store reverse attributes as current attributes
SETRET:
cEnd

sEnd	RT_TEXT 		
	END
