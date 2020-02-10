	TITLE	GWINI - GW BASIC 2.0 Initialization/Utility Routines
;***
; GWINI - GW BASIC 2.0 Initialization/Utility Routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;	The routines in this module are always called; regardless of
;	whether the statements they are initializing are found in the
;	program or not. This module should not cause any statement
;	processing module to be linked in (i.e. only low-level routines
;	should be called).
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	; general runtime macros

	UseSeg	_BSS		
	UseSeg	_DATA		
	UseSeg	ER_TEXT 	
	UseSeg	RT_TEXT 	

	INCLUDE seg.inc 	; Segment definitions
	INCLUDE baslibma.inc
	INCLUDE files.inc
	INCLUDE nhutil.inc	
	INCLUDE idmac.inc	; Internal debugging macros
	INCLUDE const.inc	; bit flag constants
	INCLUDE rtps.inc	; constants shared with QBI

	.radix	10

sBegin	_DATA			

globalW b$pCommSave,Near_Ret,1	; conditional vector to B$CommSave
globalW b$pCommRestore,Near_Ret,1 ; conditional vector to B$CommRestore

; b$TempSD is a general purpose string descriptor for temporary use
globalW b$TempSD,?,2		; handle (ppData) & SB

externB	b$CRTWIDTH		; Physical width
externB	b$SCRNWIDTH		; Logical width
externB b$WDOBOT		
externB b$WDOTOP		

externW	b$CURSOR		; (1,1)-relative screen cursor
externB b$LINCNT		
externW b$VWINI_PTR		;Calls B$GRPINI once initialized
externW __aenvseg



;			  Added with revision [28]
;***
;b$IOFLAG - General Purpose IO Bits
;OEM-callback routine (variable)
;
;Purpose:
;	This variable keeps track of the global state of the I/O module.
;	Most of the bits in the flag variable are used to handle all the
;	different cases for I/O redirection.
;
;Allocation:
;	b$IOFLAG is a BYTE value declared in the _DATA segment by
;	the OEM-Independent code.
;
;Values:
;	Field definitions of b$IOFLAG are in CONST.INC.  Initially, all
;	fields are set to 0.
;
;	RED_INP is a bit that is set when input from stdin is being redirected
;	    from a file.  This field is set during console initialization
;	    by the runtime and is not changed.	(01H)
;
;	RED_OUT is a bit that is set when output stdout is being redirected
;	    to a file.	This field is set during console initialization
;	    by the runtime and is not changed.	(02H)
;
;	LPR_ECHO is a bit that is set when output to the screen is being
;	    echoed to the line printer.  Its value does not affect redirected
;	    IO, but the printer will not echo redirected output.  (04H)
;	    This field is set/cleared when the Print Scrn key is recognized.
;
;	F_KDSP is a bit that is set when currently updating a function key
;	    display, and output should not be sent to the printer or a
;	    redirected file.  A special routine to map the function keys
;	    is called when this is true.  (08H)
;
;	IN_INPUT is a bit that is set when redirected output to stdout is
;	    to be inhibited.  This is when an INPUT statement is being
;	    processed, and the user is editing his input.  Not until the
;	    user hits <return> is the data entered sent to the redirected
;	    file, so that the editing corrections are not present.
;	    However, while IN_INPUT is set , output will go to the screen,
;	    using screen cursor values. (10H)
;
;	F_EDIT is a bit that is set when in the INPUT statement, and B$PRTMAP
;	    should be disabled as B$EDTMAP does the mapping.  (20H)
;
;	SCN_INIT is a bit that is set to indicate that the screen has been
;	    physically initialized. This field is set by the OEM at the
;	    time the screen is actually initialized (B$SCINIT).  (40H)
;
;	SCN_SCROLL is a bit indicating that we need to scroll the screen
;	    upon the first screen operation because we entered on the last
;	    line of the screen.  This flag is only used by the OEM before
;	    the screen has been physically initialized.  (80H)
;
;Initially Set:
;	RED_INP is set by the OEM-Independent code before B$GWINI or
;		B$RTLLINIT are called.
;	RED_OUT is set by the OEM-Independent code before B$GWINI or
;		B$RTLLINIT are called.
;	LPR_ECHO is statically initialized to be 0.
;	F_KDSP	is statically initialized to be 0.
;	IN_INPUT is statically initialized to be 0.
;	F_EDIT	is statically initialized to be 0.
;	SCN_INIT is statically initialized to be 0.
;	SCN_SCROLL is statically initialized to be 0.
;
;Modified By:
;	RED_INP and RED_OUT should not be modified once initialized.
;	LPR_ECHO is modified by the OEM-Independent code upon detection
;		of the Print Screen Key and upon a RUN command.
;	F_KDSP is set at the beginning of the function key update routines
;		and cleared at the end of them.
;	IN_INPUT and F_EDIT are set and cleared by the Screen Editor
;	SCN_INIT and SCN_SCROLL are only modified by the OEM-Dependent code.
;
;Used By:
;	All the fields of b$IOFLAG except for SCN_INIT and SCN_SCROLL are
;	only used by the OEM-Independent Code. SCN_INIT is used by both
;	the OEM-Dependent code and the termination code.  If SCN_INIT
;	is 1 at termination time, then B$RESETSCN will be called.  Otherwise
;	B$RESETSCN will not be called by the termination code.	SCN_SCROLL
;	is not used by the OEM-Independent Code in any way.  This flag
;	may be used for any purpose.
;****

globalB	b$IOFLAG,0		; general-purpose IO bits defined above.

;*** 
;b$vKEYDSP - Vector for indirect call to B$KEYDSP, display/clear function keys
;OEM-callback routine (variable)
;
;Purpose:
;	Update the function key display.  If b$KEY_SW is set to 0 then
;	the function key display is disabled and is removed from the
;	screen if it existed.  If b$KEY_SW is set to -1, then the
;	function key display is enabled and is displayed on the screen
;	if needed.
;
;	This routine is called indirectly through the vector
;	b$vKEYDSP
;
;Entry:
;	B$KEY_SW set appropriately:  0 = turn off function key display
;				    -1 = display function keys.
;
;Exit:
;	None.
;
;Uses:
;	Per Convention
;
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	Just returns unless B$KEYDSP is linked in by B$INPP or B$KFUN.
;
;Vector Information:
;	b$vKeyDSP is a WORD value defined in the _DATA segment by the
;	OEM-Independent code.  It is set up by an initializer before
;	the BASIC runtime is first called.  It should not be modified.
;******************************************************************************

globalW	b$vKEYDSP,Near_Ret,1	


sEnd	_DATA			


sBegin	_BSS			

externB   b$KEY_SW	  ;GWDATA -
externW   B$WNDWSW	  ;GWDATA -

;***
; b$HugeShift, b$HugeDelta - OS selector shift count for HUGE access
;OEM-callback routine (variable)
;
;Purpose:
;	This value is used to access successive pieces of a
;	HUGE array.  To get to the next segment of memory in
;	which the array exists, use the following formula:
;	(assuming that the array is accessed through DS:)
;
;		DS:  =	DS: + (1 << b$HugeShift)
;
;	The value (1 << b$HugeShift) is precomputed and stored
;	in the variable b$HugeDelta.
;
;	NOTE: For DOS, these variables are guarenteed to be identical
;	      each time the program is run and could be replaced by
;	      constants.
;
;Allocation:
;	b$HugeShift is a BYTE value defined in the _BSS segment by
;	the runtime code.
;
;	b$HugeDelta is a WORD value defined in the _BSS segment by
;	the runtime code.
;
;Values:
;	The values for b$HugeShift and b$HugeDelta are dependent upon
;	the operating system.  Both variables are initialized by the
;	runtime during runtime initialization and should not be changed.
;
;Initially Set:
;	The values of these variables are undefined until they are set
;	at initialization time.  They are initialized after B$GWINI and
;	and B$RTLLINIT are called but before any user code is executed
;	or interpreted.
;
;Modified By:
;	These variables should not be modified once they are initialized.
;
;Used By:
;	These variables are used by anyone who wants to access a dynamic
;	array bigger than 64K.
;****************************************************************************

	globalB b$HugeShift,?	;OS selector shift count for HUGE access
				;NOTE: this uses an extra byte from
				;b$HugeDelta when we pass b$HugeShift to
				;DOSGETHUGESHIFT (which returns a WORD)
	globalW b$HugeDelta,?	;OS selector seg increment for HUGE access

;***
;b$Buf1, b$Buf2 - temporary buffer space
;OEM-callback routine (variable)
;
;Purpose:
;	Both b$Buf1 and b$Buf2 are large buffers of temporary storage
;	for general use.  These buffers are used in many places
;	throughout the runtime.  While it is theoretically possible for
;	the OEM code to use either or both of these buffers any time they
;	are not in use, there is no way to determine whether they are being
;	used.  The mechanism in the rest of the runtime for determining
;	availability is visual inspection and use of the routines
;	B$HoldBuf(n) and B$FreeBuf(n) which do not exist in a release
;	version.
;
;	These buffers are mainly described here for the understanding
;	of the sample code supplied by MicroSoft.  However, there are a
;	couple of specific times the buffers are guaranteed to be free,
;	which are described below.
;
;	Note that b$Buf2 immediately follows b$Buf1, so if both are
;	available you have a block of 258 contiguous bytes of DGROUP.
;
;Allocated:
;	b$Buf1 is a block of 129 BYTES allocated in _BSS by the runtime.
;	b$Buf2 is a block of 129 BYTES allocated in _BSS by the runtime.
;
;Values:
;	Undefined
;
;Initially Set:
;	Undefined
;
;Modified By:
;	Undefined
;
;Used By:
;	b$BUF1 is guaranteed to be available during the calls to
;		B$GWINI and B$RTLLINI.	Furthermore, it is guaranteed
;		that the value of b$BUF1 will not change between these
;		two calls.
;	b$BUF2 is guaranteed to be available with the same conditions as
;		b$BUF1.  Also, b$BUF2 will be available for use during
;		the execution of any of the routines in LLCOM5.ASM.
;		However, the contents of the buffer may change between
;		calls to these routines.
;******************************************************************************
;
; NOTE -- Any (or all) of these LARGE buffers can be used by anyone
; that wants to, provided they are not used in a calling routine.  
; IN DEBUG CODE, call B$HoldBuf(1,2,12,3) to allocate the the buffers to
; your routine, and B$FreeBuf(1,2,12,3) to release the buffers.
;
;#########

labelB	<PUBLIC,b$PATHNAM>	; pathname buffer
globalB b$Buf1,,FILNAML	; 1st large (pathname-sized) scratch buffer
globalB b$Buf2,,FILNAML	; 2nd large (pathname-sized) scratch buffer
globalB b$Buf3,,16		; 3rd scratch buffer
				; these buffers MUST remain contiguous

sEnd	_BSS			


sBegin	RT_TEXT

assumes cs,RT_TEXT

	PAGE

	SUBTTL	Screen initialization

;***
; B$WHOME - Home the text cursor
; Moved here from iotty.asm with revision [60].  See note below!
;
; Input:
;	b$WDOTOP set
; Output:
;	[DL] == home row of cursor
;	[DH] == home column of cursor
; Modifies:
;	NONE
; Note:  
;  IMPORTANT: Must be kept in sync with the local WHOME routine in iotty.asm
;****

cProc	B$WHOME,<PUBLIC,NEAR>
cBegin
	MOV	DL,b$WDOTOP
	MOV	DH,1
cEnd


;***
; B$CRLF - Adust cursor row/column while doing a CR/LF. [61]
; Added with [60].
;
; Purpose:
;	This routine is called every time a CR/LF is to be output
;	to the screen.  It checks whether the screen will need to be
;	scrolled and if not, increments DL.  Flags are set indicating
;	whether or not the screen must be scrolled. DH is set to 1.
;
; Entry:
;	DL = current line
;
; Exit:
;	DH = 1
;	DL = new line
;	ZF ==> didn't change DL, since on last line
;	NZ ==> changed DL, since not on last line
;
; Modifies:
;	None
;****
cProc	B$CRLF,<PUBLIC,NEAR>	
cBegin
	MOV	DH,1		; reset cursor column to 1  
	CMP	DL,b$LINCNT 	; on status line?
	JNE	NOT_STATUS	; brif not -- don't adjust line
	MOV	DL,b$WDOBOT 	; move cursor to bottom
				; line of text window
NOT_STATUS:
	CMP	b$WDOBOT,DL	; Are we at bottom of window?
	JE	NO_INC		; brif so -- ZF ==> last line
	INC	DX		; increment row (NZ)
NO_INC:				; return with flags set
cEnd

;***
;B$SCNCLR - Home Text & Graphics Cursor, Refresh Function Key Display
;OEM-callback routine
;
;	Re-written with revision [54].
;
;Purpose:
;	This routine is used to initialize the screen editor, reset
;	the graphics viewport and window to the screen dimensions,
;	center the graphics cursor, home the text cursor and display
;	the function keys if needed.
;
;	This routine must be called at initialization and whenever
;	screen characters are no longer accessible to the user because
;	the screen dimensions have changed.  This routine should only
;	be called from B$SCRSTT, B$SWIDTH, B$RESETSCN, and during
;	initialization.
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
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$SCNCLR,<NEAR,PUBLIC>
cBegin

	CALL	B$WHOME			; DX=text cursor home
	MOV	b$CURSOR,DX		; update b$CURSOR
	CALL	[b$vKEYDSP] 		; Conditionally display softkeys
			 		; (displays user cursor at position
					; b$CURSOR when done)

cEnd	<nogen>				; fall into B$VIEWINIT


;		Added as part of revision [30]
;***
;B$VIEWINIT - Initialize viewport, center graphics cursor
;OEM-callback routine
;
;Purpose:
;	Initialize the graphics viewport and centers the graphics cursor.
;	Sets the logical coordinate system to be identical to the
;	physical coordinate system (disable WINDOW command).
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
;Preserves:
;	AX, BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$VIEWINIT,<NEAR,PUBLIC> 
cBegin				
	CALL	[b$VWINI_PTR]	;Initialize viewport, center graphics cursor
	MOV	B$WNDWSW,0	;Turn B$WNDWSW and B$WNDWSC off
Near_Ret:			;near ret for vectors
cEnd				

;***
;B$SCNSWI - Set screen width(logical/physical) and height
;OEM-callback routine
;
;Purpose:
;	B$SCNSWI will set the screen width (both logical and physical)
;	and screen height.  Since this routine is used to communicate
;	the screen dimensions to the Screen Editor, it must be called at
;	initialization and whenever the character dimensions of the
;	screen are modified.
;
;Input:
;	AL=width, CL=height
;
;Output:
;	None.
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

cProc	B$SCNSWI,<PUBLIC,NEAR>		
cBegin					

	MOV	b$CRTWIDTH,AL	; Set physical width of screen
	MOV	b$SCRNWIDTH,AL	; Set SCRN: logical width
	MOV	B$LINCNT,CL	;Save physical height
	MOV	b$WDOTOP,1	;Init window top
	PUSH	CX
	DEC	CL		; Reserve status line
	MOV	b$WDOBOT,CL	;Set window bottom
	POP	CX
cEnd				; End of B$SCNSWI


;***
;B$UPCASE - Convert Character to Upper Case
;DBCS-callback
;
;Purpose:
;	Convert the character in AL to uppercase if possible.  If it is
;	not a character or it is already uppercase, it is not modified.
;	This is done by a comparison against the range 'a'-> 'z'.  It does
;	not use the Operating System call to case convert characters
;	outside of this range.
;
;	NOTE:  It is the caller's responsibility to make sure that it is
;	       not sending 1/2 of a KANJI character to this routine.
;
;	WARNING: Because this routine is called by B$GETCH
;
;			  DS != DGROUP
;
;		 If you go to change the code, keep this in mind!!!
;Entry:
;	AL = Character to convert
;
;Exit:
;	AL = UpperCase version of character
;
;Uses:
;	Per Convention
;
;Preserves:
;	AH, BX, CX, DX
;
;Exceptions:
;	None.
;****
cProc	B$UPCASE,<PUBLIC,NEAR>	
cBegin
	CMP	AL,'a'		;Is AL < 'a'
	JB	upret		;Skip it
	CMP	AL,'z'		;Is AL > 'z'
	JA	upret		;Skit it
upit:				
	AND	AL,255-' '	;Convert to Upper Case
upret:
cEnd


;*** 
;B$Mul32x16 -- 32 by 16 bit multiply
;
;Purpose:
;	Added with revision [68].
;
;Entry:
;	[DX|AX] = multiplicand
;	[CX]	= multiplier
;
;Exit:
;	[DX|AX] = [DX|AX] * [CX]
;	CF ==> overflow
;
;Uses:
;	BX
;
;Preserves
;	CX
;
;Exceptions:
;	None
;
;******************************************************************************
cProc	B$Mul32x16,<PUBLIC,NEAR>
cBegin
	xchg	bx,dx		; [BX|AX] = multiplicand
	mul	cx		; multiply low word by 1000 ([DX|AX] = result)
	push	ax		; save low word result
	push	dx		; save first overflow
	xchg	ax,bx		; AX = high word
	mul	cx		; [DX|AX] = result of high word multiply
	pop	dx		; DX = original high word
	jc	Overflow	; brif overflow (need to clean stack)
	add	ax,dx		; AX = high word of result (PSW.C if overflow)
	xchg	ax,dx		; DX = high word of result
OverFlow:
	pop	ax		; AX = low word of result
cEnd

	page


sEnd	RT_TEXT

	END
