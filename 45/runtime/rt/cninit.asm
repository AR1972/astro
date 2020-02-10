
	TITLE	CNINIT.ASM - Console I/O Initialization/Termination module
;***
;CNINIT.ASM - Console I/O initialization/termination module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains screen and keyboard initialization and termination
;	support for the BASIC 3.0 runtime.  This module will only be present
;	in a user's program when a program contains I/O statements.
;
;******************************************************************************

	INCLUDE switch.inc
	INCLUDE rmacros.inc

;
;	Code Segments
;
	USESEG	<INIT_CODE>	;Initialization
	USESEG	<CN_TEXT>	;Console I/O
	USESEG	<RT_TEXT>	;Runtime core
	USESEG	<ST_TEXT>	;String
	USESEG	<DV_TEXT>	

;
;	Data Segments
;
	USESEG	<_BSS>		;runtime data (uninitialized)
	USESEG	<_DATA> 	;runtime data (initialized)
	USESEG	<XIB>		; initializer segment
	USESEG	<XI>		;initializer segment
	USESEG	<XIE>		; initializer segment

	INCLUDE seg.inc
	INCLUDE compvect.inc	;component vectors
	INCLUDE messages.inc	;messages and associated numbers
	INCLUDE files.inc
	INCLUDE dc.inc
	INCLUDE idmac.inc	
	INCLUDE const.inc	

	INITIALIZER	B$xCNINI	;put B$xCNINI in initializer list.

	SUBTTL	Runtime code externals
	PAGE
sBegin	DV_TEXT 			
	externNP	B$NearRet	;for disp vectors in compvect.inc
sEnd	DV_TEXT 			

sBegin	RT_TEXT 		
	externNP	B$PUTNUM	;print message from message number
sEnd	RT_TEXT 		

sBegin	ST_TEXT
sEnd	ST_TEXT

sBegin	CN_TEXT

	externNP	B$CLREOL		; clear to end of line
	externNP	B$LastLine	; clear last line
	externNP	B$KEYDSP	; display function keys if on
	externNP	B$KYBINI	
	externNP	B$TTYIN		
	externNP	B$SCNLOC	; update b$CURSOR & displ user csr
	externNP	B$GETCSRDATA	; get active page cursor position
	externNP	B$OFFCSR	; display user cursor
	externNP	B$RESETSCN	; reset screen to entry-time state
	externNP	B$GWTERM	; OEM's routine

sEnd	CN_TEXT
	SUBTTL	Runtime data definitions for BASIC Console I/O
	PAGE
sBegin	_DATA

;
;	External data
;
	externW b$ini_disp	;One time initialization dispatch table
	externW b$term_disp	;One time termination dispatch table
	externW b$run_disp	;RUN time initialization dispatch table
	externW b$shli_disp	;SHELL time initialization dispatch table
	externW b$shlt_disp	;SHELL time termination dispatch table
	externW b$errnum	;error number - non-zero after error
	externB b$Chaining	;non-zero if we are CHAINing.
	externB	b$IOFLAG	; Misc. IO flags.  Defined in GWINI.ASM
	externW	b$CURSOR	; (1,1)-relative screen cursor
	externW	b$CurPages	; current active page and visual page
	externB	b$ActPage	; current active page

sEnd	_DATA

sBegin	_BSS

;
;	Local data
;
	staticB ctlcsave,?	;Intial Control C processing state.

sEnd	_BSS

	SUBTTL	Runtime Console I/O Initialization
	PAGE
assumes CS,INIT_CODE
sBegin	INIT_CODE

;***
;B$xCNINI - Console I/O initializer
;PLM B$xCNINI()
;
;Purpose:
;	Initializer for Console I/O component.	This routine is called
;	by the Crt0 startup before _main is called.  It will update the
;	indirect dispatch tables for the console I/O routines.	This
;	insures that the only time that Console I/O is accessed is when
;	this module is linked into the user program.
;
;Entry:
;	None.
;
;Exit:
;	b$ini_disp.CN_IVEC	- contains pointer to B$CNINI
;	b$term_disp.CN_TVEC	- contains pointer to B$CNTERM
;	b$run_disp.CN_RVEC	- contains pointer to B$CNRUNINI
;	b$shli_disp.CN_SIVEC	- contains pointer to B$CNKBDSET
;	b$shlt_disp.CN_STVEC	- contains pointer to B$CNKBDRESET
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$xCNINI,<FAR>
cBegin
;
;	update "ONE" time initialization dispatch address to B$CNINI
;
	MOV	WORD PTR [b$ini_disp].CN_IVEC,CN_TEXTOFFSET B$CNINI 
;
;	update "ONE" time termination dispatch address to B$CNTERM
;
	MOV	WORD PTR [b$term_disp].CN_TVEC,CN_TEXTOFFSET B$CNTERM
;
;	update "RUN" time initialization dispatch address to B$CNRUNINI
;
	MOV	WORD PTR [b$run_disp].CN_RVEC,CN_TEXTOFFSET B$CNRUNINI
;
;	update SHELL "initialization" dispatch address to B$CNKBDSET
;
	MOV	WORD PTR [b$shli_disp].CN_SIVEC,CN_TEXTOFFSET B$CNKBDSET
;
;	update SHELL "termination" dispatch address to B$CNKBDRESET
;
	MOV	WORD PTR [b$shlt_disp].CN_STVEC,CN_TEXTOFFSET B$CNKBDRESET
cEnd
sEnd	INIT_CODE	

	PAGE		
assumes CS,CN_TEXT	
sBegin	CN_TEXT 	

;***
;B$CNINI	- One time initialization for Console I/O
;void pascal B$CNINI()
;
;Purpose:
; BC3
; ---
;	Initializes screen and keyboard for standard character
;	I/O.  B$CNINI does the following:
;		Determines if input and/or output is redirected, and sets
;			the appropriate bits in b$IOFLAG.
;		Saves high level screen dimensions
;		Save initial screen variables (low level)
;	  DOS 3 specific:
;		Save CTL-C state
;		Set ignore CTL-C
;		Save/Set keyboard interupt vector (low level)
;	  DOS 5 specific:
;		Set ignore ctl-c signal
;		Key board key trapping (??)
;
; QB3
; ---
;	Same as above. (WHO/HOW does CTL-C get handled for QB3??)
;
;Entry:
;	None.
;
;Exit:
;
;Uses:
;
;Exceptions:
;
;****
cProc	B$CNINI,<PUBLIC,NEAR>		
cBegin


;	Get the DOS information for file handles 0 and 1, and set the
;	appropriate bit of b$IOFLAG if I/O is redirected.

	MOV	BX,STDOUT		; start with stdout
	MOV	CL,RED_OUT		; test for redirected output

REDIR_LOOP:				; loop will only go through twice
;	CL = bit mask RED_INP (when BX = stdin) or RED_OUT (when BX = stdout)

	MOV	AX,C_IOCTL SHL 8 + 0	; prepare to issue IOCTL(0)
	CALLOS				; get device info
	TEST	DL,FL_CHAR		; is handle a file?
	JZ	REDIR_IO		; brif so -- redirected

					; DX = device information
	TEST	DL,FL_CONINP OR FL_CONOUT ; console device?
	JNZ	NO_REDIR_IO		; brif so -- not redirected
REDIR_IO:				
	OR	b$IOFLAG,CL		; set appropriate redir flag
NO_REDIR_IO:				

	MOV	CL,RED_INP		; prepare to check stdin
	DEC	BX			;  for redirected input
	JZ	REDIR_LOOP		; brif stdin not processed yet


	CALL	B$CNKBDSET		;initialize the keyboard

CNINIExit:				

cEnd
	PAGE

;***
;B$CNRUNINI - RUN time console i/o initialization.
;void pascal B$CNRUNINI()
;
;Purpose:
; BC3 - void pascal B$CNRUNINI()
; ---
;	B$CNRUNINI does the following:
;		Set initial cursor variables (low level)
;		Set initial Window boundaries (high and low level)
;		Clear screen and home cursors.
;		Redisplay function keys if ON.
;
; QB3 - void pascal B$CNRUNINI()
; ---
;	Same as above. (How about Function Keys???)
;
;Entry:
;	None.
;
;Exit:
;	??
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;****
cProc	B$CNRUNINI,<NEAR>	
cBegin
cEnd


	PAGE


;***
;B$CNKBDSET - Keyboard Initialization
;void pascal B$CNKBDSET()
;
;Purpose:
;	This routine is responsible for initializing the keyboard.
;	It gets called during system initializaition, RUN, CHAIN,
;	and SHELL.
; BC3
; ---
;	Initializes keyboard for standard character I/O.
;	  DOS 3 specific:
;		Save CTL-C state
;		Set ignore CTL-C
;		Save/Set keyboard interupt vector (low level)
;	  DOS 5 specific:
;		Set ignore ctl-c signal
;		Put Keyboard in Binary mode [25]
;		Key board key trapping (??)
;
; QB3
; ---
;	Same as above. (WHO/HOW does CTL-C get handled for QB3??)
;
;Entry:
;	None.
;
;Exit:
;	ctlcsave - contains current control c processing state.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;
;****
cProc	B$CNKBDSET,<NEAR>	
cBegin
	XOR	AL,AL		;Get control c function code
	CALLOS	CTRLC		;Read control c processing state.
	MOV	ctlcsave,DL	;save initial control c state.
	XOR	DL,DL		;turn off control C processing
	MOV	AL,1		;Set control c function code
	CALLOS	CTRLC		;turn off control c processing.
cEnd

	SUBTTL	Runtime Console I/O termination routines
	PAGE
;***
;B$CNKBDRESET - Keyboard Termination
;void pascal B$CNKBDRESET()
;
;Purpose:
;	This routine is responsible for resetting the keyboard.
;	It gets called during system termination, RUN, CHAIN,
;	and SHELL.
; BC3
; ---
;	resets keyboard for standard character I/O.
;		reset CTL-C state
;		reset keyboard interupt vector (low level)
;
;Entry:
;	ctlcsave - contains control c processing state upon entry.
;
;Exit:
;	None.
;
;Uses:
;	None.
;
;Exceptions:
;	None.
;
;****
cProc	B$CNKBDRESET,<NEAR>	
cBegin
	MOV	DL,ctlcsave	;get initial control c state.
	MOV	AL,1		;Set control c function code
	CALLOS	CTRLC		;reset control c processing.
cEnd

sEnd	CN_TEXT
	PAGE

assumes CS,CN_TEXT
sBegin	CN_TEXT
;***
;B$CNTERM	- One time initialization for Console I/O
;void pascal B$CNTERM()
;
;Purpose:
; BC3
; ---
;	Initializes screen and keyboard for standard character
;	I/O.  B$CNTERM does the following:
;		Saves high level screen dimensions
;		Save initial screen variables (low level)
;	  DOS 3 specific:
;		Save CTL-C state
;		Set ignore CTL-C
;		Save/Set keyboard interupt vector (low level)
;	  DOS 5 specific:
;		Set ignore ctl-c signal
;		Key board key trapping (??)
;
; QB3
; ---
;	Same as above. (WHO/HOW does CTL-C get handled for QB3??)
;
;Entry:
;	None.
;
;Exit:
;
;Uses:
;
;Exceptions:
;
;****
cProc	B$CNTERM,<NEAR> 	
cBegin
	CMP	b$errnum,0	;Has an error occurred?
	JZ	NOPAUSE 	;brif not.
	CALL	FAR PTR B$Pause ;Yes: display message and wait for char.
NOPAUSE:
	CMP	b$Chaining,0	;Are we in the process of chaining?
	JNZ	Chaining	;brif so.
	TEST	b$IOFLAG,SCN_INIT ; have we done any output yet?
	JZ	NoReset		; brif not -- don't reset screen
	CALL	B$RESETSCN	; reset screen to entry time state
NoReset:

	CALL	B$GWTERM	; OEM termination

Chaining:			
	cCall	B$CNKBDRESET	;RESET the keyboard
cEnd

;***
;B$Pause - wait until user hits a key to return.
;
;Purpose:
;	This module is needed on machines that clear the screen when resetting
;	screen mode. The error message would just flash on the screen before
;	being erased in B$CNTERM (which calls OEM's B$GWTERM routine).
;	Also used when QBI compile dialog spawns processes which have
;	errors.  The Hit any key message will be displayed before returning
;	to the user interface, which will redisplay the editting window.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per convention.
;
;Exceptions:
;	None.
;****
cProc	B$Pause,<PUBLIC,FAR>	
cBegin

	TEST	b$IOFLAG,RED_INP+RED_OUT ; input or output redirected?
	JNZ	PauseExit	; brif so -- do nothing

	MOV	DX,b$CURSOR	; DX = logical cursor for this page
	PUSH	DX		; save it for later
	MOV	AX,b$CurPages	; AX = [visual|active] pages
	PUSH	AX		; save old page values
	CMP	AH,AL		; were active and visual pages the same?
	JE	SamePages	; brif so -- don't read cursor position,
				; since it may be wrong.
	MOV	b$ActPage,AH	; active page = visual page
	CALL	B$GETCSRDATA	; DX = cursor position for this page
SamePages:			
	PUSH	DX		; save cursor for visual page
	CALL	B$LastLine	; DX = b$CURSOR = first column of last line
	CALL	B$CLREOL	; erase last line (cursor left at start)
	MOV	AX,MS_HITTORETURN ;"Hit any key to return to system"
	cCall	B$PUTNUM	;print the message


	CALL	B$KYBINI 	; Erase any typeahead
	CALL	B$TTYIN		; Wait for any key

	CALL	B$LastLine	; DX = b$CURSOR = first column of last line
	CALL	B$CLREOL	; erase last line (cursor left at start)

	POP	DX		; DX = original cursor for visual page
	CALL	B$OFFCSR	; display off cursor at original location
				; on visual page (forces relocation)
	POP	AX		; restore active page to old value
	MOV	b$ActPage,AL	

	POP	DX		; DX = old logical cursor for active page
	CMP	AH,AL		; were active and visual pages the same?

	JNE	NoDisp		; brif not -- don't display function keys
	CALL	B$KEYDSP	; display function keys in active page if
				; on (clears last line and restores cursor)
NoDisp:				
	
	CALL	B$SCNLOC	; update b$CURSOR and display user cursor

PauseExit:			
cEnd
sEnd	CN_TEXT
	END
