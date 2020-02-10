	TITLE	OUT - Output utilities
;***
; OUT - Text Output utilities
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc	;Runtime switch file
	INCLUDE rmacros.inc	;General runtime macros

	useSeg	_DATA		;Uses the Data segment
	useSeg	_BSS		;and the BSS segment
	useSeg	RT_TEXT 	;and the core code segment
	useSeg	NH_TEXT 	

	INCLUDE seg.inc 	;Segment definitions

	INCLUDE baslibma.inc
	INCLUDE devdef.inc
	INCLUDE files.inc
	INCLUDE ascii.inc
	INCLUDE idmac.inc
	INCLUDE const.inc	

sBegin	_DATA			

	externB	b$IOFLAG	; Misc. IO flags.  Defined in GWINI.ASM
	externW	b$CURSOR	; (1,1)-relative screen cursor
	externB	b$CSRX		; 1-relative x-coordinate cursor

sEnd	_DATA			

sBegin	_BSS			

	externW	b$PTRFIL	;defined in GOSTOP.ASM

sEnd	_BSS			


	externFP __FMSG_TEXT	;Get message text from number


sBegin	RT_TEXT 		

	PUBLIC	B$TYPSTR
	PUBLIC	B$TYPSTR1
	PUBLIC	B$TYPCNT
	PUBLIC	B$OUTCNT

	externNP B$TTY_SOUT
	externNP B$STRSOUT
	externNP B$CHKLASTCOL	; check for last column on screen
	externNP B$UPDATE_CSR	; update cursor position after write


assumes CS,RT_TEXT		

	PAGE

;*** 
; B$ITCR -- type CR/LF on console.  Added with [15].
;
;Purpose:
;
;Entry:
;	None
;Exit:
;	b$PTRFIL is reset
;Uses:
;	None
;Exceptions:
;	None
;******************************************************************************
cProc	B$ITCR,<PUBLIC,FAR>
cBegin
	CALL	B$$TCR
cEnd


;*** 
; B$$TCR -- type CR/LF on console.
;
;Purpose:
;
;Entry:
;	None
;Exit:
;	b$PTRFIL is reset
;Uses:
;	None
;Exceptions:
;	None
;******************************************************************************
cProc	B$$TCR,<PUBLIC,NEAR>,<AX> 
cBegin				

	MOV	AL,ASCCR	;  Output CR only
	CALL	B$$WCHT		

cEnd				

;*** 
; B$$WCHT -- type char on console.
;
;Purpose:
;
;Entry:
;	None
;Exit:
;	b$PTRFIL is reset
;Uses:
;	None
;Execptions:
;	None
;******************************************************************************
cProc	B$$WCHT,<PUBLIC,NEAR>	
cBegin				
	MOV	[b$PTRFIL],0	  ;must be zero for TTY output
	JMP	B$TTY_SOUT
cEnd	<nogen>			; return via B$TTY_SOUT


;***
; B$PRINTNUM - Print numbered message to the screen [17]
;
;Purpose:
; Prints a string to the console device (B$PTRFIL = 0), as referenced by the
; passed message number.  Must not be used for fatal errors or if you don't
; want PRINT code pulled in.  Use B$PUTNUM in ERPROC.ASM instead. 
;
;Entry:
; [AX] = Message number
;
;Exit:
; None.
;
;Uses:
; Per convention.
;
;NOTE:	The interpreter provides a version of this routine (B$PUTNUM)
;	to print strings their way.
;
;******************************************************************************

;***
; B$PUTS - Print a null terminated string to console
;
;Purpose:
; Prints a string to the console device (B$PTRFIL = 0).
;
;Entry:
; [DX:AX] = Address of string
;
;Uses:
; Per convention
;
;NOTE:	The interpreter uses this routine, so if we change the interface
;	to use a far ptr, we need to notify them.
;
;******************************************************************************
cProc	B$PUTS,<NEAR,PUBLIC>,ES 
cBegin				
	MOV	[b$PTRFIL],0	; must be zero for TTY output
	MOV	ES,DX		; set up seg reg
	XCHG	AX,BX		; [ES:BX] points to string
	OR	DX,DX		; See if null pointer
	JNZ	PUTS_10 	; Enter the loop if not
	JMP	SHORT PUTS_15	

PUTS_5: 			
	CALL	B$TTY_SOUT	; output char
PUTS_10:			
	MOV	AL,ES:[BX]	; Get byte from string
	INC	BX		
	OR	AL,AL		; see if end
	JNZ	PUTS_5		; jump if it isn't
PUTS_15:			
cEnd				

;***
;B$TYPSTR - Output string defined by string decsriptor
;
;Purpose:
;	Output string defined by string decsriptor to the screen.
;
;Entry:
;	[BX]  = Address of string descriptor
;
;Exit:
;	String output
;
;Uses:
;	Per convention, plus SI.
;
;******************************************************************************
B$TYPSTR:
	MOV	CX,[BX]

;***
;B$TYPCNT, B$TYPSTR1 - Output n bytes of a string defined by string decsriptor
;
;Purpose:
;	Output n bytes of a string defined by string decsriptor to the
;	console.  The alternate entry point B$TYPSTR1 is used by B$PTRSTR
;	and does not check for a 0 length string nor does it update
;	b$PTRFIL.
;
;Entry:
;	[BX]  = Address of string descriptor
;	[CX]  = Count of bytes to output
;
;Exit:
;	String output
;
;Uses:
;	Per convention, plus SI.
;
;******************************************************************************
B$TYPCNT:
	JCXZ	RETL
	MOV	[b$PTRFIL],0	;must be zero for TTY output
B$TYPSTR1:			;entry point used by B$PRTSTR
	MOV	SI,[BX+2]	; SI = string address
; See if we can pump the whole string out at once, or if we must do it a
; character at a time ---

;	CMP	CX,4
;	JBE	B$OUTCNT 	;brif 4 or less chars in string: not worth it
	TEST	b$IOFLAG,RED_OUT OR LPR_ECHO OR F_EDIT	
	JNZ	B$OUTCNT 	;BRIF user wants output to echo to printer
				;or in INPUT mode or redirected output
	MOV	AL,b$CSRX	; horizontal cursor position (1-relative)
	CBW			; clear high byte
	DEC	AX		; make 0-relative
	ADD	AX,CX		;add in number of chars in string
	OR	AH,AH		; string size + cursor loc > 255?
	JNZ	B$OUTCNT 	;brif so
	XCHG	DH,AL		; DH = column to test (one less than what
				; we'll get when done)
	CALL	B$CHKLASTCOL	; more than one past last column on screen?
	JA	B$OUTCNT	; brif so -- we'll have to wrap to print it
				; If we get b$CSRX = 81 when done, that's
				; OK.
	PUSH	CX		; save string length and address
	PUSH	SI		
CTL_CHK:
	LODSB			; AL = next char
	CMP	AL,31		; is this character a control character?
	JBE	CTL_CH_FOUND	;BRIF so
	LOOP	CTL_CHK 	;loop until all chars in string are checked


CTL_CH_FOUND:
	;here if we found a ctl char in our string; print it the slower way
	POP	SI		; restore string address and length
	POP	CX		

	;Fall through into B$OUTCNT

;***
;B$OUTCNT - Output a string of characters one at a time
;
;Purpose:
;	Print a string of characters to the screen one at a time.
;	A count is given of the number of characters to be printed,
;	the string does not have to be nul terminated.
;
;	If FK_KANJI, then we also have to flush the one byte buffer
;	that B$TTY_SOUT uses to hold the first part of a double
;	byte character.  This is done in case this string contains
;	pieces of a KANJI character.
;
;Entry:
;	CX - Count of characters to be printed
;	DS:SI - Pointer to first character
;
;Exit:
;	DS:SI - Points to position after the CXth character.
;
;Uses:
;	SI as a pointer to the string, it is updated upon exit.
;
;Exceptions:
;	None.
;****
B$OUTCNT:
	LODSB
	CALL	B$TTY_SOUT
	LOOP	B$OUTCNT
RETL:	RET

	SUBTTL	TTY input supporting routine -- B$INPCRLF
	page

;***
;B$INPCRLF -- print the terminating CR/LF(s).
; Moved here from inptty.asm with revision [18] for /O granularity.
;
;Purpose:
;	This routine prints a terminating CR/LF after an INPUT or
;	LINE INPUT statement that did not have a ';' to suppress the
;	CR.  It also writes a CR/LF to the screen when output is
;	redirected and input is not redirected, in order to keep
;	successive INPUT statements from writing over each other.
;
;Entry:
;	b$IOFLAG set correctly
;Exit:
;Uses:
;	none
;Exceptions:
;	none
;***********************************************************************

cProc	B$INPCRLF,<PUBLIC,NEAR>,<AX>

cBegin
	CALL	B$$TCR			; write terminating CR/LF to either
					; screen/printer or redir. file
	MOV	AL,b$IOFLAG		
	AND	AL,RED_OUT OR RED_INP	; is output redirected and input
	CMP	AL,RED_OUT		; 	not redirected?
	JNZ	NO_EXTRA		; brif not -- no extra CR required

	; output a CR/LF to the screen and possibly the printer when output
	; redirected and input not redirected.
					; notice AL = RED_OUT !!!
	XOR	b$IOFLAG,AL		; Fake B$TTY_SOUT into thinking it
					; should to print to the screen
					; (and printer) instead of to the
					; redirected file.
	CALL	B$$TCR			; write CR/LF to screen
	OR	b$IOFLAG,AL		; Reset RED_OUT flag.
NO_EXTRA:				

cEnd					; return to caller

sEnd	RT_TEXT 		

	END
