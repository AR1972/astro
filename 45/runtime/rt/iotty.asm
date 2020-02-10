	TITLE	IOTTY  - Console driver for BASCOM-86
;***
; IOTTY  - Console driver for BASCOM-86
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - BEEP Statement:
;
;      BEEP
;	 |
;   B$BEEP
;
; - WIDTH Statement:
;
;      WIDTH size
;	 |
;      B$WIDT
;
;******************************************************************************
	include switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	DV_TEXT 	
	USESEG	OI_TEXT 	

	INCLUDE seg.inc 	
	include baslibma.inc
	include devdef.inc
	include files.inc
	include ascii.inc
	INCLUDE const.inc	
	INCLUDE rtps.inc	; constants shared with QBI

sBegin	_DATA			

	globalW b$pLHDALC_CPCT,Near_Ret,1 
	globalW b$pKYBD_OPEN,Near_Ret,1 
	globalW b$pSCRN_OPEN,Near_Ret,1 
	globalW b$pLPTECHO,LPTECHO,1 ; conditional vector to B$LPTECHO
	externW	b$CURSOR	; (1,1)-relative screen cursor
	externB	b$CSRX		; 1-relative x-coordinate cursor
	externB b$LINCNT	
	externB b$WDOTOP	
	externB b$WDOBOT	
	externB b$KEY_SW	
	externW	b$vKEYDSP	; vector for indirect call to B$KEYDSP
	externB b$CRTWIDTH	; physical width of screen (40 or 80)
	externB	b$SCRNWIDTH	; logical width of SCRN:
	externB	b$IOFLAG	; Misc. IO flags.  Defined in GWINI.ASM
	staticB b$REDCSRX,01h	; 1-relative X-coordinate cursor for
				; redirected output.
sEnd	_DATA			

sBegin	_BSS			

	externW b$PTRFIL	;defined in GOSTOP.ASM
	externB	b$LPTFDB	; defined in GLOBAL.INC
	externB	b$FILMOD	; defined in GLOBAL.INC

	globalB b$TTY_BAKCHR,?,1  ; <>0 <=> contains pushed char for TTY

	staticB TWOBYT,?,1	; <>0 means it contains the first half of a
				; two-byte (not necessarily KANJI) character
	staticB TEMPFLAG,?,1	; temporary flag to store results of
				; CHKCHAR

	; equates for use with the results of CHKCHAR
SCN_CHR    =	01h		; char should be printed on the screen
RED_CHR    =	02h		; char should be printed to redirected file
LPR_CHR    =	04h		; char should be echoed to line printer

sEnd	_BSS			

sBegin	OI_TEXT 		
	externNP B$ERR_IOE	
sEnd	OI_TEXT 		



assumes CS,DV_TEXT
sBegin	DV_TEXT

;	Run-time entries

	PUBLIC	B$D_KYBD
	PUBLIC	B$D_SCRN
	PUBLIC	B$TTY_BAKC
	PUBLIC	B$TTY_SINP
	PUBLIC	B$TTY_SOUT
	PUBLIC	B$TTY_GWID
	PUBLIC	B$TTY_BLKIN
	PUBLIC	B$TTY_BLKOUT

;	Run-time externals


	externNP B$ERR_FC
	externNP   B$SWIDTH	
	externNP B$SCROUT	
	externNP B$INFMAP	; OEM's mapping routine
	externNP B$PRTMAP	
	externNP B$BLEEP	
	externNP B$USRCSR	; display user cursor
	externNP B$SCNLOC	; update b$CURSOR and display user cursor
	externNP B$TTYGetChar	
	externNP B$CLRSCN	; clear screen
	externNP B$CRLF		; update cursor for scrolling

	externNP B$FKYMAP	

	externNP B$SCROLL	; OEM scroll routine

	externNP B$STDPUT	
;	Illegal Device Functions

KYBD_EOF EQU	B$ERR_FC
KYBD_LOC EQU	B$ERR_FC
KYBD_LOF EQU	B$ERR_FC
KYBD_WIDTH EQU	B$ERR_FC
KYBD_RANDIO EQU B$ERR_FC
KYBD_SOUT EQU	B$ERR_FC
KYBD_GPOS EQU	B$ERR_FC
KYBD_GWID EQU	B$ERR_FC
KYBD_BLKIN EQU	B$ERR_FC
KYBD_BLKOUT EQU B$ERR_FC
KYBD_DWID EQU	B$ERR_FC

SCRN_EOF EQU	B$ERR_FC
SCRN_LOC EQU	B$ERR_FC
SCRN_LOF EQU	B$ERR_FC
SCRN_RANDIO EQU B$ERR_FC
SCRN_BAKC EQU	B$ERR_FC
SCRN_SINP EQU	B$ERR_FC
SCRN_BLKIN EQU	B$ERR_FC
SCRN_BLKOUT EQU B$ERR_FC

;	Device Close Routines

SCRN_WIDTH EQU	B$SCNSWD	
SCRN_DWID  EQU  B$SCNSWD	
;
; SCRN_DWID:		; set width for next open
;	MOV	b$SCRNWIDTH,DL	; set logical screen width
;	RET
;

;	Device Independent Console Interface

DSPMAC	MACRO	func
	DW	SCRN_&func
	ENDM

B$D_SCRN:
	DSPNAM


DSPMAC	MACRO	func
	DW	KYBD_&func
	ENDM

B$D_KYBD:
	DSPNAM

	PAGE

devio	PROC	NEAR

KYBD_CLOSE:			 
SCRN_CLOSE:			 
	JMP	[b$pLHDALC_CPCT] 
KYBD_OPEN:
	JMP	[b$pKYBD_OPEN]
SCRN_OPEN:
	JMP	[b$pSCRN_OPEN]
;***
;B$TTY_SINP,KYBD_SINP - Get a byte from the keyboard (non-redirectable)
;
;Purpose:
;	Retreive a byte from the keyboard and return it to the user.
;	This routine also alows a one character push back though
;	the routine B$TTY_BAKC.  If a character is not immediately
;	ready, this routine will wait until one becomes available.
;	There is no echoing of characters.
;
;	Note that when supporting double byte characters, the low
;	level routines will return two bytes of data.  Since this routine
;	will only return one byte, the second byte has to be saved.  It
;	is saved in the backup mechanism, which is extended to hold
;	two bytes.
;
;Entry:
;	None.
;
;Exit:
;	AL = character
;	PSW.C = clear (indicates character available)
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

B$TTY_SINP:
KYBD_SINP:
	XOR	AL,AL		;Get a zero value
	XCHG	AL,[b$TTY_BAKCHR] ;Get the value of/zero out BACKUP flag/char
	OR	AL,AL		;Was there a character pushed back?
	JNE	NoDblCh 	;Yes, return it.

keyin:
	PUSH	DX		; DX gets 2nd and 3rd of 3 byte codes
	XOR	DL,DL		;No ^BREAK checking
	cCALL	B$TTYGetChar	;And get the character
	cCALL	B$INFMAP 	; OEM's mapping routine
	POP	DX
	JZ	keyin		; OEM filtered out the key - get the next
	jnc	NoDblCh 	; BRIF not 2 byte char (kanji or scan)
	call	B$TTY_BAKC	; save second half of 2-byte code
	mov	al,ah		; Return first half
KYBD_Clr_Ret:
	xor	ah,ah		; Clear High order byte

NoDblCh:
	CLC			;Clear carry
Near_Ret:			;Near return for vectors
	RET

;***
;B$TTY_BAKC, KYBD_BAKC - Back up a byte for the keyboard
;
;Purpose:
;	Push a single character "back into the keyboard" so that the
;	next call to B$TTY_SINP will re-read it.  There is room for
;	only one byte of data.
;
;	The value is stored in the variable [b$TTY_BAKCHR].  If this
;	variable has a zero value, there is no character pushed back.
;	If it is non-zero, the value is the byte that was pushed back.
;
;	If Double Byte Characters are supported, then this routine is
;	enhanced to be able to store two bytes of information.	This is
;	not to store a full character, but to store 1/2 of a character
;	gotten from B$TTYIN and 1/2 of a charcter that was pushed back.
;	In this case, b$TTY_BAKCHR is a word and has the following possible
;	values:
;		HIGH BYTE	LOW BYTE	MEANING
;		 undef		   0		No bytes pushed back
;		   0		 non 0		One Byte (Low) pushed back
;		 non 0		 non 0		Two Bytes pushed back
;						(HI, then LOW)
;
;Entry:
;	AL = byte to be saved
;
;Exit:
;	None (state variables updated)
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


B$TTY_BAKC:
KYBD_BAKC:
	MOV	[b$TTY_BAKCHR],AL ;Save Char (non-zero value indicates char)
	RET

	SUBTTL	B$TTY_SOUT and B$KanjiFlush
	PAGE


; Support code for B$TTY_SOUT/SCRN_SOUT - - - code that has been pulled
; out of the "mainstream" code, so typical use of B$TTY_SOUT falls through
; all conditional jumps

SCND_BYTE:			;here if just got second of 2 bytes
				;AX = two byte character
	MOV	TWOBYT,0	;Clear TWOBYT flag
	CMP	AL,0FFH 	;test if FFFF needs to be mapped
	JNE	SCNSO3		;if not, just output both bytes
				;   AL = FF only if it is FFFF.
	XOR	AH,AH		;change FFFF to FF for output
	JMP	SHORT SCNSO3	;jump to output the one byte

FIRST_BYTE:
				;here if have the first byte of a two-byte char
	MOV	TWOBYT,AL	;save char for next time
	POP	AX
	RET

CR_OUT:
				;here if char received is a CR
	MOV	AH,0FFh 	;  carriage return is NEVER
	JMP	SHORT SCNOUT	; graphics character! (Prevents
				;  infinite recursion in CHWRAP)
SCNSO6:
				;here if F_EDIT was non-zero, i.e., in INPUT mode
	MOV	AH,TWOBYT	;if 2nd of two bytes, first byte was
				;0FFh, and was put in TWOBYT
	OR	AH,AH		;is this the second of two bytes?
	JNZ	SCND_BYTE	;brif so
	CMP	AL,255	 	;first of two bytes?
	JZ	FIRST_BYTE	;brif so
	JMP	SHORT SCNSO3	;back to mainstream

;***
; B$TTY_SOUT, SCRN_SOUT - send a character to the screen
;
;Purpose:
;	Send a char to the screen. Handle 2-byte chars at this level.
;	A two byte character is managed by calling this routine twice,
;	once with the first byte and once with the second byte.  After
;	the second call, the two byte character will be processed.
;
;	If we do not have FK_KANJI, then the only legal two byte
;	characters are FFxx (Screen Editor Function Code) and then only
;	if F_EDIT is true.  Everything else must be a single byte.  Note
;	that 0x80 and 0xFE are codes to be printed, not part of multiple
;	character sequences.
;
;	If we do have FK_KANJI on, then we can also print any two byte
;	character that has a first byte that is valid according to
;	B$CKDBLC.
;
;	Note that any control characters or double byte characters are
;	sent through B$PRTMAP before printing.	This may introduce
;	a Screen Editor Function which will be resolved internally or
;	it might indicate that there is no character to be printed.
;
;Input:
;	[AL] == character to send to screen
;
;Output:
;	NONE
;
;Modifies:
;	F
;****
B$TTY_SOUT:
SCRN_SOUT:
	PUSH	AX
;
;If last char B$TTY_SOUT was called with was the 1st byte of a 2-byte char,
; B$TTY_SOUT saved it in TWOBYT so they will be both output as one 16-bit
; character.  If we are not running with FK_KANJI, then the only possible
; 2-byte characters are FFxx (for function codes and special characters).

	XOR	AH,AH
				;if not KANJI, can only be a non-zero value in
				;TWOBYT if F_EDIT is TRUE  - - - use this to
				;reduce code executed in typical case.
	TEST	b$IOFLAG,F_EDIT ; in INPUT statement?
	JNZ	SCNSO6		;brif in INPUT
SCNSO3: 			;AH guaranteed 0 if single byte char
	CMP	AL,ASCCR	; Is this a carriage return?
	JZ	CR_OUT		; BRIF so

; Fall through into SCNOUT

;***
; SCNOUT - Output character
;
;Purpose:
;	Output a character to one or more of three places:  the screen,	[13]
;	redirected stdout, and the line printer.			[13]
;	For each char processed, checks to see which of the possible	[13]
;	destinations are to be written to.  Handles chars requiring	[13]
;	mapping (???), and escape sequences.  Only updates the screen	[13]
;	if there is a char to go to the screen.  SCNOUT is recursively	[13]
;	called in order to output a destructive tab to the screen.	[13]
;
;Input:
;	[AL] == character
;	[AH] == 0 if standard character, 0xFF if [AL] == <CR>
;	IF FO_REDIRECT , RED_OUT is set if we're going to redirect stdout [13]
;	b$PTRFIL == 0 if stdout is tty
;	LPR_ECHO is set if we're going to echo to line printer [13]
;	NOTE: Expects AX to be on stack, pops it back to AX at end
;
;Output:
;	NONE
;
;Modifies:
;	F
;****
SCNOUT:
	PUSH	DX
	MOV	DL,SCN_CHR	; assume only SCN_CHAR (speed)
	TEST	b$IOFLAG,RED_OUT OR LPR_ECHO ; Printer echo or redir out?
	JZ	SAV_FLGS	; brif not -- don't do slow checks
	CALL	CHKCHAR		; set RED_CHR, SCN_CHR and LPR_CHR flags
SAV_FLGS:			
	MOV	TEMPFLAG,DL	; save flags in TEMPFLAG

	MOV	DX,b$CURSOR	; put b$CSRY in DL, b$CSRX in DH

	TEST	TEMPFLAG,RED_CHR ; Is output redirected?
	JNZ	REDOUT	       ; BRIF redirection of output
REDOUT_RETURN:		       

;	Do control char logic
; Input:
;	[AX] == control character
;	[DH] == Current screen column position
;	[DL] == Current screen row position

;  Jumps to CTLDSX if AX=valid character to output, else
;  Jumps to SCNOTC_XIT if AX is undefined, and not to be printed to screen


; Map the given character to a new character.  This is where we take
; our internal representation of a character and turn it into something
; useful.  If after mapping the character it contains a screen editor
; function implement the function at this point.
;
; Note that if we are in the screen editor (GWLIN.ASM), we have already
; done the mapping (using EDTMAP instead of PRTMAP).  We know to avoid
; the mapping because F_EDIT was set in __bIOFLAG.

	OR	AH,AH		;test if two-byte character
	JNZ	CALL_PRTMAP1	;[12] jump to map the character
	CMP	AL,31		; is this a CTL character?
	JBE	CALL_PRTMAP2	;[12] branch if so

CTLDSX: 			; end of CTLDSP

; PUTSCR - Now print the char to the screen
;	[AX] == character to put to screen
;	[DX] == prospective destination char. (cursor) position

	TEST	TEMPFLAG,SCN_CHR ; print char on screen?
	JZ	SCNOTC_XIT	; brif not

;Force CR if char in AX won't fit

	PUSH	AX		; save char
	CALL	B$CHKLASTCOL	; DH past last column of screen?
	JBE	NO_WRAP		; brif not -- no wrap required
	CALL	B$SCNCRLF	; output a CR/LF (updates DX)
NO_WRAP:			
	CALL	B$SCROUT 	; Send char in AX to BIOS at DX posn
	INC	DH		; increment cursor location
	CALL	B$UPDATE_CSR	; update cursor variables, and display
	POP	AX		; restore char

SCNOTC_XIT:

	TEST	TEMPFLAG,LPR_CHR ; Is printer echo wanted?
	JNE	SCNOTLE		; Brif want to echo
SCNOTLE_RETURN:			

	POP	DX		; end of SCNOUT
	POP	AX		;NOTE: caller must push AX ...
	RET			; end of B$TTY_SOUT



REDOUT:

;	Here if char to go to redirected file.  Map and output char to stdout.

	PUSH	BX		;save across next two calls
	CALL	REDMAP		; do redirection mapping
	CALL	PUTSTD		; Put char to stdout (if char exists)
	POP	BX
	JMP	SHORT REDOUT_RETURN    ; Jump back to main code sequence


SCNOTLE:
				;here if want to echo to line printer
	CMP	AX,0FF0DH	;test if code for CR
	JNE	SCNOTLE_NO_CR	;if not, then jump
	XOR	AH,AH		;make one-byte char for printer
SCNOTLE_NO_CR:
	CALL	[b$pLPTECHO]	;Echo to line printer if necessary
	JMP	SHORT SCNOTLE_RETURN	


CTLDP1_1:
				;here if function key display mode
	CMP	AH,1		; Set PSW.C if two byte character
	CMC
	cCALL	B$FKYMAP	; Map function key display character
	JMP	SHORT CTLEDT

CALL_PRTMAP2:
				;here if character in AL is <= 31d

	TEST	b$IOFLAG,F_KDSP ; Test for function key display mode
	JNZ	CTLDP1_1	; BRIF function key display mode

CALL_PRTMAP1:
		;here if we don't have a normal char, i.e., it must be mapped
	TEST	b$IOFLAG,F_EDIT ; Test for Screen Edit mode
	JNZ	CTLPRN		; BRIF edit mode
	CMP	AH,1		; Set PSW.C if one byte character
	CMC			; Set PSW.C if two byte character
	cCALL	B$PRTMAP 	; Map print function/output character codes
CTLEDT: JZ	SCNOTC_XIT	; Ignore this character
				; Print or perform the editor function
CTLPRN:
	CMP	AH,255		; Is it an editor function (&HFF)?
	JE	NOT_CTLDSX	;if so, then jump around
	JMP	CTLDSX		;if not, near jump
NOT_CTLDSX:			
	XOR	AH,AH		; Clear (no longer needed) editor function flag
	CMP	AL,177O		; Delete function?
	JNZ	CTLNDL		; BRIF not DEL
	MOV	AL," "		; DEL code
CTLNDL: CMP	AL,255		;++Compiler ignores 0FFh
	JZ	SCNOTC_XIT	; BRIF "mark line for deletion" - don't print
	CMP	AL," "+1 	; Test for legal function code
	JNB	CTLEND		
	TEST	TEMPFLAG,SCN_CHR; screen char?
	JZ	CTLEND		; brif not -- don't process control char

	PUSH	AX

	; get proper index into FUNTAB -- asumes AL in range [0..31]
	SUB	AL,7		; adjust so ascii [7..13] = [0..6]
	CMP	AL,6		; ascii chars [7..13]?
	JBE	TBL_ADJ		;	brif so -- table entry = [0..6]

	SUB	AL,14		; adjust so ascii [28..31] = [7..10]
	CMP	AL,7		; < ascii char 28?
	JL	CTLIGN		;	brif so -- don't process char
	CMP	AL,10		; ascii chars [28..31]?
	JG	CTLIGN		;	brif not -- don't process char

TBL_ADJ:			

	PUSH	BX
	PUSH	CX

	ADD	AX,AX		; Two bytes per entry
	XCHG	BX,AX		
	MOV	AX,OFFSET CS:CTLDPX
	PUSH	AX		; Put CTLPDX: as return address on stack
	CLC
	JMP	WORD PTR CS:FUNTAB[BX] ; Go do control routine

;All control chars come here after processing
; Update b$CURSOR, display new cursor, restore registers, return

CTLDPX:
	CALL	B$SCNLOC	; update B$CURSOR and display user cursor
	POP	CX
	POP	BX
CTLIGN:				
	POP	AX
CTLEND:	JMP	SCNOTC_XIT	; do not to print this char to screen

;--------------------------------------------------------------------------
;  Control character processing routines.
;  All routines will return to CTLDPX.


;-- END SUBROUTINE SCNOUT

	PAGE

	SUBTTL	Control Character processing
;CONTROL CHARACTER DISPATCH TABLE
;--
;Interpreter supports more control characters than compiler.
;		Dispatch address	Action (compiler ignores some)	[13]
FUNTAB:				
	DW	OFFSET B$BLEEP	; ^G  -  Beep
	DW	0		; unused position
	DW	OFFSET LTAB	; ^I  -  Destructive tab
	DW	OFFSET B$SCNCRLF ; ^J - Linefeed outputs CR/LF.
	DW	OFFSET WHOME	; ^K  -  Home within window
	DW	OFFSET CCLRSN	; ^L  -  Clear window, home cursor
	DW	OFFSET B$SCNCRLF ; ^M - carriage return


	DW	OFFSET WCSADV	; ^\  -  Cursor advance within window
	DW	OFFSET WCSREG	; ^]  -  Cursor regress within window
	DW	OFFSET WCSUP	; ^^  -  Cursor up within window
	DW	OFFSET WCSDWN	; ^_  -  Cursor down within window

;***
;LPTECHO
;Purpose:
;	Echo to the line printer.
;	Added with revision [32].
;Entry:
;	[AX] = character code
;Exit:
;	none
;Modifies:
;	None ([AX-DX] is preserved)
;****
cProc	LPTECHO,<NEAR>,<AX,BX,CX,DX>	; preserve the world
cBegin
EchoAnother:			
	PUSH	AX		;store byte in stack
	MOV	DX,SP		;[DS:DX] points to data to be output
	.ERRE	ID_SSEQDS	;assumes DS=SS
	MOV	BX,0004H	;file handle for stdprn
        MOV     CX,1            ;[CX] = # of bytes to be written
	MOV	AH,40H		;write operation
	INT	21H		;do the write
	POP	AX		;even stack and restore AX
	JNC	LPTECHOExit	;jump if no error on write
	JMP	B$ERR_IOE	;give generic Device I/O Error
LPTECHOExit:
	CMP	AL,ASCCR	; just printed a CR?
	MOV	AL,ASCLF	; Assume so -- then we want a LF, too
	JE	EchoAnother	; brif CR -- add a Line feed
cEnd

;***
; PUTSTD - Put character to redirected standard output.
;
; Purpose:
;	Writes a char to redirected stdout.  If given character is a <cr>, [13]
;	put out an <lf> also, as stdout needs one.  Adjust the redirected  [13]
;	file cursor position (used to do line wrapping) appropriately.	   [13]
;
; Entry:	[BX] = character (or zero)
; Exit:  none
; Modifies:
;	F, AX
;****
PUTSTD:
	OR	BX,BX		; Test to see if character exists
	JZ	PUTSTDX 	; BRIF not

	MOV	AX,BX		; place char in AX as well

	CMP	AX,0AH		; about to output an <lf>?
	je	PUT_CR_FIRST	; brif <lf> to put a <cr> first
PUTSTD2:
	CLC			;PWS.C reset indicates a 1 byte character
	CALL	B$STDPUT 	; Write the character
	MOV	AX,BX		; restore character into AX
	CMP	AL,9		; ASCII code < 9 (HT)?
	JB	INC_CSR 	; brif if so -- increment cursor.
	JE	TAB_CHAR	; brif HT
	CMP	AL,0DH		; <lf>,<vt>,<ff>,<cr> ?
	JE	PUT_LF_TOO	; brif <cr> to put an <lf> too
	JBE	RESET_CSR 	; brif <LF>,<VT>, or <FF>
INC_CSR:
	INC	b$REDCSRX	; increment redirected file cursor
PUTSTDX:			
	RET

PUT_CR_FIRST:			;here if we're about to put out an lf
	MOV	AX,0DH		; output a <cr> first
	CLC			;Writing a single byte
	CALL	B$STDPUT	; write it out
	MOV	AX,0AH		; restore <lf>
	JMP	SHORT PUTSTD2

PUT_LF_TOO:			;here if we've just sent a <cr> to stdout and
				;need to put in an <lf> now
	MOV	AX,0Ah		;put <lf> in AX
	CLC			;Writing a single byte
	CALL	B$STDPUT 	; write it out, and reset cursor position

RESET_CSR:			
	XOR	AX,AX		; reset redirected file cursor to 1 for
	JMP	SHORT TAB_DONE	; <LF>,<VT>,<FF>,<CR>

TAB_CHAR:			; adjust cursor when TAB printed
	MOV	AL,b$REDCSRX	; load redirected file cursor (1-relative)
	ADD	AL,8		; move to next modulo-8 position.
	AND	AL,0F8H		
TAB_DONE:			
	INC	AX		; make it 1-relative
	MOV	b$REDCSRX,AL	; store redirected file cursor
	RET			; and return from PUTSTD



;*** 
; B$CHKLASTCOL -- check for last column on screen.  Added with revision [19]
;
;Purpose:
;	Sets flags according to whether DH is [<,=,>] the last printable
;	screen location.
;
;Entry:
;	DH = 1-relative screen column
;Exit:
;	Flags set as if CMP DH,LAST_POS was done.
;Uses:
;	None
;Preserves:
;	All
;Exceptions:
;	None
;
;******************************************************************************
cProc	B$CHKLASTCOL,<PUBLIC,NEAR>
cBegin
	CMP	b$PTRFIL,0	; Test if TTY (=0) or SCRN: (>0)
	JE	CHK_PHYS	; If TTY then check physical width
	CMP	DH,b$SCRNWIDTH	; Test if over logical (SCRN:) width
	JAE	CHK_EXIT	; Brif if greater than or equal
CHK_PHYS:
	CMP	DH,b$CRTWIDTH	; Check for last physical column
CHK_EXIT:
cEnd


;*** 
; B$UPDATE_CSR -- Update cursor after a screen write.
;			 Added with revision [19]
;
;Purpose:
;	Updates the high-level cursor position, and displays the user
;	cursor at the appropriate place.  If we have just printed into
;	the last column of the screen, the cursor is NOT displayed at
;	the first position of the next line.  Instead, it is backed up
;	on top of the character just written.
;
;Entry:
;	DX = new cursor position
;Exit:
;	b$CURSOR updated
;
;	DX = position at which cursor was displayed
;Uses:
;	None
;Preserves:
;	All
;Exceptions:
;	None
;
;******************************************************************************
cProc	B$UPDATE_CSR,<PUBLIC,NEAR>
cBegin
	MOV	b$CURSOR,DX	; update logical cursor b$CURSOR
	CALL	B$CHKLASTCOL	; past last column?
	JBE	NO_DEC		; brif not
	DEC	DH		; back up one position to display cursor
NO_DEC:
	JMP	B$USRCSR	; Redisplay user cursor and return
cEnd	<nogen>


;***
; CHKCHAR -- Sets fields of DL to facilitate complicated checks.  Added with
;		revision [13].
;
; Purpose:
;    Decide whether a character should be printed on the screen, to the
;    redirected file, and/or to the printer, according to the following chart:
;
;------------------------------------------------------------------------
;		|	|	|	|        INPUT statement	|
; Redirection	| FKEY	| SCRN:	| CONS:	|   with edits	| without edits	|
;---------------|-------|-------|-------|---------------|---------------|
; No redirection|   S	|  S,P	|  S,P	|	S	|      S,P	|
; Red OUT	|  ---	|  S,P	|  F	|	S	|      F,P	|
; Red INP	|   S	|  S,P	|  S,P	|	S	|      S,P	|
; Red INP & OUT	|  ---	|  S,P	|  F	|      ---	|      F	|
;------------------------------------------------------------------------
;
;	S = print char to screen
;	P = print char to printer
;	F = print char to redirected output
;
; Function key display when F_KDSP.
; Printing to SCRN: when b$PTRFIL <> 0.
; INPUT statement with user edits when IN_INPUT.
; INPUT statement without user edits when
;	(F_EDIT and NOT IN_INPUT).
; Printing to CONS: at all other times.
;
;
; Algorithm summary:
;
;   Char should be printed to redirected file (RED_CHR) if:
;	RED_OUT and NOT (SCRN: or IN_INPUT or F_KDSP)
;
;   Character should be printed to the screen (SCN_CHR) if:
;	SCRN: or NOT RED_OUT or (NOT RED_INP and IN_INPUT)
;
;   Character should be echoed to the printer (LPR_CHR) if:
;	LPR_ECHO and NOT F_KDSP and NOT IN_INPUT and
;	(SCRN: or NOT RED_OUT or
;	(RED_OUT and NOT IN_INPUT and F_EDIT))
; Input:
;	b$PTRFIL set correctly.
; Exit:	
;	SCN_CHR, RED_CHR, and LPR_CHR fields of DL set correctly.
; Modifies:
;	DX, F
;****
cProc	CHKCHAR,<NEAR>,<AX>
cBegin

	XOR	DX,DX			; initially all flags are false 
	MOV	AL,b$IOFLAG		; keep b$IOFLAG in AL

	TEST	AL,RED_OUT		; is output redirected?
	JZ	SCN_CHAR		;   brif not -- screen char
	CMP	b$PTRFIL,DX		; Test if TTY (=0) or SCRN: (>0)
	JNZ	SCN_CHAR		;   brif SCRN: -- screen char
	TEST	AL,F_KDSP OR IN_INPUT	; function key display or
					; INPUT statement?
	JNZ	CHK_SCN			;	brif so -- not a redir char
	OR	DL,RED_CHR		; set bit to indicate redir char
	JMP	SHORT CHK_LPR		; skip check for screen char

CHK_SCN:				; RED_OUT = TRUE here
	TEST	AL,RED_INP		; redirected input?
	JNZ	CHK_LPR			; 	brif so -- not screen char
	TEST	AL,IN_INPUT		; INPUT statement?
	JZ	CHK_LPR			; 	brif not -- not screen char
SCN_CHAR:
	OR	DL,SCN_CHR		; set bit to indicate screen char

CHK_LPR:
	TEST	AL,LPR_ECHO		; printer echo wanted?
	JZ	CHKCHAR_EXIT		;	brif not -- not a printer char
	TEST	AL,F_KDSP OR IN_INPUT	; function key display or IN_INPUT?
	JNZ	CHKCHAR_EXIT		;	brif so -- not a printer char
	CMP	b$PTRFIL,0		; Test if TTY (=0) or SCRN: (>0)
	JNZ	LPR_CHAR		;	brif SCRN: -- printer char
	TEST	AL,RED_OUT		; redirected output?
	JZ	LPR_CHAR		;	brif not -- printer char

	AND	AL,RED_INP OR F_EDIT	; redisplaying line and not
	CMP	AL,F_EDIT		; redirected input ?
	JNZ	CHKCHAR_EXIT		; 	brif not -- not a printer char
LPR_CHAR:
	OR	DL,LPR_CHR		; set bit to indicate printer char

CHKCHAR_EXIT:
cEnd


;***
; REDMAP - Map character for redirected std. output file
;
; Purpose:
;	Given a character to be printed to redirected stdout, return
;	the mapped character in BX or 0 character should be ignored.
; Input:
;	[AX] = unmapped control character for screen
; Output:
;	[BX] = mapped char for std output (0 if it should be ignored)
;		may be a 1- or a 2-byte character
; Modifies:
;	F
;****

REDMAP: PUSH	AX
	XOR	BX,BX		;Assume no character for standard output
	CMP	AH,1		;Set PSW.C for two byte character
	CMC
	cCALL	B$PRTMAP 	; Map as if a print statement
	JZ	REDMAX		;No character to print
	MOV	BX,AX		;Assume character will be used as mapped
	JNB	REDMAX		;Print character as mapped
	CMP	AH,255D		;Test for control character
	JNZ	REDMAX		;Must be KANJI or other FK_KANJI
	XOR	BH,BH		;Map to single byte char
REDMAX:
	POP	AX
	RET

	PAGE
	SUBTTL	CONTROL CHARACTER ROUTINES - Beep, form feed, home, backspace, tab.

;***
; B$BEEP - BEEP stmt
;
; Purpose:
;	Runtime Entry Point.
;	beep the speaker
; Input:
;	NONE
; Output:
;	NONE
; Modifies:
;	NONE
;****

cProc	B$BEEP,<PUBLIC,FAR>	
cBegin				
	cCALL	B$BLEEP	
cEnd				

;Clear the screen
CCLRSN:
	MOV	AL,2		; specify text only
	CALL	B$CLRSCN 	; OEM supplied Clear-Screen Routine
	CALL	[b$vKEYDSP]	; clear or output status line
				; fall into WHOME

;***
; WHOME - Home the text cursor [36]
; Public equivalent B$WHOME moved into gwini.asm with revision [36].
; Note:  These 2 routines (WHONE & B$WHOME) must be kept in sync!
;
; Input:
;	b$WDOTOP set
; Output:
;	[DL] == home row of cursor
;	[DH] == home column of cursor
; Modifies:
;	NONE
;****
WHOME:				; moved public version to gwini.asm
	MOV	DL,b$WDOTOP	
	MOV	DH,1		
SHORT_RET:			; label to jump to for a RET
	RET



;	LTAB - Destructive TAB to modulo 8 column
LTAB:
	MOV	AX,OFFSET CODE:LTAB_RET ;push the return address first
	PUSH	AX		;and then the letter value...
	MOV	AX,0020h	;Blank out chars
	PUSH	AX		;NOTE: SCNOUT pops AX, but doesn't push it -
				;	this is for B$TTY_SOUT speed
	JMP	SCNOUT		;jump with stack set up
LTAB_RET:
	MOV	DX,b$CURSOR	; put b$CSRY in DL, b$CSRX in DH
	DEC	DH		;TAB stops at 8th column, 16th, ...
	TEST	DH,7
	JNZ	LTAB		;More spacing to next column

	INC	DH
	RET			;New position set on exit.

	PAGE
	SUBTTL	CONTROL CHARACTER ROUTINES

;*** 
; B$SCNCRLF -- Output a CR/LF to the screen.  Re-written with [19].
;
;Purpose:
;	This code is used for either <CR> or <LF>.
;	This routine may find that erasing function key line,scrolling
;	whole screen and redisplaying function key line, is faster than
;	scrolling 1-24.
;
;Entry:
;	DX = present cursor position
;
;Exit:
;	DX = new cursor position
;	Screen possibly scrolled
;Uses:
;	None
;Preserves:
;	AX,BX,CX
;Exceptions:
;	None
;
;******************************************************************************

cProc	B$SCNCRLF,<NEAR>
cBegin
	CALL	B$CRLF		; adjust DX if necessary
	JNZ	SHORT_RET	; if no need to scroll, just return
	JMP	B$SCROLL	; scroll screen and return
cEnd	<nogen>

	PAGE
	SUBTTL	CONTROL CHARACTER ROUTINES - Function key display and advance

;B$LABELK - If function key display is off then turn on and exit.
;	  If function key display is on then advance display line and
;	  redisplay.
;ENTRY  - DX = cursor position
;EXIT	- None [13]
;USES	- None [13]



	PUBLIC	B$LABELK 	;Can be called by $RDLIN directly.
B$LABELK:			

	NOT	b$KEY_SW	; toggle function key display switch
	JMP	[b$vKEYDSP]	; Display function keys

	PAGE
	SUBTTL	CONTROL CHARACTER ROUTINES - Cursor movement by character and line
;	The control character routines are entered with DH=current column and
;	DL=current line. They return updated posn in the same registers. They
;	can modify AX, BX, CX but must save any other registers used.


;***
; WCSADV, WCSREG, WCSUP, WCSDWN - cursor control
;
; Purpose:
;	CURSOR CONTROL
;	The cursor control routines allow moving the cursor right (and left)
;	with wraparound to following (or previous) line unless it is at the
;	very bottom (or top) of the screen. Attempts to move the cursor up
;	from the top line of the screen or to move the cursor down from the
;	bottom line on the screen are ignored.
;
; Inputs:
;	[DL] == current line
;	[DH] == current column
; Outputs:
;	updated line position in DL
;	updated column position in DH
;
;	These routines return CF=1 when they do not change the posn.
; Modify:
;	AL
;****

;Cursor right within window
WCSADV:
	INC	DH		; Next column
	CALL	B$CHKLASTCOL	; past last valid column?
	JBE	CSRRET		; BRIF Good value
CSRADV_DOWN:			
	DEC	DH
	CALL	WCSDWN
	JB	CSRRET		; BRIF can't change physical lines
	MOV	DH,1		;First column
CSRRET:
	RET			; update position and redisplay cursor

;Cursor left within window(or within physical line if outside window)
WCSREG:
	DEC	DH		; Previous column
	JNZ	CSRRET		; BRIF good value (1 or greater)
	INC	DH		; Restore column
	CALL	WCSUP		
	JB	CSRRET		; BRIF can't change physical lines
				; fall into B$GETLASTCOL

B$GETLASTCOL:			; DH = last column (anybody need this FN?)
	MOV	DH,BYTE PTR b$CRTWIDTH ; Last column of physical screen
	CMP	b$PTRFIL,0	; Test if TTY (=0) or SCRN: (>0)
	JE	CSRRET		; If TTY, then jump to redisplay cursor
	CMP	b$SCRNWIDTH,255 ; Check for infinite width
	JE	CSRRET		; If so, then use b$CRTWIDTH, redisplay
	MOV	DH,b$SCRNWIDTH	; Else use logical width
	RET			; return and redisplay cursor

;Cursor up within window(or NOP if outside window)
WCSUP:
	CMP	b$WDOBOT,DL	
	JB	CSRRET		; BRIF outside of window
	CMP	DL,b$WDOTOP	
	JB	CSRRET
	STC
	JZ	CSRRET		; BRIF at top or outside of window
	CLC
	DEC	DX		
	RET			; return and redisplay cursor

;Cursor down within window(or NOP if outside window)
WCSDWN:
	CMP	DL,b$WDOTOP	
	JB	CSRRET		; BRIF outside window
	CMP	b$WDOBOT,DL	
	JB	CSRRET		; BRIF at bottom or outside of window
	STC
	JZ	CSRRET
	CLC
INC_DX:				
	INC	DX		; Next line
	RET			; return and redisplay cursor


;***
; B$TTY_GPOS, B$SCRN_GPOS - return screen position
;
; Input:
;	b$CSRX contains current cursor position on screen
;	or b$REDCSRX contains the current redirected file position [13]
; Output:
;	[AH] == screen position
; Modifies:
;	F
;****
labelNP	<PUBLIC,B$TTY_GPOS>	
	TEST	b$IOFLAG,RED_OUT ; redirected output?
	MOV	AH,b$REDCSRX	; return redirected file position
	JNZ	GPOS_RET	; brif so -- return redir file cursor

labelNP	<PUBLIC,B$SCRN_GPOS>	
SCRN_GPOS:
	MOV	AH,b$CSRX	; return screen cursor position
GPOS_RET:
	DEC	AH		; return 0-relative value
	RET


;***
; B$TTY_GWID, SCRN_GWID - return screen width
;
; Input:
;	b$CRTWIDTH contains current tty width
;	b$SCRNWIDTH contains current SCRN: width
; Output:
;	[AH] == screen width
; Modifies:
;	NONE
;****

B$TTY_GWID:
	MOV	AH,b$CRTWIDTH	; (AH) = physical screen width
	RET

SCRN_GWID:
	MOV	AH,b$SCRNWIDTH	; (AH) = logical screen width
	RET

	PAGE

;***
; B$TTY_BLKIN	- block input
;	NOT IMPLENTED					--DMA support for BLOAD
;	Entry	[bx] =	offset of destination
;		[cx] =	maximum number of bytes to read
;		[dx] =	DS of destination
;		[di] -> device number
;		[si] -> FDB of file to be loaded
;	exit	?      (This routine is provided to allow the user
;			to trash himself beyond all recognition!
;	Exit	[bx] =	1 past last byte read
;		CF	set if EOF encountered
;****

B$TTY_BLKIN:			;fall into B$ERR_FC jump


;***
; B$TTY_BLKOUT	-  block output
;	NOT IMPLEMENTED 				--DMA support for BSAVE
;	Entry	[bx] =	offset of source
;		[cx] =	maximum number of bytes to write
;		[dx] =	DS of source
;		[di] =	device number
;		[si] -> FDB of file to be loaded
;	Exit	[bx] =	1 past last byte written
;		CF	set if insufficient space
;****

B$TTY_BLKOUT:
width_error:  jmp B$ERR_FC	

devio	ENDP

;
; Moved here from GWINI with [41]
;
;***
;B$SCNSWD - set screen width (called by WIDTH"scrn:",n)
;
;Purpose:
;	Sets the width of the SCRN: device to the specified amount.
;
;Entry:
;	DL = new width
;
;Exit
;	b$SCRNWIDTH (logical width) set
;	physical screen width possibly changed (if 80 or 40 columns specified)
;
;Uses:
;	Per convention
;****

cProc	B$SCNSWD,<NEAR>
cBegin
	XCHG	AX,DX		;keep width in AL
	DEC	AX		;map [1-255] to [0-254]
	CMP	AX,254		;in range [0-254]?
	JA	WIDTH_ERROR	;brif not -- illegal function call error
	INC	AX		;restore width from entry

	CMP	AL,255		;infinite width?
	JZ	SCNWD2		;Brif so
SCNWD1:
	CMP	AL,b$CRTWIDTH	;new width different from physical?
	JE	SCNWD2		;brif not
	PUSH	AX		;save logical width
	MOV	CL,B$LINCNT	;pass Height in CL
	cCALL	B$SWIDTH	;Let OEM call B$SCNSWI if legal
				;if OEM didn't like it, b$CRTWIDTH unchanged
	POP	AX		;restore logical width
	CMP	AL,b$CRTWIDTH	;Test if logical < physical width
	JB	SCNWD2		;If so, then use given width
	MOV	AL,b$CRTWIDTH	;Get physical width
SCNWD2:
	MOV	b$SCRNWIDTH,AL	;set the SCRN: logical screen width

cEnd

;
;Merged in B$SCNWID from GWINI.ASM
;
;***
;B$WIDT - WIDTH wid,hgt statement
;
;Purpose:
;	Runtime Entry Point.
;	Set the WIDTH for the screen
;
;Input:
;	wid - requested width (-1 if default)
;	height - requested height (-1 if default)
;
;Output:
;	None
;
;Uses:
;	Per Convention
;
;Exceptions:
;	NONE
;****
cProc	B$WIDT,<PUBLIC,FAR>	
parmW	wid			
parmW	height			
cBegin				
	MOV	BX,wid		;get width
	MOV	DX,height	;get height
	CMP	DX,-1		;Is height specified?
	JNE	H_Specified	; Yes:
	MOV	DL,B$LINCNT	; No: set to full screen
	XOR	DH,DH
H_Specified:

	OR	DH,DH		;Height < 256?
	JNZ	width_error
	CMP	BX,-1		; Is Width specified?
	JNE	W_Specified	; Yes:
	MOV	BL,b$CRTWIDTH	; No: set to full screen
	XOR	BH,BH		; Make Hi-Byte zero
W_specified:			
	OR	BH,BH		;Width < 256?
	JNZ	width_error

	CMP	BL,b$CRTWIDTH	; Test if physical width changed
	JNE	SCNWID0 	;Yes:
	CMP	DL,B$LINCNT	; Has height changed?
	JE	SCNRET		; wrong jump sense
				;Yes: call B$SWIDTH to set $CRTWID if legal.
SCNWID0:			;if either changed, go set hardware.

	PUSH	CX		;If width has changed, set hardware
	PUSH	AX
	XCHG	AX,BX		; pass Width in AL
	XCHG	CX,DX		; pass Height in CL
	CALL	B$SWIDTH	;Let OEM call B$SCNSWI if legal
	JB	WIDTH_ERROR	;OEM didn't like it.
	POP	AX
	POP	CX

SCNRET:
cEnd				


;***
; B$IfScrnDev - Check if current device printing to screen.
;
; Purpose:
;	Used by QB4 to determine if they have to do debug screen swapping.
;	Added with [20].
;
;	Returns true if the current I/O device is TTY, SCRN:, CONS:,
;	or a console I/O DOS file.
;
;	Note:  This routine MUST preserve ES and CANNOT cause heap movement.
; Entry:
;	b$PTRFIL set correctly.
; Exit:
;	AX	=  0 if no screen output possible
;	AX	= -1 if screen output possible
; Modifies:
;	BX,CX
; Preserves:
;	DX,ES
; Exceptions:
;	None
;****

cProc	B$IfScrnDev,<PUBLIC,FAR>
cBegin

	XOR	AX,AX		; assume no screen output
	MOV	BX,b$PTRFIL	; pointer to current file/device
	OR	BX,BX		; TTY?
	JZ	ScrnDevice	; brif so -- screen device
	MOV	CL,[BX].FD_DEVICE ; AX = device number
	XOR	CH,CH		; clear high byte
	JCXZ	DiskFile	; brif disk file
	CMP	CL,DN_SCRN	; SCRN:?
	JE	ScrnDevice	; brif so -- screen device
	CMP	CL,DN_CONS	; CONS:?
	JE	ScrnDevice	; brif so -- screen device
	JMP	SHORT NoScrnDevice ; otherwise, not a screen device
DiskFile:
	TEST	[BX].FD_FLAGS,FL_CONOUT+FL_CONINP ; console I/O?
	JZ	NoScrnDevice	; brif not -- not a screen device

ScrnDevice:
	DEC	AX		; AX = -1 ==> screen output
NoScrnDevice:
cEnd




sEnd	DV_TEXT 		
	END
