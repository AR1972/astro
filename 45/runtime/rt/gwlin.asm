	TITLE	GWLIN - read and edit line from keyboard
;***
; GWLIN - read and edit line from keyboard
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
;
;	* entry point: B$RDLIN
;
;	* returns string in b$Buf1
;	  * byte vector up to length of 255
;	  * zero-byte terminated
;
;	* both ASCII and KANJI versions on switch FK_KANJI
;
;-------------------------------------------------------------------
;
;	Data structures used:
;
;	* b$INBUF - byte vector of length 256
;	  * contains ASCII/KANJI values of present state
;	    of string entered
;	  * end-of-line (EOL) indication is 0 value
;
;	* XBLTAB - word vector of length 8 (KANJI only)
;	  * entries, if any, defined up to index XBLNXT
;	  * entry contains b$INBUF location of KANJI character
;	    where an extra blank had to be inserted on the
;	    display screen to prevent splitting the KANJI
;	    character across rows.
;
;------------------------------------------------------------------
;
;	Editting functions supported:
;
;	* ^B  - move cursor back one word
;	* ^C  - exit program
;	* ^E  - truncate line at cursor
;	* ^F -	move cursor forward one word
;	* ^H  - delete character left of cursor
;	* ^I  - insert/overwrite to next tab field of length 8
;	* ^K  - move cursor to beginning of line
;	* ^M  - (carriage return) return string to program
;	* ^N  - move cursor to end of line
;	* ^R  - toggle between insertion and overwriting
;	* ^T  - toggle function key label display
;	* ^U  - erase line
;	* ^\  - (left cursor) move cursor left once
;	* ^]  - (right cursor) move cursor right once
;	* DEL - delete character over cursor
;==================================================================
				;      ASCII character definitions

	CTL_B=002D		;^B - control B
	CTL_C=003D		;^C - control C
	CTL_E=005D		;^E - control E
	CTL_F=006D		;^F - control F
	CTL_G=007D		;^G - control G (BELL)
	CTL_H=008D		;^H - control H (backspace)
	CTL_I=009D		;^I - control I (TAB)
	CTL_J=010D		;^J - control J (line feed)
	CTL_K=011D		;^K - control K
	CTL_M=013D		;^M - control M (carriage return)
	CTL_N=014D		;^N - control N
	CTL_R=018D		;^R - control R (insert map)
	CTL_T=020D		;^T - control T
	CTL_U=021D		;^U - control U (ESC map)
	CTL_BS=028D		;^\ - control \ (right arrow map)
	CTL_RB=029D		;^] - control ] (left arrow map)

	ASC_SP=032D		;ASCII space
	ASC_DL=127D		;ASCII DEL (delete map)

; Standard MS Keyboard codes (have FF as high byte)

	CNTL=0FFh		; Must be sent in front of control chars

;	other definitions

	.XLIST
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	

	USESEG	_DATA		
	USESEG	CONST		
	USESEG	_BSS		
	USESEG	CN_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE dc.inc
	INCLUDE messages.inc	; MS message
	INCLUDE const.inc	
	.LIST

	BUFLEN=255D		;length of b$INBUF

	.SALL			;suppress macro expansion

sBegin	_DATA			

	externW	b$CRTWIDTH	; width of display screen
	externW	b$CURSOR	; (1,1)-relative screen cursor
	externB	b$IOFLAG	; Misc. IO flags.  Defined in GWINI.ASM

sEnd	_DATA			

sBegin	CONST			

	globalB	b$EightSpaces,' ',8 ; string of eight spaces (for TAB)

sEnd	CONST			

sBegin	_BSS			

	externB b$Buf1		; defined in GWINI.ASM
	b$INBUF EQU b$Buf1	; (use b$Buf1 and b$Buf2)

	staticW CSRPTR,,1	;b$INBUF index to input cursor
	staticW EOLPTR,,1	;b$INBUF index to input end-of-line
	staticW UPDPTR,,1	;b$INBUF index to start of update
	staticW CSRLOC,,1	;display column of cursor
	staticW EOLLOC,,1	;display column of end-of-line
	staticB INSFLG,,1	;0=overwrite mode, 0FFH=insert mode
	staticB ENDFLG,,1	;0=process another char, 0FFH=done
	INPLEN=16D		;length of input string for insert
	externB b$Buf3		; defined in GWINI.ASM
INPSTR	EQU	b$Buf3		; string of bytes for character insert


sEnd	_BSS			

sBegin	CN_TEXT 		
	assumes CS,CN_TEXT	

	externNP B$TTYGetChar	; Wait for keyboard character, checking for
				; ^BREAK
	externNP B$TTYST	;determines if character input pending
	externNP B$BREAK	
	externNP B$TTY_SOUT	;output character in AL to screen
	externNP B$STDGET	; input char from keyboard or redir input
	externNP B$ERR_RPE	; INPUT PAST END error
	externNP B$BREAK_CHK	;check for break character entered
	externNP B$SCRN_GPOS	; get screen cursor position
	externNP B$SCNLOC	; update cursor position and variables
	externNP B$EDTMAP	;OEM mapping for keyboard characters
	externNP B$LABELK	;toggle function label display
	externNP B$OFFCSR	; off cursor
	externNP B$INSCSR	; insert mode cursor
	externNP B$OVWCSR	; overwrite mode cursor
	externNP B$USRCSR	; user cursor
	externNP B$BLEEP	; sound bell on display

;	TABLE macro to define entries in CTLTAB

TABLE	MACRO	FCODE,FADDR
	DB	FCODE
	DW	FADDR
	ENDM	TABLE

;	CTLTAB definition for control character processing

CTLTAB:
	TABLE	CTL_B,BAKWRD	;move cursor backward one word
	TABLE	CTL_C,BREAK	;exit from BASCOM program
	TABLE	CTL_E,TRUNC	;truncate input line at cursor
	TABLE	CTL_F,FORWRD	;move cursor forward one word
	TABLE	CTL_J,IGNORE	; process LF (follows CR in redirected input)
	TABLE	CTL_K,BEGIN	;move cursor to line beginning
	TABLE	CTL_M,ENDLIN	;process CR to end line input
	TABLE	CTL_N,APPEND	;move cursor to end of input line
	TABLE	CTL_U,ERASE	;erase entire input line
	TABLE	CTL_BS,CSRRGT	;move cursor once to the right
	TABLE	CTL_RB,CSRLFT	;move cursor once to the left

;	the above functions set INSFLG=0

CTLINS:
	TABLE	CTL_H,DELLFT	;delete character left of cursor
	TABLE	CTL_I,TABCHR	;process TAB printing character
	TABLE	CTL_R,TOGINS	;toggle insert/overwrite mode
	TABLE	CTL_T,B$LABELK	;toggle key label display
	TABLE	ASC_DL,DELCSR	;delete character on cursor
CTLEND: 			;end of CTLTAB


	page
;*** 
; B$RDLIN -- Read a line of input.
;
;Purpose:
;
;Entry:
;	Caller should hold (and subsequently release) b$Buf1 & b$Buf2
;Exit:
;	b$Buf1 (+ b$Buf2) contains the data input.
;Uses:
;	None
;Preserves:
;	All
;Exceptions:
;
;******************************************************************************

cProc	B$RDLIN,<NEAR,PUBLIC>,<AX,BX,CX,DX,ES>	
cBegin				


;	initialization

	PUSH	DS		; set ES=DS
	POP	ES		

	XOR	BX,BX		;clear BX for use as zero

	MOV	CSRPTR,BX	;cursor at start of input line
	MOV	EOLPTR,BX	;input line starts out null

	MOV	INSFLG,BL	;start with overwrite mode
	MOV	ENDFLG,BL	;input line is starting, not ending

	MOV	b$INBUF,BL	;clear EOL position of b$INBUF

	OR	b$IOFLAG,(F_EDIT OR IN_INPUT) ; Disable $PRTMAP
				; and tell B$TTY_SOUT not to echo to stdout
				; until final dump of edited buffer.

	CALL	FRCLIN		;force new line if cursor stuck
	CALL	OFFCSR		;turn off cursor

;********************************************************************
;	start of main program loop
;********************************************************************

;	get next keyboard character
;	  if PSW.Z=1, then ignore the character
;	  if PSW.C=0, then character is ASCII in AL

NXTCHR:

	; Update the state of the BIOS Insert bit to correspond to
	; the current state of our Insert flag.  This allows programs
	; that depend on this bit (i.e. screen readers for the blind)
	; to work properly.

	push	es
	push	ax
	push	bx

	xor	bx, bx
	mov	es, bx		;prepare ES for accessing BIOS
	cli			;No interrupts while munging Keyboard flags
	mov	al, es:[417h]	;get current shift states
	and	al, 7fh 	;assume insert mode off
	cmp	INSFLG, bl	;is insert mode off
	je	@F		;brif so, assumption correct
	or	al, 80h 	;otherwise, set insert mode on
@@:
	mov	es:[417h], al	;tell the BIOS of the change
	sti			;allow interrupts again.

	pop	bx		;Restore previous registers
	pop	ax
	pop	es


	CALL	FRCSTR		;force output if no char pending
	CALL	GETCHR		;get the next character
	JNZ	NXTCH1		;jump if input is legal
	CALL	BADCHR		;process illegal character
	JMP	SHORT NXTCHR	;and try for the next character

;	if one-byte character, process as ASCII printing code

NXTCH1:
	JC	NXTCH2		;jump if two-byte character
NXTCH1A:
	CALL	ASCCHR		;process as ASCII if carry clear
	JMP	SHORT NXTCHR	;and process the next character

;	two-byte character - test for function key code

NXTCH2:
	CMP	AH,080H 	;test if function key code
	JNE	NXTCH3		;if not, then jump
	CMP	AL,080H 	;test for 8080H -> 80H byte
	JE	NXTCH1A 	;if so, then just print it
	CALL	FKYCHR		;process function key
	JMP	SHORT NXTCHR	;and process the next character

;	test for control character code

NXTCH3:
	CMP	AH,0FFH 	;test if control character
	JNE	NXTCHR		; brif not -- process next char
	CMP	AL,010H 	;test for FF10H -> FEH
	JNE	NXTCH3A 	;if not, then jump
	MOV	AL,0FEH 	;force in mapped character
	JMP	SHORT NXTCH1A	;jump to print character
NXTCH3A:
	CMP	AL,0FFH 	;test for FFFFH -> FFH
	JE	NXTCH1A 	;if so, then print character
	CALL	CTLCHR		;process control character in AL

;	test if input line is finished - if so, leave module

	TEST	ENDFLG,0FFH	;test if input line has ended
	JNZ	RDEXIT		;if so, jump to exit the module
	JMP	SHORT NXTCHR	;jump to process the next char


RDEXIT:
	CALL	USRCSR		;turn on user cursor


cEnd				; restore registers and return to caller

	page


;********************************************************************
;	GETCHR - get keyboard character - remove 3-byte codes
;********************************************************************
; Note that the interpreter does character mapping on ouput only.
; Either B$EDTMAP and $PRTMAP is called when a character is printed
; on the screen, depending on whether in edit mode or not.
; The compiler, not having a full screen editor, only needs B$EDTMAP
; during the INPUT statement. So edit character mapping is done on the
; keyboard input, and print mapping is done on screen output. Since
; characters echoed from here have already been mapped, they must not
; be mapped in B$TTY_SOUT also, so F_EDIT [13] is set to nonzero on entry
; to B$RDLIN and cleared on the only three exits through ENDLIN, BREAK,
; and end of redirected IO.


GETCHR:
;   PUSH    DX		    ;save register on the stack

	CALL	ONCSR		;turn on cursor for input
	test	b$IOFLAG,RED_INP ; input redirected?
	jz	noredir
	call	B$STDGET 	; read char from standard input
	jnz	redir

; End of redirected input seen, flush input buffer and exit
	AND	b$IOFLAG,NOT (F_EDIT OR IN_INPUT) ; Re-enable $PRTMAP
				; Exiting INPUT statement
	CALL	APPEND		; move cursor to end of line
	JMP	B$ERR_RPE	; jump to "INPUT PAST END" error

noredir:

	MOV	DL,1		;Check for events while waiting
	CALL	B$TTYGetChar	;get character from keyboard

redir:
	cCALL	B$EDTMAP 	;map to OEM specifications
	PUSHF			;save flags on stack
	CALL	OFFCSR		;turn off cursor

;	if 3-byte code (AL=254D), clear AX to ignore character

	CMP	AL,254D 	;is this a 3-byte code?
	JNE	GETCH1		;if not, then branch
	POPF			;restore flags (and stack)
	XOR	AX,AX		;clear AX to be ignored

;   POP     DX		    ;restore register from stack
	RET			;and return to caller

;	not 3-byte code, just return AX to caller

GETCH1:
	POPF			;restore flags
;   POP     DX		    ;restore register from stack
	RET			;and return to caller

;********************************************************************
;	EDTSTR - edit string in INPSTR into input buffer
;********************************************************************

EDTSTR:
	PUSH	CX		;save registers on stack
	PUSH	SI

	MOV	CX,BX		;get length of INPSTR in bytes
	JCXZ	EDTST1		;if null, then just exit
	MOV	SI,OFFSET DGROUP:INPSTR ;get offset of string
	CALL	PRTCHR		;insert/overwrite string in b$INBUF
	XOR	BX,BX		;done - reset INPSTR pointer
EDTST1:
	POP	SI		;restore registers from stack
	POP	CX
	RET			;return to caller

;********************************************************************
;	ONCSR - turn on appropriate cursor
;********************************************************************

ONCSR:
	TEST	INSFLG,0FFH	;is insert mode active?
	JNZ	INSCSR		; brif so (returns to caller)
	JMP	SHORT OVRCSR	; otherwise, turn on overwrite cursor
				; and return to caller

;********************************************************************
;	OFFCSR - turn off cursor
;	INSCSR - turn on insert cursor (half-height)
;	OVRCSR - turn on overwrite cursor
;	USRCSR - turn on user cursor
;********************************************************************

OFFCSR:
	PUSH	AX		;save register on stack
	MOV	AX,OFFSET CS:B$OFFCSR	; turns off cursor
	JMP	SHORT CHGCSR	;jump to common code
INSCSR:
	PUSH	AX		;save register on stack
	MOV	AX,OFFSET CS:B$INSCSR	; displays insert cursor
	JMP	SHORT CHGCSR	;jump to common code
OVRCSR:
	PUSH	AX		;save register on stack
	MOV	AX,OFFSET CS:B$OVWCSR	; displays overwrite cursor
	JMP	SHORT CHGCSR	;jump to common code
USRCSR:
	PUSH	AX		;save register on stack
	MOV	AX,OFFSET CS:B$USRCSR	; displays user cursor
CHGCSR:
	PUSH	DX		;save register on stack

	MOV	DL,b$IOFLAG	
	AND	DL,RED_INP OR RED_OUT ; redirected input and output?
	CMP	DL,RED_INP OR RED_OUT 
	JZ	NO_CURSOR	; brif so -- don't touch cursor

	MOV	DX,b$CURSOR	; get cursor position
	CALL	AX 		; call appropriate cursor display routine
NO_CURSOR:			
	POP	DX		;restore register from stack
	POP	AX
	RET			;and return to caller

;********************************************************************
;	BADCHR - process an illegal character
;********************************************************************

BADCHR:
	CALL	EDTSTR		;dump out INPSTR to b$INBUF
	CALL	OUTBEL		;sound bell for signal
	RET			;return to caller

;********************************************************************
;	FKYCHR - process nonexpanded function key
;********************************************************************

FKYCHR:
	PUSH	AX		;save register on stack

	CALL	EDTSTR		;dump out INPSTR to b$INBUF
	SUB	AL,20H		;subtract offset to keys
	CMP	AL,NUM_FKEYS	;test if legal key number
	JL	FKYCH1		;if legal, then jump
	CALL	OUTBEL		;sound bell for signal
FKYCH1:
	POP	AX		;restore register from stack
	RET			;and return to caller

;********************************************************************
;	ASCCHR - process ASCII character
;********************************************************************

ASCCHR:
	CMP	BX,INPLEN	;test if INPSTR is full
	JL	ASCCH1		;if not, then jump
	CALL	EDTSTR		;dump out INPSTR to b$INBUF
ASCCH1:
	MOV	INPSTR[BX],AL	;put ASCII byte in INPSTR
	INC	BX		;advance INPSTR pointer
	RET			;and return to caller

;********************************************************************
;	KANCHR - process KANJI character
;		 rewrite to support interim characters
;********************************************************************


;********************************************************************
;	FRCSTR - force INPSTR out if no input character pending
;********************************************************************

FRCSTR:
	PUSH	AX		;save register on stack

	test	b$IOFLAG,RED_INP ; input redirected?
	jnz	frcst1		; Yes, always a char ready.
	CALL	B$TTYST	;test for character pending
	JNZ	FRCST1		;if one pending, then jump
	CALL	EDTSTR		;output INPSTR to b$INBUF
FRCST1:
	POP	AX		;restore register from stack
	RET			;return to caller

;********************************************************************
;	CTLCHR - process control character
;********************************************************************

;	search in CTLTAB for entry (1-byte code, 2-byte address)
;	if found, do processing, else signal bell for error

CTLCHR:
	PUSH	SI		; save register on stack

	CALL	EDTSTR		;dump out any INPSTR bytes
	MOV	SI,OFFSET CTLTAB ; initialize pointer to table
CTLCH1:
	CMP	SI,OFFSET CTLEND ; test if pointer at table end
	JE	CTLCH4		;if at end, sound bell for undefined
	CMP	CS:[SI],AL	; test if entry matches
	JE	CTLCH2		;if so, then jump
	ADD	SI,3		; point to next table entry
	JMP	SHORT CTLCH1	;and try to process it

;	set to overwrite mode according to function index

CTLCH2:
	CMP	SI,OFFSET CTLINS ; check if function clears INSFLG
	JAE	CTLCH3		;if not, then jump
	MOV	INSFLG,0	;put into overwrite mode
CTLCH3:
	CALL	CS:[SI+1]	; call routine address in table
	JMP	SHORT CTLCH5	;jump to return

;	sound bell for undefined key input

CTLCH4:
	CALL	OUTBEL		;output control-G (BELL) to screen
CTLCH5:
	POP	SI		; restore register from stack
	RET			;and return to caller

;********************************************************************
;	TABCHR - print TAB character
;********************************************************************

TABCHR:
	PUSH	BX		;save registers on stack
	PUSH	CX
	PUSH	SI

	MOV	BX,CSRPTR	;get cursor pointer in b$INBUF
	AND	BX,7		;get position within TAB field
	MOV	CX,8		;size of TAB field
	SUB	CX,BX		;get spaces to process
	MOV	SI,OFFSET DGROUP:b$EightSpaces ; set to string of blanks
	CALL	PRTCHR		;process the character

	POP	SI		;restore registers from stack
	POP	CX
	POP	BX
	RET			;and return to caller

;********************************************************************
;	TOGINS - toggle between insert and overwrite mode
;********************************************************************

TOGINS:
	XOR	INSFLG,0FFH	;complement flag between 00 and FF
	RET			;and return to caller

;********************************************************************
;	IGNORE - Throw away this control char without beeping.
;********************************************************************

IGNORE: RET

;********************************************************************
;	ENDLIN - signal end of line input
;	    Redisplays input line if:				[13]
;		LPR_ECHO OR RED_OUT  				[13]
;********************************************************************

ENDLIN:
	AND	b$IOFLAG,NOT IN_INPUT	; Tell B$TTY_SOUT to process edited
					; buffer
	PUSH	b$CURSOR		; save previous screen cursor values

	TEST	b$IOFLAG,LPR_ECHO OR RED_OUT ; printer echo or ouput redir?
	JZ	ENDLI2			; brif not -- don't re-display line.
	CALL	BEGIN		; Move cursor back to beginning of line
ENDLI2:
	CALL	APPEND		;move cursor to end-of-line, printing as we go
	DEC	ENDFLG		;set flag from 00 to FF
	POP	b$CURSOR		; restore previous cursor values
	AND	b$IOFLAG,NOT F_EDIT	; Re-enable $PRTMAP
	RET			;and return

;********************************************************************
;	BREAK - exit immediately from program
;********************************************************************

BREAK:
	AND	b$IOFLAG,NOT (F_EDIT OR IN_INPUT) ; Re-enable $PRTMAP
				; and exiting INPUT statement.
	CALL	APPEND		;move cursor to end of line
	JMP	B$BREAK	; print Break message and terminate

;********************************************************************
;	PRTCHR - process printing character on input line
;********************************************************************

;	entry - CX - number of bytes to process
;		SI - offset of string to be processed
;		CSRPTR - b$INBUF pointer to cursor
;		EOLPTR - b$INBUF pointer to end-of-line
;		INSFLG - insert mode, 0=overwrite, 1=insert

PRTCHR:
	PUSH	BX		;save registers used to stack
	PUSH	DX

	CALL	SETLOC		;set display columns / UPDPTR

;	set default values of insertion pointer BX and byte
;	  number DX

	MOV	DX,CX		;set number of bytes to insert
	MOV	BX,CSRPTR	;and the b$INBUF point to start

;	test if insertion or overwrite

	TEST	INSFLG,0FFH	;test for insertion
	JZ	PRTCH2		;if not, overwrite, jump

;	try to allocate DX bytes at b$INBUF[BX] for insertion

	CALL	ALCBYT		;allocate new space in b$INBUF
	JC	PRTCH4		;if not enough room, then jump
	JMP	SHORT PRTCH3	;jump to common code

;      overwrite - allocate any bytes that are appended to b$INBUF

PRTCH2:
	SUB	DX,EOLPTR	;DX=CX-EOLPTR
	ADD	DX,BX		;DX=CX-(EOLPTR-CSRPTR)
	OR	DX,DX		;test if any characters to append
	JLE	PRTCH3		;if not, just jump to finish up
	MOV	BX,EOLPTR	;insertion pointer is at b$INBUF EOL
	CALL	ALCBYT		;allocate new space in b$INBUF
	JC	PRTCH4		;if not enough room, then jump
	MOV	BX,CSRPTR	;restore pointer to cursor location

;	BLKKAN converts any KANJI bytes whose partners are modified
;	  into ASCII spaces. (KANJI only)
;	PUTCHR puts the character bytes entered into b$INBUF.
;	UPDATE updates the display screen to reflect the new contents
;	  of b$INBUF.

PRTCH3:
	CALL	PUTCHR		;put new characters in b$INBUF
	CALL	UPDATE		;update screen display
	JMP	SHORT PRTCH5	;processing successful - jump

;	if error occurred on allocation, output a control-G (BELL)

PRTCH4:
	CALL	OUTBEL		;output BELL character for error

;	done - restore registers and return

PRTCH5:
	POP	DX		;restore registers
	POP	BX
	RET			;and return to caller

;********************************************************************
;	ALCBYT - insert DX new ASCII spaces into b$INBUF indexed
;		 by BX.  The substring from BX to the end-of-line
;		 is moved to the right DX places.
;********************************************************************

;		 entry - BX - b$INBUF pointer of insertion
;			 DX - number of spaces to insert

;		 exit -  PSW.C=0 - insertion successful
;			 PSW.C=1 - no room - buffer unchanged

ALCBYT:
	PUSH	AX		;save registers on stack
	PUSH	CX
	PUSH	SI
	PUSH	DI

;	test for room in b$INBUF

	MOV	CX,EOLPTR	;get present EOL index
	ADD	CX,DX		;add insertion count
	CMP	CX,BUFLEN	;test if room in buffer
	JBE	ALCBY1		;if room, then jump

;	no room - set PSW.C=1 and return

	STC			;set carry bit in PSW
	JMP	SHORT ALCBY3	;and jump to return

;	move substring in b$INBUF located from index BX to EOLPTR
;	  over DX bytes to the right

ALCBY1:
	PUSH	BX		;save insertion pointer
	MOV	CX,EOLPTR	;get end-of-line value
	SUB	CX,BX		;now EOLPTR-BX
	INC	CX		;EOLPTR-BX+1 = length of substring
	MOV	BX,EOLPTR	;get end-of-line value
	STD			;set flag for decrementing SI/DI
	LEA	SI,b$INBUF[BX] ;get source offset
	MOV	DI,SI		;move offset to destination
	ADD	DI,DX		;add bytes to move to right
	REP	MOVSB		;move b$INBUF substring
	CLD			;clear flag
	POP	BX		;restore insertion pointer

;      put in ASCII spaces in new locations

	MOV	AL,ASC_SP	;set AL to ASCII space
	LEA	DI,b$INBUF[BX] ;get address of allocation
	MOV	CX,DX		;get count of bytes allocated
	REP	STOSB		;put ASCII spaces in them

;	update end-of-line pointer

	ADD	EOLPTR,DX	;add number of new bytes

;	signal successful insertion

ALCBY2:
	CLC			;carry cleared for success

;	done - restore registers and return

ALCBY3:
	POP	DI		;restore registers
	POP	SI
	POP	CX
	POP	AX
	RET			;and return to caller

;********************************************************************
;	BLKKAN - change KANJI bytes to ASCII spaces if their
;		 partners are being modified. (KANJI only)
;********************************************************************

;	entry - BX - b$INBUF index of start of modification
;		CX - size of string modification


;********************************************************************
;	PUTCHR - put new character(s) into b$INBUF
;********************************************************************

;	entry - BX - b$INBUF pointer to start of insertion
;		CX - number of bytes to insert
;		SI - pointer to string being inserted
;		CSRPTR - b$INBUF pointer to cursor
;		EOLPTR - b$INBUF pointer to end-of-line
;	exit  - CSRPTR - updated b$INBUF pointer to cursor

PUTCHR:
	PUSH	SI		;save registers on stack
	PUSH	DI

;	move CX bytes from string SI to b$INBUF indexed at BX

	PUSH	CX		;save insertion length
	LEA	DI,b$INBUF[BX] ;get insertion address
	REP	MOVSB		;put in characters from string at SI
	POP	CX		;restore insertion length

;	adjust CSRPTR for new character(s) inserted

	ADD	CSRPTR,CX	;move CX bytes to the right

	POP	DI		;restore registers from stack
	POP	SI
	RET			;and return from stack

;********************************************************************
;	DELCSR - delete character positioned at the cursor
;********************************************************************

DELCSR:
	PUSH	BX		;save register on the stack

	CALL	SETLOC		;set display columns / UPDPTR

	MOV	BX,CSRPTR	;get cursor pointer into b$INBUF
	CMP	BX,EOLPTR	;test if cursor at EOL
	JE	DELCS1		;at EOL, do nothing - just return
	CALL	DELETE		;delete character on cursor
	CALL	UPDATE		;and update the display
DELCS1:
	POP	BX		;restore register from stack
	RET			;and return to caller

;********************************************************************
;	DELLFT - delete character positioned left of the cursor
;		 if at start of line, delete character at cursor
;********************************************************************

DELLFT:
	PUSH	BX		;save register on the stack

	CALL	SETLOC		;set display columns / UPDPTR

	MOV	BX,CSRPTR	;get cursor pointer into b$INBUF
	OR	BX,BX		;test if cursor at start of line
	JZ	DELLF2		;at start, try to delete at cursor

	DEC	BX		;move pointer to char before cursor
	DEC	CSRPTR		;also move cursor back
	JMP	SHORT DELLF3	;jump to finish processing

;	at line start - delete character at cursor

DELLF2:
	CMP	BX,EOLPTR	;test if empty line
	JE	DELLF4		;if so, then just return

;	delete the character and update

DELLF3:
	CALL	DELETE		;delete character before cursor
	CALL	UPDATE		;update the display screen
DELLF4:
	POP	BX		;restore register from stack
	RET			;and return to caller

;********************************************************************
;	DELETE - delete character at BX and move remaining
;		 substring to the left to fill in
;********************************************************************

DELETE:
	PUSH	CX		;save registers on the stack
	PUSH	DX
	PUSH	SI
	PUSH	DI

;	If interim character is pending, just output bell and return.
;	Cursor is on KANJI2 byte only from DELLFT.


	MOV	UPDPTR,BX	;update at start of substring
	MOV	DX,1		;assume one byte to delete
	MOV	CX,EOLPTR	;CX will be length - get EOL pointer
	SUB	CX,BX		;CX=EOL-CSR
	INC	CX		;CX=EOL-CSR+1 - length of substring
	SUB	CX,DX		;subtract length of char deleted
	LEA	DI,b$INBUF[BX] ;get destination pointer
	MOV	SI,DI		;source is destination...
	ADD	SI,DX		;plus length of the deleted char
	REP	MOVSB		;move the substring in b$INBUF
	SUB	EOLPTR,DX	;move EOL pointer back
	POP	DI		;restore the registers from stack...
	POP	SI
	POP	DX
	POP	CX
	RET			;and return to caller

;********************************************************************
;	TRUNC - truncate input line at cursor
;********************************************************************

TRUNC:
	PUSH	BX		;save register on stack

	CALL	SETLOC		;set display columns / UPDPTR
	MOV	BX,CSRPTR	;get cursor b$INBUF index

;	cursor cannot be on KANJI2 byte - code removed.

	MOV	EOLPTR,BX	;truncate the line at the cursor
	MOV	b$INBUF[BX],0	;set end-of-line byte
	CALL	UPDATE		;update the screen display

	POP	BX		;restore register from stack
	RET			;return to caller

;********************************************************************
;	BEGIN - move cursor to start of input line
;********************************************************************

BEGIN:
	PUSH	BX		;save register on stack

	CALL	SETLOC		;set display columns / UPDPTR
	XOR	BX,BX		;clear BX
	MOV	CSRPTR,BX	;move cursor to start of input line
	CALL	UPDATE		;update the screen display

	POP	BX		;restore register from stack
	RET			;return to caller

;********************************************************************
;	APPEND - move cursor to end of input line
;********************************************************************

APPEND:
	PUSH	BX		;save register on stack

	CALL	SETLOC		;set display columns / UPDPTR
	MOV	BX,EOLPTR	;set to EOL pointer
	MOV	CSRPTR,BX	;move cursor to end of input line
	CALL	UPDATE		;update the screen display

	POP	BX		;restore register from stack
	RET			;return to caller

;********************************************************************
;	ERASE - delete entire input line
;********************************************************************

ERASE:
	PUSH	BX		;save register on stack

	CALL	SETLOC		;set display columns / UPDPTR
	XOR	BX,BX		;clear BX
	MOV	CSRPTR,BX	;set cursor to start of input line
	MOV	UPDPTR,BX	;set update to start of input line
	MOV	EOLPTR,BX	;set EOL to start of input line
	CALL	UPDATE		;update the screen display
	MOV	b$INBUF,BL	;put end-of-line byte at start

	POP	BX		;restore register from stack
	RET			;return to caller

;********************************************************************
;	CSRLFT - move cursor once to the left
;********************************************************************

CSRLFT:
	PUSH	AX		;save registers on stack
	PUSH	BX

	MOV	BX,CSRPTR	;get b$INBUF cursor index
	OR	BX,BX		;test if cursor at line beginning
	JZ	CSRLF2		;if so, then just jump to exit

;	backspace over byte at cursor
;	if extra display blank is now on cursor, backspace over it

	DEC	BX		;get byte before it
	CALL	OUTBS		;backspace over byte at cursor
	DEC	CSRPTR		;set pointer to previous byte
CSRLF2:
	POP	BX		;restore registers from stack
	POP	AX
	RET			;return to caller

;********************************************************************
;	CSRRGT - move cursor once to the right
;		 move over both bytes of KANJI character
;********************************************************************
;Entry	BX = pointer in INPSTR

CSRRGT:
	PUSH	AX		;save registers on stack
	PUSH	BX

	MOV	BX,CSRPTR	;get b$INBUF cursor index

	CMP	BX,EOLPTR	;test if cursor is at EOL
	JNE	CSRRG0
	POP	BX
	POP	AX
	MOV	AL,ASC_SP	; if at EOL then put out a space
	JMP	ASCCHR
CSRRG0:
;    CMP     BX,EOLPTR	     ;test if cursor is at EOL
;    JE      CSRRG4	     ;if so, then just jump to exit

;	if byte at cursor is KANJI1, then output it and its
;	  partner and then backspace once
;	do not backspace, but just output both characters


	MOV	AL,b$INBUF[BX] ;get KANJI2 (or ASCII) byte
	CALL	B$TTY_SOUT	;and output it also
;      if cursor over extra blank, then pass over it


;	update CSRPTR and call FRCLIN

	INC	CSRPTR		;update cursor pointer
	CALL	FRCLIN		;force cursor on new line if needed
CSRRG4:
	POP	BX		;restore registers from stack
	POP	AX
	RET			;and return to caller

;*********************************************************************
;    FORWRD - move cursor forward one word - first alphanumeric
;	      character after the first non-alphanumeric one
;*********************************************************************

FORWRD:
	PUSH	BX		;push register on stack
	MOV	BX,CSRPTR	;get cursor pointer
	DEC	BX		;BX is at cursor after INC next
FORWR1:
	INC	BX		;move pointer forward one byte
	CMP	BX,EOLPTR	;test if at line end
	JZ	FORWR3		;if so, then jump
	CALL	TSTALN		;test if byte is alphanumeric
	JC	FORWR1		;if so, then loop back
FORWR2:
	CMP	BX,EOLPTR	;test if at line end
	JZ	FORWR3		;if so, then jump
	INC	BX		;move pointer forward one byte
	CALL	TSTALN		;test if byte is alphanumeric
	JNC	FORWR2		;if not, then loop back
FORWR3:
	CMP	BX,CSRPTR	;is cursor at new value?
	JE	FORWR4		;if so, then jump to finish up
	CALL	CSRRGT		;move cursor right, CSRPTR updated
	JMP	SHORT FORWR3	;try for next one
FORWR4:
	POP	BX		;restore register from stack
	RET			;return to caller

;*********************************************************************
;    BAKWRD - move cursor backward one word - before first
;	      non-alphanumeric character after first alphanumeric
;*********************************************************************

BAKWRD:
	PUSH	BX		;push register on stack
	MOV	BX,CSRPTR	;get cursor pointer
BAKWR1:
	OR	BX,BX		;test if at line start
	JZ	BAKWR3		;if so, then jump
	DEC	BX		;move pointer back one byte
	CALL	TSTALN		;test if byte is alphanumeric
	JNC	BAKWR1		;if not, then loop back
BAKWR2:
	OR	BX,BX		;test if at line start
	JZ	BAKWR3		;if so, then jump
	DEC	BX		;move pointer back one byte
	CALL	TSTALN		;test if byte is alphanumeric
	JC	BAKWR2		;if so, then loop back
	INC	BX		;overshot, move to first alphanum
BAKWR3:
	CMP	BX,CSRPTR	;is cursor at new value?
	JE	BAKWR4		;if so, then jump to finish up
	CALL	CSRLFT		;move cursor left, CSRPTR updated
	JMP	SHORT BAKWR3	;try for next one
BAKWR4:
	POP	BX		;restore register from stack
	RET			;return to caller

;*********************************************************************
;    TSTALN - set carry if b$INBUF[BX] points to an alphanumeric
;	      character (KANJI, 0-9, A-Z, a-z).
;*********************************************************************

TSTALN:
	PUSH	AX		;save register on stack
	MOV	AL,b$INBUF[BX] ;get character to test
	CMP	AL,"0"		;test against lowest numeric
	JB	TSTAL1		;if less, then non-alphanumeric
	CMP	AL,"9"		;test against highest numeric
	JBE	TSTAL2		;if not greater, then alphanumeric
	CMP	AL,"A"		;test against lowest uppercase
	JB	TSTAL1		;if less, then non-alphanumeric
	CMP	AL,"Z"		;test against highest uppercase
	JBE	TSTAL2		;if not greater, then alphanumeric
	CMP	AL,"a"		;test against lowest lowercase
	JB	TSTAL1		;if less, then non-alphanumeric
	CMP	AL,"z"		;test against highest lowercase
	JBE	TSTAL2		;if not greater, then alphanumeric
TSTAL1:
	CLC			;clear carry for non-alphanumeric
	JMP	SHORT TSTAL3	;jump to return
TSTAL2:
	STC			;set carry for alphanumeric
TSTAL3:
	POP	AX		;restore register from stack
	RET			;return to caller

;********************************************************************
;	SETLOC - set display column amounts for the cursor and
;		   end-of-line positions
;		 set default value of UPDPTR to CSRPTR
;********************************************************************

SETLOC:
	PUSH	BX		;save register on stack

	MOV	BX,CSRPTR	;get cursor pointer value
	MOV	UPDPTR,BX	;set default value for update
	MOV	CSRLOC,BX	;store for later use in UPDATE
	MOV	BX,EOLPTR	;get eol pointer value
	MOV	EOLLOC,BX	;store for later use in UPDATE

	POP	BX		;restore register on stack
	RET			;and return to caller


;********************************************************************
;	GETLOC - take b$INBUF pointer in BX and add the number of
;		 extra blanks to return the display column in BX.
;********************************************************************


;********************************************************************
;	UPDATE - update screen display with the contents of b$INBUF

;	entry - CSRLOC - display column of old b$INBUF cursor
;		EOLLOC - display column of old b$INBUF EOL
;		CSRPTR - b$INBUF index of new cursor position
;		EOLPTR - b$INBUF index of new EOL position
;		UPDPTR - b$INBUF index to start update
;********************************************************************

UPDATE:
	PUSH	AX		;save registers on the stack...
	PUSH	BX
	PUSH	CX
	PUSH	DX

;	put update pointer in DX - decrement if on KANJI2 byte
;	cursor cannot be on KANJI2 byte - remove code.

	MOV	DX,UPDPTR	;get update pointer

;	backspace display to update pointer

	MOV	CX,CSRLOC	;get old cursor display column
	SUB	CX,DX		;result is number of backspaces
	JCXZ	UPDA06		;if no backspaces, then jump
UPDA05:
	CALL	OUTBS		;backspace the display
	LOOP	UPDA05		;repeat until at start of update

;      output string in b$INBUF from update start to EOL

UPDA06:
;   MOV     BX,DX	    ;start output at update pointer
	MOV	BX,UPDPTR	;start output at update pointer
UPDA07:
	CMP	BX,EOLPTR	;test if string has been output
	JE	UPDA09		;if so, then jump


;	output the character at b$INBUF[BX]

	MOV	AL,b$INBUF[BX] ;get buffer character
	CALL	B$TTY_SOUT	; output character to screen
	CMP	AL,0FFH		; Test if char was 0ffH
	JNE	UPDA08_NOT_FF	; Brif not
	CALL	B$TTY_SOUT	; Send it again
UPDA08_NOT_FF:			
	INC	BX		;advance update pointer for next
	JMP	SHORT UPDA07	;and try to process it

;	blank out trailing characters if new string was shorter
;	  backspace cursor back to EOL

UPDA09:
	MOV	CX,EOLLOC	;CX has display column of old EOL
	SUB	CX,BX		;CX now has old-new difference
	OR	CX,CX		;test if new string was shorter
	JLE	UPDA12		;if not, then jump
	PUSH	CX		;save shrinkage count
	MOV	AL,ASC_SP	;put in ASCII space for output
UPDA10:
	CALL	B$TTY_SOUT	; output the space to cover old string
	LOOP	UPDA10		;loop until old string covered

	POP	CX		;get shrinkage count again
UPDA11:
	CALL	OUTBS		;output backspace to screen
	LOOP	UPDA11		;loop until back to new EOL

;	backspace to location of the new cursor to complete update

UPDA12:
	MOV	CX,BX		;put new EOL display column in CX
	MOV	BX,CSRPTR	;BX has new cursor index
	SUB	CX,BX		;get backspaces from EOL to cursor

	JCXZ	UPDA14		;if no backspaces, then jump
UPDA13:
	CALL	OUTBS		;output backspace
	LOOP	UPDA13		;loop until back to new cursor
	JMP	SHORT UPDA15	;jump to exit

;	cursor is at end of input line - if also at end of display
;	  line, output space and backspace for new display line

UPDA14:
	CALL	FRCLIN		;force new line if present is full

;	done - restore register and return

UPDA15:
	POP	DX		;restore registers from stack
	POP	CX
	POP	BX
	POP	AX
	RET			;and return to caller

;********************************************************************
;	OUTBS - output backspace to screen
;********************************************************************

OUTBS:
	PUSH	AX		;save register on stack

	MOV	AL,b$IOFLAG	
	AND	AL,RED_INP OR RED_OUT ; redirected input and output?
	CMP	AL,RED_INP OR RED_OUT 
	JZ	OUTBX		; brif so -- don't touch screen

	PUSH	DX		; save register
	MOV	DX,b$CURSOR	; get cursor location (1,1) relative
	DEC	DH		; decrement column and set flags
	JNZ	OUTBS1		; jump if not now at column 0
	MOV	DH,BYTE PTR b$CRTWIDTH ; column 0 - put in one past end
	DEC	DX		; and decrement row counter
OUTBS1:
	CALL	B$SCNLOC 	;display cursor at new location

	POP	DX		; restore register
OUTBX:				
	POP	AX		;restore register from stack
	RET			;and return to caller

;********************************************************************
;	OUTBEL - output BELL character to screen
;********************************************************************

OUTBEL:
;   PUSH    AX		    ;save register on stack

;   MOV     AL,CNTL	    ; put control char flag in
;   CALL    B$TTY_SOUT	    ;and output it to the speaker
;   MOV     AL,CTL_G	    ;put control-G (BELL) in for output
;   CALL    B$TTY_SOUT	    ;and output it to the speaker

;   POP     AX		    ;restore register from stack
	cCALL	B$BLEEP	; sound bell on terminal
	RET			;and return to caller

;********************************************************************
;	FRCLIN - forces the cursor to the next display line if the
;		   present display line is full
;********************************************************************

FRCLIN:
	PUSH	AX		;save registers on stack
	PUSH	BX

	CALL	B$SCRN_GPOS 	; get column position of cursor
	CMP	AH,BYTE PTR b$CRTWIDTH ; test if at end of display line
	JNE	FRCLI3		;if not, then jump

;	display line full - if at b$INBUF EOL, then output space
;	  and backspace

	MOV	BX,CSRPTR	;get cursor index
	CMP	BX,EOLPTR	;test if at b$INBUF EOL
	JNE	FRCLI1		;if not, then jump
	MOV	AL,ASC_SP	;get ASCII space
	CALL	B$TTY_SOUT	;and output it
	CALL	OUTBS		;then output the backspace
	JMP	SHORT FRCLI3	;jump to exit

;	not EOL - byte must be KANJI1 or ASCII

FRCLI1:

;	ASCII byte - output it and then backspace over it

	MOV	AL,b$INBUF[BX] ;get ASCII byte
	CALL	B$TTY_SOUT	;and output it
	CALL	OUTBS		;backspace over it
FRCLI3:
	POP	BX		;restore registers from stack
	POP	AX
	RET			;return to caller

;********************************************************************
;	TSTKJ1 - test for first byte of KANJI in b$INBUF[BX]
;********************************************************************

;	entry - BX - b$INBUF index to byte tested

;	exit - PSW.C=0 - byte tested is not KANJI1
;	       PSW.C=1 - byte tested is KANJI1



;********************************************************************
;	TSTKJ2 - test for second byte of KANJI in b$INBUF[BX]
;********************************************************************

;	entry - BX - b$INBUF index to byte tested

;	exit - PSW.C=0 - byte tested is not KANJI2
;	       PSW.C=1 - byte tested is KANJI2


sEnd	CN_TEXT 		

	END			;textual end of module B$RDLIN
