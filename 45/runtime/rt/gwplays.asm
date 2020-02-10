	TITLE	GWPLAYS - GW BASIC 2.0 Multi Voice Play
;***
; GWPLAYS - GW BASIC 2.0 Multi Voice Play
;
;	Copyright <C> 1986 - 1988, Microsoft Corporation
;
;Purpose:
;	Multi Voice Play statement processor
;
; BASIC Syntax mapping to included runtime entry points:
;
; - PLAY Statement:
;
;      PLAY string
;	 |
;      B$SPLY
;
;    NOTE: the more advanced syntax of 'PLAY string, string, ...' (i.e.
;    multi-voiced PLAY) will be accepted by the compiler. For all but the
;    last parameter (which is passed to B$SPLY), defaulted parameters will
;    generate a call to B$PL1, and specified parameters get passed to B$PL0
;    both of these, in turn, just generate an Advanced Feature error.
;    NOTE 2: the above comment doesn't apply to bascom 30.  The compiler
;	will not generate calls to B$PL0 and B$PL1.  They will be flagged as
;	syntax errors.	B$PL0 and B$PL1 will not be present for BASCOM 30.
;	The code to support multivoice play will be left for future reference,
;	but it will need to be changed to the new style interface (stack based)
;	if a multi voice version is ever produced.
;
;******************************************************************************

	INCLUDE switch.inc	; switch file [new]
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	_DATA		
	USESEG	_BSS		
	USESEG	SN_TEXT 	

	INCLUDE seg.inc 	
	INCLUDE	ibmunv.inc	
	INCLUDE	rtps.inc	
	INCLUDE idmac.inc	
	INCLUDE string.inc	

	.RADIX	10

;	Two new macros Alloc & DeclareB/DeclareW are defined here.
;	The main purpose of Declare macro is to allocate given number of
;	bytes (static variable) and initialize it to the specified value.
;	Additionally, it declares an EQUate for the specified variable
;	for automatic indexing if FS_MVOICE is TRUE. The index register
;	assumed is SI. Actually, the variable defined has a trailing underscore
;	and the indexed EQUate is identical to the name supplied to this macro.
;	Alloc is just a helper macro.

Alloc	MACRO	v,nm,init,siz	
static&v	<nm&_>,<init>,<siz>

nm	EQU	nm&_		
	ENDM

DeclareB	MACRO	nm,init,siz	
Alloc	B,<nm>,<init>,<siz>
	ENDM			

DeclareW	MACRO	nm,init,siz	
Alloc	W,<nm>,<init>,<siz>
	ENDM			


sBegin	_DATA			

externW	B$AC			

sEnd				

sBegin	_BSS			

	externW B$MCLTAB 	;defined in GWDATA.ASM
	externW B$MCLLEN 	;defined in GWDATA.ASM
	externW B$MCLPTR 	;defined in GWDATA.ASM

	externB B$MMODE		;defined in GWINI.ASM
	externB B$BEATS		;defined in GWINI.ASM
	externB B$NOTE1L 	;defined in GWINI.ASM
	externB B$NOTELN 	;defined in GWINI.ASM
	externB B$NOTFLG 	;defined in GWINI.ASM
	externB B$MSCALE 	;defined in GWINI.ASM
	externB B$OCTAVE 	;defined in GWINI.ASM
	externB b$VTYP		
	externW b$curlevel	;current program level



DeclareB MQUEFL,?,1		
DeclareB PLYPRS,?,1		
DeclareW STRDSC,?,NUM_VOICES	
DeclareW VSTACK,?,<NUM_VOICES*NUM_VSTACK> 
DeclareW VSTBAS,?,NUM_VOICES	
DeclareW VSTOFF,?,NUM_VOICES	

sEnd	_BSS			

assumes CS,SN_TEXT		
sBegin	SN_TEXT 		

;	externNP B$MCLXEQ	;execute substring routine

	externNP B$DONOTE	;oem routine
	externNP B$STRTSD	
	externNP B$SNDWAT	

	externNP B$ERR_FC	
	externNP B$ERR_OM	
	externNP B$ERR_TM	; Type mismatch error

	externNP B$SETMCL	
	externNP B$MACCMD	
	externNP B$FETCHR	
	externNP B$DECFET	
	externNP B$FETCHZ	
	externNP B$VALSC2	
	externNP B$BREAK_CHK	
	externNP B$GETSTKC	; Check stack
	externNP B$SCNVAR	; get descriptor from VARPTR$

	externNP B$STDALCALLTMP ; **** liberate temporaries ****

;PLAY Statement
;Syntax:
;   PLAY ON
;   PLAY OFF
;   PLAY STOP
;   PLAY [A$] [, [B$] [, [C$]]]   - multi-voice play

;PLAY is made up of a forground task and a background task.  A queue exists
;for each voice through which the forground task passes the background task
;Commands and Syncronization information.  One SYNC byte is placed in each
;voice queue at the start of each PLAY statement.  All commands which are
;produced by the PLAY statement follow the SYNC byte in the queue.
;When the background task encounters the SYNC byte, it disables that voice
;until it has received a SYNC byte for each voice and PLYBGC .GT. 0.  This
;mechanism insures that all voices for a play statement will begin at
;the same time, even if one of the voices in the previous play statement
;was shorter than the rest.

;A String-Pointer stack is associated with each voice to allow each voice
;string to include (nest) other voice strings via the X macro-language
;command.

;PLAY Algorithm:
;    Initialize a str-ptr stack for every voice.
;    Parse n strings (saving ptrs to string temps in STRDSC(i)).
;    For each voice,
;      Put SYNC byte at start of each voice queue
;      Release Temp string desc and push str ptr&len to voice's str-ptr stack.
;    For each voice,
;1)    Fill each queue as far as possible (till end-of-string or end-of-queue)
;      if 1st pass
;	 B$STRTSD (start clock int routine, all voices)
;      if no voices active and all strings are not empty, B$STRTSD(all voices)
;	 (This is a safety valve to insure BASIC will never hang during PLAY)
;    If all strings have not been completely consumed,
;      goto step 1
;  If Music Forground, wait till all voice activity halts



PLAYS:
; Entry point for the PLAY statement.  Test to see which of the
; allowed forms has been specified.

;***
; B$PL0, B$PL1, B$SPLY - Play   <sexp>... statement
;
; Purpose:
;	Runtime Entry Points.
;	B$PL0 and B$PL1 are alternate PLAY Statement preamble entry points
;		B$SPLY is called with the last parm, and it actually executes
;		the statement.
;	B$PL0   called if a parm is specified
;	B$PL1   called if a parm is defaulted
;	B$SPLY   called for last parm (may be the only parm as well)
;
;	 Parse the strings from the program statement, and set up the pointers
;	 in the string stacks.
;
; Input:
;	sdPlay == string descriptor of a single argument (B$PL0 & B$SPLY only -
;		no input is given for B$PL1)
; Output:
;	NONE
; Modifies:
;	NONE
;****


cProc	B$SPLY,<PUBLIC,FAR>,<SI> 
parmSD	sdPlay			
cBegin				
	cCALL	B$CHKINI
	GetpSD	BX,sdPlay	;BX = psd for play string
	MOV	STRDSC,BX	; save str desc for B$PARSER.
	cCALL	B$PARSE0	; Go process everything
	MOV	AX,b$curlevel	;deallocate all temps >= current level
	cCALL	B$STDALCALLTMP	
cEnd				

;***
;B$CHKINI
;
;PURPOSE:
;	Initialize runtime variables related to music if starting play statement
;
;ENTRY:
;
;EXIT:
;
;MODIFIES:
;	AX,CX,SI
;
;****

cProc	B$CHKINI,<NEAR>		
cBegin				

				;exit -- bx is preserved

	XOR	AX,AX		

	MOV	[B$MCLTAB],OFFSET PLYTAB ;B$MCLTAB points to play command table
				; for B$MACCMD
	MOV	[MQUEFL],AL	; indicates no music cmds have been queued

; Initialize the string stacks for use while parsing the play strings.
; These stacks are used to support the X music command which functions
; like a subroutine call.

	MOV	CX,OFFSET DGROUP:VSTACK ; *cx = string stack for voice 1
inistl:
	MOV	VSTOFF,AX	; reset offset for voice si
	MOV	VSTBAS,CX	; set stack base ptr for voice si
	MOV	STRDSC,AX	; reset string desr ptr for voice si
chkret:

cEnd				; End of B$CHKINI

;
;***
;B$PARSE0
;
;PURPOSE:
;	Pre-PARSER for play string
;
;ENTRY:
;
;EXIT:
;
;USES:
;	per convention
;
;****

cProc	B$PARSE0,<NEAR>	
cBegin				; **** HERE AFTER LAST PARM ****

; Release string temps in reverse order (because its a stack)

	MOV	[PLYPRS],NUM_VOICES ;NUM_VOICES strings have been parsed


SETSTL:

	MOV	BX,STRDSC	; *bx = temp str desc for voice(si)
	OR	BX,BX
	JE	EMPTYS		;branch if no string was parsed
	CALL	B$SETMCL 	;set B$MCLPTR, B$MCLLEN to str @bx
	cCALL	B$PUTSTR	;save B$MCLPTR, B$MCLLEN on voicen's stack
	DEC	[PLYPRS]	;one less string has been parsed
emptys:
	JMP	SHORT B$PARSER	

cEnd	<nogen>			; End of B$PARSE0

;***
;B$PARSER fills the background-music queues as follows:
; Repeat
;   For Each voice
;     while (input string is not empty) and
;	    (room exists in voice's queue for 1 command)
;	Call B$MACCMD to process next command in string.
;	(This causes some routine in PLYTAB to be called which may try to
;	 queue information for the background task into that voice's queue.
;   If 1st pass, DI, bump PLYBGC, cCALL	B$STRTSD(7) to initiate background task
; Until all voice strings are empty
; If music-forground, wait until PLYMSK=0
;****

cProc	B$PARSER,<NEAR>	
cBegin				

PARSER_BEGIN:			


; Get music macro commands from the string for this voice until the
; queue for this voice fills or we hit the end of the string.

PRSL10:
	cCALL	B$GETSTR		;Get B$MCLPTR, B$MCLLEN for [voicen]


	CMP	[B$MCLLEN],0	; Check if end of string
	JNE	PRSCONT		; Brif more in string, process it
	CMP	VSTOFF,0	; Test if string was nested by X command
	JNE	PRSL10		; Brif so, jump to return to next string up
	JMP	SHORT PRSLPX	; Else, the top level string s done 
PRSCONT:			; Continue with parsing
	PUSH	[B$MCLPTR]	;SAve the current string pointers
	PUSH	[B$MCLLEN]	;  in case the queue overflows

	CALL	B$MACCMD 	;Parse one command from this string
	JB	PRSL20		;Quit this loop if the queue overflowed
	POP	AX		;Clear old pointers from stack
	POP	AX
	cCALL	B$PUTSTR		;save new B$MCLPTR, B$MCLLEN for [voicen]
	JMP	SHORT PRSL10	;continue trying to fill this queue

; Had a queue overflow on the last call to B$MACCMD.  Restore the original
; string pointers so that the command which caused the overflow can be
; reprocessed the next time.
PRSL20:
	POP	[B$MCLLEN]	;RESTORE the old pointers
	POP	[B$MCLPTR]
	cCALL	B$PUTSTR		;And  put them back on the local stack

PRSLPX:


; Test if the user wants to break
	CALL	B$BREAK_CHK	;CTRL-BREAK?

; Have made a pass through the strings.  If this is pass 1 and there
; is data in any of the queues, then start the music playing.

	TEST	PLYPRS,LOW 128D
	JNZ	NOTPS1		;Brif not Pass 1
	OR	PLYPRS,LOW 128D ;Set parsed once flag
	CMP	MQUEFL,LOW 0
	JE	NOTPS1		;branch if no cmds have been queued for
				; this PLAY statement (no need to start
				; background task
	cCALL	B$STRTSD	;Start sounds

NOTPS1:

; Test if we are all done.  This occurs when the count of voice strings
; completely parsed (contained in PLYPRS) equals the number of voices.

	CMP	[PLYPRS],LOW 200O+NUM_VOICES
	JE	PRSDON		;Brif all NUM_VOICES strings parsed
	MOV	AL,LOW TSTVOC	;Ask the OEM if any voices are active
	cCALL	B$DONOTE	
	OR	AL,AL		;See what they had to say about it
	JNE	PARSER_BEGIN	; If there are active voices, then the
				;queue's are emptying, so go try to
				;parse the rest of the strings.

; If we get here, then it means that we are not done parsing the PLAY
; strings, the queues are full, but none of the voices are active.
; This should never occur, but IF it did, BASIC would be hung indefinitely.
; This simple safeguard ensures this will never happen.

	MOV	AL,LOW STRSND	;Function code to start music
	cCALL	B$DONOTE 	; Tell OEM to start playing the music
	JMP	PARSER_BEGIN	; and go parse the rest of the strings

; We are all done parsing the input strings.  If the music mode is
; Background, then we are done.  If mode is Foreground, we need to
; wait around until the sound stops.

PRSDON:
	cCALL	B$SNDWAT	;wait if Music Foreground

cEnd				; End of B$PARSER


;***
;B$PUTSTR
;Purpose:
;	Save B$MCLPTR, B$MCLLEN on Voice Stack for voice [VOICEN]
;Input:
;	[VOICEN] = voice id (0..2)
;	[B$MCLLEN]=number of bytes left in current string for this voice
;	[B$MCLPTR]=pointer to next byte in current string for this voice
;Output:
;	If stack overflows, an Out of Memory error is issued
;Modifies:
;	AX
;****

cProc	B$PUTSTR,<NEAR>,<BX,SI,DI>	
cBegin				

; Get the stack pointer for this string and check for stack overflow


	MOV	DI,VSTBAS	; di points to stack base for voice si
	MOV	BX,VSTOFF	; bx = offset for current top of stack
	CMP	BX,NUM_VSTACK	;Check if the stack is full
	JB	STKOK		;branch if still room on stack
	JMP	B$ERR_OM	;NO room, so signal out of memory error

				; Save the current data on the stack
STKOK:
	MOV	AX,[B$MCLPTR]
	MOV	[BX+DI],AX	;save string pointer on stack
	MOV	AX,[B$MCLLEN]
	MOV	[BX+DI]+2,AX	;save string length on stack
; Update the stack pointer to account for new stack size
	ADD	BX,4
	MOV	VSTOFF,BX	; save offset to top of stack

cEnd				; End of B$PUTSTR

;***
;B$GETSTR
;Purpose:
;	Set B$MCLPTR, B$MCLLEN for Voice [VOICEN]
;Input:
;	[VOICEN] = voice id (0..2)
;Output:
;	[B$MCLLEN]=number of bytes left in current string for this voice
;	[B$MCLPTR]=pointer to next byte in current string for this voice
;	If this voice's string has been completely consumed then
;	  PLYPRS is incremented and B$MCLLEN=0
;Modifies:
;	AX
;****

cProc	B$GETSTR,<NEAR>,<BX,SI,DI>	
cBegin				

; Get the stack pointer for this voice, and test if stack is empty

	MOV	BX,VSTOFF	; bx = offset for current top of stack
	MOV	[B$MCLLEN],BX	;length=0 if no more strings stacked
	OR	BX,BX
	JE	GETSTX		;brif no entries exist on voice's stack
	MOV	DI,VSTBAS	; DI points to stack base for voice SI

; Get the next set of entries from the stack.
GETSTL:
	SUB	BX,4		;Adjust stack pointer to next entry
	MOV	AX,[BX+DI]	;get string pointer from stack
	MOV	[B$MCLPTR],AX
	MOV	AX,[BX+DI]+2	;get string length from stack
	MOV	[B$MCLLEN],AX
	OR	AX,AX
	JNE	GETSTX		;exit if this string is not empty
	OR	BX,BX
	JNE	GETSTL		;brif more entries on voice's stack
	INC	[PLYPRS]	;Bump number of strings consumed

; Update the stack pointer, restore the registers, and get out
GETSTX:
	MOV	VSTOFF,BX	; save offset to top of stack

cEnd				; End of B$GETSTR

;--------------------------------------------------------------------------
; Music Macro Language command table
; This table contains all of the command characters allowed in the music
; language strings, and the entry points of the routines to process them

PLYTAB	LABEL	BYTE		

	DB	"A"		;The notes A-G
	DW	OFFSET PLYNOT
	DB	"B"
	DW	OFFSET PLYNOT
	DB	"C"
	DW	OFFSET PLYNOT
	DB	"D"
	DW	OFFSET PLYNOT
	DB	"E"
	DW	OFFSET PLYNOT
	DB	"F"
	DW	OFFSET PLYNOT
	DB	"G"
	DW	OFFSET PLYNOT

	DB	"M"		;Music Meta Command
	DW	OFFSET B$PLYMET

;	DB	"Q"		;Envelope Subcommand Lead In
;	DW	OFFSET QCMNDS

	DB	"N"+128 	;PLAY NUMERIC NOTE
	DW	OFFSET PLYNUM
	DB	"O"+128 	;OCTAVE
	DW	OFFSET POCTAV
	DB	"P"+128 	;PAUSE
	DW	OFFSET PPAUSE
	DB	"T"+128 	;TEMPO
	DW	OFFSET PTEMPO
	DB	"L"+128 	;LENGTH
	DW	OFFSET PLYLEN

;	DB	"V"+128 	;Volume
;	DW	OFFSET PVOLUM

	DB	"X"		;EXECUTE STRING
;	DW	OFFSET B$MCLXEQ	; substring handler in MCLPRC
	DW	OFFSET B$PLYXEQ	; Substring handler is local

	DB	"<"		;Decrement Octave
	DW	OFFSET POCTAD
	DB	">"		;Increment Octave
	DW	OFFSET POCTAI
	DB	00		;END OF TABLE


; This table contains the allowed subcommands for the Q command
; in the Music Macro Language.

;	This table has been removed as part of PC Jr code removal

;B$PLYXEQ
;Purpose:
;	Reimplemented as part of revision [5]
;	This routine is dispatched to by the X command in the PLAY statement.
;	It is equivalent to a macro-language subroutine call, in that it
;	specifies a variable which is to be inserted in the Macro String.  It:
;
;	 1)  Calls B$GETSTKC to check for enought stack space.
;	 2)  Calls B$SCNVAR to get string descriptor in the FAC
;	 3)  Calls B$PUTSTR to stack the current string pointer & length,
;	 4)  Sets B$MCLPTR & B$MCLLEN to point to new nested string,
;	 5)  Returns to its caller (presumably PLAY (via B$MACCMD)

cProc	B$PLYXEQ,<NEAR>	
cBegin				

	MOV	CL,100		; Get size for stack check
	CALL	B$GETSTKC	; Check the stack for enough room
	CALL	B$SCNVAR	; Get the VARPTR$ descriptor offset in FAC
	CMP	[b$VTYP],VT_SD	; Test if type was a string
	JNE	PLYERR		; Brif not, signal an error
	cCALL	B$PUTSTR	; Put B$MCLPTR & B$MCLLEN on local stack
	MOV	BX,OFFSET DGROUP:B$AC	; Get FAC for descriptor
	CALL	B$SETMCL		; Set new values of B$MCLPTR & B$MCLLEN
	CLC			; To indicate to use new values

cEnd				; End of B$PLYXEQ

PLYERR:				; Indicate type mismatch error

	JMP	B$ERR_TM	

; Decrement the current octave number
POCTAD: CMP	B$OCTAVE,LOW 0	
	JZ	PLYRET
	DEC	B$OCTAVE		; octave -1
	RET

; Increment the current octave number
POCTAI: cmp	B$OCTAVE,low 6	
	JNB	PLYRET
	CLC
	INC	B$OCTAVE		; octave +1
	RET

; Set the volume level

; Volume support code has been deleted from here

; Set the note length
PLYLEN:
	JNB	PLGOFC		;ERROR IF NO ARG
	CMP	DL,LOW 65	;ALLOW ONLY UP TO 64
	JNB	PLGOFC		;FC ERROR IF TOO BIG
	OR	DL,DL		;DON'T ALLOW ZERO
	JZ	PLGOFC		;FC ERROR IF ZERO
	MOV	B$NOTELN,DL	; store note length
	RET

; Set the play tempo
PTEMPO:
	CMP	DL,LOW 32	;ALLOW ONLY 32 - 255
	JB	PLGOFC		;FC ERROR IF TOO SMALL
	mov	B$BEATS,dl	; store beats per minute
	CLC
	RET

; Play a rest (Pause command)
PPAUSE:
	JNB	PLGOFC		;ERROR IF NO ARG
	XOR	CX,CX		;PASS FREQ OF 0
	CMP	DL,LOW 65	;ALLOW ONLY 1-64
	JNB	PLGOFC		;FC ERROR IF TOO BIG
	OR	DL,DL		;SEE IF ZERO
	JZ	PLYRET		;RETURN IF SO - NO PAUSE
	JMP	PPAUS2		;[DX]=PAUSE LENGTH

; Set the current octave number
POCTAV:
	JNB	PLGOFC		;ERROR IF NO ARG
	CMP	DL,LOW 7	;ALLOW ONLY OCTAVES 0..6
	JNB	PLGOFC		;FC ERROR IF TO BIG
	mov	B$OCTAVE,dl	
	CLC
PLYRET: RET

; Play a particular note by note number
PLYNUM:
	JNB	PLGOFC		;ERROR IF NO ARG
	MOV	AL,DL		;GET NOTE NUMBER INTO [AL]
	OR	AL,AL		;SEE IF ZERO (PAUSE)
	JZ	PLYNO3		;DO THE PAUSE
	CMP	AL,LOW 85	;ALLOW ONLY 0..84
	JNB	PLGOFC		;FC ERROR IF TOO BIG
	CBW			;CLEAR HI BYTE FOR DIVIDE
	DEC	AX		;MAP TO 0..83
	MOV	DL,LOW 12	;DIVIDE BY 12
	DIV	DL
	MOV	DH,AL		;OCTAVE TO [DH]
	MOV	AL,AH		;NOTE NUMBER IS REMAINDER
	INC	AL		;ADD ONE
	ADD	AL,AL		;DOUBLE TO MAKE INDEX
	JMP	SHORT PLYNU3	;PLAY NOTE [AL], OCTAVE [DH]

PLGOFC: JMP	B$ERR_FC	; GIVE FUNCTION CALL ERROR

; Play a note by name
PLYNOT: SUB	CL,LOW "A"-1	;MAP TO 1..7
	ADD	CL,CL		;MAP TO 2..14 (THIS ASSUMES SHARP)
	CALL	B$FETCHR 	;GET NEXT CHARACTER
	JZ	PLYNO2		;END OF STRING - NO SHARP OR FLAT
	CMP	AL,LOW "#"	;CHECK FOR POSSIBLE SHARP
	JZ	PLYSHP		;SHARP IT THEN
	CMP	AL,LOW "+"	;"+" ALSO MEANS SHARP
	JZ	PLYSHP
	CMP	AL,LOW "-"	;"-" MEANS FLAT
	JZ	PLYFLT
	CALL	B$DECFET 	;PUT CHAR BACK IN STRING.
	JMP	SHORT PLYNO2	;TREAT AS UNMODIFIED NOTE.
PLYFLT: DEC	CL		;DECREMENT TWICE TO FLAT IT
PLYNO2: DEC	CL		;MAP BACK TO UNSHARPED
PLYSHP: MOV	AL,CL		;INTO [AL] FOR XLAT
	MOV	BX,OFFSET NOTXLT ;POINT TO TRANSLATE TABLE
	XLAT	BYTE PTR CS:[BX] ; TRANSLATE INTO NOTE TABLE INDEX
	OR	AL,AL		;SEE IF LEGAL NOTE
	JS	PLGOFC		;NOTE'S OK IF NOT .GT. 127

; ENTER HERE WITH NOTE TO PLAY IN [AL]
; NOTE 0 IS PAUSE, 2,4,6,8..10,12 ARE A-G AND FRIENDS.

PLYNO3:
	mov	dh,B$OCTAVE	; get B$OCTAVE into [dh] for later math
PLYNU3:
	PUSH	AX		;Save Note
	PUSH	DX		; Save Octave
	MOV	AL,B$NOTELN
	MOV	B$NOTE1L,AL	; one note duration = note length
	CALL	B$FETCHR
	JZ	PLYNU4		;Brif end of string
	CALL	B$VALSC2 	;See if possible number
	CMP	DL,LOW 65	;If was .gt. 64
	JNB	PLGOFC		; then error
	OR	DL,DL		;Any Length?
	JZ	PLYNU4		;Brif not, just do note
	MOV	B$NOTE1L,DL	; store duration for this note
PLYNU4:
	POP	DX		;Get Octave
	POP	AX		;Restore Note
	CBW			;FILL [AH] WITH ZEROS
	MOV	BX,AX		;TRANSFER TO BX FOR INDEXING
	OR	BX,BX		;SEE IF PAUSE (NOTE # 0)
	JZ	PLYNO4		;IF PAUSE, PASS [BX]=0
	MOV	BX,WORD PTR CS:NOTTAB-2[BX] ;FETCH FREQUENCY
	MOV	CL,LOW 6	;CALCULATE 6-OCTAVE
	SUB	CL,DH		;FOR # OF TIMES TO SHIFT FREQ.
	SHR	BX,CL		;DIVIDE BY 2^(6-OCTAVE)
	ADC	BX,0		;ADD IN CARRY TO ROUND UP
PLYNO4:
	MOV	CX,BX		;FREQUENCY INTO [CX] FOR DONOTE
	MOV	DL,B$NOTE1L	; get this note's length
PPAUS2:
	MOV	AL,B$BEATS	; GET BEATS PER UNIT TIME
	MUL	DL		;CALC NOTE LENGTH * B$BEATS
	PUSH	CX		;SAVE [CX] WHILE WE DIVIDE
	MOV	CX,AX		;CALC TIME CONST/(B$BEATS * NOTE LENGTH)
	MOV	DX,1		;96000 (4*60*400) is
	MOV	AX,73400O	;SPECIAL TIME CONSTANT
	DIV	CX
	POP	CX		;RESTORE FREQUENCY
	OR	AX,AX		;IF DURATION IS ZERO, GET OUT.
	JZ	PLYNO8
	PUSH	CX		;Save Freq

PLYDOT:
	MOV	CX,AX		; Copy of duration for doted notes
PLYDOT1:
	PUSH	AX		; Save duration
	PUSH	CX		; Save the current dot duration
	CALL	B$FETCHR
	JZ	PLYDOX		; Brif EOS
	CMP	AL,LOW "."	; Note duration extender?
	JNZ	PLYDO2		; Brif not
	POP	CX		; Get last dot duration
	POP	AX		; Get current duration
	SHR	CX,1		; This dot = previous dot / 2
	ADD	AX,CX		; Update the new duration
	JNB	PLYDOT1 	; Loop if not overflow
	JMP	B$ERR_FC	; else complain.... (wont return)

PLYDO2:
	CALL	B$DECFET 	; Put char back
PLYDOX:
	POP	AX		; Trash the dot duration
	POP	AX		; Duration
	POP	CX		; Get freq
	PUSH	AX		;Save Duration
	PUSH	CX		;Save Frequency
	JCXZ	PLYNO7		;Brif Pause
	CMP	B$MSCALE,LOW 1	
	JZ	PLYNO7		;Brif Legatto
	MOV	CL,B$MSCALE	; using scale for shift count
	MOV	BX,3		;Stacatto multiplier
	CMP	CL,LOW 2
	JZ	PLYNO6		;Brif Stacatto
	MOV	BX,7		; else Normal
PLYNO6:
	MUL	BX		;Duration * 7/8 or 3/4
	SHR	AX,CL
	OR	AX,AX
	JNZ	PLYNO7		;If zero
	INC	AX		; then make 1

; Have all of the parameters for this note.  Send the info to the OEM
; to queue the note.
; Because a note is sent via two separate commands (one for the first,
; sound generating, part of the note, and a second one for the inter-
; note pause) it is possible for the queue to overflow in the middle of
; the note.  It isn't possible to simply wait for space to become available
; in the queue, because there is no guarantee that the queue is being
; emptied.  So to handle this case the following things happen:
;	If the queue overflows on either half of the note, the carry
;	flag is returned set as a signal to the music string B$PARSER that
;	the present command needs to be rescanned the next time around
;	If the first half of the note is sent successfully, a flag is
;	set (B$NOTFLG[SI]) indicating that it has been sent.  When the
;	second half of the note is sent successfully, then this flag is
;	cleared indicating that the entire note has been sent.	Before
;	sending the first half, it is necessary to check the flag to see
;	if this part has already been passed to the OEM on a previous
;	pass through the B$PARSER.
PLYNO7:
	POP	CX		;Get Freq
	CMP	B$NOTFLG,LOW 0 	; has the first part of this note already
				;been queued
	JNZ	PLYN7B		;If so, don't send it again
	cCALL	B$SNDNOT		;Send note
	JNB	PLYN7A		;If no queue overflowed, continue
	POP	AX		;If queue overflowed, get out, not even
	JMP	SHORT PLYNO9	; the first part of note was queued.

PLYN7A:
	mov	B$NOTFLG,low 1 	; set flag to say that note has been sent
PLYN7B:

; Now send an inter-note pause for this note (if required)
	POP	AX		;Get back original duration
	JCXZ	PLYNO8		;Brif Pause
	CMP	B$MSCALE,LOW 1	
	JZ	PLYNO8		;Brif Legatto
	MOV	CL,B$MSCALE	; scale factor for current mode (1/8|1/4)
	SHR	AX,CL		;divide note duration by scale factor
	OR	AX,AX		;Pause = 0?
	JZ	PLYNO8		;Don't send anything if so.
	cCALL	B$SNDPSN		;Send the rest
	JB	PLYNO9		;If queue overflowed, don't reset
				; flag for this note
PLYNO8:
	MOV	B$NOTFLG,LOW 0 	; clear the flag to indicate that the
				;complete note has been queued
PLYNO9:
	RET			; else do nothing

;***
; B$SNDNOT,B$SNDPSN
; Purpose:
; Send the specified note information to the OEM routine to be queued.
; B$SNDNOT will queue the first part of a note.
; B$SNDPSN will queue the second (inter-note pause) part of a note.
; Entry:
;	AX	- Duration
;	CX	- Frequency
; Exit:
;	none
; Modifies:
;	SI, DI, CX preserved.

; The registers must be set up as follows for the call to B$DONOTE
; [AL] = B$DONOTE function code number (1 for note, 0 for inter-note pause)
; [AH] = Voice
; [BX] = Volume
; [CX] = FREQUENCY IN HERTZ
; [DX] = DURATION IN CLOCK TICKS (1/18.2 SECONDS)

;****

cProc	B$SNDNOT,<NEAR>	
cBegin				

	XCHG	DX,AX		; B$DONOTE wants duration in DX

;	MOV	BX,B$VCEVOL	; Volume into bx

	MOV	AL,LOW QUENOT	;B$DONOTE function code in AL

; Test if this is the first time a note has been parsed in this play
; statement, and if so, queue sync marks in all voice queues.
	CMP	MQUEFL,LOW 0
	JNE	SDNT20		;branch if cmds have already been queued
				; for this PLAY statement
	MOV	MQUEFL,LOW 1


	JMP	SHORT SDNT20

cEnd	<nogen>			; End of B$SNDNOT


; Queue an internote pause (rest) for the current note

cProc	B$SNDPSN,<NEAR>	
cBegin				

	XCHG	DX,AX		; B$DONOTE wants duration in DX

	MOV	AL,LOW QUERST	;B$DONOTE function code in AL

; Send the instruction to the OEM, and test for any error codes coming
; back
SDNT20:
	cCALL	B$DONOTE 	; PLAY THE NOTE
	JNB	MQD90		;If no error occured, then go on
	CMP	AL,LOW 1	;Test for overflow on this voice
	JNE	PLYMER		;If not queue full, then report error
MQD80:
	STC			;Set error return state
MQD90:

cEnd				; End of B$SNDPSN


; Function call error occured while processing Music Meta command.
PLYMER:
	JMP	B$ERR_FC	; wont return

; B$PLYMET -	Process Music Meta Commands.

cProc	B$PLYMET,<NEAR>	
cBegin				

	CALL	B$FETCHZ 	;Get Meta action or error
	MOV	CL,LOW 1	;Factor for Legatto (1/1)
	CMP	AL,LOW "L"
	JZ	PLYDUR		;Brif Legatto (Full note)
	INC	CL		;Factor for Stecatto (3/4)
	CMP	AL,LOW "S"
	JZ	PLYDUR		;Brif Stecatto (3/4)
	INC	CL		;Factor for Normal (7/8)
	CMP	AL,LOW "N"
	JZ	PLYDUR		;Brif Normal (7/8)
	XOR	CL,CL
	CMP	AL,LOW "F"
	JZ	PLYMOD		;Brif Foreground Music
	DEC	CL
	CMP	AL,LOW "B"
	JNZ	PLYMER		;Brif not Background Music
PLYMOD:
	MOV	[B$MMODE],CL	;Store Music Mode (0=FG, 255=BG)
	JMP	SHORT PLYMET_RET	

PLYDUR:
	MOV	B$MSCALE,CL	; store duration scaling factor

PLYMET_RET:			; Common exit point

cEnd				; End of B$PLYMET


; This is the executive for dispatching the Q command.	It sets
; up the Macro command processor table to point to the Q subcommand
; table, and then uses the macro command processor to dispatch to
; the appropriate routine to process the subcommand.

; Envelope support code has been deleted from here


; TABLE OF INDEXES INTO NOTTAB FOR EACH NOTE
; VALUE OF 255 MEANS NOTE NOT ALLOWED.

NOTXLT	LABEL	BYTE		
	DB	9*2		;A- (G#)
	DB	10*2		;A
	DB	11*2		;A#
	DB	12*2		;B
	DB	255		;NO C- OR B#
	DB	1*2		;C
	DB	2*2		;C#
	DB	3*2		;D
	DB	4*2		;D#
	DB	5*2		;E
	DB	255		;NO E# OR F-
	DB	6*2		;F
	DB	7*2		;F#
	DB	8*2		;G
	DB	9*2		;G#

; TABLE OF NOTE FREQUENCIES
; THESE ARE THE FREQUENCIES IN HERTZ OF THE TOP OCTAVE (6)
; DIVIDED DOWN BY POWERS OF TWO TO GET ALL OTHER OCTAVES

NOTTAB	LABEL	WORD		
	DW	4186		;C
	DW	4435		;C#
	DW	4699		;D
	DW	4978		;D#
	DW	5274		;E
	DW	5588		;F
	DW	5920		;F#
	DW	6272		;G
	DW	6645		;G#
	DW	7040		;A
	DW	7459		;A#
	DW	7902		;B

;***
;B$QSYNC
;Purpose:
;	Output a Syncronization Byte to all voices
;Input:
;	none
;Output:
;	A SYNC byte is put in voice [VOICEN]'s queue
;Modifies:
;	none
;****

;	SYNC byte is output only if multi-voice support is present.
;	For single-voice music it is not necessary and hence not output.


sEnd	SN_TEXT 		

	END
