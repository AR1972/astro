	TITLE	LLSND - multivoice music and sound interface
;***
; LLSND - multivoice music and sound interface
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;       This module has the following support routines :
;
;       1. B$DONOTE
;       2. B$TICTOC.... The TIMER interrupt vectors here before
;                       updating the time of the day. Normal
;                       frequency of the timer interrupt is 18.2
;                       times per second. This frequency is changed
;                       so that it interrupts at 572.4 times per
;                       second (exactly 32 times faster). This increase
;                       in speed gives better music performance. The
;                       basic philosophy in handling music is described
;                       below :
;
;               Music is composed of notes and a note consists of:
;               1. Frequency
;               2. Duration
;                       The frequency is loaded into the 8253 timer
;               chip. The duration is saved in some memory location and each
;		time the timer interrupts, we decrement the duration by 1 and
;               when the duration becomes zero we are all done. It is
;               important to note that the 8253 operates in the mode
;               wherein it reloads itself each time it counts down to
;               zero.
;               We have to remember one important thing, since we
;               change the frequency of the TIMER interrupt whenever
;               music is playing, we should see to it that the actual
;               timer interrupt (one which updates the time of the day)
;               gets called once every 32 times our timer interrupt
;               gets called. The figure 32 comes from the fact that
;               the TIMER is 32 times as fast as what it used to be.
;               Also when music stops playing we SHOULD change the
;               timer frequency back to its original rate.
;               The figure below illustrates how the TIMER interrupt
;               ( INT 8H ) is handled :
;
;                 Timer INT
;                  vectors
;                ------------        IF MUSIC      --------------
;               |     -------|------------------->| B$TICTOC      |
;                ------------    |     ACTIVE     |    Interrupt |
;               |    CS      |   |                |   service    |
;                ------------    |                |   routine for|
;                                |                |   handling   |
;                                |                |   music      |
;                                |                |     _________|______
;                                | ELSE IF         --------------       |
;                                | MUSIC NOT                            |
;                                | ACTIVE                               |
;                                |                                      |
;                                |                 ------------         |
;                                 --------------->| ROM Timer  |        |
;                                                 | interrupt  |<-------
;                                                 | service    |
;                                                 | routine    |
;                                                 | updates    |
;                                                 | time of    |
;                                                 | day.       |
;                                                 |            |
;                                                 | (F000:FEA5)|
;                                                 |            |  INT 1CH
;                                                 |  -------------------
;                                                 |            |        |
;                                                  ------------         |
;                                                                       |
;                                                                       |
;                                                  ------------         |
;                                                 | CLOCK_INT  |        |
;                                                 |   This     |<-------
;                                                 |  routine   |
;                                                 |  checks for|
;                                                 |  timer, pen|
;                                                 |  and TRIG  |
;                                                 |  traps.    |
;                                                 |            |    JUMP TO
;                                                 |    --------|------------->
;                                                  ------------     OLD 1CH
;                                                                   VECTOR
;
;               The routine B$TICTOC calls on the following routines:
;
;               1. B$NXTSND.... This looks to see if any more entries
;                             are present in the sound queue and if so
;				gets the frequency and duration information,
;				and starts that note. It also check if any play
;                             event occured and if so sets the PLYFLG
;                             and the EVTFLG.
;
;******************************************************************************
	INCLUDE	switch.inc	;switch file
	INCLUDE rmacros.inc	

	UseSeg	_BSS		
	UseSeg	_DATA		
	UseSeg	EV_TEXT 	


	INCLUDE seg.inc 	
	INCLUDE	idmac.inc	
	INCLUDE	ibmunv.inc	; include control block & DONOTE input values
	INCLUDE	intmac.inc	
	INCLUDE event.inc 	

CLK_DELTA	EQU	8	; 8 = 256/32
QUE_BYTES	EQU	1+2+2	; Note + Duration + Frequency

sBegin	_BSS			

	globalB b$PLAFLG,,1	;play event flag
	globalB b$PLENBL,,1	;play trapping enabled or not flag
	globalW b$PLYCNT,,1	;# of notes specified in ON PLAY(n)
	globalW b$NOTES,,1	;keeps running total of the music notes
	globalW b$SNDTIM,,1	

	PUBLIC	b$SNDQCB	; Sound control block
b$SNDQCB QUE_CTRL_BLOCK <>	

	externB b$MUSIC		; defined in LLSCNIO.ASM
	externW b$SNQueSeg	;SNINIT - sound queue buffer segment

	staticB b$SNDFLG,,1	;flag set if music started
	staticB CLK_TICS,8,1	;Clock tic modulo counter at any time
	externW b$pTrapEvent	; pointer to B$TrapEvent if event code
				; present

sEnd	_BSS			


assumes CS,EV_TEXT		
sBegin	EV_TEXT 		


	externW  b$BASDSG	
	externNP B$INIQUE	
	externNP B$QUE 		
	externNP B$DQUE		

POPFF	MACRO			;; macro to insure that POPF instruction
LOCAL	L1,L2			;; is executed correctly on 286. It seems

	JMP	SHORT L2	;;to enable interrupts after POPF instruction
L1:	IRET			;; even if the stack image says 'disabled'
L2:	PUSH	CS
	CALL	L1

	ENDM

;***
;B$DONOTE - Support for sound
;OEM-interface routine
;
;Purpose:
;	This routine supports speaker activity.  The support is subdivided
;	into several subfunctions as follows:
;
;	a. Queue a note:
;          This function is passed a frequency and a duration. The note is
;	   queued, if the frequency is legal, otherwise nothing it put in
;	   in the queue. No attempt to start the speaker is made.
;
;	   The definition of a legal frequency are machine dependent.  For
;	   an IBM, a frequency must be within the range 37 <= f <= 32767.
;
;	b. Queue a rest:
;          This function is passed a duration. Stacatto and Normal mode notes
;	   are built from a note and a short pause. This function is used
;	   to play the fraction of a note that defines a short pause in case
;          of Stacatto and Normal notes.
;
;	c. Query if voice is active:
;          This function is used to test the current activity of the
;          sound queue. If queued items are currently being processed
;          in the sound queue, this function code should return
;          FF in [AL], else it should return 0 in [AL].
;
;	d. Start Music:
;	   This function requests that notes that have been queued are
;	   to start playing. Notes should be played until the queue is
;	   empty. This function can be called if notes are currently
;	   playing, in which case it simply returns.  Notes may be added
;	   to the queue while it is being played. No event other than
;	   this subfunction should start music.
;
;	e. Stop Music and flush the queue:
;          This function requests that the voice be stopped and that
;          queue for the voice be flushed.
;
;
;       Certain of the above functions must add queue entries. If no room
;       remains in the queue then the routine must return immediately to
;       specify that the queue is full. This event must not cause speaker
;       activity to begin.
;
;Entry:
;       [AL] = Function number
;               0: Queue rest
;                       [DX] = Duration ( 1 = 2.5 milliseconds )
;               1: Queue note
;                       [CX] = Frequency in Hz
;                       [DX] = Duration ( 1 = 2.5 milliseconds )
;               2: Query if voice is active
;                       Return in [AL]
;                         0 - voice is not currently active
;                        FF - voice is active
;              FE: Start music
;              FF: Stop music and flush queue
;
;Exit:
;       PSW.C set indicates that [AL] contains return information
;	[AL] =	1 - No room in the queue for this request
;		2 - Illegal function request
;               3 - Unsupported frequency
;	PSW.C reset indicates successful completion.
;
;
;Uses:
;	Per convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****
;
;ALGORITHM:
;	case [AL] of
;	0,1 : begin
;		B$QNOTE (AX,CX,DX)
;		/* queues either the note or the rest */
;		end
;	  2 : B$QUERY
;		/* checks if the voice is active */
;	  3 : begin	/* in practice, this is ignored */
;		B$QSYNC()
;		end
;	4,5 : ENVELOPE SHAPE/DURATION
;		/* These functions are not supported */
;         6 : $MKNSE /* this function is not supported */
;         FC : SOUND ON/OFF /* this function is not supported */
;         FD : BEEP ON/OFF /* this function is not supported */
;         FE : B$QSTART
;              /* starts music */
;         FF : B$QFLUSH
;             /* stops music and flushes the queues */
;       endcase
;

DONOTE_TABLE	LABEL	WORD	; Dispatch table

	DW	B$QSTART	; start the music
	DW	B$QFLUSH	; stop the music & flush the queue
	DW	B$QNOTE	; queue a rest
	DW	B$QNOTE	; queue a note
	DW	B$QUERY	; query if voice is active or not

MAX_DONOTE	EQU	($ - DONOTE_TABLE)/2 ; Permissible entry value in AL
	
cProc	B$DONOTE,<PUBLIC,NEAR>,<BX,CX,DX,SI>	
cBegin				

	MOV	BX,OFFSET DGROUP:b$SNDQCB ; get addr of music queue in BX
	CMP	b$SNDFLG,0	;initialization been done?
	JNZ	DONOT1		;jump if already initialized
	INC	b$SNDFLG	;mark as initialized

	PUSH	AX
	XOR	AX,AX		;music queue starts at zero
	MOV	[BX].QUEBOT,AX	; set the bottom
	cCALL	B$INIQUE	; set put, get, num, note..
	MOV	AX,QLENTH	;    get length
	MOV	[BX].QUELEN,AX	; and set it in the block
	MOV	[BX].QUETOP,AX	;    to set the top of queue.
	POP	AX

DONOT1:
	CBW			; AH = 0/-1
	MOV	SI,AX		; SI will be used to point to the dispatcher
	INC	SI		; Add 2 to SI to make it 0-relative
	INC	SI		
	CMP	SI,MAX_DONOTE	; Check if arg within range
	JA	DONOTE_ERR	; Brif no - issue error

	SHL	SI,1		; Make it a word index
	CLD
	cCALL	DONOTE_TABLE[SI] ; Invoke respective handler
	JNC	DONOTE_EXIT	; No error - else error code is in DL
	XCHG	AX,DX		; Else get error code in AL

DONOTE_EXIT:			; Common exit point

cEnd				; End of B$DONOTE

DONOTE_ERR:			; Error case
	MOV	AL,2		; Issue voice-id invalid
	STC			; CF = 1 indicates error
	JMP	SHORT DONOTE_EXIT ; Exit...

	PAGE

;***
;B$QNOTE
;
;PURPOSE:
;       This routine queues either a note or a rest. By rest we mean
;       the short pause that goes to make Stacatto and Normal notes.
;       This routine calls a number of device dependent routines. These
;       device dependent routines check for the validity of frequency and
;       duration. They also output these quantities to the 8253 timer chip.
;       It either queues in the whole note or does'nt queue at all.
;
;ENTRY:
;	[BX] = QUEUE BLOCK BASE ADDR
;       [CX] = Frequeuncy
;       [DX] = Duration
;       [AL] = 0 OR 1 depending on whether it is a rest or
;               a music note respectively.
;
;EXIT:
;	[DL] = Error codes as specified by the routine B$DONOTE
;
;MODIFIED:
;       None
;
;****
cProc	B$QNOTE,<NEAR>,<AX,ES>	

localV	QBLK,QUE_BYTES 		; This block is used to temporarily
				; queue the note as it is processed
cBegin				

	OR	AL,AL		; was it a note ?
	JNZ	QNOTE		; brif so
	SUB	CX,CX		; Else queue in very high frequency

QNOTE:
	CMP	[BX].QUENUM,QLENTH-QUE_BYTES	; Is there enough space?
	JA	QUE_ERR1	; Brif not
	LEA	SI,QBLK 	; [si] = address of qblk
	MOV	BYTE PTR [SI],AL; temporarily queue the attribute

CHECK_DURATION:

;       [DX] = duration (1 = 2.5 millisecs)
;       [AX] = [AX] * 1.5
	
	MOV	AX,DX		;copy duration for adjustment
	SHR	AX,1		;divide by 2
	ADD	AX,DX		;duration = 1.5 * original duration
	JNC	DUROK		;Brif no overflow
	SBB	AX,AX		; else use maximum duration

DUROK:
	MOV	WORD PTR [SI+1],AX ; temporarily queue the duration

CHECK_FREQ:

; check for valid frequency and convert it to tics

	CMP	CX,37D		;check for valid frequency (37 <= CX <= 32767)
	JGE	FRQOK		; Brif in range
	MOV	DL,3		; Error code in AL (if needed)
	INC	CX		; If zero, then set it to some high value
	LOOP	QUE_ERR2	; Brif not zero - Error in freq value

FRQ_0:
	NOT	CX		; CX = -1 ; Play an inaudible frequency

FRQOK:				; Convert frequency to tics
	MOV	AX,34DCH	; 1.193180 MHz
	MOV	DX,12H		
	DIV	CX		; [AX] = COUNT = CLOCK/FREQUENCY
	CLC			; CF = 0

	MOV	WORD PTR [SI+3],AX ; temporarily queue the frequency
	MOV	ES,b$SNQueSeg	;music queue has its own buffer segment
	MOV	CX,QUE_BYTES	; 5 bytes to be queued
	CLI			;CLI,bcos this is an indivisable operation

QUETHEM:			;[si] = address of QBLK

.erre	ID_SSEQDS		; Assert SS = DS

	LODSB			; [al] = byte to be queued
	cCALL	B$QUE		; queue the byte
	LOOP	QUETHEM

	MOV	AL,BYTE PTR QBLK ; get the note/rest (1/0)
	CBW			; AH = 0
				; For rest, the following additions are NOPs
	ADD	[BX].QUNOTE,AX	; Suitably update # of notes in queue
	ADD	b$NOTES,AX	  ; and total note count
	CLC			; Clear carry to indicate no error

QUERET:
	STI			;restore interrupts

cEnd				; End of B$QNOTE

QUE_ERR1:
	MOV	DL,1		; error code in [al]
QUE_ERR2:
	STC			; indicate error
	JMP	SHORT QUERET

	PAGE

;***
;B$QUERY
;
;PURPOSE:
;       Checks to see if any voices are active
;
;ALGORITHM:
;	If the timer is turned on of off frequently, then an irritating
;	'click' sound is heard between two play statements. In order to
;	avoid this 'click', when (b$NOTES | b$SNDTIM) > 2 then say that
;	music is inactive. This lets Hi-Level to queue the next note
;	and the speaker is not turned off in-between.
;
;ENTRY:
;       None
;
;EXIT:
;       [AL] = 0 if voice is not currently active
;              FF if voice is active
;
;MODIFIED:
;       None
;
;****
cProc	B$QUERY,<NEAR>	
cBegin				

	MOV	AX,b$NOTES	; Get number of notes (may be zero)
	OR	AX,b$SNDTIM	; Add any music-ticks still left
	CMP	AX,2		; If zero or 1 then zero else -1
	CMC			; CF =1 if AX > 2
	SBB	AL,AL		; AL = -1 if speaker active
	CLC			; No error for this call


QUERY_RET:			; Just exit...

cEnd				; End of B$QUERY


	PAGE

;***
;B$TICTOC
;
;PURPOSE:
;       The timer interrupt, vectors here BEFORE updating the
;       time of the day. This routine helps support the
;       Music routines. B$TICTOC keeps decrementing the duration
;       count until it becomes zero at which point it
;       either gets the next sound from the sound
;       queue or else it turns off the voice by calling B$QFLUSH.
;ENTRY:
;       None
;
;EXIT:
;       None
;
;MODIFIED:
;       None
;
;****

cProc	B$TICTOC,<NEAR>	
cBegin				

	STI			; Enable interrupts now itself - there
				; is no chance of another timer intterupt
				; occurring since EOI is sent only later.
	PUSH	AX
	PUSH	BX
	PUSH	CX		; Save temp registers AX,BX,CX
	PUSH	DX		; Save this also
	PUSH	DS
	PUSH	ES

	MOV	DS,CS:b$BASDSG ;get BASIC's data seg
	MOV	ES,b$SNQueSeg	;point to music buffer

	XOR	AX,AX		; Get ready for some zero comparisons
	CMP	b$SNDTIM,AX	; has SND_TIM zeroed out?
	JZ	NXTONE		;Brif so to get next sound
	DEC	b$SNDTIM	; SND_TIM - 1
	JNZ	CLK_TIC
NXTONE:
	MOV	BX,OFFSET DGROUP:b$SNDQCB ; BX = address of music queue	
	CMP	[BX].QUENUM,AX	; Is the queue empty
	JE	TURN_OFF	; If so, then turn music off
	cCALL	B$DQUE		; Get first entry in queue
	OR	AL,AL		; Check if it is note or rest
	JZ	GETSND1		; Brif rest : no need to check play trap
				; AL = 1 for note
	CMP	b$PLENBL,AL	; Check if play trapping is enabled
	JNE	GETS1		; Brif disabled
	MOV	AX,b$PLYCNT	; Get the number of notes set for trapping
	CMP	[BX].QUNOTE,AX	; and compare it with current # of notes
	JNE	GETS1		; Brif event has not occurred
	MOV	b$PLAFLG,1	; Else set the flag
	MOV	AL,PLAOFF	; AL = trap number for B$TrapEvent
	CALL	[b$pTrapEvent]	; set the global flag if events linked in

GETS1:
	DEC	[BX].QUNOTE	; One more note has been played
	DEC	b$NOTES	; update overall count also

GETSND1:
	cCALL	B$DQUE		; Get the duration LSB
	XCHG	AX,CX		; CL = LSB
	cCALL	B$DQUE		; Get the duration MSB
	MOV	CH,AL		; CH = MSB
	MOV	b$SNDTIM,CX	
	cCALL	B$DQUE		; Get the frequency LSB
	OUT	TIMER2,AL	; & set the timer counter Lo-byte
				; 8253 will wait until hi-byte is loaded.
	cCALL	B$DQUE		; Get the frequency MSB
	OUT	TIMER2,AL	; & set the timer counter Hi-byte
	JMP	SHORT CLK_TIC	; All done

TURN_OFF:			; Turn off the music
	PUSH	DS		; B$FLUSH requires ES = DS
	POP	ES		
	cCALL	B$QFLUSH	

CLK_TIC:
	SUB	CLK_TICS,CLK_DELTA ; clock tick  -= CLK_DELTA
				; wait for 32 interrupts to invoke ROMCLK
	POP	ES
	POP	DS
	POP	DX		
	POP	CX
	POP	BX
	JNZ	CLKTIX		;don't do ROM clock INT now
	POP	AX
	INT	ROMCLK		;to ROM clock INT service routine
	IRET			

CLKTIX:
	MOV	AL,EOI		; send End-of-Interrupt
	OUT	INTA0,AL	; to 8259
	POP	AX		
	IRET

cEnd	<nogen>			; End of B$TICTOC

	PAGE

;***
;B$QSTART
;
;PURPOSE:
;       This routine starts the music. It does the following
;       things:
;       1. Change the interrupt vector to point at our handler
;       2. Modify timer2 to interrupt 32 times faster
;       3. Turn on the speaker and start timer2 only if timer2
;          is active.
;
;ENTRY:
;       None
;
;EXIT:
;       None
;
;MODIFIED:
;       None
;
;****
cProc	B$QSTART,<NEAR>,<AX>	
cBegin				

	MOV	AH,01		; Comes useful
; DON'T reset the counter every time: only once at program load.  This is
; a modulo counter that should be retained.
;	MOV	CLK_TICS,CLK_DELTA	; Reset the counter
	PUSHF			; Save caller's IF
	CLI
	IN	AL,MSKREG	;get IMR into [AL]
	OR	AL,AH		;mask out timer interrupt
	PAUSE			;make sure instruction fetch has occurred
	OUT	MSKREG,AL	;write mask to IMR

	XCHG	b$MUSIC,AH	; get b$MUSIC flag and set it to 1
	OR	AH,AH		; Check if music is already on
	JNZ	STRTMXT		; Brif so - timer is already changed

	PUSH	DS
	PUSH	CS
	POP	DS		;[DS] := [CS]
	SETVEC	CLKINT/4,B$TICTOC ;modify timer2 interrupt vector
	POP	DS
	MOV	AL,0		;modify timer2 to interrupt
	OUT	TIMER0,AL	;at 32 times the
	MOV	AL,8H		;original
	PAUSE			;make sure instruction fetch has occurred
	OUT	TIMER0,AL	;rate
	MOV	AL,SQUARE	;else set timer2 in square
	PAUSE			;make sure instruction fetch has occurred
	OUT	TMRCMD,AL	;wave mode
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,SPEAKER	;turn on the
	OR	AL,SPKRON	;speaker
	PAUSE			;make sure instruction fetch has occurred
	OUT	SPEAKER,AL
	PAUSE			;make sure instruction fetch has occurred

STRTMXT:
	IN	AL,MSKREG	;get IMR into [AL]
	AND	AL,0FEH		;unmask timer interrupt
				; CF = 0 to indicate no error
	PAUSE			;make sure instruction fetch has occurred
	OUT	MSKREG,AL	;write mask to IMR
	POPFF			; Restore caller's IF

cEnd				; End of B$QSTART

;***
;B$QFLUSH
;
;PURPOSE:
;	This routine stops music, initializes the music block, resets the timer
;	count, restores the original timer ISR and clears the flag b$SNDTIM
;
;ENTRY:
;	None
;
;EXIT:
;	None
;
;MODIFIED:
;	None
;
;****
;
cProc	B$QFLUSH,<NEAR>,<AX>	
cBegin				

	PUSHF			; Save caller's IF
	CLI
	IN	AL,MSKREG	; get IMR into [AL]
	OR	AL,01H		; mask out timer interrupt
	PAUSE			; make sure instruction fetch has occurred
	OUT	MSKREG,AL	; write mask to IMR
	cCALL	B$SNDOFF	
	JMP	STRTMXT		; Share code

cEnd	<nogen>			; End of B$QFLUSH

; B$SNDOFF moved here from LLQUE.ASM to increase /O modularity
; revision [6] applies to the entire routine:

;***
;B$SNDOFF - Turn off Sound and Clean Up
;OEM-interface routine
;
;Purpose:
;	This routine stops music and flushes the music queue(s).
;	It also performs all needed functions to disable sound.
;	B$SNDOFF is called before the RUN command is executed
;	and at program termination.
;
;	The sound queue is allocated and deallocated by the
;	runtime, so this routine need not worry about it.  The
;	queue will exist when this routine is called.
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
;***************************************************************************

DB	"<LEO>"			; This is used via a separate tool to
				; actually modify the .EXE file to stub
				; out B$SNDOFF. This is necessary for
				; profiling, as B$SNDOFF modifies the
				; timer hook, as does the profiler.
				; (chosen string pure vanity on Leo's part)

cProc	B$SNDOFF, <PUBLIC,NEAR>,<AX,BX>	
cBegin					
	XOR	AX,AX		;zero out [AX]
	MOV	b$NOTES,AX	; zero out b$NOTES
	MOV	b$SNDTIM,AX	; SND_TIM = 0
	MOV	b$MUSIC,AL	;music is currently OFF
	MOV	BX,OFFSET DGROUP:b$SNDQCB ; Get music block offset
	cCALL	B$INIQUE 	; init music queue at zero offset
	
	IN	AL,SPEAKER
	AND	AL,NOT SPKRON	;turn off speaker
	PAUSE			;ensure instruction fetch has occurred
	OUT	SPEAKER,AL
	XOR	BX,BX		; CF = 0 to indicate no error
	PUSH	DS
	MOV	DS,BX		; [DS] = interrupt vectors segment
	PUSHF			; Save flags
	CLI			;make sure interrupts are off
	SVINT	DS:CLKINT,DS:CLKVEC
	POPFF			; Restore flags
	POP	DS
	XCHG	AX,BX		; AX = 0
	OUT	TIMER0,AL	;restore timer2 count
	PAUSE			;ensure instruction fetch has occurred
	OUT	TIMER0,AL

cEnd				; End of B$SNDOFF

sEnd	EV_TEXT 		
	END
