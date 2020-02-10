	PAGE	56,132
	TITLE	LLAEVT - GW-BASIC Event Trapping Interface
;***
; LLAEVT - GW-BASIC Event Trapping Interface
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;   ***********************************************************************
;
;	This module has advanced EVENT support routines:
;
;	B$RTIMR
;	B$RDPEN
;	B$RDPLAY	
;	B$RDSIG
;	B$RDMOUSE	
;	B$RDUEVENT	
;	CLOCKTIC  .....clock interrupt routine branches here after
;			updating time of day. This routine is used to
;			check for TIMER events, PEN status and TRIG
;			status.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	

	useSeg	_BSS		
	useSeg	_DATA		
	useSeg	EV		; Event handling segment(s)

	INCLUDE seg.inc 	
	INCLUDE ibmunv.inc
	INCLUDE idmac.inc	; debugging macros
	INCLUDE intmac.inc
	INCLUDE event.inc	; bit flag constants

sBegin	_DATA			

	externB	b$EventFlags	; misc event flags
globalW b$pJoystInt,NearRet,1	; Joystick interrupt handler

staticW	TIMRN1,-1		; stores n of ON TIMER(n) (4 bytes)
staticW	TIMRN2,-1		; start with -1 (no timeout) for OS/2

staticW	SleepCnt1,0		; number of clock ticks to wait before
staticW	SleepCnt2,0		; SLEEP statement times out


sEnd	_DATA			


sBegin	_BSS			

	externB b$PLAFLG	;LLSND - playing flag
	externB b$PLENBL	;LLSND - play enable flag
	externW b$PLYCNT	;LLSND - play note count
	externW b$SNDQCB	; Sound queue block



LPEN_MAX_ARG	EQU	9	; Maximum allowable arg value

LPEN_COORDS	STRUC		; Structure for LPEN land/touch values

LPEN_STATUS	DB	0	; Used as filler as PEN(0) does not require
LPEN_DUMMY	DB	0	; any data variable

X_LAND		DW	0	; Graphics X-coord when pen last landed
Y_LAND		DB	0	; Graphics Y-coord when pen last landed
Y_LAND_HI	DB	0	; Dummy entry for high byte

LPEN_ENABLE	DB	FALSE	; LPEN enable/disable flag - initial disable
LAND_FLAG	DB	FALSE	; These used since PEN(3) does not require
				; any data variable
X_TOUCH		DW	0	; Graphics X-coord when pen last touched
Y_TOUCH		DB	0	; Graphics Y-coord when pen last touched
Y_TOUCH_HI	DB	0	; Dummy entry for high byte
ROW_LAND	DB	1	; Character row when pen last landed
ROW_LAND_HI	DB	0	; Dummy entry 
COL_LAND	DB	1	; Character column when pen last landed
COL_LAND_HI	DB	0	; Dummy entry
ROW_TOUCH	DB	1	; Character row when pen last touched
ROW_TOUCH_HI	DB	0	; Dummy entry
COL_TOUCH	DB	1	; Character column when pen last touched
COL_TOUCH_HI	DB	0	; Dummy entry

LPEN_COORDS	ENDS

LPEN	LPEN_COORDS	<>	; Allocate space and initialize


TMRFLG	DB	1 DUP(?)	;timer evnt flag
TMENBL	DB	1 DUP(?)	;timer event enable flag
;
;	The timer used $TIMRSW as an one-second downcounter, which counted
;	down from 18.  Although the period of 18 clock interrupts were closed
;	to one second, there was about 0.0109 second difference, which was
;	based on 18.2 interrupts being one second.
;	Instead of using an one-second counter, current method uses the
;	interrupt counter directly.  When the high level code gets n from
;	the statement ON TIMER (n) ... and passes it to the low level code,
;	the n, in seconds, is multiplied by 18.2.  The result, which is
;	how many interrupts needed, is then stored in $TMCNT1 & $TMCNT2.
;
TMCNT1	DW	1 DUP(?)	;down counter
TMCNT2	DW	1 DUP(?)	;down counter is now 4 bytes





sEnd	_BSS			


sBegin	EV_TEXT 		
assumes CS,EV_TEXT		

externNP B$TrapEvent		;routine to set global event flag



externNP	B$SCNIO		;LLINI - perform screen BIOS call
externW 	b$BASDSG	;LLINI - BASCOM data segment
externD 	b$OldClkTic	;LLINI - saved INT 1CH vector

	SUBTTL	B$RDPEN - Read Light-Pen Coordinates and Trigger Flag.
	PAGE
;***
;B$RDPEN - Read Light-Pen Coordinates and Trigger Flag.
;OEM-interface routine
;
;Purpose:
;	This routine supports the light-pen.  It allows reading of
;	the coordinates of the pen, deturmining its status (touching
;	the screen or not touching the screen), and event support
;	for ON TRIGGER(n) GOTO.
;
;	See the documentation for B$POLLEV for a description of the
;	event and trapping mechanisms.
;
;Definitions:
;	The light pen is always in one of 2 states:
;
;	   "touching the screen" or "not touching the screen".
;
;	In the following text, the point at which a light pen last landed  on
;	the  screen  is defined as the point where the pen last changed state
;	from "not-touching the screen" to "touching the screen".   The	point
;	at which the pen last touched the screen is defined as the last point
;	at which the state of the pen was "touching the screen".
;
;Entry:
;	[AL] = which value to return in [BX]
;	    0:	 If PEN has touched screen since
;		 last call to B$RDPEN with [AL]=0 then return -1
;		 else return 0.
;	    1:	 Return graphic X-Coordinate where pen last landed
;		 on the screen.
;	    2:	 Return graphic Y-Coordinate where pen last landed
;		 on the screen.
;	    3:	 If PEN is currently touching screen
;		 then return -1, else return 0.
;	    4:	 Return graphics X-Coordinate where pen last
;		 touched the screen.
;	    5:	 Return graphics Y-Coordinate where pen last
;		 touched the screen.
;	    6:	 Return character line where pen last landed
;		 on the screen.
;	    7:	 Return character column where pen last landed
;		 on the screen.
;	    8:	 Return character line where pen last
;		 touched the screen.
;	    9:	 Return character column where pen last
;		 touched the screen.
;	    254: Enable light pen interrupt processing until B$RDPEN
;		 is called with [AL]=255.  This is to increase
;		 system performance.  It can be ignored by OEM.
;	    255: Disable light pen interrupt processing until B$RDPEN
;		 is called with [AL]=254.  This is to increase
;		 system performance.  It can be ignored by OEM.
;		 When the system is initialized, light pen interrupt
;		 processing can be disabled.
;
;Exit:
;	[BX]=return value as a signed 16-bit integer.
;
; Note	- The range of Graphics Coordinates is determined by the
;	  current graphics mode.  Character line and column coordinates
;	  are 1 relative.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;****

cProc	B$RDPEN,<PUBLIC,NEAR>	
cBegin				

	cCall	B$SetClockInt	; check if CLOCK_INT has been installed
				;if not install it
	CBW			; AH = 0/-1
	CMP	AL,LPEN_MAX_ARG	; Check for input value
	JBE	PROC_LPEN	; Brif if PEN function

LPEN_ON_OFF:			; Check if request is to turn it on/off
	INC	AX		; AX = -1/0 (254/255)
	MOV	LPEN.LPEN_ENABLE,AL ; Set the enable variable appropriately

;	Now, read the LPEN_STATUS once again and discard it. This is required
;	to avoid spurious results next time the light pen status is read
;	because the BIOS internal flag is cleared only when it is read

	XCHG	BX,AX		; Save it in BX
	cCALL	B$PENINT	; Still LPEN_STATUS is modified
	AND	LPEN.LPEN_STATUS,BL ; And change the status correctly
				; If enabled & active then remain active
				; else set to inactive
	JMP	SHORT LPEN_RET	; & Exit...

LPEN_3:				; Process PEN(3) function
	SCNIOS	vReadLightPen	; Read light pen status again (current val)
	SAHF			; If AH = 1 , then CF = 1 (pen active)
	XCHG	AX,BX		; Save BX (returned value) in AX
	SBB	BX,BX		; Set BX to 0/-1 as return value
	JZ	LPEN_RET	; If pen not active, simply return
	MOV	LPEN.X_TOUCH,AX	; Current X coordinate
	MOV	LPEN.Y_TOUCH,CH	; Current Y coordinate
	MOV	LPEN.ROW_TOUCH,DH	; Current row
	MOV	LPEN.COL_TOUCH,DL	; Current col
	JMP	SHORT LPEN_RET		; Exit ...

PROC_LPEN:			
				; AH = 0 here
;	If light pen is not enabled, then issue an error

;	CMP	LPEN.LPEN_ENABLE,AH	; Is it zero? (disabled)
;	MOV	BX,-1		; Prepare for error
;	JE	LPEN_RET	; Brif disable - CF = 1 for error

;	The function value requested is other than 0.
;	If it is 3, then handle it as a special case

	CMP	AL,03		; Is it special case?
	XCHG	AX,BX		; Get input arg in BX
	JE	LPEN_3		; If 3, then read the current lpen status
	SHL	BX,1		; Make it a word index into LPEN structure
	MOV	BX,[OFFSET DGROUP:LPEN+BX] ; Get the value requested
	JNZ	LPEN_RET	; If arg not zero then return the value
	MOV	LPEN.LPEN_STATUS,BH ; If arg = 0, then reset status to zero
	MOV	BH,BL		; Copy low byte for reporting

LPEN_RET:			; Time for returning

cEnd				; End of B$RDPEN



	SUBTTL	B$RDMOUSE - Read Mouse information
	PAGE


	SUBTTL	Timer Event Routines
	PAGE
;***
;B$RTIMR - Timer Event Trapping
;OEM-interface routine
;
;Purpose:
;	This routine supports timer event trapping and the
;	ON TIMER(n) function. This routine allows enabling and
;	and disabling the timer, setting the timer interval, and
;	polling if a timer event has occured.
;
;	A timer event will not be generated until the timer is enabled
;	and the timer interval has passed.
;
;	See the documentation for B$POLLEV for a description of the
;	event and trapping mechanisms.
;
;Entry:
;	[AL] = Function code
;		0: Return [BX] = -1 if event occured since last
;					call to B$RTIMR with [AL] = 0
;				  0 if event did not occur since last
;					call to B$RTIMR with [AL] = 0
;		1: Set timer interval to three byte integer [CL]:[DX]
;		254: Enable timer trapping
;		255: Disable timer trapping
;	[CL]:[DX] = three byte integer timer interval (in seconds)
;
;Exit:
;	[BX] = value if specified by function [AL]
;	PSW.C set indicates an error in the input parameters and will
;	      cause an Illegal Function Call.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;****
cProc	B$RTIMR,<NEAR,PUBLIC>
cBegin
	CALL	B$SetClockInt	; check if CLOCK_INT has been installed
				;if not install it
	CMP	AL,0		;is [AL] = 0?
	JE	CHKTMR		;check for timer event
	CMP	AL,1
	JE	SETIMR		;set timer interval
	INC	AL		; was it 255?
	JZ	STPTMR		; disable timer trapping
	INC	AL		; was it 254?
	JZ	STATMR		; enable timer trapping
	STC			;set carry to indicate function
	JMP	SHORT RDTRET	;call error and return
;
; See if timer event occurred. Returns BX as described above.
;
CHKTMR:
	XOR	BX,BX		; default is to report no timer event
	CMP	TMENBL,1	;is timer enabled?
	JNZ	RDTRET		; Brif not
	CMP	TMRFLG,1	;did timer event occur?
	JNZ	RDTRET		; Brif not
	DEC	BX		; else report it
	MOV	TMRFLG,0	;clear the timer event flag
	JMP	SHORT RDTRET
;
; Set the timer event to occur in CL:DX seconds.
;
SETIMR:
	CALL	SCTOIC		;[CX:DX] = n * factor (OS dependant)
	MOV	TMCNT1,DX	;[TMCNT1:TMCNT2] = time, in os dependant terms
	MOV	TMCNT2,CX	;	 CNT is count decremented by timer int
	MOV	TIMRN1,DX	;[TIMRN2:TIMRN1] = same
	MOV	TIMRN2,CX
	JMP	SHORT RDTRET
;
; Start the timer
;
STATMR:
	PUSH	BX
	MOV	BX,TIMRN1	;move n of ON TIMER(n)
	MOV	TMCNT1,BX	;to the down counter
	MOV	BX,TIMRN2	; changed to four bytes
	MOV	TMCNT2,BX
	POP	BX
	MOV	TMENBL,1	;enable trapping
	JMP	SHORT RDTRET
;
; Stop the timer
;
STPTMR:
	MOV	TMENBL,AL	; disable trapping
	MOV	TMRFLG,AL	; clear the timer event flag

RDTRET:

cEnd				

;***
;B$SetClockInt - Install CLOCKTIC interrupt if not already installed
;OEM-interface routine
;
;Purpose:
;	This routine checks to see if the CLOCKTIC has been
;	installed or not. If not installed it installs it.
;
;Entry:
;	None.
;
;Exit:
;	None.
;
;Uses:
;	Per convention
;
;Exceptions:
;	None.
;****
cProc	B$SetClockInt,<NEAR,PUBLIC>	
cBegin				
	TEST	b$EventFlags,TimerInst ; timer interrupt installed yet ?
	JZ	SET_IT		;Brif not -- install it now
NearRet:			; label to a near return
	RET
SET_IT:


	PUSH	AX
	PUSH	DX
	PUSH	DS
	IN	AL,MSKREG	;get IMR into AL
	OR	AL,01H		;mask out timer interrupt
	PAUSE
	OUT	MSKREG,AL	;write mask to IMR
	CLI			;Interrupts off during mod.
	push	bx
	push	es
	savint	cs:b$OldClkTic,timadr
	pop	es
	pop	bx
	PUSH	CS		;set [DS] = [CS]
	POP	DS
	SETVEC	TMRCTL,CLOCKTIC ;set clock interrupt
	POP	DS		;restore [DS]
	POP	DX
	OR	b$EventFlags,TimerInst ; set timer installed flag
	IN	AL,MSKREG	;get IMR into AL
	AND	AL,0FEH 	;unmask timer interrupt
	PAUSE
	OUT	MSKREG,AL	;write mask to IMR
	POP	AX		;restore [AX]
	STI			;restore interrupts

cEnd				



;***
;CLOCKTIC - The Timer Interupt vectors here AFTER updating the time-of-day.
;
;		CLOCKTIC polls the PEN Status (if ON) and stores
;		new values.  Then Joystick TRIGGERS (if ON) and
;		stores their values if fired since last time
;		read with the STRIG(0) or STRIG(2) Statement.
;
;	NOTE:	Rom clock interrupt routine saved [AX],[DX],[DS]
;
;	read the comment at the top of the file for detail.
;******************************************************************************
cProc	CLOCKTIC,<FAR>		
cBegin				
	PUSH	BP
	PUSH	DI
	PUSH	SI
	PUSH	DS
	PUSH	AX
	MOV	DS,CS:b$BASDSG ;Get DS addr for our vars.
	TEST	b$EventFlags,SLEEPtmr ; SLEEP timer on?
	JZ	NoWakeup	; brif not
	DEC	SleepCnt1	; decrement low word of SLEEP timer
	JNZ	NoWakeup	; brif not timeout/wrap
	DEC	SleepCnt2	; decrement high word of SLEEP timer
	JNS	NoWakeup	; brif not timeout
	AND	b$EventFlags,NOT (InSLEEP OR SLEEPtmr) ; break out of
				; SLEEP wait loop
NoWakeup:
	CMP	TMENBL,1	;is timer enabled?
	JNZ	TRYPEN		;Brif not
	DEC	TMCNT1		;decrement timer count
	JNZ	TRYPEN		;Brif not zero
	DEC	TMCNT2		;decrement the remaining byte
	JNS	TRYPEN		; Brif not negative
RESET:
	MOV	AX,TIMRN1	;reset the timer
	MOV	TMCNT1,AX	;count after counting
	MOV	AX,TIMRN2	; down the timer
	MOV	TMCNT2,AX	; it is word now
	MOV	TMRFLG,1	;set timer event flag
	MOV	AL,TIMOFF	; AL = trap number for B$TrapEvent
	CALL	B$TrapEvent	;set global event flag
TRYPEN:

	CMP	LPEN.LPEN_ENABLE,0 ; PEN ON?
	JE	TICTRG		; No, Try Trigger
	cCall	B$PENINT 	; Yes, Update Pen status.
TICTRG:

	CALL	b$pJoystInt	; process joystick interrupt (if present)
	POP	AX
	POP	DS
	POP	SI
	POP	DI
	POP	BP
	JMP	CS:b$OldClkTic ;go service the old interrupt 1C

cEnd	nogen			


;***
;SCTOIC -- translate number of seconds to number of interrupts
;
;Purpose:
; Converts the number of seconds to a factor as required by the operating
; system. Under DOS 2/3, we multiply [CL:DX] by 18.2 and return this number
; of clock ticks in [CX:DX]. Under DOS 5 we multiply [CL:DX] by 1000, and
; return this number of millesecodns in [CX:DX].
;
;  DOS3 NOTE: .2(n)=n/5, IDIV is used here.  Since max(n)=86400, the
;	      result will be a two-byte integer, and won't cause overflow.
;Entry:
;	[CL:DX] = number of seconds
;
;Exit:
;	[CX:DX] = number of interrupts needed
;
;Uses:
;	AX.
;****
cProc	SCTOIC,<NEAR>,<SI,DI>	
cBegin
	XOR	CH,CH
	MOV	AX,DX
	MOV	DX,CX		; store n in [DX:AX] (for IDIV)
	MOV	DI,AX
	MOV	SI,DX		; save n in [SI:DI]
	MOV	CX,5		; .2 is 1/5
	IDIV	CX		; get 0.2n in AX=[DX:AX]/[CX],
				; and remainder is in DX
	CMP	DX,2		; do we need to round ?
	JNA	NOADD1		; Brif don't
	INC	AX		; increment one
NOADD1:
	MOV	DX,DI
	MOV	CX,SI		; one more n in [CX:DX]
	CLC			; NC
	RCL	DX,1
	RCL	CX,1		; [CX:DX]=2*n
	PUSH	CX		; save CX
	MOV	CX,4		; loop count
MLOOP:				; multiple loop
	CLC			; no carry
	RCL	DI,1		; rotate left through carry
	RCL	SI,1
	LOOP	MLOOP		; when done, [SI:DI]=16*n
	POP	CX		; get back CX, so [CX|DX]=2*n
	ADD	DX,DI
	ADC	CX,SI		; [CX:DX]=18*n
	ADD	DX,AX
	ADC	CX,0		; [CX:DX]=18.2 * n (nearly)

cEnd				


;***
;B$SleepInit - set up timeout for SLEEP statement
;OEM-interface routine
;
;Purpose:
;
;Entry:
;	[DX|AX] = number of seconds to wait
;Exit:
;	[SleepCnt2|SleepCnt1] = number of clock ticks to wait
;Uses:
;	Per convention
;Preserves:
;
;Exceptions:
;	None.
;****

cProc	B$SleepInit,<NEAR,PUBLIC>
cBegin
	xchg	AX,DX			; [CL|DX] = # of seconds
	xchg	AX,CX
	call	SCTOIC			; [CX|DX] = # of clock ticks
	mov	SleepCnt1,DX		; update counter variables
	mov	SleepCnt2,CX
cEnd


	SUBTTL	PLAY
	PAGE
;***
;B$RDPLAY - play trapping and the PLAY function
;OEM-interface routine
;
;Purpose:
;	This routine supports play trapping and the play function.
;	A play event occurs when the background music queue shrinks
;	from n to n-1 notes (where n is specified in the ON PLAY(n)
;	statement). The event does not occur until PLAY trapping is
;	enabled and the background music queue has length greater
;	than n-1.
;
;	When PLAY trapping is enabled and the music queue shrinks
;	from n to n-1, the event flag and the play event flag are
;	both set.
;
;	See the documentation for B$POLLEV for a description of the
;	event and trapping mechanisms.
;
;Entry:
;	[AL] = Function code
;		0:Return [BX] = -1 if event occured since last
;				   call to B$RDPLAY with [AL] = 0
;				 0 if event did not occur since
;				   last call to B$RDPLAY with [AL] = 0
;		1:Return [BX] = number of notes in the queue
;			 [AH] = Voice Id (multivoice only)
;		2:Set event threshold to [BX], the number of notes
;			specified in the ON PLAY(n) statement
;		254: Enable PLAY trapping
;		255: Disable PLAY trapping
;
;Exit:
;	[BX] = value if specified by function [AL]
;	PSW.C set will cause a function call error
;	      to be declared
;
;Uses:
;	Per convention
;
;Preserves:
;	AX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$RDPLAY,<NEAR,PUBLIC>,<SI,AX>	
cBegin				

	CMP	AL,0
	JE	CHKPLY		;check for play event
	CMP	AL,1
	JE	RTNOTE		;return # of notes in queue
	CMP	AL,2
	JE	STNOTE		;save # of notes in ON PLAY(n)
	CMP	AL,254
	JE	STPLAY		;enable play trapping
	CMP	AL,255
	JE	SPPLAY		;disable play trapping
STNOTE_ERROR:			
	STC			;set carry to indicate function
PlayExit:			
cEnd				; restore registers & return

CHKPLY:
	XOR	BX,BX		; assume no event occured (BX = 0)
	CMP	b$PLENBL,0	;is play trapping enabled?
	JE	RDPRET		; Brif not enabled
	CMP	b$PLAFLG,1	;did play event occur?
	JNE	RDPRET		; Brif not
	DEC	BX		; return -1 in BX to report it
	MOV	b$PLAFLG,0	;clear the play event flag
	JMP	SHORT RDPRET

RTNOTE:
	MOV	BX,OFFSET DGROUP:b$SNDQCB	; get the addr of sound-block
	MOV	BX,[BX].QUNOTE	; Get the remaining notes in queue
	JMP	SHORT RDPRET
STNOTE:
	OR	BX,BX		;test if zero
	JZ	STNOTE_ERROR	;if so, then error
	CMP	BX,32D		;test if greater than 32
	JA	STNOTE_ERROR	;if so, then error
	MOV	b$PLYCNT,BX	;store # of notes specified in
	JMP	SHORT RDPRET	;ON PLAY(n) statement in PLYCNT
STPLAY:
	MOV	b$PLENBL,1	;enable trapping
	JMP	SHORT RDPRET
SPPLAY:
	MOV	b$PLENBL,0	;disable trapping
RDPRET:
	CLC			;clear carry to indicate no error
	JMP	SHORT PlayExit	; and return

;***
;B$PENINT -- PEN Update
;
;		Called by Timer Interupt AFTER updating the
;		time-of-day only if PEN or STRIG are ON.
;
;		CLOCKTIC polls the PEN Status (if ON) and stores
;		new values.  Then Joystick TRIGGERS (if ON) and
;		stores their values if fired since last time
;		read with the STRIG(0), STRIG(2), STRIG(4) or
;		STRIG(6) Statement.
;
;	NOTE:	Rom clock interrupt routine saved [AX],[DX],[DS]
;****

cProc	B$PENINT,<NEAR>,<BX,CX,DX,SI>	
cBegin					

	SCNIOS	vReadLightPen		; Read Lpen
	MOV	SI,OFFSET DGROUP:LPEN	; Point to LPEN structure
					; AH 01/00 if pen down/up
	SAHF				; CF = 1/0 if pen down/up
	SBB	AX,AX			; AX = -1/0 if pen down/up
	XCHG	AH,[SI].LAND_FLAG	; Get old LAND_FLAG in AH and
					; set it to new value
	JZ	PENINT_EXIT		; If pen not down nothing else to do

;	Now, set the lpen-coords to new values as return by BIOS

	ADD	DX,0101H		; Make row/col 1-relative
	MOV	[SI].X_TOUCH,BX		; Update the TOUCH variables
	MOV	[SI].COL_TOUCH,DL	; Hi-Byte will always ramin zero
	MOV	[SI].ROW_TOUCH,DH	;  --ditto--
	MOV	[SI].Y_TOUCH,CH		;  --ditto--
	CMP	AH,AL			; Any change in the pen down status?
	JE	PENINT_EXIT		; Brif not
					; Else, update the new LAND variables
	MOV	[SI].LPEN_STATUS,AL	; Set the current status as active
	MOV	[SI].Y_LAND,CH		; Hi-Byte will remain zero always
	MOV	[SI].COL_LAND,DL	;  --ditto--
	MOV	[SI].ROW_LAND,DH	;  --ditto--
	MOV	[SI].X_LAND,BX		
	MOV	AL,PENOFF		; AL = trap number for B$TrapEvent
	cCALL	B$TrapEvent

PENINT_EXIT:			; Time to exit

cEnd				; End of B$PENINT



	SUBTTL	SIGNAL processing code
	PAGE



sEnd	EV_TEXT
	END
