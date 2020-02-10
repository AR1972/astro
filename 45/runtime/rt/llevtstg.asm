	PAGE	56,132
	TITLE	LLEVTSTG - Low-level joystick support
;***
; LLEVTSTG - Low-level joystick support
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	_BSS
	useSeg	_DATA
	useSeg	EV_TEXT

	INCLUDE seg.inc
	INCLUDE ibmunv.inc
	INCLUDE idmac.inc	; debugging macros
	INCLUDE intmac.inc
	INCLUDE event.inc	; bit flag constants

sBegin	_DATA

TRIGSA	DB	0,TRIG_A1	;TRIGGER 'A1 Save Status & Mask
	DB	0,TRIG_B1	;TRIGGER 'B1 Save Status & Mask
	DB	0,TRIG_A2	;TRIGGER 'A2 Save Status & Mask
	DB	0,TRIG_B2	;TRIGGER 'B2 Save Status & Mask

sEnd	_DATA


sBegin	_BSS

	externB	b$MACHID	; machine identification

STIKAX	DB	4 DUP(?)	;STICK 'A' x,y - 'B' x,y Positions

sEnd	_BSS


sBegin	EV_TEXT
assumes CS,EV_TEXT


externNP B$TrapEvent		;routine to set global event flag
externNP B$SetClockInt		;routine to install clock interrupt


	SUBTTL	B$RDTRIG - Read Joystick Trigger Flag.
	PAGE

;***
;B$RDTRIG - Read Joystick Trigger Flag.
;OEM-interface routine
;
;Purpose:
;	Determines the status of a Joystick Trigger.  This routine
;	can return either the current status of the trigger (AH=0)
;	or a latched status (AH=1).  The latched status will indicate
;	if the joystick trigger has been pressed since the last time
;	this routine was called.
;
;	See the documentation for B$POLLEV for a description of the
;	event and trapping mechanisms.
;
;Entry:
;	[AL]=joy-stick trigger id (0..n)
;
;	[AH]=current/latched flag
;	     [AH]=0  request current status
;	     [AH]=1  request latched status
;
;Exit:
;	[AL]=0 if trigger is not pressed (current) or has not been pressed
;	       since last call to B$RDTRIG (latched).
;	[AL]=-1 if trigger is pressed (current) or has been pressed since last
;	       call to B$RDTRIG (latched).
;
;	PSW.C set indicates that there was an error, else PSW.C is clear.
;
;Uses:
;	per convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$RDTRIG,<NEAR,PUBLIC>
cBegin

	CALL	B$SetClockInt	; check if CLOCK_INT has been installed
				;if not install it
	PUSH	BX
	PUSH	CX
	PUSH	AX
	XOR	BX,BX
	MOV	BL,AL		;place i.d. in bx
	SHL	BX,1
	TEST	AH,1		;Read Status or Trigger?
	JNZ	$STF_S		;Brif Status (0, 2, 4 or 6).
	MOV	CH,TRIGSA[BX+1] ;Get Trigger Mask for read
	CALL	RTRGST		;Read Trigger A1,B1,A2 or B2 in [CL]
	MOV	AL,CL
	JMP	SHORT $STF_RET	;Return: -1 if fired, 0 if not.

$STF_S:
	MOV	AL,BYTE PTR TRIGSA[BX] ;Get Trigger A or B value.
	MOV	BYTE PTR TRIGSA[BX],0 ;Tell INT routine we read it.
				;Return: -1 if fired, 0 if not.
$STF_RET:
	POP	BX		;restore original ah
	MOV	AH,BH
	POP	CX
	POP	BX
	CLC			;carry clear since no error
cEnd				

;***
;B$RDSTIK - Read Joystick Coordinates
;OEM-interface routine
;
;Purpose:
;	Return either the X or Y coordinate of a joystick.  The coordinate
;	is a system dependent signed value, which may simply be -1, 0 or 1.
;
;Entry:
;	[AL]=Function to be performed as follows:
;	       0 = Return the x coordinate for joystick A.
;	       1 = Return the y coordinate for joystick A.
;	       2 = Return the x coordinate for joystick B.
;	       3 = Return the y coordinate for joystick B.
;	       n = If n is even then n is x coordinate for joystick (n+2)/2
;		     else n is y coordinate for joystick (n+1)/2.
;
;Exit:
;	PSW.C is set indicates an error ([AL] represents a joystick number
;		     that is too large)
;	PSW.C is reset otherwise
;
;	[BX] = Signed integer result.
;
;Uses:
;	Per convention.
;
;Preserves:
;	AX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$RDSTIK,<NEAR,PUBLIC>,<AX,CX,DX>
cBegin

	CALL	B$SetClockInt	; check if CLOCK_INT has been installed
				;if not install it
	MOV	BL,AL		;place offset in bx
	XOR	BH,BH		;clear high byte
	CMP	BX,4		;Out of Range?
	JNB	STICK_ERROR	;Yes, complain..
	CALL	RSTICK		;read stick
	MOV	BL,STIKAX[BX]	;get it
	XOR	BH,BH		;(clears carry)
	JMP	SHORT StickExit	; return with carry clear
STICK_ERROR:
	STC			; indicate error
StickExit:
cEnd				; restore registers & return

;***
;RSTICK -	Reads the 4 Potentiometers.  Typically, each
;		value will range from 0 to 200 depending upon
;		the R values of the Pots.
;
;Entry: 	None
;
;Exit:		STIKAX,AY = x,y Coordinates of Channel A.
;		STIKBX,BY = x,y Coordinates of Channel B.
;****

cProc	RSTICK,<NEAR>,<AX,BX,DX>
cBegin

	CMP	b$MACHID,0FCH	;is this a PC-AT?
	JE	AT_STICK 	; brif so
	MOV	DX,GAMECD	;Game Card
	MOV	CX,257		;Counter, adjusted by -2.
	MOV	BX,0FH		;Pot Mask.
	CLI			;No Interrups During Sample
	PAUSE
	OUT	DX,AL		;Strobe to start
SAMPL:
	PAUSE
	IN	AL,DX		;Get the Pot Values
	AND	AL,0FH		;Isolate Pot Value
	CMP	AL,BL		;Set ZF if no change.
	LOOPZ	SAMPL		;Loop till change or count exhausted.
	JCXZ	SAMPL1		;Brif count done, else got change.
	XOR	AL,BL		;[AL] = one(s) that changed state
	MOV	AH,CL		;[AH] = compl of count.
	PUSH	AX		;Save count/mask
	INC	BH		;Push count +1.
	XOR	BL,AL		;[BL] = New Mask.
	JMP	SHORT SAMPL
SAMPL1:
	ENABLE			;Interrupts back on
	OR	BH,BH
	JZ	SAMPLX		;Brif no Game Card.
	MOV	DL,BH		;[DL] = No. to POP.
SAMPL2:
	MOV	BX,OFFSET DGROUP:STIKAX ;Pot value save area.
	MOV	CX,4		;Shift count (4 pots).
	POP	AX		;Get count/mask.
	NOT	AH		;Compl for true count.
	ADD	AH,DL		;Adjust for time in sample loop.
SAMPL3:
	SHR	AL,1		;Has this Paddle changed State?
	JNB	SAMPL4		;Brif Not..
	MOV	[BX],AH 	;Yes, store current count
SAMPL4:
	INC	BX		;Point to next Location
	LOOP	SAMPL3		;Until all 4 done...
	DEC	DL		;No. to POP -1
	JNZ	SAMPL2		;Brif more Values.
SAMPLX:

cEnd

;	read joystick coordinates for PC-AT

AT_STICK:
	MOV	AH,84H		;Joystick support
	MOV	DX,1		;read joystick pots
	INT	21D
	MOV	STIKAX,AL	;A(x)
	MOV	STIKAX+1,BL	;A(y)
	MOV	STIKAX+2,CL	;B(x)
	MOV	STIKAX+3,DL	;B(y)
	JMP	SHORT SAMPLX

;***
;B$JoystInt  STRIG Update
;
;		Called by Timer Interupt AFTER updating the
;		time-of-day only if joystick code present.
;
;		CLOCKTIC polls the PEN Status (if ON) and stores
;		new values.  Then Joystick TRIGGERS (if ON) and
;		stores their values if fired since last time
;		read with the STRIG(0), STRIG(2), STRIG(4) or
;		STRIG(6) Statement.
;
;	NOTE:	Rom clock interrupt routine saved [AX],[DX],[DS]
;****

cProc	B$JoystInt,<NEAR,PUBLIC>,<BX,CX>
cBegin

	MOV	BX,OFFSET DGROUP:TRIGSA ;Look at Trigger 'A' status.
	MOV	CX,4		;no. of triggers to read
TRGIN2:
	CMP	BYTE PTR [BX],0 ;Trigger x been read?
	JNZ	TRGIN3		;No, try next Trigger
	PUSH	CX
	MOV	CH,[BX+1]	;Get Mask for this Trigger
	CALL	RTRGST		; Read Trigger 'A' or 'B'
	MOV	[BX],CL 	; Store value
	CMP	CL,-1		; has trigger been pressed?
	JNZ	NoTrig		; brif not
	MOV	AX,BX		; AL = trap number for B$TrapEvent
	ADD	AX,STGOFF*2 + OFFSET DGROUP:TRIGSA
	SHR	AL,1		
	CALL	B$TrapEvent	; set global event flag
NoTrig:				
	POP	CX
TRGIN3:
	INC	BX
	INC	BX		;Next trigger status loc.
	LOOP	TRGIN2		;Until all triggers tested

cEnd				; restore regs & return



cProc	RTRGST,<NEAR>,<DX>	
cBegin				
	CMP	b$MACHID,0FCH	;is this a PC-AT?
	JNE	PCTRIG		;br. if not
	MOV	AH,84H		;Joystick support
	XOR	DX,DX		;read joystick triggers
	INT	15h		; [AL] = trigger values
	JMP	SHORT ALLTRIG	;skip PC trigger read
PCTRIG:
	MOV	DX,GAMECD
	PAUSE
	IN	AL,DX		;Read Joysticks/Triggers
ALLTRIG:
	AND	AL,CH		;Mask to Trigger 'A' or 'B'
	DEC	AL		;-1 if down.
	CBW			; 0 if up.
	MOV	CL,AH		;Return in [CL]
cEnd				; restore regs & return


sEnd	EV_TEXT
	END
