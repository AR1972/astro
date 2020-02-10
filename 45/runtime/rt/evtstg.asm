	TITLE	EVTSTG - Joystick Trigger (STRIG) event handling
;***
; EVTSTG - Joystick Trigger (STRIG) event handling
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Event trapping support for joystick trigger (STRIG)
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; STRIG(n) Statement - 3 syntax variants map to 3 entry points:
;
;      STRIG(n) ON	  STRIG(n) OFF	    STRIG(n) STOP
;      -----------	  ------------	    -------------
;	    |			|		   |
;	 B$ETS0		     B$ETS1		B$ETS2
;
; STICK Function:
;
;      v = STICK(n)
;	     |
;	B$STIK
;
;
; STRIG Function:
;
;      v = STRIG(x)
;	     |
;	B$FSTG
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	EV_TEXT
	useSeg	ER_TEXT
	useSeg	_DATA
	useSeg	XIB
	useSeg	XI
	useSeg	XIE

	INCLUDE seg.inc
	INCLUDE event.inc
	INCLUDE baslibma.inc
	INCLUDE idmac.inc

	INITIALIZER	B$xSTGTRAPINIT

sBegin	_DATA
externW	B$PollDispTable
externW	b$pJoystInt
sEnd	_DATA

sBegin	ER_TEXT
externNP B$ERR_FC
sEnd	ER_TEXT

sBegin	EV_TEXT
assumes CS,EV_TEXT

externNP B$RDTRIG
externNP B$RDSTIK	
externNP B$TestTrap
externNP B$ReqTrap
externNP B$EVNT_SET
externNP B$settbl
externNP B$ONFUN
externNP B$JoystInt

	SUBTTL	STRIG Traps

;***
;B$POLSTG - Poll for a STRIG event.
;
;Purpose:
; POLSTG is called by CHKINT at beginning of every BASIC statement (NEWSTT).
; If a STRIG interrupt has occured, it sets the appropriate bit in b$TRPTBL
; which will cause the BASIC program's pen service routine (ON STRIG(N) GOSUB)
; to be invoked.
;
;Entry:
;	 None.
;
;Exit:
;	 AX, BX, CX, DX can be used (restored by CHKINT).
;	 All other registers are preserved.
;
;Uses:
;	 Per Convention
;
;Exceptions:
;	 None.
;****

cProc	B$POLSTG,<NEAR>
cBegin
	MOV	AX,100H 	; [AL] = joystick trigger #0 (for B$RDTRIG)
				; [AH] = latched (not current) flag

STGILP:
	PUSH	AX		; save current trigger id
	ADD	AL,STGOFF
	CALL	B$TestTrap	; [BX] points to event mask PSW.Z if
	POP	AX		;  STRIG(n) <ON|STOP> has been done
	PUSH	AX
	JZ	STGI1		; don't call B$RDTRIG if event not enabled
	CALL	B$RDTRIG	;  [AL]=0/1 if trig is not-pressed/pressed
	OR	AL,AL
	JZ	STGI1		; brif this trigger has not interrupted
	POP	AX		; restore [AL] = joystick trigger id
	PUSH	AX
	CALL	B$ReqTrap	; Signal the occurance of a TRIGGER EVENT

STGI1:	POP	AX
	INC	AL
	CMP	AL,NUM_JOYST
	JB	STGILP		;branch if more triggers to poll
cEnd

	SUBTTL	STRIG [ON,OFF,STOP] Statement
	PAGE

;***
; B$ETS0, B$ETS1, B$ETS2 - STRIG (n) [ON,OFF,STOP] Statement
;
; Purpose:
;	Dispatch STRIG statement depending upon following clauses:
;
;	STRIG (n)	ON	Enable	STRIG(n) Trapping.
;	STRIG (n)	OFF	Disable STRIG(n) Trapping.
;	STRIG (n)	STOP	Suspend STRIG(n) Trapping.
;
;	WHERE:
;		(n) is Trigger 0 or 2.
; Input:
;	parm1 == (n)
; Output:
;	Event set in trap table, or error
;****

labelFP	<PUBLIC,B$ETS0>		; STRIG(n) ON
	MOV	DL,$ON
	SKIP	2
labelFP	<PUBLIC,B$ETS1>		; STRIG(n) OFF
	MOV	DL,$OFF
	SKIP	2
labelFP	<PUBLIC,B$ETS2>		; STRIG(n) STOP
	MOV	DL,$STOP

cProc	STRIG_PROC,<FAR>,<SI,ES>
parmW	strigParm
cBegin
	MOV	BX,[strigParm]
	ROR	BL,1		;Divide by 2 so get 0,1,2 or 3
	CMP	BX,NUM_JOYST	;rolling BX sets high order bit
	JNB	JOYST_ERROR	;Brif .gt. MAX

	cmp	dl,$ON		; rdtrig must be called to ensure
	jnz	ts_cnt		; that an event occurring while

	mov	al,bl		; strig(n) was turn off doesnt
	mov	ah,1		; generate a false event or prevent
	call	B$RDTRIG	; detection of future events
ts_cnt: MOV	CL,STGOFF
	xor	si,si		; no low-level to start trapping

	CALL	B$EVNT_SET	;Set Event
cEnd

JOYST_ERROR:
	JMP	B$ERR_FC	; give Illegal function call error

;#*****************************************************************************

cProc	B$ONSA,<FAR,PUBLIC>
parmW	strignum
parmD	fpHandler
cBegin
	MOV	BX,strignum	; [BX] = requested strig
	MOV	CX,(NUM_JOYST SHL 8) + STGOFF
	ROR	BL,1		; Divide by 2 so get 0,1,2 or 3
	inc	bx		; Make base 1 so onfun can make it base 0
	jmp	B$ONFUN 
cEnd	nogen


	SUBTTL	STICK FUNCTION

;***
;B$STIK - Joystick Function
;
;Purpose:
;	Runtime Entry Point.
;	Syntax: X = STICK(n)
;
;	Get the X or Y coordinate of joystick A or B
;
;Entry:
;	nStik = stick to get info for
;Exit:
;	AX = X/Y coord of joystick A/B
;Uses:
;	Per Convention
;Exceptions:
;	B$ERR_FC
;****
cProc	B$STIK,<PUBLIC,FAR>
parmW	nStik
cBegin
	MOV	AX,nStik	; get stick function code
	CMP	AX,NUM_JOYST	; it should be 0,1,2,3
	JNB	JOYST_ERROR	; brif not - Illegal function call
	CALL	B$RDSTIK	; call low levels to get coord for stick
	JC	JOYST_ERROR	; complain if low level didn't like arg
	XCHG	AX,BX		; put ret val in AX
cEnd

	SUBTTL	STRIG FUNCTION

;***
;B$FSTG - Read STICK Trigger Function
;
;Purpose:
;	Runtime Entry Point.
;	Syntax: X = STRIG(n)
;
;	If: n = 0 Returns -1 if the trigger has been fired since
;		  the last STRIG(0) call.  If not, STRIG(0)
;		  returns 0...
;		1 Return -1 if trigger is currently down.  If
;		  up, then STRIG(1) returns 0.
;
;		  STRIG(2) and STRIG(3) coorespond to (0) and
;		  (1) above, but apply to Joystick 'B'...
;Entry:
;	nTrig == n
;Exit:
;	[AX] = return value
;Uses:
;	Per convention
;Exceptions:
;	B$ERR_IFC
;****
cProc	B$FSTG,<PUBLIC,FAR>
parmW	nTrig
cBegin
	MOV	AX,nTrig	; get trig number/function(should be 0-7)
	MOV	BL,AL		; Save trig number
	SHR	AX,1		; should become 0-3
	CMP	AX,NUM_JOYST	; check if <= 3
	JNB	JOYST_ERROR	; brif not - Illegal Function Call
	MOV	AH,BL		; [AH] = "n" of ON STRIG(n)
	INC	AH		; map (0,1,....) to (1,2,...)
	AND	AH,01h		; [AH] = (1,0,1,0)
	CALL	B$RDTRIG	; [AL] = [0..1] for [~pressed,pressed]
	JC	JOYST_ERROR	; error if low level didn't like it.
	CBW			; [AX] = return value
cEnd

;***
;B$xSTGTRAPINIT - Initializer for STRIG trapping
;
;Purpose:
;	Put address of B$POLSTG in polling dispatch table
;
;Entry:
;	None
;Exit:
;	None
;Uses:
;	None
;
;Exceptions:
;****
cProc	B$xSTGTRAPINIT,<FAR>
cBegin
	MOV	[B$PollDispTable + (JOYST_IND * 2)],OFFSET B$POLSTG
	MOV	b$pJoystInt,EV_TEXTOFFSET B$JoystInt ; check for joystick
						; activity in CLOCKTIC.
cEnd
sEnd	EV_TEXT

	END
