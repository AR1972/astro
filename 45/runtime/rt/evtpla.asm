	TITLE	EVTPLA - Play event handling
;***
; EVTPLA - Play event handling
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Event trapping support for PLAY.
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - PLAY ON | OFF | STOP Statement - 3 possible entry points:
;
;      PLAY(n) ON	  PLAY(n) OFF	    PLAY(n) STOP
;      ----------	  -----------	    ------------
;	   |		       |		  |
;	B$ETL0		    B$ETL1	       B$ETL2
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

	INITIALIZER	B$xPLATRAPINIT

sBegin	_DATA
externW	B$PollDispTable
sEnd	_DATA

sBegin	ER_TEXT
externNP B$ERR_FC
sEnd	ER_TEXT

sBegin	EV_TEXT
assumes CS,EV_TEXT

externNP B$RDPLAY
externNP B$TestTrap
externNP B$ReqTrap
externNP B$EVNT_SET
externNP B$settbl
externNP B$ONFUN

;***
;B$POLPLA
;
;Purpose:
;	Determine if a PLAY event has occurred.
;	If it has, then EVTRP is called to signal the occurrance.
;Entry:
;	none
;Exit:
;	none
;Modifies:
;	AX, BX, CX, DX can be used (restored by CHKINT).
;	All other registers are preserved.
;****

cProc	B$POLPLA,<NEAR>
cBegin
	MOV	AL,0D
	CALL	B$RDPLAY	;Request "trap-occurred" flag
	OR	BX,BX
	JE	POLPLX		;branch if no PLAer interrupt
	MOV	AL,PLAOFF	; NBA
	CALL	B$TestTrap	;Trapping ON?
	JZ	POLPLX		;NO
	CALL	B$ReqTrap	;Enabled, Issue Request.
POLPLX:
cEnd

	SUBTTL	PLAY [ON,OFF,STOP] Statement
	PAGE

;***
; B$ETL0, B$ETL1, B$ETL2  - PLAY  [ON,OFF,STOP] Statement
;
; Purpose:
;	Dispatch PLAY statement depending upon following clauses:
;
;	PLAY	    ON	    Enable  PLAY Trapping.
;	PLAY	    OFF     Disable PLAY Trapping.
;	PLAY	    STOP    Suspend PLAY Trapping.
; Input:
;	NONE
; Output:
;	Event set in trap table, or error
;****

labelFP	<PUBLIC,B$ETL0>		; PLAY ON
	MOV	DL,$ON
	SKIP	2
labelFP	<PUBLIC,B$ETL1>		; PLAY OFF
	MOV	DL,$OFF
	SKIP	2
labelFP	<PUBLIC,B$ETL2>		; PLAY STOP
	MOV	DL,$STOP

cProc	PLAON_PROC,<FAR>,<SI,ES>
cBegin
	MOV	CL,PLAOFF	; NHL
	MOV	SI,EV_TEXTOFFSET B$RDPLAY
	XOR	BL,BL
	CALL	B$EVNT_SET	;Set Event
cEnd

;***
;****

cProc	B$ONLA,<FAR,PUBLIC>
parmW	notecount
parmD	fpHandler
cBegin

	MOV	BX,notecount	; [BX] = note count
	mov	al,set_note_cnt ; report n to oem
	call	B$RDPLAY
	jc	EVNT_error	; oem didn't like value for on play

	mov	al,plaoff	; NFL
	LEA	BX,[fpHandler]	;parm to settbl
	call	B$settbl	
cEnd

EVNT_ERROR:
	JMP	B$ERR_FC

;***
;B$xPLATRAPINIT - Initializer for PLAY trapping
;
;Purpose:
;	Put address of B$POLPLA in polling dispatch table
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
cProc	B$xPLATRAPINIT,<FAR>	
cBegin
	MOV	[B$PollDispTable + (SOUND_IND * 2)],OFFSET B$POLPLA
cEnd
sEnd	EV_TEXT


	END
