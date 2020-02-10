	TITLE	EVTTIM - Timer event handling
;***
; EVTTIM - Timer event handling
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Event trapping support for timer.
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - TIMER(n) Statement - 3 syntax variants map to 3 entry points:
;
;      TIMER(n) ON	  TIMER(n) OFF	    TIMER(n) STOP
;      -----------	  ------------	    -------------
;	    |			|		   |
;	 B$ETT0		     B$ETT1		B$ETT2
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

	INITIALIZER	B$xTIMTRAPINIT

sBegin	_DATA
externW	B$PollDispTable
sEnd	_DATA

sBegin	ER_TEXT
externNP B$ERR_FC
sEnd	ER_TEXT

sBegin	EV_TEXT
assumes CS,EV_TEXT

externNP B$RTIMR
externNP B$TestTrap
externNP B$ReqTrap
externNP B$EVNT_SET
externNP B$settbl
externNP B$ONFUN

	SUBTTL	TIMER Statement, Function, and Trapping Support
;***
;POLTIM
;Purpose:
;	Determine if a timer event has occurred.
;	If it has, then EVTRP is called to signal the occurrance.
;Entry:
;Exit:
;Modifies:
;	 AX, BX, CX, DX can be used (restored by CHKINT).
;	 All other registers are preserved.
;****

cProc	B$POLTIM,<NEAR>
cBegin
	MOV	AL,0D
	CALL	B$RTIMR		; Request "trap-occurred" flag
	OR	BX,BX
	JE	POLTIX		;branch if no timer interrupt
	MOV	AL,TIMOFF
	CALL	B$TestTrap	;Trapping ON?
	JZ	POLTIX		;NO
	CALL	B$ReqTrap	;Enabled, Issue Request.
POLTIX:
cEnd

	SUBTTL	TIMER [ON,OFF,STOP] Statement
	PAGE

;***
; B$ETT0, B$ETT1, B$ETT2  - TIMER  [ON,OFF,STOP] Statement
;
; Purpose:
;	Dispatch TIMER statement depending upon following clauses:
;
;	TIMER	     ON      Enable  TIMER Trapping.
;	TIMER	     OFF     Disable TIMER Trapping.
;	TIMER	     STOP    Suspend TIMER Trapping.
; Input:
;	NONE
; Output:
;	Event set in trap table, or error
;****

labelFP	<PUBLIC,B$ETT0>		; TIMER ON
	MOV	DL,$ON
	SKIP	2
labelFP	<PUBLIC,B$ETT1>		; TIMER OFF
	MOV	DL,$OFF
	SKIP	2
labelFP	<PUBLIC,B$ETT2>		; TIMER STOP
	MOV	DL,$STOP

cProc	TIMON_PROC,<FAR>,<SI,ES>
cBegin
	MOV	CL,TIMOFF
	MOV	SI,EV_TEXTOFFSET B$RTIMR
	XOR	BL,BL
	CALL	B$EVNT_SET	;Set Event
cEnd

EVNT_ERROR:
	JMP	B$ERR_FC	; give Illegal function call error
;***
;****

cProc	B$ONTA,<FAR,PUBLIC>,<ES>
parmD	timecount
parmD	fpHandler
cBegin

	push	dx		; save user routine
	mov	dx,off_timecount ; low word of timer value.
	mov	ax,seg_timecount ; high word of timer value.
	mov 	cx,ax		; CX contains high word
	or	ax,dx		; is input zero?
	jz	EVNT_ERROR	; brif so -- illegal function call
	jcxz	NUMOK		; brif input < 64k -- it's o.k.
	loop	EVNT_ERROR	; brif input >= 0x20000
	inc	cx		; restore high word to 1
	cmp	dx,5181H	; greater than 86400 (X'15181') ?
	jnb	EVNT_ERROR	; brif so -- out of range
NUMOK:				; CL/DX contains valid 24-bit integer
	mov	al,set_interval ; tell oem the interval
	call	B$RTIMR
	jc	EVNT_ERROR	; oem didn't like value for on timer

	pop	dx		; restore user routine
	mov	al,TIMOFF	; offset of timer into b$TRPTBL
	lea	bx,[fpHandler]	; parm to settbl
	call	B$settbl
cEnd

;***
;B$xTIMTRAPINIT - Initializer for TIMER trapping
;
;Purpose:
;	Put address of B$POLTIM in polling dispatch table
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
cProc	B$xTIMTRAPINIT,<FAR>	
cBegin
	MOV	[B$PollDispTable + (TIMER_IND * 2)],OFFSET B$POLTIM
cEnd
sEnd	EV_TEXT

	END
