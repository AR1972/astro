	TITLE	EVTKEY - Keytrapping event handling
;***
; EVTKEY - Keytrapping event handling
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Event trapping support for keytrapping.
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - KEY(n) Statement - 3 syntax variants map to 3 entry points:
;
;      KEY(n) ON       KEY(n) OFF	 KEY(n) STOP
;      ---------       ----------	 -----------
;	   |		    |		      |
;	B$ETK0	 	 B$ETK1 	   B$ETK2
;
; SLEEP [n] Statement
;
;      SLEEP [n]
;      ---------
;	   |
;	B$SLEP
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	EV_TEXT
	useSeg	ER_TEXT
	useSeg	RT_TEXT
	useSeg	INIT_CODE
	useSeg	_DATA
	useSeg	_BSS
	useSeg	XIB
	useSeg	XI
	useSeg	XIE

	INCLUDE seg.inc
	INCLUDE event.inc
	INCLUDE const.inc
	INCLUDE baslibma.inc
	INCLUDE idmac.inc

	INITIALIZER	B$xKEYTRAPINIT

sBegin	_BSS
externW b$RcoFlg
sEnd	_BSS

sBegin	_DATA
externB b$EventFlags		;misc event flags. Bits defined in EVENT.INC
externW B$PollDispTable
externW b$pInitKeys1		;vector for B$InitKeys1
externW b$pInitKeys2		;vector for B$InitKeys2
sEnd	_DATA


sBegin	ER_TEXT
externNP B$ERR_FC
sEnd	ER_TEXT




sBegin	EV_TEXT
assumes CS,EV_TEXT

	externNP B$SetKybdInt	;install keyboard interrupt handler
	externNP B$SetClockInt	;install clock interrupt handler
	externNP B$SleepInit	;set up sleep timeout

externNP B$InitKeys1		;keytrapping init for /D or ON KEY & friends
externNP B$POLKEY
externNP B$RDKYBD
externNP B$TrapAddr
externNP B$TestTrap
externNP B$ReqTrap
externNP B$EVNT_SET
externNP B$OnTrap
externNP B$OffTrap
externNP B$StopTrap
externNP B$settbl
externNP B$ONFUN

	SUBTTL	KEY (n) [ON,OFF,STOP] Statement
	PAGE
;***
; B$ETK0, B$ETK1, B$ETK2 - KEY (n) [ON,OFF,STOP] Statement
;
; Purpose:
;	Dispatch KEY statement depending upon following clauses:
;
;	KEY (n)       ON      Enable  KEY(n) Trapping.
;	KEY (n)       OFF     Disable KEY(n) Trapping.
;	KEY (n)       STOP    Suspend KEY(n) Trapping.
;
;	WHERE:
;		(n) is Softkey 1..NUM_TKEYS.
;		If (n) is 0 then ON, OFF, and STOP
;		apply to all keys 1 thru NUM_TKEYS...
; Input:
;	parm1 == (n)
; Output:
;	Event set in trap table, or error
;****

labelFP	<PUBLIC,B$ETK0>		; KEY(n) ON
	MOV	DL,$ON		
	SKIP	2
labelFP	<PUBLIC,B$ETK1>		; KEY(n) OFF
	MOV	DL,$OFF 	
	SKIP	2
labelFP	<PUBLIC,B$ETK2>		; KEY(n) STOP
	MOV	DL,$STOP	

cProc	KEYON_PROC,<FAR>,<SI,ES>
parmW	keyNum
cBegin				
	mov	bx,[keyNum]	
	OR	BX,BX		; KEY(0)?
	JNZ	NOT_ALL_KEYS	; Brif not do all keys.

	;SET KEY(0) STATUS
	;	 KEY(0) <status> SETS ALL KEYS TO <status>

	PUSH	DI		

	DEC	DL		; cheap test of ON/OFF/STOP token
.errnz	$ON - 0
	JS	KYON		; brif DL == $ON - Set all keys to ON
.errnz	$OFF - 1
	JZ	KYOFF		; brif DL = $OFF - Set all keys to OFF
	;default - fall through for DL = $STOP

	MOV	DI,EV_TEXTOFFSET B$StopTrap
	JMP	SHORT KEYALL
KYON:
	MOV	DI,EV_TEXTOFFSET B$OnTrap
	JMP	SHORT KEYALL
KYOFF:
	MOV	DI,EV_TEXTOFFSET B$OffTrap
KEYALL:
	MOV	CX,NUM_TKEYS
KEYAL1:
	PUSH	CX		;Save counter
	PUSH	DI		;Save funct dispatch
	MOV	AL,KEYOFF
	ADD	AL,CL		;index to NUM_TKEYS
	MOV	CH,CL		; CH key number
	DEC	AL		;Adjust to base 0.
	MOV	SI,EV_TEXTOFFSET KEYALX ; NOTE: this is the return address
	PUSH	SI			;     NOTE: which will eventually get
	MOV	SI,EV_TEXTOFFSET B$RDKYBD; NOTE: control. This is the adr
	PUSH	DI			;     NOTE: B$TrapAddr  will exit.
	JMP	B$TrapAddr	; Get Trap Table, do function.
KEYALX:
	POP	DI		;Dispatch addr.
	POP	CX		;counter.
	LOOP	KEYAL1		;Until all keys done..
	POP	DI		; restore this prior to exit
	JMP	SHORT KEYON_EXIT1

NOT_ALL_KEYS:
	mov	ch,bl		; save keynum for B$RDKYBD
	DEC	BX		;MAKE IT 0 TO NUM_TKEYS-1
	CMP	BX,NUM_TKEYS
	JNB	EVNT_ERROR	;Brif .gt. NUM_TKEYS-1

	TEST	b$RcoFlg,0FFH	; is ronco presented ?
	JZ	ChkUsrKy	; Brif not

	CMP	BX,NUM_TKEYS-2	; is F11,F12
	JAE	NoError 	; Brif yes
ChkUsrKy:			; check user defined keys
	CMP	BX,NUM_TKEYS-2-NUM_GAP	; is legal user defined keys ?
	JAE	EVNT_ERROR	; Brif not
NoError:			; # is in legal range
	MOV	CL,KEYOFF
	MOV	SI,EV_TEXTOFFSET B$RDKYBD ;lo-level rtn to start/stop traping
KEYON_EXIT:			; shared exit
	CALL	B$EVNT_SET	;Set Event..
KEYON_EXIT1:
cEnd

EVNT_ERROR:
	JMP	B$ERR_FC

;***
;B$InitKeys2 -- initialize keytrapping stuff
;
;****
cProc	B$InitKeys2,<PUBLIC,NEAR>
cBegin

; Now disable all function keys, cursor keys & user-defined keys

	MOV	AL,DISABLE_TRAP ; Function to disable trapping
	MOV	DX,NUM_TKEYS	; Start with maximum trappable key
DISA_KEY:			; Loop until DX = 0
	CLI
	CALL	B$RDKYBD	; Disable it
	STI
	DEC	DX		; Step to next key
	JNZ	DISA_KEY	; and loop back
cEnd

;#*****************************************************************************
cProc	B$ONKA,<FAR,PUBLIC>
parmW	keynum
parmD	fpHandler
cBegin				
	MOV	BX,keynum	; [BX] = requested key
	MOV	CH,NUM_TKEYS-2-NUM_GAP	; if no ronco presented, last legal
				;  key is 25, NUM_TKEYS including the gap
				;  and F11/F12
	TEST	b$RcoFlg,0FFH	; is ronco present ?
	JZ	ChkEnd		; Brif not
	CMP	BL,CH		; is legal user defined keys
	JBE	RoncoHere	; Brif yes
	CMP	BL,NUM_TKEYS-2	; is possibly F11 or F12 ?
	JBE	EVNT_ERROR	; Brif not
RoncoHere:			
	MOV	CH,NUM_TKEYS	; extended the range
				; 0 or beyond the range will be checked in
				; ONFUN
ChkEnd:
	MOV	CL,KEYOFF
	JMP	B$ONFUN

cEnd	nogen

	SUBTTL	SLEEP statement
	page

;*** 
; B$SLEP -- Wait for a given amount of time or for a BASIC event.
;
;Purpose:
;	Wait until one of the following occurs:
;		N seconds have elapsed
;		A trapable BASIC event occurs
;		A key is hit
;
;	If N = 0 (compiler also generates N = 0 if parameter not specified)
;	it functions as if N = infinite.
;
;Entry:
;	Number of seconds to sleep on stack
;
;Exit:
;	None
;
;Uses:
;	Per convention
;
;Exceptions:
;	None
;
;******************************************************************************
cProc	B$SLEP,<FAR,PUBLIC>
parmD	secs			; # seconds to wait (0 if no parm)
cBegin

	call	B$SetKybdInt	; install kybd int handler if not already done.
NoKybdInst:			
	call	B$SetClockInt	; set up clock interrupt if not already done

	mov	dx,seg_secs	; DX = high word of # seconds
	mov	ax,off_secs	; AX = low word of # seconds

	mov	cx,dx		; zero seconds specified?
	or	cx,ax
	jz	NoTimeout	; brif so -- no timeout


	cmp	DX,2		; is it more than SCTIOC can handle?
	jae	NoTimeout	; brif so -- assume no timeout
	call	B$SleepInit	; set up number of seconds to sleep
	OR	b$EventFlags,SLEEPtmr ; activate SLEEP timout timer

NoTimeout:
	OR	b$EventFlags,InSLEEP ; set in SLEEP statement flag

EventLoop:
	test	b$EventFlags,InSLEEP ; have any events occured?
	jnz	EventLoop	; brif not -- loop until one occurs


cEnd




	page
;***
;B$xKEYTRAPINIT - Initializer for KEY trapping
;
;Purpose:
;	Put address of B$POLKEY in polling dispatch table
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
cProc	B$xKEYTRAPINIT,<FAR>
cBegin
	MOV	[B$PollDispTable + (TKEYS_IND * 2)],EV_TEXTOFFSET B$POLKEY
	MOV	[b$pInitKeys1],EV_TEXTOFFSET B$InitKeys1
	MOV	[b$pInitKeys2],EV_TEXTOFFSET B$InitKeys2
NoKybdInit:				
cEnd

sEnd	EV_TEXT
	END
