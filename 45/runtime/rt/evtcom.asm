	TITLE	EVTCOM - Communications event handling
;***
; EVTCOM - Communications event handling
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Event trapping support for com ports
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - COM(n) Statement - 3 syntax variants map to 3 entry points:
;
;      COM(n) ON       COM(n) OFF	 COM(n) STOP
;      ---------       ----------	 -----------
;	   |		    |		      |
;	B$ETC0	 	 B$ETC1		   B$ETC2
;
;******************************************************************************
	INCLUDE switch.inc

	INCLUDE rmacros.inc
	INCLUDE baslibma.inc
	INCLUDE comdcb.inc

	useSeg	EV_TEXT
	useSeg	ER_TEXT
	useSeg	_DATA
	useSeg	_BSS
	useSeg	XIB
	useSeg	XI
	useSeg	XIE

	INCLUDE seg.inc
	INCLUDE idmac.inc
	INCLUDE event.inc

	INITIALIZER	B$xCOMTRAPINIT

sBegin	_BSS
externW b$COM_DCB
sEnd	_BSS

sBegin	_DATA
externW B$PollDispTable
sEnd	_DATA

sBegin	ER_TEXT
externNP B$ERR_CBO
externNP B$ERR_IOE
externNP B$ERR_FC
sEnd

sBegin	EV_TEXT
assumes CS,EV_TEXT

externNP B$COMLOC
externNP B$EVNT_SET
externNP B$TestTrap
externNP B$ReqTrap
externNP B$settbl
externNP B$ONFUN

	Subttl	Communications polling routines
	PAGE

;***
;B$POLCOM -
;
;Purpose:
;	POLCOM is called by CHKINT at beginning of every BASIC statement
;	(INT 3). For each COM device which is opened to a device, it calls
;	COM_TRAP if that device has input data waiting.
;
;Entry:
;	None.
;
;Exit - AX, BX, CX, DX can be used (restored by CHKINT).
;	 All other registers are preserved.
;****

cProc	B$POLCOM,<NEAR>
cBegin
	PUSH	DI
	PUSH	SI
	MOV	DI,OFFSET DGROUP:b$COM_DCB ;DI points to device control block 1
	MOV	DX,NUM_RS232	;[DH]=COM unit#, [DL]=number of COM units
POLCML: CALL	POLCM1		;test unit 1 for trap
	ADD	DI,SIZE COMDCB	;DI points to next device control block
	INC	DH		;Bump unit id
	DEC	DL		;Decrement unit counter
	JNZ	POLCML		;branch if any more to test
	POP	SI
	POP	DI
cEnd

;***
;POLCM1
;
;Purpose:
;	 POLCM1 checks 1 COM device to see if input is waiting, if so,
;	 it calls COM_TRAP
;
;Entry: [dh,dl] = [unit#,units remaining]
;	[di]	= CMnDCB
;
;Exit:
;	none.
;Uses:
;	SI
;Exceptions:
;	none
;****

cProc	POLCM1,<NEAR>
cBegin
	MOV	SI,[DI].CD_CMFDB ;SI points to FDB (if device is opened)
	OR	SI,SI
	JZ	PLCM1X		;return if device not opened
	MOV	AH,DH		;[AH]=Unit#
	PUSH	DX		;save Unit#
	CALL	GCOMSZ		;[DX]=number of bytes queued
	CMP	DX,[DI].CD_CMEVT ;test if any new character have been rcved
	MOV	[DI].CD_CMEVT,DX ;update value in case of event
	POP	DX		;restore Unit#
	MOV	AL,DH		;[AL]=trap id (0..n)
	JE	PLCM1X		;branch if no new com data queued
	CALL	COM_TRAP	; so ON-COM service routine will be called
PLCM1X:
cEnd

;***
;GCOMSZ - read comm bytes
;
;Purpose:
;	 Get COM bytes in queue
;	 Does error processing if gets error from COM channel
;
;Entry:
;	 AH = unit number
;
;Exit:
;	 DX=bytes in queue
;
;Uses:
;	 Per Convention
;
;Exceptions:
;	May call B$ERR_CBO or B$ERR_IOE
;****
cProc	GCOMSZ,<NEAR>
cBegin
	PUSH	AX
	CALL	B$COMLOC
	OR	AH,AH
	JNZ	CMERR		;Got error from COM
	POP	AX
cEnd

;Output COM Error Message

CMERR:
	DEC	AH
	JNZ	IOERR
	JMP	B$ERR_CBO	; and give "Comm Buffer Overflow"
IOERR:
	JMP	B$ERR_IOE	; and give "Device I/O Error".

	SUBTTL	COM (n) [ON,OFF,STOP] Statement
	PAGE

;***
; c$ETC0, B$ETC1, B$ETC2 - COM (n) [ON,OFF,STOP] Statement
;
; Purpose:
;	Dispatch COM statement depending upon following clauses:
;
;	COM (n)       ON      Enable  COM(n) Trapping.
;	COM (n)       OFF     Disable COM(n) Trapping.
;	COM (n)       STOP    Suspend COM(n) Trapping.
;
;	WHERE: (n) is in [1..NUM_RS232]
; Input:
;	parm1 == (n)
; Output:
;	Event set in trap table, or error
;****

labelFP	<PUBLIC,B$ETC0>		; COM(n) ON
	MOV	DL,$ON
	SKIP	2
labelFP	<PUBLIC,B$ETC1>		; COM(n) OFF
	MOV	DL,$OFF
	SKIP	2
labelFP	<PUBLIC,B$ETC2>		; COM(n) STOP
	MOV	DL,$STOP

cProc	COMON_PROC,<FAR>,<SI,ES>
parmW	commPort
cBegin
	MOV	BX,[commPort]
	DEC	BX
	CMP	BX,NUM_RS232	;In range?
	JNB	EVNT_ERROR	;Brif not

	MOV	CL,COMOFF
	XOR	si,si		; no low-level routine for trapping
	CALL	B$EVNT_SET	; jump to common exit
cEnd

EVNT_ERROR:
	JMP	B$ERR_FC

; COM_TRAP - Interrupt Trap Request for COM.
; Entry:
;	[al]	trap id [0..COMn-1]:

cProc	COM_TRAP,<NEAR>
cBegin

	ADD	AL,COMOFF	;Add Base Index to COM Traps
	CALL	B$TestTrap	;See if Traps ON.
	JZ	COM_TRAPX	;Brif not.
	CALL	B$ReqTrap	;Trap Enabled. Issue Request.
COM_TRAPX:
cEnd

;***
;****
cProc	B$ONCA,<FAR,PUBLIC>
parmW	comport
parmD	fpHandler
cBegin
	MOV	CX,(NUM_RS232 SHL 8) + COMOFF
	MOV	BX,comport	; [BX] = requested comport
	JMP	B$ONFUN 	; ON COM...

cEnd	nogen

;***
;B$xCOMTRAPINIT - Initializer for COM trapping
;
;Purpose:
;	Put address of B$POLCOM in polling dispatch table
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
cProc	B$xCOMTRAPINIT,<FAR>	
cBegin
	MOV	[B$PollDispTable + (RS232_IND * 2)],OFFSET B$POLCOM
cEnd

sEnd	EV_TEXT

	END
