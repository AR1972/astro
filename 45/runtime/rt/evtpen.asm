	TITLE	EVTPEN - Lightpen event handling
;***
; EVTPEN - Lightpen event handling
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Event trapping support for lightpen.
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - PEN Statement - 3 syntax variants map to 3 entry points:
;
;      PEN ON	    PEN OFF	   PEN STOP
;      ------	    -------	   --------
;	  |	       |	       |
;	B$EPE0      B$EPE1	     B$EPE2
;
; - PEN Function:
;
;      v = PEN(n)
;	    |
;	B$FPEN
;
;******************************************************************************
	INCLUDE switch.inc

	INCLUDE rmacros.inc

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
	INCLUDE baslibma.inc

	INITIALIZER	B$xPENTRAPINIT

sBegin	_DATA
externW	B$PollDispTable
sEnd	_DATA

sBegin	_BSS
globalW b$PNDOWN,?,1		; pen down flag
externW b$TRP_LITEPEN
sEnd	_BSS

sBegin	ER_TEXT
	externNP   B$ERR_FC	
sEnd

sBegin	EV_TEXT
assumes CS,EV_TEXT

externNP B$RDPEN
externNP B$TestTrap
externNP B$ReqTrap
externNP B$EVNT_SET
externNP B$settbl

	SUBTTL	PEN Traps
	PAGE

;***
;B$POLPEN
;
;Purpose:
;	Determine if a PEN event has occurred.
;	If it has, then EVTRP is called to signal the occurrance.
;Entry:
;	none
;Exit:
;	none
;Modifies:
;	AX, BX, CX, DX can be used (restored by CHKINT).
;	All other registers are preserved.
;****

cProc	B$POLPEN,<NEAR>	
cBegin				; New routine
	MOV	AL,0D
	CALL	B$RDPEN		;Request "trap-occurred" flag
	mov	b$PNDOWN,bx	; save the flag
	OR	BX,BX
	JE	POLPEX		;branch if no PEN interrupt
	MOV	AL,PENOFF
	CALL	B$TestTrap	;Trapping ON?
	JZ	POLPEX		;NO
	CALL	B$ReqTrap	;Enabled, Issue Request.
	cmp	word ptr b$TRP_LITEPEN+1,-1 ; "on pen gosub xxxx" present?
	jz	polpex		; brif not present
	mov	b$PNDOWN,0	; else if present clear b$PNDOWN
POLPEX:
cEnd

;***
; B$EPE0, B$EPE1, B$EPE2  - PEN [ON,OFF,STOP] Statement
;
; Purpose:
;	Dispatch PEN statement depending upon following clauses:
;
;	PEN	   ON	   Enable  PEN Trapping.
;	PEN	   OFF	   Disable PEN Trapping.
;	PEN	   STOP    Suspend PEN Trapping.
; Input:
;	NONE
; Output:
;	Event set in trap table, or error
;****

labelFP	<PUBLIC,B$EPE0>		; PEN ON
	MOV	DL,$ON
	SKIP	2
labelFP	<PUBLIC,B$EPE1>		; PEN OFF
	MOV	DL,$OFF
	SKIP	2
labelFP	<PUBLIC,B$EPE2>		; PEN STOP
	MOV	DL,$STOP

cProc	PENS_PROC,<FAR>,<SI,ES>
cBegin
	MOV	CL,PENOFF
	MOV	SI,EV_TEXTOFFSET B$RDPEN ; routine to en/disable trapping
	XOR	BL,BL
	CALL	B$EVNT_SET
cEnd

;#*****************************************************************************

cProc	B$ONPA,<FAR,PUBLIC>
parmD	fpHandler
cBegin
	MOV	AL,PENOFF
	LEA	BX,[fpHandler]	; parm to settbl
	call	B$settbl	  ; ON PEN...
cEnd


	SUBTTL	PEN FUNCTION
	PAGE

;***
;B$FPEN - Light Pen Function.
;
;Purpose:
;	Runtime Entry Point.
;	Syntax: X = PEN(n)
;		Read the Light Pen, Where n:
;		0 -	Was Pen down since last poll (-1 yes, 0 no).
;		1 -	X Pixel value when last depressed (0-319,639).
;		2 -	Y Pixel value when last depressed (0-199).
;		3 -	Current Pen switch value (-1 down, 0 up).
;		4 -	X Pixel last value valid with pen down.
;		5 -	Y Pixel last value valid with pen down.
;		6 -	Y value read when last depressed (0-24).
;			  (Char Row).
;		7 -	X value read when last depressed (0-39,79).
;			  (Char Col).
;		8 -	Y last value valid with pen down. (Char Row).
;		9 -	X last value valid with pen down. (Char Col).
;	    The parm (in BX) is passed into the low level routine
;	    which has identical functionality
;Entry:
;	nPen == (n) parameter
;Exit:
;	[AX] == Function result
;Uses:
;	Per convention
;Exceptions:
;	B$ERR_FC
;****

max_pen_arg =	9d		;current legal pen arguments are [0..9]
cProc	B$FPEN,<PUBLIC,FAR>
parmW	nPen
cBegin
	MOV	AX,nPen 	; Get pen function number
	CMP	AX,max_pen_arg
	JA	PEN_ERROR	; make sure that [bh] is also 0
	OR	AL,AL		; polling the pen ?
	JNZ	PEF1		; brif not
	XCHG	AX,b$PNDOWN	; else use b$PNDOWN as the polled
				; result and clear the b$PNDOWN flag
	OR	AX,AX		; was it down since the last poll ?
	JNZ	PEFRET		; brif so
PEF1:
	CALL	B$RDPEN		; [BX] = 16 bit return value
	XCHG	AX,BX		; return it in AX
PEFRET:
cEnd

PEN_ERROR:
	JMP	B$ERR_FC	;complain

;***
;B$xPENTRAPINIT - Initializer for LITEPEN trapping
;
;Purpose:
;	Put address of B$POLPEN in polling dispatch table
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
cProc	B$xPENTRAPINIT,<FAR>
cBegin
	MOV	[B$PollDispTable + (LITEPEN_IND * 2)],OFFSET B$POLPEN
cEnd

sEnd	EV_TEXT

	END
