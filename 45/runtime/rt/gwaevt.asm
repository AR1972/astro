	PAGE	56,132
	TITLE	GWAEVT Advanced Event Trapping Handler
;***
; GWAEVT Advanced Event Trapping Handler
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - ON COM(n) Statement:
;
;      ON COM(n) GOSUB line
;      --------------------
;		|
;	      B$ONCA
;
; - ON SIGNAL(n) Statement:
;
;      ON SIGNAL(n) GOSUB line
;      -----------------------
;		|
;	      B$ONSG
;
; - ON KEY(n) Statement:
;
;      ON KEY(n) GOSUB line
;      --------------------
;		|
;	      B$ONKA
;
; - ON MOUSE(n) Statement:
;
;      ON MOUSE(n) GOSUB line
;      --------------------
;		|
;	      B$ONMO
;
; - ON UEVENT Statement:
;
;      ON UEVENT GOSUB line
;      --------------------
;		|
;	      B$ONUE
;
; - ON PEN Statement:
;
;      ON PEN GOSUB line
;      -----------------
;	       |
;	     B$ONPA
;
; - ON PLAY(n) Statement:
;
;      ON PLAY(n) GOSUB line
;      ---------------------
;		|
;	      B$ONLA
;
; - ON STRIG(n) Statement:
;
;      ON STRIG(n) GOSUB line
;      ----------------------
;		|
;	      B$ONSA
;
; - ON TIMER(n) Statement:
;
;      ON TIMER(n) GOSUB line
;      ----------------------
;		|
;	      B$ONTA
;
;******************************************************************************
	include switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	useSeg	_BSS		; Uninitailzed data
	useSeg	_DATA		; Initialized data
	useSeg	EV_TEXT 	; Event handler code
	useSeg	RT_TEXT 	; Runtime Core
	useSeg	ER_TEXT 	; Errors
	UseSeg	INIT_CODE	
	UseSeg	<XIB>		; XIB and XIE must bracket XI!
	UseSeg	<XI>		; initializer segment
	UseSeg	<XIE>		

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc
	INCLUDE intmac.inc
	include event.inc	; oem interface
	include queues.inc	;get queue stuff
	include stack.inc	
	INCLUDE compvect.inc	
	include idmac.inc	

INITIALIZER	B$x?EVTDSPINI	

sBegin	_DATA			
	staticB b$EVTFLG,0	; event occurred flag
	globalW B$PollDispTable,0,NUM_TRAPDEV ;Dispatch table for polling.
					      ;One word per trappable dev.
					      ;Address of B$POL* routine
					      ;for that device if used,
					      ;else zero.
	externB	b$EventFlags	; misc event flags. bits def in CONST.INC

	externD b$run_disp	
	externD b$clr_disp	

	externW	b$pInitKeys1	
	externW	b$pInitKeys2	
	externW	b$pTrapEvent	

sEnd	_DATA			

sBegin	_BSS			

	externW b$cNonQBIFrames ; non qbi frame count

	externW b$cCSubs	; compiler nested sub level
	externD b$EVTRETV	;defined in RTINIT.ASM
	externB	b$inonerr	; defined in RTINIT.ASM
	externW b$TRPTBL	;defined in GWCEVT.ASM
	externW b$TRAP_QUE	;defined in GWCEVT.ASM
	externB b$TRAP_SEM	;defined in GWCEVT.ASM

staticW TRPCUR,,1		; Current trap table address

sEnd	_BSS			

;#*****************************************************************************

	externFP B$IEvSet	;interp routine to set BOS EV flag
	externFP B$IEvReset	;interp routine to reset BOS EV flag
	externFP B$IEvHandler	;interp routine to do event GOSUB
	externFP B$EnsShowOutputScr ;displays output screen
sBegin	RT_TEXT 		
	externFP B$FRAMESETUP	; Set up runtime frame
	externNP B$NearRet	
sEnd	RT_TEXT 		

sBegin	ER_TEXT 		
	externNP B$FrameAFE	; advanced feature error + frame setup
	externNP B$ERR_DNA	; device not available error
	externNP B$ERR_FC	
sEnd	ER_TEXT 		

sBegin	EV_TEXT			; EV_TEXT externals go here

	externNP B$FreeTrap	
	externNP B$StopTrap	
	externNP B$ResetTrap	
	externNP B$OffTrap	
	externNP B$OnTrap	
	externNP B$SetChk	
	externNP B$TrapAddr	
	externNP B$SetContext	;Set context bit in event flags

sEnd	EV_TEXT			

sBegin	INIT_CODE		
assumes	CS,INIT_CODE		

;*** 
;B$x?EVTDSPINI - Make sure event vectors get initialized when necessary.
;
;Purpose:
;	Make sure that event vectors are initialized if events are linked in.
;	Initializes RUN and CLEAR time event dispatch vectors to B$?EVT.
;	Also initializes [b$EVTRETV] vector to B$EVTRET.
;
;Entry:
;	None.
;
;Exit:
;	Appropriate dispatch vectors filled.
;	Sets [b$EVTRETV] to B$EVTRET.
;	Sets [b$pTrapEvent] to B$TrapEvent.
;
;Uses:
;	Per convention.
;
;Exceptions:
;	None.
;
;******************************************************************************
cProc	B$x?EVTDSPINI,<FAR>	
cBegin				; set run and clear init vectors to B$?EVT
	MOV	AX,SEG EV_TEXTBASE ; load EV_TEXT segment into AX
	MOV	WORD PTR [b$run_disp].EV_RVEC,EV_TEXTOFFSET B$?EVTN ; offset
	MOV	WORD PTR [b$clr_disp].EV_CVEC,EV_TEXTOFFSET B$?EVTN ; offset
	MOV	WORD PTR [b$EVTRETV],EV_TEXTOFFSET B$EVTRET ; offset
	MOV	WORD PTR [b$EVTRETV+2],AX ; segment
	MOV	b$pTrapEvent,EV_TEXTOFFSET B$TrapEvent ; for LLSND, LLCOMx.
cEnd				
sEnd	INIT_CODE		

sBegin	EV_TEXT 		
assumes CS,EV_TEXT		

	SUBTTL	TRAP ROUTINES - INIT
;***
; B$?EVT
;
; Purpose:
;  Runtime Entry Point for /O if /V or /W in effect in any module.
;  Always a Runtime Entry Point for non-/O and for EI_QB.
;  Clears Event Trap Table, clears GOSUB dispatch buffer, and initializes
;  keyboard trapping.
;
; Input:
;  NONE
;
; Output:
;  NONE
;
;******************************************************************************
cProc	B$?EVT,<FAR,PUBLIC>,<ES,SI,DI> 
cBegin				
	PUSH	DS		
	POP	ES		;Set ES = DS
	MOV	DI,OFFSET DGROUP:b$TRPTBL

;	Each entry in b$TRPTBL is 5 bytes.  The first byte is a flag byte
;	that must be inited to zero.  The next 4 bytes are the GOSUB address
;	and must be inited to -1 (to indicate no trap handler).

	MOV	CX,NUM_TRAPS	; count of 5-byte entries in b$TRPTBL
IniTrpTblLoop:			
	XOR	AX,AX		; start with zero each time through loop
	STOSB			; Set flag byte to 0
	DEC	AX		; Get a -1
	STOSW			; 2nd and 3rd bytes = -1
	STOSW			; 4th and 5th bytes = -1
	LOOP	IniTrpTblLoop	; keep going until entire table inited


	MOV	[b$TRAP_SEM],CL ;Set no Events in process
	MOV	SI,OFFSET DGROUP:b$TRAP_QUE ;init event gosub dispatch tbl
	mov	ax,(NUM_TRAPS+1)*2  ; room for event dispatches
	MOV	BX,OFFSET DGROUP:b$TRAP_QUE+QUE_HEADER_SIZE
	CALL	B$INITQ		;Init queue descriptor

	STI
	MOV	AL,DISABLE_TRAP ; to disable ctrl-break trapping
	CALL	[b$pInitKeys1]	; Init some keytrapping stuff
	CALL	[b$pInitKeys2]	; Init some more keytrapping stuff
cEnd				

;***
; B$?EVTN
;
; Purpose:
;  Near entry to B$?EVT.
;  Added with [52]
;
; Input:
;  NONE
;
; Output:
;  NONE
;
;******************************************************************************
cProc	B$?EVTN,<NEAR>
cBegin
	call	B$?EVT
cEnd

	SUBTTL Stack for event traps
	PAGE
;Stack for event dispatches in different environments. The notation X->Y means
;that the executing context was X (I for interp, C for comp), and
;->Y means that control was transfered to context Y. SC, DC and RC are
;abbreviations for Source Context, Destination Context, and Runtime Context.
;
;Pushed
;  By  C->C	       I->C			C->I		 I->I
;----  +-----------------------------+	 High	+-----------------------------+
;      |	      |Interp context|	  ^	|	       |Interp context|
; SC   |Far ret addr  |Far ret addr  |	  |	|Far ret addr  |Far ret addr  |
;----  |--------------|--------------|	  |	|--------------|--------------|
;      |Context flag  |Context flag  |	STACKS	|Context flag  |Context flag  |
; RC   |Trap tabl addr|Trap tabl addr|	  |	|Trap tabl addr|Trap tabl addr|
;----  |--------------|--------------|	  |	|--------------|--------------|
;      |Compiler frame|Compiler frame|	  |	|Interp frame  |Interp frame  |
; DC   |Zero	      |Zero	     |	  v	|Zero(?)       |Zero(?)       |
;----  +-----------------------------+	 Low	+-----------------------------+

;Note that the context flag is pushed on the stack even in the COMPILER ONLY
;environment to keep the event frame size the same for all environments.

	SUBTTL Event dispatch execution path
	PAGE
;The flow of control is as follows:
;
;Compiled Code (CC)	 Common runtime (RT)	   Interpreted Code (IC)
;==================	 ===================	   =====================
;
;+------------------------+		       +------------------------------+
;|CC ACTIVE AT EV DISPATCH|		       | IC ACTIVE AT EV DISPATCH     |
;|Comp calls B$EVCK	  |------>+<-----------+ interp detects BOS event flag|
;+------------------------+	  |	       | interp saves context on stack|
;				  |	       | interp calls B$EVCK	      |
;				  v	       +------------------------------+
;		     +--------------------------------+
;		     |	 B$EVCK detects trap request |
;		     |	 if last event in Q,	      | 	+------------+
;		     |	    RT calls interp to	      +<= = = =>+Reset BOS EV|
;		     |	    reset BOS flag	      | 	|FLAG	     |
;		     |	 pushes context flag	      | 	+------------+
;		     |	 pushes trap table address    |
;		     |----------------+---------------|
;		     |if CC handler   |if IC handler  |
;		     | inc contxt flag| 0 context flag|
;*Done for compiler* | builds CC frame| jmps to interp|
;*by the runtime   * | pushes 0 for   |   with ptr to |
;		     |	 B$RETA      |   handler     |
;		     | jmps to CC addr|   context     |
;		     |	 in trap table| 	      |
;		     +--------+--------------+--------+
;+-----------------------+    | 	     |	     +------------------------+
;|   CC EVENT HANDLER	 |    | 	     |	     |	  IC EVENT HANDLER    |
;|handler executes	 |<---+ 	     |	     |interp builds frame     |
;|CC calls B$RETA	 |--------+	     +------>|handler executes	      |
;+-----------------------+	  v		     |interp tears down frame |
;			+--------------------------+ |interp JMPS to B$EVTRET|
;* Done for compiler *	|B$RETA determines EV ret | +----------+-------------+
;*		     *	|tosses return address	   |		|
;*		     *	|cleans off frame	   |		|
;* by the runtime    *	|Runtime JMPS to B$EVTRET |		|
;			+---------+----------------+		|
;				  +<----------------------------+
;				  v
;			+--------------------------+
;			|B$EVTRET		   |
;			|recovers trap table addr  |
;			|recovers context flag	   |
;			|restarts event 	   |
;			|return untrapped brk flag |
;			|returns to caller	   |
;			+---------+----------------+
;+-------------------+		  |		     +------------------------+
;|Comp code continues|<-----------+----------------->|interp BOS proc recovers|
;|execution	     |				     |context and continues   |
;+-------------------+				     +------------------------+
;
	SUBTTL	B$POLLEV/Description of event trapping
	PAGE
;***
;B$POLLEV - Check if a trapable event has occured.
;	 Defunct routine, but the comments are nice.
;Purpose:
;
;      Poll to determine if a trapable event has taken place.
;	 Trapable events are:
;	   data placed in keyboard queue
;	   data placed in any RS232 queue
;	   RS232 error - timeout etc.
;	   light pen
;	   joy stick trigger
;	   music events
;	   timer events
;
;Event and Trapping Mechanisms:
;
;	All events, no matter what generates them, are handled in
;	the same way.  When the event occurs, it will either cause
;	a routine to be called or will be tested by a routine.	This
;	routine will set two different flags.  The first flag that
;	is set is b$EVTFLG.  This flag indicates that at least one
;	event has taken place.	It does not keep track if multiple
;	events have occured since events were last tested or not.
;	At the appropriate time (either after every statement or
;	after every line) the flag will be tested.  If it indicates
;	that an event has taken place, the flag will be cleared and
;	all trappable items will be polled to see if they have an
;	outstanding event.
;
;	The second flag that is set at the time of the event is
;	one that is local to the item that has the event.  This
;	is needed so that each item can properly give its status
;	when it is polled.  Note that polling an individual item
;	will not change the global event flag.
;
;	If for some reason your machine can not suport the global
;	event trapping, always set the flag to indicate that an
;	event has taken place.	Thus, at the end of each statement,
;	the OEM-independent code will poll each of the individual
;	items for its status.  This will produce a degradation in
;	execution time, however.
;
;******************************************************************************

	SUBTTL	B$EVCK - New Stmt Event Trap Dispatch Handler
	PAGE
;***
; B$EVCK - New Stmt Event Trap Dispatch Handler
;
;Purpose:
; Event trap dispatcher. Pass thru here on each new statement. Process Event
; trap if event Enabled and Requested.
;
;Entry:
; None
;
;Exit:
; GOSUBs to event handler, if so indicated.
; [AL] = non-zero if untrapped ctl-Break has been hit (QB only)
; [AH] = non-zero if event was handled (QB only)
;
;******************************************************************************
cProc	B$EVCK,<PUBLIC,FAR>	
cBegin
	XOR	AX,AX		; a Zero to switch with
	XCHG	[b$EVTFLG],AL	; get and reset trapable event flag
	OR	AL,AL		; trapable event occured?
	JZ	NoPolling	; brif not -- don't bother to check traps

	MOV	SI,OFFSET DGROUP:B$PollDispTable ;point to dispatch table
	MOV	CX,NUM_TRAPDEV	;number of entries in table
PollLoop:			
	LODSW			;get next address from table
	OR	AX,AX		;is entry zero?
	JZ	SkipCall	;yes, don't try to call it
	PUSH	CX		;preserve counter across call
	CALL	AX		;call the poll routine
	POP	CX		;restore counter
SkipCall:			
	LOOP	PollLoop	;keep going until done

NoPolling:			
	TEST	[b$TRAP_SEM],0FFh ;(faster than CMP)
	JNZ	TRAP_OEIP	;Brif events

IgnoreIt:			
	CALL	QBEvReset	;Reset interp event flag if neccessary
	XOR	AH,AH		;event not handled
	RET			

TRAP_OEIP:
	TEST	[b$inonerr],0FFh ; Are we in ON ERROR routine?
	JNZ	IgnoreIt	; brif so -- ignore event

	MOV	SI,OFFSET DGROUP:b$TRAP_QUE ;get next enabled request
	CALL	B$GETQ
	mov	ah,al
	CALL	B$GETQ
	JZ	IgnoreIt	; don't process if queue underflow
	MOV	[TRPCUR],AX	; Save current trap address
	mov	bx,ax		;[bx]= trap table event addr
	XOR	SI,SI		
	CALL	B$FreeTrap	;Free Trap Request
	CALL	B$StopTrap	;Put STOP on Trap
	CALL	QBEvReset	;Reset interp event flag if neccessary
	CALL	B$EnsShowOutputScr ;Make sure output screen is active
	PUSH	b$cCSubs	
	PUSH	BX		;save trap table index
	TEST	BYTE PTR[BX],TRP_CN ;Is compiler handler active?
	JZ	CompHandler	;brif so
	MOV	b$cCSubs,0	;set interpreted code active
	INC	BX		; [BX] = ptr to interp handler context
	JMP	B$IEvHandler	;let interp handle gosub (DOESN'T RETURN)
CompHandler:			
	MOV	DX,[BX+3]	
	MOV	AX,[BX+1]	; [DX:AX] = Trap routine address
	cCall	B$FRAMESETUP	; Set up the frame
	INC	WORD PTR [BP].FR_GOSUB	 ; set gosub count to 1
	MOV	BX,[TRPCUR]	; Get current trap table address again

	INC	b$cNonQBIFrames ;bump count of NonQBIframes
	XOR	AX,AX
	PUSH	AX		;Push a 00 so RETURN works right
	jmp	dword ptr[BX+1] ;GOSUB the Trap routine...

cEnd	nogen

;***
; QBEvReset - Checks for more trappable events
;
;Purpose:
; Added as part of [19].
; Checks for more queued events.  If none, resets QB Event flag.
; It also returns value of b$EventFlags & CNTLC, indicating if untrapped
; Ctl-Break has occurred.
;
;Entry:
; None.
;Exit:
; AX - non-zero if untrapped Ctl-Break has occurred.
;Uses:
; None.
;Exceptions:
; None.
;****
cProc	QBEvReset,<NEAR>
cBegin
	CLI
	XOR	AX,AX
	CMP	[b$TRAP_SEM],AL ;any more events queued?
	JNZ	MoreEvents	;brif so
	CMP	[b$EVTFLG],AL	;any events happen since last check?
	JNZ	MoreEvents	;brif so
	CALL	B$IEvReset	;reset interpreter BOS flag for events
MoreEvents:
	STI
	MOV	AL,b$EventFlags	; get untrapped break flag
	AND	AX,CNTLC	; mask out all bits but CNTLC, & clear AH
cEnd

;***
;B$IFindEvHandler - Finds requested event handler context.
;
;Purpose:
;	Added as part of revision [24].
;	Entry point for QBI Scanner to find QBI context information
;	for an event handler.  This routine searches for the event
;	table for the closest "handler offset" for the requested
;	"segment". Note: the seg:off for the interpreter really map
;	to an oRs:oTx pair which defines the event handler.
;	When a handler matching the specified "segment" is found,
;	the entry will be marked as processed by placing FFFF in the
;	offset part of the handler context.
;Entry:
;	Seg - requested "Segment" to search for in event table.
;Exit:
;	AX - smallest "offset" found for requested "segment".
;	     0FFFFH if not found.
;	DX - DGROUP address of Event table entry.
;	Marks processed entries with 0FFFFH.
;Uses:
;	Per convention.
;Exceptions:
;	None.
;******************************************************************************
cProc	B$IFindEvHandler,<PUBLIC,FAR>,<SI,DI>
parmW	EvSeg
cBegin
	MOV	SI,OFFSET DGROUP:b$TRPTBL ;SI = points to event trap table
	MOV	CX,NUM_TRAPS*5		;CX = size of trap table
	XOR	DI,DI			;DI = points to smallest so far
	MOV	DX,0FFFFH		;DX = smallest

;	Walk the event table, looking for QB entries with matching segments
;	Return the offset of the smallest such entry, or FFFF if none.

EvFindLoop:
	LODSB				;get context byte
	XCHG	AX,BX			;BL = context byte
	LODSW				;AX = Handler offset
	TEST	BL,TRP_CN		;see if QBI handler
	JZ	EvFindSkip		;brif not

	MOV	BX,[SI] 		;BX = Handler Seg
	CMP	BX,EvSeg		;Segment of handler match search seg?
	JNZ	EvFindSkip		;skip entry if not

	CMP	AX,DX			;is Handler offset smallest?
	JAE	EvFindSkip		;brif not

;	Table entry is smallest so far encountered.  Remember value and address

	MOV	DI,SI			;point to it.
	XCHG	DX,AX			;remember lowest so far

EvFindSkip:
	LODSW				;skip seg
	SUB	CX,4
	LOOP	EvFindLoop		;process next table entry

	OR	DI,DI			;did we find one?
	JZ	NoMatch 		;brif not

	MOV	AX,DI			;AX = address of table entry
	DEC	AX			;back up to point at "offset"
	DEC	AX			

NoMatch:
	XCHG	AX,DX			;return lowest match in AX addr in DX
cEnd

;***
; B$EVTRET - Return from event trap routine
;
;Purpose:
; Reset STOPped bit, if trap ON, and restore frame pointer to previous frame.
;
;Entry:
; [BX]	= Trap table address
; top of stack contains long return address
;
;Exit:
; [AH] = non-zero if event was handled.
; [AL] = non-zero if an untrapped ctrl-break has occurred.
;
;Uses:
;
;Preserves: (optional)
;
;Exceptions:
;
;******************************************************************************
cProc	B$EVTRET,<FAR,PUBLIC>	
cBegin				
	CMP	[b$cCSubs],0	;are we returning from a compiled event?
	JZ	QBIEvRet	;brif not
	DEC	b$cNonQBIFrames ;adjust count of NonQBIframes
QBIEvRet:			
	POP	BX		;recover trap table address
	POP	[b$cCSubs]	;Recover context flag
	TEST	BYTE PTR [BX],TRP_ON ; Is Trap ON?
	JZ	TRAPRETX	; Brif not.
	CALL	B$ResetTrap	; else Reset Stopped bit.
TRAPRETX:			
	MOV	AX,0FFh SHL 8 + CNTLC ; return event handled (AH = -1)
	AND	AL,b$EventFlags	; return whether or not Break has occurred
				; (bit CNTLC of b$EventFlags)
cEnd				

	SUBTTL	ON Event GOSUB Statement handler
	PAGE
;***
; B$ONCA, B$ONKA, B$ONPA, B$ONLA, B$ONSA, B$ONTA, B$ONSG, B$ONMO, B$ONUE
;
; Syntax:	ON [Event] GOSUB line no.
;
;	WHERE:
;		Event:	     Arg:		traps:
;		    COM(x)    x in [1..NUM_RS232] COM [1..NUM_RS232]
;		       PEN			  Light pen.
;		  STRIG(x)    x =  <0|2>	  Trigger <A|B>
;		    KEY(x)    x in [1..NUM_TKEYS] Soft/Cursor/User defined keys
;		   PLAY(x)    x in [1..32]	  size(music-q) drops below x
;		  TIMER(n)    n in [1..86400]	  every n seconds
;		 SIGNAL(n)    n in [1..7]	  DOS signal n
;		  MOUSE(n)    n in [1..5]	  MOUSE function n
;		    UEVENT			  User defined event
;
; Entry:
;  parm1 =	TIMER:		A 4-byte integer.			[23]
;		PEN:		Far ptr to event handler subroutine	[23]
;		UEVENT: 	Far ptr to event handler subroutine	[40]
;		All Others:	A 2-byte integer.			[23]
;  parm2 = Far ptr to event handler subroutine (except for PEN)		[23]
;
; Exit:
;	The event trap address is saved.
;
;******************************************************************************


;#*****************************************************************************
;
; for those with a single word parameter:
;
cProc	B$ONFUN,<FAR,PUBLIC>	
parmW	placeHolder		; ensure generated exit code matches entries
parmD	fpHandler		
cBegin	<nogen> 		

	DEC	BX		; Want base 0.
	js	on_error	; if original arg is 0 then Ill fun error
	CMP	BL,CH		;Value [BL] .gt. MAX [CH]?
	JNB	ON_ERROR	;If so, then Ill fun error.
	MOV	CH,BL		; Save Event index in [CH]
	XCHG	AX,CX		; into AX (don't care about CX)
	ADD	AL,AH		; Final offset in al
	LEA	BX,[fpHandler]	; parm to settbl
	call	B$settbl	  
cEnd				

ON_ERROR:
	JMP	B$ERR_FC

;***
;B$settbl - save ON <event> GOSUB handler address in trap table
;
;Purpose:
;
;
;entry:
;	AL contains offset for appropriate event
;	BX contains a pointer to fpHandler, on the stack
;
;exit:
;
;
;Uses:
;
;
;Exceptions:
;      None.
;****
cProc	B$settbl,<NEAR,PUBLIC>,<SI>	 
cBegin

.erre	ID_SSEQDS
	MOV	SI,BX		
	CALL	B$TrapAddr	; modifies ax & bx only - address in bx
	LODSW			
	MOV	[BX+1],AX	; Put (far) handler address in Trap Table
	LODSW			
	MOV	[BX+3],AX	
	CALL	B$SetContext	;Set context bit in event flags
	CALL	B$SetChk	;Set Trap if On+Req and GOSUB <> 0.
cEnd				



	SUBTTL	B$EVNT_SET - Set Event ON, OFF or STOP
	PAGE
;***
; B$EVNT_SET -	  Sets Event ON, OFF or STOP
;
; Entry:	[DL] = ON, OFF or STOP Token.
;		[CH] = key # if KEY on
;		[CL] = Trap table index to Event.
;		[BL] = Event subscript.
;
; Exit: 	Event set in Trap Table, or Error.
; Preserves:
;		DI
;****

cProc	B$EVNT_SET,<NEAR,PUBLIC> 
cBegin				

	XCHG	AL,BL		; [AL] = Event subscript  ([BX] = garbage)
	ADD	AL,CL		; [AL] = Event Trap base + index.
	CALL	B$TrapAddr	;Get Event addr in Trap Table
	DEC	DL		; cheap test of ON/OFF/STOP token
.errnz	$ON - 0
	JS	EVON		; brif DL == $ON
.errnz	$OFF - 1
	JZ	EVOFF		; brif DL = $OFF
	;default - fall through to EVSTP
	DbAssertRelB	DL,z,$STOP-1,EV_TEXT,<gwaevt: B$EVNT_SET, invalid input>

;EVENT(n) ON, OFF and STOP (Enable EVENT Trapping)

EVSTP:
	JMP	B$StopTrap	;STOP TRAP
EVON:
	JMP	B$OnTrap	;TURN TRAP ON
EVOFF:
	JMP	B$OffTrap	;TURN TRAP OFF
cEnd	<nogen>			

	SUBTTL unsupported device code
	page

; this code consolidated with revision [35].

labelFP	<PUBLIC,B$ESG0>		; SIGNAL(n) ON
labelFP	<PUBLIC,B$ESG1>		; SIGNAL(n) OFF
labelFP	<PUBLIC,B$ESG2>		; SIGNAL(n) STOP
labelFP	<PUBLIC,B$ONSG>		; ON SIGNAL(n) GOSUB


labelFP	<PUBLIC,B$EMO0>		; MOUSE(n) ON
labelFP	<PUBLIC,B$EMO1>		; MOUSE(n) OFF
labelFP	<PUBLIC,B$EMO2>		; MOUSE(n) STOP
labelFP	<PUBLIC,B$ONMO>		; ON MOUSE(n) GOSUB

labelFP <PUBLIC,B$EUE0> 	; UEVENT ON
labelFP <PUBLIC,B$EUE1> 	; UEVENT OFF
labelFP <PUBLIC,B$EUE2> 	; UEVENT STOP
labelFP <PUBLIC,B$ONUE> 	; ON UEVENT GOSUB


cProc	AFE_PROC,<FAR>		
cBegin
	JMP	B$FrameAFE	; "advanced feature error" + frame setup
cEnd	<nogen>






;***
;B$TrapEvent - An event has been detected.  Set event flag
;
;Purpose:
;	Moved here with revision [39]
;	This routine is called when an event has been detected to
;	set the global event flag and notify the interpreter
;	that an event has occurred.
;Entry:
;IF	(b$EventFlags & InSLEEP)
;	AL = trap number
;ELSE
;	None.
;ENDIF
;Exit:
;	B$EVTFLG = 1
;Uses:
;IF	(b$EventFlags & InSLEEP)
;	AX
;ELSE
;	None.
;ENDIF
;Exceptions:
;	None.
;****
cProc	B$TrapEvent,<NEAR,PUBLIC>
cBegin
	MOV	b$EVTFLG,1	;set global event flag

	cCall	B$IEvSet	; call interp to set event BOS flag

	TEST	b$EventFlags,InSLEEP ; in SLEEP statement?
	JZ	NotSleep	; brif not -- don't do slow stuff

	PUSH	BX		; save register
	CALL	B$TrapAddr	; BX = *trap table entry for this event
	MOV	AL,[BX]		; get trap status
	AND	AL,TRP_ON or TRP_ST ; mask out all bits but ON and STOP
	CMP	AL,TRP_ON	; trap on and NOT stopped?
	POP	BX		; restore register
	JNE	NotSleep	; brif not enabled -- don't wake up SLEEP
	
labelNP	<PUBLIC,B$Wakeup>	; entry point for keyboard interrupt handler
				; forces wakeup of SLEEP
	AND	b$EventFlags,NOT (InSLEEP OR SLEEPtmr) ; clear flags to exit
				; SLEEP statement wait loop

NotSleep:


cEnd

sEnd	EV_TEXT 		
	END
