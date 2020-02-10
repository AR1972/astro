	PAGE	56,132
	TITLE	GWCEVT - Core of GW BASIC 2.0 Event Handler
;***
; GWCEVT - Core of GW BASIC 2.0 Event Handler
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module contains event trapping support which is needed
;	whether or not the user program does event trapping.
;
;******************************************************************************
	INCLUDE switch.inc
	INCLUDE rmacros.inc	

	UseSeg	EV_TEXT 	
	UseSeg	_BSS		
	UseSeg	_DATA		

	INCLUDE seg.inc 	
	INCLUDE baslibma.inc
	INCLUDE event.inc	; misc equates
	INCLUDE devdef.inc
	INCLUDE const.inc	; b$IOFLAG field definitions

sBegin	EV_TEXT 		
	INCLUDE queues.inc	;get structure and access routines
	externNP B$NearRet	
sEnd	EV_TEXT 		

sBegin	_DATA			
	globalW b$pInitKeys1,B$NearRet,1 
	globalW b$pInitKeys2,B$NearRet,1 
sEnd	_DATA			

sBegin	_BSS			
	externW b$cCSubs	;Compiled sub nesting level

;	Trap table and flag for ON <event> GOSUB trap.

	globalB b$TRAP_SEM,,1	;nonzero if event dispatch queued
	EVEN

	labelB	<PUBLIC,b$TRPTBL>	    

	staticB b$TRP_RS232,,<5*NUM_RS232> ;NUM_RS232 COM traps.

	globalB b$TRP_LITEPEN,,<5*NUM_LITEPEN> ;NUM_LITEPEN PEN traps.

	staticB b$TRP_TKEYS,,<5*NUM_TKEYS> ;NUM_TKEYS KEY traps.

	staticB b$TRP_JOYST,,<5*NUM_JOYST> ;NUM_JOYST TRIG traps.

	staticB b$TRP_SOUND,,<5*NUM_SOUND> ;NUM_SOUND PLAY traps.

	staticB b$TRP_TIMER,,<5*NUM_TIMER> ;NUM_TIMER TIMER traps.




	labelB	<PUBLIC,b$TRPTBLEND> 

	EVEN			; Trap table must be an even number
				; of bytes
	PUBLIC	b$TRAP_QUE	
b$TRAP_QUE QUEUE<>		;Active Trap Queue Control Block

	DW	(NUM_TRAPS+1) DUP(?) ; event gosub addrs go here

sEnd	_BSS			

externFP B$IEvSet		;QB interp routine to set EVENT BOS flag

assumes CS,EV_TEXT		
sBegin	EV_TEXT 		



	SUBTTL	TRAP ROUTINES - ON/OFF/STOP/REQUEST/FREE/RESET

;***
;B$OnTrap , B$OffTrap , B$StopTrap
;
;entry: BX - index to b$TRPTBL
;	CH - KEY# if present (1 based)
;	CL - <evt>OFF
;	SI - low level routine, if present
;****

;TURN TRAP ON

cProc	B$OnTrap,<PUBLIC,NEAR> 
cBegin				
	test	byte ptr[bx],TRP_OS ; don't enable if already active
	jnz	on_already
	mov	al,enable_trap	; tell low-level
	call	set_oem_trap	; to start trapping [key #ch]
on_already:
;	CLI
	MOV	AL,[BX]
	AND	AL,TRP_RQ+TRP_CN ;LEAVE REQUEST BIT and Context bit
	OR	AL,TRP_ON	;Set Event Trapping ON.
	CMP	AL,[BX]
	MOV	[BX],AL
	JZ	ONTRP0		;NO CHANGE IN STATUS
	AND	AL,TRP_RQ
	JNZ	SETTRP		;Activate Delayed Trap Request
ONTRP0:
;	    STI
cEnd				

;TURN TRAP OFF

cProc	B$OffTrap,<PUBLIC,NEAR> 
cBegin				
	mov	al,disable_trap ; tell low-level
	call	set_oem_trap	; to stop trapping [key #ch]
;	    CLI
	MOV	AL,[BX] 	;get current status
	AND	AL,TRP_CN	;reset all but context bit
	XCHG	AL,[BX] 	;store new status
	JMP	SHORT FRECHK	;FREE OUTSTANDING REQUEST
cEnd	<nogen> 		

;STOP TRAP

cProc	B$StopTrap,<PUBLIC,NEAR> 
cBegin				  
	test	byte ptr[bx],TRP_OS ; don't enable if already active
	jnz	stopped_already
	mov	al,enable_trap	; tell low-level (if present)
	call	set_oem_trap	; to start trapping [key # in ch]
stopped_already:
;	    CLI 		    ;	 but don't mark as stopped
	MOV	AL,[BX]
	PUSH	AX
	OR	AL,TRP_ST	;Set Event Trap Stopped.
	MOV	[BX],AL
	POP	AX
FRECHK: 			;entry point for B$OffTrap
	AND	AL,TRP_MSK	;mask out context bit
	XOR	AL,TRP_ON+TRP_RQ ;if on and requested,
	JZ	FRETP2		;FREE THIS TRAP
;	    STI
cEnd				

;***
;set_oem_trap
;
;Purpose:
;
;Entry: al = enable/disable function
;	si = oem routine ($RD<evt>)
;	cl = trap table offset
;	ch = key number
;
;Exit:
;
;Uses:
;
;Exceptions:
;
;****

cProc	set_oem_trap,<NEAR>	
cBegin				
	or	si,si
	jz	on_no_oem	; no oem routine if 0
	push	dx
	mov	dl,ch
	xor	dh,dh		; dx = key# (if calling B$RDKYBD)
	PUSH	BX		; Preserve trap table address
	call	si
	POP	BX		
	pop	dx
on_no_oem:
cEnd				

;***
;B$ResetTrap  - RESET STOP ON TRAP
;
;Purpose:
;
;
;Entry:
;
;
;Exit:
;
;
;Uses:
;
;
;Exceptions:
;
;****

cProc	B$ResetTrap,<PUBLIC,NEAR> 
cBegin				   
;	    CLI
	MOV	AL,[BX]
	AND	AL,TRP_ON+TRP_RQ+TRP_CN ;preserve ON,REQUESTED, and CONTEXT
	CMP	AL,[BX]
	MOV	[BX],AL
	JNZ	SETCHK_AL	;If new Status, may want to Request Trap
;	    STI
cEnd				

;***
;B$ReqTrap  - REQUEST TRAP
;
;Purpose:
;
;
;Entry:
;
;
;Exit:
;
;
;Uses:
;
;
;Exceptions:
;
;
;****

cProc	B$ReqTrap,<PUBLIC,NEAR> 
cBegin				 
;	    CLI
	MOV	AL,[BX]
	TEST	AL,TRP_OS
	JZ	REQTPX		;TRAP NOT ON OR STOPPED
	OR	AL,TRP_RQ
	CMP	AL,[BX]
	JZ	REQTPX		;NO CHANGE
	MOV	[BX],AL 	;Remember Request
	TEST	AL,TRP_ST	;Stopped?
	JNZ	REQTPX		;Yes don't P-sem
cEnd	<nogen> 		; fall into B$SetChk

;***
;B$SetChk
;
;Purpose:
;
;
;Entry:
;
;
;Exit:
;
;
;Uses:
;
;
;Exceptions:
;
;****

cProc	B$SetChk,<PUBLIC,NEAR> 
cBegin				
;	    CLI
	MOV	AL,[BX] 	;Get Flags
SETCHK_AL:			;Entry point for B$ResetTrap
	AND	AL,TRP_MSK	;mask out context bit
	XOR	AL,TRP_ON+TRP_RQ ; If Trap On and Requested..
	JZ	SETTRP		; then check addr and Queue the Trap.
REQTPX: 			;Exit point for B$ReqTrap
;	    STI
cEnd				

;SET THE TRAP

cProc	SETTRP,<NEAR>
cBegin
	CMP	WORD PTR [BX+1],-1 
	JZ	settrp_exit	   ;can't ON xxx GOSUB 0
	PUSH	AX
	PUSH	SI
	MOV	SI,OFFSET DGROUP:b$TRAP_QUE 
	mov	al,bh
	CALL	B$PUTQ		;queue MSB of trap entry
	mov	al,bl
	CALL	B$PUTQ		;queue LSB of trap entry
	jz	qovflo		;don't record event if q overflow
	INC	[b$TRAP_SEM]	
	CALL	B$IEvSet	;notify interp of delayed activation
qovflo: POP	SI
	POP	AX
;	    STI

settrp_exit:			

cEnd

;***
;B$FreeTrap - FREE TRAP
;
;
;Purpose:
;
;
;Entry:
;
;
;Exit:
;
;
;Uses:
;
;
;Exceptions:
;
;
;****

cProc	B$FreeTrap,<PUBLIC,NEAR> 
cBegin				  
;	    CLI
	MOV	AL,[BX]
	AND	AL,TRP_OS+TRP_CN ;preserve ON,STOP,and CONTEXT
	CMP	AL,[BX] 	;was a trap requested?
	MOV	[BX],AL
	JZ	FRETPX
FRETP2:
	MOV	AL,[b$TRAP_SEM] ;V-sem if was active request
	SUB	AL,1
	JB	FRETPX
	MOV	[b$TRAP_SEM],AL 
FRETPX:
;	    STI
cEnd
				
;***
;B$SetContext - Set context bit in event flags entry
;
;Purpose:
;	Added as part of revision [3].
;	This routine is called when an ON <event> GOSUB is executed.
;	For the interpreter, the GOSUB routine may be in either
;	interpreted code, or compiled code.  A bit in the event flags
;	is used to determine the context of the handler.  When the bit
;	is set, the handler is an interpreter handler, otherwise, it
;	is a compiled code handler.
;Entry:
;	[BX] - points to event table entry.
;	b$cCSubs - 0 means interpreted code is executing
;Exit:
;	Flags byte in Event dispatch table is updated to reflect
;	handler context.
;Uses:
;	AL,CX
;Exceptions:
;	None.
;****
cProc	B$SetContext,<PUBLIC,NEAR> 
cBegin
	OR	BYTE PTR[BX],TRP_CN ;default to interp handler
	MOV	CX,b$cCSubs	;get current execution context
	JCXZ	InterpActive	;brif interp active
	AND	BYTE PTR[BX],TRP_MSK ;reset interp handler bit
InterpActive:
cEnd

;*** 
;B$TestTrap -- check if trapping on for a given trap #
;
;Purpose:
;
;Entry:
;	AL = Trap number
;Exit:
;	BX = pointer to trap table entry for this trap
;	NZ if Trapping ON or STOPPed for a trap in AL
;Uses:
;	AX
;Preserves:
;	CX,DX
;Exceptions:
;	None
;
;******************************************************************************
cProc	B$TestTrap,<PUBLIC,NEAR>	
cBegin					
	CALL	B$TrapAddr		; BX = *trap table entry
	TEST	BYTE PTR [BX],TRP_OS	;Non-zero if Trapping ON.
cEnd					

;*** 
;B$TrapAddr -- get trap table entry for a trap #
;
;Purpose:
;
;Entry:
;	AL = Trap number
;Exit:
;	BX = pointer to trap table entry for this trap
;Uses:
;	AX
;Preserves:
;	CX,DX
;Exceptions:
;	None
;
;******************************************************************************
cProc	B$TrapAddr,<PUBLIC,NEAR> 	
cBegin					
	MOV	BX,OFFSET DGROUP:b$TRPTBL 
	CBW
	ADD	BX,AX			; [BX] = [BX]+AL
	SAL	AX,1
	SAL	AX,1			; [AX] = [AL]*4
	ADD	BX,AX			; [BX] = [BX]+[AL]*5
cEnd					


sEnd	EV_TEXT 		
	END
