	TITLE	GWSOUND - GW BASIC 2.0 NOISE, SOUND, & BEEP support
;***
; GWSOUND - GW BASIC 2.0 NOISE, SOUND, & BEEP support
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - SOUND Statement:
;
;      SOUND freq, duration
;	 |
;	 |	    
;    B$SOND
;
;******************************************************************************
	include switch.inc
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	.xall
	.radix	10

	useSeg	SN_TEXT		
	useSeg	_BSS		
	useSeg	_DATA		

	include	seg.inc		
	INCLUDE	ibmunv.inc	; EQUates for B$DONOTE


sBegin	_BSS			

DURATION DW	1 DUP(?)
FREQUENCY DW	1 DUP(?)
DURASHN DW	2 DUP(?)	; need two words to store s.p. duration

sEnd	_BSS			



sBegin	SN_TEXT
	assumes CS,SN_TEXT

	externNP B$FrameAFE	; advanced function error + frame setup


	externNP B$ftolrnd	
	externNP B$fmldw     	

	externNP B$STRTSD     	
	externNP B$SNDWAT     	

	externNP B$DONOTE     

	externNP B$ERR_AFE	; advanced function error
	externNP B$ERR_FC	; function error



cProc	B$NOI,<FAR,PUBLIC>	
cBegin				
	JMP	B$FrameAFE	; sets up frame for error recovery
cEnd	<nogen>			


	SUBTTL	BEEP and SOUND Statements
	PAGE

	PUBLIC	B$BP0
	PUBLIC	B$BP1		; BEEP ON/OFF
	PUBLIC	B$SO0
	PUBLIC	B$SO1		;   SOUND ON/OFF


B$BP0:
B$BP1:
B$SO0:
	PUBLIC B$SND
B$SND:
cProc	B$SO1,<FAR>		
cBegin				
	JMP	B$FrameAFE	; sets up frame for error recovery
cEnd	<nogen>			



;***
;B$STOPSD
;Purpose:
;	Signal background task to flush voice queues and stop playing
;	music.
;Input:
;	none
;Output:
;	Preserves all registers
;****
cProc	B$STOPSD,<NEAR>,<AX>	
cBegin				
	MOV	AL,STPSND	;Function code to stop sound
	cCALL	B$DONOTE 	;Pass command to the OEM
	JC	FC_ERR		; function call error if B$DONOTE complains
cEnd				; End of B$STOPSD

;***
;B$SOND - accept parms 1&2 to sound statement
;
; Purpose:
;	Runtime Entry Point.
;	accept parms 1&2 to sound statement
;	Syntax: SOUND x,y[,[volume][,voice]]
;
;	Where:	x is the Frequency in Hertz.
;		y is the Duration in Clock ticks. (currently 18.2/sec).
;
;		Frequency must be at least 37 Hz.
;		If Duration is 0, then just turn off current sound...
;
; Input:
;	parm1 = frequency			[6]
;	parm2 = duration (s.p.)			[6]
; Output:
;****
cProc	B$SOND,<FAR,PUBLIC>,<ES,SI,DI> ;[7]
parmW	freq			
parmD	dur			
cBegin				
;
; When converting the duration into a real time quantity, the sound duration on
; the statement is specified in 55ms increments, but the B$DONOTE interface
; specifies it in 2.5ms increments. So we need to multiply the duration by 22,
; and then check for overflow. (error only if duration .gt. 64k)
;
	MOV	BX,[freq]	
	MOV	[FREQUENCY],BX	; save frequency
	LEA	SI,[dur]	; [si] points to SP duration
	MOV	DI,OFFSET DGROUP:DURASHN ; [di] points to save area
	PUSH	DS		
	POP	ES		; set ES=DS

	PUSH	DI		; save ptr to DURASHN
	MOVSW			; move first word of duration
	LODSW			; get second word of duration
	STOSW			; now we've moved s.p. duration, plus we've
				;	got second word in AX for testing below
	POP	BX		; restore ptr to s.p. DURASHN
	XCHG	AX,CX		; [CX] = second word of s.p. duration
	SHL	CX,1		; test duration - -
	JC	FC_ERR		; illegal function call if duration negative
	JZ	HLTSND		; stop sound & return if zero duration given

	FLD	DWORD PTR [BX]	; ST0 = duration (load from DS:BX to ST0)
	FLD	ST(0)		; copy for destructive compare

	CALL	B$ftolrnd	; pop copy from ST0, put in DX:AX as integer
	OR	DX,DX		; max. legal value of duration is 65535
	JZ	DUR_OK		; brif duration is legal

FC_ERR:				
	JMP	B$ERR_FC	; function call error - doesn't return
DUR_OK:
	MOV	BX,22D
	CALL	B$fmldw	; ST1 = duration, ST0 = 22D
	FMULP	ST(1),ST	; ST0 = 22 * duration
	CALL	B$ftolrnd	; DX:AX = 22 * duration
	MOV	CX,0FFFFH	; cache in register to save a couple bytes
	OR	DX,DX		
	JZ	LODDUR		; brif 22 * duration <= 64K

	MOV	AX,CX		;[3] put max value in AX (0FFFFH)
LODDUR:
	MOV	[DURATION],AX	; [duration] = [ax]
	MOV	DX,CX		;[2]default voice (0FFFFH)
	MOV	BX,DX		;default volume

SETVOI:
	; at this point, BX = volume
	; 		    DX = voice
	
	MOV	AH,DL		; assume user specified voice
	INC	DL
	JNZ	SNDES1		; go make noise if he did
	MOV	AH,DL		; he didnt so use 0

; Get the sound parameters from the first call and send them to the OEM
; routines.

SNDES1: 			; [bx] = volume
	MOV	DX,[DURATION]	; [dx]=duration
	MOV	CX,[FREQUENCY]	; [cx]=frequency
	OR	DX,DX		; if duration=0 then sound already
	JZ	SNDRET		; turned off -- exit
	CALL	B$SNDWAT	; wait until other sounds are done
DNT:
	MOV	AL,QUENOT	; function code to queue a note
	cCALL	B$DONOTE 	;start new sound.
	JNC	START		; brif no carry
	DEC	AL
	JZ	DNT		; try queuing again
	JMP	SHORT FC_ERR	; else function call error
START:
	CALL	B$STRTSD	;start sound
SNDRET: 			

cEnd				

HLTSND:				
				; range for B$SOND, below
	MOV	[DURATION],CX	; subsequent calls to B$SND use
				; zero duration!
	cCALL	B$STOPSD	; stop sound & exit
	JMP	SNDRET		; Exit gracefully



sEnd	SN_TEXT			

	END
