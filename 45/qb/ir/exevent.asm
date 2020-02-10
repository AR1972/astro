page	49,132
	TITLE	exevent.asm - event executors
;***
;exevent.asm - event executors
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;
;****************************************************************************

	.xlist
	include		version.inc
EXEVENT_ASM = ON
	IncludeOnce	executor
	IncludeOnce	exint
	IncludeOnce	opmin
	IncludeOnce	opcontrl
	IncludeOnce	opaftqb4
	IncludeOnce	context
	IncludeOnce	rttemp
	IncludeOnce	architec
	IncludeOnce	extort
	.list

assumes es, NOTHING
assumes ss, DATA

sBegin	DATA
sEnd	DATA

;The following constants are used to identify the event type. 

EV_COM		EQU	0d
EV_KEY		EQU	1d
EV_PEN		EQU	2d
EV_PLAY		EQU	3d
EV_SIGNAL	EQU	4d
EV_STRIG	EQU	5d
EV_TIMER	EQU	6d
EV_UEVENT	EQU	7d

sBegin	CODE
assumes cs, CODE

externNP	RtDispatch		

;==============================================================================
;	Event-specific executors - 
;		The following executors just push a constant which identifies
;		the associated event type, and dispatch.
;==============================================================================

MakeExe	exEvCom,opEvCom
	mov	al,EV_COM
	jmp	short Push_n_Disp

MakeExe	exEvKey,opEvKey
	mov	al,EV_KEY
	jmp	short Push_n_Disp

MakeExe	exEvPen,opEvPen
	mov	al,EV_PEN
	jmp	short Push_n_Disp

MakeExe	exEvPlay0,opEvPlay0
	SkipExHeader
MakeExe	exEvPlay1,opEvPlay1
	mov	al,EV_PLAY
	jmp	short Push_n_Disp

MakeExe	exEvSignal,opEvSignal
	mov	al,EV_SIGNAL
	jmp	short Push_n_Disp

MakeExe	exEvStrig,opEvStrig
	mov	al,EV_STRIG
	jmp	short Push_n_Disp

MakeExe	exEvTimer0,opEvTimer0
	SkipExHeader
MakeExe	exEvTimer1,opEvTimer1
	mov	al,EV_TIMER
Push_n_Disp:
	xor	ah,ah
	jmp	DispAx			;push event ID and dispatch

MakeExe exEvUEvent,opEvUEvent		
	mov	al,EV_UEVENT		
	jmp	short Push_n_Disp	

page
;==============================================================================
;	Postbyte tables -
;		The following 4 tables contain postbytes used in conjunction
;		with ExToRt.asm to allow a size-efficient gate to the runtime
;		for the 28 runtime event-specific entry points.
;==============================================================================

tON_GOSUB:
	db	PB$ONCA
	db	PB$ONKA
	db	PB$ONPA
	db	PB$ONLA
	db	PB$ONSG
	db	PB$ONSA
	db	PB$ONTA
	dw	PB$ONUE 		

;If any of the above 7 constants become larger than 255, make tON_GOSUB a 
;  table of words instead of bytes (for 2-byte postbytes)

tEVENT_ON:
	db	PB$ETC0
	db	PB$ETK0
	db	PB$EPE0
	db	PB$ETL0
	db	PB$ESG0
	db	PB$ETS0
	db	PB$ETT0
	dw	PB$EUE0 		

;If any of the above 7 constants become larger than 255, make tEVENT_ON a 
;  table of words instead of bytes (for 2-byte postbytes)

tEVENT_OFF:
	db	PB$ETC1
	db	PB$ETK1
	db	PB$EPE1
	db	PB$ETL1
	db	PB$ESG1
	db	PB$ETS1
	db	PB$ETT1
	dw	PB$EUE1 		

;If any of the above 7 constants become larger than 255, make tEVENT_OFF a 
;  table of words instead of bytes (for 2-byte postbytes)

tEVENT_STOP:
	db	PB$ETC2
	db	PB$ETK2
	db	PB$EPE2
	db	PB$ETL2
	db	PB$ESG2
	db	PB$ETS2
	db	PB$ETT2
	dw	PB$EUE2 		

;If any of the above 7 constants become larger than 255, make tEVENT_STOP a 
;  table of words instead of bytes (for 2-byte postbytes)

page
;==============================================================================
;	ON <event> GOSUB and <event> ON|OFF|STOP executors -
;		The following 4 executors all pop the event ID constant
;		pushed by an event-specific executor (guaranteed to have
;		just been executed), and use this as an index in one of the
;		associated 4 postbyte tables, above.
;==============================================================================

MakeExe	exEvGosub,opEvGosub
	LODSWTX				;ax = gosub target oTx
	pop	bx			;fetch event-ID index
	push	[grs.GRS_oMrsCur]	;pass oMrs for gosub target
	push	ax			;pass oTx for gosub target
	mov	ax,codeOFFSET tON_GOSUB	;start of ON <event> GOSUB postbytes
	jmp	short EventDisp

MakeExe	exEvOn,opEvOn
	mov	ax,codeOFFSET tEVENT_ON	;start of <event> ON postbytes
On_Off_Stop:
	pop	bx			;fetch event-ID index
EventDisp:
	cmp	bx,EV_UEVENT		;UEVENT?
	jz	UEvent			;brif so
	add	ax,bx			;ax = address of appropriate postbyte
	push	ax			;make this look like return address
	jmp	ExToRtDisp		;call runtime and dispatch

UEvent: 				
	add	bx,ax			;bx = address of appropriate postbyte
	mov	al,cs:[bx+1]		;2nd postbyte for UEVENT
	mov	ah,01H			
	mov	cx,CODEOFFSET DispMov	
	jmp	RtDispatch		;call runtime, dispatch on return

MakeExe	exEvOff,opEvOff
	mov	ax,codeOFFSET tEVENT_OFF ;start of <event> OFF postbytes
	jmp	short On_Off_Stop

MakeExe	exEvStop,opEvStop
	mov	ax,codeOFFSET tEVENT_STOP ;start of <event> STOP postbytes
	jmp	short On_Off_Stop

sEnd	CODE
end
