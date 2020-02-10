	TITLE	LLCOM3 - Communications Interface for DOS 3

	PAGE	56,132

;***
; LLCOM3.ASM - GW-BASIC Communications Interface for DOS 3
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	This module supports the initialization/open, read, write, line
;	status check, termination and interrupt/monitor service for
;	communication lines.  In DOS3, BASIC handles the actual low level
;	functions itself, whereas in DOS5, BASIC takes advantage of the dos
;	function calls.
;
;******************************************************************************

INCLUDE switch.inc		;feature switches [new file]
INCLUDE rmacros.inc		

;	Code Segments

	useSeg	DV_TEXT 	
	useSeg	RT_TEXT 	
	useSeg	EV_TEXT 	

;	Data segments

	useSeg	_DATA		
	useSeg	_BSS		

INCLUDE seg.inc 		
INCLUDE ibmunv.inc
INCLUDE	baslibma.inc		; skip macro
INCLUDE comdcb.inc		;comm data control block (DCB) structure
INCLUDE event.inc 		
INCLUDE idmac.inc		

	SUBTTL	local constant/structure definitions

QUE_CTRL_LEN =	QUSIZE		;Length of Queue Control  Block

RS232B	=	400H		; X'400' RS232 Card(s) I/O addr Save area.

COMDEB	STRUC
	DEVID	DB	?	;device id number
	COMBSY	DB	0	; nonzero if Tx Interrupts active
	COMRTS	DB	2	; Request to Send Mask bit if RTS wanted
	COMCTS	DW	?	; CTS timeout count
	COMDSR	DW	?	; DSR timeout count
	COMRLS	DW	?	; CD (RLSD) timeout count
	COMOVF	DB	0	; Nonzero on overflow
	COMERR	DB	0	; Nonzero if I/O error
	MSRERR	DB	0	; Nonzero if lost DSR
	MSREG	DB	?	; Contents of Modem Status Register
	PARTYP	DB	?	; Parity type (internal form see B$INICOM)
	BYTSIZ	DB	?	; Size of byte(internal form - see B$INICOM)
	CTRLZ	DB	0	; ASCII mode EOF flag
	BINMOD	DB	0	; BIN/ASCII (Z/NZ) flag
	COMPE	DB	?	;nonzero if PE option selected
	RCVSEG	DW	?	;segment of receive queue buffer
	XMTSEG	DW	?	;segment of transmit queue buffer
COMDEB	ENDS

PAUSE	MACRO			;macro to insure that an instruction
	JMP	$+2		;fetch occurs between IN and/or OUT
	ENDM			;instructions on the Salmon machine

	SUBTTL	data definitions
	page
sBegin	_DATA			

	COMM1	COMDEB	<>	;COM1 device equipment block
	COMM2	COMDEB	<>	;COM2 device equipment block

sEnd	_DATA			

sBegin	_BSS			

	externB	b$COFlag	; non-zero indicates called from COM_OPEN
	externB	b$EventFlags	; misc. event flags
	externW	b$IPOLKEY	; vector for B$POLKEY routine
	externB b$DOS_INT_MASK ;defined in LLINI.ASM
	externW	b$ComPort	;I/O port address table

	staticB COM1_VECTOR,?,2	; old COM1 vector is saved here
	staticB COM2_VECTOR,?,2	; old COM2 vector is saved here
	externW	b$pTrapEvent	; pointer to B$TrapEvent if events linked in

Q_IN	QUE_CTRL_BLOCK <>	;Input Que Ctrl Data Blk Ptr
Q_IN2	QUE_CTRL_BLOCK <>

Q_OUT	QUE_CTRL_BLOCK <>	;Output Que Ctrl Data Blk Ptr
Q_OUT2	QUE_CTRL_BLOCK <>
sEnd	_BSS			

	SUBTTL	code externals	
	page

sBegin	RT_TEXT 		
	externW b$BASDSG	; in code segment to record basic data seg
sEnd	Rt_TEXT 		

sBegin	EV_TEXT 		
	externNP B$BREAK_CHK	;check ctrl-break
sEnd	EV_TEXT 		

sBegin	DV_TEXT 		
	externNP	B$INIQUE	
	externNP	B$QUE	
	externNP	B$DQUE	
sEnd	DV_TEXT 		

	SUBTTL	Comm initialization
	page
	assumes CS,DV_TEXT	
sBegin	DV_TEXT 		


;***
;B$INICOM - initialize the COM device
;[OEM documentation is in file LLCOM5]
;
;Purpose:
;	Initialize the requested RS232 port, if present, and set
;	up the port with the given attributes, when they are valid.
;	The memory for the input and output queues has already been
;	allocated, but will need to be initialized.  This routine is
;	passed the address of a device control block (DCB) as defined
;	in the file comdcb.inc.
;
;	The signals RLSD, CTS, and DSR should bbe ignored by all
;	COM routines if the curresponding Support of Timeout Flags
;	by BIOS is optional.
;
;IBM Communications Error Reporting
;	The following scheme is used in reporting errors:
;
;	1.) OPEN returns any OPEN specific erros and resets the input buffer.
;	2.) CLOSE, EOF, LOC, LOF, and device polling for trapping returns
;	    no error.
;	3.) INPUT returns only input errors.
;	4.) WRITE and PRINT return only output errors.
;	5.) Errors are reported at the earliest possible BASIC I/O
;	    statement (with respect to the rules above).
;	6.) I/O errors are cleared when they are reported to the runtime
;	    code and the byte being read or written is not removed from
;	    or added to the buffer.
;	7.) If an error occurs when reading a byte and the port is opened
;	    with less than eight data bits, then set the high bit of that
;	    byte.
;
;Entry:
;	BX = points to initialized DCB.
;
;Exit:
;	AH = 0	if no errors occured
;	   = 4	if timeout while waiting for DSR signal
;		(will cause a DEVICE TIMEOUT error)
;	   = 5	if timeout while waiting for RLSD signal
;		(will cause a DEVICE TIMEOUT error)
;	   = FE if device unavailable via BIOS data location
;		(will cause a DEVICE NOT AVAILABLE error)
;	   = FF if requested mode is not supported
;		(will cause a BAD FILE NAME error)
;
;Uses:
;	Per Convention
;
;Exceptions:
;	None.
;*************************************************************************
;
;Algorithm:
;	If DOS 3,
;		check for valid device (0 or 1)
;		if card is present enable it else ERROR
;		initialize buffer control blocks
;		get parity and bytesize
;		if parity=NONE and bytesize=4 then ERROR
;		if parity<>NONE and bytesize=8 then ERROR
;		map parity to easier value
;			(0->0, 1->1, 2->3, 3->5, 4->7)
;		save new parity value
;		get baud rate
;		determine and save divisor for setting baud rate
;		set up card with appropriate characteristics

cProc	B$INICOM,<PUBLIC,NEAR>,<DI,SI> ; save DI, SI

cBegin				
	MOV	AH,0FFH 	;assume requested mode unsupported
	MOV	DL,[BX].CD_DEVID ;get device i.d.
	CMP	DL,1		;is i.d. > 1
	JBE	DeviceOK	;jump if device is 0 or 1
	JMP	INIRET		;near jump to error
DeviceOK:			
	MOV	SI,OFFSET DGROUP:COMM2 ;get com2 deb (dcb in dos5)
	JZ	IdOK		; br. if com2
	MOV	SI,OFFSET DGROUP:COMM1 ;get com1 deb (dcb in dos5)
IdOK:				

	MOV	[SI].DEVID,DL	;save device i.d.
	XOR	DH,DH
	MOV	DI,DX		;copy offset
	SHL	DI,1		;make word index (COM1=0 / COM2=2)
	CALL	InitComPort	;initialize the port, int vector, etc.
	OR	AX,AX		;test if error reported
	JZ	InitComAvail	;if zero, then initialization successful
	JMP	INIRET		;else exit with CH set with 0FEH for DNA
InitComAvail:			

	MOV	DX,[BX].CD_RXSIZ ;get size of the receive buffer
	MOV	CX,[BX].CD_TXSIZ ;get size of the transmit buffer
	CALL	BUFFINI 	;initialize buffer stuff

	MOV	CL,[BX].CD_CMFLG ;get com flags

	MOV	[SI].COMBSY,0	;not initially transmitting
	MOV	[SI].COMRTS,2	;set RTS mask
	AND	[SI].COMRTS,CL	;should RTS be enabled?
	MOV	[SI].COMPE,4D	;set PE mask
	MOV	CL,[BX].CD_CMFLG ;get com flags
	AND	[SI].COMPE,CL	;should PE be enabled?
	MOV	CX,[BX].CD_RXSEG ;get segment of receive comm buffer
	MOV	[SI].RCVSEG,CX	;set segment of receive comm buffer
	MOV	CX,[BX].CD_TXSEG ;get segment of transmit comm buffer
	MOV	[SI].XMTSEG,CX	;set segment of transmit comm buffer

	XOR	CX,CX		;assume no timeout value
	CMP	[BX].CD_RLSTO,CX ;test if RLSD timeout to be tested
	JZ	InicomNoCD	;if not, then leave timeout as zero
	MOV	CX,[BX].CD_OPNTO ;get OPEN timeout value
InicomNoCD:			
	MOV	[SI].COMRLS,CX	;set RLSD timeout count

	XOR	CX,CX		;assume no timeout value
	CMP	[BX].CD_DSRTO,CX ;test if DSR timeout to be tested
	JZ	InicomNoDS	;if not, then leave timeout as zero
	MOV	CX,[BX].CD_OPNTO ;get OPEN timeout value
InicomNoDS:			
	MOV	[SI].COMDSR,CX	;set DSR timeout count

	MOV	[SI].COMCTS,0	;don't check CTS during initialization
	MOV	[SI].COMOVF,0	;clear overflow flag
	MOV	[SI].COMERR,0	;clear I/O error flag
	MOV	[SI].MSRERR,0	;clear modem status flag
	MOV	[SI].MSREG,0	;clear modem error value
	MOV	DH,[BX].CD_BYTSZ ;get number of data bits in byte in [DH]
	MOV	DL,[BX].CD_PARIT ;get requested parity in [DL]
	OR	DL,DL		;is parity "NONE"?
	JZ	ChkParity	; br. if so
	CMP	DH,8		;is byte size 8?
	JNZ	ByteSizeOK	; Brif not
ErrorJmp:			; for jump out of range
	MOV	AH,0FFH 	; require mode not support
	JMP	SHORT TRMRET	; else error (reset interrupt vectors)

ChkParity:			
	CMP	DH,4		;was it no parity and byte size 4?
	JZ	ErrorJmp	; Brif yes, error
ByteSizeOK:			; the following is set line status and baud
				;  rate
	SUB	DH,5		;shift byte size to bits 0&1
	MOV	[SI].BYTSIZ,DH	;save byte size
	MOV	DH,DL		;copy parity
	CMP	DL,2		;does parity need adjustment?
	JB	ParityOK	; br. if not
	MOV	DH,1		;[DH]=new parity value
	DEC	DL		;[DL] = Loop count
AdjParLoop:			; keep adjust
	INC	DH		;parity 0 -> 0, parity 1 -> 1
	INC	DH		;parity 2 -> 3
	DEC	DL		;parity 3 -> 5
	JNZ	AdjParLoop	; parity 4 -> 7
ParityOK:			
	MOV	[SI].PARTYP,DH	;store adjusted parity value
	MOV	AL,[BX].CD_STOPB ;get number of stop bits 0=1,1/2= .GT. 1
	DEC	AL		;Find out if 0 stop bits?
	JS	StopBitOK	; Brif sign, must have been zero stop bits
				;Request > 1 stop bit if by ORing with BIT 2 on
	OR	BYTE PTR [SI].BYTSIZ,00000100B
				;[BYTSIZ] is number of data bits and stop bits
StopBitOK:			
	MOV	CX,[BX].CD_BAUDR ;get requested baud rate
	CALL	GETDIV		;get necessary divisor to support baud rate
	MOV	AH,0FFH		;set error code if baud not supported
	JCXZ	TRMRET		;reset vector if not and give error
	CALL	SETEM		;set up RS232 card
	OR	AH,AH		;test if error occurred
	JNZ	TRMRET		;if so, then reset vector and jump

	MOV	CX,[BX].CD_RLSTO ;get RLSD time out count
	MOV	[SI].COMRLS,CX	;set RLSD timeout count
	MOV	CX,[BX].CD_DSRTO ;get DSR time out count
	MOV	[SI].COMDSR,CX	;set DSR timeout count
	MOV	CX,[BX].CD_CTSTO ;get CTS timeout count
	MOV	[SI].COMCTS,CX	;store real CTS timeout count

	JMP	SHORT INIRET

TRMRET:
	PUSH	AX		;save registers...
	PUSH	BX
	PUSH	CX
	PUSH	DX
	XOR	CX,CX		; reset DTR
	MOV	[b$COFlag],CL	; Clear flag for next call to MSRWAT!
	CALL	CLSMSR		;full termination - bring lines down
	POP	DX		;restore registers...
	POP	CX
	POP	BX
	POP	AX
	CMP	AH,0FCh		; was CTRL-BREAK detected in MSRWAT?
	JNE	INIRET		; brif not
	CALL	B$BREAK_CHK	; process CTRL-BREAK -- should not return
DbHalt	DV_TEXT,<CTRL-BREAK lost during COM OPEN in B$INICOM>	;

INIRET:
cEnd	;B$INICOM		; pop register and exit
	page			
;***
; InitComPort - Initialize a communications port
;
; Purpose:
;	First tests if device is available through the BIOS data location
;	(0:400H for COM1 - 0:402H for COM2) which contains the I/O data port.
;	If the device is available, then set up the ISR vector and initialize
;	the hardware.
; Entry:
;	DI = device index (COM1=0 / COM2=2)
; Exit:
;	AH = 0FEH if device is not available.
;	AX = 0 if device is available.
; Modifies:
;	AX, DX.
; Exceptions:
;	None.
;*************************************************************************

;	First, determine if the device is available by examining the
;	BIOS location of the I/O port number.  If zero, it is unavailable.
;	If nonzero, clear the value for exclusive use and store the port
;	address in the DCB.

InitComPort:
	PUSH	DS		;save DGROUP on stack
	XOR	DX,DX		;clear DX for use as BIOS segment
	MOV	DS,DX		;set segment to access BIOS data area
	XCHG	DX,DS:RS232B[DI] ;get I/O address in DX, clear BIOS data
	POP	DS		;restore DGROUP from stack

	MOV	b$ComPort[DI],DX ;store I/O port address in DCB
	OR	DX,DX		;test if device is available
	JNZ	ComPortAvail	;if so, then jump
	MOV	AH,0FEH		;set CH for "device not ready" error
	RET			;do a near return to caller with CH=0FEH

;	With the device index in DI and the I/O port address in DX,
;	initialize the hardware and interrupt vectors.

ComPortAvail:
	PUSH	BX		;save registers...
	PUSH	SI

	MOV	SI,OFFSET CODE:COM_INT1 ;COM1: Service addr
	MOV	BX,OFFSET DGROUP:COM1_VECTOR ;get offset for saved vector

	OR	DI,DI		;test if COM1 is being initialized
	JZ	ComPortStart	;if so, then start the initialization

	MOV	SI,OFFSET CODE:COM_INT2 ;COM2: Service addr
	MOV	BX,OFFSET DGROUP:COM2_VECTOR ;get offset for saved vector
ComPortStart:

	MOV	CX,0EF00H+IRQ4/4 ;8259 enable mask/int number (primary)

	CMP	b$ComPort[DI],03F8H ;test if primary asynch card
	JE	ComPortPrimary	;if so, then jump

	MOV	CX,0F700H+IRQ3/4 ;8259 enable mask/int number (secondary)
ComPortPrimary:			

	CLI			;disable interrupts during initialization
	ADD	DX,4		;Modem Control Register
	MOV	AL,1		; AL = 1 = DTR active; RTS off
	OUT	DX,AL		; Turn of RTS; leave DTR active
	PAUSE			;Delay
	SUB	DX,3		;Interrupt Enable Register
	DEC	AX		; AL = 0 = disable interrupts
	OUT	DX,AL		;Turn off interrupts
	MOV	DX,SI		;Set IRQ-4 Vector
	MOV	SI,BX		;get pointer to save vector addr
	MOV	AL,CL		;get interrupt number
	MOV	AH,35H		;get vector function call
	INT	21H		;get vector in ES:BX
	MOV	[SI],BX 	;store away current vector offset
	MOV	[SI+2],ES	;and likewise the current segment
	PUSH	DS		;get BASIC data segment
	POP	ES		;and restore ES with it
	PUSH	CS		;DS = CS
	POP	DS
	MOV	AH,25H		;DOS Set Vector Function
	INT	21H
	PUSH	ES		;save BASIC data segment
	POP	DS		;Original DS
	MOV	AH,CH		;get mask to AND with current one
	IN	AL,INTA1	;get interrupt mask for 8259
	AND	AL,AH		;include IRQ-3 or IRQ-4
	PAUSE			;to give settle time for I/O bus signals
	OUT	INTA1,AL	;output the new interrupt mask
	XOR	AX,AX		;clear AX for no error
	STI			;re-enable interrupts

	POP	SI		;restore registers...
	POP	BX
	RET			;near return to caller

;***
; BUFFINI - initialize COM buffers
;
; Purpose:
;	This routine is responsible for initializing the queue control
;	block field elements for the input and output buffers.
; Entry:
;	DX = size of receive buffer.	
;	SI = pointer to deb.
; Exit:
;	None.
; Modifies:
;	None.
; Exceptions:
;	None.
;***************************************************************************

BUFFINI:
	PUSH	SI
	PUSH	BX
	PUSH	AX
	MOV	BX,OFFSET DGROUP:Q_OUT ;get output QCB for COM1
	CMP	[SI].DEVID,0	;is this com1?
	JZ	BUFF2		;jump if so
	MOV	BX,OFFSET DGROUP:Q_OUT2 ;[BX]= Out QCB
BUFF2:
	XOR	AX,AX		;each buffer starts at zero
	MOV	[BX].QUEBOT,AX	;Store Out buffer Bottom addr
	cCALL	B$INIQUE 	; initialize queue
	MOV	[BX].QUETOP,CX	;Store Out buffer Top addr
	MOV	[BX].QUELEN,CX	;Store Out Queue length

	MOV	BX,OFFSET DGROUP:Q_IN ;BX points to COM1 input QCB
	CMP	[SI].DEVID,0	;test if COM1
	JZ	BUFF4		;branch if so
	MOV	BX,OFFSET DGROUP:Q_IN2 ;[BX] points to in_QCB
BUFF4:				
	XOR	AX,AX		;each buffer starts at zero
	MOV	[BX].QUEBOT,AX	;Store In buffer Bottom addr
	cCALL	B$INIQUE 	; initialize queue
	MOV	[BX].QUETOP,DX	;store in_buf1 top addr
	MOV	[BX].QUELEN,DX	;store in_queue length
	POP	AX
	POP	BX
	POP	SI
	RET

;***
; GETDIV
;
; Purpose:
;	gets baud rate divisor from table.
; Entry:
;	[CX] = requested baud rate
; Exit:
;	[CX] = baud rate divisor
;	       0 means invalid baud request
; Modifies:
;	None.
;***************************************************************************

GETDIV:
	PUSH	BX
	PUSH	DX
	MOV	BX,OFFSET CODE:BAUD_TBL-2 ;get address of baud table - 2
	MOV	DX,CX		;copy baud rate for compare
GETRA1:
	INC	BX
	INC	BX
	MOV	CX,CS:[BX]	; Get rate from Table
	INC	BX
	INC	BX		; (Baud value)
	JCXZ	GETEXT		;EOT, Bad Value given
	CMP	DX,CX
	JNZ	GETRA1		; No match, look next
GETEXT:
	MOV	CX,CS:[BX]	; [CX] = Baud Divisor
	POP	DX
	POP	BX
	RET

BAUD	MACRO	RATE,RATDIV
	DW	RATE
	DW	RATDIV
	ENDM

BAUD_TBL:
	BAUD	75,1536 	; 75
	BAUD	110,1047	; 110
	BAUD	150,768 	; 150
	BAUD	300,384 	; 300
	BAUD	600,192 	; 600
	BAUD	1200,96 	; 1200
	BAUD	1800,64 	; 1800
	BAUD	2400,48 	; 2400
	BAUD	4800,24 	; 4800
	BAUD	9600,12 	; 9600
	BAUD	19200,6 	; 19200
	BAUD	0,0		; End

;***
;SETEM
;
; Purpose:
;	Place the RS232 card in a deterministic initial state.	This
;	routine initializes the baud rate, parity, word size, and
;	INS 8250 interrupt enable register for the card.
; Entry:
;	CX = baud rate divisor
;	SI = pointer to deb for this device
;	DI = word index to COM device
; Exit:
;	AH = 0 means no errors occurred
;	     4 means time out occurred while waiting for DSR
;	     5 means time out occurred while waiting for RLSD
; Modifies:
;	AX,CX,DX
;**************************************************************************

SETEM:
	MOV	DX,b$ComPort[DI] ;Get I/O Base Addr.
DbAssertRel	DX,NE,0,DV_TEXT,<LLCOM3.ASM: Com I/O address = 0 in SETEM>
	ADD	DX,3		;Line Ctrl Reg.
	MOV	AL,80H		;Set DLAB=1
	OUT	DX,AL		;Enable Baud Latch
	SUB	DX,2		;MSB of Baud Latch
	MOV	AL,CH
	PAUSE			;make sure instruction fetch has occurred
	OUT	DX,AL		;Set MSB of Div
	DEC	DX
	MOV	AL,CL
	PAUSE			;make sure instruction fetch has occurred
	OUT	DX,AL		;Set LSB of Div
	MOV	CH,[SI].PARTYP	;Get Parity
	MOV	CL,3
	SHL	CH,CL		;Shift Parity to D4-D3.
	MOV	AL,[SI].BYTSIZ	;Get Byte size (5,6,7,8)
	OR	AL,CH		;[AL] = Parity+Word size.
	ADD	DX,3		;Line Ctrl Reg.
	PAUSE			;make sure instruction fetch has occurred
	OUT	DX,AL		;Set Line Control Reg.
	SUB	DX,3		;Data reg.
	XOR	CX,CX		;Lots of time
DATDLY:
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX		;Trash data
	LOOP	DATDLY		;  waiting for 8250 to settle

	ADD	DX,5		;line status register (port xFDH)
	PAUSE			;wait for bus settle
	IN	AL,DX		;read to clear DR,OE,PE,FE,BI,THRE,TEMT

	INC	DX		;modem status register (port xFEH)
;	ADD	DX,6		;Modem Status reg
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX		;Throw away 1st status read
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX		;Save 2nd
	MOV	[SI].MSREG,AL	; so MSRWAT works right
	DEC	DX		;Subtract 2 to get
	DEC	DX		;Modem Contrl Register
	MOV	AL,[SI].COMRTS	;Get RTS mask
	XOR	AL,2		;toggle rts bit
	OR	AL,9		;  or in DTR/INT enable
	PAUSE			;make sure instruction fetch has occurred
	OUT	DX,AL		;Enable the Card..
	SUB	DX,3		;Interrupt Enable Reg.
	MOV	AL,MSRIE+RDAIE+TBEIE ;MSR/RDA/THRE Enable
	PAUSE			;make sure instruction fetch has occurred
	OUT	DX,AL		;Enable Tx/Rx Interrupts.
	INC	DX		;Interrupt I.D. reg.
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX		;  so interrupt line goes low
;	----------------
	CALL	MSRWAT		;Check MSR (On ret, [DH] = ERROR CODE).
;	----------------
	MOV	AH,DH		;copy error code
	RET

;***
;MSRWAT:
;
;PURPOSE:
;	Checks the modem status register for CTS,DSR, and/or RLSD
;	signals.  If a timeout occurred while checking then this
;	routine will return the appropriate error code in DH.
;	This routine will not check for any signal with a corresponding
;	time out value of 0.
;ENTRY:
;	[SI] = Pointer to deb for this device
;	time out values in COMCTS,COMDSR, and COMRLS
;EXIT:
;	[DH] = 0 indicates no time out errors occurred.
;	       3 CTS timeout occurred
;	       4 DSR timeout occurred
;	       5 RLSD timeout occurred
;	      FC CTRL-BREAK found but not handled (COM OPEN ONLY)
;MODIFIED:
;	AX,CX
;****

	PUBLIC	MSRWAT
MSRWAT:
	PUSH	BX		;save registers
	PUSH	CX		
	PUSH	DI

	XOR	BX,BX		;initialize CX:BX to zero...
	MOV	CX,BX		;...for first time in
MSRWA0:				
	XOR	DI,DI		;initialize millisecond timeout value
MSRWA1:
	CMP	[b$COFlag],0	; Is this a COM OPEN?
	JNZ	MSRComOpen	; brif so -- special case
	CALL	B$BREAK_CHK	
MSRWA2:				
	XOR	AH,AH		;[AH] will be 0 if status is Ok.
	MOV	AL,[SI].MSREG	;Read Modem Status
	TEST	AL,CTS
	JNZ	MSRWA3		;Brif got CTS
	CMP	[SI].COMCTS,0	;Checking CTS?
	JZ	MSRWA3		;Brif not
	MOV	DH,3		;assume CTS timeout
	CMP	DI,[SI].COMCTS	;CTS timeout yet?
	JAE	MSRWAX		;jump if so
	INC	AH		;Status retry
MSRWA3:
	TEST	AL,DSR
	JNZ	MSRWA4		;Brif got DSR
	CMP	[SI].COMDSR,0	;Checking DSR?
	JZ	MSRWA4		;Brif not
	MOV	DH,4		;assume DSR timeout
	CMP	DI,[SI].COMDSR	;DSR timeout yet?
	JAE	MSRWAX		;jump if so
	INC	AH		;Status retry
MSRWA4:
	TEST	AL,RLSD
	JNZ	MSRWA5		;Brif got RLSD
	CMP	[SI].COMRLS,0	;Checking RLSD?
	JZ	MSRWA5		;Brif not
	MOV	DH,5		;assume MSR timeout
	CMP	DI,[SI].COMRLS	;RLDS timeout yet?
	JAE	MSRWAX		;jump if so
	INC	AH		;Status retry
MSRWA5:
	OR	AH,AH		;Check final status
	JZ	MSRWA8		;Brif Status is Ok!

;	CX:BX has entry tick value if nonzero.
;	If zero, the entry tick value is set.

	MOV	DI,CX		;DI:BX has entry tick value
	XOR	AH,AH		;clear AH to get time-of-day...
	INT	1AH		;...tick value in CX:DX
	XCHG	DI,CX		;CX:BX has entry - DI:DX has t-o-d value
	MOV	AX,CX		;get low-order t-o-d value...
	OR	AX,BX		;...and OR with high-order to test if zero
	JNZ	MSRNotFirst	;if nonzero, then not first time in
	MOV	CX,DI		;CX:BX zero, set to value...
	MOV	BX,DX		;...in DI:DX
	JMP	SHORT MSRWA0	;jump to init DI to 0

MSRNotFirst:			
	SUB	DX,BX		;subtract CX:BX from DI:DX...
	SBB	DI,CX		;...to get elapsed tick value
	JNC	MSRNoWrap	;if no wraparound, then jump
	ADD	DX,00B0H	;add 0018:00B0H to DI:DX...
	ADC	DI,0018H	;...to wrap for the next day
MSRNoWrap:			
	OR	DI,DI		;test if over 64K ticks
	MOV	DI,0FFFFH	;assume maximum millisecond value
	JNZ	MSRWA1		;if over 64K ticks, use maximum value
	MOV	AX,55D		;convert ticks to milliseconds...
	MUL	DX		;...where 1 tick is 55 milliseconds
	JC	MSRWA1		;if over 64K milliseconds, use maximum value
	MOV	DI,AX		;move computed millisecond value
	JMP	SHORT MSRWA1	;jump to retry the test

MSRComOpen:			; special handling for COM OPEN
	CALL	[b$IPOLKEY]	; check keyboard buffer (if /D or QB)
	TEST	[b$EventFlags],CNTLC ; test for CTRL-BREAK
	JZ	MSRWA2		; brif no CTRL-BREAK
	MOV	DH,0FCh		; set error flag to indicate CTRL_BREAK
	SKIP	2		; and exit via MSRWAX
MSRWA8:
	XOR	DH,DH		;indicate no errors occurred
MSRWAX:
	POP	DI
	POP	CX		
	POP	BX		
	RET

	SUBTTL	Comm read/write/status
	page
;***
;B$RECCOM
;[OEM documentation in LLCOM5]
;
;PURPOSE:
;	Read Byte From RS232 Input Queue If Data Is Ready
;
;ALGORITHM:
;	If COM interrupt handler detected error for this device,
;	  set PSW.C and return error code in [AH]
;	Else if interrupt handler has queued data for this device,
;	  move 1 byte from queue into [AL] and
;	  return with [AH]=0 and PSW.Z reset
;	Else if cntrl z (eof) then
;	  return with [AH]=26 and PSW.Z reset
;	Else
;	  return with [AH]=0 and PSW.Z set.
;ENTRY:
;	[AH] = RS232 device ID (0..255)
;
;EXIT:
;	[AH] = 0 if no I/O errors have occured
;	     = 1 if receive data queue overflow
;	     = 2 if receive parity error
;	     = 3 if CTS timeout
;	     = 4 if DSR timeout
;	     = 5 if RLSD timeout
;	     = 6 if receive Overrun error
;	     = 7 if receive Framing error
;	     = FF for all other I/O errors
;	If data is available, PSW.Z is reset & [AL] = input byte
;	else PSW.Z is set.
;
;MODIFIED:
;	none
;****

cProc	B$RECCOM,<PUBLIC,NEAR> 

cBegin				
	PUSH	SI
	PUSH	BX
	MOV	SI,OFFSET DGROUP:COMM1 ;get comm1 deb
	OR	AH,AH		;is it com1?
	JZ	RECC2		;br. if so
	MOV	SI,OFFSET DGROUP:COMM2 ;get com2 deb
RECC2:
	CALL	CHKERR		;get any errors in [AH]
	CMP	AH,0		;have any errors occurred?
	JNZ	RECERR		;br. if so
	MOV	BX,OFFSET DGROUP:Q_IN ;get address of queue control block
	CMP	[SI].DEVID,0	; is this COM1?
	JZ	RECC3		;br. if so
	MOV	BX,OFFSET DGROUP:Q_IN2 ; must be COM2 - get its queue
RECC3:
	CMP	[BX].QUENUM,0	;are there elements in the queue
	JNZ	NOZYET		;br. if so
	CMP	[SI].CTRLZ,0	;has control z been struck?
	JZ	RECERR		;br. if not
	MOV	AL,26D		;else return control z
	JMP	SHORT RETOK	;avoid deque
NOZYET:
	PUSH	ES
	MOV	ES,[SI].RCVSEG	;get buffer segment from DCB
	cCALL	B$DQUE		; get char from queue in [AL]
	POP	ES
RETOK:
	OR	SP,SP		;reset PSW.Z
	JMP	SHORT RECEXIT	

RECERR:
	XOR	AL,AL		;set PSW.Z
RECEXIT:			; get back registers and exit
	POP	BX
	POP	SI
cEnd				; return to caller

;***
;CHKERR
;
;PURPOSE:
;	checks to see if framing, overrun, parity, or data queue overflow
;	errors have taken place.
;
;ENTRY:
;	[SI] = Pointer to deb for this device
;
;EXIT:
;	[AH] = 0 if no errors have occurred
;	       1 if receive data queue overflow
;	       2 if receive parity error
;	       6 if ovrrun error
;	       7 if framing error
;	       FF if other errors occurred
;
;MODIFIED:
;	AH
;****

CHKERR:
	CMP	[SI].COMERR,0	;did line status register return errors?
	JZ	NOCMER		;br. if not
	MOV	AL,[SI].COMERR	;get error
	MOV	AH,6		;assume overrun error
	TEST	AL,2		;was this overrun error?
	JNZ	CHKRET		;br. if so
	MOV	AH,2		;assume parity error
	TEST	AL,4		;was it parity error?
	JNZ	CHKRET		;br. if so
	MOV	AH,7		;assume framing error
	TEST	AL,8		;was it a framing error?
	JNZ	CHKRET		;br. if so
	MOV	AH,0FFH 	;assume break error
	TEST	AL,10H		;was it a break error
	JNZ	CHKRET		;br. if so
NOCMER:
	MOV	AH,1		;assume overflow
	CMP	[SI].COMOVF,0	;was there overflow
	JNZ	CHKRET		;br. if so
	XOR	AH,AH		;must have been no errors
CHKRET:
	MOV	[SI].COMOVF,0	;reset for next time
	MOV	[SI].COMERR,0	;reset for next time
	RET

;***
; B$SNDCOM
;[OEM documentation in LLCOM5]
;
; Purpose:
;	Transmit one character over the specified serial port.
; Algorithm:
;	if COM interrupt handler detected error for this device,
;	  reset error-flag and return error code in [AH]
;	else if [DCB].CD_CTSTO > 0 then begin
;	  wait [DCB].CD_CTSTO milliseconds for CTS signal to become true.
;	  If TIMEOUT, return CTS timeout indication.
;	if [DCB].CD_DSRTO > 0 then begin
;	  wait [DCB].CD_DSRTO milliseconds for DSR signal to become true.
;	  If TIMEOUT, return DSR timeout indication.
;	if [DCB].CD_RSLTO > 0 then begin
;	  wait [DCB].CD_RSLTO milliseconds for RSLD signal to become true.
;	  If TIMEOUT, return RSLD timeout indication.
;	if queue is full wait until there is room in queue
;	queue Data and return with [AH] = 0.
; Entry:
;	 [AH] = RS232 device ID (0..255)
;	 [AL] = byte to be output
;
; Exit:
;	 [AH] = 0 if no I/O errors have occured
;	      = 1 if receive data queue overflow
;	      = 2 if receive parity error
;	      = 3 if CTS timeout
;	      = 4 if DSR timeout
;	      = 5 if RLSD timeout
;	      = 6 if receive Overrun error
;	      = 7 if receive Framing error
;	      = FF for all other I/O errors
; Modifies:
;	None.
;******************************************************************************

cProc	B$SNDCOM,<PUBLIC,NEAR> 

cBegin				
	PUSH	DI
	PUSH	SI
	PUSH	DX
	PUSH	CX
	PUSH	BX
	MOV	SI,OFFSET DGROUP:COMM1 ;get com1 deb
	MOV	BX,OFFSET DGROUP:Q_OUT ; get com1 queue
	OR	AH,AH		;is it comm1?
	JZ	SND2		;br. if so
	MOV	SI,OFFSET DGROUP:COMM2 ;get com2 deb
	MOV	BX,OFFSET DGROUP:Q_OUT2 ; get com2 queue
SND2:
	PUSH	AX		
	CALL	MSRWAT		;Check MSR, return or "timeout"
	POP	AX		;restore char
	MOV	AH,DH		;Copy error code
	CMP	AH,0		;time out?
	JNZ	PUTRET		;br. if so
	MOV	CX,[BX].QUELEN	;Length of queue
;PUTCLP:
	CALL	B$BREAK_CHK	
	CMP	CX,[BX].QUENUM	;Queue full?
;	JZ	PUTCLP		;Wait for space in queue
	JZ	SND2		; always check modem lines
;	PUSH	AX		;save char
;	PUSH	BX		;Save queout
;	CALL	MSRWAT		;Check MSR, (on ret, [BX] = Q_DEB).
;	POP	BX		;[SI] = queout
;	POP	AX		;Restore char to output
;	MOV	AH,DH		;copy error code
;	CMP	AH,0		;time out?
;	JNZ	PUTRET		;br. if so
	CLI			;Critical section
	CMP	[SI].COMBSY,0	;Tx already running?
	JNZ	PUTINQ		;If so, just queue and exit.
	MOV	[SI].COMBSY,255D ;Pre-set Tx busy flag.
	XOR	DX,DX
	MOV	DL,[SI].DEVID	;get device offset
	MOV	DI,DX
	SHL	DI,1		;make word index
	MOV	DX,b$ComPort[DI] ;get the I/O address port
DbAssertRel	DX,NE,0,DV_TEXT,<LLCOM3.ASM: Com I/O address = 0 in B$SNDCOM>
	OUT	DX,AL		; restarting Tx interrupts.
	JMP	SHORT POPBAR	
PUTINQ:
	PUSH	ES		; make [ES] equal to [DS]
	MOV	ES,[SI].XMTSEG	;get buffer segment from DEB
	cCALL	B$QUE		; Put char [AL] in output queue
	POP	ES
POPBAR:
	STI			;End Critical
PUTRET:
	POP	BX
	POP	CX
	POP	DX
	POP	SI
	POP	DI
cEnd				; return to caller

;***
; B$STACOM
;[OEM documentation in file LLCOM5]
;
; Purpose:
;	Returns the number of bytes of free space in the input queue
;	and the number of bytes of information queued for input.
;	In order to support the IBM Communications Error Reporting
;	philosophy, this routine will not report communications errors.
;
; Entry:
;	AH = RS232 device ID (0..255)
;
; Exit:
;	AH = 0 for no I/O error
;	CX = number of bytes of free space in the output queue
;	DX = number of bytes queued for input
;
; Modified:
;	None.
;****

cProc	B$STACOM,<PUBLIC,NEAR>,<BX,SI>	;[1]preserve registers

cBegin				
	MOV	BX,OFFSET DGROUP:Q_IN ;Addr of Input Queue
	MOV	SI,OFFSET DGROUP:Q_OUT ;address of COM1 output queue
	OR	AH,AH		;is this COM1?
	JZ	STA2		;br. if so
	MOV	BX,OFFSET DGROUP:Q_IN2 ;else get com2 input queue
	MOV	SI,OFFSET DGROUP:Q_OUT2 ;address of COM2 output queue
STA2:
	MOV	DX,[BX].QUENUM	;get number of bytes in queue
	MOV	CX,[SI].QUELEN	;get length of queue
	SUB	CX,[SI].QUENUM	;determine amount of free space in queue
	XOR	AH,AH		;indicate no errors
cEnd				; pop BX and exit

;***
;B$TRMCOM - Terminate the communications device
;[OEM documentation given in LLCOM5]
;
;Purpose:
;	Terminate an RS232 channel.  This routine is called when a file
;	associated with a COM channel is closed, or when a SYSTEM statement
;	is executed.  It waits for any outbound data to be transmitted, drops
;	the DTR line,  disables interrupts from this device, and disables the
;	queues if needed.  Any interupt handlers or threads should be removed.
;
;	While waiting for any data to be written, make sure to check the
;	keyboard for a ^Break and to check the Modem Status Registers for
;	any timeout conditions so as not to hang. This routine will not
;	report communications errors unless operating under OS/2.
;
;Entry:
;	[AH] = RS232 device ID (0 <= AH < NUM_RS232 <= 2)
;	[CX] = FLAG: 0 = reset DTR
;
;Exit:
;	[AH] = 0 to indicate that there are no errors.
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$TRMCOM,<PUBLIC,NEAR> 

cBegin				
	PUSH	SI
	PUSH	BX
	PUSH	CX
	PUSH	DX
	MOV	SI,OFFSET DGROUP:COMM1 ;Com1 deb
	MOV	BX,OFFSET DGROUP:Q_OUT ; Get Output Queue base
	OR	AH,AH		;is this com1?
	JZ	TRM2		;br.if so
	MOV	SI,OFFSET DGROUP:COMM2 ;get com2 deb
	MOV	BX,OFFSET DGROUP:Q_OUT2 ; Get Output Queue base
TRM2:
	CMP	[BX].QUENUM,0	;Output busy?
	JZ	CLSWAX		;Brif done
CLSWAT:
	CALL	MSRWAT		;Allow "Time out" during CLOSE
	OR	DH,DH		;did a timeout occur?
	JNE	CLSWAX		;if so, just call quits and close it
	CMP	[BX].QUENUM,0	;Output busy?
	JNZ	CLSWAT		;Yes, wait until done
CLSWAX:
	PUSH	CX		; save flag
	XOR	CX,CX		;Lots of time
CLSDLY:
	LOOP	CLSDLY		;for Shift Reg to empty...
	POP	CX		; CX = flag; 0 = reset DTR
	CALL	CLSMSR		;Force the file closed
	POP	DX
	POP	CX
	POP	BX
	POP	SI
	RET			;COM file is closed and deallocated

CLSMSR:
	PUSH	DI
	XOR	DX,DX
	MOV	DL,[SI].DEVID
	MOV	DI,DX		;get device offset
	SHL	DI,1		;make a word index
	MOV	DX,b$ComPort[DI] ;get the device I/O port address
DbAssertRel	DX,NE,0,DV_TEXT,<LLCOM3.ASM: Com I/O address = 0 at start of B$TRMCOM>
	MOV	AL,0		; 0 to reset lines
	JCXZ	Reset_RTS	; if CX  = 0, reset DTR
	INC	AX		; else, leave DTR active
Reset_RTS:			; always reset RTS
	ADD	DX,4		;Modem Ctrl reg.
	OUT	DX,AL		;Clear MSR, disable card.
	MOV	[BX].QUENUM,0	;Clear # to output
	SUB	DX,3		;Interrupt Enable Reg.
	PAUSE			;make sure instruction fetch has occurred
	MOV	AX,1000h + 0	; 0 to disable interrupts; AH=10H for below
	OUT	DX,AL		; Disable COMM Interrupts
	DEC	DX		;get back base I/O address
;	MOV	AH,10H		;IRQ-4 Disable Mask
	CMP	DH,3		; RS232 Port addr 3xx?
	JZ	$C_COM_3xx	;Yes, Disable IRQ-4
	MOV	AH,8		;IRQ-3 Disable Mask, assume 2xx.
$C_COM_3xx:
	CLI
	TEST	b$DOS_INT_MASK,AH ;test if bit was initially enabled
	JZ	NO_INT_DISABLE	;if so (bit was 0), then no disable
	IN	AL,INTA1	;Get INT masks from 8259 IMR Reg.
	OR	AL,AH		;Disable IRQ-3 or IRQ-4
	PAUSE			;make sure instruction fetch has occurred
	OUT	INTA1,AL	;For RS232 Cards.
NO_INT_DISABLE:

	MOV	BX,OFFSET DGROUP:COM1_VECTOR ;get start of old COM1 vector
	OR	DI,DI		;test if COM1
	JZ	REST_VECTOR	;if COM1, then jump
	MOV	BX,OFFSET DGROUP:COM2_VECTOR ;get start of old COM2 vector
REST_VECTOR:			

	MOV	AL,IRQ4/4	;vector number for primary card
	CMP	DH,3		;test if primary asynch card
	JE	REST_MASK	;if so, then jump
	MOV	AL,IRQ3/4	;vector number for secondary card
REST_MASK:			

	PUSH	DS		; Save BASIC data segment
	LDS	DX,DWORD PTR [BX] ;load vector into DS:DX
	MOV	AH,25H		;get DOS function code
	INT	21H		;and restore it
	STI
	POP	DS		;and restore DS to it
	XOR	AX,AX		;clear AX
	MOV	[BX],AX 	;clear offset of saved vector
	MOV	[BX+2],AX	;and same for the segment...

;	Set the BIOS data location for the COM I/O port address.

	XOR	BX,BX		; get a zero
	XCHG	BX,b$ComPort[DI] ;get the COM I/O port address [25]& reset
DbAssertRel	BX,NE,0,DV_TEXT,<LLCOM3.ASM: Com I/O address = 0 at end of B$TRMCOM>
	PUSH	DS		;save DGROUP on the stack
	XOR	DX,DX		;set to zero to access...
	MOV	DS,DX		;...the BIOS data segment
	MOV	DS:RS232B[DI],BX ;restore the BIOS data location
	POP	DS		;restore DGROUP from the stack

	POP	DI
cEnd				; return to caller

	SUBTTL	Comm interrupt service routine
	page
;***
;COM_INT1 / COM_INT2
;
;PURPOSE:
;	Interrupt handlers for COM1: and COM2:.  This is the communications
;	interrupt service routine for RS232 communications.  When an RS232
;	event occurs the interrupt vectors here.  This routine determines
;	who the caller was and services the appropriate interrupt.  The
;	interrupts are prioritized in the following order:
;		1.  line status interrupt
;		2.  read data available interrupt
;		3.  transmit buffer empty interrupt
;		4.  modem service interrupt
;	This routine continues to service until all interrupts have been
;	satisfied.
;
;ENTRY:
;	none
;EXIT:
;	none
;MODIFIED:
;	none
;******************************************************************************

COM_INT2:
	PUSH	DS
	PUSH	DI
	PUSH	DX
	MOV	DS,CS:b$BASDSG    ;get BASIC's data seg
	MOV	DX,DS:b$ComPort+2 ;DX = COM2 I/O addr.
DbAssertRel	DX,NE,0,DV_TEXT,<LLCOM3.ASM: Com I/O address = 0 in COM_INT2>
	MOV	DI,OFFSET DGROUP:COMM2 ;get com2 deb
	JMP	SHORT COMM_INT

COM_INT1:
	PUSH	DS
	PUSH	DI
	PUSH	DX
	MOV	DS,CS:b$BASDSG  ;get BASIC's data seg
	MOV	DX,DS:b$ComPort ;DX = COM1 I/O addr.
DbAssertRel	DX,NE,0,DV_TEXT,<LLCOM3.ASM: Com I/O address = 0 in COM_INT1>
	MOV	DI,OFFSET DGROUP:COMM1 ;get com1 deb

COMM_INT:
	PUSH	AX
	PUSH	BX
	PUSH	SI
	INC	DX
	INC	DX		;Interrupt Id Reg.
COMM_ILP:
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX		;Get Interrupt Id.
	TEST	AL,1		;Interrupt need servicing?
	JNZ	COMSRX		;Brif not, all done..
	XOR	AH,AH		;Using [AX] for Interrupt dispatch.
	MOV	SI,AX
	PUSH	DX		;Save Id reg.
	CALL	CS:SRVTAB[SI]	;Service the Interrupt..
	CLI			; disable interrupts for next iter. of loop
	POP	DX		;Interrupt Id reg.
	JMP	COMM_ILP	; Until all Interrupts Serviced

COMSRX:
	POP	SI
	POP	BX
	MOV	DX,INTA0
	MOV	AL,EOI		;Send End-of-Interrupt
	PAUSE			;make sure instruction fetch has occurred
	OUT	DX,AL		; to 8259
	POP	AX
	POP	DX
	POP	DI
	POP	DS
	IRET

SRVTAB	LABEL	WORD
	DW	MSISRV		;Service Modem Status Interrupt
	DW	THRSRV		;Service Tx Holding Reg. Interrupt
	DW	RDASRV		;Service Rx Data Available Interrupt
	DW	RLSSRV		;Service Line Status Interrupt

; Rx Data Available Interrupt Service (Second)

;		Line Status Interrupt Service (Highest)
;		Not currently enabled.	Line status is read
;		on RDA Interrupt and sets COMERR error flag
;		if any errors exist.

RLSSRV:
	STI			; we're not reading, reenable interrupts
	RET

RDASRV:
	XOR	AH,AH		;Don't set hi bit if no errors
	ADD	DX,3		;Line Status Reg.
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX
	AND	AL,1EH		;Mask for errors
	JZ	RDASR2		;Brif no errors
	CMP	[DI].COMPE,0	;is PE option selected?
	JE	RDASR4		;if not, do not parity error
	CMP	[DI].PARTYP,0	;Checking Parity errors
	JNZ	RDASR1		;Brif so, report all errors
RDASR4:
	CMP	AL,4		;Parity error?
	JZ	RDASR2		;Ignore if so, else report others
RDASR1:
	MOV	[DI].COMERR,AL	;Nonzero for "Device I/O Error"
	MOV	AH,80H		;Will set hi bit of byte in error
RDASR2:
	SUB	DX,5		;Data I/O Reg.
	MOV	BX,OFFSET DGROUP:Q_IN ;Addr of Input Queue
	CMP	[DI].DEVID,0	;is this COM1?
	JZ	RDASR3		;br. if so
	MOV	BX,OFFSET DGROUP:Q_IN2 ;else get com2 input queue
RDASR3:
	MOV	SI,[BX].QUENUM
	CMP	SI,[BX].QUELEN	;Is queue full?
	JZ	COMOVR		;Yes, comm overrun
	CMP	[DI].CTRLZ,0	;has control z been struck?
	JNZ	COMOVR		;br. if so - treat as overflow
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX		;Read Char from 8250
	STI			; reenable interrupts now that char is read
	CMP	[DI].BINMOD,0	;is this binary stuff?
	JZ	QUEIT		;br. if so
	CMP	AL,26D		;is this a control z?
	JNZ	QUEIT		;br. if not - que data
	MOV	[DI].CTRLZ,255D ;set control z flag
	JMP	SHORT NOQUE	;skip the queing process
QUEIT:
	OR	AL,AH		;Sets hi bit if error
;#***
	PUSH	ES
	MOV	ES,[DI].RCVSEG	;get buffer segment from DCB
	cCALL	B$QUE		; Put char [AL] in output queue
	POP	ES
	JMP	SHORT NOQUE	
;#***

COMOVR:
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX		;Dismiss and lose char
	STI			; reenable interrupts now that char is read
	MOV	[DI].COMOVF,1	;Set we have overflowed
NOQUE:				
	MOV	AX,DI		; AL = 0 for COM 1, 2 for COM2
	SHR	AL,1		; AL = trap number for B$TrapEvent
;	ADD	AL,COMOFF	; COMOFF = 0
	JMP	[b$pTrapEvent]	; and indicate an event if events linked
				; in, and return

; Tx Holding Reg Empty Interrupt Service (Third)

THRSRV:
	STI			; we're not reading, reenable interrupts
	CMP	[DI].MSRERR,0	;If Modem status fault then turn
	JNZ	TXSTOP		; off Tx interrupts until cleared
THRSR2:
	MOV	SI,BX		;[SI] = Queue base for speed
	MOV	BX,OFFSET DGROUP:Q_OUT ;Get Output Queue
	CMP	[DI].DEVID,0	;is this com1?
	JZ	THRSR3		;br. if so
	MOV	BX,OFFSET DGROUP:Q_OUT2 ;else get com2 output queue
THRSR3:
	CMP	[BX].QUENUM,0	;Zero if queue empty
	JZ	XMITOF		;Brif No chars, Set COMBSY = 0.

	DEC	DX
	DEC	DX		;Data I/O Reg.
	PUSH	ES		;save register
	MOV	ES,[DI].XMTSEG	;get buffer segment from DEB
	cCALL	B$DQUE		; get char from queue in [AL]
	POP	ES		;restore register
	OUT	DX,AL		;Send char
TXSTOP:
	RET
XMITOF:
	MOV	[DI].COMBSY,0	;Set Tx Interrupt status.
	RET

;Modem Status Interrupt Service (Lowest)

MSISRV:
	STI			; we're not reading, reenable interrupts
	ADD	DX,4		;Modem Status Reg.
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX
	MOV	[DI].MSREG,AL	;Save MSR data for others
	CMP	[DI].COMRLS,0	;Did user want to check?
	JZ	MSISRD		;Brif not, Try DSR.
	TEST	AL,RLSD 	;Did we lose RLSD?
	JZ	MSISRE		;"Device Timeout" if so.
MSISRD:
	CMP	[DI].COMDSR,0	;Did user want to check?
	JZ	MSISRC		;Brif not, Try CTS.
	TEST	AL,DSR		;Did we lose DSR?
	JZ	MSISRE		;"Device Timeout" if so.
MSISRC:
	CMP	[DI].COMCTS,0	;Did user want to check?
	JZ	MSISRX		;Brif not.
	TEST	AL,CTS		;id we lose CTS?
	JNZ	MSISRX		;Ignore if not.
MSISRE:
	MOV	[DI].MSRERR,DL	;will give "Device Timeout".
MSIRET:
	RET
MSISRX:
	CMP	[DI].MSRERR,0	;If was running
	JZ	MSIRET		; leave alone
	MOV	[DI].MSRERR,0	;Clear the Modem status fault
	DEC	DX		;Line Status reg.
MSISRH:
	PAUSE			;make sure instruction fetch has occurred
	IN	AL,DX
	AND	AL,20H		;Tx Holding reg empty?
	JZ	MSISRH		;Brif not, wait for it
	SUB	DX,3		;Restore to Int Id reg.
	JMP	THRSRV		; and turn Tx Interrupts on if chars

sEnd	DV_TEXT 		
	END
