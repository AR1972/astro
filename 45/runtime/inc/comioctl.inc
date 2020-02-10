;***
;comioctl.inc - 12-Oct-86 - DOS5 com device driver IOCTL definitions
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Provides value, structure, and bit definitions for the DOS5 com
;	IOCTL and hardware.
;
;******************************************************************************

ComCategory	EQU	1		;IOCTL category for COMx device driver

;	Line characteristics

;--------------------------------------------------------------------------
IOComSetBaud	EQU	41H		;set baud rate
IOComGetBaud	EQU	61H		;get baud rate

SetBaudStruc	STRUC
IOParmBaudRate	DW	?		;baud rate (bits per second)
SetBaudStruc	ENDS
;--------------------------------------------------------------------------
IOComSetLinCtl	EQU	42H		;set line control
IOComGetLinCtl	EQU	62H		;get line control

LinCtlStruc	STRUC
IOParmDatLen	DB	?		;length of data in bits (5-8)
IOParmParity	DB	?		;parity (0-4)=(no,odd,even,mark,space)
IOParmStopBt	DB	?		;stop bits (0-2)=(1,1.5,2)
IOParmBreak	DB	?		;(get only) Tx break: 0=off; 1=on
LinCtlStruc	ENDS
;--------------------------------------------------------------------------
IOComTxStatus	EQU	65H		;return transmit data status

TxStatusStruc	STRUC
IOParmTxStat	DB	?		;bits of transmit data status (*)
TxStatusStruc	ENDS

;(*)	Bits in transmit data status byte.

	IOTxWriteReq	EQU	01H	;WRITE request packet queued
	IOTxDataInQueue	EQU	02H	;data in driver transmit queue
	IOTxCharActive	EQU	04H	;transmitter currently active
	IOTxCharWaiting	EQU	08H	;waiting for "immediate transmit"
	IOTxXonWaiting	EQU	10H	;waiting to transmit XON
	IOTxXoffWaiting	EQU	20H	;waiting to transmit XOFF
;--------------------------------------------------------------------------
IOComSetModCtl	EQU	46H		;set modem control register

SetModCtlStruc	STRUC
IOParmModCtlSet	DB	?		;bits to set in modem control reg
IOParmModCtlClr	DB	?		;bits to clear in modem control reg
SetModCtlStruc	ENDS
;--------------------------------------------------------------------------
IOComGetModCtl	EQU	66H		;get modem control register

GetModCtlStruc	STRUC
IOParmModCtl	DB	?		;value of modem control register (*)
GetModCtlStruc	ENDS

;(*)	Modem control register bit definitions.

	IOModCtlDTR	EQU	01H	;DTR active low
	IOModCtlRTS	EQU	02H	;RTS active low
;--------------------------------------------------------------------------
IOComGetLinStat	EQU	65H		;get line status register

GetLinStatStruc	STRUC
IOParmLinStat	DB	?		;value of line status register (*)
GetLinStatStruc	ENDS

;(*)	Line status register bit definitions.

	IOLinStatRxRdy	EQU	01H	;Rx data register full
	IOLinStatChrOR	EQU	02H	;Rx data register character overrun
	IOLinStatParErr	EQU	04H	;Rx data register parity error
	IOLinStatBitOR	EQU	08H	;Rx data register bit overrun (framing)
	IOLinStatBreak	EQU	10H	;Rx data register break detected
	IOLinStatTxHold	EQU	20H	;Tx holding register empty
	IOLinStatTxEmpt	EQU	40H	;Tx shift and holding registers empty
;--------------------------------------------------------------------------
IOComGetModStat	EQU	67H		;get modem status register

GetModStatStruc	STRUC
IOParmModStat	DB	?		;value of modem status register (*)
GetModStatStruc	ENDS

;(*)	Modem status register bit definitions.

	IOModStatCTS	EQU	10H	;current CTS (clear to send)
	IOModStatDSR	EQU	20H	;current DSR (data set ready)
	IOModStatRI	EQU	40H	;current RI (ring indicator)
	IOModStatDCD	EQU	80H	;current DCD (data carrier detect)
;--------------------------------------------------------------------------

;	Manual XON/XOFF processing

;--------------------------------------------------------------------------
IOComTxImmed	EQU	44H		;transmit immediate

TxImmedStruc	STRUC
IOParmChrTxImm	DB	?		;char to be transmitted immediately
TxImmedStruc	ENDS
;--------------------------------------------------------------------------
IOComSimXOFF	EQU	47H		;simulate XOFF received (stop Tx)
;--------------------------------------------------------------------------
IOComSimXON	EQU	48H		;simulate XON received (start Tx)
;--------------------------------------------------------------------------

;	Break processing and port error.

;--------------------------------------------------------------------------
IOComBreakOn	EQU	4BH		;set break condition on
IOComBreakOff	EQU	45H		;set break condition off
IOComGetPrtErr	EQU	6DH		;get com port error

PrtErrStruc	STRUC
IOParmPrtErr	DW	?		;ommunications port error word (*)
PrtErrStruc	ENDS

;(*)	Communications port error word bit definitions.

	IOErrRxQueOflo	EQU	0001H	;receive queue overflow
	IOErrRxHardOflo	EQU	0002H	;receive hardware overflow
	IOErrRxParity	EQU	0004H	;receive character parity error
	IOErrRxBitOR	EQU	0008H	;receive character bit overrun error
;--------------------------------------------------------------------------

;	Device driver queue management

;--------------------------------------------------------------------------
IOComGetInQChr	EQU	68H		;get number of char in input queue
IOComGetOutQChr	EQU	69H		;get number of char in output queue

QueChrStruc	STRUC
IOParmCharQue	DW	?		;number of characters in the queue
IOParmSizeQue	DW	?		;size of queue
QueChrStruc	ENDS
;--------------------------------------------------------------------------

;	Polled events

;--------------------------------------------------------------------------
IOComSetEvtMask	EQU	52H		;set com event mask
IOComGetEvtMask	EQU	72H		;get com event mask

EventStruc	STRUC
IOParmEvent	DW	?		;communications event word (*)
EventStruc	ENDS

;(*)	Communication event word bit definitions.

	IOEvtRxAnyChr	EQU	0001H	;any character received
	IOEvtTxLastChr	EQU	0004H	;last character transmitted
	IOEvtDeltaCTS	EQU	0008H	;delta in CTS state
	IOEvtDeltaDSR	EQU	0010H	;delta in DSR state
	IOEvtDeltaDCD	EQU	0020H	;delta in DCD state
	IOEvtBreak	EQU	0040H	;break condition detected
	IOEvtLineStat	EQU	0080H	;line status error (parity,overrun)
	IOEvtDeltaRI	EQU	0100H	;delta in RI state
;--------------------------------------------------------------------------
IOComGetPrtStat	EQU	6BH		;get com port status

PrtStatStruc	STRUC
IOParmPrtStat	DB	?		;communications port status (*)
PrtStatStruc	ENDS

;(*)	Communications port status bit definitions.

	IOPrtCTSTxWait	EQU	01H	;transmit waiting for CTS to be on
	IOPrtDSRTxWait	EQU	02H	;transmit waiting for DSR to be on
	IOPrtDCDTxWait	EQU	04H	;transmit waiting for DCD to be on
	IOPrtXOFRxWait	EQU	08H	;transmit waiting since XOFF received
	IOPrtXOFTxWait	EQU	10H	;transmit waiting since XOFF transmit
	IOPrtBrkTxWait	EQU	20H	;transmit waiting since BREAK transmit
	IOPrtDSRRxWait	EQU	40H	;receive waiting for DSR to be on
;--------------------------------------------------------------------------

;	Device data control block (DCB) management

;--------------------------------------------------------------------------
IOComSetDevDCB	EQU	53H		;set DCB parameters
IOComGetDevDCB	EQU	73H		;get DCB parameters

DevDCBStruc	STRUC
IOParmWriteTime	DW	?		;write timeout in hundredths of sec
IOParmReadTime	DW	?		;read timeout in hundrerths of sec
IOParmFlags1	DB	?		;first byte of flags (*)
IOParmFlags2	DB	?		;second byte of flags (**)
IOParmFlags3	DB	?		;third byte of flags (***)
IOParmErrRepChr	DB	?		;error replacement character
IOParmBrkRepChr	DB	?		;break replacement character
IOParmXONChar	DB	?		;XON character value
IOParmXOFFChar	DB	?		;XOFF character value
DevDCBStruc	ENDS

;(*)	First flag byte bit definitions.

	IOFlag1DTRMask	EQU	03H	;DTR control mode mask
	  IOFlag1DTRDis	EQU	00H	;DTR disable
	  IOFlag1DTREnb	EQU	01H	;DTR enable
	  IOFlag1RxDTR	EQU	02H	;DTR input handshaking
	IOFlag1TxCTS	EQU	08H	;CTS output handshaking
	IOFlag1TxDSR	EQU	10H	;DSR output handshaking
	IOFlag1TxDCD	EQU	20H	;DCD output handshaking
	IOFlag1RxDSR	EQU	40H	;DSR input handshaking

;(**)	Second flag byte bit definitions.

	IOFlag2XONOut	EQU	01H	;output XON/XOFF active
	IOFlag2XONIn	EQU	02H	;input XON/XOFF active
	IOFlag2ErrChar	EQU	04H	;error replacement char active
	IOFlag2NulStrp	EQU	08H	;null char stripping active
	IOFlag2BrkChar	EQU	10H	;break replacement char active
	IOFlag2RTSMask	EQU	0C0H	;RTS control mode mask
	  IOFlag2RTSDis	EQU	000H	;RTS disable
	  IOFlag2RTSEnb	EQU	040H	;RTS enable
	  IOFlag2RxRTS	EQU	080H	;RTS input handshaking
	  IOFlag2RTSTog	EQU	0C0H	;RTS toggling on transmit

;(***)	Third flag byte bit definitions.

	IOFlag3TxInf	EQU	01H	;enable write infinite timeout
	IOFlag3RxTOMask	EQU	06H	;read timeout processing mask
	  IOFlag3RxNorm	EQU	02H	;normal read timeout
	  IOFlag3RxWait	EQU	04H	;wait read timeout
	  IOFlag3RxNone	EQU	06H	;no-wait read timeout
;--------------------------------------------------------------------------

	.LIST
