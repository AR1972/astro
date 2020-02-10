;***
;comdcb.inc - 13-Aug-86 - communications device control block structure
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Provides the control block used between the communications
;	high- and low-level code.
;
;******************************************************************************

;	RS232 Device Control Block Definition:


COMDCB	STRUC

CD_DEVID	DB	?	;RS232 Channel ID (0..n)
CD_BYTSZ	DB	?	;bits per byte (4..8)
CD_PARIT	DB	?	;parity (0..4)= (none, odd, even, mark, space)
CD_STOPB	DB	?	;(0..2)= (1, 1.5, 2) stop bits
CD_BAUDR	DW	?	;baud rate
CD_OPNTO	DW	?	;[2]device open timeout
CD_RLSTO	DW	?	;RLSD (rec. line signal detect) timeout
CD_CTSTO	DW	?	;CTS (clear to send) timeout
CD_DSRTO	DW	?	;DSR (data set ready) timeout
				;timeout values are in milliseconds (0=infinite)
CD_CMWID	DB	?	;device width (columns per line)
CD_CMPOS	DB	?	;current column of device (must follow _CMWID)
CD_CMEVT	DW	?	;counter for event flag processing
				;set to 0 at COM open, decremented for
				;each character read from queue.  If
				;less than number of characters in queue
				;then event is signaled (if enabled)
CD_CMFDB	DW	?	;points to FDB for file (0=not opened)
CD_RXSEG	DW	?	;[2]seg of COMx receive buffer in far memory
CD_RXSIZ	DW	?	;[2]size in bytes of the COMx receive buffer
CD_TXSEG	DW	?	;[2]seg of COMx transmit buffer in far memory
CD_TXSIZ	DW	?	;[2]size in bytes of the COMx transmit buffer
CD_CMFLG	DB	?	;Boolean attributes mask for this device

COMDCB	ENDS


;	Values for bits in COMx DCB flag byte CD_CMFLG.

CF_CMBIN	EQU	01H	;(0/1)=ASC/BIN (BIN|ASC specified in filespec)
CF_CMRTS	EQU	02H	;if set, suppress Request-To-Send (RS option)
CF_CMPEN	EQU	04H	;if set, receive parity enabled
CF_CMOPN	EQU	08H	;[2]if set, open timeout specified
CF_CMCOD	EQU	10H	;if set, user specified BIN or ASC
CF_CMCTS	EQU	20H	;if set, CTS parm not defaulted (in ParseOpt)
CF_CMCTZ	EQU	20H	;[3]if set, Ctl-Z seen in ASC (after ParseOpt)
CF_CMCLF	EQU	40H	;if set, send line feed after CR
CF_CMCRF	EQU	80H	;if set, last char sent was carriage return

;	IF COM filename contains:      ASC     BIN     both    neither
;	Then CF_CMBIN, CF_CMCOD is:    0,1     1,1   bad name    1,0

	.LIST			;[1]

