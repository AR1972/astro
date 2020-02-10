;***
; event.inc - 20-Mar-86 - functions to oem trapping routines
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

	enable_trap = 254
	disable_trap= 255

;music
	set_note_cnt= 2

;timer
	set_interval= 1

;keys
	define_key =   1
	start_key = 252
	stop_key = 253

	Printer_Echo= -3	;define parameters for special keys
	Pause_Key =  -2
	Ctl_Break =  -1

;[3]start of stuff moved from switch.inc

NUM_TRAPDEV = 0 		;Number of trappable devices

NUM_JOYST =	0		;Default number of joystick triggers
NUM_JOYST =	4		;Number of joystick triggers
JOYST_IND =   NUM_TRAPDEV	;Dispatch table index for joystick
NUM_TRAPDEV = NUM_TRAPDEV + 1	;[5] Number of trappable devices
;[5] HACK -- for OS/2, would be better not to waste the slots marked with
;[5] HACK -- [5], but it would either require separate assembly, or
;[5] HACK -- changing the polling order.

NUM_LITEPEN =	0		;Default number of lightpens supported
NUM_LITEPEN =	1		;Number of lightpens supported
LITEPEN_IND = NUM_TRAPDEV	;Dispatch table index for litepen
NUM_TRAPDEV = NUM_TRAPDEV + 1	;[5] Number of trappable devices

NUM_MOUSE =	 0		;Default number of MOUSE events supported

NUM_UEVENT =	0		;Default number of user defined events

RS232_IND =   NUM_TRAPDEV	;Dispatch table index for rs232
NUM_TRAPDEV = NUM_TRAPDEV + 1	;Number of trappable devices

NUM_SIGNAL =	 0		;Default number of SIGNALs supported

NUM_SOUND =	0		;Default number of PLAY events
NUM_SOUND =	1		;Number of PLAY events
SOUND_IND =   NUM_TRAPDEV	;Dispatch table index for sound

;[5]HACK: this works since SOUND and SIGNAL are mutually exclusive
NUM_TRAPDEV = NUM_TRAPDEV + 1	;[5] Number of trappable devices

NUM_TIMER =	0		;Default number of TIMER events
NUM_TIMER =	1		;Number of timer events
TIMER_IND =   NUM_TRAPDEV	;Dispatch table index for timer
NUM_TRAPDEV = NUM_TRAPDEV + 1	;number of trappable devices

NUM_UKEYS =	11		;Number of user-defined keys for trapping
NUM_GAP =	4		;number of gap in between the last user
				; defined key and F11
NUM_TKEYS =	0		;Default number of trappable keys
NUM_TKEYS =	NUM_FKEYS + 4 + NUM_UKEYS + NUM_GAP
				;there is a gap between the last user
				; defined key and F11
				;Number of trappable keys
				;Function + Direction + User-defined
TKEYS_IND =   NUM_TRAPDEV	;Dispatch table index for key trapping
NUM_TRAPDEV = NUM_TRAPDEV + 1	;number of trappable devices

NUM_TRAPS =	NUM_TKEYS + NUM_RS232 + NUM_LITEPEN + NUM_SIGNAL
NUM_TRAPS =	NUM_TRAPS + NUM_JOYST + NUM_TIMER + NUM_SOUND
NUM_TRAPS =	NUM_TRAPS + NUM_MOUSE + NUM_UEVENT
				;Total number of trappable conditions
;[3]end of stuff moved from switch.inc

;[1]	Equates for use with b$EventFlags.

InSLEEP		= 01H		; SLEEP statement is waiting for an event
SLEEPtmr	= 02H		; SLEEP timeout timer is enabled
PAUSEF		= 04H		; ^S, ^Q flag
CNTLC		= 08H		; CTRL-BREAK encountered 
TimerInst	= 10H		; Timer interrupt installed
KybdInst	= 20H		;[6] keyboard interrupt installed

;[1] Event Action Verbs: ON, OFF and STOP

$ON=	0			;Event ON
$OFF=	1			;Event OFF
$STOP=	2			;Event STOP

;[1] Trap Table Event Action Masks.

TRP_ON= 1			;Event Trap ON.
TRP_ST= 2			;Event Trap STOP.
TRP_RQ= 4			;Event Trap Request.
TRP_OS= TRP_ON+TRP_ST		;Event Trap ON or STOPed
TRP_CN= 80H			;Context bit (0=comp handler !0 = interp)
TRP_MSK = 7FH			;Mask out Context bit

;[1] Offsets into b$TRPTBL for Event Traps.

;[4] The IFDEF's are so that files that only refer to the first few
;[4] ones don't need to have separate DOS3-OS/2 versions, but we will
;[4] detect it if any of the OS-dependent ones are referenced.

COMOFF= 0

KEYOFF= COMOFF+NUM_RS232

TIMOFF= KEYOFF+NUM_TKEYS

MOUOFF= TIMOFF+NUM_TIMER

UEVOFF= MOUOFF+NUM_MOUSE

PENOFF= UEVOFF+NUM_UEVENT

PLAOFF= PENOFF+NUM_LITEPEN

STGOFF= PLAOFF+NUM_SOUND

SIGOFF= STGOFF+NUM_JOYST

	.LIST
