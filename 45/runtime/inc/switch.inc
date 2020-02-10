;***
; SWITCH.INC - 01-Oct-87 - BASCOM compiler and runtime common switch file
;***
	.XLIST
;***
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

FALSE	      = 0
TRUE	      = NOT FALSE
UNDEFINED     = 0FFFFH		;an undefined value

memM	      = 1		;[14] medium model is internal default

NUM_CASS      = 0		;Number of cassettes supported
NUM_FKEYS     = 12		;Number of function keys (softkeys)
NUM_LINES     = 25		;Number of lines on screen
NUM_LPT       = 3		;Number of line printers supported
NUM_RS232     = 2		;Number of RS232 com ports supported
NUM_SCRLTYPE  = 1		;Number of scroll type
NUM_VOICES    = 1		;Number of voices for SOUND
NUM_VSTACK    = 24		;Number of entries for each voice in SOUND
ID_SSEQDS     = TRUE		;SS is assumed equal to DS

;	The specified <version>.<release><update> number is used
;	to guarantee compatibility among the compiler, runtime
;	library, and runtime module.

NumVer= 6
NumRel= 0
NumUpd= 0

	.LIST
