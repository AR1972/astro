;***	TOGEQU.ASM - Equates needed for MSDOS Manager's toggle code
;
;   By Eric K. Evans, Microsoft Corp, May 1987
;


multMGR     EQU     18h 		; Stub's/Manager's Int 2Fh multiplex
					;  number

; Int 2Fh subfunction numbers

TOGINST     EQU      0h 		; Installation check
TOGRUN	    EQU      1h 		; Run request
TOGREG	    EQU     45h 		; Register Manager
TOGDEREG    EQU     6Bh 		; Deregister Manager


; Return codes

TOGSUC	    EQU      0h 		; Success
TOGERR	    EQU    0FFh 		; Failure
TOGSTUB     EQU     80h 		; Stub is installed
TOGMGR	    EQU    0FFh 		; Manager is installed and registered


; Values for the fToggle flag

FT_NOTINI	EQU	-1		; Toggling hasn't been initialized yet.
FT_NOTTOGGLED	EQU	 0		; The Manager is not toggled.
FT_TOGGLED	EQU	 1		; The Manager is toggled.
FT_LOCKED	EQU	 2		; The Manager is running at task time
					;  so toggle users are locked out.


; SafeMemory subfunctions (this file is probably the wrong place for these)

SM_HIGH 	EQU	0		; Allocate memory high
SM_LOW		EQU	1		; Allocate memory low
SM_EITHER	EQU	2		; Allocate memory high or low


SM_LOWLIMIT	EQU	2000h		; Highest posssible low memory address
