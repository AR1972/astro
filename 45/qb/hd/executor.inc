;Executor.inc
EXECUTOR_INC	= ON

;===========================================================================
;Executor static data
;
;===========================================================================


;===========================================================================
;Begin of Statement flags

sBegin	DATA

externW BosFlags	;Begin of Statement Flags

; Current frame pointer, only valid when in the user interface (i.e. after
; UserInterface() is called and before it returns).
; Used to impliment PStep.

externW FrameCur

ife	EXPRINT_ASM
externB executorFlags	;flags that get reset whenever a runtime error occurs, 
			;i.e., flags whose meaning is only valid within a 
			;single statement
endif	;EXPRINT_ASM

	ife	EXARRAY_ASM

	    extrn   DimAtScanType:byte	;Type of Dim initiated by scanner

	endif				;EXARRAY_ASM

sEnd	DATA


; NOTE: FBOSDEBUG should have the highest value of all FBOS bits. This means
;	that this bit is the LAST one to be tested (lowest priority of 
;	BOS exception handling).
; NOTE: FBOSSTOP should have the next highest value. This is so that if a user
;	has an event handler for the Ctl-Break key, it will take precedence
;	over the normal 'stop the program' behavior of Ctl-Break.
; NOTE: FBOSEVENT should have the next highest value. This is so a lot of 
;	events stacked up won't prevent other BOS items from occuring.
;	It's higher priority than FBOSDEBUG because we don't want to
;	allow user to trace into event handlers (he can set a breakpoint).
;	A useful analogy for this is interrupt handlers are not traced into
;	in symdeb.

FBOSRESETSTK	EQU 1h	;Set by the CLEAR support code; causes the stack to
			; be reset (both SP & BP) at next BOS/BOL
FBOSEVENT	EQU 2h	;Set by event handlers.  Tells bos to service event
FBOSSTOP	EQU 4h	;Set by runtime callback when user hits ctl-break
FBOSDEBUG	EQU 8h	;Set by B$FERROR when a runtime error occurs in a
			; module for which no error handler exists. B$FERROR
			; moves the oTxt back to the last opBos prior to
			; where the error occured, sets this bit, and dispatches
			; Also set by user interface when any breakpoints
			; are set, any watch expressions are active, or
			; when program tracing is active.  Causes next bos
			; to enter UserInterface()

	;The following mnemonic constants are used for DimAtScanType

SSDIM_EXECUTE	equ	0		;Normal execution time Dim
SSDIM_STATIC	equ	1		;Dim of $Static array

SSDIM_COMMON	equ	2		;Dim of Common $Static array




	;Bit flags used in executorFlags

F_EXEC_ItemPrinted	EQU 1	;TRUE if we've printed something already
F_EXEC_Write		EQU 2	;TRUE if we've seen exStWrite but haven't 
				;actually printed anything yet
;===========================================================================
;Executor entry points
;===========================================================================
ife	CONTEXT_ASM
extrn	EnStaticStructs:far
extrn	DisStaticStructs:far
endif			;CONTEXT_ASM
ife	EXARYUTL_ASM
extrn	oVarToPAd:near
endif			;EXARYUTL_ASM
ife	EXMATHOP_ASM
extrn	CompareI4:near
endif			;EXMATHOP_ASM
ife	EXCONTXT_ASM
extrn	StopGrsContext:near
extrn	StartGrsContext:far
endif			;EXCONTXT_ASM

