; File: DEBUG.INC
; NOTE: When making changes to this file, be sure to consider equivalent
;	changes to DEBUG.H

DEBUG_INC = ON

DbTimer	MACRO
	ENDM

DbMessTimer	MACRO	seg,mess
	LOCAL	szMess
	ENDM

;==============================================================================
;ClockOn, ClockOff macros
;
;These macros are for use with PROFILE.EXE. When the /n switch is given to
;PROFILE.EXE, it allows us to turn on and off the profiling clock, so we
;only profile those sections of code we're interested in.
;Rather than some complicated system, these macros are always present.
;The specific modules where we wish to turn on and off the clock must be
;re-assembled with the appropriate macros whenever we want to profile something.
;
;The clock is initially off, and will be turned off automatically after the
;profiled code terminates.  Redundant off or on directives will be ignored
;and are harmless.
;==============================================================================
ClockOn     macro
					;;in non-RELEASE case
	push	ax
	mov	ax, 1
	int	3
	pop	ax
endm

ClockOff    macro
					;;in non-RELEASE case
	push	ax
	xor	ax, ax
	int	3
	pop	ax
endm

include idmac.inc			;assertion macros shared with runtime
