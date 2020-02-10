;*
;*	CW : Character Windows
;*
;*	osspec.asm : jump table for OS specific routines (DUAL only)

	include cmacros.inc
	include version.inc
	include _osspec.inc			;* defines OSSPEC_ENTRIES

;*****************************************************************************

sBegin	DATA

externW	fProtectMode

sEnd	DATA

;*****************************************************************************


OSSPEC_DEF	MACRO	label,os		;* external refernces
	externFP	<label&os>
ENDM


OSSPEC_LABEL	MACRO	label,os		;* jump table, exported labels
	PUBLIC	label
label:
	jmp	label&os
ENDM


OSSPEC_NOLABEL	MACRO	label,os		;* alternate jump table
	jmp	label&os
ENDM


OSSPEC_OS2PROC	MACRO	label,os		;* protect mode only procedure
	PUBLIC	label
label:
IFDEF	DEBUG
	mov	cx,fProtectMode
	jcxz	@F
	jmp	label&OS2
@@:
	jmp	os2_only
ELSE	;!DEBUG
	jmp	label&OS2
ENDIF	;!DEBUG
ENDM


OSSPEC_DOS3PROC	MACRO	label,os		;* real mode only procedure
	PUBLIC	label
label:
IFDEF	DEBUG
	mov	cx,fProtectMode
	jcxz	@F
	jmp	dos3_only
@@:
ENDIF	;DEBUG
	jmp	label&DOS3
ENDM


;*****************************************************************************

createSeg   OSSPEC_TEXT,OSSPEC_TEXT,BYTE,PUBLIC,CODE


;* externFP	CowAssertFailed ;* in _osspec.inc already


OSSPEC_ENTRIES	OSSPEC_DEF,OS2
OSSPEC_ENTRIES	OSSPEC_DEF,DOS3

OSSPEC_OS2ONLY	OSSPEC_DEF,OS2
OSSPEC_DOS3ONLY	OSSPEC_DEF,OS2
OSSPEC_DOS3ONLY	OSSPEC_DEF,DOS3

sBegin	OSSPEC_TEXT
	assumes CS,OSSPEC_TEXT
	assumes DS,DATA

	PUBLIC	rgfnosspec,rgfnosspec3

;*	* rgfnosspec must preceed rgfnosspec3 and the two tables must be
;*	* contiguous!!

rgfnosspec:

;*	General code
	OSSPEC_ENTRIES	OSSPEC_LABEL,OS2

;*	OS/2 only procs
	OSSPEC_OS2ONLY	OSSPEC_LABEL,OS2

;*	Stubs for OS/2 (see stubs.asm)
	OSSPEC_DOS3ONLY	OSSPEC_LABEL,OS2

rgfnosspec3:

;*	General code
	OSSPEC_ENTRIES	OSSPEC_NOLABEL,DOS3

;*	Stubs for DOS3 (see stubs.asm)
	OSSPEC_OS2ONLY	OSSPEC_NOLABEL,DOS3

;*	DOS3 only procs
	OSSPEC_DOS3ONLY	OSSPEC_NOLABEL,DOS3

;*	* mode specific entry points

IFDEF	DEBUG
os2_only:
	cCall	CowAssertFailed
	DB	"Protect mode only proc called during real mode!$"
ENDIF	;DEBUG


IFDEF	DEBUG
dos3_only:
	cCall	CowAssertFailed
	DB	"Real mode only proc called during protect mode!$"
ENDIF	;DEBUG


sEnd	OSSPEC_TEXT

;*****************************************************************************

	END
