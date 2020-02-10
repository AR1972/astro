include edityp.inc

MAXCHARS equ 256

DEBPUBLIC	macro	n
ifdef DEBPUB
	public	n
endif
ENDM

; CW util provided routines
externFPublic	SetAlarm

; LIBC routines
externFP	_memset
ifndef	PROJECT_QB
externFP        _memmove
endif
