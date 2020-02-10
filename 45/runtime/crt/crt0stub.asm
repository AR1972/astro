	TITLE	crt0stub - stub replacement for crt0*.* (QLib only)
;***
; crt0stub - stub replacement for crt0*.* (QLib only)
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	Quick Libs need the C startup for linking, but never execute it.
;	This stub contains the appropriate PUBLICs so that the link will
;	work, but contains no code.
;
;	Note:	Before this module can be linked into BQLB40 successfully
;		it must be run through DOSSEG for C segment ordering.
;
;*******************************************************************************

	DOSSEG				;[5]

	extrn	B$IEND:FAR		;[3]

_TEXT	segment byte public 'CODE'

public	__acrtused			; trick to force in startup
	__acrtused = 9876h

assume	cs:_TEXT
	public	_exit			;[3]
	public	__exit			;[3]
__astart:

__cinit:
__ctermsub:
_exit:
__exit:
	jmp	B$IEND			;[3] vector to B$IEND to stop execution

__cexit	proc far			;[6]
	public	__cexit			;[6]
	ret				;[6]
__cexit endp				;[6]

_TEXT	ends

	end	__astart 		; start address
