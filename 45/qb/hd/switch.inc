;*** 
;switch.inc - see switch.h for real definitions of switches
;
;	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
;
;
;*******************************************************************************

;===============================================================
; This file is the first thing included by VERSION.INC.
;===============================================================

ON	EQU	(-1)
OFF	EQU	(0)

include filename.inc	;include the _INC & _ASM switches

RELEASE         = ON
NOEXTRNS	= OFF	;ON - don't include EXTRN definitions in include files.
?WIN		= OFF	;ON - cmacros.inc generates WINDOWS entries and exits
memM 		= ON	;ON - cmacros.inc generates medium model code
?PLM		= ON	;ON - cmacros.inc generates PLM calling sequence
?TF		= ON	;ON - cmacros.inc - cEnd generates no 'mov sp,bp'
