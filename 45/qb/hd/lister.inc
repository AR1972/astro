; File: LISTER.INC
; NOTE: When making changes to this file, be sure to make equivalent
;	changes to file LISTER.H
; NOTE: For comments, see lister.h

LISTER_INC = -1			;remember that this file has been included

;**======================================================================**
;**==           External Interface to Lister Component                 ==**
;**==           This file is included by non-lister-component modules  ==**
;**======================================================================**


sBegin	LIST
ife	LSMAIN_ASM
	EXTRN	ListLine:far
	EXTRN	ListBaseNum:far
	EXTRN	ListNum:far
endif	;LSMAIN_ASM
sEnd	LIST

sBegin	DATA
ife	LSMAIN_ASM
	EXTRN	fLsIncluded:byte
	EXTRN	fLsDynArrays:byte
	EXTRN	otxListNext:word
	EXTRN	otxListNextInc:word
	EXTRN	otxLsCursor:word
	EXTRN	cLeadingSpaces:byte
endif	;LSMAIN_ASM

ife	LSRULES_ASM
	EXTRN	psdLsIncl:word
endif	;LSRULES_ASM

sEnd	DATA

