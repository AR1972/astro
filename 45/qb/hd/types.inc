;*** 
;types.inc - externs for the type manager
;
;	Copyright <C> 1985, 1986, 1987, 1988 Microsoft Corporation
;
;
;*******************************************************************************
TYPES_INC = ON

sBegin	CP
ife TYPMGR_ASM
	EXTRN	DefineTyp:far		
	EXTRN	RefTyp:far		
	EXTRN	DefineElem:near
	EXTRN	DefineElemFixed:near
	EXTRN	RefElem:far		
	EXTRN	ONamOElem:far
	EXTRN	ONamOTyp:far
	EXTRN	CompareTyps:far 	
endif
sEnd	CP


