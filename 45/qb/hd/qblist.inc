; File: QBLIST.INC

QBLIST_INC = -1 		 ;remember that this file has been included

;**======================================================================**
;**==		External Interface to QBC Lister support component.    ==**
;**======================================================================**

ife	QBLIST_ASM

sBegin	DATA
	EXTRN	txdCur:BYTE
	EXTRN	prsCur:BYTE
sEnd	DATA

	EXTRN	BdAlloc:FAR
	EXTRN	BdGrow:FAR
	EXTRN	BdRealloc:FAR
	EXTRN	BdlAlloc:FAR
	EXTRN	BdlRealloc:FAR


sBegin	CP
	EXTRN	UpdChanCur:NEAR
	EXTRN	LnOfONam:FAR
	EXTRN	CopyONamPb:FAR
	EXTRN	FieldsOfPrsFar:FAR
	EXTRN	ONamOvarRudeOrParse:FAR
	EXTRN	RelTempSd_CP:NEAR

;	Stubs for routines which should never be called by lister.

	EXTRN	ONamOElem:FAR
	EXTRN	ONamOTyp:FAR
	EXTRN	OTypOfONamDefault:FAR
sEnd	CP

endif	;QBLIST_ASM
