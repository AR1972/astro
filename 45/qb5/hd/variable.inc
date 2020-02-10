;*** 
; Variable.inc - QB5-specific struct offsets and other constants for the VarMgr
;
;	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
;
;*******************************************************************************
	IncludeOnce	varmgr		;generic QB varmgr stuff

VARIABLE_INC = ON


;==================== Common Block Table =================================

;COM Structure - common block entry
COM	STRUC
COM_ogNam	DW 0	
COM_bdType	DB SIZE BD DUP(0)
COM_bdValue	DB SIZE BD DUP(0)
COM_oTypCur	DW 0
COM_oValCur	DW 0
COM	ENDS

;These equates used only by scanner, but are here because they
;tie closely to COM defintion

SsComSize	equ	(size COM) - 2		; Don't include COM_ogNam
COM_oCom	equ	word ptr COM_oValCur+2	;Next word after end of structure
COM_cbFixed	equ	word ptr COM_oValCur+4	
SsCom		equ	COM_bdType	;Skip bdName



;COMREF Structure -	value part of a COMMON scaler variable
COMREF		STRUC
COMREF_oCommon	DW 0
COMREF_oValue	DW 0
COMREF		ENDS

;ACOM Structure -	value part of a COMMON array variable
ACOM		STRUC
ACOM_oCommon	DW 0
ACOM_cDims	DB 0
ACOM_filler1	DB 0
ACOM_oValue	DW 0
ACOM		ENDS


;This flag bit is stored in the oHashLink field
FV_STATIC	EQU 01h		;TRUE implies array is $STATIC array

FV_TYP_MASK	EQU 07H		;used to mask off oTyp bits from flags word

;entry flag bits
FVFUN		EQU 0008h
FVCOMMON	EQU 0010h
FVSTATIC	EQU 0020h
FVSHARED	EQU 0040h
FVFORMAL	EQU 0080h
FV_STATICSET	EQU 0100h	;TRUE implies $STATIC flag set
FVEVEREXPLICIT	EQU 0200h
FVARRAY		EQU 0400h
FVINDEXED	EQU 0800h
FVVALUESTORED	EQU 1000h
FVDECLDVAR	EQU 2000h
FVREDIRECT	EQU 4000h
FVCONST  	EQU 8000h


;MKVAR_flags Definitions
; Global inputs and outputs to MakeVariable.
; The below constants match existing FV constants (in varmgr.inc), and are
; present to allow parser and rude scanner changes go in without switches.

FVI_FUNCTION	EQU 0008h
FVI_COMMON	EQU 0010h
FVI_STATIC	EQU 0020h
FVI_SHARED	EQU 0040h
FVI_FORMAL	EQU 0080h
FVI_IMPLICIT	EQU 0100h
FVI_LVAL	EQU 0200h
FVI_ARRAY	EQU 0400h
FVI_INDEXED	EQU 0800h
FVI_DIM		EQU 1000h
FVI_ASCLAUSE	EQU 2000h
FVI_FNNAME	EQU 4000h
FVI_CONST	EQU 8000h

FVFUNCTION		EQU 0008h
;FVCOMMON		EQU 0010h	;Same as for VAR_flags above
;FVSTATIC		EQU 0020h	;Same as for VAR_flags above
;FVSHARED		EQU 0040h	;Same as for VAR_flags above
;FVFORMAL		EQU 0080h	;Same as for VAR_flags above
FVIMPLICIT		EQU 0100h
FVLVAL			EQU 0200h
FVFORCEARRAY		EQU 0400h
;FVINDEXED		EQU 0800h	;Same as for VAR_flags above
FVDIM			EQU 1000h
FVASCLAUSE		EQU 2000h
FVFNNAME		EQU 4000h
;FVCONST  		EQU 8000h	;Same as for VAR_flags above

sBegin	CP
	EXTRN	MakeVariable:near
	EXTRN	CreateVar:near
	EXTRN	ReDirect:near
	EXTRN	GetDefaultType:near
	EXTRN	AdjustMrsVarTable:near
	EXTRN	AdjustPrsVarTable:near
ife VARUTIL_ASM
	EXTRN	MakeVariableFar:far
	EXTRN	MakeMrsTVar:near
	EXTRN	MakePrsTVar:far
	EXTRN	MakeCommon:far
	EXTRN	ClearCommon:near
	EXTRN	ResetCommon:near
	EXTRN	VarRudeReset:near
	EXTRN	OTypOfONamDefault:far
	EXTRN	FirstVar:near
	EXTRN	NextVar:near
	EXTRN	ClearMV:near
	EXTRN	ClearPV:near
	EXTRN	CbTyp:near
	EXTRN	CbTypOTyp:near
	EXTRN	CbTypOTypOMrs:near
	EXTRN	CbTypFar:far		    
endif	;VARUTIL_ASM

sEnd	CP
