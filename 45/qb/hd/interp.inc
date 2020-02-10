page 49,132
;*** 
;interp.inc
;
;	Copyright <C> 1985, 1986, 1987 Microsoft Corporation
;
;For comments, see interp.h
;
;
;*******************************************************************************

TRUE		equ 1
FALSE		equ 0
NULL		equ 0		; System NULL for pointers
UNDEFINED	equ 65535d	; System NULL for offsets
MAX_LN		equ 65529d	; maximum legal line number
CB_IDNAM_MAX	equ 40d 	; max legal size of BASIC identifier

;This structure and following equates added with [18]
;pre-defined oTyp values (type constants)
;be sure to add all new types to the global table which follows.
ET_TYPES	struc
ET_IMP		db	?	; Implicitly typed variable
ET_I2		db	?	; 16 bit signed integer
ET_I4		db	?	; 32 bit signed integer

ET_R4		db	?	; 32 bit real

ET_R8		db	?	; 64 bit real


ET_SD		db	?	;String descriptor

ET_FS		db	?	


ET_TYPES	ends

ET_MAX	equ	size ET_TYPES - 1

ET_MaxNum	equ	ET_R8	

ET_MaxStr	equ	ET_FS	;[11]


; WARNING: be sure to make equivalent types in interp.h
; global et type numbers.  all new types should be appended to the end.
; added for [22]

; the next 5 types MUST be in the same order as the et types in QB 4.0

GLBL_IMP	EQU	0	    ; Implicitly typed variable
GLBL_I2 	EQU	1	    ; 16 bit signed integer
GLBL_I4 	EQU	2	    ; 32 bit signed integer
GLBL_R4 	EQU	3	    ; 32 bit real
GLBL_R8 	EQU	4	    ; 64 bit real
GLBL_SD 	EQU	5	    ; String descriptor

GLBL_CY 	EQU	6
GLBL_TX 	EQU	7	    ; Text descriptor
GLBL_FS 	EQU	8	    ; Fixed length string
GLBL_FT 	EQU	9	    ; Fixed length text
GLBL_FIELD	EQU	10
GLBL_FORM	EQU	11
GLBL_MENU	EQU	12

GLBL_ET_MAX	EQU	12	    ; number of global et types defined


CBASETYPEMAX	equ	6;Number of opcodes for different types
			 ;	i.e. number of explicit types + 1 for implicit

StartOtx	equ	0

SetStartOtx	MACRO	dest
	xor	dest,dest	;2-byte instruction for zero
	ENDM

opLitI2Max  equ     10		;Maximum literal with own executor

;SD Structure - variable length string value
SD	STRUC
SD_cb		DW	0
SD_pb		DW	0
SD	ENDS

;****************************************************************************
;includeOnce <file>
;Purpose:
;	This macro is used to make sure that include files are only
;	included once.  The included file must set <file>_INC to ON.
;	The following would include foo.inc.
;
;		includeOnce foo		;foo.inc should have the following:
;					;FOO_INC = ON
;RESTRICTIONS:
;	the include name can't contain a pathname and must omit the .inc
;	suffix.
;****************************************************************************

includeOnce	macro	name
ifndef	name&_INC
	include	name.inc   ;;include the .inc file if it hasn't already been
			   ;;included
name&_INC=OFF
endif
if2
if	name&_INC EQ OFF
	include	name.inc   ;;Include it once on the 2nd pass, too
name&_INC=ON
endif
endif
	endm
;end of 'includeOnce' macro

;
;The OrdConstStart and OrdConst macros are used to define
;	an ordered set of constants
;
OrdConstStart	MACRO	start	
OrdConstCur = start 		
	ENDM			

OrdConst	MACRO	name	
name	EQU	OrdConstCur+0	;+0 prevents name from just being an alias
OrdConstCur = OrdConstCur + 1	
	ENDM			

?NODATA =	1		;always on so ATOMIC can be used
				;harmless if ?WIN == 0
include	cmacros.inc

;macro	sBegin
;Purpose:
;The following macro is a front end to the sBegin macro in cmacros.inc.
;The reason for this macro front end is to create segments only for those
;segments that are actually used.  This is especially important in light
;of the fact that codeview only lists symbols in the first encountered
;code segment.
;
;Macro sBeginC is identical to the sBegin macro found in cMacros.  It
;was created by modifying the name of the macro in cmacros.inc.

DFP_NONE	EQU	0
DFP_FAR 	EQU	1
DFP_CODE	EQU	2
DFP_CP		EQU	3

sBegin	macro	logname
segOK	= 0
?DFP	= DFP_FAR		;; assume routine IS to be called, and far
ifidn	<&logname>,<LOADRTM>
ifCreateSeg	LOADRTM,LOADRTM,CODE
endif
ifidn	<&logname>,<FAR_PRESERVED>			;
ifCreateSeg	FAR_PRESERVED,FAR_PRESERVED,CODE	;
endif							;
ifidn	<&logname>,<LOADRTMEND>
ifCreateSeg	LOADRTMEND,LOADRTMEND,CODE
endif
ifidn	<&logname>,<RT>
ifCreateSeg	CODE,RT,CODE
endif
ifidn	<&logname>,<CODE>
ifCreateSeg	_TEXT,CODE,CODE
?DFP	= DFP_CODE		;
endif
ifidn	<&logname>,<MATH>	;
ifCreateSeg	_TEXT,MATH,CODE ;
endif				;
ifidn	<&logname>,<CP>
ifCreateSeg	CP,CP,CODE
?DFP	= DFP_CP		;
endif
ifidn	<&logname>,<UI>
ifCreateSeg	UI,UI,CODE
endif
ifidn	<&logname>,<LIST>
ifCreateSeg	LIST,LIST,CODE
endif
ifidn	<&logname>,<DBG>
ifCreateSeg	DBG,DBG,CODE
?DFP	= DFP_NONE		;
endif
ifidn	<&logname>,<ID_STRINGS>
ifCreateSeg	ID_STRINGS,ID_STRINGS,CODE
endif
ifidn	<&logname>,<RARE>
ifCreateSeg	RARE,RARE,CODE
endif
ifidn	<&logname>,<SCAN>	;
ifCreateSeg	SCAN,SCAN,CODE	;
endif				;
ifidn	<&logname>,<DATA>
segOK	= 1
endif
ifidn	<&logname>,<_TEXT>	;
ifCreateSeg	_TEXT,_TEXT,CODE;
endif				;
.erre	segOK			;;Error if logname wasn't one of the above.
sBeginC	&logname		;;See note above if this becomes undefined.
	endm

ifCreateSeg	macro	segment,logname,class
segOK	= 1
ifndef	&logname&_sBeginC
createSeg	segment,logname,WORD,PUBLIC,class
endif
		endm

createSeg	_DATA,data,word,public,DATA,DGROUP
defGrp DGROUP,DATA

ifndef NODEBUG	
include debug.inc
endif		
