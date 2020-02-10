	TITLE	STRINGFP - Floating Point String Functions
	PAGE	56,132
;***
; STRINGFP - Floating Point ST$ functions
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
;
; - STR$ Function -
;
;      v$ = STR$(x)
;
;    Examples:
;
;      v$ = STR$(b@)   v$ = STR$(a!)	    v$ = STR$(x#)
;	     |		     |			  |
;	   B$STCY	   B$STR4		B$STR8
;
;
;****

	INCLUDE switch.inc
	INCLUDE rmacros.inc

	useSeg	_DATA
	USESEG	_BSS
	useSeg	ST_TEXT

	INCLUDE seg.inc
	INCLUDE rtps.inc
	INCLUDE baslibma.inc


sBegin	ST_TEXT

	ASSUMES CS,ST_TEXT

	externNP B$FloatCONASC	;Pull in floating point conversion routines
	externNP B$STR_COMMON	;Common support for STR$

	SUBTTL	STR$ - Create String from number
	PAGE
;***
;B$STR4, B$STR8, B$STCY - STR$ function support
;
;Purpose:
;	Runtime Entry Points
;	Create a string representing the number in ASCII
;
;Entry:
;	parameter value is on the stack (R4, R8 or CY)
;
;Exit:
;	AX = Address of string descriptor
;
;Uses:
;	Per Convention
;
;Exceptions:
;	Out of memory
;****

cProc	B$STR4,<PUBLIC,FAR>
parmD	arg4
cBegin
	MOV	AL,VT_R4
	LEA	BX,arg4
	cCall	B$STR_COMMON
cEnd


cProc	B$STR8,<PUBLIC,FAR>
ParmQ	R8Arg
cBegin
	MOV	AL,VT_R8	;AL = data type
	LEA	BX,R8Arg	;BX = ptr to data
	cCall	B$STR_COMMON	;call common routine to convert
cEnd


sEND	ST_TEXT
	END
