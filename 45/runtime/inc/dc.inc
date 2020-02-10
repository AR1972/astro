;***
;dc.inc - 20-Mar-86 - dc macro definition
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

;       Data definition macros                  (formerly DATMAC.INC)

DC	MACRO	txt
	.xcref
__Pchr	=	0
	IRPC	chr,<txt>
IF	__Pchr
	DB	&__Pchr
	ENDIF
__Pchr	=	"&chr"
	ENDM
	DB	__Pchr+128
	.cref
	ENDM

	.LIST
