;***
; IDMAC.INC - 1-Apr-87 - Internal Debugging Macros
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	To provide a simple, consistent interface to internal debugging
;	code via macros; these macros allow minimal muddying of source
;	code, yet provide the ability to make runtime assertion checks,
;	and give useful status information on failure.
;
;******************************************************************************


DbHalt MACRO	seg,message
	ENDM
DbAssertRel MACRO	parm1,arel,parm2,seg,message
	ENDM
DbAssertRelB MACRO	parm1,arel,parm2,seg,message
	ENDM
DbAssertFlags MACRO	arel,seg,message
	ENDM
DbAssertTst MACRO	parm1,arel,parm2,seg,message
	ENDM
DbChk	MACRO	name,parms
	ENDM
DbPub	MACRO	ep
	ENDM
DJMP	MACRO	op,parm1,parm2
	op	parm1 parm2
ENDM


.LIST
