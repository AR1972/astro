;***
; extort.inc
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;	Contains macros for defining post bytes for runtime call dispatch
;	mechanism.
;
;	This include file:
;	- includes rtmint.inc from the runtime project to define postbytes
;	  for each runtime entry point
;	- defines the CALLRT macro for use in dispatching to these entry
;	  points via extort.asm.
;
if	EXOS_ASM
if1
	%OUT	*RT post byte constants *
endif
endif

;	Create entry in table and define as external.

%PWORD	= 0		;Initial post byte value.

if	EXOS_ASM
RTMENT_OUT MACRO	entryname,lw,hg
if1
	%OUT	* P&entryname,lw,hg
endif
	ENDM
endif

RTMENT	MACRO entryname,entryseg
if	EXOS_ASM
	RTMENT_OUT	entryname,%(LOW %PWORD),%(HIGH %PWORD)
endif

P&entryname = %PWORD
%PWORD	= %PWORD + 1
IFE	LOW %PWORD
%PWORD	= %PWORD + 255
ENDIF	;;IFE
	ENDM

;Now define the post byte constants.

	include rtmint.inc

;********************************************************************
;CALLRT macro
;	Aid to using the post byte mechanism for calling the runtime
;	in a space optimized way.
;
;	rtnam  is the name of the runtime routine to be called
;********************************************************************

EXTRN	ExToRt:near		;Call RT, return
EXTRN	ExToRtMov:near		;Call RT, return - - heap movement expected
				; (but ES & DI not updated). In RELEASE case,
				; this acts just like ExToRt.
EXTRN	ExToRtDisp:near 	;Call RT, dispatch
EXTRN	ExToRtByteDisp:near 	;Call RT, dispatch - jmp'd to w/postbyte in ax
EXTRN	ExToRtDispMovAx:near	;Call RT, update di/es, push ax, dispatch
EXTRN	ExToRtDispMov:near	;Call RT, update di/es, dispatch
EXTRN	ExToRtDispAx:near	;Call RT, push ax, dispatch
EXTRN	ExToRtDispDxAx:near	;Call RT, push dx:ax, dispatch
EXTRN	ExToRtDispMovDxAx:near	;Call RT, update di/es, push dx:ax, dispatch
EXTRN	ExToRtDispR4:near	;Call RT, push R4 pointed to by AX, dispatch
EXTRN	ExToRtDispR8:near	;Call RT, push R8 pointed to by AX, dispatch
EXTRN	ExToRtDispMovR8:near	;Call RT, push R8 pointed to by AX, dispatch
EXTRN	ExToRtCall:near 	;Call RT, CALL w/ reg & error protection, ax = postbyte





CALLRT	MACRO	rtnam,rttyp
	call	ExToRt&rttyp
	db	LOW P&rtnam
IF	(P&rtnam GE 00FFH)
	db	HIGH P&rtnam
ENDIF
	ENDM
