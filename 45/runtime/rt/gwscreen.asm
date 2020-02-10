      TITLE   GWSCREEN - Support for stmt
;***
;GWSCREEN - Support for stmt
;
; Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - SCREEN Statement:
;
;     SCREEN [mode][,[burst][,[apage][,vpage]]]
;	|
;     B$CSCN
;
;******************************************************************************
	INCLUDE switch.inc	;Product switches
	INCLUDE rmacros.inc	;Runtime Macro Defintions

	USESEG	CN_TEXT 	;Place in CN segment, when defined

	INCLUDE seg.inc 	;Segment definitions


assumes CS,CN_TEXT
sBegin	CN_TEXT

externNP B$ERR_FC		;Illegal functionc call
externFP B$ScCleanUpParms	;Clean up parameters
externNP B$SCRSTT		;Low level screen statement support
externNP B$ScSetup		;Setup parameters


	SUBTTL	B$CSCN - execute SCREEN Statement
	PAGE
;***
;B$CSCN - execute SCREEN Statement
;void pascal B$CSCN([I2 flag,<I2 parm>]...,cwParams)
;
;Purpose:
; Pass parameters on to low level SCREEN statement routine (B$SCRSTT). There is
; one flag per parameter. If flag is zero, then the parameter was defaulted. If
; flag is non-zero, the next word in parameter block contains user specifed
; value. No dummy values are specified.
;
;Entry:
; parameter block is on stack. First word is count of params, excluding the
; count parameter.
;
;Exit:
; None.
;
;Uses:
; Per convention
;
;Exceptions:
; Could result in a jump to B$ERR_FC if parameter is too large or if some error
; occurs in B$SCRSTT
;
;******************************************************************************
cProc	B$CSCN,<PUBLIC,FAR>
cBegin	<nogen>
	cCall	B$ScSetup	;set up frame, parms
	cCALL	B$SCRSTT	;call low level SCREEN statement support
	JB	FUNC_ERROR	;brif error occurred
	JMP	B$ScCleanUpParms;clean up stack and exit...
cEnd	<nogen> 		;fall into routine to clean up and return

FUNC_ERROR:
	JMP	B$ERR_FC	;Bad Parm, complain.

sEnd	CN_TEXT

	END
