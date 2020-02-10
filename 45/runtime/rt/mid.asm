	TITLE	MID - Left-hand MID$
	PAGE	56,132
;***
; MID - Left-hand MID$
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
; BASIC Syntax mapping to included runtime entry points:
;
; - MID$ Statement:
;
;      MID$(v$,n[,m]) = y$
;	|
;     B$SMID
;
;******************************************************************************
	INCLUDE switch.inc	
	INCLUDE rmacros.inc	; Runtime Macro Defintions

	USESEG	ST_TEXT 	; String package

	INCLUDE seg.inc 	; define segments

assumes CS,ST_TEXT		
sBegin	ST_TEXT 		

	externNP B$STDALCTMP	
	externNP B$ERR_FC	

	SUBTTL	B$SMID - MID$ statement
	PAGE
;***
; B$SMID - MID$ statement
; void pascal B$SMID(I2 offStart, I2 maxLen, sd *psdSource, data far *pDest,
;		I2 cbDest)
;
;Function:
; Copy the source string into the destination string. Copying limited to
; present length of destination.
;
; NOTE: This routine can take a far pointer to a movable item in a heap. This
; routine cannot directly or indirectly cause heap movement.
;
;Inputs:
; pDest 	= destination far address
; cbDest	= length of destination, if fixed length string.
; psdSource	= source string descriptor
; maxLen	= maximum string length to copy
; offStart	= starting offset in the destination string
;
;Outputs:
; None.
;
;Uses:
; Per convention.
;
;Exceptions:
; B$ERR_FC - Invalid params
;
;******************************************************************************
cProc	B$SMID,<PUBLIC,FAR>,<ES,SI,DI> 
parmD	pDest			; destination string descriptor
parmW	cbDest			; length of the destination
parmW	psdSource		; source string descriptor
parmW	maxLen			; Maximum length to copy
parmW	offStart		; starting offset in destination string
cBegin				
	MOV	CX,offStart	;get starting offset in dest string
	DEC	CX		;offset starts at 0, not 1
	JL	ARGERR		;Must be greater than zero

	MOV	AX,maxLen	;get max length to copy
	OR	AX,AX		;Test for valid length
	JL	ARGERR		;Must be zero or more

	LES	DI,pDest	; [ES:DI] = pointer to destination
	MOV	DX,cbDest	; [DX] = length thereof
	OR	DX,DX		; see if fixed or variable length string
	JNZ	SMID_5		; Jump if fixed length
	MOV	DX,[DI] 	; [DX] = length of variable length string
	MOV	DI,[DI+2]	; [DI] = pointer to (ES assumed already DS)
SMID_5: 			

	CMP	CX,DX		; compare offset to current length of string
	JAE	ARGERR		;Offset must not exceed string size
	ADD	DI,CX		;Add in offset
	SUB	CX,DX		; [CX] = offset - length
	NEG	CX		;Length - Offset
	CMP	CX,AX		;Take minimum of this and count parameter
	JB	MIN1
	MOV	CX,AX		;Count parameter was lower
MIN1:
	MOV	BX,psdSource	;Get source where we can address with it
	CMP	CX,[BX] 	;Compare current move length to string length
	JBE	MIN2
	MOV	CX,[BX] 	;Source string was smaller
MIN2:
	MOV	SI,[BX+2]	;Get pointer to source data
	REP	MOVSB		;Must use bytes for case when source = dest
	CALL	B$STDALCTMP	;Delete source if temp string
cEnd				

ARGERR:
	JMP	B$ERR_FC	;Illegal function call

sEnd	ST_TEXT 		
	END
