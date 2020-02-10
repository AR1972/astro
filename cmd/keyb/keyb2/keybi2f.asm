;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1987 - 1991
; *                      All Rights Reserved.
; */
	PAGE	,132
;
;                                           
; MS-DOS 5.00 - NLS Support - KEYB Command
;
; File Name:  KEYBI2F.ASM
; ----------
;
; Description:
; ------------
;	Contains Interrupt 2F handler.
;
; Procedures Contained in This File:
; ----------------------------------
;	KEYB_INT_2F - Interupt 2F handler
;
; Include Files Required:
; -----------------------
;	INCLUDE KEYBEQU.INC
;	INCLUDE KEYBSHAR.INC
;M004	INCLUDE KEYBMAC.INC
;	INCLUDE KEYBCMD.INC
;	INCLUDE KEYBCPSD.INC
;	INCLUDE KEYBI9C.INC
;
; External Procedure References:
; ------------------------------
;	FROM FILE  ????????.ASM:
;		procedure - description????????????????????????????????
;
; Linkage Information:  Refer to file KEYB.ASM
; --------------------
;
; Change History:
; ---------------

	INCLUDE KEYBEQU.INC		
	INCLUDE KEYBSHAR.INC		
;M004	INCLUDE KEYBMAC.INC		
	INCLUDE KEYBCMD.INC		
	INCLUDE KEYBCPSD.INC		
	INCLUDE KEYBI9C.INC		
					
	PUBLIC KEYB_INT_2F		
					
	EXTRN  ERROR_BEEP:NEAR	 
					
CODE	SEGMENT PUBLIC 'CODE'	  
					
	ASSUME  CS:CODE,DS:nothing

; Module: KEYB_INT_2F
;
; Description:
;
; Input Registers:
;	AH = 0ADH
;	AL = 80,81,82,83	; M003
;
; Output Registers:
;	N/A
;
; Logic:
;	IF AH = 0ADh THEN	 (this call is for us)
;	Set carry flag to 0
;	IF AL = 80 THEN
;	  Get major and minor
;	  Get SEG:OFFSET of SHARED_DATA_AREA
;
;	IF AL = 81 THEN
;	  Get FIRST_XLAT_PTR
;	  FOR each table
;		IF code page requested = code page value at pointer THEN
;		Set INVOKED_CODE_PAGE
;		Set ACTIVE_XLAT_PTR
;		EXIT
;		ELSE
;		Get NEXT_SECT_PTR
;	  NEXT table
;	  IF no corresponding code page found THEN
;		Set carry flag
;
;	IF AL = 82 THEN
;	  IF BL = 00 THEN
;		Set COUNTRY_FLAG = 00
;	  ELSE IF BL = 0FFH THEN
;		Set COUNTRY_FLAG = 0FFH
;	  ELSE
;		Set carry flag
;
;	IF AL = 83 THEN              ; M003
;	  Return BL=COUNTRY_FLAG     ; M003
;
;	JMP to previous INT 2FH handler

CP_QUERY	EQU	80H		
CP_INVOKE	EQU	81H		
CP_LANGUAGE	EQU	82H		
CP_QLANGUAGE	EQU	83H		; M003
					
VERSION_MAJOR	EQU	01H		
VERSION_MINOR	EQU	00H		
					
CARRY_FLAG	EQU	01H		 
					
KEYB_INT_2F	PROC			

	cmp	ah,INT_2F_SUB_FUNC	; is it for us?
	jz	our_i2f_interrupt

i2f_chain:

;	Under DOS 5, it is always safe for us to assume that there was
;	  an existing Int2f vector for us to continue to.

	jmp	cs:sd.old_int_2f
					
our_i2f_interrupt:
	push	bp
	mov	bp,sp
	and	word ptr [bp]+6,not carry_flag ; pre-clear carry
	call	do_our_i2f		; pass bp.6 -> flags to functions

	pop	bp
	jmp	i2f_chain

do_our_i2f:
	CMP	AL,CP_QUERY		; Q..query CP?
	JNE	INT_2F_CP_INVOKE	; N..next

	MOV	AX,-1			; Y..process query
	mov	bx,(version_major shl 8) + version_minor
	MOV	DI,OFFSET SD		
	PUSH	CS
	POP	ES			
	ret
					
INT_2F_CP_INVOKE:			
	CMP	AL,CP_INVOKE		; Q..invoke CP?
	JNE	INT_2F_CP_LANGUAGE	; N..next
					
	MOV	SI,cs:SD.FIRST_XLAT_PTR	; Get FIRST_XLAT_PTR
					
INT_2F_NEXT_SECTION:			
	CMP	SI,-1			
	JE	INT_2F_ERROR_FLAG

	cmp	bx,cs:[SI].XS_CP_ID	; is this the code page we want?
	JNE	INT_2F_CP_INVOKE_CONT1

	MOV	cs:SD.ACTIVE_XLAT_PTR,SI ; IF Yes, Set the ACTIVE_XLAT_PTR
	MOV	cs:SD.INVOKED_CP_TABLE,BX ;	record new code page
	ret
					
INT_2F_CP_INVOKE_CONT1:
	MOV	SI,cs:[SI].XS_NEXT_SECT_PTR ; Chain to NEXT_SECT_PTR
	JMP	INT_2F_NEXT_SECTION	;	NEXT_SECTION
					
INT_2F_ERROR_FLAG:			
	mov	ax,1			; ***???  why do we return error code
;					;   only in this case?????
i2f_reterror:
	or	word ptr [bp]+6,carry_flag ; set carry to int2f caller
	ret
					
INT_2F_CP_LANGUAGE:			
	CMP	AL,CP_LANGUAGE		; Q..Set default language??
;M003	jnz	int2f_ret		; don't handle undefined functions
	jnz	INT_2F_CP_QLANG		; go check for query language ;M003
					
;	Now, if BL=0 or 0ffh, we'll set COUNTRY_FLAG to that value.

	inc	bl
	cmp	bl,2			; set carry if bl is legal
	dec	bl			; restore old value, preserve carry
	jnc	i2f_reterror		; done if error

	MOV	cs:COUNTRY_FLAG,BL	;	Set COUNTRY_FLAG to 0 or 0ffh

;	M003 -- added code

	ret


INT_2F_CP_QLANG:
	CMP	AL,CP_QLANGUAGE
	jnz	int2f_ret

	mov	bl,cs:COUNTRY_FLAG

;	M003 -- end added code

int2f_ret:
	ret
KEYB_INT_2F	ENDP			

CODE	ENDS
	END
