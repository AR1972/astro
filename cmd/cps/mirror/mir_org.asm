; Copyright (c) 1990-1991 Central Point Software, Inc. 
; All rights reserved
;
; This is the 1st module of MIRROR.COM
;
;  Change Log:
;
;    Date    Who   #			  Description
;  --------  ---  ----  -----------------------------------------------------
;  02/11/91  DLB  M006  Changed "entry" symbol to "$entry"


CODE	SEGMENT PARA PUBLIC 'CODE'
	ASSUME	CS:CODE, DS:nothing, ES:nothing

major_ver EQU	6
minor_ver EQU	6
;
major_digit EQU major_ver + "0"
minor_digit EQU minor_ver + "0"
;
	EXTRN	start:near		;In the _DTRK module.
	EXTRN	end_prog:byte		;In the _LAST module.
;
	PUBLIC	major_digit, minor_digit
	PUBLIC	$entry, org_id_length   ;M006
	PUBLIC	org_id2, org_id2_length
;
asm_message MACRO x1,x2,x3,x4,x5,x6,x7
	%OUT	*MSG: x1&x2&x3&x4&x5&x6&x7
	ENDM
;
IF2
	asm_message <Mirror version >,%major_ver,<.>,%minor_ver
ENDIF
;
	ORG	100h
$entry:	jmp	start           ;M006
;
	ORG	$entry + 3      ;M006
org_id2 LABEL byte
	DB	" Mirror "
org_id2_length EQU ($ - org_id2)
;
version_text LABEL byte
	DB	major_digit
	DB	"."
	DB	minor_digit
	DB	"m "
	DB	1Ah
;
	DW	end_prog	;Changes with program size.
;
org_id_length EQU ($ - $entry)  ;M006
;
CODE	ENDS
	END	$entry

