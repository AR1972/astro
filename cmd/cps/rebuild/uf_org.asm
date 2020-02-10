; Copyright 1990 Central Point Software, Inc. 
; All rights reserved.
;------------------------------------------------------------
; This is the first module of UnFormat.
; LINK uf_org+uf_main+uf_io+uf_msg+uf_partn+uf_jims+uf_last, uf.exe/m;
;
;       M008    MD      2/28/91         Changed copyright text

prog	SEGMENT public
	ASSUME	CS:prog
	PUBLIC	top_of_mem, fcb1, parms, entry                  ;M008
	PUBLIC	major_ver, minor_ver
	EXTRN	start:near
cr	EQU	13
lf	EQU	10
;
major_ver EQU	6
minor_ver EQU	6
;
asm_message MACRO	x1,x2,x3,x4,x5,x6,x7,x8,x9
	%OUT	*MSG: x1&x2&x3&x4&x5&x6&x7&x8&x9
	ENDM
;
IF2
	asm_message <Version >,%major_ver,<.>,%minor_ver
ENDIF
;
cr	EQU	13
lf	EQU	10
;
;-----------------------------------------------------------
;
	ORG	2
top_of_mem DW	?
	ORG	5Ch
fcb1	DB	?
	ORG	80h
parms	DB	?
;
	ORG	100h
entry:	jmp	start
;
;

;M008 start
        DB 13,10,"Copyright (C) 1987-1990"
	DB	"  Central Point Software, Inc. "
;banner	DB	"PC Tools Rebuild/UnFormat "
;
;	DB	major_ver +"0"
;	DB	"."
;	DB	minor_ver + "0"
;	DB	"M"
;	DB	" "
;	DB	cr,lf
;	DB	0
;	DB	24h	;To end string when used from UF_JIMS.
;
;M008 end

        DB	1Ah
prog	ENDS
	END	entry
