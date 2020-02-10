;*
;*	CW : Character Oriented Windows
;*
;*	kanji.inc : Kanji / DBCS support

;*	* first range: 81->9F
chDbcMin1	=	081H
chDbcMac1	=	09FH+1
;*	* second range: E0->FC
chDbcMin2	=	0E0H
chDbcMac2	=	0FCH+1


;********** JmpNotDbc **********
;*	entry: al = first character
;*	* jump to label if not first byte of D.B. character
;*	* Trashes nothing except flags

JmpNotDbc MACRO label
	LOCAL	is_dbc
	cmp	al,chDbcMin1
	jb	label
	cmp	al,chDbcMac1
	jb	is_dbc
	cmp	al,chDbcMin2
	jb	label
	cmp	al,chDbcMac2
	jae	label
is_dbc:
ENDM


