	TITLE	messages.asm - message text module
;***
;messages.asm - message text module
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; Single module which contains all runtime text messages.
;
;******************************************************************************

	INCLUDE	switch.inc	

;
; Segment definitions for message isolation
;
; Far messages
;
FAR_HDR segment byte public 'FAR_MSG'
FAR_HDR ends
FAR_MSG segment byte public 'FAR_MSG'
FAR_MSG ends
FAR_PAD segment byte common 'FAR_MSG'
FAR_PAD ends
FAR_EPAD segment byte common 'FAR_MSG'
FAR_EPAD ends
FMGROUP group FAR_HDR,	FAR_MSG,  FAR_PAD,  FAR_EPAD

public	__acrtmsg		
__acrtmsg=	9876h		

;
;	Far Messages. Get the message text from the include file
;
FAR_MSG SEGMENT

	PUBLIC	b$messages	;a public to force inclusion of messages
b$messages label byte

RTEDEF	MACRO	code,label,number,text	;; just want the C startup messages
	ENDM				;; for QB5

RCEDEF	MACRO	code,label,number,text	;
	IFNB	<text>		;; Be sure text is to be generated
	IFNB	<number>	;; Be sure number is to be generated
	DW	number		;;message number
	ENDIF			;
	DB	text		;;message text
	DB	0		;;null terminator
	ENDIF			;
	ENDM


	INCLUDE messages.inc

FAR_MSG ENDS

	END
