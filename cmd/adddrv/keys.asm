;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

	.model	small, c
	include version.inc
	.code

;/***********************************************************************
; *
; *  void keyoff()
; *	 key input disable.
; *
; *	 input	 :
; *		   none.
; *
; *	 output  :
; *		   none.
; *
; *	 call	 :
; *
; ***********************************************************************
; */
keyoff	proc

IF	IBMVER
	pushf
	cli
	xor	cx,cx
@@:
	in	al,64h
	test	al,2
	loopnz	@B			; Wait K/B ready
	mov	al,0adh 		; KEY-DISABLE command
	out	64h,al
	popf
ELSE
%OUT	keyoff() should be created by OEM
ENDIF
	ret
keyoff	endp

;/***********************************************************************
; *
; *  void keyon()
; *	 key input enable.
; *
; *	 input	 :
; *		   none.
; *
; *	 output  :
; *		   none.
; *
; *	 call	 :
; *
; ***********************************************************************
; */
keyon	proc

IF	IBMVER
	pushf
	cli
	xor	cx,cx
@@:
	in	al,64h
	test	al,2
	loopnz	@B			; Wait K/B ready
	mov	al,0aeh 		; KEY-ENABLE command
	out	64h,al
	popf
ELSE
%OUT	keyon() should be created by OEM
ENDIF
	ret
keyon	endp

	end
