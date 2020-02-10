page	60,132								 ;AN000;
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
name	_parse								 ;AN000;
title	C	to PARSER interface					 ;AN000;
;-------------------------------------------------------------------	 ;AN000;
;									 ;AN000;
;	MODULE: 	_parse						 ;AN000;
;									 ;AN000;
;	PURPOSE:	Supplies an interface between C programs and	 ;AN000;
;			the DOS parser					 ;AN000;
;									 ;AN000;
;	CALLING FORMAT: 						 ;AN000;
;			parse(&inregs,&outregs);			 ;AN000;
;									 ;AN000;
;	DATE:		5-21-87 					 ;AN000;
;									 ;AN000;
;-------------------------------------------------------------------	 ;AN000;
;									 ;AN000;
;	extrn	sysparse:far						 ;AN000;
;									 ;AN000;
	public	_parse							 ;AN000;
;									 ;AN000;
;-------------------------------------------------------------------	 ;AN000;
FarSW	equ	0			; make sysparse be a NEAR proc	 ;AN000;
TimeSW	equ	0			; Check time format		 ;AN000;
FileSW	equ	1			; Check file specification	 ;AN000;
CAPSW	equ	1			; Perform CAPS if specified	 ;AN000;
CmpxSW	equ	0			; Check complex list		 ;AN000;
NumSW	equ	1			; Check numeric value		 ;AN000;
KeySW	equ	1			; Support keywords		 ;AN000;
SwSW	equ	1			; Support switches		 ;AN000;
Val1SW	equ	1			; Support value definition 1	 ;AN000;
Val2SW	equ	1			; Support value definition 2	 ;AN000;
Val3SW	equ	1			; Support value definition 3	 ;AN000;
DrvSW	equ	0			; Support drive only format	 ;AN000;
QusSW	equ	1			; Support quoted string format	 ;AN000;
;-------------------------------------------------------------------	 ;AN000;
DGROUP	GROUP	_DATA
PGROUP	GROUP	_TEXT

_DATA	segment byte public 'DATA'	;AN000;
BASESW	=	1			;SPECIFY, PSDATA POINTED TO BY "DS"
INCSW	=	0			;PSDATA.INC IS ALREADY INCLUDED
	INCLUDE VERSION.INC
	INCLUDE PSDATA.INC		;PARSER'S WORK SPACE
_DATA	ends				;AN000;

_TEXT	segment byte public 'CODE'	;AN000;

	ASSUME	CS: PGROUP		;AN000;
	ASSUME	DS: DGROUP		;AN000;
;-------------------------------------------------------------------	 ;AN000;
.xlist									 ;AN000;
include parse.asm			; include the parser		 ;AN000;
.list									 ;AN000;
;-------------------------------------------------------------------	 ;AN000;
;									 ;AN000;
_parse	proc	near							 ;AN000;
	push	bp			; save user's base pointer       ;AN000;
	mov	bp,sp			; set bp to current sp		 ;AN000;
	push	di			; save some registers		 ;AN000;
	push	si							 ;AN000;
;									 ;AN000;
;	copy C inregs into proper registers				 ;AN000;
;									 ;AN000;
	mov	di,[bp+4]		; fix di (arg 0)		 ;AN000;
;									 ;AN000;
	mov	ax,[di+0ah]		; load di			 ;AN000;
	push	ax			; the di value from inregs is now on stack ;AN000;
;									 ;AN000;
	mov	ax,[di+00]		; get inregs.x.ax		 ;AN000;
	mov	bx,[di+02]		; get inregs.x.bx		 ;AN000;
	mov	cx,[di+04]		; get inregs.x.cx		 ;AN000;
	mov	dx,[di+06]		; get inregs.x.dx		 ;AN000;
	mov	si,[di+08]		; get inregs.x.si		 ;AN000;
	pop	di			; get inregs.x.di from stack	 ;AN000;
;									 ;AN000;
	push	bp			; save base pointer		 ;AN000;
;									 ;AN000;
;-------------------------------------------------------------------	 ;AN000;
	call	sysparse		; call the parser		 ;AN000;
;-------------------------------------------------------------------	 ;AN000;
;									 ;AN000;
	pop	bp			; restore base pointer		 ;AN000;
	push	di			; the di value from call is now on stack ;AN000;
	mov	di,[bp+6]		; fix di (arg 1)		 ;AN000;
;									 ;AN000;
	mov	[di+00],ax		; load outregs.x.ax		 ;AN000;
	mov	[di+02],bx		; load outregs.x.bx		 ;AN000;
	mov	[di+04],cx		; load outregs.x.cx		 ;AN000;
	mov	[di+06],dx		; load outregs.x.dx		 ;AN000;
	mov	[di+08],si		; load outregs.x.si		 ;AN000;
;									 ;AN000;
	lahf				; get flags into ax		 ;AN000;
	mov	al,ah			; move into low byte		 ;AN000;
	mov	[di+0ch],ax		; load outregs.x.cflag		 ;AN000;
;									 ;AN000;
	pop	ax			; get di from stack		 ;AN000;
	mov	[di+0ah],ax		; load outregs.x.di		 ;AN000;
;									 ;AN000;
	pop	si			; restore registers		 ;AN000;
	pop	di							 ;AN000;
	mov	sp,bp			; restore sp			 ;AN000;
	pop	bp			; restore user's bp              ;AN000;
	ret								 ;AN000;
_parse	endp								 ;AN000;
_TEXT	ends				; end code segment		 ;AN000;
	end								 ;AN000;
