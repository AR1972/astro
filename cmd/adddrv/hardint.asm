title	HARDWARE INTERRAPT DISABLE/ENABLE
name	hardint
page	53, 132

;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

;***********************************************************************
;	HARDINT.ASM
;
;		Hardware interrapt disable and enable function.
;
;		_intdisable
;			hardware interrapt disable.
;
;		_intenable
;			hardware interrapt enable.
;
;-----------------------------------------------------------------------
;	Modification history
;-----------------------------------------------------------------------
;	MSKK00	July 20, 1987	akitok
;		Make out.
;-----------------------------------------------------------------------


	.xlist
include	cmacros.inc
	.list




sBegin	code
	assumes	cs,code


;***********************************************************************
;	void intdisable(void);
;		disable hardware interrapt.
;
;		input	:
;			  none.
;
;		output	:
;			  none.
;
;***********************************************************************


cProc	intdisable, <PUBLIC>

cBegin
	cli			; interrapt disable.
cEnd


;***********************************************************************
;	void intenable(void);
;		enable hardware interrapt.
;
;		input	:
;			  none.
;
;		output	:
;			  none.
;
;***********************************************************************


cProc	intenable, <PUBLIC>

cBegin
	sti			; interrapt enable.
cEnd


sEnd	code

	end
