;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This file just defines the temporary 384 byte stack area that WOA uses     ;
; accross the call to the real mode stack. 				     ;
;									     ;
; The size of 384 bytes is kind of arbitrary. A size of 256 was chosen at    ;
; first. However it was found that at a certain point the stack depth was    ;
; 0c9h (201), to be on the safe side the size has now been increased by      ;
; another 128 bytes.							     ;
;									     ;
; Also this defines a dummy labelled byte in all the segments so that we can ;
; calculate the size of the segments.					     ;
;									     ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Wed May-09-1990.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend) 		     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	.list

	.8086				;must have to run on 8086s too.

sBegin	Data

		public	SwitcherStack
		public	SwitcherStackTop
		public	DataSegEnd
		public	CodeSegEnd
		public	StubSegEnd

	;------------------------------------------------;
	; Define the stack area and the top of stack.	 ;
	;------------------------------------------------;

SwitcherStack	db	384 dup (99h)	;stack used accross call to stub

SwitcherStackTop label	byte		;top of stack

		dd	?		;just dummy area

DataSegEnd	label	byte		;end of the segment
		db	99h

sEnd	Data
;----------------------------------------------------------------------------;

sBegin Code

CodeSegEnd	label	byte		;end of code segment
		db	99h

sEnd Code
;----------------------------------------------------------------------------;
createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

StubSegEnd	label	byte		;end of stub seg
		db	99h

sEnd StubSeg
;----------------------------------------------------------------------------;

end
