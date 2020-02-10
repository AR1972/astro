;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This module contains the routine necessary to put up the tsr pop-up dialog ;
; box in case a TSR call was done and wait in a tight loop waiting for the   ;
; user to press a CTRL+C, in the mean time normal context switching can be   ;
; done (if hot keys are enabled).					     ;
;									     ;
; History:								     ;
;									     ;
;	 Fri June-15-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Adapted for the Dos Task Switcher.				     ;
;									     ;
;        Mon July-17-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created for Windows. (Added the History legend) 		     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include	njmp.mac
	include macros.mac
	.list

	.286p

		public	ProcessTsr

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

	assumes	cs,StubSeg
	assumes	ds,StubSeg


;----------------------------------------------------------------------------;
; define the external function calls.		          		     ;
;----------------------------------------------------------------------------;

	;-------------------------------------------------------;
	; external OLDAPP procedures.			        ;
	;-------------------------------------------------------;


;----------------------------------------------------------------------------;
; declare the variables which are defined elsewhere.			     ;
;----------------------------------------------------------------------------;


;----------------------------------------------------------------------------;
; define any locally used variables or messages.			     ;
;----------------------------------------------------------------------------;

	include	woamsg2.inc		;has the pop-up message box

;----------------------------------------------------------------------------;

ProcessTsr  proc near

; home the cursor and pop up the dialog

	mov	ax,1301h		;write string with attribute
	mov	bx,0007h		;page zero, normal attribute
	mov	cx,TSR_Instruct_Length	;length of string
	mov	dx,0200h		;start at line 2/col 0
	pushem	es,bp			;save
	mov	bp,StubSegOFFSET TSR_Instruct
	smov	es,cs			;es:bp has string
	int	10h			;display the string
	popem	es,bp			;restore registers


; EXTRA has some bad pit falls. It often will fail an INT 10 call depending
; on some internal state. This will cause the pop-up prompt not to be displayed
; if extra is run as 'command /c extra'. To take care of this we will try to
; check that the first character of the string is actually put out. If not
; then we will try to redisplay the string.

	mov	ah,02			;set cursor position

; the message started with a carriage return line feed, so read the chatacter
; on the next line.

	mov	dx,0300h		;the start of the string
	xor	bh,bh			;display page
	int	10h			;set back cursor to start of string
	mov	ah,08h			;read character code
	int	10h			;AL has the character
	cmp	al,bptr TSR_Instruct+2	;does it match ?
	jnz	ProcessTsr		;no.
	mov	dx,1900h		;position cursor on line 25
	mov	ah,02			;set cursor code
	int	10h

; now wait in aloop till a CTRL+C is done

TsrWaitLoop:

; Do the Read Using a INT 21 call, do not do INT 16 blocked-read because
; Attachmate's EXTRA parks on INT 16 and if we de-install at that point, it
; goes back to it's INT 16 code after de-installing hooks!! 

; More over we cannot do a block read INT 21 call either, because EXTRA is
; again going to park on this and will come back to their hook even after
; they have been deinstalled. Howver at this point their INT 21 trap code is
; in a free block and if we switch out from here their code will not be saved
; since it's in a free block and then we will die when we return.

; So we will do a get status call which returns CTRL+C too.


	mov	ah,6			;get key status
	mov	dl,0ffh			;keyboard input
	int	21h			;get a key in AL
	jz	TsrWaitLoop		;wait for a character
	cmp	al,03h			;CTRL+C pressed ?
	jnz	TsrWaitLoop		;no, continue waiting

	ret

ProcessTsr  endp
;----------------------------------------------------------------------------;

sEnd	StubSeg

end
	

	
	

