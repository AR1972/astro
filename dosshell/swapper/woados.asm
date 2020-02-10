;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; This module contains the read key borad buffer routines needed for function;
; 0ah and function 3fh (when the file is a console device)		     ;
;									     ;
; Here are some of the problems and related issues:			     ;
;									     ;
;	  (1) In case the handle points to COMMn, we should ideally like to  ;
;	      trap the call as a wait will be involved. However, there is no ;
;             provision in Dos to do a 'GetStatus' call on a given file      ;
;	      handle.							     ;
;									     ;
;	      In the above case, we will let the call go through to Dos and  ;
;	      will not be able to switch out till the 3FH read on an handle  ;
;	      redirected to COMMn or AUX is satisfied.		             ;
;									     ;
;	  (2) The 3FH call provides a file handle, where as the INT 21H      ;
;	      'GetStatus' calls assume a file handle of 0 implicitly. Thus   ;
;	      we should not use INT 21H 'GetStatus'/'ReadKey (low #s)' calls ;
;	      when trapping a 3FH call. INT 16H would be fine as we would    ;
;	      never get to our trap if the handle does not point to a CON    ;
;	      device.  							     ;
;									     ;
;	  (3) The trap for INT 21H/AH=0AH call would work fine. Here we do   ;
;	      not check to see whether handle 0 has been redirected or not,  ;
;	      but we still work OK because the INT 21H/'GetStatus' and low   ;
;	      numbered 'ReadKey' calls take care of redirection themselves.  ;
;									     ;
; History:								     ;
;									     ;
;	 Thu May-10-1990.	-by-  Amit Chatterjee [amitc]		     ;
;	 Added the 'DoDosWait' routine in place of the 'DoInt16' routine.    ;
;	 This is necessary because the console input might be redirected     ;
;        before an INT 21H/AH=0AH call is made and INT 16H calls do not      ;
;	 support redirection while Dos calls do. (Fix for bug # 1334 (new)   ;
;	 in Windows 3.0).						     ;
;									     ;
;        Tue June-20-1989.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created. (Added the History legend)				     ;
;----------------------------------------------------------------------------;

	?win = 0

	?DF = 1
	.xlist
	include cmacros.inc
	include woasegs.inc
	include macros.mac
	include njmp.mac
	include	woakeys.inc
	.list

	.286

;----------------------------------------------------------------------------;
; declare all public names here.					     ;
;----------------------------------------------------------------------------;

	public	XenixRead
	public	ReadConsoleBuffer
	public	DoDosWait

;----------------------------------------------------------------------------;

createSeg   _WOARLMSEG,StubSeg,word,public,code
sBegin	StubSeg

	assumes	cs,StubSeg
	assumes	ds,Data

;----------------------------------------------------------------------------;
; declare external functions defined elsewhere.				     ;
;----------------------------------------------------------------------------;

	externNP	SwitchIfPossible	;(WOARLM.ASM)
	externNP	DosCall			;(WOARLM.ASM)
	externNP	DoInt28			;(WOARLM.ASM)

;----------------------------------------------------------------------------;
; declare external StubSeg variables that are defined elsewhere		     ;
;----------------------------------------------------------------------------;

externD		ActualInt21   		;original INT 21 vector
externD		ActualInt16		;original INT 16 vector
externB		ConsoleRawMode		;console in raw or cooked mode
externB		WOAsInt16		;chain down our own int 16


;----------------------------------------------------------------------------;
; have any variables needed by the trap routines			     ;
;----------------------------------------------------------------------------;

		public	RCB_FileHandle
		public  b3fRCB
		public	lp3fMode

KeyBoardBuf	label	byte		;place where we will have our traps
		db	256 dup (?)	;max size of buffer for function 0ah
ReadFileBuf	label	byte
		db	128 dup (?)	;max size of buffer for func 3fh
TempReadFileBuf	label	byte
		db	128 dup (?)	;max size of buffer for func 3fh
StartCol	db	?		;start cursor coloumn
InsertMode	db	0		;insert mode indicator (toggle)
LastXenixCount  db	0		;used by Xenix Read,length of last line
XenixBufPos	dw	0		;used by Xenix Read,ptr in last line
b3fRCB		db	0		;ReadConBuffer from 3fH trap or not
RCB_FileHandle	dw	1		;handle of STDOUT (or for 3FH call)
lp3fMode	dd	?		;pointer to mode word in SFT
BufferedExtKey  db	0		;key buffered or not

;----------------------------------------------------------------------------;
; define the two tables for handling the edit characters.		     ;
;----------------------------------------------------------------------------;

; define the allowable edit character codes

EscTable  label byte

	db	ED_F6			;F6
	db	ED_RIGHT_ARROW		;-->
	db	ED_F1			;F1
	db	ED_DEL			;DEL
	db	ED_F2			;F2
	db	ED_F4			;F4
	db	ED_F3			;F3
	db	ED_F5			;F5
	db	ED_LEFT_ARROW		;<--
	db	ED_INS			;INS
	db	ED_F7			;F7
	db	ED_F7			;to terminate table

EscTableLength  equ $-EscTable

; store the offset of the corresponding handler routines in reverse order

EscFunction	label word

	dw	RCB_ReadLoop		;ignore the character
	dw	RCB_TwoEsc		;handles F7
	dw	RCB_Insert		;handles INS
	dw	RCB_BackSpace		;handles left arror
	dw	RCB_ReEdit		;handles F5
	dw	RCB_CopyLine		;handles F3
	dw	RCB_SkipStr		;handles F4
	dw	RCB_CopyStr		;handles F2
	dw	RCB_SkipOne		;handles DEL
	dw	RCB_CopyOne		;handles F1
	dw	RCB_CopyOne		;handles right arrow
	dw	RCB_CtrlZ		;handles F6

;----------------------------------------------------------------------------;
; XenixRead:								     ;
;  									     ;
; This routine reads the console device like a file. It maintains a line     ;
; buffer upto a length of 128 bytes. When the buffer is empty, it fills it   ;
; up by a 'ReadConsoleBuffer' call. It then transfers the user requested #   ;
; of bytes from this buffer. The local buffer stays intact accross call and  ;
; if it is not empty, the requests are satisfied from there.		     ;
;									     ;
; However, if the console is set into raw mode, then just the required # of  ;
; characters are obtained using a direct console input without echo and saved;
; into the buffer.							     ;
;								             ;
;                 ES:DX  -- has user buffer long pointer		     ;
;		     CX  -- # of characters requested			     ;
;----------------------------------------------------------------------------;

XenixRead  proc  near

	assumes	cs,StubSeg
	assumes	ds,StubSeg
	assumes es,nothing

	cmp	ConsoleRawMode,0ffh	;is the console in raw mode ?
	jnz	@f			;no, do a buffered read

; console is in raw mode, do a raw read

	call	RawRead			;do a raw read
	jmp	short XenixReadRet	;return back

@@:
	mov	di,dx			;ES:DI -> buffer
	push	dx			;Save start of buffer to compute count
	mov	si,XenixBufPos		;# of characters left in buffer
	or	si,si			;Buffer left from last call?
	jnz	XenixXferFromBuffer	;try to satisfy the req. from the buffer

; XenixBuffer is empty, or we have taken up all characters that it had. In any 
; case we need to get a new line from the console. However, if there is a buffer
; then it will serve as the editing template.					


	pushem	es,di,cx	       	;save the trasfer address
	smov	es,ds			;es is stubseg, which has local buffer
	mov	cx,128			;Xenix line buffer size is atbest 128
	xchg	ch,LastXenixCount	;get former length of buffer

; if we do have a buffer left from a previous call, that will serve as the
; template. Note that the 0aH and 3fh Dos calls will have different buffers.

	mov	dx,StubSegOFFSET TempReadFileBuf
	call	ReadConsoleBuffer	;get a new line.
	mov	XenixBufPos,0		;current position in buffer

; the local buffer possibly has a new line now.

	popem	es,di,cx		;get back lptr to users buffer
	mov	si,StubSegOFFSET ReadFileBuf
	or	al,al			;Empty?
	jz	XenixXferFromBuffer	;Yes, LastXenixCount left at 0
	dec	al			;Don't include CR

; set the length of the template for the next call to be the length of the 
; line just read.

	mov	[LastXenixCount],al	;Length of next template for next call

XenixXferFromBuffer:

	cmp	byte ptr [si],1ah	;^Z?
	jnz	XenixTransferLoop	;no, trasfer the characters
	mov	byte ptr es:[di],1ah	;^Z to output buffer (no inc of DI)
	jmp	short XenixReadDone	;all done, finish up

; transfer characters from local to the users buffer

XenixTransferLoop:

	lodsb				;load the next char from local buffer
	stosb				;place it in users buffer
	cmp	al,0dh			;dis we just transfer a CR ?
	jnz	@f			;No
	mov	bptr [si],0ah		;insert LF after CR to simulate file
@@:
	cmp	al,0ah			;was the last char a LF ?
	loopnz	XenixTRansferLoop	;Continue while chars and NOT LF
	jnz	@f			;Stopped due to CX = 0, still have buffer

; data transfer completed

XenixReadDone:

	mov	al,0ah			;echo LF to stdout
	call	OutChar			;display the character
	xor	si,si			;nothing in internal buffer
@@:
	mov	[XenixBufPos],si	;Set internal buffer ptr for next call
	pop	dx			;Recover start of xfer buffer
	mov	ax,di			;posn of last char in users buffer
	sub	ax,dx			;diff is count of chars transfered

XenixReadRet:

	clc				;show success
	ret

XenixRead  endp
;----------------------------------------------------------------------------;
; RawRead:								     ;
;									     ;
; This routine waits for keystrokes, and once it gets them it filles them up ;
; in the user provided buffer at es:dx. It will fill CX number of characters ;
; into the buffer. While it is waiting for characters, it tests to see if a  ;
; context switch can be done or not. (characters are not echoed by this call);
;									     ;
; As noted in the header, we must do INT 16 get-status and read calls.	     ;
;----------------------------------------------------------------------------;

RawRead	proc near

	mov	ax,cx			;set up return value.
	mov	di,dx			;get ES:DI to point to the buffer
	push	ax			;save value

RawReadLoop:

RawReadFromKeyBoard:

	call	GetInt16KeyFromDos	;get a character in AL

RawKeyInAl:

	stosb				;save it in the buffer
	loop	RawReadLoop		;read requested number of characters

	pop	ax			;get back no of characters read
	ret

RawRead endp
;----------------------------------------------------------------------------;
; ReadConsoleBuffer:							     ;
;									     ;
; This routine is called to fill a buffer with a line obtained from the cons-;
; -ole. It allows full editing support exactly like dos, based on a template ;
; which is nothing but the buffers previous contents.			     ;
;									     ;
; Dos Functions 0ah and 3fh use this call to read the next line from console ;
;									     ;
; On Entry:								     ;
;	     ES:DX  --  users buffer past the 1st two bytes		     ;
;		CL  --  user buffer size.				     ;
;		CH  --  number of characters already in buffer (if the user  ;
;			had done the same call earlier on the same buffer).  ;
;----------------------------------------------------------------------------;

ReadConsoleBuffer proc near

	assumes	cs,StubSeg
	assumes	ds,StubSeg
	assumes	es,nothing

	mov	di,dx			;this will be our work pointer
	mov	si,dx			;si will point to the start
	mov	ax,cx			;al is buffer length, ah template length
	xor	ch,ch			;cx will hold final length of input
	
; we are done if a zero length buffer is specified.

	or	al,al			;test length of buffer
	njz	ReadConsoleBufferRet	;return back

; in BL we will hold position in buffer and BH will have previous template 
; length.

	xor	bx,bx
	mov	bl,ah			;bl = template length, current pos=bh=0

;----------------------------------------------------------------------------;
; this call allows the user to edit the previous buffer. We allow edit only  ;
; when we have a valid prior template. Two conditions must be met:	     ;
;	1. length of buffer specified must not be less than or equal to the  ;
;	   template length, and						     ;
;	2. there must be a carriage return at the end of the template	     ;
;----------------------------------------------------------------------------;

	cmp	al,bl			;check length consistency
	jbe	ReadConsoleNoEdit	;editing functions not allowed
	cmp	bptr es:[bx+si],0dh	;carriage return at end ?
	jz	@f			;template is valid, it's length in BL

ReadConsoleNoEdit:
	xor	bx,bx			;throw away previous template length
@@:
	mov	dl,al			;size of buffer
	dec	dl			;no of characters we can put

;----------------------------------------------------------------------------;
; we must get the exact cursor position on the screen for the editing, do an ;
; int 10h call to get the current cursor position.			     ;
;----------------------------------------------------------------------------;

	pushem	bx,cx,dx		;save relevant registers
	mov	ah,15			;get active page call
	int	10h			;bh has current page
	mov	ah,3			;read cursor position call
	int	10h			;cursor position in dl
	mov	StartCol,dl		;save it
	popem	bx,cx,dx		;restore the saved registers

RCB_StartNewLine:

; do initializations

	push	si			;save start of user buffer
	mov	di,StubSegOFFSET KeyBoardBuf
	cmp	b3fRCB,0		;is it from a 3FH trap ?
	jz	RCB_SNL_DI_OK		;no, DI is correct
	mov	di,StubSegOFFSET ReadFileBuf

RCB_SNL_DI_OK:

	mov	InsertMode,0		;reset insert mode toggle
 	mov	bh,0			;no characters from template yet
	mov	dh,0			;# of characters in buffer
	
	call	GetChar			;get the first character
	cmp	al,0ah			;LF ?
	jnz	RCB_CharacterInAL	;no!, a valid character

; we will filter out the first LF, so that inut redirection will work.

;----------------------------------------------------------------------------;
; The main read loop starts now. We read and process characters. The charact-;
; -ers could be normal characters which go into the buffer, normal edit char-;
; -cters like backspace,etc., or they could be 2 byte template edit characte-;
; -rs. We get out of the loop,once we read a CR or we reach the end of the   ;
; buffer.								     ;
;----------------------------------------------------------------------------;

RCB_ReadLoop:

	call	GetChar			;get the next character

RCB_CharacterInAL:

; take action depending on the type of the character.

	cmp	al,"F"-"@"		;^F ?
	jz	RCB_ReadLoop		;ignore ^F
	cmp	al,ED_STARTEDIT		;start of EDIT cgaracter
	jz	RCB_ProcessEdit		;yes, process the next edit key
	cmp	al,127			;DEL key ?
	njz	RCB_BackSpace		;treat as backspace character
	cmp	al,08			;backspace character
	njz	RCB_BackSpace		;process backspace
	cmp	al,0dh			;CR?
	jz	RCB_EndLine		;Yes, finished
	cmp	al,0ah			;LF?
	jz	RCB_NewLine 		;Yes, new line and keep reading
	cmp	al,ED_CANCELEDIT	;CANCEL? (Esc for IBM)
	jz	RCB_KillNew		;yes, kill line and start over

; we have a normal character to stuff into the buffer.

RCB_SaveChar:

	cmp	dh,dl			;Room for char?
	jae	RCB_BufFul		;no room, buffer is full
	mov	[di],al 		;Char into buffer
	inc	di			;update buffer pointer
	inc	dh			;got a char
	call	OutAnyChar		;ouput the character

; one character processed.

	cmp	InsertMode,0		;are we insert mode ?

; if insert mode is on, get more characters but do not advance the template

	jnz	RCB_ReadLoop		;read more, insert is on.

; advance template

	cmp	bh,bl			;Past end of template or no template ?
	jae	RCB_ReadLoop        	;yes, we are out of chars in template
	inc	si		   	;skip to next char in template
	inc	bh			;one more character seen in template
	jmp	short RCB_ReadLoop	;continue till EOB or CR

ReadConsoleBufferRet:

	mov	ax,cx			;get return count in AX
	clc				;for success
	ret

ReadConsoleBuffer endp
;----------------------------------------------------------------------------;
; 		ROUTINES NEEDED BY READ CONSOLE BUFFER CODE		     ;
;----------------------------------------------------------------------------;

;----------------------------------------------------------------------------;
; When the end of the buffer is reached, sound the beep and continue looking ;
; for CR.								     ;
;----------------------------------------------------------------------------;

RCB_BufFul:

	mov	al,7			;code for bell
	call	OutChar			;output character in AL
	jmp	short RCB_ReadLoop	;Keep looking for CR

;----------------------------------------------------------------------------;
; We hev obtained the first character of a two character escape sequence.    ;
; We get the second character and process the chracter based on a table of   ;
; routines indexed by the second character.			             ;
;----------------------------------------------------------------------------;
	
RCB_ProcessEdit:

	push	di			;save DI (cannot change it!)
	push	es			;save users segment
	call	GetExtChar		;get the key
	smov	es,cs			;load segment for jump table
	mov	cx,EscTableLength	;length of table for scan
	mov	di,StubSegOFFSET EscTable;offset of second byte table
	repnz	scasb			;Look it up in the table
	shl	cx,1			;convert byte offset to word
	mov	bp,cx			;move to BP to index function table
	pop	es			;get back users segment
	pop	di			;restore DI
	jmp	cs:[bp+EscFunction] 	;Go to the right routine

;----------------------------------------------------------------------------;
; Process the carriage return character.				     ;
;----------------------------------------------------------------------------;

RCB_EndLine:

	mov	[di],al 		;put the cr in the buffer
	call	OutChar			;echo it
	pop	di		        ;get start of user buffer
	inc	dh      		;dh is length including CR

; fall into the re-edit command.

;----------------------------------------------------------------------------;
; RCB_CopyNew is used when the reedit command is detected		     ;
;----------------------------------------------------------------------------;

RCB_CopyNew:

	mov	si,StubSegOFFSET KeyBoardBuf;DS:SI -> local buffer
	cmp	b3fRCB,0		;is it from a 3FH trap ?
	jz	RCB_CN_SI_OK		;no, SI is correct
	mov	si,StubSegOFFSET ReadFileBuf

RCB_CN_SI_OK:

	mov	cl,dh			;get number of characters including CR
	push	cx			;save count for return
	rep	movsb			;copy final line to user buffer
	pop	ax			;get back count in ax for return
	ret				;all done

;----------------------------------------------------------------------------;
; This routine outputs a CR,LF pair onto the screen.			     ;
;----------------------------------------------------------------------------;

RCB_OutCRLF:

	mov	al,0dh			;code for CR
	call	OutChar			;output CR
	mov	al,0ah			;code for LF
	jmp	OutChar			;output and return to caller

;----------------------------------------------------------------------------;
; this code is executed when we get an LF, it outputs a CR,LF pair and goes  ;
; back to the main read loop.						     ;
;----------------------------------------------------------------------------;

RCB_NewLine:

	call	RCB_OutCRLF    		;go to next line
	jmp	RCB_ReadLoop		;go back to main read loop

;----------------------------------------------------------------------------;
; This function	flushes the inputs obtained till now but retains the template;
;----------------------------------------------------------------------------;

RCB_KillNew:

	mov	al,'\'			;code for cancel indicator
	call	OutChar		    	;print the cancel indicator
	pop	si			;remember start of edit buffer

; fall into the putnew code.

;----------------------------------------------------------------------------;
; following code fragment is used by the reedit command			     ;
;----------------------------------------------------------------------------;

RCB_PutNew:

	call	RCB_OutCRLF		;go to next line on screen
	pushem	ax,bx,cx		;save important registers
	xor	cx,cx
	mov	cl,StartCol		;get the start coloumn at command start

; we must outout a series of spaces to get to the correct starting coloumn

RCB_OutSpaces:
	jcxz	RCB_OutSpacesDone      	;we are now at correct coloumn
	mov	al, 20h			;code for space character

RCB_OutSpacesLoop:
	call	OutChar			;output the next space
	loop	RCB_OutSpacesLoop	;till we reach correct cursor position

RCB_OutSpacesDone:
	popem	ax,bx,cx		;restore registers
	jmp	RCB_StartNewLine	;start all over again

;----------------------------------------------------------------------------;
; process BACKSPACE.							     ;
;----------------------------------------------------------------------------;

RCB_BackSpace:

	or	dh,dh			;test no of characters in line
	jz	RCB_BackUpDone		;no chars in line, do nothing to line
	call	RCB_BackUp    		;do one char backup
	mov	al,[di] 		;get the deleted char
	cmp	al,' '			;test for characters below space
	jae	RCB_BackUpDone		;was a normal char
	cmp	al,9			;was it a tab ?
	jz	RCB_BackTab		;was a tab, fix up users display
	call	RCB_BackOnScreen 	;was a control char, zap the '^'

RCB_BackUpDone:

	cmp	InsertMode,0		;insert on or off ?
	njnz	RCB_ReadLoop        	;in insert mode, get more chars
	or	bh,bh	    		;test for position in template
	njz	RCB_ReadLoop		;cant go back, we are at start
	dec	bh			;go back in template ,dec count
	dec	si			;decrement pointer in template
	jmp	RCB_ReadLoop		;continue in main read loop

;----------------------------------------------------------------------------;
; This fragment backs up over a tab, with the first character already beacked;
; over.									     ;
;----------------------------------------------------------------------------;

RCB_BackTab:

	push	di			;save poistion in local buffer
	dec	di			;back up one char
	mov	cl,dh			;number of chars currently in line
	mov	al,' '			;we look for spaces
	push	bx			;save template context
	mov	bl,7			;at best we back over 7 more spaces
	jcxz	RCB_BackTabDone		;at start, do nothing

RCB_FindPos:

	cmp	al,[di] 		;look back
	jna	RCB_CheckCount		;test the count of look back
	cmp	byte ptr [di],9		;is it a tab character
	jz	RCB_HaveTab		;found a tab
	dec	bl			;back one char if non tab control char

RCB_CheckCount:

	dec	di		  	;go back one more
	loop	RCB_FindPos		;loop till satisfied

RCB_BackTabDone:

	sub	bl,[StartCol]		;subtract starting coloumn

RCB_HaveTab:

	sub	bl,dh		  	;subtract no of characters
	add	cl,bl			;add no of look backs we did
	and	cl,7			;cx has correct number to erase
	cld				;back to normal
	pop	bx			;restore template context
	pop	di			;restore position in local buffer
	jz	RCB_BackUpDone		;nothing to erase

RCB_TabBack:

	call	RCB_BackOnScreen	;back over a character
	loop	RCB_TabBack		;erase correct number of chars
	jmp	short RCB_BackUpDone	;done.

;----------------------------------------------------------------------------;
; This code fragment backs over a character on the screen as well as in the  ;
; local buffer. The second entry point backs a character just on the screen, ;
; not disturbing the buffers.						     ;
;----------------------------------------------------------------------------;

RCB_BackUp:

	dec	dh			;decrement count of characters
	dec	di			;backup buffer pointer

RCB_BackOnScreen:

	mov	al,8			;code for back space
	call	OutChar			;go back one coloumn
	mov	al,' '   	       	;space character
	call	OutChar			;space out character to erase it
	mov	al,8			;backspace again
	jmp	OutChar			;character erased, cursor ok,jmp rets

;----------------------------------------------------------------------------;
; this code fragment is hit when an escape character is actually to be put in;
; the buffer.								     ;
;----------------------------------------------------------------------------;

RCB_TwoEsc:

	mov	al,ED_STARTEDIT		;code to save for escape
	jmp	RCB_SaveChar		;save the character and proceed

;----------------------------------------------------------------------------;
; the code fragment used to copy the rest of the remplate.		     ;
;----------------------------------------------------------------------------;

RCB_CopyLine:

	mov	cl,bl			;total size of template
	sub	cl,bh			;minus position in template= copy count
	jmp	short RCB_CopyEach	;does the copy based on count in CX

;----------------------------------------------------------------------------;
; copies the template till the indicated character.			     ;
;----------------------------------------------------------------------------;

RCB_CopyStr:

	call	RCB_FindTemplateChar   	;find the char,returns distance in CX
	jmp	short RCB_CopyEach	;copy up to it

;----------------------------------------------------------------------------;
; Copies characters from the templates based on count. The first entry point ;
; copies one character, the next on expects a count in CX.		     ;
;----------------------------------------------------------------------------;

RCB_CopyOne:

	mov	cl,1			;need to copy 1 character

RCB_CopyEach:

	mov	InsertMode,0		;all copies turn off insert mode
	cmp	dh,dl			;test for end of buffer
	jz	@f			;at end of buffer, can't do anything
	cmp	bh,bl			;compares distance to template end
	jz	@f			;at end of template, can't do anything
	lods	byte ptr es:[si]	;Get byte from template
	mov	[di],al 		;Save in buffer
	inc	di			;Next char in line
	call	OutAnyChar		;Print char
	inc	bh			;ahead in template
	inc	dh			;ahead in line
	loop	RCB_CopyEach		;untill all characters are copied
@@:
	jmp	RCB_ReadLoop		;continue reading from keyboard

;----------------------------------------------------------------------------;
; skips one character in the template.					     ;
;----------------------------------------------------------------------------;

RCB_SkipOne:

	cmp	bh,bl			;compare distance to end of template
	jz	@f			;at end of template,cannot skip
	inc	bh			;ahead in template
	inc	si			;bump pointer in template
@@:
	jmp	RCB_ReadLoop		;continue in main loop

;----------------------------------------------------------------------------;
; skips to the indicated character in the template.			     ;
;----------------------------------------------------------------------------;

RCB_SkipStr:

	call	RCB_FindTemplateChar 	;find out how far to go
	add	si,cx			;go there bumping pointer in template
	add	bh,cl			;add count of character seen in templ
	jmp	RCB_ReadLoop		;get back tomain loop

;----------------------------------------------------------------------------;
; This routine gets the next character from the keyboard and looks ahead in  ;
; the template for a match, leaving in CX the number of characters toge there;
;									     ;
; If the operation cannot be done, the return address is pooped off the stack;
; and a direct jump is done to get back to the main read loop. So the caller ;
; must not push ANYTHING onto the stack before calling this code.	     ;
;----------------------------------------------------------------------------;

RCB_FindTemplateChar:

	call	GetChar			;Get the char to look for
	cmp	al,ED_STARTEDIT		;did he type a function key?
	jnz	@f			;no, set up for scan
	call	GetChar			;eat next char
	jmp	short RCB_NotFound	;go try again

@@:

	mov	cl,bl			;get number of characters in template
	sub	cl,bh	   		;cx is number of chars to end of templ
	jz	RCB_NotFound		;at end of template
	dec	cx	      		;cannot point past end, limit search
	jz	RCB_NotFound		;if only one char in template,forget it
	push	di			;save poistion in buffer
	mov	di,si	        	;template to es:di
	inc	di			;next character
	repne	scasb	        	;look
	pop	di			;get back pointer in buffer
	jnz     RCB_NotFound		;didn't find the char
	not	cl			;turn how far to go into how far we went
	add	cl,bl			;add size of template
	sub	cl,bh			;sub cur pos, result distance to skip
	ret

RCB_NotFound:
	pop	bp			;chuck return address
	jmp	RCB_ReadLoop		;go back to main loop

;----------------------------------------------------------------------------;
; this code discards the current template and makes line typed till now the  ;
; new template.								     ;
;----------------------------------------------------------------------------;

RCB_ReEdit:
	mov	al,'@'          	;output re-edit character
	call	OutChar			;display the character
	pop	di			;Recover addr of start of template buffer
	push	di			;save it
	call	RCB_CopyNew		;copy current line into template
	pop	si			;get back template start in si
	mov	bl,dh			;size of line is new size template
	jmp	RCB_PutNew		;start over again

;----------------------------------------------------------------------------;
; this code fragment, toggels the insertmode flag.			     ;
;----------------------------------------------------------------------------;

RCB_Insert:

	not	InsertMode		;toggle the flag
	jmp	RCB_ReadLoop		;go back into the main read loop

;----------------------------------------------------------------------------;
; puts a real live ^z in the buffer (embedded)				     ;
;----------------------------------------------------------------------------;

RCB_CtrlZ:

	mov	al,'Z'-'@'		;code for ^z
	jmp	RCB_SaveChar		;save in buffer and proceed

;----------------------------------------------------------------------------;
; 'OutAnyChar  prints out ctrl characters in a printable format, it jmps     ;
; 'OutChar' if the character is not a control character or if it is a tab    ;
;----------------------------------------------------------------------------;

OutAnyChar:

; filter out non control characters and tabl

	cmp	al,9			;Tab is not converted to ^char
	jz	OutChar			;print normal
	cmp	al,' '                  ;Chars above and = to space are normal
	jae	OutChar			;print normal characters
	push	ax			;Save char
	mov	al,'^'          	;Print ^
	call	OutChar			;print the character
	pop	ax			;get back control code
	add	al,'@'          	;Make char printable and print it
	
; all into the normal print code

	errn$	OutChar	
;----------------------------------------------------------------------------;
; this code prints out non-control characters and tabs (which is expanded)   ;
; we will use a 40H/INT 21H call to do the output. If this is being called   ;
; while we are in the middle of a INT 21/0AH call, RCB_FileHandle will be 1  ;
; (stdout). If we are trapping a 3FH call, RCB_FileHandle would be the handle;
; that 3FH used, we have already made sure that the handle has write access. ;
;									     ;
; Also, if the call is from a 3FH call, we will change the mode byte in the  ;
; SFT entry for the file to grant temporary write permision. We will restore ;
; the permision later.							     ;
;----------------------------------------------------------------------------;

cProc	OutChar,<NEAR,PUBLIC,PASCAL>,<ds,dx,ax,bx,cx,es,di>

	localB	OutChar_Byte		;byte to output
	localB	OutChar_Mode		;original SFT mode

cBegin

	mov	OutChar_Byte,al		;save the byte to output
	mov	bx,RCB_FileHandle	;handle to be used for write call
	lea	dx,OutChar_Byte		;ss:dx -> byte to output
	cmp	b3fRCB,0		;trapping a 3FH call ?
	jz	OutChar0AH		;no, trapping a 0AH call

; grant temporary write permison to the SFT

	les	di,lp3fMode		;es:di -> mode byte in SFT
	mov	al,es:[di]		;get the mode
	mov	OutChar_Mode,al		;save it
	and	al,80h			;reser mode/access bits
	or	al,42h			;deny-none/read-write access
	mov	es:[di],al		;save mode

; now write the byte

	smov	ds,ss			;ds:dx -> byte to output
	mov	cx,1			;1 character to write
	mov	ah,40h			;write code
	call	DosCall			;output the character

; finally restore the SFT mode.

	mov	al,OutChar_Mode		;get saved value
	mov	es:[di],al		;restore SFT mode
	jmp	short OutCharRet	;done.

OutChar0Ah:

	smov	ds,ss			;ds:dx -> byte to output
	mov	cx,1			;1 character to write
	mov	ah,40h			;write code
	call	DosCall			;output the character

OutCharRet:

cEnd
;----------------------------------------------------------------------------;
; this routine gets the next character typed in at the keyboard. It does a   ;
; get status call waiting for a character to get ready, and as it waits, it  ;
; tests to see if a context switch is possible and if it is, it would do the ;
; context switching.							     ;
;									     ;
; If the call has come from a 0AH trap, we should do INT 21 GetStatus/GetKey ;
; calls, else if the call has come from a 3FH trap we should use INT 16H to  ;
; do the 'GetStatus'/'ReadKey' calls. A flag would be set if the trap is from;
; a 3FH call.								     ;
;----------------------------------------------------------------------------;

GetChar	proc near

	cmp	b3FRCB,0		;is it a 3FH call ReadConBuffer ?
	jz	GetChar0ACall		;no,
	call	GetInt16KeyFromDos	;use INT 16H calls.
	jmp	short GetCharRet	;return.

GetChar0ACall:

	call	DoInt28			;yield, not if in crit. error handler
	call	SwitchIfPossible	;do a context switch if appropriate
	call	DoDosWait		;wait for key
	jz	GetChar			;wait till key ready
	mov	ah,8			;get the code int 21h call
	call	DosCall			;get the character

; the key is ready, return back	with code in AL

GetCharRet:

	ret

GetChar endp
;----------------------------------------------------------------------------;
; GetInt16KeyFromDos:							     ;
;									     ;
; This is called while we are trapping the 3FH dos call. As noted in the     ;
; header of the file, we will have to do INT 16H status/get key calls.       ;
; When a key code is returned as 00h, we will save the scan code and return  ;
; the scan code on the next invocation of this call.			     ;
;									     ;
; Note: some applications like EXTRA do their own INT 9/INT 16 handle without;
; chaining them on. For this reason we will do a regular INT 16H rather than ;
; trying to call down stream.						     ;
;									     ;
; This code has a special hack in it! This routine is called from the code   ;
; that traps the INT 21H/AH=3Fh call. We do INT 16H calls to get a key rather;
; than INT 21H calls to care of problems with redirection. However, this     ;
; means that we will not be processing CTRL+C and CTRL+BREAK keys correctly. ;
; To take care of this problem, we now do a dummy INT 21H get keyboard status;
; once a key becomes ready.						     ;
;----------------------------------------------------------------------------;
GetInt16KeyFromDos proc near

	cmp	BufferedExtKey,0	;is there a buffered key ?
	jz	GI16KFD_WaitLoop	;no.
	mov	al,BufferedExtKey	;get the saved key
	mov	BufferedExtKey,0	;no more in the buffer
	jmp	short GI16KFD_Ret	;ret

GI16KFD_WaitLoop:

	call	DoInt28			;yield, not if in crit. error handler

; do a INT 16H, set a flag so that our own hook will let it go and also will
; try the 'SwitchIfPossible' call.

	mov	cs:[WOAsInt16],0ffh	;our own INT 16
	mov	ah,01h			;get status call
	int	16h			;do the call
	mov	cs:[WOAsInt16],0	;reset the flag

; test for a key ready.

	jz	GI16KFD_WaitLoop	;no.

; As explained in the header, do an INT 21H get status call to give DOS a 
; chance to handle CTRL+C and CTRL+BREAK keys. 

	mov	ah,0bh			;get status key
	call	DosCall			;ignore results

; do a INT 16H, set a flag so that our own hook will let it go and also will
; try the 'SwitchIfPossible' call.

	mov	cs:[WOAsInt16],0ffh	;our own INT 16
	mov	ah,0			;get the key
	int	16h			;do the call
	mov	cs:[WOAsInt16],0	;reset the flag
	or	al,al			;is it an extended key
	jnz	GI16KFD_Ret		;no.
	mov	BufferedExtKey,ah	;save the scan code of ext key

GI16KFD_Ret:

	ret

GetInt16KeyFromDos  endp
;----------------------------------------------------------------------------;
; GetExtChar:								     ;
;									     ;
; This gets the second character of an extended key stroke. If we are in the ;
; middle of a 3FH trap, the character would already have been saved else     ;
; we must do a AH=08H dos call to get it.				     ;
;----------------------------------------------------------------------------;
GetExtChar proc near

	cmp	b3fRCB,0		;from a #FH trap ?
	jz	@f			;no.
	call	GetInt16KeyFromDos	;will get the buffered key
	ret
@@:
	mov	ah,08h			;get char
	call	DosCall			;get the char
	ret

GetExtChar endp
;----------------------------------------------------------------------------;
; DoDosWait:								     ;
;									     ;
; Does a check keyboard status call to see whether a key is ready or not.    ;
; Zero flag is set if no key is available. This routine does INT 21H call    ;
; rather than INT 16H so that redirection of console will be taken care of.  ;
;----------------------------------------------------------------------------;

DoDosWait proc near

	push	ax			;save
	mov	ah,0bh			;check keyboard status call
	call	DosCall			;call to dos direct
	or	al,al			;set/reset zero flag
	pop	ax			;restore
	ret

DoDosWait endp
;----------------------------------------------------------------------------;

sEnd	StubSeg

end
