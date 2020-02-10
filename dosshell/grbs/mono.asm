;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */

;----------------------------------------------------------------------------;
; This has all the code needed for a very simple grabber to work on machines ;
; for which we do not have a regular grabber. For this to work the display   ;
; should always be in a text mode. All the assumptions/restrictions that are ;
; assumed are:								     ;
;									     ;
;            . Will work for mode 3 and mode 7 only			     ;
;	     . Display buffer is at 0B800H for mode 3 and 0B000H for mode 7  ;
;	     . It is OK to access the display buffer directly		     ;
;	     . Only the following 4 calls are supported:		     ;
;		     . InitScreen					     ;
;		     . InquireSave					     ;
;		     . SaveScreen					     ;
;		     . RestoreScreen					     ;
;		All other calls are stubbed out and return with carry flag   ;
;		set.							     ;
;	     . Saves and restores only the active page.			     ;
;	     . Assumes 80x25 screen resolution indirectly (page size of 4K is;
;	       assumed).						     ;
;									     ;
;        Wed Sept-11-1990.  	-by-  Amit Chatterjee [amitc]		     ;
;        Created. (Added the History legend)				     ;
;----------------------------------------------------------------------------;


_TEXT		segment byte	public	'CODE'
		assume	cs:_TEXT


;
; STANDARD FUNCTION DISPATCH TABLE
;
;	Since the grabbers are loaded by Winoldap as a binary image, Winoldap
;	transfers control to them via a jump table at offset 0 of the grabber
;	code segment. The format of this table MUST remain fixed and MUST 
;       reside at offset 0!
;
;	Winoldap computes the offset of the desired entrypoint using the
;	knowledge that a near jump is 3 bytes of opcode.  However, as Winoldap
;	will be making a far call to the jmp, we MUST return far even though
;	our functions are near.  Winoldap must always set ds equal to the
;	grabber's cs before making the jump.
;
		org	0

StdFuncTable	label	word
		jmp	ObsoleteFunction	;NOP
		jmp	ObsoleteFunction	;NOP
		jmp	ObsoleteFunction	;NOP
		jmp	ObsoleteFunction	;NOP
		jmp	InquireSave		;Func 00005h
		jmp	SaveScreen		;Func 00006h
		jmp	RestoreScreen		;Func 00007h
		jmp	InitScreen		;Func 00008h


;----------------------------------------------------------------------------;
; declare global variables.						     ;
;----------------------------------------------------------------------------;

BufSize		dw	?		;size of save buffer
AppsMode	db	?		;mode at switch time
AppsPage	db	?		;page at switch time
AppsTopCurLine	db	?		;start line of cursor
AppsBotCurLine	db	?		;end line of cursor
AppsRow		db	?		;current row
AppsCol		db	?		;current coloumn

;----------------------------------------------------------------------------;
; constants.								     ;
;----------------------------------------------------------------------------;

PAGE_SIZE	equ	4096
SEG_MONO	equ	0b000h
SEG_COLOR	equ	0b800h
MODE_COLOR	equ	3
MODE_MONO	equ	7

; more constants for the beep routine

; Define IBM PC port locations for the Intel 8243 Programmable Interval Timer
; chip

PIT_PORTA	=	040h
PIT_PORTB	=	041h
PIT_PORTC	=	042h
PIT_PORTD	=	043h

; Define IBM PC port locations for the Intel 8255 Programmable Peripheral
; Interface chip

PPI_PORTA	=	060h
PPI_PORTB	=	061h
PPI_PORTC	=	062h
PPI_PORTD	=	063h

; Define timimgs and divisors to make sounds via the PIT, PPI chips and the
; PC's speaker circuitry.

BEEP_TIME1	=	02400h
BEEP_TIME2	=	03400h
BEEP_TONE1	=	00533h
BEEP_TONE2	=	00633h

;----------------------------------------------------------------------------;
; InitScreen - initialize screen to a known mode for the oldap		     ;
;									     ;
;	This routine will be called by Winoldap once before the oldap starts ;
;	up.  Winoldap will also call this routine just before it returns     ;
;	control to the oldap after a context switch from Windows back to the ;
;	oldap.	The latter of these two cases is redundent,since the grabbers;
;	RestoreScreen routine will be called shortly thereafter torestore the;
;	application's display context.  However, since we cannot discernthese;
;	two cases, we always set the display to a known mode regardless.     ;
;									     ;
; ENTRY									     ;
;	ds	=  cs							     ;
;	ax 	=  number of lines perscreen. If >= 43, 43 lime mode is	     ;
;		   set, else 25 line mode is set.			     ;
; EXIT									     ;
;	none								     ;
; USES									     ;
;	none								     ;
;									     ;
; NOTES:								     ;
;	 Every thing is ignored and the current mode is set again just to    ;
;	 clear the screen.						     ;	
;----------------------------------------------------------------------------;
InitScreen	proc	far

		assume	ds:_TEXT
		mov	ah,0fh			;get mode call
		int	10h			;al=mode
		xor	ah,ah			;set mode call
		int	10h			;screen cleared
		ret

InitScreen	endp
;----------------------------------------------------------------------------;
;									     ;
; InquireSave - return size of screen save buffer needed		     ;
;									     ;
; ENTRY									     ;
;	ds	=  cs							     ;
;	ax	=  n							     ;
;									     ;
;	where n is either:						     ;
;		1	Inquire text save buffer size			     ;
;		2	Inquire graphics save buffer size		     ;
; EXIT									     ;
;	dx:ax	=  size in bytes for save buffer			     ;
; USES									     ;
;	ax, dx, flags							     ;
;----------------------------------------------------------------------------;
InquireSave	proc	far

		assume	ds:_TEXT
		mov	ax,PAGE_SIZE		;size of one page
		mov	BufSize,ax		;save it
		xor	dx,dx			;dx:ax has size
		ret
InquireSave	endp
;----------------------------------------------------------------------------;
; SaveScreen - save the current display context				     ;
;									     ;
; ENTRY									     ;
;	ax	=  size in bytes of screen save area			     ;
;	ds	=  cs							     ;
;	es:di	-> screen save area					     ;
; EXIT									     ;
;	cf	=  0	(screen was successfully saved)			     ;
; ERROR EXIT								     ;
;	cf	=  1	(unable to save screen)				     ;
; USES									     ;
;	all except bp							     ;
; NOTES									     ;
;	Winoldap guarantees that the offset portion of the screen save area  ;
;	will always be zero, so it is safe to omit di from references to this;
;	area.								     ;
;----------------------------------------------------------------------------;

SaveScreen	proc	far
		assume	ds:_TEXT

; check to see if buffer size is OK

	cmp	ax,BufSize		;enough ?
	jb	SaveScreenErr		;no.

; save the current mode, page, cursor position.

	mov	ah,0fh			;get mode
	int	10h			
	mov	AppsMode,al		;save mode
	mov	AppsPage,bh		;save active page

; if the mode is not a text mode we must fail the call.

	cmp	al,MODE_COLOR		;color text mode ?
	jz	SS_ModeOk		;yes.
	cmp	al,MODE_MONO		;mono text mode ?
	jnz	SaveScreenErr		;no, fail the call

SS_ModeOk:

	mov	ah,3			;cursor details
	int	10h
	mov	AppsRow,dh		;cursor row
	mov	AppsCol,dl		;apps column
	mov	AppsTopCurLine,ch	;top cursor line
	mov	AppsBotCurLine,cl	;bottom cursor line

; set up start screen address.

	push	ds
	mov	ax,SEG_COLOR		;assume segment for color
	xor	bh,bh
	mov	bl,AppsPage		;current page
	mov	cl,AppsMode		;get mode
	cmp	cl,MODE_COLOR		;is it color mode ?
	jz	@f			;yes
	mov	ax,SEG_MONO		;get mode for mono
@@:
	mov	ds,ax			;load screen segment
	mov	ax,PAGE_SIZE		;size of a page
	mul	bx   			;throw away dx
	mov	si,ax			;ds:si points to start
	mov	cx,PAGE_SIZE / 2 	;size of a page
	cld				;ready for move
	rep	movsw			;save in one go
	pop	ds			;restore ds

	clc				;successful
	jmp	short SaveScreenRet

SaveScreenErr:

	call	OEMBeep			;can't switch
	stc

SaveScreenRet:

	ret

SaveScreen endp
;----------------------------------------------------------------------------;
; RestoreScreen - restore the previously saved display context		     ;
;									     ;
; ENTRY									     ;
;	ax	=  size in bytes of screen save area			     ;
;	ds	=  cs							     ;
;	es:di	-> screen save area					     ;
; EXIT									     ;
;	cf	=  0	(screen was successfully restored)		     ;
; ERROR EXIT								     ;
;	cf	=  1	(unable to restore screen)			     ;
; USES									     ;
;	all except bp							     ;
; NOTES									     ;
;	Winoldap guarantees that the offset portion of the screen save area  ;
;	will always be zero, so it is safe to omit di from references to this;
;	area.								     ;
;----------------------------------------------------------------------------;
RestoreScreen	proc	far
		assume	ds:nothing

	cmp	ax,BufSize		;right size ?
	jb	RestoreScreenErr	;no.

; restore mode and page.

	xor	ah,ah			;set mode
	mov	al,AppsMode		;saved mode
	int	10h

	mov	ah,5			;set page
	mov	al,AppsPage		;save page
	int	10h			

	push	ds
	push	es
	mov	ax,SEG_COLOR		;assume segment for color
	xor	bh,bh
	mov	bl,AppsPage		;current page
	mov	cl,AppsMode		;get mode
	cmp	cl,MODE_COLOR		;is it color mode ?
	jz	@f			;yes
	mov	ax,SEG_MONO		;get mode for mono
@@:
	mov	si,di			;offset of saved buffer
	push	es
	pop	ds			;ds:si has source
	mov	es,ax			;load screen segment
	mov	ax,PAGE_SIZE		;size of a page
	mul	bx   			;throw away dx
	mov	di,ax			;ds:si points to start
	mov	cx,PAGE_SIZE / 2 	;size of a page
	cld				;ready for move
	rep	movsw			;save in one go
	pop	es			
	pop	ds			;restore


; restore cursor details.

	mov	ah,1			;set cursor size
	mov	ch,AppsTopCurLine	;start line
	mov	cl,AppsBotCurLine	;end line
	int	10h	

	mov	ah,2
	mov	bh,AppsPage		;saved page number
	mov	dh,AppsRow		;saved cursor row
	mov	dl,AppsCol		;saved col
	int	10h

	clc				;success
	jmp	short RestoreScreenRet	;done

RestoreScreenErr:

	call	OEMBeep			;can't restore
	stc

RestoreScreenRet:

	ret

RestoreScreen  endp
;----------------------------------------------------------------------------;
; OEMBeep:								     ;
;								             ;
; This routine taken from the grabber sources hoots the hooter.              ;
;----------------------------------------------------------------------------;

OEMBeep 	proc	near

	push	ax
	push	cx			;save registers to be destroyed
	mov	al,0B6H 		;select timer 2
	out	PIT_PORTD,al
	mov	ax,BEEP_TONE1		;divisor for tone 1
	out	PIT_PORTC,al		;write timer 2 count - lsb
	mov	al,ah
	out	PIT_PORTC,al		;write timer 2 count - msb
	in	al,PPI_PORTB		;get current setting of port
	mov	ah,al			;save setting
	or	al,00000011b		;turn speaker on
	out	PPI_PORTB,al
	mov	cx,BEEP_TIME1		;wait awhile
	loop	$
	mov	cx,BEEP_TONE2		;divisor for tone 2
	mov	al,cl
	out	PIT_PORTC,al
	mov	al,ch
	out	PIT_PORTC,al
	mov	cx,BEEP_TIME2		;wait again
	loop	$
	mov	al,ah
	out	PPI_PORTB,al
	pop	cx			
	pop	ax			;restore saved registers
	ret

OEMBeep 	endp

;----------------------------------------------------------------------------;
; ObsoleteFunction:							     ;
;									     ;
; This function traps the entry level functions that are no longer needed    ;
; in the version 3 of grabbers. It sets carry and does a RETF.		     ;
;----------------------------------------------------------------------------;
ObsoleteFunction proc near

	stc				;error, unsupported function

farp_dummy proc far			;want to have far ret
	ret
farp_dummy endp

ObsoleteFunction endp
;----------------------------------------------------------------------------;

_TEXT		ends
		end

