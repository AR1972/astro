;	TITLE	TTY - TTY output routine for MSHERC.
;***
;TTY
;
;	Copyright <C> 1987, 1988, Microsoft Corporation
;
;Purpose:
;	TTY output routine for MSHERC.
;
;******************************************************************************

	include	hgcdefs.inc

code            segment para public 'code'
                assume  cs:code,ds:code

Extrn	GScrollUp:near
Extrn	WriteGrChar:near
Extrn	SetCursor:near
Extrn	Old_Int10h_Routine:dword	;Segment:Offset of previous INT 10H

Public	WriteTTY

;-------------------------------------------------------------------------------
; E  WRITE TELETYPE TO ACTIVE PAGE
;	 AL => Character
;	 BL => Color
;-------------------------------------------------------------------------------
WriteTTY	proc	near

	mov	cl,bl			;save attr

;------Fetch the current cursor position-------
	xor	bx,bx
	mov	bl,es:BIOSPAGE
	shl	bx,1
	mov	dx,es:BIOSCURS[bx]

;----- Check for special characters -------
	cmp	al,Bell 		;Is char a bell?
	jz	ExecBell		;Yes, execute a Bell

	cmp	al,BackSpace		;Is char a backspace?
	jz	ExecBackSpace		;Yes, execute a BackSpace

	cmp	al,LineFeed		;Is char a line feed?
	jz	ExecLineFeed		;Yes, execute a LineFeed

	cmp	al,CarriageReturn	;Is char a carriage return?
	jz	ExecCarriageReturn	;Yes, execute a carriage return

;-----Not a special character so write it to screen-----
	mov	bl,cl			;use attr
	mov	cx,1			;Char Cnt = 1
	mov	bh,es:BIOSPAGE		;use active page
	Call	WriteGrChar		;Write char at current cursor pos

;------Fetch the current cursor position-------
	xor	bx,bx
	mov	bl,es:BIOSPAGE
	shl	bx,1
	mov	dx,es:BIOSCURS[bx]

;----- Advance and Check Cursor Position-------
	inc	dl			;Increment the cursor column position
	cmp	dl,80			;End of Row?
	jge	GotoNextLine		;Yes, go to next row

;----- Update the cursor and exit-------
SetCursorAndExit:
	mov	bh,es:BIOSPAGE
	Call	SetCursor		;set the cursor to new position
	jmp	FiniTTY 		;exit

;----- Routines for special cases------
ExecBell:
	mov	ah,14			;Write TTY request for orig INT 10
	pushf				;so original INT 10 can IRET
	Call	Old_Int10h_Routine	;let it BEEP for us
	jmp	FiniTTY			;Finished

ExecBackSpace:
	cmp	dl,0			;Beginning of line?
	jz	FiniTTY 		;Yes, do nothing
	dec	dl			;No, set cursor to
	jmp	SetCursorAndExit	;previous column, then exit

ExecCarriageReturn:
	xor	dl,dl		       ;Set cursor at first column in line
	jmp	SetCursorAndExit    ;and then exit

ExecLineFeed:
	mov	cx,25			;Fetch # of rows on screen
	dec	cl
	cmp	dh,cl			;Is cursor on the last line?
	jz	ScrollTheScreen 	;Yes, then scroll screen
	inc	dh			;No, set cursor to next line
	jmp	SetCursorAndExit	;and exit

;----- Set column to 0. Are we at the end of the screen?-----
GotoNextLine:
	xor	dl,dl			;Set cursor to first column on line
	mov	cx,25			;Fetch screen width
	dec	cl
	cmp	dh,cl			;Is this the last row?
	jge	ScrollTheScreen 	;Yes, scroll the screen
	inc	dh			;No, set cursor to next line
	jmp	SetCursorAndExit	;and exit routine

;----- Cursor is on last row of screen. Set cursor position then scroll-----
ScrollTheScreen:
	mov	bh,es:BIOSPAGE
	Call	SetCursor

;----- Set up parameters then scroll screen-------
	mov	ax,1		;# of lines to scroll
	xor	cx,cx		;Upper Left corner of scroll
	xor	bh,bh		;blanking attribute??
	mov	dx,79+24 shl 8	;lower right corner
	Call	GScrollUp	;Scroll up window

FiniTTY:
	Ret	;Finished Text TTY Routine
WriteTTY	endp

code	ends
	end
