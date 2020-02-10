	page	,132
;******************************************************************************
TITLE DISPLAY.ASM - Screen Display for DOSKEY
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;******************************************************************************
;
;   (C) Copyright MICROSOFT Corp., 1989
;
;
	include	gendef.inc
	include dbcs.sw

video_int	equ	10h
;

;******************************************************************************
;			I N S T A N C E    D A T A
; (This data is all collectively defined in Win386.Asm in the instance block)
;******************************************************************************
	

; Purely Local

	extrn	num_cols:word
	extrn	active_page:byte
	extrn	cur_col:byte
	extrn	over_cursor:word
	extrn	mark_row:byte
	extrn	mark_col:byte
	extrn	rows_advanced:byte

	extrn	macro_base:word
	extrn	macro_last:word
	extrn	macro_end:word
	extrn	buf_base:word
	extrn	buf_end:word
	extrn	buf_front:word
	extrn	buf_back:word
	extrn	buf_current:word
	extrn	text_ptr:word
	extrn	macro_ptr:word
	extrn	end_dsp:word

	extrn	more_prompt:byte
	extrn	rows:word
	extrn	getkey:near

ifdef DBCS
	extrn	IsDBCSLeadByte:near
endif

text 	segment	byte public	'CODE'
	assume	cs:text, ds:nothing, es:nothing, ss:nothing


	public	number_of_display_columns
	public	get_video_mode
	public	get_cursor_pos
	public	set_cursor_type
	public	set_cursor_pos
	public	number_of_display_columns
	public	backspace
	public	display_char
	public	beep

	public	dumphist
	if	debug_version
	public	dumpdebug
	endif
	public	dstring
	public	newline
	public	word10

;******************************************************************************
;
;   get_video_mode
;
;   description:  Use BIOS Int16 function 0fh to setup
;		     num_cols and active_page
;		    and function 03h to setup over_cursor
;
;==============================================================================
get_video_mode		proc	near
	assume	ds:text,es:nothing


	mov	ah,0fh			; get video mode
	int	video_int
	mov	al,ah
	xor	ah,ah
	mov	[num_cols],ax
	mov	[active_page],bh

	mov	ah,3
	int	video_int
	mov	[over_cursor],cx	; save "overstrike" cursor
	mov	[cur_col],dl		; save the actual column
	ret

get_video_mode	endp
	

;******************************************************************************
;
;   get_cursor_pos	Gets the current cursor position and stores it into
;			cur_col, mark_col & mark_row.  Also sets rows_advanced
;			to zero.
;
;==============================================================================
get_cursor_pos	proc	near
	assume	ds:text,es:nothing

	mov	ah,3			; get cursor position
	mov	bh,[active_page]
	int	video_int
	mov	[cur_col], dl
	mov	[mark_row],dh
	mov	[mark_col],dl
	mov	[rows_advanced],0		; number of rows advanced

	ret
get_cursor_pos	endp

;******************************************************************************
;
;   set_cursor_pos	Sets the cursor position to the marked position
;			Note:  if (rows_advanced != 0) then set mark_row =
;				(get_cursor_pos_row) - rows_advanced
;
;==============================================================================

set_cursor_pos	proc	near
	assume	ds:text,es:nothing


	cmp	[rows_advanced],0
	je	rcp_1

	mov	ah,3			; get cursor position
	mov	bh,[active_page]
	int	video_int
	sub	dh,[rows_advanced]	; find the original 
	mov	[mark_row],dh

rcp_1:
	mov	ah,2			; set cursor position
	mov	bh,[active_page]
	mov	dh,[mark_row]
	mov	dl,[mark_col]
	mov	[cur_col],dl
	int	video_int

	ret

set_cursor_pos	endp


;******************************************************************************
;
;   set_cursor_type	Sets cursor type via ROM BIOS to type in CX
;
;==============================================================================

set_cursor_type	proc	near
	assume	ds:text,es:nothing

	mov	ah,1			; set cursor type
	int	video_int
	ret

set_cursor_type	endp

;******************************************************************************
;
;   number_of_display_columns
;
;   description: Returns the number of character positions required
;		 to display the character in al.  Note: if al=tab, we'll
;		 use cur_col in our calculations.
;
;   entry:     al contains character
;   exit:      cx number of chars corresponding to display
;	       al still contains the character
;
;==============================================================================

number_of_display_columns	proc	near
	assume	ds:text,es:nothing

	cmp	al,TAB		; is it a tab character?
	je	nodc_tab	;  brif so

	mov	cx,1		; assume it is just 1 char
	cmp	al,' '		; is it a control character?
	jae	nodc_ret
	cmp	al,CTRL_T	; these are just
	je	nodc_ret
	cmp	al,CTRL_U	; 1 character.
	je	nodc_ret
	inc	cl		; two characters then
nodc_ret:
	ret

nodc_tab:
	mov	cx,708h		; ch=mask, cl=8
	and	ch,[cur_col]
	sub	cl,ch		
	xor	ch,ch
	ret
number_of_display_columns	endp

;******************************************************************************
;
;   backspace		backspace cursor over character
;   entry:		cx number of characters to backspace
;   uses:		mark_row, mark_col
;
;==============================================================================

backspace	proc	near
	assume	ds:text,es:nothing

	push	cx
	call	get_cursor_pos	; get current position.
	pop	cx
	mov	al,[mark_col]	; ax = current column
	xor	ah,ah

	sub	ax,cx		; back up column by count
	jnb	bs_2		; brif no wrap to previous line(s)

bs_1:
	dec	[mark_row]
	add	ax,[num_cols]
	jnc	bs_1		; loop until we go positive again
bs_2:
	mov	[mark_col],al
	call	set_cursor_pos ; and position the cursor there

	ret
backspace	endp


;******************************************************************************
;
;   display_char	Processes character and displays on screen
;   entry:		al char to display
;   exit:		ax = number of display positions
;   Preserves:		si, di, bx, cx
;
;==============================================================================
display_char	proc	near
	assume	ds:text,es:nothing

	push	si
	push	cx
	call	number_of_display_columns
	push	cx
	cmp	al,TAB
	je	dc_tab

	cmp	al,' '			; is it a ^ control char display?
	jae	dc_3			; jump if not

	cmp	al,cr
	je	dc_3
	cmp	al,LF
	je	dc_3
	cmp	al,CTRL_T		; control t is not processed
	je	dc_3
	cmp	al,CTRL_U		; control u is also not processed.
	je	dc_3

; ctrl character display

	mov	cl,al
	mov	al,'^'
	call	display
	mov	al,cl
	add	al,40h		; make control character displayable

; normal character display

dc_3:
	call	display
dc_4:
	pop	ax			; number of columns into ax
	pop	cx			; restore cx
	pop	si			; restore si
	ret

dc_tab:
	mov	al,' '
dc_tab_1:
	call	display
	loop	dc_tab_1
	jmp	short dc_4
display_char	endp
	


;******************************************************************************
;
;   beep		sound a beep via int10 (ttyoutput)
;
;==============================================================================

beep		proc	near
	mov	ax,(0eh shl 8) + bell		; tty output
	int	video_int
	ret
beep	endp

;******************************************************************************
;
;	dumphist	dump the history buffer to console output
;			al = non-zero if we want line numbers and <more>
;			  (rows must be set to the size of the screen if so)
;
;==============================================================================


dumphist	proc	near

;	*note:  We use the high bit of bx to indicate the need to
;		display a newline and a line number on the next character
;		displayed.  The reason for delaying the newline/line number
;		is that we don't want an extra line number on the end.

	mov	bx,8000h		; start line numbering at 1
;					;  bit7 means next char forces newline
	mov	si,buf_front
	cmp	si,buf_back		; is it a split buffer?
	jbe	dumphist_01		; skip if not

	mov	cx,buf_end
	sub	cx,si			; get length to end of buffer
	call	dumphist_doblock	; do a simple dump for now
	mov	si,buf_base
dumphist_01:
	mov	cx,buf_back
	sub	cx,si

dumphist_doblock:
	jcxz	xret_dumphist
dumphist_doblock_1:
	push	ax			; save <more> flag

	test	bh,80h			; time to do a line number?
	jz	dumphist_doblock_1ax	; brif not
	and	bh,7fh

	or	al,al			; suppress line numbering?
	jz	dumphist_doblock_cronly

	or	bx,bx
	jz	no_more_check

	mov	ax,bx
	xor	dx,dx
	div	rows			; time to do (more)?
	or	dx,dx			;  only if remainder == 0
	jnz	no_more_check

;	now display "press any key for more" and wait for key

	push	cx
	push	si
	mov	si,offset more_prompt
	call	dstring
	pop	si
	call	getkey
	pop	cx

no_more_check:
	inc	bx			; increment number
	call	newline
	mov	ax,bx
	call	decout_ax
	mov	al,':'
	call	display_char
	mov	al,' '
	cmp	si,buf_current		; are we at the current line?
	jnz	not_at_current_line
	mov	al,'>'			; display '>' pointing to it if so
not_at_current_line:
	call	display_char
	jmp	short dumphist_doblock_1ax

dumphist_doblock_cronly:
	or	bx,bx			; don't do newline on first line
	jz	dumphist_doblock_1ay
	call	newline
dumphist_doblock_1ay:
	inc	bx			; keep count of lines
dumphist_doblock_1ax:

	lodsb
	or	al,al
	jnz	dumphist_doblock_2

	or	bh,80h		; set flag for newline on next char.
	jmp	short dumphist_doblock_3

dumphist_doblock_2:
	call	display_char
dumphist_doblock_3:
	pop	ax			; restore <more> flag
	loop	dumphist_doblock_1

xret_dumphist:
	ret

dumphist	endp


	if	debug_version

;******************************************************************************
;
;   dumpdebug: debug hotkey displays buffer pointers
;
;==============================================================================

dk_cs	dw	0

dumpdebug	proc	near
	mov	dk_cs,cs
	mov	si,offset debug_info
dumpdebug_t1:
	call	newline
	call	dstring		; display a string
	lodsw
	mov	bx,ax
	mov	ax,[bx]
	call	hex16
	cmp	byte ptr [si],0
	jnz	dumpdebug_t1
;
	ret

debug_info:
	db	'segment = ',0
	dw	dk_cs
	db	'text_ptr = ',0
	dw	text_ptr
	db	'end_dsp = ',0
	dw	end_dsp
	db	'macro_ptr = ',0
	dw	macro_ptr
	db	'macro_base = ',0
	dw	macro_base
	db	'macro_last = ',0
	dw	macro_last
	db	'macro_end = ',0
	dw	macro_end
	db	'buf_base = ',0
	dw	buf_base
	db	'buf_end = ',0
	dw	buf_end
	db	'buf_front = ',0
	dw	buf_front
	db	'buf_back = ',0
	dw	buf_back
	db	'buf_current = ',0
	dw	buf_current
	db	0
;
dumpdebug	endp

	endif


;**************************************************************
;	display a crlf
;**************************************************************

newline	proc	near
	mov	al,cr
	call	display_char
	mov	al,lf
	jmp	display_char
newline	endp

;*********************************************************
;
;	display ax in decimal, leading zero suppressed
;	  trash ax,dx
;
;	Note: algorithm taken from code published by our esteemed leader
;	  years ago in his programming column in the MITS newsletter
;
;	may use up to 20 bytes of stack
;
;*********************************************************
word10	dw	10

decout_ax	proc	near
	xor	dx,dx
	assume	ds:nothing
	div	word10
	assume	ds:text
	or	ax,ax		; any remaining digits to display?
	push	dx		; save the units digit
	jz	decout_ax_1	; done if no more digits
	call	decout_ax	; do the rest recursively
decout_ax_1:
	pop	ax		; recover digit from stack
	add	al,'0'		; make it ascii
	jmp	display_char
decout_ax	endp


;**************************************
;       display zero terminated string at si. trash al,cx.
;         return si->after null
;**************************************
 
dstring_1:
        call    display_char
dstring:
        lodsb
        or      al,al
        jnz     dstring_1
dstring_ret:
        ret

	if	debug_version

hex16:
	push	ax
	mov	al,ah
	call	hex8
	pop	ax
hex8:
	push	ax
	shr	al,1
	shr	al,1
	shr	al,1
	shr	al,1
	call	hexnib
	pop	ax
hexnib:
	and	al,0fh
	add	al,90h
	daa
	adc	al,3ah
	daa
	jmp	display_char

	endif

;******************************************************************************
;			L O C A L    R O U T I N E S
;******************************************************************************
;******************************************************************************
;
;   display	display the processed character on the screen
;	          through dos_int function char_output
;   entry:	 al char
;
;	Note:  this code keeps cur_col and rows_advanced up to date.
;		It assumes that all characters displayed through here
;		take one column to display, except <cr>.
;
;==============================================================================

ifdef DBCS
	public	display
disp_flag	db	0		; 0=single, 1=lead byte, 2=tail byte
endif

display	proc	near
	assume	ds:text,es:nothing


ifdef DBCS				; if DBCS ----------------------------
	cmp	disp_flag,1
	jz	set_dbcs		; if it was lead byte
	cmp	disp_flag,2
	jnz	@f			; if it was not tail byte
	mov	disp_flag,0		; reset
@@:
	call	IsDBCSLeadByte
	jnz	@f			; if this is not lead byte
set_dbcs:
	inc	disp_flag
@@:
	cmp	disp_flag,1
	jnz	@f			; if this is not lead byte
	mov	dl,[cur_col]
	inc	dl
	cmp	dl,byte ptr [num_cols]
	jb	@f			; if this is not last column
	push	ax
	mov	ah,char_output
	mov	dl,0			; mark it and go to next row
	int	dos_int
	pop	ax
	mov	[cur_col],0
	inc	[rows_advanced]
@@:
endif					; end if DBCS ------------------------

	mov	ah,char_output
	mov	dl,al
	int	dos_int

	xor	ah,ah		; reset column to zero if 'cr'
	cmp	al,cr
	je	prt_2

	mov	ah,[cur_col]
	inc	ah
	cmp	ah,byte ptr [num_cols]
	jb	prt_2

;	Note:  We come here if the display should have wrapped.  Unfortunately,
;	  there are times when ANSI.SYS (or another display driver) has wrap
;	  disabled.  Therefore, we must inquire the column number and force
;	  a CRLF if it is == [num_cols].

	push	bx
	push	cx
	mov	ah,3			; get cursor position
	mov	bh,[active_page]
	int	video_int
	pop	cx
	pop	bx
	inc	dl
	cmp	dl,byte ptr [num_cols]	; are we still at the end of line?
	jnz	prt_00a			; brif no need to force wrap

	mov	dl,0dh
	mov	ah,char_output
	int	dos_int
	mov	dl,0ah
	mov	ah,char_output
	int	dos_int

prt_00a:
	xor	ah,ah
	inc	[rows_advanced]
prt_2:
	mov	[cur_col],ah
	ret

display	endp

ifdef DBCS				; if DBCS ----------------------------
	public	read_character
;
;	Read Character at cursor
;
;	input:	dh = cursor row
;		dl = cursor column
;	output:	al = character
;
read_character		proc	near
	push	dx
	mov	cx,dx			; save original cursor position
	push	cx
	mov	ah,3			; get cursor position
	mov	bh,[active_page]
	int	video_int
	pop	cx
	xchg	dx,cx			; cursor position to read
	mov	ah,2			; set cursor
	int	video_int
	mov	ah,8			; read character
	int	video_int
	mov	dx,cx			; original cursor position
	mov	ah,2			; set cursor
	int	video_int
	pop	dx
	ret
read_character		endp
endif					; end if DBCS


text	ends
	end
