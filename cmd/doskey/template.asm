 page	,132
;******************************************************************************
title TEMPLATE.ASM - DOSKey's keystroke handling
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;******************************************************************************
;
;
include	gendef.inc
include dbcs.sw
include rombios.inc

; extended key / not extended key?

ext_key		equ	0
not_ext_key	equ	not ext_key

ifdef	DBCS
ifdef	KOREA
kbd_int         equ     16h
endif	; KOREA
endif	; DBCS

; Normal Keys, DOS compatible dispalay!  Keys from 20h (" ") to ffh are
; just passed to the display device as printable characters and receive
; no special consideration. The only exception to this is <ctrl_BACKSPACE>
; 7fh which is translated to a backspace.

; some not extended keys

ctrl_f	=	6
bell	=	7
bs	=	8
tab	=	9
cr	=	13
lf	=	10
ctrl_z	=	26
escape	=	27


; some extended keys

f_1	=	59
f_2	=	60
f_3	=	61
f_4	=	62
f_5	=	63
f_6	=	64
f_7	=	65
f_8	=	66
f_9	=	67
alt_f7	=	110
alt_f8	=	111
alt_f10	=	113

home_key =	47h
up	=	48h
pgup	=	49h
left	=	4bh
right	=	4dh
end_key	=	4fh
down	=	50h
pgdn	=	51h
ins	=	52h
del	=	53h

; extended control characters

ctrl_home =	77h
ctrl_end =	75h
ctrl_left =	73h
ctrl_right =	74h

lines_per_page = 24			; default screen height


text 	segment	byte public	'CODE'
	assume	cs:text,ds:text,es:text,ss:nothing

	extrn	dumphist:near
	if	debug_version
	extrn	dumpdebug:near
	endif
	extrn	dstring:near
	extrn	word10:word

	extrn	ansi_buf:byte
	extrn	rows:word
	extrn	d_mode:byte

	public	getkey

	extrn	number_of_display_columns:near
	extrn	get_video_mode:near
	extrn	get_cursor_pos:near
	extrn	set_cursor_type:near
	extrn	set_cursor_pos:near
	extrn	beep:near
	extrn	backspace:near
	extrn	display_char:near
	extrn	get_cur_line:near
	extrn	buffer_compare:near
	extrn	prev_line:near
	extrn	next_line:near

	extrn	buf_current:word
	extrn	buf_base:word
	extrn	buf_end:word
	extrn	buf_front:word
	extrn	buf_back:word
	extrn	macro_base:word
	extrn	macro_last:word

	extrn	default_insert:byte
	extrn	command_template:dword	; pointer to int 21(10) caller's buffer
	extrn	end_template:word	; length of template from command.com
	extrn	template:byte		 ; array of locations
	extrn	output:byte		 ; array of locations
	extrn	f9_buf:byte,f9_buf_end:byte

	extrn	end_dsp:word		 ; end of current template displayed
	extrn	cur_ptr:word		 ; current position in template

	extrn	oper_flags:byte		 ; insert mode
					 ; edited?
	extrn	num_prompt:byte
	extrn	more_prompt:byte
	extrn	cur_col:byte
	extrn	over_cursor:word
	extrn	mark_row:byte
	extrn	mark_col:byte

ifdef DBCS
	extrn	DBCSLeadByteTable:dword
	extrn	read_character:near
	extrn	num_cols:word
	extrn	rows_advanced:byte
	extrn	display:near
endif

;	the operating environment of all of these routines is
;	  ds and es -> text (resident data).

;******************************************************************************
;
;   fresh_template	perform any desired editing functions on the
;			template.  There may be some invisible stuff there,
;			but we'll start with the DISPLAYABLE end == 0
;
;==============================================================================

	public	fresh_template
fresh_template	proc	near
	call	copyin_template		; get template from command.com
	mov	[end_dsp],0
	mov	[cur_ptr],0		; start at beginning
	and	[oper_flags],(not ins_mode) and (not edited)
	push	ds
	xor	ax,ax
	mov	ds,ax
	and	byte ptr ds:417h,7fh	; reset INSERT mode in ROM BIOS DATA
	pop	ds
	call	get_video_mode		; video mode for display

	cmp	[default_insert],0
	jz	fresh_01
	call	ins_t			; toggle insert mode ON
fresh_01:

edtemp_1:
	call	getkey
	call	process_key		; process_key will skip one level
					;  of return address when the user
					;  presses a <cr> or when another
					;  special termination case exists
	jmp	edtemp_1		; loop as long as process_key returns

fresh_template	endp

;******************************************************************************
;
;   copyin_template	copy the template passed by command.com into template[]
;
;==============================================================================

copyin_template	proc	near
	lds	si,[command_template]	; fetch up the old command
	assume	ds:nothing
	mov	di,offset template
	inc	si			; throw away first byte
	lodsb				; get the count byte
	mov	cl,al
	xor	ch,ch
	cmp	cx,LINE_LEN-1		; in case that field was undefined
	jbe	copyin_01
	mov	cx,LINE_LEN-1		;  limit length to LINE_LEN
copyin_01:
	mov	[end_template],cx	; save it
	jcxz	copyin_03		; done if zero length line
copyin_02:
	lodsb				; get the byte
	cmp	al,cr			;  done if <cr>  (sometimes we seem
;					;  to get single <cr> in the buffer
;					;  with a non-zero count.  This
;					;  causes a bug when COMMAND.COM's
;					;  template gets trashed by the
;					;  previous transient program
	jz	copyin_02a		; special exit if <cr>
	stosb
	loop	copyin_02
copyin_02a:
	sub	[end_template],cx	; adjust for <cr> & anything following
copyin_03:
	push	cs			; reset ds: -> text segment
	pop	ds
	assume	ds:text
	ret
copyin_template	endp

;******************************************************************************
;			L O C A L    R O U T I N E S
;******************************************************************************

;	note:  the value tables are stored in reverse order from the
;	   associated dispatch tables.  this allows us to use the
;	   value of cx after the scasb instruction as our index into
;	   the branch tables.

norm_key_table	label	byte
	db	cr
	db	lf
	db	escape
	db	bs
	db	ctrl_f
	db	ctrl_f			; will never match second duplicate
					; so if we end up with cx=0, do default
norm_key_table_siz =	$ - norm_key_table

norm_key_dispatch	label	word
	dw	char_t			; default case!
	dw	dummy_t
	dw	bs_t
	dw	esc_t
	dw	dummy_t
	dw	ret_t

;	dispatch table for extended keys

alt_key_table	label	byte
	if	debug_version
	db	alt_f8
	endif
	db	alt_f10
	db	alt_f7
	db	f_9
	db	f_8
	db	f_7
	db	ctrl_right
	db	ctrl_left
	db	ctrl_home
	db	ctrl_end
	db	f_6
	db	f_5
	db	f_4
	db	f_3
	db	f_2
	db	f_1
	db	del
	db	ins
	db	end_key
	db	home_key
	db	down
	db	up
	db	left
	db	right
	db	pgup
	db	pgdn
	db	pgdn			; duplicate for default case
alt_key_table_siz	= $ - alt_key_table

alt_key_dispatch	label	word
	dw	dummy_t			; default means ignore
	dw	pgup_or_dn_t
	dw	pgup_or_dn_t
	dw	right_t
	dw	left_t
	dw	up_t
	dw	down_t
	dw	home_t
	dw	end_t
	dw	ins_t
	dw	del_t
	dw	right_t
	dw	f2_t
	dw	f3_t
	dw	f4_t
	dw	f5_t
	dw	f6_t
	dw	ctrl_end_t
	dw	ctrl_home_t
	dw	ctrl_left_t
	dw	ctrl_right_t

	dw	f7_t
	dw	f8_t
	dw	f9_t
	dw	af7_t
	dw	af10_t
	if	debug_version
	dw	af8_t
	endif
	

;******************************************************************************
;
;   process_key		perform whatever action is needed for key in ax
;			RETURNS ONE LEVEL UP WHEN TERMINATION CONDITION EXISTS
;
;   trashes:  just about everything except segment registers
;
;==============================================================================

process_key	proc	near

	mov	di,offset norm_key_table ; set up for normal key dispatch
	mov	cx,norm_key_table_siz
	mov	bx,offset norm_key_dispatch

	cmp	ah,ext_key
	jnz	prockey_2

	mov	di,offset alt_key_table	; dispatch alternate keys then
	mov	cx,alt_key_table_siz
	mov	bx,offset alt_key_dispatch

prockey_2:
	repne	scasb			; the cx == 0 case means no match
	shl	cx,1			; convert count to word index
	add	bx,cx			; index into branch table

;	note:  some functions may assume that al still contains the character

	jmp	word ptr [bx]		; perform action

process_key	endp

;	the processing routines are generally free to trash any registers
;	  they like.  Some of them are called from the others.  A few
;	  have special register behavior for this reason.

;******************************************************************************
;
;   ret_t	- user typed a <cr>  Display it and return one level back
;
;  entry - al still has <cr>
;
;==============================================================================

ret_t	proc	near

	push	ax
	call	end_t			; be sure that cursor is at last row
	pop	ax

	call	display_char
return_from_mainloop:
	mov	cx,[over_cursor]
	call	set_cursor_type		; restore original cursor type
	pop	ax		; get rid of normal return from process_key
;	ret			; fall into dummy_t for return
ret_t	endp

;******************************************************************************
;  dummy_t .. just a return (to be used when some key is to be ignored
;******************************************************************************

dummy_t	  	proc	near
	ret
dummy_t		endp

;******************************************************************************
;
;   pgup_or_dn_t	skip directly to newest/oldest line in history
;
;	entry:  uses al==pgup to distinguish up case from down case
;
;==============================================================================

pgup_or_dn_t	proc	near
	cmp	[buf_current],0
	jz	dummy_t		; do nothing if empty workspace
	push	ax		; save pgup or pgdn character
	call	esc_t		; cancel any current line
	mov	ax,[buf_front]
	mov	[buf_current],ax
	pop	ax
	cmp	al,pgup
	jz	pgup_or_dn_t_nobackup
	mov	ax,[buf_back]	; point to end
	mov	[buf_current],ax
	call	prev_line	; back up one line if it was a pgdn
pgup_or_dn_t_nobackup:
	jmp	get_current_line
pgup_or_dn_t	endp


;******************************************************************************
;
;   left_t	shifts back one char in the template
;
;	note:  frequently called internally.  Returns zero flag true
;	       if we couldn't backup cuz [cur_ptr] was already zero.
;
;==============================================================================

ifdef	DBCS
ifdef   KOREA
; We need another left operation when we get left arrow key after during
; we write interim.
InterimOneLeft:
        mov     WasInterimFlag, 0
endif	; KOREA
endif	; DBCS

left_t	proc	near

	cmp	[cur_ptr],0		; if cur_ptr == 0, exit
	je	leftt_1

	dec	[cur_ptr]
	mov	si,[cur_ptr]
	mov	cl,output[si]
	xor	ch,ch

ifdef DBCS				; if DBCS ----------------------------
	call	CheckDBCSTailByte
	jnz	@f			; if not at tail byte
	cmp	si,0
	jz	@f			; if cur_ptr at top
	dec	[cur_ptr]		; pass DBCS tail byte
	add	cl,output[si-1]		; backspace for lead byte
@@:
	push	cx
	call	get_cursor_pos
	pop	cx
	mov	bl,[mark_col]		; save current column
	push	bx
	call	backspace		; backspace
	pop	bx

	cmp	bl,0
	jnz	@f			; if it was not at top of row
	mov	dl,byte ptr [num_cols]
	dec	dl			; last column
	mov	dh,[mark_row]
	call	read_character
	cmp	al,0
	jnz	@f			; if not ajusted
	mov	cx,1
	call	backspace
@@:
ifdef   KOREA                           ;
        cmp     WasInterimFlag, 1       ; Previous char are final, then
        je      InterimOneLeft          ; We
        mov     WasInterimFlag, 0
endif   ; KOREA
else					; if Not DBCS ------------------------

	call	backspace		; backspace so many times

endif					; end if Not DBCS --------------------

	or	al,1			; reset zero flag cuz we did backup
leftt_1:
	ret
left_t	endp


;******************************************************************************
;
;   right_t (or f1)	move right one character, set zero flag if at end
;
;==============================================================================

right_t	proc	near

	mov	si,[cur_ptr]		; get cursor
	cmp	si,[end_dsp]		; at end of displayed stuff?
	jb	rightt_01		; go ahead if not

;	well, they MUST be equal if we get here, cuz the cursor should
;	   never be past the displayed part of the line

	cmp	si,[end_template]	; is there invisible stuff at end?
	jz	rightt_09		;  return with zero true of not
	inc	[end_dsp]		; accept one more character

rightt_01:

	mov	al,template[si]		; display it, keep track of columns
	call	display_char
 	mov	output[si],al
	inc	[cur_ptr]		; will always reset zero flag

ifdef DBCS				; if DBCS ----------------------------
	mov	al,template[si]
	call	IsDBCSLeadByte
	jnz	@f			; if it was not lead byte
	inc	si
	cmp	si,[end_dsp]
	jb	rightt_tail_byte	; if not at end of displayed stuff
	cmp	si,[end_template]
	jz	@f			; if at end
	inc	[end_dsp]
rightt_tail_byte:
	mov	al,template[si]		; get the tail byte
	call	display_char
	mov	output[si],al
	inc	[cur_ptr]		; pass tail byte
@@:
	mov	al,[cur_col]
	inc	al
	cmp	al,byte ptr [num_cols]
	jnz	@f			; if this is not last column
	mov	si,[cur_ptr]
	cmp	si,[end_dsp]
	jz	@f			; if at end of displayed stuff
	mov	al,template[si]
	call	IsDbcsLeadByte
	jnz	@f			; if next character not lead byte
	mov	al,0
	call	display			; display dummy character
@@:
	or	al,1			; reset ZF
endif					; end if DBCS ------------------------

rightt_09:
	ret
right_t	endp


;******************************************************************************
;
;   home_t	move cursor to beginning of line
;
;==============================================================================

home_t	proc	near
	call	left_t
	jnz	home_t		; keep going as long as it could back up
	ret
home_t	endp

;******************************************************************************
;
;   end_t		advances cursor to end of displayed template
;
;==============================================================================

end_t	proc	near
	mov	si,[cur_ptr]		; are we at end of displayed line?
	cmp	si,[end_dsp]
	jz	xret_endt		; done if so
	call	right_t
	jmp	end_t			; keep on loopin'
xret_endt:
	ret
end_t	endp

;******************************************************************************
;
;   f3			advances cursor to end of hidden template
;
;==============================================================================

f3_t	proc	near
	call	right_t
	jnz	f3_t			; keep looping until end of line
xret_f3:
	ret
f3_t	endp

;******************************************************************************
;
;   del_t	deletes one character at cursor
;
;==============================================================================

del_t	proc	near
ifdef	DBCS
ifdef   KOREA                           ; If we get DEL key after final char
        cmp     WasInterimFlag, 1       ; then we must delete current final char
        jne     normal_del_t            ; We must delete a char at CCP
        call    TwoBackSpace
normal_del_t:
        mov     WasInterimFlag, 0
endif	; KOREA
endif	; DBCS

	mov	cx,1		; delete one character

ifdef DBCS
	mov	si,[cur_ptr]
	mov	al,template[si]		; get character at cursor
	call	IsDBCSLeadByte
	jnz	@f			; if not lead byte
	inc	cx			; erase tail byte also
@@:
endif

del_t	endp			; fall into delete_cx_chars

;******************************************************************************
;
;  delete_cx_chars	deletes cx characters at cursor
;
;==============================================================================

delete_cx_chars	proc	near
	push	cx			; save deletion count
	or	[oper_flags],edited
	call	count_columns_to_end	; how many columns does rest of line
					;  currently occupy?
	pop	bx			; restore deletion count

	mov	di,[cur_ptr]
	sub	[end_dsp],bx		; backup [end_dsp]
	jb	delcx_0			;  don't let it wrap past zero
	cmp	di,[end_dsp]		;  or even get below [cur_ptr]
	jbe	delcx_0a
delcx_0:
	mov	[end_dsp],di		; force [end_dsp] = [cur_ptr]
delcx_0a:

	lea	si,[di+bx]		; point to new under-cursor character
delcx_1:
	cmp	si,[end_template]	; are we past template?
	jae	delcx_2			; brif so
	mov	al,template[si]
	mov	template[di],al		; copy one character down
	inc	si
	inc	di
	jmp	delcx_1			;  and loop

delcx_2:
	mov	[end_template],di	; store new [end_template]

;	now all we have to do is:  save cursor, display rest of line,
;	  and display some number of spaces at the end

	push	cx			; save number of columns of old line
	call	get_cursor_pos		; get cursor position
	call	display_to_eol
	call	count_columns_to_end	; see how many it takes now
	pop	ax			; get old column count
	sub	ax,cx
	mov	cx,ax			; column count into cx

ifdef DBCS				; if DBCS ----------------------------
	inc	cx
	call	display_cx_spaces	; display blanks, reset cursor
	call	get_cursor_pos		; get cursor position
	mov	dl,[mark_col]
	inc	dl
	cmp	dl,byte ptr [num_cols]
	jnz	@f			; if this is not last column
	dec	dl
	mov	dh,[mark_row]
	call	read_character
	cmp	al,0
	jnz	@f			; if not adjusted
	inc	[mark_row]
	mov	[mark_col],0		; go to top of next row
	call	set_cursor_pos
@@:
	ret

else					; if Not DBCS ------------------------

	jmp	display_cx_spaces	; display blanks, reset cursor

endif					; end if Not DBCS --------------------

delete_cx_chars	endp

;******************************************************************************
;
;   bs_t  	Deletes the previous character
;
;==============================================================================

bs_t	proc	near
	cmp	[cur_ptr],0		; inhibit bs in column zero
	jz	bs_t_ret
	call	left_t

;	Now we want to delete the character and squeeze everything else
;	  down, UNLESS we're at the end of the line, in which case we'll
;	  let the character stay in the buffer but just erase it from
;	  the screen.


ifdef DBCS				; if DBCS ----------------------------
	mov	si,[cur_ptr]
	mov	cl,output[si]
	xor	ch,ch
	mov	al,template[si]		; get current character
	call	IsDBCSLeadByte
	jnz	@f			; if not lead byte
	inc	si			; for tail byte
	add	cl,output[si]
@@:
	inc	si
	cmp	si,[end_dsp]
	jz	@f
	jmp	del_t			; if it is not last character
@@:
	mov	si,[cur_ptr]
	mov	[end_dsp],si

else					; if Not DBCS ------------------------

	mov	si,[end_dsp]	; chop last char if we're at end of display
	dec	si
	cmp	si,[cur_ptr]
	jnz	del_t			; delete character if not at end

	mov	[end_dsp],si	; back up the display
	mov	cl,output[si]	; get number of columns it took

endif					; end if Not DBCS ---------------------

	jmp	gc_and_disp_cx	; blank out the character

bs_t_ret:
	ret
bs_t	endp

;******************************************************************************
;
;   ctrl_home_t	deletes to beginning of line
;
;==============================================================================

ctrl_home_t	proc	near
	push	[cur_ptr]	; get cursor pointer
	call	home_t		; move cursor to home
	pop	cx		; get count to delete
	jmp	delete_cx_chars	; delete up to cursor
ctrl_home_t	endp

;******************************************************************************
;
;   ctrl_end_t	deletes to end of line
;
;==============================================================================

ctrl_end_t	proc	near

	call	count_columns_to_end	; how many columns is rest of line?
	mov	si,[cur_ptr]
	mov	[end_template],si 	; delete rest of template
	mov	[end_dsp],si

ifdef DBCS
	inc	cx
endif

	jmp	gc_and_disp_cx		; erase rest of line, reset cursor
ctrl_end_t	endp

;******************************************************************************
;
;   ctrl_left_t	go to start of previous word
;
;==============================================================================

ctrl_left_t	proc	near

c_leftt_1:
	call	left_t		; first take care of adjacent whitespace
	mov	si,[cur_ptr]
	or	si,si
	jz	xret_crightt  	; if we are at beg. of line we are done

	mov	al,template[si]
	call	is_whitespace
	jz	c_leftt_1	; if whitespace we need to retreat left.

c_leftt_2:
	mov	si,[cur_ptr]	; while (there's a column to our left) &&
	or	si,si		;  (it's contents are non-white)
	jz	xret_crightt	; return if at left border
	dec	si
	mov	al,template[si]
	call	is_whitespace
	je	xret_crightt	; return if on whitespace
	call	left_t		;  keep retreating cursor to the left
	jmp	c_leftt_2

ctrl_left_t	endp
	
;******************************************************************************
;
;   ctrl_right_t	move to beginning of next word
;
;==============================================================================

ctrl_right_t	proc	near

cright_01:
	mov	si,[cur_ptr]
	cmp	si,[end_template]	; are we at end of buffer?
	jz	xret_crightt		; done if so

	mov	al,template[si]		; are we on whitespace?
	call	is_whitespace
	je	c_rightt_2		; advance until so (or hit end)
	call	right_t
	jmp	short cright_01

c_rightt_2:
	call	right_t			; now advance until end or
	jz	xret_crightt

	mov	si,[cur_ptr]
	mov	al,template[si]		; find non-whitespace
	call	is_whitespace
	je	c_rightt_2

xret_crightt:
	ret
ctrl_right_t	endp


;******************************************************************************
;
;   f2_t	search for character in template
;
;==============================================================================

f2_t	proc	near

	call	getkey		; get the next char specified
	cmp	ah,ext_key	; extended key?
	je	xret_f2		; yes .. exit

;	search for the key in template.  if found advance number of chars
	
	mov	si,[cur_ptr]
	cmp	si,[end_template]	; have we hit end with no match?
	je	xret_f2			; done if so
f2t_1:
	inc	si			; skip one character
	cmp	si,[end_template]	; at end?
	je	xret_f2

ifdef DBCS
	push	ax
	mov	al,template[si-1]
	call	IsDBCSLeadByte
	pop	ax
	jnz	@f			; if not DBCS lead byte
	inc	si			; pass tail byte
	cmp	si,[end_template]
	jz	xret_f2			; if it is end
@@:
endif

	cmp	template[si],al		; compare the characters
	jne	f2t_1			; loop if new character doesn't match

;	if we've gotten here, we know that we've advanced si at least
;	  once.  Therefore, there's no need to pre-test for si=[cur_ptr]

f2t_2:
	push	si
	call	right_t			; advance one position
	pop	si
	cmp	si,[cur_ptr]
	ja	f2t_2			; loop until we reach match

xret_f2:
	ret
f2_t	endp


;******************************************************************************
;
;   f4t		Delete chars till char specified
;
;==============================================================================
f4_t	proc	near
	call	getkey		; get the next char specified
	cmp	ah,ext_key	; extended key?
	je	xret_f2		; yes .. exit

;	search for the key in template.  if found delete till character
	
	mov	si,[cur_ptr]
	cmp	si,[end_template]
	je	xret_f2		; brif hit end with no match

f4t_1:
	inc	si		; pre-increment
	cmp	si,[end_template]
	je	xret_f2		; done if at end, no match

ifdef DBCS
	push	ax
	mov	al,template[si-1]
	call	IsDBCSLeadByte
	pop	ax
	jnz	@f			; if not DBCS lead byte
	inc	si			; pass tail byte
	cmp	si,[end_template]
	jz	xret_f2			; if end
@@:
endif

	cmp	template[si],al	; compare the characters
	jne	f4t_1		; loop if no match

	sub	si,[cur_ptr]	; how many shall we delete?
	mov	cx,si		; we incremented si at least once, cx != 0
	jmp	delete_cx_chars

f4_t	endp


;******************************************************************************
;
;   f6_t		(puts ctrl_Z in template)
;
;==============================================================================

f6_t	proc	near
	mov	al,ctrl_Z
	jmp	char_t
f6_t	endp

;******************************************************************************
;
;   af7_t	erase the history queue
;
;==============================================================================

af7_t	proc	near
	mov	ax,buf_base
	mov	buf_front,ax
	mov	buf_back,ax
	mov	buf_current,0
	ret
af7_t	endp

;******************************************************************************
;
;   f7_t	lists the entire command queue
;
;==============================================================================

f7_t	proc	near

;	first find out how many lines are on the screen for accurate paging


	mov	ax,440ch		; ANSI_GET
	mov	bx,2			; STDERR
	mov	cx,37fh			; get subfunction
	mov	dx,offset ansi_buf	; point to result buffer
	int	21h			

	jc	ansi_error		; branch if error from ANSI

	cmp	d_mode,1
	jz	rows_all_set		; done if in text mode

ansi_error:

;	Get screen height from ROM BIOS data area in ANSI's absence

ifndef JAPAN				; if Not JAPAN

	push	ds
	mov	ax,ROMBIOS_DATA 	;Get ROM Data segment
	mov	ds,ax
	Assume	DS:ROMBIOS_DATA

	mov	al,CRT_Rows		;Get max rows
	pop	ds
	Assume	DS:text

	or	al,al			;If zero specified
	jnz	@F

endif					; end if Not DBCS

	mov	al,lines_per_page	;assume 24 rows

@@:
	xor	ah,ah
	mov	rows,ax 		; set default

rows_all_set:
	dec	rows			; leave last line for (more)
	call	esc_t			; delete any input
	mov	al,1			; force <more> prompts
	call	dumphist
	jmp	return_from_mainloop	;  return a blank line so we get
;					;  re-prompted
f7_t	endp

;******************************************************************************
;
;   f8_t	scan back through history buffer looking for match
;		of characters up to cur_ptr
;
;==============================================================================

f8_t	proc	near
	mov	si,buf_current		; get original current pointer
					;  for termination test
f8_t1:
	push	si

	mov	si,buf_current
	cmp	si,buf_front		; in this case only, we'll wrap to back
	jnz	f8_t2
	mov	si,buf_back
	mov	buf_current,si
	pop	si
	cmp	si,buf_current		; does the wrap bring us back home?
	jz	f8_t3
	push	si			; re-save original pointer
f8_t2:

	call	prev_line
	mov	di,offset template
	mov	bx,[cur_ptr]		; compare this far
	call	buffer_compare		; see if we match that much
	jz	f8_matched
	pop	si
f8_t3:
	cmp	si,buf_current
	jnz	f8_t1			; loop until we go all the way 'round
	ret				; done.  No match, no change.

f8_matched:
	pop	si
	push	[cur_ptr]
	call	esc_t			; erase previous input
	call	get_current_line
	pop	ax			; get match length pointer
	mov	cx,[cur_ptr]		; get end of new line
	sub	cx,ax			; back up this much

;	we know cx can't be less than zero because we matched orig.cur_ptr
;	  bytes out of the current string

	jcxz	f8_ret
f8_backup:
	push	cx
	call	left_t		; backup cursor to match point
	pop	cx
	loop	f8_backup
f8_ret:
	ret
f8_t	endp

;******************************************************************************
;
;   f9_t	prompt for a decimal line number and skip there
;
;==============================================================================
	public	f9_t

f9_t	proc	near
	cmp	[buf_current],0	; if workspace is empty, we can't function
	jnz	f9_t001
	ret			; do nothing



f9_bs:
	cmp	si,offset f9_buf
	jbe	f9_t1		; can't bs past front of buffer
	dec	si
	mov	cl,[mark_row]
	mov	ch,[mark_col]	; save mark_row/mark_col for restore
	push	cx
	mov	cx,1
	call	backspace
	mov	al,' '		; destructive bs
	call	display_char
	mov	cx,1
	call	backspace
	pop	cx
	mov	[mark_row],cl
	mov	[mark_col],ch
	jmp	short f9_t1

f9_t001:
	call	esc_t		; delete any input
	call	get_cursor_pos
	mov	si,offset num_prompt
	call	dstring
	mov	si,offset f9_buf
f9_t1:
	call	getkey
	sub	ah,not_ext_key	; force ah=0 if non-extended
	jnz	f9_t1		; ignore extended keys
	cmp	al,escape
	jz	f9_esc
	cmp	al,cr
	jz	f9_cr		; done if cr
	cmp	al,bs
	jz	f9_bs
	cmp	al,'0'
	jb	f9_t1		; ignore non-decimal
	cmp	al,'9'
	ja	f9_t1
	cmp	si,offset f9_buf_end
	jae	f9_t1		; don't accept chars past end of buffer
	mov	[si],al
	inc	si
	call	display_char	; echo it
	jmp	f9_t1

f9_cr:
	mov	cx,si
	mov	si,offset f9_buf
	sub	cx,si		; see how many characters we've got in buffer
	xor	ax,ax		; accumulator
	jcxz	f9_esc

f9_cra:
	mul	word10
	mov	bx,ax
	lodsb
	sub	al,'0'
	xor	ah,ah
	add	ax,bx
	loop	f9_cra

	mov	bx,ax

	mov	ax,[buf_front]		; point after last line
	mov	[buf_current],ax
	mov	cx,bx		; line count
	sub	cx,1		; make 0 = 1, advance in neither case
	jbe	f9_t4
f9_t3:
	push	cx
	call	next_line
	pop	cx
	mov	ax,buf_current
	cmp	ax,buf_back	; are we at end?
	jz	f9_t3x		; exit loop if at end
	loop	f9_t3
	jmp	short f9_t4

f9_t3x:
	call	prev_line	; stick on last line

f9_t4:
	xor	al,al		; not escape flag
f9_esc:
	push	ax		; save zero or escape
	push	word ptr [cur_col] ; save current cursor column
	call	set_cursor_pos
	pop	ax		; restore end of text column
	sub	al,[cur_col]
	xor	ah,ah
	mov	cx,ax
	call	display_cx_spaces ; erase stuff with spaces
	pop	ax
	cmp	al,escape
	jnz	get_current_line ; call up the selected line
	ret
f9_t	endp

;******************************************************************************
;
;   af10_t	erase the entire macro table
;		   (without reallocating memory to history)
;
;==============================================================================

af10_t	proc	near
	mov	ax,macro_base
	mov	macro_last,ax
	ret
af10_t	endp


	if	debug_version

;******************************************************************************
;
;   af8_t
;   description: debug hotkey displays buffer pointers
;
;==============================================================================

af8_t	proc	near
	call	esc_t		; delete any input
	call	dumpdebug	; display debug info
	jmp	return_from_mainloop	;  return a blank line so we get
;					;  re-prompted
af8_t	endp

	endif

;******************************************************************************
;
;   esc_t	Invalidates the current editing on the template
;		 and starts fresh with the current template
;
;==============================================================================

esc_t	proc	near
	call	home_t		; move cursor to home
	call	ctrl_end_t	; delete to end of line
	and	[oper_flags],not edited
	jmp	copyin_template	; get a new copy of template
esc_t	endp

;******************************************************************************
;
;   f5_t	Clear the line on the screen, continue using current
;		 template.
;
;==============================================================================

f5_t	proc	near
	push	[end_template]	; save length of current template
	call	home_t		; move cursor to home
	call	ctrl_end_t	; delete to end of line
	pop	[end_template]	; leave template that same size
	and	[oper_flags],not edited
	ret

f5_t	endp


;******************************************************************************
;
;   get_current_line  Retrieves current line from buffers and displays it
;
;==============================================================================

get_current_line	proc	near

ifdef DBCS
	call	get_cursor_pos
endif

	mov	di,offset template
	call	get_cur_line
	mov	[end_dsp],cx		; set end of line pointers
	mov	[end_template],cx
	mov	[cur_ptr],0
;
;	display the template
;
	push	cx		; save line length count
	call	display_to_eol
	pop	[cur_ptr]	; cursor is at end of line
 	ret

get_current_line	endp

;******************************************************************************
;
;   up_t	gets previous command and displays it for editing. if there
;		 is no present command, the current command in buffers 
;		 is gotten
;
;==============================================================================

up_t	proc	near
	call	prev_line
	call	esc_t			; common processing
upt_01:
	jmp	get_current_line
up_t	endp

;******************************************************************************
;
;   down_t 	Makes next command the current and displays it for 
;		editing.  if there is not current command then the
;		current one in buffers is fetched
;
;==============================================================================

down_t	proc	near
	cmp	[end_dsp],0
	je	downt_01
	call	esc_t
	call	next_line		; bump pointer to next line
downt_01:
	jmp	get_current_line
down_t	endp

;******************************************************************************
;
;   ins		toggles the ins_mode bit in the flags
;
;==============================================================================

ins_t	proc	near
	xor	[oper_flags],ins_mode
;M001	push	ds
;M001	xor	ax,ax
;M001	mov	ds,ax
;M001	xor	byte ptr ds:417h,80h	; toggle INSERT bit in ROM BIOS DATA
;M001	pop	ds
	mov	al,[default_insert]
	xor	al,[oper_flags]		; are we in "alternate" mode?
	test	al,ins_mode
	push	cx
	mov	cx,[over_cursor]
	jz	ins_t1			; brif not insert mode
	sub	ch,2			; back off
	ja	ins_t1			; but not past 0
	xor	ch,ch
ins_t1:
	call	set_cursor_type
	pop	cx

;	M001 -- begin additions

	push	ds
	xor	ax,ax
	mov	ds,ax
	assume	ds:nothing
	and	byte ptr ds:417h,7fh	; force ROM BIOS insert flag off
	test	cs:[oper_flags],ins_mode
	jz	not_rep_mode
	or	byte ptr ds:417h,80h	; turn it on if in insert mode
not_rep_mode:
	pop	ds
	assume	ds:text

;	M001 -- end additions

xret_ins:
	ret
ins_t	endp

;******************************************************************************
;
;   char_t	al = character to put in template
;
;==============================================================================

char_t	proc	near

ifdef DBCS				; if DBCS ----------------------------

	jmp	dbcs_char_t

else					; if Not DBCS ------------------------

	or	[oper_flags],edited
	test	[oper_flags],ins_mode
	jne	chart_1			; brif insert mode

	mov	si,[cur_ptr]
	cmp	si,LINE_LEN-1
	jae	near_beep		; just beep if at end of buffer

	mov	si,[cur_ptr]

	mov	template[si],al		; stick character into array
	call	display_char		; display the new character
	mov	si,[cur_ptr]
	xchg	al,output[si]		; save # columns it took
	sub	al,output[si]		; how many fewer columns new char took

	inc	si
	mov	[cur_ptr],si
	cmp	si,[end_dsp]		; incr [end_dsp] & [end_template]?
	jbe	char_t_opt_01
	inc	[end_dsp]
	cmp	si,[end_template]
	jbe	char_t_opt_01
	inc	[end_template]
char_t_opt_01:

	or	al,al			; did new & old take same # columns?
	jz	xret_ins		; all finished if so

	push	ax			; save # of columns we're different by
	call	get_cursor_pos		; this is where cursor will end up
	pop	ax

	mov	si,[cur_ptr]		; display until eol or columns match
char_t_opt_02:
	cmp	si,[end_dsp]
	jz	char_t_opt_03		; done if at end of line

	push	ax
	mov	al,template[si]		; get next character
	call	display_char		;  and display it
	mov	cl,al			; get number of columns it took into cl
	pop	ax
	add	al,output[si]		; keep track of old vs. new columns
	mov	output[si],cl		; save # columns
	inc	si
	sub	al,cl			; keep running track
	jnz	char_t_opt_02		; loop only until/unless columns match

char_t_opt_03:
	cbw				; get # spaces needed at end
	mov	cx,ax			; into cx
	jmp	display_cx_spaces	; do spaces, reset cursor

;	okay.  Now we're going to insert a character into the template

chart_1:
	mov	si,[end_dsp]
	cmp	si,LINE_LEN-1
	jb	chart_2			; brif room to insert
near_beep:
	jmp	beep			; can't do it if it's full

chart_2:

	mov	si,[end_template]
	cmp	si,LINE_LEN-1		; was line already full?
	jz	chart_2a		; skip if so
	inc	si
	mov	[end_template],si
chart_2a:
	cmp	si,[cur_ptr]
	je	chart_3
	dec	si
	mov	cl,template[si]
	mov	template[si][1],cl
	jmp	chart_2a

chart_3:
	mov	template[si],al
	inc	[end_dsp]
	call	display_char
	mov	output[si],al	; save the size of the inserted char
	inc	[cur_ptr]	; and bump cursor

	call	get_cursor_pos

	call	display_to_eol	; display the rest of the line

	jmp	set_cursor_pos

endif					; end if Not DBCS --------------------

char_t	endp


;******************************************************************************
;
;   count_columns_to_end	returns cx= number of columns between
;				[cur_ptr] and [end_dsp]
;
;==============================================================================

count_columns_to_end	proc	near
	mov	cx,0		; initialize count
	mov	si,[cur_ptr]
ccte_1:
	cmp	si,[end_dsp]	; are we at the end of the display?
	jae	xret_iswhite	; return if so
	mov	al,output[si]
	inc	si
	cbw
	add	cx,ax		; accumulate number of columns
	jmp	ccte_1
count_columns_to_end	endp

;******************************************************************************
;
;   display_to_eol	Displays the current template from cur_ptr to end_dsp
;
;==============================================================================

display_to_eol	proc	near

ifdef DBCS
	call	clear_to_eol
endif

	mov	si,[cur_ptr]
dteol_1:
	cmp	si,[end_dsp]
	jae	xret_iswhite		; return if at end
	mov	al,template[si]
	call	display_char
	mov	output[si],al
	inc	si
	jmp	dteol_1
display_to_eol	endp

;******************************************************************************
;
;   is_whitespace	sets zero flag if al is a space or a tab
;
;==============================================================================

is_whitespace	proc	near
	cmp	al,' '
	je	xret_iswhite
	cmp	al,tab
xret_iswhite:
      	ret
is_whitespace	endp

;******************************************************************************
;
;   getkey		Waits for key to be entered and returns it
;   exit:   		al= keystroke, ah= ext_key / not_ext_key
;
;	note that we won't execute a blocking call.  We'll go into a
;	polling loop and issue win386 idle signals while we wait.
;
;==============================================================================

getkeyx	proc	near

getkey_1:
	int	IDLE_INT	; signal an idle interrupt

	mov	ax,1680h
	int	I2f_INT		; and call in win386 to release current
				; vm's time slice
getkey:

ifdef   KOREA
; In KOREA, we can not use the DOS system call to get char from KBD because
; of some hangeul/hanja char has special scan code in AH. We must seperate it.
;       AH = 0f0h       : Interim Char
;            0f1h       : Final Char
;            0f2h       : Hanja Char which were conversioned at Current Cursor
;                         Position(CCP)
;

        mov     ah, CHECK_INPUT_INTERIM
        int     kbd_int
        jz      getkey_1                ; if no character, do idling stuff

;	character present in keyboard buffer, get it and if it is an
;       extended key then store AH value to AL
;	
        mov     ah, CHAR_INPUT_INTERIM
        int     kbd_int
        or      al, al                  ; ah = extended key value if exist
        jnz     getkey_3
;
;	extended key input
;
        mov     al, ah                  ; Store extended key
        mov     ah, ext_key             ; Mark it
        ret
getkey_3:
        mov     InterimFlag, 1
        cmp     ah, CHAR_INTERIM        ; Is it interim char?
        jz      getkey_4                ; Jump if yes
        mov     InterimFlag, 0          ; Default is final
        cmp     ah, HANJA_AT_CCP        ; Hanja at CCP
        jnz     getkey_4                ; Jump if not
        mov     InterimStart, 1         ; Must overwrite at CCP
getkey_4:
        mov     ah, not_ext_key         ; It isn't extended key
	ret

else

	mov	ah,CHECK_INPUT_STATUS
	int	DOS_INT
	or	al,al
	jz	getkey_1		; if no character, do idling stuff

;	character present in keyboard buffer, get it and if it is an
;	extended key then go to get the second character in the buffer.
;	
	mov	ah,char_inp_no_echo
	int	DOS_INT

	mov	ah,not_ext_key		; assume not extended key
	or	al,al
	jne	getkey_3

;
;	extended key input
;
	mov	ah,char_inp_no_echo
	int	DOS_INT
	mov	ah,ext_key
getkey_3:
	ret
endif   ; KOREA

getkeyx	endp


;**************************************************************
;	here are some useful display utility subroutines
;**************************************************************

;**************************************************************
;  gc_and_disp_cx	save cursor position and fall into display_cx_spaces
;**************************************************************
gc_and_disp_cx	proc	near
	push	cx
	call	get_cursor_pos		; mark cursor position for restoration
	pop	cx
gc_and_disp_cx	endp

;**************************************************************
;  display_cx_spaces	displays cx spaces and then jump to
;			set_cursor_pos, does no spaces if cx is <= 0
;**************************************************************

display_cx_spaces	proc	near
	cmp	cx,0
	jle	dcxs_done	; done if cx <= 0
dcxs_loop:
	mov	al,' '
	call	display_char
	loop	dcxs_loop
dcxs_done:
	jmp	set_cursor_pos
display_cx_spaces	endp

ifdef DBCS				; if DBCS ----------------------------
;
;	Clear till end of line
;
clear_to_eol		proc	near
	push	ax
	push	si
	push	word ptr [mark_row]
	mov	bh,[rows_advanced]
	mov	bl,[cur_col]
	push	bx
	call	count_columns_to_end	; how many columns is rest of line?
	xor	bh,bh
	add	bx,cx
	cmp	bx,[num_cols]
	jb	@f			; if only one row
	inc	cx
@@:
	call	gc_and_disp_cx
	pop	bx
	mov	[rows_advanced],bh
	mov	[cur_col],bl
	pop	word ptr [mark_row]
	pop	si
	pop	ax
	ret
clear_to_eol		endp
endif					; end if DBCS ------------------------

ifdef DBCS				; if DBCS ----------------------------
;
;	DBCS routine of char_t
;
;	input:	AL = character
;

dbcs_flag	db	0		; 0=sbcs, 1=lead byte, 2=tail byte
dbcs_char	db	0
refresh_flag	db	0
ifdef   KOREA
        public  InterimFlag
InterimFlag             db      0
WasInterimFlag          db      0
InterimStart            db      0
endif   ; KOREA

dbcs_char_t	proc	near
	cmp	dbcs_flag,1
	jz	chart_dbcs		; if this is tail byte
	cmp	dbcs_flag,2
	jnz	@f			; if previous was not tail byte
	mov	dbcs_flag,0		; reset flag
@@:
	call	IsDBCSLeadByte
	jnz	@f			; if this is not lead byte
chart_dbcs:
	inc	dbcs_flag		; 0=sbcs, 1=lead byte, 2=tail byte
@@:

	or	[oper_flags],edited
	test	[oper_flags],ins_mode
	je	@f

ifdef	KOREA
        cmp     InterimStart, 1
        je      @f
endif   ; KOREA

	jmp	chart_1
@@:
	mov	si,[cur_ptr]

	cmp	dbcs_flag,1
	jnz	chart_o_notlead		; if this is not lead byte
	cmp	si,LINE_LEN-2
	jnb	chart_o_full		; if no space for double byte
	push	ax
	mov	al,template[si]		; get current character
	call	IsDBCSLeadByte
	pop	ax
	jz	@f			; if overwrite DBCS to DBCS
	mov	si,[end_dsp]
	cmp	si,LINE_LEN-1
	jnb	chart_o_full		; if there is no space for tail byte
	call	ins_space		; save space for tail byte
	mov	refresh_flag,1
@@:
	mov	dbcs_char,al
	jmp	chart_ret
chart_o_full:
	call	getkey			; throw tail byte
	mov	dbcs_flag,0		; reset
chart_o_beep:
	jmp	beep
chart_o_notlead:

	cmp	dbcs_flag,2
	jz	chart_o_tail		; if this is tail byte
	cmp	si,LINE_LEN-1
	jae	chart_o_beep		; just beep if at end of buffer
	call	check_cur		; adjust cursor for wrap
	jnc	@f
	mov	refresh_flag,1
@@:
	push	ax
	mov	al,template[si]		; get current character
	call	IsDBCSLeadByte
	pop	ax
	jnz	@f			; if overwrite single to single
	call	clear_to_eol
	call	del_char
	mov	refresh_flag,1
	jmp	short @f
chart_o_tail:

ifdef   KOREA
        cmp     InterimFlag, 0
        mov     cl, [InterimStart]      ;
        mov     WasInterimFlag, cl      ; We wrote Final char previously
        mov     InterimStart, 0         ; Yes, we will write interim char
        je      YesFinal1
        mov     refresh_flag, 1
        mov     WasInterimFlag, 0       ; We wrote writing Interim
        mov     InterimStart, 1         ; Yes, we will write interim char
YesFinal1:
endif   ; KOREA

	xchg	al,dbcs_char		; get lead byte
@@:

	mov	template[si],al		; stick character into array
	call	display_char		; display the new character
	mov	si,[cur_ptr]
	mov	ch,al			; set new char length
	xchg	al,output[si]		; save # columns it took
	mov	cl,al			; set old char length
	inc	si
	mov	[cur_ptr],si

	cmp	dbcs_flag,2
	jnz	@f			; if this is not dbcs

	mov	al,dbcs_char		; get tail byte
	mov	template[si],al		; stick character into array
	call	display_char		; display the new character
	mov	si,[cur_ptr]
	add	ch,al			; add new char length
	xchg	al,output[si]		; save # columns it took
	add	cl,al			; add old char length
	inc	si
	mov	[cur_ptr],si
@@:
	sub	cl,ch
	mov	al,cl

	cmp	si,[end_dsp]		; incr [end_dsp] & [end_template]?
	jbe	char_t_opt_01
	mov	[end_dsp],si
	cmp	si,[end_template]
	jbe	char_t_opt_01
	mov	[end_template],si
char_t_opt_01:

	cmp	refresh_flag,0
	jz	@f
	mov	refresh_flag,0
	call	get_cursor_pos
	call	display_to_eol

ifdef	KOREA
        cmp     InterimFlag, 0          ; Is it final char?
        je      YesFinal
        call    set_cursor_pos
        call    TwoBackSpace
        ret
YesFinal:
        jmp     set_cursor_pos
else
        jmp     set_cursor_pos
endif   ; KOREA

@@:

	or	al,al			; did new & old take same # columns?
	jnz	@f
	jmp	chart_ret		; all finished if so
@@:

	push	ax			; save # of columns we're different by
	call	get_cursor_pos		; this is where cursor will end up
	pop	ax

	mov	si,[cur_ptr]		; display until eol or columns match
char_t_opt_02:
	cmp	si,[end_dsp]
	jz	char_t_opt_03		; done if at end of line

	push	ax
	mov	al,template[si]		; get next character
	call	display_char		;  and display it
	mov	cl,al			; get number of columns it took into cl
	pop	ax
	add	al,output[si]		; keep track of old vs. new columns
	mov	output[si],cl		; save # columns
	inc	si
	sub	al,cl			; keep running track

	jnz	char_t_opt_02		; loop only until/unless columns match

char_t_opt_03:
	cbw				; get # spaces needed at end
	mov	cx,ax			; into cx
	jmp	display_cx_spaces	; do spaces, reset cursor

;	okay.  Now we're going to insert a character into the template

chart_1:
	mov	si,[end_dsp]

	cmp	dbcs_flag,1
	jnz	chart_i_notlead		; if this is not lead byte
	cmp	si,LINE_LEN-2
	jnb	@f			; if there no space for tail byte
	mov	dbcs_char,al
	jmp	short chart_ret
@@:
	call	getkey			; throw tail byte
	mov	dbcs_flag,0		; reset
chart_i_beep:
	jmp	beep
chart_i_notlead:

	cmp	dbcs_flag,2
	jz	chart_i_tail

	cmp	si,LINE_LEN-1
	jnb	chart_i_beep		; no room to insert
	call	check_cur		; adjust cursor for wrap
	jmp	short @f
chart_i_tail:
	xchg	al,dbcs_char
@@:

	call	ins_space
	mov	si,[cur_ptr]
	mov	template[si],al
	call	display_char
	mov	output[si],al		; save the size of the inserted char
	inc	[cur_ptr]		; and bump cursor

	cmp	dbcs_flag,2
	jnz	@f			; if this is not dbcs
	mov	al,dbcs_char
	call	ins_space
	mov	si,[cur_ptr]
	mov	template[si],al
	call	display_char
	mov	output[si],al		; save the size of the inserted char
	inc	[cur_ptr]		; and bump cursor
@@:

	call	get_cursor_pos
	call	display_to_eol		; display the rest of the line

ifdef   KOREA
        cmp     InterimFlag, 0
        je      YesFinal2
        mov     InterimStart, 1
        call    set_cursor_pos
        call    TwoBackSpace
        ret
YesFinal2:
        jmp     set_cursor_pos
else
	jmp	set_cursor_pos
endif   ; KOREA

chart_ret:
	ret
dbcs_char_t	endp

;
;	Insert one space
;
ins_space	proc	near
	push	ax
	push	si
	mov	si,[end_template]
	cmp	si,LINE_LEN-1		; was line already full?
	jz	inss_loop		; skip if so
	inc	si
	mov	[end_template],si
inss_loop:
	cmp	si,[cur_ptr]
	je	@f			; if this is end
	dec	si
	mov	al,template[si]
	mov	template[si+1],al	; move template
	mov	al,output[si]
	mov	output[si+1],al		; move output
	jmp	inss_loop

@@:
	mov	template[si],' '
	mov	output[si],1
	inc	[end_dsp]

	mov	si,[end_template]
	call	CheckDBCSTailByte
	jz	@f			; if end char is tail byte
	mov	al,template[si]
	call	IsDBCSLeadByte
	jnz	@f			; if end char is not lead byte
	mov	template[si],' '
@@:

	pop	si
	pop	ax
	ret
ins_space	endp

;
;	Delete one character
;
del_char	proc	near
	push	ax
	push	si
	mov	si,[cur_ptr]
	cmp	si,[end_template]
	jnb	delc_end		; if this is at end
delc_loop:
	inc	si
	mov	al,template[si]
	mov	template[si-1],al	; move template
	mov	al,output[si]
	mov	output[si-1],al		; move output
	cmp	si,[end_template]
	jnz	delc_loop		; if this is end
delc_end:
	mov	template[si],' '
	mov	output[si],1
	dec	[end_template]
	mov	si,[cur_ptr]
	cmp	si,[end_dsp]
	jz	@f			; if at end
	dec	[end_dsp]
@@:
	pop	si
	pop	ax
	ret
del_char	endp

;
;	Check ajust mark at the cursor
;
check_cur	proc	near
	push	ax
	push	dx
	call	get_cursor_pos
	cmp	[mark_col],0
	jnz	cc_ok			; if not at top of row
	cmp	[cur_ptr],0
	jz	cc_ok			; if top of buffer
	mov	dh,[mark_row]
	cmp	dh,0
	jz	cc_ok			; if at top
	dec	dh			; previus row
	mov	dl,byte ptr [num_cols]
	dec	dl			; last column
	call	read_character
	cmp	al,0
	jnz	cc_ok			; if not adjusted
	mov	[mark_row],dh
	mov	[mark_col],dl
	call	set_cursor_pos
	stc
	jmp	short cc_ret
cc_ok:
	clc
cc_ret:
	pop	dx
	pop	ax
	ret
check_cur	endp

ifdef   KOREA
TwoBackSpace    proc
        mov     cx, 2
        call    backspace
        mov     si, [cur_ptr]
        sub     si, 2
        mov     [cur_ptr], si
        ret
TwoBackSpace     endp
endif   ; KOREA

endif					; end if DBCS ------------------------


ifdef DBCS
;****************************************************************************
;
;		Common Subroutines for DBCS version
;
;****************************************************************************

	public	IsDBCSLeadByte
;
;	Test if the character is DBCS Lead Byte
;
;	input:	AL = character to check
;	outpit:	ZF = 1 if DBCS Lead Byte
;
IsDBCSLeadByte		proc	near
	push	ax
	push	si
	push	ds
	mov	ah,al			; save character to check
	lds	si,DBCSLeadByteTable	; get table address
test_lead_byte:
	lodsb				; get lower value
	or	al,al
	jz	not_lead_byte		; if at end of table
	cmp	ah,al
	jb	not_lead_byte		; if out of range
	lodsb				; get higher value
	cmp	ah,al
	ja	test_lead_byte		; if out of range do next
	and	al,0			; set ZF
	jmp	short test_lead_byte_end
not_lead_byte:
	or	al,1			; reset ZF
test_lead_byte_end:
	pop	ds
	pop	si
	pop	ax
	ret
IsDBCSLeadByte		endp



	public	CheckDBCSTailByte
;
;	Check if the character position is at Tail Byte of DBCS
;
;	input:	template = start address of the string
;		si = character position to check
;	output:	ZF = 1 if at Tail Byte
;
CheckDBCSTailByte	proc	near
	push	ax
	push	bx
	push	si
	lea	bx,template[si]		; get current character address
	lea	si,template		; get start address
check_tail_byte:
	cmp	si,bx
	jz	not_tail_byte		; if at DBCS lead byte or single byte
	ja	tail_byte		; if at DBCS tail byte
	lodsb				; get character
	call	IsDBCSLeadByte
	jnz	check_tail_byte		; if not DBCS lead byte
	inc	si			; pass tail byte
	jmp	short check_tail_byte
not_tail_byte:
	or	al,1			; reset ZF
	jmp	short check_tail_byte_end
tail_byte:
	and	al,0			; set ZF
check_tail_byte_end:
	pop	si
	pop	bx
	pop	ax
	ret
CheckDBCSTailByte	endp

endif					; end of DBCS

text	ends
	end
