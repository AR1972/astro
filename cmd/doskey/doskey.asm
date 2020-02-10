page ,132
;******************************************************************************
Title DOSKEY.ASM - First module/init code
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;******************************************************************************
;
;   (C) Copyright Microsoft Corp., 1989
;
;	Revision history:
;
;	06/09/89	1.01	Original version
;	09/10/89	1.02	Restructuring source modules
;	04/17/90	1.03	Added /? option (c-PaulB)

hipper_install	=	0	; true for hipper final install sequence
	include	gendef.inc
	include dbcs.sw

text 	segment	byte public 'CODE'
	assume	cs:text,ds:nothing,es:nothing,ss:nothing


	org	100h
start:
	extrn	init_main:near
	if	hipper_install
	call	init_main		; only returns if new- or re-install
	rep	movsb			; move buffers down over install code
	int	TSR_INT			; terminate stay resident
	else
	jmp	init_main
	endif


;******************************************************************************
;			I N S T A N C E    D A T A
;******************************************************************************

begin_inst_data:

	public	command_template
command_template dd	0
 
; buffer.asm data..

	public	macro_base
	public	macro_last
	public	macro_end
	public	buf_base
	public	buf_back
	public	buf_front
	public	buf_end
	public	buf_current

macro_base	dw	0
macro_last	dw	0
macro_end	dw	0

buf_base	dw	0		; base of command buffer
buf_back	dw	0		; end of data in command buffer
buf_front	dw	0		; start of data in command buffer
buf_end		dw	0		; end of command buffer
buf_current	dw	0		; current command pointer

	public	default_insert

default_insert	db	0

	public	text_ptr
	public	macro_ptr
text_ptr	dw	0		; pointers for output to user
macro_ptr	dw	0

; display.asm data..

	public	num_cols
	public	active_page
	public	cur_col
	public	mark_row
	public	mark_col
	public	rows_advanced
	public	over_cursor
			
num_cols	dw	0
active_page	db	0
cur_col		db	0
mark_row	db	0
mark_col	db	0
rows_advanced	db	0
over_cursor	dw	0

; template.asm data..

	public	template
	public	end_template
	public	output
	public	end_dsp
	public	cur_ptr
	public	oper_flags

end_template	dw	0		; template size passed from command.com
template	db	LINE_LEN	dup (0)
		db	0		; always allow room for a <cr>
output		db	LINE_LEN	dup (0)
end_dsp		dw	0
cur_ptr		dw	0
oper_flags	db	0		; flags for operation

	public	f9_buf,f9_buf_end

f9_buf	db	5 dup (?)
f9_buf_end:

ifdef DBCS
	public	DBCSLeadByteTable
DBCSLeadByteTable	dd	?
endif

	
;	Here's the buffer we use for learning the display size on f7

	public	ansi_buf,d_mode,rows

ansi_buf dw	0
	dw	14		; data length
	dw	?
d_mode	db	?
	db	?
	dw	?
	dw	?
	dw	?
	dw	?
rows	dw	?

end_inst_data:

; Message texts from DOSKEY.  These are messages needed at run time.
; Those needed only for initialization are in INITMSG.INC

	public	more_prompt
	public	num_prompt
include doskey.cl1			; message text for resident

;	We'll hook into the Multiplex Interrupt (2f) in order to give
;	  access between our .COM file and the resident part, and also
;	  so that we can report instance data to win386.

next2f_int	label	dword
	public	old2f
old2f		dw	2 dup (?)

; the table used to report the instance data is given below.  structure
; is understood by win 3.0 and later versions will support it too.
;
	public	w386_instptr
	public	w386_instseg1
	public	w386_instoff2
	public	w386_instseg2
	public	w386_instsize2

w386loadinfo:
w386_ver	db	3,0			
w386_nextdev	dw	2 dup (?)		 
w386_fileptr	dw	2 dup (0)
w386_refdata	dw	2 dup (?)
w386_instptr	dw	offset w386_insttable,?

w386_insttable:
w386_instoff1	dw	offset begin_inst_data
w386_instseg1	dw	?
w386_instsize1	dw	end_inst_data - begin_inst_data
w386_instoff2	dw	?
w386_instseg2	dw	?
w386_instsize2	dw	?

		dw	0
		dw	0


;******************************************************************************
;
;   ourint2f 	 this is our int2f handler.
;   entry: 	 ax = INT2F Multiplex # and Sub-Function.
;   exit:	 If ax = 1605h then es:bx = instance table str. ptr
;		 If ax = 4b05h then es:bx = instance table (for dos switcher)
;		 if ax = 4800h then ax=our_version, es:-> our data area
;		 if ax = 4810h, then do a read command line function
;
;==============================================================================

	public	ourint2f
ourint2f	proc	near

	assume	ds:nothing,es:nothing,ss:nothing

	cmp	ax,1605h   		; check to see if 
	je	w386install		; win386 int 2f.
	cmp	ax,4b05h		; check to see if dos switcher
	je	w386install		;  and treat it the same as windows
	cmp	ah,48h			; DOSKEY private function?
	je	doskey_int2f		; go handle our subfunctions
int2f_notus:
	jmp	next2f_int


;	note that the int2f(4b05h) and int2f(1605h) are not precisely
;	  the same.  the registers for the task switcher callback are
;	  different.  but the registers we care about (es:bx) are the same.

w386install:
	pushf
	call	next2f_int		; this is a cooperative
					; chain.  the buffer returned
	mov	cs:w386_nextdev,bx	; by the next guy is stored
	mov	cs:w386_nextdev[2],es	; in our data.

	push	cs			; return pointer to our 
	pop	es			; buffer
	assume	es:text
	mov	bx,offset w386loadinfo
	iret

doskey_int2f:
	cmp	al,10h			; read console line?
	jnz	not_readconsole		; brif not

;	M002 -- The following code didn't actually work, so it's deleted

;M002	;	we just disable ourselves if STDIN is not the CON device.
;M002	;	  that is because we don't know how to work with external
;M002	;	  terminals on CTTY.
;M002	
;M002		push	ax
;M002		push	bx
;M002		push	dx
;M002		mov	ax,4400h
;M002		mov	bx,1			; IOCTL for STDIN
;M002		int	21h
;M002		test	dl,1			; console input device?
;M002		pop	dx
;M002		pop	bx
;M002		pop	ax
;M002		jz	int2f_notus		; fail the call if not CON dev

;	handle most common case of int2f(48xx) first.

	xchg	bx,dx			; see if length of buffer == 128
	cmp	byte ptr [bx],128
	xchg	bx,dx
	jnz	int2f_notus		; don't handle other buffer lengths

	sti
	call	line_input		; implement line input
	xor	ax,ax			; signal that we handled it
	iret


not_readconsole:
	or	al,al			; is it doskey installation check?
	jnz	int2f_notus		;  back down chain if not
	push	cs
	pop	es
	mov	ax,our_version
	iret

ourint2f	endp


	extrn	fresh_template:near
	extrn	ins_new_line:near
	extrn	display_char:near

line_input	proc	near
	assume	ds:nothing,es:nothing

	push	ax			; save all the registers
	push	bx
	push	cx
	push	dx
	push	si
	push	di

;	we don't use bp, but we supposedly need to preserve it because
;	  some ROM BIOSs trash it on int 10h calls

	push	bp
	push	ds
	push	es
	cld

;	save the template address
 
	mov	word ptr cs:[command_template],dx
	mov	word ptr cs:[command_template.2],ds

	push	cs			; point ds, es -> text segment
	pop	ds
	push	cs
	pop	es
	assume	ds:text,es:text

;	check the command flag and do appropriate thing

	test	[oper_flags],OLDINP	; is there old input to scan?
	jnz	short continue_with_old_input

  	call	fresh_template
;
; transfer data from template to history buffer if it has been edited
;
	mov	si,offset template
	mov	cx,[end_dsp]

;	cas note:  commented out following two lines to implement
;	  OPTION 2 from discussions
;
;	test	[oper_flags],EDITED
;	je	linein_1		; if not edited we don't reset
;					; current line nor do we insert
;					; new one.

	or	cx,cx			; don't insert zero length lines
	jz	linein_1
	call	ins_new_line
linein_1:

;	Okay.  Here we go.  We've got a new line of input from the
;	  user.  If A) the line starts with a macro invocation or
;	  B) the line contains multiple lines with separators, then
;	  we'll resort to a special mode where 1) we just return from
;	  this call with no input so that we get re-prompted, 2) when
;	  we get re-entered, we start spitting out the macro expansion
;	  text and individual lines as executed.

;	see if the stuff in template matches any defined macros

	mov	si,[end_dsp]
	mov	template[si],cr		; store <cr> at end of string
	mov	[text_ptr],offset template ; save scan ptr just in case

	call	check_for_macro_expand	; see if we need to expand a macro
	jc	return_with_empty_command ; brif it set up a macro expansion

	mov	di,[text_ptr]
	mov	cx,[end_dsp]
	mov	al,ctrl_t
	jcxz	no_macro_match_01	; don't scan empty lines
	repnz	scasb			; does the new line have separators?
	jz	return_with_empty_command ; found separator, do special mode

no_macro_match_01:
	les	di,command_template
	assume	es:nothing
	mov	si,offset template
	mov	cx,[end_dsp]
	inc	di
	mov	es:byte ptr [di],cl
	inc	di
	rep	movsb
	mov	al,cr
	stosb
	jmp	line_input_return	; return the line

	assume	es:text
return_with_empty_command:
	or	[oper_flags],OLDINP	; force invocation of saved command
;					;  strings next time we're entered
	les	di,command_template
	assume	es:nothing
	mov	es:word ptr 1[di],0d00h	; zero length, <cr> terminator
	jmp	line_input_return

continue_with_old_input:
	les	di,command_template
	assume	es:nothing
	inc	di			; point to actual data field
	inc	di
	mov	cx,LINE_LEN
	dec	cx			; allow room for separator

	test	[oper_flags],MAC_EXPAND
	jnz	time_to_expand_macro
	jmp	no_macro_to_expand
time_to_expand_macro:

	mov	si,[macro_ptr]		; is there an macro to send?

;	now copy characters out of the macro string

expand_macro:
	lodsb				; next char from macro
	or	al,al			; terminator?
	jz	macro_exp_done
	cmp	al,ctrl_t		; macro string command separator?
	jz	got_macro_sep

	cmp	al,'$'			; macro parameter character?
	jnz	scanning_mac_01		; brif not

	lodsb				; get the next character
	or	al,al			; terminator?
	jz	macro_exp_done		; done if so
	cmp	al,ctrl_t		; string command separator?
	jz	got_macro_sep

	cmp	al,'*'			; $* means use entire command tail
	jz	expand_parm0
	cmp	al,'1'
	jb	scanning_mac_01		; done if below digit-land
	cmp	al,'9'
	jbe	expand_parameter	; go insert macro expansion
scanning_mac_01:
	stosb				; copy one character
	call	display_char		; and echo to screen
	loop	expand_macro

;	we're out of buffer.  Truncate rest of command.

mac_exp_outofdstbuf:
	lodsb				; next char from macro?
	or	al,al			; end?
	jz	macro_exp_done		; brif so
	cmp	al,ctrl_t		; command separator?
	jnz	mac_exp_outofdstbuf	; loop until one or the other
got_macro_sep:
	mov	[macro_ptr],si		; save the macro_scan pointer
	jmp	return_the_buffer_to_caller

macro_exp_done:
	and	[oper_flags],not (MAC_EXPAND or OLDINP)	; macro expander off

;	now see if there was a command separator in the original command tail

	mov	si,[text_ptr]
	mov	cx,[end_dsp]
	jcxz	mac_exp_done_02		; brif no more command tail
mac_exp_done_01:
	lodsb				; can't do scasb cuz es:di is busy
	cmp	al,ctrl_t
	loopnz	mac_exp_done_01		; loop until end or we find one
	jnz	mac_exp_done_02		; brif no command separator

	or	[oper_flags],OLDINP	; turn regular scanning back on
	mov	[text_ptr],si		; save pointers
	mov	[end_dsp],cx

	push	es			; save destination buffer
	push	di
	push	cs
	pop	es			; set es: -> text for macro search
	call	check_for_macro_expand	; does NEXT line have ANOTHER macro?
	pop	di
	pop	es
mac_exp_done_02:
	jmp	return_the_buffer_to_caller

expand_parm0:
	push	si			; save pointer to macro text
	mov	si,[text_ptr]		; get pointer to command tail
	mov	bx,[end_dsp]		; get length of command tail
exp_parm0_01:
	or	bx,bx			; nothing left in command tail?
	jz	expand_parm_done	; brif so
	dec	bx
	lodsb				; scan any blanks at front of tail
	cmp	al,' '
	jz	exp_parm0_01
	cmp	al,tab
	jz	exp_parm0_01

	dec	si			; point back to first non-whitespace
	inc	bx			; adjust count
exp_parm0_01a:
	or	bx,bx			; command tail finished?
	jz	expand_parm_done	; done if so
	dec	bx
	lodsb
	cmp	al,ctrl_t		; command separator?
	jz	expand_parm_done	; don't go any further if so
	stosb				; copy it
	call	display_char		; display it
	loop	exp_parm0_01a		; loop as long as we've got dest buffer
expand_parm_outobuf:
	pop	si			; restore macro text pointer
	jmp	mac_exp_outofdstbuf

expand_parm_done:
	pop	si
	jmp	expand_macro

expand_parameter:
	push	si			; save pointer to macro text
	mov	si,[text_ptr]		; get pointer to command tail
	mov	bx,[end_dsp]		; get length of command tail
	sub	al,'0'			; subtract ascii offset from parm #
	mov	ah,al
expand_parm_01:
	or	bx,bx			; nothing left in command tail?
	jz	expand_parm_done	; done if so
	dec	bx
	lodsb				; get character
	cmp	al,ctrl_t		; command separator?
	jz	expand_parm_done	; done if so
	cmp	al,' '			; scan past if whitespace
	jz	expand_parm_01
	cmp	al,tab
	jz	expand_parm_01

;	now we've got non-whitespace.  check parameter number request.

	dec	ah			; have we found the one we wanted?
	jz	expand_parm_found	; brif so

;	oh well.  Now scan until we find whitespace and loop.

expand_parm_02:
	or	bx,bx			; nothing left in command tail?
	jz	expand_parm_done	; done if so
	dec	bx
	lodsb				; get character
	cmp	al,' '
	jz	expand_parm_01		;  back to outer loop if whitespace
	cmp	al,tab
	jz	expand_parm_01
	cmp	al,ctrl_t
	jz	expand_parm_done	; done if command separator
	jmp	expand_parm_02

expand_parm_found:
	inc	bx			; count back for first character
	dec	si			; we overscanned first character
exp_parm_found_01:
	or	bx,bx			; nothing left in command tail?
	jz	expand_parm_done	; brif so
	dec	bx
	lodsb				; get character from command tail
	cmp	al,' '			; consider whitespace a delimiter
	jz	expand_parm_done
	cmp	al,tab
	jz	expand_parm_done
	cmp	al,ctrl_t		; or command separator
	jz	expand_parm_done
	stosb				; copy it
	call	display_char		; display it
	loop	exp_parm_found_01	; loop as long as we've got dest buffer
	jmp	short expand_parm_outobuf



no_macro_to_expand:
	mov	si,[text_ptr]
	mov	bx,[end_dsp]		; get count remaining
scanning_text:
	or	bx,bx			; have we exhausted count?
	jz	text_exp_done
	dec	bx
	lodsb				; next character from text
	cmp	al,ctrl_t		; command separator?
	jz	got_text_sep		; skip if so
	stosb				; copy it
	call	display_char		; echo to screen
	loop	scanning_text		; loop until we run out of buffer

;	if we run out of buffer, force a fake separator

got_text_sep:
	mov	[text_ptr],si
	mov	[end_dsp],bx		; save the scan pointers
	or	bx,bx
	jnz	 return_the_buffer_to_caller

text_exp_done:
	and	[oper_flags],NOT OLDINP	; force new input next time
return_the_buffer_to_caller:
	mov	al,cr			; store delimiter into buffer
	stosb
	mov	cx,di
	mov	di,word ptr command_template
	sub	cx,di			; get length we actually copied
	sub	cx,3			; less the two count bytes and the <cr>
	mov	es:byte ptr [di][1],cl

;
; restore all the registers
;
line_input_return:
	pop	es
	pop	ds
	pop	bp
	pop	di
	pop	si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	assume	ds:nothing
	ret

line_input	endp
	
;	this subroutine is called when we're about to expand a
;	  command line.  It checks to see if the first thing on
;	  the line is a macro.  If so, it sets up the appropriate
;	  pointers and stuff and sets the MAC_EXPAND and OLDINP bits.
;	  sets carry when macro will be expanded.

check_for_macro_expand	proc	near
	assume	ds:text

	mov	si,[text_ptr]
	call	count_name		; how far to macro delimiter?
	jcxz	no_macro_match		; if line started with delim, no macro

	mov	si,[text_ptr]		; now look that up in macro table
	push	cx			; save macro length
	call	search_macro
	pop	cx
	jc	no_macro_match		; brif search failed

	add	di,cx			; point to end of macro name in table
	inc	di			; point past delimiter
	add	[text_ptr],cx		; advance pointer past macro name
	sub	[end_dsp],cx		; reduce length of string by macro size
	mov	[macro_ptr],di		; save the command tail
	or	[oper_flags],MAC_EXPAND or OLDINP ; set macro-expansion flags
	stc				; set flag indicating macro expansion
	ret
no_macro_match:
	clc				; no macro expansion
	ret
check_for_macro_expand	endp

;	return al=force_upper(al)

	public	ucase
ucase	proc	near
	cmp	al,'a'
	jb	ucas1		; skip if below lower case
	cmp	al,'z'
	ja	ucas1		; skip if above lower case
	and	al,5fh		; force to upper case
ucas1:
	ret
ucase	endp


;	Return the length of macro name at ds:si in cx, return si->delimiter
;	   trash ax

	public	count_name
count_name	proc	near
	xor	cx,cx			; start counting
count_name_size:
	lodsb
	cmp	al,cr			; check for all possible delimiters
	jz	count_name_gotdel
	cmp	al,' '
	jz	count_name_gotdel
	cmp	al,tab
	jz	count_name_gotdel
	cmp	al,'='
	jz	count_name_gotdel
	cmp	al,ctrl_t
	jz	count_name_gotdel
	inc	cx			; count it
	jmp	count_name_size

count_name_gotdel:
	dec	si			; point back to the delimiter
	ret
count_name	endp

;	search macro table for macro named at ds:si, length = cx
;	  assume es:-> resident data.  Return es:di -> match entry
;	  or carry true if fail.  Note:  Alias table always stored
;	  as CAPS, other string non-case-sensitive.
;
;	returns si-> after the matched string if matched

	public	search_macro
search_macro	proc	near
	assume	ds:nothing,es:text
	lodsb				; get first character to match
	call	ucase			; force upper
	mov	ah,al			; save first character in ah, too
	dec	cx			; eat the first character

	mov	di,[macro_base]
sa_01:
	cmp	di,[macro_last]
	jae	sa_08			; fail if we hit end of table

	push	di			; save pointer to string
	scasb				; match first character?
	jnz	sa_02			;  don't do string compare if not

	jcxz	sa_017			; skip if it was a one byte string
	push	si
	push	cx			; save target string and length
sa_015:
	lodsb				; get source string
	call	ucase			; force upper
	scasb				; compare
	loopz	sa_015
	pop	cx
	pop	si			; restore compare string
	jnz	sa_02			; brif no match
sa_017:
	cmp	es:byte ptr [di],0	;  was the length the same?
sa_02:
	pop	di
	jz	sa_09			; success! (carry clear)

sa_03:
	push	cx
	mov	cx,[macro_last]
	sub	cx,di
	xor	al,al
	repnz	scasb			; find two delimiters
	repnz	scasb
	pop	cx
	mov	al,ah			; restore first character to match
	jmp	sa_01			; next entry

sa_08:
	stc
	ret
sa_09:
	add	si,cx			; point si past comparison string
					; guaranteed to leave carry reset
	ret
search_macro	endp

text	ends

	end	start
