page ,132
;******************************************************************************
Title TRANS.ASM - Transient parts of DOSKey
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;******************************************************************************
;
;   (C) Copyright Microsoft Corp., 1989
;
hipper_install	=	0

	include	gendef.inc
	include	versiona.inc
	include dbcs.sw

cgroup	group	text,initseg
text	segment	byte public 'code'
	extrn	macro_base:word
	extrn	macro_end:word
	extrn	macro_last:word

	extrn	buf_base:word
	extrn	buf_front:word
	extrn	buf_back:word
	extrn	buf_end:word
	extrn	buf_current:word

	extrn	default_insert:byte

	extrn	w386_instptr:word
	extrn	w386_instseg1:word
	extrn	w386_instseg2:word
	extrn	w386_instoff2:word
	extrn	w386_instsize2:word

	extrn	old2f:word
	extrn	ourint2f:near

	extrn	dumphist:near
	extrn	newline:near
	extrn	dstring:near
	extrn	display_char:near
	extrn	ucase:near
	extrn	count_name:near
	extrn	search_macro:near

ifdef DBCS
	extrn	DBCSLeadByteTable:dword
endif

text	ends
initseg	segment	byte public 'code'
	assume	cs:cgroup		; allow cs: access to text & initseg

start_of_init_code:
	if	not hipper_install
base_of_buffers:
	endif

zero	dw	0		; segment of interrupt vectors

reinstall_string db	'/REINSTALL',0
bufsize_string	db	'/BUFSIZE',0
dhist_string	db	'/HISTORY',0
dh1_string	db	'/H',0
dmacs_string	db	'/MACROS',0
dm1_string	db	'/M',0
options_string	db	'/?',0
insert_string	db	'/INSERT',0
overstrike_string db	'/OVERSTRIKE',0

REINSTALL	equ	01h
NEW_BUFSIZE	equ	02h
NEW_INSTALL	equ	04h
DHIST		equ	08h
DMACS		equ	10h
INSERT_FLAG	equ	20h
OVERSTRIKE_FLAG	equ	40h

bufsizer	dw	512	       ; DEFAULT BUFFER SIZE

	if	hipper_install
;	init_main will only return in the case that it does
;	  a new installation.  In that case, it returns with
;	  everything all setup for the movsb which moves the
;	  buffer space down over the init code.  Also, it has
;	  dx set up for a TSR_INT
	endif

	public	init_main
init_main	proc	near
	assume	cs:initseg		; tell assembler not to assume
					;  it can get to data through cs:
	push	cs			; point ds:si to our command tail
	pop	ds
	assume	ds:cgroup,es:nothing
	push	cs
	pop	es			; point es into our command strings

	mov	ah,30h			; check DOS version
	int	21h
	cmp	ax,expected_version	; must match version EXACTLY!
	jz	dvers_ok

	mov	dx,offset cgroup:IncorrectDOS
	jmp	error_abort		; abort if wrong DOS

dvers_ok:
	mov	si,81h			; look at command tail

scan_options:
	call	get_nonwhite

	mov	di,offset cgroup:options_string		; /?
	call	noncase_compare
	jz	options_command

	mov	di,offset cgroup:insert_string		; /insert
	call	noncase_compare
	jz	insert_command

	mov	di,offset cgroup:overstrike_string	; /overstrike
	call	noncase_compare
	jz	overstrike_command


	mov	di,offset cgroup:reinstall_string	; /REINSTALL
	call	noncase_compare
	jz	reinstall_command

	mov	di,offset cgroup:dhist_string		; /HIST
	call	noncase_compare
	jz	dhist_command
	mov	di,offset cgroup:dh1_string		; '/H'
	call	noncase_compare
	jz	dhist_command

	mov	di,offset cgroup:dmacs_string		; /MACS
	call	noncase_compare
	jz	dmacs_command
	mov	di,offset cgroup:dm1_string		; '/M'
	call	noncase_compare
	jz	dmacs_command

	mov	di,offset cgroup:bufsize_string		; /BUFSIZE
	call	noncase_compare
	jnz	done_scanning_options	; brif what's there ain't an option

;	scan the /bufsize= option

	call	get_nonwhite		; get past any blanks
	cmp	al,'='
	jz	scanning_bufsize_01
	jmp	bad_syntax		; syntax error if not '='
scanning_bufsize_01:

	call	get_nonwhite_1
	call	scan_decimal_string
	mov	[bufsizer],bx
	assume	cs:cgroup
	or	cs:byte ptr [install_flags],NEW_BUFSIZE
	assume	cs:initseg
	jmp	scan_options

reinstall_command:
	assume	cs:cgroup
	or	cs:byte ptr [install_flags],REINSTALL
	assume	cs:initseg
	jmp	scan_options

insert_command:
	assume	cs:cgroup
	or	cs:byte ptr [install_flags],INSERT_FLAG
	assume	cs:initseg
	jmp	scan_options

overstrike_command:
	assume	cs:cgroup
	or	cs:byte ptr [install_flags],OVERSTRIKE_FLAG
	assume	cs:initseg
	jmp	scan_options


options_command:

;	can't use normal message output here cuz message contains "$"

	mov	bx,offset cgroup:help_info
opt_com:
	mov	al,[bx]
	inc	bx
	or	al,al
	jz	opt_com_done		; null at end

	push	bx
	mov	dl,al
	mov	ah,2
	int	21h			; console out
	pop	bx
	jmp	opt_com

opt_com_done:
	mov	al, 0			; return code
	jmp	normal_return

dhist_command:
	assume	cs:cgroup
	or	cs:byte ptr [install_flags],DHIST
	assume	cs:initseg
	jmp	scan_options

dmacs_command:
	assume	cs:cgroup
	or	cs:byte ptr [install_flags],DMACS
	assume	cs:initseg
	jmp	scan_options

;	okay.  Now we've scanned our options.  Install us if needed,
;	  then fall into a possible macro definition.

done_scanning_options:
	assume	cs:cgroup
	test	cs:byte ptr [install_flags],REINSTALL
	assume	cs:initseg
	jnz	force_reinstall		; do the install, damnit!

	mov	ax,4800h		; Installation check
	int	I2F_INT			; get es:-> resident data if installed
	cmp	ax,4800h		; is ANY DOSKey installed?
	jz	force_reinstall
	jmp	already_installed	;  yep.  go see if it's the same one

;	come here to install us as resident part

force_reinstall:
	push	si			; save command line pointer
	mov	ax,[bufsizer]
	cmp	ax,1+2*LINE_LEN		; minimum size
	jae	got_bufsize_1
	mov	ax,1+2*LINE_LEN		; set size to minimum
got_bufsize_1:

	mov	cx,0ffffh		; figure out max buffer size
	sub	cx,offset cgroup:base_of_buffers
	cmp	ax,cx			; are we over max?
	jbe	got_bufsize_2		; brif okay
	mov	ax,cx
got_bufsize_2:
	push	ax			; save buffer size

;	free the environment

	mov	es,word ptr ds:[02ch]
	mov	ah,49h
	int	21h

;	initialize buffers module.

	mov	ax,offset cgroup:base_of_buffers
	mov	[macro_base],ax
	mov	[macro_last],ax
	mov	[macro_end],ax

	mov	[buf_base],ax
	mov	[buf_front],ax
	mov	[buf_back],ax
	pop	bx
	add	ax,bx
	mov	[buf_end],ax


;	initialize win386 module.

;
; first set vital values in instance table
;
	mov	ax,cs
	mov	[w386_instptr.2],ax	; segment of instance table ptr
	mov	[w386_instseg1],ax	; segment of instance data block
					; in win386.asm
	mov	[w386_instseg2],ax	; segment of buffer data area.

	mov	ax,[macro_base]
	mov	[w386_instoff2],ax 	; offset of buffer data area
	mov	ax,[buf_end]
	sub	ax,[macro_base]
	mov	[w386_instsize2],ax	; size of buffer data area

ifdef DBCS

;	set DBCS Lead Byte Table

	push	ds
	mov	ax,6300h		; get address of DBCS Lead Byte Table
	int	21h
	mov	bx,ds
	pop	ds
	mov	word ptr DBCSLeadByteTable,si
	mov	word ptr DBCSLeadByteTable+2,bx
endif

;	hook us into int2f


	mov	ax,offset ourint2f
	mov	bx,cs
	mov	es,zero
	cli
	xchg	ax,es:word ptr [I2F_INT*4]
	xchg	bx,es:word ptr [I2F_INT*4][2]
	mov	[old2f],ax
	mov	[old2f][2],bx

	sti

	mov	dx,offset cgroup:installed
	call	dsp_msg_crlf

	assume	cs:cgroup
	or	cs:byte ptr [install_flags],NEW_INSTALL
	assume	cs:initseg
	pop	si			; restore command tail pointer
	push	cs			; point es: to text segment
	pop	es
	jmp	short continue_after_install

;	put install_flags down here so they don't get wiped out by
;	  a macro definition on an installation run

install_flags	db	0

already_installed:
	cmp	ax,our_version		; proper version installed?
	mov	dx,offset cgroup:vers_mismatch
	jnz	err_abrt_01
	test	[install_flags],NEW_BUFSIZE
	jz	continue_after_install
	mov	dx,offset cgroup:cant_change_bufsize
err_abrt_01:
	jmp	short error_abort

;	okay.  Now we know we have a resident portion to talk to.
;	  we'll tell the assembler to use es (returned by int 2f) to
;	  access our resident data variables, and invalidate ds

	assume	ds:nothing,es:cgroup

continue_after_install:

	assume	cs:cgroup
	test	cs:[install_flags],INSERT_FLAG
	assume	cs:initseg
	jz	not_in_forced_insert
	mov	[default_insert],-1
not_in_forced_insert:

	assume	cs:cgroup
	test	cs:[install_flags],OVERSTRIKE_FLAG
	assume	cs:initseg
	jz	not_in_forced_overstrike
	mov	[default_insert],0
not_in_forced_overstrike:


	call	get_nonwhite		; scan past any blanks

;	okay.  Now.  si is the start of the macro name.  Find the
;	  length of the name, the start of the definition, and the
;	  length of the definition so we can take the needed space
;	  away from the history buffer.

	mov	di,si			; save start of name string
	call	count_name
	jcxz	no_macro_to_define	;  done if name is zero length
	call	get_nonwhite		; scan whitespace
	cmp	al,'='			; got the required equals?
	jnz	bad_syntax		;  error if not equals
	call	get_nonwhite_1		; scan up to actual definition string

	mov	bx,cx			; get length of name string into bx

;	now convert the definition string for storage and get its length

	call	pack_macro		; pack the macro & count its length

;	now di->name, bx=name length, si->definition, cx=def length

	if	not hipper_install
mustbe_greater_than_128	=	$ - start_of_init_code

;	see that label above us?  there's probably some way of
;	  convincing the assembler to generate an error if an
;	  expression is less than 128, but I'm probably not
;	  going to figure it out at 6:40 a.m.  The reason why
;	  it's a concern is that the macro is going to start
;	  overlaying our initialization code.  the biggest
;	  possible macro is limited by the length of the
;	  invocation line.  128 is a good number to use.
	endif

	call	insert_macro		; create the macro string
	assume	ds:cgroup,es:nothing
	mov	dx,offset cgroup:out_o_macro_msg
	jc	error_abort

no_macro_to_define:
	assume	cs:cgroup
	test	cs:byte ptr [install_flags],DHIST
	assume	cs:initseg
	push	ds
	push	es
	pop	ds			; get ds -> resident data
	jz	no_dhist
	xor	al,al			; no line numbers
	call	dumphist
	call	newline			; put a newline after
no_dhist:
	assume	cs:cgroup
	test	cs:byte ptr [install_flags],DMACS
	assume	cs:initseg
	jz	no_dmacs
	xor	al,al		; no newline the first time
	call	dumpmacs
	call	newline		; put a newline after
no_dmacs:
	pop	ds			; restore our data segment
	mov	al,0			; return code okay
	jmp	short normal_return


bad_syntax:
	mov	dx,offset cgroup:syntax_error
error_abort:
	call	dsp_msg_crlf
	mov	al,1			; abnormal return
normal_return:
	test	[install_flags],NEW_INSTALL
	jnz	must_do_tsr
	mov	ah,4ch			; terminate
	int	21h

must_do_tsr:

	if	hipper_install

;	now we're going to move the buffers down on top of the init
;	  code.  we'll have to adjust a bunch of pointers first.

	mov	ax,offset cgroup:end_of_init_code
	sub	ax,offset cgroup:start_of_init_code

	mov	si,[macro_base]
	sub	[macro_base],ax
	mov	di,[macro_base]
	sub	[macro_last],ax
	sub	[macro_end],ax
	sub	[buf_base],ax
	sub	[buf_front],ax
	sub	[buf_back],ax
	sub	[buf_end],ax

;	we know ds: and es: point to cs: for the movsb
;	  which follows the call to init_main

	mov	cx,[macro_last]
	sub	cx,di			; get length of macro block
	mov	dx,[buf_end]		; end of last buffer
	add	dx,16			; augment by a paragraph
	ret				; return back to low memory
	else
	mov	dx,[buf_end]		; end of last buffer
	add	dx,16			; augmented by a paragraph
	int	TSR_INT
	endif

init_main	endp

;	delete old macro definition (if any) and insert new def (if any)
;	  di->name, bx=name length, si->definition, cx=def length
;	  trash: all, return carry if couldn't get enough room for macro
;	  name must already be checked for zero length
;	Note:  es: must be pointing to our RESIDENT data area.  ds: will
;	  be pointing to all of our names and stuff

insert_macro	proc	near
	assume	es:text,ds:nothing

	push	di			; save name and length
	push	bx

insert_al_01:
	mov	al,ds:byte ptr [di]	; fetch name character
	call	ucase			;  force to upper case
	mov	ds:byte ptr [di],al	;   and store it back
	inc	di
	dec	bx
	jnz	insert_al_01		; loop through entire name

	pop	bx
	pop	di

	push	si
	push	cx
	push	di
	push	bx

	call	delete_macro		; delete the any old definitions

	pop	bx
	pop	di
	pop	cx
	pop	si
	jcxz	insert_macro_okay	; if zero length, we're done

;	do the insertion here

	mov	ax,bx			; get the amount of room we need
	add	ax,cx			; for name and definition
	add	ax,2			; plus two nulls
	mov	dx,[macro_end]		; how much room exists now?
	sub	dx,[macro_last]		; this is the amount of space we have
	sub	ax,dx			; do we need any additional?
	jbe	insert_al_spaceok	; skip if not

	push	ax
	call	shrink_history		; try to get the space from history
	pop	ax
	jc	insert_macro_done	;  exit with carry set if problem
	add	[macro_end],ax		; update macro_end

insert_al_spaceok:
	push	si			; save definition string
	push	cx			; save definition string length
	mov	si,di			; get name string
	mov	cx,bx			; get name string length
	mov	di,[macro_last]
	rep	movsb			; zip it in there.  We already know
					;  we've got enough space.
	xor	al,al			; get a null to store at end
	stosb				;  store null
	pop	cx			; get definition string length
	pop	si			; get definition string
					;  we've already disposed of the case
					;   where cx == 0
	rep	movsb			; copy already-crunched string
	xor	al,al
	stosb				; store null at end
	mov	[macro_last],di		; update end-of-macroes pointer
insert_macro_okay:
	clc				; clear carry - no error
insert_macro_done:
	ret
insert_macro	endp

;	si -> macro definition string.  We must pack it in place.  This
;	  is safe because the packed form NEVER takes more room than the
;	  user-entered form.  We'll return cx == count of string in bytes.
;	  the original string was <cr> terminated.
;
;	bx, si, di & es must be preserved!


pack_macro	proc	near
	push	es			; save resident data segment
	push	di			; preserve macro name pointer
	push	si			; preserve macro def pointer

	push	ds
	pop	es
	mov	di,si			; point es:di to ds:si

pack_macro_0:
	lodsb				; get the character
	cmp	al,cr			; is that the end?
	jz	pack_macro_9		; done if so!

	cmp	al,'$'			; is it a '$'?
	jnz	pack_macro_5		; just copy this character if not '$'

	lodsb				;  get following character
	cmp	al,cr			; handle '$'<cr> case
	jz	pack_macro_9

;	see if it's a $$, $* or $1 - $9.  If so, allow it into the macro.

	cmp	al,'$'
	jz	pack_macro_2
	cmp	al,'*'
	jz	pack_macro_2

	cmp	al,'1'
	jb	pack_macro_3		; brif sub zero
	cmp	al,'9'
	ja	pack_macro_3		; brif super nine

pack_macro_2:
	mov	byte ptr es:[di],'$'	; copy $ + trail character
	inc	di
	jmp	short pack_macro_5	; copy trail character & loop

;	character after '$' wasn't "$*123456789".  It's either
;	  illegal, or a $b, $t, etc...

pack_macro_3:
	call	ucase			; force upper

	mov	ah,'|'
	cmp	al,'B'			; vertical bar?
	jz	pack_macro_4

	mov	ah,'<'
	cmp	al,'L'			; less than?
	jz	pack_macro_4

	mov	ah,'>'
	cmp	al,'G'			; greater than?
	jz	pack_macro_4

	mov	ah,ctrl_t
	cmp	al,'T'			; control t?

;	$<illegal> just gets thrown out.

	jnz	pack_macro_0		; just ignore illegal '$x'

pack_macro_4:
	mov	al,ah
pack_macro_5:
	stosb
	jmp	pack_macro_0

pack_macro_9:
	mov	cx,di			; get pointer to end of new string
	pop	si			; restore macro def pointer
	pop	di			; restore macro name pointer
	pop	es			; restore resident data pointer
	sub	cx,si			; get length of definition string
	ret
pack_macro	endp

;	delete any existing macro matching name at ds:di length bx
;	   trashes si, di, ax, bx, cx
;
;	note:  the recovered space WILL NOT be returned to the history
;	   pool.  It could be with a fairly minimal amount of work.

delete_macro	proc	near
	mov	si,di			; get string pointer in ds:si
	mov	cx,bx
	call	search_macro		; find the macro
	jc	delete_macro_done	; done if no match

	push	di			; save the pointer to macro table
	mov	cx,[macro_last]
	sub	cx,di
	xor	al,al			; scan to end
	repnz	scasb
	repnz	scasb
	mov	si,di
	pop	di			; restore pointer to macro match
	db	26h			; do the es: override
	rep	movsb
;  rep	movs	es:byte ptr [si],byte ptr [di]	; es: seg override
	mov	[macro_last],di
delete_macro_done:
	ret
delete_macro	endp

;******************************************************************************
;
;   shrink_history	Reduce the history buffer size by ax bytes,
;			truncating commands if necessary.
;   entry:		ax = number of bytes to shrink by
;   exit:		Carry set if couldn't shrink that much
;   trashes:		ax
;			sets buf_current to end of history
;   guaranteed preserves:  bx, cx, si, di
;
;==============================================================================

shrink_history	proc	near
	assume	ds:nothing,es:text
	push	cx			; we won't save bx cuz we won't use it
	push	si
	push	di
	push	ds		; save original ds:
	push	es		; copy es: -> ds: (text)
	pop	ds
	assume	ds:text

	mov	di,buf_end
	sub	di,buf_base		; how big is total buffer?
	sub	di,ax			; requested reduction size
	jc	shrink_exit3		; exit with carry if going past zero
	cmp	di,2+(2*LINE_LEN)	; allow for at least two full lines
	jb	shrink_exit3		; exit with carry if hit minimum size

	mov	cx,buf_back
	cmp	cx,buf_front		; split buffer configuration?
	jae	shrink_nosplit

;	We're in a split buffer configuration.  di has our new buffer size.
;	   first of all, look to see if we'll be discarding the entire
;	   2nd half of the buffer.

	sub	cx,buf_base		; get size of bottom segment
	sub	di,cx			; need to look at any of top segment?
	jbe	shrink_split_02		;  brif not, plenty of data in bottom

;	now let's start scanning through data to find the limits of
;	  what we're actually going to keep

	mov	si,buf_end
	sub	si,di			; subtract the amount of room left
;					;  to find the earliest possible
;					;  history we might be able to keep
	cmp	si,buf_front		; can we keep it all?
	jbe	shrink_split_01		;  yep.  all of it!  Just compactify.

;	welp, let's scan for a null to see how much we need to delete.

	mov	cx,di			; get length remaining in top buffer
	mov	di,si
	dec	di			; delete no more if previous was null
	inc	cx
	push	ax
	xor	al,al
	repnz	scasb			; try to find a null
	pop	ax
	jnz	shrink_split_02		;  no data left to keep in top part
	mov	buf_front,di		; do the truncation

;	any data needing deletion has been.  Just squeeze out the gap.

shrink_split_01:
	mov	si,buf_back
	add	buf_back,ax
	mov	di,buf_back
	mov	cx,si
	sub	cx,buf_base
	add	buf_base,ax
	dec	si
	dec	di
	std
	rep	movsb			; move bottom half up into gap
	cld
shrink_exitok:
	mov	ax,[buf_end]
	cmp	ax,[buf_front]
	jnz	shrink_exit1		; done if [buf_front] not at [buf_end]
	mov	si,[buf_base]
	mov	[buf_front],si		; [buf_front] = [buf_base]
	cmp	ax,[buf_back]		; is buffer completely empty?
	jnz	shrink_exit1		; done if not
	mov	[buf_back],si		; set [buf_back] = [buf_base] too
shrink_exit1:
	mov	ax,buf_back
	cmp	ax,buf_front		; is buffer empty?
	jnz	shrink_exit2		; set buf_current to buf_back if not
	xor	ax,ax			; else set it to zero
shrink_exit2:
	mov	buf_current,ax
	clc				; return with carry clear

shrink_exit3:
	pop	ds			; restore caller's ds
	pop	di
	pop	si
	pop	cx
	ret

;	there's no keeper data in the second half.  Change buffer to
;	  non-split configuration and let the normal non-split code
;	  do the job.

shrink_split_02:
	mov	si,buf_base
	mov	buf_front,si		; undo the split

shrink_nosplit:
	add	buf_base,ax		; don't care about old buf_base value

;	non-split configuration.  If the gap at the front is big
;	  enough, we're already done.

	mov	ax,buf_base
	cmp	ax,buf_front
	jbe	shrink_exitok		; done!

;	the next step is to move the data up to the top of the workspace,
;	  then check again for buf_base <= buf_front

	mov	di,buf_end
	mov	si,di
	xchg	si,buf_back		; new buf_back = buf_end
	mov	cx,buf_front
	sub	cx,si			; get size of move
	neg	cx
	dec	di
	dec	si
	std
	rep	movsb			; move the data up
	inc	di
	inc	si
	cld
	mov	buf_front,di		; update buf_front

	mov	si,buf_base
	cmp	si,buf_front
	jbe	shrink_exitok

;	now we've got to scan forward to delete some data to find the new
;	  buf_front

	mov	di,si			; get pointer to keeper data
	dec	di			;  we need not delete any more if
;					;  PREVIOUS byte was a null
;					;  And we KNOW data is there cuz
;					;  the old buf_front is less than
;					;  the new buf_base (si)
	mov	cx,buf_back
	sub	cx,di			; get length to scan
	xor	al,al			; scan for null
	repnz	scasb			; guaranteed to succeed
	mov	buf_front,di
	jmp	shrink_exitok

shrink_history	endp

	assume	ds:nothing

;	use int21(9) to display message at ds:dx

dsp_msg_crlf	proc	near
	mov	ah,9
	int	21h
	mov	dx,offset cgroup:crlf_msg
	mov	ah,9
	int	21h
	ret
dsp_msg_crlf	endp

;	advance si and then scan past any whitespace by falling
;	  into get_nonwhite

get_nonwhite_1	proc	near
	inc	si
get_nonwhite_1	endp

;	scan past any blanks at ds:si
;	  return first non-blank in al, si still points to it

get_nonwhite	proc	near
	lodsb
	cmp	al,tab
	jz	get_nonwhite
	cmp	al,' '
	jz	get_nonwhite
	dec	si
	ret
get_nonwhite	endp

;	compare string at ds:si to string at es:di.  If any mismatch,
;	  return zero flag false and si unchanged.  If match, return
;	  zero true and si pointing past the matched string.
;	  Note:  string at es:di is all alpha caps, zero terminated.
;	  comparison is non-case-sensitive to string at si.
;	  Also note that the string at ds:si need not have a delimiter

noncase_compare	proc	near
	push	si		; save original string start
nc_comp_01:
	cmp	es:byte ptr [di],0
	jz	nc_comp_09	; brif terminator on constant string, done!
	lodsb			; get character
	call	ucase		; force to upper case
	scasb			; compare to target string
	jz	nc_comp_01	; branch while continue to match
	pop	si		; restore original string
	ret			; return with zero false

nc_comp_09:
	pop	ax		; unjunk stack by trashing ax
	ret			; return with zero true
noncase_compare	endp

;	scan up a decimal string at ds:si.  Return the value in bx.
;	  trash ax,cx

scan_decimal_string proc near
	xor	bx,bx		; zero accumulator
scan_decimal_1:
	lodsb
	sub	al,'0'
	cmp	al,10
	jae	scan_decimal_9
	mov	cx,bx
	add	bx,bx
	add	bx,bx
	add	bx,cx
	add	bx,bx		; bx=bx*10
	cbw
	add	bx,ax
	jmp	scan_decimal_1
scan_decimal_9:
	dec	si		; point back to the delimiter
scandec_ret:
	ret
scan_decimal_string endp

;******************************************************************************
;
;       dumpmacs        dump the macro list to console output
;                       suppress first newline
;                       convert "><|^t" back to "$?" equivalent
;
;==============================================================================
 
dumpmacs        proc    near
	assume	ds:text,es:text
	xor	al,al		; set first time through flag
        mov     si,[macro_base]
        cmp     si,[macro_last]
        jz      scandec_ret
 
dmacs_t1:
        call    dstring         ; display a string
        mov     al,'='
        call    display_char
        call    dstringz        ; display the value
	cmp	si,[macro_last]	; done?
	jz	scandec_ret	; skip if so
	call	newline		; force newline between lines
	jmp	dmacs_t1	; and loop until done
 
dumpmacs        endp

;**********************************
;       dstringz: - display zero termianted string at si.  trash al,cx.
;         return si->after null
;	  translate special characters back into "$?" representation
;**********************************
 
dstringz proc   near
 
 
dstringz_1:
        lodsb                   ; get the character
        or      al,al
        jz      scandec_ret
 
        mov     ah,'b'          ; vertical bar
        cmp     al,'|'
        jz      dstringz_subs   ; make the $v substitution
        mov     ah,'g'          ; greater than
        cmp     al,'>'
        jz      dstringz_subs
        mov     ah,'l'
        cmp     al,'<'
        jz      dstringz_subs
        mov     ah,'t'
        cmp     al,ctrl_t
        jz      dstringz_subs
        cmp     al,'$'          ; is it '$'?
        jnz     dstringz_nosub  ; done if not

	lodsb			; and the character after it, which is
	mov	ah,al		;  known to be present after a '$'
dstringz_subs:
        push    ax
        mov     al,'$'
        call    display_char
        pop     ax
        mov     al,ah
dstringz_nosub:
        call    display_char
        jmp     dstringz_1              ; back to loop
 
dstringz endp
 
include doskey.cl2		; text for initialization time

end_of_init_code:
	if	hipper_install
base_of_buffers:
	endif

initseg	ends
	end
