; Copyright 1990 Central Point Software, Inc. 
; All rights reserved.
;--------------------------------------------------------------------
; These are the console I/O procedures for the UnFormat program.
; Written by GWD, 1987-1988.
;
; International support added 5-23-89 GWD.
; Fixed DOS bug (time separator = "." ?!)  5-24-89 GWD.
; Added ALT or SHIFT key pause in PRINTC.  02-16-90  6.0
;
;--------------------------------------------------------------------
;
prog	SEGMENT public
	ASSUME	CS:prog, DS:prog

        include uf_incl.inc
;
	PUBLIC	display, dis_word
	PUBLIC	tab, printc, pr_text, crout, getkey, flushkey
	PUBLIC	getline, uppercase, pr_hex_word, pr_hex_byte
	PUBLIC	pr_dec, red_pointer, ask_for_yes, skipb, ask_trunc
	PUBLIC	pr_decl
	PUBLIC	print_flags, pf_allowed, pf_con_pe, pf_ptime
	PUBLIC	show_progress, show_dir_info, pr_dec2
	PUBLIC	show_date, show_time, show_month, show_year, show_day
	PUBLIC	display_options, look_for_parms, parse_item
	PUBLIC	copy_fname
	PUBLIC	get_country_info, date_control, date_separator
;
	EXTRN	cluster_index:word, cluster_cnt_plus_1:word
	EXTRN	progress:word, hsub_count:word
	EXTRN	curr_fname:byte, options:word, option_table:byte
;
	EXTRN	text_yes:byte, text_yes_length:abs
	EXTRN	delete_char:abs, truncate_char:abs, all_char:abs
	EXTRN	msg_sure:byte
	EXTRN	msg_ask_trunc:byte
	EXTRN	msg_fat_prot:byte, msg_root_prot:byte
	EXTRN	msg_erase_fat:byte, msg_erase_root:byte
	EXTRN	msg_arrow:byte, msg_write_fake:byte
	EXTRN	msg_dir:byte, msg_vol:byte

;	M005 -- made the following into two separate messages

	EXTRN	msg_progress_sing:byte, msg_progress_plural:byte

;
cr	EQU	13
lf	EQU	10
;
;------------------------------------------------------------------
; These control the country-dependant display of dates & times.
; The main module must initialized these by calling
; GET_COUNTRY_INFO (only once!).
;
country_buffer LABEL byte			;A buffer for DOS fct # 38h.
		DB	0,0, 27 DUP (-1)	;Init to -1 to detect DOS 2.x
		DB	0,0,0				;Just in case.
;
country_code	DW	0
date_control	DW	0	;0='m-d-y',  1='d-m-y',  2='y-m-d'.
date_separator	DB	"-"
time_separator	DB	":"
time_in_24hr	DB	0	;0 = 12-hour.  1 = 24-hour.
;
;------------------------------------------------------------------
; On exit: country-dependant info is filled in and AX=country_code.
;
; Only AX is changed.
;
get_country_info PROC NEAR
	push	bx
	push	dx
	cmp	country_code,0
	jnz	gci_end 		;Already got the info.
	xor	bx,bx
	lea	dx,country_buffer
	mov	ax,3800h		;Get country-dependant info from DOS.
	int	21h
	mov	country_code,bx
	mov	ax,word ptr country_buffer+0	;Date/Time format code.
	mov	date_control,ax
	mov	al,country_buffer+11h	;Clock 12hr/24hr control.
	cmp	al,-1			;DOS 3.0+ would change it from -1.
	je	gci_end 		;It's DOS 2.x, so that's all.
	mov	time_in_24hr,al
	mov	al,country_buffer+0Bh
	mov	date_separator,al
	mov	al,country_buffer+0Dh
	cmp	al,"."
	je	gci_end 		;Don't beleive ".", use default colon.
	mov	time_separator,al
gci_end:
	mov	ax,country_code
	pop	dx
	pop	bx
	ret
get_country_info ENDP
;
;------------------------------------------------------------------
tab	PROC NEAR	;Move to column AL on the current line.
	push	ax
	cmp	al,79
	jbe	tab1
	mov	al,79
tab1:	sub	al,column	;(Requested_column - current_column)
	jbe	tab_done	;Already there or beyond.  Do nothing.
	mov	ah,al
	mov	al," "
tab_lp: call	printc
	dec	ah
	jnz	tab_lp
tab_done:
	pop	ax
	ret
tab	ENDP
;
column	DB	0
;
;------------------------------------------------------------------
; Output the character in AL.	On exit: all regs unchanged.
;
; If PRINT_FLAGS contains the proper value, chars are echoed to LPT1.
;
print_flags DB	0	;Bit flags,  definitions are below.
;
	DB	0	;Extra 0 byte - option parser expects a Word var.
;
pf_allowed EQU	1	;Printing is possible (cmd line option).
pf_con_pe EQU	2	;Console has toggled printing ON.
pf_ptime EQU	80h	;Printable output is now occuring.
;
; Note - printing will actually occur only when PRINT_FLAGS = PF_ACTIVE.
;
pf_active EQU	pf_allowed + pf_con_pe + pf_ptime
;
;
; If RED_POINTER is NZ, PRINTC will write the char to CS:[RED_POINTER],
; But while RED_POINTER is 0, characters are sent to the screen.
;
red_pointer DW	0	;Redirection pointer.
;
printc	PROC NEAR
	cmp	cs:red_pointer,0	;Are we redirected to memory?
	jnz	prc_redirect		;Yes.  Display/print nothing.
	cmp	al," "
	jb	prc_ctrl

prc_inc:
	inc	cs:column

prc_go: push	ax
	PUSH	DX
;	push	bx
;	mov	bx,7
;	mov	ah,0Eh
	MOV	AH,2
	MOV	DL,AL
	INT	21H
;	int	10h		;Video BIOS 'teletype' output.
;	pop	bx
	POP	DX
	pop	ax
	cmp	cs:print_flags, pf_active
	je	prc_printer

prc_done:
	ret
;
prc_ctrl:
	cmp	al,7
	je	prc_go
	cmp	al,lf
	je	prc_go
	cmp	al,cr
	je	prc_cr
	cmp	al,8
	jne	prc_inc
	sub	cs:column,1
	jae	prc_go

prc_cr: mov	cs:column,0
	push	ax

prc_wait:
	sti
	mov	ah,2		;Get Bios keyboard shift states.
	int	16h
	sti
	test	al,0100B	;Ctrl is pressed?
	jnz	prc_end_wait
	test	al,1011B	;Alt or Shift is pressed?
	jnz	prc_wait

prc_end_wait:
	pop	ax
	jmp	prc_go

prc_printer:
	push	ax
	push	dx
	cmp	al,7
	je	prc_px		;Don't send Bell to LPT1.
	xor	dx,dx		;Select LPT1.
	push	bx		;Cover bug in stupid spooler.
	mov	ah,0		;Command = print.
	int	17h
	pop	bx
	test	ah,1			;Any error printing?
	jz	prc_px			;No.
	and	cs:print_flags, NOT pf_con_pe	;Error.  Shut printing off.

prc_px: pop	dx
	pop	ax
	jmp	prc_done
;
prc_redirect:
	xchg	di,cs:red_pointer
	mov	cs:[di],al		;Write character to memory.
	inc	di
	xchg	cs:red_pointer,di
	jmp	prc_done

printc	ENDP
;
;-------------------------------------------------------------
; Display the asciiz string at CS:DX.
;
; On exit: all regs unchanged.
;
pr_text PROC NEAR
	push	ax
	push	si
	mov	si,dx

prx_lp: mov	al,cs:[si]
	inc	si
	cmp	al,0
	jz	prx_done
	call	printc
	jmp	prx_lp

prx_done:
	pop	si
	pop	ax
	ret
pr_text ENDP
;
;----------------------------------------------------------
crout	PROC NEAR
	push	ax
	mov	al,cr
	call	printc
	mov	al,lf
	call	printc
	pop	ax
	ret
crout	ENDP
;
;--------------------------------------------------------------------
;  GET ONE KEY.
;
; On exit: CF=true for ESC, Ctrl-C, Ctrl-Brk.  Otherwise CF=false.
;
getkey	PROC NEAR
	mov	ah,0		;Get one key, wait if none ready.
	int	16h
	cmp	ax,011Bh	;Esc key?
	je	getk_brk
	cmp	al,3		;Ctrl-C ?
	je	getk_brk
	or	ax,ax		;Ctrl-Break ?
	jnz	getk_exit	;No, CF=false.
getk_brk:
	mov	ah,1
	int	16h		;Flush.
	jz	getk_brk2
	mov	ah,0
	int	16h
	jmp	getk_brk
getk_brk2:
	xor	ax,ax
	stc
	jmp	short getk_exit
getk_exit:
	ret
getkey	ENDP
;
;------------------------------------------------------------------------
; Flush the keyboard buffer.
; Ctrl-S causes a pause until a different key is pressed.
;
; On exit: If ESC, Ctrl-C or Break pressed, then CF=true.
;
;
flushkey PROC NEAR	;If Break/Esc/Ctrl-C then CF=true, else CF=false.
	push	ax	;Only flags are changed.
	push	bx
	xor	bx,bx
flush_lp:
	mov	ah,1
	int	16h
	jz	flush_exit	;No keys in the buffer.
flush2: call	getkey
	lahf			;Save flags into AH.
	or	bl,ah		;Accumulate CF image.
	cmp	al,13h		;Ctrl-S ?
	je	flush_s
	cmp	al,10h		;Ctrl-P ?
	je	flush_p
	cmp	ax,7200h	;Ctrl-PrtSc ?
	je	flush_p
	jmp	flush_lp	;Repeat until empty.
flush_s:
	mov	ah,1		;Wait for another key.
	int	16h
	jz	flush_s
	cmp	al,13h		;Another Ctrl-S ?
	jne	flush2		;No.
	mov	ah,0		;Eat it.
	int	16h
	jmp	flush_s 	;Wait for a different key.
flush_p:
	test	cs:print_flags, pf_allowed	;Printout option enabled?
	jz	flush_lp			;No - ignore it.
	xor	cs:print_flags, pf_con_pe	;Toggle state.
	jmp	flush_lp
flush_exit:
	mov	ah,bl
	sahf			;Return CF according to results.
	pop	bx
	pop	ax
	ret
flushkey ENDP
;
;------------------------------------------------------------------------
;  Get a line from the console.
;
; Characters accepted are echoed onto the screen via PRINTC.
; Special keys: backspace key works;
; Ctrl-Brk, ^C or ESC will return CF=true.  ^P toggles printing (if enabled).
;
; On entry: nothing.
;
; On exit: SI points at 1st character of line,
;	  line_length (excluding CR) is at byte ptr [SI-1] and in AH reg,
;	  and AL=1st character (in uppercase, 0 if empty).
;	  ZF=true for empty buffer, unless carry is set by ESC or Ctrl-Break.
;
; Only AX and SI are modified.
;
getline PROC NEAR
	push	bx
	lea	si,getline_buffer
	xor	bx,bx
getl_top:
	call	getkey
	jnc	getl_not_break	;Break or ESC?
getl_break:
	xor	bx,bx		;Esc/Ctrl-Break returns with empty buffer.
	mov	[si-1],bx
	push	dx
	lea	dx,msg_break	;"^c",13,10
	call	pr_text
	pop	dx
	stc
	jmp	short getl_exit
getl_p: test	print_flags, pf_allowed ;Printing allowed?
	jz	getl_beep		;No.
	xor	print_flags, pf_con_pe
	jmp	getl_top
getl_not_break:
	cmp	al,10h		;Ctrl-P ?
	je	getl_p
	cmp	ax,7200h	;Ctrl-PrtSc ?
	je	getl_p
	or	al,al
	jnz	getl3		;If not valid ASCII, beep and discard it.
getl_beep:
	mov	al,7		;'Bell' char.
	call	printc
	call	flushkey
	jc	getl_break
	jmp	getl_top	;Try again.
getl3:	cmp	ax,1C0Dh	;Enter key?
	je	getl_cr
	cmp	ah,0Eh		;Backspace key?
	je	getl_bs
	cmp	al," "
	jl	getl_beep	;Reject other control chars and any AL > 127.
	cmp	bx,18		;Buffer nearly full?
	jae	getl_beep
	mov	[bx+si],al	;Put the character into the buffer.
	inc	bx		;Increment line length counter.
	call	printc		;Echo the typed key onto the screen.
	cmp	bx,1		;1st char entered?
	jne	no_cleol
	push	dx
	lea	dx,msg_cleol	;Just 5 blanks, not really to end-of-line.
	call	pr_text
	pop	dx
no_cleol:
	jmp	getl_top	;Get another char.
getl_bs:
	or	bx,bx		;Is buffer already empty?
	jz	getl_top	;Buffer is empty, ignore Backspace key.
	dec	bx
	mov	al,8		;Back up the cursor.
	call	printc
	mov	al," "
	call	printc		;Blank the deleted character on the screen.
	mov	al,8		;Back up again.
	call	printc
	jmp	getl_top
getl_cr:
	call	crout
	mov	[bx+si],bh	;Put 0 into buffer without incr BX.
	mov	[si-1],bl	;Store line length.
	mov	ah,bl
	mov	al,[si]
	call	uppercase
	or	ah,ah		;Set/clear ZF, always clear CF.
getl_exit:
	pop	bx
	ret
getline ENDP
;
msg_cleol DB	"     ",8,8,8,8,8,0	;5 blanks, 5 backspaces.
msg_break DB	"^c",13,10,0
	DB	0
getline_buffer DB "  GETLINE BUFFER   ",0
;
;----------------------------------------------------------------------
uppercase PROC NEAR
	cmp	al,"a"
	jb	uc
	cmp	al,"z"
	ja	uc
	sub	al,20h
uc:	ret
uppercase ENDP
;
;------------------------------------------------------------------
; Scan a string, skipping over blanks to the next non-blank char.
;
; On entry: DS:SI points to the string to be scanned.
; On exit: SI points at 1st nonblank char found,
;	   AL=[SI] (converted to uppercase).
;	   If AL contains a contrl char, then CF=true.
;
; AL is always altered, SI is usually altered.
;
skipb	PROC NEAR
	mov	al,[si]
	call	uppercase
	cmp	al," "
	jne	skipb_done
	inc	si
	jmp	skipb
skipb_done:
	ret
skipb	ENDP
;
;------------------------------------------------------------------
; This routine displays a hexadecimal string from AX.
; Exactly four digits are always printed.  'PRINTC' is called.
; on entry: AX=value to be converted.
; on exit: all regs preserved.
;
pr_hex_word PROC NEAR
	xchg	ah,al
	call	pr_hex_byte
	xchg	al,ah
	call	pr_hex_byte
	ret
pr_hex_word ENDP
;
;------------------------------------------------------------------
; This routine displays a hexadecimal string from AL.
; Exactly two digits are always displayed.  'PRINTC' is called.
; on entry: AL=value to be converted.
; on exit: all regs preserved.
;
pr_hex_byte PROC NEAR
	push	cx
	mov	cx,0204h	;CH=digit count, CL=shift count.
	push	ax
prhxb:	pop	ax
	rol	al,cl		;Bring high nibble into lower half of AL.
	push	ax		;Save rotated version.
	and	al,0Fh		;Leave only lowest 4 bits.
	cmp	al,9		;If 9 or less, it's easy.
	jbe	prhxb_ok9
	add	al,7		;Otherwise we must add 7 to make it A-F
prhxb_ok9:
	add	al,30h		;ASCII offset.
	call	printc		;Display the character in AL.
	dec	ch
	jnz	prhxb
	pop	ax		;After 8 rotations AL is restored.
	pop	cx
	ret
pr_hex_byte ENDP
;
;-------------------------------------------------------------
;  PRINT A DECIMAL NUMBER
; This converts the binary value in AX to a decimal string.
; The string will be at least one digit, with leading zeros supressed.
; The string is built on the stack.  The routine 'PRINTC' is called.
; 'PRINTC' must display the char in AL and preserve all registers.
; On entry: AX=value to be converted.  On exit: all regs preserved.
;
pr_dec	PROC NEAR
	push	ax
	push	bx
	push	dx
	push	di
	push	bp
	sub	sp,8	;Eight bytes.
	mov	bp,sp
	mov	di,6
	mov	byte ptr [bp+di],0	;String terminator.
	mov	bx,000Ah		;we're going to be dividing by ten.
ddigit: xor	dx,dx
	div	bx		;Remainder = the rightmost digit.
	xchg	al,dl
	add	al,30h		;Convert to ASCII numeric char.
	dec	di
	mov	[bp+di],al	;Store the character.
	mov	al,dl		;Restore AX to be the quotient.
	or	ax,ax
	jnz	ddigit		;If quotient is not yet zero, then repeat.
ddlp:	mov	al,[bp+di]
	inc	di
	cmp	al,0
	jz	dd_done
	call	printc
	jmp	ddlp
dd_done:
	add	sp,8		;Discard stack frame.
	pop	bp
	pop	di
	pop	dx
	pop	bx
	pop	ax
	ret
pr_dec	ENDP
;
;--------------------------------------------------------------------
; Display a 32-bit number as a right-justified,
; nine-character decimal string.
;
; On entry: DX,AX = number (DX = high part).  Max value = 655 million.
;
; On exit: all regs preserved.
;
pr_decl PROC NEAR
	push	ax
	push	bx
	push	cx
	push	dx
	mov	cx,10000
	cmp	dx,cx
	jb	prdl_ok
	mov	ax,999
	call	pr_dec		;Display 999999999 (nine of 'em).
	call	pr_dec
	jmp	short prdl_3
prdl_ok:
	div	cx		;Would overflow if nmbr > 655 Million.
	or	ax,ax		;AX = quotient, DX = remainder (<10,000).
	jnz	prdl_big
	mov	bl," "
	call	prefix_dec
	jmp	short prdl_lo
prdl_big:
	mov	bl," "
	call	prefix_dec
	call	pr_dec		;Print the high part.
	mov	bl,"0"
prdl_lo:
	mov	ax,dx		;Get low part.
	call	prefix_dec
prdl_3: call	pr_dec
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
pr_decl ENDP
;
;------------------------------------------------------------
; Generate enough leading characters to keep
; a decimal number string right-justified.
;
; On entry: BL="0" and AX<10000 for a field width of 4.
;	    BL=" " and AX=anything for a field width of 5.
;
; All regs perserved.
;
prefix_dec PROC NEAR
	push	ax
	push	cx
	xor	cx,cx
	cmp	bl," "
	jne	pref_2
	cmp	ax,10000	;Number might be as large as 65535.
	adc	cl,ch
pref_2: cmp	ax,1000 	;CF=true if AX is below 1000.
	adc	cl,ch		;CH=0.
	cmp	ax,100
	adc	cl,ch
	cmp	ax,10
	adc	cl,ch		;CX = # of leading zeros needed.
	jcxz	pref_end
	mov	al,bl
pref_lp:
	call	printc
	loop	pref_lp
pref_end:
	pop	cx
	pop	ax
	ret
prefix_dec ENDP
;
;------------------------------------------------------------------
;  DISPLAY A COMPLEX MESSAGE.
;
; On entry: [DX]= ASCIIZ message text (ends with a 00h byte).
; On exit: all regs preserved.
;
; Some special command 'macros' are allowed inside the text:
;   @vf
; where v selects the source variable and f selects the format.
;
; Possible values for 'v' are:
;
; 0		value is taken from the input AX register.
; 1 thru 9	takes value from DIS_WORD + (2*number).
;
; Supported 'f' format selectors (must be lower case) are marked * below:
;
; *	b	2 hexadecimal digits from low Byte of variable.
; *	w	4 hex digits from Word.
; *	a	Ascii character from low byte (except byte=0 is discarded).
; *	d	Decimal (word, 1 to 5 columns).
; *	r	Right-justified decimal word (5 columns, leading blanks).
; *	l	right-justified Long (dword) decimal (9 columns).
;M005  delete "s" option
; *	s	pluralize.  I.e., output an 's', unless variable=1.
; *	t	pointer to asciiz Text string (no macros).
;
; To display an @-sign, use "@@".  Bad or unsupported 'macros'
; will appear verbatim in the line, so everyone can see the mistake.
;
; Special cases: @0l takes low word from AX, high word from var #1.
;		 @9l will not work right.
;
; Example:	"There were @1d file@1s found on drive @2a:",cr,lf,0
;
; Assuming that variable #1 contains 7, and var #2 contains "B" (XX42h),
; the displayed message will look like this:
;
;		There were 7 files found on drive B:
;
;
	EVEN
dis_word LABEL word
	DW	0		;Input AX is saved here.
	DW	9 DUP (0)	;Other input variables.
;
; Caution!  Do not attempt to store into DIS_WORD+20 !
;
display PROC NEAR
	push	ax
	push	bx
	push	si
	mov	dis_word,ax		;Save input AX into item #0.
	mov	si,dx
dsp_lp: mov	al,[si]
	inc	si
	cmp	al,"@"
	je	dsp_special
	cmp	al,0
	je	dsp_done
dsp_1:	call	printc
dsp_more:
	jmp	dsp_lp
dsp_done:
	pop	si
	pop	bx
	pop	ax
	ret
dsp_special:
	mov	ah,[si] 	;Fetch char after the "@".
	or	ah,ah
	jz	dsp_1		;Print "@", let the 0 end this proc.
	inc	si
	cmp	ah,al		;Double @-sign?
	je	dsp_1		;Just display the desired single "@".
	mov	al,[si] 	;Fetch 2nd char after "@".
	cmp	al,0
	jz	dsp_err 	;Show error and let 0 end.
	inc	si
	mov	bl,ah		;Now BL holds 1st char after "@".
	mov	bh,0
	sub	bl,"0"
	jb	dsp_err
	cmp	bl,9
	ja	dsp_err
	shl	bx,1
	mov	bx, dis_word [bx]
dsp_fmt:
	cmp	al,"b"
	je	dsp_byte
	cmp	al,"w"
	je	dsp_word
	cmp	al,"a"
	je	dsp_ascii
;M005	cmp	al,"s"
;M005	je	dsp_plural
	cmp	al,"t"
	je	dsp_text
	cmp	al,"d"
	je	dsp_decimal
	cmp	al,"r"
	je	dsp_decimal_rj
	cmp	al,"l"
	je	dsp_long
dsp_err:
	push	ax		;Save the bad character(s) of the macro.
	mov	al,"@"
	call	printc		;Display the usually invisible "@".
	pop	ax
	xchg	al,ah		;Show first character after the "@".
	call	printc
	xchg	ah,al
	jmp	dsp_1		;Display 2nd char and continue.
;
dsp_byte:
	xchg	ax,bx		;Use XCHG, not MOV, to save space.
	call	pr_hex_byte
	jmp	dsp_more
;
dsp_word:
	xchg	ax,bx
	call	pr_hex_word
	jmp	dsp_more
;
dsp_ascii:
	xchg	ax,bx
	cmp	al,0
	jz	dsp_more_stp	;Ignore null bytes.
	call	printc
dsp_more_stp:
	jmp	dsp_more
;
;M005dsp_plural:
;M005	cmp	bx,1		;Exactly one?
;M005	je	dsp_more_stp	;If so, it's singular.  Do nothing.
;M005	mov	al,"s"
;M005	jmp	dsp_1
;
dsp_text:
	push	dx
	lea	dx,[bx]
	inc	bx
	cmp	bx,2		;Pointer is FFFFh or 0?
	jb	dsp_text0	;Impossible value.  Print nothing.
	call	pr_text
dsp_text0:
	pop	dx
	jmp	dsp_more_stp
;
dsp_decimal:
	xchg	ax,bx
	call	pr_dec
	jmp	dsp_more_stp
;
dsp_decimal_rj:
	xchg	ax,bx
	mov	bl," "
	call	prefix_dec
	call	pr_dec
	jmp	dsp_more_stp
;
dsp_long:
	push	dx
	mov	al,ah
	mov	ah,0
	sub	al,"0"
	shl	ax,1
	xchg	ax,bx
	mov	dx,dis_word [bx+2]	;Must fetch the second word.
	call	pr_decl
	pop	dx
	jmp	dsp_more_stp
display ENDP
;
;--------------------------------------------------------------------
; On entry: nothing.
; On exit: If user types in "YES" or "yes", then ZF=true, else ZF=false.
;	   SI always returns address of getline buffer.
;
; Only SI is changed.
;
ask_for_yes PROC NEAR
	push	ax
	push	cx
	push	dx
	push	es
	push	di
	lea	dx,msg_sure	;"Are you SURE?  Type in YES."
	call	pr_text
	call	flushkey
	jc	askfy_esc
	call	getline
	jc	askfy_esc
	push	cs
	pop	es		;Make darn sure ES=CS.
	cld
	lea	di,text_yes
	mov	cx,text_yes_length
askfy_cmp:
	lodsb
	call	uppercase	;Accept either upper or lowercase.
	scasb
	loope	askfy_cmp
	clc
askfy_done:
	lea	si,getline_buffer
	pop	es
	pop	di
	pop	dx
	pop	cx
	pop	ax
	ret
askfy_esc:
	or	al,-1
	stc
	jmp	askfy_done
ask_for_yes ENDP
;
;----------------------------------------------------------------
; Ask user "Delete (or Truncate) this one (or All)?"
; The initials of the first one or two words entered
; will be examined.  First word: only "D" or "T" are allowed.
; Second word (if present) must begin with "A".  If improper
; text (or null) is entered, the user will be prompted again.
; Correct examples:
;	delete
;	d
;	truncate
;	delete all
;	d a
;	TRUNCATE ALL
;	 txxx	  ayyy
;
; ** Note: the "D", "T" and "A" above can all be translated now.
;
; On entry: nothing.
;
; On exit: AL will contain (never translated)
;	"D" = delete this one file, ask again for next.
;	"d" = delete all such files, don't ask again.
;	"T" = truncate (shorten) this file, ask again.
;	"t" = truncate all such files, don't ask again.
;	But if ESC or ctrl-C are pressed, CF=true and AX=?
;
; The intent is that our variable FRAG_OPT should initially be "?".
; The above chioce of values allows quick tests elsewhere:
; Ascii value < "d" means we must ask the question.
;
; Only AX is changed.
;
ask_trunc PROC NEAR
	push	bx
	push	dx
	push	si
askt_1: lea	dx,msg_ask_trunc	;External.
	call	pr_text
	call	getline
	jc	askt_exit
	jz	askt_1		;Null line is not allowed.  Ask again.
	call	skipb		;Returns 1st nonblank in AL (uppercase).
	jc	askt_1		;Found only final 0 - ask again.
	mov	bl,"D"
	cmp	al,delete_char
	je	askt_next_wd
	mov	bl,"T"
	cmp	al,truncate_char
	je	askt_next_wd
	jmp	askt_1
askt_next_wd:			;Find the 2nd text word (if any).
	inc	si
	mov	al,[si]
	cmp	al," "
	jb	askt_done	;No more.
	jne	askt_next_wd	;It's not a blank.  Get next char.
	call	skipb		;Skip blanks to next word.
	jc	askt_done
	call	uppercase
	cmp	al,all_char	;"A" for 'all'?
	jne	askt_1		;Invalid, ask again.
	add	bl,20h		;Convert to lowercase (do it for ALL files).
askt_done:
	mov	al,bl
	clc
askt_exit:
	pop	si
	pop	dx
	pop	bx
	ret
ask_trunc ENDP
;
;----------------------------------------------------------
; Display (screen only) the percentage scanned.
; Destroys AX,BX,DX.
;
show_progress PROC NEAR
	mov	ax,hsub_count
	mov	dis_word+(2*1),ax
	mov	ax,100
	mul	cluster_index
	mov	bx,cluster_cnt_plus_1
	dec	bx
	div	bx		;AX = 100 * index / total.
	cmp	ax,progress	;Any change in the percentage completed?
	jbe	no_progress	;No change.  Don't display.
	mov	progress,ax
	mov	dl,pf_ptime
	not	dl
	and	print_flags, dl ;Make sure this doesn't go to LPT1.

;	M005 -- made the following use two separate messages for
;	        singular/plural

	lea	dx,msg_progress_sing ;"@0d% searched, @1d subdir found.",cr
	cmp	hsub_count,1
	jz	show_prog_sing
	lea	dx,msg_progress_plural ; same message, but plural subdirs
show_prog_sing:

;	M005 -- end changes

	call	display
	or	print_flags, pf_ptime
no_progress:
	ret
show_progress ENDP
;
;----------------------------------------------------------------------
; Display info from a directory entry: Name, Size, Date, Time.
; FILENAME.EXT 123456789 mm-dd-yy HH:MMpm   (+ two blanks, no CRLF).
; VOLUME_N.AME	<VOL>	 mm-dd-yy HH:MMam
;
; On entry: ES:DI points to a directory entry.
; On exit: all regs preserved.
;
show_dir_info PROC NEAR
	push	ax
	push	bx
	push	cx
	push	dx
	lea	bx,curr_fname
	call	copy_fname	;From ES:DI to CS:BX.
	lea	dx,[bx]
	call	pr_text
	mov	al,13
	call	tab
	mov	al, es:[di].file_attr
	test	al, dir_attr OR vol_attr
	jz	shdi_size
	lea	dx,msg_dir	;"<DIR>" instead of size.
	test	al, vol_attr
	jz	shdi_attr
	lea	dx,msg_vol	;"<VOL>".
shdi_attr:
	call	pr_text
	jmp	short shdi_date
shdi_size:
	mov	ax,es:[di].file_size
	mov	dx,es:[di].file_size+2
	call	pr_decl
shdi_date:
	mov	al," "
	call	printc
	mov	ax,es:[di].date 	;Date into AX.
	call	show_date
	mov	al," "
	call	printc
	mov	ax,es:[di].time 	;Time into AX.
	call	show_time
	mov	al," "
	call	printc
	call	printc
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret
show_dir_info ENDP
;
;------------------------------------------------------------
; Display a date & time like this (US format): "mm-dd-yy"
; Exactly eight characters are output.
;
; On entry: AX = date (in DOS dir format).
;
; Destroys AX,BX,CX,DX.
;
show_date PROC NEAR
	mov	dx,ax		;Date into DX (sub-procs expect it there).
	mov	bh,date_separator
	mov	ax,date_control
	cmp	ax,1
	je	shdate_europe
	cmp	ax,2
	je	shdate_japan
	mov	bl," "
	call	show_month	;US format.
	mov	al,bh
	call	printc
	mov	bl,"0"
	call	show_day
	mov	al,bh
	call	printc
	call	show_year
	jmp	short shdate_end
shdate_europe:
	mov	bl," "
	call	show_day
	mov	al,bh
	call	printc
	mov	bl,"0"
	call	show_month
	mov	al,bh
	call	printc
	call	show_year
	jmp	short shdate_end
shdate_japan:
	call	show_year
	mov	al,bh
	call	printc
	mov	bl,"0"
	call	show_month
	mov	al,bh
	call	printc
	call	show_day
shdate_end:
	ret
show_date ENDP
;
;------------------------------
show_month PROC NEAR		;Input: DX=date, BL=prefix.
	mov	ax,dx
	and	ax,0000000111100000B	;Extract the month.
	mov	cl,5
	shr	ax,cl
	call	pr_dec2
	ret
show_month ENDP
;
;------------------------------
show_day PROC NEAR
	mov	ax,dx
	and	ax,0011111B		;Day.
	call	pr_dec2
	ret
show_day ENDP
;
;------------------------------
show_year PROC NEAR
	mov	ax,dx
	mov	cl,9
	shr	ax,cl			;AX= 0-199 (for 1980-2099).
	add	ax,80
	cmp	ax,100			;Beyond 1999?
	jb	sh_yr2
	sub	ax,100			;For year 2000-2099, show 00-99.
sh_yr2: mov	bl,"0"
	call	pr_dec2 		;Year.
	ret
show_year ENDP
;
;----------------------------------------------------
; Display time like this (US format):	"HH:MMam"
; In 24-hour format it looks like this: " HH:MM "
; Exactly seven characters are output.
;
; On entry: AX = time (in DOS 16-bit format).
;
; Destroys AX,BX,CX,DX.
;
show_time PROC NEAR
	mov	dx,ax			;Save time into DX.
	mov	cl,11
	mov	ax,dx
	shr	ax,cl			;Extract hours.
	mov	ch," "			;Assume 24-hr format.
	test	time_in_24hr,1
	jz	shtime12		;12-hour format.
	xchg	bx,ax			;Save AX.
	mov	al," "
	call	printc			;One leading blank in 24-hour format.
	xchg	ax,bx			;Recover AX.
	jmp	short shtime_hrs2	;Leave it in 24-hour format.
shtime12:
	mov	ch,"a"			;Assume AM.
	or	ax,ax
	jnz	shtime_hrs
	mov	al,12			;0 = midnight = 12am.
	jmp	short shtime_hrs2
shtime_hrs:
	cmp	al,12
	jb	shtime_hrs2
	mov	ch,"p"			;PM.
	je	shtime_hrs2		;Exactly 12pm.
	sub	al,12			;Adjust for 1pm to 11pm.
shtime_hrs2:
	mov	bl," "
	call	pr_dec2
	mov	al,time_separator
	call	printc
	mov	ax,dx
	and	ax,0000011111100000B	;Extract minutes.
	mov	cl,5
	shr	ax,cl
	mov	bl,"0"
	call	pr_dec2
	mov	al,ch			;" " or "a" or "p".
	call	printc
	cmp	al," "			;In 24-hour format?
	je	shtime_end		;Yes.
	mov	al,"m"			;Add an M after the A or P.
	call	printc
shtime_end:
	ret
show_time ENDP
;
;----------------------------------------------------------------
; Display a decimal number.  If # digits < 2, then prefix the
; number with one character taken from BL (usually "0" or " ").
; This right-justifies numbers less than 100.
;
; On entry: AX=number, BL=char to prefix.
;
; On exit: All regs preserved.
;
pr_dec2 PROC NEAR
	cmp	ax,10
	jae	prd2
	xchg	ax,bx
	call	printc	;Prefix a "0" or " " to a single digit number.
	xchg	bx,ax
prd2:	call	pr_dec
	ret
pr_dec2 ENDP
;
;---------------------------------------------------------------
; Display the options (if any).  /ER /EF /KR /KF /TEST
; AX,DX destroyed.
;
display_options PROC NEAR
	mov	ax,options
	test	options, opt_wrfake + opt_k + opt_erase_fat + opt_erase_root
	jz	disp99
	call	crout
	test	ax,opt_wrfake
	jz	disp_keep
	lea	dx,msg_write_fake
	call	pr_text
disp_keep:
	test	ax,opt_keep_fat OR opt_keep_root
	jz	disp_erase
	lea	dx,msg_fat_prot
	test	ax,opt_keep_fat
	jnz	dispk2
	lea	dx,msg_root_prot
dispk2: call	pr_text
disp_erase:
	test	ax,opt_erase_root OR opt_erase_fat
	jz	disp99
	lea	dx,msg_erase_fat
	test	ax,opt_erase_fat
	jnz	dispe2
	lea	dx,msg_erase_root
dispe2: call	pr_text
disp99: ret
display_options ENDP
;
;--------------------------------------------------------------------------
; EVALUATE THE COMMAND LINE FOR PARMS & OPTIONS.
;
; On entry: [SI]=text line.
; On exit: if all OK then CF=false and variables are updated.
;	   If error then messages are displayed and CF=true.
;
; AX,BX,CX,DX,SI,DI destroyed.
;
look_for_parms PROC NEAR
	lea	dx,[si] 	;Save ptr to line.
lookfp_lp:
	mov	al,[si]
	inc	si
	cmp	al," "
	jb	lookfp_end	;Quit at first control char.
	cmp	al,"/"		;A parm must begin with a slash.
	jne	lookfp_lp
	dec	si		;Point at the slash.
	lea	bx,option_table
	call	parse_item	;Returns AX,BX,SI changed.
	jc	lookfp_err
	mov	di,[bx].optdef_var_ptr
	cmp	di,1		;Valid offset?
	jle	lookfp_err
	test	byte ptr [bx].optdef_action, action_switch
	jnz	lookfp_sw	;Switch, not numeric parm.
	xchg	ax,[di]
	or	ax,ax
	jz	lookfp_lp	;Was not previously defined.  Get more.
	jmp	short lookfp_err
lookfp_sw:
	test	[di],ax 	;Was that bit already set?
	jnz	lookfp_err	;Yes - duplicate parm!
	or	[di],ax
	jmp	lookfp_lp	;Get more.
lookfp_err:
	call	crout
	call	pr_text 	;Echo the entire parm line.
	call	crout
	mov	cx,si
	sub	cx,dx		;How far into the line was the error?
	jbe	lookfp_arrow
lookfp_blnk:
	mov	al," "
	call	printc
	loop	lookfp_blnk
lookfp_arrow:
	lea	dx,msg_arrow	;"^ Error",cr,lf.
	call	pr_text
	stc
	jmp	short lookfp_exit
lookfp_end:
	clc
lookfp_exit:
	ret
look_for_parms ENDP
;
;--------------------------------------------------------------------
; Check a slash parm item against a table of possible parameters.
;
; On entry: [SI]=item to be examined,
;	    [BX]=table of opt_def's (see STRUC definition).
;
;   Parms can take the following forms:
;	Text		examples:  /auto    /V	 /1  /80
;
; On exit:
;  If it's good then CF=false and SI is advanced just past the item.
;  BX will point at the matching table entry.  AX=parm value.
;
;  But if CF=true then item was bad and SI will point to
;  the offending item.	AX and BX are undefined.
;
;  Registers: only AX, BX, SI and flags are changed.
;
parse_item PROC NEAR
	push	cx
	push	dx
	push	di
	push	bp
	mov	bp,si			;Save address of input item.
p_hunt: cmp	byte ptr [bx].optdef_action, 0
	jz	p_err			;End of list - no match.
	lea	di,[bx].optdef_text	;Point DI to text in table.
	mov	cl,[bx].optdef_length
	mov	ch,0
	jcxz	p_fail
p_huntlp:
	mov	al,[si] 		;Fetch char from input line.
	inc	si
	call	uppercase
	cmp	al,[di] 		;Compare against table.
	lea	di,[di+1]
	loope	p_huntlp		;Still equal?  Continue checking.
	je	p_match
p_fail: lea	bx,[bx].optdef_text+max_optdef_text
	mov	si,bp			;Reset SI to the input item.
	jmp	p_hunt			;Try the next table entry.
p_match:
	mov	bp,si			;Save ptr to just past the text.
	mov	al,[bx].optdef_action
;	test	al,action_dec		;Numeric parm needed?
;	jnz	pnum			;Yes.
	test	al,action_switch
	jz	p_err			;No action!?  Illegal.
	mov	ax,[bx].optdef_mask	;Switch returns a bit mask in AX.
	jmp	short got_value
p_err:	jmp	short p_error
;pnum:	mov	dx,ax			;Save 'action' into DL.
;	call	rd_dec
;	jc	p_err
;	test	dl,action_k
;	jz	got_value		;Is trailing K allowed?
;	mov	dx,ax			;Save decimal value into DX.
;	mov	al,[si]
;	call	uppercase
;	xchg	ax,dx			;Decimal value into AX, char into DL.
;	cmp	dl,"K"			;Is next char a "K"?
;	jne	got_value
;	inc	si			;Accept & skip over the K.
got_value:
	mov	dx,ax		;Save value.
	mov	al,[si] 	;Examine the next character.
	cmp	al," "
	jbe	p_done		;We expect some kind of delimiter.
	cmp	al,","
	je	p_done
	cmp	al,"/"
	je	p_done
p_error:
	mov	si,bp		;Return ptr to offending item.
	stc
	jmp	short p_exit
p_done: mov	ax,dx		;Result into AX.
	clc
p_exit: pop	bp
	pop	di
	pop	dx
	pop	cx
	ret
parse_item ENDP
;
;--------------------------------------------------------------
; Copy a file/subdir name from a directory
; entry, making an ASCIIZ string.  "NAME.EXT",0
;
; On entry: ES:DI point at the directory entry,
;	    CS:BX point at the target memory area (13 bytes).
;
; On exit: CS:AX points at the 0 byte following the name.
;	   Only AX is changed.
;
copy_fname PROC NEAR
	push	bx
	push	cx
	push	si
	push	di
	push	ds
	push	es
;
	push	es
	pop	ds
	push	cs
	pop	es		;DS=yonder, ES=CS.
	cld
	xchg	bx,di
	lea	si,[bx] 	;[SI]=[BX]=dir entry, ES:DI=target memory.
	test	byte ptr [bx].file_attr, vol_attr
	jz	copyf0
	mov	cx,11		;For vol label, copy all 11 chars exactly.
	rep movsb
	jmp	short copyf4
copyf0: mov	cx,8		;8 chars max in name.
copyf1: lodsb
	cmp	al," "
	je	copyf2
	stosb
	loop	copyf1
copyf2: lea	si,[bx+8]
	cmp	byte ptr [si]," "
	je	copyf4		;No file extension.
	mov	al,"."
	stosb
	mov	cl,3		;3 chars max in extension.
copyf3: lodsb
	cmp	al," "
	je	copyf4
	stosb
	loop	copyf3
copyf4: mov	al,0		;End ASCIIZ string.
	stosb
	lea	ax,[di-1]
	pop	es
	pop	ds
	pop	di
	pop	si
	pop	cx
	pop	bx
	ret
copy_fname ENDP
;
;--------------------------------------------
prog	ENDS
	END
