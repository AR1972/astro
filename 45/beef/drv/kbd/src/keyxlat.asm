;*
;*	CW : Character Windows Drivers
;*
;*	keyxlat.asm : Key translation
;*****************************************************************************


;********** VwFromSc **********
;*	entry : CH = sc (scan code)
;*		CL = ch (character)
;*		BX = mpscvw = pointer to MPSCVW array (in CS)
;*	* convert scan code to virtual key
;*	* return virtual key
;*	exit : BL = vw
;*		Z flag set if (vw == 0)
;*	* TRASHES BX and AX only

cProc	VwFromSc, <NEAR, ATOMIC, PUBLIC>
cBegin	nogen ;VwFromSc

;{{
;	Assert(sc != 0 && sc != 0xe0);
;	while (mpscvw->sc != 0)
;		{
;		if (mpscvw->sc == sc (DH))
;			return mpscvw->vw;	// in bl
;		mpscvw++;
;		}
;	return 0;
;}}
IFDEF DEBUG
	cmp	ch,0
	jnz	ok1
	int	3
ok1:
	cmp	ch,0e0h
	jnz	ok2
	int	3
ok2:
ENDIF

tovw_loop:
	mov	al,CS:[bx].scMp
	or	al,al
	jz	fail_tovw
	add	bx,SIZE MPSCVW
	cmp	al,ch
	jne	tovw_loop
	mov	bl,CS:[bx].(vwMp - SIZE MPSCVW)
	or	bl,bl		;* set nz
	ret

fail_tovw:	;* no match
	xor	bl,bl		;* set z
	ret

cEnd	nogen ;VwFromSc



;********* XlateKey **********
;*	entry: DI => driver data
;*		AL = char, AH = scan code
;*		DX = shift states
;*	* translate into character with shifts
;*	* call UpdateShiftKk and KeyboardMessage with any results

cProc	XlateKey,<NEAR, PUBLIC, ATOMIC>,<SI, DI>
    localW	ssOn
    localW	ssTemp
ssOff	EQU	SI		;* ssOff register variable

cBegin	XlateKey

	AssertEQ di,OFF_lpwDataKbd

	mov	cx,ax				;* AL = CL = ch, AH = CH = sc
	mov	ssTemp,dx			;* WARNING ch is in cl !!!!

;{{
;	vw (bl) = 0;
;	ssOn = ssOff = 0
;}}
	xor	bx,bx				;* vw = 0, can assume bh == 0
	mov	ssOn,bx
	xor	ssOff,ssOff			; ssOff = 0

;{{
;	if (sc == 0)
;		{
;		if (ch != 0)
;			{
;			 /*
;			    ch is a character entered numerically using the
;			    Alt key in combination with the numeric key pad.
;			    (e.g. The user typed Alt-down 1 2 3 Alt-Up the enter
;			    the char with decimal value 123)
;			 */
;			vw = 0;	/* already */
;			ssOff = SS_ALT | SS_SHIFT | SS_CONTROL;
;			}
;		}
;}}

	or	ch,ch
	jnz	real_scan_code
;*	* no real scan code (probably from alt-numpad)
	jcxz	done_xlat_4		;* just update shift states
	mov	ssOff,(SS_ALT OR SS_SHIFT OR SS_CONTROL)
	jmp	short done_xlat_4

real_scan_code:
;{{	/* check for some RT extended keys ('/' and ENTER) */
;	else if (sc == 0xe0)
;		{
;		ssOn = SS_EXTENDED;
;		if (ch == 13 || ch == 10)	/* ENTER or CTRL-ENTER */
;			vw = VwOfVk(VK_RETURN);
;		}
;}}
	cmp	ch,0e0h
	jne	not_ext_ascii
	cmp	cl, 13
	je	is_enter
	cmp	cl, 10
	jne	is_ext_ascii
	or	ssOn, SS_CONTROL
is_enter:
	mov	bl, VwOfVk(VK_RETURN)
is_ext_ascii:
	mov	ssOn, SS_EXTENDED
	jmp	short done_xlat_4
not_ext_ascii:

;{{	/* special ITL keyboard check */
;	// ITL keyboards can use ALT-CTRL and ALT for special characters.
;	//  the normal BIOS will never return a (non-itl) character if the ALT
;	//  key is down.  Therefore if we get a real character with the ALT
;	//  key down we will assume it is an international character and
;	//  turn off the ALT(MENU) and CONTROL bits.
;	// there are a few exceptions, where we diddled the BIOS to let
;	//  some unusual ALT keys through (BS, '=', '+', '-')
;	else if ((ss & SS_ALT) && ch != 0 && ch != 0xe0 &&
;			ch != ' ' && 
;			ch != 8 & ch != '=' &&
;			sc != '+' && sc != '-')
;		{
;		ssOff = SS_ALT | SS_CONTROL
;		}
;}}
	test	ssTemp,SS_ALT
	jz	no_itl_char
	or	cl,cl
	jz	no_itl_char
	cmp	cl,0e0h
	je	no_itl_char
	cmp	cl, 8
	je	no_itl_char
	cmp	cl, '='
	je	no_itl_char
	cmp	cl, '+'
	je	no_itl_char
	cmp	cl, '-'
	je	no_itl_char
	cmp	cl, ' '
	je	no_itl_char
	mov	ssOff,SS_ALT OR SS_CONTROL	;* turn off special states
done_xlat_4:
	jmp	done_xlat
no_itl_char:

;{{	/* simple cases */
;	else if (sc == SC_MULTIPLY)
;		ssOn = SS_EXTENDED, vw = VwOfVk(VK_MULTIPLY);
;	else if (sc == SC_ADD)
;		ssOn = SS_EXTENDED, vw = VwOfVk(VK_ADD);
;	else if (sc == SC_SUBTRACT)
;		ssOn = SS_EXTENDED, vw = VwOfVk(VK_SUBTRACT);
;}}
	mov	al, VwOfVk(VK_MULTIPLY)
	cmp	ch, SC_MULTIPLY
	je	done_xlat_x
	mov	al, VwOfVk(VK_ADD)
	cmp	ch, SC_ADD
	je	done_xlat_x
	mov	al, VwOfVk(VK_SUBTRACT)
	cmp	ch, SC_SUBTRACT
	jne	not_greyminus
done_xlat_x:
	mov	ssOn, SS_EXTENDED
	jmp	done_xlat_al

not_greyminus:
;{{	/* more simple cases */
;	else if (sc == SC_ENTER)
;		vw = VwOfVk(VK_RETURN);
;	else if (sc == SC_ESC)
;		vw = VwOfVk(VK_ESCAPE);
;	else if (sc == SC_BACKSPACE)
;		vw = VwOfVk(VK_BACK);
;}}
	mov	al,VwOfVk(VK_RETURN)
	cmp	ch,SC_ENTER
	jz	done_xlat_al

	mov	al,VwOfVk(VK_ESCAPE)
	cmp	ch,SC_ESC
	jz	done_xlat_al

	mov	al,VwOfVk(VK_BACK)
	cmp	ch,SC_BACKSPACE
	jz	done_xlat_al

ifdef KANJI
	mov	al, VwOfVk(VK_KANJI)
	cmp	ch, SC_KANJI
	jz	done_xlat_al
endif
;*	* More complicated cases
;{{
;	else if (sc == SC_TAB)
;		ch = vw = VwOfVk(VK_TAB);
;}}
	cmp	ch,SC_TAB
	jnz	not_tab
	mov	bl,VwOfVk(VK_TAB)
	mov	cl, bl
	jmp	short done_xlat_1

not_tab:
;{{
;	else if (ch == ' ')
;		vw = 0;
;}}
	cmp	cl,' '
	je	done_xlat_1
	jmp	short not_space

;************************************************************************
;*	* done_xlat_al : al = vw	;* located here for near jumps !!
done_xlat_al:
	mov	bl,al
done_xlat_1:
	jmp	done_xlat
;************************************************************************

not_space:
;{{	/* Alphas */
;	else if (ch >= 'A' && ch <= 'Z')	/* special jump for control */
;		{
;		vw = ch;
;		ssOff = SS_ALT SS_CONTROL;
;		}
;	else if (ch >= 'a' && ch <= 'z')
;		{
;		vw = 'A' + (ch - 'a');
;		ssOff = SS_ALT | SS_CONTROL;
;		}
;}}
	mov	ax,cx
	cmp	al,'A'
	jb	maybe_control			;* maybe a control char ??
	cmp	al,'Z'
	ja	not_upper_alpha
;*	* upper case alpha
	mov	ssOff,SS_ALT OR SS_CONTROL
	jmp	done_xlat_al

not_upper_alpha:
	cmp	al,'a'
	jb	not_alpha
	cmp	al,'z'
	ja	not_alpha
;*	* lower case alpha
	sub	al,20H				;* to upper
	mov	ssOff,SS_ALT OR SS_CONTROL
	jmp	done_xlat_al

maybe_control:
;{{	/* test for control alpha characters */
;	else if (ch >= 1 && ch <= 26)
;		{
;		vw = 'A' + (ch - 1);
;		ssOff = SS_ALT;
;		ssOn = SS_CONTROL;
;		}

	cmp	al,1
	jb	not_alpha
	cmp	al,26
	ja	not_alpha
	add	al,40H
	mov	ssOff,SS_ALT
	mov	ssOn,SS_CONTROL
	jmp	done_xlat_al

not_alpha:
;{{	/* Check peculiar Ctrl keys (6 and -) */
;	else if (sc == SC_MINUS && ch == 0x1F)
;		ch = '-', ssOn |= SS_CONTROL;
;	else if (sc == SC_6 && ch == 0x1E)
;		ch = 0, vw = VwOfVk(VK_6), ssOn |= SS_CONTROL;
;}}
	cmp	cx, (SC_MINUS shl 8) + 1Fh
	jne	not_ctl_minus
	mov	cl, '-'
	jmp	short done_xlat_ctl
not_ctl_minus:
	cmp	cx, (SC_6 shl 8) + 1Eh
	jne	not_weirdctl
	xor	cl, cl
	mov	bl, VwOfVk(VK_6)
done_xlat_ctl:
	or	ssOn, SS_CONTROL
	jmp	short done_xlat_1

not_weirdctl:
;{{	/* Numpad exceptions */
;	else if (sc >= SC_NUMPAD_MIN && sc < SC_NUMPAD_MAX)
;		{
;		/*
;		At this point we know the user typed a key on the numeric
;		keypad (but NOT ctrl+numpad that's handled farther down).
;		We must figure out how the keystoke is to be interpreted.
;		If ch == 0 then we interpret the keystroke as an arrow key.
;		*/
;		if (sc != SC_INSERT)
;			ssOff = SS_CONTROL;
;		ivw (bx) = sc - SC_NUMPAD_MIN
;		vw = rgvwNumpad[ivw];
;		if (ch == 0xe0)
;			{
;			ch = 0;
;			ssOn = SS_EXTENDED;
;			}
;		else if (ch == 0)
;			{
;			if (ss & SS_NUMLOCK)
;				ssOff |= SS_SHIFT;
;			}
;		else if ((ss & SS_SHIFT) ||
;				(sc == SC_NUMPAD5 && 
;				!(ss & SS_CONTROL) && (ss & SS_NUMLOCK)))
;			ch = 0;
;		else
;			{
;			vw = rgvwNumeric[ivw];
;			ssOn = SS_EXTENDED;
;			}
;		if (ch == 0 && sc == SC_DELETE)
;			ch = (char) 0x7f;
;		}
;	}
;}}

	cmp	ch,SC_NUMPAD_MIN
	jb	not_num_pad
	cmp	ch,SC_NUMPAD_MAX
	jae	not_num_pad

IFDEF DOS5
	test	ssTemp, SS_ALT		;* ignore alt-numpad keystrokes
	jz	@F
	jmp	exit_xlat
@@:
ENDIF ;DOS5

;*	* NUMPAD
	mov	bl,ch
;;	xor	bh,bh		;; not needed
	mov	bl,(rgvwNumpad-SC_NUMPAD_MIN)[BX]
	cmp	cl,0E0H
	jne	not_E0
	xor	cl,cl
	mov	ssOn, SS_EXTENDED
	jmp	num_pad_fin
not_E0:
	or	cl,cl
	jnz	num_pad_2
	test	ssTemp,SS_NUMLOCK
	jz	num_pad_fin
	or	ssOff,SS_SHIFT
	jmp	short num_pad_fin

num_pad_2:
IFDEF	STD_NUMPAD
	cmp	ch, 76			;numpad 5
	jnz	num_pad_normal
	test	ssTemp, SS_NUMLOCK
	jz	num_pad_fun
	test	ssTemp, SS_CONTROL
	jnz	num_pad_fun
	test	ssTemp, SS_SHIFT
	jz	num_pad_num
	or	ssOff, SS_SHIFT
	jmp	short num_pad_fun
ENDIF	; STD_NUMPAD

IFDEF	TOSHIBA_NUMPAD
	test	ssTemp, SS_NUMLOCK
	jz	num_pad_fun
	test	ssTemp, SS_CONTROL
	jnz	num_pad_fun
	test	ssTemp, SS_SHIFT
	jz	num_pad_num
	or	ssOff, SS_SHIFT
ELSE	;TOSHIBA_NUMPAD
num_pad_normal:
	test	ssTemp, SS_SHIFT
	jnz	num_pad_fun
	test	ssTemp,SS_NUMLOCK
	jnz	num_pad_num
ENDIF	;TOSHIBA_NUMPAD

num_pad_fun:
	mov	cl,0
	jmp	short num_pad_fin

num_pad_num:
	mov	bl,ch
;;	xor	bh,bh		;; not needed
	mov	bl,(rgvwNumeric-SC_NUMPAD_MIN)[BX]
	mov	ssOn, SS_EXTENDED

num_pad_fin:	;* test delete case
	or	cl,cl
	jnz	done_xlat_2
	cmp	ch,SC_DELETE
	jnz	done_xlat_2
	mov	cl,07FH
done_xlat_2:
	jmp	done_xlat

not_num_pad:
;{{	/* check for number keys */
;	else if (ch >= '0' && ch <= '9')
;		{
;		vw = ch;
;		if (ss & SS_CONTROL)
;			ch = 0;
;		}
;}}

	cmp	cl,'0'
	jb	not_num
	cmp	cl,'9'
	ja	not_num
	mov	bl,cl
	test	ssTemp, SS_CONTROL
	jz	done_xlat_2
	xor	cl, cl
	jmp	done_xlat_2

not_num:
;{{	/* F1..F10 and ALT-1..9 */
;	else if (ch == 0 || ch = 0xe0)
;		{
;		ch = 0;
;		ssOff = SS_SHIFT | SS_CONTROL | SS_ALT;
;		/* special cases */
;		if (sc >= 59 && sc <= 68)		/* Fn */
;			{
;			/* normal key */
;			vw = VwOfVk(VK_F1) + (sc - 59);
;			}
;		else if (sc >= 84 && sc <= 93)		/* SHIFT Fn */
;			{
;			vw = VwOfVk(VK_F1) + (sc - 84);
;			ssOn = SS_LSHIFT;
;			}
;		else if (sc >= 94 && sc <= 103)		/* CTRL Fn */
;			{
;			vw = VwOfVk(VK_F1) + (sc - 94);
;			ssOn = SS_CONTROL;
;			ssOff = SS_ALT;	// allow SHIFT
;			}
;		else if (sc >= 104 && sc <= 113)	/* ALT Fn */
;			{
;			vw = VwOfVk(VK_F1) + (sc - 104);
;			ssOn = SS_ALT;
;			ssOff = 0;	// allow SHIFT/CONTROL
;			}
;		else if (sc >= 120 && sc <= 128)
;			{
;			vw = '1' + (sc - 120);
;			ssOn = SS_ALT;
;			ssOff = 0;	// allow SHIFT/CONTROL
;			}
;}}

	or	cl,cl
	jz	is_special
	cmp	cl, 0e0h
	jnz	done_xlat_3
	xor	cl, cl
	mov	ssOn, SS_EXTENDED

is_special:
	xor	dx, dx				;* additional shift states
	mov	ssOff,SS_SHIFT OR SS_CONTROL OR SS_ALT
	mov	ah,VwOfVk(VK_F1)		;* base vw

	mov	al,59				;* al = base sc
	cmp	ch,al
	jb	not_special
	cmp	ch,68
	jbe	do_special

	mov	al,84
	mov	dx,SS_LSHIFT
	cmp	ch,al
	jb	not_special
	cmp	ch,93
	jbe	do_special

	mov	ssOff,SS_ALT		;* menu is the only thing overridden
	mov	al,94
	mov	dx,SS_CONTROL
	cmp	ch,al
	jb	not_special
	cmp	ch,103
	jbe	do_special

	xor	ssOff,ssOff		;* allow everything
	mov	al,104
	mov	dx,SS_ALT
	cmp	ch,al
	jb	not_special
	cmp	ch,113
	jbe	do_special

	mov	al,120
	mov	dx,SS_ALT
	cmp	ch,al
	jb	not_special
	cmp	ch,128
	ja	not_special
	mov	ah,'1'
do_special:
;	* ah = base key, ch = sc, al = base scan code, dx = ssOn
	neg	al
	add	al,ch
	add	al,ah
	mov	bl,al
	or	ssOn,dx
done_xlat_3:
	jmp	done_xlat

not_special:

;{{	/* last default case */
;	else
;		{
;		if (vw == 0)
;			vw = VwFromSc(sc, pmpscvwPlain);
;		if (vw == 0)
;			{
;			if (vw = VwFromSc(sc, pmpscvwAltRonco))
;				{
;				/* Extended keypad Alt */
;				ssOn |= SS_ALT | SS_EXTENDED;
;				}
;			if (vw = VwFromSc(sc, pmpscvwAlt))
;				{
;				/* normal ALT */
;				ssOn |= SS_ALT;
;				}
;			else if (ch == 0 && vw = VwFromSc(sc, pmpscvwAltNoChar))
;				{
;				/* special ALT */
;				ssOn |= SS_ALT;
;				ch = vw;
;				vw = 0;
;				}
;			else if (vw = VwFromSc(sc, mpscvwCtrl))
;				{
;				ssOff = 0;
;				ssOn |= SS_CONTROL;
;				}
;			else if (vw = VwFromSc(sc, pmpscvwShift))
;				{
;				ssOff = 0;
;				ssOn |= SS_LSHIFT;
;				}
;			}
;		}
;	} /*end of major if*/
;}}
	mov	bx,[di].pmpscvwPlain
	cCall	VwFromSc
	jnz	done_xlat

	mov	bx,drvOffset mpscvwAltRonco
	cCall	VwFromSc
	jz	not_extalt
	or	ssOn, SS_ALT OR SS_EXTENDED
	jmp	short done_xlat

not_extalt:
	mov	bx,drvOffset mpscvwAlt
	cCall	VwFromSc
	jz	not_alt1

yes_alt:
	or	ssOn,SS_ALT
	jmp	short done_xlat

not_alt1:
	or	cl,cl
	jnz	not_alt2
	mov	bx,drvOffset mpscvwAltNoChar
	cCall	VwFromSc
	jz	not_alt2
	xchg	cl, bl			;* actually it's a CH, not a VW
	jmp	short yes_alt
	
not_alt2:
	mov	bx,drvOffset mpscvwCtrl
	cCall	VwFromSc
	jz	not_control
;*	* control
	xor	ssOff,ssOff
	or	ssOn,SS_CONTROL
	jmp	short done_xlat

not_control:
	mov	bx,[di].pmpscvwShift
	cCall	VwFromSc
	jz	done_xlat
	xor	ssOff,ssOff
	or	ssOn,SS_LSHIFT

done_xlat:	;* CX == sc:ch, ssTemp == ss, BL = vw

;{{
;	ssNew = (ss & ~ssOff) | ssOn;
;}}

	mov	ax,ssOff
	not	ax
	and	ax,ssTemp			;* ss & ~ssOff
	or	ax,ssOn
	mov	dx, ax				;* ssNew

;{{
;	if (vw | ch)
;		{
;		/* 1 message for WM_CHAR only !! */
;		KeyboardMessage(vw, ch, KkOfSs(ssNew), FALSE);
;		}
	mov	al,bl
	or	al,cl
	jz	no_key_event

	mov	al,ch				;* sc
IFDEF	KANJI
	xor	ah,ah
	push	ax				;* sc
ENDIF	; KANJI
	Assert	<?PLM>

	push	bx				;* vw
	xor	ch,ch
	or	cl,cl
	jnz	we_have_char
;*	* no char -- put in the VK
	Assert	<VwOfVk(155H) EQ 55H>
	mov	cl,bl
	inc	ch				;* ch == 1
we_have_char:
	push	cx				;* ch
	cCall	KkOfSs, <dx>
	push	ax
	xor	bx,bx
	push	bx				;* FALSE
	mov	bx,[di].pinkbCur
	cCall	[bx].lpfnKeyboardMessageInkb

no_key_event:

;{{	/* restore shift states */
;	DoShift(ss);
;}}
	cCall	DoShift,<ssTemp>

exit_xlat:
cEnd	XlateKey

;*********************************************************************
