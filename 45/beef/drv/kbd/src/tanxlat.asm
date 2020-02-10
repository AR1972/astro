;*
;*	CW : Character Windows Drivers
;*
;*	tanxlat.asm : Tandy 1000 Key translation
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
;	Assert(sc != 0);
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
	jnz	vfs_ok1
	int	3
vfs_ok1:
ENDIF

tovw_loop:
	mov	al,CS:[bx].scMpVw
	or	al,al
	jz	fail_tovw
	add	bx,SIZE MPSCVW
	cmp	al,ch
	jne	tovw_loop
	mov	bl,CS:[bx].(vwMpVw - SIZE MPSCVW)
	or	bl,bl		;* set nz
	ret

fail_tovw:	;* no match
	xor	bl,bl		;* set z
	ret

cEnd	nogen ;VwFromSc


;********** ChFromSc **********
;*	entry : CH = sc (scan code)
;*		CL = ch (character)
;*		DX = mpscch = pointer to MPSCCH array (in CS)
;*	* convert scan code to virtual key
;*	* return virtual key
;*	exit : CL = ch (character)
;*	* TRASHES AX only

cProc	ChFromSc, <NEAR, ATOMIC, PUBLIC>
cBegin	nogen ;ChFromSc

	push	bx
	mov	bx, dx
;{{
;	Assert(sc != 0);
;	while (mpscch->sc != 0)
;		{
;		if (mpscch->sc == sc (DH))
;			return mpscch->ch;	/* in cl */
;		mpscch++;
;		}
;	return ch;	/* in cl */
;}}
IFDEF DEBUG
	cmp	ch,0
	jnz	cfs_ok1
	int	3
cfs_ok1:
ENDIF

toch_loop:
	mov	al,CS:[bx].scMpCh
	or	al,al
	jz	done_toch
	add	bx,SIZE MPSCCH
	cmp	al,ch
	jne	toch_loop
	mov	cl,CS:[bx].(chMpCh - SIZE MPSCCH)

done_toch:
	pop	bx
	ret

cEnd	nogen ;ChFromSc


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
	jcxz	done_xlat_2		;* just update shift states
	mov	ssOff,(SS_ALT OR SS_SHIFT OR SS_CONTROL)
done_xlat_2:
	jmp	done_xlat

real_scan_code:

;{{	/* Some exceptions: override CH to force table lookup */
;	if (sc == SC_6 && (ch == 01Eh || ch == '}') ||	/* [Alt] Ctrl 6 */
;		sc == SC_MINUS && ch == 01Fh ||		/* Ctrl - */
;		sc == SC_MULTIPLY && ch == 010h ||	/* PRINT key */
;		sc == SC_ENTER ||			/* main ENTER */
;		sc == SC_TAB ||
;		sc == SC_ESCAPE ||
;		sc == SC_BACKSPACE)
;		ch = 0;
;}}
	cmp	cx, 071Eh		;* Ctrl 6^
	je	kill_ch
	cmp	cx, 077Dh		;* Alt Ctrl 6^
	je	kill_ch
	cmp	cx, 0C1Fh		;* Ctrl -_
	je	kill_ch
	cmp	cx, 3710h		;* PRINT
	je	kill_ch
	cmp	ch, SC_ENTER
	je	kill_ch
	cmp	ch, SC_TAB
	je	kill_ch
	cmp	ch, SC_ESC
	je	kill_ch
	cmp	ch, SC_BACKSPACE
	jne	not_special
kill_ch:
	xor	cl, cl
	jmp	short xlat_look_1
not_special:

;{{	/* Numpad ENTER is a little peculiar */
;	if (ch == SC_NUMPAD_ENTER && ch != 0)
;		{
;		vw = VwOfVk(VK_RETURN);
;		ssOn |= SS_EXTENDED;
;		goto done_xlat;		/* skip the table lookup */
;		}
;}}

	cmp	ch, 57h			;* SC_NUMPAD_ENTER
	jne	not_num_enter
	or	cl, cl			;* watch out! Shift-F4 has same sc
	jz	not_num_enter
	mov	bl, VwOfVk(VK_RETURN)
	or	ssOn, SS_EXTENDED
	jmp	short done_xlat_2

not_num_enter:

;{{	/* Some numpad exceptions when NumLock is off */
;	if (! (ssTemp & SS_NUMLOCK))
;		{
;		if (sc == SC_NUMPAD1 && ch == '1' ||
;			sc == SC_NUMPAD3 && ch == '3' ||
;			sc == SC_NUMPAD9 && ch == '5' ||
;			sc == SC_SHIFT_INSERT && ch == '+' ||
;			sc == SC_DELETE && ch == '-')
;			{
;			ssTemp &= ~SS_SHIFT;
;			ssOn |= SS_SHIFT;
;			/* the following is optimized into the above 'if' */
;			if (sc == SC_NUMPAD5)
;				sc = SC_PLAIN_NUMPAD5, ch = 0;
;			else if (sc == SC_SHIFT_INSERT)
;				sc = SC_INSERT, ch = 0;
;			else if (sc == SC_DELETE)
;				ch = 0;
;			}
;		}
;}}
	test	ssTemp, SS_NUMLOCK
	jnz	num_lock_on		;* handle shift w/o NumLock first
	cmp	cx, 4F31h		;* sc == NUMPAD1, ch == '1'
	je	num_shift
	cmp	cx, 5133h		;* sc == NUMPAD3, ch == '3'
	je	num_shift
	cmp	cx, 4939h		;* sc == NUMPAD9, ch == '9'
	je	num_shift
	cmp	cx, 4C35h		;* sc == NUMPAD5, ch == '5'
	jne	not_5
	mov	ch, 0F3h		;* sc = PLAIN_NUMPAD5
	jmp	short num_shift
not_5:
	cmp	cx, 552Bh		;* sc == SHIFT_INSERT, ch == '+'
	jne	not_ins
	mov	ch, 52h			;* sc = INSERT
	jmp	short num_shift
not_ins:
	cmp	cx, 532Dh		;* sc == DELETE, ch = '-'
	jne	xlat_look_1
num_shift:
	xor	cl, cl			;* kill ch to force table lookup
	and	ssTemp, not SS_SHIFT	;* fool the sc to vw conversion
	or	ssOn, SS_SHIFT		;* but keep the shift state
xlat_look_1:
	jmp	xlat_lookup

num_lock_on:
;{{	/* Some more exceptions, this time with NumLock on */
;	else
;		{	/* The weird INSERT/'+' key */
;		if (sc == SC_SHIFT_INSERT && ch == '+' ||
;			sc == SC_INSERT && ch == 0 ||
;			sc == SC_CONTROL_INSERT && ch == 0)
;			ch = '+', vw = VwOfVk(VK_ADD),
;			ssOn |= SS_EXTENDED;
;			/* and skip the VwFromSc transform */
;}}
	cmp	cx, 552Bh		;* sc == SHIFT_INSERT, ch = '+'
	je	num_add_1
	cmp	cx, 5200h		;* sc == INSERT, ch == 0
	je	num_add
	cmp	cx, 9F00h		;* sc == CONTROL_INSERT, ch = 0
	jne	num_not_add
num_add:
	mov	cl, '+'
num_add_1:
	mov	bl, VwOfVk(VK_ADD)
	jmp	short num_add_sub

num_not_add:
;{{	/* The weird DELETE/'-' key */
;		else if (sc == SC_DELETE && (ch == '-' || ch == 0) ||
;			sc == SC_CONTROL_DELETE && ch == 0)
;			ch = '-', vw = VwOfVk(VK_SUBTRACT);
;			/* and skip the VwFromSc transform */
;		}
;}}
	cmp	cx, 532Dh		;* sc == DELETE, ch == '-'
	je	num_sub_1
	cmp	cx, 5300h		;* sc == DELETE, ch == 0
	je	num_sub
	cmp	cx, 9D00h		;* sc == CONTROL_DELETE, ch = 0
	jne	not_num_sub
num_sub:
	mov	cl, '-'
num_sub_1:
	mov	bl, VwOfVk(VK_SUBTRACT)
num_add_sub:
	or	ssOn, SS_EXTENDED
	jmp	done_xlat
not_num_sub:

xlat_lookup:
;{{	/* Convert scan codes to VW's, aided by knowledge of shift state */
;	if (ch == 0)
;		{
;		if (ssTemp & SS_ALT)
;			pmpscvw = pmpscvwAlt,
;			pmpscch = pmpscchAlt;
;		else if (ssTemp & SS_CONTROL)
;			pmpscvw = pmpscvwCtrl,
;			pmpscch = pmpscchCtrl;
;		else if (ssTemp & SS_SHIFT)
;			pmpscvw = pmpscvwShift,
;			pmpscch = pmpscchShift;
;		else
;			pmpscvw = pmpscvwPlain,
;			pmpscch = pmpscchPlain;
;		ch = ChFromSc(pmpscch, sc);
;		vw = VwFromSc(pmpscvw, sc);
;		if (vw)
;			if (vw == VwOfVk(VK_MULTIPLY) ||
;				vw == VwOfVk(VK_ADD) ||
;				vw == VwOfVk(VK_SUBTRACT) ||
;				vw == VwOfVk(VK_DECIMAL) ||
;				vw == VwOfVk(VK_NUMPAD0 ||
;				vw == VwOfVk(VK_NUMPAD6))
;				ssOn |= SS_EXTENDED;
;		}
;}}
	or	cl, cl
	jnz	done_lookup
	mov	ax, ssTemp
	mov	bx, drvOffset mpscvwAlt
	mov	dx, drvOffset mpscchAlt
	test	ax, SS_ALT
	jnz	lookup_bx
	mov	bx, drvOffset mpscvwCtrl
	mov	dx, drvOffset mpscchCtrl
	test	ax, SS_CONTROL
	jnz	lookup_bx
	mov	bx, drvOffset mpscvwShift
	mov	dx, drvOffset mpscchShift
	test	ax, SS_SHIFT
	jnz	lookup_bx
	mov	bx, drvOffset mpscvwPlain
	mov	dx, drvOffset mpscchPlain
lookup_bx:
	cCall	ChFromSc
	cCall	VwFromSc
	jz	done_lookup			;* if (!vw)
	cmp	bl, VwOfVk(VK_MULTIPLY)
	je	add_extend
	cmp	bl, VwOfVk(VK_ADD)
	je	add_extend
	cmp	bl, VwOfVk(VK_SUBTRACT)
	je	add_extend
	cmp	bl, VwOfVk(VK_DECIMAL)
	je	add_extend
	cmp	bl, VwOfVk(VK_NUMPAD0)
	je	add_extend
	cmp	bl, VwOfVk(VK_NUMPAD6)
	jne	done_xlat
add_extend:
	or	ssOn, SS_EXTENDED
	jmp	short done_xlat

done_lookup:
;{{	/* Assign VW's to some leftover ascii keys */
;	if (vw == 0)
;		{
;		if (ch >= 'A' && ch <= 'Z')
;			vw = VwOfVk(VK_A) + ch - 'A';
;		else if (ch >= 'a' && ch <= 'z')
;			vw = VwOfVk(VK_A) + ch - 'a';
;		else if (ch >= 1 && ch <= 26)
;			vw = VwOfVk(VK_A) + ch - 1;
;		else if (ch >= '0' && ch <= '9')
;			{
;			if (sc < SC_NUMBER_MAX)
;				vkBase = VK_0;
;			else
;				vkBase = VK_NUMPAD, ssOn |= SS_EXTENDED;
;			vw = VwOfVk(vkBase) + ch - '0';
;			}
;		else if (ch == ' ')
;			vw = 0;
;		else if (ch == '.' && sc == SC_DECIMAL)
;			vw = VwOfVk(VK_DECIMAL), ssOn |= SS_EXTENDED;
;		}
;}}
	AssertEQ bl, 0
	mov	bl, VwOfVk(VK_A)	;* min VW of run
	mov	al, 'A'			;* min CH
	cmp	cl, al
	jb	not_upper
	cmp	cl, 'Z'			;* max CH
	jna	assign_vw
not_upper:
	mov	al, 'a'
	cmp	cl, al
	jb	not_lower
	cmp	cl, 'z'
	jna	assign_vw
not_lower:
	mov	al, 1
	cmp	cl, al
	jb	not_ctrl
	cmp	cl, 26
	jna	assign_vw
not_ctrl:
	mov	bl, VwOfVk(VK_0)
	mov	al, '0'
	cmp	cl, al
	jb	not_number
	cmp	cl, '9'
	ja	not_number
	Assert	SC_NUMBER_MAX LT SC_NUMPAD_MIN
	cmp	ch, SC_NUMBER_MAX
	jb	assign_vw
	mov	bl, VwOfVk(VK_NUMPAD0)
	or	ssOn, SS_EXTENDED
assign_vw:
	sub	al, cl		;* bl = bl + (cl - al)
	sub	bl, al		;/* vw += ch - chMin; */
	jmp	short done_xlat
not_number:
	xor	bl, bl
	cmp	cl, ' '
	je	done_xlat
	cmp	cx, 562Eh	;* ch = '.', sc = SC_DECIMAL
	jne	done_xlat
	mov	bl, VwOfVk(VK_DECIMAL)
	or	ssOn, SS_EXTENDED

done_xlat:	;* CX == sc:ch, ssTemp == ss, BL = vw

;{{
;	ssNew = (ss & ~ssOff) | ssOn;
;	DoShift(ssNew);
;}}

	mov	ax,ssOff
	not	ax
	and	ax,ssTemp			;* ss & ~ssOff
	or	ax,ssOn
	push	ax				;* save ssNew
	pop	dx				;* ssNew

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
;	goto retry_xlating;
;}}
	cCall	DoShift,<ssTemp>

cEnd	XlateKey

;*********************************************************************
