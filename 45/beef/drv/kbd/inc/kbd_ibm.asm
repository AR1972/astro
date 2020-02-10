;*
;*	CW : Character Windows
;*
;*	kbd_ibm.asm : standard IBM procedures (and helpers)



ifndef MkGetShiftStates_NonDefault
;*****************************************************************************
;********** MkGetShiftStatesKbd **********
;*	* KBD entry point (see documentation for interface)
;*	* return MK values from last saved shift states

cProc	MkGetShiftStatesKbd, <FAR, PUBLIC, ATOMIC>
cBegin	MkGetShiftStatesKbd

	mov	bx,OFF_lpwDataKbd

	xor	ax,ax
	mov	dx,[bx].ssOld
	test	dl,SS_SHIFT
	jz	not_mk_shift
	Assert	<MK_SHIFT LT 100H>
	or	ax,MK_SHIFT
not_mk_shift:
	test	dl,SS_CONTROL
	jz	not_mk_control
	Assert	<MK_CONTROL LT 100H>
	or	ax,MK_CONTROL
not_mk_control:
	test	dl,SS_ALT
	jz	not_mk_menu
	Assert	<MK_MENU GE 100H>
	or	ah,HIGH MK_MENU
not_mk_menu:

cEnd	MkGetShiftStatesKbd

;*****************************************************************************
endif	;* MkGetShiftStates_NonDefault


;*****************************************************************************
;*	* HELPER ROUTINES *


;********** DoShift **********
;*	entry : ssNew = shift state
;*		DS:DI => driver data
;*	* set the new shift state
;*	exit : n/a

cProc	DoShift, <NEAR, ATOMIC, PUBLIC>
    parmW ssNew
cBegin	DoShift

	AssertEQ di,OFF_lpwDataKbd
;{{
;	if (ssNew == ssOld)
;		return;
;}}
	mov	ax,ssNew
	mov	dx,[di].ssOld
	cmp	ax,dx
	je	end_do_shift

;{{
;	kkNew = KkOfSs(ssNew);
;	kkOld = KkOfSs(ssOld);
;	if (kkNew != kkOld)
;		// special message for change in shift states
;		KeyboardMessage(0, kkOld, kkNew, TRUE);
;}}

	cCall	KkOfSs, <ax>
	mov	cx,ax				;* cx = kkNew
	cCall	KkOfSs, <dx>			;* ax = kkOld, dx = ssOld

	cmp	ax,cx
	je	done_update_shift_kk

	mov	bx,[di].pinkbCur
	push	dx				;* save ssOld
	push	ax				;* save kkOld

	xor	dx,dx
IFDEF	KANJI
	cCall	[bx].lpfnKeyboardMessageInkb, <dx, dx, ax, cx, sp>
ELSE
	cCall	[bx].lpfnKeyboardMessageInkb, <dx, ax, cx, sp>
ENDIF	; KANJI

	pop	ax
	pop	dx

done_update_shift_kk:	;* ax = kkOld, dx = ssOld

;{{
;	/* check for shift event up or down */
;	ssDelta = ssOld ^ ssNew;
;	if (ssDelta & SS_ALT)
;		{
;		KeyboardMessage(VwOfVk(VK_MENU), VwOfVk(VK_MENU), KkOfSs(ssNew),
;		    ~ssNew & SS_ALT);
;		}
;	if ((ssDelta & SS_SPACE) && !(ssNew & SS_SPACE))
;		{
;		/* releasing SPACE key */
;		KeyboardMessage(VwOfVk(VK_SPACE), VwOfVk(VK_SPACE),
;		    KkOfSs(ssNew), TRUE);
;		}
;}}

	mov	ax,ssNew
	xor	dx,ax				;* dx = ssDelta
	test	dx,SS_ALT
	jz	not_ss_menu
	mov	bx,ax
	not	bx
	and	bx,SS_ALT			;* ssMask & ~ssNew
	mov	cl,VwOfVk(VK_MENU)
	cCall	KeyboardMessageShort

not_ss_menu:
	test	dx,SS_SPACE
	jz	not_ss_space
	test	ax,SS_SPACE
	jnz	not_ss_space

	mov	cl,VwOfVk(VK_SPACE)
	mov	bx,sp				;* fUp = TRUE
	cCall	KeyboardMessageShort

not_ss_space:
	
;{{
;	ssOld = ssNew;
;}}
	mov	ax,ssNew
	mov	[di].ssOld,ax

end_do_shift:

cEnd	DoShift


;********** KeyboardMessageShort **********
;*	entry : cl = vw
;*		bx = fUp
;*		ax = ssNew
;*		DS:DI => driver data
;*	* Call KeyboardMessage
;*	exit : n/a
;*	RETAINS : ax, dx

cProc	KeyboardMessageShort, <NEAR>, <AX, DX>
cBegin	KeyboardMessageShort

	AssertEQ di,OFF_lpwDataKbd

IFDEF	KANJI
	xor	dx,dx
	push	dx				;* sc
ENDIF	; KANJI
	xor	ch,ch
	push	cx				;* vw
	inc	ch				;* vw -> vk
	push	cx				;* vk
	cCall	KkOfSs, <ax>			;* KkOfSs(ssNew)
	push	ax
	push	bx				; fUp

	mov	bx,[di].pinkbCur
	cCall	[bx].lpfnKeyboardMessageInkb

cEnd	KeyboardMessageShort


;********** KkOfSs() **********
;*	entry : ssParm contains shift states
;*	* convert ss to kk values
;*	exit : AX = kk
;*	* TRASHES only AX

cProc	KkOfSs, <NEAR, ATOMIC, PUBLIC>
    parmW ssParm
cBegin	KkOfSs

	mov	ax,(ssParm)
;*	* the following code assumes the SS_ and KK_ values !!!
	Assert	<SS_SHIFT EQ 3>		;* lower 2 bits
	Assert	<SS_CONTROL EQ 4>
	Assert	<SS_ALT EQ 8>
	Assert	<SS_SCRLOCK EQ 10H>
	Assert	<SS_NUMLOCK EQ 20H>
	Assert	<SS_CAPLOCK EQ 40H>

	Assert	<SS_LSHIFT EQ HIGH(KK_SHIFT)>
	Assert	<SS_CONTROL EQ HIGH(KK_CONTROL)>
	Assert	<SS_ALT EQ HIGH(KK_ALT)>
	Assert	<SS_SCRLOCK EQ HIGH(KK_SCRLOCK)>
	Assert	<SS_NUMLOCK EQ HIGH(KK_NUMLOCK)>
	Assert	<SS_CAPLOCK EQ HIGH(KK_CAPLOCK)>

	test	al,SS_RSHIFT
	jz	not_rshift
	or	al,SS_LSHIFT		;* set lshift to reflect total shift
not_rshift:
	and	al,SS_CAPLOCK OR SS_NUMLOCK OR SS_SCRLOCK OR SS_ALT OR SS_CONTROL OR SS_LSHIFT
	xchg	al,ah			;* convert ss to kk
ifdef KANJI
	test	al,HIGH(SS_EXTENDED)
	jz	no_extend
	or	ah,HIGH(KK_EXTENDED)
no_extend:
	and	al,KJ_KANA
else
	and	al,HIGH(SS_EXTENDED)
	jz	no_extend
	xor	al,al
	or	ah,HIGH(KK_EXTENDED)
no_extend:
endif

cEnd	KkOfSs

;*****************************************************************************
