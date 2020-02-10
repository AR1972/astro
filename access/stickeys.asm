;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center (just kidding)		*
;									*
;	STICKEYS.ASM							*
;									*
;************************************************************************

TITLE	StickeyKeys

INCLUDE	keyboard.inc


	EXTRN	shift_tbl:byte				; in Handicap.asm
	EXTRN	shift_tbl_len:abs
	EXTRN	current_shift:byte
	EXTRN 	real_states:byte
	EXTRN	prev_real_states:byte
	EXTRN	comp_flag:byte
	EXTRN	beep_high:PROC
	EXTRN	beep_low:PROC
	EXTRN	click:PROC
	EXTRN	no_beep:PROC
	EXTRN	beep_turn_on:PROC
	EXTRN	beep_turn_off:PROC
	EXTRN	key_data_tail:word
	EXTRN	key_data_head:word
	EXTRN	key_data:word
	EXTRN	key_data_end:word
	EXTRN	Put_Key_Data:PROC
	EXTRN	_finject_keys:byte
	EXTRN	_vector:byte
	EXTRN	fswitching_video:byte
	EXTRN	faccess_sound:byte
	EXTRN	fsecond_cue:byte
	EXTRN	fkey_not_passed:byte

	EXTRN	fSticKeysOn:byte   			; from Param.asm
	EXTRN	fSK_On_Off_Feedback:byte
	EXTRN	fAudible_Feedback:byte
	EXTRN	fTriState:byte
	EXTRN	fTwo_Keys_Off:byte	 
	EXTRN	fDialog_Stickeys_off:byte
	EXTRN	fstickeys_click:byte
	EXTRN	fFilterKeysOn:byte	
	EXTRN	_comp_id:byte

	EXTRN	MouseKeys:PROC				; in Mousekey.asm

	EXTRN	TimeOut_Reset:PROC			; in Toggle.asm

	EXTRN	on_wait_ticks:word			; in Filter.asm

IFDEF	BUG

	EXTRN	portid:byte
	EXTRN	portout:byte
	EXTRN	HexCharsOut:PROC

ENDIF; BUG

	PUBLIC	SticKeys_TurnOff			; in StickeyKeys.asm
	PUBLIC	SticKeys,key_cnt
	PUBLIC	shift_flg,lock_flg,fkeys_injected
	PUBLIC	StickeyKeys_dialog
	PUBLIC	set_shift_states
	PUBLIC	flatch
	PUBLIC	shift_flg

;----------------------------------------------------------------------------
_TEXT	segment	word public 'CODE'

	assume CS:_TEXT
	assume DS:NOTHING
	assume ES:NOTHING
	assume SS:NOTHING

;----------------------------------------------------------------------------
;			R E S I D E N T   D A T A   A R E A
;
;  The below area defines the resident data area.
;
;----------------------------------------------------------------------------
SetShiftTable	label	word

	DW	fRShift_word
	dw	LShift
	dw	RShift

	DW	fLShift_word
	dw	RShift
	dw	LShift

;----------------------------- break of 4 count

	DW	fLCtrl_word
	dw	RCtrl
	dw	Ctrl

	DW	fLAlt_word
	dw	RAlt
	dw	Alt

	DW	fRCtrl_word
	dw	Ctrl
	dw	RCtrl

	DW	fRAlt_word
	dw	Alt
	dw	RAlt

SetShiftTableLen	equ	6


flgs		label	word

shift_flg		DB	0		; shift flags
lock_flg		DB	0		; lock flags


key_cnt			DB	0		; key count for turning on and off
key_to_pass		DW	0		; holds key to pass on ( ax-->ah,al)


shift_break_cnt		DB	0  		; holds the number of successive received shift breaks
flatch			DB	false		; flag to tell set_shift_states the latch key to clear
flock			DB	false		; flag to tell set_shift_states the lock key to clear
ftwo_down		DB	false		; flag to tell when two keys are down at the same time
fkeys_injected		DB	false		; flag to tell us that we put modifier break codes into Key_Data buffer
fopp_mod_sent		db	false		; flag used during injection of keys to control # of loops
;fmodel_25_30_latch	db	false		; flag used with PS/2 Model 25/30-86 during latching condition only


;----------------------------------------------------------------------------
; 
; The StickeyKey routine provide the user who may be a 1-finger typist, use a mouthstick
; or head pointer the ability to activate mutiple keystrokes sequentially instead of
; the standard simultaneous method.  For example, the Ctrl-Alt_Del. three simultaneous 
; sequence will reboot the IBM computer.  A 1-finger typist cannot depress all three keys 
; at the same time.  With StickeyKeys, the user need only press each key in order for the
; same desired result.
;----------------------------------------------------------------------------

SticKeys	proc	

	jmp	stickeys_begin	

;----------------------------------------------------------------------------
sk_turn_on_fb	proc	

	assume	DS:_TEXT

	cmp	fSK_On_Off_Feedback,false
	je	SticKeys_TurnOn
	call	beep_turn_on

SticKeys_TurnOn:
	cmp	fswitching_video,false
	jne	SticKeys_TurnOn_5
	mov	faccess_sound,true
	mov	fsecond_cue,24

SticKeys_TurnOn_5:

	mov	fSticKeysOn,true
sk_Restart:
	call	TimeOut_Reset
	mov	ftwo_down,false				; clear this flag 
	mov	fkeys_injected,false			; clear this flag at turnon
	mov	key_cnt,0				; reset key_cnt to 0
	mov	shift_break_cnt,0			; re-zero count 
;;	mov	fmodel_25_30_latch,false
	ret
sk_turn_on_fb	endp

;-----------------------------------------------------------------------------

two_keys_down	proc	near

	assume	DS:_TEXT

	cmp	fTwo_Keys_Off,false			; turn off at two keys?
	je	short two_keys_down_end
sk_turn_off_fb:
	cmp	fSK_On_Off_Feedback,false
	je	SticKeys_TurnOff
	call	beep_turn_off

SticKeys_TurnOff:
	cmp	fswitching_video,false
	jne	SticKeys_TurnOff_5
	mov	faccess_sound,true
	mov	fsecond_cue,25

SticKeys_TurnOff_5:

	mov	fSticKeysOn,false			; turn off
	mov	key_cnt,0				; reset key count
	mov	shift_break_cnt,0			; re-zero count 

set_real_states:
; currently, will shut off all modifiers when turned off, so if operator
; chooses to hold another modifier during shutoff, then it maybe cleared also 

	call	set_shift_states

	mov	ftwo_down,false				; clear this flag receiving a modifier key	
	mov	real_states,0
	mov	prev_real_states,0

two_keys_down_end:

	mov	flgs,0					; reset lock and shift flags 
	ret

two_keys_down	endp

;----------------------------------------------------------------------------
; StickeyKeys_dialog 
;
;	Checks a single flag from Dialog box to see if StickeyKeys was turned
; on/off.  If it was, this routine makes a call to Stickeys_TurnOn/Off so the realstates
; can be updated to match the computer.  Upon exit the flag that was set by 
; Dialog box is cleared.(returned to false)


StickeyKeys_dialog	proc	

	assume	DS:_TEXT

	cmp	fSticKeysOn,true			; was Stickeys turned on
	jne	Sd_25
	call	SticKeys_TurnOn_5 			; yes it was true, so turn on
	jmp	short Sd_50

Sd_25:
	call	SticKeys_TurnOff_5			; fSticKeysOn was false, so turn off
Sd_50:
	mov	fDialog_Stickeys_off,false		; reset flag to false
	ret

StickeyKeys_dialog	endp

;----------------------------------------------------------------------------
; set_shift_states
;
; assumes dl = gets set to the state to clear, if called to unlatch or unlock 
;

set_shift_states	proc

	push	ax
	push	bx
	push	cx
	push	es
	push	dx
	push	di
	push	si

	assume	ES:NOTHING
	mov	bx, RAMBIOS				; BIOS RAM segment at 40h
	mov	es,bx					;  .. point ES to that!
	assume	ES:RAMBIOS
	assume	DS:_TEXT


SticKey_On:

	mov	cx,SetShiftTableLen			; get length of table in cx
	mov	si,offset SetShiftTable			; get address of first word into si

	cmp	fSticKeysOn,false			; is Stickeys Off ?
	jne	sss_20					; No, Stickeys is ON, jmp to here
sss_10:

; If we get here, we now know that fStickeysOn = false and we have been called by either
;
;	1. Dialog Box of AccesDos Main Menu
;	2. A deactivation call, (i.e. 5 taps of the shift key)
;	3. Two key down turn off


	mov	dl,shift_flg				; need to clear any/all flags which might be set
	or	dl,lock_flg				; all modifiers !!!

; If we get here, check if Stickeys was turned off by two_keys_down

	cmp	ftwo_down,true				; was set_shift_states	called from two keys down ?
	je	sss_15					; yes, so jump to get only the locked and latched 
							; modifiers which are not actually currently pressed
	jmp	state_check				; if not from two_keys_down, jump to state_check and 
							; release all latched and locked modifiers
sss_15:

; Stickeys is Off from two keys down, but we need to only unlock any locked modifiers that are different from the
; modifiers which triggered the two_keys_down call.  dh= real_states which reflects exactly what keys are actually
; pressed down (break code not yet passed), where as dl=lock flag of those modifiers which are locked down, which may
; or may not be the key which triggered a two_key_turnoff. Also need to unlatch any other modifiers which the 
; release of the two_key_down will not unlatch.

	cmp	dh,dl
	jne	sss_18					; if the two_key_down is not the same as locked and latched, cont.
	jmp	set_shift_states_end			; if no other keys are latched or locked down, exit from a two_key_turnoff
sss_18:
	
	not	dh					; compliment real_states
	and	dl,dh					; "and" locked and complimented real states, which should leave 
	jmp	state_check	

sss_20:

; Stickeys is currently On, do we want to unlatch or unlock ?

	cmp	flatch,true				; is this an unlatch ?
	jne	undo_lock

undo_latch:						; if jump here, undo all the latched keys

	mov	dl,shift_flg				; move current shifted modifier flgs to dl
	jmp	state_check


;;	cmp	_comp_id,6				; do we have a PS/2 Model 25/30-86 ?
;;	jne	state_check				; if not the goofy PS/2 Model 25/30-86, cont. on
;;
;;;------------------------------------------------------------------------------------------------------------------------
;;; If we get here, it is the "goofy" PS/2 Model 25/30-86, which cannot recieve injected scan codes, so we must do some
;;; messing around to make it work.  If it is an unlatching condition, then we must set some flags and call when the next key is 
;;; entered, otherwise the flag bit is cleared before the shifted key is sent to the computer, effectively
;;; undoing the latch before it occurs.  For a locked condition, this is not a problem, so if the locked flag is set, we can 
;;; proced as normal.  For a latched condition, I will try to set another flag, and unlatch when another key is sent for
;;; this "goofy" computer only......in other words, clear kb_flag bits on the break or release of the intended key to
;;; be shifted, versus the make as normally done.
;;
;;
;;	cmp	fmodel_25_30_latch,true
;;	je   	state_check				; if called from the break, procede
;;
;;	mov	fmodel_25_30_latch,true			; if called from the normal make, set flag 
;;							; and exit, as break will undo flag bits
;;	jmp	set_shift_states_end_5
;;
;--------------------------------------------------------------------------------------------------------------------------

undo_lock:						; if jump here, undo the single current shift LOCKED KEY ONLY

	mov	dl,current_shift			; this is the modifier key to unlock
;;	jmp	state_check
							; modifiers not being pressed to be turned off
state_check:

	lodsw						; get flags for bits to clr (real and opposite bits)
	mov	bx,ax					; get a copy in bx

	lodsw						; get second word off table (opposite scan code)
	mov	di,ax					; save a copy in di

	lodsw						; get third word off table, correct scan code, stored in AX for later use
	and	bl,dl
	jnz	clear_flag				; no, get next set of flags
	jmp	check_loop

clear_flag:

; If we cont. here, then we found the modifier key which needs to have it's flag in BIOS reset, so we either
; "twiggle" the appropriate bit(s) or if _finject_keys is true, we can inject the scan code(s) back into the hardware buffer

	cmp	_finject_keys,true			; can we inject keys
	je	clear_scancode				; if not, check which vector compute we are 

;;	cmp	_comp_id,6				; do we have a PS/2 Model 25/30-86 ?
;;	je	clear_flag_3				; if yes, then goofy PS/2 Model 25/30-86 and we toggle bits for StickeyKeys

	cmp	_vector,15h				; are we on a computer supporting int 15 ?
	je	clear_scancode				; if we can, jump here

;-------------------------------------------------------------------------------------------------------------------------
; This area of code runs for int 9h computers and must access the BIOS flags to undo StickeyKeys
; latch and lock states.  This is very similar to how Trace program 1-finger operates

clear_flag_3:

; We must first check where we are in the SetShiftTable
	cmp	cx,2					; are we at the right ctrl or alt ?
	jg	clear_flag_3A				; no we have cx = 3...6, so we have l/r shift, lctrl or lalt
	shr	bl,1					; yes, cx=1 or 2, so we must adjust bit flag on these only(divide by 4)
	shr	bl,1					; bl was "20 or 10, and is now 08 or 04"

clear_flag_3A:

	not	bl					; clear the intended flag bit
	and	es:kb_flag,bl				; 

	cmp	cx,4					; if lshift or rshift, don't bother kb_flag_1/3
	jle	clear_flag_5

	cmp	flock,true				; are we unlocking ??
	je	clear_flag_4				; no, and it was a shift key, so out
	jmp	check_loop

clear_flag_4:

	not	bh					; yes we are unlocking, so get other shift key
	and	es:kb_flag,bh
	jmp	check_loop

clear_flag_5:

	cmp	comp_flag,true				; do we have BIOS support of kb_flag_1/3 ?
	je	clear_flag_7
	jmp	check_loop				; if not, quit at this point

clear_flag_7:
; must determine where we are in the SetShiftTable again

	cmp	cx,2					; are we at the right ctrl or alt ?
	jle	clear_flag_10
	not	bh
	and	es:kb_flag_1,ah

	cmp	flock,true				; are we unlocking ???
;	jne	check_loop				; no
	je	clear_flag_8				; 
	jmp	check_loop

clear_flag_8:

	and	es:kb_flag_3,bl
	jmp	check_loop


clear_flag_10:

	not	bh
	and	es:kb_flag_3,bh

	cmp	flock,true				; are we unlocking ???
	jne	check_loop				; no
	shr	bl,1					; yes, cx=1 or 2, so we must adjust bit flag on these only(divide by 4)
	shr	bl,1					; bl was "08 or 04, and is now 02 or 01"
	and	es:kb_flag_1,bl
	jmp	check_loop

;-----------------------------------------------------------------------------------------------------
; This area of code runs on computers which can either hardware inject scan codes into the keyboard and mouse
; buffers or can use int 15h to inject scan codes when called from int 9h.  Both method of 
; scab code injection are new to DOS programs written at Trace.
;
; cx = count into the SetShiftTable that we found the necessary modifier at 
;
clear_scancode:

	mov	fkeys_injected,true			; if it is a lock reset, then we need to set this flag

clear_scan_10:
; AX has the scan code to be injected for the modifier found to match the flag bit

	cmp	ah,0					; was there a hidden code ?
	je	clear_scan_25				; if ah = 0, then there wasn't any hidden code
	xchg	al,ah					; put ah into al prior to call

	call	Put_Key_Data				; put the scan code for the key into the buffer
	xchg	al,ah					; restore al

clear_scan_25:
	or	al,break_bit				; make it a break scan code for whatever
	call	Put_Key_Data				; put the scan code for the key into the buffer

	cmp	flock,true				; is this an unlock, then we must get the opposite modifier
	jne	check_loop				; if not, then loop to next key

; while in this loop, one computer type, AT 239, which supports int 15h, but uses an 84 key keyboard
; so comp_flag will be false since there in no right control or alternate keys.  We need to watch for this.

	cmp	comp_flag,true				; if true, we are okay
	je	clear_scan_35
	cmp	cx,4					; if not true, are we doing shift keys ? or ctrl or alt. keys ?
	jle	clear_scan_40				; if cx=4 or less, we are in the ctrl or alt. keys, and 
							; the AT 239 doesn't have any right ctrl/alt. so jump

clear_scan_35:

	cmp	fopp_mod_sent,true			; flag so we only sent one opposite modifier key
	je	clear_scan_40				; if true, we already sent the opposite modifier, so go to loop
	mov	fopp_mod_sent,true			; flag ourselves once through this inner loop
	mov	ax,di					; DI register has opposite modifier key from above

	jmp	clear_scan_10				; repeat same inner loop for opposite modifier

clear_scan_40:
	mov	fopp_mod_sent,false
check_loop:
	loop	check_loop_15
	jmp	short set_shift_states_end		; if cx =0, quit

check_loop_15:
	jmp	state_check

set_shift_states_end:

	mov	flock,false  				; reset to clear the condition flags
	mov	flatch,false

set_shift_states_end_5:

	pop	si
	pop	di
	pop	dx
	pop	es

	assume	ES:NOTHING

	pop	cx
	pop	bx
	pop	ax

	ret

set_shift_states	endp

;----------------------------------------------------------------------------
; SticKeys
;
;   This is the beginning of the routine for 1-finger operation of the
; keyboard.  It expects the raw scan code to be in AX where AH holds E0, E1 
; or nothing, and AL is the scan code.
;-----------------------------------------------------------------------------

stickeys_begin:

	push	bx
	assume	ES:NOTHING
	mov	bx, RAMBIOS				; BIOS RAM segment at 40h
	mov	es,bx					;  .. point ES to that!
	assume	ES:RAMBIOS
	pop	bx

	assume	DS:_TEXT

	mov	key_to_pass,ax				; save current key
	mov	dh,real_states
	mov	dl,prev_real_states
	mov	prev_real_states,dh			; update previous
	mov	ah,current_shift			; get bit flag of shift

	or	ah,ah					; a modifier key (ie. shift, ctrl, or alt)?	
	jnz	sk_010					; yes, a modifier key 
	jmp	ns_010					; non-shift key or fake shift keys

;----------------------------------------------------------------------------
;			s h i f t   k e y   r o u t i n e
;
; assumes 
;	al = scan code
;	ah = shift flag
;	dl = previous real states
;	dh = real states
;
; filter the shift key.  If E1 precedes shift, then it is always a fake shift.
; If E0 precedes shift and it is a left or right shift, then it is a fake 
; shift.  Fake shifts just get passed on.
;

sk_010: 

;
; We don't want the typematic make of shift to cause it to be turned
; on so we will only count number of breaks.  We also don't want typematic
; of shift make to change hold/lock state of stickeys so we will only pass
; on the first make of a shift through stickeys, and pass all typematic of
; shift key on through to mousekeys.
;
; added another test to check for two shift breaks in a row, as this is the
; check to prevent co-activation of StickeyKeys when connected to the T-TAM.
;

	test	al,break_bit				; a break?
	jnz	sk_020					; yes, check if manual on off
	mov	shift_break_cnt,0			; re-zero count if not a break
	test	dl,ah					; was it previously set?
	jz	sk_040					; no, is first make of shift
	jmp	pass_on					; not first make so just pass on

;----------------------------------------------------------------------------
; check if manually turned on or off by five shift keys in a row
;
sk_020:

	mov	cl,shift_break_cnt			; get current number of successive shift break counted
	inc	cl					; increment it
	mov	shift_break_cnt,cl			; keep track of current count
	cmp	cl,2					; have we had two breaks in a row without a make?
	jne	sk_040					; no, jump around

	cmp	fSticKeysOn,true			; are we on ???
	jne	sk_035					; if no, and if we get more than 1 shift key break in a row, reset
							; since this is a trap for the T-TAM
; if we are on,and get mutiple shift break keys in a row, check if the computer supports scan code injection, then this is okay

	cmp	_vector,15h				; can we inject or buffer ?
	jne	sk_035					; if not, something is wrong, so do our usual clean up
	dec	key_cnt					; if everything is okay, we need to reset key_cnt back by two 
	dec	key_cnt					; (we got two extra shift breaks from a unlock condition)
	cmp	key_cnt,0				; one final check to be sure key_cnt didn't go negative
	jge	sk_040					; if key_cnt >= to 0, cont. else re-zero in next line
							

sk_035:
	mov	key_cnt,0				; yes, re-zero on/off key count
	mov	shift_break_cnt,0			; re-zero count 

sk_040:
	mov	cl,key_cnt				; increment key cnt
	test	ah,fShift				; is shift an on/off shift?
	jz	sk_090					; neither on or off shift -->
	inc	cl
	cmp	cl,10					; have we reached 10 decimal
	jl	sk_100					; no -->
	test	al,break_bit				; a break?
	jz	sk_100					; no, simply update
	cmp	fSticKeysOn,true			; is it currently on?
	jne	sk_060					; no, then turn on -->
	call	sk_turn_off_fb				; yes, so turn off
	jmp	pass_on
sk_060:							; turn on
	call	sk_turn_on_fb
	jmp	pass_on					; pass on -->
sk_090:
	xor	cx,cx					; clear key_cnt

;---------------------------------------------------------------------------
; was not turned on or off just now
;
sk_100: 
	mov	key_cnt,cl				; save on/off shift count
sk_105:
	cmp	fSticKeysOn,true			; are we on?
	je	sk_110					; yes -->
	jmp	pass_on					; no, just pass on -->

;----------------------------------------------------------------------------
; process a filtered shift key.
; 
sk_110:

	mov	cx,flgs					; ch = lock_flg, cl = shift_flg
	test	al,break_bit				; break of shift flag?
	jz	sk_200					; no, process make shift key -->

;----------------------------------------------------------------------------
; process the BREAK of a shift (modifier) key
;
; There are no modifier break codes of E0AA and E0B6....These are fake modifiers with these codes
; but they should be passed as non-shift keys to ns_010, and not pass thru here.
;
;;	cmp	ax,0E0AAh				; do we have a fake shift break ?
;;	je	sk_115					; yes, just pass it on
;;	cmp	ax,0E0B6h				; do we have a fake shift break ?
;;	je	sk_115	

	or	ch,cl					; ch=locked or shifted
	test	ch,ah					; either lock or shift set?
	jnz	sk_eoi					; yes, don't pass on

sk_115:

	jmp	pass_on
sk_eoi: 
	and	es:kb_flag_3,0fch			; clear the "e0" and "e1" bits
	jmp	SticKeys_end				; end


;----------------------------------------------------------------------------
; process the MAKE of a shift (modifier) key.  check for two shift (modifier) keys down
;
sk_200: 
	cmp	dh,ah					; is another shift key down?
	je	sk_300					; no, go on -->
	mov	ftwo_down,true				; we have two keys down together
	call	two_keys_down
	cmp	fSticKeysOn,true			; are we still on? after the two_keys_down proc.
	je	sk_300					; yes, then cont. as normal
	jmp	pass_on					; no, StickeyKeys got shut off, so pass on this second modifier
							; key which is down, and the first modifier key down will
							; get passed around StickeyKeys since it will be off 

;----------------------------------------------------------------------------
; process tri-state or bi-state shift (modifier) key states 

sk_300: 
;	cmp	fjust_unlocked,false			; are weawaiting a break ?
;	je	sk_305					; if not, continue on
;	jmp	SticKeys_end				; if yes, eat present modifier key
;sk_305:

	mov	bx,fShift_word				; check for left/right shift exception
	test	ah,bl					; is current key a shift?
	jnz	sk_310					; yes, --> check for locked
	mov	bx,fCtrl_word				; check for left/right ctl exception
	test	ah,bl					; is current key a ctl?
	jnz	sk_310					; yes, --> check for locked
	mov	bx,fAlt_word				; check for left/right alt exception
sk_310: 
	test	ch,bl					; is either left or right locked?
	jz	sk_320					; no -->
	not	bx					; yes, release the keys
	and	cx,bx					; clear lock and shifts
	mov	flock,true
;	mov	fjust_unlocked,true			; flag ourselves that we just unlocked, so we don't latch 
;							; or start another lock until we see a break code
	call	set_shift_states			; set the states
	jmp	short sk_340				; continue with feedback -->
sk_320: 
	mov	bx,cx	     				; get a copy in bx
	or	bl,bh					; bl= shift or lock previously set
	test	ch,ah					; was lock previously set?
	jnz	sk_330					; yes, don't toggle shift -->
	xor	cl,ah					; no, then toggle shift
sk_330: 
	cmp	fTriState,true				; are we doing tri-state shifts?
	jne	sk_340					; no -->
	test	bl,ah					; was shift or lock prevously set?
	jz	sk_340					; no, don't toggle lock -->
	xor	ch,ah					; yes, then toggle lock
sk_340: 
	mov	flgs,cx					; save lock and shift states
;----------------------------------------------------------------------------
; the following routines provide the sound feedback.
; sound feedback, expects ch=lock states cl=shift states ah=flag

fb_010: 
	cmp	fAudible_Feedback,true			; do they want feedback?
	jne	sk_400					; no -->

	test	ch,ah					; are we in lock state?
	jnz	fb_020					; yes, just give high tone
	call	beep_low
	cmp	fswitching_video,false
	jne	fb_020
	mov	faccess_sound,true
	mov	fsecond_cue,9
fb_020: 
	test	ch,ah					; was there a lock?
	jnz	fb_030					; yes -->
	test	cl,ah					; was there a shift?
	jz	fb_end					; no
fb_030:
	call	beep_high
	cmp	fswitching_video,false
	jne	fb_end
	mov	faccess_sound,true
	mov	fsecond_cue,7
fb_end: 

;----------------------------------------------------------------------------
; determine if need to send on this modifier  key

sk_400: 
	test	ch,ah					; are we in lock state?
	jnz	sk_410					; yes, just give high tone
	cmp	fswitching_video,false
	jne	sk_410
	mov	faccess_sound,true
	mov	fsecond_cue,9
sk_410:

	test	ch,ah					; was there a lock?
	jnz	sk_415					; yes -->
	test	cl,ah					; was there a shift?
	jz	sk_420					; no
sk_415:
	cmp	fswitching_video,false
	jne	sk_420
	mov	faccess_sound,true
	mov	fsecond_cue,7

sk_420:
	test	cl,ah					; is the shift flag set?
	jnz	sk_pass_on				; yes, pass on -->
	and	es:kb_flag_3,0fch			; clear the "e0" and "e1" bits
	jmp	SticKeys_end				; no, inhibit -->
sk_pass_on: 
	jmp	pass_on					; -->

;----------------------------------------------------------------------------
;		non modifier keys (NOT a shift, alternate or control)
;
;		n o n - s h i f t   k e y
;
; assumes dh=real_states, al=scan code 
;
; current key is a non-shift or a fake shift.  Treat fake shifts like 
; non-shift keys since they are always followed by a non-shift down, so we
; want to set the real states before these fake shift keys get sent.
; check for two keys pressed.

ns_010:
	mov	key_cnt,0				; reset on/off key count
	cmp	fSticKeysOn,false			; are we off?
	je	pass_on					; yes, pass on -->
	or	dh,dh					; is a shift key down?
	jz	pass_on_non_shift			; no  --> pass_on_non_shift
	mov	ftwo_down,true				; we have two keys down together
	call	two_keys_down				; take care of two key down case

;----------------------------------------------------------------------------
; if we get here, then we have a modifier key and a non-modifier key down together
; pass scan code one to next routine and clear flags if NOT locked.

pass_on_non_shift:

	mov	ax,key_to_pass

; Assumes MouseKeys saves nothing on return

	cmp	fSticKeysOn,true			; did two_keys_down shut us off ?
	jne	pass_on					; if yes, quit

	cmp	fstickeys_click,false			; do a click in StickeyKeys ?
	je	ns_100					; if not, we don't need to check any further

; if we are going to click, be sure we don't have Filterkeys on and that it
; already did a click.  FIlterKeys will do a click if click is enabled, and there is an 
; acceptance delay.  We need to check if there is an acceptance delay first, as we would
; never StickeyKeys click for that, or we would hear alot of clicking, without any keys.

	cmp	fFilterKeysOn,true
	jne	ns_80					; if FilterKeys is off, safe to do StickeyKeys click
							; if FilterKeys is On, if we have an acceotance delay,
							; do not do a click here, as we would get multiple clicks
	cmp	on_wait_ticks,0				; if there is an acceptance delay, do not click in StickeyKeys
	jne	ns_100

ns_80:
	call	click					; StickeyKeys click
ns_100:

	call	MouseKeys

	mov	ax,key_to_pass
	cmp	ax,0e02ah	
	je	short SticKeys_end			; don't reset if fake shift
	cmp	ax,0e036h	
	je	short SticKeys_end			; don't reset if fake shift
ns_150:
	cmp	shift_flg,0				; was any key StickeyKeyed ?
	je	SticKeys_end				; if not, end
							; if yes, set latch flag and reset modifier(s)

  	mov	flatch,true				; set flag so we know what kind of flag clearing to do

	cmp	fkey_not_passed,true			; this will only occur on int 9, FilterKeysOn 
	jne	ns_160					; if not set, cont. as normal
	jmp	short SticKeys_end			; if set, don't reset flags as yet

ns_160:

	call	set_shift_states			; clear latched key(s) states
	mov	shift_flg,0				; clear shifted states
	jmp	short SticKeys_end

;----------------------------------------------------------------------------
; just pass on current key to next routine

pass_on:

	mov	ax,key_to_pass

; Assumes MouseKeys saves nothing on return

	call	MouseKeys

SticKeys_end:

	ret

SticKeys	endp

_TEXT	ends

	end	







