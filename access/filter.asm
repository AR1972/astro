;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center (just kidding)		*
;									*
;	FILTER.ASM							*
;									*
;************************************************************************

TITLE	FilterKeys

INCLUDE	keyboard.inc

	EXTRN	shift_tbl:byte				; in Handicap.asm
	EXTRN	shift_tbl_len:abs
	EXTRN 	real_states:byte
	EXTRN	prev_real_states:byte
	EXTRN	current_shift:byte
	EXTRN	beep_high:PROC
	EXTRN	beep_low:PROC
	EXTRN	no_beep:PROC
	EXTRN	beep_turn_on:PROC
	EXTRN	beep_turn_off:PROC
	EXTRN	click:PROC
	EXTRN	_comp_id:byte
	EXTRN	fsilent_click:byte
	EXTRN	fbios_called_timing:byte
	EXTRN	_vector:byte
	EXTRN	fswitching_video:byte
	EXTRN	faccess_sound:byte
	EXTRN	fsecond_cue:byte

	EXTRN	SticKeys:PROC	  			; in StickeyKeys.asm

	EXTRN	TimeOut_Reset:PROC			; in ToggleKeys.asm

	EXTRN	fFilterKeysOn:byte			; from Param.asm
	EXTRN	fFK_On_Off_Feedback:byte
	EXTRN	fUser_SetUp_Option1:byte
	EXTRN	fUser_SetUp_Option2:byte
	EXTRN	wait_ticks:word
	EXTRN	delay_ticks:word
	EXTRN	repeat_ticks:word
	EXTRN	fmax_default:byte
	EXTRN	fclick_on:byte
	EXTRN	fDialog_Filter_off:byte

	EXTRN	fRecovery_On:byte
	EXTRN	recovery_ticks:word


	PUBLIC	FilterKeys				; in FilterKeys.asm
	PUBLIC	FilterKeys_timer
	PUBLIC	FilterKeys_TurnOff
	PUBLIC	FilterKeys_dialog
	PUBLIC	fshift_click
	PUBLIC	on_repeat_ticks	     
	PUBLIC	on_wait_ticks

IFDEF	BUG

	EXTRN	HexCharsOut:PROC
	EXTRN	portid:byte
	EXTRN	portout:byte

ENDIF; BUG

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


repeating?		DB	false		; beginning repeat
hot_key_only?		DB	false		; only hot key is down
previous_code		DW	0  		; holds previous code
prior_code		DW	0		; holds code before previous code
tick_count		DW	0		; holds the actual count that gets dec
toggle_time		DW	reset_cnt	; holds count for toggle time
fshift_click		DB	false		; holds a flag such that a modifier only clicks once
frecovery_timing	DB	false		; flag used to tell us that recovery_ticks is counting out by timer

on_wait_ticks		DW	0		; holds wait tick count while running
on_delay_ticks		DW	0		; holds delay tick count while running
on_repeat_ticks		DW	0		; holds repeat tick count while running
on_recovery_ticks	DW	0		; holds recovery tick count while running
on_delay_ticks_adj	DW	0		; holds adjusted value for delay ticks assuming recovery keys is on

;----------------------------------------------------------------------------
; FilterKeys
;
;   This routine does typeamatic rate adjustment and acceptance key timing and
;   the recovery keys routine.
;   It is called by the hardware interrupt routine keybd_int in the file, "handicap.asm".
;   FilterKeys does not depend on any register to be saved, but
;   we must save whatever we use since we are in an interrupt routine.  
;
; Expects:	AX = extended scan code
;
; Changes:	Nothing
;-----------------------------------------------------------------------------

FilterKeys	proc

	assume	DS:_TEXT
	assume	ES:_TEXT

	jmp	filter_begin

;----------------------------------------------------------------------------
; FilterKeys_timer 
;
; Increments the counts used in the turn on/off of FilterKeys and the
; acceptance, delay until repeat, and key repeat time out options.  


FilterKeys_timer	proc

	assume	DS:_TEXT

;----------------------------------------------------------------------------
; first check to see if we are manually being turned off or on.  To toggle 
; it on or off, we have it set so that the persom must hold down the
; carriage return key for at least eight seconds.  The person receives a 3 warning 
; beeps after 4 seconds.
;
FKT_10:
	cmp	hot_key_only?,false			; hot key not down?
	jne	FKT_12

	jmp	start_cnt_over				; no, it is down -->
FKT_12:

	mov	ax,toggle_time				; get current count
	cmp	ax,0					; are we at no repeat amount = 0?
	je	set_max_default				; yes -->
	jg	FKT_15

	jmp	begin_FilterKeys_timer			; less than, so already done

; Now check for toggle on/off of FilterKeys

FKT_15:
	dec	toggle_time				; no, decrement count
	cmp	ax,warning_cnt				; are we at the warning count ?
	je	warning_cnt_sub

	cmp	ax,toggle_dflt_cnt			; are we at toggle amount?
	je	turn_filter

	cmp	ax,no_accept_cnt			; are we at User setup option 1??
	je	no_accept_cnt_sub			; yes

	jmp	begin_FilterKeys_timer			; no -->

; Reached on/off time limit so toggle.

turn_filter:

	cmp	fFilterKeysOn,false			; are we off?
	je	turn_filter_on				; yes -->
	call	fk_turn_off_fb
	mov	toggle_time,to_dflt_cnt			; make toggle_time very large so if user
							; cont. to hold rshift key after FilterK/eys turns
							; off, it will not just turn back on.  After the user
							; finally releases the rshift key, then the correct count
							; is restored to toggle_time
	jmp	hot_key_end
turn_filter_on:
	call	fk_turn_on_fb
	jmp	hot_key_end

set_max_default:

	dec	toggle_time
	cmp 	fswitching_video,false
	jne	set_max_default_5
	mov	faccess_sound,true
	mov	fsecond_cue,24				; up arrow for up siren

set_max_default_5:

	cmp	fFK_On_Off_Feedback,false
	je	set_max_default_10

	call	beep_turn_on
	call	no_beep
	call	beep_turn_on
	call	no_beep
	call	beep_turn_on

set_max_default_10:

	mov	on_wait_ticks,max_dflt_wait_cnt
;;	mov	on_delay_ticks,32760			; put on_delay_ticks at maximum
	mov	fUser_SetUp_Option2,true		; set user option 2 true
	mov	fUser_SetUp_Option1,false		;reset flag
	jmp	hot_key_end

warning_cnt_sub:

	cmp 	fswitching_video,false
	jne	warning_cnt_sub_5
	mov	faccess_sound,true
	mov	fsecond_cue,7				; filled circle for high beep

warning_cnt_sub_5:

	call	beep_high
	call	no_beep
	call	beep_high
	call	no_beep
	call	beep_high
	jmp	hot_key_end

no_accept_cnt_sub:

	cmp 	fswitching_video,false
	jne	no_accept_cnt_sub_5
	mov	faccess_sound,true
	mov	fsecond_cue,24

no_accept_cnt_sub_5:

	cmp	fFK_On_Off_Feedback,false
	je	no_accept_cnt_sub_10

	call	beep_turn_on
	call	no_beep
	call	beep_turn_on

no_accept_cnt_sub_10:

	mov	fUser_SetUp_Option1,true		; set user option 1 true
	mov	fUser_SetUp_Option2,false		; set user option 2 false
	mov	on_wait_ticks,0				; in User_SetUp_Option1, the is no acceptance delay
	mov	on_delay_ticks,18			; default User_SetUp_Option1 BounceKeys of 1 second
	mov	on_delay_ticks,32760			; put on_delay_ticks at maximum
hot_key_end:
	jmp	FilterKeys_timer_end

;----------------------------------------------------------------------------
; NOT Hot Key down by itself.
;
start_cnt_over:
	mov	toggle_time,reset_cnt

;----------------------------------------------------------------------------
; check and see if it is time to send a key through, both delayed acceptance
; and repeat.

begin_FilterKeys_timer:
	cmp	fFilterKeysOn,false
	jne	check_time
	jmp	FilterKeys_timer_end			; if not on, quit
check_time:

	mov	ax,tick_count
	or	ax,ax					; are we at 0?
	jz	generate_key				; yes, then time to pass on key -->
	dec	ax					; no, decrement count
	jmp	set_ticks				; save value -->
generate_key:
	cmp	frecovery_timing,true			; is RecoveryKeys On, and a key already passed
	jne	generate_key_15				; if not, cont as before

	mov	frecovery_timing,false			; incase RecoveryKeys is On, we elapsed the timer
	jmp	short FKT_35				; reset counter to next delay (ie on_delay_ticks)

generate_key_15:

	mov	ax,previous_code			; get key to pass
	or	ax,ax					; is a key down?
	jnz	go_ahead
	jmp	FilterKeys_timer_end			; 

go_ahead:


;***********
; We do not expect any registers to be saved upon return from SticKeys
; Added call to Click for whenever SlowKeys is on.
;
;	cmp	_comp_id,1				; IBM PC or PC/XT ?
;	jne	FKT_25					; if so, we will click later in pass_to_computer
;	push	ax					; temp save
;	in	al,kb_data				; read in data from keyboard port
;	cmp	al,0					; was byte a 0??
;	jne	FKT_20					; if not, okay to click if on
;	pop	ax					; restore ax
;
;;;	cmp	fclick_on,true				; is click sound turned on
;;;	jne	FKT_16
;;;	call	click
;;;	jmp	short FKT_18
;;;FKT_16:
;	mov	fsilent_click,true			; flag to allow time for PC /XT
;	call	click
;FKT_18:
;	call	SticKeys				; pass on actual code
;	cmp	fsilent_click,true			; if we return from call to Stickeys, and fsilent_click
;	je	FilterKeys_timer_end			; is true, then al=0 here in Filters or in handicap (pass_to_computer)
;							; meaning no scan code was passed, so we need to jump to the end without
;							; changing the current delay we are in, and the next timer int. will try
;							; once again to pass this same scan code along
;
;	cmp	fclick_on,true				; if we get here, we passed a key, is click sound turned on
;	jne	FKT_19
;	call	click
;FKT_19:
;	jmp	short FKT_35
;
;FKT_20:
;	pop	ax					; restore ax
;***********


FKT_25:

;*	cmp	fclick_on,true				; is click sound turned on
;* 	jne	FKT_30

;; suggestion was made to not have the repeat of modifier keys cause a click, so added short check here to 
;; do this, but not to the PC or PC/XT (ax [al] has the scan code at this point)

;*	cmp	fshift_click,true			; was at least one modifier key passed with a click ?
;*	je	FKT_30					; is so, cont. on., if not, calll click and then set flag true
;*
;*	cmp	current_shift,0
;*	jne	FKT_30
;*	mov	fshift_click,true			; if we clicked once, alway set true, as any non modifier key will clear
;*	cmp	on_wait_ticks,0				; is there an acceptance delay ?
;*	je	FKT_30					; if not, don't click the modifier key at all
;*
;*
;***********
;FKT_27:
;	cmp	_vector,15h				; are we on a computer which supports int 15h
;	je	FKT_30					; if yes, save click for when key is actually pased int kybd int15
; 	call	click					; audible feedback that SlowKeys is on	
;***********
;*
;*
;*FKT_30:	

	mov	fbios_called_timing,true		; tell kybd_int 15h that timer passed a key
							; and do it before we call StickeyKeys, in case 
							; MouseKeys cancels it (i.e. MouseKeys not sent on)

	call	SticKeys				; pass on actual code

FKT_35:
	mov	ax,on_repeat_ticks    		 	; get on_repeat_ticks value
	cmp	repeating?,true				; are in typematic?
	je	set_ticks				; yes -->
	mov	repeating?,true				; no, then say we are repeating

	cmp	fRecovery_On,true			; is RecoveryKEys On ?
	jne	FKT_40					; if not, cont.
	mov	ax,on_delay_ticks_adj			; use value of on_delay_ticks_adj instead
	jmp	short set_ticks

FKT_40:
	mov	ax,on_delay_ticks    		  	; get on_delay_ticks
set_ticks:
	mov	tick_count,ax
FilterKeys_timer_end:
	ret
FilterKeys_timer	endp

;---------------------------------------------------------------------------

; the following routines provide the sound feedback.

fk_turn_off_fb	proc	near

	assume	DS:_TEXT

	cmp	fFK_On_Off_Feedback,false
	je	FilterKeys_TurnOff
	call	beep_turn_off

FilterKeys_TurnOff:
	cmp	fswitching_video,false
	jne	FilterKeys_TurnOff_5
	mov	faccess_sound,true
	mov	fsecond_cue,25				; down arrow for falling siren

FilterKeys_TurnOff_5:

	mov	fFilterKeysOn,false			; turn off
fk_Restart:
	mov	toggle_time,reset_cnt			; reset count down
	mov	previous_code,0				; clear previous code
	mov	prior_code,0				; clear as well
	mov	repeating?,false			; reset flag
	mov	fUser_SetUp_Option1,false		; reset flag
	mov	fUser_SetUp_Option2,false		; reset flag
	ret

fk_turn_off_fb	endp

;----------------------------------------------------------------------------

fk_turn_on_fb	proc	near

	assume DS:_TEXT

	cmp	fFK_On_Off_Feedback,false
	je	FilterKeys_TurnOn
	call	beep_turn_on

FilterKeys_TurnOn:
;;	call	TimeOut_Reset

	cmp	fswitching_video,false
	jne	FilterKeys_TurnOn_5
	mov	faccess_sound,true
	mov	fsecond_cue,24

FilterKeys_TurnOn_5:

	mov	fFilterKeysOn,true			; turn on

	push	ax

	mov	ax,delay_ticks
	mov	on_delay_ticks,ax

	mov	ax,repeat_ticks
	mov	on_repeat_ticks,ax

	mov	ax,recovery_ticks
	mov	on_recovery_ticks,ax

	mov	ax,wait_ticks
	mov	on_wait_ticks,ax

;******** DO NOT ALLOW RecoveryKeys to be on, if Acceptance Delay is NOT zero *****************

	cmp	ax,0					; is wait_ticks zero?
	je	FKTO_15					; if yes, then don't affect fRecovery_On flag, may be On or Off
	mov	fRecovery_On,false  			; if no, then be sure to NOT allow fRecovery_On to be ON

FKTO_15:
	pop	ax

	cmp	fRecovery_On,true			; if RecoveryKeys is On, setup on_delay_ticks_adj
	jne	FKTO_50

	cmp	on_delay_ticks,32760			; is delay_ticks "off" (i.ei no repeats)
	jne	FKTO_18					; if delay_ticks is not off, adjust it for RecoveryTicks
	mov	on_delay_ticks_adj,32760		; if it was off, also make on_delay_ticks_adj off
	jmp	short FKTO_50  				; and exit


FKTO_18:

	push	ax
	push	bx
	mov	ax,on_delay_ticks

;IFDEF	BUG
;	mov	portid,0ddh
;	mov	portout,ah
;	call	HexCharsOut
;	mov	portout,al
;	call	HexCharsOut
;ENDIF;	BUG

	mov	bx,on_recovery_ticks

;IFDEF	BUG
;	mov	portid,0deh
;	mov	portout,bh
;	call	HexCharsOut
;	mov	portout,bl
;	call	HexCharsOut
;ENDIF;	BUG
	cmp	ax,bx					; be sure delay ticks is greater than recovery ticks
	jg	FKTO_20
	mov	on_delay_ticks_adj,0			; if not greater, force on_delay_ticks_adj to 0
	jmp	short FKTO_30

FKTO_20:
	sub	ax,bx					; subtract  on_recoveryticks from on_delay_ticks
	mov	on_delay_ticks_adj,ax			; store that value in on_delay_ticks_adj

;IFDEF	BUG
;	mov	portid,0dfh
;	mov	portout,ah
;	call	HexCharsOut
;	mov	portout,al
;	call	HexCharsOut
;ENDIF;	BUG

FKTO_30:
	pop	bx
	pop	ax

FKTO_50:

	ret

fk_turn_on_fb	endp
;----------------------------------------------------------------------------
; FilterKeys_dialog 
;
;	Checks a single flag from Dialog box to see if FilterKeys was turned
; on/off.  If it was, this routine makes a call to FilterKeys_TurnOn/Off so the realstates
; can be updated to match the computer.  Upon exit the flag that was set by 
; Dialog box is cleared.(returned to false)


FilterKeys_dialog	proc	

	assume	DS:_TEXT
	cmp	fFilterKeysOn,true			; was FilterKeys turned on
	jne	Fd_25
	call	FilterKeys_TurnOn_5	   		; yes it was a 1, so turn on
	jmp	short Fd_50

Fd_25:
	call	FilterKeys_TurnOff_5			; fFilterKeysOn was false, so turn off
Fd_50:
	mov	fDialog_Filter_off,false		; reset flag to false
	ret

FilterKeys_dialog	endp

;----------------------------------------------------------------------------

filter_begin:

	push	ds					; save the world away
	push	es
	push	di
	push	si
	push	dx
	push	cx
	push	bx
	push	ax

;----------------------------------------------------------------------------
; first thing to do is to check for the right shift key or other modifier key
; (shift, control, and alternate keys) and keep track of the real
; states.  This is needed by the timer routine to determine if the 
; fFilterKeysOn is being manually toggled on and off and by
; the next routine in line, StickeyKeys.

	mov	bx,ax					; save copy in bx
	mov	current_shift,0				; assume not a shift key

	and	al,not break_bit			; get make scan code in al
	mov	cx,shift_tbl_len			; search for a modifier key
	mov	di,offset shift_tbl			; when done, cl will hold modifier
	cld						; count to get modifier flag
	repne	scasw					; ne=not found  e=found

	jne	FK_30					; not a modifier (shift, ctrl, alt) key -->

	mov	al,1
	shl	al,cl					; get modifier key flag

	mov	current_shift,al			; set current modifier to flag

;----------------------------------------------------------------------------
;
; Now set real states
;
	mov	ah,real_states				; get real current state
	test	bl,break_bit				; is it a break of a shift?
	jz	FK_10					; no -->
	not	al					; yes, clear flag
	and	ah,al
	jmp	short FK_20
FK_10:
	or	ah,al					; set flag
FK_20:
	mov	real_states,ah				; save

	cmp	bl,RShift	 	   		; is it a make of the right shift key ?
	jne	FK_32					; if not, jump around hot key setting

;----------------------------------------------------------------------------
;
; Now see if only the turn on hot key is down 
; changed hot key to carriage return key to match T-TAM
;

FK_25:
; not sure if I need this next test any more either 

	test	ah,fNotToggles				; any ctl, alt keys down?
	jnz	FK_30					; yes -->

	mov	hot_key_only?,true			; no, then set hot_key_only to true
	jmp	short FK_100
FK_30:
      	mov	fshift_click,false			; reset fshift_click flag for any non modifier keys
FK_32:

	mov	hot_key_only?,false

;----------------------------------------------------------------------------
; We now start the logic for the Filter features
;
FK_100:
	mov	ax,bx					; restore extended scan
	and	bl,not break_bit			; get make in bl
	cmp	fFilterKeysOn,false			; are we to filter?
	jne	FK_105					; yes, cont on
	jmp	FK_800					; no -->pass around

FK_105:
	test	al,break_bit				; is it a break?
	jz	FK_200					; no, it is a make -->

;----------------------------------------------------------------------------
; BREAK CODE HANDLING
;----------------------------------------------------------------------------
; must let all break keys go through immediately since they are only sent 
; once.  Check to see if break of current down key.  To be true to the
; function of FilterKeys, we should only let the break codes get passed
; on through if a make of the same code has been sent.  However, due to
; n-key roll over and single keys which send multiple makes and breaks, this
; is a complicated (i.e. more lines of code) to check for.

      	mov	fshift_click,false			; reset fshift_click flag for any non modifier keys
	cmp	previous_code,bx			; break of previous make?
	je	FK_110					; yes, cont. on
	jmp	FK_800					; no -->pass around
	
FK_110:
	cmp	fRecovery_On,true			; is RecoveryKeys On ?
	jne	FK_130
	mov	bx,on_recovery_ticks			; if it did time out, load or reeload counter
	mov	tick_count,bx
	mov	repeating?,false			; fake out FilterKeys timer, it will think it's doing acceptance delay
	mov	frecovery_timing,true			; set to true because we are going to pass this key press on 

FK_130:

	mov	previous_code,0				; yes clear previous code
	mov	repeating?,false			; reset flag
	jmp	FK_800					; pass key on -->

;----------------------------------------------------------------------------
; MAKE CODE HANDLING
;----------------------------------------------------------------------------
; Must make exception for fake keys that are only ever sent once since they
; are part of a sequence of keystrokes.  These keys are the left shift key 
; that is preceded by E0 (e02ah or e0aah), or the left ctrl that is preceded 
; by E1 (e11dh or e19dh).

FK_200:
	cmp	ax,0e02ah				; left shift make (keys 75-89)
	je	FK_230	     				; yes, pass on -->
FK_210:

	cmp	ax,0e036h				; right shift make (keys 75-89)
	je	FK_230	 				; yes, pass on -->
FK_220:
	cmp	ax,0e11dh				; key 126 PAUSE ?
	je	FK_230					; yes, pass on -->

;----------------------------------------------------------------------------
; Check for the special sequence caused by the key 126 (Pause).  The keyboard
; sends this sequence straight as shown.  It does not wait to the actual 
; release of the key to send the up codes sequence.  So we need to pass on
; the down stroke right away.  The up strokes will get sent right away since
; they are break keys.
;
; e0 46 e0 c6   or   (e1 1d) 45 (e1 9d) c5

	cmp	ax,0e046h				; is it the pause key?
	je	FK_230					; yes, send on -->
	cmp	previous_code,0e11dh			; previous one a 0e1dh ?
	jne	FK_300					; no, go on -->
	cmp	al,45h					; is current key a 45h?
	jne	FK_300					; yes, then pass on -->

FK_230:
       	jmp	FK_800

;----------------------------------------------------------------------------
; we will always inhibit a code that is identical to previous one since
; the initial key and repeat will always be done via the timer routine since
; this gives better resolution then depending on the keyboard.
;
FK_300:
	cmp	ax,previous_code			; equal to previous code?
							; previous code set to 0 on a break
	jne	FK_300A
	jmp	filter_ret				; bypass all typematic makes of a key

;-----------------------------------------------------------------------------
; Check if RecoveryKeys is On, if it is On, we will not pass a key along (make code)
; if that key is determined to be the same as the previous key (check prior key), and the 
; on_recovery_ticks counter has not elapsed.  If the on_recovery_ticks counter has elapsed,
; we will pass the make code along, and reset the frecovery_timing variable

FK_300A:
	cmp	fRecovery_On,true			; check if RecoveryKeys is On ?
;	jne	FK_300B					; if not, cont here
	jne	FK_306					; if not, cont here
	mov	bx,prior_code				; check make code against pre-previous code
	cmp	ax,bx					; same key after a tremor
;	je	FK_300B					; yes, allow recovery_timing to elapse
	je	FK_306					; yes, allow recovery_timing to elapse

; If not the same as the prior code, elapse frecovery_timing flag

	mov	frecovery_timing,false			; if user targetted a new key, reset this counter

;------------------------------------------------------------------------------
; Now determine if we should send the key on right away, and what value to set
; the tick_count, but first make sure that if fUser_SetUp_Option is set or checked,that we
; keep the settings for each. i.e. Don't allow thses settings to change unless operator
; leaves the User_SetUp_Option mode
;******************
; REMOVE DUE TO REMOVAL FROM DIALOG BOX
;******************
;;
;;
;;FK_300B:
;;	cmp	fUser_SetUp_Option1,true		; if set reset on_wait_ticks and on_delay_ticks
;;	je	FK_302
;;	cmp	fUser_SetUp_Option2,true		; if set reset on_wait_ticks and on_delay_ticks
;;	je	FK_304
;;	jmp	short FK_306	  			; not equal to either User SetUp Option 
;;
;;FK_302:
;;	mov	on_wait_ticks,0				; in User_SetUp_Option1, the is no acceptance delay
;;	mov	on_delay_ticks,32760			; put on_delay_ticks at maximum, i.e. do repeat keys
;;	jmp	short FK_306				; cont. on
;;FK_304:
;;	mov	on_wait_ticks,max_dflt_wait_cnt		; put at maximum
;;	mov	on_delay_ticks,32760			; put on_delay_ticks at maximum, i.e. do repeat keys


FK_306:

	mov	bx,on_wait_ticks			; reset count

FK_310:

; Now test to see if we have an acceptance delay time

	or	bx,bx					; any wait tick value?
	jz	FK_318

; Yes, have Acceptance delay time.  Let timer routine pass the initial make
; as well as the repeats.
; Added call to Click for whenever SlowKeys is on with an acceptance delay.

        cmp	fclick_on,true				; is click sound turned on
	jne	FK_315					; no, jump around
 	call	click					; audible feedback that SlowKeys is on
FK_315:
	mov	tick_count,bx				; yes, just set count
	mov	repeating?,false			; not repeating yet
	mov	previous_code,ax			; no, update previous code to new one
	jmp	short filter_ret    			; let timer send on the key

; No, we do not have an acceptance delay time.  Pass current make right now
; and let timer do the repeats. But also check for RecoveryKeys since we do not
; have any acceptance delay

FK_318:

	cmp	fRecovery_On,true			; check if RecoveryKeys is On ?
	jne	FK_320					; if not, cont here

	cmp	frecovery_timing,true			; if RecoveryKeys is On, has an earlier count timed out ?
	jne	FK_318A					; if yes, start a new count

	mov	bx,on_recovery_ticks			; if it did not time out, reeload counter again
	mov	tick_count,bx
	mov	previous_code,ax			; save as previous code
	mov	prior_code,ax				; save code previous to
	jmp	short filter_ret	

FK_318A:

	mov	bx,on_recovery_ticks			; if it did time out, load or reeload counter
	mov	tick_count,bx
	mov	repeating?,false			; fake out FilterKeys timer, it will think it's doing acceptance delay
	mov	frecovery_timing,true			; set to true because we are going to pass this key press on 

        cmp	fclick_on,true				; is click sound turned on
	jne	FK_319					; no, jump around
	call	click					; click if flag set 
FK_319:

	jmp	short FK_700

FK_320:
	mov	bx,on_delay_ticks			; get on_delay_ticks
	mov	tick_count,bx				; set count
	mov	repeating?,true				; repeating after this count

;----------------------------------------------------------------------------
; pass on the extended scan code in AX, but save as previous code if needed.

FK_700:

	mov	previous_code,ax			; save as previous code
	mov	prior_code,ax				; save code previous to previous code


; We do not expect any registers to be saved upon return from SticKeys

FK_800:
	call	SticKeys				; pass on actual code

filter_ret:

	pop	ax					; restore the world
	pop	bx
	pop	cx
	pop	dx
	pop	si
	pop	di
	pop	es
	pop	ds
	ret

FilterKeys	endp

_TEXT	ends

	end


