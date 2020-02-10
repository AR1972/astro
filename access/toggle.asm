;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center (just kidding)		*
;									*
;	TOGGLE.ASM							*
;									*
;************************************************************************

TITLE	ToggleKeys

INCLUDE	keyboard.inc

	PUBLIC	ToggleKeys				; in ToggleKeys.asm
	PUBLIC	ToggleKeys_timer
	PUBLIC	ToggleKeys_TurnOff
	PUBLIC	ToggleKeys_dialog

	EXTRN	pass_to_computer:PROC			; in Handicap.asm
	EXTRN	beep_high:PROC
	EXTRN	beep_low:PROC
	EXTRN	beep_turn_on:PROC
	EXTRN	beep_turn_off:PROC
	EXTRN	fpause_being_sent:byte
	EXTRN	fsecond_cue:byte
;
; Does not make any sense to give a visual que for ToggleKeys since ToggleKeys is
; intended to provide audible feedback for people with a visual disability.  They 
; would not be able to see the visual que anyway.....However, onthe PC/XT keyboard
; and on the SpaceSaver kybd, the LED's do not exist, and if the operator may want 
; a visual que from ToggleKeys and not the sound que, for any program, so we will
; turn it on here like any where else

	EXTRN	fswitching_video:byte
	EXTRN	faccess_sound:byte

	EXTRN	TimeOut_timer:PROC			; in TimeOut.asm
	EXTRN	TimeOut_Reset:PROC


	EXTRN	fToggleKeysOn:byte			; from Param.asm
	EXTRN	fTK_On_Off_Feedback:byte
	EXTRN	fDialog_Toggle_off:byte
;;	EXTRN	fspace_saver:byte

IFDEF	BUG

	EXTRN	portid:byte
	EXTRN	portout:byte
	EXTRN	HexCharsOut:PROC

ENDIF; BUG

;---------------------------------------------------------------------------

_TEXT	segment	word public 'CODE'

	assume CS:_TEXT
	assume DS:NOTHING
	assume ES:NOTHING
	assume SS:NOTHING

;----------------------------------------------------------------------------
;			R E S I D E N T   D A T A   A R E A
;
;  The below area defines the resident data area.

tk_hot_key_only?	DB	false		; only hot key is down
tk_toggle_count		DW	reset_cnt	; holds count for toggle time
tk_on_flag		DB	false		; internal flag used to avoid ToggleBeep on key repeats


;----------------------------------------------------------------------------

ToggleKeys	proc	

	assume	DS:_TEXT

	jmp	toggle_begin

;----------------------------------------------------------------------------
; ToggleKeys_timer 
;
; Increments the counts used in the turn on and off of ToggleKeys.  
;

ToggleKeys_timer	proc

	assume	DS:_TEXT

;----------------------------------------------------------------------------
; first check to see if we are manually being turned off or on.  To toggle 
; it on or off, we have it set so that the person must hold down the 
; Num Lock key for about five seconds.

	cmp	tk_hot_key_only?,true		; hot key only down?
	jne	tk_start_cnt_over		; no -->
	mov	ax,tk_toggle_count		; get current count
	cmp	ax,0				; are we at toggle ammount = 0?
	je	tk_toggle_on_off_flag		; yes -->
	jl	ToggleKeys_timer_end		; less than, so already done
	dec	tk_toggle_count			; decrement count
	jmp	ToggleKeys_timer_end

tk_toggle_on_off_flag:

	dec	tk_toggle_count			; set to -1 so must release first
	cmp	fToggleKeysOn,false		; are we off?
	je	tk_turn_toggle_on		; yes -->
	call	tk_turn_off_fb
	jmp	ToggleKeys_timer_end

tk_turn_toggle_on:

	call	tk_turn_on_fb
	jmp	ToggleKeys_timer_end

tk_start_cnt_over:

	mov	tk_toggle_count,tk_reset_cnt

ToggleKeys_timer_end:

	call	TimeOut_timer			; service TimeOut 

	ret
ToggleKeys_timer	endp

;-----------------------------------------------------------------------------
; the following routines provide the turn on/off and sound feedback.

tk_turn_on_fb	proc

	assume	DS:_TEXT

	cmp	fTK_On_Off_Feedback,false
	je	ToggleKeys_TurnOn
	call	beep_turn_on
ToggleKeys_TurnOn:
	cmp	fswitching_video,false
	jne	ToggleKeys_TurnOn_5
	mov	faccess_sound,true
	mov	fsecond_cue,24

ToggleKeys_TurnOn_5:

	mov	fToggleKeysOn,true		; turn on
	mov	tk_on_flag,true		       	; set to flag first toggle key make
	call	TimeOut_Reset
	ret
tk_turn_on_fb	endp


tk_turn_off_fb	proc
	cmp	fTK_On_Off_Feedback,false
	je	ToggleKeys_TurnOff
	call	beep_turn_off
ToggleKeys_TurnOff:

	cmp	fswitching_video,false
	jne	ToggleKeys_TurnOff_5
	mov	faccess_sound,true
	mov	fsecond_cue,25

ToggleKeys_TurnOff_5:

	mov	fToggleKeysOn,false		; turn off
	ret
tk_turn_off_fb	endp


;----------------------------------------------------------------------------
; Toggle_dialog 
;
;	Checks a single flag from Dialog box to see if ToggleKeys was turned
; on/off.  If it was, this routine makes a call to ToggleKeys_TurnOn/Off so the realstates
; can be updated to match the computer.  Upon exit the flag that was set by 
; Dialog box is cleared.(returned to false)


ToggleKeys_dialog	proc	

	assume	DS:_TEXT

	cmp	fToggleKeysOn,true			; was Stickeys turned on
	jne	Td_25
	call	ToggleKeys_TurnOn_5	  		; yes it was true, so turn on
	jmp	short Td_50

Td_25:
	call	ToggleKeys_TurnOff_5			; fToggleKEysOn was false, so turn off
Td_50:
	mov	fDialog_Toggle_off,false  		; reset flag to false
	ret

ToggleKeys_dialog	endp

;----------------------------------------------------------------------------
; ToggleBeep
;
;   This routine provides audible feedback to indicate if the lights for the
;   toggle keys got turned off or on.  If more than one light changes per
;   call, then we do not do any beeps since the program most likely changed
;   them and we would just add confusion to the situation.
;
; Expects:	ah = state of lights before changes
;
; Returns:	Nothing
;
; Uses:		ax,bx,es
;

ToggleBeep	proc	

	assume	DS:_TEXT

	push	ax
	push	bx
	push	es
	assume	ES:NOTHING
	mov	bx, RAMBIOS				; BIOS RAM segment at 40h
	mov	es,bx					;  .. point ES to that!
	assume	ES:RAMBIOS
	mov	tk_on_flag,true				; set to flag first toggle key make

	cmp	fToggleKeysOn,true
	jne	ToggleBeep_ret

	mov	al,es:[kb_flag]

	and	al,fCaps + fScroll + fNum

	and	ah,al					; turned on? assume only one changes
	jnz	tb_10					; no -->
	call	beep_high
	cmp	fswitching_video,false
	jne	ToggleBeep_ret
	mov	faccess_sound,true
	mov	fsecond_cue,7
	jmp	short ToggleBeep_ret
tb_10:
	call	beep_low
	cmp	fswitching_video,false
	jne	ToggleBeep_ret
	mov	faccess_sound,true
	mov	fsecond_cue,9
ToggleBeep_ret:

	pop	es
	assume ES:NOTHING
	pop	bx
	pop	ax

	ret
ToggleBeep	endp

;----------------------------------------------------------------------------
; ToggleKeys
;
; The ToggleKeys routine provide audible feedback or response for designated keys which can be toggled
; On or Off at the keyboard.  Typically, this includes the CAPS LCOK, NUM LOCK, and the SCROLL LOCK
; keys but could also include the cursor pad INSERT key.  When ToggleKeys is On, toggling one of the 
; above three keys on will produce a high beep while toggling that same key Off, produces a low beep.
;
;   It expects the extended scan code to be in AX.
;

toggle_begin:

;----------------------------------------------------------------------------
; first thing to do is to check for the Num Lock key to determine if the
; ToggleKeys features is being manually turned on or off.
;

	mov	bx,ax					; save copy in bx

	cmp	fpause_being_sent,true			; is PAUSE key being sent ??
	jne	togg_15					; if not a PAUSE, cont on as normal	
	cmp	ax,0C5h	 				; did we get the pause break finally ?
	jne	togg_10
	mov	fpause_being_sent,false			; if yes, reset the pause flag

togg_10:
	jmp     tk_send_key

togg_15:

	and	al,not break_bit			; get make scan code in al

	test	fToggleKeysOn,true			; is togglekeys on?
	jz	donot_toggle				; no
	cmp	al,NumLock				; is it the num lock key?
	je	toggle_check
	cmp	al,CapsLock				; is it a caps lock ?
	je	toggle_check
	cmp	al,ScrollLock				; is it a scroll lock ?
	jne	donot_toggle

toggle_check:

	test	bl,break_bit				; is it a break?
	jnz	donot_toggle				; do not do togglebeep on a break

num_check:
	cmp	al,NumLock				; is it the num lock key?
	jne	caps_check

;NOTE: We are messing up ah without saving!!!!

	mov	ah,fNum					; get numlock mask
	jmp	toggle
caps_check:

	cmp	al,CapsLock				; is it a caps lock ?
	jne	scrol_check
	mov	ah,fCaps
	jmp	toggle

scrol_check:

	cmp	al,ScrollLock				; is it a scroll lock ?
	jne	donot_toggle
	mov	ah,fScroll

toggle:
	test	tk_on_flag,true		
	jnz	on_off_toggle				; make key repeating

	call	ToggleBeep				; if was on, and wasn't a break

	jmp	on_off_toggle

donot_toggle:

	mov	tk_on_flag,false			; clear on the breaks

on_off_toggle:

	cmp	al,NumLock				; is it the num lock key?
;;	je	on_off_toggle_20			; was a num lock, so check if make/break for hot key
;;
;;	cmp	fspace_saver,true			; check to be sure we don't have a space saver keyboard ?
	jne	tk_clear_hot_key			; not a space saver keyboard either, so exit hot_key

;;	push	bx
;;	push	es
;;	push	ax
;;	assume	ES:NOTHING
;;	mov	bx, RAMBIOS				; BIOS RAM segment at 40h
;;	mov	es,bx					;  .. point ES to that!
;;	assume	ES:RAMBIOS
;;	mov	al,es:[kb_flag]
;;	test	al,03					; any shift keys down ?
;;	jz	on_off_toggle_15			; if not, don't bother checking for shift + scroll lock
;;	pop	ax
;;
;;IFDEF	BUG
;;	mov	portid,0b0h
;;	mov	portout,al
;;	call	HexCharsOut
;;ENDIF;	BUG
;;
;;	cmp	al,ScrollLock				; is this shift+scroll lock ?
;;	jne	on_off_toggle_16
;;	pop	es					; is shift+scroll lock w/space saver, so cont. at hot_key
;;	assume ES:NOTHING
;;	pop	bx
;;	call	beep_high				; DEBUGGING
;;	jmp	short on_off_toggle_20
;;
;;on_off_toggle_15:
;;	call	beep_low				; DEBUGGING
;;	pop	ax
;;
;;on_off_toggle_16:
;;
;;	pop	es
;;	assume ES:NOTHING
;;	pop	bx
;;	jmp	short tk_clear_hot_key			; not a shift+scrol lock by space saver
;;
;;on_off_toggle_20:

	test	bl,break_bit				; is it a break?
	jnz	tk_clear_hot_key			; yes, ignore -->
	mov	tk_hot_key_only?,true			; no, then set tk_hot_key_only to true
	jmp	tk_send_key				;
tk_clear_hot_key:

	mov	tk_hot_key_only?,false			;

tk_send_key:

	mov	ax,bx					; restore extended scan
;
; Doesn't assume any registers saved
;

	call	pass_to_computer			;

ToggleKeys_ret:

	ret
ToggleKeys	endp

_TEXT	ends

	end

