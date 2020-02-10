;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center (just kidding)		*
;									*
;	TIMEOUT.ASM							*
;									*
;************************************************************************

TITLE	Timeout

INCLUDE	keyboard.inc

	PUBLIC	TimeOut_Reset				; in TimeOut.asm
	PUBLIC	TimeOut_timer
	PUBLIC	TimeOut_dialog 
	PUBLIC	last_address
	
	EXTRN	FilterKeys_TurnOff:PROC			; in FilterKeys.asm

	EXTRN	SticKeys_TurnOff:PROC			; in StickeyKeys.asm

	EXTRN	MouseKeys_TurnOff:PROC			; in MOuseKeys.asm

	EXTRN	ToggleKeys_TurnOff:PROC			; in ToggleKeys.asm

	EXTRN	beep_turn_off:PROC			; in Handicap.asm
	EXTRN	fswitching_video:byte
	EXTRN	faccess_sound:byte
	EXTRN	fsecond_cue:byte


	EXTRN	fTime_Out:byte				; from Param.asm
	EXTRN	fTO_On_Off_Feedback:byte
	EXTRN	to_value:word
	EXTRN	fDialog_TimeOut_off:byte

	EXTRN	fhearing_on:byte
	EXTRN	fvideo_flash:byte

	EXTRN	fFilterKeysOn:byte
	EXTRN	fSticKeysOn:byte
	EXTRN	fMouseKeysOn:byte
	EXTRN	fToggleKeysOn:byte

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

to_cnt		DW	to_dflt_cnt		; hold count for time out

;----------------------------------------------------------------------------
; TimeOut_timer
;
; The TimeOut routine provides the user with a choice of time settings for which if no
; activity occurred in the specified period of time, the keyboard and mouse enhancement 
; features will time out and disable themselves.
;
; Expects:	Nothing
;
; Uses:		bx
;----------------------------------------------------------------------------

TimeOut_Reset	proc	

	assume	DS:_TEXT

	jmp	timeout_begin

;----------------------------------------------------------------------------
; TimeOut_dialog 
;
;	Checks a single flag from Dialog box to see if TimeOut was turned
; on/off.  

TimeOut_dialog	proc	

	assume	DS:_TEXT

	cmp	fTime_Out,true				; was TimeOut turned on
	jne	TOd_25
	mov	fTime_Out,true				; turn time out on
	jmp	short TOd_50

TOd_25:
	mov	fTime_Out,false				; turn time out off
TOd_50:
	mov	fDialog_TimeOut_off,false  		; reset flag to false
	ret

TimeOut_dialog	endp
;-----------------------------------------------------------------------------

TimeOut_timer	proc

	assume	DS:_TEXT

	push	bx

	cmp	fTime_Out,false			; is time out off?
	je	TimeOut_timer_end		; yes, quit -->
	xor	bx,bx				; set bx to 0
	cmp	to_cnt,bx			; is count zero?
	jg	TimeOut_10			; no, just decrement count
	jl	TimeOut_timer_end		; is it less than zero? yes -->
	cmp	fFilterKeysOn,false
	je	TimeOut_1
	inc	bl				; set flag
	call	FilterKeys_TurnOff		; turn off all the features
TimeOut_1:			       	
	cmp	fSticKeysOn,false
	je	TimeOut_2
	inc	bl				; set flag
	call	SticKeys_TurnOff
TimeOut_2:
	cmp	fMouseKeysOn,false
	je	TimeOut_3
	inc	bl				; set flag
	call	MouseKeys_TurnOff
TimeOut_3:
	cmp	fToggleKeysOn,false
	je	TimeOut_4
	inc	bl				; set flag
	call	ToggleKeys_TurnOff
TimeOut_4:
	cmp	fhearing_on,false
	je	TimeOut_5
	inc	bl				; set flag
	mov	fhearing_on,false
TimeOut_5:
	cmp	fvideo_flash,false
	je	TimeOut_6
	inc	bl				; set flag
	mov	fvideo_flash,false
TimeOut_6:
	or	bl,bl				; any turned off (i.e. bl <> 0)?
	jz	TimeOut_timer_end		; no -->
	cmp	fTO_On_Off_Feedback,false
	je	TimeOut_8
	call	beep_turn_off

TimeOut_8:
	cmp	fswitching_video,false
	jne	TimeOut_10
	mov	faccess_sound,true
	mov	fsecond_cue,25

TimeOut_10: 
	dec	to_cnt				; decrement count
TimeOut_timer_end: 

	pop	bx
	ret

TimeOut_timer	endp


;----------------------------------------------------------------------------
; TimeOut_Reset
;
; Expects:	Nothing
;
; Uses:		ax
;----------------------------------------------------------------------------

timeout_begin:

	push	ax
	mov	ax,to_value			; reset time out count
	mov	to_cnt,ax
	pop	ax
	ret

TimeOut_Reset	endp

last_address	dw	"$"	    		; last address we want to save when TSR'ing

_TEXT	ends

	end


