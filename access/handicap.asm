;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center (just kidding)		*
;									*
;	HANDICAP.ASM							*
;									*
;************************************************************************
;----------------------------------------------------------------------------
; HANDICAP.ASM
;
; FilterKeys  = Adjustment of typematic rate, key acceptance, debounce time and recovery times
; StickyKeys = One finger typing feature
; MouseKeys   = Complete mouse function from the numeric keypad
; ToggleKeys  = Audible feedback of indicator light states of keyboard
;
; The way this is done is to chain together the routines that implement 
; these features.  The routine below, keybd_int, actually reads the keyboard 
; port and then builds up the extended scan code which consists of the 
; "hidden" byte and then the scan byte.  It then passes it in AX to the first 
; routine in line (FilterKeys).  Each successive routine then continues to 
; pass it on or inhibits it.  Note, that if the scan code is an "e0", "e1", or 
; a response to a keyboard command, (>"f0"), then this routine passes the
; code to the original keyboard int. while also saving it to use as a scan code
; identification as the real scan code is later passed along between the routines.
; There may be a few exceptions to this, like if a control break is being trapped.
;
; The routine keybd_int does all the hardware management, unless one of the 
; routines (namely the BIOS in the computer ROM ) resets the hardware, in which case 
; keybd_int does not.  The variable fbios_called_direct is used to flag this condition.


print	MACRO	string
	mov	dx,OFFSET string
	mov	ah,9
	int	21h
	ENDM

INCLUDE	keyboard.inc

IFDEF	BUG

	PUBLIC	portid,portout
	EXTRN	HexCharsOut:PROC

ENDIF; BUG

	EXTRN	_end:ABS

	EXTRN	flatch:byte				; in Stickeys.asm
	EXTRN	shift_flg:byte

	EXTRN	FilterKeys:NEAR				; in FilterKeys.asm
	EXTRN	FilterKeys_timer:PROC
	EXTRN	FilterKeys_dialog:PROC
	EXTRN	fshift_click:byte

	EXTRN	MouseKeys_timer:PROC			; in  MouseKeys.asm
	EXTRN	InjectMouse:PROC			
	EXTRN	button_click:PROC			
;;	EXTRN	Put_Mouse_Data:PROC
	EXTRN	fMoving:byte
	EXTRN	Mouse_Status:word
	EXTRN	Delta_X:word
	EXTRN	Delta_Y:word
	EXTRN	Last_Direction:byte
	EXTRN	Status:byte
	EXTRN	mouse_data:word
	EXTRN	mouse_data_head:word
	EXTRN	mouse_data_tail:word
	EXTRN	mouse_data_end:word
	EXTRN	fbutton_up:byte
	EXTRN	fbutton_down:byte
	EXTRN	MouseKeys_dialog:PROC
	EXTRN	Button_Status:byte
	EXTRN	mouse_cnt:byte
	EXTRN	fnum_lock_was_off:byte
	EXTRN	fserial_stop:byte

	EXTRN	ToggleKeys_timer:PROC			; in ToggleKeys.asm
	EXTRN	ToggleKeys_dialog:PROC

	EXTRN	TimeOut_Reset:PROC			; in TimeOut.asm
	EXTRN	TimeOut_dialog:PROC
	EXTRN	last_address:word

	EXTRN	key_cnt:byte				; in StickeyKeys.asm
	EXTRN	StickeyKeys_dialog:PROC
	EXTRN	fkeys_injected:byte
	EXTRN	set_shift_states:PROC

;	EXTRN	_serialKeysInit:PROC			; in SerialKeys
	EXTRN	_kickStartSerialKeys:PROC
;	EXTRN	_initCommPort:PROC
;	EXTRN	_serialKeysStartupInit:PROC
	EXTRN	_forcedInt9Flag:byte
	EXTRN	_injectByte:byte
	EXTRN	_skWindowCompatible:byte
	EXTRN	_serialKeysEnableFar:FAR
	EXTRN	_serialKeysDisableFar:FAR

	EXTRN	fFilterKeysOn:byte			; from Param.asm
	EXTRN	fMouseKeysOn:byte	
	EXTRN	fcomp_dialog:byte
	EXTRN	fcomp_dialog_id:byte
	EXTRN	fDialog_Filter_off:byte
	EXTRN	fDialog_Stickeys_off:byte
	EXTRN	fDialog_Mouse_off:byte
	EXTRN	fDialog_Toggle_off:byte
	EXTRN	fDialog_TimeOut_off:byte
	EXTRN	fDialog_Action:byte
	EXTRN	fmkeys_override:byte
	EXTRN	fspace_saver:byte
	EXTRN	fhearing_on:byte
	EXTRN	fvideo_flash:byte
	EXTRN	fcomputer_not_found:byte
	EXTRN	fSticKeysOn:byte   
	EXTRN	fclick_on:byte
	EXTRN	_serialKeysOn:byte
	EXTRN	fslow_baud_mouse:byte

	EXTRN	_comp_id:byte
	EXTRN   _combase:word
	EXTRN	fmouse_driver:byte
	EXTRN	_finject_keys:byte
	EXTRN	_vector:byte
	EXTRN	comp_flag:byte
	EXTRN	ExtendedSeg:word
	EXTRN	_fmouse_id:byte
        EXTRN   comp_flag:byte
        EXTRN   btn_1:byte
        EXTRN   btn_2:byte
        EXTRN   Current_Button:byte
        EXTRN   fmouse_driver:byte
        EXTRN   fvideo_type:byte
	EXTRN	fserial_keys_loaded:byte


	PUBLIC	beep_low				; in Handicap.asm
	PUBLIC	pass_to_computer
	PUBLIC	beep_high
	PUBLIC	click
	PUBLIC	no_beep
	PUBLIC	beep_turn_on
	PUBLIC	beep_turn_off
	PUBLIC	InjectKeys
	PUBLIC	Get_Mouse_Data
	PUBLIC  Put_Key_Data
	PUBLIC	shift_tbl
	PUBLIC	shift_tbl_len
	PUBLIC	real_states
	PUBLIC	prev_real_states
	PUBLIC	current_shift
	PUBLIC	fpause_being_sent
	PUBLIC	fmouse_button
	PUBLIC	fwindows_st_re
	PUBLIC	fsilent_click
	PUBLIC	key_data_tail
	PUBLIC	key_data_head
	PUBLIC	key_data
	PUBLIC	key_data_end
	PUBLIC	faccess_sound
	PUBLIC	fswitching_video

	PUBLIC	fbios_called_timing
	PUBLIC	Enable
	PUBLIC	fsecond_cue
	PUBLIC	fkey_not_passed
	PUBLIC	ftimer_1C_active 
	PUBLIC	fserial_key_recieved
	PUBLIC	fmousetrapping
;	PUBLIC  fwindows_enh				; DEBUGGING

;----------------------------------------------------------------------------

_TEXT	segment	word public 'CODE'

	assume CS:_TEXT
	assume DS:_TEXT
	assume ES:_TEXT
	assume SS:NOTHING

;;PUBLIC	timer_Stack_top

PUBLIC	kybd_Stack_top


;		even
;timer_sp_save		dw 0
;timer_ss_save		dw 0
;		db 'tstacke'
;		even
;timer_Stack		dw 150 dup(0)
;timer_Stack_top	dw $-2
;		db 'tstacks'

		even
kybd_sp_save		dw 0
kybd_ss_save		dw 0
		db 'kstacke'
		even
kybd_Stack		dw 75 dup(0)
kybd_Stack_top	dw $-2
		db 'kstacks'


;;	ORG	100h
;;
;;begin:
;;
;;	jmp	_enable		  
;----------------------------------------------------------------------------
;			R E S I D E N T   D A T A   A R E A
;

;  The below area defines the resident data area.
;
;----------------------------------------------------------------------------

fserial_key_recieved	db	false		; flag used to tell my code modules that they are processing a serial key input
fkey_not_passed		db	false		; flag used to signal that a key didn't pass yet in int 9 w/stickeys on

ftimer_1C_active	db	false		; flag to signal me when I am in a timer 1C interrupt
fint9_active_temp	db	false		; flag used by int 9 to prevent double pushing of stack
;;fint1C_active_temp	db	false		; flag used by int 1C to prevent double pushing of stack

fdonnot_hook_address	db	false		; flag used by int 33 routine to hook or unhook address's
fslow_mouse		db	false		; flag used in slow serial mice to tell timer to write every other clock tic

fvideo_flash_cnt	db	0		; reset counter at start
fvideo_flash_on		db	false		; reset flag at start
video_cnst		db	4		; 4 for wait between toggling screen

shift_tbl	label	byte
	dw	RAlt,RCtrl,Alt,Ctrl,LShift,RShift
shift_tbl_len	equ	6

ctrl_break_buf	label	byte
	db	0e0h,046h,0e0h,0c6h
ctrl_break_buf_len	equ	4


ctrl_break_buf_cnt	DB	false			; counter into buffer when keys are being Injected

prev_real_states	DB	0			; previous state of real shift keys
real_states		DB	0			; current state of real shift keys

current_shift		DB	0			; holds flag of current shift if there is one

video_state		DB	0			; hold current video state for comparison checking
old_char_1		DB	0			; hold the char, which gets over written by hearing impaired symbol
old_char_2		DB	0			; hold the char, which gets over written by hearing impaired symbol
old_char_1_attr		db	0			; hold attribute of char 1
old_char_2_attr		db	0			; hold attribute of char 2
fsecond_cue		db	0			; holds second character if needed for visual cue

old_row			DB	0			; temp storage of cursor row position if hearing flag set
old_col			DB	0			; temp storage of cursor column position if hearing flag set
old_cursor_1		DB	0			; temp save of cursor type
old_cursor_2		DB	0			; temp save of cursor type
txt_or_grph		DB	0			; temp save of current video state, text or graphics mode

video_count		DB	false			; counter used if hearing flag set
faccess_sound		DB	false			; flag which will cause video output for AccesDos beeps 
							; since some of them are too short in length to be trapped by the timer int.
fswitching_video	DB	false			; flag to inform ADOS that video is switching when true	
fint9_by_timer		DB	false			; flag to our int15 routine that timer inititated an int9h
							; and that we should get scan code out of the key_data_buffer

floop_back     		db	false			; flag to let me know if I need to grab byte/replace byte
							; to prevent keyboard int from getting between timer injected codes

count_down_ticks	label	word			; counter used during tone setup
no_tone			DW	014h			; soft tone sound
low_tone		DW	06c0h			; 500 hz FilterKeys tones
high_tone		DW	0120h			; 2.0 khz
click_tone		DW	06c0h			;  hz ??????
turn_on_start_beep	DW	0360h			; 1.0 khz
turn_off_start_beep	DW	0120h			; 2.0 khz
on_off_change		DW	6			; for 100 steps

fbios_called_direct	DB	false			; flag for if BIOS routine called
fbios_called_timing	DB	false			; flag for when timer function calls BIOS routine

fpause_being_sent	DB	false			; flag to tell MouseKeys that a real PAUSE key is passing, so don't 
							; turn off on the "45h" part of it
fmouse_or_kybd		DB	false			; flag to tell us if the int. routine called the keybd or mouse int. vector
frehook_mouse		DB	false			; flag to during int33h
fmouse_button		DB	false			; flag to tell int 33 or timer when mousekeys has a mouse button down for polled mice
fFake_mouse		DB	false			; flag to tell sub int33h, when mask has been set for Int. driven mice
fsilent_click		DB	false			; flag to call click but silently
							; so we should convert to control break on computers which support it

fmousetrapping		DB	false			; if this flag is false, we allow MouseKeys to hook int 33h (added 4/92)
fctrl_break_on		DB	false			; same as above,used in Handicap.asm to eat keystrokes

fsysreq	   	  	DB	false			; flag to tell us the alternate key is down and a print screen key was pressed
							; so we should convert to system request on computers which support it
							; keyboard buffer
fscroll_changed		DB	false			; flag to tell us we detected shift+numlock on space saver keyboard
fvideo_flash_save	DB	false			; temp storage flag for fvideo_flash when Windows takes over
fhearing_on_save	DB	false			; "" "" ""

call_mask		DW	0			; mask sent by applications to mouse driver

;------------------------------------------------------------------------------------------------------------------------------
fwindows		DB	false			; flag to tell us if we started program inside of or under windows
							; anytime we exit Windows, this TSR program will have to terminate if started
							; after Windows was already running.
fwindows_st_re		DB	false			; flag to tell us which mode of Windows we are running in
fwindows_enh		DB	false			; flag to tell us which mode of Windows we are running in
;------------------------------------------------------------------------------------------------------------------------------

fwindows_after		DB	false 			; flag to tell us that Windows was loaded after ADOS was already started
fwindows_after_st	DB	false			; flag that says Windows started after ADOS in standard mode
fwindows_after_re	DB	false			; flag that says Windows started after ADOS in real mode
fwinfows_after_enh 	DB	false			; flag that says Windows started after ADOS in enhanced mode

fDOS_box		DB	false			; flag to Windows and serial keys that Windows is either DOS boxing
							; int2f w/ax=1680 =true first fime, or alt/ctrl+esc from DSO box back to Windows
							; and fDOS_box=false
;-------------------------------------------------------------------------------------------------------------------------------

falt_esc		DB	false			; flag to tell us that an alt+esc make was sent
falt_esc_send		DB	false			; flag that alt+esc break was sent and we can now safely inject Windows Alt+esc

fctrl_esc		DB	false			; flag to tell us that an ctrl+esc make was sent
fctrl_esc_send		DB	false			; flag that ctrl+esc break was sent and we can now safely inject Windows Ctrl+esc


mouse_x_temp		DB	false			; byte used to keep semi track of mouse x coordinate, to tell if it moved 
							; during int. 33 polling
mouse_y_temp		DB	false			; byte used to keep semi track of mouse y coordinate, to tell if it moved 
							; during int. 33 polling

hidden_code		DB	0			; holds either E0 or E1 or 00

key_data_tail		DW	?			; pointer into key_data
key_data_head		DW	?			; pointer into key_data
key_data		DW	20 DUP (?)		; array of (20 words) for circular buffer of keyboard scan code data
key_data_end	label	word

IFDEF	BUG
	portid		DB	0
	portout		DB	0

fserial_debugging	DB	0

	PUBLIC	old_row,old_col
	PUBLIC	fserial_debugging

; Message data area for the various routines used during initialization

	mesg41		DB	"Program started while running Windows in Enhanced Mode", 13, 10, "$"
	mesg42		DB	"Program started while running Windows in Real/Standard Mode", 13, 10, "$"

	mesg45		DB	"Your version of DOS (pre 3.0) does not support all Access DOS features", 13, 10, "$"
	mesg46		DB	"Your computer is running DOS version 3.0 or higher", 13, 10, "$"

ENDIF;	BUG

	mesg22		DB	"Computer was not identifiable and will be treated as a PC/XT/AT with an 84 key keyboard.", 13, 10, "$"
	mesg22A		DB	"Please restart AccesDOS menu, type access,  to change this selection if your ", 13, 10, "$"
	mesg22B		DB	"computer is NOT a PC/XT/AT with an 84 key keyboard.", 13, 10, "$"

	mesg43		DB	"Windows loading aborted, DO NOT run Enhanced Mode after AccessDOS is started", 13, 10, "$"
	mesg44		DB	"Unload Access DOS or run Windows in Standard Mode (win /s) ", 13, 10, "$"
	mesg60		DB	"Please wait, AccessDOS is loading...", 13, 10, "$"

; store these old vector variables here such that they always get addressed correcly


old_1C_int		label	word			; holds far call vector to previous timer int
old_1C_int_off		DW	0			; holds old timer routine offset address
old_1C_int_seg		DW	0			; holds old timer routine segment address

old_kybd_int		label	word			; holds far call vector to previous keyboard int
old_kybd_int_off	DW	0
old_kybd_int_seg	DW	0

old_2f_int		label	word			; holds far call vector to previous int 2f service routine
old_2f_int_off		DW	0		    	; we watch this int solely to know if windows is loaded after
old_2f_int_seg		DW	0		    	; ADOS has been started

old_33_int		label	word			; holds far call vector to previous int 33 service routine
old_33_int_off		DW	0
old_33_int_seg		DW	0

old_ps2mou_int		label	word			; holds far call vector to previous int
old_ps2mou_int_off	DW	0
old_ps2mou_int_seg	DW	0

;testpattern		db	'markstart'
sub_int33_ptr		label	word
sub_int33_ptr_off	DW	0			; holds far call vector to an address if an application trys
sub_int33_ptr_seg	DW	0			; holds far call vector to an address if an application trys

;sub_int33_ptr_2		label	word
;sub_int33_ptr_off_2	DW	0			; holds far call vector to an address if an application trys
;sub_int33_ptr_seg_2	DW	0			; holds far call vector to an address if an application trys
;
;sub_int33_ptr_3		label	word
;sub_int33_ptr_off_3	DW	0			; holds far call vector to an address if an application trys
;sub_int33_ptr_seg_3	DW	0			; holds far call vector to an address if an application trys

;---------------------------------------------------------------------------
; ship_it
;
; Routine to disable and enable the keyboard.  Works for IBM AT, NAT, 50,60 70,80
; according to Tech Ref. manual.  Send what is in al register to status port of 
; keyboard.  We only need to use this routine for int 9 computers of AT class
;
; expects	al register to contain code to send to the keyboard control chip


ship_it	proc

	push	cx
	push	ax
	xor	cx,cx				; zero cx register

ship_it_5:

	in	al,status_port			; read keyboard status port
	test	al,inpt_buf_full		; wait till we are allowed to write to port
	loopnz	ship_it_5
	pop	ax				; char to send is in al register
	out	status_port,al			; may have gotten an  enable or disable code for keyboard in "al" register
	pop	cx				; clean up stack
	ret

ship_it	endp

;----------------------------------------------------------------------------
; pass_to_computer 
;
; This routine passes the scan code to the original keyboard device driver when 
; called on a computer using keyboard interrupt 9h in byte form in al.
;
; In reality, the original keyboard int. reads the kb_data
; in port, and places the result in "al" register, which is the same scan code
; which this routine is passing along
;
; If this computer supports int 15h, then the only thing this routine does is set the 
; fbios_called_direct flag to true, telling the return code that this key should get
; passed to the computer



pass_to_computer	proc

	assume	DS:_TEXT

;;	push	ax
;;	push	ds

	cmp	_vector,15h				; are we running under int 15h
	je	pass_to_computer_100			; if yes, life as usual

; int 9h computers will get ot this code

	cmp	fFilterKeysOn,false			; is FilterKeys On ???
	je	pass_to_computer_25			; if FilterKeys is off, safe to pass key in this int 9 pass

	cmp	ftimer_1C_active,true			; is the timerr trying to pass a key
	jne	pass_to_computer_25			; if not, allow key to pass

; if stickeys is on, we need to flag that we didn't pass the key as yet

	cmp	fSticKeysOn,true
	jne	pass_to_computer_10
	mov	fkey_not_passed,true			; flag that timed key has not yet passed

pass_to_computer_10:

	jmp	pass_to_computer_end

;**************
;	cmp	_comp_id,1				; is this an IBM PC or original PC/XT ??
;	jne	pass_to_computer_25			; if not, don't need to bother with the following check
;
;	mov	ah,al					; save al, the scan code we are supposed to pass on
;	in	al,kb_data				; read in data from keyboard port
;	cmp	al,0					; was bit 0 a 0??
;	je	pass_to_computer_110			; yes, no data in buffer, skip call to orig. kybd int.
;
;	cmp	al,ah					; was the data at the keybd port, the same as what is supposed to be passed ?
;	jne	pass_to_computer_110			; if not, don't send it
;**************

pass_to_computer_25:
  	
	mov	fkey_not_passed,false			; reset this flag
	push	ds

	pushf
	call	dword ptr cs:old_kybd_int

	pop	ds
	assume	ds:_TEXT

pass_to_computer_100:
  	
	mov	fbios_called_direct,true


;************
;	jmp	short pass_to_computer_120
;
;pass_to_computer_110:
;
;	mov	fsilent_click,true			; set true if didn't send anything
;
;pass_to_computer_120:
;
;	pop	ds
;	pop	ax
;************

pass_to_computer_end:

	ret

pass_to_computer	endp
;----------------------------------------------------------------------------
; 
; InjectKeys
; 
; This procedure is used on computers which support the Injection of key scan codes 
; into the hardware buffer to inject either a control break scan code or an alternate
; print screen (SYSREQ) scan code if the user tries to do so with StickeyKeys.
;
; This routine is also used to inject StickeyKeys modifier break codes.
; This routine should never be called if the computer type is determined not capable of 
; handling scan codes injected into the hardware buffer.  Currently, the only computers
; which support hardware injections are PS/1 and all PS/2 except the 25/30-86 models.
;
; Expects 	al=code to inject


InjectKeys	proc

	assume	DS:_TEXT

;	push	cx				; DO NOT need to save any registers, called from inside timer int. only..
	mov	cx,995				; approx. 15msec/15.086*10-6/sec

IK_5:

	in	al,status_port			; read keyboard status port
	test	al,inpt_buf_full		; wait till we are allowed to write to port
	jz	IK_8				; yes buffers are empty, okay to do inject so far


	in	al,kb_ctl			; buffer not empty, check time
	and	al,10h				; mask to check refresh bit = 10h
	cmp	al,ah				; did it change? or first time thru
	jz	short IK_5    			; no wait for change, else cont.
	mov	ah,al
	loop	IK_5	      			; have we timed out ?
	jmp	short IK_20			; can't write this time thru, exit inject 

IK_8:
	mov	cx,995				; approx. 15msec/15.086*10-6/sec
	mov	al,0d2h				; code to tell keyboard output buffer that next byte
	out	status_port,al			; written is to I/O address hex 060 (kb_data) 	
IK_10:

	in	al,status_port			; read keyboard status port
	test	al,inpt_buf_full		; wait till we are allowed to write to port
	jz	IK_15				; yes buffers are empty, okay to do inject so far

	in	al,kb_ctl			; buffer not empty, check time
	and	al,10h				; mask to check refresh bit = 10h
	cmp	al,ah				; did it change? or first time thru
	jz	short IK_10    			; no wait for change, else cont.
	mov	ah,al
	loop	IK_10	      			; have we timed out ?
	jmp	short IK_20			; can't write this time thru, exit inject 

IK_15:
	call	Get_Key_Data		       	; ax has the data upon return
	out	kb_data,al			; send our char. which will generate an int. as if sent by kybd.

;IFDEF	BUG
;;	mov	fserial_debugging,true
;	mov	portid,06
;	mov	portout,al
;	call	HexCharsOut
;;	mov	fserial_debugging,false
;ENDIF;	BUG

	mov	floop_back,true			; flag that we sent a byte
IK_20:
;	pop	cx				; clean up stack
	ret

InjectKeys	endp

;----------------DEBUGGING------------------------------------------------
;
;Inject_Mouse_Low	proc
;
;	assume	DS:_TEXT
;	call	click	       
;	mov	cx,995				; approx. 15msec/15.086*10-6/sec
;
;IKM_5:
;
;	in	al,status_port			; read keyboard status port
;	test	al,inpt_buf_full		; wait till we are allowed to write to port
;	jz	IKM_8				; yes buffers are empty, okay to do inject so far
;
;
;	in	al,kb_ctl			; buffer not empty, check time
;	and	al,10h				; mask to check refresh bit = 10h
;	cmp	al,ah				; did it change? or first time thru
;	jz	short IKM_5    			; no wait for change, else cont.
;	mov	ah,al
;	loop	IKM_5	      			; have we timed out ?
;	jmp	short IKM_20			; can't write this time thru, exit inject 
;
;IKM_8:
;	mov	cx,995				; approx. 15msec/15.086*10-6/sec
;	mov	al,0d3h				; code to tell mouse output buffer that next byte
;	out	status_port,al			; written is to I/O address hex 060 (kb_data) 	
;IKM_10:
;
;	in	al,status_port			; read keyboard status port
;	test	al,inpt_buf_full		; wait till we are allowed to write to port
;	jz	IKM_15				; yes buffers are empty, okay to do inject so far
;
;	in	al,kb_ctl			; buffer not empty, check time
;	and	al,10h				; mask to check refresh bit = 10h
;	cmp	al,ah				; did it change? or first time thru
;	jz	short IKM_10    			; no wait for change, else cont.
;	mov	ah,al
;	loop	IKM_10	      			; have we timed out ?
;	jmp	short IKM_20			; can't write this time thru, exit inject 
;
;IKM_15:
;	call	Get_Mouse_Data		 	; ax has the data upon return
;	out	kb_data,al			; send our char. which will generate an int. as if sent by kybd.
;
;IKM_20:
;	ret
;
;Inject_Mouse_Low	endp
;----------------DEBUGGING------------------------------------------------

;----------------------------------------------------------------------------
; 
; kybd_echo
; 
; This routine is used on computers which DO NOT support hardware injection of scan codes but
; do support int 15h, to cause an int9h to occur, such that when int15 is called, we can trap for
; it and inject our scan code.  For IBM's, we will do this by using an undocumented call to 8042, which will
; cause it to do an interrupt with our data loaded into port 60h.  For non IBM's, we will write to status port
; which caused the 8042 to return a byte, which we then trapped.

kybd_echo	proc

	assume	DS:_TEXT

	cmp	_comp_id,5Ah			; is this an IBM product (Old AT, AT239, AT339)?
	je	kybd_echo_10			; if yes, do 2nd half of kybd_echo
						; if not, do the first half
 	cmp	_comp_id,2			; is this a New XT (1/10-5/9/86) running 
	je	kybd_echo_10			; if yes, do 2nd half of kybd_echo

;----------------------------------------------------------

kybd_echo_5:

	xor	cx,cx				; zero cx

kybd_echo_5A:

	in	al,status_port			; read keyboard status port
	test	al,both_buf_full		; check to be sure we can write to the port
	jz	kybd_echo_6
	loop	kybd_echo_5A
	jmp	short kybd_echo_8		; if cannot write this time, exit out


kybd_echo_6:

	mov	al,020h
	out	status_port,al			; a write to the keyboard status port (ctrl port) generates an interrupt

; if succesful in generating an interrupt, set a flag for myself to watch for when int 9 calls int 15h

	mov	fint9_by_timer,true 		; set flag so I know when int 9 occur, to disregard
				    		; the scan code int 9 has, and to read the key_data_buffer
				    		; for the scan code I want to insert
kybd_echo_8:

	jmp	kybd_echo_end			; exit

;----------------------------------------------------------

kybd_echo_10:

;	push	cx
	xor	cx,cx				; zero cx

kybd_echo_10A:

	in	al,status_port			; read keyboard status port
	test	al,inpt_buf_full		; check to be sure we can we can write buffer
	jz	kybd_echo_15
	loop	kybd_echo_10A
	jmp	kybd_echo_end			; if can't write this time, exit

kybd_echo_15:

	mov	al,7fh				; write command for 8042
	out	status_port,al			; write to port 64h
	jmp	$+2				; delay
	xor	cx,cx				; reset cx to 0

kybd_echo_20:

	in	al,status_port			; read keyboard status port
	test	al,inpt_buf_full		; check to be sure we can we can write buffer
	jz	kybd_echo_25
	loop	kybd_echo_20
	jmp	short kybd_echo_end		; if can't write this time, exit

kybd_echo_25:

	call	Get_Key_Data		       	; ax has the data upon return
	mov	floop_back,true			; flag to be sure we get our keystroke on the way back thru int 15h

	out	kb_data,al			; write "al"register to port 60h
	jmp	$+2				; delay
	xor	cx,cx				; reset cx to 0
	
kybd_echo_30:

	in	al,status_port			; read keyboard status port
	test	al,inpt_buf_full		; check to be sure we can we can write buffer
	jz	kybd_echo_35
	loop	kybd_echo_30
	jmp	short kybd_echo_end		; if can't write this time, exit

kybd_echo_35:

	mov	al,3fh				; read command to 8042
	out	status_port,al			; write to port 64h
	jmp	$+2				; delay
	xor	cx,cx				; reset cx to 0

kybd_echo_40:

	in	al,status_port			; read keyboard status port
	test	al,inpt_buf_full		; check to be sure input buffer empties
	jz	kybd_echo_45
	loop	kybd_echo_40
	jmp	short kybd_echo_end		; if can't write this time, exit

kybd_echo_45:

	xor	cx,cx				; reset cx to 0

kybd_echo_50:

	in	al,status_port			; read keyboard status port
	test	al,out_buf_full			; check to be sure output buffer filled
	jnz	kybd_echo_end
	loop	kybd_echo_50			; wait till not zero, (i.e. buffer is filled)

;	pop	cx				; clean up stack

kybd_echo_end:

	ret

kybd_echo	endp


;----------------------------------------------------------------------------
;	Put_Key_Data
; 
; This procedure loads keyboard key data into a buffer where the timer will later
; retrieve it and send it to the InjectKeys.  
;
; Key scan codes may be retrieved by computers which support int 15h also.  If they 
; do, the fbios_called_timer flag will be set.
;
;	Expects   Ax= keyboard data to be stored, order is
;			
;		  ah = 0
;		  al = hidden code and scan code if called twice, scan code if called once 

Put_Key_Data	proc

	assume	DS:_TEXT

	push	bx				; temp store of bx register
	push	si				; temp store of si register

	mov	bx,key_data_tail		; get tail pointer of key_data buffer
	mov	si,bx				; save pointer value
	add	bx,2				; move to next word address in buffer
	cmp	bx,OFFSET key_data_end		; are we at the end of the buffer ?
	jne	PKD_5				; no
	mov	bx,OFFSET key_data		; yes we are, so reset to the buffer beginning

PKD_5:
	cmp	bx,key_data_head		; has the buffer wrapped around ?
	jne	PKD_10

; if equal, buffer is full, beep high to inform user, and exit

	jmp	Put_Key_Data_End		; if full, error beep and leave routine

PKD_10:
	mov	[si],ax				; move whats in ax into address pointed to by si
	mov	key_data_tail,bx		; update tail pointer

Put_Key_Data_End:

	pop	si
	pop	bx
	ret

Put_Key_Data	endp

;----------------------------------------------------------------------------
;	Get_Key_Data
; 
; This procedure retrieves keyboard keystroke info. from a buffer for the timer 
; to send it to InjectKeys or for int15h to retrieve.
;
;	Returns   ax = keyboard keystroke data stored
;
;		  ah = 0
;		  al = hidden code and scan code if called twice, scan code if called once 


Get_Key_Data	proc

	assume	DS:_TEXT

	push	bx				; temp store of bx register

	mov	bx,key_data_head		; get head pointer of key_data buffer
	cmp	bx,key_data_tail		; test to see if head = tail, i.e. no data to send
	je	Get_Key_Data_End

	mov	ax,[bx]				; move whats in address pointed to by bx into ax
	add	bx,2				; move to next word in list
	cmp	bx,OFFSET key_data_end		; are we at the end of the buffer
	jne	GKD_5				; no, then we are done, except update the pointer
	mov	bx,OFFSET key_data		; if yes, then reset to the beginning of the buffer
GKD_5:
	mov	key_data_head,bx   		; move the head pointer to the start also

Get_Key_Data_End:

	pop	bx
	ret

Get_Key_Data	endp

;----------------------------------------------------------------------------
; beep
;
; Expects:	bx = length (number of cycles)
;		cx = tone (length of cycle)
;
; Changes:	bx

beep	proc

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	push	ax			; save ax
	IN	al,kb_ctl		; get keyboard control

	push	ax			; save
	push	cx			; save cx=wave length
b_1:	
	and	al,0fch			; set speaker bits
	OUT	kb_ctl,al		; send out to speaker
	pop	cx			; get cx=wave length
	push	cx			; save cx
b_2:	
	loop	b_2			; keep on for wave length of time
	cmp	fsilent_click,true	; should this be a silent click
	je	b_2A
	or	al,2			; set speaker bit
	jmp	short b_2B
b_2A:
	or	al,0			; NO SOUND 
b_2B:
	OUT	kb_ctl,al		; send out to speaker
	pop	cx			; get cx
	push	cx			; save cx
b_3:	
	loop	b_3			; keep off for wave length of time
	dec	bx			; decrement length of tone
	jnz	b_1			; loop if not zero

	pop	cx			; restore cx
	pop	ax			; get old keyboard control
	OUT	kb_ctl,al		; send out
	pop	ax			; restore old ax

	pop	ds

	ret
beep	endp

;----------------------------------------------------------------------------
; beep_high
; beep_low
; beep_turn_on
; beep_turn_off
; no_beep
; click
;
; The following routines provide the sound feedback.
;
; Expects:	Nothing
;
; Changes:	Nothing
;


beep_high	proc	

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	push	cx			; save registers
	push	bx
	mov	bx,high_tone_len	; give low tone
	mov	cx,high_tone
beep_high_20:
	call	beep
	pop	bx			; restore registers
	pop	cx

	pop	ds
;	assume	DS:NOTHING

	ret
beep_high	endp



beep_low	proc	

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	push	cx			; save registers
	push	bx
	mov	bx,low_tone_len		; give low tone
	mov	cx,low_tone
	call	beep
	pop	bx			; restore registers
	pop	cx

	pop	ds
;	assume	DS:NOTHING

	ret
beep_low	endp



no_beep		proc	

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	push	cx			; save registers
	push	bx
	mov	bx,no_tone_len		; give no tone
	mov	cx,no_tone
no_beep_20:
	call	beep
	pop	bx			; restore registers
	pop	cx

	pop	ds
;	assume	DS:NOTHING

	ret
no_beep		endp

;--------------------------------------------------------------	

beep_turn_on	proc	

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	push	cx			; save registers
	push	bx
	mov	cx,turn_on_start_beep	; low frequency
bon_20:	
	mov	bx,02h
	call	beep
	sub	cx,on_off_change	; rate of change
	cmp	cx,turn_off_start_beep	; high frequency
	jg	bon_20

	pop	bx			; restore registers
	pop	cx

	pop	ds
;	assume	DS:NOTHING

	ret
beep_turn_on	endp
;---------------------------------------------------------------
	
beep_turn_off	proc   

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	push	cx			; save registers
	push	bx
	mov	cx,turn_off_start_beep	; high frequency
bof_20:	
	mov	bx,02h
	call	beep
	add	cx,on_off_change	; rate of change
	cmp	cx,turn_on_start_beep	; low frequency
	jl	bof_20

	pop	bx			; restore registers
	pop	cx

	pop	ds
;	assume	DS:NOTHING

	ret
beep_turn_off	endp

;----------------------------------------------------------------------------

click 	proc

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	push	cx			; save registers
	push	bx
	mov	bx,click_tone_len      	; give click tone
	mov	cx,click_tone
	call	beep
	mov	fsilent_click,false	; reset to clear for PC or XT
	pop	bx			; restore registers
	pop	cx

	pop	ds
;	assume	DS:NOTHING

	ret
click	endp

;----------------------------------------------------------------------------
;	Get_Mouse_Data
; 
; This procedure retrieves mouse button info. from a buffer for the timer 
; to send it to the appropriate mouse input mechanism.
;
;	Returns   Ax= mouse data stored
;
;		  1st Mouse_Status
;		  2nd Delta_X
;		  3rd Delta_Y

Get_Mouse_Data	proc

	assume	DS:_TEXT

	push	bx				; temp store of bx register

	mov	bx,mouse_data_head		; get head pointer of mouse_data buffer
	cmp	bx,mouse_data_tail		; test to see if head = tail, i.e. no data to send
	je	Get_Mouse_Data_End

	mov	ax,[bx]				; move whats in address pointed to by bx into ax
	dec	mouse_cnt			; derease counter since we took a word out of the buffer

	add	bx,2				; move to next word in list
	cmp	bx,OFFSET mouse_data_end	; are we at the end of the buffer
	jne	GMD_5				; no, then we are done, except update the pointer
	mov	bx,OFFSET mouse_data		; if yes, then reset to the beginning of the buffer
GMD_5:
	mov	mouse_data_head,bx   		; move the head pointer to the start also

Get_Mouse_Data_End:

	pop	bx
	ret

Get_Mouse_Data	endp

;---------------------------------------------------------------------------
; _int2f
;
; We hook int2fh, to monitor if Windows is ever loaded after Access Dos is already 
; loaded.  To test, we will abort Enhanced loading and put up a message.
; AccessDOS version 1.1 allows enhanced mode windows.


_int2f	proc	far

	cmp	ax,01605h				; watch for Windows to load after us
	jne	int2f_50				; this is what Windows broadcasts when it loads

	push	dx
	and	dx,001h					; mask off bit 0
	cmp	dx,0					; is this Windows 3 in enhanced mode ?
	pop	dx   
	jne	int2f_45				; if not, exit to next int 2fh handler

; If we find enhanced mode, flag it accordingly instead of aborting as AccessDOS v1.00 did
;
;	mov	cx,1					; return cx=1, will cause windows to abort
;	pop	dx					; 
;	jmp	int2f_100

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	mov	fwindows_after,true			; general flag for Windows standard mode loading after ADOS
	mov	fwindows_enh,true			; flag that we are in Windows enhanced mode

	push	ax
	mov	al,fhearing_on				; save state of hearing impaired flag(s)
	mov	fhearing_on_save,al
	mov	fhearing_on,false
	mov	al,fvideo_flash				; ""
	mov	fvideo_flash_save,al
	mov	fvideo_flash,false
	pop	ax
	jmp	int2f_90


int2f_45:

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	mov	fwindows_after,true			; general flag for Windows standard mode loading after ADOS
	mov	fwindows_after_st,true			; flag that Windows stand. mode is starting after ADOS

	push	ax
	mov	al,fhearing_on				; save state of hearing impaired flag(s)
	mov	fhearing_on_save,al
	mov	fhearing_on,false
	mov	al,fvideo_flash				; ""
	mov	fvideo_flash_save,al
	mov	fvideo_flash,false
	pop	ax

;	call	serialKeysDisableFar

	jmp	int2f_90

int2f_50:

	cmp	ax,01606h				; watch for Windows to unload after us
	jne	int2f_75				; this is what Windows broadcasts when it unloads
 
; right now don't need to check if unloading from enhanced or stand./real  -- change for 1.1

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

;;	cmp	fwindows_after_st,true			; this would have been set true by a successful Windows loading
;;	je	int2f_70				; if not, operator tried enhanced windows, so give error messasge
;;
;;	print	mesg43					; print message to abort
;;	print	mesg44					; instruct user to try Windows in Real/Standard Mode

int2f_70:
; reset flags upon exit

	mov	fwindows_after,false			; general flag for Windows real mode running after ADOS
	mov	fwindows_after_st,false			; flag that Windows stand. mode is running after ADOS
	mov	fwindows_enh,false
	mov	video_count,07fh			; reset such that it will take 5 seconds to start video 

	push	ax
	mov	al,fhearing_on_save			; restore upon exit
	mov	fhearing_on,al
	mov	al,fvideo_flash_save
	mov	fvideo_flash,al
	pop	ax

;	cmp	fserial_keys_loaded,true		; is serial keys loaded ?
;	jne	int2f_72
;	cmp	_serialKeysOn,true			; is SerialKeys code on ?
;	jne	int2f_72
;
;	call	_serialKeysEnableFar
;	call	_initCommPort				; reset 
;	call	_serialKeysStartupInit			; clear serialkeys buffers

int2f_72:

	jmp	short int2f_90


int2f_75:

	cmp	ax,01684h				; watch for Windows to load after us
	jne	int2f_80				; this is what Windows broadcasts when it loads Real Mode
							; NOTE, Windows doesn't broadcast REAL mode exit of 01606h !@!

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	mov	fwindows_after,true			; general flag for Windows loading after ADOS
	mov	fwindows_after_re,true			; flag that Windows real mode is starting after ADOS

	push	ax
	mov	al,fhearing_on				; save state of hearing impaired flag(s)
	mov	fhearing_on_save,al
	mov	fhearing_on,false
	mov	al,fvideo_flash				; ""
	mov	fvideo_flash_save,al
	mov	fvideo_flash,false
	pop	ax
							; and no way to tell when we exit, use timer to find out if we exitted
;	call	_serialKeysDisableFar

	mov	video_count,07fh			; reset such that it will take 5 seconds to start video 
	jmp	short int2f_90

								   
int2f_80:

	cmp	ax,04680h				; watch for Windows 3.0 to DOS box
	je	int2f_85

	cmp	ax,04810h				; watch for Windows 3.1 to DOS box
	jne	int2f_100

int2f_85:

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	cmp	fwindows_after_st,true			; are we DOS boxing after Windows standard was started ?
	jne	int2f_86 
	jmp	short int2f_88

int2f_86:
	cmp	fwindows_enh,true			; are we DOS boxing after Windows enhanced mode was started ?
	jne	int2f_87 
;	jmp	short int2f_88
	jmp	short int2f_90				; if running in enhanced mode, allow Access Utility SerialKeys
							; to run as is, do not attempt tp take back the port in a DOS box
int2f_87:
	cmp	fwindows_after_re,true			; are we DOS boxing after Windows real was started ?
	jne	int2f_90				; if not, exit as some one is asking if Windows is loaded
int2f_88:

;	call	click					; DEBUGGING
	cmp	fserial_keys_loaded,true		; is serial keys loaded ?
	jne	int2f_90

	cmp	_skWindowCompatible,true
	je	int2f_89

	cmp	_serialKeysOn,true			; is SerialKeys code on ?
	jne	int2f_90

int2f_89:

	cmp	_serialKeysOn,true
	je	int2f_90

	call	_serialKeysEnableFar
;	call	_initCommPort				; reset 
;;	call	_serialKeysStartupInit			; clear serialkeys buffers

int2f_90:

	pop	ds
	assume	DS:NOTHING

int2f_100:

	jmp	dword ptr cs:old_2f_int	    		; jump to original interrupt 2fh handler

_int2f	endp

;---------------------------------------------------------------------------
; _int71/74
; 
; All we are doing is hooking the input of the mouse to be able to reset the 
; StickeyKeys count (key_cnt) if we get real mouse activity.  Turns out for the the PS/2 Model
; 25..30/86, the keyboard int9h, is shared by the keyboard and the pointing device.  Therefore,
; we don't necessarily know if the interrupt was generated by the mouse or the keyboard, unless
; we do some further investigation.  After looking at the new PS/2 BIOS, (or what DOS adds to it)the 
; original keybd. int. has a pre-amble attached which decifers whether or not the interrupt was a 
; keystroke or the pointing device.  After review, I decided to set my own flag, such that if the 
; keyboard int 9h routine is called by the original int ? device(mouse or keyboard), then I will 
; not reset the key_cnt variable of StickeyKeys, since it was not mouse activity.
;

_intps2mou	proc
		  
	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	mov	fmouse_or_kybd,false 			; set flag to false at entry

	pushf
	call	dword ptr cs:old_ps2mou_int	    	; call original interrupt 71 or 74h

	cmp	fmouse_or_kybd,false			; if keybd int 9h. was called, this would have been set true
	jne	_intps2mou_25				; if false, then it was mouse activity, and we need to reset key_cnt
	mov	key_cnt,false

_intps2mou_25:

	pop	ds
	assume	DS:NOTHING

	iret

_intps2mou	endp


;---------------------------------------------------------------------------
; sub_int33 
;
;	If this suroutine is called, it's because an application passed the mouse driver
; a new address to send data when ever the mouse int. occurs.  If this is called, we are really only 
; interested in maintaining the button status connection between MouseKeys and the real mouse.
;
; This routine is called with ax=mask=bit1=left but. pressed, bit2=left button released
; bit3=right button pressed, bit4=right button released

sub_int33	proc	far

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

; if this routine gets called, then there was mouse activity, a mouse int. was generated, so reset StickeyKey count

	mov	key_cnt,0				; reset key_cnt to 0
	cmp	fMouseKeysOn,true			; check to see if MouseKeys is even On ?
	je	sub_int33_2
	jmp  	sub_int33_65
  
sub_int33_2:

	cmp	fmouse_button,true			; using a MouseKeys button ?
	je	sub_int33_4				; if yes, then the int. was initiated by kybd, follow thru

; If not a MouseKeys int. (i.e. from the kybd), then the real mouse was moved or a real mouse button was pushed while
; MouseKeys is on, so if we want to allow the MouseKeys mouse button to override the real mouse button, then we better
; check that they agree, and if not, alter the real mouse button info accordingly. 


	cmp	fbutton_down,true			; is the MouseKeys button down ???
	jne	sub_int33_3A				; if no, MouseKeys button is down, then don't need to compare status'

;********************************************************************************************************************************
; REAL MOUSE HANDLING
;
; Real mouse input while the MouseKeys mouse button is being held down, so we must act on it
; If we see that the REAL mouse depresses a button, we will assume it is a mouse capable users, 
; and reset allow the real mouse to release the depressed MouseKeys button.  However, do not
; let only button up information reset the MouseKeys mouse button so check to see if the real 
; mouse is sending any mouse button up info ??(ax=0001 0100 ?)
;
; ax = mask passed back to application from mouse driver to inform application why mouse driver is sending data
;
; mouse moved           ax = 0000 0001 (01h)
; mouse r/l button down ax = 0000 1010 (0Ah)
; mouse r/l button up	ax = 0001 0100 (14h)
;
;********************************************************************************************************************************

	push	bx
	push	ax

; check if button down info

	mov	bx,ax					; get button info into ax
	and	ax,0ah					; mask ax for either right/left/both buttons down
	jz	sub_int33_2A				; if zero, then there was not any button pressed down
	pop	ax

	mov	fbutton_down,false			; reset MouseKeys button down flag
	mov	Button_Status,0				; reset since we let MouseKeys mouse button up

	cmp	_fmouse_id,4				; check if a PS/2 mouse ?
	jne	sub_int33_2B
	mov	Mouse_Status,08h			; reset Mouse_Status to button up also for PS2 mouse
	jmp	short sub_int33_3			; exit

sub_int33_2B:
	mov	Mouse_Status,040h			; reset Mouse_Status to button up also for serial mouse
	jmp	short sub_int33_3			; exit

sub_int33_2A:

	mov	ax,bx					; get button info into ax
	and	ax,014h					; mask ax for either right/left/both buttons up
	pop	ax					; pop has no affect on the flags status
	jz	sub_int33_3				; if zero, there was no button release info, so pass along as is

; If we get here, then we have Real mouse button up info, and we want to suppress it
; Some applications need to see mouse button dowm to remain in sync, so we will try to fool them also


	xor	ax,ax					; zero ax mask, causing real mouse info to not pass on the fact that real mouse 
							; button is actually up, since we want MouseKeys mouse button to override
	mov	ax,01					; first bit is set in real codes
;	shr	bx,1					; move whatever up mask bit right 1 bit to a down mask bit
;	or	ax,bx					; logical -or- them to ax, so it looks like mouse button is down


sub_int33_3:
	pop	bx

sub_int33_3A:

	jmp	sub_int33_50				; we can exit

;************************************************************************************************************************************

sub_int33_4:

; if we get a MouseKeys button_up key, we want to EAT the down portion of the button_click
; and not pass that along to the application.  Remember, we added the button_click to a standard
; button_up to fool the mouse driver for those occassions when the user operates both MouseKeys
; and the real mouse

	cmp	fbutton_up,true				; did we have a button up ?
	jne	sub_int33_50
	mov	bx,0					; clear button down mask before the application gets it
	xor	ax,ax					; clear ax
	mov	ax,14h					; mask to release both buttons
	mov	fbutton_up,false			; reset flag to false since we ate the button down code as designed

sub_int33_50:

	push	ax
	mov	ax,mouse_data_head			; get head pointer of mouse_data buffer
	cmp	ax,mouse_data_tail			; test to see if head = tail, i.e. no data to send
	jne	sub_int33_60			   	; if not equal, data in mouse_data buffer, so don't reset this flag yet
	mov	fmouse_button,false			; after we have sent the ALL THE mouse info, reset this flag

sub_int33_60:
	pop	ax

sub_int33_65:

	call	dword ptr cs:sub_int33_ptr

	pop	ds
	assume	ds:NOTHING
	ret

sub_int33	endp

;----------------------------------------------------------------------------
; int33h hook
; 
; Hooked int33h in an attempt to watch mouse activity and to keep mousekeys in 
; sync with real mouse activity, i.e. if mousekeys locks a button down, movement 
; real mouse will alter this, so if we monitor for this condition, we can correct
; or keep the two mice in sync
;----------------------------------------------------------------------------

PUBLIC	_int33

_int33	proc

	push	ds					; save registers used

	push	cs
	pop	ds
	assume DS:_TEXT

	cmp	fmousetrapping,false			; is mouse address hooking allowed
	je	i33_2
	jmp	i33_A_50  				; if not,call original int 33h

i33_2:
;-------------
; AX=0 or 33 decimal (21h)
;-------------
	cmp	ax,0					; did we get a call to mouse reset and status
	je	i33_3
	cmp	ax,21h					; did we do a software reset ?, if so, only clear mask
	je	i33_4
	jmp	short i33_5

i33_3:
	mov	sub_int33_ptr_off,0
	mov	sub_int33_ptr_seg,0
i33_4:
	mov	call_mask,0				; reset call mask with 0 or 21h calls
	jmp	i33_A_50

;-------------
; AX= 12 decimal (0Ch) or 20 decimal (14h)
;-------------
;
; If we get here, someone is using int 33 vector with function 12 decimal, which will be
; passed an address for the mouse driver to send mouse data when a mouse int. occurs, based 
; upon a mouse mask which gets set.  We have to trap for this address, if we want to monitor
; what the real mouse might be doing, to keep it in sync with MouseKeys.  This situation occurs
; for example, if MouseKeys has a mouse button down, and then I move the real mouse, normally the 
; real mouse movement would cancel the mouse button down of MouseKeys, and we didn't want it to.
; Therefore, we need to be able to trap such conditions, and alter them to the desired condition, 
; before the application actually gets the mouse info.

i33_5:

	cmp	ax,0Ch					; is int 33h being called with ax=12, i.e. reseting an address
	je	i33_5_A
	cmp	ax,014h
	je	i33_5_A
	jmp	short i33_8

i33_5_A:
	mov	call_mask,cx
	
i33_5_D:

	pushf
	call	dword ptr cs:old_33_int			; call original interrupt 33h

	push	es
	push	ax
	push	bx
	push	cx
	push	dx

	mov	ax,cs
	mov	bx,ax
	mov	es,ax
assume es:_TEXT
	mov	dx,OFFSET sub_int33			; our routine that gets executed if an application repoints the 
							; now int 33h, called with ax=12, will load our sub_int33h address 
							; as es:dx instead of the application's es:dx
	mov	cx,call_mask
	mov	ax,0014h				; call to swap int. routines

	pushf
	call	dword ptr cs:old_33_int			; call original interrupt 33h

i33_5_G:
	mov	word ptr [sub_int33_ptr_off],dx 	; save address our call returned as new sub_int33_ptr
	mov	word ptr [sub_int33_ptr_seg],es	 	

	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	es
assume es:nothing

	jmp	i33_B_50


;-------------
; AX=22 decimal (16h)
;-------------
; If we get here, application is exitting for some reason, and wants to save the state of the mouse.
; We need to put back the address the application may have originally passed to the mouse driver, to 
; tell the mouse driver where to send data, so when the application resatarts, we will have an address
; we can use to rehook ourselves.

i33_8:

	cmp	ax,016h
	je	i33_8_A
	jmp	short i33_10

i33_8_A:

	push	es
	push	ax
	push	bx
	push	cx
	push	dx

	mov	dx,word ptr [sub_int33_ptr_off]
	mov	es,word ptr [sub_int33_ptr_seg]
	mov	bx,es
	mov	cx,call_mask
	mov	ax,0014h				; call to swap int. routines

	pushf
	call	dword ptr cs:old_33_int			; call original interrupt 33h

	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	es

	jmp	i33_3

;------------------------------------
; AX = 23 decimal (17h) 
;------------------------------------

i33_10:
	cmp	ax,017h
	je	i33_10_A
	jmp	short i33_A_50

i33_10_A:

	pushf
	call	dword ptr cs:old_33_int			; call original interrupt 33h

	push	es
	push	ax
	push	bx
	push	cx
	push	dx

	mov	ax,cs
	mov	bx,ax
	mov	es,ax
assume es:_TEXT
	mov	dx,OFFSET sub_int33			; our routine that gets executed if an application repoints the 
							; now int 33h, called with ax=12, will load our sub_int33h address 
							; as es:dx instead of the application's es:dx
	mov	cx,01fh
	mov	ax,0014h				; call to swap int. routines

	pushf
	call	dword ptr cs:old_33_int			; call original interrupt 33h

; check call_mask returned, to see if this is an active mouse application

	cmp	cx,0   
	jne	i33_10_G

; we did get back a zero for call_mask
; so call back with ax = 0014h, es:dx are the address we received
; leave CX alone, as we want to pass back the original call_mask

	mov	ax,0014h				; call to swap int. routines
	pushf
	call	dword ptr cs:old_33_int			; call original interrupt 33h	

	mov	ax,0
	mov	cx,ax					; to rezero call mask coming up if was zero
;	mov	es,ax
;	mov	dx,ax					; clear our sub_int33_ptr upon return

i33_10_G:

	mov	call_mask,cx				; get new call mask
	mov	word ptr [sub_int33_ptr_off],dx 	; save address our call returned as new sub_int33_ptr
	mov	word ptr [sub_int33_ptr_seg],es	 	

	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop	es
assume es:nothing

	jmp	short i33_B_50

;-----------------------------------------------

i33_A_50:

	pushf
	call	dword ptr cs:old_33_int			; call original interrupt 33h

;-----------------------------------------------

i33_B_50:

; at this point, we don't know if this is a MouseKeys or real mouse entry, using int. 33 or polled, so we need to 
; check if button status or x,y coordinate info was requested, so we can decide if we should reset key_cnt of 
; StickeyKeys.

	cmp	ax,03					; request for button status and mouse postion ?
	je	i33_26A
	jmp	i33_50

; is requesting mouse button or movement info, unfortunately, this may be a polled mouse, which would do this
; request several times each second, even if no mouse activity occurred, or this may be from an int. mouse
; which didn' use int.33 to repoint the mouse vector like seen in sub_int33 routine, so we have to check if any mouse
; data is being returned, before we can judge to reset the key_cnt variable of StickeyKeys
; Not setting mouse_x/y_temp first time thru, so may miss a StickeyKeys count, but unlikely, since polling is so fast

i33_26A:
	cmp	bx,0					; if bx=0, the mouse button(s) is/are up
	jne	i33_26B					; if not zero, then we had mouse button activity, so reset key_cnt

	cmp	mouse_x_temp,cl				; did mouse x data change from last poll ??
	jne	i33_26B					

	cmp	mouse_y_temp,dl				; did mouse y data change from last poll ??
	je	i33_27					; no change, so jump to cont

i33_26B:
	mov	mouse_x_temp,cl				; update mouse_x_temp position	
	mov	mouse_y_temp,dl				; update mouse_y_temp position
	mov	key_cnt,0				; reset key_cnt to 0


i33_27:
; now we can continue as normal, by first checking to see if MouseKeys is even on ???

	cmp	fMouseKeysOn,true
	je	int33_27
	jmp	i33_50

int33_27:

;; Okay, MouseKeys is On and Int33h, requested button info.  Is there a MouseKeys keys down???
;

	cmp	fbutton_up,true				; did we have a button up ?
	jne	int33_27B
	mov	bx,0					; clear the extra button down mask that MouseKeys sends, before the 
							; application gets it

	mov	fbutton_up,false			; reset flag to false since we ate the button down code as designed
	jmp	i33_50					; then jump around all this checking

int33_27B:

; if no MouseKeys keys are currently down, look out for real mouse info, if get any real mouse info, and we want to 
; keep the MouseKeys mouse button down if it was down, then we must alter the real mouse button info if it tries to
; let the mouse buttons up

	cmp	fbutton_down,true			; is the MouseKeys button down ???
	je	int33_28				; if yes, then a MouseKeys button is down, and we need to check on REAL mouse status
	jmp	i33_50					; if not, jump around all this checking

int33_28:

;********************************************************************************************************************************
; REAL MOUSE HANDLING
;
; Real mouse input while the MouseKeys mouse button is being held down, so we must act on it
; If the REAL mouse button is pushed, allow it to release the MouseKeys mouse button.
; Problem with a "polled" mouse routine, is that it constantly asks for mouse status updates, and MouseKeys
; buttons are one time things.  If this routine gets asked for mouse status info, we have no way of
; knowing if it is MouseKeys mouse button down, or REAL mouse buttons down, since the asking is continous
; which is alot different than asking only when an action occurs (interrupt driven) as in the 
; mouse routines above which handle the interrupt driven mouse.  We can figure one thing out however,
; and that is that we should never see mouse button up information, as long as the fbutton_down flag
; is set within MouseKeys, unless the REAL mouse sent it.  

; Next check to see if the real mouse is sending any mouse button up info ??(bx=0000 0000 ?)
; We could get REAL mouse button up info. from moving the REAL mouse or from clicking the REAL mouse button ?
; Problem is, we want to override a button up if it was from moving, and allow a button up if it was a click

	cmp	bx,0					; if zero, real mouse is sending all buttons are up info.
	jz	int33_29				; if zero, there is button up info, so we must alter
	jmp	i33_50					; if not, jump around all this checking

int33_29:
	
; If we get here, then we have Real mouse button up info, and we want to suppress it, so we have to check and see what
; MouseKeys mouse button is down, and echo this condition

	cmp	_fmouse_id,4				; check if a PS/2 mouse ?
	jne	int33_37

;-------------------------------------------------------------------------------------------------
; PS/2 Mouse

	push	ax					; temp store
	mov	ax,Mouse_Status
	and	al,03h					; mask off lowest bits
	cmp	al,01					; is the left MouseKeys button down ?
	jne	int33_30
	xor	bx,bx					; clear what ever was in bx
	or	bx,01h					; or the left button pressed in bx mask
	pop	ax
	jmp	i33_50	  				; exit to normal address called

int33_30:
	cmp	al,02h
	jne	int33_32
	xor	bx,bx					; clear what ever was in bx
	or	bx,02h					; or the right button pressed
	pop	ax
	jmp	i33_50

int33_32:

	cmp	al,03h
	jne	int33_34				; if we do this jump, no buttons were down
	xor	bx,bx					; clear what ever was in bx
	or	bx,03h					; or both the left and right buttons pressed
	pop	ax
	jmp	i33_50

int33_34:
	pop	ax
	jmp	i33_50					; if we already called int33h, jump around doing it again
;----------------------------------------------------------------------------------------------------------------------------
; serial mouse

int33_37:

	cmp	_fmouse_id,2				; check if a serial mouse ?
	jne	i33_50

	push	ax					; temp store
	mov	ax,Mouse_Status
	and	al,030h					; mask off high byte low bits
	cmp	al,00h					; any MouseKeys buttons down
	je	int33_44				; no
	cmp	al,020h
	jne	int33_40
	xor	bx,bx					; clear what ever was in bx
	or	bx,01h					; or the left button pressed in bx mask
	pop	ax
	jmp	i33_50					; exit to normal address called

int33_40:
	cmp	al,010h
	jne	int33_42
	xor	bx,bx					; clear what ever was in bx
	or	bx,02h					; or the right button pressed
	pop	ax
	jmp	i33_50

int33_42:

	cmp	al,030h
	jne	int33_44				; if we do this jump, we had an error ?
	xor	bx,bx					; clear what ever was in bx
	or	bx,03h					; or both the left and right buttons pressed
	pop	ax
	jmp	i33_50

int33_44:
	pop	ax

i33_50:

	pop	ds
	assume	DS:NOTHING

	iret

_int33	endp
									    
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
; new_timer_int 
;
; Calls various procedures to handle time dependent function of the different
; features.  Also calls the original timer 1Ch interrupt

_new_timer_int	proc

	push	ds					; save registers used
	push	es
	push	si
	push	di
	push	dx
	push	cx
	push	bx
	push	ax

	push	cs
	pop	ds
	assume DS:_TEXT
	push	cs
	pop	es
	assume ES:_TEXT

;	cmp	ftimer_1C_active,true 			; check to see if we got re-interrupted ?
;	jne	NT_5
;	call	click
;	jmp	NT_END_5
;NT_5:

	mov	ftimer_1C_active,true

; TimeOut_timer is currently called from the Togglekeys_timer routine and seems to work there ????

	call	FilterKeys_timer			; service FilterKeys
	call	ToggleKeys_timer			; service ToggleKeys

	cmp	fMouseKeysOn,true			; is MouseKeys On, if not don't call MouseKey_timer
	jne	NT_8					; if MouseKeys is Off, make sure buffer got emptied on turn off

	call	MouseKeys_timer				; yes, so call the timer

NT_8:

	mov	bx,mouse_data_head			; get head pointer of mouse_data buffer
	cmp	bx,mouse_data_tail			; test to see if head = tail, i.e. no data to send
	jne	NT_10					; if not equal, we have mouse data in the buffer
	jmp	NT_105 					; nothing to send from mouse buffer so jump to here

;---------------------------------------------------------------------------------------------
; if not equal, we have mouse_data to send, after we call, ax will have the data to send
; at this point, we should also disable the real mouse from int. and messing up mousekey 
; data from the buffer.  Quick and dirty way to do this would be to force the mouse clock
; line low, so the mouse would think that the host computer is inhibiting, but we have a serial
; mouse here, not the PS/2 mouse.   Since we again are not inside the mouse driver, we have
; a problem of knowing when it is safe to disable the real mouse (serial this time), since we
; have no way of knowing which data byte might currently be being sent. (i.e. 1st, 2nd or 
; 3rd)  Therefore, we can not truely disable the real mouse safely, so we will NOT currently
; inhibit the real mouse, but inform the user that both mice are active (real serial mouse 
; and MouseKeys) but that the user should not try to input data from both at the same instant 
; of time......
;---------------------------------------------------------------------------------------------
; If we get here,we have mouse data in buffer, so we need to check if PS/2 mouse check

NT_10:

	cmp	_fmouse_id,4				; do we have a PS/2 mouse ?
	jne	NT_50

;-----DEBUGGING------------------------------------------------------
;;;	cmp	fwindows_enh,true			; DEBUGGING
;;;	jne	NT_20
;	call	Inject_Mouse_Low
;	jmp	NT_110	
;
;-----DEBUGGING------------------------------------------------------

NT_20:
	call	Get_Mouse_Data				; ax has the data upon return
	mov	Mouse_Status,ax
	call	Get_Mouse_Data				; ax has the data upon return
	mov	Delta_X,ax
	call	Get_Mouse_Data				; ax has the data upon return
	mov	Delta_Y,ax

	call	InjectMouse
	jmp	short NT_110

;----------------------------------------------------------------------------------------------
; If we get here,we have mouse data in buffer, and we must have a Serial Mouse 

NT_50:

	cmp	_fmouse_id,2				; do we have a serial mouse ?
	jne	NT_110


	cmp	_comp_id,1				; are we on a PC/XT
	je	NT_60
	cmp	_comp_id,1Ah				; are we on a PC/XT with int 15h
	je	NT_60

NT_55:
	cmp	fslow_baud_mouse,true			; do we have a slow mouse
	jne	NT_65

NT_60:

	cmp	fslow_mouse,true			; is this first pass at slow mouse or second ?
	je	NT_65					; second pas, so cont. on
	mov	fslow_mouse,true			; first pass, set flag and exit
	jmp	short NT_110

NT_65:
	mov	fslow_mouse,false			; reset on second pass or every pass if not a slow mouse
	mov	dx,_combase				; put address of combase in dx

; first check to be sure the byte we may have transmitted previously has been sent out

	add	dx,5
	in	al,dx
	test	al,00100000b				; bit 5, transmit buffer should be clear if okay to write
	jz	NT_110					; if zero, register is not empty, jump around

; next check to be sure we are not suppossed to stop sending serial mouse data

	cmp	fserial_stop,true			; if set true, we should stop sending serial mouse data and flush th buffer
	jne	NT_70					; if not set, okay to send so cont

; if we get here, we are suppossed to stop sending serial mouse data, but we need to be sure we have sent a complete packet (3 bytes)
; before we quit.  The easiest way to do this, will be to get a byte from the buffer, and if bit 6 is set, then it is the first byte
; of another three byte packet, and then it is safe to stop sending serial mouse data

	call	Get_Mouse_Data				; get the next mouse data if it exists
	test	al,01000000b				; is bit 6 set high
	jz	NT_80					; if bit 6 is 0, not forst byte, cont. on

; if we get here, bit 6 was set, it is a first byte of a packet, and it is safe to flush the mouse data buffer

	mov	bx,OFFSET mouse_data			; get address of mouse_data buffer
	mov	mouse_data_head,bx			; reset head pointer to start of buffer
	mov	mouse_data_tail,bx			; reset tail pointer to start of buffer
	mov	mouse_cnt,false				; 
	mov	fserial_stop,false
	jmp	short NT_105


NT_70:
	call	Get_Mouse_Data				; get the next mouse data if it exists
NT_80:

	mov	dx,_combase				; put address of combase in dx
	out	dx,al		

NT_90:
	jmp	short NT_110

NT_105:
;--------------------------------------------------------------------------------------------------------------------
; call original timer, If jump was to NT_105, then mouse data buffer is empty, and we can reset fmouse_button flag

	mov	fmouse_button,false			; if buffer is empty reset flag for a serial mouse

NT_110:

; ************ later setup that if this is called and serial port has input, we won't do stuff after orig. int *****************

	cmp	fserial_keys_loaded,true		; is serial keys loaded ?
	jne	NT_112
	cmp	_serialKeysOn,true			; is SerialKeys code on ?
	jne	NT_112

	push	ds
	push	es
	call	_kickStartSerialKeys			; check if SerialKeys needs anything
	pop	es
	pop	ds

; ******************************************************************************************************************************

NT_112:

; If jump was to NT_110, then there was mouse data in the buffer buffer is empty, and after we call orig. int, we should 
; exit the timer int. since we have been inside the timer interrupt extra long doing something with mouse buffer data

	pushf
	call	dword ptr cs:old_1C_int			; call original timer 1Ch interrupt in case any one else chained in

	cmp	fmouse_button,false			; is mouse data buffer empty, (i.e. we didn't do any mouse activity)


	je	NT_115					; if the mouse data buffer was empty, we have the time to cont.
							; inside the timer interrupt


	jmp	NT_END					; since we had mouse activity, exit timer interrupt

;--------------------------------------------------------------------------------------------------------------------
; check to see if we need to call InjectKeys or just fake an interrupt 9h
; because we are running on a computer which supports int 15h and not hardware injection
;--------------------------------------------------------------------------------------------------------------------

NT_115:

; We will never inject if it is not a computer which supports int 15 intercept method of some kind...

	cmp	_vector,15h				; does this computer support int 15 h ?
							; this check should be REDUNDANT !!!!!
	je	NT_118					; cont. on and do an injection method if int 15h computer
	jmp	NT_200					; if not an int 15h computer, buffer should never get data and we should 
							; NEVER attempt to inject in any fashion

NT_118:

	mov	bx,key_data_head			; get head pointer of key_data buffer
	cmp	bx,key_data_tail			; test to see if head = tail, i.e. no data to send
	jne	NT_120					; if not equal, then we may need to empty buffer
	mov	fint9_by_timer,false			; if equal, reset this flag
	jmp	NT_200


NT_120:

; We will never try to inject if keyboard and system are talking...check mode indicator flag at 40:97 in BIOS...

	push	es
	mov	ax,RAMBIOS
	mov	es,ax
	assume	es:RAMBIOS
	mov	al,es:[97h]
	test	al,01000000b				; look at keyboard mode indicator update
	pop	es
	assume	es:_TEXT
	jz	NT_122					; if this flag is zero, safe to talke with keyboard since system is not
	jmp	NT_200					; if this flag is set, system is talking with keyboard, jump around and 
							; wait tilll that interaction is complete before trying to inject

NT_122:

; We will never try to inject if we are simultaneously filling our own keystroke buffer...

	cmp	fkeys_injected,true			; when this is true, something is filling the buffer, so we wait
	jne	NT_123					; when this is false, we can check the buffer for emptying
	jmp	short NT_200				


NT_123:

; Need to check 8259 interrupt controller to be sure that nothing other than the timer interrupt
; which we are currently in is pending before we fake a keyboard int using software int 9h.  We do this to avoid
; unwanted and dangerous extra EOI's being generated. We check "al" register upon return, and if it doesn't 
; equal 1, then we have other pending interrupts and we don't do an inject method now..

	call	controller_check			; check if 8259 controller has any other pending interrupts 
						     	

	cmp	al,01h					; is the timer and only the timer int. pending ?
	jne	NT_200					; if not, don't insert a key just yet
							; if yes, disable interrupts 

	cli						; disable interrupts, leaving timer int. will re-enable 

NT_125:

; Buffer HAS data, and we want to empty it, but need to check which method to use.

	cmp	_finject_keys,true			; can we inject into the hardware ?
	je	NT_150					; if yes, jump to here to do that routine


NT_130:

; If we get here, we do have a computer which supports int 15, but NOT hardware injection
; we must check for the two exception comnputers to the int15, 8042 insert of keys, they are the PS/2 Model 25/30-8086
; and the old PC or PX/XT running DOS 4+/5 with keyb.com loaded simulating int15 services
       
; Exception 1
; if _comp_id = 6, then we have the PS/2 Model 25/30-8086 and we must handle it by checking to see that
; that the timer didn't interrupt a keyboard interrupt, if not, cli, write to port 60h, and do and software int9h

; Exception 2
; if _comp_id = 1A, then we have the PC or PC/XT running DOS 4+/5 with keyb.com loaded and we must handle it by checking to see that
; that the timer didn't interrupt a keyboard interrupt, if not, do software int9h, and insert our code via int15h when it comes by
; since we cannot write a byte to keyboard port 60h on this computer.

	cmp	_comp_id,6
	jne	NT_142

	cmp	fnum_lock_was_off,true			; if we have a space saver kybd, wait till numlock kybd codes have passed
	je	NT_200					; before injecting StickyKey breaks
	jmp	short NT_145

NT_142:
	cmp	_comp_id,1Ah
	jne	NT_148

NT_145:
	cmp	_comp_id,1Ah				; is is the PC or PC/XT ?
	jne	NT_146
	mov	fint9_by_timer,true			; if PC or PC/XT signal to catch in int15h on the way back
	jmp	short NT_147

NT_146:

	call	Get_Key_Data				; ax has the data upon return when doing PS/2 Model 25/20-8086
	out	kb_data,al				; write to kybd data port
	mov	floop_back,true

NT_147:

	int	9h					; do a software int 9h
	jmp	NT_END

NT_148:

; I we get here, it was not one of the exception type computers form the abocve list, so cont. on with the kybd_echo routines...

	call	kybd_echo				; call our routine to insert a keystroke via 8042 undocumented feature
	jmp	NT_END

NT_150:

	cmp	fnum_lock_was_off,true			; if we have a space saver kybd, wait till numlock kybd codes have passed
	je	NT_200					; before injecting StickyKey breaks

	call	InjectKeys				; inject buffer data, keystrokes, into keyboard hardware buffer
	jmp	NT_END

NT_200:

;-----------------------------------------------------------------------------------------------------------------------
; Need to check our dialog flags here.  If fDialog_Action flag is false, (=0), then no action occurred and we can ignore
; any call to the respective turn on or turn off function.  If fDdialog_Action flag is true (1 ), then we need to call the
; respective routine to handle turning on or off of the feature.
;-----------------------------------------------------------------------------------------------------------------------

	cmp	fDialog_Action,true			; any action occur in Dialog or Menu's ?
	jne	NT_300					; if not, skip to next major check

	cmp	fDialog_Filter_off,false		; is filterkeys off ?
	je	NT_220					; yes
	call	FilterKeys_dialog
	jmp	NT_END					; exit timer int. any time we do a routine

NT_220:
	cmp	fDialog_Stickeys_off,false		; is stickeys off ?
	je	NT_240					; yes
	call	StickeyKeys_dialog
	jmp	NT_END					; exit timer int. any time we do a routine


NT_240:
	cmp	fDialog_Mouse_off,false			; is mousekeys off ?
	je	NT_260					; yes
	call	MouseKeys_dialog
	jmp	NT_END					; exit timer int. any time we do a routine

NT_260:
	cmp	fDialog_Toggle_off,false		; is togglekeys off ?
	je	NT_280
	call	ToggleKeys_dialog
	jmp	NT_END					; exit timer int. any time we do a routine
	
NT_280:	
	cmp	fDialog_TimeOut_off,false		; is timeout off ?
	je	NT_300
	call	TimeOut_dialog
	jmp	NT_END					; exit timer int. any time we do a routine

NT_300:
	mov	fDialog_Action,false			; reset this flag upon exit the above routines

;-------------------------------------------------------------------------------------------------------
; Hearing impaired attempt to display to screen a character for sound on/off
; check only 2 times a second otherwise cursor blinking is too annoying
; and also programs which take over the video like Microsoft Works will not
; start if we check the speaker port too often
;-------------------------------------------------------------------------------------------------------

	cmp	fhearing_on,true			; is hearing inpaired viaual cue flag set on ?
	je	video_1
	cmp	fvideo_flash,true			; is hearing impaired video flashing flag set on ?
	je	video_0
	mov	faccess_sound,false			; reset flag and
video_0:
	jmp	video_end				; if hearing impaired flag is not on, jump to here

; Go get the current video state and save it.  If it changes from the previous video state, we
; will prevent writes to video for a short period of time in hopes of not messing up the switched
; video state.
;
; Before we do anything, we will get current video state and check it with the old. Since we asssign
; video state a ZERO to start, it will alway pause when ADOS is first enabled.  We could try to read
; the video state in _Enable Proc and avoid this, if it seems to miss any beeps.

video_1:

	push	es
	mov	ax,RAMBIOS
	mov	es,ax
	assume	es:RAMBIOS
	mov	al,es:[49h]				; read same area int 10h would, but cause less video trouble if not
	pop	es					; going to write visual cue this time thru
	assume	es:_TEXT

;	mov	ah,0fh					; read current video state
;	int	10h					; int 10

	cmp	al,video_state				; is the read video state the same as previous video state ?
	je	video_3					; if yes, cont. on here
	mov	video_state,al				; if not, save current video state
							; and set the video_count such that a 5 second delay will 
							; take palce while the video state is changing

	mov	video_count,07fh			; reset such that it will take 5 seconds to start video 
							; for hearing impaired if on
	mov	faccess_sound,false			; if ADOS produced a sound during a switch, reset that flag also
	mov	fswitching_video,true			; inform myself (ADOS) that video mode is switching
	jmp	video_end				; if switching video , jump out

video_3:

	inc	video_count				; increment counter each time through
	cmp	video_count,9				; have we reach 1/2 second ?
	je	video_4
	jmp	video_end				; if not at 9 yet, jump around

video_4:
	mov	video_count,0				; reset count to 0
	mov	fswitching_video,false			; inform myself (ADOS) that safe to do video

; any reason to produce a video que for some kind of sound which happened or is happening ?

	cmp	faccess_sound,true			; did AccesDos produce a beep
	je	video_20				; yes or speaker did 

video_10:	

	in	al,kb_ctl				; read speaker control port
	and	al,02					; test speaker bit on/off
	jnz	video_18				; speaker bit is high, do video
	jmp	video_end

video_18:
	mov	faccess_sound,true			; use our variable to flag speaker bit toggling also

video_20:

	cmp	video_state,7				; is this mode 7, Mono text
	je	video_22
	cmp	video_state,3				; is this any of the Text video modes below 3
	jle	video_22
		       					; if not, must be in a graphics mode, so 
	mov	txt_or_grph,09h	     			; write a character to video graphically
	jmp	short video_23

video_22:
	mov	txt_or_grph,0ah	 			; write a character to video as text

video_23:

	cmp	fvideo_type,3				; check if we have a CGA monitor
	jne	video_25				; if not, continue

; if yes, don't write to video memory until horizontal retrace is in progress

	mov	dx,3dah
	in	al,dx					; read CGA status byte
	test	al,1					; test bit 1
	jnz	video_25				; if bit is set, in retrace, semi safe to write
	jmp	video_end

video_25:

; if not true, not a CGA monitor and we can write to video directly

	mov	ah,3					; read current cursor position
	mov	bh,0					; page 0
	int	10h					; video interrupt
	mov	old_row,dh				; save current row
	mov	old_col,dl				; save current column
	mov	old_cursor_1,cl				; save current cursor type
	mov	old_cursor_2,ch				; save current cursor type

	mov	ah,2					; prepare to set current position
	mov	dh,0					; row 0
	mov	dl,0					; column 0
	mov	bh,0					; page 0
	int	10h					; video int
			     
video_30:

	mov	ah,08h					; read character and attribute at cursor
	int 	10h
	mov	old_char_1_attr,ah	 		; put attributes into variable

	cmp	al,14					; did we just read back the hearing impaired sysbol ?
	je	video_50

	mov	old_char_1,al				; save orig. char. to rewrite

	mov	ah,2					; prepare to set current position
	mov	dh,0					; row 0
	mov	dl,1					; column 1
	mov	bh,0					; page 0
	int	10h					; video int

	mov	ah,08h					; read character and attribute at cursor
	int 	10h
	mov	old_char_2_attr,ah	  		; put attributes into variable
	mov	old_char_2,al				; save orig. char. to rewrite

	mov	ah,txt_or_grph				; set video mode to write in
video_40:

	cmp	fsecond_cue,false
	je	video_45

	mov	cx,1					; write 1 character			
	mov	al,fsecond_cue				; 
	mov	bl,old_char_2_attr
	int	10h

video_45:

	mov	ah,2					; prepare to set current position
	mov	dh,0					; row 0
	mov	dl,0					; column 0
	mov	bh,0					; page 0
	int	10h					; video int

	mov	cx,1					; write 1 character			
	mov	ah,txt_or_grph				; set video mode to write in
	mov	bl,old_char_1_attr
	mov	al,14					; send ASCII character 14 to video for sounds on
	int	10h

	jmp	video_70				; jump to restore cursor

video_50:

	mov	ah,txt_or_grph				; set video mode to write in
video_66:

	mov	cx,1					; write 1 character			
	mov	faccess_sound,false
	mov	bl,old_char_1_attr
	mov	al,old_char_1				; replace original character
	int	10h

	cmp	fsecond_cue,false
	je	video_70

	mov	ah,2					; prepare to set current position
	mov	dh,0					; row 0
	mov	dl,1					; column 1
	mov	bh,0					; page 0
	int	10h					; video int

	mov	ah,txt_or_grph				; set video mode to write in
	mov	bl,old_char_2_attr
	mov	al,old_char_2				; replace original character
	mov	fsecond_cue,false			; reset fsecond_cue flag
	int	10h

video_70:

	mov	ah,2					; restore current cursor position
	mov	ch,old_cursor_2
	mov	cl,old_cursor_1
	mov	dh,old_row
	mov	dl,old_col
	mov	bh,0
	int	10h

	jmp	NT_END					; exit timer int. any time we do a routine

video_end:

;-------------------------------------------------------------------------------------------------------
; video flash routines


	cmp	fvideo_flash,true			; is hearing impaired video flashing flag set on ?
	je	flash_10
	jmp	flash_end_out

flash_10:

	cmp	fvideo_flash_on,true			; did we already flash
	je	flash_40				; if yes, jump to routine to turn video back on

	cmp	faccess_sound,true			; did AccesDos produce a beep
	jne	flash_15				
	mov	video_cnst,4				; yes  
	jmp	short flash_30

flash_15:	

	in	al,kb_ctl				; read speaker control port
	and	al,02					; test speaker bit on/off
	jnz	flash_20				; speaker bit is high, do video
	jmp	flash_end_out

flash_20:
	mov	faccess_sound,true			; use our variable to flag speaker bit toggling also
	mov	video_cnst,4				; speaker did, keep seperateincase we change system flash length

flash_30:

;;	cmp	fvideo_flash_on,true			; did we already flash
;;	je	flash_40				; if yes, jump to routine to turn video back on

	mov	fvideo_flash_on,true
	call	video_flash_on				; subroutine to do borderflash on
	jmp	short flash_end

flash_40:


	inc	fvideo_flash_cnt
	mov	al,video_cnst
	cmp	al,fvideo_flash_cnt
	jne	flash_end_out

	call	video_flash_off				; subroutine to do borderflash off
	mov	faccess_sound,false
	mov	fvideo_flash_cnt,0
	mov	fvideo_flash_on,false

flash_end:

	jmp	NT_END					; exit timer int. any time we do a routine

flash_end_out:

;--------------------------------- End of Video for Hearing Impaired flag ------------------------------

	cmp	_finject_keys,true			; if flag isn't true, donot check any further
	jne	NT_END

	cmp	fwindows_enh,true			; are we in enhanced mode ?
	jne	NT_END

	cmp	falt_esc_send,true			; safe to send codes ?
	jne	sound_100

	mov	al,038h					; put ALT make into buffer
	call	Put_Key_Data
	mov	al,001h					; put ESC make into buffer
	call	Put_Key_Data
	mov	al,081h					; put ESC break into buffer
	call	Put_Key_Data
	mov	al,0b8h					; put ALT break into buffer
	call	Put_Key_Data

sound_90:

	mov	falt_esc,false
	mov	falt_esc_send,false
	jmp	short NT_END

sound_100:

;--------------------------------- check if we ever loaded Windows Real mode, and if we did, did we exit

	cmp	fwindows_after_re,true			; was Windows ever startec in REAL mode ?
	jne	NT_500

	mov	ax,04680h				; use int2f to see if still loaded
	int	2fh
	cmp	al,00					; al = 00, if REAL mode still running
	je	NT_500
							; if al <> 00, then REAL mode exitted, reset flags

	mov	video_count,07fh			; reset such that it will take 5 seconds to start video 
	mov	fwindows_after_re,false
	mov	fwindows_after,false
	mov	al,fhearing_on_save			; restore upon exit
	mov	fhearing_on,al
	mov	al,fvideo_flash_save
	mov	fvideo_flash,al


NT_500:

;----------------------------------- run if and only if ADOS started after windows in enhanced mode-----------------

	cmp	fctrl_esc_send,true			; safe to send codes ?
	jne	NT_END

	mov	al,01dh					; put CTRL make into buffer
	call	Put_Key_Data
	mov	al,001h					; put ESC make into buffer
	call	Put_Key_Data
	mov	al,081h					; put ESC break into buffer
	call	Put_Key_Data
	mov	al,09dh					; put CTRL break into buffer
	call	Put_Key_Data

	mov	fctrl_esc,false
	mov	fctrl_esc_send,false

NT_END:

	mov	ftimer_1C_active,false

;NT_END_5:

	pop	ax
	pop	bx					; restore registers
	pop	cx
	pop	dx
	pop	di
	pop	si
	pop	es
	assume	ES:NOTHING
	pop	ds
	assume	DS:NOTHING

	iret

_new_timer_int	endp

;--------------------------------------------------------------------------------------------------------

controller_check	proc	near

	assume DS:_TEXT

	mov	al,0Bh
	out	ack_port,al				; write operation command byte 3, to get ISR 
	jmp	$+2

	in	al,ack_port				; read the 8259 controller port
	jmp	$+2

	mov	ah,al
	mov	al,08h
	out	ack_port,al				; write operation to rese controller

	mov	al,ah
	ret

	controller_check	endp
;--------------------------------------------------------------------------------------------------------

video_flash_on	proc	near

	assume DS:_TEXT
	assume	es:NOTHING

	mov	ax,RAMBIOS
	mov	es,ax

	assume	es:RAMBIOS
	mov	al,es:vid_flag
	and	al,0f7h

	cmp	fvideo_type,1
	jne	von_10
	
        mov     dx,3b8h
        out     dx,al
	mov	es:vid_flag,al
	jmp	short von_end

von_10:

	cmp	fvideo_type,3
	jne	von_30

        mov     dx,3d8h
        out     dx,al
	mov	es:vid_flag,al
	jmp	short von_end

von_30:	

	cmp	fvideo_type,4
	jne	von_40

        mov     dx,3bah
        in      al,dx
        mov     dx,3c0h
        xor     al,al
        out     dx,al
	jmp	short von_end

von_40:

	cmp	fvideo_type,5
	jne	von_50
        mov     dx,3dah
        in      al,dx
        mov     dx,3c0h
        xor     al,al
        out     dx,al
	jmp	short von_end


von_50:

	cmp	fvideo_type,6
	jne	von_60
        mov     dx,3d8h
        in      al,dx
        and     al,0f7h
        out     dx,al
	mov	es:vid_flag,al
	jmp	short von_end

von_60:
	cmp	fvideo_type,7
	jne	von_end
        mov     ax,1201h
        mov     bx,0036h
        int     10h

von_end:
	assume	es:NOTHING
	ret

video_flash_on	endp

;--------------------------------------------------------------------------------------------------------

video_flash_off proc	near

	assume DS:_TEXT
	assume	es:NOTHING

	mov	ax,RAMBIOS
	mov	es,ax

	assume	es:RAMBIOS
	mov	al,es:vid_flag
	or	al,08h

	cmp	fvideo_type,1
	jne	voff_10

        mov     dx,3b8h
        out     dx,al
	mov	es:vid_flag,al
	jmp	short voff_end

voff_10:

	cmp	fvideo_type,3
	jne	voff_30

        mov     dx,3d8h
        out     dx,al
	mov	es:vid_flag,al
	jmp	short voff_end

voff_30:	

	cmp	fvideo_type,4
	jne	voff_40
        mov     dx,3bah
        in      al,dx
        mov     dx,3c0h
        mov     al,20h
        out     dx,al
	jmp	short voff_end


voff_40:

	cmp	fvideo_type,5
	jne	voff_50
        mov     dx,3dah
        in      al,dx
        mov     dx,3c0h
        mov     al,20h
        out     dx,al
	jmp	short voff_end


voff_50:

	cmp	fvideo_type,6
	jne	voff_60
        mov     dx,3d8h
        in      al,dx
        or      al,08h
        out     dx,al
	mov	es:vid_flag,al
	jmp	short voff_end

voff_60:
	cmp	fvideo_type,7
	jne	voff_end

        mov     ax,1200h
        mov     bx,0036h
        int     10h

voff_end:

	assume	es:NOTHING
	ret

video_flash_off	endp

;-----------------------------------------------------------------------------------------------------------------
;*****************************************************************************************************************
; This is the beginning of the resident code of the routine which receives
; the keyboard scan code via int 15h from the keyboard interrupt, int 9h.  It builds up 
; the extended scan code while also passing the scan code along back to interrupt 9, most of the time.
; If after passing thru the keyboard features, it is detemined that the scan code should not pass along,
; the software returns to int. 9h with the carry flag cleared.  
;
; The flags fbios_called_direct or fbios_called_timing are used to tell if the scan code should get
; passed back to interrupt 9h, or should be eaten upon passing back to interrrpt 9h.
;
; The scan code is in Al when int 15h is called with Ah = 4f
;*****************************************************************************************************************
;-----------------------------------------------------------------------------------------------------------------


_keybd_int_15  PROC	far

	cmp	ah,4fh					; is this an int15 for the keyboard ?
	je	kybd_int_15_5

	jmp	not_kybd_int_15

kybd_int_15_5:

	push	ds					; save the ds and ax
	push	es
	push	bx
	push	ax

	push	cs
	pop	ds
	assume DS:_TEXT
	push	cs
	pop	es
	assume ES:_TEXT

;---------------------------------------------------------------------------------------------------------------------------
; difference here to standard int 9h routine *******************************************************************************
;---------------------------------------------------------------------------------------------------------------------------

	mov	fmouse_or_kybd,true			; flag any int. callers, that kybd int. 9h executed
	call	TimeOut_Reset				; reset the time out count
	mov	fbios_called_direct,false 		; reset to false upon entry

;----------------------------------------------------------------------------
; we do not want to have any of the routines that process key input have to
; deal with any of the commands that are specifc to the operation of the 
; keyboard, so we will pass them on directly to the BIOS routine since the
; original device driver routine also passed commands to the BIOS routine.
; See original BIOS of Tech. Ref. manuals for PC/PCXT/PCAT
;
	pop	ax

	cmp	al,kybd_Command				; is it a command (0EDh)?
	jb	not_command_15				; no -->

kybd_command_50:

	jmp	kybd_int_15_exit	  		; done -->

;----------------------------------------------------------------------------

not_command_15:

;	cmp	fserial_keys_loaded,true		; is serial keys loaded ?
;	jne	kybd_int_15_5A
	cmp	_serialKeysOn,true			; is SerialKeys code on ?
	jne	kybd_int_15_5A

;---------------------------- serial port keyboard emulation ----------
; if we get here, we need to first look and see if SerialKeys injected a code

 	test	cs:_forcedInt9Flag,true			; if flag is true, SerialKeys injected a scan code
	jz	kybd_int_15_5A

	mov	cs:_forcedInt9Flag,false   		; reset flag
	mov	fserial_key_recieved,true		; set flag for my code modules
	mov	ah,04fh					; fill in the rest of a fake int 15h AX 
	mov	al,cs:_injectByte   			; inject new scan code

;IFDEF	BUG
;	mov	portid,9
;	mov	portout,al
;	call	HexCharsOut
;ENDIF;	BUG			

;	call	click
	jmp	short kybd_int_15_7


kybd_int_15_5A:
;---------------------------- keyboard emulation-----------------------
; When we get here, we need to check if this is a real int9 or did the timer simulate an int9 when int 15h was called ?

	cmp	fint9_by_timer,true			; if the timer simulated an int9, this flag will be true
	jne	kybd_int_15_6				; if false, it is a real int 9 and call to int 15
	call	Get_Key_Data				; if true, this is a fake int9, and we want to get 
							; the scan code to pass out of the key_data_buffer
							; the scan code we want to pass is now in AL register
	mov	ah,04fh					; fill in the rest of a fake int 15h AX 
	jmp	short kybd_int_15_7

kybd_int_15_6:


;IFDEF	BUG
;;	mov	fserial_debugging,true
;	mov	portid,0
;	mov	portout,al
;	call	HexCharsOut
;;	mov	fserial_debugging,false
;ENDIF;	BUG

	cmp	fkeys_injected,false
	jne	kybd_int_15_6C
	push	bx
	mov	bx,key_data_head			; get head pointer of key_data buffer
	cmp	bx,key_data_tail			; test to see if head = tail, i.e. no data to send

	je	kybd_int_15_6B				; if equal, then we may need to empty buffer
	cmp	floop_back,true
	je	kybd_int_15_6B

	push	es
	push	ax
	mov	ax,RAMBIOS
	mov	es,ax
	assume	es:RAMBIOS
	mov	al,es:[97h]
	test	al,01000000b				; look at keyboard mode indicator update
	pop	ax
	pop	es
	assume	es:_TEXT
	jz	kybd_int_15_6A				; if clear, comp. and kybd are not talking, okay to in-line inject
	jmp	short kybd_int_15_6B

kybd_int_15_6A:

	call	Put_Key_Data				; save current al
	call	Get_Key_Data				; get my code
	mov	ah,04fh					; fill in the rest of a fake int 15h AX 

kybd_int_15_6B:
	mov	floop_back,false	 		; reset
	pop	bx


kybd_int_15_6C:


;IFDEF	BUG
;;	mov	fserial_debugging,true
;	mov	portid,1
;	mov	portout,al
;	call	HexCharsOut
;;	mov	fserial_debugging,false
;ENDIF;	BUG

kybd_int_15_7:
;---------------------------------------------------------
; first check to see if the space saver keyboard is in use
;---------------------------------------------------------

	cmp	fspace_saver,true			; is flag set to indicate space saver ?
	jne	kybd_space_saver_end			; if not, exit to next check

kybd_space_saver_15_3:

	cmp	al,046h					; is space saver kybd, so check is this SCROLL LOCK make key ?
	jne	kybd_space_saver_15_5
	jmp	short kybd_space_saver_15_15

kybd_space_saver_15_5:

	cmp	al,0C6h					; is this SCROLL LOCK break key ?
	jne	kybd_space_saver_end			; if not, exit
	cmp	fscroll_changed,true			; did we previously detect shift+numlock ?
	jne	kybd_space_saver_end			; if not, exit
	mov	fscroll_changed,false			; reset flag
	mov	al,0C5h					; insert NUM LOCK break into "al" register instead of scroll lock break
	jmp	short kybd_space_saver_end

kybd_space_saver_15_15:

	cmp	fscroll_changed,true			; did we already change scroll lock to num lock and this is typematic
	jne	kybd_space_saver_15_25			; because if it is, it will also need to be changed, as StickyKeys would have cleared
							; the shift flag by now...If a person hold both keys down, we don't have a problem, since the
							; keyboard will then send a "45,C5", not a "46,C6" code

	jmp	short kybd_space_saver_15_30		; fscroill was set, so keep changing makes until we get a break


kybd_space_saver_15_25:

	push	es
	push	ax
	push	bx
	assume	ES:NOTHING
	mov	bx,RAMBIOS
	mov	es,bx
	ASSUME	ES:RAMBIOS
	mov	al,es:kb_flag
	test	al,03h					; is a shift key also down with the scroll lock ?
	jz	kybd_space_saver_15_50			; if not, cont
	mov	fscroll_changed,true			; if yes, set htis flag to catch on the scroll break
	pop	bx
	pop	ax
	pop	es
	assume ES:_TEXT

kybd_space_saver_15_30:

	mov	al,045h				      	; change al to NUM LOCK make
	jmp	short kybd_space_saver_end

kybd_space_saver_15_50:

	pop	bx
	pop	ax
	pop	es
	assume ES:_TEXT

kybd_space_saver_end:

;-----------------------------------------------------
; Next, need to check if we are in a control break condition
;-----------------------------------------------------

	cmp	fctrl_break_on,true			; was ctrl break flagged on previous int ?
	jne	kybd_int_15_13
	cmp	al,0c5h					; control break condition on, wait for "C5h" to clear
	jne	kybd_int_15_12				; if not "C5h" yet, jump to end

kybd_int_15_10:
; if we get here, we are looking for the "C5h" scan code only-last part of PAUSE key
; what ever is in "ax" register doesn't need to be saved, since we are throwing it away
	
	push	cx
	push	si
	push	ax
	xor	cx,cx
	xor	ax,ax
	mov	fkeys_injected,true			; set flag since we are going to put keys in the buffer

kybd_int_15_11:

	mov	cl,ctrl_break_buf_cnt
	mov	bx,offset ctrl_break_buf
	mov	si,cx
	mov	al,[bx+si]
	call	Put_Key_Data
	inc	ctrl_break_buf_cnt

	mov	al,ctrl_break_buf_cnt
	mov	bl,ctrl_break_buf_len
	cmp	al,bl
	jne	kybd_int_15_11

	mov	ctrl_break_buf_cnt,false		; reset buffer count
	mov	fkeys_injected,false			; buffer stuffed, turn off flag so timer can inject
	mov	fctrl_break_on,false			; if it is "C5h" jump to end

	pop	ax
	pop	si
	pop	cx

kybd_int_15_12:

	jmp	keybd_int_15_end     			; jump to end, since we ate a code

kybd_int_15_13:

;;----------------------------------------------------------------------------------------------------------------------
;; For this next routine, we need to check if we support Inject keys routine

	cmp	_finject_keys,true			; first check if we can inject ?
	je	kybd_win_15_5				; if we can inject, do next code which is based on the ability to INJECT

	jmp	kybd_win_end				; if we CAN NOT inject, cont. at _13

;-------------------------------------------------------------------------------------------
; THIS CODE ASSUMES THE KEYBOARD AND COMPUTER ACCEPT DIRECT INJECTION OF MOUSE AND KYBD CODES
;-------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------
; next check to see if we are running under Windows 3.0 in the enhanced mode only
;-------------------------------------------------------------------------------------------

kybd_win_15_5:


	cmp	fwindows_enh,true			; is windows running ?
	jne	kybd_win_end


kybd_win_15_7:

	cmp	al,01h					; did we read in an "ESC" key ?
	jne	kybd_win_15_10
 	jmp	short kybd_win_15_30			; was an "ESC" key make , check if "ALT" key down ?

kybd_win_15_10:

	cmp	al,081h					; is this the "ESC" break ?
	jne	kybd_win_end

kybd_win_15_20:						; was an "ESC" key break

	cmp	falt_esc,true
	jne	kybd_win_15_25
	mov	falt_esc_send,true
	mov	falt_esc,false
	jmp	short kybd_win_end

kybd_win_15_25:

	cmp	fctrl_esc,true
	jne	kybd_win_end
	mov	fctrl_esc_send,true
	mov	fctrl_esc,false
	jmp	short kybd_win_end

kybd_win_15_30:

	push	es
	push	ax
	push	bx
	assume	ES:NOTHING
	mov	bx,RAMBIOS
	mov	es,bx
	assume	ES:RAMBIOS
	mov	al,es:kb_flag
	test	al,08h
	jz	kybd_win_15_50

	mov	falt_esc,true
	jmp	short kybd_win_15_95

kybd_win_15_50:
	mov	falt_esc,false
	test	al,04h
	jz	kybd_win_15_60

	mov	fctrl_esc,true
	jmp	short kybd_win_15_95

kybd_win_15_60:

	mov	fctrl_esc,false

kybd_win_15_95:

	pop	bx
	pop	ax
	pop	es
	assume ES:_TEXT

kybd_win_end:

;;;----------------------------------------------------------------------------
;;; we do not want to have any of the routines that process key input have to
;;; deal with any of the commands that are specifc to the operation of the 
;;; keyboard, so we will pass them on directly to the BIOS routine since the
;;; original device driver routine also passed commands to the BIOS routine.
;;; See original BIOS of Tech. Ref. manuals for PC/PCXT/PCAT
;;;
;;	cmp	al,kybd_Command				; is it a command (0F0h)?
;;	jb	not_command_15				; no -->
;;	jmp	kybd_int_15_exit	  		; done -->
;;
;----------------------------------------------------------------------------
; we need to build up the two byte key codes if appropriate.

	cmp	al,kb_ext_code1				; is it a hidden code prefix e0?
	je	is_hidden_code_15			; yes, save -->
	cmp	al,kb_ext_code2				; is it a hidden code prefix e1?
	jne	not_hidden_code_15			; no, -->

is_hidden_code_15:

	mov	hidden_code,al				; yes, save

; at this point, we know we had a hidden code, so check for ctrl-break
; or alt-print screen for sysreq.

	push	es					; save es
	push	ax
	assume	ES:NOTHING
	mov	bx, RAMBIOS				; BIOS RAM segment at 40h
	mov	es,bx					;  .. point ES to that!
	assume	ES:RAMBIOS
	mov	al,es:kb_flag				; 

	cmp	hidden_code,0E1h			; is it "E1h" ?
	jne	kybd_int_15_20				; if not jump to check the "E0h" code

	test	al,04h
	jz	kybd_int_15_30				; if zero, the control key isn't pressed
	mov	fctrl_break_on,true			; set flag to tell us we have ctrl break condition
	mov	fpause_being_sent,false			; if control break, not a PAUSE
	jmp	short kybd_int_15_30


kybd_int_15_20:
; if it jumps here, we know we have a hidden code, and it must be "E0h" only, and all we want to check is if an alt. key is down

	test	al,08h
	jz	kybd_int_15_25				; if not set, jump out
	mov	fsysreq,true				; alt. key is down with "E0h" code, MAY have sys req. in progress
	jmp	short kybd_int_15_30

kybd_int_15_25:
	mov	fsysreq,false				; alt key not down with "E0h" code
kybd_int_15_30:

	pop	ax
	pop	es
	assume ES:_TEXT

kybd_int_15_35:

	cmp	fctrl_break_on,true			; are we in a ctrl break condition ???
	jne	kybd_int_15_45
	jmp	keybd_int_15_end 			; reset without any call to original BIOS

kybd_int_15_45:
	cmp	al,0E1h					; is this an E1h hidden code ?
	jne	kybd_int_15_46

	mov	fpause_being_sent,true			; if "E1h" unaltered, then PAUSE key is being sent, flag this so MouseKeys
							; doesn't turn off on the "45h" part of the scan code
							; will alway stay on unless MouseKeys is on, detects this, and clears it
							; or it is a computer which Injects, then it is turned off above kybd_int_20
kybd_int_15_46:	   

	jmp	kybd_int_15_exit 			; pass the E0 or E1 code

;------------------------------------------------------------
; now build up the code and pass to first one of the routines
;------------------------------------------------------------
; last check for alt-print screen for sysreq done here
;------------------------------------------------------------

not_hidden_code_15:

; int 15h with ah = 4fh will get burnt here, since ah is replaced with hidden code and passed thru AccessDos features
; appears not to bother int 15h, 4fh, unless DOS keyb.com is running, so I reworked this 
; code to maintain the "ah"=4fh ..........

	push	ax					; save al, ah =4fh 

	mov	ah,hidden_code				; get current hidden code value
	mov	hidden_code,0				; reset hidden code

; for passing thru keyboard features only

	cmp	fsysreq,true				; did we previously set this flag due to alt.+"E0h" code ?
	jne	kybd_int_15_48				; if not, cont.
; if yes, then we must watch for the "B7h" scan code which is PrtSc make code

	cmp	al,0B7h					; is this the "37h" key  scan code ?
	jne	kybd_int_15_48				; cont. on if not

; if we get here, it was "B7h" scan code, so we pass the last code and inject the SysReq codes (54/D4)

	push	ax
	mov	fkeys_injected,true			; set this flag to inject
	mov	al,054h					; SysReq make code
	call	Put_Key_Data
	mov	al,0D4h					; SysReq break code
	call	Put_Key_Data
	mov	fsysreq,false				; we've inject sysreq, so clear htis falg 
	pop	ax

;;	mov	fkeys_injected,false			; we are done stuffing buffer, so reset this flag to allow timer to inject
;;	jmp	short kybd_int_15_exit      		; exit

kybd_int_15_48:

; Now call Handicap routines.  We do not expect any registers to be
; the same.  So the routine does not need to save any.


	call	FilterKeys				; pass on extended code in AX

	pop	ax					; restore ah, and al, to code to be passed or eaten

	test	al,break_bit				; was this code a break code ?
	jz	kybd_int_15_48A				; it was a make scan code, check other condition 

	mov	fkeys_injected,false			; reset this flag if a break upon return
	jmp	short kybd_int_15_49			; 

kybd_int_15_48A:

	cmp	fSticKeysOn,true			; is StickeyKeys On ?
	je	kybd_int_15_48C				; 

	cmp	fint9_by_timer,true			; is this an injected scan code by the timer ?
	je	kybd_int_15_48C				; 
	cmp	al,46h
	je	kybd_int_15_48C				; 
	cmp	al,54h
	je	kybd_int_15_48C				; 
	jmp	short kybd_int_15_49			; if flag gets reset on a break, okay

kybd_int_15_48C:

	mov	fkeys_injected,false			; if yes, reset this flag also so buffer will empty makes/breaks

kybd_int_15_49:

	cmp	fbios_called_timing,true		; Does the timer want to let a key pass ? (FROM FILTERKEYS ONLY)
	jne	kybd_int_15_50

	cmp	fclick_on,true
	jne	kybd_int_15_49A

	cmp	fshift_click,true
	je	kybd_int_15_49A

	cmp	current_shift,0
	je	kybd_int_15_49B				; if not a modifier key, call click
	mov	fshift_click,true			; if we clicked once, set, as non-modifier will clear

kybd_int_15_49B:

	call	click

kybd_int_15_49A:

	mov	fbios_called_timing,false		; reset flag
	jmp	short kybd_int_15_exit			; and allow code to pass

kybd_int_15_50:

	cmp	fbios_called_direct,true  		; do we want to pass on the code ?
	jne	keybd_int_15_end 			; no, so end and eat the code
		
;-----------------------------------------
; jumps to here, if passing scan code on as is or what ever has been stored in the AL register

kybd_int_15_exit:

	mov	fserial_key_recieved,false		; reset flag for my code modules

;IFDEF	BUG
;;	mov	fserial_debugging,true
;	mov	portid,2
;	mov	portout,al
;	call	HexCharsOut
;;	mov	fserial_debugging,false
;ENDIF;	BUG

	pop	bx
	pop	es
	pop	ds
	assume	DS:NOTHING
	assume	ES:NOTHING

	stc
	jmp	dword ptr cs:old_kybd_int			; call old int 15 routine

;-----------------------------------------
; jumps to here if ate a code, if we eat code, return to int15 caller
; with ret 2, which in effect, through away flags on stack and allows
; our carry flag clear to return as it should

keybd_int_15_end:


	mov	fserial_key_recieved,false		; reset flag for my code modules

;;IFDEF	BUG
;;	mov	fserial_debugging,true
;;	mov	portid,3
;;	mov	portout,al
;;	call	HexCharsOut
;;	mov	fserial_debugging,false
;;ENDIF;	BUG

	pop	bx
	pop	es
	pop	ds

	assume	DS:NOTHING
	assume	ES:NOTHING

	clc
	ret	2

;-----------------------------------------
; jumps to here, if not int15h with Ah=4fh

not_kybd_int_15:

	jmp	dword ptr cs:old_kybd_int			; call old int 15 routine

_keybd_int_15	endp

;----------------------------------------------------------------------------
; keybd_int
;
; This is the beginning of the resident code of the routine which intercepts 
; the keyboard interrupt, int 9h.  This keyboard interrput routine should only load
; and run for IBM PC /PCXT/ PCAT Original 1/10/84 (i.e. 84 key keyboard) computers.
; Non of these computer should ever see a hidden code (i.e E0/E1), and non of these 
; computers support the Extended BIOS or kb_flags1/3 or hardware keyboard injection.  
;
; Since I know some PC/PCXT/PCAT users prefer 101 keyboard, and that these keyboards
; typically cheat by sending hidden codes for the extra keys expecting the BIOS 
; to straighten the situation out, I can easily send on any hidden codes straight to BIOS
; where the computer will ignore them, and this may allow our code to work on 
; these NASTY keyboards.
;-----------------------------------------------------------------------------
; NOTE: IF I get codes for 101 cursor keys like 2A 4D AA 2A CD AA for the --> key, 
; we will still have trouble since the extra 2A and AA look like shift keys to PC/PCXT/PCAT
; Original, since it never had cursor keys, and if it did have cursor keys, they should 
; send E0 2A E0 4D E0 CD E0 AA for the --> key, which would distinguish it correctly,
; and this code should handle by passing along to BIOS where the hidden codes would be ignored.
;-----------------------------------------------------------------------------

_keybd_int  PROC	


	push	ax
	mov	ax,1					; watch for myself to be re-entrant
	xchg	cs:fint9_active_temp,al			; flag that I'm in interrupt

	cmp	al,1					; which time are we ???
	je	kybd_int_3
	
	mov	cs:kybd_ss_save,ss
	mov	cs:kybd_sp_save,sp
	push	cs
	pop	ss
	mov	sp,offset kybd_Stack_top

kybd_int_3:

	push	ax					; save value of ax

	push	ds					; save the ds and ax
	push	ax
	push	bx
	push	es

	push	cs
	pop	ds
	assume DS:_TEXT
	push	cs
	pop	es
	assume ES:_TEXT

	call	TimeOut_Reset				; reset the time out count
	mov	fbios_called_direct,false
	mov	fmouse_or_kybd,true			; flag any int. callers, that kybd int. 9h executed

kybd_int_5:

	cmp	_comp_id,5				; do we have an AT class machine
	jne	kybd_int_8				; if not, cont.

	mov	al,dis_kbd				; if yes, disable keyboard
	call 	ship_it


kybd_int_8:
	
	in	al,kb_data				; read in data from keyboard port

;IFDEF	BUG
;	mov	portid,0
;	mov	portout,al
;	call	HexCharsOut
;ENDIF;	BUG

;----------------------------------------------------------------------------
; we do not want to have any of the routines that process key input have to
; deal with any of the commands that are specifc to the operation of the 
; keyboard, so we will pass them on directly to the BIOS routine since the
; original device driver routine also passed commands to the BIOS routine.
; See original BIOS of Tech. Ref. manuals for PC/PCXT/PCAT
;
	cmp	al,kybd_Command				; is it a command (0EDh)?
	jb	not_command				; no -->

kybd_int_10:

	pushf						; yes, call bios routine
	call	dword ptr cs:old_kybd_int
	jmp	keybd_int_ret				; done -->

not_command:
;----------------------------------------------------------------------------
; we need to pass on hidden key codes if appropriate.

	cmp	al,kb_ext_code1				; is it a hidden code prefix e0?
	je	is_hidden_code				; yes, eat-->
	cmp	al,kb_ext_code2				; is it a hidden code prefix e1?
	jne	not_hidden_code				; no, -->

is_hidden_code:

	mov	hidden_code,al	
	jmp	short kybd_int_10			; allow BIOS to deal with any hidden codes we may get


not_hidden_code:

; if the hidden code is not zero, then the previous scan code was either E0/E1 and something, which
; should not exist on these computers, so I will pass them along to the original BIOS and let it
; sort them out.

	mov	ah,hidden_code				; get previous hidden code if any ?
	mov	hidden_code,0				; reset for next key

	cmp	ah,0					; was there a previous hidden code ?
	je	kybd_int_20				; if not, we can cont.----else goto BIOS

; Before we send all E0 /E1 codes to BIOS, ....
; check for right alt. and ctrl. as these older computers shouldn't have these keys, but someone may
; use a 101 key keyboard on tis type of computer as the BIOS just treat the right alt. and ctrl. keys
; as the left alt. and ctrl. keys, as should I

	push	ax
	and	al,not break_bit			; be sure al is the make version of a make/break code
	cmp	al,38h					; an right alt. key ?
	jne	kybd_int_11
	jmp	short kybd_int_18

kybd_int_11:

	cmp	al,1dh					; an right ctrl. key ?
	jne	kybd_int_12
	jmp	short kybd_int_18

kybd_int_12:

	cmp	al,1ch					; an right enter key ?
	jne	kybd_int_15
	jmp	short kybd_int_18

kybd_int_15:
	pop	ax
	jmp	short kybd_int_10			; allow BIOS to deal with any codes

kybd_int_18:
	pop	ax

kybd_int_20:

; Now call Handicap routines.  We do not expect any registers to be
; the same.  So the routine does not need to save any.

;	sti

;IFDEF	BUG
;	mov	portid,0
;	mov	portout,ah
;	call	HexCharsOut
;	mov	portid,1
;	mov	portout,al
;	call	HexCharsOut
;ENDIF;	BUG

	call	FilterKeys				; pass on code in Al

; Disable int. and then re-enable keyboard INTs at the interrupt controller

	cli						; Ints off again. NOTE: don't need
							; to turn them back on because we are
							; going to to an IRET when we finish.

	cmp	fbios_called_direct,true  		; did we pass on to ROM routine?
	je	keybd_int_ret				; yes, so no need to reset it again
							; and no need to re-enable jeyboard as original
							; BIOS routine already did if it was supposed to

	cmp	fFilterKeysOn,true			; is FilterKeys on ???
	jne	keybd_int_end				; if not, this must have been a stickeys eat or something

	cmp	fbios_called_timing,true		; Does the timer want to let a key pass ? (FROM FILTERKEYS ONLY)
	jne	keybd_int_end				; if not, exit w/o out calling old int 9 kybd routine

kybd_int_25:

	cmp	fclick_on,true
	jne	kybd_int_40

	cmp	fshift_click,true
	je	kybd_int_40

	cmp	current_shift,0
	jne	kybd_int_40				; if not a modifier key, call click
	mov	fshift_click,true			; if we clicked once, set, as non-modifier will clear
	call	click

kybd_int_40:

	mov	fbios_called_timing,false		; reset flag
	mov	fkey_not_passed,false			; reset flag

	push	ds
	pushf
	call	dword ptr cs:old_kybd_int
	pop	ds

; any other clean up to do,like reset stickey flags ???

	cmp	fSticKeysOn,true
	jne	kybd_int_50
	cmp	flatch,true
	jne	kybd_int_50

	call	set_shift_states			; clear latched key(s) states
	mov	shift_flg,0				; clear shifted states

kybd_int_50:

	assume	ds:_TEXT
	jmp	keybd_int_ret		

;-----------------------------------------------------------------------


keybd_int_end:

; If we get here, then we did a call to FilterKeys, StickeyKeys,.....and we DID NOT want to
; call the Original BIOS, so we need to reset the kybd hardware ourselves to allow 
; any other key strokes to generate interrupts.


;IFDEF	BUG
;	mov	portid,3
;	mov	portout,al
;	call	HexCharsOut
;ENDIF;	BUG


	in	al,kb_ctl				; reset interface chip (8042)
	mov	ah,al
	or	al,80h
	jmp	$ + 2					; jump delay for fast computers
	out	kb_ctl,al
	xchg	ah,al
	jmp	$ + 2					; jump delay for fast computers
	out	kb_ctl,al

	mov	al,020h					; acknowledge interrupt  (8259)
	jmp	$ + 2					; jump delay for fast computers
	out	ack_port,al

	cmp	_comp_id,5				; do we have an AT class machine
	jne	keybd_int_ret				; if not, cont.

	mov	al,ena_kbd				; if yes, disable keyboard
	call 	ship_it

keybd_int_ret:

;IFDEF	BUG
;	mov	portid,2
;	mov	portout,al
;	call	HexCharsOut
;ENDIF;	BUG

	pop	es

	assume	ES:NOTHING
	pop	bx
	pop	ax					; restore the world
	pop	ds

	assume	DS:NOTHING

	pop	ax					; save value of ax
	cmp	al,1					; which time are we ???
	je	kybd_int_ret_3

	mov	ss,cs:kybd_ss_save
	mov	sp,cs:kybd_sp_save
	mov	cs:fint9_active_temp,0			; clear my flag since I'm out finally

kybd_int_ret_3:

	pop	ax
	iret						; also clears any int. which were set

_keybd_int	endp

;;----------------------------------------------------------------------------
; speed_timer_int 
;
; increments the counts used in the time out and key delay repeat options for setting up
; the beep sounds on the various computers.  This routine is called during initialization only.


speed_timer_int	PROC

	push	ds
	push	cs
	pop	ds
	assume DS:_TEXT

	dec	count_down_ticks	; decrement the count

	pushf
	call	dword ptr cs:old_1C_int	; call original interrupt

	pop	ds
	assume	DS:NOTHING

	iret

speed_timer_int	endp

;----------------------------------------------------------------------------
;****************************************************************************
;----------------------------------------------------------------------------
; Enable_Handicap
;----------------------------------------------------------------------------
;****************************************************************************
;----------------------------------------------------------------------------

Enable	PROC	NEAR

	push	ds					; save registers used
	push	es
	push	si
	push	di
	push	dx
	push	cx
	push	bx
	push	ax

	assume DS:_TEXT

; reset key_data buffer to empty, i.e. point head and tail to the start of the buffer

	mov	bx,OFFSET key_data			; get address of key_data buffer
	mov	key_data_head,bx			; reset head pointer to start of buffer
	mov	key_data_tail,bx			; reset tail pointer to start of buffer

; reset mouse_data buffer to empty, i.e. point head and tail to the start of the buffer

	mov	bx,OFFSET mouse_data			; get address of mouse_data buffer
	mov	mouse_data_head,bx			; reset head pointer to start of buffer
	mov	mouse_data_tail,bx			; reset tail pointer to start of buffer
	mov	mouse_cnt,false				; 

	print	mesg60					; tell user we are loading


ena_2:

;------------------------------------------------------------------------------
; Enable serial port COMM 1/2 at 9600, n, 8, 1 if DEBUG is On
IFDEF	BUG

	cli
;	cmp	_comp_id,8				; am I on the PS2 Model 70 ?
;	je	com_set_20
	mov	dx,0   					; com1
	mov	ah,0					; init com port
	jmp	short com_set_30
;
;com_set_20:
;
;	mov	dx,1   					; com2
;	mov	ah,0					; init com port
;
com_set_30:

	mov	al,0e3h					; 9600 baud, 1 stop bit, no parity, 8 data bits
;	mov	al,043h					; 300 baud, 1 stop bit, no parity, 8 data bits

	int	14h

;	mov	fserial_debugging,true
	mov	al,_vector
	mov	portid,0fh
	mov	portout,al
	call	HexCharsOut
;	mov	fserial_debugging,false

	sti
ENDIF;BUG


;---------------------------------------------------------------------------
; Check if DOS version 3.0 or higher, so we know if we have support for DOS int2f

	mov	ah,30h					; call int21h with al=30h
	int	21h					; major version # in al, minor version # in ah
	cmp	al,3					; if 3 or higher, then int2f supported
	jge	ena_4
IFDEF	BUG
	print 	mesg45					; message that pre 3.0 version of DOS is running
ENDIF;	BUG
	jmp	short ena_10


ena_4:
;IFDEF	BUG
;	print	mesg46					; running DOS 3.0 or higher
;ENDIF;	BUG

;---------------------------------------------------------------------------
; check if AccesDos was started under Windows 3.0 in real, standard or enhanced mode ?

	mov	ax,1600h				; code to ask Windows if running enhanced ?
	int	2fh					; DOS int call
	test	al,7fh					; if al=0 or 80h, then Windows enhanced in NOT running
	jz	ena_5					; if jump, enhanced not running, go check if real or standard ?

	assume	DS:_TEXT

	mov	fwindows,true				; flag set true if we are in Windows
	mov	fwindows_enh,true			; flag that we are in Windows enhanced mode
	mov	fwindows_st_re,false

;IFDEF	BUG
;	print	mesg41					; tell users program started under Windows Enhanced
;ENDIF;	BUG

	jmp	short ena_10				; no need to hook int2f if ADOS started after Windows

ena_5:

	mov	ax,4680h				; code to ask Windows if real or standard
	int	2fh					; DOS int call
	cmp	ax,0					; if ax=0, then real or standard Windows is running
	jne	ena_8					; if not, running Windows, hook int 2fh incase 

	assume	DS:_TEXT							; Windows is started after us

	mov	fwindows,true				; if ax=0, then we are in real or standard mode
	mov	fwindows_st_re,true			; flag that we are in Windows standard or real mode
	mov	fwindows_enh,false


;IFDEF	BUG
;	print	mesg42					; tell user
;ENDIF;	BUG
	jmp	short ena_10				; no need to hook int2f if ADOS started after Windows
ena_8:

;-----------------------------------------------------------------------------
; get INT 2Fh vector and save away
; Need to hook into int2Fh to watch if Windows is loaded after Access Dos
    				
	assume ES:NOTHING
	mov	ax,352fh
	int	21h
	mov	word ptr [old_2f_int_off],bx
	mov	word ptr [old_2f_int_seg],es

;-----------------------------------------------------------------------------
; get INT 2fh vector and save away

	push	ds					; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET _int2f			; hook int. 2fh also
	mov	ax,252fh
	int	21h
	pop	ds

ena_10:

;-----------------------------------------------------------------------------
	cmp	_vector,09				; do we hook int 9 or int 15
	jne	ena_10A

	mov	ax,3509h				; get INT 09h vector and save away
	jmp	short ena_10B

ena_10A:

	mov	ax,3515h				; get INT 15h vector and save away
ena_10B:
	
	int	21h
	mov	word ptr [old_kybd_int_off],bx
	mov	word ptr [old_kybd_int_seg],es

;----------------------------------------------------------------------------
; Setup timer interrupt vector to point to our interrupt routine
; get INT 1Ch vector and save away in old_timer_int this time
; NOTE: Original timer 08h must have been previously restored above.

	assume ES:NOTHING
	mov	ax,351Ch
	int	21h
	mov	word ptr [old_1C_int_off],bx
	mov	word ptr [old_1C_int_seg],es

;----------------------------------------------------------------------------
; Setup timer interrupt vector to point to our speed timer interrupt routine

	push	ds				; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET speed_timer_int
	mov	ax,251Ch
	int	21h				; set the vector
	pop	ds

	xor	ax,ax
	mov	count_down_ticks,2
;
; The way we will do this is to simulate the exact same set of code that
; is our beep routine.  There will be some overhead, but hopefully it will
; not factor into the accuracy that much.  We will keep track of how many
; times we will be able to get through the beep routine before we
; get through 1 tick of the real time timer.

ena11:

	cmp	count_down_ticks,1
	jg	ena11
;
; 46h is the value for a 2000hz square wave running on a PC.  Therefore,
; there would be about 110 cycles within the 18.2 clock (54.95ms).
; If we do 5 cycles, then the count in AX would be 22.  Assume that a fast
; 386 machine running at 25Mhz is about 15 times faster than a PC so AX
; in the upper range would be 330.
;

	cmp	fwindows,true
	jne	ena12			; if Windows isn't running, don't need cli
	cli

ena12:
	mov	cx,46h			; test tone, length of square cycle
	mov	bx,5			; length of test tone

	push	ax			; save ax, count
	IN	al,kb_ctl		; get keyboard control
	push	ax			; save
	push	cx			; save cx=wave length
ena13:	
	and	al,0ffh			; set speaker bits, really don't
	OUT	kb_ctl,al		; send out to speaker
	pop	cx			; get cx=wave length
	push	cx			; save cx
ena14:	
	loop	ena14			; keep on for wave length of time
	or	al,0			; set speaker bit, really don't
	OUT	kb_ctl,al		; send out to speaker
	pop	cx			; get cx
	push	cx			; save cx
ena15:	
	loop	ena15			; keep off for wave length of time
	dec	bx			; decrement length of tone
	jnz	ena13			; loop if not zero

	pop	cx			; restore cx
	pop	ax			; get old keyboard control
	OUT	kb_ctl,al		; send out
	pop	ax			; restore old ax


	sti
;
; Now keep track of count
;

	inc	ax			; increment count

	cmp	count_down_ticks,0	; are we done?
;;	ja	ena12			; no
	jg	ena12			; if count_down_ticks is >= to zero,loop again
					; make this a signed check so if we miss it, FFFF
					; is -1, and we will pass
;--------------------------------------------------------------------------------
; for some unknown reason, if running under Windows, especially enhanced mode, the DOS
; timer doesn't seem to handle the speed timer routine correctly all the time, so we will
; check the "ax" register value here to see if it messed up

	cmp	fwindows,true		; did we find out earlier that Window is running ?
	jne	ena_20
	cmp	ax,80			; Low end 8/10 Mhz Model 50/60 value we might expect for Windows
	jg	ena_20			; if ax from speed loop is higher, don't change
	mov	ax,150			; otherwise override Windows loop to 150 decimal

ena_20:
; for a 4.77 MHz PC, the count in ax should be about 22
; for a 16   MHz PC, the count in ax should be about 165
; for a 25   MHz PC, the count in ax should be about 330

	mov	cx,ax			; save a copy in cx
	mov	ax,46h			; put tone value in ax
	mul	cx			; multiply by number of times through
	mov	bx,22			; normalize to standard pc
	div	bx

	mov	high_tone,ax

	mov	turn_off_start_beep,ax

	shl	ax,1			; multiply by 2 to get 1000hz
	mov	turn_on_start_beep,ax

	shl	ax,1			; multiply by 2 to get 500hz
	mov	low_tone,ax

	mov	ax,turn_on_start_beep		; get 1000hz back
	sub	ax,turn_off_start_beep		; get difference

	mov	bx,100				; 100 steps
	xor	dx,dx
	div	bx

	mov	on_off_change,ax

	cmp	ax,00				; did on_off_change crank out to be ZERO ??
	jne	ena25				; if not, things should be okay, so cont

	mov	on_off_change,1			; if it was ZERO, very slow computer, so override prev. calc.

ena25:

;-----------------------------------------------------------------------------
	cmp	fmouse_driver,false
	je	ena_100				; if we didn't find a mouse, do not need to hook into any of these

;-----------------------------------------------------------------------------
; get INT 33h vector and save away
; Need to hook into int33h for either serial or PS/2 mouse

	assume ES:NOTHING
	mov	ax,3533h
	int	21h
	mov	word ptr [old_33_int_off],bx
	mov	word ptr [old_33_int_seg],es

;-----------------------------------------------------------------------------
; PS/2 MOUSE
;
; If we get here, we found a PS/2 mouse, so we need to check which computer we are on

	assume ES:NOTHING

	cmp	_comp_id,6			; Int 71h is for computer id=6, 25..30/86
	jne	ena_70				; if not equal, jump to other PS/2 mouse int

	mov	ax,3571h			; get INT 71h vector and save away
	jmp	short ena_75
ena_70:
						; If we get here, then we should have computer id = 7 or 8
	mov	ax,3574h			; get INT 74h vector and save away
ena_75:

	int	21h
	mov	word ptr [old_ps2mou_int_off],bx
	mov	word ptr [old_ps2mou_int_seg],es

ena_100:

;-------------------------------------------------------------------------------------
; Setup kybd interrupt vector to point to our interrupt routine

	cmp	_vector,09h				; do we hook int9 or int 15 ?
	jne	ena_102

	push	ds					; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET _keybd_int			; hook into int. 09h, keyboard
	mov	ax,2509h
	int	21h					; set the vector
	pop	ds
	jmp	short ena_103

ena_102:
	push	ds					; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET _keybd_int_15			; hook into int. 15h of the keyboard interrupt
	mov	ax,2515h
	int	21h					; set the vector
	pop	ds

ena_103:

; if not on during a call to Enable, serialkeys will not be loaded, so we don'y have to check the other flags

	cmp	_serialKeysOn,true			; is SerialKeys code on ?
	jne	ena_104

	push	ds
;	call	_serialKeysInit     			; initialize serial keys
	call	_serialKeysEnableFar
	pop	ds


ena_104:
;-------------------------------------------------------------------------------------
; Setup 1C timer interrupt vector to point to our interrupt routine

	push	ds					; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET _new_timer_int		; hook into int. 1Ch, timer
	mov	ax,251Ch
	int	21h					; set the vector
	pop	ds

;---------------------------------------------------------------------------------------

	cmp	fmouse_driver,false			; did we load a mouse driver ???
	je	ena_200					; if not, quit

;---------------------------------------------------------------------------------------
; Setup interrupt 33h vector to point to our interrupt routine. we need int33h for either serial or PS/2 mouse

	push	ds					; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET _int33			; hook int. 33h also
	mov	ax,2533h
	int	21h
	pop	ds

;-----------------------------------------------------------------------------
; get INT 71h vector and save away
; If we get here, we found a PS/2 mouse, so we need to check which computer we are on

	cmp	_comp_id,6				; Int 71h is for computer id=6, 25..30/86
	jne	ena_140					; if not equal, jump to other PS/2 int

	push	ds					; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET _intps2mou			; hook int. 71h also
	mov	ax,2571h
	int	21h
	pop	ds
	jmp	ena_200				

ena_140:
;-----------------------------------------------------------------------------
; get INT 74h vector and save away
; If we get here, then we should have computer id = 7 or 8

	push	ds					; SAVE DS for restore
	mov	ax,cs
	mov	ds,ax
	mov	dx,OFFSET _intps2mou			; hook int. 74h also
	mov	ax,2574h
	int	21h
	pop	ds

ena_200:

	pop	ax
	pop	bx					; restore registers
	pop	cx
	pop	dx
	pop	di
	pop	si
	pop	es
	pop	ds

	ret	


Enable	endp

_TEXT	ends

	end



