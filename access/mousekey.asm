;************************************************************************
;									*
;	Copyright (C) 1991 by Trace Center (just kidding)		*
;									*
;	MOUSEKEY.ASM							*
;									*
;************************************************************************

TITLE	MouseKeys

INCLUDE	keyboard.inc

	EXTRN 	real_states:byte
	EXTRN	shift_flg:byte
	EXTRN	lock_flg:byte

	EXTRN	ToggleKeys:PROC				; in ToggleKeys.asm


	EXTRN	beep_high:PROC				; in Handicap.asm
	EXTRN	beep_low:PROC
	EXTRN	no_beep:PROC
	EXTRN	beep_turn_on:PROC
	EXTRN	beep_turn_off:PROC
	EXTRN	click:PROC
	EXTRN	fmouse_driver:byte
	EXTRN	_fmouse_id:byte
	EXTRN	btn_1:byte
	EXTRN	btn_2:byte
	EXTRN	Current_Button:byte
	EXTRN	fmouse_button:byte
	EXTRN	_comp_id:byte
	EXTRN	comp_flag:byte
	EXTRN	Get_Mouse_Data:PROC
	EXTRN	ExtendedSeg:word
	EXTRN	fpause_being_sent:byte
	EXTRN	fswitching_video:byte
	EXTRN	faccess_sound:byte
	EXTRN	fbios_called_timing:byte
	EXTRN	fsecond_cue:byte
	EXTRN	Put_Key_Data:PROC
	EXTRN	ftimer_1C_active:byte
	EXTRN	fserial_key_recieved:byte
	EXTRN	fmousetrapping:byte
;	EXTRN	fwindows_enh:byte			; DEBUGGING

	EXTRN	fMouseKeysOn:byte			; from Param.asm
	EXTRN	fMK_On_Off_Feedback:byte
	EXTRN	Max_Speed:word
	EXTRN	Time_To_Max_Speed:word
	EXTRN	fDialog_Mouse_off:byte
	EXTRN	fRecovery_On:byte
	EXTRN	fspace_saver:byte
	EXTRN	fToggleKeysOn:byte			
	EXTRN 	fSticKeysOn:byte

	EXTRN	TimeOut_Reset:PROC			; in TimeOut.asm

	EXTRN	fUser_SetUp_Option1:byte		; in FilterKeys.asm
	EXTRN	fUser_SetUp_Option2:byte
	EXTRN	on_repeat_ticks:word

	PUBLIC	MouseKeys				; in MouseKeys.asm
	PUBLIC	MouseKeys_timer
	PUBLIC	MouseKeys_TurnOff
	PUBLIC	InjectMouse
	PUBLIC	button_click,Status
	PUBLIC	fMoving,fbutton_up,Last_Direction,fbutton_down
	PUBLIC	Delta_X,Delta_Y,Mouse_Status
	PUBLIC	mouse_data
	PUBLIC	mouse_data_head
	PUBLIC	mouse_data_tail
	PUBLIC	mouse_data_end
	PUBLIC	Put_Mouse_Data
	PUBLIC	MouseKeys_dialog
	PUBLIC	Button_Status
	PUBLIC	mouse_cnt 
	PUBLIC	fnum_lock_was_off
	PUBLIC	fserial_stop

IFDEF	BUG

	EXTRN	portid:byte
	EXTRN	portout:byte
	EXTRN	HexCharsOut:PROC

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

fMoving			DB	false			; flag for accelerated Moving
fSteady			DB	false			; flag for if at constant speed
fJust_Manually_On	DB	false			; flag to prevent turn off right after turn on
fJust_Manually_Off	DB	false			; flag to prevent turn on right after turn off
fbutton			DB	false			; flag to indicate a mousekey mouse button was pressed
fbutton_up		DB	false			; flag to tell code after the mouse driver that MouseKeys button up was sent
fbutton_down		DB	false			; flag to tell code that mouse button down has been pressed once already
foff_mousekeys		DB	false			; flag that MouseKeys is turning off after we get numlock break code
fpass_mousekeys		DB	true			; flag that turns numeric pad on/off to mousekeys with numlock key only
fnum_lock_was_off	db	false			; flag used with space saver kybd, should on num lock be passed or not ?
fpass_numlock_spsav	db	false			; flag used with space saver kybd to block typematic numlock makes from
fserial_stop		db	false			; flag used to tell timer when to stop sending serial mouse mvmnts to driver

							; toggling MouseKeys on/off/on/off/on.......

Accel_Ptr		DB	0			; pointer into table
Constant_Ptr		DB	0

Last_Direction	label	word

Y_Direction		DB	0			; direction
X_Direction		DB	0			; direction

Status			DB	0			; byte to hold the mouse status as built
Button_Status		DB	0			; holds button_status info for the mouse
Temp_Button		DB	0			; holds al register while I check if kybd supports 3rd button

Mouse_Packet	label	word				; next four words

Delta_Z			DW	0			; added for our matching in AccesDos
Delta_Y			DW	0
Delta_X			DW	0
Mouse_Status		DW	0			; only changes in states
mouse_cnt		DB	0			; mouse data packet buffer counter

mouse_data_tail		DW	?			; pointer into mouse_data
mouse_data_head		DW	?			; pointer into mouse_data
mouse_data		DW	120 DUP (?)		; array of (120 words) for circular buffer of mouse button data
mouse_data_end	label	word

;----------------------------------------------------------------------------
; MouseKeys_Param

Accel_Table_Len		DB	73			; just storage initially
							; 73 bytes gives alittle more 4 seconds maximum if choosen
							; to complete the accel. table ( 73 tics / 18.2 ticks/sec)
 
Accel_Table	db	 0, 0, 1, 0, 0, 1, 0, 1
		db	 0, 1, 0, 1, 0, 1, 0, 1
		db	 0, 1, 0, 1, 0, 1, 0, 1
		db	 1, 0, 1, 0, 1, 0, 1, 0

		db	 1, 0, 1, 0, 1, 0, 1, 1
		db	 0, 1, 0, 1, 0, 1, 0, 1
		db	 0, 1, 0, 1, 1, 1, 1, 1
		db	 1, 1, 1, 1, 1, 1, 1, 1
		db	 1, 1, 1, 1, 1, 1, 1, 1
		db	 1 


Constant_Table_Len	DB	21			; just storage

Constant_Table	db	 1, 1, 1, 1, 1, 1, 1, 1
		db	 1, 1, 1, 1, 1, 1, 1, 1
		db	 1, 1, 1, 1, 1 
		
;----------------------------------------------------------------------------
; search table for a shift key.

mouse_tbl	label	 word

		dw	NumLock
		dw	NumPad_Divide
		dw	NumPad_Times
		dw	NumPad_Minus
		dw	NumPad_Plus
		dw	NumPad_Enter	
		dw	NumPad_Period	
		dw	NumPad_9	
		dw	NumPad_8	
		dw	NumPad_7	
		dw	NumPad_6	
		dw	NumPad_5	
		dw	NumPad_4	
		dw	NumPad_3	
		dw	NumPad_2	
		dw	NumPad_1	
		dw	NumPad_0	
mouse_tbl_len	equ	17

MK_call_table	label	word

		dw	button_delta			; button down
		dw	move_proc	
		dw	move_proc	
		dw	move_proc	
		dw	move_proc	
		dw	button_click			; button click
		dw	move_proc	
		dw	move_proc	
		dw	move_proc	
		dw	move_proc	
		dw	button_click  			; button up, changed to accomadate MouseKeys and real mouse together
		dw	dummy_proc	
		dw	dbl_click_proc			; double click
		dw	btn_sel_proc	
		dw	btn_sel_proc	
		dw	btn_sel_proc	
		dw	off_proc	

MK_data_table	label	word

		dw	1				; numpad 0 button down
		dw	0ffffh				; numpad 1 -x,-y
		dw	000ffh				; numpad 2  0,-y
		dw	001ffh				; numpad 3 +x,-y
		dw	0ff00h				; numpad 4 -x, 0
		dw	0				; numpad +, click
		dw	00100h				; numpad 6 +x, 0
		dw	0ff01h				; numpad 7 -x,+y
		dw	00001h		  		; numpad 8  0,+y
		dw	00101h				; numpad 9 +x,+y
		dw	1				; numpad . button up
		dw	0				; numpad   "Enter"
		dw	0				; numpad + double click
		dw	01002h				; high=10,serial right button, low=2 AUX right button		
		dw	03003h				; high=30,serial both buttons, low=3 AUX both buttons
		dw	02001h				; high=20,serial left button, low=1 AUX left button		
		dw	0				; numpad "Num Lock" key for On/Off

;----------------------------------------------------------------------------
;
; The mouseKeys routine allows everyone who has difficulty controlling a standard mouse input
; device, the ability to alternately control the standard mouse from the keyboard.  Mousekeys
; remaps the numeric keypad into a mouse control pad, with all the same functionality of the 
; standard input mouse available to the user from the keyboard.
;----------------------------------------------------------------------------

MouseKeys	proc	

	assume	DS:_TEXT

	jmp	mouse_begin


;----------------------------------------------------------------------------
;	Put_Mouse_Data
; 
; This procedure loads mouse button info into a buffer where the timer will later
; retrieve it and send it to the appropriate mouse input mechanism.  All serial
; mouse info ends up in this buffer, but only the ps/2 mouse button info needs 
; to be buffered, the rest of the PS/2 mouse information can be injected directly.
;
;	Expects   Ax= mouse data to be stored, order is
;
;		  1st Mouse_Status
;		  2nd Delta_X
;		  3rd Delta_Y

Put_Mouse_Data	proc

	assume	DS:_TEXT

	push	bx				; temp store of bx register
	push	si				; temp store of si register

	mov	bx,mouse_data_tail		; get tail pointer of mouse_data buffer
	mov	si,bx				; save pointer value
	add	bx,2				; move to next word address in buffer
	cmp	bx,OFFSET mouse_data_end	; are we at the end of the buffer ?
	jne	PMD_5				; no
	mov	bx,OFFSET mouse_data		; yes we are, so reset to the buffer beginning

PMD_5:
	cmp	bx,mouse_data_head		; has the buffer wrapped around ?
	jne	PMD_10

; if equal, buffer is full, exit

	jmp	Put_Mouse_Data_End		; if full, leave routine
PMD_10:
	mov	[si],ax				; move whats in ax into address pointed to by si
	inc	mouse_cnt			; increase counter by 1, since we put a word into the buffer

	mov	mouse_data_tail,bx		; update tail pointer

Put_Mouse_Data_End:

	pop	si
	pop	bx
	ret

Put_Mouse_Data	endp

;----------------------------------------------------------------------------
; MouseKeys_timer
;
; Expects:	Nothing
;
; Changes:	ax,bx

MouseKeys_timer	proc	

	assume	DS:_TEXT

	cmp	fMouseKeysOn,true		; is mouse key on?
	jne	mt_10				; no, skip routine -->
	cmp	fMoving,true			; are we in motion?
	je	mt_20				; yes -->
mt_10:
	jmp	MouseKeys_timer_end		; don't do anything
mt_20:
	xor	ax,ax				; clear motion value
	xor	bx,bx				; clear pointer
	cmp	fSteady,false			; are we in a steady motion?
	jne	mt_40				; yes -->
	mov	bl,Accel_Ptr			; motion is accelerating

	cmp	Time_To_Max_Speed,1		; do we accel. for 1 sec. ?
	jne	mt_22				; not a 1
	mov	al,18				; 1 sec <=> 18 ticks
	mov	Accel_Table_Len,al
	jmp	mt_28

mt_22:
	cmp	Time_To_Max_Speed,2		; do we accel. for 2 sec. ?
	jne	mt_24
	mov	al,36				; 2 sec <=> 36 ticks
	mov	Accel_Table_Len,al
	jmp	mt_28

mt_24:
	cmp	Time_To_Max_Speed,3		; do we accel. for 3 sec. ?
	jne	mt_26
	mov	al,54				; 3 sec <=> 54 ticks
	mov	Accel_Table_Len,al
	jmp	mt_28

mt_26:
	mov	al,73				; 4 sec <=> 73 ticks Default back to
	mov	Accel_Table_Len,al

mt_28:

	cmp	bl,Accel_Table_Len		; are we at end of accel?
	jb	mt_30				; no -->
	mov	fSteady,true			; yes, start using constant table
	xor	bx,bx
	jmp	short mt_50			; -->
mt_30:
	mov	al,Accel_Table[bx]
	cbw
	inc	bx				; increment pointer
	mov	Accel_Ptr,bl
	jmp	short mt_60

mt_40:
	mov	bl,Constant_Ptr			; yes, get next movement
	cmp	bl,Constant_Table_Len		; are we at end of table?
	jb	mt_50				; no -->
	xor	bx,bx				; yes, reset to beginning
mt_50:
	mov	al,Constant_Table[bx]
	cbw
	inc	bx				; increment pointer
	mov	Constant_Ptr,bl
mt_60:
	or	ax,ax				; motion on this interrupt?, based on "..."table value
	jnz	mt_62				; we have a non zero, so cont. to do motion
	jmp	MouseKeys_timer_end		; no motion this time,end

mt_62:
	cmp	fSteady,false			; catches here and below (x and y direction)
	jne	mt_65
	cmp	Max_Speed,8			; don't adjust speed unless above Max_Speed 8
	jle	mt_65
	mov	ax,Max_Speed
	cmp	ax,36				; if Max_Sped is 36, shr twice
	jne	mt_64

	shr	ax,1		
mt_64:
	shr	ax,1		
;------------------
; acceleration speed adjustments

	xor	bx,bx				; clear x direction
	cmp	X_Direction,bl			; is direction 0?
	je	mt_70A				; yes -->
	mov	bx,ax				; get current acceleration variable, doesn't change flag bits
	jg	mt_70A				; is it positive? yes --> from above compare YET !!!
	neg	bx			

mt_70A:
	xor	cx,cx				; clear y direction
	cmp	Y_Direction,cl			; is direction 0?
	je	mt_80				; yes -->
	mov	cx,ax				; get accel. variable***
	jg	mt_80				; is it positive?, yes -->from above compare YET !!!
	neg	cx
	jmp	short mt_80
;------------------
; standard acceleration and constant speed

mt_65:
	xor	bx,bx				; clear x direction
	cmp	X_Direction,bl			; is direction 0?
	je	mt_70				; yes -->
	mov	bx,Max_Speed			; get current acceleration variable, doesn't change flag bits
	jg	mt_70				; is it positive? yes --> from above compare YET !!!
	neg	bx			

mt_70:
	xor	cx,cx				; clear y direction
	cmp	Y_Direction,cl			; is direction 0?
	je	mt_80				; yes -->
	mov	cx,Max_Speed			; get accel. variable***
	jg	mt_80				; is it positive?, yes -->from above compare YET !!!
	neg	cx

mt_80:
	cmp	_fmouse_id,4			; is the PS/2 mouse driver loaded
	jne	mt_90

	mov	Delta_Y,cx			; preload Delta_X before calling Inject Mouse
	mov	Delta_X,bx			; preload Delta_Y before calling Inject Mouse

	xor	ax,ax
	mov	al,Status			; now have direction status in al register
	or	al,Button_Status		; get button status info. into al register
	mov	Mouse_Status,ax

	call	InjectMouse
	jmp	short MouseKeys_timer_end

mt_90:
; cx register has Delta_y, and bx register has Delta_X data

	cmp	_fmouse_id,2	 		; do we have a serial mouse ?
	jne	MouseKeys_timer_end

	cmp	cl,00h				; if y is zero, leave alone
	je	mt_95

	test	cl,80h				; if y is neg, for serial mouse must switch
	jnz	mt_92
	or	Status,0Ch			; build Status for neg. Y dir.

mt_92:
	neg	cl				; 2's compl. Y, 

mt_95:

	mov	Delta_Y,cx			; load Delta_Y
	and	Delta_Y,03fh			; flag not the first byte

	mov	Delta_X,bx			; load Delta_X 

	test	bl,80h				; is x neg?
	jz	mt_97
	or	Status,03h			; build Status for neg X dir.

mt_97:
	and 	Delta_X,03fh			; flag not the first byte
	xor	ax,ax				; make sure ax is 0
      	mov	al,Status
	or	al,Button_Status
	or	al,40h		  		; need to always set this bit for serial mouse button byte


	mov	Mouse_Status,ax	

	cmp	mouse_cnt,100			; are there 100 nouse data wors in buffer already ?
	jge	MouseKeys_timer_end

	call	Put_Mouse_Data			; store info on serial mouse in buffer
	mov	ax,Delta_X
	call	Put_Mouse_Data			; store info on serial mouse in buffer
	mov	ax,Delta_Y
	call	Put_Mouse_Data			; store info on serial mouse in buffer

	mov	fmouse_button,true     		; is a mousekeys mouse button data

MouseKeys_timer_end:
	ret
MouseKeys_timer	endp
;------------------------------------------------------------------------
; the following routines provide the sound feedback.

mk_turn_on_fb	proc	
	assume	DS:_TEXT

	cmp	fMK_On_Off_Feedback,false
	je	MouseKeys_TurnOn
	call	beep_turn_on
MouseKeys_TurnOn:
	cmp	fswitching_video,false
	jne	MouseKeys_TurnOn_3
	mov	faccess_sound,true
	mov	fsecond_cue,24

MouseKeys_TurnOn_3:

	mov	fJust_Manually_On,true

MouseKeys_TurnOn_5:

	mov	fpass_numlock_spsav,false	
	mov	fpass_mousekeys,true			; trap numeric pad keys as MouseKeys

	call	stop_moving
	mov	fbutton,false				; reset mousekey mouse button to false
	mov	fbutton_up,false
	mov	fbutton_down,false
	mov	fmouse_button,false


; reset mouse_data buffer to empty, i.e. point head and tail to the start of the buffer

	push	bx					; temp save of bx register
	mov	bx,OFFSET mouse_data			; get address of mouse_data buffer
	mov	mouse_data_head,bx			; reset head pointer to start of buffer
	mov	mouse_data_tail,bx			; reset tail pointer to start of buffer
	mov	mouse_cnt,false				; 
	mov	fserial_stop,false	
	pop	bx

	call	TimeOut_Reset
	mov	fMouseKeysOn,true
	cmp	_fmouse_id,4				; do we have a PS/2 mouse ?
	jne	mk_turn_on_5
	mov	Current_Button,01h
	mov	Temp_Button,false
	jmp	short mk_turn_on_10
mk_turn_on_5:
	cmp	_fmouse_id,2				; do we have a serial mouse ?
	jne	short mk_turn_on_10
	mov	Current_Button,20h
mk_turn_on_10:
	mov	Button_Status,0				; buttons default up at turn on

	ret

mk_turn_on_fb	endp

;----------------------------------------------------------------------------

mk_turn_off_fb	proc	

	assume	DS:_TEXT

	cmp	fMK_On_Off_Feedback,false
	je	MouseKeys_TurnOff
	call	beep_turn_off

MouseKeys_TurnOff:

	cmp	fswitching_video,false
	jne	MouseKeys_TurnOff_5
	mov	faccess_sound,true
	mov	fsecond_cue,25

MouseKeys_TurnOff_5:

	mov	fJust_Manually_Off,true			; flag that we just turned off
	mov	fserial_stop,false	
	call	stop_moving

	cmp	Button_Status,0				; check to see if MouseButton's are up?
	je	MKTOff_20				; yes, ignore and jump around resetting of button code				       

	push	ax					; temp save
	push	cx					; temp save
	mov	cx,0053h				; button up key code
	mov	ax,1					; same as if called from table for a button up
	mov	fbutton_up,true				; ""
	mov	fbutton_down,false			; ""
	call	button_click				; 
	pop	cx					; restore register
	pop	ax					; restore register

MKTOff_20:

	mov	fMouseKeysOn,false			; no, then turn off
	mov	fnum_lock_was_off,false			; reset to false
	ret

mk_turn_off_fb	endp
;----------------------------------------------------------------------------
; MouseKeys_dialog 
;
;	Checks a single flag from Dialog box to see if MouseKeys was turned
; on/off.  If it was, this routine makes a call to MouseKeys_TurnOn/Off so the realstates
; can be updated to match the computer.  Upon exit the flag that was set by 
; Dialog box is cleared.(returned to false)


MouseKeys_dialog	proc	

	assume	DS:_TEXT

	cmp	fMouseKeysOn,true			; was Stickeys turned on
	jne	Md_25
	cmp	fmouse_driver,false			; if fmouse_driver false, don't turn mousekeys on
	je	Md_50					; no, just pass on -->
	call	MouseKeys_TurnOn_5     			; yes it was true, so turn on
	cmp	fspace_saver,true
	jne	Md_50
	mov	fpass_mousekeys,false			; start space_saver kybd in off position if turned on via menu
	jmp	short Md_50

Md_25:
	call	MouseKeys_TurnOff_5			; fMouseKeyOn was false, so turn off
Md_50:
	mov	fDialog_Mouse_off,false			; reset flag to false
	ret

MouseKeys_dialog	endp

;----------------------------------------------------------------------------
;     INJECT MOUSE
;
;expects:  
;	AX Mouse Status Word
;	BX X Data Word
;	CX Y Data Word
;	DX Z Data Word
;
;returns: nothing
;
;calls:	Pointing Device Driver at segment [40:0E], offset [22h]
;	Int 74h (80286 or higher CPUs) or 71h (8086 CPUs)
;
;----------------------------------------------------------------------------

InjectMouse	proc

	assume	DS:_TEXT

;-DEBUGGING---------------------------------------------------------------------
;
;;;	cmp	fwindows_enh,true
;;;	jne	IM_10
;;;	call	click	       
;	push	ax
;	mov	ax,Mouse_Status
;	call	Put_Mouse_Data	 
;	mov	ax,Delta_X
;	call	Put_Mouse_Data	 
;	mov	ax,Delta_Y
;	call	Put_Mouse_Data	 
;	pop	ax
;	jmp	IM_150		; ADDED LABEL
;----------------------------------------------------------------------
;
;IM_10:

	cli					; disable int

	push 	es
	push	ax
	push	bx
	push	cx
	push	dx

	xor	ax,ax				; make sure the registers are zero
	xor	bx,bx
	xor	cx,cx
	xor 	dx,dx

	mov	fmouse_button,true     		; is a mousekeys mouse button data

	mov	ax,Mouse_Status
	push 	ax			 	;first status goes on stack

	mov	bx,Delta_X
	push 	bx			 	;now X data

       	mov	cx,Delta_Y
	push 	cx			 	;now Y

       	mov	dx,Delta_Z
	push 	dx			 	;now Z, always equal to 0


;now call device driver pointer routine to store values in its data area

	assume ES:NOTHING

	mov 	ax,ExtendedSeg      		;get extended BIOS segment
	mov	es,ax

	call dword ptr es:[22h]			;call user pointer routine

; Since the device driver does most of its processing in the hardware interrupt routine rather than the pointer 
; routine, we have to call it.  This is done by simulating an interrupt.  The device driver calls the 
; normal bios interrupt routine, checks to see if it got any data, and then processes the data.  We stuffed data 
; in already, (i.e. simulated the normal bios interrupt routine).  When the normal bios interrupt routine is 
; called, it will see no data at the hardware port so it should just return.  We assume this will always happen 
; but we may have to disable the mouse to make sure it will always happen.  Also, since no data was found, the
; normal bios interrupt routine will not call the pointer routine. This is great since we called the pointer 
; routine with our data. The device driver will now process our data.
;
; Now call interrupt routine to process the new data

	cmp	_comp_id,8			; which computer model do we have ?
	jne	IM_40
	int 74h					; works for high end PS/2 Models (55SX,70,80)
	jmp	IM_100

IM_40:
	cmp	_comp_id,7			; which computer model do we have ?
	jne	IM_50
	int 74h					; works for high end PS/2 Models ("25/286","30/286",50,60)
	jmp	IM_100

IM_50:
	cmp	_comp_id,6
	jne	IM_120
	int	71h				; works for low end PS/2 Models (25,30,.../86)


IM_100:
	add sp,+8				; get our stuff off stack after the interrupt

IM_120:
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	pop 	es

	sti					; re-enable int.'s
;;IM_150:

	ret

InjectMouse endp

;----------------------------------------------------------------------------
; stops smooth motion moving of the PS/2 and Serial mouse

stop_moving	proc	

	assume	DS:_TEXT

	mov	Last_Direction,0
	mov	fMoving,false
	mov	fSteady,false
	mov	Accel_Ptr,0
	mov	Delta_X,0
	mov	Delta_Y,0
	mov	Status,0

; don't reset mouse_data buffer on a mouse button (i.e. button down/up, click, dblclick)
	cmp	fbutton,true
	je	stop_moving_end

; don't reset mouse data buffer unless it's a serial mouse
	cmp	_fmouse_id,2
	jne	stop_moving_end

	mov	fserial_stop,true			; flag that we want to stop sending serial mouse novmnts

;	push	bx					; temp save of bx register
;	mov	bx,OFFSET mouse_data			; get address of mouse_data buffer
;	mov	mouse_data_head,bx			; reset head pointer to start of buffer
;	mov	mouse_data_tail,bx			; reset tail pointer to start of buffer
;	mov	mouse_cnt,false				; 
;	pop	bx

stop_moving_end:

	ret
stop_moving	endp

;----------------------------------------------------------------------------
; dummy_proc
;
; Expects:	Nothing

dummy_proc	proc	

	assume	DS:_TEXT

; When this procedure is called, the actual scan code still resides in "cx" register
; so if we mov "cx" back to "ax" and then call ToggleKeys, then everything is back to normal.

	mov	ax,cx					; restore scan code to "ax" register
	mov	fbutton,false				; move mousekey mouse button to false

	call	ToggleKeys				; pass ENTER key on, actually the "E0" part of scan code
							; was passed by handicap.asm

	cmp	ftimer_1C_active,true			; if space_saver kybd, and we called ToggleKeys, 
							; must undo previous clearing of fbios_called_timing if fFilterKeys is On
	jne	dummy_proc_end

	mov	fbios_called_timing,true		; reset flag so int. 15 will pass the key and exit

dummy_proc_end:

	ret
dummy_proc	endp

;----------------------------------------------------------------------------
; off_proc
;
; Expects:	cx = extended scan code
;
; This routine only gets the break codes of numlock when turned on, the make/break
; code of the numlock if the operator wants to toggle MouseKeys on/off, or the make num lock
; scan code when the operator wants to turn off MouseKeys.  It is set up for laptops especially,
; and all other computers that toggling the numlock key by itself whilc MouseKeys is on, will
; toggle the Numeric KeyPad On/Off (i.e. MouseKeys will toggle On/Off but not tottaly off)
; which requires three key left shift left alt and numlock keys

off_proc	proc	

	assume	DS:_TEXT

	cmp	fJust_Manually_On,true			; is this the break of turn On numlock ?
	jne	off_25

	test	cl,break_bit				; is it a make numlock, if so pass
	jnz	off_10					; wait for numlock break to turn on
       	jmp	off_112					; typematic makes of num lock turn on, eat

off_10:
       	cmp	fspace_saver,true			; if space saver keyboard, must pass on break
	jne	off_20 					; if we passed on the num lock make originally

	cmp	fSticKeysOn,true			; is StickeyKeys On ???
	jne	off_20

	cmp	fnum_lock_was_off,true			; see if we originally passed num lock make code ?
	jne	off_20					; if not, do not pass on the break as well
	jmp	off_107
						
off_20:
	jmp	off_110					; if not, exit at 110 by not passing key on

off_25:

; if gets here, it could be numlock make/break and we don't know if the user waants to turn MouseKeys Off
; unless we check if the left shift and the left alt keys are depressed
; NOTE: If StickeyKeys is On, the modifier flags will only be set on the make of the num lock key.

	cmp	dh,fLshift + fAlt			; are the left shift and alt down?
	jne	off_100					; no, so just a numlock make by itself, pass on
							; if yes, left alt + left shift + numlock so shut off
	call	mk_turn_off_fb	       	
	mov	fbutton,false				; move mousekey mouse button to false
	mov	fpass_numlock_spsav,false		; clear flag
	jmp	off_110					; jump to here so we eat the numlock make code
		
off_100:
; if gets here, it is just a numlock make/break without any modifier keys

	test	cl,break_bit				; is it a make numlock, if yes, change status
	jz	off_101
	jmp	off_106       			 	; it was a break, pass on for all KYBDS 
 
off_101:
	cmp   	fpass_numlock_spsav,true		; flag to stop typematic on/off/on/off... of MouseKeys
	jne	off_102
	jmp	off_110

off_102:
	mov	fpass_numlock_spsav,true

	cmp	fpass_mousekeys,true			; are MouseKeys currently being trapped ???
	je	off_105

	mov	fpass_mousekeys,true			; trap numeric keys (i.e MouseKeys is On)
       	cmp	fspace_saver,true			; it was a make, do we have the space saver ?
	jne	off_103D
	cmp	fSticKeysOn,true			; is StickeyKeys On ???
	jne	off_103D				; if StickeyKeys isn't On, don't mess with Num Lock


off_103A:
	cmp	fToggleKeysOn,true
	jne	off_103D
	jmp	off_107					; passing num lock on by space saver will do beeps

off_103D:
	cmp	fMK_On_Off_Feedback,false
	je	off_103E
	call	beep_high				; beep high if toggle Mousekeys back on
off_103E:
	cmp	fswitching_video,false
	jne	off_104
	mov	faccess_sound,true
	mov	fsecond_cue,7
off_104:
       	cmp	fspace_saver,true			; is it space saver ?
	je	off_107
	jmp	short off_110

;--------------------------------------------
off_105:
	mov	fpass_mousekeys,false			; untrap or pass numeric keys (i.e MouseKeys is Off)
       	cmp	fspace_saver,true			; it was a make, do we have the space saver ?
	jne	off_105C
	cmp	fSticKeysOn,true			; is StickeyKeys On ???
	jne	off_105C				; if StickeyKeys isn't On, don't mess with Num Lock

off_105A:

	cmp	fToggleKeysOn,true
	jne	off_105C
	jmp	short off_107				; passing num lock on by space saver will do beeps

off_105C:
	cmp	fMK_On_Off_Feedback,false
	je	off_105D
	call	beep_low				; beep low if toggle Mousekeys back off
off_105D:

	cmp	fswitching_video,false
	jne	off_105E
	mov	faccess_sound,true
	mov	fsecond_cue,9

off_105E:

; if we toggle off, we must be sure that mousekeys mouse buttons are released

	push	ax					; temp save
	push	cx					; temp save
	mov	cx,0053h				; button up key code
	mov	ax,1					; same as if called from table for a button up
	mov	fbutton_up,true				; ""
	mov	fbutton_down,false			; ""
	call	button_click	   			; ""
	pop	cx					; restore register
	pop	ax					; restore register
       	cmp	fspace_saver,true			; it was a make, do we have the space saver ?
	je	off_107
	jmp	short off_110

off_106:
	mov	fpass_numlock_spsav,false		; we got a break of the num lock, clear flag

off_107:
	cmp	fSticKeysOn,true			; is StickeyKeys On ???
	jne	off_110					; if StickeyKeys isn't On, don't mess with Num Lock

	mov	ax,cx					; ToggleKeys expects scan code in ax register
	call	ToggleKeys				; pass num lock key on, regardless if Mkeys On/Off
	cmp	ftimer_1C_active,true			; if space_saver kybd, and we called ToggleKeys, 
							; must undo previous clearing of fbios_called_timing if fFilterKeys is On
	jne	off_110
	mov	fbios_called_timing,true		; reset flag so int. 15 will pass the key and exit

off_110:
	mov	fnum_lock_was_off,false
	mov	fJust_Manually_On,false
off_112:	
	ret
off_proc	endp

;----------------------------------------------------------------------------
; btn_sel_proc
;
; Expects:	ax = button value, where ah=button if serial mouse and al=button if AUX. port mouse
;
;		Must check here for which type of keyboard we have.  If we have the 101/102
;		key beyboard, then we can emulate a 3 button mouse because we have "/,*, and-"
;		on the numeric keypad.  If we do not have the 101/102 keyboard, then we 
;		probably only have the 84 key keyboard, which only has the "* and -" key,
;		so we can only emulate a 2 button mouse.  If ax="btn_1 + btn_2", or "03h",(AUX) mouse,
;		then, we must check to keyboard type and either leave ax alone for the 
;		101/102 key keyboard, or reset to only "btn_1" for an 84 key keyboard.



btn_sel_proc	proc	

	assume	DS:_TEXT

	push	ax

	mov	fbutton,false				; move mousekey mouse button to false
	cmp	_fmouse_id,4				; do we have a AUX. port PS/2 mouse ?
	jne	btn_sel_10

	mov	Temp_Button,al				; set to which ever button(s)
	cmp	Temp_Button,03h				; is it a "btn_1 + btn_2" ?
	je	btn_sel_20				; yes, so check if we support it

	mov	Current_Button,al			; no, select what ever button, and quit
	jmp	short btn_sel_60     			; process both buttons pushed together


btn_sel_10:
	cmp	_fmouse_id,2				; do we have a serial port mouse ?
	jne	btn_sel_50				; no, somethings wrong !!! this should never execute !

	mov	Temp_Button,ah				; set to which ever button(s)
	cmp	Temp_Button,030h			; is it a "btn_1 + btn_2" ?
	je	btn_sel_20	 			; yes, so check if we support it

	mov	Current_Button,ah			; no, select what ever button, and quit
	jmp	short btn_sel_60     			; process both buttons pushed together


btn_sel_20:

; if we get here, we have a "btn_1 + btn_2" push, and need to verify the keyboard type
; we can now check comp_id, and if we don't have a 2(New XT), 6(25..30/86), 7(New XT
; New AT, 25..30..50..60/286) or 8(55SX..70..80), then we don't have a kb_flag_3 to check !!!
; Could also be true for new 5A or 1A....see new keyboard.inc

 	cmp	comp_flag,true	 		   	; do we have a comp_id of #2,#5A,#5B,#6,#7, or #8 ?
	jne	btn_sel_25
	mov	Current_Button,al			; let btn1+btn2 through
	jmp	short btn_sel_60

btn_sel_25:

; if comp_flag is not set, we have a comp_id of 1=PC, 5=Orig. AT
; 3= Jr. or 4=Conv.  We do not support selections 3 and 4, and if it is either
; 1 or 5, the mouse must be a serial mouse, so reset the mouse button to the left 
; button for either of these two, or beep error for Jr./Conv. and then set button

	cmp	_comp_id,1
	je	btn_sel_55
	cmp	_comp_id,5
	je	btn_sel_55

btn_sel_50:

	call	beep_high				; beep high for button error selection
	cmp	fswitching_video,false
	jne	btn_sel_55
	mov	faccess_sound,true
	mov	fsecond_cue,7

btn_sel_55:
; there is really no reason why a PS/2 should have an 84 key keyboard and a serial mouse !!!

	cmp	_fmouse_id,4				; do we have a AUX. port PS/2 mouse ?
	jne	btn_sel_56
	mov	Current_Button,01h			; left button AUX
	jmp	short btn_sel_60

btn_sel_56:
	cmp	_fmouse_id,2				; do we have a serial port mouse ?
	jne	btn_sel_60				; no, somethings wrong !!! this should never execute !
	mov	Current_Button,020h			; left button serial

btn_sel_60:

	pop	ax
	ret

btn_sel_proc	endp

;----------------------------------------------------------------------------
; button_delta
;
; Expects:	ax = 0 - buttons up
;		   = non 0 - buttons down
;		cx = extended scan code
;
; Changes:	bx,dx

button_delta	proc

	assume	DS:_TEXT

	push	ax
	push	bx
	push	cx
	push	dx

	mov	fbutton,true				; mousekeys mouse button 
	test	cl,break_bit				; is this the break key?
	jz	bd_5					; no, so cont. on
	jmp	button_delta_end			; was a break key, so quit

bd_5:

	call	stop_moving				; yes, so stop any motion
	mov	bl,Button_Status			; current status
	mov	cl,Current_Button			; current button(s) to press


	or	ax,ax					; do a button(s) down?
	jz	bd_10					; no -->

; Button down

	or	dl,cl					; get new status
	mov	Button_Status,dl			; keep Button_Status for next time thru
	jmp	bd_100

; Button up

bd_10:

	or	bl,bl					; any buttons down to let up
	jnz	bd_15
	jmp	button_delta_end			; no, was zero, so quit

bd_15:

	not	cl					; get mask to clear state
	and	dl,cl					; get new status
	mov	Button_Status,dl			; keep Button_Status for next time thru

; Now do delta

bd_100:

	cmp	_fmouse_id,4				; is the PS/2 mouse driver loaded
	jne	bd_140		

	cmp	Current_Button,1			; left button ?
	jne	bd_110
	cmp	dl,0					; button up ?
	jne	bd_105
	mov	dx,0008h				; release button 1
	jmp	bd_130

bd_105:
	mov	dx,0009h				; press button 1
	jmp	bd_130

bd_110:

	cmp	Current_Button,2			; right button ?
	jne	bd_120
	cmp	dl,0					; button up ?
	jne	bd_115
	mov	dx,0008h				; release button 2
	jmp	bd_130

bd_115:
	mov	dx,000ah				; press button 2
	jmp	bd_130

bd_120:							; if gets here, both buttons must be down/up

	cmp	dl,0					; button up ?
	jne	bd_125
	mov	dx,0008h 				; release both buttons
	jmp	bd_130

bd_125:
	mov	dx,000bh 				; press both buttons
bd_130:

	mov	Mouse_Status,dx
	jmp	bd_200

;-----------------------------------------------------------------------------
; serial mouse buttons

bd_140:

	cmp	_fmouse_id,2	 			; do we have a serial mouse ?
	jne	button_delta_end
							; process serial mouse data
	cmp	Current_Button,20h			; left button ?
	jne	bd_150
	cmp	dl,0h					; button up ?
	jne	bd_145
	mov	dx,0040h				; release button 1
	jmp	bd_190

bd_145:
	mov	dx,0060h				; press button 1
	jmp	bd_190

bd_150:

	cmp	Current_Button,10h			; right button ?
	jne	bd_160
	cmp	dl,0h					; button up ?
	jne	bd_155
	mov	dx,0040h				; release button 2
	jmp	bd_190

bd_155:
	mov	dx,0050h     				; press button 2
	jmp	bd_190

bd_160:							; if gets here, both buttons must be down/up

	cmp	dl,00h					; button up ?
	jne	bd_165
	mov	dx,0040h 				; release both buttons
	jmp	bd_190

bd_165:
	mov	dx,0070h 				; press both buttons

bd_190:

	mov	Mouse_Status,dx	

bd_200:

	cmp	mouse_cnt,100				; are there 100 mouse data words in buffer already ?
	jge	button_delta_end

	push	ax

	mov	ax,Mouse_Status
	call	Put_Mouse_Data				; store info on serial mouse or PS/2 mouse in buffer
	mov	ax,Delta_X
	call	Put_Mouse_Data				; store info on serial mouse or PS/2 mouse in buffer
	mov	ax,Delta_Y
	call	Put_Mouse_Data				; store info on serial mouse or PS/2 mouse in buffer

	pop	ax
	mov	fmouse_button,true     			; is a mousekeys mouse button data

button_delta_end:

	pop	dx
	pop	cx
	pop	bx
	pop	ax

	ret
button_delta	endp

;----------------------------------------------------------------------------
; button_click
;
; Expects:	nothing
;
; Changes:	ax,bx,cx,dx

button_click 	proc

	assume	DS:_TEXT

  	mov	fbutton,true			; mousekeys mouse button to true

	cmp	fbutton_up,true			; if we got a MouseKeys button_up, jump to do a button down/up only
	jne	button_click_5
	cmp	fmousetrapping,true		; is mouse int. hook off?
	jne	button_click_10
	mov	fbutton_up,false		; reset here if mouse int. trapping is off
	jmp	short button_click_15

button_click_5:

	xor	ax,ax
	call	button_delta			; make sure they are all up
	not	ax

button_click_10:

	call	button_delta			; then do click

button_click_15:

	xor	ax,ax
	call	button_delta
	ret
button_click	endp

;----------------------------------------------------------------------------
; dbl_click_proc
;
; Expects:	Nothing

dbl_click_proc	proc	

	assume	DS:_TEXT

	call	button_click
	call	button_click
	ret
dbl_click_proc	endp

;----------------------------------------------------------------------------
; move_proc
;
; Expects:	ax = direction	ah = x dir
;				al = y dir
;		cx = extended code
;
; Changes:	ax,cx


move_proc	proc	

	assume	DS:_TEXT

	push	cx
	push	ax

; check for break of key
	
	mov	fbutton,false		; move mousekey mouse button to false
	test	cl,break_bit		; is it a break key?
	jz	move_10			; no -->

; deal with break of key

move_proc_5:

	call	stop_moving		; yes, stop moving mouse

move_7:	
	jmp	move_proc_end

; deal with the make of a mousekey

move_10:
	cmp	Last_Direction,ax	; is this the same as last direction?
	jne	move_20			; no -->
	mov	fMoving,true
	jmp	move_proc_end
move_20:
	call	stop_moving		; stop any other current motion

	mov	Last_Direction,ax	; save direction

; added code to allow the mouse cursor to move using mousekeys even if RepeatKeys is off

	cmp	fUser_SetUp_Option1,true	; if set, reallow fmoving
	je	MK_10

	cmp	fUser_SetUp_Option2,true	; if set, reallow fmoving
	je	MK_10

	cmp	on_repeat_ticks,32760		; if set, repeat keys is off, reallow fmoving
	je	MK_10

	cmp	fRecovery_On,true		; if RecoveryKeys is on, reallow fmoving
	je	MK_10

	cmp	fserial_key_recieved,true	; if serial_key input, reallow fmoving
	je	MK_10

	cmp	_fmouse_id,2	 		; do we have a serial mouse ?
	jne	MK_20				; if we have a serial mouse, allow each key tap to get sent

MK_10:
	mov	fMoving,true			; allow mousekeys to work even if RepeatKeys is off

MK_20:
	mov	cx,ax				; get copy into cx

	cmp	_fmouse_id,4			; is the PS/2 mouse driver loaded
	jne	MK_60

	and	ax,00ffh			; mask off ah, so al = y data

	mov	Delta_Y,ax			; Set delta Y

	test	al,80h				; is the high bit set (i.e. neg ?)
	jz	MK_30
	or	Status,20h			; if was neg, set bit 5 for negative Y
MK_30:

	mov	ax,cx				; restore ax with ah = x data, and al = y data
	mov	al,ah
	and	ax,00ffh			; mask off ah, so al now = x data

	mov	Delta_X,ax			; Set delta X

	test	al,80h				; is the high bit set (i.e. neg ?)
	jz	MK_50
	or	Status,10h			; if was neg, set bit 4 for negative X

MK_50:

	mov	al,Status			; now have direction in al register
	or	al,Button_Status		; get button status info. into al register
	mov	Mouse_Status,ax			; store into mouse_status for prep. to call inject proc.
	call	InjectMouse
	jmp	move_proc_end
;-------------------------------------------------------------------------------------
; serial mouse


MK_60:

	cmp	_fmouse_id,2	 		; do we have a serial mouse ?
	jne	move_proc_end
						; process serial mouse data
MK_70:
	and	ax,00ffh			; mask off ah, so al = y data

	cmp	al,00h				; if zero y movement, cont. on
	je	MK_80

	test	al,80h				; is the high bit set (i.e. neg ?)
	jnz	MK_75
	or	Status,0Ch			; if was pos, set bit 3 and 2 for negative Y
MK_75:
	neg	al				; serial mouse neg Y is up ! 
						
MK_80:
	mov	Delta_Y,ax			; Set delta Y
	and	Delta_Y,03fh			; flag not the first byte

	mov	ax,cx				; restore ax with ah = x data, and al = y data
	mov	al,ah
	and	ax,00ffh			; mask off ah, so al now = x data

	mov	Delta_X,ax			; Set delta X


	test	al,80h				; is the high bit set (i.e. neg ?)
	jz	MK_85
	or	Status,03h			; if was neg, set bit 1 and 0 for negative X

MK_85:

	and	Delta_X,03fh			; flag not the first byte

	mov	al,Status			; now have direction in al register
	or	al,Button_Status		; get button status info. into al register
	or	al,40h				; set the bit which always needs to be set for serial mouse

	mov	Mouse_Status,ax			; store into mouse_status for prep. to call inject proc.

	cmp	mouse_cnt,100			; are there 100 nouse data wors in buffer already ?
	jge	move_proc_end

	call	Put_Mouse_Data			; store info on serial mouse in buffer
	mov	ax,Delta_X
	call	Put_Mouse_Data			; store info on serial mouse in buffer
	mov	ax,Delta_Y
	call	Put_Mouse_Data			; store info on serial mouse in buffer

	mov	fmouse_button,true     		; is a mousekeys mouse button data

move_proc_end:

	pop	ax
	pop	cx
	ret
move_proc	endp

;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
; MouseKeys
;
;   This is the beginning of the routine to emulate a mouse with the keypad
; of the keyboard.  It expects the extended scan code to be in AX.
;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------
;---------------------------------------------------------------------------------------

mouse_begin:

	assume	DS:_TEXT

	push	es
	push	cs
	pop	es
	assume	ES:_TEXT

	mov	dh,shift_flg			; get assumed current states
	or	dh,real_states
	or	dh,lock_flg

	cmp	fMouseKeysOn,true		; is mouse key on?
	je	mouse_100			; yes, do routines -->

;----------------------------------------------------------------------------
; We are off so only check to see if turned on
;
	
	cmp	ax,NumLock			; make of numlock?
	jne	mouse_108			; no, just pass on
	cmp	dh,fLshift + fAlt		; only the left shift and alt down?
	jne	mouse_110			; no, pass on -->

; must check if turned on by 3 key sequence, if mouse driver is not loaded,
; then don't allow mousekeys to turn on 
 
	cmp	fmouse_driver,false		; if fmouse_driver false, don't turn mousekeys on
	je	mouse_112			; no, just pass on -->

	cmp	fJust_Manually_Off,true		; did we just turn off
	je	mouse_112

	call	mk_turn_on_fb
	cmp	fspace_saver,true
	jne	mouse_55

	cmp	fSticKeysOn,true		; is StickeyKeys On ???
	jne	mouse_55			; if StickeyKeys isn't On, don't mess with Num Lock
				       	
	push	es
	push	bx
	push	ax
	assume 	es:NOTHING
	mov	bx,RAMBIOS
	mov	es,bx
	assume	es:RAMBIOS
	mov	al,es:[kb_flag]
	test	al,fNum				; is the num lock key already down ?
	jnz	mouse_50			; if not zero, then it is already down and we don't want to pass it again
	mov	fnum_lock_was_off,true		; flag ourselves that we are going to pass the make
	pop	ax
	pop	bx
	pop	es
	assume	es:_TEXT
	jmp	short mouse_110			; if space saver keyboard, pass num lock make on, as it will 
						; set up mousekeys correctly as num lock will be on
mouse_50:
	pop	ax
	pop	bx
	pop	es
	assume	es:_TEXT
	mov	fnum_lock_was_off,false		; flag ourselves that we did not pass the make, so we don't have to
						; pass the break in off_proc

mouse_55:
	jmp	MouseKeys_end			; inhibit key stroke -->

;----------------------------------------------------------------------------
; we are on, so check for pertainent keys of mousekeys
;
mouse_100:

; must check if turned on by Dialog Box, if mouse driver is not loaded,
; then don't allow mousekeys to call Inject Mouse 

mouse_102:

	cmp	fmouse_driver,false		; if fmouse_driver false, don't turn mousekeys on
	je	mouse_110			; no, just pass on -->

	mov	si,ax				; save extended scan code in bx
	and	al,not break_bit		; get make scan code in al
	mov	cx,mouse_tbl_len		; search for a mouse key
	mov	di,offset mouse_tbl		; when done, cl will hold shift
	cld					;   count to get flag for mouse key
	repne	scasw				; ne=not found  e=found
	je	mouse_10			; yes, a key we are interested in -->
;
; Does not assume ToggleKeys saves any registers
;
	call	stop_moving			; no
	mov	ax,si				; restore original extended scan

	push	es
	push	ax
	assume	ES:NOTHING
	mov	ax, RAMBIOS	 		; BIOS RAM segment at 40h
	mov	es,ax		 		;  .. point ES to that!
	assume	ES:RAMBIOS
	mov	al,es:kb_flag			; take a quick look at the flags
	and	al,03h				; mask off eveything except bits 1 and 0, left and right shift
	jnz	mouse_105			; if either bit is set, then don't reset fmouse_button flag (i.e. shift click in progress)
	mov	fmouse_button,false  		; not a mousekeys mouse button

mouse_105:

	pop	ax			
	pop	es
	assume	ES:_TEXT

mouse_108:
	mov	fJust_Manually_Off,false	 ; reset to false upon non mousekey 

mouse_110:
	call	ToggleKeys			; pass key on
mouse_112:
	jmp	MouseKeys_end
;
; We expect cx to hold the count of the key we found.  We will now use it
; as an index into tables and jump to the appropriate routine to handle the
; key.

mouse_10:

	shl	cx,1				; multiply by 2 to get offset to words
	mov	bx,cx				; put in bx
	mov	cx,si				; put scan in cx
	cmp	cx,052h				; is it mouse button down ?
	jne	mouse_12
	mov	fbutton_down,true		; yes it was, flag for later processing
	mov	fbutton_up,false		; reset other button flag
	jmp	short mouse_20

mouse_12:

	cmp	cx,053h				; button_up make code only, break falls thru per norm
	jne	mouse_20
	mov 	fbutton_up,true			; if it is button_up, flag for later processing
	mov	fbutton_down,false		; reset other button flag

; since it is the period key, or "DEL", check to see if "CTRL and ALT" keys are down,
; i.e. the user wants to reboot, so don't make them have to turn off MouseKeys to do so
; for those users who do not have a seperate "DEL" cursor pad key

	push	es
	push	ax
	assume	ES:NOTHING
	mov	ax, RAMBIOS	 		; BIOS RAM segment at 40h
	mov	es,ax		 		;  .. point ES to that!
	assume	ES:RAMBIOS
	mov	al,es:kb_flag			; take a quick look at the flags
	and	al,0ch				; mask off eveything except bits 2 and 3(ctrl and alt)
	cmp	al,0ch				; are both bits set
	jne	mouse_18			; if no flags are set, cont. on as normal
	pop	ax			
	pop	es
	mov	fmouse_button,false		; not a mousekeys mouse button
	mov	ax,si				; restore original extended scan
	call	ToggleKeys			; pass key on, which will cause a reboot !!!
	jmp	short MouseKeys_end	

mouse_18:
	pop	ax
	pop	es
	assume	ES:_TEXT

mouse_20:

	cmp	cx,045h				; numlock make key ???
	jne	mouse_21
	jmp	short mouse_22	

mouse_21:
	cmp	cx,0c5h				; numlock break key ???
	jne	mouse_25

mouse_22:

; was a numlock make/break code, so check if it is really the nulock, or part of the PAUSE key?


	cmp	fpause_being_sent,true		; is PAUSE key being sent ??
	jne	mouse_25			; if not a PAUSE, send on as normal MouseKey

; if part of PAUSE key, pass on to ToggleKeys

	mov	ax,si				; restore original extended scan
	call	ToggleKeys			; pass key on
						; when we retrun from ToggleKeys
  	mov	fpause_being_sent,false		; reset this flag after we pass the PAUSE key parts along
	jmp	short MouseKeys_end

mouse_25:

; check first if we've been silently toggled off due to num lock key (used for laptops)

	cmp	fpass_mousekeys,true 	    	; allow numeric keys to be trapped
	je	mouse_40      		  	; yes, cont on

; fpass_mousekeys is false, we are not trapping mousekeys, so only trap numlock key

	cmp	cx,045h	     		  	; numlock make key ???
	je	mouse_40
	cmp	cx,0c5h			  	; numlock break key ??
	je	mouse_40	   	

	mov	ax,si			  	; restore original extended scan
	call	ToggleKeys		  	; pass key on
	jmp	short MouseKeys_end

mouse_40:

	mov	fbios_called_timing,false 	; reset flag if it was a mousekey and we are not calling ToggleKeys
	mov	ax,MK_data_table[bx]	  	; get data to pass to routine
	call	MK_call_table[bx]	  	; now do it

MouseKeys_end:

	pop	es
	assume ES:NOTHING

	ret
MouseKeys	endp

_TEXT	ends
	
	end

